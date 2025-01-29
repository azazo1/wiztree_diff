use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Chain, Cursor, Read, Seek};
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::rc::{Rc, Weak};
use std::time::{Duration, Instant};

/// 控制调用的时间频率
struct Throttler {
    last_trigger_time: Instant,
    interval: Duration,
}

impl Throttler {
    fn new(interval: Duration) -> Self {
        Self {
            last_trigger_time: Instant::now() - interval, // 确保能直接触发
            interval,
        }
    }

    /// 设置间隔时间并重置触发时间
    fn set_interval(&mut self, interval: Duration) {
        self.last_trigger_time = Instant::now() - interval;
        self.interval = interval;
    }

    fn throttle(&mut self) -> bool {
        if self.last_trigger_time.elapsed() >= self.interval {
            self.last_trigger_time = Instant::now();
            true
        } else {
            false
        }
    }

    fn throttle_run<F, R>(&mut self, f: F) -> Option<R>
    where
        F: FnOnce() -> R,
    {
        if self.throttle() {
            Some(f())
        } else {
            None
        }
    }
}

/// 一个 Record 记录了一个文件(夹)的基本信息.
/// 对应一个 WizTree 导出的 csv 文件的一行.
#[derive(Default, Serialize, Deserialize, Debug, Clone)]
struct RawRecord {
    /// 文件名称,
    ///
    /// 在 Windows 下如果是文件夹, 那么以 `\` 结尾.
    path: PathBuf,
    /// 大小
    size: usize,
    /// 分配
    alloc: usize,
    /// 修改时间, "yyyy/mm/dd HH:mm:ss"
    modify_time: String,
    /// 属性
    /// 属性存储为一个值，并且是以下值的组合相加而成：
    /// 1 = 只读 (R), 2 = 隐藏 (H), 4 = 系统 (S), 32 = 存档 (A), 2048 = 压缩 (C)
    /// 例如，如果一个文件是只读且隐藏的，它的属性值将等于 3（1 + 2）
    ///
    /// 但是有例外, 有的文件的属性值可能为十进制 "268435456"
    attributes: usize,
    /// 文件数量
    n_files: usize,
    /// 文件夹数量
    n_folders: usize,
}

impl RawRecord {
    fn is_folder(&self) -> bool {
        self.path.as_os_str()
            .to_string_lossy/*转换成字符串, 替换无效字符*/()
            .ends_with(std::path::MAIN_SEPARATOR_STR)
    }
    fn is_file(&self) -> bool {
        !self.is_folder()
    }
}

struct DiffView {
    // todo
}

/// 可以实现一个简短 Debug, Display 输出的 [`Rc<RefCell<RecordNode>>`] 类型
struct RcRecordNode(Rc<RefCell<RecordNode>>);

impl RcRecordNode {
    fn clone(node: &RcRecordNode) -> Self {
        Self(Rc::clone(&node.0))
    }
}

impl Debug for RcRecordNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Rc")?; // 在结构体名称前面加上 Rc
        self.0.borrow().fmt(f)
    }
}

impl std::fmt::Display for RcRecordNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.borrow().path.to_string_lossy())
    }
}

impl Deref for RcRecordNode {
    type Target = Rc<RefCell<RecordNode>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for RcRecordNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AsRef<Rc<RefCell<RecordNode>>> for RcRecordNode {
    fn as_ref(&self) -> &Rc<RefCell<RecordNode>> {
        &self.0
    }
}

/// 一个记录节点, 用于构建 [`SpaceDistribution`] 文件树.
#[derive(Debug)]
struct RecordNode {
    raw_record: RawRecord,
    children: Vec<RcRecordNode>,
    parent: Weak<RefCell<RecordNode>>,
}

impl AsRef<RawRecord> for RecordNode {
    fn as_ref(&self) -> &RawRecord {
        &self.raw_record
    }
}

impl Deref for RecordNode {
    type Target = RawRecord;

    fn deref(&self) -> &Self::Target {
        &self.raw_record
    }
}

impl RecordNode {
    fn new(raw_record: RawRecord) -> Self {
        Self {
            raw_record,
            children: Vec::new(),
            parent: Weak::new(),
        }
    }

    fn set_parent(&mut self, parent: &RcRecordNode) {
        self.parent = Rc::downgrade(parent);
    }

    /// 尝试获取父节点.
    fn parent(&self) -> Option<RcRecordNode> {
        self.parent.upgrade().map(|x| RcRecordNode(x))
    }

    /// 放入子节点, 返回子节点的一个强引用.
    fn push_child(parent: &RcRecordNode, mut child: RecordNode) -> RcRecordNode {
        child.set_parent(parent);
        let rc = RcRecordNode(Rc::new(RefCell::new(child)));
        parent.borrow_mut().children.push(RcRecordNode::clone(&rc));
        rc
    }

    fn last_child_mut(&mut self) -> Option<&mut RcRecordNode> {
        self.children.last_mut()
    }
}

/// 空间分布, 储存着文件信息.
///
/// 对应的是文件 WizTree 产生的一个 csv 文件.
///
/// 此结构会对 csv 文件的进行树状结构的解析.
/// 这个树状结构可能不止一个根节点.
#[derive(Debug)]
pub struct SpaceDistribution {
    roots: Vec<RcRecordNode>,
}

impl SpaceDistribution {
    /// 从 [记录](RawRecord) 中构建一个 [空间分布](SpaceDistribution).
    ///
    /// 此方法假定:
    /// - 原始记录数组是按照原始 csv 文件中的行顺序排列的
    /// (wiztree 的所有排列方式都能保证子目录紧随父目录),
    /// 没有进行排序过.
    /// - RawRecord 中所有的路径都是已经解析过的, 不包含 `..` 或 `.` 或符号链接等内容.
    /// - 没有重复路径的 RawRecord.
    /// - 一个 RawRecord 的路径如果存在, 那么在它的逐级父路径对应的 RawRecord 都在它之前存在, 直到根节点.
    ///   比如:
    ///   > 如果 `/a/b/c/d` 存在, csv 中的根目录为 `/a/b`,
    ///   > 那么 `/a/b/c` 和 `/a/b` 一定存在且在 `/a/b/c/d` 之前.
    ///
    /// 如果传入已经排序过的记录, 会产生错误地结果,
    /// 此时应该使用性能较劣的 [`SpaceDistribution::from_unordered_records`]
    /// 以获得正确的输出.
    /// <!-- todo 有待验证两者性能 -->
    fn from_ordered_records(records: &[RawRecord]) -> SpaceDistribution {
        if records.is_empty() {
            return SpaceDistribution { roots: Vec::new() }; // 空值
        }
        let mut sd = SpaceDistribution { roots: Vec::new() };
        let mut cur_rec = sd.push_root(
            RecordNode::new(records[0].clone()) // 第一个 record 一定是根目录之一.
        );
        for raw_rec in &records[1..] {
            let rec = RecordNode::new(raw_rec.clone());
            // 不断向上查找, 直到 cur_rec 为 rec 的父目录.
            while !rec.path.starts_with(&cur_rec.borrow().path) {
                let Some(parent) = cur_rec.borrow().parent() else {
                    // cur_rec 如果是根节点.
                    break;
                };
                cur_rec = parent;
            }
            if rec.path.starts_with(&cur_rec.borrow().path) {
                // 如果 rec 是 cur_rec 的子目录.
                cur_rec = RecordNode::push_child(&cur_rec, rec);
            } else {
                // rec 不是 cur_rec 的子目录, 此处是因为上面的 while 循环 break 了.
                cur_rec = sd.push_root(rec);
            }
        }
        sd
    }

    /// 从 [记录](RawRecord) 中构建一个 [空间分布](SpaceDistribution).
    ///
    /// 此方法假定:
    /// - RawRecord 中所有的路径都是已经解析过的, 不包含 `..` 或 `.` 或符号链接等内容.
    /// - 没有重复路径的 RawRecord.
    ///
    /// 此方法允许输入 records 数组是乱序的,
    /// 但是性能相比 [`SpaceDistribution::from_ordered_records`] 较弱.
    fn from_unordered_records(records: &[RawRecord]) -> SpaceDistribution {
        todo!();
    }

    /// 从 csv 文件中构建 [`SpaceDistribution`].
    ///
    /// csv 文件内容需要是从 WizTree 从文件中直接导出(WizTree 内部排序任意皆可)而不经过任何顺序调整的.
    pub fn from_csv_file(file: impl AsRef<Path>) -> Result<SpaceDistribution, Error> {
        let mut reader = build_csv_reader(File::open(file)?)?;
        let mut ok = Ok(());
        let records: Vec<_> = reader
            .records()
            .map_while(|r| {
                let string_rec = match r {
                    Ok(string_rec) => string_rec,
                    Err(e) => {
                        ok = Err(e);
                        return None;
                    }
                };
                match string_rec.deserialize(None) {
                    Ok(rec) => Some(rec),
                    Err(e) => {
                        ok = Err(e);
                        None
                    }
                }
            })
            .collect();
        ok?;
        Ok(SpaceDistribution::from_ordered_records(&records))
    }

    /// 放入新的根节点, 并返回该节点的一个强引用.
    fn push_root(&mut self, root: RecordNode) -> RcRecordNode {
        let rc = RcRecordNode(Rc::new(RefCell::new(root)));
        self.roots.push(RcRecordNode::clone(&rc));
        rc
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("IO error")]
    Io(#[from] io::Error),
    /// 可能会和 [`Io`](Error::Io) 重叠,
    ///
    /// 一般来说, 当 io 错误从 csv 操作中产生的时候,
    /// 会归因于此.
    #[error("Csv parsing error")]
    Csv(#[from] csv::Error),
}

/// 以一个 [`Read`] 作为内容, 创建一个 CSV Reader,
/// 并自动跳过可能的 header 行之前的无效行.
///
/// 此函数假定:
/// - 每个 csv 文件都有至少一个 header 行.
///
/// # Parameters
/// - `content_reader`: csv 内容, 注意不是 csv 文件路径.
fn build_csv_reader<R: Read>(
    content_reader: R,
) -> Result<csv::Reader<Chain<Cursor<String>, BufReader<R>>>, Error> {
    let mut buf_reader = BufReader::new(content_reader);
    // 找到 header 行
    let header_line = loop {
        let mut line = String::new();
        if buf_reader.read_line(&mut line)? == 0 {
            // 读取到文件末尾
            return Err(Error::Io(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "No header line found",
            )));
        };
        if line.contains(",") {
            break line;
        }
    };

    let reader = csv::ReaderBuilder::new().has_headers(true).from_reader(
        // 拼接回 header 行
        Cursor::new(header_line + "\n").chain(buf_reader),
    );

    Ok(reader)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_read_csv_records() {
        let start = Instant::now();
        let mut reader =
            build_csv_reader(File::open("example_data/example_1.csv").unwrap()).unwrap();
        let mut throttle = Throttler::new(Duration::from_secs(1));
        for (i, record) in reader.records().enumerate() {
            let record = record.unwrap();
            let v: Vec<_> = record.iter().collect();
            let raw_record: RawRecord = record.deserialize(None).unwrap();
            throttle.throttle_run(|| println!("{i}: {:?}", raw_record));
        }
        println!("Elapsed: {:?}", start.elapsed());
    }
    #[test]
    fn size_of() {
        assert_eq!(
            96,
            size_of_val(&RawRecord {
                size: 0,
                alloc: 0,
                attributes: 0,
                n_files: 0,
                n_folders: 0,
                modify_time: "".to_string(),
                path: "".into()
            })
        );
    }
    #[test]
    #[cfg(target_os = "windows")]
    fn prefix_path() {
        let root: PathBuf = "D:/".into();
        let a: PathBuf = "D:/a".into();
        let b: PathBuf = "D:/a/b".into();
        let b_d: PathBuf = "D:/a/b/..".into();
        let b_dd: PathBuf = "D:/a/b/../..".into();
        assert!(a.starts_with(&root));
        assert!(b_d.starts_with(&a));
        assert!(b_dd.starts_with(&a)); // 实际上, 这个应该为 false, 但是 b_dd 这个路径没有标准化
        // 路径的标准化我知道的有两种方法:
        // - 使用 fs::canonicalize, 这个还没了解过具体的情况, 还不知道其和 Path::canonicalize 的区别.
        //   两者似乎一样, 但是对 "/" 在 Windows 下进行调用会产生奇怪的值.
        // - 使用 path_clean::clean
        let b_dd_normalized = path_clean::clean(b_dd.clone());
        dbg!(&b_dd_normalized);
        assert!(!b_dd_normalized.starts_with(&a));
        // 空路径
        let root: PathBuf = "/".into();
        let empty: PathBuf = Default::default();
        assert!(empty.starts_with(&empty));
        assert!(!empty.starts_with(&root));
        assert!(root.starts_with(&empty)); // 空路径满足其他路径的 starts_with
    }

    #[test]
    #[cfg(target_os = "windows")]
    fn canonicalize() {
        use std::fs::canonicalize;
        // assert_eq!(canonicalize("/").unwrap(), PathBuf::from("\\\\?\\D:\\"));
        // 产生的路径是: "\\\\?\\D:\\", `\\?\` 这个前缀似乎是用来表示这个路径是长路径.
        assert_eq!(canonicalize("D:/").unwrap(), PathBuf::from("\\\\?\\D:\\"));
        assert_eq!(
            PathBuf::from("D:/").canonicalize().unwrap(),
            canonicalize(PathBuf::from("D:/")).unwrap()
        );
        assert!(
            ! // 这里就很麻烦了, 标准化后的路径是以 `\\?\` 开头的, 但是这个前缀仍然参与 starts_with 判断.
                PathBuf::from("D:/").canonicalize().unwrap()
                    .starts_with("D:/")
        );
    }

    #[test]
    fn test_build_space_distribution() {
        let sd = SpaceDistribution::from_csv_file("example_data/example_multi_roots.csv").unwrap();
        dbg!(sd);
    }

    #[test]
    fn bench_build_space_distribution_ordered_records() {
        let start = Instant::now();
        let sd = SpaceDistribution::from_csv_file("example_data/example_1.csv").unwrap();
        println!("Elapsed: {:?}", start.elapsed()); // 10.5s (260w记录)
        for root in &sd.roots {
            println!("root: {}", root);
            for child in &root.borrow().children {
                println!("  {}", child);
            }
        }
    }
}
