use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Chain, Cursor, Read, Seek};
use std::path::{Path, PathBuf};
use std::rc::Weak;
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
    /// 文件名称
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

struct DiffView {
    // todo
}

/// 一个记录节点, 用于构建 [`SpaceDistribution`] 文件树.
struct RecordNode {
    raw_record: RawRecord,
    children: Vec<RecordNode>,
    parent: Weak<RecordNode>,
}

/// 空间分布, 储存着文件信息.
///
/// 对应的是文件 WizTree 产生的一个 csv 文件.
///
/// 此结构会对 csv 文件的进行树状结构的解析.
/// 这个树状结构可能不止一个根节点.
pub struct SpaceDistribution {
    dummy_root: RecordNode,
}

impl SpaceDistribution {
    /// 从 [记录](RawRecord) 中构建一个 [空间分布](SpaceDistribution).
    ///
    /// 此方法假定:
    /// - 原始记录数组是按照原始 csv 文件中的行顺序排列的
    /// (wiztree 的所有排列方式都能保证子目录紧随父目录),
    /// 没有进行排序过.
    /// - RawRecord 中所有的路径都是已经解析过的, 不包含 `..` 或 `.` 或符号链接等内容.
    ///
    /// 如果传入已经排序过的记录, 会产生错误地结果,
    /// 此时应该使用性能较劣的 [`SpaceDistribution::from_unordered_records`]
    /// 以获得正确的输出.
    /// <!-- todo 有待验证两者性能 -->
    fn from_ordered_records(records: &[RawRecord]) -> SpaceDistribution {
        let mut sd = SpaceDistribution {
            dummy_root: RecordNode {
                raw_record: Default::default(),
                children: Vec::new(),
                parent: Weak::new(),
            },
        };
        let mut cur_rec = &mut sd.dummy_root;
        for rec in records {
            if rec.path.starts_with(&cur_rec.raw_record.path) {}
        }
        sd
    }

    /// 从 [记录](RawRecord) 中构建一个 [空间分布](SpaceDistribution).
    ///
    /// 此方法假定:
    /// - RawRecord 中所有的路径都是已经解析过的, 不包含 `..` 或 `.` 或符号链接等内容.
    ///
    /// 此方法允许输入 records 数组是乱序的,
    /// 但是性能相比 [`SpaceDistribution::from_ordered_records`] 较弱.
    fn from_unordered_records(records: &[RawRecord]) -> SpaceDistribution {
        todo!();
    }

    /// 从 csv 文件中构建 [`SpaceDistribution`].
    pub fn from_csv_file(file: impl AsRef<Path>) -> Result<SpaceDistribution, Error> {
        let mut reader = build_csv_reader(File::open(file)?)?;
        let records: Vec<_> = reader
            .records()
            .map(|r| r.unwrap().deserialize(None).unwrap())
            .collect();
        Ok(SpaceDistribution::from_ordered_records(&records))
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

pub fn diff() -> DiffView {
    todo!()
}

pub fn diff_file(file1: impl AsRef<Path>, file2: impl AsRef<Path>) -> Result<DiffView, Error> {
    let builder = csv::ReaderBuilder::new();
    todo!();
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
                                       // - 使用 Path::canonicalize, 这个还没了解过具体的情况
                                       // - 使用 path_clean::clean
        let b_dd_normalized = path_clean::clean(b_dd.clone());
        dbg!(&b_dd_normalized);
        assert!(!b_dd_normalized.starts_with(&a));
    }
}
