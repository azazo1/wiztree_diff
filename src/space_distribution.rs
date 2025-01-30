use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::path;
use std::path::{Path, PathBuf};
use std::rc::{Rc, Weak};
use serde::{de, Deserialize, Deserializer};
use serde::de::{SeqAccess, Visitor};
use crate::{build_csv_reader, Error};

/// 一个 Record 记录了一个文件(夹)的基本信息.
/// 对应一个 WizTree 导出的 csv 文件的一行.
#[derive(Default, Debug, Clone)]
pub(crate) struct RawRecord {
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
    /// 是否是文件夹
    folder: bool,
}

impl RawRecord {
    fn default_with_path(path: PathBuf) -> RawRecord {
        let mut rec = RawRecord::default();
        rec.folder = path.to_string_lossy().ends_with(|x| x == '/' || x == '\\');
        rec.path = path;
        rec
    }
}

impl PartialEq for RawRecord {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl Eq for RawRecord {}

impl Hash for RawRecord {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

/// 如果是文件夹, 确保字符串以路径分隔符结尾, 否则删除结尾的路径分隔符
///
/// 自动替换 `/` 和 `\` 为当前系统的路径分隔符.
///
/// 返回原字符串的借用.
fn ensure_path_sep(path_str: &mut String, is_folder: bool) -> &mut String {
    fn pattern(x: char) -> bool {
        x == '/' || x == '\\'
    }
    *path_str = path_str.replace(pattern, path::MAIN_SEPARATOR.to_string().as_str());
    if is_folder {
        if !path_str.ends_with(pattern) {
            path_str.push(path::MAIN_SEPARATOR);
        }
    } else {
        while path_str.ends_with(pattern) {
            path_str.pop();
        }
    }
    path_str
}

impl<'de> Deserialize<'de> for RawRecord {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct RawRecordVisitor;

        impl<'d> Visitor<'d> for RawRecordVisitor {
            type Value = RawRecord;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("invalid csv row")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'d>,
            {
                let mut rec = RawRecord::default();
                let path_string: String = seq.next_element()?.ok_or_else(|| de::Error::missing_field("path"))?;
                rec.folder = path_string.ends_with('\\');
                rec.path = path_string.into();
                rec.size = seq.next_element()?.ok_or_else(|| de::Error::missing_field("size"))?;
                rec.alloc = seq.next_element()?.ok_or_else(|| de::Error::missing_field("alloc"))?;
                rec.modify_time = seq.next_element()?.ok_or_else(|| de::Error::missing_field("modify_time"))?;
                rec.attributes = seq.next_element()?.ok_or_else(|| de::Error::missing_field("attributes"))?;
                rec.n_files = seq.next_element()?.ok_or_else(|| de::Error::missing_field("n_files"))?;
                rec.n_folders = seq.next_element()?.ok_or_else(|| de::Error::missing_field("n_folders"))?;
                Ok(rec)
            }
        }
        deserializer.deserialize_seq(RawRecordVisitor)
    }
}

/// 可以实现一个简短 Debug, Display 输出的 [`Rc<RefCell<RecordNode>>`] 类型
pub(crate) struct RcRecordNode(Rc<RefCell<RecordNode>>);

impl RcRecordNode {
    fn clone(node: &RcRecordNode) -> Self {
        Self(Rc::clone(&node.0))
    }

    fn new(node: RecordNode) -> Self {
        Self(Rc::new(RefCell::new(node)))
    }

    /// 见 [`RecordNode::push_child`]
    fn push_child(&self, child: RecordNode) -> RcRecordNode {
        RecordNode::push_child(self, child)
    }

    /// 见 [`RcRecordNode::push_rc_child`]
    fn push_rc_child(&self, child: &RcRecordNode) {
        RecordNode::push_rc_child(self, child)
    }

    /// 见 [`RecordNode::find_direct_parent`]
    fn find_direct_parent_of(&self, child: &RecordNode) -> Option<RcRecordNode> {
        RecordNode::find_direct_parent(self, child)
    }
}

impl PartialEq for RcRecordNode {
    fn eq(&self, other: &Self) -> bool {
        self.borrow().eq(&other.borrow())
    }
}

impl Eq for RcRecordNode {}

impl Hash for RcRecordNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.borrow().hash(state);
    }
}

impl Debug for RcRecordNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Rc")?; // 在结构体名称前面加上 Rc
        self.borrow().fmt(f)
    }
}

impl std::fmt::Display for RcRecordNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.borrow().path.to_string_lossy())
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
pub(crate) struct RecordNode {
    raw_record: RawRecord,
    children: Vec<RcRecordNode>,
    parent: Weak<RefCell<RecordNode>>,
}

impl AsRef<RawRecord> for RecordNode {
    fn as_ref(&self) -> &RawRecord {
        &self.raw_record
    }
}

impl PartialEq for RecordNode {
    fn eq(&self, other: &Self) -> bool {
        self.raw_record == other.raw_record
    }
}

impl Hash for RecordNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw_record.hash(state);
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

    fn clear_parent(&mut self) {
        self.parent = Weak::new();
    }

    /// 尝试获取父节点的强引用.
    fn parent(&self) -> Option<RcRecordNode> {
        self.parent.upgrade().map(|x| RcRecordNode(x))
    }

    pub(crate) fn children(&self) -> &[RcRecordNode] {
        &self.children
    }

    /// 将节点 child 放入 this 的直接子节点列表中, 返回对应子节点的一个强引用.
    ///
    /// 如果需要将 child 放入合适的父节点, 可以使用 [`RecordNode::find_direct_parent`] 然后在返回值调用此方法.
    fn push_child(this: &RcRecordNode, mut child: RecordNode) -> RcRecordNode {
        child.set_parent(this);
        let rc = RcRecordNode(Rc::new(RefCell::new(child)));
        this.borrow_mut().children.push(RcRecordNode::clone(&rc));
        rc
    }

    /// 类似 [`RecordNode::push_child`], 但是参数 child 是 [`RcRecordNode`],
    /// 放入的是 child record 引用的拷贝.
    fn push_rc_child(this: &RcRecordNode, child: &RcRecordNode) {
        child.borrow_mut().set_parent(this);
        this.borrow_mut().children.push(RcRecordNode::clone(child));
    }

    /// 从当前节点 this 开始向下查找, 找 child 期望的直接父节点.
    ///
    /// 比如:
    /// 如果当前节点为 `/a`, child 为 `/a/b/c/d`,
    /// 那么 `/a/b/c` 就是 child 期望的直接父节点.
    ///
    /// # Example
    ///
    /// ```text
    /// if let Some(parent) = RecordNode::find_direct_parent(&root_node, &child_node) {
    ///    RecordNode::push_child(&parent, child_node);
    /// }
    /// ```
    ///
    /// # Returns
    /// 如果找到了, 返回直接父节点的强引用, 否则返回 None.
    fn find_direct_parent(this: &RcRecordNode, child: &RecordNode) -> Option<RcRecordNode> {
        let borrowed_this = this.borrow();
        if child.path.starts_with(&borrowed_this.path) {
            // 判断是否是直接父目录
            let this_comp_cnt = borrowed_this.path.components().count();
            let child_comp_cnt = child.path.components().count();
            let expected_comp_cnt = match child_comp_cnt.checked_sub(1) {
                Some(x) => x,
                None => return None,
            };
            if this_comp_cnt == expected_comp_cnt {
                Some(RcRecordNode::clone(this))
            } else {
                for sub_this in borrowed_this.children() {
                    if let Some(p) = RecordNode::find_direct_parent(sub_this, child) {
                        return Some(p);
                    }
                }
                None
            }
        } else {
            None
        }
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
    ///   (wiztree 的所有排列方式都能保证子目录紧随直接父目录),
    ///   没有进行排序过.
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
                cur_rec = cur_rec.push_child(rec);
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
    /// 但是性能相比 [`SpaceDistribution::from_ordered_records`] 较差.
    fn from_unordered_records(records: &[RawRecord]) -> SpaceDistribution {
        let mut roots = Vec::new();
        // 一次全量拷贝
        let records_set: HashSet<_> = records.iter()
            .map(|raw| RcRecordNode::new(RecordNode::new(raw.clone())))
            .collect();
        // 一次全量引用
        let mut paths: Vec<_> = records.iter()
            .map(|raw_rec| &raw_rec.path)
            .collect();
        // 逐级排序, 保证父目录在子目录之前.
        paths.sort_unstable_by_key(|x| x.components().count());
        for p in paths {
            let mut has_parent = false;
            // 创建一个临时无用节点, 用于 hash
            // 一次对 Path 的拷贝
            let dummy_node = RcRecordNode::new(RecordNode::new(RawRecord::default_with_path(p.into())));
            let node = records_set.get(&dummy_node).unwrap();
            if let Some(parent_p) = p.parent() {
                // 一次对 Path 的拷贝
                let dummy_parent_node = RcRecordNode::new(RecordNode::new(RawRecord::default_with_path(parent_p.into())));
                if let Some(parent_node) = records_set.get(&dummy_parent_node) {
                    parent_node.push_rc_child(node);
                    has_parent = true;
                }
            };
            if !has_parent {
                roots.push(RcRecordNode::clone(node));
            }
        }
        SpaceDistribution { roots }
    }

    /// 从 csv 文件中构建 [`SpaceDistribution`].
    ///
    /// csv 文件内容需要是从 WizTree 从文件中直接导出(WizTree 内部排序任意皆可)而不经过任何顺序调整的.
    pub fn from_csv_file(file: impl AsRef<Path>) -> Result<SpaceDistribution, Error> {
        let records = SpaceDistribution::records_from_csv_file(file)?;
        Ok(SpaceDistribution::from_ordered_records(&records))
    }

    /// 从 csv 文件中构建 [`SpaceDistribution`].
    ///
    /// 同 [`Self::from_ordered_records`], 但是在牺牲性能的情况下允许 csv 的各行记录允许被打乱.
    pub fn from_unordered_csv_file(file: impl AsRef<Path>) -> Result<SpaceDistribution, Error> {
        let records = Self::records_from_csv_file(file)?;
        Ok(Self::from_unordered_records(&records))
    }

    fn records_from_csv_file(file: impl AsRef<Path>) -> Result<Vec<RawRecord>, Error> {
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
        Ok(records)
    }

    /// 放入新的根节点, 并返回该节点的一个强引用.
    fn push_root(&mut self, mut root: RecordNode) -> RcRecordNode {
        root.clear_parent();
        let rc = RcRecordNode(Rc::new(RefCell::new(root)));
        self.roots.push(RcRecordNode::clone(&rc));
        rc
    }

    pub(crate) fn iter_roots(&self) -> core::slice::Iter<RcRecordNode> {
        self.roots.iter()
    }

    /// 返回个节点路径的简单树状图字符串表示.
    ///
    /// - `max_depth` 表示显示的最大深度, 如果为 None, 则表示无限深度.
    pub fn format_tree(&self, max_depth: Option<usize>) -> String {
        let mut s = String::new();
        for (i, root) in self.iter_roots().enumerate() {
            let i = i + 1;
            let root = root.borrow();
            let root_path_string = ensure_path_sep(
                &mut root.path.to_string_lossy().to_string(),
                root.folder,
            ).to_string();
            s.push_str(&format!("Root {i}: {}\n", root_path_string));
            match max_depth {
                Some(0) => {
                    s.push_str("  ...\n");
                }
                None | Some(_) => {
                    for child in root.children() {
                        s.push_str(&SpaceDistribution::format_tree_node(
                            child, 1, max_depth,
                        ))
                    }
                }
            }
        }
        s
    }

    /// 返回一个节点路径的简单树状图字符串表示.
    ///
    /// - `depth` 表示当前节点的深度, 用于缩进, depth 为 1 时缩进 2 空格.
    /// - `max_depth` 表示显示的最大深度, 如果为 None, 则表示无限深度.
    #[inline(never)]
    fn format_tree_node(node: &RcRecordNode, depth: usize, max_depth: Option<usize>) -> String {
        let mut s = String::new();
        s.push_str(&"  ".repeat(depth));
        let node = node.borrow();
        let mut comp_name = node.path.iter()
            .last()
            .map_or_else(|| "[no name]".to_string(),
                         |x| x.to_string_lossy().to_string());
        s.push_str(&format!("{}\n", ensure_path_sep(
            &mut comp_name,
            node.folder,
        )));
        match max_depth {
            Some(d) if d == depth => {
                if node.folder && node.n_folders > 0 {
                    s.push_str(&"  ".repeat(depth + 1));
                    s.push_str("...\n");
                }
            }
            None | Some(_) => {
                for child in node.children() {
                    s.push_str(&SpaceDistribution::format_tree_node(child, depth + 1, max_depth));
                }
            }
        }
        s
    }
}

#[cfg(test)]
mod tests {
    use std::time::Instant;
    use super::*;

    #[test]
    fn size_of() {
        assert_eq!(
            96,
            size_of_val(&RawRecord::default())
        );
    }
    #[test]
    #[cfg(target_os = "windows")]
    fn prefix_path() {
        let root: PathBuf = "D:/".into();
        let a: PathBuf = "D:/a".into();
        let _b: PathBuf = "D:/a/b".into();
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
    fn canonicalize_path() {
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
        dbg!(PathBuf::from("D:/").canonicalize().unwrap().components().collect::<Vec<_>>());
    }

    #[test]
    #[cfg(target_os = "windows")]
    fn path_eq() {
        assert_eq!(PathBuf::from("a\\"), PathBuf::from("a"));
        assert_eq!(PathBuf::from("D:/a/b"), PathBuf::from("D:/a/b/"));
        assert_eq!(PathBuf::from("D:/a/b"), PathBuf::from("D:\\a\\b\\"));
        assert_eq!(PathBuf::from("D:\\a\\b"), PathBuf::from("D:\\a\\b\\"));
    }

    fn new_raw_record(path: impl AsRef<Path>) -> RawRecord {
        RawRecord::default_with_path(path.as_ref().into())
    }

    #[test]
    fn find_direct_parent() {
        let a = RcRecordNode::new(RecordNode::new(new_raw_record("a")));
        let b = RecordNode::new(new_raw_record("a/b/"));
        let c = RecordNode::new(new_raw_record("a/b/c/"));
        let d = RecordNode::new(new_raw_record("a/b/c/d/"));
        let e = RecordNode::new(new_raw_record("a/b/c/d/e"));
        matches!(RecordNode::find_direct_parent(&a, &b), Some(_));
        for node in [b, c, d, e] {
            let option = a.find_direct_parent_of(&node);
            option.unwrap()
                .push_child(node);
        }
        dbg!(a);
    }

    #[test]
    fn format_nodes() {
        let sd = SpaceDistribution::from_ordered_records(&[
            new_raw_record("a/"),
            new_raw_record("a/b/"),
            new_raw_record("a/b/c/"),
            new_raw_record("a/b/c/d/"),
            new_raw_record("a/b/c/d/e"),
            new_raw_record("a/b/c/f/"),
        ]);
        assert_eq!(
            r#"Root 1: a\
  b\
    c\
      d\
        e
      f\
"#,
            sd.format_tree(None)
        )
    }


    #[test]
    fn build_space_distribution_ordered() {
        let sd = SpaceDistribution::from_csv_file("example_data/example_small_partial.csv").unwrap();
        println!("{}", sd.format_tree(None));
    }

    #[test]
    fn bench_build_space_distribution_ordered_records() {
        let start = Instant::now();
        let sd = SpaceDistribution::from_csv_file("example_data/example_1.csv").unwrap();
        println!("Elapsed: {:?}", start.elapsed()); // 10.5s (260w记录)
        println!("{}", sd.format_tree(Some(1)));
    }

    #[test]
    fn build_sd_from_unordered_csv() {
        let sd = SpaceDistribution::from_unordered_csv_file("example_data/example_multi_roots.csv").unwrap();
        println!("{}", sd.format_tree(None));
    }

    #[test]
    fn bench_build_space_distribution_unordered_records() {
        let start = Instant::now();
        let sd = SpaceDistribution::from_unordered_csv_file("example_data/example_1.csv").unwrap();
        println!("Elapsed: {:?}", start.elapsed()); // 260w rows in 31.8670859s
        println!("{}", sd.format_tree(Some(1)));
    }
}