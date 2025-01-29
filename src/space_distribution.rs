use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::ops::{Deref, DerefMut};
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

    /// 见 [`RecordNode::find_direct_parent`]
    fn find_direct_parent_of(&self, child: &RecordNode) -> Option<RcRecordNode> {
        RecordNode::find_direct_parent(self, child)
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
    fn push_root(&mut self, mut root: RecordNode) -> RcRecordNode {
        root.clear_parent();
        let rc = RcRecordNode(Rc::new(RefCell::new(root)));
        self.roots.push(RcRecordNode::clone(&rc));
        rc
    }

    pub(crate) fn iter_roots(&self) -> core::slice::Iter<RcRecordNode> {
        self.roots.iter()
    }
}

#[cfg(test)]
mod tests {
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

    // 不是完整的构造方法, 没设置 folder
    fn new_raw_record(path: impl AsRef<Path>) -> RawRecord {
        let mut rec = RawRecord::default();
        rec.path = path.as_ref().into();
        rec
    }

    #[test]
    fn find_direct_parent() {
        let a = RcRecordNode::new(RecordNode::new(new_raw_record("a")));
        let b = RecordNode::new(new_raw_record("a/b"));
        let c = RecordNode::new(new_raw_record("a/b/c"));
        let d = RecordNode::new(new_raw_record("a/b/c/d"));
        let e = RecordNode::new(new_raw_record("a/b/c/d/e"));
        matches!(RecordNode::find_direct_parent(&a, &b), Some(_));
        for node in [b, c, d, e] {
            let option = a.find_direct_parent_of(&node);
            option.unwrap()
                .push_child(node);
        }
        dbg!(a);
    }
}