use crate::Error;
use serde::de::{SeqAccess, Visitor};
use serde::{de, Deserialize, Deserializer};
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::Read;
use std::ops::{Deref, DerefMut};
use std::path;
use std::path::{Component, Path, PathBuf};

/// 一个 [`RawRecord`] 记录了一个文件(夹)的基本信息.
/// 对应一个 WizTree 导出的 csv 文件的一行.
#[derive(Default, Debug, Clone)]
pub(crate) struct RawRecord {
    /// 文件名称,
    ///
    /// 在 Windows 下如果是文件夹, 那么以 `\` 结尾.
    pub(crate) path: PathBuf,
    /// 大小
    pub(crate) size: usize,
    /// 分配大小
    pub(crate) alloc: usize,
    /// 修改时间, "yyyy/mm/dd HH:mm:ss"
    pub(crate) modify_time: String,
    /// 属性
    /// 属性存储为一个值，并且是以下值的组合相加而成：
    /// 1 = 只读 (R), 2 = 隐藏 (H), 4 = 系统 (S), 32 = 存档 (A), 2048 = 压缩 (C)
    /// 例如，如果一个文件是只读且隐藏的，它的属性值将等于 3（1 + 2）
    ///
    /// 但是有例外, 有的文件的属性值可能为十进制 "268435456"
    pub(crate) attributes: usize,
    /// 文件数量
    pub(crate) n_files: usize,
    /// 文件夹数量
    pub(crate) n_folders: usize,
    /// 是否是文件夹
    pub(crate) folder: bool,
}

impl RawRecord {
    fn default_with_path(path: PathBuf) -> RawRecord {
        RawRecord {
            folder: path.to_string_lossy().ends_with(['/', '\\']),
            path,
            ..Self::default()
        }
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

#[cfg(not(feature = "sync"))]
mod arc {
    pub(super) type RefCount<T> = std::rc::Rc<T>;
    pub(super) type Weak<T> = std::rc::Weak<T>;
    pub(super) use std::cell::RefCell;
}
#[cfg(feature = "sync")]
mod arc {
    use std::ops::Deref;

    pub(super) type RefCount<T> = std::sync::Arc<T>;
    pub(super) type Weak<T> = std::sync::Weak<T>;

    pub struct RefCell<T: ?Sized>(std::cell::RefCell<T>);
    unsafe impl<T: ?Sized> Sync for RefCell<T> {}
    unsafe impl<T: ?Sized> Send for RefCell<T> {}
    impl<T: ?Sized> Deref for RefCell<T> {
        type Target = std::cell::RefCell<T>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
    impl<T> RefCell<T> {
        pub(super) fn new(value: T) -> Self {
            Self(std::cell::RefCell::new(value))
        }
    }
}
use arc::{RefCount, Weak, RefCell};

/// 可以实现一个简短 Debug, Display 输出的 [`RefCount<RefCell<RecordNode>>`] 类型
pub(crate) struct RcRecordNode(RefCount<RefCell<RecordNode>>);

impl RcRecordNode {
    pub(crate) fn clone(node: &RcRecordNode) -> Self {
        Self(RefCount::clone(&node.0))
    }

    fn new(node: RecordNode) -> Self {
        Self(RefCount::new(RefCell::new(node)))
    }

    /// 见 [`RecordNode::push_child`]
    fn push_child(&self, child: RecordNode) -> RcRecordNode {
        RecordNode::push_child(self, child)
    }

    /// 见 [`RcRecordNode::push_rc_child`]
    fn push_rc_child(&self, child: &RcRecordNode) {
        RecordNode::push_rc_child(self, child)
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
    type Target = RefCount<RefCell<RecordNode>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for RcRecordNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AsRef<RefCount<RefCell<RecordNode>>> for RcRecordNode {
    fn as_ref(&self) -> &RefCount<RefCell<RecordNode>> {
        &self.0
    }
}

/// 一个记录节点, 用于构建 [`Snapshot`] 文件树.
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
        self.parent = RefCount::downgrade(parent);
    }

    fn clear_parent(&mut self) {
        self.parent = Weak::new();
    }

    /// 尝试获取父节点的强引用.
    pub(crate) fn parent(&self) -> Option<RcRecordNode> {
        self.parent.upgrade().map(RcRecordNode)
    }

    pub(crate) fn children(&self) -> &[RcRecordNode] {
        &self.children
    }

    /// 将节点 child 放入 this 的直接子节点列表中, 返回对应子节点的一个强引用.
    fn push_child(this: &RcRecordNode, mut child: RecordNode) -> RcRecordNode {
        child.set_parent(this);
        let rc = RcRecordNode::new(child);
        this.borrow_mut().children.push(RcRecordNode::clone(&rc));
        rc
    }

    /// 类似 [`Self::push_child`], 但是参数 child 是 [`RcRecordNode`],
    /// 放入的是 child record 引用的拷贝.
    fn push_rc_child(this: &RcRecordNode, child: &RcRecordNode) {
        child.borrow_mut().set_parent(this);
        this.borrow_mut().children.push(RcRecordNode::clone(child));
    }

    /// 查找直接子节点,
    /// 返回第一个满足"节点路径相对于 self 路径开始的第一个组成部分和 `comp` 相等"的子节点.
    pub(crate) fn find_child(&self, comp: Component) -> Option<RcRecordNode> {
        self.children().iter()
            // 这里假设所有的子节点的路径都是以当前节点路径为前缀且和当前节点路径不相同的.
            .find(|child|
                comp == child.borrow().path
                    .strip_prefix(&self.path).unwrap()
                    .components().next().unwrap()
            )
            .map(RcRecordNode::clone)
    }
}

/// 空间分布快照, 储存着文件信息.
///
/// 对应的是文件 WizTree 产生的一个 csv 文件.
///
/// 此结构会对 csv 文件的进行树状结构的解析.
/// 这个树状结构可能不止一个根节点.
#[derive(Debug)]
pub struct Snapshot {
    roots: Vec<RcRecordNode>,
}

impl Snapshot {
    /// 放入新的根节点, 并返回该节点的一个强引用.
    fn push_root(&mut self, mut root: RecordNode) -> RcRecordNode {
        root.clear_parent();
        let rc = RcRecordNode::new(root);
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
                        s.push_str(&Snapshot::format_tree_node(
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
                    s.push_str(&Snapshot::format_tree_node(child, depth + 1, max_depth));
                }
            }
        }
        s
    }

    /// 获取所有根节点的分配大小总和.
    pub fn total_alloc(&self) -> usize {
        self.iter_roots().map(|x| x.borrow().alloc).sum()
    }

    /// 获取所有根节点的大小总和.
    pub fn total_size(&self) -> usize {
        self.iter_roots().map(|x| x.borrow().size).sum()
    }

    /// 获取所有根节点的文件数量总和.
    pub fn total_n_files(&self) -> usize {
        self.iter_roots().map(|x| x.borrow().n_files).sum()
    }

    /// 获取所有根节点的文件夹数量总和.
    pub fn total_n_folders(&self) -> usize {
        self.iter_roots().map(|x| x.borrow().n_folders).sum()
    }

    /// 获取各个根节点的最长公共路径前缀.
    ///
    /// 如果 [`Snapshot`] 为空, 返回 None
    pub fn get_common_path_prefix(&self) -> Option<PathBuf> {
        let mut roots = self.iter_roots();
        let root = roots.next()?;
        let mut prefix = root.borrow().path.clone();
        for r in roots {
            let mut new_prefix = PathBuf::new();
            for (a, b) in prefix.components().zip(r.borrow().path.components()) {
                if a != b {
                    break;
                }
                new_prefix.push(a);
            }
            prefix = new_prefix;
        }
        Some(prefix)
    }

    /// 查找路径和 [`path`] 的根节点.
    pub(crate) fn find_root(&self, path: impl AsRef<Path>) -> Option<RcRecordNode> {
        self.iter_roots()
            .find(|r| r.borrow().path == path.as_ref())
            .map(RcRecordNode::clone)
    }

    /// 获取指定路径(路径需要从根节点开始表示)下的节点, 如果节点不存在返回 None.
    pub(crate) fn search_node(&self, path: impl AsRef<Path>) -> Option<RcRecordNode> {
        let path = path.as_ref();
        let mut cur_path = PathBuf::from("");
        let mut node: Option<RcRecordNode> = None;
        for comp in path.components() {
            // 更新 cur_path
            match comp {
                Component::ParentDir => { cur_path.pop(); }
                Component::CurDir => continue,
                Component::Prefix(_) if cfg!(windows) => {
                    cur_path.push(comp);
                    cur_path.push(Component::RootDir); // windows 下自动添加 "\\"
                }
                Component::RootDir if cfg!(windows) => continue,
                _ => { cur_path.push(comp); }
            }
            if node.is_some() {
                if comp == Component::ParentDir {
                    node = node.unwrap().borrow().parent();
                } else {
                    node = node.unwrap().borrow().find_child(comp)
                }
            } else {
                node = self.find_root(&cur_path);
            }
        }
        #[cfg(test)]
        {
            if let Some(node) = &node {
                assert_eq!(node.borrow().path, cur_path); // cur_path 是标准化的参数 path
            }
        }
        node
    }
}

pub(crate) mod builder {
    use std::io;
    use super::*;
    use std::io::{BufRead, BufReader, Chain, Cursor, Seek};
    use std::time::{Duration, Instant};
    use serde::Serialize;

    struct InspectReader<R: Read, F: FnMut(usize)> {
        reader: R,
        /// 每次读取时调用, 参数为本次读取的字节数
        inspect_handler: F,
    }

    impl<R: Read, F: FnMut(usize)> InspectReader<R, F> {
        fn new(reader: R, inspect_handler: F) -> Self {
            InspectReader {
                reader,
                inspect_handler,
            }
        }
    }

    impl<R: Read, F: FnMut(usize)> Read for InspectReader<R, F> {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            let n = self.reader.read(buf)?;
            (self.inspect_handler)(n);
            Ok(n)
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq, Serialize)]
    #[serde(tag = "type", content = "data")]
    pub enum Message {
        /// 开始构建, 包含一个总字节数
        Start(usize),
        /// 读取 csv 内容中
        Reading {
            /// 当前已读取的字节数
            current: usize,
            /// 距离上一次报告读取的字节数
            delta: usize,
            /// 总字节数
            total: usize,
        },
        /// 内容读取完毕
        ReadDone,
        /// 正在排序 records
        ///
        /// # Note
        /// 只会在 `ordered == false` 的时候出现此消息
        Sorting,
        /// 排序完毕
        ///
        /// # Note
        /// 只会在 `ordered == false` 的时候出现此消息
        SortDone,
        /// 加载分析内容
        ///
        /// # Note
        /// record 对应 csv 中的一行.
        Processing {
            /// 当前已处理的 records 数
            current: usize,
            /// 距离上一次报告处理的 records 数
            delta: usize,
            /// 总 records 数
            total: usize,
        },
        /// 构建完成
        Finished,
        /// 错误, 错误信息可以在返回的 Result 中找到
        Error,
    }

    pub trait Reporter {
        fn report(&mut self, msg: Message);
    }

    impl<F: FnMut(Message)> Reporter for F {
        fn report(&mut self, msg: Message) {
            self(msg);
        }
    }

    impl Reporter for () {
        fn report(&mut self, _: Message) {}
    }

    /// [`Reporter`] 被调用的间隔.
    /// 只会对 [`Message::Reading`] 消息生效.
    #[derive(Copy, Eq, PartialEq, Debug, Clone)]
    pub enum ReportReadingInterval {
        /// 至少间隔时间 t 才报告一次
        Time(Duration),
        /// 至少读取 n 字节才报告一次
        Bytes(usize),
        /// 至少进行 r 次读取操作才报告一次
        Count(usize),
        /// 不设置间隔, 每次读取操作都报告
        Zero,
    }

    /// [`Reporter`] 被调用的间隔.
    /// 只会对 [`Message::Processing`] 消息生效.
    #[derive(Copy, Eq, PartialEq, Debug, Clone)]
    pub enum ReportProcessingInterval {
        /// 至少间隔时间 t 才报告一次
        Time(Duration),
        /// 至少处理 r 个 records 才报告一次
        Records(usize),
        /// 不设置间隔, 每次读取操作都报告
        Zero,
    }

    struct ReadingThrottler {
        interval: ReportReadingInterval,
        last: Option<Instant>,
        bytes: usize,
        count: usize,
    }
    impl ReadingThrottler {
        fn new(interval: ReportReadingInterval) -> Self {
            Self {
                interval,
                last: None,
                bytes: 0,
                count: 0,
            }
        }

        fn throttle(&mut self, read_bytes: usize) -> bool {
            match self.interval {
                ReportReadingInterval::Time(duration) => {
                    let now = Instant::now();
                    let elapsed = self.last.map_or(true, |t| now.duration_since(t) >= duration);
                    if elapsed { self.last = Some(now); }
                    elapsed
                }
                ReportReadingInterval::Bytes(bytes) => {
                    self.bytes += read_bytes;
                    if self.bytes >= bytes {
                        if bytes == 0 { self.bytes = 0 } else { self.bytes %= bytes }
                        true
                    } else {
                        false
                    }
                }
                ReportReadingInterval::Count(count) => {
                    self.count += 1;
                    if self.count >= count {
                        self.count = 0;
                        true
                    } else {
                        false
                    }
                }
                ReportReadingInterval::Zero => true,
            }
        }
    }

    struct ProcessingThrottler {
        interval: ReportProcessingInterval,
        last: Option<Instant>,
        count: usize,
    }

    impl ProcessingThrottler {
        fn new(interval: ReportProcessingInterval) -> Self {
            Self {
                interval,
                last: None,
                count: 0,
            }
        }

        fn throttle(&mut self) -> bool {
            match self.interval {
                ReportProcessingInterval::Time(duration) => {
                    let now = Instant::now();
                    let elapsed = self.last.map_or(true, |t| now.duration_since(t) >= duration);
                    if elapsed { self.last = Some(now); }
                    elapsed
                }
                ReportProcessingInterval::Records(r) => {
                    self.count += 1;
                    let w = self.count >= r;
                    if w { self.count = 0; }
                    w
                }
                ReportProcessingInterval::Zero => true,
            }
        }
    }

    pub struct Builder<P: Reporter = ()> {
        reporter: Option<RefCell<P>>,
        reading_interval: ReportReadingInterval,
        processing_interval: ReportProcessingInterval,
    }

    impl Default for Builder<()> {
        /// 构建一个不能提供 [`Reporter`] 的 [`Builder`].
        fn default() -> Self {
            Self::new()
        }
    }

    impl<P: Reporter> Builder<P> {
        pub fn new() -> Builder<P> {
            Builder {
                reporter: None,
                reading_interval: ReportReadingInterval::Zero,
                processing_interval: ReportProcessingInterval::Zero,
            }
        }

        fn report_msg(&self, msg: Message) {
            if let Some(r) = self.reporter.as_ref() {
                r.borrow_mut().report(msg)
            }
        }

        pub fn set_reporter(&mut self, reporter: P) -> &mut Self {
            self.reporter = Some(RefCell::new(reporter));
            self
        }

        pub fn set_reading_report_interval(&mut self, interval: ReportReadingInterval) -> &mut Self {
            self.reading_interval = interval;
            self
        }

        pub fn set_processing_report_interval(&mut self, interval: ReportProcessingInterval) -> &mut Self {
            self.processing_interval = interval;
            self
        }

        /// 同 [`Builder::build_from_file`] 但是直接使用 csv 文件的内容.
        pub fn build_from_content<R: Read + Seek>(&self, mut reader: R, ordered: bool) -> Result<Snapshot, Error> {
            let total = reader.seek(io::SeekFrom::End(0))? as usize;
            reader.seek(io::SeekFrom::Start(0))?;
            let mut current = 0usize;
            let mut reported = 0usize;
            let mut throttler = ReadingThrottler::new(self.reading_interval);
            self.report_msg(Message::Start(total));
            let rst = self.build_from_csv_content(InspectReader::new(
                reader, |n| {
                    current += n;
                    if throttler.throttle(n) {
                        let delta = current - reported;
                        self.report_msg(Message::Reading { current, delta, total });
                        reported = current;
                    }
                },
            ), ordered);
            self.report_msg(if rst.is_ok() {
                Message::Finished
            } else {
                Message::Error
            });
            rst
        }


        /// 从 csv 文件中构建 [`Snapshot`].
        ///
        /// 如果 `ordered` 为 `true`, 则不允许 csv 在从 WizTree 导出后调换行顺序.
        /// 如果 `ordered` 为 `false`, 则会在牺牲性能的情况下允许 csv 的各行记录允许被打乱.
        pub fn build_from_file(&self, path: impl AsRef<Path>, ordered: bool) -> Result<Snapshot, Error> {
            self.build_from_content(File::open(path)?, ordered)
        }

        /// 从 [记录](RawRecord) 中构建一个 [空间分布快照](Snapshot).
        ///
        /// 此方法会对输入参数的所有数据进行拷贝.
        ///
        /// 此方法假定:
        /// - 原始记录数组是按照原始 csv 文件中的行顺序排列的
        ///   (wiztree 的所有排列方式都能保证子目录紧随直接父目录),
        ///   没有进行顺序调整过.
        /// - RawRecord 中所有的路径都是已经标准化的, 不包含 `..` 或 `.` 或符号链接等内容.
        /// - 没有重复路径的 RawRecord.
        /// - 一个 RawRecord 的路径如果存在, 那么在它的逐级父路径对应的 RawRecord 都在它之前存在, 直到根节点.
        ///   比如:
        ///   > 如果 `/a/b/c/d` 存在, csv 中的根目录为 `/a/b`,
        ///   > 那么 `/a/b/c` 和 `/a/b` 一定存在且在 `/a/b/c/d` 之前.
        ///
        /// 如果传入已经排序过的记录, 会产生错误地结果,
        /// 此时应该使用性能较劣的 [`Builder::process_unordered_records`]
        /// 以获得正确的输出.
        pub(super) fn process_ordered_records(&self, records: &[RawRecord]) -> Snapshot {
            if records.is_empty() {
                return Snapshot { roots: Vec::new() }; // 空值
            }
            let mut sd = Snapshot { roots: Vec::new() };
            let mut cur_rec = sd.push_root(
                RecordNode::new(records[0].clone()) // 第一个 record 一定是根目录之一.
            );
            let mut current = 1;
            let mut reported = 0usize;
            let mut throttler = ProcessingThrottler::new(self.processing_interval);
            macro_rules! throttle_report {
                () => {
                    if throttler.throttle() {
                        self.report_msg(Message::Processing {
                            current,
                            delta: current - reported,
                            total: records.len(),
                        });
                        reported = current;
                    } 
                }
            }
            throttle_report!();
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

                current += 1;
                throttle_report!();
            }
            sd
        }

        /// 从 [记录](RawRecord) 中构建一个 [空间分布快照](Snapshot).
        ///
        /// 此方法会对输入参数的所有数据进行拷贝.
        ///
        /// 此方法假定:
        /// - RawRecord 中所有的路径都是已经解析过的, 不包含 `..` 或 `.` 或符号链接等内容.
        /// - 没有重复路径的 RawRecord.
        ///
        /// 此方法允许输入 records 数组是乱序的,
        /// 但是性能相比 [`Builder::process_ordered_records`] 较差.
        pub(super) fn process_unordered_records(&self, records: &[RawRecord]) -> Snapshot {
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
            self.report_msg(Message::Sorting);
            paths.sort_unstable_by_key(|x| x.components().count());
            self.report_msg(Message::SortDone);

            let mut throttler = ProcessingThrottler::new(self.processing_interval);
            let mut reported = 0usize;
            for (i, p) in paths.iter().enumerate() {
                let mut has_parent = false;
                // 创建一个临时无用节点, 用于 hash
                // 一次对 Path 的拷贝
                let dummy_node = RcRecordNode::new(RecordNode::new(RawRecord::default_with_path(p.into())));
                let node = records_set.get(&dummy_node).unwrap(); // 此节点一定存在
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
                if throttler.throttle() {
                    let current = i + 1; // index -> count
                    self.report_msg(Message::Processing {
                        current,
                        delta: current - reported,
                        total: paths.len(),
                    });
                    reported = current;
                }
            }
            Snapshot { roots }
        }

        pub(super) fn build_from_csv_content(&self, csv_content: impl Read, ordered: bool) -> Result<Snapshot, Error> {
            let records = self.read_records_from_csv_content(csv_content)?;
            self.report_msg(Message::ReadDone);
            Ok(if ordered {
                self.process_ordered_records(&records)
            } else {
                self.process_unordered_records(&records)
            })
        }

        pub(super) fn read_records_from_csv_content(&self, csv_content: impl Read) -> Result<Vec<RawRecord>, Error> {
            let mut reader = build_csv_reader(csv_content)?;
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
    }


    /// 以一个 [`Read`] 作为内容, 创建一个 CSV Reader,
    /// 并自动跳过可能的 header 行之前的无效行.
    ///
    /// 此函数假定:
    /// - 每个 csv 文件都有至少一个 header 行.
    ///
    /// # Parameters
    /// - `content_reader`: csv 内容, 注意不是 csv 文件路径.
    pub(super) fn build_csv_reader<R: Read>(
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::builder::*;
    use std::time::{Duration, Instant};

    #[test]
    fn size_of() {
        assert_eq!(
            104,
            size_of_val(&RawRecord::default())
        );
    }

    fn new_raw_record(path: impl AsRef<Path>) -> RawRecord {
        RawRecord::default_with_path(path.as_ref().into())
    }

    #[test]
    fn common_path_prefix() {
        let sd = Builder::default().process_ordered_records(&[
            new_raw_record("a/b/c/d/"),
            new_raw_record("a/b/c/f/"),
            new_raw_record("a/b/go/g/"),
            new_raw_record("a/b/c/h/"),
        ]);
        assert_eq!(sd.get_common_path_prefix().unwrap(), PathBuf::from("a/b/"));
        #[cfg(windows)]
        {
            let sd = Builder::default().process_ordered_records(&[
                new_raw_record("D:/"),
                new_raw_record("C:/"),
            ]);
            assert_eq!(sd.get_common_path_prefix().unwrap(), PathBuf::from(""));
        }
    }

    #[test]
    fn format_nodes() {
        let sd = Builder::default().process_ordered_records(&[
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
    fn build_snapshot_ordered() {
        let sd = Builder::default().build_from_file(
            "example_data/example_small_partial.csv",
            true,
        ).unwrap();
        println!("{}", sd.format_tree(None));
    }

    #[test]
    fn bench_build_snapshot_ordered_records() {
        let start = Instant::now();
        let sd = Builder::default().build_from_file(
            "example_data/example_old.csv",
            true,
        ).unwrap();
        println!("Elapsed: {:?}", start.elapsed()); // 10.5s (260w记录)
        println!("{}", sd.format_tree(Some(1)));
    }

    #[test]
    fn build_sd_from_unordered_csv() {
        let sd = Builder::default().build_from_file(
            "example_data/example_multi_roots.csv",
            false,
        ).unwrap();
        println!("{}", sd.format_tree(None));
    }

    #[test]
    fn bench_build_space_distribution_unordered_records() {
        let start = Instant::now();
        let sd = Builder::default().build_from_file(
            "example_data/example_old.csv",
            false,
        ).unwrap();
        println!("Elapsed: {:?}", start.elapsed()); // 260w rows in 31.8670859s
        println!("{}", sd.format_tree(Some(1)));
    }

    #[test]
    fn builder() {
        let snapshot = Builder::new()
            .set_reporter(|m| {
                dbg!(m);
            })
            .set_reading_report_interval(ReportReadingInterval::Zero)
            .build_from_file("example_data/example_small_partial.csv", true)
            .unwrap();
        println!("{}", snapshot.format_tree(None));
    }

    #[test]
    fn parse_csv_records() {
        let start = Instant::now();
        let mut reader =
            build_csv_reader(File::open("example_data/example_old.csv").unwrap()).unwrap();
        let mut throttle = Throttler::new(Duration::from_secs(1));
        for (i, record) in reader.records().enumerate() {
            let record = record.unwrap();
            // let v: Vec<_> = record.iter().collect();
            let raw_record: RawRecord = record.deserialize(None).unwrap();
            throttle.throttle_run(|| println!("{i}: {:?}", raw_record));
        }
        println!("Elapsed: {:?}", start.elapsed()); // 260w rows in 6.34s
    }

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
}