use std::cmp::Ordering;
use std::fmt;
use std::fmt::Formatter;
use std::path::{Component, Path, PathBuf};
use serde::Serialize;
use crate::snapshot::{RcRecordNode, Snapshot};

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum DiffError {
    #[error("Failed to diff given node for both node being None")]
    BothNone,
    #[error("Path between two node are in different paths")]
    PathNEqual,
    #[error("Node type mismatch, one is folder, the other is file")]
    TypeMismatch,
}

#[derive(Debug, Copy, Clone, Serialize)]
pub enum DiffKind {
    /// 节点新建
    New,
    /// 节点删除
    Removed,
    /// 节点变化
    Changed,
}

/// 用于比较一对相同路径的文件夹或者文件的节点
#[derive(Serialize)]
pub struct DiffNode {
    kind: DiffKind,
    // dummy node 的 path 为 None.
    path: Option<PathBuf>,
    folder: bool,
    delta_size: isize,
    delta_alloc: isize,
    delta_n_files: isize,
    delta_n_folders: isize,
    /// newer_side_node 和 older_side_node 同时为 None 时是 dummy_node.
    /// newer_side_node 和 older_side_Node 同时为 Some 时, 它们的 path 相同.
    #[serde(skip)]
    newer_side_node: Option<RcRecordNode>,
    #[serde(skip)]
    older_side_node: Option<RcRecordNode>,
}

impl Clone for DiffNode {
    fn clone(&self) -> Self {
        DiffNode {
            kind: self.kind,
            path: self.path.clone(),
            folder: self.folder,
            delta_size: self.delta_size,
            delta_alloc: self.delta_alloc,
            delta_n_files: self.delta_n_files,
            delta_n_folders: self.delta_n_folders,
            newer_side_node: self.newer_side_node.as_ref().map(RcRecordNode::clone),
            older_side_node: self.older_side_node.as_ref().map(RcRecordNode::clone),
        }
    }
}

impl fmt::Debug for DiffNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        struct DebugOption<T>(Option<T>);
        impl<T: fmt::Debug> fmt::Debug for DebugOption<T> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                match &self.0 {
                    Some(v) => write!(f, "Some({:?})", v),
                    None => write!(f, "None"),
                }
            }
        }

        f.debug_struct("DiffNode")
            .field("kind", &self.kind)
            .field("path", &self.path)
            .field("is_folder", &self.folder)
            .field("delta_size", &self.delta_size)
            .field("delta_alloc", &self.delta_alloc)
            .field("delta_n_files", &self.delta_n_files)
            .field("delta_n_dirs", &self.delta_n_folders)
            .field("newer_side_node", &DebugOption(
                self.newer_side_node.as_ref().map(
                    |node| node.borrow().path.to_string_lossy().to_string()
                )
            ))
            .field("older_side_node", &DebugOption(
                self.older_side_node.as_ref().map(
                    |node| node.borrow().path.to_string_lossy().to_string()
                )
            ))
            .finish()
    }
}

impl DiffNode {
    /// 接收两个节点, 产生一个比较节点;
    /// - 如果两个节点都为 None, 返回 None.
    /// - 如果两个节点的路径不同, 返回 None.
    fn new(newer_side_node: Option<RcRecordNode>, older_side_node: Option<RcRecordNode>) -> Result<Self, crate::Error> {
        let (kind, path, folder, delta_size, delta_alloc, delta_n_files, delta_n_folders) = match (&newer_side_node, &older_side_node) {
            (Some(new), None) => {
                let new = new.borrow();
                (
                    DiffKind::New,
                    Some(new.path.clone()),
                    new.folder,
                    new.size as isize,
                    new.alloc as isize,
                    new.n_files as isize,
                    new.n_folders as isize
                )
            }
            (None, Some(old)) => {
                let old = old.borrow();
                (
                    DiffKind::Removed,
                    Some(old.path.clone()),
                    old.folder,
                    -(old.size as isize),
                    -(old.alloc as isize),
                    -(old.n_files as isize),
                    -(old.n_folders as isize)
                )
            }
            (Some(new), Some(old)) => {
                let new = new.borrow();
                let old = old.borrow();
                if new.folder != old.folder {
                    Err(DiffError::TypeMismatch)?
                } else if new.path != old.path {
                    Err(DiffError::PathNEqual)?
                } else {
                    (
                        DiffKind::Changed,
                        Some(new.path.clone()),
                        new.folder,
                        new.size as isize - old.alloc as isize,
                        new.alloc as isize - old.alloc as isize,
                        new.n_files as isize - old.n_files as isize,
                        new.n_folders as isize - old.n_folders as isize
                    )
                }
            }
            (None, None) => Err(DiffError::BothNone)?
        };
        Ok(DiffNode {
            kind,
            path,
            folder,
            delta_size,
            delta_alloc,
            delta_n_files,
            delta_n_folders,
            newer_side_node,
            older_side_node,
        })
    }

    fn dummy() -> DiffNode {
        DiffNode {
            kind: DiffKind::Changed,
            path: None,
            folder: true,
            delta_size: 0,
            delta_alloc: 0,
            delta_n_files: 0,
            delta_n_folders: 0,
            newer_side_node: None,
            older_side_node: None,
        }
    }

    pub fn is_dummy(&self) -> bool {
        self.newer_side_node.is_none() && self.older_side_node.is_none()
    }

    /// 获取 newer_side_node 和 older_side_node 的父节点.
    pub(crate) fn get_parents(&self) -> (Option<RcRecordNode>, Option<RcRecordNode>) {
        (self.newer_side_node.as_ref().and_then(|node| node.borrow().parent()),
         self.older_side_node.as_ref().and_then(|node| node.borrow().parent()))
    }

    /// 同时对正在观察对比的两个节点调用 [`RecordNode::find_child`], 并返回其结果.
    fn find_children(&self, comp: Component) -> (Option<RcRecordNode>, Option<RcRecordNode>) {
        (self.newer_side_node.as_ref().and_then(|n| n.borrow().find_child(comp)),
         self.older_side_node.as_ref().and_then(|n| n.borrow().find_child(comp)))
    }

    /// 获取当前节点表示的路径, 如果当前节点是 dummy node, 则返回 None.
    pub fn path(&self) -> Option<PathBuf> {
        self.path.clone()
    }

    /// 节点是否为文件夹
    pub fn is_folder(&self) -> bool {
        self.folder
    }

    /// 节点是否为文件
    pub fn is_file(&self) -> bool {
        !self.folder
    }

    /// 节点大小变化
    pub fn delta_size(&self) -> isize {
        self.delta_size
    }

    /// 节点分配大小变化
    pub fn delta_alloc(&self) -> isize {
        self.delta_alloc
    }

    /// 节点文件数量变化
    pub fn delta_n_files(&self) -> isize {
        self.delta_n_files
    }

    /// 节点文件夹数量变化
    pub fn delta_n_folders(&self) -> isize {
        self.delta_n_folders
    }

    /// 节点变化类型
    pub fn kind(&self) -> DiffKind {
        self.kind
    }
}

/// 两个 [`Snapshot`] 的对比,
/// 此结构体对两者进行借用而不拷贝数据.
#[derive(Debug)]
#[cfg(not(feature = "owning_diff"))]
pub struct Diff<'s> {
    /// 比较中较新的 [`Snapshot`]
    newer: &'s Snapshot,
    /// 比较中较旧的 [`Snapshot`]
    older: &'s Snapshot,
    /// 当前在观察的节点.
    ///
    /// Diff 构造初始时为 dummy node, 表示比较两个 Snapshot 的各个根节点.
    ///
    /// dummy node:
    /// - `newer_side_node` 和 `older_side_node` 都为 None
    /// - `kind` 为 [`DiffKind::Changed`],
    /// - `path` 为 None,
    /// - `folder` 为 true,
    current_node: DiffNode,
    /// 当前在观察的节点的子节点, Diff 构造初始时为空
    nodes: Vec<DiffNode>,
}

#[derive(Debug)]
#[cfg(feature = "owning_diff")]
pub struct Diff {
    newer: Snapshot,
    older: Snapshot,
    current_node: DiffNode,
    nodes: Vec<DiffNode>,
}

macro_rules! impl_diff {
    () => {
        fn diff_records<'t, I1, I2>(newer: I1, older: I2) -> Vec<DiffNode>
        where
            I1: Iterator<Item=&'t RcRecordNode>,
            I2: Iterator<Item=&'t RcRecordNode>,
        {
            let mut newer: Vec<_> = newer.collect();
            let mut older: Vec<_> = older.collect();
            // 可能这里的排序会导致性能问题, 但是这里应该是非热点代码.
            fn cmp(a: &&RcRecordNode, b: &&RcRecordNode) -> Ordering {
                // 可能这里的 borrow 会导致性能问题
                a.borrow().path.cmp(&b.borrow().path)
            }
            newer.sort_unstable_by(cmp);
            older.sort_unstable_by(cmp);
            let mut newer_iter = newer.iter();
            let mut older_iter = older.iter();
            let mut newer_cur = newer_iter.next();
            let mut older_cur = older_iter.next();
            let mut rst = Vec::new();
            loop {
                match (newer_cur, older_cur) {
                    (Some(new), Some(old)) => {
                        let new = RcRecordNode::clone(new);
                        let old = RcRecordNode::clone(old);
                        match cmp(&&new, &&old) {
                            Ordering::Less => {
                                rst.push(DiffNode::new(Some(new), None).unwrap());
                                newer_cur = newer_iter.next();
                            }
                            Ordering::Equal => {
                                if new.borrow().folder == old.borrow().folder {
                                    rst.push(DiffNode::new(Some(new), Some(old)).unwrap());
                                } else {
                                    rst.push(DiffNode::new(Some(new), None).unwrap());
                                    rst.push(DiffNode::new(None, Some(old)).unwrap());
                                }
                                newer_cur = newer_iter.next();
                                older_cur = older_iter.next();
                            }
                            Ordering::Greater => {
                                rst.push(DiffNode::new(None, Some(old)).unwrap());
                                older_cur = older_iter.next();
                            }
                        }
                    }
                    (Some(new), None) => {
                        rst.push(DiffNode::new(Some(RcRecordNode::clone(new)), None).unwrap());
                        newer_cur = newer_iter.next();
                    }
                    (None, Some(old)) => {
                        rst.push(DiffNode::new(None, Some(RcRecordNode::clone(old))).unwrap());
                        older_cur = older_iter.next();
                    }
                    (None, None) => break
                }
            }
            rst
        }

        /// 观察对比一对新旧节点.
        fn view_node(&mut self, newer: Option<RcRecordNode>, older: Option<RcRecordNode>) -> Result<(), crate::Error> {
            if newer.is_none() && older.is_none() {
                Err(DiffError::BothNone)?;
            }
            let borrow_newer = newer.as_ref().map(|n| n.borrow());
            let borrow_older = older.as_ref().map(|n| n.borrow());
            if let (Some(newer), Some(older)) = (&borrow_newer, &borrow_older) {
                if newer.path != older.path {
                    Err(DiffError::PathNEqual)?;
                } else if newer.folder != older.folder {
                    Err(DiffError::TypeMismatch)?;
                }
            }
            self.nodes = Diff::diff_records(
                // Option iter flat_map 的结果是产生一个迭代器.
                // - 如果 Option 为 None, 那么代器返回 None.
                // - 如果 Option 为 Some, 将值 map 为一个迭代器(这里为迭代节点的子节点),
                //   然后产生的迭代器就是这个迭代器.
                borrow_newer.iter().flat_map(|n| n.children().iter()),
                borrow_older.iter().flat_map(|n| n.children().iter()),
            );
            drop(borrow_newer);
            drop(borrow_older);
            self.current_node = DiffNode::new(newer, older)?;
            Ok(())
        }

        /// 同 [`Self::view_node`], 但是如果新旧节点类型不同, 那么观察是文件夹的那个节点.
        fn pick_folder_to_view(&mut self, newer_node: Option<RcRecordNode>, older_node: Option<RcRecordNode>) -> Result<(), crate::Error> {
            match (newer_node, older_node) {
                (Some(newer_node), Some(older_node)) => {
                    let borrow_newer = newer_node.borrow();
                    let borrow_older = older_node.borrow();
                    if borrow_newer.path != borrow_older.path {
                        Err(DiffError::PathNEqual)?;
                    }
                    let new_is_folder = borrow_newer.folder;
                    let old_is_folder = borrow_older.folder;
                    drop((borrow_newer, borrow_older));
                    if new_is_folder != old_is_folder { // 如果一个是文件, 一个是文件夹, 那么观察文件夹.
                        if new_is_folder {
                            self.view_node(Some(newer_node), None)?;
                        } else {
                            self.view_node(None, Some(older_node))?;
                        }
                        Ok(())
                    } else {
                        self.view_node(Some(newer_node), Some(older_node))
                    }
                }
                (newer_node, older_node) => {
                    self.view_node(newer_node, older_node)
                }
            }
        }

        /// 观察并 diff 指定路径 `path` 节点下的子节点,
        /// - 如果路径在观察的两个 [`Snapshot`] 中都不存在, 则返回错误.
        /// - 如果 `path` 表示的节点在新旧其中一个 Snapshot 中表示为文件, 在另一个中表示为文件夹,
        ///   那么观察文件夹, 因为文件不可进入.
        /// - `path` 需要为完整的路径, 能够完整表示一个节点, 可以使用 `..` 或者 `.`, 但是不会解析符号链接.
        pub fn view_path(&mut self, path: impl AsRef<Path>) -> Result<(), crate::Error> {
            let newer_node = self.newer.search_node(&path);
            let older_node = self.older.search_node(&path);
            self.pick_folder_to_view(newer_node, older_node)
        }

        /// 按照 [`comp`] 改变观察路径.
        pub fn view_comp(&mut self, comp: Component) -> Result<(), crate::Error> {
            match comp {
                Component::CurDir => (),
                Component::ParentDir => {
                    // 如果两个节点都有父节点, 那么由于两个节点路径相同, 父节点的路径也相同, 直接构建 DiffNode.
                    if let (Some(newer_parent), Some(older_parent)) = self.current_node.get_parents() {
                        self.view_node(Some(newer_parent), Some(older_parent))?; // 这里不会 TypeMismatch
                    }
                    if let Some(path) = self.current_node.path() {
                        if let Some(parent) = path.parent() {
                            self.view_path(parent).unwrap_or_else(|_| self.view_roots());
                        } else {
                            self.view_roots();
                        }
                    } // 如果 get_path 返回 None, 则说明当前节点是 dummy node, 不用继续向上.
                }
                Component::Normal(_) => {
                    let (newer, older) = self.current_node.find_children(comp);
                    self.view_node(newer, older)?;
                }
                Component::Prefix(_) => {
                    self.view_node(self.newer.search_node(comp), self.older.search_node(comp))?;
                }
                Component::RootDir => {
                    if !cfg!(windows) {
                        self.view_node(
                            self.newer.find_root("/"),
                            self.newer.find_root("/"),
                        )?;
                    }
                }
            }
            Ok(())
        }

        /// 从当前节点开始, 一层一层进入相对路径 `path` 观察.
        ///
        /// # Developing Note
        /// 如果无法进入 `path`, 那么会尝试复原原来的观察状态.
        pub fn view_relpath(&mut self, path: impl AsRef<Path>) -> Result<(), crate::Error> {
            let raw_path = self.current_node.path();
            let path = path.as_ref();
            for comp in path.components() {
                self.view_comp(comp).inspect_err(|_| {
                    if let Some(raw_path) = &raw_path {
                        self.view_path(raw_path).unwrap(); // 复原的时候不应该报错
                    } else {
                        self.view_roots();
                    }
                })?;
            }
            Ok(())
        }

        /// 观察 diff [`Snapshot`] 的各个根节点, 这是最顶层的观察.
        pub fn view_roots(&mut self) {
            self.current_node = DiffNode::dummy();
            self.current_node.delta_size = self.newer.total_size() as isize - self.older.total_size() as isize;
            self.current_node.delta_alloc = self.newer.total_alloc() as isize - self.older.total_alloc() as isize;
            self.current_node.delta_n_files = self.newer.total_n_files() as isize - self.older.total_n_files() as isize;
            self.current_node.delta_n_folders = self.newer.total_n_folders() as isize - self.older.total_n_folders() as isize;
            self.nodes = Diff::diff_records(self.newer.iter_roots(), self.older.iter_roots());
        }

        /// 获取当前正在观察比较的节点下的子节点.
        pub fn nodes(&self) -> &[DiffNode] {
            &self.nodes
        }

        /// 获取当前正在观察的路径, 如果当前正在观察各个根节点, 则返回 None.
        pub fn current_path(&self) -> Option<PathBuf> {
            self.current_node.path()
        }
    }
}

#[cfg(not(feature = "owning_diff"))]
impl<'s> Diff<'s> {
    pub fn new(newer: &'s Snapshot, older: &'s Snapshot) -> Diff<'s> {
        let mut diff = Diff {
            newer,
            older,
            current_node: DiffNode::dummy(),
            nodes: Vec::new(),
        };
        diff.view_roots();
        diff
    }

    impl_diff!();
}

#[cfg(feature = "owning_diff")]
impl Diff {
    pub fn new(newer: Snapshot, older: Snapshot) -> Diff {
        let mut diff = Diff {
            newer,
            older,
            current_node: DiffNode::dummy(),
            nodes: Vec::new(),
        };
        diff.view_roots();
        diff
    }

    impl_diff!();
}

#[cfg(test)]
mod tests {
    use std::ffi::OsStr;
    use std::io::Cursor;
    use crate::Builder;
    use super::*;

    #[test]
    fn size_of() {
        assert_eq!(
            std::mem::size_of::<DiffNode>(),
            88
        );
    }

    #[test]
    #[cfg(windows)]
    fn build_diff() {
        const OLDER: &str = r#"
文件名称,大小,分配,修改时间,属性,文件,文件夹
"D:\Temp\Temp\a\",3536,12288,2025/01/31 21:19:30,0,5,2
"D:\Temp\Temp\a\vegetable\",1230,4096,2025/01/31 21:21:07,16,2,0
"D:\Temp\Temp\a\vegetable\tomato.txt",1010,4096,2025/01/31 21:18:55,32,0,0
"D:\Temp\Temp\a\vegetable\cabbage.txt",220,0,2025/01/31 21:18:34,32,0,0
"D:\Temp\Temp\a\go.txt",1234,4096,2025/01/31 21:16:52,32,0,0
"D:\Temp\Temp\a\fruit\",1072,4096,2025/01/31 21:19:30,16,2,0
"D:\Temp\Temp\a\fruit\banana.txt",1024,4096,2025/01/31 21:19:30,32,0,0
"D:\Temp\Temp\a\fruit\apple.txt",48,0,2025/01/31 21:19:15,32,0,0
"#;
        const NEWER: &str = r#"
文件名称,大小,分配,修改时间,属性,文件,文件夹
"D:\Temp\Temp\a\",3075,8192,2025/01/31 21:28:22,0,4,3
"D:\Temp\Temp\a\vegetable\",2048,4096,2025/01/31 21:25:23,16,1,0
"D:\Temp\Temp\a\vegetable\cabbage.txt",2048,4096,2025/01/31 21:25:14,32,0,0
"D:\Temp\Temp\a\fruit\",1027,4096,2025/01/31 21:28:22,16,3,1
"D:\Temp\Temp\a\fruit\banana.txt",1024,4096,2025/01/31 21:19:30,32,0,0
"D:\Temp\Temp\a\fruit\pear.txt",0,0,2025/01/31 21:24:14,32,0,0
"D:\Temp\Temp\a\fruit\other\",0,0,2025/01/31 21:24:30,16,0,0
"D:\Temp\Temp\a\fruit\apple.txt",3,0,2025/01/31 21:28:22,32,0,0
"#;
        let older = Builder::default().build_from_content(Cursor::new(OLDER), true).unwrap();
        let newer = Builder::default().build_from_content(Cursor::new(NEWER), true).unwrap();
        #[cfg(not(feature = "owning_diff"))]
        let mut diff = Diff::new(&newer, &older);
        #[cfg(feature = "owning_diff")]
        let mut diff = Diff::new(newer, older);
        assert_eq!(diff.current_node.delta_alloc, -4096);
        assert_eq!(diff.nodes.len(), 1);
        assert_eq!(diff.nodes[0].delta_alloc, -4096);
        diff.view_path("D:/Temp/Temp/a").unwrap();
        dbg!(&diff.nodes);
        diff.view_path("D:/Temp/Temp/a/fruit/./..").unwrap();
        dbg!(&diff.nodes);
        diff.view_comp(Component::Normal(OsStr::new("fruit"))).unwrap();
        dbg!(&diff.nodes);
        diff.view_relpath("other").unwrap();
        dbg!(&diff.nodes);
        diff.view_relpath("..").unwrap();
        dbg!(&diff.nodes);
        diff.view_relpath("../..").unwrap();
        dbg!(&diff.nodes);
    }

    #[test]
    #[cfg(windows)]
    fn diff_types() {
        const NEWER: &str = r#"
文件名称,大小,分配,修改时间,属性,文件,文件夹
"D:\Temp\",3536,12288,2025/01/31 21:19:30,0,0,0
"#;
        const OLDER: &str = r#"
文件名称,大小,分配,修改时间,属性,文件,文件夹
"D:\Temp",3536,12288,2025/01/31 21:19:30,0,0,0
"#;
        let newer = Builder::default().build_from_content(Cursor::new(NEWER), true).unwrap();
        let older = Builder::default().build_from_content(Cursor::new(OLDER), true).unwrap();
        #[cfg(not(feature = "owning_diff"))]
        let mut diff = Diff::new(&newer, &older);
        #[cfg(feature = "owning_diff")]
        let mut diff = Diff::new(newer, older);
        dbg!(&diff.nodes);
        assert_eq!(diff.nodes.len(), 2);
        let json = serde_json::to_string_pretty(&diff.nodes).unwrap();
        println!("json: {}", json);
    }
}