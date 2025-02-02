use std::cell::Ref;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Formatter;
use std::path::{Component, Path, PathBuf, Prefix};
use std::slice::Iter;
use crate::space_distribution::{RcRecordNode, RecordNode, SpaceDistribution};

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub(crate) enum Error {
    #[error("Failed to diff given node for both node being None")]
    BothNone,
    #[error("Path between two node are in different paths")]
    PathNEqual,
}

#[derive(Debug)]
pub enum DiffKind {
    /// 节点新建
    New,
    /// 节点删除
    Removed,
    /// 节点大小变化
    SizeChanged,
}

struct DiffNode {
    kind: DiffKind,
    /// 节点大小变化的字节数, 参考的是节点的分配大小而不是内容大小.
    delta: isize,
    // newer_side_node 和 older_side_node 同时为 None 时是 dummy_node.
    // newer_side_node 和 older_side_Node 同时为 Some 时, 它们的 path 相同.
    newer_side_node: Option<RcRecordNode>,
    older_side_node: Option<RcRecordNode>,
}

impl<'a> fmt::Debug for DiffNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("DiffNode")
            .field("kind", &self.kind)
            .field("delta", &self.delta)
            .field("newer_side_node", &match &self.newer_side_node {
                Some(node) => node.borrow().path.to_string_lossy().to_string(),
                None => "[None]".to_string(),
            })
            .field("newer_side_node", &match &self.older_side_node {
                Some(node) => node.borrow().path.to_string_lossy().to_string(),
                None => "[None]".to_string(),
            })
            .finish()
    }
}

impl DiffNode {
    /// 接收两个节点, 产生一个比较节点;
    /// - 如果两个节点都为 None, 返回 None.
    /// - 如果两个节点的路径不同, 返回 None.
    fn new(newer_side_node: Option<RcRecordNode>, older_side_node: Option<RcRecordNode>) -> Result<Self, Error> {
        let (kind, delta) = match (&newer_side_node, &older_side_node) {
            (Some(new), None) => (DiffKind::New, new.borrow().alloc as isize),
            (None, Some(old)) => (DiffKind::Removed, -(old.borrow().alloc as isize)),
            (Some(new), Some(old)) => if new.borrow().path == old.borrow().path {
                (DiffKind::SizeChanged, new.borrow().alloc as isize - old.borrow().alloc as isize)
            } else { return Err(Error::PathNEqual); },
            (None, None) => return Err(Error::BothNone)
        };
        Ok(DiffNode {
            kind,
            delta,
            newer_side_node,
            older_side_node,
        })
    }

    fn dummy(delta: isize) -> DiffNode {
        DiffNode {
            kind: DiffKind::SizeChanged,
            delta,
            newer_side_node: None,
            older_side_node: None,
        }
    }

    pub fn is_dummy(&self) -> bool {
        self.newer_side_node.is_none() && self.older_side_node.is_none()
    }

    /// 获取当前节点表示的路径, 如果当前节点是 dummy node, 则返回 None.
    pub fn get_path(&self) -> Option<PathBuf> {
        self.newer_side_node.as_ref().map(
            |node| node.borrow().path.clone()
        ).or(self.older_side_node.as_ref().map(
            |node| node.borrow().path.clone()
        ))
    }

    /// 获取 newer_side_node 和 older_side_node 的父节点.
    pub fn get_parents(&self) -> (Option<RcRecordNode>, Option<RcRecordNode>) {
        (self.newer_side_node.as_ref().and_then(|node| node.borrow().parent()),
         self.older_side_node.as_ref().and_then(|node| node.borrow().parent()))
    }

    /// 同时对正在观察对比的两个节点调用 [`RecordNode::find_child`], 并返回其结果.
    fn find_children(&self, comp: Component) -> (Option<RcRecordNode>, Option<RcRecordNode>) {
        (self.newer_side_node.as_ref().and_then(|n| n.borrow().find_child(comp)),
         self.older_side_node.as_ref().and_then(|n| n.borrow().find_child(comp)))
    }
}

/// 两个 [`SpaceDistribution`] 的对比,
/// 此结构体对两者进行借用而不拷贝数据.
///
/// 对 SpaceDistribution 中节点的比较是比较其分配大小, 而不是内容大小.
#[derive(Debug)]
pub struct Diff<'sd> {
    /// 比较中较新的 [`SpaceDistribution`]
    newer: &'sd SpaceDistribution,
    /// 比较中较旧的 [`SpaceDistribution`]
    older: &'sd SpaceDistribution,
    /// 当前在观察的节点.
    ///
    /// Diff 构造初始时为 dummy node, 表示比较两个 SpaceDistribution 的各个根节点.
    ///
    /// dummy node:
    /// - `newer_side_node` 和 `older_side_node` 都为 None
    /// - `kind` 为 [`DiffKind::SizeChanged`],
    /// - `delta` 为各个根节点总和大小变化.
    current_node: DiffNode,
    /// 当前在观察的节点的子节点, Diff 构造初始时为空
    nodes: Vec<DiffNode>,
}

impl<'sd> Diff<'sd> {
    pub fn new(newer: &'sd SpaceDistribution, older: &'sd SpaceDistribution) -> Diff<'sd> {
        let mut diff = Diff {
            newer,
            older,
            current_node: DiffNode::dummy(0),
            nodes: Vec::new(),
        };
        diff.view_roots();
        diff
    }

    fn diff_records(newer: impl Iterator<Item=&'sd RcRecordNode>, older: impl Iterator<Item=&'sd RcRecordNode>) -> Vec<DiffNode> {
        let mut newer: Vec<_> = newer.collect();
        let mut older: Vec<_> = older.collect();
        // todo 检查性能损耗
        // 可能这里的排序会导致性能问题
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
                    let new = RcRecordNode::clone(&new);
                    let old = RcRecordNode::clone(&old);
                    match cmp(&&new, &&old) {
                        Ordering::Less => {
                            rst.push(DiffNode::new(Some(new), None).unwrap());
                            newer_cur = newer_iter.next();
                        }
                        Ordering::Equal => {
                            rst.push(DiffNode::new(Some(new), Some(old)).unwrap());
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
                    rst.push(DiffNode::new(Some(RcRecordNode::clone(&new)), None).unwrap());
                    newer_cur = newer_iter.next();
                }
                (None, Some(old)) => {
                    rst.push(DiffNode::new(None, Some(RcRecordNode::clone(&old))).unwrap());
                    older_cur = older_iter.next();
                }
                (None, None) => break
            }
        }
        rst
    }

    /// 观察对比一对新旧节点.
    ///
    /// 需要确保新旧节点的路径相同, 否则 panic.
    fn view_node(&mut self, newer: Option<RcRecordNode>, older: Option<RcRecordNode>) -> Result<(), Error> {
        if newer.is_none() && older.is_none() {
            return Err(Error::BothNone);
        }
        let borrow_newer = newer.as_ref().map(|n| n.borrow());
        let borrow_older = older.as_ref().map(|n| n.borrow());
        if let (Some(newer), Some(older)) = (&borrow_newer, &borrow_older) {
            if newer.path != older.path {
                return Err(Error::PathNEqual);
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

    /// 观察并 diff 指定路径 `path` 节点下的子节点,
    /// 如果路径在观察的两个 [`SpaceDistribution`] 中都不存在, 则返回错误.
    ///
    /// - `path` 需要为完整的路径, 能够完整表示一个节点, 可以使用 `..` 或者 `.`, 但是不会解析符号链接.
    pub fn view_path(&mut self, path: impl AsRef<Path>) -> Result<(), Error> {
        let newer_node = self.newer.search_node(&path);
        let older_node = self.older.search_node(&path);
        self.view_node(newer_node, older_node)?;
        Ok(())
    }

    /// 按照 [`comp`] 改变观察路径.
    pub fn view_comp(&mut self, comp: Component) -> Result<(), Error> {
        match comp {
            Component::CurDir => (),
            Component::ParentDir => {
                // 如果两个节点都有父节点, 那么由于两个节点路径相同, 父节点的路径也相同, 直接构建 DiffNode.
                if let (Some(newer_parent), Some(older_parent)) = self.current_node.get_parents() {
                    self.view_node(Some(newer_parent), Some(older_parent))?;
                }
                if let Some(path) = self.current_node.get_path() {
                    if let Some(parent) = path.parent() {
                        let newer = self.newer.search_node(parent);
                        let older = self.older.search_node(parent);
                        self.view_node(newer, older).unwrap_or_else(|e| self.view_roots());
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
                if !cfg!(target_os="windows") {
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
    pub fn view_relpath(&mut self, path: impl AsRef<Path>) -> Result<(), Error> {
        let raw_path = self.current_node.get_path();
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

    /// 观察 diff [`SpaceDistribution`] 的各个根节点, 这是最顶层的观察.
    pub fn view_roots(&mut self) {
        self.current_node = DiffNode::dummy(
            self.newer.total_alloc() as isize
                - self.older.total_alloc() as isize
        );
        self.nodes = Diff::diff_records(self.newer.iter_roots(), self.older.iter_roots());
    }
}

pub trait Diffable {
    type DiffResult<'a>
    where
        Self: 'a;

    fn diff<'a>(&'a self, other: &'a Self) -> Self::DiffResult<'a>;
}

impl Diffable for SpaceDistribution {
    type DiffResult<'sd> = Diff<'sd>;

    /// 以 other 作为基底, self 作为变化, 比较两个 [`SpaceDistribution`]
    fn diff<'sd>(&'sd self, other: &'sd Self) -> Self::DiffResult<'sd> {
        Diff::new(self, other)
    }
}

#[cfg(test)]
mod tests {
    use std::ffi::OsStr;
    use std::io::Cursor;
    use std::path::PrefixComponent;
    use super::*;

    #[test]
    #[cfg(target_os = "windows")]
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
        let older = SpaceDistribution::from_csv_content(Cursor::new(OLDER)).unwrap();
        let newer = SpaceDistribution::from_csv_content(Cursor::new(NEWER)).unwrap();
        let mut diff = newer.diff(&older);
        assert_eq!(diff.current_node.delta, -4096);
        assert_eq!(diff.nodes.len(), 1);
        assert_eq!(diff.nodes[0].delta, -4096);
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
    fn path_components() {
        let buf = PathBuf::from("D:/a/b/c/d");
        let mut p = buf.components();
        assert!(matches!(p.next(), Some(Component::Prefix(PrefixComponent{..}))));
        assert!(matches!(p.next(), Some(Component::RootDir)));
        assert!(matches!(p.next(), Some(Component::Normal(_a))));
        assert!(matches!(p.next(), Some(Component::Normal(_b))));
        assert!(matches!(p.next(), Some(Component::Normal(_c))));
        assert!(matches!(p.next(), Some(Component::Normal(_d))));
        assert!(p.next().is_none());
    }

    #[test]
    fn parent_path() {
        let path: PathBuf = "D:\\".into();
        let mut it = path.components();
        assert!(matches!(it.next(), Some(Component::Prefix(PrefixComponent{..}))));
        assert!(matches!(it.next(), Some(Component::RootDir)));
        assert_eq!(it.next(), None);
        assert_eq!(path.parent(), None);

        let empty_path = PathBuf::from("");
        assert_eq!(empty_path.components().next(), None);

        let slash = PathBuf::from("/");
        assert_eq!(slash.components().collect::<Vec<_>>(), [Component::RootDir]);
        assert_eq!(slash.parent(), None);
    }

    #[test]
    fn prefix_path() {
        let prefix = Path::new("D:\\").components().next().unwrap();
        let Component::Prefix(prefix) = prefix else { panic!(); };
        assert!(matches!(prefix.kind(), Prefix::Disk(68)));
        let path_from_prefix_comp = Path::new(prefix.as_os_str());
        dbg!(path_from_prefix_comp);
    }
}