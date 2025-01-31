use std::cmp::Ordering;
use std::fmt;
use std::fmt::Formatter;
use std::slice::Iter;
use crate::space_distribution::{RcRecordNode, SpaceDistribution};

#[derive(Debug)]
pub enum DiffKind {
    /// 节点新建
    New,
    /// 节点删除
    Removed,
    /// 节点大小变化
    SizeChanged,
}

struct DiffNode<'a> {
    kind: DiffKind,
    /// 节点大小变化的字节数, 参考的是节点的分配大小而不是内容大小.
    delta: isize,
    newer_side_node: Option<&'a RcRecordNode>,
    older_side_node: Option<&'a RcRecordNode>,
}

impl<'a> fmt::Debug for DiffNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("DiffNode")
            .field("kind", &self.kind)
            .field("delta", &self.delta)
            .field("newer_side_node", &match &self.newer_side_node {
                Some(node) => node.borrow().path.to_string_lossy().to_string(),
                None => "None".to_string(),
            })
            .field("newer_side_node", &match &self.older_side_node {
                Some(node) => node.borrow().path.to_string_lossy().to_string(),
                None => "None".to_string(),
            })
            .finish()
    }
}

impl<'a> DiffNode<'a> {
    /// 接收两个节点, 产生一个比较节点, 如果两个节点都为 None, 返回 None.
    fn new(newer_side_node: Option<&'a RcRecordNode>, older_side_node: Option<&'a RcRecordNode>) -> Option<Self> {
        let (kind, delta) = match (newer_side_node, older_side_node) {
            (Some(new), None) => (DiffKind::New, new.borrow().alloc as isize),
            (None, Some(old)) => (DiffKind::Removed, -(old.borrow().alloc as isize)),
            (Some(new), Some(old)) => (
                DiffKind::SizeChanged, new.borrow().alloc as isize - old.borrow().alloc as isize
            ),
            (None, None) => return None
        };
        Some(DiffNode {
            kind,
            delta,
            newer_side_node,
            older_side_node,
        })
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
    /// Diff 构造初始时 dummy node, 表示比较两个 SpaceDistribution 的各个根节点.
    ///
    /// dummy node:
    /// - `newer_side_node` 和 `older_side_node` 都为 None
    /// - `kind` 为 [`DiffKind::SizeChanged`],
    /// - `delta` 为各个根节点总和大小变化.
    current_node: DiffNode<'sd>,
    /// 当前在观察的节点的子节点, Diff 构造初始时为空
    nodes: Vec<DiffNode<'sd>>,
}

impl<'sd> Diff<'sd> {
    pub fn new(newer: &'sd SpaceDistribution, older: &'sd SpaceDistribution) -> Diff<'sd> {
        Diff {
            newer,
            older,
            current_node: DiffNode {
                kind: DiffKind::SizeChanged,
                delta: newer.total_alloc() as isize - older.total_alloc() as isize,
                newer_side_node: None,
                older_side_node: None,
            },
            nodes: Diff::diff_records(newer.iter_roots(), older.iter_roots()),
        }
    }

    fn diff_records(newer: Iter<'sd, RcRecordNode>, older: Iter<'sd, RcRecordNode>) -> Vec<DiffNode<'sd>> {
        let mut newer: Vec<_> = newer.collect();
        let mut older: Vec<_> = older.collect();
        // todo 检查性能损耗
        // 可能这里的排序会导致性能问题
        fn cmp(a: &&RcRecordNode, b: &&RcRecordNode) -> std::cmp::Ordering {
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
                    match cmp(new, old) {
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
                    rst.push(DiffNode::new(Some(new), None).unwrap());
                    newer_cur = newer_iter.next();
                }
                (None, Some(old)) => {
                    rst.push(DiffNode::new(None, Some(old)).unwrap());
                    older_cur = older_iter.next();
                }
                (None, None) => break
            }
        }
        rst
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
    use std::io::Cursor;
    use super::*;

    #[test]
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
        let diff = newer.diff(&older);
        assert_eq!(diff.current_node.delta, -4096);
        assert_eq!(diff.nodes.len(), 1);
        assert_eq!(diff.nodes[0].delta, -4096);
    }
}