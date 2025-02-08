// 虽然代码部分做了 linux 适配, 但是 wiztree 是不支持 linux 的.
mod snapshot;
mod diff;

pub use diff::{Diff, DiffKind, DiffNode};
pub use snapshot::{Snapshot, builder::{Builder, ReportReadingInterval, Reporter, Message}};

use std::fmt::Debug;
use std::io;

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
    #[error("Error in diffing two space distribution")]
    Diffing(#[from] diff::Error),
}
