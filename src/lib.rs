// 虽然代码部分做了 linux 适配, 但是 wiztree 是不支持 linux 的.
mod snapshot;
mod diff;

pub use diff::{Diff, DiffKind, DiffNode};
pub use snapshot::{Snapshot, builder::{Builder, ReportReadingInterval, ReportProcessingInterval, Reporter, Message}};

use std::fmt::Debug;
use std::io;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
    /// 可能会和 [`Io`](Error::Io) 重叠,
    ///
    /// 一般来说, 当 io 错误从 csv 操作中产生的时候,
    /// 会归因于此.
    #[error(transparent)]
    Csv(#[from] csv::Error),
    #[error(transparent)]
    Diffing(#[from] diff::DiffError),
}
