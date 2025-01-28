use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io;
use std::io::BufRead;
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
#[derive(Serialize, Deserialize, Debug)]
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
    roots: Vec<RecordNode>,
}

impl SpaceDistribution {
    /// 从 [记录](RawRecord) 中构建一个 [空间分布](SpaceDistribution).
    ///
    /// 此方法假定原始记录是按照原始 csv 文件中的行顺序排列的, 没有进行排序过.
    ///
    /// 如果传入已经排序过的记录, 会产生错误地结果,
    /// 此时应该使用性能较劣的 [`SpaceDistribution::from_unordered_records`]
    /// 以获得正确的输出.
    /// <!-- todo 有待验证两者性能 -->
    pub fn from_ordered_records(records: &[RawRecord]) -> SpaceDistribution {
        todo!();
    }

    /// 从 [记录](RawRecord) 中构建一个 [空间分布](SpaceDistribution).
    ///
    /// 此方法允许输入数组 records 是乱序的,
    /// 但是性能相比 [`SpaceDistribution::from_ordered_records`] 较弱.
    pub fn from_unordered_records(records: &[RawRecord]) -> SpaceDistribution {
        todo!();
    }
}

#[derive(thiserror::Error, Debug)]
pub enum DiffError {
    #[error("IO error")]
    Io(#[from] io::Error),
    /// 可能会和 [`Io`](DiffError::Io) 重叠,
    ///
    /// 一般来说, 当 io 错误从 csv 操作中产生的时候,
    /// 会归因于此.
    #[error("Csv parsing error")]
    Csv(#[from] csv::Error),
}

/// 从 csv 文件路径中创建一个 CSV Reader, 并自动跳过可能的 header 行之前的无效行.
///
/// 此函数假设每个 csv 文件都有至少一个 header 行.
fn get_csv_reader(path: impl AsRef<Path>) -> Result<csv::Reader<io::BufReader<File>>, csv::Error> {
    let buf_reader = io::BufReader::new(File::open(path.as_ref())?);
    // 找到 header 行
    let mut n_pre_header_lines = 0;
    for (i, line) in buf_reader.lines().enumerate() {
        if line?.contains(",") {
            n_pre_header_lines = i;
            break;
        }
    }
    let mut buf_reader = io::BufReader::new(File::open(path.as_ref())?);
    for _ in 0..n_pre_header_lines {
        buf_reader.read_line(&mut String::new())?;
    }

    let reader = csv::ReaderBuilder::new()
        .has_headers(true)
        .from_reader(buf_reader);

    Ok(reader)
}

pub fn diff() -> DiffView {
    todo!()
}

pub fn diff_file(file1: impl AsRef<Path>, file2: impl AsRef<Path>) -> Result<DiffView, DiffError> {
    let builder = csv::ReaderBuilder::new();
    todo!();
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_read_csv_records() {
        let start = Instant::now();
        let mut reader = get_csv_reader("example_data/example_1.csv").unwrap();
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
}
