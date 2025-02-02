mod space_distribution;
mod diff;

pub use diff::{Diff, DiffKind, Diffable};
pub use space_distribution::{SpaceDistribution};

use std::fmt::Debug;
use std::io;
use std::io::{BufRead, BufReader, Chain, Cursor, Read};

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
    Diff(#[from] diff::Error),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::space_distribution::RawRecord;
    use std::fs::File;
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

    #[test]
    fn test_read_csv_records() {
        let start = Instant::now();
        let mut reader =
            build_csv_reader(File::open("example_data/example_1.csv").unwrap()).unwrap();
        let mut throttle = Throttler::new(Duration::from_secs(1));
        for (i, record) in reader.records().enumerate() {
            let record = record.unwrap();
            // let v: Vec<_> = record.iter().collect();
            let raw_record: RawRecord = record.deserialize(None).unwrap();
            throttle.throttle_run(|| println!("{i}: {:?}", raw_record));
        }
        println!("Elapsed: {:?}", start.elapsed()); // 260w rows in 6.34s
    }
}
