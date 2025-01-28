use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io;
use std::io::BufRead;
use std::path::Path;

/// 一个 Record 记录了一个文件(夹)的基本信息.
/// 对应一个 WizTree 导出的 csv 文件的一行.
#[derive(Serialize, Deserialize, Debug)]
struct RawRecord {
    /// 文件名称
    path: String,
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
    attributes: u16,
    /// 文件数量
    n_files: usize,
    /// 文件夹数量
    n_folders: usize,
}

struct DiffView {
    // todo
}

/// 磁盘空间分布,
/// 储存着特定范围内的文件及其大小信息.
///
/// 对应的是文件 WizTree 产生的一个 csv 文件.
pub struct SpaceDistribution {
    /// csv 文件数据来源
    csv: Vec<RawRecord>,
}

pub fn diff() -> DiffView {
    todo!()
}

#[derive(thiserror::Error, Debug)]
pub enum DiffError {
    #[error("IO error")]
    Io(#[from] std::io::Error),
    #[error("CSV value don't match")]
    Type,
}

/// 从 csv 文件路径中创建一个 CSV Reader, 并自动跳过可能的多行文件头.
fn get_csv_reader(path: impl AsRef<Path>) -> Result<csv::Reader<io::BufReader<File>>, csv::Error> {
    let file = File::open(path)?;
    let mut buf_reader = io::BufReader::new(file.try_clone()?);
    // 跳过非数据行
    let mut n_header_lines = 0;
    for (i, line) in buf_reader.lines().enumerate() {
        if line?.contains(",") {
            n_header_lines = i;
            break;
        }
    }
    let mut buf_reader = io::BufReader::new(file);
    for _ in 0..n_header_lines {
        buf_reader.read_line(&mut String::new())?;
    }

    let mut reader = csv::Reader::from_reader(buf_reader);
    #[cfg(test)]
    for (i, record) in reader.records().take(3).enumerate() {
        let raw_record: RawRecord = record?.deserialize(None)?;
        println!("{i}: {raw_record:?}");
    }
    Ok(reader)
}

pub fn diff_file(file1: impl AsRef<Path>, file2: impl AsRef<Path>) -> Result<DiffView, DiffError> {
    let builder = csv::ReaderBuilder::new();
    todo!();
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_get_csv_reader() {
        get_csv_reader("example_data/example_1.csv").unwrap();
    }
}
