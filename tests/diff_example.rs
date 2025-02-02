use std::path::PathBuf;
use wiztree_diff_lib::{Diff, SpaceDistribution};

#[test]
#[cfg(windows)]
fn main() {
    let sd_older = SpaceDistribution::from_csv_file("example_data/example_1.csv").unwrap();
    let sd_newer = SpaceDistribution::from_csv_file("example_data/example_2.csv").unwrap();
    let mut diff = Diff::new(&sd_newer, &sd_older);
    assert_eq!(diff.current_path(), None);
    dbg!(diff.nodes());
    diff.view_path("D:/").unwrap();
    assert_eq!(diff.current_path(), Some("D:/".into()));
    diff.view_relpath("Temp").unwrap(); // 观察对比 D:/Temp 文件夹
    dbg!(diff.nodes());
}