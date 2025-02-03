use wiztree_diff::{Diff, Snapshot};

#[test]
#[cfg(windows)]
fn main() {
    let ss_older = Snapshot::from_csv_file("example_data/example_1.csv").unwrap();
    let ss_newer = Snapshot::from_csv_file("example_data/example_2.csv").unwrap();
    let mut diff = Diff::new(&ss_newer, &ss_older);
    assert_eq!(diff.current_path(), None);
    dbg!(diff.nodes());
    diff.view_path("D:/").unwrap();
    assert_eq!(diff.current_path(), Some("D:/".into()));
    diff.view_relpath("Temp").unwrap(); // 观察对比 D:/Temp 文件夹
    dbg!(diff.nodes());
}