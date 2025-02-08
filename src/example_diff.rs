use std::io::{stdout, Write};
use std::time::Duration;
use wiztree_diff::{Builder, Diff, Message, ReportReadingInterval, Snapshot};

fn progress_bar(cur: usize, total: usize) -> String {
    const FILL: &str = "■";
    const HOLLOW: &str = "□";
    const LEN: usize = 20;
    let ratio = cur as f64 / total as f64;
    let fill_len = (ratio * LEN as f64) as usize;
    format!("{:.2}% [{}{}]", ratio * 100.0, FILL.repeat(fill_len), HOLLOW.repeat(LEN - fill_len))
}

fn main() {
    let mut builder = Builder::new();
    builder
        .set_reporter(|m| {
            if let Message::Processing { total, current, .. } = m {
                print!("\r{}", progress_bar(current, total));
                stdout().flush().unwrap();
            }
        })
        .set_reading_report_interval(ReportReadingInterval::Time(Duration::from_secs(1)));
    let ss_older = builder.build_from_file("example_data/example_old.csv", true).unwrap();
    let ss_newer = builder.build_from_file("example_data/example_new.csv", true).unwrap();
    #[cfg(not(feature = "owning_diff"))]
    let mut diff = Diff::new(&ss_newer, &ss_older);
    #[cfg(feature = "owning_diff")]
    let mut diff = Diff::new(ss_newer, ss_older);
    assert_eq!(diff.current_path(), None);
    dbg!(diff.nodes());
    diff.view_path("D:/").unwrap();
    assert_eq!(diff.current_path(), Some("D:/".into()));
    diff.view_relpath("Temp").unwrap(); // 观察对比 D:/Temp 文件夹
    dbg!(diff.nodes());
}