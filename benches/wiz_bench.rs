use criterion::{criterion_group, criterion_main, Criterion};
use std::fs::File;
use std::io::{BufRead, BufReader};

fn count_file_lines() -> usize {
    // 数 260w 行对应所有操作大约需要 360ms.
    let buf_reader = BufReader::new(File::open("example_data/example_1.csv").unwrap());
    buf_reader.lines().count()
}

fn bench_functions(c: &mut Criterion) {
    c.bench_function("count_file_lines", |b| b.iter(|| count_file_lines()));
}

criterion_group!(benches, bench_functions);
criterion_main!(benches);
