use std::fs;
use criterion::{criterion_group, criterion_main, Criterion};
use std::fs::File;
use std::io::{BufRead, BufReader};

fn count_file_lines_buf_reader() -> usize {
    // 数 260w 行对应所有操作大约需要 360ms.
    let buf_reader = BufReader::new(File::open("example_data/example_old.csv").unwrap());
    buf_reader.lines().count()
}

fn count_file_lines_read_to_string() -> usize {
    // 260w rows, 152.63 ms
    let content = fs::read_to_string("example_data/example_old.csv").unwrap();
    content.lines().count()
}

fn bench_functions(c: &mut Criterion) {
    c
        .bench_function("count_file_lines_buf_reader",
                        |b| b.iter(||
                            count_file_lines_buf_reader()))
        .bench_function("count_file_lines_read_to_string",
                        |b| b.iter(||
                            count_file_lines_read_to_string()));
}

criterion_group!(benches, bench_functions);
criterion_main!(benches);
