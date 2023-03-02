use criterion::{black_box, criterion_group, criterion_main, Criterion};

const CONTENTS: &str = include_str!("../testfiles/bench.sr");

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("Lexer", |b| {
        b.iter(|| {
            let mut lexer = lexer::Lexer::new(CONTENTS).iter();

            let mut res = vec![];
            while let Some(token) = black_box(lexer.next()) {
                res.push(token);
            }

            res
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
