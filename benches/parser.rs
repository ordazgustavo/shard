use criterion::{black_box, criterion_group, criterion_main, Criterion};

const CONTENTS: &str = include_str!("../testfiles/parser.sr");

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("Parser", |b| {
        b.iter(|| {
            let lexer = lexer::Lexer::new(CONTENTS);

            parser::Parser::new(black_box(lexer))
                .iter()
                .collect::<parser::Program>()
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
