use criterion::{black_box, criterion_group, criterion_main, Criterion};
use parser::Program;

const CONTENTS: &str = include_str!("../testfiles/parser.sr");

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("Parser", |b| {
        b.iter(|| {
            let lexer = lexer::Lexer::new(CONTENTS);

            parser::Parser::new(black_box(lexer.iter()))
                .iter()
                .collect::<Program>()
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
