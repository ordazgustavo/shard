use criterion::{black_box, criterion_group, criterion_main, Criterion};
use parser::Program;

const CONTENTS: &str = include_str!("../testfiles/parser.sr");
const LEXER: lexer::Lexer = lexer::Lexer::new_const(CONTENTS);

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("Parser", |b| {
        b.iter(|| {
            parser::Parser::new(black_box(LEXER.iter()))
                .iter()
                .collect::<Program>()
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
