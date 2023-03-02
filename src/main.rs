use lexer::{Lexer, Token, TokenKind};

const CONTENTS: &'static str = include_str!("../testfiles/main.sr");

fn error_at_point(at: usize) {
    let (first, second) = CONTENTS.split_at(at);

    let mut start = first.lines();
    let mut end = second.lines();

    let row = std::cmp::max(start.clone().count(), 1);
    let col = start.clone().last().unwrap_or("").chars().count() + 1;
    let sep = '|';

    println!("Syntax error: unexpected token");
    if row > 1 {
        println!(
            "{row:>2} {sep} {prev}",
            row = row - 1,
            prev = start.next().unwrap()
        );
    }
    println!(
        "{row:>2} {sep} {start}{end}",
        start = start.last().unwrap_or(""),
        end = end.next().unwrap()
    );
    println!("{:>2} {sep} {:>col$}", "", "^");
    if let Some(next) = end.next() {
        println!("{row:>2} {sep} {next}", row = row + 1);
    }
}

fn main() {
    let tokens = Lexer::new(CONTENTS).iter().collect::<Vec<Token>>();

    println!("Input:\n{CONTENTS}\n");
    println!("Tokens:\n{tokens:?}\n");
    println!("Parsed elements");

    for token in tokens {
        match token.kind {
            TokenKind::Unknown => error_at_point(token.span.range().start),
            TokenKind::Eof => println!("{:<16} -> {}", token.kind, "<eof>"),
            _ => println!("{:<16} -> {}", token.kind, &CONTENTS[token.span.range()]),
        }
    }
}
