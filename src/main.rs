// #![allow(dead_code, unused_imports)]

use std::time::Instant;

use lexer::{Lexer, Token, TokenKind};

const CONTENTS: &str = include_str!("../testfiles/bench.sr");

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

macro_rules! log {
    ($title:expr) => {
        println!("\x1b[1m\x1b[32m{}\x1b[0m", $title)
    };
    ($title:expr, $desc:expr) => {
        println!("\x1b[1m\x1b[32m{}\x1b[0m {}", $title, $desc)
    };
}

fn main() {
    log!("Lexing");
    let start = Instant::now();
    let tokens = Lexer::new(CONTENTS).iter().collect::<Vec<Token>>();
    let duration = start.elapsed().as_secs_f64();
    log!("Lexed", format!("in: {duration}s"));

    println!("Tokens:");
    for token in tokens {
        match token.kind {
            TokenKind::Unknown => error_at_point(token.span.range().start),
            _ => println!("{token:?}"),
        }
    }
}
