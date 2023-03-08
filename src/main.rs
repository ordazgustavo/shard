#![allow(dead_code, unused_imports, unused_macros)]

use lexer::{Lexer, Span, TokenKind};
use parser::{Parser, Program};

const CONTENTS: &str = include_str!("../testfiles/main.sr");

// TODO: figure out how to show EOF errors,
fn error_at_range(span: Span) {
    let at = span.range().start;
    let (first, second) = CONTENTS.split_at(at);

    let has_newline = first.lines().last().unwrap().contains('\n');

    let mut start = first.lines();
    let mut end = second.lines();

    let row = std::cmp::max(start.clone().count(), 1);
    let padding = span.range().count();
    let col = start.clone().last().unwrap_or("").chars().count() + padding;
    let sep = '|';

    println!("Syntax error: unexpected token");
    if row > 1 {
        println!(
            "{row:>2} {sep} {prev}",
            row = row - 1,
            prev = start.next().unwrap()
        );
    } else {
        println!("{:>2} {sep}", "");
    }
    if has_newline {
        println!("{row:>2} {sep} {start}", start = start.last().unwrap_or(""));
    } else {
        println!(
            "{row:>2} {sep} {start}{end}",
            start = start.last().unwrap_or(""),
            end = end.next().unwrap()
        );
    }
    println!("{:>2} {sep} {:>col$}", "", "^".repeat(padding));
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

// log!("Lexing");
// let start = Instant::now();
// let tokens = Lexer::new(CONTENTS);
// let duration = start.elapsed().as_secs_f64();
// log!("Lexed", format!("in: {duration}s"));

fn main() {
    let lexer = Lexer::new(CONTENTS);
    let parser = Parser::new(lexer.iter());

    // log!("Finished", parser.len());

    println!();
    log!("Lexed:");
    for token in lexer {
        match token.kind {
            TokenKind::Unknown => error_at_range(token.span),
            _ => println!("{token:?}"),
        }
    }

    println!();
    log!("Parsed:");
    for stmt in parser {
        match stmt {
            parser::Stmt::Error(e) => match e {
                parser::ParserError::MissingToken(_) => todo!(),
                parser::ParserError::UnexpectedToken {
                    unexpected,
                    expected: _,
                } => {
                    println!("{unexpected:?}");
                    error_at_range(unexpected.span);
                }
                parser::ParserError::ExpectedExpr(found) => error_at_range(found.span),
            },
            _ => println!("{stmt:?}"),
        }
    }
}
