#![allow(dead_code, unused_imports, unused_macros)]

use colored::*;

use lexer::{Lexer, Span, TokenKind};
use parser::{Parser, ParserError, Program};

const CONTENTS: &str = include_str!("../testfiles/main.sr");

fn error_at_range(span: Span) {
    let affected_line = CONTENTS
        .lines()
        .enumerate()
        .find(|(_, line)| line.get(span.range()).is_some());

    let sep = "|".blue().bold();

    if let Some((idx, line)) = affected_line {
        if idx > 0 {
            let prev_line = CONTENTS.lines().nth(idx - 1);
            if let Some(line) = prev_line {
                println!("{ln:>2} {sep} {line}", ln = idx.to_string().blue().bold());
            }
        }

        let (first, _) = CONTENTS.split_at(span.range().start);

        let ln = idx + 1;
        let offset = span.range().count();
        let col = first.lines().last().unwrap_or("").chars().count() + offset;
        println!("{ln:>2} {sep} {line}", ln = ln.to_string().blue().bold());
        println!("{:>2} {sep} {:>col$}", "", "^".repeat(offset).red().bold());

        let next_line = CONTENTS.lines().nth(idx + 1);
        if let Some(line) = next_line {
            let ln = ln + 1;
            println!("{ln:>2} {sep} {line}", ln = ln.to_string().blue().bold());
        }
    }
}

fn print_error(error: ParserError) {
    use ParserError::*;

    match error {
        MissingToken(_) => todo!(),
        UnexpectedToken {
            unexpected,
            expected: _,
        } => {
            println!(
                "{} unexpected token {}",
                "error:".red().bold(),
                unexpected.kind
            );
            error_at_range(unexpected.span)
        }
        ExpectedExpr(_) => todo!(),
    }
}

macro_rules! log {
    ($title:expr) => {
        println!("{}", $title.green().bold())
    };
    ($title:expr, $desc:expr) => {
        println!("{} {}", $title.green().bold(), $desc)
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
            parser::Stmt::Error(e) => print_error(e),
            _ => println!("{stmt:?}"),
        }
    }
}
