// #![allow(dead_code, unused_imports, unused_macros)]
use colored::*;

use lexer::{Lexer, Span, TokenKind};
use parser::{Parser, ParserError};

const CONTENTS: &str = include_str!("../testfiles/main.sr");

macro_rules! log {
    (error: $kind:expr) => {
        println!();
        println!("{} unexpected token {}", "error:".red().bold(), $kind);
    };
    ($title:expr) => {
        println!();
        println!("{}", $title.green().bold());
    };
    ($title:expr, $desc:expr) => {
        println!();
        println!("{} {}", $title.green().bold(), $desc);
    };
}

fn error_at_range(span: &Span) {
    let lines = CONTENTS.lines().enumerate();
    let idx = CONTENTS[..=span.range().start].lines().count() - 1;
    let (ln, line) = lines.clone().nth(idx).expect("Expected line");

    let sep = "|".blue().bold();

    if idx > 0 {
        let prev_line = lines.clone().nth(idx - 1);
        if let Some((ln, line)) = prev_line {
            println!(
                "{ln:>2} {sep} {line}",
                ln = (ln + 1).to_string().blue().bold()
            );
        }
    }

    let offset = span.range().count();
    let col = CONTENTS[..=span.range().start]
        .lines()
        .last()
        .unwrap_or("")
        .chars()
        .count();

    println!(
        "{ln:>2} {sep} {line}",
        ln = (ln + 1).to_string().blue().bold()
    );
    println!("{:>2} {sep} {:>col$}", "", "^".repeat(offset).red().bold());

    if let Some((ln, line)) = lines.clone().nth(idx + 1) {
        println!(
            "{ln:>2} {sep} {line}",
            ln = (ln + 1).to_string().blue().bold()
        );
    }
}

fn print_error(error: ParserError) {
    match error {
        ParserError::UnexpectedToken {
            unexpected,
            expected,
        } => {
            log!(error: unexpected.kind);
            if let Some(kind) = expected {
                println!("expected {kind}");
            }
            error_at_range(&unexpected.span);
        }
        ParserError::ExpectedExpr(unexpected) => {
            log!(error: unexpected.kind);
            error_at_range(&unexpected.span);
        }
    }
}

// log!("Lexing");
// let start = Instant::now();
// let tokens = Lexer::new(CONTENTS);
// let duration = start.elapsed().as_secs_f64();
// log!("Lexed", format!("in: {duration}s"));

fn main() {
    let lexer = Lexer::new(CONTENTS);
    let parser = Parser::new(lexer.clone());

    log!("Lexed:");
    for token in lexer {
        match token.kind {
            TokenKind::Unknown => error_at_range(&token.span),
            _ => println!("{token:?}"),
        }
    }

    log!("Parsed:");
    for decl in parser {
        match decl {
            parser::Decl::Error(e) => print_error(e),
            _ => println!("{decl:?}"),
        }
    }
}
