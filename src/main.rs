use std::{env, fs::File, io::Read};

use lexer::{LexError, Lexer};

fn read_file(file_path: &str) -> Result<String, std::io::Error> {
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = args.get(1).expect("Missing file path");
    let input = &read_file(&path).unwrap();

    // let input = "fn main() {\n\tlet name = \"Gustavo\";\n\tlet age = 420.69;\n}";
    let tokens = Lexer::new(input).lex();

    println!("Input:\n{input}\n");
    println!("Tokens:\n{tokens:?}\n");
    println!("Parsed elements");
    match tokens {
        Ok(ts) => {
            for t in ts {
                println!("{:<16} -> {}", t.kind, &input[t.span])
            }
        }
        Err(e) => match e {
            LexError::Empty => println!("Empty source provided"),
            LexError::UnknownToken(at) => {
                let lines = input[..at].lines();
                let line_number = lines.clone().count();
                let line = lines.last().unwrap();
                let col = line.chars().count() + 1;
                let rest = input[at..].lines().nth(0).unwrap();
                println!("Syntax error: unexpected token");
                println!("{:>2} | {line}{rest}", line_number);
                println!("{:>2} | {:>col$}", "", "^");
            }
        },
    };
}
