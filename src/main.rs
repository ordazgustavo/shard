use lexer::Lexer;

fn main() {
    let input = "";
    let tokens = Lexer::new(input).lex();

    println!("Input: {input}");
    println!("");
    println!("Tokens: {tokens:?}");

    println!("");
    println!("Parsed elements");
    for token in tokens.unwrap() {
        println!("{:<14} -> {}", token.kind, &input[token.span])
    }
}
