use crate::lexer::Lexer;
use std::io;
use std::io::prelude::*;

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        // Print prompt (flush stdout is required)
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        // Read input from user
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let lex = Lexer::new(&input);

        // Print each token from the input
        for tok in lex {
            println!("{:?}", tok);
        }
    }
}
