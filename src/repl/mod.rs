use crate::evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
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

        // Parse input
        let mut parser = Parser::new(Lexer::new(&input));
        let program = parser.parse();
        if parser.errors().len() > 0 {
            print_parser_errors(&parser);
            continue;
        }

        // Evaluate input
        println!("{}", evaluator::eval(program));
    }
}

fn print_parser_errors(parser: &Parser) {
    println!("Whoops! We ran into some monkey business here!");
    println!(" parser errors:");
    for err in parser.errors() {
        println!("    {}", err);
    }
}
