use monkey::evaluator::env::Env;
use monkey::evaluator::Evaluator;
use monkey::lexer::Lexer;
use monkey::parser::Parser;
use std::cell::RefCell;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

const PROMPT: &str = ">> ";

fn main() {
    // Set up evaluator with reusable environment
    let env = Env::new();
    let evaluator = Evaluator::new(Rc::new(RefCell::new(env)));

    println!("Welcome to the Monkey Programming Language!");
    println!("Feel free to type in commands\n");
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
        println!("{}", evaluator.eval(program));
    }
}

fn print_parser_errors(parser: &Parser) {
    println!("Whoops! We ran into some monkey business here!");
    println!(" parser errors:");
    for err in parser.errors() {
        println!("    {}", err);
    }
}
