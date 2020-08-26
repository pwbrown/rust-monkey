#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident(String),
    Int(i64),

    // Operators
    Assign, // =
    Plus,   // +

    // Delimiters
    Comma,     // ,
    Semicolon, // ;
    Lparen,    // (
    Rparen,    // )
    Lbrace,    // {
    Rbrace,    // }

    // Reserved Keywords
    Func,
    Let,
}
