#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident(String),
    String(String),
    Int(i64),
    Bool(bool),

    // Operators
    Assign,   // =
    Plus,     // +
    Minus,    // -
    Bang,     // !
    Slash,    // /
    Asterisk, // *

    Lt,  // <
    Gt,  // >
    Eq,  // ==
    Neq, // !=

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
    If,
    Else,
    Return,
}
