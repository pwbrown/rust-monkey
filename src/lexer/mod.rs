#[cfg(test)]
mod tests;

use crate::token::Token;
use std::str::Chars;

pub struct Lexer<'a>(Chars<'a>);

pub const EOF_CHAR: char = '\0';

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer(input.chars())
    }

    // Reads and returns the next token by consuming characters
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.curr() {
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '>' => Token::Gt,
            '<' => Token::Lt,
            '=' => self.if_peek('=', Token::Eq, Token::Assign),
            '!' => self.if_peek('=', Token::Neq, Token::Bang),
            _ => {
                if is_letter(self.curr()) {
                    return self.read_ident();
                } else if is_digit(self.curr()) {
                    return self.read_num();
                }
                Token::Eof
            }
        };

        self.bump();

        tok
    }

    // Discards all consecutive whitespace characters from the current char
    fn skip_whitespace(&mut self) {
        while is_whitespace(self.curr()) {
            self.bump();
        }
    }

    // Reads in the next identifier by consuming characters
    fn read_ident(&mut self) -> Token {
        let mut ident = String::new();
        while is_letter(self.curr()) {
            ident.push(self.curr());
            self.bump();
        }
        // Match reserved keywords otherwise create an identifier
        match ident.as_ref() {
            "fn" => Token::Func,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            _ => Token::Ident(ident),
        }
    }

    // Reads in the next number by consuming characters
    fn read_num(&mut self) -> Token {
        let mut num = String::new();
        while is_digit(self.curr()) {
            num.push(self.curr());
            self.bump();
        }
        Token::Int(num.parse::<i64>().unwrap())
    }

    // Bumps to the next char in the iterator
    fn bump(&mut self) -> Option<char> {
        self.0.next()
    }

    // Retrieves a copy of the current char in the iterator
    fn curr(&self) -> char {
        self.char_at(0)
    }

    // If the peek character matches the specified character
    // the next token is consumed and the if_tok is returned
    // otherwise the else_tok (default) is returned
    fn if_peek(&mut self, ch: char, if_tok: Token, else_tok: Token) -> Token {
        if self.char_at(1) == ch {
            self.bump();
            if_tok
        } else {
            else_tok
        }
    }

    // Retrieves a copy of the character at the position from the iterator
    fn char_at(&self, pos: usize) -> char {
        self.0.clone().nth(pos).unwrap_or(EOF_CHAR)
    }
}

fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

fn is_whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

// Lexer iterator
impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let tok = self.next_token();
        if tok != Token::Eof {
            return Some(tok);
        }
        None
    }
}
