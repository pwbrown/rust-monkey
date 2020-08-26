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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.curr() {
            '=' => Token::Assign,
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
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

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.curr()) {
            self.bump();
        }
    }

    fn read_ident(&mut self) -> Token {
        let mut ident = String::new();
        while is_letter(self.curr()) {
            ident.push(self.curr());
            self.bump();
        }
        match ident.as_ref() {
            "fn" => Token::Func,
            "let" => Token::Let,
            _ => Token::Ident(ident),
        }
    }

    fn read_num(&mut self) -> Token {
        let mut num = String::new();
        while is_digit(self.curr()) {
            num.push(self.curr());
            self.bump();
        }
        Token::Int(num.parse::<i64>().unwrap())
    }

    fn bump(&mut self) -> Option<char> {
        self.0.next()
    }
    fn curr(&self) -> char {
        self.char_at(0)
    }

    fn peek(&self) -> char {
        self.char_at(1)
    }

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
