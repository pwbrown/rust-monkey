#[cfg(test)]
mod tests;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    // Creates a new Parser and reads the first 2 tokens
    pub fn new(mut lexer: Lexer) -> Parser {
        Parser {
            curr_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer,
            errors: Vec::new(),
        }
    }

    // Parses the entire program from the lexer
    pub fn parse(&mut self) -> Program {
        let mut stmts = Vec::new();
        while self.curr_token != Token::Eof {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            }
            self.bump();
        }
        BlockStmt(stmts)
    }

    // Returns a list of all parser errors
    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.curr_token {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_block_stmt(&mut self) -> BlockStmt {
        let mut stmts = Vec::new();

        self.bump();

        while self.curr_token != Token::Rbrace && self.curr_token != Token::Eof {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            }
            self.bump();
        }

        BlockStmt(stmts)
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        match self.peek_token {
            Token::Ident(_) => self.bump(),
            _ => {
                self.peek_error(Token::Ident(String::from(String::new())));
                return None;
            }
        }

        let name = match &self.curr_token {
            Token::Ident(ident) => String::from(ident),
            _ => return None,
        };

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        self.bump();

        let expr = match self.parse_expr(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if self.peek_token == Token::Semicolon {
            self.bump();
        }

        Some(Stmt::Let(name, expr))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.bump();

        let expr = match self.parse_expr(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if self.peek_token == Token::Semicolon {
            self.bump();
        }

        Some(Stmt::Return(expr))
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        match self.parse_expr(Precedence::Lowest) {
            Some(expr) => {
                if self.peek_token == Token::Semicolon {
                    self.bump();
                }
                Some(Stmt::Expr(expr))
            }
            None => None,
        }
    }

    fn parse_expr(&mut self, prec: Precedence) -> Option<Expr> {
        // Handle prefix expression
        let mut left = match self.curr_token {
            Token::Ident(_) => self.parse_ident_expr(),
            Token::Int(_) => self.parse_integer_literal_expr(),
            Token::Bool(_) => self.parse_boolean_literal_expr(),
            Token::String(_) => self.parse_string_literal_expr(),
            Token::Lparen => self.parse_grouped_expr(),
            Token::Lbracket => self.parse_array_literal_expr(),
            Token::Lbrace => self.parse_hash_literal_expr(),
            Token::If => self.parse_if_expr(),
            Token::Func => self.parse_function_literal_expr(),
            Token::Bang | Token::Minus => self.parse_prefix_expr(),
            _ => {
                self.no_prefix_parser_error(self.curr_token.clone());
                return None;
            }
        };

        // Handle infix expression
        while self.peek_token != Token::Semicolon && prec < Precedence::from_token(&self.peek_token)
        {
            left = match self.peek_token {
                Token::Lparen => {
                    self.bump();
                    self.parse_call_expr(left.unwrap())
                }
                Token::Lbracket => {
                    self.bump();
                    self.parse_index_expr(left.unwrap())
                }
                _ => {
                    if Infix::from_token(&self.peek_token) != None {
                        self.bump();
                        self.parse_infix_expr(left.unwrap())
                    } else {
                        left
                    }
                }
            }
        }

        left
    }

    fn parse_ident_expr(&self) -> Option<Expr> {
        if let Token::Ident(ident) = &self.curr_token {
            return Some(Expr::Ident(String::from(ident)));
        }
        None
    }

    fn parse_integer_literal_expr(&self) -> Option<Expr> {
        if let Token::Int(num) = self.curr_token {
            return Some(Expr::Literal(Literal::Int(num)));
        }
        None
    }

    fn parse_boolean_literal_expr(&self) -> Option<Expr> {
        if let Token::Bool(boolean) = self.curr_token {
            return Some(Expr::Literal(Literal::Bool(boolean)));
        }
        None
    }

    fn parse_string_literal_expr(&self) -> Option<Expr> {
        if let Token::String(string) = &self.curr_token {
            return Some(Expr::Literal(Literal::String(String::from(string))));
        }
        None
    }

    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let prefix = match Prefix::from_token(&self.curr_token) {
            Some(prefix) => prefix,
            None => return None,
        };

        self.bump();

        match self.parse_expr(Precedence::Prefix) {
            Some(expr) => Some(Expr::Prefix(prefix, Box::new(expr))),
            None => None,
        }
    }

    fn parse_infix_expr(&mut self, left: Expr) -> Option<Expr> {
        let infix = match Infix::from_token(&self.curr_token) {
            Some(infix) => infix,
            None => return None,
        };

        let precedence = Precedence::from_token(&self.curr_token);

        self.bump();

        match self.parse_expr(precedence) {
            Some(right) => Some(Expr::Infix(Box::new(left), infix, Box::new(right))),
            None => return None,
        }
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.bump();

        match self.parse_expr(Precedence::Lowest) {
            Some(expr) => {
                if !self.expect_peek(Token::Rparen) {
                    return None;
                }
                Some(expr)
            }
            None => None,
        }
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        if !self.expect_peek(Token::Lparen) {
            return None;
        }

        self.bump();

        let condition = match self.parse_expr(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if !self.expect_peek(Token::Rparen) || !self.expect_peek(Token::Lbrace) {
            return None;
        }

        let consequence = self.parse_block_stmt();

        let mut alternative: Option<BlockStmt> = None;

        if self.peek_token == Token::Else {
            self.bump();

            if !self.expect_peek(Token::Lbrace) {
                return None;
            }

            alternative = Some(self.parse_block_stmt())
        }

        Some(Expr::If(Box::new(condition), consequence, alternative))
    }

    fn parse_function_literal_expr(&mut self) -> Option<Expr> {
        if !self.expect_peek(Token::Lparen) {
            return None;
        }

        let params = match self.parse_function_params() {
            Some(params) => params,
            None => return None,
        };

        if !self.expect_peek(Token::Lbrace) {
            return None;
        }

        Some(Expr::Func(params, self.parse_block_stmt()))
    }

    fn parse_function_params(&mut self) -> Option<Vec<String>> {
        let mut params = Vec::new();

        if self.peek_token == Token::Rparen {
            self.bump();
            return Some(params);
        }

        self.bump();

        loop {
            match &self.curr_token {
                Token::Ident(ident) => params.push(ident.clone()),
                _ => {
                    self.errors.push(format!(
                        "expected an identifier, got a '{:?}'",
                        self.curr_token
                    ));
                    return None;
                }
            }
            if self.peek_token != Token::Comma {
                break;
            }
            self.bump();
            self.bump();
        }

        if !self.expect_peek(Token::Rparen) {
            return None;
        }
        Some(params)
    }

    fn parse_call_expr(&mut self, func: Expr) -> Option<Expr> {
        match self.parse_expr_list(Token::Rparen) {
            Some(args) => Some(Expr::Call(Box::new(func), args)),
            None => None,
        }
    }

    fn parse_index_expr(&mut self, arr: Expr) -> Option<Expr> {
        self.bump();

        let index = match self.parse_expr(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if !self.expect_peek(Token::Rbracket) {
            return None;
        }

        Some(Expr::Index(Box::new(arr), Box::new(index)))
    }

    fn parse_array_literal_expr(&mut self) -> Option<Expr> {
        match self.parse_expr_list(Token::Rbracket) {
            Some(list) => Some(Expr::Literal(Literal::Array(list))),
            None => None,
        }
    }

    fn parse_hash_literal_expr(&mut self) -> Option<Expr> {
        let mut pairs = vec![];

        while self.peek_token != Token::Rbrace {
            self.bump();
            let key = match self.parse_expr(Precedence::Lowest) {
                Some(expr) => expr,
                None => return None,
            };
            if !self.expect_peek(Token::Colon) {
                return None;
            }
            self.bump();
            let val = match self.parse_expr(Precedence::Lowest) {
                Some(expr) => expr,
                None => return None,
            };
            pairs.push((key, val));
            if self.peek_token != Token::Rbrace && !self.expect_peek(Token::Comma) {
                return None;
            }
        }

        if !self.expect_peek(Token::Rbrace) {
            return None;
        }

        Some(Expr::Literal(Literal::Hash(pairs)))
    }

    fn parse_expr_list(&mut self, end: Token) -> Option<Vec<Expr>> {
        let mut list = vec![];

        if self.peek_token == end {
            self.bump();
            return Some(list);
        }

        self.bump();

        loop {
            match self.parse_expr(Precedence::Lowest) {
                Some(expr) => list.push(expr),
                None => return None,
            }
            if self.peek_token != Token::Comma {
                break;
            }
            self.bump();
            self.bump();
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }

    // Iterates the current and peek tokens
    fn bump(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    // Bumps if the peek token matches the expected token
    fn expect_peek(&mut self, tok: Token) -> bool {
        if self.peek_token == tok {
            self.bump();
            true
        } else {
            self.peek_error(tok);
            false
        }
    }

    // Appends an error message to the parser errors list
    fn peek_error(&mut self, tok: Token) {
        self.errors.push(format!(
            "expected next token to be '{:?}', got '{:?}' instead",
            tok, self.peek_token
        ))
    }

    // Appends an error message to the parser errors list
    fn no_prefix_parser_error(&mut self, tok: Token) {
        self.errors
            .push(format!("no prefix parser found for token '{:?}'", tok))
    }
}
