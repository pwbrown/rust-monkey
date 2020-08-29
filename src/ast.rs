use crate::token::Token;
use std::fmt;

pub type Program = BlockStmt;

// *********************** BLOCK *******************
#[derive(Debug, PartialEq)]
pub struct BlockStmt(pub Vec<Stmt>);

impl fmt::Display for BlockStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Combines all Statements into a single string
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|stmt| format!("{}", stmt))
                .collect::<Vec<String>>()
                .join("")
        )
    }
}

// ************************** STATEMENT **********************
#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let(String, Expr), // let my_var = 1 + 2;
    Return(Expr),      // return 1 + 2;
    Expr(Expr),        // 1 + 2
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Let(name, expr) => write!(f, "let {} = {};", name, expr),
            Stmt::Return(return_value) => write!(f, "return {};", return_value),
            Stmt::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

// ************************** EXPRESSION **********************
#[derive(Debug, PartialEq)]
pub enum Expr {
    Ident(String),                               // my_var
    Literal(Literal),                            // 10
    Prefix(Prefix, Box<Expr>),                   // -10
    Infix(Box<Expr>, Infix, Box<Expr>),          // 10 - 5
    If(Box<Expr>, BlockStmt, Option<BlockStmt>), // if (5 < 10) { true } else { false }
    Func(Vec<String>, BlockStmt),                // fn(x, y) { return x + y; }
    Call(Box<Expr>, Vec<Expr>),                  // add(1, 2)
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Ident(ident) => write!(f, "{}", ident),
            Expr::Literal(literal) => write!(f, "{}", literal),
            Expr::Prefix(op, right) => write!(f, "({}{})", op, right),
            Expr::Infix(left, op, right) => write!(f, "({} {} {})", left, op, right),
            Expr::If(cond, cons, alt) => {
                let mut if_expr = format!("if {} {{ {} }}", cond, cons);
                if let Some(alt) = alt {
                    if_expr = format!("{} else {{ {} }}", if_expr, alt);
                }
                write!(f, "{}", if_expr)
            }
            Expr::Func(params, body) => write!(f, "fn({}) {}", params.join(", "), body),
            Expr::Call(func, args) => {
                let str_args: Vec<String> = args.iter().map(|expr| format!("{}", expr)).collect();
                write!(f, "{}({})", func, str_args.join(", "))
            }
        }
    }
}

// ************************** LITERAL **********************
#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i64),
    Bool(bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(num) => write!(f, "{}", num),
            Literal::Bool(boolean) => write!(f, "{}", boolean),
        }
    }
}

// ************************ PREFIX ***********************
#[derive(Debug, PartialEq)]
pub enum Prefix {
    Not,      // !
    Negative, // -
}

impl Prefix {
    pub fn from_token(token: &Token) -> Option<Prefix> {
        match token {
            Token::Bang => Some(Prefix::Not),
            Token::Minus => Some(Prefix::Negative),
            _ => None,
        }
    }
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prefix::Not => write!(f, "!"),
            Prefix::Negative => write!(f, "-"),
        }
    }
}

// ************************ INFIX ***********************
#[derive(Debug, PartialEq)]
pub enum Infix {
    Plus,     // +
    Minus,    // -
    Multiply, // /
    Divide,   // *
    Gt,       // >
    Lt,       // <
    Eq,       // ==
    Neq,      // !=
}

impl Infix {
    pub fn from_token(token: &Token) -> Option<Infix> {
        match token {
            Token::Plus => Some(Infix::Plus),
            Token::Minus => Some(Infix::Minus),
            Token::Asterisk => Some(Infix::Multiply),
            Token::Slash => Some(Infix::Divide),
            Token::Gt => Some(Infix::Gt),
            Token::Lt => Some(Infix::Lt),
            Token::Eq => Some(Infix::Eq),
            Token::Neq => Some(Infix::Neq),
            _ => None,
        }
    }
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Multiply => write!(f, "*"),
            Infix::Divide => write!(f, "/"),
            Infix::Gt => write!(f, ">"),
            Infix::Lt => write!(f, "<"),
            Infix::Eq => write!(f, "=="),
            Infix::Neq => write!(f, "!="),
        }
    }
}

// ******************** PRECEDENCE ***********************
#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // + or -
    Product,     // * or /
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

impl Precedence {
    // Derive the precedence from the specific token
    pub fn from_token(token: &Token) -> Precedence {
        match token {
            Token::Eq | Token::Neq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::Lparen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}
