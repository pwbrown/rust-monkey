use crate::lexer::Lexer;
use crate::token::Token;

#[test]
fn test_next_token() {
    let input = "
        let five = 5;
        let ten = 10;
        
        let add = fn(x, y) {
            x + y;
        };
        
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        \"foobar\";
        \"foo bar\";
    ";

    let tests = vec![
        // let five = 5;
        Token::Let,
        Token::Ident(String::from("five")),
        Token::Assign,
        Token::Int(5),
        Token::Semicolon,
        // let ten = 10;
        Token::Let,
        Token::Ident(String::from("ten")),
        Token::Assign,
        Token::Int(10),
        Token::Semicolon,
        // let add = fn(x, y) {
        //     x + y;
        // };
        Token::Let,
        Token::Ident(String::from("add")),
        Token::Assign,
        Token::Func,
        Token::Lparen,
        Token::Ident(String::from("x")),
        Token::Comma,
        Token::Ident(String::from("y")),
        Token::Rparen,
        Token::Lbrace,
        Token::Ident(String::from("x")),
        Token::Plus,
        Token::Ident(String::from("y")),
        Token::Semicolon,
        Token::Rbrace,
        Token::Semicolon,
        // let result = add(five, ten);
        Token::Let,
        Token::Ident(String::from("result")),
        Token::Assign,
        Token::Ident(String::from("add")),
        Token::Lparen,
        Token::Ident(String::from("five")),
        Token::Comma,
        Token::Ident(String::from("ten")),
        Token::Rparen,
        Token::Semicolon,
        // !-/*5;
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Int(5),
        Token::Semicolon,
        // 5 < 10 > 5;
        Token::Int(5),
        Token::Lt,
        Token::Int(10),
        Token::Gt,
        Token::Int(5),
        Token::Semicolon,
        // if (5 < 10) {
        //     return true;
        // } else {
        //     return false;
        // }
        Token::If,
        Token::Lparen,
        Token::Int(5),
        Token::Lt,
        Token::Int(10),
        Token::Rparen,
        Token::Lbrace,
        Token::Return,
        Token::Bool(true),
        Token::Semicolon,
        Token::Rbrace,
        Token::Else,
        Token::Lbrace,
        Token::Return,
        Token::Bool(false),
        Token::Semicolon,
        Token::Rbrace,
        // 10 == 10;
        Token::Int(10),
        Token::Eq,
        Token::Int(10),
        Token::Semicolon,
        // 10 != 9;
        Token::Int(10),
        Token::Neq,
        Token::Int(9),
        Token::Semicolon,
        // "foobar";
        Token::String(String::from("foobar")),
        Token::Semicolon,
        // "foo bar";
        Token::String(String::from("foo bar")),
        Token::Semicolon,
        // END
        Token::Eof,
    ];

    let mut lexer = Lexer::new(input);

    for expect in tests {
        let tok = lexer.next_token();

        assert_eq!(expect, tok);
    }
}
