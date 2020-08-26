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
    ";

    let tests = vec![
        Token::Let,
        Token::Ident(String::from("five")),
        Token::Assign,
        Token::Int(5),
        Token::Semicolon,
        Token::Let,
        Token::Ident(String::from("ten")),
        Token::Assign,
        Token::Int(10),
        Token::Semicolon,
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
        Token::Eof,
    ];

    let mut lexer = Lexer::new(input);

    for expect in tests {
        let tok = lexer.next_token();

        assert_eq!(expect, tok);
    }
}
