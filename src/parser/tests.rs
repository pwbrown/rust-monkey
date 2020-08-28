use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::Parser;

// ********************** UTILITIES **************************

// Parses a program given an input string
fn parse_program(input: &str) -> Program {
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse();
    check_parser_errors(&parser);
    program
}

// Retrieves the list of errors from the parser and prints them before panicking, otherwise returns
fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();
    if errors.len() == 0 {
        return;
    }
    println!("\n--PARSER ERRORS ({})--", errors.len());
    for err in errors {
        println!("  -- {}", err);
    }
    println!("\n");
    panic!("Failed with parser errors");
}

// *********************** TESTS ************************

#[test]
fn test_let_statements() {
    let program = parse_program(
        "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ",
    );

    assert_eq!(
        program.0,
        vec![
            Stmt::Let(String::from("x"), Expr::Literal(Literal::Int(5))),
            Stmt::Let(String::from("y"), Expr::Literal(Literal::Int(10))),
            Stmt::Let(String::from("foobar"), Expr::Literal(Literal::Int(838383))),
        ],
    );
}

#[test]
fn test_return_statements() {
    let program = parse_program(
        "
        return 5;
        return 10;
        return 993322;
    ",
    );
    assert_eq!(
        program.0,
        vec![
            Stmt::Return(Expr::Literal(Literal::Int(5))),
            Stmt::Return(Expr::Literal(Literal::Int(10))),
            Stmt::Return(Expr::Literal(Literal::Int(993322))),
        ],
    );
}

#[test]
fn test_ident_expression() {
    let program = parse_program("foobar;");
    assert_eq!(
        program.0,
        vec![Stmt::Expr(Expr::Ident(String::from("foobar")))]
    );
}

#[test]
fn test_integer_literal_expression() {
    let program = parse_program("5;");
    assert_eq!(program.0, vec![Stmt::Expr(Expr::Literal(Literal::Int(5)))]);
}

#[test]
fn test_boolean_literal_expression() {
    let program = parse_program("true; false;");
    assert_eq!(
        program.0,
        vec![
            Stmt::Expr(Expr::Literal(Literal::Bool(true))),
            Stmt::Expr(Expr::Literal(Literal::Bool(false)))
        ]
    );
}

#[test]
fn test_prefix_expressions() {
    let tests = vec![
        (
            "!5;",
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
        ),
        (
            "-15;",
            Stmt::Expr(Expr::Prefix(
                Prefix::Negative,
                Box::new(Expr::Literal(Literal::Int(15))),
            )),
        ),
    ];

    for (input, exp_ast) in tests {
        let program = parse_program(input);
        assert_eq!(program.0, vec![exp_ast]);
    }
}

#[test]
fn test_infix_expressions() {
    let tests = vec![
        (
            "5 + 5;",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Infix::Plus,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
        ),
        (
            "5 - 5;",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Infix::Minus,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
        ),
        (
            "5 * 5;",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Infix::Multiply,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
        ),
        (
            "5 / 5;",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Infix::Divide,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
        ),
        (
            "5 > 5;",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Infix::Gt,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
        ),
        (
            "5 < 5;",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Infix::Lt,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
        ),
        (
            "5 == 5;",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Infix::Eq,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
        ),
        (
            "5 != 5;",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Infix::Neq,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
        ),
    ];

    for (input, exp_ast) in tests {
        let program = parse_program(input);
        assert_eq!(program.0, vec![exp_ast]);
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        (
            "-a * b",
            "((-a) * b)",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Prefix(
                    Prefix::Negative,
                    Box::new(Expr::Ident(String::from("a"))),
                )),
                Infix::Multiply,
                Box::new(Expr::Ident(String::from("b"))),
            )),
        ),
        (
            "!-a",
            "(!(-a))",
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Prefix(
                    Prefix::Negative,
                    Box::new(Expr::Ident(String::from("a"))),
                )),
            )),
        ),
        (
            "a + b + c",
            "((a + b) + c)",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Ident(String::from("a"))),
                    Infix::Plus,
                    Box::new(Expr::Ident(String::from("b"))),
                )),
                Infix::Plus,
                Box::new(Expr::Ident(String::from("c"))),
            )),
        ),
        (
            "a + b - c",
            "((a + b) - c)",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Ident(String::from("a"))),
                    Infix::Plus,
                    Box::new(Expr::Ident(String::from("b"))),
                )),
                Infix::Minus,
                Box::new(Expr::Ident(String::from("c"))),
            )),
        ),
        (
            "a * b * c",
            "((a * b) * c)",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Ident(String::from("a"))),
                    Infix::Multiply,
                    Box::new(Expr::Ident(String::from("b"))),
                )),
                Infix::Multiply,
                Box::new(Expr::Ident(String::from("c"))),
            )),
        ),
        (
            "a * b / c",
            "((a * b) / c)",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Ident(String::from("a"))),
                    Infix::Multiply,
                    Box::new(Expr::Ident(String::from("b"))),
                )),
                Infix::Divide,
                Box::new(Expr::Ident(String::from("c"))),
            )),
        ),
        (
            "a + b / c",
            "(a + (b / c))",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Ident(String::from("a"))),
                Infix::Plus,
                Box::new(Expr::Infix(
                    Box::new(Expr::Ident(String::from("b"))),
                    Infix::Divide,
                    Box::new(Expr::Ident(String::from("c"))),
                )),
            )),
        ),
        (
            "a + b * c + d / e - f",
            "(((a + (b * c)) + (d / e)) - f)",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Infix(
                        Box::new(Expr::Ident(String::from("a"))),
                        Infix::Plus,
                        Box::new(Expr::Infix(
                            Box::new(Expr::Ident(String::from("b"))),
                            Infix::Multiply,
                            Box::new(Expr::Ident(String::from("c"))),
                        )),
                    )),
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Box::new(Expr::Ident(String::from("d"))),
                        Infix::Divide,
                        Box::new(Expr::Ident(String::from("e"))),
                    )),
                )),
                Infix::Minus,
                Box::new(Expr::Ident(String::from("f"))),
            )),
        ),
        (
            "5 > 4 == 3 < 4",
            "((5 > 4) == (3 < 4))",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::Gt,
                    Box::new(Expr::Literal(Literal::Int(4))),
                )),
                Infix::Eq,
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(3))),
                    Infix::Lt,
                    Box::new(Expr::Literal(Literal::Int(4))),
                )),
            )),
        ),
        (
            "5 > 4 != 3 > 4",
            "((5 > 4) != (3 > 4))",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::Gt,
                    Box::new(Expr::Literal(Literal::Int(4))),
                )),
                Infix::Neq,
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(3))),
                    Infix::Gt,
                    Box::new(Expr::Literal(Literal::Int(4))),
                )),
            )),
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(3))),
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(4))),
                        Infix::Multiply,
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                )),
                Infix::Eq,
                Box::new(Expr::Infix(
                    Box::new(Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Infix::Multiply,
                        Box::new(Expr::Literal(Literal::Int(1))),
                    )),
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(4))),
                        Infix::Multiply,
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                )),
            )),
        ),
        // Adding Boolean literals
        (
            "3 > 5 == false",
            "((3 > 5) == false)",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(3))),
                    Infix::Gt,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
                Infix::Eq,
                Box::new(Expr::Literal(Literal::Bool(false))),
            )),
        ),
        (
            "3 < 5 == true",
            "((3 < 5) == true)",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(3))),
                    Infix::Lt,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
                Infix::Eq,
                Box::new(Expr::Literal(Literal::Bool(true))),
            )),
        ),
        // Adding Grouped Expressions
        (
            "1 + (2 + 3) + 4",
            "((1 + (2 + 3)) + 4)",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(1))),
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(2))),
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::Int(3))),
                    )),
                )),
                Infix::Plus,
                Box::new(Expr::Literal(Literal::Int(4))),
            )),
        ),
        (
            "(5 + 5) * 2",
            "((5 + 5) * 2)",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
                Infix::Multiply,
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        ),
        (
            "2 / (5 + 5)",
            "(2 / (5 + 5))",
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(2))),
                Infix::Divide,
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            )),
        ),
        (
            "-(5 + 5)",
            "(-(5 + 5))",
            Stmt::Expr(Expr::Prefix(
                Prefix::Negative,
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            )),
        ),
        (
            "!(true == true)",
            "(!(true == true))",
            Stmt::Expr(Expr::Prefix(
                Prefix::Not,
                Box::new(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Bool(true))),
                    Infix::Eq,
                    Box::new(Expr::Literal(Literal::Bool(true))),
                )),
            )),
        ),
    ];

    for (input, exp_str, exp_ast) in tests {
        let program = parse_program(input);
        assert_eq!(format!("{}", &program), exp_str);
        assert_eq!(program.0, vec![exp_ast]);
    }
}

#[test]
fn test_if_expression() {
    let program = parse_program("if (x < y) { x }");

    assert_eq!(
        program.0,
        vec![Stmt::Expr(Expr::If(
            Box::new(Expr::Infix(
                Box::new(Expr::Ident(String::from("x"))),
                Infix::Lt,
                Box::new(Expr::Ident(String::from("y"))),
            )),
            BlockStmt(vec![Stmt::Expr(Expr::Ident(String::from("x")))]),
            None
        ))],
    )
}

#[test]
fn test_if_else_expression() {
    let program = parse_program("if (x < y) { x } else { y }");

    assert_eq!(
        program.0,
        vec![Stmt::Expr(Expr::If(
            Box::new(Expr::Infix(
                Box::new(Expr::Ident(String::from("x"))),
                Infix::Lt,
                Box::new(Expr::Ident(String::from("y"))),
            )),
            BlockStmt(vec![Stmt::Expr(Expr::Ident(String::from("x")))]),
            Some(BlockStmt(vec![Stmt::Expr(Expr::Ident(String::from("y")))])),
        ))],
    )
}

#[test]
fn test_function_literal_parsing() {
    let program = parse_program("fn(x, y) { x + y; }");

    assert_eq!(
        program.0,
        vec![Stmt::Expr(Expr::Func(
            vec![String::from("x"), String::from("y")],
            BlockStmt(vec![Stmt::Expr(Expr::Infix(
                Box::new(Expr::Ident(String::from("x"))),
                Infix::Plus,
                Box::new(Expr::Ident(String::from("y"))),
            ))])
        ))],
    )
}

#[test]
fn test_function_parameter_parsing() {
    let tests = vec![
        (
            "fn() {};",
            Stmt::Expr(Expr::Func(Vec::new(), BlockStmt(Vec::new()))),
        ),
        (
            "fn(x) {};",
            Stmt::Expr(Expr::Func(vec![String::from("x")], BlockStmt(Vec::new()))),
        ),
        (
            "fn(x, y, z) {};",
            Stmt::Expr(Expr::Func(
                vec![String::from("x"), String::from("y"), String::from("z")],
                BlockStmt(Vec::new()),
            )),
        ),
    ];

    for (input, exp_ast) in tests {
        let program = parse_program(input);
        assert_eq!(program.0, vec![exp_ast]);
    }
}

#[test]
fn test_call_expression_parsing() {
    let program = parse_program("add(1, 2 * 3, 4 + 5);");

    assert_eq!(
        program.0,
        vec![Stmt::Expr(Expr::Call(
            Box::new(Expr::Ident(String::from("add"))),
            vec![
                Expr::Literal(Literal::Int(1)),
                Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(2))),
                    Infix::Multiply,
                    Box::new(Expr::Literal(Literal::Int(3))),
                ),
                Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(4))),
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(5))),
                ),
            ]
        ))],
    )
}
