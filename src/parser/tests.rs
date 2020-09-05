use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::Parser;

// *********************** TESTS ************************

#[test]
fn test_let_statements() {
    assert_stmts(vec![
        ("let x = 5;", _let("x", int(5))),
        ("let y = true;", _let("y", bool(true))),
        ("let foobar = y;", _let("foobar", ident("y"))),
    ]);
}

#[test]
fn test_return_statements() {
    assert_stmts(vec![
        ("return 5;", Stmt::Return(int(5))),
        ("return true;", Stmt::Return(bool(true))),
        ("return y;", Stmt::Return(ident("y"))),
    ]);
}

#[test]
fn test_ident_expression() {
    assert_expr("foobar;", ident("foobar"));
}

#[test]
fn test_integer_literal_expression() {
    assert_expr("5;", int(5));
}

#[test]
fn test_boolean_literal_expression() {
    assert_exprs(vec![("true;", bool(true)), ("false;", bool(false))]);
}

#[test]
fn test_string_literal_expression() {
    assert_expr("\"hello world\"", str("hello world"));
}

#[test]
fn test_prefix_expressions() {
    assert_exprs(vec![
        ("!5;", prefix(Prefix::Not, int(5))),
        ("-15;", prefix(Prefix::Negative, int(15))),
    ]);
}

#[test]
fn test_infix_expressions() {
    assert_exprs(vec![
        ("5 + 5;", infix(int(5), Infix::Plus, int(5))),
        ("5 - 5;", infix(int(5), Infix::Minus, int(5))),
        ("5 * 5;", infix(int(5), Infix::Multiply, int(5))),
        ("5 / 5;", infix(int(5), Infix::Divide, int(5))),
        ("5 > 5;", infix(int(5), Infix::Gt, int(5))),
        ("5 < 5;", infix(int(5), Infix::Lt, int(5))),
        ("5 == 5;", infix(int(5), Infix::Eq, int(5))),
        ("5 != 5;", infix(int(5), Infix::Neq, int(5))),
    ]);
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        // "(!(-a))"
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 > 4 != 3 > 4", "((5 > 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        // Adding Boolean literals
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        // Adding Grouped Expressions
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        // Adding Call Expressions
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
        // Adding Array Literal and Index Expression parsing
        (
            "a * [1, 2, 3, 4][b * c] * d",
            "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        ),
        (
            "add(a * b[2], b[1], 2 * [1, 2][1])",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        ),
    ];

    for (input, exp) in tests {
        let program = parse_program(input);
        assert_eq!(format!("{}", program), exp);
    }
}

#[test]
fn test_if_expression() {
    assert_expr(
        "if (x < y) { x }",
        _if(infix(ident("x"), Infix::Lt, ident("y")), ident("x")),
    );
}

#[test]
fn test_if_else_expression() {
    assert_expr(
        "if (x < y) { x } else { y }",
        _ifelse(
            infix(ident("x"), Infix::Lt, ident("y")),
            ident("x"),
            ident("y"),
        ),
    );
}

#[test]
fn test_function_literal_parsing() {
    assert_expr(
        "fn(x, y) { x + y; }",
        _func(
            vec!["x", "y"],
            vec![infix(ident("x"), Infix::Plus, ident("y"))],
        ),
    );
}

#[test]
fn test_function_parameter_parsing() {
    assert_exprs(vec![
        ("fn() {};", _func(vec![], vec![])),
        ("fn(x) {};", _func(vec!["x"], vec![])),
        ("fn(x, y, z) {};", _func(vec!["x", "y", "z"], vec![])),
    ]);
}

#[test]
fn test_call_expression_parsing() {
    assert_expr(
        "add(1, 2 * 3, 4 + 5);",
        _call(
            ident("add"),
            vec![
                int(1),
                infix(int(2), Infix::Multiply, int(3)),
                infix(int(4), Infix::Plus, int(5)),
            ],
        ),
    );
}

#[test]
fn test_array_literal_parsing() {
    assert_expr(
        "[1, 2 * 2, 3 + 3]",
        arr(vec![
            int(1),
            infix(int(2), Infix::Multiply, int(2)),
            infix(int(3), Infix::Plus, int(3)),
        ]),
    );
}

#[test]
fn test_parsing_index_expressions() {
    assert_expr(
        "myArray[1 + 1]",
        index(ident("myArray"), infix(int(1), Infix::Plus, int(1))),
    );
}

#[test]
fn test_parsing_hash_literals_string_keys() {
    assert_expr(
        "{ \"one\": 1, \"two\": 2, \"three\": 3 }",
        hash(vec![
            (str("one"), int(1)),
            (str("two"), int(2)),
            (str("three"), int(3)),
        ]),
    );
}

#[test]
fn test_parsing_empty_hash_literal() {
    assert_expr("{}", hash(vec![]));
}

#[test]
fn test_parsing_hash_literals_with_expressions() {
    assert_expr(
        "{ \"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5 }",
        hash(vec![
            (str("one"), infix(int(0), Infix::Plus, int(1))),
            (str("two"), infix(int(10), Infix::Minus, int(8))),
            (str("three"), infix(int(15), Infix::Divide, int(5))),
        ]),
    );
}

// ********************** UTILITIES **************************

// Parsers a program and checks for parser errors
fn parse_program(input: &str) -> Program {
    // Parse Program
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse();

    // Check Parser for errors
    let errors = parser.errors();
    if errors.len() > 0 {
        println!("\n--PARSER ERRORS ({})--", errors.len());
        for err in errors {
            println!("  -- {}", err);
        }
        println!("\n");
        panic!("Failed with parser errors");
    }

    program
}

// Tests to see if the incoming input is parsed into the expected AST
fn assert_stmt(input: &str, result: Stmt) {
    let program = parse_program(input);
    assert_eq!(program.0, vec![result]);
}

// Tests an expression and handles conversion to stmt
fn assert_expr(input: &str, result: Expr) {
    assert_stmt(input, Stmt::Expr(result));
}

// Tests a vector of tuples that hold a single statement test
fn assert_stmts(tests: Vec<(&str, Stmt)>) {
    for (input, result) in tests {
        assert_stmt(input, result);
    }
}

// Tests a vector of tuples that hold a single expression test
fn assert_exprs(tests: Vec<(&str, Expr)>) {
    for (input, result) in tests {
        assert_expr(input, result);
    }
}

// Generates an integer literal expression
fn int(val: i64) -> Expr {
    Expr::Literal(Literal::Int(val))
}

// Generates a string literal expression
fn str(val: &str) -> Expr {
    Expr::Literal(Literal::String(String::from(val)))
}

// Generates a boolean literal expression
fn bool(val: bool) -> Expr {
    Expr::Literal(Literal::Bool(val))
}

// Generates an array literal expression
fn arr(items: Vec<Expr>) -> Expr {
    Expr::Literal(Literal::Array(items))
}

// Generates a hash literal expression
fn hash(pairs: Vec<(Expr, Expr)>) -> Expr {
    Expr::Literal(Literal::Hash(pairs))
}

// Generates a identifier expression
fn ident(val: &str) -> Expr {
    Expr::Ident(String::from(val))
}

// Generates a prefix expression
fn prefix(op: Prefix, right: Expr) -> Expr {
    Expr::Prefix(op, Box::new(right))
}

// Generates an infix expression
fn infix(left: Expr, op: Infix, right: Expr) -> Expr {
    Expr::Infix(Box::new(left), op, Box::new(right))
}

// Generates an index expression
fn index(arr: Expr, i: Expr) -> Expr {
    Expr::Index(Box::new(arr), Box::new(i))
}

// Generates a let statement
fn _let(ident: &str, expr: Expr) -> Stmt {
    Stmt::Let(String::from(ident), expr)
}

// Generates a return statement
fn _return(expr: Expr) -> Stmt {
    Stmt::Return(expr)
}

// Generates an if expression with only a single expression in the block
fn _if(cond: Expr, consequence: Expr) -> Expr {
    Expr::If(
        Box::new(cond),
        BlockStmt(vec![Stmt::Expr(consequence)]),
        None,
    )
}

// Generates an if else expression with only a single expression in each block
fn _ifelse(cond: Expr, consequence: Expr, alternative: Expr) -> Expr {
    Expr::If(
        Box::new(cond),
        BlockStmt(vec![Stmt::Expr(consequence)]),
        Some(BlockStmt(vec![Stmt::Expr(alternative)])),
    )
}

fn _func(args: Vec<&str>, body: Vec<Expr>) -> Expr {
    Expr::Func(
        args.iter()
            .map(|s| String::from(*s))
            .collect::<Vec<String>>(),
        BlockStmt(
            body.iter()
                .map(|e| Stmt::Expr(e.clone()))
                .collect::<Vec<Stmt>>(),
        ),
    )
}

fn _call(func: Expr, params: Vec<Expr>) -> Expr {
    Expr::Call(Box::new(func), params)
}
