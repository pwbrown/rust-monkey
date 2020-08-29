use crate::evaluator;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;

// ********************** UTILITIES **************************

// Parses a program given an input string
fn eval(input: &str) -> Object {
    evaluator::eval(Parser::new(Lexer::new(input)).parse())
}

#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, result) in tests {
        assert_eq!(eval(input), Object::Int(result));
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];

    for (input, result) in tests {
        assert_eq!(eval(input), Object::Bool(result));
    }
}

#[test]
fn test_eval_bang_operator() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for (input, result) in tests {
        assert_eq!(eval(input), Object::Bool(result));
    }
}

#[test]
fn test_if_else_expressions() {
    let tests = vec![
        ("if (true) { 10 }", Object::Int(10)),
        ("if (false) { 10 }", Object::Null),
        ("if (1) { 10 }", Object::Int(10)),
        ("if (1 < 2) { 10 }", Object::Int(10)),
        ("if (1 > 2) { 10 }", Object::Null),
        ("if (1 > 2) { 10 } else { 20 }", Object::Int(20)),
        ("if (1 < 2) { 10 } else { 20 }", Object::Int(10)),
    ];

    for (input, result) in tests {
        assert_eq!(eval(input), result);
    }
}

#[test]
fn test_return_statements() {
    let tests = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        (
            "
            if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }
            ",
            10,
        ),
    ];

    for (input, result) in tests {
        assert_eq!(eval(input), Object::Int(result));
    }
}

#[test]
fn test_error_handling() {
    let tests = vec![
        ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false", "unknown operator: BOOLEAN + BOOLEAN"),
        ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
        (
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        (
            "
            if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                }

                return 1;
            }
            ",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
    ];

    for (input, msg) in tests {
        assert_eq!(eval(input), Object::Error(String::from(msg)));
    }
}
