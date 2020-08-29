use crate::ast::*;
use crate::evaluator::env::Env;
use crate::evaluator::object::Object;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::cell::RefCell;
use std::rc::Rc;

// ********************** UTILITIES **************************

// Parses a program given an input string
fn eval(input: &str) -> Object {
    let env = Rc::new(RefCell::new(Env::new()));
    let evaluator = Evaluator::new(env);
    let program = Parser::new(Lexer::new(input)).parse();
    evaluator.eval(program)
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
        ("if (false) { 10 }", Object::Undefined),
        ("if (1) { 10 }", Object::Int(10)),
        ("if (1 < 2) { 10 }", Object::Int(10)),
        ("if (1 > 2) { 10 }", Object::Undefined),
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
        ("foobar", "identifier not found: foobar"),
    ];

    for (input, msg) in tests {
        assert_eq!(eval(input), Object::Error(String::from(msg)));
    }
}

#[test]
fn test_let_statements() {
    let tests = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for (input, result) in tests {
        assert_eq!(eval(input), Object::Int(result));
    }
}

#[test]
fn test_function_object() {
    let tests = vec![(
        "fn(x) { x + 2; };",
        vec![String::from("x")],
        vec![Stmt::Expr(Expr::Infix(
            Box::new(Expr::Ident(String::from("x"))),
            Infix::Plus,
            Box::new(Expr::Literal(Literal::Int(2))),
        ))],
    )];

    for (input, exp_params, exp_body) in tests {
        let obj = eval(input);
        match obj {
            Object::Func(params, body, _) => {
                assert_eq!(params, exp_params);
                assert_eq!(body, BlockStmt(exp_body));
            }
            _ => panic!("Expected a Function Object"),
        }
    }
}

#[test]
fn test_function_application() {
    let tests = vec![
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
        ("let rate = 5; let mult = fn(x) { x * rate }; mult(5);", 25),
    ];

    for (input, result) in tests {
        assert_eq!(eval(input), Object::Int(result));
    }
}

#[test]
fn test_enclosing_envs() {
    let input = "
        let first = 10;
        let second = 10;
        let third = 10;
        
        let ourFunction = fn(first) {
            let second = 20;
        
            first + second + third;
        };
        
        ourFunction(20) + first + second;
    ";

    assert_eq!(eval(input), Object::Int(70));
}

#[test]
fn test_closures() {
    let input = "
        let newAdder = fn(x) {
            fn(y) { x + y; };
        };
        
        let addTwo = newAdder(2);

        addTwo(2);
    ";

    assert_eq!(eval(input), Object::Int(4));
}
