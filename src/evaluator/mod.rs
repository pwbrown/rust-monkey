#[cfg(test)]
mod tests;

use crate::ast::*;
use crate::object::*;

pub fn eval(program: Program) -> Object {
    let mut result = Object::Null;

    for stmt in program.0 {
        result = eval_stmt(stmt);
    }

    result
}

fn eval_stmt(stmt: Stmt) -> Object {
    match stmt {
        Stmt::Expr(expr) => eval_expr(expr),
        _ => Object::Null,
    }
}

fn eval_expr(expr: Expr) -> Object {
    match expr {
        Expr::Literal(lit) => eval_literal(lit),
        Expr::Prefix(op, right) => eval_prefix_expr(op, eval_expr(*right)),
        Expr::Infix(left, op, right) => eval_infix_expr(op, eval_expr(*left), eval_expr(*right)),
        Expr::If(cond, cons, alt) => eval_if_expr(*cond, cons, alt),
        _ => Object::Null,
    }
}

fn eval_literal(literal: Literal) -> Object {
    match literal {
        Literal::Int(int) => Object::Int(int),
        Literal::Bool(boolean) => Object::Bool(boolean),
    }
}

fn eval_prefix_expr(operator: Prefix, right: Object) -> Object {
    match operator {
        Prefix::Not => eval_not_prefix_expr(right),
        Prefix::Negative => eval_negative_prefix_expr(right),
    }
}

fn eval_not_prefix_expr(right: Object) -> Object {
    match right {
        Object::Bool(true) => Object::Bool(false),
        Object::Bool(false) => Object::Bool(true),
        Object::Null => Object::Bool(true),
        _ => Object::Bool(false),
    }
}

fn eval_negative_prefix_expr(right: Object) -> Object {
    match right {
        Object::Int(int) => Object::Int(-int),
        _ => Object::Null,
    }
}

fn eval_infix_expr(operator: Infix, left: Object, right: Object) -> Object {
    match left {
        Object::Int(left) => {
            if let Object::Int(right) = right {
                eval_integer_infix_expr(operator, left, right)
            } else {
                Object::Null
            }
        }
        Object::Bool(left) => {
            if let Object::Bool(right) = right {
                eval_boolean_infix_expr(operator, left, right)
            } else {
                Object::Null
            }
        }
        _ => Object::Null,
    }
}

fn eval_integer_infix_expr(operator: Infix, left: i64, right: i64) -> Object {
    match operator {
        Infix::Plus => Object::Int(left + right),
        Infix::Minus => Object::Int(left - right),
        Infix::Multiply => Object::Int(left * right),
        Infix::Divide => Object::Int(left / right),
        Infix::Lt => Object::Bool(left < right),
        Infix::Gt => Object::Bool(left > right),
        Infix::Eq => Object::Bool(left == right),
        Infix::Neq => Object::Bool(left != right),
    }
}

fn eval_boolean_infix_expr(operator: Infix, left: bool, right: bool) -> Object {
    match operator {
        Infix::Eq => Object::Bool(left == right),
        Infix::Neq => Object::Bool(left != right),
        _ => Object::Null,
    }
}

fn eval_if_expr(condition: Expr, consequence: BlockStmt, alternative: Option<BlockStmt>) -> Object {
    if is_truthy(eval_expr(condition)) {
        eval(consequence)
    } else if let Some(alternative) = alternative {
        eval(alternative)
    } else {
        Object::Null
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null | Object::Bool(false) => false,
        _ => true,
    }
}
