#[cfg(test)]
mod tests;

pub mod env;
pub mod object;

use crate::ast::*;
use crate::evaluator::env::*;
use crate::evaluator::object::*;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Evaluator {
    pub env: Rc<RefCell<Env>>,
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<Env>>) -> Evaluator {
        Evaluator { env }
    }

    pub fn eval(&self, program: Program) -> Object {
        let mut result = Object::Undefined;

        for stmt in program.0 {
            match self.eval_stmt(stmt) {
                Object::ReturnValue(val) => return *val,
                Object::Error(msg) => return Object::Error(msg),
                obj => result = obj,
            }
        }

        result
    }

    // Same as eval except it does not unwrap ReturnValue
    fn eval_block_stmt(&self, block: BlockStmt) -> Object {
        let mut result = Object::Undefined;

        for stmt in block.0 {
            match self.eval_stmt(stmt) {
                Object::ReturnValue(val) => return Object::ReturnValue(val),
                Object::Error(msg) => return Object::Error(msg),
                obj => result = obj,
            }
        }

        result
    }

    fn eval_stmt(&self, stmt: Stmt) -> Object {
        match stmt {
            Stmt::Let(ident, expr) => {
                let value = self.eval_expr(expr);
                if self.is_error(&value) {
                    value
                } else {
                    self.env.borrow_mut().set(ident, &value);
                    Object::Undefined
                }
            }
            Stmt::Expr(expr) => self.eval_expr(expr),
            Stmt::Return(expr) => Object::ReturnValue(Box::new(self.eval_expr(expr))),
        }
    }

    fn eval_expr(&self, expr: Expr) -> Object {
        match expr {
            Expr::Literal(lit) => self.eval_literal(lit),
            Expr::Prefix(op, right) => self.eval_prefix_expr(op, self.eval_expr(*right)),
            Expr::Infix(left, op, right) => {
                self.eval_infix_expr(op, self.eval_expr(*left), self.eval_expr(*right))
            }
            Expr::If(cond, cons, alt) => self.eval_if_expr(*cond, cons, alt),
            Expr::Ident(name) => self.eval_identifier(&name),
            Expr::Func(params, body) => Object::Func(params, body, Rc::clone(&self.env)),
            Expr::Call(func, args) => self.eval_call_expr(*func, args),
        }
    }

    fn eval_literal(&self, literal: Literal) -> Object {
        match literal {
            Literal::Int(int) => Object::Int(int),
            Literal::Bool(boolean) => Object::Bool(boolean),
        }
    }

    fn eval_prefix_expr(&self, operator: Prefix, right: Object) -> Object {
        match operator {
            Prefix::Not => self.eval_not_prefix_expr(right),
            Prefix::Negative => self.eval_negative_prefix_expr(right),
        }
    }

    fn eval_not_prefix_expr(&self, right: Object) -> Object {
        match right {
            Object::Bool(true) => Object::Bool(false),
            Object::Bool(false) => Object::Bool(true),
            Object::Undefined => Object::Bool(true),
            _ => Object::Bool(false),
        }
    }

    fn eval_negative_prefix_expr(&self, right: Object) -> Object {
        match right {
            Object::Int(int) => Object::Int(-int),
            _ => Object::Error(format!("unknown operator: -{}", right.get_type())),
        }
    }

    fn eval_infix_expr(&self, operator: Infix, left: Object, right: Object) -> Object {
        match left {
            Object::Int(left) => {
                if let Object::Int(right) = right {
                    self.eval_integer_infix_expr(operator, left, right)
                } else {
                    Object::Error(format!(
                        "type mismatch: INTEGER {} {}",
                        operator,
                        right.get_type()
                    ))
                }
            }
            Object::Bool(left) => {
                if let Object::Bool(right) = right {
                    self.eval_boolean_infix_expr(operator, left, right)
                } else {
                    Object::Error(format!(
                        "type mismatch: BOOLEAN {} {}",
                        operator,
                        right.get_type()
                    ))
                }
            }
            _ => Object::Error(format!(
                "unknown operator: {} {} {}",
                left.get_type(),
                operator,
                right.get_type()
            )),
        }
    }

    fn eval_integer_infix_expr(&self, operator: Infix, left: i64, right: i64) -> Object {
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

    fn eval_boolean_infix_expr(&self, operator: Infix, left: bool, right: bool) -> Object {
        match operator {
            Infix::Eq => Object::Bool(left == right),
            Infix::Neq => Object::Bool(left != right),
            _ => Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", operator,)),
        }
    }

    fn eval_if_expr(
        &self,
        condition: Expr,
        consequence: BlockStmt,
        alternative: Option<BlockStmt>,
    ) -> Object {
        if self.is_truthy(self.eval_expr(condition)) {
            self.eval_block_stmt(consequence)
        } else if let Some(alternative) = alternative {
            self.eval_block_stmt(alternative)
        } else {
            Object::Undefined
        }
    }

    fn eval_identifier(&self, name: &str) -> Object {
        match self.env.borrow_mut().get(String::from(name)) {
            Some(value) => value,
            None => Object::Error(format!("identifier not found: {}", name)),
        }
    }

    fn eval_call_expr(&self, func: Expr, args: Vec<Expr>) -> Object {
        // Evaluate function
        let (params, body, env) = match self.eval_expr(func) {
            Object::Func(params, body, env) => (params, body, env),
            Object::Error(msg) => return Object::Error(msg),
            _ => return Object::Undefined,
        };
        // Evaluate arguments
        let args = args
            .iter()
            .map(|expr| self.eval_expr(expr.clone()))
            .collect::<Vec<_>>();
        if args.len() == 1 && self.is_error(&args[0]) {
            return args[0].clone();
        }
        if params.len() != args.len() {
            return Object::Error(format!(
                "wrong number of arguments: {} expected but got {}",
                params.len(),
                args.len()
            ));
        }

        let mut scoped_env = Env::new_with_outer(Rc::clone(&env));
        for (ident, val) in params.iter().zip(args.iter()) {
            scoped_env.set(String::from(ident), val);
        }

        let evaluator = Evaluator::new(Rc::new(RefCell::new(scoped_env)));

        let result = evaluator.eval(body);

        result
    }

    fn is_truthy(&self, obj: Object) -> bool {
        match obj {
            Object::Undefined | Object::Bool(false) => false,
            _ => true,
        }
    }

    fn is_error(&self, obj: &Object) -> bool {
        match obj {
            Object::Error(_) => true,
            _ => false,
        }
    }
}
