use crate::ast::BlockStmt;
use crate::evaluator::env::Env;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    ReturnValue(Box<Object>),
    Func(Vec<String>, BlockStmt, Rc<RefCell<Env>>),
    Error(String),
    Undefined,
}

impl Object {
    pub fn get_type(&self) -> String {
        match self {
            Object::Int(_) => String::from("INTEGER"),
            Object::Bool(_) => String::from("BOOLEAN"),
            Object::String(_) => String::from("STRING"),
            Object::ReturnValue(val) => val.get_type(),
            Object::Func(_, _, _) => String::from("FUNCTION"),
            Object::Error(_) => String::from("ERROR"),
            Object::Undefined => String::from("UNDEFINED"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(num) => write!(f, "{}", num),
            Object::Bool(boolean) => write!(f, "{}", boolean),
            Object::String(string) => write!(f, "\"{}\"", string),
            Object::ReturnValue(val) => write!(f, "{}", *val),
            Object::Func(params, body, _) => {
                write!(f, "fn({}) {{\n{}\n}}", params.join(", "), body)
            }
            Object::Error(msg) => write!(f, "{}", msg),
            Object::Undefined => write!(f, "undefined"),
        }
    }
}
