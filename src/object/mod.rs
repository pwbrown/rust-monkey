pub mod env;
pub use env::Env;

use crate::ast::BlockStmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

type BuiltinFunc = fn(Vec<Object>) -> Object;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    ReturnValue(Box<Object>),
    Func(Vec<String>, BlockStmt, Rc<RefCell<Env>>),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Builtin(BuiltinFunc),
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
            Object::Array(_) => String::from("ARRAY"),
            Object::Hash(_) => String::from("HASH"),
            Object::Builtin(_) => String::from("BUILTIN"),
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
            Object::Array(elements) => {
                let elem_strs: Vec<String> = elements.iter().map(|e| format!("{}", e)).collect();
                write!(f, "[{}]", elem_strs.join(", "))
            }
            Object::Hash(hash) => {
                let pairs: Vec<String> =
                    hash.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "{{{}}}", pairs.join(", "))
            }
            Object::Builtin(_) => write!(f, "builtin function"),
            Object::Error(msg) => write!(f, "{}", msg),
            Object::Undefined => write!(f, "undefined"),
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H>(&self, _hasher: &mut H)
    where
        H: Hasher,
    {
        // match *self {
        //     Object::Int(ref i) => i.hash(hasher),
        //     Object::Bool(ref b) => b.hash(hasher),
        //     Object::String(ref s) => s.hash(hasher),
        //     _ => "".hash(hasher),
        // }
    }
}
