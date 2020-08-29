use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    Int(i64),
    Bool(bool),
    ReturnValue(Box<Object>),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "NULL"),
            Object::Int(_) => write!(f, "INTEGER"),
            Object::Bool(_) => write!(f, "BOOLEAN"),
            Object::ReturnValue(val) => write!(f, "{}", *val),
            Object::Error(msg) => write!(f, "{}", msg),
        }
    }
}
