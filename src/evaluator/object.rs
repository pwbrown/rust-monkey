use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Undefined,
    Int(i64),
    Bool(bool),
    ReturnValue(Box<Object>),
    Error(String),
}

impl Object {
    pub fn get_type(&self) -> String {
        match self {
            Object::Undefined => String::from("UNDEFINED"),
            Object::Int(_) => String::from("INTEGER"),
            Object::Bool(_) => String::from("BOOLEAN"),
            Object::ReturnValue(val) => val.get_type(),
            _ => String::from("UNKNOWN"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Undefined => write!(f, "undefined"),
            Object::Int(num) => write!(f, "{}", num),
            Object::Bool(boolean) => write!(f, "{}", boolean),
            Object::ReturnValue(val) => write!(f, "{}", *val),
            Object::Error(msg) => write!(f, "{}", msg),
        }
    }
}
