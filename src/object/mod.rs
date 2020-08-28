use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Object {
    Null,
    Int(i64),
    Bool(bool),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Int(num) => write!(f, "{}", num),
            Object::Bool(boolean) => write!(f, "{}", boolean),
        }
    }
}
