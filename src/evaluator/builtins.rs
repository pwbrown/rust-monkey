use crate::evaluator::object::Object;

pub fn get_builtin(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::Builtin(len)),
        "first" => Some(Object::Builtin(first)),
        "last" => Some(Object::Builtin(last)),
        "rest" => Some(Object::Builtin(rest)),
        "push" => Some(Object::Builtin(push)),
        _ => None,
    }
}

// Calculates length of string or array
fn len(args: Vec<Object>) -> Object {
    if let Some(err) = arg_len_error(1, args.len()) {
        return err;
    }
    match &args[0] {
        Object::String(string) => Object::Int(string.chars().count() as i64),
        Object::Array(elements) => Object::Int(elements.len() as i64),
        obj => arg_type_error("len", obj),
    }
}

// Returns the first element in an array
fn first(args: Vec<Object>) -> Object {
    if let Some(err) = arg_len_error(1, args.len()) {
        return err;
    }
    match &args[0] {
        Object::Array(elements) => match elements.first() {
            Some(obj) => obj.clone(),
            None => Object::Undefined,
        },
        obj => arg_type_error("first", obj),
    }
}

// Returns the last element in an array
fn last(args: Vec<Object>) -> Object {
    if let Some(err) = arg_len_error(1, args.len()) {
        return err;
    }
    match &args[0] {
        Object::Array(elements) => match elements.last() {
            Some(obj) => obj.clone(),
            None => Object::Undefined,
        },
        obj => arg_type_error("last", obj),
    }
}

// Returns all elements in an array except the first
fn rest(args: Vec<Object>) -> Object {
    if let Some(err) = arg_len_error(1, args.len()) {
        return err;
    }
    match &args[0] {
        Object::Array(elements) => {
            if elements.len() > 0 {
                Object::Array(elements[1..].to_vec())
            } else {
                Object::Undefined
            }
        }
        obj => arg_type_error("rest", obj),
    }
}

// Appends a new element to an array and returns a new array
fn push(args: Vec<Object>) -> Object {
    if let Some(err) = arg_len_error(2, args.len()) {
        return err;
    }
    match &args[0] {
        Object::Array(elements) => {
            let mut arr = elements.clone();
            arr.push(args[1].clone());
            Object::Array(arr)
        }
        obj => arg_type_error("push", obj),
    }
}

// Generates an argument length error if applicable
fn arg_len_error(expected: usize, actual: usize) -> Option<Object> {
    if expected != actual {
        return Some(Object::Error(format!(
            "wrong number of arguments: {} expected but got {}",
            expected, actual
        )));
    }
    None
}

// Generates an argument type error
fn arg_type_error(func: &str, obj: &Object) -> Object {
    Object::Error(format!(
        "argument to `{}` not supported, got {}",
        func,
        obj.get_type()
    ))
}
