use crate::evaluator::object::Object;

pub fn get_builtin(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::Builtin(len)),
        _ => None,
    }
}

// Calculates length of string
fn len(args: Vec<Object>) -> Object {
    if let Some(err) = arg_len_error(1, args.len()) {
        return err;
    }
    match &args[0] {
        Object::String(string) => Object::Int(string.chars().count() as i64),
        obj => Object::Error(format!(
            "argument to `len` not supported, got {}",
            obj.get_type()
        )),
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
