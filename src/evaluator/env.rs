use crate::evaluator::object::Object;
use std::collections::HashMap;

pub struct Env {
    store: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: String) -> Option<Object> {
        match self.store.get(&name) {
            Some(value) => Some(value.clone()),
            None => None,
        }
    }

    pub fn set(&mut self, name: String, value: &Object) {
        self.store.insert(name, value.clone());
    }
}
