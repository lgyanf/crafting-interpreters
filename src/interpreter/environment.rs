use crate::interpreter::value::Value;

type Scope = std::collections::HashMap<String, Value>;

pub struct Environment {
    root: Scope,
    child_scopes: Vec<Scope>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            root: Scope::new(),
            child_scopes: Vec::new(),
        }
    }

    pub fn new_scope(&mut self) {
        self.child_scopes.push(Scope::new());
    }

    pub fn destroy_scope(&mut self) {
        self.child_scopes.pop();
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.child_scopes.last_mut().unwrap_or(&mut self.root).insert(name, value);    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.child_scopes.iter().rev() {
            let scope_lookup = scope.get(name);
            match scope_lookup {
                Some(_) => return scope_lookup,
                None => {},
            }
        }
        self.root.get(name)
    }

    pub fn contains_key(&self, name: &str) -> bool {
        for scope in self.child_scopes.iter().rev() {
            let scope_lookup = scope.contains_key(name);
            match scope_lookup {
                true => return scope_lookup,
                false => {},
            }
        }
        self.root.contains_key(name)
    }
}