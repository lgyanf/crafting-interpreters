use std::fmt::Display;

use crate::error::LoxError;

#[derive(PartialEq, Clone)]
pub enum CallableType {
    Native {
        name: &'static str,
        arity: usize,
        call: fn(&Vec<Value>) -> Value,
    },
}

impl CallableType {
    pub fn arity(&self) -> usize {
        match self {
            CallableType::Native { name: _, arity, call: _ } => *arity,
        }
    }

    pub fn call(&self, args: &Vec<Value>) -> Result<Value, LoxError> {
        match self {
            CallableType::Native { name: _, arity: _, call } => Ok(call(args)),
        }
    }
}

impl Display for CallableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CallableType::Native { name, arity: _, call: _ } => write!(f, "NativeCallable({})", name),
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(CallableType),
}

impl Value {
    pub fn to_debug_string(&self) -> String {
        match self {
            Value::Nil => "Nil".to_owned(),
            Value::Boolean(b) => format!("Boolean({})", b),
            Value::Number(n) => format!("Number({})", n),
            Value::String(s) => format!("String({})", s),
            Value::Callable(ct) => format!("Callable({})", ct),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Callable(ct) => write!(f, "{}", ct),
        }
    }
}
