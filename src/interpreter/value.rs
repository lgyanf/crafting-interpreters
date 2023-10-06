use std::fmt::Display;


#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Value {
    pub fn to_debug_string(&self) -> String {
        match self {
            Value::Nil => "Nil".to_owned(),
            Value::Boolean(b) => format!("Boolean({})", b),
            Value::Number(n) => format!("Number({})", n),
            Value::String(s) => format!("String({})", s),
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
        }
    }
}
