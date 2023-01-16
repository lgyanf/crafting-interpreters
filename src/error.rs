use std::{error::Error, fmt::Display};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum LoxErrorKind {
    Lexical,
    Syntax,
    Runtime,
    Type,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LoxError {
    pub kind: LoxErrorKind,
    pub line: u32,
    pub message: String,
}

impl Error for LoxError {}

impl Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {}\nline {}", self.kind, self.message, self.line)
    }
}
