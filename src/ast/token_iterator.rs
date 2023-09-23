use std::iter::Peekable;

use crate::{
    position::{Position, PositionRange},
    token::Token,
};

pub struct TokenIterator<'a> {
    token_iterator: Peekable<std::slice::Iter<'a, Token>>,
    last_position: Option<PositionRange>,
}

impl<'a> TokenIterator<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            token_iterator: tokens.iter().peekable(),
            last_position: Some(PositionRange {
                start: Position { line: 0, column: 0 },
                end: Position { line: 0, column: 0 },
            }),
        }
    }

    pub fn peek(&mut self) -> Option<&&Token> {
        let peek = self.token_iterator.peek();
        peek
    }

    pub fn peek_clone(&mut self) -> Option<Token> {
        let peek = self.peek();
        peek.map(|token_ref| token_ref.to_owned().to_owned())
    }

    pub fn last_position(&self) -> Option<PositionRange> {
        self.last_position.as_ref().cloned()
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.token_iterator.next();
        self.last_position = match next {
            None => self.last_position.clone(),
            Some(t) => Some(t.position.clone()),
        };
        next
    }
}
