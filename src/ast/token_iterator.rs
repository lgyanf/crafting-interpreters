use std::iter::Peekable;

use crate::{token::Token, position::{PositionRange, Position}};

pub struct TokenIterator<'a> {
    token_iterator: Peekable<std::slice::Iter<'a, Token>>,
    last_position: Option<PositionRange>,
}

impl<'a> TokenIterator<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            token_iterator: tokens.iter().peekable(),
            last_position: Some(PositionRange { start: Position {
                line: 0,
                column: 0,
            }, end: Position {
                line: 0, column: 0,
            } }),
        }
    }

    pub fn peek(&mut self) -> Option<&&Token> {
        let peek = self.token_iterator.peek();
        peek
    }

    pub fn peek_clone(&mut self) -> Option<Token> {
        let peek = self.peek();
        match peek {
            None => None,
            Some(token_ref) => Some(token_ref.to_owned().to_owned())
        }
    }

    pub fn last_position(&self) -> Option<PositionRange> {
        match &self.last_position {
            None => None,
            Some(range) => Some(range.clone()),
        }
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
