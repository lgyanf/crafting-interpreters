use std::{iter::Peekable, str::Chars};

use crate::position::{Line, Column, Position};

pub struct ScannerIterator<'a> {
    it: Peekable<Chars<'a>>,
    current_line: Line,
    current_column: Column,
    prev_char_is_line_break: bool,
}

impl<'a> ScannerIterator<'a> {
    pub fn new(s: &'a str) -> Self {
        ScannerIterator {
            it: s.chars().into_iter().peekable(),
            current_line: 0,
            current_column: 0,
            prev_char_is_line_break: false
        }
    }

    pub fn current_position(&self) -> Position {
        Position {
            line: self.current_line,
            column: self.current_column,
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.it.peek()
    }
}

impl<'a> Iterator for ScannerIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.it.next();
        if self.prev_char_is_line_break {
            self.current_line += 1;
            self.current_column = 0;
        } else {
            self.current_column += 1;
        }
        match next {
            Some(c) if c == '\n' => self.prev_char_is_line_break = true,
            _ => self.prev_char_is_line_break = false,
        }
        next
    }
}