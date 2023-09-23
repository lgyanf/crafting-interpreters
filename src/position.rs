pub type Line = usize;
pub type Column = usize;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Position {
    pub line: Line,
    pub column: Column,
}

impl Position {
    pub fn as_range(&self) -> PositionRange {
        PositionRange {
            start: self.clone(),
            end: self.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PositionRange {
    pub start: Position,
    pub end: Position,
}

impl PositionRange {
    pub fn from_bounds(left: &PositionRange, right: &PositionRange) -> Self {
        Self {
            start: left.start.clone(),
            end: right.end.clone(),
        }
    }
}
