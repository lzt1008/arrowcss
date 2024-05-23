pub mod candidate;
pub mod state;
pub mod utility;
pub mod variant;

pub use self::{utility::*, variant::*};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ParserPosition {
    pub start: usize,
    pub end: usize,
}

impl ParserPosition {
    pub fn advance(&mut self, n: usize) {
        self.start += n;
    }

    pub fn retreat(&mut self, n: usize) {
        self.end -= n;
    }
}
