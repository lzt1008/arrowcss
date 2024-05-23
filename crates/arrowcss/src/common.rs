use std::{fmt::Debug, ops::Deref};

use smallvec::SmallVec;
use smol_str::format_smolstr;

/// This trait provides a method to replace a character in a string
///
/// this method has a better performance than `str::replace`
/// because it doesn't allocate a new string.
///
/// Uses `memchr` and `smol_str`'s `format_smolstr!` to replace the character
///
/// Only when replaced string's length less than 22
pub trait StrReplaceExt {
    fn replace_char(&self, chr: char, replacement: &str) -> smol_str::SmolStr;
}

impl StrReplaceExt for str {
    fn replace_char(&self, chr: char, replacement: &str) -> smol_str::SmolStr {
        if let Some(pos) = memchr::memchr(chr as u8, self.as_bytes()) {
            return format_smolstr!("{}{}{}", &self[..pos], replacement, &self[pos + 1..]);
        }
        smol_str::SmolStr::from(self)
    }
}

/// This trait provides a method to split a string at the top level
///
/// e.g. `[&:hover]:[color:red]` will be split into `["&:hover", "color:red"]`
pub trait StrSplitExt {
    fn split_toplevel(&self, delimiter: u8) -> Option<SmallVec<[&str; 2]>>;
}

impl StrSplitExt for str {
    fn split_toplevel(&self, delimiter: u8) -> Option<SmallVec<[&str; 2]>> {
        let mut result = smallvec::smallvec![];
        let mut start = 0;
        let mut in_brackets = 0;

        let bytes = self.as_bytes();

        for index in memchr::memchr3_iter(delimiter, b'[', b']', bytes) {
            match bytes[index] {
                b':' if in_brackets == 0 => {
                    result.push(&self[start..index]);
                    start = index + 1;
                }
                b'[' => {
                    in_brackets += 1;
                }
                b']' => {
                    in_brackets -= 1;

                    if in_brackets < 0 {
                        return None;
                    }
                }
                _ => {}
            }
        }

        result.push(&self[start..]);

        Some(result)
    }
}

pub trait BasicParser {
    fn advance(&mut self, n: usize);
    fn is_eof(&self) -> bool;
    fn next_byte(&self) -> u8;
}

/// A representation of value
///
/// Either an arbitrary value or a named value
///
/// This is used to store e.g. `[#123456]` or `blur-500` in like `text-*`
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MaybeArbitrary<'a> {
    Arbitrary(&'a str),
    Named(&'a str),
}

impl MaybeArbitrary<'_> {
    pub fn take_arbitrary(&self) -> Option<&str> {
        match self {
            MaybeArbitrary::Arbitrary(s) => Some(s),
            _ => None,
        }
    }

    pub fn take_named(&self) -> Option<&str> {
        match self {
            MaybeArbitrary::Named(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            MaybeArbitrary::Arbitrary(s) => s,
            MaybeArbitrary::Named(s) => s,
        }
    }
}

impl Default for MaybeArbitrary<'_> {
    fn default() -> Self {
        MaybeArbitrary::Named("")
    }
}

impl<'a> Deref for MaybeArbitrary<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            MaybeArbitrary::Arbitrary(s) => s,
            MaybeArbitrary::Named(s) => s,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_str_replace() {
        let s = "hello";
        assert_eq!(s.replace_char('e', "abc").as_str(), "habcllo");
    }
}
