#![allow(unused)]
use std::ops::{ControlFlow, Deref, DerefMut, Index};

use arrowcss_extractor::cursor::Cursor;
use either::Either;
use smallvec::{smallvec, SmallVec};
use tracing::debug;
use tracing_subscriber::util;

use super::{UtilityCandidate, VariantCandidate};
use crate::{
    common::MaybeArbitrary,
    context::{
        utilities::{UtilityStorage, UtilityStorageImpl},
        Context,
    },
};

pub struct CandidateParser<'a> {
    input: &'a str,
    cursor: Cursor<'a>,
    state: State,
    ctx: &'a Context,
    utility: UtilityCandidate<'a>,
    variants: SmallVec<[VariantCandidate<'a>; 1]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

// str` to implement `std::ops::Index<candidate::Span>
impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[index.start..index.end]
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn to(&self, other: &Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }

    pub fn from_start(&self) -> Span {
        Span {
            start: 0,
            end: self.start,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
    /// An ident
    Ident(Span),
    /// `[...]`
    Arbitrary(&'a str),
    /// `/`
    Slash,
    /// `!`
    Bang,
    /// `-`
    Minus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum State {
    /// Initial state
    ///
    /// Accepts: Ident, Arbitrary
    Initial,
    /// At least one Ident has been parsed
    ///
    /// Accepts: Ident, Arbitrary, Colon, Slash
    AfterIdent,
    /// After Arbitrary
    ///
    /// Accepts: Colon, Slash
    AfterArbitrary,
    /// After Slash
    ///
    /// Accepts: Ident, Arbitrary
    AfterSlash,
    /// Done
    Eof,
}

impl State {
    pub fn transform(&mut self, token: Token) -> Result<(), ()> {
        let res = match (&self, token) {
            (State::Initial, Token::Bang) => Ok(State::Initial),
            (State::Initial, Token::Minus) => Ok(State::Initial),
            (State::Initial, Token::Ident(_)) => Ok(State::AfterIdent),
            (State::Initial, Token::Arbitrary(_)) => Ok(State::AfterArbitrary),
            (State::AfterIdent, Token::Ident(_)) => Ok(State::AfterIdent),
            (State::AfterIdent, Token::Arbitrary(_)) => Ok(State::AfterArbitrary),
            (State::AfterIdent, Token::Slash) => Ok(State::AfterSlash),
            (State::AfterArbitrary, Token::Slash) => Ok(State::AfterSlash),
            (State::AfterSlash, Token::Ident(_)) => Ok(State::Eof),
            (State::AfterSlash, Token::Arbitrary(_)) => Ok(State::Eof),
            _ => Err(()),
        };
        debug!("transform: {:?} to {:?}, token: {:?}", self, res, token);
        *self = res?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Repr<'a> {
    value: &'a str,
    idents: SmallVec<[Span; 2]>,
    arbitrary: Option<&'a str>,
    modifier: Option<Either<Span, &'a str>>,
}

impl<'a> Repr<'a> {
    pub fn new() -> Self {
        Self {
            value: "",
            idents: SmallVec::new(),
            arbitrary: None,
            modifier: None,
        }
    }
}

impl<'a> Deref for CandidateParser<'a> {
    type Target = Cursor<'a>;

    fn deref(&self) -> &Self::Target {
        &self.cursor
    }
}

impl<'a> DerefMut for CandidateParser<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cursor
    }
}

impl<'a> CandidateParser<'a> {
    pub fn new(ctx: &'a Context, input: &'a str) -> Self {
        Self {
            input,
            ctx,
            state: State::Initial,
            cursor: Cursor::new(input),
            utility: UtilityCandidate::default(),
            variants: SmallVec::new(),
        }
    }

    pub fn str_from(&self, start: usize) -> &'a str {
        &self.input[start..self.cursor.pos()]
    }

    pub fn span_from(&self, start: usize) -> Span {
        Span {
            start,
            end: self.cursor.pos(),
        }
    }

    fn consume<R>(&mut self, f: impl FnOnce(&mut Cursor<'a>) -> R) -> &'a str {
        let start = self.cursor.pos();
        f(&mut self.cursor);
        self.str_from(start)
    }

    pub fn next_token(&mut self) -> Result<Option<Token<'a>>, ()> {
        let start = self.cursor.pos();
        let res = self
            .consume(|c| c.eat_until(|c| c == '-' || c == '[' || c == ']' || c == '/' || c == '!'));

        let res = match self.first() {
            '[' => {
                self.bump();
                let start = self.cursor.pos();
                self.eat_until_char(b']');
                let tok = self.str_from(start);
                Some(Token::Arbitrary(tok))
            }
            '-' => Some(Token::Ident(self.span_from(start))),
            '!' => Some(Token::Bang),
            '/' => Some(Token::Slash),
            '\0' if !res.is_empty() => Some(Token::Ident(self.span_from(start))),
            '\0' => None,
            _ => unreachable!(),
        };

        if let Some(tok) = res {
            self.state.transform(tok)?;
            self.bump();
        }

        Ok(res)
    }

    pub fn parse_ast(&mut self) -> Option<Repr<'a>> {
        let mut accept_modifier = false;
        let mut repr = Repr::new();

        while let Some(token) = self.next_token().ok()? {
            debug!("token: {:?}, state: {:?}", token, self.state);
            match token {
                Token::Bang => {
                    // repr.negative = true;
                }
                Token::Ident(span) => {
                    if self.state == State::Eof {
                        repr.modifier = Some(Either::Left(span));
                        accept_modifier = false;
                    } else {
                        repr.idents.push(span);
                    }
                }
                Token::Arbitrary(arb) => {
                    if self.state == State::Eof {
                        repr.modifier = Some(Either::Right(arb));
                    } else {
                        repr.arbitrary = Some(arb);
                    }
                }
                _ => {}
            }
        }
        Some(repr)
    }

    pub fn parse_utility(&mut self, ut: &UtilityStorageImpl) -> Option<UtilityCandidate<'a>> {
        let repr = self.parse_ast()?;

        if let Some(arb) = repr.arbitrary {
            if repr.idents.is_empty() {
                let (key, value) = arb.split_once(':')?;
                return Some(UtilityCandidate {
                    key,
                    value: Some(MaybeArbitrary::Arbitrary(value)),
                    modifier: None,
                    arbitrary: true,
                    important: false,
                    negative: false,
                });
            }

            let key = repr.idents[0].to(repr.idents.last().unwrap());

            debug!("key: {:?}", &self.input[key]);
            return Some(UtilityCandidate {
                key: &self.input[key],
                value: Some(MaybeArbitrary::Arbitrary(arb)),
                modifier: match repr.modifier {
                    Some(Either::Left(span)) => Some(MaybeArbitrary::Named(&self.input[span])),
                    Some(Either::Right(arb)) => Some(MaybeArbitrary::Arbitrary(arb)),
                    None => None,
                },
                arbitrary: false,
                important: false,
                negative: false,
            });
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use tracing::Level;

    use super::*;
    use crate::preset::preset_tailwind;

    #[test]
    fn test_parse() {
        tracing_subscriber::fmt()
            .without_time()
            .with_max_level(Level::DEBUG)
            .init();

        let input = r#"[color:red]"#;
        let mut ctx = Context::default();
        preset_tailwind(&mut ctx);
        let mut parser = CandidateParser::new(&ctx, input);

        let ast = parser.parse_utility(&ctx.utilities);
        println!("{:#?}", ast);
    }
}
