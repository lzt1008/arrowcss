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
    context::{utilities::UtilityStorage, Context},
};

pub struct Candidate<'a> {
    utility: UtilityCandidate<'a>,
    variants: SmallVec<[VariantCandidate<'a>; 1]>,
}

pub struct CandidateParser<'a> {
    input: &'a str,
    cursor: Cursor<'a>,
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
    /// `:`
    Colon,
    /// `/`
    Slash,
}

#[derive(Debug, Clone, Copy)]
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
    EofOrColon,
}

impl State {
    pub fn transform(self, token: Token) -> Option<Self> {
        let res = match (self, token) {
            (State::Initial, Token::Ident(_)) => Some(State::AfterIdent),
            (State::Initial, Token::Arbitrary(_)) => Some(State::AfterArbitrary),
            (State::Initial, Token::Colon) => Some(State::Initial),
            (State::AfterIdent, Token::Ident(_)) => Some(State::AfterIdent),
            (State::AfterIdent, Token::Arbitrary(_)) => Some(State::AfterArbitrary),
            (State::AfterIdent, Token::Colon) => Some(State::Initial),
            (State::AfterIdent, Token::Slash) => Some(State::AfterSlash),
            (State::AfterArbitrary, Token::Colon) => Some(State::Initial),
            (State::AfterArbitrary, Token::Slash) => Some(State::AfterSlash),
            (State::AfterSlash, Token::Ident(_)) => Some(State::EofOrColon),
            (State::AfterSlash, Token::Arbitrary(_)) => Some(State::EofOrColon),
            (State::EofOrColon, Token::Colon) => Some(State::Initial),
            _ => None,
        };
        debug!("transform: {:?} to {:?}, token: {:?}", self, res, token);
        res
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

#[derive(Debug, Clone)]
pub struct StyleHierarchy<'a> {
    variants: SmallVec<[Repr<'a>; 1]>,
    utility: Repr<'a>,
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

    pub fn parse(&mut self) -> Option<Candidate<'a>> {
        let ast = self.parse_ast()?;

        let utility = ast.utility;
        let first = Span::new(0, 0);

        let (key, value) = if let Some(arb) = utility.arbitrary {
            (
                &self.input[utility
                    .idents
                    .first()
                    .unwrap()
                    .to(utility.idents.last().unwrap())],
                MaybeArbitrary::Arbitrary(arb),
            )
        } else {
            let mut iter = utility.idents.iter().rev();
            let mut prev = utility.idents.last().unwrap();
            iter.find_map(|cur| {
                let key = &self.input[utility.idents.first().unwrap().to(cur)];
                debug!("key: {:?}", key);
                let res = self
                    .ctx
                    .utilities
                    .get(key)
                    .map(|_| (key, MaybeArbitrary::Named(&self.input[cur.end..utility.idents.last().unwrap().end])));
                prev = cur;
                res
            })?
        };

        let utility = UtilityCandidate {
            key,
            value: Some(value),
            modifier: match utility.modifier {
                Some(Either::Left(span)) => Some(MaybeArbitrary::Named(self.str_from(span.start))),
                Some(Either::Right(arb)) => Some(MaybeArbitrary::Arbitrary(&arb)),
                None => None,
            },
            arbitrary: false,
            important: false,
            negative: false,
        };

        debug!("utility: {:#?}", utility);

        // let variants = ast
        //     .variants
        //     .into_iter()
        //     .map(|repr| {

        //     })
        //     .collect();
        todo!()
    }

    pub fn parse_next(&mut self) -> Option<()> {
        let mut tokens: SmallVec<[Token; 3]> = smallvec![];

        let mut is_variant = false;
        while let Some(token) = self.next_token() {
            if Token::Colon == token {
                is_variant = true;
                break;
            }
            tokens.push(token);
        }
        if let Some(Token::Arbitrary(arb)) = tokens.last() {
            // full arbitrary e.g. [color:red]
            if tokens.is_empty() {
                todo!()
            } else if let (Some(Token::Ident(ident)), Some(Token::Ident(first))) =
                (tokens.pop(), tokens.first())
            {
                let span = first.to(&ident);
            }
        }
        // from back to front, try
        while let (Some(Token::Ident(ident)), Some(Token::Ident(first))) =
            (tokens.last(), tokens.first())
        {
            let span = first.to(ident);
            tokens.pop();
        }

        // todo!()
        Some(())
    }

    pub fn parse_ast(&mut self) -> Option<StyleHierarchy<'a>> {
        let mut is_variant = false;
        let mut accept_modifier = false;
        let mut variants = SmallVec::new();
        let mut repr = Repr::new();

        let mut state = State::Initial;

        while let Some(token) = self.next_token() {
            // filter out invalid occurrences
            state = state.transform(token)?;

            match token {
                Token::Ident(span) => {
                    if accept_modifier {
                        repr.modifier = Some(Either::Left(span));
                        accept_modifier = false;
                    } else {
                        repr.idents.push(span);
                    }
                }
                Token::Arbitrary(arb) => {
                    if accept_modifier {
                        repr.modifier = Some(Either::Right(arb));
                        accept_modifier = false;
                    } else {
                        repr.arbitrary = Some(arb);
                    }
                }
                Token::Colon => {
                    variants.push(std::mem::take(&mut repr));
                }
                Token::Slash => {
                    accept_modifier = true;
                }
            }
        }
        let ast = StyleHierarchy {
            utility: repr,
            variants,
        };
        Some(ast)
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        let start = self.cursor.pos();
        let res = self
            .consume(|c| c.eat_until(|c| c == '-' || c == '[' || c == ']' || c == ':' || c == '/'));
        debug!("res: {:?}", res);

        // if !res.is_empty() {
        //     return Some(Token::Ident(self.span_from(start)));
        // }

        let res = match self.first() {
            '[' => {
                let start = self.cursor.pos();
                self.eat_until_char(b']');
                let tok = self.str_from(start);
                self.bump();
                Some(Token::Arbitrary(tok))
            }
            '-' => Some(Token::Ident(self.span_from(start))),
            ':' => Some(Token::Colon),
            '/' => Some(Token::Slash),
            '\0' if !res.is_empty() => Some(Token::Ident(self.span_from(start))),
            '\0' => None,
            _ => unreachable!(),
        };

        self.bump();

        res
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

        let input = r#"hover:dark:border-x-red-500/80"#;
        let mut ctx = Context::default();
        preset_tailwind(&mut ctx);
        let mut parser = CandidateParser::new(&ctx, input);

        let ast = parser.parse();
    }
}
