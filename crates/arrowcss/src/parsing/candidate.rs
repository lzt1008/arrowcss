use std::ops::Index;

use arrowcss_extractor::cursor::Cursor;
use derive_more::{Deref, DerefMut};
use either::Either::{self, Left, Right};
use smallvec::{smallvec, SmallVec};
use tracing::debug;

use super::{
    state::{State, StateTransformer, UtilityTransformer},
    UtilityCandidate, VariantCandidate,
};
use crate::{
    common::MaybeArbitrary,
    context::{
        utilities::{UtilityStorage, UtilityStorageImpl},
        VariantStorage,
    },
    parsing::state::VariantTransformer,
    process::{Variant, VariantKind},
};

#[derive(Deref, DerefMut)]
pub struct CandidateParser<'a> {
    input: &'a str,
    #[deref]
    #[deref_mut]
    cursor: Cursor<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

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
    /// `@`
    At,
}

impl<'a> CandidateParser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            cursor: Cursor::new(input),
        }
    }

    fn str_from(&self, start: usize) -> &'a str {
        &self.input[start..self.cursor.pos()]
    }

    fn span_from(&self, start: usize) -> Span {
        Span {
            start,
            end: self.cursor.pos(),
        }
    }

    fn next_token(&mut self) -> Result<Option<Token<'a>>, ()> {
        let start = self.pos();

        let res = match self.first() {
            'a'..='z' | '0'..='9' => {
                self.eat_while(|c| c.is_ascii_lowercase() || c.is_ascii_digit());
                let res = Some(Token::Ident(self.span_from(start)));
                if self.first() == '-' {
                    self.bump();
                }
                res
            }
            '[' => {
                self.bump();
                let start = self.cursor.pos();
                self.eat_until_char(b']');
                let tok = self.str_from(start);
                Some(Token::Arbitrary(tok))
            }
            '-' => Some(Token::Minus),
            '!' => Some(Token::Bang),
            '/' => Some(Token::Slash),
            '@' => Some(Token::At),
            '\0' => None,
            _ => return Err(()),
        };

        if !matches!(res, Some(Token::Ident(_))) {
            self.bump();
        }

        Ok(res)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct UtilityRepr<'a> {
    idents: SmallVec<[Span; 2]>,
    arbitrary: Option<&'a str>,
    modifier: Option<Either<Span, &'a str>>,
    important: bool,
    negative: bool,
}

impl<'a> CandidateParser<'a> {
    fn parse_utility_repr(&mut self) -> Option<UtilityRepr<'a>> {
        let mut repr = UtilityRepr::default();
        let mut state = State::Initial;

        while let Some(token) = self.next_token().ok()? {
            debug!("token: {:?}, state: {:?}", token, state);
            let new_state = UtilityTransformer::transform(&state, token).ok()?;
            match (token, state) {
                (Token::Bang, State::Initial) if !repr.important => {
                    repr.important = true;
                }
                (Token::Bang, _) if !repr.important && new_state == State::Eof => {
                    repr.important = true;
                }
                (Token::Minus, State::Initial) if !repr.negative => {
                    repr.negative = true;
                }
                (Token::Ident(span), State::Initial | State::AfterIdent) => {
                    repr.idents.push(span);
                }
                (Token::Arbitrary(arb), State::Initial | State::AfterIdent) => {
                    repr.arbitrary = Some(arb);
                }
                (Token::Ident(span), State::AfterSlash) if repr.modifier.is_none() => {
                    repr.modifier = Some(Either::Left(span));
                }
                (Token::Arbitrary(arb), State::AfterSlash) if repr.modifier.is_none() => {
                    repr.modifier = Some(Either::Right(arb));
                }
                (Token::Slash, State::AfterArbitrary | State::AfterIdent) => {}
                _ => return None,
            }
            state = new_state;
        }
        Some(repr)
    }

    pub fn parse_utility(&mut self, ut: &UtilityStorageImpl) -> Option<UtilityCandidate<'a>> {
        let repr = self.parse_utility_repr()?;

        if let Some(arb) = repr.arbitrary {
            if repr.idents.is_empty() {
                let (key, value) = arb.split_once(':')?;
                return Some(UtilityCandidate {
                    key,
                    value: Some(MaybeArbitrary::Arbitrary(value)),
                    modifier: None,
                    arbitrary: true,
                    important: repr.important,
                    negative: repr.negative,
                });
            }

            let key = repr.idents[0].to(repr.idents.last().unwrap());

            ut.get(&self.input[key])?;
            return Some(UtilityCandidate {
                key: &self.input[key],
                value: Some(MaybeArbitrary::Arbitrary(arb)),
                modifier: match repr.modifier {
                    Some(Either::Left(span)) => Some(MaybeArbitrary::Named(&self.input[span])),
                    Some(Either::Right(arb)) => Some(MaybeArbitrary::Arbitrary(arb)),
                    None => None,
                },
                arbitrary: false,
                important: repr.important,
                negative: repr.negative,
            });
        }

        debug!("{:?}", repr);

        let mut iter = repr.idents.iter().rev().peekable();
        let mut prev = iter.next();

        for ident in iter {
            let key = &self.input[repr.idents[0].to(ident)];
            if ut.get(key).is_some() {
                return Some(UtilityCandidate {
                    key,
                    value: Some(MaybeArbitrary::Named(
                        &self.input[prev.unwrap().to(repr.idents.last().unwrap())],
                    )),
                    modifier: match repr.modifier {
                        Some(Either::Left(span)) => Some(MaybeArbitrary::Named(&self.input[span])),
                        Some(Either::Right(arb)) => Some(MaybeArbitrary::Arbitrary(arb)),
                        None => None,
                    },
                    arbitrary: false,
                    important: repr.important,
                    negative: repr.negative,
                });
            }

            prev = Some(ident);
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct VariantRepr<'a> {
    idents: SmallVec<[Span; 2]>,
    arbitrary: Option<&'a str>,
    modifier: Option<MaybeArbitrary<'a>>,
}

impl<'a> CandidateParser<'a> {
    fn parse_variant_repr(&mut self) -> Option<VariantRepr<'a>> {
        let mut repr = VariantRepr::default();
        let mut modifier: Option<Either<Span, &str>> = None;
        let mut state = State::Initial;

        while let Some(token) = self.next_token().ok()? {
            debug!("token: {:?}, state: {:?}", token, state);
            let new_state = VariantTransformer::transform(&state, token).ok()?;

            match (token, state) {
                (Token::At, State::Initial) => {
                    repr.idents.push(Span::new(0, 1));
                }
                (Token::Ident(span), State::Initial | State::AfterIdent) => {
                    repr.idents.push(span);
                }
                (Token::Arbitrary(arb), State::Initial | State::AfterIdent) => {
                    repr.arbitrary = Some(arb);
                }
                (Token::Ident(span), State::AfterSlash) if modifier.is_none() => {
                    modifier = Some(Either::Left(span));
                }
                (Token::Ident(span), State::AfterSlashIdent)
                    if modifier.is_some_and(|m| m.is_left()) =>
                {
                    let Some(Left(s)) = modifier else { return None };
                    modifier = Some(Left(s.to(&span)))
                }
                (Token::Arbitrary(arb), State::AfterSlash) if repr.modifier.is_none() => {
                    modifier = Some(Either::Right(arb));
                }
                (Token::Slash, State::AfterArbitrary | State::AfterIdent) => {}
                _ => return None,
            }
            state = new_state;
        }
        repr.modifier = modifier.map(|m| match m {
            Left(span) => MaybeArbitrary::Named(&self.input[span]),
            Right(arb) => MaybeArbitrary::Arbitrary(arb),
        });
        Some(repr)
    }

    pub fn parse_variant(&mut self, v: &VariantStorage) -> Option<VariantCandidate<'a>> {
        // try static match
        if let Some(variant) = v.get(self.input) {
            return (variant.kind == VariantKind::Static)
                .then(|| VariantCandidate::new(variant.clone(), self.input).into());
        }

        let repr = self.parse_variant_repr()?;

        // full arbitrary
        if let (Some(arb), true) = (repr.arbitrary, repr.idents.is_empty()) {
            return Some(VariantCandidate::new(Variant::new_static([arb]), arb).arbitrary());
        }

        let mut layers = smallvec![];
        let slice = &mut repr.idents.as_slice();
        while let Some((key, variant)) = find_key(&self.input, v, slice) {
            match variant.kind {
                VariantKind::Composable => {
                    layers.push(variant.take_composable()?.clone());
                }
                VariantKind::Dynamic => {
                    return VariantCandidate::new(variant, key)
                        .with_value(
                            (!slice.is_empty())
                                .then(|| MaybeArbitrary::Named(substr(self.input, slice)))
                                .or_else(|| repr.arbitrary.map(MaybeArbitrary::Arbitrary)),
                        )
                        .with_modifier(repr.modifier)
                        .with_layers(layers)
                        .into();
                }
                VariantKind::Static => {
                    // must exhausted
                    if !slice.is_empty() {
                        return None;
                    }
                    return VariantCandidate::new(variant, key)
                        .with_layers(layers)
                        .with_modifier(repr.modifier)
                        .into();
                }
            }
        }

        None
    }
}

fn substr<'a>(input: &'a str, span: &[Span]) -> &'a str {
    &input[span.first().unwrap().to(span.last().unwrap())]
}

fn find_key<'a>(
    input: &'a str,
    v: &VariantStorage,
    spans: &mut &[Span],
) -> Option<(&'a str, Variant)> {
    let first = spans.first()?;

    for (idx, i) in spans.iter().enumerate().rev() {
        let key = &input[first.to(i)];
        if let Some(variant) = v.get(key) {
            *spans = &mut &spans[(idx + 1)..];
            return Some((key, variant.clone()));
        }
    }

    None
}

#[cfg(test)]
mod tests {

    macro_rules! maybe_arb {
        (named $named:literal) => {
            Some($crate::common::MaybeArbitrary::Named($named))
        };
        (arb $arb:literal) => {
            Some($crate::common::MaybeArbitrary::Arbitrary($arb))
        };
        (None) => {
            None
        };
        () => {
            None
        };
    }

    macro_rules! get_neg {
        ($bool:literal) => {
            true
        };
        () => {
            false
        };
    }

    macro_rules! candidate {
        ( $key:literal: $value_ty:ident $value:literal $( / $mod_ty:ident $mod:literal )? $(, neg: $neg:literal)? $(, imp: $imp:literal)? ) => {

            crate::parsing::UtilityCandidate {
                key: $key,
                value: maybe_arb!($value_ty $value),
                modifier: maybe_arb!($($mod_ty $mod)?),
                arbitrary: false,
                important: get_neg!($($imp)?),
                negative: get_neg!($($neg)?),
            }
        };
        ( [ $key:literal: $value:literal ] $(, neg: $neg:literal)? $(, imp: $imp:literal)? ) => {
            crate::parsing::UtilityCandidate {
                key: $key,
                value: maybe_arb!(arb $value),
                modifier: None,
                arbitrary: true,
                important: get_neg!($($imp)?),
                negative: get_neg!($($neg)?),
            }
        };
    }

    macro_rules! test_group {
        (utility => $($fn_name:ident $input:expr => $expected:expr),* $(,)?) => {
            $(
                paste::item! {
                    #[test]
                    fn [< test_utility_ $fn_name >] () {
                        assert_eq!(run($input), Some($expected));
                    }
                }
            )*
        };
        (variant => $($fn_name:ident $input:expr => $expected:expr),* $(,)?) => {
            $(
                paste::item! {
                    #[test]
                    fn [< test_variant_ $fn_name >] () {
                        assert_eq!(run_variant($input), Some($expected.into()));
                    }
                }
            )*
        };
    }

    mod utility {
        use crate::{
            parsing::{candidate::CandidateParser, UtilityCandidate},
            preset::preset_tailwind,
            Context,
        };

        fn run(input: &str) -> Option<UtilityCandidate> {
            let mut ctx = Context::default();
            preset_tailwind(&mut ctx);
            let mut parser = CandidateParser::new(input);

            parser.parse_utility(&ctx.utilities)
        }

        test_group! { utility =>
            basic           "text-blue-500"     => candidate!("text": named "blue-500"),
            basic_important "text-blue-500!"    => candidate!("text": named "blue-500", imp: true),
            basic_w         "w-10"              => candidate!("w": named "10"),
            neg_w           "-w-10"             => candidate!("w": named "10", neg: true),
            imp_w           "!w-10"             => candidate!("w": named "10", imp: true),
            neg_imp_w       "-!w-10"            => candidate!("w": named "10", neg: true, imp: true),
            neg_imp_w2      "!-w-10"            => candidate!("w": named "10", neg: true, imp: true),
            neg_imp_w3      "-w-10!"            => candidate!("w": named "10", neg: true, imp: true),
            arb_w           "w-[10px]"          => candidate!("w": arb "10px"),
            arb_mod_w       "text-[10px]/100"   => candidate!("text": arb "10px" / named "100"),
            arb_arbmod_w    "text-[10px]/[100]" => candidate!("text": arb "10px" / arb "100"),
        }
    }

    mod variant {
        use arrowcss_css_macro::rule_list;
        use smol_str::SmolStr;

        use crate::{parsing::candidate::CandidateParser, preset::preset_tailwind, Context};

        fn run_variant(input: &str) -> Option<SmolStr> {
            let mut ctx = Context::default();
            preset_tailwind(&mut ctx);

            let mut parser = CandidateParser::new(input);

            parser
                .parse_variant(&ctx.variants)?
                .handle(rule_list!("&" {}))
                .as_single()?
                .selector
                .into()
        }

        test_group! { variant =>
            basic "hover" => "&:hover",
            functional "aria-hidden" => r#"&[aria-hidden="true"]"#,
            composable "group-hover" => "&:is(:where(.group):hover *)",
            arb "[:hover]" => "&:is(:hover)",
            arb2 "[&:hover]" => "&:hover",
            arb_at "[@supports(display:grid)]" => "@supports(display:grid)",
            // TODO: [@media(any-hover:hover){&:hover}]
            modifier "group-hover/name" => r#"&:is(:where(.group\/name):hover *)"#,
            modifier2 "group-hover/the-name" => r#"&:is(:where(.group\/the-name):hover *)"#,
            modifier3 "group-hover/[the-name]" => r#"&:is(:where(.group\/the-name):hover *)"#,
            basic_composable "has-hover" => r#"&:has(*:hover)"#,
            composable_funtional "has-aria-hidden" => "&:has(*[aria-hidden=\"true\"])",
            composable_funtional_arb "has-aria-[sort=ascending]" => "&:has(*[aria-sort=ascending])",
            composable_funtional_arb_modifier "has-group-hover/the-name" => "&:has(*:is(:where(.group\\/the-name):hover *))",
            composable_funtional_arb_modifier_arb "has-group-hover/[the-name]" => "&:has(*:is(:where(.group\\/the-name):hover *))",
            multi_composable "has-not-group-hover" => "&:has(*:not(*:is(:where(.group):hover *)))",
            multi_composable2 "has-not-group-hover/the-name" => r#"&:has(*:not(*:is(:where(.group\/the-name):hover *)))"#,
        }
    }
}
