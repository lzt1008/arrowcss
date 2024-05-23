use std::fmt::Debug;

use tracing::debug;

use super::candidate::Token;

pub trait StateTransformer {
    fn transform(state: &State, token: Token) -> Result<State, ()>;
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
    /// After Slash Ident
    ///
    /// Accepts: Ident
    AfterSlashIdent,
    /// Done
    Eof,
}

pub struct UtilityTransformer;

impl StateTransformer for UtilityTransformer {
    fn transform(state: &State, token: Token) -> Result<State, ()> {
        let res = match (&state, token) {
            (State::Initial, Token::Bang) => Ok(State::Initial),
            (State::Initial, Token::Minus) => Ok(State::Initial),
            (State::Initial, Token::Ident(_)) => Ok(State::AfterIdent),
            (State::Initial, Token::Arbitrary(_)) => Ok(State::AfterArbitrary),
            (State::AfterIdent, Token::Ident(_)) => Ok(State::AfterIdent),
            (State::AfterIdent, Token::Arbitrary(_)) => Ok(State::AfterArbitrary),
            (State::AfterIdent, Token::Slash) => Ok(State::AfterSlash),
            (State::AfterIdent, Token::Bang) => Ok(State::Eof),
            (State::AfterArbitrary, Token::Bang) => Ok(State::Eof),
            (State::AfterArbitrary, Token::Slash) => Ok(State::AfterSlash),
            (State::AfterSlash, Token::Ident(_)) => Ok(State::Eof),
            (State::AfterSlashIdent, Token::Ident(_)) => Ok(State::AfterSlashIdent),
            (State::AfterSlash, Token::Arbitrary(_)) => Ok(State::Eof),
            _ => Err(()),
        };
        debug!("transform: {:?} to {:?} with token {:?}", state, res, token);
        res
    }
}

pub struct VariantTransformer;

impl StateTransformer for VariantTransformer {
    fn transform(state: &State, token: Token) -> Result<State, ()> {
        let res = match (&state, token) {
            (State::Initial, Token::Ident(_)) => Ok(State::AfterIdent),
            (State::Initial, Token::At) => Ok(State::AfterIdent),
            (State::Initial, Token::Arbitrary(_)) => Ok(State::AfterArbitrary),
            (State::AfterIdent, Token::Ident(_)) => Ok(State::AfterIdent),
            (State::AfterIdent, Token::Arbitrary(_)) => Ok(State::AfterArbitrary),
            (State::AfterIdent, Token::Slash) => Ok(State::AfterSlash),
            (State::AfterArbitrary, Token::Slash) => Ok(State::AfterSlash),
            (State::AfterSlash, Token::Ident(_)) => Ok(State::AfterSlashIdent),
            (State::AfterSlashIdent, Token::Ident(_)) => Ok(State::AfterSlashIdent),
            (State::AfterSlash, Token::Arbitrary(_)) => Ok(State::Eof),
            _ => Err(()),
        };
        debug!("transform: {:?} to {:?} with token {:?}", state, res, token);
        res
    }
}
