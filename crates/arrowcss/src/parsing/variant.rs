use smallvec::{smallvec, SmallVec};

use crate::{
    common::MaybeArbitrary,
    css::rule::RuleList,
    process::{ComposableHandler, Variant, VariantHandlerExt, VariantOrdering},
};

#[derive(Debug, Clone)]
pub struct VariantCandidate<'a> {
    pub key: &'a str,
    pub value: Option<MaybeArbitrary<'a>>,
    pub modifier: Option<MaybeArbitrary<'a>>,
    // fully arbitrary, e.g. [@media(min-width:300px)] [&:nth-child(3)]
    pub arbitrary: bool,
    pub processor: Variant,
    pub layers: SmallVec<[ComposableHandler; 1]>,
    pub ordering_key: Option<VariantOrdering>,
}

impl<'a> VariantCandidate<'a> {
    pub fn new(processor: Variant, key: &'a str) -> Self {
        Self {
            key,
            value: None,
            modifier: None,
            arbitrary: false,
            ordering_key: processor.ordering,
            processor,
            layers: smallvec![],
        }
    }

    pub fn with_value(mut self, value: Option<MaybeArbitrary<'a>>) -> Self {
        self.value = value;
        self
    }

    pub fn with_modifier(mut self, modifier: Option<MaybeArbitrary<'a>>) -> Self {
        self.modifier = modifier;
        self
    }

    pub fn with_layers(mut self, layers: SmallVec<[ComposableHandler; 1]>) -> Self {
        self.layers = layers;
        self
    }

    pub fn arbitrary(mut self) -> Self {
        self.arbitrary = true;
        self
    }

    pub fn handle(&self, rule: RuleList) -> RuleList {
        let rule = self.processor.handle(self, rule);
        self.layers
            .iter()
            .rev()
            .fold(rule, |rule, handler| handler.handle(self, rule))
    }
}
