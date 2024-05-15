use std::fmt::Debug;

use smallvec::{smallvec, SmallVec};
use smol_str::{format_smolstr, SmolStr};

use super::{MetaData, ValuePreprocessor, ValueRepr};
use crate::{
    css::{rule::RuleList, Decl, Rule, ToCssString},
    ordering::OrderingKey,
    parsing::UtilityCandidate,
    theme::ThemeValue,
};

#[rustfmt::skip]
pub trait RuleMatchingFn: Fn(MetaData, SmolStr) -> Rule + Send + Sync + 'static {}

#[rustfmt::skip]
impl<T: Fn(MetaData, SmolStr) -> Rule + Send + Sync + 'static> RuleMatchingFn for T {}

#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
pub struct UtilityHandler(Box<dyn RuleMatchingFn>);

impl Debug for UtilityHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("UtilityHandler { ")?;

        // Call the function, simply get the css
        let rule = self.0(MetaData::modifier("$2"), SmolStr::new("$1"));
        write!(f, "{}", rule.to_css_minified())?;

        f.write_str(" }")
    }
}

impl UtilityHandler {
    pub fn new(handler: impl RuleMatchingFn + 'static) -> Self {
        Self(Box::new(handler))
    }

    pub fn call(&self, meta: MetaData, value: SmolStr) -> Rule {
        (self.0)(meta, value)
    }
}

#[derive(Debug)]
pub struct Utility {
    pub handler: UtilityHandler,

    pub supports_negative: bool,

    pub supports_fraction: bool,

    pub value_repr: ValueRepr,

    pub modifier: Option<ValueRepr>,

    /// This will be use as generated Rule selector
    /// default: '&'
    pub wrapper: Option<SmolStr>,

    /// Additional css which append to stylesheet root
    /// useful when utilities like `animate-spin`
    pub additional_css: Option<RuleList>,

    pub ordering_key: Option<OrderingKey>,

    pub group: Option<UtilityGroup>,
}

// TODO: make this configurable
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
pub enum UtilityGroup {
    Transform,
    Filter,
    BackdropFilter,
}

impl UtilityGroup {
    pub fn as_decls(&self) -> SmallVec<[Decl; 2]> {
        match self {
            Self::Filter => smallvec![Decl::new(
                "filter", "var(--tw-blur,) var(--tw-brightness,) var(--tw-contrast,) var(--tw-grayscale,) var(--tw-hue-rotate,) var(--tw-invert,) var(--tw-saturate,) var(--tw-sepia,) var(--tw-drop-shadow,)"

            )],
            Self::BackdropFilter => smallvec![
                Decl::new("-webkit-backdrop-filter", "var(--tw-backdrop-blur,) var(--tw-backdrop-brightness,) var(--tw-backdrop-contrast,) var(--tw-backdrop-grayscale,) var(--tw-backdrop-hue-rotate,) var(--tw-backdrop-invert,) var(--tw-backdrop-opacity,) var(--tw-backdrop-saturate,) var(--tw-backdrop-sepia,)"),
                Decl::new("backdrop-filter", "var(--tw-backdrop-blur,) var(--tw-backdrop-brightness,) var(--tw-backdrop-contrast,) var(--tw-backdrop-grayscale,) var(--tw-backdrop-hue-rotate,) var(--tw-backdrop-invert,) var(--tw-backdrop-opacity,) var(--tw-backdrop-saturate,) var(--tw-backdrop-sepia,)")
            ],
            Self::Transform => smallvec![Decl::new(
                "transform", "var(--tw-rotate-x) var(--tw-rotate-y) var(--tw-rotate-z) var(--tw-skew-x) var(--tw-skew-y)"
            )],
        }
    }
}

impl ValuePreprocessor for Utility {
    fn validate(&self, value: &str) -> bool {
        self.value_repr.validate(value)
    }

    fn allowed_values(&self) -> Option<&ThemeValue> {
        self.value_repr.allowed_values()
    }
}

impl<F: RuleMatchingFn + 'static> From<F> for Utility {
    fn from(handler: F) -> Self {
        Utility::new(handler)
    }
}

impl Utility {
    pub fn new<F: RuleMatchingFn + 'static>(handler: F) -> Self {
        Self {
            handler: UtilityHandler(Box::new(handler)),
            supports_negative: false,
            supports_fraction: false,
            value_repr: ValueRepr::default(),
            modifier: None,
            wrapper: None,
            additional_css: None,
            ordering_key: None,
            group: None,
        }
    }


    pub fn apply_to(
        &self,
        candidate: UtilityCandidate<'_>,
    ) -> Option<(Rule, OrderingKey, Option<UtilityGroup>)> {
        if !self.supports_negative && candidate.negative {
            return None;
        }

        let mut process_result = self.preprocess(candidate.value)?;
        let mut meta = MetaData::from_candidate(&candidate);

        // handing modifier
        if let (Some(modifier), Some(candidate)) = (&self.modifier, candidate.modifier) {
            meta.modifier = modifier.preprocess(Some(candidate));
        }

        if self.supports_fraction {
            if let Some(fraction) = candidate.take_fraction() {
                process_result = format_smolstr!("calc({} * 100%)", fraction);
            }
        }

        if candidate.negative {
            process_result = format_smolstr!("calc({} * -1)", process_result);
        }

        let mut node = self.handler.call(meta, process_result);

        if let Some(wrapper) = &self.wrapper {
            node.selector.clone_from(wrapper);
        }

        Some((
            node,
            self.ordering_key.unwrap_or(OrderingKey::Disorder),
            self.group,
        ))
    }
}
