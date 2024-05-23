use std::{
    fmt::Write,
    hash::{Hash, Hasher},
};

use cssparser::serialize_name;
use rustc_hash::{FxHashMap as HashMap, FxHasher};
use smallvec::SmallVec;
use smol_str::SmolStr;

use self::utilities::{StaticUtility, UtilityStorage, UtilityStorageImpl};
use crate::{
    common::{StrReplaceExt, StrSplitExt},
    css::rule::RuleList,
    ordering::OrderingKey,
    parsing::{
        candidate::CandidateParser, UtilityCandidate, VariantCandidate,
    },
    process::{Utility, UtilityApplyResult, UtilityGroup, Variant},
    theme::{Theme, ThemeValue},
};
#[macro_use]
pub mod macros;
pub mod utilities;

pub type VariantStorage = HashMap<SmolStr, Variant>;

#[derive(Default)]
pub struct Context {
    /// Storage for utilities
    pub utilities: UtilityStorageImpl,

    /// Storage for variants
    pub variants: VariantStorage,

    /// Theme values
    pub theme: Theme,
}

/// The result of a utility generation
#[derive(Debug, Clone)]
pub struct GenerateResult {
    /// The generated rule
    pub rule: RuleList,

    /// The grouping of the utility, if any
    pub group: Option<UtilityGroup>,

    /// The ordering key, will be [`OrderingKey::Disorder`] if not set
    pub ordering: OrderingKey,

    /// The variants in the utility, collect to sort them later
    pub variants: SmallVec<[u64; 2]>,

    pub additional_css: Option<RuleList>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            variants: HashMap::default(),
            utilities: UtilityStorageImpl::HashMap(Default::default()),
            theme: Theme::default(),
        }
    }

    /// Add a static utility
    ///
    ///
    /// # Examples
    ///
    /// ```
    /// use arrowcss::Context;
    /// use arrowcss::css::{Decl, DeclList, ToCssString};
    ///
    /// let mut ctx = Context::new();
    ///
    /// ctx.add_static("flex", DeclList::from([Decl::new("display", "flex")]));
    ///
    /// let res = ctx.generate("flex").unwrap();
    ///
    /// assert_eq!(res.rule.to_css_minified(), ".flex{display:flex;}");
    ///
    /// ```
    pub fn add_static(
        &mut self,
        key: impl Into<SmolStr>,
        value: impl Into<StaticUtility>,
    ) -> &Self {
        self.utilities.add_static(key.into(), value.into());
        self
    }

    /// Add a static variant
    ///
    ///
    /// # Examples
    ///
    /// ```
    /// use arrowcss::Context;
    /// use arrowcss::css::{Decl, DeclList, ToCssString};
    ///
    /// let mut ctx = Context::new();
    ///
    /// ctx.add_static("flex", DeclList::from([Decl::new("display", "flex")]));
    /// ctx.add_variant("hover", ["&:hover"]);
    ///
    /// let res = ctx.generate("hover:flex").unwrap();
    ///
    /// assert_eq!(res.rule.to_css_minified(), ".hover\\:flex:hover{display:flex;}");
    ///
    /// ```
    pub fn add_variant<T>(&mut self, key: impl Into<SmolStr>, matcher: T) -> &mut Self
    where
        T: IntoIterator,
        T::Item: Into<SmolStr>,
        T::IntoIter: ExactSizeIterator,
    {
        self.variants
            .insert(key.into(), Variant::new_static(matcher));
        self
    }

    pub fn add_variant_fn(
        &mut self,
        key: &str,
        func: fn(RuleList, &VariantCandidate) -> RuleList,
        nested: bool,
    ) -> &Self {
        self.variants
            .insert(key.into(), Variant::new_dynamic(func, nested));
        self
    }

    pub fn add_variant_composable(
        &mut self,
        key: &str,
        handler: fn(RuleList, &VariantCandidate) -> RuleList,
    ) -> &mut Self {
        self.variants
            .insert(key.into(), Variant::new_composable(handler));
        self
    }

    pub fn add_utility(&mut self, key: &str, utility: Utility) {
        self.utilities.add(key.into(), utility);
    }

    pub fn get_theme(&self, key: &str) -> Option<ThemeValue> {
        self.theme.get(key).cloned()
    }

    /// Try generate a utility with the given value
    pub fn generate(&self, value: &str) -> Option<GenerateResult> {
        let mut parts: SmallVec<[&str; 2]> = value.split_toplevel(b':')?;

        let utility = parts.pop()?;

        // Try static utility first
        if let Some(UtilityApplyResult {
            rule: node,
            ordering,
            group,
            ..
        }) = self
            .utilities
            .try_apply(UtilityCandidate::with_key(utility))
        {
            return Some(GenerateResult {
                group,
                rule: fill_selector_placeholder(utility, node.to_rule_list())?,
                ordering,
                variants: SmallVec::new(),
                additional_css: None,
            });
        }

        let utility_candidate = CandidateParser::new(utility).parse_utility(&self.utilities)?;

        let vs = parts
            .into_iter()
            .map(|v| CandidateParser::new(v).parse_variant(&self.variants))
            .collect::<Option<SmallVec<[_; 2]>>>()?;

        let variants = vs
            .iter()
            .map(|v| {
                let mut hasher = FxHasher::default();
                v.processor.hash(&mut hasher);
                hasher.finish()
            })
            .collect();

        let (nested, selector): (SmallVec<[_; 1]>, SmallVec<[_; 1]>) =
            vs.iter().partition(|v| v.processor.nested);

        let UtilityApplyResult {
            rule: node,
            ordering,
            group,
            additional_css,
        } = self.utilities.try_apply(utility_candidate)?;

        let mut node = selector
            .iter()
            .fold(node.to_rule_list(), |acc, cur| cur.handle(acc));

        node = fill_selector_placeholder(value, node)?;

        let node = nested.iter().fold(node, |acc, cur| cur.handle(acc));

        Some(GenerateResult {
            rule: node,
            ordering,
            group,
            variants,
            additional_css,
        })
    }
}

fn fill_selector_placeholder(value: &str, node: RuleList) -> Option<RuleList> {
    let mut writer = smol_str::Writer::new();
    writer.write_str(".").ok()?;
    serialize_name(value, &mut writer).ok()?;
    let w = SmolStr::from(writer);

    Some(node.modify_with(|s| s.replace_char('&', &w)))
}
