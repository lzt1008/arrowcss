use smallvec::{smallvec, SmallVec};
use smol_str::SmolStr;

use crate::{
    css::{rule::RuleList, Rule},
    parsing::VariantCandidate,
};

#[rustfmt::skip]
pub trait VariantMatchingFn: Fn(RuleList) -> Option<RuleList> + Sync + Send {}

#[rustfmt::skip]
impl<T: Fn(RuleList) -> Option<RuleList> + Sync + Send> VariantMatchingFn for T {}

pub trait VariantHandlerExt {
    fn handle(&self, candidate: VariantCandidate<'_>, rule: RuleList) -> RuleList;
}

#[derive(Debug, Clone)]
pub enum VariantKind {
    Arbitrary,
    Static,
    Dynamic,
    Composable,
}

#[derive(Debug, Clone)]
pub enum VariantHandler {
    Static(StaticHandler),
    Dynamic(DynamicHandler),
    Composable(ComposableHandler),
}

impl VariantHandler {
    pub fn take_composable(self) -> Option<ComposableHandler> {
        match self {
            Self::Composable(handler) => Some(handler),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub handler: VariantHandler,
    pub composable: bool,
    pub kind: VariantKind,
    pub ordering: Option<VariantOrdering>,
}

impl PartialEq for Variant {
    fn eq(&self, other: &Self) -> bool {
        self.ordering == other.ordering
    }
}

impl Eq for Variant {}

impl PartialOrd for Variant {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Variant {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.ordering.cmp(&other.ordering)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum VariantOrdering {
    Length(i32),
}

impl VariantOrdering {
    pub fn from_px(s: &str) -> Self {
        Self::Length(s.strip_suffix("px").unwrap().parse().unwrap())
    }
}

impl Variant {
    pub fn new_static(
        matcher: impl IntoIterator<Item: Into<SmolStr>, IntoIter: ExactSizeIterator>,
    ) -> Self {
        Self {
            handler: VariantHandler::Static(StaticHandler::new(matcher)),
            composable: true,
            kind: VariantKind::Static,
            ordering: None,
        }
    }

    pub fn new_composable(handler: fn(RuleList, VariantCandidate) -> RuleList) -> Self {
        Self {
            handler: VariantHandler::Composable(ComposableHandler::new(handler)),
            composable: true,
            kind: VariantKind::Composable,
            ordering: None,
        }
    }

    pub fn new_dynamic(handler: fn(RuleList, VariantCandidate) -> RuleList) -> Self {
        Self {
            handler: VariantHandler::Dynamic(DynamicHandler::new(handler)),
            composable: true,
            kind: VariantKind::Dynamic,
            ordering: None,
        }
    }

    pub fn with_ordering(self, ordering: VariantOrdering) -> Self {
        Self {
            ordering: Some(ordering),
            ..self
        }
    }

    pub fn process(&self, candidate: VariantCandidate<'_>, rule: RuleList) -> RuleList {
        match &self.handler {
            VariantHandler::Static(handler) => handler.handle(candidate, rule),
            VariantHandler::Dynamic(handler) => handler.handle(candidate, rule),
            VariantHandler::Composable(handler) => handler.handle(candidate, rule),
        }
    }

    pub fn take_composable(&self) -> Option<&ComposableHandler> {
        match &self.handler {
            VariantHandler::Composable(handler) => Some(handler),
            _ => None,
        }
    }
}

impl VariantHandlerExt for Variant {
    fn handle(&self, candidate: VariantCandidate<'_>, rule: RuleList) -> RuleList {
        self.process(candidate, rule)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StaticHandler {
    // for single rule
    Selector(SmolStr),
    // for single rule
    PseudoElement(SmolStr),
    // for multiple rules
    Nested(SmolStr),
    // for multiple rules
    Duplicate(SmallVec<[SmolStr; 2]>),
}

impl StaticHandler {
    pub fn new(
        matcher: impl IntoIterator<Item: Into<SmolStr>, IntoIter: ExactSizeIterator>,
    ) -> Self {
        let mut iter = matcher.into_iter();
        let is_duplicate = iter.len() > 1;
        if !is_duplicate {
            let matcher = iter.next().unwrap().into();
            match matcher.chars().next() {
                Some('&') => {
                    if matcher.starts_with("&::") {
                        Self::PseudoElement(matcher)
                    } else {
                        Self::Selector(matcher)
                    }
                }
                Some('@') => Self::Nested(matcher),
                _ => panic!("Invalid matcher: {}", matcher),
            }
        } else {
            Self::new_duplicate(iter)
        }
    }

    pub fn new_duplicate(matcher: impl IntoIterator<Item: Into<SmolStr>>) -> Self {
        Self::Duplicate(matcher.into_iter().map(Into::into).collect())
    }
}

impl VariantHandlerExt for StaticHandler {
    fn handle(&self, _candidate: VariantCandidate<'_>, rules: RuleList) -> RuleList {
        match self {
            Self::Selector(a) | Self::PseudoElement(a) => rules
                .into_iter()
                .map(|rule| rule.modify_with(|selector| selector.replace('&', a)))
                .collect(),
            Self::Nested(a) => RuleList::new(Rule {
                selector: a.clone(),
                decls: smallvec![],
                rules,
            }),
            Self::Duplicate(list) => {
                list.iter()
                    .flat_map(move |a| {
                        rules.clone().into_iter().map(|rule| {
                            rule.modify_with(|selector| selector.replace('&', a))
                        })
                    })
                    .collect()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct DynamicHandler {
    pub handler: fn(RuleList, VariantCandidate) -> RuleList,
    pub composable: bool,
}

impl VariantHandlerExt for DynamicHandler {
    fn handle(&self, candidate: VariantCandidate<'_>, rule: RuleList) -> RuleList {
        (self.handler)(rule, candidate)
    }
}

impl DynamicHandler {
    pub fn new(handler: fn(RuleList, VariantCandidate) -> RuleList) -> Self {
        Self {
            handler,
            composable: true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ComposableHandler {
    pub handler: fn(RuleList, VariantCandidate) -> RuleList,
    pub composable: bool,
}

impl ComposableHandler {
    pub fn new(handler: fn(RuleList, VariantCandidate) -> RuleList) -> Self {
        Self {
            handler,
            composable: true,
        }
    }

    pub fn composable(self) -> Self {
        Self {
            composable: true,
            ..self
        }
    }
}

impl VariantHandlerExt for ComposableHandler {
    fn handle(&self, candidate: VariantCandidate<'_>, rule: RuleList) -> RuleList {
        (self.handler)(rule, candidate)
    }
}

#[cfg(test)]
mod tests {
    use arrowcss_css_macro::css;
    use smallvec::smallvec;
    use smol_str::format_smolstr;

    use super::{DynamicHandler, VariantHandlerExt};
    use crate::{
        context::Context,
        css::{rule::RuleList, Decl, Rule},
        parsing::VariantParser,
    };

    #[test]
    fn test_variant_process() {
        let mut ctx = Context::default();
        ctx.add_variant("hover", ["&:hover"]);
        ctx.add_variant("active", ["&:active"]);

        let candidates = vec![
            VariantParser::new("hover").parse(&ctx).unwrap(),
            VariantParser::new("active").parse(&ctx).unwrap(),
        ];

        let _input = css! {
            ".flex" {
                "display": "flex";
            }
        };

        let selector = RuleList::new(Rule {
            selector: "&".into(),
            rules: RuleList::default(),
            decls: smallvec![Decl {
                name: "display".into(),
                value: "flex".into(),
            }],
        });

        let _res = candidates
            .into_iter()
            .map(|candidate| {
                let processor = ctx.variants.get(candidate.key).unwrap();
                (processor, candidate)
            })
            .fold(selector, |acc, (processor, candidate)| {
                processor.process(candidate, acc)
            });

        // let res = res
        //     .into_iter()
        //     .map(|res| Rule {
        //         selector: res.selector.replace("&", ".flex"),
        //         nodes: res.nodes,
        //     })
        //     .collect::<Vec<_>>();

        // println!("{input:?}");
        // println!("res: {:#?}", res);
        // expect: ".flex:hover"
    }

    #[test]
    fn test_dynamic_process() {
        let mut ctx = Context::default();
        ctx.add_variant("hover", ["&:hover"]);
        ctx.add_variant("active", ["&:active"]);

        let candidate = VariantParser::new("hover").parse(&ctx).unwrap();
        let input = css! {
            ".flex" {
                "display": "flex";
            }
        }
        .to_rule_list();
        // @media (hover: hover) and (pointer: fine) | &:hover
        let variant = DynamicHandler::new(|rule, _| {
            let hovered = rule
                .into_iter()
                .map(|rule| rule.modify_with(|s| format_smolstr!("{}:hover", s)))
                .collect();
            Rule {
                selector: "@media (hover: hover) and (pointer: fine)".into(),
                decls: smallvec![],
                rules: hovered,
            }
            .to_rule_list()
        });

        let res = variant.handle(candidate, input);

        println!("{:#?}", res);
    }
}
