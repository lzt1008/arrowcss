use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc};

use crate::{
    config::{ArrowConfig, Config},
    css::{CSSDecls, CssRuleList},
    rule::{InContextRule, Rule},
    theme::Theme,
    themes::theme,
    utils::{create_variant_fn, Matcher, VariantHandler},
};

pub trait VariantMatchingFn = Fn(CssRuleList) -> Option<CssRuleList> + 'static;

#[derive(Default, Clone)]
pub struct Context<'a> {
    pub static_rules: RefCell<HashMap<String, CSSDecls>>,
    pub rules: Arc<RefCell<HashMap<String, Vec<Arc<InContextRule<'a>>>>>>,

    pub variants: RefCell<HashMap<String, Rc<VariantHandler>>>,

    pub theme: RefCell<Rc<Theme>>,
    pub config: Config,
    pub tokens: RefCell<HashMap<String, Option<CssRuleList>>>,
}

pub struct ThemeValue<S: Into<String>> {
    pub key: S,
    pub decl_key: Vec<String>,
}

impl<S: Into<String>> ThemeValue<S> {
    pub fn new(key: S, decl_key: Vec<String>) -> Self {
        Self { key, decl_key }
    }
}

impl<'a> Context<'a> {
    pub fn new(config: ArrowConfig) -> Self {
        Self {
            tokens: HashMap::new().into(),
            static_rules: HashMap::new().into(),
            variants: HashMap::new().into(),
            rules: RefCell::new(HashMap::new()).into(),
            theme: Rc::clone(&Rc::new(theme().merge(config.theme))).into(),
            config: config.config,
        }
    }

    pub fn add_static<S>(&self, pair: (S, CSSDecls)) -> &Self
    where
        S: Into<String>,
    {
        self.static_rules.borrow_mut().insert(pair.0.into(), pair.1);
        self
    }

    pub fn add_theme_rule<S, T>(
        &self,
        _theme_key: T,
        values: Vec<ThemeValue<S>>,
    ) -> &Self
    where
        S: Into<String>,
        T: Into<String>,
    {
        // let theme_key: String = _theme_key.into();
        // for value in values {
        //     // TODO: use theme_key
        //     let theme_key = theme_key.clone();
        //     let theme_clone = Rc::clone(&self.theme.borrow());
        //     self.rules.borrow_mut().insert(
        //         value.key.into(),
        //         vec![Rc::new(move |input| {
        //             theme_clone
        //                 .get(theme_key.as_str())
        //                 .and_then(|theme| theme.get(input))
        //                 .map(|theme_val| {
        //                     theme_rule_handler(
        //                         value.decl_key.clone(),
        //                         theme_val.into(),
        //                     )
        //                 })
        //         })],
        //     );
        // }
        self
    }

    pub fn add_variant<'b, S, M>(&self, key: S, matcher: M) -> &Self
    where
        M: Matcher<'b>,
        S: Into<String>,
    {
        let key_clone: String = key.into();
        create_variant_fn(&key_clone, matcher).map(|func| {
            self.variants
                .borrow_mut()
                .insert(key_clone.clone(), func.into())
        });
        self
    }

    pub fn get_theme_value(&self, key: &str, value: &str) -> Option<String> {
        self.theme
            .borrow()
            .get(key)
            .and_then(|theme| theme.get(value))
            .map(|s| s.to_owned())
    }

    pub fn get_theme(&self, key: &str) -> Option<crate::theme::ThemeValue> {
        self.theme.borrow().get(key).map(Clone::clone)
    }
}

pub trait AddRule<'a, 'b> {
    fn add_rule<S: Into<String>>(&'b self, key: S, rule: Rule<'a>) -> &'b Self;
}

impl<'a, 'b> AddRule<'a, 'b> for Arc<Context<'a>> {
    fn add_rule<S: Into<String>>(&'b self, key: S, rule: Rule<'a>) -> &'b Self {
        let rule = rule.bind_context(self.clone());
        self.rules
            .borrow_mut()
            .entry(key.into())
            .or_insert_with(Vec::new)
            .push(rule.into());
        self
    }
}
fn theme_rule_handler(decl_keys: Vec<String>, value: String) -> CSSDecls {
    decl_keys
        .into_iter()
        .map(|decl_key| (decl_key, value.to_owned()))
        .collect::<CSSDecls>()
}

#[macro_export]
macro_rules! add_static {
  ($ctx:ident, {
    $($key:literal => {
      $($name:literal: $value:literal;)+
    })+
  }) => {
    $(
      $ctx.add_static(($key, CSSDecls::multi([
        $(($name, $value),)+
      ])));
    )+
  };
}

#[macro_export]
macro_rules! add_theme_rule {
  ($ctx:expr, {
    $($theme_key:literal => {
      $($key:literal => [$($decl_key:literal),+])+
    })+
  }) => {
    $(
      $ctx.add_theme_rule($theme_key, vec![
        $($crate::context::ThemeValue::new($key, vec![$($decl_key.into()),+]),)+
      ]);
    )+
  };
}
