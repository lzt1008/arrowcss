use either::Either::{self, Left, Right};
use enum_dispatch::enum_dispatch;
use rustc_hash::FxHashMap as HashMap;
use smol_str::SmolStr;

use crate::{
    config::StaticUtilityConfig,
    css::{DeclList, Rule},
    ordering::OrderingKey,
    parsing::UtilityCandidate,
    process::{Utility, UtilityGroup},
};

#[derive(Debug)]
pub struct StaticUtility {
    selector: Option<SmolStr>,
    decls: DeclList,
}

impl StaticUtility {
    pub fn new(selector: SmolStr, decls: DeclList) -> Self {
        Self {
            selector: Some(selector),
            decls,
        }
    }
}

impl From<DeclList> for StaticUtility {
    fn from(value: DeclList) -> Self {
        Self {
            selector: None,
            decls: value,
        }
    }
}

impl From<(SmolStr, DeclList)> for StaticUtility {
    fn from((selector, decl_list): (SmolStr, DeclList)) -> Self {
        Self {
            selector: Some(selector),
            decls: decl_list,
        }
    }
}

impl From<StaticUtilityConfig> for StaticUtility {
    fn from(value: StaticUtilityConfig) -> Self {
        match value {
            StaticUtilityConfig::DeclList(decl_list) => Self {
                selector: None,
                decls: decl_list.into_iter().collect(),
            },
            StaticUtilityConfig::WithSelector(value) => Self {
                selector: Some(value.0),
                decls: value.1.into_iter().collect(),
            },
        }
    }
}

pub type UtilityValue = Either<StaticUtility, Utility>;

#[enum_dispatch]
pub trait UtilityStorage: Sync + Send {
    fn add(&mut self, key: SmolStr, value: Utility);
    fn reserve(&mut self, additional: usize);
    fn add_static(&mut self, key: SmolStr, value: StaticUtility);
    fn get(&self, key: &str) -> Option<&Vec<UtilityValue>>;
    fn try_apply(
        &self,
        input: UtilityCandidate<'_>,
    ) -> Option<(Rule, OrderingKey, Option<UtilityGroup>)>;
}

#[enum_dispatch(UtilityStorage)]
pub enum UtilityStorageImpl {
    HashMap(HashMapUtilityStorage),
}

impl Default for UtilityStorageImpl {
    fn default() -> Self {
        Self::HashMap(HashMapUtilityStorage::default())
    }
}

#[derive(Default)]
pub struct HashMapUtilityStorage {
    utilities: HashMap<SmolStr, Vec<UtilityValue>>,
}

impl UtilityStorage for HashMapUtilityStorage {
    fn add(&mut self, key: SmolStr, value: Utility) {
        self.utilities
            .entry(key)
            .or_default()
            .push(Either::Right(value));
    }

    fn reserve(&mut self, additional: usize) {
        self.utilities.reserve(additional);
    }

    fn add_static(&mut self, key: SmolStr, value: StaticUtility) {
        self.utilities
            .entry(key)
            .or_default()
            .push(Either::Left(value));
    }

    fn get(&self, key: &str) -> Option<&Vec<UtilityValue>> {
        self.utilities.get(key)
    }

    fn try_apply(
        &self,
        candidate: UtilityCandidate<'_>,
    ) -> Option<(Rule, OrderingKey, Option<UtilityGroup>)> {
        self.get(candidate.key)?.iter().find_map(|rule| match rule {
            Left(value) => Some((
                Rule::new_with_decls(
                    value.selector.as_ref().map(|s| s.as_str()).unwrap_or("&"),
                    value.decls.0.clone(),
                ),
                OrderingKey::Disorder,
                None,
            )),
            Right(handler) => handler.apply_to(candidate),
        })
    }
}
