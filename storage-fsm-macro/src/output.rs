use std::collections::BTreeMap;
use proc_macro2::Ident;
use crate::parsing::StorageField;

pub struct StatePossibleTransitions {
    pub has_self_transition: bool,
    pub transition_required_storage: BTreeMap<Ident, Vec<StorageField>>,
}