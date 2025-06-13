use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::{Debug, Formatter};
use std::mem;
use std::ops::Deref;
use convert_case::{Case, Casing};
use proc_macro2::{Delimiter, Ident, Span};
use quote::{format_ident, ToTokens};
use syn::{braced, Attribute, FieldsNamed, Path, PathSegment, Token, Type, TypePath, Variant};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::SelfType;

pub struct StatePossibleTransitions {
    pub has_self_transition: bool,
    pub transition_required_fields: BTreeMap<Ident, BTreeSet<StorageField>>,
}

pub struct StateStorageFields {
    pub own_storage_fields: BTreeSet<StorageField>,
    pub transitions_storage_fields: BTreeSet<StorageField>,
}

#[derive(Clone)]
pub struct StorageField {
    pub name: Ident,
    pub ty: Type,
}

impl PartialOrd for StorageField {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialEq for StorageField {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for StorageField {
}

impl Ord for StorageField {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl StorageField {
    pub fn new(name: Ident, ty: Ident) -> Self {
        StorageField{
            name,
            ty: TypePath{
                path: Path {
                    leading_colon: None,
                    segments: Punctuated::from_iter([PathSegment{
                        ident: ty,
                        arguments: syn::PathArguments::None,
                    }].into_iter()),
                },
                qself: None,
            }.into()
        } 
    }
}

impl Debug for StorageField {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty.to_token_stream())
    }
}

#[derive(Clone)]
pub struct MiddlewareDefinition {
    pub name: Ident,
    pub fields: BTreeSet<StorageField>,
}


impl Debug for MiddlewareDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.name)?;
        if !self.fields.is_empty() {
            write!(f, "{{ {} }}", self.fields.iter().map(|f| format!("{:?}", f)).collect::<Vec<_>>().join(", "))?;
        }
        Ok(())
    }
}

fn fields_named_to_storage_fields(fields: &FieldsNamed) -> BTreeSet<StorageField> {
    fields.named.iter().map(|f| {
        let name = f.ident.clone().unwrap();
        let ty = f.ty.clone();
        StorageField { name, ty }
    }).collect()
}

impl Parse for MiddlewareDefinition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        let fields: FieldsNamed = input.parse()?;
        let fields = fields_named_to_storage_fields(&fields);

        Ok(MiddlewareDefinition {
            name,
            fields,
        })
    }
}

#[derive(Clone)]
pub struct MiddlewareModifier(pub Ident);

impl MiddlewareModifier {
    pub fn field_name(&self) -> Ident {
        // convert to snake case and append "_middleware"
        let snake_case_name = self.0.to_string().to_case(Case::Snake);
        format_ident!("{}_middleware", snake_case_name, span = self.0.span())
    }

    pub fn storage_type_name(&self) -> Ident {
        format_ident!("{}Middleware", self.0, span = self.0.span())
    }
}

impl MiddlewareDefinition {
    pub fn field_name(&self) -> Ident {
        // convert to snake case and append "_middleware"
        let snake_case_name = self.name.to_string().to_case(Case::Snake);
        format_ident!("{}_middleware", snake_case_name, span = self.name.span())
    }

    pub fn storage_type_name(&self) -> Ident {
        format_ident!("{}Middleware", self.name, span = self.name.span())
    }

    pub fn to_modifier(&self) -> MiddlewareModifier {
        MiddlewareModifier(self.name.clone())
    }
}
impl Debug for MiddlewareModifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "MiddlewareModifier({})", self.0)
    }
}
impl Deref for MiddlewareModifier {
    type Target = Ident;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Parse for MiddlewareModifier {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::middleware>()?;
        let name = input.parse::<Ident>()?;
        Ok(MiddlewareModifier(name))
    }
}

pub struct RawStateTransition {
    state: Ident,
    new_storage_fields: BTreeSet<StorageField>,
    dst_states: Vec<Ident>,
}

impl Parse for RawStateTransition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let state = input.parse::<Ident>()?;
        let new_storage_fields: Option<FieldsNamed> = input.parse().ok();
        let new_storage_fields = new_storage_fields.as_ref()
            .map(fields_named_to_storage_fields)
            .unwrap_or_default();

        input.parse::<Token![=>]>()?;
        let dst_states: Punctuated<Ident, Token![|]> = Punctuated::parse_separated_nonempty_with(input, |s| {
            let Ok(id) = s.parse::<Ident>() else {
                return s.parse::<SelfType>().map(|_| Ident::new("Self", s.span()));
            };

            Ok(id)
        })?;
        let dst_states = dst_states.into_iter().map(|id| if id == "Self" {state.clone()} else {id}).collect();

        Ok(RawStateTransition {
            state,
            new_storage_fields,
            dst_states,
        })
    }
}

pub enum StateTransitionTree {
    Group {
        state_transitions: Vec<StateTransitionTree>,
        additional_states: Vec<Ident>,
        middleware_modifier: Option<MiddlewareModifier>
    },
    StateTransition(RawStateTransition, bool), // transition, reset_storage attribute
    InlineStateTransition(RawStateTransition),
}

fn replace_middleware_storage_fields(transitions: &mut BTreeMap<Ident, StateTransition>, middlewares: impl Iterator<Item=MiddlewareModifier> + Clone) {
    for transition in transitions.values_mut() {
        let fields = mem::take(&mut transition.new_storage_fields);
        transition.new_storage_fields = fields.into_iter().map(|mut storage_field| {
            // try represent type as Ident
            if let Type::Path(type_path) = &mut storage_field.ty {
                if let Some(segment) = type_path.path.segments.last_mut() {
                    if let Some(middleware) = middlewares.clone().find(|m| m.0 == segment.ident) {
                        let storage_type_name = middleware.storage_type_name();
                        segment.ident = storage_type_name;
                    }
                }
            }
            storage_field
        }).collect();
    }
}
impl StateTransitionTree {
    fn collect_state_transitions(self, cur_additional_states: &Vec<Ident>,
                                     cur_middleware_modifiers: &Vec<MiddlewareModifier>) -> BTreeMap<Ident, StateTransition> {
        let mut res = BTreeMap::new();
        match self {
            StateTransitionTree::Group {
                state_transitions,
                mut additional_states,
                middleware_modifier
            } => {
                additional_states.extend_from_slice(cur_additional_states);
                let mut middleware_modifiers = cur_middleware_modifiers.clone();
                if let Some(modifier) = middleware_modifier {
                    middleware_modifiers.push(modifier);
                }
                for state_tt in state_transitions.into_iter() {
                    let new_states = state_tt.collect_state_transitions(&additional_states, &middleware_modifiers);
                    res.extend(new_states);
                }

            }
            StateTransitionTree::StateTransition(mut state, reset_storage) => {
                state.dst_states.extend_from_slice(&cur_additional_states);
                let transition = StateTransition {
                    new_storage_fields: state.new_storage_fields,
                    dst_states: state.dst_states,
                    is_inline: false,
                    is_reset_storage_state: reset_storage,
                    middlewares: cur_middleware_modifiers.to_vec(),
                    name: state.state,
                };
                res.insert(transition.name.clone(), transition);
            }
            StateTransitionTree::InlineStateTransition(mut state) => {
                state.dst_states.extend_from_slice(&cur_additional_states);
                let transition = StateTransition {
                    new_storage_fields: state.new_storage_fields,
                    dst_states: state.dst_states,
                    is_inline: true,
                    is_reset_storage_state: false,
                    middlewares: cur_middleware_modifiers.to_vec(),
                    name: state.state,
                };
                res.insert(transition.name.clone(), transition);
            }
        }

        res
    }
}

impl Parse for StateTransitionTree {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let reset_storage  = input.call(Attribute::parse_outer)
            .is_ok_and(|attr| attr.iter().any(|attr| attr.path().is_ident("reset_storage")));
        Ok(if input.parse::<kw::inline>().is_ok() {
            if reset_storage {
                return Err(input.error("#[reset_storage] cannot be used with inline transitions!"));
            }
            let transition = input.parse::<RawStateTransition>()?;
            StateTransitionTree::InlineStateTransition(transition)
        }
        else if input.cursor().group(Delimiter::Brace).is_some() || input.peek(kw::middleware) {
            // parse as group
            // 1. optional middleware
            let middleware_modifier = input.parse::<MiddlewareModifier>().ok();
            // 2. parse body as TT
            let body;
            braced!(body in input);
            let states: RootTransitionTree = body.parse()?;
            // 3. parse additional dst states
            let additional_states = if input.parse::<Token![=>]>().is_ok() {
                let additional_states: Punctuated<Ident, Token![|]> = input.call(Punctuated::parse_separated_nonempty)?;
                additional_states.into_iter().collect()
            }
            else {
                Vec::new()
            };

            Self::Group {
                additional_states,
                middleware_modifier,
                state_transitions: states.0,
            }
        }
        else {
            let transition = input.parse::<RawStateTransition>()?;
            StateTransitionTree::StateTransition(transition, reset_storage)
        }
        )
    }
}

pub struct RootTransitionTree(Vec<StateTransitionTree>);
impl Parse for RootTransitionTree {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut states = Vec::new();
        while !input.is_empty() {
            let new_state_tt: StateTransitionTree = input.parse()?;
            states.push(new_state_tt);

            let _ = input.parse::<Token![,]>();
        }
        Ok(RootTransitionTree(states))
    }
}

#[derive(Clone)]
pub struct StateTransition {
    pub name: Ident,
    pub new_storage_fields: BTreeSet<StorageField>,
    pub dst_states: Vec<Ident>,

    pub is_inline: bool,
    pub is_reset_storage_state: bool,
    pub middlewares: Vec<MiddlewareModifier>,
}

impl Debug for StateTransition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let storage_args = if !self.new_storage_fields.is_empty() {
            format!(" +{{ {} }}", self.new_storage_fields.iter().map(|f| format!("{:?}", f)).collect::<Vec<_>>().join(", "))
        } else {
            String::new()
        };
        let reset_storage = if self.is_reset_storage_state {
            " 0{}".to_string()
        }
        else {
            String::new()
        };
        let inline = if self.is_inline {
            " (inline)".to_string()
        } else {
            String::new()
        };
        let dst_states = self.dst_states.iter().map(|id| id.to_string()).collect::<Vec<_>>().join(", ");
        let middlewares = if self.middlewares.is_empty() {
            String::new()
        } else {
            format!(" with middlewares: {}", self.middlewares.iter().map(|m| m.to_token_stream().to_string()).collect::<Vec<_>>().join(", "))
        };
        write!(f, "{}{}{}{} => [{}]{}", self.name, inline, reset_storage, storage_args, dst_states, middlewares)?;

        Ok(())
    }
}

pub struct StateMachineMacroParsed {
    pub name: Ident,
    pub input: BTreeSet<StorageField>,
    pub output_name: Ident,
    pub initial_state: Variant,
    pub middlewares: Vec<MiddlewareDefinition>,
    pub transitions: BTreeMap<Ident, StateTransition>,
}

pub struct StateDescription {
    pub transitions: StatePossibleTransitions,
    pub storage_fields: StateStorageFields,
}

impl StateMachineMacroParsed {
    fn calculate_storage_island(&self, src_state: &Ident) -> BTreeMap<Ident, BTreeSet<StorageField>> {
        let mut res = BTreeMap::new();
        let mut path= BTreeSet::new();
        let mut visited = HashMap::new();

        struct Ctx<'a> {
            res: &'a mut BTreeMap<Ident, BTreeSet<StorageField>>,
            path: &'a mut BTreeSet<Ident>,
            visited: &'a mut HashMap<Ident, BTreeSet<Ident>>,
            transitions: &'a BTreeMap<Ident, StateTransition>,
        }
        let mut ctx = Ctx {
            res: &mut res,
            path: &mut path,
            visited: &mut visited,
            transitions: &self.transitions,
        };

        fn explore_state(state: &Ident, ctx: &mut Ctx<'_>) {
            let state_info = ctx.transitions.get(state).unwrap();
            ctx.path.insert(state.clone());
            for dst_state in state_info.dst_states.iter() {
                if ctx.path.contains(dst_state) || ctx.transitions.get(dst_state).unwrap().is_reset_storage_state {
                    // Skip cycles, ignore reset storage states
                    continue;
                }

                for path_state in ctx.path.iter() {
                    if ctx.visited.get(path_state).map_or(false, |visited_states| visited_states.contains(dst_state)) {
                        // we already visited this pair, skip it
                        continue;
                    }
                    else {
                        // <- custom code with access to new state and its path
                        let src_fields = &ctx.transitions.get(path_state).unwrap().new_storage_fields;
                        let dst_fields = ctx.res.entry(dst_state.clone()).or_default();
                        dst_fields.extend(src_fields.iter().cloned());
                        ctx.visited.entry(path_state.clone())
                            .or_default()
                            .insert(dst_state.clone());
                    }
                }

                explore_state(dst_state, ctx);
            }
            ctx.path.remove(state);
        }

        explore_state(src_state, &mut ctx);
        res
    }
    pub fn calculate_transitions(&self) -> BTreeMap<Ident, StateDescription> {
        let mut res = BTreeMap::new();

        let mut calculated_prev_states_own_storage: BTreeMap<Ident, BTreeSet<StorageField>> = BTreeMap::new();
        // calculate reset islands
        for (src_state, state_info) in self.transitions.iter().filter(|(_, info)| info.is_reset_storage_state) {
            let new_results: BTreeMap<Ident, BTreeSet<StorageField>> = self.calculate_storage_island(src_state);
            calculated_prev_states_own_storage.extend(new_results);
        }

        for (src_state, state_info) in &self.transitions {
            let state_def = self.transitions.get(&src_state).unwrap();
            let has_self_transition = state_def.dst_states.iter().any(|i| i == src_state);
            let transitions_desc = StatePossibleTransitions {
                has_self_transition,
                transition_required_fields: state_def.dst_states.iter().map(|i| {
                    (i.clone(), self.transitions.get(i).unwrap().new_storage_fields.clone())
                }).collect(),
            };
            
            let own_storage_fields = state_info.new_storage_fields.clone();
            let prev_states_storage = calculated_prev_states_own_storage.get(src_state).cloned().unwrap_or_default();
            let combined_state_storage: BTreeSet<_> = own_storage_fields.iter().chain(prev_states_storage.iter()).cloned().collect();
            
            let mut merged_transitions_storage_fields = BTreeMap::new();
            for dst_state in &state_def.dst_states {
                if let Some(transition_info) = self.transitions.get(dst_state) {
                    for storage_field in &transition_info.new_storage_fields {
                        if !combined_state_storage.iter().any(|s| s.name == storage_field.name) {
                            merged_transitions_storage_fields.insert(storage_field.name.clone(), storage_field.clone());
                        }
                    }
                }
            }
            let storage_fields_desc = StateStorageFields {
                own_storage_fields: combined_state_storage,
                transitions_storage_fields: BTreeSet::from_iter(merged_transitions_storage_fields.values().cloned()),
            };
            
            let state_desc = StateDescription {
                transitions: transitions_desc,
                storage_fields: storage_fields_desc,
            };
            res.insert(src_state.clone(), state_desc);
        }
        res
    }
}

mod kw {
    use syn::custom_keyword;
    custom_keyword!(name);
    custom_keyword!(input);
    custom_keyword!(output_name);
    custom_keyword!(states);
    custom_keyword!(middlewares);
    custom_keyword!(initial_state);

    custom_keyword!(inline);
    custom_keyword!(middleware);
}

impl Parse for  StateMachineMacroParsed {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut name = None;
        let mut sm_input = None;
        let mut output_name = None;
        let mut initial_state = None;
        let mut state_defs = None;
        let mut middlewares = None;
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(kw::name) {
                if name.is_some() {
                    return Err(input.error("duplicate state machine param: `name`"));
                }
                input.parse::<kw::name>()?;
                let _: Token![:] = input.parse()?;
                name = Some(input.parse::<Ident>()?);
            }
            else if lookahead.peek(kw::input) {
                if sm_input.is_some() {
                    return Err(input.error("duplicate state machine param: `input`"));
                }
                input.parse::<kw::input>()?;
                let _: Token![:] = input.parse()?;
                let storage_fields: FieldsNamed = input.parse()?;
                sm_input = Some(fields_named_to_storage_fields(&storage_fields));
            }
            else if lookahead.peek(kw::output_name) {
                if output_name.is_some() {
                    return Err(input.error("duplicate state machine param: `output_name`"));
                }
                input.parse::<kw::output_name>()?;
                let _: Token![:] = input.parse()?;
                output_name = Some(input.parse()?);
            }
            else if lookahead.peek(kw::initial_state) {
                if initial_state.is_some() {
                    return Err(input.error("duplicate state machine param: `initial_state`"));
                }
                input.parse::<kw::initial_state>()?;
                let _: Token![:] = input.parse()?;
                let initial_state_ident = input.parse::<Variant>()?;
                initial_state = Some(initial_state_ident);
            }
            else if lookahead.peek(kw::middlewares) {
                if middlewares.is_some() {
                    return Err(input.error("duplicate state machine param: `middlewares`"));
                }
                input.parse::<kw::middlewares>()?;
                let _: Token![:] = input.parse()?;

                let body;
                braced!(body in input);
                let middlewares_parsed: Punctuated<MiddlewareDefinition, Token![,]> = body.call(Punctuated::parse_terminated)?;

                middlewares = Some(middlewares_parsed.into_iter().collect());
            }
            else if lookahead.peek(kw::states) {
                if state_defs.is_some() {
                    return Err(input.error("duplicate state machine param: `states`"));
                }
                input.parse::<kw::states>()?;
                let _: Token![:] = input.parse()?;
                let body;
                braced!(body in input);
                let root_tt: RootTransitionTree = body.parse()?;

                // create stream parser for body
                state_defs = Some(StateTransitionTree::Group {
                    additional_states: Vec::new(),
                    state_transitions: root_tt.0,
                    middleware_modifier: None
                });
            }
            else {
                return Err(lookahead.error());
            }

            // optional comma
            let _ = input.parse::<Token![,]>();
        }
        let Some(name) = name else {
            return Err(input.error("`name` must present in state_machine! body!"));
        };
        let Some(sm_input) = sm_input else {
            return Err(input.error("`input` must present in state_machine! body!"));
        };
        let Some(output_name) = output_name else {
            return Err(input.error("`output_name` must present in state_machine! body!"));
        };
        let Some(initial_state) = initial_state else {
            return Err(input.error("`initial_state` must present in state_machine! body!"));
        };
        if middlewares.is_none() {
            // if middlewares are not specified, we use empty Vec
            middlewares = Some(Vec::new());
        };
        let middlewares = middlewares.unwrap();
        let Some(states) = state_defs else {
            return Err(input.error("`states` must present in state_machine! body!"));
        };

        let mut transitions = states.collect_state_transitions(&Vec::new(), &Vec::new());

        // replace middleware storage fields
        replace_middleware_storage_fields(&mut transitions, middlewares.iter().map(MiddlewareDefinition::to_modifier));

        Ok(StateMachineMacroParsed {
            name,
            input: sm_input,
            output_name,
            initial_state,
            middlewares,
            transitions,
        })
    }
}

