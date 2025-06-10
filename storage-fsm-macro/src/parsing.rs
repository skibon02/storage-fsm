use proc_macro2::{Delimiter, Ident, TokenStream, TokenTree};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{Attribute, FieldsNamed, Token};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::SelfType;

#[derive(Clone)]
struct MiddlewareModifier{
    name: Ident,
    args: FieldsNamed,
}

impl Parse for MiddlewareModifier {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::middleware>()?;
        let name = input.parse()?;
        let args = input.parse()?;

        Ok(MiddlewareModifier{
            name,
            args
        })
    }
}

pub struct RawStateTransition {
    state: Ident,
    args: Option<FieldsNamed>,
    dst_states: Vec<Ident>,
}

impl Parse for RawStateTransition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let state = input.parse::<Ident>()?;
        let args = input.parse().ok();
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
            args,
            dst_states,
        })
    }
}

enum StateTransitionTree {
    Group {
        state_transitions: Vec<StateTransitionTree>,
        additional_states: Vec<Ident>,
        middleware_modifier: Option<MiddlewareModifier>
    },
    StateTransition(RawStateTransition, bool), // transition, reset_storage attribute
    InlineStateTransition(RawStateTransition),
}

impl StateTransitionTree {
    fn collect_state_transitions(self, cur_additional_states: &Vec<Ident>,
                                 cur_middleware_modifiers: &Vec<MiddlewareModifier>) -> Vec<StateTransition> {
        let mut res = Vec::new();
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
                    res.extend_from_slice(&new_states);
                }

            }
            StateTransitionTree::StateTransition(mut state, reset_storage) => {
                state.dst_states.extend_from_slice(&cur_additional_states);
                let transition = StateTransition {
                    args: state.args,
                    dst_states: state.dst_states,
                    is_inline: false,
                    is_reset_storage_state: reset_storage,
                    middlewares: cur_middleware_modifiers.to_vec(),
                    state: state.state,
                };
                res.push(transition);
            }
            StateTransitionTree::InlineStateTransition(mut state) => {
                state.dst_states.extend_from_slice(&cur_additional_states);
                let transition = StateTransition {
                    args: state.args,
                    dst_states: state.dst_states,
                    is_inline: true,
                    is_reset_storage_state: false,
                    middlewares: cur_middleware_modifiers.to_vec(),
                    state: state.state,
                };
                res.push(transition);
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
            let body = input.step(|cursor| {
                if let Some((TokenTree::Group(group), next)) = cursor.token_tree() {
                    if group.delimiter() != Delimiter::Brace {
                        return Err(input.error("expected {...}"));
                    };

                    return Ok((group.stream(), next));
                }
                else {
                    return Err(cursor.error("expected {...}"));
                }
            })?;
            let states = syn::parse2::<RootTransitionTree>(body)?.0;
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
                state_transitions: states,
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
    state: Ident,
    args: Option<FieldsNamed>,
    dst_states: Vec<Ident>,

    is_inline: bool,
    is_reset_storage_state: bool,
    middlewares: Vec<MiddlewareModifier>,
}

impl ToTokens for StateTransition {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let dst_states = self.dst_states.iter().map(|id| quote! { #id }).collect::<Vec<_>>();
        let middlewares = self.middlewares.iter().map(|modif| {
            let name = &modif.name;
            let args = &modif.args;
            quote! { MiddlewareModifier { name: #name, args: #args } }
        }).collect::<Vec<_>>();
        tokens.append_all(quote! {
            StateTransition {
                state: #self.state,
                args: #self.args,
                is_inline: #self.is_inline,
                is_reset_storage_state: #self.is_reset_storage_state,
                dst_states: vec![#(#dst_states),*],
                middlewares: vec![#(#middlewares),*],
            }
        })
    }
}

pub struct StateMachineMacroParsed {
    pub name: Ident,
    pub input: FieldsNamed,
    pub output_name: Ident,
    pub initial_state: Ident,
    states: StateTransitionTree
}

impl StateMachineMacroParsed {
    pub fn calculate_transitions(&self) {
    }
}

mod kw {
    use syn::custom_keyword;
    custom_keyword!(name);
    custom_keyword!(input);
    custom_keyword!(output_name);
    custom_keyword!(states);
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
                sm_input = Some(input.parse()?);
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
                let initial_state_ident = input.parse::<Ident>()?;
                initial_state = Some(initial_state_ident);
            }
            else if lookahead.peek(kw::states) {
                if state_defs.is_some() {
                    return Err(input.error("duplicate state machine param: `states`"));
                }
                input.parse::<kw::states>()?;
                let _: Token![:] = input.parse()?;
                let body = input.step(|cursor| {
                    if let Some((TokenTree::Group(group), next)) = cursor.token_tree() {
                        if group.delimiter() != Delimiter::Brace {
                            return Err(input.error("expected {...}"));
                        };

                        return Ok((group.stream(), next));
                    }
                    else {
                        return Err(cursor.error("expected {...}"));
                    }
                })?;
                let root_tt: RootTransitionTree = syn::parse2(body)?;

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
        let Some(states) = state_defs else {
            return Err(input.error("`states` must present in state_machine! body!"));
        };
        Ok(StateMachineMacroParsed {
            name,
            input: sm_input,
            output_name,
            initial_state,
            states
        })
    }
}

