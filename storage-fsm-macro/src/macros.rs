use std::collections::BTreeMap;
use convert_case::{Case, Casing};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{parse_macro_input};
use crate::parsing::{StateMachineMacroParsed, StatePossibleTransitions, StateStorageFields, StorageField};

fn pub_fields_from_storage_fields(fields: &Vec<StorageField>) -> TokenStream {
    let fields = fields.iter().map(|f| {
        let field_name = &f.name;
        let field_type = &f.ty;
        quote! {
            pub #field_name: #field_type
        }
    });
    quote! {
        #(#fields),*
    }
}

fn args_from_storage_fields(fields: &Vec<StorageField>) -> TokenStream {
    let fields = fields.iter().map(|f| {
        let field_name = &f.name;
        let field_type = &f.ty;
        quote! {
            #field_name: #field_type
        }
    });
    quote! {
        #(#fields),*
    }
}

fn names_from_storage_fields(fields: &Vec<StorageField>) -> TokenStream {
    let fields = fields.iter().map(|f| {
        let field_name = &f.name;
        quote! {
            #field_name
        }
    });
    quote! {
        #(#fields),*
    }
}

pub fn state_machine(input: TokenStream) -> proc_macro::TokenStream {
    let input: proc_macro::TokenStream = input.into();
    let state_machine = parse_macro_input!(input as StateMachineMacroParsed);

    let state_machine_name = state_machine.name.clone();
    let state_machine_output = state_machine.output_name.clone();

    // Parsed state transitions
    let state_defs = &state_machine.transitions;
    let inline_states = state_defs.values().filter_map(|s| {
        if s.is_inline {
            Some(s.name.clone())
        } else {
            None
        }
    });
    let inline_states2 = inline_states.clone();
    let non_inline_states = state_defs.values().filter_map(|s| {
        if !s.is_inline {
            Some(s.name.clone())
        } else {
            None
        }
    });
    let non_inline_states2 = non_inline_states.clone();
    let non_inline_states3 = non_inline_states.clone();
    let all_states = state_defs.keys();

    let state_enum = format_ident!("{}State", state_machine_name);
    let inline_state_enum = format_ident!("{}InlineState", state_machine_name);

    let inline_states_storage_name = inline_states.clone().map(|s| {
        format_ident!("{}Storage", s)
    });
    let inline_states_storage_name2 = inline_states_storage_name.clone();
    let non_inline_states_storage_name = non_inline_states.clone().map(|s| {
        format_ident!("{}Storage", s)
    });
    let non_inline_states_storage_name2 = non_inline_states_storage_name.clone();
    let non_inline_states_storage_names2 = non_inline_states_storage_name.clone();
    let all_states_storage_name = state_defs.keys().map(|s| {
        format_ident!("{}Storage", s)
    });

    let private_mod_name = format_ident!("__{}_private", state_machine_name);

    let handler_type = format_ident!("{}Handler", state_machine_name);
    let cmd_enum = format_ident!("{}Cmd", state_machine_name);

    let context_type = format_ident!("{}Context", state_machine_name);

    // calculate transitions
    let transitions_calculated = state_machine.calculate_transitions();

    let state_machine_input_pub_fields = pub_fields_from_storage_fields(&state_machine.input);
    let state_machine_input_args = args_from_storage_fields(&state_machine.input);
    let state_machine_input_field_vals = names_from_storage_fields(&state_machine.input);

    let middlewares_storage = state_machine.middlewares.iter().map(|m| {
        m.storage_type_name()
    });
    let middlewares_storage2 = middlewares_storage.clone();
    let middlewares_storage3 = middlewares_storage.clone();
    let middlewares_storage_struct = state_machine.middlewares.iter().map(|m| {
        let storage_name = m.storage_type_name();
        let fields = pub_fields_from_storage_fields(&m.fields);
        quote!{
            #[derive(Default)]
            pub struct #storage_name {
                #fields
            }
        }
    });

    let handler_result = format_ident!("{}HandlerResult", state_machine_name);

    let storage_struct = format_ident!("{}Storage", state_machine_name);
    let initial_state = state_machine.initial_state;

    let prepare_state_transitions = |src_state: &Ident, transitions: &StatePossibleTransitions, inline: bool| {
        let state_transitions = transitions.transition_required_fields.iter().map(|(dst_state, fields)| {
            let transition_fn_name = format_ident!("transition_{}", dst_state.to_string().to_case(Case::Snake), span = dst_state.span());
            let args = args_from_storage_fields(fields);
            let fields_names = fields.iter().map(|f| {
                let field_name = &f.name;
                quote! {
                    #field_name
                }
            });

            let is_dst_inline = state_defs.get(dst_state).unwrap().is_inline;
            let transition_res = if is_dst_inline {
                quote! {
                    HandlerResult::InlineTransition(#inline_state_enum :: #dst_state)
                }
            } else {
                quote! {
                    HandlerResult::Transition(#state_enum :: #dst_state)
                }
            };

            let self_storage = if inline {
                quote! {
                    self
                }
            } else {
                quote! {
                    self.storage
                }
            };
            quote! {
                pub fn #transition_fn_name(self, #args) -> #handler_result {
                    #(
                        *#self_storage.#fields_names = #fields_names;
                    )*
                    #handler_result {
                        result: #transition_res,
                        parser_output: None,
                    }
                }
            }
        });
        let stay_fn = if transitions.has_self_transition {
            quote! {
                pub fn stay(self) -> #handler_result {
                    #handler_result {
                        result: HandlerResult::Stay,
                        parser_output: None,
                    }
                }
            }
        } else {
            quote! {}
        };

        quote! {
            #(#state_transitions)*
            #stay_fn
        }
    };

    // calculated stuff
    let inline_states_transitions = inline_states.clone().map(|s| {
        prepare_state_transitions(&s, &transitions_calculated.get(&s).unwrap().transitions, true)
    });

    let non_inline_states_transitions = non_inline_states.clone().map(|s| {
        prepare_state_transitions(&s, &transitions_calculated.get(&s).unwrap().transitions, false)
    });

    let all_states_storage_fields = all_states.clone().map(|s| {
        let fields = &transitions_calculated.get(&s).unwrap().storage_fields;
        let pub_fields = fields.own_storage_fields.iter().map(|f| {
            let field_name = &f.name;
            let field_type = &f.ty;
            quote! {
                pub #field_name: &'a mut #field_type
            }
        });
        let transition_fields = fields.transitions_storage_fields.iter().map(|f| {
            let field_name = &f.name;
            let field_type = &f.ty;
            quote! {
                #field_name: &'a mut #field_type
            }
        });
        let middleware_fields = state_machine.middlewares.iter()
            .filter(|mdw| {
                // only include middlewares that are used in this state
                state_defs.get(&s).unwrap().middlewares.iter().any(|m| m.0 == mdw.name)
            })
            .map(|m| {
                let field_name = m.field_name();
                let storage_name = m.storage_type_name();
                quote! {
                    pub #field_name: &'a mut #storage_name
                }
            });
        let phantom = if fields.transitions_storage_fields.is_empty() && fields.own_storage_fields.is_empty() && state_defs.get(&s).unwrap().middlewares.is_empty() {
            quote! {
                _phantom: std::marker::PhantomData<&'a ()>
            }
        } else {
            quote! {}
        };

        quote! {
            #(#pub_fields,)*
            #(#transition_fields,)*
            #(#middleware_fields,)*
            #phantom
        }
    });

    let all_states_storage_initializers = all_states.clone().map(|s| {
        let fields = &transitions_calculated.get(&s).unwrap().storage_fields;
        let pub_fields_names = fields.own_storage_fields.iter().map(|f| {
            let field_name = &f.name;
            quote! {
                #field_name
            }
        });
        let transition_fields_names = fields.transitions_storage_fields.iter().map(|f| {
            let field_name = &f.name;
            quote! {
                #field_name
            }
        });
        let middleware_fields_names = state_machine.middlewares.iter()
            .filter(|mdw| {
                // only include middlewares that are used in this state
                state_defs.get(&s).unwrap().middlewares.iter().any(|m| m.0 == mdw.name)
            })
            .map(|m| {
            let field_name = m.field_name();
            quote! {
                #field_name
            }
        });

        let phantom = if fields.transitions_storage_fields.is_empty() && fields.own_storage_fields.is_empty() && state_defs.get(&s).unwrap().middlewares.is_empty() {
            quote! {
                _phantom: std::marker::PhantomData
            }
        } else {
            quote! {}
        };

        quote! {
            Self {
                #(#pub_fields_names: &mut storage.#pub_fields_names,)*
                #(#transition_fields_names: &mut storage.#transition_fields_names,)*
                #(#middleware_fields_names: &mut storage.#middleware_fields_names,)*
                #phantom
            }
        }
    });

    // Ensure uniqueness
    let full_storage_struct_fields: BTreeMap<_, _> = state_defs.values().map(|s| {
        s.new_storage_fields.iter().map(|f| (f.name.clone(), f.clone()))
    }).flatten().collect();
    let full_storage_struct_fields = full_storage_struct_fields.values().map(|f| {
        let field_name = &f.name;
        let field_type = &f.ty;
        quote! {
            pub #field_name: #field_type
        }
    }).chain(
        state_machine.middlewares.iter().map(|m| {
            let storage_name = m.storage_type_name();
            let field_name = m.field_name();
            quote! {
                pub #field_name: #storage_name
            }
        })
    );

    quote!{
        /// 1. State enum
        use crate as _;

        #[derive(Copy, Clone, Debug)]
        pub enum #state_enum {
            #(#non_inline_states),*
        }

        /// 2. Inline state enum
        use crate as _;
        pub enum #inline_state_enum {
            #(#inline_states),*
        }

        /// 3. State machine struct
        use crate as _;

        type #handler_type = fn(#private_mod_name :: #cmd_enum) -> #private_mod_name :: #handler_result;
        struct #state_machine_name {
            state: #state_enum,
            handler: #handler_type,
            storage: #private_mod_name :: #storage_struct,
        }
        impl #state_machine_name {
            pub fn new(handler: #handler_type) -> Self {
                Self {
                    state: #state_enum :: #initial_state,
                    handler,
                    storage: #private_mod_name :: #storage_struct :: default(),
                }
            }
        }

        /// 4. private types and functions
        use crate as _;

        mod #private_mod_name {
            use storage_fsm::HandlerResult;
            use super::*;

            /// 4.1 Storage enum
            use crate as _;

            #[derive(Default)]
            pub struct #storage_struct {
                #(#full_storage_struct_fields),*
            }

            /// 4.2 General types
            use crate as _;

            pub struct #context_type<S> {
                #state_machine_input_pub_fields,
                pub storage: S
            }
            // keep fields private to control set of possible handler results
            pub struct #handler_result {
                result: HandlerResult<#state_enum, #inline_state_enum>,
                parser_output: Option<#state_machine_output>,
            }
            impl #handler_result {
               pub fn emit(mut self, output: #state_machine_output) -> Self {
                    self.parser_output= Some(output);
                    self
                }
            }

            /// 4.3 State storages for all states
            use crate as _;

            #(
                pub struct #all_states_storage_name<'a> {
                    #all_states_storage_fields
                }
                impl<'a> #all_states_storage_name<'a> {
                    pub fn from_storage(storage: &'a mut #storage_struct) -> Self {
                        // initialize refs to storage fields
                        #all_states_storage_initializers
                    }
                }
            )*

            /// 4.4 State transitions for all states
            #(
                // transitions for non-inline states
                impl #context_type<#non_inline_states_storage_name<'_>> {
                    // transitions
                    #non_inline_states_transitions
                }
            )*
            #(
                // transitions for inline states
                impl #inline_states_storage_name<'_> {
                    // transitions
                    #inline_states_transitions
                }
            )*

            /// 4.5 Middleware storage structs
            use crate as _;

            #(
                #middlewares_storage_struct
                impl #context_type<#middlewares_storage> {
                    // transitions
                    pub fn next() -> #handler_result {
                        #handler_result {
                            result: HandlerResult::Stay,
                            parser_output: None,
                        }
                    }
                }
            )*

            /// 4.6 Commands enum
            use crate as _;

            pub enum #cmd_enum<'a> {
                // non inline states
                #(
                    #non_inline_states2(#context_type<#non_inline_states_storage_name2<'a>>),
                )*

                // inline states
                #(
                    #inline_states2(#inline_states_storage_name2<'a>),
                )*

                // Middlewares
                #(
                    #middlewares_storage2(#context_type<#middlewares_storage3>),
                )*
            }

            /// 4.7 State machine impl
            use crate as _;

            impl #state_machine_name {
                fn handle_transition_result(&mut self, res: #handler_result, handle_output: &mut impl FnMut(#state_machine_output)) {
                    match res.result {
                        HandlerResult::Stay => {
                            // Stay in the current state
                        }
                        HandlerResult::Transition(state) => {
                            self.state = state;
                        }
                        HandlerResult::InlineTransition(state) => {
                            // Execute inline transition
                            unimplemented!("inline transition case");
                        }
                    }
                    if let Some(output) = res.parser_output {
                        handle_output(output);
                    }
                }
                pub fn advance(&mut self, #state_machine_input_args, mut handle_output: impl FnMut(#state_machine_output)) {
                    match self.state {
                        // handle non-inline states
                        #(
                            #state_enum :: #non_inline_states3 => {
                                let res = (self.handler)(
                                    #cmd_enum :: #non_inline_states3(#context_type {
                                        #state_machine_input_field_vals,
                                        storage: #non_inline_states_storage_names2::from_storage(&mut self.storage)
                                    })
                                );

                                self.handle_transition_result(res, &mut handle_output)
                            }
                        )*
                    }
                }
            }
        }
        use #private_mod_name::CsafeParserCmd;

    }.into()
}