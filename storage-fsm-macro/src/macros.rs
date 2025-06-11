use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};
use syn::{parse_macro_input, Path, PathSegment, Type, TypePath};
use syn::punctuated::Punctuated;
use crate::parsing::{StateMachineMacroParsed, StatePossibleTransitions, StorageField};

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

    let state_machine_name = state_machine.name;
    let state_machine_output = state_machine.output_name;

    // Parsed state transitions
    let state_defs = state_machine.states.collect_state_transitions(&Vec::new(), &Vec::new());
    let inline_states = state_defs.values().filter_map(|s| {
        if s.is_inline {
            Some(s.name.clone())
        } else {
            None
        }
    });
    let non_inline_states = state_defs.values().filter_map(|s| {
        if !s.is_inline {
            Some(s.name.clone())
        } else {
            None
        }
    });
    let non_inline_states2 = non_inline_states.clone();
    let all_states = state_defs.keys();

    let state_enum = format_ident!("{}State", state_machine_name);
    let inline_state_enum = format_ident!("{}InlineState", state_machine_name);

    let inline_states_storage_name = inline_states.clone().map(|s| {
        format_ident!("{}Storage", s)
    });
    let non_inline_states_storage_name = non_inline_states.clone().map(|s| {
        format_ident!("{}Storage", s)
    });
    let non_inline_states_storage_names2 = non_inline_states_storage_name.clone();
    let all_states_storage_name = state_defs.keys().map(|s| {
        format_ident!("{}Storage", s)
    });
    let all_states_storage_name2 = all_states_storage_name.clone();

    let private_mod_name = format_ident!("__{}_private", state_machine_name);

    let handler_type = format_ident!("{}Handler", state_machine_name);
    let cmd_enum = format_ident!("{}Cmd", state_machine_name);

    let context_type = format_ident!("{}Context", state_machine_name);

    let state_machine_input_pub_fields = pub_fields_from_storage_fields(&state_machine.input);
    let state_machine_input_args = args_from_storage_fields(&state_machine.input);
    let state_machine_input_field_vals = names_from_storage_fields(&state_machine.input);

    let middlewares_storage = state_machine.middlewares.iter().map(|m| {
        let storage_name = format_ident!("{}Middleware", m.name);
        storage_name
    });
    let middlewares_storage2 = middlewares_storage.clone();
    let middlewares_storage3 = middlewares_storage.clone();
    let middlewares_storage_struct = state_machine.middlewares.iter().map(|m| {
        let storage_name = format_ident!("{}Middleware", m.name);
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

    let prepare_state_transitions = |transitions: &StatePossibleTransitions, transition_res: TokenStream| {
        let state_transitions = transitions.transition_required_storage.iter().map(|(state, fields)| {
            let transition_fn_name = format_ident!("transition_{}", state);
            let args = args_from_storage_fields(fields);
            let fields_names = fields.iter().map(|f| {
                let field_name = &f.name;
                quote! {
                    #field_name
                }
            });
            quote! {
                pub fn #transition_fn_name(self, #args) -> #handler_result {
                    #(
                        *self.storage.#fields_names = #fields_names;
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
        // example
        let transitions = StatePossibleTransitions {
            has_self_transition: true,
            transition_required_storage: state_defs.get(&s).unwrap().dst_states.iter().map(|i| {
                (i.clone(), vec![StorageField{
                    name: format_ident!("v1"),
                    ty: TypePath{
                        path: Path {
                            leading_colon: None,
                            segments: Punctuated::from_iter([PathSegment{
                                ident: Ident::new("u8", Span::call_site()),
                                arguments: syn::PathArguments::None,
                            }].into_iter()),
                        },
                        qself: None,
                    }.into()
                }])
            }).collect(),
        };

        prepare_state_transitions(&transitions, quote! {
            HandlerResult::InlineTransition(#inline_state_enum :: #s)
        })
    });

    let non_inline_states_transitions = non_inline_states.clone().map(|s| {
        // example
        let transitions = StatePossibleTransitions {
            has_self_transition: true,
            transition_required_storage: state_defs.get(&s).unwrap().dst_states.iter().map(|i| {
                (i.clone(), vec![StorageField{
                    name: format_ident!("v2"),
                    ty: TypePath{
                        path: Path {
                            leading_colon: None,
                            segments: Punctuated::from_iter([PathSegment{
                                ident: Ident::new("bool", Span::call_site()),
                                arguments: syn::PathArguments::None,
                            }].into_iter()),
                        },
                        qself: None,
                    }.into()
                }])
            }).collect(),
        };

        prepare_state_transitions(&transitions, quote! {
            HandlerResult::Transition(#state_enum :: #s)
        })
    });

    quote!{
        // 1. State enum
        #[derive(Copy, Clone, Debug)]
        pub enum #state_enum {
            #(#non_inline_states),*
        }

        // 2. Inline state enum
        pub enum #inline_state_enum {
            #(#inline_states),*
        }

        // 3. Middleware storage structs

        // 4. State machine struct
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

        // 5. private types and functions
        mod #private_mod_name {
            use storage_fsm::HandlerResult;
            use super::*;

            // 5.1 Storage enum
            #[derive(Default)]
            pub struct #storage_struct {
                // Define storage fields here
            }

            // 5.2 General types
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

            // 5.3 State storages and transitions for all states
            #(
                pub struct #all_states_storage_name {
                    // Define storage fields here
                }
                impl #all_states_storage_name {
                    pub fn from_storage(storage: &mut #storage_struct) -> Self {
                        // initialize refs to storage fields
                        Self {

                        }
                    }
                }
            )*
            #(
                // transitions for non-inline states
                impl #context_type<#non_inline_states_storage_name> {
                    // transitions
                    #non_inline_states_transitions
                }
            )*
            #(
                // transitions for inline states
                impl #inline_states_storage_name {
                    // transitions
                    #inline_states_transitions
                }
            )*

            // 5.4 Middleware storages
            #(
                #middlewares_storage_struct
                impl #context_type<#middlewares_storage> {
                    // transitions
                    pub fn stay() -> #handler_result {
                        #handler_result {
                            result: HandlerResult::Stay,
                            parser_output: None,
                        }
                    }
                }
            )*

            // 5.5 Commands enum
            pub enum #cmd_enum {
                // Commands
                #(
                    #all_states(#context_type<#all_states_storage_name2>),
                )*


                // Middlewares
                #(
                    #middlewares_storage2(#context_type<#middlewares_storage3>),
                )*
            }

            // 5.6 State machine impl
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
                            #state_enum :: #non_inline_states2 => {
                                let res = (self.handler)(
                                    #cmd_enum :: #non_inline_states2(#context_type {
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