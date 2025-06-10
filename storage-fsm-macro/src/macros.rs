use proc_macro2::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use crate::parsing::StateMachineMacroParsed;

pub fn state_machine(input: TokenStream) -> proc_macro::TokenStream {
    let input: proc_macro::TokenStream = input.into();
    let state_machine = parse_macro_input!(input as StateMachineMacroParsed);

    let state_machine_name = state_machine.name;
    let state_machine_input = state_machine.input.named;
    let state_machine_output_name = state_machine.output_name;
    
    // let state_defs = state_machine.calculate_transitions();

    quote!{
        struct #state_machine_name {
            
        }
        
        impl #state_machine_name {
            pub fn new() -> Self {
                Self {
                    
                }
            }
            
            pub fn advance(&mut self, #state_machine_input) -> Option<#state_machine_output_name> {
                None
            }
        }
    }.into()
}