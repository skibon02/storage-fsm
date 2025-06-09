use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

pub fn state_machine(input: TokenStream) -> impl ToTokens {
    quote!{
        struct ParserState {
            
        }
        
        impl ParserState {
            pub fn new() -> Self {
                Self {
                    
                }
            }
            
            pub fn advance(&mut self, byte: u8) -> Option<ParserStateOutput> {
                None
            }
        }
    }

}