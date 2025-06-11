mod macros;
mod parsing;
mod output;

use proc_macro::TokenStream;

#[proc_macro]
pub fn state_machine(token_stream: TokenStream) -> TokenStream {
    macros::state_machine(token_stream.into())
}