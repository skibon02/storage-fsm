mod macros;

use proc_macro::TokenStream;
use quote::ToTokens;

#[proc_macro]
pub fn state_machine(token_stream: TokenStream) -> TokenStream {
    macros::state_machine(token_stream.into()).to_token_stream().into()
}