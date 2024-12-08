use h::codegen::{CodeGenerator, Codegen};
use proc_macro2::TokenStream;

pub struct PassThrough(pub TokenStream);

impl Codegen for PassThrough {
    fn generate_piece(&self, _gen: &mut CodeGenerator) -> TokenStream {
        self.0.clone()
    }
}
