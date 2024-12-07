mod constants;
mod parse;
mod types;
mod values;

use proc_macro::TokenStream;
use syn::{parse_macro_input, Expr};
use types::TypePtr;
use values::{evaluate_syn_expr, TypedValue, Value};

fn build_phf<'a>(context: &parse::Context, keys: impl Iterator<Item = &'a Expr>) {
    let mut key_type = if let Some(ref ty) = context.key_type {
        TypePtr::from_syn_type(ty)
    } else {
        TypePtr::Infer
    };

    let mut parsed_keys = Vec::new();
    for key in keys {
        let TypedValue { value, ty } = evaluate_syn_expr(key);
        let mut failed = false;
        key_type.unify_with(ty, &mut |e| {
            e.emit_inference();
            failed = true;
        });
        if failed {
            key_type = TypePtr::Inconsistent;
        }
        parsed_keys.push(value);
    }
    if key_type.has_inconsistencies() || parsed_keys.iter().any(Value::has_inconsistencies) {
        return;
    }

    // let phf = h::Phf::<Value>::from_keys(parsed_keys.iter());
}

#[proc_macro_error2::proc_macro_error]
#[proc_macro]
pub fn map(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as parse::WithContext<parse::MapArm>);

    build_phf(&input.context, input.elements.iter().map(|arm| &arm.key));

    // panic!("{input:?}");

    "42".parse().unwrap()
}
