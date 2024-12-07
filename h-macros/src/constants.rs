use super::values::{AsTypedValue, TypedValue};
use proc_macro2::Span;

macro_rules! constants {
    ($path:expr, $span:expr, { $($name:path),* $(,)? }) => {
        match $path {
            $(stringify!($name) => $name.as_typed_value($span),)*
            _ => return None,
        }
    }
}

pub fn get_constant(path: &str, span: Span) -> Option<TypedValue> {
    Some(constants!(path, span, {
        i8::MIN, i8::MAX,
        i16::MIN, i16::MAX,
        i32::MIN, i32::MAX,
        i64::MIN, i64::MAX,
        i128::MIN, i128::MAX,
        u8::MIN, u8::MAX,
        u16::MIN, u16::MAX,
        u32::MIN, u32::MAX,
        u64::MIN, u64::MAX,
        u128::MIN, u128::MAX,
        char::MIN, char::MAX, char::REPLACEMENT_CHARACTER,
    }))
}
