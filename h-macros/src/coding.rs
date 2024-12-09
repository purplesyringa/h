#![allow(
    clippy::shadow_unrelated,
    reason = "https://github.com/rust-lang/rust-clippy/issues/11827"
)]

use super::{
    types::{IntegerTypeNode, TypeNode, TypePtr},
    values::Value,
};
use byteorder::{NativeEndian as NE, WriteBytesExt};
use proc_macro_error2::emit_error;

struct State {
    data: Vec<u8>,
    had_error: bool,
}

impl State {
    fn encode_value_to(&mut self, value: &Value, ty: &TypePtr) {
        match (value, ty.unwrap_ref()) {
            (
                Value::Integer {
                    absolute_value,
                    negated,
                    span,
                },
                TypeNode::Integer(integer_type_node),
            ) => {
                let as_u128 = if *negated {
                    absolute_value.wrapping_neg()
                } else {
                    *absolute_value
                };

                let integer_type_node = integer_type_node.unwrap_ref();
                let fits = match integer_type_node.range() {
                    Ok(range) =>
                    {
                        #[allow(clippy::cast_sign_loss, reason = "intended")]
                        if *negated {
                            as_u128 >= *range.start() as u128
                        } else {
                            as_u128 <= *range.end() as u128
                        }
                    }
                    Err(max) => {
                        if *negated {
                            emit_error!(span, "cannot apply unary operator `-` to type `{}`", ty);
                            self.had_error = true;
                            return;
                        }
                        as_u128 <= max
                    }
                };
                if !fits {
                    emit_error!(span, "too large integer (doesn't fit in `{}`)", ty);
                    self.had_error = true;
                    return;
                }

                #[allow(clippy::cast_possible_truncation, reason = "checked to not occur")]
                #[allow(clippy::cast_possible_wrap, reason = "intended")]
                match integer_type_node {
                    IntegerTypeNode::U8 => self.data.write_u8(as_u128 as u8),
                    IntegerTypeNode::U16 => self.data.write_u16::<NE>(as_u128 as u16),
                    IntegerTypeNode::U32 => self.data.write_u32::<NE>(as_u128 as u32),
                    IntegerTypeNode::U64 => self.data.write_u64::<NE>(as_u128 as u64),
                    IntegerTypeNode::U128 => self.data.write_u128::<NE>(as_u128),
                    IntegerTypeNode::I8 => self.data.write_i8(as_u128 as i8),
                    IntegerTypeNode::I16 => self.data.write_i16::<NE>(as_u128 as i16),
                    IntegerTypeNode::I32 => self.data.write_i32::<NE>(as_u128 as i32),
                    IntegerTypeNode::I64 => self.data.write_i64::<NE>(as_u128 as i64),
                    IntegerTypeNode::I128 => self.data.write_i128::<NE>(as_u128 as i128),
                }
                .unwrap();
            }

            (Value::Bool(value), TypeNode::Bool) => self.data.write_u8((*value).into()).unwrap(),

            (Value::Char(value), TypeNode::Char) => {
                self.data.write_u32::<NE>(*value as u32).unwrap();
            }

            (Value::Reference(value), TypeNode::Reference(ty)) => self.encode_value_to(value, ty),

            (Value::ArrayOrSlice(elems), TypeNode::ArrayOrSlice(ty)) => {
                self.data.write_u64::<NE>(elems.len() as u64).unwrap();
                for elem in elems {
                    self.encode_value_to(elem, ty);
                }
            }

            (Value::Tuple(elems), TypeNode::Tuple(tys)) => {
                for (elem, ty) in elems.iter().zip(tys) {
                    self.encode_value_to(elem, ty);
                }
            }

            (Value::Str(value), TypeNode::Str) => {
                self.data.write_u64::<NE>(value.len() as u64).unwrap();
                self.data.extend_from_slice(value.as_bytes());
            }

            _ => panic!("mismatched value and type"),
        }
    }
}

pub fn encode_value(value: &Value, ty: &TypePtr) -> (Vec<u8>, bool) {
    let mut state = State {
        data: Vec::new(),
        had_error: false,
    };
    state.encode_value_to(value, ty);
    (state.data, !state.had_error)
}
