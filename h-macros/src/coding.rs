//! Serialization for [`Value`]s.
//!
//! The goal here is to compensate for lack of dynamically constructed `struct` types, not to make
//! a portable serialization protocol. All this does is transform a recursive [`Value`] object into
//! a memory-efficient representation that can be hashed without jumping all over the heap.

use super::{
    types::{IntegerTypeNode, TypeNode, TypePtr},
    values::Value,
};
use byteorder::{NativeEndian as NE, WriteBytesExt};
use proc_macro_error2::emit_error;
use std::io::Write;

/// Serialization state.
///
/// This is a `Result<Vec<u8>, ()>` that blackholes the writes if [`Err`] is stored.
struct State(Result<Vec<u8>, ()>);

impl Write for State {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match &mut self.0 {
            Ok(data) => data.write(buf),
            Err(()) => Ok(buf.len()),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl State {
    /// Push the serialized representation of `value` as `ty` into the state.
    ///
    /// This method emits errors directly with [`emit_error`] and sets the internal state to
    /// [`Err`] instead of returning a [`Result`] to ensure multiple independent errors can be
    /// emitted.
    #[expect(
        clippy::missing_panics_doc,
        reason = "writing to `Vec<u8>` is infallible"
    )]
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
                        #[expect(clippy::cast_sign_loss, reason = "intended")]
                        if *negated {
                            as_u128 >= *range.start() as u128
                        } else {
                            as_u128 <= *range.end() as u128
                        }
                    }
                    Err(max) => {
                        if *negated {
                            emit_error!(span, "cannot apply unary operator `-` to type `{}`", ty);
                            self.0 = Err(());
                            return;
                        }
                        as_u128 <= max
                    }
                };
                if !fits {
                    emit_error!(span, "too large integer (doesn't fit in `{}`)", ty);
                    self.0 = Err(());
                    return;
                }

                #[expect(clippy::cast_possible_truncation, reason = "checked to not occur")]
                #[expect(clippy::cast_possible_wrap, reason = "intended")]
                match integer_type_node {
                    IntegerTypeNode::U8 => self.write_u8(as_u128 as u8),
                    IntegerTypeNode::U16 => self.write_u16::<NE>(as_u128 as u16),
                    IntegerTypeNode::U32 => self.write_u32::<NE>(as_u128 as u32),
                    IntegerTypeNode::U64 => self.write_u64::<NE>(as_u128 as u64),
                    IntegerTypeNode::U128 => self.write_u128::<NE>(as_u128),
                    IntegerTypeNode::I8 => self.write_i8(as_u128 as i8),
                    IntegerTypeNode::I16 => self.write_i16::<NE>(as_u128 as i16),
                    IntegerTypeNode::I32 => self.write_i32::<NE>(as_u128 as i32),
                    IntegerTypeNode::I64 => self.write_i64::<NE>(as_u128 as i64),
                    IntegerTypeNode::I128 => self.write_i128::<NE>(as_u128 as i128),
                }
                .unwrap();
            }

            (Value::Bool(value), TypeNode::Bool) => self.write_u8((*value).into()).unwrap(),

            (Value::Char(value), TypeNode::Char) => {
                self.write_u32::<NE>(*value as u32).unwrap();
            }

            (Value::Reference(value), TypeNode::Reference(ty)) => self.encode_value_to(value, ty),

            (Value::ArrayOrSlice(elems), TypeNode::ArrayOrSlice(ty)) => {
                self.write_u64::<NE>(elems.len() as u64).unwrap();
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
                self.write_u64::<NE>(value.len() as u64).unwrap();
                self.write_all(value.as_bytes()).unwrap();
            }

            _ => panic!("mismatched value and type"),
        }
    }
}

/// Serialize `value` as `ty` into a binary format.
///
/// # Errors
///
/// Emits an error and returns `Err(())` if the value is not a valid instance of `ty` despite
/// inferring to `ty`. This can occur if the value is an integer that does not fit in the inferred
/// numeric type.
pub fn encode_value(value: &Value, ty: &TypePtr) -> Result<Vec<u8>, ()> {
    let mut state = State(Ok(Vec::new()));
    state.encode_value_to(value, ty);
    state.0
}
