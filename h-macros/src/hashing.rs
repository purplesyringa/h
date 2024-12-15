#![expect(
    clippy::shadow_unrelated,
    reason = "https://github.com/rust-lang/rust-clippy/issues/11827"
)]

use super::types::{IntegerTypeNode, TypeNode, TypePtr};
use byteorder::{NativeEndian as NE, ReadBytesExt};
use core::hash::Hasher;
use h::{
    codegen::{CodeGenerator, Codegen},
    hash::PortableHash,
};
use proc_macro2::TokenStream;

// Primitives need to be hashed differently from other values. Chaos ensues.

fn hash<H: Hasher>(data: &mut &[u8], ty: &TypePtr, state: &mut H) {
    match ty.unwrap_ref() {
        TypeNode::Integer(integer_type_node) => match integer_type_node.unwrap_ref() {
            IntegerTypeNode::U8 => data.read_u8().unwrap().hash(state),
            IntegerTypeNode::U16 => data.read_u16::<NE>().unwrap().hash(state),
            IntegerTypeNode::U32 => data.read_u32::<NE>().unwrap().hash(state),
            IntegerTypeNode::U64 => data.read_u64::<NE>().unwrap().hash(state),
            IntegerTypeNode::U128 => data.read_u128::<NE>().unwrap().hash(state),
            IntegerTypeNode::I8 => data.read_i8().unwrap().hash(state),
            IntegerTypeNode::I16 => data.read_i16::<NE>().unwrap().hash(state),
            IntegerTypeNode::I32 => data.read_i32::<NE>().unwrap().hash(state),
            IntegerTypeNode::I64 => data.read_i64::<NE>().unwrap().hash(state),
            IntegerTypeNode::I128 => data.read_i128::<NE>().unwrap().hash(state),
        },

        TypeNode::Bool => (data.read_u8().unwrap() != 0).hash(state),

        TypeNode::Char => char::from_u32(data.read_u32::<NE>().unwrap())
            .unwrap()
            .hash(state),

        TypeNode::Reference(ty) => hash(data, ty, state),

        TypeNode::ArrayOrSlice(ty) => {
            let count: usize = data.read_u64::<NE>().unwrap().try_into().unwrap();
            for _ in 0..count {
                hash(data, ty, state);
            }
        }

        TypeNode::Tuple(tys) => {
            for ty in tys {
                hash(data, ty, state);
            }
        }

        TypeNode::Str => {
            let len = data.read_u64::<NE>().unwrap().try_into().unwrap();
            core::str::from_utf8(&data[..len]).unwrap().hash(state);
            *data = &data[len..];
        }

        TypeNode::Never => unreachable!(),
    }
}

struct ComplexValue<'a> {
    ty: &'a TypePtr,
    data: Vec<u8>,
    pub tokens: TokenStream,
}

impl PortableHash for ComplexValue<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut cursor = &*self.data;
        hash(&mut cursor, self.ty, state);
    }
}

impl Codegen for ComplexValue<'_> {
    fn generate_piece(&self, _gen: &mut CodeGenerator) -> TokenStream {
        self.tokens.clone()
    }
}

pub trait Callback {
    type Output;

    fn call_once<Key: PortableHash + Codegen>(
        self,
        keys: impl ExactSizeIterator<Item = Key> + Clone,
    ) -> Self::Output;
}

/// # Safety
///
/// `keys` must be encoded variants of `T`, where `T` is a primitive.
unsafe fn with_primitive<T: PortableHash + Codegen, Cb: Callback>(
    keys: impl ExactSizeIterator<Item = (Vec<u8>, TokenStream)> + Clone,
    cb: Cb,
    add_reference: bool,
) -> Cb::Output {
    let keys = keys.map(|key| key.0.as_ptr().cast::<T>().read_unaligned());
    if add_reference {
        // Allocations. Eugh.
        let keys: Vec<T> = keys.collect();
        cb.call_once(keys.iter())
    } else {
        cb.call_once(keys)
    }
}

fn with_integer_keys<Cb: Callback>(
    keys: impl ExactSizeIterator<Item = (Vec<u8>, TokenStream)> + Clone,
    key_type: IntegerTypeNode,
    cb: Cb,
    add_reference: bool,
) -> Cb::Output {
    match key_type {
        IntegerTypeNode::U8 => unsafe { with_primitive::<u8, Cb>(keys, cb, add_reference) },
        IntegerTypeNode::U16 => unsafe { with_primitive::<u16, Cb>(keys, cb, add_reference) },
        IntegerTypeNode::U32 => unsafe { with_primitive::<u32, Cb>(keys, cb, add_reference) },
        IntegerTypeNode::U64 => unsafe { with_primitive::<u64, Cb>(keys, cb, add_reference) },
        IntegerTypeNode::U128 => unsafe { with_primitive::<u128, Cb>(keys, cb, add_reference) },
        IntegerTypeNode::I8 => unsafe { with_primitive::<i8, Cb>(keys, cb, add_reference) },
        IntegerTypeNode::I16 => unsafe { with_primitive::<i16, Cb>(keys, cb, add_reference) },
        IntegerTypeNode::I32 => unsafe { with_primitive::<i32, Cb>(keys, cb, add_reference) },
        IntegerTypeNode::I64 => unsafe { with_primitive::<i64, Cb>(keys, cb, add_reference) },
        IntegerTypeNode::I128 => unsafe { with_primitive::<i128, Cb>(keys, cb, add_reference) },
    }
}

pub fn with_hashable_keys<Cb: Callback>(
    keys: impl ExactSizeIterator<Item = (Vec<u8>, TokenStream)> + Clone,
    key_type: &TypePtr,
    cb: Cb,
) -> Cb::Output {
    if let TypeNode::Integer(integer_type_node) = key_type.unwrap_ref() {
        return with_integer_keys(keys, *integer_type_node.unwrap_ref(), cb, false);
    }

    if let TypeNode::Reference(target) = key_type.unwrap_ref() {
        if let TypeNode::Integer(integer_type_node) = target.unwrap_ref() {
            return with_integer_keys(keys, *integer_type_node.unwrap_ref(), cb, true);
        }
    }

    cb.call_once(keys.map(|key| ComplexValue {
        ty: key_type,
        data: key.0,
        tokens: key.1,
    }))
}
