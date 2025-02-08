//! Hashing serialized values with [`Hasher`]s.

use super::types::{IntegerTypeNode, TypeNode, TypePtr};
use byteorder::{NativeEndian as NE, ReadBytesExt};
use core::hash::Hasher;
use h::{
    codegen::{CodeGenerator, Codegen},
    hash::{PortableHash, Seed},
};
use proc_macro2::TokenStream;

/// Simulate `<T as Hash>::hash(state)` based on serialized `data` of type `ty`.
///
/// The serialized data must have been produced by
/// [`coding::encode_value`](super::coding::encode_value).
///
/// # Panics
///
/// Panics if the data could not be deserialized.
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

/// A typed serialized value.
///
/// Unlike [`TypedValue`](super::values::TypedValue), the type is deduplicated among multiple
/// [`HashableValue`] instances, and data is stored in a serialized format.
pub struct HashableValue<'a> {
    /// Type of the value.
    ty: &'a TypePtr,
    /// Serialized value bytes.
    data: Vec<u8>,
    /// An expression evaluating to the value.
    tokens: TokenStream,
}

impl<'a> HashableValue<'a> {
    /// Create a new [`HashableValue`] struct.
    pub const fn new(ty: &'a TypePtr, data: Vec<u8>, tokens: TokenStream) -> Self {
        Self { ty, data, tokens }
    }
}

impl PortableHash for HashableValue<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut cursor = &*self.data;
        hash(&mut cursor, self.ty, state);
    }

    fn hash_one(&self, seed: &Seed) -> u64 {
        // Certain types overload `hash_one`, and we can't exactly construct types dynamically, so
        // there's a bit of hard-coding here. Keep this in sync with `h_bare::hash`.
        let mut ty = self.ty;
        while let TypeNode::Reference(target) = ty.unwrap_ref() {
            ty = target;
        }

        if let TypeNode::Integer(integer_type_node) = self.ty.unwrap_ref() {
            let mut cursor = &*self.data;
            match integer_type_node.unwrap_ref() {
                IntegerTypeNode::U8 => cursor.read_u8().unwrap().hash_one(seed),
                IntegerTypeNode::U16 => cursor.read_u16::<NE>().unwrap().hash_one(seed),
                IntegerTypeNode::U32 => cursor.read_u32::<NE>().unwrap().hash_one(seed),
                IntegerTypeNode::U64 => cursor.read_u64::<NE>().unwrap().hash_one(seed),
                IntegerTypeNode::U128 => cursor.read_u128::<NE>().unwrap().hash_one(seed),
                IntegerTypeNode::I8 => cursor.read_i8().unwrap().hash_one(seed),
                IntegerTypeNode::I16 => cursor.read_i16::<NE>().unwrap().hash_one(seed),
                IntegerTypeNode::I32 => cursor.read_i32::<NE>().unwrap().hash_one(seed),
                IntegerTypeNode::I64 => cursor.read_i64::<NE>().unwrap().hash_one(seed),
                IntegerTypeNode::I128 => cursor.read_i128::<NE>().unwrap().hash_one(seed),
            }
        } else {
            // Default case: `hash_one` for `(T,)` constructs a hasher and calls
            // `<T as PortableHash>::hash`, which is exactly what we want. Saves us from hard-coding
            // rapidhash here.
            AsIs(self).hash_one(seed)
        }
    }
}

impl Codegen for HashableValue<'_> {
    fn generate_piece(&self, _gen: &mut CodeGenerator) -> TokenStream {
        self.tokens.clone()
    }
}

/// Transparent hashable wrapper without a `hash_one` overload.
///
/// Hashing this type is guaranteed to produce the same result as creating a hasher (internally
/// rapidhash) and invoking `<T as PortableHash>::hash`, ignoring the specialized
/// `<T as PortableHash>::hash_one` method if present.
struct AsIs<T>(T);

impl<T: PortableHash> PortableHash for AsIs<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}
