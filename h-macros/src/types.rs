//! Typing machinery.
//!
//! This module is a rather general implementation of a subset of the Rust type system together with
//! type inference. It does not interact well with external Rust code (e.g. we don't see struct
//! definitions), but we handle built-in types just fine. The system is extensible enough to cover
//! structs if we learn to parse user-provided `struct` annotations.
//!
//!
//! # Type system
//!
//! The type system we use is considerably simpler than Hindley-Milner because we don't have type
//! variables, so type inference can be formalized as a sequence of unifications where types are
//! never reused; e.g. `unify(unify(T, U), V)` or, realistically, `T <- U; T <- V;`, after which `U`
//! and `V` are never used. This saves us from needing to introduce a DSU data structure.
//!
//!
//! # Representation
//!
//! Types are represented as recursive data structures, roughly
//!
//! ```text
//! Type = IntegerType | bool | char | &Type | [Type] | (Type, ..) | str | !
//! IntegerType = u8 | u16 | ..
//! ```
//!
//! This is complicated by several facts:
//!
//! - We're in Rust, so there's a bit of visible indirection.
//! - During type inference, types may be partially unknown or even invalid (i.e. inconsistently
//!   inferred).
//! - We want to keep track of which annotation each type comes from (or which expression it is
//!   inferred from) so that we can emit reasonable type errors.
//!
//! The [`NodePtr<T>`] wrapper handles all of this by wrapping the `T` in a [`Box`], storing span
//! information, and having dedicated [`NodePtr::Infer`] and [`NodePtr::Inconsistent`] values,
//! mapping to `_` and a type error in rustc. We represent them as `_` and `?` in text,
//! respectively.
//!
//! The [`TypePtr`] alias semantically represents any of the above, and instances of this type can
//! be constructed with pseudocode-like syntax with the [`type_ptr`] macro. In almost all cases,
//! types should be represented with a [`TypePtr`], not a [`TypeNode`] or [`NodeWithSpan`].
//!
//!
//! # Integers
//!
//! Note the odd outlining of `IntegerType` in the diagram above. As the type of an integer like `1`
//! cannot be inferred to any specific type like `u8` from the value alone, integer types can be in
//! a state of limbo, where we know that a certain type is an integer type, but nothing else.
//! Integer types can also be inconsistent, e.g. `1u8` and `2u16` are both integers, but lead to
//! an error if unified further.
//!
//! We handle this by introducing a general `{integer}` node and further specializing it to types
//! like `{integer} u8`, along with `{integer} _` and `{integer} ?`, storing the data in
//! [`NodePtr<IntegerTypeNode>`].
//!
//!
//! # Precision
//!
//! We don't allow mutable references, so there's no need to disambiguate between `&T` and `&mut T`.
//! Lifetimes are not supported either.
//!
//! `[T]` and `[T; N]` hash to the same value due to the [`Borrow`](std::borrow::Borrow)
//! implementation, so we leave checking the validity of `[T]` and `[T; N]` to rustc and use
//! a single [`TypeNode::ArrayOrSlice`] node.
//!
//! We don't support floats as they can't be hashed. If we decide to support them eventually, the
//! design would be similar to `{integer}`.

use core::fmt;
use proc_macro2::Span;
use proc_macro_error2::emit_error;
use syn::spanned::Spanned;

/// Type pointer.
///
/// If you ever need to represent an arbitrary type, e.g. the result of inference or when taking
/// a value in along with a type, this is the correct choice. It can represent `_` and a failed type
/// inference result along with an arbitrary "valid" type.
///
/// Instances of [`TypePtr`] can be `match`ed on; there aren't many helper methods. [`TypePtr`] can
/// be constructed with the [`node_ptr`] macro.
pub type TypePtr = NodePtr<TypeNode>;

/// Construct a [`TypePtr`].
///
/// The syntax is `type_ptr!(span => ty)` where `span` is an expression evaluating to a type
/// implementing [`Spanned`] corresponding to the source of the type, and `ty` is one of the
/// following expressions:
///
/// - `_`: evaluates to [`TypePtr::Infer`].
/// - `?`: evaluates to [`TypePtr::Inconsistent`].
/// - `#e`: evaluates to `e`, i.e. a substitution of an expression of type [`TypePtr`].
/// - *...or all the following expressions wrapped in [`TypePtr::Known`]:*
/// - `{integer} i`, where `i` is `_`, `?`, `#e` for `e` of type [`IntegerTypeNode`], or a specifc
///   type like `u8`
/// - `bool`
/// - `char`
/// - `&ty`, where `ty` is any item on this list. Mutable references and lifetimes are not
///   supported.
/// - `[ty]`, where `ty` is any item on this list. This represents both sized arrays and unsized
///   slices, as we don't track array lengths.
/// - `(..#e)`, where `e` is any expression of type `Vec<TypePtr>`, evaluating to a tuple
///   containing elements of listed types. Notably, `()`, `(bool,)`, and `(bool, bool)` is not
///   valid input to this macro.
/// - `str`
/// - `!`
macro_rules! type_ptr {
    ($span:expr => $($tt:tt)*) => {
        $crate::types::node_ptr!(type_node, $span => $($tt)*)
    };
}
pub(crate) use type_ptr;

/// Construct a [`NodePtr<T>`].
///
/// This is a general version of [`type_ptr`]. This handles `_`, `?`, and `#e` as expected, and
/// forwards other values to the macro named by `$node`. `$node` is not a full path, but a name of
/// the identifier in module [`types`](self), and the macro must be exported. The interface of
/// `$node` must be
///
/// ```nocompile
/// macro_rules! node {
///     ($span:expr => /* any syntax */) => { /* a value of type T */ };
/// }
/// ```
macro_rules! node_ptr {
    ($node:ident, $span:expr => _) => {
        NodePtr::Infer
    };
    ($node:ident, $span:expr => ?) => {
        NodePtr::Inconsistent
    };
    ($node:ident, $span:expr => # $e:expr) => {
        $e
    };
    ($node:ident, $span:expr => $($tt:tt)*) => {
        NodePtr::Known(NodeWithSpan {
            node: Box::new($crate::types::$node!($span => $($tt)*)),
            span: $span.span(),
        })
    };
}
pub(crate) use node_ptr;

/// Construct a [`TypeNode`].
///
/// This is an implementation detail of [`type_ptr`]. See the docs of that macro for more
/// information.
macro_rules! type_node {
    ($span:expr => {integer} $($tt:tt)*) => {
        TypeNode::Integer($crate::types::node_ptr!(integer_type_node, $span => $($tt)*))
    };
    ($span:expr => bool) => {
        TypeNode::Bool
    };
    ($span:expr => char) => {
        TypeNode::Char
    };
    ($span:expr => & $($tt:tt)*) => {
        TypeNode::Reference($crate::types::type_ptr!($span => $($tt)*))
    };
    ($span:expr => [$($tt:tt)*]) => {
        TypeNode::ArrayOrSlice($crate::types::type_ptr!($span => $($tt)*))
    };
    ($span:expr => (..# $e:expr)) => {
        TypeNode::Tuple($e)
    };
    ($span:expr => str) => {
        TypeNode::Str
    };
    ($span:expr => !) => {
        TypeNode::Never
    };
}
pub(crate) use type_node;

/// Construct an [`IntegerTypeNode`].
///
/// This is an implementation detail of [`type_ptr`]. See the docs of that macro for more
/// information.
macro_rules! integer_type_node {
    ($span:expr => i8) => {
        IntegerTypeNode::I8
    };
    ($span:expr => i16) => {
        IntegerTypeNode::I16
    };
    ($span:expr => i32) => {
        IntegerTypeNode::I32
    };
    ($span:expr => i64) => {
        IntegerTypeNode::I64
    };
    ($span:expr => i128) => {
        IntegerTypeNode::I128
    };
    ($span:expr => u8) => {
        IntegerTypeNode::U8
    };
    ($span:expr => u16) => {
        IntegerTypeNode::U16
    };
    ($span:expr => u32) => {
        IntegerTypeNode::U32
    };
    ($span:expr => u64) => {
        IntegerTypeNode::U64
    };
    ($span:expr => u128) => {
        IntegerTypeNode::U128
    };
}
pub(crate) use integer_type_node;

/// Owning node pointer.
///
/// This is a fixed-size wrapper around `T` of type [`Node`] with additional functionality:
///
/// - It can alternatively store `_` (`NodePtr::Infer`) and `?` (`NodePtr::Inconsistent`) (see
///   [module-level documentation](self) for what these symbols mean).
/// - It stores a [`Span`] in addition to the `T` for the sake of diagnostics.
///
/// We use the word "pointer" not because this is a literal pointer to `T`, but due to associations:
/// just like pointers can be nullable, this type has two additional sentinel values, and just like
/// pointer can store metadata, this type stores a span. This is not a reference, and it owns `T`
/// rather than shares it.
///
/// The order of variants indicates a subtyping relationship. Unifying any known type with `?`
/// subsumes the known type into `?`, and unifying `_` with any known type subsumes `_` into the
/// known type.
#[derive(Clone)]
pub enum NodePtr<T> {
    /// An unknown type, indicated `_`.
    Infer,
    /// A known type.
    Known(NodeWithSpan<T>),
    /// An inconsistent type, i.e. an inference error, indicated `?`.
    Inconsistent,
}

impl<T: Node> NodePtr<T> {
    /// Unify `other` into `self`.
    ///
    /// This mutates `self` to be the supertype of `self` and `other`, consuming `other`. No HM DSU
    /// shenanigans take place.
    ///
    /// If `self` and `other` could not be unified because they represent fundamentally distinct
    /// types (e.g. `{integer} u8` and `&[i32]`), [`NodePtr::Inconsistent`] is produced, and the
    /// unification error is emitted via `report`. Unifying a type with [`NodePtr::Inconsistent`]
    /// does not yield any new errors, because producing [`NodePtr::Inconsistent`] in the first
    /// place should have reported an error.
    ///
    /// `report` is a mutable reference rather than `impl FnMut` to allow reborrows.
    pub fn unify_with(&mut self, other: Self, report: &mut impl FnMut(UnifyError)) {
        match (self, other) {
            (this @ Self::Infer, other) | (this @ Self::Known(_), other @ Self::Inconsistent) => {
                *this = other;
            }
            (Self::Inconsistent, _) | (Self::Known(_), Self::Infer) => {}
            (Self::Known(this), Self::Known(other)) => Node::unify_with(this, other, report),
        }
    }

    /// Check if this node contains [`NodePtr::Inconsistent`] anywhere recursively.
    pub fn has_inconsistencies(&self) -> bool {
        match self {
            Self::Infer => false,
            Self::Known(NodeWithSpan { node, .. }) => node.has_inconsistencies(),
            Self::Inconsistent => true,
        }
    }

    /// Check if this node contains [`NodePtr::Infer`] anywhere recursively.
    pub fn has_infer(&self) -> bool {
        match self {
            Self::Infer => true,
            Self::Known(NodeWithSpan { node, .. }) => node.has_infer(),
            Self::Inconsistent => false,
        }
    }

    /// Extract the underlying type `T`.
    ///
    /// This can be used to cleanly navigate the types post-inference, after ensuring typecheck
    /// passes by ensuring both [`NodePtr::has_inconsistencies`] and [`NodePtr::has_infer`] return
    /// `false`.
    ///
    /// # Panics
    ///
    /// Panics if the value is [`NodePtr::Infer`] or [`NodePtr::Inconsistent`].
    pub fn unwrap_ref(&self) -> &T {
        match self {
            Self::Infer => panic!("cannot unwrap _ type"),
            Self::Known(NodeWithSpan { node, .. }) => node,
            Self::Inconsistent => panic!("cannot unwrap ? type"),
        }
    }
}

impl<T: fmt::Display> fmt::Display for NodePtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Infer => f.write_str("_"),
            Self::Known(this) => write!(f, "{}", this.node),
            Self::Inconsistent => f.write_str("?"),
        }
    }
}

/// Unification error.
///
/// Errors of this type are reported by [`NodePtr::unify_with`] and are then supposed to be emitted
/// as diagnostics by calling [`UnifyError::emit_inference`] or [`UnifyError::emit_cast`] from the
/// `report` callback.
///
/// A unification error occurs during unification of `T` and `U` if there's a path `p` such that
/// the nodes `T->p` and `U->p` are of distinct enum variants.
///
/// We deliberately don't *enter* the variants, i.e. a failure to unify `&[u8]` and `u32` is
/// reported as a mismatch between `&_` and `{integer}`. This is because the internal state can
/// depend on unrelated inference state, leading to changes in diagnostics due to modifications in
/// mostly unrelated code (e.g. whether an integer is unified with an empty vs non-empty slice
/// first). This "shell" of a type is produced by [`Node::outer`].
///
/// We also deliberately report the mismatch between `T->p` and `U->p`, not `T` and `U`. For
/// example, the diagnostic for `[i32]` failing to unify with `[i64]` points to the integer
/// literals, showing the root of the problem more precisely.
pub struct UnifyError {
    /// A type string representing the first outer type that failed unification.
    type1: String,
    /// A type string representing the second outer type that failed unification.
    type2: String,
    /// The span of the first outer type that failed unification.
    span1: Span,
    /// The span of the second outer type that failed unification.
    span2: Span,
}

impl UnifyError {
    /// Create a unification error from two nodes.
    ///
    /// This takes [`NodeWithSpan`] instead of [`NodePtr`] because unification with
    /// [`NodePtr::Infer`] or [`NodePtr::Inconsistent`] can never produce a type error.
    ///
    /// This function extracts the `outer` part of the nodes automatically, so there's no need to do
    /// that by hand. The only requirement is that `node1.node.outer()` and `node2.node.outer()`
    /// mismatch.
    fn new<T: Node>(node1: &NodeWithSpan<T>, node2: &NodeWithSpan<T>) -> Self {
        Self {
            type1: node1.node.outer().to_string(),
            type2: node2.node.outer().to_string(),
            span1: node1.span,
            span2: node2.span,
        }
    }

    /// Emit a diagnostic formatted as a type inference error.
    ///
    /// Used e.g. when unifying the types of `a` and `b` in `[a, b]`.
    pub fn emit_inference(self) {
        emit_error!(
            self.span2,
            "type `{}`, inferred from here, contradicts an earlier type annotation",
            self.type2,
        );
        emit_error!(self.span1, "the type was inferred as `{}` here", self.type1);
    }

    /// Emit a diagnostic formatted as an invalid `as` conversion.
    ///
    /// Used e.g. when unifying the type of `a` and `T` in `a as T`.
    ///
    /// Our understanding of `as` does not exactly match rustc, e.g. `&[1] as &[u32]` infers `1` as
    /// `u32` in our type system, but not in rustc. However, we have to support unsizing coercions,
    /// and we allow a superset of rustc conversions (within the bounds of values and types we
    /// support), and rustc typechecks the generated code anyway, so there's no harm in using
    /// unification to implement `as`.
    pub fn emit_cast(self) {
        emit_error!(
            self.span2,
            "cannot cast a value of type `{}` to `{}`\n`h` is not a full-blown interpreter; only trivial and unsizing casts (`&[T; N]` to `&[T]`) are supported",
            self.type2,
            self.type1
        );
    }
}

/// A node of a type tree.
///
/// This includes terminal types like `bool`, type groups like `{integer}`, and type containers like
/// tuples.
///
/// The methods of this trait should usually not be used directly, but invoked via [`NodePtr`].
pub trait Node: fmt::Display + Sized {
    /// Unify `other` into `self`.
    ///
    /// This method takes `NodeWithSpan<Self>` instead of `Self` because unification errors require
    /// spans, and `this` instead of `self` due to lack of arbitrary self types.
    ///
    /// See the documentation for [`NodePtr::unify_with`] for more information. This should usually
    /// ensure variants of `this` and `other` match and call `unify_with` recursively, or report
    /// a unification error if the variants don't match.
    fn unify_with(
        this: &mut NodeWithSpan<Self>,
        other: NodeWithSpan<Self>,
        report: &mut impl FnMut(UnifyError),
    );

    /// Check if this node contains [`NodePtr::Inconsistent`] anywhere recursively.
    fn has_inconsistencies(&self) -> bool;

    /// Check if this node contains [`NodePtr::Infer`] anywhere recursively.
    fn has_infer(&self) -> bool;

    /// Get the outer "shell" of the type.
    ///
    /// If the node is a `enum`, this is supposed to return the corresponding variant of the enum
    /// with all the internals replaced with [`NodePtr::Infer`]. This is used for [`UnifyError`]
    /// formatting, see the docs for that type for more information.
    fn outer(&self) -> Self;
}

/// An owning pointer to `T` with a span attached.
///
/// This is one of the variants of [`NodePtr`].
#[derive(Clone)]
pub struct NodeWithSpan<T> {
    /// The node.
    pub node: Box<T>,
    /// The span which the node originated from.
    pub span: Span,
}

/// A [`Node`] corresponding to an arbitrary type.
///
/// Unless you're matching on this enum, you should probably use [`TypePtr`] instead.
#[derive(Clone)]
pub enum TypeNode {
    /// `{integer} _`
    ///
    /// The exact type of the integer is stored in the `NodePtr<IntegerTypeNode>`, i.e. `u8` is
    /// represented by this type `{integer} u8`. `{integer} _` is an integer literal that hasn't yet
    /// been inferred to a particualr type, and `{integer} ?` is a type unification error between
    /// integers of different bitnesses.
    Integer(NodePtr<IntegerTypeNode>),
    /// `bool`
    Bool,
    /// `char`
    Char,
    /// `&_`
    ///
    /// Mutable references are not supported.
    Reference(TypePtr),
    /// `[_]`
    ///
    /// This includes both unsized slices and sized arrays. The array size is not stored, and
    /// checking its correctness is left to rustc.
    ArrayOrSlice(TypePtr),
    /// `(_, ..)`
    Tuple(Vec<TypePtr>),
    /// `str`
    Str,
    /// `!`
    Never,
}

impl Node for TypeNode {
    fn unify_with(
        this: &mut NodeWithSpan<Self>,
        mut other: NodeWithSpan<Self>,
        report: &mut impl FnMut(UnifyError),
    ) {
        match (&mut *this.node, *other.node) {
            (Self::Integer(x), Self::Integer(y)) => x.unify_with(y, report),

            (Self::Reference(x), Self::Reference(y))
            | (Self::ArrayOrSlice(x), Self::ArrayOrSlice(y)) => x.unify_with(y, report),

            (Self::Tuple(xs), Self::Tuple(ys)) if xs.len() == ys.len() => {
                for (x, y) in xs.iter_mut().zip(ys) {
                    x.unify_with(y, report);
                }
            }

            (Self::Bool, Self::Bool)
            | (Self::Char, Self::Char)
            | (Self::Str, Self::Str)
            | (Self::Never, Self::Never) => {}

            (_, other_node) => {
                // This dealloc-alloc pair should be avoidable. Rust thinks otherwise. This is
                // a cold path and I'm done wrangling with stupid semantics for today.
                other.node = Box::new(other_node);
                report(UnifyError::new(this, &other));
            }
        }
    }

    fn has_inconsistencies(&self) -> bool {
        match self {
            Self::Integer(node) => node.has_inconsistencies(),
            Self::Reference(node) | Self::ArrayOrSlice(node) => node.has_inconsistencies(),
            Self::Tuple(elems) => elems.iter().any(NodePtr::has_inconsistencies),
            Self::Bool | Self::Char | Self::Str | Self::Never => false,
        }
    }

    fn has_infer(&self) -> bool {
        match self {
            Self::Integer(node) => node.has_infer(),
            Self::Reference(node) | Self::ArrayOrSlice(node) => node.has_infer(),
            Self::Tuple(elems) => elems.iter().any(NodePtr::has_infer),
            Self::Bool | Self::Char | Self::Str | Self::Never => false,
        }
    }

    fn outer(&self) -> Self {
        match self {
            Self::Integer(_) => Self::Integer(NodePtr::Infer),
            Self::Bool => Self::Bool,
            Self::Char => Self::Char,
            Self::Reference(_) => Self::Reference(TypePtr::Infer),
            Self::ArrayOrSlice(_) => Self::ArrayOrSlice(TypePtr::Infer),
            Self::Tuple(elems) => Self::Tuple(vec![TypePtr::Infer; elems.len()]),
            Self::Str => Self::Str,
            Self::Never => Self::Never,
        }
    }
}

impl fmt::Display for TypeNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(NodePtr::Infer | NodePtr::Inconsistent) => f.write_str("{integer}"),
            Self::Integer(NodePtr::Known(this)) => write!(f, "{}", this.node),
            Self::Bool => f.write_str("bool"),
            Self::Char => f.write_str("char"),
            Self::Reference(node) => write!(f, "&{node}"),
            Self::ArrayOrSlice(node) => write!(f, "[{node}]"),
            Self::Tuple(elements) => match &**elements {
                [] => f.write_str("()"),
                [elem] => write!(f, "({elem},)"),
                [head, tail @ ..] => {
                    write!(f, "({head}")?;
                    for elem in tail {
                        write!(f, ", {elem}")?;
                    }
                    f.write_str(")")
                }
            },
            Self::Str => f.write_str("str"),
            Self::Never => f.write_str("!"),
        }
    }
}

/// A [`Node`] corresponding to a known integer type.
///
/// This is used in [`TypeNode::Integer`].
#[derive(Clone, Copy, PartialEq)]
pub enum IntegerTypeNode {
    /// `u8`
    U8,
    /// `u16`
    U16,
    /// `u32`
    U32,
    /// `u64`
    U64,
    /// `u128`
    U128,
    /// `i8`
    I8,
    /// `i16`
    I16,
    /// `i32`
    I32,
    /// `i64`
    I64,
    /// `i128`
    I128,
}

impl IntegerTypeNode {
    /// Get the minimum value of the integer type, cast to `u128`.
    ///
    /// If the type is signed, the value wraps.
    pub const fn min_as_u128(self) -> u128 {
        #[expect(clippy::cast_sign_loss, reason = "intended")]
        match self {
            Self::U8 => u8::MIN as u128,
            Self::U16 => u16::MIN as u128,
            Self::U32 => u32::MIN as u128,
            Self::U64 => u64::MIN as u128,
            Self::U128 => u128::MIN,
            Self::I8 => i8::MIN as u128,
            Self::I16 => i16::MIN as u128,
            Self::I32 => i32::MIN as u128,
            Self::I64 => i64::MIN as u128,
            Self::I128 => i128::MIN as u128,
        }
    }

    /// Get the maximum value of the integer type, losslessly cast to `u128`.
    pub const fn max_as_u128(self) -> u128 {
        match self {
            Self::U8 => u8::MAX as u128,
            Self::U16 => u16::MAX as u128,
            Self::U32 => u32::MAX as u128,
            Self::U64 => u64::MAX as u128,
            Self::U128 => u128::MAX,
            Self::I8 => i8::MAX as u128,
            Self::I16 => i16::MAX as u128,
            Self::I32 => i32::MAX as u128,
            Self::I64 => i64::MAX as u128,
            Self::I128 => i128::MAX as u128,
        }
    }

    /// Check if the type is signed.
    pub const fn is_signed(self) -> bool {
        match self {
            Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::U128 => false,
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::I128 => true,
        }
    }
}

impl Node for IntegerTypeNode {
    fn unify_with(
        this: &mut NodeWithSpan<Self>,
        other: NodeWithSpan<Self>,
        report: &mut impl FnMut(UnifyError),
    ) {
        if this.node != other.node {
            report(UnifyError::new(this, &other));
        }
    }

    fn has_inconsistencies(&self) -> bool {
        false
    }

    fn has_infer(&self) -> bool {
        false
    }

    fn outer(&self) -> Self {
        *self
    }
}

impl fmt::Display for IntegerTypeNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U8 => f.write_str("u8"),
            Self::U16 => f.write_str("u16"),
            Self::U32 => f.write_str("u32"),
            Self::U64 => f.write_str("u64"),
            Self::U128 => f.write_str("u128"),
            Self::I8 => f.write_str("i8"),
            Self::I16 => f.write_str("i16"),
            Self::I32 => f.write_str("i32"),
            Self::I64 => f.write_str("i64"),
            Self::I128 => f.write_str("i128"),
        }
    }
}

impl TypePtr {
    /// Parse a `syn` type.
    ///
    /// Directly emits a diagnostic and returns a type containing [`NodePtr::Inconsistent`] (not
    /// necessarily on the outer layer) on error.
    pub fn from_syn_type(ty: &syn::Type) -> Self {
        let node = match ty {
            syn::Type::Array(ty) => TypeNode::ArrayOrSlice(Self::from_syn_type(&ty.elem)),
            syn::Type::Group(ty) => return Self::from_syn_type(&ty.elem),
            syn::Type::Infer(_) => return NodePtr::Infer,
            syn::Type::Never(_) => TypeNode::Never,
            syn::Type::Macro(_) => {
                emit_error!(ty, "macros are not allowed in type position");
                return NodePtr::Inconsistent;
            }
            syn::Type::Paren(ty) => return Self::from_syn_type(&ty.elem),
            syn::Type::Path(ty) => {
                if ty.qself.is_some() {
                    emit_error!(ty, "associated types are not allowed in type position");
                    return NodePtr::Inconsistent;
                }
                if ty.path.is_ident("f32") || ty.path.is_ident("f64") {
                    emit_error!(ty, "floating-point numbers cannot be hashed");
                    return NodePtr::Inconsistent;
                }
                if let Some(node) = ty.path.get_ident().and_then(TypeNode::try_from_ident) {
                    node
                } else {
                    emit_error!(ty, "unknown type name\n`h` macros do not understand manually declared types, use `h::codegen` instead");
                    return NodePtr::Inconsistent;
                }
            }
            syn::Type::Reference(ty) => {
                if let Some(lifetime) = &ty.lifetime {
                    emit_error!(lifetime, "lifetimes are not allowed in type annotations");
                }
                if let Some(mutability) = ty.mutability {
                    emit_error!(mutability, "references in keys need to be immutable");
                }
                TypeNode::Reference(Self::from_syn_type(&ty.elem))
            }
            syn::Type::Slice(ty) => TypeNode::ArrayOrSlice(Self::from_syn_type(&ty.elem)),
            syn::Type::Tuple(ty) => {
                TypeNode::Tuple(ty.elems.iter().map(Self::from_syn_type).collect())
            }
            _ => {
                emit_error!(ty, "unsupported key type");
                return NodePtr::Inconsistent;
            }
        };

        NodePtr::Known(NodeWithSpan {
            node: Box::new(node),
            span: ty.span(),
        })
    }
}

impl TypeNode {
    /// Parse a `syn` identifier.
    ///
    /// This parses singular types like `bool` and integer types like `u8`.
    ///
    /// This method takes in an identifier rather than a name because constructing an integer type
    /// requires a span.
    ///
    /// This method never emits an error. If the identifier is unknown, returns `None`.
    pub fn try_from_ident(ident: &syn::Ident) -> Option<Self> {
        let name = &*ident.to_string();
        let span = ident.span();

        if let Some(node) = IntegerTypeNode::try_from_name(name) {
            return Some(TypeNode::Integer(NodePtr::Known(NodeWithSpan {
                node: Box::new(node),
                span,
            })));
        }

        Some(match name {
            "bool" => TypeNode::Bool,
            "char" => TypeNode::Char,
            "str" => TypeNode::Str,
            _ => return None,
        })
    }
}

impl IntegerTypeNode {
    /// Parse an identifier, e.g. `u8`.
    ///
    /// This method never emits an error. If the identifier is unknown, returns `None`.
    pub fn try_from_name(name: &str) -> Option<Self> {
        Some(match name {
            "u8" => Self::U8,
            "u16" => Self::U16,
            "u32" => Self::U32,
            "u64" => Self::U64,
            "u128" => Self::U128,
            "i8" => Self::I8,
            "i16" => Self::I16,
            "i32" => Self::I32,
            "i64" => Self::I64,
            "i128" => Self::I128,
            _ => return None,
        })
    }
}
