use core::{fmt, ops::RangeInclusive};
use proc_macro2::Span;
use proc_macro_error2::emit_error;
use syn::spanned::Spanned;

pub type TypePtr = NodePtr<TypeNode>;

macro_rules! type_ptr {
    ($span:expr => $($tt:tt)*) => {
        $crate::types::node_ptr!(type_node, $span => $($tt)*)
    };
}
pub(crate) use type_ptr;

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

#[derive(Clone)]
pub enum NodePtr<T> {
    Infer,
    Known(NodeWithSpan<T>),
    Inconsistent,
}

impl<T: Node> NodePtr<T> {
    // When a new inconsistency arises, invokes `report`. Ignores old inconsistencies.
    pub fn unify_with(&mut self, other: Self, report: &mut impl FnMut(UnifyError)) {
        match (self, other) {
            (this @ Self::Infer, other) | (this @ Self::Known(_), other @ Self::Inconsistent) => {
                *this = other;
            }
            (Self::Inconsistent, _) | (Self::Known(_), Self::Infer) => {}
            (Self::Known(this), Self::Known(other)) => Node::unify_with(this, other, report),
        }
    }

    pub fn has_inconsistencies(&self) -> bool {
        match self {
            Self::Infer => false,
            Self::Known(NodeWithSpan { node, .. }) => node.has_inconsistencies(),
            Self::Inconsistent => true,
        }
    }

    pub fn has_infer(&self) -> bool {
        match self {
            Self::Infer => true,
            Self::Known(NodeWithSpan { node, .. }) => node.has_infer(),
            Self::Inconsistent => false,
        }
    }

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

pub struct UnifyError {
    type1: String,
    type2: String,
    span1: Span,
    span2: Span,
}

impl UnifyError {
    fn new<T: Node>(node1: &NodeWithSpan<T>, node2: &NodeWithSpan<T>) -> Self {
        Self {
            type1: node1.node.outer().to_string(),
            type2: node2.node.outer().to_string(),
            span1: node1.span,
            span2: node2.span,
        }
    }

    pub fn emit_inference(self) {
        emit_error!(
            self.span2,
            "type `{}`, inferred from here, contradicts an earlier type annotation",
            self.type2,
        );
        emit_error!(self.span1, "the type was inferred as `{}` here", self.type1);
    }

    pub fn emit_cast(self) {
        emit_error!(
            self.span2,
            "cannot cast a value of type `{}` to `{}`\n`h` is not a full-blown interpreter; only trivial and unsizing casts (`&[T; N]` to `&[T]`) are supported",
            self.type2,
            self.type1
        );
    }
}

pub trait Node: fmt::Display + Sized {
    fn unify_with(
        this: &mut NodeWithSpan<Self>,
        other: NodeWithSpan<Self>,
        report: &mut impl FnMut(UnifyError),
    );

    fn has_inconsistencies(&self) -> bool;
    fn has_infer(&self) -> bool;

    fn outer(&self) -> Self;
}

#[derive(Clone)]
pub struct NodeWithSpan<T> {
    pub node: Box<T>,
    pub span: Span,
}

#[derive(Clone)]
pub enum TypeNode {
    Integer(NodePtr<IntegerTypeNode>),
    Bool,
    Char,
    Reference(TypePtr),
    ArrayOrSlice(TypePtr),
    Tuple(Vec<TypePtr>),
    Str,
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

#[derive(Clone, Copy, PartialEq)]
pub enum IntegerTypeNode {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
}

impl IntegerTypeNode {
    // For signed integers, returns `Ok(MIN..=MAX)`. For unsigned integers, returns `Err(MAX)`.
    pub fn range(self) -> Result<RangeInclusive<i128>, u128> {
        match self {
            Self::U8 => Err(u8::MAX.into()),
            Self::U16 => Err(u16::MAX.into()),
            Self::U32 => Err(u32::MAX.into()),
            Self::U64 => Err(u64::MAX.into()),
            Self::U128 => Err(u128::MAX),
            Self::I8 => Ok(i8::MIN.into()..=i8::MAX.into()),
            Self::I16 => Ok(i16::MIN.into()..=i16::MAX.into()),
            Self::I32 => Ok(i32::MIN.into()..=i32::MAX.into()),
            Self::I64 => Ok(i64::MIN.into()..=i64::MAX.into()),
            Self::I128 => Ok(i128::MIN..=i128::MAX),
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
