use proc_macro2::Span;
use proc_macro_error2::emit_error;
use std::fmt;
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
    ($span:expr => {float} $($tt:tt)*) => {
        TypeNode::Float($crate::types::node_ptr!(float_type_node, $span => $($tt)*))
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
    ($span:expr => array [$($tt:tt)*]) => {
        TypeNode::Array($crate::types::type_ptr!($span => $($tt)*))
    };
    ($span:expr => [$($tt:tt)*]) => {
        TypeNode::Slice($crate::types::type_ptr!($span => $($tt)*))
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

macro_rules! float_type_node {
    ($span:expr => f32) => {
        FloatTypeNode::F32
    };
    ($span:expr => f64) => {
        FloatTypeNode::F64
    };
}
pub(crate) use float_type_node;

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
}

impl<T: fmt::Display> fmt::Display for NodePtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Infer | Self::Inconsistent => f.write_str("_"),
            Self::Known(this) => write!(f, "{}", this.node),
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
            span1: node1.span.clone(),
            span2: node2.span.clone(),
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
    Float(NodePtr<FloatTypeNode>),
    Bool,
    Char,
    Reference(TypePtr),
    Array(TypePtr),
    Slice(TypePtr),
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
            (Self::Integer(a), Self::Integer(b)) => a.unify_with(b, report),

            (Self::Float(a), Self::Float(b)) => a.unify_with(b, report),

            (Self::Reference(a), Self::Reference(b))
            | (Self::Array(a), Self::Array(b))
            | (Self::Slice(a), Self::Slice(b)) => a.unify_with(b, report),

            (Self::Tuple(a), Self::Tuple(b)) if a.len() == b.len() => {
                for (a, b) in a.iter_mut().zip(b) {
                    a.unify_with(b, report);
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
            Self::Float(node) => node.has_inconsistencies(),
            Self::Reference(node) => node.has_inconsistencies(),
            Self::Array(node) => node.has_inconsistencies(),
            Self::Slice(node) => node.has_inconsistencies(),
            Self::Tuple(elems) => elems.iter().any(|elem| elem.has_inconsistencies()),
            Self::Bool | Self::Char | Self::Str | Self::Never => false,
        }
    }

    fn outer(&self) -> Self {
        match self {
            Self::Integer(_) => Self::Integer(NodePtr::Infer),
            Self::Float(_) => Self::Float(NodePtr::Infer),
            Self::Bool => Self::Bool,
            Self::Char => Self::Char,
            Self::Reference(_) => Self::Reference(TypePtr::Infer),
            Self::Array(_) => Self::Array(TypePtr::Infer),
            Self::Slice(_) => Self::Slice(TypePtr::Infer),
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
            Self::Float(NodePtr::Infer | NodePtr::Inconsistent) => f.write_str("{float}"),
            Self::Float(NodePtr::Known(this)) => write!(f, "{}", this.node),
            Self::Bool => f.write_str("bool"),
            Self::Char => f.write_str("char"),
            Self::Reference(node) => write!(f, "&{node}"),
            Self::Array(node) => write!(f, "[{node}; _]"),
            Self::Slice(node) => write!(f, "[{node}]"),
            Self::Tuple(elements) => match &elements[..] {
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

#[derive(Clone, Copy, PartialEq)]
pub enum FloatTypeNode {
    F32,
    F64,
}

impl Node for FloatTypeNode {
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

    fn outer(&self) -> Self {
        *self
    }
}

impl fmt::Display for FloatTypeNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::F32 => f.write_str("f32"),
            Self::F64 => f.write_str("f64"),
        }
    }
}

impl TypePtr {
    pub fn from_syn_type(ty: &syn::Type) -> Self {
        let node = match ty {
            syn::Type::Array(ty) => TypeNode::Array(Self::from_syn_type(&ty.elem)),
            syn::Type::Group(ty) => return Self::from_syn_type(&ty.elem),
            syn::Type::Infer(_) => return NodePtr::Infer,
            syn::Type::Never(_) => TypeNode::Never,
            syn::Type::Macro(_) => {
                emit_error!(ty, "Macros are not allowed in type position");
                return NodePtr::Inconsistent;
            }
            syn::Type::Paren(ty) => return Self::from_syn_type(&ty.elem),
            syn::Type::Path(ty) => {
                if ty.qself.is_some() {
                    emit_error!(ty, "Associated types are not allowed in type position");
                    return NodePtr::Inconsistent;
                }
                if let Some(node) = ty.path.get_ident().and_then(TypeNode::try_from_ident) {
                    node
                } else {
                    emit_error!(ty, "Unknown type name");
                    return NodePtr::Inconsistent;
                }
            }
            syn::Type::Reference(ty) => {
                if let Some(lifetime) = &ty.lifetime {
                    emit_error!(lifetime, "Lifetimes are not allowed in type annotations");
                }
                if let Some(mutability) = ty.mutability {
                    emit_error!(mutability, "References in keys need to be immutable");
                }
                TypeNode::Reference(Self::from_syn_type(&ty.elem))
            }
            syn::Type::Slice(ty) => TypeNode::Slice(Self::from_syn_type(&ty.elem)),
            syn::Type::Tuple(ty) => {
                TypeNode::Tuple(ty.elems.iter().map(Self::from_syn_type).collect())
            }
            _ => {
                emit_error!(ty, "Unsupported key type");
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

        if let Some(node) = FloatTypeNode::try_from_name(name) {
            return Some(TypeNode::Float(NodePtr::Known(NodeWithSpan {
                node: Box::new(node),
                span,
            })));
        }

        return Some(match name {
            "bool" => TypeNode::Bool,
            "char" => TypeNode::Char,
            "str" => TypeNode::Str,
            _ => return None,
        });
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

impl FloatTypeNode {
    pub fn try_from_name(name: &str) -> Option<Self> {
        Some(match name {
            "f32" => Self::F32,
            "f64" => Self::F64,
            _ => return None,
        })
    }
}
