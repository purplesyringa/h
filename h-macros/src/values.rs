use super::{
    constants::get_constant,
    types::{type_ptr, IntegerTypeNode, Node, NodePtr, NodeWithSpan, TypeNode, TypePtr},
};
use core::fmt::Write;
use proc_macro2::Span;
use proc_macro_error2::emit_error;
use syn::spanned::Spanned;

#[derive(Clone)]
pub enum Value {
    Integer {
        absolute_value: u128,
        negated: bool,
        span: Span,
    },
    Bool(bool),
    Char(char),
    Reference(Box<Value>),
    ArrayOrSlice(Vec<Value>),
    Tuple(Vec<Value>),
    Str(String),
    Inconsistent,
}

impl Value {
    pub fn has_inconsistencies(&self) -> bool {
        match self {
            Self::Integer { .. } | Self::Bool(_) | Self::Char(_) | Self::Str(_) => false,
            Self::Reference(target) => target.has_inconsistencies(),
            Self::ArrayOrSlice(elems) | Self::Tuple(elems) => {
                elems.iter().any(Self::has_inconsistencies)
            }
            Self::Inconsistent => true,
        }
    }
}

#[derive(Clone)]
pub struct TypedValue {
    pub value: Value,
    pub ty: TypePtr,
}

impl TypedValue {
    const fn inconsistent() -> Self {
        Self {
            value: Value::Inconsistent,
            ty: NodePtr::Inconsistent,
        }
    }
}

pub trait AsTypedValue {
    #[must_use]
    fn as_typed_value(&self, source: Span) -> TypedValue;
}

macro_rules! from_signed_integer {
    ($($ty:tt)*) => {
        $(
            impl AsTypedValue for $ty {
                fn as_typed_value(&self, source: Span) -> TypedValue {
                    TypedValue {
                        value: Value::Integer {
                            absolute_value: self.unsigned_abs().into(),
                            negated: *self < 0,
                            span: source,
                        },
                        ty: type_ptr!(source => {integer} $ty),
                    }
                }
            }
        )*
    };
}

from_signed_integer!(i8 i16 i32 i64 i128);

macro_rules! from_unsigned_integer {
    ($($ty:tt)*) => {
        $(
            impl AsTypedValue for $ty {
                fn as_typed_value(&self, source: Span) -> TypedValue {
                    TypedValue {
                        value: Value::Integer {
                            absolute_value: (*self).into(),
                            negated: false,
                            span: source,
                        },
                        ty: type_ptr!(source => {integer} $ty),
                    }
                }
            }
        )*
    };
}

from_unsigned_integer!(u8 u16 u32 u64 u128);

impl AsTypedValue for bool {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        TypedValue {
            value: Value::Bool(*self),
            ty: type_ptr!(source => bool),
        }
    }
}

impl AsTypedValue for char {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        TypedValue {
            value: Value::Char(*self),
            ty: type_ptr!(source => char),
        }
    }
}

impl<T: AsTypedValue + ?Sized> AsTypedValue for &T {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        let target = (**self).as_typed_value(source);
        TypedValue {
            value: Value::Reference(Box::new(target.value)),
            ty: type_ptr!(source => & #target.ty),
        }
    }
}

impl AsTypedValue for str {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        TypedValue {
            value: Value::Str(self.to_string()),
            ty: type_ptr!(source => str),
        }
    }
}

impl AsTypedValue for syn::ExprArray {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        let mut unified_ty = NodePtr::Infer;
        let values = self
            .elems
            .iter()
            .map(|elem| {
                let elem = elem.as_typed_value(elem.span());
                let mut failed = false;
                unified_ty.unify_with(elem.ty, &mut |e| {
                    e.emit_inference();
                    failed = true;
                });
                if failed {
                    unified_ty = NodePtr::Inconsistent;
                }
                elem.value
            })
            .collect();
        TypedValue {
            value: Value::ArrayOrSlice(values),
            ty: type_ptr!(source => [#unified_ty]),
        }
    }
}

impl AsTypedValue for syn::ExprCast {
    fn as_typed_value(&self, _source: Span) -> TypedValue {
        let TypedValue {
            mut value,
            ty: ty_from,
        } = self.expr.as_typed_value(self.expr.span());
        let mut ty_to = TypePtr::from_syn_type(&self.ty);

        // `value: From` and `From ~ To` does not imply `value: To` if `From` has inconsistencies,
        // as `~` ignores them. This can lead to casts returning consistent values of consistent
        // types that don't match the type. Prevent this by preemptively marking such values as
        // inconsistent.
        if ty_from.has_inconsistencies() {
            value = Value::Inconsistent;
        }

        ty_to.unify_with(ty_from, &mut |e| {
            value = Value::Inconsistent;
            e.emit_cast();
        });

        TypedValue { value, ty: ty_to }
    }
}

impl AsTypedValue for syn::ExprGroup {
    fn as_typed_value(&self, _source: Span) -> TypedValue {
        self.expr.as_typed_value(self.expr.span())
    }
}

impl AsTypedValue for syn::ExprIndex {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        // For diagnostics
        let _ = self.expr.as_typed_value(self.expr.span());
        let _ = self.index.as_typed_value(self.index.span());

        let is_unsizing = matches!(&*self.index, syn::Expr::Range(range) if range.start.is_none() && range.end.is_none());

        if is_unsizing {
            emit_error!(
                source,
                "indexing is not supported\n`h` is not a full-blown interpreter\nto unsize an array, use `as &[_]`",
            );
        } else {
            emit_error!(
                source,
                "indexing is not supported\n`h` is not a full-blown interpreter",
            );
        }

        TypedValue::inconsistent()
    }
}

impl AsTypedValue for syn::ExprLit {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        match &self.lit {
            syn::Lit::Str(lit) => <&str>::as_typed_value(&&*lit.value(), source),

            syn::Lit::ByteStr(lit) => TypedValue {
                value: Value::Reference(Box::new(Value::ArrayOrSlice(
                    lit.value()
                        .into_iter()
                        .map(|byte| Value::Integer {
                            absolute_value: byte.into(),
                            negated: false,
                            span: source,
                        })
                        .collect(),
                ))),
                ty: type_ptr!(source => &[{integer} u8]),
            },

            syn::Lit::CStr(_) => {
                emit_error!(
                    source,
                    "C-string literals are not supported\n`h` is not a full-blown interpreter",
                );
                TypedValue::inconsistent()
            }

            syn::Lit::Byte(lit) => u8::as_typed_value(&lit.value(), source),

            syn::Lit::Char(lit) => char::as_typed_value(&lit.value(), source),

            syn::Lit::Int(lit) => {
                let integer_type = if lit.suffix() == "" {
                    NodePtr::Infer
                } else if let Some(ty) = IntegerTypeNode::try_from_name(lit.suffix()) {
                    NodePtr::Known(NodeWithSpan {
                        node: Box::new(ty),
                        span: source,
                    })
                } else {
                    emit_error!(lit, "invalid integer literal suffix `{}`", lit.suffix());
                    NodePtr::Inconsistent
                };

                let Ok(value) = lit.base10_parse::<u128>() else {
                    emit_error!(lit, "too large integer");
                    return TypedValue::inconsistent();
                };

                TypedValue {
                    value: Value::Integer {
                        absolute_value: value,
                        negated: false,
                        span: source,
                    },
                    ty: type_ptr!(source => {integer} #integer_type),
                }
            }

            syn::Lit::Float(_) => {
                emit_error!(source, "floating-point numbers cannot be hashed");
                TypedValue::inconsistent()
            }

            syn::Lit::Bool(lit) => bool::as_typed_value(&lit.value(), source),

            _ => {
                emit_error!(
                    source,
                    "invalid literal\n`h` is not a full-blown interpreter",
                );
                TypedValue::inconsistent()
            }
        }
    }
}

impl AsTypedValue for syn::ExprMacro {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        emit_error!(source, "macros are not allowed in key position");
        TypedValue::inconsistent()
        // That was anticlimatic
    }
}

impl AsTypedValue for syn::ExprParen {
    fn as_typed_value(&self, _source: Span) -> TypedValue {
        self.expr.as_typed_value(self.expr.span())
    }
}

impl AsTypedValue for syn::ExprPath {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        if self.qself.is_some() {
            emit_error!(source, "trait paths are not allowed in key position\n`h` is not a full-blown interpreter; only certain constants are supported");
            return TypedValue::inconsistent();
        }

        if self
            .path
            .segments
            .iter()
            .any(|segment| !segment.arguments.is_empty())
        {
            emit_error!(source, "generic parameters are not allowed in paths in key position\n`h` is not a full-blown interpreter; only certain constants are supported");
            return TypedValue::inconsistent();
        }

        let mut path_str = String::new();
        if self.path.leading_colon.is_some() {
            path_str += "::";
        }
        for (i, segment) in self.path.segments.iter().enumerate() {
            if i > 0 {
                path_str += "::";
            }
            write!(path_str, "{}", segment.ident).unwrap();
        }

        get_constant(&path_str, self.span()).unwrap_or_else(|| {
            emit_error!(source, "unsupported path `{}`\n`h` is not a full-blown interpreter; only certain constants are supported", path_str);
            TypedValue::inconsistent()
        })
    }
}

impl AsTypedValue for syn::ExprReference {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        if let Some(mutability) = self.mutability {
            emit_error!(mutability, "references in keys need to be immutable");
        }

        let TypedValue { value, ty } = self.expr.as_typed_value(self.expr.span());
        TypedValue {
            value: Value::Reference(Box::new(value)),
            ty: type_ptr!(source => & #ty),
        }
    }
}

impl AsTypedValue for syn::ExprTuple {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        let mut values = Vec::with_capacity(self.elems.len());
        let mut types = Vec::with_capacity(self.elems.len());

        for elem in &self.elems {
            let TypedValue { value, ty } = elem.as_typed_value(elem.span());
            values.push(value);
            types.push(ty);
        }

        TypedValue {
            value: Value::Tuple(values),
            ty: type_ptr!(source => (..#types)),
        }
    }
}

impl AsTypedValue for syn::ExprUnary {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        let argument = self.expr.as_typed_value(self.expr.span());

        match argument.ty {
            NodePtr::Infer => {
                emit_error!(self.expr, "type annotations needed");
                emit_error!(source, "type must be known at this point");
            }
            NodePtr::Known(NodeWithSpan {
                node: arg_node,
                span: arg_span,
            }) => {
                match self.op {
                    syn::UnOp::Deref(_) => {
                        // It's occasionally useful to dereference byte string literals, so
                        // implement this despite not being a literal
                        let TypeNode::Reference(target_ty) = *arg_node else {
                            emit_error!(
                                source,
                                "type `{}` cannot be dereferenced",
                                arg_node.outer()
                            );
                            return TypedValue::inconsistent();
                        };
                        let target_value = match argument.value {
                            Value::Reference(target_value) => *target_value,
                            Value::Inconsistent => return TypedValue::inconsistent(),
                            _ => unreachable!(),
                        };
                        return TypedValue {
                            value: target_value,
                            ty: target_ty,
                        };
                    }

                    syn::UnOp::Neg(_) => match *arg_node {
                        TypeNode::Integer(_) => {
                            let value = match argument.value {
                                Value::Integer {
                                    absolute_value,
                                    negated,
                                    ..
                                } => Value::Integer {
                                    absolute_value,
                                    negated: !negated,
                                    span: source,
                                },
                                Value::Inconsistent => Value::Inconsistent,
                                _ => unreachable!(),
                            };
                            return TypedValue {
                                value,
                                ty: NodePtr::Known(NodeWithSpan {
                                    node: arg_node,
                                    span: arg_span,
                                }),
                            };
                        }
                        _ => {
                            emit_error!(
                                source,
                                "cannot apply unary operator `-` to type `{}`",
                                arg_node.outer()
                            );
                        }
                    },

                    _ => {
                        emit_error!(source, "unsupported unary operator\n`h` is not a full-blown interpreter; only certain operators are allowed");
                    }
                }
            }
            NodePtr::Inconsistent => {}
        }

        TypedValue::inconsistent()
    }
}

impl AsTypedValue for syn::Expr {
    fn as_typed_value(&self, source: Span) -> TypedValue {
        match self {
            syn::Expr::Array(expr) => expr.as_typed_value(source),
            syn::Expr::Cast(expr) => expr.as_typed_value(source),
            syn::Expr::Group(expr) => expr.as_typed_value(source),
            syn::Expr::Index(expr) => expr.as_typed_value(source),
            syn::Expr::Lit(expr) => expr.as_typed_value(source),
            syn::Expr::Macro(expr) => expr.as_typed_value(source),
            syn::Expr::Paren(expr) => expr.as_typed_value(source),
            syn::Expr::Path(expr) => expr.as_typed_value(source),
            syn::Expr::Reference(expr) => expr.as_typed_value(source),
            syn::Expr::Tuple(expr) => expr.as_typed_value(source),
            syn::Expr::Unary(expr) => expr.as_typed_value(source),
            _ => {
                emit_error!(source, "unsupported expression\n`h` is not a full-blown interpreter; only certain operators are allowed");
                TypedValue::inconsistent()
            }
        }
    }
}
