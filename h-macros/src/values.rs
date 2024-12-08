use super::{
    constants::get_constant,
    types::{type_ptr, IntegerTypeNode, Node, NodePtr, NodeWithSpan, TypeNode, TypePtr},
};
use proc_macro2::Span;
use proc_macro_error2::emit_error;
use std::fmt::Write;
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
    fn inconsistent() -> Self {
        Self {
            value: Value::Inconsistent,
            ty: NodePtr::Inconsistent,
        }
    }
}

pub trait AsTypedValue {
    fn as_typed_value(&self, source: Span) -> TypedValue;
}

struct Inconsistent;

impl AsTypedValue for Inconsistent {
    fn as_typed_value(&self, _source: Span) -> TypedValue {
        TypedValue::inconsistent()
    }
}

macro_rules! from_signed_integer {
    ($($ty:tt)*) => {
        $(
            impl AsTypedValue for $ty {
                fn as_typed_value(&self, source: Span) -> TypedValue {
                    TypedValue {
                        value: Value::Integer {
                            absolute_value: self.unsigned_abs() as u128,
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
                            absolute_value: *self as u128,
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

pub fn evaluate_syn_expr(expr: &syn::Expr) -> TypedValue {
    match expr {
        syn::Expr::Array(expr) => {
            let mut unified_ty = NodePtr::Infer;
            let values = expr
                .elems
                .iter()
                .map(|elem| {
                    let elem = evaluate_syn_expr(elem);
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
                ty: type_ptr!(expr => array [#unified_ty]),
            }
        }

        syn::Expr::Cast(expr) => {
            let TypedValue {
                mut value,
                ty: mut ty_from,
            } = evaluate_syn_expr(&expr.expr);
            let mut ty_to = TypePtr::from_syn_type(&expr.ty);

            // `value: From` and `From ~ To` does not imply `value: To` if `From` has
            // inconsistencies, as `~` ignores them. This can lead to casts returning consistent
            // values of consistent types that don't match the type. Prevent this by
            // preemptively marking such values as inconsistent.
            if ty_from.has_inconsistencies() {
                value = Value::Inconsistent;
            }

            // Allow `&[T; N] -> &[T]` casts
            if let (
                NodePtr::Known(NodeWithSpan {
                    node: node_from, ..
                }),
                NodePtr::Known(NodeWithSpan { node: node_to, .. }),
            ) = (&mut ty_from, &mut ty_to)
            {
                if let (
                    TypeNode::Reference(NodePtr::Known(NodeWithSpan {
                        node: node_from, ..
                    })),
                    TypeNode::Reference(NodePtr::Known(NodeWithSpan { node: node_to, .. })),
                ) = (&mut **node_from, &mut **node_to)
                {
                    if let (TypeNode::Array(elem_ty_from), TypeNode::Slice(elem_ty_to)) =
                        (&mut **node_from, &mut **node_to)
                    {
                        // Stupid fucking ownership semantics
                        let elem_ty_from = std::mem::replace(elem_ty_from, NodePtr::Infer);
                        elem_ty_to.unify_with(elem_ty_from, &mut |e| {
                            value = Value::Inconsistent;
                            e.emit_cast();
                        });
                        return TypedValue { value, ty: ty_to };
                    }
                }
            }

            ty_to.unify_with(ty_from, &mut |e| {
                value = Value::Inconsistent;
                e.emit_cast();
            });

            TypedValue { value, ty: ty_to }
        }

        syn::Expr::Group(expr) => evaluate_syn_expr(&expr.expr),

        syn::Expr::Index(expr) => {
            let array = evaluate_syn_expr(&expr.expr);

            // Allow `&[T; N] -> &[T]` indexing
            if let syn::Expr::Range(range) = &*expr.index {
                if range.start.is_none() && range.end.is_none() {
                    if let NodePtr::Known(NodeWithSpan { node, .. }) = array.ty {
                        if let TypeNode::Array(elem_ty) = *node {
                            return TypedValue {
                                value: array.value,
                                ty: type_ptr!(expr => [#elem_ty]),
                            };
                        }
                    }
                }
            }

            emit_error!(
                expr.span(),
                "cannot perform this indexing operation\n`h` is not a full-blown interpreter; only unsizing `array[..]` slicing is supported",
            );
            TypedValue::inconsistent()
        }

        syn::Expr::Lit(expr) => match &expr.lit {
            syn::Lit::Str(lit) => <&str>::as_typed_value(&&*lit.value(), expr.span()),

            syn::Lit::ByteStr(lit) => TypedValue {
                value: Value::Reference(Box::new(Value::ArrayOrSlice(
                    lit.value()
                        .into_iter()
                        .map(|byte| Value::Integer {
                            absolute_value: byte as u128,
                            negated: false,
                            span: lit.span(),
                        })
                        .collect(),
                ))),
                ty: type_ptr!(expr => &array [{integer} u8]),
            },

            syn::Lit::CStr(_) => {
                emit_error!(
                    expr.span(),
                    "C-string literals are not supported\n`h` is not a full-blown interpreter",
                );
                <&Inconsistent>::as_typed_value(&&Inconsistent, expr.span())
            }

            syn::Lit::Byte(lit) => u8::as_typed_value(&lit.value(), expr.span()),

            syn::Lit::Char(lit) => char::as_typed_value(&lit.value(), expr.span()),

            syn::Lit::Int(lit) => {
                let integer_type = if lit.suffix() == "" {
                    NodePtr::Infer
                } else {
                    match IntegerTypeNode::try_from_name(lit.suffix()) {
                        Some(ty) => NodePtr::Known(NodeWithSpan {
                            node: Box::new(ty),
                            span: expr.span(),
                        }),
                        None => {
                            emit_error!(
                                lit.span(),
                                "invalid integer literal suffix `{}`",
                                lit.suffix()
                            );
                            NodePtr::Inconsistent
                        }
                    }
                };

                match lit.base10_parse::<u128>() {
                    Ok(value) => TypedValue {
                        value: Value::Integer {
                            absolute_value: value,
                            negated: false,
                            span: lit.span(),
                        },
                        ty: type_ptr!(expr => {integer} #integer_type),
                    },
                    Err(_) => {
                        emit_error!(lit.span(), "too large integer");
                        TypedValue::inconsistent()
                    }
                }
            }

            syn::Lit::Float(_) => {
                emit_error!(expr.span(), "floating-point numbers cannot be hashed");
                TypedValue::inconsistent()
            }

            syn::Lit::Bool(lit) => bool::as_typed_value(&lit.value(), expr.span()),

            _ => {
                emit_error!(
                    expr.span(),
                    "invalid literal\n`h` is not a full-blown interpreter"
                );
                TypedValue::inconsistent()
            }
        },

        syn::Expr::Macro(expr) => {
            emit_error!(expr, "macros are not allowed in key position");
            TypedValue::inconsistent()
        }

        syn::Expr::Paren(expr) => evaluate_syn_expr(&expr.expr),

        syn::Expr::Path(expr) => {
            if expr.qself.is_some() {
                emit_error!(expr, "trait paths are not allowed in key position\n`h` is not a full-blown interpreter; only certain constants are supported");
                return TypedValue::inconsistent();
            }

            if expr
                .path
                .segments
                .iter()
                .any(|segment| !segment.arguments.is_empty())
            {
                emit_error!(expr, "generic parameters are not allowed in paths in key position\n`h` is not a full-blown interpreter; only certain constants are supported");
                return TypedValue::inconsistent();
            }

            let mut path_str = String::new();
            if expr.path.leading_colon.is_some() {
                path_str += "::";
            }
            for (i, segment) in expr.path.segments.iter().enumerate() {
                if i > 0 {
                    path_str += "::";
                }
                write!(path_str, "{}", segment.ident).unwrap();
            }

            match get_constant(&path_str, expr.span()) {
                Some(ty_val) => ty_val,
                None => {
                    emit_error!(expr, "unsupported path `{}`\n`h` is not a full-blown interpreter; only certain constants are supported", path_str);
                    TypedValue::inconsistent()
                }
            }
        }

        syn::Expr::Reference(expr) => {
            if let Some(mutability) = expr.mutability {
                emit_error!(mutability, "references in keys need to be immutable");
            }

            let TypedValue { value, ty } = evaluate_syn_expr(&expr.expr);
            TypedValue {
                value: Value::Reference(Box::new(value)),
                ty: type_ptr!(expr => & #ty),
            }
        }

        syn::Expr::Tuple(expr) => {
            let mut values = Vec::with_capacity(expr.elems.len());
            let mut types = Vec::with_capacity(expr.elems.len());
            for elem in &expr.elems {
                let TypedValue { value, ty } = evaluate_syn_expr(elem);
                values.push(value);
                types.push(ty);
            }

            TypedValue {
                value: Value::Tuple(values),
                ty: type_ptr!(expr => (..#types)),
            }
        }

        syn::Expr::Unary(expr) => {
            let argument = evaluate_syn_expr(&expr.expr);

            match argument.ty {
                NodePtr::Infer => {
                    emit_error!(expr.expr, "type annotations needed");
                    emit_error!(expr, "type must be known at this point");
                }
                NodePtr::Known(NodeWithSpan {
                    node: arg_node,
                    span: arg_span,
                }) => {
                    match expr.op {
                        syn::UnOp::Deref(_) => {
                            // It's occasionally useful to dereference byte string literals, so
                            // implement this despite not being a literal
                            let TypeNode::Reference(target_ty) = *arg_node else {
                                emit_error!(
                                    expr,
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
                                        span: expr.span(),
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
                                    expr,
                                    "cannot apply unary operator `-` to type `{}`",
                                    arg_node.outer()
                                );
                            }
                        },

                        _ => {
                            emit_error!(expr, "unsupported unary operator\n`h` is not a full-blown interpreter; only certain operators are allowed");
                        }
                    }
                }
                NodePtr::Inconsistent => {}
            }

            TypedValue::inconsistent()
        }

        _ => {
            emit_error!(expr, "unsupported expression\n`h` is not a full-blown interpreter; only certain operators are allowed");
            TypedValue::inconsistent()
        }
    }
}
