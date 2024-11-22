use alloc::borrow::{Cow, ToOwned};
use core::fmt;
use core::marker::PhantomData;

pub struct Codegen<'a, T: ?Sized>(pub &'a T);

macro_rules! suffix_literal {
    ($($ty:ty)*) => {
        $(
            impl fmt::Display for Codegen<'_, $ty> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, concat!("{}", stringify!($ty)), self.0)
                }
            }
        )*
    };
}

suffix_literal!(u8 u16 u32 u64 u128 usize i8 i16 i32 i64 i128 isize f32 f64);

impl fmt::Display for Codegen<'_, bool> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: ?Sized> fmt::Display for Codegen<'_, PhantomData<T>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "::core::marker::PhantomData")
    }
}

impl<'a, T> fmt::Display for Codegen<'a, [T]>
where
    Codegen<'a, T>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for element in self.0 {
            write!(f, "{}, ", Codegen(element))?;
        }
        write!(f, "]")
    }
}

impl<'a, 'b, T: ?Sized> fmt::Display for Codegen<'a, &'b T>
where
    Codegen<'a, T>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&{}", Codegen(self.0))
    }
}

impl<'a, B: ToOwned + ?Sized> fmt::Display for Codegen<'a, Cow<'_, B>>
where
    Codegen<'a, B>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "::h::low_level::Cow::Borrowed(&{})",
            Codegen(self.0.as_ref())
        )
    }
}

impl<'a, T> fmt::Display for Codegen<'a, Option<T>>
where
    Codegen<'a, T>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            None => write!(f, "::core::option::Option::None"),
            Some(value) => write!(f, "::core::option::Option::Some({})", Codegen(value)),
        }
    }
}

impl fmt::Display for Codegen<'_, ()> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "()")
    }
}

impl<'a, T, U> fmt::Display for Codegen<'a, (T, U)>
where
    Codegen<'a, T>: fmt::Display,
    Codegen<'a, U>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", Codegen(&(self.0).0), Codegen(&(self.0).1))
    }
}
