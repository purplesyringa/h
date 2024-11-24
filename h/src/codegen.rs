//! Code generation.
//!
//! This module is useful for generating tables programmatically in `build.rs`.
//!
//! Scalars, tuples, strings, and hash tables can be codegen-ed. For your own keys, you'll need to
//! implement the [`Codegen`] trait manually.
//!
//! # Example
//!
//! ```rust
//! # fn main() -> std::io::Result<()> {
//! // Create a hash map
//! let map: h::Map<i32, i32> = h::Map::from_entries(vec![(1, 2), (3, 4)]);
//!
//! // Convert to code
//! let mut code = Vec::new();  // or anything else that implements io::Write
//! let mut gen = h::codegen::CodeGenerator::new(&mut code)?;
//! gen.write(&map)?;
//! gen.finish()?;
//!
//! // `code` can now be saved to an `.rs` file and then loaded with `include!`
//! # Ok(())
//! # }
//! ```

use alloc::borrow::{Cow, ToOwned};
use alloc::format;
use alloc::string::String;
use std::collections::{HashMap, HashSet};
use std::io::{Result, Write};

/// Code generator.
pub struct CodeGenerator<'a> {
    writer: &'a mut dyn Write,
    crate_paths: HashMap<String, String>,
    path_to_alias: HashMap<String, String>,
    aliases: HashSet<String>,
    alloc_wanted: bool,
}

impl<'a> CodeGenerator<'a> {
    /// Create a code generator with default settings.
    ///
    /// # Errors
    ///
    /// May fail if the underlying [`Write`](std::io::Write) implementation fails.
    #[inline]
    pub fn new(writer: &'a mut dyn Write) -> Result<Self> {
        let mut gen = Self {
            writer,
            crate_paths: HashMap::from([
                ("h".into(), "::h".into()),
                ("alloc".into(), "_Alloc".into()),
            ]),
            path_to_alias: HashMap::new(),
            aliases: HashSet::new(),
            alloc_wanted: false,
        };
        gen.write_code("{let __codegen_value=")?;
        Ok(gen)
    }

    /// Configure name-to-path mapping for crates.
    ///
    /// By default, `h` is mapped to `::h`. Reconfiguring this might be necessary if you want to
    /// generate code with `h` from a proc-macro.
    #[inline]
    pub fn with_crate(&mut self, name: &str, path: &str) {
        self.crate_paths.insert(name.into(), path.into());
    }

    /// Append literal code.
    ///
    /// # Errors
    ///
    /// May fail if the underlying [`Write`](std::io::Write) implementation fails.
    #[inline]
    pub fn write_code(&mut self, piece: &str) -> Result<()> {
        self.writer.write_all(piece.as_bytes())
    }

    /// Append a `b"..."` literal.
    ///
    /// # Errors
    ///
    /// May fail if the underlying [`Write`](std::io::Write) implementation fails.
    #[inline]
    #[allow(clippy::missing_panics_doc, reason = "false positive")]
    pub fn write_byte_string(&mut self, bytes: &[u8]) -> Result<()> {
        self.write_code("b\"")?;
        for byte in bytes {
            for c in core::ascii::escape_default(*byte) {
                self.write_code(core::str::from_utf8(&[c]).unwrap())?;
            }
        }
        self.write_code("\"")
    }

    /// Append a resolved path.
    ///
    /// The path must look like `crate::path::inside::it`, or just `crate`. Semantically, this path
    /// must be `use`able. This is different from `write_code` for two reasons:
    ///
    /// - This method resolves crates according to the paths configured by
    ///   [`CodeGenerator::with_crate`].
    /// - This method replaces long paths with short aliases imported just once with `use`, reducing
    ///   code size.
    ///
    /// # Errors
    ///
    /// May fail if the underlying [`Write`](std::io::Write) implementation fails.
    #[inline]
    #[allow(
        clippy::maybe_infinite_iter,
        clippy::missing_panics_doc,
        reason = "false positive"
    )]
    pub fn write_path(&mut self, path: &str) -> Result<()> {
        if let Some(alias) = self.path_to_alias.get(path) {
            return self.writer.write_all(alias.as_bytes());
        }

        if path.split_once("..").unwrap_or((path, "")).0 == "alloc" {
            self.alloc_wanted = true;
        }

        let mut alias = format!("__{}", path.rsplit_once("::").unwrap_or(("", path)).1);
        if self.aliases.contains(&alias) {
            alias = (2usize..)
                .map(|n| format!("{alias}{n}"))
                .find(|attempt| !self.aliases.contains(attempt))
                .unwrap();
        }

        self.write_code(&alias)?;
        self.path_to_alias.insert(path.to_owned(), alias);
        Ok(())
    }

    /// Append a codegen-able object.
    ///
    /// # Errors
    ///
    /// May fail if the underlying [`Write`](std::io::Write) implementation fails.
    #[inline]
    pub fn write<T: ?Sized + Codegen>(&mut self, piece: &T) -> Result<()> {
        piece.generate_into(self)
    }

    /// Flush all the unwritten code to the writer.
    ///
    /// # Errors
    ///
    /// May fail if the underlying [`Write`](std::io::Write) implementation fails.
    #[inline]
    pub fn finish(mut self) -> Result<()> {
        self.write_code(";")?;

        let mut crate_paths = core::mem::take(&mut self.crate_paths);

        if self.alloc_wanted
            && crate_paths
                .insert("alloc".to_owned(), "_Alloc".to_owned())
                .is_none()
        {
            self.write_code("extern crate alloc as _Alloc;")?;
        }

        for (path, alias) in core::mem::take(&mut self.path_to_alias) {
            let (crate_name, rest) = path.split_at(path.find("::").unwrap_or(path.len()));

            self.write_code("use ")?;
            if let Some(crate_path) = crate_paths.get(crate_name) {
                self.write_code(crate_path)?;
            } else {
                self.write_code("::")?;
                self.write_code(crate_name)?;
            }
            self.write_code(rest)?;
            self.write_code(" as ")?;
            self.write_code(&alias)?;
            self.write_code(";")?;
        }

        self.write_code("__codegen_value}")
    }
}

/// Values that can be turned into code.
pub trait Codegen {
    /// Append code into the code generator.
    ///
    /// # Errors
    ///
    /// May fail if the underlying [`Write`](std::io::Write) implementation fails.
    fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()>;
}

macro_rules! suffix_literal {
    ($($ty:ty)*) => {
        $(
            impl Codegen for $ty {
                #[inline]
                fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()> {
                    gen.write_code(&format!("{}", self))?;
                    gen.write_code(stringify!($ty))
                }
            }
        )*
    };
}

suffix_literal!(u8 u16 u32 u64 u128 usize i8 i16 i32 i64 i128 isize f32 f64);

impl Codegen for bool {
    #[inline]
    fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()> {
        gen.write_code(&format!("{self}"))
    }
}

impl<T: ?Sized> Codegen for core::marker::PhantomData<T> {
    #[inline]
    fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()> {
        gen.write_path("core::marker::PhantomData")
    }
}

impl<T: Codegen> Codegen for alloc::vec::Vec<T> {
    #[inline]
    fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()> {
        if let Some(bytes) = as_byte_slice(&**self) {
            gen.write_path("alloc::vec::Vec")?;
            gen.write_code("::from(")?;
            gen.write_byte_string(bytes)?;
            gen.write_code(")")
        } else {
            gen.write_path("alloc::vec")?;
            gen.write_code("!")?;
            gen.write(&**self)
        }
    }
}

impl<T: Codegen> Codegen for [T] {
    #[inline]
    fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()> {
        gen.write_code("[")?;
        for element in self {
            gen.write(element)?;
            gen.write_code(",")?;
        }
        gen.write_code("]")
    }
}

impl<T: ?Sized + Codegen> Codegen for &T {
    #[inline]
    fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()> {
        if let Some(bytes) = as_byte_slice(*self) {
            gen.write_byte_string(bytes)?;
            gen.write_code(" as &[u8]")
        } else {
            gen.write_code("&")?;
            gen.write(*self)
        }
    }
}

fn as_byte_slice<T: ?Sized>(object: &T) -> Option<&[u8]> {
    if typeid::of::<T>() == typeid::of::<[u8]>() {
        Some(unsafe { core::mem::transmute_copy::<&T, &[u8]>(&object) })
    } else {
        None
    }
}

/// Always converts to a borrowed version, making the generated code usable in a `const` context.
impl<T: ?Sized + ToOwned + Codegen> Codegen for Cow<'_, T> {
    #[inline]
    fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()> {
        gen.write_path("h::low_level::Cow::Borrowed")?;
        gen.write_code("(&")?;
        gen.write(self.as_ref())?;
        gen.write_code(")")
    }
}

impl<T: Codegen> Codegen for Option<T> {
    #[inline]
    fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()> {
        match &self {
            None => gen.write_path("core::option::Option::None"),
            Some(value) => {
                gen.write_path("core::option::Option::Some")?;
                gen.write_code("(")?;
                gen.write(value)?;
                gen.write_code(")")
            }
        }
    }
}

impl Codegen for () {
    #[inline]
    fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()> {
        gen.write_code("()")
    }
}

impl<T: Codegen, U: ?Sized + Codegen> Codegen for (T, U) {
    #[inline]
    fn generate_into(&self, gen: &mut CodeGenerator<'_>) -> Result<()> {
        gen.write_code("(")?;
        gen.write(&self.0)?;
        gen.write_code(",")?;
        gen.write(&self.1)?;
        gen.write_code(")")
    }
}
