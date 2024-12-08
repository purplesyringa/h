use syn::{
    parse::{Parse, ParseStream, Result},
    punctuated::Punctuated,
    Expr, Path, Token, Type,
};

#[derive(Debug)]
pub struct MapArm {
    pub key: Expr,
    pub value: Expr,
}

impl Parse for MapArm {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let key = input.parse::<Expr>()?;
        input.parse::<Token![=>]>()?;
        let value = input.parse::<Expr>()?;
        Ok(Self { key, value })
    }
}

#[derive(Debug)]
pub struct Context {
    pub h_crate: Option<Path>,
    pub key_type: Option<Type>,
    pub mutability: bool,
}

impl Parse for Context {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let h_crate = if input.parse::<Token![crate]>().is_ok() {
            let path = input.parse::<Path>()?;
            input.parse::<Token![;]>()?;
            Some(path)
        } else {
            None
        };

        let key_type = if input.parse::<Token![for]>().is_ok() {
            let ty = input.parse::<Type>()?;
            input.parse::<Token![;]>()?;
            Some(ty)
        } else {
            None
        };

        let mutability = if input.parse::<Token![mut]>().is_ok() {
            input.parse::<Token![;]>()?;
            true
        } else {
            false
        };

        Ok(Self {
            h_crate,
            key_type,
            mutability,
        })
    }
}

type Comma = Token![,];

#[derive(Debug)]
pub struct WithContext<Element> {
    pub context: Context,
    // Using `Token![,]` directly here leads to "`derive` cannot be used on items with type macros".
    pub elements: Punctuated<Element, Comma>,
}

impl<Element: Parse> Parse for WithContext<Element> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(Self {
            context: input.parse::<Context>()?,
            elements: Punctuated::parse_terminated(input)?,
        })
    }
}
