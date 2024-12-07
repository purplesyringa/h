use syn::{
    parse::{Parse, ParseStream, Result},
    punctuated::Punctuated,
    Expr, Token, Type,
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
    pub key_type: Option<Type>,
}

impl Parse for Context {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let key_type = if input.parse::<Token![for]>().is_ok() {
            let ty = input.parse::<Type>()?;
            input.parse::<Token![;]>()?;
            Some(ty)
        } else {
            None
        };
        Ok(Self { key_type })
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
