trait WithConst {
    const MEOW: i32 = 1234;
}

impl WithConst for () {}

struct Meow<T>(core::marker::PhantomData<T>);

impl<T> WithConst for Meow<T> {}

fn main() {
    let c_str = h::map! {
        for _;
        c"meow" => (),
    };

    let index = h::map! {
        for _;
        [1][0] => (),
    };

    let integer_literal = h::map! {
        for _;
        0_integer => (),
    };

    let macros = h::map! {
        for _;
        h::map! {
            for _;
            () => (),
        } => (),
    };

    let associated_const = h::map! {
        i32::MAX => (),
        <()>::MEOW => (),
        <() as WithConst>::MEOW => (),
        Meow::<i32>::WithConst => (),
    };
}
