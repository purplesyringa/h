fn main() {
    let invert = h::map! {
        !0_i32 => (),
    };

    let negate = h::map! {
        -"meow" => (),
    };

    let deref = h::map! {
        *0 => (),
    };

    let what = h::map! {
        if false { main() } => ()
    };
}
