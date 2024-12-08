fn main() {
    let meow = h::map! {
        for cute!();
    };

    let nya = h::map! {
        for <i32 as core::ops::Add>::Output;
    };

    let mrrrp = h::map! {
        for (f32, f64);
        (0., -0.) => (),
    };

    let unknown = h::map! {
        for What_is_a_type;
    };

    let lifetime_elided = h::map! {
        for &'_ str;
        "meow" => (),
    };

    let lifetime_generic = h::map! {
        for &'a str;
        "meow" => (),
    };

    let lifetime_static = h::map! {
        for &'static str;
        "meow" => (),
    };

    let mutable_key = h::map! {
        for &mut i32;
        &0 => (),
        &mut 1 => (),
    };

    let generic = h::map! {
        for impl Sync;
    };
}
