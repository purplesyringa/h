fn main() {
    let meow = h::map! {
        for _;
        0 => (),
    };

    let nya = h::map! {
        for (i32, _);
        (0, 1) => (),
    };

    let mrrp = h::map! {
        0 => (),
    };
}
