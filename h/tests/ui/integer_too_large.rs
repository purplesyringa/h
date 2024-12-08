fn main() {
    let meow = h::map!(
        for u8;
        256 => (),
    );

    let mrrrp = h::map!(
        for i8;
        -129 => (),
    );

    let absurdly_large = h::map! {
        for _;
        1_000_000_000_000_000_000_000_000_000_000_000_000_000_000_000_u128 => (),
    };
}
