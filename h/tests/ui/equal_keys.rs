fn main() {
    let meow = h::map!(
        for u8;
        0 => Some(()),
        0 => None,
    );
}
