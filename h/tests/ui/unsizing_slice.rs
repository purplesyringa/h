fn main() {
    let unsizing_slice = h::map! {
        &[1, 2, 3][..] => (),
        &b"abc"[..] => (),
    };
}
