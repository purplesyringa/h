fn main() {
    let array = h::map! {
        [1, 2_u32, 3_u64] => ()
    };

    let keys = h::map! {
        1 => (),
        2_u32 => (),
        3_u64 => (),
    };

    let unsized_cast = h::map! {
        &[1_i32, 2, 3][..] => (),
        &[1_i64, 2, 3, 4][..] => (),
    };

    let what = h::map! {
        1 as &str => (),
    };
}
