static MAP: h::StaticMap<&[u8], usize> = include!(concat!(env!("OUT_DIR"), "/english_h.rs"));

fn main() {
    println!("Hello, world!");
}
