#![cfg(feature = "enabled")]

use h::codegen::CodeGenerator;
use std::io::BufWriter;
use std::path::PathBuf;

fn main() {
    let mut path = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    path.push("..");
    path.push("tests");
    path.push("rockyou.txt.zst");
    let file = std::fs::File::open(path).expect("Failed to open file");
    let testdata = zstd::decode_all(file).expect("Failed to decode zstd");

    let mut entries = Vec::new();
    for (i, line) in testdata.split(|&c| c == b'\n').enumerate() {
        if !line.is_empty() {
            entries.push((line, i));
        }
    }

    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());

    // let mut rockyou_h =
    //     BufWriter::new(std::fs::File::create(out_dir.join("rockyou_h.rs")).unwrap());
    // let map = h::Map::<&[u8], usize>::from_entries(entries.clone());
    // let mut gen = CodeGenerator::new(&mut rockyou_h).unwrap();
    // gen.write(&map).unwrap();
    // gen.finish().unwrap();
}
