#![cfg(feature = "enabled")]

use h::codegen::CodeGenerator;
use std::path::PathBuf;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    let mut path = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    path.push("..");
    path.push("tests");
    path.push("english.txt.zst");
    let file = std::fs::File::open(path).expect("Failed to open file");
    let testdata = zstd::decode_all(file).expect("Failed to decode zstd");

    let mut entries = Vec::new();
    for (i, line) in testdata.split(|&c| c == b'\n').enumerate() {
        if !line.is_empty() {
            entries.push((line, i));
        }
    }
    entries.truncate(50000);

    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());

    let h_map = h::Map::<&[u8], usize>::from_entries(entries.clone());
    let h_code = CodeGenerator::new().generate(&h_map.borrow());
    std::fs::write(out_dir.join("english_h.rs"), h_code.to_string()).unwrap();
}
