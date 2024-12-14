use super::Map;
use alloc::string::String;
use alloc::{vec, vec::Vec};
use ruzstd::{io::Read, StreamingDecoder};

#[test]
fn borrow() {
    let phf: Map<&u64, usize> = Map::from_entries(vec![(&123, 0), (&456, 1)]);
    assert_eq!(phf.get(&123), Some(&0));
    assert_eq!(phf.get(&456), Some(&1));
}

#[test]
#[ignore = "needs a lot of memory"]
fn build_10m_integers() {
    // This particular seed does not have collisions in the first 10M elements, so no need to sort
    // and deduplicate, which takes time.
    let mut rng = rapidhash::RapidRng::new(0x439f26744da767e5);
    let entries: alloc::vec::Vec<(u64, usize)> =
        (0..10000000).map(|i: usize| (rng.next(), i)).collect();

    let phf: Map<u64, usize> = Map::from_entries(entries.clone());
    for (k, v) in &entries {
        assert_eq!(phf.get(k), Some(v));
    }
    assert_eq!(phf.get(&0), None);
}

fn decode_zstd(source: impl Read) -> Vec<u8> {
    let mut decoder = StreamingDecoder::new(source).expect("Failed to decode zstd");
    let mut result = Vec::new();
    decoder
        .read_to_end(&mut result)
        .expect("Failed to decode zstd");
    result
}

#[cfg(feature = "std")]
macro_rules! read_testdata {
    ($name:literal) => {{
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../tests/", $name, ".zst");
        let file = std::fs::File::open(path).expect("Failed to open file");
        decode_zstd(file)
    }};
}

#[cfg(not(feature = "std"))]
macro_rules! read_testdata {
    ($name:literal) => {{
        // Slower to compile, but doesn't need std
        static DATA: &[u8] = include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../tests/",
            $name,
            ".zst"
        ));
        decode_zstd(DATA)
    }};
}

#[test]
fn build_500k_strings() {
    let testdata = read_testdata!("english.txt");
    let mut entries = Vec::new();
    for (i, line) in testdata.split(|&c| c == b'\n').enumerate() {
        entries.push((String::from_utf8(line.to_vec()).unwrap(), i));
    }
    let phf: Map<String, usize> = Map::from_entries(entries);
    assert_eq!(phf.get("schoolroom"), Some(&351755));
    assert_eq!(phf.get("gdfkghdfsjlgfd"), None);
}

#[test]
#[ignore = "needs a lot of memory"]
fn build_14m_strings() {
    let testdata = read_testdata!("rockyou.txt");
    let mut entries = Vec::new();
    for (i, line) in testdata.split(|&c| c == b'\n').enumerate() {
        if !line.is_empty() {
            entries.push((line, i));
        }
    }
    let phf: Map<&[u8], usize> = Map::from_entries(entries);
    assert_eq!(phf.get(&b"password"[..]), Some(&3));
    assert_eq!(phf.get(&b"gdfkghdfsjlgfd"[..]), None);
}
