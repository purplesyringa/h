use super::Map;
use alloc::borrow::Cow;
use alloc::string::String;
use alloc::{vec, vec::Vec};

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
    let mut rng = fastrand::Rng::with_seed(0x439f26744da767e5);
    let entries: alloc::vec::Vec<(u64, usize)> =
        (0..10000000).map(|i: usize| (rng.u64(..), i)).collect();

    let phf: Map<u64, usize> = Map::from_entries(entries.clone());
    for (k, v) in &entries {
        assert_eq!(phf.get(k), Some(v));
    }
    assert_eq!(phf.get(&0), None);
}

#[cfg(feature = "std")]
macro_rules! read_testdata {
    ($name:literal) => {{
        let mut path = std::path::PathBuf::from(file!());
        path.pop();
        path.push(concat!($name, ".zst"));
        let file = std::fs::File::open(path).expect("Failed to open file");
        Cow::<'static, [u8]>::from(zstd::decode_all(file).expect("Failed to decode zstd"))
    }};
}

#[cfg(not(feature = "std"))]
macro_rules! read_testdata {
    ($name:literal) => {{
        // Slower to compile, but doesn't need std
        static DATA: &[u8] = include_bytes!(concat!($name, ".zst"));
        Cow::<'static, [u8]>::from(zstd::decode_all(DATA).expect("Failed to decode zstd"))
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
        entries.push((line, i));
    }
    let phf: Map<&[u8], usize> = Map::from_entries(entries);
    assert_eq!(phf.get(&b"password"[..]), Some(&3));
    assert_eq!(phf.get(&b"gdfkghdfsjlgfd"[..]), None);
}
