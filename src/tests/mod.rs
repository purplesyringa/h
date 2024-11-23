use super::Map;
use alloc::string::String;
use alloc::{vec, vec::Vec};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

#[test]
fn borrow() {
    let phf: Map<&u64, usize> =
        Map::try_new(vec![(&123, 0), (&456, 1)]).expect("Failed to build PHF");
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

    let phf: Map<u64, usize> = Map::try_new(entries.clone()).expect("Failed to build PHF");
    for (k, v) in &entries {
        assert_eq!(phf.get(k), Some(v));
    }
    assert_eq!(phf.get(&0), None);
}

fn read_testdata(name: &str) -> impl BufRead {
    let mut path = PathBuf::from(file!());
    path.pop();
    path.push(std::format!("{name}.zst"));
    let file = File::open(path).expect("Failed to open file");
    let decoder = zstd::Decoder::new(file).expect("Failed to decode zstd");
    BufReader::new(decoder)
}

#[test]
fn build_500k_strings() {
    let mut entries = Vec::new();
    for (i, line) in read_testdata("english.txt").lines().enumerate() {
        entries.push((line.unwrap(), i));
    }
    let phf: Map<String, usize> = Map::try_new(entries).expect("Failed to build PHF");
    assert_eq!(phf.get("schoolroom"), Some(&351755));
    assert_eq!(phf.get("gdfkghdfsjlgfd"), None);
}

#[test]
#[ignore = "needs a lot of memory"]
fn build_14m_strings() {
    let mut entries = Vec::new();
    for (i, line) in read_testdata("rockyou.txt").split(b'\n').enumerate() {
        entries.push((line.unwrap(), i));
    }
    let phf: Map<Vec<u8>, usize> = Map::try_new(entries).expect("Failed to build PHF");
    assert_eq!(phf.get(&b"password"[..]), Some(&3));
    assert_eq!(phf.get(&b"gdfkghdfsjlgfd"[..]), None);
}
