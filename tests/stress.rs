#![allow(missing_docs, reason = "tests")]
#![cfg(all(feature = "build", not(miri)))]

use core::hash::BuildHasher;
use hyble::Phf;
use rapidhash::{quality::SeedableState, rng::RapidRng};
use ruzstd::{decoding::StreamingDecoder, io::Read};
use std::fs::File;
use std::time::Instant;

fn test_phf<T, Seed>(
    keys: &[T],
    seeds: impl Iterator<Item = Seed>,
    hasher: impl Fn(&T, &Seed) -> u64,
) {
    let start = Instant::now();
    let (seed, phf) = Phf::build(keys, seeds, &hasher);
    println!("{} keys: {:?} elapsed", keys.len(), start.elapsed());
    let mut indices: Vec<usize> = keys.iter().map(|key| phf.get(hasher(key, &seed))).collect();
    indices.sort_unstable();
    let capacity = phf.capacity();
    assert!(
        indices.iter().all(|i| *i < capacity),
        "index outside capacity"
    );
    assert!(
        indices.windows(2).all(|pair| pair[0] != pair[1]),
        "duplicate indices"
    );
}

fn test_integers(n: usize) {
    // This seed does not have collisions in the first 10M elements, so no need to deduplicate
    // elements, which takes time.
    assert!(n <= 10_000_000, "too many elements for this test");
    let mut rng = RapidRng::new(0x439f26744da767e5);
    let keys: Vec<u64> = (0..n).map(|_| rng.next()).collect();
    let seeds = core::iter::from_fn(|| Some(rng.next()));
    test_phf(&keys, seeds, |key, seed| key.wrapping_mul(*seed));
}

#[test]
fn build_10_integers() {
    test_integers(10);
}

#[test]
fn build_100_integers() {
    test_integers(100);
}

#[test]
fn build_1k_integers() {
    test_integers(1_000);
}

#[test]
fn build_10k_integers() {
    test_integers(10_000);
}

#[test]
fn build_100k_integers() {
    test_integers(100_000);
}

#[test]
fn build_1m_integers() {
    test_integers(1_000_000);
}

#[test]
#[ignore = "needs a lot of memory"]
fn build_10m_integers() {
    test_integers(10_000_000);
}

fn read_testdata(name: &str) -> Vec<u8> {
    let path = format!("tests/{}.zst", name);
    let file = File::open(path).expect("Failed to open file");
    let mut decoder = StreamingDecoder::new(file).expect("Failed to decode zstd");
    let mut result = Vec::new();
    decoder
        .read_to_end(&mut result)
        .expect("Failed to decode zstd");
    result
}

fn rapidhash() -> impl Iterator<Item = SeedableState<'static>> {
    let mut rng = RapidRng::new(1);
    core::iter::from_fn(move || Some(SeedableState::new(rng.next())))
}

#[test]
#[cfg_attr(miri, ignore = "slow")]
fn build_500k_strings() {
    let testdata = read_testdata("english.txt");
    let mut keys: Vec<String> = Vec::new();
    for line in testdata.split(|&c| c == b'\n') {
        keys.push(String::from_utf8(line.to_vec()).unwrap());
    }
    test_phf(&keys, rapidhash(), |key, seed| seed.hash_one(key));
}

#[test]
#[ignore = "needs a lot of memory"]
fn build_15m_strings() {
    let testdata = read_testdata("rockyou.txt");
    let mut keys: Vec<&[u8]> = Vec::new();
    for line in testdata.split(|&c| c == b'\n') {
        if !line.is_empty() {
            keys.push(line);
        }
    }
    test_phf(&keys, rapidhash(), |key, seed| seed.hash_one(key));
}
