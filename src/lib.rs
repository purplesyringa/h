mod hashed;
mod table;
mod unhashed;

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test() {
        use core::hint::black_box;
        use hashed::{HashOrFallback, MultiplicativeU32Hash, Phf};
        use rand::{distributions::Standard, rngs::SmallRng, Rng, SeedableRng};
        use std::hash::{BuildHasher, Hasher};
        use std::time::Instant;
        use table::PhfMap;
        use unhashed::{Mixer, Phf as UnhashedPhf};

        struct H {
            data: u32,
        }
        impl Hasher for H {
            fn finish(&self) -> u64 {
                self.data as u64 * 0x100000001
            }
            fn write(&mut self, bytes: &[u8]) {
                if bytes.len() >= 4 {
                    self.data = u32::from_ne_bytes(bytes[..4].try_into().unwrap());
                }
            }
        }

        #[derive(Default)]
        struct B;
        impl BuildHasher for B {
            type Hasher = H;
            fn build_hasher(&self) -> H {
                H { data: 0 }
            }
        }

        let phf: PhfMap<u32, usize, MultiplicativeU32Hash> = PhfMap {
            phf: Phf {
                hash_fn: HashOrFallback::Hash(MultiplicativeU32Hash { factor: 1 }),
                unhashed_phf: UnhashedPhf {
                    hash_space: 27,
                    hash_space_with_oob: 47,
                    bucket_shift: 29,
                    displacements: vec![0, 0, 11, 11, 9, 20, 0, 9],
                    mixer: Mixer::Add,
                },
            },
            data: vec![
                None,
                None,
                None,
                None,
                Some((651922383, 10)),
                None,
                None,
                None,
                None,
                None,
                Some((1616197046, 19)),
                Some((459866136, 7)),
                Some((629494837, 6)),
                None,
                Some((2257590677, 16)),
                None,
                Some((2576413408, 13)),
                Some((995478318, 4)),
                Some((1154560249, 18)),
                Some((3057944555, 0)),
                Some((3214797839, 24)),
                Some((3370796855, 21)),
                Some((2212716128, 17)),
                Some((3690479214, 20)),
                Some((2472360478, 8)),
                Some((2633605796, 15)),
                Some((2801406199, 9)),
                Some((1216674179, 5)),
                Some((3163203821, 22)),
                None,
                Some((3079493726, 11)),
                Some((3641629404, 14)),
                None,
                Some((3900700927, 2)),
                Some((4063049665, 12)),
                Some((3879193679, 3)),
                Some((4052517131, 23)),
                None,
                None,
                None,
                Some((3286801301, 1)),
                Some((3451060895, 25)),
                None,
                None,
                None,
                None,
                None,
            ],
            len: 26,
        };

        // let phf: PhfMap<u32, usize, hashed::MultiplicativeU32Hash> = PhfMap::try_new(vec![
        //     (0xb64487ebu32, 0),
        //     (0xc3e89b95u32, 1),
        //     (0xe87ff8ffu32, 2),
        //     (0xe737cc4fu32, 3),
        //     (0x3b55cb2eu32, 4),
        //     (0x4884f983u32, 5),
        //     (0x25855435u32, 6),
        //     (0x1b690018u32, 7),
        //     (0x935d3a1eu32, 8),
        //     (0xa6fa10f7u32, 9),
        //     (0x26db8bcfu32, 10),
        //     (0xb78d585eu32, 11),
        //     (0xf22d37c1u32, 12),
        //     (0x9990f2e0u32, 13),
        //     (0xd90edadcu32, 14),
        //     (0x9cf9a2a4u32, 15),
        //     (0x86901995u32, 16),
        //     (0x83e35e60u32, 17),
        //     (0x44d130f9u32, 18),
        //     (0x605535b6u32, 19),
        //     (0xdbf83e6eu32, 20),
        //     (0xc8ea4737u32, 21),
        //     (0xbc8aa8edu32, 22),
        //     (0xf18c810bu32, 23),
        //     (0xbf9dec0fu32, 24),
        //     (0xcdb3029fu32, 25),
        // ])
        // .unwrap();

        // println!("{phf:?}");

        // let mut entries: Vec<(u32, ())> = SmallRng::seed_from_u64(0x439f26744da767e5)
        //     .sample_iter(Standard)
        //     .take(1000000)
        //     .map(|k| (k, ()))
        //     .collect();
        // entries.sort_unstable();
        // entries.dedup();

        let start = Instant::now();
        for _ in 0..1000000000 {
            black_box(phf.get(&black_box(0xcdb3029f)).unwrap());
            //     // let phf: HashMap<u32, (), B> = entries.clone().into_iter().collect();
            //     let phf: PhfMap<u32, (), hashed::MultiplicativeU32Hash> =
            //         PhfMap::try_new(entries.clone()).unwrap();
            //     black_box(phf);
        }
        println!("{:?}", start.elapsed());

        // for (k, _) in entries {
        //     black_box(phf.get(&k));
        // }
    }
}

// use std::hint::black_box;
// use std::time::Instant;

// fn main() {
//     use rand::{distributions::Standard, rngs::SmallRng, Rng, SeedableRng};
//     let mut keys: Vec<u32> = SmallRng::seed_from_u64(0xa767e5439f26744d)
//         .sample_iter(Standard)
//         .take(1000000)
//         .collect();
//     // let mut keys: Vec<u32> = (0..100000).map(|x: u32| x.rotate_right(17)).collect();
//     // let mut keys: Vec<u32> = (0..1000000).map(|x: u32| x.rotate_right(20)).collect();
//     for _ in 0..50 {
//         // let _ = keys
//         //     .iter()
//         //     .copied()
//         //     .collect::<std::collections::HashSet<u32>>();
//         try_generate_phf(black_box(&keys)).unwrap();
//     }
// }
