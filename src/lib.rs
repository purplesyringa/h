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
        use hashed::{HashOrFallback, MultiplicativeU64Hash, Phf};
        use rand::{distributions::Standard, rngs::SmallRng, Rng, SeedableRng};
        use std::hash::{BuildHasher, Hasher};
        use std::time::Instant;
        use table::PhfMap;
        use unhashed::{Mixer, Phf as UnhashedPhf};

        // struct H {
        //     data: u64,
        // }
        // impl Hasher for H {
        //     fn finish(&self) -> u64 {
        //         self.data
        //     }
        //     fn write(&mut self, bytes: &[u8]) {
        //         if bytes.len() >= 8 {
        //             self.data = u64::from_ne_bytes(bytes[..8].try_into().unwrap());
        //         }
        //     }
        // }

        // #[derive(Default)]
        // struct B;
        // impl BuildHasher for B {
        //     type Hasher = H;
        //     fn build_hasher(&self) -> H {
        //         H { data: 0 }
        //     }
        // }

        // let phf: PhfMap<u64, usize, MultiplicativeU64Hash> = PhfMap {
        //     phf: Phf {
        //         hash_fn: HashOrFallback::Hash(MultiplicativeU64Hash {
        //             factor: 6221665126544504664,
        //         }),
        //         unhashed_phf: UnhashedPhf {
        //             hash_space: 30,
        //             hash_space_with_oob: 49,
        //             bucket_shift: 61,
        //             displacements: vec![2, 0, 7, 19, 3, 8, 6, 0],
        //             mixer: Mixer::Add,
        //         },
        //     },
        //     data: vec![
        //         None,
        //         None,
        //         Some((2257590677, 16)),
        //         None,
        //         Some((3690479214, 20)),
        //         Some((4063049665, 12)),
        //         Some((459866136, 7)),
        //         None,
        //         Some((4052517131, 23)),
        //         Some((3286801301, 1)),
        //         Some((3163203821, 22)),
        //         Some((1616197046, 19)),
        //         Some((3214797839, 24)),
        //         Some((995478318, 4)),
        //         Some((1216674179, 5)),
        //         Some((1154560249, 18)),
        //         Some((651922383, 10)),
        //         None,
        //         None,
        //         None,
        //         None,
        //         None,
        //         Some((3079493726, 11)),
        //         Some((3451060895, 25)),
        //         Some((629494837, 6)),
        //         Some((2472360478, 8)),
        //         Some((3370796855, 21)),
        //         Some((3641629404, 14)),
        //         Some((2801406199, 9)),
        //         Some((2576413408, 13)),
        //         Some((3900700927, 2)),
        //         None,
        //         Some((2212716128, 17)),
        //         Some((3057944555, 0)),
        //         Some((2633605796, 15)),
        //         None,
        //         None,
        //         Some((3879193679, 3)),
        //         None,
        //         None,
        //         None,
        //         None,
        //         None,
        //         None,
        //         None,
        //         None,
        //         None,
        //         None,
        //         None,
        //     ],
        //     len: 26,
        // };

        // let phf: PhfMap<u64, usize, hashed::MultiplicativeU64Hash> = PhfMap::try_new(vec![
        //     (0xb64487eb, 0),
        //     (0xc3e89b95, 1),
        //     (0xe87ff8ff, 2),
        //     (0xe737cc4f, 3),
        //     (0x3b55cb2e, 4),
        //     (0x4884f983, 5),
        //     (0x25855435, 6),
        //     (0x1b690018, 7),
        //     (0x935d3a1e, 8),
        //     (0xa6fa10f7, 9),
        //     (0x26db8bcf, 10),
        //     (0xb78d585e, 11),
        //     (0xf22d37c1, 12),
        //     (0x9990f2e0, 13),
        //     (0xd90edadc, 14),
        //     (0x9cf9a2a4, 15),
        //     (0x86901995, 16),
        //     (0x83e35e60, 17),
        //     (0x44d130f9, 18),
        //     (0x605535b6, 19),
        //     (0xdbf83e6e, 20),
        //     (0xc8ea4737, 21),
        //     (0xbc8aa8ed, 22),
        //     (0xf18c810b, 23),
        //     (0xbf9dec0f, 24),
        //     (0xcdb3029f, 25),
        // ])
        // .unwrap();
        // println!("{phf:?}");

        let mut entries: Vec<(u64, ())> = SmallRng::seed_from_u64(0x439f26744da767e5)
            .sample_iter(Standard)
            .take(1000000)
            .map(|k| (k, ()))
            .collect();
        entries.sort_unstable();
        entries.dedup();

        let start = Instant::now();
        for _ in 0..10 {
            //     black_box(phf.get(&black_box(0xcdb3029f)).unwrap());
            //     //     // let phf: HashMap<u32, (), B> = entries.clone().into_iter().collect();
            let phf: PhfMap<u64, (), MultiplicativeU64Hash> =
                PhfMap::try_new(entries.clone()).unwrap();
            black_box(phf);
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
