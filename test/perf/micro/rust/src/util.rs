use rand::{Rand, Rng, SeedableRng};

use mersenne_twister::MT19937_64;
pub type MTRng = MT19937_64;

#[inline]
pub fn gen_rng(seed: u64) -> MTRng {
    MTRng::from_seed(seed)
}

pub fn fill_rand<'a, I, T: 'a, R>(a: I, rng: &mut R)
where
    I: IntoIterator<Item=&'a mut T>,
    T: Rand,
    R: Rng,
{
    for v in a.into_iter() {
        *v = rng.gen();
    }
}

pub fn myrand<R: Rng>(n: usize, rng: &mut R) -> Vec<f64> {
    let mut d: Vec<f64> = vec![0.; n];
    fill_rand(&mut d, rng);
    d
}
