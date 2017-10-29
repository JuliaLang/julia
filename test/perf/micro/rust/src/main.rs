#![feature(test)]
#![deny(unsafe_code)]

extern crate itertools;
extern crate mersenne_twister;
extern crate num;
extern crate rand;
extern crate test;

// Use BLAS directly
#[cfg(feature = "direct_blas")]
extern crate blas;

// Use ndarray (with BLAS implementation)
#[cfg(not(feature = "direct_blas"))]
#[macro_use(s)]
extern crate ndarray;

use std::time::{Duration, Instant};
use std::u32;
use std::fs::OpenOptions;
use std::io::{BufWriter, Write};

use test::black_box;
use num::complex::Complex64;
use rand::Rng;

mod util;
use util::{gen_rng, myrand};

#[cfg(feature = "direct_blas")]
mod direct_blas;
#[cfg(feature = "direct_blas")]
use direct_blas::{randmatstat, randmatmul, check_randmatmul};

#[cfg(not(feature = "direct_blas"))]
use ndarray::Array2;
#[cfg(not(feature = "direct_blas"))]
use util::fill_rand;
#[cfg(not(feature = "direct_blas"))]
use num::Zero;

const NITER: u32 = 5;

#[cfg(not(feature = "direct_blas"))]
fn nrand<R: Rng>(shape: (usize, usize), rng: &mut R) -> Array2<f64> {
    let mut m = Array2::zeros(shape);
    fill_rand(&mut m, rng);
    m
}

fn fib(n: i32) -> i32 {
    if n < 2 {
        n
    } else {
        fib(black_box(n - 1)) + fib(black_box(n - 2))
    }
}

fn mandel(z: Complex64) -> u32 {
    use std::iter;

    iter::repeat(z)
        .scan(z, |z, c| {
            let current = *z;
            *z = current * current + c;
            Some(current)
        })
        .take(80)
        .take_while(|z| z.norm() <= 2.0)
        .count() as u32
}

fn mandelperf() -> Vec<u32> {
    let mut m: Vec<u32> = vec![0; 21 * 26];

    for ((i, j), mut v) in (0..21)
        .flat_map(|i| (0..26).map(move |j| (i, j)))
        .zip(&mut m)
    {
        *v = mandel(Complex64::new((j - 20) as f64 / 10.,
            ((i - 10) as f64 / 10.)));
    }

    m
}

fn pisum() -> f64 {
    let mut sum = 0.;
    for _ in 0..500 {
        sum = (1..10001)
            .map(|k| {
                let k = k as f64;
                1. / (k * k)
            })
            .sum();
    }
    sum
}

#[cfg(not(feature = "direct_blas"))]
fn randmatstat(t: usize) -> (f64, f64) {
    let mut rng = gen_rng(1234u64);

    let n = 5;

    let mut v = vec![0.; t];
    let mut w = vec![0.; t];

    for (ve, we) in v.iter_mut().zip(w.iter_mut()) {
        let a = nrand((n, n), &mut rng);
        let b = nrand((n, n), &mut rng);
        let c = nrand((n, n), &mut rng);
        let d = nrand((n, n), &mut rng);
        let p = { // P = [a b c d]
            let mut p = Array2::<f64>::zeros((n, 4 * n));
            let n = n as isize;
            p.slice_mut(s![.., 0..n]).assign(&a);
            p.slice_mut(s![.., n..2*n]).assign(&b);
            p.slice_mut(s![.., 2*n..3*n]).assign(&c);
            p.slice_mut(s![.., 3*n..4*n]).assign(&d);
            p
        };
        let q = { // Q = [a b ; c d]
            let mut q = Array2::<f64>::zeros((2 * n, 2 * n));
            let n = n as isize;
            q.slice_mut(s![0..n, 0..n]).assign(&a);
            q.slice_mut(s![0..n, n..2*n]).assign(&b);
            q.slice_mut(s![n..2*n, 0..n]).assign(&c);
            q.slice_mut(s![n..2*n, n..2*n]).assign(&d);
            q
        };

        let pt = p.t();
        let ptp = pt.dot(&p);
        let ptp2 = ptp.dot(&ptp);
        let ptp4 = ptp2.dot(&ptp2);
        *ve = trace_arr(&ptp4);

        let qt = q.t();
        let ptq = qt.dot(&q);
        let ptq2 = ptq.dot(&ptq);
        let ptq4 = ptq2.dot(&ptq2);
        *we = trace_arr(&ptq4);
    }

    let (v1, v2, w1, w2) = v.iter()
        .zip(w.iter())
        .fold((0., 0., 0., 0.), |(v1, v2, w1, w2), (ve, we)| (
            v1 + *ve,
            v2 + ve * ve,
            w1 + *we,
            w2 + we * we
        ));

    let t = t as f64;

    (
        f64::sqrt((t * (t * v2 - v1 * v1)) / ((t - 1.) * v1 * v1)),
        f64::sqrt((t * (t * w2 - w1 * w1)) / ((t - 1.) * w1 * w1)),
    )
}

/// Calculate the trace of a square matrix
#[cfg(not(feature = "direct_blas"))]
#[inline]
fn trace_arr<'a, T: 'a>(m: &'a Array2<T>) -> T
where
    T: Zero + Clone
{
    m.diag().scalar_sum()
}

#[cfg(not(feature = "direct_blas"))]
fn randmatmul<R: Rng>(n: usize, mut rng: R) -> Array2<f64> {
    let a = nrand((n, n), &mut rng);
    let b = nrand((n, n), &mut rng);

    a.dot(&b)
}

#[cfg(not(feature = "direct_blas"))]
#[inline]
fn check_randmatmul(m: Array2<f64>) {
    assert!(0. <= m[[0, 0]]);
}

#[test]
fn test_quicksort() {
    let mut a = [10., 9., 8., 7., 6., 5., 4., 3., 2., 1.];
    quicksort(a.as_mut(), 0);
    assert_eq!(a, [1., 2., 3., 4., 5., 6., 7., 8., 9., 10.]);

    let mut a = [8., 2., 10., 4., 7., 6., 9., 5., 1., 3.];
    quicksort(a.as_mut(), 0);
    assert_eq!(a, [1., 2., 3., 4., 5., 6., 7., 8., 9., 10.]);
}

fn quicksort(a: &mut [f64], mut lo: usize) {
    let hi = a.len() as usize - 1;
    let mut i: usize = lo;
    // j is isize because it can be -1
    let mut j: isize = hi as isize;

    while i < hi {
        let pivot = a[(lo + hi) / 2];
        while i as isize <= j {
            while a[i] < pivot {
                i += 1;
            }
            while a[j as usize] > pivot {
                j -= 1;
            }
            if i as isize <= j {
                a.swap(i, j as usize);
                i += 1;
                j -= 1;
            }
        }

        let (l, _r) = a.split_at_mut((j + 1) as usize);

        if (lo as isize) < j {
            quicksort(l, lo);
        }

        lo = i;
        j = hi as isize;
    }
}

fn printfd(n: usize) {
    let f = OpenOptions::new()
        .write(true).open("/dev/null").unwrap();
    let mut f = BufWriter::new(f);
    for i in 0..n {
        writeln!(f, "{} {}", i, i).unwrap();
    }
}

fn print_perf(name: &str, t: f64) {
    println!("rust,{},{:.6}", name, t * 1000.);
}

/// convert duration to float in seconds
fn to_float(d: Duration) -> f64 {
    d.as_secs() as f64 + d.subsec_nanos() as f64 / 1e9
}

#[inline]
fn measure_best<F: FnMut()>(niters: u32, mut op: F) -> Duration {
    (0..niters)
        .map(move |_| {
            let t = Instant::now();
            op();
            t.elapsed()
        }).min().unwrap()
}

fn main() {
    // initialize RNG
    let mut rng = gen_rng(0);

    // fib(20)
    assert_eq!(fib(20), 6765);
    let mut f = 0i32;
    let fibarg = black_box(20);
    let tmin = measure_best(NITER, || {
        for _ in 0..1000 {
            f = f.wrapping_add(fib(fibarg));
        }
    });
    print_perf("fib", to_float(tmin) / 1000.0);

    // parse_int
    let tmin = measure_best(NITER, || {
        for _ in 0..1000 * 100 {
            let n: u32 = rng.gen();
            let s = format!("{:x}", n);
            let m = u32::from_str_radix(&s, 16).unwrap();
            assert_eq!(m, n);
        }
    });
    print_perf("parse_int", to_float(tmin) / 100.0);

    let mandel_sum_init = black_box(0u32);
    let mut mandel_sum2 = mandel_sum_init;
    let tmin = measure_best(NITER, || {
        for j in 0..100 {
            let m = mandelperf();
            if j == 0 {
                let mandel_sum: u32 = m.iter().sum();
                assert_eq!(mandel_sum, 14791);
                mandel_sum2 += mandel_sum;
            }
        }
    });
    assert_eq!(mandel_sum2, 14791 * NITER);
    print_perf("mandel", to_float(tmin) / 100.0);

    // sort
    let tmin = measure_best(NITER, || {
        let mut d = myrand(5000, &mut rng);
        quicksort(&mut d, 0);
    });
    print_perf("quicksort", to_float(tmin));

    // pi sum
    let mut pi = 0.;
    let tmin = measure_best(NITER, || {
        pi = pisum();
    });
    assert!(f64::abs(pi - 1.644834071848065) < 1e-12);
    print_perf("pi_sum", to_float(tmin));

    // rand mat stat
    let mut r = (0., 0.);
    let tmin = measure_best(NITER, || {
        r = black_box(randmatstat(1000));
    });
    print_perf("rand_mat_stat", to_float(tmin));

    // rand mat mul
    let tmin = measure_best(NITER, || {
        let c = randmatmul(1000, &mut rng);
        check_randmatmul(c);
    });
    print_perf("rand_mat_mul", to_float(tmin));

    // printfd
    let tmin = measure_best(NITER, || {
        printfd(100000);
    });
    print_perf("printfd", to_float(tmin));
}
