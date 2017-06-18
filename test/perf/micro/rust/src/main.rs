//! This is a Rust implementation of the Julia micro benchmark.
//! 
//! This program is based on the C implementation, but it's still mostly
//! idiomatic Rust, and does not have unsafe code blocks.
//! 
//! This project requires a **nightly** version of the Rust compiler and
//! its package manager, Cargo. When using `rustup`, installing the
//! nightly toolchain and executing `rustup override set nightly` on
//! this directory is sufficient.
#![feature(test)]
#![deny(unsafe_code)]

extern crate blas;
extern crate test;
extern crate num;
extern crate rand;
extern crate mersenne_twister;

use std::time::{Duration, Instant};
use std::u32;
use std::fs::OpenOptions;
use std::io::{BufWriter, Write};

use blas::c::{dgemm, Layout, Transpose};
use test::black_box;
use num::complex::Complex64;
use rand::{SeedableRng, Rng};
use mersenne_twister::MT19937_64;

type MTRng = MT19937_64;

#[inline]
fn gen_rng(seed: u64) -> MTRng {
    MTRng::from_seed(seed)
}

const NITER: u32 = 5;

fn fill_rand<R: Rng>(a: &mut [f64], rng: &mut R) {
    for v in a {
        *v = rng.gen();
    }
}

fn myrand<R: Rng>(n: usize, rng: &mut R) -> Vec<f64> {
    let mut d: Vec<f64> = vec![0.; n];
    fill_rand(&mut d, rng);
    d
}

fn fib(n: i32) -> i32 {
    if n < 2 {
        n
    } else {
        fib(black_box(n - 1)) + fib(black_box(n - 2))
    }
}

fn mandel(mut z: Complex64) -> u32 {
    let maxiter = 80;
    let c = z.clone();
    for n in 0..maxiter {
        if z.norm() > 2.0 {
            return n;
        }
        z = z * z + c;
    }

    maxiter
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

fn randmatstat(t: usize) -> (f64, f64) {
    let mut rng = gen_rng(1234u64);

    let n = 5;

    let mut v = vec![0.; t];
    let mut w = vec![0.; t];

    {
        let mut a = vec![0.; n * n];
        let mut b = vec![0.; n * n];
        let mut c = vec![0.; n * n];
        let mut d = vec![0.; n * n];
        let mut p = vec![0.; n * 4 * n];
        let mut q = vec![0.; 2 * n * 2 * n];

        let mut pt_p1 = vec![0.; 4 * n * 4 * n];
        let mut pt_p2 = vec![0.; 4 * n * 4 * n];
        let mut qt_q1 = vec![0.; 2 * n * 2 * n];
        let mut qt_q2 = vec![0.; 2 * n * 2 * n];

        for (ve, we) in v.iter_mut().zip(w.iter_mut()) {
            fill_rand(&mut a, &mut rng);
            fill_rand(&mut b, &mut rng);
            fill_rand(&mut c, &mut rng);
            fill_rand(&mut d, &mut rng);

            p[0 .. n * n].copy_from_slice(&a);
            p[n * n .. 2 * n * n].copy_from_slice(&b);
            p[2 * n * n .. 3 * n * n].copy_from_slice(&c);
            p[3 * n * n .. 4 * n * n].copy_from_slice(&d);

            for j in 0..n {
                for k in 0..n {
                    q[2 * n * j + k] = a[k];
                    q[2 * n * j + n + k] = b[k];
                    q[2 * n * (n + j) + k] = c[k];
                    q[2 * n * (n + j) + n + k] = d[k];
                }
            }

            {
                let n = n as i32;

                dgemm(Layout::ColumnMajor, Transpose::Ordinary, Transpose::None,
                    n , n, 4 * n, 1., &p, 4 * n, &p, 4 * n, 0.,
                    &mut pt_p1, 4 * n);
                dgemm(Layout::ColumnMajor, Transpose::None, Transpose::None,
                    4 * n, 4 * n, 4 * n, 1., &pt_p1, 4 * n, &pt_p1, 4 * n, 0.,
                    &mut pt_p2, 4 * n);
                dgemm(Layout::ColumnMajor, Transpose::None, Transpose::None,
                    4 * n, 4 * n, 4 * n, 1., &pt_p2, 4 * n, &pt_p2, 4 * n, 0.,
                    &mut pt_p1, 4 * n);
            }

            for j in 0..n {
                *ve += pt_p1[(n + 1) * j];
            }

            {
                let n = n as i32;

                dgemm(Layout::ColumnMajor, Transpose::Ordinary, Transpose::None,
                    2 * n, 2 * n, 2 * n, 1., &q, 2 * n, &q, 2 * n, 0.,
                    &mut qt_q1, 2 * n);
                dgemm(Layout::ColumnMajor, Transpose::None, Transpose::None,
                    2 * n, 2 * n, 2 * n, 1., &qt_q1, 2 * n, &qt_q1, 2 * n, 0.,
                    &mut qt_q2, 2 * n);
                dgemm(Layout::ColumnMajor, Transpose::None, Transpose::None,
                    2 * n, 2 * n, 2 * n, 1., &qt_q2, 2 * n, &qt_q2, 2 * n, 0.,
                    &mut qt_q1, 2 * n);
            }

            for j in 0..n {
                *we += qt_q1[(2 * n + 1) * j];
            }
        }
    }

    let (mut v1, mut v2, mut w1, mut w2) = (0., 0., 0., 0.);

    for (ve, we) in v.iter().zip(w.iter()) {
        v1 += *ve; v2 += ve * ve;
        w1 += *we; w2 += we * we;
    }

    let t = t as f64;

    (
        f64::sqrt((t * (t * v2 - v1 * v1)) / ((t - 1.) * v1 * v1)),
        f64::sqrt((t * (t * w2 - w1 * w1)) / ((t - 1.) * w1 * w1)),
    )
}

fn randmatmul<R: Rng>(n: usize, mut rng: R) -> Vec<f64> {
    let a = myrand(n * n, &mut rng);
    let b = myrand(n * n, &mut rng);
    let mut c = vec![0.; n * n];

    let n = n as i32;
    dgemm(Layout::ColumnMajor, Transpose::None, Transpose::None,
        n, n, n, 1., &a, n, &b, n, 0., &mut c, n);

    c
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

fn quicksort(mut a: &mut [f64], mut lo: usize) {
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
            let m: u32 = u32::from_str_radix(&s, 16).unwrap();
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
        assert!(0. <= c[0]);
    });
    print_perf("rand_mat_mul", to_float(tmin));

    // printfd
    let tmin = measure_best(NITER, || {
        printfd(100000);
    });
    print_perf("printfd", to_float(tmin));
}
