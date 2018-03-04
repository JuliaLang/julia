#![allow(unsafe_code)]

use rand::Rng;
use std::iter::Sum;
use util::{gen_rng, fill_rand, myrand};
use blas::c::{dgemm, Layout, Transpose};
use itertools::Itertools;

pub fn randmatstat(t: usize) -> (f64, f64) {
    let mut rng = gen_rng(1234u64);

    let n = 5;

    let mut v = vec![0.; t];
    let mut w = vec![0.; t];

    {
        let mut a = vec![0.; n * n];
        let mut b = vec![0.; n * n];
        let mut c = vec![0.; n * n];
        let mut d = vec![0.; n * n];
        let mut p = vec![0.; (n) * (4 * n)];
        let mut q = vec![0.; (2 * n) * (2 * n)];

        let mut pt_p1 = vec![0.; (4 * n) * (4 * n)];
        let mut pt_p2 = vec![0.; (4 * n) * (4 * n)];
        let mut qt_q1 = vec![0.; (2 * n) * (2 * n)];
        let mut qt_q2 = vec![0.; (2 * n) * (2 * n)];

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

            unsafe {
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

            *ve = trace(&pt_p1, n * 4);

            unsafe {
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

            *we = trace(&qt_q1, 2 * n);
        }
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
#[inline]
fn trace<'a, T>(m: &'a [T], n: usize) -> T
where
    T: Sum<&'a T>
{
    debug_assert_eq!(m.len(), n * n);
    m.into_iter().step(n + 1).sum()
}

pub fn randmatmul<R: Rng>(n: usize, mut rng: R) -> Vec<f64> {
    let a = myrand(n * n, &mut rng);
    let b = myrand(n * n, &mut rng);
    let mut c = vec![0.; n * n];

    unsafe {
        let n = n as i32;
        dgemm(Layout::ColumnMajor, Transpose::None, Transpose::None,
            n, n, n, 1., &a, n, &b, n, 0., &mut c, n);
    }

    c
}

#[inline]
pub fn check_randmatmul(m: Vec<f64>) {
    assert!(0. <= m[0]);
}
