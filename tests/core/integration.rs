use std::f64::consts::PI;

use approx::assert_abs_diff_eq;
use cavint::{core::integrate::gauss_kronrod_quadrature, errors::IntegError};

fn test_fn(
    f: impl Fn(f64) -> f64,
    a: f64,
    b: f64,
    tol: f64,
    max_iter: Option<usize>,
    true_res: f64,
) {
    let (res, err) = gauss_kronrod_quadrature(f, a, b, tol, max_iter).unwrap();

    assert!(err < tol);
    assert_abs_diff_eq!(res, true_res, epsilon = tol);
}

#[test]
fn constant() {
    test_fn(|_| 4f64, -1f64, 5f64, 1e-9, Some(10), 24f64)
}

#[test]
fn linear() {
    test_fn(|x| x + 2f64, 1f64, -1f64, 1e-9, Some(10), -4f64)
}

#[test]
fn oscillating() {
    test_fn(|x| x.sin(), 0f64, 1.5 * PI, 1e-9, Some(100), 1.0)
}


#[test]
fn outside_domain() {
    assert!(
        gauss_kronrod_quadrature(|x| x.ln(), -1f64, 1f64, 1e-9, Some(100)).is_err(),
    )
}

#[test]
fn iter_restriction() {
    assert!(
        gauss_kronrod_quadrature(|x| x.powi(3).sin() - (x - 0.5).powi(4).sin(), 2f64, 20f64, 1e-9, Some(1)).is_err(),
    )
}
