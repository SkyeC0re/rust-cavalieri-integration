use approx::assert_abs_diff_eq;
use cavint::{
    cav2d::display::split_strictly_monotone,
    core::{
        differentiable::{Differentiable1D, AD, ONE},
        helpers::linspace,
    },
};

fn standard_test(
    f: &dyn Differentiable1D,
    a: f64,
    b: f64,
    points: usize,
    tol: f64,
    expected_roots: Vec<f64>,
) {
    let roots = split_strictly_monotone(f, &linspace(a, b, points), tol, 500).unwrap();

    assert_eq!(roots.len(), expected_roots.len());
    for (ar, er) in roots.into_iter().zip(expected_roots.into_iter()) {
        assert_abs_diff_eq!(ar, er, epsilon = tol)
    }
}

use crate::setup;
#[test]
fn test_saddle() {
    setup();
    standard_test(
        &|x: AD| AD(4f64, 0f64) + (x - AD(1f64, 0f64)).powi(3),
        -10f64,
        10f64,
        100,
        1e-9,
        vec![],
    );
}

#[test]
fn test_saddle2() {
    setup();
    standard_test(&|x: AD| x.powi(3), -10f64, 10f64, 100, 1e-9, vec![]);
}

#[test]
fn test_single_root() {
    setup();

    standard_test(
        &|x| (x + ONE) * (x - ONE),
        -10f64,
        10f64,
        79,
        1e-9,
        vec![0f64],
    );
}

#[test]
fn test_multiple_roots() {
    setup();

    standard_test(
        &|x: AD| x.powi(3) / AD(3f64, 0f64) - AD(0.25, 0f64) * x,
        -10f64,
        10f64,
        100,
        1e-9,
        vec![-0.5, 0.5],
    );
}

#[test]
fn singularity() {
    setup();

    standard_test(
        &|x: AD| ONE / x.abs().sqrt() + 10f64.into(),
        -1f64,
        1f64,
        100,
        1e-9,
        vec![0.0],
    );
}
