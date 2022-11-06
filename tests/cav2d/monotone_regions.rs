use approx::assert_abs_diff_eq;
use cavint::{
    cav2d::display::split_strictly_monotone,
    core::differentiable::{AD, ONE},
};
use peroxide::prelude::linspace;

#[test]
fn test_saddle() {
    let roots = split_strictly_monotone(
        &|x: AD| AD(4f64, 0f64) + (x - AD(1f64, 0f64)).powi(3),
        &linspace(-10f64, 10f64, 100),
        1e-9,
        100,
    )
    .unwrap();

    assert_eq!(roots.len(), 0);
}

#[test]
fn test_saddle2() {
    let roots =
        split_strictly_monotone(&|x: AD| x.powi(3), &linspace(-10f64, 10f64, 100), 1e-9, 100).unwrap();

    assert_eq!(roots.len(), 0);
}

#[test]
fn test_single_root() {
    let roots = split_strictly_monotone(
        &|x| (x + ONE) * (x - ONE),
        &linspace(-10f64, 10f64, 79),
        1e-9,
        100,
    )
    .unwrap();

    assert_abs_diff_eq!(roots[0], 0f64, epsilon = 1e-9);
}

#[test]
fn test_multiple_roots() {
    let roots = split_strictly_monotone(
        &|x: AD| x.powi(3) / AD(3f64, 0f64) - AD(0.25, 0f64) * x,
        &linspace(-10f64, 10f64, 100),
        1e-9,
        100,
    )
    .unwrap();

    assert_abs_diff_eq!(roots[0], -0.5, epsilon = 1e-9);
    assert_abs_diff_eq!(roots[1], 0.5, epsilon = 1e-9);
}
