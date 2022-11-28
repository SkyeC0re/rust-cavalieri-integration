use std::f64::consts::PI;

use cavint::core::{
    differentiable::{Differentiable1D, AD},
    helpers::{linspace, Signed},
};

use crate::test_helpers::assert_float_iters_equal;

fn test_f_df_over_interval(
    ad_f: impl Fn(AD) -> AD,
    f: impl Fn(f64) -> f64,
    df: impl Fn(f64) -> f64,
    a: f64,
    b: f64,
) {
    let xv = linspace(a, b, 67);

    let fdf = xv.iter().map(|&x| ad_f.fdf(x)).collect::<Vec<_>>();
    assert_float_iters_equal(fdf.iter().map(|(y, _)| *y), xv.iter().map(|&x| f(x)), 1e-13);
    assert_float_iters_equal(
        fdf.iter().map(|(_, dy)| *dy),
        xv.iter().map(|&x| df(x)),
        1e-13,
    );
}

#[test]
fn pow2() {
    test_f_df_over_interval(|x| x.powi(2), |x| x.powi(2), |x| 2f64 * x, -3f64, 3f64);
}

#[test]
fn pow_xx() {
    test_f_df_over_interval(
        |x| x.pow(x),
        |x| x.powf(x),
        |x| x.powf(x) * (1f64 + x.ln()),
        2f64,
        3f64,
    );
}

#[test]
fn abs_sin_asin() {
    test_f_df_over_interval(
        |x| x.sin().abs(),
        |x| x.sin().abs(),
        |x| x.sign_val() * x.cos(),
        -PI,
        PI,
    );
}
