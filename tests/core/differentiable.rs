use std::f64::consts::PI;

use cavint::core::{
    differentiable::{Differentiable1D, AD},
    helpers::{linspace, Signed},
};

use crate::{test_helpers::assert_float_iters_equal, setup};

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
    setup();
    test_f_df_over_interval(|x| x.powi(2), |x| x.powi(2), |x| 2f64 * x, -3f64, 3f64);
}

#[test]
fn one_over_x() {
    setup();
    test_f_df_over_interval(|x| AD::from(1f64) / x, |x| 1f64 / x, |x| -1f64/x.powi(2), 1f64, 3f64);
}

#[test]
fn ln_x_p1() {
    setup();
    test_f_df_over_interval(|x| (AD::from(1f64) + x).ln(), |x| x.ln_1p(), |x| 1f64/(1f64 + x), 0f64, 3f64);
}


#[test]
fn pow_xx() {
    setup();
    test_f_df_over_interval(
        |x| x.pow(x),
        |x| x.powf(x),
        |x| x.powf(x) * (1f64 + x.ln()),
        2f64,
        3f64,
    );
}

#[test]
fn abs_sin() {
    setup();
    test_f_df_over_interval(
        |x| x.sin().abs(),
        |x| x.sin().abs(),
        |x| x.sign_val() * x.cos(),
        -PI,
        PI,
    );
}

#[test]
fn asin() {
    setup();
    test_f_df_over_interval(
        |x| x.asin(),
        |x| x.asin(),
        |x| 1f64 / (1f64 - x.powi(2)).sqrt(),
        0.99,
        -0.99,
    );
}

#[test]
fn sqrt_plus_cos() {
    setup();
    test_f_df_over_interval(
        |x| AD::from(2f64)*x.sqrt() - x.cos(),
        |x| 2f64 *x.sqrt() - x.cos(),
        |x| 1f64 / x.sqrt() + x.sin(),
        1f64,
        2f64*PI,
    );
}

#[test]
fn tan() {
    setup();
    test_f_df_over_interval(
        |x| x.tan(),
        |x| x.tan(),
        |x| 1f64 / x.cos().powi(2),
        -PI/6f64,
        PI/6f64,
    );
}

#[test]
fn acos_atan() {
    setup();
    test_f_df_over_interval(
        |x| x.acos().atan(),
        |x| x.acos().atan(),
        |x| -1f64 / ((x.acos().powi(2) + 1f64)*(1f64 - x.powi(2)).sqrt()),
        -0.99,
        0.99,
    );
}

#[test]
fn cosh_plus_sinh() {
    setup();
    test_f_df_over_interval(
        |x| x.cosh() + x.sinh(),
        |x| x.cosh() + x.sinh(),
        |x|x.sinh() + x.cosh(),
        -2f64,
        2f64,
    );
}

#[test]
fn acosh_asinh() {
    setup();
    test_f_df_over_interval(
        |x| x.acosh() * x.asinh(),
        |x| x.acosh() * x.asinh(),
        |x| x.asinh()/ (x.powi(2) - 1f64).sqrt() + x.acosh()/ (x.powi(2) + 1f64).sqrt(),
        2f64,
        5f64,
    );
}

#[test]
fn tanh_atanh() {
    setup();
    test_f_df_over_interval(
        |x| x.tanh().atanh(),
        |x| x.tanh().atanh(),
        |_| 1f64,
        -1f64,
        1f64,
    );
}