use std::{f64::consts::PI, mem::swap};

use approx::assert_abs_diff_eq;
use cavint::{
    core::{
        differentiable::{Differentiable1D, AD, ONE},
        helpers::vec_from_res,
    },
    pyo3_wrappers::{display_cav2d, display_cav2d_rs},
};
use log::debug;

use crate::{
    setup,
    test_helpers::{assert_float_iters_equal, IteratorMean},
};

/* 2D Cavalieri Tests */

fn test_c2d(
    f_expr: &str,
    c_expr: &str,
    intervals_expr: &str,
    f: impl Fn(AD) -> AD,
    c: impl Fn(AD) -> AD,
    integ_over_monotone_intervals: Vec<([f64; 2], f64)>,
    tol: f64,
) {
    let displays = display_cav2d(
        f_expr.to_string(),
        c_expr.to_string(),
        intervals_expr.to_string(),
        true,
        50,
        50,
        1,
        100,
        100,
        tol,
    )
    .expect("Expected displays");

    assert_eq!(displays.len(), integ_over_monotone_intervals.len());

    let g = |x: AD| x - c(f(x));

    displays
        .into_iter()
        .zip(integ_over_monotone_intervals.into_iter())
        .for_each(|(display, (monotone_interval, expected_integ_val))| {
            debug!("INTERVAL: {:?}", &monotone_interval);
            assert_abs_diff_eq!(display.a, monotone_interval[0], epsilon = tol);
            assert_abs_diff_eq!(display.b, monotone_interval[1], epsilon = tol);

            assert_abs_diff_eq!(
                display.integ_value.expect("Expected integration value").0,
                expected_integ_val,
                epsilon = tol
            );

            let expected_xv = vec_from_res(monotone_interval[0], monotone_interval[1], 50);
            debug!("xv assertions");
            assert_float_iters_equal(display.xv.into_iter(), expected_xv.iter().copied(), tol);

            debug!("f assertions");
            assert_float_iters_equal(
                display.fv.into_iter(),
                expected_xv.iter().map(|x| f.f(*x)),
                tol,
            );

            debug!("g assertions");
            assert_float_iters_equal(
                display.gv.into_iter(),
                expected_xv.iter().map(|x| g.f(*x)),
                tol,
            );

            debug!("dg assertions");
            assert_float_iters_equal(
                display.dgv.into_iter(),
                expected_xv.iter().map(|x| g.df(*x)),
                tol,
            );

            let yr = vec_from_res(0f64, 1f64, 50);

            debug!("cvs assertions");
            assert_eq!(display.cvs.len(), 3);
            for ((xv_index, cv), expected_xv_index) in
                display.cvs.into_iter().zip(vec![0, 25, 50].into_iter())
            {
                assert_eq!(xv_index, expected_xv_index);
                let k = g.f(expected_xv[expected_xv_index]);
                let fx = f.f(expected_xv[expected_xv_index]);

                let expected_yv = yr.iter().map(|r| *r * fx).collect::<Vec<_>>();
                assert_float_iters_equal(cv.iter().map(|v| v[0]), expected_yv.iter().copied(), tol);
                assert_float_iters_equal(
                    cv.iter().map(|v| v[1]),
                    expected_yv.iter().map(|y| c((*y).into()).0 + k),
                    tol,
                );
            }
        });
}

#[test]
fn c2d_constant() {
    setup();
    test_c2d(
        "4",
        "0",
        "[0, 1]",
        |_x| 4f64.into(),
        |_y| 0f64.into(),
        vec![([0f64, 1f64], 4f64)],
        1e-9,
    );
}

#[test]
fn c2d_parabola() {
    setup();
    let anti_derivative = |x: f64| x.powi(4) / 2f64 - x.powi(3) / 3f64 - x.powi(2) + x;
    test_c2d(
        "-x**2 + 1",
        "-y",
        "[1.23, -0.27]",
        |x| -x.powi(2) + 1f64.into(),
        |y| -y,
        vec![
            ([1.23, 0.5], anti_derivative(0.5) - anti_derivative(1.23)),
            ([0.5, -0.27], anti_derivative(-0.27) - anti_derivative(0.5)),
        ],
        1e-9,
    );
}

/* Riemann Stieltjes Tests */

fn test_rs2d(
    f_expr: &str,
    g_expr: &str,
    intervals_expr: &str,
    f: impl Fn(AD) -> AD,
    c_integ_over_monotone_intervals: Vec<(Box<dyn Fn(AD) -> AD>, [f64; 2], f64)>,
    tol: f64,
) {
    let displays = display_cav2d_rs(
        f_expr.to_string(),
        g_expr.to_string(),
        intervals_expr.to_string(),
        true,
        50,
        50,
        1,
        100,
        100,
        tol,
    )
    .expect("Expected displays");

    assert_eq!(displays.len(), c_integ_over_monotone_intervals.len());
    displays
        .into_iter()
        .zip(c_integ_over_monotone_intervals.into_iter())
        .for_each(|(display, (c, monotone_interval, expected_integ_val))| {
            debug!("INTERVAL: {:?}", &monotone_interval);
            assert_abs_diff_eq!(display.a, monotone_interval[0], epsilon = tol);
            assert_abs_diff_eq!(display.b, monotone_interval[1], epsilon = tol);

            assert_abs_diff_eq!(
                display.integ_value.expect("Expected integration value").0,
                expected_integ_val,
                epsilon = tol
            );

            let expected_xv = vec_from_res(monotone_interval[0], monotone_interval[1], 50);

            debug!("xv assertions");
            assert_float_iters_equal(display.xv.iter().copied(), expected_xv.iter().copied(), tol);

            debug!("f assertions");
            assert_float_iters_equal(
                display.fv.into_iter(),
                expected_xv.iter().map(|x| f((*x).into()).0),
                tol,
            );

            debug!("g assertions");
            let tilde_g = |x: AD| x - c(f(x));
            let k = display.gv[1..display.gv.len() - 1]
                .iter()
                .copied()
                .zip(display.xv[1..display.xv.len() - 1].iter().copied())
                .map(|(x_r, x_s)| x_r - tilde_g.f(x_s))
                .mean();

            debug!("k: {}", k);
            assert_float_iters_equal(
                display.gv[1..display.gv.len() - 1].iter().copied(),
                expected_xv[1..expected_xv.len() - 1]
                    .iter()
                    .map(|x| tilde_g.f(*x) + k),
                tol,
            );

            debug!("dg assertions");
            assert_float_iters_equal(
                display.dgv[1..display.dgv.len() - 1].iter().copied(),
                expected_xv[1..expected_xv.len() - 1]
                    .iter()
                    .map(|x| tilde_g.df(*x)),
                tol,
            );

            let yr = vec_from_res(0f64, 1f64, 50);

            let x_sign = (monotone_interval[1] - monotone_interval[0]).signum();
            let mut y_a = f.f(monotone_interval[0] + x_sign * 2f64 * tol);
            let mut y_b = f.f(monotone_interval[1] - x_sign * 2f64 * tol);
            if y_a > y_b {
                swap(&mut y_a, &mut y_b);
            }

            debug!("cvs assertions");
            assert_eq!(display.cvs.len(), 3);
            for ((xv_index, cv), (i, expected_xv_index)) in display
                .cvs
                .into_iter()
                .zip(vec![0, 25, 50].into_iter().enumerate())
            {
                debug!(" - c curve no. {}", i);
                assert_eq!(xv_index, expected_xv_index);

                // Finite x-axis intersection
                assert!(cv[0][1].is_finite());

                let true_ck_k_val = cv
                    .iter()
                    .filter(|[y, _]| y_a < *y && *y < y_b)
                    .map(|&[y, x]| x - c.f(y))
                    .mean();

                let fx = f.f(expected_xv[expected_xv_index]);
                let expected_yv = yr.iter().map(|r| *r * fx).collect::<Vec<_>>();
                assert_float_iters_equal(cv.iter().map(|v| v[0]), expected_yv.iter().copied(), tol);

                let (internal_c_x, expected_internal_c_x): (Vec<f64>, Vec<f64>) = cv
                    .iter()
                    .filter(|[y, _]| y_a < *y && *y < y_b)
                    .map(|[y, x]| (*x, c.f(*y) + true_ck_k_val))
                    .unzip();

                assert_float_iters_equal(
                    internal_c_x.into_iter(),
                    expected_internal_c_x.into_iter(),
                    2f64 * tol, // Additional allowance. Multiple computations at `tol` precision.
                );
            }
        });
}

#[test]
fn rs2d_linear() {
    setup();
    test_rs2d(
        "4*x",
        "-x",
        "[0, 1]",
        |x| x * 4f64.into(),
        vec![(Box::new(|y: AD| AD(0.5, 0f64) * y), [0f64, 1f64], -2f64)],
        1e-9,
    );
}

#[test]
fn rs2d_parabola() {
    setup();
    let f_inv1 = |y: AD| AD::sqrt(ONE - y);
    let c1 = move |y: AD| f_inv1(y) - f_inv1(y).powi(2);
    let c2 = move |y: AD| -f_inv1(y) - f_inv1(y).powi(2);
    let anti_derivative = |x: f64| -x.powi(4) / 2f64 + x.powi(2);
    test_rs2d(
        "-x**2 + 1",
        "x**2",
        "[1, -1]",
        |x| -x.powi(2) + 1f64.into(),
        vec![
            (
                Box::new(c1),
                [1f64, 0f64],
                anti_derivative(0f64) - anti_derivative(1f64),
            ),
            (
                Box::new(c2),
                [0f64, -1f64],
                anti_derivative(-1f64) - anti_derivative(0f64),
            ),
        ],
        1e-9,
    );
}

#[test]
fn rs2d_parabola_above() {
    setup();
    let f_inv1 = |y: AD| AD::sqrt(AD::from(3f64) - y);
    let c1 = move |y: AD| f_inv1(y) + AD::from(3f64) * f_inv1(y);
    let c2 = move |y: AD| -f_inv1(y) - AD::from(3f64) * f_inv1(y);
    let anti_derivative = |x: f64| x.powi(3) - 9f64 * x;
    test_rs2d(
        "-x**2 +3",
        "-3*x",
        "[-1.53, 1.5]",
        |x| -x.powi(2) + 3f64.into(),
        vec![
            (
                Box::new(c2),
                [-1.53, 0f64],
                anti_derivative(0f64) - anti_derivative(-1.53),
            ),
            (
                Box::new(c1),
                [0f64, 1.5],
                anti_derivative(1.5) - anti_derivative(0f64),
            ),
        ],
        1e-10,
    );
}

#[test]
fn rs2d_sine_below() {
    setup();
    let f_inv = |y: AD| AD::asin(AD::from(1f64) + y);
    let c = move |y: AD| f_inv(y) - f_inv(y).powi(2);
    let anti_derivative = |x: f64| 2f64 * (x.sin() - x * x.cos()) - x.powi(2);
    test_rs2d(
        "sin(x) - 1",
        "x**2",
        "[0, pi/4]",
        |x| x.sin() - AD::from(1f64),
        vec![(
            Box::new(c),
            [0f64, PI / 4f64],
            anti_derivative(PI / 4f64) - anti_derivative(0f64),
        )],
        1e-10,
    );
}
