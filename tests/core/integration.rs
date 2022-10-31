use std::f64::consts::PI;

use approx::assert_abs_diff_eq;
use cavint::core::{integrate::{
    gauss_kronrod_quadrature, gauss_kronrod_quadrature_2d, gauss_kronrod_quadrature_triangle,
}, differentiable::{AD, abs_jacobian_det}};

/* One Dimensional Integration Tests */

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
    test_fn(|_| 4f64, -1f64, 5f64, 1e-9, Some(1), 24f64)
}

#[test]
fn linear() {
    test_fn(|x| x + 2f64, 1f64, -1f64, 1e-9, Some(1), -4f64)
}

#[test]
fn oscillating() {
    test_fn(|x| x.sin(), 0f64, 1.5 * PI, 1e-9, Some(100), 1.0)
}

#[test]
fn outside_domain() {
    assert!(gauss_kronrod_quadrature(|x| x.ln(), -1f64, 1f64, 1e-9, Some(100)).is_err(),)
}

#[test]
fn iter_restriction() {
    assert!(gauss_kronrod_quadrature(
        |x| x.powi(3).sin() - (x - 0.5).powi(4).sin(),
        2f64,
        20f64,
        1e-9,
        Some(1)
    )
    .is_err(),)
}

/* Two Dimensional Itegration Tests */

fn test_fn_2d(
    f: impl Fn([f64; 2]) -> f64,
    a: f64,
    b: f64,
    inner_ab_fn: impl Fn(f64) -> [f64; 2],
    tol: f64,
    max_iter: Option<usize>,
    true_res: f64,
) {
    let (res, err) = gauss_kronrod_quadrature_2d(f, a, b, inner_ab_fn, tol, max_iter).unwrap();

    assert!(err < tol);
    assert_abs_diff_eq!(res, true_res, epsilon = tol);
}

#[test]
fn prismatoid_bot() {
    let f = |[x, y]: [f64; 2]| -x - y + 8f64;
    let h = |[x, y]: [f64; 2]| [0.8 * x - 0.2 * y + 1.6, -0.4 * x + 0.6 * y + 3.2];
    test_fn_2d(
        |xy| f(h(xy)),
        0f64,
        4f64,
        |_| [0f64, 4f64],
        1e-11,
        Some(100),
        25.6,
    )
}

#[test]
fn prismatoid_top() {
    let f = |[x, y]: [AD; 2]| -x - y + AD(8f64, 0f64);
    let c = |z: AD| [z/AD(2f64, 0f64), z];
    let g = |x: [AD; 2]| {
        let mut cfx = c(f(x));
        cfx[0] = x[0] - cfx[0];
        cfx[1] = x[1] - cfx[1];
        cfx
    };
    let g_abs_jdet = move |xy: [f64; 2]| abs_jacobian_det(g, xy);
    test_fn_2d(
        |xy| f(xy.map(|v| v.into())).0 * g_abs_jdet(xy),
        0.8,
        4.8,
        |x| [
            3.2 + if x < 1.6 {
                -3f64 * (x - 1.6)
            } else {
                -0.5 * (x - 1.6)
            },
            4f64 + if x < 4f64 {
                -0.5 * (x - 4f64)
            } else {
                -3f64 * (x - 4f64)
            }
        ],
        1e-11,
        Some(100),
        25.6,
    )
}

#[test]
fn prismatoid_top2() {
    let f = |[u, v]: [f64; 2]| 8f64 - 0.8 * u - 0.2 * v;
    test_fn_2d(
        f,
        4f64,
        6f64,
        |_| [
           8f64,
           16f64
        ],
        1e-11,
        Some(100),
        25.6,
    )
}


/* Triangle Integation Tests */

fn test_fn_triag(
    f: impl Fn([f64; 2]) -> f64,
    triag: [[f64; 2]; 3],
    tol: f64,
    max_iter: Option<usize>,
    true_res: f64,
) {
    let (res, err) = gauss_kronrod_quadrature_triangle(f, triag, tol, max_iter).unwrap();

    assert!(err < tol);
    assert_abs_diff_eq!(res, true_res, epsilon = tol);
}

fn rotate_triag_clockwise(triag: [[f64; 2]; 3], rad: f64) -> [[f64; 2]; 3] {
    triag.map(|[x, y]| [x * rad.cos() + y * rad.sin(), y * rad.cos() - x * rad.sin()])
}

#[test]
fn constant_triag() {
    test_fn_triag(
        |_| 4f64,
        [[0f64, 0f64], [1f64, 0f64], [0f64, 1f64]],
        1e-9,
        Some(10),
        2f64,
    )
}

#[test]
fn rotated_triag() {
    test_fn_triag(
        |_| 4f64,
        rotate_triag_clockwise([[0f64, 0f64], [1f64, 0f64], [0f64, 1f64]], 1.1),
        1e-9,
        Some(10),
        2f64,
    )
}

#[test]
fn linear_x_triag() {
    test_fn_triag(
        |[x, _]| x,
        [[-1f64, 0f64], [1f64, 0f64], [0f64, 1f64]],
        1e-9,
        Some(10),
        0f64,
    )
}

#[test]
fn linear_y_triag() {
    test_fn_triag(
        |[_, y]| y,
        [[-1f64, 0f64], [1f64, 0f64], [0f64, 1f64]],
        1e-9,
        Some(10),
        1f64 / 3f64,
    )
}
