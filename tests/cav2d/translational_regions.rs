use approx::assert_abs_diff_eq;
use cavint::{
    cav2d::display::split_translational,
    core::{differentiable::AD, helpers::Signed},
};
use peroxide::prelude::linspace;

#[test]
fn test_multiple_roots() {
    let f = |x: AD| x.powi(3);

    let g = |x: AD| (x - AD(0.5f64, 0f64)) * (x + AD(0.5f64, 0f64));
    let finv = |y: f64| y.sign_val() * y.abs().powf(1f64 / 3f64);
    let _cc = |x: f64, y: f64| {
        if x >= 0f64 {
            finv(y) - g(AD(y, 0f64)).0 * finv(y)
        } else {
            finv(y) + g(AD(y, 0f64)).0 * finv(y)
        }
    };
    let a = -3f64;
    let b = 5f64;
    let xv = linspace(a, b, 100);
    let roots = split_translational(f, g, &xv[..], 1e-9, 100).unwrap();

    assert_eq!(roots.len(), 1);
    assert_abs_diff_eq!(roots[0], 0.0, epsilon = 1e-9);
}
