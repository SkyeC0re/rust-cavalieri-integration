use approx::assert_abs_diff_eq;
use peroxide::prelude::{AD::{AD0, self}, ADFn, linspace};
use rust_cavalieri_integration::cav2d::integrate::cavs_interval;

#[test]
fn test_constant() {
    let val = cavs_interval(
        ADFn::new(|_| AD0(5.)),
        ADFn::new(|_| AD0(0.)),
        (5., 0.),
        1e-9,
    );

    assert_abs_diff_eq!(val, -25., epsilon=1e-9);
}

#[test]
fn test_simple() {
    let val = cavs_interval(
        ADFn::new(|x: AD| x + AD0(2.)),
        ADFn::new(|y: AD| -y),
        (0., 3.),
        1e-9,
    );

    assert_abs_diff_eq!(val, 21., epsilon=1e-9);
}