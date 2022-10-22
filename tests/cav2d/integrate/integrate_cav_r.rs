use approx::assert_abs_diff_eq;
use cavint::cav2d::integrate::integ_cavs_interval;
use peroxide::prelude::AD::{self, AD0};

#[test]
fn test_constant() {
    let val = integ_cavs_interval(|_| AD0(5.), |_| AD0(0.), (5., 0.), 1e-9);

    assert_abs_diff_eq!(val, -25., epsilon = 1e-9);
}

#[test]
fn test_simple() {
    let val = integ_cavs_interval(|x: AD| x + AD0(2.), |y: AD| -y, (0., 3.), 1e-9);

    assert_abs_diff_eq!(val, 21., epsilon = 1e-9);
}
