use approx::assert_abs_diff_eq;
use cavint::cav2d::display::split_strictly_monotone;
use peroxide::prelude::{PowOps, linspace, AD::AD0};



#[test]
fn test_saddle() {
  println!("{:?}", &linspace(-10f64, 10f64, 100));
    let roots = split_strictly_monotone(
        |x| AD0(4f64) + (x - AD0(1f64)).pow(3),
        &linspace(-10f64, 10f64, 100),
          1e-9, 100,
        ).unwrap();
    
    assert_eq!(roots.len(), 0);
}

#[test]
fn test_multiple_roots() {
    let roots = split_strictly_monotone(
        |x| x.powi(3)/3f64 - 0.25*x,
        &linspace(-10f64, 10f64, 100),
          1e-9, 100,
        ).unwrap();
    
    assert_abs_diff_eq!(roots[0], -0.5, epsilon=1e-9);
    assert_abs_diff_eq!(roots[1], 0.5, epsilon=1e-9);

}