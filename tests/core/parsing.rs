use std::f64::consts::PI;

use approx::assert_abs_diff_eq;
use cavint::core::parsing::{compile_expression, DefaultContext};

fn test_x(expr: &str, input: f64, output: f64) {
    let mut context = DefaultContext::default();
    context.add_var("x", 0);

    let expr = compile_expression(expr, context).unwrap();
    assert_abs_diff_eq!(expr.eval(&[input]), output, epsilon = 1e-13)
}

fn test_xy(expr: &str, input: [f64; 2], output: f64) {
    let mut context = DefaultContext::default();
    context.add_var("x", 0);
    context.add_var("y", 1);

    let expr = compile_expression(expr, context).unwrap();
    assert_abs_diff_eq!(expr.eval(&input), output, epsilon = 1e-13)
}

#[test]
fn basic_expression() {
    test_x("x**2 - 5", 2f64, -1f64)
}

#[test]
fn signs() {
    test_x("-x*-2 +(-5)", 2f64, -1f64)
}

#[test]
fn named_elements() {
    test_x("sin((2*x)/ 4) - ln(e)", PI, 0f64)
}

#[test]
fn two_vars() {
    test_xy("cos((2*x*y)/4)", [PI, 2f64], -1f64)
}

#[test]
fn exponents() {
    test_xy("ln(e^(x+y**0))", [PI, 2f64], PI + 1f64)
}
#[test]
fn break_me() {
    test_xy(
        "ln(-(-(e^(x+y**0)))) / y / -cos(0)",
        [PI, 2f64],
        -(PI + 1f64) / 2f64,
    )
}
