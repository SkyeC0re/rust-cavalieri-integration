use std::f64::consts::PI;

use approx::assert_abs_diff_eq;
use cavint::core::parsing::{
    compile_expression, compile_interval_list, compile_polygon_set, DefaultContext,
};

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

/* Expression testing */

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

/* Interval set tests */

#[test]
fn single_interval() {
    let expected = vec![[0f64, 1f64]];
    let actual = compile_interval_list("[0,  1]", DefaultContext::default()).unwrap();

    assert_eq!(expected, actual)
}

#[test]
fn multiple_intervals() {
    let expected = vec![[0f64, 1f64], [2f64, -1f64]];
    let actual = compile_interval_list("[0,  1], [2, -1]", DefaultContext::default()).unwrap();

    assert_eq!(expected, actual)
}

/* Polygon set tests */

#[test]
fn single_polgyonl() {
    let expected = vec![vec![[0f64, 1f64], [2f64, 3f64], [4f64, 5f64]]];
    let actual = compile_polygon_set("[[0,  1], [2,3], [4, 5]]", DefaultContext::default()).unwrap();

    assert_eq!(expected, actual)
}

#[test]
fn multiple_polygons() {
    let expected = vec![
        vec![[0f64, 1f64], [2f64, 3f64], [4f64, 5f64]],
        vec![[6f64, 7f64], [8f64, 9f64], [10f64, 11f64]],
    ];
    let actual =
        compile_polygon_set("[[0, 1], [2, 3], [4, 5]],[[6, 7], [8, 9], [10, 11]]", DefaultContext::default()).unwrap();

    assert_eq!(expected, actual)
}
