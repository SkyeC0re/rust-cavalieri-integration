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

fn test_safe_is_standard_xy(expr: &str, input: [f64; 2]) {
    let mut context = DefaultContext::default();
    context.add_var("x", 0);
    context.add_var("y", 1);

    let expr = compile_expression(expr, context).unwrap();

    assert_eq!(expr.eval(&input), expr.safe_eval(&input).unwrap());
}

/* Expression testing */

#[test]
fn basic_expression() {
    test_x("x**2 - 5", 2f64, -1f64);
}

#[test]
fn signs() {
    test_x("-x*-2 +(-5)", 2f64, -1f64);
}

#[test]
fn named_elements() {
    test_x("sin((2*x)/ 4) - ln(e)", PI, 0f64);
}

#[test]
fn two_vars() {
    test_xy("cos((2*x*y)/4)", [PI, 2f64], -1f64);
}

#[test]
fn exponents() {
    test_xy("ln(e^(x+y**0))", [PI, 2f64], PI + 1f64);
}
#[test]
fn break_me() {
    test_xy(
        "ln(-(-(e^(x+y**0)))) / y / -cos(0)",
        [PI, 2f64],
        -(PI + 1f64) / 2f64,
    );
}

#[test]
fn break_me_safe_is_standard() {
    test_safe_is_standard_xy("ln(-(-(e^(x+y**0)))) / y / -cos(0)", [PI, 2f64]);
}

#[test]
fn constant_expression() {
    let context = DefaultContext::default();
    assert_abs_diff_eq!(
        compile_expression::<0, f64>("(1+2)", &context)
            .unwrap()
            .eval(&[]),
        3f64,
        epsilon = 1e-13
    );
}

#[test]
fn missing_name() {
    let context = DefaultContext::default();
    assert!(compile_expression::<2, f64>("x", &context).is_err());
}

#[test]
fn out_of_bounds_variable() {
    let mut context = DefaultContext::default();
    context.add_var("x", 2);
    assert!(compile_expression::<2, f64>("x", &context).is_err());
}

#[test]
fn repeated_negation() {
    let mut context = DefaultContext::default();
    context.add_var("x", 0);
    assert!(compile_expression::<2, f64>("--x", &context).is_err());
}

#[test]
fn trailing_residue() {
    let context = DefaultContext::default();
    assert!(compile_expression::<2, f64>("1+2)", &context).is_err());
}

#[test]
fn missing_closing_parenthesis() {
    let context = DefaultContext::default();
    assert!(compile_expression::<2, f64>("(1+2", &context).is_err());
}

#[test]
fn missing_expression() {
    let context = DefaultContext::default();
    assert!(compile_expression::<2, f64>("", &context).is_err());
}

#[test]
fn missing_function_parenthesis() {
    let mut context = DefaultContext::default();
    let f = |x: f64| x;
    context.add_func("f", &f);
    assert!(compile_expression::<2, f64>("f5", &context).is_err());
    assert!(compile_expression::<2, f64>("f(5", &context).is_err());
}

#[test]
fn missing_term() {
    let context = DefaultContext::default();
    assert!(compile_expression::<2, f64>("10^", &context).is_err());
}

#[test]
fn invalid_element() {
    let context = DefaultContext::default();
    assert!(compile_expression::<2, f64>("4+?", &context).is_err());
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

#[test]
fn missing_interval_brackets() {
    assert!(compile_interval_list("0,  1], [2, -1]", DefaultContext::default()).is_err());
    assert!(compile_interval_list("[0,  1], [2, -1", DefaultContext::default()).is_err());
}

#[test]
fn extra_and_misaligned_interval_brackets() {
    assert!(compile_interval_list("[[0,  1], [2, -1]", DefaultContext::default()).is_err());
    assert!(compile_interval_list("[0,  1]], [2, -1]", DefaultContext::default()).is_err());
    assert!(compile_interval_list("[0,  1][, 2, -1]", DefaultContext::default()).is_err());
}

/* Polygon set tests */

#[test]
fn single_polgyonl() {
    let expected = vec![vec![[0f64, 1f64], [2f64, 3f64], [4f64, 5f64]]];
    let actual =
        compile_polygon_set("[[0,  1], [2,3], [4, 5]]", DefaultContext::default()).unwrap();

    assert_eq!(expected, actual)
}

#[test]
fn multiple_polygons() {
    let expected = vec![
        vec![[0f64, 1f64], [2f64, 3f64], [4f64, 5f64]],
        vec![[6f64, 7f64], [8f64, 9f64], [10f64, 11f64]],
    ];
    let actual = compile_polygon_set(
        "[[0, 1], [2, 3], [4, 5]],[[6, 7], [8, 9], [10, 11]]",
        DefaultContext::default(),
    )
    .unwrap();

    assert_eq!(expected, actual)
}

#[test]
fn missing_poly_set_brackets() {
    assert!(compile_polygon_set(
        "[[0, 1], [2, 3], [4, 5],[[6, 7], [8, 9], [10, 11]]",
        DefaultContext::default()
    )
    .is_err());
    assert!(compile_interval_list(
        "[[0, 1], [2, 3], [4, 5]],[6, 7], [8, 9], [10, 11]]",
        DefaultContext::default()
    )
    .is_err());
}

#[test]
fn extra_and_misaligned_poly_set_brackets() {
    assert!(compile_interval_list(
        "[[0, 1], [2, 3], [4, 5]]],[[6, 7], [8, 9], [10, 11]]",
        DefaultContext::default()
    )
    .is_err());
    assert!(compile_interval_list(
        "[[0, 1], [2, 3], [4, 5]],[[6, 7], [8, 9], [10, 11]]]",
        DefaultContext::default()
    )
    .is_err());
    assert!(compile_interval_list(
        "[[0, 1], [2, 3], [4, 5],][[6, 7], [8, 9], [10, 11]]",
        DefaultContext::default()
    )
    .is_err());
}
