use cavint::core::{parsing::parse_expr, differentiable::AD};

mod basic_expressions;

fn test_expression(expr: &str, vars: &str, input: &[f64], expected: f64) {
    let mut f =
        parse_expr(expr, vars).expect(&format!("Expected Function for expression '{}'", expr));

    let ad_input = input.iter().map(|v| AD(*v, 0f64)).collect::<Vec<_>>();

    let res = f(&ad_input[..]).expect(&format!(
        "Expected Function for expression '{}' with input '{:?}'",
        expr, input
    ));
    assert_eq!(res, AD(expected, 0f64));
}
