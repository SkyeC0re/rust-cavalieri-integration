use cavint::core::{parsing::parse_expr, differentiable::AD};

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


#[test]
fn test_empty_fail() {
    let expr = "";

    assert_eq!(parse_expr(expr, "").is_err(), true,)
}

#[test]
fn test_basics() {
    test_expression("x+2", "x", &[3.0], 5.0);
    test_expression("y^x", "xy", &[3.0, 2.0], 8.0);
}
