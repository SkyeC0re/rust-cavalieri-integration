use cavint::core::parsing::parse_expr;

use super::test_expression;

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
