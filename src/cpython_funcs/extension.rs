use pyo3::prelude::*;

use pyo3::PyResult;

use crate::cav2d::display::{gen_display_interval_cav, CavDisplay, DisplayConfig};

use crate::core::parsing::compile_expression;
use crate::core::parsing::DefaultContext;

#[pyfunction]
pub fn display_cav(f_expr: String, c_expr: String, intervals: Vec<[f64; 2]>) -> PyResult<Vec<CavDisplay>> {
    let mut f_context = DefaultContext::default();
    f_context.add_var("x", 0);
    let f_expr = compile_expression(&f_expr, f_context)?;
    let mut c_context = DefaultContext::default();
    c_context.add_var("y", 0);
    let c_expr = compile_expression(&c_expr, c_context)?;

    Ok(gen_display_interval_cav(
        move |x| f_expr.eval(&[x]),
        move |y| c_expr.eval(&[y]),
        intervals,
        DisplayConfig::default(),
    )?)
}
