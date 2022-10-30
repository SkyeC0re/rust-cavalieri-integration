use pyo3::prelude::*;

use pyo3::PyResult;

use crate::cav2d::display::gen_display_rs;
use crate::cav2d::display::{gen_display_cav, CavDisplay2D, DisplayConfig2D};

use crate::core::parsing::compile_expression;
use crate::core::parsing::compile_interval_list;
use crate::core::parsing::compile_polygon_set;
use crate::core::parsing::DefaultContext;

#[pyfunction]
pub fn display_cav2d(
    f_expr: String,
    c_expr: String,
    intervals: String,
    compute_integ: bool,
    x_res: usize,
    y_res: usize,
    max_rf_iters: usize,
    max_int_iters: usize,
    tol: f64,
) -> PyResult<Vec<CavDisplay2D>> {
    let mut f_context = DefaultContext::default();
    f_context.add_var("x", 0);
    let f_expr = compile_expression(&f_expr, f_context)?;
    let mut c_context = DefaultContext::default();
    c_context.add_var("y", 0);
    let c_expr = compile_expression(&c_expr, c_context)?;
    let intervals = compile_interval_list(&intervals)?;

    Ok(gen_display_cav(
        move |x| f_expr.eval(&[x]),
        move |y| c_expr.eval(&[y]),
        intervals,
        DisplayConfig2D {
            compute_integ,
            x_res,
            y_res,
            max_rf_iters,
            max_int_iters,
            tol,
        },
    )?)
}

#[pyfunction]
pub fn display_cav2d_rs(
    f_expr: String,
    g_expr: String,
    intervals: String,
    compute_integ: bool,
    x_res: usize,
    y_res: usize,
    max_rf_iters: usize,
    max_int_iters: usize,
    tol: f64,
) -> PyResult<Vec<CavDisplay2D>> {
    let mut f_context = DefaultContext::default();
    f_context.add_var("x", 0);
    let f_expr = compile_expression(&f_expr, f_context)?;
    let mut g_context = DefaultContext::default();
    g_context.add_var("x", 0);
    let g_expr = compile_expression(&g_expr, g_context)?;
    let intervals = compile_interval_list(&intervals)?;

    Ok(gen_display_rs(
        move |x| f_expr.eval(&[x]),
        move |x| g_expr.eval(&[x]),
        intervals,
        DisplayConfig2D {
            compute_integ,
            x_res,
            y_res,
            max_rf_iters,
            max_int_iters,
            tol,
        },
    )?)
}

#[pyfunction]
pub fn display_cav3d(
    f_expr: String,
    c1_expr: String,
    c2_expr: String,
    polygon_set: String,
    compute_integ: bool,
    radial_res: usize,
    angular_res: usize,
    y_res: usize,
    max_int_iters: usize,
    tol: f64,
) -> PyResult<Vec<CavDisplay2D>> {
    // let mut f_context = DefaultContext::default();
    // f_context.add_var("x", 0);
    // f_context.add_var("y", 0);
    // let f_expr = compile_expression(&f_expr, f_context)?;
    // let mut c_context = DefaultContext::default();
    // c_context.add_var("z", 0);
    // let c1_expr = compile_expression(&c1_expr, &c_context)?;
    // let c2_expr = compile_expression(&c2_expr, &c_context)?;
    // let intervals = compile_polygon_set(&polygon_set)?;

    todo!()
}
