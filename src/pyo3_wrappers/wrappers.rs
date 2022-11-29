use pyo3::prelude::*;

use pyo3::PyResult;

use crate::cav2d::display::CavDisplay2D;

use crate::cav3d::display::gen_display_cav as gen_display_cav3d;
use crate::cav3d::display::CavDisplay3D;
use crate::cav3d::display::DisplayConfig3D;
use crate::core::parsing::compile_expression;

use crate::core::parsing::compile_polygon_set;
use crate::core::parsing::DefaultContext;

use super::display_cav3d;
use super::standardized_gui_methods::display_cav2d;
use super::standardized_gui_methods::display_cav2d_rs;

#[pyfunction]
pub fn wrapped_display_cav2d(
    f_expr: String,
    c_expr: String,
    intervals: String,
    compute_integ: bool,
    x_res: usize,
    y_res: usize,
    interm_cs: usize,
    max_rf_iters: usize,
    max_int_iters: usize,
    tol: f64,
) -> PyResult<Vec<CavDisplay2D>> {
    display_cav2d(
        f_expr,
        c_expr,
        intervals,
        compute_integ,
        x_res,
        y_res,
        interm_cs,
        max_rf_iters,
        max_int_iters,
        tol,
    )
    .map_err(|err| err.into())
}

#[pyfunction]
pub fn wrapped_display_cav2d_rs(
    f_expr: String,
    g_expr: String,
    intervals: String,
    compute_integ: bool,
    x_res: usize,
    y_res: usize,
    interm_cs: usize,
    max_rf_iters: usize,
    max_int_iters: usize,
    tol: f64,
) -> PyResult<Vec<CavDisplay2D>> {
    display_cav2d_rs(
        f_expr,
        g_expr,
        intervals,
        compute_integ,
        x_res,
        y_res,
        interm_cs,
        max_rf_iters,
        max_int_iters,
        tol,
    )
    .map_err(|err| err.into())
}

#[pyfunction]
pub fn wrapped_display_cav3d(
    f_expr: String,
    c1_expr: String,
    c2_expr: String,
    polygon_set: String,
    compute_integ: bool,
    radial_res: usize,
    x_res: usize,
    y_res: usize,
    max_int_iters: usize,
    tol: f64,
) -> PyResult<Vec<CavDisplay3D>> {
    display_cav3d(
        f_expr,
        c1_expr,
        c2_expr,
        polygon_set,
        compute_integ,
        radial_res,
        x_res,
        y_res,
        max_int_iters,
        tol,
    ).map_err(|err| err.into())
}
