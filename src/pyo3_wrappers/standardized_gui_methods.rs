use crate::cav2d::display::{gen_display_cav, gen_display_rs, DisplayConfig2D};
use crate::cav3d::display::{CavDisplay3D, DisplayConfig3D, gen_display_cav as gen_display_cav3d};
use crate::core::parsing::compile_polygon_set;
use crate::errors::PyProxyError;
use crate::{
    cav2d::display::CavDisplay2D,
    core::parsing::{compile_expression, compile_interval_list, DefaultContext},
};

pub fn display_cav2d(
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
) -> Result<Vec<CavDisplay2D>, PyProxyError> {
    let mut f_context = DefaultContext::default();
    f_context.add_var("x", 0);
    let f_expr = compile_expression(&f_expr, &f_context)?;
    let mut c_context = DefaultContext::default();
    c_context.add_var("y", 0);
    let c_expr = compile_expression(&c_expr, &c_context)?;

    let intervals_context = DefaultContext::default();
    let intervals = compile_interval_list(&intervals, &intervals_context)?;

    Ok(gen_display_cav(
        &move |x| f_expr.eval(&[x]),
        &move |y| c_expr.eval(&[y]),
        intervals,
        DisplayConfig2D {
            compute_integ,
            x_res,
            y_res,
            interm_cs,
            max_rf_iters,
            max_int_iters,
            tol,
        },
    )?)
}

pub fn display_cav2d_rs(
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
) -> Result<Vec<CavDisplay2D>, PyProxyError> {
    let mut context = DefaultContext::default();
    context.add_var("x", 0);
    let f_expr = compile_expression(&f_expr, &context)?;
    let g_expr = compile_expression(&g_expr, &context)?;
    let intervals_context = DefaultContext::default();
    let intervals = compile_interval_list(&intervals, &intervals_context)?;

    Ok(gen_display_rs(
        &move |x| f_expr.eval(&[x]),
        &move |x| g_expr.eval(&[x]),
        intervals,
        DisplayConfig2D {
            compute_integ,
            x_res,
            y_res,
            interm_cs,
            max_rf_iters,
            max_int_iters,
            tol,
        },
    )?)
}

pub fn display_cav3d(
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
) ->Result<Vec<CavDisplay3D>, PyProxyError> {
    let mut f_context = DefaultContext::default();
    f_context.add_var("x", 0);
    f_context.add_var("y", 1);
    let f_expr = compile_expression(&f_expr, f_context)?;
    let mut c_context = DefaultContext::default();
    c_context.add_var("z", 0);
    let c1_expr = compile_expression(&c1_expr, &c_context)?;
    let c2_expr = compile_expression(&c2_expr, &c_context)?;
    let poly_set_context = DefaultContext::default();
    let poly_set = compile_polygon_set(&polygon_set, poly_set_context)?;

    Ok(gen_display_cav3d(
        move |x| f_expr.eval(&x),
        move |x| [c1_expr.eval(&[x]), c2_expr.eval(&[x])],
        poly_set,
        DisplayConfig3D {
            compute_integ,
            radial_res,
            x_res,
            y_res,
            max_int_iters,
            tol,
        },
    )?)
}
