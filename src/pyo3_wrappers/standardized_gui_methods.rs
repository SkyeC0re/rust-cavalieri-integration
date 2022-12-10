use crate::cav2d::display::{gen_display_cav, gen_display_rs, DisplayConfig2D};
use crate::cav3d::display::{gen_display_cav as gen_display_cav3d, CavDisplay3D, DisplayConfig3D};
use crate::core::parsing::compile_polygon_set;
use crate::errors::PyProxyError;
use crate::{
    cav2d::display::CavDisplay2D,
    core::parsing::{compile_expression, compile_interval_list, DefaultContext},
};

/// Generates a 2-dimensional Cavalieri integral visualization from text input.
/// 
/// * `f_expr` - The integrand. Must be written in terms of x.
/// * `c_expr` - The c-curve associated with the integral. Must be written in terms of y.
/// * `intervals` - The set of intervals over which to integrate
/// * `compute_integ` - Whether or not to compute the integral value
/// * `x_res` - The resolution of the x-dimension
/// * `y_res` - The resolution of the y-dimension
/// * `interm_cs` - The amount of intermediate c-curves to show
/// * `max_rf_iters` - The maximum number of root finding iterations allowed
/// * `max_int_iters` - The maximum number of integration iterations allowed
/// * `tol` - The tolerance allowed when computing the integral value
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

/// Generates a 2-dimensional Cavalieri integral representation of a Riemann-Stieltjes
/// integral from text input.
/// 
/// * `f_expr` - The integrand. Must be written in terms of x.
/// * `g_expr` - The integrator. Must be written in terms of x.
/// * `intervals` - The set of intervals over which to integrate
/// * `compute_integ` - Whether or not to compute the integral value
/// * `x_res` - The resolution of the x-dimension
/// * `y_res` - The resolution of the y-dimension
/// * `interm_cs` - The amount of intermediate c-curves to show
/// * `max_rf_iters` - The maximum number of root finding iterations allowed
/// * `max_int_iters` - The maximum number of integration iterations allowed
/// * `tol` - The tolerance allowed when computing the integral value
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

/// Generates a 3-dimensional Cavalieri integral visualization from text input.
/// 
/// * `f_expr` - The integrand. Must be written in terms of x.
/// * `c1_expr` - The $c_1$ function governing the c-curve associated with the integral. 
///     Must be written in terms of y.
/// * `c2_expr` - The $c_2$ function governing the c-curve associated with the integral. 
///     Must be written in terms of y.
/// * `poygons_set` - A non-intersection, non-degenerate polygon set
/// * `compute_integ` - Whether or not to compute the integral value
/// * `radial_res` - The radial resolution of the top triangle meshes 
/// * `x_res` - The resolution of the x-dimensions
/// * `y_res` - The resolution of the y-dimension
/// * `max_rf_iters` - The maximum number of root finding iterations allowed
/// * `max_int_iters` - The maximum number of integration iterations allowed
/// * `tol` - The tolerance allowed when computing the integral value
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
) -> Result<Vec<CavDisplay3D>, PyProxyError> {
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
