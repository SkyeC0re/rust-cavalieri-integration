use parking_lot::FairMutex;
use peroxide::prelude::AD::{self, AD0};
use pyo3::exceptions::PyAssertionError;
use pyo3::prelude::*;
use pyo3::types::PyString;
use pyo3::{wrap_pyfunction, PyErr, PyResult};

use crate::cav2d::display::{gen_display_interval_cav, CavDisplay, DisplayConfig};
use crate::{core::parsing::parse_expr, errors::ParsedFuncError};

#[pyfunction]
pub fn eval_expr(expr: String, vars: String, pars: Vec<f64>) -> PyResult<f64> {
    let mut f = parse_expr(&expr, vars.as_str())?;
    let res = f(&pars.into_iter().map(|v| v.into()).collect::<Vec<_>>()[..])?.x();

    Ok(res)
}

#[pyfunction]
pub fn display_cav(f_expr: String, c_expr: String, a: f64, b: f64) -> PyResult<Vec<CavDisplay>> {
    let mut f = parse_expr(&f_expr, "x")?;
    let mut c = parse_expr(&c_expr, "y")?;

    // Ensure expression is valid and operations can be run.
    c(&[f(&[AD0(a)])?])?;

    let f = FairMutex::new(f);
    let f = move |x: AD| (f.lock())(&[x]).unwrap_or(AD0(f64::NAN));
    let c = FairMutex::new(c);
    let c = move |x: AD| (c.lock())(&[x]).unwrap_or(AD0(f64::NAN));

    Ok(gen_display_interval_cav(
        f,
        c,
        a,
        b,
        DisplayConfig::default(),
    )?)
}
