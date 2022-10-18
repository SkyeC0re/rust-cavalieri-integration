use pyo3::exceptions::PyAssertionError;
use pyo3::types::PyString;
use pyo3::{PyResult, PyErr, wrap_pyfunction};
use pyo3::prelude::*;

use crate::{core::parsing::parse_expr, errors::ParseError};

#[pyfunction]
pub fn evaluate_string_expression(expr: String, vars: String, pars: Vec<f64>) -> PyResult<f64> {
    let mut f = match parse_expr(&expr, vars.as_str()) {
        Ok(f) => f,
        Err(e) => return Err(PyAssertionError::new_err("Err")),
     };

    let res = match f(
        &pars
        .into_iter()
        .map(|v| v.into())
        .collect::<Vec<_>>()[..]
    ) {
        Ok(res) => res.x(),
        Err(e) => return Err(PyAssertionError::new_err("Err")),
    };

    Ok(res)
}