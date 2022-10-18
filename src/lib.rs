
pub mod core;
pub mod cav2d;
pub mod cav3d;
pub mod errors;
pub mod cpython_funcs;

use pyo3::prelude::*;

use cpython_funcs::extension::evaluate_string_expression;

#[pymodule]
fn cavint(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(evaluate_string_expression, m)?)?;
    Ok(())
}