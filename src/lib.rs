pub mod cav2d;
pub mod cav3d;
pub mod core;
pub mod cpython_funcs;
pub mod errors;

use pyo3::prelude::*;

use cav2d::display::CavDisplay2D;
use cpython_funcs::extension::display_cav2d;

#[pymodule]
fn cavint(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<CavDisplay2D>()?;
    m.add_function(wrap_pyfunction!(display_cav2d, m)?)?;
    Ok(())
}
