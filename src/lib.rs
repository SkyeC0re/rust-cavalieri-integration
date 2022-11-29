pub mod cav2d;
pub mod cav3d;
pub mod core;
pub mod errors;
pub mod pyo3_wrappers;

use pyo3::prelude::*;

use cav2d::display::CavDisplay2D;
use pyo3_wrappers::{wrapped_display_cav2d, wrapped_display_cav2d_rs, wrapped_display_cav3d};

#[pymodule]
fn cavint(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<CavDisplay2D>()?;
    m.add_function(wrap_pyfunction!(wrapped_display_cav2d, m)?)?;
    m.add_function(wrap_pyfunction!(wrapped_display_cav2d_rs, m)?)?;
    m.add_function(wrap_pyfunction!(wrapped_display_cav3d, m)?)?;
    Ok(())
}
