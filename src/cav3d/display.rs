use pyo3::pyclass;


#[pyclass]
pub struct CavDisplay3D {
    #[pyo3(get)]
    pub triag: [[f64; 2]; 3],
    #[pyo3(get)]
    pub curtains: [Vec<Vec<[f64; 2]>>; 3],
    #[pyo3(get)]
    pub top_mesh: Vec<Vec<[f64; 2]>>,
    #[pyo3(get)]
    pub bot_mesh: Vec<Vec<[f64; 2]>>,
    #[pyo3(get)]
    pub integ_value: Option<(f64, f64)>,
}

#[pyclass]
pub struct DisplayConfig3D {
    #[pyo3(get, set)]
    pub compute_integ: bool,
    #[pyo3(get, set)]
    pub radial_res: usize,
    #[pyo3(get, set)]
    pub x_res: usize,
    #[pyo3(get, set)]
    pub y_res: usize,
    #[pyo3(get, set)]
    pub max_rf_iters: usize,
    #[pyo3(get, set)]
    pub max_int_iters: usize,
    #[pyo3(get, set)]
    pub tol: f64,
}