use pyo3::pyclass;

use crate::{
    core::{
        differentiable::{abs_jacobian_det, AD},
        helpers::{n_vec_from_res, vec_from_res},
        integrate::gauss_kronrod_quadrature_triangle,
        triangulation::triangulate_polygon_set,
    },
    errors::Display3DError,
};

#[pyclass]
/// A 3-dimensional Cavalieri integral visualization over a triangular region.
/// 
/// * `triag` - Vertices of the triangular $S$ region
/// * `curtains` - The three side curtains of the Cavalieri integral over
///     the triangle
/// * `top_mesh` - A mesh for $S$
/// * `bot_mesh` - A mesh for $R$
/// * `integ_value` - The integration value and estimated error
pub struct CavDisplay3D {
    #[pyo3(get)]
    pub triag: [[f64; 2]; 3],
    #[pyo3(get)]
    pub curtains: [Vec<Vec<[f64; 3]>>; 3],
    #[pyo3(get)]
    pub top_mesh: Vec<Vec<[f64; 3]>>,
    #[pyo3(get)]
    pub bot_mesh: Vec<Vec<[f64; 2]>>,
    #[pyo3(get)]
    pub integ_value: Option<(f64, f64)>,
}

impl CavDisplay3D {
    /// Generates a new 3-dimensional Cavalieri integral visualization over a triangular region.
    /// 
    /// * `f` - The integrand
    /// * `c` - The c-curve
    /// * `triag` - The triangle over which to create the display
    /// * `integ_value` - The integration value and estimated error to pass through
    /// * `cfg` - The configuration settings
    fn new(
        f: impl Fn([f64; 2]) -> f64,
        c: impl Fn(f64) -> [f64; 2],
        triag: [[f64; 2]; 3],
        integ_value: Option<(f64, f64)>,
        cfg: &DisplayConfig3D,
    ) -> Self {
        let xvs = [
            n_vec_from_res(&triag[1], &triag[2], cfg.x_res),
            n_vec_from_res(&triag[2], &triag[0], cfg.x_res),
            n_vec_from_res(&triag[0], &triag[1], cfg.x_res),
        ];
        let yrv = vec_from_res(0f64, 1f64, cfg.y_res);
        let g = |x: [f64; 2]| {
            let mut cfx = c(f(x));
            cfx[0] = x[0] - cfx[0];
            cfx[1] = x[1] - cfx[1];
            cfx
        };

        let curtains = [
            gen_display_curtain(&f, &c, &xvs[0], &yrv),
            gen_display_curtain(&f, &c, &xvs[1], &yrv),
            gen_display_curtain(&f, &c, &xvs[2], &yrv),
        ];

        let radrv = vec_from_res(0f64, 1f64, cfg.radial_res);

        let center = [
            (triag[0][0] + triag[1][0] + triag[2][0]) / 3f64,
            (triag[0][1] + triag[1][1] + triag[2][1]) / 3f64,
        ];
        let top_mesh = radrv
            .iter()
            .map(|radr| {
                xvs[0]
                    .iter()
                    .chain(xvs[1].iter().skip(1))
                    .chain(xvs[2].iter().skip(1))
                    .map(|x| {
                        let x = [
                            (1f64 - radr) * center[0] + radr * x[0],
                            (1f64 - radr) * center[1] + radr * x[1],
                        ];
                        [x[0], x[1], f(x)]
                    })
                    .collect::<Vec<_>>()
            })
            .collect();

        let bot_mesh = radrv
            .iter()
            .map(|radr| {
                xvs[0]
                    .iter()
                    .chain(xvs[1].iter().skip(1))
                    .chain(xvs[2].iter().skip(1))
                    .map(|x| {
                        let x = [
                            (1f64 - radr) * center[0] + radr * x[0],
                            (1f64 - radr) * center[1] + radr * x[1],
                        ];
                        g(x)
                    })
                    .collect::<Vec<_>>()
            })
            .collect();

        CavDisplay3D {
            triag,
            curtains,
            top_mesh,
            bot_mesh,
            integ_value,
        }
    }
}

/// Configuration settings for the 3-dimensional Cavalieri integral visualization. 
/// 
/// * `compute_integ` - Whether or not to compute the integral value
/// * `radial_res` - The radial resolution of the top triangle meshes 
/// * `x_res` - The resolution of the x-dimensions
/// * `y_res` - The resolution of the y-dimension
/// * `max_rf_iters` - The maximum number of root finding iterations allowed
/// * `max_int_iters` - The maximum number of integration iterations allowed
/// * `tol` - The tolerance allowed when computing the integral value
pub struct DisplayConfig3D {
    pub compute_integ: bool,
    pub radial_res: usize,
    pub x_res: usize,
    pub y_res: usize,
    pub max_int_iters: usize,
    pub tol: f64,
}

/// Generates a side mesh for a triangular region.
/// 
/// Produces a 2-dimensional grid representing a surface in $\mathbb{R}^3$.
/// 
/// * `f` - The integrand of the Cavalieri integral
/// * `c` - The c-curve of the Cavalieri integral
/// * `xv` - Points on the interval $[a_1, b_1] \times [a_2, b_2]$, with the first and last points being treated 
///     as $\{a_1, a_2\}$ and $\{b_1, b_2 \}$ respectively
/// * `yrv` - The ratios $0 \leq r \leq 1$ of $y = f(\mathbf{x^*})$ to at which to sample the mesh
pub fn gen_display_curtain(
    f: impl Fn([f64; 2]) -> f64,
    c: impl Fn(f64) -> [f64; 2],
    xv: &[[f64; 2]],
    yrv: &[f64],
) -> Vec<Vec<[f64; 3]>> {
    let c_0 = c(0f64);
    let c = move |y: f64| {
        let mut cy = c(y);
        cy[0] -= c_0[0];
        cy[1] -= c_0[1];
        cy
    };
    let g = |x: [f64; 2]| {
        let mut cfx = c(f(x));
        cfx[0] = x[0] - cfx[0];
        cfx[1] = x[1] - cfx[1];
        cfx
    };
    let xrv: Vec<[f64; 2]> = xv.into_iter().map(|&x| g(x)).collect();
    let fv: Vec<f64> = xv.into_iter().map(|&x| f(x)).collect();

    yrv.into_iter()
        .copied()
        .map(|yr| {
            let mut row = Vec::with_capacity(xv.len());
            for i in 0..xv.len() {
                let y = yr * fv[i];
                let mut x = c(y);
                x[0] += xrv[i][0];
                x[1] += xrv[i][1];
                row.push([x[0], x[1], y]);
            }
            row
        })
        .collect()
}

/// Generates a 3-dimensional Cavalieri visualization.
/// 
/// * `f` - The integrand of the Cavalieri integral
/// * `c` - The c-curve of the Cavalieri integral
/// * `poly_set` - A set of non-intersecting, non-degenerate polygons
/// * `cfg` - The configuration settings
pub fn gen_display_cav(
    f: impl Fn([AD; 2]) -> AD,
    c: impl Fn(AD) -> [AD; 2],
    poly_set: Vec<Vec<[f64; 2]>>,
    cfg: DisplayConfig3D,
) -> Result<Vec<CavDisplay3D>, Display3DError> {
    let mut displays = vec![];

    let triangles = triangulate_polygon_set(poly_set)?;
    for triag in triangles {
        let triag: [[f64; 2]; 3] = triag.into();
        displays.push(CavDisplay3D::new(
            |x| f([AD(x[0], 0f64), AD(x[1], 0f64)]).0,
            |y| {
                let cy = c(AD(y, 0f64));
                [cy[0].0, cy[1].0]
            },
            triag,
            if cfg.compute_integ {
                let g = |x: [AD; 2]| {
                    let mut cfx = c(f(x));
                    cfx[0] = x[0] - cfx[0];
                    cfx[1] = x[1] - cfx[1];
                    cfx
                };

                Some(gauss_kronrod_quadrature_triangle(
                    |xy| f(xy.map(|v| v.into())).0 * abs_jacobian_det(g, xy),
                    triag,
                    cfg.tol,
                    Some(cfg.max_int_iters),
                )?)
            } else {
                None
            },
            &cfg,
        ));
    }
    Ok(displays)
}
