use std::{cmp::Ordering, mem::swap};

use crate::{
    core::{
        differentiable::{Differentiable1D, AD},
        helpers::{vec_from_res, Signed},
        integrate::gauss_kronrod_quadrature,
    },
    errors::Display2DError,
};
use log::debug;
use pyo3::pyclass;
use roots::{find_root_brent, Convergency};

const C_GRAD_MAX: f64 = 10f64;

#[pyclass]
/// 2-dimensional Cavalieri integral visualization over an interval.
/// 
/// * `a` - Start of the interval
/// * `b` - End of the interval
/// * `fv` - Values of the integrand across the interval
/// * `xv` - The x-values across the interval
/// * `cvs` - A tuple containing the values of intermediate c-curves across
///     the interval and the index `i` at which they connect to `(xv[i], fv[i])`
/// * `gv` - Values of the integrator across the interval
/// * `dgv` - Derivative values of the integrator across the interval
/// * `integ_value` - The integration value and estimated error
pub struct CavDisplay2D {
    #[pyo3(get)]
    pub a: f64,
    #[pyo3(get)]
    pub b: f64,
    #[pyo3(get)]
    pub fv: Vec<f64>,
    #[pyo3(get)]
    pub xv: Vec<f64>,
    #[pyo3(get)]
    pub cvs: Vec<(usize, Vec<[f64; 2]>)>,
    #[pyo3(get)]
    pub gv: Vec<f64>,
    #[pyo3(get)]
    pub dgv: Vec<f64>,
    #[pyo3(get)]
    pub integ_value: Option<(f64, f64)>,
}

/// Configuration settings for the 2-dimensional Cavalieri integral visualization.
///
/// * `compute_integ` - Whether or not to compute the integral value
/// * `x_res` - The resolution of the x-dimension
/// * `y_res` - The resolution of the y-dimension
/// * `interm_cs` - The amount of intermediate c-curves to show
/// * `max_rf_iters` - The maximum number of root finding iterations allowed
/// * `max_int_iters` - The maximum number of integration iterations allowed
/// * `tol` - The tolerance allowed when computing the integral value
pub struct DisplayConfig2D {
    pub compute_integ: bool,
    pub x_res: usize,
    pub y_res: usize,
    pub interm_cs: usize,
    pub max_rf_iters: usize,
    pub max_int_iters: usize,
    pub tol: f64,
}

struct XConvergency {
    pub max_iters: usize,
    pub tol: f64,
}

impl Convergency<f64> for XConvergency {
    fn is_root_found(&mut self, y: f64) -> bool {
        y == 0f64
    }

    fn is_converged(&mut self, x1: f64, x2: f64) -> bool {
        (x2 - x1).abs() < self.tol / 2f64
    }

    fn is_iteration_limit_reached(&mut self, iter: usize) -> bool {
        iter >= self.max_iters
    }
}

/// Computes a visualization for the 2-dimensional Cavalieri integral.
/// 
/// * `f` - The differentiable integrand
/// * `c` - The differentiable c-curve
/// * `intervals` - The intervals over which to integrate
/// * `cfg` - The configuration settings
pub fn gen_display_cav(
    f: &dyn Differentiable1D,
    c: &dyn Differentiable1D,
    intervals: Vec<[f64; 2]>,
    cfg: DisplayConfig2D,
) -> Result<Vec<CavDisplay2D>, Display2DError> {
    let mut displays = vec![];
    let c_0 = c.f(0f64);
    let g = |x: AD| x - c.composition(f.fdf(x.0)).into() + AD(c_0, 0f64);
    let c = |y: f64| c.f(y) - c_0;
    for interval in intervals {
        let a = interval[0];
        let b = interval[1];
        let xv = vec_from_res(a, b, cfg.x_res);
        let mut splits = split_strictly_monotone(&g, &xv[..], cfg.tol, cfg.max_rf_iters)?;
        splits.insert(0, a);
        splits.push(b);
        for i in 1..splits.len() {
            let a = splits[i - 1];
            let b = splits[i];

            // Find integration value
            let integ_value = if cfg.compute_integ {
                Some(gauss_kronrod_quadrature(
                    |x| f.f(x) * g.df(x),
                    a,
                    b,
                    cfg.tol,
                    Some(cfg.max_int_iters),
                )?)
            } else {
                None
            };

            // Compute x*, f(x*), g(x*) and g'(x*)
            let xv = vec_from_res(a, b, cfg.x_res);
            let fv: Vec<f64> = xv.iter().map(|x| f.f(*x)).collect();
            let (gv, dgv): (Vec<f64>, Vec<f64>) = xv.iter().map(|x| g.fdf(*x)).unzip();

            // Compute required c(y) translations
            let mut cvs = Vec::with_capacity(2 + cfg.interm_cs);
            for i in [0]
                .into_iter()
                .chain(
                    (1..=cfg.interm_cs)
                        .map(|n| {
                            (((xv.len() - 1) as f64 * n as f64) / (cfg.interm_cs + 1) as f64)
                                .round() as usize
                        })
                        .filter(|i| *i < xv.len()),
                )
                .chain([xv.len() - 1].into_iter())
            {
                let fx = fv[i];
                let xr = gv[i];

                let yrv = vec_from_res(0f64, 1f64, cfg.y_res);
                cvs.push((
                    i,
                    yrv.into_iter()
                        .map(|r| [(r * fx), xr + c(r * fx)])
                        .collect(),
                ))
            }

            displays.push(CavDisplay2D {
                a,
                b,
                fv,
                xv,
                cvs,
                gv,
                dgv,
                integ_value,
            });
        }
    }
    Ok(displays)
}

/// Computes a 2-dimensional Cavalieri integral representation for a Riemann-Stieltjes integral.
/// 
/// * `f` - The differentiable integrand
/// * `g` - The differentiable integrator
/// * `intervals` - The intervals over which to integrate
/// * `cfg` - The configuration settings
pub fn gen_display_rs(
    f: &dyn Differentiable1D,
    g: &dyn Differentiable1D,
    intervals: Vec<[f64; 2]>,
    cfg: DisplayConfig2D,
) -> Result<Vec<CavDisplay2D>, Display2DError> {
    let mut displays = vec![];
    for interval in intervals {
        let a = interval[0];
        let b = interval[1];
        let xv = vec_from_res(a, b, cfg.x_res);
        let mut splits = split_translational(f, g, &xv[..], cfg.tol, cfg.max_rf_iters)?;
        splits.insert(0, a);
        splits.push(b);

        for i in 1..splits.len() {
            let a = splits[i - 1];
            let b = splits[i];

            // Find integration value
            let integ_value = if cfg.compute_integ {
                Some(gauss_kronrod_quadrature(
                    |x| f.f(x) * g.df(x),
                    a,
                    b,
                    cfg.tol,
                    Some(cfg.max_int_iters),
                )?)
            } else {
                None
            };

            // Find x*, f(x*), g(x*), dg(x*)
            let xv = vec_from_res(a, b, cfg.x_res);
            let fv: Vec<f64> = xv.iter().map(|x| f.f(*x)).collect();
            let (mut gv, dgv): (Vec<f64>, Vec<f64>) = xv.iter().map(|x| g.fdf(*x)).unzip();

            // Generate c(y)
            let mut min_x = a;
            let mut max_x = b;
            if min_x > max_x {
                swap(&mut min_x, &mut max_x);
            }
            min_x += cfg.tol;
            max_x -= cfg.tol;

            let mut min_fdf = f.fdf(min_x);
            let mut max_fdf = f.fdf(max_x);

            if min_fdf > max_fdf {
                swap(&mut min_fdf, &mut max_fdf);
                swap(&mut min_x, &mut max_x);
            }
            let p_decay_max = C_GRAD_MAX * (max_x - min_x).abs();

            let mut min_f_dcy = (1f64 - g.df(min_x)) / min_fdf.1;
            // Revert to c'(y) = 0 if gradient is too large
            if !(min_f_dcy.abs() < p_decay_max) {
                min_f_dcy = 0f64;
            }
            let mut max_f_dcy = (1f64 - g.df(max_x)) / max_fdf.1;
            // Revert to c'(y) = 0 if gradient is too large
            if !(max_f_dcy.abs() < p_decay_max) {
                max_f_dcy = 0f64;
            }

            let cx = |x: f64| x - g.f(x);
            let min_f_cy = cx(min_x);
            let max_f_cy = cx(max_x);
            let c_raw = |y: f64| -> Result<f64, Display2DError> {
                if y < min_fdf.0 {
                    Ok(min_f_cy
                        - min_f_dcy.sign_val() * (min_f_dcy.abs() * (min_fdf.0 - y)).ln_1p())
                } else if y > max_fdf.0 {
                    Ok(max_f_cy
                        + max_f_dcy.sign_val() * (max_f_dcy.abs() * (y - max_fdf.0)).ln_1p())
                } else {
                    let x = find_root_brent(
                        min_x,
                        max_x,
                        |x| f.f(x) - y,
                        &mut XConvergency {
                            tol: cfg.tol,
                            max_iters: cfg.max_rf_iters,
                        },
                    )?;
                    Ok(cx(x))
                }
            };

            let k = c_raw(0f64)?;
            let c = |y| c_raw(y).map(|cy| cy - k);
            gv.iter_mut().for_each(|xr| *xr += k);

            // Compute required c(y) translations
            let mut cvs = Vec::with_capacity(2 + cfg.interm_cs);
            let yrv = vec_from_res(0f64, 1f64, cfg.y_res);
            for i in [0]
                .into_iter()
                .chain(
                    (1..=cfg.interm_cs)
                        .map(|n| {
                            (((xv.len() - 1) as f64 * n as f64) / (cfg.interm_cs + 1) as f64)
                                .round() as usize
                        })
                        .filter(|i| *i < xv.len()),
                )
                .chain([xv.len() - 1].into_iter())
            {
                let fx = fv[i];
                let xr = gv[i];

                cvs.push((i, {
                    let mut cv = Vec::with_capacity(yrv.len());
                    for r in yrv.iter().copied() {
                        cv.push([(r * fx), xr + c(r * fx)?])
                    }
                    cv
                }))
            }

            displays.push(CavDisplay2D {
                a,
                b,
                fv,
                xv,
                cvs,
                gv,
                dgv,
                integ_value,
            })
        }
    }
    Ok(displays)
}

pub fn gen_func_interval(f: impl Fn(AD) -> AD, xv: &[f64]) -> Vec<f64> {
    let yv = xv.into_iter().map(|x| f(AD(*x, 0f64)).0).collect();
    yv
}

/// Determines whether or not a differentiable function\'s derivative changes sign at
/// a point x.
/// 
/// * `f` - The function to evaluate
/// * `x` - The point at which to evalutate
/// * `tol` - Points are sampled at `x - tol` and `x + tol` to determine monotonicity
pub fn is_monotonic_saddle(f: &dyn Differentiable1D, x: f64, tol: f64) -> bool {
    let x1 = f.fdf(x - tol);
    let x2 = f.fdf(x + tol);

    if x1.1.sign_val() == x2.1.sign_val() && x1.1.sign_val() == (x2.0 - x1.0).sign_val() {
        true
    } else {
        false
    }
}

/// Partitions an interval into strictly monotone sub-intervals with respect to a differentiable function.
/// 
/// * `f` - A differentiable function
/// * `xv` - Points on the interval $[a, b]$, with the first and last points being treated as $a$ and $b$ respectively
/// * `tol` - The tolerance threshold to use when computing a root of the function derivative
/// * `max_rf_iters` - The maximum number of root finding iterations allowed
pub fn split_strictly_monotone(
    f: &dyn Differentiable1D,
    xv: &[f64],
    tol: f64,
    max_rf_iters: usize,
) -> Result<Vec<f64>, Display2DError> {
    let xv_len = xv.len();
    if xv_len < 2 {
        return Ok(vec![]);
    };
    let mut xv: Vec<f64> = xv.into_iter().copied().collect();
    let a = xv[0];
    let b = xv[xv_len - 1];

    let x_sign = (b - a).sign_val();
    xv[0] += x_sign * tol;
    xv[xv_len - 1] -= x_sign * tol;

    let dfv: Vec<(f64, f64)> = xv.iter().map(|x| f.fdf(*x)).collect();
    let mut roots = vec![];
    let mut i = 1;

    while i < xv.len() {
        let ldfs = dfv[i - 1].1;
        let rdfs = dfv[i].1;
        if !ldfs.is_finite() || (ldfs == 0f64 && !is_monotonic_saddle(f, xv[i - 1], tol)) {
            debug!("Root by Left: {}", xv[i - 1]);
            roots.push(xv[i - 1]);
        } else if !rdfs.is_finite() || (rdfs == 0f64 && !is_monotonic_saddle(f, xv[i], tol)) {
            debug!("Root by Right: {}", xv[i]);
            roots.push(xv[i]);
            i += 1;
        } else if ldfs.signum() != rdfs.signum() {
            let r = find_root_brent(
                xv[i - 1],
                xv[i],
                |x| f.df(x),
                &mut XConvergency {
                    tol,
                    max_iters: max_rf_iters,
                },
            )?;
            debug!("Root by Finder: {r}");
            roots.push(r);
        }
        i += 1;
    }

    Ok(roots)
}

// Partitions an interval into translational sub-intervals with respect to the integrand $f$ and integrator $g$.
/// 
/// * `f` - A differentiable integrand
/// * `g` - A differentiable integrator
/// * `xv` - Points on the interval $[a, b]$, with the first and last points being treated as $a$ and $b$ respectively
/// * `tol` - The tolerance threshold to use when computing a root of the function derivative
/// * `max_rf_iters` - The maximum number of root finding iterations allowed
pub fn split_translational(
    f: &dyn Differentiable1D,
    g: &dyn Differentiable1D,
    xv: &[f64],
    tol: f64,
    max_rf_iters: usize,
) -> Result<Vec<f64>, Display2DError> {
    if xv.len() < 2 {
        return Ok(vec![]);
    }
    let a = xv[0];
    let b = xv[xv.len() - 1];
    let x_sign = (b - a).sign_val();
    let f_roots = split_strictly_monotone(f, xv, tol, max_rf_iters)?;
    let g_roots = split_strictly_monotone(g, xv, tol, max_rf_iters)?;

    let mut merged_roots: Vec<f64> = f_roots.into_iter().chain(g_roots.into_iter()).collect();

    merged_roots.sort_by(|a, b| {
        let sdiff = x_sign * (a - b);
        if sdiff > 0f64 {
            Ordering::Greater
        } else if sdiff < 0f64 {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    });

    // Merge roots if they overlap within `tol` precision.
    let roots = if merged_roots.len() > 1 {
        let mut li = 0;
        let mut roots = Vec::with_capacity(merged_roots.len());
        for (i, r) in merged_roots.iter().enumerate() {
            if (*r - merged_roots[li]).abs() > 2.0 * tol {
                roots.push(merged_roots[li..i].iter().copied().sum::<f64>() / (i - li) as f64);
                li = i;
            }
        }
        roots.push(
            merged_roots[li..].iter().copied().sum::<f64>() / (merged_roots.len() - li) as f64,
        );
        roots
    } else {
        merged_roots
    };

    Ok(roots)
}
