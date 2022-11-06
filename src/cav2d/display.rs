use std::{cmp::Ordering, mem::swap};
use pyo3::pyclass;
use roots::{find_root_brent, SimpleConvergency};
use crate::{
    core::{
        differentiable::{Differentiable1D, AD},
        helpers::{vec_from_res, Sign, Signed},
        integrate::gauss_kronrod_quadrature,
    },
    errors::Display2DError,
};

#[pyclass]
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
    pub cvs: Vec<Vec<[f64; 2]>>,
    #[pyo3(get)]
    pub gv: Vec<f64>,
    #[pyo3(get)]
    pub dgv: Vec<f64>,
    #[pyo3(get)]
    pub integ_value: Option<(f64, f64)>,
}

pub struct DisplayConfig2D {
    pub compute_integ: bool,
    pub x_res: usize,
    pub y_res: usize,
    pub interm_cs: usize,
    pub max_rf_iters: usize,
    pub max_int_iters: usize,
    pub tol: f64,
}

impl Default for DisplayConfig2D {
    fn default() -> Self {
        Self {
            compute_integ: false,
            x_res: 100,
            y_res: 100,
            interm_cs: 4,
            max_rf_iters: 500,
            max_int_iters: 500,
            tol: 1e-9,
        }
    }
}

pub fn gen_display_grid_cav(
    f: impl Fn(f64) -> f64,
    c: impl Fn(f64) -> f64,
    xv: &[f64],
    yrv: &[f64],
) -> Vec<Vec<[f64; 2]>> {
    let c_0 = c(0f64);
    let c = |y: f64| c(y) - c_0;
    let g = |x: f64| x - c(f(x));
    let xrv: Vec<f64> = xv.into_iter().copied().map(|x| g(x)).collect();

    yrv.into_iter()
        .copied()
        .map(|yr| {
            xrv.iter()
                .zip(xv.into_iter())
                .map(|(&xr, &x)| {
                    let y = yr * f(x);
                    [xr + c(y), y]
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn gen_display_cav(
    f: &dyn Differentiable1D,
    c: &dyn Differentiable1D,
    intervals: Vec<[f64; 2]>,
    cfg: DisplayConfig2D,
) -> Result<Vec<CavDisplay2D>, Display2DError> {
    let mut displays = vec![];
    let g = |x: AD| x - c.composition(f.fdf(x.0)).into();
    let c_0 = c.f(0f64);
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
                        .filter(|i| *i >= xv.len()),
                )
                .chain([xv.len() - 1].into_iter())
            {
                let fx = fv[i];
                let xr = gv[i];

                let yrv = vec_from_res(0f64, 1f64, cfg.y_res);
                cvs.push(
                    yrv.into_iter()
                        .map(|r| [(r * fx), xr + c(r * fx)])
                        .collect(),
                )
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
            let (gv, dgv): (Vec<f64>, Vec<f64>) = xv.iter().map(|x| g.fdf(*x)).unzip();

            // Generate c(y)
            let sign_x = (b - a).sign_val();
            let mut min_f_x = a + sign_x * cfg.tol;
            let mut max_f_x = b - sign_x * cfg.tol;

            let mut min_fdf = f.fdf(min_f_x);
            let mut max_fdf = f.fdf(max_f_x);

            if min_fdf > max_fdf {
                swap(&mut min_fdf, &mut max_fdf);
                swap(&mut min_f_x, &mut max_f_x);
            }

            let min_f_dcy = (1f64 - g.df(min_f_x)) / min_fdf.1;
            let max_f_dcy = (1f64 - g.df(max_f_x)) / max_fdf.1;
            let cx = |x: f64| x - g.f(x);
            let min_f_cy = cx(min_f_x);
            let max_f_cy = cx(max_f_x);
            let c = |y: f64| {
                if y <= min_fdf.0 {
                    min_f_cy - min_f_dcy * (min_fdf.0 - y).ln_1p()
                } else if y >= max_fdf.0 {
                    max_f_cy + max_f_dcy * (y - max_fdf.0).ln_1p()
                } else {
                    let x = find_root_brent(
                        min_f_x,
                        max_f_x,
                        |x| f.f(x) - y,
                        &mut SimpleConvergency {
                            eps: cfg.tol,
                            max_iter: cfg.max_rf_iters,
                        },
                    )
                    .unwrap_or(f64::NAN);
                    cx(x)
                }
            };

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
                        .filter(|i| *i >= xv.len()),
                )
                .chain([xv.len() - 1].into_iter())
            {
                let fx = fv[i];
                let xr = gv[i];

                let yrv = vec_from_res(0f64, 1f64, cfg.y_res);
                cvs.push(
                    yrv.into_iter()
                        .map(|r| [(r * fx), xr + c(r * fx)])
                        .collect(),
                )
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

pub fn is_monotic_saddle(f: &dyn Differentiable1D, x: f64, tol: f64) -> bool {
    let x1 = f.fdf(x - tol);
    let x2 = f.fdf(x + tol);

    if x1.1.sign_val() == x2.1.sign_val() && x1.1.sign_val() == (x2.0 - x1.0).sign_val() {
        true
    } else {
        false
    }
}

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
    xv[0] = xv[0] + x_sign * tol;
    xv[xv_len - 1] = xv[xv_len - 1] - x_sign * tol;

    let dfv: Vec<(f64, f64)> = xv.iter().map(|x| f.fdf(*x)).collect();
    let mut roots = vec![];
    let mut i = 1;

    while i < xv.len() {
        let ldfs = dfv[i - 1].1.sign();
        let rdfs = dfv[i].1.sign();
        if ldfs == Sign::NAN || (ldfs == Sign::ZERO && !is_monotic_saddle(f, xv[i - 1], tol)) {
            roots.push(dfv[i - 1].0);
        } else if rdfs == Sign::NAN || (rdfs == Sign::ZERO && !is_monotic_saddle(f, xv[i], tol)) {
            i += 1;
            roots.push(dfv[i].0);
        } else if ldfs != rdfs {
            let r = find_root_brent(
                xv[i - 1],
                xv[i],
                |x| f.df(x),
                &mut SimpleConvergency {
                    eps: tol,
                    max_iter: max_rf_iters,
                },
            )?;
            roots.push(r);
        }
        i += 1;
    }

    Ok(roots)
}

pub fn is_strictly_monotone(v: &[f64]) -> bool {
    if v.len() < 2 {
        return true;
    }
    let expected_sign = match (v[1] - v[0]).sign() {
        Sign::POS => Sign::POS,
        Sign::NEG => Sign::NEG,
        _ => return false,
    };
    (2..v.len())
        .into_iter()
        .all(|i| (v[i] - v[i - 1]).sign() == expected_sign)
}

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

    let mut merged_roots: Vec<f64> = f_roots
        .into_iter()
        .chain(g_roots.into_iter())
        .filter(|r| (r - a).abs() > tol && (b - r).abs() > tol)
        .collect();

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
