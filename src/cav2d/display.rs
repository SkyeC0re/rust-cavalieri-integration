use std::cmp::Ordering;

use peroxide::{prelude::{cubic_spline, linspace, Spline}, fuga::{cubic_hermite_spline, SlopeMethod}};
use pyo3::pyclass;
use roots::{find_root_brent, SimpleConvergency};

use crate::{
    core::{
        differentiable::{AD, ZERO},
        helpers::{Sign, Signed},
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
    pub cav_grid: Vec<Vec<[f64; 2]>>,
    #[pyo3(get)]
    pub dgv: Vec<f64>,
    #[pyo3(get)]
    pub integ_value: Option<(f64, f64)>,
}

impl CavDisplay2D {
    pub(crate) fn new(
        f: impl Fn(f64) -> f64,
        c: impl Fn(f64) -> f64,
        dg: impl Fn(f64) -> f64,
        a: f64,
        b: f64,
        integ_value: Option<(f64, f64)>,
        cfg: &DisplayConfig2D,
    ) -> Self {
        let mut xv = linspace(a, b, cfg.x_res);
        let yrv = linspace(0f64, 1f64, cfg.y_res);

        CavDisplay2D {
            a,
            b,
            cav_grid: gen_display_grid_cav(&f, &c, &xv[..], &yrv[..]),
            dgv: {
                xv.iter_mut().for_each(|x| *x = dg(*x));
                xv
            },
            integ_value,
        }
    }
}

pub struct DisplayConfig2D {
    pub compute_integ: bool,
    pub x_res: usize,
    pub y_res: usize,
    pub max_rf_iters: usize,
    pub max_int_iters: usize,
    pub tol: f64,
}

impl Default for DisplayConfig2D {
    fn default() -> Self {
        Self {
            compute_integ: false,
            x_res: 50,
            y_res: 50,
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
    f: impl Fn(AD) -> AD,
    c: impl Fn(AD) -> AD,
    intervals: Vec<[f64; 2]>,
    cfg: DisplayConfig2D,
) -> Result<Vec<CavDisplay2D>, Display2DError> {
    let mut displays = vec![];
    let g = |x: AD| x - c(f(x));
    for interval in intervals {
        let a = interval[0];
        let b = interval[1];
        let xv = linspace(a, b, cfg.x_res);
        let mut splits = split_strictly_monotone(g, &xv[..], cfg.tol, cfg.max_rf_iters)?;
        splits.insert(0, a);
        splits.push(b);
        for i in 1..splits.len() {
            //let c = generate_c(&f, g, &xv)?;
            let integ_value = if cfg.compute_integ {
                Some(gauss_kronrod_quadrature(
                    |x| f(AD(x, 1f64)).0 * g(AD(x, 1f64)).1,
                    a,
                    b,
                    cfg.tol,
                    Some(cfg.max_int_iters),
                )?)
            } else {
                None
            };
            displays.push(CavDisplay2D::new(
                |x| f(AD(x, 0f64)).0,
                |y| c(AD(y, 0f64)).0,
                |x| g(AD(x, 1f64)).1,
                splits[i - 1],
                splits[i],
                integ_value,
                &cfg,
            ));
        }
    }
    Ok(displays)
}

pub fn gen_display_rs(
    f: impl Fn(AD) -> AD,
    g: impl Fn(AD) -> AD,
    intervals: Vec<[f64; 2]>,
    cfg: DisplayConfig2D,
) -> Result<Vec<CavDisplay2D>, Display2DError> {
    let mut displays = vec![];
    for interval in intervals {
        let a = interval[0];
        let b = interval[1];
        let xv = linspace(a, b, cfg.x_res);
        let mut splits = split_translational(&f, &g, &xv[..], cfg.tol, cfg.max_rf_iters)?;
        splits.insert(0, a);
        splits.push(b);

        for i in 1..splits.len() {
            let c = generate_c(&f, &g, &xv)?;
            let integ_value = if cfg.compute_integ {
                Some(gauss_kronrod_quadrature(
                    |x| f(AD(x, 0f64)).0 * g(AD(x, 1f64)).1,
                    a,
                    b,
                    cfg.tol,
                    Some(cfg.max_int_iters),
                )?)
            } else {
                None
            };
            displays.push(CavDisplay2D::new(
                |x| f(AD(x, 0f64)).0,
                c,
                |x| g(AD(x, 1f64)).1,
                splits[i - 1],
                splits[i],
                integ_value,
                &cfg,
            ));
        }
    }
    Ok(displays)
}

pub fn gen_func_interval(f: impl Fn(AD) -> AD, xv: &[f64]) -> Vec<f64> {
    let yv = xv.into_iter().map(|x| f(AD(*x, 0f64)).0).collect();
    yv
}

pub fn is_monotic_saddle(f: impl Fn(AD) -> AD, x: f64, tol: f64) -> bool {
    let x1 = f(AD(x - tol, 1f64));
    let x2 = f(AD(x + tol, 1f64));

    if x1.1.sign_val() == x2.1.sign_val() && x1.1.sign_val() == (x2.0 - x1.0).sign_val() {
        true
    } else {
        false
    }
}

pub fn split_strictly_monotone(
    f: impl Fn(AD) -> AD,
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

    let dfv: Vec<AD> = xv.iter().map(|x| f(AD(*x, 1f64))).collect();
    let mut roots = vec![];
    let mut i = 1;

    while i < xv.len() {
        let ldfs = dfv[i - 1].1.sign();
        let rdfs = dfv[i].1.sign();
        if ldfs == Sign::NAN || (ldfs == Sign::ZERO && !is_monotic_saddle(&f, xv[i - 1], tol)) {
            roots.push(dfv[i - 1].0);
        } else if rdfs == Sign::NAN || (rdfs == Sign::ZERO && !is_monotic_saddle(&f, xv[i], tol)) {
            i += 1;
            roots.push(dfv[i].0);
        } else if ldfs != rdfs {
            let r = find_root_brent(
                xv[i - 1],
                xv[i],
                |x| f(AD(x, 1f64)).1,
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
    f: impl Fn(AD) -> AD,
    g: impl Fn(AD) -> AD,
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
    let f_roots = split_strictly_monotone(&f, xv, tol, max_rf_iters)?;
    let g_roots = split_strictly_monotone(&g, xv, tol, max_rf_iters)?;

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

pub fn generate_c(
    f: impl Fn(AD) -> AD,
    g: impl Fn(AD) -> AD,
    xv: &[f64],
) -> Result<impl Fn(f64) -> f64, Display2DError> {
    let mut xv: Vec<f64> = xv.into_iter().copied().collect();

    let mut yv: Vec<f64> = xv.iter().map(|x| f(AD(*x, 0f64)).0).collect();

    if !is_strictly_monotone(&yv[..]) {
        return Err(Display2DError::NonMonotone);
    }

    if yv[0] > yv[yv.len() - 1] {
        yv.reverse();
        xv.reverse();
    }

    let mut cyv: Vec<f64> = xv.iter().map(|x| g(AD(*x, 0f64)).0).collect();

    if !is_strictly_monotone(&cyv[..]) {
        return Err(Display2DError::NonMonotone);
    }

    cyv.iter_mut()
        .zip(xv.iter_mut())
        .for_each(|(cy, x)| *cy = *x - *cy);

    let c = cubic_hermite_spline(&yv[..], &cyv[..], SlopeMethod::Quadratic);
    Ok(move |y: f64| c.eval(y))
}
