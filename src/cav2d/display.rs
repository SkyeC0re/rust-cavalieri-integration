use std::{
    borrow::Borrow,
    cmp::{max_by, min_by, Ordering},
};

use peroxide::{
    fuga::{RootFind, RootFinder},
    prelude::{
        cubic_spline, false_position, linspace, Spline,
    },
};
use pyo3::pyclass;
use roots::{find_root_brent, SimpleConvergency};

use crate::{
    core::{helpers::{Sign, Signed}, differentiable::{AD, ZERO}, integrate::gauss_kronrod_quadrature},
    errors::Display2DError,
};

use super::integrate::integ_cavs_interval;


#[pyclass]
pub struct CavDisplay {
    #[pyo3(get)]
    pub a: f64,
    #[pyo3(get)]
    pub b: f64,
    #[pyo3(get)]
    pub cav_grid: Vec<Vec<f64>>,
    #[pyo3(get)]
    pub xv: Vec<f64>,
    #[pyo3(get)]
    pub dgv: Vec<f64>,
    #[pyo3(get)]
    pub integ_value: Option<(f64, f64)>,
}

pub struct DisplayConfig {
    pub compute_integ: bool,
    pub x_res_gen: Box<dyn Fn(f64, f64) -> Vec<f64>>,
    pub y_res_gen: Box<dyn Fn(f64, f64) -> Vec<f64>>,
    pub max_rf_iters: usize,
    pub max_int_iters: usize,
    pub tol: f64,
}

impl Default for DisplayConfig {
    fn default() -> Self {
        Self {
            compute_integ: false,
            x_res_gen: Box::new(|a: f64, b: f64| linspace(a, b, 100)),
            y_res_gen: Box::new(|a: f64, b: f64| linspace(0f64, 1f64, 100)),
            max_rf_iters: 500,
            max_int_iters: 500,
            tol: 1e-9,
        }
    }
}

impl DisplayConfig {
    pub fn display(
        &self,
        f: impl Fn(AD) -> AD,
        c: impl Fn(AD) -> AD,
        a: f64,
        b: f64,
    ) -> Result<CavDisplay, Display2DError> {
        let xv = (self.x_res_gen)(a, b);
        let yrv = (self.y_res_gen)(a, b);

        let g = |x: AD| x - f(c(x));

        let dgv = xv.iter().copied().map(|x| g(AD(x, 1f64)).0).collect();

        let integ_value = if self.compute_integ {
            Some(gauss_kronrod_quadrature(
                |x| {
                    let y = f(AD(x, 1f64));
                    y.0 * (1f64 - c(y).1)
                }, a, b, self.tol, Some(self.max_int_iters),
            )?)
        } else {
            None
        };

        Ok(CavDisplay {
            a,
            b,
            cav_grid: gen_display_grid_cav(&f, &c, &xv[..], &yrv[..])?,
            xv,
            dgv,
            integ_value,
        })
    }
}

pub struct TRegion {
    pub c: Box<dyn Fn(f64) -> f64>,
    pub a: f64,
    pub b: f64,
    pub xv: Vec<f64>,
}

pub fn gen_display_grid_cav(
    f: impl Fn(AD) -> AD,
    c: impl Fn(AD) -> AD,
    xv: &[f64],
    yrv: &[f64],
) -> Result<Vec<Vec<f64>>, Display2DError> {
    if yrv.len() < 2 {
        return Err(Display2DError::BadInput(
            "Expected at least 2 point in the y resolution vector.".to_string(),
        ));
    }
    if yrv[0] != 0f64 {
        return Err(Display2DError::BadInput(format!(
            "Expected y ratio to start at 0.0, but found {}",
            yrv[0]
        )));
    }
    if yrv[yrv.len() - 1] != 1f64 {
        return Err(Display2DError::BadInput(format!(
            "Expected y ratio to end at 1.0, but found {}",
            yrv[yrv.len() - 1]
        )));
    }

    let c_0 = c(ZERO).0;
    let c = |y: f64| c(AD(y, 0f64)).0 - c_0;
    let f = |x: f64| f(AD(x, 0f64)).0;

    let g = |x: f64| x - c(f(x));
    let xrv: Vec<f64> = xv.into_iter().copied().map(|x| g(x)).collect();

    if !is_strictly_monotone(&xrv[..]) {
        return Err(Display2DError::NonMonotone);
    }

    Ok(yrv
        .into_iter()
        .copied()
        .map(|yr| {
            xrv.iter()
                .zip(xv.into_iter())
                .map(|(&xr, &x)| xr + c(yr * f(x)))
                .collect::<Vec<_>>()
        })
        .collect())
}

pub fn gen_display_interval_cav(
    f: impl Fn(AD) -> AD,
    c: impl Fn(AD) -> AD,
    a: f64,
    b: f64,
    cfg: DisplayConfig,
) -> Result<Vec<CavDisplay>, Display2DError> {
    let g = |x: AD| x - f(c(x));
    let xv = (cfg.x_res_gen)(a, b);
    let mut splits = split_translational(&f, g, &xv[..], cfg.tol, cfg.max_rf_iters)?;
    splits.insert(0, a);
    splits.push(b);
    let mut displays = Vec::with_capacity(splits.len() - 1);
    for i in 1..splits.len() {
        displays.push(cfg.display(&f, &c, splits[i-1], splits[i])?);
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

    if  x1.1.sign_val() == x2.1.sign_val() && x1.1.sign_val() == (x2.0 - x1.0).sign_val() {
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

    let dfv: Vec<AD> = xv
        .iter()
        .map(|x| {
            f(AD(*x, 1f64))
        })
        .collect();
    let mut roots = vec![];
    let mut i = 1;

    while i < xv.len() {
        let ldfs = dfv[i - 1].1.sign();
        let rdfs = dfv[i].1.sign();
        if ldfs == Sign::NAN
            || (ldfs == Sign::ZERO && !is_monotic_saddle(&f, xv[i - 1], tol))
        {
            roots.push(dfv[i - 1].0);
        } else if rdfs == Sign::NAN || (rdfs == Sign::ZERO && !is_monotic_saddle(&f, xv[i], tol))
        {
            i += 1;
            roots.push(dfv[i].0);
           
        } else if ldfs != rdfs {
            let r = find_root_brent(xv[i - 1], xv[i], |x| f(AD(x, 1f64)).1 , &mut SimpleConvergency {
                eps: tol,
                max_iter: max_rf_iters,
            })?;
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
    let mut roots = if merged_roots.len() > 1 {
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
    a: f64,
    b: f64,
    x_res_gen: impl Fn(f64, f64) -> Vec<f64>,
    tol: f64,
) -> Result<TRegion, Display2DError> {
    let mut xv = x_res_gen(a, b);
    let yv: Vec<f64> = xv.iter().map(|x| f(AD(*x, 0f64)).0).collect();

    if !is_strictly_monotone(&yv[..]) {
        return Err(Display2DError::NonMonotone);
    }

    let mut cyv: Vec<f64> = xv.iter().map(|x| g(AD(*x, 0f64)).0).collect();

    if !is_strictly_monotone(&cyv[..]) {
        return Err(Display2DError::NonMonotone);
    }

    cyv.iter_mut()
        .zip(xv.iter_mut())
        .for_each(|(cy, x)| *cy = *x - *cy);

    let c = cubic_spline(&yv[..], &cyv[..]);
    Ok(TRegion {
        c: Box::new(move |y: f64| c.eval(y)),
        a,
        b,
        xv,
    })
}
