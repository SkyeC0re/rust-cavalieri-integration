use std::{cmp::{max_by, min_by, Ordering}, borrow::Borrow};

use peroxide::{prelude::{AD::{self, AD0, AD1, AD2}, false_position, cubic_spline, Spline}, fuga::{RootFind, RootFinder}};

use crate::{core::{helpers::{Signed, Sign}}, errors::MonotoneSplitError};

struct Interval {
    pub a: f64,
    pub b: f64,
}

pub struct CavIntConfig {
    pub make_cav: bool,
    pub make_rs: bool,
    pub make_g: bool,
    pub make_dg: bool,
    pub compute_integ: bool,
    pub x_res_gen: Box<dyn FnMut(f64, f64) -> Vec<f64>>,
    pub y_res_gen: Box<dyn FnMut(f64, f64) -> Vec<f64>>,
}

pub struct CavInterval {
    pub a: f64,
    pub b: f64,
}

pub struct TRegion {
    pub c: Box<dyn Fn(f64) -> f64>,
    pub a: f64,
    pub b: f64,
    pub xv: Vec<f64>,
}

pub fn gen_integ_interval_cav(
    f: impl Fn(AD) -> AD,
    c: impl Fn(AD) -> AD,
    a: f64,
    b: f64,
    cfg: CavIntConfig,
) -> CavInterval {
    todo!()
}


pub fn gen_func_interval(
    f: impl Fn(AD) -> AD,
    a: f64,
    b: f64,
    x_res_gen: impl Fn(f64, f64) -> Vec<f64>,
) -> (Vec<f64>, Vec<f64>) {
    let xv = x_res_gen(a, b);

    let yv = xv
    .iter()
    .map(|x| f(AD0(*x)).x())
    .collect();
   
    (xv, yv)
}

pub fn is_monotic_saddle(
    f: impl Fn(AD) -> AD,
    x: f64,
    tol: f64
) -> bool {
    let ad = f(AD2(x, 1.0, 0.0));
    if ad.dx() < tol && ad.ddx() < tol {
        true
    } else {
        false
    }
}

pub fn find_monotone_splits(
    f: impl Fn(AD) -> AD,
    a: f64,
    b: f64,
    x_res_gen: impl Fn(f64, f64) -> Vec<f64>,
    tol: f64,
    max_hint_bisections: usize,
    max_rf_iters: usize,
) -> Result<Vec<f64>, MonotoneSplitError> {
    let mut xv = x_res_gen(a, b);
    let xv_len = xv.len();
    if xv_len <= 2 {
        return Ok(vec![]);
    }

    let x_sign = (b-a).sign_val();
    xv[0] = max_by(xv[0], xv[0] + x_sign * tol, |v1, v2| 
        (x_sign * *v1).total_cmp(&(x_sign * *v2)) 
    );

    xv[xv_len - 1] = min_by(xv[0], xv[0] - x_sign * tol, |v1, v2| 
        (x_sign * *v1).total_cmp(&(x_sign * *v2)) 
    );

    let mut dfv: Vec<(f64, f64)> = xv
    .iter()
    .map(|x| {
        match f(AD1(*x, 1f64)) {ad => (ad.x(), ad.dx())}
    })
    .collect();
    let mut roots = vec![];
    let mut i = 1;

    while i < xv.len() {
        let ldfs = dfv[i-1].1.sign();
        let rdfs = dfv[i].1.sign();
        let root = if ldfs == Sign::NAN || (ldfs == Sign::ZERO &&  !is_monotic_saddle(&f, dfv[i-1].0, tol)) {
            dfv[i-1].0
        } else if rdfs == Sign::NAN || (rdfs == Sign::ZERO &&  !is_monotic_saddle(&f, dfv[i].0, tol)) {
            dfv[i].0
        } else if ldfs != rdfs {
            false_position(&f, (dfv[i-1].0, dfv[i].0), max_rf_iters, tol)?
        } else {
            i += 1;
            continue;
        };

        let rfwd = root + x_sign * tol;
            if x_sign * (rfwd - xv[i]) > 0f64 {i += 1}
            if rfwd != xv[i-1] {
                xv[i-1] = rfwd;
                dfv[i-1] = match f(AD1(rfwd, 1f64)) {ad => (ad.x(), ad.dx())}
            }
            roots.push(root);
    }

    Ok(roots)
}

pub fn is_strictly_monotone(v: &[f64]) -> bool {
    if v.len() < 2 {return true;}
    let expected_sign = match (v[1] - v[0]).sign() {
        Sign::POS => Sign::POS,
        Sign::NEG => Sign::NEG,
        _ => return false,
    };
    (2..v.len())
    .into_iter()
    .all(|i|  (v[i] - v[i-1]).sign() == expected_sign)
}

pub fn split_translational(
    f: impl Fn(AD) -> AD,
    g: impl Fn(AD) -> AD,
    a: f64,
    b: f64,
    x_res_gen: impl Fn(f64, f64) -> Vec<f64>,
    tol: f64,
    max_hint_bisections: usize,
    max_rf_iters: usize,
) -> Result<Vec<TRegion>, MonotoneSplitError> {
    let x_sign = (b-a).sign_val();
    let f_roots = find_monotone_splits(&f, a, b, &x_res_gen,
        tol, max_hint_bisections, max_rf_iters)?;
    let g_roots = find_monotone_splits(&f, a, b, &x_res_gen,
        tol, max_hint_bisections, max_rf_iters)?;

    let mut merged_roots: Vec<f64> = f_roots
    .into_iter()
    .chain(g_roots.into_iter())
    .collect();

    merged_roots.sort_by(|a, b| {
        let sdiff = x_sign * (a-b);
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
        for (i, r) in merged_roots
            .iter()
            .enumerate() {
            if (*r - merged_roots[li]).abs() > 2.0 * tol {
                roots.push(merged_roots[li..i].iter().copied().sum::<f64>() / (i - li) as f64);
                li = i;
            }
        }
        roots.push(merged_roots[li..].iter().copied().sum::<f64>() / (merged_roots.len() - li) as f64);
        roots    
    } else {
        merged_roots
    };

    let mut t_regions = Vec::with_capacity(roots.len() - 1);
    for i in 1..roots.len() {
        let a = roots[i-1];
        let b = roots[i];
        let xv = x_res_gen(a, b);
        let y: Vec<f64> = xv
            .iter()
            .map(|x| f(AD0(*x)).x())
            .collect();

        if !is_strictly_monotone(&y[..]) {
            return Err(MonotoneSplitError::NonMonotone)
        }

        let cy: Vec<f64> = xv
            .iter()
            .map(|x| (AD0(*x) - g(AD0(*x))).x())
            .collect();

        let c = cubic_spline(&y[..], &cy[..]);
        t_regions.push(
            TRegion {
                c: Box::new(move | y: f64 | c.eval(y)),
                a,
                b,
                xv,
            }
        );
    }
    Ok(t_regions)
}
