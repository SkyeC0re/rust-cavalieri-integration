use std::{cmp::max, collections::BTreeSet};

use ordered_float::OrderedFloat;

use crate::errors::IntegError;

const LEGENDRE_NODES_WEIGHTS_10: [(f64, f64); 5] = [
    (0.148874338981631211, 0.29552422471475287),
    (0.433395394129247191, 0.269266719309996355),
    (0.679409568299024406, 0.21908636251598204),
    (0.865063366688984511, 0.14945134915058059),
    (0.97390652851717172, 0.066671344308688138),
];

const KRONROD_NODES_WEIGHTS_21: [(f64, f64); 11] = [
    (0f64, 0.149445554002916906),
    (0.148874338981631211, 0.14773910490133849),
    (0.294392862701460198, 0.142775938577060081),
    (0.433395394129247191, 0.13470921731147333),
    (0.562757134668604683, 0.12349197626206585),
    (0.679409568299024406, 0.109387158802297642),
    (0.780817726586416897, 0.09312545458369761),
    (0.865063366688984511, 0.07503967481091995),
    (0.930157491355708226, 0.054755896574351996),
    (0.97390652851717172, 0.032558162307964727),
    (0.995657163025808081, 0.011694638867371874),
];
#[derive(PartialEq, PartialOrd, Clone)]
struct ApproxInterval {
    pub err: f64,
    pub val: f64,
    pub a: f64,
    pub b: f64,
}

impl Eq for ApproxInterval {}

impl Ord for ApproxInterval {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Equal)
    }
}

/// A globally adaptive 10-21 Gauss Kronrod integration routine.
///
/// Returns the integration value and estimated error of the integration.
///
/// * `f` - The function to integrate
/// * `a` - Lower integration bound
/// * `b` - Upper integration bound
/// * `tol` - The maximum estimated error allowed
/// * `max_iter` - The maximum amount of adaptive Gauss-Kronrod integration
///     iterations allowed
pub fn gauss_kronrod_quadrature(
    f: impl Fn(f64) -> f64,
    a: f64,
    b: f64,
    tol: f64,
    max_iter: Option<usize>,
) -> Result<(f64, f64), IntegError> {
    if a == b {
        return Ok((0f64, 0f64));
    }
    let gk_approx = |a: f64, b: f64| -> (f64, f64) {
        let li = symmetric_gauss_quadrature(&f, a, b, &LEGENDRE_NODES_WEIGHTS_10);
        let ki = symmetric_gauss_quadrature(&f, a, b, &KRONROD_NODES_WEIGHTS_21);
        (ki, (li - ki).abs())
    };
    let (val, mut accu_err) = gk_approx(a, b);
    let mut approx_intvls: BTreeSet<ApproxInterval> = BTreeSet::new();
    approx_intvls.insert(ApproxInterval {
        err: accu_err,
        val,
        a,
        b,
    });
    for _ in 0..max_iter.unwrap_or(usize::MAX) {
        if accu_err.is_nan() {
            return Err(IntegError::NaNError);
        } else if accu_err < tol {
            return Ok((approx_intvls.into_iter().map(|v| v.val).sum(), accu_err));
        }
        match approx_intvls.iter().last() {
            Some(intvl) => {
                let intvl = intvl.clone();
                if intvl.a != intvl.b {
                    accu_err -= intvl.err;
                    let m = (intvl.a + intvl.b) / 2f64;
                    let left = gk_approx(intvl.a, m);
                    let right = gk_approx(m, intvl.b);
                    accu_err += left.1 + right.1;
                    approx_intvls.insert(ApproxInterval {
                        err: left.1,
                        val: left.0,
                        a: intvl.a,
                        b: m,
                    });
                    approx_intvls.insert(ApproxInterval {
                        err: right.1,
                        val: right.0,
                        a: m,
                        b: intvl.b,
                    });
                }
                approx_intvls.remove(&intvl);
            }
            None => break,
        }
    }
    Err(IntegError::ConvergenceError)
}

/// A globally adaptive two dimensional 10-21 Gauss Kronrod integration routine.
///
/// Returns the integration value and estimated error of the integration.
///
/// * `f` - The function to integrate
/// * `a` - The outer integral's lower integration bound
/// * `b` - The outer integral's upper integration bound
/// * `inner_ab_fn` - A function which specifies the inner integral
///     lower and upper bounds in terms of the outer integral's bounds
/// * `tol` - The maximum estimated error allowed
/// * `max_iter` - The maximum amount of adaptive Gauss-Kronrod integration
///     iterations allowed for the inner and outer integrals
pub fn gauss_kronrod_quadrature_2d(
    f: impl Fn([f64; 2]) -> f64,
    a: f64,
    b: f64,
    inner_ab_fn: impl Fn(f64) -> [f64; 2],
    tol: f64,
    max_iter: Option<usize>,
) -> Result<(f64, f64), IntegError> {
    if a == b {
        return Ok((0f64, 0f64));
    }
    let gk_approx = |a: f64, b: f64| -> Result<(f64, f64), IntegError> {
        let li = nested_gauss_kronrod_quadrature(
            &f,
            a,
            b,
            &inner_ab_fn,
            tol / 2f64,
            max_iter,
            &LEGENDRE_NODES_WEIGHTS_10,
        )?;
        let ki = nested_gauss_kronrod_quadrature(
            &f,
            a,
            b,
            &inner_ab_fn,
            tol / 2f64,
            max_iter,
            &KRONROD_NODES_WEIGHTS_21,
        )?;
        Ok((
            ki.0,
            (li.0 - ki.0).abs() + max(OrderedFloat(li.1), OrderedFloat(ki.1)).0,
        ))
    };
    let (val, mut accu_err) = gk_approx(a, b)?;
    let mut approx_intvls: BTreeSet<ApproxInterval> = BTreeSet::new();
    approx_intvls.insert(ApproxInterval {
        err: accu_err,
        val,
        a,
        b,
    });
    for _ in 0..max_iter.unwrap_or(usize::MAX) {
        if accu_err.is_nan() {
            return Err(IntegError::NaNError);
        } else if accu_err < tol {
            return Ok((approx_intvls.into_iter().map(|v| v.val).sum(), accu_err));
        }
        match approx_intvls.iter().last() {
            Some(intvl) => {
                let intvl = intvl.clone();
                if intvl.a != intvl.b {
                    accu_err -= intvl.err;
                    let m = (intvl.a + intvl.b) / 2f64;
                    let left = gk_approx(intvl.a, m)?;
                    let right = gk_approx(m, intvl.b)?;
                    accu_err += left.1 + right.1;
                    approx_intvls.insert(ApproxInterval {
                        err: left.1,
                        val: left.0,
                        a: intvl.a,
                        b: m,
                    });
                    approx_intvls.insert(ApproxInterval {
                        err: right.1,
                        val: right.0,
                        a: m,
                        b: intvl.b,
                    });
                }
                approx_intvls.remove(&intvl);
            }
            None => break,
        }
    }
    Err(IntegError::ConvergenceError)
}

pub fn gauss_kronrod_quadrature_triangle(
    f: impl Fn([f64; 2]) -> f64,
    triag: [[f64; 2]; 3],
    tol: f64,
    max_iter: Option<usize>,
) -> Result<(f64, f64), IntegError> {
    let c = (triag[1][0] * (triag[2][1] - triag[0][1])
        + triag[0][0] * (triag[1][1] - triag[2][1])
        + triag[2][0] * (triag[0][1] - triag[1][1]))
        .abs();

    let f = |u: [f64; 2]| {
        c * f([
            (1f64 - u[0] - u[1]) * triag[0][0] + u[0] * triag[1][0] + u[1] * triag[2][0],
            (1f64 - u[0] - u[1]) * triag[0][1] + u[0] * triag[1][1] + u[1] * triag[2][1],
        ])
    };
    gauss_kronrod_quadrature_2d(f, 0f64, 1f64, |u1| [0f64, 1f64 - u1], tol, max_iter)
}

pub fn symmetric_gauss_quadrature(
    f: impl Fn(f64) -> f64,
    a: f64,
    b: f64,
    rule: &[(f64, f64)],
) -> f64 {
    (b - a) / 2f64
        * unit_symmetric_gauss_quadrature(|x| f(x * (b - a) / 2f64 + (a + b) / 2f64), rule)
}

pub fn unit_symmetric_gauss_quadrature(f: impl Fn(f64) -> f64, rule: &[(f64, f64)]) -> f64 {
    let mut s = 0f64;
    if rule.len() == 0 {
        return s;
    }
    let i = if rule[0].0 == 0f64 {
        s += rule[0].1 * f(rule[0].0);
        1
    } else {
        0
    };
    for &(n, w) in &rule[i..] {
        s += w * (f(-n) + f(n));
    }
    s
}

pub fn nested_gauss_kronrod_quadrature(
    f: impl Fn([f64; 2]) -> f64,
    a: f64,
    b: f64,
    inner_ab_fn: impl Fn(f64) -> [f64; 2],
    tol: f64,
    max_iter: Option<usize>,
    rule: &[(f64, f64)],
) -> Result<(f64, f64), IntegError> {
    let outer_denormalizer = |x: f64| x * (b - a) / 2f64 + (a + b) / 2f64;
    let outer_unit_f = |xy: [f64; 2]| f([outer_denormalizer(xy[0]), xy[1]]);
    let mut s = 0f64;
    let mut accu_err = 0f64;
    if rule.len() == 0 {
        return Ok((s, accu_err));
    }
    let i = if rule[0].0 == 0f64 {
        let [ia, ib] = inner_ab_fn(outer_denormalizer(rule[0].0));
        let res =
            gauss_kronrod_quadrature(|y| outer_unit_f([rule[0].0, y]), ia, ib, tol, max_iter)?;
        s += rule[0].1 * res.0;
        accu_err += rule[0].1 * res.1;
        1
    } else {
        0
    };
    for &(n, w) in &rule[i..] {
        let [ia, ib] = inner_ab_fn(outer_denormalizer(-n));
        let nres = gauss_kronrod_quadrature(|y| outer_unit_f([-n, y]), ia, ib, tol, max_iter)?;
        let [ia, ib] = inner_ab_fn(outer_denormalizer(n));
        let pres = gauss_kronrod_quadrature(|y| outer_unit_f([n, y]), ia, ib, tol, max_iter)?;
        s += w * (nres.0 + pres.0);
        accu_err += w * (nres.1 + pres.1);
    }
    Ok(((b - a) / 2f64 * s, (b - a) / 2f64 * accu_err))
}
