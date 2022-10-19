use peroxide::{fuga::{integrate, Integral::G10K21, StableFn}, prelude::{ADFn, AD::{self, AD0, AD1}}};

pub fn integ_cavs_interval(
    f: impl Fn(AD) -> AD,
    c: impl Fn(AD) -> AD,
    interval: (f64, f64),
    tol: f64,
) -> f64 {
    let g = |x| x - c(f(x));

    integrate(
        |x: f64| f(AD0(x)).x() * g(AD1(x, 1.0)).dx(),
        interval,
        G10K21(tol),
    )
}

pub fn integ_cavr_interval(
    f: impl Fn(AD) -> AD,
    h: impl Fn(AD) -> AD,
    interval: (f64, f64),
    tol: f64,
) -> f64 {
    integrate(
        |x: f64| f(h(AD0(x))).x(),
        interval,
        G10K21(tol),
    )
}

pub fn integ_rs_interval(
    f: impl Fn(AD) -> AD,
    g: impl Fn(AD) -> AD,
    interval: (f64, f64),
    tol: f64,
) -> f64 {
    integrate(
        |x: f64| f(AD0(x)).x() * g(AD1(x, 1.0)).dx(),
        interval,
        G10K21(tol),
    )
}

pub fn integ_interval<A: Fn(AD) -> AD>(
    f: impl Fn(AD) -> AD,
    interval: (f64, f64),
    tol: f64,
) -> f64 {
    integrate(
        |x| f(AD0(x)).x(),
        interval,
        G10K21(tol),
    )
}