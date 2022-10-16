use peroxide::{fuga::{integrate, Integral::G10K21, StableFn}, prelude::{ADFn, AD}};

pub fn cavs_interval<A, B> (
    f: ADFn<A>,
    c: ADFn<B>,
    interval: (f64, f64),
    tol: f64,
) -> f64 where
    A: Fn(AD) -> AD + Clone,
    B: Fn(AD) -> AD + Clone,
{
    let g = ADFn::new(|x: AD|
        x - c.call_stable(f.call_stable(x))
    );

    let dg = g.grad();


    integrate(
        |x: f64| f.call_stable(x) * dg.call_stable(x),
        interval,
        G10K21(tol),
    )
}

pub fn cavr_interval<A, B>(
    f: ADFn<A>,
    h: ADFn<B>,
    interval: (f64, f64),
    tol: f64,
) -> f64 where
    A: Fn(AD) -> AD + Clone,
    B: Fn(AD) -> AD + Clone,
{
    integrate(
        |x: f64| f.call_stable(h.call_stable(x)),
        interval,
        G10K21(tol),
    )
}

pub fn rs_interval<A, B>(
    f: ADFn<A>,
    g: ADFn<B>,
    interval: (f64, f64),
    tol: f64,
) -> f64 where
    A: Fn(AD) -> AD + Clone,
    B: Fn(AD) -> AD + Clone,
{
    let dg = g.grad();
    r_interval(
        ADFn::new(|x| f.call_stable(x) * dg.call_stable(x)),
        interval,
        tol,
    )
}

pub fn r_interval<A: Fn(AD) -> AD>(
    f: ADFn<A>,
    interval: (f64, f64),
    tol: f64,
) -> f64 {
    integrate(
        |x| f.call_stable(x),
        interval,
        G10K21(tol),
    )
}