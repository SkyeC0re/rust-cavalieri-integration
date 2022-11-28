use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
pub struct AD(pub f64, pub f64);

pub const ONE: AD = AD(1f64, 0f64);
pub const ZERO: AD = AD(0f64, 0f64);

impl From<f64> for AD {
    #[inline(always)]
    fn from(x: f64) -> Self {
        AD(x, 0f64)
    }
}

impl From<(f64, f64)> for AD {
    fn from(xdx: (f64, f64)) -> Self {
        Self(xdx.0, xdx.1)
    }
}

impl Into<(f64, f64)> for AD {
    fn into(self) -> (f64, f64) {
        (self.0, self.1)
    }
}

impl Add<AD> for AD {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        AD(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl Sub<AD> for AD {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        AD(self.0 - rhs.0, self.1 - rhs.1)
    }
}

impl Neg for AD {
    type Output = Self;
    fn neg(self) -> Self::Output {
        AD(-self.0, -self.1)
    }
}

impl Mul for AD {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        AD(self.0 * rhs.0, self.0 * rhs.1 + self.1 * rhs.0)
    }
}

impl Div for AD {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        AD(
            self.0 / rhs.0,
            self.1 / rhs.0 - (self.0 * rhs.1) / (rhs.0 * rhs.0),
        )
    }
}

impl AD {
    pub fn abs(self) -> Self {
        AD(self.0.abs(), if self.0 < 0f64 { -self.1 } else { self.1 })
    }

    pub fn ln(self) -> Self {
        AD(self.0.ln(), self.1 / self.0)
    }

    pub fn sqrt(self) -> Self {
        let rt = self.0.sqrt();
        AD(rt, self.1 / (2f64 * rt))
    }

    pub fn exp(self) -> Self {
        let p = self.0.exp();
        AD(p, p * self.1)
    }

    pub fn pow(self, rhs: Self) -> Self {
        (self.ln() * rhs).exp()
    }

    pub fn powi(self, n: i32) -> Self {
        let p = self.0.powi(n - 1);
        AD(self.0 * p, (n as f64) * p * self.1)
    }

    pub fn sin(self) -> Self {
        AD(self.0.sin(), self.0.cos() * self.1)
    }

    pub fn cos(self) -> Self {
        AD(self.0.cos(), -self.0.sin() * self.1)
    }

    pub fn tan(self) -> Self {
        AD(self.0.tan(), self.1 / self.0.cos().powi(2))
    }

    pub fn asin(self) -> Self {
        AD(self.0.asin(), self.1 / (1f64 - self.0.powi(2)).sqrt())
    }

    pub fn acos(self) -> Self {
        AD(self.0.acos(), -self.1 / (1f64 - self.0.powi(2)).sqrt())
    }

    pub fn atan(self) -> Self {
        AD(self.0.atan(), self.1 / (self.0.powi(2) + 1f64))
    }

    pub fn sinh(self) -> Self {
        AD(self.0.sinh(), self.0.cosh() * self.1)
    }

    pub fn cosh(self) -> Self {
        AD(self.0.cosh(), self.0.sinh() * self.1)
    }

    pub fn tanh(self) -> Self {
        AD(self.0.tanh(), self.1 / self.0.cosh().powi(2))
    }

    pub fn asinh(self) -> Self {
        AD(self.0.asinh(), self.1 / (self.0.powi(2) + 1f64).sqrt())
    }

    pub fn acosh(self) -> Self {
        AD(self.0.acosh(), self.1 / (self.0.powi(2) - 1f64).sqrt())
    }

    pub fn atanh(self) -> Self {
        AD(self.0.atanh(), self.1 / (1f64 - self.0 * self.0))
    }
}

pub trait Differentiable1D {
    fn f(&self, x: f64) -> f64;
    fn df(&self, x: f64) -> f64;
    fn fdf(&self, x: f64) -> (f64, f64) {
        (self.f(x), self.df(x))
    }

    fn composition(&self, gdg: (f64, f64)) -> (f64, f64) {
        let mut fdf = self.fdf(gdg.0);
        fdf.1 *= gdg.1;
        fdf
    }
}

impl<F: Fn(AD) -> AD> Differentiable1D for F {
    fn f(&self, x: f64) -> f64 {
        self(AD(x, 0f64)).0
    }

    fn df(&self, x: f64) -> f64 {
        self(AD(x, 1f64)).1
    }

    fn fdf(&self, x: f64) -> (f64, f64) {
        let fx = self(AD(x, 1f64));
        (fx.0, fx.1)
    }

    fn composition(&self, gdg: (f64, f64)) -> (f64, f64) {
        self(gdg.into()).into()
    }
}

pub fn abs_jacobian_det(g: impl Fn([AD; 2]) -> [AD; 2], x: [f64; 2]) -> f64 {
    let c1 = g([AD(x[0], 1f64), AD(x[1], 0f64)]);
    let c2 = g([AD(x[0], 0f64), AD(x[1], 1f64)]);

    (c1[0].1 * c2[1].1 - c1[1].1 * c2[0].1).abs()
}
