use std::ops::{Add, Div, Mul, Neg, Sub};

use super::parsing::{BasicArithmetic, Parsable};

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
        AD(self.0.abs(), self.1.abs())
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
        AD(self.0.asin(), self.1 / (self.0 * self.0 - 1f64).sqrt())
    }

    pub fn acos(self) -> Self {
        AD(self.0.acos(), -self.1 / (self.0 * self.0 - 1f64).sqrt())
    }

    pub fn atan(self) -> Self {
        AD(self.0.atan(), self.1 / (self.0 * self.0 + 1f64))
    }

    pub fn sinh(self) -> Self {
        AD(self.0.sinh(), self.0.cosh() * self.1)
    }

    pub fn cosh(self) -> Self {
        AD(self.0.cosh(), -self.0.sinh() * self.1)
    }

    pub fn tanh(self) -> Self {
        AD(self.0.tanh(), self.1 / self.0.cosh().powi(2))
    }

    pub fn asinh(self) -> Self {
        AD(self.0.asinh(), self.1 / (self.0 * self.0 + 1f64).sqrt())
    }

    pub fn acosh(self) -> Self {
        AD(self.0.acosh(), self.1 / (self.0 * self.0 - 1f64).sqrt())
    }

    pub fn atanh(self) -> Self {
        AD(self.0.atanh(), self.1 / (1f64 - self.0 * self.0))
    }
}
