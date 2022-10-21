use std::ops::{Add, Sub, Neg, Mul, Div};


#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
pub struct AD(pub f64, pub f64);

pub const ONE: AD = AD(1f64, 0f64);
pub const ZERO: AD = AD(0f64, 0f64);
impl AD {
    #[inline(always)]
    fn zdf1(self) -> bool {
        self.1 == 0f64
    }

    #[inline(always)]
    fn zdf2(self, rhs: AD) -> bool {
        self.1 == 0f64 && rhs.1 == 0f64
    }
}

impl From<f64> for AD {
    #[inline(always)]
    fn from(x: f64) -> Self {
        AD(x, 0f64)
    }
}

impl Add<AD> for AD {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        AD(self.0 + rhs.0, if self.zdf2(rhs) {0f64} else {self.1 + rhs.1})
    }
}

impl Sub<AD> for AD {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        AD(self.0 - rhs.0, if self.zdf2(rhs) {0f64} else {self.1 - rhs.1})
    }
}

impl Neg for AD {
    type Output = Self;
    fn neg(self) -> Self::Output {
        AD(-self.0, if self.zdf1() {0f64} else {-self.1})
    }
}

impl Mul for AD {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        AD(self.0 * rhs.0, if self.zdf2(rhs) {0f64} else {self.0 * rhs.1 + self.1 * rhs.0})
    }
}

impl Div for AD {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
    
        AD(self.0 / rhs.0, if self.zdf2(rhs) {0f64} else {self.1 / rhs.0  - (self.0 * rhs.1) / (rhs.0 * rhs.0)})
    }
}

impl AD {
    pub fn abs(self) -> Self {
        AD(self.0.abs(), if self.zdf1() {0f64} else {self.1.abs()})
    }

    pub fn ln(self) -> Self {
        AD(self.0.ln(), if self.zdf1() {0f64} else {self.1 / self.0})
    }

    pub fn sqrt(self) -> Self {
        let rt = self.0.sqrt();
        AD(rt, if self.zdf1() {0f64} else {self.1 / (2f64 * rt)})
    }

    pub fn exp(self) -> Self {
        let p = self.0.exp();
        AD(p, if self.zdf1() {0f64} else {p * self.1})
    }

    pub fn pow(self, rhs: Self) -> Self {
        if self.zdf1() {AD(self.0.powf(rhs.0), 0f64)} else {
        (self.ln() * rhs).exp()
        }
    }

    pub fn sin(self) -> Self {
        AD(self.0.sin(), if self.zdf1() {0f64} else {self.0.cos() * self.1})
    }

    pub fn cos(self) -> Self {
        AD(self.0.cos(), if self.zdf1() {0f64} else {-self.0.sin() * self.1})
    }

    pub fn tan(self) -> Self {
        AD(self.0.tan(), if self.zdf1() {0f64} else {self.1 / self.0.cos().powi(2)})
    }

    pub fn asin(self) -> Self {
        AD(self.0.asin(), if self.zdf1() {0f64} else {self.1 / (self.0* self.0 - 1f64).sqrt()})
    }

    pub fn acos(self) -> Self {
        AD(self.0.acos(), if self.zdf1() {0f64} else {-self.1 / (self.0* self.0 - 1f64).sqrt()})
    }

    pub fn atan(self) -> Self {
        AD(self.0.atan(), if self.zdf1() {0f64} else {self.1 / (self.0* self.0 + 1f64)})
    }

    pub fn sinh(self) -> Self {
        AD(self.0.sinh(), if self.zdf1() {0f64} else {self.0.cosh() * self.1})
    }

    pub fn cosh(self) -> Self {
        AD(self.0.cosh(), if self.zdf1() {0f64} else {-self.0.sinh() * self.1})
    }

    pub fn tanh(self) -> Self {
        AD(self.0.tanh(), if self.zdf1() {0f64} else {self.1 / self.0.cosh().powi(2)})
    }

    pub fn asinh(self) -> Self {
        AD(self.0.asinh(), if self.zdf1() {0f64} else {self.1 / (self.0 * self.0 + 1f64).sqrt()})
    }

    pub fn acosh(self) -> Self {
        AD(self.0.acosh(), if self.zdf1() {0f64} else {self.1 / (self.0* self.0 - 1f64).sqrt()})
    }

    pub fn atanh(self) -> Self {
        AD(self.0.atanh(), if self.zdf1() {0f64} else {self.1 / (1f64 - self.0 * self.0)})
    }
}