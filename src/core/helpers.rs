use peroxide::prelude::linspace;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Sign {
    NEG,
    ZERO,
    POS,
    NAN,
}

pub trait Signed: From<Sign> {
    fn sign(&self) -> Sign;

    fn sign_val(&self) -> Self {
        Self::from(self.sign())
    }
}

impl From<Sign> for f64 {
    fn from(s: Sign) -> Self {
        match s {
            Sign::POS => 1.0,
            Sign::ZERO => 0.0,
            Sign::NEG => -1.0,
            Sign::NAN => f64::NAN,
        }
    }
}

impl Signed for f64 {
    fn sign(&self) -> Sign {
        if self.is_nan() {
            return Sign::NAN;
        }
        if self.is_sign_positive() {
            return Sign::POS;
        }
        if self.is_sign_negative() {
            return Sign::NEG;
        }
        Sign::ZERO
    }
}

pub fn vec_from_res_func(a: f64, b: f64, res: impl Fn(f64, f64) -> usize) -> Vec<f64> {
    let res = res(a, b);
    linspace(a, b, res)
}
