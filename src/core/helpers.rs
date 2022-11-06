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

pub fn n_linspace<const N: usize>(a: &[f64; N], b: &[f64; N], mut length: usize) -> Vec<[f64; N]> {
    if length < 2 {
        length = 2
    };
    let mut vals = Vec::with_capacity(length);
    for p in 0..length {
        let c = (p as f64) / ((length - 1) as f64);
        let mut pos = [0f64; N];
        for i in 0..N {
            pos[i] = (1f64 - c) * a[i] + c * b[i];
        }
        vals.push(pos);
    }
    vals
}

pub fn linspace(a: f64, b: f64, mut length: usize) -> Vec<f64> {
    if length < 2 {
        length = 2
    };
    let mut vals = Vec::with_capacity(length);
    for p in 0..length {
        let c = (p as f64) / ((length - 1) as f64);
        vals.push((1f64 - c) * a + c * b);
    }
    vals
}

pub fn vec_from_res(a: f64, b: f64, res: usize) -> Vec<f64> {
    linspace(a, b, res + 1)
}

pub fn n_vec_from_res<const N: usize>(a: &[f64; N], b: &[f64; N], res: usize) -> Vec<[f64; N]> {
    n_linspace(a, b, res + 1)
}
