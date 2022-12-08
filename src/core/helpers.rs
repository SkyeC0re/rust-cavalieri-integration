#[derive(PartialEq, Eq, Clone, Copy)]
/// An enum for representing the sign of a numeric type.
pub enum Sign {
    NEG,
    ZERO,
    POS,
    NAN,
}

/// A trait for determining the sign of a numeric type.
pub trait Signed: From<Sign> {
    /// The sign of the numeric type.
    fn sign(&self) -> Sign;

    /// A number of the numeric type representing the numeric type's sign.
    /// 
    /// Should be the multiplicative identity element for `POS`, its additive inverse of the
    /// aforementioned for `NEG` and the additive identity element for `ZERO`.
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

/// Generates a vector of linearly spaced N dimensional points over an N+1 
/// dimensional hypercube from a resolution.
/// 
/// Includes both N dimensional endpoints of the hypercube and will always generate at least 2 points.
/// 
/// * `a` - Start of the interval
/// * `b` - End of the interval
/// * `length` - The amount of points to generate
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

/// Generates a vector of linearly spaced points on an interval.
/// 
/// Includes both endpoints and will always generate at least 2 points.
/// 
/// * `a` - Start of the interval
/// * `b` - End of the interval
/// * `length` - The amount of points to generate
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

/// Generates a vector of linearly spaced points on an interval
/// from a resolution.
/// 
/// Includes both endpoints and will always generate at least 2 points.
/// 
/// * `a` - Start of the interval
/// * `b` - End of the interval
/// * `res` - Generates `res + 1` points
pub fn vec_from_res(a: f64, b: f64, res: usize) -> Vec<f64> {
    linspace(a, b, res + 1)
}

/// Generates a vector of linearly spaced N dimensional points over an N+1 
/// dimensional hypercube from a resolution.
/// 
/// Includes both N dimensional endpoints of the hypercube and will always generate at least 2 points.
/// 
/// * `a` - Start of the interval
/// * `b` - End of the interval
/// * `res` - Generates `res + 1` points
pub fn n_vec_from_res<const N: usize>(a: &[f64; N], b: &[f64; N], res: usize) -> Vec<[f64; N]> {
    n_linspace(a, b, res + 1)
}
