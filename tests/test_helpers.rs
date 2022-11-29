use approx::assert_abs_diff_eq;

pub fn assert_float_iters_equal<I1, I2>(av: I1, bv: I2, tol: f64)
where
    I1: Iterator<Item = f64> + ExactSizeIterator,
    I2: Iterator<Item = f64> + ExactSizeIterator,
{
    assert_eq!(av.len(), bv.len());

    av.zip(bv).for_each(|(a, b)| {
        assert_abs_diff_eq!(a, b, epsilon = tol);
    })
}

pub fn assert_grids_equal<'a, const N: usize>(
    ag: &'a Vec<Vec<[f64; N]>>,
    bg: &'a Vec<Vec<[f64; N]>>,
    tol: f64,
) {
    assert_eq!(ag.len(), bg.len());

    ag.iter().zip(bg.iter()).for_each(|(av, bv)| {
        assert_eq!(av.len(), bv.len());
        av.iter().zip(bv.iter()).for_each(|(a, b)| {
            a.iter()
                .zip(b.iter())
                .for_each(|(xa, xb)| assert_abs_diff_eq!(*xa, *xb, epsilon = tol))
        });
    })
}

pub trait IteratorMean: Iterator
where
    Self: Sized,
{
    fn mean(self) -> f64;
}

impl<I: Iterator<Item = f64>> IteratorMean for I {
    fn mean(self) -> f64 {
        let avg_sum = self.fold((0usize, 0f64), |mut acc, x| {
            acc.0 += 1;
            acc.1 += x;
            acc
        });
        avg_sum.1 / avg_sum.0 as f64
    }
}
