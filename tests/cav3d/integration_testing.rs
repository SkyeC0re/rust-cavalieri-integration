use approx::assert_abs_diff_eq;
use cavint::{
    core::{
        differentiable::AD,
        helpers::{n_vec_from_res, vec_from_res},
    },
    pyo3_wrappers::display_cav3d,
};
use log::debug;

use crate::test_helpers::assert_float_iters_equal;

fn test_c3d(
    f_expr: &str,
    c1_expr: &str,
    c2_expr: &str,
    intervals_expr: &str,
    f: impl Fn([AD; 2]) -> AD,
    c: impl Fn(AD) -> [AD; 2],
    integ_over_triangles: Vec<([[f64; 2]; 3], f64)>,
    tol: f64,
) {
    let displays = display_cav3d(
        f_expr.to_string(),
        c1_expr.to_string(),
        c2_expr.to_string(),
        intervals_expr.to_string(),
        true,
        50,
        50,
        50,
        100,
        tol,
    )
    .expect("Expected displays");

    assert_eq!(displays.len(), integ_over_triangles.len());

    let g = |x: [AD; 2]| {
        let cfx = c(f(x));
        [x[0] - cfx[0], x[1] - cfx[1]]
    };

    displays
        .into_iter()
        .zip(integ_over_triangles.into_iter())
        .for_each(|(display, (triangle, expected_integ_val))| {
            debug!("TRIANGLE: {:?}", &triangle);

            // Assert triangular regions are equal
            display
                .triag
                .iter()
                .zip(triangle.iter())
                .for_each(|(&ta, &te)| {
                    assert_float_iters_equal(ta.into_iter(), te.into_iter(), 0f64)
                });

            assert_abs_diff_eq!(
                display.integ_value.expect("Expected integration value").0,
                expected_integ_val,
                epsilon = tol
            );

            let xvs = [
                n_vec_from_res(&triangle[1], &triangle[2], 50),
                n_vec_from_res(&triangle[2], &triangle[0], 50),
                n_vec_from_res(&triangle[0], &triangle[1], 50),
            ];

            let radrv = vec_from_res(0f64, 1f64, 50);

            let center = [
                (triangle[0][0] + triangle[1][0] + triangle[2][0]) / 3f64,
                (triangle[0][1] + triangle[1][1] + triangle[2][1]) / 3f64,
            ];

            let triag_mesh: Vec<Vec<[f64;2]>> = radrv
                .iter()
                .map(|radr| {
                    xvs[0]
                        .iter()
                        .chain(xvs[1].iter().skip(1))
                        .chain(xvs[2].iter().skip(1))
                        .map(|x| {
                            [
                                (1f64 - radr) * center[0] + radr * x[0],
                                (1f64 - radr) * center[1] + radr * x[1],
                            ]
                        })
                        .collect::<Vec<_>>()
                })
                .collect();
        });
}
