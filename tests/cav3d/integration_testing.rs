use approx::assert_abs_diff_eq;
use cavint::{
    core::helpers::{n_vec_from_res, vec_from_res},
    pyo3_wrappers::display_cav3d,
};
use log::debug;

use crate::test_helpers::{assert_float_iters_equal, assert_grids_equal};

fn test_c3d(
    f_expr: &str,
    c1_expr: &str,
    c2_expr: &str,
    polygon_set_expr: &str,
    f: impl Fn([f64; 2]) -> f64,
    c: impl Fn(f64) -> [f64; 2],
    integ_over_triangles: Vec<([[f64; 2]; 3], f64)>,
    tol: f64,
) {
    let displays = display_cav3d(
        f_expr.to_string(),
        c1_expr.to_string(),
        c2_expr.to_string(),
        polygon_set_expr.to_string(),
        true,
        50,
        50,
        50,
        100,
        tol,
    )
    .expect("Expected displays");

    assert_eq!(displays.len(), integ_over_triangles.len());

    let g = |x: [f64; 2]| {
        let cfx = c(f(x));
        [x[0] - cfx[0], x[1] - cfx[1]]
    };

    displays
        .into_iter()
        .zip(integ_over_triangles.into_iter())
        .for_each(|(display, (triangle, expected_integ_val))| {
            debug!("TRIANGLE: {:?}", &triangle);

            debug!("triangular region similarity assertions");
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

            let ratiov = vec_from_res(0f64, 1f64, 50);

            let center = [
                (triangle[0][0] + triangle[1][0] + triangle[2][0]) / 3f64,
                (triangle[0][1] + triangle[1][1] + triangle[2][1]) / 3f64,
            ];

            let triag_mesh: Vec<Vec<[f64; 2]>> = ratiov
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

            debug!("top mesh assertions");
            let top_mesh: Vec<Vec<[f64; 3]>> = triag_mesh
                .iter()
                .map(|row| {
                    row.iter()
                        .map(|&pt| [pt[0], pt[1], f(pt)])
                        .collect::<Vec<_>>()
                })
                .collect();
            assert_eq!(display.top_mesh.len(), top_mesh.len());
            assert_grids_equal(&display.top_mesh, &top_mesh, 0f64);

            debug!("bottom mesh assertions");
            let bot_mesh: Vec<Vec<[f64; 2]>> = triag_mesh
                .iter()
                .map(|row| row.iter().map(|&pt| g(pt)).collect::<Vec<_>>())
                .collect();
            assert_eq!(display.bot_mesh.len(), bot_mesh.len());
            assert_grids_equal(&display.bot_mesh, &bot_mesh, 0f64);

            debug!("curtain assertions");
            assert_eq!(display.curtains.len(), 3);
            for (curtain, xv) in display.curtains.iter().zip(xvs.iter()) {
                let xrv: Vec<[f64; 2]> = xv.iter().map(|&x| g(x)).collect();
                let fv: Vec<f64> = xv.iter().map(|&x| f(x)).collect();
                let true_curtain: Vec<Vec<[f64; 3]>> = ratiov
                    .iter()
                    .copied()
                    .map(|yr| {
                        let mut row = Vec::with_capacity(xv.len());
                        for i in 0..xv.len() {
                            let y = yr * fv[i];
                            let mut x = c(y);
                            x[0] += xrv[i][0];
                            x[1] += xrv[i][1];
                            row.push([x[0], x[1], y]);
                        }
                        row
                    })
                    .collect();

                assert_grids_equal(&curtain, &true_curtain, 0f64);
            }
        });
}

#[test]
fn constant_triangle() {
    test_c3d(
        "2",
        "0",
        "0",
        "[[0, 0], [0, 1], [1, 1]]",
        |_| 2f64,
        |_| [0f64, 0f64],
        vec![([[0f64, 0f64], [0f64, 1f64], [1f64, 1f64]], 1f64)],
        1e-9,
    );
}
