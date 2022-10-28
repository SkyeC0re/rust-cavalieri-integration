use cavint::{
    core::triangulation::{triangulate_polygon_set, PType, Pt, Triag},
    errors::TriangulationError,
};

use crate::setup;

fn test_triangulation(p_set: Vec<Vec<impl Into<Pt> + Clone>>, mut expected: Vec<Triag>) {
    let mut actual = triangulate_polygon_set(p_set).unwrap();
    actual.sort();
    expected.sort();
    assert_eq!(actual, expected)
}

#[test]
fn point_comparison() {
    setup();
    let a = Pt::new(0f64, 0f64);
    let b = Pt::new(0f64, 1f64);
    let c = Pt::new(1f64, 0f64);

    assert!(a < b);
    assert!(b < c);
    assert!(a < c);
}

#[test]
fn point_type() {
    setup();
    let a = Pt::new(0f64, 0f64);
    let b = Pt::new(0f64, 1f64);
    let c = Pt::new(1f64, 0f64);

    assert!(PType::from_triplet(a, a, b).is_err());
    assert_eq!(PType::from_triplet(a, c, b).unwrap(), PType::Start);
    assert_eq!(PType::from_triplet(b, a, c).unwrap(), PType::Bend);
    assert_eq!(PType::from_triplet(c, a, b).unwrap(), PType::End);
}

#[test]
fn triangle() {
    setup();
    let a = Pt::new(0f64, 0f64);
    let b = Pt::new(0f64, 1f64);
    let c = Pt::new(0.5f64, 1f64);

    test_triangulation(vec![vec![a, b, c]], vec![Triag::new(a, b, c)])
}

#[test]
fn square() {
    setup();
    let a = Pt::new(0f64, 0f64);
    let b = Pt::new(0f64, 1f64);
    let c = Pt::new(1f64, 1f64);
    let d = Pt::new(1f64, 0f64);

    test_triangulation(
        vec![vec![a, b, c, d]],
        vec![Triag::new(a, b, d), Triag::new(b, c, d)],
    )
}

/* Positive Polygon triangulation tests */

fn p1() -> Vec<Pt> {
    vec![
        Pt::new(2f64, 1.5),
        Pt::new(8f64, 3f64),
        Pt::new(4f64, 5f64),
        Pt::new(3f64, 4f64),
        Pt::new(5f64, 3f64),
    ]
}

fn p2() -> Vec<Pt> {
    vec![
        Pt::new(0f64, 0f64),
        Pt::new(2f64, 3f64),
        Pt::new(1f64, 4f64),
        Pt::new(1f64, 6f64),
        Pt::new(2f64, 4f64),
        Pt::new(2f64, 7f64),
        Pt::new(9f64, 3f64),
        Pt::new(7f64, 0f64),
    ]
}

#[test]
fn p1_triangulation() {
    setup();
    let p = p1();
    test_triangulation(
        vec![p.clone()],
        vec![
            Triag::new(p[4], p[3], p[2]),
            Triag::new(p[1], p[2], p[4]),
            Triag::new(p[1], p[4], p[0]),
        ],
    );
}

#[test]
fn p2_triangulation() {
    setup();
    let p = p2();
    test_triangulation(
        vec![p.clone()],
        vec![
            Triag::new(p[1], p[2], p[3]),
            Triag::new(p[1], p[3], p[4]),
            Triag::new(p[7], p[0], p[1]),
            Triag::new(p[7], p[1], p[4]),
            Triag::new(p[7], p[4], p[5]),
            Triag::new(p[6], p[5], p[7]),
        ],
    );
}

#[test]
fn p1_in_p2_triangulation() {
    setup();
    // Inner polygon
    let i = p1();
    // Outer polygon
    let o = p2();

    test_triangulation(
        vec![o.clone(), i.clone()],
        vec![
            Triag::new(o[1], i[0], o[0]),
            Triag::new(o[1], o[2], o[3]),
            Triag::new(o[1], o[3], o[4]),
            Triag::new(i[3], o[5], o[4]),
            Triag::new(i[3], o[4], o[1]),
            Triag::new(i[3], o[1], i[0]),
            Triag::new(i[2], i[3], o[5]),
            Triag::new(i[4], i[3], i[0]),
            Triag::new(o[7], o[0], i[0]),
            Triag::new(i[1], i[0], o[7]),
            Triag::new(i[1], i[2], o[5]),
            Triag::new(o[6], o[5], i[1]),
            Triag::new(o[6], i[1], o[7]),
        ],
    );
}

/* Negative polygon triangulation tests */

#[test]
fn nan_triangulation() {
    setup();
    let res = triangulate_polygon_set(vec![vec![
        Pt::new(2f64, 1.5),
        Pt::new(8f64, 3f64),
        Pt::new(4f64, 5f64),
        Pt::new(3f64, 4f64),
        Pt::new(5f64, f64::NAN),
    ]]);

    assert_eq!(res, Err(TriangulationError::NonFiniteInputError));
}

#[test]
fn infinity_triangulation() {
    setup();
    let res = triangulate_polygon_set(vec![vec![
        Pt::new(2f64, 1.5),
        Pt::new(8f64, 3f64),
        Pt::new(f64::INFINITY, 5f64),
        Pt::new(3f64, 4f64),
        Pt::new(5f64, 3f64),
    ]]);

    assert_eq!(res, Err(TriangulationError::NonFiniteInputError));
}

#[test]
fn duplicate_point_triangulation() {
    setup();
    let res = triangulate_polygon_set(vec![
        vec![Pt::new(2f64, 1.5), Pt::new(4f64, 0f64), Pt::new(2f64, 3f64)],
        vec![
            Pt::new(0f64, 0f64),
            Pt::new(0f64, 1f64),
            Pt::new(2f64, 3f64),
        ],
    ]);

    assert_eq!(
        res,
        Err(TriangulationError::DuplicatePoint(Pt::new(2f64, 3f64)))
    );
}

#[test]
fn degeneracy1() {
    setup();
    let res = triangulate_polygon_set(vec![vec![
        Pt::new(0f64, 0f64),
        Pt::new(1f64, 0f64),
        Pt::new(2f64, 0f64),
    ]]);

    assert_eq!(
        res,
        Err(TriangulationError::Overlap(
            PType::Start,
            Pt::new(0f64, 0f64)
        ))
    );
}

#[test]
fn degeneracy2() {
    setup();
    let res = triangulate_polygon_set(vec![vec![
        Pt::new(0f64, 0f64),
        Pt::new(1f64, 0f64),
        Pt::new(0f64, 1f64),
        Pt::new(1f64, 1f64),
    ]]);

    assert_eq!(
        res,
        Err(TriangulationError::Overlap(
            PType::Start,
            Pt::new(0f64, 1f64)
        ))
    );
}