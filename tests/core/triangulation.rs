use cavint::core::triangulation::{triangulate_polygon_set, Pt, Triag, PType};

fn test_triangulation(p_set: Vec<Vec<[f64; 2]>>, mut expected: Vec<Triag>) {
    let mut actual = triangulate_polygon_set(&p_set).unwrap();
    actual.sort();
    expected.sort();
    assert_eq!(actual, expected)
}

#[test]
fn point_comparison() {
    let a = Pt::new(0f64, 0f64);
    let b = Pt::new(0f64, 1f64);
    let c = Pt::new(1f64, 0f64);

    assert!(a < b);
    assert!(b < c);
    assert!(a < c);
}

#[test]
fn point_type() {
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
    let triag = vec![[0f64, 0f64], [0f64, 1f64], [1f64, 0f64]];

    test_triangulation(
        vec![triag.clone()],
        vec![Triag::new([0f64, 0f64], [0f64, 1f64], [1f64, 0f64])],
    )
}
