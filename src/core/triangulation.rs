use std::{
    cell::RefCell,
    cmp::{min, min_by, Ordering},
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::Display,
    mem::{self, transmute},
    ops::Bound,
    rc::Rc,
};

use ordered_float::OrderedFloat as OFlt;

use crate::errors::TriangulationError;
use std::hash::Hash;

use super::helpers::Signed;

type Of64 = OFlt<f64>;

/* Point and point type implementations */

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum PType {
    Start,
    End,
    Bend,
}
impl PType {
    pub fn from_triplet(p: Pt, p1: Pt, p2: Pt) -> Result<PType, TriangulationError> {
        if p == p1 || p == p2 {
            Err(TriangulationError::NoPointType(p))
        } else if p < p1 && p < p2 {
            Ok(PType::Start)
        } else if p > p1 && p > p2 {
            Ok(PType::End)
        } else {
            Ok(PType::Bend)
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Clone, Copy, Debug)]
pub struct Pt([OFlt<f64>; 2]);

impl Pt {
    pub fn new(x: impl Into<Of64>, y: impl Into<Of64>) -> Self {
        Self([x.into(), y.into()])
    }

    pub fn x(self) -> OFlt<f64> {
        self.0[0]
    }
    pub fn y(self) -> OFlt<f64> {
        self.0[1]
    }

    pub fn grad(self, other: Pt) -> OFlt<f64> {
        let diff_x = other.x() - self.x();
        let diff_y = other.y() - self.y();
        if diff_x == OFlt(0f64) {
            OFlt(diff_y.sign_val()) * OFlt(f64::INFINITY)
        } else {
            diff_y / diff_x
        }
    }
}

impl Display for Pt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x(), self.y())
    }
}

impl From<[f64; 2]> for Pt {
    fn from(xy: [f64; 2]) -> Self {
        Self([OFlt(xy[0]), OFlt(xy[1])])
    }
}

fn y_extrap(p1: Pt, p2: Pt, x: OFlt<f64>, right: bool) -> OFlt<f64> {
    let (p1, p2) = if p1 > p2 { (p2, p1) } else { (p1, p2) };

    if x == p1.x() && x == p2.x() {
        return if right { p2.y() } else { p1.y() };
    }

    if x <= p1.x() {
        p1.y()
    } else if x >= p2.x() {
        p2.y()
    } else {
        let c = (x - p1.x()) / (p2.x() - p1.x());
        (OFlt(1f64) - c) * p1.y() + c * p2.y()
    }
}

/* Triangle implementation */

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Triag([Pt; 3]);

impl Triag {
    pub fn new(v1: impl Into<Pt>, v2: impl Into<Pt>, v3: impl Into<Pt>) -> Self {
        let mut pts = [v1.into(), v2.into(), v3.into()];
        pts.sort();
        Self(pts)
    }

    pub fn from_pt_arr(mut pts: [Pt; 3]) -> Self {
        pts.sort();
        Self(pts)
    }
}

impl<P: Into<Pt>> From<[P; 3]> for Triag {
    fn from(t: [P; 3]) -> Self {
        let t = t.map(|v| v.into());
        Self::from_pt_arr(t)
    }
}

#[derive(Clone, PartialEq, Eq)]
enum PSign {
    C,
    CC,
    None,
}

fn clockwise_sign(polygon: &[Pt]) -> PSign {
    if let Some((i_min, _)) = polygon
        .iter()
        .enumerate()
        .reduce(|a, b| min_by(a, b, |a, b| a.1.cmp(b.1)))
    {
        let p = polygon[i_min];
        let p_prev = polygon[(i_min + polygon.len() - 1) % polygon.len()];
        let p_next = polygon[(i_min + 1) % polygon.len()];

        let grad_diff = p.grad(p_next) - p.grad(p_prev);
        if grad_diff.is_nan() || grad_diff == OFlt(0f64) {
            PSign::None
        } else if grad_diff.is_sign_positive() {
            PSign::C
        } else {
            PSign::CC
        }
    } else {
        PSign::None
    }
}

/* Linked point implementation */

#[derive(Clone)]
struct LPt {
    pub p: Pt,
    pub prev: Option<Rc<RefCell<LPt>>>,
    pub next: Option<Rc<RefCell<LPt>>>,
}

impl LPt {
    pub fn new(p: Pt) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            p,
            prev: None,
            next: None,
        }))
    }

    pub fn get_type(&self) -> Result<PType, TriangulationError> {
        match (self.p, self.prev.as_ref(), self.next.as_ref()) {
            (p, Some(p1), Some(p2)) => PType::from_triplet(p, p1.borrow().p, p2.borrow().p),
            _ => Err(TriangulationError::NoPointType(self.p)),
        }
    }
}

impl PartialEq for LPt {
    fn eq(&self, other: &Self) -> bool {
        self.p == other.p
    }
}

impl Eq for LPt {}

impl PartialOrd for LPt {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.p.partial_cmp(&other.p)
    }
}

impl Ord for LPt {
    fn cmp(&self, other: &Self) -> Ordering {
        self.p.cmp(&other.p)
    }
}

impl Display for LPt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.p.fmt(f)
    }
}

/* Backchain implementation */

#[derive(Clone, PartialEq, Eq)]
struct BackChain {
    pub rm: Rc<RefCell<LPt>>,
    pub head: Rc<RefCell<LPt>>,
    pub tail: Rc<RefCell<LPt>>,
}

impl BackChain {
    pub fn new(p: Pt) -> Self {
        let p = LPt::new(p);
        Self {
            rm: p.clone(),
            head: p.clone(),
            tail: p,
        }
    }

    pub fn split(&mut self, p: Pt) -> (Self, Self) {
        // New bottom chain with new rightmost point `p`.
        let mut b_chain = Self::new(p);
        // Attach old head to new head `p`.
        b_chain.head = self.head.clone();
        b_chain.tail.borrow_mut().prev = Some(self.rm.clone());
        let old_rm_next = self.rm.borrow().next.clone();
        let old_rm_pt = self.rm.borrow().p;
        self.rm.borrow_mut().next = Some(b_chain.tail.clone());

        // Create duplicate of old rightmost point and link
        let old_rm_detached = LPt::new(old_rm_pt);
        old_rm_detached.borrow_mut().next = old_rm_next;
        if let Some(rm_next) = &mut old_rm_detached.borrow_mut().next {
            rm_next.borrow_mut().prev = Some(old_rm_detached.clone())
        }
        // New top chain with new rightmost point `p`.
        let mut t_chain = Self::new(p);
        // If old rm was the tail, reattach detached old rightmost point.
        t_chain.tail = if Rc::ptr_eq(&self.tail, &self.rm) {
            old_rm_detached.clone()
        } else {
            self.tail.clone()
        };

        // Attach old tail to new tail `p`.
        old_rm_detached.borrow_mut().prev = Some(t_chain.head.clone());
        t_chain.head.borrow_mut().next = Some(old_rm_detached);

        (b_chain, t_chain)
    }

    pub fn merge(b_chain: &mut Self, t_chain: &mut Self, p: Pt) -> Self {
        let mut merged = Self::new(p);

        // Attach bottom chain to new rightmost point `p`
        b_chain.tail.borrow_mut().next = Some(merged.head.clone());
        merged.head.borrow_mut().prev = Some(b_chain.tail.clone());
        merged.head = b_chain.head.clone();

        // Attach top chain to new rightmost point `p`
        t_chain.head.borrow_mut().prev = Some(merged.tail.clone());
        merged.tail.borrow_mut().next = Some(t_chain.head.clone());
        merged.tail = t_chain.tail.clone();

        merged
    }

    pub fn append(&mut self, p: Pt, to_tail: bool) {
        let new_rm = LPt::new(p);
        self.rm = new_rm.clone();
        if to_tail {
            new_rm.borrow_mut().prev = Some(self.tail.clone());
            self.tail.borrow_mut().next = Some(new_rm.clone());
            self.tail = new_rm
        } else {
            new_rm.borrow_mut().next = Some(self.head.clone());
            self.head.borrow_mut().prev = Some(new_rm.clone());
            self.head = new_rm
        }
    }

    fn node_triangulate(from_node: &Rc<RefCell<LPt>>, backward: bool, triag_list: &mut Vec<Triag>) {
        loop {
            let mut triplet = if backward {
                let n3 = from_node;
                if let Some(n2) = &n3.borrow().prev {
                    if let Some(n1) = &n2.borrow().prev {
                        [n1.clone(), n2.clone(), n3.clone()]
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                let n1 = from_node;
                if let Some(n2) = &n1.borrow().next {
                    if let Some(n3) = &n2.borrow().next {
                        [n1.clone(), n2.clone(), n3.clone()]
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            };
            let pts = [
                triplet[0].borrow().p,
                triplet[1].borrow().p,
                triplet[2].borrow().p,
            ];
            if clockwise_sign(&pts) == PSign::C {
                triplet[0].borrow_mut().next = Some(triplet[2].clone());
                triplet[2].borrow_mut().prev = Some(triplet[0].clone());
                triag_list.push(Triag::from_pt_arr(pts));
            } else {
                break;
            }
        }
    }

    pub fn back_triangulate(&mut self, from_tail: bool, triag_list: &mut Vec<Triag>) {
        let node = match from_tail {
            true => &mut self.tail,
            false => &mut self.head,
        };

        Self::node_triangulate(node, from_tail, triag_list);
    }

    pub fn rm_split_triangulate(&mut self, triag_list: &mut Vec<Triag>) {
        Self::node_triangulate(&self.rm, true, triag_list);
        Self::node_triangulate(&self.rm, false, triag_list);
    }
}

/* Y-structure implementation */

// enum RelevantEdges {
//     None,
//     One(Rc<RefCell<YEdge>>),
//     Two(Rc<RefCell<YEdge>>, Rc<RefCell<YEdge>>),
// }
struct YStruct {
    x: Rc<RefCell<Of64>>,
    active_edges: BTreeSet<Rc<RefCell<YEdge>>>,
    ordered_points: BTreeMap<Rc<RefCell<LPt>>, Vec<Rc<RefCell<YEdge>>>>,
}

/* Active Y-structure edge implementation */

#[derive(PartialEq, Eq)]
struct YEdge {
    pub rpt: Pt,
    pub backchain: Rc<RefCell<BackChain>>,
    pub shared_x: Rc<RefCell<Of64>>,
    pub bof_in_interval: bool,
    pub b_partner: Option<Rc<RefCell<Self>>>,
    pub t_partner: Option<Rc<RefCell<Self>>>,
}

impl YEdge {
    pub fn new(
        rpt: Pt,
        backchain: Rc<RefCell<BackChain>>,
        shared_x: Rc<RefCell<Of64>>,
        bof_in_interval: bool,
    ) -> Self {
        Self {
            rpt,
            backchain,
            bof_in_interval,
            shared_x,
            b_partner: None,
            t_partner: None,
        }
    }

    pub fn y_at(&self, x: OFlt<f64>, right: bool) -> OFlt<f64> {
        let lpt = if self.bof_in_interval {
            self.backchain.borrow().head.borrow().p
        } else {
            self.backchain.borrow().tail.borrow().p
        };

        y_extrap(lpt, self.rpt, x, right)
    }

    pub fn grad(&self) -> OFlt<f64> {
        let lpt = if self.bof_in_interval {
            self.backchain.borrow().head.borrow().p
        } else {
            self.backchain.borrow().tail.borrow().p
        };

        lpt.grad(self.rpt)
    }

    pub fn cmp_at(&self, other: &Self, x: OFlt<f64>, right: bool) -> Ordering {
        if !x.is_finite() {
            return Ordering::Equal;
        }
        self.y_at(x, right)
            .total_cmp(&other.y_at(x, right))
            .then(self.grad().total_cmp(&other.grad()))
    }

    pub fn will_overlap_bot(&self) -> bool {
        if let Some(bp) = &self.b_partner {
            let bp = bp.borrow();
            let x1 = self.rpt.x();
            let x2 = bp.rpt.x();
            if x1 == x2 {
                self.y_at(x1, true) < bp.y_at(x1, true)
            } else {
                self.cmp_at(&bp, min_by(x1, x2, |x1, x2| x1.total_cmp(x2)), true)
                    != Ordering::Greater
            }
        } else {
            false
        }
    }

    pub fn will_overlap_top(&self) -> bool {
        if let Some(tp) = &self.t_partner {
            let tp = tp.borrow();
            let x1 = self.rpt.x();
            let x2 = tp.rpt.x();
            if x1 == x2 {
                self.y_at(x1, true) > tp.y_at(x1, true)
            } else {
                self.cmp_at(&tp, min_by(x1, x2, |x1, x2| x1.total_cmp(x2)), true) != Ordering::Less
            }
        } else {
            false
        }
    }
}

impl PartialOrd for YEdge {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let x = self.shared_x.borrow().clone();
        if !x.is_finite() {
            return Some(Ordering::Equal);
        }
        match self.y_at(x, true).partial_cmp(&other.y_at(x, true)) {
            Some(Ordering::Equal) => self.grad().partial_cmp(&other.grad()),
            None => None,
            order => order,
        }
    }
}

impl Ord for YEdge {
    fn cmp(&self, other: &Self) -> Ordering {
        let x = self.shared_x.borrow().clone();
        if !x.is_finite() {
            return Ordering::Equal;
        }
        self.y_at(x, true)
            .total_cmp(&other.y_at(x, true))
            .then(self.grad().total_cmp(&other.grad()))
    }
}

impl Display for YEdge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bc = self.backchain.borrow();
        let lpt = match self.bof_in_interval {
            true => bc.head.borrow().p,
            false => bc.tail.borrow().p,
        };
        write!(f, "{}--{}", lpt, self.rpt,)
    }
}

// fn opt_rc_cell_is_eq<T: Eq>(a: Option<Rc<RefCell<T>>>, b: Option<Rc<RefCell<T>>>, default: bool) -> bool {

// }

fn handle_next(
    y_struct: &mut YStruct,
    triag_list: &mut Vec<Triag>,
) -> Result<(), TriangulationError> {
    println!("START HANDLE NEXT");
    let (lp, ptype, r_edges) = match y_struct.ordered_points.iter().next() {
        Some((lp, r_edges)) => (lp.clone(), lp.borrow().get_type()?, r_edges.clone()),
        _ => unreachable!("DDDDD"),
    };
    println!("START HANDLE NEXT2");
    y_struct.ordered_points.remove(lp.as_ref());
    println!("START HANDLE NEXT3");
    let p = lp.borrow().p;
    println!("Handling point {p}");
    let lp1 = match &lp.borrow().prev {
        Some(lp1) => lp1.clone(),
        _ => unreachable!(),
    };
    let lp2 = match &lp.borrow().next {
        Some(lp2) => lp2.clone(),
        _ => unreachable!(),
    };

    match ptype {
        PType::Start => {
            println!("Handling Start Point {p}");
            *y_struct.x.borrow_mut() = p.x();
            let backchain = Rc::new(RefCell::new(BackChain::new(p)));
            // Create new bottom and top
            let bot = YEdge::new(lp1.borrow().p, backchain.clone(), y_struct.x.clone(), false);
            let top = YEdge::new(lp2.borrow().p, backchain, y_struct.x.clone(), false);

            // Correct ordering
            let (lp_bot, bot, lp_top, top) = match bot.cmp(&top) {
                Ordering::Less => (lp1, bot, lp2, top),
                Ordering::Greater => (lp2, top, lp1, bot),
                Ordering::Equal => return Err(TriangulationError::Overlap(ptype, p)),
            };

            let bot = Rc::new(RefCell::new(bot));
            let top = Rc::new(RefCell::new(top));

            // Add bottom as relevant edge to its right point.
            y_struct
                .ordered_points
                .entry(lp_bot)
                .and_modify(|e| e.push(bot.clone()))
                .or_insert(vec![bot.clone()]);

            // Add top as relevant edge to its right point.
            y_struct
                .ordered_points
                .entry(lp_top)
                .and_modify(|e| e.push(top.clone()))
                .or_insert(vec![top.clone()]);

            // Get nesting partners.
            let bot_bot = y_struct
                .active_edges
                .range::<Rc<RefCell<YEdge>>, _>((Bound::Unbounded, Bound::Excluded(&bot)))
                .next_back()
                .cloned();
            let top_top = y_struct
                .active_edges
                .range::<Rc<RefCell<YEdge>>, _>((Bound::Excluded(&top), Bound::Unbounded))
                .next()
                .cloned();

            // Ensure nesting partners have no edges between them.
            if y_struct
                .active_edges
                .range::<Rc<RefCell<YEdge>>, _>((
                    if let Some(bot_bot) = &bot_bot {
                        Bound::Excluded(bot_bot)
                    } else {
                        Bound::Unbounded
                    },
                    if let Some(top_top) = &top_top {
                        Bound::Excluded(top_top)
                    } else {
                        Bound::Unbounded
                    },
                ))
                .any(|_| true)
            {
                return Err(TriangulationError::Overlap(ptype, p));
            }

            // Link nested edges if exists and set in-interval flags appropriately
            if let Some(bot_bot) = &bot_bot {
                let mut b = bot.borrow_mut();
                let mut bb = bot_bot.borrow_mut();
                b.b_partner = Some(bot_bot.clone());
                b.bof_in_interval = !bb.bof_in_interval;
                bb.t_partner = Some(bot.clone());
                if b.will_overlap_bot() {
                    return Err(TriangulationError::Overlap(ptype, p));
                }
            }else {
                bot.borrow_mut().bof_in_interval = true;
            }
            if let Some(top_top) = &top_top {
                let mut t = top.borrow_mut();
                let mut tt = top_top.borrow_mut();
                t.b_partner = Some(top_top.clone());
                t.bof_in_interval = !tt.bof_in_interval;
                tt.t_partner = Some(top.clone());
                if t.will_overlap_top() {
                    return Err(TriangulationError::Overlap(ptype, p));
                }
            } else {
                top.borrow_mut().bof_in_interval = false;
            }

            if let (Some(bot_bot), Some(top_top)) = (bot_bot, top_top) {
                let mut bb = bot_bot.borrow_mut();
                let mut tt = top_top.borrow_mut();
                // Improper start
                if bb.bof_in_interval {
                    let (mut bc_bot, mut bc_top) = bb.backchain.borrow_mut().split(p);

                    // Create convex in-angle triangulations
                    bc_bot.back_triangulate(true, triag_list);
                    bc_top.back_triangulate(false, triag_list);

                    // Attach new bottom backchain
                    let bc_bot = Rc::new(RefCell::new(bc_bot));
                    bb.backchain = bc_bot.clone();
                    bot.borrow_mut().backchain = bc_bot;

                    // Attach new top backcahin
                    let bc_top = Rc::new(RefCell::new(bc_top));
                    tt.backchain = bc_top.clone();
                    top.borrow_mut().backchain = bc_top;
                }
            }

            // Finally insert newly created edges into Y-structure.
            y_struct.active_edges.insert(bot);
            y_struct.active_edges.insert(top);
        }
        PType::Bend => {
            println!("Handling Bend Point {p}");
            *y_struct.x.borrow_mut() = p.x();
            let edge = &r_edges[0];
            let rlp = if lp1.borrow().p >= lp2.borrow().p {
                lp1
            } else {
                lp2
            };
            let mut eb = edge.borrow_mut();
            let from_tail = !eb.bof_in_interval;
            eb.backchain.borrow_mut().append(p, from_tail);
            eb.backchain
                .borrow_mut()
                .back_triangulate(from_tail, triag_list);
            eb.rpt = rlp.borrow().p;

            if eb.will_overlap_bot() || eb.will_overlap_top() {
                return Err(TriangulationError::Overlap(ptype, p));
            }

            // Add as relevant edge to its right point.
            y_struct
                .ordered_points
                .entry(rlp)
                .and_modify(|e| e.push(edge.clone()))
                .or_insert(vec![edge.clone()]);
        }
        PType::End => {
            println!("Handling End Point {p}");
            let bot = &r_edges[0];
            let top = &r_edges[1];

            let (bot, top) = if bot.borrow().le(&top.borrow()) {
                (bot, top)
            } else {
                (top, bot)
            };

            // Remove edges from Y-structure before advancing x, so as not to invalidate binary tree node ordering.
            y_struct.active_edges.remove(bot);
            y_struct.active_edges.remove(top);

            *y_struct.x.borrow_mut() = p.x();

            let b = bot.borrow();
            let t = top.borrow();
            // Proper end
            if b.bof_in_interval {
                let mut bc = t.backchain.borrow_mut();
                bc.append(p, true);
                bc.back_triangulate(true, triag_list);
            // Improper end
            } else {
                let mut bc = BackChain::merge(
                    &mut b.backchain.borrow_mut(),
                    &mut t.backchain.borrow_mut(),
                    p,
                );
                bc.rm_split_triangulate(triag_list);
                let bc = Rc::new(RefCell::new(bc));
                match (&b.b_partner, &t.t_partner) {
                    (Some(bb), Some(tt)) => {
                        bb.borrow_mut().backchain = bc.clone();
                        tt.borrow_mut().backchain = bc;
                    }
                    _ => unreachable!(),
                }
            }

            if let Some(bb) = &b.b_partner {
                bb.borrow_mut().t_partner = t.t_partner.clone();
            }
            if let Some(tt) = &t.t_partner {
                tt.borrow_mut().b_partner = b.b_partner.clone();
            }
        }
    };
    Ok(())
}

pub fn triangulate_polygon_set(
    poly_set: &Vec<Vec<impl Into<Pt> + Clone>>,
) -> Result<Vec<Triag>, TriangulationError> {
    println!("Start ALG");
    let mut discovered_points: HashSet<Pt> = HashSet::new();

    let mut valid_pt = |pt: Pt| {
        if pt.x().is_finite() && pt.y().is_finite() {
            match discovered_points.insert(pt) {
                true => Ok(()),
                false => Err(TriangulationError::DuplicatePoint(pt)),
            }
        } else {
            Err(TriangulationError::NonFiniteInputError)
        }
    };
    let mut y_struct = YStruct {
        x: Rc::new(RefCell::new(OFlt(f64::NEG_INFINITY))),
        active_edges: BTreeSet::new(),
        ordered_points: BTreeMap::new(),
    };
    for polygon in poly_set {
        if polygon.len() < 3 {
            return Err(TriangulationError::NoPolygon);
        }
        let pt = polygon[0].clone().into();
        valid_pt(pt)?;
        let first = LPt::new(pt);
        match PType::from_triplet(
            pt,
            polygon[1].clone().into(),
            polygon[polygon.len() - 1].clone().into(),
        )? {
            PType::Start => {
                println!("Start point {pt}");
                y_struct.ordered_points.insert(first.clone(), vec![]);
            }
            _ => {}
        }

        let mut curr = first.clone();
        for i in 1..polygon.len() {
            let pt = polygon[i].clone().into();
            valid_pt(pt)?;
            let new_lp = LPt::new(pt);
            curr.borrow_mut().next = Some(new_lp.clone());
            new_lp.borrow_mut().prev = Some(curr);
            println!("Point {pt}");
            match PType::from_triplet(
                pt,
                polygon[(i + polygon.len() - 1) % polygon.len()]
                    .clone()
                    .into(),
                polygon[(i + 1) % polygon.len()].clone().into(),
            )? {
                PType::Start => {
                    println!("Start point {pt}");
                    y_struct.ordered_points.insert(new_lp.clone(), vec![]);
                }
                _ => {}
            }
            curr = new_lp;
        }
        curr.borrow_mut().next = Some(first.clone());
        first.borrow_mut().prev = Some(curr);
    }
    let mut triag_list = vec![];
    loop {
        if y_struct.ordered_points.is_empty() {
            break;
        } else {
            handle_next(&mut y_struct, &mut triag_list)?
        }
    }
    Ok(triag_list)
}
