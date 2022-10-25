use std::{
    cell::RefCell,
    cmp::{min_by, Ordering},
    mem::{transmute},
    rc::Rc,
};

use crate::errors::TriangulationError;
use std::hash::Hash;

use super::helpers::Signed;

enum PType {
    Start,
    End,
    Bend,
}

impl PType {
    pub fn from_triplet(p: Pt, p1: Pt, p2: Pt) -> Option<PType> {
        if p == p1 || p == p2 {
            None
        } else if p < p1 && p < p2 {
            Some(PType::Start)
        } else if p > p1 && p > p2 {
            Some(PType::End)
        } else {
            Some(PType::Bend)
        }
    }
}

#[derive(PartialEq, PartialOrd, Clone, Copy)]
struct Pt([f64; 2]);

impl Pt {
    pub fn x(self) -> f64 {
        self.0[0]
    }
    pub fn y(self) -> f64 {
        self.0[1]
    }

    pub fn grad(self, other: Pt) -> f64 {
        let diff_x = other.x() - self.x();
        let diff_y = other.y() - self.y();
        if diff_x == 0f64 {
            diff_y.sign_val() * f64::INFINITY
        } else {
            diff_y / diff_x
        }
    }

    pub fn dummy() -> Self {
        Self([f64::NAN, f64::NAN])
    }
}

impl Hash for Pt {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe { transmute::<_, [u64; 2]>(self.0).hash(state) }
    }
}

impl Eq for Pt {}

// impl PartialOrd for Pt {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         self.x()
//             .partial_cmp(&other.x())
//             .filter(|c| c.is_eq())
//             .or(self.y().partial_cmp(&other.y()))
//     }
// }

impl Ord for Pt {
    fn cmp(&self, other: &Self) -> Ordering {
        self.x()
            .total_cmp(&other.x())
            .then(self.y().total_cmp(&other.y()))
    }
}

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
        if grad_diff.is_nan() || grad_diff == 0f64 {
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct LPt {
    pub p: Pt,
    pub prev: Option<Rc<RefCell<LPt>>>,
    pub next: Option<Rc<RefCell<LPt>>>,
}

impl LPt {
    pub fn new(p: Pt) -> Self {
        Self {
            p,
            prev: None,
            next: None,
        }
    }

    pub fn get_type(&self) -> Option<PType> {
        match (self.p, self.prev.as_ref(), self.next.as_ref()) {
            (p, Some(p1), Some(p2)) => PType::from_triplet(p, p1.borrow().p, p2.borrow().p),
            _ => None,
        }
    }
}

fn y_extrap(p1: Pt, p2: Pt, x: f64, right: bool) -> f64 {
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
        (1f64 - c) * p1.y() + c * p2.y()
    }
}

#[derive(Clone)]
struct BackChain {
    pub rm: Rc<RefCell<LPt>>,
    pub head: Rc<RefCell<LPt>>,
    pub tail: Rc<RefCell<LPt>>,
}

impl BackChain {
    pub fn new(p: Pt) -> Self {
        let p = Rc::new(RefCell::new(LPt::new(p)));
        Self {
            rm: p.clone(),
            head: p.clone(),
            tail: p,
        }
    }

    pub fn split(mut self, p: Pt) -> (Self, Self) {
        // New bottom chain with new rightmost point `p`.
        let mut b_chain = Self::new(p);
        // Attach old head to new head `p`.
        b_chain.head = self.head;
        b_chain.tail.borrow_mut().prev = Some(self.rm.clone());
        let old_rm_next = self.rm.borrow().next.clone();
        let old_rm_pt = self.rm.borrow().p;
        self.rm.borrow_mut().next = Some(b_chain.tail.clone());

        // Create duplicate of old rightmost point and link
        let mut old_rm_detached = Rc::new(RefCell::new(LPt::new(old_rm_pt)));
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
            self.tail
        };

        // Attach old tail to new tail `p`.
        old_rm_detached.borrow_mut().prev = Some(t_chain.head.clone());
        t_chain.head.borrow_mut().next = Some(old_rm_detached);

        (b_chain, t_chain)
    }
}

// struct YEdge {
//     pub rp: LPt,
//     pub
// }
