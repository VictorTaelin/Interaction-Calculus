use std::collections::HashMap;

pub use crate::inet::*;

use std::collections::BTreeMap;
use std::collections::VecDeque;

// It is that simple
pub fn check(inet: &mut INet, prev: Port) -> bool {
  reduce(inet, prev, &|a,b| false);
  let next = enter(inet, prev);
  if slot(next) == 0 {
    if kind(inet, addr(next)) == ANN && !equal(inet, prev, next) {
      return false;
    }
    let e1 = check(inet, port(addr(next), 1));
    let e2 = check(inet, port(addr(next), 2));
    return e1 && e2;
  }
  return true;
}

pub struct Cursor<'a> {
  root: Port,
  prev: Port,
  path: &'a mut BTreeMap<u32, VecDeque<u8>>,
  logs: &'a mut Vec<String>,
}

impl<'a> Cursor<'a> {
  fn next(&mut self, inet: &mut INet, slot: u8) -> Cursor {
    Cursor {
      root: self.root,
      prev: port(addr(enter(inet, self.prev)), slot as u32),
      path: self.path,
      logs: self.logs,
    }
  }

  fn push_back(&mut self, kind: u32, slot: u8) {
    self.path.entry(kind).or_default().push_back(slot);
  }

  fn push_front(&mut self, kind: u32, slot: u8) {
    self.path.entry(kind).or_default().push_front(slot);
  }

  fn pop_back(&mut self, kind: u32) -> Option<u8> {
    let opt = self.path.get_mut(&kind).and_then(|vec| vec.pop_back());
    self.cleanup(kind);
    opt
  }

  fn pop_front(&mut self, kind: u32) -> Option<u8> {
    let opt = self.path.get_mut(&kind).and_then(|vec| vec.pop_front());
    self.cleanup(kind);
    opt
  }

  fn cleanup(&mut self, kind: u32) {
    if self.path.get(&kind).map_or(false, |vec| vec.is_empty()) {
      self.path.remove(&kind);
    }
  }
}

// Checks if two interaction nets ports are equal.
pub fn equal(inet: &mut INet, a: Port, b: Port) -> bool {
  let mut a_path = BTreeMap::new();
  let mut b_path = BTreeMap::new();
  let mut a_logs = vec![];
  let mut b_logs = vec![];
  let mut a_cursor = Cursor { root: a, prev: a, path: &mut a_path, logs: &mut a_logs, };
  let mut b_cursor = Cursor { root: b, prev: b, path: &mut b_path, logs: &mut b_logs, };
  compare(inet, &mut a_cursor, &mut b_cursor, true)
}

// Compares two cursors by moving them forward until root is reached
pub fn compare(inet: &mut INet, a: &mut Cursor, b: &mut Cursor, flip: bool) -> bool {
  //println!("Equal: (Node: {}, Slot: {}) ~ (Node {}, Slot {})\n  Paths: {:?} | {:?}\n  Logs : {:?} | {:?}", addr(a.prev), slot(a.prev), addr(b.prev), slot(b.prev), a.path, b.path, a.logs, b.logs);
  
  let a_next = enter(inet, a.prev);
  let a_slot = slot(a_next);
  let a_kind = kind(inet, addr(a_next));

  // If on root, stop
  if a_next == a.root || a_next == ROOT || a_kind == ERA {
    if flip {
      return compare(inet, b, a, false);
    } else {
      return a.path.get(&ANN) != b.path.get(&ANN) || a.path.get(&CON) == b.path.get(&CON);
    }

  // If entering main port...
  } else if a_slot == 0 {

    // If deque isn't empty, pop_back a slot and move to it
    if let Some(slot) = a.pop_back(a_kind) {
      a.logs.push(format!("V{}", slot));
      let an = &mut a.next(inet, slot);
      let eq = compare(inet, an, b, flip);
      a.logs.pop();
      a.push_back(a_kind, slot);
      return eq;

    // If deque is empty, move to slots [1,2] and push_front to the *other* deque
    } else {
      //println!("enter main (split)");
      for slot in [2,1] {
        a.logs.push(format!("W{}", slot));
        b.push_front(a_kind, slot);
        let an = &mut a.next(inet, slot);
        let eq = compare(inet, an, b, flip);
        a.logs.pop();
        b.pop_front(a_kind);
        if !eq {
          return false;
        }
      }
      return true;
    }

  // If entering an aux port, push_back that slot to the deque, and move to the main port
  } else {
    a.logs.push(format!("^{}", a_slot));
    a.push_back(a_kind, slot(enter(inet, a.prev)) as u8);
    let an = &mut a.next(inet, 0);
    let eq = compare(inet, an, b, flip);
    a.pop_back(a_kind);
    a.logs.pop();
    return eq;
  }
}

// FIX-POINT COMPARISON (removed for now)
// --------------------------------------

//// If both are fixed-point, check `@x (f (g x)) == @x (g (f x))`
////if false { // TODO: re-add once the TODO's are done
  ////let a_next = enter(inet, a.prev);
  ////let b_next = enter(inet, b.prev);
  ////if kind(inet,addr(a_next)) == FIX && kind(inet,addr(b_next)) == FIX {
    ////// If both ports are different, apply the fixpose transformation and compare
    ////if a_next != b_next {
      ////fixpose(inet, a_next, b_next);
      ////return compare(inet, a, b);
    ////}
    ////// If both ports are identical on slot 2, enter the merged fixpoint and compare
    ////if slot(a_next) == 2 {
      ////let a = &mut a.next(inet,0);
      ////let b = &mut b.next(inet,0);
      ////return compare(inet, a, b);
    ////}
  ////}

  ////if kind(inet,addr(a_next)) == FIX && kind(inet,addr(b_next)) != FIX {
    ////todo!()
  ////}

  ////if kind(inet,addr(a_next)) != FIX && kind(inet,addr(b_next)) == FIX {
    ////todo!()
  ////}
////}

//// Composes two fixed points:
//// @x(F x)     , @y(G y)
//// -------------------------- fixpose
//// @x(F (G x)) , @x(G (F x)))
//// https://twitter.com/VictorTaelin/status/1659724812057452549
//fn fixpose(inet: &mut INet, a: Port, b: Port) {
  //if kind(inet,addr(a)) == FIX
  //&& kind(inet,addr(b)) == FIX
  //&& slot(a) == 2
  //&& slot(b) == 2 {
    //let ff = enter(inet, port(addr(a), 0));
    //let fx = enter(inet, port(addr(a), 1));
    //let gg = enter(inet, port(addr(b), 0));
    //let gx = enter(inet, port(addr(b), 1));
    //let fu = new_node(inet, FIX + 1);
    //let fd = new_node(inet, FIX + 1);
    //let gu = new_node(inet, FIX + 1);
    //let gd = new_node(inet, FIX + 1);
    //let up = new_node(inet, FIX + 1);
    //let dw = new_node(inet, FIX + 1);
    //let cc = new_node(inet, FIX);
    //let rt = new_node(inet, FIX + 1);
    //link(inet, port(fu,0), ff);
    //link(inet, port(fu,1), port(gd,2));
    //link(inet, port(fu,2), port(up,1));
    //link(inet, port(gu,0), gg);
    //link(inet, port(gu,1), port(fd,2));
    //link(inet, port(gu,2), port(up,2));
    //link(inet, port(fd,0), fx);
    //link(inet, port(fd,1), port(dw,2));
    //link(inet, port(gd,0), gx);
    //link(inet, port(gd,1), port(dw,1));
    //link(inet, port(cc,0), port(up,0));
    //link(inet, port(cc,1), port(dw,0));
    //link(inet, port(cc,2), port(rt,0));
    //link(inet, port(rt,1), enter(inet,a));
    //link(inet, port(rt,2), enter(inet,b));
  //}
//}
