// Implements Interaction Combinators. The Interaction Calculus is directly isomorphic to them, so,
// to reduce a term, we simply translate to interaction combinators, reduce, then translate back.

#![allow(dead_code)]

#[derive(Clone, Debug)]
pub struct INet {
  pub nodes: Vec<u32>,
  pub reuse: Vec<u32>,
  pub rules: u32,
}

// Node types are consts because those are used in a Vec<u32>.
pub const TAG : u32 = 28;
pub const ERA : u32 = 0 << TAG;
pub const CON : u32 = 1 << TAG;
pub const ANN : u32 = 2 << TAG;
pub const DUP : u32 = 3 << TAG;
pub const FIX : u32 = 4 << TAG;
pub const OBS : u32 = 5 << TAG;

// The ROOT port is on the deadlocked root node at address 0.
pub const ROOT : u32 = 1;

// A port is just a u32 combining address (30 bits) and slot (2 bits).
pub type Port = u32;

// Create a new net, with a deadlocked root node.
pub fn new_inet() -> INet {
  INet {
    nodes: vec![2,1,0,0], // p2 points to p0, p1 points to net
    reuse: vec![],
    rules: 0
  }
}

// Allocates a new node, reclaiming a freed space if possible.
pub fn new_node(inet: &mut INet, kind: u32) -> u32 {
  let node : u32 = match inet.reuse.pop() {
    Some(index) => index,
    None => {
      let len = inet.nodes.len();
      inet.nodes.resize(len + 4, 0);
      (len as u32) / 4
    }
  };
  inet.nodes[port(node, 0) as usize] = port(node, 0);
  inet.nodes[port(node, 1) as usize] = port(node, 1);
  inet.nodes[port(node, 2) as usize] = port(node, 2);
  inet.nodes[port(node, 3) as usize] = kind;
  return node;
}

// Builds a port (an address / slot pair).
pub fn port(node: u32, slot: u32) -> Port {
  (node << 2) | slot
}

// Returns the address of a port (TODO: rename).
pub fn addr(port: Port) -> u32 {
  port >> 2
}

// Returns the slot of a port.
pub fn slot(port: Port) -> u32 {
  port & 3
}

// Enters a port, returning the port on the other side.
pub fn enter(inet: &INet, port: Port) -> Port {
  inet.nodes[port as usize]
}

// Enters a slot on the node pointed by this port.
pub fn get(inet: &INet, p: Port, s: u32) -> Port {
  enter(inet, port(addr(p), s))
}

// Kind of the node.
pub fn kind(inet: &INet, node: u32) -> u32 {
  inet.nodes[port(node, 3) as usize]
}

// Links two ports.
pub fn link(inet: &mut INet, ptr_a: u32, ptr_b: u32) {
  inet.nodes[ptr_a as usize] = ptr_b;
  inet.nodes[ptr_b as usize] = ptr_a;
}

// Reduces a wire to weak normal form.
pub fn reduce(inet: &mut INet, root: Port, skip: &dyn Fn(u32,u32) -> bool) -> Port {
  let mut path = vec![];
  let mut prev = root;
  loop {
    let next = enter(inet, prev);
    // If next is ROOT, stop.
    if next == ROOT {
      return enter(inet, path.get(0).cloned().unwrap_or(ROOT));
    }
    // If next is a main port...
    if slot(next) == 0 {
      // Checks if caller asked to skip this rule.
      let skipped = skip(kind(inet,addr(prev)), kind(inet,addr(next)));
      // If prev is a main port, reduce the active pair.
      if slot(prev) == 0 && !skipped {
        inet.rules += 1;
        rewrite(inet, addr(prev), addr(next));
        prev = path.pop().unwrap();
        continue;
      // Otherwise, return the axiom.
      } else {
        return next;
      }
    }
    // If next is an aux port, pass through.
    path.push(prev);
    prev = port(addr(next), 0);
  }
}

// Reduces the net to normal form.
pub fn normal(inet: &mut INet, root: Port) {
  let mut warp = vec![root];
  while let Some(prev) = warp.pop() {
    let next = reduce(inet, prev, &|ak,bk| false);
    if slot(next) == 0 {
      warp.push(port(addr(next), 1));
      warp.push(port(addr(next), 2));
    }
  }
}

// Rewrites an active pair.
pub fn rewrite(inet: &mut INet, x: Port, y: Port) {
  if kind(inet, x) == kind(inet, y) {
    let p0 = enter(inet, port(x, 1));
    let p1 = enter(inet, port(y, 1));
    link(inet, p0, p1);
    let p0 = enter(inet, port(x, 2));
    let p1 = enter(inet, port(y, 2));
    link(inet, p0, p1);
    inet.reuse.push(x);
    inet.reuse.push(y);
  } else {
    let t = kind(inet, x);
    let a = new_node(inet, t);
    let t = kind(inet, y);
    let b = new_node(inet, t);
    let t = enter(inet, port(x, 1));
    link(inet, port(b, 0), t);
    let t = enter(inet, port(x, 2));
    link(inet, port(y, 0), t);
    let t = enter(inet, port(y, 1));
    link(inet, port(a, 0), t);
    let t = enter(inet, port(y, 2));
    link(inet, port(x, 0), t);
    link(inet, port(a, 1), port(b, 1));
    link(inet, port(a, 2), port(y, 1));
    link(inet, port(x, 1), port(b, 2));
    link(inet, port(x, 2), port(y, 2));
  }
}

// Equality on Interaction Combinators
//
// A Path is a deque of aux slots (1 or 2)
//
// > type Slot = 1 | 2
// > type Path = [Slot]
//
// A cursor has a root port, a prev port, and a map of paths
//
// > type Cursor = { root: Port, prev: Port, path: Map<Kind, Path> }
// 
// The equality function returns if two nets are equal
//
// > eq : INet -> Cursor -> Cursor -> Bool
//
// If we're on root, compare both deque maps
//
// > eq a am b bm = am == bm
//
// If main port, non-empty deque: pop_back a slot from this deque, and move to it
//
// > eq ak#[*a0 a1 a2] {[ak]:(1,ap),..am} b bm = eq a1 {[ak]:ap,..am} b bm
// > eq ak#[*a0 a1 a2] {[ak]:(2,ap),..am} b bm = eq a2 {[ak]:ap,..am} b bm
//
// If main port, empty deque: push_front [1,2] slots to the other deque, and move to both
//
// eq ak#[*a0 a1 a2] {[ak]:ap,..am} b {[ak]:bs,..bm} = eq a1 ap b {[ak]:(bs,1),..bm}
//                                                   & eq a2 ap b {[ak]:(bs,2),..bm}
//
// If aux port, push_back it to this deque, and move to main port
//
// > eq ak[a0 *a1 a2] {[ak]:ap,..am} b bm = eq a1 {[ak]:(1,ap),..am} b bm
// > eq ak[a0 a1 *a2] {[ak]:ap,..am} b bm = eq a1 {[ak]:(2,ap),..am} b bm

use std::collections::HashMap;
use std::collections::VecDeque;

pub struct Cursor<'a> {
  root: Port,
  prev: Port,
  path: &'a mut HashMap<u32, VecDeque<u8>>,
}

impl Cursor<'_> {
  fn next(&mut self, inet: &mut INet, slot: u8) -> Cursor {
    Cursor {
      root: self.root,
      prev: port(addr(enter(inet, self.prev)), slot as u32),
      path: self.path,
    }
  }
}

// Checks if two interaction nets ports are equal.
pub fn equal(inet: &mut INet, a: Port, b: Port) -> bool {
  let mut a_path = HashMap::new();
  let mut b_path = HashMap::new();
  let mut a_cursor = Cursor { root: a, prev: a, path: &mut a_path };
  let mut b_cursor = Cursor { root: b, prev: b, path: &mut b_path };
  compare(inet, &mut a_cursor, &mut b_cursor)
}

// Compares two cursors by moving them forward until root is reached, and comparing paths
pub fn compare(inet: &mut INet, a: &mut Cursor, b: &mut Cursor) -> bool {
  //println!("eq {} {} {:?} {:?}", a.prev, b.prev, a.path, b.path);
  //println!("== {}", crate::term::read_at(inet, a.prev));
  //println!("== {}", crate::term::read_at(inet, b.prev));

  // Moves one of the cursors forward and compares
  fn advance(inet: &mut INet, a: &mut Cursor, b: &mut Cursor) -> Option<bool> {
    let a_next = reduce(inet, a.prev, &|ak, bk| false);
    let a_kind = kind(inet, addr(a_next));

    // If on root, there is nothing to do
    if a_next == a.root || a_next == ROOT {
      return None;

    // If entering main port...
    } else if slot(enter(inet, a.prev)) == 0 {

      // If deque isn't empty, pop_back a slot and move to it
      if let Some(slot) = a.path.get_mut(&a_kind).and_then(|vec| vec.pop_back()) {
        let an = &mut a.next(inet, slot);
        let eq = compare(inet, an, b);
        a.path.entry(a_kind).or_default().push_back(slot);
        return Some(eq);

      // If deque is empty, move to slots [1,2] and push_front to the *other* deque
      } else {
        for slot in [1,2] {
          b.path.entry(a_kind).or_default().push_front(slot);
          let an = &mut a.next(inet, slot);
          let eq = compare(inet, an, b);
          b.path.get_mut(&a_kind).unwrap().pop_front();
          if !eq {
            return Some(false);
          }
        }
        return Some(true);
      }

    // If entering an aux port, push_back that slot to the deque, and move to the main port
    } else {
      a.path.entry(a_kind).or_default().push_back(slot(enter(inet, a.prev)) as u8);
      let an = &mut a.next(inet, 0);
      let eq = compare(inet, an, b);
      a.path.get_mut(&a_kind).unwrap().pop_back();
      return Some(eq);
    }
  }

  // If 'a' can be advanced, advance it and compare
  if let Some(eq) = advance(inet, a, b) {
    return eq;
  }

  // If 'b' can be advanced, advance it and compare
  if let Some(eq) = advance(inet, b, a) {
    return eq;
  }

  // Otherwise, compare the resulting paths.
  // We get CON for DUP-invariant equality.
  return a.path.get(&CON) == b.path.get(&CON);
}
