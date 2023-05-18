// Implements Interaction Combinators. The Abstract Calculus is directly isomorphic to them, so, to
// reduce a term, we simply translate to interaction combinators, reduce, then translate back.

#![allow(dead_code)]

#[derive(Clone, Debug)]
pub struct Stats {
  pub loops: u32,
  pub rules: u32,
  pub betas: u32,
  pub dupls: u32,
  pub annis: u32
}

#[derive(Clone, Debug)]
pub struct INet {
  pub nodes: Vec<u32>,
  pub reuse: Vec<u32>
}

// Node types are consts because those are used in a Vec<u32>.
pub const ERA : u32 = 0;
pub const CON : u32 = 1;
pub const FAN : u32 = 2;

pub type Port = u32;

// Allocates a new node, reclaiming a freed space if possible.
pub fn new_node(inet : &mut INet, kind : u32) -> u32 {
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
pub fn port(node : u32, slot : u32) -> Port {
  (node << 2) | slot
}

// Returns the address of a port (TODO: rename).
pub fn addr(port : Port) -> u32 {
  port >> 2
}

// Returns the slot of a port.
pub fn slot(port : Port) -> u32 {
  port & 3
}

// Enters a port, returning the port on the other side.
pub fn enter(inet : &INet, port : Port) -> Port {
  inet.nodes[port as usize]
}

// Type of the node.
// 0 = era (i.e., a set or a garbage collector)
// 1 = con (i.e., a lambda or an application)
// 2 = fan (i.e., a pair or a let)
pub fn kind(inet : &INet, node : u32) -> u32 {
  inet.nodes[port(node, 3) as usize]
}

// Links two ports.
pub fn link(inet : &mut INet, ptr_a : u32, ptr_b : u32) {
  inet.nodes[ptr_a as usize] = ptr_b;
  inet.nodes[ptr_b as usize] = ptr_a;
}

// Reduces a net to normal form lazily and sequentially.
pub fn reduce(inet : &mut INet) -> Stats {
  let mut stats = Stats { loops: 0, rules: 0, betas: 0, dupls: 0, annis: 0 };
  let mut warp : Vec<u32> = Vec::new();
  let mut exit : Vec<u32> = Vec::new();
  let mut next : Port = inet.nodes[0];
  let mut prev : Port;
  let mut back : Port;
  while next > 0 || warp.len() > 0 {
    next = if next == 0 { enter(inet, warp.pop().unwrap()) } else { next };
    prev = enter(inet, next);
    if slot(next) == 0 && slot(prev) == 0 && addr(prev) != 0 {
      stats.rules += 1;
      back = enter(inet, port(addr(prev), exit.pop().unwrap()));
      rewrite(inet, addr(prev), addr(next));
      next = enter(inet, back);
    } else if slot(next) == 0 {
      warp.push(port(addr(next), 2));
      next = enter(inet, port(addr(next), 1));
    } else {
      exit.push(slot(next));
      next = enter(inet, port(addr(next), 0));
    }
    stats.loops += 1;
  }
  stats
}

// Rewrites an active pair.
pub fn rewrite(inet : &mut INet, x : Port, y : Port) {
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
