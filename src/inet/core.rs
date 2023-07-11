// Implements Interaction Combinators. The Interaction Calculus is directly isomorphic to them, so,
// to reduce a term, we simply translate to interaction combinators, reduce, then translate back.

use crate::term::{definition_book::DefinitionBook, alloc_at, read_at};
pub use super::*;

#[derive(Clone, Debug)]
pub struct INet {
  pub nodes: Vec<u32>,
  pub reuse: Vec<u32>,
  pub rules: u32,
}

pub type NodeKind = u32;

// Node types are consts because those are used in a Vec<u32>.
pub const TAG : NodeKind = 28;
pub const ERA : NodeKind = 0 << TAG;
pub const CON : NodeKind = 1 << TAG;
pub const ANN : NodeKind = 2 << TAG;
pub const DUP : NodeKind = 3 << TAG;
pub const FIX : NodeKind = 4 << TAG;
pub const REF : NodeKind = 5 << TAG;

pub const LABEL_MASK: NodeKind = ((1 << TAG) - 1);
pub const TAG_MASK: NodeKind = !LABEL_MASK;

// The ROOT port is on the deadlocked root node at address 0.
pub const ROOT : u32 = 1;

// A port is just a u32 combining address (30 bits) and slot (2 bits).
pub type Port = u32;

pub type NodeId = u32;

pub type SlotId = u32;

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
pub fn port(node: NodeId, slot: SlotId) -> Port {
  (node << 2) | slot
}

// Returns the address of a port (TODO: rename).
pub fn addr(port: Port) -> NodeId {
  port >> 2
}

// Returns the slot of a port.
pub fn slot(port: Port) -> SlotId {
  port & 3
}

// Enters a port, returning the port on the other side.
pub fn enter(inet: &INet, port: Port) -> Port {
  inet.nodes[port as usize]
}

// Enters a slot on the node pointed by this port.
pub fn get(inet: &INet, p: Port, s: SlotId) -> Port {
  enter(inet, port(addr(p), s))
}

// Kind of the node.
pub fn kind(inet: &INet, node: NodeId) -> NodeKind {
  inet.nodes[port(node, 3) as usize]
}

// Links two ports.
pub fn link(inet: &mut INet, ptr_a: Port, ptr_b: Port) {
  inet.nodes[ptr_a as usize] = ptr_b;
  inet.nodes[ptr_b as usize] = ptr_a;
}

// Reduces a wire to weak normal form.
pub fn reduce(inet: &mut INet, root: Port, skip: &dyn Fn(NodeKind, NodeKind) -> bool, definition_book: &DefinitionBook) {
  let mut path = vec![];
  let mut prev = root;
  loop {
    let mut next = enter(inet, prev);
    // If next is ROOT, stop.
    if next == ROOT {
      return;
    }
    // If next is a main port...
    if slot(next) == 0 {
      // Checks if caller asked to skip this rule.
      let skipped = skip(kind(inet,addr(prev)), kind(inet,addr(next)));
      // If prev is a main port, reduce the active pair.
      if slot(prev) == 0 && !skipped {
        if let Some(new_next) = rewrite(inet, addr(prev), addr(next), definition_book) {
          // Continue advancing because no rewrite happened (only REF substitution).
          // We need to overwrite `next` because the target node of the active pair was replaced.
          next = new_next;
        } else {
          inet.rules += 1;
          prev = path.pop().unwrap();
          continue;
        }
      } else {
        // Otherwise, return the axiom.
        return;
      }
    }
    // If next is an aux port, pass through.
    path.push(prev);
    prev = port(addr(next), 0);
  }
}

// Reduces the net to normal form.
pub fn normal(inet: &mut INet, root: Port, definition_book: &DefinitionBook) {
  let mut warp = vec![root];
  let mut tick = 0;
  while let Some(prev) = warp.pop() {
    reduce(inet, prev, &|ak,bk| false, definition_book);
    let next = enter(inet, prev);
    if slot(next) == 0 {
      warp.push(port(addr(next), 1));
      warp.push(port(addr(next), 2));
    }
  }
}

/// Rewrites an active pair.
/// Returns None if rewrite happened, Some(next) otherwise.
/// This is the case when `y` is REF node and was substituted by its body, and the
/// body's entry node doesn't form an active pair with `x`, e.g. `def A = (B C)`
/// The caller (`reduce`) can then use the returned port to continue advancing.
pub fn rewrite(inet: &mut INet, x: NodeId, y: NodeId, definition_book: &DefinitionBook) -> Option<Port> {
  /// Inserts the definition body in place of the REF node.
  /// Returns ID of node that replaced it (connected to principal port of other node)
  fn expand_ref_node(
    inet: &mut INet,
    definition_book: &DefinitionBook,
    ref_node: NodeId,
    other_node: NodeId,
    ref_kind: NodeKind,
  ) -> NodeId {
    inet.reuse.push(ref_node); // Remove REF node

    let definition_id = (ref_kind - REF) as usize;
    let definition_data = &definition_book.definition_id_to_data[definition_id];
    let host = port(other_node, 0);
    debug_assert_eq!(enter(inet, host), port(ref_node, 0));
    // TODO: Instead of calling `alloc_at`, insert nodes from precomputed `definition_data.net`.
    // and map node indices correctly (new node indices aren't necessarily contiguous).
    alloc_at(inet, &definition_data.term, host, &definition_book.definition_name_to_id);

    addr(enter(inet, host)) // Return ID of new node facing `other_node`
  }

  let kind_x = kind(inet, x);
  let kind_y = kind(inet, y);

  // x is `prev` in `reduce`, we came from `prev`. But REF nodes have ports 1 and 2 linked together.
  // So the `prev` node that we came from cannot be a REF.
  // This is important because if `prev` could potentially be a REF node, substituting it would
  // invalidate the return `path` in `reduce`.
  debug_assert_ne!(kind_x & TAG_MASK, REF);

  // We only need to check if `y` is a REF node
  let (y, kind_y) = if kind_y & TAG_MASK == REF {
    let (mut y, mut kind_y) = (y, kind_y);
    // Substitute REF node by its body, repeatedly if the body is another REF node (e.g. `def A = B`)
    while {
      y = expand_ref_node(inet, definition_book, y, x, kind_y);
      kind_y = kind(inet, y);
      kind_y & TAG_MASK == REF
    } {}
    let target_port = enter(inet, port(x, 0));
    if target_port != port(y, 0) {
      // If the inserted subnet's entry wire is not at port 0, (x,y) is not an active pair anymore.
      // E.g. an application: def A = (B C)
      // So we cannot rewrite this new pair, but the caller (`reduce`) can keep advancing
      // using `target_port` as its `next` port, which is why we return it here.
      return Some(target_port);
    }
    (y, kind_y)
  } else {
    (y, kind_y)
  };

  if kind_x == kind_y {
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
  None
}

