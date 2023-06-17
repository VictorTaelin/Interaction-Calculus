//// Implements Interaction Combinators. The Interaction Calculus is directly isomorphic to them, so,
// to reduce a term, we simply translate to interaction combinators, reduce, then translate back.

#![allow(dead_code)]

mod core;
mod equal;
mod syntax;

pub use self::core::*;
pub use self::equal::*;
pub use self::syntax::*;

#[derive(Debug)]
pub struct INode {
  label: u32,
  ports: [String; 3]
}

type INodes = Vec<INode>;
