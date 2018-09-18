// Extra functions that aren't used by the core algorithm.

use net::*;
use term::*;
use term::Term::*;

// Converts a lambda term (with non-affine functions) to a net. Reduction of the resulting net is
// *not* guaranteed to return the normal form of the original lambda term. (TODO. Should be based
// on the original to-net implementation of Absal.)
pub fn lambda_term_to_net(_term : &Term) -> Net {
    panic!("Not implemented.");
}

// Converts a net to a lambda term, turning explicit "lets" into non-affine functions. Will fail if
// the corresponding abstract-calculus term includes variables that scape a lambda's scope.
// TODO: support pairs (will crash if it has).
pub fn lambda_term_from_net(net : &Net) -> Term {
    fn go(net : &Net, node_depth : &mut Vec<u32>, next : Port, exit : &mut Vec<Port>, depth : u32) -> Term {
        let prev_port = enter(net, next);
        let prev_slot = slot(prev_port);
        let prev_node = addr(prev_port);
        if kind(net, prev_node) == 1 {
            match prev_slot {
                0 => {
                    node_depth[prev_node as usize] = depth;
                    let nam = new_name(depth);
                    let bod = Box::new(go(net, node_depth, port(prev_node, 2), exit, depth + 1));
                    Lam {nam, bod}
                },
                1 => {
                    let nam = new_name(node_depth[prev_node as usize]);
                    Var {nam}
                },
                _ => {
                    let fun = Box::new(go(net, node_depth, port(prev_node, 0), exit, depth));
                    let arg = Box::new(go(net, node_depth, port(prev_node, 1), exit, depth));
                    App {fun, arg}
                }
            }
        } else if prev_slot > 0 {
            exit.push(prev_slot);
            let term = go(net, node_depth, port(prev_node, 0), exit, depth);
            exit.pop();
            term
        } else {
            let e = exit.pop().unwrap();
            let term = go(net, node_depth, port(prev_node, e), exit, depth);
            exit.push(e);
            term
        }
    }
    let mut node_depth : Vec<u32> = Vec::with_capacity(net.nodes.len() / 4);
    let mut exit : Vec<u32> = Vec::new();
    node_depth.resize(net.nodes.len() / 4, 0);
    go(net, &mut node_depth, 0, &mut exit, 0)
}


