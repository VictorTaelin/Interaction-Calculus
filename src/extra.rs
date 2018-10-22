// Extra functions that aren't used by the core algorithm.

use net::*;
use term::*;
use term::Term::*;

// Converts a lambda term (with non-affine functions) to a net. Reduction of the resulting net is
// *not* guaranteed to return the normal form of the original lambda term.
pub fn lambda_term_to_net(term : &Term) -> Net {
    fn encode(net : &mut Net, label : &mut u32, scope : &mut Vec<(Vec<u8>, u32)>, term : &Term) -> Port {
        match term {
            &App{ref fun, ref arg} => {
                let app = new_node(net, CON);
                let fun = encode(net, label, scope, fun);
                link(net, port(app, 0), fun);
                let arg = encode(net, label, scope, arg);
                link(net, port(app, 1), arg);
                port(app, 2)
            },
            &Lam{ref nam, ref bod} => {
                let fun = new_node(net, CON);
                let era = new_node(net, ERA);
                link(net, port(fun, 1), port(era, 0));
                link(net, port(era, 1), port(era, 2));
                scope.push((nam.to_vec(), fun));
                let bod = encode(net, label, scope, bod);
                scope.pop();
                link(net, port(fun, 2), bod);
                port(fun, 0)
            },
            &Var{ref nam} => {
                let mut lam = 0;
                for i in 0..scope.len() {
                    if *nam == scope[i].0 {
                        lam = scope[i].1
                    }
                }
                if lam == 0 {
                    panic!("Invalid λ-term.");
                }
                let arg = enter(net, port(lam, 1));
                if kind(net, addr(arg)) == 0 {
                    net.reuse.push(addr(arg));
                    port(lam, 1)
                } else {
                    *label += 1;
                    let dup = new_node(net, *label);
                    link(net, port(dup, 2), arg);
                    link(net, port(dup, 0), port(lam, 1));
                    port(dup, 1)
                }
            },
            _ => panic!("Invalid λ-term.")
        }
    }
    let mut net : Net = Net { nodes: vec![0,2,1,4], reuse: vec![] };
    let mut label : u32 = 1;
    let mut scope : Vec<(Vec<u8>, u32)> = Vec::new();
    let ptr : Port = encode(&mut net, &mut label, &mut scope, term);
    link(&mut net, 0, ptr);
    net
}

// Converts a net to a lambda term, turning explicit "lets" into non-affine functions. Will fail if
// the corresponding abstract-calculus term includes variables that scape a lambda's scope.
pub fn lambda_term_from_net(net : &Net) -> Term {
    fn go(net : &Net, node_depth : &mut Vec<u32>, next : Port, exit : &mut Vec<Port>, depth : u32) -> Term {
        let prev_port = enter(net, next);
        let prev_slot = slot(prev_port);
        let prev_node = addr(prev_port);
        if kind(net, prev_node) == 1 {
            match prev_slot {
                0 => {
                    node_depth[prev_node as usize] = depth;
                    let nam = new_name(depth + 1);
                    let bod = Box::new(go(net, node_depth, port(prev_node, 2), exit, depth + 1));
                    Lam {nam, bod}
                },
                1 => {
                    let nam = new_name(node_depth[prev_node as usize] + 1);
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
