// Extra functions. Those are not used by the core algorithm and are just conveniences for libs
// interacting with it.

use crate::net::*;
use crate::term::Term::*;
use crate::term::*;

// Converts a lambda term (with non-affine functions) to a net. Reduction of the resulting net is
// *not* guaranteed to return the normal form of the original lambda term.
#[allow(dead_code)]
pub fn lambda_term_to_net(term: &Term) -> Net {
    fn encode(
        net: &mut Net,
        label: &mut u32,
        scope: &mut Vec<(Vec<u8>, u32)>,
        term: &Term,
    ) -> Port {
        match term {
            App { ref fun, ref arg } => {
                let app = new_node(net, CON);
                let fun = encode(net, label, scope, fun);
                link(net, port(app, 0), fun);
                let arg = encode(net, label, scope, arg);
                link(net, port(app, 1), arg);
                port(app, 2)
            }
            Lam { ref nam, ref bod } => {
                let fun = new_node(net, CON);
                let era = new_node(net, ERA);
                link(net, port(fun, 1), port(era, 0));
                link(net, port(era, 1), port(era, 2));
                scope.push((nam.to_vec(), fun));
                let bod = encode(net, label, scope, bod);
                scope.pop();
                link(net, port(fun, 2), bod);
                port(fun, 0)
            }
            Var { ref nam } => {
                let mut lam = 0;
                for &(ref name, idx) in scope.iter() {
                    if nam == name {
                        lam = idx;
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
            }
            _ => panic!("Invalid λ-term."),
        }
    }
    let mut net: Net = Net {
        nodes: vec![0, 2, 1, 4],
        reuse: vec![],
    };
    let mut label: u32 = 1;
    let mut scope: Vec<(Vec<u8>, u32)> = Vec::new();
    let ptr: Port = encode(&mut net, &mut label, &mut scope, term);
    link(&mut net, 0, ptr);
    net
}

// Converts a net to a lambda term, turning explicit "lets" into non-affine functions. Will fail if
// the corresponding abstract-calculus term includes variables that scape a lambda's scope.
#[allow(dead_code)]
pub fn lambda_term_from_net(net: &Net) -> Term {
    fn go(
        net: &Net,
        node_depth: &mut Vec<u32>,
        next: Port,
        exit: &mut Vec<Port>,
        depth: u32,
    ) -> Term {
        let prev_port = enter(net, next);
        let prev_slot = slot(prev_port);
        let prev_node = addr(prev_port);
        if kind(net, prev_node) == 1 {
            match prev_slot {
                0 => {
                    node_depth[prev_node as usize] = depth;
                    let nam = new_name(depth + 1);
                    let bod = Box::new(go(net, node_depth, port(prev_node, 2), exit, depth + 1));
                    Lam { nam, bod }
                }
                1 => {
                    let nam = new_name(node_depth[prev_node as usize] + 1);
                    Var { nam }
                }
                _ => {
                    let fun = Box::new(go(net, node_depth, port(prev_node, 0), exit, depth));
                    let arg = Box::new(go(net, node_depth, port(prev_node, 1), exit, depth));
                    App { fun, arg }
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
    let mut node_depth: Vec<u32> = Vec::with_capacity(net.nodes.len() / 4);
    let mut exit: Vec<u32> = Vec::new();
    node_depth.resize(net.nodes.len() / 4, 0);
    go(net, &mut node_depth, 0, &mut exit, 0)
}

// Converts a binary input such as b"1001" into a λ-encoded bitstring
// such as λa.λb.λc.(a λa.λb.λc.(b λa.λb.λc.(b λa.λb.λc.(a λa.λb.λc.c))))
pub fn bitstring_to_term(s: &[u8], i: u32) -> Term {
    match if !s.is_empty() { s[0] } else { b' ' } {
        b'0' => {
            let nam = new_name(i + 1);
            let app = Term::App {
                fun: Box::new(Var { nam: nam.clone() }),
                arg: Box::new(bitstring_to_term(&s[1..], i + 1)),
            };
            let e_lam = Term::Lam {
                nam: b"-".to_vec(),
                bod: Box::new(app),
            };
            let i_lam = Term::Lam {
                nam: b"-".to_vec(),
                bod: Box::new(e_lam),
            };

            Term::Lam {
                nam,
                bod: Box::new(i_lam),
            }
        }
        b'1' => {
            let nam = new_name(i + 1);
            let app = Term::App {
                fun: Box::new(Var { nam: nam.clone() }),
                arg: Box::new(bitstring_to_term(&s[1..], i + 1)),
            };
            let e_lam = Term::Lam {
                nam: b"-".to_vec(),
                bod: Box::new(app),
            };
            let i_lam = Term::Lam {
                nam,
                bod: Box::new(e_lam),
            };

            Term::Lam {
                nam: b"-".to_vec(),
                bod: Box::new(i_lam),
            }
        }
        _ => {
            let nam = new_name(i + 1);
            let var = Var { nam: nam.clone() };
            let e_lam = Term::Lam {
                nam,
                bod: Box::new(var),
            };
            let i_lam = Term::Lam {
                nam: b"-".to_vec(),
                bod: Box::new(e_lam),
            };

            Term::Lam {
                nam: b"-".to_vec(),
                bod: Box::new(i_lam),
            }
        }
    }
}

// Can this highly-idented style be improved?
pub fn term_to_bitstring(t: &Term) -> Vec<u8> {
    fn format_binary_output(t: &Term, v: &mut Vec<u8>) {
        if let Term::Lam {
            nam: ref o_nam,
            bod: ref o_bod,
        } = t
        {
            if let Term::Lam {
                nam: ref i_nam,
                bod: ref i_bod,
            } = **o_bod
            {
                if let Term::Lam {
                    nam: _,
                    bod: ref e_bod,
                } = **i_bod
                {
                    if let Term::App {
                        fun: ref app_fun,
                        arg: ref app_arg,
                    } = **e_bod
                    {
                        if let Term::Var { nam: ref var_nam } = **app_fun {
                            if var_nam == o_nam {
                                v.extend_from_slice(b"0");
                                format_binary_output(app_arg, v);
                            } else if var_nam == i_nam {
                                v.extend_from_slice(b"1");
                                format_binary_output(app_arg, v);
                            }
                        }
                    }
                }
            }
        }
    }
    let mut v: Vec<u8> = Vec::new();
    format_binary_output(t, &mut v);
    v
}

// Converts a bitstring (up to 8 bits) to a character.
pub fn bits_to_char(s: &[u8]) -> u8 {
    let mut c = 0;
    for &si in s.iter().take(8) {
        c = c * 2 + u8::from(si == b'0');
    }
    c
}

// Converts a character to a bitstring.
pub fn char_to_bits(c: u8) -> Vec<u8> {
    let mut v: Vec<u8> = Vec::new();
    let mut c = c;
    for _i in 0..8 {
        v.extend_from_slice(if c % 2 == 0 { b"0" } else { b"1" });
        c /= 2;
    }
    v.reverse();
    v
}

// Converts a bitstring to an ascii string.
pub fn bits_to_ascii(s: &[u8]) -> Vec<u8> {
    let mut v: Vec<u8> = Vec::new();
    for i in 0..s.len() / 8 {
        v.push(bits_to_char(&s[i * 8..i * 8 + 8]));
    }
    v
}

// Converts an ascii string to a bitstring.
pub fn ascii_to_bits(a: &[u8]) -> Vec<u8> {
    let mut v: Vec<u8> = Vec::new();
    for &ai in a.iter() {
        v.append(&mut char_to_bits(ai));
    }
    v
}
