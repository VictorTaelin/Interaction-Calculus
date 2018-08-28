#![allow(dead_code)]

use std::collections::*;
use net::*;
use std;

// Terms of the Abstract-Calculus.
#[derive(Clone, Debug)]
pub enum Term {

    // Abstractions (affine functions).
    Lam {nam: Vec<u8>, bod: Box<Term>},                               

    // Applications.
    App {fun: Box<Term>, arg: Box<Term>},

    // Superposition (pairs).
    Sup {fst: Box<Term>, snd: Box<Term>},

    // Definitions (let).
    Dup {fst: Vec<u8>, snd: Vec<u8>, val: Box<Term>, nxt: Box<Term>},

    // Erasure (garbage collection).
    Era {val: Box<Term>, nxt: Box<Term>},

    // Variable.
    Var {nam: Vec<u8>}, 
}
use self::Term::{*};

// Source code is Ascii-encoded.
pub type Str = [u8];
pub type Chr = u8;

// Builds a var name from an index (0="a", 1="b", 26="aa"...).
pub fn new_name(idx : u32) -> Vec<Chr> {
    let mut name = Vec::new();
    let mut idx = idx + 1;
    while idx > 0 {
        idx = idx - 1;
        name.push((97 + idx % 26) as u8);
        idx = idx / 26;
    }
    return name;
}

// Parses a name, returns the remaining code and the name.
fn parse_name(code : &Str) -> (&Str, &Str) {
    let mut i : usize = 0;
    while i < code.len() && !(code[i] == b' ' || code[i] == b'\n') {
        i += 1;
    }
    (&code[i..], &code[0..i])
}

// Parses a term, returns the remaining code and the term.
pub fn parse_term<'a>(code : &'a Str) -> (&'a Str, Term) {
    match code[0] {
        // Whitespace
        b' ' => parse_term(&code[1..]),
        // Newline
        b'\n' => parse_term(&code[1..]),
        // Abstraction
        b'#' => {
            let (code, nam) = parse_name(&code[1..]);
            let (code, bod) = parse_term(code);
            let nam = nam.to_vec();
            let bod = Box::new(bod);
            (code, Lam{nam,bod})
        },
        // Application
        b':' => {
            let (code, fun) = parse_term(&code[1..]);
            let (code, arg) = parse_term(code);
            let fun = Box::new(fun);
            let arg = Box::new(arg);
            (code, App{fun,arg})
        },
        // Superposition
        b'&' => {
            let (code, fst) = parse_term(&code[1..]);
            let (code, snd) = parse_term(code);
            let fst = Box::new(fst);
            let snd = Box::new(snd);
            (code, Sup{fst,snd})
        },
        // Duplication
        b'@' => {
            let (code, fst) = parse_name(&code[1..]);
            let (code, snd) = parse_name(&code[1..]);
            let (code, val) = parse_term(code);
            let (code, nxt) = parse_term(code);
            let fst = fst.to_vec();
            let snd = snd.to_vec();
            let val = Box::new(val);
            let nxt = Box::new(nxt);
            (code, Dup{fst, snd, val, nxt})
        },
        // Erase
        b'-' => {
            let (code, val) = parse_term(&code[1..]);
            let (code, nxt) = parse_term(code);
            let val = Box::new(val);
            let nxt = Box::new(nxt);
            (code, Era{val,nxt})
        },
        // Variable
        _ => {
            let (code, nam) = parse_name(code);
            let nam = nam.to_vec();
            (code, Var{nam})
        }
    }
}

// Converts a source-code to a λ-term.
pub fn from_string<'a>(code : &'a Str) -> Term {
    parse_term(code).1
}

// Converts a λ-term back to a source-code.
pub fn to_string(term : &Term) -> Vec<Chr> {
    fn stringify_term(code : &mut Vec<u8>, term : &Term) {
        match term {
            &Lam{ref nam, ref bod} => {
                code.extend_from_slice(b"#");
                code.append(&mut nam.clone());
                code.extend_from_slice(b". ");
                stringify_term(code, &bod);
            },
            &App{ref fun, ref arg} => {
                code.extend_from_slice(b"(");
                stringify_term(code, &fun);
                code.extend_from_slice(b" ");
                stringify_term(code, &arg);
                code.extend_from_slice(b")");
            },
            &Sup{ref fst, ref snd} => {
                code.extend_from_slice(b"(");
                stringify_term(code, &fst);
                code.extend_from_slice(b",");
                stringify_term(code, &snd);
                code.extend_from_slice(b")");
            },
            &Dup{ref fst, ref snd, ref val, ref nxt} => {
                code.extend_from_slice(b"let (");
                code.append(&mut fst.clone());
                code.extend_from_slice(b",");
                code.append(&mut snd.clone());
                code.extend_from_slice(b") = ");
                stringify_term(code, &val);
                code.extend_from_slice(b" in\n");
                stringify_term(code, &nxt);
            },
            &Era{ref val, ref nxt} => {
                code.extend_from_slice(b"-");
                stringify_term(code, &val);
                code.extend_from_slice(b"\n");
                stringify_term(code, &nxt);
            },
            &Var{ref nam} => {
                code.append(&mut nam.clone());
            },
        }
    }
    let mut code = Vec::new();
    stringify_term(&mut code, &term);
    return code;
}

// Display macro.
impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(&to_string(&self)))
    }
}

// Converts a term to an Interaction-Combinator net. Both systems are directly isomorphic, so,
// each node of the Abstract-Calculus correspond to a single Interaction-Combinator node.
pub fn to_net(term : &Term) -> Net {
    fn encode_term(net : &mut Net, term : &Term, up : Port, scope : &mut HashMap<Vec<u8>,u32>, vars : &mut Vec<(Vec<u8>,u32)>) -> Port {
        match term {
            &Lam{ref nam, ref bod} => {
                let fun = new_node(net, 0);
                scope.insert(nam.to_vec(), port(fun, 1));
                let bod = encode_term(net, bod, port(fun, 2), scope, vars);
                link(net, port(fun, 2), bod);
                port(fun, 0)
            },
            &App{ref fun, ref arg} => {
                let app = new_node(net, 0);
                let fun = encode_term(net, fun, port(app, 0), scope, vars);
                link(net, port(app, 0), fun);
                let arg = encode_term(net, arg, port(app, 1), scope, vars);
                link(net, port(app, 1), arg);
                port(app, 2)
            },
            &Sup{ref fst, ref snd} => {
                let dup = new_node(net, 1);
                let fst = encode_term(net, fst, port(dup, 1), scope, vars);
                link(net, port(dup, 1), fst);
                let snd = encode_term(net, snd, port(dup, 2), scope, vars);
                link(net, port(dup, 2), snd);
                port(dup, 0)
            },
            &Dup{ref fst, ref snd, ref val, ref nxt} => {
                let dup = new_node(net, 1);
                scope.insert(fst.to_vec(), port(dup, 1));
                scope.insert(snd.to_vec(), port(dup, 2));
                let val = encode_term(net, &val, port(dup, 0), scope, vars);
                link(net, val, port(dup, 0));
                encode_term(net, &nxt, up, scope, vars)
            },
            &Era{ref val, ref nxt} => {
                let era = new_node(net, 2);
                link(net, port(era, 1), port(era, 2));
                let val = encode_term(net, &val, port(era, 0), scope, vars);
                link(net, val, port(era, 0));
                encode_term(net, &nxt, up, scope, vars)
            },
            Var{ref nam} => {
                vars.push((nam.to_vec(), up));
                up
            }
        }
    }
    // Initializes net with a root node
    let mut net = Net { nodes: vec![0,2,1,4], reuse: vec![] };
    let mut vars = Vec::new();
    let mut scope = HashMap::new();
    // Encodes the main term
    let main = encode_term(&mut net, &term, 0, &mut scope, &mut vars);
    // Links bound variables
    for i in 0..vars.len() {
        let (ref nam, var) = vars[i];
        match scope.get(nam) {
            Some(next) => {
                let next = *next;
                if enter(&net, next) == next {
                    link(&mut net, var, next);
                } else {
                    panic!("Variable used more than once: {}.", std::str::from_utf8(nam).unwrap());
                }
            },
            None => panic!("Unbound variable: {}.", std::str::from_utf8(nam).unwrap())
        }
    }
    // Links the term to the net's root
    link(&mut net, 0, main);
    net
}

// Converts an Interaction-Net node to an Abstract-Calculus term.
pub fn from_net(net : &Net) -> Term {
    fn name_of(next : Port, node_name : &mut HashMap<u32, Vec<u8>>) -> Vec<u8> {
        if !node_name.contains_key(&node(next)) {
            let nam = new_name(node_name.len() as u32);
            node_name.insert(node(next), nam.clone());
        }
        node_name.get(&node(next)).unwrap().to_vec()
    }
    fn read_term(net : &Net, next : Port, node_name : &mut HashMap<u32, Vec<u8>>, new_dups : &mut Vec<(Vec<u8>, u32)>) -> Term {
        if kind(net, node(next)) == 0 {
            match slot(next) {
                0 => {
                    let nam = new_name(node_name.len() as u32);
                    node_name.insert(node(next), nam.clone());
                    let dir = enter(net, port(node(next), 2));
                    let bod = read_term(net, dir, node_name, new_dups);
                    let mut lam = Lam{nam: nam, bod: Box::new(bod)};
                    lam
                },
                1 => {
                    Var{nam: name_of(next, node_name)}
                },
                _ => {
                    let dir = enter(net, port(node(next), 0));
                    let fun = read_term(net, dir, node_name, new_dups);
                    let dir = enter(net, port(node(next), 1));
                    let arg = read_term(net, dir, node_name, new_dups);
                    App{fun: Box::new(fun), arg: Box::new(arg)}
                }
            }
        } else {
            match slot(next) {
                0 => {
                    let dir = enter(net, port(node(next), 1));
                    let fst = read_term(net, dir, node_name, new_dups);
                    let dir = enter(net, port(node(next), 2));
                    let snd = read_term(net, dir, node_name, new_dups);
                    Sup{fst: Box::new(fst), snd: Box::new(snd)}
                },
                _ => {
                    if !node_name.contains_key(&node(next)) {
                        new_dups.push((name_of(next, node_name), node(next)))
                    }
                    let mut nam = name_of(next, node_name).clone();
                    nam.extend_from_slice(if slot(next) == 1 { b"0" } else { b"1" });
                    Var{nam}
                }
            }
        }
    }
    // Reads the main term from the net
    let mut node_name = HashMap::new();
    let mut new_dups = Vec::new();
    let mut main = read_term(net, enter(net, 0), &mut node_name, &mut new_dups);
    // Reads top-level let..in definitions found
    while new_dups.len() > 0 {
        let (nam, dup) = new_dups.pop().unwrap();
        let val = read_term(net, enter(net,port(dup,0)), &mut node_name, &mut new_dups);
        let mut fst = nam.clone();
        let mut snd = nam.clone();
        let val = Box::new(val);
        let nxt = Box::new(main);
        fst.extend_from_slice(b"0");
        snd.extend_from_slice(b"1");
        main = Dup{fst, snd, val, nxt};
    }
    main
}

// Reduces an Abstract-Calculus term through Interaction-Combinators.
pub fn reduce(term : &Term) -> Term {
    let mut net : Net = to_net(&term);
    ::net::reduce(&mut net);
    from_net(&net)
}
