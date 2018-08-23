//True  = #tt #tf tt
//False = #ft #ff ff
//not   = #nb #nt #nf ::nb nt nf 
//:not True

#![allow(dead_code)]

use std::collections::*;
use net::*;
use std;

// λ-Terms are either lambdas, variables or applications.
#[derive(Clone, Debug)]
pub enum Term {
    Bth {fst: Box<Term>, snd: Box<Term>},
    App {fun: Box<Term>, arg: Box<Term>},
    Lam {nam: Vec<u8>, bod: Box<Term>},
    Var {nam: Vec<u8>},
}
use self::Term::{*};

pub struct Prose {
    defs: Vec<(Vec<u8>, Term)>,
    main: Term
}

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
        // Both
        b'&' => {
            let (code, fst) = parse_term(&code[1..]);
            let (code, snd) = parse_term(code);
            let fst = Box::new(fst);
            let snd = Box::new(snd);
            (code, Bth{fst,snd})
        },
        // Applicationn
        b':' => {
            let (code, fun) = parse_term(&code[1..]);
            let (code, arg) = parse_term(code);
            let fun = Box::new(fun);
            let arg = Box::new(arg);
            (code, App{fun,arg})
        },
        // Lambda
        b'#' => {
            let (code, nam) = parse_name(&code[1..]);
            let (code, bod) = parse_term(code);
            let nam = nam.to_vec();
            let bod = Box::new(bod);
            (code, Lam{nam,bod})
        },
        // Variable
        _ => {
            let (code, nam) = parse_name(code);
            let nam = nam.to_vec();
            (code, Var{nam})
        }
    }
}

// Parses a prose, returns the remaining code and the prose.
pub fn parse_prose<'a>(code : &'a Str) -> (&'a Str, Prose) {
    let mut code = code;
    let mut defs = Vec::new();
    while (code[0] as char).is_ascii_whitespace() {
        code = &code[1..];
    }
    while code[0] == b'@' {
        let (new_code, nam) = parse_name(&code[1..]);
        let (new_code, def) = parse_term(new_code);
        defs.push((nam.to_vec(), def));
        code = new_code;
        while (code[0] as char).is_ascii_whitespace() {
            code = &code[1..];
        }
    }
    let (code, main) = parse_term(code);
    (code, Prose{defs, main})
}

// Converts a source-code to a λ-term.
pub fn from_string<'a>(code : &'a Str) -> Prose {
    parse_prose(code).1
}

// Converts a λ-term back to a source-code.
pub fn to_string(prose : &Prose) -> Vec<Chr> {
    fn stringify_term(code : &mut Vec<u8>, term : &Term) {
        match term {
            &Bth{ref fst, ref snd} => {
                code.extend_from_slice(b"(");
                stringify_term(code, &fst);
                code.extend_from_slice(b" ");
                code.extend_from_slice(b"&");
                code.extend_from_slice(b" ");
                stringify_term(code, &snd);
                code.extend_from_slice(b")");
            },
            &App{ref fun, ref arg} => {
                code.extend_from_slice(b"(");
                stringify_term(code, &fun);
                code.extend_from_slice(b" ");
                stringify_term(code, &arg);
                code.extend_from_slice(b")");
            },
            &Lam{ref nam, ref bod} => {
                code.extend_from_slice(b"#");
                code.append(&mut nam.clone());
                code.extend_from_slice(b".");
                stringify_term(code, &bod);
            }
            &Var{ref nam} => {
                code.append(&mut nam.clone());
            },
        }
    }
    let mut code = Vec::new();
    for i in 0..prose.defs.len() {
        let mut nam : Vec<u8> = prose.defs[i].0.clone();
        code.append(&mut nam);
        code.extend_from_slice(b" = ");
        stringify_term(&mut code, &prose.defs[i].1);
        code.extend_from_slice(b"\n");
    }
    stringify_term(&mut code, &prose.main);
    return code;
}

impl std::fmt::Display for Prose {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(&to_string(&self)))
    }
}

pub fn to_net(prose : &Prose) -> Net {
    fn encode_term(net : &mut Net, term : &Term, up : Port, scope : &mut HashMap<Vec<u8>,u32>, vars : &mut Vec<(Vec<u8>,u32)>) -> Port {
        match term {
            &Bth{ref fst, ref snd} => {
                let dup = new_node(net, 1);
                let fst = encode_term(net, fst, port(dup, 1), scope, vars);
                link(net, port(dup, 1), fst);
                let snd = encode_term(net, snd, port(dup, 2), scope, vars);
                link(net, port(dup, 2), snd);
                port(dup, 0)
            },
            &App{ref fun, ref arg} => {
                let app = new_node(net, 0);
                let fun = encode_term(net, fun, port(app, 0), scope, vars);
                link(net, port(app, 0), fun);
                let arg = encode_term(net, arg, port(app, 1), scope, vars);
                link(net, port(app, 1), arg);
                port(app, 2)
            },
            &Lam{ref nam, ref bod} => {
                let fun = new_node(net, 0);
                link(net, port(fun, 1), port(fun, 1));
                scope.insert(nam.to_vec(), fun);
                let bod = encode_term(net, bod, port(fun, 2), scope, vars);
                link(net, port(fun, 2), bod);
                port(fun, 0)
            },
            Var{ref nam} => {
                vars.push((nam.to_vec(), up));
                up
            }
        }
    }
    let mut net = Net { nodes: vec![0,2,1,4], reuse: vec![] };
    let mut vars = Vec::new();
    let mut scope = HashMap::new();
    for i in 0..prose.defs.len() {
        let dup = new_node(&mut net, 1);
        scope.insert(prose.defs[i].0.to_vec(), dup);
        let def = encode_term(&mut net, &prose.defs[i].1, port(dup, 0), &mut scope, &mut vars);
        link(&mut net, def, port(dup, 0));
    }
    let root = encode_term(&mut net, &prose.main, 0, &mut scope, &mut vars);
    for i in 0..vars.len() {
        let (ref nam, var) = vars[i];
        match scope.get(nam) {
            Some(nod) => {
                let nod = *nod;
                if kind(&net,nod) == 0 {
                    if enter(&net, port(nod,1)) == port(nod, 1) {
                        link(&mut net, var, port(nod, 1));
                    } else {
                        panic!("Lambda variable used more than once.");
                    }
                } else {
                    if enter(&net, port(nod, 1)) == port(nod, 1) {
                        link(&mut net, var, port(nod, 1));
                    } else if enter(&net, port(nod, 2)) == port(nod, 2) {
                        link(&mut net, var, port(nod, 2));
                    } else {
                        panic!("Definition variable used more than twice.");
                    }
                }
            },
            None => panic!("Unbound variable: {}.", std::str::from_utf8(nam).unwrap())
        }
    }
    link(&mut net, 0, root);
    net
}

pub fn from_net(net : &Net) -> Prose {
    fn name_of(net : &Net, next : Port, node_name : &mut HashMap<u32, Vec<u8>>) -> Vec<u8> {
        if !node_name.contains_key(&node(next)) {
            let nam = new_name(node_name.len() as u32);
            node_name.insert(node(next), nam.clone());
        }
        node_name.get(&node(next)).unwrap().to_vec()
    }
    fn read_term(net : &Net, next : Port, node_name : &mut HashMap<u32, Vec<u8>>, new_defs : &mut Vec<(Vec<u8>, u32)>) -> Term {
        if kind(net, node(next)) == 0 {
            match slot(next) {
                0 => {
                    let nam = new_name(node_name.len() as u32);
                    node_name.insert(node(next), nam.clone());
                    let dir = enter(net, port(node(next), 2));
                    let bod = read_term(net, dir, node_name, new_defs);
                    let mut lam = Lam{nam: nam, bod: Box::new(bod)};
                    lam
                },
                1 => {
                    Var{nam: name_of(net, next, node_name)}
                },
                _ => {
                    let dir = enter(net, port(node(next), 0));
                    let fun = read_term(net, dir, node_name, new_defs);
                    let dir = enter(net, port(node(next), 1));
                    let arg = read_term(net, dir, node_name, new_defs);
                    App{fun: Box::new(fun), arg: Box::new(arg)}
                }
            }
        } else {
            match slot(next) {
                0 => {
                    let dir = enter(net, port(node(next), 1));
                    let fst = read_term(net, dir, node_name, new_defs);
                    let dir = enter(net, port(node(next), 2));
                    let snd = read_term(net, dir, node_name, new_defs);
                    Bth{fst: Box::new(fst), snd: Box::new(snd)}
                },
                _ => {
                    if !node_name.contains_key(&node(next)) {
                        new_defs.push((name_of(net, next, node_name), node(next)))
                    }
                    Var{nam: name_of(net, next, node_name)}
                }
            }
        }
    }
    let mut node_name = HashMap::new();
    let mut new_defs = Vec::new();
    let main = read_term(net, enter(net, 0), &mut node_name, &mut new_defs);
    let mut prose = Prose { defs: Vec::new(), main: main };
    while new_defs.len() > 0 {
        let (nam, nod) = new_defs.pop().unwrap();
        let def = read_term(net, enter(net,port(nod,0)), &mut node_name, &mut new_defs);
        prose.defs.push((nam, def));
    }
    prose
}

pub fn reduce(prose : &Prose) -> Prose {
    let mut net : Net = to_net(&prose);
    ::net::reduce(&mut net);
    from_net(&net)
}
