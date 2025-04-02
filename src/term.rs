#![allow(dead_code)]

use std::collections::*;
use inet::*;
use std;

// Terms of the Interaction Calculus.
#[derive(Clone, Debug)]
pub enum Term {
  // Abstractions
  Lam {nam: Vec<u8>, bod: Box<Term>},                

  // Applications
  App {fun: Box<Term>, arg: Box<Term>},

  // Superpositions
  Sup {tag: u32, fst: Box<Term>, snd: Box<Term>},

  // Duplications
  Dup {tag: u32, fst: Vec<u8>, snd: Vec<u8>, val: Box<Term>, nxt: Box<Term>},

  // Variables
  Var {nam: Vec<u8>}, 

  // Erasure
  Era
}

use self::Term::{*};

// Conversion
// ==========

// Converts a term to an inet, injecting at location.
pub fn inject(inet: &mut INet, term: &Term, host: Port) {
  fn encode_term(
    net   : &mut INet,
    term  : &Term,
    up    : Port,
    scope : &mut HashMap<Vec<u8>,u32>,
    vars  : &mut Vec<(Vec<u8>,u32)>
  ) -> Port {
    match term {
      // A lambda becomes to a con node. Ports:
      // - 0: points to where the lambda occurs.
      // - 1: points to the lambda variable.
      // - 2: points to the lambda body.
      &Lam { ref nam, ref bod } => {
        let fun = new_node(net, CON);
        scope.insert(nam.to_vec(), port(fun, 1));
        if nam == b"*" {
          let era = new_node(net, ERA);
          link(net, port(era, 1), port(era, 2));
          link(net, port(fun, 1), port(era, 0));
        }
        let bod = encode_term(net, bod, port(fun, 2), scope, vars);
        link(net, port(fun, 2), bod);
        port(fun, 0)
      },
      // An application becomes to a con node too. Ports:
      // - 0: points to the function being applied.
      // - 1: points to the function's argument.
      // - 2: points to where the application occurs.
      &App{ref fun, ref arg} => {
        let app = new_node(net, CON);
        let fun = encode_term(net, fun, port(app, 0), scope, vars);
        link(net, port(app, 0), fun);
        let arg = encode_term(net, arg, port(app, 1), scope, vars);
        link(net, port(app, 1), arg);
        port(app, 2)
      },
      // A pair becomes a dup node. Ports:
      // - 0: points to where the pair occurs.
      // - 1: points to the first value.
      // - 2: points to the second value.
      &Sup{tag, ref fst, ref snd} => {
        let dup = new_node(net, DUP + tag);
        let fst = encode_term(net, fst, port(dup, 1), scope, vars);
        link(net, port(dup, 1), fst);
        let snd = encode_term(net, snd, port(dup, 2), scope, vars);
        link(net, port(dup, 2), snd);
        port(dup, 0)
      },
      // A dup becomes a dup node too. Ports:
      // - 0: points to the value projected.
      // - 1: points to the occurrence of the first variable.
      // - 2: points to the occurrence of the second variable.
      &Dup{tag, ref fst, ref snd, ref val, ref nxt} => {
        let dup = new_node(net, DUP + tag);
        scope.insert(fst.to_vec(), port(dup, 1));
        scope.insert(snd.to_vec(), port(dup, 2));
        // If the first variable is unused, create an erase node.
        if fst == b"*" {
          let era = new_node(net, ERA);
          link(net, port(era, 1), port(era, 2));
          link(net, port(dup, 1), port(era, 0));
        }
        // If the second variable is unused, create an erase node.
        if snd == b"*" {
          let era = new_node(net, ERA);
          link(net, port(era, 1), port(era, 2));
          link(net, port(dup, 2), port(era, 0));
        }
        let val = encode_term(net, &val, port(dup, 0), scope, vars);
        link(net, val, port(dup, 0));
        encode_term(net, &nxt, up, scope, vars)
      },
      // A set is just an erase node stored in a place.
      &Era => {
        let set = new_node(net, ERA);
        link(net, port(set, 1), port(set, 2));
        port(set, 0)
      },
      Var{ref nam} => {
        vars.push((nam.to_vec(), up));
        up
      }
    }
  }

  // Initializes state variables
  let mut vars = Vec::new();
  let mut scope = HashMap::new();

  // Encodes the main term.
  let main = encode_term(inet, &term, host, &mut scope, &mut vars);

  // Links bound variables.
  for i in 0..vars.len() {
    let (ref nam, var) = vars[i];
    match scope.get(nam) {
      Some(next) => {
        let next = *next;
        if enter(&inet, next) == next {
          link(inet, var, next);
        } else {
          panic!("Variable used more than once: {}.", std::str::from_utf8(nam).unwrap());
        }
      },
      None => panic!("Unbound variable: {}.", std::str::from_utf8(nam).unwrap())
    }
  }

  // Connects unbound variables to erasure nodes
  for (_, addr) in scope {
    if enter(&inet, addr) == addr {
      let era = new_node(inet, ERA);
      link(inet, port(era, 1), port(era, 2));
      link(inet, addr, port(era, 0));
    }
  }

  link(inet, host, main);
}

// Converts an inet to a term, injecting on ROOT location.
pub fn to_net(term: &Term) -> INet {
  let mut inet = new_inet();
  inject(&mut inet, &term, ROOT);
  return inet;
}

// Converts an inet to a term, reading from location.
pub fn readback(net : &INet, host : Port) -> Term {
  // Given a port, returns its name, or assigns one if it wasn't named yet.
  fn name_of(net : &INet, var_port : Port, var_name : &mut HashMap<u32, Vec<u8>>) -> Vec<u8> {
    // If port is linked to an erase node, return an unused variable
    if kind(net, addr(enter(net, var_port))) == ERA {
      return b"*".to_vec();
    }
    if !var_name.contains_key(&var_port) {
      let nam = index_to_name(var_name.len() as u32 + 1);
      var_name.insert(var_port, nam.clone());
    }
    var_name.get(&var_port).unwrap().to_vec()
  }

  // Reads a term recursively by starting at root node.
  fn reader (
    net      : &INet,
    next     : Port,
    var_name : &mut HashMap<u32, Vec<u8>>,
    dups_vec : &mut Vec<u32>,
    dups_set : &mut HashSet<u32>,
    seen     : &mut HashSet<u32>
  ) -> Term {

    if seen.contains(&next) {
      return Var{nam: b"...".to_vec()};
    }

    seen.insert(next);

    match kind(net, addr(next)) {
      // If we're visiting a set...
      ERA => Era,
      // If we're visiting a con node...
      CON => match slot(next) {
        // If we're visiting a port 0, then it is a lambda.
        0 => {
          let nam = name_of(net, port(addr(next), 1), var_name);
          let prt = enter(net, port(addr(next), 2));
          let bod = reader(net, prt, var_name, dups_vec, dups_set, seen);
          let ann = enter(net, port(addr(next), 1));
          let lam = Lam { nam: nam, bod: Box::new(bod) };
          lam
        },
        // If we're visiting a port 1, then it is a variable.
        1 => {
          Var{nam: name_of(net, next, var_name)}
          //Var{nam: format!("{}@{}", String::from_utf8_lossy(&name_of(net, next, var_name)), addr(next)).into()}
        },
        // If we're visiting a port 2, then it is an application.
        _ => {
          let prt = enter(net, port(addr(next), 0));
          let fun = reader(net, prt, var_name, dups_vec, dups_set, seen);
          let prt = enter(net, port(addr(next), 1));
          let arg = reader(net, prt, var_name, dups_vec, dups_set, seen);
          App{fun: Box::new(fun), arg: Box::new(arg)}
        }
      },
      // If we're visiting a fan node...
      tag => match slot(next) {
        // If we're visiting a port 0, then it is a pair.
        0 => {
          let tag = tag - DUP;
          let prt = enter(net, port(addr(next), 1));
          let fst = reader(net, prt, var_name, dups_vec, dups_set, seen);
          let prt = enter(net, port(addr(next), 2));
          let snd = reader(net, prt, var_name, dups_vec, dups_set, seen);
          Sup{tag, fst: Box::new(fst), snd: Box::new(snd)}
        },
        // If we're visiting a port 1 or 2, then it is a variable.
        // Also, that means we found a dup, so we store it to read later.
        _ => {
          if !dups_set.contains(&addr(next)) {
            dups_set.insert(addr(next));
            dups_vec.push(addr(next));
          }
          //Var{nam: format!("{}@{}", String::from_utf8_lossy(&name_of(net, next, var_name)), addr(next)).into()}
          Var{nam: name_of(net, next, var_name)}
        }
      }
    }
  }

  // A hashmap linking ports to binder names. Those ports have names:
  // Port 1 of a con node (λ), ports 1 and 2 of a fan node (let).
  let mut binder_name = HashMap::new();

  // Dup aren't scoped. We find them when we read one of the variables
  // introduced by them. Thus, we must store the dups we find to read later.
  // We have a vec for .pop(). and a set to avoid storing duplicates.
  let mut dups_vec = Vec::new();
  let mut dups_set = HashSet::new();
  let mut seen     = HashSet::new();

  // Reads the main term from the net
  let mut main = reader(net, enter(net, host), &mut binder_name, &mut dups_vec, &mut dups_set, &mut seen);

  // Reads let founds by starting the reader function from their 0 ports.
  while dups_vec.len() > 0 {
    let dup = dups_vec.pop().unwrap();
    let val = reader(net, enter(net,port(dup,0)), &mut binder_name, &mut dups_vec, &mut dups_set, &mut seen);
    let tag = kind(net, dup) - DUP;
    let fst = name_of(net, port(dup,1), &mut binder_name);
    let snd = name_of(net, port(dup,2), &mut binder_name);
    let val = Box::new(val);
    let nxt = Box::new(main);
    main = Dup{tag, fst, snd, val, nxt};
  }
  main
}

// Converts an inet to a term, reading from ROOT location.
pub fn from_net(inet: &INet) -> Term {
  return readback(inet, ROOT);
}

// Reduction
// =========

// Normalizes a term via inet reduction.
pub fn normalize(term : &Term) -> (Term, u32) {
  let mut net = new_inet();
  inject(&mut net, &term, ROOT);
  normal(&mut net, ROOT);
  let term = readback(&net, ROOT);
  (term, net.rules)
}

// Syntax
// ======

// Term parser and stringifier. Grammar:
// <Term> ::= <Lam> | <App> | <Sup> | <Dup> | <Var> | <Era>
// <Lam>  ::= "λ" <Name> <Term>
// <App>  ::= "(" <Term> <Term> ")"
// <Sup>  ::= "{" <Term> <Term> "}" ["#" <Tag>]
// <Dup>  ::= "dup" ["#" <Tag>] <Name> <Name> "=" <Term> [";"] <Term>
// <Var>  ::= <Name>
// <Era>  ::= "*"
// <Name> ::= <alphanumeric_Name>
// <Tag>  ::= <positive_integer>

// Source code is Ascii-encoded.
pub type Str = [u8];
pub type Chr = u8;

// Parses a name, returns the remaining code and the name.
fn is_name_char(c: Chr) -> bool {
  false
  || (c >= b'A' && c <= b'Z')
  || (c >= b'a' && c <= b'z')
  || (c >= b'0' && c <= b'9')
  || (c == b'_')
  || (c == b'.')
}

// Converts an index to a name
pub fn index_to_name(idx : u32) -> Vec<Chr> {
  let mut name = Vec::new();
  let mut idx = idx;
  while idx > 0 {
    idx = idx - 1;
    name.push((97 + idx % 26) as u8);
    idx = idx / 26;
  }
  return name;
}

// Converts a name to an index
pub fn name_to_index(name : &Vec<Chr>) -> u32 {
  let mut idx : u32 = 0;
  for byte in name.iter().rev() {
    idx = (idx * 26) + (*byte as u32 - 97) + 1;
  }
  return idx;
}

// A context is a vector of (name, value) assignments.
type Context<'a> = Vec<(&'a Str, Option<Term>)>;

// Extends a context with a (name, value) assignments.
fn extend<'a,'b>(nam : &'a Str, val : Option<Term>, ctx : &'b mut Context<'a>) -> &'b mut Context<'a> {
  ctx.push((nam,val));
  ctx
}

// Removes an assignment from a context.
fn narrow<'a,'b>(ctx : &'b mut Context<'a>) -> &'b mut Context<'a> {
  ctx.pop();
  ctx
}

pub fn namespace(space : &Vec<u8>, idx : u32, var : &Vec<u8>) -> Vec<u8> {
  if var != b"*" {
    let mut nam = space.clone();
    nam.extend_from_slice(b"/");
    nam.append(&mut idx.to_string().as_bytes().to_vec());
    nam.extend_from_slice(b"/");
    nam.append(&mut var.clone());
    nam
  } else {
    var.clone()
  }
}

// Makes a namespaced copy of a term
pub fn copy(space : &Vec<u8>, idx : u32, term : &Term) -> Term {
  match term {
    Lam{nam, bod} => {
      let nam = namespace(space, idx, nam);
      let bod = Box::new(copy(space, idx, bod));
      Lam{nam, bod}
    },
    App{fun, arg} => {
      let fun = Box::new(copy(space, idx, fun));
      let arg = Box::new(copy(space, idx, arg));
      App{fun, arg}
    },
    Sup{tag, fst, snd} => {
      let tag = *tag;
      let fst = Box::new(copy(space, idx, fst));
      let snd = Box::new(copy(space, idx, snd));
      Sup{tag, fst, snd}
    },
    Dup{tag, fst, snd, val, nxt} => {
      let tag = *tag;
      let fst = namespace(space, idx, fst);
      let snd = namespace(space, idx, snd);
      let val = Box::new(copy(space, idx, val));
      let nxt = Box::new(copy(space, idx, nxt));
      Dup{tag, fst, snd, val, nxt}
    },
    Var{nam} => {
      let nam = namespace(space, idx, nam);
      Var{nam}
    },
    Era => Era
  }
}

fn parse_name(code: &Str) -> (&Str, &Str) {
  let code = skip_whitespace(code);
  let mut i: usize = 0;
  while i < code.len() && is_name_char(code[i]) {
    i += 1;
  }
  (&code[i..], &code[0..i])
}

fn skip_whitespace(code: &Str) -> &Str {
  let mut i: usize = 0;
  while i < code.len() && code[i].is_ascii_whitespace() {
    i += 1;
  }
  &code[i..]
}

fn parse_text<'a>(code: &'a Str, text: &Str) -> Result<&'a Str, String> {
  let code = skip_whitespace(code);
  if code.starts_with(text) {
    Ok(&code[text.len()..])
  } else {
    Err(format!("Expected '{}', found '{}'", String::from_utf8_lossy(text), String::from_utf8_lossy(code)))
  }
}

// Parses a term, returns the remaining code and the term.
pub fn parse_term<'a>(code: &'a Str, ctx: &mut Context<'a>, idx: &mut u32) -> (&'a Str, Term) {
  let code = skip_whitespace(code);
  match code[0] {
    // Comment: `// many words here ... <newline>`
    b'/' if code[1] == b'/' => {
      let end = code.iter().position(|&c| c == b'\n').unwrap_or(code.len());
      parse_term(&code[end..], ctx, idx)
    }
    // Definition: `def nam = val; bod` (note: ';' is optional)
    b'd' if code.starts_with(b"def ") => {
      let (code, nam) = parse_name(&code[4..]);
      let  code       = parse_text(code, b"=").unwrap();
      let (code, val) = parse_term(code, ctx, idx);
      let  code       = if code[0] == b';' { &code[1..] } else { code };
      extend(nam, Some(val), ctx);
      let (code, bod) = parse_term(code, ctx, idx);
      narrow(ctx);
      (code, bod)
    }
    // Untyped Abstraction: `λvar body`
    b'\xce' if code[1] == b'\xbb' => {
      let (code, nam) = parse_name(&code[2..]);
      extend(nam, None, ctx);
      let (code, bod) = parse_term(code, ctx, idx);
      narrow(ctx);
      let nam = nam.to_vec();
      let bod = Box::new(bod);
      (code, Lam { nam, bod })
    }
    // Application: `(func argm1 argm2 ... argmN)`
    b'(' => {
      let (mut code, mut fun) = parse_term(&code[1..], ctx, idx);
      while code[0] != b')' {
        let (new_code, arg) = parse_term(code, ctx, idx);
        code = skip_whitespace(new_code);
        let arg = Box::new(arg);
        fun = App { fun: Box::new(fun), arg };
      }
      let code = parse_text(code, b")").unwrap();
      (code, fun)
    }
    // Pair: `{val0 val1}#tag` (note: '#tag' is optional)
    b'{' => {
      let (code, fst) = parse_term(&code[1..], ctx, idx);
      let (code, snd) = parse_term(code, ctx, idx);
      let  code       = parse_text(code, b"}").unwrap();
      let (code, tag) = if code[0] == b'#' { parse_name(&code[1..]) } else { (code, &b""[..]) };
      let tag = name_to_index(&tag.to_vec());
      let fst = Box::new(fst);
      let snd = Box::new(snd);
      (code, Sup { tag, fst, snd })
    }
    // Dup: `dup #tag fst snd = val; bod` (note: '#tag' and ';' are optional)
    b'd' if code.starts_with(b"dup ") => {
      let  code       = &code[4..];
      let (code, tag) = if code[0] == b'#' { parse_name(&code[1..]) } else { (code, &b""[..]) };
      let (code, fst) = parse_name(code);
      let (code, snd) = parse_name(code);
      let  code       = parse_text(code, b"=").unwrap();
      extend(snd, None, ctx);
      extend(fst, None, ctx);
      let (code, val) = parse_term(code, ctx, idx);
      let  code       = if code[0] == b';' { &code[1..] } else { code };
      let (code, nxt) = parse_term(code, ctx, idx);
      narrow(ctx);
      narrow(ctx);
      let tag = name_to_index(&tag.to_vec());
      let fst = fst.to_vec();
      let snd = snd.to_vec();
      let val = Box::new(val);
      let nxt = Box::new(nxt);
      (code, Dup { tag, fst, snd, val, nxt })
    }
    // Era: `*`
    b'*' => {
      (&code[1..], Era)
    },
    // Variable: `<alphanumeric_name>`
    _ => {
      let (code, nam) = parse_name(code);
      let mut val: Option<Term> = None;
      for i in (0..ctx.len()).rev() {
        if ctx[i].0 == nam {
          match ctx[i].1 {
            Some(ref term) => {
              let mut name = nam.clone().to_vec();
              val = Some(copy(&name, *idx, term));
              *idx += 1;
              break;
            }
            None => {
              break;
            }
          }
        }
      }
      let nam = nam.to_vec();
      (code, match val { Some(term) => term, None => Var { nam } })
    }
  }
}

// Converts a source-code to a λ-term.
pub fn from_string<'a>(code : &'a Str) -> Term {
  let mut ctx = Vec::new();
  let mut idx = 0;
  parse_term(code, &mut ctx, &mut idx).1
}

// Converts a λ-term back to a source-code.
pub fn to_string(term : &Term) -> Vec<Chr> {
  fn stringify_term(code : &mut Vec<u8>, term : &Term) {
    match term {
      &Lam{ref nam, ref bod} => {
        code.extend_from_slice("λ".as_bytes());
        code.append(&mut nam.clone());
        code.extend_from_slice(b" ");
        stringify_term(code, &bod);
      },
      &App{ref fun, ref arg} => {
        code.extend_from_slice(b"(");
        stringify_term(code, &fun);
        code.extend_from_slice(b" ");
        stringify_term(code, &arg);
        code.extend_from_slice(b")");
      },
      &Sup{tag, ref fst, ref snd} => {
        code.extend_from_slice(b"[");
        stringify_term(code, &fst);
        code.extend_from_slice(b" ");
        stringify_term(code, &snd);
        if tag != 0 {
          code.extend_from_slice(b"#");
          code.append(&mut index_to_name(tag));
        }
        code.extend_from_slice(b"]");
      },
      &Dup{tag, ref fst, ref snd, ref val, ref nxt} => {
        code.extend_from_slice(b"dup ");
        if tag != 0 {
          code.extend_from_slice(b"#");
          code.append(&mut index_to_name(tag));
          code.extend_from_slice(b" ");
        }
        code.append(&mut fst.clone());
        code.extend_from_slice(b" ");
        code.append(&mut snd.clone());
        code.extend_from_slice(b" = ");
        stringify_term(code, &val);
        code.extend_from_slice(b"; ");
        stringify_term(code, &nxt);
      },
      &Era => {
        code.extend_from_slice(b"*");
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

// Extra: Full λ-Terms
// ===================

// Converts a *full* lambda term to a net.
pub fn lambda_term_to_inet(term : &Term) -> INet {
  fn encode(inet : &mut INet, label : &mut u32, scope : &mut Vec<(Vec<u8>, u32)>, term : &Term) -> Port {
    match term {
      &App{ref fun, ref arg} => {
        let app = new_node(inet, CON);
        let fun = encode(inet, label, scope, fun);
        link(inet, port(app, 0), fun);
        let arg = encode(inet, label, scope, arg);
        link(inet, port(app, 1), arg);
        port(app, 2)
      },
      &Lam{ref nam, ref bod} => {
        let fun = new_node(inet, CON);
        let era = new_node(inet, ERA);
        link(inet, port(fun, 1), port(era, 0));
        link(inet, port(era, 1), port(era, 2));
        scope.push((nam.to_vec(), fun));
        let bod = encode(inet, label, scope, bod);
        scope.pop();
        link(inet, port(fun, 2), bod);
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
        let arg = enter(inet, port(lam, 1));
        if kind(inet, addr(arg)) == 0 {
          inet.reuse.push(addr(arg));
          port(lam, 1)
        } else {
          *label += 1;
          let dup = new_node(inet, *label);
          link(inet, port(dup, 2), arg);
          link(inet, port(dup, 0), port(lam, 1));
          port(dup, 1)
        }
      },
      _ => panic!("Invalid λ-term.")
    }
  }
  let mut inet : INet = new_inet();
  let mut label : u32 = 1;
  let mut scope : Vec<(Vec<u8>, u32)> = Vec::new();
  let ptr : Port = encode(&mut inet, &mut label, &mut scope, term);
  link(&mut inet, 0, ptr);
  inet
}

// Converts a net to a *full* lambda term.
pub fn lambda_term_from_inet(inet : &INet) -> Term {
  fn go(inet : &INet, node_depth : &mut Vec<u32>, next : Port, exit : &mut Vec<Port>, depth : u32) -> Term {
    let prev_port = enter(inet, next);
    let prev_slot = slot(prev_port);
    let prev_node = addr(prev_port);
    if kind(inet, prev_node) == CON {
      match prev_slot {
        0 => {
          node_depth[prev_node as usize] = depth;
          let nam = index_to_name(depth + 1);
          let bod = Box::new(go(inet, node_depth, port(prev_node, 2), exit, depth + 1));
          Lam {nam, bod}
        },
        1 => {
          let nam = index_to_name(node_depth[prev_node as usize] + 1);
          Var {nam}
        },
        _ => {
          let fun = Box::new(go(inet, node_depth, port(prev_node, 0), exit, depth));
          let arg = Box::new(go(inet, node_depth, port(prev_node, 1), exit, depth));
          App {fun, arg}
        }
      }
    } else if kind(inet, prev_node) >= DUP {
      if prev_slot > 0 {
        exit.push(prev_slot);
        let term = go(inet, node_depth, port(prev_node, 0), exit, depth);
        exit.pop();
        term
      } else {
        let e = exit.pop().unwrap();
        let term = go(inet, node_depth, port(prev_node, e), exit, depth);
        exit.push(e);
        term
      }
    } else {
      Era
    }
  }
  let mut node_depth : Vec<u32> = Vec::with_capacity(inet.nodes.len() / 4);
  let mut exit : Vec<u32> = Vec::new();
  node_depth.resize(inet.nodes.len() / 4, 0);
  go(inet, &mut node_depth, ROOT, &mut exit, 0)
}
