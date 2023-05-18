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
  Set
}

use self::Term::{*};

// Source code is Ascii-encoded.
pub type Str = [u8];
pub type Chr = u8;

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
    Set => Set
  }
}

// Parses a name, returns the remaining code and the name.
fn is_name_char(c: Chr) -> bool {
  false
  || (c >= b'A' && c <= b'Z')
  || (c >= b'a' && c <= b'z')
  || (c >= b'0' && c <= b'9')
  || (c == b'_')
  || (c == b'.')
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
  while i < code.len() && (code[i] == b' ' || code[i] == b'\n') {
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
    // Abstraction: `λvar body`
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
        code = new_code;
        let arg = Box::new(arg);
        fun = App { fun: Box::new(fun), arg };
      }
      let code = parse_text(code, b")").unwrap();
      (code, fun)
    }
    // Pair: `[val0 val1]#tag` (note: '#tag' is optional)
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
    // Set: `*`
    b'*' => (&code[1..], Set),
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
        match &**fun {
          &App{..} => {
            stringify_term(code, &fun);
            code.extend_from_slice(b" ");
            stringify_term(code, &arg);
          },
          _ => {
            code.extend_from_slice(b"(");
            stringify_term(code, &fun);
            code.extend_from_slice(b" ");
            stringify_term(code, &arg);
            code.extend_from_slice(b")");
          }
        }
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
      &Set => {
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

// Converts a term to an Interaction Combinator net. Both systems are directly isomorphic, so,
// each node of the Interaction Calculus correspond to a single Interaction Combinator node.
pub fn to_net(term : &Term) -> INet {
  fn encode_term
    ( net   : &mut INet
    , term  : &Term
    , up    : Port
    , scope : &mut HashMap<Vec<u8>,u32>
    , vars  : &mut Vec<(Vec<u8>,u32)>
    ) -> Port {
    match term {
      // A lambda becomes to a con node. Ports:
      // - 0: points to where the lambda occurs.
      // - 1: points to the lambda variable.
      // - 2: points to the lambda body.
      &Lam{ref nam, ref bod} => {
        let fun = new_node(net, CON);
        scope.insert(nam.to_vec(), port(fun, 1));
        // Also, if the variable is unused, crease an erase node.
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
        let dup = new_node(net, FAN + tag);
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
        let dup = new_node(net, FAN + tag);
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
      &Set => {
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

  // Initializes net with a root node.
  let mut net = INet { nodes: vec![0,2,1,4], reuse: vec![] };
  let mut vars = Vec::new();
  let mut scope = HashMap::new();

  // Encodes the main term.
  let main = encode_term(&mut net, &term, 0, &mut scope, &mut vars);

  // Links bound variables.
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

  // Connects unbound variables to erase nodes
  for (_, addr) in scope {
    if enter(&net, addr) == addr {
      let era = new_node(&mut net, ERA);
      link(&mut net, port(era, 1), port(era, 2));
      link(&mut net, addr, port(era, 0));
    }
  }

  // Links the term to the net's root.
  link(&mut net, 0, main);

  net
}

// Converts an Interaction-INet node to an Interaction Calculus term.
pub fn from_net(net : &INet) -> Term {
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
  fn read_term
    ( net   : &INet
    , next   : Port
    , var_name : &mut HashMap<u32, Vec<u8>>
    , dups_vec : &mut Vec<u32>
    , dups_set : &mut HashSet<(u32)>
    ) -> Term {
    match kind(net, addr(next)) {
      // If we're visiting a set...
      ERA => Set,
      // If we're visiting a con node...
      CON => match slot(next) {
        // If we're visiting a port 0, then it is a lambda.
        0 => {
          let nam = name_of(net, port(addr(next),1), var_name);
          let prt = enter(net, port(addr(next), 2));
          let bod = read_term(net, prt, var_name, dups_vec, dups_set);
          let mut lam = Lam{nam: nam, bod: Box::new(bod)};
          lam
        },
        // If we're visiting a port 1, then it is a variable.
        1 => {
          Var{nam: name_of(net, next, var_name)}
        },
        // If we're visiting a port 2, then it is an application.
        _ => {
          let prt = enter(net, port(addr(next), 0));
          let fun = read_term(net, prt, var_name, dups_vec, dups_set);
          let prt = enter(net, port(addr(next), 1));
          let arg = read_term(net, prt, var_name, dups_vec, dups_set);
          App{fun: Box::new(fun), arg: Box::new(arg)}
        }
      },
      // If we're visiting a fan node...
      tag => match slot(next) {
        // If we're visiting a port 0, then it is a pair.
        0 => {
          let tag = tag - FAN;
          let prt = enter(net, port(addr(next), 1));
          let fst = read_term(net, prt, var_name, dups_vec, dups_set);
          let prt = enter(net, port(addr(next), 2));
          let snd = read_term(net, prt, var_name, dups_vec, dups_set);
          Sup{tag, fst: Box::new(fst), snd: Box::new(snd)}
        },
        // If we're visiting a port 1 or 2, then it is a variable.
        // Also, that means we found a dup, so we store it to read later.
        _ => {
          if !dups_set.contains(&addr(next)) {
            dups_set.insert(addr(next));
            dups_vec.push(addr(next));
          }
          let nam = name_of(net, next, var_name);
          Var{nam}
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

  // Reads the main term from the net
  let mut main = read_term(net, enter(net, 0), &mut binder_name, &mut dups_vec, &mut dups_set);

  // Reads let founds by starting the read_term function from their 0 ports.
  while dups_vec.len() > 0 {
    let dup = dups_vec.pop().unwrap();
    let val = read_term(net, enter(net,port(dup,0)), &mut binder_name, &mut dups_vec, &mut dups_set);
    let tag = kind(net, dup) - FAN;
    let fst = name_of(net, port(dup,1), &mut binder_name);
    let snd = name_of(net, port(dup,2), &mut binder_name);
    let val = Box::new(val);
    let nxt = Box::new(main);
    main = Dup{tag, fst, snd, val, nxt};
  }
  main
}

// Reduces an Interaction Calculus term through Interaction Combinators.
pub fn reduce(term : &Term) -> Term {
  let mut net : INet = to_net(&term);
  ::inet::reduce(&mut net);
  from_net(&net)
}

pub fn reduce_with_stats(term : &Term) -> (Term, Stats) {
  let mut net : INet = to_net(&term);
  let stats = ::inet::reduce(&mut net);
  (from_net(&net), stats)
}
