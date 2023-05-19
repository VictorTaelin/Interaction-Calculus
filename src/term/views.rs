use super::*;

// Converts a lambda term (with non-affine functions) to a net. Reduction of the resulting net is
// *not* guaranteed to return the normal form of the original lambda term.
#[allow(dead_code)]
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
      &Lam{ref nam, ref typ, ref bod} => {
        // TODO: handle typ
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
      &Ann{..} => {
        todo!();
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

// Converts a net to a lambda term, turning explicit "lets" into non-affine functions. Will fail if
// the corresponding abstract-calculus term includes variables that scape a lambda's scope.
#[allow(dead_code)]
pub fn lambda_term_from_inet(inet : &INet) -> Term {
  fn go(inet : &INet, node_depth : &mut Vec<u32>, next : Port, exit : &mut Vec<Port>, depth : u32) -> Term {
    let prev_port = enter(inet, next);
    let prev_slot = slot(prev_port);
    let prev_node = addr(prev_port);
    if kind(inet, prev_node) == 1 {
      match prev_slot {
        0 => {
          node_depth[prev_node as usize] = depth;
          let nam = index_to_name(depth + 1);
          let typ = None; // TODO: handle
          let bod = Box::new(go(inet, node_depth, port(prev_node, 2), exit, depth + 1));
          Lam {nam, typ, bod}
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
    } else if prev_slot > 0 {
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
  }
  let mut node_depth : Vec<u32> = Vec::with_capacity(inet.nodes.len() / 4);
  let mut exit : Vec<u32> = Vec::new();
  node_depth.resize(inet.nodes.len() / 4, 0);
  go(inet, &mut node_depth, 0, &mut exit, 0)
}

// Converts a binary input such as b"1001" into a λ-encoded bitstring
// such as λa.λb.λc.(a λa.λb.λc.(b λa.λb.λc.(b λa.λb.λc.(a λa.λb.λc.c))))
pub fn bitstring_to_term(s : &[u8], i : u32) -> Term {
  match if s.len() > 0 { s[0] } else { b' ' } {
    b'0' => {
      let nam = index_to_name(i+1);
      let app = Term::App{
        fun: Box::new(Var{nam: nam.clone()}),
        arg: Box::new(bitstring_to_term(&s[1..], i+1))
      };
      let e_lam = Term::Lam{
        nam: b"-".to_vec(),
        typ: None, // TODO
        bod: Box::new(app)
      };
      let i_lam = Term::Lam{
        nam: b"-".to_vec(),
        typ: None, // TODO
        bod: Box::new(e_lam)
      };
      let o_lam = Term::Lam{
        nam: nam,
        typ: None, // TODO
        bod: Box::new(i_lam)
      };
      o_lam
    },
    b'1' => {
      let nam = index_to_name(i+1);
      let app = Term::App{
        fun: Box::new(Var{nam: nam.clone()}),
        arg: Box::new(bitstring_to_term(&s[1..], i+1))
      };
      let e_lam = Term::Lam{
        nam: b"-".to_vec(),
        typ: None, // TODO
        bod: Box::new(app)
      };
      let i_lam = Term::Lam{
        nam: nam,
        typ: None, // TODO
        bod: Box::new(e_lam)
      };
      let o_lam = Term::Lam{
        nam: b"-".to_vec(),
        typ: None, // TODO
        bod: Box::new(i_lam)
      };
      o_lam
    },
    _ => {
      let nam = index_to_name(i+1);
      let var = Var{nam: nam.clone()};
      let e_lam = Term::Lam{
        nam: nam,
        typ: None, // TODO
        bod: Box::new(var)
      };
      let i_lam = Term::Lam{
        nam: b"-".to_vec(),
        typ: None, // TODO
        bod: Box::new(e_lam)
      };
      let o_lam = Term::Lam{
        nam: b"-".to_vec(),
        typ: None, // TODO
        bod: Box::new(i_lam)
      };
      o_lam
    }
  }
}

// Can this highly-idented style be improved?
pub fn term_to_bitstring(t : &Term) -> Vec<u8> {
  fn format_binary_output(t : &Term, v : &mut Vec<u8>) {
    match t {
      Term::Lam{nam: ref o_nam, typ: _, bod: ref o_bod} => { // TODO: handle typ
        match **o_bod {
          Term::Lam{nam: ref i_nam, typ: _, bod: ref i_bod} => { // TODO: handle typ
            match **i_bod {
              Term::Lam{nam: _, typ: _, bod: ref e_bod} => { // TODO: handle typ
                match **e_bod {
                  Term::App{fun: ref app_fun, arg: ref app_arg} => {
                    match **app_fun {
                      Term::Var{nam: ref var_nam} => {
                        if var_nam == o_nam {
                          v.extend_from_slice(b"0");
                          format_binary_output(app_arg, v);
                        } else if var_nam == i_nam {
                          v.extend_from_slice(b"1");
                          format_binary_output(app_arg, v);
                        }
                      },
                      _ => {}
                    }
                  },
                  _ => {}
                }
              },
              _ => {}
            }
          },
          _ => {}
        }
      },
      _ => {}
    }
  }
  let mut v : Vec<u8> = Vec::new();
  format_binary_output(t, &mut v);
  v
}

// Converts a bitstring (up to 8 bits) to a character.
pub fn bits_to_char(s : &[u8]) -> u8 {
  let mut c = 0;
  for i in 0..8 {
    c = c * 2 + (if s[i] == b'0' { 0 } else { 1 });
  }
  c
}

// Converts a character to a bitstring.
pub fn char_to_bits(c : u8) -> Vec<u8> {
  let mut v : Vec<u8> = Vec::new();
  let mut c = c;
  for _i in 0..8 {
    v.extend_from_slice(if c % 2 == 0 { b"0" } else { b"1" });
    c = c / 2;
  }
  v.reverse();
  v
}

// Converts a bitstring to an ascii string.
pub fn bits_to_ascii(s : &[u8]) -> Vec<u8> {
  let mut v : Vec<u8> = Vec::new();
  for i in 0..s.len()/8 {
    v.push(bits_to_char(&s[i*8..i*8+8]));
  }
  v
}

// Converts an ascii string to a bitstring.
pub fn ascii_to_bits(a : &[u8]) -> Vec<u8> {
  let mut v : Vec<u8> = Vec::new();
  for i in 0..a.len() {
    v.append(&mut char_to_bits(a[i]))
  }
  v
}
