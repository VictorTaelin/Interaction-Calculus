// Converts Term to Nets, and back.

use super::*;

// Converts a term to an Interaction Combinator net. Both systems are directly isomorphic, so,
// each node of the Interaction Calculus correspond to a single Interaction Combinator node.
pub fn alloc_at(inet: &mut INet, term: &Term, host: Port) {
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
      //&Lam{ref nam, ref typ, ref bod} => {
        //// TODO: handle typ
        //let fun = new_node(net, CON);
        //scope.insert(nam.to_vec(), port(fun, 1));
        //// Also, if the variable is unused, crease an erase node.
        //if nam == b"*" {
          //let era = new_node(net, ERA);
          //link(net, port(era, 1), port(era, 2));
          //link(net, port(fun, 1), port(era, 0));
        //}
        //let bod = encode_term(net, bod, port(fun, 2), scope, vars);
        //link(net, port(fun, 2), bod);
        //port(fun, 0)
      //},
      &Lam { ref nam, ref typ, ref bod } => {
        let fun = new_node(net, CON);
        if let Some(ref typ) = typ {
          let ann = new_node(net, ANN);
          let typ = encode_term(net, typ, port(ann, 0), scope, vars);
          link(net, port(ann, 0), typ);
          link(net, port(fun, 1), port(ann, 1));
          scope.insert(nam.to_vec(), port(ann, 2));
        } else {
          scope.insert(nam.to_vec(), port(fun, 1));
        }
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
      // An annotation becomes an ANN node. Ports:
      // - 0: points to the type of the annotation.
      // - 1: points to where the annotation occurs.
      // - 2: points to the value being annotated.
      &Ann{ref val, ref typ} => {
        let ann = new_node(net, ANN);
        let val = encode_term(net, val, port(ann, 2), scope, vars);
        link(net, port(ann, 2), val);
        let typ = encode_term(net, typ, port(ann, 0), scope, vars);
        link(net, port(ann, 0), typ);
        port(ann, 1)
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

// Converts an Interaction-INet node to an Interaction Calculus term.
pub fn read_at(net : &INet, host : Port) -> Term {
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
  fn reader
    ( net      : &INet
    , next     : Port
    , var_name : &mut HashMap<u32, Vec<u8>>
    , dups_vec : &mut Vec<u32>
    , dups_set : &mut HashSet<u32>
    , seen     : &mut HashSet<u32>
    ) -> Term {

    if seen.contains(&next) {
      return Var{nam: b"...".to_vec()};
    }

    seen.insert(next);

    match kind(net, addr(next)) {
      // If we're visiting a set...
      ERA => Set,
      // If we're visiting a con node...
      CON => match slot(next) {
        // If we're visiting a port 0, then it is a lambda.
        0 => {
          let nam = name_of(net, port(addr(next), 1), var_name);
          let prt = enter(net, port(addr(next), 2));
          let bod = reader(net, prt, var_name, dups_vec, dups_set, seen);
          let ann = enter(net, port(addr(next), 1));
          let typ = if kind(net, addr(ann)) == ANN && slot(ann) == 1 {
            let ann_addr = addr(ann);
            let typ_port = enter(net, port(ann_addr, 0));
            Some(Box::new(reader(net, typ_port, var_name, dups_vec, dups_set, seen)))
          } else {
            None
          };
          let mut lam = Lam {
            nam: nam,
            typ: typ,
            bod: Box::new(bod),
          };
          lam
        },
        // If we're visiting a port 1, then it is a variable.
        1 => {
          //Var{nam: name_of(net, next, var_name)}
          Var{nam: format!("{}@{}", String::from_utf8_lossy(&name_of(net, next, var_name)), addr(next)).into()}
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
      // If we're visiting an ANN node...
      ANN => match slot(next) {
        // If we're visiting a port 0, then it is an annotation...
        0 => {
          todo!();
        },
        // If we're visiting a port 1, then it is where the annotation occurs.
        1 => {
          let prt = enter(net, port(addr(next), 2));
          let val = reader(net, prt, var_name, dups_vec, dups_set, seen);
          let prt = enter(net, port(addr(next), 0));
          let typ = reader(net, prt, var_name, dups_vec, dups_set, seen);
          Ann{val: Box::new(val), typ: Box::new(typ)}
        },
        // If we're visiting a port 2, then it is the value being annotated.
        _ => {
          let prt = enter(net, port(addr(next), 1));
          let val = reader(net, prt, var_name, dups_vec, dups_set, seen);
          val
          //let prt = enter(net, port(addr(next), 0));
          //let typ = reader(net, prt, var_name, dups_vec, dups_set, seen);
          //Ann{val: Box::new(val), typ: Box::new(typ)}
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
          Var{nam: format!("{}@{}", String::from_utf8_lossy(&name_of(net, next, var_name)), addr(next)).into()}
          //let nam = name_of(net, next, var_name);
          //Var{nam}
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

pub fn to_net(term: &Term) -> INet {
  let mut inet = new_inet();
  alloc_at(&mut inet, &term, ROOT);
  return inet;
}

pub fn from_net(inet: &INet) -> Term {
  return read_at(inet, ROOT);
}

  

