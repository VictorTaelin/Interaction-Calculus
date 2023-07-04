// Converts Term to Nets, and back.

use super::*;

// Converts a term to an Interaction Combinator net. Both systems are directly isomorphic, so,
// each node of the Interaction Calculus correspond to a single Interaction Combinator node.
pub fn to_net(term: &Term) -> INet {
    fn encode_term(
        net: &mut INet,
        term: &Term,
        up: Port,
        scope: &mut HashMap<Vec<u8>, u32>,
        vars: &mut Vec<(Vec<u8>, u32)>,
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
            }
            // An application becomes to a con node too. Ports:
            // - 0: points to the function being applied.
            // - 1: points to the function's argument.
            // - 2: points to where the application occurs.
            &App { ref fun, ref arg } => {
                let app = new_node(net, CON);
                let fun = encode_term(net, fun, port(app, 0), scope, vars);
                link(net, port(app, 0), fun);
                let arg = encode_term(net, arg, port(app, 1), scope, vars);
                link(net, port(app, 1), arg);
                port(app, 2)
            }
            // A pair becomes a dup node. Ports:
            // - 0: points to where the pair occurs.
            // - 1: points to the first value.
            // - 2: points to the second value.
            &Sup {
                tag,
                ref fst,
                ref snd,
            } => {
                let dup = new_node(net, DUP + tag);
                let fst = encode_term(net, fst, port(dup, 1), scope, vars);
                link(net, port(dup, 1), fst);
                let snd = encode_term(net, snd, port(dup, 2), scope, vars);
                link(net, port(dup, 2), snd);
                port(dup, 0)
            }
            // A dup becomes a dup node too. Ports:
            // - 0: points to the value projected.
            // - 1: points to the occurrence of the first variable.
            // - 2: points to the occurrence of the second variable.
            &Dup {
                tag,
                ref fst,
                ref snd,
                ref val,
                ref nxt,
            } => {
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
            }
            // A set is just an erase node stored in a place.
            &Set => {
                let set = new_node(net, ERA);
                link(net, port(set, 1), port(set, 2));
                port(set, 0)
            }
            Var { ref nam } => {
                vars.push((nam.to_vec(), up));
                up
            }
        }
    }

    let mut net = new_inet();
    let mut vars = vec![];
    let mut scope = HashMap::new();

    // Encodes the main term.
    let main = encode_term(&mut net, &term, ROOT, &mut scope, &mut vars);

    // Links bound variables.
    for i in 0..vars.len() {
        let (ref nam, var) = vars[i];
        match scope.get(nam) {
            Some(next) => {
                let next = *next;
                if enter(&net, next) == next {
                    link(&mut net, var, next);
                } else {
                    panic!(
                        "Variable used more than once: {}.",
                        std::str::from_utf8(nam).unwrap()
                    );
                }
            }
            None => panic!("Unbound variable: {}.", std::str::from_utf8(nam).unwrap()),
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
    link(&mut net, 1, main);

    net
}

// Converts an Interaction-INet node to an Interaction Calculus term.
pub fn from_net(net: &INet) -> Term {
    // Given a port, returns its name, or assigns one if it wasn't named yet.
    fn name_of(net: &INet, var_port: Port, var_name: &mut HashMap<u32, Vec<u8>>) -> Vec<u8> {
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
    fn read_term(
        net: &INet,
        next: Port,
        var_name: &mut HashMap<u32, Vec<u8>>,
        dups_vec: &mut Vec<u32>,
        dups_set: &mut HashSet<(u32)>,
    ) -> Term {
        match kind(net, addr(next)) {
            // If we're visiting a set...
            ERA => Set,
            // If we're visiting a con node...
            CON => match slot(next) {
                // If we're visiting a port 0, then it is a lambda.
                0 => {
                    let nam = name_of(net, port(addr(next), 1), var_name);
                    let prt = enter(net, port(addr(next), 2));
                    let bod = read_term(net, prt, var_name, dups_vec, dups_set);
                    let ann = enter(net, port(addr(next), 1));
                    Lam {
                        nam,
                        bod: Box::new(bod),
                    }
                }
                // If we're visiting a port 1, then it is a variable.
                1 => Var {
                    nam: name_of(net, next, var_name),
                },
                // If we're visiting a port 2, then it is an application.
                _ => {
                    let prt = enter(net, port(addr(next), 0));
                    let fun = read_term(net, prt, var_name, dups_vec, dups_set);
                    let prt = enter(net, port(addr(next), 1));
                    let arg = read_term(net, prt, var_name, dups_vec, dups_set);
                    App {
                        fun: Box::new(fun),
                        arg: Box::new(arg),
                    }
                }
            },
            // If we're visiting a fan node...
            tag => match slot(next) {
                // If we're visiting a port 0, then it is a pair.
                0 => {
                    let tag = tag - DUP;
                    let prt = enter(net, port(addr(next), 1));
                    let fst = read_term(net, prt, var_name, dups_vec, dups_set);
                    let prt = enter(net, port(addr(next), 2));
                    let snd = read_term(net, prt, var_name, dups_vec, dups_set);
                    Sup {
                        tag,
                        fst: Box::new(fst),
                        snd: Box::new(snd),
                    }
                }
                // If we're visiting a port 1 or 2, then it is a variable.
                // Also, that means we found a dup, so we store it to read later.
                _ => {
                    if !dups_set.contains(&addr(next)) {
                        dups_set.insert(addr(next));
                        dups_vec.push(addr(next));
                    }
                    let nam = name_of(net, next, var_name);
                    Var { nam }
                }
            },
        }
    }

    // A hashmap linking ports to binder names. Those ports have names:
    // Port 1 of a con node (λ), ports 1 and 2 of a fan node (let).
    let mut binder_name = HashMap::new();

    // Dup aren't scoped. We find them when we read one of the variables
    // introduced by them. Thus, we must store the dups we find to read later.
    // We have a vec for .pop(). and a set to avoid storing duplicates.
    let mut dups_vec = vec![];
    let mut dups_set = HashSet::new();

    // Reads the main term from the net
    let mut main = read_term(
        net,
        enter(net, ROOT),
        &mut binder_name,
        &mut dups_vec,
        &mut dups_set,
    );

    // Reads let founds by starting the read_term function from their 0 ports.
    while let Some(dup) = dups_vec.pop() {
        let val = read_term(
            net,
            enter(net, port(dup, 0)),
            &mut binder_name,
            &mut dups_vec,
            &mut dups_set,
        );
        let tag = kind(net, dup) - DUP;
        let fst = name_of(net, port(dup, 1), &mut binder_name);
        let snd = name_of(net, port(dup, 2), &mut binder_name);
        let val = Box::new(val);
        let nxt = Box::new(main);
        main = Dup {
            tag,
            fst,
            snd,
            val,
            nxt,
        };
    }
    main
}
