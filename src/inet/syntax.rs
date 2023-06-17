// INode parser and stringifier. Grammar:
// <Node> ::= "[" <name> "|" <name> <name> <name> "]"
// <INodes> ::= <Node> <INodes> | <eof>

use super::*;

// FIXME: some definitions here are replicated on term/, remove duplication

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

// Parses a node, returns the remaining code and the node.
fn parse_node<'a>(code: &'a Str) -> Result<(&'a Str, INode), String> {
  let code = skip_whitespace(code);
  let code = parse_text(code, b"[")?;

  let (code, port1) = parse_name(code);
  let port1 = String::from_utf8_lossy(port1).to_string();

  let (code, port2) = parse_name(code);
  let port2 = String::from_utf8_lossy(port2).to_string();

  let (code, port3) = parse_name(code);
  let port3 = String::from_utf8_lossy(port3).to_string();

  let code = parse_text(code, b"]")?;

  let (code, label) = parse_name(code);
  let label = String::from_utf8_lossy(label).parse::<u32>().unwrap_or(1);

  Ok((code, INode {
    label,
    ports: [port1, port2, port3],
  }))
}

// Parses an INodes, returns the remaining code and the INodes.
pub fn parse_inodes<'a>(code: &'a Str) -> Result<(&'a Str, INodes), String> {
  let mut inodes = Vec::new();
  let mut code = code;

  while !code.is_empty() {
    match parse_node(code) {
      Ok((new_code, node)) => {
        code = new_code;
        inodes.push(node);
      }
      Err(_) => break,
    }
  }

  Ok((code, inodes))
}

// Converts a source-code to an INodes.
pub fn from_string_inodes<'a>(code: &'a Str) -> Result<INodes, String> {
  parse_inodes(code).map(|(_, inodes)| inodes)
}

// Converts an INodes back to a source-code.
pub fn to_string_inodes(inodes: &INodes) -> Vec<Chr> {
  let mut code = Vec::new();

  for node in inodes {
    code.extend_from_slice(b"[");
    code.extend_from_slice(node.ports[0].as_bytes());
    code.extend_from_slice(b" ");
    code.extend_from_slice(node.ports[1].as_bytes());
    code.extend_from_slice(b" ");
    code.extend_from_slice(node.ports[2].as_bytes());
    code.extend_from_slice(b"]");
    code.extend_from_slice(node.label.to_string().as_bytes());
    code.extend_from_slice(b"\n");
  }

  code
}

pub fn show_inodes(inodes: &INodes) -> String {
  return String::from_utf8_lossy(&to_string_inodes(inodes)).to_string();
}

// Converts an INet to INodes by creating names for connected ports.
pub fn inet_to_inodes(inet: &INet) -> INodes {
  let mut inodes = vec![];
  let mut names = vec![String::new(); inet.nodes.len()];
  let mut name_counter = 0;

  // Generates a new name for a port.
  fn new_name(name_counter: &mut usize) -> String {
    if *name_counter == 0 {
      *name_counter += 1;
      return "a".to_string();
    } else {
      let mut name = String::new();
      let mut n = *name_counter;
      while n > 0 {
        let c = (n % 26) as u8 + b'a';
        name.push(c as char);
        n /= 26;
      }
      *name_counter += 1;
      name
    }
  }

  for i in 1..(inet.nodes.len() / 4) {
    let node = i as u32;
    let k = kind(inet, node);
    if k != 0 {
      let mut ports = [String::new(), String::new(), String::new()];
      for j in 0..3 {
        let p = port(node, j);
        if enter(inet, p) == ROOT { 
          ports[j as usize] = "_".to_string();
        } else {
          if names[p as usize].is_empty() {
            let name = new_name(&mut name_counter);
            names[p as usize] = name.clone();
            let q = enter(inet, p);
            names[q as usize] = name;
          }
          ports[j as usize] = names[p as usize].clone();
        }
      }
      inodes.push(INode {
        label: k,
        ports,
      });
    }
  }

  inodes
}

// Converts INodes to an INet by linking ports based on names.
pub fn inodes_to_inet(inodes: &INodes) -> INet {
  let mut inet = new_inet();
  let mut name_map = std::collections::HashMap::new();

  for (i, inode) in inodes.iter().enumerate() {
    let node = new_node(&mut inet, inode.label);
    for (j, name) in inode.ports.iter().enumerate() {
      let p = port(node, j as u32);
      if name == "_" {
        link(&mut inet, p, ROOT);
      } else {
        if let Some(&q) = name_map.get(name) {
          link(&mut inet, p, q);
          name_map.remove(name);
        } else {
          name_map.insert(name.clone(), p);
        }
      }
    }
  }

  inet
}
