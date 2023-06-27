// Term parser and stringifier. Grammar:
// <Term> ::= <Lam> | <App> | <Sup> | <Dup> | <Var> | <Set>
// <Lam>  ::= "λ" <name> <Term>
// <App>  ::= "(" <Term> <Term> ")"
// <Ann>  ::= "<" <Term> ":" <Term> ")"
// <Sup>  ::= "{" <Term> <Term> "}" ["#" <tag>]
// <Dup>  ::= "dup" ["#" <tag>] <name> <name> "=" <Term> [";"] <Term>
// <Fix>  ::= "@" <name> <Term>
// <Var>  ::= <name>
// <Set>  ::= "*"
// <name> ::= <alphanumeric_name>
// <tag>  ::= <positive_integer>

use super::*;

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
    // Typed Abstraction: `λ(var: Type) body`
    b'\xce' if code[1] == b'\xbb' && code[2] == b'(' => {
      let (code, nam) = parse_name(&code[3..]);
      let  code       = parse_text(code, b":").unwrap();
      let (code, typ) = parse_term(code, ctx, idx);
      let  code       = parse_text(code, b")").unwrap();
      extend(nam, None, ctx);
      let (code, bod) = parse_term(code, ctx, idx);
      narrow(ctx);
      let nam = nam.to_vec();
      let typ = Some(Box::new(typ));
      let bod = Box::new(bod);
      (code, Lam { nam, typ, bod })
    },
    // Untyped Abstraction: `λvar body`
    b'\xce' if code[1] == b'\xbb' => {
      let (code, nam) = parse_name(&code[2..]);
      extend(nam, None, ctx);
      let (code, bod) = parse_term(code, ctx, idx);
      narrow(ctx);
      let nam = nam.to_vec();
      let typ = None;
      let bod = Box::new(bod);
      (code, Lam { nam, typ, bod })
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
    // Annotation: `<val:typ>`
    b'<' => {
      let (code, val) = parse_term(&code[1..], ctx, idx);
      let code = parse_text(code, b":").unwrap();
      let (code, typ) = parse_term(code, ctx, idx);
      let code = parse_text(code, b">").unwrap();
      (code, Ann { val: Box::new(val), typ: Box::new(typ) })
    },
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
    // Fix: `@name body`
    b'@' => {
      let (code, nam) = parse_name(&code[1..]);
      let (code, bod) = parse_term(code, ctx, idx);
      let nam = nam.to_vec();
      let bod = Box::new(bod);
      (code, Fix { nam, bod })
    }
    // Set: `*`
    b'*' => {
      (&code[1..], Set)
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
      &Lam{ref nam, ref typ, ref bod} => {
        code.extend_from_slice("λ".as_bytes());
        if let Some(ref t) = typ {
          code.extend_from_slice(b"(");
          code.append(&mut nam.clone());
          code.extend_from_slice(b": ");
          stringify_term(code, &t);
          code.extend_from_slice(b")");
        } else {
          code.append(&mut nam.clone());
        }
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
      &Ann{ref val, ref typ} => {
        code.extend_from_slice(b"<");
        stringify_term(code, &val);
        code.extend_from_slice(b": ");
        stringify_term(code, &typ);
        code.extend_from_slice(b">");
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
      &Fix{ref nam, ref bod} => {
        code.extend_from_slice(b"@");
        code.append(&mut nam.clone());
        code.extend_from_slice(b" ");
        stringify_term(code, &bod);
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

