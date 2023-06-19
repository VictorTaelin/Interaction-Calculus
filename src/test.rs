use crate::main;
use crate::term::*;
use crate::inet::*;

pub fn get_body(inet: &INet, host: Port) -> Port {
  return port(addr(enter(inet,host)), 2);
}

pub fn get_func(inet: &INet, host: Port) -> Port {
  return port(addr(enter(inet,host)), 0);
}

pub fn get_argm(inet: &INet, host: Port) -> Port {
  return port(addr(enter(inet,host)), 1);
}

pub fn show(inet: &INet, prev: Port) -> String {
  let next = enter(inet, prev);
  if next == ROOT {
    "*".to_string()
  } else {
    let slot_next = slot(next);
    if slot_next == 0 {
      let a = show(inet, port(addr(next), 1));
      let b = show(inet, port(addr(next), 2));
      format!("({} {})", a, b)
    } else {
      let x = show(inet, port(addr(next), 0));
      format!("{}{}", slot_next, x)
    }
  }
}

pub fn test() {
  let code = b"
[_ a b]
[a c c]
[b d e]
[d f f]
[e g h]
[h g i]
[i j j]
";

  let inodes = from_string_inodes(code).unwrap();
  println!("inodes: {:?}", inodes);

  let mut inet = inodes_to_inet(&inodes);
  println!("inet {:?}", inet);

  let inodes = inet_to_inodes(&inet);
  println!("oi {:?}", inodes);

  println!("{}", show_inodes(&inodes));

  let body = get_body(&mut inet, ROOT);
  let arg0 = get_body(&mut inet, body);
  let arg1 = get_argm(&mut inet, body);

  println!("{}", show(&inet, arg0));
  println!("{}", show(&inet, arg1));

  let eq = equal(&mut inet, arg0, arg1);
  println!("eq = {}", eq)

}

//pub fn test() {

  //let code = "
//// Nats
//def Z = λs λz (z)
//def S = λn λs λz (s n)

//// Church arithmetic
//def zero = λs λz (z)
//def succ = λn λs λz dup s0 s1 = s; (s0 (n s1 z))
//def mul = λn λm λs (n (m s))

//// Church consts
//def c1 = λf λx (f x)
//def c2 = λf λx (dup #c f0 f1 = f; (f0 (f1 x)))
//def c3 = λf λx (dup #c f0 f1 = f; dup #c f2 f3 = f0; (f1 (f2 (f3 x))))
//def c4 = λf λx (dup #c f0 f1 = f; dup #c f2 f3 = f0; dup #c f4 f5 = f1; (f2 (f3 (f4 (f5 x)))))
//def p1 = c2          // 2
//def p2 = (mul c2 p1) // 4
//def p3 = (mul c2 p2) // 8
//def p4 = (mul c2 p3) // 16
//def p5 = (mul c2 p4) // 32
//def p6 = (mul c2 p5) // 64
//def p7 = (mul c2 p6) // 128
//def p8 = (mul c2 p7) // 256

//// Booleans
//def true = λt λf t
//def false = λt λf f
//def not = λb ((b false) true)
//def neg = λb λt λf (b f t)

//// Lists
//def cons = λhead λtail λcons λnil (cons head tail)
//def nil = λcons λnil nil
//def head = λlist (list λhλt(h) λx(x))
//def tail = λlist (list λhλt(t) nil)

//def map = @map λf λxs
  //dup #f f0 f1 = f;
  //(xs λhead λtail (cons (f0 head) (map f1 tail)) nil)

//def ids = @k λcons λnil (cons λx(x) k)
//def nums = @x (cons zero (map succ x))
//def inf = @inf λs λz (s inf)

////def f0 = @x (cons true (cons true (cons false x)))
////def f1 = @x (cons true (cons true (cons false (cons true (cons true (cons false x))))))

//def f0 = @x λsλz(s inf)
//def f1 = inf

//λt (t p2 c4)

////λt (t @xλs0λz0(s0 x) @yλs1λz1(s1 λs2λz2(s2 zero)))

////(head (tail (tail (tail ids))))
//";

  ////  Creates initial term
  //let term = from_string(code.as_bytes());

  //// Creates the net from term
  //let mut inet = new_inet();
  //alloc_at(&mut inet, &term, ROOT);

  //// Equal
  //let body = get_body(&inet, ROOT);
  //let arg0 = get_argm(&inet, get_func(&inet, body));
  //let arg1 = get_argm(&inet, body);
  ////println!("{}", read_at(&inet, ROOT));
  //println!("a = {}", read_at(&inet, arg0));
  //println!("b = {}", read_at(&inet, arg1));
  //let eq = equal(&mut inet, arg0, arg1);
  //println!("");
  //println!("[[a==b : {}]]", eq);

  //// Normal
  ////normal(&mut inet, ROOT);
  ////println!("itt {}", read_at(&inet, ROOT));
  ////println!("lam {}", lambda_term_from_inet(&inet));
  ////println!("{:?} rewrites", inet.rules);


//}
