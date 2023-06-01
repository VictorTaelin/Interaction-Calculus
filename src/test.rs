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

pub fn test() {

  let code = "
// Church arithmetic
def zero = λs λz (z)
def succ = λn λs λz dup s0 s1 = s; (s0 (n s1 z))
def mul = λn λm λs (n (m s))

// Church consts
def c1 = λf λx (f x)
def c2 = λf λx (dup #b f0 f1 = f; (f0 (f1 x)))
def c3 = λf λx (dup #c f0 f1 = f; dup #c f2 f3 = f0; (f1 (f2 (f3 x))))
def p1 = c2          // 2
def p2 = (mul c2 p1) // 4
def p3 = (mul c2 p2) // 8
def p4 = (mul c2 p3) // 16
def p5 = (mul c2 p4) // 32
def p6 = (mul c2 p5) // 64
def p7 = (mul c2 p6) // 128
def p8 = (mul c2 p7) // 256

// Booleans
def true = λt λf t
def false = λt λf f
def not = λb ((b false) true)
def neg = λb λt λf (b f t)

// Lists
def cons = λhead λtail λcons λnil (cons head tail)
def nil = λcons λnil nil
def head = λlist (list λhλt(h) λx(x))
def tail = λlist (list λhλt(t) nil)

def map = @map λf λxs
  dup #f f0 f1 = f;
  (xs λhead λtail (cons (f0 head) (map f1 tail)) nil)

def ids = @k λcons λnil (cons λx(x) k)
def nums = @x (cons zero (map succ x))
def inf = @inf λs λz (s inf)

def f0 = @x (cons true (cons true (cons false x)))
def f1 = @x (cons true (cons true (cons false (cons true (cons true (cons false x))))))

λt (t f0 f1)

//(head (tail (tail (tail ids))))

";

  //  Creates initial term
  let term = from_string(code.as_bytes());

  // Creates the net from term
  let mut inet = new_inet();
  alloc_at(&mut inet, &term, ROOT);

  // Equal
  let body = get_body(&inet, ROOT);
  let arg0 = get_argm(&inet, get_func(&inet, body));
  let arg1 = get_argm(&inet, body);
  //println!("{}", read_at(&inet, ROOT));
  println!("a = {}", read_at(&inet, arg0));
  println!("b = {}", read_at(&inet, arg1));
  let eq = equal(&mut inet, arg0, arg1);
  println!("");
  println!("[[a==b : {}]]", eq);

  // Normal
  //normal(&mut inet, ROOT);
  //println!("itt {}", read_at(&inet, ROOT));
  //println!("lam {}", lambda_term_from_inet(&inet));
  //println!("{:?} rewrites", inet.rules);


}











