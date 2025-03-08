# Interaction Calculus

The Interaction Calculus is a minimal term rewriting system inspired by the
Lambda Calculus (λC), but with some key differences:
1. Vars are affine: they can only occur up to one time.
2. Vars are global: they can occur anywhere in the program.
3. There is a new core primitive: the superposition.

An IC term is defined by the following grammar:

```
Term ::=
  | VAR: Name
  | ERA: "*"
  | LAM: "λ" Name "." Term
  | APP: "(" Term " " Term ")"
  | SUP: "&" Label "{" Term "," Term "}"
  | DUP: "!" "&" Label "{" Name "," Name "}" "=" Term ";" Term
  | NUM: Number
  | SUC: "+" Term
  | SWI: "?" Term "{" "0" ":" Term ";" "+" ":" Term ";" "}"
```

Where:
- VAR represents a variable.
- ERA represents an erasure.
- LAM represents a lambda.
- APP represents a application.
- SUP represents a superposition.
- DUP represents a duplication.
- NUM represents a number.
- SUC represents a successor
- SWI represents a switch.

Lambdas are curried, and work like their λC counterpart, except with a relaxed
scope, and with affine usage. Applications eliminate lambdas, like in λC,
through the beta-reduce (APP-LAM) interaction.

Superpositions work like pairs. Duplications eliminate superpositions through
the DUP-SUP interaction, which works exactly like a pair projection.

What makes SUPs and DUPs unique is how they interact with LAMs and APPs. When a
SUP is applied to an argument, it reduces through the APP-SUP interaction, and
when a LAM is projected, it reduces through the DUP-LAM interaction. This gives
a computational behavior for every possible interaction: there are no runtime
errors on the Interaction Calculus.

NUM, SUC and SWI aren't essential to the theory, but are added for convenience.

The 'Label' is just a numeric value. It affects the DUP-SUP interaction.

The core interaction rules are listed below:

```
(* a)
----- APP-ERA
*

(λx.f a)
-------- APP-LAM
x <- a
f

(&L{a,b} c)
----------------- APP-SUP
! &L{c0,c1} = c;
&L{(a c0),(b c1)}

! &L{r,s} = *;
K
-------------- DUP-ERA
r <- *
s <- *
K

! &L{r,s} = λx.f;
K
----------------- DUP-LAM
r <- λx0.f0
s <- λx1.f1
x <- &L{x0,x1}
! &L{f0,f1} = f;
K

! &L{x,y} = &L{a,b};
K
-------------------- DUP-SUP (if equal labels)
x <- a
y <- b
K

! &L{x,y} = &R{a,b};
K
-------------------- DUP-SUP (if different labels)
x <- &R{a0,b0} 
y <- &R{a1,b1}
! &L{a0,a1} = a;
! &L{b0,b1} = b;
K
```

The numeric interaction rules are listed below:

```
+N
--- SUC-NUM
N+1

+*
-- SUC-ERA
*

+&L{x,y}
--------- SUC-SUP
&L{+x,+y}

?N{0:z;+:s;}
------------ SWI-NUM (if N==0)
z

?N{0:z;+:s;}
------------ SWI-NUM (if N>0)
(s N-1)

?*{0:z;+:s;}
------------ SWI-ERA
*

?&L{x,y}{0:z;+:s;}
--------------------------------- SWI-SUP
!&L{z0,z1} = z;
!&L{s0,s1} = s;
&L{?x{0:z0;+:s0;},?y{0:z1;+:s1;}}

! &L{x,y} = N;
K
-------------- DUP-NUM
x <- N
y <- N
K
```

Where `x <- t` stands for a global substitution of `x` by `t`.

Since variables are affine, substitutions can be implemented efficiently by just
inserting an entry in a global substitution map (`sub[var] = value`). There is
no need to traverse the target term, or to handle name capture, as long as fresh
variable names are globally unique. It can also be implemented in a concurrent
setup with a single atomic-swap.

Below is a pseudocode implementation of these interaction rules:

```
def app_lam(app, lam):
  sub[lam.nam] = app.arg
  return lam.bod

def app_sup(app, sup):
  x0 = fresh()
  x1 = fresh()
  a0 = App(sup.lft, Var(x0))
  a1 = App(sup.rgt, Var(x1))
  return Dup(sup.lab, x0, x1, app.arg, Sup(a0, a1))

def dup_lam(dup, lam):
  x0 = fresh()
  x1 = fresh()
  f0 = fresh()
  f1 = fresh()
  sub[dup.lft] = Lam(x0, Var(f0))
  sub[dup.rgt] = Lam(x1, Var(f1))
  sub[lam.nam] = Sup(dup.lab, Var(x0), Var(x1))
  return Dup(dup.lab, f0, f1, lam.bod, dup.bod)

def dup_sup(dup, sup):
  if dup.lab == sup.lab:
    sub[dup.lft] = sup.lft
    sub[dup.rgt] = sup.rgt
    return dup.bod
```

Terms can be reduced to weak head normal form, which means reducing until the
outermost constructor is a value (LAM, SUP, etc.), or until no more reductions
are possible. Example:

```
def whnf(term):
  while True:
    match term:
      case Var(nam):
        if nam in sub:
          term = sub[nam]
        else:
          return term
      case App(fun, arg):
        fun = whnf(fun)
        match fun:
          case Lam(_, _):
            term = app_lam(term, fun)
          case Sup(_, _):
            term = app_sup(term, fun)
          case _:
            return App(fun, arg)
      case Dup(lft, rgt, val, bod):
        val = whnf(val)
        match val:
          case Lam(_, _):
            term = dup_lam(term, val)
          case Sup(_, _):
            term = dup_sup(term, val)
          case _:
            return Dup(lft, rgt, val, bod)
      case _:
        return term
```

Terms can be reduced to full normal form by recursively taking the whnf:

```
def normal(term):
  term = whnf(term)
  match term:
    case Lam(nam, bod):
      bod_nf = normal(bod)
      return Lam(nam, bod_nf)
    case App(fun, arg):
      fun_nf = normal(fun)
      arg_nf = normal(arg)
      return App(fun_nf, arg_nf)
    case Sup(lft, rgt):
      lft_nf = normal(lft)
      rgt_nf = normal(rgt)
      return Sup(lft_nf, rgt_nf)
    case Dup(lft, rgt, val, bod):
      val_nf = normal(val)
      bod_nf = normal(bod)
      return Dup(lft, rgt, val_nf, bod_nf)
    case _:
      return term
```

Below are some normalization examples.

Example 0: (simple λ-term)

```
(λx.λt.(t x) λy.y)
------------------ APP-LAM
λt.(t λy.y)
```

Example 1: (larger λ-term)

```
(λb.λt.λf.((b f) t) λT.λF.T)
---------------------------- APP-LAM
λt.λf.((λT.λF.T f) t)
----------------------- APP-LAM
λt.λf.(λF.t f)
-------------- APP-LAM
λt.λf.t
```

Example 2: (global scopes)

```
{x,(λx.λy.y λk.k)}
------------------ APP-LAM
{λk.k,λy.y}
```

Example 3: (superposition)

```
!{a,b} = {λx.x,λy.y}; (a b)
--------------------------- DUP-SUP
(λx.x λy.y)
----------- APP-LAM
λy.y
```

Example 4: (overlap)

```
({λx.x,λy.y} λz.z)
------------------ APP-SUP  
! {x0,x1} = λz.z; {(λx.x x0),(λy.y x1)}  
--------------------------------------- DUP-LAM  
! {f0,f1} = {r,s}; {(λx.x λr.f0),(λy.y λs.f1)}  
---------------------------------------------- DUP-SUP  
{(λx.x λr.r),(λy.y λs.s)}  
------------------------- APP-LAM  
{λr.r,(λy.y λs.s)}  
------------------ APP-LAM  
{λr.r,λs.s}  
```

Example 5: (default test term)

The following term can be used to test all interactions:

```
((λf.λx.!{f0,f1}=f;(f0 (f1 x)) λB.λT.λF.((B F) T)) λa.λb.a)
----------------------------------------------------------- 16 interactions
λa.λb.a
```

# Collapsing

An Interaction Calculus term can be collapsed to a superposed tree of pure
Lambda Calculus terms without SUPs and DUPs, by extending the evaluator with the
following collapse interactions:

```
λx.*
------ ERA-LAM
x <- *
*

(f *)
----- ERA-APP
*

λx.&L{f0,f1}
----------------- SUP-LAM
x <- &L{x0,x1}
&L{λx0.f0,λx1.f1}

(f &L{x0,x1})
------------------- SUP-APP
!&L{f0,f1} = f;
&L{(f0 x0),(f1 x1)}

~N{0:&L{z0,z1};+:s;}
--------------------------------- SUP-SWI-Z
!&L{N0,N1} = N;
!&L{S0,S1} = S;
&L{~N0{0:z0;+:S0},~N1{0:z1;+:S1}}

~N{0:z;+:&0{s0,s1};}
--------------------------------- SUP-SWI-S
!&L{N0,N1} = N;
!&L{Z0,Z1} = Z;
&L{~N0{0:z0;+:S0},~N1{0:z1;+:S1}}

&R{&L{x0,x1},y}
----------------------- SUP-SUP-X (if R>L)
!&R{y0,y1} = y;
&L{&R{x0,x1},&R{y0,y1}}

&R{x,&L{y0,y1}}
----------------------- SUP-SUP-Y (if R>L)
!&R{x0,x1} = x;
&L{&R{x0,x1},&R{y0,y1}}

!&L{x0,x1} = x; K
----------------- DUP-VAR
x0 <- x
x1 <- x
K

!&L{a0,a1} = (f x); K
--------------------- DUP-APP
a0 <- (f0 x0)
a1 <- (f1 x1)
!&L{f0,f1} = f;
!&L{x0,x1} = x;
K
```

# IC32: a 32-Bit Runtime

IC32 is implemented in portable C.

Each Term is represented as a 32-bit word, split into the following fields:

- sub (1-bit): true if this is a substitution
- tag (5-bit): the tag identifying the term type and label
- val (26-bit): the value, typically a pointer to a node in memory

The tag field can be one of the following:

- `VAR`: 0x00
- `LAM`: 0x01
- `APP`: 0x02
- `ERA`: 0x03
- `NUM`: 0x04
- `SUC`: 0x05
- `SWI`: 0x06
- `TMP`: 0x07
- `SP0`: 0x08
- `SP1`: 0x09
- `SP2`: 0x0A
- `SP3`: 0x0B
- `SP4`: 0x0C
- `SP5`: 0x0D
- `SP6`: 0x0E
- `SP7`: 0x0F
- `CX0`: 0x10
- `CX1`: 0x11
- `CX2`: 0x12
- `CX3`: 0x13
- `CX4`: 0x14
- `CX5`: 0x15
- `CX6`: 0x16
- `CX7`: 0x17
- `CY0`: 0x18
- `CY1`: 0x19
- `CY2`: 0x1A
- `CY3`: 0x1B
- `CY4`: 0x1C
- `CY5`: 0x1D
- `CY6`: 0x1E
- `CY7`: 0x1F

The val field depends on the variant:

- `VAR`: points to a Lam node ({bod: Term}) or a substitution.
- `LAM`: points to a Lam node ({bod: Term}).
- `APP`: points to an App node ({fun: Term, arg: Term}).
- `ERA`: unused.
- `NUM`: stores an unsigned integer.
- `SUC`: points to a Suc node ({num: Term})
- `SWI`: points to a Swi node ({num: Term, ifZ: Term, ifS: Term})
- `SP{L}`: points to a Sup node ({lft: Term, rgt: Term}).
- `CX{L}`: points to a Dup node ({val: Term}) or a substitution.
- `CY{L}`: points to a Dup node ({val: Term}) or a substitution.

A node is a consecutive block of its child terms. For example, the SUP term
points to the memory location where its two child terms are stored.

Variable terms (`VAR`, `CX{L}`, and `CY{L}`) point to the location where the
substitution will be placed. As an optimization, that location is always the
location of the corresponding binder node (like a Lam or Dup). When the
interaction occurs, we replace the binder node by the substituted term, with the
'sub' bit set. Then, when we access it from a variable, we retrieve that term,
clearing the bit.

On SUPs and DUPs, the 'L' stands for the label of the corresponding node.

Note that there is no explicit DUP term. That's because Dup nodes are special:
they aren't part of the AST, and they don't store a body; they "float" on the
heap.  In other words, `λx. !&0{x0,x1}=x; &0{x0,x1}` and `!&0{x0,x1}=x; λx.
&0{x0,x1}` are both valid, and stored identically in memory. As such, the only
way to access a Dup node is via its bound variables, `CX{L}` and `CY{L}`.

Before the interaction, the Dup node stores just the duplicated value (no body).
After a collapse is triggered (when we access it via a `CX{L}` or `CY{L}`
variable), the first half of the duplicated term is returned, and the other half
is stored where the Dup node was, allowing the other variable to get it as a
substitution. For example, the DUP-SUP interaction could be implemented as:

```
def dup_sup(dup, sup):
  dup_lab = dup.tag & 0x3
  sup_lab = sup.tag & 0x3
  if dup_lab == sup_lab:
    tm0 = heap[sup.loc + 0]
    tm1 = heap[sup.loc + 1]
    heap[dup.loc] = as_sub(tm1 if (dup.tag & 0x4) == 0 else tm0)
    return (tm0 if (dup.tag & 0x4) == 0 else tm1)
  else:
    co0_loc = alloc(1)
    co1_loc = alloc(1)
    su0_loc = alloc(2)
    su1_loc = alloc(2)
    su0_val = Term(SP0 + sup_lab, su0_loc)
    su1_val = Term(SP0 + sup_lab, su1_loc)
    heap[co0_loc] = heap[sup.loc + 0]
    heap[co1_loc] = heap[sup.loc + 1]
    heap[su0_loc + 0] = Term(CX0 + dup_lab, co0_loc)
    heap[su0_loc + 1] = Term(CX0 + dup_lab, co1_loc)
    heap[su1_loc + 0] = Term(CY0 + dup_lab, co0_loc)
    heap[su1_loc + 1] = Term(CY0 + dup_lab, co1_loc)
    heap[dup.loc] = as_sub(su1_val if (dup.tag & 0x4) == 0 else su0_val)
    return (su0_val if (dup.tag & 0x4) == 0 else su1_val)
```

The NUM, SUC and SWI terms extend the IC with unboxed unsigned integers.

# Parsing IC32

On IC32, all bound variables have global range. For example, consider the term:

λt.((t x) λx.λy.y)

Here, the `x` variable appears before its binder, `λx`. Since runtime variables
must point to their bound λ's, linking them correctly requires caution. A way to
do it is to store two structures at parse-time:

1. lcs: an array from names to locations
2. vrs: a map from names to var terms

Whenever we parse a name, we add the current location to the 'uses' array, and
whenever we parse a binder (lams, lets, etc.), we add a variable term pointing
to it to the 'vars' map. Then, once the parsing is done, we run iterate through
the 'uses' array, and write, to each location, the corresponding term. Below
are some example parsers using this strategy:

```
def parse_var(loc):
  nam = parse_name()
  uses.push((nam,loc))

def parse_lam(loc):
  lam = alloc(1)
  consume("λ")
  nam = parse_name()
  consume(".")
  vars[nam] = Term(VAR, 0, lam)
  parse_term(lam)
  heap[loc] = Term(LAM, 0, lam)

def parse_app(loc):
  app = alloc(2)
  consume("(")
  parse_term(app + 0)
  consume(" ")
  parse_term(app + 1)
  consume(")")
  heap[loc] = Term(APP, 0, app)

def parse_sup(loc):
  sup = alloc(2)
  consume("&")
  lab = parse_uint()
  consume("{")
  lft = parse_term(sup + 0)
  consume(",")
  rgt = parse_term(sup + 1)
  consume("}")
  heap[loc] = Term(SUP, lab, sup)

def parse_dup(loc):
  dup = alloc(1)
  consume("!")
  consume("&")
  lab = parse_uint()
  consume("{")
  co0 = parse_name()
  consume(",")
  co1 = parse_name()
  consume("}")
  consume("=")
  val = parse_term(dup)
  bod = parse_term(loc)
  vars[co0] = Term(DP0, lab, loc)
  vars[co1] = Term(DP1, lab, loc)
```

# Stringifying IC32

Converting IC32 terms to strings faces two challenges:

First, IC32 terms and nodes don't store variable names. As such, we must
generate fresh, unique variable names during stringification, and maintain a
mapping from each binder's memory location to its assigned name.

Second, on IC32, Dup nodes aren't part of the main program's AST. Instead,
they "float" on the heap, and are only reachable via DP0 and DP1 variables.
Because of that, by stringifying a term naively, Col nodes will be missing.

To solve these, we proceed as follows:

1. Before stringifying, we pass through the full term, and assign a id to each
variable binder we find (on lam, let, dup, etc.)

2. We also register every Dup node we found, avoiding duplicates (remember the
same dup node is pointed to by up to 2 variables, DP0 and DP1)

Then, to stringify the term, we first stringify each DUP node, and then we
stringify the actual term. As such, the result will always be in the form:

! &{x0 x1} = t0
! &{x2 x3} = t1
! &{x4 x5} = t2
...
term

With no Dup nodes inside the ASTs of t0, t1, t2 ... and term.
