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
  | LAM: "λ" Name "." Term
  | APP: "(" Term " " Term ")"
  | SUP: "&" Label "{" Term "," Term "}"
  | COL: "!" "&" Label "{" Name "," Name "}" "=" Term ";" Term
```

Where:
- VAR represents a named variable.
- LAM represents a lambda.
- APP represents a application.
- SUP represents a superposition.
- COL represents a collapse.

Lambdas are curried, and work like their λC counterpart, except with a relaxed
scope, and with affine usage. Applications eliminate lambdas, like in λC,
through the beta-reduce (APP-LAM) interaction.

Superpositions work like pairs. Collapsers eliminate superpositions through
the collapse (COL-SUP) interaction, which works exactly like a pair projection.

What makes SUPs and COLs unique is how they interact with LAMs and APPs. When a
SUP is applied to an argument, it reduces through the overlap interaction
(APP-SUP), and when a LAM is projected, it reduces through the entangle
interaction (COL-LAM). This gives a computational behavior for every possible
interaction: there are no runtime errors on the Interaction Calculus.

The 'Label' is just a numeric value. It affects the COL-SUP interaction.

The interaction rules are listed below:

```
(λx.f a)
-------- APP-LAM
x <- a
f

(&L{a,b} c)
----------------- APP-SUP
! &L{c0,c1} = c;
&L{(a c0),(b c1)}

! &L{r,s} = λx.f;
K
----------------- COL-LAM
r <- λx0.f0
s <- λx1.f1
x <- &L{x0,x1}
! &L{f0,f1} = f;
K

! &L{x,y} = &L{a,b};
K
-------------------- COL-SUP (if equal labels)
x <- a
y <- b
K

! &L{x,y} = &R{a,b};
K
-------------------- COL-SUP (if different labels)
x <- &R{a0,b0} 
y <- &R{a1,b1}
! &L{a0,a1} = a;
! &L{b0,b1} = b;
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
  return Col(sup.lab, x0, x1, app.arg, Sup(a0, a1))

def col_lam(col, lam):
  x0 = fresh()
  x1 = fresh()
  f0 = fresh()
  f1 = fresh()
  sub[col.lft] = Lam(x0, Var(f0))
  sub[col.rgt] = Lam(x1, Var(f1))
  sub[lam.nam] = Sup(col.lab, Var(x0), Var(x1))
  return Col(col.lab, f0, f1, lam.bod, col.bod)

def col_sup(col, sup):
  if col.lab == sup.lab:
    sub[col.lft] = sup.lft
    sub[col.rgt] = sup.rgt
    return col.bod
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
      case Col(lft, rgt, val, bod):
        val = whnf(val)
        match val:
          case Lam(_, _):
            term = dup_lam(term, val)
          case Sup(_, _):
            term = dup_sup(term, val)
          case _:
            return Col(lft, rgt, val, bod)
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
    case Col(lft, rgt, val, bod):
      val_nf = normal(val)
      bod_nf = normal(bod)
      return Col(lft, rgt, val_nf, bod_nf)
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
--------------------------- COL-SUP
(λx.x λy.y)
----------- APP-LAM
λy.y
```

Example 4: (overlap)

```
({λx.x,λy.y} λz.z)
------------------ APP-SUP  
! {x0,x1} = λz.z; {(λx.x x0),(λy.y x1)}  
--------------------------------------- COL-LAM  
! {f0,f1} = {r,s}; {(λx.x λr.f0),(λy.y λs.f1)}  
---------------------------------------------- COL-SUP  
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
Lambda Calculus terms without SUPs and COLs, by extending the evaluator with
the following collapse interactions:

```
λx.&L{f0,f1}
----------------- SUP-LAM
x <- &L{x0,x1}
&L{λx0.f0,λx1.f1}

(f &L{x0,x1})
------------------- SUP-APP
!&L{f0,f1} = f
&L{(f0 x0),(f1 x1)}

&R{&L{x0,x1},y}
----------------------- SUP-SUP-X (if R>L)
!&R{y0,y1} = y;
&L{&R{x0,x1},&R{y0,y1}}

&R{x,&L{y0,y1}}
----------------------- SUP-SUP-Y (if R>L)
!&R{x0,x1} = x;
&L{&R{x0,x1},&R{y0,y1}}

!&L{x0,x1} = x; K
----------------- COL-VAR
x0 <- x
x1 <- x
K

!&L{a0,a1} = (f x); K
--------------------- COL-APP
a0 <- (f0 x0)
a1 <- (f1 x1)
!&L{f0,f1} = f;
!&L{x0,x1} = x;
K
```

# IC32: a 32-Bit Runtime

IC32 is implemented in portable C.

It represents terms with u32-pointers, which store 4 fields:

- sub (1-bit): true if this is a substitution
- tag (3-bit): the tag
- lab (4-bit): the label
- val (24-bit): the value

The tag field can be:

- `VAR`
- `SUP`
- `CO0`
- `CO1`
- `LAM`
- `APP`

The lab field stores:

- On SUP, CO0 and CO1 terms: a label.

The val field depends on the variant:

- `VAR`: points to a Lam node ({bod: Term}) or a substitution
- `CO0`: points to a Col Node ({val: Term}) or a substitution
- `CO1`: points to a Col Node ({val: Term}) or a substitution
- `SUP`: points to a Sup Node ({lft: Term, rgt: Term})
- `LAM`: points to a Lam Node ({bod: Term})
- `APP`: points to an App Node ({fun: Term, arg: Term})

A Node is a consecutive block of its child terms. For example, the SUP term
points to the memory location where its two child terms are stored.

Variable terms (VAR, CO0 and CO1) point to the location where the substitution
will be placed. As an optimization, that location is always the location of the
corresponding binder node (like a Lam or Col). When the interaction occurs, we
replace the binder node by the substituted term, with a 'sub' bit set. Then,
when we access it from a variable, we retrieve that term, clearing the bit.

Note that there is no COL term. That's because Col Nodes are special: they are't
part of the AST, and they don't store a body; they "float" on the heap. In other
words, `λx. !&0{x0,x1}=x; &0{x0,x1}` and `!&0{x0,x1}=x; λx. &0{x0,x1}` are both
valid, and stored identically on memory. As such, the only way to access a Col
Node is via its bound variables, CO0 and CO1.

Before the collapse, the Col Node stores just the collapsed value (no body).
After a collapse is triggered (when we access it via a CO0 or CO1 vars), the
first half of the collapsed term is returned, and the other half is stored where
the Col Node was, allowing the other var to get it as a substitution. For
example, the COL-SUP interaction could be implemented as:

```
def col_sup(col, sup):
  if col.lab == sup.lab:
    tm0 = heap[sup.loc + 0]
    tm1 = heap[sup.loc + 1]
    heap[col.loc] = as_sub(tm1 if col.tag == CO0 else tm0)
    return (tm0 if col.tag == CO0 else tm1)
  else:
    co0_loc = alloc(1)
    co1_loc = alloc(1)
    su0_loc = alloc(2)
    su1_loc = alloc(2)
    su0_val = Term(SUP, sup.lab, su0_loc)
    su1_val = Term(SUP, sup.lab, su1_loc)
    heap[co0_loc] = heap[sup.loc + 0]
    heap[co1_loc] = heap[sup.loc + 1]
    heap[su0_loc + 0] = Term(DP0, col.lab, co0_loc)
    heap[su0_loc + 1] = Term(DP0, col.lab, co1_loc)
    heap[su1_loc + 0] = Term(DP1, col.lab, co0_loc)
    heap[su1_loc + 1] = Term(DP1, col.lab, co1_loc)
    heap[col.loc] = as_sub(su1_val if col.tag == CO0 else tm1)
    return (su0_val if col.tag == CO0 else su1_val)
```

# NOTE: NEW, COMPACT MEMORY FORMAT

We're refactoring IC32 to a new, more compact memory format.

Each Term will still have 32 bits, but will be split as:

- sub (1-bit)
- tag (4-bit)
- val (27-bit)

The 'lab' bit will not exist anymore. Instead, tags will be:

- VAR: 0x0
- LAM: 0x1
- APP: 0x2
- ERA: 0x3
- SP0: 0x4
- SP1: 0x5
- SP2: 0x6
- SP3: 0x7
- CX0: 0x8
- CX1: 0x9
- CX2: 0xA
- CX3: 0xB
- CY0: 0xC
- CY1: 0xD
- CY2: 0xE
- CY3: 0xF

Where:
- SP{N} represents a SUP term with label N
- CX{N} represents a CO0 term with label N
- CY{N} represents a CO1 term with label N

This allows IC32 to have 4 labels, and an addressable space of 2^27 terms (512
MB), rather than just 2^24 terms (64 MB).

The 'sub' flag remains the same.

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

def parse_col(loc):
  col = alloc(1)
  consume("!")
  consume("&")
  lab = parse_uint()
  consume("{")
  co0 = parse_name()
  consume(",")
  co1 = parse_name()
  consume("}")
  consume("=")
  val = parse_term(col)
  bod = parse_term(loc)
  vars[co0] = Term(CO0, lab, loc)
  vars[co1] = Term(CO1, lab, loc)
```

# Stringifying IC32

Converting IC32 terms to strings faces two challenges:

First, IC32 terms and nodes don't store variable names. As such, we must
generate fresh, unique variable names during stringification, and maintain a
mapping from each binder's memory location to its assigned name.

Second, on IC32, Col Nodes aren't part of the main program's AST. Instead,
they "float" on the heap, and are only reachable via CO0 and CO1 variables.
Because of that, by stringifying a term naively, col nodes will be missing.

To solve these, we proceed as follows:

1. Before stringifying, we pass through the full term, and assign a id to each
variable binder we find (on lam, let, col nodes, etc.)

2. We also register every col node we found, avoiding duplicates (remember the
same col node is pointed to by up to 2 variables, CO0 and CO1)

Then, to stringify the term, we first stringify each COL node, and then we
stringify the actual term. As such, the result will always be in the form:

! &{x0 x1} = t0
! &{x2 x3} = t1
! &{x4 x5} = t2
...
term

With no COL nodes inside the ASTs of t0, t1, t2 ... and term.
