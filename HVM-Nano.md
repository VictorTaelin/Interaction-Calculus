# HVM-Nano

The HVM-Nano is a minimal runtime for the Interaction Calculus, a term rewriting
system inspired by the Lambda Calculus (λC), but with some key differences:
1. Vars are affine: they can only occur up to one time.
2. Vars are global: they can occur anywhere in the program.
3. There is a new core primitive: the superposition.

An HVM-Nano term is defined by the following grammar:

```
Term ::=
  | VAR: Name
  | LAM: "λ" Name "." Term
  | APP: "(" Term " " Term ")"
  | SUP: "&" Label "{" Term "," Term "}"
  | COL: "!" "&" Label "{" Name "," Name "}" "=" Term ";" Term
  | NUM: Numb
  | SUC: "+" Term
  | CAL: "@" Name "(" Term ")"
```

Where:
- VAR represents a named variable.
- LAM represents a lambda.
- APP represents a application.
- SUP represents a superposition.
- COL represents a collapse.
- NUM represents a number literal.
- SUC represents a number's successor.
- CAL represents a function call.

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

For practical purposes, HVM-Nano also extends the Interaction Calculus with
numbers and functions, which aren't part of the original theory. 

The main interaction rules are listed below:

```
(λx.f a)
-------- APP-LAM
x <- a
f

(&L{a,b} c)
----------------- APP-SUP
! &L{c0,c1} = c;
&L{(a c0),(b c1)}

@F(&L{a,b})
--------------- CAL-SUP
&L{@F(a) @F(b)}

@F(N)
---------------- CAL-NUM
deref(F)[x <- N]

+N
--- SUC-NUM
N+1

+{x,y}
------- SUC-SUP
{+x,+y}
```

The collapsing interactions are listed below:

```
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

! &L{x,y} = N; K
---------------- COL-NUM
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
  return Col(sup.lab, x0, x1, app.arg, Sup(a0, a1))

def dup_lam(dup, lam):
  x0 = fresh()
  x1 = fresh()
  f0 = fresh()
  f1 = fresh()
  sub[dup.lft] = Lam(x0, Var(f0))
  sub[dup.rgt] = Lam(x1, Var(f1))
  sub[lam.nam] = Sup(dup.lab, Var(x0), Var(x1))
  return Col(dup.lab, f0, f1, lam.bod, dup.bod)

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

# Global Functions

HVM-Nano features global functions that pattern-match on a numberic argument:

```
// identity
@A(0+i) = i

// predecessor
@B(0)   = 0
@B(1+x) = x

// is-even
@C(0)   = λt. λf. t
@C(1)   = λt. λf. f
@C(2+n) = @C(n)
```

Note that:
- The first clause must be 0.
- Each clause must increment by 1.
- The last clause must be 'K+var'.

# HVM-Nano-32: a 32-Bit Runtime

HVM-Nano-32 is implemented in portable C.

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
- `NAT` (overloaded for NUM and SUC terms)
- `CAL`

The lab field stores:

- On SUP, CO0 and CO1 terms: a label.
- On CAL terms: the function id.
- On NAT terms: a flag (0 if NUM, 1 if SUC).

The val field depends on the variant:

- `VAR`: points to a Lam node ({bod: Term}) or a substitution
- `CO0`: points to a Col Node ({val: Term}) or a substitution
- `CO1`: points to a Col Node ({val: Term}) or a substitution
- `SUP`: points to a Sup Node ({lft: Term, rgt: Term})
- `LAM`: points to a Lam Node ({bod: Term})
- `APP`: points to an App Node ({fun: Term, arg: Term})
- `NUM`: stores its numeric value, unboxed
- `SUC`: points to a Suc Node ({pre: Term})
- `CAL`: points to a Cal Node ({arg: Term})

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

# Global Functions

Global Functions are parsed before the main term, and stored in a global
structure called 'Book', which holds an array mapping function ids to Function
objects. A Function object stores an array of arrays of Terms, representing, for
each clause, its body.

The function book is accessed on the CAL-NUM interaction, which selects the nth
clause of the called function, allocates enough space for its body, and copies
it into the heap, adjusting the vals of its terms, and replacing the bound var
by the call's argument.

For example, if we have the following function:

```
@F(0)   = λx. x
@F(1+n) = λt. (t n)
```

Then, that function would be stored on the Book as:

```
book[0] = {
  0: [LAM(1), VAR(0)],
  1: [LAM(1), APP(2), VAR(0), NULL],
}
```

Then, calling it as `@F((λx.x 5))` would:
1. Take the whnf of `(λx.x 5)`, resulting in `5`.
2. Select the second clause (`λt. (t n)`), as it matches the pattern `1+n` with `n=4`.
3. Allocate 4 nodes on loc `L`, and fill it as `LAM(L+1), APP(L+2), VAR(L+0), NAT(4)`.

# Parsing HVM-Nano

On HVM-Nano, all bound variables have global range. For example, consider the term:

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

HVM-Nano files are parsed as a series of functions, followed by a main term:

```
@A(0)   = ...
@A(1+x) = ...
@B(0)   = ...
@B(1+x) = ...
...
```

# Stringifying HVM-Nano

Converting HVM-Nano terms to strings faces two challenges:

First, HVM-Nano terms and nodes don't store variable names. As such, we must
generate fresh, unique variable names during stringification, and maintain a
mapping from each binder's memory location to its assigned name.

Second, on HVM-Nano, Col Nodes aren't part of the main program's AST. Instead,
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
