# The Interaction Calculus

The Interaction Calculus (IC) is term rewriting system inspired by the Lambda
Calculus (λC), but with some major differences:
1. Vars are affine: they can only occur up to one time.
2. Vars are global: they can occur anywhere in the program.
3. There is a new core primitive: the superposition.

An IC term is defined by the following grammar:

```
Term ::=
  | VAR: Name
  | LAM: "λ" Name "." Term
  | APP: "(" Term " " Term ")"
  | SUP: "{" Term "," Term "}"
  | COL: "!" "{" Name "," Name "}" "=" Term ";" Term
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
interaction: there are no runtime errors on IC.

The interaction rules are defined below:

Beta-Reduce:

```
(λx.f a)
-------- APP-LAM
x <- a
f
```

Superpose:

```
({a,b} c)
--------------- APP-SUP
! {x0,x1} = c;
{(a x0),(b x1)}
```

Entangle:

```
! {r,s} = λx.f;
K
--------------- COL-LAM
r <- λx0.f0
s <- λx1.f1
x <- {x0,x1}
! {f0,f1} = f;
K
```

Collapse:

```
! {x,y} = {a,b};
K
--------------- COL-SUP
x <- a
y <- b
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
  return Col(x0, x1, app.arg, Sup(a0, a1))

def dup_lam(dup, lam):
  x0 = fresh()
  x1 = fresh()
  f0 = fresh()
  f1 = fresh()
  sub[dup.lft] = Lam(x0, Var(f0))
  sub[dup.rgt] = Lam(x1, Var(f1))
  sub[lam.nam] = Sup(Var(x0), Var(x1))
  return Col(f0, f1, lam.bod, dup.bod)

def dup_sup(dup, sup):
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
