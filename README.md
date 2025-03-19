# Interaction Calculus

The Interaction Calculus is a minimal term rewriting system inspired by the
Lambda Calculus (λC), but with some key differences that make it inherently more
efficient, in a way that closely resembles Lamping's optimal λ-calculus
evaluator, and more expressive, in some ways. In particular:

1. Vars are affine: they can only occur up to one time.

2. Vars are global: they can occur anywhere in the program.

3. It features first-class *superpositions* and *duplications*.

Global lambdas allow the IC to express concepts that aren't possible on the
traditional λC, including continuations, linear HOAS, and mutable references.
Superpositions and duplications allow the IC to be optimally evaluated, making
some computations exponentially faster. Finally, being fully affine makes its
garbage collector very efficient, and greatly simplifies parallelism.

The [HVM](https://github.com/HigherOrderCO/HVM3) is an efficient, fully featured
implementation of this calculus. This repo includes a reference implementation
in C, which is also quite fast.

## Usage

This repository includes a reference implementation of the Interaction Calculus
in plain C, with some additional features, like native numbers. To install it:

```
make clean
make
```

Then, run one of the examples:

```
./bin/ic run book/test_0.ic
```

## Specification

An IC term is defined by the following grammar:

```haskell
Term ::=
  | VAR: Name
  | ERA: "*"
  | LAM: "λ" Name "." Term
  | APP: "(" Term " " Term ")"
  | SUP: "&" Label "{" Term "," Term "}"
  | DUP: "!" "&" Label "{" Name "," Name "}" "=" Term ";" Term
```

Where:
- VAR represents a variable.
- ERA represents an erasure.
- LAM represents a lambda.
- APP represents a application.
- SUP represents a superposition.
- DUP represents a duplication.

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

The 'Label' is just a numeric value. It affects the DUP-SUP interaction.

The core interaction rules are listed below:

```haskell
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

Where `x <- t` stands for a global substitution of `x` by `t`.

Since variables are affine, substitutions can be implemented efficiently by just
inserting an entry in a global substitution map (`sub[var] = value`). There is
no need to traverse the target term, or to handle name capture, as long as fresh
variable names are globally unique. It can also be implemented in a concurrent
setup with a single atomic-swap.

Below is a pseudocode implementation of these interaction rules:

```python
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

```python
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
        match fun.tag:
          case Lam: term = app_lam(term, fun)
          case Sup: term = app_sup(term, fun)
          case _  : return App(fun, arg)
      case Dup(lft, rgt, val, bod):
        val = whnf(val)
        match val.tag:
          case Lam: term = dup_lam(term, val)
          case Sup: term = dup_sup(term, val)
          case _  : return Dup(lft, rgt, val, bod)
      case _:
        return term
```

Terms can be reduced to full normal form by recursively taking the whnf:

```python
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
    ...
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

## Collapsing

An Interaction Calculus term can be collapsed to a superposed tree of pure
Lambda Calculus terms without SUPs and DUPs, by extending the evaluator with the
following collapse interactions:

```haskell
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

## IC = Lambda Calculus U Interaction Combinators

Consider the conventional Lambda Calculus, with pairs. It has two computational rules:

- Lambda Application : `(λx.body arg)`

- Pair Projection : `let {a,b} = {fst,snd} in cont`

When compiling the Lambda Calculus to Interaction Combinators:

- `lams` and `apps` can be represented as constructor nodes (γ) 

- `pars` and `lets` can be represented as duplicator nodes (δ)

As such, lambda applications and pair projections are just annihilations:

```
      Lambda Application                 Pair Projection
                                                                   
      (λx.body arg)                      let {a,b} = {fst,snd} in cont 
      ----------------                   -----------------------------
      x <- arg                           a <- fst                  
      body                               b <- snd                  
                                         cont                      
                                                                   
    ret  arg    ret  arg                  b   a       b    a       
     |   |       |    |                   |   |       |    |       
     |___|       |    |                   |___|       |    |       
 app  \ /         \  /                let  \#/         \  /        
       |    ==>    \/                       |    ==>    \/         
       |           /\                       |           /\         
 lam  /_\         /  \               pair  /#\         /  \        
     |   |       |    |                   |   |       |    |       
     |   |       |    |                   |   |       |    |       
     x  body     x   body                fst snd    fst   snd      
                                                                   
 "The application of a lambda        "The projection of a pair just 
 substitutes the lambda's var        substitutes the projected vars
 by the application's arg, and       by each element of the pair, and
 returns the lambda body."           returns the continuation."
```

But annihilations only happen when identical nodes interact. On interaction
nets, it is possible for different nodes to interact, which triggers another rule,
the commutation. That rule could be seen as handling the following expressions:

- Lambda Projection : `let {a b} = (λx body) in cont`

- Pair Application : `({fst snd} arg)`

But how could we "project" a lambda or "apply" a pair? On the Lambda Calculus, these
cases are undefined and stuck, and should be type errors. Yet, by interpreting the
effects of the commutation rule on the interaction combinator point of view, we
can propose a reasonable reduction for these lambda expressions:

```
   Lambda Application                         Pair Application
                                                                  
   let {a,b} = (λx.body) in cont             ({fst,snd} arg)   
   ------------------------------             ---------------
   a <- λx0.b0                               let {x0,x1} = arg in
   b <- λx1.b1                               {(fst x0),(snd x1)}
   x <- {x0,x1}
   let {b0,b1} = body in
   cont                   
       
    ret  arg         ret  arg            ret  arg         ret  arg  
     |   |            |    |              |   |            |    |   
     |___|            |    |              |___|            |    |   
 let  \#/            /_\  /_\         app  \ /            /#\  /#\  
       |      ==>    |  \/  |               |      ==>    |  \/  |  
       |             |_ /\ _|               |             |_ /\ _|  
 lam  /_\            \#/  \#/        pair  /#\            \ /  \ /  
     |   |            |    |              |   |            |    |   
     |   |            |    |              |   |            |    |   
     x  body          x   body           var body         var  body 

 "The projection of a lambda         "The application of a pair is a pair
 substitutes the projected vars      of the first element and the second
 by a copies of the lambda that      element applied to projections of the
 return its projected body, with     application argument."
 the bound variable substituted
 by the new lambda vars paired."
```

This, in a way, completes the lambda calculus; i.e., previously "stuck"
expressions now have a meaningful computation. That system, as written, is
Turing complete, yet, it is very limited, since it isn't capable of cloning
pairs, or cloning cloned lambdas. There is a simple way to greatly increase its
expressivity, though: by decorating lets with labels, and upgrading the pair
projection rule to:

```haskell
let &i{a,b} = &j{fst,snd} in cont
---------------------------------
if i == j:
  a <- fst
  b <- snd
  cont
else:
  a <- &j{a0,a1}
  b <- &j{b0,b1} 
  let &i{a0,a1} = fst in
  let &i{b0,b1} = snd in
  cont
```

That is, it may correspond to either an Interaction Combinator annihilation or
commutation, depending on the value of the labels `&i` and `&j`. This makes IC
capable of cloning pairs, cloning cloned lambdas, computing nested loops,
performing Church-encoded arithmetic up to exponentiation, expressing arbitrary
recursive functions such as the Y-combinators and so on. In other words, with
this simple extension, IC becomes extraordinarily powerful and expressive,
giving us a new foundation for symbolic computing, that is, in many ways, very
similar to the λ-Calculus, yet, with key differences that make it more
efficient in some senses, and capable of expressing new things (like call/cc,
O(1) queues, linear HOAS), but unable to express others (like `λx.(x x)`).

## IC32: a 32-Bit Runtime

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

```python
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

## Parsing IC32

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

```python
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

## Stringifying IC32

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

```haskell
! &{x0 x1} = t0
! &{x2 x3} = t1
! &{x4 x5} = t2
...
term
```

With no Dup nodes inside the ASTs of t0, t1, t2 ... and term.
