# Interaction Calculus

The Interaction Calculus (IC) is a minimal programming language and model of
computation obtained by "completing" the affine Lambda Calculus in a way that
matches perfectly Lamping's optimal reduction algorithm. It can also be seen as
a textual syntax for [Symetric Interaction
Combinators](https://lipn.univ-paris13.fr/~mazza/papers/CombSem-MSCS.pdf): both
views are equivalent. As a model of computation, the IC has compeling
characteristics:

1. It features higher-order functions, just like the Lambda Calculus.

2. It has a well-defined cost model, just like the Turing Machine.

3. It is inherently concurrent, making it prone to massive parallelism.

4. It is fully linear, making it garbage-collection free.

This repository contains a Rust reference implementation. Also check:

- A formalization in Kind-Lang: [IC.kind](https://github.com/HigherOrderCO/Kindex/blob/master/Apps/IC/_.kind2).

- A typed version (a la Calculus of Constructions): [Interaction Type Theory](https://github.com/VictorTaelin/Interaction-Type-Theory)

# Usage

## 1. Install

```bash
git clone https://github.com/victortaelin/interaction_calculus
cd interaction_calculus
cargo install --path .
```

## 2. Create a 'main.ic' file

```javascript
def id = λx x
def c2 = λf λx (dup #b f0 f1 = f; (f0 (f1 x)))
(c2 id)
```

## 3. Run it

```bash
ic main.ic
```

See [example.ic](example.ic) for a larger example.

# Language

Interaction Calculus terms are defined by the following grammar:

```haskell
term ::=
  | λx term                   -- abstraction
  | (term term)               -- application
  | {term term}#N             -- superposition
  | dup #N {p q} = term; term -- duplication
  | x                         -- variable
```

Where variable have global scope (can occur outside binding lambdas).

The IC has 4 primitive reduction rules:

```haskell
((λx f) a)
---------- lambda application
x <- a
f

({u v}#i a)
---------------- superposition application
dup #i x0 x1 = a
{(u x0) (v x1)}#i

dup #i p q = λx f
body
----------------- lambda duplication
p <- λx0 r
q <- λx1 s
x <- {x0 x1}#i
dup #i r s = f
body

dup #i p q = {r s}#j
body
-------------------- superposition duplication
if #i == #j:
  a <- fst
  b <- snd
  cont
else:
  a <- {a0 a1}#j
  b <- {b0 b1}#j
  dup #i a0 a1 = fst;
  dup #i b0 b1 = snd;
  cont

```

Where, `a <- b` stands for a global, linear substitution of `a` by `b`. It can
be performed in `O(1)` by a simple array write, which, in turn, makes all
rewrite rules above `O(1)` too.

And that's all!

# Why?

Consider the conventional Lambda Calculus, with pairs. It has two computational rules:

- Lambda Application : `(λx body) arg`

- Pair Projection : `let {a b} = {fst snd} in cont`

When compiling the Lambda Calculus to Interaction Combinators:

- `lams` and `apps` can be represented as constructor nodes (γ) 

- `pars` and `lets` can be represented as duplicator nodes (δ)

As such, lambda applications and pair projections are just annihilations:

```
      Lambda Application                 Pair Projection
                                                                   
      (λx body) arg                      let {a b} = {fst snd} in cont 
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
   Lambda Projection                         Pair Application
                                                                  
   let {a b} = (λx body) in cont             ({fst snd} arg)   
   ------------------------------             ---------------
   a <- (λx0 b0)                             let {x0 x1} = arg in
   b <- (λx1 b1)                             {(fst x0) (snd x1)}
   x <- {x0 x1}
   let {b0 b1} = body in
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

```
let #i{a,b} = #j{fst,snd} in cont
---------------------------------
if #i == #j:
  a <- fst
  b <- snd
  cont
else:
  a <- #j{a0,a1}
  b <- #j{b0,b1} 
  let #i{a0,a1} = fst in
  let #i{b0,b1} = snd in
  cont
```

That is, it may correspond to either an Interaction Combinator annihilation or
commutation, depending on the value of the labels `#i` and `#j`. This makes IC
capable of cloning pairs, cloning cloned lambdas, computing nested loops,
performing Church-encoded arithmetic up to exponentiation, expressing arbitrary
recursive functions such as the Y-combinators and so on. In other words, with
this simple extension, IC becomes extraordinarily powerful and expressive,
giving us:

1. A new model of computation that is similar to the lambda calculus, yet, can be reduced optimally.

2. A general purpose, higher-order "core language" that is lighter and faster than the lambda calculus.

3. A term-based view for interaction combinators, making it easier to reason about their graphs.

That said, keep in mind the IC is not equivalent to the Lambda Calculus. It is a
different model. There are λ-terms that IC can't compute, and vice-versa. For
example, the Lambda Calculus can perform self-exponentiation of church-nats as
`λx (x x)`, which isn't possible on IC. Yet, on IC, we can have call/cc, direct
O(1) queues, and fully linear HOAS, which aren't possible on the Lambda
Calculus.

Finally, note that, in order to differentiate IC's "pairs" and "lets" from their
λ-Calculus counterparts, which behave differently, we call them "sups" and
"dups", respectivelly.

# Examples

### Lambda-application and superposition-projection (same as pair-projection).

```haskell
λu λv dup {a b} = {(λx x) (λy y)}; {(a u) (b v)}
------------------------------------------------ superposition-projection
λu λv {((λx x) u) ((λy y) v)}
----------------------------- lambda-application
λu λv {((λx x) u) v}
-------------------- lambda-application
λu λv {u v}
```

### Using lambda-projection to copy a function.

```haskell
dup {a b} = λx λy λz y; {a b}
----------------------------- lambda-projection
dup {a b} = λy λz y; {(λx0 a) (λx1 b)}
-------------------------------------- lambda-projection
dup {a b} = λz {y0 y1}; {(λx0 λy0 a) (λx1 λy1 b)}
------------------------------------------------- lambda-projection
dup {a b} = {y0 y1}; {(λx0 λy0 λz0 a) (λx1 λy1 λz1 b)}
------------------------------------------------------ superposition-projection
{(λx0 λy0 λz0 y0) (λx1 λy1 λz1 y1)}
``` 

### Demonstrating superposition-application (not part of Lambda Calculus)

```haskell
{{(λx x) (λy y)} (λt t)}
------------------------ superposition-application
dup {a0 a1} = λt t; {((λx x) a0) ((λy y) a1)}
--------------------------------------------- lambda-projection
dup {a0 a1} = {t0 t1}; {((λx x) (λt0 a0)) ((λy y) (λt1 a1))}
------------------------------------------------------------ superposition-projection
{((λx x) (λt0 t0)) ((λy y) (λt1 t1))}
------------------------------------- lambda-application
{((λx x) (λt0 t0)) (λt1 t1)}
---------------------------- lambda-application
{(λt0 t0) (λt1 t1)}
```

### Example 3: 2 + 3.

This is equivalent to:

```haskell
data Nat = S Nat | Z

add : Nat -> Nat -> Nat
add (S n) m = S (add n m)
add Z     m = m

main : Nat
main = add (S (S (S Z))) (S (S Z))
```

[Full reduction.](https://gist.github.com/VictorTaelin/659e047c267a801c7d9cf1970541cb54)

### Example 4: applying not 8 times to True. 

[Full reduction.](https://gist.github.com/VictorTaelin/d565bee7d9083e98ae1470067ce12dbb)

[Here is a handwritten reduction of 2^(2^2).](drawing.jpeg)

# High-order Virtual Machine (HVM)

The [High-order Virtual Machine (HVM)](https://github.com/kindelia/hvm) is a
high-performance practical implementation of the IC. Check it out!
