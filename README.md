# Symmetric Interaction Calculus

The [Symmetric Interaction Calculus](https://medium.com/@maiavictor/the-abstract-calculus-fe8c46bcf39c) is a minimal programming language and model of computation obtained by slightly modifying the Lambda Calculus so that it matches perfectly the abstract part of [Lamping's optimal reduction algorithm](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.2386&rep=rep1&type=pdf). Characteristics:

1. Like the Lambda Calculus, it can easily express algebraic datatypes and arbitrary recursive algorithms.

2. Like Turing Machines, it can be evaluated through simple, constant-time operations.

3. Unlike the Lambda Calculus (and like Rust), it does not require garbage-collection.

4. Unlike the Lambda Calculus, all intermediate steps of its optimal reduction correspond to terms.

5. Unlike the Lambda Calculus, terms can be reduced not only optimally, but efficiently (i.e., [no oracles](https://dl.acm.org/citation.cfm?id=1131315)).

6. Unlike both, it is intrinsically parallel.

7. It is isomorphic to [symmetric interaction combinators](https://pdfs.semanticscholar.org/1731/a6e49c6c2afda3e72256ba0afb34957377d3.pdf), a beautiful model of computation.

**Note:** this is an old repository. A new version is being developed on
[Kind](https://github.com/kindelia/kind). Check the code
[here](https://github.com/Kindelia/Wikind/blob/master/IC/_.kind2).


## Syntax

The syntax is obtained by simply extending the Lambda Calculus with `pairs` and `let`:


```haskell
term ::=
  | λx. term                 -- abstraction
  | (term term)              -- application
  | (term,term)              -- pair
  | let (p,q) = term in term -- definition
  | x                        -- variable
```

Except variables are restricted to occur only once and are freed to occur globally ([why?](https://stackoverflow.com/questions/52048420/can-a-calculus-have-incremental-copying-and-closed-scopes)).

## Reduction rules

There are 4 reduction rules. Two of them are usual: the application of a lambda, and the projection of a pair. The other two deal with the previously unspecified cases: "applying a pair" and "projecting a lambda". The first performs a parallel application, and the second performs an incremental duplication. All of them are constant-time operations.

```haskell
-- Rule 0: lambda-application

((λx.f) a)
----------
f [x / a]

-- Rule 1: pair-projection

let (p,q) = (u,v) in t
----------------------
t [p / u] [q / v]

-- Rule 2: pair-application

((u,v) a)
----------------
let (x0,x1) = a 
in ((u x0),(v x1))

-- Rule 3: lambda-projection

let (p,q) = (λx.f) in t
-----------------------
let (p,q) = f in t
[p / λx0.p]
[q / λx1.q]
[x / (x0,x1)]
```

Here, `[a / b]` stands for a global substitution of the occurrence of `a` by `b`, and `x0`, `x1` are fresh variables. I've used additional parenthesis around lambdas to make the reading clearer.

## Examples

### Example 0: lambda-application and pair-projection (nothing new).

```haskell
λu. λv. let (a,b) = (λx.x, λy.y) in (a u, b v)
----------------------------------------------  pair-projection
λu. λv. ((λx.x) u, (λy.y) v)
---------------------------- lambda-application
λu. λv. ((λx.x) u, v)
--------------------- lambda-application
λu. λv. (u, v)
```

### Example 1: using lambda-projection to copy a function.

```haskell
let (a,b) = λx.λy.λz.y in (a,b)
------------------------------- lambda-projection
let (a,b) = λy.λz.y in (λx0.a, λx1.b)
--------------------------------------- lambda-projection
let (a,b) = λz. (y0,y1) in (λx0.λy0.a, λx1.λy1.b)
-------------------------------------------------- lambda-projection
let (a,b) = (y0,y1) in (λx0.λy0.λz0.a, λx1.λy1.λz1.b)
----------------------------------------------------- pair-projection
(λx0.λy0.λz0.y0, λx1.λy1.λz1.y1)
``` 

### Example 2: demonstrating pair-application.

```haskell
((λx.x, λy.y) λt.t) 
------------------- pair-application
let (a0,a1) = λt. t in ((λx.x) a0, (λy.y) a1)
--------------------------------------------- lambda-projection
let (a0,a1) = (t0,t1) in ((λx.x) λt0.a0, (λy.y) λt1.a1)
-------------------------------------------------------  pair-projection
((λx.x) λt0.t0, (λy.y) λt1.t1)
------------------------------- lambda-application
((λx.x) λt0.t0, λt1.t1)
----------------------- lambda-application
(λt0.t0, λt1.t1)
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

---

[Here is a handwritten reduction of 2^(2^2).](drawing.jpeg)

## High-order Virtual Machine (HVM)

The [High-order Virtual Machine (HVM)](https://github.com/kindelia/hvm) is a high-performance practical implementation of SIC. It is a lazy, parallel runtime capable of evaluating functional programming languages optimally.
