mod term;
mod net;

fn main() {
    let prose = term::from_string(b"
        @P S
        @Q S
        @S s
        :: #s #z :P :P :Q :Q z
            #x :x #A #B :B A
            #a #b :b a
    ");
    println!("-- Input:\n{}", prose);
    println!("-- Net:\n{:?}", term::to_net(&prose));
    println!("-- Normal:\n{}", term::reduce(&prose));
}

/*
## Starting point: lambdas and applications

(λp. p (λa. λb. a)) (λt. t 1 2)
-------------------------------
(λt. t 1 2) (λa. λb. a)
-----------------------
(λa. λb. a) 1 2
---------------
(λb. 1) 2
---------
1

## An act of freedom: getting rid of scopes

λt. t a b ((λa. λb. λc. c) 1 2 3)
---------------------------------
λt. t 1 b ((λb. λc. c) 2 3)
---------------------------
λt. t 1 2 ((λc. c) 3)
---------------------
λt. t 1 2 3

λx. 

## Adding copying

    term = λf. λx. f x
    λt. t term term
    -----------------------
    λt. t (λf. λx. f x) (λf. λx. f x)

But that's global. Try to do it step-wise?

    term = λf. λx. f x
    λt. t term term
    -----------------------
    term = λx. f x
    λt. t (λf. term) (λf. term)

Here we have a problem, f appears twice.
Solve this problem by (...). Like this:

    term = λx. f x
    λt. t (λf0. term) (λf1. term)

But what about `f`? 
Think of it as an imaginary number. It is an intermediate
construct that may not make a lot of sense, but
eventually things cancel out. (Curiously, imaginary
numbers were also introduced with the purpose of
polynomials always have a solution, whereas here 
sobreposition is introduced with the purpose of...)

term = λf. λx. f x
λt. t term term
---------------
term = λx. (f0 & f1) x
λt. t (λf0. term) (λf1. term)
-----------------------------
term = (f0 & f1) (x0 & x1)
λt. t (λf0. λx0. term) (λf1. λx1. term)

We have successfully copied the lambdas, but now we're stuck with
the body, which is a sobreposed application of (f0 & f1) to (x0 & x1)
to solve this, we add let-application rule:

    Sobreposed application:
        ((f0 & f1) x) ~> f0 X & f1 X
                      <~ X = x

This can be interpreted as: "the sobreposed application of two functions
to an argument is the sobreposition of the application of each one of them
to a copy of the argument."

    term = (f0 & f1) (x0 & x1)
    λt. t (λf0. λx0. term) (λf1. λx1. term)
    -------------------------------------------------------------
    x    = x0 & x1
    term = f0 X & f1 X
    λt. (λf0. λx0. term) (λf1. λx1. term)

Now we have successfully copied the application, but we're now
stuck with two sobreposed copies. Obviously we need to deal with
that case too. That's the rule we add:

    Sobreposed copy:
        (v = x0 & x1) ~>
        each v        <~ xN

In other words, each sobreposed value is moved to each occurrence of v.


    appA = f0 xs
    appB = f1 xs
    term = appA & appB
    λt. (λf0. λx0. term) (λf1. λx1. term)
    -------------------------------------------------
    appA = f0 x0
    appB = f1 x1
    term = appA & appB
    λt. (λf0. λx0. term) (λf1. λx1. term)
    -------------------------------------------------
    appB = f1 x1
    term = f0 x0 & appB
    λt. (λf0. λx0. term) (λf1. λx1. term)
    -------------------------------------------------
    term = f0 x0 & f1 x1
    λt. (λf0. λx0. term) (λf1. λx1. term)
    -------------------------------------------------
    λt. (λf0. λx0. f0 x0) (λf1. λf1. f1 x1)






    










Rules:

    Beta-reduction:
        ((λx. body) arg) ~> body
        x                 <~ arg

    Lambda copy:
        (v = (λx. body)) ~> body
        each v            <~ (λxN. v)
        x                 <~ (x0 & x1 & ...)

    Sobreposed application:
        ((f0 & f1) x) ~> f0 X & f1 K
                      <~ K = x

    Sobreposed copy:
        (v = x0 & x1) ~>
        each v        <~ xN

    let-let:
        (a = x0 & x1)) ~> (a = x0), (a = x1)


*/



//: #f #x :f :f x 
  //#f #x :f :f x

//: #f #x :f :f x 
  //#f #x :f :f x
