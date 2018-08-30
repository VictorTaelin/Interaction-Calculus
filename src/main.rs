// I'm using a different syntax here, because the parser is dumb.
// - [=a b c d] stands for [let (a,b) = c in d]
// - [@a b] stands for [(a b)]
// - [#a b] stands for [λa. b]
// - [&a b] stands for [(a,b)]
// - [*] stands for [()]
// - [-] stands for an erased (unused) lambda or let variable

mod term;
mod net;

fn main() {
    // Adds 2 + 3
    let ex2 = term::from_string(b"
        =add0 add1 #n @@n
            #np #m0 #S #- @S @@add0 np m0
            #m1 m1
        @@add1
            #S0 #- @S0 #S1 #- @S1 #S2 #- @S2 #- #Z3 Z3
            #S4 #- @S4 #S5 #- @S5 #- #Z6 Z6
    ");
    println!("-- Input:\n\n{}\n", term::from_net(&term::to_net(&ex2)));
    println!("-- Output:\n\n{}\n", term::reduce(&ex2));

    // Applies "not" 8 times
    let ex3 = term::from_string(b"
        =f1x f1y f
        =f2x f2y #x2 @f1x @f1y x2
        =f4x f4y #x4 @f2x @f2y x4
        =f8x f8y #x8 @f4x @f4y x8
        @@#f #x @f8y x 
          #t #A #B @@t B A
          #a #- a
    ");
    println!("-- Input:\n\n{}\n", term::from_net(&term::to_net(&ex3)));
    println!("-- Output:\n\n{}\n", term::reduce(&ex3));
}
