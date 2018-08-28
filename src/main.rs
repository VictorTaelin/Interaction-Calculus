mod term;
mod net;

// Note: using a different syntax (because the parser is dumb).
// [@a b c d] stands for [let (a,b) = c in d]
// [:a b] stands for [(a b)]
// [#a b] stands for [λa. b]
// [&a b] stands for [(a,b)]

fn main() {
    let _ex2 = term::from_string(b"
        @add0 add1 #n ::n
            #np #m0 #S #Z :S ::add0 np m0
            #m1 m1
        ::add1
            #S0 #Z0 :S0 #S1 #Z1 :S1 #S2 #Z2 :S2 #S3 #Z3 Z3
            #S4 #Z4 :S4 #S5 #Z5 :S5 #S6 #Z6 Z6
    ");

    let ex3 = term::from_string(b"
        @f1x f1y f
        @f2x f2y #x2 :f1x :f1y x2
        @f4x f4y #x4 :f2x :f2y x4
        @f8x f8y #x8 :f4x :f4y x8
        -f8x
        -b
        ::#f #x :f8y x 
          #t #A #B ::t B A
          #a #b a
    ");
    println!("-- Input:\n{}", ex3);
    println!("-- Net:\n{:?}", term::to_net(&ex3));
    println!("-- Input:\n{}", term::from_net(&term::to_net(&ex3)));
    println!("-- Normal:\n{}", term::reduce(&ex3));
}
