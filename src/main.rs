// I'm using a different syntax here, because the parser is dumb.
// - [=a b c d] stands for [let (a,b) = c in d]
// - [@a b] stands for [(a b)]
// - [#a b] stands for [λa. b]
// - [&a b] stands for [(a,b)]
// - [*] stands for [()]
// - [-] stands for an erased (unused) lambda or let variable
// - [/a b c] inlines all occurrences of [a] by the closed term [b]

mod term;
mod net;

fn main() {
    let example = term::from_string(b"
        (repetitor 1)
        /c1 #f #x
            @f x

        (repetitor 2)
        /c2 #f #x
            =f1_a f1_b f
            @f1_a @f1_b x

        (repetitor 3)
        /c3 #f #x
            =f1_a f1_b f
            =f1_c f1_d f1_b
            =f2_a -    #x2 @f1_c @f1_d x2
            @f1_a @f2_a x

        (repetitor 4)
        /c4 #f #x
            =f1_a f1_b f
            =f2_a f2_b #x2 @f1_a @f1_b x2
            @f2_a @f2_b x

        (boolean true)
        /true #true #-
            true

        (boolean false)
        /false #- #false
            false

        (boolean negation)
        /not #bool #true #false
            @@bool false true

        (pair)
        /pair #a #b #t
            @@t a b

        (nat zero)
        /zer #- #Z
            Z

        (nat successor)
        /suc #n #S #-
            @S n

        (nat addition)
        /add =add_a add_b #n @@n
            #n_pred #m_a #S #- @S @@add_a n_pred m_a
            #m_b m_b
            add_b

        (nat multiplication)
        /mul #n =mul_a mul_b #m @@m
            #m_pred @@add n @mul_a m_pred
            zer
            mul_b

        (test program)
        @@pair
            @@c3 not true
            @@mul
                @suc @suc @suc zer
                @suc @suc @suc zer
    ");
    println!("-- Input (with original names):\n\n{}\n", &example);
    println!("-- Input:\n\n{}\n", term::from_net(&term::to_net(&example)));
    println!("-- Output:\n\n{}\n", term::reduce(&example));
}
