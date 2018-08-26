mod term;
mod net;

fn main() {
    let prose = term::from_string(b"
        @f1x f1y f
        @F1x F1y f1x
        @f2x f2y #x2 :F1x :F1y x2
        @F2x F2y f2x
        @f4x f4y #x4 :F2x :F2y x4
        @F4x F4y f4x
        @f8x f8y #x8 :F4x :F4y x8
        ::#f #x :f1y :f4y :f8y x 
          #b0 #T0 #F0 ::b0 F0 T0
          #T #F T
    ");
    println!("-- Input:\n{}", prose);
    println!("-- Net:\n{:?}", term::to_net(&prose));
    println!("-- Input:\n{}", term::from_net(&term::to_net(&prose)));
    println!("-- Normal:\n{}", term::reduce(&prose));
}
