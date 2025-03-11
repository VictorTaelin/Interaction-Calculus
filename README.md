# Interaction Calculus

A C implementation of the Interaction Calculus.

For more info, read the Spec.

TODO:

- Implement a mini hashmap (for parser, stringifier, and more).
- Add a tail call primitive (Taelin).
- Complete the metal implementation.
- Add a garbage collector → freelist / collect() (trigger LAM-ERA) (Nicolas).
- Add types (Uni, Sum, Mul, Fix) → recursive struct in C! malloc().
- Add the collapse() from HVM3 (replacing normal()).
- Add equal() : Type → Term → Type → Term → Bool.
- Add the enumerator : (id : u32) → (typ : Type) → Term.
- Add the Metal version.
