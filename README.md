# Interaction Calculus

A C implementation of the Interaction Calculus.

For more info, read the [Spec](./InteractionCalculus.md).

TODO:
- implement a mini hashmaps (for parser, stringifier, and more)
- add a tail call primitive Taelin
- complete the metal implementation
- add a garbage collector → freelist / collect() (trigger LAM-ERA) Nicolas
- adicionar tipos (Uni, Sum, Mul, Fix) -> rec struct em C! malloc()
- adicionar o collapse() da HVM3 (no lugar normal())
- adicionar o equal() : Type → Term → Type → Term → Bool
- adicionar o enumerator : (id : u32) → (typ : Type) → Term 
- adicionar a versão Metal
