# The Superposed Type Theory (SupTT)

The Superposed Type Theory (SupTT) is an extension of the Interaction Calculus
with a type system, labelled superpositions, and new primitives. Its grammar is:

```
Term ::=
  -- Variables
  | VAR: Name
  | LET: "!" Name "=" Term ";" Term
  -- Superpositions
  | SUP: "&" Uint "{" Term "," Term "}"
  | COL: "!" "&" Uint "{" Name "," Name "}" "=" Term ";" Term
  -- Universe Type
  | SET: "*"
  -- Empty Type
  | EMP: "⊥"
  | EFQ: "¬" Term
  -- Unit Type
  | UNI: "⊤"
  | NIL: "()"
  | USE: "-" Term ";" Term
  -- Bool Type
  | BIT: "𝔹"
  | B_0: "0"
  | B_1: "1"
  | ITE: "?" Term "{" Term "}" ";" "{" Term "}"
  -- Sigma Type
  | SIG: "Σ" Name ":" Term "." Term
  | TUP: "[" Term "," Term "]"
  | GET: "!" "[" Name "," Name "]" "=" Term ";" Term
  -- Pi Type
  | ALL: "Π" Name ":" Term "." Term
  | LAM: "λ" Name "." Term
  | APP: "(" Term " " Term ")"
  -- Identity Type
  | EQL: "<" Term "=" Term ">"
  | RFL: "θ"
  | RWT: "%" Term ";" Term
```

The Universe Type is the type of types.

The Empty Type is a set without elements. It is eliminated as `¬x`, which
corresponds to an empty pattern-match (agda: `case x of λ{}`).

The Unit Type is a set with one element, `()`. It is eliminated as `-x; body`,
which corresponds to an unitary pattern-match (agda: `case x of λ{(): body}`).

The Bool Type is a set with two elements, `0` and `1`. It is eliminated as `?x {
t } ; { f }`, which corresponds to a boolean pattern-match (agda: `case x of λ{
true → t ; false → f }`).

The Sigma Type is a dependent pair, constructed as `[a,b]`. It is eliminated by
the projection `! [a,b] = p; body`, which extracts `a` and `b` from `p`.

The Pi Type is a dependent function, constructed as `λx. body`. It is eliminated
by an application, `(f x)`, as discussed before.

The Identity Type represents a propositional equality, constructed by `θ`, which
stands for reflexivity. It is eliminated as `% e; body`, which corresponds to an
identity type pattern-match (agda: `case e of λ{ refl: body }`).

The computation rules (interactions) of SupTT are written below.

Constructors interact with their eliminators, as expected:

```
- () t
------ USE-NIL
t

? 0 { t } ; { f }
----------------- ITE-B_0
f

? 1 { t } ; { f }
----------------- ITE-B_1
t

! [x,y] = [a,b]; t
------------------ GET-TUP
x <- a
y <- b
t

% θ; t
------ RWT-RFL
t

(λx.f a)
-------- APP-LAM
x <- a
f
```

Constructors also interact with collapsers:

```
! &L{x0,x1} = (); K
------------------- COL-NIL
x0 <- ()
x1 <- ()
K

! &L{x0,x1} = 0; K
------------------ COL-B_0
x0 <- 0
x1 <- 0
K

! &L{x0,x1} = 1; K
------------------ COL-B_1
x0 <- 1
x1 <- 1
K

! &L{x0,x1} = [a,b]; K
---------------------- COL-TUP
x0 <- [a0,b0]
x1 <- [a1,b1]
! &L{a0,a1} = a
! &L{b0,b1} = b
K

! &L{r,s} = λx.f;
K
----------------- COL-LAM
r <- λx0.f0
s <- λx1.f1
x <- &L{x0,x1}
! &L{f0,f1} = f;
K
```

Eliminators interact with superpositions:

```
- &L{a,b}; k
-------------------- USE-SUP
! &L{k0,k1} = k;
&L{-a;k0, -b;k1}

? &L{a,b} {t} ; {f}
---------------------------- ITE-SUP
! &L{t0,t1} = t;
! &L{f0,f1} = f;
&L{?a{t0};{f0}, ?b{t1};{f1}}

! [x,y] = &L{a,b}; k
---------------------------- GET-SUP
! &L{k0,k1} = k;
&L{![x,y]=a;k0, ![x,y]=b;k1}

% &L{a,b}; k
---------------- RWT-SUP
! &L{k0,k1} = k;
&L{%a;k0, %b;k1}

(&L{a,b} c)
----------------- APP-SUP
! &L{c0,c1} = c;
&L{(a c0),(b c1)}
```

Superpositions and collapsers interact:

```
! &L{x,y} = &L{a,b};
K
-------------------- COL-SUP (if equal labels)
x <- a
y <- b
K

! &L{x,y} = &R{a,b};
K
-------------------- COL-SUP (if different labels)
x <- &R{a0,b0} 
y <- &R{a1,b1}
! &L{a0,a1} = a
! &L{b0,b1} = b
K
```

# SupTT's Runtime (32-Bit)

SupTT-32 is implemented in portable C.

It represents terms with u32-pointers, which store 3 fields:

- sub (1-bit): true if this is a substitution
- tag (5-bit): the term tag
- lab (2-bit): the label (optionally)
- val (24-bit): the value of this pointer

The tag field can be:

- VAR
- LET
- SUP
- CO0
- CO1
- SET
- EMP
- EFQ
- UNI
- NIL
- USE
- BIT
- B_0
- B_1
- ITE
- SIG
- TUP
- ALL
- LAM
- APP
- EQL
- RFL
- RWT

The lab field stores a label on SUP, CO0 and CO1 terms.

The val field depends on the label:

- VAR: points to a Subst location
- CO0: points to a Subst location
- CO1: points to a Subst location
- LET: points to a Let Node ({val: Term, bod: Term})
- SUP: points to a Sup Node ({lft: Term, rgt: Term})
- SET: unused
- EMP: unused
- EFQ: points to an Efq Node ({val: Term})
- UNI: unused
- NIL: unused
- USE: points to a Use Node ({val: Term, bod: Term})
- BIT: unused
- B_0: unused
- B_1: unused
- ITE: points to a Ite Node ({cnd: Term, thn: Term, els: Term})
- SIG: points to a Sig Node ({fst: Term, snd: Term})
- TUP: points to a Tup Node ({fst: Term, snd: Term})
- GET: points to a Get Node ({val: Term, bod: Term})
- ALL: points to an All Node ({inp: Term, out: Term})
- LAM: points to a Lam Node ({bod: Term})
- APP: points to an App Node ({fun: Term, arg: Term})
- EQL: points to an Eql Node ({lft: Term, rgt: Term})
- RFL: unused
- RWT: points to a Rwt Node ({val: Term, bod: Term})

Non-nullary terms point to a Node, i.e., a consecutive block of its child terms.
For example, the SUP term points to the memory location where its two child
terms are stored.

Variable terms (VAR, CO0 and CO1) point to a location in memory where the
substitution will be inserted. As an optimization, rather than keeping a
separate subst map in memory, we just re-use the location of the corresponding
binder. For example, the VAR term of a lambda points either to its corresponding
Lam Node (before the beta reduction), or to the substituted term (after the beta
reduction). To distinguish, we set the 'sub' bit to signal that a memory
location is a substitution entry.

Note that there is no COL term. That's because collapser nodes are special:
1. they aren't part of the AST
2. they don't store a body
3. their bound vars are represented by CO0 and CO1 instead of VAR
Because of that, the only way to access them is via the CO0 and CO1 terms, which
point to the collapser node. When a collapse interaction takes place, the bound
var that triggered the interaction immediatelly gets its half of the collapse,
and the collapser node is replaced by the other half, with the sub bit set,
allowing the other bound var to get it. For example, the COL-SUP interaction
could be implemented as:

```
def col_sup(col, sup):
  if col.lab == sup.lab:
    tm0 = heap[sup.loc + 0]
    tm1 = heap[sup.loc + 1]
    heap[col.loc] = as_sub(tm1 if col.tag == CO0 else tm0)
    return (tm0 if col.tag == CO0 else tm1)
  else:
    co0_loc = alloc(1)
    co1_loc = alloc(1)
    su0_loc = alloc(2)
    su1_loc = alloc(2)
    su0_val = Term(SUP, sup.lab, su0_loc)
    su1_val = Term(SUP, sup.lab, su1_loc)
    heap[co0_loc] = heap[sup.loc + 0]
    heap[co1_loc] = heap[sup.loc + 1]
    heap[su0_loc + 0] = Term(DP0, col.lab, co0_loc)
    heap[su0_loc + 1] = Term(DP0, col.lab, co1_loc)
    heap[su1_loc + 0] = Term(DP1, col.lab, co0_loc)
    heap[su1_loc + 1] = Term(DP1, col.lab, co1_loc)
    heap[col.loc] = as_sub(su1_val if col.tag == CO0 else tm1)
    return (su0_val if col.tag == CO0 else su1_val)
```

Note how the var (CO0 or CO1) that triggers col_sup is given one of the half of
the collapse, while the other half is stored on the collapser node memory
location, now reinterpreted as a subst entry, allowing the other var to get it.

# Parsing SupTT

On SupTT, all bound variables have global range. For example, consider the term:

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

```
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
  
def parse_col(loc):
  col = alloc(1)
  consume("!")
  consume("&")
  lab = parse_uint()
  consume("{")
  co0 = parse_name()
  consume(",")
  co1 = parse_name()
  consume("}")
  consume("=")
  val = parse_term(col)
  bod = parse_term(loc)
  vars[co0] = Term(CO0, lab, loc)
  vars[co1] = Term(CO1, lab, loc)
```


