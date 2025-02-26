#include "../whnf.h"
#include "../memory.h"

// Implementation of COL-NIL interaction: !&L{x0,x1}=(); K -> x0<-(); x1<-(); K
Term col_nil(Term col, Term nil) {
  uint32_t col_loc = TERM_VAL(col);
  
  // Store () as substitution for the other half of the collapser
  heap[col_loc] = make_sub(nil);
  
  // Return () for this variable
  return nil;
}

// Implementation of COL-B_0 interaction: !&L{x0,x1}=0; K -> x0<-0; x1<-0; K
Term col_b_0(Term col, Term b_0) {
  uint32_t col_loc = TERM_VAL(col);
  
  // Store 0 as substitution for the other half of the collapser
  heap[col_loc] = make_sub(b_0);
  
  // Return 0 for this variable
  return b_0;
}

// Implementation of COL-B_1 interaction: !&L{x0,x1}=1; K -> x0<-1; x1<-1; K
Term col_b_1(Term col, Term b_1) {
  uint32_t col_loc = TERM_VAL(col);
  
  // Store 1 as substitution for the other half of the collapser
  heap[col_loc] = make_sub(b_1);
  
  // Return 1 for this variable
  return b_1;
}

// Implementation of COL-TUP interaction:
// !&L{x0,x1}=[a,b]; K -> x0<-[a0,b0]; x1<-[a1,b1]; !&L{a0,a1}=a; !&L{b0,b1}=b; K
Term col_tup(Term col, Term tup) {
  uint8_t col_lab = TERM_LAB(col);
  uint32_t col_loc = TERM_VAL(col);
  uint32_t tup_loc = TERM_VAL(tup);
  
  // Get tuple components
  Term fst = heap[tup_loc];
  Term snd = heap[tup_loc + 1];
  
  // Create new locations for components and tuples
  uint32_t a0_loc = alloc(1);
  uint32_t a1_loc = alloc(1);
  uint32_t b0_loc = alloc(1);
  uint32_t b1_loc = alloc(1);
  uint32_t tup0_loc = alloc(2);
  uint32_t tup1_loc = alloc(2);
  
  // Set up collapsers for components
  heap[a0_loc] = fst;
  heap[b0_loc] = snd;
  
  // Create new tuples
  heap[tup0_loc] = make_term(CO0, col_lab, a0_loc);
  heap[tup0_loc + 1] = make_term(CO0, col_lab, b0_loc);
  
  heap[tup1_loc] = make_term(CO1, col_lab, a0_loc);
  heap[tup1_loc + 1] = make_term(CO1, col_lab, b0_loc);
  
  Term tup0 = make_term(TUP, 0, tup0_loc);
  Term tup1 = make_term(TUP, 0, tup1_loc);
  
  // Store the appropriate tuple as substitution based on which col var we are
  heap[col_loc] = make_sub(TERM_TAG(col) == CO0 ? tup1 : tup0);
  
  // Return the appropriate tuple for this variable
  return TERM_TAG(col) == CO0 ? tup0 : tup1;
}

// Implementation of USE-NIL interaction: -(); t -> t
Term use_nil(Term use, Term nil) {
  uint32_t use_loc = TERM_VAL(use);
  
  // Return the body of the use expression
  return heap[use_loc + 1];
}

// Implementation of USE-SUP interaction: -&L{a,b}; k -> !&L{k0,k1}=k; &L{-a;k0, -b;k1}
Term use_sup(Term use, Term sup) {
  uint32_t use_loc = TERM_VAL(use);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  
  // Get the body of use and components of superposition
  Term body = heap[use_loc + 1];
  Term lft = heap[sup_loc];
  Term rgt = heap[sup_loc + 1];
  
  // Create locations for new terms
  uint32_t k0_loc = alloc(1);
  uint32_t k1_loc = alloc(1);
  uint32_t use0_loc = alloc(2);
  uint32_t use1_loc = alloc(2);
  uint32_t new_sup_loc = alloc(2);
  
  // Store body in collapsers
  heap[k0_loc] = body;
  
  // Create new use expressions
  heap[use0_loc] = lft;
  heap[use0_loc + 1] = make_term(CO0, sup_lab, k0_loc);
  
  heap[use1_loc] = rgt;
  heap[use1_loc + 1] = make_term(CO1, sup_lab, k0_loc);
  
  // Create superposition of use expressions
  heap[new_sup_loc] = make_term(USE, 0, use0_loc);
  heap[new_sup_loc + 1] = make_term(USE, 0, use1_loc);
  
  // Return the superposition
  return make_term(SUP, sup_lab, new_sup_loc);
}

// Implementation of ITE-B0 interaction: ?0{t};{f} -> f
Term ite_b0(Term ite, Term b_0) {
  uint32_t ite_loc = TERM_VAL(ite);
  
  // Return the else branch
  return heap[ite_loc + 2];
}

// Implementation of ITE-B1 interaction: ?1{t};{f} -> t
Term ite_b1(Term ite, Term b_1) {
  uint32_t ite_loc = TERM_VAL(ite);
  
  // Return the then branch
  return heap[ite_loc + 1];
}

// Implementation of ITE-SUP interaction:
// ?&L{a,b}{t};{f} -> !&L{t0,t1}=t; !&L{f0,f1}=f; &L{?a{t0};{f0}, ?b{t1};{f1}}
Term ite_sup(Term ite, Term sup) {
  uint32_t ite_loc = TERM_VAL(ite);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  
  // Get components
  Term lft = heap[sup_loc];
  Term rgt = heap[sup_loc + 1];
  Term thn = heap[ite_loc + 1];
  Term els = heap[ite_loc + 2];
  
  // Create locations for new terms
  uint32_t t0_loc = alloc(1);
  uint32_t t1_loc = alloc(1);
  uint32_t f0_loc = alloc(1);
  uint32_t f1_loc = alloc(1);
  uint32_t ite0_loc = alloc(3);
  uint32_t ite1_loc = alloc(3);
  uint32_t new_sup_loc = alloc(2);
  
  // Set up collapsers
  heap[t0_loc] = thn;
  heap[f0_loc] = els;
  
  // Create new ite expressions
  heap[ite0_loc] = lft;
  heap[ite0_loc + 1] = make_term(CO0, sup_lab, t0_loc);
  heap[ite0_loc + 2] = make_term(CO0, sup_lab, f0_loc);
  
  heap[ite1_loc] = rgt;
  heap[ite1_loc + 1] = make_term(CO1, sup_lab, t0_loc);
  heap[ite1_loc + 2] = make_term(CO1, sup_lab, f0_loc);
  
  // Create superposition of ite expressions
  heap[new_sup_loc] = make_term(ITE, 0, ite0_loc);
  heap[new_sup_loc + 1] = make_term(ITE, 0, ite1_loc);
  
  // Return the superposition
  return make_term(SUP, sup_lab, new_sup_loc);
}

// Implementation of GET-TUP interaction: ![x,y]=[a,b]; t -> x<-a; y<-b; t
Term get_tup(Term get, Term tup) {
  uint32_t get_loc = TERM_VAL(get);
  uint32_t tup_loc = TERM_VAL(tup);
  
  // Get components
  Term fst = heap[tup_loc];
  Term snd = heap[tup_loc + 1];
  
  // Get variables from getter
  Term x = heap[get_loc];
  Term y = heap[get_loc + 1];
  
  // Get projection body
  Term body = heap[get_loc + 3];
  
  // Create substitutions for the variables
  uint32_t x_loc = TERM_VAL(x);
  uint32_t y_loc = TERM_VAL(y);
  
  heap[x_loc] = make_sub(fst);
  heap[y_loc] = make_sub(snd);
  
  // Return the body
  return body;
}

// Implementation of GET-SUP interaction:
// ![x,y]=&L{a,b}; k -> !&L{k0,k1}=k; &L{![x,y]=a;k0, ![x,y]=b;k1}
Term get_sup(Term get, Term sup) {
  uint32_t get_loc = TERM_VAL(get);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  
  // Get components
  Term x = heap[get_loc];
  Term y = heap[get_loc + 1];
  Term body = heap[get_loc + 3];
  Term lft = heap[sup_loc];
  Term rgt = heap[sup_loc + 1];
  
  // Create locations for new terms
  uint32_t k0_loc = alloc(1);
  uint32_t k1_loc = alloc(1);
  uint32_t get0_loc = alloc(4);
  uint32_t get1_loc = alloc(4);
  uint32_t new_sup_loc = alloc(2);
  
  // Set up collapsers for body
  heap[k0_loc] = body;
  
  // Create new get expressions
  heap[get0_loc] = x;
  heap[get0_loc + 1] = y;
  heap[get0_loc + 2] = lft;
  heap[get0_loc + 3] = make_term(CO0, sup_lab, k0_loc);
  
  heap[get1_loc] = x;
  heap[get1_loc + 1] = y;
  heap[get1_loc + 2] = rgt;
  heap[get1_loc + 3] = make_term(CO1, sup_lab, k0_loc);
  
  // Create superposition of get expressions
  heap[new_sup_loc] = make_term(GET, 0, get0_loc);
  heap[new_sup_loc + 1] = make_term(GET, 0, get1_loc);
  
  // Return the superposition
  return make_term(SUP, sup_lab, new_sup_loc);
}

// Implementation of RWT-RFL interaction: %θ; t -> t
Term rwt_rfl(Term rwt, Term rfl) {
  uint32_t rwt_loc = TERM_VAL(rwt);
  
  // Return the body
  return heap[rwt_loc + 1];
}

// Implementation of RWT-SUP interaction: %&L{a,b}; k -> !&L{k0,k1}=k; &L{%a;k0, %b;k1}
Term rwt_sup(Term rwt, Term sup) {
  uint32_t rwt_loc = TERM_VAL(rwt);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  
  // Get components
  Term body = heap[rwt_loc + 1];
  Term lft = heap[sup_loc];
  Term rgt = heap[sup_loc + 1];
  
  // Create locations for new terms
  uint32_t k0_loc = alloc(1);
  uint32_t k1_loc = alloc(1);
  uint32_t rwt0_loc = alloc(2);
  uint32_t rwt1_loc = alloc(2);
  uint32_t new_sup_loc = alloc(2);
  
  // Set up collapsers for body
  heap[k0_loc] = body;
  
  // Create new rwt expressions
  heap[rwt0_loc] = lft;
  heap[rwt0_loc + 1] = make_term(CO0, sup_lab, k0_loc);
  
  heap[rwt1_loc] = rgt;
  heap[rwt1_loc + 1] = make_term(CO1, sup_lab, k0_loc);
  
  // Create superposition of rwt expressions
  heap[new_sup_loc] = make_term(RWT, 0, rwt0_loc);
  heap[new_sup_loc + 1] = make_term(RWT, 0, rwt1_loc);
  
  // Return the superposition
  return make_term(SUP, sup_lab, new_sup_loc);
}