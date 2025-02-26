#ifndef WHNF_H
#define WHNF_H

#include "types.h"

// Global interaction counter
extern uint64_t interaction_count;

// Reduce a term to weak head normal form
Term whnf(Term term);

// Pi Type interactions
Term app_lam(Term app, Term lam);
Term app_sup(Term app, Term sup);
Term col_lam(Term col, Term lam);

// Superposition interactions
Term col_sup(Term col, Term sup);

// Unit Type interactions
Term col_nil(Term col, Term nil);
Term use_nil(Term use, Term nil);
Term use_sup(Term use, Term sup);

// Bool Type interactions
Term col_b_0(Term col, Term b_0);
Term col_b_1(Term col, Term b_1);
Term ite_b_0(Term ite, Term b_0);
Term ite_b_1(Term ite, Term b_1);
Term ite_sup(Term ite, Term sup);

// Sigma Type interactions
Term col_tup(Term col, Term tup);
Term get_tup(Term get, Term tup);
Term get_sup(Term get, Term sup);

// Identity Type interactions
Term rwt_rfl(Term rwt, Term rfl);
Term rwt_sup(Term rwt, Term sup);

#endif // WHNF_H