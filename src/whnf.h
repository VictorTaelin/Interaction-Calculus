#ifndef WHNF_H
#define WHNF_H

#include "types.h"

// Global interaction counter
extern uint64_t interaction_count;

// Function declarations for interactions
Term app_lam(Term app, Term lam);
Term app_sup(Term app, Term sup);
Term col_lam(Term col, Term lam);
Term col_sup(Term col, Term sup);

// Reduce a term to weak head normal form
Term whnf(Term term);

#endif // WHNF_H
