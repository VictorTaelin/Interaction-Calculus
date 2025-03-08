//./collapse.c//

#ifndef IC_COLLAPSE_H
#define IC_COLLAPSE_H

#include "ic.h"

static inline Term ic_era_lam(IC* ic, Term lam, Term era);
static inline Term ic_era_app(IC* ic, Term app, Term era);
static inline Term ic_sup_lam(IC* ic, Term lam, Term sup);
static inline Term ic_sup_app(IC* ic, Term app, Term sup);
static inline Term ic_sup_sup_x(IC* ic, Term outer_sup, Term inner_sup);
static inline Term ic_sup_sup_y(IC* ic, Term outer_sup, Term inner_sup);
static inline Term ic_dup_var(IC* ic, Term dup, Term var);
static inline Term ic_dup_app(IC* ic, Term dup, Term app);

Term ic_collapse_sups(IC* ic, Term term);
Term ic_collapse_dups(IC* ic, Term term);

#endif // IC_COLLAPSE_H
