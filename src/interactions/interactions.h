#ifndef INTERACTIONS_H
#define INTERACTIONS_H

#include "../types.h"

// Core interaction rules
Term app_lam(Term app, Term lam);
Term app_sup(Term app, Term sup);
Term col_sup(Term col, Term sup);
Term col_lam(Term col, Term lam);

// Constructor-collapser interactions
Term col_nil(Term col, Term nil);
Term col_b_0(Term col, Term b_0);
Term col_b_1(Term col, Term b_1);
Term col_tup(Term col, Term tup);

// Eliminator interactions
Term use_nil(Term use, Term nil);
Term use_sup(Term use, Term sup);
Term ite_b0(Term ite, Term b_0);
Term ite_b1(Term ite, Term b_1);
Term ite_sup(Term ite, Term sup);
Term get_tup(Term get, Term tup);
Term get_sup(Term get, Term sup);
Term rwt_rfl(Term rwt, Term rfl);
Term rwt_sup(Term rwt, Term sup);

#endif // INTERACTIONS_H