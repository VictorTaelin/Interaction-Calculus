#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"
#include "memory.h"
#include "whnf.h"
#include "show.h"

// Create the default test term programmatically:
// ((λf.λx.!{f0,f1}=f;(f0 (f1 x)) λB.λT.λF.((B F) T)) λa.λb.a)
// This term tests multiple interactions and evaluates to λa.λb.a
Term create_default_test_term() {
  // Allocate memory for the various parts of the term
  
  // Create the innermost term (λa.λb.a)
  uint32_t lam_a = alloc(1); // For lambda body
  uint32_t lam_b = alloc(1); // For the innermost result (a)
  
  // Create λa.λb.a
  heap[lam_b] = make_term(VAR, 0, lam_a); // 'a' variable
  Term lam_b_term = make_term(LAM, 0, lam_b); // λb.a
  heap[lam_a] = lam_b_term;
  Term lam_a_term = make_term(LAM, 0, lam_a); // λa.λb.a
  
  // Create λB.λT.λF.((B F) T)
  uint32_t lam_B = alloc(1); // For lambda body
  uint32_t lam_T = alloc(1); // For lambda body
  uint32_t lam_F = alloc(1); // For lambda body
  uint32_t app_inner = alloc(2); // For (B F)
  uint32_t app_outer = alloc(2); // For ((B F) T)
  
  // Build ((B F) T)
  heap[app_inner] = make_term(VAR, 0, lam_B); // B variable
  heap[app_inner + 1] = make_term(VAR, 0, lam_F); // F variable
  Term app_inner_term = make_term(APP, 0, app_inner);
  
  heap[app_outer] = app_inner_term;
  heap[app_outer + 1] = make_term(VAR, 0, lam_T); // T variable
  Term app_outer_term = make_term(APP, 0, app_outer);
  
  // Build λF.((B F) T)
  heap[lam_F] = app_outer_term;
  Term lam_F_term = make_term(LAM, 0, lam_F);
  
  // Build λT.λF.((B F) T)
  heap[lam_T] = lam_F_term;
  Term lam_T_term = make_term(LAM, 0, lam_T);
  
  // Build λB.λT.λF.((B F) T)
  heap[lam_B] = lam_T_term;
  Term lam_B_term = make_term(LAM, 0, lam_B);
  
  // Create the collapse variables and body
  uint32_t col_node = alloc(3); // For f0, f1, and body
  heap[col_node] = make_term(VAR, 0, 0); // f0 (placeholder)
  heap[col_node + 1] = make_term(VAR, 0, 0); // f1 (placeholder)
  
  // Build (f0 (f1 x))
  uint32_t app_f1_x = alloc(2); // For (f1 x)
  uint32_t app_f0_result = alloc(2); // For (f0 (f1 x))
  
  // Build (f1 x)
  heap[app_f1_x] = make_term(CO1, 0, col_node); // f1 collapser variable
  heap[app_f1_x + 1] = make_term(VAR, 0, 0); // x (placeholder)
  Term app_f1_x_term = make_term(APP, 0, app_f1_x);
  
  // Build (f0 (f1 x))
  heap[app_f0_result] = make_term(CO0, 0, col_node); // f0 collapser variable
  heap[app_f0_result + 1] = app_f1_x_term;
  Term app_f0_result_term = make_term(APP, 0, app_f0_result);
  
  // Store the result in the collapse node
  heap[col_node + 2] = app_f0_result_term;
  
  // Build λx.!{f0,f1}=f;(f0 (f1 x))
  uint32_t lam_x = alloc(1); // For λx body
  heap[lam_x] = make_term(LET, 0, col_node); // LET f0, f1 = f in (f0 (f1 x))
  Term lam_x_term = make_term(LAM, 0, lam_x);
  
  // Build λf.λx.!{f0,f1}=f;(f0 (f1 x))
  uint32_t lam_f = alloc(1); // For λf body
  heap[lam_f] = lam_x_term;
  Term lam_f_term = make_term(LAM, 0, lam_f);
  
  // Build the outermost application
  uint32_t app_outer_most = alloc(2);
  heap[app_outer_most] = lam_f_term;
  heap[app_outer_most + 1] = lam_B_term;
  Term app_outer_most_term = make_term(APP, 0, app_outer_most);
  
  // Build the final term ((λf.λx.!{f0,f1}=f;(f0 (f1 x)) λB.λT.λF.((B F) T)) λa.λb.a)
  uint32_t app_final = alloc(2);
  heap[app_final] = app_outer_most_term;
  heap[app_final + 1] = lam_a_term;
  Term app_final_term = make_term(APP, 0, app_final);
  
  return app_final_term;
}

int main(int argc, char* argv[]) {
  // Initialize memory
  init_memory();
  
  // Create the default test term
  Term term = create_default_test_term();
  
  printf("Original term:\n");
  show_term(stdout, term);
  printf("\n\n");
  
  // Normalize the term
  term = normal(term);
  
  printf("Normal form:\n");
  show_term(stdout, term);
  printf("\n\n");
  
  printf("Total interactions: %llu\n", interaction_count);
  
  return 0;
}
