/*****************************************************************
  File     : swi_robdd.c
  Author   : Peter Schachte
  Purpose  : Interface robdd package to SWI Prolog
  Copyright: (c) 2017 Peter Schachte.  All rights reserved.

*****************************************************************/

#include <SWI-Prolog.h>
#include "bryantPrint.h"
#include <SWI-Stream.h> /* for debugging printfs */
#include "bryant.h"

#define VAR_LIMIT 256     /* max number of variables we can handle in a
                           * list or as term arguments.
                           */

static foreign_t
pl_max_variable(term_t num_term) {
  return PL_unify_integer(num_term, max_variable());
}


static foreign_t
pl_init_rep() {
  initRep();
  PL_succeed;
}


static foreign_t
pl_conclude_rep() {
  concludeRep();
  PL_succeed;
}


static foreign_t
pl_bdd_variable(term_t var_term, term_t bdd) {
  int var;
  if (PL_get_integer(var_term, &var)) {
    node *nd = variableRep(var);
    return PL_unify_pointer(bdd, nd);
  } else {
    PL_fail;
  }
}


static foreign_t
pl_bdd_true(term_t bdd) {
  return PL_unify_pointer(bdd, (void *)TRUE);
}


static foreign_t
pl_bdd_false(term_t bdd) {
  return PL_unify_pointer(bdd, (void *)FALSE);
}


static foreign_t
pl_bdd_and(term_t f, term_t g, term_t result_term) {
  void *f_nd, *g_nd;
  if (PL_is_integer(f)
      && PL_is_integer(g)
      && PL_get_pointer(f, &f_nd)
      && PL_get_pointer(g, &g_nd)) {
    node *result = glb((node *)f_nd, (node *)g_nd);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


static foreign_t
pl_bdd_or(term_t f, term_t g, term_t result_term) {
  void *f_nd, *g_nd;
  if (PL_is_integer(f)
      && PL_is_integer(g)
      && PL_get_pointer(f, &f_nd)
      && PL_get_pointer(g, &g_nd)) {
    node *result = lub((node *)f_nd, (node *)g_nd);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


static foreign_t
pl_bdd_implies(term_t f, term_t g, term_t result_term) {
  void *f_nd, *g_nd;
  if (PL_is_integer(f)
      && PL_is_integer(g)
      && PL_get_pointer(f, &f_nd)
      && PL_get_pointer(g, &g_nd)) {
    node *result = implies((node *)f_nd, (node *)g_nd);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


/* XXX We don't actually use this for groundness analysis.
 * static foreign_t
 * pl_bdd_ite(term_t f, term_t g, term_t h, term_t result_term) {
 *   void *f_nd, *g_nd, *h_nd;
 *   if (PL_is_integer(f)
 *       && PL_is_integer(g)
 *       && PL_is_integer(h)
 *       && PL_get_pointer(f, &f_nd)
 *       && PL_get_pointer(g, &g_nd)
 *       && PL_get_pointer(h, &h_nd)) {
 *     node *result = (void *) ite((node *)f, (node *)g, (node *)h);
 *     return PL_unify_pointer(result_term, result);
 *   } else {
 *     PL_fail;
 *   }
 * }
 */


static foreign_t
pl_bdd_project(term_t thresh_term, term_t f, term_t result_term) {
  void *f_nd;
  int thresh;
  if (   PL_get_pointer(f, &f_nd)
      && PL_get_integer(thresh_term, &thresh)) {
    node *result = projectThresh(thresh, (node *)f_nd);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


static foreign_t
pl_bdd_projected_and(term_t thresh_term, term_t f, term_t g, term_t result_term) {
  void *f_nd, *g_nd;
  int thresh;
  if (   PL_get_pointer(f, &f_nd)
      && PL_get_pointer(g, &g_nd)
      && PL_get_integer(thresh_term, &thresh)) {
    node *result = projected_glb(thresh, (node *)f_nd, (node *)g_nd);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


static foreign_t
pl_bdd_print(term_t bdd) {
  void *nd;
  if (PL_is_integer(bdd)
      && PL_get_pointer(bdd, &nd)) {
    printOut(stdout, (node *)nd);
    PL_succeed;
  } else {
    PL_fail;
  }
}


static foreign_t
pl_bdd_print_stderr(term_t bdd) {
  void *nd;
  if (PL_is_integer(bdd)
      && PL_get_pointer(bdd, &nd)) {
    printOut(stderr, (node *)nd);
    PL_succeed;
  } else {
    PL_fail;
  }
}


/* forward declarations to keep C happy */
static int list_to_array(term_t l, int arr[], int *n);
static int explode_term(term_t term, int arr[], int *n);


static foreign_t
pl_bdd_and_vars(term_t list, term_t result_term) {
  int n = VAR_LIMIT;
  int arr[VAR_LIMIT];
  if (list_to_array(list, arr, &n)) {
    node *result = glb_array(n, arr);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


static foreign_t
pl_rename_term(term_t bdd, term_t renaming, term_t result_term) {
  int n = VAR_LIMIT;
  int mapping[VAR_LIMIT];
  void *nd;
  if (PL_is_integer(bdd)
      && PL_get_pointer(bdd, &nd)
      && explode_term(renaming, mapping, &n)) {
    node *result = renameArray(nd, n, mapping);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


static foreign_t
pl_reverse_rename_term(term_t bdd_term, term_t renaming, term_t result_term) {
  int n = VAR_LIMIT;
  int mapping[VAR_LIMIT];
  void *nd;
  if (PL_is_integer(bdd_term)
      && PL_get_pointer(bdd_term, &nd)
      && explode_term(renaming, mapping, &n)) {
    node *result = reverseRenameArray(nd, n, mapping);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


static foreign_t
pl_iff_conj_list(term_t var_term, term_t list_term, term_t result_term) {
  int n = VAR_LIMIT;
  int arr[VAR_LIMIT];
  int var;
  if (   PL_get_integer(var_term, &var)
      && list_to_array(list_term, arr, &n)) {
    node *result = iff_conj_array(var, n, arr);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


static foreign_t
pl_abstract_unify_list(term_t context_term, term_t v0_term, term_t list_term,
                      term_t thresh_term, term_t result_term) {
  void *context;
  int v0;
  int n = VAR_LIMIT;
  int arr[VAR_LIMIT];
  int thresh;
  if (   PL_get_pointer(context_term, &context)
      && PL_get_integer(v0_term, &v0)
      && PL_get_integer(thresh_term, &thresh)
      && list_to_array(list_term, arr, &n)) {
    node *result = abstract_unify(context, v0, n, arr, thresh);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


static foreign_t
pl_abstract_exit_term(term_t context_term, term_t f_term, term_t term,
                      term_t thresh_term, term_t result_term) {
  int n = VAR_LIMIT;
  int arr[VAR_LIMIT];
  int thresh;
  void *context;
  void *f;
  if (   PL_get_pointer(context_term, &context)
      && PL_get_pointer(f_term, &f)
      && PL_get_integer(thresh_term, &thresh)
      && explode_term(term, arr, &n)) {
    node *result = abstract_exit(context, f, n, arr, thresh, identity_renaming);
    return PL_unify_pointer(result_term, (void *)result);
  } else {
    PL_fail;
  }
}


/* Copy Prolog list of ints l into a array arr of length n.  Return FALSE if
 * any element of the list is not an integer, or if the list is too long;
 * otherwise return TRUE and reassign n to the actual length of the list.
 */
static int
list_to_array(term_t list_term, int arr[], int *n) {
  term_t head = PL_new_term_ref();   /* the elements */
  term_t list = PL_copy_term_ref(list_term); /* copy (we modify list) */
  int i = 0;
  while( PL_get_list(list, head, list) ) {
    int num;
    if (i > *n) return FALSE;
    if ( PL_get_integer(head, &num) ) {
      arr[i++] = num;
    } else {
      return FALSE;
    }
  }
  *n = i;
  return PL_get_nil(list);            /* test end for [] */
}


/* Copy Prolog term term, all of whose arguments are ints, into array arr
 * of length *n at indices 1 - arity, and set *n to the arity.  Note that
 * we never use array slot 0.  Return FALSE if term is not a compound term
 * (or an atom), or any argument is not an integer, or if the arity is
 * greater or equal to *n.
 */
static int
explode_term(term_t term, int arr[], int *n) {
  atom_t name;
  int arity;
  int i = 1;
  if( PL_get_name_arity(term, &name, &arity)
      && arity < *n) {
    for (i=1; i<=arity; ++i) {
      term_t arg_term = PL_new_term_ref();
      int num;
      if ( !(   PL_get_arg(i, term, arg_term)
                && PL_get_integer(arg_term, &num)) ) {
        return FALSE;
      }
      arr[i] = num;
    }
    *n = arity;
    return TRUE;
  } else {
    return FALSE;
  }
}


install_t
install_swi_robdd() {
  PL_register_foreign("max_variable", 1, pl_max_variable, 0);
  PL_register_foreign("init_rep", 0, pl_init_rep, 0);
  PL_register_foreign("conclude_rep", 0, pl_conclude_rep, 0);
  PL_register_foreign("variable_rep", 2, pl_bdd_variable, 0);
  PL_register_foreign("anz_top", 1, pl_bdd_true, 0);
  PL_register_foreign("anz_bottom", 1, pl_bdd_false, 0);
  PL_register_foreign("anz_meet", 3, pl_bdd_and, 0);
  PL_register_foreign("anz_join", 3, pl_bdd_or, 0);
  PL_register_foreign("anz_implies", 3, pl_bdd_implies, 0);
  /* PL_register_foreign("bdd_ite", 4, pl_bdd_ite, 0); */
  PL_register_foreign("project_threshold", 3, pl_bdd_project, 0);
  PL_register_foreign("anz_meet", 4, pl_bdd_projected_and, 0);
  PL_register_foreign("anz_print", 1, pl_bdd_print, 0);
  PL_register_foreign("anz_print_stderr", 1, pl_bdd_print_stderr, 0);
  PL_register_foreign("anz_meet_vars", 2, pl_bdd_and_vars, 0);
  PL_register_foreign("rename_term", 3, pl_rename_term, 0);
  PL_register_foreign("reverse_rename_term", 3, pl_reverse_rename_term, 0);
  PL_register_foreign("anz_iffconj", 3, pl_iff_conj_list, 0);
  PL_register_foreign("abstract_unify_list", 5, pl_abstract_unify_list, 0);
  PL_register_foreign("analyze_call", 5, pl_abstract_exit_term, 0);
}
