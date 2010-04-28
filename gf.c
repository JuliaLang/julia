/*
  Generic Functions
  . method table and lookup
  . GF constructor, add_method
  . dispatch
  . static parameter inference
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#ifndef NO_BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

static jl_methtable_t *new_method_table()
{
    jl_methtable_t *mt = (jl_methtable_t*)allocb(sizeof(jl_methtable_t));
    mt->mlist = NULL;
    mt->generics = NULL;
    return mt;
}

// takes arguments in the same order as jl_subtype()
typedef int (*jl_type_comparer_t)(jl_type_t *a, jl_type_t *b);

typedef int (*jl_argtuple_comparer_t)(jl_value_t **args, size_t n,
                                      jl_type_t *b);

// trivial linked list implementation for now
// TODO: pull out all the stops
jl_methlist_t *jl_method_list_assoc_args(jl_methlist_t *ml,
                                         jl_value_t **args, size_t n)
{
    while (ml != NULL) {
        if (jl_tuple_subtype(args, n, &jl_tupleref(ml->sig,0),
                             ((jl_tuple_t*)ml->sig)->length, 1, 0, 0))
            return ml;
        ml = ml->next;
    }
    return NULL;
}

static jl_tuple_t *flatten_pairs(jl_tuple_t *t)
{
    size_t i, n = 0;
    jl_tuple_t *t0 = t;
    while (t != jl_null) {
        n++;
        t = (jl_tuple_t*)jl_nextpair(t);
    }
    jl_tuple_t *nt = jl_alloc_tuple(n*2);
    t = t0;
    for(i=0; i < n*2; i+=2) {
        jl_tupleset(nt, i,   jl_t0(t));
        jl_tupleset(nt, i+1, jl_t1(t));
        t = (jl_tuple_t*)jl_nextpair(t);
    }
    return nt;
}

// return a new lambda-info that has some extra static parameters
// merged in.
jl_lambda_info_t *jl_add_static_parameters(jl_lambda_info_t *l, jl_tuple_t *sp)
{
    if (sp->length == 0)
        return l;
    if (l->sparams->length > 0)
        sp = jl_tuple_append(sp, l->sparams);
    jl_lambda_info_t *nli = jl_new_lambda_info(l->ast, sp);
    nli->fptr = l->fptr;
    return nli;
}

jl_function_t *jl_instantiate_method(jl_function_t *f, jl_tuple_t *sp)
{
    if (f->linfo == NULL)
        return f;
    jl_function_t *nf = jl_new_closure(f->fptr, f->env);
    if (f->env != NULL && jl_is_tuple(f->env) &&
        ((jl_tuple_t*)f->env)->length == 2 &&
        jl_t0(f->env) == (jl_value_t*)f) {
        nf->env = (jl_value_t*)jl_pair((jl_value_t*)nf, jl_t1(f->env));
    }
    nf->linfo = jl_add_static_parameters(f->linfo, sp);
    return nf;
}

static int exact_arg_match(jl_value_t **args, size_t n, jl_tuple_t *sig)
{
    assert(jl_is_tuple(sig));
    if (sig->length != n) return 0;
    size_t i;
    for(i=0; i < n; i++) {
        // note: because this uses jl_typeof() directly, it never detects
        // exact matches for tuples. however this is a conservative answer
        // given the rest of the dispatch process.
        if (!jl_types_equal((jl_value_t*)jl_typeof(args[i]), jl_tupleref(sig,i)))
            return 0;
    }
    return 1;
}

jl_methlist_t *jl_method_table_insert(jl_methtable_t *mt, jl_type_t *type,
                                      jl_function_t *method);

jl_methlist_t *jl_method_table_assoc(jl_methtable_t *mt,
                                     jl_value_t **args, size_t nargs)
{
    /*
      search order:
      look at concrete signatures
      if there is an exact match, return it
      otherwise look for a matching generic signature
      if no concrete or generic match, raise error
      if no generic match, use the concrete one even if inexact
      otherwise instantiate the generic method and use it
      
      TODO: cache exact matches in a faster lookup table
    */
    jl_methlist_t *m = jl_method_list_assoc_args(mt->mlist, args, nargs);
    if (m!=NULL && exact_arg_match(args, nargs, (jl_tuple_t*)m->sig))
        return m;
    
    // try generics
    jl_methlist_t *gm = mt->generics;
    jl_value_t *env = (jl_value_t*)jl_false;
    jl_tuple_t *tt = NULL;
    if (gm != NULL) {
        // TODO: avoid this allocation
        tt = jl_alloc_tuple(nargs);
        size_t i;
        for(i=0; i < tt->length; i++) {
            jl_tupleset(tt, i, (jl_value_t*)jl_full_type(args[i]));
        }
        while (gm != NULL) {
            env = jl_type_conform((jl_type_t*)tt, gm->sig);
            if (env != (jl_value_t*)jl_false) break;
            gm = gm->next;
        }
    }
    if (env == (jl_value_t*)jl_false) {
        if (m != NULL) {
            // TODO: possibly re-specialize method on inexact match
        }
        return m;
    }
    assert(tt!=NULL);

    // cache result in concrete part of method table
    assert(jl_is_tuple(env));
    jl_function_t *newmeth =
        jl_instantiate_method(gm->func,
                              flatten_pairs((jl_tuple_t*)env));
    return jl_method_table_insert(mt, (jl_type_t*)tt, newmeth);
}

static int args_match_generic(jl_type_t *a, jl_type_t *b)
{
    return (jl_type_conform_morespecific(a,b) != (jl_value_t*)jl_false);
}

static int args_match(jl_type_t *a, jl_type_t *b)
{
    return jl_type_morespecific((jl_value_t*)a,(jl_value_t*)b,0,0);
}

static int sigs_match(jl_type_t *a, jl_type_t *b)
{
    if (jl_has_typevars((jl_value_t*)a)) {
        if (jl_has_typevars((jl_value_t*)b)) {
            return jl_types_equal_generic((jl_value_t*)a,(jl_value_t*)b);
        }
        return 0;
    }
    return jl_types_equal((jl_value_t*)a, (jl_value_t*)b);
}

static
jl_methlist_t *jl_method_list_insert_p(jl_methlist_t **pml, jl_type_t *type,
                                       jl_function_t *method,
                                       jl_type_comparer_t pred)
{
    jl_methlist_t *l, **pl;

    assert(jl_is_tuple(type));
    l = *pml;
    while (l != NULL) {
        if (sigs_match(type, l->sig))
            break;
        l = l->next;
    }
    if (l != NULL) {
        // method overwritten
        // TODO: invalidate cached methods
        l->sig = type;
        l->func = method;
        return l;
    }
    jl_methlist_t *newrec = (jl_methlist_t*)allocb(sizeof(jl_methlist_t));
    newrec->sig = type;
    newrec->func = method;
    pl = pml;
    l = *pml;
    while (l != NULL) {
        if (pred(type, l->sig))
            break;
        pl = &l->next;
        l = l->next;
    }
    newrec->next = l;
    *pl = newrec;
    return newrec;
}

jl_methlist_t *jl_method_table_insert(jl_methtable_t *mt, jl_type_t *type,
                                      jl_function_t *method)
{
    /*
      TODO: warn about ambiguous method priorities
      
      the relative priority of A and B is ambiguous if
      !subtype(A,B) && !subtype(B,A) && no corresponding tuple
      elements are disjoint.
      
      for example, (Tensor, Matrix) and (Matrix, Tensor) are ambiguous.
      however, (Tensor, Matrix, Foo) and (Matrix, Tensor, Bar) are fine
      since Foo and Bar are disjoint, so there would be no confusion over
      which one to call.
    */
    if (jl_has_typevars((jl_value_t*)type)) {
        return jl_method_list_insert_p(&mt->generics, type, method,
                                       args_match_generic);
    }
    else {
        return jl_method_list_insert_p(&mt->mlist, type, method, args_match);
    }
}

JL_CALLABLE(jl_apply_generic)
{
    jl_methtable_t *mt = (jl_methtable_t*)jl_t0(env);

    jl_methlist_t *m = jl_method_table_assoc(mt, args, nargs);

    if (m == NULL) {
        jl_sym_t *name = (jl_sym_t*)jl_t1(env);
        jl_tuple_t *argt = (jl_tuple_t*)jl_f_tuple(NULL, args, nargs);
        char *argt_str = jl_print_to_string(jl_full_type((jl_value_t*)argt));
        jl_errorf("no method %s%s", name->name, argt_str);
    }

#if 0
    // TRACE
    ios_printf(ios_stdout, "%s(", ((jl_sym_t*)jl_t1(env))->name);
    size_t i;
    for(i=0; i < nargs; i++) {
        if (i > 0) ios_printf(ios_stdout, ", ");
        ios_printf(ios_stdout, "%s", jl_tname(jl_typeof(args[i]))->name->name);
    }
    ios_printf(ios_stdout, ")\n");
#endif

    return (m->func->fptr)(m->func->env, args, nargs);
}

void jl_print_method_table(jl_function_t *gf)
{
    assert(jl_is_gf(gf));
    char *name = ((jl_sym_t*)jl_t1(gf->env))->name;
    jl_methtable_t *mt = (jl_methtable_t*)jl_t0(gf->env);
    jl_methlist_t *ml = mt->mlist;
    while (ml != NULL) {
        ios_printf(ios_stdout, "%s", name);
        jl_print((jl_value_t*)ml->sig);
        ios_printf(ios_stdout, "\n");
        ml = ml->next;
    }
    ios_printf(ios_stdout, "generic:\n");
    ml = mt->generics;
    while (ml != NULL) {
        ios_printf(ios_stdout, "%s", name);
        jl_print((jl_value_t*)ml->sig);
        ios_printf(ios_stdout, "\n");
        ml = ml->next;
    }
}

jl_function_t *jl_new_generic_function(jl_sym_t *name)
{
    return jl_new_closure(jl_apply_generic,
                          (jl_value_t*)jl_pair((jl_value_t*)new_method_table(),
                                               (jl_value_t*)name));
}

void jl_add_method(jl_function_t *gf, jl_tuple_t *types, jl_function_t *meth)
{
    assert(jl_is_gf(gf));
    assert(jl_is_tuple(types));
    assert(jl_is_func(meth));
    (void)jl_method_table_insert(jl_gf_mtable(gf), (jl_type_t*)types, meth);
}
