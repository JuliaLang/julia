/*
  Generic Functions
  . method table and lookup
  . GF constructor, add_method
  . dispatch
  . static parameter inference
  . method specialization, invoking type inference
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
#include "julia.h"

static jl_methtable_t *new_method_table()
{
    jl_methtable_t *mt = (jl_methtable_t*)allocobj(sizeof(jl_methtable_t));
    mt->type = (jl_type_t*)jl_methtable_type;
    mt->defs = NULL;
    mt->cache = NULL;
    mt->cache_1arg = NULL;
    mt->sealed = 0;
    mt->max_args = 0;
#ifdef JL_GF_PROFILE
    mt->ncalls = 0;
#endif
    return mt;
}

static int cache_match_by_type(jl_value_t **types, size_t n, jl_tuple_t *sig,
                               int va)
{
    if (!va && n > sig->length)
        return 0;
    if (sig->length > n) {
        if (!(n == sig->length-1 && va))
            return 0;
    }
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *decl = jl_tupleref(sig, i);
        if (i == sig->length-1) {
            if (va) {
                jl_value_t *t = jl_tparam0(decl);
                for(; i < n; i++) {
                    if (!jl_subtype(types[i], t, 0))
                        return 0;
                }
                return 1;
            }
        }
        jl_value_t *a = types[i];
        if (jl_is_tuple(decl)) {
            // tuples don't have to match exactly, to avoid caching
            // signatures for tuples of every length
            if (!jl_subtype(a, decl, 0))
                return 0;
        }
        else if (jl_is_tag_type(a) && jl_is_tag_type(decl) &&
                 ((jl_tag_type_t*)decl)->name == jl_type_type->name &&
                 ((jl_tag_type_t*)a   )->name == jl_type_type->name) {
            if (jl_tparam0(decl) == (jl_value_t*)jl_typetype_tvar) {
                // in the case of Type{T}, the types don't have
                // to match exactly either. this is cached as Type{T}.
                // analogous to the situation with tuples.
            }
            else {
                if (!jl_types_equal(jl_tparam0(a), jl_tparam0(decl))) {
                    return 0;
                }
            }
        }
        else if (decl == (jl_value_t*)jl_any_type) {
        }
        else {
            if (!jl_types_equal(a, decl))
                return 0;
        }
    }
    return 1;
}

static inline int cache_match(jl_value_t **args, size_t n, jl_tuple_t *sig,
                              int va)
{
    if (sig->length > n) {
        if (n != sig->length-1)
            return 0;
    }
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *decl = jl_tupleref(sig, i);
        if (i == sig->length-1) {
            if (va) {
                jl_value_t *t = jl_tparam0(decl);
                for(; i < n; i++) {
                    if (!jl_subtype(args[i], t, 1))
                        return 0;
                }
                return 1;
            }
        }
        jl_value_t *a = args[i];
        if (jl_is_tuple(decl)) {
            // tuples don't have to match exactly, to avoid caching
            // signatures for tuples of every length
            if (!jl_subtype(a, decl, 1))
                return 0;
        }
        else if (jl_is_tag_type(decl) &&
                 ((jl_tag_type_t*)decl)->name == jl_type_type->name &&
                 jl_is_nontuple_type(a)) {   //***
            if (jl_tparam0(decl) == (jl_value_t*)jl_typetype_tvar) {
                // in the case of Type{T}, the types don't have
                // to match exactly either. this is cached as Type{T}.
                // analogous to the situation with tuples.
            }
            else {
                if (a!=jl_tparam0(decl) && !jl_types_equal(a,jl_tparam0(decl)))
                    return 0;
            }
        }
        else if (decl == (jl_value_t*)jl_any_type) {
        }
        else {
            /*
              we know there are only concrete types here, and types are
              hash-consed, so pointer comparison should work.
            */
            if ((jl_value_t*)jl_typeof(a) != decl)
                return 0;
        }
    }
    return 1;
}

#define FASTER_1ARG 1

static jl_function_t *jl_method_table_assoc_exact_by_type(jl_methtable_t *mt,
                                                          jl_tuple_t *types)
{
    if (FASTER_1ARG && types->length == 1) {
        jl_value_t *ty = jl_t0(types);
        if (jl_is_struct_type(ty) || jl_is_bits_type(ty)) {
            uptrint_t uid = ((jl_struct_type_t*)ty)->uid;
            assert(uid > 0);
            if (mt->cache_1arg && uid < jl_array_len(mt->cache_1arg)) {
                jl_function_t *m = (jl_function_t*)jl_cellref(mt->cache_1arg, uid);
                if (m)
                    return m;
            }
        }
    }
    jl_methlist_t *ml = mt->cache;
    while (ml != NULL) {
        if (cache_match_by_type(&jl_tupleref(types,0), types->length,
                                (jl_tuple_t*)ml->sig, ml->va)) {
            return ml->func;
        }
        ml = ml->next;
    }
    return NULL;
}

// trivial linked list implementation for now
// TODO: pull out all the stops
static jl_function_t *jl_method_table_assoc_exact(jl_methtable_t *mt,
                                                  jl_value_t **args, size_t n)
{
    if (FASTER_1ARG && n == 1) {
        /*
          valgrind says:
          ==11709== Use of uninitialised value of size 4
          ==11709==    at 0x8053F51: jl_method_table_assoc_exact (gf.c:191)
          could this be?
        */
        jl_value_t *ty = (jl_value_t*)jl_typeof(args[0]);
        if (jl_is_struct_type(ty) || jl_is_bits_type(ty)) {
            uptrint_t uid = ((jl_struct_type_t*)ty)->uid;
            if (uid > 0 && mt->cache_1arg &&
                uid < jl_array_len(mt->cache_1arg)) {
                jl_function_t *m = (jl_function_t*)jl_cellref(mt->cache_1arg, uid);
                if (m)
                    return m;
            }
        }
    }
    jl_methlist_t *ml = mt->cache;
    while (ml != NULL) {
        if (((jl_tuple_t*)ml->sig)->length == n || ml->va) {
            if (cache_match(args, n, (jl_tuple_t*)ml->sig, ml->va)) {
                return ml->func;
            }
        }
        ml = ml->next;
    }
    return NULL;
}

// return a new lambda-info that has some extra static parameters
// merged in.
jl_lambda_info_t *jl_add_static_parameters(jl_lambda_info_t *l, jl_tuple_t *sp)
{
    JL_GC_PUSH(&sp);
    if (l->sparams->length > 0)
        sp = jl_tuple_append(sp, l->sparams);
    jl_lambda_info_t *nli = jl_new_lambda_info(l->ast, sp);
    nli->name = l->name;
    nli->fptr = l->fptr;
    JL_GC_POP();
    return nli;
}

void jl_specialize_ast(jl_lambda_info_t *li);

JL_CALLABLE(jl_trampoline);

jl_function_t *jl_instantiate_method(jl_function_t *f, jl_tuple_t *sp)
{
    if (f->linfo == NULL)
        return f;
    jl_function_t *nf = jl_new_closure(f->fptr, f->env);
    JL_GC_PUSH(&nf);
    if (f->env != NULL && jl_is_tuple(f->env) &&
        ((jl_tuple_t*)f->env)->length == 2 &&
        jl_t0(f->env) == (jl_value_t*)f) {
        nf->env = (jl_value_t*)jl_tuple2((jl_value_t*)nf, jl_t1(f->env));
    }
    nf->linfo = jl_add_static_parameters(f->linfo, sp);
    JL_GC_POP();
    return nf;
}

// make a new method that calls the generated code from the given linfo
jl_function_t *jl_reinstantiate_method(jl_function_t *f, jl_lambda_info_t *li)
{
    jl_function_t *nf = jl_new_closure(NULL, NULL);
    nf->linfo = li;
    JL_GC_PUSH(&nf);
    jl_value_t *env;
    if (f->fptr == &jl_trampoline) {
        env = jl_t1(f->env);
    }
    else {
        env = f->env;
    }
    if (li->fptr != NULL) {
        nf->fptr = li->fptr;
        nf->env = env;
    }
    else {
        nf->fptr = &jl_trampoline;
        nf->env = (jl_value_t*)jl_tuple2((jl_value_t*)nf, env);
    }
    JL_GC_POP();
    return nf;
}

jl_methlist_t *jl_method_table_insert(jl_methtable_t *mt, jl_tuple_t *type,
                                      jl_function_t *method);

static
jl_methlist_t *jl_method_list_insert(jl_methlist_t **pml, jl_tuple_t *type,
                                     jl_function_t *method, int check_amb);

static
jl_function_t *jl_method_cache_insert(jl_methtable_t *mt, jl_tuple_t *type,
                                      jl_function_t *method)
{
    if (FASTER_1ARG && type->length == 1) {
        jl_value_t *t0 = jl_t0(type);
        uptrint_t uid=0;
        if (jl_is_struct_type(t0))
            uid = ((jl_struct_type_t*)t0)->uid;
        else if (jl_is_bits_type(t0))
            uid = ((jl_bits_type_t*)t0)->uid;
        if (uid > 0) {
            if (mt->cache_1arg == NULL)
                mt->cache_1arg = jl_alloc_cell_1d(0);
            if (uid >= jl_array_len(mt->cache_1arg)) {
                jl_array_grow_end(mt->cache_1arg, uid+10-jl_array_len(mt->cache_1arg));
            }
            jl_cellset(mt->cache_1arg, uid, method);
            return method;
        }
    }
    return jl_method_list_insert(&mt->cache, type, method, 0)->func;
}

extern jl_function_t *jl_typeinf_func;
#define ENABLE_INFERENCE
//#define TRACE_INFERENCE

#ifdef TRACE_INFERENCE
static char *type_summary(jl_value_t *t);
static void print_sig(jl_tuple_t *type)
{
    size_t i;
    for(i=0; i < type->length; i++) {
        if (i > 0) ios_printf(ios_stdout, ", ");
        ios_printf(ios_stdout, "%s", type_summary(jl_tupleref(type,i)));
    }
}
#endif

static jl_value_t *nth_slot_type(jl_tuple_t *sig, size_t i)
{
    size_t len = sig->length;
    if (len == 0)
        return NULL;
    if (i < len-1)
        return jl_tupleref(sig, i);
    if (jl_is_seq_type(jl_tupleref(sig,len-1))) {
        return jl_tparam0(jl_tupleref(sig,len-1));
    }
    if (i == len-1)
        return jl_tupleref(sig, i);
    return NULL;
}

static int very_general_type(jl_value_t *t)
{
    return (t && (t==(jl_value_t*)jl_any_type ||
                  (jl_is_typevar(t) &&
                   ((jl_tvar_t*)t)->ub==(jl_value_t*)jl_any_type)));
}

static jl_tuple_t *ml_matches(jl_methlist_t *ml, jl_value_t *type,
                              jl_tuple_t *t, jl_sym_t *name);

static jl_function_t *cache_method(jl_methtable_t *mt, jl_tuple_t *type,
                                   jl_function_t *method, jl_tuple_t *decl,
                                   jl_tuple_t *sparams)
{
    size_t i;
    for (i=0; i < type->length; i++) {
        jl_value_t *elt = jl_tupleref(type,i);
        int set_to_any = 0;
        if (nth_slot_type(decl,i) == jl_ANY_flag) {
            // don't specialize on slots marked ANY
            jl_value_t *orig = jl_tupleref(type, i);
            jl_tupleset(type, i, (jl_value_t*)jl_any_type);
            int nintr=0;
            jl_methlist_t *curr = mt->defs;
            // if this method is the only match even with the current slot
            // set to Any, then it is safe to cache it that way.
            while (curr != NULL && curr->func!=method) {
                if (jl_type_intersection((jl_value_t*)curr->sig,
                                         (jl_value_t*)type) !=
                    (jl_value_t*)jl_bottom_type) {
                    nintr++;
                    break;
                }
                curr = curr->next;
            }
            if (nintr) {
                // TODO: even if different specializations of this slot need
                // separate cache entries, have them share code.
                jl_tupleset(type, i, orig);
            }
            else {
                set_to_any = 1;
            }
        }
        if (set_to_any) {
        }
        else if (jl_is_tuple(elt)) {
            /*
              don't cache tuple type exactly; just remember that it was
              a tuple, unless the declaration asks for something more
              specific. determined with a type intersection.
            */
            if (i < decl->length) {
                jl_value_t *declt = jl_tupleref(decl,i);
                // for T..., intersect with T
                if (jl_is_seq_type(declt))
                    declt = jl_tparam0(declt);
                if (declt == (jl_value_t*)jl_tuple_type ||
                    jl_subtype((jl_value_t*)jl_tuple_type, declt, 0)) {
                    // don't specialize args that matched (Any...) or Any
                    jl_tupleset(type, i, (jl_value_t*)jl_tuple_type);
                }
                else if (((jl_tuple_t*)elt)->length > 4) {
                    declt = jl_type_intersection(declt,
                                                 (jl_value_t*)jl_tuple_type);
                    jl_tupleset(type, i, declt);
                }
            }
            else {
                jl_tupleset(type, i, (jl_value_t*)jl_tuple_type);
            }
            assert(jl_tupleref(type,i) != (jl_value_t*)jl_bottom_type);
        }
        else if (jl_is_tag_type(elt) &&
                 ((jl_tag_type_t*)elt)->name==jl_type_type->name &&
                 jl_is_tag_type(jl_tparam0(elt)) &&
                 ((jl_tag_type_t*)jl_tparam0(elt))->name==jl_type_type->name) {
            /*
              actual argument was Type{...}, we computed its type as
              Type{Type{...}}. we must avoid unbounded nesting here, so
              cache the signature as Type{T}, unless something more
              specific like Type{Type{Int32}} was actually declared.
              this can be determined using a type intersection.
            */
            if (i < decl->length) {
                jl_value_t *declt = jl_tupleref(decl,i);
                // for T..., intersect with T
                if (jl_is_seq_type(declt))
                    declt = jl_tparam0(declt);
                jl_tupleset(type, i,
                            jl_type_intersection(declt, (jl_value_t*)jl_typetype_type));
            }
            else {
                jl_tupleset(type, i, (jl_value_t*)jl_typetype_type);
            }
            assert(jl_tupleref(type,i) != (jl_value_t*)jl_bottom_type);
        }
        else if (jl_is_tag_type(elt) &&
                 ((jl_tag_type_t*)elt)->name==jl_type_type->name &&
                 very_general_type(nth_slot_type(decl,i))) {
            /*
              here's a fairly complex heuristic: if this argument slot's
              declared type is Any, and no definition overlaps with Type
              for this slot, then don't specialize for every Type that
              might be passed.
              Since every type x has its own type Type{x}, this would be
              excessive specialization for an Any slot.
            */
            int ok=1;
            jl_methlist_t *curr = mt->defs;
            while (curr != NULL) {
                jl_value_t *slottype = nth_slot_type(curr->sig, i);
                if (slottype &&
                    !very_general_type(slottype) &&
                    jl_type_intersection(slottype,
                                         (jl_value_t*)jl_type_type) !=
                    (jl_value_t*)jl_bottom_type) {
                    ok=0;
                    break;
                }
                curr = curr->next;
            }
            if (ok) {
                jl_tupleset(type, i, (jl_value_t*)jl_typetype_type);
            }
        }
    }
    jl_value_t *temp=NULL;
    jl_function_t *newmeth=NULL;
    JL_GC_PUSH(&type, &temp, &newmeth);
    // for varargs methods, only specialize up to max_args.
    // in general, here we want to find the biggest type that's not a
    // supertype of any other method signatures. so far we are conservative
    // and the types we find should be bigger.
    if (type->length > mt->max_args &&
        jl_is_seq_type(jl_tupleref(decl,decl->length-1))) {
        size_t nspec = mt->max_args+2;
        jl_tuple_t *limited = jl_alloc_tuple(nspec);
        for(i=0; i < nspec-1; i++) {
            jl_tupleset(limited, i, jl_tupleref(type, i));
        }
        jl_value_t *lasttype = jl_tupleref(type,i-1);
        // if all subsequent arguments are subtypes of lasttype, specialize
        // on that instead of decl. for example, if decl is
        // (Any...)
        // and type is
        // (Symbol, Symbol, Symbol)
        // then specialize as (Symbol...), but if type is
        // (Symbol, Int32, Expr)
        // then specialize as (Any...)
        size_t j = i;
        int all_are_subtypes=1;
        for(; j < type->length; j++) {
            if (!jl_subtype(jl_tupleref(type,j), lasttype, 0)) {
                all_are_subtypes = 0;
                break;
            }
        }
        type = limited;
        if (all_are_subtypes) {
            // avoid Type{Type{...}...}...
            if (jl_is_tag_type(lasttype) &&
                ((jl_tag_type_t*)lasttype)->name == jl_type_type->name)
                lasttype = (jl_value_t*)jl_type_type;
            temp = (jl_value_t*)jl_tuple1(lasttype);
            jl_tupleset(type, i, jl_apply_type((jl_value_t*)jl_seq_type,
                                               (jl_tuple_t*)temp));
        }
        else {
            jl_tupleset(type, i, jl_tupleref(decl,decl->length-1));
        }
        // now there is a problem: the computed signature is more
        // general than just the given arguments, so it might conflict
        // with another definition that doesn't have cache instances yet.
        // to fix this, we insert dummy cache entries for all intersections
        // of this signature and definitions. those dummy entries will
        // supersede this one in conflicted cases, alerting us that there
        // should actually be a cache miss.
        temp = (jl_value_t*)
            ml_matches(mt->defs, (jl_value_t*)type, jl_null, lambda_sym);
        while (temp != (jl_value_t*)jl_null) {
            jl_method_cache_insert(mt, (jl_tuple_t*)jl_tupleref(temp, 0), NULL);
            temp = jl_tupleref(temp, 4);
        }
    }

    // here we infer types and specialize the method
    /*
    if (sparams==jl_null)
        newmeth = method;
    else
    */
    jl_tuple_t *lilist = jl_null;
    jl_lambda_info_t *li=NULL;
    if (method->linfo) {
        // reuse code already generated for this combination of lambda and
        // arguments types. this happens for inner generic functions where
        // a new closure is generated on each call to the enclosing function.
        lilist = method->linfo->specializations;
        while (lilist != jl_null) {
            li = (jl_lambda_info_t*)jl_t0(lilist);
            if (jl_types_equal(li->specTypes, (jl_value_t*)type))
                break;
            lilist = (jl_tuple_t*)jl_t1(lilist);
        }
    }
    if (lilist != jl_null) {
        assert(li);
        newmeth = jl_reinstantiate_method(method, li);
        (void)jl_method_cache_insert(mt, type, newmeth);
        JL_GC_POP();
        return newmeth;
    }
    else {
        newmeth = jl_instantiate_method(method, sparams);
    }
    /*
      if "method" itself can ever be compiled, for example for use as
      an unspecialized method (see below), then newmeth->fptr might point
      to some slow compiled code instead of jl_trampoline, meaning our
      type-inferred code would never get compiled. this can be fixed with
      the commented-out snippet below.
    */
    assert(!(newmeth->linfo && newmeth->linfo->ast) ||
           newmeth->fptr == &jl_trampoline);
    /*
    if (newmeth->linfo&&newmeth->linfo->ast&&newmeth->fptr!=&jl_trampoline) {
        newmeth->fptr = &jl_trampoline;
        newmeth->env =
            (jl_value_t*)jl_tuple2((jl_value_t*)newmeth, newmeth->env);
    }
    */

    (void)jl_method_cache_insert(mt, type, newmeth);

    if (newmeth->linfo != NULL && newmeth->linfo->sparams == jl_null) {
        // when there are no static parameters, one unspecialized version
        // of a function can be shared among all cached specializations.
        if (method->linfo->unspecialized == NULL) {
            method->linfo->unspecialized =
                jl_instantiate_method(method, jl_null);
        }
        newmeth->linfo->unspecialized = method->linfo->unspecialized;
    }

    if (newmeth->linfo != NULL && newmeth->linfo->ast != NULL) {
        newmeth->linfo->specTypes = (jl_value_t*)type;
        method->linfo->specializations =
            jl_tuple2((jl_value_t*)newmeth->linfo,
                      (jl_value_t*)method->linfo->specializations);
        jl_specialize_ast(newmeth->linfo);
        if (jl_typeinf_func != NULL) {
            // TODO: this should be done right before code gen, so if it is
            // interrupted we can try again the next time the function is
            // called
            assert(newmeth->linfo->inInference == 0);
            newmeth->linfo->inInference = 1;
            jl_value_t *fargs[5];
            fargs[0] = (jl_value_t*)newmeth->linfo;
            fargs[1] = (jl_value_t*)type;
            fargs[2] = (jl_value_t*)newmeth->linfo->sparams;
            fargs[3] = jl_false;
            fargs[4] = (jl_value_t*)method->linfo;
#ifdef TRACE_INFERENCE
            ios_printf(ios_stdout,"inference on %s(", newmeth->linfo->name->name);
            print_sig(type);
            ios_printf(ios_stdout, ")\n");
#endif
#ifdef ENABLE_INFERENCE
            jl_value_t *newast = jl_apply(jl_typeinf_func, fargs, 5);
            newmeth->linfo->ast = jl_tupleref(newast, 0);
            newmeth->linfo->inferred = 1;
#endif
            newmeth->linfo->inInference = 0;
        }
    }
    JL_GC_POP();
    return newmeth;
}

static jl_function_t *jl_mt_assoc_by_type(jl_methtable_t *mt, jl_tuple_t *tt, int cache)
{
    jl_methlist_t *m = mt->defs;
    size_t nargs = tt->length;
    size_t i;
    jl_value_t *env = (jl_value_t*)jl_false;

    while (m != NULL) {
        if (m->has_tvars) {
            env = jl_type_match((jl_value_t*)tt, (jl_value_t*)m->sig);
            if (env != (jl_value_t*)jl_false) break;
        }
        else if (jl_tuple_subtype(&jl_tupleref(tt,0), nargs,
                                  &jl_tupleref(m->sig,0),
                                  ((jl_tuple_t*)m->sig)->length, 0, 0)) {
            break;
        }
        m = m->next;
    }

    if (env == (jl_value_t*)jl_false) {
        if (m != NULL) {
            if (!cache) {
                return m->func;
            }
            return cache_method(mt, tt, m->func, (jl_tuple_t*)m->sig, jl_null);
        }
        return NULL;
    }

    jl_tuple_t *tpenv=NULL;
    jl_tuple_t *newsig=NULL;
    JL_GC_PUSH(&env, &tpenv, &newsig);

    assert(jl_is_tuple(env));
    tpenv = jl_flatten_pairs((jl_tuple_t*)env);
    // don't bother computing this if no arguments are tuples
    for(i=0; i < tt->length; i++) {
        if (jl_is_tuple(jl_tupleref(tt,i)))
            break;
    }
    if (i < tt->length) {
        newsig = (jl_tuple_t*)jl_instantiate_type_with((jl_type_t*)m->sig,
                                                       &jl_tupleref(tpenv,0),
                                                       tpenv->length/2);
    }
    else {
        newsig = (jl_tuple_t*)m->sig;
    }
    assert(jl_is_tuple(newsig));
    jl_function_t *nf;
    if (!cache)
        nf = m->func;
    else
        nf = cache_method(mt, tt, m->func, newsig, tpenv);
    JL_GC_POP();
    return nf;
}

jl_tag_type_t *jl_wrap_Type(jl_value_t *t);

static int sigs_eq(jl_value_t *a, jl_value_t *b)
{
    if (jl_has_typevars(a) || jl_has_typevars(b)) {
        return jl_types_equal_generic(a,b);
    }
    return jl_types_equal(a, b);
}

static int args_morespecific(jl_value_t *a, jl_value_t *b)
{
    int msp = jl_type_morespecific(a,b,0);
    if (jl_has_typevars(b)) {
        if (jl_type_match_morespecific(a,b) == (jl_value_t*)jl_false) {
            return 0;
        }
        if (jl_has_typevars(a)) {
            if (jl_type_match_morespecific(b,a) == (jl_value_t*)jl_false) {
                return 1;
            }
        }
        int nmsp = jl_type_morespecific(b,a,0);
        if (nmsp == msp)
            return 0;
    }
    if (jl_has_typevars((jl_value_t*)a)) {
        int nmsp = jl_type_morespecific(b,a,0);
        if (nmsp && msp)
            return 1;
        if (jl_type_match_morespecific(b,a) != (jl_value_t*)jl_false) {
            return 0;
        }
    }
    return msp;
}

static jl_tuple_t *without_typectors(jl_tuple_t *t)
{
    jl_tuple_t *tc = jl_alloc_tuple_uninit(t->length);
    size_t i;
    for(i=0; i < t->length; i++) {
        jl_value_t *v = jl_tupleref(t,i);
        if (jl_is_typector(v))
            jl_tupleset(tc,i,(jl_value_t*)((jl_typector_t*)v)->body);
        else
            jl_tupleset(tc,i,v);
    }
    return tc;
}

static int is_va_tuple(jl_tuple_t *t)
{
    return (t->length>0 && jl_is_seq_type(jl_tupleref(t,t->length-1)));
}

/*
  warn about ambiguous method priorities
  
  the relative priority of A and B is ambiguous if
  !subtype(A,B) && !subtype(B,A) && no corresponding tuple
  elements are disjoint.
  
  for example, (AbstractArray, AbstractMatrix) and (AbstractMatrix, AbstractArray) are ambiguous.
  however, (AbstractArray, AbstractMatrix, Foo) and (AbstractMatrix, AbstractArray, Bar) are fine
  since Foo and Bar are disjoint, so there would be no confusion over
  which one to call.
  
  There is also this kind of ambiguity: foo{T,S}(T, S) vs. foo(Any,Any)
  In this case jl_types_equal() is true, but one is jl_type_morespecific
  or jl_type_match_morespecific than the other.
  To check this, jl_types_equal_generic needs to be more sophisticated
  so (T,T) is not equivalent to (Any,Any). (TODO)
*/
static void check_ambiguous(jl_methlist_t *ml, jl_tuple_t *type,
                            jl_tuple_t *sig, jl_sym_t *fname)
{
    // we know !args_morespecific(type, sig)
    if ((type->length==sig->length ||
         (type->length==sig->length+1 && is_va_tuple(type)) ||
         (type->length+1==sig->length && is_va_tuple(sig))) &&
        !args_morespecific((jl_value_t*)sig, (jl_value_t*)type)) {
        jl_value_t *isect = jl_type_intersection((jl_value_t*)type,
                                                 (jl_value_t*)sig);
        if (isect == (jl_value_t*)jl_bottom_type)
            return;
        jl_value_t *t1=NULL, *t2=NULL;
        JL_GC_PUSH(&isect, &t1, &t2);
        jl_methlist_t *l = ml;
        while (l != NULL) {
            if (sigs_eq(isect, (jl_value_t*)l->sig))
                goto done_chk_amb;  // ok, intersection is covered
            l = l->next;
        }
        char *n = fname->name;
        t1 = (jl_value_t*)without_typectors(type);
        t2 = (jl_value_t*)without_typectors(sig);
        jl_value_t *errstream = jl_get_global(jl_system_module,
                                              jl_symbol("stderr_stream"));
        JL_TRY {
            if (errstream)
                jl_set_current_output_stream_obj(errstream);
            ios_t *s = jl_current_output_stream();
            ios_printf(s, "Warning: New definition %s", n);
            jl_show(t1);
            ios_printf(s, " is ambiguous with %s", n);
            jl_show(t2);
            ios_printf(s, ".\n         Make sure %s", n);
            jl_show(isect);
            ios_printf(s, " is defined first.\n");
        }
        JL_CATCH {
            jl_raise(jl_exception_in_transit);
        }
    done_chk_amb:
        JL_GC_POP();
    }
}

static jl_tuple_t *find_tvars(jl_value_t *v, jl_tuple_t *env)
{
    if (jl_is_typevar(v) && ((jl_tvar_t*)v)->bound) {
        jl_tuple_t *pe = env;
        while (pe != jl_null) {
            if (jl_t0(pe) == v)
                return env;
            pe = (jl_tuple_t*)jl_t1(pe);
        }
        return jl_tuple2(v, env);
    }
    JL_GC_PUSH(&env);
    if (jl_is_func_type(v)) {
        env = find_tvars((jl_value_t*)((jl_func_type_t*)v)->from, env);
        env = find_tvars((jl_value_t*)((jl_func_type_t*)v)->to  , env);
        goto done_find_tvars;
    }
    jl_tuple_t *params;
    if (jl_is_tuple(v))
        params = (jl_tuple_t*)v;
    else if (jl_is_union_type(v))
        params = ((jl_uniontype_t*)v)->types;
    else if (jl_is_some_tag_type(v))
        params = ((jl_tag_type_t*)v)->parameters;
    else
        goto done_find_tvars;
    int i;
    for(i=0; i < params->length; i++) {
        jl_value_t *p = jl_tupleref(params, i);
        if (p != v)
            env = find_tvars(p, env);
    }
 done_find_tvars:
    JL_GC_POP();
    return env;
}

static int has_unions(jl_tuple_t *type)
{
    int i;
    for(i=0; i < type->length; i++) {
        jl_value_t *t = jl_tupleref(type,i);
        if (jl_is_union_type(t) ||
            (jl_is_seq_type(t) && jl_is_union_type(jl_tparam0(t))))
            return 1;
    }
    return 0;
}

static
jl_methlist_t *jl_method_list_insert(jl_methlist_t **pml, jl_tuple_t *type,
                                     jl_function_t *method, int check_amb)
{
    jl_methlist_t *l, **pl;

    assert(jl_is_tuple(type));
    l = *pml;
    while (l != NULL) {
        if (sigs_eq((jl_value_t*)type, (jl_value_t*)l->sig)) {
            // method overwritten
            JL_SIGATOMIC_BEGIN();
            l->sig = type;
            l->tvars = find_tvars((jl_value_t*)type, jl_null);
            l->has_tvars = (l->tvars != jl_null);
            l->va = (type->length > 0 &&
                     jl_is_seq_type(jl_tupleref(type,type->length-1)));
            l->invokes = NULL;
            l->func = method;
            JL_SIGATOMIC_END();
            return l;
        }
        l = l->next;
    }
    pl = pml;
    l = *pml;
    while (l != NULL) {
        if (args_morespecific((jl_value_t*)type, (jl_value_t*)l->sig))
            break;
        if (check_amb) {
            check_ambiguous(*pml, (jl_tuple_t*)type, (jl_tuple_t*)l->sig,
                            method->linfo ? method->linfo->name :
                            jl_symbol("anonymous"));
        }
        pl = &l->next;
        l = l->next;
    }
    jl_tuple_t *tv = find_tvars((jl_value_t*)type, jl_null);
    JL_GC_PUSH(&tv);
    jl_methlist_t *newrec = (jl_methlist_t*)allocb(sizeof(jl_methlist_t));
    newrec->sig = type;
    newrec->tvars = tv;
    newrec->has_tvars = (newrec->tvars != jl_null);
    newrec->va = (type->length > 0 &&
                  jl_is_seq_type(jl_tupleref(type,type->length-1)));
    newrec->func = method;
    newrec->invokes = NULL;
    newrec->next = l;
    JL_SIGATOMIC_BEGIN();
    *pl = newrec;
    // if this contains Union types, methods after it might actually be
    // more specific than it. we need to re-sort them.
    if (has_unions(type)) {
        jl_methlist_t *item = newrec->next, *next;
        jl_methlist_t **pitem = &newrec->next, **pnext;
        while (item != NULL) {
            pl = pml;
            l = *pml;
            next = item->next;
            pnext = &item->next;
            while (l != newrec->next) {
                if (args_morespecific((jl_value_t*)item->sig,
                                      (jl_value_t*)l->sig)) {
                    // reinsert item earlier in the list
                    *pitem = next;
                    item->next = l;
                    *pl = item;
                    pnext = pitem;
                    break;
                }
                pl = &l->next;
                l = l->next;
            }
            item = next;
            pitem = pnext;
        }
    }
    JL_SIGATOMIC_END();
    JL_GC_POP();
    return newrec;
}

jl_methlist_t *jl_method_table_insert(jl_methtable_t *mt, jl_tuple_t *type,
                                      jl_function_t *method)
{
    JL_SIGATOMIC_BEGIN();
    jl_methlist_t *ml = jl_method_list_insert(&mt->defs, type, method, 1);
    // invalidate cached methods that overlap this definition
    jl_methlist_t *l = mt->cache;
    jl_methlist_t **pthis = &mt->cache;
    while (l != NULL) {
        if (jl_type_intersection((jl_value_t*)type, (jl_value_t*)l->sig) !=
            (jl_value_t*)jl_bottom_type) {
            *pthis = l->next;
        }
        else {
            pthis = &l->next;
        }
        l = l->next;
    }
    if (type->length == 1) {
        mt->cache_1arg = NULL;
    }
    // update max_args
    jl_tuple_t *t = (jl_tuple_t*)type;
    size_t na = t->length;
    if (t->length>0 && jl_is_seq_type(jl_tupleref(t,t->length-1)))
        na--;
    if (na > mt->max_args) {
        mt->max_args = na;
    }
    JL_SIGATOMIC_END();
    return ml;
}

jl_value_t *jl_no_method_error(jl_function_t *f, jl_value_t **args, size_t na)
{
    jl_value_t **a = alloca(na+1);
    a[0] = (jl_value_t*)f;
    int i;
    for(i=0; i < na; i++)
        a[i+1] = args[i];
    return jl_apply(jl_method_missing_func, a, na+1);
}

//#define JL_TRACE
#if defined(JL_TRACE) || defined(TRACE_INFERENCE)
static char *type_summary(jl_value_t *t)
{
    if (jl_is_tuple(t)) return "Tuple";
    if (jl_is_func_type(t)) return "Function";
    if (jl_is_some_tag_type(t))
        return ((jl_tag_type_t*)t)->name->name->name;
    ios_printf(ios_stdout, "unexpected argument type: ");
    jl_show(t);
    ios_printf(ios_stdout, "\n");
    assert(0);
    return NULL;
}
#endif

static jl_tuple_t *arg_type_tuple(jl_value_t **args, size_t nargs)
{
    jl_tuple_t *tt = jl_alloc_tuple(nargs);
    JL_GC_PUSH(&tt);
    size_t i;
    for(i=0; i < tt->length; i++) {
        jl_value_t *a;
        if (jl_is_nontuple_type(args[i])) {  //***
            a = (jl_value_t*)jl_wrap_Type(args[i]);
        }
        else {
            a = (jl_value_t*)jl_full_type(args[i]);
        }
        jl_tupleset(tt, i, a);
    }
    JL_GC_POP();
    return tt;
}

jl_function_t *jl_method_lookup_by_type(jl_methtable_t *mt, jl_tuple_t *types,
                                        int cache)
{
    jl_function_t *sf = jl_method_table_assoc_exact_by_type(mt, types);
    if (sf == NULL) {
        sf = jl_mt_assoc_by_type(mt, types, cache);
    }
    return sf;
}

jl_function_t *jl_method_lookup(jl_methtable_t *mt, jl_value_t **args, size_t nargs, int cache)
{
    jl_function_t *sf = jl_method_table_assoc_exact(mt, args, nargs);
    if (sf == NULL) {
        jl_tuple_t *tt = arg_type_tuple(args, nargs);
        JL_GC_PUSH(&tt);
        sf = jl_mt_assoc_by_type(mt, tt, cache);
        JL_GC_POP();
    }
    return sf;
}

// compile-time method lookup
DLLEXPORT
jl_function_t *jl_get_specialization(jl_function_t *f, jl_tuple_t *types)
{
    assert(jl_is_gf(f));
    if (!jl_is_leaf_type((jl_value_t*)types))
        return NULL;
    jl_methtable_t *mt = (jl_methtable_t*)jl_t0(f->env);
    jl_function_t *sf = jl_method_lookup_by_type(mt, types, 1);
    if (sf == NULL) {
        return NULL;
    }
    if (sf->linfo == NULL || sf->linfo->ast == NULL) {
        return NULL;
    }
    if (sf->linfo->inInference) return NULL;
    if (sf->linfo->functionObject == NULL) {
        if (sf->fptr != &jl_trampoline)
            return NULL;
        jl_compile(sf);
    }
    return sf;
}

#ifdef JL_TRACE
static int trace_en = 0;
static void enable_trace(int x) { trace_en=x; }
#endif

JL_CALLABLE(jl_apply_generic)
{
    jl_methtable_t *mt = (jl_methtable_t*)jl_t0(env);
#ifdef JL_GF_PROFILE
    mt->ncalls++;
#endif
#ifdef JL_TRACE
    if (trace_en) {
        ios_printf(ios_stdout, "%s(", ((jl_sym_t*)jl_t1(env))->name);
        size_t i;
        for(i=0; i < nargs; i++) {
            if (i > 0) ios_printf(ios_stdout, ", ");
            ios_printf(ios_stdout, "%s", type_summary(jl_typeof(args[i])));
        }
        ios_printf(ios_stdout, ")\n");
    }
#endif
    /*
      search order:
      look at concrete signatures
      if there is an exact match, return it
      otherwise look for a matching generic signature
      if no concrete or generic match, raise error
      if no generic match, use the concrete one even if inexact
      otherwise instantiate the generic method and use it
    */
    jl_function_t *mfunc = jl_method_table_assoc_exact(mt, args, nargs);
    if (mfunc != NULL) {
        if (mfunc->linfo != NULL && 
            (mfunc->linfo->inInference || mfunc->linfo->inCompile)) {
            // if inference is running on this function, return a copy
            // of the function to be compiled without inference and run.
            jl_lambda_info_t *li = mfunc->linfo;
            if (li->unspecialized == NULL) {
                li->unspecialized = jl_instantiate_method(mfunc, li->sparams);
            }
            mfunc = li->unspecialized;
        }
    }
    else {
        jl_tuple_t *tt = arg_type_tuple(args, nargs);
        JL_GC_PUSH(&tt);
        mfunc = jl_mt_assoc_by_type(mt, tt, 1);
        JL_GC_POP();
    }

    if (mfunc == NULL) {
        return jl_no_method_error((jl_function_t*)jl_t2(env), args, nargs);
    }

    JL_GC_PUSH(&mfunc);
    jl_value_t *result = jl_apply(mfunc, args, nargs);
    JL_GC_POP();
    return result;
}

// invoke()
// this does method dispatch with a set of types to match other than the
// types of the actual arguments. this means it sometimes does NOT call the
// most specific method for the argument types, so we need different logic.
// first we use the given types to look up a definition, then we perform
// caching and specialization within just that definition.
// every definition has its own private method table for this purpose.
//
// NOTE: assumes argument type is a subtype of the lookup type.
jl_value_t *jl_gf_invoke(jl_function_t *gf, jl_tuple_t *types,
                         jl_value_t **args, size_t nargs)
{
    assert(jl_is_gf(gf));
    jl_methtable_t *mt = jl_gf_mtable(gf);

    jl_methlist_t *m = mt->defs;
    size_t typelen = types->length;
    size_t i;
    jl_value_t *env = (jl_value_t*)jl_false;

    while (m != NULL) {
        if (m->has_tvars) {
            env = jl_type_match((jl_value_t*)types, (jl_value_t*)m->sig);
            if (env != (jl_value_t*)jl_false) break;
        }
        else if (jl_tuple_subtype(&jl_tupleref(types,0), typelen,
                                  &jl_tupleref(m->sig,0),
                                  ((jl_tuple_t*)m->sig)->length, 0, 0)) {
            break;
        }
        m = m->next;
    }

    if (m == NULL) {
        return jl_no_method_error(gf, args, nargs);
    }

    // now we have found the matching definition.
    // next look for or create a specialization of this definition.

    jl_function_t *mfunc;
    if (m->invokes == NULL)
        mfunc = NULL;
    else
        mfunc = jl_method_table_assoc_exact(m->invokes, args, nargs);
    if (mfunc != NULL) {
        if (mfunc->linfo != NULL && 
            (mfunc->linfo->inInference || mfunc->linfo->inCompile)) {
            // if inference is running on this function, return a copy
            // of the function to be compiled without inference and run.
            jl_lambda_info_t *li = mfunc->linfo;
            if (li->unspecialized == NULL) {
                li->unspecialized = jl_instantiate_method(mfunc, li->sparams);
            }
            mfunc = li->unspecialized;
        }
    }
    else {
        jl_tuple_t *tpenv=jl_null;
        jl_tuple_t *newsig=NULL;
        jl_tuple_t *tt=NULL;
        JL_GC_PUSH(&env, &tpenv, &newsig, &tt);

        if (m->invokes == NULL) {
            m->invokes = new_method_table();
            // this private method table has just this one definition
            jl_method_list_insert(&m->invokes->defs, m->sig, m->func, 0);
        }

        tt = arg_type_tuple(args, nargs);

        newsig = (jl_tuple_t*)m->sig;

        if (env != (jl_value_t*)jl_false) {
            tpenv = jl_flatten_pairs((jl_tuple_t*)env);
            // don't bother computing this if no arguments are tuples
            for(i=0; i < tt->length; i++) {
                if (jl_is_tuple(jl_tupleref(tt,i)))
                    break;
            }
            if (i < tt->length) {
                newsig =
                    (jl_tuple_t*)jl_instantiate_type_with((jl_type_t*)m->sig,
                                                          &jl_tupleref(tpenv,0),
                                                          tpenv->length/2);
            }
        }
        mfunc = cache_method(m->invokes, tt, m->func, newsig, tpenv);
        JL_GC_POP();
    }

    JL_GC_PUSH(&mfunc);
    jl_value_t *result = jl_apply(mfunc, args, nargs);
    JL_GC_POP();
    return result;
}

static void print_methlist(char *name, jl_methlist_t *ml)
{
    ios_t *s = jl_current_output_stream();
    while (ml != NULL) {
        ios_printf(s, "%s", name);
        jl_show((jl_value_t*)ml->sig);
        if (ml->func == NULL)  // mark dummy cache entries
            ios_printf(s, " *");
        //if (ml->func && ml->func->linfo && ml->func->linfo->ast &&
        //    ml->func->linfo->inferred) {
        //    jl_show(ml->func->linfo->ast);
        //}
        if (ml->next != NULL)
            ios_printf(s, "\n");
        ml = ml->next;
    }
}

void jl_show_method_table(jl_function_t *gf)
{
    char *name = ((jl_sym_t*)jl_t1(gf->env))->name;
    jl_methtable_t *mt = (jl_methtable_t*)jl_t0(gf->env);
    print_methlist(name, mt->defs);
    //ios_printf(ios_stdout, "\ncache:\n");
    //print_methlist(name, mt->cache);
}

void jl_initialize_generic_function(jl_function_t *f, jl_sym_t *name)
{
    f->fptr = jl_apply_generic;
    jl_value_t *nmt = (jl_value_t*)new_method_table();
    JL_GC_PUSH(&nmt);
    f->env = (jl_value_t*)jl_tuple3(nmt, (jl_value_t*)name, jl_null);
    jl_t2(f->env) = (jl_value_t*)f;
    JL_GC_POP();
}

jl_function_t *jl_new_generic_function(jl_sym_t *name)
{
    jl_function_t *f = jl_new_closure(NULL, NULL);
    JL_GC_PUSH(&f);
    jl_initialize_generic_function(f, name);
    JL_GC_POP();
    return f;
}

void jl_add_method(jl_function_t *gf, jl_tuple_t *types, jl_function_t *meth)
{
    assert(jl_is_function(gf));
    assert(jl_is_tuple(types));
    assert(jl_is_func(meth));
    assert(jl_is_tuple(gf->env));
    assert(jl_is_mtable(jl_t0(gf->env)));
    if (jl_gf_mtable(gf)->sealed)
        jl_errorf("cannot add methods to %s", jl_gf_name(gf)->name);
    if (meth->linfo != NULL)
        meth->linfo->name = jl_gf_name(gf);
    (void)jl_method_table_insert(jl_gf_mtable(gf), types, meth);
}

static jl_tuple_t *match_method(jl_value_t *type, jl_function_t *func,
                                jl_tuple_t *sig, jl_tuple_t *tvars,
                                jl_sym_t *name, jl_tuple_t *next)
{
    jl_tuple_t *env = jl_null;
    jl_value_t *temp=NULL;
    jl_value_t *ti=NULL;
    JL_GC_PUSH(&env, &ti, &temp);

    ti = jl_type_intersection_matching((jl_value_t*)sig, type, &env, tvars);
    jl_tuple_t *result = NULL;
    if (ti != (jl_value_t*)jl_bottom_type) {
        if (func->linfo == NULL) {
            // builtin
            result = jl_tuple(5, ti, env, name, jl_null, next);
        }
        else {
            jl_value_t *cenv;
            if (func->env != NULL) {
                if (func->fptr == &jl_trampoline)
                    cenv = jl_t1(func->env);
                else
                    cenv = func->env;
            }
            else {
                cenv = (jl_value_t*)jl_null;
            }
            result = jl_tuple(5, ti, env, func->linfo, cenv, next);
        }
    }
    JL_GC_POP();
    return result;
}

// returns linked tuples (argtypes, static_params, lambdainfo, cloenv, next)
static jl_tuple_t *ml_matches(jl_methlist_t *ml, jl_value_t *type,
                              jl_tuple_t *t, jl_sym_t *name)
{
    JL_GC_PUSH(&t);
    while (ml != NULL) {
        // a method is shadowed if type <: S <: m->sig where S is the
        // signature of another applicable method
        /*
          more generally?
          given arguments T and applicable methods A, X (T⊆A ∧ T⊆X),
          X is shadowed if there exists an applicable method B
          (possibly A==B), such that (B ⊆ X) ∧ (A∩X ⊆ B)
          in other words, B covers any ambiguity between A and X.
        */
        //jl_tuple_t *tt = t;
        //int shadowed = 0;
        /*
        while (tt != jl_null) {
            jl_value_t *S = jl_tupleref(tt,0);
            if (jl_subtype(type, S, 0) &&
                jl_subtype(S, (jl_value_t*)ml->sig, 0)) {
                shadowed = 1;
                break;
            }
            tt = (jl_tuple_t*)jl_tupleref(tt,4);
        }
        */
        if (1/*!shadowed*/) {
            jl_tuple_t *matc = match_method(type, ml->func, ml->sig, ml->tvars,
                                            name, t);
            if (matc != NULL) {
                t = matc;
                // (type ∩ ml->sig == type) ⇒ (type ⊆ ml->sig)
                if (jl_types_equal(jl_t0(matc), type)) {
                    JL_GC_POP();
                    return t;
                }
                /*
                if (ml->has_tvars) {
                    if (jl_type_match((jl_type_t*)type, ml->sig) != jl_false)
                        return t;
                }
                else {
                    if (jl_subtype(type, (jl_value_t*)ml->sig, 0))
                        return t;
                }
                */
            }
        }
        ml = ml->next;
    }
    JL_GC_POP();
    return t;
}

// return linked tuples (t1, M1, (t2, M2, (... ()))) of types and methods.
// t is the intersection of the type argument and the method signature,
// and M is the corresponding LambdaStaticData (jl_lambda_info_t)
DLLEXPORT
jl_value_t *jl_matching_methods(jl_function_t *gf, jl_value_t *type)
{
    jl_tuple_t *t = jl_null;
    if (!jl_is_gf(gf)) {
        return (jl_value_t*)t;
    }
    jl_methtable_t *mt = jl_gf_mtable(gf);
    jl_sym_t *gfname = jl_gf_name(gf);
    t = ml_matches(mt->defs, type, t, gfname);
    return (jl_value_t*)t;
}

DLLEXPORT
int jl_is_builtin(jl_value_t *v)
{
    return ((jl_is_func(v) && (((jl_function_t*)v)->linfo==NULL) &&
             !jl_is_gf(v)) ||
            jl_typeis(v,jl_intrinsic_type));
}

DLLEXPORT
int jl_is_genericfunc(jl_value_t *v)
{
    return (jl_is_func(v) && jl_is_gf(v));
}

DLLEXPORT
jl_sym_t *jl_genericfunc_name(jl_value_t *v)
{
    return jl_gf_name(v);
}
