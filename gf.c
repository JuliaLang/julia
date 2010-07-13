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
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

static jl_methtable_t *new_method_table()
{
    jl_methtable_t *mt = (jl_methtable_t*)allocb(sizeof(jl_methtable_t));
    mt->type = (jl_type_t*)jl_methtable_type;
    mt->defs = NULL;
    mt->cache = NULL;
    mt->sealed = 0;
    mt->max_args = 0;
    return mt;
}

static int cache_match_by_type(jl_value_t **types, size_t n, jl_tuple_t *sig)
{
    if (sig->length > n) {
        return 0;
    }
    if (sig->length < n) {
        if (sig->length==0 || !jl_is_seq_type(jl_tupleref(sig,sig->length-1)))
            return 0;
    }
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *decl = jl_tupleref(sig, i);
        if (i == sig->length-1) {
            if (jl_is_seq_type(decl)) {
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
            if (jl_tparam0(decl) == (jl_value_t*)jl_type_type) {
                // in the case of Type{Type{...}}, the types don't have
                // to match exactly either. this is cached as Type{Type}.
                // analogous to the situation with tuples.
                return 1;
            }
            if (!jl_types_equal(jl_tparam0(a), jl_tparam0(decl))) {
                return 0;
            }
        }
        else {
            if (!jl_types_equal(a, decl))
                return 0;
        }
    }
    return 1;
}

static inline int cache_match(jl_value_t **args, size_t n, jl_tuple_t *sig)
{
#if 0
    if (sig->length > n) {
        //if (!(n == sig->length-1 && jl_is_seq_type(jl_tupleref(sig,n))))
        /*
          in the cache, don't allow T... to match 0 args. here's why:
          say we have these definitions:
          f(x::Int32)
          f(xs...)

          say f(1,2) is called first. this is cached as f(Int32,Any...).
          now I call f(1). If f(Int32,Any...) matched this, it would call
          the cached f(xs...) definition, but it should call f(x::Int32).

          alternatively, we could cache f(1,2) as f(Int32,Int32,Any...).
          however this leads to more compilation.
        */
        return 0;
    }
#endif
    if (sig->length < n) {
        if (sig->length==0 || !jl_is_seq_type(jl_tupleref(sig,sig->length-1)))
            return 0;
    }
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *decl = jl_tupleref(sig, i);
        if (i == sig->length-1) {
            if (jl_is_seq_type(decl)) {
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
        else if (jl_is_some_tag_type(a) && jl_is_tag_type(decl) &&
                 ((jl_tag_type_t*)decl)->name == jl_type_type->name) {
            if (jl_tparam0(decl) == (jl_value_t*)jl_type_type) {
                // in the case of Type{Type{...}}, the types don't have
                // to match exactly either. this is cached as Type{Type}.
                // analogous to the situation with tuples.
                return 1;
            }
            if (!jl_types_equal(a, jl_tparam0(decl))) {
                return 0;
            }
        }
        else {
            if (!jl_types_equal((jl_value_t*)jl_typeof(a), decl))
                return 0;
        }
    }
    return 1;
}

// trivial linked list implementation for now
// TODO: pull out all the stops
static jl_methlist_t *jl_method_list_assoc_exact(jl_methlist_t *ml,
                                                 jl_value_t **args, size_t n)
{
    while (ml != NULL) {
        if (((jl_tuple_t*)ml->sig)->length <= n) {
            if (cache_match(args, n, (jl_tuple_t*)ml->sig))
                return ml;
        }
        ml = ml->next;
    }
    return NULL;
}

// return a new lambda-info that has some extra static parameters
// merged in.
jl_lambda_info_t *jl_add_static_parameters(jl_lambda_info_t *l, jl_tuple_t *sp)
{
    if (l->sparams->length > 0)
        sp = jl_tuple_append(sp, l->sparams);
    jl_lambda_info_t *nli = jl_new_lambda_info(l->ast, sp);
    nli->name = l->name;
    nli->fptr = l->fptr;
    return nli;
}

void jl_specialize_ast(jl_lambda_info_t *li);

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

jl_methlist_t *jl_method_table_insert(jl_methtable_t *mt, jl_type_t *type,
                                      jl_function_t *method);

static
jl_methlist_t *jl_method_list_insert(jl_methlist_t **pml, jl_type_t *type,
                                     jl_function_t *method, int check_amb);

extern jl_function_t *jl_typeinf_func;
//TODO: disabled for now
//#define ENABLE_INFERENCE
//#define TRACE_INFERENCE

#ifdef TRACE_INFERENCE
static char *cur_fname;
static char *type_summary(jl_value_t *t);
#endif

static jl_function_t *cache_method(jl_methtable_t *mt, jl_tuple_t *type,
                                   jl_function_t *method, jl_tuple_t *decl,
                                   jl_tuple_t *sparams)
{
    size_t i;
    for (i=0; i < type->length; i++) {
        jl_value_t *elt = jl_tupleref(type,i);
        if (jl_is_tuple(elt)) {
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
                jl_tupleset(type, i,
                            jl_type_intersection(declt,
                                                 (jl_value_t*)jl_tuple_type));
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
              cache the signature as Type{Type{_}}, unless something more
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
    }
    // for varargs methods, only specialize up to max_args
    if (type->length > mt->max_args &&
        jl_is_seq_type(jl_tupleref(decl,decl->length-1))) {
        jl_tuple_t *limited = jl_alloc_tuple(mt->max_args+1);
        for(i=0; i < mt->max_args; i++) {
            jl_tupleset(limited, i, jl_tupleref(type, i));
        }
        jl_tupleset(limited, i, jl_tupleref(decl,decl->length-1));
        type = limited;
    }

    // here we infer types and specialize the method
    jl_function_t *newmeth;
    /*
    if (sparams==jl_null)
        newmeth = method;
    else
    */
    newmeth = jl_instantiate_method(method, sparams);

    jl_methlist_t *ret =
        jl_method_list_insert(&mt->cache, (jl_type_t*)type, newmeth, 0);

    if (newmeth->linfo != NULL && newmeth->linfo->sparams == jl_null) {
        // when there are no static parameters, one unspecialized version
        // of a function can be shared among all cached specializations.
        newmeth->linfo->unspecialized = method;
    }

    if (newmeth->linfo != NULL && newmeth->linfo->ast != NULL) {
        newmeth->linfo->specTypes = (jl_value_t*)type;
        jl_specialize_ast(newmeth->linfo);
        if (jl_typeinf_func != NULL) {
            newmeth->linfo->inInference = 1;
            jl_value_t *fargs[4];
            fargs[0] = (jl_value_t*)newmeth->linfo;
            fargs[1] = (jl_value_t*)type;
            fargs[2] = (jl_value_t*)newmeth->linfo->sparams;
            fargs[3] = jl_false;
#ifdef TRACE_INFERENCE
            ios_printf(ios_stdout,"inference on %s(", cur_fname);
            size_t i;
            for(i=0; i < type->length; i++) {
                if (i > 0) ios_printf(ios_stdout, ", ");
                ios_printf(ios_stdout, "%s", type_summary(jl_tupleref(type,i)));
            }
            ios_printf(ios_stdout, ")\n");
#endif
#ifdef ENABLE_INFERENCE
            jl_value_t *newast = jl_apply(jl_typeinf_func, fargs, 4);
            newmeth->linfo->ast = jl_tupleref(newast, 0);
#endif
            newmeth->linfo->inInference = 0;
        }
    }
    return ret->func;
}

static jl_function_t *mt_assoc_by_type(jl_methtable_t *mt, jl_tuple_t *tt)
{
    jl_methlist_t *m = mt->defs;
    size_t nargs = tt->length;
    size_t i;
    jl_value_t *env = (jl_value_t*)jl_false;

    while (m != NULL) {
        if (jl_has_typevars((jl_value_t*)m->sig)) {
            env = jl_type_match((jl_type_t*)tt, m->sig);
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
            return cache_method(mt, tt, m->func, (jl_tuple_t*)m->sig, jl_null);
        }
        return NULL;
    }

    assert(jl_is_tuple(env));
    jl_tuple_t *tpenv = jl_flatten_pairs((jl_tuple_t*)env);
    jl_tuple_t *newsig;
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
    return cache_method(mt, tt, m->func, newsig, tpenv);
}

jl_tag_type_t *jl_wrap_Type(jl_value_t *t);

jl_function_t *jl_method_table_assoc(jl_methtable_t *mt,
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
    */
    jl_methlist_t *m = jl_method_list_assoc_exact(mt->cache, args, nargs);
    if (m != NULL) {
        if (m->func->linfo != NULL && 
            (m->func->linfo->inInference || m->func->linfo->inCompile)) {
            // if inference is running on this function, return a copy
            // of the function to be compiled without inference and run.
            jl_lambda_info_t *li = m->func->linfo;
            if (li->unspecialized == NULL) {
                li->unspecialized = jl_instantiate_method(m->func, jl_null);
            }
            return li->unspecialized;
        }
        return m->func;
    }

    jl_tuple_t *tt = jl_alloc_tuple(nargs);
    size_t i;
    for(i=0; i < tt->length; i++) {
        jl_value_t *a;
        if (jl_is_some_tag_type(args[i])) {
            a = (jl_value_t*)jl_wrap_Type(args[i]);
        }
        else {
            a = (jl_value_t*)jl_full_type(args[i]);
        }
        jl_tupleset(tt, i, a);
    }

    return mt_assoc_by_type(mt, tt);
}

static int sigs_eq(jl_type_t *a, jl_type_t *b)
{
    if (jl_has_typevars((jl_value_t*)a) || jl_has_typevars((jl_value_t*)b)) {
        return jl_types_equal_generic((jl_value_t*)a,(jl_value_t*)b);
    }
    return jl_types_equal((jl_value_t*)a, (jl_value_t*)b);
}

static int args_morespecific(jl_type_t *a, jl_type_t *b)
{
    int msp = jl_type_morespecific((jl_value_t*)a,(jl_value_t*)b,0);
    if (jl_has_typevars((jl_value_t*)b)) {
        if (jl_type_match_morespecific(a,b) == (jl_value_t*)jl_false) {
            return 0;
        }
        if (jl_has_typevars((jl_value_t*)a)) {
            if (jl_type_match_morespecific(b,a) == (jl_value_t*)jl_false) {
                return 1;
            }
        }
        int nmsp = jl_type_morespecific((jl_value_t*)b,(jl_value_t*)a,0);
        if (nmsp == msp)
            return 0;
    }
    if (jl_has_typevars((jl_value_t*)a)) {
        int nmsp = jl_type_morespecific((jl_value_t*)b,(jl_value_t*)a,0);
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
    jl_tuple_t *tc = jl_alloc_tuple(t->length);
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

/*
  warn about ambiguous method priorities
  
  the relative priority of A and B is ambiguous if
  !subtype(A,B) && !subtype(B,A) && no corresponding tuple
  elements are disjoint.
  
  for example, (Tensor, Matrix) and (Matrix, Tensor) are ambiguous.
  however, (Tensor, Matrix, Foo) and (Matrix, Tensor, Bar) are fine
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
    if (type->length==sig->length && !args_morespecific((jl_type_t*)sig,
                                                        (jl_type_t*)type)) {
        jl_value_t *isect = jl_type_intersection((jl_value_t*)type,
                                                 (jl_value_t*)sig);
        if (isect == (jl_value_t*)jl_bottom_type)
            return;
        jl_methlist_t *l = ml;
        while (l != NULL) {
            if (sigs_eq((jl_type_t*)isect, l->sig))
                return;  // ok, intersection is covered
            l = l->next;
        }
        char *n = fname->name;
        ios_printf(ios_stdout,
                   "Warning: new definition %s%s is ambiguous with %s%s. "
                   "Make sure %s%s is also defined.\n",
                   n, jl_print_to_string((jl_value_t*)without_typectors(type)),
                   n, jl_print_to_string((jl_value_t*)without_typectors(sig)),
                   n, jl_print_to_string(isect));
    }
}

static
jl_methlist_t *jl_method_list_insert(jl_methlist_t **pml, jl_type_t *type,
                                     jl_function_t *method, int check_amb)
{
    jl_methlist_t *l, **pl;

    assert(jl_is_tuple(type));
    l = *pml;
    while (l != NULL) {
        if (sigs_eq(type, l->sig)) {
            // method overwritten
            l->sig = type;
            l->func = method;
            return l;
        }
        l = l->next;
    }
    jl_methlist_t *newrec = (jl_methlist_t*)allocb(sizeof(jl_methlist_t));
    newrec->sig = type;
    newrec->func = method;
    pl = pml;
    l = *pml;
    while (l != NULL) {
        if (args_morespecific(type, l->sig))
            break;
        if (check_amb) {
            check_ambiguous(*pml, (jl_tuple_t*)type, (jl_tuple_t*)l->sig,
                            method->linfo ? method->linfo->name :
                            jl_symbol("anonymous"));
        }
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
    // update max_args
    jl_tuple_t *t = (jl_tuple_t*)type;
    size_t na = t->length;
    if (t->length>0 && jl_is_seq_type(jl_tupleref(t,t->length-1)))
        na--;
    if (na > mt->max_args) {
        mt->max_args = na;
    }
    return ml;
}

void jl_no_method_error(jl_sym_t *name, jl_value_t **args, size_t nargs)
{
    jl_tuple_t *argt = (jl_tuple_t*)jl_f_tuple(NULL, args, nargs);
    char *argt_str = jl_print_to_string(jl_full_type((jl_value_t*)argt));
    jl_errorf("no method %s%s", name->name, argt_str);
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
    jl_print(t);
    ios_printf(ios_stdout, "\n");
    assert(0);
}
#endif

JL_CALLABLE(jl_trampoline);

// compile-time method lookup
jl_function_t *jl_get_specialization(jl_function_t *f, jl_tuple_t *types)
{
    assert(jl_is_gf(f));
    jl_methtable_t *mt = (jl_methtable_t*)jl_t0(f->env);
    jl_methlist_t *ml = mt->cache;
    jl_function_t *sf = NULL;
    while (ml != NULL) {
        if (cache_match_by_type(&jl_tupleref(types,0), types->length,
                                (jl_tuple_t*)ml->sig)) {
            sf = ml->func;
            break;
        }
        ml = ml->next;
    }
    if (sf == NULL) {
        sf = mt_assoc_by_type(mt, types);
        if (sf == NULL)
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

JL_CALLABLE(jl_apply_generic)
{
    jl_methtable_t *mt = (jl_methtable_t*)jl_t0(env);

#ifdef JL_TRACE
    ios_printf(ios_stdout, "%s(", ((jl_sym_t*)jl_t1(env))->name);
    size_t i;
    for(i=0; i < nargs; i++) {
        if (i > 0) ios_printf(ios_stdout, ", ");
        ios_printf(ios_stdout, "%s", type_summary(jl_typeof(args[i])));
    }
    ios_printf(ios_stdout, ")\n");
#endif
#ifdef TRACE_INFERENCE
    cur_fname = ((jl_sym_t*)jl_t1(env))->name;
#endif
    jl_function_t *mfunc = jl_method_table_assoc(mt, args, nargs);

    if (mfunc == NULL) {
        jl_no_method_error((jl_sym_t*)jl_t1(env), args, nargs);
    }

    return jl_apply(mfunc, args, nargs);
}

static void print_methlist(char *name, jl_methlist_t *ml)
{
    while (ml != NULL) {
        ios_printf(ios_stdout, "%s", name);
        jl_print((jl_value_t*)ml->sig);
        ios_printf(ios_stdout, "\n");
        ml = ml->next;
    }
}

void jl_print_method_table(jl_function_t *gf)
{
    char *name = ((jl_sym_t*)jl_t1(gf->env))->name;
    jl_methtable_t *mt = (jl_methtable_t*)jl_t0(gf->env);
    print_methlist(name, mt->defs);
    //ios_printf(ios_stdout, "cache:\n");
    //print_methlist(name, mt->cache);
}

void jl_initialize_generic_function(jl_function_t *f, jl_sym_t *name)
{
    f->fptr = jl_apply_generic;
    f->env = (jl_value_t*)jl_pair((jl_value_t*)new_method_table(),
                                  (jl_value_t*)name);
}

jl_function_t *jl_new_generic_function(jl_sym_t *name)
{
    return jl_new_closure(jl_apply_generic,
                          (jl_value_t*)jl_pair((jl_value_t*)new_method_table(),
                                               (jl_value_t*)name));
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
    (void)jl_method_table_insert(jl_gf_mtable(gf), (jl_type_t*)types, meth);
}

JL_CALLABLE(jl_generic_ctor);

static jl_tuple_t *match_method(jl_value_t *type, jl_function_t *func,
                                jl_type_t *sig, jl_sym_t *name,
                                jl_tuple_t *next)
{
    jl_tuple_t *env = jl_null;
    jl_value_t *ti =
        jl_type_intersection_matching((jl_value_t*)sig, type, &env);
    env = jl_flatten_pairs(env);
    if (ti != (jl_value_t*)jl_bottom_type) {
        if (func->linfo == NULL) {
            // builtin
            return jl_tuple(5, ti, env, name, jl_null, next);
        }
        else if (func->fptr == jl_generic_ctor) {
            // a generic struct constructor
            jl_type_t *body = (jl_type_t*)jl_t1(func->env);
            // determine what kind of object this constructor call
            // would make
            jl_type_t *objt =
                jl_instantiate_type_with(body, &jl_t0(env), env->length/2);
            return jl_tuple(5, ti, env, (jl_value_t*)objt, jl_null, next);
        }
        else {
            jl_value_t *cenv;
            if (func->env != NULL)
                cenv = func->env;
            else
                cenv = (jl_value_t*)jl_null;
            return jl_tuple(5, ti, env, func->linfo, cenv, next);
        }
    }
    return NULL;
}

// returns linked tuples (argtypes, static_params, lambdainfo, cloenv, next)
static jl_tuple_t *ml_matches(jl_methlist_t *ml, jl_value_t *type,
                              jl_tuple_t *t, jl_sym_t *name)
{
    while (ml != NULL) {
        // a method is shadowed if type <: S <: m->sig where S is the
        // signature of another applicable method
        /*
          more generally?
          given arguments T
          X is shadowed if there exist other applicable methods A, B
          (possibly A==B), such that (T ⊆ A) ∧ (B ⊆ X) ∧ (A∩X ⊆ B)
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
            jl_tuple_t *matc = match_method(type, ml->func, ml->sig, name, t);
            if (matc != NULL) {
                t = matc;
                if (jl_subtype(type, (jl_value_t*)ml->sig, 0))
                    return t;
            }
        }
        ml = ml->next;
    }
    return t;
}

JL_CALLABLE(jl_new_struct_internal);

// return linked tuples (t1, M1, (t2, M2, (... ()))) of types and methods.
// t is the intersection of the type argument and the method signature,
// and M is the corresponding LambdaStaticData (jl_lambda_info_t)
DLLEXPORT
jl_value_t *jl_matching_methods(jl_function_t *gf, jl_value_t *type)
{
    jl_tuple_t *t = jl_null;
    if (!jl_is_gf(gf)) {
        if (gf->fptr == jl_new_struct_internal) {
            if (jl_is_struct_type(gf)) {
                t = jl_tuple(5, ((jl_struct_type_t*)gf)->types,
                             jl_null, gf, jl_null, t);
            }
            else {
                t = jl_tuple(5, ((jl_struct_type_t*)gf->env)->types,
                             jl_null, gf->env, jl_null, t);
            }
        }
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
