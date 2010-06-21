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
    return mt;
}

// takes arguments in the same order as jl_subtype()
typedef int (*jl_type_comparer_t)(jl_type_t *a, jl_type_t *b);

typedef int (*jl_argtuple_comparer_t)(jl_value_t **args, size_t n,
                                      jl_type_t *b);

static int exact_match(jl_value_t **args, size_t n, jl_tuple_t *sig)
{
    if (sig->length != n) return 0;
    size_t i;
    for(i=0; i < n; i++) {
        // note: because this uses jl_typeof() directly, it never
        // detects exact matches for tuples. however this is a 
        // conservative answer given the rest of the dispatch process.
        jl_value_t *decl = jl_tupleref(sig, i);
        jl_value_t *a = args[i];
        if (jl_is_tuple(decl)) {
            // tuples don't have to match exactly, to avoid caching
            // signatures for tuples of every length
            if (!jl_subtype(a, decl, 1))
                return 0;
        }
        else if (jl_is_type(a) && jl_is_tag_type(decl) &&
            ((jl_tag_type_t*)decl)->name == jl_type_type->name) {
            if (!jl_types_equal(a, jl_tparam0(decl)))
                return 0;
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
        if (exact_match(args, n, (jl_tuple_t*)ml->sig))
            return ml;
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

jl_methlist_t *jl_method_table_insert(jl_methtable_t *mt, jl_type_t *type,
                                      jl_function_t *method);

static
jl_methlist_t *jl_method_list_insert_p(jl_methlist_t **pml, jl_type_t *type,
                                       jl_function_t *method,
                                       jl_type_comparer_t pred);

static int args_match_generic(jl_type_t *a, jl_type_t *b)
{
    return (jl_type_match_morespecific(a,b) != (jl_value_t*)jl_false);
}

static int args_match(jl_type_t *a, jl_type_t *b)
{
    return jl_type_morespecific((jl_value_t*)a,(jl_value_t*)b,0);
}

static jl_methlist_t *cache_method(jl_methtable_t *mt, jl_tuple_t *type,
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
                jl_tupleset(type, i,
                            jl_type_intersection(jl_tupleref(decl,i),
                                                 (jl_value_t*)jl_tuple_type));
            }
            else {
                jl_tupleset(type, i, (jl_value_t*)jl_tuple_type);
            }
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
            jl_value_t *typetype = // Type{Type{_}}
                (jl_value_t*)jl_apply_type((jl_value_t*)jl_type_type,
                                           jl_tuple(1,jl_type_type));
            if (i < decl->length) {
                jl_tupleset(type, i, jl_type_intersection(jl_tupleref(decl,i),
                                                          typetype));
            }
            else {
                jl_tupleset(type, i, typetype);
            }
        }
    }
    // TODO: specialize method
    jl_function_t *newmeth;
    if (sparams==jl_null)
        newmeth = method;
    else
        newmeth = jl_instantiate_method(method, sparams);
    return jl_method_list_insert_p(&mt->cache, (jl_type_t*)type, newmeth,
                                   args_match);
}

jl_tag_type_t *jl_wrap_Type(jl_value_t *t);

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
    */
    jl_methlist_t *m = jl_method_list_assoc_exact(mt->cache, args, nargs);
    if (m != NULL)
        return m;

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

    m = mt->defs;
    jl_value_t *env = (jl_value_t*)jl_false;
    while (m != NULL) {
        if (jl_has_typevars((jl_value_t*)m->sig)) {
            env = jl_type_match((jl_type_t*)tt, m->sig);
            if (env != (jl_value_t*)jl_false) break;
        }
        else if (jl_tuple_subtype(args, nargs, &jl_tupleref(m->sig,0),
                                  ((jl_tuple_t*)m->sig)->length, 1, 0)) {
            break;
        }
        m = m->next;
    }

    if (env == (jl_value_t*)jl_false) {
        if (m != NULL) {
            cache_method(mt, tt, m->func, (jl_tuple_t*)m->sig, jl_null);
        }
        return m;
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

static int sigs_match(jl_type_t *a, jl_type_t *b)
{
    if (jl_has_typevars((jl_value_t*)a) || jl_has_typevars((jl_value_t*)b)) {
        return jl_types_equal_generic((jl_value_t*)a,(jl_value_t*)b);
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
      
      There is also this kind of ambiguity: foo{T,S}(T, S) vs. foo(Any,Any)
      In this case jl_types_equal() is true, but one is jl_type_morespecific
      or jl_type_match_morespecific than the other.
      To check this, jl_types_equal_generic needs to be more sophisticated
      so (T,T) is not equivalent to (Any,Any).
    */
    if (jl_has_typevars((jl_value_t*)type)) {
        return jl_method_list_insert_p(&mt->defs, type, method,
                                       args_match_generic);
    }
    else {
        return jl_method_list_insert_p(&mt->defs, type, method, args_match);
    }
}

void jl_no_method_error(jl_sym_t *name, jl_value_t **args, size_t nargs)
{
    jl_tuple_t *argt = (jl_tuple_t*)jl_f_tuple(NULL, args, nargs);
    char *argt_str = jl_print_to_string(jl_full_type((jl_value_t*)argt));
    jl_errorf("no method %s%s", name->name, argt_str);
}

//#define JL_TRACE
#ifdef JL_TRACE
static char *type_summary(jl_value_t *t)
{
    if (jl_is_tuple(t)) return "Tuple";
    if (jl_is_func_type(t)) return "Function";
    if (jl_is_some_tag_type(t))
        return ((jl_tag_type_t*)t)->name->name->name;
    assert(0);
}
#endif

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

    jl_methlist_t *m = jl_method_table_assoc(mt, args, nargs);

    if (m == NULL) {
        jl_no_method_error((jl_sym_t*)jl_t1(env), args, nargs);
    }

    return jl_apply(m->func, args, nargs);
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
    (void)jl_method_table_insert(jl_gf_mtable(gf), (jl_type_t*)types, meth);
}

JL_CALLABLE(jl_generic_ctor);

static jl_tuple_t *ml_matches(jl_methlist_t *ml, jl_value_t *type,
                              jl_tuple_t *t, jl_sym_t *name)
{
    while (ml != NULL) {
        // a method is shadowed if type <: S <: m->sig where S is the
        // signature of another applicable method
        jl_tuple_t *tt = t;
        int shadowed = 0;
        while (tt != jl_null) {
            jl_value_t *S = jl_tupleref(tt,0);
            if (jl_subtype(type, S, 0) &&
                jl_subtype(S, (jl_value_t*)ml->sig, 0)) {
                shadowed = 1;
                break;
            }
            tt = (jl_tuple_t*)jl_tupleref(tt,3);
        }
        if (!shadowed) {
            jl_tuple_t *env=jl_null;
            jl_value_t *ti =
                jl_type_intersection_matching((jl_value_t*)ml->sig, type, &env);
            env = jl_flatten_pairs(env);
            if (ti != (jl_value_t*)jl_bottom_type) {
                if (ml->func->linfo == NULL) {
                    // builtin
                    t = jl_tuple(4, ti, env, name, t);
                }
                else if (ml->func->fptr == jl_generic_ctor) {
                    // a generic struct constructor
                    jl_type_t *body =
                        ((jl_typector_t*)jl_t1(ml->func->env))->body;
                    // determine what kind of object this constructor call
                    // would make
                    jl_type_t *objt =
                        jl_instantiate_type_with(body,
                                                 &jl_t0(env),env->length/2);
                    t = jl_tuple(4, ti, env, (jl_value_t*)objt, t);
                }
                /*
                else if (ml->func->fptr == jl_new_struct_internal) {
                }
                */
                else {
                    t = jl_tuple(4, ti, env, ml->func->linfo, t);
                }
            }
        }
        ml = ml->next;
    }
    return t;
}

// return linked tuples (t1, M1, (t2, M2, (... ()))) of types and methods.
// t is the intersection of the type argument and the method signature,
// and M is the corresponding LambdaStaticData (jl_lambda_info_t)
jl_value_t *jl_matching_methods(jl_function_t *gf, jl_value_t *type)
{
    jl_tuple_t *t = jl_null;
    if (!jl_is_gf(gf)) return (jl_value_t*)t;
    jl_methtable_t *mt = jl_gf_mtable(gf);
    jl_sym_t *gfname = jl_gf_name(gf);
    t = ml_matches(mt->defs, type, t, gfname);
    return (jl_value_t*)t;
}

int jl_is_builtin(jl_value_t *v)
{
    return ((jl_is_func(v) && (((jl_function_t*)v)->linfo==NULL) &&
             !jl_is_gf(v)) ||
            jl_typeis(v,jl_intrinsic_type));
}

int jl_is_genericfunc(jl_value_t *v)
{
    return (jl_is_func(v) && jl_is_gf(v));
}
