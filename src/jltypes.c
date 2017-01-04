// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  Types
  . type predicates (subtype) and type matching
  . type union and intersection
  . builtin type definitions
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"

#ifdef __cplusplus
extern "C" {
#endif

jl_datatype_t *jl_any_type;
jl_datatype_t *jl_type_type;
jl_methtable_t *jl_type_type_mt;
jl_datatype_t *jl_typename_type;
jl_datatype_t *jl_sym_type;
jl_datatype_t *jl_symbol_type;
jl_datatype_t *jl_ssavalue_type;
jl_datatype_t *jl_abstractslot_type;
jl_datatype_t *jl_slotnumber_type;
jl_datatype_t *jl_typedslot_type;
jl_datatype_t *jl_simplevector_type;
jl_typename_t *jl_tuple_typename;
jl_tupletype_t *jl_anytuple_type;
jl_datatype_t *jl_anytuple_type_type;
jl_typename_t *jl_vecelement_typename;
jl_datatype_t *jl_vararg_type;
jl_datatype_t *jl_tvar_type;
jl_datatype_t *jl_uniontype_type;
jl_datatype_t *jl_datatype_type;
jl_datatype_t *jl_function_type;
jl_datatype_t *jl_builtin_type;

jl_value_t *jl_bottom_type;
jl_datatype_t *jl_abstractarray_type;
jl_datatype_t *jl_densearray_type;

jl_datatype_t *jl_bool_type;
jl_datatype_t *jl_char_type;
jl_datatype_t *jl_int8_type;
jl_datatype_t *jl_uint8_type;
jl_datatype_t *jl_int16_type;
jl_datatype_t *jl_uint16_type;
jl_datatype_t *jl_int32_type;
jl_datatype_t *jl_uint32_type;
jl_datatype_t *jl_int64_type;
jl_datatype_t *jl_uint64_type;
jl_datatype_t *jl_float16_type;
jl_datatype_t *jl_float32_type;
jl_datatype_t *jl_float64_type;
jl_datatype_t *jl_floatingpoint_type;
jl_datatype_t *jl_number_type;
jl_datatype_t *jl_complex_type;
jl_datatype_t *jl_signed_type;

JL_DLLEXPORT jl_value_t *jl_emptytuple=NULL;
jl_svec_t *jl_emptysvec;
jl_value_t *jl_nothing;

jl_cgparams_t jl_default_cgparams = {1, 1, 1, 1, 1, 1, 1, {NULL, NULL, NULL}};

// --- type properties and predicates ---

STATIC_INLINE int is_unspec(jl_datatype_t *dt)
{
    return (jl_datatype_t*)dt->name->primary == dt;
}

static int jl_has_typevars__(jl_value_t *v, int incl_wildcard, jl_value_t **p, size_t np)
{
    size_t i;
    if (jl_typeis(v, jl_tvar_type)) {
        if (jl_has_typevars__(((jl_tvar_t*)v)->ub, incl_wildcard, p, np) ||
            jl_has_typevars__(((jl_tvar_t*)v)->lb, incl_wildcard, p, np))
            return 1;
        if (p != NULL) {
            for (i = 0; i < np; i++) {
                if (v == p[i])
                    return 1;
            }
            return 0;
        }
        if (!((jl_tvar_t*)v)->bound)
            return incl_wildcard;
        return 1;
    }
    if (jl_is_typector(v))
        return incl_wildcard;
    jl_svec_t *t;
    int expect = -1;
    if (jl_is_uniontype(v)) {
        t = ((jl_uniontype_t*)v)->types;
    }
    else if (jl_is_datatype(v)) {
        if (is_unspec((jl_datatype_t*)v))
            return 0; // TODO: fix expect in this case
        if (p == NULL) {
            if (incl_wildcard)
                expect = ((jl_datatype_t*)v)->haswildcard;
            else
                expect = ((jl_datatype_t*)v)->hastypevars;
#ifdef NDEBUG
            return expect;
#endif
        }
        t = ((jl_datatype_t*)v)->parameters;
    }
    else {
        return 0;
    }
    size_t l = jl_svec_len(t);
    for (i = 0; i < l; i++) {
        jl_value_t *elt = jl_svecref(t, i);
        if (elt != v) {
            if (jl_has_typevars__(elt, incl_wildcard, p, np)) {
                if (expect >= 0) assert(expect);
                return 1;
            }
        }
    }
    // probably not necessary; no reason to use match() instead of subtype()
    // on the unconstrained version of a type
    //if (jl_is_typector(v))
    //    return jl_svec_len((((jl_typector_t*)v)->parameters) > 0);
    if (expect >= 0) assert(!expect);
    return 0;
}

JL_DLLEXPORT int jl_has_typevars_(jl_value_t *v, int incl_wildcard)
{
    if (jl_is_typevar(v)) return 1;
    return jl_has_typevars__(v, incl_wildcard, NULL, 0);
}

static int jl_has_typevars_from(jl_value_t *v, jl_svec_t *p)
{
    if (jl_svec_len(p) == 0) return 0;
    return jl_has_typevars__(v, 0, jl_svec_data(p), jl_svec_len(p));
}

static int jl_has_typevars_from_v(jl_value_t *v, jl_value_t **p, size_t np)
{
    if (np == 0) return 0;
    return jl_has_typevars__(v, 0, p, np);
}

JL_DLLEXPORT int jl_has_typevars(jl_value_t *v)
{
    if (jl_is_typevar(v)) return 1;
    return jl_has_typevars__(v, 0, NULL, 0);
}

JL_DLLEXPORT int (jl_is_leaf_type)(jl_value_t *v)
{
    if (jl_is_datatype(v)) {
        int isleaf = ((jl_datatype_t*)v)->isleaftype;
#ifdef NDEBUG
        return isleaf;
#else
        if (((jl_datatype_t*)v)->abstract) {
            int x = 0;
            if (jl_is_type_type(v)) {
                x = !jl_has_typevars(jl_tparam0(v));
            }
            assert(x == isleaf);
            return x;
        }
        jl_svec_t *t = ((jl_datatype_t*)v)->parameters;
        size_t l = jl_svec_len(t);
        if (((jl_datatype_t*)v)->name == jl_tuple_typename) {
            for(int i=0; i < l; i++) {
                if (!jl_is_leaf_type(jl_svecref(t,i))) {
                    assert(!isleaf);
                    return 0;
                }
            }
        }
        else {
            for(int i=0; i < l; i++) {
                jl_value_t *p = jl_svecref(t, i);
                if (jl_has_typevars(p)) {
                    assert(!isleaf);
                    return 0;
                }
            }
        }
        assert(isleaf);
        return 1;
#endif
    }
    return 0;
}

static int type_eqv_(jl_value_t *a, jl_value_t *b);

// Return true for any type (Integer or Unsigned) that can fit in a
// size_t and pass back value, else return false
JL_DLLEXPORT int jl_get_size(jl_value_t *val, size_t *pnt)
{
    if (jl_is_long(val)) {
        ssize_t slen = jl_unbox_long(val);
        if (slen < 0)
            jl_errorf("size or dimension is negative: %d", slen);
        *pnt = slen;
        return 1;
    }
    return 0;
}
// --- type union ---

static int count_union_components(jl_value_t **types, size_t n)
{
    size_t i, c=0;
    for(i=0; i < n; i++) {
        jl_value_t *e = types[i];
        if (jl_is_uniontype(e)) {
            jl_svec_t *ts = ((jl_uniontype_t*)e)->types;
            c += count_union_components(jl_svec_data(ts), jl_svec_len(ts));
        }
        else {
            c++;
        }
    }
    return c;
}

static void flatten_type_union(jl_value_t **types, size_t n, jl_value_t **out, size_t *idx)
{
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *e = types[i];
        if (jl_is_uniontype(e)) {
            jl_svec_t *ts = ((jl_uniontype_t*)e)->types;
            flatten_type_union(jl_svec_data(ts), jl_svec_len(ts), out, idx);
        }
        else {
            out[*idx] = e;
            (*idx)++;
        }
    }
}

static int union_elt_morespecific(const void *a, const void *b)
{
    jl_value_t *va = *(jl_value_t**)a;
    jl_value_t *vb = *(jl_value_t**)b;
    if (jl_args_morespecific(va, vb))
        return -1;
    // impose a partially-arbitrary ordering on Union elements, to make it more
    // likely that many Unions will be identical and can be merged.
    // NOTE: we know !(a <: b) && !(b <: a), since otherwise one would have
    // been eliminated from the Union.
    return jl_object_id(va) < jl_object_id(vb) ? -1 : 1;
}

// NOTE: this is a hack to avoid simplifying type unions too early inside
// type definitions. (issue #2365)
int inside_typedef = 0;

static jl_svec_t *jl_compute_type_union(jl_value_t **types, size_t ntypes)
{
    size_t n = count_union_components(types, ntypes);
    jl_value_t **temp;
    JL_GC_PUSHARGS(temp, n+1);
    size_t idx=0;
    flatten_type_union(types, ntypes, temp, &idx);
    assert(idx == n);
    size_t i, j, ndel=0;
    for(i=0; i < n; i++) {
        for(j=0; j < n; j++) {
            if (j != i && temp[i] && temp[j]) {
                if (temp[i] == temp[j] ||
                    (!jl_has_typevars(temp[i]) && !jl_has_typevars(temp[j]) &&
                     !(inside_typedef && (jl_is_typevar(temp[i]) ||
                                          jl_is_typevar(temp[j]))) &&
                     (type_eqv_(temp[i], temp[j]) ||
                      jl_subtype(temp[i], temp[j], 0)))) {
                    temp[i] = NULL;
                    ndel++;
                }
            }
        }
    }
    temp[n] = NULL;
    jl_svec_t *result = jl_alloc_svec_uninit(n - ndel);
    temp[n] = (jl_value_t*)result; // root result tuple while sorting
    j=0;
    for(i=0; i < n; i++) {
        if (temp[i] != NULL) {
            jl_svecset(result, j, temp[i]);
            j++;
        }
    }
    assert(j == n-ndel);
    // sort Union components by specificity, so "complex" type Unions work as
    // long as there are no ambiguities (see e.g. issue #126).
    // TODO: maybe warn about ambiguities
    qsort(jl_svec_data(result), j, sizeof(jl_value_t*), union_elt_morespecific);
    JL_GC_POP();
    return result;
}

static jl_value_t *jl_type_union_v(jl_value_t **ts, size_t n)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (n == 0) return (jl_value_t*)jl_bottom_type;
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *pi = ts[i];
        if (!(jl_is_type(pi) || jl_is_typevar(pi)) || jl_is_vararg_type(pi))
            jl_type_error_rt("Union", "parameter", (jl_value_t*)jl_type_type, pi);
    }
    if (n == 1) return ts[0];
    jl_svec_t *types = jl_compute_type_union(ts, n);
    if (jl_svec_len(types) == 0) return (jl_value_t*)jl_bottom_type;
    if (jl_svec_len(types) == 1) return jl_svecref(types, 0);
    JL_GC_PUSH1(&types);
    jl_uniontype_t *tu =
        (jl_uniontype_t*)jl_gc_alloc(ptls, sizeof(jl_uniontype_t),
                                     jl_uniontype_type);
    tu->types = types;
    jl_gc_wb(tu, types);
    JL_GC_POP();
    return (jl_value_t*)tu;
}

JL_DLLEXPORT jl_value_t *jl_type_union(jl_svec_t *types)
{
    return jl_type_union_v(jl_svec_data(types), jl_svec_len(types));
}

// --- type intersection ---

typedef enum {invariant, covariant} variance_t;

#define MAX_CENV_SIZE 128

typedef struct {
    jl_value_t **data;
    size_t n;
    jl_svec_t *tvars;
} cenv_t;

STATIC_INLINE int is_bnd(jl_tvar_t *tv, cenv_t *env)
{
    if (env->tvars == jl_emptysvec)
        return tv->bound;
    if (jl_is_typevar(env->tvars))
        return (jl_tvar_t*)env->tvars == tv;
    for(size_t i=0; i < jl_svec_len(env->tvars); i++) {
        if ((jl_tvar_t*)jl_svecref(env->tvars,i) == tv)
            return 1;
    }
    return 0;
}

STATIC_INLINE int is_btv(jl_value_t *v)
{
    return jl_is_typevar(v) && ((jl_tvar_t*)v)->bound;
}

static void extend_(jl_value_t *var, jl_value_t *val, cenv_t *soln, int allowself)
{
    if (!allowself && var == val)
        return;
    for(int i=0; i < soln->n; i+=2) {
        if (soln->data[i]==var &&
            (soln->data[i+1]==val || (!jl_is_typevar(val) &&
                                      type_eqv_(soln->data[i+1],val))))
            return;
        if (soln->data[i]==val && soln->data[i+1]==var)
            return;
    }
    if (soln->n >= MAX_CENV_SIZE)
        jl_error("type too large");
    soln->data[soln->n++] = var;
    soln->data[soln->n++] = val;
}

static void extend(jl_value_t *var, jl_value_t *val, cenv_t *soln)
{
    extend_(var, val, soln, 0);
}

static jl_value_t *jl_type_intersect(jl_value_t *a, jl_value_t *b,
                                     cenv_t *penv, cenv_t *eqc,
                                     int *recheck_tuple_intersection,
                                     variance_t var);

static jl_value_t *intersect_union(jl_uniontype_t *a, jl_value_t *b,
                                   cenv_t *penv, cenv_t *eqc,
                                   int *recheck_tuple_intersection,
                                   variance_t var)
{
    int eq0 = eqc->n, co0 = penv->n;
    size_t i, l = jl_svec_len(a->types);
    // shortcut an easy case: union contains type b
    if (!jl_is_typevar(b)) {
        for(i=0; i < l; i++) {
            if (jl_svecref(a->types,i) == b)
                return b;
        }
    }
    jl_svec_t *t = jl_alloc_svec(l);
    JL_GC_PUSH1(&t);
    for(i=0; i < l; i++) {
        int eq_l = eqc->n, co_l = penv->n;
        jl_value_t *ti = jl_type_intersect(jl_svecref(a->types,i), b,
                                           penv, eqc, recheck_tuple_intersection, var);
        if (ti == (jl_value_t*)jl_bottom_type) {
            eqc->n = eq0; penv->n = co0;
            ti = jl_type_intersect(jl_svecref(a->types,i), b,
                                   penv, eqc, recheck_tuple_intersection, var);
            if (ti != (jl_value_t*)jl_bottom_type) {
                // tvar conflict among union elements; keep the conflicting
                // constraints rolled back
                eqc->n = eq0; penv->n = co0;
            }
            else {
                // union element doesn't overlap no matter what.
                // so remove only its constraints.
                eqc->n = eq_l; penv->n = co_l;
            }
        }
        jl_svecset(t, i, ti);
    }
    // problem: an intermediate union type we make here might be too
    // complex, even though the final type after typevars are replaced
    // might be ok.
    jl_value_t *tu = jl_type_union(t);
    JL_GC_POP();
    return tu;
}

/*
Simplification of varargs tuple types:
 JL_TUPLE_FIXED: tuples of known length (e.g., JL_VARARG_NONE or JL_VARARG_INT)
 JL_TUPLE_VAR:   tuples of unknown length (e.g., JL_VARARG_BOUND or JL_VARARG_UNBOUND)

In some cases, JL_VARARG_BOUND tuples get described as JL_TUPLE_FIXED,
if the constraints on length are already known.

lenr = "representation length" (the number of parameters)
lenf = "full length" (including the Vararg length, if known)

In general, lenf >= lenr-1. The lower bound is achieved only for a Vararg of length 0.
*/
typedef enum {
    JL_TUPLE_FIXED = 0,
    JL_TUPLE_VAR   = 1
} jl_tuple_lenkind_t;

// Set the parameters for a single tuple
// returns lenf, sets kind and lenkind
static size_t data_vararg_params(jl_value_t **data, size_t lenr, cenv_t *eqc, jl_vararg_kind_t *kind, jl_tuple_lenkind_t *lenkind)
{
    size_t lenf = lenr;
    int i;
    if (lenr == 0) {
        *kind = JL_VARARG_NONE;
        *lenkind = JL_TUPLE_FIXED;
        return lenf;
    }
    *lenkind = JL_TUPLE_VAR;
    *kind = jl_vararg_kind(data[lenr-1]);
    if (*kind == JL_VARARG_NONE || *kind == JL_VARARG_INT)
        *lenkind = JL_TUPLE_FIXED;
    if (*kind == JL_VARARG_INT || *kind == JL_VARARG_BOUND) {
        // try to set N from eqc parameters
        jl_value_t *N = jl_tparam1(data[lenr-1]);
        if (!jl_is_long(N) && eqc != NULL) {
            for (i = 0; i < eqc->n; i+=2)
                if (eqc->data[i] == N && jl_is_long(eqc->data[i+1])) {
                    N = eqc->data[i+1];
                    break;
                }
        }
        if (jl_is_long(N)) {
            lenf += jl_unbox_long(N)-1;
            *lenkind = JL_TUPLE_FIXED;
        }
    }
    return lenf;
}

static size_t tuple_vararg_params(jl_svec_t *a, cenv_t *eqc, jl_vararg_kind_t *kind, jl_tuple_lenkind_t *lenkind)
{
    return data_vararg_params(jl_svec_data(a), jl_svec_len(a), eqc, kind, lenkind);
}

jl_datatype_t *jl_wrap_vararg(jl_value_t *t, jl_value_t *n)
{
    if (n == NULL) {
        if (t == NULL)
            return (jl_datatype_t*)jl_instantiate_type_with((jl_value_t*)jl_vararg_type, NULL, 0);
        jl_value_t *env[2];
        env[0] = jl_tparam0(jl_vararg_type);
        env[1] = t;
        return (jl_datatype_t*)jl_instantiate_type_with((jl_value_t*)jl_vararg_type, env, 1);
    }
    jl_value_t *env[4];
    env[0] = jl_tparam0(jl_vararg_type);
    env[1] = t;
    env[2] = jl_tparam1(jl_vararg_type);
    env[3] = n;
    return (jl_datatype_t*)jl_instantiate_type_with((jl_value_t*)jl_vararg_type, env, 2);
}

/*
Tuple intersection
Stage 1: compute lengths of each tuple
--------------------------------------
See above

Stage 2: paired length analysis
-------------------------------
Check and combine lengths.  In cells of the following table,
- row 1 is the criterion that must be satisfied, or Bottom will be returned
- row 2 is the allocated length for the output tuple
- row 3, if present, indicates any additional steps taken at the time
  of length computation

                             b
                 FIXED                 VAR
          |---------------------------------------|
          | alenf == blenf   |   alenf+1 >= blenr |
    FIXED |      alenf       |        alenf       |
          |                  |        bind b?     |
a         |---------------------------------------|
          | blenf+1 >= alenr |                    |
    VAR   |      blenf       |  max(alenr,blenr)  |
          |      bind a?     |        flag?       |
          |---------------------------------------|

"bind" is performed if the VAR tuple is of state BOUND, using (for
the b BOUND case) N == alenf-blenr+1 for b's length parameter N.

"flag" is set if at least one of the tuples is of state BOUND. With
this, we signify that the intersection of these tuples is going to
have to be repeated once all lengths are constrained.

Stage 3: slot type intersection
-------------------------------
Iterate over each slot of the _output_ tuple, intersecting
corresponding pairs of types.  Any intersection failure causes Bottom
to be returned, with one exception illustrated by:
   typeintersect(Tuple{A, Vararg{B}}, Tuple{A, Vararg{C}}) == Tuple{A}
where typeintersect(B,C) == Bottom.
*/

static jl_value_t *intersect_tuple(jl_datatype_t *a, jl_datatype_t *b,
                                   cenv_t *penv, cenv_t *eqc,
                                   int *recheck_tuple_intersection, // "flag" above
                                   variance_t var)
{
    jl_svec_t *ap = a->parameters, *bp = b->parameters;
    size_t alenr = jl_svec_len(ap), blenr = jl_svec_len(bp);
    size_t alenf, blenf;
    jl_vararg_kind_t akind, bkind;
    jl_tuple_lenkind_t alenkind, blenkind;
    int bottom = 0;
    size_t n;
    // Stage 1
    alenf = tuple_vararg_params(ap, eqc, &akind, &alenkind);
    blenf = tuple_vararg_params(bp, eqc, &bkind, &blenkind);
    // Stage 2
    if (alenkind == JL_TUPLE_FIXED && blenkind == JL_TUPLE_FIXED) {
        bottom = alenf != blenf;
        n = alenf;
    }
    else if (alenkind == JL_TUPLE_FIXED && blenkind == JL_TUPLE_VAR) {
        bottom = alenf+1 < blenf;
        n = alenf;
        if (bkind == JL_VARARG_BOUND)
            extend(jl_tparam1(jl_svecref(bp, blenr-1)), jl_box_long(alenf-blenr+1), eqc);
    }
    else if (alenkind == JL_TUPLE_VAR && blenkind == JL_TUPLE_FIXED) {
        bottom = blenf+1 < alenf;
        n = blenf;
        if (akind == JL_VARARG_BOUND)
            extend(jl_tparam1(jl_svecref(ap, alenr-1)), jl_box_long(blenf-alenr+1), eqc);
    }
    else {
        n = alenr > blenr ? alenr : blenr;
        // Do we need to store "at least N" constraints in penv?
        // Formerly, typeintersect(Tuple{A,Vararg{B}}, NTuple{N,C}) did that
        if (akind == JL_VARARG_BOUND || bkind == JL_VARARG_BOUND)
            *recheck_tuple_intersection = 1;
    }
    if (bottom) return (jl_value_t*) jl_bottom_type;
    if (n == 0) return jl_typeof(jl_emptytuple);
    jl_svec_t *tc = jl_alloc_svec(n);
    jl_value_t *result = (jl_value_t*)tc;
    jl_value_t *ce = NULL;
    JL_GC_PUSH2(&tc, &ce);
    size_t ai=0, bi=0, ci;
    jl_value_t *ae=NULL, *be=NULL, *an=NULL, *bn=NULL;
    int aseq=0, bseq=0;
    // Stage 3
    for(ci=0; ci < n; ci++) {
        if (ai < alenr) {
            ae = jl_svecref(ap,ai);
            if (jl_is_vararg_type(ae)) {
                if (alenkind != JL_TUPLE_FIXED) {
                    an = jl_tparam1(ae);
                    aseq = 1;
                }
                ae = jl_tparam0(ae);
            }
            ai++;
        }
        if (bi < blenr) {
            be = jl_svecref(bp,bi);
            if (jl_is_vararg_type(be)) {
                if (blenkind != JL_TUPLE_FIXED) {
                    bn = jl_tparam1(be);
                    bseq=1;
                }
                be = jl_tparam0(be);
            }
            bi++;
        }
        assert(ae!=NULL && be!=NULL);
        ce = jl_type_intersect(ae, be, penv, eqc, recheck_tuple_intersection, var);
        if (ce == (jl_value_t*)jl_bottom_type) {
            if (var!=invariant && aseq && bseq) {
                // (X∩Y)==∅ → (X...)∩(Y...) == ()
                // We don't need to set bindings here because
                //   *recheck_tuple_intersection = 1
                if (n == 1) {
                    JL_GC_POP();
                    return (jl_value_t*)jl_typeof(jl_emptytuple);
                }
                jl_svec_set_len_unsafe(tc, jl_svec_len(tc) - 1);
                goto done_intersect_tuple;
            }
            JL_GC_POP();
            return (jl_value_t*)jl_bottom_type;
        }
        if (aseq && bseq)
            ce = (jl_value_t*)jl_wrap_vararg(ce, akind==JL_VARARG_BOUND ? bn : an);
        jl_svecset(tc, ci, ce);
    }
 done_intersect_tuple:
    result = (jl_value_t*)jl_apply_tuple_type(tc);
    JL_GC_POP();
    return result;
}

static jl_value_t *intersect_tag(jl_datatype_t *a, jl_datatype_t *b,
                                 cenv_t *penv, cenv_t *eqc,
                                 int *recheck_tuple_intersection,
                                 variance_t var)
{
    assert(a->name == b->name);
    assert(jl_svec_len(a->parameters) == jl_svec_len(b->parameters));
    jl_svec_t *p = jl_alloc_svec(jl_svec_len(a->parameters));
    JL_GC_PUSH1(&p);
    jl_value_t *ti;
    size_t i;
    for(i=0; i < jl_svec_len(p); i++) {
        jl_value_t *ap = jl_svecref(a->parameters,i);
        jl_value_t *bp = jl_svecref(b->parameters,i);
        if (jl_is_typevar(ap)) {
            if (var==invariant && jl_is_typevar(bp)) {
                if (((jl_tvar_t*)ap)->bound != ((jl_tvar_t*)bp)->bound) {
                    JL_GC_POP();
                    return (jl_value_t*)jl_bottom_type;
                }
                if ((is_unspec(a) && is_bnd((jl_tvar_t*)bp,penv)) ||
                    (is_bnd((jl_tvar_t*)ap,penv) && is_unspec(b))) {
                    // Foo{T} and Foo can never be equal since the former
                    // is always a subtype of the latter
                    JL_GC_POP();
                    return (jl_value_t*)jl_bottom_type;
                }
            }
            ti = jl_type_intersect(ap, bp, penv, eqc, recheck_tuple_intersection, invariant);
            if (bp == (jl_value_t*)jl_bottom_type &&
                !((jl_tvar_t*)ap)->bound) {
                // "Union{}" as a type parameter
                jl_svecset(p, i, ti);
                continue;
            }
        }
        else if (jl_is_typevar(bp)) {
            ti = jl_type_intersect(ap, bp, penv, eqc, recheck_tuple_intersection, invariant);
            if (ap == (jl_value_t*)jl_bottom_type &&
                !((jl_tvar_t*)bp)->bound) {
                // "Union{}" as a type parameter
                jl_svecset(p, i, ti);
                continue;
            }
        }
        else {
            int tva = jl_has_typevars_(ap,0);
            int tvb = jl_has_typevars_(bp,0);
            if (tva || tvb) {
                if (jl_subtype_invariant(ap,bp,0) ||
                    jl_subtype_invariant(bp,ap,0)) {
                    ti = jl_type_intersect(ap, bp, penv, eqc, recheck_tuple_intersection, invariant);
                }
                else {
                    ti = (jl_value_t*)jl_bottom_type;
                }
            }
            else if (type_eqv_(ap,bp)) {
                ti = ap;
                if (ti == (jl_value_t*)jl_bottom_type) {
                    // "Union{}" as a type parameter
                    jl_svecset(p, i, ti);
                    continue;
                }
            }
            else {
                ti = (jl_value_t*)jl_bottom_type;
            }
        }
        if (ti == (jl_value_t*)jl_bottom_type) {
            JL_GC_POP();
            return (jl_value_t*)jl_bottom_type;
        }
        jl_svecset(p, i, ti);
    }
    if (a->name->primary != NULL) {
        jl_value_t *res = (jl_value_t*)jl_apply_type(a->name->primary, p);
        JL_GC_POP();
        return res;
    }
    assert(0 && "not yet implemented");
    return NULL;
}

static long meet_tuple_lengths(long bv, long vv, int *bot)
{
    /*
      do a meet over the lattice of tuple lengths:
                       >=0
                        | \
                        |  0
                       >=1
                        | \
                        |  1
                       >=2
                        | \
                        |  2
                       ...
      ">=N" is represented as ~N
    */
    if (bv < 0) {
        if (vv < 0) {
            if (bv < vv)
                return bv;
            else
                return vv;
        }
        else {
            if (~bv > vv) {
                *bot = 1;
                return 0;
            }
        }
    }
    else {
        if (vv < 0) {
            if (~vv > bv) {
                *bot = 1;
                return 0;
            }
            return bv;
        }
        else {
            if (bv != vv) {
                *bot = 1;
                return 0;
            }
        }
    }
    return vv;
}

static int match_intersection_mode = 0;
static jl_value_t *meet_tvars(jl_tvar_t *a, jl_tvar_t *b);

static jl_value_t *intersect_typevar(jl_tvar_t *a, jl_value_t *b,
                                     cenv_t *penv, cenv_t *eqc,
                                     int *recheck_tuple_intersection,
                                     variance_t var)
{
    jl_value_t *both=NULL;
    jl_tvar_t *new_b=NULL;
    JL_GC_PUSH3(&b, &both, &new_b);
    if (jl_subtype(b, (jl_value_t*)a, 0)) {
        if (!is_bnd(a,penv)) {
            JL_GC_POP();
            return b;
        }
    }
    else if (var==invariant && !jl_has_typevars_(b,0)) {
        // for typevar a and non-typevar type b, b must be within a's bounds
        // in invariant contexts.
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }
    else if (jl_subtype((jl_value_t*)a, b, 0)) {
        /*
          TODO: get sharper types when the overlap between a typevar and
          a type is not simple. Ex:
          tintersect(Type{Array{T,n}}, Type{typevar(:_,Vector)})
          should give Type{_<:Vector}
        */
        if (jl_is_typevar(b)) {
            if (!is_bnd((jl_tvar_t*)b,penv)) {
                JL_GC_POP();
                return (jl_value_t*)a;
            }
        }
        else {
            if (a->ub == jl_bottom_type) {
                JL_GC_POP();
                return jl_bottom_type;
            }
            if (!is_bnd(a,penv)) {
                JL_GC_POP();
                return (jl_value_t*)a;
            }
        }
    }
    else {
        b = jl_type_intersect(a->ub, b, penv, eqc, recheck_tuple_intersection, covariant);
        if (b == jl_bottom_type) {
            JL_GC_POP();
            return b;
        }
    }
    if ((jl_value_t*)a == b) {
        JL_GC_POP();
        return b;
    }
    if (var == invariant) {
        if (!jl_has_typevars_(b,0) && !jl_is_typevar(b)) {
            int i;
            for(i=0; i < eqc->n; i+=2) {
                if (eqc->data[i] == (jl_value_t*)a) {
                    jl_value_t *v = eqc->data[i+1];
                    if (jl_is_typevar(v))
                        continue;
                    if (!jl_types_equal(v, b)) {
                        JL_GC_POP();
                        return (jl_value_t*)jl_bottom_type;
                    }
                    break;
                }
            }
            if (i >= eqc->n)
                extend((jl_value_t*)a, b, eqc);
            JL_GC_POP();
            return (jl_value_t*)a;
        }
        if (jl_is_typevar(b)) {
            both = meet_tvars(a, (jl_tvar_t*)b);
            if (both == jl_bottom_type) {
                JL_GC_POP();
                return both;
            }
            if (!jl_is_typevar(both))
                both = (jl_value_t*)jl_new_typevar(underscore_sym, jl_bottom_type, both);
            extend((jl_value_t*)a, both, penv);
            extend((jl_value_t*)b, both, penv);
        }
        if (is_btv(b))
            extend(b, (jl_value_t*)a, eqc);
        else
            extend((jl_value_t*)a, b, eqc);
    }
    else {
        int i;
        for(i=0; i < penv->n; i+=2) {
            if (penv->data[i] == (jl_value_t*)a && !jl_is_typevar(penv->data[i+1])) {
                if (jl_types_equal(b, penv->data[i+1])) {
                    JL_GC_POP();
                    return (jl_value_t*)a;
                }
                break;
            }
        }
        if (jl_is_typevar(b)) {
            for(i=0; i < penv->n; i+=2) {
                if (penv->data[i] == b && !jl_is_typevar(penv->data[i+1])) {
                    jl_value_t *ti = jl_type_intersection((jl_value_t*)a, penv->data[i+1]);
                    if (ti == (jl_value_t*)jl_bottom_type) {
                        JL_GC_POP();
                        return ti;
                    }
                    break;
                }
            }
            for(i=0; i < eqc->n; i+=2) {
                if (eqc->data[i] == b && !jl_is_typevar(eqc->data[i+1])) {
                    jl_value_t *ti = jl_type_intersection((jl_value_t*)a, eqc->data[i+1]);
                    if (ti == (jl_value_t*)jl_bottom_type) {
                        JL_GC_POP();
                        return ti;
                    }
                    break;
                }
            }
        }
        extend((jl_value_t*)a, b, penv);
        if (jl_is_typevar(b)) {
            JL_GC_POP();
            return (jl_value_t*)a;
        }
        else {
            new_b = jl_new_typevar(underscore_sym, jl_bottom_type, b);
            extend((jl_value_t*)new_b, b, penv);
            extend((jl_value_t*)new_b, (jl_value_t*)a, penv);
            JL_GC_POP();
            return (jl_value_t*)new_b;
        }
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}

static jl_value_t *approxify_type(jl_datatype_t *dt, jl_svec_t *pp, int *recheck_tuple_intersection)
{
    size_t i, l = jl_svec_len(dt->parameters);
    jl_svec_t *p = jl_alloc_svec(l);
    JL_GC_PUSH1(&p);
    for(i=0; i < l; i++) {
        jl_value_t *el = jl_svecref(dt->parameters, i);
        if (jl_has_typevars_from(el, pp))
            jl_svecset(p, i, jl_new_typevar(underscore_sym, jl_bottom_type, el));
        else
            jl_svecset(p, i, el);
    }
    jl_value_t *nt = jl_apply_type(dt->name->primary, p);
    JL_GC_POP();
    return nt;
}

static jl_value_t *jl_type_intersect(jl_value_t *a, jl_value_t *b,
                                     cenv_t *penv, cenv_t *eqc,
                                     int *recheck_tuple_intersection,
                                     variance_t var)
{
    if (jl_is_typector(a))
        a = (jl_value_t*)((jl_typector_t*)a)->body;
    if (jl_is_typector(b))
        b = (jl_value_t*)((jl_typector_t*)b)->body;
    if (a == b) return a;
    if (jl_is_typevar(a)) {
        if (var == covariant && !((jl_tvar_t*)a)->bound)
            a = ((jl_tvar_t*)a)->ub;
        else if (a != jl_ANY_flag)
            return intersect_typevar((jl_tvar_t*)a, b, penv, eqc, recheck_tuple_intersection, var);
    }
    if (jl_is_typevar(b)) {
        if (var == covariant && !((jl_tvar_t*)b)->bound)
            b = ((jl_tvar_t*)b)->ub;
        else if (b != jl_ANY_flag)
            return intersect_typevar((jl_tvar_t*)b, a, penv, eqc, recheck_tuple_intersection, var);
    }
    if (a == (jl_value_t*)jl_bottom_type || b == (jl_value_t*)jl_bottom_type)
        return (jl_value_t*)jl_bottom_type;
    if (!jl_has_typevars(a) && !jl_has_typevars(b)) {
        if (jl_subtype(a, b, 0))
            return a;
        if (jl_subtype(b, a, 0))
            return b;
    }
    // union
    if (jl_is_uniontype(a))
        return intersect_union((jl_uniontype_t*)a, b, penv, eqc, recheck_tuple_intersection, var);
    if (jl_is_uniontype(b))
        return intersect_union((jl_uniontype_t*)b, a, penv, eqc, recheck_tuple_intersection, var);
    if (a == (jl_value_t*)jl_any_type || a == jl_ANY_flag) return b;
    if (b == (jl_value_t*)jl_any_type || b == jl_ANY_flag) return a;
    // tuple
    if (jl_is_tuple_type(a)) {
        if (jl_is_tuple_type(b)) {
            return intersect_tuple((jl_datatype_t*)a, (jl_datatype_t*)b, penv, eqc, recheck_tuple_intersection, var);
        }
    }
    if (jl_is_tuple_type(b)) {
        return jl_type_intersect(b, a, penv, eqc, recheck_tuple_intersection, var);
    }
    // tag
    if (!jl_is_datatype(a) || !jl_is_datatype(b))
        return (jl_value_t*)jl_bottom_type;
    jl_datatype_t *tta = (jl_datatype_t*)a;
    jl_datatype_t *ttb = (jl_datatype_t*)b;
    if (tta->name == ttb->name)
        return (jl_value_t*)intersect_tag(tta, ttb, penv, eqc, recheck_tuple_intersection, var);
    jl_datatype_t *super = NULL;
    jl_datatype_t *sub = NULL;
    jl_value_t *env = NULL;
    jl_value_t *p = NULL;
    jl_value_t *temp3 = NULL;
    JL_GC_PUSH5(&super, &sub, &env, &p, &temp3);
    while (tta != jl_any_type) {
        if (tta->name == ttb->name) {
            sub = (jl_datatype_t*)a;
            super = (jl_datatype_t*)b;
            break;
        }
        tta = tta->super;
    }
    if (sub == NULL) {
        tta = (jl_datatype_t*)a;
        while (ttb != jl_any_type) {
            if (tta->name == ttb->name) {
                sub = (jl_datatype_t*)b;
                super = (jl_datatype_t*)a;
                break;
            }
            ttb = ttb->super;
        }
        if (sub == NULL) {
            JL_GC_POP();
            return (jl_value_t*)jl_bottom_type;
        }
    }

    if (sub->super == jl_type_type && jl_is_type_type((jl_value_t*)super)) {
        // subtypes of Type like DataType do not constrain the type
        // parameter, and yet contain Type instances with a more specific
        // parameter (like Type{Int}). This is a special case.
        jl_value_t *tp0 = jl_tparam0(super);
        if (jl_is_typevar(tp0) || (jl_value_t*)sub == jl_typeof(tp0)) {
            JL_GC_POP();
            return (jl_value_t*)super;
        }
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }

    /*
      issue #6387
      Say we have

      type DateRange{C} <: Range{Date{C}}; end

      and

      vcat{T}(r::Range{T}) = ...

      Then inferring vcat(::DateRange) concludes that T==Date{C}, but it should
      conclude T<:Date{C}. The core problem seems to be that in moving from a
      type to its supertype, we drop the environment that binds C --- we
      forget that C is a variable in Range{Date{C}}. For now I work around this
      by rewriting this type to Range{_<:Date{C}}, effectively tagging type
      parameters that are variable due to the extra (dropped) environment.
    */
    if (var == covariant &&
        sub == (jl_datatype_t*)sub->name->primary &&
        jl_has_typevars_from((jl_value_t*)sub->super, ((jl_datatype_t*)sub->name->primary)->parameters))
        env = approxify_type((jl_datatype_t*)sub->super, ((jl_datatype_t*)sub->name->primary)->parameters, recheck_tuple_intersection);
    else
        env = (jl_value_t*)sub->super;
    super = (jl_datatype_t*)jl_type_intersect((jl_value_t*)env, (jl_value_t*)super, penv, eqc, recheck_tuple_intersection, var);

    if ((jl_value_t*)super == jl_bottom_type) {
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }

    // super needs to be instantiated so the matching below finds actual types
    // and doesn't fail due to the presence of extra typevars.
    super = (jl_datatype_t*)jl_instantiate_type_with((jl_value_t*)super, eqc->data, eqc->n/2);

    size_t n = jl_svec_len(sub->parameters);

    assert(sub->name->primary != NULL);
    jl_value_t *tc = sub->name->primary;
    jl_svec_t *tc_params = ((jl_datatype_t*)tc)->parameters;
    // compute what constraints the supertype imposes on the subtype
    jl_svec_t *subs_sup_params =
        ((jl_datatype_t*)((jl_datatype_t*)tc)->super)->parameters;
    // match the intersected supertype against the pattern this subtype
    // uses to instantiate its supertype. this tells us what subtype parameter
    // values are implied by the intersected supertype, or that the
    // intersected supertype cannot come from this subtype (in which case
    // our final answer is Union{}).
    size_t i;
    // hack: we need type_match to find assignments for all typevars
    int prev_mim = match_intersection_mode;
    match_intersection_mode = 1;
    // TODO get rid of these intermediate tuple types
    p = (jl_value_t*)jl_apply_tuple_type(super->parameters);
    temp3 = (jl_value_t*)jl_apply_tuple_type(subs_sup_params);
    env = jl_type_match(p, temp3);
    int sub_needs_parameters = 0;
    if (env == jl_false) {
        env = jl_type_match(temp3, p);
    }
    else {
        // this means it needs to be possible to instantiate the subtype
        // such that the supertype gets the matching parameters we just
        // determined.
        sub_needs_parameters = 1;
    }
    match_intersection_mode = prev_mim;
    if (env == jl_false) {
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }
    if (sub_needs_parameters) {
        for(int e=0; e < jl_svec_len(env); e+=2) {
            jl_value_t *tp = jl_svecref(env, e);
            // make sure each needed parameter is actually set by the subtype
            size_t j;
            for(j=0; j < n; j++) {
                if (tp == jl_svecref(tc_params, j))
                    break;
            }
            if (j >= n) {
                JL_GC_POP();
                return (jl_value_t*)jl_bottom_type;
            }
        }
    }

    p = (jl_value_t*)jl_alloc_svec(n);
    for(i=0; i < n; i++) {
        jl_value_t *tp = jl_svecref(tc_params, i);
        jl_value_t *elt = jl_svecref(sub->parameters, i);
        for(int e=0; e < jl_svec_len(env); e+=2) {
            if (jl_svecref(env, e) == tp) {
                elt = jl_type_intersect(elt, jl_svecref(env, e+1),
                                        penv, eqc, recheck_tuple_intersection, invariant);
                // note: elt might be Union{} if "Union{}" was the type parameter
                break;
            }
        }
        jl_svecset(p, i, elt);
    }
    jl_value_t *result = (jl_value_t*)jl_apply_type(tc, (jl_svec_t*)p);
    JL_GC_POP();
    return result;
}

JL_DLLEXPORT jl_value_t *jl_type_intersection(jl_value_t *a, jl_value_t *b)
{
    jl_svec_t *env = jl_emptysvec;
    JL_GC_PUSH1(&env);
    jl_value_t *ti = jl_type_intersection_matching(a, b, &env, jl_emptysvec);
    JL_GC_POP();
    return ti;
}

JL_DLLEXPORT jl_svec_t *jl_type_intersection_env(jl_value_t *a, jl_value_t *b, jl_svec_t *tvars)
{
    jl_svec_t *env = jl_emptysvec;
    JL_GC_PUSH1(&env);
    jl_value_t *ti = jl_type_intersection_matching(a, b, &env, tvars);
    jl_svec_t *pair = jl_svec2(ti, env);
    JL_GC_POP();
    return pair;
}

/*
  constraint satisfaction algorithm:
  - keep lists of equality constraints and subtype constraints
    (invariant and covariant)
  - all constraints between two typevars are equality, i.e. it means the
    two corresponding typevars must end up with the same value. however
    they are kept in the subtype constraint list because they are not part
    of the final answer yet.
  - after computing the intersection, we try to solve the typevar constraints
  - for each equality constraint T=S, add T=S to the results
  - for each other constraint T=S, do
      if T=U is in the results
        if S is a typevar
          if S=R is in the results
            update R to meet(lookup(R),lookup(U))
          else
            add S=meet(S,lookup(U))
          end
          update U to S
        else
          update U to meet(lookup(U),S)
        end
      else
        if S has typevars
          add T=S
        else
          add T=_<:S
        end
      end

    meet(X,Y) =
      if X and Y both have no typevars and not equal, fail
      if X has no typevars and X<:Y, return X, else fail
      if Y has no typevars and Y<:X, return Y, else fail
      if one or both is typevar, return meet_typevar(X,Y)
      else return intersect(X,Y)

    update X to Y =
      if X is a typevar, trace to its class root and put Y there
      else put Y where X was

    lookup(X) =
      if X is a typevar and X=Y is in the results, return lookup(Y)
      else return X
*/

static jl_value_t **tvar_lookup(cenv_t *env, jl_value_t **pX)
{
    jl_value_t *v = *pX;
    if (is_btv(v)) {
        for(int i=0; i < env->n; i+=2) {
            if (env->data[i] == v) {
                if (env->data[i+1] == v)  // allow T=T
                    return pX;
                return tvar_lookup(env, &env->data[i+1]);
            }
        }
    }
    return pX;
}

static jl_value_t *meet_tvars(jl_tvar_t *a, jl_tvar_t *b)
{
    jl_value_t *lb=NULL, *ub=NULL;
    if (type_eqv_((jl_value_t*)a->lb, (jl_value_t*)b->lb) &&
        type_eqv_((jl_value_t*)a->ub, (jl_value_t*)b->ub))
        return (jl_value_t*)b;
    ub = jl_type_intersection((jl_value_t*)a->ub, (jl_value_t*)b->ub);
    if (ub == (jl_value_t*)jl_bottom_type)
        return ub;
    JL_GC_PUSH2(&lb, &ub);
    lb = (jl_value_t*)jl_svec2(a->lb, b->lb);
    lb = jl_type_union((jl_svec_t*)lb);
    if (!jl_subtype(lb, ub, 0)) {
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }
    // TODO: might not want to collapse tvar to non-tvar in all cases
    if (jl_is_leaf_type(ub)) {
        JL_GC_POP();
        return ub;
    }
    jl_value_t *res = (jl_value_t*)jl_new_typevar(underscore_sym, lb, ub);
    ((jl_tvar_t*)res)->bound = a->bound & b->bound;
    JL_GC_POP();
    return res;
}

static jl_value_t *meet_tvar(jl_tvar_t *tv, jl_value_t *ty)
{
    if (jl_is_typevar(ty))
        return (jl_value_t*)meet_tvars(tv, (jl_tvar_t*)ty);
    //if (jl_types_equal((jl_value_t*)tv->ub, ty))
    //    return ty;
    if (jl_subtype((jl_value_t*)tv->ub, ty, 0))
        return (jl_value_t*)tv;
    // TODO: should we check type_intersection(tv->ub, ty) instead?
    if (!jl_subtype(ty, (jl_value_t*)tv->ub, 0))
        return (jl_value_t*)jl_bottom_type;
    //if (jl_types_equal((jl_value_t*)tv->lb, ty))
    //    return ty;
    if (jl_subtype((jl_value_t*)tv->lb, ty, 0)) {
        if (jl_is_leaf_type(ty) || !jl_is_type(ty))
            return ty;
        jl_tvar_t *ntv = jl_new_typevar(underscore_sym, tv->lb, ty);
        // TODO: this would be nice but causes some spurious ambiguity warnings
        // due to typevars in covariant position, which we should simplify out somewhere.
        //ntv->bound = tv->bound;
        return (jl_value_t*)ntv;
    }
    return (jl_value_t*)jl_bottom_type;
}

static jl_value_t *meet(jl_value_t *X, jl_value_t *Y, variance_t var)
{
    if (jl_is_typevar(X)) {
        jl_value_t *tv;
        if (jl_is_typevar(Y)) {
            tv = meet_tvars((jl_tvar_t*)X, (jl_tvar_t*)Y);
        }
        else {
            tv = meet_tvar((jl_tvar_t*)X, Y);
        }
        if (tv == (jl_value_t*)jl_bottom_type)
            return NULL;
        return tv;
    }
    if (jl_is_typevar(Y)) {
        jl_value_t *tv = meet_tvar((jl_tvar_t*)Y, X);
        if (tv == (jl_value_t*)jl_bottom_type)
            return NULL;
        return tv;
    }
    if (jl_subtype(X,Y,0)) return X;
    if (jl_subtype(Y,X,0)) return Y;
    jl_value_t *v = jl_type_intersection(X, Y);
    return (v == (jl_value_t*)jl_bottom_type ?  NULL : v);
}

// convert a type to the value it would have if assigned to a static parameter
// in covariant context.
// example: {Type{Int},} => {DataType,}
// calling f{T}(x::T) as f({Int,}) should give T == {DataType,}, but we
// might temporarily represent this type as {Type{Int},} for more precision.
static jl_value_t *type_to_static_parameter_value(jl_value_t *t, jl_value_t *tv, jl_value_t **tvs, int ntv)
{
    int i;
    for(i=0; i < ntv; i++) {
        if (tv == tvs[i])
            break;
    }
    if (i >= ntv)
        return t;  // don't widen vars not in env
    if (jl_is_type_type(t) && !jl_is_typevar(jl_tparam0(t)))
        return jl_typeof(jl_tparam0(t));
    if (jl_is_tuple_type(t)) {
        jl_svec_t *p = ((jl_datatype_t*)t)->parameters;
        size_t l = jl_svec_len(p);
        int changed = 0;
        jl_svec_t *np = jl_alloc_svec(l);
        JL_GC_PUSH1(&np);
        for(size_t i=0; i < l; i++) {
            jl_value_t *el = type_to_static_parameter_value(jl_svecref(p,i), NULL, NULL, 0);
            jl_svecset(np, i, el);
            if (el != jl_svecref(p,i))
                changed = 1;
        }
        jl_value_t *result = changed ? (jl_value_t*)jl_apply_tuple_type(np) : t;
        JL_GC_POP();
        return result;
    }
    return t;
}

/*
void print_env(cenv_t *soln)
{
    for(int i=0; i < soln->n; i+=2) {
        jl_value_t *T, *S;
        T = soln->data[i]; S = soln->data[i+1];
        jl_printf(JL_STDOUT, "%s@%x=", jl_symbol_name(((jl_tvar_t*)T)->name), T);
        jl_static_show(JL_STDOUT, S);
        jl_printf(JL_STDOUT, " ");
    }
    jl_printf(JL_STDOUT, "\n");
}
*/

static int solve_tvar_constraints(cenv_t *env, cenv_t *soln, jl_value_t **tvs, int ntv)
{
    jl_value_t *rt1=NULL, *rt2=NULL, *S=NULL;
    JL_GC_PUSH3(&rt1, &rt2, &S);

    while (1) {
        int old_n = soln->n;

        // 1. replace each T=S with T=find(S)
        for(int i=0; i < soln->n; i+=2) {
            jl_value_t **pS = &soln->data[i+1];
            if (jl_is_typevar(*pS))
                *pS = *tvar_lookup(soln, pS);
            if (!jl_is_typevar(*pS)) {
                // detect cycles
                if (jl_has_typevars_from_v(*pS, &soln->data[i], 1))
                    goto ret_no;
            }
        }

        // 2. instantiate all RHSes using soln
        if (soln->n > 0) {
            for(int i=0; i < env->n; i+=2) {
                jl_value_t **pS = &env->data[i+1];
                JL_TRY {
                    *pS = jl_instantiate_type_with(*pS, &soln->data[0], soln->n/2);
                }
                JL_CATCH {
                }
            }
        }

        // 3. given T, let S´ = intersect(all S s.t. (T=S) or (S=T) ∈ env). add (T=S´) to soln.
        for(int i=0; i < env->n; i+=2) {
            jl_value_t *T = env->data[i];
            jl_value_t **pS = &env->data[i+1];
            S = *pS;
            if (!jl_is_typevar(S)) {
                for(int j=i+2; j < env->n; j+=2) {
                    jl_value_t *TT = env->data[j];
                    jl_value_t *SS = env->data[j+1];
                    if (TT == T) {
                        // found T=SS in env
                        if (!jl_is_typevar(SS)) {
                            rt1 = type_to_static_parameter_value(S, T, tvs, ntv);
                            rt2 = type_to_static_parameter_value(SS, TT, tvs, ntv);
                            jl_value_t *m = meet(rt1, rt2, covariant);
                            if (m == NULL) goto ret_no;
                            S = m;
                        }
                    }
                    else if (SS == T) {
                        // found TT=T in env; meet with TT
                        jl_value_t **pTT = tvar_lookup(soln, &TT);
                        if (pTT != &TT) {
                            jl_value_t *m = meet(S, *pTT, covariant);
                            if (m == NULL) goto ret_no;
                            S = m;
                        }
                    }
                }

                if (!(jl_is_leaf_type(S) || S == (jl_value_t*)jl_bottom_type)) {
                    goto next_in_env;
                }

                jl_value_t **pT = tvar_lookup(soln, &T);
                if (pT != &T) {
                    size_t lenS, lenT;
                    if (jl_get_size(S, &lenS) && jl_get_size(*pT, &lenT)) {
                        int bot = 0;
                        long mv = meet_tuple_lengths(~lenS, lenT, &bot);
                        if (bot)
                            goto ret_no;
                        // NOTE: this is unused. can we do anything with it?
                        (void)mv;
                        //S = jl_box_long(mv);
                    }
                    else {
                        if (meet(*pT,S,covariant) == NULL)
                            goto ret_no;
                    }
                }
                else {
                    extend(T, type_to_static_parameter_value(S, T, tvs, ntv), soln);
                }
            }
            else {
                jl_value_t **pT = tvar_lookup(soln, &T);
                if (pT != &T) {
                    if (tvar_lookup(soln, &S) == &S) {
                        jl_value_t *v = meet(S, *pT, covariant);
                        if (v == NULL) goto ret_no;
                        extend(S, v, soln);
                        *pT = S;
                    }
                }
            }
        next_in_env:
            ;
        }
        if (soln->n == old_n)
            break;
    }

    for(int i=0; i < env->n; i+=2) {
        jl_value_t *T = env->data[i];
        jl_value_t **pS = &env->data[i+1];
        S = *pS;
        if (tvar_lookup(soln, &T) == &T) {
            for(int j=i+2; j < env->n; j+=2) {
                jl_value_t *TT = env->data[j];
                jl_value_t *SS = env->data[j+1];
                if (TT == T) {
                    rt1 = type_to_static_parameter_value(S, T, tvs, ntv);
                    rt2 = type_to_static_parameter_value(SS, TT, tvs, ntv);
                    jl_value_t *m = meet(rt1, rt2, covariant);
                    if (m == NULL) goto ret_no;
                    S = m;
                }
                else if (SS == T) {
                    jl_value_t *m = meet(S, *tvar_lookup(soln, &TT), covariant);
                    if (m == NULL) goto ret_no;
                    S = m;
                }
            }
            if (jl_is_type(S) || jl_is_typevar(S)) {
                if (!jl_is_typevar(S) && !jl_is_leaf_type(S) && S != jl_bottom_type) {
                    S = (jl_value_t*)jl_new_typevar(underscore_sym,
                                                    (jl_value_t*)jl_bottom_type, S);
                }
                extend(T, type_to_static_parameter_value(S, T, tvs, ntv), soln);
            }
        }
    }

    JL_GC_POP();
    return 1;
 ret_no:
    JL_GC_POP();
    return 0;
}

jl_value_t *jl_type_intersection_matching(jl_value_t *a, jl_value_t *b,
                                          jl_svec_t **penv, jl_svec_t *tvars)
{
    jl_value_t **rts;
    JL_GC_PUSHARGS(rts, 2 + 2*MAX_CENV_SIZE);
    cenv_t eqc; eqc.n = 0; eqc.data = &rts[2];
    cenv_t env; env.n = 0; env.data = &rts[2+MAX_CENV_SIZE];
    eqc.tvars = tvars; env.tvars = tvars;
    jl_value_t **pti = &rts[0];
    jl_value_t **extraroot = &rts[1];

    int recheck_tuple_intersection = 0;
    JL_TRY {
        // This is kind of awful, but an inner call to instantiate_type
        // might fail due to a mismatched type parameter. The problem is
        // that we allow Range{T} to exist, even though the declaration of
        // Range specifies Range{T<:Real}. Therefore intersection cannot see
        // that some parameter values actually don't match.
        *pti = jl_type_intersect(a, b, &env, &eqc, &recheck_tuple_intersection, covariant);
    }
    JL_CATCH {
        *pti = (jl_value_t*)jl_bottom_type;
    }
    if (*pti == (jl_value_t*)jl_bottom_type ||
        !(env.n > 0 || eqc.n > 0 || tvars != jl_emptysvec)) {
        JL_GC_POP();
        return *pti;
    }

    int e;

    if (recheck_tuple_intersection) {
        for (e = 0; e < eqc.n; e += 2) {
            jl_value_t *val = eqc.data[e + 1];
            if (jl_is_long(val))
                break;
        }
        if (e < eqc.n) {
            /*
              if there are integer-valued parameters, repeat intersection
              with the full environment visible. this is needed because
              NTuple has only one element type, so we can't keep track of
              the fact that an arbitrary tuple's length must match some
              typevar, e.g. "(Int8,Int32...) of length N". the solution is
              to find all other constraints on N first, then do intersection
              again with that knowledge.
            */
            *pti = jl_type_intersect(a, b, &env, &eqc, &recheck_tuple_intersection, covariant);
            if (*pti == (jl_value_t*)jl_bottom_type) {
                JL_GC_POP();
                return *pti;
            }
        }
    }

    jl_value_t **tvs;
    int tvarslen;
    if (jl_is_typevar(tvars)) {
        tvs = (jl_value_t**)&tvars;
        tvarslen = 1;
    }
    else {
        assert(jl_is_svec(tvars));
        tvs = jl_svec_data(tvars);
        tvarslen = jl_svec_len(tvars);
    }

    if (!solve_tvar_constraints(&env, &eqc, tvs, tvarslen)) {
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }
    //jl_printf(JL_STDOUT, "env: "); print_env(&env);
    //jl_printf(JL_STDOUT, "sol: "); print_env(&eqc);

    int env0 = eqc.n;
    for(int tk=0; tk < tvarslen; tk++) {
        jl_tvar_t *tv = (jl_tvar_t*)tvs[tk];
        for(e=0; e < env0; e+=2) {
            if (eqc.data[e] == (jl_value_t*)tv) {
                break;
            }
        }
        // bind type vars to new similar tvars if they were not matched explicitly
        // during type intersection.
        if (e >= env0) {
            /*
              Note: we used to bind T=T, but this can cause a loop if a recursion
              is set up such that in a future call T=Foo{T}. If the RHS is
              instantiated, we unintentionally construct Foo{Foo{T}} since the
              typevar happens to match. This caused issue #6404.
            */
            jl_tvar_t *ntv = jl_new_typevar(tv->name, tv->lb, tv->ub);
            ntv->bound = tv->bound;
            extend_((jl_value_t*)tv, (jl_value_t*)ntv, &eqc, 1);
        }
    }

    for(int i=0; i < eqc.n; i+=2) {
        eqc.data[i+1] = *tvar_lookup(&eqc, &eqc.data[i+1]);
    }
    if (env0 > 0) {
        /*
          in a situation like this:
          Type{_<:Array{T,1}} ∩ Type{Array{S,N}}
          We end up with environment
          _ = Array{S,N}
          N = 1
          So we need to instantiate all the RHS's first.
        */
        for(int i=0; i < eqc.n; i+=2) {
            jl_value_t *rhs = eqc.data[i+1];
            int tvar = jl_is_typevar(rhs);
            jl_value_t *rhs2 = rhs;
            if (tvar && jl_has_typevars(((jl_tvar_t*)rhs)->ub)) {
                rhs2 = ((jl_tvar_t*)rhs)->ub;
            }
            else tvar = 0;
            JL_TRY {
                jl_value_t *inst = jl_instantiate_type_with(rhs2, eqc.data, eqc.n/2);
                eqc.data[i+1] = inst;
                if (tvar) {
                    *extraroot = rhs;
                    eqc.data[i+1] = (jl_value_t*)jl_new_typevar(underscore_sym, ((jl_tvar_t*)rhs)->lb, inst);
                }
            }
            JL_CATCH {
            }
        }

        // detect cycles; e.g. (T,Ptr{T}) ∩ (Ptr{S},S) == ⊥
        for(int i=0; i < eqc.n; i+=2) {
            jl_value_t *var = eqc.data[i];
            jl_value_t *val = eqc.data[i+1];
            if (val != var && jl_has_typevars_from_v(val, &var, 1)) {
                // var's RHS contains the var itself => unsatisfiable (e.g. T = Foo{T})
                JL_GC_POP();
                return (jl_value_t*)jl_bottom_type;
            }
        }

        JL_TRY {
            *pti = (jl_value_t*)jl_instantiate_type_with(*pti, eqc.data, eqc.n/2);
        }
        JL_CATCH {
            *pti = (jl_value_t*)jl_bottom_type;
        }
    }

    // return environment in same order as tvars
    *penv = jl_alloc_svec_uninit(tvarslen);
    for(int tk=0; tk < tvarslen; tk++) {
        jl_tvar_t *tv = (jl_tvar_t*)tvs[tk];
        for(e=0; e < eqc.n; e+=2) {
            if (eqc.data[e] == (jl_value_t*)tv) {
                jl_svecset(*penv, tk, eqc.data[e+1]);
            }
        }
    }

    JL_GC_POP();
    if (jl_is_typevar(*pti) && !(jl_is_typevar(a) && jl_is_typevar(b)))
        return ((jl_tvar_t*)*pti)->ub;
    return *pti;
}

// --- type instantiation and cache ---

static int extensionally_same_type(jl_value_t *a, jl_value_t *b)
{
    return jl_subtype(a, b, 0) && jl_subtype(b, a, 0);
}

static int type_eqv__(jl_value_t *a, jl_value_t *b, int distinguish_tctor)
{
    if (a == b) return 1;
    if (distinguish_tctor && jl_is_typector(a) != jl_is_typector(b)) return 0;
    if (jl_is_typector(a)) a = (jl_value_t*)((jl_typector_t*)a)->body;
    if (jl_is_typector(b)) b = (jl_value_t*)((jl_typector_t*)b)->body;
    if (jl_is_typevar(a)) {
        if (jl_is_typevar(b)) {
            return !distinguish_tctor && type_eqv_(((jl_tvar_t*)a)->ub, ((jl_tvar_t*)b)->ub) &&
                type_eqv_(((jl_tvar_t*)a)->lb, ((jl_tvar_t*)b)->lb);
        }
        else {
            return 0;
        }
    }
    if (jl_is_uniontype(a)) {
        if (jl_is_uniontype(b)) {
            return extensionally_same_type(a, b);
        }
        return 0;
    }
    if (!jl_is_datatype(a) || !jl_is_datatype(b)) {
        return jl_egal(a, b);
    }
    jl_datatype_t *tta = (jl_datatype_t*)a;
    jl_datatype_t *ttb = (jl_datatype_t*)b;
    if (tta->name != ttb->name) return 0;
    jl_svec_t *ap = tta->parameters;
    jl_svec_t *bp = ttb->parameters;
    if (jl_svec_len(ap) != jl_svec_len(bp)) {
        assert(tta->name == jl_tuple_typename);
        return 0;
    }
    size_t i;
    for(i=0; i < jl_svec_len(ap); i++) {
        jl_value_t *api = jl_svecref(ap,i);
        jl_value_t *bpi = jl_svecref(bp,i);
        if (api == bpi) continue;
        if (!type_eqv__(api, bpi, distinguish_tctor))
            return 0;
    }
    return 1;
}

static int type_eqv_(jl_value_t *a, jl_value_t *b)
{
    return type_eqv__(a, b, 0);
}

JL_DLLEXPORT int jl_types_equal(jl_value_t *a, jl_value_t *b)
{
    return type_eqv_(a, b);
}

static int type_le_generic(jl_value_t *a, jl_value_t *b, int useenv)
{
    jl_value_t *env = jl_type_match(a, b);
    if (env == jl_false) return 0;
    size_t l = jl_svec_len(env);
    // make sure all typevars correspond to other unique typevars
    for(size_t i=0; i < l; i+=2) {
        jl_value_t *envi = jl_svecref(env,i+1);
        if (!jl_is_typevar(envi))
            return 0;
        if (useenv && ((jl_tvar_t*)envi)->bound!=((jl_tvar_t*)jl_svecref(env,i))->bound)
            return 0;
        for(size_t j=0; j < l; j+=2) {
            if (i != j) {
                if (envi == jl_svecref(env,j+1))
                    return 0;
            }
        }
    }
    return 1;
}

int jl_types_equal_generic(jl_value_t *a, jl_value_t *b, int useenv)
{
    return type_le_generic(a, b, useenv) && type_le_generic(b, a, useenv);
}

static int valid_type_param(jl_value_t *v)
{
    if (jl_is_tuple(v)) {
        // NOTE: tuples of symbols are not currently bits types, but have been
        // allowed as type parameters. this is a bit ugly.
        jl_value_t *tt = jl_typeof(v);
        size_t i;
        size_t l = jl_nparams(tt);
        for(i=0; i < l; i++) {
            jl_value_t *pi = jl_tparam(tt,i);
            if (!(pi == (jl_value_t*)jl_sym_type || jl_isbits(pi)))
                return 0;
        }
        return 1;
    }
    // TODO: maybe more things
    return jl_is_type(v) || jl_is_typevar(v) || jl_is_symbol(v) || jl_isbits(jl_typeof(v));
}

jl_value_t *jl_apply_type_(jl_value_t *tc, jl_value_t **params, size_t n)
{
    if (tc == (jl_value_t*)jl_anytuple_type)
        return (jl_value_t*)jl_apply_tuple_type_v(params, n);
    if (tc == (jl_value_t*)jl_uniontype_type)
        return (jl_value_t*)jl_type_union_v(params, n);
    if (n == 0) {
        if (jl_is_typector(tc))
            return (jl_value_t*)((jl_typector_t*)tc)->body;
        return tc;
    }
    size_t i;
    const char *tname;
    jl_svec_t *tp;
    jl_datatype_t *stprimary = NULL;
    if (jl_is_typector(tc)) {
        tp = ((jl_typector_t*)tc)->parameters;
        tname = "typealias";
    }
    else {
        assert(jl_is_datatype(tc));
        tp = ((jl_datatype_t*)tc)->parameters;
        tname = jl_symbol_name(((jl_datatype_t*)tc)->name->name);
        stprimary = (jl_datatype_t*)((jl_datatype_t*)tc)->name->primary;
    }
    for(i=0; i < n; i++) {
        jl_value_t *pi = params[i];
        if (!valid_type_param(pi)) {
            jl_type_error_rt(tname, "parameter",
                             jl_subtype(pi, (jl_value_t*)jl_number_type, 1) ?
                             (jl_value_t*)jl_long_type : (jl_value_t*)jl_type_type,
                             pi);
        }
    }
    size_t ntp = jl_svec_len(tp);
    if (n > ntp)
        jl_errorf("too many parameters for type %s", tname);
    jl_value_t **env;
    JL_GC_PUSHARGS(env, 2*ntp);
    size_t ne = 0;
    for(i=0; i < ntp; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_svecref(tp,i);
        if (!jl_is_typevar(tv))
            continue;
        env[ne*2+0] = (jl_value_t*)tv;
        if (ne >= n) {
            if (stprimary && stprimary->types == NULL) {
                // during creation of type Foo{A,B}, fill in missing
                // trailing parameters with copies for recursive
                // instantiations, so that in the future Foo{A} as a field
                // type will only be instantiated with the first parameter.
                env[ne*2+1] = (jl_value_t*)jl_new_typevar(tv->name, tv->lb, tv->ub);
            }
            else {
                env[ne*2+1] = (jl_value_t*)tv;
            }
        }
        else {
            // NOTE: type checking deferred to inst_type_w_ to make sure
            // supertype parameters are checked recursively.
            jl_value_t *pi = params[ne];
            if (tc!=(jl_value_t*)jl_type_type && jl_is_typector(pi))
                env[ne*2+1] = (jl_value_t*)((jl_typector_t*)pi)->body;
            else
                env[ne*2+1] = pi;
        }
        ne++;
    }
    if (ne < n)
        jl_errorf("too many parameters for type %s", tname);
    if (jl_is_typector(tc)) tc = (jl_value_t*)((jl_typector_t*)tc)->body;
    jl_value_t *result = jl_instantiate_type_with((jl_value_t*)tc, env, ne);
    JL_GC_POP();
    return (jl_value_t*)result;
}

JL_DLLEXPORT jl_value_t *jl_apply_type(jl_value_t *tc, jl_svec_t *params)
{
    // NOTE: callers are supposed to root these arguments, but there are
    // several uses that don't, so root here just to be safe.
    JL_GC_PUSH1(&params);
    jl_value_t *t = jl_apply_type_(tc, jl_svec_data(params), jl_svec_len(params));
    JL_GC_POP();
    return t;
}

JL_DLLEXPORT jl_value_t *jl_tupletype_fill(size_t n, jl_value_t *v)
{
    // TODO: replace with just using NTuple
    jl_value_t *p = NULL;
    JL_GC_PUSH1(&p);
    p = (jl_value_t*)jl_svec_fill(n, v);
    p = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)p);
    JL_GC_POP();
    return p;
}

static int contains_unions(jl_value_t *type)
{
    if (jl_is_uniontype(type)) return type != jl_bottom_type;
    if (jl_is_typector(type)) return contains_unions(((jl_typector_t*)type)->body);
    if (!jl_is_datatype(type)) return 0;
    int i;
    for(i=0; i < jl_nparams(type); i++) {
        if (contains_unions(jl_tparam(type,i)))
            return 1;
    }
    return 0;
}

// this function determines whether a type is simple enough to form
// a total order based on UIDs and object_id.
static int is_typekey_ordered(jl_value_t **key, size_t n)
{
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *k = key[i];
        if (jl_is_typevar(k))
            return 0;
        if (jl_is_type(k) &&
            !(jl_is_datatype(k) && (((jl_datatype_t*)k)->uid ||
                                    k == ((jl_datatype_t*)k)->name->primary ||
                                    (!jl_has_typevars_(k,1) && !contains_unions(k)))) &&
            k != jl_bottom_type)
            return 0;
    }
    return 1;
}

// ordered comparison of types
static int typekey_compare(jl_datatype_t *tt, jl_value_t **key, size_t n)
{
    size_t j;
    if (tt == NULL) return -1;  // place NULLs at end to allow padding for fast growing
    size_t tnp = jl_nparams(tt);
    if (n < tnp) return -1;
    if (n > tnp) return 1;
    for(j=0; j < n; j++) {
        jl_value_t *kj = key[j], *tj = jl_svecref(tt->parameters,j);
        if (tj != kj) {
            int dtk = jl_is_datatype(kj);
            if (!jl_is_datatype(tj)) {
                if (!dtk) {
                    if (jl_egal(tj, kj))
                        continue;
                    return (jl_object_id(kj) < jl_object_id(tj) ? -1 : 1);
                }
                else {
                    return 1;
                }
            }
            else if (!dtk) {
                return -1;
            }
            jl_datatype_t *dt = (jl_datatype_t*)tj;
            jl_datatype_t *dk = (jl_datatype_t*)kj;
            if (dk->uid != dt->uid) {
                return dk->uid < dt->uid ? -1 : 1;
            }
            else if (dk->uid != 0) {
                continue;
            }
            else if (dk->name->hash != dt->name->hash) {
                return dk->name->hash < dt->name->hash ? -1 : 1;
            }
            else {
                int cmp = typekey_compare(dt, jl_svec_data(dk->parameters), jl_nparams(dk));
                if (cmp != 0)
                    return cmp;
            }
        }
    }
    return 0;
}

static int dt_compare(const void *ap, const void *bp)
{
    jl_datatype_t *a = *(jl_datatype_t**)ap;
    jl_datatype_t *b = *(jl_datatype_t**)bp;
    if (a == b) return 0;
    if (b == NULL) return -1;
    if (a == NULL) return 1;
    return typekey_compare(b, jl_svec_data(a->parameters), jl_svec_len(a->parameters));
}

void jl_resort_type_cache(jl_svec_t *c)
{
    qsort(jl_svec_data(c), jl_svec_len(c), sizeof(jl_value_t*), dt_compare);
}

static int typekey_eq(jl_datatype_t *tt, jl_value_t **key, size_t n)
{
    size_t j;
    size_t tnp = jl_nparams(tt);
    if (n != tnp) return 0;
    for(j=0; j < n; j++) {
        jl_value_t *kj = key[j], *tj = jl_svecref(tt->parameters,j);
        if (tj != kj && !type_eqv__(tj, kj, 1))
            return 0;
    }
    return 1;
}

// look up a type in a cache by binary or linear search.
// if found, returns the index of the found item. if not found, returns
// ~n, where n is the index where the type should be inserted.
static ssize_t lookup_type_idx(jl_typename_t *tn, jl_value_t **key, size_t n, int ordered)
{
    if (n==0) return -1;
    if (ordered) {
        jl_svec_t *cache = tn->cache;
        jl_value_t **data = jl_svec_data(cache);
        size_t cl = jl_svec_len(cache);
        ssize_t lo = -1;
        ssize_t hi = cl;
        while (lo < hi-1) {
            ssize_t m = ((size_t)(lo+hi))>>1;
            jl_datatype_t *tt = (jl_datatype_t*)data[m];
            int cmp = typekey_compare(tt, key, n);
            if (cmp == 0) return m;
            if (cmp < 0)
                hi = m;
            else
                lo = m;
        }
        return ~hi;
    }
    else {
        jl_svec_t *cache = tn->linearcache;
        jl_value_t **data = jl_svec_data(cache);
        size_t cl = jl_svec_len(cache);
        ssize_t i;
        for(i=0; i < cl; i++) {
            jl_datatype_t *tt = (jl_datatype_t*)data[i];
            if (tt == NULL) return ~i;
            if (typekey_eq(tt, key, n))
                return i;
        }
        return ~cl;
    }
}

static jl_value_t *lookup_type(jl_typename_t *tn, jl_value_t **key, size_t n)
{
    JL_TIMING(TYPE_CACHE_LOOKUP);
    int ord = is_typekey_ordered(key, n);
    ssize_t idx = lookup_type_idx(tn, key, n, ord);
    jl_value_t *t = (idx < 0) ? NULL : jl_svecref(ord ? tn->cache : tn->linearcache, idx);
    return t;
}

static volatile int t_uid_ctr = 1;

int  jl_get_t_uid_ctr(void) { return t_uid_ctr; }
void jl_set_t_uid_ctr(int i) { t_uid_ctr=i; }

int jl_assign_type_uid(void)
{
    assert(t_uid_ctr != 0);
    return jl_atomic_fetch_add(&t_uid_ctr, 1);
}

static int is_cacheable(jl_datatype_t *type)
{
    // only cache types whose behavior will not depend on the identities
    // of contained TypeVars
    assert(jl_is_datatype(type));
    jl_svec_t *t = type->parameters;
    if (jl_svec_len(t) == 0) return 0;
    // cache abstract types with no type vars
    if (jl_is_abstracttype(type))
        return !jl_has_typevars_((jl_value_t*)type, 1);
    // ... or concrete types
    return jl_is_leaf_type((jl_value_t*)type);
}

static void cache_insert_type(jl_value_t *type, ssize_t insert_at, int ordered)
{
    assert(jl_is_datatype(type));
    // assign uid if it hasn't been done already
    if (!jl_is_abstracttype(type) && ((jl_datatype_t*)type)->uid==0)
        ((jl_datatype_t*)type)->uid = jl_assign_type_uid();
    jl_svec_t *cache;
    if (ordered)
        cache = ((jl_datatype_t*)type)->name->cache;
    else
        cache = ((jl_datatype_t*)type)->name->linearcache;
    assert(jl_is_svec(cache));
    size_t n = jl_svec_len(cache);
    if (n==0 || jl_svecref(cache,n-1) != NULL) {
        jl_svec_t *nc = jl_alloc_svec(n < 8 ? 8 : (n*3)>>1);
        memcpy(jl_svec_data(nc), jl_svec_data(cache), sizeof(void*) * n);
        if (ordered)
            ((jl_datatype_t*)type)->name->cache = nc;
        else
            ((jl_datatype_t*)type)->name->linearcache = nc;
        jl_gc_wb(((jl_datatype_t*)type)->name, nc);
        cache = nc;
        n = jl_svec_len(nc);
    }
    jl_value_t **p = jl_svec_data(cache);
    size_t i = insert_at;
    jl_value_t *temp = p[i], *temp2;
    jl_svecset(cache, insert_at, (jl_value_t*)type);
    assert(i < n-1 || temp == NULL);
    while (temp != NULL && i < n-1) {
        i++;
        temp2 = p[i];
        p[i] = temp;
        temp = temp2;
    }
}

jl_value_t *jl_cache_type_(jl_datatype_t *type)
{
    if (is_cacheable(type)) {
        JL_TIMING(TYPE_CACHE_INSERT);
        int ord = is_typekey_ordered(jl_svec_data(type->parameters), jl_svec_len(type->parameters));
        ssize_t idx = lookup_type_idx(type->name, jl_svec_data(type->parameters),
                                      jl_svec_len(type->parameters), ord);
        if (idx >= 0)
            type = (jl_datatype_t*)jl_svecref(ord ? type->name->cache : type->name->linearcache, idx);
        else
            cache_insert_type((jl_value_t*)type, ~idx, ord);
    }
    return (jl_value_t*)type;
}

typedef struct _jl_typestack_t {
    jl_datatype_t *tt;
    struct _jl_typestack_t *prev;
} jl_typestack_t;

static jl_value_t *inst_type_w_(jl_value_t *t, jl_value_t **env, size_t n,
                                jl_typestack_t *stack, int check);
static jl_svec_t *inst_all(jl_svec_t *p, jl_value_t **env, size_t n,
                           jl_typestack_t *stack, int check);

static jl_value_t *lookup_type_stack(jl_typestack_t *stack, jl_datatype_t *tt, size_t ntp,
                                     jl_value_t **iparams)
{
    // if an identical instantiation is already in process somewhere up the
    // stack, return it. this computes a fixed point for recursive types.
    jl_typename_t *tn = tt->name;
    while (stack != NULL) {
        if (stack->tt->name == tn &&
            ntp == jl_svec_len(stack->tt->parameters) &&
            typekey_eq(stack->tt, iparams, ntp)) {
            jl_value_t *lkup = (jl_value_t*)stack->tt;
            return lkup == tn->primary ? NULL : lkup;
        }
        stack = stack->prev;
    }
    return NULL;
}

static size_t jl_type_depth(jl_value_t *dt)
{
    if (jl_is_uniontype(dt)) {
        jl_svec_t *t = ((jl_uniontype_t*)dt)->types;
        size_t i, l = jl_svec_len(t);
        size_t depth = 0;
        for (i = 0; i < l; i++) {
            jl_value_t *p = jl_svecref(t, i);
            size_t d = jl_type_depth(p);
            if (d > depth)
                depth = d;
        }
        return depth;
    }
    else if (jl_is_datatype(dt)) {
        return ((jl_datatype_t*)dt)->depth;
    }
    return 0;
}

void jl_precompute_memoized_dt(jl_datatype_t *dt)
{
    int istuple = dt->name == jl_tuple_typename;
    size_t i, l = jl_nparams(dt);
    dt->isleaftype = !dt->abstract || (jl_type_type != NULL && dt->name == jl_type_type->name);
    for (i = 0; i < l; i++) {
        jl_value_t *p = jl_tparam(dt, i);
        size_t d = jl_type_depth(p) + 1;
        if (d > dt->depth)
            dt->depth = d;
        if (!dt->hastypevars)
            dt->hastypevars = jl_has_typevars__(p, 0, NULL, 0);
        if (!dt->haswildcard)
            dt->haswildcard = jl_has_typevars__(p, 1, NULL, 0);
        if (dt->isleaftype)
            dt->isleaftype = (istuple ? jl_is_leaf_type(p) : !jl_has_typevars(p));
    }
}

static arraylist_t partial_inst;

static jl_value_t *inst_datatype(jl_datatype_t *dt, jl_svec_t *p, jl_value_t **iparams, size_t ntp,
                                 int cacheable, jl_typestack_t *stack,
                                 jl_value_t **env, size_t n)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_typestack_t top;
    jl_typename_t *tn = dt->name;
    jl_value_t *tc = tn->primary;
    int istuple = (tn == jl_tuple_typename);
    // check type cache
    if (cacheable) {
        JL_LOCK(&typecache_lock); // Might GC
        jl_value_t *lkup = (jl_value_t*)lookup_type(tn, iparams, ntp);
        if (lkup != NULL) {
            JL_UNLOCK(&typecache_lock); // Might GC
            return lkup;
        }
    }
    jl_value_t *stack_lkup = lookup_type_stack(stack, dt, ntp, iparams);
    if (stack_lkup) {
        if (cacheable) JL_UNLOCK(&typecache_lock); // Might GC
        return stack_lkup;
    }

    if (istuple && ntp > 0 && jl_is_vararg_type(iparams[ntp - 1])) {
        // normalize Tuple{..., Vararg{Int, 3}} to Tuple{..., Int, Int, Int}
        jl_value_t *va = iparams[ntp - 1];
        // return same `Tuple` object for types equal to it
        if (ntp == 1 && jl_tparam0(va) == (jl_value_t*)jl_any_type &&
            jl_tparam1(va) == jl_tparam1(jl_tparam0(jl_anytuple_type))) {
            if (cacheable) JL_UNLOCK(&typecache_lock); // Might GC
            return (jl_value_t*)jl_anytuple_type;
        }
        if (jl_is_long(jl_tparam1(va))) {
            ssize_t nt = jl_unbox_long(jl_tparam1(va));
            if (nt < 0)
                jl_errorf("apply_type: Vararg length N is negative: %zd", nt);
            va = jl_tparam0(va);
            if (nt == 0 || !jl_has_typevars(va)) {
                if (cacheable) JL_UNLOCK(&typecache_lock); // Might GC
                if (ntp == 1)
                    return jl_tupletype_fill(nt, va);
                size_t i, l;
                p = jl_alloc_svec(ntp - 1 + nt);
                for (i = 0, l = ntp - 1; i < l; i++) {
                    jl_svecset(p, i, iparams[i]);
                }
                l = ntp - 1 + nt;
                for (; i < l; i++) {
                    jl_svecset(p, i, va);
                }
                JL_GC_PUSH1(&p);
                jl_value_t *ndt = (jl_value_t*)jl_apply_tuple_type(p);
                JL_GC_POP();
                return ndt;
            }
        }
    }

    // always use original type constructor
    if (!istuple) {
        if (jl_is_vararg_type((jl_value_t*)dt) && ntp == 2) {
            if (!jl_is_long(iparams[1]) && !jl_is_typevar(iparams[1])) {
                jl_type_error_rt("apply_type", "Vararg count", (jl_value_t*)jl_long_type, iparams[1]);
            }
        }
        if (tc != (jl_value_t*)dt) {
            if (cacheable) JL_UNLOCK(&typecache_lock); // Might GC
            return (jl_value_t*)jl_apply_type_(tc, iparams, ntp);
        }
    }
    else if (ntp == 0 && jl_emptytuple != NULL) {
        if (cacheable) JL_UNLOCK(&typecache_lock); // Might GC
        return jl_typeof(jl_emptytuple);
    }

    jl_datatype_t *ndt = NULL;
    jl_svec_t *ftypes;

    // move array of instantiated parameters to heap; we need to keep it
    JL_GC_PUSH2(&p, &ndt);
    if (p == NULL) {
        p = jl_alloc_svec_uninit(ntp);
        for(unsigned i=0; i < ntp; i++)
            jl_svecset(p, i, iparams[i]);
    }

    // create and initialize new type
    ndt = jl_new_uninitialized_datatype();
    // associate these parameters with the new type on
    // the stack, in case one of its field types references it.
    top.tt = (jl_datatype_t*)ndt;
    top.prev = stack;
    stack = &top;
    ndt->name = tn;
    jl_gc_wb(ndt, ndt->name);
    ndt->super = NULL;
    ndt->parameters = p;
    jl_gc_wb(ndt, ndt->parameters);
    ndt->types = istuple ? p : NULL; // to be filled in below
    ndt->mutabl = dt->mutabl;
    ndt->abstract = dt->abstract;
    ndt->instance = NULL;
    ndt->uid = 0;
    ndt->struct_decl = NULL;
    ndt->ditype = NULL;
    ndt->size = 0;
    jl_precompute_memoized_dt(ndt);

    // assign uid as early as possible
    if (cacheable && !ndt->abstract)
        ndt->uid = jl_assign_type_uid();

    if (istuple) {
        ndt->super = jl_any_type;
    }
    else if (dt->super) {
        ndt->super = (jl_datatype_t*)inst_type_w_((jl_value_t*)dt->super, env,n,stack, 1);
        jl_gc_wb(ndt, ndt->super);
    }
    ftypes = dt->types;
    if (!istuple && ndt->name->names == jl_emptysvec) {
        assert(ftypes == NULL || ftypes == jl_emptysvec);
        ndt->size = dt->size;
        ndt->layout = dt->layout;
        ndt->types = jl_emptysvec;
        if (jl_is_datatype_make_singleton(ndt)) {
            ndt->instance = jl_gc_alloc(ptls, 0, ndt);
            jl_gc_wb(ndt, ndt->instance);
        }
    }
    if (ftypes == NULL || dt->super == NULL) {
        // in the process of creating this type definition:
        // need to instantiate the super and types fields later
        assert(inside_typedef && !istuple);
        arraylist_push(&partial_inst, ndt);
    }
    else {
        if (ftypes != jl_emptysvec) {
            assert(!ndt->abstract);
            if (!istuple) {
                // recursively instantiate the types of the fields
                ndt->types = inst_all(ftypes, env, n, stack, 1);
                jl_gc_wb(ndt, ndt->types);
            }
            if (cacheable) {
                jl_compute_field_offsets(ndt);
                if (jl_is_datatype_make_singleton(ndt)) {
                    ndt->instance = jl_gc_alloc(ptls, 0, ndt);
                    jl_gc_wb(ndt, ndt->instance);
                }
            }
        }
        else {
            assert(ndt->name->names == jl_emptysvec);
        }
    }
    if (istuple)
        ndt->ninitialized = ntp;
    else
        ndt->ninitialized = dt->ninitialized;

    if (cacheable) {
        jl_cache_type_(ndt);
        JL_UNLOCK(&typecache_lock); // Might GC
    }

    JL_GC_POP();
    return (jl_value_t*)ndt;
}

static void check_tuple_parameter(jl_value_t *pi, size_t i, size_t np)
{
    // TODO: should possibly only allow Types and TypeVars, but see
    // https://github.com/JuliaLang/julia/commit/85f45974a581ab9af955bac600b90d9ab00f093b#commitcomment-13041922
    if (!valid_type_param(pi))
        jl_type_error_rt("Tuple", "parameter", (jl_value_t*)jl_type_type, pi);
    if (i != np-1 && jl_is_vararg_type(pi))
        jl_type_error_rt("Tuple", "non-final parameter", (jl_value_t*)jl_type_type, pi);
}

static jl_tupletype_t *jl_apply_tuple_type_v_(jl_value_t **p, size_t np, jl_svec_t *params)
{
    int cacheable = 1;
    for(size_t i=0; i < np; i++) {
        jl_value_t *pi = p[i];
        check_tuple_parameter(pi, i, np);
        if (!jl_is_leaf_type(pi))
            cacheable = 0;
    }
    jl_datatype_t *ndt = (jl_datatype_t*)inst_datatype(jl_anytuple_type, params, p, np,
                                                       cacheable, NULL, NULL, 0);
    return ndt;
}

JL_DLLEXPORT jl_tupletype_t *jl_apply_tuple_type(jl_svec_t *params)
{
    return jl_apply_tuple_type_v_(jl_svec_data(params), jl_svec_len(params), params);
}

JL_DLLEXPORT jl_tupletype_t *jl_apply_tuple_type_v(jl_value_t **p, size_t np)
{
    return jl_apply_tuple_type_v_(p, np, NULL);
}

jl_datatype_t *jl_inst_concrete_tupletype(jl_svec_t *p)
{
    return (jl_datatype_t*)inst_datatype(jl_anytuple_type, p, jl_svec_data(p), jl_svec_len(p), 1, NULL, NULL, 0);
}

jl_datatype_t *jl_inst_concrete_tupletype_v(jl_value_t **p, size_t np)
{
    return (jl_datatype_t*)inst_datatype(jl_anytuple_type, NULL, p, np, 1, NULL, NULL, 0);
}

static jl_svec_t *inst_all(jl_svec_t *p, jl_value_t **env, size_t n,
                           jl_typestack_t *stack, int check)
{
    size_t i;
    size_t lp = jl_svec_len(p);
    jl_svec_t *np = jl_alloc_svec(lp);
    JL_GC_PUSH1(&np);
    for(i=0; i < lp; i++) {
        jl_svecset(np, i, (jl_value_t*)inst_type_w_(jl_svecref(p,i), env, n, stack, check));
    }
    JL_GC_POP();
    return np;
}

static jl_value_t *inst_tuple_w_(jl_value_t *t, jl_value_t **env, size_t n,
                                 jl_typestack_t *stack, int check)
{
    jl_datatype_t *tt = (jl_datatype_t*)t;
    jl_svec_t *tp = tt->parameters;
    size_t ntp = jl_svec_len(tp);
    // Instantiate NTuple{3,Int}
    // Note this does not instantiate Tuple{Vararg{Int,3}}; that's done in
    // jl_apply_tuple_type_v_
    if (jl_is_va_tuple(tt) && ntp == 1) {
        // If this is a Tuple{Vararg{T,N}} with known N, expand it to
        // a fixed-length tuple
        jl_value_t *T=NULL, *N=NULL;
        jl_value_t *ttT = jl_tparam0(jl_tparam0(tt));
        jl_value_t *ttN = jl_tparam1(jl_tparam0(tt));
        int i;
        for (i = 0; i < 2*n; i+=2) {
            jl_value_t *tv = env[i];
            if (jl_is_typevar(tv)) {
                if (tv == ttT)
                    T = env[i+1];
                else if (tv == ttN)
                    N = env[i+1];
            }
        }
        if (T != NULL && N != NULL && jl_is_long(N)) {
            ssize_t nt = jl_unbox_long(N);
            if (nt < 0)
                jl_errorf("size or dimension is negative: %zd", nt);
            return (jl_value_t*)jl_tupletype_fill(nt, T);
        }
    }
    jl_value_t **iparams;
    int onstack = ntp < jl_page_size/sizeof(jl_value_t*);
    JL_GC_PUSHARGS(iparams, onstack ? ntp : 1);
    jl_svec_t *ip_heap=NULL;
    if (!onstack) {
        ip_heap = jl_alloc_svec(ntp);
        iparams[0] = (jl_value_t*)ip_heap;
        iparams = jl_svec_data(ip_heap);
    }
    int cacheable = 1;
    if (jl_is_va_tuple(tt)) {
        cacheable = 0;
    }
    int i;
    for(i=0; i < ntp; i++) {
        jl_value_t *elt = jl_svecref(tp, i);
        jl_value_t *pi = (jl_value_t*)inst_type_w_(elt, env, n, stack, 0);
        iparams[i] = pi;
        if (ip_heap)
            jl_gc_wb(ip_heap, pi);
        check_tuple_parameter(pi, i, ntp);
        if (cacheable && !jl_is_leaf_type(pi)) {
            cacheable = 0;
        }
    }
    jl_value_t *result = inst_datatype((jl_datatype_t*)tt, ip_heap, iparams, ntp, cacheable,
                                       stack, env, n);
    JL_GC_POP();
    return result;
}

static jl_value_t *inst_type_w_(jl_value_t *t, jl_value_t **env, size_t n,
                                jl_typestack_t *stack, int check)
{
    size_t i, j;
    if (n == 0) return t;
    if (jl_is_typevar(t)) {
        for(i=0; i < n; i++) {
            if (env[i*2] == t) {
                jl_value_t *val = env[i*2+1];
                if (check && !jl_is_typevar(val) && !jl_subtype(val, t, 0)) {
                    jl_type_error_rt("type parameter",
                                     jl_symbol_name(((jl_tvar_t*)t)->name),
                                     t, val);
                }
                return val;
            }
        }
        return (jl_value_t*)t;
    }
    if (jl_is_uniontype(t)) {
        jl_svec_t *p = inst_all(((jl_uniontype_t*)t)->types, env, n, stack, 1);
        JL_GC_PUSH1(&p);
        jl_value_t *res = (jl_value_t*)jl_type_union(p);
        JL_GC_POP();
        return res;
    }
    if (!jl_is_datatype(t))
        return t;
    jl_datatype_t *tt = (jl_datatype_t*)t;
    jl_svec_t *tp = tt->parameters;
    if (tp == jl_emptysvec)
        return (jl_value_t*)t;
    jl_typename_t *tn = tt->name;
    jl_value_t *tc = tn->primary;
    // don't instantiate "Foo" without parameters inside Foo
    if (t == tc && stack!=NULL)
        return (jl_value_t*)t;
    assert(jl_is_datatype(tc));
    if (tn == jl_tuple_typename)
        return inst_tuple_w_(t, env, n, stack, check);
    size_t ntp = jl_svec_len(tp);
    assert(ntp == jl_svec_len(((jl_datatype_t*)tc)->parameters));
    jl_value_t **iparams;
    JL_GC_PUSHARGS(iparams, ntp);
    int cacheable = 1, bound = 0;
    for(i=0; i < ntp; i++) {
        jl_value_t *elt = jl_svecref(tp, i);
        if (elt == t) {
            iparams[i] = t;
        }
        else {
            jl_value_t *tv = jl_svecref(((jl_datatype_t*)tc)->parameters, i);
            iparams[i] = (jl_value_t*)inst_type_w_(elt, env, n, stack, elt != tv);
            if (jl_is_typevar(tv) && !jl_is_typevar(iparams[i])) {
                if (!jl_subtype(iparams[i], tv, 0)) {
                    jl_type_error_rt(jl_symbol_name(tt->name->name),
                                     jl_symbol_name(((jl_tvar_t*)tv)->name),
                                     tv, iparams[i]);
                }
            }
            if (!bound) {
                for(j=0; j < n; j++) {
                    if (env[j*2] == tv) {
                        bound = 1; break;
                    }
                }
            }
            if (cacheable && jl_has_typevars(iparams[i]))
                cacheable = 0;
        }
    }
    // if t's parameters are not bound in the environment, return it uncopied (#9378)
    if (!bound && t == tc) { JL_GC_POP(); return (jl_value_t*)t; }

    jl_value_t *result = inst_datatype((jl_datatype_t*)tt, NULL, iparams, ntp, cacheable,
                                       stack, env, n);
    JL_GC_POP();
    return result;
}

jl_value_t *jl_instantiate_type_with(jl_value_t *t, jl_value_t **env, size_t n)
{
    return inst_type_w_((jl_value_t*)t, env, n, NULL, 1);
}

jl_datatype_t *jl_wrap_Type(jl_value_t *t)
{
    jl_value_t *env[2];
    env[0] = jl_tparam0(jl_type_type);
    //if (jl_is_typector(t))
    //    env[1] = (jl_value_t*)((jl_typector_t*)t)->body;
    //else
        env[1] = t;
    return (jl_datatype_t*)
        jl_instantiate_type_with((jl_value_t*)jl_type_type, env, 1);
}

void jl_reinstantiate_inner_types(jl_datatype_t *t) // can throw!
{
    jl_ptls_t ptls = jl_get_ptls_states();
    inside_typedef = 0;
    assert(jl_is_datatype(t));
    jl_typestack_t top;
    top.tt = t;
    top.prev = NULL;
    size_t i, j, n = jl_svec_len(t->parameters);
    if (n == 0) {
        assert(partial_inst.len == 0);
        return;
    }

    jl_value_t **env = (jl_value_t**)alloca(n * 2 * sizeof(void*));
    for (i = 0; i < n; i++) {
        env[i * 2] = jl_svecref(t->parameters, i);
        env[i * 2 + 1] = NULL;
    }

    for (j = 0; j < partial_inst.len; j++) {
        jl_datatype_t *ndt = (jl_datatype_t*)partial_inst.items[j];
        assert(ndt->name->primary == (jl_value_t*)t);
        for (i = 0; i < n; i++)
            env[i * 2 + 1] = jl_svecref(ndt->parameters, i);

        ndt->super = (jl_datatype_t*)inst_type_w_((jl_value_t*)t->super, env, n, &top, 1);
        jl_gc_wb(ndt, ndt->super);
    }

    if (t->name->names != jl_emptysvec) {
        for (j = 0; j < partial_inst.len; j++) {
            jl_datatype_t *ndt = (jl_datatype_t*)partial_inst.items[j];
            assert(ndt->name->primary == (jl_value_t*)t);
            for (i = 0; i < n; i++)
                env[i * 2 + 1] = jl_svecref(ndt->parameters, i);

            ndt->types = inst_all(t->types, env, n, &top, /*check*/1);
            jl_gc_wb(ndt, ndt->types);
            if (ndt->uid) { // cacheable
                jl_compute_field_offsets(ndt);
                if (jl_is_datatype_make_singleton(ndt)) {
                    ndt->instance = jl_gc_alloc(ptls, 0, ndt);
                    jl_gc_wb(ndt, ndt->instance);
                }
            }
        }
    }
    else {
        assert(t->types == jl_emptysvec);
    }
    partial_inst.len = 0;
}

void jl_reset_instantiate_inner_types(jl_datatype_t *t)
{
    // the declaration of `t` is invalid, forget about all of the WIP
    inside_typedef = 0;
    partial_inst.len = 0;
}

// subtype comparison

static int jl_subtype_le(jl_value_t *a, jl_value_t *b, int ta, int invariant);

static int jl_tuple_subtype_(jl_value_t **child, size_t clenr,
                             jl_datatype_t *pdt, int ta, int invariant)
{
    size_t plenr = jl_nparams(pdt);
    jl_value_t **parent = jl_svec_data(pdt->parameters);
    size_t plenf, clenf;
    jl_vararg_kind_t ckind, pkind;
    jl_tuple_lenkind_t clenkind, plenkind;
    int bottom = 0;
    // Stage 1
    clenf = data_vararg_params(child, clenr, NULL, &ckind, &clenkind);
    plenf = tuple_vararg_params(pdt->parameters, NULL, &pkind, &plenkind);
    // Stage 2
    if (clenkind == JL_TUPLE_FIXED && plenkind == JL_TUPLE_FIXED) {
        bottom = clenf != plenf;
    }
    else if (clenkind == JL_TUPLE_FIXED && plenkind == JL_TUPLE_VAR) {
        bottom = clenf+1 < plenf;
    }
    if (plenr == 0 && clenr > 0)
        bottom = 1;
    if (bottom) return 0;
    size_t ci=0, pi=0;
    jl_value_t *ce=NULL, *pe=NULL;
    int cseq=0, pseq=0;
    // Stage 3
    int result = 0;
    while (1) {
        if (!cseq)
            cseq = !ta && ci<clenr && clenkind != JL_TUPLE_FIXED && jl_is_vararg_type(child[ci]);
        if (!pseq)
            pseq = pi<plenr && plenkind != JL_TUPLE_FIXED && jl_is_vararg_type(parent[pi]);
        if (cseq && !pseq)
            break;
        if (ci >= clenf) {
            result = pi >= plenf || (pseq && !invariant);
            break;
        }
        if (pi >= plenf && !pseq)
            break;
        if (ci < clenr) {
            ce = child[ci];
            if (jl_is_vararg_type(ce)) ce = jl_tparam0(ce);
        }
        if (pi < plenr) {
            pe = parent[pi];
            if (jl_is_vararg_type(pe)) pe = jl_tparam0(pe);
        }

        if (!jl_subtype_le(ce, pe, ta, invariant))
            break;

        if (cseq && pseq) {
            result = 1;
            break;
        }
        ci++;
        pi++;
    }
    return result;
}

int jl_tuple_subtype(jl_value_t **child, size_t cl, jl_datatype_t *pdt, int ta)
{
    return jl_tuple_subtype_(child, cl, pdt, ta, 0);
}

//  ta specifies whether typeof() should be implicitly applied to a.
int jl_subtype_le(jl_value_t *a, jl_value_t *b, int ta, int invariant)
{
    if (!ta&&jl_is_typector(a)) a = (jl_value_t*)((jl_typector_t*)a)->body;
    if (jl_is_typector(b)) b = (jl_value_t*)((jl_typector_t*)b)->body;
    if (ta) {
        if (jl_is_type_type(b)) {
            return jl_is_type(a) && jl_subtype_le(a, jl_tparam0(b), 0, 1);
        }
    }
    else if (a == b) {
        // Union{} <: Union{}
        return 1;
    }

    size_t i;
    if (!ta && jl_is_uniontype(a)) {
        jl_svec_t *ap = ((jl_uniontype_t*)a)->types;
        size_t l_ap = jl_svec_len(ap);
        if (invariant && !jl_is_typevar(b)) {
            return jl_subtype_le(a,b,0,0) && jl_subtype_le(b,a,0,0);
        }
        for(i=0; i < l_ap; i++) {
            if (!jl_subtype_le(jl_svecref(ap,i), b, 0, invariant))
                return 0;
        }
        return 1;
    }

    if (!ta && jl_is_type_type(a) && !invariant) {
        jl_value_t *tp0a = jl_tparam0(a);
        if (jl_is_typevar(tp0a)) {
            jl_value_t *ub = ((jl_tvar_t*)tp0a)->ub;
            jl_value_t *lb = ((jl_tvar_t*)tp0a)->lb;
            if (jl_subtype_le(ub, b, 1, 0) &&
                !jl_subtype_le((jl_value_t*)jl_any_type, ub, 0, 0)) {
                if (jl_subtype_le(lb, b, 1, 0))
                    return 1;
            }
        }
        else {
            if (jl_subtype_le(tp0a, b, 1, 0))
                return 1;
        }
    }

    if (jl_is_uniontype(b)) {
        if (invariant)
            return 0;
        jl_svec_t *bp = ((jl_uniontype_t*)b)->types;
        for(i=0; i < jl_svec_len(bp); i++) {
            if (jl_subtype_le(a, jl_svecref(bp,i), ta, invariant))
                return 1;
        }
        if (!ta && jl_is_typevar(a) && ((jl_tvar_t*)a)->ub == jl_bottom_type)
            return 1;
        return 0;
    }

    if (ta) a = (jl_value_t*)jl_typeof(a);

    if (jl_is_tuple_type(a)) {
        if (jl_is_tuple_type(b)) {
            return jl_tuple_subtype_(jl_svec_data(((jl_datatype_t*)a)->parameters), jl_nparams(a),
                                     (jl_datatype_t*)b, 0, invariant);
        }
    }

    if (a == b) return 1;
    if (!invariant && (jl_datatype_t*)b == jl_any_type) return 1;

    if (jl_is_datatype(a) && jl_is_datatype(b)) {
        if ((jl_datatype_t*)a == jl_any_type) return 0;
        jl_datatype_t *tta = (jl_datatype_t*)a;
        jl_datatype_t *ttb = (jl_datatype_t*)b;
        int super=0;
        while (tta != (jl_datatype_t*)jl_any_type) {
            if (tta->name == ttb->name) {
                if (super && ttb->name == jl_type_type->name && jl_is_typevar(jl_tparam0(b))) {
                    if (jl_subtype_le(a, jl_tparam0(b), 0, 1))
                        return 1;
                }
                assert(jl_nparams(tta) == jl_nparams(ttb));
                size_t l = jl_nparams(tta);
                for(i=0; i < l; i++) {
                    jl_value_t *apara = jl_tparam(tta,i);
                    jl_value_t *bpara = jl_tparam(ttb,i);
                    if (invariant) {
                        if (jl_is_typevar(bpara) && !((jl_tvar_t*)bpara)->bound) {
                            if (!jl_is_typevar(apara))
                                return 0;
                        }
                    }
                    if (!jl_subtype_le(apara, bpara, 0, 1))
                        return 0;
                }
                return 1;
            }
            else if (invariant) {
                return 0;
            }
            if (tta->super == NULL)
                jl_errorf("circular type parameter constraint in definition of %s", jl_symbol_name(tta->name->name));
            tta = tta->super; super = 1;
        }
        assert(!invariant);
        return 0;
    }

    if (jl_is_typevar(a)) {
        if (jl_is_typevar(b)) {
            return
                jl_subtype_le((jl_value_t*)((jl_tvar_t*)a)->ub,
                              (jl_value_t*)((jl_tvar_t*)b)->ub, 0, 0) &&
                jl_subtype_le((jl_value_t*)((jl_tvar_t*)b)->lb,
                              (jl_value_t*)((jl_tvar_t*)a)->lb, 0, 0);
        }
        if (invariant) {
            return 0;
        }
        return jl_subtype_le((jl_value_t*)((jl_tvar_t*)a)->ub, b, 0, 0);
    }
    if (jl_is_typevar(b)) {
        return jl_subtype_le(a, (jl_value_t*)((jl_tvar_t*)b)->ub, 0, 0) &&
            jl_subtype_le((jl_value_t*)((jl_tvar_t*)b)->lb, a, 0, 0);
    }
    if ((jl_datatype_t*)a == jl_any_type) return 0;

    return jl_egal(a, b);
}

JL_DLLEXPORT int jl_subtype(jl_value_t *a, jl_value_t *b, int ta)
{
    return jl_subtype_le(a, b, ta, 0);
}

int jl_subtype_invariant(jl_value_t *a, jl_value_t *b, int ta)
{
    return jl_subtype_le(a, b, ta, 1);
}

// specificity comparison

static int type_eqv_with_ANY(jl_value_t *a, jl_value_t *b)
{
    // equate ANY and Any for specificity purposes, #16153
    return ((a == (jl_value_t*)jl_any_type && b == jl_ANY_flag) ||
            (b == (jl_value_t*)jl_any_type && a == jl_ANY_flag) ||
            type_eqv_(a, b));
}

static int jl_type_morespecific_(jl_value_t *a, jl_value_t *b, int invariant);

jl_datatype_t *jl_fix_vararg_bound(jl_datatype_t *tt, int nfix)
{
    assert(jl_is_va_tuple(tt));
    assert(nfix >= 0);
    jl_svec_t *tp = tt->parameters;
    size_t ntp = jl_svec_len(tp);
    jl_value_t *env[2] = {NULL, NULL};
    JL_GC_PUSH2(&env[0], &env[1]);
    env[0] = jl_tparam1(jl_tparam(tt, ntp-1));
    env[1] = jl_box_long(nfix);
    jl_datatype_t *ret = (jl_datatype_t*)jl_instantiate_type_with((jl_value_t*)tt, env, 1);
    JL_GC_POP();
    return ret;
}

static int jl_tuple_morespecific(jl_datatype_t *cdt, jl_datatype_t *pdt, int invariant)
{
    size_t clenr = jl_nparams(cdt);
    jl_value_t **child = jl_svec_data(cdt->parameters);
    size_t plenr = jl_nparams(pdt);
    jl_value_t **parent = jl_svec_data(pdt->parameters);
    size_t plenf, clenf;
    jl_vararg_kind_t ckind, pkind;
    jl_tuple_lenkind_t clenkind, plenkind;
    clenf = tuple_vararg_params(cdt->parameters, NULL, &ckind, &clenkind);
    plenf = tuple_vararg_params(pdt->parameters, NULL, &pkind, &plenkind);
    size_t ci=0, pi=0;
    int cseq=0, pseq=0;
    int some_morespecific = 0;
    jl_value_t *ce=NULL, *pe=NULL;
    while (1) {
        if (!cseq)
            cseq = (ci<clenr) && clenkind != JL_TUPLE_FIXED && jl_is_vararg_type(child[ci]);
        if (!pseq)
            pseq = (pi<plenr) && plenkind != JL_TUPLE_FIXED && jl_is_vararg_type(parent[pi]);
        if (ci >= clenf && !cseq)
            return 1;
        if (pi >= plenf && !pseq)
            return some_morespecific;
        if (ci < clenr) {
            ce = child[ci];
            if (jl_is_vararg_type(ce)) ce = jl_tparam0(ce);
        }
        if (pi < plenr) {
            pe = parent[pi];
            if (jl_is_vararg_type(pe)) pe = jl_tparam0(pe);
        }

        if (!jl_type_morespecific_(ce, pe, invariant)) {
            if (type_eqv_with_ANY(ce,pe)) {
                if (ci==clenf-1 && pi==plenf-1) {
                    if (!cseq && pseq)
                        return 1;
                    if (!some_morespecific)
                        return 0;
                }
            }
            else {
                return 0;
            }
        }
        else if (ci==clenr-1 && pi==plenr-1 && clenr == plenr && !cseq && pseq) {
            // make Vararg{X, 1} more specific than Vararg{X, N}
            if (jl_is_vararg_type(child[ci]) && type_eqv_with_ANY(ce,pe))
                return 1;
        }

        if (some_morespecific && cseq && !pseq)
            return 1;

        // at this point we know one element is strictly more specific
        if (!(type_eqv_with_ANY(ce,pe) ||
              (jl_is_typevar(pe) &&
               jl_types_equal(ce,((jl_tvar_t*)pe)->ub)))) {
            some_morespecific = 1;
            // here go into a different mode where we return 1
            // if the only reason the child is not more specific is
            // argument count (i.e. ...)
        }

        /*
          Ideally this would test `clenr > plenr`, but that causes something
          bad to happen with these two definitions:
          sub2ind(              inds::Indices{1},                I::Integer...)
          sub2ind{N,T<:Integer}(inds::Union{Dims{N},Indices{N}}, I::AbstractVector{T}...)
        */
        if (cseq && pseq) return clenr >= plenr || some_morespecific;
        ci++;
        pi++;
    }
    return 0;
}

static int partially_morespecific(jl_value_t *a, jl_value_t *b, int invariant)
{
    if (jl_is_uniontype(b)) {
        jl_svec_t *bp = ((jl_uniontype_t*)b)->types;
        size_t i, l=jl_svec_len(bp);
        for(i=0; i < l; i++) {
            jl_value_t *bi = jl_svecref(bp,i);
            if (jl_type_morespecific_(a, bi, invariant) &&
                !jl_type_morespecific_(bi, a, invariant)) {
                return 1;
            }
        }
        return 0;
    }
    return jl_type_morespecific_(a, b, invariant);
}

static int jl_type_morespecific_(jl_value_t *a, jl_value_t *b, int invariant)
{
    if (jl_is_typector(a)) a = (jl_value_t*)((jl_typector_t*)a)->body;
    if (jl_is_typector(b)) b = (jl_value_t*)((jl_typector_t*)b)->body;
    if (a == b) {
        // TODO; maybe change this
        return 1;
    }
    size_t i;
    if (jl_is_tuple_type(a)) {
        if (jl_is_tuple_type(b)) {
            return jl_tuple_morespecific((jl_datatype_t*)a, (jl_datatype_t*)b, invariant);
        }
    }

    if (jl_is_uniontype(a)) {
        jl_svec_t *ap = ((jl_uniontype_t*)a)->types;
        size_t l_ap = jl_svec_len(ap);
        if (jl_subtype_le(b, a, 0, 0)) {
            // fixes issue #4413
            if (!jl_subtype_le(a, b, 0, invariant))
                return 0;
        }
        else if (jl_subtype_le(a, b, 0, invariant)) {
            return 1;
        }
        // Union a is more specific than b if some element of a is
        // more specific than b, and b is not more specific than any
        // element of a.
        for(i=0; i < l_ap; i++) {
            jl_value_t *ai = jl_svecref(ap,i);
            if (partially_morespecific(ai, b, invariant) &&
                !jl_type_morespecific_(b, ai, invariant)) {
                if (partially_morespecific(b, a, invariant))
                    return 0;
                return 1;
            }
        }
        return 0;
    }

    if (jl_is_type_type(a) && !invariant) {
        jl_value_t *tp0a = jl_tparam0(a);
        if (jl_is_typevar(tp0a)) {
            jl_value_t *ub = ((jl_tvar_t*)tp0a)->ub;
            if (jl_subtype_le(ub, b, 1, 0) &&
                !jl_subtype_le((jl_value_t*)jl_any_type, ub, 0, 0)) {
                return 1;
            }
        }
        else {
            if (jl_subtype_le(tp0a, b, 1, 0))
                return 1;
        }
    }

    if (jl_is_uniontype(b)) {
        if (invariant)
            return 0;
        jl_svec_t *bp = ((jl_uniontype_t*)b)->types;
        for(i=0; i < jl_svec_len(bp); i++) {
            if (jl_type_morespecific_(a, jl_svecref(bp,i), invariant))
                return 1;
        }
        return 0;
    }

    if (!invariant && (jl_datatype_t*)b == jl_any_type) return 1;

    if (jl_is_datatype(a) && jl_is_datatype(b)) {
        if ((jl_datatype_t*)a == jl_any_type) return 0;
        jl_datatype_t *tta = (jl_datatype_t*)a;
        jl_datatype_t *ttb = (jl_datatype_t*)b;
        int super=0;
        while (tta != (jl_datatype_t*)jl_any_type) {
            if (tta->name == ttb->name) {
                if (super) {
                    if (tta->name != jl_type_type->name)
                        return 1;
                }
                if (super && ttb->name == jl_type_type->name && jl_is_typevar(jl_tparam0(b))) {
                    if (jl_type_morespecific_(a, jl_tparam0(b), 1))
                        return 1;
                }
                assert(jl_nparams(tta) == jl_nparams(ttb));
                for(i=0; i < jl_nparams(tta); i++) {
                    jl_value_t *apara = jl_tparam(tta,i);
                    jl_value_t *bpara = jl_tparam(ttb,i);
                    if (!jl_type_morespecific_(apara, bpara, 1))
                        return 0;
                }
                return 1;
            }
            else if (invariant) {
                return 0;
            }
            tta = tta->super; super = 1;
        }
        return 0;
    }

    if (jl_is_typevar(a)) {
        if (jl_is_typevar(b)) {
            return
                jl_type_morespecific_((jl_value_t*)((jl_tvar_t*)a)->ub,
                                      (jl_value_t*)((jl_tvar_t*)b)->ub, 0) &&
                jl_type_morespecific_((jl_value_t*)((jl_tvar_t*)b)->lb,
                                      (jl_value_t*)((jl_tvar_t*)a)->lb, 0);
        }
        if (invariant)
            return 0;
        return jl_subtype_le((jl_value_t*)((jl_tvar_t*)a)->ub, b, 0, 0);
    }
    if (jl_is_typevar(b)) {
        return jl_subtype_le(a, (jl_value_t*)((jl_tvar_t*)b)->ub, 0, 0) &&
            jl_subtype_le((jl_value_t*)((jl_tvar_t*)b)->lb, a, 0, 0);
    }
    if ((jl_datatype_t*)a == jl_any_type) return 0;

    return 0;
}

JL_DLLEXPORT int jl_type_morespecific(jl_value_t *a, jl_value_t *b)
{
    return jl_type_morespecific_(a, b, 0);
}

int jl_args_morespecific_(jl_value_t *a, jl_value_t *b)
{
    int msp = jl_type_morespecific(a,b);
    int btv = jl_has_typevars(b);
    if (btv) {
        if (jl_type_match_morespecific(a,b) == (jl_value_t*)jl_false) {
            if (jl_has_typevars(a))
                return 0;
            return msp;
        }
        if (jl_has_typevars(a)) {
            type_match_invariance_mask = 0;
            //int result = jl_type_match_morespecific(b,a) == (jl_value_t*)jl_false);
            // this rule seems to work better:
            int result = jl_type_match(b,a) == (jl_value_t*)jl_false;
            type_match_invariance_mask = 1;
            if (result)
                return 1;
        }
        int nmsp = jl_type_morespecific(b,a);
        if (nmsp == msp)
            return 0;
    }
    if (jl_has_typevars((jl_value_t*)a)) {
        int nmsp = jl_type_morespecific(b,a);
        if (nmsp && msp)
            return 1;
        if (!btv && jl_types_equal(a,b))
            return 1;
        if (jl_type_match_morespecific(b,a) != (jl_value_t*)jl_false)
            return 0;
    }
    return msp;
}

// Called when a is a bound-vararg and b is not a vararg. Sets the
// vararg length in a to match b, as long as this makes some earlier
// argument more specific.
int jl_args_morespecific_fix1(jl_value_t *a, jl_value_t *b, int swap)
{
    jl_datatype_t *tta = (jl_datatype_t*)a;
    jl_datatype_t *ttb = (jl_datatype_t*)b;
    size_t n = jl_nparams(tta);
    jl_datatype_t *newtta = jl_fix_vararg_bound(tta, jl_nparams(ttb)-n+1);
    int changed = 0;
    for (size_t i = 0; i < n-1; i++) {
        if (jl_tparam(tta, i) != jl_tparam(newtta, i)) {
            changed = 1;
            break;
        }
    }
    if (changed) {
        JL_GC_PUSH1(&newtta);
        int ret;
        if (type_eqv_(b, (jl_value_t*)newtta))
            ret = swap;
        else if (swap)
            ret = jl_args_morespecific_(b, (jl_value_t*)newtta);
        else
            ret = jl_args_morespecific_((jl_value_t*)newtta, b);
        JL_GC_POP();
        return ret;
    }
    if (swap)
        return jl_args_morespecific_(b, a);
    return jl_args_morespecific_(a, b);
}

JL_DLLEXPORT int jl_args_morespecific(jl_value_t *a, jl_value_t *b)
{
    if (jl_is_tuple_type(a) && jl_is_tuple_type(b)) {
        jl_datatype_t *tta = (jl_datatype_t*)a;
        jl_datatype_t *ttb = (jl_datatype_t*)b;
        size_t alenf, blenf;
        jl_vararg_kind_t akind, bkind;
        jl_tuple_lenkind_t alenkind, blenkind;
        alenf = tuple_vararg_params(tta->parameters, NULL, &akind, &alenkind);
        blenf = tuple_vararg_params(ttb->parameters, NULL, &bkind, &blenkind);
        // When one is JL_VARARG_BOUND and the other has fixed length,
        // allow the argument length to fix the tvar
        if (akind == JL_VARARG_BOUND && blenkind == JL_TUPLE_FIXED && blenf >= alenf) {
            return jl_args_morespecific_fix1(a, b, 0);
        }
        if (bkind == JL_VARARG_BOUND && alenkind == JL_TUPLE_FIXED && alenf >= blenf) {
            return jl_args_morespecific_fix1(b, a, 1);
        }
    }
    return jl_args_morespecific_(a, b);
}

// ----------------------------------------------------------------------------

int type_match_invariance_mask = 1;

static jl_value_t *type_match_(jl_value_t *child, jl_value_t *parent,
                               cenv_t *env, int morespecific, int invariant);

static jl_value_t *tuple_match(jl_datatype_t *child, jl_datatype_t *parent,
                               cenv_t *env, int morespecific, int invariant)
{
    size_t ci=0, pi=0;
    size_t clenr = jl_nparams(child);
    size_t plenr = jl_nparams(parent);
    size_t plenf, clenf;
    jl_vararg_kind_t ckind, pkind;
    jl_tuple_lenkind_t clenkind, plenkind;
    clenf = tuple_vararg_params(child->parameters, NULL, &ckind, &clenkind);
    plenf = tuple_vararg_params(parent->parameters, NULL, &pkind, &plenkind);
    int cseq=0, pseq=0;
    jl_value_t *ce=NULL, *pe=NULL, *cn=NULL, *pn=NULL;
    int mode = 0;
    invariant = invariant & type_match_invariance_mask;
    while(1) {
        if (!cseq)
            cseq = (ci<clenr) && clenkind != JL_TUPLE_FIXED && jl_is_vararg_type(jl_tparam(child,ci));
        if (!pseq)
            pseq = (pi<plenr) && plenkind != JL_TUPLE_FIXED && jl_is_vararg_type(jl_tparam(parent,pi));
        if (!morespecific && cseq && !pseq)
            return jl_false;
        if (ci >= clenf && !cseq)
            return (mode || pi>=plenf || (pseq && !invariant)) ? jl_true : jl_false;
        if (pi >= plenf && !pseq)
            return mode ? jl_true : jl_false;
        if (ci < clenr) {
            ce = jl_tparam(child,ci);
            if (jl_is_vararg_type(ce)) {
                cn = jl_tparam1(ce);
                ce = jl_tparam0(ce);
            }
        }
        if (pi < plenr) {
            pe = jl_tparam(parent,pi);
            if (jl_is_vararg_type(pe)) {
                pn = jl_tparam1(pe);
                pe = jl_tparam0(pe);
            }
        }

        int n = env->n;
        if (type_match_(ce, pe, env, morespecific, invariant) == jl_false) {
            env->n = n;
            if (jl_types_equal_generic(ce,pe,1)) {
                if (ci==clenf-1 && pi==plenf-1 && !cseq && pseq) {
                    return jl_true;
                }
                if (!mode) return jl_false;
            }
            else {
                return jl_false;
            }
        }
        // Match the number parameter in Vararg, too
        if (cseq && pseq) {
            n = env->n;
            if (type_match_(cn, pn, env, morespecific, invariant) == jl_false) {
                env->n = n;
                return jl_false;
            }
        }

        if (mode && cseq && !pseq)
            return jl_true;

        if (morespecific) {
            if (!(jl_types_equal_generic(ce,pe,1) ||
                  (jl_is_typevar(pe) &&
                   jl_types_equal(ce,((jl_tvar_t*)pe)->ub)))) {
                mode = 1;
            }
        }

        if (cseq && pseq) return jl_true;
        ci++;
        pi++;
    }
    return jl_false;
}

static jl_value_t *type_match_(jl_value_t *child, jl_value_t *parent,
                               cenv_t *env, int morespecific, int invariant)
{
    jl_value_t *tmp, *tmp2;
    invariant = invariant & type_match_invariance_mask;
    if (jl_is_typector(child))
        child = (jl_value_t*)((jl_typector_t*)child)->body;
    if (jl_is_typector(parent))
        parent = (jl_value_t*)((jl_typector_t*)parent)->body;
    size_t i, j;
    if (match_intersection_mode && jl_is_typevar(child) && !jl_is_typevar(parent)) {
        tmp = child;
        child = parent;
        parent = tmp;
    }
    if (jl_is_typevar(parent)) {
        // make sure type is within this typevar's bounds
        if (morespecific) {
            if (!jl_type_morespecific_(child, parent, 0))
                return jl_false;
        }
        else {
            if (!jl_subtype_le(child, parent, 0, 0))
                return jl_false;
        }
        if (!match_intersection_mode) {
            if (!((jl_tvar_t*)parent)->bound) return jl_true;
        }
        for(int i=0; i < env->n; i+=2) {
            if (env->data[i] == (jl_value_t*)parent) {
                jl_value_t *pv = env->data[i+1];
                if (jl_is_typevar(pv) && jl_is_typevar(child)) {
                    if (pv == (jl_value_t*)child)
                        return jl_true;
                    return jl_false;
                }
                if (morespecific) {
                    if (jl_type_morespecific_(child, pv, 0)) {
                        return jl_true;
                    }
                    else if (!jl_is_typevar(child) && !jl_type_morespecific_(pv, child, 0)) {
                        return jl_true;
                    }
                    else if (jl_subtype(pv, child, 0)) {
                        env->data[i+1] = (jl_value_t*)child;
                        return jl_true;
                    }
                }
                else {
                    if (type_eqv_(child, pv))
                        return jl_true;
                }
                return jl_false;
            }
        }
        extend(parent, child, env);
        return jl_true;
    }

    if (child == parent) return jl_true;

    if (jl_is_typevar(child)) {
        if (!invariant || morespecific) {
            if (morespecific) {
                if (jl_type_morespecific_(child, parent, 0))
                    return jl_true;
            }
            else {
                if (jl_subtype_le(child, parent, 0, 0))
                    return jl_true;
            }
        }
        return jl_false;
    }
    if (!invariant && parent == (jl_value_t*)jl_any_type)
        return jl_true;
    if (child  == (jl_value_t*)jl_any_type) return jl_false;

    if (jl_is_uniontype(child)) {
        jl_svec_t *t = ((jl_uniontype_t*)child)->types;
        if (morespecific) {
            jl_value_t **rts;
            JL_GC_PUSHARGS(rts, MAX_CENV_SIZE);
            cenv_t tenv; tenv.data = rts;
            for(i=0; i < jl_svec_len(t); i++) {
                int n = env->n;
                tmp = type_match_(jl_svecref(t,i), parent, env, 1, invariant);
                if (tmp != jl_false) {
                    tenv.n = 0;
                    tmp2 = type_match_(parent, jl_svecref(t,i), &tenv, 1, invariant);
                    if (tmp2 == jl_false) {
                        n = env->n;
                        for(j=0; j < jl_svec_len(t); j++) {
                            tenv.n = 0;
                            env->n = n;
                            if (type_match_(parent, jl_svecref(t,j),
                                            &tenv, 1, invariant) != jl_false &&
                                type_match_(jl_svecref(t,j), parent,
                                            env, 1, invariant) == jl_false) {
                                env->n = n;
                                JL_GC_POP();
                                return jl_false;
                            }
                        }
                        JL_GC_POP();
                        return jl_true;
                    }
                }
                else {
                    env->n = n;
                }
            }
            JL_GC_POP();
            return jl_false;
        }
        else {
            for(i=0; i < jl_svec_len(t); i++) {
                int n = env->n;
                if (type_match_(jl_svecref(t,i), parent, env, morespecific,
                                invariant) == jl_false)
                    { env->n = n; return jl_false; }
            }
            if (invariant && child == (jl_value_t*)jl_bottom_type &&
                !jl_is_typevar(parent))
                return jl_false;
        }
        return jl_true;
    }
    if (jl_is_uniontype(parent)) {
        jl_svec_t *t = ((jl_uniontype_t*)parent)->types;
        int n = env->n;
        for(i=0; i < jl_svec_len(t); i++) {
            env->n = n;
            if (type_match_(child, jl_svecref(t,i), env,
                            morespecific, invariant) != jl_false)
                return jl_true;
        }
        return jl_false;
    }

    if (jl_is_tuple_type(child)) {
        if (jl_is_tuple_type(parent)) {
            return tuple_match((jl_datatype_t*)child, (jl_datatype_t*)parent, env,
                               morespecific, invariant);
        }
        return jl_false;
    }
    if (jl_is_tuple_type(parent)) {
        return jl_false;
    }

    if (!jl_is_datatype(child) || !jl_is_datatype(parent)) {
        return jl_egal(child,parent) ? jl_true : jl_false;
    }
    jl_datatype_t *tta = (jl_datatype_t*)child;
    jl_datatype_t *ttb = (jl_datatype_t*)parent;
    int super = 0;
    while (tta != (jl_datatype_t*)jl_any_type) {
        if (tta->name == ttb->name) {
            // note: DataType <: Type, but Type{T} <: DataType
            // for any specific T.
            if (super && morespecific && tta->name != jl_type_type->name)
                return jl_true;
            assert(jl_nparams(tta) == jl_nparams(ttb));
            for(i=0; i < jl_nparams(tta); i++) {
                int n = env->n;
                if (type_match_(jl_tparam(tta,i), jl_tparam(ttb,i),
                                env, morespecific, 1) == jl_false)
                    { env->n = n; return jl_false; }
            }
            return jl_true;
        }
        else if (invariant) {
            return jl_false;
        }
        tta = tta->super; super = 1;
    }
    assert(!invariant);
    if (((jl_datatype_t*)child)->name == jl_type_type->name &&
        ttb->name != jl_type_type->name) {
        // Type{T} also matches >:typeof(T)
        return type_match_(jl_typeof(jl_tparam0(child)),
                           parent, env, morespecific, 0);
    }
    return jl_false;
}

/*
  typically a is a concrete type and b is a type containing typevars.
  this function tries to find a typevar assignment such that "a" is a subtype
  of "b".
  returns a tuple of (typevar,type,...) pairs.
  used to infer static parameter values in generic method definitions.
*/
jl_value_t *jl_type_match_(jl_value_t *a, jl_value_t *b, int morespecific)
{
    jl_value_t **rts;
    JL_GC_PUSHARGS(rts, MAX_CENV_SIZE);
    cenv_t env; env.n = 0; env.data = rts;
    jl_value_t *m = type_match_(a, b, &env, morespecific, 0);
    if (m != jl_false) {
        m = (jl_value_t*)jl_alloc_svec_uninit(env.n);
        for(int i=0; i < env.n; i++) {
            jl_svecset(m, i, env.data[i]);
        }
    }
    JL_GC_POP();
    return m;
}

jl_value_t *jl_type_match(jl_value_t *a, jl_value_t *b)
{
    return jl_type_match_(a, b, 0);
}

jl_value_t *jl_type_match_morespecific(jl_value_t *a, jl_value_t *b)
{
    return jl_type_match_(a, b, 1);
}

// initialization -------------------------------------------------------------

JL_DLLEXPORT jl_tvar_t *jl_new_typevar_(jl_sym_t *name, jl_value_t *lb,
                                        jl_value_t *ub, jl_value_t *b)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_tvar_t *tv = (jl_tvar_t*)jl_gc_alloc(ptls, sizeof(jl_tvar_t),
                                            jl_tvar_type);
    tv->name = name;
    tv->lb = lb;
    tv->ub = ub;
    tv->bound = (b != jl_false);
    return tv;
}

JL_DLLEXPORT jl_tvar_t *jl_new_typevar(jl_sym_t *name, jl_value_t *lb,
                                       jl_value_t *ub)
{
    return jl_new_typevar_(name, lb, ub, jl_false);
}

static jl_tvar_t *tvar(const char *name)
{
    return jl_new_typevar(jl_symbol(name), (jl_value_t*)jl_bottom_type,
                          (jl_value_t*)jl_any_type);
}

extern void jl_init_int32_int64_cache(void);

void jl_init_types(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    arraylist_new(&partial_inst, 0);
    // create base objects
    jl_datatype_type = jl_new_uninitialized_datatype();
    jl_set_typeof(jl_datatype_type, jl_datatype_type);
    jl_typename_type = jl_new_uninitialized_datatype();
    jl_sym_type = jl_new_uninitialized_datatype();
    jl_symbol_type = jl_sym_type;
    jl_simplevector_type = jl_new_uninitialized_datatype();
    jl_methtable_type = jl_new_uninitialized_datatype();
    jl_nothing = jl_gc_alloc(ptls, 0, NULL);

    jl_default_cgparams.hooks.module_setup = jl_nothing;
    jl_default_cgparams.hooks.module_activation = jl_nothing;
    jl_default_cgparams.hooks.raise_exception = jl_nothing;

    jl_emptysvec = (jl_svec_t*)jl_gc_alloc(ptls, sizeof(void*),
                                           jl_simplevector_type);
    jl_svec_set_len_unsafe(jl_emptysvec, 0);

    jl_any_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Any"), NULL, jl_emptysvec);
    jl_any_type->super = jl_any_type;
    jl_type_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Type"), jl_any_type, jl_emptysvec);
    jl_type_type_mt = jl_new_method_table(jl_type_type->name->name, ptls->current_module);
    jl_type_type->name->mt = jl_type_type_mt;

    // initialize them. lots of cycles.
    jl_datatype_type->name = jl_new_typename(jl_symbol("DataType"));
    jl_datatype_type->name->primary = (jl_value_t*)jl_datatype_type;
    jl_datatype_type->super = jl_type_type;
    jl_datatype_type->parameters = jl_emptysvec;
    jl_datatype_type->name->names = jl_svec(17,
                                            jl_symbol("name"),
                                            jl_symbol("super"),
                                            jl_symbol("parameters"),
                                            jl_symbol("types"),
                                            jl_symbol("instance"),
                                            jl_symbol("layout"),
                                            jl_symbol("size"),
                                            jl_symbol("ninitialized"),
                                            jl_symbol("uid"),
                                            jl_symbol("abstract"),
                                            jl_symbol("mutable"),
                                            jl_symbol("llvm::StructType"),
                                            jl_symbol("llvm::DIType"),
                                            jl_symbol("depth"),
                                            jl_symbol("hastypevars"),
                                            jl_symbol("haswildcard"),
                                            jl_symbol("isleaftype"));
    jl_datatype_type->types = jl_svec(17,
                                      jl_typename_type,
                                      jl_type_type,
                                      jl_simplevector_type,
                                      jl_simplevector_type,
                                      jl_any_type, // instance
                                      jl_any_type, jl_any_type, jl_any_type, jl_any_type,
                                      jl_any_type, jl_any_type, jl_any_type, jl_any_type,
                                      jl_any_type, jl_any_type, jl_any_type, jl_any_type);
    jl_datatype_type->instance = NULL;
    jl_datatype_type->uid = jl_assign_type_uid();
    jl_datatype_type->struct_decl = NULL;
    jl_datatype_type->ditype = NULL;
    jl_datatype_type->abstract = 0;
    // NOTE: types should not really be mutable, but the instance and
    // struct_decl fields are basically caches, which are mutated.
    jl_datatype_type->mutabl = 1;
    jl_datatype_type->ninitialized = 4;

    jl_typename_type->name = jl_new_typename(jl_symbol("TypeName"));
    jl_typename_type->name->primary = (jl_value_t*)jl_typename_type;
    jl_typename_type->name->mt = jl_new_method_table(jl_typename_type->name->name, ptls->current_module);
    jl_typename_type->super = jl_any_type;
    jl_typename_type->parameters = jl_emptysvec;
    jl_typename_type->name->names = jl_svec(8, jl_symbol("name"), jl_symbol("module"),
                                            jl_symbol("names"), jl_symbol("primary"),
                                            jl_symbol("cache"), jl_symbol("linearcache"),
                                            jl_symbol("hash"), jl_symbol("mt"));
    jl_typename_type->types = jl_svec(8, jl_sym_type, jl_any_type, jl_simplevector_type,
                                      jl_type_type, jl_simplevector_type, jl_simplevector_type,
                                      jl_any_type, jl_any_type);
    jl_typename_type->uid = jl_assign_type_uid();
    jl_typename_type->instance = NULL;
    jl_typename_type->struct_decl = NULL;
    jl_typename_type->ditype = NULL;
    jl_typename_type->abstract = 0;
    jl_typename_type->mutabl = 1;
    jl_typename_type->ninitialized = 2;

    jl_methtable_type->name = jl_new_typename(jl_symbol("MethodTable"));
    jl_methtable_type->name->primary = (jl_value_t*)jl_methtable_type;
    jl_methtable_type->name->mt = jl_new_method_table(jl_methtable_type->name->name, ptls->current_module);
    jl_methtable_type->super = jl_any_type;
    jl_methtable_type->parameters = jl_emptysvec;
    jl_methtable_type->name->names = jl_svec(9, jl_symbol("name"), jl_symbol("defs"),
                                             jl_symbol("cache"), jl_symbol("max_args"),
                                             jl_symbol("kwsorter"), jl_symbol("module"),
                                             jl_symbol("backedges"), jl_symbol(""), jl_symbol(""));
    jl_methtable_type->types = jl_svec(9, jl_sym_type, jl_any_type, jl_any_type, jl_any_type/*jl_long*/,
                                       jl_any_type, jl_any_type/*module*/,
                                       jl_any_type/*any vector*/, jl_any_type/*long*/, jl_any_type/*int32*/);
    jl_methtable_type->uid = jl_assign_type_uid();
    jl_methtable_type->instance = NULL;
    jl_methtable_type->struct_decl = NULL;
    jl_methtable_type->ditype = NULL;
    jl_methtable_type->abstract = 0;
    jl_methtable_type->mutabl = 1;
    jl_methtable_type->ninitialized = 4;

    jl_sym_type->name = jl_new_typename(jl_symbol("Symbol"));
    jl_sym_type->name->primary = (jl_value_t*)jl_sym_type;
    jl_sym_type->name->mt = jl_new_method_table(jl_sym_type->name->name, ptls->current_module);
    jl_sym_type->super = jl_any_type;
    jl_sym_type->parameters = jl_emptysvec;
    jl_sym_type->name->names = jl_emptysvec;
    jl_sym_type->types = jl_emptysvec;
    jl_sym_type->instance = NULL;
    jl_sym_type->uid = jl_assign_type_uid();
    jl_sym_type->struct_decl = NULL;
    jl_sym_type->ditype = NULL;
    jl_sym_type->size = 0;
    jl_sym_type->abstract = 0;
    jl_sym_type->mutabl = 1;
    jl_sym_type->ninitialized = 0;

    jl_simplevector_type->name = jl_new_typename(jl_symbol("SimpleVector"));
    jl_simplevector_type->name->primary = (jl_value_t*)jl_simplevector_type;
    jl_simplevector_type->name->mt = jl_new_method_table(jl_simplevector_type->name->name, ptls->current_module);
    jl_simplevector_type->super = jl_any_type;
    jl_simplevector_type->parameters = jl_emptysvec;
    jl_simplevector_type->name->names = jl_svec(1, jl_symbol("length"));
    jl_simplevector_type->types = jl_svec(1, jl_any_type);
    jl_simplevector_type->uid = jl_assign_type_uid();
    jl_simplevector_type->instance = NULL;
    jl_simplevector_type->struct_decl = NULL;
    jl_simplevector_type->ditype = NULL;
    jl_simplevector_type->abstract = 0;
    jl_simplevector_type->mutabl = 1;
    jl_simplevector_type->ninitialized = 1;

    // now they can be used to create the remaining base kinds and types
    jl_void_type = jl_new_datatype(jl_symbol("Void"), jl_any_type, jl_emptysvec,
                                   jl_emptysvec, jl_emptysvec, 0, 0, 0);
    jl_set_typeof(jl_nothing, jl_void_type);
    jl_void_type->instance = jl_nothing;

    jl_uniontype_type = jl_new_datatype(jl_symbol("Union"),
                                        jl_type_type, jl_emptysvec,
                                        jl_svec(1, jl_symbol("types")),
                                        jl_svec(1, jl_simplevector_type),
                                        0, 0, 1);

    jl_bottom_type = (jl_value_t*)jl_new_struct(jl_uniontype_type, jl_emptysvec);

    jl_tvar_type = jl_new_datatype(jl_symbol("TypeVar"),
                                   jl_any_type, jl_emptysvec,
                                   jl_svec(4, jl_symbol("name"),
                                           jl_symbol("lb"), jl_symbol("ub"),
                                           jl_symbol("bound")),
                                   jl_svec(4, jl_sym_type, jl_type_type,
                                           jl_type_type, jl_any_type),
                                   0, 1, 3);

    vararg_sym = jl_symbol("Vararg");
    jl_svec_t *tv;
    tv = jl_svec2(tvar("T"),tvar("N"));
    jl_vararg_type = jl_new_abstracttype((jl_value_t*)vararg_sym, jl_any_type, tv);

    jl_anytuple_type = jl_new_datatype(jl_symbol("Tuple"), jl_any_type, jl_emptysvec,
                                       jl_emptysvec, jl_emptysvec, 0, 0, 0);
    jl_tuple_typename = jl_anytuple_type->name;
    jl_anytuple_type->uid = 0;
    jl_anytuple_type->parameters = jl_svec(1, jl_wrap_vararg((jl_value_t*)jl_any_type, (jl_value_t*)NULL));
    //jl_anytuple_type->parameters = jl_svec(1, jl_wrap_vararg((jl_value_t*)NULL, (jl_value_t*)NULL));
    jl_anytuple_type->types = jl_anytuple_type->parameters;
    jl_anytuple_type->layout = NULL;
    jl_anytuple_type->hastypevars = 1;
    jl_anytuple_type->haswildcard = 1;
    jl_anytuple_type->isleaftype = 0;

    jl_tvar_t *tttvar = jl_new_typevar(jl_symbol("T"),
                                       (jl_value_t*)jl_bottom_type,(jl_value_t*)jl_any_type);
    jl_type_type->parameters = jl_svec(1, tttvar);

    jl_tupletype_t *empty_tuple_type = jl_apply_tuple_type(jl_emptysvec);
    empty_tuple_type->uid = jl_assign_type_uid();
    jl_emptytuple = jl_gc_alloc(ptls, 0, empty_tuple_type);
    empty_tuple_type->instance = jl_emptytuple;

    // non-primitive definitions follow
    jl_int32_type = NULL;
    jl_int32_type = jl_new_bitstype((jl_value_t*)jl_symbol("Int32"),
                                    jl_any_type, jl_emptysvec, 32);
    jl_int64_type = NULL;
    jl_int64_type = jl_new_bitstype((jl_value_t*)jl_symbol("Int64"),
                                    jl_any_type, jl_emptysvec, 64);

    jl_uint8_type = NULL;
    jl_uint8_type = jl_new_bitstype((jl_value_t*)jl_symbol("UInt8"),
                                    jl_any_type, jl_emptysvec, 8);

    jl_ssavalue_type = jl_new_datatype(jl_symbol("SSAValue"), jl_any_type, jl_emptysvec,
                                       jl_svec1(jl_symbol("id")),
                                       jl_svec1(jl_long_type), 0, 0, 1);

    jl_abstractslot_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Slot"), jl_any_type,
                                               jl_emptysvec);

    jl_slotnumber_type = jl_new_datatype(jl_symbol("SlotNumber"), jl_abstractslot_type, jl_emptysvec,
                                         jl_svec1(jl_symbol("id")),
                                         jl_svec1(jl_long_type), 0, 0, 1);

    jl_typedslot_type = jl_new_datatype(jl_symbol("TypedSlot"), jl_abstractslot_type, jl_emptysvec,
                                        jl_svec(2, jl_symbol("id"), jl_symbol("typ")),
                                        jl_svec(2, jl_long_type, jl_any_type), 0, 0, 2);

    jl_init_int32_int64_cache();

    jl_bool_type = NULL;
    jl_bool_type = jl_new_bitstype((jl_value_t*)jl_symbol("Bool"),
                                   jl_any_type, jl_emptysvec, 8);
    jl_false = jl_box8(jl_bool_type, 0);
    jl_true  = jl_box8(jl_bool_type, 1);

    jl_typemap_level_type =
        jl_new_datatype(jl_symbol("TypeMapLevel"), jl_any_type, jl_emptysvec,
                        jl_svec(7,
                            jl_symbol("index_arg1"),
                            jl_symbol("arg1"),
                            jl_symbol("index_targ"),
                            jl_symbol("targ"),
                            jl_symbol("list"),
                            jl_symbol("any"),
                            jl_symbol("key")),
                        jl_svec(7,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type),
                        0, 1, 6);

    jl_typemap_entry_type =
        jl_new_datatype(jl_symbol("TypeMapEntry"), jl_any_type, jl_emptysvec,
                        jl_svec(11,
                            jl_symbol("next"),
                            jl_symbol("sig"),
                            jl_symbol("tvars"),
                            jl_symbol("simplesig"),
                            jl_symbol("guardsigs"),
                            jl_symbol("min_world"),
                            jl_symbol("max_world"),
                            jl_symbol("func"),
                            jl_symbol("isleafsig"),
                            jl_symbol("issimplesig"),
                            jl_symbol("va")),
                        jl_svec(11,
                            jl_any_type, // Union{TypeMapEntry, Void}
                            jl_type_type, // TupleType
                            jl_any_type, // Union{SimpleVector{TypeVar}, TypeVar}
                            jl_any_type, // TupleType
                            jl_any_type, // SimpleVector{TupleType}
                            jl_long_type, // Int
                            jl_long_type, // Int
                            jl_any_type, // Any
                            jl_bool_type,
                            jl_bool_type,
                            jl_bool_type),
                        0, 1, 5);

    jl_function_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Function"), jl_any_type, jl_emptysvec);
    jl_builtin_type  = jl_new_abstracttype((jl_value_t*)jl_symbol("Builtin"), jl_function_type, jl_emptysvec);

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_abstractarray_type =
        jl_new_abstracttype((jl_value_t*)jl_symbol("AbstractArray"),
                            jl_any_type, tv);

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_densearray_type =
        jl_new_abstracttype((jl_value_t*)jl_symbol("DenseArray"),
                            (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_abstractarray_type, tv),
                            tv);

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_array_type =
        jl_new_datatype(jl_symbol("Array"),
                        (jl_datatype_t*)
                        jl_apply_type((jl_value_t*)jl_densearray_type, tv),
                        tv,
                        jl_emptysvec, jl_emptysvec, 0, 1, 0);
    jl_array_typename = jl_array_type->name;
    jl_array_type->ninitialized = 0;
    static const jl_datatype_layout_t _jl_array_layout = { 0, sizeof(void*), 0, 0, 0 };
    jl_array_type->layout = &_jl_array_layout;

    jl_array_any_type =
        (jl_value_t*)jl_apply_type((jl_value_t*)jl_array_type,
                                   jl_svec(2, jl_any_type,
                                            jl_box_long(1)));

    jl_array_symbol_type =
        (jl_value_t*)jl_apply_type((jl_value_t*)jl_array_type,
                                   jl_svec(2, jl_sym_type,
                                            jl_box_long(1)));

    jl_array_uint8_type = jl_apply_type((jl_value_t*)jl_array_type,
                                        jl_svec2(jl_uint8_type, jl_box_long(1)));

    jl_expr_type =
        jl_new_datatype(jl_symbol("Expr"),
                        jl_any_type, jl_emptysvec,
                        jl_svec(3, jl_symbol("head"), jl_symbol("args"),
                                jl_symbol("typ")),
                        jl_svec(3, jl_sym_type, jl_array_any_type,
                                 jl_any_type),
                        0, 1, 3);

    jl_linenumbernode_type =
        jl_new_datatype(jl_symbol("LineNumberNode"), jl_any_type, jl_emptysvec,
                        jl_svec(1, jl_symbol("line")),
                        jl_svec(1, jl_long_type), 0, 0, 1);

    jl_labelnode_type =
        jl_new_datatype(jl_symbol("LabelNode"), jl_any_type, jl_emptysvec,
                        jl_svec(1, jl_symbol("label")),
                        jl_svec(1, jl_long_type), 0, 0, 1);

    jl_gotonode_type =
        jl_new_datatype(jl_symbol("GotoNode"), jl_any_type, jl_emptysvec,
                        jl_svec(1, jl_symbol("label")),
                        jl_svec(1, jl_long_type), 0, 0, 1);

    jl_quotenode_type =
        jl_new_datatype(jl_symbol("QuoteNode"), jl_any_type, jl_emptysvec,
                        jl_svec(1, jl_symbol("value")),
                        jl_svec(1, jl_any_type), 0, 0, 1);

    jl_newvarnode_type =
        jl_new_datatype(jl_symbol("NewvarNode"), jl_any_type, jl_emptysvec,
                        jl_svec(1, jl_symbol("slot")),
                        jl_svec(1, jl_slotnumber_type), 0, 0, 1);

    jl_module_type =
        jl_new_datatype(jl_symbol("Module"), jl_any_type, jl_emptysvec,
                        jl_svec(2, jl_symbol("name"), jl_symbol("parent")),
                        jl_svec(2, jl_sym_type, jl_any_type), 0, 1, 2);

    jl_globalref_type =
        jl_new_datatype(jl_symbol("GlobalRef"), jl_any_type, jl_emptysvec,
                        jl_svec(2, jl_symbol("mod"), jl_symbol("name")),
                        jl_svec(2, jl_module_type, jl_sym_type), 0, 0, 2);

    jl_code_info_type =
        jl_new_datatype(jl_symbol("CodeInfo"),
                        jl_any_type, jl_emptysvec,
                        jl_svec(9,
                                jl_symbol("code"),
                                jl_symbol("slottypes"),
                                jl_symbol("ssavaluetypes"),
                                jl_symbol("slotnames"),
                                jl_symbol("slotflags"),
                                jl_symbol("inferred"),
                                jl_symbol("inlineable"),
                                jl_symbol("propagate_inbounds"),
                                jl_symbol("pure")),
                        jl_svec(9,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_array_any_type,
                                jl_array_uint8_type,
                                jl_bool_type,
                                jl_bool_type,
                                jl_bool_type,
                                jl_bool_type),
                        0, 1, 9);

    jl_method_type =
        jl_new_datatype(jl_symbol("Method"),
                        jl_any_type, jl_emptysvec,
                        jl_svec(21,
                                jl_symbol("name"),
                                jl_symbol("module"),
                                jl_symbol("file"),
                                jl_symbol("line"),
                                jl_symbol("sig"),
                                jl_symbol("tvars"),
                                jl_symbol("min_world"),
                                jl_symbol("max_world"),
                                jl_symbol("ambig"),
                                jl_symbol("specializations"),
                                jl_symbol("sparam_syms"),
                                jl_symbol("source"),
                                jl_symbol("unspecialized"),
                                jl_symbol("generator"),
                                jl_symbol("roots"),
                                jl_symbol("invokes"),
                                jl_symbol("nargs"),
                                jl_symbol("called"),
                                jl_symbol("isva"),
                                jl_symbol("isstaged"),
                                jl_symbol("needs_sparam_vals_ducttape")),
                        jl_svec(21,
                                jl_sym_type,
                                jl_module_type,
                                jl_sym_type,
                                jl_int32_type,
                                jl_type_type,
                                jl_any_type, // Union{TypeVar, SimpleVector}
                                jl_long_type,
                                jl_long_type,
                                jl_any_type, // Union{Array, Void}
                                jl_any_type, // TypeMap
                                jl_simplevector_type,
                                jl_code_info_type,
                                jl_any_type, // jl_method_instance_type
                                jl_any_type, // jl_method_instance_type
                                jl_array_any_type,
                                jl_any_type,
                                jl_int32_type,
                                jl_int32_type,
                                jl_bool_type,
                                jl_bool_type,
                                jl_bool_type),
                        0, 1, 11);

    jl_method_instance_type =
        jl_new_datatype(jl_symbol("MethodInstance"),
                        jl_any_type, jl_emptysvec,
                        jl_svec(16,
                                jl_symbol("specTypes"),
                                jl_symbol("rettype"),
                                jl_symbol("sparam_vals"),
                                jl_symbol("backedges"),
                                jl_symbol("inferred"),
                                jl_symbol("inferred_const"),
                                jl_symbol("def"),
                                jl_symbol("min_world"),
                                jl_symbol("max_world"),
                                jl_symbol("inInference"),
                                jl_symbol("jlcall_api"),
                                jl_symbol(""),
                                jl_symbol("fptr"),
                                jl_symbol("unspecialized_ducttape"),
                                jl_symbol(""), jl_symbol("")),
                        jl_svec(16,
                                jl_any_type,
                                jl_any_type,
                                jl_simplevector_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_method_type,
                                jl_long_type,
                                jl_long_type,
                                jl_bool_type,
                                jl_uint8_type,
                                jl_bool_type,
                                jl_any_type, // void*
                                jl_any_type, // void*
                                jl_any_type, jl_any_type), // void*, void*
                        0, 1, 3);

    jl_typector_type =
        jl_new_datatype(jl_symbol("TypeConstructor"),
                        jl_type_type, jl_emptysvec,
                        jl_svec(2, jl_symbol("parameters"),
                                jl_symbol("body")),
                        jl_svec(2, jl_simplevector_type, jl_any_type),
                        0, 0, 2);

    // all kinds of types share a method table
    jl_typector_type->name->mt = jl_uniontype_type->name->mt = jl_datatype_type->name->mt =
        jl_type_type->name->mt;

    jl_intrinsic_type = jl_new_bitstype((jl_value_t*)jl_symbol("IntrinsicFunction"),
                                        jl_builtin_type, jl_emptysvec, 32);

    tv = jl_svec1(tvar("T"));
    jl_ref_type =
        jl_new_abstracttype((jl_value_t*)jl_symbol("Ref"), jl_any_type, tv);

    tv = jl_svec1(tvar("T"));
    jl_pointer_type =
        jl_new_bitstype((jl_value_t*)jl_symbol("Ptr"),
                        (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_ref_type, tv), tv,
                        sizeof(void*)*8);

    // Type{T}
    jl_typetype_tvar = jl_new_typevar(jl_symbol("T"),
                                      (jl_value_t*)jl_bottom_type,(jl_value_t*)jl_any_type);
    jl_typetype_type = (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_type_type,
                                                     jl_svec1(jl_typetype_tvar));

    jl_ANY_flag = (jl_value_t*)tvar("ANY");

    // complete builtin type metadata
    jl_value_t *pointer_void = jl_apply_type((jl_value_t*)jl_pointer_type,
                                             jl_svec1(jl_void_type));
    jl_voidpointer_type = (jl_datatype_t*)pointer_void;
    jl_svecset(jl_datatype_type->types, 5, jl_voidpointer_type);
    jl_svecset(jl_datatype_type->types, 6, jl_int32_type);
    jl_svecset(jl_datatype_type->types, 7, jl_int32_type);
    jl_svecset(jl_datatype_type->types, 8, jl_int32_type);
    jl_svecset(jl_datatype_type->types, 9, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 10, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 11, jl_voidpointer_type);
    jl_svecset(jl_datatype_type->types, 12, jl_voidpointer_type);
    jl_svecset(jl_datatype_type->types, 13, jl_int32_type);
    jl_svecset(jl_datatype_type->types, 14, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 15, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 16, jl_bool_type);
    jl_svecset(jl_tvar_type->types, 3, jl_bool_type);
    jl_svecset(jl_simplevector_type->types, 0, jl_long_type);
    jl_svecset(jl_typename_type->types, 1, jl_module_type);
    jl_svecset(jl_typename_type->types, 6, jl_long_type);
    jl_svecset(jl_methtable_type->types, 3, jl_long_type);
    jl_svecset(jl_methtable_type->types, 5, jl_module_type);
    jl_svecset(jl_methtable_type->types, 6, jl_array_any_type);
#ifdef __LP64__
    jl_svecset(jl_methtable_type->types, 7, jl_int64_type); // unsigned long
#else
    jl_svecset(jl_methtable_type->types, 7, jl_int32_type); // DWORD
#endif
    jl_svecset(jl_methtable_type->types, 8, jl_int32_type); // uint32_t
    jl_svecset(jl_method_type->types, 12, jl_method_instance_type);
    jl_svecset(jl_method_type->types, 13, jl_method_instance_type);
    jl_svecset(jl_method_instance_type->types, 12, jl_voidpointer_type);
    jl_svecset(jl_method_instance_type->types, 13, jl_voidpointer_type);
    jl_svecset(jl_method_instance_type->types, 14, jl_voidpointer_type);
    jl_svecset(jl_method_instance_type->types, 15, jl_voidpointer_type);

    jl_compute_field_offsets(jl_datatype_type);
    jl_compute_field_offsets(jl_typename_type);
    jl_compute_field_offsets(jl_uniontype_type);
    jl_compute_field_offsets(jl_tvar_type);
    jl_compute_field_offsets(jl_methtable_type);
    jl_compute_field_offsets(jl_expr_type);
    jl_compute_field_offsets(jl_linenumbernode_type);
    jl_compute_field_offsets(jl_labelnode_type);
    jl_compute_field_offsets(jl_gotonode_type);
    jl_compute_field_offsets(jl_quotenode_type);
    jl_compute_field_offsets(jl_module_type);
    jl_compute_field_offsets(jl_method_instance_type);
    jl_compute_field_offsets(jl_typector_type);
    jl_compute_field_offsets(jl_simplevector_type);
    jl_compute_field_offsets(jl_sym_type);

    // TODO: don't modify layout objects
    ((jl_datatype_layout_t*)jl_sym_type->layout)->pointerfree = 0;
    ((jl_datatype_layout_t*)jl_simplevector_type->layout)->pointerfree = 0;

    empty_sym = jl_symbol("");
    call_sym = jl_symbol("call");
    invoke_sym = jl_symbol("invoke");
    quote_sym = jl_symbol("quote");
    inert_sym = jl_symbol("inert");
    top_sym = jl_symbol("top");
    core_sym = jl_symbol("core");
    globalref_sym = jl_symbol("globalref");
    line_sym = jl_symbol("line");
    jl_incomplete_sym = jl_symbol("incomplete");
    error_sym = jl_symbol("error");
    goto_sym = jl_symbol("goto");
    goto_ifnot_sym = jl_symbol("gotoifnot");
    label_sym = jl_symbol("label");
    return_sym = jl_symbol("return");
    lambda_sym = jl_symbol("lambda");
    module_sym = jl_symbol("module");
    export_sym = jl_symbol("export");
    import_sym = jl_symbol("import");
    using_sym = jl_symbol("using");
    importall_sym = jl_symbol("importall");
    assign_sym = jl_symbol("=");
    body_sym = jl_symbol("body");
    colons_sym = jl_symbol("::");
    method_sym = jl_symbol("method");
    exc_sym = jl_symbol("the_exception");
    enter_sym = jl_symbol("enter");
    leave_sym = jl_symbol("leave");
    new_sym = jl_symbol("new");
    const_sym = jl_symbol("const");
    global_sym = jl_symbol("global");
    thunk_sym = jl_symbol("thunk");
    anonymous_sym = jl_symbol("anonymous");
    underscore_sym = jl_symbol("_");
    amp_sym = jl_symbol("&");
    abstracttype_sym = jl_symbol("abstract_type");
    bitstype_sym = jl_symbol("bits_type");
    compositetype_sym = jl_symbol("composite_type");
    toplevel_sym = jl_symbol("toplevel");
    dot_sym = jl_symbol(".");
    boundscheck_sym = jl_symbol("boundscheck");
    inbounds_sym = jl_symbol("inbounds");
    fastmath_sym = jl_symbol("fastmath");
    newvar_sym = jl_symbol("newvar");
    copyast_sym = jl_symbol("copyast");
    simdloop_sym = jl_symbol("simdloop");
    pure_sym = jl_symbol("pure");
    meta_sym = jl_symbol("meta");
    dots_sym = jl_symbol("...");
    list_sym = jl_symbol("list");
    unused_sym = jl_symbol("#unused#");
    slot_sym = jl_symbol("slot");
    static_parameter_sym = jl_symbol("static_parameter");
    compiler_temp_sym = jl_symbol("#temp#");
    polly_sym = jl_symbol("polly");
    inline_sym = jl_symbol("inline");
    propagate_inbounds_sym = jl_symbol("propagate_inbounds");

    tttvar = jl_new_typevar(jl_symbol("T"),
                                  (jl_value_t*)jl_bottom_type,
                                  (jl_value_t*)jl_anytuple_type);
    jl_anytuple_type_type = jl_wrap_Type((jl_value_t*)tttvar);
    jl_cfunction_list.unknown = jl_nothing;
}

#ifdef __cplusplus
}
#endif
