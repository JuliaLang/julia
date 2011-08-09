/*
  Types
  . type predicates (subtype) and type matching
  . type union and intersection
  . builtin type definitions
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
#include "libsupport.h"
#include "julia.h"
#include "newobj_internal.h"
#include "jltypes_internal.h"

jl_tag_type_t *jl_any_type;
jl_tag_type_t *jl_type_type;
jl_struct_type_t *jl_typename_type;
jl_struct_type_t *jl_sym_type;
jl_struct_type_t *jl_symbol_type;
jl_tuple_t *jl_tuple_type;
jl_tag_type_t *jl_ntuple_type;
jl_typename_t *jl_ntuple_typename;
jl_struct_type_t *jl_tvar_type;

jl_struct_type_t *jl_func_kind;
jl_struct_type_t *jl_union_kind;
jl_struct_type_t *jl_tag_kind;
jl_struct_type_t *jl_tag_type_type;
jl_struct_type_t *jl_struct_kind;
jl_struct_type_t *jl_bits_kind;

jl_type_t *jl_bottom_type;
jl_tag_type_t *jl_seq_type;
jl_tag_type_t *jl_abstractarray_type;

jl_bits_type_t *jl_bool_type;
jl_bits_type_t *jl_char_type;
jl_bits_type_t *jl_int8_type;
jl_bits_type_t *jl_uint8_type;
jl_bits_type_t *jl_int16_type;
jl_bits_type_t *jl_uint16_type;
jl_bits_type_t *jl_int32_type;
jl_bits_type_t *jl_uint32_type;
jl_bits_type_t *jl_int64_type;
jl_bits_type_t *jl_uint64_type;
jl_bits_type_t *jl_float32_type;
jl_bits_type_t *jl_float64_type;

jl_tuple_t *jl_null;
jl_value_t *jl_nothing;

jl_func_type_t *jl_any_func;
jl_function_t *jl_bottom_func;

void jl_add_constructors(jl_struct_type_t *t);

// --- type properties and predicates ---

int jl_is_type(jl_value_t *v)
{
    if (jl_is_tuple(v)) {
        jl_tuple_t *t = (jl_tuple_t*)v;
        size_t i;
        for(i=0; i < t->length; i++) {
            jl_value_t *vv = jl_tupleref(t, i);
            if (!jl_is_typevar(vv) && !jl_is_type(vv) &&
                !jl_is_typector(vv))
                return 0;
        }
        return 1;
    }
    return jl_is_nontuple_type(v);
}

int jl_has_typevars_(jl_value_t *v, int incl_wildcard);

DLLEXPORT int jl_is_leaf_type(jl_value_t *v)
{
    if (jl_is_tuple(v)) {
        jl_tuple_t *t = (jl_tuple_t*)v;
        size_t i;
        for(i=0; i < t->length; i++) {
            if (!jl_is_leaf_type(jl_tupleref(t, i)))
                return 0;
        }
        return 1;
    }
    if (!jl_is_struct_type(v) && !jl_is_func_type(v) && !jl_is_bits_type(v))
        return 0;
    return !jl_has_typevars_(v,1);
}

int jl_has_typevars_(jl_value_t *v, int incl_wildcard)
{
    size_t i;
    if (jl_typeis(v, jl_tvar_type)) {
        if (!((jl_tvar_t*)v)->bound)
            return incl_wildcard;
        return 1;
    }
    if (jl_is_func_type(v))
        return jl_has_typevars_((jl_value_t*)((jl_func_type_t*)v)->from,
                                incl_wildcard) ||
            jl_has_typevars_((jl_value_t*)((jl_func_type_t*)v)->to,
                             incl_wildcard);

    jl_tuple_t *t;
    if (jl_is_union_type(v))
        t = ((jl_uniontype_t*)v)->types;
    else if (jl_is_some_tag_type(v))
        t = ((jl_tag_type_t*)v)->parameters;
    else if (jl_is_tuple(v))
        t = (jl_tuple_t*)v;
    else
        t = jl_null;
    for(i=0; i < t->length; i++) {
        jl_value_t *elt = jl_tupleref(t, i);
        if (elt != v) {
            if (jl_has_typevars_(elt, incl_wildcard))
                return 1;
        }
    }
    // probably not necessary; no reason to use match() instead of subtype()
    // on the unconstrained version of a type
    //if (jl_is_typector(v))
    //    return (((jl_typector_t*)v)->parameters->length > 0);
    return 0;
}

int jl_has_typevars(jl_value_t *v)
{
    return jl_has_typevars_(v, 0);
}

// construct the full type of a value, possibly making a tuple type
jl_value_t *jl_full_type(jl_value_t *v)
{
    if (!jl_is_tuple(v))
        return (jl_value_t*)jl_typeof(v);
    jl_tuple_t *in = (jl_tuple_t*)v;
    jl_tuple_t *out = jl_alloc_tuple(in->length);
    JL_GC_PUSH(&out);
    size_t i;
    for(i=0; i < in->length; i++) {
        jl_tupleset(out, i, jl_full_type(jl_tupleref(in, i)));
    }
    JL_GC_POP();
    return (jl_value_t*)out;
}

typedef struct _jl_value_pair_t {
    jl_value_t *a;
    jl_value_t *b;
    struct _jl_value_pair_t *next;
} jl_value_pair_t;

static int type_eqv_(jl_value_t *a, jl_value_t *b, jl_value_pair_t *stack);

// --- union and intersection ---

static int count_union_components(jl_tuple_t *types)
{
    size_t i, c=0;
    for(i=0; i < types->length; i++) {
        jl_value_t *e = jl_tupleref(types,i);
        if (jl_is_union_type(e)) {
            c += count_union_components(((jl_uniontype_t*)e)->types);
        }
        else {
            c++;
        }
    }
    return c;
}

static void flatten_type_union(jl_tuple_t *types, jl_value_t **out, size_t *idx)
{
    size_t i;
    for(i=0; i < types->length; i++) {
        jl_value_t *e = jl_tupleref(types,i);
        if (jl_is_union_type(e)) {
            flatten_type_union(((jl_uniontype_t*)e)->types, out, idx);
        }
        else {
            out[*idx] = e;
            (*idx)++;
        }
    }
}

DLLEXPORT
jl_tuple_t *jl_compute_type_union(jl_tuple_t *types)
{
    size_t n = count_union_components(types);
    jl_value_t **temp = alloca(n * sizeof(jl_value_t*));
    size_t idx=0;
    flatten_type_union(types, temp, &idx);
    JL_GC_PUSHARGS(temp, n);
    assert(idx == n);
    size_t i, j, ndel=0;
    for(i=0; i < n; i++) {
        for(j=0; j < n; j++) {
            if (j != i && temp[i] && temp[j]) {
                if (temp[i] == temp[j] ||
                    type_eqv_(temp[i], temp[j], NULL) ||
                    (jl_subtype(temp[i], temp[j], 0) /*&&
                     (temp[i] == (jl_value_t*)jl_bottom_type ||
                     !jl_has_typevars(temp[j]))*/)) {
                    temp[i] = NULL;
                    ndel++;
                }
                else if (jl_is_typevar(temp[i]) && jl_is_typevar(temp[j])) {
                    jl_tvar_t *ti = (jl_tvar_t*)temp[i];
                    jl_tvar_t *tj = (jl_tvar_t*)temp[j];
                    if (jl_subtype(ti->lb,tj->lb,0) &&
                        jl_subtype(ti->lb,tj->ub,0) &&
                        jl_subtype(ti->ub,tj->ub,0)) {
                        temp[j] =
                            (jl_value_t*)
                            jl_new_typevar(tj->name, ti->lb, tj->ub);
                        temp[i] = NULL;
                    }
                }
            }
        }
    }
    jl_tuple_t *result = jl_alloc_tuple_uninit(n - ndel);
    j=0;
    for(i=0; i < n; i++) {
        if (temp[i] != NULL) {
            jl_tupleset(result, j, temp[i]);
            j++;
        }
    }
    assert(j == n-ndel);
    JL_GC_POP();
    return result;
}

jl_value_t *jl_type_union(jl_tuple_t *types)
{
    types = jl_compute_type_union(types);
    if (types->length == 1)
        return jl_tupleref(types, 0);
    if (types->length == 0)
        return (jl_value_t*)jl_bottom_type;
    JL_GC_PUSH(&types);
    jl_value_t *tu = (jl_value_t*)jl_new_uniontype(types);
    JL_GC_POP();
    return tu;
}

static jl_value_t *unsafe_type_union(jl_tuple_t *types)
{
    types = jl_compute_type_union(types);
    if (types->length == 1)
        return jl_tupleref(types, 0);
    if (types->length == 0)
        return (jl_value_t*)jl_bottom_type;
    JL_GC_PUSH(&types);
    jl_uniontype_t *t = (jl_uniontype_t*)newobj((jl_type_t*)jl_union_kind, 1);
    t->types = types;
    JL_GC_POP();
    return (jl_value_t*)t;
}

typedef enum {invariant, covariant} variance_t;

static jl_value_t *jl_type_intersect(jl_value_t *a, jl_value_t *b,
                                     jl_tuple_t **penv, jl_tuple_t **eqc,
                                     variance_t var);

static jl_value_t *intersect_union(jl_uniontype_t *a, jl_value_t *b,
                                   jl_tuple_t **penv, jl_tuple_t **eqc,
                                   variance_t var)
{
    jl_tuple_t *t = jl_alloc_tuple(a->types->length);
    JL_GC_PUSH(&t);
    size_t i;
    for(i=0; i < t->length; i++) {
        jl_tupleset(t, i, jl_type_intersection(jl_tupleref(a->types,i),b));
    }
    // problem: an intermediate union type we make here might be too
    // complex, even though the final type after typevars are replaced
    // might be ok.
    jl_value_t *tu = unsafe_type_union(t);
    JL_GC_POP();
    return tu;
}

// if returns with *bot!=0, then intersection is None
static size_t tuple_intersect_size(jl_tuple_t *a, jl_tuple_t *b, int *bot)
{
    size_t al = a->length;
    size_t bl = b->length;
    *bot = 0;
    if (al == bl) return al;
    if (al > bl) return tuple_intersect_size(b, a, bot);
    assert(al < bl);
    if (jl_is_seq_type(jl_tupleref(b,bl-1))) {
        if (al > 0 && jl_is_seq_type(jl_tupleref(a,al-1))) {
            return bl;
        }
        else {
            if (bl == al+1)
                return al;
            *bot=1;
            return 0;
        }
    }
    if (al > 0 && jl_is_seq_type(jl_tupleref(a,al-1))) {
        return bl;
    }
    *bot=1;
    return 0;
}

static jl_value_t *intersect_tuple(jl_tuple_t *a, jl_tuple_t *b,
                                   jl_tuple_t **penv, jl_tuple_t **eqc,
                                   variance_t var)
{
    size_t al = a->length;
    size_t bl = b->length;
    int bot=0;
    size_t n = tuple_intersect_size(a, b, &bot);
    if (bot)
        return (jl_value_t*)jl_bottom_type;
    if (n == 0) return (jl_value_t*)jl_null;
    jl_tuple_t *tc = jl_alloc_tuple(n);
    jl_value_t *result = (jl_value_t*)tc;
    jl_value_t *ce = NULL;
    JL_GC_PUSH(&tc, &ce);
    size_t ai=0, bi=0, ci;
    jl_value_t *ae=NULL, *be=NULL;
    int aseq=0, bseq=0;
    for(ci=0; ci < n; ci++) {
        if (ai < al) {
            ae = jl_tupleref(a,ai);
            if (jl_is_seq_type(ae)) {
                aseq=1;
                ae = jl_tparam0(ae);
            }
            ai++;
        }
        if (bi < bl) {
            be = jl_tupleref(b,bi);
            if (jl_is_seq_type(be)) {
                bseq=1;
                be = jl_tparam0(be);
            }
            bi++;
        }
        assert(ae!=NULL && be!=NULL);
        ce = jl_type_intersect(ae,be,penv,eqc,var);
        if (ce == (jl_value_t*)jl_bottom_type) {
            if (aseq && bseq) {
                // (X∩Y)==∅ → (X...)∩(Y...) == ()
                if (n == 1) {
                    result = (jl_value_t*)jl_null;
                    goto done_intersect_tuple;
                }
                tc->length--;
                goto done_intersect_tuple;
            }
            result = (jl_value_t*)jl_bottom_type;
            goto done_intersect_tuple;
        }
        if (aseq && bseq) {
            ce = (jl_value_t*)jl_tuple1(ce);
            ce = (jl_value_t*)jl_apply_type((jl_value_t*)jl_seq_type,
                                            (jl_tuple_t*)ce);
        }
        jl_tupleset(tc, ci, ce);
    }
 done_intersect_tuple:
    JL_GC_POP();
    return result;
}

static jl_value_t *intersect_tag(jl_tag_type_t *a, jl_tag_type_t *b,
                                 jl_tuple_t **penv, jl_tuple_t **eqc,
                                 variance_t var)
{
    assert(a->name == b->name);
    assert(a->parameters->length == b->parameters->length);
    jl_tuple_t *p = jl_alloc_tuple(a->parameters->length);
    JL_GC_PUSH(&p);
    jl_value_t *ti;
    size_t i;
    if (a->name == jl_ntuple_typename) {
        assert(p->length == 2);
        // NOTE: tuples are covariant, so NTuple element type is too
        ti = jl_type_intersect(jl_tparam0(a),jl_tparam0(b),penv,eqc,invariant);
        jl_tupleset(p, 0, ti);
        ti = jl_type_intersect(jl_tparam1(a),jl_tparam1(b),penv,eqc,var);
        if (ti==(jl_value_t*)jl_bottom_type ||
            jl_t0(p)==(jl_value_t*)jl_bottom_type) {
            JL_GC_POP();
            return (jl_value_t*)jl_bottom_type;
        }
        jl_tupleset(p, 1, ti);
    }
    else {
        for(i=0; i < p->length; i++) {
            jl_value_t *ap = jl_tupleref(a->parameters,i);
            jl_value_t *bp = jl_tupleref(b->parameters,i);
            if (jl_is_typevar(ap)) {
                ti = jl_type_intersect(ap,bp,penv,eqc,invariant);
                if (bp == (jl_value_t*)jl_bottom_type &&
                    !((jl_tvar_t*)ap)->bound) {
                    // "None" as a type parameter
                    jl_tupleset(p, i, ti);
                    continue;
                }
            }
            else if (jl_is_typevar(bp)) {
                ti = jl_type_intersect(ap,bp,penv,eqc,invariant);
                if (ap == (jl_value_t*)jl_bottom_type &&
                    !((jl_tvar_t*)bp)->bound) {
                    // "None" as a type parameter
                    jl_tupleset(p, i, ti);
                    continue;
                }
            }
            else if (jl_has_typevars_(ap,1)) {
                // if the types have parameters, see if there exists a
                // parameter assignment that makes them the same type
                jl_value_t *tm = jl_type_match_invariant(bp, ap);
                if (tm != jl_false && tm != (jl_value_t*)jl_null) {
                    ti = jl_type_intersect(ap,bp,penv,eqc,invariant);
                }
                else if (jl_has_typevars_(bp,1)) {
                    tm = jl_type_match_invariant(ap, bp);
                    if (tm != jl_false && tm != (jl_value_t*)jl_null) {
                        ti = jl_type_intersect(ap,bp,penv,eqc,invariant);
                    }
                    else {
                        ti = (jl_value_t*)jl_bottom_type;
                    }
                }
                else {
                    ti = (jl_value_t*)jl_bottom_type;
                }
            }
            else if (jl_has_typevars_(bp,1)) {
                jl_value_t *tm = jl_type_match_invariant(ap, bp);
                if (tm != jl_false && tm != (jl_value_t*)jl_null) {
                    ti = jl_type_intersect(ap,bp,penv,eqc,invariant);
                }
                else {
                    ti = (jl_value_t*)jl_bottom_type;
                }
            }
            else if (type_eqv_(ap,bp,NULL)) {
                ti = ap;
                if (ti == (jl_value_t*)jl_bottom_type) {
                    // "None" as a type parameter
                    jl_tupleset(p, i, ti);
                    continue;
                }
            }
            else {
                ti = (jl_value_t*)jl_bottom_type;
            }
            if (ti == (jl_value_t*)jl_bottom_type) {
                JL_GC_POP();
                return (jl_value_t*)jl_bottom_type;
            }
            jl_tupleset(p, i, ti);
        }
    }
    if (a->name->primary != NULL) {
        jl_value_t *res = (jl_value_t*)jl_apply_type(a->name->primary, p);
        JL_GC_POP();
        return res;
    }
    assert(0 && "not yet implemented");
    return NULL;
}

// use, essentially, union-find on type variables to group them
// into equivalence classes.
static jl_value_t *intersect_typevar(jl_tvar_t *a, jl_value_t *b,
                                     jl_tuple_t **penv, jl_tuple_t **eqc,
                                     variance_t var)
{
    if (jl_subtype(b, (jl_value_t*)a, 0)) {
        if (!a->bound) return b;
    }
    else if (var==invariant && !jl_is_typevar(b)) {
        // for typevar a and non-typevar type b, b must be within a's bounds
        // in invariant contexts.
        return (jl_value_t*)jl_bottom_type;
    }
    else if (jl_subtype((jl_value_t*)a, b, 0)) {
        if (!a->bound) return (jl_value_t*)a;
    }
    else {
        return (jl_value_t*)jl_bottom_type;
    }
    jl_tuple_t *p;
    if (var == invariant && !jl_has_typevars_(b,1)) {
        p = *eqc;
        while (p != jl_null) {
            if (jl_t0(p) == (jl_value_t*)a) {
                jl_value_t *v = jl_t1(p);
                if (jl_is_long(b)) {
                    if (jl_is_long(v)) {
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
                        */
                        long bv = jl_unbox_long(b);
                        long vv = jl_unbox_long(v);
                        if (bv < 0) {
                            if (vv < 0) {
                                if (bv < vv) {
                                    jl_t1(p) = b;
                                }
                            }
                            else {
                                if (~bv > vv)
                                    return (jl_value_t*)jl_bottom_type;
                            }
                        }
                        else {
                            if (vv < 0) {
                                if (~vv > bv)
                                    return (jl_value_t*)jl_bottom_type;
                                jl_t1(p) = b;
                            }
                            else {
                                if (bv != vv)
                                    return (jl_value_t*)jl_bottom_type;
                            }
                        }
                        break;
                    }
                    else {
                        return (jl_value_t*)jl_bottom_type;
                    }
                }
                if (!jl_types_equal(v, b))
                    return (jl_value_t*)jl_bottom_type;
                break;
            }
            p = (jl_tuple_t*)jl_nextpair(p);
        }
        if (p == jl_null) {
            *eqc = jl_tuple3((jl_value_t*)a, b, (jl_value_t*)*eqc);
        }
        return (jl_value_t*)a;
    }
    if ((jl_value_t*)a != b) {
        *penv = jl_tuple3((jl_value_t*)a, b, (jl_value_t*)*penv);
    }
    return (jl_value_t*)a;
}

static jl_value_t *jl_type_intersect(jl_value_t *a, jl_value_t *b,
                                     jl_tuple_t **penv, jl_tuple_t **eqc,
                                     variance_t var)
{
    if (jl_is_typector(a))
        a = (jl_value_t*)((jl_typector_t*)a)->body;
    if (jl_is_typector(b))
        b = (jl_value_t*)((jl_typector_t*)b)->body;
    if (a == b) return a;
    if (jl_is_typevar(a) && a != jl_ANY_flag)
        return intersect_typevar((jl_tvar_t*)a, b, penv, eqc, var);
    if (jl_is_typevar(b) && b != jl_ANY_flag)
        return intersect_typevar((jl_tvar_t*)b, a, penv, eqc, var);
    if (a == (jl_value_t*)jl_bottom_type || b == (jl_value_t*)jl_bottom_type)
        return (jl_value_t*)jl_bottom_type;
    if (!jl_has_typevars(a) && !jl_has_typevars(b)) {
        if (jl_subtype(a, b, 0))
            return a;
        if (jl_subtype(b, a, 0))
            return b;
    }
    // union
    if (jl_is_union_type(a))
        return intersect_union((jl_uniontype_t*)a, b, penv, eqc, var);
    if (jl_is_union_type(b))
        return intersect_union((jl_uniontype_t*)b, a, penv, eqc, var);
    if (a == (jl_value_t*)jl_undef_type) return (jl_value_t*)jl_bottom_type;
    if (b == (jl_value_t*)jl_undef_type) return (jl_value_t*)jl_bottom_type;
    if (a == (jl_value_t*)jl_any_type ||
        a == jl_ANY_flag) return b;
    if (b == (jl_value_t*)jl_any_type ||
        b == jl_ANY_flag) return a;
    // tuple
    if (jl_is_tuple(a)) {
        jl_value_t *temp=NULL;
        JL_GC_PUSH(&b, &temp);
        if (jl_is_ntuple_type(b)) {
            long alen = (long)((jl_tuple_t*)a)->length;
            jl_value_t *lenvar = jl_tparam0(b);
            jl_value_t *elty = jl_tparam1(b);
            jl_tuple_t *pe = *eqc;
            while (pe != jl_null) {
                if (jl_t0(pe) == lenvar) {
                    jl_value_t *v = jl_t1(pe);
                    if (jl_is_long(v) && jl_unbox_long(v)>=0) {
                        // N is already known in NTuple{N,...}
                        alen = jl_unbox_long(v);
                        break;
                    }
                }
                pe = (jl_tuple_t*)jl_t2(pe);
            }
            b = (jl_value_t*)jl_tuple_fill(alen, elty);
            if (pe == jl_null) {
                // don't know N yet, so add a constraint for it based on
                // the length of the other tuple
                if (alen > 0 && jl_is_seq_type(jl_tupleref(a,alen-1))) {
                    temp = (jl_value_t*)jl_tuple1(elty);
                    jl_tupleset(b, alen-1, jl_apply_type((jl_value_t*)jl_seq_type,
                                                         (jl_tuple_t*)temp));
                    // if the value of an NTuple typevar, N, is negative,
                    // it means the tuple needs to be at least ~N long.
                    alen = ~(alen-1);
                }
                if (jl_is_typevar(lenvar)) {
                    temp = jl_box_long(alen);
                    if (intersect_typevar((jl_tvar_t*)lenvar,temp,penv,eqc,
                                          invariant) ==
                        (jl_value_t*)jl_bottom_type) {
                        JL_GC_POP();
                        return (jl_value_t*)jl_bottom_type;
                    }
                }
            }
        }
        if (!jl_is_tuple(b)) {
            JL_GC_POP();
            return (jl_value_t*)jl_bottom_type;
        }
        a = intersect_tuple((jl_tuple_t*)a, (jl_tuple_t*)b, penv,eqc,var);
        JL_GC_POP();
        return a;
    }
    if (jl_is_tuple(b)) {
        return jl_type_intersect(b, a, penv,eqc,var);
    }
    // function
    if (jl_is_func_type(a)) {
        if (!jl_is_func_type(b))
            return (jl_value_t*)jl_bottom_type;
        jl_value_t *t1=NULL, *t2=NULL;
        JL_GC_PUSH(&t1, &t2);
        t1 = (jl_value_t*)jl_tuple2(((jl_func_type_t*)a)->from,
                                    ((jl_func_type_t*)b)->from);
        t1 = jl_type_union((jl_tuple_t*)t1);
        t2 = jl_type_intersect((jl_value_t*)((jl_func_type_t*)a)->to,
                               (jl_value_t*)((jl_func_type_t*)b)->to, penv,
                               eqc, var);
        jl_value_t *result =
            (jl_value_t*)jl_new_functype((jl_type_t*)t1, (jl_type_t*)t2);
        JL_GC_POP();
        return result;
    }
    if (jl_is_func_type(b))
        return (jl_value_t*)jl_bottom_type;
    if (jl_is_long(a) || jl_is_long(b))
        return (jl_value_t*)jl_bottom_type;
    // tag
    assert(jl_is_some_tag_type(a));
    assert(jl_is_some_tag_type(b));
    jl_tag_type_t *tta = (jl_tag_type_t*)a;
    jl_tag_type_t *ttb = (jl_tag_type_t*)b;
    if (tta->name == ttb->name)
        return (jl_value_t*)intersect_tag(tta, ttb, penv, eqc, var);
    jl_tag_type_t *super = NULL;
    jl_tag_type_t *sub = NULL;
    jl_value_t *ti = NULL;
    jl_value_t *env = NULL;
    jl_tuple_t *p = NULL;
    JL_GC_PUSH(&super, &sub, &env, &p);
    while (tta != jl_any_type) {
        if (tta->name == ttb->name) {
            ti = intersect_tag(tta, ttb, penv, eqc, var);
            if (ti == (jl_value_t*)jl_bottom_type) {
                JL_GC_POP();
                return ti;
            }
            super = (jl_tag_type_t*)ti;
            sub = (jl_tag_type_t*)a;
            break;
        }
        tta = tta->super;
    }
    if (super == NULL) {
        tta = (jl_tag_type_t*)a;
        while (ttb != jl_any_type) {
            if (tta->name == ttb->name) {
                ti = intersect_tag(tta, ttb, penv, eqc, var);
                if (ti == (jl_value_t*)jl_bottom_type) {
                    JL_GC_POP();
                    return ti;
                }
                super = (jl_tag_type_t*)ti;
                sub = (jl_tag_type_t*)b;
                break;
            }
            ttb = ttb->super;
        }
    }
    if (super == NULL) {
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }

    size_t n = sub->parameters->length;
    if (n == 0) {
        JL_GC_POP();
        return (jl_value_t*)sub;
    }

    assert(sub->name->primary != NULL);
    jl_value_t *tc = sub->name->primary;
    // compute what constraints the supertype imposes on the subtype
    jl_tuple_t *sup_params =
        ((jl_tag_type_t*)((jl_tag_type_t*)tc)->super)->parameters;
    // match the intersected supertype against the pattern this subtype
    // uses to instantiate its supertype. this tells us what subtype parameter
    // values are implied by the intersected supertype, or that the
    // intersected supertype cannot come from this subtype (in which case
    // our final answer is None).
    size_t i;
    // hack: we need type_match to find assignments for these
    // typevars even though they normally aren't constrained. temporarily
    // switch them.
    jl_tuple_t *tc_params = ((jl_tag_type_t*)tc)->parameters;
    for(i=0; i < tc_params->length; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_tupleref(tc_params,i);
        assert(jl_is_typevar(tv));
        assert(!tv->bound);
        tv->bound = 1;
    }
    env = jl_type_match((jl_value_t*)super->parameters,
                        (jl_value_t*)sup_params);
    if (env == jl_false) {
        env = jl_type_match((jl_value_t*)sup_params,
                            (jl_value_t*)super->parameters);
    }
    for(i=0; i < tc_params->length; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_tupleref(tc_params,i);
        tv->bound = 0;
    }
    if (env == jl_false) {
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }

    p = jl_alloc_tuple(n);
    for(i=0; i < n; i++) {
        jl_value_t *tp = jl_tupleref(((jl_tag_type_t*)tc)->parameters, i);
        jl_value_t *elt = jl_tupleref(sub->parameters, i);
        jl_value_t *e = env;
        while (e != (jl_value_t*)jl_null) {
            if (jl_t0(e) == tp) {
                elt = jl_type_intersect(elt, jl_t1(e), penv, eqc, var);
                // note: elt might be None if "None" was the type parameter
                break;
            }
            e = jl_nextpair(e);
        }
        jl_tupleset(p, i, elt);
    }
    jl_value_t *result = (jl_value_t*)jl_apply_type(tc, p);
    JL_GC_POP();
    return result;
}

jl_value_t *jl_type_intersection(jl_value_t *a, jl_value_t *b)
{
    jl_tuple_t *env = jl_null;
    JL_GC_PUSH(&env);
    jl_value_t *ti = jl_type_intersection_matching(a, b, &env, jl_null);
    JL_GC_POP();
    return ti;
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

static inline int is_btv(jl_value_t *v)
{
    return jl_is_typevar(v) && ((jl_tvar_t*)v)->bound;
}

static jl_value_t **tvar_lookup(jl_tuple_t *env, jl_value_t **pX)
{
    jl_tuple_t *p = env;
    jl_value_t *v = *pX;
    if (is_btv(v)) {
        while (p != jl_null) {
            if (jl_t0(p) == v) {
                if (jl_t1(p) == v)  // allow T=T
                    return pX;
                return tvar_lookup(env, &jl_t1(p));
            }
            p = (jl_tuple_t*)jl_nextpair(p);
        }
    }
    return pX;
}

static jl_value_t *meet_tvars(jl_tvar_t *a, jl_tvar_t *b)
{
    jl_value_t *lb=NULL, *ub=NULL;
    if (type_eqv_((jl_value_t*)a->lb, (jl_value_t*)b->lb, NULL) &&
        type_eqv_((jl_value_t*)a->ub, (jl_value_t*)b->ub, NULL))
        return (jl_value_t*)b;
    ub = jl_type_intersection((jl_value_t*)a->ub, (jl_value_t*)b->ub);
    if (ub == (jl_value_t*)jl_bottom_type)
        return ub;
    JL_GC_PUSH(&lb, &ub);
    lb = (jl_value_t*)jl_tuple2(a->lb, b->lb);
    lb = jl_type_union((jl_tuple_t*)lb);
    if (!jl_subtype(lb, ub, 0)) {
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }
    // TODO: might not want to collapse tvar to non-tvar in all cases
    if (jl_is_leaf_type(ub)) {
        JL_GC_POP();
        return ub;
    }
    jl_value_t *res = (jl_value_t*)jl_new_typevar(jl_symbol("_"), lb, ub);
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
        if (jl_is_leaf_type(ty) || jl_is_long(ty))
            return ty;
        return (jl_value_t*)jl_new_typevar(jl_symbol("_"), tv->lb, ty);
    }
    return (jl_value_t*)jl_bottom_type;
}

static jl_value_t *meet(jl_value_t *X, jl_value_t *Y)
{
    if (!jl_has_typevars_(X,1)) {
        if (!jl_has_typevars_(Y,1)) {
            if (!jl_types_equal(X,Y))
                return NULL;
            return X;
        }
        if (jl_subtype(X,Y,0))
            return X;
        return NULL;
    }
    if (!jl_has_typevars_(Y,1)) {
        if (jl_subtype(Y,X,0))
            return Y;
        return NULL;
    }
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
    return jl_type_intersection(X, Y);
}

static jl_tuple_t *extend(jl_value_t *var, jl_value_t *val, jl_tuple_t *soln)
{
    if (var == val)
        return soln;
    /*
        ios_printf(ios_stdout,
                   "adding %s@%x = %s@%x\n",
                   ((jl_tvar_t*)var)->name->name, var,
                   ((jl_tvar_t*)val)->name->name, val);
    */
    return jl_tuple3(var, val, soln);
}

static int solve_tvar_constraints(jl_tuple_t *env, jl_tuple_t **soln)
{
    //ios_printf(ios_stdout, "\n");
    jl_tuple_t *p = env;
    jl_value_t *v=NULL;
    JL_GC_PUSH(&v);
    while (p != jl_null) {
        jl_value_t *T = jl_t0(p);
        jl_value_t *S = jl_t1(p);
        if (S < T && is_btv(S)) {
            jl_value_t *temp = S;
            S = T;
            T = temp;
        }
        jl_value_t **pT;
        pT = tvar_lookup(*soln, &T);
        if (pT != &T) {
            // T=U is in the results
            jl_value_t **pU = pT;
            //jl_value_t *U = *pU;
            if (is_btv(S)) {
                // S is a typevar
                jl_value_t **pS;
                pS = tvar_lookup(*soln, &S);
                if (pS != &S) {
                    // S=R is in the results
                    jl_value_t **pR = pS;
                    *pR = meet(*pR, *pU);
                    if (*pR == NULL) {
                        JL_GC_POP();
                        return 0;
                    }
                }
                else {
                    v = meet(S, *pU);
                    if (v == NULL) {
                        JL_GC_POP();
                        return 0;
                    }
                    *soln = extend(S, v, *soln);
                }
                if (pS != pU)
                    *pU = S;
            }
            else {
                v = meet(*pU, S);
                if (v == NULL) {
                    JL_GC_POP();
                    return 0;
                }
                if (is_btv(*pU)) {
                    *soln = extend(*pU, v, *soln);
                }
                else {
                    *pU = v;
                }
            }
        }
        else {
            if (jl_has_typevars_(S,1)) {
                if (*tvar_lookup(*soln, &S) != T)
                    *soln = extend(T, S, *soln);
            }
            else {
                if (jl_is_leaf_type(S) || jl_is_long(S) ||
                    S == (jl_value_t*)jl_bottom_type) {
                    v = S;
                }
                else {
                    v = (jl_value_t*)
                        jl_new_typevar(jl_symbol("_"),
                                       (jl_value_t*)jl_bottom_type, S);
                    ((jl_tvar_t*)v)->bound = 0;
                }
                *soln = extend(T, v, *soln);
            }
        }
        p = (jl_tuple_t*)jl_nextpair(p);
    }
    JL_GC_POP();
    return 1;
}

#if 0
static char *type_summary(jl_value_t *t)
{
    if (jl_is_tuple(t)) return "Tuple";
    if (jl_is_func_type(t)) return "Function";
    if (jl_is_some_tag_type(t))
        return ((jl_tag_type_t*)t)->name->name->name;
    return "?";
}

static void print_env(jl_tuple_t *soln)
{
    jl_tuple_t *p = soln;
    while (p != jl_null) {
        jl_value_t *T, *S;
        T = jl_t0(p); S = jl_t1(p);
        ios_printf(ios_stdout,
                   "%s@%x=%s ",
                   ((jl_tvar_t*)T)->name->name, T,
                   type_summary(S));
        p = (jl_tuple_t*)jl_nextpair(p);
    }
    ios_printf(ios_stdout, "\n");
}
#endif

jl_value_t *jl_type_intersection_matching(jl_value_t *a, jl_value_t *b,
                                          jl_tuple_t **penv, jl_tuple_t *tvars)
{
    jl_tuple_t *eqc = jl_null;
    jl_tuple_t *t=NULL;
    jl_value_t *ti=NULL;
    JL_GC_PUSH(&ti, &t, &eqc);
    ti = jl_type_intersect(a, b, penv, &eqc, covariant);
    if (ti == (jl_value_t*)jl_bottom_type) {
        JL_GC_POP();
        return ti;
    }
    if (*penv != jl_null || eqc != jl_null || tvars != jl_null) {
        jl_tuple_t *pe = eqc;
        while (pe != jl_null) {
            jl_value_t *val = jl_t1(pe);
            if (jl_is_long(val) && jl_unbox_long(val)>=0) {
                break;
            }
            pe = (jl_tuple_t*)jl_t2(pe);
        }
        if (pe != jl_null) {
            /*
              if there are integer-valued parameters, repeat intersection
              with the full environment visible. this is needed because
              NTuple has only one element type, so we can't keep track of
              the fact that an arbitrary tuple's length must match some
              typevar, e.g. "(Int8,Int32...) of length N". the solution is
              to find all other constraints on N first, then do intersection
              again with that knowledge.
            */
            ti = jl_type_intersect(a, b, penv, &eqc, covariant);
            if (ti == (jl_value_t*)jl_bottom_type) {
                JL_GC_POP();
                return ti;
            }
        }

        if (!solve_tvar_constraints(*penv, &eqc)) {
            JL_GC_POP();
            return (jl_value_t*)jl_bottom_type;
        }
        //ios_printf(ios_stdout, "env: "); print_env(*penv);
        //ios_printf(ios_stdout, "sol: "); print_env(eqc);
        
        // convert non-specific integer vars to typevars
        pe = eqc;
        while (pe != jl_null) {
            jl_value_t *val = jl_t1(pe);
            if (jl_is_long(val) && jl_unbox_long(val)<0) {
                jl_t1(pe) = jl_t0(pe);
            }
            pe = (jl_tuple_t*)jl_t2(pe);
        }

        t = tvars;
        jl_tuple_t *env0 = eqc;
        while (t != jl_null) {
            jl_value_t *tv = jl_t0(t);
            int found = 0;
            jl_tuple_t *pe = env0;
            while (pe != jl_null) {
                if (jl_t0(pe) == tv) {
                    found = 1;
                    break;
                }
                pe = (jl_tuple_t*)jl_t2(pe);
            }
            // bind type vars to themselves if they were not matched explicitly
            // during type intersection.
            if (!found)
                eqc = jl_tuple3(tv, tv, eqc);
            t = (jl_tuple_t*)jl_t1(t);
        }

        t = jl_flatten_pairs(eqc);
        *penv = t;

        if (env0 != jl_null) {
            int i;
            for(i=0; i < t->length; i+=2) {
                jl_tupleset(t, i+1, *tvar_lookup(eqc, &jl_tupleref(t,i+1)));
            }
            ti = (jl_value_t*)
                jl_instantiate_type_with((jl_type_t*)ti,
                                         &jl_tupleref(t, 0), t->length/2);
        }
    }
    JL_GC_POP();
    return ti;
}

// --- type instantiation and cache ---

static int extensionally_same_type(jl_value_t *a, jl_value_t *b)
{
    return (jl_subtype(a, b, 0) && jl_subtype(b, a, 0));
}

static int type_eqv_(jl_value_t *a, jl_value_t *b, jl_value_pair_t *stack)
{
    jl_value_pair_t top;
    if (a == b) return 1;
    if (jl_is_typector(a)) a = (jl_value_t*)((jl_typector_t*)a)->body;
    if (jl_is_typector(b)) b = (jl_value_t*)((jl_typector_t*)b)->body;
    if (jl_is_typevar(a)) return 0;
    if (jl_is_long(a)) {
        if (jl_is_long(b))
            return (jl_unbox_long(a) == jl_unbox_long(b));
        return 0;
    }
    if (jl_is_tuple(a)) {
        if (jl_is_tuple(b)) {
            return extensionally_same_type(a, b);
        }
        return 0;
    }
    if (jl_is_union_type(a)) {
        if (jl_is_union_type(b)) {
            return extensionally_same_type(a, b);
        }
        return 0;
    }
    jl_value_pair_t *p = stack;
    while (p != NULL) {
        if (p->a == a && p->b == b)
            return 1;
        p = p->next;
    }
    top.a = a;
    top.b = b;
    top.next = stack;
    stack = &top;
    if (jl_is_func_type(a)) {
        if (jl_is_func_type(b)) {
            return (type_eqv_((jl_value_t*)((jl_func_type_t*)a)->from,
                              (jl_value_t*)((jl_func_type_t*)b)->from, stack) &&
                    type_eqv_((jl_value_t*)((jl_func_type_t*)a)->to,
                              (jl_value_t*)((jl_func_type_t*)b)->to, stack));
        }
        return 0;
    }
    assert(jl_is_some_tag_type(a));
    if (!jl_is_some_tag_type(b)) return 0;
    jl_tag_type_t *tta = (jl_tag_type_t*)a;
    jl_tag_type_t *ttb = (jl_tag_type_t*)b;
    if (tta->name != ttb->name) return 0;
    jl_tuple_t *ap = tta->parameters;
    jl_tuple_t *bp = ttb->parameters;
    assert(ap->length == bp->length);
    size_t i;
    for(i=0; i < ap->length; i++) {
        jl_value_t *api = jl_tupleref(ap,i);
        jl_value_t *bpi = jl_tupleref(bp,i);
        if (api == bpi) continue;
        if (!type_eqv_(api, bpi, stack))
            return 0;
    }
    return 1;
}

int jl_types_equal(jl_value_t *a, jl_value_t *b)
{
    return type_eqv_(a, b, NULL);
}

static int type_le_generic(jl_value_t *a, jl_value_t *b)
{
    jl_value_t *env = jl_type_match(a, b);
    if (env == jl_false) return 0;
    jl_value_t *vp = env;
    jl_value_t *x;
    // make sure all typevars correspond to other unique typevars
    while (vp != (jl_value_t*)jl_null) {
        if (!jl_is_typevar(jl_t1(vp)))
            return 0;
        x = env;
        while (x != (jl_value_t*)jl_null) {
            if (x != vp) {
                if (jl_t1(x) == jl_t1(vp))
                    return 0;
            }
            x = jl_nextpair(x);
        }
        vp = jl_nextpair(vp);
    }
    return 1;
}

int jl_types_equal_generic(jl_value_t *a, jl_value_t *b)
{
    return type_le_generic(a, b) && type_le_generic(b, a);
}

static jl_value_t *apply_type_(jl_value_t *tc, jl_value_t **params, size_t n)
{
    if (n == 0) {
        if (jl_is_typector(tc))
            return (jl_value_t*)((jl_typector_t*)tc)->body;
        return tc;
    }
    size_t i;
    char *tname;
    jl_tuple_t *tp;
    if (jl_is_typector(tc)) {
        tp = ((jl_typector_t*)tc)->parameters;
        tname = "alias";
    }
    else {
        assert(jl_is_some_tag_type(tc));
        tp = ((jl_tag_type_t*)tc)->parameters;
        tname = ((jl_tag_type_t*)tc)->name->name->name;
    }
    for(i=0; i < n; i++) {
        jl_value_t *pi = params[i];
        if (!jl_is_typector(pi) && !jl_is_type(pi) && !jl_is_long(pi) &&
            !jl_is_typevar(pi)) {
            jl_type_error_rt("apply_type", tname,
                             (jl_value_t*)jl_type_type, pi);
        }
    }
    if (tc == (jl_value_t*)jl_ntuple_type && (n==1||n==2) &&
        jl_is_long(params[0])) {
        size_t nt = jl_unbox_long(params[0]);
        return (jl_value_t*)jl_tuple_fill(nt, (n==2) ? params[1] :
                                          (jl_value_t*)jl_any_type);
    }
    if (n > tp->length)
        jl_errorf("too many parameters for type %s", tname);
    jl_value_t **env = alloca(2 * tp->length * sizeof(jl_value_t*));
    for(i=0; i < tp->length; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_tupleref(tp,i);
        if (!jl_is_typevar(tv))
            continue;
        env[i*2+0] = (jl_value_t*)tv;
        if (i >= n) {
            env[i*2+1] = (jl_value_t*)tv;
        }
        else {
            if (!jl_is_typevar(params[i]) &&
                // TODO: Undef should not be special here; fix.
                // maybe introduce Top == Union(Any,Undef), and make this
                // the default upper bound.
                params[i] != (jl_value_t*)jl_undef_type &&
                !jl_subtype(params[i], (jl_value_t*)tv, 0)) {
                jl_type_error_rt(tname, tv->name->name,
                                 tv->ub, params[i]);
            }
            if (jl_is_typector(params[i]))
                env[i*2+1] = (jl_value_t*)((jl_typector_t*)params[i])->body;
            else
                env[i*2+1] = params[i];
        }
    }
    if (jl_is_typector(tc)) tc = (jl_value_t*)((jl_typector_t*)tc)->body;
    return (jl_value_t*)jl_instantiate_type_with((jl_type_t*)tc, env, i);
}

jl_value_t *jl_apply_type(jl_value_t *tc, jl_tuple_t *params)
{
    return apply_type_(tc, &jl_tupleref(params,0), params->length);
}

static jl_type_t *lookup_type(typekey_stack_t *table,
                              jl_typename_t *tn, jl_value_t **key, size_t n)
{
    if (n==0) return NULL;
    while (table != NULL) {
        assert(table->n > 0);
        if (table->n == n && table->tn == tn) {
            size_t i;
            for(i=0; i < n; i++) {
                if (!type_eqv_(table->key[i], key[i], NULL))
                    break;
            }
            if (i==n) return table->type;
        }
        table = table->next;
    }
    return NULL;
}

static int t_uid_ctr = 1;  // TODO: lock

int  jl_get_t_uid_ctr() { return t_uid_ctr; }
void jl_set_t_uid_ctr(int i) { t_uid_ctr=i; }

int jl_assign_type_uid()
{
    return t_uid_ctr++;
}

// TODO: synchronize
static void cache_type_(jl_value_t **key, size_t n, jl_type_t *type)
{
    // only cache concrete types
    if (jl_has_typevars((jl_value_t*)type) || n==0)
        return;
    // assign uid
    if (jl_is_struct_type(type) && ((jl_struct_type_t*)type)->uid==0)
        ((jl_struct_type_t*)type)->uid = jl_assign_type_uid();
    else if (jl_is_bits_type(type) && ((jl_bits_type_t*)type)->uid==0)
        ((jl_bits_type_t*)type)->uid = jl_assign_type_uid();
    typekey_stack_t *tc =
        (typekey_stack_t*)((jl_tag_type_t*)type)->name->cache;
    typekey_stack_t *tk = (typekey_stack_t*)allocb(sizeof(typekey_stack_t));
    tk->tn = ((jl_tag_type_t*)type)->name;
    tk->type = type;
    tk->next = tc;
    ((jl_tag_type_t*)type)->name->cache = tk;
    tk->n = 0;
    tk->key = NULL;

    tk->key = (jl_value_t**)allocb(n * sizeof(void*));
    size_t i;
    for(i=0; i < n; i++) tk->key[i] = key[i];
    tk->n = n;
}

void jl_cache_type_(jl_tuple_t *params, jl_value_t *type)
{
    cache_type_(&params->data[0], params->length, (jl_type_t*)type);
}

jl_type_t *jl_lookup_type_(jl_typename_t *tn, jl_tuple_t *params)
{
    return lookup_type(tn->cache, tn, &params->data[0], params->length);
}

#ifdef JL_GC_MARKSWEEP
void jl_mark_type_cache(void *tc)
{
    typekey_stack_t *tk = (typekey_stack_t*)tc;
    while (tk != NULL) {
        jl_gc_setmark(tk);
        if (tk->key) jl_gc_setmark(tk->key);
        size_t i;
        for(i=0; i < tk->n; i++)
            jl_gc_markval(tk->key[i]);
        jl_gc_markval((jl_value_t*)tk->type);
        tk = tk->next;
    }
}
#endif

JL_CALLABLE(jl_f_tuple);

static jl_type_t *inst_type_w_(jl_value_t *t, jl_value_t **env, size_t n,
                               typekey_stack_t *stack)
{
    typekey_stack_t top;
    size_t i;
    if (n == 0) return (jl_type_t*)t;
    if (jl_is_typevar(t)) {
        for(i=0; i < n; i++) {
            if (env[i*2] == t)
                return (jl_type_t*)env[i*2+1];
        }
        return (jl_type_t*)t;
    }
    if (jl_is_tuple(t)) {
        jl_tuple_t *p = (jl_tuple_t*)t;
        jl_tuple_t *nt = jl_alloc_tuple(p->length);
        JL_GC_PUSH(&nt);
        for(i=0; i < p->length; i++) {
            jl_tupleset(nt, i, (jl_value_t*)inst_type_w_(jl_tupleref(p,i), env, n, stack));
        }
        JL_GC_POP();
        return (jl_type_t*)nt;
    }
    if (jl_is_union_type(t)) {
        jl_tuple_t *tw = (jl_tuple_t*)inst_type_w_((jl_value_t*)((jl_uniontype_t*)t)->types,
                                                   env, n, stack);
        JL_GC_PUSH(&tw);
        jl_type_t *res = (jl_type_t*)jl_new_uniontype(tw);
        JL_GC_POP();
        return res;
    }
    if (jl_is_func_type(t)) {
        jl_func_type_t *ft = (jl_func_type_t*)t;
        jl_type_t *fr=NULL, *to=NULL;
        JL_GC_PUSH(&fr, &to);
        fr = inst_type_w_((jl_value_t*)ft->from, env, n, stack);
        to = inst_type_w_((jl_value_t*)ft->to  , env, n, stack);
        jl_type_t *res = (jl_type_t*)jl_new_functype(fr, to);
        JL_GC_POP();
        return res;
    }
    if (jl_is_some_tag_type(t)) {
        jl_tag_type_t *tt = (jl_tag_type_t*)t;
        jl_tuple_t *tp = tt->parameters;
        if (jl_is_null(tp))
            return (jl_type_t*)t;
        jl_type_t *result;
        size_t ntp = tp->length;
        jl_value_t **iparams = (jl_value_t**)alloca((ntp+2) * sizeof(void*));
        for(i=0; i < ntp+2; i++) iparams[i] = NULL;
        jl_value_t **rt1 = &iparams[ntp+0];  // some extra gc roots
        jl_value_t **rt2 = &iparams[ntp+1];
        JL_GC_PUSHARGS(iparams, ntp+2);
        for(i=0; i < ntp; i++) {
            jl_value_t *elt = jl_tupleref(tp, i);
            if (elt == t)
                iparams[i] = t;
            else
                iparams[i] = (jl_value_t*)inst_type_w_(elt, env, n, stack);
        }
        jl_typename_t *tn = tt->name;

        // if an identical instantiation is already in process somewhere
        // up the stack, return it. this computes a fixed point for
        // recursive types.
        jl_type_t *lkup = lookup_type(stack, tn, iparams, ntp);
        if (lkup != NULL) { result = lkup; goto done_inst_tt; }

        // check type cache
        lkup = lookup_type(tn->cache, tn, iparams, ntp);
        if (lkup != NULL) { result = lkup; goto done_inst_tt; }

        // always use original type constructor (?)
        // only necessary for special cases like NTuple
        jl_value_t *tc = tn->primary;
        if (tc == (jl_value_t*)jl_ntuple_type && tc != t) {
            //(tc != NULL && tc != t)
            result = (jl_type_t*)apply_type_(tc, iparams, ntp);
            goto done_inst_tt;
        }

        // move array of instantiated parameters to heap; we need to keep it
        jl_tuple_t *iparams_tuple = jl_alloc_tuple_uninit(ntp);
        for(i=0; i < ntp; i++)
            jl_tupleset(iparams_tuple, i, iparams[i]);
        *rt1 = (jl_value_t*)iparams_tuple;
        if (jl_is_tag_type(t)) {
            jl_tag_type_t *tagt = (jl_tag_type_t*)t;
            jl_tag_type_t *ntt =
                (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAG_TYPE_NW);
            *rt2 = (jl_value_t*)ntt;
            top.tn = tn;
            top.key = iparams;
            top.n = ntp;
            top.type = (jl_type_t*)ntt;
            top.next = stack;
            stack = &top;
            ntt->name = tn;
            // temporarily initialize all fields so object is valid during
            // allocation of other objects (possible GC)
            ntt->fptr = NULL;
            ntt->env = NULL;
            ntt->linfo = NULL;
            ntt->super = jl_any_type;
            ntt->parameters = iparams_tuple;
            ntt->super = (jl_tag_type_t*)inst_type_w_((jl_value_t*)tagt->super,env,n,stack);
            cache_type_(iparams, ntp, (jl_type_t*)ntt);
            result = (jl_type_t*)ntt;
        }
        else if (jl_is_bits_type(t)) {
            jl_bits_type_t *bitst = (jl_bits_type_t*)t;
            jl_bits_type_t *nbt =
                (jl_bits_type_t*)newobj((jl_type_t*)jl_bits_kind, BITS_TYPE_NW);
            *rt2 = (jl_value_t*)nbt;
            top.tn = tn;
            top.key = iparams;
            top.n = ntp;
            top.type = (jl_type_t*)nbt;
            top.next = stack;
            stack = &top;
            nbt->name = tn;
            nbt->fptr = NULL;
            nbt->env = NULL;
            nbt->linfo = NULL;
            nbt->super = jl_any_type;
            nbt->parameters = iparams_tuple;
            nbt->nbits = bitst->nbits;
            nbt->bnbits = bitst->bnbits;
            nbt->super = (jl_tag_type_t*)inst_type_w_((jl_value_t*)bitst->super, env, n, stack);
            nbt->uid = 0;
            cache_type_(iparams, ntp, (jl_type_t*)nbt);
            result = (jl_type_t*)nbt;
        }
        else {
            assert(jl_is_struct_type(t));
            jl_struct_type_t *st = (jl_struct_type_t*)t;
            // create and initialize new struct type
            jl_struct_type_t *nst =
                (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind,
                                          STRUCT_TYPE_NW);
            *rt2 = (jl_value_t*)nst;
            // associate these parameters with the new struct type on
            // the stack, in case one of its field types references it.
            top.tn = tn;
            top.key = iparams;
            top.n = ntp;
            top.type = (jl_type_t*)nst;
            top.next = stack;
            stack = &top;
            nst->name = tn;
            nst->super = jl_any_type;
            nst->parameters = iparams_tuple;
            nst->names = st->names;
            nst->types = jl_null; // to be filled in below
            nst->fptr = jl_f_no_function;
            nst->env = (jl_value_t*)nst;
            nst->linfo = NULL;
            nst->ctor_factory = st->ctor_factory;
            nst->instance = NULL;
            nst->uid = 0;
            nst->super = (jl_tag_type_t*)inst_type_w_((jl_value_t*)st->super, env,n,stack);
            jl_tuple_t *ftypes = st->types;
            if (ftypes != NULL) {
                // recursively instantiate the types of the fields
                jl_tuple_t *nftypes = jl_alloc_tuple(ftypes->length);
                nst->types = nftypes;
                for(i=0; i < ftypes->length; i++) {
                    jl_tupleset(nftypes, i,
                                (jl_value_t*)inst_type_w_(jl_tupleref(ftypes,i),
                                                          env,n,stack));
                }
            }
            cache_type_(iparams, ntp, (jl_type_t*)nst);
            if (!jl_has_typevars_((jl_value_t*)nst,1)) {
                jl_add_constructors(nst);
            }
            result = (jl_type_t*)nst;
        }
    done_inst_tt:
        JL_GC_POP();
        return result;
    }
    return (jl_type_t*)t;
}

jl_type_t *jl_instantiate_type_with(jl_type_t *t, jl_value_t **env, size_t n)
{
    return inst_type_w_((jl_value_t*)t, env, n, NULL);
}

static int jl_subtype_le(jl_value_t *a,jl_value_t *b,int ta,int morespecific,
                         int invariant);

static int jl_tuple_subtype_(jl_value_t **child, size_t cl,
                             jl_value_t **parent, size_t pl, int ta,
                             int morespecific, int invariant)
{
    size_t ci=0, pi=0;
    int mode = 0;
    while(1) {
        int cseq = !ta && (ci<cl) && jl_is_seq_type(child[ci]);
        int pseq = (pi<pl) && jl_is_seq_type(parent[pi]);
        if (ci >= cl)
            return (pi>=pl || pseq);
        if (cseq && !pseq)
            return mode;
        if (pi >= pl)
            return 0;
        jl_value_t *ce = child[ci];
        jl_value_t *pe = parent[pi];
        if (cseq) ce = jl_tparam0(ce);
        if (pseq) pe = jl_tparam0(pe);

        if (!jl_subtype_le(ce, pe, ta, morespecific, invariant))
            return 0;

        if (morespecific) {
            // stop as soon as one element is strictly more specific
            if (!jl_types_equal(ce,pe)) {
                mode = 1;
                assert(!ta);
                // here go into a different mode where we return 1
                // if the only reason the child is not more specific is
                // argument count (i.e. ...)
            }
        }

        if (cseq && pseq) return 1;
        if (!cseq) ci++;
        if (!pseq) pi++;
    }
    return 0;
}

int jl_tuple_subtype(jl_value_t **child, size_t cl,
                     jl_value_t **parent, size_t pl, int ta, int morespecific)
{
    return jl_tuple_subtype_(child, cl, parent, pl, ta, morespecific, 0);
}

static int tuple_all_subtype(jl_tuple_t *t, jl_value_t *super,
                             int ta, int morespecific, int invariant)
{
    size_t ci;
    for(ci=0; ci < t->length; ci++) {
        jl_value_t *ce = jl_tupleref(t,ci);
        if (!ta && jl_is_seq_type(ce))
            ce = jl_tparam0(ce);
        if (!jl_subtype_le(ce, super, ta, morespecific, invariant))
            return 0;
    }
    return 1;
}

/*
  ta specifies whether typeof() should be implicitly applied to a.
  this is used for tuple types to avoid allocating them explicitly.
  morespecific means we only care whether a is more specific than b,
  not necessarily a strict subtype
*/
static int jl_subtype_le(jl_value_t *a, jl_value_t *b, int ta, int morespecific,
                         int invariant)
{
    if (!ta&&jl_is_typector(a)) a = (jl_value_t*)((jl_typector_t*)a)->body;
    if (jl_is_typector(b)) b = (jl_value_t*)((jl_typector_t*)b)->body;
    if (ta) {
        if (jl_is_tag_type(b) &&
            ((jl_tag_type_t*)b)->name == jl_type_type->name) {
            jl_value_t *bp = jl_tparam0(b);
            if (jl_is_type(a))
                return jl_subtype_le(a, bp, 0, morespecific, 1);
            if (jl_is_typector(a))
                return jl_subtype_le((jl_value_t*)((jl_typector_t*)a)->body, bp, 0, morespecific, 1);
        }
    }
    else if (a == b) {
        // None <: None
        return 1;
    }
    size_t i, j;
    if (jl_is_tuple(a)) {
        if ((jl_tuple_t*)b == jl_tuple_type) return 1;
        if (jl_is_tag_type(b) &&
            ((jl_tag_type_t*)b)->name == jl_ntuple_typename) {
            jl_tuple_t *tp = ((jl_tag_type_t*)b)->parameters;
            return tuple_all_subtype((jl_tuple_t*)a,
                                     jl_tupleref(tp,1), ta, morespecific,
                                     invariant);
        }
        if (jl_is_tuple(b)) {
            return jl_tuple_subtype_(&jl_tupleref(a,0),((jl_tuple_t*)a)->length,
                                     &jl_tupleref(b,0),((jl_tuple_t*)b)->length,
                                     ta, morespecific,invariant);
        }
    }

    if (!ta && jl_is_union_type(a)) {
        jl_tuple_t *ap = ((jl_uniontype_t*)a)->types;
        if (morespecific) {
            // Union a is more specific than b if some element of a is
            // more specific than b, and b is not more specific than any
            // element of a.
            for(i=0; i < ap->length; i++) {
                if (jl_subtype_le(jl_tupleref(ap,i), b, 0, 1, invariant) &&
                    !jl_subtype_le(b, jl_tupleref(ap,i), 0, 1, invariant)) {
                    for(j=0; j < ap->length; j++) {
                        if (jl_subtype_le(b, jl_tupleref(ap,j), 0, 1, invariant) &&
                            !jl_subtype_le(jl_tupleref(ap,j), b, 0, 1, invariant)) {
                            return 0;
                        }
                    }
                    return 1;
                }
            }
            if (!jl_is_typevar(b))
                return 0;
        }
        else {
            if (invariant && !jl_is_typevar(b)) {
                return jl_subtype_le(a,b,0,0,0) && jl_subtype_le(b,a,0,0,0);
            }
            for(i=0; i < ap->length; i++) {
                if (!jl_subtype_le(jl_tupleref(ap,i), b, 0, morespecific,
                                   invariant))
                    return 0;
            }
        }
        return 1;
    }

    if (jl_is_union_type(b)) {
        if (invariant)
            return 0;
        jl_tuple_t *bp = ((jl_uniontype_t*)b)->types;
        for(i=0; i < bp->length; i++) {
            if (jl_subtype_le(a, jl_tupleref(bp,i), ta, morespecific, invariant))
                return 1;
        }
        return 0;
    }

    if (ta) a = (jl_value_t*)jl_typeof(a);

    if (a == b) return 1;
    if (a==(jl_value_t*)jl_undef_type || b==(jl_value_t*)jl_undef_type)
        return 0;
    if (!invariant && (jl_tag_type_t*)b == jl_any_type) return 1;
    if (jl_is_typevar(a)) {
        if (jl_is_typevar(b)) {
            return
                jl_subtype_le((jl_value_t*)((jl_tvar_t*)a)->ub,
                              (jl_value_t*)((jl_tvar_t*)b)->ub, 0, 0, 0) &&
                jl_subtype_le((jl_value_t*)((jl_tvar_t*)b)->lb,
                              (jl_value_t*)((jl_tvar_t*)a)->lb, 0, 0, 0);
        }
        if (invariant) {
            return 0;
            //return
            //    jl_subtype_le((jl_value_t*)((jl_tvar_t*)a)->ub, b, 0, 0, 1) &&
            //    jl_subtype_le((jl_value_t*)((jl_tvar_t*)a)->lb, b, 0, 0, 1);
        }
        return jl_subtype_le((jl_value_t*)((jl_tvar_t*)a)->ub, b, 0, 0, 0);
    }
    if (jl_is_typevar(b)) {
        return jl_subtype_le(a, (jl_value_t*)((jl_tvar_t*)b)->ub, 0, 0, 0) &&
            jl_subtype_le((jl_value_t*)((jl_tvar_t*)b)->lb, a, 0, 0, 0);
    }
    if ((jl_tag_type_t*)a == jl_any_type) return 0;
    if (jl_is_tuple(b)) {
        if (jl_is_tag_type(a) &&
            ((jl_tag_type_t*)a)->name == jl_ntuple_typename) {
            // only ((T>:S)...,) can be a supertype of NTuple[N,S]
            jl_tuple_t *tp = (jl_tuple_t*)b;
            jl_value_t *ntp = jl_tupleref(((jl_tag_type_t*)a)->parameters, 1);
            if (tp->length == 1 && jl_is_seq_type(jl_tupleref(tp,0))) {
                return jl_subtype_le(ntp, jl_tparam0(jl_tupleref(tp,0)),
                                     0, morespecific, invariant);
            }
        }
        return 0;
    }
    if (jl_is_tuple(a)) return 0;

    if (jl_is_long(a)) {
        if (jl_is_long(b))
            return (jl_unbox_long(a)==jl_unbox_long(b));
        return 0;
    }
    if (jl_is_long(b)) return 0;

    if (jl_is_func_type(a)) {
        if (jl_is_func_type(b)) {
            jl_func_type_t *fa = (jl_func_type_t*)a;
            jl_func_type_t *fb = (jl_func_type_t*)b;
            return ( (jl_is_typevar(fb->from) ||
                      jl_subtype_le((jl_value_t*)fb->from,
                                    (jl_value_t*)fa->from, 0, morespecific,
                                    invariant)) &&
                     jl_subtype_le((jl_value_t*)fa->to,
                                   (jl_value_t*)fb->to,   0, morespecific,
                                   invariant) );
        }
        return 0;
    }
    else if (jl_is_func_type(b)) {
        return 0;
    }

    assert(jl_is_some_tag_type(a));
    assert(jl_is_some_tag_type(b));
    jl_tag_type_t *tta = (jl_tag_type_t*)a;
    jl_tag_type_t *ttb = (jl_tag_type_t*)b;
    int super=0;
    while (tta != (jl_tag_type_t*)jl_any_type) {
        if (tta->name == ttb->name) {
            if (super && morespecific) {
                if (tta->name != jl_type_type->name)
                    return 1;
            }
            if (tta->name == jl_ntuple_typename) {
                // NTuple must be covariant
                return jl_subtype_le(jl_tupleref(tta->parameters,1),
                                     jl_tupleref(ttb->parameters,1),
                                     0, morespecific, invariant);
            }
            assert(tta->parameters->length == ttb->parameters->length);
            for(i=0; i < tta->parameters->length; i++) {
                jl_value_t *apara = jl_tupleref(tta->parameters,i);
                jl_value_t *bpara = jl_tupleref(ttb->parameters,i);
                if (!jl_subtype_le(apara, bpara, 0, morespecific, 1))
                    return 0;
            }
            return 1;
        }
        else if (invariant) {
            return 0;
        }
        tta = tta->super; super = 1;
    }
    assert(!invariant);
    if (((jl_tag_type_t*)a)->name == jl_type_type->name) {
        // Type{T} also matches >:typeof(T)
        return jl_subtype_le(jl_tparam0(a), b, 1, morespecific, 0);
    }
    return 0;
}

int jl_subtype(jl_value_t *a, jl_value_t *b, int ta)
{
    return jl_subtype_le(a, b, ta, 0, 0);
}

int jl_subtype_invariant(jl_value_t *a, jl_value_t *b, int ta)
{
    return jl_subtype_le(a, b, ta, 0, 1);
}

int jl_type_morespecific(jl_value_t *a, jl_value_t *b, int ta)
{
    return jl_subtype_le(a, b, ta, 1, 0);
}

static jl_value_t *type_match_(jl_value_t *child, jl_value_t *parent,
                               jl_tuple_t **env, int morespecific,
                               int invariant);

static jl_value_t *tuple_match(jl_tuple_t *child, jl_tuple_t *parent,
                               jl_tuple_t **env, int morespecific,
                               int invariant)
{
    size_t ci=0, pi=0;
    size_t cl = child->length;
    size_t pl = parent->length;
    jl_value_t *tmp;
    while(1) {
        int cseq = (ci<cl) && jl_is_seq_type(jl_tupleref(child,ci));
        int pseq = (pi<pl) && jl_is_seq_type(jl_tupleref(parent,pi));
        if (ci >= cl)
            return (pi>=pl || pseq) ? (jl_value_t*)*env : jl_false;
        if (cseq && !pseq)
            return jl_false;
        if (pi >= pl)
            return jl_false;
        jl_value_t *ce = jl_tupleref(child,ci);
        jl_value_t *pe = jl_tupleref(parent,pi);
        if (cseq) ce = jl_tparam0(ce);
        if (pseq) pe = jl_tparam0(pe);

        tmp = type_match_(ce, pe, env, morespecific, invariant);
        if (tmp == jl_false) return jl_false;
        *env = (jl_tuple_t*)tmp;

        if (cseq && pseq) return (jl_value_t*)*env;
        if (!cseq) ci++;
        if (!pseq) pi++;
    }
    return (jl_value_t*)*env;
}

jl_tag_type_t *jl_wrap_Type(jl_value_t *t)
{
    jl_value_t *env[2];
    env[0] = jl_tparam0(jl_type_type);
    env[1] = t;
    return (jl_tag_type_t*)
        jl_instantiate_type_with((jl_type_t*)jl_type_type, env, 1);
}

static jl_value_t *type_match_(jl_value_t *child, jl_value_t *parent,
                               jl_tuple_t **env, int morespecific,
                               int invariant)
{
    jl_value_t *tmp, *tmp2;
    if (jl_is_typector(child))
        child = (jl_value_t*)((jl_typector_t*)child)->body;
    if (jl_is_typector(parent))
        parent = (jl_value_t*)((jl_typector_t*)parent)->body;
    size_t i, j;
    if (jl_is_typevar(parent)) {
        // make sure type is within this typevar's bounds
        if (!jl_subtype_le(child, parent, 0, 0, 0))
            return jl_false;
        if (!((jl_tvar_t*)parent)->bound) return (jl_value_t*)*env;
        jl_tuple_t *p = *env;
        while (p != jl_null) {
            if (jl_t0(p) == (jl_value_t*)parent) {
                jl_value_t *pv = jl_t1(p);
                if (jl_is_typevar(pv) && jl_is_typevar(child)) {
                    if (pv == (jl_value_t*)child)
                        return (jl_value_t*)*env;
                    return jl_false;
                }
                if (morespecific) {
                    if (jl_subtype(child, pv, 0)) {
                        return (jl_value_t*)*env;
                    }
                    else if (jl_subtype(pv, child, 0)) {
                        jl_t1(p) = (jl_value_t*)child;
                        return (jl_value_t*)*env;
                    }
                }
                else {
                    if (type_eqv_(child, pv, NULL))
                        return (jl_value_t*)*env;
                }
                return jl_false;
            }
            p = (jl_tuple_t*)jl_nextpair(p);
        }
        jl_tuple_t *np = jl_tuple3(parent, child, (jl_value_t*)*env);
        return (jl_value_t*)np;
    }

    if (child == parent) return (jl_value_t*)*env;

    if (jl_is_typevar(child)) {
        if (!invariant) {
            if (jl_subtype_le(child, parent, 0, morespecific, 0))
                return (jl_value_t*)*env;
        }
        return jl_false;
    }
    if (jl_is_long(child)) {
        if (jl_is_long(parent)) {
            if (jl_unbox_long((jl_value_t*)child) ==
                jl_unbox_long((jl_value_t*)parent))
                return (jl_value_t*)*env;
        }
        return jl_false;
    }
    if (jl_is_long(parent))
        return jl_false;
    if (!invariant && parent == (jl_value_t*)jl_any_type)
        return (jl_value_t*)*env;
    if (child  == (jl_value_t*)jl_any_type) return jl_false;

    if (jl_is_union_type(child)) {
        jl_tuple_t *t = ((jl_uniontype_t*)child)->types;
        if (morespecific) {
            for(i=0; i < t->length; i++) {
                jl_tuple_t *tenv = jl_null;
                tmp = type_match_(jl_tupleref(t,i), parent, env, 1, invariant);
                if (tmp != jl_false) {
                    *env = (jl_tuple_t*)tmp;
                    tmp2 = type_match_(parent, jl_tupleref(t,i), &tenv, 1,
                                       invariant);
                    if (tmp2 == jl_false) {
                        for(j=0; j < t->length; j++) {
                            tenv = jl_null;
                            if (type_match_(parent, jl_tupleref(t,j),
                                            &tenv, 1, invariant) != jl_false &&
                                type_match_(jl_tupleref(t,j), parent,
                                            env, 1, invariant) == jl_false) {
                                return jl_false;
                            }
                        }
                        return (jl_value_t*)*env;
                    }
                }
            }
            return jl_false;
        }
        else {
            for(i=0; i < t->length; i++) {
                tmp = type_match_(jl_tupleref(t,i), parent, env, morespecific,
                                  invariant);
                if (tmp == jl_false) return jl_false;
                *env = (jl_tuple_t*)tmp;
            }
            if (invariant && child == (jl_value_t*)jl_bottom_type &&
                !jl_is_typevar(parent))
                return jl_false;
        }
        return (jl_value_t*)*env;
    }
    if (jl_is_union_type(parent)) {
        jl_tuple_t *t = ((jl_uniontype_t*)parent)->types;
        for(i=0; i < t->length; i++) {
            jl_value_t *p = type_match_(child, jl_tupleref(t,i), env,
                                        morespecific, invariant);
            if (p != jl_false) return p;
        }
        return jl_false;
    }

    if (jl_is_func_type(parent)) {
        if (jl_is_func_type(child)) {
            tmp =
                type_match_((jl_value_t*)((jl_func_type_t*)child)->from,
                            (jl_value_t*)((jl_func_type_t*)parent)->from, env,
                            morespecific, invariant);
            if (tmp == jl_false) return jl_false;
            *env = (jl_tuple_t*)tmp;
            return type_match_((jl_value_t*)((jl_func_type_t*)child)->to,
                               (jl_value_t*)((jl_func_type_t*)parent)->to, env,
                               morespecific, invariant);
        }
        return jl_false;
    }
    else if (jl_is_func_type(child)) {
        return jl_false;
    }

    if (jl_is_tuple(child)) {
        if (jl_is_tag_type(parent) &&
            ((jl_tag_type_t*)parent)->name == jl_ntuple_typename) {
            jl_tuple_t *tp = ((jl_tag_type_t*)parent)->parameters;
            size_t alen = ((jl_tuple_t*)child)->length;
            // if child has a sequence type, there exists no N such that
            // NTuple[N,Any] could be its supertype.
            if (alen>0 && jl_is_seq_type(jl_tupleref(child,alen-1)))
                return jl_false;
            jl_value_t *nt_len = jl_tupleref(tp,0);
            jl_value_t *childlen = jl_box_long(((jl_tuple_t*)child)->length);
            if (jl_is_typevar(nt_len)) {
                tmp = type_match_(childlen, nt_len, env, morespecific,
                                  invariant);
                if (tmp == jl_false) return jl_false;
                *env = (jl_tuple_t*)tmp;
            }
            else {
                return jl_false;
            }
            jl_tuple_t *p_seq =
                jl_tuple1(jl_apply_type((jl_value_t*)jl_seq_type,
                                        jl_tuple1(jl_tupleref(tp,1))));
            return tuple_match((jl_tuple_t*)child, p_seq,
                               env, morespecific, invariant);
        }

        if (jl_is_tuple(parent)) {
            return tuple_match((jl_tuple_t*)child, (jl_tuple_t*)parent, env,
                               morespecific, invariant);
        }
        return jl_false;
    }
    if (jl_is_tuple(parent)) {
        if (jl_is_tag_type(child) &&
            ((jl_tag_type_t*)child)->name == jl_ntuple_typename) {
            // only ((T>:S)...,) can be a supertype of NTuple[N,S]
            jl_tuple_t *tp = (jl_tuple_t*)parent;
            jl_value_t *ntp = jl_tupleref(((jl_tag_type_t*)child)->parameters,
                                          1);
            if (tp->length == 1 && jl_is_seq_type(jl_tupleref(tp,0))) {
                return type_match_(ntp, jl_tparam0(jl_tupleref(tp,0)),
                                   env, morespecific, invariant);
            }
        }
        return jl_false;
    }

    assert(jl_is_some_tag_type(child));
    assert(jl_is_some_tag_type(parent));
    jl_tag_type_t *tta = (jl_tag_type_t*)child;
    jl_tag_type_t *ttb = (jl_tag_type_t*)parent;
    int super = 0;
    while (tta != (jl_tag_type_t*)jl_any_type) {
        if (tta->name == ttb->name) {
            if (super && morespecific)
                return (jl_value_t*)*env;
            assert(tta->parameters->length == ttb->parameters->length);
            for(i=0; i < tta->parameters->length; i++) {
                tmp = type_match_(jl_tupleref(tta->parameters,i),
                                  jl_tupleref(ttb->parameters,i),
                                  env, morespecific, 1);
                if (tmp == jl_false) return jl_false;
                *env = (jl_tuple_t*)tmp;
            }
            return (jl_value_t*)*env;
        }
        else if (invariant) {
            return jl_false;
        }
        tta = tta->super; super = 1;
    }
    assert(!invariant);
    if (((jl_tag_type_t*)child)->name == jl_type_type->name &&
        ttb->name != jl_type_type->name) {
        // Type{T} also matches >:typeof(T)
        return type_match_(jl_full_type(jl_tparam0(child)),
                           parent, env, morespecific, 0);
    }
    return jl_false;
}

/*
  typically a is a concrete type and b is a type containing typevars.
  this function tries to find a typevar assignment such that "a" is a subtype
  of "b".
  returns a linked list of (typevar,type) pairs.
  used to infer static parameter values in generic method definitions.
*/
jl_value_t *jl_type_match(jl_value_t *a, jl_value_t *b)
{
    jl_tuple_t *env = jl_null;
    JL_GC_PUSH(&env);
    jl_value_t *m = type_match_(a, b, &env, 0, 0);
    JL_GC_POP();
    return m;
}

jl_value_t *jl_type_match_invariant(jl_value_t *a, jl_value_t *b)
{
    jl_tuple_t *env = jl_null;
    JL_GC_PUSH(&env);
    jl_value_t *m = type_match_(a, b, &env, 0, 1);
    JL_GC_POP();
    return m;
}

jl_value_t *jl_type_match_morespecific(jl_value_t *a, jl_value_t *b)
{
    jl_tuple_t *env = jl_null;
    JL_GC_PUSH(&env);
    jl_value_t *m = type_match_(a, b, &env, 1, 0);
    JL_GC_POP();
    return m;
}

// given a (possibly-generic) function type and some argument types,
// determine the result type. this is using a function type A-->B as a
// transfer function.
DLLEXPORT
jl_value_t *jl_func_type_tfunc(jl_func_type_t *ft, jl_tuple_t *argtypes)
{
    if (!jl_has_typevars((jl_value_t*)ft->from)) {
        return (jl_value_t*)ft->to;
    }
    jl_value_t *env=jl_type_match((jl_value_t*)argtypes,(jl_value_t*)ft->from);
    jl_tuple_t *te = (env == jl_false) ? jl_null : (jl_tuple_t*)env;
    JL_GC_PUSH(&te);
    te = jl_flatten_pairs(te);
    jl_value_t *ty =
        (jl_value_t*)jl_instantiate_type_with(ft->to, &jl_t0(te), te->length/2);
    JL_GC_POP();
    return ty;
}

// initialization -------------------------------------------------------------

jl_tvar_t *jl_new_typevar(jl_sym_t *name, jl_value_t *lb, jl_value_t *ub)
{
    jl_tvar_t *tv = (jl_tvar_t*)newobj((jl_type_t*)jl_tvar_type, 4);
    tv->name = name;
    tv->lb = lb;
    tv->ub = ub;
    tv->bound = 1;
    return tv;
}

static jl_tvar_t *tvar(const char *name)
{
    jl_tvar_t *tv =
        jl_new_typevar(jl_symbol(name), (jl_value_t*)jl_bottom_type,
                       (jl_value_t*)jl_any_type);
    tv->bound=0;
    return tv;
}

static jl_tuple_t *jl_typevars(size_t n, ...)
{
#ifdef JL_GC_MARKSWEEP
    assert(!jl_gc_is_enabled());
#endif
    va_list args;
    va_start(args, n);
    jl_tuple_t *t = jl_alloc_tuple(n);
    size_t i;
    for(i=0; i < n; i++) {
        jl_tupleset(t, i, (jl_value_t*)tvar(va_arg(args, char*)));
    }
    va_end(args);
    return t;
}

JL_CALLABLE(jl_f_new_expr);
JL_CALLABLE(jl_f_new_symbolnode);
JL_CALLABLE(jl_f_new_box);

extern void jl_init_int32_int64_cache();

void jl_init_types()
{
    // create base objects
    jl_struct_kind = (jl_struct_type_t*)newobj(NULL, STRUCT_TYPE_NW);
    jl_struct_kind->type = (jl_type_t*)jl_struct_kind;
    jl_tag_kind = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);
    jl_tag_type_type = jl_tag_kind;
    jl_func_kind = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);

    jl_typename_type = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);
    jl_sym_type = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);
    jl_symbol_type = jl_sym_type;

    jl_any_type = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAG_TYPE_NW);
    jl_type_type = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAG_TYPE_NW);
    jl_tuple_type = jl_alloc_tuple(1);
    jl_tuple_type->type = (jl_type_t*)jl_tuple_type;

    jl_null = (jl_tuple_t*)newobj((jl_type_t*)jl_tuple_type, 1);
    jl_null->length = 0;
    jl_nothing = (jl_value_t*)jl_null; // for bootstrapping

    // initialize them. lots of cycles.
    jl_struct_kind->name = jl_new_typename(jl_symbol("CompositeKind"));
    jl_struct_kind->name->primary = (jl_value_t*)jl_struct_kind;
    jl_struct_kind->super = (jl_tag_type_t*)jl_tag_kind;
    jl_struct_kind->parameters = jl_null;
    jl_struct_kind->names = jl_tuple(5, jl_symbol("name"), jl_symbol("super"),
                                     jl_symbol("parameters"),
                                     jl_symbol("names"), jl_symbol("types"));
    jl_struct_kind->types = jl_tuple(5, jl_typename_type, jl_type_type,
                                     jl_tuple_type, jl_tuple_type,
                                     jl_tuple_type);
    jl_struct_kind->fptr = jl_f_no_function;
    jl_struct_kind->env = NULL;
    jl_struct_kind->linfo = NULL;
    jl_struct_kind->ctor_factory = NULL;
    jl_struct_kind->instance = NULL;
    jl_struct_kind->uid = jl_assign_type_uid();

    jl_tag_kind->name = jl_new_typename(jl_symbol("AbstractKind"));
    jl_tag_kind->name->primary = (jl_value_t*)jl_tag_kind;
    jl_tag_kind->super = jl_type_type;
    jl_tag_kind->parameters = jl_null;
    jl_tag_kind->names = jl_tuple(3, jl_symbol("name"), jl_symbol("super"),
                                  jl_symbol("parameters"));
    jl_tag_kind->types = jl_tuple(3, jl_typename_type, jl_type_type,
                                  jl_tuple_type);
    jl_tag_kind->fptr = jl_f_no_function;
    jl_tag_kind->env = NULL;
    jl_tag_kind->linfo = NULL;
    jl_tag_kind->ctor_factory = NULL;
    jl_tag_kind->instance = NULL;
    jl_tag_kind->uid = jl_assign_type_uid();

    jl_func_kind->name = jl_new_typename(jl_symbol("FuncKind"));
    jl_func_kind->name->primary = (jl_value_t*)jl_func_kind;
    jl_func_kind->super = jl_type_type;
    jl_func_kind->parameters = jl_null;
    jl_func_kind->names = jl_tuple(2, jl_symbol("from"), jl_symbol("to"));
    jl_func_kind->types = jl_tuple(2, jl_type_type, jl_type_type);
    jl_func_kind->fptr = jl_f_no_function;
    jl_func_kind->env = NULL;
    jl_func_kind->linfo = NULL;
    jl_func_kind->ctor_factory = NULL;
    jl_func_kind->instance = NULL;
    jl_func_kind->uid = jl_assign_type_uid();

    jl_typename_type->name = jl_new_typename(jl_symbol("TypeName"));
    jl_typename_type->name->primary = (jl_value_t*)jl_typename_type;
    jl_typename_type->super = jl_any_type;
    jl_typename_type->parameters = jl_null;
    jl_typename_type->names = jl_tuple(1, jl_symbol("name"));
    jl_typename_type->types = jl_tuple(1, jl_sym_type);
    jl_typename_type->uid = jl_assign_type_uid();
    jl_typename_type->fptr = jl_f_no_function;
    jl_typename_type->env = NULL;
    jl_typename_type->linfo = NULL;
    jl_typename_type->ctor_factory = NULL;
    jl_typename_type->instance = NULL;

    jl_sym_type->name = jl_new_typename(jl_symbol("Symbol"));
    jl_sym_type->name->primary = (jl_value_t*)jl_sym_type;
    jl_sym_type->super = jl_any_type;
    jl_sym_type->parameters = jl_null;
    jl_sym_type->names = jl_null;
    jl_sym_type->types = jl_null;
    jl_sym_type->fptr = jl_f_no_function;
    jl_sym_type->env = NULL;
    jl_sym_type->linfo = NULL;
    jl_sym_type->ctor_factory = NULL;
    jl_sym_type->instance = NULL;
    jl_sym_type->uid = jl_assign_type_uid();

    jl_any_type->name = jl_new_typename(jl_symbol("Any"));
    jl_any_type->name->primary = (jl_value_t*)jl_any_type;
    jl_any_type->super = jl_any_type;
    jl_any_type->parameters = jl_null;
    jl_any_type->fptr = NULL;
    jl_any_type->env = NULL;
    jl_any_type->linfo = NULL;

    jl_type_type->name = jl_new_typename(jl_symbol("Type"));
    jl_type_type->name->primary = (jl_value_t*)jl_type_type;
    jl_type_type->super = jl_any_type;
    jl_type_type->fptr = NULL;
    jl_type_type->env = NULL;
    jl_type_type->linfo = NULL;

    // now they can be used to create the remaining base kinds and types
    jl_methtable_type =
        jl_new_struct_type(jl_symbol("MethodTable"),
                           jl_any_type, jl_null, jl_null, jl_null);
    jl_methtable_type->fptr = jl_f_no_function;

    jl_union_kind = jl_new_struct_type(jl_symbol("UnionKind"),
                                       jl_type_type, jl_null,
                                       jl_tuple(1, jl_symbol("types")),
                                       jl_tuple(1, jl_tuple_type));
    jl_union_kind->fptr = jl_f_no_function;

    jl_bottom_type = (jl_type_t*)jl_new_struct(jl_union_kind, jl_null);

    // the universal (compatible w/ any signature) function type is Any-->None.
    // the most general (any function is compatible with it) function type
    // is None-->Any.
    // generic functions have the type Any-->Any, as they take any arguments
    // but don't satisfy any particular return-type requirements.
    jl_any_func = jl_new_functype((jl_type_t*)jl_any_type, (jl_type_t*)jl_any_type);

    jl_bottom_func = jl_new_closure(jl_f_no_function, NULL);

    jl_bits_kind =
        jl_new_struct_type(jl_symbol("BitsKind"), (jl_tag_type_t*)jl_tag_kind,
                           jl_null,
                           jl_tuple(4, jl_symbol("name"), jl_symbol("super"),
                                    jl_symbol("parameters"),
                                    jl_symbol("nbits")),
                           jl_tuple(4, jl_typename_type, jl_type_type,
                                    jl_tuple_type, jl_any_type));
    // cannot be created with normal constructor due to hidden fields
    jl_bits_kind->fptr = jl_f_no_function;
    
    jl_tvar_type = jl_new_struct_type(jl_symbol("TypeVar"),
                                      jl_any_type, jl_null,
                                      jl_tuple(3, jl_symbol("name"),
                                               jl_symbol("lb"),
                                               jl_symbol("ub")),
                                      jl_tuple(3, jl_sym_type, jl_type_type,
                                               jl_type_type));
    jl_tvar_type->fptr = jl_f_no_function;

    jl_tvar_t *tttvar = tvar("T");
    jl_type_type->parameters = jl_tuple(1, tttvar);

    jl_tuple_t *tv;

    tv = jl_typevars(1, "T");
    jl_seq_type = jl_new_tagtype((jl_value_t*)jl_symbol("..."),
                                 jl_any_type, tv);

    jl_tupleset(jl_tuple_type, 0,
                (jl_value_t*)jl_apply_type((jl_value_t*)jl_seq_type,
                                           jl_tuple(1,jl_any_type)));

    tv = jl_typevars(2, "N", "T");
    jl_ntuple_type = jl_new_tagtype((jl_value_t*)jl_symbol("NTuple"),
                                    jl_any_type, tv);
    jl_ntuple_typename = jl_ntuple_type->name;

    // non-primitive definitions follow
    jl_int32_type = NULL;
    jl_int32_type = jl_new_bitstype((jl_value_t*)jl_symbol("Int32"),
                                    jl_any_type, jl_null, 32);
    jl_int64_type = NULL;
    jl_int64_type = jl_new_bitstype((jl_value_t*)jl_symbol("Int64"),
                                    jl_any_type, jl_null, 64);
    jl_init_int32_int64_cache();
    jl_int32_type->bnbits = jl_box_int32(32);
    jl_int64_type->bnbits = jl_box_int32(64);
    jl_tupleset(jl_bits_kind->types, 3, (jl_value_t*)jl_int32_type);

    jl_bool_type = NULL;
    jl_bool_type = jl_new_bitstype((jl_value_t*)jl_symbol("Bool"),
                                   jl_any_type, jl_null, 8);
    jl_false = jl_box8(jl_bool_type, 0);
    jl_true  = jl_box8(jl_bool_type, 1);

    tv = jl_typevars(2, "T", "N");
    jl_abstractarray_type = jl_new_tagtype((jl_value_t*)jl_symbol("AbstractArray"),
                                           jl_any_type, tv);

    tv = jl_typevars(2, "T", "N");
    jl_array_type = 
        jl_new_struct_type(jl_symbol("Array"),
                           (jl_tag_type_t*)
                           jl_apply_type((jl_value_t*)jl_abstractarray_type, tv),
                           tv,
                           jl_null, jl_null);
    jl_array_typename = jl_array_type->name;
    jl_array_type->linfo = NULL;
    jl_initialize_generic_function((jl_function_t*)jl_array_type,
                                   jl_array_typename->name);

    jl_array_any_type =
        (jl_type_t*)jl_apply_type((jl_value_t*)jl_array_type,
                                  jl_tuple(2, jl_any_type,
                                           jl_box_long(1)));

    jl_expr_type =
        jl_new_struct_type(jl_symbol("Expr"),
                           jl_any_type, jl_null,
                           jl_tuple(3, jl_symbol("head"), jl_symbol("args"),
                                    jl_symbol("type")),
                           jl_tuple(3, jl_sym_type, jl_array_any_type,
                                    jl_any_type));
    jl_expr_type->fptr = jl_f_new_expr;

    jl_symbolnode_type =
        jl_new_struct_type(jl_symbol("SymbolNode"),
                           jl_any_type, jl_null,
                           jl_tuple(2, jl_symbol("name"), jl_symbol("type")),
                           jl_tuple(2, jl_sym_type, jl_any_type));
    jl_symbolnode_type->fptr = jl_f_new_symbolnode;

    jl_linenumbernode_type =
        jl_new_struct_type(jl_symbol("LineNumberNode"),
                           jl_any_type, jl_null,
                           jl_tuple(1, jl_symbol("line")),
                           jl_tuple(1, jl_long_type));

    jl_labelnode_type =
        jl_new_struct_type(jl_symbol("LabelNode"),
                           jl_any_type, jl_null,
                           jl_tuple(1, jl_symbol("label")),
                           jl_tuple(1, jl_long_type));

    jl_lambda_info_type =
        jl_new_struct_type(jl_symbol("LambdaStaticData"),
                           jl_any_type, jl_null,
                           jl_tuple(8, jl_symbol("ast"), jl_symbol("sparams"),
                                    jl_symbol("tfunc"), jl_symbol("name"),
                                    /*
                                    jl_symbol("roots"), jl_symbol("specTypes"),
                                    jl_symbol("unspecialized"),
                                    jl_symbol("specializations")*/
                                    jl_symbol(""), jl_symbol(""),
                                    jl_symbol(""), jl_symbol("")),
                           jl_tuple(8, jl_expr_type, jl_tuple_type,
                                    jl_any_type, jl_sym_type,
                                    jl_any_type, jl_tuple_type,
                                    jl_function_type, jl_tuple_type));
    jl_lambda_info_type->fptr = jl_f_no_function;

    jl_box_type =
        jl_new_struct_type(jl_symbol("Box"),
                           jl_any_type, jl_null,
                           jl_tuple(1, jl_symbol("contents")),
                           jl_tuple(1, jl_any_type));
    jl_box_type->fptr = jl_f_new_box;
    jl_box_typename = jl_box_type->name;
    jl_box_any_type = (jl_type_t*)jl_box_type;

    jl_typector_type =
        jl_new_struct_type(jl_symbol("TypeConstructor"),
                           jl_any_type, jl_null,
                           jl_tuple(2, jl_symbol("parameters"),
                                    jl_symbol("body")),
                           jl_tuple(2, jl_tuple_type, jl_any_type));

    tv = jl_typevars(2, "A", "B");
    jl_function_type =
        jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_functype((jl_type_t*)jl_tupleref(tv,0),
                                                     (jl_type_t*)jl_tupleref(tv,1)));

    jl_intrinsic_type = jl_new_bitstype((jl_value_t*)jl_symbol("IntrinsicFunction"),
                                        jl_any_type, jl_null, 32);

    tv = jl_typevars(1, "T");
    jl_pointer_type =
        jl_new_bitstype((jl_value_t*)jl_symbol("Ptr"), jl_any_type, tv,
#ifdef __LP64__
                        64
#else
                        32
#endif
                        );

    jl_pointer_void_type =
        (jl_bits_type_t*)jl_apply_type((jl_value_t*)jl_pointer_type,
                                       jl_tuple(1, jl_bottom_type));

    jl_undef_type = jl_new_tagtype((jl_value_t*)jl_symbol("Undef"),
                                   jl_any_type, jl_null);

    // Type{T}
    jl_typetype_tvar = tvar("T");
    jl_typetype_type = (jl_tag_type_t*)
        jl_apply_type((jl_value_t*)jl_type_type,
                      jl_tuple(1,jl_typetype_tvar));

    jl_ANY_flag = (jl_value_t*)tvar("ANY");

    call_sym = jl_symbol("call");
    call1_sym = jl_symbol("call1");
    quote_sym = jl_symbol("quote");
    top_sym = jl_symbol("top");
    dots_sym = jl_symbol("...");
    dollar_sym = jl_symbol("$");
    line_sym = jl_symbol("line");
    jl_continue_sym = jl_symbol("continue");
    error_sym = jl_symbol("error");
    goto_sym = jl_symbol("goto");
    goto_ifnot_sym = jl_symbol("gotoifnot");
    label_sym = jl_symbol("label");
    return_sym = jl_symbol("return");
    lambda_sym = jl_symbol("lambda");
    vinf_sym = jl_symbol("vinf");
    macro_sym = jl_symbol("macro");
    unexpanded_sym = jl_symbol("unexpanded");
    assign_sym = jl_symbol("=");
    null_sym = jl_symbol("null");
    isbound_sym = jl_symbol("isbound");
    body_sym = jl_symbol("body");
    locals_sym = jl_symbol("locals");
    colons_sym = jl_symbol("::");
    method_sym = jl_symbol("method");
    exc_sym = jl_symbol("the_exception");
    enter_sym = jl_symbol("enter");
    leave_sym = jl_symbol("leave");
    Any_sym = jl_symbol("Any");
    static_typeof_sym = jl_symbol("static_typeof");
    new_sym = jl_symbol("new");
    multivalue_sym = jl_symbol("multiple_value");
}
