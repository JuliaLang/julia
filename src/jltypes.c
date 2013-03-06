/*
  Types
  . type predicates (subtype) and type matching
  . type union and intersection
  . builtin type definitions
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef __WIN32__
#include <malloc.h>
#endif
#include "julia.h"
#include "newobj_internal.h"
#include "jltypes_internal.h"
#include "builtin_proto.h"

jl_datatype_t *jl_any_type;
jl_datatype_t *jl_type_type;
jl_datatype_t *jl_typename_type;
jl_datatype_t *jl_sym_type;
jl_datatype_t *jl_symbol_type;
jl_tuple_t *jl_tuple_type;
jl_datatype_t *jl_ntuple_type;
jl_typename_t *jl_ntuple_typename;
jl_datatype_t *jl_tvar_type;
jl_datatype_t *jl_uniontype_type;
jl_datatype_t *jl_datatype_type;

jl_value_t *jl_bottom_type;
jl_value_t *jl_top_type;
jl_datatype_t *jl_vararg_type;
jl_datatype_t *jl_abstractarray_type;

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
jl_datatype_t *jl_float32_type;
jl_datatype_t *jl_float64_type;

jl_tuple_t *jl_null;
jl_value_t *jl_nothing;

void jl_add_constructors(jl_datatype_t *t);

// --- type properties and predicates ---

int jl_is_type(jl_value_t *v)
{
    if (jl_is_tuple(v)) {
        jl_tuple_t *t = (jl_tuple_t*)v;
        size_t i;
        for(i=0; i < jl_tuple_len(t); i++) {
            jl_value_t *vv = jl_tupleref(t, i);
            if (!jl_is_typevar(vv) && !jl_is_type(vv))
                return 0;
        }
        return 1;
    }
    return jl_is_nontuple_type(v);
}

int jl_has_typevars_(jl_value_t *v, int incl_wildcard)
{
    size_t i;
    if (jl_typeis(v, jl_tvar_type)) {
        if (!((jl_tvar_t*)v)->bound)
            return incl_wildcard;
        return 1;
    }
    if (jl_is_typector(v))
        return incl_wildcard;
    jl_tuple_t *t;
    if (jl_is_uniontype(v))
        t = ((jl_uniontype_t*)v)->types;
    else if (jl_is_datatype(v))
        t = ((jl_datatype_t*)v)->parameters;
    else if (jl_is_tuple(v))
        t = (jl_tuple_t*)v;
    else
        t = jl_null;
    for(i=0; i < jl_tuple_len(t); i++) {
        jl_value_t *elt = jl_tupleref(t, i);
        if (elt != v) {
            if (jl_has_typevars_(elt, incl_wildcard))
                return 1;
        }
    }
    // probably not necessary; no reason to use match() instead of subtype()
    // on the unconstrained version of a type
    //if (jl_is_typector(v))
    //    return jl_tuple_len((((jl_typector_t*)v)->parameters) > 0);
    return 0;
}

int jl_has_typevars(jl_value_t *v)
{
    return jl_has_typevars_(v, 0);
}

DLLEXPORT int jl_is_leaf_type(jl_value_t *v)
{
    if (jl_is_datatype(v)) {
        if (((jl_datatype_t*)v)->abstract) {
            if (jl_is_type_type(v)) {
                return !jl_is_typevar(jl_tparam0(v));
            }
            return 0;
        }
        jl_tuple_t *t = ((jl_datatype_t*)v)->parameters;
        for(int i=0; i < jl_tuple_len(t); i++) {
            if (jl_is_typevar(jl_tupleref(t,i)))
                return 0;
        }
        return 1;
    }
    if (jl_is_tuple(v)) {
        jl_tuple_t *t = (jl_tuple_t*)v;
        for(int i=0; i < jl_tuple_len(t); i++) {
            if (!jl_is_leaf_type(jl_tupleref(t, i)))
                return 0;
        }
        return 1;
    }
    return 0;
}

// construct the full type of a value, possibly making a tuple type
jl_value_t *jl_full_type(jl_value_t *v)
{
    if (!jl_is_tuple(v))
        return (jl_value_t*)jl_typeof(v);
    jl_tuple_t *in = (jl_tuple_t*)v;
    jl_tuple_t *out = jl_alloc_tuple(jl_tuple_len(in));
    JL_GC_PUSH(&out);
    size_t i;
    for(i=0; i < jl_tuple_len(in); i++) {
        jl_tupleset(out, i, jl_full_type(jl_tupleref(in, i)));
    }
    JL_GC_POP();
    return (jl_value_t*)out;
}

static int type_eqv_(jl_value_t *a, jl_value_t *b);

// --- type union ---

static int count_union_components(jl_tuple_t *types)
{
    size_t i, c=0;
    for(i=0; i < jl_tuple_len(types); i++) {
        jl_value_t *e = jl_tupleref(types,i);
        if (jl_is_uniontype(e)) {
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
    for(i=0; i < jl_tuple_len(types); i++) {
        jl_value_t *e = jl_tupleref(types,i);
        if (jl_is_uniontype(e)) {
            flatten_type_union(((jl_uniontype_t*)e)->types, out, idx);
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

DLLEXPORT
jl_tuple_t *jl_compute_type_union(jl_tuple_t *types)
{
    size_t n = count_union_components(types);
    jl_value_t **temp;
    JL_GC_PUSHARGS(temp, n);
    size_t idx=0;
    flatten_type_union(types, temp, &idx);
    assert(idx == n);
    size_t i, j, ndel=0;
    for(i=0; i < n; i++) {
        for(j=0; j < n; j++) {
            if (j != i && temp[i] && temp[j]) {
                if (temp[i] == temp[j] ||
                    (!jl_has_typevars(temp[i]) &&
                     !jl_has_typevars(temp[j]) &&
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
    jl_tuple_t *result = jl_alloc_tuple_uninit(n - ndel);
    j=0;
    for(i=0; i < n; i++) {
        if (temp[i] != NULL) {
            jl_tupleset(result, j, temp[i]);
            j++;
        }
    }
    assert(j == n-ndel);
    // sort Union components by specificity, so "complex" type Unions work as
    // long as there are no ambiguities (see e.g. issue #126).
    // TODO: maybe warn about ambiguities
    qsort(result->data, j, sizeof(jl_value_t*), union_elt_morespecific);
    JL_GC_POP();
    return result;
}

jl_value_t *jl_type_union(jl_tuple_t *types)
{
    types = jl_compute_type_union(types);
    if (jl_tuple_len(types) == 1)
        return jl_tupleref(types, 0);
    if (jl_tuple_len(types) == 0)
        return (jl_value_t*)jl_bottom_type;
    JL_GC_PUSH(&types);
    jl_value_t *tu = (jl_value_t*)jl_new_uniontype(types);
    JL_GC_POP();
    return tu;
}

// --- type intersection ---

typedef enum {invariant, covariant} variance_t;

#define MAX_CENV_SIZE 128

typedef struct {
    jl_value_t **data;
    size_t n;
} cenv_t;

static inline int is_btv(jl_value_t *v)
{
    return jl_is_typevar(v) && ((jl_tvar_t*)v)->bound;
}

static void extend_(jl_value_t *var, jl_value_t *val, cenv_t *soln, int allow,
                    int ordered)
{
    if (!allow && var == val)
        return;
    if (!ordered && val < var && is_btv(val) && is_btv(var)) {
        jl_value_t *temp = val;
        val = var;
        var = temp;
    }
    for(int i=0; i < soln->n; i+=2) {
        if (soln->data[i]==var &&
            (soln->data[i+1]==val || (!jl_is_typevar(val) &&
                                      type_eqv_(soln->data[i+1],val))))
            return;
    }
    if (soln->n >= MAX_CENV_SIZE)
        jl_error("type too large");
    soln->data[soln->n++] = var;
    soln->data[soln->n++] = val;
}

static void extend(jl_value_t *var, jl_value_t *val, cenv_t *soln)
{
    extend_(var, val, soln, 0, 0);
}

static void extend_ordered(jl_value_t *var, jl_value_t *val, cenv_t *soln)
{
    extend_(var, val, soln, 0, 1);
}

static jl_value_t *jl_type_intersect(jl_value_t *a, jl_value_t *b,
                                     cenv_t *penv, cenv_t *eqc, variance_t var);

static jl_value_t *intersect_union(jl_uniontype_t *a, jl_value_t *b,
                                   cenv_t *penv, cenv_t *eqc, variance_t var)
{
    int eq0 = eqc->n, co0 = penv->n;
    jl_tuple_t *t = jl_alloc_tuple(jl_tuple_len(a->types));
    JL_GC_PUSH(&t);
    size_t i;
    for(i=0; i < jl_tuple_len(t); i++) {
        int eq_l = eqc->n, co_l = penv->n;
        jl_value_t *ti = jl_type_intersect(jl_tupleref(a->types,i), b,
                                           penv, eqc, var);
        if (ti == (jl_value_t*)jl_bottom_type) {
            eqc->n = eq0; penv->n = co0;
            ti = jl_type_intersect(jl_tupleref(a->types,i), b,
                                   penv, eqc, var);
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
        jl_tupleset(t, i, ti);
    }
    // problem: an intermediate union type we make here might be too
    // complex, even though the final type after typevars are replaced
    // might be ok.
    jl_value_t *tu = jl_type_union(t);
    JL_GC_POP();
    return tu;
}

// if returns with *bot!=0, then intersection is None
static size_t tuple_intersect_size(jl_tuple_t *a, jl_tuple_t *b, int *bot)
{
    size_t al = jl_tuple_len(a);
    size_t bl = jl_tuple_len(b);
    *bot = 0;
    if (al == bl) return al;
    if (al > bl) return tuple_intersect_size(b, a, bot);
    assert(al < bl);
    if (jl_is_vararg_type(jl_tupleref(b,bl-1))) {
        if (al > 0 && jl_is_vararg_type(jl_tupleref(a,al-1))) {
            return bl;
        }
        else {
            if (bl == al+1)
                return al;
            *bot=1;
            return 0;
        }
    }
    if (al > 0 && jl_is_vararg_type(jl_tupleref(a,al-1))) {
        return bl;
    }
    *bot=1;
    return 0;
}

static jl_value_t *intersect_tuple(jl_tuple_t *a, jl_tuple_t *b,
                                   cenv_t *penv, cenv_t *eqc, variance_t var)
{
    size_t al = jl_tuple_len(a);
    size_t bl = jl_tuple_len(b);
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
            if (jl_is_vararg_type(ae)) {
                aseq=1;
                ae = jl_tparam0(ae);
            }
            ai++;
        }
        if (bi < bl) {
            be = jl_tupleref(b,bi);
            if (jl_is_vararg_type(be)) {
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
                jl_tuple_set_len_unsafe(tc,jl_tuple_len(tc)-1);
                goto done_intersect_tuple;
            }
            result = (jl_value_t*)jl_bottom_type;
            goto done_intersect_tuple;
        }
        if (aseq && bseq) {
            ce = (jl_value_t*)jl_tuple1(ce);
            ce = (jl_value_t*)jl_apply_type((jl_value_t*)jl_vararg_type,
                                            (jl_tuple_t*)ce);
        }
        jl_tupleset(tc, ci, ce);
    }
 done_intersect_tuple:
    JL_GC_POP();
    return result;
}

static jl_value_t *intersect_tag(jl_datatype_t *a, jl_datatype_t *b,
                                 cenv_t *penv, cenv_t *eqc, variance_t var)
{
    assert(a->name == b->name);
    assert(jl_tuple_len(a->parameters) == jl_tuple_len(b->parameters));
    jl_tuple_t *p = jl_alloc_tuple(jl_tuple_len(a->parameters));
    JL_GC_PUSH(&p);
    jl_value_t *ti;
    size_t i;
    if (a->name == jl_ntuple_typename) {
        assert(jl_tuple_len(p) == 2);
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
        for(i=0; i < jl_tuple_len(p); i++) {
            jl_value_t *ap = jl_tupleref(a->parameters,i);
            jl_value_t *bp = jl_tupleref(b->parameters,i);
            if (jl_is_typevar(ap)) {
                if (var==invariant && jl_is_typevar(bp)) {
                    if (((jl_tvar_t*)ap)->bound != ((jl_tvar_t*)bp)->bound) {
                        // Foo{T} and Foo can never be equal since the former
                        // is always a subtype of the latter
                        JL_GC_POP();
                        return (jl_value_t*)jl_bottom_type;
                    }
                }
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
            else if (jl_has_typevars_(ap,1) || jl_has_typevars_(bp,1)) {
                if (jl_subtype_invariant(ap,bp,0) ||
                    jl_subtype_invariant(bp,ap,0)) {
                    ti = jl_type_intersect(ap,bp,penv,eqc,invariant);
                }
                else {
                    ti = (jl_value_t*)jl_bottom_type;
                }
            }
            else if (type_eqv_(ap,bp)) {
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

// convert a type to the value it would have if assigned to a static parameter
// in covariant context.
// example: (Type{Int},) => (DataType,)
// calling f{T}(x::T) as f((Int,)) should give T == (DataType,), but we
// might temporarily represent this type as (Type{Int},) for more precision.
static jl_value_t *type_to_static_parameter_value(jl_value_t *t)
{
    if (jl_is_type_type(t) && !jl_is_typevar(jl_tparam0(t)))
        return jl_full_type(jl_tparam0(t));
    if (jl_is_tuple(t)) {
        size_t l = jl_tuple_len(t);
        jl_tuple_t *nt = jl_alloc_tuple(l);
        JL_GC_PUSH(&nt);
        for(size_t i=0; i < l; i++) {
            jl_tupleset(nt, i, type_to_static_parameter_value(jl_tupleref(t,i)));
        }
        JL_GC_POP();
        return (jl_value_t*)nt;
    }
    return t;
}

static int match_intersection_mode = 0;

static jl_value_t *intersect_typevar(jl_tvar_t *a, jl_value_t *b,
                                     cenv_t *penv, cenv_t *eqc, variance_t var)
{
    if (var == covariant) {
        // matching T to Type{S} in covariant context
        b = type_to_static_parameter_value(b);
    }
    if (jl_subtype(b, (jl_value_t*)a, 0)) {
        if (!a->bound) return b;
    }
    else if (var==invariant && !jl_has_typevars_(b,1)) {
        // for typevar a and non-typevar type b, b must be within a's bounds
        // in invariant contexts.
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
            if (!((jl_tvar_t*)b)->bound) return (jl_value_t*)a;
        }
        else {
            if (!a->bound) return (jl_value_t*)a;
        }
    }
    else {
        return (jl_value_t*)jl_bottom_type;
    }
    if (var == invariant && !jl_has_typevars_(b,0)) {
        int i;
        for(i=0; i < eqc->n; i+=2) {
            if (eqc->data[i] == (jl_value_t*)a) {
                jl_value_t *v = eqc->data[i+1];
                if (jl_is_typevar(v))
                    continue;
                if (!jl_types_equal(v, b))
                    return (jl_value_t*)jl_bottom_type;
                break;
            }
        }
        if (i >= eqc->n) {
            extend((jl_value_t*)a, b, eqc);
        }
        return (jl_value_t*)a;
    }
    if ((jl_value_t*)a != b) {
        if (var == invariant)
            extend((jl_value_t*)a, b, eqc);
        else
            extend((jl_value_t*)a, b, penv);
    }
    return (jl_value_t*)a;
}

static int has_ntuple_intersect_tuple = 0;

static jl_value_t *jl_type_intersect(jl_value_t *a, jl_value_t *b,
                                     cenv_t *penv, cenv_t *eqc, variance_t var)
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
            return intersect_typevar((jl_tvar_t*)a, b, penv, eqc, var);
    }
    if (jl_is_typevar(b)) {
        if (var == covariant && !((jl_tvar_t*)b)->bound)
            b = ((jl_tvar_t*)b)->ub;
        else if (b != jl_ANY_flag)
            return intersect_typevar((jl_tvar_t*)b, a, penv, eqc, var);
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
        return intersect_union((jl_uniontype_t*)a, b, penv, eqc, var);
    if (jl_is_uniontype(b))
        return intersect_union((jl_uniontype_t*)b, a, penv, eqc, var);
    if (a == (jl_value_t*)jl_undef_type) return (jl_value_t*)jl_bottom_type;
    if (b == (jl_value_t*)jl_undef_type) return (jl_value_t*)jl_bottom_type;
    if (a == (jl_value_t*)jl_any_type || a == jl_ANY_flag) return b;
    if (b == (jl_value_t*)jl_any_type || b == jl_ANY_flag) return a;
    // tuple
    if (jl_is_tuple(a)) {
        jl_value_t *temp=NULL;
        JL_GC_PUSH(&b, &temp);
        if (jl_is_ntuple_type(b)) {
            has_ntuple_intersect_tuple = 1;
            long alen = (long)jl_tuple_len(a);
            jl_value_t *lenvar = jl_tparam0(b);
            jl_value_t *elty = jl_tparam1(b);
            int i;
            for(i=0; i < eqc->n; i+=2) {
                if (eqc->data[i] == lenvar) {
                    jl_value_t *v = eqc->data[i+1];
                    if (jl_is_long(v)) {
                        // N is already known in NTuple{N,...}
                        alen = jl_unbox_long(v);
                        break;
                    }
                }
            }
            b = (jl_value_t*)jl_tuple_fill(alen, elty);
            if (i >= eqc->n) {
                // don't know N yet, so add a constraint for it based on
                // the length of the other tuple
                if (alen > 0 && jl_is_vararg_type(jl_tupleref(a,alen-1))) {
                    temp = (jl_value_t*)jl_tuple1(elty);
                    jl_tupleset(b, alen-1, jl_apply_type((jl_value_t*)jl_vararg_type,
                                                         (jl_tuple_t*)temp));
                    if (jl_is_typevar(lenvar)) {
                        // store "at least N" constraints in the <: env
                        for(i=0; i < penv->n; i+=2) {
                            if (penv->data[i] == lenvar) {
                                jl_value_t *v = penv->data[i+1];
                                if (jl_is_long(v)) {
                                    int bot = 0;
                                    long met =
                                        meet_tuple_lengths(~jl_unbox_long(v),
                                                           ~(alen-1), &bot);
                                    if (bot) {
                                        JL_GC_POP();
                                        return (jl_value_t*)jl_bottom_type;
                                    }
                                    penv->data[i+1] = jl_box_long(~met);
                                    break;
                                }
                            }
                        }
                        if (i >= penv->n) {
                            temp = jl_box_long(alen-1);
                            extend(lenvar, temp, penv);
                        }
                    }
                }
                else {
                    if (jl_is_typevar(lenvar)) {
                        // store "== N" constraints in the == env
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
    // tag
    if (!jl_is_datatype(a) || !jl_is_datatype(b))
        return (jl_value_t*)jl_bottom_type;
    jl_datatype_t *tta = (jl_datatype_t*)a;
    jl_datatype_t *ttb = (jl_datatype_t*)b;
    if (tta->name == ttb->name)
        return (jl_value_t*)intersect_tag(tta, ttb, penv, eqc, var);
    jl_datatype_t *super = NULL;
    jl_datatype_t *sub = NULL;
    jl_value_t *env = NULL;
    jl_tuple_t *p = NULL;
    JL_GC_PUSH(&super, &sub, &env, &p);
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

    super = (jl_datatype_t*)jl_type_intersect((jl_value_t*)sub->super, (jl_value_t*)super, penv, eqc, var);

    if ((jl_value_t*)super == jl_bottom_type) {
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }

    // super needs to be instantiated so the matching below finds actual types
    // and doesn't fail due to the presence of extra typevars.
    super = (jl_datatype_t*)jl_instantiate_type_with((jl_value_t*)super, eqc->data, eqc->n/2);

    size_t n = jl_tuple_len(sub->parameters);

    assert(sub->name->primary != NULL);
    jl_value_t *tc = sub->name->primary;
    jl_tuple_t *tc_params = ((jl_datatype_t*)tc)->parameters;
    // compute what constraints the supertype imposes on the subtype
    jl_tuple_t *subs_sup_params =
        ((jl_datatype_t*)((jl_datatype_t*)tc)->super)->parameters;
    // match the intersected supertype against the pattern this subtype
    // uses to instantiate its supertype. this tells us what subtype parameter
    // values are implied by the intersected supertype, or that the
    // intersected supertype cannot come from this subtype (in which case
    // our final answer is None).
    size_t i;
    // hack: we need type_match to find assignments for all typevars
    int prev_mim = match_intersection_mode;
    match_intersection_mode = 1;
    env = jl_type_match((jl_value_t*)super->parameters,
                        (jl_value_t*)subs_sup_params);
    int sub_needs_parameters = 0;
    if (env == jl_false) {
        env = jl_type_match((jl_value_t*)subs_sup_params,
                            (jl_value_t*)super->parameters);
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
        for(int e=0; e < jl_tuple_len(env); e+=2) {
            jl_value_t *tp = jl_tupleref(env, e);
            // make sure each needed parameter is actually set by the subtype
            size_t j;
            for(j=0; j < n; j++) {
                if (tp == jl_tupleref(tc_params, j))
                    break;
            }
            if (j >= n) {
                JL_GC_POP();
                return (jl_value_t*)jl_bottom_type;
            }
        }
    }

    p = jl_alloc_tuple(n);
    for(i=0; i < n; i++) {
        jl_value_t *tp = jl_tupleref(tc_params, i);
        jl_value_t *elt = jl_tupleref(sub->parameters, i);
        for(int e=0; e < jl_tuple_len(env); e+=2) {
            if (jl_tupleref(env, e) == tp) {
                elt = jl_type_intersect(elt, jl_tupleref(env, e+1),
                                        penv, eqc, invariant);
                // note: elt might be None if "None" was the type parameter
                break;
            }
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
    jl_value_t *res = (jl_value_t*)jl_new_typevar(underscore_sym, lb, ub);
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
        return (jl_value_t*)jl_new_typevar(underscore_sym, tv->lb, ty);
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
    if (!jl_has_typevars_(X,1)) {
        if (!jl_has_typevars_(Y,1)) {
            if (var==invariant) {
                return (jl_types_equal(X,Y) ? X : NULL);
            }
        }
        return (jl_subtype(X,Y,0) ? X : NULL);
    }
    if (!jl_has_typevars_(Y,1)) {
        return (jl_subtype(Y,X,0) ? Y : NULL);
    }
    jl_value_t *v = jl_type_intersection(X, Y);
    return (v == (jl_value_t*)jl_bottom_type ?  NULL : v);
}

static int solve_tvar_constraints(cenv_t *env, cenv_t *soln)
{
    //JL_PRINTF(JL_STDOUT, "\n");
    jl_value_t *v=NULL;
    for(int i=0; i < env->n; i+=2) {
        jl_value_t *T = env->data[i];
        jl_value_t *S = env->data[i+1];
        jl_value_t **pT;
        pT = tvar_lookup(soln, &T);
        if (pT != &T) {
            // T=U is in the results
            jl_value_t **pU = pT;
            //jl_value_t *U = *pU;
            if (is_btv(S)) {
                // S is a typevar
                jl_value_t **pS;
                pS = tvar_lookup(soln, &S);
                if (pS != &S) {
                    // S=R is in the results
                    jl_value_t **pR = pS;
                    *pR = meet(*pR, *pU, invariant);
                    if (*pR == NULL) {
                        return 0;
                    }
                }
                else {
                    v = meet(*pU, S, covariant);
                    if (v == NULL) {
                        return 0;
                    }
                    extend(S, v, soln);
                }
                if (pS != pU)
                    *pU = S;
            }
            else {
                if (jl_is_long(*pU) && jl_is_long(S)) {
                    int bot = 0;
                    long mv = meet_tuple_lengths(~jl_unbox_long(S),
                                                  jl_unbox_long(*pU), &bot);
                    if (bot)
                        return 0;
                    v = jl_box_long(mv);
                }
                else if (!jl_is_type(S) && jl_is_typevar(*pU)) {
                    // combine T<:2 with T==N  =>  T==N
                    v = *pU;
                }
                else {
                    if (!jl_subtype(*pU, S, 0)) {
                        // T<:S and T=U and !(U<:S)
                        return 0;
                    }
                    v = meet(*pU, S, covariant);
                    if (v == NULL)
                        return 0;
                }
                if (is_btv(*pU)) {
                    extend(*pU, v, soln);
                }
                else {
                    *pU = v;
                }
            }
        }
        else {
            if (jl_has_typevars_(S,1)) {
                if (*tvar_lookup(soln, &S) != T)
                    extend(T, S, soln);
            }
            else if (jl_is_type(S)) {
                // ints in the <: env are not definite
                if (jl_is_leaf_type(S) || S == (jl_value_t*)jl_bottom_type) {
                    v = S;
                }
                else {
                    assert(jl_is_typevar(T));
                    v = meet(S, T, covariant);
                    if (!jl_is_typevar(v)) {
                        v = (jl_value_t*)
                            jl_new_typevar(underscore_sym,
                                           (jl_value_t*)jl_bottom_type, v);
                    }
                }
                extend(T, v, soln);
            }
        }
    }
    return 1;
}

/*
char *type_summary(jl_value_t *t)
{
    if (jl_is_tuple(t)) return "Tuple";
    if (jl_is_datatype(t))
        return ((jl_datatype_t*)t)->name->name->name;
    return "?";
}
void print_env(cenv_t *soln)
{
    for(int i=0; i < soln->n; i+=2) {
        jl_value_t *T, *S;
        T = soln->data[i]; S = soln->data[i+1];
        JL_PRINTF(JL_STDOUT,
                   "%s@%x=%s ",
                   ((jl_tvar_t*)T)->name->name, T,
                   type_summary(S));
    }
    JL_PRINTF(JL_STDOUT, "\n");
}
*/

jl_value_t *jl_type_intersection_matching(jl_value_t *a, jl_value_t *b,
                                          jl_tuple_t **penv, jl_tuple_t *tvars)
{
    jl_value_t **rts;
    JL_GC_PUSHARGS(rts, 1 + 2*MAX_CENV_SIZE);
    memset(rts, 0, (1+2*MAX_CENV_SIZE)*sizeof(void*));
    cenv_t eqc; eqc.n = 0; eqc.data = &rts[1];
    cenv_t env; env.n = 0; env.data = &rts[1+MAX_CENV_SIZE];
    jl_value_t **pti = &rts[0];

    has_ntuple_intersect_tuple = 0;
    JL_TRY {
        // This is kind of awful, but an inner call to instantiate_type
        // might fail due to a mismatched type parameter. The problem is
        // that we allow Range{T} to exist, even though the declaration of
        // Range specifies Range{T<:Real}. Therefore intersection cannot see
        // that some parameter values actually don't match.
        *pti = jl_type_intersect(a, b, &env, &eqc, covariant);
    }
    JL_CATCH {
        *pti = (jl_value_t*)jl_bottom_type;
    }
    if (*pti == (jl_value_t*)jl_bottom_type ||
        !(env.n > 0 || eqc.n > 0 || tvars != jl_null)) {
        JL_GC_POP();
        return *pti;
    }

    int e;

    if (has_ntuple_intersect_tuple) {
        for(e=0; e < eqc.n; e+=2) {
            jl_value_t *val = eqc.data[e+1];
            if (jl_is_long(val)) {
                break;
            }
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
            *pti = jl_type_intersect(a, b, &env, &eqc, covariant);
            if (*pti == (jl_value_t*)jl_bottom_type) {
                JL_GC_POP();
                return *pti;
            }
        }
    }

    if (!solve_tvar_constraints(&env, &eqc)) {
        JL_GC_POP();
        return (jl_value_t*)jl_bottom_type;
    }
    //JL_PRINTF(JL_STDOUT, "env: "); print_env(&env);
    //JL_PRINTF(JL_STDOUT, "sol: "); print_env(&eqc);

    int env0 = eqc.n;
    jl_value_t **tvs;
    int tvarslen;
    if (jl_is_typevar(tvars)) {
        tvs = (jl_value_t**)&tvars;
        tvarslen = 1;
    }
    else {
        tvs = &jl_t0(tvars);
        tvarslen = jl_tuple_len(tvars);
    }
    for(int tk=0; tk < tvarslen; tk++) {
        jl_value_t *tv = tvs[tk];
        for(e=0; e < env0; e+=2) {
            if (eqc.data[e] == tv) {
                break;
            }
        }
        // bind type vars to themselves if they were not matched explicitly
        // during type intersection.
        if (e >= env0)
            extend_(tv, tv, &eqc, 1, 0);
    }

    *penv = jl_alloc_tuple_uninit(eqc.n);
    for(int i=0; i < eqc.n; i+=2) {
        jl_tupleset(*penv, i, eqc.data[i]);
        jl_tupleset(*penv, i+1, *tvar_lookup(&eqc, &eqc.data[i+1]));
    }

    if (env0 > 0) {
        JL_TRY {
            *pti=(jl_value_t*)jl_instantiate_type_with((jl_value_t*)*pti,
                                                       &jl_t0(*penv), eqc.n/2);
        }
        JL_CATCH {
            *pti = (jl_value_t*)jl_bottom_type;
        }
    }

    JL_GC_POP();
    return *pti;
}

// --- type instantiation and cache ---

static int extensionally_same_type(jl_value_t *a, jl_value_t *b)
{
    return jl_subtype(a, b, 0) && jl_subtype(b, a, 0);
}

static int type_eqv_(jl_value_t *a, jl_value_t *b)
{
    if (a == b) return 1;
    if (jl_is_typector(a)) a = (jl_value_t*)((jl_typector_t*)a)->body;
    if (jl_is_typector(b)) b = (jl_value_t*)((jl_typector_t*)b)->body;
    if (jl_is_typevar(a)) {
        if (jl_is_typevar(b)) {
            return type_eqv_(((jl_tvar_t*)a)->ub, ((jl_tvar_t*)b)->ub) &&
                type_eqv_(((jl_tvar_t*)a)->lb, ((jl_tvar_t*)b)->lb);
        }
        else {
            return 0;
        }
    }
    if (jl_is_tuple(a)) {
        if (jl_is_tuple(b)) {
            jl_tuple_t *ta = (jl_tuple_t*)a; jl_tuple_t *tb = (jl_tuple_t*)b;
            int la = jl_tuple_len(ta), lb = jl_tuple_len(tb);
            if (la != lb) return 0;
            int sqa = (la>0 && jl_is_vararg_type(jl_tupleref(ta,la-1)));
            int sqb = (lb>0 && jl_is_vararg_type(jl_tupleref(tb,lb-1)));
            if (sqa != sqb) return 0;
            for(int i=0; i < la; i++) {
                jl_value_t *ea=jl_tupleref(ta,i), *eb=jl_tupleref(tb,i);
                if (jl_is_vararg_type(ea)) ea = jl_tparam0(ea);
                if (jl_is_vararg_type(eb)) eb = jl_tparam0(eb);
                if (!type_eqv_(ea, eb))
                    return 0;
            }
            return 1;
        }
        return 0;
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
    jl_tuple_t *ap = tta->parameters;
    jl_tuple_t *bp = ttb->parameters;
    assert(jl_tuple_len(ap) == jl_tuple_len(bp));
    size_t i;
    for(i=0; i < jl_tuple_len(ap); i++) {
        jl_value_t *api = jl_tupleref(ap,i);
        jl_value_t *bpi = jl_tupleref(bp,i);
        if (api == bpi) continue;
        if (!type_eqv_(api, bpi))
            return 0;
    }
    return 1;
}

int jl_types_equal(jl_value_t *a, jl_value_t *b)
{
    return type_eqv_(a, b);
}

static int type_le_generic(jl_value_t *a, jl_value_t *b)
{
    jl_value_t *env = jl_type_match(a, b);
    if (env == jl_false) return 0;
    // make sure all typevars correspond to other unique typevars
    for(int i=0; i < jl_tuple_len(env); i+=2) {
        if (!jl_is_typevar(jl_tupleref(env,i+1)))
            return 0;
        for(int j=0; j < jl_tuple_len(env); j+=2) {
            if (i != j) {
                if (jl_tupleref(env,i+1) == jl_tupleref(env,j+1))
                    return 0;
            }
        }
    }
    return 1;
}

int jl_types_equal_generic(jl_value_t *a, jl_value_t *b)
{
    return type_le_generic(a, b) && type_le_generic(b, a);
}

static int valid_type_param(jl_value_t *v)
{
    // TODO: maybe more things
    return jl_is_type(v) || jl_is_long(v) || jl_is_symbol(v) || jl_is_typevar(v);
}

jl_value_t *jl_apply_type_(jl_value_t *tc, jl_value_t **params, size_t n)
{
    if (n == 0) {
        if (jl_is_typector(tc))
            return (jl_value_t*)((jl_typector_t*)tc)->body;
        return tc;
    }
    size_t i;
    char *tname;
    jl_tuple_t *tp;
    jl_datatype_t *stprimary = NULL;
    if (jl_is_typector(tc)) {
        tp = ((jl_typector_t*)tc)->parameters;
        tname = "alias";
    }
    else {
        assert(jl_is_datatype(tc));
        tp = ((jl_datatype_t*)tc)->parameters;
        tname = ((jl_datatype_t*)tc)->name->name->name;
        stprimary = (jl_datatype_t*)((jl_datatype_t*)tc)->name->primary;
    }
    for(i=0; i < n; i++) {
        jl_value_t *pi = params[i];
        if (!valid_type_param(pi)) {
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
    size_t ntp = jl_tuple_len(tp);
    if (n > ntp)
        jl_errorf("too many parameters for type %s", tname);
    jl_value_t **env;
    JL_GC_PUSHARGS(env, 2*ntp);
    memset(env, 0, 2 * ntp * sizeof(jl_value_t*));
    size_t ne = 0;
    for(i=0; i < ntp; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_tupleref(tp,i);
        if (!jl_is_typevar(tv))
            continue;
        env[ne*2+0] = (jl_value_t*)tv;
        if (i >= n) {
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
            if (tc!=(jl_value_t*)jl_type_type && jl_is_typector(params[i]))
                env[ne*2+1] = (jl_value_t*)((jl_typector_t*)params[i])->body;
            else
                env[ne*2+1] = params[i];
        }
        ne++;
    }
    if (jl_is_typector(tc)) tc = (jl_value_t*)((jl_typector_t*)tc)->body;
    jl_value_t *result = jl_instantiate_type_with((jl_value_t*)tc, env, ne);
    JL_GC_POP();
    return (jl_value_t*)result;
}

jl_value_t *jl_apply_type(jl_value_t *tc, jl_tuple_t *params)
{
    return jl_apply_type_(tc, &jl_tupleref(params,0), jl_tuple_len(params));
}

static int typekey_compare(jl_datatype_t *tt, jl_value_t **key, size_t n)
{
    size_t j;
    for(j=0; j < n; j++) {
        if (!type_eqv_(jl_tupleref(tt->parameters,j), key[j]))
            return 0;
    }
    return 1;
}

static jl_value_t *lookup_type(jl_typename_t *tn, jl_value_t **key, size_t n)
{
    if (n==0) return NULL;
    jl_value_t *cache = tn->cache;
    jl_value_t **data;
    size_t cl;
    if (jl_is_tuple(cache)) {
        data = ((jl_tuple_t*)cache)->data;
        cl = jl_tuple_len(cache);
    }
    else {
        data = jl_array_data(cache);
        cl = jl_array_len(cache);
    }
    for(size_t i=0; i < cl; i++) {
        jl_datatype_t *tt = (jl_datatype_t*)data[i];
        if (typekey_compare(tt, key, n))
            return (jl_value_t*)tt;
    }
    return NULL;
}

static int t_uid_ctr = 1;

int  jl_get_t_uid_ctr(void) { return t_uid_ctr; }
void jl_set_t_uid_ctr(int i) { t_uid_ctr=i; }

int jl_assign_type_uid(void)
{
    return int32hash(t_uid_ctr++);
}

static void cache_type_(jl_value_t *type)
{
    // only cache concrete types
    jl_tuple_t *t = ((jl_datatype_t*)type)->parameters;
    if (jl_tuple_len(t) == 0) return;
    if (jl_is_abstracttype(type)) {
        if (jl_has_typevars_((jl_value_t*)type,1))
            return;
    }
    else {
        if (jl_has_typevars_((jl_value_t*)type,0))
            return;
        for(int i=0; i < jl_tuple_len(t); i++) {
            if (jl_is_typevar(jl_tupleref(t,i)))
                return;
        }
    }
    // assign uid
    if (!jl_is_abstracttype(type) && ((jl_datatype_t*)type)->uid==0)
        ((jl_datatype_t*)type)->uid = jl_assign_type_uid();
    jl_value_t *cache = ((jl_datatype_t*)type)->name->cache;
    // this needs to work before jl_array_any_type exists, so start with
    // a tuple and switch to an Array when possible.
    if (jl_array_any_type != NULL) {
        if (jl_is_tuple(cache)) {
            jl_array_t *nc = jl_alloc_cell_1d(jl_tuple_len(cache));
            memcpy(nc->data, ((jl_tuple_t*)cache)->data, sizeof(void*)*jl_tuple_len(cache));
            cache = (jl_value_t*)nc;
            ((jl_datatype_t*)type)->name->cache = cache;
        }
        jl_cell_1d_push((jl_array_t*)cache, (jl_value_t*)type);
    }
    else {
        assert(jl_is_tuple(cache));
        size_t n = jl_tuple_len(cache);
        jl_tuple_t *nc = jl_alloc_tuple_uninit(n+1);
        memcpy(nc->data, ((jl_tuple_t*)cache)->data, sizeof(void*) * n);
        jl_tupleset(nc, n, (jl_value_t*)type);
        ((jl_datatype_t*)type)->name->cache = (jl_value_t*)nc;
    }
}

jl_value_t *jl_cache_type_(jl_datatype_t *type)
{
    jl_value_t *t = lookup_type(type->name, type->parameters->data,
                                jl_tuple_len(type->parameters));
    if (t != NULL) return t;
    cache_type_((jl_value_t*)type);
    return (jl_value_t*)type;
}

JL_CALLABLE(jl_f_tuple);
JL_CALLABLE(jl_f_ctor_trampoline);

typedef struct _jl_typestack_t {
    jl_datatype_t *tt;
    struct _jl_typestack_t *prev;
} jl_typestack_t;

static jl_value_t *inst_type_w_(jl_value_t *t, jl_value_t **env, size_t n,
                                jl_typestack_t *stack)
{
    jl_typestack_t top;
    size_t i;
    if (n == 0) return (jl_value_t*)t;
    if (jl_is_typevar(t)) {
        for(i=0; i < n; i++) {
            if (env[i*2] == t)
                return (jl_value_t*)env[i*2+1];
        }
        return (jl_value_t*)t;
    }
    if (jl_is_tuple(t)) {
        jl_tuple_t *p = (jl_tuple_t*)t;
        jl_tuple_t *nt = jl_alloc_tuple(jl_tuple_len(p));
        JL_GC_PUSH(&nt);
        for(i=0; i < jl_tuple_len(p); i++) {
            jl_tupleset(nt, i, (jl_value_t*)inst_type_w_(jl_tupleref(p,i), env, n, stack));
        }
        JL_GC_POP();
        return (jl_value_t*)nt;
    }
    if (jl_is_uniontype(t)) {
        jl_tuple_t *tw = (jl_tuple_t*)inst_type_w_((jl_value_t*)((jl_uniontype_t*)t)->types,
                                                   env, n, stack);
        JL_GC_PUSH(&tw);
        jl_value_t *res = (jl_value_t*)jl_new_uniontype(tw);
        JL_GC_POP();
        return res;
    }
    if (jl_is_datatype(t)) {
        jl_datatype_t *tt = (jl_datatype_t*)t;
        jl_tuple_t *tp = tt->parameters;
        if (jl_is_null(tp))
            return (jl_value_t*)t;
        jl_typename_t *tn = tt->name;
        jl_value_t *tc = tn->primary;
        // don't instantiate "Foo" without parameters inside Foo
        if (t == tc && stack!=NULL)
            return (jl_value_t*)t;
        jl_value_t *result;
        size_t ntp = jl_tuple_len(tp);
        assert(ntp == jl_tuple_len(((jl_datatype_t*)tc)->parameters));
        jl_value_t **iparams;
        JL_GC_PUSHARGS(iparams, ntp+2);
        for(i=0; i < ntp+2; i++) iparams[i] = NULL;
        jl_value_t **rt1 = &iparams[ntp+0];  // some extra gc roots
        jl_value_t **rt2 = &iparams[ntp+1];
        int cacheable = 1, isabstract = 0;
        for(i=0; i < ntp; i++) {
            jl_value_t *elt = jl_tupleref(tp, i);
            if (elt == t) {
                iparams[i] = t;
            }
            else {
                iparams[i] = (jl_value_t*)inst_type_w_(elt, env, n, stack);
                jl_value_t *tv =
                    jl_tupleref(((jl_datatype_t*)tc)->parameters, i);
                if (jl_is_typevar(tv) && !jl_is_typevar(iparams[i])) {
                    // TODO: Undef should not be special here; fix.
                    // maybe introduce Top == Union(Any,Undef), and make this
                    // the default upper bound.
                    if (!jl_subtype(iparams[i], tv, 0)) {
                        jl_type_error_rt(tt->name->name->name,
                                         ((jl_tvar_t*)tv)->name->name,
                                         ((jl_tvar_t*)tv)->ub,
                                         iparams[i]);
                    }
                }
            }
            if (jl_is_typevar(iparams[i]))
                isabstract = 1;
            if (jl_has_typevars_(iparams[i],0))
                cacheable = 0;
        }

        // if an identical instantiation is already in process somewhere
        // up the stack, return it. this computes a fixed point for
        // recursive types.
        jl_typestack_t *tmp = stack;
        jl_value_t *lkup = NULL;
        while (tmp != NULL) {
            if (tmp->tt->name==tn && ntp==jl_tuple_len(tmp->tt->parameters) &&
                typekey_compare(tmp->tt, iparams, ntp)) {
                lkup = (jl_value_t*)tmp->tt;
                break;
            }
            tmp = tmp->prev;
        }
        if (lkup != NULL && lkup != (jl_value_t*)tc) {
            result = lkup; goto done_inst_tt;
        }

        // check type cache
        if (cacheable) {
            lkup = (jl_value_t*)lookup_type(tn, iparams, ntp);
            if (lkup != NULL) {
                result = lkup; goto done_inst_tt;
            }
        }

        // always use original type constructor
        if (tc != t) {
            //(tc != NULL && tc != t)
            result = (jl_value_t*)jl_apply_type_(tc, iparams, ntp);
            goto done_inst_tt;
        }

        // move array of instantiated parameters to heap; we need to keep it
        jl_tuple_t *iparams_tuple = jl_alloc_tuple_uninit(ntp);
        for(i=0; i < ntp; i++)
            jl_tupleset(iparams_tuple, i, iparams[i]);
        *rt1 = (jl_value_t*)iparams_tuple;

        jl_datatype_t *dt = (jl_datatype_t*)t;
        // create and initialize new type
        jl_datatype_t *ndt =
            jl_new_uninitialized_datatype(jl_tuple_len(dt->names));
        *rt2 = (jl_value_t*)ndt;
        // associate these parameters with the new type on
        // the stack, in case one of its field types references it.
        top.tt = (jl_datatype_t*)ndt;
        top.prev = stack;
        stack = &top;
        ndt->name = tn;
        ndt->super = jl_any_type;
        ndt->parameters = iparams_tuple;
        ndt->names = dt->names;
        ndt->types = jl_null; // to be filled in below
        if (isabstract || !jl_is_function(dt->ctor_factory))
            ndt->fptr = jl_f_no_function;
        else
            ndt->fptr = jl_f_ctor_trampoline;
        ndt->mutabl = dt->mutabl;
        ndt->abstract = dt->abstract;
        ndt->env = (jl_value_t*)ndt;
        ndt->linfo = NULL;
        ndt->ctor_factory = dt->ctor_factory;
        ndt->instance = NULL;
        ndt->uid = 0;
        ndt->struct_decl = NULL;
        ndt->super = (jl_datatype_t*)inst_type_w_((jl_value_t*)dt->super, env,n,stack);
        jl_tuple_t *ftypes = dt->types;
        if (ftypes != NULL) {
            // recursively instantiate the types of the fields
            ndt->types = (jl_tuple_t*)inst_type_w_((jl_value_t*)ftypes, env, n, stack);
            if (!isabstract) {
                jl_compute_field_offsets(ndt);
            }
            else {
                ndt->size = 0;
                ndt->pointerfree = 0;
            }
            if (tn == jl_array_typename)
                ndt->pointerfree = 0;
            if (ftypes->length == 0)
                ndt->size = dt->size;
        }
        if (cacheable) cache_type_((jl_value_t*)ndt);
        result = (jl_value_t*)ndt;

    done_inst_tt:
        JL_GC_POP();
        return result;
    }
    return (jl_value_t*)t;
}

jl_value_t *jl_instantiate_type_with(jl_value_t *t, jl_value_t **env, size_t n)
{
    return inst_type_w_((jl_value_t*)t, env, n, NULL);
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

void jl_reinstantiate_inner_types(jl_datatype_t *t)
{
    jl_typestack_t top;
    top.tt = (jl_datatype_t*)t;
    top.prev = NULL;
    size_t n = jl_tuple_len(t->parameters);
    jl_value_t **env = alloca(n*2*sizeof(void*));
    for(int i=0; i < n; i++) {
        env[i*2] = jl_tupleref(t->parameters,i);
        env[i*2+1] = env[i*2];
    }
    t->super = (jl_datatype_t*)inst_type_w_((jl_value_t*)t->super, env, n, &top);
    if (jl_is_datatype(t)) {
        jl_datatype_t *st = (jl_datatype_t*)t;
        st->types = (jl_tuple_t*)inst_type_w_((jl_value_t*)st->types, env, n, &top);
    }
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
        int cseq = !ta && (ci<cl) && jl_is_vararg_type(child[ci]);
        int pseq = (pi<pl) && jl_is_vararg_type(parent[pi]);
        if ((!morespecific||mode) && cseq && !pseq)
            return mode;
        if (ci >= cl)
            return (pi>=pl || pseq);
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
            if (!(jl_types_equal(ce,pe) ||
                  (jl_is_typevar(pe) &&
                   jl_types_equal(ce,((jl_tvar_t*)pe)->ub)))) {
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
    for(ci=0; ci < jl_tuple_len(t); ci++) {
        jl_value_t *ce = jl_tupleref(t,ci);
        if (!ta && jl_is_vararg_type(ce))
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
        if (jl_is_type_type(b)) {
            jl_value_t *bp = jl_tparam0(b);
            return jl_subtype_le((jl_value_t*)jl_typeof(a),
                                 (jl_value_t*)jl_type_type, 0, morespecific, 0) &&
                jl_subtype_le(a, bp, 0, morespecific, 1);
        }
    }
    else if (a == b) {
        // None <: None
        return 1;
    }
    size_t i, j;
    if (jl_is_tuple(a)) {
        if ((jl_tuple_t*)b == jl_tuple_type) return 1;
        if (jl_is_datatype(b) &&
            ((jl_datatype_t*)b)->name == jl_ntuple_typename) {
            jl_tuple_t *tp = ((jl_datatype_t*)b)->parameters;
            return tuple_all_subtype((jl_tuple_t*)a,
                                     jl_tupleref(tp,1), ta, morespecific,
                                     invariant);
        }
        if (jl_is_tuple(b)) {
            return jl_tuple_subtype_(&jl_tupleref(a,0),jl_tuple_len(a),
                                     &jl_tupleref(b,0),jl_tuple_len(b),
                                     ta, morespecific,invariant);
        }
    }

    if (!ta && jl_is_uniontype(a)) {
        jl_tuple_t *ap = ((jl_uniontype_t*)a)->types;
        if (morespecific) {
            // Union a is more specific than b if some element of a is
            // more specific than b, and b is not more specific than any
            // element of a.
            for(i=0; i < jl_tuple_len(ap); i++) {
                if (jl_subtype_le(jl_tupleref(ap,i), b, 0, 1, invariant) &&
                    !jl_subtype_le(b, jl_tupleref(ap,i), 0, 1, invariant)) {
                    for(j=0; j < jl_tuple_len(ap); j++) {
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
            for(i=0; i < jl_tuple_len(ap); i++) {
                if (!jl_subtype_le(jl_tupleref(ap,i), b, 0, morespecific,
                                   invariant))
                    return 0;
            }
        }
        return 1;
    }

    if (jl_is_uniontype(b)) {
        if (invariant)
            return 0;
        jl_tuple_t *bp = ((jl_uniontype_t*)b)->types;
        for(i=0; i < jl_tuple_len(bp); i++) {
            if (jl_subtype_le(a, jl_tupleref(bp,i), ta, morespecific, invariant))
                return 1;
        }
        return 0;
    }

    if (ta) a = (jl_value_t*)jl_typeof(a);

    if (a == b) return 1;
    if ((a==(jl_value_t*)jl_undef_type && !jl_is_typevar(b)) ||
        b==(jl_value_t*)jl_undef_type)
        return 0;
    if (!invariant && (jl_datatype_t*)b == jl_any_type) return 1;

    if (jl_is_datatype(a) && jl_is_datatype(b)) {
        if ((jl_datatype_t*)a == jl_any_type) return 0;
        jl_datatype_t *tta = (jl_datatype_t*)a;
        jl_datatype_t *ttb = (jl_datatype_t*)b;
        int super=0;
        while (tta != (jl_datatype_t*)jl_any_type) {
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
                assert(jl_tuple_len(tta->parameters) == jl_tuple_len(ttb->parameters));
                for(i=0; i < jl_tuple_len(tta->parameters); i++) {
                    jl_value_t *apara = jl_tupleref(tta->parameters,i);
                    jl_value_t *bpara = jl_tupleref(ttb->parameters,i);
                    if (invariant && !morespecific && jl_is_typevar(bpara) &&
                        !((jl_tvar_t*)bpara)->bound) {
                        if (!jl_is_typevar(apara))
                            return 0;
                    }
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
        if (((jl_datatype_t*)a)->name == jl_type_type->name) {
            // Type{T} also matches >:typeof(T)
            if (!jl_is_typevar(jl_tparam0(a)))
                return jl_subtype_le(jl_tparam0(a), b, 1, morespecific, 0);
        }
        return 0;
    }

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
    if ((jl_datatype_t*)a == jl_any_type) return 0;
    if (jl_is_tuple(b)) {
        if (jl_is_datatype(a) &&
            ((jl_datatype_t*)a)->name == jl_ntuple_typename) {
            // only ((T>:S)...,) can be a supertype of NTuple[N,S]
            jl_tuple_t *tp = (jl_tuple_t*)b;
            jl_value_t *ntp = jl_tupleref(((jl_datatype_t*)a)->parameters, 1);
            if (jl_tuple_len(tp) == 1 && jl_is_vararg_type(jl_tupleref(tp,0))) {
                return jl_subtype_le(ntp, jl_tparam0(jl_tupleref(tp,0)),
                                     0, morespecific, invariant);
            }
        }
        return 0;
    }
    if (jl_is_tuple(a)) return 0;

    return jl_egal(a, b);
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
                               cenv_t *env, int morespecific, int invariant);

static jl_value_t *tuple_match(jl_tuple_t *child, jl_tuple_t *parent,
                               cenv_t *env, int morespecific, int invariant)
{
    size_t ci=0, pi=0;
    size_t cl = jl_tuple_len(child);
    size_t pl = jl_tuple_len(parent);
    while(1) {
        int cseq = (ci<cl) && jl_is_vararg_type(jl_tupleref(child,ci));
        int pseq = (pi<pl) && jl_is_vararg_type(jl_tupleref(parent,pi));
        if (ci >= cl)
            return (pi>=pl || pseq) ? jl_true : jl_false;
        if (cseq && !pseq)
            return jl_false;
        if (pi >= pl)
            return jl_false;
        jl_value_t *ce = jl_tupleref(child,ci);
        jl_value_t *pe = jl_tupleref(parent,pi);
        if (cseq) ce = jl_tparam0(ce);
        if (pseq) pe = jl_tparam0(pe);

        int n = env->n;
        if (type_match_(ce, pe, env, morespecific, invariant) == jl_false)
            { env->n = n; return jl_false; }

        if (cseq && pseq) return jl_true;
        if (!cseq) ci++;
        if (!pseq) pi++;
    }
    return jl_true;
}

static jl_value_t *type_match_(jl_value_t *child, jl_value_t *parent,
                               cenv_t *env, int morespecific, int invariant)
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
                    if (jl_subtype(child, pv, 0)) {
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
        extend_ordered(parent, child, env);
        return jl_true;
    }

    if (child == parent) return jl_true;

    if (jl_is_typevar(child)) {
        if (!invariant) {
            if (jl_subtype_le(child, parent, 0, morespecific, 0))
                return jl_true;
        }
        return jl_false;
    }
    if (!invariant && parent == (jl_value_t*)jl_any_type)
        return jl_true;
    if (child  == (jl_value_t*)jl_any_type) return jl_false;

    if (jl_is_uniontype(child)) {
        jl_tuple_t *t = ((jl_uniontype_t*)child)->types;
        if (morespecific) {
            cenv_t tenv;
            tenv.data = alloca(MAX_CENV_SIZE*sizeof(void*));
            for(i=0; i < jl_tuple_len(t); i++) {
                int n = env->n;
                tmp = type_match_(jl_tupleref(t,i), parent, env, 1, invariant);
                if (tmp != jl_false) {
                    tenv.n = 0;
                    tmp2 = type_match_(parent, jl_tupleref(t,i), &tenv, 1,
                                       invariant);
                    if (tmp2 == jl_false) {
                        n = env->n;
                        for(j=0; j < jl_tuple_len(t); j++) {
                            tenv.n = 0;
                            env->n = n;
                            if (type_match_(parent, jl_tupleref(t,j),
                                            &tenv, 1, invariant) != jl_false &&
                                type_match_(jl_tupleref(t,j), parent,
                                            env, 1, invariant) == jl_false) {
                                env->n = n;
                                return jl_false;
                            }
                        }
                        return jl_true;
                    }
                }
                else {
                    env->n = n;
                }
            }
            return jl_false;
        }
        else {
            for(i=0; i < jl_tuple_len(t); i++) {
                int n = env->n;
                if (type_match_(jl_tupleref(t,i), parent, env, morespecific,
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
        jl_tuple_t *t = ((jl_uniontype_t*)parent)->types;
        int n = env->n;
        for(i=0; i < jl_tuple_len(t); i++) {
            env->n = n;
            if (type_match_(child, jl_tupleref(t,i), env,
                            morespecific, invariant) != jl_false)
                return jl_true;
        }
        return jl_false;
    }

    if (jl_is_tuple(child)) {
        if (jl_is_datatype(parent) &&
            ((jl_datatype_t*)parent)->name == jl_ntuple_typename) {
            jl_tuple_t *tp = ((jl_datatype_t*)parent)->parameters;
            size_t alen = jl_tuple_len(child);
            // if child has a sequence type, there exists no N such that
            // NTuple[N,Any] could be its supertype.
            if (alen>0 && jl_is_vararg_type(jl_tupleref(child,alen-1)))
                return jl_false;
            jl_value_t *nt_len = jl_tupleref(tp,0);
            jl_value_t *childlen = jl_box_long(jl_tuple_len(child));
            if (jl_is_typevar(nt_len)) {
                int n = env->n;
                if (type_match_(childlen, nt_len, env, morespecific,
                                invariant) == jl_false)
                    { env->n = n; return jl_false; }
            }
            else {
                return jl_false;
            }
            jl_value_t *p_seq = (jl_value_t*)jl_tuple1(jl_tupleref(tp,1));
            JL_GC_PUSH(&p_seq);
            p_seq = (jl_value_t*)jl_apply_type((jl_value_t*)jl_vararg_type,
                                               (jl_tuple_t*)p_seq);
            p_seq = (jl_value_t*)jl_tuple1(p_seq);
            tmp = tuple_match((jl_tuple_t*)child, (jl_tuple_t*)p_seq,
                              env, morespecific, invariant);
            JL_GC_POP();
            return tmp;
        }

        if (jl_is_tuple(parent)) {
            return tuple_match((jl_tuple_t*)child, (jl_tuple_t*)parent, env,
                               morespecific, invariant);
        }
        return jl_false;
    }
    if (jl_is_tuple(parent)) {
        if (jl_is_datatype(child) &&
            ((jl_datatype_t*)child)->name == jl_ntuple_typename) {
            // only ((T>:S)...,) can be a supertype of NTuple[N,S]
            jl_tuple_t *tp = (jl_tuple_t*)parent;
            jl_value_t *ntp = jl_tupleref(((jl_datatype_t*)child)->parameters,
                                          1);
            if (jl_tuple_len(tp) == 1 && jl_is_vararg_type(jl_tupleref(tp,0))) {
                return type_match_(ntp, jl_tparam0(jl_tupleref(tp,0)),
                                   env, morespecific, invariant);
            }
        }
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
            assert(jl_tuple_len(tta->parameters) == jl_tuple_len(ttb->parameters));
            for(i=0; i < jl_tuple_len(tta->parameters); i++) {
                int n = env->n;
                if (type_match_(jl_tupleref(tta->parameters,i),
                                jl_tupleref(ttb->parameters,i),
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
        return type_match_(jl_full_type(jl_tparam0(child)),
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
    memset(env.data, 0, MAX_CENV_SIZE*sizeof(void*));
    jl_value_t *m = type_match_(a, b, &env, morespecific, 0);
    if (m != jl_false) {
        m = (jl_value_t*)jl_alloc_tuple_uninit(env.n);
        for(int i=0; i < env.n; i++) {
            jl_tupleset(m, i, env.data[i]);
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

jl_tvar_t *jl_new_typevar(jl_sym_t *name, jl_value_t *lb, jl_value_t *ub)
{
    jl_tvar_t *tv = (jl_tvar_t*)newobj((jl_value_t*)jl_tvar_type, 4);
    tv->name = name;
    tv->lb = lb;
    tv->ub = ub;
    tv->bound = 0;
    return tv;
}

static jl_tvar_t *tvar(const char *name)
{
    return jl_new_typevar(jl_symbol(name), (jl_value_t*)jl_bottom_type,
                          (jl_value_t*)jl_any_type);
}

extern void jl_init_int32_int64_cache(void);

void jl_init_types(void)
{
    // create base objects
    jl_datatype_type = jl_new_uninitialized_datatype(14);
    jl_datatype_type->type = (jl_value_t*)jl_datatype_type;
    jl_typename_type = jl_new_uninitialized_datatype(4);
    jl_sym_type = jl_new_uninitialized_datatype(0);
    jl_symbol_type = jl_sym_type;

    jl_tuple_type = jl_alloc_tuple(1);
    jl_tuple_type->type = (jl_value_t*)jl_tuple_type;

    jl_null = (jl_tuple_t*)newobj((jl_value_t*)jl_tuple_type, 1);
    jl_tuple_set_len_unsafe(jl_null, 0);
    jl_nothing = (jl_value_t*)jl_null; // for bootstrapping

    jl_any_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Any"), NULL, jl_null);
    jl_any_type->super = jl_any_type;
    jl_type_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Type"), jl_any_type, jl_null);

    // initialize them. lots of cycles.
    jl_datatype_type->name = jl_new_typename(jl_symbol("DataType"));
    jl_datatype_type->name->primary = (jl_value_t*)jl_datatype_type;
    jl_datatype_type->super = jl_type_type;
    jl_datatype_type->parameters = jl_null;
    jl_datatype_type->names = jl_tuple(14, jl_symbol("fptr"),
                                       jl_symbol("env"),
                                       jl_symbol("code"),
                                       jl_symbol("name"),
                                       jl_symbol("super"),
                                       jl_symbol("parameters"),
                                       jl_symbol("names"),
                                       jl_symbol("types"),
                                       jl_symbol("ctor_factory"),
                                       jl_symbol("instance"),
                                       jl_symbol("size"),
                                       jl_symbol("abstract"),
                                       jl_symbol("mutable"),
                                       jl_symbol("pointerfree"));
    jl_datatype_type->types = jl_tuple(14, jl_any_type,jl_any_type,jl_any_type,
                                       jl_typename_type, jl_type_type,
                                       jl_tuple_type, jl_tuple_type,
                                       jl_tuple_type, jl_any_type, jl_any_type,
                                       jl_any_type, //types will be fixed later
                                       jl_any_type, jl_any_type, jl_any_type);
    jl_datatype_type->fptr = jl_f_no_function;
    jl_datatype_type->env = (jl_value_t*)jl_null;
    jl_datatype_type->linfo = NULL;
    jl_datatype_type->ctor_factory = NULL;
    jl_datatype_type->instance = NULL;
    jl_datatype_type->uid = jl_assign_type_uid();
    jl_datatype_type->struct_decl = NULL;
    jl_datatype_type->abstract = 0;
    jl_datatype_type->pointerfree = 0;
    // NOTE: types should not really be mutable, but the instance and
    // struct_decl fields are basically caches, which are mutated.
    jl_datatype_type->mutabl = 1;

    jl_typename_type->name = jl_new_typename(jl_symbol("TypeName"));
    jl_typename_type->name->primary = (jl_value_t*)jl_typename_type;
    jl_typename_type->super = jl_any_type;
    jl_typename_type->parameters = jl_null;
    jl_typename_type->names = jl_tuple(4, jl_symbol("name"),
                                       jl_symbol("module"),
                                       jl_symbol("primary"), jl_symbol(""));
    jl_typename_type->types = jl_tuple(4, jl_sym_type, jl_any_type,
                                       jl_type_type, jl_any_type);
    jl_typename_type->uid = jl_assign_type_uid();
    jl_typename_type->fptr = jl_f_no_function;
    jl_typename_type->env = (jl_value_t*)jl_null;
    jl_typename_type->linfo = NULL;
    jl_typename_type->ctor_factory = NULL;
    jl_typename_type->instance = NULL;
    jl_typename_type->struct_decl = NULL;
    jl_typename_type->abstract = 0;
    jl_typename_type->pointerfree = 0;
    jl_typename_type->mutabl = 1;

    jl_sym_type->name = jl_new_typename(jl_symbol("Symbol"));
    jl_sym_type->name->primary = (jl_value_t*)jl_sym_type;
    jl_sym_type->super = jl_any_type;
    jl_sym_type->parameters = jl_null;
    jl_sym_type->names = jl_null;
    jl_sym_type->types = jl_null;
    jl_sym_type->fptr = jl_f_no_function;
    jl_sym_type->env = (jl_value_t*)jl_null;
    jl_sym_type->linfo = NULL;
    jl_sym_type->ctor_factory = NULL;
    jl_sym_type->instance = NULL;
    jl_sym_type->uid = jl_assign_type_uid();
    jl_sym_type->struct_decl = NULL;
    jl_sym_type->size = 0;
    jl_sym_type->abstract = 0;
    jl_sym_type->pointerfree = 0;
    jl_sym_type->mutabl = 1;

    // now they can be used to create the remaining base kinds and types
    jl_uniontype_type = jl_new_datatype(jl_symbol("UnionType"),
                                        jl_type_type, jl_null,
                                        jl_tuple(1, jl_symbol("types")),
                                        jl_tuple(1, jl_tuple_type),
                                        0, 0);
    jl_uniontype_type->fptr = jl_f_no_function;

    jl_bottom_type = (jl_value_t*)jl_new_struct(jl_uniontype_type, jl_null);

    jl_tvar_type = jl_new_datatype(jl_symbol("TypeVar"),
                                   jl_any_type, jl_null,
                                   jl_tuple(3, jl_symbol("name"),
                                            jl_symbol("lb"),
                                            jl_symbol("ub")),
                                   jl_tuple(3, jl_sym_type, jl_type_type,
                                            jl_type_type),
                                   0, 0);
    jl_tvar_type->fptr = jl_f_typevar;

    jl_undef_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Undef"),
                                        jl_any_type, jl_null);

    jl_top_type = jl_new_struct(jl_uniontype_type,
                                jl_tuple2(jl_any_type, jl_undef_type));

    jl_tvar_t *tttvar = jl_new_typevar(jl_symbol("T"),
                                       (jl_value_t*)jl_bottom_type,jl_top_type);
    jl_type_type->parameters = jl_tuple(1, tttvar);

    jl_tuple_t *tv;
    tv = jl_tuple1(tvar("T"));
    jl_vararg_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Vararg"),
                                         jl_any_type, tv);

    jl_tupleset(jl_tuple_type, 0,
                (jl_value_t*)jl_apply_type((jl_value_t*)jl_vararg_type,
                                           jl_tuple(1,jl_any_type)));

    tv = jl_tuple2(tvar("N"), tvar("T"));
    jl_ntuple_type = jl_new_abstracttype((jl_value_t*)jl_symbol("NTuple"),
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

    jl_bool_type = NULL;
    jl_bool_type = jl_new_bitstype((jl_value_t*)jl_symbol("Bool"),
                                   jl_any_type, jl_null, 8);
    jl_false = jl_box8(jl_bool_type, 0);
    jl_true  = jl_box8(jl_bool_type, 1);

    jl_method_type =
        jl_new_datatype(jl_symbol("Method"), jl_any_type, jl_null,
                        jl_tuple(6, jl_symbol("sig"), jl_symbol("va"),
                                 jl_symbol("tvars"), jl_symbol("func"),
                                 jl_symbol("invokes"), jl_symbol("next")),
                        jl_tuple(6, jl_tuple_type, jl_bool_type,
                                 jl_tuple_type, jl_any_type,
                                 jl_any_type, jl_any_type),
                        0, 1);
    jl_method_type->fptr = jl_f_no_function;

    jl_methtable_type =
        jl_new_datatype(jl_symbol("MethodTable"), jl_any_type, jl_null,
                        jl_tuple(6, jl_symbol("name"), jl_symbol("defs"),
                                 jl_symbol("cache"), jl_symbol("cache_arg1"),
                                 jl_symbol("cache_targ"),
                                 jl_symbol("max_args")),
                        jl_tuple(6, jl_sym_type, jl_any_type, jl_any_type,
                                 jl_any_type, jl_any_type, jl_long_type),
                        0, 1);
    jl_methtable_type->fptr = jl_f_no_function;

    tv = jl_tuple2(tvar("T"), tvar("N"));
    jl_abstractarray_type = jl_new_abstracttype((jl_value_t*)jl_symbol("AbstractArray"),
                                                jl_any_type, tv);

    tv = jl_tuple2(tvar("T"), tvar("N"));
    jl_array_type = 
        jl_new_datatype(jl_symbol("Array"),
                        (jl_datatype_t*)
                        jl_apply_type((jl_value_t*)jl_abstractarray_type, tv),
                        tv,
                        jl_null, jl_null, 0, 1);
    jl_array_typename = jl_array_type->name;
    jl_array_type->linfo = NULL;
    jl_array_type->pointerfree = 0;
    jl_initialize_generic_function((jl_function_t*)jl_array_type,
                                   jl_array_typename->name);

    jl_array_any_type =
        (jl_value_t*)jl_apply_type((jl_value_t*)jl_array_type,
                                   jl_tuple(2, jl_any_type,
                                            jl_box_long(1)));
    
    jl_array_symbol_type =
        (jl_value_t*)jl_apply_type((jl_value_t*)jl_array_type,
                                   jl_tuple(2, jl_symbol_type,
                                            jl_box_long(1)));
    
    jl_expr_type =
        jl_new_datatype(jl_symbol("Expr"),
                        jl_any_type, jl_null,
                        jl_tuple(3, jl_symbol("head"), jl_symbol("args"),
                                 jl_symbol("typ")),
                        jl_tuple(3, jl_sym_type, jl_array_any_type,
                                 jl_any_type),
                        0, 1);
    jl_expr_type->fptr = jl_f_new_expr;

    jl_linenumbernode_type =
        jl_new_datatype(jl_symbol("LineNumberNode"), jl_any_type, jl_null,
                        jl_tuple(1, jl_symbol("line")),
                        jl_tuple(1, jl_long_type), 0, 0);

    jl_labelnode_type =
        jl_new_datatype(jl_symbol("LabelNode"), jl_any_type, jl_null,
                        jl_tuple(1, jl_symbol("label")),
                        jl_tuple(1, jl_long_type), 0, 0);

    jl_gotonode_type =
        jl_new_datatype(jl_symbol("GotoNode"), jl_any_type, jl_null,
                        jl_tuple(1, jl_symbol("label")),
                        jl_tuple(1, jl_long_type), 0, 0);

    jl_quotenode_type =
        jl_new_datatype(jl_symbol("QuoteNode"), jl_any_type, jl_null,
                        jl_tuple(1, jl_symbol("value")),
                        jl_tuple(1, jl_any_type), 0, 0);

    jl_topnode_type =
        jl_new_datatype(jl_symbol("TopNode"), jl_any_type, jl_null,
                        jl_tuple(1, jl_symbol("name")),
                        jl_tuple(1, jl_sym_type), 0, 0);

    jl_module_type =
        jl_new_datatype(jl_symbol("Module"), jl_any_type, jl_null,
                        jl_tuple(2, jl_symbol("name"), jl_symbol("parent")),
                        jl_tuple(2, jl_sym_type, jl_any_type), 0, 1);

    jl_tupleset(jl_typename_type->types, 1, jl_module_type);

    jl_lambda_info_type =
        jl_new_datatype(jl_symbol("LambdaStaticData"),
                        jl_any_type, jl_null,
                        jl_tuple(14, jl_symbol("ast"), jl_symbol("sparams"),
                                 jl_symbol("tfunc"), jl_symbol("name"),
                                 jl_symbol("roots"),
                                 /* jl_symbol("specTypes"),
                                    jl_symbol("unspecialized"),
                                    jl_symbol("specializations")*/
                                 jl_symbol(""), jl_symbol(""), jl_symbol(""),
                                 jl_symbol("module"), jl_symbol("def"),
                                 jl_symbol("capt"),
                                 jl_symbol("file"), jl_symbol("line"),
                                 jl_symbol("inferred")),
                        jl_tuple(14, jl_any_type, jl_tuple_type,
                                 jl_any_type, jl_sym_type,
                                 jl_any_type, jl_tuple_type,
                                 jl_any_type, jl_array_any_type,
                                 jl_module_type, jl_any_type,
                                 jl_any_type,
                                 jl_sym_type, jl_int32_type,
                                 jl_bool_type),
                        0, 1);
    jl_lambda_info_type->fptr = jl_f_no_function;

    jl_box_type =
        jl_new_datatype(jl_symbol("Box"),
                        jl_any_type, jl_null,
                        jl_tuple(1, jl_symbol("contents")),
                        jl_tuple(1, jl_any_type), 0, 1);
    jl_box_type->fptr = jl_f_new_box;
    jl_box_typename = jl_box_type->name;
    jl_box_any_type = (jl_value_t*)jl_box_type;

    jl_typector_type =
        jl_new_datatype(jl_symbol("TypeConstructor"),
                        jl_type_type, jl_null,
                        jl_tuple(2, jl_symbol("parameters"),
                                 jl_symbol("body")),
                        jl_tuple(2, jl_tuple_type, jl_any_type),
                        0, 0);
    jl_typector_type->fptr = jl_f_new_type_constructor;

    jl_function_type =
        jl_new_datatype(jl_symbol("Function"), jl_any_type, jl_null,
                        jl_tuple(3, jl_symbol("fptr"), jl_symbol("env"),
                                 jl_symbol("code")),
                        jl_tuple(3, jl_any_type, jl_any_type,
                                 jl_lambda_info_type),
                        0, 1);
    jl_function_type->fptr = jl_f_no_function;

    jl_tupleset(jl_method_type->types, 3, jl_function_type);
    jl_tupleset(jl_lambda_info_type->types, 6, jl_function_type);

    jl_bottom_func = jl_new_closure(jl_f_no_function, JL_NULL, NULL);

    jl_intrinsic_type = jl_new_bitstype((jl_value_t*)jl_symbol("IntrinsicFunction"),
                                        jl_any_type, jl_null, 32);

    tv = jl_tuple1(tvar("T"));
    jl_pointer_type =
        jl_new_bitstype((jl_value_t*)jl_symbol("Ptr"), jl_any_type, tv,
                        sizeof(void*)*8);

    // Type{T}
    jl_typetype_tvar = jl_new_typevar(jl_symbol("T"),
                                      (jl_value_t*)jl_bottom_type,
                                      jl_top_type);
    jl_typetype_type = (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_type_type,
                                                     jl_tuple(1,jl_typetype_tvar));

    jl_ANY_flag = (jl_value_t*)tvar("ANY");

    // complete builtin type metadata
    jl_value_t *pointer_void = jl_apply_type((jl_value_t*)jl_pointer_type,
                                             jl_tuple(1,jl_bottom_type));
    jl_voidpointer_type = (jl_datatype_t*)pointer_void;
    jl_tupleset(jl_datatype_type->types, 0, pointer_void);
    jl_tupleset(jl_datatype_type->types, 10, jl_int32_type);
    jl_tupleset(jl_datatype_type->types, 11, (jl_value_t*)jl_bool_type);
    jl_tupleset(jl_datatype_type->types, 12, (jl_value_t*)jl_bool_type);
    jl_tupleset(jl_datatype_type->types, 13, (jl_value_t*)jl_bool_type);
    jl_tupleset(jl_function_type->types, 0, pointer_void);

    jl_compute_field_offsets(jl_datatype_type);
    jl_compute_field_offsets(jl_typename_type);
    jl_compute_field_offsets(jl_uniontype_type);
    jl_compute_field_offsets(jl_tvar_type);
    jl_compute_field_offsets(jl_method_type);
    jl_compute_field_offsets(jl_methtable_type);
    jl_compute_field_offsets(jl_expr_type);
    jl_compute_field_offsets(jl_linenumbernode_type);
    jl_compute_field_offsets(jl_labelnode_type);
    jl_compute_field_offsets(jl_gotonode_type);
    jl_compute_field_offsets(jl_quotenode_type);
    jl_compute_field_offsets(jl_topnode_type);
    jl_compute_field_offsets(jl_module_type);
    jl_compute_field_offsets(jl_lambda_info_type);
    jl_compute_field_offsets(jl_box_type);
    jl_compute_field_offsets(jl_typector_type);
    jl_compute_field_offsets(jl_function_type);

    call_sym = jl_symbol("call");
    call1_sym = jl_symbol("call1");
    quote_sym = jl_symbol("quote");
    top_sym = jl_symbol("top");
    dots_sym = jl_symbol("Vararg");
    line_sym = jl_symbol("line");
    jl_continue_sym = jl_symbol("continue");
    error_sym = jl_symbol("error");
    goto_sym = jl_symbol("goto");
    goto_ifnot_sym = jl_symbol("gotoifnot");
    label_sym = jl_symbol("label");
    return_sym = jl_symbol("return");
    lambda_sym = jl_symbol("lambda");
    macro_sym = jl_symbol("macro");
    module_sym = jl_symbol("module");
    export_sym = jl_symbol("export");
    import_sym = jl_symbol("import");
    using_sym = jl_symbol("using");
    importall_sym = jl_symbol("importall");
    assign_sym = jl_symbol("=");
    null_sym = jl_symbol("null");
    body_sym = jl_symbol("body");
    colons_sym = jl_symbol("::");
    method_sym = jl_symbol("method");
    exc_sym = jl_symbol("the_exception");
    enter_sym = jl_symbol("enter");
    leave_sym = jl_symbol("leave");
    static_typeof_sym = jl_symbol("static_typeof");
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
    type_goto_sym = jl_symbol("type_goto");
    toplevel_sym = jl_symbol("toplevel");
    tuple_sym = jl_symbol("tuple");
}
