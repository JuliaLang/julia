/*
  Types
  . type predicates (subtype) and type matching
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
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"
#include "newobj_internal.h"

jl_tag_type_t *jl_any_type;
jl_typector_t *jl_type_typector;
jl_tag_type_t *jl_type_type;
jl_struct_type_t *jl_typename_type;
jl_struct_type_t *jl_sym_type;
jl_tuple_t *jl_tuple_type;
jl_typename_t *jl_tuple_typename;
jl_typector_t *jl_ntuple_type;
jl_typename_t *jl_ntuple_typename;
jl_struct_type_t *jl_tvar_type;

jl_struct_type_t *jl_func_kind;
jl_struct_type_t *jl_union_kind;
jl_struct_type_t *jl_tag_kind;
jl_struct_type_t *jl_struct_kind;
jl_struct_type_t *jl_bits_kind;

jl_type_t *jl_bottom_type;
jl_typector_t *jl_seq_type;
jl_typector_t *jl_tensor_type;
jl_typector_t *jl_scalar_type;
jl_typector_t *jl_number_type;
jl_typector_t *jl_real_type;
jl_typector_t *jl_int_type;
jl_typector_t *jl_float_type;

jl_bits_type_t *jl_bool_type;
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

jl_func_type_t *jl_any_func;
jl_function_t *jl_bottom_func;
jl_function_t *jl_identity_func;
static jl_tvar_t *jl_wildcard_type;

// --- type properties and predicates ---

int jl_is_type(jl_value_t *v)
{
    if (jl_is_tuple(v)) {
        jl_tuple_t *t = (jl_tuple_t*)v;
        int i;
        for(i=0; i < t->length; i++) {
            jl_value_t *vv = jl_tupleref(t, i);
            if (!jl_is_typevar(vv) && !jl_is_type(vv) &&
                !jl_is_typector(vv))
                return 0;
        }
        return 1;
    }
    return (jl_typeis(v, jl_union_kind) ||
            jl_typeis(v, jl_struct_kind) ||
            jl_typeis(v, jl_func_kind) ||
            jl_typeis(v, jl_tag_kind) ||
            jl_typeis(v, jl_bits_kind));
}

jl_typename_t *jl_tname(jl_value_t *v)
{
    if (jl_is_tuple(v))
        return jl_tuple_typename;
    if (jl_is_some_tag_type(v))
        return ((jl_tag_type_t*)v)->name;
    if (jl_is_typector(v))
        return jl_tname((jl_value_t*)((jl_typector_t*)v)->body);
    return ((jl_tag_type_t*)(v->type))->name;
}

jl_tuple_t *jl_tparams(jl_value_t *v)
{
    if (jl_is_tuple(v))
        return (jl_tuple_t*)v;
    if (jl_is_some_tag_type(v))
        return ((jl_tag_type_t*)v)->parameters;
    if (jl_is_union_type(v))
        return ((jl_uniontype_t*)v)->types;
    return jl_null;
}

int jl_has_typevars(jl_value_t *v)
{
    size_t i;
    if (jl_typeis(v, jl_tvar_type))
        return 1;
    if (jl_is_tuple(v)) {
        jl_tuple_t *t = (jl_tuple_t*)v;
        for(i=0; i < t->length; i++) {
            if (jl_has_typevars(jl_tupleref(t, i)))
                return 1;
        }
        return 0;
    }
    if (jl_is_func_type(v))
        return jl_has_typevars((jl_value_t*)((jl_func_type_t*)v)->from) ||
            jl_has_typevars((jl_value_t*)((jl_func_type_t*)v)->to);

    jl_tuple_t *t = jl_tparams(v);
    for(i=0; i < t->length; i++) {
        jl_value_t *elt = jl_tupleref(t, i);
        if (elt != v) {
            if (jl_has_typevars(elt))
                return 1;
        }
    }
    // probably not necessary; no reason to use match() instead of subtype()
    // on the unconstrained version of a type
    //if (jl_is_typector(v))
    //    return (((jl_typector_t*)v)->parameters->length > 0);
    return 0;
}

// construct the full type of a value, possibly making a tuple type
jl_value_t *jl_full_type(jl_value_t *v)
{
    if (!jl_is_tuple(v))
        return (jl_value_t*)jl_typeof(v);
    jl_tuple_t *in = (jl_tuple_t*)v;
    jl_tuple_t *out = jl_alloc_tuple(in->length);
    size_t i;
    for(i=0; i < in->length; i++) {
        jl_tupleset(out, i, jl_full_type(jl_tupleref(in, i)));
    }
    return (jl_value_t*)out;
}

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

jl_tuple_t *jl_compute_type_union(jl_tuple_t *types)
{
    size_t n = count_union_components(types);
    jl_value_t **temp = alloca(n * sizeof(jl_value_t*));
    size_t idx=0;
    flatten_type_union(types, temp, &idx);
    assert(idx == n);
    size_t i, j, ndel=0;
    for(i=0; i < n; i++) {
        for(j=0; j < n; j++) {
            if (j != i && temp[i] && temp[j]) {
                if (temp[i] == temp[j] ||
                    (jl_subtype(temp[i], temp[j], 0) &&
                     (temp[i] == (jl_value_t*)jl_bottom_type ||
                      !jl_has_typevars(temp[j])))) {
                    temp[i] = NULL;
                    ndel++;
                }
            }
        }
    }
    jl_tuple_t *result = jl_alloc_tuple(n - ndel);
    j=0;
    for(i=0; i < n; i++) {
        if (temp[i] != NULL) {
            jl_tupleset(result, j, temp[i]);
            j++;
        }
    }
    assert(j == n-ndel);
    return result;
}

jl_value_t *jl_type_union(jl_tuple_t *types)
{
    types = jl_compute_type_union(types);
    if (types->length == 1)
        return jl_tupleref(types, 0);
    if (types->length == 0)
        return (jl_value_t*)jl_bottom_type;
    return (jl_value_t*)jl_new_uniontype(types);
}

jl_value_t *jl_type_intersection(jl_value_t *a, jl_value_t *b);

static jl_value_t *intersect_union(jl_uniontype_t *a, jl_value_t *b)
{
    jl_tuple_t *t = jl_alloc_tuple(a->types->length);
    size_t i;
    for(i=0; i < t->length; i++) {
        jl_tupleset(t, i, jl_type_intersection(jl_tupleref(a->types,i),b));
    }
    return jl_type_union(t);
}

// if returns with *bot!=0, then intersection is Bottom
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

static jl_value_t *intersect_tuple(jl_tuple_t *a, jl_tuple_t *b)
{
    size_t al = a->length;
    size_t bl = b->length;
    int bot=0;
    size_t n = tuple_intersect_size(a, b, &bot);
    if (bot)
        return (jl_value_t*)jl_bottom_type;
    if (n == 0) return (jl_value_t*)jl_null;
    jl_tuple_t *tc = jl_alloc_tuple(n);
    size_t ai=0, bi=0, ci;
    jl_value_t *ae, *be;
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
        jl_value_t *ce = jl_type_intersection(ae,be);
        if (ce == (jl_value_t*)jl_bottom_type)
            return (jl_value_t*)jl_bottom_type;
        if (aseq && bseq) {
            ce = (jl_value_t*)jl_apply_type_ctor(jl_seq_type, jl_tuple(1, ce));
        }
        jl_tupleset(tc, ci, ce);
    }
    return (jl_value_t*)tc;
}

static jl_value_t *intersect_tag(jl_tag_type_t *a, jl_tag_type_t *b)
{
    assert(a->name == b->name);
    assert(a->parameters->length == b->parameters->length);
    jl_tuple_t *p = jl_alloc_tuple(a->parameters->length);
    size_t i;
    for(i=0; i < p->length; i++) {
        jl_value_t *ti = jl_type_intersection(jl_tupleref(a->parameters,i),
                                              jl_tupleref(b->parameters,i));
        if (ti == (jl_value_t*)jl_bottom_type)
            return (jl_value_t*)jl_bottom_type;
        jl_tupleset(p, i, ti);
    }
    if (a->name->ctor != NULL)
        return (jl_value_t*)jl_apply_type_ctor(a->name->ctor, p);
    assert(0 && "not yet implemented");
    return NULL;
}

jl_value_t *jl_type_intersection(jl_value_t *a, jl_value_t *b)
{
    if (jl_type_match((jl_type_t*)a, (jl_type_t*)b) != jl_false)
        return a;
    if (jl_type_match((jl_type_t*)b, (jl_type_t*)a) != jl_false)
        return b;
    if (jl_is_int32(a) || jl_is_int32(b))
        return (jl_value_t*)jl_bottom_type;
    if (jl_is_typector(a))
        a = jl_unconstrained_type((jl_typector_t*)a);
    if (jl_is_typector(b))
        b = jl_unconstrained_type((jl_typector_t*)b);
    // union
    if (jl_is_union_type(a))
        return intersect_union((jl_uniontype_t*)a, b);
    if (jl_is_union_type(b))
        return intersect_union((jl_uniontype_t*)b, a);
    // tuple
    if (jl_is_tuple(a)) {
        if (!jl_is_tuple(b))
            return (jl_value_t*)jl_bottom_type;
        return intersect_tuple((jl_tuple_t*)a, (jl_tuple_t*)b);
    }
    if (jl_is_tuple(b))
        return (jl_value_t*)jl_bottom_type;
    // function
    if (jl_is_func_type(a)) {
        if (!jl_is_func_type(b))
            return (jl_value_t*)jl_bottom_type;
        return
            (jl_value_t*)jl_new_functype((jl_type_t*)
                                         jl_type_union(jl_tuple(2,((jl_func_type_t*)a)->from,
                                                                ((jl_func_type_t*)b)->from)),
                                         (jl_type_t*)
                                         jl_type_intersection((jl_value_t*)((jl_func_type_t*)a)->to,
                                                              (jl_value_t*)((jl_func_type_t*)b)->to));
    }
    if (jl_is_func_type(b))
        return (jl_value_t*)jl_bottom_type;
    // tag
    assert(jl_is_some_tag_type(a));
    assert(jl_is_some_tag_type(b));
    jl_tag_type_t *tta = (jl_tag_type_t*)a;
    jl_tag_type_t *ttb = (jl_tag_type_t*)b;
    if (tta->name == ttb->name)
        return (jl_value_t*)intersect_tag(tta, ttb);
    jl_tag_type_t *super = NULL;
    jl_tag_type_t *sub = NULL;
    jl_value_t *ti;
    while (tta != jl_any_type) {
        if (tta->name == ttb->name) {
            ti = intersect_tag(tta, ttb);
            if (ti == (jl_value_t*)jl_bottom_type) return ti;
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
                ti = intersect_tag(tta, ttb);
                if (ti == (jl_value_t*)jl_bottom_type) return ti;
                super = (jl_tag_type_t*)ti;
                sub = (jl_tag_type_t*)b;
                break;
            }
            ttb = ttb->super;
        }
    }
    if (super == NULL)
        return (jl_value_t*)jl_bottom_type;

    size_t n = sub->parameters->length;
    if (n == 0) return (jl_value_t*)sub;

    assert(sub->name->ctor != NULL);
    jl_typector_t *tc = sub->name->ctor;
    // compute what constraints the supertype imposes on the subtype
    jl_tuple_t *sup_params =
        ((jl_tag_type_t*)((jl_tag_type_t*)tc->body)->super)->parameters;
    // match the intersected supertype against the pattern this subtype
    // uses to instantiate its supertype. this tells use that the
    // intersected supertype cannot come from this subtype (in which case
    // our final answer is Bottom), or it tells us what subtype parameter
    // values are implied by the intersected supertype.
    jl_value_t *env = jl_type_match((jl_type_t*)super->parameters,
                                    (jl_type_t*)sup_params);
    if (env == jl_false)
        return (jl_value_t*)jl_bottom_type;

    jl_tuple_t *p = jl_alloc_tuple(n);
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *tp = jl_tupleref(tc->parameters, i);
        jl_value_t *elt = jl_tupleref(sub->parameters, i);
        jl_value_t *e = env;
        while (e != (jl_value_t*)jl_null) {
            if (jl_t0(e) == tp) {
                elt = jl_type_intersection(elt, jl_t1(e));
                break;
            }
            e = jl_nextpair(e);
        }
        jl_tupleset(p, i, elt);
    }
    return (jl_value_t*)jl_apply_type_ctor(tc, p);
}

// --- type instantiation and cache ---

static int extensionally_same_type(jl_value_t *a, jl_value_t *b)
{
    return (jl_subtype(a, b, 0) && jl_subtype(b, a, 0));
}

typedef struct _jl_value_pair_t {
    jl_value_t *a;
    jl_value_t *b;
    struct _jl_value_pair_t *next;
} jl_value_pair_t;

static int type_eqv_(jl_value_t *a, jl_value_t *b, jl_value_pair_t *stack)
{
    jl_value_pair_t top;
    if (a == b) return 1;
    if (jl_is_typevar(a) || jl_is_typevar(b)) return 0;
    if (jl_is_int32(a) && jl_is_int32(b))
        return (jl_unbox_int32(a) == jl_unbox_int32(b));
    if ((jl_is_tuple(a) && jl_is_tuple(b)) ||
        (jl_is_union_type(a) && jl_is_union_type(b))) {
        return extensionally_same_type(a, b);
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
    if (jl_tname(a) != jl_tname(b)) return 0;
    jl_tuple_t *ap = jl_tparams(a);
    jl_tuple_t *bp = jl_tparams(b);
    if (ap->length != bp->length) return 0;
    size_t i;
    for(i=0; i < ap->length; i++) {
        if (!type_eqv_(jl_tupleref(ap,i), jl_tupleref(bp,i), stack))
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
    jl_value_t *env = jl_type_match((jl_type_t*)a, (jl_type_t*)b);
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

static jl_type_t *apply_type_ctor_(jl_typector_t *tc, jl_value_t **params,
                                   size_t n)
{
    if (n == 0) return (jl_type_t*)jl_unconstrained_type(tc);
    size_t i;
    char *tname = jl_tname((jl_value_t*)tc)->name->name;
    for(i=0; i < n; i++) {
        jl_value_t *pi = params[i];
        if (!jl_is_typector(pi) && !jl_is_type(pi) && !jl_is_int32(pi) &&
            !jl_is_typevar(pi))
            jl_errorf("invalid parameter for type %s", tname);
    }
    if (tc == jl_ntuple_type && (n==1||n==2) && jl_is_int32(params[0])) {
        size_t nt = jl_unbox_int32(params[0]);
        if (nt == 0) return (jl_type_t*)jl_null;
        if (n==2) {
            jl_tuple_t *tup = jl_alloc_tuple(nt);
            jl_value_t *eltype = params[1];
            for(i=0; i < nt; i++) {
                jl_tupleset(tup, i, eltype);
            }
            return (jl_type_t*)tup;
        }
    }
    jl_tuple_t *tp = tc->parameters;
    if (n > tp->length)
        jl_errorf("too many parameters for type %s", tname);
    jl_value_t **env = alloca(2 * tp->length * sizeof(jl_value_t*));
    for(i=0; i < tp->length; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_tupleref(tp,i);
        assert(jl_is_typevar(tv));
        env[i*2+0] = (jl_value_t*)tv;
        if (i >= n) {
            env[i*2+1] = (jl_value_t*)jl_wildcard_type;
        }
        else {
            env[i*2+1] = params[i];
        }
    }
    return jl_instantiate_type_with(tc->body, env, tp->length);
}

jl_type_t *jl_apply_type_ctor(jl_typector_t *tc, jl_tuple_t *params)
{
    return apply_type_ctor_(tc, &jl_tupleref(params,0), params->length);
}

typedef struct _typekey_stack_t {
    jl_value_t **key;
    size_t n;  // key length
    jl_type_t *type;
    struct _typekey_stack_t *next;
} typekey_stack_t;

static jl_type_t *lookup_type(typekey_stack_t *table, jl_value_t **key,
                              size_t n)
{
    assert(n > 0);
    while (table != NULL) {
        assert(table->n > 0);
        if (table->n == n && table->key[0] == key[0]) {
            size_t i;
            for(i=1; i < n; i++) {
                if (!jl_types_equal(table->key[i], key[i]))
                    break;
            }
            if (i==n) return table->type;
        }
        table = table->next;
    }
    return NULL;
}

static int t_uid_ctr = 1;  // TODO: lock

int jl_assign_type_uid()
{
    return t_uid_ctr++;
}

// TODO: synchronize
// TODO: convert to hash table
static typekey_stack_t *Type_Cache = NULL;
static void cache_type_(jl_value_t **key, size_t n, jl_type_t *type)
{
    // only cache concrete types
    if (jl_has_typevars((jl_value_t*)type))
        return;
    // assign uid
    if (jl_is_struct_type(type))
        ((jl_struct_type_t*)type)->uid = t_uid_ctr++;
    else if (jl_is_bits_type(type)) 
        ((jl_bits_type_t*)type)->uid = t_uid_ctr++;
    typekey_stack_t *tk = (typekey_stack_t*)allocb(sizeof(typekey_stack_t));
    tk->key = (jl_value_t**)allocb(n * sizeof(void*));
    size_t i;
    for(i=0; i < n; i++) tk->key[i] = key[i];
    tk->n = n;
    tk->type = type;
    tk->next = Type_Cache;
    Type_Cache = tk;
}

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
        for(i=0; i < p->length; i++) {
            jl_tupleset(nt, i, (jl_value_t*)inst_type_w_(jl_tupleref(p,i), env, n, stack));
        }
        return (jl_type_t*)nt;
    }
    if (jl_is_union_type(t)) {
        jl_tuple_t *tw = (jl_tuple_t*)inst_type_w_((jl_value_t*)((jl_uniontype_t*)t)->types,
                                                   env, n, stack);
        return (jl_type_t*)jl_new_uniontype(tw);
    }
    if (jl_is_func_type(t)) {
        jl_func_type_t *ft = (jl_func_type_t*)t;
        return (jl_type_t*)jl_new_functype(inst_type_w_((jl_value_t*)ft->from, env, n, stack),
                                           inst_type_w_((jl_value_t*)ft->to  , env, n, stack));
    }
    if (jl_is_some_tag_type(t)) {
        jl_tag_type_t *tt = (jl_tag_type_t*)t;
        jl_tuple_t *tp = tt->parameters;
        if (jl_is_null(tp))
            return (jl_type_t*)t;
        size_t ntp = tp->length;
        jl_value_t **iparams = (jl_value_t**)alloca((ntp+1) * sizeof(void*));
        for(i=0; i < ntp; i++) {
            jl_value_t *elt = jl_tupleref(tp, i);
            if (elt == t)
                iparams[i+1] = t;
            else
                iparams[i+1] = (jl_value_t*)inst_type_w_(elt, env, n, stack);
        }
        jl_typename_t *tn = jl_tname(t);
        iparams[0] = (jl_value_t*)tn;

        // if an identical instantiation is already in process somewhere
        // up the stack, return it. this computes a fixed point for
        // recursive types.
        jl_type_t *lkup = lookup_type(stack, iparams, ntp+1);
        if (lkup != NULL) return lkup;

        // check type cache
        lkup = lookup_type(Type_Cache, iparams, ntp+1);
        if (lkup != NULL) return lkup;

        // always use original type constructor
        jl_typector_t *tc = tn->ctor;
        if (tc != NULL && tc->body != (jl_type_t*)t)
            return apply_type_ctor_(tc, &iparams[1], ntp);

        // move array of instantiated parameters to heap; we need to keep it
        jl_tuple_t *iparams_tuple = jl_alloc_tuple(ntp);
        for(i=0; i < ntp; i++)
            jl_tupleset(iparams_tuple, i, iparams[i+1]);
        if (jl_is_tag_type(t)) {
            jl_tag_type_t *tagt = (jl_tag_type_t*)t;
            jl_tag_type_t *ntt =
                (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAG_TYPE_NW);
            top.key = iparams;
            top.n = ntp+1;
            top.type = (jl_type_t*)ntt;
            top.next = stack;
            stack = &top;
            ntt->name = tn;
            ntt->super = (jl_tag_type_t*)inst_type_w_((jl_value_t*)tagt->super,env,n,stack);
            ntt->parameters = iparams_tuple;
            return (jl_type_t*)ntt;
        }
        else if (jl_is_bits_type(t)) {
            jl_bits_type_t *bitst = (jl_bits_type_t*)t;
            jl_bits_type_t *nbt =
                (jl_bits_type_t*)newobj((jl_type_t*)jl_bits_kind, BITS_TYPE_NW);
            top.key = iparams;
            top.n = ntp+1;
            top.type = (jl_type_t*)nbt;
            top.next = stack;
            stack = &top;
            nbt->name = tn;
            nbt->super = (jl_tag_type_t*)inst_type_w_((jl_value_t*)bitst->super, env, n, stack);
            nbt->parameters = iparams_tuple;
            nbt->nbits = bitst->nbits;
            cache_type_(iparams, ntp+1, (jl_type_t*)nbt);
            return (jl_type_t*)nbt;
        }
        else {
            assert(jl_is_struct_type(t));
            jl_struct_type_t *st = (jl_struct_type_t*)t;
            // create and initialize new struct type
            jl_struct_type_t *nst =
                (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind,
                                          STRUCT_TYPE_NW);
            // associate these parameters with the new struct type on
            // the stack, in case one of its field types references it.
            top.key = iparams;
            top.n = ntp+1;
            top.type = (jl_type_t*)nst;
            top.next = stack;
            stack = &top;
            nst->name = tn;
            nst->super = (jl_tag_type_t*)inst_type_w_((jl_value_t*)st->super, env,n,stack);
            nst->parameters = iparams_tuple;
            nst->names = st->names;
            nst->types = jl_null; // to be filled in below
            nst->fnew = jl_new_closure(st->fnew->fptr, (jl_value_t*)nst);
            nst->uid = 0;
            jl_tuple_t *ftypes = st->types;
            if (ftypes != NULL) {
                // recursively instantiate the types of the fields
                jl_tuple_t *nftypes = jl_alloc_tuple(ftypes->length);
                for(i=0; i < ftypes->length; i++) {
                    jl_tupleset(nftypes, i,
                                (jl_value_t*)inst_type_w_(jl_tupleref(ftypes,i),
                                                          env,n,stack));
                }
                nst->types = nftypes;
            }
            cache_type_(iparams, ntp+1, (jl_type_t*)nst);
            return (jl_type_t*)nst;
        }
    }
    return (jl_type_t*)t;
}

jl_type_t *jl_instantiate_type_with(jl_type_t *t, jl_value_t **env, size_t n)
{
    return inst_type_w_((jl_value_t*)t, env, n, NULL);
}

int jl_subtype_le(jl_value_t *a, jl_value_t *b, int ta, int morespecific);

int jl_tuple_subtype(jl_value_t **child, size_t cl,
                     jl_value_t **parent, size_t pl, int ta, int morespecific)
{
    size_t ci=0, pi=0;
    while(1) {
        int cseq = !ta && (ci<cl) && jl_is_seq_type(child[ci]);
        int pseq = (pi<pl) && jl_is_seq_type(parent[pi]);
        if (ci >= cl)
            return (pi>=pl || pseq);
        if (cseq && !pseq)
            return 0;
        if (pi >= pl)
            return 0;
        jl_value_t *ce = child[ci];
        jl_value_t *pe = parent[pi];
        if (cseq) ce = jl_tparam0(ce);
        if (pseq) pe = jl_tparam0(pe);

        if (!jl_subtype_le(ce, pe, ta, morespecific))
            return 0;

        if (cseq && pseq) return 1;
        if (!cseq) ci++;
        if (!pseq) pi++;
    }
    return 0;
}

static int tuple_all_subtype(jl_tuple_t *t, jl_value_t *super,
                             int ta, int morespecific)
{
    size_t ci;
    for(ci=0; ci < t->length; ci++) {
        jl_value_t *ce = jl_tupleref(t,ci);
        if (!ta && jl_is_seq_type(ce))
            ce = jl_tparam0(ce);
        if (!jl_subtype_le(ce, super, ta, morespecific))
            return 0;
    }
    return 1;
}

// compute the unconstrained version of a type constructor
jl_value_t *jl_unconstrained_type(jl_typector_t *tc)
{
    if (tc->unconstrained == NULL) {
        jl_tuple_t *p = jl_alloc_tuple(tc->parameters->length);
        size_t i;
        for(i=0; i < p->length; i++)
            jl_tupleset(p, i, (jl_value_t*)jl_wildcard_type);
        tc->unconstrained = jl_apply_type_ctor(tc, p);
    }
    return (jl_value_t*)tc->unconstrained;
}

/*
  ta specifies whether typeof() should be implicitly applied to a.
  this is used for tuple types to avoid allocating them explicitly.
  morespecific means we only care whether a is more specific than b,
  not necessarily a strict subtype
*/
int jl_subtype_le(jl_value_t *a, jl_value_t *b, int ta, int morespecific)
{
    if (!ta&&jl_is_typector(a)) a = jl_unconstrained_type((jl_typector_t*)a);
    if (jl_is_typector(b)) b = jl_unconstrained_type((jl_typector_t*)b);
    if (ta) {
        if (jl_is_tag_type(b) &&
            ((jl_tag_type_t*)b)->name == jl_type_type->name) {
            if (jl_is_type(a) || jl_is_typector(a))
                return jl_subtype_le(a, jl_tparam0(b), 0, morespecific);
        }
    }
    size_t i;
    if (jl_is_tuple(a)) {
        if ((jl_tuple_t*)b == jl_tuple_type) return 1;
        if (jl_is_tag_type(b) &&
            ((jl_tag_type_t*)b)->name == jl_ntuple_typename) {
            jl_tuple_t *tp = ((jl_tag_type_t*)b)->parameters;
            return tuple_all_subtype((jl_tuple_t*)a,
                                     jl_tupleref(tp,1), ta, morespecific);
        }
        if (jl_is_tuple(b)) {
            return jl_tuple_subtype(&jl_tupleref(a,0), ((jl_tuple_t*)a)->length,
                                    &jl_tupleref(b,0), ((jl_tuple_t*)b)->length,
                                    ta, morespecific);
        }
    }

    if (jl_is_union_type(b)) {
        // Bottom <: Bottom
        if (!ta && a == b && b==(jl_value_t*)jl_bottom_type) return 1;
        jl_tuple_t *bp = ((jl_uniontype_t*)b)->types;
        for(i=0; i < bp->length; i++) {
            if (jl_subtype_le(a, jl_tupleref(bp,i), ta, morespecific))
                return 1;
        }
        return 0;
    }

    if (ta) a = (jl_value_t*)jl_typeof(a);

    if (jl_is_union_type(a)) {
        jl_tuple_t *ap = ((jl_uniontype_t*)a)->types;
        for(i=0; i < ap->length; i++) {
            if (!jl_subtype_le(jl_tupleref(ap,i), b, 0, morespecific))
                return 0;
        }
        return 1;
    }

    if ((jl_tag_type_t*)b == jl_any_type) return 1;
    if (a == b) return 1;
    if (jl_is_typevar(a)) return 0;
    if (jl_is_typevar(b)) return 1;
    if ((jl_tag_type_t*)a == jl_any_type) return 0;
    if (jl_is_tuple(b)) {
        if (jl_is_tag_type(a) &&
            ((jl_tag_type_t*)a)->name == jl_ntuple_typename) {
            // only ((T:>S)...,) can be a supertype of NTuple[N,S]
            jl_tuple_t *tp = (jl_tuple_t*)b;
            jl_value_t *ntp = jl_tupleref(((jl_tag_type_t*)a)->parameters, 1);
            if (tp->length == 1 && jl_is_seq_type(jl_tupleref(tp,0))) {
                return jl_subtype_le(ntp, jl_tparam0(jl_tupleref(tp,0)),
                                     0, morespecific);
            }
        }
        return 0;
    }
    if (jl_is_tuple(a)) return 0;

    if (jl_is_int32(a) && jl_is_int32(b))
        return (jl_unbox_int32(a)==jl_unbox_int32(b));

    if (jl_is_func_type(a)) {
        if (jl_is_func_type(b)) {
            jl_func_type_t *fa = (jl_func_type_t*)a;
            jl_func_type_t *fb = (jl_func_type_t*)b;
            return ( (jl_is_typevar(fb->from) ||
                      jl_subtype_le((jl_value_t*)fb->from,
                                    (jl_value_t*)fa->from, 0, morespecific)) &&
                      jl_subtype_le((jl_value_t*)fa->to,
                                    (jl_value_t*)fb->to,   0, morespecific) );
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
            if (super && morespecific)
                return 1;
            if (tta->name == jl_ntuple_typename) {
                // NTuple must be covariant
                return jl_subtype_le(jl_tupleref(tta->parameters,1),
                                     jl_tupleref(ttb->parameters,1),
                                     0, morespecific);
            }
            assert(tta->parameters->length == ttb->parameters->length);
            for(i=0; i < tta->parameters->length; i++) {
                jl_value_t *apara = jl_tupleref(tta->parameters,i);
                jl_value_t *bpara = jl_tupleref(ttb->parameters,i);
                if (jl_is_typevar(bpara))
                    continue;
                if (jl_is_tag_type(ttb)) {
                    // allow abstract types to be covariant
                    if (!jl_subtype_le(apara, bpara, 0, morespecific))
                        goto check_Type;
                }
                else {
                    if (!jl_types_equal(apara, bpara))
                        goto check_Type;
                }
            }
            return 1;
        }
        tta = tta->super; super = 1;
    }
 check_Type:
    if (((jl_tag_type_t*)a)->name == jl_type_type->name) {
        // Type{T} matches either Type{:>T} or :>typeof(T)
        return jl_subtype_le(jl_tparam0(a), b, 1, morespecific);
    }

    return 0;
}

int jl_subtype(jl_value_t *a, jl_value_t *b, int ta)
{
    return jl_subtype_le(a, b, ta, 0);
}

int jl_type_morespecific(jl_value_t *a, jl_value_t *b, int ta)
{
    return jl_subtype_le(a, b, ta, 1);
}

static jl_value_t *type_match_(jl_type_t *child, jl_type_t *parent,
                               jl_tuple_t *env, int morespecific);

static jl_value_t *tuple_match(jl_tuple_t *child, jl_tuple_t *parent,
                               jl_tuple_t *env, int morespecific)
{
    size_t ci=0, pi=0;
    size_t cl = child->length;
    size_t pl = parent->length;
    while(1) {
        int cseq = (ci<cl) && jl_is_seq_type(jl_tupleref(child,ci));
        int pseq = (pi<pl) && jl_is_seq_type(jl_tupleref(parent,pi));
        if (ci >= cl)
            return (pi>=pl || pseq) ? (jl_value_t*)env : jl_false;
        if (cseq && !pseq)
            return jl_false;
        if (pi >= pl)
            return jl_false;
        jl_value_t *ce = jl_tupleref(child,ci);
        jl_value_t *pe = jl_tupleref(parent,pi);
        if (cseq) ce = jl_tparam0(ce);
        if (pseq) pe = jl_tparam0(pe);

        env = (jl_tuple_t*)type_match_((jl_type_t*)ce, (jl_type_t*)pe, env, morespecific);
        if ((jl_value_t*)env == jl_false) return (jl_value_t*)env;

        if (cseq && pseq) return (jl_value_t*)env;
        if (!cseq) ci++;
        if (!pseq) pi++;
    }
    return (jl_value_t*)env;
}

jl_tuple_t *jl_pair(jl_value_t *a, jl_value_t *b)
{
    return jl_tuple(2, a, b);
}

jl_tag_type_t *jl_wrap_Type(jl_value_t *t)
{
    return jl_new_tagtype((jl_value_t*)jl_type_type->name,
                          jl_any_type, jl_tuple(1, t));
}

static jl_value_t *type_match_(jl_type_t *child, jl_type_t *parent,
                               jl_tuple_t *env, int morespecific)
{
    if (jl_is_typector(child))
        child = (jl_type_t*)jl_unconstrained_type((jl_typector_t*)child);
    if (jl_is_typector(parent))
        parent = (jl_type_t*)jl_unconstrained_type((jl_typector_t*)parent);
    size_t i;
    if (jl_is_typevar(parent)) {
        if ((jl_tvar_t*)parent == jl_wildcard_type) return (jl_value_t*)env;
        jl_tuple_t *p = env;
        while (p != jl_null) {
            if (jl_t0(p) == (jl_value_t*)parent) {
                if (jl_types_equal((jl_value_t*)child, jl_t1(p)))
                    return (jl_value_t*)env;
                else
                    return jl_false;
            }
            p = (jl_tuple_t*)jl_nextpair(p);
        }
        jl_tuple_t *np = jl_tuple(3, (jl_value_t*)parent, (jl_value_t*)child,
                                  (jl_value_t*)env);
        return (jl_value_t*)np;
    }
    if (jl_is_typevar(child)) return jl_false;
    if (jl_is_int32(child) && jl_is_int32(parent)) {
        if (jl_unbox_int32((jl_value_t*)child) == jl_unbox_int32((jl_value_t*)parent))
            return (jl_value_t*)env;
        return jl_false;
    }
    if (child == parent) return (jl_value_t*)env;
    if (parent == (jl_type_t*)jl_any_type) return (jl_value_t*)env;
    if (child  == (jl_type_t*)jl_any_type) return jl_false;

    if (jl_is_union_type(child)) {
        jl_tuple_t *t = ((jl_uniontype_t*)child)->types;
        for(i=0; i < t->length; i++) {
            env = (jl_tuple_t*)type_match_((jl_type_t*)jl_tupleref(t,i),
                                           (jl_type_t*)parent, env, morespecific);
            if ((jl_value_t*)env == jl_false) return (jl_value_t*)env;
        }
        return (jl_value_t*)env;
    }
    if (jl_is_union_type(parent)) {
        jl_tuple_t *t = ((jl_uniontype_t*)parent)->types;
        for(i=0; i < t->length; i++) {
            jl_value_t *p = type_match_((jl_type_t*)child,
                                        (jl_type_t*)jl_tupleref(t,i),
                                        env, morespecific);
            if (p != jl_false) return p;
        }
        return jl_false;
    }

    if (jl_is_func_type(parent)) {
        if (jl_is_func_type(child)) {
            env = (jl_tuple_t*)type_match_(((jl_func_type_t*)child)->from,
                                           ((jl_func_type_t*)parent)->from, env,
                                           morespecific);
            if ((jl_value_t*)env == jl_false) return (jl_value_t*)env;
            return type_match_(((jl_func_type_t*)child)->to,
                               ((jl_func_type_t*)parent)->to, env,
                               morespecific);
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
            jl_value_t *childlen = jl_box_int32(((jl_tuple_t*)child)->length);
            if (jl_is_typevar(nt_len)) {
                env = (jl_tuple_t*)type_match_((jl_type_t*)childlen, (jl_type_t*)nt_len,
                                               env, morespecific);
                if ((jl_value_t*)env == jl_false) return (jl_value_t*)env;
            }
            else {
                return jl_false;
            }
            jl_tuple_t *p_seq =
                jl_tuple(1, jl_apply_type_ctor(jl_seq_type,
                                               jl_tuple(1,jl_tupleref(tp,1))));
            return tuple_match((jl_tuple_t*)child, p_seq,
                               env, morespecific);
        }

        if (jl_is_tuple(parent)) {
            return tuple_match((jl_tuple_t*)child, (jl_tuple_t*)parent, env,
                               morespecific);
        }
        return jl_false;
    }
    if (jl_is_tuple(parent)) {
        if (jl_is_tag_type(child) &&
            ((jl_tag_type_t*)child)->name == jl_ntuple_typename) {
            // only ((T:>S)...,) can be a supertype of NTuple[N,S]
            jl_tuple_t *tp = (jl_tuple_t*)parent;
            jl_value_t *ntp = jl_tupleref(((jl_tag_type_t*)child)->parameters,
                                          1);
            if (tp->length == 1 && jl_is_seq_type(jl_tupleref(tp,0))) {
                return type_match_((jl_type_t*)ntp,
                                   (jl_type_t*)jl_tparam0(jl_tupleref(tp,0)),
                                   env, morespecific);
            }
        }
        return jl_false;
    }

    assert(jl_is_some_tag_type(child));
    assert(jl_is_some_tag_type(parent));
    jl_tag_type_t *tta = (jl_tag_type_t*)child;
    jl_tag_type_t *ttb = (jl_tag_type_t*)parent;
    int super = 0;
    if (ttb->name == jl_type_type->name && tta->name != ttb->name)
        tta = jl_wrap_Type((jl_value_t*)tta);
    while (tta != (jl_tag_type_t*)jl_any_type) {
        if (tta->name == ttb->name) {
            if (super && morespecific)
                return (jl_value_t*)env;
            assert(tta->parameters->length == ttb->parameters->length);
            for(i=0; i < tta->parameters->length; i++) {
                env = (jl_tuple_t*)type_match_((jl_type_t*)jl_tupleref(tta->parameters,i),
                                               (jl_type_t*)jl_tupleref(ttb->parameters,i),
                                               env, morespecific);
                if ((jl_value_t*)env == jl_false) goto check_Type;
            }
            return (jl_value_t*)env;
        }
        tta = tta->super; super = 1;
    }
 check_Type:
    if (((jl_tag_type_t*)child)->name == jl_type_type->name) {
        // Type{T} matches either Type{:>T} or :>typeof(T)
        return type_match_((jl_type_t*)jl_full_type(jl_tparam0(child)),
                           parent, env, morespecific);
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
jl_value_t *jl_type_match(jl_type_t *a, jl_type_t *b)
{
    return type_match_(a, b, jl_null, 0);
}

jl_value_t *jl_type_match_morespecific(jl_type_t *a, jl_type_t *b)
{
    return type_match_(a, b, jl_null, 1);
}

// initialization -------------------------------------------------------------

static jl_tvar_t *tvar(const char *name)
{
    return (jl_tvar_t*)jl_new_struct(jl_tvar_type, jl_symbol(name),
                                     jl_bottom_type, jl_any_type);
}

jl_tuple_t *jl_typevars(size_t n, ...)
{
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

static jl_bits_type_t *make_scalar_type(const char *name, jl_typector_t *super,
                                        int nbits)
{
    jl_bits_type_t *bt =
        jl_new_bitstype((jl_value_t*)jl_symbol(name), jl_any_type,
                        jl_null, nbits);
    bt->super = (jl_tag_type_t*)jl_apply_type_ctor(super, jl_tuple(1, bt));
    assert(jl_is_tag_type(bt->super));
    return bt;
}

static jl_typector_t *make_scalar_subtype(const char *name,
                                          jl_typector_t *super)
{
    jl_typector_t *t;
    jl_tuple_t *tv = jl_typevars(1, "T");
    t = jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_tagtype((jl_value_t*)jl_symbol(name),
                                                    (jl_tag_type_t*)jl_apply_type_ctor(super, tv),
                                                    tv));
    return t;
}

void jl_init_types()
{
    // create base objects
    jl_struct_kind = (jl_struct_type_t*)newobj(NULL, STRUCT_TYPE_NW);
    jl_struct_kind->type = (jl_type_t*)jl_struct_kind;
    jl_tag_kind = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);
    jl_func_kind = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);

    jl_typename_type = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);
    jl_sym_type = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);

    jl_any_type = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAG_TYPE_NW);
    jl_type_type = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAG_TYPE_NW);
    jl_tuple_type = jl_alloc_tuple(1);
    jl_tuple_type->type = (jl_type_t*)jl_tuple_type;

    jl_null = (jl_tuple_t*)newobj((jl_type_t*)jl_tuple_type, 1);
    jl_null->length = 0;

    jl_any_func = jl_new_functype((jl_type_t*)jl_any_type, (jl_type_t*)jl_any_type);

    jl_bottom_func = jl_new_closure(jl_f_no_function, NULL);
    jl_identity_func = jl_new_closure(jl_f_identity, NULL);

    // initialize them. lots of cycles.
    jl_struct_kind->name = jl_new_typename(jl_symbol("StructKind"));
    jl_struct_kind->super = (jl_tag_type_t*)jl_tag_kind;
    jl_struct_kind->parameters = jl_null;
    jl_struct_kind->names = jl_tuple(6, jl_symbol("name"), jl_symbol("super"),
                                     jl_symbol("parameters"),
                                     jl_symbol("names"), jl_symbol("types"),
                                     jl_symbol("new"));
    jl_struct_kind->types = jl_tuple(6, jl_typename_type, jl_type_type,
                                     jl_tuple_type, jl_tuple_type,
                                     jl_tuple_type, jl_any_func);
    jl_struct_kind->uid = t_uid_ctr++;

    jl_tag_kind->name = jl_new_typename(jl_symbol("TagKind"));
    jl_tag_kind->super = jl_type_type;
    jl_tag_kind->parameters = jl_null;
    jl_tag_kind->names = jl_tuple(3, jl_symbol("name"), jl_symbol("super"),
                                  jl_symbol("parameters"));
    jl_tag_kind->types = jl_tuple(3, jl_typename_type, jl_type_type,
                                  jl_tuple_type);
    jl_tag_kind->uid = t_uid_ctr++;

    jl_func_kind->name = jl_new_typename(jl_symbol("FuncKind"));
    jl_func_kind->super = jl_type_type;
    jl_func_kind->parameters = jl_null;
    jl_func_kind->names = jl_tuple(2, jl_symbol("from"), jl_symbol("to"));
    jl_func_kind->types = jl_tuple(2, jl_type_type, jl_type_type);
    jl_func_kind->uid = t_uid_ctr++;

    jl_typename_type->name = jl_new_typename(jl_symbol("TypeName"));
    jl_typename_type->super = jl_any_type;
    jl_typename_type->parameters = jl_null;
    jl_typename_type->names = jl_tuple(1, jl_symbol("name"));
    jl_typename_type->types = jl_tuple(1, jl_sym_type);
    jl_typename_type->uid = t_uid_ctr++;

    jl_sym_type->name = jl_new_typename(jl_symbol("Symbol"));
    jl_sym_type->super = jl_any_type;
    jl_sym_type->parameters = jl_null;
    jl_sym_type->names = jl_null;
    jl_sym_type->types = jl_null;
    jl_sym_type->uid = t_uid_ctr++;

    jl_any_type->name = jl_new_typename(jl_symbol("Any"));
    jl_any_type->super = jl_any_type;
    jl_any_type->parameters = jl_null;

    jl_type_type->name = jl_new_typename(jl_symbol("Type"));
    jl_type_type->super = jl_any_type;

    jl_tuple_typename = jl_new_typename(jl_symbol("Tuple"));

    // now they can be used to create the remaining base kinds and types
    jl_methtable_type =
        jl_new_struct_type(jl_symbol("MethodTable"),
                           jl_any_type, jl_null, jl_null, jl_null);
    jl_methtable_type->fnew = jl_bottom_func;

    jl_union_kind = jl_new_struct_type(jl_symbol("UnionKind"),
                                       jl_type_type, jl_null,
                                       jl_tuple(1, jl_symbol("types")),
                                       jl_tuple(1, jl_tuple_type));

    jl_bottom_type = (jl_type_t*)jl_new_struct(jl_union_kind, jl_null);

    jl_bits_kind =
        jl_new_struct_type(jl_symbol("BitsKind"), (jl_tag_type_t*)jl_tag_kind,
                           jl_null,
                           jl_tuple(3, jl_symbol("name"), jl_symbol("super"),
                                    jl_symbol("parameters")),
                           jl_tuple(3, jl_typename_type, jl_type_type,
                                    jl_tuple_type));
    // cannot be created with normal constructor due to hidden fields
    
    jl_tvar_type = jl_new_struct_type(jl_symbol("TypeVar"),
                                      jl_any_type, jl_null,
                                      jl_tuple(3, jl_symbol("name"),
                                               jl_symbol("lb"),
                                               jl_symbol("ub")),
                                      jl_tuple(3, jl_sym_type, jl_type_type,
                                               jl_type_type));

    jl_wildcard_type = tvar("_");
    jl_type_type->parameters = jl_tuple(1, jl_wildcard_type);

    jl_tuple_t *tv;
    tv = jl_typevars(1, "T");
    jl_type_typector =
        jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_tagtype((jl_value_t*)jl_type_type->name, jl_any_type, tv));
    jl_type_typector->unconstrained = (jl_type_t*)jl_type_type;

    tv = jl_typevars(1, "T");
    jl_seq_type =
        jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_tagtype((jl_value_t*)jl_symbol("..."),
                                                    jl_any_type, tv));

    jl_tupleset(jl_tuple_type, 0,
                (jl_value_t*)jl_apply_type_ctor(jl_seq_type,
                                                jl_tuple(1,jl_any_type)));

    tv = jl_typevars(2, "N", "T");
    jl_ntuple_type =
        jl_new_type_ctor(tv,
                         (jl_type_t*)
                         jl_new_tagtype((jl_value_t*)jl_symbol("NTuple"),
                                        jl_any_type, tv));
    jl_ntuple_typename = ((jl_tag_type_t*)jl_ntuple_type->body)->name;

    tv = jl_typevars(2, "T", "N");
    jl_tensor_type =
        jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_tagtype((jl_value_t*)jl_symbol("Tensor"),
                                                    jl_any_type, tv));

    tv = jl_typevars(1, "T");
    jl_scalar_type =
        jl_new_type_ctor(tv,
                         jl_apply_type_ctor(jl_tensor_type,
                                            jl_tuple(2, jl_tupleref(tv,0),
                                                     jl_bottom_type)));

    jl_number_type = make_scalar_subtype("Number", jl_scalar_type);
    jl_real_type = make_scalar_subtype("Real", jl_number_type);
    jl_int_type = make_scalar_subtype("Int", jl_real_type);

    jl_int32_type = make_scalar_type("Int32", jl_int_type, 32);
    jl_value_t *zero = jl_new_box_int32(0);
    jl_tupleset(((jl_tag_type_t*)jl_scalar_type->body)->parameters, 1, zero);
    jl_tupleset(((jl_tag_type_t*)jl_number_type->body)->super->parameters, 1,
                zero);
    jl_tupleset(((jl_tag_type_t*)jl_real_type->body)->super->super->parameters, 1,
                zero);
    jl_tupleset(((jl_tag_type_t*)jl_int_type->body)->super->super->super->parameters, 1,
                zero);
    jl_tupleset(jl_int32_type->super->super->super->super->parameters, 1,
                zero);

    jl_float_type = make_scalar_subtype("Float", jl_real_type);

    jl_bool_type    = make_scalar_type("Bool"  , jl_scalar_type, 8);
    jl_int8_type    = make_scalar_type("Int8"  , jl_int_type, 8);
    jl_uint8_type   = make_scalar_type("Uint8" , jl_int_type, 8);
    jl_int16_type   = make_scalar_type("Int16" , jl_int_type, 16);
    jl_uint16_type  = make_scalar_type("Uint16", jl_int_type, 16);
    jl_uint32_type  = make_scalar_type("Uint32", jl_int_type, 32);
    jl_int64_type   = make_scalar_type("Int64" , jl_int_type, 64);
    jl_uint64_type  = make_scalar_type("Uint64", jl_int_type, 64);

    jl_float32_type = make_scalar_type("Float32", jl_float_type, 32);
    jl_float64_type = make_scalar_type("Float64", jl_float_type, 64);
}
