// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  Types
  . type union, type cache, instantiation, and specificity
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
jl_unionall_t *jl_type_type;
jl_typename_t *jl_type_typename;
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
jl_datatype_t *jl_anytuple_type;
jl_unionall_t *jl_anytuple_type_type;
jl_typename_t *jl_vecelement_typename;
jl_unionall_t *jl_vararg_type;
jl_typename_t *jl_vararg_typename;
jl_datatype_t *jl_tvar_type;
jl_datatype_t *jl_uniontype_type;
jl_datatype_t *jl_unionall_type;
jl_datatype_t *jl_datatype_type;
jl_datatype_t *jl_function_type;
jl_datatype_t *jl_builtin_type;

jl_datatype_t *jl_bottomtype_type;
jl_value_t *jl_bottom_type;
jl_unionall_t *jl_abstractarray_type;
jl_unionall_t *jl_densearray_type;

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
jl_unionall_t *jl_complex_type;
jl_datatype_t *jl_signed_type;

JL_DLLEXPORT jl_value_t *jl_emptytuple=NULL;
jl_svec_t *jl_emptysvec;
jl_value_t *jl_nothing;

jl_cgparams_t jl_default_cgparams = {1, 1, 1, 1, 1, 1, 1, {NULL, NULL, NULL}};

// --- type properties and predicates ---

typedef struct _typeenv {
    jl_tvar_t *var;
    jl_value_t *val;
    struct _typeenv *prev;
} jl_typeenv_t;

static int typeenv_has(jl_typeenv_t *env, jl_tvar_t *v)
{
    while (env != NULL) {
        if (env->var == v)
            return 1;
        env = env->prev;
    }
    return 0;
}

static int has_free_typevars(jl_value_t *v, jl_typeenv_t *env)
{
    if (jl_typeis(v, jl_tvar_type)) {
        if (v == jl_ANY_flag) return 0;
        return !typeenv_has(env, (jl_tvar_t*)v);
    }
    if (jl_is_uniontype(v))
        return has_free_typevars(((jl_uniontype_t*)v)->a, env) ||
            has_free_typevars(((jl_uniontype_t*)v)->b, env);
    if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        jl_typeenv_t newenv = { ua->var, NULL, env };
        return has_free_typevars(ua->var->lb, env) || has_free_typevars(ua->var->ub, env) ||
            has_free_typevars(ua->body, &newenv);
    }
    if (jl_is_datatype(v)) {
        int expect = ((jl_datatype_t*)v)->hasfreetypevars;
        if (expect == 0 || env == NULL)
            return expect;
        size_t i;
        for (i=0; i < jl_nparams(v); i++) {
            if (has_free_typevars(jl_tparam(v,i), env)) {
                assert(expect);
                return 1;
            }
        }
    }
    return 0;
}

JL_DLLEXPORT int jl_has_free_typevars(jl_value_t *v)
{
    return has_free_typevars(v, NULL);
}

// test whether a type has vars bound by the given environment
JL_DLLEXPORT int jl_has_bound_typevars(jl_value_t *v, jl_typeenv_t *env)
{
    if (jl_typeis(v, jl_tvar_type))
        return typeenv_has(env, (jl_tvar_t*)v);
    if (jl_is_uniontype(v))
        return jl_has_bound_typevars(((jl_uniontype_t*)v)->a, env) ||
            jl_has_bound_typevars(((jl_uniontype_t*)v)->b, env);
    if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        if (jl_has_bound_typevars(ua->var->lb, env) || jl_has_bound_typevars(ua->var->ub, env))
            return 1;
        jl_typeenv_t *te = env;
        while (te != NULL) {
            if (te->var == ua->var)
                break;
            te = te->prev;
        }
        if (te) te->var = NULL;  // temporarily remove this var from env
        int ans = jl_has_bound_typevars(ua->body, env);
        if (te) te->var = ua->var;
        return ans;
    }
    if (jl_is_datatype(v)) {
        if (!((jl_datatype_t*)v)->hasfreetypevars)
            return 0;
        size_t i;
        for (i=0; i < jl_nparams(v); i++) {
            if (jl_has_bound_typevars(jl_tparam(v,i), env))
                return 1;
        }
    }
    return 0;
}

JL_DLLEXPORT int jl_has_typevar(jl_value_t *t, jl_tvar_t *v)
{
    jl_typeenv_t env = { v, NULL, NULL };
    return jl_has_bound_typevars(t, &env);
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
            if (jl_is_type_type(v))
                x = !jl_has_free_typevars(jl_tparam0(v));
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
                if (jl_has_free_typevars(p)) {
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
            jl_uniontype_t *u = (jl_uniontype_t*)e;
            c += count_union_components(&u->a, 1);
            c += count_union_components(&u->b, 1);
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
            jl_uniontype_t *u = (jl_uniontype_t*)e;
            flatten_type_union(&u->a, 1, out, idx);
            flatten_type_union(&u->b, 1, out, idx);
        }
        else {
            out[*idx] = e;
            (*idx)++;
        }
    }
}

JL_DLLEXPORT jl_value_t *jl_type_union(jl_value_t **ts, size_t n)
{
    if (n == 0) return (jl_value_t*)jl_bottom_type;
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *pi = ts[i];
        if (!(jl_is_type(pi) || jl_is_typevar(pi)) || jl_is_vararg_type(pi))
            jl_type_error_rt("Union", "parameter", (jl_value_t*)jl_type_type, pi);
    }
    if (n == 1) return ts[0];

    size_t nt = count_union_components(ts, n);
    jl_value_t **temp;
    JL_GC_PUSHARGS(temp, nt+1);
    size_t count = 0;
    flatten_type_union(ts, n, temp, &count);
    assert(count == nt);
    size_t j;
    for(i=0; i < nt; i++) {
        int has_free = temp[i]!=NULL && jl_has_free_typevars(temp[i]);
        for(j=0; j < nt; j++) {
            if (j != i && temp[i] && temp[j]) {
                if (temp[i] == temp[j] || temp[i] == jl_bottom_type ||
                    temp[j] == (jl_value_t*)jl_any_type ||
                    (!has_free && !jl_has_free_typevars(temp[j]) &&
                     jl_subtype(temp[i], temp[j]))) {
                    temp[i] = NULL;
                }
            }
        }
    }
    jl_value_t **ptu = &temp[nt];
    *ptu = jl_bottom_type;
    int k;
    for (k = (int)nt-1; k >= 0; --k) {
        if (temp[k] != NULL) {
            if (*ptu == jl_bottom_type)
                *ptu = temp[k];
            else
                *ptu = jl_new_struct(jl_uniontype_type, temp[k], *ptu);
        }
    }
    assert(*ptu != NULL);
    jl_value_t *tu = *ptu;
    JL_GC_POP();
    return tu;
}

// unionall types -------------------------------------------------------------

JL_DLLEXPORT jl_tvar_t *jl_new_typevar(jl_sym_t *name, jl_value_t *lb, jl_value_t *ub)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_tvar_t *tv = (jl_tvar_t*)jl_gc_alloc(ptls, sizeof(jl_tvar_t), jl_tvar_type);
    tv->name = name;
    tv->lb = lb;
    tv->ub = ub;
    return tv;
}

JL_DLLEXPORT jl_value_t *jl_type_unionall(jl_tvar_t *v, jl_value_t *body)
{
    // normalize `T where T<:S` => S
    if (body == (jl_value_t*)v)
        return v->ub;
    // where var doesn't occur in body just return body
    if (!jl_has_typevar(body, v))
        return body;
    //if (v->lb == v->ub)  // TODO maybe
    //    return jl_substitute_var(body, v, v->ub);
    return jl_new_struct(jl_unionall_type, v, body);
}

// --- type instantiation and cache ---

static int contains_unions(jl_value_t *type)
{
    if (jl_is_uniontype(type)) return 1;
    if (jl_is_unionall(type)) return contains_unions(((jl_unionall_t*)type)->body);
    if (!jl_is_datatype(type)) return 0;
    int i;
    for(i=0; i < jl_nparams(type); i++) {
        if (contains_unions(jl_tparam(type,i)))
            return 1;
    }
    return 0;
}

static intptr_t wrapper_id(jl_value_t *t)
{
    // DataType wrappers occur often, e.g. when called as constructors.
    // make sure any type equal to a wrapper gets a consistent, ordered ID.
    if (!jl_is_unionall(t)) return 0;
    jl_value_t *u = jl_unwrap_unionall(t);
    if (jl_is_datatype(u) && ((jl_datatype_t*)u)->name->wrapper == t)
        return ((jl_datatype_t*)u)->name->hash;
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
        if (jl_is_type(k) && k != jl_bottom_type && !wrapper_id(k) &&
            !(jl_is_datatype(k) && (((jl_datatype_t*)k)->uid ||
                                    (!jl_has_free_typevars(k) && !contains_unions(k)))))
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
            uint32_t tid=0, kid=0;
            int dtk = jl_is_datatype(kj);
            if (!jl_is_datatype(tj)) {
                tid = wrapper_id(tj);
                if (tid) {
                    kid = wrapper_id(kj);
                }
                else if (!dtk) {
                    if (jl_egal(tj, kj))
                        continue;
                    return (jl_object_id(kj) < jl_object_id(tj) ? -1 : 1);
                }
                else {
                    return 1;
                }
            }
            else if (!dtk) {
                kid = wrapper_id(kj);
                if (!kid)
                    return -1;
            }
            if (tid || kid) {
                if (kid != tid)
                    return kid < tid ? -1 : 1;
                continue;
            }
            assert(dtk && jl_is_datatype(tj));
            jl_datatype_t *dt = (jl_datatype_t*)tj;
            jl_datatype_t *dk = (jl_datatype_t*)kj;
            if (dk->uid != dt->uid) {
                return dk->uid < dt->uid ? -1 : 1;
            }
            else if (dk->uid != 0) {
                assert(0);
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
        if (tj != kj && !jl_types_equal(tj, kj))
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
    // cache abstract types with no free type vars
    if (jl_is_abstracttype(type))
        return !jl_has_free_typevars((jl_value_t*)type);
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

// type instantiation

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

static int within_typevar(jl_value_t *t, jl_value_t *vlb, jl_value_t *vub)
{
    jl_value_t *lb = t, *ub = t;
    if (jl_is_typevar(t) || jl_has_free_typevars(t)) {
        // TODO: automatically restrict typevars in method definitions based on
        // types they are used in.
        return 1;
        //lb = ((jl_tvar_t*)t)->lb;
        //ub = ((jl_tvar_t*)t)->ub;
    }
    else if (!jl_is_type(t)) {
        return vlb == jl_bottom_type && vub == (jl_value_t*)jl_any_type;
    }
    return jl_subtype(vlb, lb) && jl_subtype(ub, vub);
}

jl_value_t *jl_apply_type(jl_value_t *tc, jl_value_t **params, size_t n)
{
    if (tc == (jl_value_t*)jl_anytuple_type)
        return (jl_value_t*)jl_apply_tuple_type_v(params, n);
    if (tc == (jl_value_t*)jl_uniontype_type)
        return (jl_value_t*)jl_type_union(params, n);
    JL_GC_PUSH1(&tc);
    size_t i;
    for (i=0; i < n; i++) {
        if (!jl_is_unionall(tc))
            jl_error("too many parameters for type");
        jl_value_t *pi = params[i];

        if (!valid_type_param(pi)) {
            jl_type_error_rt("Type", "parameter",
                             jl_isa(pi, (jl_value_t*)jl_number_type) ?
                             (jl_value_t*)jl_long_type : (jl_value_t*)jl_type_type,
                             pi);
        }

        jl_unionall_t *ua = (jl_unionall_t*)tc;
        if (!jl_has_free_typevars(ua->var->lb) && !jl_has_free_typevars(ua->var->ub) &&
            !within_typevar(pi, ua->var->lb, ua->var->ub)) {
            jl_datatype_t *inner = (jl_datatype_t*)jl_unwrap_unionall(tc);
            int iswrapper = 0;
            if (jl_is_datatype(inner)) {
                jl_value_t *temp = inner->name->wrapper;
                while (jl_is_unionall(temp)) {
                    if (temp == tc) {
                        iswrapper = 1;
                        break;
                    }
                    temp = ((jl_unionall_t*)temp)->body;
                }
            }
            // if this is a wrapper, let check_datatype_parameters give the error
            if (!iswrapper)
                jl_type_error_rt("Type", jl_symbol_name(ua->var->name), (jl_value_t*)ua->var, pi);
        }

        tc = jl_instantiate_unionall(ua, pi);
    }
    JL_GC_POP();
    return tc;
}

JL_DLLEXPORT jl_value_t *jl_apply_type1(jl_value_t *tc, jl_value_t *p1)
{
    JL_GC_PUSH1(&p1);
    jl_value_t *t = jl_apply_type(tc, &p1, 1);
    JL_GC_POP();
    return t;
}

JL_DLLEXPORT jl_value_t *jl_apply_type2(jl_value_t *tc, jl_value_t *p1, jl_value_t *p2)
{
    jl_value_t **args;
    JL_GC_PUSHARGS(args, 2);
    args[0] = p1; args[1] = p2;
    jl_value_t *t = jl_apply_type(tc, args, 2);
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

typedef struct _jl_typestack_t {
    union {
        jl_value_t *ua;
        jl_datatype_t *tt;
    };
    jl_value_t *ua_new;
    struct _jl_typestack_t *prev;
} jl_typestack_t;

static jl_value_t *inst_type_w_(jl_value_t *t, jl_typeenv_t *env, jl_typestack_t *stack, int check);
static jl_svec_t *inst_all(jl_svec_t *p, jl_typeenv_t *env, jl_typestack_t *stack, int check);

JL_DLLEXPORT jl_value_t *jl_instantiate_unionall(jl_unionall_t *u, jl_value_t *p)
{
    jl_typeenv_t env = { u->var, p, NULL };
    return inst_type_w_(u->body, &env, NULL, 1);
}

jl_value_t *jl_substitute_var(jl_value_t *t, jl_tvar_t *var, jl_value_t *val)
{
    jl_typeenv_t env = { var, val, NULL };
    return inst_type_w_(t, &env, NULL, 1);
}

jl_value_t *jl_unwrap_unionall(jl_value_t *v)
{
    while (jl_is_unionall(v))
        v = ((jl_unionall_t*)v)->body;
    return v;
}

// wrap `t` in the same unionalls that surround `u`
jl_value_t *jl_rewrap_unionall(jl_value_t *t, jl_value_t *u)
{
    if (!jl_is_unionall(u))
        return t;
    JL_GC_PUSH1(&t);
    t = jl_rewrap_unionall(t, ((jl_unionall_t*)u)->body);
    t = jl_new_struct(jl_unionall_type, ((jl_unionall_t*)u)->var, t);
    JL_GC_POP();
    return t;
}

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
            // TODO jb/subtype
            //return lkup == jl_unwrap_unionall(tn->wrapper) ? NULL : lkup;
            return lkup;
        }
        stack = stack->prev;
    }
    return NULL;
}

static size_t jl_type_depth(jl_value_t *dt)
{
    if (jl_is_uniontype(dt)) {
        size_t ad = jl_type_depth(((jl_uniontype_t*)dt)->a);
        size_t bd = jl_type_depth(((jl_uniontype_t*)dt)->b);
        return ad > bd ? ad : bd;
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
    dt->isleaftype = !dt->abstract || (jl_type_type != NULL && dt->name == jl_type_typename);
    for (i = 0; i < l; i++) {
        jl_value_t *p = jl_tparam(dt, i);
        size_t d = jl_type_depth(p) + 1;
        if (d > dt->depth)
            dt->depth = d;
        if (!dt->hasfreetypevars)
            dt->hasfreetypevars = jl_has_free_typevars(p);
        if (dt->isleaftype)
            dt->isleaftype = (istuple ? jl_is_leaf_type(p) : !dt->hasfreetypevars);
    }
}

static void check_datatype_parameters(jl_typename_t *tn, jl_value_t **params, size_t np)
{
    jl_value_t *wrapper = tn->wrapper;
    jl_value_t **bounds;
    JL_GC_PUSHARGS(bounds, np*2);
    int i = 0;
    while (jl_is_unionall(wrapper)) {
        jl_tvar_t *tv = ((jl_unionall_t*)wrapper)->var;
        bounds[i++] = tv->lb;
        bounds[i++] = tv->ub;
        wrapper = ((jl_unionall_t*)wrapper)->body;
    }
    assert(i == np*2);
    wrapper = tn->wrapper;
    for(i=0; i < np; i++) {
        assert(jl_is_unionall(wrapper));
        jl_tvar_t *tv = ((jl_unionall_t*)wrapper)->var;
        if (!within_typevar(params[i], bounds[2*i], bounds[2*i+1])) {
            // TODO: pass a new version of `tv` containing the instantiated bounds
            jl_type_error_rt(jl_symbol_name(tn->name), jl_symbol_name(tv->name), (jl_value_t*)tv, params[i]);
        }
        int j;
        for(j=2*i+2; j < 2*np; j++) {
            jl_value_t*bj = bounds[j];
            if (bj != (jl_value_t*)jl_any_type && bj != jl_bottom_type)
                bounds[j] = jl_substitute_var(bj, tv, params[i]);
        }
        wrapper = ((jl_unionall_t*)wrapper)->body;
    }
    JL_GC_POP();
}

static arraylist_t partial_inst;
int inside_typedef = 0;

static jl_value_t *extract_wrapper(jl_value_t *t)
{
    t = jl_unwrap_unionall(t);
    if (jl_is_datatype(t))
        return ((jl_datatype_t*)t)->name->wrapper;
    if (jl_is_uniontype(t)) {
        jl_value_t *n1 = extract_wrapper(((jl_uniontype_t*)t)->a);
        if (n1 != NULL) return n1;
        return extract_wrapper(((jl_uniontype_t*)t)->b);
    }
    if (jl_is_typevar(t))
        return extract_wrapper(((jl_tvar_t*)t)->ub);
    return NULL;
}

static jl_value_t *inst_datatype(jl_datatype_t *dt, jl_svec_t *p, jl_value_t **iparams, size_t ntp,
                                 int cacheable, jl_typestack_t *stack, jl_typeenv_t *env)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_typestack_t top;
    jl_typename_t *tn = dt->name;
    int istuple = (tn == jl_tuple_typename);
    // check type cache
    if (cacheable) {
        JL_LOCK(&typecache_lock); // Might GC
        size_t i;
        for(i=0; i < ntp; i++) {
            jl_value_t *pi = iparams[i];
            jl_value_t *tw = extract_wrapper(pi);
            if (tw && jl_types_equal(pi, tw)) {
                iparams[i] = tw;
                if (p) jl_gc_wb(p, tw);
            }
        }
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

    jl_value_t *last = iparams[ntp - 1];
    if (istuple && ntp > 0 && jl_is_vararg_type(last)) {
        // normalize Tuple{..., Vararg{Int, 3}} to Tuple{..., Int, Int, Int}
        jl_value_t *va = jl_unwrap_unionall(last);
        // return same `Tuple` object for types equal to it
        if (ntp == 1 && jl_tparam0(va) == (jl_value_t*)jl_any_type &&
            jl_is_unionall(last) && jl_tparam1(va) == (jl_value_t*)((jl_unionall_t*)last)->var) {
            if (cacheable) JL_UNLOCK(&typecache_lock); // Might GC
            return (jl_value_t*)jl_anytuple_type;
        }
        if (jl_is_long(jl_tparam1(va))) {
            ssize_t nt = jl_unbox_long(jl_tparam1(va));
            if (nt < 0)
                jl_errorf("apply_type: Vararg length N is negative: %zd", nt);
            va = jl_tparam0(va);
            if (nt == 0 || !jl_has_free_typevars(va)) {
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

    if (!istuple) {
        if (jl_is_vararg_type((jl_value_t*)dt) && ntp == 2) {
            if (!jl_is_long(iparams[1]) && !jl_is_typevar(iparams[1])) {
                jl_type_error_rt("apply_type", "Vararg count", (jl_value_t*)jl_long_type, iparams[1]);
            }
        }
    }
    else if (ntp == 0 && jl_emptytuple != NULL) {
        if (cacheable) JL_UNLOCK(&typecache_lock); // Might GC
        return jl_typeof(jl_emptytuple);
    }

    jl_datatype_t *ndt = NULL;
    jl_svec_t *ftypes;

    // check parameters against bounds in type definition
    if (!istuple)
        check_datatype_parameters(tn, iparams, ntp);

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
        ndt->super = (jl_datatype_t*)inst_type_w_((jl_value_t*)dt->super, env, stack, 1);
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
                ndt->types = inst_all(ftypes, env, stack, 1);
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
                                                       cacheable, NULL, NULL);
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
    return (jl_datatype_t*)inst_datatype(jl_anytuple_type, p, jl_svec_data(p), jl_svec_len(p), 1, NULL, NULL);
}

jl_datatype_t *jl_inst_concrete_tupletype_v(jl_value_t **p, size_t np)
{
    return (jl_datatype_t*)inst_datatype(jl_anytuple_type, NULL, p, np, 1, NULL, NULL);
}

static jl_svec_t *inst_all(jl_svec_t *p, jl_typeenv_t *env, jl_typestack_t *stack, int check)
{
    size_t i;
    size_t lp = jl_svec_len(p);
    jl_svec_t *np = jl_alloc_svec(lp);
    JL_GC_PUSH1(&np);
    for(i=0; i < lp; i++) {
        jl_svecset(np, i, (jl_value_t*)inst_type_w_(jl_svecref(p,i), env, stack, check));
    }
    JL_GC_POP();
    return np;
}

static jl_value_t *inst_tuple_w_(jl_value_t *t, jl_typeenv_t *env, jl_typestack_t *stack, int check)
{
    jl_datatype_t *tt = (jl_datatype_t*)t;
    jl_svec_t *tp = tt->parameters;
    size_t ntp = jl_svec_len(tp);
    // Instantiate NTuple{3,Int}
    // Note this does not instantiate Tuple{Vararg{Int,3}}; that's done in inst_datatype
    if (jl_is_va_tuple(tt) && ntp == 1) {
        // If this is a Tuple{Vararg{T,N}} with known N, expand it to
        // a fixed-length tuple
        jl_value_t *T=NULL, *N=NULL;
        jl_value_t *va = jl_unwrap_unionall(jl_tparam0(tt));
        jl_value_t *ttT = jl_tparam0(va);
        jl_value_t *ttN = jl_tparam1(va);
        jl_typeenv_t *e = env;
        while (e != NULL) {
            if ((jl_value_t*)e->var == ttT)
                T = e->val;
            else if ((jl_value_t*)e->var == ttN)
                N = e->val;
            e = e->prev;
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
        jl_value_t *pi = (jl_value_t*)inst_type_w_(elt, env, stack, 0);
        iparams[i] = pi;
        if (ip_heap)
            jl_gc_wb(ip_heap, pi);
        check_tuple_parameter(pi, i, ntp);
        if (cacheable && !jl_is_leaf_type(pi)) {
            cacheable = 0;
        }
    }
    jl_value_t *result = inst_datatype((jl_datatype_t*)tt, ip_heap, iparams, ntp, cacheable,
                                       stack, env);
    JL_GC_POP();
    return result;
}

static jl_value_t *inst_type_w_(jl_value_t *t, jl_typeenv_t *env, jl_typestack_t *stack, int check)
{
    size_t i;
    if (jl_is_typevar(t)) {
        jl_typeenv_t *e = env;
        while (e != NULL) {
            if (e->var == (jl_tvar_t*)t) {
                jl_value_t *val = e->val;
                // TODO jb/subtype this seems unnecessary
                //if (check && !jl_is_typevar(val) && !within_typevar(val, (jl_tvar_t*)t)) {
                //    jl_type_error_rt("type parameter",
                //                     jl_symbol_name(((jl_tvar_t*)t)->name), t, val);
                //}
                return val;
            }
            e = e->prev;
        }
        return (jl_value_t*)t;
    }
    if (jl_is_unionall(t)) {
        jl_typestack_t *sp = stack;
        while (sp != NULL) {
            if (sp->ua == t)
                return sp->ua_new;
            sp = sp->prev;
        }
        if (!jl_has_free_typevars(t))
            return t;
        jl_unionall_t *ua = (jl_unionall_t*)t;
        jl_value_t *res=NULL, *lb=ua->var->lb, *ub=ua->var->ub;
        JL_GC_PUSH3(&lb, &ub, &res);
        res = jl_new_struct(jl_unionall_type, ua->var, NULL);
        jl_typestack_t top = { {t}, res, stack };
        if (jl_has_bound_typevars(ua->var->lb, env) || jl_has_bound_typevars(ua->var->ub, env)) {
            lb = inst_type_w_(ua->var->lb, env, &top, check);
            ub = inst_type_w_(ua->var->ub, env, &top, check);
        }
        if (lb != ua->var->lb || ub != ua->var->ub)
            ((jl_unionall_t*)res)->var = jl_new_typevar(ua->var->name, lb, ub);
        jl_typeenv_t newenv = { ua->var, (jl_value_t*)((jl_unionall_t*)res)->var, env };
        ((jl_unionall_t*)res)->body = inst_type_w_(ua->body, &newenv, &top, check);
        if (((jl_unionall_t*)res)->body == ua->body)
            res = t;
        JL_GC_POP();
        return res;
    }
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        jl_value_t *a = inst_type_w_(u->a, env, stack, check);
        jl_value_t *b = NULL;
        JL_GC_PUSH2(&a, &b);
        b = inst_type_w_(u->b, env, stack, check);
        jl_value_t *res;
        if (a == u->a && b == u->b) {
            res = t;
        }
        else {
            jl_value_t *uargs[2] = {a, b};
            res = jl_type_union(uargs, 2);
        }
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
    if (tn == jl_tuple_typename)
        return inst_tuple_w_(t, env, stack, check);
    size_t ntp = jl_svec_len(tp);
    jl_value_t **iparams;
    JL_GC_PUSHARGS(iparams, ntp);
    int cacheable = 1, bound = 0;
    for(i=0; i < ntp; i++) {
        jl_value_t *elt = jl_svecref(tp, i);
        iparams[i] = (jl_value_t*)inst_type_w_(elt, env, stack, check);
        bound |= (iparams[i] != elt);
        if (cacheable && jl_has_free_typevars(iparams[i]))
            cacheable = 0;
    }
    // if t's parameters are not bound in the environment, return it uncopied (#9378)
    if (!bound) { JL_GC_POP(); return (jl_value_t*)t; }

    jl_value_t *result = inst_datatype((jl_datatype_t*)tt, NULL, iparams, ntp, cacheable,
                                       stack, env);
    JL_GC_POP();
    return result;
}

jl_value_t *instantiate_with(jl_value_t *t, jl_value_t **env, size_t n, jl_typeenv_t *te, jl_typestack_t *stack)
{
    if (n > 0) {
        jl_typeenv_t en = { (jl_tvar_t*)env[0], env[1], te };
        return instantiate_with(t, &env[2], n-1, &en, stack);
    }
    return inst_type_w_(t, te, stack, 1);
}

jl_value_t *jl_instantiate_type_with(jl_value_t *t, jl_value_t **env, size_t n)
{
    return instantiate_with(t, env, n, NULL, NULL);
}

jl_datatype_t *jl_wrap_Type(jl_value_t *t)
{
    return (jl_datatype_t*)jl_instantiate_unionall(jl_type_type, t);
}

jl_value_t *jl_wrap_vararg(jl_value_t *t, jl_value_t *n)
{
    if (n == NULL) {
        if (t == NULL)
            return (jl_value_t*)jl_vararg_type;
        return jl_instantiate_unionall(jl_vararg_type, t);
    }
    return jl_instantiate_unionall((jl_unionall_t*)jl_instantiate_unionall(jl_vararg_type, t), n);
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
        assert(jl_unwrap_unionall(ndt->name->wrapper) == (jl_value_t*)t);
        for (i = 0; i < n; i++)
            env[i * 2 + 1] = jl_svecref(ndt->parameters, i);

        ndt->super = (jl_datatype_t*)instantiate_with((jl_value_t*)t->super, env, n, NULL, &top);
        jl_gc_wb(ndt, ndt->super);
    }

    if (t->name->names != jl_emptysvec) {
        for (j = 0; j < partial_inst.len; j++) {
            jl_datatype_t *ndt = (jl_datatype_t*)partial_inst.items[j];
            for (i = 0; i < n; i++)
                env[i * 2 + 1] = jl_svecref(ndt->parameters, i);

            int k;
            if (ndt->types == NULL) {
                ndt->types = jl_alloc_svec(jl_svec_len(t->types));
                jl_gc_wb(ndt, ndt->types);
            }
            for (k=0; k < jl_svec_len(t->types); k++) {
                jl_svecset(ndt->types, k, instantiate_with(jl_svecref(t->types,k), env, n, NULL, &top));
            }
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

// specificity comparison

static int type_eqv_with_ANY(jl_value_t *a, jl_value_t *b)
{
    // equate ANY and Any for specificity purposes, #16153
    return ((a == (jl_value_t*)jl_any_type && b == jl_ANY_flag) ||
            (b == (jl_value_t*)jl_any_type && a == jl_ANY_flag) ||
            jl_types_equal(a, b));
}

static jl_datatype_t *jl_fix_vararg_bound(jl_datatype_t *tt, int nfix)
{
    assert(jl_is_va_tuple(tt));
    assert(nfix >= 0);
    jl_svec_t *tp = tt->parameters;
    size_t ntp = jl_svec_len(tp);
    jl_typeenv_t env = { (jl_tvar_t*)jl_tparam1(jl_unwrap_unionall(jl_tparam(tt, ntp-1))), jl_box_long(nfix), NULL };
    JL_GC_PUSH2(&env.var, &env.val);
    jl_datatype_t *ret = (jl_datatype_t*)inst_type_w_((jl_value_t*)tt, &env, NULL, 1);
    JL_GC_POP();
    return ret;
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

static size_t tuple_vararg_params(jl_svec_t *a, jl_vararg_kind_t *kind, jl_tuple_lenkind_t *lenkind)
{
    jl_value_t **data = jl_svec_data(a); size_t lenr = jl_svec_len(a);
    size_t lenf = lenr;
    if (lenr == 0) {
        *kind = JL_VARARG_NONE;
        *lenkind = JL_TUPLE_FIXED;
        return lenf;
    }
    *lenkind = JL_TUPLE_VAR;
    jl_value_t *last = data[lenr-1];
    *kind = jl_vararg_kind(last);
    if (*kind == JL_VARARG_NONE || *kind == JL_VARARG_INT)
        *lenkind = JL_TUPLE_FIXED;
    if (*kind == JL_VARARG_INT || *kind == JL_VARARG_BOUND) {
        jl_value_t *N = jl_tparam1(jl_unwrap_unionall(last));
        if (jl_is_long(N)) {
            lenf += jl_unbox_long(N)-1;
            *lenkind = JL_TUPLE_FIXED;
        }
    }
    return lenf;
}

static int type_morespecific_(jl_value_t *a, jl_value_t *b, int invariant, jl_typeenv_t *env);

static int tuple_morespecific(jl_datatype_t *cdt, jl_datatype_t *pdt, int invariant, jl_typeenv_t *env)
{
    size_t clenr = jl_nparams(cdt);
    jl_value_t **child = jl_svec_data(cdt->parameters);
    size_t plenr = jl_nparams(pdt);
    jl_value_t **parent = jl_svec_data(pdt->parameters);
    size_t plenf, clenf;
    jl_vararg_kind_t ckind, pkind;
    jl_tuple_lenkind_t clenkind, plenkind;
    clenf = tuple_vararg_params(cdt->parameters, &ckind, &clenkind);
    plenf = tuple_vararg_params(pdt->parameters, &pkind, &plenkind);
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
            return (clenf==plenf || cseq) && some_morespecific;
        if (ci < clenr) {
            ce = child[ci];
            if (jl_is_vararg_type(ce)) ce = jl_unwrap_vararg(ce);
        }
        if (pi < plenr) {
            pe = parent[pi];
            if (jl_is_vararg_type(pe)) pe = jl_unwrap_vararg(pe);
        }

        if (!type_morespecific_(ce, pe, invariant, env)) {
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

// Called when a is a bound-vararg and b is not a vararg. Sets the vararg length
// in a to match b, as long as this makes some earlier argument more specific.
static int args_morespecific_fix1(jl_value_t *a, jl_value_t *b, int swap, jl_typeenv_t *env)
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
        if (jl_types_equal(b, (jl_value_t*)newtta))
            ret = swap;
        else if (swap)
            ret = type_morespecific_(b, (jl_value_t*)newtta, 0, env);
        else
            ret = type_morespecific_((jl_value_t*)newtta, b, 0, env);
        JL_GC_POP();
        return ret;
    }
    return -1;
}

static int partially_morespecific(jl_value_t *a, jl_value_t *b, int invariant, jl_typeenv_t *env)
{
    if (jl_is_uniontype(b)) {
        jl_uniontype_t *u = (jl_uniontype_t*)b;
        if ((type_morespecific_(a, u->a, invariant, env) &&
             !type_morespecific_(u->a, a, invariant, env)) ||
            (type_morespecific_(a, u->b, invariant, env) &&
             !type_morespecific_(u->b, a, invariant, env)))
            return 1;
        return 0;
    }
    return type_morespecific_(a, b, invariant, env);
}

static int count_occurs(jl_value_t *t, jl_tvar_t *v)
{
    if (t == (jl_value_t*)v)
        return 1;
    if (jl_is_uniontype(t)) {
        int a = count_occurs(((jl_uniontype_t*)t)->a, v);
        int b = count_occurs(((jl_uniontype_t*)t)->b, v);
        return a > b ? a : b;
    }
    if (jl_is_unionall(t)) {
        if (((jl_unionall_t*)t)->var == v)
            return 0;
        return count_occurs(((jl_unionall_t*)t)->body, v);
    }
    if (jl_is_datatype(t)) {
        int i, c=0;
        for(i=0; i < jl_nparams(t); i++)
            c += count_occurs(jl_tparam(t,i), v);
        return c;
    }
    return 0;
}

static int num_occurs(jl_tvar_t *v, jl_typeenv_t *env)
{
    jl_typeenv_t *e = env;
    while (e != NULL) {
        if (e->var == v)
            return (int)(ssize_t)e->val;
        e = e->prev;
    }
    return 0;
}

static int type_morespecific_(jl_value_t *a, jl_value_t *b, int invariant, jl_typeenv_t *env)
{
    if (jl_is_unionall(a)) {
        jl_unionall_t *ua = (jl_unionall_t*)a;
        jl_typeenv_t newenv = { ua->var, 0x0, env };
        newenv.val = (jl_value_t*)(intptr_t)count_occurs(ua->body, ua->var);
        return type_morespecific_(ua->body, b, invariant, &newenv);
    }
    if (jl_is_unionall(b)) {
        jl_unionall_t *ub = (jl_unionall_t*)b;
        jl_typeenv_t newenv = { ub->var, 0x0, env };
        newenv.val = (jl_value_t*)(intptr_t)count_occurs(ub->body, ub->var);
        return type_morespecific_(a, ub->body, invariant, &newenv);
    }
    if (a == b) {
        // TODO; maybe change this
        return 1;
    }
    size_t i;
    if (jl_is_tuple_type(a) && jl_is_tuple_type(b)) {
        jl_datatype_t *tta = (jl_datatype_t*)a;
        jl_datatype_t *ttb = (jl_datatype_t*)b;
        size_t alenf, blenf;
        jl_vararg_kind_t akind, bkind;
        jl_tuple_lenkind_t alenkind, blenkind;
        alenf = tuple_vararg_params(tta->parameters, &akind, &alenkind);
        blenf = tuple_vararg_params(ttb->parameters, &bkind, &blenkind);
        // When one is JL_VARARG_BOUND and the other has fixed length,
        // allow the argument length to fix the tvar
        int ans = -1;
        if (akind == JL_VARARG_BOUND && blenkind == JL_TUPLE_FIXED && blenf >= alenf)
            ans = args_morespecific_fix1(a, b, 0, env);
        if (bkind == JL_VARARG_BOUND && alenkind == JL_TUPLE_FIXED && alenf >= blenf)
            ans = args_morespecific_fix1(b, a, 1, env);
        if (ans != -1) return ans;
        return tuple_morespecific((jl_datatype_t*)a, (jl_datatype_t*)b, invariant, env);
    }

    if (jl_is_uniontype(a)) {
        if (jl_subtype(b, a)) {
            // fixes issue #4413
            if (!jl_subtype(a, b))
                return 0;
        }
        else if (jl_subtype(a, b)) {
            return 1;
        }
        // Union a is more specific than b if some element of a is
        // more specific than b, and b is not more specific than any
        // element of a.
        jl_uniontype_t *u = (jl_uniontype_t*)a;
        if (partially_morespecific(u->a, b, invariant, env) && !type_morespecific_(b, u->a, invariant, env)) {
            if (partially_morespecific(b, a, invariant, env))
                return 0;
            return 1;
        }
        if (partially_morespecific(u->b, b, invariant, env) && !type_morespecific_(b, u->b, invariant, env)) {
            if (partially_morespecific(b, a, invariant, env))
                return 0;
            return 1;
        }
        return 0;
    }

    if (jl_is_type_type(a) && !invariant) {
        jl_value_t *tp0a = jl_tparam0(a);
        if (jl_is_typevar(tp0a)) {
            jl_value_t *ub = ((jl_tvar_t*)tp0a)->ub;
            if (is_kind(b) && !jl_subtype((jl_value_t*)jl_any_type, ub))
                return 1;
        }
        else {
            if (jl_isa(tp0a, b))
                return 1;
        }
    }

    if (jl_is_uniontype(b)) {
        if (invariant)
            return 0;
        jl_uniontype_t *u = (jl_uniontype_t*)b;
        if (type_morespecific_(a, u->a, invariant, env) || type_morespecific_(a, u->b, invariant, env))
            return 1;
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
                    if (tta->name != jl_type_typename)
                        return 1;
                }
                if (super && ttb->name == jl_type_typename && jl_is_typevar(jl_tparam0(b))) {
                    if (type_morespecific_(a, jl_tparam0(b), 1, env))
                        return 1;
                }
                assert(jl_nparams(tta) == jl_nparams(ttb));
                int ascore=0, bscore=0, ascore1=0, bscore1=0;
                for(i=0; i < jl_nparams(tta); i++) {
                    jl_value_t *apara = jl_tparam(tta,i);
                    jl_value_t *bpara = jl_tparam(ttb,i);
                    ascore += type_morespecific_(apara, bpara, 1, env);
                    bscore += type_morespecific_(bpara, apara, 1, env);
                    if (jl_is_typevar(bpara) && !jl_is_typevar(apara) && !jl_is_type(apara))
                        ascore1 += 1;
                    if (jl_is_typevar(apara) && !jl_is_typevar(bpara) && !jl_is_type(bpara))
                        bscore1 += 1;
                }
                if (bscore1 == 0 && ascore1 > 0)
                    return 1;
                if (ascore1 == 0 && bscore1 > 0)
                    return 0;
                return ascore == jl_nparams(tta);
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
            return type_morespecific_((jl_value_t*)((jl_tvar_t*)a)->ub,
                                      (jl_value_t*)((jl_tvar_t*)b)->ub, 0, env) &&
                type_morespecific_((jl_value_t*)((jl_tvar_t*)b)->lb,
                                   (jl_value_t*)((jl_tvar_t*)a)->lb, 0, env) &&
                num_occurs((jl_tvar_t*)a, env) >= num_occurs((jl_tvar_t*)b, env);
        }
        if (!jl_is_type(b))
            return 0;
        if (invariant && num_occurs((jl_tvar_t*)a, env) < 2)
            return 0;
        return jl_subtype((jl_value_t*)((jl_tvar_t*)a)->ub, b);
    }
    if (jl_is_typevar(b)) {
        if (!jl_is_type(a))
            return 1;
        if (invariant)
            return type_morespecific_(a, (jl_value_t*)((jl_tvar_t*)b)->ub, 0, env) &&
                !type_morespecific_((jl_value_t*)((jl_tvar_t*)b)->ub, a, 0, env);
        return jl_subtype(a, (jl_value_t*)((jl_tvar_t*)b)->ub) &&
            jl_subtype((jl_value_t*)((jl_tvar_t*)b)->lb, a);
    }
    if ((jl_datatype_t*)a == jl_any_type) return 0;

    return 0;
}

JL_DLLEXPORT int jl_type_morespecific(jl_value_t *a, jl_value_t *b)
{
    if (jl_subtype(a, b)) return 1;
    if (jl_subtype(b, a)) return 0;
    return type_morespecific_(a, b, 0, NULL);
}

// initialization -------------------------------------------------------------

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

    jl_any_type = (jl_datatype_t*)jl_new_abstracttype((jl_value_t*)jl_symbol("Any"), NULL, jl_emptysvec);
    jl_any_type->super = jl_any_type;
    jl_type_type = (jl_unionall_t*)jl_new_abstracttype((jl_value_t*)jl_symbol("Type"), jl_any_type, jl_emptysvec);
    jl_type_typename = ((jl_datatype_t*)jl_type_type)->name;
    jl_type_type_mt = jl_new_method_table(jl_type_typename->name, ptls->current_module);
    jl_type_typename->mt = jl_type_type_mt;

    // initialize them. lots of cycles.
    jl_datatype_type->name = jl_new_typename(jl_symbol("DataType"));
    jl_datatype_type->name->wrapper = (jl_value_t*)jl_datatype_type;
    jl_datatype_type->super = (jl_datatype_t*)jl_type_type;
    jl_datatype_type->parameters = jl_emptysvec;
    jl_datatype_type->name->names = jl_svec(16,
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
                                            jl_symbol("hasfreetypevars"),
                                            jl_symbol("isleaftype"));
    jl_datatype_type->types = jl_svec(16,
                                      jl_typename_type,
                                      jl_datatype_type,
                                      jl_simplevector_type,
                                      jl_simplevector_type,
                                      jl_any_type, // instance
                                      jl_any_type, jl_any_type, jl_any_type, jl_any_type,
                                      jl_any_type, jl_any_type, jl_any_type, jl_any_type,
                                      jl_any_type, jl_any_type, jl_any_type);
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
    jl_typename_type->name->wrapper = (jl_value_t*)jl_typename_type;
    jl_typename_type->name->mt = jl_new_method_table(jl_typename_type->name->name, ptls->current_module);
    jl_typename_type->super = jl_any_type;
    jl_typename_type->parameters = jl_emptysvec;
    jl_typename_type->name->names = jl_svec(8, jl_symbol("name"), jl_symbol("module"),
                                            jl_symbol("names"), jl_symbol("wrapper"),
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
    jl_methtable_type->name->wrapper = (jl_value_t*)jl_methtable_type;
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
    jl_sym_type->name->wrapper = (jl_value_t*)jl_sym_type;
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
    jl_simplevector_type->name->wrapper = (jl_value_t*)jl_simplevector_type;
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

    jl_datatype_t *type_type = (jl_datatype_t*)jl_type_type;
    jl_bottomtype_type = jl_new_datatype(jl_symbol("BottomType"), type_type, jl_emptysvec,
                                         jl_emptysvec, jl_emptysvec, 0, 0, 0);
    jl_bottom_type = jl_new_struct(jl_bottomtype_type);
    jl_bottomtype_type->instance = jl_bottom_type;

    jl_uniontype_type = jl_new_datatype(jl_symbol("Union"), type_type, jl_emptysvec,
                                        jl_svec(2, jl_symbol("a"), jl_symbol("b")),
                                        jl_svec(2, jl_any_type, jl_any_type),
                                        0, 0, 2);

    jl_tvar_type = jl_new_datatype(jl_symbol("TypeVar"), jl_any_type, jl_emptysvec,
                                   jl_svec(3, jl_symbol("name"),
                                           jl_symbol("lb"), jl_symbol("ub")),
                                   jl_svec(3, jl_sym_type,
                                           jl_any_type, jl_any_type),
                                   0, 1, 3);

    jl_unionall_type = jl_new_datatype(jl_symbol("UnionAll"), type_type, jl_emptysvec,
                                       jl_svec(2, jl_symbol("var"), jl_symbol("body")),
                                       jl_svec(2, jl_tvar_type, jl_any_type),
                                       0, 0, 2);

    vararg_sym = jl_symbol("Vararg");
    jl_svec_t *tv;
    tv = jl_svec2(tvar("T"),tvar("N"));
    jl_vararg_type = (jl_unionall_t*)jl_new_abstracttype((jl_value_t*)vararg_sym, jl_any_type, tv)->name->wrapper;
    jl_vararg_typename = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_vararg_type))->name;

    jl_anytuple_type = jl_new_datatype(jl_symbol("Tuple"), jl_any_type, jl_emptysvec,
                                       jl_emptysvec, jl_emptysvec, 0, 0, 0);
    jl_tuple_typename = jl_anytuple_type->name;
    jl_anytuple_type->uid = 0;
    jl_anytuple_type->parameters = jl_svec(1, jl_wrap_vararg((jl_value_t*)jl_any_type, (jl_value_t*)NULL));
    jl_anytuple_type->types = jl_anytuple_type->parameters;
    jl_anytuple_type->layout = NULL;
    jl_anytuple_type->hasfreetypevars = 0;
    jl_anytuple_type->isleaftype = 0;

    jl_tvar_t *tttvar = tvar("T");
    ((jl_datatype_t*)jl_type_type)->parameters = jl_svec(1, tttvar);
    ((jl_datatype_t*)jl_type_type)->hasfreetypevars = 1;
    jl_type_typename->wrapper = jl_new_struct(jl_unionall_type, tttvar, (jl_value_t*)jl_type_type);
    jl_type_type = (jl_unionall_t*)jl_type_typename->wrapper;

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
    jl_abstractarray_type = (jl_unionall_t*)
        jl_new_abstracttype((jl_value_t*)jl_symbol("AbstractArray"),
                            jl_any_type, tv)->name->wrapper;

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_densearray_type = (jl_unionall_t*)
        jl_new_abstracttype((jl_value_t*)jl_symbol("DenseArray"),
                            (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_abstractarray_type, jl_svec_data(tv), 2),
                            tv)->name->wrapper;

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_array_type = (jl_unionall_t*)
        jl_new_datatype(jl_symbol("Array"),
                        (jl_datatype_t*)
                        jl_apply_type((jl_value_t*)jl_densearray_type, jl_svec_data(tv), 2),
                        tv,
                        jl_emptysvec, jl_emptysvec, 0, 1, 0)->name->wrapper;
    jl_array_typename = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_array_type))->name;
    static const jl_datatype_layout_t _jl_array_layout = { 0, sizeof(void*), 0, 0, 0 };
    ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_array_type))->layout = &_jl_array_layout;

    jl_array_any_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_any_type, jl_box_long(1));

    jl_array_symbol_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_sym_type, jl_box_long(1));

    jl_array_uint8_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_uint8_type, jl_box_long(1));

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

    // all kinds of types share a method table
    jl_unionall_type->name->mt = jl_uniontype_type->name->mt = jl_datatype_type->name->mt =
        jl_type_typename->mt;

    jl_intrinsic_type = jl_new_bitstype((jl_value_t*)jl_symbol("IntrinsicFunction"),
                                        jl_builtin_type, jl_emptysvec, 32);

    tv = jl_svec1(tvar("T"));
    jl_ref_type = (jl_unionall_t*)
        jl_new_abstracttype((jl_value_t*)jl_symbol("Ref"), jl_any_type, tv)->name->wrapper;

    tv = jl_svec1(tvar("T"));
    jl_pointer_type = (jl_unionall_t*)
        jl_new_bitstype((jl_value_t*)jl_symbol("Ptr"),
                        (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_ref_type, jl_svec_data(tv), 1), tv,
                        sizeof(void*)*8)->name->wrapper;
    jl_pointer_typename = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_pointer_type))->name;

    // U T<:Tuple Type{T}
    tttvar = jl_new_typevar(jl_symbol("T"),
                            (jl_value_t*)jl_bottom_type,
                            (jl_value_t*)jl_anytuple_type);
    jl_anytuple_type_type = (jl_unionall_t*)jl_new_struct(jl_unionall_type,
                                                          tttvar, (jl_value_t*)jl_wrap_Type((jl_value_t*)tttvar));

    // Type{T}
    jl_typetype_tvar = tvar("T");
    jl_typetype_type =
        (jl_unionall_t*)jl_new_struct(jl_unionall_type, jl_typetype_tvar,
                                      jl_apply_type1((jl_value_t*)jl_type_type, (jl_value_t*)jl_typetype_tvar));

    jl_ANY_flag = (jl_value_t*)tvar("ANY");

    jl_abstractstring_type = jl_new_abstracttype((jl_value_t*)jl_symbol("AbstractString"), jl_any_type, jl_emptysvec);
    jl_string_type = jl_new_datatype(jl_symbol("String"), jl_abstractstring_type, jl_emptysvec,
                                     jl_svec1(jl_symbol("len")), jl_svec1(jl_long_type),
                                     0, 1, 1);

    // complete builtin type metadata
    jl_value_t *pointer_void = jl_apply_type1((jl_value_t*)jl_pointer_type, (jl_value_t*)jl_void_type);
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
    jl_svecset(jl_simplevector_type->types, 0, jl_long_type);
    jl_svecset(jl_typename_type->types, 1, jl_module_type);
    jl_svecset(jl_typename_type->types, 6, jl_long_type);
    jl_svecset(jl_typename_type->types, 3, jl_type_type);
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
    jl_compute_field_offsets(jl_unionall_type);
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

    jl_cfunction_list.unknown = jl_nothing;
}

#ifdef __cplusplus
}
#endif
