// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "julia.h"
#include "julia_internal.h"
#ifndef _OS_WINDOWS_
#include <unistd.h>
#endif

#define MAX_METHLIST_COUNT 12 // this can strongly affect the sysimg size and speed!
#define INIT_CACHE_SIZE 8 // must be a power-of-two

#ifdef __cplusplus
extern "C" {
#endif

// compute whether the specificity of this type is equivalent to Any in the sort order
static int jl_is_any(jl_value_t *t1)
{
    return (t1 == (jl_value_t*)jl_any_type ||
            (jl_is_typevar(t1) &&
             ((jl_tvar_t*)t1)->ub == (jl_value_t*)jl_any_type &&
             !((jl_tvar_t*)t1)->bound));
}

// ----- Type Signature Subtype Testing ----- //

static int sig_match_by_type_leaf(jl_value_t **types, jl_tupletype_t *sig, size_t n)
{
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *decl = jl_field_type(sig, i);
        jl_value_t *a = types[i];
        if (jl_is_type_type(a)) // decl is not Type, because it wouldn't be leafsig
            a = jl_typeof(jl_tparam0(a));
        if (!jl_types_equal(a, decl))
            return 0;
    }
    return 1;
}

static int sig_match_by_type_simple(jl_value_t **types, size_t n, jl_tupletype_t *sig, size_t lensig, int va)
{
    size_t i;
    if (va) lensig -= 1;
    for (i = 0; i < lensig; i++) {
        jl_value_t *decl = jl_field_type(sig, i);
        jl_value_t *a = types[i];
        if (jl_is_type_type(decl)) {
            jl_value_t *tp0 = jl_tparam0(decl);
            if (jl_is_type_type(a)) {
                if (tp0 == (jl_value_t*)jl_typetype_tvar) {
                    // in the case of Type{T}, the types don't have
                    // to match exactly either. this is cached as Type{T}.
                    // analogous to the situation with tuples.
                }
                else if (jl_is_typevar(tp0)) {
                    if (!jl_subtype(jl_tparam0(a), ((jl_tvar_t*)tp0)->ub, 0))
                        return 0;
                }
                else {
                    if (!jl_types_equal(jl_tparam0(a), tp0))
                        return 0;
                }
            }
            else if (!is_kind(a) || !jl_is_typevar(tp0) || ((jl_tvar_t*)tp0)->ub != (jl_value_t*)jl_any_type) {
                // manually unroll jl_subtype(a, decl)
                // where `a` can be a subtype like TypeConstructor
                // and decl is Type{T}
                return 0;
            }
        }
        else if (decl == (jl_value_t*)jl_any_type) {
        }
        else {
            if (jl_is_type_type(a)) // decl is not Type, because it would be caught above
                a = jl_typeof(jl_tparam0(a));
            if (!jl_types_equal(a, decl))
                return 0;
        }
    }
    if (va) {
        jl_value_t *decl = jl_field_type(sig, i);
        if (jl_vararg_kind(decl) == JL_VARARG_INT) {
            if (n-i != jl_unbox_long(jl_tparam1(decl)))
                return 0;
        }
        jl_value_t *t = jl_tparam0(decl);
        for(; i < n; i++) {
            if (!jl_subtype(types[i], t, 0))
                return 0;
        }
        return 1;
    }
    return 1;
}

static inline int sig_match_leaf(jl_value_t **args, jl_value_t **sig, size_t n)
{
    // NOTE: This function is a huge performance hot spot!!
    size_t i;
    for (i = 0; i < n; i++) {
        jl_value_t *decl = sig[i];
        jl_value_t *a = args[i];
        if ((jl_value_t*)jl_typeof(a) != decl) {
            /*
              we are only matching concrete types here, and those types are
              hash-consed, so pointer comparison should work.
            */
            return 0;
        }
    }
    return 1;
}

static inline int sig_match_simple(jl_value_t **args, size_t n, jl_value_t **sig,
                                   int va, size_t lensig)
{
    // NOTE: This function is a performance hot spot!!
    size_t i;
    if (va) lensig -= 1;
    for (i = 0; i < lensig; i++) {
        jl_value_t *decl = sig[i];
        jl_value_t *a = args[i];
        if (decl == (jl_value_t*)jl_any_type) {
        }
        else if ((jl_value_t*)jl_typeof(a) == decl) {
            /*
              we are only matching concrete types here, and those types are
              hash-consed, so pointer comparison should work.
            */
        }
        else if (jl_is_type_type(decl) && jl_is_type(a)) {
            jl_value_t *tp0 = jl_tparam0(decl);
            if (tp0 == (jl_value_t*)jl_typetype_tvar) {
                // in the case of Type{T}, the types don't have
                // to match exactly either. this is cached as Type{T}.
                // analogous to the situation with tuples.
            }
            else if (jl_is_typevar(tp0)) {
                if (!jl_subtype(a, ((jl_tvar_t*)tp0)->ub, 0))
                    return 0;
            }
            else {
                if (a!=tp0 && !jl_types_equal(a,tp0))
                    return 0;
            }
        }
        else {
            return 0;
        }
    }
    if (va) {
        jl_value_t *decl = sig[i];
        if (jl_vararg_kind(decl) == JL_VARARG_INT) {
            if (n-i != jl_unbox_long(jl_tparam1(decl)))
                return 0;
        }
        jl_value_t *t = jl_tparam0(decl);
        for(; i < n; i++) {
            if (!jl_subtype(args[i], t, 1))
                return 0;
        }
        return 1;
    }
    return 1;
}


// ----- MethodCache helper functions ----- //

static inline size_t jl_intref(const jl_array_t *arr, size_t idx)
{
    jl_value_t *el = jl_tparam0(jl_typeof(arr));
    if (el == (jl_value_t*)jl_uint8_type)
        return ((uint8_t*)jl_array_data(arr))[idx];
    else if (el == (jl_value_t*)jl_uint16_type)
        return ((uint16_t*)jl_array_data(arr))[idx];
    else if (el == (jl_value_t*)jl_uint32_type)
        return ((uint32_t*)jl_array_data(arr))[idx];
    else
        abort();
}

static inline void jl_intset(const jl_array_t *arr, size_t idx, size_t val)
{
    jl_value_t *el = jl_tparam0(jl_typeof(arr));
    if (el == (jl_value_t*)jl_uint8_type)
        ((uint8_t*)jl_array_data(arr))[idx] = val;
    else if (el == (jl_value_t*)jl_uint16_type)
        ((uint16_t*)jl_array_data(arr))[idx] = val;
    else if (el == (jl_value_t*)jl_uint32_type)
        ((uint32_t*)jl_array_data(arr))[idx] = val;
    else
        abort();
}

static inline size_t jl_max_int(const jl_array_t *arr)
{
    jl_value_t *el = jl_tparam0(jl_typeof(arr));
    if (el == (jl_value_t*)jl_uint8_type)
        return 0xFF;
    else if (el == (jl_value_t*)jl_uint16_type)
        return 0xFFFF;
    else if (el == (jl_value_t*)jl_uint32_type)
        return 0xFFFFFFFF;
    else
        abort();
}


static jl_array_t *jl_alloc_int_1d(size_t np, size_t len)
{
    jl_value_t *ty;
    if (np < 0xFF) {
        ty = jl_array_uint8_type;
    }
    else if (np < 0xFFFF) {
        static jl_value_t *int16 = NULL;
        if (int16 == NULL)
            int16 = jl_apply_array_type(jl_uint16_type, 1);
        ty = int16;
    }
    else {
        assert(np < 0x7FFFFFFF);
        static jl_value_t *int32 = NULL;
        if (int32 == NULL)
            int32 = jl_apply_array_type(jl_uint32_type, 1);
        ty = int32;
    }
    jl_array_t *a = jl_alloc_array_1d(ty, len);
    memset(a->data, 0, len * a->elsize);
    return a;
}

static inline
union jl_typemap_t mtcache_hash_lookup(const struct jl_ordereddict_t *a, jl_value_t *ty, int8_t tparam, int8_t offs)
{
    uintptr_t uid = ((jl_datatype_t*)ty)->uid;
    union jl_typemap_t ml;
    ml.unknown = jl_nothing;
    if (!uid)
        return ml;
    size_t idx = jl_intref(a->indexes, uid & (a->indexes->nrows-1));
    if (idx > 0) {
        ml.unknown = jl_array_ptr_ref(a->values, idx - 1);
        if (ml.unknown == jl_nothing)
            return ml;
        jl_value_t *t;
        if (jl_typeof(ml.unknown) == (jl_value_t*)jl_typemap_level_type) {
            t = ml.node->key;
        }
        else {
            assert(jl_typeof(ml.unknown) == (jl_value_t*)jl_typemap_entry_type);
            t = jl_field_type(ml.leaf->sig, offs);
            if (tparam)
                t = jl_tparam0(t);
        }
        if (t != ty)
            ml.unknown = jl_nothing;
    }
    return ml;
}

static void mtcache_rehash(struct jl_ordereddict_t *pa, size_t newlen, jl_value_t *parent, int8_t tparam, int8_t offs)
{
    size_t i, nval = jl_array_len(pa->values);
    jl_array_t *n = jl_alloc_int_1d(nval + 1, newlen);
    for (i = 1; i <= nval; i++) {
        union jl_typemap_t ml;
        ml.unknown = jl_array_ptr_ref(pa->values, i - 1);
        if (ml.unknown == jl_nothing)
            continue;
        jl_value_t *t;
        if (jl_typeof(ml.unknown) == (jl_value_t*)jl_typemap_level_type) {
            t = ml.node->key;
        }
        else {
            assert(jl_typeof(ml.unknown) == (jl_value_t*)jl_typemap_entry_type);
            t = jl_field_type(ml.leaf->sig, offs);
            if (tparam)
                t = jl_tparam0(t);
        }
        uintptr_t uid = ((jl_datatype_t*)t)->uid;
        size_t newi = uid & (newlen - 1);
        if (jl_intref(n, newi) == 0) {
            jl_intset(n, newi, i);
        }
        else {
            // hash collision: start over after doubling the size again
            i = 0;
            newlen *= 2;
            n = jl_alloc_int_1d(nval + 1, newlen);
        }
    }
    pa->indexes = n;
    jl_gc_wb(parent, n);
}

// Recursively rehash a TypeMap (for example, after deserialization)
void jl_typemap_rehash(union jl_typemap_t ml, int8_t offs);
void jl_typemap_rehash_array(struct jl_ordereddict_t *pa, jl_value_t *parent, int8_t tparam, int8_t offs)
{
    size_t i, len = jl_array_len(pa->values);
    for (i = 0; i < len; i++) {
        union jl_typemap_t ml;
        ml.unknown = jl_array_ptr_ref(pa->values, i);
        assert(ml.unknown != NULL);
        jl_typemap_rehash(ml, offs+1);
    }
    mtcache_rehash(pa, 4 * next_power_of_two(len), parent, tparam, offs);
}
void jl_typemap_rehash(union jl_typemap_t ml, int8_t offs) {
    if (jl_typeof(ml.unknown) == (jl_value_t*)jl_typemap_level_type) {
        if (ml.node->targ.values != (void*)jl_nothing)
            jl_typemap_rehash_array(&ml.node->targ, ml.unknown, 1, offs);
        if (ml.node->arg1.values != (void*)jl_nothing)
            jl_typemap_rehash_array(&ml.node->arg1, ml.unknown, 0, offs);
        jl_typemap_rehash(ml.node->any, offs+1);
    }
}

static union jl_typemap_t *mtcache_hash_bp(struct jl_ordereddict_t *pa, jl_value_t *ty,
                                           int8_t tparam, int8_t offs, jl_value_t *parent)
{
    if (jl_is_datatype(ty)) {
        uintptr_t uid = ((jl_datatype_t*)ty)->uid;
        if (!uid || is_kind(ty) || jl_has_typevars(ty))
            // be careful not to put non-leaf types or DataType/TypeConstructor in the cache here,
            // since they should have a lower priority and need to go into the sorted list
            return NULL;
        if (pa->values == (void*)jl_nothing) {
            pa->indexes = jl_alloc_int_1d(0, INIT_CACHE_SIZE);
            jl_gc_wb(parent, pa->indexes);
            pa->values = jl_alloc_vec_any(0);
            jl_gc_wb(parent, pa->values);
        }
        while (1) {
            size_t slot = uid & (pa->indexes->nrows - 1);
            size_t idx = jl_intref(pa->indexes, slot);
            if (idx == 0) {
                jl_array_ptr_1d_push(pa->values, jl_nothing);
                idx = jl_array_len(pa->values);
                if (idx > jl_max_int(pa->indexes))
                    mtcache_rehash(pa, jl_array_len(pa->indexes), parent, tparam, offs);
                jl_intset(pa->indexes, slot, idx);
                return &((union jl_typemap_t*)jl_array_data(pa->values))[idx - 1];
            }
            union jl_typemap_t *pml = &((union jl_typemap_t*)jl_array_data(pa->values))[idx - 1];
            if (pml->unknown == jl_nothing)
                return pml;
            jl_value_t *t;
            if (jl_typeof(pml->unknown) == (jl_value_t*)jl_typemap_level_type) {
                t = pml->node->key;
            }
            else {
                assert(jl_typeof(pml->unknown) == (jl_value_t*)jl_typemap_entry_type);
                t = jl_field_type(pml->leaf->sig, offs);
                if (tparam)
                    t = jl_tparam0(t);
            }
            if (t == ty)
                return pml;
            mtcache_rehash(pa, jl_array_len(pa->indexes) * 2, parent, tparam, offs);
        }
    }
    return NULL;
}

// ----- Sorted Type Signature Lookup Matching ----- //

jl_value_t *jl_lookup_match(jl_value_t *a, jl_value_t *b, jl_svec_t **penv, jl_svec_t *tvars)
{
    jl_value_t *ti = jl_type_intersection_matching(a, b, penv, tvars);
    if (ti == (jl_value_t*)jl_bottom_type)
        return ti;
    JL_GC_PUSH1(&ti);
    assert(jl_is_svec(*penv));
    int l = jl_svec_len(*penv);
    for(int i=0; i < l; i++) {
        jl_value_t *val = jl_svecref(*penv,i);
        /*
          since "a" is a concrete type, we assume that
          (a∩b != Union{}) => a<:b. However if a static parameter is
          forced to equal Union{}, then part of "b" might become Union{},
          and therefore a subtype of "a". For example
          (Type{Union{}},Int) ∩ (Type{T},T)
          issue #5254
        */
        if (val == (jl_value_t*)jl_bottom_type) {
            if (!jl_subtype(a, ti, 0)) {
                JL_GC_POP();
                return (jl_value_t*)jl_bottom_type;
            }
        }
    }
    JL_GC_POP();
    return ti;
}

static int jl_typemap_array_visitor(struct jl_ordereddict_t *a, jl_typemap_visitor_fptr fptr, void *closure)
{
    size_t i, l = jl_array_len(a->values);
    union jl_typemap_t *data = (union jl_typemap_t*)jl_array_data(a->values);
    for(i=0; i < l; i++) {
        if (!jl_typemap_visitor(data[i], fptr, closure))
            return 0;
    }
    return 1;
}

// calls fptr on each jl_typemap_entry_t in cache in sort order, until fptr return false
static int jl_typemap_node_visitor(jl_typemap_entry_t *ml, jl_typemap_visitor_fptr fptr, void *closure)
{
    while (ml != (void*)jl_nothing) {
        if (!fptr(ml, closure))
            return 0;
        ml = ml->next;
    }
    return 1;
}

int jl_typemap_visitor(union jl_typemap_t cache, jl_typemap_visitor_fptr fptr, void *closure)
{
    if (jl_typeof(cache.unknown) == (jl_value_t*)jl_typemap_level_type) {
        if (cache.node->targ.values != (void*)jl_nothing)
            if (!jl_typemap_array_visitor(&cache.node->targ, fptr, closure))
                return 0;
        if (cache.node->arg1.values != (void*)jl_nothing)
            if (!jl_typemap_array_visitor(&cache.node->arg1, fptr, closure))
                return 0;
        if (!jl_typemap_node_visitor(cache.node->linear, fptr, closure))
            return 0;
        return jl_typemap_visitor(cache.node->any, fptr, closure);
    }
    else {
        return jl_typemap_node_visitor(cache.leaf, fptr, closure);
    }
}

// predicate to fast-test if this type is a leaf type that can exist in the cache
// and does not need a more expensive linear scan to find all intersections
int is_cache_leaf(jl_value_t *ty)
{
    return (jl_is_datatype(ty) && ((jl_datatype_t*)ty)->uid != 0 && !is_kind(ty));
}

static int jl_typemap_intersection_array_visitor(struct jl_ordereddict_t *a, jl_value_t *ty, int tparam,
                                                 int offs, struct typemap_intersection_env *closure)
{
    size_t i, l = jl_array_len(a->values);
    union jl_typemap_t *data = (union jl_typemap_t*)jl_array_data(a->values);
    for (i = 0; i < l; i++) {
        union jl_typemap_t ml = data[i];
        if (ml.unknown == jl_nothing)
            continue;
        jl_value_t *t;
        if (jl_typeof(ml.unknown) == (jl_value_t*)jl_typemap_level_type) {
            t = ml.node->key;
        }
        else {
            t = jl_field_type(ml.leaf->sig, offs);
            if (tparam)
                t = jl_tparam0(t);
        }
        if (ty == (jl_value_t*)jl_any_type || // easy case: Any always matches
            (tparam ?  // need to compute `ty <: Type{t}`
             (jl_is_uniontype(ty) || // punt on Union{...} right now
              jl_typeof(t) == ty || // deal with kinds (e.g. ty == DataType && t == Type{t})
              (jl_is_type_type(ty) && (jl_is_typevar(jl_tparam0(ty)) ?
                                       jl_subtype(t, ((jl_tvar_t*)jl_tparam0(ty))->ub, 0) : // deal with ty == Type{<:T}
                                       jl_subtype(t, jl_tparam0(ty), 0)))) // deal with ty == Type{T{#<:T}}
                    : jl_subtype(t, ty, 0))) // `t` is a leaftype, so intersection test becomes subtype
            if (!jl_typemap_intersection_visitor(ml, offs+1, closure))
                return 0;
    }
    return 1;
}

// calls fptr on each jl_typemap_entry_t in cache in sort order
// for which type ∩ ml->type != Union{}, until fptr return false
static int jl_typemap_intersection_node_visitor(jl_typemap_entry_t *ml, struct typemap_intersection_env *closure)
{
    // slow-path scan everything in ml
    // mark this `register` because (for branch prediction)
    // that can be absolutely critical for speed
    register jl_typemap_intersection_visitor_fptr fptr = closure->fptr;
    while (ml != (void*)jl_nothing) {
        if (closure->type == (jl_value_t*)ml->sig) {
            // fast-path for the intersection of a type with itself
            if (closure->env) {
                if (jl_is_typevar(ml->tvars))
                    closure->env = jl_svec1(ml->tvars);
                else
                    closure->env = ml->tvars;
            }
            closure->ti = closure->type;
            if (!fptr(ml, closure))
                return 0;
        }
        else {
            jl_value_t *ti;
            if (closure->env) {
                closure->env = jl_emptysvec;
                ti = jl_lookup_match(closure->type, (jl_value_t*)ml->sig, &closure->env, ml->tvars);
            }
            else {
                ti = jl_type_intersection(closure->type, (jl_value_t*)ml->sig);
            }
            if (ti != (jl_value_t*)jl_bottom_type) {
                closure->ti = ti;
                if (!fptr(ml, closure))
                    return 0;
            }
        }
        ml = ml->next;
    }
    return 1;
}

int jl_typemap_intersection_visitor(union jl_typemap_t map, int offs,
                                    struct typemap_intersection_env *closure)
{
    if (jl_typeof(map.unknown) == (jl_value_t*)jl_typemap_level_type) {
        jl_typemap_level_t *cache = map.node;
        jl_value_t *ty = NULL;
        size_t l = jl_field_count(closure->type);
        if (closure->va && l <= offs + 1) {
            ty = closure->va;
        }
        else if (l > offs) {
            ty = jl_tparam(closure->type, offs);
        }
        if (ty) {
            if (cache->targ.values != (void*)jl_nothing) {
                jl_value_t *typetype = jl_is_type_type(ty) ? jl_tparam0(ty) : NULL;
                if (typetype && !jl_has_typevars(typetype)) {
                    if (is_cache_leaf(typetype)) {
                        // direct lookup of leaf types
                        union jl_typemap_t ml = mtcache_hash_lookup(&cache->targ, typetype, 1, offs);
                        if (ml.unknown != jl_nothing) {
                            if (!jl_typemap_intersection_visitor(ml, offs+1, closure)) return 0;
                        }
                    }
                }
                else {
                    // else an array scan is required to check subtypes
                    // first, fast-path: optimized pre-intersection test to see if `ty` could intersect with any Type
                    if (typetype || jl_type_intersection((jl_value_t*)jl_type_type, ty) != jl_bottom_type)
                        if (!jl_typemap_intersection_array_visitor(&cache->targ, ty, 1, offs, closure)) return 0;
                }
            }
            if (cache->arg1.values != (void*)jl_nothing) {
                if (is_cache_leaf(ty)) {
                    // direct lookup of leaf types
                    union jl_typemap_t ml = mtcache_hash_lookup(&cache->arg1, ty, 0, offs);
                    if (ml.unknown != jl_nothing) {
                        if (!jl_typemap_intersection_visitor(ml, offs+1, closure)) return 0;
                    }
                }
                else {
                    // else an array scan is required to check subtypes
                    if (!jl_typemap_intersection_array_visitor(&cache->arg1, ty, 0, offs, closure)) return 0;
                }
            }
        }
        if (!jl_typemap_intersection_node_visitor(map.node->linear, closure))
            return 0;
        return jl_typemap_intersection_visitor(map.node->any, offs+1, closure);
    }
    else {
        return jl_typemap_intersection_node_visitor(map.leaf, closure);
    }
}

int sigs_eq(jl_value_t *a, jl_value_t *b, int useenv)
{
    // useenv == 0 : subtyping + ensure typevars correspond
    // useenv == 1 : subtyping + ensure typevars correspond + fail if bound != bound in some typevar match
    // useenv == 2 : ignore typevars (because UnionAll getting lost in intersection can cause jl_types_equal to fail in the wrong direction for some purposes)
    if (useenv != 2 && (jl_has_typevars(a) || jl_has_typevars(b))) {
        return jl_types_equal_generic(a, b, useenv);
    }
    return jl_subtype(a, b, 0) && jl_subtype(b, a, 0);
}

/*
  Method caches are divided into three parts: one for signatures where
  the first argument is a singleton kind (Type{Foo}), one indexed by the
  UID of the first argument's type in normal cases, and a fallback
  table of everything else.

  Note that the "primary key" is the type of the first *argument*, since
  there tends to be lots of variation there. The type of the 0th argument
  (the function) is always the same for most functions.
*/
static jl_typemap_entry_t *jl_typemap_assoc_by_type_(jl_typemap_entry_t *ml, jl_tupletype_t *types,
                                                     int8_t inexact, jl_svec_t **penv, size_t world)
{
    size_t n = jl_field_count(types);
    int typesisva = n == 0 ? 0 : jl_is_vararg_type(jl_tparam(types, n-1));
    for (; ml != (void*)jl_nothing; ml = ml->next) {
        if (world < ml->min_world || world > ml->max_world)
            continue; // ignore replaced methods
        size_t lensig = jl_field_count(ml->sig);
        if (lensig == n || (ml->va && lensig <= n+1)) {
            int resetenv = 0, ismatch = 1;
            if (ml->simplesig != (void*)jl_nothing) {
                size_t lensimplesig = jl_field_count(ml->simplesig);
                int isva = lensimplesig > 0 && jl_is_vararg_type(jl_tparam(ml->simplesig, lensimplesig - 1));
                if (lensig == n || (isva && lensimplesig <= n + 1))
                    ismatch = sig_match_by_type_simple(jl_svec_data(types->parameters), n,
                                                       ml->simplesig, lensimplesig, isva);
                else
                    ismatch = 0;
            }

            if (ismatch == 0)
                ; // nothing
            else if (ml->isleafsig && !typesisva)
                ismatch = sig_match_by_type_leaf(jl_svec_data(types->parameters),
                                                 ml->sig, lensig);
            else if (ml->issimplesig && !typesisva)
                ismatch = sig_match_by_type_simple(jl_svec_data(types->parameters), n,
                                                   ml->sig, lensig, ml->va);
            else if (ml->tvars == jl_emptysvec)
                ismatch = jl_tuple_subtype(jl_svec_data(types->parameters), n, ml->sig, 0);
            else if (penv == NULL) {
                ismatch = jl_type_match((jl_value_t*)types, (jl_value_t*)ml->sig) != (jl_value_t*)jl_false;
            }
            else {
                // TODO: this is missing the actual subtype test,
                // which works currently because types is typically a leaf tt,
                // or inexact is set (which then does a sort of subtype test via jl_types_equal)
                // but this isn't entirely general
                jl_value_t *ti = jl_lookup_match((jl_value_t*)types, (jl_value_t*)ml->sig, penv, ml->tvars);
                resetenv = 1;
                ismatch = (ti != (jl_value_t*)jl_bottom_type);
                if (ismatch) {
                    // parametric methods only match if all typevars are matched by
                    // non-typevars.
                    size_t i, l;
                    for (i = 0, l = jl_svec_len(*penv); i < l; i++) {
                        if (jl_is_typevar(jl_svecref(*penv, i))) {
                            if (inexact) {
                                // "inexact" means the given type is compile-time,
                                // where a failure to determine the value of a
                                // static parameter is inconclusive.
                                // this is issue #3182, see test/core.jl
                                return INEXACT_ENTRY;
                            }
                            ismatch = 0;
                            break;
                        }
                    }
                    if (inexact) {
                        // the compiler might attempt jl_get_specialization on e.g.
                        // convert(::Type{Type{Int}}, ::DataType), which is concrete but might not
                        // equal the run time type. in this case ti would be {Type{Type{Int}}, Type{Int}}
                        // but tt would be {Type{Type{Int}}, DataType}.
                        JL_GC_PUSH1(&ti);
                        ismatch = jl_types_equal(ti, (jl_value_t*)types);
                        JL_GC_POP();
                        if (!ismatch)
                            return INEXACT_ENTRY;
                    }
                }
            }

            if (ismatch) {
                size_t i, l;
                for (i = 0, l = jl_svec_len(ml->guardsigs); i < l; i++) {
                    // see corresponding code in jl_typemap_assoc_exact
                    if (jl_subtype((jl_value_t*)types, jl_svecref(ml->guardsigs, i), 0)) {
                        ismatch = 0;
                        break;
                    }
                }
                if (ismatch)
                    return ml;
            }
            if (resetenv)
                *penv = jl_emptysvec;
        }
    }
    return NULL;
}

static jl_typemap_entry_t *jl_typemap_lookup_by_type_(jl_typemap_entry_t *ml, jl_tupletype_t *types, int8_t useenv, size_t world)
{
    for (; ml != (void*)jl_nothing; ml = ml->next) {
        if (world < ml->min_world || world > ml->max_world)
            continue;
        // TODO: more efficient
        if (sigs_eq((jl_value_t*)types, (jl_value_t*)ml->sig, useenv)) {
            return ml;
        }
    }
    return NULL;
}


// this is the general entry point for looking up a type in the cache
// (as a subtype, or with typeseq)
jl_typemap_entry_t *jl_typemap_assoc_by_type(union jl_typemap_t ml_or_cache, jl_tupletype_t *types, jl_svec_t **penv,
                                             int8_t subtype_inexact__sigseq_useenv, int8_t subtype, int8_t offs, size_t world)
{
    if (jl_typeof(ml_or_cache.unknown) == (jl_value_t*)jl_typemap_level_type) {
        jl_typemap_level_t *cache = ml_or_cache.node;
        // called object is the primary key for constructors, otherwise first argument
        jl_value_t *ty = NULL;
        size_t l = jl_field_count(types);
        int isva = 0;
        // compute the type at offset `offs` into `types`, which may be a Vararg
        if (l <= offs + 1) {
            ty = jl_tparam(types, l - 1);
            if (jl_is_vararg_type(ty)) {
                ty = jl_tparam0(ty);
                isva = 1;
            }
            else if (l <= offs) {
                ty = NULL;
            }
        }
        else if (l > offs) {
            ty = jl_tparam(types, offs);
        }
        // If there is a type at offs, look in the optimized caches
        if (!subtype) {
            if (ty && jl_is_any(ty))
                return jl_typemap_assoc_by_type(cache->any, types, penv, subtype_inexact__sigseq_useenv, subtype, offs+1, world);
            if (isva) // in lookup mode, want to match Vararg exactly, not as a subtype
                ty = NULL;
        }
        if (ty) {
            if (jl_is_type_type(ty)) {
                jl_value_t *a0 = jl_tparam0(ty);
                if (cache->targ.values != (void*)jl_nothing && jl_is_datatype(a0)) {
                    union jl_typemap_t ml = mtcache_hash_lookup(&cache->targ, a0, 1, offs);
                    if (ml.unknown != jl_nothing) {
                        jl_typemap_entry_t *li = jl_typemap_assoc_by_type(ml, types, penv,
                                subtype_inexact__sigseq_useenv, subtype, offs+1, world);
                        if (li) return li;
                    }
                }
                if (!subtype && is_cache_leaf(a0)) return NULL;
            }
            if (cache->arg1.values != (void*)jl_nothing && jl_is_datatype(ty)) {
                union jl_typemap_t ml = mtcache_hash_lookup(&cache->arg1, ty, 0, offs);
                if (ml.unknown != jl_nothing) {
                    jl_typemap_entry_t *li = jl_typemap_assoc_by_type(ml, types, penv,
                            subtype_inexact__sigseq_useenv, subtype, offs+1, world);
                    if (li) return li;
                }
            }
            if (!subtype && is_cache_leaf(ty)) return NULL;
        }
        // Always check the list (since offs doesn't always start at 0)
        if (subtype) {
            jl_typemap_entry_t *li = jl_typemap_assoc_by_type_(cache->linear, types, subtype_inexact__sigseq_useenv, penv, world);
            if (li) return li;
            return jl_typemap_assoc_by_type(cache->any, types, penv, subtype_inexact__sigseq_useenv, subtype, offs+1, world);
        }
        else {
            return jl_typemap_lookup_by_type_(cache->linear, types, subtype_inexact__sigseq_useenv, world);
        }
    }
    else {
        return subtype ?
            jl_typemap_assoc_by_type_(ml_or_cache.leaf, types, subtype_inexact__sigseq_useenv, penv, world) :
            jl_typemap_lookup_by_type_(ml_or_cache.leaf, types, subtype_inexact__sigseq_useenv, world);
    }
}

jl_typemap_entry_t *jl_typemap_entry_assoc_exact(jl_typemap_entry_t *ml, jl_value_t **args, size_t n, size_t world)
{
    // some manually-unrolled common special cases
    while (ml->simplesig == (void*)jl_nothing && ml->guardsigs == jl_emptysvec && ml->isleafsig) {
        // use a tight loop for a long as possible
        if (world >= ml->min_world && world <= ml->max_world) {
            if (n == jl_field_count(ml->sig) && jl_typeof(args[0]) == jl_tparam(ml->sig, 0)) {
                if (n == 1)
                    return ml;
                if (n == 2) {
                    if (jl_typeof(args[1]) == jl_tparam(ml->sig, 1))
                        return ml;
                }
                else if (n == 3) {
                    if (jl_typeof(args[1]) == jl_tparam(ml->sig, 1) &&
                        jl_typeof(args[2]) == jl_tparam(ml->sig, 2))
                        return ml;
                }
                else {
                    if (sig_match_leaf(args, jl_svec_data(ml->sig->parameters), n))
                        return ml;
                }
            }
        }
        ml = ml->next;
        if (ml == (void*)jl_nothing)
            return NULL;
    }

    for (; ml != (void*)jl_nothing; ml = ml->next) {
        if (world < ml->min_world || world > ml->max_world)
            continue; // ignore replaced methods
        size_t lensig = jl_field_count(ml->sig);
        if (lensig == n || (ml->va && lensig <= n+1)) {
            if (ml->simplesig != (void*)jl_nothing) {
                size_t lensimplesig = jl_field_count(ml->simplesig);
                int isva = lensimplesig > 0 && jl_is_vararg_type(jl_tparam(ml->simplesig, lensimplesig - 1));
                if (lensig == n || (isva && lensimplesig <= n + 1)) {
                    if (!sig_match_simple(args, n, jl_svec_data(ml->simplesig->parameters), isva, lensimplesig))
                        continue;
                }
                else {
                    continue;
                }
            }

            if (ml->isleafsig) {
                if (!sig_match_leaf(args, jl_svec_data(ml->sig->parameters), n))
                    continue;
            }
            else if (ml->issimplesig) {
                if (!sig_match_simple(args, n, jl_svec_data(ml->sig->parameters), ml->va, lensig))
                    continue;
            }
            else {
                if (!jl_tuple_subtype(args, n, ml->sig, 1))
                    continue;
            }

            size_t i, l;
            if (ml->guardsigs != jl_emptysvec) {
                for (i = 0, l = jl_svec_len(ml->guardsigs); i < l; i++) {
                    // checking guard entries require a more
                    // expensive subtype check, since guard entries added for ANY might be
                    // abstract. this fixed issue #12967.
                    if (jl_tuple_subtype(args, n, (jl_tupletype_t*)jl_svecref(ml->guardsigs, i), 1)) {
                        goto nomatch;
                    }
                }
            }
            return ml;
nomatch:
            continue;
        }
    }
    return NULL;
}

jl_typemap_entry_t *jl_typemap_level_assoc_exact(jl_typemap_level_t *cache, jl_value_t **args, size_t n, int8_t offs, size_t world)
{
    if (n > offs) {
        jl_value_t *a1 = args[offs];
        jl_value_t *ty = (jl_value_t*)jl_typeof(a1);
        assert(jl_is_datatype(ty));
        if (ty == (jl_value_t*)jl_datatype_type && cache->targ.values != (void*)jl_nothing) {
            union jl_typemap_t ml_or_cache = mtcache_hash_lookup(&cache->targ, a1, 1, offs);
            jl_typemap_entry_t *ml = jl_typemap_assoc_exact(ml_or_cache, args, n, offs+1, world);
            if (ml) return ml;
        }
        if (cache->arg1.values != (void*)jl_nothing) {
            union jl_typemap_t ml_or_cache = mtcache_hash_lookup(&cache->arg1, ty, 0, offs);
            jl_typemap_entry_t *ml = jl_typemap_assoc_exact(ml_or_cache, args, n, offs+1, world);
            if (ml) return ml;
        }
    }
    if (cache->linear != (jl_typemap_entry_t*)jl_nothing) {
        jl_typemap_entry_t *ml = jl_typemap_entry_assoc_exact(cache->linear, args, n, world);
        if (ml) return ml;
    }
    if (cache->any.unknown != jl_nothing)
        return jl_typemap_assoc_exact(cache->any, args, n, offs+1, world);
    return NULL;
}


// ----- Method List Insertion Management ----- //

static unsigned jl_typemap_list_count(jl_typemap_entry_t *ml)
{
    unsigned count = 0;
    while (ml != (void*)jl_nothing) {
        count++;
        ml = ml->next;
    }
    return count;
}

static void jl_typemap_level_insert_(jl_typemap_level_t *cache, jl_typemap_entry_t *newrec, int8_t offs, const struct jl_typemap_info *tparams);
static void jl_typemap_list_insert_sorted(jl_typemap_entry_t **pml, jl_value_t *parent,
                                          jl_typemap_entry_t *newrec, const struct jl_typemap_info *tparams);

static jl_typemap_level_t *jl_new_typemap_level(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_typemap_level_t *cache =
        (jl_typemap_level_t*)jl_gc_alloc(ptls, sizeof(jl_typemap_level_t),
                                         jl_typemap_level_type);
    cache->key = NULL;
    cache->linear = (jl_typemap_entry_t*)jl_nothing;
    cache->any.unknown = jl_nothing;
    cache->targ.indexes = (jl_array_t*)jl_nothing;
    cache->targ.values = (jl_array_t*)jl_nothing;
    cache->arg1.indexes = (jl_array_t*)jl_nothing;
    cache->arg1.values = (jl_array_t*)jl_nothing;
    return cache;
}

static jl_typemap_level_t *jl_method_convert_list_to_cache(jl_typemap_entry_t *ml, jl_value_t *key, int8_t offs)
{
    jl_typemap_level_t *cache = jl_new_typemap_level();
    cache->key = key;
    jl_typemap_entry_t *next = NULL;
    JL_GC_PUSH3(&cache, &next, &ml);
    while (ml != (void*)jl_nothing) {
        next = ml->next;
        ml->next = (jl_typemap_entry_t*)jl_nothing;
        jl_typemap_level_insert_(cache, ml, offs, 0);
        ml = next;
    }
    JL_GC_POP();
    return cache;
}

static void jl_typemap_list_insert_(jl_typemap_entry_t **pml, jl_value_t *parent,
                                    jl_typemap_entry_t *newrec, const struct jl_typemap_info *tparams)
{
    if (*pml == (void*)jl_nothing || newrec->isleafsig) {
        newrec->next = *pml;
        jl_gc_wb(newrec, newrec->next);
        *pml = newrec;
        jl_gc_wb(parent, newrec);
    }
    else {
        jl_typemap_list_insert_sorted(pml, parent, newrec, tparams);
    }
}

static void jl_typemap_insert_generic(union jl_typemap_t *pml, jl_value_t *parent,
                                      jl_typemap_entry_t *newrec, jl_value_t *key, int8_t offs,
                                      const struct jl_typemap_info *tparams)
{
    if (jl_typeof(pml->unknown) == (jl_value_t*)jl_typemap_level_type) {
        jl_typemap_level_insert_(pml->node, newrec, offs, tparams);
        return;
    }

    unsigned count = jl_typemap_list_count(pml->leaf);
    if (count > MAX_METHLIST_COUNT) {
        pml->node = jl_method_convert_list_to_cache(pml->leaf, key, offs);
        jl_gc_wb(parent, pml->node);
        jl_typemap_level_insert_(pml->node, newrec, offs, tparams);
        return;
    }

    jl_typemap_list_insert_(&pml->leaf, parent, newrec, tparams);
}

static int jl_typemap_array_insert_(struct jl_ordereddict_t *cache, jl_value_t *key, jl_typemap_entry_t *newrec,
                                    jl_value_t *parent, int8_t tparam, int8_t offs,
                                    const struct jl_typemap_info *tparams)
{
    union jl_typemap_t *pml = mtcache_hash_bp(cache, key, tparam, offs, (jl_value_t*)parent);
    if (pml)
        jl_typemap_insert_generic(pml, (jl_value_t*)cache->values, newrec, key, offs+1, tparams);
    return pml != NULL;
}

static void jl_typemap_level_insert_(jl_typemap_level_t *cache, jl_typemap_entry_t *newrec, int8_t offs,
                                     const struct jl_typemap_info *tparams)
{
    size_t l = jl_field_count(newrec->sig);
    // compute the type at offset `offs` into `sig`, which may be a Vararg
    jl_value_t *t1 = NULL;
    int isva = 0;
    if (l <= offs + 1) {
        t1 = jl_tparam(newrec->sig, l - 1);
        if (jl_is_vararg_type(t1)) {
            isva = 1;
            t1 = jl_tparam0(t1);
        }
        else if (l <= offs) {
            t1 = NULL;
        }
    }
    else if (l > offs) {
        t1 = jl_tparam(newrec->sig, offs);
    }
    // If the type at `offs` is Any, put it in the Any list
    if (t1 && jl_is_any(t1))
        return jl_typemap_insert_generic(&cache->any, (jl_value_t*)cache, newrec, (jl_value_t*)jl_any_type, offs+1, tparams);
    // Don't put Varargs in the optimized caches (too hard to handle in lookup and bp)
    if (t1 && !isva) {
        // if t1 != jl_typetype_type and the argument is Type{...}, this
        // method has specializations for singleton kinds and we use
        // the table indexed for that purpose.
        if (t1 != (jl_value_t*)jl_typetype_type && jl_is_type_type(t1)) {
            jl_value_t *a0 = jl_tparam0(t1);
            if (jl_typemap_array_insert_(&cache->targ, a0, newrec, (jl_value_t*)cache, 1, offs, tparams))
                return;
        }
        if (jl_typemap_array_insert_(&cache->arg1, t1, newrec, (jl_value_t*)cache, 0, offs, tparams))
            return;
    }
    jl_typemap_list_insert_(&cache->linear, (jl_value_t*)cache, newrec, tparams);
}

jl_typemap_entry_t *jl_typemap_insert(union jl_typemap_t *cache, jl_value_t *parent,
                                      jl_tupletype_t *type, jl_svec_t *tvars,
                                      jl_tupletype_t *simpletype, jl_svec_t *guardsigs,
                                      jl_value_t *newvalue, int8_t offs,
                                      const struct jl_typemap_info *tparams,
                                      size_t min_world, size_t max_world,
                                      jl_value_t **overwritten)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(min_world > 0 && max_world > 0);
    assert(jl_is_tuple_type(type));
    if (!simpletype) {
        simpletype = (jl_tupletype_t*)jl_nothing;
    }

    if ((jl_value_t*)simpletype == jl_nothing) {
        jl_typemap_entry_t *ml = jl_typemap_assoc_by_type(*cache, type, NULL, 1, 0, offs, min_world);
        if (ml && ml->simplesig == (void*)jl_nothing) {
            if (newvalue == ml->func.value) // no change. TODO: involve world in computation!
                return ml;
            if (overwritten != NULL)
                *overwritten = ml->func.value;
            if (newvalue == NULL)  // don't overwrite with guard entries
                return ml;
            ml->max_world = min_world - 1;
        }
    }

    jl_typemap_entry_t *newrec =
        (jl_typemap_entry_t*)jl_gc_alloc(ptls, sizeof(jl_typemap_entry_t),
                                         jl_typemap_entry_type);
    newrec->sig = type;
    newrec->simplesig = simpletype;
    newrec->tvars = tvars;
    newrec->func.value = newvalue;
    newrec->guardsigs = guardsigs;
    newrec->next = (jl_typemap_entry_t*)jl_nothing;
    newrec->min_world = min_world;
    newrec->max_world = max_world;
    // compute the complexity of this type signature
    newrec->va = jl_is_va_tuple(type);
    newrec->issimplesig = (tvars == jl_emptysvec); // a TypeVar environment needs an complex matching test
    newrec->isleafsig = newrec->issimplesig && !newrec->va; // entirely leaf types don't need to be sorted
    JL_GC_PUSH1(&newrec);
    size_t i, l;
    for (i = 0, l = jl_field_count(type); i < l && newrec->issimplesig; i++) {
        jl_value_t *decl = jl_field_type(type, i);
        if (decl == (jl_value_t*)jl_datatype_type)
            newrec->isleafsig = 0; // Type{} may have a higher priority than DataType
        else if (decl == (jl_value_t*)jl_typector_type)
            newrec->isleafsig = 0; // Type{} may have a higher priority than TypeConstructor
        else if (jl_is_type_type(decl))
            newrec->isleafsig = 0; // Type{} may need special processing to compute the match
        else if (jl_is_vararg_type(decl))
            newrec->isleafsig = 0; // makes iteration easier when the endpoints are the same
        else if (decl == (jl_value_t*)jl_any_type)
            newrec->isleafsig = 0; // Any needs to go in the general cache
        else if (!jl_is_leaf_type(decl)) // anything else can go through the general subtyping test
            newrec->isleafsig = newrec->issimplesig = 0;
    }
    // TODO: assert that guardsigs == jl_emptysvec && simplesig == jl_nothing if isleafsig and optimize with that knowledge?
    jl_typemap_insert_generic(cache, parent, newrec, NULL, offs, tparams);
    JL_GC_POP();
    return newrec;
}

static int has_unions(jl_tupletype_t *type)
{
    int i;
    for (i = 0; i < jl_nparams(type); i++) {
        jl_value_t *t = jl_tparam(type, i);
        if (jl_is_uniontype(t) ||
            (jl_is_vararg_type(t) && jl_is_uniontype(jl_tparam0(t))))
            return 1;
    }
    return 0;
}

static void jl_typemap_list_insert_sorted(jl_typemap_entry_t **pml, jl_value_t *parent,
                                          jl_typemap_entry_t *newrec,
                                          const struct jl_typemap_info *tparams)
{
    jl_typemap_entry_t *l, **pl;
    pl = pml;
    l = *pml;
    jl_value_t *pa = parent;
    while (l != (void*)jl_nothing) {
        if (!l->isleafsig) { // quickly ignore all of the leafsig entries (these were handled by caller)
            if (jl_args_morespecific((jl_value_t*)newrec->sig, (jl_value_t*)l->sig)) {
                if (l->simplesig == (void*)jl_nothing ||
                    newrec->simplesig != (void*)jl_nothing || !sigs_eq((jl_value_t*)l->sig, (jl_value_t*)newrec->sig, 1)) {
                    // might need to insert multiple entries for a lookup differing only by their simplesig
                    // when simplesig contains a kind
                    // TODO: make this test more correct or figure out a better way to compute this
                    break;
                }
            }
        }
        pl = &l->next;
        pa = (jl_value_t*)l;
        l = l->next;
    }

    JL_SIGATOMIC_BEGIN();
    newrec->next = l;
    jl_gc_wb(newrec, l);
    *pl = newrec;
    jl_gc_wb(pa, newrec);
    // if this contains Union types, methods after it might actually be
    // more specific than it. we need to re-sort them.
    if (has_unions(newrec->sig)) {
        jl_value_t *item_parent = (jl_value_t*)newrec;
        jl_value_t *next_parent = 0;
        jl_typemap_entry_t *item = newrec->next, *next;
        jl_typemap_entry_t **pitem = &newrec->next, **pnext;
        while (item != (void*)jl_nothing) {
            pl = pml;
            l = *pml;
            pa = parent;
            next = item->next;
            pnext = &item->next;
            next_parent = (jl_value_t*)item;
            while (l != newrec->next) {
                if (jl_args_morespecific((jl_value_t*)item->sig,
                                         (jl_value_t*)l->sig)) {
                    // reinsert item earlier in the list
                    *pitem = next;
                    jl_gc_wb(item_parent, next);
                    item->next = l;
                    jl_gc_wb(item, item->next);
                    *pl = item;
                    jl_gc_wb(pa, item);
                    pnext = pitem;
                    next_parent = item_parent;
                    break;
                }
                pl = &l->next;
                pa = (jl_value_t*)l;
                l = l->next;
            }
            item = next;
            pitem = pnext;
            item_parent = next_parent;
        }
    }
    JL_SIGATOMIC_END();
    return;
}

#ifdef __cplusplus
}
#endif
