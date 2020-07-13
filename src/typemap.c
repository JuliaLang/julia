// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <string.h>
#include "julia.h"
#include "julia_internal.h"
#ifndef _OS_WINDOWS_
#include <unistd.h>
#endif
#include "julia_assert.h"

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
             ((jl_tvar_t*)t1)->ub == (jl_value_t*)jl_any_type));
}

// ----- Type Signature Subtype Testing ----- //

static int sig_match_by_type_leaf(jl_value_t **types, jl_tupletype_t *sig, size_t n)
{
    size_t i;
    for (i = 0; i < n; i++) {
        jl_value_t *decl = jl_tparam(sig, i);
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
        jl_value_t *decl = jl_tparam(sig, i);
        jl_value_t *a = types[i];
        jl_value_t *unw = jl_is_unionall(decl) ? ((jl_unionall_t*)decl)->body : decl;
        if (jl_is_type_type(unw)) {
            jl_value_t *tp0 = jl_tparam0(unw);
            if (jl_is_type_type(a)) {
                if (jl_is_typevar(tp0)) {
                    // in the case of Type{_}, the types don't have to match exactly.
                    // this is cached as `Type{T} where T`.
                    if (((jl_tvar_t*)tp0)->ub != (jl_value_t*)jl_any_type &&
                        !jl_subtype(jl_tparam0(a), ((jl_tvar_t*)tp0)->ub))
                        return 0;
                }
                else if (!jl_types_equal(jl_tparam0(a), tp0)) {
                    return 0;
                }
            }
            else if (!jl_is_kind(a) || !jl_is_typevar(tp0) || ((jl_tvar_t*)tp0)->ub != (jl_value_t*)jl_any_type) {
                // manually unroll jl_subtype(a, decl)
                // where `a` can be a subtype and decl is Type{T}
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
        jl_value_t *decl = jl_unwrap_unionall(jl_tparam(sig, i));
        if (jl_vararg_kind(decl) == JL_VARARG_INT) {
            if (n - i != jl_unbox_long(jl_tparam1(decl)))
                return 0;
        }
        jl_value_t *t = jl_tparam0(decl);
        if (jl_is_typevar(t))
            t = ((jl_tvar_t*)t)->ub;
        for (; i < n; i++) {
            if (!jl_subtype(types[i], t))
                return 0;
        }
        return 1;
    }
    return 1;
}

static inline int sig_match_leaf(jl_value_t *arg1, jl_value_t **args, jl_value_t **sig, size_t n)
{
    // NOTE: This function is a huge performance hot spot!!
    size_t i;
    if (jl_typeof(arg1) != sig[0])
        return 0;
    for (i = 1; i < n; i++) {
        jl_value_t *decl = sig[i];
        jl_value_t *a = args[i - 1];
        if (jl_typeof(a) != decl) {
            /*
              we are only matching concrete types here, and those types are
              hash-consed, so pointer comparison should work.
            */
            return 0;
        }
    }
    return 1;
}

static inline int sig_match_simple(jl_value_t *arg1, jl_value_t **args, size_t n, jl_value_t **sig,
                                   int va, size_t lensig)
{
    // NOTE: This function is a performance hot spot!!
    size_t i;
    if (va)
        lensig -= 1;
    for (i = 0; i < lensig; i++) {
        jl_value_t *decl = sig[i];
        jl_value_t *a = (i == 0 ? arg1 : args[i - 1]);
        if (jl_typeof(a) == decl || decl == (jl_value_t*)jl_any_type) {
            /*
              we are only matching concrete types here, and those types are
              hash-consed, so pointer comparison should work.
            */
            continue;
        }
        jl_value_t *unw = jl_is_unionall(decl) ? ((jl_unionall_t*)decl)->body : decl;
        if (jl_is_type_type(unw) && jl_is_type(a)) {
            jl_value_t *tp0 = jl_tparam0(unw);
            if (jl_is_typevar(tp0)) {
                // in the case of Type{_}, the types don't have to match exactly.
                // this is cached as `Type{T} where T`.
                if (((jl_tvar_t*)tp0)->ub != (jl_value_t*)jl_any_type &&
                    !jl_subtype(a, ((jl_tvar_t*)tp0)->ub))
                    return 0;
            }
            else {
                if (a != tp0) {
                    jl_datatype_t *da = (jl_datatype_t*)a;
                    jl_datatype_t *dt = (jl_datatype_t*)tp0;
                    while (jl_is_unionall(da))
                        da = (jl_datatype_t*)((jl_unionall_t*)da)->body;
                    while (jl_is_unionall(dt))
                        dt = (jl_datatype_t*)((jl_unionall_t*)dt)->body;
                    if (jl_is_datatype(da) && jl_is_datatype(dt) && da->name != dt->name)
                        return 0;
                    if (!jl_types_equal(a, tp0))
                        return 0;
                }
            }
        }
        else {
            return 0;
        }
    }
    if (va) {
        jl_value_t *decl = sig[i];
        if (jl_vararg_kind(decl) == JL_VARARG_INT) {
            if (n - i != jl_unbox_long(jl_tparam1(decl)))
                return 0;
        }
        jl_value_t *t = jl_unwrap_vararg(decl);
        for (; i < n; i++) {
            jl_value_t *a = (i == 0 ? arg1 : args[i - 1]);
            if (!jl_isa(a, t))
                return 0;
        }
        return 1;
    }
    return 1;
}


// ----- MethodCache helper functions ----- //

// predicate to fast-test if this type is a leaf type that can exist in the cache
// and does not need a more expensive linear scan to find all intersections
// be careful not to put non-leaf types or DataType/UnionAll in the cache here,
// since they should have a lower priority and need to go into the sorted list
int is_cache_leaf(jl_value_t *ty)
{
    return (jl_is_concrete_type(ty) && !jl_is_kind(ty));
}

static jl_typemap_t **mtcache_hash_lookup_bp(jl_array_t *cache JL_PROPAGATES_ROOT, jl_value_t *ty) JL_NOTSAFEPOINT
{
    if (cache == (jl_array_t*)jl_an_empty_vec_any)
        return NULL;
    jl_typemap_t **pml = jl_table_peek_bp(cache, ty);
    JL_GC_PROMISE_ROOTED(pml); // clang-sa doesn't trust our JL_PROPAGATES_ROOT claim
    return pml;
}

static void mtcache_hash_insert(jl_array_t **cache, jl_value_t *parent, jl_value_t *key, jl_typemap_t *val)
{
    int inserted = 0;
    jl_array_t *a = *cache;
    if (a == (jl_array_t*)jl_an_empty_vec_any) {
        a = jl_alloc_vec_any(16);
        *cache = a;
        jl_gc_wb(parent, a);
    }
    a = jl_eqtable_put(a, key, val, &inserted);
    assert(inserted);
    if (a != *cache) {
        *cache = a;
        jl_gc_wb(parent, a);
    }
}

static jl_typemap_t *mtcache_hash_lookup(jl_array_t *cache JL_PROPAGATES_ROOT, jl_value_t *ty) JL_NOTSAFEPOINT
{
    if (cache == (jl_array_t*)jl_an_empty_vec_any)
        return NULL;
    jl_typemap_t *ml = (jl_typemap_t*)jl_eqtable_get(cache, ty, jl_nothing);
    JL_GC_PROMISE_ROOTED(ml); // clang-sa doesn't trust our JL_PROPAGATES_ROOT claim
    return ml;
}

// ----- Sorted Type Signature Lookup Matching ----- //

static int jl_typemap_array_visitor(jl_array_t *a, jl_typemap_visitor_fptr fptr, void *closure)
{
    size_t i, l = jl_array_len(a);
    jl_typemap_t **data = (jl_typemap_t **)jl_array_data(a);
    for (i = 1; i < l; i += 2) {
        jl_value_t *d = data[i];
        JL_GC_PROMISE_ROOTED(d);
        if (d && !jl_typemap_visitor(d, fptr, closure))
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

int jl_typemap_visitor(jl_typemap_t *cache, jl_typemap_visitor_fptr fptr, void *closure)
{
    if (jl_typeof(cache) == (jl_value_t*)jl_typemap_level_type) {
        jl_typemap_level_t *node = (jl_typemap_level_t*)cache;
        if (node->targ != (jl_array_t*)jl_an_empty_vec_any)
            if (!jl_typemap_array_visitor(node->targ, fptr, closure))
                return 0;
        if (node->arg1 != (jl_array_t*)jl_an_empty_vec_any)
            if (!jl_typemap_array_visitor(node->arg1, fptr, closure))
                return 0;
        if (!jl_typemap_node_visitor(node->linear, fptr, closure))
            return 0;
        return jl_typemap_visitor(node->any, fptr, closure);
    }
    else {
        return jl_typemap_node_visitor((jl_typemap_entry_t*)cache, fptr, closure);
    }
}

static int jl_typemap_intersection_array_visitor(jl_array_t *a, jl_value_t *ty, int tparam,
                                                 int offs, struct typemap_intersection_env *closure)
{
    size_t i, l = jl_array_len(a);
    jl_typemap_t **data = (jl_typemap_t **)jl_array_data(a);
    for (i = 0; i < l; i += 2) {
        jl_value_t *t = data[i];
        JL_GC_PROMISE_ROOTED(t);
        if (t == jl_nothing || t == NULL)
            continue;
        // `t` is a leaftype, so intersection test becomes subtype
        if (ty == (jl_value_t*)jl_any_type || // easy case: Any always matches
            (tparam
             ? (jl_typeof(t) == ty || jl_isa(t, ty)) // (Type{t} <: ty), where is_leaf_type(t) => isa(t, ty)
             : (t == ty || jl_subtype(t, ty)))) {
            jl_typemap_t *ml = data[i + 1];
            JL_GC_PROMISE_ROOTED(ml);
            if (!jl_typemap_intersection_visitor(ml, offs + 1, closure))
                return 0;
        }
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
            if (closure->env)
                closure->env = jl_outer_unionall_vars((jl_value_t*)ml->sig);
            closure->ti = closure->type;
            closure->issubty = 1;
            if (!fptr(ml, closure))
                return 0;
        }
        else {
            jl_svec_t **penv = NULL;
            if (closure->env) {
                closure->env = jl_emptysvec;
                penv = &closure->env;
            }
            closure->ti = jl_type_intersection_env_s(closure->type, (jl_value_t*)ml->sig, penv, &closure->issubty);
            if (closure->ti != (jl_value_t*)jl_bottom_type) {
                // In some corner cases type intersection is conservative and returns something
                // for intersect(A, B) even though A is a dispatch tuple and !(A <: B).
                // For dispatch purposes in such a case we know there's no match. This check
                // fixes issue #30394.
                if (closure->issubty || !jl_is_dispatch_tupletype(closure->type))
                    if (!fptr(ml, closure))
                        return 0;
            }
        }
        ml = ml->next;
    }
    return 1;
}

int jl_typemap_intersection_visitor(jl_typemap_t *map, int offs,
                                    struct typemap_intersection_env *closure)
{
    jl_value_t *ttypes = jl_unwrap_unionall(closure->type);
    assert(jl_is_datatype(ttypes));
    //TODO: fast-path for leaf-type tuples?
    //if (ttypes->isdispatchtuple) {
    //    register jl_typemap_intersection_visitor_fptr fptr = closure->fptr;
    //        struct jl_typemap_assoc search = {(jl_value_t*)closure->type, world, closure->env, 0, ~(size_t)0};
    //        jl_typemap_entry_t *ml = jl_typemap_assoc_by_type(map, search, offs, /*subtype*/1);
    //        if (ml) {
    //            closure->env = search->env;
    //            if (!fptr(ml, closure))
    //                return 0;
    //        }
    //    }
    //    return 1;
    //}
    if (jl_typeof(map) == (jl_value_t *)jl_typemap_level_type) {
        jl_typemap_level_t *cache = (jl_typemap_level_t*)map;
        jl_value_t *ty = NULL;
        size_t l = jl_nparams(ttypes);
        if (closure->va && l <= offs + 1) {
            ty = closure->va;
        }
        else if (l > offs) {
            ty = jl_tparam(ttypes, offs);
        }
        if (ty) {
            while (jl_is_typevar(ty))
                ty = ((jl_tvar_t*)ty)->ub;
            // approxify the tparam until we have a valid type
            if (jl_has_free_typevars(ty)) {
                ty = jl_unwrap_unionall(ty);
                if (jl_is_datatype(ty))
                    ty = ((jl_datatype_t*)ty)->name->wrapper;
                else
                    ty = (jl_value_t*)jl_any_type;
            }
            if (cache->targ != (jl_array_t*)jl_an_empty_vec_any) {
                jl_value_t *typetype = jl_is_type_type(ty) ? jl_tparam0(ty) : NULL;
                if (typetype) {
                    if (is_cache_leaf(typetype)) {
                        // direct lookup of leaf types
                        jl_typemap_t *ml = mtcache_hash_lookup(cache->targ, typetype);
                        if (ml != jl_nothing) {
                            if (!jl_typemap_intersection_visitor(ml, offs+1, closure)) return 0;
                        }
                    }
                }
                else {
                    // else an array scan is required to check subtypes
                    // first, fast-path: optimized pre-intersection test to see if `ty` could intersect with any Type
                    if (typetype || !jl_has_empty_intersection((jl_value_t*)jl_type_type, ty))
                        if (!jl_typemap_intersection_array_visitor(cache->targ, ty, 1, offs, closure)) return 0;
                }
            }
            if (cache->arg1 != (jl_array_t*)jl_an_empty_vec_any) {
                if (is_cache_leaf(ty)) {
                    // direct lookup of leaf types
                    jl_typemap_t *ml = mtcache_hash_lookup(cache->arg1, ty);
                    if (ml != jl_nothing) {
                        if (!jl_typemap_intersection_visitor(ml, offs+1, closure)) return 0;
                    }
                }
                else {
                    // else an array scan is required to check subtypes
                    if (!jl_typemap_intersection_array_visitor(cache->arg1, ty, 0, offs, closure)) return 0;
                }
            }
        }
        if (!jl_typemap_intersection_node_visitor(cache->linear, closure))
            return 0;
        return jl_typemap_intersection_visitor(cache->any, offs+1, closure);
    }
    else {
        return jl_typemap_intersection_node_visitor(
            (jl_typemap_entry_t*)map, closure);
    }
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
static jl_typemap_entry_t *jl_typemap_entry_assoc_by_type(
        jl_typemap_entry_t *ml,
        struct jl_typemap_assoc *search)
{
    jl_value_t *types = search->types;
    JL_GC_PROMISE_ROOTED(types);
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)types);
    int isua = jl_is_unionall(types);
    size_t n = jl_nparams(unw);
    int typesisva = n == 0 ? 0 : jl_is_vararg_type(jl_tparam(unw, n-1));
    for (; ml != (void*)jl_nothing; ml = ml->next) {
        size_t lensig = jl_nparams(jl_unwrap_unionall((jl_value_t*)ml->sig));
        if (lensig == n || (ml->va && lensig <= n+1)) {
            int resetenv = 0, ismatch = 1;
            if (ml->simplesig != (void*)jl_nothing && !isua) {
                size_t lensimplesig = jl_nparams(ml->simplesig);
                int isva = lensimplesig > 0 && jl_is_vararg_type(jl_tparam(ml->simplesig, lensimplesig - 1));
                if (lensig == n || (isva && lensimplesig <= n + 1))
                    ismatch = sig_match_by_type_simple(jl_svec_data(((jl_datatype_t*)types)->parameters), n,
                                                       ml->simplesig, lensimplesig, isva);
                else
                    ismatch = 0;
            }

            if (ismatch == 0)
                ; // nothing
            else if (ml->isleafsig && !typesisva && !isua)
                ismatch = sig_match_by_type_leaf(jl_svec_data(((jl_datatype_t*)types)->parameters),
                                                 ml->sig, lensig);
            else if (ml->issimplesig && !typesisva && !isua)
                ismatch = sig_match_by_type_simple(jl_svec_data(((jl_datatype_t*)types)->parameters), n,
                                                   ml->sig, lensig, ml->va);
            else {
                ismatch = jl_subtype_matching(types, (jl_value_t*)ml->sig, search->env ? &search->env : NULL);
                if (ismatch && search->env)
                    resetenv = 1;
            }

            if (ismatch) {
                size_t i, l;
                for (i = 0, l = jl_svec_len(ml->guardsigs); i < l; i++) {
                    // see corresponding code in jl_typemap_entry_assoc_exact
                    if (jl_subtype(types, jl_svecref(ml->guardsigs, i))) {
                        ismatch = 0;
                        break;
                    }
                }
                if (ismatch) {
                    if (search->world < ml->min_world) {
                        // ignore method table entries that are part of a later world
                        if (search->max_valid >= ml->min_world)
                            search->max_valid = ml->min_world - 1;
                    }
                    else if (search->world > ml->max_world) {
                        // ignore method table entries that have been replaced in the current world
                        if (search->min_valid <= ml->max_world)
                            search->min_valid = ml->max_world + 1;
                    }
                    else {
                        // intersect the env valid range with method's valid range
                        if (search->min_valid < ml->min_world)
                            search->min_valid = ml->min_world;
                        if (search->max_valid > ml->max_world)
                            search->max_valid = ml->max_world;
                        return ml;
                    }
                }
            }
            if (resetenv)
                search->env = jl_emptysvec;
        }
    }
    return NULL;
}

int jl_obviously_unequal(jl_value_t *a, jl_value_t *b);

static jl_typemap_entry_t *jl_typemap_entry_lookup_by_type(
        jl_typemap_entry_t *ml, struct jl_typemap_assoc *search)
{
    for (; ml != (void*)jl_nothing; ml = ml->next) {
        if (search->world < ml->min_world || search->world > ml->max_world)
            continue;
        // unroll the first few cases here, to the extent that is possible to do fast and easily
        jl_value_t *types = search->types;
        JL_GC_PROMISE_ROOTED(types);
        jl_value_t *a = jl_unwrap_unionall(types);
        jl_value_t *b = jl_unwrap_unionall((jl_value_t*)ml->sig);
        size_t na = jl_nparams(a);
        size_t nb = jl_nparams(b);
        int va_a = na > 0 && jl_is_vararg_type(jl_tparam(a, na - 1));
        int va_b = nb > 0 && jl_is_vararg_type(jl_tparam(b, nb - 1));
        if (!va_a && !va_b) {
            if (na != nb)
                continue;
        }
        if (na - va_a > 0 && nb - va_b > 0) {
            if (jl_obviously_unequal(jl_tparam(a, 0), jl_tparam(b, 0)))
                continue;
            if (na - va_a > 1 && nb - va_b > 1) {
                if (jl_obviously_unequal(jl_tparam(a, 1), jl_tparam(b, 1)))
                    continue;
                if (na - va_a > 2 && nb - va_b > 2) {
                    if (jl_obviously_unequal(jl_tparam(a, 2), jl_tparam(b, 2)))
                        continue;
                }
            }
        }
        if (jl_types_equal(types, (jl_value_t*)ml->sig))
            return ml;
    }
    return NULL;
}


// this is the general entry point for looking up a type in the cache
// as a subtype, or with type_equal
jl_typemap_entry_t *jl_typemap_assoc_by_type(
        jl_typemap_t *ml_or_cache,
        struct jl_typemap_assoc *search,
        int8_t offs, uint8_t subtype)
{
    if (jl_typeof(ml_or_cache) == (jl_value_t *)jl_typemap_level_type) {
        jl_typemap_level_t *cache = (jl_typemap_level_t*)ml_or_cache;
        // called object is the primary key for constructors, otherwise first argument
        jl_value_t *ty = NULL;
        jl_value_t *ttypes = jl_unwrap_unionall((jl_value_t*)search->types);
        JL_GC_PROMISE_ROOTED(ttypes);
        assert(jl_is_datatype(ttypes));
        size_t l = jl_nparams(ttypes);
        int isva = 0;
        // compute the type at offset `offs` into `types`, which may be a Vararg
        if (l <= offs + 1) {
            ty = jl_tparam(ttypes, l - 1);
            if (jl_is_vararg_type(ty)) {
                ty = jl_unwrap_vararg(ty);
                isva = 1;
            }
            else if (l <= offs) {
                ty = NULL;
            }
        }
        else if (l > offs) {
            ty = jl_tparam(ttypes, offs);
        }
        // If there is a type at offs, look in the optimized caches
        if (!subtype) {
            if (ty && jl_is_any(ty))
                return jl_typemap_assoc_by_type(cache->any, search, offs + 1, subtype);
            if (isva) // in lookup mode, want to match Vararg exactly, not as a subtype
                ty = NULL;
        }
        if (ty) {
            if (jl_is_type_type(ty)) {
                jl_value_t *a0 = jl_tparam0(ty);
                if (is_cache_leaf(a0)) {
                    if (cache->targ != (jl_array_t*)jl_an_empty_vec_any) {
                        jl_typemap_t *ml = mtcache_hash_lookup(cache->targ, a0);
                        if (ml != jl_nothing) {
                            jl_typemap_entry_t *li = jl_typemap_assoc_by_type(ml, search, offs + 1, subtype);
                            if (li) return li;
                        }
                    }
                    if (!subtype) return NULL;
                }
            }
            if (is_cache_leaf(ty)) {
                if (cache->arg1 != (jl_array_t*)jl_an_empty_vec_any) {
                    jl_typemap_t *ml = mtcache_hash_lookup(cache->arg1, ty);
                    if (ml != jl_nothing) {
                        jl_typemap_entry_t *li = jl_typemap_assoc_by_type(ml, search, offs + 1, subtype);
                        if (li) return li;
                    }
                }
                if (!subtype) return NULL;
            }
        }
        // Always check the list (since offs doesn't always start at 0)
        if (subtype) {
            jl_typemap_entry_t *li = jl_typemap_entry_assoc_by_type(cache->linear, search);
            if (li) return li;
            return jl_typemap_assoc_by_type(cache->any, search, offs + 1, subtype);
        }
        else {
            return jl_typemap_entry_lookup_by_type(cache->linear, search);
        }
    }
    else {
        jl_typemap_entry_t *leaf = (jl_typemap_entry_t*)ml_or_cache;
        return subtype ?
            jl_typemap_entry_assoc_by_type(leaf, search) :
            jl_typemap_entry_lookup_by_type(leaf, search);
    }
}

jl_typemap_entry_t *jl_typemap_entry_assoc_exact(jl_typemap_entry_t *ml, jl_value_t *arg1, jl_value_t **args, size_t n, size_t world)
{
    // some manually-unrolled common special cases
    while (ml->simplesig == (void*)jl_nothing && ml->guardsigs == jl_emptysvec && ml->isleafsig) {
        // use a tight loop for as long as possible
        if (world >= ml->min_world && world <= ml->max_world) {
            if (n == jl_nparams(ml->sig) && jl_typeof(arg1) == jl_tparam(ml->sig, 0)) {
                if (n == 1)
                    return ml;
                if (n == 2) {
                    if (jl_typeof(args[0]) == jl_tparam(ml->sig, 1))
                        return ml;
                }
                else if (n == 3) {
                    if (jl_typeof(args[0]) == jl_tparam(ml->sig, 1) &&
                        jl_typeof(args[1]) == jl_tparam(ml->sig, 2))
                        return ml;
                }
                else {
                    if (sig_match_leaf(arg1, args, jl_svec_data(ml->sig->parameters), n))
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
        size_t lensig = jl_nparams(ml->sig);
        if (lensig == n || (ml->va && lensig <= n+1)) {
            if (ml->simplesig != (void*)jl_nothing) {
                size_t lensimplesig = jl_nparams(ml->simplesig);
                int isva = lensimplesig > 0 && jl_is_vararg_type(jl_tparam(ml->simplesig, lensimplesig - 1));
                if (lensig == n || (isva && lensimplesig <= n + 1)) {
                    if (!sig_match_simple(arg1, args, n, jl_svec_data(ml->simplesig->parameters), isva, lensimplesig))
                        continue;
                }
                else {
                    continue;
                }
            }

            if (ml->isleafsig) {
                if (!sig_match_leaf(arg1, args, jl_svec_data(ml->sig->parameters), n))
                    continue;
            }
            else if (ml->issimplesig) {
                if (!sig_match_simple(arg1, args, n, jl_svec_data(ml->sig->parameters), ml->va, lensig))
                    continue;
            }
            else {
                if (!jl_tuple1_isa(arg1, args, n, ml->sig))
                    continue;
            }

            size_t i, l;
            if (ml->guardsigs != jl_emptysvec) {
                for (i = 0, l = jl_svec_len(ml->guardsigs); i < l; i++) {
                    // checking guard entries require a more
                    // expensive subtype check, since guard entries added for @nospecialize might be
                    // abstract. this fixed issue #12967.
                    if (jl_tuple1_isa(arg1, args, n, (jl_tupletype_t*)jl_svecref(ml->guardsigs, i))) {
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

jl_typemap_entry_t *jl_typemap_level_assoc_exact(jl_typemap_level_t *cache, jl_value_t *arg1, jl_value_t **args, size_t n, int8_t offs, size_t world)
{
    if (n > offs) {
        jl_value_t *a1 = (offs == 0 ? arg1 : args[offs - 1]);
        jl_value_t *ty = jl_typeof(a1);
        assert(jl_is_datatype(ty));
        if (ty == (jl_value_t*)jl_datatype_type && cache->targ != (jl_array_t*)jl_an_empty_vec_any && is_cache_leaf(a1)) {
            jl_typemap_t *ml_or_cache = mtcache_hash_lookup(cache->targ, a1);
            jl_typemap_entry_t *ml = jl_typemap_assoc_exact(ml_or_cache, arg1, args, n, offs+1, world);
            if (ml) return ml;
        }
        if (cache->arg1 != (jl_array_t*)jl_an_empty_vec_any && is_cache_leaf(ty)) {
            jl_typemap_t *ml_or_cache = mtcache_hash_lookup(cache->arg1, ty);
            jl_typemap_entry_t *ml = jl_typemap_assoc_exact(ml_or_cache, arg1, args, n, offs+1, world);
            if (ml) return ml;
        }
    }
    if (cache->linear != (jl_typemap_entry_t*)jl_nothing) {
        jl_typemap_entry_t *ml = jl_typemap_entry_assoc_exact(cache->linear, arg1, args, n, world);
        if (ml) return ml;
    }
    if (cache->any != jl_nothing)
        return jl_typemap_assoc_exact(cache->any, arg1, args, n, offs+1, world);
    return NULL;
}


// ----- Method List Insertion Management ----- //

static unsigned jl_typemap_list_count(jl_typemap_entry_t *ml) JL_NOTSAFEPOINT
{
    unsigned count = 0;
    while (ml != (void*)jl_nothing) {
        count++;
        ml = ml->next;
    }
    return count;
}

static void jl_typemap_level_insert_(jl_typemap_t *map, jl_typemap_level_t *cache, jl_typemap_entry_t *newrec, int8_t offs, const struct jl_typemap_info *tparams);
static void jl_typemap_list_insert_sorted(
        jl_typemap_t *map, jl_typemap_entry_t **pml, jl_value_t *parent,
        jl_typemap_entry_t *newrec, const struct jl_typemap_info *tparams);

static jl_typemap_level_t *jl_new_typemap_level(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_typemap_level_t *cache =
        (jl_typemap_level_t*)jl_gc_alloc(ptls, sizeof(jl_typemap_level_t),
                                         jl_typemap_level_type);
    cache->linear = (jl_typemap_entry_t*)jl_nothing;
    cache->any = jl_nothing;
    cache->targ = (jl_array_t*)jl_an_empty_vec_any;
    cache->arg1 = (jl_array_t*)jl_an_empty_vec_any;
    return cache;
}

static jl_typemap_level_t *jl_method_convert_list_to_cache(
        jl_typemap_t *map, jl_typemap_entry_t *ml, int8_t offs,
        const struct jl_typemap_info *tparams)
{
    jl_typemap_level_t *cache = jl_new_typemap_level();
    jl_typemap_entry_t *next = NULL;
    JL_GC_PUSH3(&cache, &next, &ml);
    while (ml != (void*)jl_nothing) {
        next = ml->next;
        ml->next = (jl_typemap_entry_t*)jl_nothing;
        // TODO: for Methods this can locally mess up the `.resorted` list
        // --> how to recover that?
        jl_typemap_level_insert_(map, cache, ml, offs, tparams);
        ml = next;
    }
    JL_GC_POP();
    return cache;
}

static void jl_typemap_list_insert_(
        jl_typemap_t *map, jl_typemap_entry_t **pml, jl_value_t *parent,
        jl_typemap_entry_t *newrec, const struct jl_typemap_info *tparams)
{
    if (*pml == (void*)jl_nothing || newrec->isleafsig || (tparams && tparams->unsorted)) {
        newrec->next = *pml;
        jl_gc_wb(newrec, newrec->next);
        *pml = newrec;
        jl_gc_wb(parent, newrec);
    }
    else {
        jl_typemap_list_insert_sorted(map, pml, parent, newrec, tparams);
    }
}

static void jl_typemap_insert_generic(
        jl_typemap_t *map, jl_typemap_t **pml, jl_value_t *parent,
        jl_typemap_entry_t *newrec, int8_t offs,
        const struct jl_typemap_info *tparams)
{
    if (jl_typeof(*pml) == (jl_value_t*)jl_typemap_level_type) {
        jl_typemap_level_insert_(map, (jl_typemap_level_t*)*pml, newrec, offs, tparams);
        return;
    }

    unsigned count = jl_typemap_list_count((jl_typemap_entry_t*)*pml);
    if (count > MAX_METHLIST_COUNT) {
        *pml = (jl_typemap_t*)jl_method_convert_list_to_cache(
            map, (jl_typemap_entry_t *)*pml,
            offs, tparams);
        jl_gc_wb(parent, *pml);
        jl_typemap_level_insert_(map, (jl_typemap_level_t*)*pml, newrec, offs, tparams);
        return;
    }

    jl_typemap_list_insert_(map, (jl_typemap_entry_t **)pml,
        parent, newrec, tparams);
}

static int jl_typemap_array_insert_(
        jl_typemap_t *map, jl_array_t **cache, jl_value_t *key, jl_typemap_entry_t *newrec,
        jl_value_t *parent, int8_t tparam, int8_t offs,
        const struct jl_typemap_info *tparams)
{
    if (!is_cache_leaf(key))
        return 0;
    jl_typemap_t **pml = mtcache_hash_lookup_bp(*cache, key);
    if (pml != NULL)
        jl_typemap_insert_generic(map, pml, (jl_value_t*)*cache, newrec, offs+1, tparams);
    else
        mtcache_hash_insert(cache, parent, key, (jl_typemap_t*)newrec);
    return 1;
}

static void jl_typemap_level_insert_(
        jl_typemap_t *map, jl_typemap_level_t *cache, jl_typemap_entry_t *newrec, int8_t offs,
        const struct jl_typemap_info *tparams)
{
    jl_value_t *ttypes = jl_unwrap_unionall((jl_value_t*)newrec->sig);
    size_t l = jl_nparams(ttypes);
    // compute the type at offset `offs` into `sig`, which may be a Vararg
    jl_value_t *t1 = NULL;
    int isva = 0;
    if (l <= offs + 1) {
        t1 = jl_tparam(ttypes, l - 1);
        if (jl_is_vararg_type(t1)) {
            isva = 1;
            t1 = jl_unwrap_vararg(t1);
        }
        else if (l <= offs) {
            t1 = NULL;
        }
    }
    else if (l > offs) {
        t1 = jl_tparam(ttypes, offs);
    }
    // If the type at `offs` is Any, put it in the Any list
    if (t1 && jl_is_any(t1)) {
        jl_typemap_insert_generic(map, &cache->any, (jl_value_t*)cache, newrec, offs+1, tparams);
        return;
    }
    // Don't put Varargs in the optimized caches (too hard to handle in lookup and bp)
    if (t1 && !isva) {
        // if t1 != jl_typetype_type and the argument is Type{...}, this
        // method has specializations for singleton kinds and we use
        // the table indexed for that purpose.
        if (t1 != (jl_value_t*)jl_typetype_type && jl_is_type_type(t1)) {
            jl_value_t *a0 = jl_tparam0(t1);
            if (jl_typemap_array_insert_(map, &cache->targ, a0, newrec, (jl_value_t*)cache, 1, offs, tparams))
                return;
        }
        if (jl_typemap_array_insert_(map, &cache->arg1, t1, newrec, (jl_value_t*)cache, 0, offs, tparams))
            return;
    }
    jl_typemap_list_insert_(map, &cache->linear, (jl_value_t*)cache, newrec, tparams);
}

jl_typemap_entry_t *jl_typemap_alloc(
        jl_tupletype_t *type, jl_tupletype_t *simpletype, jl_svec_t *guardsigs,
        jl_value_t *newvalue, size_t min_world, size_t max_world)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(min_world > 0 && max_world > 0);
    if (!simpletype)
        simpletype = (jl_tupletype_t*)jl_nothing;
    jl_value_t *ttype = jl_unwrap_unionall((jl_value_t*)type);
    assert(jl_is_tuple_type(ttype));
    // compute the complexity of this type signature
    int isva = jl_is_va_tuple((jl_datatype_t*)ttype);
    int issimplesig = !jl_is_unionall(type); // a TypeVar environment needs a complex matching test
    int isleafsig = issimplesig && !isva; // entirely leaf types don't need to be sorted
    size_t i, l;
    for (i = 0, l = jl_nparams(ttype); i < l && issimplesig; i++) {
        jl_value_t *decl = jl_tparam(ttype, i);
        if (jl_is_kind(decl))
            isleafsig = 0; // Type{} may have a higher priority than a kind
        else if (jl_is_type_type(decl))
            isleafsig = 0; // Type{} may need special processing to compute the match
        else if (jl_is_vararg_type(decl))
            isleafsig = 0; // makes iteration easier when the endpoints are the same
        else if (decl == (jl_value_t*)jl_any_type)
            isleafsig = 0; // Any needs to go in the general cache
        else if (!jl_is_concrete_type(decl)) // anything else needs to go through the general subtyping test
            isleafsig = issimplesig = 0;
    }

    jl_typemap_entry_t *newrec =
        (jl_typemap_entry_t*)jl_gc_alloc(ptls, sizeof(jl_typemap_entry_t),
                                         jl_typemap_entry_type);
    newrec->sig = type;
    newrec->simplesig = simpletype;
    newrec->func.value = newvalue;
    newrec->guardsigs = guardsigs;
    newrec->next = (jl_typemap_entry_t*)jl_nothing;
    newrec->min_world = min_world;
    newrec->max_world = max_world;
    newrec->va = isva;
    newrec->issimplesig = issimplesig;
    newrec->isleafsig = isleafsig;
    return newrec;
}

void jl_typemap_insert(jl_typemap_t **cache, jl_value_t *parent,
        jl_typemap_entry_t *newrec, int8_t offs,
        const struct jl_typemap_info *tparams)
{
    jl_typemap_insert_generic(*cache, cache, parent, newrec, offs, tparams);
}

static void jl_typemap_list_insert_sorted(
        jl_typemap_t *map, jl_typemap_entry_t **pml, jl_value_t *parent,
        jl_typemap_entry_t *newrec, const struct jl_typemap_info *tparams)
{
    jl_typemap_entry_t *l, **pl;
    pl = pml;
    l = *pml;
    jl_value_t *pa = parent;
    while ((jl_value_t*)l != jl_nothing) {
        if (!l->isleafsig) { // quickly ignore all of the leafsig entries (these were handled by caller)
            if (jl_type_morespecific((jl_value_t*)newrec->sig, (jl_value_t*)l->sig)) {
                if (l->simplesig == (void*)jl_nothing ||
                    newrec->simplesig != (void*)jl_nothing ||
                    !jl_types_equal((jl_value_t*)l->sig, (jl_value_t*)newrec->sig)) {
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

    // insert newrec at the first point it is more specific than the following method
    newrec->next = l;
    jl_gc_wb(newrec, l);
    *pl = newrec;
    jl_gc_wb(pa, newrec);
}

#ifdef __cplusplus
}
#endif
