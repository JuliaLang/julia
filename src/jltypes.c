// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  Types
  . type union, type cache, and instantiation
  . builtin type definitions
*/
#include <stdlib.h>
#include <string.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

_Atomic(jl_value_t*) cmpswap_names JL_GLOBALLY_ROOTED;
jl_datatype_t *ijl_small_typeof[(jl_max_tags << 4) / sizeof(*ijl_small_typeof)]; // 16-bit aligned, like the GC

// Global data structures for accessing symbols and other globals
#if !defined(__clang_analyzer__)
struct jl_sysimg_global sysimg_global;
struct jl_const_globals const_globals;
struct jl_internal_global internal_global;
#endif

// compute empirical max-probe for a given size
#define max_probe(size) ((size) <= 1024 ? 16 : (size) >> 6)
#define h2index(hv, sz) (size_t)((hv) & ((sz)-1))

// --- type properties and predicates ---

static int typeenv_has(jl_typeenv_t *env, jl_tvar_t *v) JL_NOTSAFEPOINT
{
    while (env != NULL) {
        if (env->var == v)
            return 1;
        env = env->prev;
    }
    return 0;
}

static int typeenv_has_ne(jl_typeenv_t *env, jl_tvar_t *v) JL_NOTSAFEPOINT
{
    while (env != NULL) {
        if (env->var == v)
            return env->val != (jl_value_t*)v; // consider it actually not present if it is bound to itself unchanging
        env = env->prev;
    }
    return 0;
}


static int layout_uses_free_typevars(jl_value_t *v, jl_typeenv_t *env) JL_CANSAFEPOINT
{
    while (1) {
        if (jl_is_typevar(v))
            return !typeenv_has(env, (jl_tvar_t*)v);
        if (jl_is_typeapp(v))
            return 1;
        while (jl_is_unionall(v)) {
            jl_unionall_t *ua = (jl_unionall_t*)v;
            jl_typeenv_t *newenv = (jl_typeenv_t*)alloca(sizeof(jl_typeenv_t));
            newenv->var = ua->var;
            newenv->val = NULL;
            newenv->prev = env;
            env = newenv;
            v = ua->body;
        }
        if (jl_is_datatype(v)) {
            jl_datatype_t *dt = (jl_datatype_t*)v;
            if (dt->isconcretetype)
                return 0;
            if (dt->layout || !dt->name->mayinlinealloc)
                return 0;
            if (dt->name == jl_namedtuple_typename) {
                jl_value_t *names = jl_tparam0(dt);
                jl_value_t *types = jl_tparam1(dt);
                return layout_uses_free_typevars(names, env) || layout_uses_free_typevars(types, env);
            }
            if (dt->name == jl_tuple_typename)
                // conservative, since we don't want to inline an abstract tuple,
                // and we currently declare !has_fixed_layout for these, but that
                // means we also won't be able to inline a tuple which is concrete
                // except for the use of free type-vars
                return 1;
            jl_svec_t *types = jl_get_fieldtypes(dt);
            size_t i, l = jl_svec_len(types);
            for (i = 0; i < l; i++) {
                jl_value_t *ft = jl_svecref(types, i);
                if (layout_uses_free_typevars(ft, env))
                    // This might be inline-alloc, but we don't know the layout
                    return 1;
            }
            return 0;
        }
        else if (jl_is_uniontype(v) || jl_is_intersecttype(v)) {
            if (layout_uses_free_typevars(((jl_uniontype_t*)v)->a, env))
                return 1;
           v = ((jl_uniontype_t*)v)->b;
        }
        else if (jl_is_some_Type(v)) {
            v = jl_some_Type_T(v);
        }
        else if (jl_is_vararg(v)) {
            jl_vararg_t *vm = (jl_vararg_t*)v;
            if (!vm->T)
                return 0;
            if (vm->N) {
                if (layout_uses_free_typevars(vm->N, env))
                    return 1;
            }
            v = vm->T;
        }
        else {
            return 0;
        }
    }
}

static int has_free_typevars(jl_value_t *v, jl_typeenv_t *env) JL_NOTSAFEPOINT
{
    while (1) {
        if (jl_is_datatype(v) && (env == NULL || !((jl_datatype_t*)v)->hasfreetypevars))
            return ((jl_datatype_t*)v)->hasfreetypevars;
        if (jl_is_typevar(v)) {
            return !typeenv_has(env, (jl_tvar_t*)v);
        }
        if (jl_is_typeapp(v))
            return 1;
        while (jl_is_unionall(v)) {
            jl_unionall_t *ua = (jl_unionall_t*)v;
            if (ua->var->lb != jl_bottom_type && has_free_typevars(ua->var->lb, env))
                return 1;
            if (ua->var->ub != (jl_value_t*)jl_any_type && has_free_typevars(ua->var->ub, env))
                return 1;
            jl_typeenv_t *newenv = (jl_typeenv_t*)alloca(sizeof(jl_typeenv_t));
            newenv->var = ua->var;
            newenv->val = NULL;
            newenv->prev = env;
            env = newenv;
            v = ua->body;
        }
        // After unwrapping UnionAll, body might be TypeApp or TypeVar;
        // restart the loop so those checks at the top fire.
        if (jl_is_typeapp(v) || jl_is_typevar(v))
            continue;
        if (jl_is_datatype(v)) {
            int expect = ((jl_datatype_t*)v)->hasfreetypevars;
            if (expect == 0 || env == NULL)
                return expect;
            size_t i;
            for (i = 0; i < jl_nparams(v); i++) {
                if (has_free_typevars(jl_tparam(v, i), env))
                    return 1;
            }
            return 0;
        }
        else if (jl_is_uniontype(v) || jl_is_intersecttype(v)) {
            if (has_free_typevars(((jl_uniontype_t*)v)->a, env))
                return 1;
           v = ((jl_uniontype_t*)v)->b;
        }
        else if (jl_is_some_Type(v)) {
            v = jl_some_Type_T(v);
        }
        else if (jl_is_vararg(v)) {
            jl_vararg_t *vm = (jl_vararg_t*)v;
            if (!vm->T)
                return 0;
            if (vm->N) {
                if (has_free_typevars(vm->N, env))
                    return 1;
            }
            v = vm->T;
        }
        else {
            return 0;
        }
    }
}

JL_DLLEXPORT int jl_has_free_typevars(jl_value_t *v) JL_NOTSAFEPOINT
{
    return has_free_typevars(v, NULL);
}

static void find_free_typevars(jl_value_t *v, jl_typeenv_t *env, jl_array_t *out) JL_CANSAFEPOINT
{
    while (1) {
        if (jl_is_typevar(v)) {
            jl_tvar_t *var = (jl_tvar_t*)v;
            if (!typeenv_has(env, var)) {
                jl_typeenv_t *newenv = (jl_typeenv_t*)alloca(sizeof(jl_typeenv_t));
                newenv->var = var;
                newenv->val = NULL;
                newenv->prev = env;
                env = newenv;
                if (var->lb != jl_bottom_type)
                    find_free_typevars(var->lb, env, out);
                if (var->ub != (jl_value_t*)jl_any_type)
                    find_free_typevars(var->ub, env, out);
                jl_array_ptr_1d_push(out, v);
            }
            return;
        }
        if (jl_is_typeapp(v)) {
            jl_array_ptr_1d_push(out, v);
            return;
        }
        while (jl_is_unionall(v)) {
            jl_unionall_t *ua = (jl_unionall_t*)v;
            if (ua->var->lb != jl_bottom_type)
                find_free_typevars(ua->var->lb, env, out);
            if (ua->var->ub != (jl_value_t*)jl_any_type)
                find_free_typevars(ua->var->ub, env, out);
            jl_typeenv_t *newenv = (jl_typeenv_t*)alloca(sizeof(jl_typeenv_t));
            newenv->var = ua->var;
            newenv->val = NULL;
            newenv->prev = env;
            env = newenv;
            v = ua->body;
        }
        if (jl_is_datatype(v)) {
            if (!((jl_datatype_t*)v)->hasfreetypevars)
                return;
            size_t i;
            for (i = 0; i < jl_nparams(v); i++) {
                jl_value_t *p = jl_tparam(v, i);
                find_free_typevars(p, env, out);
            }
            return;
        }
        else if (jl_is_uniontype(v) || jl_is_intersecttype(v)) {
            find_free_typevars(((jl_uniontype_t*)v)->a, env, out);
            v = ((jl_uniontype_t*)v)->b;
        }
        else if (jl_is_some_Type(v)) {
            v = jl_some_Type_T(v);
        }
        else if (jl_is_vararg(v)) {
            jl_vararg_t *vm = (jl_vararg_t *)v;
            if (!vm->T)
                return;
            if (vm->N) { // this swap the visited order, but we don't mind it
                find_free_typevars(vm->N, env, out);
            }
            v = vm->T;
        }
        else {
            return;
        }
    }
}

JL_DLLEXPORT jl_array_t *jl_find_free_typevars(jl_value_t *v)
{
    jl_array_t *out = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&out);
    find_free_typevars(v, NULL, out);
    JL_GC_POP();
    return out;
}

// test whether a type has vars bound by the given environment
int jl_has_bound_typevars(jl_value_t *v, jl_typeenv_t *env) JL_NOTSAFEPOINT
{
    while (1) {
        if (jl_is_datatype(v) && !((jl_datatype_t*)v)->hasfreetypevars)
            return 0;
        if (jl_is_typevar(v)) {
            return typeenv_has_ne(env, (jl_tvar_t*)v);
        }
        if (jl_is_typeapp(v)) {
            jl_typeapp_t *ta = (jl_typeapp_t*)v;
            if (jl_has_bound_typevars(ta->head, env))
                return 1;
            v = ta->param;
            continue;
        }
        while (jl_is_unionall(v)) {
            jl_unionall_t *ua = (jl_unionall_t*)v;
            if (ua->var->lb != jl_bottom_type && jl_has_bound_typevars(ua->var->lb, env))
                return 1;
            if (ua->var->ub != (jl_value_t*)jl_any_type && jl_has_bound_typevars(ua->var->ub, env))
                return 1;
            // Temporarily remove this var from env if necessary
            // Note that te might be bound more than once in the env, so
            // we remove it by setting it to itself in a new env.
            if (typeenv_has_ne(env, ua->var)) {
                jl_typeenv_t *newenv = (jl_typeenv_t*)alloca(sizeof(jl_typeenv_t));
                newenv->var = ua->var;
                newenv->val = (jl_value_t*)ua->var;
                newenv->prev = env;
                env = newenv;
            }
            v = ua->body;
        }
        // After unwrapping UnionAll, body might be TypeApp or TypeVar;
        // restart the loop so those checks at the top fire.
        if (jl_is_typeapp(v) || jl_is_typevar(v))
            continue;
        if (jl_is_datatype(v)) {
            if (!((jl_datatype_t*)v)->hasfreetypevars)
                return 0;
            size_t i;
            for (i = 0; i < jl_nparams(v); i++) {
                if (jl_has_bound_typevars(jl_tparam(v, i), env))
                    return 1;
            }
            return 0;
        }
        else if (jl_is_uniontype(v) || jl_is_intersecttype(v)) {
            if (jl_has_bound_typevars(((jl_uniontype_t*)v)->a, env))
                return 1;
           v = ((jl_uniontype_t*)v)->b;
        }
        else if (jl_is_some_Type(v)) {
            v = jl_some_Type_T(v);
        }
        else if (jl_is_vararg(v)) {
            jl_vararg_t *vm = (jl_vararg_t *)v;
            if (!vm->T)
                return 0;
            if (vm->N) {
                if (jl_has_bound_typevars(vm->N, env))
                    return 1;
            }
            v = vm->T;
        }
        else {
            return 0;
        }
    }
}

JL_DLLEXPORT int jl_has_typevar(jl_value_t *t, jl_tvar_t *v) JL_NOTSAFEPOINT
{
    jl_typeenv_t env = { v, NULL, NULL };
    return jl_has_bound_typevars(t, &env);
}

static int _jl_has_typevar_from_ua(jl_value_t *t, jl_unionall_t *ua, jl_typeenv_t *prev)
{
    jl_typeenv_t env = { ua->var, NULL, prev };
    if (jl_is_unionall(ua->body))
        return _jl_has_typevar_from_ua(t, (jl_unionall_t*)ua->body, &env);
    else
        return jl_has_bound_typevars(t, &env);
}

JL_DLLEXPORT int jl_has_typevar_from_unionall(jl_value_t *t, jl_unionall_t *ua)
{
    return _jl_has_typevar_from_ua(t, ua, NULL);
}

int jl_has_fixed_layout(jl_datatype_t *dt)
{
    // A type with isconcretetype=1 but types=NULL is currently being instantiated
    // and doesn't have a fixed layout yet. This prevents infinite recursion when
    // computing layouts for mutually recursive parametric types.
    if (dt->isconcretetype && dt->types != NULL)
        return 1;
    if (jl_is_genericmemory_type(dt)) { // GenericMemory{kind,addrspace,T} uses T for final layout, which is a parameter not a field however
        // optionally: return !layout_uses_free_typevars(jl_tparam1(dt), env);
        return 0;
    }
    if (dt->layout)
        return 1;
    if (dt->name->abstract)
        return 0;
    if (dt->name == jl_namedtuple_typename) {
        jl_value_t *names = jl_tparam0(dt);
        jl_value_t *types = jl_tparam1(dt);
        return !layout_uses_free_typevars(names, NULL) && !layout_uses_free_typevars(types, NULL);
    }
    if (dt->name == jl_tuple_typename)
        return 0;
    jl_svec_t *types = jl_get_fieldtypes(dt);
    size_t i, l = jl_svec_len(types);
    for (i = 0; i < l; i++) {
        jl_value_t *ft = jl_svecref(types, i);
        if (layout_uses_free_typevars(ft, NULL)) {
            // This might be inline-alloc, but we don't know the layout
            return 0;
        }
    }
    return 1;
}

int jl_type_mappable_to_c(jl_value_t *ty)
{
    assert(!jl_is_typevar(ty) && jl_is_type(ty));
    if (jl_is_array_type(ty) || jl_is_genericmemory_type(ty) ||
        (jl_is_datatype(ty) && ((jl_datatype_t*)ty)->layout != NULL &&
            jl_is_layout_opaque(((jl_datatype_t*)ty)->layout)))
        return 1; // as boxed
    if (jl_is_structtype(ty))
        return jl_has_fixed_layout((jl_datatype_t*)ty) && ((jl_datatype_t*)ty)->name->atomicfields == NULL;
    if (jl_is_primitivetype(ty))
        return 1; // as isbits
    if (ty == (jl_value_t*)jl_any_type || ty == (jl_value_t*)jl_bottom_type || jl_is_abstract_ref_type(ty))
        return 1; // as boxed
    return 0; // refuse to map Union and UnionAll to C
}

// Return true for any type (Integer or Unsigned) that can fit in a
// size_t and pass back value, else return false
JL_DLLEXPORT int jl_get_size(jl_value_t *val, size_t *pnt)
{
    if (jl_is_long(val)) {
        ssize_t slen = jl_unbox_long(val);
        if (slen < 0)
            jl_errorf("size or dimension is negative: %zd", slen);
        *pnt = slen;
        return 1;
    }
    return 0;
}

// --- type union ---

int jl_count_union_components(jl_value_t *v)
{
    size_t c = 0;
    while (jl_is_uniontype(v)) {
        jl_uniontype_t *u = (jl_uniontype_t*)v;
        c += jl_count_union_components(u->a);
        v = u->b;
    }
    return c + 1;
}

// Return the `*pi`th element of a nested type union, according to a
// standard traversal order. Anything that is not itself a `Union` is
// considered an "element". `*pi` is destroyed in the process.
static jl_value_t *nth_union_component(jl_value_t *v, int *pi) JL_NOTSAFEPOINT
{
    while (jl_is_uniontype(v)) {
        jl_uniontype_t *u = (jl_uniontype_t*)v;
        jl_value_t *a = nth_union_component(u->a, pi);
        if (a) return a;
        v = u->b;
    }
    if (*pi == 0)
        return v;
    (*pi)--;
    return NULL;
}

jl_value_t *jl_nth_union_component(jl_value_t *v, int i) JL_NOTSAFEPOINT
{
    return nth_union_component(v, &i);
}

// inverse of jl_nth_union_component
static int union_component_matches(jl_value_t *haystack, jl_value_t *needle) JL_NOTSAFEPOINT
{
    if (needle == haystack)
        return 1;
    if (jl_typeofbottom_type == NULL)
        return 0;
    if (needle == (jl_value_t*)jl_typeofbottom_type &&
        jl_is_typeeq(haystack) && jl_typeeq_T(haystack) == jl_bottom_type)
        return 1;
    if (haystack == (jl_value_t*)jl_typeofbottom_type &&
        jl_is_typeeq(needle) && jl_typeeq_T(needle) == jl_bottom_type)
        return 1;
    return 0;
}

int jl_find_union_component(jl_value_t *haystack, jl_value_t *needle, unsigned *nth) JL_NOTSAFEPOINT
{
    while (jl_is_uniontype(haystack)) {
        jl_uniontype_t *u = (jl_uniontype_t*)haystack;
        if (jl_find_union_component(u->a, needle, nth))
            return 1;
        haystack = u->b;
    }
    if (union_component_matches(haystack, needle))
        return 1;
    (*nth)++;
    return 0;
}

STATIC_INLINE const char *datatype_module_name(jl_value_t *t) JL_NOTSAFEPOINT
{
    if (((jl_datatype_t*)t)->name->module == NULL)
        return NULL;
    return jl_symbol_name(((jl_datatype_t*)t)->name->module->name);
}

STATIC_INLINE const char *str_(const char *s) JL_NOTSAFEPOINT
{
    return s == NULL ? "" : s;
}

STATIC_INLINE int cmp_(int a, int b) JL_NOTSAFEPOINT
{
    return a < b ? -1 : a > b;
}

STATIC_INLINE int is_value_param(jl_value_t *p) JL_NOTSAFEPOINT
{
    return !jl_is_type(p) && !jl_is_typevar(p) && !jl_is_vararg(p);
}

static int datatype_name_cmp(jl_value_t *a, jl_value_t *b) JL_NOTSAFEPOINT;

// order two value type parameters by their type, then by value; 0 if there is
// no deterministic order
static int value_param_cmp(jl_value_t *a, jl_value_t *b) JL_NOTSAFEPOINT
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(a);
    if (dt != (jl_datatype_t*)jl_typeof(b))
        return datatype_name_cmp((jl_value_t*)dt, jl_typeof(b));
    if (dt == jl_long_type) {
        // numerically, so e.g. `Val{-1}` prints before `Val{1}`
        ssize_t x = jl_unbox_long(a), y = jl_unbox_long(b);
        return x < y ? -1 : x > y;
    }
    if (dt == jl_symbol_type)
        return strcmp(jl_symbol_name((jl_sym_t*)a), jl_symbol_name((jl_sym_t*)b));
    // padding bytes are undefined, so only padding-free bits values compare by bytes
    if (dt->isbitstype && !dt->layout->flags.haspadding)
        return memcmp(a, b, jl_datatype_size(dt));
    return 0;
}

// a/b are jl_datatype_t* & not NULL
static int datatype_name_cmp(jl_value_t *a, jl_value_t *b) JL_NOTSAFEPOINT
{
    if (!jl_is_datatype(a))
        return jl_is_datatype(b) ? 1 : 0;
    if (!jl_is_datatype(b))
        return -1;
    int cmp = strcmp(str_(datatype_module_name(a)), str_(datatype_module_name(b)));
    if (cmp != 0)
        return cmp;
    cmp = strcmp(str_(jl_typename_str(a)), str_(jl_typename_str(b)));
    if (cmp != 0)
        return cmp;
    cmp = cmp_(jl_nparams(a), jl_nparams(b));
    if (cmp != 0)
        return cmp;
    // compare up to 3 type parameters
    for (int i = 0; i < 3 && i < jl_nparams(a); i++) {
        jl_value_t *ap = jl_tparam(a, i);
        jl_value_t *bp = jl_tparam(b, i);
        if (ap == bp) {
            continue;
        }
        else if (jl_is_datatype(ap) && jl_is_datatype(bp)) {
            cmp = datatype_name_cmp(ap, bp);
            if (cmp != 0)
                return cmp;
        }
        else if (jl_is_unionall(ap) && jl_is_unionall(bp)) {
            cmp = datatype_name_cmp(jl_unwrap_unionall(ap), jl_unwrap_unionall(bp));
            if (cmp != 0)
                return cmp;
        }
        else if (is_value_param(ap) && is_value_param(bp)) {
            cmp = value_param_cmp(ap, bp);
            if (cmp != 0)
                return cmp;
        }
        else {
            // give up
            cmp = 0;
        }
    }
    return cmp;
}

// sort singletons first, then DataTypes, then UnionAlls,
// ties broken alphabetically including module name & type parameters
static int union_sort_cmp(jl_value_t *a, jl_value_t *b) JL_NOTSAFEPOINT
{
    if (a == NULL)
        return b == NULL ? 0 : 1;
    if (b == NULL)
        return -1;
    if (jl_is_datatype(a)) {
        if (!jl_is_datatype(b))
            return -1;
        if (jl_is_datatype_singleton((jl_datatype_t*)a)) {
            if (jl_is_datatype_singleton((jl_datatype_t*)b))
                return datatype_name_cmp(a, b);
            return -1;
        }
        else if (jl_is_datatype_singleton((jl_datatype_t*)b)) {
            return 1;
        }
        else if (jl_isbits(a)) {
            if (jl_isbits(b))
                return datatype_name_cmp(a, b);
            return -1;
        }
        else if (jl_isbits(b)) {
            return 1;
        }
        else {
            return datatype_name_cmp(a, b);
        }
    }
    else {
        if (jl_is_datatype(b))
            return 1;
        // `Type{T}`/`TypeEgal{T}` wrappers sort like the `Type` datatypes they
        // used to be: after other datatypes, but before typevars and unionalls,
        // so that e.g. `Union{T, Type{T}}` keeps its historical component order
        // (the subtype environment binds a variable from the first matching
        // branch, and `foo(::Union{T, Type{T}}) where T` must bind `T` from the
        // `Type{T}` component when both match, #62141)
        if (jl_is_some_Type(a)) {
            if (!jl_is_some_Type(b))
                return -1;
            // order two wrappers by their parameters, as the parameter
            // comparison loop used to
            return datatype_name_cmp(jl_some_Type_T(a), jl_some_Type_T(b));
        }
        if (jl_is_some_Type(b))
            return 1;
        return datatype_name_cmp(jl_unwrap_unionall(a), jl_unwrap_unionall(b));
    }
}

static int count_union_components(jl_value_t **types, size_t n, int widen)
{
    size_t i, c = 0;
    for (i = 0; i < n; i++) {
        jl_value_t *e = types[i];
        while (jl_is_uniontype(e)) {
            jl_uniontype_t *u = (jl_uniontype_t*)e;
            c += count_union_components(&u->a, 1, widen);
            e = u->b;
        }
        if (widen && jl_is_unionall(e) && jl_is_uniontype(jl_unwrap_unionall(e))) {
            jl_uniontype_t *u = (jl_uniontype_t*)jl_unwrap_unionall(e);
            c += count_union_components(&u->a, 2, widen);
        }
        else {
            c++;
        }
    }
    return c;
}

static void flatten_type_union(jl_value_t **types, size_t n, jl_value_t **out, size_t *idx, int widen) JL_CANSAFEPOINT
{
    size_t i;
    for (i = 0; i < n; i++) {
        jl_value_t *e = types[i];
        while (jl_is_uniontype(e)) {
            jl_uniontype_t *u = (jl_uniontype_t*)e;
            flatten_type_union(&u->a, 1, out, idx, widen);
            e = u->b;
        }
        if (widen && jl_is_unionall(e) && jl_is_uniontype(jl_unwrap_unionall(e))) {
            // flatten this UnionAll into place by switching the union and unionall
            jl_uniontype_t *u = (jl_uniontype_t*)jl_unwrap_unionall(e);
            size_t old_idx = *idx;
            flatten_type_union(&u->a, 2, out, idx, widen);
            for (; old_idx < *idx; old_idx++)
                out[old_idx] = jl_rewrap_unionall(out[old_idx], e);
        }
        else {
            out[*idx] = e;
            (*idx)++;
        }
    }
}


static void isort_union(jl_value_t **a, size_t len) JL_NOTSAFEPOINT
{
    size_t i, j;
    for (i = 1; i < len; i++) {
        jl_value_t *x = a[i];
        for (j = i; j > 0; j--) {
            jl_value_t *y = a[j - 1];
            if (!(union_sort_cmp(x, y) < 0))
                break;
            a[j] = y;
        }
        a[j] = x;
    }
}

int simple_subtype(jl_value_t *a, jl_value_t *b, int hasfree, int isUnion)
{
    assert(hasfree == (jl_has_free_typevars(a) | (jl_has_free_typevars(b) << 1)));
    if (a == jl_bottom_type || b == (jl_value_t*)jl_any_type)
        return 1;
    if (jl_egal(a, b))
        return 1;
    if (hasfree == 0) {
        int mergeable = isUnion;
        if (!mergeable) // issue #24521: don't merge Type{T} where typeof(T) varies
            mergeable = !(jl_is_typeeq(a) && jl_is_typeeq(b) &&
             jl_typeof(jl_typeeq_T(a)) != jl_typeof(jl_typeeq_T(b)));
        return mergeable && jl_subtype(a, b);
    }
    if (jl_is_typevar(a)) {
        jl_value_t *na = ((jl_tvar_t*)a)->ub;
        hasfree &= (jl_has_free_typevars(na) | 2);
        return simple_subtype(na, b, hasfree, isUnion);
    }
    if (jl_is_typevar(b)) {
        jl_value_t *nb = ((jl_tvar_t*)b)->lb;
        // This branch is not valid if `b` obeys diagonal rule,
        // as it might normalize `Union` into a single `TypeVar`, e.g.
        // Tuple{Union{Int,T},T} where {T>:Int} != Tuple{T,T} where {T>:Int}
        if (is_leaf_bound(nb))
            return 0;
        hasfree &= ((jl_has_free_typevars(nb) << 1) | 1);
        return simple_subtype(a, nb, hasfree, isUnion);
    }
    if (b == (jl_value_t*)jl_typeofbottom_type) {
        // `Type{Union{}} == TypeofBottom` (the bottom object is unique). No
        // other `Type{T}` lies in a kind: its members include `UnionAll`
        // spellings of `T` (#33136).
        return jl_is_typeeq(a) && jl_typeeq_T(a) == jl_bottom_type;
    }
    return 0;
}

// merge Union{Tuple{}, Tuple{T}, Tuple{T, T, Vararg{T}}} into Tuple{Vararg{T}}
// assumes temp is already sorted by number of type parameters
STATIC_INLINE void merge_vararg_unions(jl_value_t **temp, size_t nt) JL_CANSAFEPOINT
{
    for (size_t i = nt-1; i > 0; i--) {
        // match types of form Tuple{T, ..., Vararg{T}}
        jl_value_t *tt = temp[i];
        if (!(tt && jl_is_tuple_type(tt))) continue;
        size_t nfields = jl_nparams(tt);
        if (nfields <= 1) continue;
        jl_value_t *va = jl_tparam(tt, nfields-1);
        if (jl_vararg_kind(va) != JL_VARARG_UNBOUND) continue;
        jl_value_t *t = jl_unwrap_vararg(va);
        size_t min_elements = nfields-1;
        for (size_t j = 0; j < nfields-1; j++)
            if (!jl_egal(jl_tparam(tt, j), t)) goto outer_loop;

        // look for Tuple{T, T, ...} then Tuple{T, ...}, etc
        for (long j = i-1; j >= 0; j--) {
            jl_value_t *ttj = temp[j];
            if (!(ttj && jl_is_tuple_type(ttj))) break;
            size_t nfieldsj = jl_nparams(ttj);
            if (nfieldsj >= min_elements) continue;
            if (nfieldsj != min_elements-1) break;
            for (size_t k = 0; k < nfieldsj; k++)
                if (!jl_egal(jl_tparam(ttj, k), t)) goto inner_loop;

            temp[j] = NULL;
            min_elements--;
 inner_loop:
            continue;
        }

        if (min_elements == nfields-1) continue;
        jl_value_t** params;
        JL_GC_PUSHARGS(params, min_elements+1);
        for (size_t j = 0; j < min_elements; j++)
            params[j] = t;
        params[min_elements] = va;
        temp[i] = jl_apply_type((jl_value_t*)jl_tuple_type, params, min_elements+1);
        JL_GC_POP();
 outer_loop:
        continue;
    }
}

JL_DLLEXPORT jl_value_t *jl_type_union(jl_value_t **ts, size_t n)
{
    if (n == 0)
        return (jl_value_t*)jl_bottom_type;
    size_t i;
    for (i = 0; i < n; i++) {
        jl_value_t *pi = ts[i];
        // reject the internal `Intersect` meet node (see #61917): it must not
        // be embedded into a user-visible `Union`.
        if (!(jl_is_type(pi) || jl_is_typevar(pi)))
            jl_type_error("Union", (jl_value_t*)jl_type_type, pi);
    }
    if (n == 1)
        return ts[0];

    size_t nt = count_union_components(ts, n, 1);
    jl_value_t **temp;
    JL_GC_PUSHARGS(temp, nt+1);
    size_t count = 0;
    flatten_type_union(ts, n, temp, &count, 1);
    assert(count == nt);
    size_t j;
    for (i = 0; i < nt; i++) {
        int has_free = temp[i] != NULL && jl_has_free_typevars(temp[i]);
        for (j = 0; j < nt; j++) {
            if (j != i && temp[i] && temp[j]) {
                int has_free2 = has_free | (jl_has_free_typevars(temp[j]) << 1);
                if (simple_subtype(temp[i], temp[j], has_free2, 1))
                    temp[i] = NULL;
            }
        }
    }
    isort_union(temp, nt);
    merge_vararg_unions(temp, nt);
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

static int simple_subtype2(jl_value_t *a, jl_value_t *b, int hasfree, int isUnion) JL_CANSAFEPOINT
{
    assert(hasfree == (jl_has_free_typevars(a) | (jl_has_free_typevars(b) << 1)));
    int subab = 0, subba = 0;
    if (jl_egal(a, b)) {
        subab = subba = 1;
    }
    else if (a == jl_bottom_type || b == (jl_value_t*)jl_any_type) {
        subab = 1;
    }
    else if (b == jl_bottom_type || a == (jl_value_t*)jl_any_type) {
        subba = 1;
    }
    else if (hasfree != 0) {
        subab = simple_subtype(a, b, hasfree, isUnion);
        subba = simple_subtype(b, a, ((hasfree & 2) >> 1) | ((hasfree & 1) << 1), isUnion);
    }
    else if (jl_is_typeeq(a) && jl_is_typeeq(b) &&
             jl_typeof(jl_typeeq_T(a)) != jl_typeof(jl_typeeq_T(b))) {
        // issue #24521: don't merge Type{T} where typeof(T) varies
    }
    else if (jl_typeof(a) == jl_typeof(b) && jl_types_struct_equiv(a, b)) {
        subab = subba = 1;
    }
    else {
        subab = jl_subtype(a, b);
        subba = jl_subtype(b, a);
    }
    return subab | (subba<<1);
}

jl_value_t *simple_union(jl_value_t *a, jl_value_t *b)
{
    size_t nta = count_union_components(&a, 1, 1);
    size_t ntb = count_union_components(&b, 1, 1);
    size_t nt = nta + ntb;
    jl_value_t **temp;
    JL_GC_PUSHARGS(temp, nt+1);
    size_t count = 0;
    flatten_type_union(&a, 1, temp, &count, 1);
    flatten_type_union(&b, 1, temp, &count, 1);
    assert(count == nt);
    size_t i, j;
    size_t ra = nta, rb = ntb;
    // first remove cross-redundancy and check if `a >: b` or `a <: b`.
    for (i = 0; i < nta; i++) {
        if (temp[i] == NULL) continue;
        int has_free = jl_has_free_typevars(temp[i]);
        for (j = nta; j < nt; j++) {
            if (temp[j] == NULL) continue;
            int has_free2 = has_free | (jl_has_free_typevars(temp[j]) << 1);
            int subs = simple_subtype2(temp[i], temp[j], has_free2, 0);
            int subab = subs & 1, subba = subs >> 1;
            if (subab) {
                temp[i] = NULL;
                if (!subba) ra = 0;
                count--;
                break;
            }
            else if (subba) {
                temp[j] = NULL;
                rb = 0;
                count--;
            }
        }
    }
    if (count == ra) {
        JL_GC_POP();
        return a;
    }
    if (count == rb) {
        JL_GC_POP();
        return b;
    }
    // then remove self-redundancy
    for (i = 0; i < nt; i++) {
        int has_free = temp[i] != NULL && jl_has_free_typevars(temp[i]);
        size_t jmin = i < nta ? 0 : nta;
        size_t jmax = i < nta ? nta : nt;
        for (j = jmin; j < jmax; j++) {
            if (j != i && temp[i] && temp[j]) {
                int has_free2 = has_free | (jl_has_free_typevars(temp[j]) << 1);
                if (simple_subtype(temp[i], temp[j], has_free2, 0))
                    temp[i] = NULL;
            }
        }
    }
    isort_union(temp, nt);
    merge_vararg_unions(temp, nt);
    temp[nt] = jl_bottom_type;
    size_t k;
    for (k = nt; k-- > 0; ) {
        if (temp[k] != NULL) {
            if (temp[nt] == jl_bottom_type)
                temp[nt] = temp[k];
            else
                temp[nt] = jl_new_struct(jl_uniontype_type, temp[k], temp[nt]);
        }
    }
    assert(temp[nt] != NULL);
    jl_value_t *tu = temp[nt];
    JL_GC_POP();
    return tu;
}

jl_value_t *simple_intersect(jl_value_t *a, jl_value_t *b, int overesi)
{
    // Unlike `Union`, we don't unwrap `UnionAll` here to avoid possible widening.
    size_t nta = count_union_components(&a, 1, 0);
    size_t ntb = count_union_components(&b, 1, 0);
    size_t nt = nta + ntb;
    jl_value_t **temp;
    JL_GC_PUSHARGS(temp, nt+1);
    size_t count = 0;
    flatten_type_union(&a, 1, temp, &count, 0);
    flatten_type_union(&b, 1, temp, &count, 0);
    assert(count == nt);
    size_t i, j;
    int8_t *stemp = (int8_t *)alloca(count);
    // first remove disjoint elements.
    memset(stemp, 0, count);
    for (i = 0; i < nta; i++) {
        int hasfree = jl_has_free_typevars(temp[i]);
        for (j = nta; j < nt; j++) {
            if (!stemp[i] || !stemp[j]) {
                int intersect = !hasfree && !jl_has_free_typevars(temp[j]);
                if (!(intersect ? jl_has_empty_intersection(temp[i], temp[j]) : obviously_disjoint(temp[i], temp[j], 0)))
                    stemp[i] = stemp[j] = 1;
            }
        }
    }
    for (i = 0; i < nt; i++) {
        temp[i] = stemp[i] ? temp[i] : NULL;
    }
    // then check subtyping.
    // stemp[k] == -1 : ∃i temp[k] >:ₛ temp[i]
    // stemp[k] == 1 : ∃i temp[k] == temp[i]
    // stemp[k] == 2 : ∃i temp[k] <:ₛ temp[i]
    memset(stemp, 0, count);
    int all_disjoint = 1, subs[2] = {1, 1}, rs[2] = {1, 1};
    for (i = 0; i < nta; i++) {
        if (temp[i] == NULL) continue;
        all_disjoint = 0;
        int has_free = jl_has_free_typevars(temp[i]);
        for (j = nta; j < nt; j++) {
            if (temp[j] == NULL) continue;
            int has_free2 = has_free | (jl_has_free_typevars(temp[j]) << 1);
            int subs = simple_subtype2(temp[i], temp[j], has_free2, 0);
            int subab = subs & 1, subba = subs >> 1;
            if (subba && !subab) {
                stemp[i] = -1;
                if (stemp[j] >= 0) stemp[j] = 2;
            }
            else if (subab && !subba) {
                stemp[j] = -1;
                if (stemp[i] >= 0) stemp[i] = 2;
            }
            else if (subs) {
                if (stemp[i] == 0) stemp[i] = 1;
                if (stemp[j] == 0) stemp[j] = 1;
            }
        }
    }
    if (!all_disjoint) {
        for (i = 0; i < nt; i++) {
            subs[i >= nta] &= (temp[i] == NULL || stemp[i] > 0);
            rs[i >= nta] &= (temp[i] != NULL && stemp[i] > 0);
        }
        // return a(b) if a(b) <: b(a)
        if (rs[0]) {
            JL_GC_POP();
            return a;
        }
        if (rs[1]) {
            JL_GC_POP();
            return b;
        }
    }
    // return `Union{}` for `merge_env` if we can't prove `<:` or `>:`
    if (all_disjoint || (!overesi && !subs[0] && !subs[1])) {
        JL_GC_POP();
        return jl_bottom_type;
    }
    if (!subs[0] && !subs[1] && overesi == 1) {
        // Neither operand subsumes the other and they are not disjoint, so the
        // meet is not expressible as a single existing type. With `overesi==1`
        // (exact-meet mode, used on the subtype path) keep it as an internal
        // `Intersect{a, b}` node rather than over-approximating to one side,
        // which would silently drop the other. `overesi==2` falls through to
        // the legacy over-approximation below (used to widen an `Intersect`
        // before it could escape into a static parameter). See #61917.
        JL_GC_POP();
        return jl_new_struct(jl_intersect_type, a, b);
    }
    nt = subs[0] ? nta : subs[1] ? nt  : nt;
    i  = subs[0] ? 0   : subs[1] ? nta : 0;
    count = nt - i;
    if (!subs[0] && !subs[1]) {
        // over-approximate (`overesi==2`): keep only `a` components with strict
        // `<:` and all of `b`, then union them.
        for (j = 0; j < nt; j++)
            if (stemp[j] < (j < nta ? 2 : 0))
                temp[j] = NULL;
    }
    isort_union(&temp[i], count);
    temp[nt] = jl_bottom_type;
    size_t k;
    for (k = nt; k-- > i; ) {
        if (temp[k] != NULL) {
            if (temp[nt] == jl_bottom_type)
                temp[nt] = temp[k];
            else
                temp[nt] = jl_new_struct(jl_uniontype_type, temp[k], temp[nt]);
        }
    }
    assert(temp[nt] != NULL);
    jl_value_t *tu = temp[nt];
    JL_GC_POP();
    return tu;
}

// unionall types -------------------------------------------------------------

JL_DLLEXPORT jl_value_t *jl_type_unionall(jl_tvar_t *v, jl_value_t *body)
{
    if (jl_is_vararg(body)) {
        if (jl_options.depwarn) {
            if (jl_options.depwarn == JL_OPTIONS_DEPWARN_ERROR)
                jl_error("Wrapping `Vararg` directly in UnionAll is deprecated (wrap the tuple instead).\nYou may need to write `f(x::Vararg{T})` rather than `f(x::Vararg{<:T})` or `f(x::Vararg{T}) where T` instead of `f(x::Vararg{T} where T)`.");
            jl_printf(JL_STDERR, "WARNING: Wrapping `Vararg` directly in UnionAll is deprecated (wrap the tuple instead).\nYou may need to write `f(x::Vararg{T})` rather than `f(x::Vararg{<:T})` or `f(x::Vararg{T}) where T` instead of `f(x::Vararg{T} where T)`.\nTo make this warning an error, and hence obtain a stack trace, use `julia --depwarn=error`.\n");
        }
        jl_vararg_t *vm = (jl_vararg_t*)body;
        int T_has_tv = vm->T && jl_has_typevar(vm->T, v);
        int N_has_tv = vm->N && jl_has_typevar(vm->N, v);
        if (!T_has_tv && !N_has_tv) {
            return body;
        }
        if (T_has_tv && N_has_tv) {
            jl_error("Wrapping `Vararg` directly in UnionAll is disallowed if the typevar occurs in both `T` and `N`");
        }
        if (T_has_tv) {
            jl_value_t *wrapped = jl_type_unionall(v, vm->T);
            JL_GC_PUSH1(&wrapped);
            wrapped = (jl_value_t*)jl_wrap_vararg(wrapped, vm->N, 1, 0);
            JL_GC_POP();
            return wrapped;
        }
        else {
            assert(N_has_tv);
            assert(vm->N == (jl_value_t*)v);
            return (jl_value_t*)jl_wrap_vararg(vm->T, NULL, 1, 0);
        }
    }
    if (!jl_is_type(body) && !jl_is_typevar(body) && !jl_is_typeapp(body))
        jl_type_error("UnionAll", (jl_value_t*)jl_type_type, body);
    // normalize `T where T<:S` => S
    if (body == (jl_value_t*)v)
        return v->ub;
    // where var doesn't occur in body just return body
    if (jl_is_typeeq(body) && v->ub != (jl_value_t*)jl_any_type) {
        if (!jl_has_typevar(body, v))
            return body;
    }
    else if (!jl_has_typevar(body, v))
        return body;
    //if (v->lb == v->ub)  // TODO maybe
    //    return jl_substitute_var(body, v, v->ub);
    return jl_new_struct(jl_unionall_type, v, body);
}

// --- type instantiation and cache ---

static int typekey_eq(jl_datatype_t *tt, jl_value_t **key, size_t n) JL_CANSAFEPOINT
{
    size_t j;
    // TODO: This shouldn't be necessary
    JL_GC_PROMISE_ROOTED(tt);
    size_t tnp = jl_nparams(tt);
    if (n != tnp)
        return 0;
    for (j = 0; j < n; j++) {
        jl_value_t *kj = key[j];
        jl_value_t *tj = jl_svecref(tt->parameters, j);
        if (tj != kj) {
            if (tt->name == jl_tuple_typename) {
                // require exact same Type{T} in covariant context. see e.g. issue #22842
                // this should work because `Tuple{Type}`s don't need unique pointers, and aren't the
                // direct tags of values (concrete) so we don't rely on pointer equality.
                if (jl_is_some_Type(tj) || jl_is_some_Type(kj))
                    return 0;
            }
            if (jl_type_equality_is_identity(tj, kj))
                return 0;
            if (!jl_types_equal(tj, kj))
                return 0;
        }
    }
    return 1;
}

// These `value` functions return the same values as the primary functions,
// but operate on the typeof/Typeof each object in an array
static int typekeyvalue_eq(jl_datatype_t *tt, jl_value_t *key1, jl_value_t **key, size_t n, int leaf) JL_CANSAFEPOINT
{
    size_t j;
    // TODO: This shouldn't be necessary
    JL_GC_PROMISE_ROOTED(tt);
    size_t tnp = jl_nparams(tt);
    if (n != tnp)
        return 0;
    for (j = 0; j < n; j++) {
        jl_value_t *kj = j == 0 ? key1 : key[j - 1];
        jl_value_t *tj = jl_svecref(tt->parameters, j);
        if (leaf && jl_is_typeeq(tj)) {
            // a closed `Type{T}` key can only belong to a by-type constructed tuple,
            // which must not capture (`==`-equal but non-egal) argument values (#61323)
            jl_value_t *tp0 = jl_typeeq_T(tj);
            if (!jl_has_free_typevars(tp0))
                return 0;
            if (!(kj == tp0 || (jl_typeof(tp0) == jl_typeof(kj) && jl_types_equal(tp0, kj))))
                return 0;
        }
        else if (leaf && jl_is_typeegal(tj)) {
            // `TypeEgal{T}` matches the argument value by egality (`===`)
            jl_value_t *tp0 = jl_typeegal_T(tj);
            if (!(kj == tp0 || jl_egal(tp0, kj)))
                return 0;
        }
        else if (jl_typeof(kj) != tj) {
            return 0;
        }
        else if (leaf && jl_is_kind(tj)) {
            // type-valued arguments are keyed through the wrappers above, except
            // `Union{}` (the unique instance of `typeof(Union{})`)
            if (tj != (jl_value_t*)jl_typeofbottom_type)
                return 0;
        }
    }
    return 1;
}

static unsigned typekey_hash(jl_typename_t *tn, jl_value_t **key, size_t n, int nofail) JL_NOTSAFEPOINT;
static unsigned typekeyvalue_hash(jl_typename_t *tn, jl_value_t *key1, jl_value_t **key, size_t n, int leaf) JL_NOTSAFEPOINT;
static jl_value_t *extract_wrapper(jl_value_t *t JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT JL_GLOBALLY_ROOTED;

/* returns val if key is in hash, otherwise NULL */
// A comparison may compute a lazy supertype, which can insert into this cache: an
// insert lands later in this probe, but a rehash (which always grows the table)
// moves every entry, so reload the cache every step and restart the probe on one.
static jl_datatype_t *lookup_type_set(jl_typename_t *tn, jl_value_t **key, size_t n, uint_t hv) JL_CANSAFEPOINT
{
    while (1) {
        jl_svec_t *cache = jl_atomic_load_relaxed(&tn->cache);
        size_t sz = jl_svec_len(cache);
        if (sz == 0)
            return NULL;
        size_t maxprobe = max_probe(sz);
        size_t index = h2index(hv, sz);
        size_t orig = index;
        size_t iter = 0;
        do {
            jl_datatype_t *val = jl_atomic_load_relaxed(&((_Atomic(jl_datatype_t*)*)jl_svec_data(cache))[index]);
            if ((jl_value_t*)val == jl_nothing)
                return NULL;
            if (val->hash == hv && typekey_eq(val, key, n))
                return val;
            cache = jl_atomic_load_relaxed(&tn->cache);
            if (jl_svec_len(cache) != sz)
                break; // rehashed: restart the probe in the new table
            index = (index + 1) & (sz - 1);
            iter++;
        } while (iter <= maxprobe && index != orig);
        if (jl_svec_len(cache) == sz)
            return NULL;
    }
}

/* returns val if key is in hash, otherwise NULL */
static jl_datatype_t *lookup_type_setvalue(jl_typename_t *tn, jl_value_t *key1, jl_value_t **key, size_t n, uint_t hv, int leaf) JL_CANSAFEPOINT
{
    while (1) {
        jl_svec_t *cache = jl_atomic_load_relaxed(&tn->cache);
        size_t sz = jl_svec_len(cache);
        if (sz == 0)
            return NULL;
        size_t maxprobe = max_probe(sz);
        size_t index = h2index(hv, sz);
        size_t orig = index;
        size_t iter = 0;
        do {
            jl_datatype_t *val = jl_atomic_load_relaxed(&((_Atomic(jl_datatype_t*)*)jl_svec_data(cache))[index]);
            if ((jl_value_t*)val == jl_nothing)
                return NULL;
            if (val->hash == hv && typekeyvalue_eq(val, key1, key, n, leaf))
                return val;
            cache = jl_atomic_load_relaxed(&tn->cache);
            if (jl_svec_len(cache) != sz)
                break; // rehashed: restart the probe in the new table
            index = (index + 1) & (sz - 1);
            iter++;
        } while (iter <= maxprobe && index != orig);
        if (jl_svec_len(cache) == sz)
            return NULL;
    }
}

// look up a type in a cache by linear search.
// if found, returns the index of the found item. if not found, returns
// ~n, where n is the index where the type should be inserted.
// A comparison may compute a lazy supertype, which can append to (or regrow) this
// cache, so reload it every step: entries are only appended, and keep their index.
static ssize_t lookup_type_idx_linear(jl_typename_t *tn, jl_value_t **key, size_t n) JL_CANSAFEPOINT
{
    if (n == 0)
        return -1;
    for (size_t i = 0; ; i++) {
        jl_svec_t *cache = jl_atomic_load_relaxed(&tn->linearcache);
        if (i >= jl_svec_len(cache))
            return ~(ssize_t)i;
        jl_datatype_t *tt = jl_atomic_load_relaxed(&((_Atomic(jl_datatype_t*)*)jl_svec_data(cache))[i]);
        if ((jl_value_t*)tt == jl_nothing)
            return ~(ssize_t)i;
        if (typekey_eq(tt, key, n))
            return i;
    }
}

static ssize_t lookup_type_idx_linearvalue(jl_typename_t *tn, jl_value_t *key1, jl_value_t **key, size_t n) JL_CANSAFEPOINT
{
    if (n == 0)
        return -1;
    for (size_t i = 0; ; i++) {
        jl_svec_t *cache = jl_atomic_load_relaxed(&tn->linearcache);
        if (i >= jl_svec_len(cache))
            return ~(ssize_t)i;
        jl_datatype_t *tt = jl_atomic_load_relaxed(&((_Atomic(jl_datatype_t*)*)jl_svec_data(cache))[i]);
        if ((jl_value_t*)tt == jl_nothing)
            return ~(ssize_t)i;
        if (typekeyvalue_eq(tt, key1, key, n, 1))
            return i;
    }
}

static jl_value_t *lookup_type(jl_typename_t *tn JL_PROPAGATES_ROOT, jl_value_t **key, size_t n) JL_CANSAFEPOINT
{
    JL_TIMING(TYPE_CACHE_LOOKUP, TYPE_CACHE_LOOKUP);
    unsigned hv = typekey_hash(tn, key, n, 0);
    if (hv) {
        return (jl_value_t*)lookup_type_set(tn, key, n, hv);
    }
    else {
        ssize_t idx = lookup_type_idx_linear(tn, key, n);
        return (idx < 0) ? NULL : jl_svecref(jl_atomic_load_relaxed(&tn->linearcache), idx);
    }
}

static jl_value_t *lookup_typevalue(jl_typename_t *tn, jl_value_t *key1, jl_value_t **key, size_t n, int leaf) JL_CANSAFEPOINT
{
    JL_TIMING(TYPE_CACHE_LOOKUP, TYPE_CACHE_LOOKUP);
    unsigned hv = typekeyvalue_hash(tn, key1, key, n, leaf);
    if (hv) {
        return (jl_value_t*)lookup_type_setvalue(tn, key1, key, n, hv, leaf);
    }
    else {
        assert(leaf);
        ssize_t idx = lookup_type_idx_linearvalue(tn, key1, key, n);
        return (idx < 0) ? NULL : jl_svecref(jl_atomic_load_relaxed(&tn->linearcache), idx);
    }
}

static int cache_insert_type_set_(jl_svec_t *a, jl_datatype_t *val, uint_t hv, int atomic)
{
    _Atomic(jl_value_t*) *tab = (_Atomic(jl_value_t*)*)jl_svec_data(a);
    size_t sz = jl_svec_len(a);
    if (sz <= 1)
        return 0;
    size_t orig, index, iter;
    iter = 0;
    index = h2index(hv, sz);
    orig = index;
    size_t maxprobe = max_probe(sz);
    do {
        jl_value_t *tab_i = jl_atomic_load_relaxed(&tab[index]);
        if (tab_i == jl_nothing) {
            jl_gc_wb(a, (void*)&tab[index], (jl_value_t*)val);
            if (atomic)
                jl_atomic_store_release(&tab[index], (jl_value_t*)val);
            else
                jl_atomic_store_relaxed(&tab[index], (jl_value_t*)val);
            return 1;
        }
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);

    return 0;
}

static void cache_insert_type_set(jl_datatype_t *val, uint_t hv) JL_CANSAFEPOINT
{
    jl_svec_t *a = jl_atomic_load_relaxed(&val->name->cache);
    while (1) {
        JL_GC_PROMISE_ROOTED(a);
        if (cache_insert_type_set_(a, val, hv, 1))
            return;

        /* table full */
        /* rehash to grow and retry the insert */
        /* it's important to grow the table really fast; otherwise we waste */
        /* lots of time rehashing all the keys over and over. */
        size_t newsz;
        size_t sz = jl_svec_len(a);
        if (sz < HT_N_INLINE)
            newsz = HT_N_INLINE;
        else if (sz >= (1 << 19) || (sz <= (1 << 8)))
            newsz = sz << 1;
        else
            newsz = sz << 2;
        a = cache_rehash_set(a, newsz);
        jl_gc_write_atomic(val->name, val->name->cache, jl_svec_t, a, release);
    }
}

jl_svec_t *cache_rehash_set(jl_svec_t *a, size_t newsz)
{
    newsz = newsz ? next_power_of_two(newsz) : 0;
    jl_value_t **ol = jl_svec_data(a);
    size_t sz = jl_svec_len(a);
    while (1) {
        size_t i;
        jl_svec_t *newa = jl_svec_fill(newsz, jl_nothing);
        JL_GC_PUSH1(&newa);
        for (i = 0; i < sz; i += 1) {
            jl_value_t *val = ol[i];
            if (val != jl_nothing) {
                uint_t hv = ((jl_datatype_t*)val)->hash;
                if (!cache_insert_type_set_(newa, (jl_datatype_t*)val, hv, 0)) {
                    break;
                }
            }
        }
        JL_GC_POP();
        if (i == sz)
            return newa;
        newsz <<= 1;
    }
}

static void cache_insert_type_linear(jl_datatype_t *type, ssize_t insert_at) JL_CANSAFEPOINT
{
    jl_svec_t *cache = jl_atomic_load_relaxed(&type->name->linearcache);
    assert(jl_is_svec(cache));
    size_t n = jl_svec_len(cache);
    if (n == 0 || jl_svecref(cache, n - 1) != jl_nothing) {
        jl_svec_t *nc = jl_svec_fill(n < 4 ? 4 : n * 2, jl_nothing);
        memcpy(jl_svec_data(nc), jl_svec_data(cache), sizeof(void*) * n);
        jl_gc_write_atomic(type->name, type->name->linearcache, jl_svec_t, nc, release);
        cache = nc;
    }
    assert(jl_svecref(cache, insert_at) == jl_nothing);
    jl_svecset(cache, insert_at, (jl_value_t*)type); // todo: make this an atomic-store
}

#ifndef NDEBUG
static int is_cacheable(jl_datatype_t *type)
{
    // ensure cache only contains types whose behavior will not depend on the
    // identities of contained TypeVars
    return !jl_has_free_typevars((jl_value_t*)type);
}
#endif


void jl_cache_type_(jl_datatype_t *type)
{
    JL_TIMING(TYPE_CACHE_INSERT, TYPE_CACHE_INSERT);
    assert(is_cacheable(type));
    jl_value_t **key = jl_svec_data(type->parameters);
    int n = jl_svec_len(type->parameters);
    unsigned hv = typekey_hash(type->name, key, n, 0);
    if (hv) {
        assert(hv == type->hash);
        // Type cache keys must be unique.
        assert(jl_lookup_cache_type_(type) == NULL);
        cache_insert_type_set(type, hv);
    }
    else {
        ssize_t idx = lookup_type_idx_linear(type->name, key, n);
        assert(idx < 0);
        cache_insert_type_linear(type, ~idx);
    }
}

jl_datatype_t *jl_lookup_cache_type_(jl_datatype_t *type)
{
    assert(is_cacheable(type));
    jl_value_t **key = jl_svec_data(type->parameters);
    int n = jl_svec_len(type->parameters);
    return (jl_datatype_t*)lookup_type(type->name, key, n);
}

// compute whether kj might actually be a subtype of something in the cache
// (which otherwise would normally be comparable with pointer-egal)
static int maybe_subtype_of_cache(jl_value_t *kj, int covariant) JL_NOTSAFEPOINT
{
    jl_value_t *uw = jl_is_unionall(kj) ? jl_unwrap_unionall(kj) : kj;
    if (jl_is_datatype(uw)) {
        jl_datatype_t *dt = (jl_datatype_t*)uw;
        return dt->maybe_subtype_of_cache;
    }
    else if (jl_is_uniontype(uw)) {
        int ca = maybe_subtype_of_cache(((jl_uniontype_t*)uw)->a, covariant);
        int cb = maybe_subtype_of_cache(((jl_uniontype_t*)uw)->b, covariant);
        return ca && cb;
    }
    else if (uw == jl_bottom_type) {
        return 1;
    }
    else if (jl_is_typevar(uw) && !covariant) { // assume Tuple's bounds are always degenerate
        // TODO: improve this bound if we can prove that typeintersect(lb,ub) is a leaftype
        jl_tvar_t *tv = (jl_tvar_t*)uw;
        return tv->lb == tv->ub ||
               tv->lb != jl_bottom_type;
    }
    return 1;
}

// compute whether kj might have a supertype which is actually concrete
static int has_concrete_supertype(jl_value_t *kj) JL_NOTSAFEPOINT
{
    jl_value_t *uw = jl_is_unionall(kj) ? jl_unwrap_unionall(kj) : kj;
    if (jl_is_datatype(uw)) {
        jl_datatype_t *dt = (jl_datatype_t*)uw;
        if (dt->name->abstract)
            return 0;
        if (!dt->maybe_subtype_of_cache)
            return 0;
        if (dt->name == jl_tuple_typename) {
            // check tuple parameters recursively for has_concrete_supertype
            size_t i, n = jl_nparams(dt);
            for (i = 0; i < n; i++) {
                jl_value_t *p = jl_tparam(dt, i);
                if (jl_is_vararg(p))
                    p = jl_unwrap_vararg(p);
                if (!has_concrete_supertype(p))
                    return 0;
            }
        }
        return 1;
    }
    else if (jl_is_uniontype(uw)) {
        int ca = has_concrete_supertype(((jl_uniontype_t*)uw)->a);
        int cb = has_concrete_supertype(((jl_uniontype_t*)uw)->b);
        return ca && cb;
    }
    else if (jl_is_some_Type(uw)) {
        return 1;
    }
    else if (uw == jl_bottom_type) {
        return 1;
    }
    else if (jl_is_typevar(uw)) {
        jl_tvar_t *tv = (jl_tvar_t*)uw;
        return has_concrete_supertype(tv->ub);
    }
    return 0;
}

int jl_type_equality_is_identity(jl_value_t *t1, jl_value_t *t2) JL_NOTSAFEPOINT
{
    int c1 = jl_is_concrete_type(t1);
    int c2 = jl_is_concrete_type(t2);
    if (c1 && c2) {
        if (((jl_datatype_t*)t1)->name != jl_tuple_typename)
            return 1;
        if (((jl_datatype_t*)t2)->name != jl_tuple_typename)
            return 1;
        if (((jl_datatype_t*)t1)->has_concrete_subtype && ((jl_datatype_t*)t2)->has_concrete_subtype)
            return 1;
        // e.g. Tuple{Union{}} and Tuple{Int} are both concrete!
    }
    if (c1 && !has_concrete_supertype(t2))
        return 1;
    if (c2 && !has_concrete_supertype(t1))
        return 1;
    return 0;
}

// type instantiation

static int within_typevar(jl_value_t *t, jl_value_t *vlb, jl_value_t *vub) JL_CANSAFEPOINT
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
    return ((jl_has_free_typevars(vlb) || jl_subtype(vlb, lb)) &&
            (jl_has_free_typevars(vub) || jl_subtype(ub, vub)));
}

struct _jl_typestack_t;
typedef struct _jl_typestack_t jl_typestack_t;

static jl_value_t *inst_datatype_inner(jl_datatype_t *dt, jl_svec_t *p, jl_value_t **iparams, size_t ntp,
                                       jl_typestack_t *stack, jl_typeenv_t *env, int check, int nothrow) JL_CANSAFEPOINT;
static jl_value_t *instantiate_unionall_(jl_unionall_t *u, jl_value_t *p) JL_CANSAFEPOINT;
static jl_value_t *jl_apply_tuple_type_v_(jl_value_t **p, size_t np, jl_svec_t *params, int check) JL_CANSAFEPOINT;
static jl_svec_t *compute_fieldtypes_(jl_datatype_t *st JL_PROPAGATES_ROOT, void *stack, int cacheable) JL_CANSAFEPOINT;

// Build an environment mapping a TypeName's parameters to parameter values.
// This is the environment needed for instantiating a type's supertype and field types.
static jl_value_t *inst_datatype_env(jl_value_t *dt, jl_svec_t *p, jl_value_t **iparams, size_t ntp,
                                     jl_typestack_t *stack, jl_typeenv_t *env, int c) JL_CANSAFEPOINT
{
    if (jl_is_datatype(dt))
        return inst_datatype_inner((jl_datatype_t*)dt, p, iparams, ntp, stack, env, 1, 0);
    assert(jl_is_unionall(dt));
    jl_unionall_t *ua = (jl_unionall_t*)dt;
    jl_typeenv_t e = { ua->var, iparams[c], env };
    return inst_datatype_env(ua->body, p, iparams, ntp, stack, &e, c + 1);
}

static jl_value_t *apply_type_(jl_value_t *tc, jl_value_t **params, size_t n) JL_CANSAFEPOINT
{
    if (tc == (jl_value_t*)jl_anytuple_type)
        return jl_apply_tuple_type_v_(params, n, NULL, 1);
    if (tc == (jl_value_t*)jl_uniontype_type)
        return (jl_value_t*)jl_type_union(params, n);
    if (tc == (jl_value_t*)jl_typeeq_type) {
        if (n == 0)
            return (jl_value_t*)jl_type_type;
        if (n != 1)
            jl_errorf("too many parameters for type `TypeEq`: expected 1, got %zu", n);
        if (!jl_valid_type_param(params[0]))
            jl_type_error_rt("TypeEq", "parameter", (jl_value_t*)jl_type_type, params[0]);
        return (jl_value_t*)jl_wrap_Type(params[0]);
    }
    if (tc == (jl_value_t*)jl_typeegal_type) {
        if (n != 1)
            jl_errorf("wrong number of parameters for type `TypeEgal`: expected 1, got %zu", n);
        if (!jl_is_type(params[0]) || jl_has_free_typevars(params[0]))
            jl_type_error_rt("TypeEgal", "parameter", (jl_value_t*)jl_type_type, params[0]);
        return jl_wrap_TypeEgal(params[0]);
    }
    size_t i;
    if (n > 1) {
        // detect common case of applying a wrapper, where we know that all parameters will
        // end up as direct parameters of a certain datatype, which can be optimized.
        jl_value_t *u = jl_unwrap_unionall(tc);
        if (jl_is_datatype(u) && n == jl_nparams((jl_datatype_t*)u) &&
            ((jl_datatype_t*)u)->name->wrapper == tc) {
            return inst_datatype_env(tc, NULL, params, n, NULL, NULL, 0);
        }
    }
    JL_GC_PUSH1(&tc);
    jl_value_t *tc0 = tc;
    for (i=0; i < n; i++) {
        if (!jl_is_unionall(tc0)){
            char *typ = "";
            if (jl_is_datatype(tc0))
                typ = jl_symbol_name_(((jl_datatype_t*)tc0)->name->name);
            jl_errorf("too many parameters for type `%s`: expected %zu, got %zu", typ, i, n);
        }
        jl_value_t *pi = params[i];

        tc0 = ((jl_unionall_t*)tc0)->body;
        // doing a substitution can cause later UnionAlls to be dropped,
        // as in `NTuple{0,T} where T` => `Tuple{}`. allow values to be
        // substituted for these missing parameters.
        // TODO: figure out how to get back a type error for e.g.
        // S = Tuple{Vararg{T,N}} where T<:NTuple{N} where N
        // S{0,Int}
        if (!jl_is_unionall(tc)) continue;

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
                jl_type_error_rt(jl_is_datatype(inner) ? jl_symbol_name(inner->name->name) : "Type",
                                 jl_symbol_name(ua->var->name), (jl_value_t*)ua->var, pi);
        }

        tc = instantiate_unionall_(ua, pi);
    }
    JL_GC_POP();
    return tc;
}

jl_value_t *jl_apply_type(jl_value_t *tc, jl_value_t **params, size_t n)
{
    return apply_type_(tc, params, n);
}

JL_DLLEXPORT jl_value_t *jl_apply_type1(jl_value_t *tc, jl_value_t *p1)
{
    return jl_apply_type(tc, &p1, 1);
}

JL_DLLEXPORT jl_value_t *jl_apply_type2(jl_value_t *tc, jl_value_t *p1, jl_value_t *p2)
{
    jl_value_t *args[2];
    args[0] = p1;
    args[1] = p2;
    return jl_apply_type(tc, args, 2);
}

JL_DLLEXPORT jl_value_t *jl_apply_type3(jl_value_t *tc, jl_value_t *p1, jl_value_t *p2, jl_value_t *p3)
{
    jl_value_t *args[3];
    args[0] = p1;
    args[1] = p2;
    args[2] = p3;
    return jl_apply_type(tc, args, 3);
}

jl_datatype_t *jl_apply_modify_type(jl_value_t *dt)
{
    jl_datatype_t *rettyp = (jl_datatype_t*)jl_apply_type2(jl_pair_type, dt, dt);
    JL_GC_PROMISE_ROOTED(rettyp); // (JL_ALWAYS_LEAFTYPE)
    return rettyp;
}

jl_datatype_t *jl_apply_cmpswap_type(jl_value_t *ty)
{
    jl_value_t *params[2];
    jl_value_t *names = jl_atomic_load_relaxed(&cmpswap_names);
    if (names == NULL) {
        params[0] = (jl_value_t*)jl_symbol("old");
        params[1] = (jl_value_t*)jl_symbol("success");
        jl_value_t *lnames = jl_f_tuple(NULL, params, 2);
        if (jl_atomic_cmpswap(&cmpswap_names, &names, lnames))
            names = jl_atomic_load_relaxed(&cmpswap_names); // == lnames
    }
    params[0] = ty;
    params[1] = (jl_value_t*)jl_bool_type;
    jl_value_t *tuptyp = jl_apply_tuple_type_v(params, 2);
    JL_GC_PUSH1(&tuptyp);
    jl_datatype_t *rettyp = (jl_datatype_t*)jl_apply_type2((jl_value_t*)jl_namedtuple_type, names, tuptyp);
    JL_GC_POP();
    return rettyp;
}

JL_EXTENSION struct _jl_typestack_t {
    jl_datatype_t *tt;
    struct _jl_typestack_t *prev;
    // the bottom of a lazy supertype computation (`jl_datatype_compute_super`):
    // it completes a type that exists, so the levels above it exist too
    int lazy;
};

// The instantiation stack of the critical region of `inst_datatype_inner`
// (under `typecache_lock`) on the task holding the lock, NULL outside one. A
// lazy supertype computation nested in that region continues from it, so that
// a type the region is still building is found on the stack by name and
// parameters instead of being instantiated a second time
static jl_typestack_t *typecache_stack = NULL;

#ifndef NDEBUG
// whether `base` is `stack` or one of its tails
static int typestack_extends(jl_typestack_t *stack, jl_typestack_t *base) JL_NOTSAFEPOINT
{
    while (stack != base) {
        if (stack == NULL)
            return 0;
        stack = stack->prev;
    }
    return 1;
}
#endif

// `typecache_stack` when the current task holds `typecache_lock`, else NULL
// (it then belongs to another task's region, or to none)
static jl_typestack_t *typecache_stack_held(void) JL_NOTSAFEPOINT
{
    return jl_mutex_held(&typecache_lock) ? typecache_stack : NULL;
}

// The types of the same region that cannot be published yet (see
// `jl_deferred_typecache_t`), lent the same way as `typecache_stack`
static jl_deferred_typecache_t *typecache_region = NULL;
// the `list` of every region: one exists at a time, and it ends empty
jl_array_t *jl_typecache_region_list JL_GLOBALLY_ROOTED = NULL;
// the length of `list` from which its datatypes are indexed by hash
#define REGION_INDEX_MIN 32

static jl_deferred_typecache_t *typecache_region_held(void) JL_NOTSAFEPOINT
{
    return jl_mutex_held(&typecache_lock) ? typecache_region : NULL;
}

// What an uncommitted type depends on is a level: 0 for the members of a
// typegroup, `i + 1` for frame `i` of the region. A type is committed (cached,
// or its pending write performed) when the frame of the lowest level it
// depends on completes, and dropped when that frame fails.
#define REGION_NONE INT_MAX
#define region_encode(level) ((void*)(uintptr_t)(2 + (uintptr_t)(level)))
#define region_decode_level(v) ((int)((uintptr_t)(v) - 2))
#define region_entry_level(r, i) ((int)(uintptr_t)(r)->lows.items[i])

#define region_clean(r) ((r)->dirty == REGION_NONE)

// An uncommitted type of `level` was handed out: until that level completes,
// what a new type references decides whether it may be committed. The frames
// at `dirty` and deeper are in `pending`
static void region_dirty(jl_deferred_typecache_t *r, int level) JL_NOTSAFEPOINT
{
    if (level >= r->dirty)
        return;
    size_t i, end = r->frames.len;
    if (region_clean(r)) {
        htable_new(&r->pending, 0);
        htable_new(&r->writes, 0);
    }
    else
        end = r->dirty - 1;
    for (i = level > 0 ? level - 1 : 0; i < end; i++)
        ptrhash_put(&r->pending, r->frames.items[i], region_encode(i + 1));
    r->dirty = level;
}

// The frame at `level` is gone: if it was the shallowest one handed out, what
// depended on it is committed or dropped, and the region is clean again
static void region_pop(jl_deferred_typecache_t *r, int level) JL_NOTSAFEPOINT
{
    if (r->frames.len == 0) // no frame is left to take a write back
        r->written.len = 0;
    if (r->dirty != level || r->lows.len != 0)
        return;
    r->written.len = 0;
    htable_free(&r->pending);
    htable_free(&r->writes);
    r->dirty = REGION_NONE;
}

// The level of the frame of the region building `t`, or 0 if none is
static int region_frame_level(jl_deferred_typecache_t *r, jl_datatype_t *t) JL_NOTSAFEPOINT
{
    for (size_t i = r->frames.len; i > 0; i--)
        if (r->frames.items[i - 1] == (void*)t)
            return i;
    return 0;
}

// the slot of an entry of the index since committed or dropped
#define REGION_INDEX_GONE ((void*)(uintptr_t)1)

// Whether lookups of the key of `dt` go by hash: as in the cache, only a key
// with a stable hash (`typekey_hash`) does, and then it is `dt->hash`
static int region_hashed(jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    return typekey_hash(dt->name, jl_svec_data(dt->parameters), jl_svec_len(dt->parameters), 0) != 0;
}

static void region_index_insert(jl_deferred_typecache_t *r, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    if (!region_hashed(dt))
        return;
    size_t mask = r->indexsz - 1, i = dt->hash & mask;
    while (r->index[i] != NULL && r->index[i] != REGION_INDEX_GONE)
        i = (i + 1) & mask;
    if (r->index[i] == NULL)
        r->nindexed++;
    r->index[i] = dt;
}

static void region_index_remove(jl_deferred_typecache_t *r, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    if (r->index == NULL)
        return;
    size_t mask = r->indexsz - 1;
    for (size_t i = dt->hash & mask; r->index[i] != NULL; i = (i + 1) & mask) {
        if (r->index[i] == dt) {
            r->index[i] = REGION_INDEX_GONE;
            return;
        }
    }
}

// Index the datatypes of the list anew
static void region_reindex(jl_deferred_typecache_t *r) JL_NOTSAFEPOINT
{
    size_t i, len = r->lows.len;
    free(r->index);
    r->indexgen++;
    r->indexsz = next_power_of_two(4 * len);
    r->index = (void**)calloc_s(r->indexsz * sizeof(void*));
    r->nindexed = 0;
    for (i = 0; i < len; i++) {
        jl_datatype_t *e = (jl_datatype_t*)jl_array_ptr_ref(r->list, i);
        if (e != NULL && jl_is_datatype(e))
            region_index_insert(r, e);
    }
}

static void region_unindex(jl_deferred_typecache_t *r) JL_NOTSAFEPOINT
{
    free(r->index);
    r->index = NULL;
    r->indexsz = r->nindexed = 0;
    r->indexgen++;
}

static void region_init(jl_deferred_typecache_t *r) JL_NOTSAFEPOINT
{
    r->list = jl_typecache_region_list;
    r->dirty = REGION_NONE;
    r->index = NULL;
    r->indexsz = r->nindexed = r->indexgen = 0;
    arraylist_new(&r->lows, 0);
    arraylist_new(&r->frames, 0);
    arraylist_new(&r->written, 0);
}

static void region_free(jl_deferred_typecache_t *r) JL_NOTSAFEPOINT
{
    if (!region_clean(r)) {
        htable_free(&r->pending);
        htable_free(&r->writes);
    }
    region_unindex(r);
    arraylist_free(&r->lows);
    arraylist_free(&r->frames);
    arraylist_free(&r->written);
}

// The lowest level among `low` and those of the uncommitted types `t` mentions
static int region_low(jl_deferred_typecache_t *r, jl_value_t *t, int low) JL_NOTSAFEPOINT
{
    if (t == NULL || low == 0)
        return low;
    if (jl_is_datatype(t)) {
        void *v = ptrhash_get(&r->pending, t);
        if (v == HT_NOTFOUND) // an instantiation of a typegroup's member
            v = ptrhash_get(&r->pending, ((jl_datatype_t*)t)->name);
        if (v != HT_NOTFOUND) {
            int level = region_decode_level(v);
            return level < low ? level : low;
        }
        return region_low(r, (jl_value_t*)((jl_datatype_t*)t)->parameters, low);
    }
    if (jl_is_svec(t)) {
        size_t i, l = jl_svec_len(t);
        for (i = 0; i < l; i++)
            low = region_low(r, jl_svecref(t, i), low);
        return low;
    }
    if (jl_is_uniontype(t))
        return region_low(r, ((jl_uniontype_t*)t)->b, region_low(r, ((jl_uniontype_t*)t)->a, low));
    if (jl_is_unionall(t))
        return region_low(r, ((jl_unionall_t*)t)->body, region_low(r, (jl_value_t*)((jl_unionall_t*)t)->var, low));
    if (jl_is_typevar(t))
        return region_low(r, ((jl_tvar_t*)t)->ub, region_low(r, ((jl_tvar_t*)t)->lb, low));
    if (jl_is_vararg(t))
        return region_low(r, ((jl_vararg_t*)t)->N, region_low(r, ((jl_vararg_t*)t)->T, low));
    if (jl_is_typeeq(t) || jl_is_typeegal(t))
        return region_low(r, ((jl_typeeq_t*)t)->T, low);
    return low;
}

// a key for the supertype (`types == 0`) or the field types (`types == 1`) of `t`
#define region_write_key(t, types) ((void*)((char*)(t) + (types)))

// Record `v` (an uncommitted type, or a pending write) as depending on level `low`
static void region_defer(jl_deferred_typecache_t *r, jl_value_t *v, int low) JL_CANSAFEPOINT
{
    assert(!region_clean(r));
    JL_GC_PUSH1(&v);
    if (r->list == NULL)
        r->list = jl_typecache_region_list = jl_alloc_vec_any(0);
    jl_array_ptr_1d_push(r->list, v);
    arraylist_push(&r->lows, (void*)(uintptr_t)low);
    if (jl_is_datatype(v)) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        ptrhash_put(&r->pending, v, region_encode(low));
        if (r->index != NULL ? 2 * (r->nindexed + 1) > r->indexsz : r->lows.len > REGION_INDEX_MIN)
            region_reindex(r);
        else if (r->index != NULL)
            region_index_insert(r, dt);
    }
    else {
        jl_value_t *t = jl_svecref(v, 0);
        ptrhash_put(&r->writes, region_write_key(t, jl_is_svec(jl_svecref(v, 1))), v);
    }
    JL_GC_POP();
}

// Look up a type in the region: a linear scan while it holds few, since it
// holds only the types that depend on one still in progress, else by hash
static jl_value_t *lookup_deferred_type(jl_deferred_typecache_t *r, jl_typename_t *tn, jl_value_t **key, size_t n) JL_CANSAFEPOINT
{
restart:
    if (r == NULL || region_clean(r) || r->lows.len == 0)
        return NULL;
    unsigned hv = r->index == NULL ? 0 : typekey_hash(tn, key, n, 0);
    if (hv != 0) {
        size_t gen = r->indexgen, mask = r->indexsz - 1;
        for (size_t i = hv & mask; r->index[i] != NULL; i = (i + 1) & mask) {
            jl_datatype_t *e = (jl_datatype_t*)r->index[i];
            if (e == REGION_INDEX_GONE || e->hash != hv || e->name != tn)
                continue;
            if (typekey_eq(e, key, n))
                return (jl_value_t*)e;
            if (r->indexgen != gen) // `typekey_eq` added entries, and rebuilt it
                goto restart;
        }
        return NULL;
    }
    // `typekey_eq` may add entries; the ones scanned stay put meanwhile
    size_t gen = r->indexgen;
    for (size_t j = 0; j < r->lows.len; j++) {
        jl_datatype_t *e = (jl_datatype_t*)jl_array_ptr_ref(r->list, j);
        if (e != NULL && jl_is_datatype(e) && e->name == tn && jl_svec_len(e->parameters) == n &&
                (r->index == NULL || !region_hashed(e)) && typekey_eq(e, key, n))
            return (jl_value_t*)e;
        if (r->indexgen != gen) // it built the index, which the rest must be looked up in
            goto restart;
    }
    return NULL;
}

// The supertype (`types == 0`) or field types (`types == 1`) of `t` computed
// lazily in the region, waiting there for a type they depend on
static jl_value_t *region_pending_write(jl_deferred_typecache_t *r, jl_datatype_t *t, int types) JL_NOTSAFEPOINT
{
    if (r == NULL || region_clean(r))
        return NULL;
    jl_value_t *w = (jl_value_t*)ptrhash_get(&r->writes, region_write_key(t, types));
    return w == HT_NOTFOUND ? NULL : jl_svecref(w, 1);
}

// Whether the lazily computed supertype or field types `v` of `t` must wait in
// the region instead of being published now: they mention a type that may
// still fail. A frame of the region checks the fields of its own type when it
// completes, so those are written directly, and taken back if a frame nested
// in it fails meanwhile (`region_undo`)
static int region_hold_write(jl_deferred_typecache_t *r, jl_datatype_t *t, jl_value_t *v) JL_CANSAFEPOINT
{
    if (r == NULL || region_clean(r))
        return 0;
    if (region_frame_level(r, t)) {
        arraylist_push(&r->written, region_write_key(t, jl_is_svec(v)));
        return 0;
    }
    int low = region_low(r, v, REGION_NONE);
    if (low == REGION_NONE)
        return 0;
    jl_value_t *w = NULL;
    JL_GC_PUSH2(&v, &w);
    w = (jl_value_t*)jl_svec2(t, v);
    region_defer(r, w, low);
    JL_GC_POP();
    return 1;
}

// Take back the writes to the fields of the frames logged since `wmark`: a
// frame that failed meanwhile may have built what they mention
static void region_undo(jl_deferred_typecache_t *r, size_t wmark) JL_NOTSAFEPOINT
{
    for (size_t i = wmark; i < r->written.len; i++) {
        uintptr_t key = (uintptr_t)r->written.items[i];
        jl_datatype_t *t = (jl_datatype_t*)(key & ~(uintptr_t)1);
        if (!region_frame_level(r, t))
            continue;
        if (key & 1)
            jl_atomic_store_relaxed((_Atomic(jl_svec_t*)*)&t->types, NULL);
        else
            jl_atomic_store_relaxed((_Atomic(jl_datatype_t*)*)&t->super, NULL);
    }
    r->written.len = wmark;
}

// Remove the entry `e` of the list from the lookups of the region
static void region_forget(jl_deferred_typecache_t *r, jl_value_t *e) JL_NOTSAFEPOINT
{
    if (jl_is_datatype(e)) {
        ptrhash_remove(&r->pending, e);
        region_index_remove(r, (jl_datatype_t*)e);
    }
    else {
        void *key = region_write_key(jl_svecref(e, 0), jl_is_svec(jl_svecref(e, 1)));
        assert(ptrhash_get(&r->writes, key) == (void*)e); // one pending write per slot
        ptrhash_remove(&r->writes, key);
    }
}

static void region_publish(jl_value_t *e) JL_CANSAFEPOINT
{
    if (jl_is_svec(e)) {
        jl_datatype_t *t = (jl_datatype_t*)jl_svecref(e, 0);
        jl_value_t *v = jl_svecref(e, 1);
        if (jl_is_svec(v)) {
            jl_publish_fieldtypes(t, (jl_svec_t*)v);
        }
        else {
            jl_datatype_t *unset = NULL;
            jl_gc_wb(t, (void*)&t->super, v);
            jl_atomic_cmpswap((_Atomic(jl_datatype_t*)*)&t->super, &unset, (jl_datatype_t*)v);
        }
    }
    else {
        // the region owns every type with this key: lookups find it there
        jl_datatype_t *t = (jl_datatype_t*)e;
        if (t->layout == NULL && t->types != NULL && t->isconcretetype)
            jl_compute_field_offsets(t);
        jl_cache_type_(t);
    }
}

// Remove the entries from `mark` on at `level` or above (all of them for 0:
// their frame failed), publishing them first if `publish`, so that a lookup
// nested in the publication still finds each in the region or in the cache.
// What the publication adds at that level depends on them: it goes with them
static void region_remove(jl_deferred_typecache_t *r, size_t mark, int level, int publish) JL_CANSAFEPOINT
{
    if (region_clean(r) || r->lows.len == mark)
        return;
    size_t i, j;
    if (publish) {
        for (i = mark; i < r->lows.len; i++) {
            jl_value_t *e = jl_array_ptr_ref(r->list, i);
            if (e != NULL && region_entry_level(r, i) >= level)
                region_publish(e);
        }
    }
    for (i = j = mark; i < r->lows.len; i++) {
        jl_value_t *e = jl_array_ptr_ref(r->list, i);
        if (region_entry_level(r, i) >= level) {
            if (e != NULL)
                region_forget(r, e);
            continue;
        }
        jl_array_ptr_set(r->list, j, e);
        r->lows.items[j] = r->lows.items[i];
        j++;
    }
    jl_array_del_end(r->list, r->lows.len - j);
    r->lows.len = j;
    if (j == 0)
        region_unindex(r);
}

// Compute the layouts of the entries from `mark` on at `level` or above before
// any is published: if one fails, they all go
static void region_layouts(jl_deferred_typecache_t *r, size_t mark, int level) JL_CANSAFEPOINT
{
    for (size_t i = mark; i < r->lows.len; i++) {
        jl_datatype_t *t = (jl_datatype_t*)jl_array_ptr_ref(r->list, i);
        if (t != NULL && jl_is_datatype(t) && region_entry_level(r, i) >= level &&
                t->layout == NULL && t->types != NULL && t->isconcretetype)
            jl_compute_field_offsets(t);
    }
}

// `ndt`, the type of the region's frame at `level` whose entries start at
// `mark`, is complete: cache it, with the entries that depend only on it, if it
// depends on no type of a lower level; else it waits in the region with them
static void region_finish(jl_deferred_typecache_t *r, jl_datatype_t *ndt, int level, size_t mark, int defer) JL_CANSAFEPOINT
{
    // no frame shallower than `dirty` was handed out: the one at it is a root
    int low = REGION_NONE;
    if (r->dirty < level) {
        low = region_low(r, (jl_value_t*)ndt->parameters, low);
        low = region_low(r, (jl_value_t*)jl_atomic_load_relaxed((_Atomic(jl_datatype_t*)*)&ndt->super), low);
        low = region_low(r, (jl_value_t*)ndt->types, low);
    }
    // for a type that depends on one still in progress (or on a typegroup),
    // the layout is computed at publication, once that one has its own: now
    // it would bake in wrong assumptions (cf. #40050)
    if (!defer && low >= level && ndt->layout == NULL && ndt->types != NULL && ndt->isconcretetype)
        jl_compute_field_offsets(ndt);
    if (low >= level) {
        region_layouts(r, mark, level);
        jl_cache_type_(ndt);
        region_remove(r, mark, level, 1);
    }
    else {
        // the entries at its level may mention it
        for (size_t i = mark; i < r->lows.len; i++) {
            if (region_entry_level(r, i) >= level) {
                r->lows.items[i] = (void*)(uintptr_t)low;
                jl_value_t *e = jl_array_ptr_ref(r->list, i);
                if (jl_is_datatype(e))
                    ptrhash_put(&r->pending, e, region_encode(low));
            }
        }
        region_defer(r, (jl_value_t*)ndt, low);
    }
}

void jl_typecache_region_begin(jl_deferred_typecache_t *r)
{
    JL_LOCK(&typecache_lock); // Might GC
    assert(typecache_region == NULL);
    region_init(r);
    region_dirty(r, 0);
    typecache_region = r;
}

void jl_typegroup_region_member(jl_deferred_typecache_t *r, jl_datatype_t *dt)
{
    ptrhash_put(&r->pending, dt, region_encode(0));
    ptrhash_put(&r->pending, dt->name, region_encode(0));
}

// Drop entry `i` of a region of level 0 before its publication
void jl_typegroup_region_drop(jl_deferred_typecache_t *r, size_t i)
{
    assert(typecache_region == r && r->dirty == 0);
    jl_value_t *e = jl_array_ptr_ref(r->list, i);
    if (e != NULL) {
        region_forget(r, e);
        jl_array_ptr_set(r->list, i, NULL);
    }
}

// Compute the layouts of the types a region of level 0 (a typegroup's, or the
// image loader's) waits to publish. They may fail, so this precedes
// `jl_typecache_region_end` (and can go before what must not)
void jl_typecache_region_layouts(jl_deferred_typecache_t *r)
{
    assert(typecache_region == r && r->frames.len == 0);
    region_layouts(r, 0, 0);
}

// Publish the entries of a region of level 0 that are not NULL, if `publish`
// (after `jl_typecache_region_layouts`), and close it
void jl_typecache_region_end(jl_deferred_typecache_t *r, int publish)
{
    assert(typecache_region == r && r->frames.len == 0);
    JL_TRY {
        for (size_t i = 0; publish && i < r->lows.len; i++) {
            jl_value_t *e = jl_array_ptr_ref(r->list, i);
            if (e != NULL)
                region_publish(e);
        }
    }
    JL_CATCH {
        jl_typecache_region_end(r, 0);
        jl_rethrow();
    }
    if (r->lows.len != 0)
        jl_array_del_end(r->list, r->lows.len);
    typecache_region = NULL;
    region_free(r);
    JL_UNLOCK(&typecache_lock); // Might GC
}

// For the image loader, inside the region it holds (`jl_typecache_region_begin`):
// return the type with the key of the image object `dt` (whose name and parameters
// are already unique), cached or waiting in the region, else a fresh copy of `dt`,
// reported through `isnew`, which waits in the region at level 0 until the loader
// publishes it, with whatever is built meanwhile that mentions it. The copy gets
// the `instance` of `dt` (allocated here once the copy is new, if the image left
// `jl_nothing`: none until then, so the sentinel is never seen). It
// carries no `types` except for tuples (the caller sets them once the image is
// relocated); `super` is unset in the image until the loader publishes the stored
// one, or is computed lazily before that. The lookup compares keys by subtyping,
// which may compute a lazy supertype that instantiates this very key: the copy
// stands for it on the instantiation stack meanwhile, as a type of level 0, so
// that instantiation adopts the copy instead of creating the type
jl_datatype_t *jl_lookup_or_defer_type_(jl_datatype_t *dt, int *isnew)
{
    jl_deferred_typecache_t *r = typecache_region_held();
    assert(r != NULL && r->dirty == 0);
    jl_datatype_t *newdt = jl_new_uninitialized_datatype();
    JL_GC_PUSH1(&newdt);
    jl_astaggedvalue(newdt)->bits.gc = GC_OLD;
    *newdt = *dt;
    if (dt->instance == jl_nothing)
        newdt->instance = NULL;
    if (dt->name != jl_tuple_typename && dt->name != jl_namedtuple_typename)
        newdt->types = NULL;
    jl_svec_t *p = newdt->parameters;
    ptrhash_put(&r->pending, newdt, region_encode(0));
    jl_typestack_t *saved = typecache_stack;
    jl_typestack_t top = { newdt, saved, 0 };
    typecache_stack = &top;
    jl_datatype_t *lkup = NULL;
    JL_TRY {
        lkup = jl_lookup_cache_type_(newdt);
        if (lkup == NULL)
            lkup = (jl_datatype_t*)lookup_deferred_type(r, newdt->name, jl_svec_data(p), jl_svec_len(p));
        if (lkup == NULL) {
            region_defer(r, (jl_value_t*)newdt, 0);
            lkup = newdt;
        }
    }
    JL_CATCH {
        typecache_stack = saved;
        ptrhash_remove(&r->pending, newdt);
        jl_rethrow();
    }
    typecache_stack = saved;
    *isnew = lkup == newdt;
    if (*isnew && dt->instance == jl_nothing)
        newdt->instance = dt->instance = jl_gc_permobj(jl_current_task->ptls, 0, newdt, 0);
    if (!*isnew) // nothing adopted it: the lookups that missed this key found it nowhere either
        ptrhash_remove(&r->pending, newdt);
    JL_GC_POP();
    return lkup;
}

static jl_value_t *inst_type_w_(jl_value_t *t, jl_typeenv_t *env, jl_typestack_t *stack, int check, int nothrow) JL_CANSAFEPOINT;
static jl_vararg_t *wrap_vararg_(jl_value_t *t, jl_value_t *n, int check, int nothrow,
                                 jl_typestack_t *stack) JL_CANSAFEPOINT;
static jl_svec_t *inst_ftypes(jl_svec_t *p, jl_typeenv_t *env, jl_typestack_t *stack, int cacheable) JL_CANSAFEPOINT;

static jl_value_t *instantiate_unionall_(jl_unionall_t *u, jl_value_t *p)
{
    jl_typeenv_t env = { u->var, p, NULL };
    return inst_type_w_(u->body, &env, NULL, 1, 0);
}

JL_DLLEXPORT jl_value_t *jl_instantiate_unionall(jl_unionall_t *u, jl_value_t *p)
{
    return instantiate_unionall_(u, p);
}

jl_unionall_t *jl_rename_unionall(jl_unionall_t *u)
{
    jl_tvar_t *v = jl_new_typevar(u->var->name, u->var->lb, u->var->ub);
    jl_value_t *t = NULL;
    JL_GC_PUSH2(&v, &t);
    jl_typeenv_t env = { u->var, (jl_value_t *)v, NULL };
    t = inst_type_w_(u->body, &env, NULL, 0, 0);
    t = jl_new_struct(jl_unionall_type, v, t);
    JL_GC_POP();
    return (jl_unionall_t*)t;
}

static jl_value_t *substitute_var_(jl_value_t *t, jl_tvar_t *var, jl_value_t *val, int nothrow,
                                   jl_typestack_t *stack) JL_CANSAFEPOINT
{
    if (val == (jl_value_t*)var)
        return t;
    nothrow = jl_is_typevar(val) ? 0 : nothrow;
    jl_typeenv_t env = { var, val, NULL };
    return inst_type_w_(t, &env, stack, 1, nothrow);
}

jl_value_t *jl_substitute_var_nothrow(jl_value_t *t, jl_tvar_t *var, jl_value_t *val, int nothrow)
{
    return substitute_var_(t, var, val, nothrow, NULL);
}

jl_value_t *jl_substitute_var(jl_value_t *t, jl_tvar_t *var, jl_value_t *val)
{
    if (val == (jl_value_t*)var)
        return t;
    jl_typeenv_t env = { var, val, NULL };
    return inst_type_w_(t, &env, NULL, 1, 0);
}

jl_value_t *jl_unwrap_unionall(jl_value_t *v)
{
    while (jl_is_unionall(v))
        v = ((jl_unionall_t*)v)->body;
    return v;
}

// wrap `t` in the same unionalls that surround `u`
// where `t` is derived from `u`, so the error checks in jl_type_unionall are unnecessary
jl_value_t *jl_rewrap_unionall(jl_value_t *t, jl_value_t *u)
{
    if (!jl_is_unionall(u))
        return t;
    t = jl_rewrap_unionall(t, ((jl_unionall_t*)u)->body);
    jl_tvar_t *v = ((jl_unionall_t*)u)->var;
    // normalize `T where T<:S` => S
    if (t == (jl_value_t*)v)
        return v->ub;
    // where var doesn't occur in body just return body
    if (!jl_has_typevar(t, v))
        return t;
    JL_GC_PUSH1(&t);
    //if (v->lb == v->ub)  // TODO maybe
    //    t = jl_substitute_var(body, v, v->ub);
    //else
    t = jl_new_struct(jl_unionall_type, v, t);
    JL_GC_POP();
    return t;
}

// wrap `t` in the same unionalls that surround `u`
// where `t` is extended from `u`, so the checks in jl_rewrap_unionall are unnecessary
jl_value_t *jl_rewrap_unionall_(jl_value_t *t, jl_value_t *u)
{
    if (!jl_is_unionall(u))
        return t;
    t = jl_rewrap_unionall_(t, ((jl_unionall_t*)u)->body);
    JL_GC_PUSH1(&t);
    t = jl_new_struct(jl_unionall_type, ((jl_unionall_t*)u)->var, t);
    JL_GC_POP();
    return t;
}

// Create a copy of type expression t where any occurrence of data type x is replaced by y.
// If x does not occur in t, return t without any copy.
// For example, jl_substitute_datatype(Foo{Bar}, Foo{T}, Qux{S}) is Qux{Bar}, with T and S
// free type variables.
// To substitute type variables, use jl_substitute_var instead.
jl_value_t *jl_substitute_datatype(jl_value_t *t, jl_datatype_t * x, jl_datatype_t * y)
{
    if jl_is_datatype(t) {
        jl_datatype_t *typ = (jl_datatype_t*)t;
        // For datatypes call itself recursively on the parameters to form new parameters.
        // Then, if typename(t) == typename(x), rewrap the wrapper of y around the new
        // parameters. Otherwise, do the same around the wrapper of t.
        // This ensures that the types and supertype are properly set.
        // Start by check whether there is a parameter that needs replacing.
        long i_firstnewparam = -1;
        size_t nparams = jl_svec_len(typ->parameters);
        jl_value_t *firstnewparam = NULL;
        JL_GC_PUSH1(&firstnewparam);
        for (size_t i = 0; i < nparams; i++) {
            jl_value_t *param = NULL;
            JL_GC_PUSH1(&param);
            param = jl_svecref(typ->parameters, i);
            firstnewparam = jl_substitute_datatype(param, x, y);
            if (param != firstnewparam) {
                i_firstnewparam = i;
                JL_GC_POP();
                break;
            }
            JL_GC_POP();
        }
        // If one of the parameters needs to be updated, or if the type name is that to
        // substitute, create a new datataype
        if (i_firstnewparam != -1 || typ->name == x->name) {
            jl_datatype_t *uw = typ->name == x->name ? y : typ; // substitution occurs here
            jl_value_t *wrapper = uw->name->wrapper;
            jl_datatype_t *w = (jl_datatype_t*)jl_unwrap_unionall(wrapper);
            jl_svec_t *sv = jl_alloc_svec(jl_svec_len(uw->parameters));
            JL_GC_PUSH1(&sv);
            jl_value_t **vals = jl_svec_data(sv);
            // no JL_GC_PUSHARGS(vals, ...) since GC is already aware of sv
            for (long i = 0; i < i_firstnewparam; i++) { // copy the identical parameters
                vals[i] = jl_svecref(typ->parameters, i); // value
            }
            if (i_firstnewparam != -1) { // insert the first non-identical parameter
                vals[i_firstnewparam] = firstnewparam;
            }
            for (size_t i = i_firstnewparam+1; i < nparams; i++) { // insert the remaining parameters
                jl_svecset(sv, i, jl_substitute_datatype(jl_svecref(typ->parameters, i), x, y));
            }
            if (jl_is_tuple_type(wrapper)) {
                // special case for tuples, since the wrapper (Tuple) does not have as
                // many parameters as t (it only has a Vararg instead).
                t = jl_apply_tuple_type(sv, 0);
            } else {
                t = jl_instantiate_type_in_env((jl_value_t*)w, (jl_unionall_t*)wrapper, vals);
            }
            JL_GC_POP();
        }
        JL_GC_POP();
    }
    else if jl_is_unionall(t) { // recursively call itself on body and var bounds
        jl_unionall_t* ut = (jl_unionall_t*)t;
        jl_value_t *lb = NULL;
        jl_value_t *ub = NULL;
        jl_value_t *body = NULL;
        JL_GC_PUSH3(&lb, &ub, &body);
        lb = jl_substitute_datatype(ut->var->lb, x, y);
        ub = jl_substitute_datatype(ut->var->ub, x, y);
        body = jl_substitute_datatype(ut->body, x, y);
        if (lb != ut->var->lb || ub != ut->var->ub) {
            jl_tvar_t *newtvar = jl_new_typevar(ut->var->name, lb, ub);
            JL_GC_PUSH1(&newtvar);
            body = jl_substitute_var(body, ut->var, (jl_value_t*)newtvar);
            t = jl_new_struct(jl_unionall_type, newtvar, body);
            JL_GC_POP();
        }
        else if (body != ut->body) {
            t = jl_new_struct(jl_unionall_type, ut->var, body);
        }
        JL_GC_POP();
    }
    else if jl_is_uniontype(t) { // recursively call itself on a and b
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        jl_value_t *a = NULL;
        jl_value_t *b = NULL;
        JL_GC_PUSH2(&a, &b);
        a = jl_substitute_datatype(u->a, x, y);
        b = jl_substitute_datatype(u->b, x, y);
        if (a != u->a || b != u->b) {
            t = jl_new_struct(jl_uniontype_type, a, b);
        }
        JL_GC_POP();
    }
    else if (jl_is_some_Type(t)) {
        int egal = jl_is_typeegal(t);
        jl_typeeq_t *te = (jl_typeeq_t*)t;
        jl_value_t *T = NULL;
        JL_GC_PUSH1(&T);
        T = jl_substitute_datatype(te->T, x, y);
        if (T != te->T)
            t = egal ? jl_wrap_TypeEgal(T) : (jl_value_t*)jl_wrap_Type(T);
        JL_GC_POP();
    }
    else if jl_is_vararg(t) { // recursively call itself on T
        jl_vararg_t *vt = (jl_vararg_t*)t;
        if (vt->T) { // vt->T could be NULL
            jl_value_t *rT = NULL;
            JL_GC_PUSH1(&rT);
            rT = jl_substitute_datatype(vt->T, x, y);
            if (rT != vt->T) {
                jl_task_t *ct = jl_current_task;
                t = jl_gc_alloc(ct->ptls, sizeof(jl_vararg_t), jl_vararg_type);
                jl_set_typetagof((jl_vararg_t *)t, jl_vararg_tag, 0);
                ((jl_vararg_t *)t)->T = rT;
                ((jl_vararg_t *)t)->N = vt->N;
            }
            JL_GC_POP();
        }
    }
    return t;
}

static jl_value_t *lookup_type_stack(jl_typestack_t *stack, jl_datatype_t *tt, size_t ntp,
                                     jl_value_t **iparams) JL_CANSAFEPOINT
{
    // if an identical instantiation is already in process somewhere up the
    // stack, return it. this computes a fixed point for recursive types.
    jl_typename_t *tn = tt->name;
    while (stack != NULL) {
        JL_GC_PROMISE_ROOTED(stack->tt);
        if (stack->tt->name == tn &&
            ntp == jl_svec_len(stack->tt->parameters) &&
            typekey_eq(stack->tt, iparams, ntp)) {
            // an unfinished type handed out: what mentions it must wait for it
            // (a type that is not cacheable has no frame and is not in
            // `pending`: `region_low` looks through it to its parameters)
            jl_deferred_typecache_t *region = typecache_region_held();
            if (region != NULL) {
                int level = region_frame_level(region, stack->tt);
                region_dirty(region, level ? level : region->frames.len);
            }
            return (jl_value_t*)stack->tt;
        }
        stack = stack->prev;
    }
    return NULL;
}

static unsigned typeeq_hash(jl_value_t *T, int nofail) JL_NOTSAFEPOINT;
static unsigned typeegal_hash(jl_value_t *T) JL_NOTSAFEPOINT;

// stable numbering for types--starts with name->hash, then falls back to objectid
// returns 0 if (and only if) the hash value isn't stable, unless nofail is set
static unsigned type_hash(jl_value_t *kj, int nofail) JL_NOTSAFEPOINT
{
    jl_value_t *uw = jl_is_unionall(kj) ? jl_unwrap_unionall(kj) : kj;
    if (jl_is_datatype(uw)) {
        jl_datatype_t *dt = (jl_datatype_t*)uw;
        unsigned hash = dt->hash;
        if (!hash) {
            // A typename wrapper whose body has no stable hash (e.g. because its typevars'
            // bounds contain a Union, such as `Broadcasted`) can still be hashed
            // structurally, since everything `==` to the wrapper is normalized to the
            // wrapper itself at construction.
            if (!nofail && dt->name->wrapper != kj)
                return 0;
            // compute a hash now, only for the parent object we are putting in the cache
            hash = typekey_hash(dt->name, jl_svec_data(dt->parameters), jl_svec_len(dt->parameters), 1);
        }
        return hash;
    }
    else if (jl_is_typevar(uw)) {
        // ignore var and lb, since those might get normalized out in equality testing
        return type_hash(((jl_tvar_t*)uw)->ub, nofail);
    }
    else if (jl_is_uniontype(uw)) {
        if (!nofail)
            return 0;
        // compute a hash now, only for the parent object we are putting in the cache
        unsigned hasha = type_hash(((jl_uniontype_t*)uw)->a, 1);
        unsigned hashb = type_hash(((jl_uniontype_t*)uw)->b, 1);
        // use an associative mixing function, with well-defined overflow
        // since Union is associative
        unsigned hash = hasha + hashb;
        return hash ? hash : 1;
    }
    else if (jl_is_typeeq(uw)) {
        if (jl_is_typeofbottom_param(uw))
            return jl_typeofbottom_type->hash;
        return typeeq_hash(jl_typeeq_T(uw), nofail);
    }
    else if (jl_is_typeegal(uw)) {
        return typeegal_hash(jl_typeegal_T(uw));
    }
    else {
        unsigned hash = jl_object_id(uw);
        return hash ? hash : 1;
    }
}

// return true if T isa TypeVar with no constraints (Union{}<:T<:Any), traced recursively
static int typevar_admits_all(jl_value_t *T) JL_NOTSAFEPOINT
{
    if (!jl_is_typevar(T))
        return 0;
    while (jl_is_typevar(T) && ((jl_tvar_t*)T)->lb == jl_bottom_type)
        T = ((jl_tvar_t*)T)->ub;
    return T == (jl_value_t*)jl_any_type;
}

// hash of `TypeEq{T}`, similar to a normal datatype, with three exceptions
static unsigned typeeq_hash(jl_value_t *T, int nofail) JL_NOTSAFEPOINT
{
    // `TypeEq{Union{}}` is hashed as `typeof(Union{})` by the callers, not here
    assert(T != jl_bottom_type);
    // `TypeEq{T} where T == AnyType`
    if (typevar_admits_all(T))
        return type_hash((jl_value_t*)jl_anytype_type, nofail);

    // `TypeEq{TypeEq{T}} == TypeEq` (for a free typevar `T`)
    // There is a special case TODO in subtype.c to remove this case.
    if (jl_is_typeeq(T) && typevar_admits_all(jl_typeeq_T(T)))
        return type_hash((jl_value_t*)jl_typeeq_type, nofail);
    return typekey_hash(jl_type_typename, &T, 1, nofail);
}

// hash of `TypeEgal{T}`. Its keys compare by egality, so it never fails (returns 0) for a non-structural hash.
static unsigned typeegal_hash(jl_value_t *T) JL_NOTSAFEPOINT
{
    unsigned hash = bitmix(jl_typeegal_type->name->hash, type_hash(T, 1));
    return hash ? hash : 1;
}

// the hash of `v` (may be 0 if its `==` class has no stable hash defined)
JL_DLLEXPORT uintptr_t jl_type_hash(jl_value_t *v) JL_NOTSAFEPOINT
{
    return type_hash(v, 0);
}

static unsigned typekey_hash(jl_typename_t *tn, jl_value_t **key, size_t n, int nofail) JL_NOTSAFEPOINT
{
    assert(tn != jl_type_typename || n == 0 || key[0] != jl_bottom_type); // `TypeEq{Union{}}` gets returned directly by `jl_wrap_Type`, so the hash isn't relevant to compute here
    size_t j;
    unsigned hash = 3;
    for (j = 0; j < n; j++) {
        jl_value_t *p = key[j];
        size_t repeats = 1;
        if (jl_is_vararg(p)) {
            jl_vararg_t *vm = (jl_vararg_t*)p;
            if (vm->N && jl_is_long(vm->N))
                repeats = jl_unbox_long(vm->N);
            else
                hash = bitmix(0x064eeaab, hash); // 0x064eeaab is just a randomly chosen constant
            p = vm->T ? vm->T : (jl_value_t*)jl_any_type;
        }
        unsigned hashp = type_hash(p, nofail);
        if (!hashp)
            return 0;
        while (repeats--)
            hash = bitmix(hashp, hash);
    }
    hash = bitmix(~tn->hash, hash);
    return hash ? hash : 1;
}

static unsigned typekeyvalue_hash(jl_typename_t *tn, jl_value_t *key1, jl_value_t **key, size_t n, int leaf) JL_NOTSAFEPOINT
{
    size_t j;
    unsigned hash = 3;
    for (j = 0; j < n; j++) {
        jl_value_t *kj = j == 0 ? key1 : key[j - 1];
        uint_t hj;
        if (leaf && jl_is_kind(jl_typeof(kj))) {
            // hash to match whichever key `jl_inst_arg_tuple_type` stores
            // (`Union{}` is keyed as `typeof(Union{})`, see `jl_wrap_TypeEgal`)
            if (kj == jl_bottom_type)
                hj = jl_typeofbottom_type->hash;
            else if (jl_has_free_typevars(kj))
                hj = typeeq_hash(kj, 0);
            else
                hj = typeegal_hash(kj);
            if (!hj)
                return 0;
        }
        else {
            hj = ((jl_datatype_t*)jl_typeof(kj))->hash;
        }
        hash = bitmix(hj, hash);
    }
    hash = bitmix(~tn->hash, hash);
    return hash ? hash : 1;
}

void jl_precompute_memoized_dt(jl_datatype_t *dt, int cacheable)
{
    int istuple = (dt->name == jl_tuple_typename);
    dt->hasfreetypevars = 0;
    dt->maybe_subtype_of_cache = 1;
    dt->isconcretetype = !dt->name->abstract;
    dt->isdispatchtuple = istuple;
    size_t i, l = jl_nparams(dt);
    for (i = 0; i < l; i++) {
        jl_value_t *p = jl_tparam(dt, i);
        if (!dt->hasfreetypevars) {
            dt->hasfreetypevars = jl_has_free_typevars(p);
            if (dt->hasfreetypevars)
                dt->isconcretetype = 0;
        }
        if (istuple) {
            if (dt->isconcretetype)
                dt->isconcretetype = (jl_is_datatype(p) && ((jl_datatype_t*)p)->isconcretetype) ||
                    p == jl_bottom_type;
            if (dt->isdispatchtuple) {
                // a type-valued slot is a dispatch slot only in the egality form
                // `TypeEgal{T}` that `jl_inst_arg_tuple_type` produces; an equality
                // `Type{T}` slot also admits `==`-equal but non-egal arguments --
                // except `Type{Union{}}`, whose sole instance is the unique bottom
                // object (and which aliases the dispatch slot `typeof(Union{})`)
                dt->isdispatchtuple =
                    (jl_is_datatype(p) && ((!jl_is_kind(p) && ((jl_datatype_t*)p)->isconcretetype) ||
                     (p == (jl_value_t*)jl_typeofbottom_type))) ||
                    jl_is_typeegal(p) ||
                    (jl_is_typeeq(p) && jl_typeeq_T(p) == jl_bottom_type);
            }
        }
        if (jl_is_vararg(p))
            p = ((jl_vararg_t*)p)->T;
        if (istuple && dt->has_concrete_subtype) {
            // tuple types like Tuple{:x} and Tuple{Union{}} cannot have instances
            if (p && !jl_is_type(p) && !jl_is_typevar(p))
                dt->has_concrete_subtype = 0;
            if (p == jl_bottom_type)
                dt->has_concrete_subtype = 0;
        }
        if (dt->maybe_subtype_of_cache) {
            dt->maybe_subtype_of_cache = !p || maybe_subtype_of_cache(p, istuple) || !jl_has_free_typevars(p);
        }
    }
    assert(dt->isconcretetype || dt->isdispatchtuple ? dt->maybe_subtype_of_cache : 1);
    dt->hash = typekey_hash(dt->name, jl_svec_data(dt->parameters), l, cacheable);
}

static int check_datatype_parameters(jl_typename_t *tn, jl_value_t **params, size_t np, int nothrow,
                                     jl_typestack_t *stack) JL_CANSAFEPOINT
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
    for (i = 0; i < np; i++) {
        assert(jl_is_unionall(wrapper));
        jl_tvar_t *tv = ((jl_unionall_t*)wrapper)->var;
        if (!within_typevar(params[i], bounds[2*i], bounds[2*i+1])) {
            if (nothrow) {
                JL_GC_POP();
                return 1;
            }
            if (tv->lb != bounds[2*i] || tv->ub != bounds[2*i+1])
                // pass a new version of `tv` containing the instantiated bounds
                tv = jl_new_typevar(tv->name, bounds[2*i], bounds[2*i+1]);
            JL_GC_PUSH1(&tv);
            jl_type_error_rt(jl_symbol_name(tn->name), jl_symbol_name(tv->name), (jl_value_t*)tv, params[i]);
        }
        int j;
        for (j = 2*i + 2; j < 2*np; j++) {
            jl_value_t *bj = bounds[j];
            if (bj != (jl_value_t*)jl_any_type && bj != jl_bottom_type) {
                int isub = j & 1;
                // use different nothrow level for lb and ub substitution.
                // TODO: This assuming the top instantiation could only start with
                // `nothrow == 2` or `nothrow == 0`. If `nothrow` is initially set to 1
                // then we might miss some inner error, perhaps the normal path should
                // also follow this rule？
                jl_value_t *nb = substitute_var_(bj, tv, params[i], nothrow ? (isub ? 2 : 1) : 0, stack);
                if (nb == NULL) {
                    assert(nothrow);
                    JL_GC_POP();
                    return 1;
                }
                bounds[j] = nb;
            }
        }
        wrapper = ((jl_unionall_t*)wrapper)->body;
    }
    JL_GC_POP();
    return 0;
}

static jl_value_t *extract_wrapper(jl_value_t *t JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT JL_GLOBALLY_ROOTED
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

static int _may_substitute_ub(jl_value_t *v, jl_tvar_t *var, int inside_inv, int *cov_count) JL_NOTSAFEPOINT
{
    while (1) {
        if (v == (jl_value_t*)var) {
            if (inside_inv) {
                return 0;
            }
            else {
                (*cov_count)++;
                return *cov_count <= 1 || jl_is_concrete_type(var->ub);
            }
        }
        while (jl_is_unionall(v)) {
            jl_unionall_t *ua = (jl_unionall_t*)v;
            if (ua->var == var)
                return 1;
            if (ua->var->lb != jl_bottom_type && !_may_substitute_ub(ua->var->lb, var, inside_inv, cov_count))
                return 0;
            if (ua->var->ub != (jl_value_t*)jl_any_type && !_may_substitute_ub(ua->var->ub, var, inside_inv, cov_count))
                return 0;
            v = ua->body;
        }
        if (jl_is_datatype(v)) {
            int invar = inside_inv || !jl_is_tuple_type(v);
            for (size_t i = 0; i < jl_nparams(v); i++) {
                jl_value_t *p = jl_tparam(v, i);
                if (!_may_substitute_ub(p, var, invar, cov_count))
                    return 0;
            }
            return 1;
        }
        else if (jl_is_uniontype(v)) {
            // TODO: is !inside_inv, these don't have to share the changes to cov_count
            if (!_may_substitute_ub(((jl_uniontype_t*)v)->a, var, inside_inv, cov_count))
                return 0;
            v = ((jl_uniontype_t*)v)->b;
        }
        else if (jl_is_vararg(v)) {
            jl_vararg_t *va = (jl_vararg_t*)v;
            if (!va->T)
                return 1;
            if (va->N) {
                if (!_may_substitute_ub(va->N, var, 1, cov_count))
                    return 0;
            }
            if (!jl_is_concrete_type(var->ub))
                inside_inv = 1; // treat as invariant inside vararg, for the sake of this algorithm
            v = va->T;
        }
        else if (jl_is_some_Type(v)) {
            inside_inv = 1;
            v = jl_some_Type_T(v);
        }
        else {
            return 1;
        }
    }
}

// Check whether `var` may be replaced with its upper bound `ub` in `v where var<:ub`
// Conditions:
//  * `var` does not appear in invariant position
//  * `var` appears at most once (in covariant position) and not in a `Vararg`
//    unless the upper bound is concrete (diagonal rule)
static int may_substitute_ub(jl_value_t *v, jl_tvar_t *var) JL_NOTSAFEPOINT
{
    int cov_count = 0;
    return _may_substitute_ub(v, var, 0, &cov_count);
}

static jl_value_t *normalize_unionalls(jl_value_t *t, jl_typestack_t *stack) JL_CANSAFEPOINT
{
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        jl_value_t *a = NULL;
        jl_value_t *b = NULL;
        JL_GC_PUSH2(&a, &b);
        a = normalize_unionalls(u->a, stack);
        b = normalize_unionalls(u->b, stack);
        if (a != u->a || b != u->b) {
            t = jl_new_struct(jl_uniontype_type, a, b);
        }
        JL_GC_POP();
    }
    else if (jl_is_typeeq(t)) {
        jl_typeeq_t *te = (jl_typeeq_t*)t;
        jl_value_t *T = normalize_unionalls(te->T, stack);
        JL_GC_PUSH1(&T);
        if (T != te->T && jl_typeof(T) == jl_typeof(te->T))
            t = (jl_value_t*)jl_wrap_Type(T);
        JL_GC_POP();
    }
    else if (jl_is_typeegal(t)) {
        // `TypeEgal{T}` pins `T` by object identity: substituting a normalized
        // (`==`-equal but non-egal) rep would change which object the kind
        // claims its instance is
    }
    else if (jl_is_unionall(t)) {
        jl_unionall_t *u = (jl_unionall_t*)t;
        jl_value_t *body = normalize_unionalls(u->body, stack);
        JL_GC_PUSH2(&body, &t);
        if (body != u->body) {
            t = jl_new_struct(jl_unionall_type, u->var, body);
            u = (jl_unionall_t*)t;
        }

        if (u->var->lb == u->var->ub || may_substitute_ub(body, u->var)) {
            body = (jl_value_t*)u;
            JL_TRY {
                jl_typeenv_t env = { u->var, u->var->ub, NULL };
                t = inst_type_w_(u->body, &env, stack, 1, 0);
            }
            JL_CATCH {
                // just skip normalization
                // (may happen for bounds inconsistent with the wrapper's bounds)
            }
        }
        JL_GC_POP();
    }
    return t;
}

// used to expand an NTuple to a flat representation
static jl_value_t *jl_tupletype_fill(size_t n, jl_value_t *t, int check, int nothrow,
                                     jl_typestack_t *stack) JL_CANSAFEPOINT
{
    jl_value_t *p = NULL;
    JL_GC_PUSH1(&p);
    if (check) {
        // Since we are skipping making the Vararg and skipping checks later,
        // we inline the checks from jl_wrap_vararg here now
        if (!jl_valid_type_param(t)) {
            if (nothrow) {
                JL_GC_POP();
                return NULL;
            }
            jl_type_error_rt("Vararg", "type", (jl_value_t*)jl_type_type, t);
        }
        // jl_wrap_vararg sometimes simplifies the type, so we only do this 1 time, instead of for each n later
        t = normalize_unionalls(t, stack);
        p = t;
        jl_value_t *tw = extract_wrapper(t);
        if (tw && t != tw && !jl_has_free_typevars(t) && jl_types_equal(t, tw))
            t = tw;
        p = t;
        check = 0; // remember that checks are already done now
    }
    p = (jl_value_t*)jl_svec_fill(n, t);
    p = inst_datatype_inner(jl_anytuple_type, (jl_svec_t*)p, jl_svec_data((jl_svec_t*)p), n, stack, NULL, check, 0);
    JL_GC_POP();
    return p;
}

static jl_value_t *_jl_instantiate_type_in_env(jl_value_t *ty, jl_unionall_t *env, jl_value_t **vals, jl_typeenv_t *prev, jl_typestack_t *stack) JL_CANSAFEPOINT;

// is `stack` inside a lazy supertype computation?
static int typestack_lazy(jl_typestack_t *stack) JL_NOTSAFEPOINT
{
    for (; stack != NULL; stack = stack->prev)
        if (stack->lazy)
            return 1;
    return 0;
}

// the number of instantiations of `tn` in progress on `stack`, up to `max`
static int typename_depth_on_stack(jl_typestack_t *stack, jl_typename_t *tn, int max) JL_NOTSAFEPOINT
{
    int depth = 0;
    while (stack != NULL && depth < max) {
        if (stack->tt && stack->tt->name == tn)
            depth++;
        stack = stack->prev;
    }
    return depth;
}

// does `t` (a type or parameter value) mention an application of `tn`? Used to
// recognize self-referential definitions, whose supertype/field-type graph may
// have no finite materialization when the recursive applications are
// structurally increasing (issue #61347), and is completed lazily instead.
static int typename_occurs_in(jl_value_t *t, jl_typename_t *tn) JL_NOTSAFEPOINT
{
    if (t == NULL)
        return 0;
    if (jl_is_svec(t)) {
        size_t i, l = jl_svec_len(t);
        for (i = 0; i < l; i++) {
            if (typename_occurs_in(jl_svecref(t, i), tn))
                return 1;
        }
        return 0;
    }
    if (jl_is_datatype(t)) {
        if (((jl_datatype_t*)t)->name == tn)
            return 1;
        return typename_occurs_in((jl_value_t*)((jl_datatype_t*)t)->parameters, tn);
    }
    if (jl_is_unionall(t)) {
        jl_unionall_t *u = (jl_unionall_t*)t;
        return typename_occurs_in(u->var->lb, tn) || typename_occurs_in(u->var->ub, tn) ||
               typename_occurs_in(u->body, tn);
    }
    if (jl_is_uniontype(t)) {
        return typename_occurs_in(((jl_uniontype_t*)t)->a, tn) ||
               typename_occurs_in(((jl_uniontype_t*)t)->b, tn);
    }
    if (jl_is_vararg(t)) {
        jl_vararg_t *vm = (jl_vararg_t*)t;
        return typename_occurs_in(vm->T, tn) || typename_occurs_in(vm->N, tn);
    }
    if (jl_is_typeeq(t))
        return typename_occurs_in(jl_typeeq_T(t), tn);
    if (jl_is_typeegal(t))
        return typename_occurs_in(jl_typeegal_T(t), tn);
    // n.b. free TypeVar bounds are not walked: materialized typevar bound
    // graphs may be cyclic
    return 0;
}

// Does the supertype declaration `decl` of `tn` close a cycle of declarations
// on `stack`: is `tn` in progress, and does `decl` mention it or a typename
// instantiated since (`struct S{T} <: A{R{Tuple{T}}}` with
// `struct R{T} <: A{S{Tuple{T}}}`)?
static int super_cycle_on_stack(jl_typestack_t *stack, jl_typename_t *tn, jl_value_t *decl) JL_NOTSAFEPOINT
{
    if (!typename_depth_on_stack(stack, tn, 1))
        return 0;
    for (; stack != NULL; stack = stack->prev) {
        if (stack->tt == NULL)
            continue;
        if (typename_occurs_in(decl, stack->tt->name))
            return 1;
        if (stack->tt->name == tn)
            return 0;
    }
    return 0;
}

// Is a supertype declaration valid for every value of the definition's
// typevars? Deferring a supertype (see `inst_datatype_complete`) moves the
// instantiation of a level from the construction of the type to the first
// demand for its supertype, from inside subtyping. That must not throw, so a
// supertype is deferred only when its declaration provably cannot fail: every
// type application in it satisfies the applied wrapper's bounds universally
// (the argument wrapped in `UnionAll`s over the free typevars, a lower or
// dependent bound only when trivial), can't be an invalid `Tuple`, `NamedTuple` or
// `GenericMemory`, and the applied typenames' own declarations hold too
// (recursively, a typename under consideration taken as valid). This holds for
// typevars ranging over the types other than `Union{}`, if no recursive
// application has a value parameter (which makes levels with values), or with
// `values` over other values too, if no typevar is a `Union` member or a
// typevar bound. Each deferred level checks what its parameters need
// (`level_decl_total`). Otherwise the level is instantiated eagerly and an
// invalid one fails at construction.
struct decl_stack_t {
    jl_typename_t *tn;
    struct decl_stack_t *prev;
};

static int decl_seen(jl_typename_t *tn, struct decl_stack_t *seen) JL_NOTSAFEPOINT
{
    for (; seen != NULL; seen = seen->prev)
        if (seen->tn == tn)
            return 1;
    return 0;
}

// with `values`, can `t` be a value: is it a typevar of the declaration?
static int decl_value_var(jl_value_t *t, jl_svec_t *vars, int values) JL_NOTSAFEPOINT
{
    if (!values || !jl_is_typevar(t))
        return 0;
    for (size_t i = 0; i < jl_svec_len(vars); i++)
        if (jl_svecref(vars, i) == t)
            return 1;
    return 0;
}

static int decl_total(jl_value_t *t, jl_svec_t *vars, struct decl_stack_t *seen, int values) JL_CANSAFEPOINT;

static int typename_decl_total(jl_typename_t *tn, struct decl_stack_t *seen, int values) JL_CANSAFEPOINT
{
    if (decl_seen(tn, seen))
        return 1;
    if (tn->wrapper == NULL)
        return 1; // bootstrap: declared by hand, no bounds
    jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(tn->wrapper);
    if (dt->super == NULL)
        return 0; // definition in progress
    if (dt->super == jl_any_type)
        return 1;
    struct decl_stack_t top = { tn, seen };
    return decl_total((jl_value_t*)dt->super, dt->parameters, &top, values);
}

// `lb <: arg <: ub` for every value of `vars`
static int bound_total(jl_value_t *arg, jl_value_t *lb, jl_value_t *ub, jl_svec_t *vars) JL_CANSAFEPOINT
{
    if (!jl_is_type(arg) && !jl_is_typevar(arg))
        return lb == jl_bottom_type && ub == (jl_value_t*)jl_any_type;
    if (lb != jl_bottom_type && (jl_has_free_typevars(lb) || jl_has_free_typevars(arg) || !jl_subtype(lb, arg)))
        return 0;
    if (ub == (jl_value_t*)jl_any_type)
        return 1;
    if (jl_has_free_typevars(ub))
        return 0;
    jl_value_t *w = arg;
    JL_GC_PUSH1(&w);
    for (size_t i = jl_svec_len(vars); i-- > 0; ) {
        jl_tvar_t *v = (jl_tvar_t*)jl_svecref(vars, i);
        if (jl_has_typevar(w, v))
            w = jl_type_unionall(v, w);
    }
    int total = !jl_has_free_typevars(w) && jl_subtype(w, ub);
    JL_GC_POP();
    return total;
}

static int decl_total(jl_value_t *t, jl_svec_t *vars, struct decl_stack_t *seen, int values)
{
    // a closed term was checked with the declaration, and instantiating a
    // level returns it as is, so it never makes a new level
    if (!jl_has_free_typevars(t))
        return 1;
    if (jl_is_datatype(t)) {
        jl_datatype_t *d = (jl_datatype_t*)t;
        jl_typename_t *dn = d->name;
        size_t i, np = jl_nparams(d);
        if (dn == jl_tuple_typename) {
            for (i = 0; i < np; i++) {
                jl_value_t *p = jl_tparam(d, i);
                if (jl_is_vararg(p)) {
                    jl_value_t *n = jl_unwrap_vararg_num(p);
                    if (n != NULL && jl_is_typevar(n))
                        return 0; // must become an integer
                }
            }
        }
        else if (dn == jl_namedtuple_typename) {
            // names and field types are validated at instantiation
            return 0;
        }
        else {
            if (dn == jl_genericmemory_typename || dn == jl_genericmemoryref_typename) {
                if (!jl_is_symbol(jl_tparam(d, 0)) || !jl_is_addrspace(jl_tparam(d, 2)))
                    return 0;
            }
            // a bound that depends on an earlier parameter (`D{X, Y<:X}`) has free
            // typevars, which `bound_total` does not prove
            jl_value_t *w = dn->wrapper;
            for (i = 0; i < np && jl_is_unionall(w); i++) {
                jl_tvar_t *v = ((jl_unionall_t*)w)->var;
                if (!bound_total(jl_tparam(d, i), v->lb, v->ub, vars))
                    return 0;
                w = ((jl_unionall_t*)w)->body;
            }
            if (!values && decl_seen(dn, seen)) {
                for (i = 0; i < np; i++) {
                    jl_value_t *p = jl_tparam(d, i);
                    if (!jl_is_type(p) && !jl_is_typevar(p))
                        return 0;
                }
            }
            if (!typename_decl_total(dn, seen, values))
                return 0;
        }
        for (i = 0; i < np; i++) {
            if (!decl_total(jl_tparam(d, i), vars, seen, values))
                return 0;
        }
        return 1;
    }
    if (jl_is_unionall(t)) {
        jl_unionall_t *u = (jl_unionall_t*)t;
        if (decl_value_var(u->var->lb, vars, values) || decl_value_var(u->var->ub, vars, values))
            return 0;
        return decl_total(u->var->lb, vars, seen, values) && decl_total(u->var->ub, vars, seen, values) &&
               decl_total(u->body, vars, seen, values);
    }
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        if (decl_value_var(u->a, vars, values) || decl_value_var(u->b, vars, values))
            return 0;
        return decl_total(u->a, vars, seen, values) && decl_total(u->b, vars, seen, values);
    }
    if (jl_is_vararg(t)) {
        jl_vararg_t *vm = (jl_vararg_t*)t;
        return (vm->T == NULL || decl_total(vm->T, vars, seen, values)) &&
               (vm->N == NULL || decl_total(vm->N, vars, seen, values));
    }
    if (jl_is_typeeq(t))
        return decl_total(jl_typeeq_T(t), vars, seen, values);
    if (jl_is_typeegal(t))
        return decl_total(jl_typeegal_T(t), vars, seen, values);
    return 1; // typevars and constants
}

// is the declaration of level `dt` proved for its parameters? Inside a lazy
// supertype computation a new level passes again: its parameters are built
// from those of a checked level, so they hold a value only if that one did or
// a recursive application has one (refused by the proof for types), and
// `Union{}` only from a literal one (rejected with the definition)
static int level_decl_total(jl_datatype_t *dt) JL_CANSAFEPOINT
{
    int values = 0;
    size_t i, np = jl_nparams(dt);
    for (i = 0; i < np; i++) {
        jl_value_t *p = jl_tparam(dt, i);
        if (p == jl_bottom_type)
            return 0;
        if (!jl_is_type(p))
            values = 1;
    }
    return (!values && typename_decl_total(dt->name, NULL, 0)) ||
           typename_decl_total(dt->name, NULL, 1);
}

static jl_value_t *inst_datatype_complete(jl_datatype_t *dt, jl_datatype_t *ndt, jl_typestack_t *stack,
                                          jl_typeenv_t *env, int check, int nothrow, int cacheable, int defer) JL_CANSAFEPOINT;
static jl_value_t *inst_datatype_region(jl_datatype_t *dt, jl_datatype_t *ndt, jl_typestack_t *stack,
                                        jl_typeenv_t *env, int check, int nothrow, int low) JL_CANSAFEPOINT;

static jl_value_t *inst_datatype_inner(jl_datatype_t *dt, jl_svec_t *p, jl_value_t **iparams, size_t ntp,
                                       jl_typestack_t *stack, jl_typeenv_t *env, int check, int nothrow)
{
    jl_typestack_t top;
    jl_typename_t *tn = dt->name;
    int istuple = (tn == jl_tuple_typename);
    int isnamedtuple = (tn == jl_namedtuple_typename);

    // check if type cache will be applicable
    // n.b. for deferred instantiations, `cacheable` still controls the cache
    // lookup, hash computation and eager field/layout instantiation; only the
    // write side (cache insertion, global-root interning) is redirected below
    int cacheable = 1;
    if (istuple) {
        size_t i;
        for (i = 0; i < ntp; i++) {
            jl_value_t *pi = iparams[i];
            if (jl_is_vararg(pi) && jl_unwrap_vararg(pi) == jl_bottom_type) {
                jl_value_t *va1 = jl_unwrap_vararg_num(pi);
                if (va1 && jl_is_long(va1)) {
                    ssize_t nt = jl_unbox_long(va1);
                    if (nt == 0)
                        va1 = NULL;
                    else
                        pi = jl_bottom_type; // trigger errorf below
                }
                // This imposes an implicit constraint that va1==0,
                // so we keep the Vararg if it has a TypeVar
                if (va1 == NULL) {
                    p = NULL;
                    ntp -= 1;
                    assert(i == ntp);
                    break;
                }
            }
            if (pi == jl_bottom_type) {
                if (nothrow)
                    return NULL;
                jl_errorf("Tuple field type cannot be Union{}");
            }
            if (cacheable && !jl_is_concrete_type(pi))
                cacheable = 0;
        }
    }
    else {
        size_t i;
        for (i = 0; cacheable && i < ntp; i++)
            if (jl_has_free_typevars(iparams[i]))
                cacheable = 0;
    }
    // if applicable, check the cache first for a match
    jl_deferred_typecache_t *region = typecache_region_held();
    if (cacheable) {
        jl_value_t *lkup = (jl_value_t*)lookup_type(tn, iparams, ntp);
        if (lkup == NULL)
            lkup = lookup_deferred_type(region, tn, iparams, ntp);
        if (lkup != NULL)
            return lkup;
    }
    // if some normalization might be needed, do that now
    // it is probably okay to mutate iparams, and we only store globally rooted objects here
    if (check) {
        size_t i;
        for (i = 0; i < ntp; i++) {
            jl_value_t *pi = iparams[i];
            if (pi == jl_bottom_type)
                continue;
            if (jl_is_datatype(pi))
                continue;
            if (jl_is_vararg(pi))
                // This is already handled in jl_wrap_vararg instead
                continue;
            if (!cacheable && jl_has_free_typevars(pi))
                continue;
            // normalize types equal to wrappers
            jl_value_t *tw = extract_wrapper(pi);
            if (tw && tw != pi &&
                    !jl_has_free_typevars(pi) && jl_types_equal(pi, tw)) {
                if (p)
                    jl_gc_write(p, iparams[i], jl_value_t, tw);
                else
                    iparams[i] = tw;
            }
        }
    }
    // then check the cache again, if applicable
    if (cacheable) {
        jl_value_t *lkup = (jl_value_t*)lookup_type(tn, iparams, ntp);
        if (lkup != NULL)
            return lkup;
    }
    jl_value_t *stack_lkup = lookup_type_stack(stack, dt, ntp, iparams);
    if (stack_lkup)
        return stack_lkup;

    // check parameters against bounds in type definition
    // for whether this is even valid
    if (check && !istuple) {
        assert(ntp > 0);
        if (check_datatype_parameters(tn, iparams, ntp, nothrow, stack))
            return NULL;
    }
    else if (ntp == 0 && jl_emptytuple_type != NULL) {
        // empty tuple type case
        assert(istuple);
        return (jl_value_t*)jl_emptytuple_type;
    }

    jl_datatype_t *ndt = NULL;
    jl_value_t *res = NULL;
    JL_GC_PUSH3(&p, &ndt, &res);

    jl_value_t *last = iparams[ntp - 1];
    if (istuple && ntp > 0 && jl_is_vararg(last)) {
        // normalize Tuple{..., Vararg{Int, 3}} to Tuple{..., Int, Int, Int}
        jl_value_t *va = jl_unwrap_unionall(last);
        jl_value_t *va0 = jl_unwrap_vararg(va), *va1 = jl_unwrap_vararg_num(va);
        // return same `Tuple` object for types equal to it
        if (ntp == 1 && va0 == (jl_value_t*)jl_any_type && !va1) {
            JL_GC_POP();
            return (jl_value_t*)jl_anytuple_type;
        }
        if (va1 && jl_is_long(va1)) {
            ssize_t nt = jl_unbox_long(va1);
            assert(nt >= 0);
            if (nt == 0 || !jl_has_free_typevars(va0)) {
                if (ntp == 1) {
                    JL_GC_POP();
                    return jl_tupletype_fill(nt, va0, 0, 0, stack);
                }
                size_t i, l;
                p = jl_alloc_svec(ntp - 1 + nt);
                for (i = 0, l = ntp - 1; i < l; i++)
                    jl_svecset(p, i, iparams[i]);
                l = ntp - 1 + nt;
                for (; i < l; i++)
                    jl_svecset(p, i, va0);
                size_t np = jl_svec_len(p);
                jl_value_t **pp = jl_svec_data(p);
                jl_value_t *ndt = inst_datatype_inner(jl_anytuple_type, p, pp, np, stack, NULL, check, nothrow);
                JL_GC_POP();
                return ndt;
            }
        }
    }

    if (p == NULL) {
        // slots in iparams are untracked by the GC static analyzer so it won't
        // complain, but this is used as rooting storage for normalized types
        // below so it must be rooted properly by the GC
        p = jl_alloc_svec_uninit(ntp);
        for (size_t i = 0; i < ntp; i++) {
            jl_gc_write_fresh(p, jl_svec_data(p)[i], jl_value_t, iparams[i]);
        }
        iparams = jl_svec_data(p);
    }
    assert(jl_is_svec(p) && iparams == jl_svec_data(p));

    // try to simplify some type parameters
    if (check) {
        int changed = 0;
        if (istuple) // normalization might change Tuple's, but not other types's, cacheable status
            cacheable = 1;
        size_t i;
        for (i = 0; i < ntp; i++) {
            jl_value_t *pi = iparams[i];
            jl_value_t *newp = normalize_unionalls(pi, stack);
            if (newp != pi) {
                jl_gc_write(p, iparams[i], jl_value_t, newp);
                changed = 1;
            }
            if (istuple && cacheable && !jl_is_concrete_type(newp))
                cacheable = 0;
        }
        if (changed) {
            // If this changed something, we need to check the cache again, in
            // case we missed the match earlier before the normalizations
            //
            // e.g. return inst_datatype_inner(dt, p, iparams, ntp, stack, env, 0);
            if (cacheable) {
                jl_value_t *lkup = (jl_value_t*)lookup_type(tn, iparams, ntp);
                if (lkup == NULL)
                    lkup = lookup_deferred_type(region, tn, iparams, ntp);
                if (lkup != NULL) {
                    JL_GC_POP();
                    return lkup;
                }
            }
            jl_value_t *stack_lkup = lookup_type_stack(stack, dt, ntp, iparams);
            if (stack_lkup) {
                JL_GC_POP();
                return stack_lkup;
            }
        }
    }

    // the level of the unfinished types the parameters mention (see
    // `region_finish`); one that mentions a typegroup's member (`defer`) is
    // made without its layout
    int low = REGION_NONE;
    if (cacheable && region != NULL && !region_clean(region))
        low = region_low(region, (jl_value_t*)p, low);
    int defer = low == 0;

    // try to reduce duplication in objects (if the caller didn't already check) by
    // comparing them against a list of objects already known to be globally rooted and
    // swapping them as possible
    if (check && jl_global_roots_list != NULL) {
        for (size_t i = 0; i < ntp; i++) {
            jl_value_t *pi = iparams[i];
            if (cacheable || !jl_has_free_typevars(pi)) {
                pi = jl_as_global_root(pi, cacheable && low == REGION_NONE);
                if (pi != NULL) {
                    jl_gc_write(p, iparams[i], jl_value_t, pi);
                }
            }
        }
    }

    ndt = jl_new_uninitialized_datatype();

    // create and initialize new type
    ndt->isprimitivetype = dt->isprimitivetype;
    // Usually dt won't have ismutationfree set at this point, but it is
    // overridden for `Type`, which we handle here.
    ndt->ismutationfree = dt->ismutationfree;
    jl_gc_write(ndt, ndt->name, jl_typename_t, tn);
    ndt->super = NULL;
    jl_gc_write(ndt, ndt->parameters, jl_svec_t, p);
    ndt->types = NULL; // to be filled in below
    int invalid = 0;
    if (istuple) {
        jl_gc_write(ndt, ndt->types, jl_svec_t, p); // TODO: this may need to filter out certain types
    }
    else if (isnamedtuple) {
        jl_value_t *names_tup = jl_svecref(p, 0);
        jl_value_t *values_tt = jl_svecref(p, 1);
        if (!jl_has_free_typevars(names_tup) && !jl_has_free_typevars(values_tt)) {
            if (!jl_is_tuple(names_tup)) {
                if (!nothrow)
                    jl_type_error_rt("NamedTuple", "names", (jl_value_t*)jl_anytuple_type, names_tup);
                invalid = 1;
            }
            size_t nf = jl_nfields(names_tup);
            for (size_t i = 0; i < nf; i++) {
                jl_value_t *ni = jl_fieldref(names_tup, i);
                if (!jl_is_symbol(ni)) {
                    if (!nothrow)
                        jl_type_error_rt("NamedTuple", "name", (jl_value_t*)jl_symbol_type, ni);
                    invalid = 1; break;
                }
                for (size_t j = 0; j < i; j++) {
                    if (ni == jl_fieldref_noalloc(names_tup, j)) {
                        if (!nothrow)
                            jl_errorf("duplicate field name in NamedTuple: \"%s\" is not unique", jl_symbol_name((jl_sym_t*)ni));
                        invalid = 1; break;
                    }
                }
                if (invalid) break;
            }
            if (values_tt == jl_bottom_type && nf > 0) {
                jl_gc_write(ndt, ndt->types, jl_svec_t, jl_svec_fill(nf, jl_bottom_type));
            }
            else {
                if (!jl_is_datatype(values_tt)) {
                    // reachable for `NamedTuple{(), Union{}}`, e.g. from type intersection
                    if (!nothrow)
                        jl_error("NamedTuple field type must be a tuple datatype");
                    invalid = 1;
                }
                else {
                    if (jl_is_va_tuple((jl_datatype_t*)values_tt) || jl_nparams(values_tt) != nf) {
                        if (!nothrow)
                            jl_error("NamedTuple names and field types must have matching lengths");
                        invalid = 1;
                    }
                    jl_gc_write(ndt, ndt->types, jl_svec_t, ((jl_datatype_t*)values_tt)->parameters);
                }
            }
        }
        else {
            ndt->types = jl_emptysvec; // XXX: this is essentially always incorrect
        }
    }
    else if (tn == jl_genericmemoryref_typename || tn == jl_genericmemory_typename) {
        jl_value_t *isatomic = jl_svecref(p, 0);
        if (!jl_is_typevar(isatomic) && !jl_is_symbol(isatomic)) {
            if (!nothrow)
                jl_type_error_rt("GenericMemory", "isatomic parameter", (jl_value_t*)jl_symbol_type, isatomic);
            invalid = 1;
        }
        jl_value_t *addrspace = jl_svecref(p, 2);
        if (!jl_is_typevar(addrspace) && !jl_is_addrspace(addrspace)) {
            if (!nothrow)
                jl_type_error_rt("GenericMemory", "addrspace parameter", (jl_value_t*)jl_addrspace_type, addrspace);
            invalid = 1;
        }
    }

    if (nothrow && invalid) {
        JL_GC_POP();
        return NULL;
    }
    jl_precompute_memoized_dt(ndt, cacheable);
    // associate these parameters with the new type on
    // the stack, in case one of its field types references it.
    top.tt = (jl_datatype_t*)ndt;
    top.prev = stack;
    top.lazy = 0;
    stack = &top;
    if (cacheable)
        res = inst_datatype_region(dt, ndt, stack, env, check, nothrow, low);
    else
        res = inst_datatype_complete(dt, ndt, stack, env, check, nothrow, cacheable, defer);
    JL_GC_POP();
    return res;
}

// Complete the cacheable `ndt` under `typecache_lock`, as a frame of the
// current region (`typecache_region`), or of a new one if there is none.
// `low` is the level of what its parameters mention
static jl_value_t *inst_datatype_region(jl_datatype_t *dt, jl_datatype_t *ndt, jl_typestack_t *stack,
                                        jl_typeenv_t *env, int check, int nothrow, int low)
{
    int defer = low == 0;
    jl_value_t *res = NULL;
    JL_GC_PUSH1(&res);
    // acquire the write lock now that we know we need a new object
    // since we're going to immediately leak it globally via the instantiation stack
    JL_LOCK(&typecache_lock); // Might GC
    jl_deferred_typecache_t local;
    region_init(&local);
    // lend the stack and the region to the lazy supertype computations nested
    // in this region (`typecache_stack_held`); every exit takes them back, so
    // no dead frame is ever reachable from the globals
    jl_typestack_t *saved = typecache_stack;
    // a nested region continues the stack it replaces (every instantiation
    // hands over its stack), so lazy supertypes still see the outer types
    assert(typestack_extends(stack, saved));
    jl_deferred_typecache_t *region = typecache_region ? typecache_region : &local;
    assert(region != &local || local.list == NULL || jl_array_nrows(local.list) == 0);
    size_t mark = region->lows.len, wmark = region->written.len;
    arraylist_push(&region->frames, ndt);
    int level = region->frames.len;
    // what mentions the frame also waits for what its parameters mention
    if (!region_clean(region))
        ptrhash_put(&region->pending, ndt, region_encode(low < level ? low : level));
    typecache_stack = stack;
    typecache_region = region;
    int done = 0;
    JL_TRY {
        res = inst_datatype_complete(dt, ndt, stack, env, check, nothrow, 1, defer);
        arraylist_pop(&region->frames);
        if (!region_clean(region))
            ptrhash_remove(&region->pending, ndt);
        done = 1;
        if (res == (jl_value_t*)ndt) {
            region_finish(region, ndt, level, mark, defer);
        }
        else { // a `nothrow` failure, or a duplicate of a cached type: what mentions it goes with it
            region_undo(region, wmark);
            region_remove(region, mark, 0, 0);
        }
        region_pop(region, level);
    }
    JL_CATCH {
        if (!done) {
            arraylist_pop(&region->frames);
            if (!region_clean(region))
                ptrhash_remove(&region->pending, ndt);
        }
        region_undo(region, wmark);
        region_remove(region, mark, 0, 0);
        region_pop(region, level);
        typecache_stack = saved;
        typecache_region = region == &local ? NULL : region;
        region_free(&local);
        jl_rethrow();
    }
    typecache_stack = saved;
    typecache_region = region == &local ? NULL : region;
    assert(local.lows.len == 0);
    region_free(&local);
    JL_UNLOCK(&typecache_lock); // Might GC
    JL_GC_POP();
    return res;
}

// the supertype declaration of `primarydt`, the definition of `ndt`, at its parameters
static jl_value_t *inst_declared_super(jl_datatype_t *primarydt, jl_datatype_t *ndt, jl_typestack_t *stack,
                                       int check, int nothrow) JL_CANSAFEPOINT
{
    size_t nparams = jl_svec_len(primarydt->parameters), pi;
    jl_typeenv_t *penv = (jl_typeenv_t*)alloca(nparams * sizeof(jl_typeenv_t));
    for (pi = 0; pi < nparams; pi++) {
        penv[pi].var = (jl_tvar_t*)jl_svecref(primarydt->parameters, pi);
        penv[pi].val = jl_svecref(ndt->parameters, pi);
        penv[pi].prev = pi == 0 ? NULL : &penv[pi - 1];
    }
    return inst_type_w_((jl_value_t*)primarydt->super, nparams == 0 ? NULL : &penv[nparams - 1],
                        stack, check, nothrow);
}

// Finish `ndt`, which has its name, parameters and memoized flags and sits on
// top of `stack`: look the key up once more under `typecache_lock` (it was
// taken for a cacheable, non-deferred type), then compute the layout, the
// supertype and the field types, and publish. The lookup compares keys by
// subtyping, which may compute a lazy supertype whose instantiation reaches
// the key of `ndt` itself; it finds `ndt` on the stack (`typecache_stack`).
// Returns the cached type if one exists, NULL for a `nothrow` failure, else `ndt`
static jl_value_t *inst_datatype_complete(jl_datatype_t *dt, jl_datatype_t *ndt, jl_typestack_t *stack,
                                          jl_typeenv_t *env, int check, int nothrow, int cacheable, int defer)
{
    jl_typename_t *tn = ndt->name;
    int istuple = (tn == jl_tuple_typename);
    int isnamedtuple = (tn == jl_namedtuple_typename);
    if (cacheable) {
        jl_svec_t *p = ndt->parameters;
        jl_value_t *lkup = defer ? NULL : (jl_value_t*)lookup_type(tn, jl_svec_data(p), jl_svec_len(p));
        if (lkup == NULL)
            lkup = lookup_deferred_type(typecache_region_held(), tn, jl_svec_data(p), jl_svec_len(p));
        if (lkup)
            return lkup;
    }
    jl_datatype_t *primarydt = ((jl_datatype_t*)jl_unwrap_unionall(tn->wrapper));
    // for deferred instantiations, the layout is computed at publication:
    // the group's types are still incomplete here, and computing it now
    // would bake in wrong assumptions (cf. #40050)
    if (primarydt->layout && !defer)
        jl_compute_field_offsets(ndt);

    // A definition whose supertype declaration recursively applies the type
    // being defined at structurally increasing parameters (issue #61347, e.g.
    // `struct S{T} <: A{S{Tuple{T}}} end`), directly or through other
    // declarations, has an infinite supertype graph: eagerly instantiating
    // each level's supertype creates the next level. When another
    // instantiation of this typename is already in flight above this one
    // (`stack` has `ndt` itself on top) and the supertype template mentions
    // the typename or one instantiated since (`super_cycle_on_stack`), defer
    // the supertype and complete it lazily, one
    // level per demand (`jl_datatype_compute_super`). Same-parameter
    // self-references never get here (`lookup_type_stack` ties that knot),
    // so ordinary recursive types are unaffected. Field types stay eager: a
    // concrete type must publish with its layout, and only the supertype
    // declaration can force an infinite graph through abstract levels.
    // A closed type defers only a declaration that cannot fail (`decl_total`):
    // the deferred computation runs from inside subtyping, which must not
    // throw. Another one is instantiated eagerly, so that an invalid level
    // fails at construction, and an unbounded graph is refused at a depth no
    // finite one reaches (each level of a finite one is an application written
    // in the declaration, found on the stack). A lazy supertype computation
    // skips the refusal: it completes a type that exists (such as one loaded
    // from an image), whose levels were instantiated once already, and the
    // depth it reaches them at depends on the stack it continues. The templates of a definition
    // (free typevars) always defer, as the definition is finite only this way,
    // and so does an unchecked copy (`jl_rename_unionall`): it carries the
    // renamed typevar, so no level of it can fail a bounds check.
    int in_definition = primarydt->super == NULL;
    int defer_super = 0;
    if (!istuple && !isnamedtuple && !in_definition) {
        jl_value_t *decl = (jl_value_t*)primarydt->super;
        if (!check) {
            defer_super = typename_occurs_in(decl, tn) || super_cycle_on_stack(stack->prev, tn, decl);
            assert(!defer_super || !cacheable);
        }
        else if (super_cycle_on_stack(stack->prev, tn, decl)) {
            defer_super = !cacheable || level_decl_total(ndt);
            if (!defer_super && typename_depth_on_stack(stack->prev, tn, 64) == 64 &&
                    !typestack_lazy(stack)) {
                if (nothrow)
                    return NULL;
                jl_errorf("supertype declaration of %s applies it at structurally increasing parameters "
                          "and cannot be shown valid for every parameter, so its supertypes cannot be instantiated",
                          jl_symbol_name(tn->name));
            }
        }
    }
    if (istuple || isnamedtuple) {
        ndt->super = jl_any_type;
    }
    else if (defer_super) {
        // deferred (see above)
    }
    else if (!in_definition) {
        // from the declaration, not from `dt`: it may be a template whose own
        // supertype is deferred (unset)
        jl_value_t *super = inst_declared_super(primarydt, ndt, stack, check, nothrow);
        if (nothrow && super == NULL)
            return NULL;
        // a nested subtype walk that found `ndt` on the stack may have computed
        // it lazily meanwhile (`jl_datatype_compute_super`): keep that one
        jl_datatype_t *unset = NULL;
        jl_gc_wb(ndt, (void*)&ndt->super, super);
        jl_atomic_cmpswap((_Atomic(jl_datatype_t*)*)&ndt->super, &unset, (jl_datatype_t*)super);
    }
    jl_svec_t *ftypes = dt->types;
    if (ftypes == NULL)
        ftypes = primarydt->types;
    // `partial` is drained only when the definition completes, so a type made
    // afterwards never goes there, even from a template whose supertype is
    // deferred (unset): its supertype and field types are computed lazily
    if (ftypes == NULL || in_definition) {
        // in the process of creating this type definition:
        // need to instantiate the super and types fields later
        if (tn->partial == NULL) {
            jl_gc_write(tn, tn->partial, jl_array_t, jl_alloc_vec_any(0));
        }
        jl_array_ptr_1d_push(tn->partial, (jl_value_t*)ndt);
    }
    else if (!isnamedtuple && !istuple) {
        assert(ftypes != jl_emptysvec || jl_field_names(ndt) == jl_emptysvec);
        assert(ftypes == jl_emptysvec || !ndt->name->abstract);
        if (ftypes == jl_emptysvec) {
            jl_publish_fieldtypes(ndt, ftypes);
        }
        else if (cacheable) {
            // recursively instantiate the types of the fields
            if (dt->types == NULL)
                compute_fieldtypes_(ndt, stack, cacheable && !defer);
            else
                jl_publish_fieldtypes(ndt, inst_ftypes(ftypes, env, stack, cacheable && !defer));
        }
    }

    // the caller publishes a cacheable result (`region_finish`)
    return (jl_value_t*)ndt;
}

static jl_value_t *jl_apply_tuple_type_v_(jl_value_t **p, size_t np, jl_svec_t *params, int check)
{
    return inst_datatype_inner(jl_anytuple_type, params, p, np, NULL, NULL, check, 0);
}

JL_DLLEXPORT jl_value_t *jl_apply_tuple_type(jl_svec_t *params, int check)
{
    return jl_apply_tuple_type_v_(jl_svec_data(params), jl_svec_len(params), params, check);
}

JL_DLLEXPORT jl_value_t *jl_apply_tuple_type_v(jl_value_t **p, size_t np)
{
    return jl_apply_tuple_type_v_(p, np, NULL, 1);
}

jl_tupletype_t *jl_lookup_arg_tuple_type(jl_value_t *arg1, jl_value_t **args, size_t nargs, int leaf)
{
    return (jl_datatype_t*)lookup_typevalue(jl_tuple_typename, arg1, args, nargs, leaf);
}

jl_tupletype_t *jl_inst_arg_tuple_type(jl_value_t *arg1, jl_value_t **args, size_t nargs, int leaf)
{
    jl_tupletype_t *tt = (jl_datatype_t*)lookup_typevalue(jl_tuple_typename, arg1, args, nargs, leaf);
    if (tt == NULL) {
        size_t i;
        jl_svec_t *params = jl_alloc_svec(nargs);
        JL_GC_PUSH1(&params);
        for (i = 0; i < nargs; i++) {
            jl_value_t *ai = (i == 0 ? arg1 : args[i - 1]);
            if (leaf && jl_is_type(ai)) {
                if (jl_has_free_typevars(ai)) {
                    // if `ai` has free type vars this will not be a valid
                    // (concrete) type.
                    // TODO: it would be really nice to only dispatch and cache
                    // those as `jl_typeof(ai)`, but that will require some
                    // redesign of the caching logic.
                    // free typevars are disallowed inside `TypeEgal`; fall back to the
                    // equality key `Type{ai}`, which still binds static parameters (#61242)
                    ai = (jl_value_t*)jl_wrap_Type(ai);
                }
                else {
                    // key the dispatch cache on the type value by egality, so `==`-equal
                    // but non-egal type objects get distinct entries (#61323)
                    ai = jl_wrap_TypeEgal(ai);
                }
            }
            else {
                ai = jl_typeof(ai);
            }
            jl_svecset(params, i, ai);
        }
        tt = (jl_datatype_t*)inst_datatype_inner(jl_anytuple_type, params, jl_svec_data(params), nargs, NULL, NULL, 1, 0);
        JL_GC_POP();
    }
    return tt;
}

static jl_svec_t *inst_ftypes(jl_svec_t *p, jl_typeenv_t *env, jl_typestack_t *stack, int cacheable)
{
    size_t i;
    size_t lp = jl_svec_len(p);
    jl_svec_t *np = jl_alloc_svec(lp);
    jl_value_t *pi = NULL;
    JL_GC_PUSH2(&np, &pi);
    for (i = 0; i < lp; i++) {
        pi = jl_svecref(p, i);
        JL_TRY {
            pi = inst_type_w_(pi, env, stack, 1, 0);
            if (!jl_is_type(pi) && !jl_is_typevar(pi)) {
                pi = jl_bottom_type;
            }
        }
        JL_CATCH {
            pi = jl_bottom_type;
        }
        jl_value_t *globalpi = jl_as_global_root(pi, cacheable);
        jl_svecset(np, i, globalpi ? globalpi : pi);
    }
    JL_GC_POP();
    return np;
}

static jl_value_t *inst_tuple_w_(jl_value_t *t, jl_typeenv_t *env, jl_typestack_t *stack, int check, int nothrow) JL_CANSAFEPOINT
{
    jl_datatype_t *tt = (jl_datatype_t*)t;
    jl_svec_t *tp = tt->parameters;
    size_t ntp = jl_svec_len(tp);
    // Instantiate Tuple{Vararg{T,N}} where T is fixed and N is known, such as Dims{3}
    // And avoiding allocating the intermediate steps
    // Note this does not instantiate Tuple{Vararg{Int,3}}; that's done in inst_datatype_inner
    // Note this does not instantiate NTuple{N,T}, since it is unnecessary and inefficient to expand that now
    if (jl_is_va_tuple(tt) && ntp == 1) {
        // If this is a Tuple{Vararg{T,N}} with known N and T, expand it to
        // a fixed-length tuple
        jl_value_t *T=NULL, *N=NULL;
        jl_value_t *va = jl_tparam0(tt);
        jl_value_t *ttT = jl_unwrap_vararg(va);
        jl_value_t *ttN = jl_unwrap_vararg_num(va);
        jl_typeenv_t *e = env;
        while (e != NULL) {
            if ((jl_value_t*)e->var == ttT)
                T = e->val;
            else if ((jl_value_t*)e->var == ttN)
                N = e->val;
            e = e->prev;
        }
        if (T != NULL && N != NULL && jl_is_long(N)) { // TODO: && !jl_has_free_typevars(T) to match inst_datatype_inner, or even && jl_is_concrete_type(T)
            // Since this is skipping jl_wrap_vararg, we inline the checks from it here
            ssize_t nt = jl_unbox_long(N);
            if (nt >= 0)
                return jl_tupletype_fill(nt, T, check, nothrow, stack);
            if (nothrow)
                return NULL;
            jl_errorf("Vararg length is negative: %zd", nt);
        }
    }
    jl_value_t **iparams;
    int onstack = ntp < jl_page_size/sizeof(jl_value_t*);
    JL_GC_PUSHARGS(iparams, onstack ? ntp : 1);
    jl_svec_t *ip_heap = NULL;
    if (!onstack) {
        ip_heap = jl_alloc_svec(ntp);
        iparams[0] = (jl_value_t*)ip_heap;
        iparams = jl_svec_data(ip_heap);
    }
    int i, bound = 0;
    for (i = 0; i < ntp; i++) {
        jl_value_t *elt = jl_svecref(tp, i);
        jl_value_t *pi = inst_type_w_(elt, env, stack, check, nothrow);
        if (pi == NULL) {
            assert(nothrow);
            if (nothrow == 1 || (i == ntp-1 && jl_is_vararg(elt))) {
                t = NULL;
                break;
            }
            else {
                pi = jl_bottom_type;
            }
        }
        if (ip_heap)
            jl_gc_write(ip_heap, iparams[i], jl_value_t, pi);
        else
            iparams[i] = pi;
        bound |= (pi != elt);
    }
    if (t != NULL && bound)
        t = inst_datatype_inner(tt, ip_heap, iparams, ntp, stack, env, check, nothrow);
    JL_GC_POP();
    return t;
}

// `nothrow` means that when type checking fails, the type instantiation should
// return `NULL` instead of immediately throwing an error. If `nothrow` == 2 then
// we further assume that the imprecise instantiation for non invariant parameters
// is acceptable, and inner error (`NULL`) would be ignored.
static jl_value_t *inst_type_w_(jl_value_t *t, jl_typeenv_t *env, jl_typestack_t *stack, int check, int nothrow)
{
    size_t i;
    if (jl_is_typeapp(t))
        return t;
    if (jl_is_typevar(t)) {
        jl_typeenv_t *e = env;
        while (e != NULL) {
            if (e->var == (jl_tvar_t*)t) {
                jl_value_t *val = e->val;
                return val;
            }
            e = e->prev;
        }
        return t;
    }
    if (jl_is_unionall(t)) {
        jl_unionall_t *ua = (jl_unionall_t*)t;
        jl_value_t *lb = NULL;
        jl_value_t *var = NULL;
        jl_value_t *newbody = NULL;
        JL_GC_PUSH3(&lb, &var, &newbody);
        // set nothrow <= 1 to ensure lb's accuracy.
        lb = inst_type_w_(ua->var->lb, env, stack, check, nothrow ? 1 : 0);
        if (lb == NULL) {
            assert(nothrow);
            t = NULL;
        }
        if (t != NULL) {
            var = inst_type_w_(ua->var->ub, env, stack, check, nothrow);
            if (var == NULL) {
                if (lb == jl_bottom_type)
                    var = jl_bottom_type;
                else
                    t = NULL;
            }
            else if (lb != ua->var->lb || var != ua->var->ub) {
                var = (jl_value_t*)jl_new_typevar(ua->var->name, lb, var);
            }
            else {
                var = (jl_value_t*)ua->var;
            }
        }
        if (t != NULL) {
            jl_typeenv_t newenv = { ua->var, var, env };
            newbody = inst_type_w_(ua->body, &newenv, stack, check, nothrow);
            if (newbody == NULL) {
                t = NULL;
            }
            else if (!jl_has_typevar(newbody, (jl_tvar_t *)var) && jl_has_typevar(ua->body, ua->var)) {
                // inner instantiation made a typevar disappear, e.g.
                // NTuple{0,T} => Tuple{}; drop the now-vacuous UnionAll
                // However, if the original body was degenerate and didn't have the typevar (special
                // case in method signature creation, then we don't normalize it here either to avoid
                // confusing subtyping).
                t = newbody;
            }
            else if (newbody != ua->body || var != (jl_value_t*)ua->var) {
                // if t's parameters are not bound in the environment, return it uncopied (#9378)
                t = jl_new_struct(jl_unionall_type, var, newbody);
            }
        }
        JL_GC_POP();
        return t;
    }
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        jl_value_t *a = inst_type_w_(u->a, env, stack, check, nothrow);
        jl_value_t *b = NULL;
        JL_GC_PUSH2(&a, &b);
        b = inst_type_w_(u->b, env, stack, check, nothrow);
        if (nothrow) {
            // ensure jl_type_union nothrow.
            if (a && !(jl_is_typevar(a) || jl_is_type(a)))
                a = NULL;
            if (b && !(jl_is_typevar(b) || jl_is_type(b)))
                b = NULL;
        }
        if (a != u->a || b != u->b) {
            if (!check) {
                // fast path for `jl_rename_unionall`.
                t = jl_new_struct(jl_uniontype_type, a, b);
            }
            else if (a == NULL || b == NULL) {
                assert(nothrow);
                t = nothrow == 1 ? NULL : a == NULL ? b : a;
            }
            else {
                assert(a != NULL && b != NULL);
                jl_value_t *uargs[2] = {a, b};
                t = jl_type_union(uargs, 2);
            }
        }
        JL_GC_POP();
        return t;
    }
    if (jl_is_typeeq(t)) {
        jl_typeeq_t *te = (jl_typeeq_t*)t;
        jl_value_t *T = inst_type_w_(te->T, env, stack, check, nothrow ? 1 : 0);
        JL_GC_PUSH1(&T);
        if (T == NULL) {
            assert(nothrow);
            t = NULL;
        }
        else if (T != te->T) {
            t = (jl_value_t*)jl_wrap_Type(T);
        }
        JL_GC_POP();
        return t;
    }
    if (jl_is_typeegal(t)) {
        jl_typeeq_t *te = (jl_typeeq_t*)t;
        jl_value_t *T = inst_type_w_(te->T, env, stack, check, nothrow ? 1 : 0);
        JL_GC_PUSH1(&T);
        if (T == NULL) {
            assert(nothrow);
            t = NULL;
        }
        else if (T != te->T) {
            t = jl_wrap_TypeEgal(T);
        }
        JL_GC_POP();
        return t;
    }
    if (jl_is_vararg(t)) {
        jl_vararg_t *v = (jl_vararg_t*)t;
        jl_value_t *T = NULL;
        jl_value_t *N = NULL;
        JL_GC_PUSH2(&T, &N);
        if (v->T) {
            T = inst_type_w_(v->T, env, stack, check, nothrow);
            if (T == NULL) {
                if (nothrow == 2)
                    T = jl_bottom_type;
                else
                    t = NULL;
            }
            if (t && v->N) {
                // set nothrow <= 1 to ensure invariant parameter's accuracy.
                N = inst_type_w_(v->N, env, stack, check, nothrow ? 1 : 0);
                if (N == NULL)
                    t = NULL;
            }
        }
        if (t && (T != v->T || N != v->N))
            t = (jl_value_t*)wrap_vararg_(T, N, check, nothrow, stack);
        JL_GC_POP();
        return t;
    }
    if (!jl_is_datatype(t))
        return t;
    jl_datatype_t *tt = (jl_datatype_t*)t;
    jl_svec_t *tp = tt->parameters;
    if (tp == jl_emptysvec)
        return t;
    jl_typename_t *tn = tt->name;
    if (tn == jl_tuple_typename)
        return inst_tuple_w_(t, env, stack, check, nothrow);
    size_t ntp = jl_svec_len(tp);
    jl_value_t **iparams;
    JL_GC_PUSHARGS(iparams, ntp);
    int bound = 0;
    for (i = 0; i < ntp; i++) {
        jl_value_t *elt = jl_svecref(tp, i);
        // set nothrow <= 1 to ensure invariant parameter's accuracy.
        jl_value_t *pi = inst_type_w_(elt, env, stack, check, nothrow ? 1 : 0);
        if (pi == NULL) {
            assert(nothrow);
            t = NULL;
            break;
        }
        iparams[i] = pi;
        bound |= (pi != elt);
    }
    // if t's parameters are not bound in the environment, return it uncopied (#9378)
    if (t != NULL && bound)
        t = inst_datatype_inner(tt, NULL, iparams, ntp, stack, env, check, nothrow);
    JL_GC_POP();
    return t;
}

static jl_value_t *instantiate_with(jl_value_t *t, jl_value_t **env, size_t n, jl_typeenv_t *te) JL_CANSAFEPOINT
{
    if (n > 0) {
        jl_typeenv_t en = { (jl_tvar_t*)env[0], env[1], te };
        return instantiate_with(t, &env[2], n-1, &en );
    }
    return inst_type_w_(t, te, NULL, 1, 0);
}

jl_value_t *jl_instantiate_type_with(jl_value_t *t, jl_value_t **env, size_t n)
{
    return instantiate_with(t, env, n, NULL);
}

static jl_value_t *_jl_instantiate_type_in_env(jl_value_t *ty, jl_unionall_t *env, jl_value_t **vals, jl_typeenv_t *prev, jl_typestack_t *stack)
{
    // `svec(inner, constrained::Bool)` is the env-entry marker for an uncertain
    // sparam value produced by subtyping/intersection. `inner` is either the
    // TypeVar itself (identity preserved) or a DataType that still contains
    // free typevars; either way it is the value to substitute here.
    jl_value_t *val = vals[0];
    if (jl_is_svec(val) && jl_svec_len((jl_svec_t*)val) == 2) {
        jl_value_t *second = jl_svecref((jl_svec_t*)val, 1);
        if (second == jl_true || second == jl_false)
            val = jl_svecref((jl_svec_t*)val, 0);
    }
    jl_typeenv_t en = { env->var, val, prev };
    if (jl_is_unionall(env->body))
        return _jl_instantiate_type_in_env(ty, (jl_unionall_t*)env->body, vals + 1, &en, stack);
    else
        return inst_type_w_(ty, &en, stack, 1, 0);
}

JL_DLLEXPORT jl_value_t *jl_instantiate_type_in_env(jl_value_t *ty, jl_unionall_t *env, jl_value_t **vals)
{
    jl_value_t *typ = ty;
    if (jl_is_unionall(env)) {
        JL_TRY {
            typ = _jl_instantiate_type_in_env(ty, env, vals, NULL, NULL);
        }
        JL_CATCH {
            typ = jl_bottom_type;
        }
    }
    return typ;
}

jl_typeeq_t *jl_wrap_Type(jl_value_t *t)
{
    if (jl_is_typeeq(t) && jl_typeeq_T(t) == jl_bottom_type)
        t = (jl_value_t*)jl_typeofbottom_type;
    jl_value_t *tw = extract_wrapper(t);
    if (tw && tw != t && jl_typeof(t) == jl_typeof(tw) &&
            !jl_has_free_typevars(t) && jl_types_equal(t, tw)) {
        t = tw;
    }
    if (t == jl_bottom_type && jl_typeofbottom_type && jl_typeofbottom_type->name) {
        jl_value_t *cached = jl_atomic_load_relaxed(&jl_typeofbottom_type->name->Typeofwrapper);
        if (cached)
            return (jl_typeeq_t*)cached;
    }
    jl_task_t *ct = jl_current_task;
    JL_GC_PUSH1(&t);
    jl_typeeq_t *te = (jl_typeeq_t*)jl_gc_alloc(ct->ptls, sizeof(jl_typeeq_t), jl_typeeq_type);
    jl_set_typetagof(te, jl_typeeq_tag, 0);
    te->T = t;
    if (t == jl_bottom_type && jl_typeofbottom_type && jl_typeofbottom_type->name) {
        jl_gc_write_atomic(jl_typeofbottom_type->name, jl_typeofbottom_type->name->Typeofwrapper, jl_value_t, (jl_value_t*)te, relaxed);
    }
    JL_GC_POP();
    return te;
}

jl_value_t *jl_wrap_TypeEgal(jl_value_t *t)
{
    // `TypeEgal` pins a type value by egality; non-type parameters and free
    // typevars are not valid.
    if (!jl_is_type(t) || jl_has_free_typevars(t))
        jl_type_error_rt("TypeEgal", "parameter", (jl_value_t*)jl_type_type, t);
    // `typeof(Union{})` already denotes exactly the single-instance set `{Union{}}`
    // (the bottom object is unique), so normalize to it
    if (t == jl_bottom_type)
        return (jl_value_t*)jl_typeofbottom_type;
    jl_task_t *ct = jl_current_task;
    JL_GC_PUSH1(&t);
    jl_value_t *te = jl_gc_alloc(ct->ptls, sizeof(jl_typeeq_t), jl_typeegal_type);
    jl_set_typetagof(te, jl_typeegal_tag, 0);
    ((jl_typeeq_t*)te)->T = t;
    JL_GC_POP();
    return te;
}

static jl_vararg_t *wrap_vararg_(jl_value_t *t, jl_value_t *n, int check, int nothrow,
                                 jl_typestack_t *stack)
{
    int valid = 1;
    jl_vararg_t *vm = NULL;
    jl_task_t *ct = jl_current_task;
    JL_GC_PUSH1(&t);
    if (check) {
        if (n) {
            if (jl_is_typevar(n) || jl_is_uniontype(jl_unwrap_unionall(n))) {
                // TODO: this is disabled due to #39698; it is also inconsistent
                // with other similar checks, where we usually only check substituted
                // values and not the bounds of variables.
                /*
                jl_tvar_t *N = (jl_tvar_t*)n;
                if (valid && !(N->lb == jl_bottom_type && N->ub == (jl_value_t*)jl_any_type)) {
                    if (!nothrow)
                        jl_error("TypeVar in Vararg length must have bounds Union{} and Any");
                    invalid = 1;
                }
                */
            }
            else if (valid && !jl_is_long(n)) {
                if (!nothrow)
                    jl_type_error_rt("Vararg", "count", (jl_value_t*)jl_long_type, n);
                valid = 0;
            }
            else if (valid && jl_unbox_long(n) < 0) {
                if (!nothrow)
                    jl_errorf("Vararg length is negative: %zd", jl_unbox_long(n));
                valid = 0;
            }
        }
        if (t) {
            if (valid && !jl_valid_type_param(t)) {
                if (!nothrow)
                    jl_type_error_rt("Vararg", "type", (jl_value_t*)jl_type_type, t);
                valid = 0;
            }
            if (valid) {
                t = normalize_unionalls(t, stack);
                jl_value_t *tw = extract_wrapper(t);
                if (tw && t != tw && !jl_has_free_typevars(t) && jl_types_equal(t, tw))
                    t = tw;
            }
        }
    }
    if (valid) {
        vm = (jl_vararg_t *)jl_gc_alloc(ct->ptls, sizeof(jl_vararg_t), jl_vararg_type);
        jl_set_typetagof(vm, jl_vararg_tag, 0);
        vm->T = t;
        vm->N = n;
    }
    JL_GC_POP();
    return vm;
}

jl_vararg_t *jl_wrap_vararg(jl_value_t *t, jl_value_t *n, int check, int nothrow)
{
    return wrap_vararg_(t, n, check, nothrow, NULL);
}

static jl_svec_t *compute_fieldtypes_(jl_datatype_t *st JL_PROPAGATES_ROOT, void *stack, int cacheable) JL_CANSAFEPOINT
{
    assert(st->name != jl_namedtuple_typename && st->name != jl_tuple_typename);
    jl_datatype_t *wt = (jl_datatype_t*)jl_unwrap_unionall(st->name->wrapper);
    size_t i, n = jl_svec_len(wt->parameters);
    assert(n > 0 && "expected empty case to be handled during construction");
    //if (n == 0)
    //    return ((st->types = jl_emptysvec));
    if (wt->types == NULL)
        jl_errorf("cannot determine field types of incomplete type %s",
                  jl_symbol_name(st->name->name));
    jl_typeenv_t *env = (jl_typeenv_t*)alloca(n * sizeof(jl_typeenv_t));
    for (i = 0; i < n; i++) {
        env[i].var = (jl_tvar_t*)jl_svecref(wt->parameters, i);
        env[i].val = jl_svecref(st->parameters, i);
        env[i].prev = i == 0 ? NULL : &env[i - 1];
    }
    jl_value_t *held = region_pending_write(typecache_region_held(), st, 1);
    if (held != NULL)
        return (jl_svec_t*)held;
    // a lazy query continues the stack of the instantiation it is nested in, as
    // `jl_datatype_compute_super` does
    jl_typestack_t top = { st, stack ? (jl_typestack_t*)stack : typecache_stack_held(), 0 };
    jl_svec_t *types = inst_ftypes(wt->types, &env[n - 1], &top, cacheable);
    JL_GC_PUSH1(&types);
    if (!region_hold_write(typecache_region_held(), st, (jl_value_t*)types))
        types = jl_publish_fieldtypes(st, types);
    JL_GC_POP();
    return types;
}

// Set the field types of `st` unless they are already set, and return the ones
// it keeps. Field types are filled in lazily (`jl_get_fieldtypes`) and published
// by the instantiation or the loader that made `st`, so several writers can race
// on the same type: they compute equal values, and the compare-and-swap keeps a
// single winner
jl_svec_t *jl_publish_fieldtypes(jl_datatype_t *st, jl_svec_t *types)
{
    _Atomic(jl_svec_t*) *typesp = (_Atomic(jl_svec_t*)*)&st->types;
    jl_svec_t *unset = NULL;
    jl_gc_wb(st, (void*)typesp, types);
    if (jl_atomic_cmpswap(typesp, &unset, types))
        return types;
    return unset;
}

JL_DLLEXPORT jl_svec_t *jl_compute_fieldtypes(jl_datatype_t *st JL_PROPAGATES_ROOT, void *stack, int cacheable)
{
    return compute_fieldtypes_(st, stack, cacheable);
}


// Complete the deferred supertype of an instantiation of a self-referential
// definition (issue #61347): eager instantiation of a structurally increasing
// supertype graph would not terminate, so it is materialized lazily, one
// level per demand. Returns NULL if the type's definition is still in
// progress. Consumers that do not funnel through here (raw `getfield`,
// direct C reads) see either the published value or an undefined field: the
// Julia-side metadata declares `super` maybe-undefined, non-const and atomic,
// so an unfilled slot reads as `UndefRefError`, never as garbage.
JL_DLLEXPORT jl_datatype_t *jl_datatype_compute_super(jl_datatype_t *ndt JL_PROPAGATES_ROOT)
{
    // one-time lazy initialization; the acquire pairs with the releasing
    // compare-and-swap below and makes the computed object graph visible
    _Atomic(jl_datatype_t*) *superp = (_Atomic(jl_datatype_t*)*)&ndt->super;
    jl_datatype_t *super = jl_atomic_load_acquire(superp);
    if (super != NULL)
        return super;
    if (ndt->name->wrapper == NULL)
        return NULL; // too early in bootstrap
    jl_datatype_t *primarydt = (jl_datatype_t*)jl_unwrap_unionall(ndt->name->wrapper);
    if (primarydt == ndt || primarydt->super == NULL)
        return NULL; // definition in progress
    jl_value_t *held = region_pending_write(typecache_region_held(), ndt, 0);
    if (held != NULL)
        return (jl_datatype_t*)held;
    // nested in an instantiation of the current task (through the subtyping
    // of a cache lookup), continue its stack: a type that instantiation is
    // still building is found there instead of being created a second time
    jl_typestack_t stop = { ndt, typecache_stack_held(), 1 };
    jl_value_t *s = inst_declared_super(primarydt, ndt, &stop, 1, 0);
    // one that mentions a type the instantiation may still fail to make waits
    // for it in the region
    JL_GC_PUSH1(&s);
    int waits = region_hold_write(typecache_region_held(), ndt, s);
    JL_GC_POP();
    if (waits)
        return (jl_datatype_t*)s;
    // concurrent first queries compute equal values; the compare-and-swap
    // keeps a single winner
    super = NULL;
    jl_gc_wb(ndt, (void*)superp, s);
    if (jl_atomic_cmpswap(superp, &super, (jl_datatype_t*)s)) {
        super = (jl_datatype_t*)s;
    }
    return super;
}

// forcing accessor for Julia-visible supertype reads
JL_DLLEXPORT jl_value_t *jl_datatype_super(jl_datatype_t *dt)
{
    jl_datatype_t *super = jl_datatype_compute_super(dt);
    if (super == NULL)
        jl_errorf("supertype of %s is not defined yet (type definition in progress)",
                  jl_symbol_name(dt->name->name));
    return (jl_value_t*)super;
}

void jl_reinstantiate_inner_types(jl_datatype_t *t) // can throw!
{
    assert(jl_is_datatype(t));
    jl_typestack_t top = { t, NULL, 0 };
    size_t i, j, n = jl_svec_len(t->parameters);
    jl_array_t *partial = t->name->partial;
    if (partial == NULL)
        return;
    if (n == 0) {
        assert(jl_array_nrows(partial) == 0);
        return;
    }

    jl_typeenv_t *env = (jl_typeenv_t*)alloca(n * sizeof(jl_typeenv_t));
    for (i = 0; i < n; i++) {
        env[i].var = (jl_tvar_t*)jl_svecref(t->parameters, i);
        env[i].val = NULL;
        env[i].prev = i == 0 ? NULL : &env[i - 1];
    }

    for (j = 0; j < jl_array_nrows(partial); j++) {
        jl_datatype_t *ndt = (jl_datatype_t*)jl_array_ptr_ref(partial, j);
        if (ndt == NULL)
            continue;
        assert(jl_unwrap_unionall(ndt->name->wrapper) == (jl_value_t*)t);
        // one made after the supertype was declared is kept: it came from the
        // same declaration
        if (jl_atomic_load_relaxed((_Atomic(jl_datatype_t*)*)&ndt->super) == NULL) {
            jl_value_t *super = inst_declared_super(t, ndt, &top, 1, 0);
            jl_datatype_t *unset = NULL;
            jl_gc_wb(ndt, (void*)&ndt->super, super);
            jl_atomic_cmpswap((_Atomic(jl_datatype_t*)*)&ndt->super, &unset, (jl_datatype_t*)super);
        }
    }

    assert(t->types != jl_emptysvec || jl_field_names(t) == jl_emptysvec);
    for (j = 0; j < jl_array_nrows(partial); j++) {
        jl_datatype_t *ndt = (jl_datatype_t*)jl_array_ptr_ref(partial, j);
        if (ndt == NULL)
            continue;
        for (i = 0; i < n; i++)
            env[i].val = jl_svecref(ndt->parameters, i);
        // Push ndt onto the stack for cycle detection.
        // This is important for mutually recursive types: if a field type
        // of ndt references ndt (directly or indirectly), we need ndt on
        // the stack so lookup_type_stack can find it and return the
        // in-progress type instead of recursing infinitely.
        jl_typestack_t ndt_top = { ndt, &top, 0 };
        jl_publish_fieldtypes(ndt, inst_ftypes(t->types, &env[n - 1], &ndt_top, typecache_region_held() == NULL));
        if (ndt->isconcretetype) { // cacheable
            jl_compute_field_offsets(ndt);
        }
        jl_array_ptr_set(partial, j, NULL);
    }
    jl_gc_write(t->name, t->name->partial, jl_array_t, NULL);
}

// initialization -------------------------------------------------------------

static jl_tvar_t *tvar(const char *name) JL_CANSAFEPOINT
{
    return jl_new_typevar(jl_symbol(name), (jl_value_t*)jl_bottom_type,
                          (jl_value_t*)jl_any_type);
}

void export_jl_small_typeof(void)
{
    memcpy(&jl_small_typeof, &ijl_small_typeof, sizeof(jl_small_typeof));
}

#ifdef JL_LIBRARY_STATIC
// defined in static_exports.c
extern const void **const jl_static_exported_data_ptrs[];

void export_jl_sysimg_globals(void)
{
    // fill the public copies defined in static_exports.c through the table,
    // since their names are macros for the internal copies here
    size_t i = 0;
#define XX(name, type) *jl_static_exported_data_ptrs[i++] = (const void*)jl_##name;
    JL_EXPORTED_DATA_POINTERS(XX)
    JL_CONST_GLOBAL_VARS(XX)
#undef XX
}
#else
void export_jl_sysimg_globals(void)
{
    // Use jl_dlsym to reference "jl_"#name from the jl_libjulia_handle instead
    // of directly making a symbol from it which will have problems with cpp
#define XX(name, type) do { \
        type *ptr; \
        jl_dlsym(jl_libjulia_handle, "jl_" #name, (void**)&ptr, 1, 0); \
        *ptr = jl_##name; \
    } while (0);
    JL_EXPORTED_DATA_POINTERS(XX)
#undef XX
#define YY(name, type) do { \
        type *ptr; \
        jl_dlsym(jl_libjulia_handle, "jl_" #name, (void**)&ptr, 1, 0); \
        *ptr = jl_##name; \
    } while (0);
    JL_CONST_GLOBAL_VARS(YY)
#undef YY
}
#endif

#define XX(name) \
    ijl_small_typeof[(jl_##name##_tag << 4) / sizeof(*ijl_small_typeof)] = jl_##name##_type; \
    jl_##name##_type->smalltag = jl_##name##_tag;
void jl_init_types(void) JL_GC_DISABLED
{
    // n.b. When adding fields to existing types, update const/atomic field bitvectors carefully:
    //   Bits represent field positions (0-indexed).
    //   Prefer `0b` notation (converting if needed from `0x`).
    //   Only set bits for new atomic/const fields, shift existing bits as needed.
    //   Only 32 bits in one field, overflow goes into the next field.

    jl_module_t *core = NULL; // will need to be assigned later
    jl_task_t *ct = jl_current_task;

    // create base objects
    jl_datatype_type = jl_new_uninitialized_datatype();
    XX(datatype);
    jl_typename_type = jl_new_uninitialized_datatype();
    jl_symbol_type = jl_new_uninitialized_datatype();
    XX(symbol);
    jl_simplevector_type = jl_new_uninitialized_datatype();
    XX(simplevector);
    jl_methcache_type = jl_new_uninitialized_datatype();
    jl_methtable_type = jl_new_uninitialized_datatype();
    jl_method_table = jl_new_method_table(jl_symbol("methodtable"), core);

    jl_emptysvec = (jl_svec_t*)jl_gc_permobj(ct->ptls, sizeof(void*), jl_simplevector_type, 0);
    jl_set_typetagof(jl_emptysvec, jl_simplevector_tag, GC_OLD_MARKED);
    jl_svec_set_len_unsafe(jl_emptysvec, 0);

    jl_any_type = (jl_datatype_t*)jl_new_abstracttype((jl_value_t*)jl_symbol("Any"), core, NULL, jl_emptysvec);
    jl_any_type->super = jl_any_type;

    jl_anytype_type = jl_new_abstracttype((jl_value_t*)jl_symbol("AnyType"), core, jl_any_type, jl_emptysvec);
    jl_type_type = (jl_unionall_t*)jl_any_type; // replaced with TypeEq(T) where T below
    jl_type_typename = NULL;

    // initialize them. lots of cycles.
    // NOTE: types are not actually mutable, but we want to ensure they are heap-allocated with stable addresses
    jl_datatype_type->name = jl_new_typename_in(jl_symbol("DataType"), core, 0, 1);
    jl_datatype_type->name->wrapper = (jl_value_t*)jl_datatype_type;
    jl_datatype_type->super = jl_anytype_type;
    jl_datatype_type->parameters = jl_emptysvec;
    // only `name` is guaranteed initialized: `super` of a deferred
    // self-referential instantiation stays NULL until
    // `jl_datatype_compute_super` fills it (reads then see an undefined
    // field, matching the interpreter), and `types`/`layout` are lazy too
    jl_datatype_type->name->n_uninitialized = 8 - 1;
    jl_datatype_type->name->names = jl_perm_symsvec(8,
            "name",
            "super",
            "parameters",
            "types",
            "instance",
            "layout",
            "hash",
            "flags"); // "hasfreetypevars", "isconcretetype", "isdispatchtuple", "isbitstype", "zeroinit", "has_concrete_subtype", "maybe_subtype_of_cache"
    jl_datatype_type->types = jl_svec(8,
            jl_typename_type,
            jl_datatype_type,
            jl_simplevector_type,
            jl_simplevector_type,
            jl_any_type, // instance
            jl_any_type /*jl_voidpointer_type*/,
            jl_any_type /*jl_int32_type*/,
            jl_any_type /*jl_uint16_type*/);
    const static uint32_t datatype_constfields[1] = { 0x00000055 }; // (1<<0)|(1<<2)|(1<<4)|(1<<6)
    const static uint32_t datatype_atomicfields[1] = { 0x0000002a }; // (1<<1)|(1<<3)|(1<<5)
    jl_datatype_type->name->constfields = datatype_constfields;
    jl_datatype_type->name->atomicfields = datatype_atomicfields;
    jl_precompute_memoized_dt(jl_datatype_type, 1);

    jl_typename_type->name = jl_new_typename_in(jl_symbol("TypeName"), core, 0, 1);
    jl_typename_type->name->wrapper = (jl_value_t*)jl_typename_type;
    jl_typename_type->super = jl_any_type;
    jl_typename_type->parameters = jl_emptysvec;
    jl_typename_type->name->n_uninitialized = 19 - 2;
    jl_typename_type->name->names = jl_perm_symsvec(19, "name", "module", "singletonname",
                                                    "names", "atomicfields", "constfields",
                                                    "wrapper", "Typeofwrapper", "cache", "linearcache",
                                                    "partial", "hash", "max_args", "n_uninitialized",
                                                    "flags", // "abstract", "mutable", "mayinlinealloc",
                                                    "cache_entry_count", "max_methods", "constprop_heuristic",
                                                    "concrete_only");
    const static uint32_t typename_constfields[1]  = { 0b0000110100001001011 }; // TODO: put back atomicfields and constfields in this list
    const static uint32_t typename_atomicfields[1] = { 0b0001001001110000000 };
    jl_typename_type->name->constfields = typename_constfields;
    jl_typename_type->name->atomicfields = typename_atomicfields;
    jl_precompute_memoized_dt(jl_typename_type, 1);
    jl_typename_type->types = jl_svec(19, jl_symbol_type, jl_any_type /*jl_module_type*/, jl_symbol_type,
                                      jl_simplevector_type,
                                      jl_any_type/*jl_voidpointer_type*/, jl_any_type/*jl_voidpointer_type*/,
                                      jl_type_type, jl_simplevector_type, jl_simplevector_type,
                                      jl_simplevector_type, jl_any_type,
                                      jl_any_type /*jl_long_type*/,
                                      jl_any_type /*jl_int32_type*/,
                                      jl_any_type /*jl_int32_type*/,
                                      jl_any_type /*jl_uint8_type*/,
                                      jl_any_type /*jl_uint8_type*/,
                                      jl_any_type /*jl_uint8_type*/,
                                      jl_any_type /*jl_uint8_type*/,
                                      jl_any_type /*jl_bool_type*/);

    jl_methcache_type->name = jl_new_typename_in(jl_symbol("MethodCache"), core, 0, 1);
    jl_methcache_type->name->wrapper = (jl_value_t*)jl_methcache_type;
    jl_methcache_type->super = jl_any_type;
    jl_methcache_type->parameters = jl_emptysvec;
    jl_methcache_type->name->n_uninitialized = 4 - 2;
    jl_methcache_type->name->names = jl_perm_symsvec(4, "leafcache", "cache", "", "");
    const static uint32_t methcache_atomicfields[1] = { 0b1111 };
    jl_methcache_type->name->atomicfields = methcache_atomicfields;
    jl_precompute_memoized_dt(jl_methcache_type, 1);
    jl_methcache_type->types = jl_svec(4, jl_any_type, jl_any_type, jl_any_type/*voidpointer*/, jl_any_type/*int32*/);

    jl_methtable_type->name = jl_new_typename_in(jl_symbol("MethodTable"), core, 0, 1);
    jl_methtable_type->name->wrapper = (jl_value_t*)jl_methtable_type;
    jl_methtable_type->super = jl_any_type;
    jl_methtable_type->parameters = jl_emptysvec;
    jl_methtable_type->name->n_uninitialized = 0;
    jl_methtable_type->name->names = jl_perm_symsvec(6, "defs", "cache", "name", "module", "backedges", "contributors");
    const static uint32_t methtable_constfields[1] = { 0b001110 };
    const static uint32_t methtable_atomicfields[1] = { 0b100001 };
    jl_methtable_type->name->constfields = methtable_constfields;
    jl_methtable_type->name->atomicfields = methtable_atomicfields;
    jl_precompute_memoized_dt(jl_methtable_type, 1);
    jl_methtable_type->types = jl_svec(6, jl_any_type, jl_methcache_type, jl_symbol_type, jl_any_type /*jl_module_type*/, jl_any_type, jl_any_type);

    jl_symbol_type->name = jl_new_typename_in(jl_symbol("Symbol"), core, 0, 1);
    jl_symbol_type->name->wrapper = (jl_value_t*)jl_symbol_type;
    jl_symbol_type->super = jl_any_type;
    jl_symbol_type->parameters = jl_emptysvec;
    jl_symbol_type->name->n_uninitialized = 0;
    jl_symbol_type->name->names = jl_emptysvec;
    jl_symbol_type->types = jl_emptysvec;
    jl_precompute_memoized_dt(jl_symbol_type, 1);

    jl_simplevector_type->name = jl_new_typename_in(jl_symbol("SimpleVector"), core, 0, 1);
    jl_simplevector_type->name->wrapper = (jl_value_t*)jl_simplevector_type;
    jl_simplevector_type->super = jl_any_type;
    jl_simplevector_type->parameters = jl_emptysvec;
    jl_simplevector_type->name->n_uninitialized = 0;
    jl_simplevector_type->name->names = jl_emptysvec;
    jl_simplevector_type->types = jl_emptysvec;
    jl_precompute_memoized_dt(jl_simplevector_type, 1);

    // now they can be used to create the remaining base kinds and types
    jl_nothing_type = jl_new_datatype(jl_symbol("Nothing"), core, jl_any_type, jl_emptysvec,
                                      jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 0, 0);
    XX(nothing);
    jl_nothing_type->instance = jl_nothing;

    jl_tvar_type = jl_new_datatype(jl_symbol("TypeVar"), core, jl_any_type, jl_emptysvec,
                                   jl_perm_symsvec(3, "name", "lb", "ub"),
                                   jl_svec(3, jl_symbol_type, jl_any_type, jl_any_type),
                                   jl_emptysvec, 0, 1, 3);
    XX(tvar);
    const static uint32_t tvar_constfields[1] = { 0x00000007 }; // all fields are constant, even though TypeVar itself has identity
    jl_tvar_type->name->constfields = tvar_constfields;

    jl_typeofbottom_type = jl_new_datatype(jl_symbol("TypeofBottom"), core, jl_anytype_type, jl_emptysvec,
                                           jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 0, 0);
    XX(typeofbottom);
    jl_bottom_type = jl_gc_permobj(ct->ptls, 0, jl_typeofbottom_type, 0);
    jl_set_typetagof(jl_bottom_type, jl_typeofbottom_tag, GC_OLD_MARKED);
    jl_typeofbottom_type->instance = jl_bottom_type;

    jl_unionall_type = jl_new_datatype(jl_symbol("UnionAll"), core, jl_anytype_type, jl_emptysvec,
                                       jl_perm_symsvec(2, "var", "body"),
                                       jl_svec(2, jl_tvar_type, jl_any_type),
                                       jl_emptysvec, 0, 0, 2);
    XX(unionall);
    // It seems like we probably usually end up needing the box for kinds (often used in an Any context), so force it to exist
    jl_unionall_type->name->mayinlinealloc = 0;

    jl_uniontype_type = jl_new_datatype(jl_symbol("Union"), core, jl_anytype_type, jl_emptysvec,
                                        jl_perm_symsvec(2, "a", "b"),
                                        jl_svec(2, jl_any_type, jl_any_type),
                                        jl_emptysvec, 0, 0, 2);
    XX(uniontype);
    // It seems like we probably usually end up needing the box for kinds (often used in an Any context), so force it to exist
    jl_uniontype_type->name->mayinlinealloc = 0;

    jl_value_t *anytype_or_typevar_types[2] = { (jl_value_t*)jl_anytype_type, (jl_value_t*)jl_tvar_type };
    jl_value_t *kind_or_typevar_type = jl_type_union(anytype_or_typevar_types, 2);
    jl_svecset(jl_tvar_type->types, 1, kind_or_typevar_type);
    jl_svecset(jl_tvar_type->types, 2, kind_or_typevar_type);

    // Internal-use-only kind dual to Union (see #61917). Not registered as a
    // small_typeof tag: it is recognized by identity (jl_is_intersecttype) and
    // only ever lives transiently inside the subtyping algorithm.
    jl_intersect_type = jl_new_datatype(jl_symbol("Intersect"), core, jl_anytype_type, jl_emptysvec,
                                        jl_perm_symsvec(2, "a", "b"),
                                        jl_svec(2, jl_any_type, jl_any_type),
                                        jl_emptysvec, 0, 0, 2);
    jl_intersect_type->name->mayinlinealloc = 0;

    // `T` must be declared `Any`: jl_valid_type_param admits non-type values
    // (`Type{1}`), so a narrower field type would be unsound (#62897)
    jl_typeeq_type = jl_new_datatype(jl_symbol("TypeEq"), core, jl_anytype_type, jl_emptysvec,
                                     jl_perm_symsvec(1, "T"),
                                     jl_svec(1, jl_any_type),
                                     jl_emptysvec, 0, 0, 1);
    XX(typeeq);
    // It seems like we probably usually end up needing the box for kinds (often used in an Any context), so force it to exist
    jl_typeeq_type->name->mayinlinealloc = 0;
    jl_typeeq_type->ismutationfree = 1;
    jl_type_typename = jl_typeeq_type->name;

    jl_tvar_t *tttvar = tvar("T");
    jl_value_t *typeeq_body = (jl_value_t*)jl_wrap_Type((jl_value_t*)tttvar);
    JL_GC_PUSH1(&typeeq_body);
    jl_type_type = (jl_unionall_t*)jl_new_struct(jl_unionall_type, tttvar, typeeq_body);
    JL_GC_POP();
    jl_wrap_Type(jl_bottom_type);

    // egality-based dual of `TypeEq`: the only instance of `TypeEgal{T}` is `T`
    // itself, matched by `===` (see `boot.jl`)
    jl_typeegal_type = jl_new_datatype(jl_symbol("TypeEgal"), core, jl_anytype_type, jl_emptysvec,
                                       jl_perm_symsvec(1, "T"),
                                       jl_svec(1, jl_anytype_type),
                                       jl_emptysvec, 0, 0, 1);
    XX(typeegal);
    jl_typeegal_type->name->mayinlinealloc = 0;
    jl_typeegal_type->ismutationfree = 1;

    jl_vararg_type = jl_new_datatype(jl_symbol("TypeofVararg"), core, jl_any_type, jl_emptysvec,
                                            jl_perm_symsvec(2, "T", "N"),
                                            jl_svec(2, jl_any_type, jl_any_type),
                                            jl_emptysvec, 0, 0, 0);
    XX(vararg);
    // It seems like we probably usually end up needing the box for kinds (often used in an Any context), so force it to exist
    jl_vararg_type->name->mayinlinealloc = 0;
    jl_vararg_type->ismutationfree = 1;

    jl_svec_t *anytuple_params = jl_svec(1, jl_wrap_vararg((jl_value_t*)jl_any_type, (jl_value_t*)NULL, 0, 0));
    jl_anytuple_type = jl_new_datatype(jl_symbol("Tuple"), core, jl_any_type, anytuple_params,
                                       jl_emptysvec, anytuple_params, jl_emptysvec, 0, 0, 0);
    jl_tuple_typename = jl_anytuple_type->name;
    // fix some miscomputed values, since we didn't know this was going to be a Tuple in jl_precompute_memoized_dt
    jl_tuple_typename->wrapper = (jl_value_t*)jl_anytuple_type; // remove UnionAll wrappers
    jl_anytuple_type->isconcretetype = 0;
    jl_anytuple_type->maybe_subtype_of_cache = 0;
    jl_anytuple_type->layout = NULL;

    jl_typeofbottom_type->super = jl_anytype_type;
    jl_emptytuple_type = (jl_datatype_t*)jl_apply_tuple_type(jl_emptysvec, 0);
    jl_emptytuple = jl_gc_permobj(ct->ptls, 0, jl_emptytuple_type, 0);
    jl_emptytuple_type->instance = jl_emptytuple;

    // non-primitive definitions follow
    jl_int32_type = jl_new_primitivetype((jl_value_t*)jl_symbol("Int32"), core,
                                         jl_any_type, jl_emptysvec, 32);
    XX(int32);
    jl_int64_type = jl_new_primitivetype((jl_value_t*)jl_symbol("Int64"), core,
                                         jl_any_type, jl_emptysvec, 64);
    XX(int64);
    jl_uint32_type = jl_new_primitivetype((jl_value_t*)jl_symbol("UInt32"), core,
                                          jl_any_type, jl_emptysvec, 32);
    XX(uint32);
    jl_uint64_type = jl_new_primitivetype((jl_value_t*)jl_symbol("UInt64"), core,
                                          jl_any_type, jl_emptysvec, 64);
    XX(uint64);
    jl_uint8_type = jl_new_primitivetype((jl_value_t*)jl_symbol("UInt8"), core,
                                         jl_any_type, jl_emptysvec, 8);
    XX(uint8);
    jl_uint16_type = jl_new_primitivetype((jl_value_t*)jl_symbol("UInt16"), core,
                                          jl_any_type, jl_emptysvec, 16);
    XX(uint16);

    jl_ssavalue_type = jl_new_datatype(jl_symbol("SSAValue"), core, jl_any_type, jl_emptysvec,
                                       jl_perm_symsvec(1, "id"),
                                       jl_svec1(jl_long_type),
                                       jl_emptysvec, 0, 0, 1);
    XX(ssavalue);

    jl_slotnumber_type = jl_new_datatype(jl_symbol("SlotNumber"), core, jl_any_type, jl_emptysvec,
                                         jl_perm_symsvec(1, "id"),
                                         jl_svec1(jl_long_type),
                                         jl_emptysvec, 0, 0, 1);
    XX(slotnumber);

    jl_argument_type = jl_new_datatype(jl_symbol("Argument"), core, jl_any_type, jl_emptysvec,
                                       jl_perm_symsvec(1, "n"),
                                       jl_svec1(jl_long_type),
                                       jl_emptysvec, 0, 0, 1);
    XX(argument);

    jl_bool_type = NULL;
    jl_bool_type = jl_new_primitivetype((jl_value_t*)jl_symbol("Bool"), core,
                                        jl_any_type, jl_emptysvec, 8);
    XX(bool);
    jl_false = jl_permbox8(jl_bool_type, jl_bool_tag, 0);
    jl_true  = jl_permbox8(jl_bool_type, jl_bool_tag, 1);

    jl_abstractstring_type = jl_new_abstracttype((jl_value_t*)jl_symbol("AbstractString"), core, jl_any_type, jl_emptysvec);
    jl_string_type = jl_new_datatype(jl_symbol("String"), core, jl_abstractstring_type, jl_emptysvec,
                                     jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 1, 0);
    XX(string);
    jl_string_type->instance = NULL;
    jl_compute_field_offsets(jl_string_type); // re-compute now that we assigned jl_string_type
    jl_an_empty_string = jl_pchar_to_string("\0", 1);
    *(size_t*)jl_an_empty_string = 0;

    jl_typemap_level_type =
        jl_new_datatype(jl_symbol("TypeMapLevel"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(6,
                            "arg1",
                            "targ",
                            "name1",
                            "tname",
                            "list",
                            "any"),
                        jl_svec(6,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type),
                        jl_emptysvec,
                        0, 1, 6);
    const static uint32_t typemap_level_atomicfields[1] = { 0x0000003f }; // (1<<0)|(1<<1)|(1<<2)|(1<<3)|(1<<4)|(1<<5)
    jl_typemap_level_type->name->atomicfields = typemap_level_atomicfields;

    jl_typemap_entry_type =
        jl_new_datatype(jl_symbol("TypeMapEntry"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(10,
                            "next",
                            "sig",
                            "simplesig",
                            "guardsigs",
                            "min_world",
                            "max_world",
                            "func",
                            "isleafsig",
                            "issimplesig",
                            "va"),
                        jl_svec(10,
                            jl_any_type, // Union{TypeMapEntry, Nothing}
                            jl_type_type, // TupleType
                            jl_any_type, // TupleType
                            jl_any_type, // SimpleVector{TupleType}
                            jl_ulong_type, // UInt
                            jl_ulong_type, // UInt
                            jl_any_type, // Any
                            jl_bool_type,
                            jl_bool_type,
                            jl_bool_type),
                        jl_emptysvec,
                        0, 1, 4);
    const static uint32_t typemap_entry_constfields[1] = { 0x000003ce }; // (1<<1)|(1<<2)|(1<<3)|(1<<6)|(1<<7)|(1<<8)|(1<<9)
    const static uint32_t typemap_entry_atomicfields[1] = { 0x00000031 }; // (1<<0)|(1<<4)|(1<<5)
    jl_typemap_entry_type->name->constfields = typemap_entry_constfields;
    jl_typemap_entry_type->name->atomicfields = typemap_entry_atomicfields;

    jl_function_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Function"), core, jl_any_type, jl_emptysvec);
    jl_builtin_type  = jl_new_abstracttype((jl_value_t*)jl_symbol("Builtin"), core, jl_function_type, jl_emptysvec);

    jl_svec_t *tv;

    jl_module_type =
        jl_new_datatype(jl_symbol("Module"), core, jl_any_type, jl_emptysvec,
                        jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 1, 0);
    XX(module);
    assert(jl_module_type->instance == NULL);
    jl_compute_field_offsets(jl_module_type);

    jl_binding_partition_type =
        jl_new_datatype(jl_symbol("BindingPartition"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(5, "restriction", "min_world", "max_world", "next", "kind"),
                        jl_svec(5, jl_any_type,
                        jl_ulong_type, jl_ulong_type, jl_any_type/*Union{jl_binding_partition_type, jl_binding_type}*/, jl_ulong_type),
                        jl_emptysvec, 0, 1, 0);
    const static uint32_t binding_partition_atomicfields[] = { 0b01110 }; // Set fields 2, 3, 4 as atomic
    jl_binding_partition_type->name->atomicfields = binding_partition_atomicfields;
    const static uint32_t binding_partition_constfields[]  = { 0b10001 }; // Set fields 1, 5 as constant
    jl_binding_partition_type->name->constfields = binding_partition_constfields;

    jl_binding_type =
        jl_new_datatype(jl_symbol("Binding"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(5, "globalref", "value", "partitions", "backedges", "flags"),
                        jl_svec(5, jl_any_type/*jl_globalref_type*/, jl_any_type, jl_binding_partition_type,
                                   jl_any_type, jl_uint8_type),
                        jl_emptysvec, 0, 1, 0);
    const static uint32_t binding_atomicfields[] = { 0x0016 }; // Set fields 2, 3, 5 as atomic
    jl_binding_type->name->atomicfields = binding_atomicfields;
    const static uint32_t binding_constfields[] = { 0x0001 }; // Set fields 1 as constant
    jl_binding_type->name->constfields = binding_constfields;

    jl_globalref_type =
        jl_new_datatype(jl_symbol("GlobalRef"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(3, "mod", "name", "binding"),
                        jl_svec(3, jl_module_type, jl_symbol_type, jl_binding_type),
                        jl_emptysvec, 0, 0, 3);
    jl_globalref_type->name->mayinlinealloc = 0; // not at all worthwhile, since the only constructor returns a boxed object
    XX(globalref);

    jl_core_module = jl_new_module(jl_symbol("Core"), NULL);

    tv = jl_svec1(tvar("Backend"));
    jl_addrspace_typename =
        jl_new_primitivetype((jl_value_t*)jl_symbol("AddrSpace"), core, jl_any_type, tv, 8)->name;
    jl_addrspace_type = (jl_unionall_t*)jl_addrspace_typename->wrapper;
    jl_addrspacecore_type = (jl_datatype_t*)jl_apply_type1((jl_value_t*)jl_addrspace_type, (jl_value_t*)jl_core_module);
    XX(addrspacecore);
    jl_value_t *cpumem = jl_permbox8(jl_addrspacecore_type, jl_addrspacecore_tag, 0);

    tv = jl_svec1(tvar("T"));
    jl_ref_type = (jl_unionall_t*)
        jl_new_abstracttype((jl_value_t*)jl_symbol("Ref"), core, jl_any_type, tv)->name->wrapper;

    tv = jl_svec1(tvar("T"));
    jl_pointer_typename =
        jl_new_primitivetype((jl_value_t*)jl_symbol("Ptr"), core,
                             (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_ref_type, jl_svec_data(tv), 1),
                             tv,
                             sizeof(void*) * 8)->name;
    jl_pointer_type = (jl_unionall_t*)jl_pointer_typename->wrapper;
    jl_value_t *pointer_void = jl_apply_type1((jl_value_t*)jl_pointer_type, (jl_value_t*)jl_nothing_type);
    jl_voidpointer_type = (jl_datatype_t*)pointer_void;

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_abstractarray_type = (jl_unionall_t*)
        jl_new_abstracttype((jl_value_t*)jl_symbol("AbstractArray"), core,
                            jl_any_type, tv)->name->wrapper;

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_densearray_type = (jl_unionall_t*)
        jl_new_abstracttype((jl_value_t*)jl_symbol("DenseArray"), core,
                            (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_abstractarray_type, jl_svec_data(tv), 2),
                            tv)->name->wrapper;

    tv = jl_svec(3, tvar("isatomic"), tvar("T"), tvar("addrspace"));
    jl_datatype_t *jl_memory_supertype = (jl_datatype_t*)jl_apply_type2((jl_value_t*)jl_densearray_type, jl_svecref(tv, 1), jl_box_long(1));
    jl_datatype_t *memory_datatype =
        jl_new_datatype(jl_symbol("GenericMemory"), core, jl_memory_supertype, tv,
                        jl_perm_symsvec(2, "length", "ptr"),
                        jl_svec(2, jl_long_type, pointer_void),
                        jl_emptysvec, 0, 1, 2);
    jl_genericmemory_typename = memory_datatype->name;
    jl_genericmemory_type = (jl_unionall_t*)jl_genericmemory_typename->wrapper;
    const static uint32_t memory_constfields[1] = { 0x00000003 }; // (1<<1)|(1<<0)
    memory_datatype->name->constfields = memory_constfields;
    memory_datatype->ismutationfree = 0;

    jl_datatype_t *jl_memoryref_supertype = (jl_datatype_t*)jl_apply_type1((jl_value_t*)jl_ref_type, jl_svecref(tv, 1));
    jl_datatype_t *memoryref_datatype =
        jl_new_datatype(jl_symbol("GenericMemoryRef"), core, jl_memoryref_supertype, tv,
                        jl_perm_symsvec(2, "ptr_or_offset", "mem"),
                        jl_svec(2, pointer_void, memory_datatype),
                        jl_emptysvec, 0, 0, 2);
    jl_genericmemoryref_typename = memoryref_datatype->name;
    jl_genericmemoryref_type = (jl_unionall_t*)jl_genericmemoryref_typename->wrapper;
    memoryref_datatype->ismutationfree = 0;

    jl_memory_any_type = jl_apply_type3((jl_value_t*)jl_genericmemory_type, (jl_value_t*)jl_not_atomic_sym, (jl_value_t*)jl_any_type, cpumem);
    jl_memory_uint8_type = jl_apply_type3((jl_value_t*)jl_genericmemory_type, (jl_value_t*)jl_not_atomic_sym, (jl_value_t*)jl_uint8_type, cpumem);
    jl_memory_uint16_type = jl_apply_type3((jl_value_t*)jl_genericmemory_type, (jl_value_t*)jl_not_atomic_sym, (jl_value_t*)jl_uint16_type, cpumem);
    jl_memory_uint32_type = jl_apply_type3((jl_value_t*)jl_genericmemory_type, (jl_value_t*)jl_not_atomic_sym, (jl_value_t*)jl_uint32_type, cpumem);
    jl_memory_uint64_type = jl_apply_type3((jl_value_t*)jl_genericmemory_type, (jl_value_t*)jl_not_atomic_sym, (jl_value_t*)jl_uint64_type, cpumem);
    jl_memoryref_any_type = jl_apply_type3((jl_value_t*)jl_genericmemoryref_type, (jl_value_t*)jl_not_atomic_sym, (jl_value_t*)jl_any_type, cpumem);
    jl_memoryref_uint8_type = jl_apply_type3((jl_value_t*)jl_genericmemoryref_type, (jl_value_t*)jl_not_atomic_sym, (jl_value_t*)jl_uint8_type, cpumem);

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_array_typename = jl_new_datatype(jl_symbol("Array"), core,
                        (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_densearray_type, jl_svec_data(tv), 2),
                        tv,
                        jl_perm_symsvec(2, "ref", "size"),
                        jl_svec(2,
                            jl_apply_type3((jl_value_t*)jl_genericmemoryref_type, (jl_value_t*)jl_not_atomic_sym, jl_svecref(tv, 0), cpumem),
                            jl_apply_type1((jl_value_t*)jl_tuple_type, (jl_value_t*)jl_wrap_vararg((jl_value_t*)jl_long_type, jl_svecref(tv, 1), 0, 0))),
                            jl_emptysvec, 0, 1, 2)->name;
    jl_array_type = (jl_unionall_t*)jl_array_typename->wrapper;

    jl_array_any_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_any_type, jl_box_long(1));
    jl_array_symbol_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_symbol_type, jl_box_long(1));
    jl_array_uint8_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_uint8_type, jl_box_long(1));
    jl_array_uint32_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_uint32_type, jl_box_long(1));
    jl_array_int32_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_int32_type, jl_box_long(1));
    jl_array_uint64_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_uint64_type, jl_box_long(1));
    jl_an_empty_vec_any = (jl_value_t*)jl_alloc_vec_any(0); // used internally
    jl_an_empty_memory_any = (jl_value_t*)jl_alloc_memory_any(0); // used internally

    // finish initializing module Core
    core = jl_core_module;
    jl_method_table->module = core;
    jl_atomic_store_relaxed(&jl_method_table->cache->leafcache, (jl_genericmemory_t*)jl_an_empty_memory_any);
    jl_method_table->backedges = (jl_genericmemory_t*)jl_an_empty_memory_any;
    jl_atomic_store_relaxed(&jl_method_table->contributors, (jl_genericmemory_t*)jl_an_empty_memory_any);
    jl_atomic_store_relaxed(&core->bindingkeyset, (jl_genericmemory_t*)jl_an_empty_memory_any);
    // export own name, so "using Foo" makes "Foo" itself visible
    jl_set_initial_const(core, core->name, (jl_value_t*)core, 1);
    jl_set_initial_const(core, jl_symbol("CPU"), (jl_value_t*)cpumem, 0);
    core = NULL;

    jl_expr_type =
        jl_new_datatype(jl_symbol("Expr"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "head", "args"),
                        jl_svec(2, jl_symbol_type, jl_array_any_type),
                        jl_emptysvec, 0, 1, 2);

    jl_value_t *symornothing[2] = { (jl_value_t*)jl_symbol_type, (jl_value_t*)jl_nothing_type };
    jl_linenumbernode_type =
        jl_new_datatype(jl_symbol("LineNumberNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "line", "file"),
                        jl_svec(2, jl_long_type, jl_type_union(symornothing, 2)),
                        jl_emptysvec, 0, 0, 2);

    jl_lineinfonode_type =
        jl_new_datatype(jl_symbol("LegacyLineInfoNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(3, "file", "line", "inlined_at"),
                        jl_svec(3, jl_symbol_type, jl_int32_type, jl_int32_type),
                        jl_emptysvec, 0, 0, 3);

    jl_gotonode_type =
        jl_new_datatype(jl_symbol("GotoNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "label"),
                        jl_svec(1, jl_long_type),
                        jl_emptysvec, 0, 0, 1);
    XX(gotonode);

    jl_gotoifnot_type =
        jl_new_datatype(jl_symbol("GotoIfNot"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "cond", "dest"),
                        jl_svec(2, jl_any_type, jl_long_type),
                        jl_emptysvec, 0, 0, 2);
    XX(gotoifnot);

    jl_enternode_type =
        jl_new_datatype(jl_symbol("EnterNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "catch_dest", "scope"),
                        jl_svec(2, jl_long_type, jl_any_type),
                        jl_emptysvec, 0, 0, 1);
    XX(enternode);

    jl_returnnode_type =
        jl_new_datatype(jl_symbol("ReturnNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "val"),
                        jl_svec(1, jl_any_type),
                        jl_emptysvec, 0, 0, 0);
    XX(returnnode);

    jl_pinode_type =
        jl_new_datatype(jl_symbol("PiNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "val", "typ"),
                        jl_svec(2, jl_any_type, jl_any_type),
                        jl_emptysvec, 0, 0, 2);
    XX(pinode);

    jl_phinode_type =
        jl_new_datatype(jl_symbol("PhiNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "edges", "values"),
                        jl_svec(2, jl_array_int32_type, jl_array_any_type),
                        jl_emptysvec, 0, 0, 2);
    XX(phinode);

    jl_phicnode_type =
        jl_new_datatype(jl_symbol("PhiCNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "values"),
                        jl_svec(1, jl_array_any_type),
                        jl_emptysvec, 0, 0, 1);
    XX(phicnode);

    jl_upsilonnode_type =
        jl_new_datatype(jl_symbol("UpsilonNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "val"),
                        jl_svec(1, jl_any_type),
                        jl_emptysvec, 0, 0, 0);
    XX(upsilonnode);

    jl_quotenode_type =
        jl_new_datatype(jl_symbol("QuoteNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "value"),
                        jl_svec(1, jl_any_type),
                        jl_emptysvec, 0, 0, 1);
    XX(quotenode);

    jl_newvarnode_type =
        jl_new_datatype(jl_symbol("NewvarNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "slot"),
                        jl_svec(1, jl_slotnumber_type),
                        jl_emptysvec, 0, 0, 1);

    jl_debuginfo_type =
        jl_new_datatype(jl_symbol("DebugInfo"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(4,
                            "def",
                            "linetable",
                            "edges",
                            "codelocs"),
                        jl_svec(4,
                            jl_any_type, // union(jl_method_instance_type, jl_method_type, jl_symbol_type),
                            jl_any_type, // union(jl_nothing, jl_debuginfo_type)
                            jl_simplevector_type, // memory{debuginfo}
                            jl_string_type),
                        jl_emptysvec, 0, 0, 4);
    jl_debuginfo_type->name->mayinlinealloc = 0;

    jl_code_info_type =
        jl_new_datatype(jl_symbol("CodeInfo"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(23,
                            "code",
                            "debuginfo",
                            "ssavaluetypes",
                            "ssaflags",
                            "slotnames",
                            "slotflags",
                            "slottypes",
                            "rettype",
                            "parent",
                            "edges",
                            "min_world",
                            "max_world",
                            "method_for_inference_limit_heuristics",
                            "nargs",
                            "propagate_inbounds",
                            "has_fcall",
                            "has_image_globalref",
                            "nospecializeinfer",
                            "isva",
                            "inlining",
                            "constprop",
                            "purity",
                            "inlining_cost"),
                        jl_svec(23,
                            jl_array_any_type,
                            jl_debuginfo_type,
                            jl_any_type,
                            jl_array_uint32_type,
                            jl_array_symbol_type,
                            jl_array_uint8_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type, // prefers svec, but tolerates Vector{Any}
                            jl_ulong_type,
                            jl_ulong_type,
                            jl_any_type,
                            jl_ulong_type,
                            jl_bool_type,
                            jl_bool_type,
                            jl_bool_type,
                            jl_bool_type,
                            jl_bool_type,
                            jl_uint8_type,
                            jl_uint8_type,
                            jl_uint16_type,
                            jl_uint16_type),
                        jl_emptysvec,
                        0, 1, 22);

    jl_method_type =
        jl_new_datatype(jl_symbol("Method"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(33,
                            "name",
                            "module",
                            "file",
                            "line",
                            "dispatch_status", // atomic
                            "interferences", // atomic
                            "primary_world", // atomic
                            "sig",
                            "specializations", // !const
                            "speckeyset", // !const
                            "slot_syms",
                            "external_mt",
                            "source", // !const
                            "debuginfo", // !const
                            "unspecialized", // !const
                            "generator", // !const
                            "roots", // !const
                            "root_blocks", // !const
                            "nroots_sysimg",
                            "ccallable", // !const
                            "invokes", // !const
                            "recursion_relation", // !const
                            "nargs",
                            "called",
                            "nospecialize",
                            "nkw",
                            "isva",
                            "is_for_opaque_closure",
                            "nospecializeinfer",
                            "did_scan_source",
                            "constprop",
                            "max_varargs",
                            "purity"),
                        jl_svec(33,
                            jl_symbol_type,
                            jl_module_type,
                            jl_symbol_type,
                            jl_int32_type,
                            jl_uint8_type,
                            jl_memory_any_type,
                            jl_ulong_type,
                            jl_type_type,
                            jl_any_type, // union(jl_simplevector_type, jl_method_instance_type),
                            jl_genericmemory_type, // union(jl_memory_uint8_type, jl_memory_uint16_type, jl_memory_uint32_type, jl_memory_uint64_type, jl_memory_any_type)
                            jl_string_type,
                            jl_any_type,
                            jl_any_type,
                            jl_debuginfo_type,
                            jl_any_type, // jl_method_instance_type
                            jl_any_type,
                            jl_array_any_type,
                            jl_array_uint64_type,
                            jl_int32_type,
                            jl_simplevector_type,
                            jl_any_type,
                            jl_any_type,
                            jl_int32_type,
                            jl_int32_type,
                            jl_int32_type,
                            jl_int32_type,
                            jl_bool_type,
                            jl_bool_type,
                            jl_bool_type,
                            jl_uint8_type,
                            jl_uint8_type,
                            jl_uint8_type,
                            jl_uint16_type),
                        jl_emptysvec,
                        0, 1, 10);
    //const static uint32_t method_constfields[] = { 0b0, 0b0 }; // (1<<0)|(1<<1)|(1<<2)|(1<<3)|(1<<6)|(1<<9)|(1<<10)|(1<<17)|(1<<21)|(1<<22)|(1<<23)|(1<<24)|(1<<25)|(1<<26)|(1<<27)|(1<<28)|(1<<29)|(1<<30);
    //jl_method_type->name->constfields = method_constfields;
    const static uint32_t method_atomicfields[] = { 0x20000070, 0x0 }; // (1<<4)|(1<<5)|(1<<6)|(1<<29)
    jl_method_type->name->atomicfields = method_atomicfields;

    jl_method_instance_type =
        jl_new_datatype(jl_symbol("MethodInstance"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(9,
                            "def",
                            "specTypes",
                            "sparam_vals",
                            "backedges",
                            "cache",
                            "cache_with_orig",
                            "flags",
                            "dispatch_status",
                            "precompile"),
                        jl_svec(9,
                            jl_new_struct(jl_uniontype_type, jl_method_type, jl_module_type),
                            jl_any_type,
                            jl_simplevector_type,
                            jl_array_any_type,
                            jl_any_type/*jl_code_instance_type*/,
                            jl_bool_type,
                            jl_bool_type,
                            jl_uint8_type,
                            jl_bool_type),
                        jl_emptysvec,
                        0, 1, 3);
    // These fields should be constant, but Serialization wants to mutate them in initialization
    //const static uint32_t method_instance_constfields[1] = { 0b00000111 }; // fields 1, 2, 3
    const static uint32_t method_instance_atomicfields[1]  = { 0b111010000 }; // fields 5, 7, 8, 9
    //Fields 4 and 5 must be protected by method->write_lock, and thus all operations on jl_method_instance_t are threadsafe.
    //jl_method_instance_type->name->constfields = method_instance_constfields;
    jl_method_instance_type->name->atomicfields = method_instance_atomicfields;

    jl_code_instance_type =
        jl_new_datatype(jl_symbol("CodeInstance"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(20,
                            "def",
                            "owner",
                            "next",
                            "min_world",
                            "max_world",
                            "rettype",
                            "exctype",
                            "rettype_const",
                            "inferred",
                            "debuginfo",
                            "edges",
                            "analysis_results",
                            "ipo_purity_bits",
                            "time_infer_total",
                            "time_infer_cache_saved",
                            "time_infer_self",
                            "time_compile",
                            //"absolute_max",
                            "flags",
                            "invoke", "specptr"), // function object decls
                        jl_svec(20,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_ulong_type,
                            jl_ulong_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_debuginfo_type,
                            jl_simplevector_type,
                            jl_any_type,
                            jl_uint32_type,
                            jl_uint16_type,
                            jl_uint16_type,
                            jl_uint16_type,
                            jl_uint16_type,
                            //jl_bool_type,
                            jl_uint8_type,
                            jl_any_type, jl_any_type), // fptrs
                        jl_emptysvec,
                        0, 1, 1);
    jl_svecset(jl_code_instance_type->types, 2, jl_code_instance_type);
    const static uint32_t code_instance_constfields[1]  = { 0b00001110000011100011 }; // Set fields 1, 2, 6-8, 14-16 as const
    const static uint32_t code_instance_atomicfields[1] = { 0b11110001011100011100 }; // Set fields 3-5, 9-12, 13, 17-20 as atomic
    // Fields 4-5 are only operated on by construction and deserialization, so are effectively const at runtime
    // Fields ipo_purity_bits and analysis_results are not currently threadsafe or reliable, as they get mutated after optimization, but are not declared atomic
    // and there is no way to tell (during inference) if their value is finalized yet (to wait for them to be narrowed if applicable)
    jl_code_instance_type->name->constfields = code_instance_constfields;
    jl_code_instance_type->name->atomicfields = code_instance_atomicfields;

    jl_method_match_type = jl_new_datatype(jl_symbol("MethodMatch"), core, jl_any_type, jl_emptysvec,
                                       jl_perm_symsvec(4, "spec_types", "sparams", "method", "fully_covers"),
                                       jl_svec(4, jl_type_type, jl_simplevector_type, jl_method_type, jl_bool_type),
                                       jl_emptysvec, 0, 0, 4);

    jl_intrinsic_type = jl_new_primitivetype((jl_value_t*)jl_symbol("IntrinsicFunction"), core,
                                             jl_builtin_type, jl_emptysvec, 32);
    XX(intrinsic);

    // LLVMPtr{T, AS} where {T, AS}
    jl_tvar_t *elvar = tvar("T");
    tv = jl_svec2(elvar, tvar("AS"));
    jl_svec_t *tv_base = jl_svec1(elvar);
    jl_llvmpointer_type = (jl_unionall_t*)
        jl_new_primitivetype((jl_value_t*)jl_symbol("LLVMPtr"), core,
                             (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_ref_type, jl_svec_data(tv_base), 1), tv,
                             sizeof(void*)*8)->name->wrapper;
    jl_llvmpointer_typename = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_llvmpointer_type))->name;

    // Type{T} where T<:Tuple
    tttvar = jl_new_typevar(jl_symbol("T"),
                            (jl_value_t*)jl_bottom_type,
                            (jl_value_t*)jl_anytuple_type);
    jl_anytuple_type_type = (jl_unionall_t*)jl_new_struct(jl_unionall_type,
                                                          tttvar, (jl_value_t*)jl_wrap_Type((jl_value_t*)tttvar));

    jl_tvar_t *ntval_var = jl_new_typevar(jl_symbol("T"), (jl_value_t*)jl_bottom_type,
                                          (jl_value_t*)jl_anytuple_type);
    tv = jl_svec2(tvar("names"), ntval_var);
    jl_datatype_t *ntt = jl_new_datatype(jl_symbol("NamedTuple"), core, jl_any_type, tv,
                                         jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 0, 0);
    jl_namedtuple_type = (jl_unionall_t*)ntt->name->wrapper;
    ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_namedtuple_type))->layout = NULL;
    jl_namedtuple_typename = ntt->name;

    // Variable-sized (see jl_cancel_source_t) - only the fixed fields are exposed to
    // julia.
    jl_cancel_source_type = (jl_datatype_t*)
        jl_new_datatype(jl_symbol("CancellationTokenSource"), core, jl_any_type,
                        jl_emptysvec,
                        jl_perm_symsvec(7,
                                        "child_head",
                                        "waiters_head",
                                        "walk_lock",
                                        "state",
                                        "nparents",
                                        "dead_count",
                                        "reg_count"),
                        jl_svec(7,
                                jl_any_type, // Union{Nothing, CancellationTokenSource}, weak
                                jl_any_type, // Union{Nothing, WaitEntry}, strong
                                jl_any_type, // Union{Nothing, ReentrantLock}, strong
                                jl_uint8_type,
                                jl_uint16_type,
                                jl_uint32_type,
                                jl_uint32_type),
                        jl_emptysvec,
                        0, 1, 7);
    // Field 5 (nparents) is const; fields 1-4 (child_head, waiters_head,
    // walk_lock, state), 6 (dead_count) and 7 (reg_count) are atomic
    const static uint32_t cancel_source_constfields[1]  = { 0b0010000 };
    const static uint32_t cancel_source_atomicfields[1] = { 0b1101111 };
    jl_cancel_source_type->name->constfields = cancel_source_constfields;
    jl_cancel_source_type->name->atomicfields = cancel_source_atomicfields;
    XX(cancel_source);
    assert(jl_datatype_size(jl_cancel_source_type) == sizeof(jl_cancel_source_t));

    // Variable-sized (see jl_wait_entry_t) - only the fixed fields are exposed
    // to julia; the trailing wait slots are reached through the
    // jl_wait_entry_slot_* accessors.
    jl_wait_entry_type = (jl_datatype_t*)
        jl_new_datatype(jl_symbol("WaitEntryN"), core, jl_any_type,
                        jl_emptysvec,
                        jl_perm_symsvec(2,
                                        "task",
                                        "nslots"),
                        jl_svec(2,
                                jl_any_type, // Union{Nothing, Task}
                                jl_uint32_type),
                        jl_emptysvec,
                        0, 1, 2);
    // Field 2 (nslots) is const; field 1 (task) is atomic (accessed
    // relaxed - see jl_wait_entry_t)
    const static uint32_t wait_entry_constfields[1] = { 0b10 };
    const static uint32_t wait_entry_atomicfields[1] = { 0b01 };
    jl_wait_entry_type->name->constfields = wait_entry_constfields;
    jl_wait_entry_type->name->atomicfields = wait_entry_atomicfields;
    XX(wait_entry);
    assert(jl_datatype_size(jl_wait_entry_type) == sizeof(jl_wait_entry_t));

    jl_task_type = (jl_datatype_t*)
        jl_new_datatype(jl_symbol("Task"),
                        NULL,
                        jl_any_type,
                        jl_emptysvec,
                        jl_perm_symsvec(32,
                                        "next",
                                        "queue",
                                        "storage",
                                        "donenotify",
                                        "result",
                                        "scope",
                                        "code",
                                        "_state",
                                        "sticky",
                                        "priority",
                                        "_isexception",
                                        "preempt_request",
                                        "bound_cancel_default",
                                        "pad02",
                                        "rngState0",
                                        "rngState1",
                                        "rngState2",
                                        "rngState3",
                                        "rngState4",
                                        "metrics_enabled",
                                        "pad10",
                                        "pad11",
                                        "pad12",
                                        "first_enqueued_at",
                                        "last_started_running_at",
                                        "running_time_ns",
                                        "finished_at",
                                        "waiting_on",
                                        "cached_wait_entry",
                                        "cached_cancel_entry",
                                        "invoked",
                                        "bound_cancel_token"),
                        jl_svec(32,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_uint8_type,
                                jl_bool_type,
                                jl_uint16_type,
                                jl_bool_type,
                                jl_uint8_type,
                                jl_uint8_type,
                                jl_uint8_type,
                                jl_uint64_type,
                                jl_uint64_type,
                                jl_uint64_type,
                                jl_uint64_type,
                                jl_uint64_type,
                                jl_bool_type,
                                jl_uint8_type,
                                jl_uint8_type,
                                jl_uint8_type,
                                jl_uint64_type,
                                jl_uint64_type,
                                jl_uint64_type,
                                jl_uint64_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type),
                        jl_emptysvec,
                        0, 1, 6);
    XX(task);
    jl_value_t *listt = jl_new_struct(jl_uniontype_type, jl_task_type, jl_nothing_type);
    jl_svecset(jl_task_type->types, 0, listt);
    // Declare bound_cancel_token as Union{Nothing, CancellationTokenSource}
    // so reads narrow with a `=== nothing` check instead of a type-tag load
    // (the tag load is a volatile unsafe point that would tear a published
    // reset region on every cache-hit token lookup).
    jl_value_t *boundt = jl_new_struct(jl_uniontype_type, (jl_value_t*)jl_cancel_source_type, jl_nothing_type);
    jl_svecset(jl_task_type->types, 31, boundt);
    // Set field 20 (metrics_enabled) as const
    // Set fields 8 (_state), 12 (preempt_request), 24-27 (metric counters),
    // 28 (waiting_on) and 32 (bound_cancel_token) as atomic
    const static uint32_t task_constfields[1]  = { 0b00000000000010000000000000000000 };
    const static uint32_t task_atomicfields[1] = { 0b10001111100000000000100010000000 };
    jl_task_type->name->constfields = task_constfields;
    jl_task_type->name->atomicfields = task_atomicfields;

    tv = jl_svec2(tvar("A"), tvar("R"));
    jl_opaque_closure_type = (jl_unionall_t*)jl_new_datatype(jl_symbol("OpaqueClosure"), core, jl_function_type, tv,
        // N.B.: OpaqueClosure call code relies on specptr being field 5.
        // Update that code if you change this.
        jl_perm_symsvec(5, "captures", "world", "source", "invoke", "specptr"),
        jl_svec(5, jl_any_type, jl_long_type, jl_any_type, pointer_void, pointer_void),
        jl_emptysvec, 0, 0, 5)->name->wrapper;
    jl_opaque_closure_typename = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_opaque_closure_type))->name;
    jl_compute_field_offsets((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_opaque_closure_type));

    // complete builtin type metadata
    jl_uint8pointer_type = (jl_datatype_t*)jl_apply_type1((jl_value_t*)jl_pointer_type, (jl_value_t*)jl_uint8_type);
    jl_svecset(jl_datatype_type->types, 5, jl_voidpointer_type);
    jl_svecset(jl_datatype_type->types, 6, jl_int32_type);
    jl_svecset(jl_datatype_type->types, 7, jl_uint16_type);
    jl_svecset(jl_typename_type->types, 1, jl_module_type);
    jl_svecset(jl_typename_type->types, 4, jl_voidpointer_type);
    jl_svecset(jl_typename_type->types, 5, jl_voidpointer_type);
    jl_svecset(jl_typename_type->types, 6, jl_type_type);
    jl_svecset(jl_typename_type->types, 7, jl_type_type);
    jl_svecset(jl_typename_type->types, 11, jl_long_type);
    jl_svecset(jl_typename_type->types, 12, jl_int32_type);
    jl_svecset(jl_typename_type->types, 13, jl_int32_type);
    jl_svecset(jl_typename_type->types, 14, jl_uint8_type);
    jl_svecset(jl_typename_type->types, 15, jl_uint8_type);
    jl_svecset(jl_typename_type->types, 16, jl_uint8_type);
    jl_svecset(jl_typename_type->types, 17, jl_uint8_type);
    jl_svecset(jl_typename_type->types, 18, jl_bool_type);
    jl_svecset(jl_methcache_type->types, 2, jl_long_type); // voidpointer
    jl_svecset(jl_methcache_type->types, 3, jl_long_type); // uint32_t plus alignment
    jl_svecset(jl_methtable_type->types, 3, jl_module_type);
    jl_svecset(jl_method_type->types, 14, jl_method_instance_type);
    //jl_svecset(jl_debuginfo_type->types, 0, jl_method_instance_type); // union(jl_method_instance_type, jl_method_type, jl_symbol_type)
    jl_svecset(jl_method_instance_type->types, 4, jl_code_instance_type);
    jl_svecset(jl_code_instance_type->types, 18, jl_voidpointer_type);
    jl_svecset(jl_code_instance_type->types, 19, jl_voidpointer_type);
    jl_svecset(jl_binding_type->types, 0, jl_globalref_type);
    jl_svecset(jl_binding_type->types, 3, jl_array_any_type);
    jl_value_t *partition_next_types[2] = { (jl_value_t*)jl_binding_partition_type, (jl_value_t*)jl_binding_type };
    jl_svecset(jl_binding_partition_type->types, 3, jl_type_union(partition_next_types, 2));
    jl_value_t *wait_entry_task_types[2] = { (jl_value_t*)jl_nothing_type, (jl_value_t*)jl_task_type };
    jl_svecset(jl_wait_entry_type->types, 0, jl_type_union(wait_entry_task_types, 2));

    jl_compute_field_offsets(jl_datatype_type);
    jl_compute_field_offsets(jl_typename_type);
    jl_compute_field_offsets(jl_uniontype_type);
    jl_compute_field_offsets(jl_intersect_type);
    jl_compute_field_offsets(jl_typeeq_type);
    jl_compute_field_offsets(jl_typeegal_type);
    jl_compute_field_offsets(jl_tvar_type);
    jl_compute_field_offsets(jl_methtable_type);
    jl_compute_field_offsets(jl_methcache_type);
    jl_compute_field_offsets(jl_method_instance_type);
    jl_compute_field_offsets(jl_code_instance_type);
    jl_compute_field_offsets(jl_unionall_type);
    jl_compute_field_offsets(jl_simplevector_type);
    jl_compute_field_offsets(jl_symbol_type);
    jl_compute_field_offsets(jl_binding_partition_type);

    // override ismutationfree for builtin types that are mutable for identity
    jl_string_type->ismutationfree = jl_string_type->isidentityfree = 1;
    jl_symbol_type->ismutationfree = jl_symbol_type->isidentityfree = 1;
    jl_simplevector_type->isidentityfree = 1;
    jl_typename_type->ismutationfree = 1;
    jl_datatype_type->ismutationfree = 1;
    jl_uniontype_type->ismutationfree = 1;
    jl_intersect_type->ismutationfree = 1;
    jl_unionall_type->ismutationfree = 1;
    assert(((jl_datatype_t*)jl_array_any_type)->ismutationfree == 0);
    assert(((jl_datatype_t*)jl_array_uint8_type)->ismutationfree == 0);

    // Technically not ismutationfree, but there's a separate system to deal
    // with mutations for global state.
    jl_module_type->ismutationfree = 1;
    // Module object identity is determined by its name and parent name.
    jl_module_type->isidentityfree = 1;

    export_jl_small_typeof();
    export_jl_sysimg_globals();
}

static jl_value_t *core(const char *name) JL_CANSAFEPOINT JL_GLOBALLY_ROOTED
{
    return jl_get_global(jl_core_module, jl_symbol(name));
}


// fetch references to things defined in boot.jl
void post_boot_hooks(void)
{
    jl_char_type    = (jl_datatype_t*)core("Char");
    XX(char);
    jl_int8_type    = (jl_datatype_t*)core("Int8");
    XX(int8);
    jl_int16_type   = (jl_datatype_t*)core("Int16");
    XX(int16);
    jl_float16_type = (jl_datatype_t*)core("Float16");
    //XX(float16);
    jl_float32_type = (jl_datatype_t*)core("Float32");
    //XX(float32);
    jl_float64_type = (jl_datatype_t*)core("Float64");
    //XX(float64);
    jl_bfloat16_type = (jl_datatype_t*)core("BFloat16");
    //XX(bfloat16);
    jl_floatingpoint_type = (jl_datatype_t*)core("AbstractFloat");
    jl_number_type  = (jl_datatype_t*)core("Number");
    jl_signed_type  = (jl_datatype_t*)core("Signed");
    jl_datatype_t *jl_unsigned_type = (jl_datatype_t*)core("Unsigned");
    jl_datatype_t *jl_integer_type = (jl_datatype_t*)core("Integer");

    jl_gc_write(jl_bool_type, jl_bool_type->super, jl_datatype_t, jl_integer_type);
    jl_gc_write(jl_uint8_type, jl_uint8_type->super, jl_datatype_t, jl_unsigned_type);
    jl_gc_write(jl_uint16_type, jl_uint16_type->super, jl_datatype_t, jl_unsigned_type);
    jl_gc_write(jl_uint32_type, jl_uint32_type->super, jl_datatype_t, jl_unsigned_type);
    jl_gc_write(jl_uint64_type, jl_uint64_type->super, jl_datatype_t, jl_unsigned_type);
    jl_gc_write(jl_int32_type, jl_int32_type->super, jl_datatype_t, jl_signed_type);
    jl_gc_write(jl_int64_type, jl_int64_type->super, jl_datatype_t, jl_signed_type);

    jl_stackovf_exception       = jl_new_struct_uninit((jl_datatype_t*)core("StackOverflowError"));
    jl_diverror_exception       = jl_new_struct_uninit((jl_datatype_t*)core("DivideError"));
    jl_undefref_exception       = jl_new_struct_uninit((jl_datatype_t*)core("UndefRefError"));
    jl_interrupt_exception      = jl_new_struct_uninit((jl_datatype_t*)core("InterruptException"));
    jl_memory_exception         = jl_new_struct_uninit((jl_datatype_t*)core("OutOfMemoryError"));
    jl_readonlymemory_exception = jl_new_struct_uninit((jl_datatype_t*)core("ReadOnlyMemoryError"));
    jl_precompilable_error      = jl_new_struct_uninit((jl_datatype_t*)core("PrecompilableError"));

    jl_errorexception_type   = (jl_datatype_t*)core("ErrorException");
    jl_undefvarerror_type    = (jl_datatype_t*)core("UndefVarError");
    jl_fielderror_type       = (jl_datatype_t*)core("FieldError");
    jl_atomicerror_type      = (jl_datatype_t*)core("ConcurrencyViolationError");
    jl_boundserror_type      = (jl_datatype_t*)core("BoundsError");
    jl_typeerror_type        = (jl_datatype_t*)core("TypeError");
    jl_argumenterror_type    = (jl_datatype_t*)core("ArgumentError");
    jl_methoderror_type      = (jl_datatype_t*)core("MethodError");
    jl_loaderror_type        = (jl_datatype_t*)core("LoadError");
    jl_initerror_type        = (jl_datatype_t*)core("InitError");
    jl_missingcodeerror_type = (jl_datatype_t*)core("MissingCodeError");

    jl_pair_type             = core("Pair");
    jl_value_t *kwcall_func  = core("kwcall");
    jl_kwcall_type = (jl_datatype_t*)jl_typeof(kwcall_func);
    jl_atomic_store_relaxed(&jl_kwcall_type->name->max_args, 0);

    // Initialize TypeApp type reference for mutually recursive types
    jl_typeapp_type = (jl_datatype_t*)core("TypeApp");

    jl_weakref_type = (jl_datatype_t*)core("WeakRef");
    jl_vecelement_typename = ((jl_datatype_t*)jl_unwrap_unionall(core("VecElement")))->name;
    jl_abioverride_type = (jl_datatype_t*)core("ABIOverride");

    jl_const_type = (jl_datatype_t*)core("Const");
    jl_partial_struct_type = (jl_datatype_t*)core("PartialStruct");
    jl_interconditional_type = (jl_datatype_t*)core("InterConditional");
    jl_partial_opaque_type = (jl_datatype_t*)core("PartialOpaque");
    jl_partial_task_type = (jl_datatype_t*)core("PartialTask");
    jl_possibly_ambiguous_type = (jl_datatype_t*)core("PossiblyAmbiguous");
    jl_inter_must_alias_type = (jl_datatype_t*)core("InterMustAlias");

    export_jl_small_typeof();
    export_jl_sysimg_globals();
}

#undef XX

#ifdef __cplusplus
}
#endif
