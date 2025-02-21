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


static int layout_uses_free_typevars(jl_value_t *v, jl_typeenv_t *env)
{
    while (1) {
        if (jl_is_typevar(v))
            return !typeenv_has(env, (jl_tvar_t*)v);
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
            if (dt->name == jl_namedtuple_typename)
                return layout_uses_free_typevars(jl_tparam0(dt), env) || layout_uses_free_typevars(jl_tparam1(dt), env);
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
        else if (jl_is_uniontype(v)) {
            if (layout_uses_free_typevars(((jl_uniontype_t*)v)->a, env))
                return 1;
           v = ((jl_uniontype_t*)v)->b;
        }
        else if (jl_is_vararg(v)) {
            jl_vararg_t *vm = (jl_vararg_t*)v;
            if (!vm->T)
                return 0;
            if (vm->N && layout_uses_free_typevars(vm->N, env))
                return 1;
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
        if (jl_is_typevar(v)) {
            return !typeenv_has(env, (jl_tvar_t*)v);
        }
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
        else if (jl_is_uniontype(v)) {
            if (has_free_typevars(((jl_uniontype_t*)v)->a, env))
                return 1;
           v = ((jl_uniontype_t*)v)->b;
        }
        else if (jl_is_vararg(v)) {
            jl_vararg_t *vm = (jl_vararg_t*)v;
            if (!vm->T)
                return 0;
            if (vm->N && has_free_typevars(vm->N, env))
                return 1;
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

static void find_free_typevars(jl_value_t *v, jl_typeenv_t *env, jl_array_t *out)
{
    while (1) {
        if (jl_is_typevar(v)) {
            if (!typeenv_has(env, (jl_tvar_t*)v))
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
            for (i = 0; i < jl_nparams(v); i++)
                find_free_typevars(jl_tparam(v, i), env, out);
            return;
        }
        else if (jl_is_uniontype(v)) {
            find_free_typevars(((jl_uniontype_t*)v)->a, env, out);
            v = ((jl_uniontype_t*)v)->b;
        }
        else if (jl_is_vararg(v)) {
            jl_vararg_t *vm = (jl_vararg_t *)v;
            if (!vm->T)
                return;
            if (vm->N) // this swap the visited order, but we don't mind it
                find_free_typevars(vm->N, env, out);
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
        if (jl_is_typevar(v)) {
            return typeenv_has_ne(env, (jl_tvar_t*)v);
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
        else if (jl_is_uniontype(v)) {
            if (jl_has_bound_typevars(((jl_uniontype_t*)v)->a, env))
                return 1;
           v = ((jl_uniontype_t*)v)->b;
        }
        else if (jl_is_vararg(v)) {
            jl_vararg_t *vm = (jl_vararg_t *)v;
            if (!vm->T)
                return 0;
            if (vm->N && jl_has_bound_typevars(vm->N, env))
                return 1;
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
    if (dt->isconcretetype)
        return 1;
    if (jl_is_genericmemory_type(dt)) { // GenericMemory{kind,addrspace,T} uses T for final layout, which is a parameter not a field however
        // optionally: return !layout_uses_free_typevars(jl_tparam1(dt), env);
        return 0;
    }
    if (dt->layout)
        return 1;
    if (dt->name->abstract)
        return 0;
    if (dt->name == jl_namedtuple_typename)
        return !layout_uses_free_typevars(jl_tparam0(dt), NULL) && !layout_uses_free_typevars(jl_tparam1(dt), NULL);
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
int jl_find_union_component(jl_value_t *haystack, jl_value_t *needle, unsigned *nth) JL_NOTSAFEPOINT
{
    while (jl_is_uniontype(haystack)) {
        jl_uniontype_t *u = (jl_uniontype_t*)haystack;
        if (jl_find_union_component(u->a, needle, nth))
            return 1;
        haystack = u->b;
    }
    if (needle == haystack)
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

static void flatten_type_union(jl_value_t **types, size_t n, jl_value_t **out, size_t *idx, int widen)
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
            size_t old_idx = 0;
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

static int simple_subtype(jl_value_t *a, jl_value_t *b, int hasfree, int isUnion)
{
    assert(hasfree == (jl_has_free_typevars(a) | (jl_has_free_typevars(b) << 1)));
    if (a == jl_bottom_type || b == (jl_value_t*)jl_any_type)
        return 1;
    if (jl_egal(a, b))
        return 1;
    if (hasfree == 0) {
        int mergeable = isUnion;
        if (!mergeable) // issue #24521: don't merge Type{T} where typeof(T) varies
            mergeable = !(jl_is_type_type(a) && jl_is_type_type(b) &&
             jl_typeof(jl_tparam0(a)) != jl_typeof(jl_tparam0(b)));
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
    if (b==(jl_value_t*)jl_datatype_type || b==(jl_value_t*)jl_typeofbottom_type) {
        // This branch is not valid for `Union`/`UnionAll`, e.g.
        // (Type{Union{Int,T2} where {T2<:T1}} where {T1}){Int} == Type{Int64}
        // (Type{Union{Int,T1}} where {T1}){Int} == Type{Int64}
        return jl_is_type_type(a) && jl_typeof(jl_tparam0(a)) == b;
    }
    return 0;
}

// merge Union{Tuple{}, Tuple{T}, Tuple{T, T, Vararg{T}}} into Tuple{Vararg{T}}
// assumes temp is already sorted by number of type parameters
STATIC_INLINE void merge_vararg_unions(jl_value_t **temp, size_t nt)
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
        for (size_t j = 0; j < nfields-1; j++)
            if (!jl_egal(jl_tparam(tt, j), t)) goto outer_loop;

        // look for Tuple{T, T, ...} then Tuple{T, ...}, etc
        size_t min_elements = nfields-1;
        for (long j = i-1; j >= 0; j--) {
            jl_value_t *ttj = temp[j];
            if (!jl_is_tuple_type(ttj)) break;
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

static int simple_subtype2(jl_value_t *a, jl_value_t *b, int hasfree, int isUnion)
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
    else if (jl_is_type_type(a) && jl_is_type_type(b) &&
             jl_typeof(jl_tparam0(a)) != jl_typeof(jl_tparam0(b))) {
        // issue #24521: don't merge Type{T} where typeof(T) varies
    }
    else if (jl_typeof(a) == jl_typeof(b) && jl_types_egal(a, b)) {
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

int obviously_disjoint(jl_value_t *a, jl_value_t *b, int specificity);

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
    nt = subs[0] ? nta : subs[1] ? nt  : nt;
    i  = subs[0] ? 0   : subs[1] ? nta : 0;
    count = nt - i;
    if (!subs[0] && !subs[1]) {
        // prepare for over estimation
        // only preserve `a` with strict <:, but preserve `b` without strict >:
        for (j = 0; j < nt; j++) {
            if (stemp[j] < (j < nta ? 2 : 0))
                temp[j] = NULL;
        }
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
    if (!jl_is_type(body) && !jl_is_typevar(body))
        jl_type_error("UnionAll", (jl_value_t*)jl_type_type, body);
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

static int typekey_eq(jl_datatype_t *tt, jl_value_t **key, size_t n)
{
    size_t j;
    // TODO: This shouldn't be necessary
    JL_GC_PROMISE_ROOTED(tt);
    size_t tnp = jl_nparams(tt);
    if (n != tnp)
        return 0;
    if (tt->name == jl_type_typename) {
        // for Type{T}, require `typeof(T)` to match also, to avoid incorrect
        // dispatch from changing the type of something.
        // this should work because `Type`s don't need unique pointers, and aren't the
        // direct tags of values (concrete) so we don't rely on pointer equality.
        jl_value_t *kj = key[0];
        jl_value_t *tj = jl_tparam0(tt);
        return (kj == tj || (jl_typeof(tj) == jl_typeof(kj) && jl_types_equal(tj, kj)));
    }
    for (j = 0; j < n; j++) {
        jl_value_t *kj = key[j];
        jl_value_t *tj = jl_svecref(tt->parameters, j);
        if (tj != kj) {
            if (tt->name == jl_tuple_typename) {
                // require exact same Type{T} in covariant context. see e.g. issue #22842
                // this should work because `Tuple{Type}`s don't need unique pointers, and aren't the
                // direct tags of values (concrete) so we don't rely on pointer equality.
                if (jl_is_type_type(tj) || jl_is_type_type(kj))
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
static int typekeyvalue_eq(jl_datatype_t *tt, jl_value_t *key1, jl_value_t **key, size_t n, int leaf)
{
    size_t j;
    // TODO: This shouldn't be necessary
    JL_GC_PROMISE_ROOTED(tt);
    size_t tnp = jl_nparams(tt);
    if (n != tnp)
        return 0;
    if (leaf && tt->name == jl_type_typename) {
        // for Type{T}, require `typeof(T)` to match also, to avoid incorrect
        // dispatch from changing the type of something.
        // this should work because `Type`s don't have uids, and aren't the
        // direct tags of values so we don't rely on pointer equality.
        jl_value_t *kj = key1;
        jl_value_t *tj = jl_tparam0(tt);
        return (kj == tj || (jl_typeof(tj) == jl_typeof(kj) && jl_types_equal(tj, kj)));
    }
    for (j = 0; j < n; j++) {
        jl_value_t *kj = j == 0 ? key1 : key[j - 1];
        jl_value_t *tj = jl_svecref(tt->parameters, j);
        if (leaf && jl_is_type_type(tj)) {
            jl_value_t *tp0 = jl_tparam0(tj);
            if (!(kj == tp0 || (jl_typeof(tp0) == jl_typeof(kj) && jl_types_equal(tp0, kj))))
                return 0;
        }
        else if (jl_typeof(kj) != tj) {
            return 0;
        }
        else if (leaf && jl_is_kind(tj)) {
            return 0;
        }
    }
    return 1;
}

static unsigned typekey_hash(jl_typename_t *tn, jl_value_t **key, size_t n, int nofail) JL_NOTSAFEPOINT;
static unsigned typekeyvalue_hash(jl_typename_t *tn, jl_value_t *key1, jl_value_t **key, size_t n, int leaf) JL_NOTSAFEPOINT;

/* returns val if key is in hash, otherwise NULL */
static jl_datatype_t *lookup_type_set(jl_svec_t *cache, jl_value_t **key, size_t n, uint_t hv)
{
    size_t sz = jl_svec_len(cache);
    if (sz == 0)
        return NULL;
    size_t maxprobe = max_probe(sz);
    _Atomic(jl_datatype_t*) *tab = (_Atomic(jl_datatype_t*)*)jl_svec_data(cache);
    size_t index = h2index(hv, sz);
    size_t orig = index;
    size_t iter = 0;
    do {
        jl_datatype_t *val = jl_atomic_load_relaxed(&tab[index]);
        if ((jl_value_t*)val == jl_nothing)
            return NULL;
        if (val->hash == hv && typekey_eq(val, key, n))
            return val;
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);
    return NULL;
}

/* returns val if key is in hash, otherwise NULL */
static jl_datatype_t *lookup_type_setvalue(jl_svec_t *cache, jl_value_t *key1, jl_value_t **key, size_t n, uint_t hv, int leaf)
{
    size_t sz = jl_svec_len(cache);
    if (sz == 0)
        return NULL;
    size_t maxprobe = max_probe(sz);
    _Atomic(jl_datatype_t*) *tab = (_Atomic(jl_datatype_t*)*)jl_svec_data(cache);
    size_t index = h2index(hv, sz);
    size_t orig = index;
    size_t iter = 0;
    do {
        jl_datatype_t *val = jl_atomic_load_relaxed(&tab[index]);
        if ((jl_value_t*)val == jl_nothing)
            return NULL;
        if (val->hash == hv && typekeyvalue_eq(val, key1, key, n, leaf))
            return val;
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);
    return NULL;
}

// look up a type in a cache by binary or linear search.
// if found, returns the index of the found item. if not found, returns
// ~n, where n is the index where the type should be inserted.
static ssize_t lookup_type_idx_linear(jl_svec_t *cache, jl_value_t **key, size_t n)
{
    if (n == 0)
        return -1;
    _Atomic(jl_datatype_t*) *data = (_Atomic(jl_datatype_t*)*)jl_svec_data(cache);
    size_t cl = jl_svec_len(cache);
    ssize_t i;
    for (i = 0; i < cl; i++) {
        jl_datatype_t *tt = jl_atomic_load_relaxed(&data[i]);
        if ((jl_value_t*)tt == jl_nothing)
            return ~i;
        if (typekey_eq(tt, key, n))
            return i;
    }
    return ~cl;
}

static ssize_t lookup_type_idx_linearvalue(jl_svec_t *cache, jl_value_t *key1, jl_value_t **key, size_t n)
{
    if (n == 0)
        return -1;
    _Atomic(jl_datatype_t*) *data = (_Atomic(jl_datatype_t*)*)jl_svec_data(cache);
    size_t cl = jl_svec_len(cache);
    ssize_t i;
    for (i = 0; i < cl; i++) {
        jl_datatype_t *tt = jl_atomic_load_relaxed(&data[i]);
        if ((jl_value_t*)tt == jl_nothing)
            return ~i;
        if (typekeyvalue_eq(tt, key1, key, n, 1))
            return i;
    }
    return ~cl;
}

static jl_value_t *lookup_type(jl_typename_t *tn JL_PROPAGATES_ROOT, jl_value_t **key, size_t n)
{
    JL_TIMING(TYPE_CACHE_LOOKUP, TYPE_CACHE_LOOKUP);
    if (tn == jl_type_typename) {
        assert(n == 1);
        jl_value_t *uw = jl_unwrap_unionall(key[0]);
        if (jl_is_datatype(uw) && key[0] == ((jl_datatype_t*)uw)->name->wrapper)
            return jl_atomic_load_acquire(&((jl_datatype_t*)uw)->name->Typeofwrapper);
    }
    unsigned hv = typekey_hash(tn, key, n, 0);
    if (hv) {
        jl_svec_t *cache = jl_atomic_load_relaxed(&tn->cache);
        return (jl_value_t*)lookup_type_set(cache, key, n, hv);
    }
    else {
        jl_svec_t *linearcache = jl_atomic_load_relaxed(&tn->linearcache);
        ssize_t idx = lookup_type_idx_linear(linearcache, key, n);
        return (idx < 0) ? NULL : jl_svecref(linearcache, idx);
    }
}

static jl_value_t *lookup_typevalue(jl_typename_t *tn, jl_value_t *key1, jl_value_t **key, size_t n, int leaf)
{
    JL_TIMING(TYPE_CACHE_LOOKUP, TYPE_CACHE_LOOKUP);
    unsigned hv = typekeyvalue_hash(tn, key1, key, n, leaf);
    if (hv) {
        jl_svec_t *cache = jl_atomic_load_relaxed(&tn->cache);
        return (jl_value_t*)lookup_type_setvalue(cache, key1, key, n, hv, leaf);
    }
    else {
        assert(leaf);
        jl_svec_t *linearcache = jl_atomic_load_relaxed(&tn->linearcache);
        ssize_t idx = lookup_type_idx_linearvalue(linearcache, key1, key, n);
        return (idx < 0) ? NULL : jl_svecref(linearcache, idx);
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
            if (atomic)
                jl_atomic_store_release(&tab[index], (jl_value_t*)val);
            else
                jl_atomic_store_relaxed(&tab[index], (jl_value_t*)val);
            jl_gc_wb(a, val);
            return 1;
        }
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);

    return 0;
}

static void cache_insert_type_set(jl_datatype_t *val, uint_t hv)
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
        jl_atomic_store_release(&val->name->cache, a);
        jl_gc_wb(val->name, a);
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

static void cache_insert_type_linear(jl_datatype_t *type, ssize_t insert_at)
{
    jl_svec_t *cache = jl_atomic_load_relaxed(&type->name->linearcache);
    assert(jl_is_svec(cache));
    size_t n = jl_svec_len(cache);
    if (n == 0 || jl_svecref(cache, n - 1) != jl_nothing) {
        jl_svec_t *nc = jl_svec_fill(n < 4 ? 4 : n * 2, jl_nothing);
        memcpy(jl_svec_data(nc), jl_svec_data(cache), sizeof(void*) * n);
        jl_atomic_store_release(&type->name->linearcache, nc);
        jl_gc_wb(type->name, nc);
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
    if (type->name == jl_type_typename) {
        assert(n == 1);
        jl_value_t *uw = jl_unwrap_unionall(key[0]);
        if (jl_is_datatype(uw) && key[0] == ((jl_datatype_t*)uw)->name->wrapper) {
            jl_typename_t *tn2 = ((jl_datatype_t*)uw)->name;
            jl_atomic_store_release(&tn2->Typeofwrapper, (jl_value_t*)type);
            jl_gc_wb(tn2, type);
            return;
        }
    }
    unsigned hv = typekey_hash(type->name, key, n, 0);
    if (hv) {
        assert(hv == type->hash);
        cache_insert_type_set(type, hv);
    }
    else {
        ssize_t idx = lookup_type_idx_linear(jl_atomic_load_relaxed(&type->name->linearcache), key, n);
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
        if (dt->name->abstract && dt->name != jl_type_typename)
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
    return ((jl_has_free_typevars(vlb) || jl_subtype(vlb, lb)) &&
            (jl_has_free_typevars(vub) || jl_subtype(ub, vub)));
}

struct _jl_typestack_t;
typedef struct _jl_typestack_t jl_typestack_t;

static jl_value_t *inst_datatype_inner(jl_datatype_t *dt, jl_svec_t *p, jl_value_t **iparams, size_t ntp,
                                       jl_typestack_t *stack, jl_typeenv_t *env, int check, int nothrow);

// Build an environment mapping a TypeName's parameters to parameter values.
// This is the environment needed for instantiating a type's supertype and field types.
static jl_value_t *inst_datatype_env(jl_value_t *dt, jl_svec_t *p, jl_value_t **iparams, size_t ntp,
                                     jl_typestack_t *stack, jl_typeenv_t *env, int c)
{
    if (jl_is_datatype(dt))
        return inst_datatype_inner((jl_datatype_t*)dt, p, iparams, ntp, stack, env, 1, 0);
    assert(jl_is_unionall(dt));
    jl_unionall_t *ua = (jl_unionall_t*)dt;
    jl_typeenv_t e = { ua->var, iparams[c], env };
    return inst_datatype_env(ua->body, p, iparams, ntp, stack, &e, c + 1);
}

jl_value_t *jl_apply_type(jl_value_t *tc, jl_value_t **params, size_t n)
{
    if (tc == (jl_value_t*)jl_anytuple_type)
        return jl_apply_tuple_type_v(params, n);
    if (tc == (jl_value_t*)jl_uniontype_type)
        return (jl_value_t*)jl_type_union(params, n);
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
            jl_errorf("too many parameters for type %s", typ);
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

        tc = jl_instantiate_unionall(ua, pi);
    }
    JL_GC_POP();
    return tc;
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
};

static jl_value_t *inst_type_w_(jl_value_t *t, jl_typeenv_t *env, jl_typestack_t *stack, int check, int nothrow);
static jl_svec_t *inst_ftypes(jl_svec_t *p, jl_typeenv_t *env, jl_typestack_t *stack, int cacheable);

JL_DLLEXPORT jl_value_t *jl_instantiate_unionall(jl_unionall_t *u, jl_value_t *p)
{
    jl_typeenv_t env = { u->var, p, NULL };
    return inst_type_w_(u->body, &env, NULL, 1, 0);
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

jl_value_t *jl_substitute_var_nothrow(jl_value_t *t, jl_tvar_t *var, jl_value_t *val, int nothrow)
{
    if (val == (jl_value_t*)var)
        return t;
    nothrow = jl_is_typevar(val) ? 0 : nothrow;
    jl_typeenv_t env = { var, val, NULL };
    return inst_type_w_(t, &env, NULL, 1, nothrow);
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
            jl_svec_t *sv = jl_alloc_svec_uninit(jl_svec_len(uw->parameters));
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
                vals[i] = jl_substitute_datatype(jl_svecref(typ->parameters, i), x, y);
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
                                     jl_value_t **iparams)
{
    // if an identical instantiation is already in process somewhere up the
    // stack, return it. this computes a fixed point for recursive types.
    jl_typename_t *tn = tt->name;
    while (stack != NULL) {
        JL_GC_PROMISE_ROOTED(stack->tt);
        if (stack->tt->name == tn &&
            ntp == jl_svec_len(stack->tt->parameters) &&
            typekey_eq(stack->tt, iparams, ntp)) {
            return (jl_value_t*)stack->tt;
        }
        stack = stack->prev;
    }
    return NULL;
}

// stable numbering for types--starts with name->hash, then falls back to objectid
// sets *failed if the hash value isn't stable (if this param not set on entry)
static unsigned type_hash(jl_value_t *kj, int *failed) JL_NOTSAFEPOINT
{
    jl_value_t *uw = jl_is_unionall(kj) ? jl_unwrap_unionall(kj) : kj;
    if (jl_is_datatype(uw)) {
        jl_datatype_t *dt = (jl_datatype_t*)uw;
        unsigned hash = dt->hash;
        if (!hash) {
            if (!*failed) {
                *failed = 1;
                return 0;
            }
            // compute a hash now, only for the parent object we are putting in the cache
            hash = typekey_hash(dt->name, jl_svec_data(dt->parameters), jl_svec_len(dt->parameters), *failed);
        }
        return hash;
    }
    else if (jl_is_typevar(uw)) {
        // ignore var and lb, since those might get normalized out in equality testing
        return type_hash(((jl_tvar_t*)uw)->ub, failed);
    }
    else if (jl_is_uniontype(uw)) {
        if (!*failed) {
            *failed = 1;
            return 0;
        }
        // compute a hash now, only for the parent object we are putting in the cache
        unsigned hasha = type_hash(((jl_uniontype_t*)uw)->a, failed);
        unsigned hashb = type_hash(((jl_uniontype_t*)uw)->b, failed);
        // use a associative mixing function, with well-defined overflow
        // since Union is associative
        return hasha + hashb;
    }
    else {
        return jl_object_id(uw);
    }
}

JL_DLLEXPORT uintptr_t jl_type_hash(jl_value_t *v) JL_NOTSAFEPOINT
{
    // NOTE: The value of `failed` is purposefully ignored here. The parameter is relevant
    // for other parts of the internal algorithm but not for exposing to the Julia side.
    int failed = 0;
    return type_hash(v, &failed);
}

static unsigned typekey_hash(jl_typename_t *tn, jl_value_t **key, size_t n, int nofail) JL_NOTSAFEPOINT
{
    if (tn == jl_type_typename && key[0] == jl_bottom_type)
        return jl_typeofbottom_type->hash;
    size_t j;
    unsigned hash = 3;
    int failed = nofail;
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
        unsigned hashp = type_hash(p, &failed);
        if (failed && !nofail)
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
            hj = typekey_hash(jl_type_typename, &kj, 1, 0);
            if (hj == 0)
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
                dt->isconcretetype = (jl_is_datatype(p) && ((jl_datatype_t*)p)->isconcretetype) || p == jl_bottom_type;
            if (dt->isdispatchtuple) {
                dt->isdispatchtuple = jl_is_datatype(p) &&
                    ((!jl_is_kind(p) && ((jl_datatype_t*)p)->isconcretetype) ||
                     (p == (jl_value_t*)jl_typeofbottom_type) || // == Type{Union{}}, so needs to be consistent
                     (((jl_datatype_t*)p)->name == jl_type_typename && !((jl_datatype_t*)p)->hasfreetypevars));
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
    if (dt->name == jl_type_typename) {
        jl_value_t *p = jl_tparam(dt, 0);
        if (!jl_is_type(p) && !jl_is_typevar(p)) // Type{v} has no subtypes, if v is not a Type
            dt->has_concrete_subtype = 0;
        dt->maybe_subtype_of_cache = 1;
        jl_value_t *uw = jl_unwrap_unionall(p);
        // n.b. the cache for Type ignores parameter normalization except for Typeofwrapper, so it can't be used to make a stable hash value
        if (!jl_is_datatype(uw) || ((jl_datatype_t*)uw)->name->wrapper != p)
            cacheable = 0;
    }
    dt->hash = typekey_hash(dt->name, jl_svec_data(dt->parameters), l, cacheable);
}

static int check_datatype_parameters(jl_typename_t *tn, jl_value_t **params, size_t np, int nothrow)
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
                jl_value_t *nb = jl_substitute_var_nothrow(bj, tv, params[i], nothrow ? (isub ? 2 : 1) : 0 );
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
                if (!_may_substitute_ub(jl_tparam(v, i), var, invar, cov_count))
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
            if (va->N && !_may_substitute_ub(va->N, var, 1, cov_count))
                return 0;
            if (!jl_is_concrete_type(var->ub))
                inside_inv = 1; // treat as invariant inside vararg, for the sake of this algorithm
            v = va->T;
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

static jl_value_t *normalize_unionalls(jl_value_t *t)
{
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        jl_value_t *a = NULL;
        jl_value_t *b = NULL;
        JL_GC_PUSH2(&a, &b);
        a = normalize_unionalls(u->a);
        b = normalize_unionalls(u->b);
        if (a != u->a || b != u->b) {
            t = jl_new_struct(jl_uniontype_type, a, b);
        }
        JL_GC_POP();
    }
    else if (jl_is_unionall(t)) {
        jl_unionall_t *u = (jl_unionall_t*)t;
        jl_value_t *body = normalize_unionalls(u->body);
        JL_GC_PUSH2(&body, &t);
        if (body != u->body) {
            t = jl_new_struct(jl_unionall_type, u->var, body);
            u = (jl_unionall_t*)t;
        }

        if (u->var->lb == u->var->ub || may_substitute_ub(body, u->var)) {
            body = (jl_value_t*)u;
            JL_TRY {
                t = jl_instantiate_unionall(u, u->var->ub);
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
static jl_value_t *jl_tupletype_fill(size_t n, jl_value_t *t, int check, int nothrow)
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
        t = normalize_unionalls(t);
        p = t;
        jl_value_t *tw = extract_wrapper(t);
        if (tw && t != tw && !jl_has_free_typevars(t) && jl_types_equal(t, tw))
            t = tw;
        p = t;
        check = 0; // remember that checks are already done now
    }
    p = (jl_value_t*)jl_svec_fill(n, t);
    p = jl_apply_tuple_type((jl_svec_t*)p, check);
    JL_GC_POP();
    return p;
}

static jl_value_t *_jl_instantiate_type_in_env(jl_value_t *ty, jl_unionall_t *env, jl_value_t **vals, jl_typeenv_t *prev, jl_typestack_t *stack);

static jl_value_t *inst_datatype_inner(jl_datatype_t *dt, jl_svec_t *p, jl_value_t **iparams, size_t ntp,
                                       jl_typestack_t *stack, jl_typeenv_t *env, int check, int nothrow)
{
    jl_typestack_t top;
    jl_typename_t *tn = dt->name;
    int istuple = (tn == jl_tuple_typename);
    int isnamedtuple = (tn == jl_namedtuple_typename);

    // check if type cache will be applicable
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
    if (cacheable) {
        jl_value_t *lkup = (jl_value_t*)lookup_type(tn, iparams, ntp);
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
            // normalize types equal to wrappers (prepare for Typeofwrapper)
            jl_value_t *tw = extract_wrapper(pi);
            if (tw && tw != pi && (tn != jl_type_typename || jl_typeof(pi) == jl_typeof(tw)) &&
                    !jl_has_free_typevars(pi) && jl_types_equal(pi, tw)) {
                iparams[i] = tw;
                if (p) jl_gc_wb(p, tw);
            }
        }
        if (tn == jl_type_typename && jl_is_datatype(iparams[0]) && ((jl_datatype_t*)iparams[0])->name == jl_type_typename &&
            jl_tparam0(iparams[0]) == jl_bottom_type) {
            // normalize Type{Type{Union{}}} to Type{TypeofBottom}
            iparams[0] = (jl_value_t*)jl_typeofbottom_type;
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
        if (check_datatype_parameters(tn, iparams, ntp, nothrow))
            return NULL;
    }
    else if (ntp == 0 && jl_emptytuple_type != NULL) {
        // empty tuple type case
        assert(istuple);
        return (jl_value_t*)jl_emptytuple_type;
    }

    jl_datatype_t *ndt = NULL;
    JL_GC_PUSH2(&p, &ndt);

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
                    return jl_tupletype_fill(nt, va0, 0, 0);
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
                jl_value_t *ndt = inst_datatype_inner(jl_anytuple_type, p, pp, np, NULL, NULL, check, nothrow);
                JL_GC_POP();
                return ndt;
            }
        }
    }

    // try to simplify some type parameters
    if (check && tn != jl_type_typename) {
        int changed = 0;
        if (istuple) // normalization might change Tuple's, but not other types's, cacheable status
            cacheable = 1;
        size_t i;
        for (i = 0; i < ntp; i++) {
            jl_value_t *pi = iparams[i];
            jl_value_t *newp = normalize_unionalls(pi);
            if (newp != pi) {
                iparams[i] = newp;
                if (p) jl_gc_wb(p, newp);
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

    // try to reduce duplication in objects (if the caller didn't already check) by
    // comparing them against a list of objects already known to be globally rooted and
    // swapping them as possible
    if (check && jl_global_roots_list != NULL) {
        for (size_t i = 0; i < ntp; i++) {
            jl_value_t *pi = iparams[i];
            if (cacheable || !jl_has_free_typevars(pi)) {
                pi = jl_as_global_root(pi, cacheable);
                if (pi != NULL) {
                    iparams[i] = pi;
                    if (p) jl_gc_wb(p, pi);
                }
            }
        }
    }

    // move array of instantiated parameters to heap; we need to keep it
    if (p == NULL) {
        p = jl_alloc_svec_uninit(ntp);
        for (size_t i = 0; i < ntp; i++) {
            jl_svecset(p, i, iparams[i]);
        }
    }

    ndt = jl_new_uninitialized_datatype();

    // now that most allocations are done
    // acquire the write lock now that we know we need a new object
    // since we're going to immediately leak it globally via the instantiation stack
    if (cacheable) {
        JL_LOCK(&typecache_lock); // Might GC
        jl_value_t *lkup = (jl_value_t*)lookup_type(tn, iparams, ntp);
        if (lkup) {
            JL_UNLOCK(&typecache_lock); // Might GC
            JL_GC_POP();
            return lkup;
        }
    }

    // create and initialize new type
    ndt->isprimitivetype = dt->isprimitivetype;
    // Usually dt won't have ismutationfree set at this point, but it is
    // overridden for `Type`, which we handle here.
    ndt->ismutationfree = dt->ismutationfree;
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
    ndt->types = NULL; // to be filled in below
    int invalid = 0;
    if (istuple) {
        ndt->types = p; // TODO: this may need to filter out certain types
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
                ndt->types = jl_svec_fill(nf, jl_bottom_type);
            }
            else {
                if (!jl_is_datatype(values_tt)) {
                    // should have been checked within `check_datatype_parameters`.
                    jl_error("NamedTuple field type must be a tuple datatype");
                }
                if (jl_is_va_tuple((jl_datatype_t*)values_tt) || jl_nparams(values_tt) != nf) {
                    if (!nothrow)
                        jl_error("NamedTuple names and field types must have matching lengths");
                    invalid = 1;
                }
                ndt->types = ((jl_datatype_t*)values_tt)->parameters;
            }
            jl_gc_wb(ndt, ndt->types);
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
        if (cacheable)
            JL_UNLOCK(&typecache_lock);
        JL_GC_POP();
        return NULL;
    }
    jl_datatype_t *primarydt = ((jl_datatype_t*)jl_unwrap_unionall(tn->wrapper));
    jl_precompute_memoized_dt(ndt, cacheable);
    if (primarydt->layout)
        jl_compute_field_offsets(ndt);

    if (istuple || isnamedtuple) {
        ndt->super = jl_any_type;
    }
    else if (dt->super) {
        jl_value_t *super = inst_type_w_((jl_value_t*)dt->super, env, stack, check, nothrow);
        if (nothrow && super == NULL) {
            if (cacheable)
                JL_UNLOCK(&typecache_lock);
            JL_GC_POP();
            return NULL;
        }
        ndt->super = (jl_datatype_t *)super;
        jl_gc_wb(ndt, ndt->super);
    }
    jl_svec_t *ftypes = dt->types;
    if (ftypes == NULL)
        ftypes = primarydt->types;
    if (ftypes == NULL || dt->super == NULL) {
        // in the process of creating this type definition:
        // need to instantiate the super and types fields later
        if (tn->partial == NULL) {
            tn->partial = jl_alloc_vec_any(0);
            jl_gc_wb(tn, tn->partial);
        }
        jl_array_ptr_1d_push(tn->partial, (jl_value_t*)ndt);
    }
    else if (!isnamedtuple && !istuple) {
        assert(ftypes != jl_emptysvec || jl_field_names(ndt) == jl_emptysvec);
        assert(ftypes == jl_emptysvec || !ndt->name->abstract);
        if (ftypes == jl_emptysvec) {
            ndt->types = ftypes;
        }
        else if (cacheable) {
            // recursively instantiate the types of the fields
            if (dt->types == NULL)
                ndt->types = jl_compute_fieldtypes(ndt, stack, cacheable);
            else
                ndt->types = inst_ftypes(ftypes, env, stack, cacheable);
            jl_gc_wb(ndt, ndt->types);
        }
    }

    // now publish the finished result
    // XXX: if the stack was used, this will publish in the wrong order,
    // leading to incorrect layouts and data races (#40050: the A{T} should be
    // an isbitstype singleton of size 0)
    if (cacheable) {
        if (ndt->layout == NULL && ndt->types != NULL && ndt->isconcretetype)
            jl_compute_field_offsets(ndt);
        jl_cache_type_(ndt);
        JL_UNLOCK(&typecache_lock); // Might GC
    }

    JL_GC_POP();
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
                // if `ai` has free type vars this will not be a valid (concrete) type.
                // TODO: it would be really nice to only dispatch and cache those as
                // `jl_typeof(ai)`, but that will require some redesign of the caching
                // logic.
                ai = (jl_value_t*)jl_wrap_Type(ai);
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

static jl_value_t *inst_tuple_w_(jl_value_t *t, jl_typeenv_t *env, jl_typestack_t *stack, int check, int nothrow)
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
                return jl_tupletype_fill(nt, T, check, nothrow);
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
        iparams[i] = pi;
        if (ip_heap)
            jl_gc_wb(ip_heap, pi);
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
            else if (!jl_has_typevar(newbody, (jl_tvar_t *)var)) {
                // inner instantiation might make a typevar disappear, e.g.
                // NTuple{0,T} => Tuple{}
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
            t = (jl_value_t*)jl_wrap_vararg(T, N, check, nothrow);
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

static jl_value_t *instantiate_with(jl_value_t *t, jl_value_t **env, size_t n, jl_typeenv_t *te)
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
    jl_typeenv_t en = { env->var, vals[0], prev };
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

jl_datatype_t *jl_wrap_Type(jl_value_t *t)
{
    return (jl_datatype_t*)jl_instantiate_unionall(jl_type_type, t);
}

jl_vararg_t *jl_wrap_vararg(jl_value_t *t, jl_value_t *n, int check, int nothrow)
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
                t = normalize_unionalls(t);
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

JL_DLLEXPORT jl_svec_t *jl_compute_fieldtypes(jl_datatype_t *st JL_PROPAGATES_ROOT, void *stack, int cacheable)
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
    jl_typestack_t top;
    top.tt = st;
    top.prev = (jl_typestack_t*)stack;
    st->types = inst_ftypes(wt->types, &env[n - 1], &top, cacheable);
    jl_gc_wb(st, st->types);
    return st->types;
}


void jl_reinstantiate_inner_types(jl_datatype_t *t) // can throw!
{
    assert(jl_is_datatype(t));
    jl_typestack_t top;
    top.tt = t;
    top.prev = NULL;
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
        for (i = 0; i < n; i++)
            env[i].val = jl_svecref(ndt->parameters, i);

        ndt->super = (jl_datatype_t*)inst_type_w_((jl_value_t*)t->super, &env[n - 1], &top, 1, 0);
        jl_gc_wb(ndt, ndt->super);
    }

    if (t->types != jl_emptysvec) {
        for (j = 0; j < jl_array_nrows(partial); j++) {
            jl_datatype_t *ndt = (jl_datatype_t*)jl_array_ptr_ref(partial, j);
            if (ndt == NULL)
                continue;
            for (i = 0; i < n; i++)
                env[i].val = jl_svecref(ndt->parameters, i);
            assert(ndt->types == NULL);
            ndt->types = inst_ftypes(t->types, &env[n - 1], &top, 1);
            jl_gc_wb(ndt, ndt->types);
            if (ndt->isconcretetype) { // cacheable
                jl_compute_field_offsets(ndt);
            }
            jl_array_ptr_set(partial, j, NULL);
        }
        t->name->partial = NULL;
    }
    else {
        assert(jl_field_names(t) == jl_emptysvec);
    }
}

// initialization -------------------------------------------------------------

static jl_tvar_t *tvar(const char *name)
{
    return jl_new_typevar(jl_symbol(name), (jl_value_t*)jl_bottom_type,
                          (jl_value_t*)jl_any_type);
}

void export_jl_small_typeof(void)
{
    memcpy(&jl_small_typeof, &ijl_small_typeof, sizeof(jl_small_typeof));
}

#define XX(name) \
    ijl_small_typeof[(jl_##name##_tag << 4) / sizeof(*ijl_small_typeof)] = jl_##name##_type; \
    jl_##name##_type->smalltag = jl_##name##_tag;
void jl_init_types(void) JL_GC_DISABLED
{
    jl_module_t *core = NULL; // will need to be assigned later

    // create base objects
    jl_datatype_type = jl_new_uninitialized_datatype();
    XX(datatype);
    jl_typename_type = jl_new_uninitialized_datatype();
    jl_symbol_type = jl_new_uninitialized_datatype();
    XX(symbol);
    jl_simplevector_type = jl_new_uninitialized_datatype();
    XX(simplevector);
    jl_methtable_type = jl_new_uninitialized_datatype();

    jl_emptysvec = (jl_svec_t*)jl_gc_permobj(sizeof(void*), jl_simplevector_type, 0);
    jl_set_typetagof(jl_emptysvec, jl_simplevector_tag, GC_OLD_MARKED);
    jl_svec_set_len_unsafe(jl_emptysvec, 0);

    jl_any_type = (jl_datatype_t*)jl_new_abstracttype((jl_value_t*)jl_symbol("Any"), core, NULL, jl_emptysvec);
    jl_any_type->super = jl_any_type;
    jl_nonfunction_mt = jl_any_type->name->mt;
    jl_any_type->name->mt = NULL;

    jl_datatype_t *type_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Type"), core, jl_any_type, jl_emptysvec);
    jl_type_type = (jl_unionall_t*)type_type;
    jl_type_typename = type_type->name;
    jl_type_type_mt = jl_new_method_table(jl_type_typename->name, core);
    jl_type_typename->mt = jl_type_type_mt;

    // initialize them. lots of cycles.
    // NOTE: types are not actually mutable, but we want to ensure they are heap-allocated with stable addresses
    jl_datatype_type->name = jl_new_typename_in(jl_symbol("DataType"), core, 0, 1);
    jl_datatype_type->name->wrapper = (jl_value_t*)jl_datatype_type;
    jl_datatype_type->super = type_type;
    jl_datatype_type->parameters = jl_emptysvec;
    jl_datatype_type->name->n_uninitialized = 8 - 3;
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
    const static uint32_t datatype_constfields[1] = { 0x00000057 }; // (1<<0)|(1<<1)|(1<<2)|(1<<4)|(1<<6)
    const static uint32_t datatype_atomicfields[1] = { 0x00000028 }; // (1<<3)|(1<<5)
    jl_datatype_type->name->constfields = datatype_constfields;
    jl_datatype_type->name->atomicfields = datatype_atomicfields;
    jl_precompute_memoized_dt(jl_datatype_type, 1);

    jl_typename_type->name = jl_new_typename_in(jl_symbol("TypeName"), core, 0, 1);
    jl_typename_type->name->wrapper = (jl_value_t*)jl_typename_type;
    jl_typename_type->name->mt = jl_nonfunction_mt;
    jl_typename_type->super = jl_any_type;
    jl_typename_type->parameters = jl_emptysvec;
    jl_typename_type->name->n_uninitialized = 16 - 2;
    jl_typename_type->name->names = jl_perm_symsvec(16, "name", "module",
                                                    "names", "atomicfields", "constfields",
                                                    "wrapper", "Typeofwrapper", "cache", "linearcache",
                                                    "mt", "partial",
                                                    "hash", "n_uninitialized",
                                                    "flags", // "abstract", "mutable", "mayinlinealloc",
                                                    "max_methods", "constprop_heuristic");
    const static uint32_t typename_constfields[1] = { 0x00003a27 }; // (1<<0)|(1<<1)|(1<<2)|(1<<5)|(1<<9)|(1<<11)|(1<<12)|(1<<13) ; TODO: put back (1<<3)|(1<<4) in this list
    const static uint32_t typename_atomicfields[1] = { 0x00000180 }; // (1<<7)|(1<<8)
    jl_typename_type->name->constfields = typename_constfields;
    jl_typename_type->name->atomicfields = typename_atomicfields;
    jl_precompute_memoized_dt(jl_typename_type, 1);
    jl_typename_type->types = jl_svec(16, jl_symbol_type, jl_any_type /*jl_module_type*/,
                                      jl_simplevector_type, jl_any_type/*jl_voidpointer_type*/, jl_any_type/*jl_voidpointer_type*/,
                                      jl_type_type, jl_type_type, jl_simplevector_type, jl_simplevector_type,
                                      jl_methtable_type, jl_any_type,
                                      jl_any_type /*jl_long_type*/, jl_any_type /*jl_int32_type*/,
                                      jl_any_type /*jl_uint8_type*/,
                                      jl_any_type /*jl_uint8_type*/,
                                      jl_any_type /*jl_uint8_type*/);

    jl_methtable_type->name = jl_new_typename_in(jl_symbol("MethodTable"), core, 0, 1);
    jl_methtable_type->name->wrapper = (jl_value_t*)jl_methtable_type;
    jl_methtable_type->name->mt = jl_nonfunction_mt;
    jl_methtable_type->super = jl_any_type;
    jl_methtable_type->parameters = jl_emptysvec;
    jl_methtable_type->name->n_uninitialized = 11 - 6;
    jl_methtable_type->name->names = jl_perm_symsvec(11, "name", "defs",
                                                     "leafcache", "cache", "max_args",
                                                     "module", "backedges",
                                                     "", "", "offs", "");
    const static uint32_t methtable_constfields[1] = { 0x00000020 }; // (1<<5);
    const static uint32_t methtable_atomicfields[1] = { 0x0000001e }; // (1<<1)|(1<<2)|(1<<3)|(1<<4);
    jl_methtable_type->name->constfields = methtable_constfields;
    jl_methtable_type->name->atomicfields = methtable_atomicfields;
    jl_precompute_memoized_dt(jl_methtable_type, 1);
    jl_methtable_type->types = jl_svec(11, jl_symbol_type, jl_any_type, jl_any_type,
                                       jl_any_type, jl_any_type/*jl_long*/,
                                       jl_any_type/*module*/, jl_any_type/*any vector*/,
                                       jl_any_type/*voidpointer*/, jl_any_type/*int32*/,
                                       jl_any_type/*uint8*/, jl_any_type/*uint8*/);

    jl_symbol_type->name = jl_new_typename_in(jl_symbol("Symbol"), core, 0, 1);
    jl_symbol_type->name->wrapper = (jl_value_t*)jl_symbol_type;
    jl_symbol_type->name->mt = jl_nonfunction_mt;
    jl_symbol_type->super = jl_any_type;
    jl_symbol_type->parameters = jl_emptysvec;
    jl_symbol_type->name->n_uninitialized = 0;
    jl_symbol_type->name->names = jl_emptysvec;
    jl_symbol_type->types = jl_emptysvec;
    jl_precompute_memoized_dt(jl_symbol_type, 1);

    jl_simplevector_type->name = jl_new_typename_in(jl_symbol("SimpleVector"), core, 0, 1);
    jl_simplevector_type->name->wrapper = (jl_value_t*)jl_simplevector_type;
    jl_simplevector_type->name->mt = jl_nonfunction_mt;
    jl_simplevector_type->super = jl_any_type;
    jl_simplevector_type->parameters = jl_emptysvec;
    jl_simplevector_type->name->n_uninitialized = 0;
    jl_simplevector_type->name->names = jl_emptysvec;
    jl_simplevector_type->types = jl_emptysvec;
    jl_precompute_memoized_dt(jl_simplevector_type, 1);

    // now they can be used to create the remaining base kinds and types
    jl_nothing_type = jl_new_datatype(jl_symbol("Nothing"), core, jl_any_type, jl_emptysvec,
                                      jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 0, 0);
    jl_void_type = jl_nothing_type; // deprecated alias
    jl_astaggedvalue(jl_nothing)->header = ((uintptr_t)jl_nothing_type) | GC_OLD_MARKED;
    jl_nothing_type->instance = jl_nothing;

    jl_tvar_type = jl_new_datatype(jl_symbol("TypeVar"), core, jl_any_type, jl_emptysvec,
                                   jl_perm_symsvec(3, "name", "lb", "ub"),
                                   jl_svec(3, jl_symbol_type, jl_any_type, jl_any_type),
                                   jl_emptysvec, 0, 1, 3);
    XX(tvar);
    const static uint32_t tvar_constfields[1] = { 0x00000007 }; // all fields are constant, even though TypeVar itself has identity
    jl_tvar_type->name->constfields = tvar_constfields;

    jl_typeofbottom_type = jl_new_datatype(jl_symbol("TypeofBottom"), core, type_type, jl_emptysvec,
                                           jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 0, 0);
    XX(typeofbottom);
    jl_bottom_type = jl_gc_permobj(0, jl_typeofbottom_type, 0);
    jl_set_typetagof(jl_bottom_type, jl_typeofbottom_tag, GC_OLD_MARKED);
    jl_typeofbottom_type->instance = jl_bottom_type;

    jl_unionall_type = jl_new_datatype(jl_symbol("UnionAll"), core, type_type, jl_emptysvec,
                                       jl_perm_symsvec(2, "var", "body"),
                                       jl_svec(2, jl_tvar_type, jl_any_type),
                                       jl_emptysvec, 0, 0, 2);
    XX(unionall);
    // It seems like we probably usually end up needing the box for kinds (often used in an Any context), so force it to exist
    jl_unionall_type->name->mayinlinealloc = 0;

    jl_uniontype_type = jl_new_datatype(jl_symbol("Union"), core, type_type, jl_emptysvec,
                                        jl_perm_symsvec(2, "a", "b"),
                                        jl_svec(2, jl_any_type, jl_any_type),
                                        jl_emptysvec, 0, 0, 2);
    XX(uniontype);
    // It seems like we probably usually end up needing the box for kinds (often used in an Any context), so force it to exist
    jl_uniontype_type->name->mayinlinealloc = 0;

    jl_tvar_t *tttvar = tvar("T");
    type_type->parameters = jl_svec(1, tttvar);
    jl_precompute_memoized_dt(type_type, 0); // update the hash value ASAP
    type_type->hasfreetypevars = 1;
    type_type->ismutationfree = 1;
    jl_type_typename->wrapper = jl_new_struct(jl_unionall_type, tttvar, (jl_value_t*)jl_type_type);
    jl_type_type = (jl_unionall_t*)jl_type_typename->wrapper;

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

    jl_typeofbottom_type->super = jl_wrap_Type(jl_bottom_type);
    jl_typeofbottom_type->super->layout = jl_typeofbottom_type->layout; // the only abstract type with a layout
    jl_emptytuple_type = (jl_datatype_t*)jl_apply_tuple_type(jl_emptysvec, 0);
    jl_emptytuple = jl_gc_permobj(0, jl_emptytuple_type, 0);
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

    jl_slotnumber_type = jl_new_datatype(jl_symbol("SlotNumber"), core, jl_any_type, jl_emptysvec,
                                         jl_perm_symsvec(1, "id"),
                                         jl_svec1(jl_long_type),
                                         jl_emptysvec, 0, 0, 1);

    jl_argument_type = jl_new_datatype(jl_symbol("Argument"), core, jl_any_type, jl_emptysvec,
                                       jl_perm_symsvec(1, "n"),
                                       jl_svec1(jl_long_type),
                                       jl_emptysvec, 0, 0, 1);

    jl_init_int32_int64_cache();

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
    jl_function_type->name->mt = NULL; // subtypes of Function have independent method tables
    jl_builtin_type->name->mt = NULL;  // so they don't share the Any type table

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
                        jl_ulong_type, jl_ulong_type, jl_any_type/*jl_binding_partition_type*/, jl_ulong_type),
                        jl_emptysvec, 0, 1, 0);
    const static uint32_t binding_partition_atomicfields[] = { 0b01101 }; // Set fields 1, 3, 4 as atomic
    jl_binding_partition_type->name->atomicfields = binding_partition_atomicfields;

    jl_binding_type =
        jl_new_datatype(jl_symbol("Binding"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(5, "globalref", "value", "partitions", "backedges", "flags"),
                        jl_svec(5, jl_any_type/*jl_globalref_type*/, jl_any_type, jl_binding_partition_type,
                                   jl_any_type, jl_uint8_type),
                        jl_emptysvec, 0, 1, 0);
    const static uint32_t binding_atomicfields[] = { 0x0005 }; // Set fields 2, 3 as atomic
    jl_binding_type->name->atomicfields = binding_atomicfields;
    const static uint32_t binding_constfields[] = { 0x0001 }; // Set fields 1 as constant
    jl_binding_type->name->constfields = binding_constfields;

    jl_globalref_type =
        jl_new_datatype(jl_symbol("GlobalRef"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(3, "mod", "name", "binding"),
                        jl_svec(3, jl_module_type, jl_symbol_type, jl_binding_type),
                        jl_emptysvec, 0, 0, 3);

    core = jl_new_module(jl_symbol("Core"), NULL);
    core->parent = core;
    jl_type_typename->mt->module = core;
    jl_core_module = core;
    core = NULL; // not ready yet to use

    tv = jl_svec1(tvar("Backend"));
    jl_addrspace_typename =
        jl_new_primitivetype((jl_value_t*)jl_symbol("AddrSpace"), core, jl_any_type, tv, 8)->name;
    jl_addrspace_type = (jl_unionall_t*)jl_addrspace_typename->wrapper;
    jl_addrspacecore_type = (jl_datatype_t*)jl_apply_type1((jl_value_t*)jl_addrspace_type, (jl_value_t*)jl_core_module);
    jl_value_t *cpumem = jl_permbox8(jl_addrspacecore_type, 0, 0);

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
    jl_atomic_store_relaxed(&jl_nonfunction_mt->leafcache, (jl_genericmemory_t*)jl_an_empty_memory_any);
    jl_atomic_store_relaxed(&jl_type_type_mt->leafcache, (jl_genericmemory_t*)jl_an_empty_memory_any);

    // finish initializing module Core
    core = jl_core_module;
    jl_atomic_store_relaxed(&core->bindingkeyset, (jl_genericmemory_t*)jl_an_empty_memory_any);
    // export own name, so "using Foo" makes "Foo" itself visible
    jl_set_const(core, core->name, (jl_value_t*)core);
    jl_module_public(core, core->name, 1);
    jl_set_const(core, jl_symbol("CPU"), (jl_value_t*)cpumem);
    core = NULL;

    jl_expr_type =
        jl_new_datatype(jl_symbol("Expr"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "head", "args"),
                        jl_svec(2, jl_symbol_type, jl_array_any_type),
                        jl_emptysvec, 0, 1, 2);

    jl_value_t *symornothing[2] = { (jl_value_t*)jl_symbol_type, (jl_value_t*)jl_void_type };
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

    jl_gotoifnot_type =
        jl_new_datatype(jl_symbol("GotoIfNot"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "cond", "dest"),
                        jl_svec(2, jl_any_type, jl_long_type),
                        jl_emptysvec, 0, 0, 2);

    jl_enternode_type =
        jl_new_datatype(jl_symbol("EnterNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "catch_dest", "scope"),
                        jl_svec(2, jl_long_type, jl_any_type),
                        jl_emptysvec, 0, 0, 1);

    jl_returnnode_type =
        jl_new_datatype(jl_symbol("ReturnNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "val"),
                        jl_svec(1, jl_any_type),
                        jl_emptysvec, 0, 0, 0);

    jl_pinode_type =
        jl_new_datatype(jl_symbol("PiNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "val", "typ"),
                        jl_svec(2, jl_any_type, jl_any_type),
                        jl_emptysvec, 0, 0, 2);

    jl_phinode_type =
        jl_new_datatype(jl_symbol("PhiNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "edges", "values"),
                        jl_svec(2, jl_array_int32_type, jl_array_any_type),
                        jl_emptysvec, 0, 0, 2);

    jl_phicnode_type =
        jl_new_datatype(jl_symbol("PhiCNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "values"),
                        jl_svec(1, jl_array_any_type),
                        jl_emptysvec, 0, 0, 1);

    jl_upsilonnode_type =
        jl_new_datatype(jl_symbol("UpsilonNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "val"),
                        jl_svec(1, jl_any_type),
                        jl_emptysvec, 0, 0, 0);

    jl_quotenode_type =
        jl_new_datatype(jl_symbol("QuoteNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "value"),
                        jl_svec(1, jl_any_type),
                        jl_emptysvec, 0, 0, 1);

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
                        jl_perm_symsvec(31,
                            "name",
                            "module",
                            "file",
                            "line",
                            "primary_world", // atomic
                            "deleted_world", // atomic
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
                            "constprop",
                            "max_varargs",
                            "purity"),
                        jl_svec(31,
                            jl_symbol_type,
                            jl_module_type,
                            jl_symbol_type,
                            jl_int32_type,
                            jl_ulong_type,
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
                            jl_uint16_type),
                        jl_emptysvec,
                        0, 1, 10);
    //const static uint32_t method_constfields[1] = { 0b0 }; // (1<<0)|(1<<1)|(1<<2)|(1<<3)|(1<<6)|(1<<9)|(1<<10)|(1<<17)|(1<<21)|(1<<22)|(1<<23)|(1<<24)|(1<<25)|(1<<26)|(1<<27)|(1<<28)|(1<<29)|(1<<30);
    //jl_method_type->name->constfields = method_constfields;
    const static uint32_t method_atomicfields[1] = { 0x00000030 }; // (1<<4)|(1<<5)
    jl_method_type->name->atomicfields = method_atomicfields;

    jl_method_instance_type =
        jl_new_datatype(jl_symbol("MethodInstance"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(7,
                            "def",
                            "specTypes",
                            "sparam_vals",
                            "backedges",
                            "cache",
                            "cache_with_orig",
                            "flags"),
                        jl_svec(7,
                            jl_new_struct(jl_uniontype_type, jl_method_type, jl_module_type),
                            jl_any_type,
                            jl_simplevector_type,
                            jl_array_any_type,
                            jl_any_type/*jl_code_instance_type*/,
                            jl_bool_type,
                            jl_bool_type),
                        jl_emptysvec,
                        0, 1, 3);
    // These fields should be constant, but Serialization wants to mutate them in initialization
    //const static uint32_t method_instance_constfields[1] = { 0b0000111 }; // fields 1, 2, 3
    const static uint32_t method_instance_atomicfields[1]  = { 0b1010000 }; // fields 5, 7
    //Fields 4 and 5 must be protected by method->write_lock, and thus all operations on jl_method_instance_t are threadsafe.
    //jl_method_instance_type->name->constfields = method_instance_constfields;
    jl_method_instance_type->name->atomicfields = method_instance_atomicfields;

    jl_code_instance_type =
        jl_new_datatype(jl_symbol("CodeInstance"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(17,
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
                            //"absolute_max",
                            "ipo_purity_bits",
                            "analysis_results",
                            "specsigflags", "precompile",
                            "invoke", "specptr"), // function object decls
                        jl_svec(17,
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
                            //jl_bool_type,
                            jl_uint32_type,
                            jl_any_type,
                            jl_bool_type,
                            jl_bool_type,
                            jl_any_type, jl_any_type), // fptrs
                        jl_emptysvec,
                        0, 1, 1);
    jl_svecset(jl_code_instance_type->types, 2, jl_code_instance_type);
    const static uint32_t code_instance_constfields[1]  = { 0b00001000011100011 }; // Set fields 1, 2, 6-8, 13 as const
    const static uint32_t code_instance_atomicfields[1] = { 0b11110111100011100 }; // Set fields 3-5, 9-12, 14-17 as atomic
    // Fields 4-5 are only operated on by construction and deserialization, so are effectively const at runtime
    // Fields ipo_purity_bits and analysis_results are not currently threadsafe or reliable, as they get mutated after optimization, but are not declared atomic
    // and there is no way to tell (during inference) if their value is finalized yet (to wait for them to be narrowed if applicable)
    jl_code_instance_type->name->constfields = code_instance_constfields;
    jl_code_instance_type->name->atomicfields = code_instance_atomicfields;

    jl_const_type = jl_new_datatype(jl_symbol("Const"), core, jl_any_type, jl_emptysvec,
                                       jl_perm_symsvec(1, "val"),
                                       jl_svec1(jl_any_type),
                                       jl_emptysvec, 0, 0, 1);

    jl_partial_struct_type = jl_new_datatype(jl_symbol("PartialStruct"), core, jl_any_type, jl_emptysvec,
                                       jl_perm_symsvec(3, "typ", "undef", "fields"),
                                       jl_svec(3, jl_any_type, jl_any_type, jl_array_any_type),
                                       jl_emptysvec, 0, 0, 3);

    jl_interconditional_type = jl_new_datatype(jl_symbol("InterConditional"), core, jl_any_type, jl_emptysvec,
                                          jl_perm_symsvec(3, "slot", "thentype", "elsetype"),
                                          jl_svec(3, jl_long_type, jl_any_type, jl_any_type),
                                          jl_emptysvec, 0, 0, 3);

    jl_method_match_type = jl_new_datatype(jl_symbol("MethodMatch"), core, jl_any_type, jl_emptysvec,
                                       jl_perm_symsvec(4, "spec_types", "sparams", "method", "fully_covers"),
                                       jl_svec(4, jl_type_type, jl_simplevector_type, jl_method_type, jl_bool_type),
                                       jl_emptysvec, 0, 0, 4);

    // all Kinds share the Type method table (not the nonfunction one)
    jl_unionall_type->name->mt =
        jl_uniontype_type->name->mt =
        jl_datatype_type->name->mt =
        jl_typeofbottom_type->name->mt =
            jl_type_type_mt;

    jl_intrinsic_type = jl_new_primitivetype((jl_value_t*)jl_symbol("IntrinsicFunction"), core,
                                             jl_builtin_type, jl_emptysvec, 32);

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

    jl_task_type = (jl_datatype_t*)
        jl_new_datatype(jl_symbol("Task"),
                        NULL,
                        jl_any_type,
                        jl_emptysvec,
                        jl_perm_symsvec(27,
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
                                        "pad00",
                                        "pad01",
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
                                        "finished_at"),
                        jl_svec(27,
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
                                jl_uint64_type),
                        jl_emptysvec,
                        0, 1, 6);
    XX(task);
    jl_value_t *listt = jl_new_struct(jl_uniontype_type, jl_task_type, jl_nothing_type);
    jl_svecset(jl_task_type->types, 0, listt);
    // Set field 20 (metrics_enabled) as const
    // Set fields 8 (_state) and 24-27 (metric counters) as atomic
    const static uint32_t task_constfields[1]  = { 0b00000000000010000000000000000000 };
    const static uint32_t task_atomicfields[1] = { 0b00000111100000000000000010000000 };
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

    jl_partial_opaque_type = jl_new_datatype(jl_symbol("PartialOpaque"), core, jl_any_type, jl_emptysvec,
        jl_perm_symsvec(4, "typ", "env", "parent", "source"),
        jl_svec(4, jl_type_type, jl_any_type, jl_method_instance_type, jl_any_type),
        jl_emptysvec, 0, 0, 4);

    // complete builtin type metadata
    jl_uint8pointer_type = (jl_datatype_t*)jl_apply_type1((jl_value_t*)jl_pointer_type, (jl_value_t*)jl_uint8_type);
    jl_svecset(jl_datatype_type->types, 5, jl_voidpointer_type);
    jl_svecset(jl_datatype_type->types, 6, jl_int32_type);
    jl_svecset(jl_datatype_type->types, 7, jl_uint16_type);
    jl_svecset(jl_typename_type->types, 1, jl_module_type);
    jl_svecset(jl_typename_type->types, 3, jl_voidpointer_type);
    jl_svecset(jl_typename_type->types, 4, jl_voidpointer_type);
    jl_svecset(jl_typename_type->types, 5, jl_type_type);
    jl_svecset(jl_typename_type->types, 6, jl_type_type);
    jl_svecset(jl_typename_type->types, 11, jl_long_type);
    jl_svecset(jl_typename_type->types, 12, jl_int32_type);
    jl_svecset(jl_typename_type->types, 13, jl_uint8_type);
    jl_svecset(jl_typename_type->types, 14, jl_uint8_type);
    jl_svecset(jl_typename_type->types, 15, jl_uint8_type);
    jl_svecset(jl_methtable_type->types, 4, jl_long_type);
    jl_svecset(jl_methtable_type->types, 5, jl_module_type);
    jl_svecset(jl_methtable_type->types, 6, jl_array_any_type);
    jl_svecset(jl_methtable_type->types, 7, jl_long_type); // voidpointer
    jl_svecset(jl_methtable_type->types, 8, jl_long_type); // uint32_t plus alignment
    jl_svecset(jl_methtable_type->types, 9, jl_uint8_type);
    jl_svecset(jl_methtable_type->types, 10, jl_uint8_type);
    jl_svecset(jl_method_type->types, 13, jl_method_instance_type);
    //jl_svecset(jl_debuginfo_type->types, 0, jl_method_instance_type); // union(jl_method_instance_type, jl_method_type, jl_symbol_type)
    jl_svecset(jl_method_instance_type->types, 4, jl_code_instance_type);
    jl_svecset(jl_code_instance_type->types, 15, jl_voidpointer_type);
    jl_svecset(jl_code_instance_type->types, 16, jl_voidpointer_type);
    jl_svecset(jl_binding_type->types, 0, jl_globalref_type);
    jl_svecset(jl_binding_type->types, 3, jl_array_any_type);
    jl_svecset(jl_binding_partition_type->types, 3, jl_binding_partition_type);

    jl_compute_field_offsets(jl_datatype_type);
    jl_compute_field_offsets(jl_typename_type);
    jl_compute_field_offsets(jl_uniontype_type);
    jl_compute_field_offsets(jl_tvar_type);
    jl_compute_field_offsets(jl_methtable_type);
    jl_compute_field_offsets(jl_method_instance_type);
    jl_compute_field_offsets(jl_code_instance_type);
    jl_compute_field_offsets(jl_unionall_type);
    jl_compute_field_offsets(jl_simplevector_type);
    jl_compute_field_offsets(jl_symbol_type);
    jl_compute_field_offsets(jl_binding_partition_type);

    // override ismutationfree for builtin types that are mutable for identity
    jl_string_type->ismutationfree = jl_string_type->isidentityfree = 1;
    jl_symbol_type->ismutationfree = jl_symbol_type->isidentityfree = 1;
    jl_simplevector_type->ismutationfree = jl_simplevector_type->isidentityfree = 1;
    jl_typename_type->ismutationfree = 1;
    jl_datatype_type->ismutationfree = 1;
    jl_uniontype_type->ismutationfree = 1;
    jl_unionall_type->ismutationfree = 1;
    assert(((jl_datatype_t*)jl_array_any_type)->ismutationfree == 0);
    assert(((jl_datatype_t*)jl_array_uint8_type)->ismutationfree == 0);

    // Technically not ismutationfree, but there's a separate system to deal
    // with mutations for global state.
    jl_module_type->ismutationfree = 1;
    // Module object identity is determined by its name and parent name.
    jl_module_type->isidentityfree = 1;

    export_jl_small_typeof();
}

static jl_value_t *core(const char *name)
{
    return jl_get_global(jl_core_module, jl_symbol(name));
}

jl_debuginfo_t *jl_nulldebuginfo;

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

    jl_bool_type->super = jl_integer_type;
    jl_uint8_type->super = jl_unsigned_type;
    jl_uint16_type->super = jl_unsigned_type;
    jl_uint32_type->super = jl_unsigned_type;
    jl_uint64_type->super = jl_unsigned_type;
    jl_int32_type->super = jl_signed_type;
    jl_int64_type->super = jl_signed_type;

    jl_errorexception_type = (jl_datatype_t*)core("ErrorException");
    jl_stackovf_exception  = jl_new_struct_uninit((jl_datatype_t*)core("StackOverflowError"));
    jl_diverror_exception  = jl_new_struct_uninit((jl_datatype_t*)core("DivideError"));
    jl_undefref_exception  = jl_new_struct_uninit((jl_datatype_t*)core("UndefRefError"));
    jl_undefvarerror_type  = (jl_datatype_t*)core("UndefVarError");
    jl_fielderror_type     = (jl_datatype_t*)core("FieldError");
    jl_atomicerror_type    = (jl_datatype_t*)core("ConcurrencyViolationError");
    jl_interrupt_exception = jl_new_struct_uninit((jl_datatype_t*)core("InterruptException"));
    jl_boundserror_type    = (jl_datatype_t*)core("BoundsError");
    jl_memory_exception    = jl_new_struct_uninit((jl_datatype_t*)core("OutOfMemoryError"));
    jl_readonlymemory_exception = jl_new_struct_uninit((jl_datatype_t*)core("ReadOnlyMemoryError"));
    jl_typeerror_type      = (jl_datatype_t*)core("TypeError");
    jl_argumenterror_type  = (jl_datatype_t*)core("ArgumentError");
    jl_methoderror_type    = (jl_datatype_t*)core("MethodError");
    jl_loaderror_type      = (jl_datatype_t*)core("LoadError");
    jl_initerror_type      = (jl_datatype_t*)core("InitError");
    jl_missingcodeerror_type = (jl_datatype_t*)core("MissingCodeError");
    jl_precompilable_error = jl_new_struct_uninit((jl_datatype_t*)core("PrecompilableError"));
    jl_pair_type           = core("Pair");
    jl_kwcall_func         = core("kwcall");
    jl_kwcall_mt           = ((jl_datatype_t*)jl_typeof(jl_kwcall_func))->name->mt;
    jl_atomic_store_relaxed(&jl_kwcall_mt->max_args, 0);

    jl_weakref_type = (jl_datatype_t*)core("WeakRef");
    jl_vecelement_typename = ((jl_datatype_t*)jl_unwrap_unionall(core("VecElement")))->name;
    jl_nulldebuginfo = (jl_debuginfo_t*)core("NullDebugInfo");
    jl_abioverride_type = (jl_datatype_t*)core("ABIOverride");

    jl_init_box_caches();

    // set module field of primitive types
    jl_svec_t *bindings = jl_atomic_load_relaxed(&jl_core_module->bindings);
    jl_value_t **table = jl_svec_data(bindings);
    for (size_t i = 0; i < jl_svec_len(bindings); i++) {
        if (table[i] != jl_nothing) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            jl_value_t *v = jl_get_binding_value(b);
            if (v) {
                if (jl_is_unionall(v))
                    v = jl_unwrap_unionall(v);
                if (jl_is_datatype(v)) {
                    jl_datatype_t *tt = (jl_datatype_t*)v;
                    tt->name->module = jl_core_module;
                    if (tt->name->mt)
                        tt->name->mt->module = jl_core_module;
                }
            }
        }
    }

    export_jl_small_typeof();
}

void post_image_load_hooks(void) {
    // Ensure that `Base` has been loaded.
    assert(jl_base_module != NULL);

    jl_module_t *libc_module = (jl_module_t *)jl_get_global(jl_base_module, jl_symbol("Libc"));
    if (libc_module) {
        jl_libdl_module = (jl_module_t *)jl_get_global(libc_module, jl_symbol("Libdl"));
    }
    if (jl_libdl_module) {
        jl_libdl_dlopen_func = jl_get_global(
            jl_libdl_module,
            jl_symbol("dlopen")
        );
    }
}
#undef XX

#ifdef __cplusplus
}
#endif
