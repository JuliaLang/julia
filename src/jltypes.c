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

// compute empirical max-probe for a given size
#define max_probe(size) ((size) <= 1024 ? 16 : (size) >> 6)
#define h2index(hv, sz) (size_t)((hv) & ((sz)-1))

jl_datatype_t *jl_any_type;
jl_unionall_t *jl_type_type;
jl_typename_t *jl_type_typename;
jl_methtable_t *jl_type_type_mt;
jl_methtable_t *jl_nonfunction_mt;
jl_datatype_t *jl_typename_type;
jl_datatype_t *jl_symbol_type;
jl_datatype_t *jl_ssavalue_type;
jl_datatype_t *jl_abstractslot_type;
jl_datatype_t *jl_slotnumber_type;
jl_datatype_t *jl_typedslot_type;
jl_datatype_t *jl_argument_type;
jl_datatype_t *jl_simplevector_type;
jl_typename_t *jl_tuple_typename;
jl_datatype_t *jl_anytuple_type;
jl_datatype_t *jl_emptytuple_type;
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

jl_datatype_t *jl_typeofbottom_type;
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
jl_datatype_t *jl_signed_type;

JL_DLLEXPORT jl_value_t *jl_emptytuple;
jl_svec_t *jl_emptysvec;
jl_value_t *jl_nothing;

JL_DLLEXPORT jl_value_t *jl_true;
JL_DLLEXPORT jl_value_t *jl_false;

jl_unionall_t *jl_typetype_type;

jl_unionall_t *jl_array_type;
jl_typename_t *jl_array_typename;
jl_value_t *jl_array_uint8_type;
jl_value_t *jl_array_any_type;
jl_value_t *jl_array_symbol_type;
jl_value_t *jl_array_int32_type;
jl_datatype_t *jl_weakref_type;
jl_datatype_t *jl_abstractstring_type;
jl_datatype_t *jl_string_type;
jl_datatype_t *jl_expr_type;
jl_datatype_t *jl_globalref_type;
jl_datatype_t *jl_linenumbernode_type;
jl_datatype_t *jl_gotonode_type;
jl_datatype_t *jl_gotoifnot_type;
jl_datatype_t *jl_returnnode_type;
jl_datatype_t *jl_pinode_type;
jl_datatype_t *jl_phinode_type;
jl_datatype_t *jl_phicnode_type;
jl_datatype_t *jl_upsilonnode_type;
jl_datatype_t *jl_quotenode_type;
jl_datatype_t *jl_newvarnode_type;
jl_datatype_t *jl_intrinsic_type;
jl_datatype_t *jl_method_type;
jl_datatype_t *jl_methtable_type;
jl_datatype_t *jl_typemap_entry_type;
jl_datatype_t *jl_typemap_level_type;
jl_datatype_t *jl_method_instance_type;
jl_datatype_t *jl_code_instance_type;
jl_datatype_t *jl_code_info_type;
jl_datatype_t *jl_module_type;
jl_datatype_t *jl_errorexception_type;
jl_datatype_t *jl_argumenterror_type;
jl_datatype_t *jl_typeerror_type;
jl_datatype_t *jl_methoderror_type;
jl_datatype_t *jl_loaderror_type;
jl_datatype_t *jl_initerror_type;
jl_datatype_t *jl_undefvarerror_type;
jl_datatype_t *jl_lineinfonode_type;
jl_unionall_t *jl_ref_type;
jl_unionall_t *jl_pointer_type;
jl_typename_t *jl_pointer_typename;
jl_unionall_t *jl_llvmpointer_type;
jl_typename_t *jl_llvmpointer_typename;
jl_datatype_t *jl_void_type; // deprecated
jl_datatype_t *jl_nothing_type;
jl_datatype_t *jl_voidpointer_type;
jl_datatype_t *jl_uint8pointer_type;
jl_typename_t *jl_namedtuple_typename;
jl_unionall_t *jl_namedtuple_type;
jl_datatype_t *jl_task_type;
jl_value_t *jl_an_empty_vec_any;
jl_value_t *jl_an_empty_string;
jl_value_t *jl_stackovf_exception;
#ifdef SEGV_EXCEPTION
jl_value_t *jl_segv_exception;
#endif
JL_DLLEXPORT jl_value_t *jl_diverror_exception;
JL_DLLEXPORT jl_value_t *jl_undefref_exception;
jl_value_t *jl_interrupt_exception;
jl_datatype_t *jl_boundserror_type;
jl_value_t *jl_memory_exception;
jl_value_t *jl_readonlymemory_exception;

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

static int has_free_typevars(jl_value_t *v, jl_typeenv_t *env) JL_NOTSAFEPOINT
{
    if (jl_typeis(v, jl_tvar_type)) {
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

JL_DLLEXPORT int jl_has_free_typevars(jl_value_t *v) JL_NOTSAFEPOINT
{
    return has_free_typevars(v, NULL);
}

static void find_free_typevars(jl_value_t *v, jl_typeenv_t *env, jl_array_t *out)
{
    if (jl_typeis(v, jl_tvar_type)) {
        if (!typeenv_has(env, (jl_tvar_t*)v))
            jl_array_ptr_1d_push(out, v);
    }
    else if (jl_is_uniontype(v)) {
        find_free_typevars(((jl_uniontype_t*)v)->a, env, out);
        find_free_typevars(((jl_uniontype_t*)v)->b, env, out);
    }
    else if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        jl_typeenv_t newenv = { ua->var, NULL, env };
        find_free_typevars(ua->var->lb, env, out);
        find_free_typevars(ua->var->ub, env, out);
        find_free_typevars(ua->body, &newenv, out);
    }
    else if (jl_is_datatype(v)) {
        if (!((jl_datatype_t*)v)->hasfreetypevars)
            return;
        size_t i;
        for (i=0; i < jl_nparams(v); i++)
            find_free_typevars(jl_tparam(v,i), env, out);
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
static int jl_has_bound_typevars(jl_value_t *v, jl_typeenv_t *env) JL_NOTSAFEPOINT
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

int jl_count_union_components(jl_value_t *v)
{
    if (!jl_is_uniontype(v)) return 1;
    jl_uniontype_t *u = (jl_uniontype_t*)v;
    return jl_count_union_components(u->a) + jl_count_union_components(u->b);
}

// Return the `*pi`th element of a nested type union, according to a
// standard traversal order. Anything that is not itself a `Union` is
// considered an "element". `*pi` is destroyed in the process.
static jl_value_t *nth_union_component(jl_value_t *v, int *pi) JL_NOTSAFEPOINT
{
    if (!jl_is_uniontype(v)) {
        if (*pi == 0)
            return v;
        (*pi)--;
        return NULL;
    }
    jl_uniontype_t *u = (jl_uniontype_t*)v;
    jl_value_t *a = nth_union_component(u->a, pi);
    if (a) return a;
    return nth_union_component(u->b, pi);
}

jl_value_t *jl_nth_union_component(jl_value_t *v, int i) JL_NOTSAFEPOINT
{
    return nth_union_component(v, &i);
}

// inverse of jl_nth_union_component
int jl_find_union_component(jl_value_t *haystack, jl_value_t *needle, unsigned *nth) JL_NOTSAFEPOINT
{
    if (jl_is_uniontype(haystack)) {
        if (jl_find_union_component(((jl_uniontype_t*)haystack)->a, needle, nth))
            return 1;
        if (jl_find_union_component(((jl_uniontype_t*)haystack)->b, needle, nth))
            return 1;
        return 0;
    }
    if (needle == haystack)
        return 1;
    (*nth)++;
    return 0;
}

static void flatten_type_union(jl_value_t **types, size_t n, jl_value_t **out, size_t *idx) JL_NOTSAFEPOINT
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
int datatype_name_cmp(jl_value_t *a, jl_value_t *b) JL_NOTSAFEPOINT
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
int union_sort_cmp(const void *ap, const void *bp) JL_NOTSAFEPOINT
{
    jl_value_t *a = *(jl_value_t**)ap;
    jl_value_t *b = *(jl_value_t**)bp;
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

JL_DLLEXPORT jl_value_t *jl_type_union(jl_value_t **ts, size_t n)
{
    if (n == 0) return (jl_value_t*)jl_bottom_type;
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *pi = ts[i];
        if (!(jl_is_type(pi) || jl_is_typevar(pi)) || jl_is_vararg_type(pi))
            jl_type_error("Union", (jl_value_t*)jl_type_type, pi);
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
    qsort(temp, nt, sizeof(jl_value_t*), union_sort_cmp);
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

JL_DLLEXPORT jl_value_t *jl_type_unionall(jl_tvar_t *v, jl_value_t *body)
{
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
    // TOOD: This shouldn't be necessary
    JL_GC_PROMISE_ROOTED(tt);
    size_t tnp = jl_nparams(tt);
    if (n != tnp)
        return 0;
    if (tt->name == jl_type_typename) {
        // for Type{T}, require `typeof(T)` to match also, to avoid incorrect
        // dispatch from changing the type of something.
        // this should work because `Type`s don't have uids, and aren't the
        // direct tags of values so we don't rely on pointer equality.
        jl_value_t *kj = key[0];
        jl_value_t *tj = jl_tparam0(tt);
        return (kj == tj || (jl_typeof(tj) == jl_typeof(kj) && jl_types_equal(tj, kj)));
    }
    for (j = 0; j < n; j++) {
        jl_value_t *kj = key[j];
        jl_value_t *tj = jl_svecref(tt->parameters, j);
        if (tj != kj) {
            // require exact same Type{T}. see e.g. issue #22842
            if (jl_is_type_type(tj) || jl_is_type_type(kj))
                return 0;
            if (jl_is_concrete_type(tj) || jl_is_concrete_type(kj))
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
    // TOOD: This shouldn't be necessary
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
    jl_datatype_t **tab = (jl_datatype_t**)jl_svec_data(cache);
    size_t index = h2index(hv, sz);
    size_t orig = index;
    size_t iter = 0;
    do {
        jl_datatype_t *val = jl_atomic_load_relaxed(&tab[index]);
        if (val == NULL)
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
    jl_datatype_t **tab = (jl_datatype_t**)jl_svec_data(cache);
    size_t index = h2index(hv, sz);
    size_t orig = index;
    size_t iter = 0;
    do {
        jl_datatype_t *val = jl_atomic_load_relaxed(&tab[index]);
        if (val == NULL)
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
    jl_datatype_t **data = (jl_datatype_t**)jl_svec_data(cache);
    size_t cl = jl_svec_len(cache);
    ssize_t i;
    for (i = 0; i < cl; i++) {
        jl_datatype_t *tt = jl_atomic_load_relaxed(&data[i]);
        if (tt == NULL)
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
    jl_datatype_t **data = (jl_datatype_t**)jl_svec_data(cache);
    size_t cl = jl_svec_len(cache);
    ssize_t i;
    for (i = 0; i < cl; i++) {
        jl_datatype_t *tt = jl_atomic_load_relaxed(&data[i]);
        if (tt == NULL)
            return ~i;
        if (typekeyvalue_eq(tt, key1, key, n, 1))
            return i;
    }
    return ~cl;
}

static jl_value_t *lookup_type(jl_typename_t *tn, jl_value_t **key, size_t n)
{
    JL_TIMING(TYPE_CACHE_LOOKUP);
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
    JL_TIMING(TYPE_CACHE_LOOKUP);
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

static int cache_insert_type_set_(jl_svec_t *a, jl_datatype_t *val, uint_t hv)
{
    jl_datatype_t **tab = (jl_datatype_t**)jl_svec_data(a);
    size_t sz = jl_svec_len(a);
    if (sz <= 1)
        return 0;
    size_t orig, index, iter;
    iter = 0;
    index = h2index(hv, sz);
    orig = index;
    size_t maxprobe = max_probe(sz);
    do {
        if (tab[index] == NULL) {
            jl_atomic_store_release(&tab[index], val);
            jl_gc_wb(a, val);
            return 1;
        }
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);

    return 0;
}

static jl_svec_t *cache_rehash_set(jl_svec_t *a, size_t newsz);

static void cache_insert_type_set(jl_datatype_t *val, uint_t hv)
{
    jl_svec_t *a = val->name->cache;
    while (1) {
        JL_GC_PROMISE_ROOTED(a);
        if (cache_insert_type_set_(a, val, hv))
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

static jl_svec_t *cache_rehash_set(jl_svec_t *a, size_t newsz)
{
    jl_datatype_t **ol = (jl_datatype_t**)jl_svec_data(a);
    size_t sz = jl_svec_len(a);
    while (1) {
        size_t i;
        jl_svec_t *newa = jl_alloc_svec(newsz);
        JL_GC_PUSH1(&newa);
        for (i = 0; i < sz; i += 1) {
            jl_datatype_t *val = ol[i];
            if (val != NULL) {
                uint_t hv = val->hash;
                if (!cache_insert_type_set_(newa, val, hv)) {
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
    jl_svec_t *cache = type->name->linearcache;
    assert(jl_is_svec(cache));
    size_t n = jl_svec_len(cache);
    if (n == 0 || jl_svecref(cache, n - 1) != NULL) {
        jl_svec_t *nc = jl_alloc_svec(n < 8 ? 8 : (n*3)>>1);
        memcpy(jl_svec_data(nc), jl_svec_data(cache), sizeof(void*) * n);
        jl_atomic_store_release(&type->name->linearcache, nc);
        jl_gc_wb(type->name, nc);
        cache = nc;
        n = jl_svec_len(nc);
    }
    assert(jl_svecref(cache, insert_at) == NULL);
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
    JL_TIMING(TYPE_CACHE_INSERT);
    assert(is_cacheable(type));
    jl_value_t **key = jl_svec_data(type->parameters);
    int n = jl_svec_len(type->parameters);
    unsigned hv = typekey_hash(type->name, key, n, 0);
    if (hv) {
        assert(hv == type->hash);
        cache_insert_type_set(type, hv);
    }
    else {
        ssize_t idx = lookup_type_idx_linear(type->name->linearcache, key, n);
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
                                       int cacheable, jl_typestack_t *stack, jl_typeenv_t *env);

// Build an environment mapping a TypeName's parameters to parameter values.
// This is the environment needed for instantiating a type's supertype and field types.
static jl_value_t *inst_datatype_env(jl_value_t *dt, jl_svec_t *p, jl_value_t **iparams, size_t ntp,
                                     int cacheable, jl_typestack_t *stack, jl_typeenv_t *env, int c)
{
    if (jl_is_datatype(dt))
        return inst_datatype_inner((jl_datatype_t*)dt, p, iparams, ntp, cacheable, stack, env);
    assert(jl_is_unionall(dt));
    jl_unionall_t *ua = (jl_unionall_t*)dt;
    jl_typeenv_t e = { ua->var, iparams[c], env };
    return inst_datatype_env(ua->body, p, iparams, ntp, cacheable, stack, &e, c + 1);
}

jl_value_t *jl_apply_type(jl_value_t *tc, jl_value_t **params, size_t n)
{
    if (tc == (jl_value_t*)jl_anytuple_type)
        return (jl_value_t*)jl_apply_tuple_type_v(params, n);
    if (tc == (jl_value_t*)jl_uniontype_type)
        return (jl_value_t*)jl_type_union(params, n);
    size_t i;
    if (n > 1) {
        // detect common case of applying a wrapper, where we know that all parameters will
        // end up as direct parameters of a certain datatype, which can be optimized.
        jl_value_t *u = jl_unwrap_unionall(tc);
        if (jl_is_datatype(u) && n == jl_nparams((jl_datatype_t*)u) &&
            ((jl_datatype_t*)u)->name->wrapper == tc) {
            int cacheable = 1;
            for (i = 0; i < n; i++) {
                if (jl_has_free_typevars(params[i])) {
                    cacheable = 0;
                    break;
                }
            }
            return inst_datatype_env(tc, NULL, params, n, cacheable, NULL, NULL, 0);
        }
    }
    JL_GC_PUSH1(&tc);
    jl_value_t *tc0 = tc;
    for (i=0; i < n; i++) {
        if (!jl_is_unionall(tc0))
            jl_error("too many parameters for type");
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

JL_EXTENSION struct _jl_typestack_t {
    jl_datatype_t *tt;
    struct _jl_typestack_t *prev;
};

static jl_value_t *inst_type_w_(jl_value_t *t, jl_typeenv_t *env, jl_typestack_t *stack, int check);
static jl_svec_t *inst_ftypes(jl_svec_t *p, jl_typeenv_t *env, jl_typestack_t *stack);

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
// sets failed if the hash value isn't stable (if not set on entry)
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
            hash = typekey_hash(dt->name, jl_svec_data(dt->parameters), jl_svec_len(dt->parameters), *failed);
        }
        return hash;
    }
    else if (jl_is_typevar(uw)) {
        if (!*failed) {
            *failed = 1;
            return 0;
        }
        // ignore var and lb, since those might get normalized out in equality testing
        return type_hash(((jl_tvar_t*)uw)->ub, failed);
    }
    else if (jl_is_uniontype(uw)) {
        if (!*failed) {
            *failed = 1;
            return 0;
        }
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

static unsigned typekey_hash(jl_typename_t *tn, jl_value_t **key, size_t n, int nofail) JL_NOTSAFEPOINT
{
    if (tn == jl_type_typename && key[0] == jl_bottom_type)
        return jl_typeofbottom_type->hash;
    size_t j;
    unsigned hash = 3;
    int failed = nofail;
    for (j = 0; j < n; j++) {
        hash = bitmix(type_hash(key[j], &failed), hash);
        if (failed && !nofail)
            return 0;
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
    dt->isconcretetype = !dt->abstract;
    dt->isdispatchtuple = istuple;
    if (dt->name == jl_vararg_typename)
        dt->isconcretetype = 0;
    size_t i, l = jl_nparams(dt);
    for (i = 0; i < l; i++) {
        jl_value_t *p = jl_tparam(dt, i);
        if (!dt->hasfreetypevars) {
            dt->hasfreetypevars = jl_has_free_typevars(p);
            if (dt->hasfreetypevars)
                dt->isconcretetype = 0;
        }
        if (istuple && dt->isconcretetype)
            dt->isconcretetype = (jl_is_datatype(p) && ((jl_datatype_t*)p)->isconcretetype) || p == jl_bottom_type;
        if (dt->isdispatchtuple) {
            dt->isdispatchtuple = jl_is_datatype(p) &&
                ((!jl_is_kind(p) && ((jl_datatype_t*)p)->isconcretetype) ||
                 (((jl_datatype_t*)p)->name == jl_type_typename && !((jl_datatype_t*)p)->hasfreetypevars));
        }
    }
    if (dt->name == jl_type_typename)
        cacheable = 0; // the cache for Type ignores parameter normalization, so it can't be used as a regular hash
    dt->hash = typekey_hash(dt->name, jl_svec_data(dt->parameters), l, cacheable);
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

static jl_value_t *extract_wrapper(jl_value_t *t JL_PROPAGATES_ROOT)
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

// convert `Vararg{X, Y} where T` to `Vararg{X where T, Y}` where T doesn't occur free in Y
static jl_value_t *normalize_vararg(jl_value_t *va)
{
    assert(jl_is_vararg_type(va));
    if (!jl_is_unionall(va)) return va;
    jl_value_t *body=NULL;
    JL_GC_PUSH2(&va, &body);
    jl_unionall_t *ua = (jl_unionall_t*)va;
    body = normalize_vararg(ua->body);
    jl_value_t *unw = jl_unwrap_unionall(body);
    jl_value_t *va0 = jl_tparam0(unw), *va1 = jl_tparam1(unw);
    if (jl_has_typevar(va1, ua->var)) {
        if (body != ua->body)
            va = jl_type_unionall(ua->var, body);
    }
    else {
        va = jl_type_unionall(ua->var, va0);
        va = jl_wrap_vararg(va, va1);
        va = jl_rewrap_unionall(va, body);
    }
    JL_GC_POP();
    return va;
}

static jl_value_t *_jl_instantiate_type_in_env(jl_value_t *ty, jl_unionall_t *env, jl_value_t **vals, jl_typeenv_t *prev, jl_typestack_t *stack);

static jl_value_t *inst_datatype_inner(jl_datatype_t *dt, jl_svec_t *p, jl_value_t **iparams, size_t ntp,
                                       int cacheable, jl_typestack_t *stack, jl_typeenv_t *env)
{
    jl_typestack_t top;
    jl_typename_t *tn = dt->name;
    int istuple = (tn == jl_tuple_typename);
    int isnamedtuple = (tn == jl_namedtuple_typename);
    // check type cache
    if (cacheable) {
        size_t i;
        for (i = 0; i < ntp; i++) {
            jl_value_t *pi = iparams[i];
            if (pi == jl_bottom_type)
                continue;
            if (jl_is_datatype(pi))
                continue;
            if (jl_is_vararg_type(pi)) {
                pi = jl_unwrap_vararg(pi);
                if (jl_has_free_typevars(pi))
                    continue;
            }
            // normalize types equal to wrappers (prepare for wrapper_id)
            jl_value_t *tw = extract_wrapper(pi);
            if (tw && tw != pi && (tn != jl_type_typename || jl_typeof(pi) == jl_typeof(tw)) &&
                    jl_types_equal(pi, tw)) {
                if (jl_is_vararg_type(iparams[i])) {
                    tw = jl_wrap_vararg(tw, jl_tparam1(jl_unwrap_unionall(iparams[i])));
                    JL_GC_PUSH1(&tw);
                    tw = jl_rewrap_unionall(tw, iparams[i]);
                    JL_GC_POP();
                }
                iparams[i] = tw;
                if (p) jl_gc_wb(p, tw);
            }
        }
        jl_value_t *lkup = (jl_value_t*)lookup_type(tn, iparams, ntp);
        if (lkup != NULL)
            return lkup;
    }
    jl_value_t *stack_lkup = lookup_type_stack(stack, dt, ntp, iparams);
    if (stack_lkup)
        return stack_lkup;

    if (!istuple) {
        if (jl_is_vararg_type((jl_value_t*)dt) && ntp == 2) {
            jl_value_t *lenparam = iparams[1];
            if (jl_is_typevar(lenparam)) {
                jl_tvar_t *N = (jl_tvar_t*)lenparam;
                if (!(N->lb == jl_bottom_type && N->ub == (jl_value_t*)jl_any_type))
                    jl_error("TypeVar in Vararg length must have bounds Union{} and Any");
            }
            else if (!jl_is_long(lenparam)) {
                jl_type_error_rt("Vararg", "count", (jl_value_t*)jl_long_type, lenparam);
            }
            else if (jl_unbox_long(lenparam) < 0) {
                jl_errorf("Vararg length is negative: %zd", jl_unbox_long(lenparam));
            }
        }
        // check parameters against bounds in type definition
        check_datatype_parameters(tn, iparams, ntp);
    }
    else if (ntp == 0 && jl_emptytuple_type != NULL) {
        // empty tuple type case
        return (jl_value_t*)jl_emptytuple_type;
    }

    jl_datatype_t *ndt = NULL;
    jl_value_t *last = iparams[ntp - 1];
    JL_GC_PUSH3(&p, &ndt, &last);

    int isvatuple = 0;
    if (istuple && ntp > 0 && jl_is_vararg_type(last)) {
        isvatuple = 1;
        // normalize Tuple{..., Vararg{Int, 3}} to Tuple{..., Int, Int, Int}
        jl_value_t *va = jl_unwrap_unionall(last);
        jl_value_t *va0 = jl_tparam0(va), *va1 = jl_tparam1(va);
        // return same `Tuple` object for types equal to it
        if (ntp == 1 &&
            (last == (jl_value_t*)jl_vararg_type ||  // Tuple{Vararg} == Tuple
             (va0 == (jl_value_t*)jl_any_type &&
              jl_is_unionall(last) && va1 == (jl_value_t*)((jl_unionall_t*)last)->var))) {
            JL_GC_POP();
            return (jl_value_t*)jl_anytuple_type;
        }
        int did_normalize = 0;
        jl_value_t *last2 = normalize_vararg(last);
        assert(!jl_is_unionall(last2) || !jl_is_unionall(((jl_unionall_t*)last2)->body));
        if (last2 != last) {
            last = last2;
            did_normalize = 1;
            va = jl_unwrap_unionall(last);
            va0 = jl_tparam0(va); va1 = jl_tparam1(va);
        }
        if (jl_is_long(va1)) {
            ssize_t nt = jl_unbox_long(va1);
            assert(nt >= 0);
            if (nt == 0 || !jl_has_free_typevars(va0)) {
                if (ntp == 1) {
                    JL_GC_POP();
                    return jl_tupletype_fill(nt, va0);
                }
                size_t i, l;
                p = jl_alloc_svec(ntp - 1 + nt);
                for (i = 0, l = ntp - 1; i < l; i++)
                    jl_svecset(p, i, iparams[i]);
                l = ntp - 1 + nt;
                for (; i < l; i++)
                    jl_svecset(p, i, va0);
                jl_value_t *ndt = (jl_value_t*)jl_apply_tuple_type(p);
                JL_GC_POP();
                return ndt;
            }
        }
        if (did_normalize) {
            p = jl_alloc_svec(ntp);
            for (size_t i = 0; i < ntp-1; i++)
                jl_svecset(p, i, iparams[i]);
            jl_svecset(p, ntp-1, last);
        }
    }

    // move array of instantiated parameters to heap; we need to keep it
    if (p == NULL) {
        p = jl_alloc_svec_uninit(ntp);
        for (size_t i = 0; i < ntp; i++)
            jl_svecset(p, i, iparams[i]);
    }

    // acquire the write lock now that we know we need a new object
    // since we're going to immediately leak it globally via the instantiation stack
    if (cacheable) {
        JL_LOCK(&typecache_lock); // Might GC
        jl_value_t *lkup = (jl_value_t*)lookup_type(tn, iparams, ntp);
        if (lkup != NULL) {
            JL_UNLOCK(&typecache_lock); // Might GC
            JL_GC_POP();
            return lkup;
        }
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
    ndt->types = NULL; // to be filled in below
    if (istuple) {
        ndt->types = p;
    }
    else if (isnamedtuple) {
        jl_value_t *names_tup = jl_svecref(p, 0);
        jl_value_t *values_tt = jl_svecref(p, 1);
        if (!jl_has_free_typevars(names_tup) && !jl_has_free_typevars(values_tt)) {
            if (!jl_is_tuple(names_tup))
                jl_type_error_rt("NamedTuple", "names", (jl_value_t*)jl_anytuple_type, names_tup);
            size_t nf = jl_nfields(names_tup);
            jl_svec_t *names = jl_alloc_svec_uninit(nf);
            for (size_t i = 0; i < nf; i++) {
                jl_value_t *ni = jl_fieldref(names_tup, i);
                if (!jl_is_symbol(ni))
                    jl_type_error_rt("NamedTuple", "name", (jl_value_t*)jl_symbol_type, ni);
                for (size_t j = 0; j < i; j++) {
                    if (ni == jl_svecref(names, j))
                        jl_errorf("duplicate field name in NamedTuple: \"%s\" is not unique", jl_symbol_name((jl_sym_t*)ni));
                }
                jl_svecset(names, i, ni);
            }
            if (!jl_is_datatype(values_tt))
                jl_error("NamedTuple field type must be a tuple type");
            if (jl_is_va_tuple((jl_datatype_t*)values_tt) || jl_nparams(values_tt) != nf)
                jl_error("NamedTuple names and field types must have matching lengths");
            ndt->names = names;
            jl_gc_wb(ndt, ndt->names);
            ndt->types = ((jl_datatype_t*)values_tt)->parameters;
            jl_gc_wb(ndt, ndt->types);
        }
        else {
            ndt->types = jl_emptysvec;
        }
    }
    ndt->mutabl = dt->mutabl;
    ndt->abstract = dt->abstract;
    ndt->instance = NULL;
    ndt->size = 0;
    jl_precompute_memoized_dt(ndt, cacheable);
    if (istuple)
        ndt->ninitialized = ntp - isvatuple;
    else if (isnamedtuple)
        ndt->ninitialized = jl_svec_len(ndt->types);
    else
        ndt->ninitialized = dt->ninitialized;

    if (jl_is_primitivetype(dt)) {
        ndt->size = dt->size;
        ndt->layout = dt->layout;
        ndt->isbitstype = ndt->isinlinealloc = ndt->isconcretetype;
    }

    jl_datatype_t *primarydt = ((jl_datatype_t*)jl_unwrap_unionall(tn->wrapper));
    if (istuple || isnamedtuple) {
        ndt->super = jl_any_type;
    }
    else if (dt->super) {
        ndt->super = (jl_datatype_t*)inst_type_w_((jl_value_t*)dt->super, env, stack, 1);
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
        assert(ftypes == jl_emptysvec || !ndt->abstract);
        if (ftypes == jl_emptysvec) {
            ndt->types = ftypes;
        }
        else if (cacheable) {
            // recursively instantiate the types of the fields
            if (dt->types == NULL)
                ndt->types = jl_compute_fieldtypes(ndt, stack);
            else
                ndt->types = inst_ftypes(ftypes, env, stack);
            jl_gc_wb(ndt, ndt->types);
        }
    }

    // now publish the finished result
    if (cacheable) {
        if (!jl_is_primitivetype(dt) && ndt->types != NULL && !ndt->abstract) {
            jl_compute_field_offsets(ndt);
        }
        jl_cache_type_(ndt);
        JL_UNLOCK(&typecache_lock); // Might GC
    }

    JL_GC_POP();
    return (jl_value_t*)ndt;
}

static jl_tupletype_t *jl_apply_tuple_type_v_(jl_value_t **p, size_t np, jl_svec_t *params)
{
    int cacheable = 1;
    for (size_t i = 0; i < np; i++) {
        assert(p[i]);
        if (!jl_is_concrete_type(p[i]) && p[i] != jl_bottom_type)
            cacheable = 0;
    }
    return (jl_datatype_t*)inst_datatype_inner(jl_anytuple_type, params, p, np, cacheable, NULL, NULL);
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
    return (jl_datatype_t*)inst_datatype_inner(jl_anytuple_type, p, jl_svec_data(p), jl_svec_len(p), 1, NULL, NULL);
}

jl_datatype_t *jl_inst_concrete_tupletype_v(jl_value_t **p, size_t np)
{
    return (jl_datatype_t*)inst_datatype_inner(jl_anytuple_type, NULL, p, np, 1, NULL, NULL);
}

jl_tupletype_t *jl_lookup_arg_tuple_type(jl_value_t *arg1, jl_value_t **args, size_t nargs, int leaf)
{
    return (jl_datatype_t*)lookup_typevalue(jl_tuple_typename, arg1, args, nargs, leaf);
}

jl_tupletype_t *jl_inst_arg_tuple_type(jl_value_t *arg1, jl_value_t **args, size_t nargs, int leaf)
{
    jl_tupletype_t *tt = (jl_datatype_t*)lookup_typevalue(jl_tuple_typename, arg1, args, nargs, leaf);
    if (tt == NULL) {
        int cacheable = 1;
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
                cacheable = 0;
            }
            else {
                ai = jl_typeof(ai);
            }
            jl_svecset(params, i, ai);
        }
        tt = (jl_datatype_t*)inst_datatype_inner(jl_anytuple_type, params, jl_svec_data(params), nargs, cacheable, NULL, NULL);
        JL_GC_POP();
    }
    return tt;
}

static jl_svec_t *inst_ftypes(jl_svec_t *p, jl_typeenv_t *env, jl_typestack_t *stack)
{
    size_t i;
    size_t lp = jl_svec_len(p);
    jl_svec_t *np = jl_alloc_svec(lp);
    JL_GC_PUSH1(&np);
    for (i = 0; i < lp; i++) {
        jl_value_t *pi = jl_svecref(p, i);
        JL_TRY {
            pi = inst_type_w_(pi, env, stack, 1);
        }
        JL_CATCH {
            pi = jl_bottom_type;
        }
        jl_svecset(np, i, pi);
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
    // Note this does not instantiate Tuple{Vararg{Int,3}}; that's done in inst_datatype_inner
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
    jl_svec_t *ip_heap = NULL;
    if (!onstack) {
        ip_heap = jl_alloc_svec(ntp);
        iparams[0] = (jl_value_t*)ip_heap;
        iparams = jl_svec_data(ip_heap);
    }
    int bound = 0;
    int cacheable = 1;
    if (jl_is_va_tuple(tt))
        cacheable = 0;
    int i;
    for (i = 0; i < ntp; i++) {
        jl_value_t *elt = jl_svecref(tp, i);
        jl_value_t *pi = inst_type_w_(elt, env, stack, 0);
        iparams[i] = pi;
        if (ip_heap)
            jl_gc_wb(ip_heap, pi);
        bound |= (pi != elt);
        if (cacheable && !jl_is_concrete_type(pi))
            cacheable = 0;
    }
    if (bound)
        t = inst_datatype_inner(tt, ip_heap, iparams, ntp, cacheable, stack, env);
    JL_GC_POP();
    return t;
}

static jl_value_t *inst_type_w_(jl_value_t *t, jl_typeenv_t *env, jl_typestack_t *stack, int check)
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
        lb = inst_type_w_(ua->var->lb, env, stack, check);
        var = inst_type_w_(ua->var->ub, env, stack, check);
        if (lb != ua->var->lb || var != ua->var->ub) {
            var = (jl_value_t*)jl_new_typevar(ua->var->name, lb, var);
        }
        else {
            var = (jl_value_t*)ua->var;
        }
        jl_typeenv_t newenv = { ua->var, var, env };
        newbody = inst_type_w_(ua->body, &newenv, stack, check);
        if (newbody == (jl_value_t*)jl_emptytuple_type) {
            // NTuple{0} => Tuple{} can make a typevar disappear
            t = (jl_value_t*)jl_emptytuple_type;
        }
        else if (newbody != ua->body || var != (jl_value_t*)ua->var) {
            // if t's parameters are not bound in the environment, return it uncopied (#9378)
            t = jl_new_struct(jl_unionall_type, var, newbody);
        }
        JL_GC_POP();
        return t;
    }
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        jl_value_t *a = inst_type_w_(u->a, env, stack, check);
        jl_value_t *b = NULL;
        JL_GC_PUSH2(&a, &b);
        b = inst_type_w_(u->b, env, stack, check);
        if (a != u->a || b != u->b) {
            jl_value_t *uargs[2] = {a, b};
            t = jl_type_union(uargs, 2);
        }
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
        return inst_tuple_w_(t, env, stack, check);
    size_t ntp = jl_svec_len(tp);
    jl_value_t **iparams;
    JL_GC_PUSHARGS(iparams, ntp);
    int cacheable = 1, bound = 0;
    for (i = 0; i < ntp; i++) {
        jl_value_t *elt = jl_svecref(tp, i);
        jl_value_t *pi = inst_type_w_(elt, env, stack, check);
        iparams[i] = pi;
        bound |= (pi != elt);
        if (cacheable && jl_has_free_typevars(pi))
            cacheable = 0;
    }
    // if t's parameters are not bound in the environment, return it uncopied (#9378)
    if (bound)
        t = inst_datatype_inner(tt, NULL, iparams, ntp, cacheable, stack, env);
    JL_GC_POP();
    return t;
}

static jl_value_t *instantiate_with(jl_value_t *t, jl_value_t **env, size_t n, jl_typeenv_t *te)
{
    if (n > 0) {
        jl_typeenv_t en = { (jl_tvar_t*)env[0], env[1], te };
        return instantiate_with(t, &env[2], n-1, &en );
    }
    return inst_type_w_(t, te, NULL, 1);
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
        return inst_type_w_(ty, &en, stack, 1);
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

jl_value_t *jl_wrap_vararg(jl_value_t *t, jl_value_t *n)
{
    if (t == NULL) {
        assert(n == NULL);
        return (jl_value_t*)jl_vararg_type;
    }
    jl_value_t *vt = jl_instantiate_unionall(jl_vararg_type, t);
    if (n == NULL)
        return vt;
    JL_GC_PUSH1(&vt);
    jl_value_t *vn = jl_instantiate_unionall((jl_unionall_t*)vt, n);
    JL_GC_POP();
    return vn;
}

JL_DLLEXPORT jl_svec_t *jl_compute_fieldtypes(jl_datatype_t *st JL_PROPAGATES_ROOT, void *stack)
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
    st->types = inst_ftypes(wt->types, &env[n - 1], &top);
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
        assert(jl_array_len(partial) == 0);
        return;
    }

    jl_typeenv_t *env = (jl_typeenv_t*)alloca(n * sizeof(jl_typeenv_t));
    for (i = 0; i < n; i++) {
        env[i].var = (jl_tvar_t*)jl_svecref(t->parameters, i);
        env[i].val = NULL;
        env[i].prev = i == 0 ? NULL : &env[i - 1];
    }

    for (j = 0; j < jl_array_len(partial); j++) {
        jl_datatype_t *ndt = (jl_datatype_t*)jl_array_ptr_ref(partial, j);
        assert(jl_unwrap_unionall(ndt->name->wrapper) == (jl_value_t*)t);
        for (i = 0; i < n; i++)
            env[i].val = jl_svecref(ndt->parameters, i);

        ndt->super = (jl_datatype_t*)inst_type_w_((jl_value_t*)t->super, env, &top, 1);
        jl_gc_wb(ndt, ndt->super);
    }

    if (t->types != jl_emptysvec) {
        for (j = 0; j < jl_array_len(partial); j++) {
            jl_datatype_t *ndt = (jl_datatype_t*)jl_array_ptr_ref(partial, j);
            for (i = 0; i < n; i++)
                env[i].val = jl_svecref(ndt->parameters, i);
            assert(ndt->types == NULL);
            ndt->types = inst_ftypes(t->types, env, &top);
            jl_gc_wb(ndt, ndt->types);
            if (ndt->isconcretetype) { // cacheable
                jl_compute_field_offsets(ndt);
            }
        }
    }
    else {
        assert(jl_field_names(t) == jl_emptysvec);
    }
}

// initialization -------------------------------------------------------------

extern void jl_init_int32_int64_cache(void);

static jl_tvar_t *tvar(const char *name)
{
    return jl_new_typevar(jl_symbol(name), (jl_value_t*)jl_bottom_type,
                          (jl_value_t*)jl_any_type);
}

void jl_init_types(void) JL_GC_DISABLED
{
    jl_module_t *core = NULL; // will need to be assigned later

    // create base objects
    jl_datatype_type = jl_new_uninitialized_datatype();
    jl_set_typeof(jl_datatype_type, jl_datatype_type);
    jl_typename_type = jl_new_uninitialized_datatype();
    jl_symbol_type = jl_new_uninitialized_datatype();
    jl_simplevector_type = jl_new_uninitialized_datatype();
    jl_methtable_type = jl_new_uninitialized_datatype();
    jl_nothing = jl_gc_permobj(0, NULL);

    jl_emptysvec = (jl_svec_t*)jl_gc_permobj(sizeof(void*), jl_simplevector_type);
    jl_svec_set_len_unsafe(jl_emptysvec, 0);

    jl_any_type = (jl_datatype_t*)jl_new_abstracttype((jl_value_t*)jl_symbol("Any"), core, NULL, jl_emptysvec);
    jl_any_type->super = jl_any_type;
    jl_nonfunction_mt = jl_any_type->name->mt;
    jl_any_type->name->mt = NULL;
    jl_nonfunction_mt->offs = 0;

    jl_type_type = (jl_unionall_t*)jl_new_abstracttype((jl_value_t*)jl_symbol("Type"), core, jl_any_type, jl_emptysvec);
    jl_type_typename = ((jl_datatype_t*)jl_type_type)->name;
    jl_type_type_mt = jl_new_method_table(jl_type_typename->name, core);
    jl_type_typename->mt = jl_type_type_mt;

    // initialize them. lots of cycles.
    jl_datatype_type->name = jl_new_typename_in(jl_symbol("DataType"), core);
    jl_datatype_type->name->wrapper = (jl_value_t*)jl_datatype_type;
    jl_datatype_type->super = (jl_datatype_t*)jl_type_type;
    jl_datatype_type->parameters = jl_emptysvec;
    jl_datatype_type->name->names = jl_perm_symsvec(19,
                                                    "name",
                                                    "super",
                                                    "parameters",
                                                    "types",
                                                    "names",
                                                    "instance",
                                                    "layout",
                                                    "size",
                                                    "ninitialized",
                                                    "hash",
                                                    "abstract",
                                                    "mutable",
                                                    "hasfreetypevars",
                                                    "isconcretetype",
                                                    "isdispatchtuple",
                                                    "isbitstype",
                                                    "zeroinit",
                                                    "isinlinealloc",
                                                    "has_concrete_subtype");
    jl_datatype_type->types = jl_svec(19,
                                      jl_typename_type,
                                      jl_datatype_type,
                                      jl_simplevector_type,
                                      jl_simplevector_type, jl_simplevector_type,
                                      jl_any_type, // instance
                                      jl_any_type, jl_any_type, jl_any_type, jl_any_type, // properties
                                      jl_any_type, jl_any_type, jl_any_type, jl_any_type,
                                      jl_any_type, jl_any_type, jl_any_type, jl_any_type,
                                      jl_any_type);
    jl_datatype_type->instance = NULL;
    jl_datatype_type->abstract = 0;
    // NOTE: types are not actually mutable, but we want to ensure they are heap-allocated with stable addresses
    jl_datatype_type->mutabl = 1;
    jl_datatype_type->ninitialized = 3;
    jl_precompute_memoized_dt(jl_datatype_type, 1);

    jl_typename_type->name = jl_new_typename_in(jl_symbol("TypeName"), core);
    jl_typename_type->name->wrapper = (jl_value_t*)jl_typename_type;
    jl_typename_type->name->mt = jl_nonfunction_mt;
    jl_typename_type->super = jl_any_type;
    jl_typename_type->parameters = jl_emptysvec;
    jl_typename_type->name->names = jl_perm_symsvec(9, "name", "module",
                                                    "names", "wrapper",
                                                    "cache", "linearcache",
                                                    "hash", "mt", "partial");
    jl_typename_type->types = jl_svec(9, jl_symbol_type, jl_any_type, jl_simplevector_type,
                                      jl_type_type, jl_simplevector_type, jl_simplevector_type,
                                      jl_any_type, jl_any_type, jl_any_type);
    jl_typename_type->instance = NULL;
    jl_typename_type->abstract = 0;
    jl_typename_type->mutabl = 1;
    jl_typename_type->ninitialized = 2;
    jl_precompute_memoized_dt(jl_typename_type, 1);

    jl_methtable_type->name = jl_new_typename_in(jl_symbol("MethodTable"), core);
    jl_methtable_type->name->wrapper = (jl_value_t*)jl_methtable_type;
    jl_methtable_type->name->mt = jl_nonfunction_mt;
    jl_methtable_type->super = jl_any_type;
    jl_methtable_type->parameters = jl_emptysvec;
    jl_methtable_type->name->names = jl_perm_symsvec(12, "name", "defs",
                                                     "leafcache", "cache", "max_args",
                                                     "kwsorter", "module",
                                                     "backedges", "", "", "offs", "");
    jl_methtable_type->types = jl_svec(12, jl_symbol_type, jl_any_type, jl_any_type,
                                       jl_any_type, jl_any_type/*jl_long*/,
                                       jl_any_type, jl_any_type/*module*/,
                                       jl_any_type/*any vector*/, jl_any_type/*long*/, jl_any_type/*int32*/,
                                       jl_any_type/*uint8*/, jl_any_type/*uint8*/);
    jl_methtable_type->instance = NULL;
    jl_methtable_type->abstract = 0;
    jl_methtable_type->mutabl = 1;
    jl_methtable_type->ninitialized = 5;
    jl_precompute_memoized_dt(jl_methtable_type, 1);

    jl_symbol_type->name = jl_new_typename_in(jl_symbol("Symbol"), core);
    jl_symbol_type->name->wrapper = (jl_value_t*)jl_symbol_type;
    jl_symbol_type->name->mt = jl_nonfunction_mt;
    jl_symbol_type->super = jl_any_type;
    jl_symbol_type->parameters = jl_emptysvec;
    jl_symbol_type->name->names = jl_emptysvec;
    jl_symbol_type->types = jl_emptysvec;
    jl_symbol_type->instance = NULL;
    jl_symbol_type->size = 0;
    jl_symbol_type->abstract = 0;
    jl_symbol_type->mutabl = 1;
    jl_symbol_type->ninitialized = 0;
    jl_precompute_memoized_dt(jl_symbol_type, 1);

    jl_simplevector_type->name = jl_new_typename_in(jl_symbol("SimpleVector"), core);
    jl_simplevector_type->name->wrapper = (jl_value_t*)jl_simplevector_type;
    jl_simplevector_type->name->mt = jl_nonfunction_mt;
    jl_simplevector_type->super = jl_any_type;
    jl_simplevector_type->parameters = jl_emptysvec;
    jl_simplevector_type->name->names = jl_emptysvec;
    jl_simplevector_type->types = jl_emptysvec;
    jl_simplevector_type->instance = NULL;
    jl_simplevector_type->abstract = 0;
    jl_simplevector_type->mutabl = 1;
    jl_simplevector_type->ninitialized = 0;
    jl_precompute_memoized_dt(jl_simplevector_type, 1);

    // now they can be used to create the remaining base kinds and types
    jl_nothing_type = jl_new_datatype(jl_symbol("Nothing"), core, jl_any_type, jl_emptysvec,
                                      jl_emptysvec, jl_emptysvec, 0, 0, 0);
    jl_void_type = jl_nothing_type; // deprecated alias
    jl_astaggedvalue(jl_nothing)->header = ((uintptr_t)jl_nothing_type) | GC_OLD_MARKED;
    jl_nothing_type->instance = jl_nothing;

    jl_datatype_t *type_type = (jl_datatype_t*)jl_type_type;
    jl_typeofbottom_type = jl_new_datatype(jl_symbol("TypeofBottom"), core, type_type, jl_emptysvec,
                                         jl_emptysvec, jl_emptysvec, 0, 0, 0);
    jl_bottom_type = jl_new_struct(jl_typeofbottom_type);
    jl_typeofbottom_type->instance = jl_bottom_type;

    jl_uniontype_type = jl_new_datatype(jl_symbol("Union"), core, type_type, jl_emptysvec,
                                        jl_perm_symsvec(2, "a", "b"),
                                        jl_svec(2, jl_any_type, jl_any_type),
                                        0, 0, 2);

    jl_tvar_type = jl_new_datatype(jl_symbol("TypeVar"), core, jl_any_type, jl_emptysvec,
                                   jl_perm_symsvec(3, "name", "lb", "ub"),
                                   jl_svec(3, jl_symbol_type, jl_any_type, jl_any_type),
                                   0, 1, 3);

    jl_unionall_type = jl_new_datatype(jl_symbol("UnionAll"), core, type_type, jl_emptysvec,
                                       jl_perm_symsvec(2, "var", "body"),
                                       jl_svec(2, jl_tvar_type, jl_any_type),
                                       0, 0, 2);

    jl_svec_t *tv;
    tv = jl_svec2(tvar("T"),tvar("N"));
    jl_vararg_type = (jl_unionall_t*)jl_new_abstracttype((jl_value_t*)jl_symbol("Vararg"), core, jl_any_type, tv)->name->wrapper;
    jl_vararg_typename = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_vararg_type))->name;

    jl_svec_t *anytuple_params = jl_svec(1, jl_wrap_vararg((jl_value_t*)jl_any_type, (jl_value_t*)NULL));
    jl_anytuple_type = jl_new_datatype(jl_symbol("Tuple"), core, jl_any_type, anytuple_params,
                                       jl_emptysvec, anytuple_params, 0, 0, 0);
    jl_tuple_typename = jl_anytuple_type->name;
    // fix some miscomputed values, since we didn't know this was going to be a Tuple in jl_precompute_memoized_dt
    jl_tuple_typename->wrapper = (jl_value_t*)jl_anytuple_type; // remove UnionAll wrappers
    jl_anytuple_type->isconcretetype = 0;
    jl_anytuple_type->layout = NULL;
    jl_anytuple_type->size = 0;

    jl_tvar_t *tttvar = tvar("T");
    ((jl_datatype_t*)jl_type_type)->parameters = jl_svec(1, tttvar);
    ((jl_datatype_t*)jl_type_type)->hasfreetypevars = 1;
    jl_type_typename->wrapper = jl_new_struct(jl_unionall_type, tttvar, (jl_value_t*)jl_type_type);
    jl_type_type = (jl_unionall_t*)jl_type_typename->wrapper;

    jl_typeofbottom_type->super = jl_wrap_Type(jl_bottom_type);

    jl_emptytuple_type = jl_apply_tuple_type(jl_emptysvec);
    jl_emptytuple = jl_gc_permobj(0, jl_emptytuple_type);
    jl_emptytuple_type->instance = jl_emptytuple;

    // non-primitive definitions follow
    jl_int32_type = jl_new_primitivetype((jl_value_t*)jl_symbol("Int32"), core,
                                         jl_any_type, jl_emptysvec, 32);
    jl_int64_type = jl_new_primitivetype((jl_value_t*)jl_symbol("Int64"), core,
                                         jl_any_type, jl_emptysvec, 64);
    jl_uint32_type = jl_new_primitivetype((jl_value_t*)jl_symbol("UInt32"), core,
                                          jl_any_type, jl_emptysvec, 32);
    jl_uint64_type = jl_new_primitivetype((jl_value_t*)jl_symbol("UInt64"), core,
                                          jl_any_type, jl_emptysvec, 64);
    jl_uint8_type = jl_new_primitivetype((jl_value_t*)jl_symbol("UInt8"), core,
                                         jl_any_type, jl_emptysvec, 8);

    jl_ssavalue_type = jl_new_datatype(jl_symbol("SSAValue"), core, jl_any_type, jl_emptysvec,
                                       jl_perm_symsvec(1, "id"),
                                       jl_svec1(jl_long_type), 0, 0, 1);

    jl_abstractslot_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Slot"), core, jl_any_type,
                                               jl_emptysvec);

    jl_slotnumber_type = jl_new_datatype(jl_symbol("SlotNumber"), core, jl_abstractslot_type, jl_emptysvec,
                                         jl_perm_symsvec(1, "id"),
                                         jl_svec1(jl_long_type), 0, 0, 1);

    jl_typedslot_type = jl_new_datatype(jl_symbol("TypedSlot"), core, jl_abstractslot_type, jl_emptysvec,
                                        jl_perm_symsvec(2, "id", "typ"),
                                        jl_svec(2, jl_long_type, jl_any_type), 0, 0, 2);

    jl_argument_type = jl_new_datatype(jl_symbol("Argument"), core, jl_any_type, jl_emptysvec,
                                       jl_perm_symsvec(1, "n"),
                                       jl_svec1(jl_long_type), 0, 0, 1);

    jl_init_int32_int64_cache();

    jl_bool_type = NULL;
    jl_bool_type = jl_new_primitivetype((jl_value_t*)jl_symbol("Bool"), core,
                                        jl_any_type, jl_emptysvec, 8);
    jl_false = jl_permbox8(jl_bool_type, 0);
    jl_true  = jl_permbox8(jl_bool_type, 1);

    jl_abstractstring_type = jl_new_abstracttype((jl_value_t*)jl_symbol("AbstractString"), core, jl_any_type, jl_emptysvec);
    jl_string_type = jl_new_datatype(jl_symbol("String"), core, jl_abstractstring_type, jl_emptysvec,
                                     jl_emptysvec, jl_emptysvec, 0, 1, 0);
    jl_string_type->instance = NULL;
    jl_compute_field_offsets(jl_string_type);
    jl_an_empty_string = jl_pchar_to_string("\0", 1);
    *(size_t*)jl_an_empty_string = 0;

    jl_typemap_level_type =
        jl_new_datatype(jl_symbol("TypeMapLevel"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(4,
                            "arg1",
                            "targ",
                            "list",
                            "any"),
                        jl_svec(4,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type),
                        0, 1, 4);

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
                        0, 1, 4);

    jl_function_type = jl_new_abstracttype((jl_value_t*)jl_symbol("Function"), core, jl_any_type, jl_emptysvec);
    jl_builtin_type  = jl_new_abstracttype((jl_value_t*)jl_symbol("Builtin"), core, jl_function_type, jl_emptysvec);
    jl_function_type->name->mt = NULL; // subtypes of Function have independent method tables
    jl_builtin_type->name->mt = NULL;  // so they don't share the Any type table

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_abstractarray_type = (jl_unionall_t*)
        jl_new_abstracttype((jl_value_t*)jl_symbol("AbstractArray"), core,
                            jl_any_type, tv)->name->wrapper;

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_densearray_type = (jl_unionall_t*)
        jl_new_abstracttype((jl_value_t*)jl_symbol("DenseArray"), core,
                            (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_abstractarray_type, jl_svec_data(tv), 2),
                            tv)->name->wrapper;

    tv = jl_svec2(tvar("T"), tvar("N"));
    jl_array_type = (jl_unionall_t*)
        jl_new_datatype(jl_symbol("Array"), core,
                        (jl_datatype_t*)
                        jl_apply_type((jl_value_t*)jl_densearray_type, jl_svec_data(tv), 2),
                        tv,
                        jl_emptysvec, jl_emptysvec, 0, 1, 0)->name->wrapper;
    jl_array_typename = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_array_type))->name;
    jl_compute_field_offsets((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_array_type));

    jl_array_any_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_any_type, jl_box_long(1));
    jl_array_symbol_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_symbol_type, jl_box_long(1));
    jl_array_uint8_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_uint8_type, jl_box_long(1));
    jl_array_int32_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_int32_type, jl_box_long(1));
    jl_an_empty_vec_any = (jl_value_t*)jl_alloc_vec_any(0); // used internally
    jl_nonfunction_mt->leafcache = (jl_array_t*)jl_an_empty_vec_any;
    jl_type_type_mt->leafcache = (jl_array_t*)jl_an_empty_vec_any;

    jl_expr_type =
        jl_new_datatype(jl_symbol("Expr"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "head", "args"),
                        jl_svec(2, jl_symbol_type, jl_array_any_type),
                        0, 1, 2);

    jl_module_type =
        jl_new_datatype(jl_symbol("Module"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "name", "parent"),
                        jl_svec(2, jl_symbol_type, jl_any_type), 0, 1, 2);

    jl_value_t *symornothing[2] = { (jl_value_t*)jl_symbol_type, (jl_value_t*)jl_void_type };
    jl_linenumbernode_type =
        jl_new_datatype(jl_symbol("LineNumberNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "line", "file"),
                        jl_svec(2, jl_long_type, jl_type_union(symornothing, 2)), 0, 0, 2);

    jl_lineinfonode_type =
        jl_new_datatype(jl_symbol("LineInfoNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(4, "method", "file", "line", "inlined_at"),
                        jl_svec(4, jl_any_type, jl_symbol_type, jl_long_type, jl_long_type), 0, 0, 4);

    jl_gotonode_type =
        jl_new_datatype(jl_symbol("GotoNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "label"),
                        jl_svec(1, jl_long_type), 0, 0, 1);

    jl_gotoifnot_type =
        jl_new_datatype(jl_symbol("GotoIfNot"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "cond", "dest"),
                        jl_svec(2, jl_any_type, jl_long_type), 0, 0, 2);

    jl_returnnode_type =
        jl_new_datatype(jl_symbol("ReturnNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "val"),
                        jl_svec(1, jl_any_type), 0, 0, 0);

    jl_pinode_type =
        jl_new_datatype(jl_symbol("PiNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "val", "typ"),
                        jl_svec(2, jl_any_type, jl_any_type), 0, 0, 2);

    jl_phinode_type =
        jl_new_datatype(jl_symbol("PhiNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "edges", "values"),
                        jl_svec(2, jl_array_any_type, jl_array_any_type), 0, 0, 2);

    jl_phicnode_type =
        jl_new_datatype(jl_symbol("PhiCNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "values"),
                        jl_svec(1, jl_array_any_type), 0, 0, 1);

    jl_upsilonnode_type =
        jl_new_datatype(jl_symbol("UpsilonNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "val"),
                        jl_svec(1, jl_any_type), 0, 0, 0);

    jl_quotenode_type =
        jl_new_datatype(jl_symbol("QuoteNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "value"),
                        jl_svec(1, jl_any_type), 0, 0, 1);

    jl_newvarnode_type =
        jl_new_datatype(jl_symbol("NewvarNode"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(1, "slot"),
                        jl_svec(1, jl_slotnumber_type), 0, 0, 1);

    jl_globalref_type =
        jl_new_datatype(jl_symbol("GlobalRef"), core, jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(2, "mod", "name"),
                        jl_svec(2, jl_module_type, jl_symbol_type), 0, 0, 2);

    jl_code_info_type =
        jl_new_datatype(jl_symbol("CodeInfo"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(18,
                            "code",
                            "codelocs",
                            "ssavaluetypes",
                            "ssaflags",
                            "method_for_inference_limit_heuristics",
                            "linetable",
                            "slotnames",
                            "slotflags",
                            "slottypes",
                            "rettype",
                            "parent",
                            "edges",
                            "min_world",
                            "max_world",
                            "inferred",
                            "inlineable",
                            "propagate_inbounds",
                            "pure"),
                        jl_svec(18,
                            jl_array_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_array_uint8_type,
                            jl_any_type,
                            jl_any_type,
                            jl_array_symbol_type,
                            jl_array_uint8_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_ulong_type,
                            jl_ulong_type,
                            jl_bool_type,
                            jl_bool_type,
                            jl_bool_type,
                            jl_bool_type),
                        0, 1, 18);

    jl_method_type =
        jl_new_datatype(jl_symbol("Method"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(24,
                            "name",
                            "module",
                            "file",
                            "line",
                            "primary_world",
                            "deleted_world",
                            "sig",
                            "ambig",
                            "resorted",
                            "specializations",
                            "speckeyset",
                            "slot_syms",
                            "source",
                            "unspecialized",
                            "generator",
                            "roots",
                            "ccallable",
                            "invokes",
                            "nargs",
                            "called",
                            "nospecialize",
                            "nkw",
                            "isva",
                            "pure"),
                        jl_svec(24,
                            jl_symbol_type,
                            jl_module_type,
                            jl_symbol_type,
                            jl_int32_type,
                            jl_ulong_type,
                            jl_ulong_type,
                            jl_type_type,
                            jl_any_type, // Union{Vector, Nothing}
                            jl_any_type, // Union{Vector, Nothing}
                            jl_simplevector_type,
                            jl_array_type,
                            jl_string_type,
                            jl_any_type,
                            jl_any_type, // jl_method_instance_type
                            jl_any_type,
                            jl_array_any_type,
                            jl_simplevector_type,
                            jl_any_type,
                            jl_int32_type,
                            jl_int32_type,
                            jl_int32_type,
                            jl_int32_type,
                            jl_bool_type,
                            jl_bool_type),
                        0, 1, 12);

    jl_method_instance_type =
        jl_new_datatype(jl_symbol("MethodInstance"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(7,
                            "def",
                            "specTypes",
                            "sparam_vals",
                            "uninferred",
                            "backedges",
                            "cache",
                            "inInference"),
                        jl_svec(7,
                            jl_new_struct(jl_uniontype_type, jl_method_type, jl_module_type),
                            jl_any_type,
                            jl_simplevector_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            jl_bool_type),
                        0, 1, 3);

    jl_code_instance_type =
        jl_new_datatype(jl_symbol("CodeInstance"), core,
                        jl_any_type, jl_emptysvec,
                        jl_perm_symsvec(10,
                            "def",
                            "next",
                            "min_world",
                            "max_world",
                            "rettype",
                            "rettype_const",
                            "inferred",
                            //"edges",
                            //"absolute_max",
                            "isspecsig", "invoke", "specptr"), // function object decls
                        jl_svec(10,
                            jl_method_instance_type,
                            jl_any_type,
                            jl_ulong_type,
                            jl_ulong_type,
                            jl_any_type,
                            jl_any_type,
                            jl_any_type,
                            //jl_any_type,
                            //jl_bool_type,
                            jl_bool_type,
                            jl_any_type, jl_any_type), // fptrs
                        0, 1, 1);
    jl_svecset(jl_code_instance_type->types, 1, jl_code_instance_type);

    // all Kinds share the Type method table (not the nonfunction one)
    jl_unionall_type->name->mt = jl_uniontype_type->name->mt = jl_datatype_type->name->mt =
        jl_type_type_mt;

    jl_intrinsic_type = jl_new_primitivetype((jl_value_t*)jl_symbol("IntrinsicFunction"), core,
                                             jl_builtin_type, jl_emptysvec, 32);

    tv = jl_svec1(tvar("T"));
    jl_ref_type = (jl_unionall_t*)
        jl_new_abstracttype((jl_value_t*)jl_symbol("Ref"), core, jl_any_type, tv)->name->wrapper;

    tv = jl_svec1(tvar("T"));
    jl_pointer_type = (jl_unionall_t*)
        jl_new_primitivetype((jl_value_t*)jl_symbol("Ptr"), core,
                             (jl_datatype_t*)jl_apply_type((jl_value_t*)jl_ref_type, jl_svec_data(tv), 1), tv,
                             sizeof(void*)*8)->name->wrapper;
    jl_pointer_typename = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_pointer_type))->name;

    // LLVMPtr{T, AS} where {T, AS}
    tv = jl_svec2(tvar("T"), tvar("AS"));
    jl_svec_t *tv_base = jl_svec1(tvar("T"));
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

    // Type{T}
    jl_tvar_t *typetype_tvar = tvar("T");
    jl_typetype_type =
        (jl_unionall_t*)jl_new_struct(jl_unionall_type, typetype_tvar,
                                      jl_apply_type1((jl_value_t*)jl_type_type, (jl_value_t*)typetype_tvar));

    jl_tvar_t *ntval_var = jl_new_typevar(jl_symbol("T"), (jl_value_t*)jl_bottom_type,
                                          (jl_value_t*)jl_anytuple_type);
    tv = jl_svec2(tvar("names"), ntval_var);
    jl_datatype_t *ntt = jl_new_datatype(jl_symbol("NamedTuple"), core, jl_any_type, tv,
                                         jl_emptysvec, jl_emptysvec, 0, 0, 0);
    jl_namedtuple_type = (jl_unionall_t*)ntt->name->wrapper;
    ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_namedtuple_type))->layout = NULL;
    jl_namedtuple_typename = ntt->name;

    jl_task_type = (jl_datatype_t*)
        jl_new_datatype(jl_symbol("Task"),
                        NULL,
                        jl_any_type,
                        jl_emptysvec,
                        jl_perm_symsvec(11,
                                        "next",
                                        "queue",
                                        "storage",
                                        "state",
                                        "donenotify",
                                        "result",
                                        "exception",
                                        "backtrace",
                                        "logstate",
                                        "code",
                                        "sticky"),
                        jl_svec(11,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_symbol_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_bool_type),
                        0, 1, 9);
    jl_value_t *listt = jl_new_struct(jl_uniontype_type, jl_task_type, jl_nothing_type);
    jl_svecset(jl_task_type->types, 0, listt);

    // complete builtin type metadata
    jl_value_t *pointer_void = jl_apply_type1((jl_value_t*)jl_pointer_type, (jl_value_t*)jl_nothing_type);
    jl_voidpointer_type = (jl_datatype_t*)pointer_void;
    jl_uint8pointer_type = (jl_datatype_t*)jl_apply_type1((jl_value_t*)jl_pointer_type, (jl_value_t*)jl_uint8_type);
    jl_svecset(jl_datatype_type->types, 6, jl_voidpointer_type);
    jl_svecset(jl_datatype_type->types, 7, jl_int32_type);
    jl_svecset(jl_datatype_type->types, 8, jl_int32_type);
    jl_svecset(jl_datatype_type->types, 9, jl_int32_type);
    jl_svecset(jl_datatype_type->types, 10, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 11, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 12, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 13, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 14, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 15, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 16, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 17, jl_bool_type);
    jl_svecset(jl_datatype_type->types, 18, jl_bool_type);
    jl_svecset(jl_typename_type->types, 1, jl_module_type);
    jl_svecset(jl_typename_type->types, 6, jl_long_type);
    jl_svecset(jl_typename_type->types, 3, jl_type_type);
    jl_svecset(jl_methtable_type->types, 4, jl_long_type);
    jl_svecset(jl_methtable_type->types, 6, jl_module_type);
    jl_svecset(jl_methtable_type->types, 7, jl_array_any_type);
#ifdef __LP64__
    jl_svecset(jl_methtable_type->types, 8, jl_int64_type); // unsigned long
    jl_svecset(jl_methtable_type->types, 9, jl_int64_type); // uint32_t plus alignment
#else
    jl_svecset(jl_methtable_type->types, 8, jl_int32_type); // DWORD
    jl_svecset(jl_methtable_type->types, 9, jl_int32_type); // uint32_t
#endif
    jl_svecset(jl_methtable_type->types, 10, jl_uint8_type);
    jl_svecset(jl_methtable_type->types, 11, jl_uint8_type);
    jl_svecset(jl_method_type->types, 13, jl_method_instance_type);
    jl_svecset(jl_method_instance_type->types, 5, jl_code_instance_type);
    jl_svecset(jl_code_instance_type->types, 8, jl_voidpointer_type);
    jl_svecset(jl_code_instance_type->types, 9, jl_voidpointer_type);

    jl_compute_field_offsets(jl_datatype_type);
    jl_compute_field_offsets(jl_typename_type);
    jl_compute_field_offsets(jl_uniontype_type);
    jl_compute_field_offsets(jl_tvar_type);
    jl_compute_field_offsets(jl_methtable_type);
    jl_compute_field_offsets(jl_module_type);
    jl_compute_field_offsets(jl_method_instance_type);
    jl_compute_field_offsets(jl_code_instance_type);
    jl_compute_field_offsets(jl_unionall_type);
    jl_compute_field_offsets(jl_simplevector_type);
    jl_compute_field_offsets(jl_symbol_type);

    // override the preferred layout for a couple types
    jl_lineinfonode_type->isinlinealloc = 0; // FIXME: assumed to be a pointer by codegen
    // It seems like we probably usually end up needing the box for kinds (used in an Any context)--but is that true?
    jl_uniontype_type->isinlinealloc = 0;
    jl_unionall_type->isinlinealloc = 0;
}

#ifdef __cplusplus
}
#endif
