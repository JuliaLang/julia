// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  implementations of built-in functions
*/
#include "platform.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#if defined(_OS_WINDOWS_)
#include <malloc.h>
#else
#include <unistd.h>
#endif
#include <ctype.h>
#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"
#include "intrinsics.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// egal and object_id ---------------------------------------------------------

static int bits_equal(const void *a, const void *b, int sz) JL_NOTSAFEPOINT
{
    switch (sz) {
    case 1:  return *(int8_t*)a == *(int8_t*)b;
        // Let compiler constant folds the following.
    case 2:  return memcmp(a, b, 2) == 0;
    case 4:  return memcmp(a, b, 4) == 0;
    case 8:  return memcmp(a, b, 8) == 0;
    default: return memcmp(a, b, sz) == 0;
    }
}

// The frequently used jl_egal function deserves special attention when it
// comes to performance which is made challenging by the fact that the
// function has to handle quite a few different cases and because it is
// called recursively.  To optimize performance many special cases are
// handle with separate comparisons which can dramatically reduce the run
// time of the function.  The compiler can translate these simple tests
// with little effort, e.g., few registers are used.
//
// The complex cases require more effort and more registers to be translated
// efficiently.  The effected cases include comparing tuples and fields.  If
// the code to perform these operation would be inlined in the jl_egal
// function then the compiler would generate at the or close to the top of
// the function a prologue which saves all the callee-save registers and at
// the end the respective epilogue.  The result is that even the fast cases
// are slowed down.
//
// The solution is to keep the code in jl_egal simple and split out the
// (more) complex cases into their own functions which are marked with
// NOINLINE.
static int NOINLINE compare_svec(jl_svec_t *a, jl_svec_t *b) JL_NOTSAFEPOINT
{
    size_t i, l = jl_svec_len(a);
    if (l != jl_svec_len(b))
        return 0;
    for (i = 0; i < l; i++) {
        if (!jl_egal(jl_svecref(a, i), jl_svecref(b, i)))
            return 0;
    }
    return 1;
}

// See comment above for an explanation of NOINLINE.
static int NOINLINE compare_fields(const jl_value_t *a, const jl_value_t *b, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    size_t nf = jl_datatype_nfields(dt);
    // npointers is used at end, but fetched here for locality with nfields.
    int npointers = ((jl_datatype_t*)dt)->layout->npointers;
    for (size_t f = 0; f < nf; f++) {
        size_t offs = jl_field_offset(dt, f);
        char *ao = (char*)a + offs;
        char *bo = (char*)b + offs;
        if (jl_field_isptr(dt, f)) {
            // Save ptr recursion until the end -- only recurse if otherwise equal
            // Note that we also skip comparing the pointers for null here, because
            // null fields are rare so it can save CPU to delay this read too.
            continue;
        }
        else {
            jl_datatype_t *ft = (jl_datatype_t*)jl_field_type_concrete(dt, f);
            if (jl_is_uniontype(ft)) {
                size_t idx = jl_field_size(dt, f) - 1;
                uint8_t asel = ((uint8_t*)ao)[idx];
                uint8_t bsel = ((uint8_t*)bo)[idx];
                if (asel != bsel)
                    return 0;
                ft = (jl_datatype_t*)jl_nth_union_component((jl_value_t*)ft, asel);
            }
            else if (ft->layout->first_ptr >= 0) {
                // If the field is a inline immutable that can be undef
                // we need to check for undef first since undef struct
                // may have fields that are different but should still be treated as equal.
                int32_t idx = ft->layout->first_ptr;
                jl_value_t *ptra = ((jl_value_t**)ao)[idx];
                jl_value_t *ptrb = ((jl_value_t**)bo)[idx];
                if ((ptra == NULL) != (ptrb == NULL)) {
                    return 0;
                }
                else if (ptra == NULL) { // implies ptrb == NULL
                    continue; // skip this field (it is #undef)
                }
            }
            if (!ft->layout->haspadding) {
                if (!bits_equal(ao, bo, ft->size))
                    return 0;
            }
            else {
                assert(jl_datatype_nfields(ft) > 0);
                if (!compare_fields((jl_value_t*)ao, (jl_value_t*)bo, ft))
                    return 0;
            }
        }
    }
    // If we've gotten here, the objects are bitwise equal, besides their pointer fields.
    // Now, we will recurse into jl_egal for the pointed-to elements, which might be
    // arbitrarily expensive.
    for (size_t p = 0; p < npointers; p++) {
        size_t offs = jl_ptr_offset(dt, p);
        jl_value_t *af = ((jl_value_t**)a)[offs];
        jl_value_t *bf = ((jl_value_t**)b)[offs];
        if (af != bf) {
            if (af == NULL || bf == NULL)
                return 0;
            if (!jl_egal(af, bf))
                return 0;
        }
    }
    return 1;
}

static int egal_types(const jl_value_t *a, const jl_value_t *b, jl_typeenv_t *env, int tvar_names) JL_NOTSAFEPOINT
{
    if (a == b)
        return 1;
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(a);
    if (dt != (jl_datatype_t*)jl_typeof(b))
        return 0;
    if (dt == jl_datatype_type) {
        jl_datatype_t *dta = (jl_datatype_t*)a;
        jl_datatype_t *dtb = (jl_datatype_t*)b;
        if (dta->name != dtb->name)
            return 0;
        size_t i, l = jl_nparams(dta);
        if (jl_nparams(dtb) != l)
            return 0;
        for (i = 0; i < l; i++) {
            if (!egal_types(jl_tparam(dta, i), jl_tparam(dtb, i), env, tvar_names))
                return 0;
        }
        return 1;
    }
    if (dt == jl_tvar_type) {
        jl_typeenv_t *pe = env;
        while (pe != NULL) {
            if (pe->var == (jl_tvar_t*)a)
                return pe->val == b;
            pe = pe->prev;
        }
        return 0;
    }
    if (dt == jl_unionall_type) {
        jl_unionall_t *ua = (jl_unionall_t*)a;
        jl_unionall_t *ub = (jl_unionall_t*)b;
        if (tvar_names && ua->var->name != ub->var->name)
            return 0;
        if (!(egal_types(ua->var->lb, ub->var->lb, env, tvar_names) && egal_types(ua->var->ub, ub->var->ub, env, tvar_names)))
            return 0;
        jl_typeenv_t e = { ua->var, (jl_value_t*)ub->var, env };
        return egal_types(ua->body, ub->body, &e, tvar_names);
    }
    if (dt == jl_uniontype_type) {
        return egal_types(((jl_uniontype_t*)a)->a, ((jl_uniontype_t*)b)->a, env, tvar_names) &&
            egal_types(((jl_uniontype_t*)a)->b, ((jl_uniontype_t*)b)->b, env, tvar_names);
    }
    if (dt == jl_vararg_type) {
        jl_vararg_t *vma = (jl_vararg_t*)a;
        jl_vararg_t *vmb = (jl_vararg_t*)b;
        jl_value_t *vmaT = vma->T ? vma->T : (jl_value_t*)jl_any_type;
        jl_value_t *vmbT = vmb->T ? vmb->T : (jl_value_t*)jl_any_type;
        if (!egal_types(vmaT, vmbT, env, tvar_names))
            return 0;
        if (vma->N && vmb->N)
            return egal_types(vma->N, vmb->N, env, tvar_names);
        return !vma->N && !vmb->N;
    }
    if (dt == jl_symbol_type)
        return 0;
    assert(!dt->name->mutabl);
    return jl_egal__bits(a, b, dt);
}

JL_DLLEXPORT int jl_types_egal(jl_value_t *a, jl_value_t *b)
{
    return egal_types(a, b, NULL, 0);
}

JL_DLLEXPORT int (jl_egal)(const jl_value_t *a JL_MAYBE_UNROOTED, const jl_value_t *b JL_MAYBE_UNROOTED) JL_NOTSAFEPOINT
{
    // warning: a,b may NOT have been gc-rooted by the caller
    return jl_egal(a, b);
}

JL_DLLEXPORT int jl_egal__unboxed(const jl_value_t *a JL_MAYBE_UNROOTED, const jl_value_t *b JL_MAYBE_UNROOTED, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    // warning: a,b may NOT have been gc-rooted by the caller
    return jl_egal__unboxed_(a, b, dt);
}

int jl_egal__special(const jl_value_t *a JL_MAYBE_UNROOTED, const jl_value_t *b JL_MAYBE_UNROOTED, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    if (dt == jl_simplevector_type)
        return compare_svec((jl_svec_t*)a, (jl_svec_t*)b);
    if (dt == jl_datatype_type) {
        jl_datatype_t *dta = (jl_datatype_t*)a;
        jl_datatype_t *dtb = (jl_datatype_t*)b;
        if (dta->name != dtb->name)
            return 0;
        if (dta->name != jl_tuple_typename && (dta->isconcretetype || dtb->isconcretetype))
            return 0;
        return compare_svec(dta->parameters, dtb->parameters);
    }
    if (dt == jl_string_type) {
        size_t l = jl_string_len(a);
        if (jl_string_len(b) != l)
            return 0;
        return !memcmp(jl_string_data(a), jl_string_data(b), l);
    }
    assert(0 && "unreachable");
    return 0;
}

int jl_egal__bits(const jl_value_t *a JL_MAYBE_UNROOTED, const jl_value_t *b JL_MAYBE_UNROOTED, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    size_t sz = jl_datatype_size(dt);
    if (sz == 0)
        return 1;
    size_t nf = jl_datatype_nfields(dt);
    if (nf == 0 || !dt->layout->haspadding)
        return bits_equal(a, b, sz);
    if (dt == jl_unionall_type)
        return egal_types(a, b, NULL, 1);
    return compare_fields(a, b, dt);
}

// object_id ------------------------------------------------------------------

static uintptr_t bits_hash(const void *b, size_t sz) JL_NOTSAFEPOINT
{
    switch (sz) {
    case 1:  return int32hash(*(const int8_t*)b);
    case 2:  return int32hash(jl_load_unaligned_i16(b));
    case 4:  return int32hash(jl_load_unaligned_i32(b));
#ifdef _P64
    case 8:  return int64hash(jl_load_unaligned_i64(b));
#else
    case 8:  return int64to32hash(jl_load_unaligned_i64(b));
#endif
    default:
#ifdef _P64
        return memhash((const char*)b, sz);
#else
        return memhash32((const char*)b, sz);
#endif
    }
}

static uintptr_t NOINLINE hash_svec(jl_svec_t *v) JL_NOTSAFEPOINT
{
    uintptr_t h = 0;
    size_t i, l = jl_svec_len(v);
    for (i = 0; i < l; i++) {
        jl_value_t *x = jl_svecref(v, i);
        uintptr_t u = (x == NULL) ? 0 : jl_object_id(x);
        h = bitmix(h, u);
    }
    return h;
}

static uintptr_t immut_id_(jl_datatype_t *dt, jl_value_t *v, uintptr_t h) JL_NOTSAFEPOINT;

typedef struct _varidx {
    jl_tvar_t *var;
    struct _varidx *prev;
} jl_varidx_t;

static uintptr_t type_object_id_(jl_value_t *v, jl_varidx_t *env) JL_NOTSAFEPOINT
{
    if (v == NULL)
        return 0;
    jl_datatype_t *tv = (jl_datatype_t*)jl_typeof(v);
    if (tv == jl_tvar_type) {
        jl_varidx_t *pe = env;
        int i = 0;
        while (pe != NULL) {
            if (pe->var == (jl_tvar_t*)v)
                return (i<<8) + 42;
            i++;
            pe = pe->prev;
        }
        return inthash((uintptr_t)v);
    }
    if (tv == jl_uniontype_type) {
        return bitmix(bitmix(jl_object_id((jl_value_t*)tv),
                             type_object_id_(((jl_uniontype_t*)v)->a, env)),
                      type_object_id_(((jl_uniontype_t*)v)->b, env));
    }
    if (tv == jl_unionall_type) {
        jl_unionall_t *u = (jl_unionall_t*)v;
        uintptr_t h = u->var->name->hash;
        h = bitmix(h, type_object_id_(u->var->lb, env));
        h = bitmix(h, type_object_id_(u->var->ub, env));
        jl_varidx_t e = { u->var, env };
        return bitmix(h, type_object_id_(u->body, &e));
    }
    if (tv == jl_datatype_type) {
        jl_datatype_t *dtv = (jl_datatype_t*)v;
        if (dtv->isconcretetype)
            return dtv->hash;
        uintptr_t h = ~dtv->name->hash;
        size_t i, l = jl_nparams(v);
        for (i = 0; i < l; i++) {
            h = bitmix(h, type_object_id_(jl_tparam(v, i), env));
        }
        return h;
    }
    if (tv == jl_vararg_type) {
        jl_vararg_t *vm = (jl_vararg_t*)v;
        jl_value_t *t = vm->T ? vm->T : (jl_value_t*)jl_any_type;
        jl_value_t *n = vm->N ? vm->N : jl_nothing;
        return bitmix(type_object_id_(t, env),
            type_object_id_(n, env));
    }
    if (tv == jl_symbol_type)
        return ((jl_sym_t*)v)->hash;
    assert(!tv->name->mutabl);
    return immut_id_(tv, v, tv->hash);
}

static uintptr_t immut_id_(jl_datatype_t *dt, jl_value_t *v, uintptr_t h) JL_NOTSAFEPOINT
{
    size_t sz = jl_datatype_size(dt);
    if (sz == 0)
        return ~h;
    size_t f, nf = jl_datatype_nfields(dt);
    if (nf == 0 || (!dt->layout->haspadding && dt->layout->npointers == 0)) {
        // operate element-wise if there are unused bits inside,
        // otherwise just take the whole data block at once
        // a few select pointers (notably symbol) also have special hash values
        // which may affect the stability of the objectid hash, even though
        // they don't affect egal comparison
        return bits_hash(v, sz) ^ h;
    }
    if (dt == jl_unionall_type)
        return type_object_id_(v, NULL);
    for (f = 0; f < nf; f++) {
        size_t offs = jl_field_offset(dt, f);
        char *vo = (char*)v + offs;
        uintptr_t u;
        if (jl_field_isptr(dt, f)) {
            jl_value_t *f = *(jl_value_t**)vo;
            u = (f == NULL) ? 0 : jl_object_id(f);
        }
        else {
            jl_datatype_t *fieldtype = (jl_datatype_t*)jl_field_type_concrete(dt, f);
            if (jl_is_uniontype(fieldtype)) {
                uint8_t sel = ((uint8_t*)vo)[jl_field_size(dt, f) - 1];
                fieldtype = (jl_datatype_t*)jl_nth_union_component((jl_value_t*)fieldtype, sel);
            }
            assert(jl_is_datatype(fieldtype) && !fieldtype->name->abstract && !fieldtype->name->mutabl);
            int32_t first_ptr = fieldtype->layout->first_ptr;
            if (first_ptr >= 0 && ((jl_value_t**)vo)[first_ptr] == NULL) {
                // If the field is a inline immutable that can be can be undef
                // we need to check to check for undef first since undef struct
                // may have fields that are different but should still be treated as equal.
                u = 0;
            }
            else {
                u = immut_id_(fieldtype, (jl_value_t*)vo, 0);
            }
        }
        h = bitmix(h, u);
    }
    return h;
}

static uintptr_t NOINLINE jl_object_id__cold(jl_datatype_t *dt, jl_value_t *v) JL_NOTSAFEPOINT
{
    if (dt == jl_simplevector_type)
        return hash_svec((jl_svec_t*)v);
    if (dt == jl_datatype_type) {
        jl_datatype_t *dtv = (jl_datatype_t*)v;
        uintptr_t h = ~dtv->name->hash;
        return bitmix(h, hash_svec(dtv->parameters));
    }
    if (dt == jl_string_type) {
#ifdef _P64
        return memhash_seed(jl_string_data(v), jl_string_len(v), 0xedc3b677);
#else
        return memhash32_seed(jl_string_data(v), jl_string_len(v), 0xedc3b677);
#endif
    }
    if (dt->name->mutabl)
        return inthash((uintptr_t)v);
    return immut_id_(dt, v, dt->hash);
}

JL_DLLEXPORT inline uintptr_t jl_object_id_(jl_value_t *tv, jl_value_t *v) JL_NOTSAFEPOINT
{
    jl_datatype_t *dt = (jl_datatype_t*)tv;
    if (dt == jl_symbol_type)
        return ((jl_sym_t*)v)->hash;
    if (dt == jl_typename_type)
        return ((jl_typename_t*)v)->hash;
    if (dt == jl_datatype_type) {
        jl_datatype_t *dtv = (jl_datatype_t*)v;
        if (dtv->isconcretetype)
            return dtv->hash;
    }
    return jl_object_id__cold(dt, v);
}


JL_DLLEXPORT uintptr_t jl_object_id(jl_value_t *v) JL_NOTSAFEPOINT
{
    return jl_object_id_(jl_typeof(v), v);
}

// eq hash table --------------------------------------------------------------

#include "iddict.c"

// object model and type primitives -------------------------------------------

JL_CALLABLE(jl_f_is)
{
    JL_NARGS(===, 2, 2);
    return jl_egal(args[0], args[1]) ? jl_true : jl_false;
}

JL_CALLABLE(jl_f_typeof)
{
    JL_NARGS(typeof, 1, 1);
    return jl_typeof(args[0]);
}

JL_CALLABLE(jl_f_sizeof)
{
    JL_NARGS(sizeof, 1, 1);
    jl_value_t *x = args[0];
    if (jl_is_unionall(x) || jl_is_uniontype(x)) {
        x = jl_unwrap_unionall(x);
        size_t elsize = 0;
        int isinline = jl_uniontype_size(x, &elsize);
        if (isinline)
            return jl_box_long(elsize);
        if (!jl_is_datatype(x))
            jl_error("Argument is an abstract type and does not have a definite size.");
    }
    if (jl_is_datatype(x)) {
        jl_datatype_t *dx = (jl_datatype_t*)x;
        if (dx->layout == NULL) {
            if (dx->name->abstract)
                jl_errorf("Abstract type %s does not have a definite size.", jl_symbol_name(dx->name->name));
            else
                jl_errorf("Argument is an incomplete %s type and does not have a definite size.", jl_symbol_name(dx->name->name));
        }
        if (jl_is_layout_opaque(dx->layout))
            jl_errorf("Type %s does not have a definite size.", jl_symbol_name(dx->name->name));
        return jl_box_long(jl_datatype_size(x));
    }
    if (x == jl_bottom_type)
        jl_error("The empty type does not have a definite size since it does not have instances.");
    if (jl_is_array(x)) {
        return jl_box_long(jl_array_len(x) * ((jl_array_t*)x)->elsize);
    }
    if (jl_is_string(x))
        return jl_box_long(jl_string_len(x));
    if (jl_is_symbol(x))
        return jl_box_long(strlen(jl_symbol_name((jl_sym_t*)x)));
    if (jl_is_svec(x))
        return jl_box_long((1+jl_svec_len(x))*sizeof(void*));
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(x);
    assert(jl_is_datatype(dt));
    assert(!dt->name->abstract);
    return jl_box_long(jl_datatype_size(dt));
}

JL_CALLABLE(jl_f_issubtype)
{
    JL_NARGS(<:, 2, 2);
    jl_value_t *a = args[0], *b = args[1];
    JL_TYPECHK(<:, type, a);
    JL_TYPECHK(<:, type, b);
    return (jl_subtype(a,b) ? jl_true : jl_false);
}

JL_CALLABLE(jl_f_isa)
{
    JL_NARGS(isa, 2, 2);
    JL_TYPECHK(isa, type, args[1]);
    return (jl_isa(args[0],args[1]) ? jl_true : jl_false);
}

JL_CALLABLE(jl_f_typeassert)
{
    JL_NARGS(typeassert, 2, 2);
    JL_TYPECHK(typeassert, type, args[1]);
    if (!jl_isa(args[0],args[1]))
        jl_type_error("typeassert", args[1], args[0]);
    return args[0];
}

JL_CALLABLE(jl_f_throw)
{
    JL_NARGS(throw, 1, 1);
    jl_throw(args[0]);
    return jl_nothing;
}

JL_CALLABLE(jl_f_ifelse)
{
    JL_NARGS(ifelse, 3, 3);
    JL_TYPECHK(ifelse, bool, args[0]);
    return (args[0] == jl_false ? args[2] : args[1]);
}

// apply ----------------------------------------------------------------------

static NOINLINE jl_svec_t *_copy_to(size_t newalloc, jl_value_t **oldargs, size_t oldalloc)
{
    size_t j;
    jl_svec_t *newheap = jl_alloc_svec_uninit(newalloc);
    jl_value_t **newargs = jl_svec_data(newheap);
    for (j = 0; j < oldalloc; j++)
        newargs[j] = oldargs[j];
    for (; j < newalloc; j++)
        newargs[j] = NULL;
    return newheap;
}

STATIC_INLINE void _grow_to(jl_value_t **root, jl_value_t ***oldargs, jl_svec_t **arg_heap, size_t *n_alloc, size_t newalloc, size_t extra)
{
    size_t oldalloc = *n_alloc;
    if (oldalloc >= newalloc)
        return;
    if (extra)
        // grow by an extra 50% if newalloc is still only a guess
        newalloc += oldalloc / 2 + 16;
    JL_GC_PROMISE_ROOTED(*oldargs);
    jl_svec_t *newheap = _copy_to(newalloc, *oldargs, oldalloc);
    *root = (jl_value_t*)newheap;
    *arg_heap = newheap;
    *oldargs = jl_svec_data(newheap);
    *n_alloc = newalloc;
}

static jl_value_t *do_apply( jl_value_t **args, uint32_t nargs, jl_value_t *iterate)
{
    jl_function_t *f = args[0];
    if (nargs == 2) {
        // some common simple cases
        if (f == jl_builtin_svec) {
            if (jl_is_svec(args[1]))
                return args[1];
            if (jl_is_array(args[1])) {
                size_t n = jl_array_len(args[1]);
                jl_svec_t *t = jl_alloc_svec(n);
                JL_GC_PUSH1(&t);
                for (size_t i = 0; i < n; i++) {
                    jl_svecset(t, i, jl_arrayref((jl_array_t*)args[1], i));
                }
                JL_GC_POP();
                return (jl_value_t*)t;
            }
        }
        else if (f == jl_builtin_tuple && jl_is_tuple(args[1])) {
            return args[1];
        }
    }
    // estimate how many real arguments we appear to have
    size_t precount = 1;
    size_t extra = 0;
    size_t i;
    for (i = 1; i < nargs; i++) {
        if (jl_is_svec(args[i])) {
            precount += jl_svec_len(args[i]);
        }
        else if (jl_is_tuple(args[i]) || jl_is_namedtuple(args[i])) {
            precount += jl_nfields(args[i]);
        }
        else if (jl_is_array(args[i])) {
            precount += jl_array_len(args[i]);
        }
        else {
            extra += 1;
        }
    }
    if (extra && iterate == NULL) {
        jl_undefined_var_error(jl_symbol("iterate"));
    }
    // allocate space for the argument array and gc roots for it
    // based on our previous estimates
    // use the stack if we have a good estimate that it is small
    // otherwise, use the heap and grow it incrementally
    // and if there are any extra elements, we'll also need a couple extra roots
    int onstack = (precount + 32 * extra < jl_page_size / sizeof(jl_value_t*));
    size_t stackalloc = onstack ? (precount + 4 * extra + (extra ? 16 : 0)) : 1;
    size_t n_alloc;
    jl_value_t **roots;
    JL_GC_PUSHARGS(roots, stackalloc + (extra ? 2 : 0));
    jl_value_t **newargs;
    jl_svec_t *arg_heap = NULL;
    if (onstack) {
        newargs = roots;
        n_alloc = stackalloc;
    }
    else {
        // put arguments on the heap if there are too many
        newargs = NULL;
        n_alloc = precount;
        if (extra)
            // grow by an extra 50% if newalloc is still only a guess
            n_alloc += n_alloc / 2 + 16;
        arg_heap = jl_alloc_svec(n_alloc);
        roots[0] = (jl_value_t*)arg_heap;
        newargs = jl_svec_data(arg_heap);
    }
    newargs[0] = f;
    precount -= 1;
    size_t n = 1;
    for (i = 1; i < nargs; i++) {
        jl_value_t *ai = args[i];
        if (jl_is_svec(ai)) {
            jl_svec_t *t = (jl_svec_t*)ai;
            size_t j, al = jl_svec_len(t);
            precount = (precount > al) ? precount - al : 0;
            _grow_to(&roots[0], &newargs, &arg_heap, &n_alloc, n + precount + al, extra);
            assert(newargs != NULL); // inform GCChecker that we didn't write a NULL here
            for (j = 0; j < al; j++) {
                newargs[n++] = jl_svecref(t, j);
                // GC Note: here we assume that the return value of `jl_svecref`
                //          will not be young if `arg_heap` becomes old
                //          since they are allocated before `arg_heap`. Otherwise,
                //          we need to add write barrier for !onstack
            }
        }
        else if (jl_is_tuple(ai) || jl_is_namedtuple(ai)) {
            size_t j, al = jl_nfields(ai);
            precount = (precount > al) ? precount - al : 0;
            _grow_to(&roots[0], &newargs, &arg_heap, &n_alloc, n + precount + al, extra);
            assert(newargs != NULL); // inform GCChecker that we didn't write a NULL here
            for (j = 0; j < al; j++) {
                // jl_fieldref may allocate.
                newargs[n++] = jl_fieldref(ai, j);
                if (arg_heap)
                    jl_gc_wb(arg_heap, newargs[n - 1]);
            }
        }
        else if (jl_is_array(ai)) {
            jl_array_t *aai = (jl_array_t*)ai;
            size_t j, al = jl_array_len(aai);
            precount = (precount > al) ? precount - al : 0;
            _grow_to(&roots[0], &newargs, &arg_heap, &n_alloc, n + precount + al, extra);
            assert(newargs != NULL); // inform GCChecker that we didn't write a NULL here
            if (aai->flags.ptrarray) {
                for (j = 0; j < al; j++) {
                    jl_value_t *arg = jl_array_ptr_ref(aai, j);
                    // apply with array splatting may have embedded NULL value (#11772)
                    if (__unlikely(arg == NULL))
                        jl_throw(jl_undefref_exception);
                    newargs[n++] = arg;
                    if (arg_heap)
                        jl_gc_wb(arg_heap, arg);
                }
            }
            else {
                for (j = 0; j < al; j++) {
                    newargs[n++] = jl_arrayref(aai, j);
                    if (arg_heap)
                        jl_gc_wb(arg_heap, newargs[n - 1]);
                }
            }
        }
        else {
            assert(extra > 0);
            jl_value_t *args[2];
            args[0] = ai;
            jl_value_t *next = jl_apply_generic(iterate, args, 1);
            while (next != jl_nothing) {
                roots[stackalloc] = next;
                jl_value_t *value = jl_get_nth_field_checked(next, 0);
                roots[stackalloc + 1] = value;
                jl_value_t *state = jl_get_nth_field_checked(next, 1);
                roots[stackalloc] = state;
                _grow_to(&roots[0], &newargs, &arg_heap, &n_alloc, n + precount + 1, extra);
                JL_GC_ASSERT_LIVE(value);
                newargs[n++] = value;
                if (arg_heap)
                    jl_gc_wb(arg_heap, value);
                roots[stackalloc + 1] = NULL;
                JL_GC_ASSERT_LIVE(state);
                args[1] = state;
                next = jl_apply_generic(iterate, args, 2);
            }
            roots[stackalloc] = NULL;
            extra -= 1;
        }
    }
    if (arg_heap) {
        // optimization: keep only the first root, free the others
#ifndef __clang_gcanalyzer__
        ((void**)roots)[-2] = (void*)JL_GC_ENCODE_PUSHARGS(1);
#endif
    }
    jl_value_t *result = jl_apply(newargs, n);
    JL_GC_POP();
    return result;
}

JL_CALLABLE(jl_f__apply_iterate)
{
    JL_NARGSV(_apply_iterate, 2);
    return do_apply(args + 1, nargs - 1, args[0]);
}

// this is like `_apply`, but with quasi-exact checks to make sure it is pure
JL_CALLABLE(jl_f__apply_pure)
{
    jl_task_t *ct = jl_current_task;
    int last_in = ct->ptls->in_pure_callback;
    jl_value_t *ret = NULL;
    JL_TRY {
        ct->ptls->in_pure_callback = 1;
        // because this function was declared pure,
        // we should be allowed to run it in any world
        // so we run it in the newest world;
        // because, why not :)
        // and `promote` works better this way
        size_t last_age = ct->world_age;
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        ret = do_apply(args, nargs, NULL);
        ct->world_age = last_age;
        ct->ptls->in_pure_callback = last_in;
    }
    JL_CATCH {
        ct->ptls->in_pure_callback = last_in;
        jl_rethrow();
    }
    return ret;
}

// this is like a regular call, but always runs in the newest world
JL_CALLABLE(jl_f__call_latest)
{
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    if (!ct->ptls->in_pure_callback)
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
    jl_value_t *ret = jl_apply(args, nargs);
    ct->world_age = last_age;
    return ret;
}

// Like call_in_world, but runs in the specified world.
// If world > jl_atomic_load_acquire(&jl_world_counter), run in the latest world.
JL_CALLABLE(jl_f__call_in_world)
{
    JL_NARGSV(_apply_in_world, 2);
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    JL_TYPECHK(_apply_in_world, ulong, args[0]);
    size_t world = jl_unbox_ulong(args[0]);
    if (!ct->ptls->in_pure_callback) {
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        if (ct->world_age > world)
            ct->world_age = world;
    }
    jl_value_t *ret = jl_apply(&args[1], nargs - 1);
    ct->world_age = last_age;
    return ret;
}

JL_CALLABLE(jl_f__call_in_world_total)
{
    JL_NARGSV(_call_in_world_total, 2);
    JL_TYPECHK(_apply_in_world, ulong, args[0]);
    jl_task_t *ct = jl_current_task;
    int last_in = ct->ptls->in_pure_callback;
    jl_value_t *ret = NULL;
    size_t last_age = ct->world_age;
    JL_TRY {
        ct->ptls->in_pure_callback = 1;
        size_t world = jl_unbox_ulong(args[0]);
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        if (ct->world_age > world)
            ct->world_age = world;
        ret = jl_apply(&args[1], nargs - 1);
        ct->world_age = last_age;
        ct->ptls->in_pure_callback = last_in;
    }
    JL_CATCH {
        ct->ptls->in_pure_callback = last_in;
        jl_rethrow();
    }
    return ret;
}

// tuples ---------------------------------------------------------------------

JL_CALLABLE(jl_f_tuple)
{
    size_t i;
    if (nargs == 0)
        return (jl_value_t*)jl_emptytuple;
    jl_datatype_t *tt = jl_inst_arg_tuple_type(args[0], &args[1], nargs, 0);
    JL_GC_PROMISE_ROOTED(tt); // it is a concrete type
    if (tt->instance != NULL)
        return tt->instance;
    jl_task_t *ct = jl_current_task;
    jl_value_t *jv = jl_gc_alloc(ct->ptls, jl_datatype_size(tt), tt);
    for (i = 0; i < nargs; i++)
        set_nth_field(tt, jv, i, args[i], 0);
    return jv;
}

JL_CALLABLE(jl_f_svec)
{
    size_t i;
    if (nargs == 0)
        return (jl_value_t*)jl_emptysvec;
    jl_svec_t *t = jl_alloc_svec_uninit(nargs);
    for (i = 0; i < nargs; i++) {
        jl_svecset(t, i, args[i]);
    }
    return (jl_value_t*)t;
}

// struct operations ------------------------------------------------------------

enum jl_memory_order jl_get_atomic_order(jl_sym_t *order, char loading, char storing)
{
    if (order == jl_not_atomic_sym)
        return jl_memory_order_notatomic;
    if (order == jl_unordered_sym && (loading ^ storing))
        return jl_memory_order_unordered;
    if (order == jl_monotonic_sym && (loading || storing))
        return jl_memory_order_monotonic;
    if (order == jl_acquire_sym && loading)
        return jl_memory_order_acquire;
    if (order == jl_release_sym && storing)
        return jl_memory_order_release;
    if (order == jl_acquire_release_sym && loading && storing)
        return jl_memory_order_acq_rel;
    if (order == jl_sequentially_consistent_sym)
        return jl_memory_order_seq_cst;
    return jl_memory_order_invalid;
}

enum jl_memory_order jl_get_atomic_order_checked(jl_sym_t *order, char loading, char storing)
{
    enum jl_memory_order mo = jl_get_atomic_order(order, loading, storing);
    if (mo < 0) // invalid
        jl_atomic_error("invalid atomic ordering");
    return mo;
}

static inline size_t get_checked_fieldindex(const char *name, jl_datatype_t *st, jl_value_t *v, jl_value_t *arg, int mutabl)
{
    if (mutabl) {
        if (st == jl_module_type)
            jl_error("cannot assign variables in other modules");
        if (!st->name->mutabl)
            jl_errorf("%s: immutable struct of type %s cannot be changed", name, jl_symbol_name(st->name->name));
    }
    size_t idx;
    if (jl_is_long(arg)) {
        idx = jl_unbox_long(arg) - 1;
        if (idx >= jl_datatype_nfields(st))
            jl_bounds_error(v, arg);
    }
    else {
        JL_TYPECHKS(name, symbol, arg);
        idx = jl_field_index(st, (jl_sym_t*)arg, 1);
    }
    if (mutabl && jl_field_isconst(st, idx)) {
        jl_errorf("%s: const field .%s of type %s cannot be changed", name,
                jl_symbol_name((jl_sym_t*)jl_svec_ref(jl_field_names(st), idx)), jl_symbol_name(st->name->name));
    }
    return idx;
}

JL_CALLABLE(jl_f_getfield)
{
    enum jl_memory_order order = jl_memory_order_unspecified;
    JL_NARGS(getfield, 2, 4);
    if (nargs == 4) {
        JL_TYPECHK(getfield, symbol, args[2]);
        JL_TYPECHK(getfield, bool, args[3]);
        order = jl_get_atomic_order_checked((jl_sym_t*)args[2], 1, 0);
    }
    else if (nargs == 3) {
        if (!jl_is_bool(args[2])) {
            JL_TYPECHK(getfield, symbol, args[2]);
            order = jl_get_atomic_order_checked((jl_sym_t*)args[2], 1, 0);
        }
    }
    jl_value_t *v = args[0];
    jl_value_t *vt = jl_typeof(v);
    if (vt == (jl_value_t*)jl_module_type)
        return jl_f_getglobal(NULL, args, 2); // we just ignore the atomic order and boundschecks
    jl_datatype_t *st = (jl_datatype_t*)vt;
    size_t idx = get_checked_fieldindex("getfield", st, v, args[1], 0);
    int isatomic = jl_field_isatomic(st, idx);
    if (!isatomic && order != jl_memory_order_notatomic && order != jl_memory_order_unspecified)
        jl_atomic_error("getfield: non-atomic field cannot be accessed atomically");
    if (isatomic && order == jl_memory_order_notatomic)
        jl_atomic_error("getfield: atomic field cannot be accessed non-atomically");
    v = jl_get_nth_field_checked(v, idx);
    if (order >= jl_memory_order_acq_rel || order == jl_memory_order_acquire)
        jl_fence(); // `v` already had at least consume ordering
    return v;
}

JL_CALLABLE(jl_f_setfield)
{
    enum jl_memory_order order = jl_memory_order_notatomic;
    JL_NARGS(setfield!, 3, 4);
    if (nargs == 4) {
        JL_TYPECHK(setfield!, symbol, args[3]);
        order = jl_get_atomic_order_checked((jl_sym_t*)args[3], 0, 1);
    }
    jl_value_t *v = args[0];
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t idx = get_checked_fieldindex("setfield!", st, v, args[1], 1);
    int isatomic = !!jl_field_isatomic(st, idx);
    if (isatomic == (order == jl_memory_order_notatomic))
        jl_atomic_error(isatomic ? "setfield!: atomic field cannot be written non-atomically"
                                 : "setfield!: non-atomic field cannot be written atomically");
    jl_value_t *ft = jl_field_type_concrete(st, idx);
    if (!jl_isa(args[2], ft))
        jl_type_error("setfield!", ft, args[2]);
    if (order >= jl_memory_order_acq_rel || order == jl_memory_order_release)
        jl_fence(); // `st->[idx]` will have at least relaxed ordering
    set_nth_field(st, v, idx, args[2], isatomic);
    return args[2];
}

JL_CALLABLE(jl_f_swapfield)
{
    enum jl_memory_order order = jl_memory_order_notatomic;
    JL_NARGS(swapfield!, 3, 4);
    if (nargs == 4) {
        JL_TYPECHK(swapfield!, symbol, args[3]);
        order = jl_get_atomic_order_checked((jl_sym_t*)args[3], 1, 1);
    }
    jl_value_t *v = args[0];
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t idx = get_checked_fieldindex("swapfield!", st, v, args[1], 1);
    int isatomic = !!jl_field_isatomic(st, idx);
    if (isatomic == (order == jl_memory_order_notatomic))
        jl_atomic_error(isatomic ? "swapfield!: atomic field cannot be written non-atomically"
                                 : "swapfield!: non-atomic field cannot be written atomically");
    v = swap_nth_field(st, v, idx, args[2], isatomic); // always seq_cst, if isatomic needed at all
    return v;
}

JL_CALLABLE(jl_f_modifyfield)
{
    enum jl_memory_order order = jl_memory_order_notatomic;
    JL_NARGS(modifyfield!, 4, 5);
    if (nargs == 5) {
        JL_TYPECHK(modifyfield!, symbol, args[4]);
        order = jl_get_atomic_order_checked((jl_sym_t*)args[4], 1, 1);
    }
    jl_value_t *v = args[0];
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t idx = get_checked_fieldindex("modifyfield!", st, v, args[1], 1);
    int isatomic = !!jl_field_isatomic(st, idx);
    if (isatomic == (order == jl_memory_order_notatomic))
        jl_atomic_error(isatomic ? "modifyfield!: atomic field cannot be written non-atomically"
                                 : "modifyfield!: non-atomic field cannot be written atomically");
    v = modify_nth_field(st, v, idx, args[2], args[3], isatomic); // always seq_cst, if isatomic needed at all
    return v;
}

JL_CALLABLE(jl_f_replacefield)
{
    enum jl_memory_order success_order = jl_memory_order_notatomic;
    JL_NARGS(replacefield!, 4, 6);
    if (nargs >= 5) {
        JL_TYPECHK(replacefield!, symbol, args[4]);
        success_order = jl_get_atomic_order_checked((jl_sym_t*)args[4], 1, 1);
    }
    enum jl_memory_order failure_order = success_order;
    if (nargs == 6) {
        JL_TYPECHK(replacefield!, symbol, args[5]);
        failure_order = jl_get_atomic_order_checked((jl_sym_t*)args[5], 1, 0);
    }
    if (failure_order > success_order)
        jl_atomic_error("invalid atomic ordering");
    // TODO: filter more invalid ordering combinations?
    jl_value_t *v = args[0];
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t idx = get_checked_fieldindex("replacefield!", st, v, args[1], 1);
    int isatomic = !!jl_field_isatomic(st, idx);
    if (isatomic == (success_order == jl_memory_order_notatomic))
        jl_atomic_error(isatomic ? "replacefield!: atomic field cannot be written non-atomically"
                                 : "replacefield!: non-atomic field cannot be written atomically");
    if (isatomic == (failure_order == jl_memory_order_notatomic))
        jl_atomic_error(isatomic ? "replacefield!: atomic field cannot be accessed non-atomically"
                                 : "replacefield!: non-atomic field cannot be accessed atomically");
    v = replace_nth_field(st, v, idx, args[2], args[3], isatomic); // always seq_cst, if isatomic needed at all
    return v;
}


static jl_value_t *get_fieldtype(jl_value_t *t, jl_value_t *f, int dothrow)
{
    if (jl_is_unionall(t)) {
        jl_value_t *u = t;
        JL_GC_PUSH1(&u);
        u = get_fieldtype(((jl_unionall_t*)t)->body, f, dothrow);
        u = jl_type_unionall(((jl_unionall_t*)t)->var, u);
        JL_GC_POP();
        return u;
    }
    if (jl_is_uniontype(t)) {
        jl_value_t **u;
        jl_value_t *r;
        JL_GC_PUSHARGS(u, 2);
        u[0] = get_fieldtype(((jl_uniontype_t*)t)->a, f, 0);
        u[1] = get_fieldtype(((jl_uniontype_t*)t)->b, f, 0);
        if (u[0] == jl_bottom_type && u[1] == jl_bottom_type && dothrow) {
            // error if all types in the union might have
            get_fieldtype(((jl_uniontype_t*)t)->a, f, 1);
            get_fieldtype(((jl_uniontype_t*)t)->b, f, 1);
        }
        r = jl_type_union(u, 2);
        JL_GC_POP();
        return r;
    }
    if (!jl_is_datatype(t))
        jl_type_error("fieldtype", (jl_value_t*)jl_datatype_type, t);
    jl_datatype_t *st = (jl_datatype_t*)t;
    int field_index;
    if (jl_is_long(f)) {
        field_index = jl_unbox_long(f) - 1;
    }
    else {
        JL_TYPECHK(fieldtype, symbol, f);
        field_index = jl_field_index(st, (jl_sym_t*)f, dothrow);
        if (field_index == -1)
            return jl_bottom_type;
    }
    if (st->name == jl_namedtuple_typename) {
        jl_value_t *nm = jl_tparam0(st);
        if (jl_is_tuple(nm)) {
            int nf = jl_nfields(nm);
            if (field_index < 0 || field_index >= nf) {
                if (dothrow)
                    jl_bounds_error(t, f);
                else
                    return jl_bottom_type;
            }
        }
        jl_value_t *tt = jl_tparam1(st);
        while (jl_is_typevar(tt))
            tt = ((jl_tvar_t*)tt)->ub;
        if (tt == (jl_value_t*)jl_any_type)
            return (jl_value_t*)jl_any_type;
        JL_GC_PUSH1(&f);
        if (jl_is_symbol(f))
            f = jl_box_long(field_index+1);
        jl_value_t *ft = get_fieldtype(tt, f, dothrow);
        JL_GC_POP();
        return ft;
    }
    jl_svec_t *types = jl_get_fieldtypes(st);
    int nf = jl_svec_len(types);
    if (nf > 0 && field_index >= nf-1 && st->name == jl_tuple_typename) {
        jl_value_t *ft = jl_field_type(st, nf-1);
        if (jl_is_vararg(ft))
            return jl_unwrap_vararg(ft);
    }
    if (field_index < 0 || field_index >= nf) {
        if (dothrow)
            jl_bounds_error(t, f);
        else
            return jl_bottom_type;
    }
    return jl_field_type(st, field_index);
}

JL_CALLABLE(jl_f_fieldtype)
{
    JL_NARGS(fieldtype, 2, 3);
    if (nargs == 3) {
        JL_TYPECHK(fieldtype, bool, args[2]);
    }
    return get_fieldtype(args[0], args[1], 1);
}

JL_CALLABLE(jl_f_nfields)
{
    JL_NARGS(nfields, 1, 1);
    jl_datatype_t *xt = (jl_datatype_t*)jl_typeof(args[0]);
    return jl_box_long(jl_datatype_nfields(xt));
}

JL_CALLABLE(jl_f_isdefined)
{
    jl_module_t *m = NULL;
    jl_sym_t *s = NULL;
    JL_NARGS(isdefined, 2, 3);
    enum jl_memory_order order = jl_memory_order_unspecified;
    if (nargs == 3) {
        JL_TYPECHK(isdefined, symbol, args[2]);
        order = jl_get_atomic_order_checked((jl_sym_t*)args[2], 1, 0);
    }
    if (jl_is_module(args[0])) {
        JL_TYPECHK(isdefined, symbol, args[1]);
        m = (jl_module_t*)args[0];
        s = (jl_sym_t*)args[1];
        return jl_boundp(m, s) ? jl_true : jl_false; // is seq_cst already
    }
    jl_datatype_t *vt = (jl_datatype_t*)jl_typeof(args[0]);
    assert(jl_is_datatype(vt));
    size_t idx;
    if (jl_is_long(args[1])) {
        idx = jl_unbox_long(args[1]) - 1;
        if (idx >= jl_datatype_nfields(vt)) {
            if (order != jl_memory_order_unspecified)
                jl_atomic_error("isdefined: atomic ordering cannot be specified for nonexistent field");
            return jl_false;
        }
    }
    else {
        JL_TYPECHK(isdefined, symbol, args[1]);
        idx = jl_field_index(vt, (jl_sym_t*)args[1], 0);
        if ((int)idx == -1) {
            if (order != jl_memory_order_unspecified)
                jl_atomic_error("isdefined: atomic ordering cannot be specified for nonexistent field");
            return jl_false;
        }
    }
    int isatomic = jl_field_isatomic(vt, idx);
    if (!isatomic && order != jl_memory_order_notatomic && order != jl_memory_order_unspecified)
        jl_atomic_error("isdefined: non-atomic field cannot be accessed atomically");
    if (isatomic && order == jl_memory_order_notatomic)
        jl_atomic_error("isdefined: atomic field cannot be accessed non-atomically");
    int v = jl_field_isdefined(args[0], idx);
    if (v == 2) {
        if (order > jl_memory_order_notatomic)
            jl_fence(); // isbits case has no ordering already
    }
    else {
        if (order >= jl_memory_order_acq_rel || order == jl_memory_order_acquire)
            jl_fence(); // `v` already gave at least consume ordering
    }
    return v ? jl_true : jl_false;
}


// module bindings

JL_CALLABLE(jl_f_getglobal)
{
    enum jl_memory_order order = jl_memory_order_monotonic;
    JL_NARGS(getglobal, 2, 3);
    if (nargs == 3) {
        JL_TYPECHK(getglobal, symbol, args[2]);
        order = jl_get_atomic_order_checked((jl_sym_t*)args[2], 1, 0);
    }
    JL_TYPECHK(getglobal, module, args[0]);
    JL_TYPECHK(getglobal, symbol, args[1]);
    if (order == jl_memory_order_notatomic)
        jl_atomic_error("getglobal: module binding cannot be read non-atomically");
    jl_value_t *v = jl_eval_global_var((jl_module_t*)args[0], (jl_sym_t*)args[1]);
    // is seq_cst already, no fence needed
    return v;
}

JL_CALLABLE(jl_f_setglobal)
{
    enum jl_memory_order order = jl_memory_order_monotonic;
    JL_NARGS(setglobal!, 3, 4);
    if (nargs == 4) {
        JL_TYPECHK(setglobal!, symbol, args[3]);
        order = jl_get_atomic_order_checked((jl_sym_t*)args[3], 0, 1);
    }
    JL_TYPECHK(setglobal!, module, args[0]);
    JL_TYPECHK(setglobal!, symbol, args[1]);
    if (order == jl_memory_order_notatomic)
        jl_atomic_error("setglobal!: module binding cannot be written non-atomically");
    // is seq_cst already, no fence needed
    jl_binding_t *b = jl_get_binding_wr_or_error((jl_module_t*)args[0], (jl_sym_t*)args[1]);
    jl_checked_assignment(b, args[2]);
    return args[2];
}

JL_CALLABLE(jl_f_get_binding_type)
{
    JL_NARGS(get_binding_type, 2, 2);
    JL_TYPECHK(get_binding_type, module, args[0]);
    JL_TYPECHK(get_binding_type, symbol, args[1]);
    jl_module_t *mod = (jl_module_t*)args[0];
    jl_sym_t *sym = (jl_sym_t*)args[1];
    jl_value_t *ty = jl_binding_type(mod, sym);
    if (ty == (jl_value_t*)jl_nothing) {
        jl_binding_t *b = jl_get_binding_wr(mod, sym, 0);
        if (b && b->owner == mod) {
            jl_value_t *old_ty = NULL;
            jl_atomic_cmpswap_relaxed(&b->ty, &old_ty, (jl_value_t*)jl_any_type);
            return jl_atomic_load_relaxed(&b->ty);
        }
        return (jl_value_t*)jl_any_type;
    }
    return ty;
}

JL_CALLABLE(jl_f_set_binding_type)
{
    JL_NARGS(set_binding_type!, 2, 3);
    JL_TYPECHK(set_binding_type!, module, args[0]);
    JL_TYPECHK(set_binding_type!, symbol, args[1]);
    jl_value_t *ty = nargs == 2 ? (jl_value_t*)jl_any_type : args[2];
    JL_TYPECHK(set_binding_type!, type, ty);
    jl_binding_t *b = jl_get_binding_wr((jl_module_t*)args[0], (jl_sym_t*)args[1], 1);
    jl_value_t *old_ty = NULL;
    if (!jl_atomic_cmpswap_relaxed(&b->ty, &old_ty, ty) && ty != old_ty) {
        if (nargs == 2)
            return jl_nothing;
        jl_errorf("cannot set type for global %s. It already has a value or is already set to a different type.",
                  jl_symbol_name(b->name));
    }
    return jl_nothing;
}


// apply_type -----------------------------------------------------------------

int jl_valid_type_param(jl_value_t *v)
{
    if (jl_is_tuple(v)) {
        // NOTE: tuples of symbols are not currently bits types, but have been
        // allowed as type parameters. this is a bit ugly.
        jl_value_t *tt = jl_typeof(v);
        size_t i, l = jl_nparams(tt);
        for(i=0; i < l; i++) {
            jl_value_t *pi = jl_tparam(tt,i);
            if (!(pi == (jl_value_t*)jl_symbol_type || jl_isbits(pi)))
                return 0;
        }
        return 1;
    }
    if (jl_is_vararg(v))
        return 0;
    // TODO: maybe more things
    return jl_is_type(v) || jl_is_typevar(v) || jl_is_symbol(v) || jl_isbits(jl_typeof(v));
}

JL_CALLABLE(jl_f_apply_type)
{
    JL_NARGSV(apply_type, 1);
    int i;
    if (args[0] == (jl_value_t*)jl_anytuple_type) {
        for(i=1; i < nargs; i++) {
            jl_value_t *pi = args[i];
            // TODO: should possibly only allow Types and TypeVars, but see
            // https://github.com/JuliaLang/julia/commit/85f45974a581ab9af955bac600b90d9ab00f093b#commitcomment-13041922
            if (jl_is_vararg(pi)) {
                if (i != nargs-1)
                    jl_type_error_rt("Tuple", "non-final parameter", (jl_value_t*)jl_type_type, pi);
            }
            else if (!jl_valid_type_param(pi)) {
                jl_type_error_rt("Tuple", "parameter", (jl_value_t*)jl_type_type, pi);
            }
        }
        return (jl_value_t*)jl_apply_tuple_type_v(&args[1], nargs-1);
    }
    else if (args[0] == (jl_value_t*)jl_uniontype_type) {
        // Union{} has extra restrictions, so it needs to be checked after
        // substituting typevars (a valid_type_param check here isn't sufficient).
        return (jl_value_t*)jl_type_union(&args[1], nargs-1);
    }
    else if (jl_is_vararg(args[0])) {
        jl_vararg_t *vm = (jl_vararg_t*)args[0];
        if (!vm->T) {
            JL_NARGS(apply_type, 2, 3);
            return (jl_value_t*)jl_wrap_vararg(args[1], nargs == 3 ? args[2] : NULL);
        }
        else if (!vm->N) {
            JL_NARGS(apply_type, 2, 2);
            return (jl_value_t*)jl_wrap_vararg(vm->T, args[1]);
        }
    }
    else if (jl_is_unionall(args[0])) {
        for(i=1; i < nargs; i++) {
            jl_value_t *pi = args[i];
            if (!jl_valid_type_param(pi)) {
                jl_type_error_rt("Type", "parameter",
                                 jl_isa(pi, (jl_value_t*)jl_number_type) ?
                                 (jl_value_t*)jl_long_type : (jl_value_t*)jl_type_type,
                                 pi);
            }
        }
        return jl_apply_type(args[0], &args[1], nargs-1);
    }
    jl_type_error("Type{...} expression", (jl_value_t*)jl_unionall_type, args[0]);
}

// generic function reflection ------------------------------------------------

JL_CALLABLE(jl_f_applicable)
{
    JL_NARGSV(applicable, 1);
    size_t world = jl_current_task->world_age;
    return jl_method_lookup(args, nargs, world) != NULL ? jl_true : jl_false;
}

JL_CALLABLE(jl_f_invoke)
{
    JL_NARGSV(invoke, 2);
    jl_value_t *argtypes = args[1];
    JL_GC_PUSH1(&argtypes);
    if (!jl_is_tuple_type(jl_unwrap_unionall(args[1])))
        jl_type_error("invoke", (jl_value_t*)jl_anytuple_type_type, args[1]);
    if (!jl_tuple_isa(&args[2], nargs - 2, (jl_datatype_t*)argtypes))
        jl_error("invoke: argument type error");
    jl_value_t *res = jl_gf_invoke(argtypes, args[0], &args[2], nargs - 1);
    JL_GC_POP();
    return res;
}

JL_CALLABLE(jl_f_invoke_kwsorter)
{
    JL_NARGSV(invoke, 3);
    jl_value_t *kwargs = args[0];
    // args[1] is `invoke` itself
    jl_value_t *func = args[2];
    jl_value_t *argtypes = args[3];
    jl_value_t *kws = jl_get_keyword_sorter(func);
    JL_GC_PUSH1(&argtypes);
    if (jl_is_tuple_type(argtypes)) {
        // construct a tuple type for invoking a keyword sorter by putting the kw container type
        // and the type of the function at the front.
        size_t i, nt = jl_nparams(argtypes) + 2;
        if (nt < jl_page_size/sizeof(jl_value_t*)) {
            jl_value_t **types = (jl_value_t**)alloca(nt*sizeof(jl_value_t*));
            types[0] = (jl_value_t*)jl_namedtuple_type;
            types[1] = jl_is_type(func) ? (jl_value_t*)jl_wrap_Type(func) : jl_typeof(func);
            for (i = 2; i < nt; i++)
                types[i] = jl_tparam(argtypes, i - 2);
            argtypes = (jl_value_t*)jl_apply_tuple_type_v(types, nt);
        }
        else {
            jl_svec_t *types = jl_alloc_svec_uninit(nt);
            JL_GC_PUSH1(&types);
            jl_svecset(types, 0, jl_namedtuple_type);
            jl_svecset(types, 1, jl_is_type(func) ? (jl_value_t*)jl_wrap_Type(func) : jl_typeof(func));
            for (i = 2; i < nt; i++)
                jl_svecset(types, i, jl_tparam(argtypes, i - 2));
            argtypes = (jl_value_t*)jl_apply_tuple_type(types);
            JL_GC_POP();
        }
    }
    else {
        // invoke will throw an error
    }
    args[0] = kws;
    args[1] = argtypes;
    args[2] = kwargs;
    args[3] = func;
    jl_value_t *res = jl_f_invoke(NULL, args, nargs);
    JL_GC_POP();
    return res;
}

// Expr constructor for internal use ------------------------------------------

jl_expr_t *jl_exprn(jl_sym_t *head, size_t n)
{
    jl_task_t *ct = jl_current_task;
    jl_array_t *ar = jl_alloc_vec_any(n);
    JL_GC_PUSH1(&ar);
    jl_expr_t *ex = (jl_expr_t*)jl_gc_alloc(ct->ptls, sizeof(jl_expr_t),
                                            jl_expr_type);
    ex->head = head;
    ex->args = ar;
    JL_GC_POP();
    return ex;
}

JL_CALLABLE(jl_f__expr)
{
    jl_task_t *ct = jl_current_task;
    JL_NARGSV(Expr, 1);
    JL_TYPECHK(Expr, symbol, args[0]);
    jl_array_t *ar = jl_alloc_vec_any(nargs-1);
    JL_GC_PUSH1(&ar);
    for(size_t i=0; i < nargs-1; i++)
        jl_array_ptr_set(ar, i, args[i+1]);
    jl_expr_t *ex = (jl_expr_t*)jl_gc_alloc(ct->ptls, sizeof(jl_expr_t),
                                            jl_expr_type);
    ex->head = (jl_sym_t*)args[0];
    ex->args = ar;
    JL_GC_POP();
    return (jl_value_t*)ex;
}

// Typevar constructor for internal use
JL_DLLEXPORT jl_tvar_t *jl_new_typevar(jl_sym_t *name, jl_value_t *lb, jl_value_t *ub)
{
    if (lb != jl_bottom_type && !jl_is_type(lb) && !jl_is_typevar(lb))
        jl_type_error_rt("TypeVar", "lower bound", (jl_value_t *)jl_type_type, lb);
    if (ub != (jl_value_t *)jl_any_type && !jl_is_type(ub) && !jl_is_typevar(ub))
        jl_type_error_rt("TypeVar", "upper bound", (jl_value_t *)jl_type_type, ub);
    jl_task_t *ct = jl_current_task;
    jl_tvar_t *tv = (jl_tvar_t *)jl_gc_alloc(ct->ptls, sizeof(jl_tvar_t), jl_tvar_type);
    tv->name = name;
    tv->lb = lb;
    tv->ub = ub;
    return tv;
}

JL_CALLABLE(jl_f__typevar)
{
    JL_NARGS(TypeVar, 3, 3);
    JL_TYPECHK(TypeVar, symbol, args[0]);
    return (jl_value_t *)jl_new_typevar((jl_sym_t*)args[0], args[1], args[2]);
}

// arrays ---------------------------------------------------------------------

JL_CALLABLE(jl_f_arraysize)
{
    JL_NARGS(arraysize, 2, 2);
    JL_TYPECHK(arraysize, array, args[0]);
    jl_array_t *a = (jl_array_t*)args[0];
    size_t nd = jl_array_ndims(a);
    JL_TYPECHK(arraysize, long, args[1]);
    int dno = jl_unbox_long(args[1]);
    if (dno < 1)
        jl_error("arraysize: dimension out of range");
    if (dno > nd)
        return jl_box_long(1);
    return jl_box_long((&a->nrows)[dno-1]);
}

static size_t array_nd_index(jl_array_t *a, jl_value_t **args, size_t nidxs,
                             const char *fname)
{
    size_t i = 0;
    size_t k, stride = 1;
    size_t nd = jl_array_ndims(a);
    for (k = 0; k < nidxs; k++) {
        if (!jl_is_long(args[k]))
            jl_type_error(fname, (jl_value_t*)jl_long_type, args[k]);
        size_t ii = jl_unbox_long(args[k]) - 1;
        i += ii * stride;
        size_t d = (k >= nd) ? 1 : jl_array_dim(a, k);
        if (k < nidxs - 1 && ii >= d)
            jl_bounds_error_v((jl_value_t*)a, args, nidxs);
        stride *= d;
    }
    for (; k < nd; k++)
        stride *= jl_array_dim(a, k);
    if (i >= stride)
        jl_bounds_error_v((jl_value_t*)a, args, nidxs);
    return i;
}

JL_CALLABLE(jl_f_arrayref)
{
    JL_NARGSV(arrayref, 3);
    JL_TYPECHK(arrayref, bool, args[0]);
    JL_TYPECHK(arrayref, array, args[1]);
    jl_array_t *a = (jl_array_t*)args[1];
    size_t i = array_nd_index(a, &args[2], nargs - 2, "arrayref");
    return jl_arrayref(a, i);
}

JL_CALLABLE(jl_f_const_arrayref)
{
    return jl_f_arrayref(F, args, nargs);
}

JL_CALLABLE(jl_f_arrayset)
{
    JL_NARGSV(arrayset, 4);
    JL_TYPECHK(arrayset, bool, args[0]);
    JL_TYPECHK(arrayset, array, args[1]);
    jl_array_t *a = (jl_array_t*)args[1];
    size_t i = array_nd_index(a, &args[3], nargs - 3, "arrayset");
    jl_arrayset(a, args[2], i);
    return args[1];
}

// type definition ------------------------------------------------------------

JL_CALLABLE(jl_f__structtype)
{
    JL_NARGS(_structtype, 7, 7);
    JL_TYPECHK(_structtype, module, args[0]);
    JL_TYPECHK(_structtype, symbol, args[1]);
    JL_TYPECHK(_structtype, simplevector, args[2]);
    JL_TYPECHK(_structtype, simplevector, args[3]);
    JL_TYPECHK(_structtype, simplevector, args[4]);
    JL_TYPECHK(_structtype, bool, args[5]);
    JL_TYPECHK(_structtype, long, args[6]);
    jl_value_t *fieldnames = args[3];
    jl_value_t *fieldattrs = args[4];
    jl_datatype_t *dt = NULL;
    dt = jl_new_datatype((jl_sym_t*)args[1], (jl_module_t*)args[0], NULL, (jl_svec_t*)args[2],
                         (jl_svec_t*)fieldnames, NULL, (jl_svec_t*)fieldattrs,
                         0, args[5]==jl_true ? 1 : 0, jl_unbox_long(args[6]));
    return dt->name->wrapper;
}

JL_CALLABLE(jl_f__abstracttype)
{
    JL_NARGS(_abstracttype, 3, 3);
    JL_TYPECHK(_abstracttype, module, args[0]);
    JL_TYPECHK(_abstracttype, symbol, args[1]);
    JL_TYPECHK(_abstracttype, simplevector, args[2]);
    jl_datatype_t *dt = jl_new_abstracttype(args[1], (jl_module_t*)args[0], NULL, (jl_svec_t*)args[2]);
    return dt->name->wrapper;
}

JL_CALLABLE(jl_f__primitivetype)
{
    JL_NARGS(_primitivetype, 4, 4);
    JL_TYPECHK(_primitivetype, module, args[0]);
    JL_TYPECHK(_primitivetype, symbol, args[1]);
    JL_TYPECHK(_primitivetype, simplevector, args[2]);
    jl_sym_t *name = (jl_sym_t*)args[1];
    jl_value_t *vnb = args[3];
    if (!jl_is_long(vnb))
        jl_errorf("invalid declaration of primitive type %s",
                  jl_symbol_name((jl_sym_t*)name));
    ssize_t nb = jl_unbox_long(vnb);
    if (nb < 1 || nb >= (1 << 23) || (nb & 7) != 0)
        jl_errorf("invalid number of bits in primitive type %s",
                  jl_symbol_name((jl_sym_t*)name));
    jl_datatype_t *dt = jl_new_primitivetype(args[1], (jl_module_t*)args[0], NULL, (jl_svec_t*)args[2], nb);
    return dt->name->wrapper;
}

static void jl_set_datatype_super(jl_datatype_t *tt, jl_value_t *super)
{
    const char *error = NULL;
    if (!jl_is_datatype(super))
        error = "can only subtype data types";
    else if (tt->super != NULL)
        error = "type already has a supertype";
    else if (tt->name == ((jl_datatype_t*)super)->name)
        error = "a type cannot subtype itself";
    else if (jl_is_tuple_type(super))
        error = "cannot subtype a tuple type";
    else if (jl_is_namedtuple_type(super))
        error = "cannot subtype a named tuple type";
    else if (jl_subtype(super, (jl_value_t*)jl_type_type))
        error = "cannot add subtypes to Type";
    else if (jl_subtype(super, (jl_value_t*)jl_builtin_type))
        error = "cannot add subtypes to Core.Builtin";
    else if (!jl_is_abstracttype(super))
        error = "can only subtype abstract types";
    if (error)
         jl_errorf("invalid subtyping in definition of %s: %s.", jl_symbol_name(tt->name->name), error);
    tt->super = (jl_datatype_t*)super;
    jl_gc_wb(tt, tt->super);
}

JL_CALLABLE(jl_f__setsuper)
{
    JL_NARGS(_setsuper!, 2, 2);
    jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(args[0]);
    JL_TYPECHK(_setsuper!, datatype, (jl_value_t*)dt);
    jl_set_datatype_super(dt, args[1]);
    return jl_nothing;
}

JL_CALLABLE(jl_f_donotdelete)
{
    return jl_nothing;
}

JL_CALLABLE(jl_f_finalizer)
{
    JL_NARGS(finalizer, 2, 4);
    jl_task_t *ct = jl_current_task;
    jl_gc_add_finalizer_(ct->ptls, args[1], args[0]);
    return jl_nothing;
}

static int equiv_field_types(jl_value_t *old, jl_value_t *ft)
{
    size_t nf = jl_svec_len(ft);
    if (jl_svec_len(old) != nf)
        return 0;
    size_t i;
    for (i = 0; i < nf; i++) {
        jl_value_t *ta = jl_svecref(old, i);
        jl_value_t *tb = jl_svecref(ft, i);
        if (jl_has_free_typevars(ta)) {
            if (!jl_has_free_typevars(tb) || !jl_egal(ta, tb))
                return 0;
        }
        else if (jl_has_free_typevars(tb) || jl_typeof(ta) != jl_typeof(tb) ||
                 !jl_types_equal(ta, tb)) {
            return 0;
        }
    }
    return 1;
}

// If a field can reference its enclosing type, then the inlining
// recursive depth is not statically bounded for some layouts, so we cannot
// inline it. The only way fields can reference this type (due to
// syntax-enforced restrictions) is via being passed as a type parameter. Thus
// we can conservatively check this by examining only the parameters of the
// dependent types.
// affects_layout is a hack introduced by #35275 to workaround a problem
// introduced by #34223: it checks whether we will potentially need to
// compute the layout of the object before we have fully computed the types of
// the fields during recursion over the allocation of the parameters for the
// field types (of the concrete subtypes)
static int references_name(jl_value_t *p, jl_typename_t *name, int affects_layout) JL_NOTSAFEPOINT
{
    if (jl_is_uniontype(p))
        return references_name(((jl_uniontype_t*)p)->a, name, affects_layout) ||
               references_name(((jl_uniontype_t*)p)->b, name, affects_layout);
    if (jl_is_unionall(p))
        return references_name((jl_value_t*)((jl_unionall_t*)p)->var->lb, name, 0) ||
               references_name((jl_value_t*)((jl_unionall_t*)p)->var->ub, name, 0) ||
               references_name(((jl_unionall_t*)p)->body, name, affects_layout);
    if (jl_is_typevar(p))
        return 0; // already checked by unionall, if applicable
    if (jl_is_datatype(p)) {
        jl_datatype_t *dp = (jl_datatype_t*)p;
        if (affects_layout && dp->name == name)
            return 1;
        // affects_layout checks whether we will need to attempt to layout this
        // type (based on whether all copies of it have the same layout) in
        // that case, we still need to check the recursive parameters for
        // layout recursion happening also, but we know it won't itself cause
        // problems for the layout computation
        affects_layout = ((jl_datatype_t*)jl_unwrap_unionall(dp->name->wrapper))->layout == NULL;
        size_t i, l = jl_nparams(p);
        for (i = 0; i < l; i++) {
            if (references_name(jl_tparam(p, i), name, affects_layout))
                return 1;
        }
    }
    return 0;
}


JL_CALLABLE(jl_f__typebody)
{
    JL_NARGS(_typebody!, 1, 2);
    jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(args[0]);
    JL_TYPECHK(_typebody!, datatype, (jl_value_t*)dt);
    if (nargs == 2) {
        jl_value_t *ft = args[1];
        JL_TYPECHK(_typebody!, simplevector, ft);
        size_t nf = jl_svec_len(ft);
        for (size_t i = 0; i < nf; i++) {
            jl_value_t *elt = jl_svecref(ft, i);
            if (!jl_is_type(elt) && !jl_is_typevar(elt)) {
                jl_type_error_rt(jl_symbol_name(dt->name->name),
                                 "type definition",
                                 (jl_value_t*)jl_type_type, elt);
            }
        }
        if (dt->types != NULL) {
            if (!equiv_field_types((jl_value_t*)dt->types, ft))
                jl_errorf("invalid redefinition of type %s", jl_symbol_name(dt->name->name));
        }
        else {
            dt->types = (jl_svec_t*)ft;
            jl_gc_wb(dt, ft);
            // If a supertype can reference the same type, then we may not be
            // able to compute the layout of the object before needing to
            // publish it, so we must assume it cannot be inlined, if that
            // check passes, then we also still need to check the fields too.
            if (!dt->name->mutabl && (nf == 0 || !references_name((jl_value_t*)dt->super, dt->name, 1))) {
                int mayinlinealloc = 1;
                size_t i;
                for (i = 0; i < nf; i++) {
                    jl_value_t *fld = jl_svecref(ft, i);
                    if (references_name(fld, dt->name, 1)) {
                        mayinlinealloc = 0;
                        break;
                    }
                }
                dt->name->mayinlinealloc = mayinlinealloc;
            }
        }
    }

    JL_TRY {
        jl_reinstantiate_inner_types(dt);
    }
    JL_CATCH {
        dt->name->partial = NULL;
        jl_rethrow();
    }

    if (jl_is_structtype(dt))
        jl_compute_field_offsets(dt);
    return jl_nothing;
}

// this is a heuristic for allowing "redefining" a type to something identical
static int equiv_type(jl_value_t *ta, jl_value_t *tb)
{
    jl_datatype_t *dta = (jl_datatype_t*)jl_unwrap_unionall(ta);
    if (!jl_is_datatype(dta))
        return 0;
    jl_datatype_t *dtb = (jl_datatype_t*)jl_unwrap_unionall(tb);
    if (!(jl_typeof(dta) == jl_typeof(dtb) &&
          dta->name->name == dtb->name->name &&
          dta->name->abstract == dtb->name->abstract &&
          dta->name->mutabl == dtb->name->mutabl &&
          dta->name->n_uninitialized == dtb->name->n_uninitialized &&
          (jl_svec_len(jl_field_names(dta)) != 0 || dta->size == dtb->size) &&
          (dta->name->atomicfields == NULL
           ? dtb->name->atomicfields == NULL
           : (dtb->name->atomicfields != NULL &&
              memcmp(dta->name->atomicfields, dtb->name->atomicfields, (jl_svec_len(dta->name->names) + 31) / 32 * sizeof(uint32_t)) == 0)) &&
          (dta->name->constfields == NULL
           ? dtb->name->constfields == NULL
           : (dtb->name->constfields != NULL &&
              memcmp(dta->name->constfields, dtb->name->constfields, (jl_svec_len(dta->name->names) + 31) / 32 * sizeof(uint32_t)) == 0)) &&
          jl_egal((jl_value_t*)jl_field_names(dta), (jl_value_t*)jl_field_names(dtb)) &&
          jl_nparams(dta) == jl_nparams(dtb)))
        return 0;
    jl_value_t *a=NULL, *b=NULL;
    int ok = 1;
    JL_GC_PUSH2(&a, &b);
    a = jl_rewrap_unionall((jl_value_t*)dta->super, dta->name->wrapper);
    b = jl_rewrap_unionall((jl_value_t*)dtb->super, dtb->name->wrapper);
    if (!jl_types_equal(a, b))
        goto no;
    JL_TRY {
        a = jl_apply_type(dtb->name->wrapper, jl_svec_data(dta->parameters), jl_nparams(dta));
    }
    JL_CATCH {
        ok = 0;
    }
    if (!ok)
        goto no;
    assert(jl_is_datatype(a));
    a = dta->name->wrapper;
    b = dtb->name->wrapper;
    while (jl_is_unionall(a)) {
        jl_unionall_t *ua = (jl_unionall_t*)a;
        jl_unionall_t *ub = (jl_unionall_t*)b;
        if (!jl_types_egal(ua->var->lb, ub->var->lb) || !jl_types_egal(ua->var->ub, ub->var->ub) ||
            ua->var->name != ub->var->name)
            goto no;
        a = jl_instantiate_unionall(ua, (jl_value_t*)ub->var);
        b = ub->body;
    }
    JL_GC_POP();
    return 1;
 no:
    JL_GC_POP();
    return 0;
}

JL_CALLABLE(jl_f__equiv_typedef)
{
    JL_NARGS(_equiv_typedef, 2, 2);
    return equiv_type(args[0], args[1]) ? jl_true : jl_false;
}

// IntrinsicFunctions ---------------------------------------------------------

static void (*runtime_fp[num_intrinsics])(void);
static unsigned intrinsic_nargs[num_intrinsics];

JL_CALLABLE(jl_f_intrinsic_call)
{
    JL_TYPECHK(intrinsic_call, intrinsic, F);
    enum intrinsic f = (enum intrinsic)*(uint32_t*)jl_data_ptr(F);
    if (f == cglobal && nargs == 1)
        f = cglobal_auto;
    unsigned fargs = intrinsic_nargs[f];
    if (!fargs)
        jl_errorf("`%s` must be compiled to be called", jl_intrinsic_name(f));
    JL_NARGS(intrinsic_call, fargs, fargs);

    union {
        void (*fptr)(void);
        jl_value_t *(*call1)(jl_value_t*);
        jl_value_t *(*call2)(jl_value_t*, jl_value_t*);
        jl_value_t *(*call3)(jl_value_t*, jl_value_t*, jl_value_t*);
        jl_value_t *(*call4)(jl_value_t*, jl_value_t*, jl_value_t*, jl_value_t*);
        jl_value_t *(*call5)(jl_value_t*, jl_value_t*, jl_value_t*, jl_value_t*, jl_value_t*);
    } fptr;
    fptr.fptr = runtime_fp[f];
    switch (fargs) {
        case 1:
            return fptr.call1(args[0]);
        case 2:
            return fptr.call2(args[0], args[1]);
        case 3:
            return fptr.call3(args[0], args[1], args[2]);
        case 4:
            return fptr.call4(args[0], args[1], args[2], args[3]);
        case 5:
            return fptr.call5(args[0], args[1], args[2], args[3], args[4]);
        default:
            assert(0 && "unexpected number of arguments to an intrinsic function");
    }
    jl_gc_debug_critical_error();
    abort();
}

JL_DLLEXPORT const char *jl_intrinsic_name(int f)
{
    switch ((enum intrinsic)f) {
    default: return "invalid";
#define ADD_I(func, nargs) case func: return #func;
#define ADD_HIDDEN ADD_I
#define ALIAS ADD_I
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS
    }
}

unsigned jl_intrinsic_nargs(int f)
{
    return intrinsic_nargs[f];
}

// init -----------------------------------------------------------------------

static void add_intrinsic_properties(enum intrinsic f, unsigned nargs, void (*pfunc)(void))
{
    intrinsic_nargs[f] = nargs;
    runtime_fp[f] = pfunc;
}

static void add_intrinsic(jl_module_t *inm, const char *name, enum intrinsic f) JL_GC_DISABLED
{
    jl_value_t *i = jl_permbox32(jl_intrinsic_type, (int32_t)f);
    jl_sym_t *sym = jl_symbol(name);
    jl_set_const(inm, sym, i);
    jl_module_export(inm, sym);
}

void jl_init_intrinsic_properties(void) JL_GC_DISABLED
{
#define ADD_I(name, nargs) add_intrinsic_properties(name, nargs, (void(*)(void))&jl_##name);
#define ADD_HIDDEN ADD_I
#define ALIAS(alias, base) add_intrinsic_properties(alias, intrinsic_nargs[base], runtime_fp[base]);
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS
}

void jl_init_intrinsic_functions(void) JL_GC_DISABLED
{
    jl_module_t *inm = jl_new_module(jl_symbol("Intrinsics"));
    inm->parent = jl_core_module;
    jl_set_const(jl_core_module, jl_symbol("Intrinsics"), (jl_value_t*)inm);
    jl_mk_builtin_func(jl_intrinsic_type, "IntrinsicFunction", jl_f_intrinsic_call);
    jl_mk_builtin_func(
        (jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_opaque_closure_type),
        "OpaqueClosure", jl_f_opaque_closure_call);

#define ADD_I(name, nargs) add_intrinsic(inm, #name, name);
#define ADD_HIDDEN(name, nargs)
#define ALIAS ADD_I
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS
}

static void add_builtin(const char *name, jl_value_t *v)
{
    jl_set_const(jl_core_module, jl_symbol(name), v);
}

jl_fptr_args_t jl_get_builtin_fptr(jl_value_t *b)
{
    assert(jl_isa(b, (jl_value_t*)jl_builtin_type));
    jl_typemap_entry_t *entry = (jl_typemap_entry_t*)jl_atomic_load_relaxed(&jl_gf_mtable(b)->defs);
    jl_method_instance_t *mi = jl_atomic_load_relaxed(&entry->func.method->unspecialized);
    jl_code_instance_t *ci = jl_atomic_load_relaxed(&mi->cache);
    return jl_atomic_load_relaxed(&ci->specptr.fptr1);
}

static jl_value_t *add_builtin_func(const char *name, jl_fptr_args_t fptr)
{
    return jl_mk_builtin_func(NULL, name, fptr)->instance;
}

void jl_init_primitives(void) JL_GC_DISABLED
{
    jl_builtin_is = add_builtin_func("===", jl_f_is);
    jl_builtin_typeof = add_builtin_func("typeof", jl_f_typeof);
    jl_builtin_sizeof = add_builtin_func("sizeof", jl_f_sizeof);
    jl_builtin_issubtype = add_builtin_func("<:", jl_f_issubtype);
    jl_builtin_isa = add_builtin_func("isa", jl_f_isa);
    jl_builtin_typeassert = add_builtin_func("typeassert", jl_f_typeassert);
    jl_builtin_throw = add_builtin_func("throw", jl_f_throw);
    jl_builtin_tuple = add_builtin_func("tuple", jl_f_tuple);
    jl_builtin_ifelse = add_builtin_func("ifelse", jl_f_ifelse);

    // field access
    jl_builtin_getfield = add_builtin_func("getfield",  jl_f_getfield);
    jl_builtin_setfield = add_builtin_func("setfield!",  jl_f_setfield);
    jl_builtin_swapfield = add_builtin_func("swapfield!",  jl_f_swapfield);
    jl_builtin_modifyfield = add_builtin_func("modifyfield!",  jl_f_modifyfield);
    jl_builtin_replacefield = add_builtin_func("replacefield!",  jl_f_replacefield);
    jl_builtin_fieldtype = add_builtin_func("fieldtype", jl_f_fieldtype);
    jl_builtin_nfields = add_builtin_func("nfields", jl_f_nfields);
    jl_builtin_isdefined = add_builtin_func("isdefined", jl_f_isdefined);

    // module bindings
    jl_builtin_getglobal = add_builtin_func("getglobal", jl_f_getglobal);
    jl_builtin_setglobal = add_builtin_func("setglobal!", jl_f_setglobal);
    add_builtin_func("get_binding_type", jl_f_get_binding_type);
    add_builtin_func("set_binding_type!", jl_f_set_binding_type);

    // array primitives
    jl_builtin_arrayref = add_builtin_func("arrayref", jl_f_arrayref);
    jl_builtin_const_arrayref = add_builtin_func("const_arrayref", jl_f_arrayref);
    jl_builtin_arrayset = add_builtin_func("arrayset", jl_f_arrayset);
    jl_builtin_arraysize = add_builtin_func("arraysize", jl_f_arraysize);

    // method table utils
    jl_builtin_applicable = add_builtin_func("applicable", jl_f_applicable);
    jl_builtin_invoke = add_builtin_func("invoke", jl_f_invoke);
    jl_typename_t *itn = ((jl_datatype_t*)jl_typeof(jl_builtin_invoke))->name;
    jl_value_t *ikws = jl_new_generic_function_with_supertype(itn->name, jl_core_module, jl_builtin_type);
    itn->mt->kwsorter = ikws;
    jl_gc_wb(itn->mt, ikws);
    jl_mk_builtin_func((jl_datatype_t*)jl_typeof(ikws), jl_symbol_name(jl_gf_name(ikws)), jl_f_invoke_kwsorter);

    // internal functions
    jl_builtin_apply_type = add_builtin_func("apply_type", jl_f_apply_type);
    jl_builtin__apply_iterate = add_builtin_func("_apply_iterate", jl_f__apply_iterate);
    jl_builtin__expr = add_builtin_func("_expr", jl_f__expr);
    jl_builtin_svec = add_builtin_func("svec", jl_f_svec);
    add_builtin_func("_apply_pure", jl_f__apply_pure);
    add_builtin_func("_call_latest", jl_f__call_latest);
    add_builtin_func("_call_in_world", jl_f__call_in_world);
    add_builtin_func("_call_in_world_total", jl_f__call_in_world_total);
    add_builtin_func("_typevar", jl_f__typevar);
    add_builtin_func("_structtype", jl_f__structtype);
    add_builtin_func("_abstracttype", jl_f__abstracttype);
    add_builtin_func("_primitivetype", jl_f__primitivetype);
    add_builtin_func("_setsuper!", jl_f__setsuper);
    jl_builtin__typebody = add_builtin_func("_typebody!", jl_f__typebody);
    add_builtin_func("_equiv_typedef", jl_f__equiv_typedef);
    jl_builtin_donotdelete = add_builtin_func("donotdelete", jl_f_donotdelete);
    add_builtin_func("finalizer", jl_f_finalizer);

    // builtin types
    add_builtin("Any", (jl_value_t*)jl_any_type);
    add_builtin("Type", (jl_value_t*)jl_type_type);
    add_builtin("Nothing", (jl_value_t*)jl_nothing_type);
    add_builtin("nothing", (jl_value_t*)jl_nothing);
    add_builtin("TypeName", (jl_value_t*)jl_typename_type);
    add_builtin("DataType", (jl_value_t*)jl_datatype_type);
    add_builtin("TypeVar", (jl_value_t*)jl_tvar_type);
    add_builtin("UnionAll", (jl_value_t*)jl_unionall_type);
    add_builtin("Union", (jl_value_t*)jl_uniontype_type);
    add_builtin("TypeofBottom", (jl_value_t*)jl_typeofbottom_type);
    add_builtin("Tuple", (jl_value_t*)jl_anytuple_type);
    add_builtin("TypeofVararg", (jl_value_t*)jl_vararg_type);
    add_builtin("SimpleVector", (jl_value_t*)jl_simplevector_type);

    add_builtin("Module", (jl_value_t*)jl_module_type);
    add_builtin("MethodTable", (jl_value_t*)jl_methtable_type);
    add_builtin("Method", (jl_value_t*)jl_method_type);
    add_builtin("CodeInstance", (jl_value_t*)jl_code_instance_type);
    add_builtin("TypeMapEntry", (jl_value_t*)jl_typemap_entry_type);
    add_builtin("TypeMapLevel", (jl_value_t*)jl_typemap_level_type);
    add_builtin("Symbol", (jl_value_t*)jl_symbol_type);
    add_builtin("SSAValue", (jl_value_t*)jl_ssavalue_type);
    add_builtin("Slot", (jl_value_t*)jl_abstractslot_type);
    add_builtin("SlotNumber", (jl_value_t*)jl_slotnumber_type);
    add_builtin("TypedSlot", (jl_value_t*)jl_typedslot_type);
    add_builtin("Argument", (jl_value_t*)jl_argument_type);
    add_builtin("Const", (jl_value_t*)jl_const_type);
    add_builtin("PartialStruct", (jl_value_t*)jl_partial_struct_type);
    add_builtin("PartialOpaque", (jl_value_t*)jl_partial_opaque_type);
    add_builtin("MethodMatch", (jl_value_t*)jl_method_match_type);
    add_builtin("IntrinsicFunction", (jl_value_t*)jl_intrinsic_type);
    add_builtin("Function", (jl_value_t*)jl_function_type);
    add_builtin("Builtin", (jl_value_t*)jl_builtin_type);
    add_builtin("MethodInstance", (jl_value_t*)jl_method_instance_type);
    add_builtin("CodeInfo", (jl_value_t*)jl_code_info_type);
    add_builtin("Ref", (jl_value_t*)jl_ref_type);
    add_builtin("Ptr", (jl_value_t*)jl_pointer_type);
    add_builtin("LLVMPtr", (jl_value_t*)jl_llvmpointer_type);
    add_builtin("Task", (jl_value_t*)jl_task_type);
    add_builtin("OpaqueClosure", (jl_value_t*)jl_opaque_closure_type);

    add_builtin("AbstractArray", (jl_value_t*)jl_abstractarray_type);
    add_builtin("DenseArray", (jl_value_t*)jl_densearray_type);
    add_builtin("Array", (jl_value_t*)jl_array_type);

    add_builtin("Expr", (jl_value_t*)jl_expr_type);
    add_builtin("LineNumberNode", (jl_value_t*)jl_linenumbernode_type);
    add_builtin("LineInfoNode", (jl_value_t*)jl_lineinfonode_type);
    add_builtin("GotoNode", (jl_value_t*)jl_gotonode_type);
    add_builtin("GotoIfNot", (jl_value_t*)jl_gotoifnot_type);
    add_builtin("ReturnNode", (jl_value_t*)jl_returnnode_type);
    add_builtin("PiNode", (jl_value_t*)jl_pinode_type);
    add_builtin("PhiNode", (jl_value_t*)jl_phinode_type);
    add_builtin("PhiCNode", (jl_value_t*)jl_phicnode_type);
    add_builtin("UpsilonNode", (jl_value_t*)jl_upsilonnode_type);
    add_builtin("QuoteNode", (jl_value_t*)jl_quotenode_type);
    add_builtin("NewvarNode", (jl_value_t*)jl_newvarnode_type);
    add_builtin("GlobalRef", (jl_value_t*)jl_globalref_type);
    add_builtin("NamedTuple", (jl_value_t*)jl_namedtuple_type);

    add_builtin("Bool", (jl_value_t*)jl_bool_type);
    add_builtin("UInt8", (jl_value_t*)jl_uint8_type);
    add_builtin("UInt16", (jl_value_t*)jl_uint16_type);
    add_builtin("UInt32", (jl_value_t*)jl_uint32_type);
    add_builtin("UInt64", (jl_value_t*)jl_uint64_type);
    add_builtin("Int32", (jl_value_t*)jl_int32_type);
    add_builtin("Int64", (jl_value_t*)jl_int64_type);
#ifdef _P64
    add_builtin("Int", (jl_value_t*)jl_int64_type);
#else
    add_builtin("Int", (jl_value_t*)jl_int32_type);
#endif

    add_builtin("AbstractString", (jl_value_t*)jl_abstractstring_type);
    add_builtin("String", (jl_value_t*)jl_string_type);
}

#ifdef __cplusplus
}
#endif
