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

static int bits_equal(void *a, void *b, int sz)
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
static int NOINLINE compare_svec(jl_svec_t *a, jl_svec_t *b)
{
    size_t l = jl_svec_len(a);
    if (l != jl_svec_len(b))
        return 0;
    for(size_t i=0; i < l; i++) {
        if (!jl_egal(jl_svecref(a,i),jl_svecref(b,i)))
            return 0;
    }
    return 1;
}

// See comment above for an explanation of NOINLINE.
static int NOINLINE compare_fields(jl_value_t *a, jl_value_t *b, jl_datatype_t *dt)
{
    size_t f, nf = jl_datatype_nfields(dt);
    for (f = 0; f < nf; f++) {
        size_t offs = jl_field_offset(dt, f);
        char *ao = (char*)jl_data_ptr(a) + offs;
        char *bo = (char*)jl_data_ptr(b) + offs;
        if (jl_field_isptr(dt, f)) {
            jl_value_t *af = *(jl_value_t**)ao;
            jl_value_t *bf = *(jl_value_t**)bo;
            if (af != bf) {
                if (af == NULL || bf == NULL)
                    return 0;
                if (!jl_egal(af, bf))
                    return 0;
            }
        }
        else {
            jl_datatype_t *ft = (jl_datatype_t*)jl_field_type(dt, f);
            if (jl_is_uniontype(ft)) {
                uint8_t asel = ((uint8_t*)ao)[jl_field_size(dt, f) - 1];
                uint8_t bsel = ((uint8_t*)bo)[jl_field_size(dt, f) - 1];
                if (asel != bsel)
                    return 0;
                ft = (jl_datatype_t*)jl_nth_union_component((jl_value_t*)ft, asel);
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
    return 1;
}

static int egal_types(jl_value_t *a, jl_value_t *b, jl_typeenv_t *env)
{
    if (a == b)
        return 1;
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(a);
    if (dt != (jl_datatype_t*)jl_typeof(b))
        return 0;
    if (dt == jl_tvar_type) {
        jl_typeenv_t *pe = env;
        while (pe != NULL) {
            if (pe->var == (jl_tvar_t*)a)
                return pe->val == b;
            pe = pe->prev;
        }
        return 0;
    }
    if (dt == jl_uniontype_type) {
        return egal_types(((jl_uniontype_t*)a)->a, ((jl_uniontype_t*)b)->a, env) &&
            egal_types(((jl_uniontype_t*)a)->b, ((jl_uniontype_t*)b)->b, env);
    }
    if (dt == jl_unionall_type) {
        jl_unionall_t *ua = (jl_unionall_t*)a;
        jl_unionall_t *ub = (jl_unionall_t*)b;
        if (ua->var->name != ub->var->name)
            return 0;
        if (!(egal_types(ua->var->lb, ub->var->lb, env) && egal_types(ua->var->ub, ub->var->ub, env)))
            return 0;
        jl_typeenv_t e = { ua->var, (jl_value_t*)ub->var, env };
        return egal_types(ua->body, ub->body, &e);
    }
    if (dt == jl_datatype_type) {
        jl_datatype_t *dta = (jl_datatype_t*)a;
        jl_datatype_t *dtb = (jl_datatype_t*)b;
        if (dta->name != dtb->name)
            return 0;
        size_t i, l = jl_nparams(dta);
        if (jl_nparams(dtb) != l)
            return 0;
        for (i = 0; i < l; i++) {
            if (!egal_types(jl_tparam(dta, i), jl_tparam(dtb, i), env))
                return 0;
        }
        return 1;
    }
    return jl_egal(a, b);
}

JL_DLLEXPORT int jl_egal(jl_value_t *a, jl_value_t *b)
{
    // warning: a,b may NOT have been gc-rooted by the caller
    if (a == b)
        return 1;
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(a);
    if (dt != (jl_datatype_t*)jl_typeof(b))
        return 0;
    if (dt == jl_simplevector_type)
        return compare_svec((jl_svec_t*)a, (jl_svec_t*)b);
    if (dt == jl_datatype_type) {
        jl_datatype_t *dta = (jl_datatype_t*)a;
        jl_datatype_t *dtb = (jl_datatype_t*)b;
        return dta->name == dtb->name && compare_svec(dta->parameters, dtb->parameters);
    }
    if (dt == jl_string_type) {
        size_t l = jl_string_len(a);
        if (jl_string_len(b) != l)
            return 0;
        return !memcmp(jl_string_data(a), jl_string_data(b), l);
    }
    if (dt->mutabl)
        return 0;
    size_t sz = jl_datatype_size(dt);
    if (sz == 0)
        return 1;
    size_t nf = jl_datatype_nfields(dt);
    if (nf == 0)
        return bits_equal(jl_data_ptr(a), jl_data_ptr(b), sz);
    if (dt == jl_unionall_type)
        return egal_types(a, b, NULL);
    return compare_fields(a, b, dt);
}

// object_id ------------------------------------------------------------------

static uintptr_t bits_hash(const void *b, size_t sz)
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

static uintptr_t NOINLINE hash_svec(jl_svec_t *v)
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

typedef struct _varidx {
    jl_tvar_t *var;
    struct _varidx *prev;
} jl_varidx_t;

static uintptr_t jl_object_id_(jl_value_t *tv, jl_value_t *v);

static uintptr_t type_object_id_(jl_value_t *v, jl_varidx_t *env)
{
    if (v == NULL) return 0;
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
        uintptr_t h = ~((jl_datatype_t*)v)->name->hash;
        size_t i, l = jl_nparams(v);
        for (i = 0; i < l; i++) {
            h = bitmix(h, type_object_id_(jl_tparam(v, i), env));
        }
        return h;
    }
    return jl_object_id_((jl_value_t*)tv, v);
}

static uintptr_t jl_object_id_(jl_value_t *tv, jl_value_t *v)
{
    if (tv == (jl_value_t*)jl_sym_type)
        return ((jl_sym_t*)v)->hash;
    if (tv == (jl_value_t*)jl_simplevector_type)
        return hash_svec((jl_svec_t*)v);
    jl_datatype_t *dt = (jl_datatype_t*)tv;
    if (dt == jl_datatype_type) {
        jl_datatype_t *dtv = (jl_datatype_t*)v;
        // `name->wrapper` is cacheable even though it contains TypeVars
        // that don't have stable IDs.
        //if (jl_egal(dtv->name->wrapper, v))
        //    return bitmix(~dtv->name->hash, 0xaa5566aa);
        return bitmix(~dtv->name->hash, hash_svec(dtv->parameters));
    }
    if (dt == jl_typename_type)
        return ((jl_typename_t*)v)->hash;
    if (dt == jl_string_type) {
#ifdef _P64
        return memhash_seed(jl_string_data(v), jl_string_len(v), 0xedc3b677);
#else
        return memhash32_seed(jl_string_data(v), jl_string_len(v), 0xedc3b677);
#endif
    }
    if (dt->mutabl)
        return inthash((uintptr_t)v);
    size_t sz = jl_datatype_size(tv);
    uintptr_t h = jl_object_id(tv);
    if (sz == 0)
        return ~h;
    size_t f, nf = jl_datatype_nfields(dt);
    if (nf == 0)
        return bits_hash(jl_data_ptr(v), sz) ^ h;
    if (dt == jl_unionall_type)
        return type_object_id_(v, NULL);
    for (f = 0; f < nf; f++) {
        size_t offs = jl_field_offset(dt, f);
        char *vo = (char*)jl_data_ptr(v) + offs;
        uintptr_t u;
        if (jl_field_isptr(dt, f)) {
            jl_value_t *f = *(jl_value_t**)vo;
            u = (f == NULL) ? 0 : jl_object_id(f);
        }
        else {
            jl_datatype_t *fieldtype = (jl_datatype_t*)jl_field_type(dt, f);
            if (jl_is_uniontype(fieldtype)) {
                uint8_t sel = ((uint8_t*)vo)[jl_field_size(dt, f) - 1];
                fieldtype = (jl_datatype_t*)jl_nth_union_component((jl_value_t*)fieldtype, sel);
            }
            assert(jl_is_datatype(fieldtype) && !fieldtype->abstract && !fieldtype->mutabl);
            if (fieldtype->layout->haspadding)
                u = jl_object_id_((jl_value_t*)fieldtype, (jl_value_t*)vo);
            else
                u = bits_hash(vo, fieldtype->size);
        }
        h = bitmix(h, u);
    }
    return h;
}

JL_DLLEXPORT uintptr_t jl_object_id(jl_value_t *v)
{
    return jl_object_id_(jl_typeof(v), v);
}

// eq hash table --------------------------------------------------------------

#include "table.c"

// object model and type primitives -------------------------------------------

JL_CALLABLE(jl_f_is)
{
    JL_NARGS(===, 2, 2);
    if (args[0] == args[1])
        return jl_true;
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
        size_t elsize = 0, al = 0;
        int isinline = jl_islayout_inline(x, &elsize, &al);
        if (isinline)
            return jl_box_long(elsize);
        if (!jl_is_datatype(x))
            jl_error("argument is an abstract type; size is indeterminate");
    }
    if (jl_is_datatype(x)) {
        jl_datatype_t *dx = (jl_datatype_t*)x;
        if (dx->layout == NULL)
            jl_error("argument is an abstract type; size is indeterminate");
        if (jl_is_layout_opaque(dx->layout))
            jl_error("type does not have a fixed size");
        return jl_box_long(jl_datatype_size(x));
    }
    if (x == jl_bottom_type)
        jl_error("The empty type does not have a well-defined size since it does not have instances.");
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
    assert(!dt->abstract);
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

jl_function_t *jl_append_any_func;

JL_CALLABLE(jl_f__apply)
{
    JL_NARGSV(apply, 1);
    jl_function_t *f = args[0];
    if (nargs == 2) {
        if (f == jl_builtin_svec) {
            if (jl_is_svec(args[1]))
                return args[1];
            if (jl_is_array(args[1])) {
                size_t n = jl_array_len(args[1]);
                jl_svec_t *t = jl_alloc_svec(n);
                JL_GC_PUSH1(&t);
                for(size_t i=0; i < n; i++) {
                    jl_svecset(t, i, jl_arrayref((jl_array_t*)args[1], i));
                }
                JL_GC_POP();
                return (jl_value_t*)t;
            }
        }
    }
    size_t n=0, i, j;
    for(i=1; i < nargs; i++) {
        if (jl_is_svec(args[i])) {
            n += jl_svec_len(args[i]);
        }
        else if (jl_is_tuple(args[i])) {
            n += jl_nfields(args[i]);
        }
        else if (jl_is_array(args[i])) {
            n += jl_array_len(args[i]);
        }
        else {
            if (jl_append_any_func == NULL) {
                jl_append_any_func =
                    (jl_function_t*)jl_get_global(jl_top_module, jl_symbol("append_any"));
                if (jl_append_any_func == NULL) {
                    // error if append_any not available
                    JL_TYPECHK(apply, tuple, jl_typeof(args[i]));
                }
            }
            jl_array_t *argarr = NULL;
            JL_GC_PUSH2(&argarr, &f);
            args[0] = jl_append_any_func;
            argarr = (jl_array_t*)jl_apply(args, nargs);
            assert(jl_typeis(argarr, jl_array_any_type));
            jl_array_grow_beg(argarr, 1);
            jl_array_ptr_set(argarr, 0, f);
            args[0] = f;
            jl_value_t *result = jl_apply(jl_array_ptr_data(argarr), jl_array_len(argarr));
            JL_GC_POP();
            return result;
        }
    }
    jl_value_t **newargs;
    n++;
    int onstack = (n < jl_page_size/sizeof(jl_value_t*));
    JL_GC_PUSHARGS(newargs, onstack ? n : 1);
    jl_svec_t *arg_heap = NULL;
    if (!onstack) {
        // put arguments on the heap if there are too many
        arg_heap = jl_alloc_svec(n);
        newargs[0] = (jl_value_t*)arg_heap;
        newargs = jl_svec_data(arg_heap);
    }
    // GC Note: here we assume that the return value of `jl_svecref`,
    //          `jl_array_ptr_ref` will not be young if `arg_heap` becomes old
    //          since they are allocated before `arg_heap`. Otherwise,
    //          we need to add write barrier for !onstack
    newargs[0] = f;
    n = 1;
    for(i=1; i < nargs; i++) {
        jl_value_t *ai = args[i];
        if (jl_is_svec(ai)) {
            jl_svec_t *t = (jl_svec_t*)ai;
            size_t al = jl_svec_len(t);
            for(j=0; j < al; j++)
                newargs[n++] = jl_svecref(t, j);
        }
        else if (jl_is_tuple(ai)) {
            size_t al = jl_nfields(ai);
            for(j=0; j < al; j++) {
                // jl_fieldref may allocate.
                newargs[n++] = jl_fieldref(ai, j);
                if (arg_heap)
                    jl_gc_wb(arg_heap, newargs[n - 1]);
            }
        }
        else {
            assert(jl_is_array(ai));
            jl_array_t *aai = (jl_array_t*)ai;
            size_t al = jl_array_len(aai);
            if (aai->flags.ptrarray) {
                for (j = 0; j < al; j++) {
                    jl_value_t *arg = jl_array_ptr_ref(aai, j);
                    // apply with array splatting may have embedded NULL value
                    // #11772
                    if (__unlikely(arg == NULL))
                        jl_throw(jl_undefref_exception);
                    newargs[n++] = arg;
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
    }
    jl_value_t *result = jl_apply(newargs, n);
    JL_GC_POP();
    return result;
}

// this is like `_apply`, but with quasi-exact checks to make sure it is pure
JL_CALLABLE(jl_f__apply_pure)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    int last_in = ptls->in_pure_callback;
    jl_value_t *ret = NULL;
    JL_TRY {
        ptls->in_pure_callback = 1;
        // because this function was declared pure,
        // we should be allowed to run it in any world
        // so we run it in the newest world;
        // because, why not :)
        // and `promote` works better this way
        size_t last_age = ptls->world_age;
        ptls->world_age = jl_world_counter;
        ret = jl_f__apply(NULL, args, nargs);
        ptls->world_age = last_age;
        ptls->in_pure_callback = last_in;
    }
    JL_CATCH {
        ptls->in_pure_callback = last_in;
        jl_rethrow();
    }
    return ret;
}

// this is like `_apply`, but always runs in the newest world
JL_CALLABLE(jl_f__apply_latest)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    size_t last_age = ptls->world_age;
    if (!ptls->in_pure_callback)
        ptls->world_age = jl_world_counter;
    jl_value_t *ret = jl_f__apply(NULL, args, nargs);
    ptls->world_age = last_age;
    return ret;
}

// eval -----------------------------------------------------------------------

JL_DLLEXPORT jl_value_t *jl_toplevel_eval_in(jl_module_t *m, jl_value_t *ex)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls->in_pure_callback)
        jl_error("eval cannot be used in a generated function");
    jl_value_t *v = NULL;
    int last_lineno = jl_lineno;
    size_t last_age = ptls->world_age;
    jl_module_t *last_m = ptls->current_module;
    jl_module_t *task_last_m = ptls->current_task->current_module;
    if (jl_options.incremental && jl_generating_output()) {
        if (m != last_m) {
            jl_printf(JL_STDERR, "WARNING: eval from module %s to %s:    \n",
                      jl_symbol_name(m->name), jl_symbol_name(last_m->name));
            jl_static_show(JL_STDERR, ex);
            jl_printf(JL_STDERR, "\n  ** incremental compilation may be broken for this module **\n\n");
        }
    }
    JL_TRY {
        ptls->current_task->current_module = ptls->current_module = m;
        ptls->world_age = jl_world_counter;
        v = jl_toplevel_eval(m, ex);
    }
    JL_CATCH {
        jl_lineno = last_lineno;
        ptls->current_module = last_m;
        ptls->current_task->current_module = task_last_m;
        jl_rethrow();
    }
    jl_lineno = last_lineno;
    ptls->world_age = last_age;
    ptls->current_module = last_m;
    ptls->current_task->current_module = task_last_m;
    assert(v);
    return v;
}

JL_CALLABLE(jl_f_isdefined)
{
    jl_module_t *m = NULL;
    jl_sym_t *s = NULL;
    JL_NARGS(isdefined, 2, 2);
    if (!jl_is_module(args[0])) {
        jl_datatype_t *vt = (jl_datatype_t*)jl_typeof(args[0]);
        assert(jl_is_datatype(vt));
        size_t idx;
        if (jl_is_long(args[1])) {
            idx = jl_unbox_long(args[1])-1;
            if (idx >= jl_datatype_nfields(vt))
                return jl_false;
        }
        else {
            JL_TYPECHK(isdefined, symbol, args[1]);
            idx = jl_field_index(vt, (jl_sym_t*)args[1], 0);
            if ((int)idx == -1)
                return jl_false;
        }
        return jl_field_isdefined(args[0], idx) ? jl_true : jl_false;
    }
    JL_TYPECHK(isdefined, module, args[0]);
    JL_TYPECHK(isdefined, symbol, args[1]);
    m = (jl_module_t*)args[0];
    s = (jl_sym_t*)args[1];
    return jl_boundp(m, s) ? jl_true : jl_false;
}

// tuples ---------------------------------------------------------------------

JL_CALLABLE(jl_f_tuple)
{
    size_t i;
    if (nargs == 0) return (jl_value_t*)jl_emptytuple;
    jl_datatype_t *tt;
    if (nargs < jl_page_size/sizeof(jl_value_t*)) {
        jl_value_t **types = (jl_value_t**)alloca(nargs*sizeof(jl_value_t*));
        for(i=0; i < nargs; i++)
            types[i] = jl_typeof(args[i]);
        tt = jl_inst_concrete_tupletype_v(types, nargs);
    }
    else {
        jl_svec_t *types = jl_alloc_svec_uninit(nargs);
        JL_GC_PUSH1(&types);
        for(i=0; i < nargs; i++)
            jl_svecset(types, i, jl_typeof(args[i]));
        tt = jl_inst_concrete_tupletype(types);
        JL_GC_POP();
    }
    return jl_new_structv(tt, args, nargs);
}

JL_CALLABLE(jl_f_svec)
{
    size_t i;
    if (nargs == 0) return (jl_value_t*)jl_emptysvec;
    jl_svec_t *t = jl_alloc_svec_uninit(nargs);
    for(i=0; i < nargs; i++) {
        jl_svecset(t, i, args[i]);
    }
    return (jl_value_t*)t;
}

// struct operations ------------------------------------------------------------

JL_CALLABLE(jl_f_getfield)
{
    if (nargs == 3) {
        JL_TYPECHK(getfield, bool, args[2]);
        nargs -= 1;
    }
    JL_NARGS(getfield, 2, 2);
    jl_value_t *v = args[0];
    jl_value_t *vt = (jl_value_t*)jl_typeof(v);
    if (vt == (jl_value_t*)jl_module_type) {
        JL_TYPECHK(getfield, symbol, args[1]);
        return jl_eval_global_var((jl_module_t*)v, (jl_sym_t*)args[1]);
    }
    if (!jl_is_datatype(vt))
        jl_type_error("getfield", (jl_value_t*)jl_datatype_type, v);
    jl_datatype_t *st = (jl_datatype_t*)vt;
    size_t idx;
    if (jl_is_long(args[1])) {
        idx = jl_unbox_long(args[1])-1;
        if (idx >= jl_datatype_nfields(st))
            jl_bounds_error(args[0], args[1]);
    }
    else {
        JL_TYPECHK(getfield, symbol, args[1]);
        jl_sym_t *fld = (jl_sym_t*)args[1];
        idx = jl_field_index(st, fld, 1);
    }
    jl_value_t *fval = jl_get_nth_field(v, idx);
    if (fval == NULL)
        jl_throw(jl_undefref_exception);
    return fval;
}

JL_CALLABLE(jl_f_setfield)
{
    JL_NARGS(setfield!, 3, 3);
    jl_value_t *v = args[0];
    jl_value_t *vt = (jl_value_t*)jl_typeof(v);
    if (vt == (jl_value_t*)jl_module_type)
        jl_error("cannot assign variables in other modules");
    if (!jl_is_datatype(vt))
        jl_type_error("setfield!", (jl_value_t*)jl_datatype_type, v);
    jl_datatype_t *st = (jl_datatype_t*)vt;
    if (!st->mutabl)
        jl_errorf("type %s is immutable", jl_symbol_name(st->name->name));
    size_t idx;
    if (jl_is_long(args[1])) {
        idx = jl_unbox_long(args[1])-1;
        if (idx >= jl_datatype_nfields(st))
            jl_bounds_error(args[0], args[1]);
    }
    else {
        JL_TYPECHK(setfield!, symbol, args[1]);
        idx = jl_field_index(st, (jl_sym_t*)args[1], 1);
    }
    jl_value_t *ft = jl_field_type(st,idx);
    if (!jl_isa(args[2], ft)) {
        jl_type_error("setfield!", ft, args[2]);
    }
    jl_set_nth_field(v, idx, args[2]);
    return args[2];
}

static jl_value_t *get_fieldtype(jl_value_t *t, jl_value_t *f)
{
    if (jl_is_unionall(t)) {
        jl_value_t *u = t;
        JL_GC_PUSH1(&u);
        u = get_fieldtype(((jl_unionall_t*)t)->body, f);
        u = jl_type_unionall(((jl_unionall_t*)t)->var, u);
        JL_GC_POP();
        return u;
    }
    if (jl_is_uniontype(t)) {
        jl_value_t **u;
        jl_value_t *r;
        JL_GC_PUSHARGS(u, 2);
        u[0] = get_fieldtype(((jl_uniontype_t*)t)->a, f);
        u[1] = get_fieldtype(((jl_uniontype_t*)t)->b, f);
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
        if (st->name == jl_namedtuple_typename) {
            jl_value_t *nm = jl_tparam0(st);
            if (jl_is_tuple(nm)) {
                int nf = jl_nfields(nm);
                if (field_index < 0 || field_index >= nf)
                    jl_bounds_error(t, f);
            }
            jl_value_t *tt = jl_tparam1(st);
            while (jl_is_typevar(tt))
                tt = ((jl_tvar_t*)tt)->ub;
            if (tt == (jl_value_t*)jl_any_type)
                return (jl_value_t*)jl_any_type;
            return get_fieldtype(tt, f);
        }
        int nf = jl_field_count(st);
        if (nf > 0 && field_index >= nf-1 && st->name == jl_tuple_typename) {
            jl_value_t *ft = jl_field_type(st, nf-1);
            if (jl_is_vararg_type(ft))
                return jl_unwrap_vararg(ft);
        }
        if (field_index < 0 || field_index >= nf)
            jl_bounds_error(t, f);
    }
    else {
        JL_TYPECHK(fieldtype, symbol, f);
        field_index = jl_field_index(st, (jl_sym_t*)f, 1);
    }
    return jl_field_type(st, field_index);
}

JL_CALLABLE(jl_f_fieldtype)
{
    if (nargs == 3) {
        JL_TYPECHK(fieldtype, bool, args[2]);
        nargs -= 1;
    }
    JL_NARGS(fieldtype, 2, 2);
    jl_datatype_t *st = (jl_datatype_t*)args[0];
    if (st == jl_module_type)
        jl_error("cannot assign variables in other modules");
    return get_fieldtype(args[0], args[1]);
}

JL_CALLABLE(jl_f_nfields)
{
    JL_NARGS(nfields, 1, 1);
    jl_value_t *x = args[0];
    return jl_box_long(jl_field_count(jl_typeof(x)));
}

// apply_type -----------------------------------------------------------------

static int valid_type_param(jl_value_t *v)
{
    if (jl_is_tuple(v)) {
        // NOTE: tuples of symbols are not currently bits types, but have been
        // allowed as type parameters. this is a bit ugly.
        jl_value_t *tt = jl_typeof(v);
        size_t i, l = jl_nparams(tt);
        for(i=0; i < l; i++) {
            jl_value_t *pi = jl_tparam(tt,i);
            if (!(pi == (jl_value_t*)jl_sym_type || jl_isbits(pi)))
                return 0;
        }
        return 1;
    }
    if (jl_is_vararg_type(v))
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
            if (jl_is_vararg_type(pi)) {
                if (i != nargs-1)
                    jl_type_error_rt("Tuple", "non-final parameter", (jl_value_t*)jl_type_type, pi);
            }
            else if (!valid_type_param(pi)) {
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
    else if (jl_is_unionall(args[0])) {
        for(i=1; i < nargs; i++) {
            jl_value_t *pi = args[i];
            if (!valid_type_param(pi)) {
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
    size_t world = jl_get_ptls_states()->world_age;
    return jl_method_lookup(jl_gf_mtable(args[0]), args, nargs, 1, world) != NULL ?
        jl_true : jl_false;
}

JL_CALLABLE(jl_f_invoke)
{
    JL_NARGSV(invoke, 2);
    jl_value_t *argtypes = args[1];
    JL_GC_PUSH1(&argtypes);
    if (!jl_is_tuple_type(jl_unwrap_unionall(args[1])))
        jl_type_error_rt(jl_symbol_name(jl_gf_name(args[0])), "invoke", (jl_value_t*)jl_type_type, args[1]);
    if (!jl_tuple_isa(&args[2], nargs-2, (jl_datatype_t*)argtypes))
        jl_error("invoke: argument type error");
    args[1] = args[0];  // move function directly in front of arguments
    jl_value_t *res = jl_gf_invoke(argtypes, &args[1], nargs-1);
    JL_GC_POP();
    return res;
}

JL_DLLEXPORT jl_value_t *jl_get_keyword_sorter(jl_value_t *f);

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
            types[0] = (jl_value_t*)jl_namedtuple_type; types[1] = jl_typeof(func);
            for(i=2; i < nt; i++)
                types[i] = jl_tparam(argtypes,i-2);
            argtypes = (jl_value_t*)jl_apply_tuple_type_v(types, nt);
        }
        else {
            jl_svec_t *types = jl_alloc_svec_uninit(nt);
            JL_GC_PUSH1(&types);
            jl_svecset(types, 0, jl_array_any_type);
            jl_svecset(types, 1, jl_typeof(func));
            for(i=2; i < nt; i++)
                jl_svecset(types, i, jl_tparam(argtypes,i-2));
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
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_array_t *ar = jl_alloc_vec_any(n);
    JL_GC_PUSH1(&ar);
    jl_expr_t *ex = (jl_expr_t*)jl_gc_alloc(ptls, sizeof(jl_expr_t),
                                            jl_expr_type);
    ex->head = head;
    ex->args = ar;
    JL_GC_POP();
    return ex;
}

JL_CALLABLE(jl_f__expr)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_NARGSV(Expr, 1);
    JL_TYPECHK(Expr, symbol, args[0]);
    jl_array_t *ar = jl_alloc_vec_any(nargs-1);
    JL_GC_PUSH1(&ar);
    for(size_t i=0; i < nargs-1; i++)
        jl_array_ptr_set(ar, i, args[i+1]);
    jl_expr_t *ex = (jl_expr_t*)jl_gc_alloc(ptls, sizeof(jl_expr_t),
                                            jl_expr_type);
    ex->head = (jl_sym_t*)args[0];
    ex->args = ar;
    JL_GC_POP();
    return (jl_value_t*)ex;
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

// IntrinsicFunctions ---------------------------------------------------------

static void (*runtime_fp[num_intrinsics])(void);
static unsigned intrinsic_nargs[num_intrinsics];

JL_CALLABLE(jl_f_intrinsic_call)
{
    JL_NARGSV(intrinsic_call, 1);
    JL_TYPECHK(intrinsic_call, intrinsic, F);
    enum intrinsic f = (enum intrinsic)*(uint32_t*)jl_data_ptr(F);
    if (f == cglobal && nargs == 1)
        f = cglobal_auto;
    unsigned fargs = intrinsic_nargs[f];
    if (!fargs)
        jl_error("this intrinsic must be compiled to be called");
    JL_NARGS(intrinsic_call, fargs, fargs);

    union {
        void (*fptr)(void);
        jl_value_t *(*call1)(jl_value_t*);
        jl_value_t *(*call2)(jl_value_t*, jl_value_t*);
        jl_value_t *(*call3)(jl_value_t*, jl_value_t*, jl_value_t*);
        jl_value_t *(*call4)(jl_value_t*, jl_value_t*, jl_value_t*, jl_value_t*);
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
        default:
            assert(0 && "unexpected number of arguments to an intrinsic function");
    }
    gc_debug_critical_error();
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

static void add_intrinsic(jl_module_t *inm, const char *name, enum intrinsic f)
{
    jl_value_t *i = jl_permbox32(jl_intrinsic_type, (int32_t)f);
    jl_sym_t *sym = jl_symbol(name);
    jl_set_const(inm, sym, i);
    jl_module_export(inm, sym);
}

void jl_init_intrinsic_properties(void)
{
#define ADD_I(name, nargs) add_intrinsic_properties(name, nargs, (void(*)(void))&jl_##name);
#define ADD_HIDDEN ADD_I
#define ALIAS(alias, base) add_intrinsic_properties(alias, intrinsic_nargs[base], runtime_fp[base]);
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS
}

void jl_init_intrinsic_functions(void)
{
    jl_module_t *inm = jl_new_module(jl_symbol("Intrinsics"));
    inm->parent = jl_core_module;
    jl_set_const(jl_core_module, jl_symbol("Intrinsics"), (jl_value_t*)inm);
    jl_mk_builtin_func(jl_intrinsic_type, "IntrinsicFunction", jl_f_intrinsic_call);

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
    return jl_gf_mtable(b)->cache.leaf->func.linfo->specptr.fptr1;
}

static void add_builtin_func(const char *name, jl_fptr_args_t fptr)
{
    jl_mk_builtin_func(NULL, name, fptr);
}

void jl_init_primitives(void)
{
    add_builtin_func("===", jl_f_is);
    add_builtin_func("typeof", jl_f_typeof);
    add_builtin_func("sizeof", jl_f_sizeof);
    add_builtin_func("<:", jl_f_issubtype);
    add_builtin_func("isa", jl_f_isa);
    add_builtin_func("typeassert", jl_f_typeassert);
    add_builtin_func("throw", jl_f_throw);
    add_builtin_func("tuple", jl_f_tuple);
    add_builtin_func("ifelse", jl_f_ifelse);

    // field access
    add_builtin_func("getfield",  jl_f_getfield);
    add_builtin_func("setfield!",  jl_f_setfield);
    add_builtin_func("fieldtype", jl_f_fieldtype);
    add_builtin_func("nfields", jl_f_nfields);
    add_builtin_func("isdefined", jl_f_isdefined);

    // array primitives
    add_builtin_func("arrayref", jl_f_arrayref);
    add_builtin_func("arrayset", jl_f_arrayset);
    add_builtin_func("arraysize", jl_f_arraysize);

    // method table utils
    add_builtin_func("applicable", jl_f_applicable);
    add_builtin_func("invoke", jl_f_invoke);
    jl_value_t *invokef = jl_get_global(jl_core_module, jl_symbol("invoke"));
    jl_typename_t *itn = ((jl_datatype_t*)jl_typeof(invokef))->name;
    jl_value_t *ikws = jl_new_generic_function_with_supertype(itn->name, jl_core_module, jl_builtin_type, 1);
    itn->mt->kwsorter = ikws;
    jl_gc_wb(itn->mt, ikws);
    jl_mk_builtin_func((jl_datatype_t*)jl_typeof(ikws), jl_symbol_name(jl_gf_name(ikws)), jl_f_invoke_kwsorter);

    // internal functions
    add_builtin_func("apply_type", jl_f_apply_type);
    add_builtin_func("_apply", jl_f__apply);
    add_builtin_func("_apply_pure", jl_f__apply_pure);
    add_builtin_func("_apply_latest", jl_f__apply_latest);
    add_builtin_func("_expr", jl_f__expr);
    add_builtin_func("svec", jl_f_svec);

    // builtin types
    add_builtin("Any", (jl_value_t*)jl_any_type);
    add_builtin("Type", (jl_value_t*)jl_type_type);
    add_builtin("Nothing", (jl_value_t*)jl_void_type);
    add_builtin("nothing", (jl_value_t*)jl_nothing);
    add_builtin("TypeName", (jl_value_t*)jl_typename_type);
    add_builtin("DataType", (jl_value_t*)jl_datatype_type);
    add_builtin("TypeVar", (jl_value_t*)jl_tvar_type);
    add_builtin("UnionAll", (jl_value_t*)jl_unionall_type);
    add_builtin("Union", (jl_value_t*)jl_uniontype_type);
    add_builtin("TypeofBottom", (jl_value_t*)jl_typeofbottom_type);
    add_builtin("Tuple", (jl_value_t*)jl_anytuple_type);
    add_builtin("Vararg", (jl_value_t*)jl_vararg_type);
    add_builtin("SimpleVector", (jl_value_t*)jl_simplevector_type);

    add_builtin("Module", (jl_value_t*)jl_module_type);
    add_builtin("MethodTable", (jl_value_t*)jl_methtable_type);
    add_builtin("Method", (jl_value_t*)jl_method_type);
    add_builtin("TypeMapEntry", (jl_value_t*)jl_typemap_entry_type);
    add_builtin("TypeMapLevel", (jl_value_t*)jl_typemap_level_type);
    add_builtin("Symbol", (jl_value_t*)jl_sym_type);
    add_builtin("SSAValue", (jl_value_t*)jl_ssavalue_type);
    add_builtin("Slot", (jl_value_t*)jl_abstractslot_type);
    add_builtin("SlotNumber", (jl_value_t*)jl_slotnumber_type);
    add_builtin("TypedSlot", (jl_value_t*)jl_typedslot_type);
    add_builtin("IntrinsicFunction", (jl_value_t*)jl_intrinsic_type);
    add_builtin("Function", (jl_value_t*)jl_function_type);
    add_builtin("Builtin", (jl_value_t*)jl_builtin_type);
    add_builtin("MethodInstance", (jl_value_t*)jl_method_instance_type);
    add_builtin("CodeInfo", (jl_value_t*)jl_code_info_type);
    add_builtin("Ref", (jl_value_t*)jl_ref_type);
    add_builtin("Ptr", (jl_value_t*)jl_pointer_type);
    add_builtin("Task", (jl_value_t*)jl_task_type);

    add_builtin("AbstractArray", (jl_value_t*)jl_abstractarray_type);
    add_builtin("DenseArray", (jl_value_t*)jl_densearray_type);
    add_builtin("Array", (jl_value_t*)jl_array_type);

    add_builtin("Expr", (jl_value_t*)jl_expr_type);
    add_builtin("LineNumberNode", (jl_value_t*)jl_linenumbernode_type);
    add_builtin("LineInfoNode", (jl_value_t*)jl_lineinfonode_type);
    add_builtin("GotoNode", (jl_value_t*)jl_gotonode_type);
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
    add_builtin("Int32", (jl_value_t*)jl_int32_type);
    add_builtin("Int64", (jl_value_t*)jl_int64_type);
    add_builtin("UInt32", (jl_value_t*)jl_uint32_type);
    add_builtin("UInt64", (jl_value_t*)jl_uint64_type);
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
