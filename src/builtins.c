// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  implementations of some built-in functions and utilities
*/
#include "platform.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
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

#ifdef __cplusplus
extern "C" {
#endif

// exceptions -----------------------------------------------------------------

DLLEXPORT void NORETURN jl_error(const char *str)
{
    if (jl_errorexception_type == NULL) {
        jl_printf(JL_STDERR, "ERROR: %s\n", str);
        jl_exit(1);
    }
    jl_value_t *msg = jl_pchar_to_string((char*)str, strlen(str));
    JL_GC_PUSH1(&msg);
    jl_throw(jl_new_struct(jl_errorexception_type, msg));
}

extern int vasprintf(char **str, const char *fmt, va_list ap);

static void NORETURN jl_vexceptionf(jl_datatype_t *exception_type, const char *fmt, va_list args)
{
    if (exception_type == NULL) {
        jl_printf(JL_STDERR, "ERROR: ");
        jl_vprintf(JL_STDERR, fmt, args);
        jl_printf(JL_STDERR, "\n");
        jl_exit(1);
    }
    char *str = NULL;
    int ok = vasprintf(&str, fmt, args);
    jl_value_t *msg;
    if (ok < 0) {  // vasprintf failed
        msg = jl_cstr_to_string("internal error: could not display error message");
    }
    else {
        msg = jl_pchar_to_string(str, strlen(str));
        free(str);
    }
    JL_GC_PUSH1(&msg);
    jl_throw(jl_new_struct(exception_type, msg));
}

DLLEXPORT void NORETURN jl_errorf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    jl_vexceptionf(jl_errorexception_type, fmt, args);
    va_end(args);
}

DLLEXPORT void NORETURN jl_exceptionf(jl_datatype_t *exception_type, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    jl_vexceptionf(exception_type, fmt, args);
    va_end(args);
}

void NORETURN jl_too_few_args(const char *fname, int min)
{
    jl_exceptionf(jl_argumenterror_type, "%s: too few arguments (expected %d)", fname, min);
}

void NORETURN jl_too_many_args(const char *fname, int max)
{
    jl_exceptionf(jl_argumenterror_type, "%s: too many arguments (expected %d)", fname, max);
}

void NORETURN jl_type_error_rt(const char *fname, const char *context,
                               jl_value_t *ty, jl_value_t *got)
{
    jl_value_t *ctxt=NULL;
    JL_GC_PUSH2(&ctxt, &got);
    ctxt = jl_pchar_to_string((char*)context, strlen(context));
    jl_value_t *ex = jl_new_struct(jl_typeerror_type, jl_symbol(fname),
                                   ctxt, ty, got);
    jl_throw(ex);
}

void NORETURN jl_type_error_rt_line(const char *fname, const char *context,
                                    jl_value_t *ty, jl_value_t *got, int line)
{
    jl_type_error_rt(fname, context, ty, got);
}

void NORETURN jl_type_error(const char *fname, jl_value_t *expected, jl_value_t *got)
{
    jl_type_error_rt(fname, "", expected, got);
}

DLLEXPORT void NORETURN jl_undefined_var_error(jl_sym_t *var)
{
    if (var->name[0] == '#') {
        // convention for renamed variables: #...#original_name
        char *nxt = strchr(var->name+1, '#');
        if (nxt)
            var = jl_symbol(nxt+1);
    }
    jl_throw(jl_new_struct(jl_undefvarerror_type, var));
}

DLLEXPORT void NORETURN jl_bounds_error(jl_value_t *v, jl_value_t *t)
{
    JL_GC_PUSH2(&v, &t); // root arguments so the caller doesn't need to
    jl_throw(jl_new_struct((jl_datatype_t*)jl_boundserror_type, v, t));
}

DLLEXPORT void NORETURN jl_bounds_error_v(jl_value_t *v, jl_value_t **idxs, size_t nidxs)
{
    jl_value_t *t = NULL;
    // items in idxs are assumed to already be rooted
    JL_GC_PUSH2(&v, &t); // root v so the caller doesn't need to
    t = jl_f_tuple(NULL, idxs, nidxs);
    jl_throw(jl_new_struct((jl_datatype_t*)jl_boundserror_type, v, t));
}

DLLEXPORT void NORETURN jl_bounds_error_tuple_int(jl_value_t **v, size_t nv, size_t i)
{
    // values in v are expected to already be gc-rooted
    jl_bounds_error_int(jl_f_tuple(NULL, v, nv), i);
}

DLLEXPORT void NORETURN jl_bounds_error_unboxed_int(void *data, jl_value_t *vt, size_t i)
{
    jl_value_t *t = NULL, *v = NULL;
    // data is expected to be gc-safe (either gc-rooted, or alloca)
    // vt is expected to be gc-rooted (in a linfo-root probably)
    JL_GC_PUSH2(&v, &t);
    v = jl_new_bits(vt, data);
    t = jl_box_long(i);
    jl_throw(jl_new_struct((jl_datatype_t*)jl_boundserror_type, v, t));
}

DLLEXPORT void NORETURN jl_bounds_error_int(jl_value_t *v, size_t i)
{
    jl_value_t *t = NULL;
    JL_GC_PUSH2(&v, &t); // root arguments so the caller doesn't need to
    t = jl_box_long(i);
    jl_throw(jl_new_struct((jl_datatype_t*)jl_boundserror_type, v, t));
}

DLLEXPORT void NORETURN jl_bounds_error_ints(jl_value_t *v, size_t *idxs, size_t nidxs)
{
    size_t i;
    jl_value_t *t = NULL;
    JL_GC_PUSH2(&v, &t); // root arguments so the caller doesn't need to
    t = (jl_value_t*)jl_alloc_svec(nidxs);
    for (i = 0; i < nidxs; i++) {
        jl_svecset(t, i, jl_box_long(idxs[i]));
    }
    t = jl_f_tuple(NULL, jl_svec_data(t), nidxs);
    jl_throw(jl_new_struct((jl_datatype_t*)jl_boundserror_type, v, t));
}

JL_CALLABLE(jl_f_throw)
{
    JL_NARGS(throw, 1, 1);
    jl_throw(args[0]);
    return jl_nothing;
}

void jl_enter_handler(jl_handler_t *eh)
{
    JL_SIGATOMIC_BEGIN();
    eh->prev = jl_current_task->eh;
    eh->gcstack = jl_pgcstack;
    jl_current_task->eh = eh;
    // TODO: this should really go after setjmp(). see comment in
    // ctx_switch in task.c.
    JL_SIGATOMIC_END();
}

void jl_pop_handler(int n)
{
    while (n > 0) {
        jl_eh_restore_state(jl_current_task->eh);
        n--;
    }
}

// primitives -----------------------------------------------------------------

static int bits_equal(void *a, void *b, int sz)
{
    switch (sz) {
    case 1:  return *(int8_t*)a == *(int8_t*)b;
    case 2:  return *(int16_t*)a == *(int16_t*)b;
    case 4:  return *(int32_t*)a == *(int32_t*)b;
    case 8:  return *(int64_t*)a == *(int64_t*)b;
    default: return memcmp(a, b, sz)==0;
    }
}

// jl_egal
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
static int NOINLINE compare_svec(jl_value_t *a, jl_value_t *b)
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
    size_t nf = jl_datatype_nfields(dt);
    for (size_t f=0; f < nf; f++) {
        size_t offs = jl_field_offset(dt, f);
        char *ao = (char*)jl_data_ptr(a) + offs;
        char *bo = (char*)jl_data_ptr(b) + offs;
        int eq;
        if (jl_field_isptr(dt, f)) {
            jl_value_t *af = *(jl_value_t**)ao;
            jl_value_t *bf = *(jl_value_t**)bo;
            if (af == bf) eq = 1;
            else if (af==NULL || bf==NULL) eq = 0;
            else eq = jl_egal(af, bf);
        }
        else {
            jl_datatype_t *ft = (jl_datatype_t*)jl_field_type(dt, f);
            if (!ft->haspadding) {
                eq = bits_equal(ao, bo, jl_field_size(dt, f));
            }
            else {
                assert(jl_datatype_nfields(ft) > 0);
                eq = compare_fields((jl_value_t*)ao, (jl_value_t*)bo, ft);
            }
        }
        if (!eq) return 0;
    }
    return 1;
}

int jl_egal(jl_value_t *a, jl_value_t *b) // warning: a,b may NOT have been gc-rooted by the caller
{
    if (a == b)
        return 1;
    jl_value_t *ta = (jl_value_t*)jl_typeof(a);
    if (ta != (jl_value_t*)jl_typeof(b))
        return 0;
    if (jl_is_svec(a))
        return compare_svec(a, b);
    jl_datatype_t *dt = (jl_datatype_t*)ta;
    if (dt == jl_datatype_type) {
        jl_datatype_t *dta = (jl_datatype_t*)a;
        jl_datatype_t *dtb = (jl_datatype_t*)b;
        return dta->name == dtb->name &&
            jl_egal((jl_value_t*)dta->parameters, (jl_value_t*)dtb->parameters);
    }
    if (dt->mutabl) return 0;
    size_t sz = dt->size;
    if (sz == 0) return 1;
    size_t nf = jl_datatype_nfields(dt);
    if (nf == 0)
        return bits_equal(jl_data_ptr(a), jl_data_ptr(b), sz);
    return compare_fields(a, b, dt);
}

JL_CALLABLE(jl_f_is)
{
    JL_NARGS(is, 2, 2);
    if (args[0] == args[1])
        return jl_true;
    return jl_egal(args[0],args[1]) ? jl_true : jl_false;
}

JL_CALLABLE(jl_f_no_function)
{
    jl_error("invalid function object");
    return jl_nothing;
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
    if (jl_is_datatype(x)) {
        jl_datatype_t *dx = (jl_datatype_t*)x;
        if (dx->name == jl_array_typename || dx == jl_symbol_type || dx == jl_simplevector_type)
            jl_error("type does not have a canonical binary representation");
        if (!(dx->name->names == jl_emptysvec && dx->size > 0)) {
            // names===() and size > 0  =>  bitstype, size always known
            if (dx->abstract || !jl_is_leaf_type(x))
                jl_error("argument is an abstract type; size is indeterminate");
        }
        return jl_box_long(jl_datatype_size(x));
    }
    if (jl_is_array(x)) {
        return jl_box_long(jl_array_len(x) * ((jl_array_t*)x)->elsize);
    }
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(x);
    assert(jl_is_datatype(dt));
    assert(!dt->abstract);
    if (dt == jl_symbol_type)
        jl_error("value does not have a canonical binary representation");
    if (dt == jl_simplevector_type)
        return jl_box_long((1+jl_svec_len(x))*sizeof(void*));
    return jl_box_long(jl_datatype_size(dt));
}

JL_CALLABLE(jl_f_subtype)
{
    JL_NARGS(subtype, 2, 2);
    if (!jl_is_typevar(args[0]))
        JL_TYPECHK(subtype, type, args[0]);
    if (!jl_is_typevar(args[1]))
        JL_TYPECHK(subtype, type, args[1]);
    return (jl_subtype(args[0],args[1],0) ? jl_true : jl_false);
}

JL_CALLABLE(jl_f_isa)
{
    JL_NARGS(isa, 2, 2);
    JL_TYPECHK(isa, type, args[1]);
    return (jl_subtype(args[0],args[1],1) ? jl_true : jl_false);
}

DLLEXPORT void jl_typeassert(jl_value_t *x, jl_value_t *t)
{
    if (!jl_subtype(x,t,1))
        jl_type_error("typeassert", t, x);
}

JL_CALLABLE(jl_f_typeassert)
{
    JL_NARGS(typeassert, 2, 2);
    JL_TYPECHK(typeassert, type, args[1]);
    if (!jl_subtype(args[0],args[1],1))
        jl_type_error("typeassert", args[1], args[0]);
    return args[0];
}

static jl_function_t *jl_append_any_func;

JL_CALLABLE(jl_f_apply)
{
    JL_NARGSV(apply, 2);
    jl_function_t *f;
    jl_function_t *call_func = (jl_function_t*)args[0];
    assert(jl_is_function(call_func));
    if (jl_is_function(args[1])) {
        f = (jl_function_t*)args[1];
        --nargs; ++args; /* args[1] becomes args[0] */
    }
    else { /* do generic call(args...) instead */
        f = call_func;
        // protect "function" arg from splicing
        args[1] = (jl_value_t*)jl_svec1(args[1]);
    }
    if (nargs == 2) {
        if (f->fptr == &jl_f_svec) {
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
        if (jl_is_svec(args[1])) {
            return jl_apply(f, jl_svec_data(args[1]), jl_svec_len(args[1]));
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
        else if (jl_is_array(args[i]) && ((jl_array_t*)args[i])->ptrarray) {
            n += jl_array_len(args[i]);
        }
        else {
            if (jl_append_any_func == NULL) {
                jl_append_any_func =
                    (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("append_any"));
                if (jl_append_any_func == NULL) {
                    // error if append_any not available
                    JL_TYPECHK(apply, tuple, jl_typeof(args[i]));
                }
            }
            jl_value_t *argarr = jl_apply(jl_append_any_func, &args[1], nargs-1);
            assert(jl_typeis(argarr, jl_array_any_type));
            JL_GC_PUSH1(&argarr);
            jl_value_t *result = jl_apply(f, jl_cell_data(argarr), jl_array_len(argarr));
            JL_GC_POP();
            return result;
        }
    }
    jl_value_t **newargs;
    int onstack = (n < jl_page_size/sizeof(jl_value_t*));
    JL_GC_PUSHARGS(newargs, onstack ? n : 1);
    jl_svec_t *arg_heap = NULL;
    if (!onstack) {
        // put arguments on the heap if there are too many
        arg_heap = jl_alloc_svec(n);
        newargs[0] = (jl_value_t*)arg_heap;
        newargs = jl_svec_data(arg_heap);
    }
    // GC Note: here we assume that the the return value of `jl_svecref`,
    //          `jl_cellref` will not be young if `arg_heap` becomes old
    //          since they are allocated before `arg_heap`. Otherwise,
    //          we need to add write barrier for !onstack
    n = 0;
    for(i=1; i < nargs; i++) {
        if (jl_is_svec(args[i])) {
            jl_svec_t *t = (jl_svec_t*)args[i];
            size_t al = jl_svec_len(t);
            for(j=0; j < al; j++)
                newargs[n++] = jl_svecref(t, j);
        }
        else if (jl_is_tuple(args[i])) {
            size_t al = jl_nfields(args[i]);
            for(j=0; j < al; j++) {
                // jl_fieldref may allocate.
                newargs[n++] = jl_fieldref(args[i], j);
                if (arg_heap) {
                    jl_gc_wb(arg_heap, newargs[n - 1]);
                }
            }
        }
        else {
            size_t al = jl_array_len(args[i]);
            for (j = 0;j < al;j++) {
                jl_value_t *arg = jl_cellref(args[i], j);
                // apply with array splatting may have embedded NULL value
                // #11772
                if (__unlikely(arg == NULL)) {
                    jl_throw(jl_undefref_exception);
                }
                newargs[n++] = arg;
            }
        }
    }
    jl_value_t *result = jl_apply(f, newargs, n);
    JL_GC_POP();
    return result;
}

JL_CALLABLE(jl_f_kwcall)
{
    if (nargs < 4)
        jl_error("internal error: malformed keyword argument call");
    jl_function_t *f;
    jl_function_t *call_func = (jl_function_t*)args[0];
    assert(jl_is_function(call_func));
    size_t nkeys = jl_unbox_long(args[1]);
    size_t pa = 4 + 2*nkeys;
    jl_array_t *container = (jl_array_t*)args[pa-1];
    assert(jl_array_len(container) > 0);
    f = (jl_function_t*)args[pa-2];
    if (!jl_is_function(f)) {
        // do generic call(args...; kws...) instead
        // switch (f container pa...) to (container f pa...)
        args[pa-2] = args[pa-1];     // TODO: this might not be safe
        args[pa-1] = (jl_value_t*)f;
        f = call_func;
        pa--;
    }

    if (!jl_is_gf(f))
        jl_exceptionf(jl_argumenterror_type, "function does not accept keyword arguments");
    jl_function_t *sorter = ((jl_methtable_t*)f->env)->kwsorter;
    if (sorter == NULL) {
        jl_exceptionf(jl_argumenterror_type, "function %s does not accept keyword arguments",
                      jl_gf_name(f)->name);
    }

    for(size_t i=0; i < nkeys*2; i+=2) {
        jl_cellset(container, i  , args[2+i]);
        jl_cellset(container, i+1, args[2+i+1]);
    }

    args += pa-1;
    nargs -= pa-1;
    assert(jl_is_gf(sorter));
    jl_function_t *m = jl_method_lookup((jl_methtable_t*)sorter->env, args, nargs, 1);
    if (m == jl_bottom_func) {
        jl_no_method_error(f, args+1, nargs-1);
        // unreachable
    }

    return jl_apply(m, args, nargs);
}

// eval -----------------------------------------------------------------------

extern int jl_lineno;

DLLEXPORT jl_value_t *jl_toplevel_eval_in(jl_module_t *m, jl_value_t *ex, int delay_warn)
{
    static int jl_warn_on_eval = 0;
    int last_delay_warn = jl_warn_on_eval;
    if (m == NULL)
        m = jl_main_module;
    if (jl_is_symbol(ex))
        return jl_eval_global_var(m, (jl_sym_t*)ex);
    jl_value_t *v=NULL;
    int last_lineno = jl_lineno;
    jl_module_t *last_m = jl_current_module;
    jl_module_t *task_last_m = jl_current_task->current_module;
    if (!delay_warn && jl_options.incremental && jl_generating_output()) {
        if (m != last_m) {
            jl_printf(JL_STDERR, "WARNING: eval from module %s to %s:    \n", m->name->name, last_m->name->name);
            jl_static_show(JL_STDERR, ex);
            jl_printf(JL_STDERR, "\n  ** incremental compilation may be broken for this module **\n\n");
        }
        else if (jl_warn_on_eval) {
            jl_printf(JL_STDERR, "WARNING: eval from staged function in module %s:    \n", m->name->name);
            jl_static_show(JL_STDERR, ex);
            jl_printf(JL_STDERR, "\n  ** incremental compilation may be broken for these modules **\n\n");
        }
    }
    JL_TRY {
        jl_warn_on_eval = delay_warn && (jl_warn_on_eval || m != last_m); // compute whether a warning was suppressed
        jl_current_task->current_module = jl_current_module = m;
        v = jl_toplevel_eval(ex);
    }
    JL_CATCH {
        jl_warn_on_eval = last_delay_warn;
        jl_lineno = last_lineno;
        jl_current_module = last_m;
        jl_current_task->current_module = task_last_m;
        jl_rethrow();
    }
    jl_warn_on_eval = last_delay_warn;
    jl_lineno = last_lineno;
    jl_current_module = last_m;
    jl_current_task->current_module = task_last_m;
    assert(v);
    return v;
}

JL_CALLABLE(jl_f_top_eval)
{
    jl_module_t *m;
    jl_value_t *ex;
    if (nargs == 1) {
        m = jl_main_module;
        ex = args[0];
    }
    else {
        JL_NARGS(eval, 2, 2);
        JL_TYPECHK(eval, module, args[0]);
        m = (jl_module_t*)args[0];
        ex = args[1];
    }
    return jl_toplevel_eval_in(m, ex, 0);
}

JL_CALLABLE(jl_f_isdefined)
{
    jl_module_t *m = jl_current_module;
    jl_sym_t *s=NULL;
    JL_NARGSV(isdefined, 1);
    if (jl_is_array(args[0])) {
        return jl_array_isdefined(args, nargs) ? jl_true : jl_false;
    }
    if (nargs == 1) {
        JL_TYPECHK(isdefined, symbol, args[0]);
        s = (jl_sym_t*)args[0];
    }
    if (nargs != 2) {
        JL_NARGS(isdefined, 1, 1);
    }
    else {
        if (!jl_is_module(args[0])) {
            jl_datatype_t *vt = (jl_datatype_t*)jl_typeof(args[0]);
            if (!jl_is_datatype(vt)) {
                jl_type_error("isdefined", (jl_value_t*)jl_datatype_type, args[0]);
            }
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
    }
    assert(s);
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

// composite types ------------------------------------------------------------

JL_CALLABLE(jl_f_get_field)
{
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

JL_CALLABLE(jl_f_set_field)
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
        jl_errorf("type %s is immutable", st->name->name->name);
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
    if (!jl_subtype(args[2], ft, 1)) {
        jl_type_error("setfield!", ft, args[2]);
    }
    jl_set_nth_field(v, idx, args[2]);
    return args[2];
}

JL_CALLABLE(jl_f_field_type)
{
    JL_NARGS(fieldtype, 2, 2);
    jl_datatype_t *st = (jl_datatype_t*)args[0];
    if (st == jl_module_type)
        jl_error("cannot assign variables in other modules");
    if (!jl_is_datatype(st))
        jl_type_error("fieldtype", (jl_value_t*)jl_datatype_type, (jl_value_t*)st);
    int field_index;
    if (jl_is_long(args[1])) {
        field_index = jl_unbox_long(args[1]) - 1;
        if (field_index < 0 || field_index >= jl_datatype_nfields(st))
            jl_bounds_error(args[0], args[1]);
    }
    else {
        JL_TYPECHK(fieldtype, symbol, args[1]);
        field_index = jl_field_index(st, (jl_sym_t*)args[1], 1);
    }
    return jl_field_type(st, field_index);
}

JL_CALLABLE(jl_f_nfields)
{
    JL_NARGS(nfields, 1, 1);
    jl_value_t *x = args[0];
    if (!jl_is_datatype(x))
        x = jl_typeof(x);
    return jl_box_long(jl_datatype_nfields(x));
}

// conversion -----------------------------------------------------------------

DLLEXPORT void *jl_symbol_name(jl_sym_t *s)
{
    return s->name;
}

//WARNING: THIS FUNCTION IS NEVER CALLED BUT INLINE BY CCALL
DLLEXPORT void *jl_array_ptr(jl_array_t *a)
{
    return a->data;
}
DLLEXPORT jl_value_t *jl_value_ptr(jl_value_t *a)
{
    return a;
}

// printing -------------------------------------------------------------------

int substr_isspace(char *p, char *pend)
{
    while (p != pend) {
        if (!isspace((unsigned char)*p)) {
            return 0;
        }
        p++;
    }
    return 1;
}

int str_isspace(char *p)
{
    while (*p != '\0') {
        if (!isspace((unsigned char)*p)) {
            return 0;
        }
        p++;
    }
    return 1;
}

DLLEXPORT jl_nullable_float64_t jl_try_substrtod(char *str, size_t offset, size_t len)
{
    char *p;
    char *bstr = str+offset;
    char *pend = bstr+len;
    int err = 0;

    errno = 0;
    if (!(*pend == '\0' || isspace((unsigned char)*pend) || *pend == ',')) {
        // confusing data outside substring. must copy.
        char *newstr = (char*)malloc(len+1);
        memcpy(newstr, bstr, len);
        newstr[len] = 0;
        bstr = newstr;
        pend = bstr+len;
    }
    double out = strtod_c(bstr, &p);

    if (errno==ERANGE && (out==0 || out==HUGE_VAL || out==-HUGE_VAL)) {
        err = 1;
    }
    else if (p == bstr) {
        err = 1;
    }
    else {
        // Deal with case where the substring might be something like "1 ",
        // which is OK, and "1 X", which we don't allow.
        err = substr_isspace(p, pend) ? 0 : 1;
    }

    if (bstr != str+offset)
        free(bstr);

    jl_nullable_float64_t ret = {(uint8_t)err, out};
    return ret;
}

DLLEXPORT int jl_substrtod(char *str, size_t offset, size_t len, double *out)
{
    jl_nullable_float64_t nd = jl_try_substrtod(str, offset, len);
    if (0 == nd.isnull) {
        *out = nd.value;
        return 0;
    }
    return 1;
}

// MSVC pre-2013 did not define HUGE_VALF
#ifndef HUGE_VALF
#define HUGE_VALF (1e25f * 1e25f)
#endif

DLLEXPORT jl_nullable_float32_t jl_try_substrtof(char *str, size_t offset, size_t len)
{
    char *p;
    char *bstr = str+offset;
    char *pend = bstr+len;
    int err = 0;

    errno = 0;
    if (!(*pend == '\0' || isspace((unsigned char)*pend) || *pend == ',')) {
        // confusing data outside substring. must copy.
        char *newstr = (char*)malloc(len+1);
        memcpy(newstr, bstr, len);
        newstr[len] = 0;
        bstr = newstr;
        pend = bstr+len;
    }
#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
    float out = (float)strtod_c(bstr, &p);
#else
    float out = strtof_c(bstr, &p);
#endif

    if (errno==ERANGE && (out==0 || out==HUGE_VALF || out==-HUGE_VALF)) {
        err = 1;
    }
    else if (p == bstr) {
        err = 1;
    }
    else {
        // Deal with case where the substring might be something like "1 ",
        // which is OK, and "1 X", which we don't allow.
        err = substr_isspace(p, pend) ? 0 : 1;
    }

    if (bstr != str+offset)
        free(bstr);

    jl_nullable_float32_t ret = {(uint8_t)err, out};
    return ret;
}

DLLEXPORT int jl_substrtof(char *str, int offset, size_t len, float *out)
{
    jl_nullable_float32_t nf = jl_try_substrtof(str, offset, len);
    if (0 == nf.isnull) {
        *out = nf.value;
        return 0;
    }
    return 1;
}

// showing --------------------------------------------------------------------

void jl_flush_cstdio(void)
{
    fflush(stdout);
    fflush(stderr);
}

jl_value_t *jl_stdout_obj(void)
{
    if (jl_base_module == NULL) return NULL;
    jl_value_t *stdout_obj = jl_get_global(jl_base_module, jl_symbol("STDOUT"));
    if (stdout_obj != NULL) return stdout_obj;
    return jl_get_global(jl_base_module, jl_symbol("OUTPUT_STREAM"));
}

jl_value_t *jl_stderr_obj(void)
{
    if (jl_base_module == NULL) return NULL;
    jl_value_t *stderr_obj = jl_get_global(jl_base_module, jl_symbol("STDERR"));
    if (stderr_obj != NULL) return stderr_obj;
    return jl_get_global(jl_base_module, jl_symbol("OUTPUT_STREAM"));
}

static jl_function_t *jl_show_gf=NULL;

void jl_show(jl_value_t *stream, jl_value_t *v)
{
    if (jl_base_module) {
        if (jl_show_gf == NULL) {
            jl_show_gf = (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("show"));
        }
        if (jl_show_gf==NULL || stream==NULL) {
            jl_printf(JL_STDERR, " could not show value of type %s",
                      ((jl_datatype_t*)jl_typeof(v))->name->name->name);
            return;
        }
        jl_value_t *args[2] = {stream,v};
        jl_apply(jl_show_gf, args, 2);
    }
}

// internal functions ---------------------------------------------------------

extern int jl_in_inference;
extern int jl_boot_file_loaded;
int jl_eval_with_compiler_p(jl_expr_t *ast, jl_expr_t *expr, int compileloops, jl_module_t *m);

static int jl_eval_inner_with_compiler(jl_expr_t *e, jl_module_t *m)
{
    int i;
    for(i=0; i < jl_array_len(e->args); i++) {
        jl_value_t *ei = jl_exprarg(e,i);
        if (jl_is_lambda_info(ei)) {
            jl_lambda_info_t *li = (jl_lambda_info_t*)ei;
            if (!jl_is_expr(li->ast)) {
                li->ast = jl_uncompress_ast(li, li->ast);
                jl_gc_wb(li, li->ast);
            }
            jl_expr_t *a = (jl_expr_t*)li->ast;
            if (jl_lam_capt(a)->length > 0 && jl_eval_with_compiler_p(a, jl_lam_body(a), 1, m))
                return 1;
        }
        if (jl_is_expr(ei) && jl_eval_inner_with_compiler((jl_expr_t*)ei, m))
            return 1;
    }
    return 0;
}

void jl_trampoline_compile_function(jl_function_t *f, int always_infer, jl_tupletype_t *sig)
{
    assert(sig);
    assert(f->linfo != NULL);
    // to run inference on all thunks. slows down loading files.
    // NOTE: if this call to inference is removed, type_annotate in inference.jl
    // needs to be updated to infer inner functions.
    if (f->linfo->inferred == 0) {
        if (!jl_in_inference) {
            if (!jl_is_expr(f->linfo->ast)) {
                f->linfo->ast = jl_uncompress_ast(f->linfo, f->linfo->ast);
                jl_gc_wb(f->linfo, f->linfo->ast);
            }
            assert(jl_is_expr(f->linfo->ast));
            if (always_infer ||
                jl_eval_with_compiler_p((jl_expr_t*)f->linfo->ast, jl_lam_body((jl_expr_t*)f->linfo->ast), 1, f->linfo->module) ||
                // if this function doesn't need to be compiled, but contains inner
                // functions that do and that capture variables, we need to run
                // inference on the whole thing to propagate types into the inner
                // functions. caused issue #12794
                jl_eval_inner_with_compiler(jl_lam_body((jl_expr_t*)f->linfo->ast), f->linfo->module)) {
                jl_type_infer(f->linfo, sig, f->linfo);
            }
        }
    }
    jl_compile(f);
    // this assertion is probably not correct; the fptr could have been assigned
    // by a recursive invocation from inference above.
    //assert(f->fptr == &jl_trampoline);
    jl_generate_fptr(f);
    if (jl_boot_file_loaded && jl_is_expr(f->linfo->ast)) {
        f->linfo->ast = jl_compress_ast(f->linfo, f->linfo->ast);
        jl_gc_wb(f->linfo, f->linfo->ast);
    }
}

JL_CALLABLE(jl_trampoline)
{
    assert(jl_is_func(F));
    jl_function_t *f = (jl_function_t*)F;
    jl_trampoline_compile_function(f, 0, f->linfo->specTypes ? f->linfo->specTypes : jl_anytuple_type);
    return jl_apply(f, args, nargs);
}

JL_CALLABLE(jl_f_instantiate_type)
{
    JL_NARGSV(instantiate_type, 1);
    if (!jl_is_datatype(args[0]) && !jl_is_typector(args[0])) {
        jl_type_error("Type{...} expression", (jl_value_t*)jl_type_type, args[0]);
    }
    return jl_apply_type_(args[0], &args[1], nargs-1);
}

DLLEXPORT jl_value_t *jl_new_type_constructor(jl_svec_t *p, jl_value_t *t)
{
    jl_value_t *tc = (jl_value_t*)jl_new_type_ctor(p, t);
    int i;
    for(i=0; i < jl_svec_len(p); i++)
        ((jl_tvar_t*)jl_svecref(p,i))->bound = 0;
    return tc;
}

// generic function reflection ------------------------------------------------

static void jl_check_type_tuple(jl_value_t *t, jl_sym_t *name, const char *ctx)
{
    if (!jl_is_tuple_type(t))
        jl_type_error_rt(name->name, ctx, (jl_value_t*)jl_type_type, t);
}

JL_CALLABLE(jl_f_methodexists)
{
    JL_NARGS(method_exists, 2, 2);
    JL_TYPECHK(method_exists, function, args[0]);
    if (!jl_is_gf(args[0]))
        jl_error("method_exists: not a generic function");
    jl_value_t *argtypes = args[1];
    JL_GC_PUSH1(&argtypes);
    if (jl_is_tuple(args[1])) {
        // TODO: maybe deprecation warning, better checking
        argtypes = (jl_value_t*)jl_apply_tuple_type_v((jl_value_t**)jl_data_ptr(argtypes),
                                                      jl_nfields(argtypes));
    }
    else {
        jl_check_type_tuple(args[1], jl_gf_name(args[0]), "method_exists");
    }
    jl_value_t *res = jl_method_lookup_by_type(jl_gf_mtable(args[0]),
                                               (jl_tupletype_t*)argtypes,0,0)!=jl_bottom_func ?
        jl_true : jl_false;
    JL_GC_POP();
    return res;
}

JL_CALLABLE(jl_f_applicable)
{
    JL_NARGSV(applicable, 1);
    JL_TYPECHK(applicable, function, args[0]);
    if (!jl_is_gf(args[0]))
        jl_error("applicable: not a generic function");
    return jl_method_lookup(jl_gf_mtable(args[0]),
                            &args[1], nargs-1, 1) != jl_bottom_func ?
        jl_true : jl_false;
}

JL_CALLABLE(jl_f_invoke)
{
    JL_NARGSV(invoke, 2);
    JL_TYPECHK(invoke, function, args[0]);
    if (!jl_is_gf(args[0]))
        jl_error("invoke: not a generic function");
    jl_value_t *argtypes = args[1];
    JL_GC_PUSH1(&argtypes);
    if (jl_is_tuple(args[1])) {
        // TODO: maybe deprecation warning, better checking
        argtypes = (jl_value_t*)jl_apply_tuple_type_v((jl_value_t**)jl_data_ptr(argtypes),
                                                      jl_nfields(argtypes));
    }
    else {
        jl_check_type_tuple(args[1], jl_gf_name(args[0]), "invoke");
    }
    if (!jl_tuple_subtype(&args[2], nargs-2, (jl_datatype_t*)argtypes, 1))
        jl_error("invoke: argument type error");
    jl_value_t *res = jl_gf_invoke((jl_function_t*)args[0],
                                   (jl_tupletype_t*)argtypes, &args[2], nargs-2);
    JL_GC_POP();
    return res;
}

// eq hash table --------------------------------------------------------------

#include "table.c"

// hashing --------------------------------------------------------------------

#ifdef _P64
#define bitmix(a,b) int64hash((a)^bswap_64(b))
#define hash64(a)   int64hash(a)
#else
#define bitmix(a,b) int64to32hash((((uint64_t)a)<<32)|((uint64_t)b))
#define hash64(a)   int64to32hash(a)
#endif

static uptrint_t bits_hash(void *b, size_t sz)
{
    switch (sz) {
    case 1:  return int32hash(*(int8_t*)b);
    case 2:  return int32hash(*(int16_t*)b);
    case 4:  return int32hash(*(int32_t*)b);
    case 8:  return hash64(*(int64_t*)b);
    default:
#ifdef _P64
        return memhash((char*)b, sz);
#else
        return memhash32((char*)b, sz);
#endif
    }
}

static uptrint_t jl_object_id_(jl_value_t *tv, jl_value_t *v)
{
    if (tv == (jl_value_t*)jl_sym_type)
        return ((jl_sym_t*)v)->hash;
    if (tv == (jl_value_t*)jl_simplevector_type) {
        uptrint_t h = 0;
        size_t l = jl_svec_len(v);
        for(size_t i = 0; i < l; i++) {
            uptrint_t u = jl_object_id(jl_svecref(v,i));
            h = bitmix(h, u);
        }
        return h;
    }
    jl_datatype_t *dt = (jl_datatype_t*)tv;
    if (dt == jl_datatype_type) {
        jl_datatype_t *dtv = (jl_datatype_t*)v;
        uptrint_t h = 0xda1ada1a;
        // has_typevars always returns 0 on name->primary, so that type
        // can exist in the cache. however, interpreter.c mutates its
        // typevars' `bound` fields to 0, corrupting the cache. this is
        // avoided simply by hashing name->primary specially here.
        if (jl_egal(dtv->name->primary, v))
            return bitmix(bitmix(h, dtv->name->uid), 0xaa5566aa);
        return bitmix(bitmix(h, dtv->name->uid),
                      jl_object_id((jl_value_t*)dtv->parameters));
    }
    if (dt == jl_typename_type)
        return bitmix(((jl_typename_t*)v)->uid, 0xa1ada1ad);
    if (dt->mutabl) return inthash((uptrint_t)v);
    size_t sz = jl_datatype_size(tv);
    uptrint_t h = jl_object_id(tv);
    if (sz == 0) return ~h;
    size_t nf = jl_datatype_nfields(dt);
    if (nf == 0) {
        return bits_hash(jl_data_ptr(v), sz) ^ h;
    }
    for (size_t f=0; f < nf; f++) {
        size_t offs = jl_field_offset(dt, f);
        char *vo = (char*)jl_data_ptr(v) + offs;
        uptrint_t u;
        if (jl_field_isptr(dt, f)) {
            jl_value_t *f = *(jl_value_t**)vo;
            u = f==NULL ? 0 : jl_object_id(f);
        }
        else {
            jl_datatype_t *fieldtype = (jl_datatype_t*)jl_field_type(dt, f);
            assert(jl_is_datatype(fieldtype) && !fieldtype->abstract && !fieldtype->mutabl);
            if (fieldtype->haspadding)
                u = jl_object_id_((jl_value_t*)fieldtype, (jl_value_t*)vo);
            else
                u = bits_hash(vo, jl_field_size(dt, f));
        }
        h = bitmix(h, u);
    }
    return h;
}

DLLEXPORT uptrint_t jl_object_id(jl_value_t *v)
{
    return jl_object_id_(jl_typeof(v), v);
}

// init -----------------------------------------------------------------------

static void add_builtin(const char *name, jl_value_t *v)
{
    jl_set_const(jl_core_module, jl_symbol(name), v);
}

static void add_builtin_func(const char *name, jl_fptr_t f)
{
    add_builtin(name, (jl_value_t*)
                jl_new_closure(f, (jl_value_t*)jl_symbol(name), NULL));
}

void jl_init_primitives(void)
{
    add_builtin_func("is", jl_f_is);
    add_builtin_func("typeof", jl_f_typeof);
    add_builtin_func("sizeof", jl_f_sizeof);
    add_builtin_func("issubtype", jl_f_subtype);
    add_builtin_func("isa", jl_f_isa);
    add_builtin_func("typeassert", jl_f_typeassert);
    add_builtin_func("_apply", jl_f_apply);
    add_builtin_func("kwcall", jl_f_kwcall);
    add_builtin_func("throw", jl_f_throw);
    add_builtin_func("tuple", jl_f_tuple);
    add_builtin_func("svec", jl_f_svec);
    add_builtin_func("method_exists", jl_f_methodexists);
    add_builtin_func("applicable", jl_f_applicable);
    add_builtin_func("invoke", jl_f_invoke);
    add_builtin_func("eval", jl_f_top_eval);
    add_builtin_func("isdefined", jl_f_isdefined);

    // functions for internal use
    add_builtin_func("getfield",  jl_f_get_field);
    add_builtin_func("setfield!",  jl_f_set_field);
    add_builtin_func("fieldtype", jl_f_field_type);
    add_builtin_func("nfields", jl_f_nfields);
    add_builtin_func("_expr", jl_f_new_expr);

    add_builtin_func("arraylen", jl_f_arraylen);
    add_builtin_func("arrayref", jl_f_arrayref);
    add_builtin_func("arrayset", jl_f_arrayset);
    add_builtin_func("arraysize", jl_f_arraysize);

    add_builtin_func("apply_type", jl_f_instantiate_type);

    // builtin types
    add_builtin("Any", (jl_value_t*)jl_any_type);
    add_builtin("Void", (jl_value_t*)jl_void_type);
    add_builtin("nothing", (jl_value_t*)jl_nothing);
    add_builtin("TypeVar", (jl_value_t*)jl_tvar_type);
    add_builtin("TypeName", (jl_value_t*)jl_typename_type);
    add_builtin("TypeConstructor", (jl_value_t*)jl_typector_type);
    add_builtin("Tuple", (jl_value_t*)jl_anytuple_type);
    add_builtin("NTuple", (jl_value_t*)jl_ntuple_type);
    add_builtin("Vararg", (jl_value_t*)jl_vararg_type);
    add_builtin("Type", (jl_value_t*)jl_type_type);
    add_builtin("DataType", (jl_value_t*)jl_datatype_type);
    add_builtin("Union", (jl_value_t*)jl_uniontype_type);
    add_builtin("SimpleVector", (jl_value_t*)jl_simplevector_type);

    add_builtin("Module", (jl_value_t*)jl_module_type);
    add_builtin("Method", (jl_value_t*)jl_method_type);
    add_builtin("MethodTable", (jl_value_t*)jl_methtable_type);
    add_builtin("Symbol", (jl_value_t*)jl_sym_type);
    add_builtin("GenSym", (jl_value_t*)jl_gensym_type);
    add_builtin("IntrinsicFunction", (jl_value_t*)jl_intrinsic_type);
    add_builtin("Function", (jl_value_t*)jl_function_type);
    add_builtin("LambdaStaticData", (jl_value_t*)jl_lambda_info_type);
    add_builtin("Ref", (jl_value_t*)jl_ref_type);
    add_builtin("Ptr", (jl_value_t*)jl_pointer_type);
    add_builtin("Box", (jl_value_t*)jl_box_type);
    add_builtin("Task", (jl_value_t*)jl_task_type);

    add_builtin("AbstractArray", (jl_value_t*)jl_abstractarray_type);
    add_builtin("DenseArray", (jl_value_t*)jl_densearray_type);
    add_builtin("Array", (jl_value_t*)jl_array_type);

    add_builtin("Expr", (jl_value_t*)jl_expr_type);
    add_builtin("LineNumberNode", (jl_value_t*)jl_linenumbernode_type);
    add_builtin("LabelNode", (jl_value_t*)jl_labelnode_type);
    add_builtin("GotoNode", (jl_value_t*)jl_gotonode_type);
    add_builtin("QuoteNode", (jl_value_t*)jl_quotenode_type);
    add_builtin("TopNode", (jl_value_t*)jl_topnode_type);
    add_builtin("NewvarNode", (jl_value_t*)jl_newvarnode_type);
    add_builtin("GlobalRef", (jl_value_t*)jl_globalref_type);

#ifdef _P64
    add_builtin("Int", (jl_value_t*)jl_int64_type);
#else
    add_builtin("Int", (jl_value_t*)jl_int32_type);
#endif

    add_builtin("ANY", jl_ANY_flag);
}

// toys for debugging ---------------------------------------------------------

static size_t jl_show_svec(JL_STREAM *out, jl_svec_t *t, char *head, char *opn, char *cls)
{
    size_t i, n=0, len = jl_svec_len(t);
    n += jl_printf(out, "%s", head);
    n += jl_printf(out, "%s", opn);
    for (i = 0; i < len; i++) {
        jl_value_t *v = jl_svecref(t,i);
        n += jl_static_show(out, v);
        if (i != len-1)
            n += jl_printf(out, ", ");
    }
    n += jl_printf(out, "%s", cls);
    return n;
}

#define MAX_DEPTH 25

static size_t jl_static_show_x(JL_STREAM *out, jl_value_t *v, int depth);

// `v` might be pointing to a field inlined in a structure therefore
// `jl_typeof(v)` may not be the same with `vt` and only `vt` should be
// used to determine the type of the value.
// This is necessary to make sure that this function doesn't allocate any
// memory through the Julia GC
static size_t jl_static_show_x_(JL_STREAM *out, jl_value_t *v,
                                jl_datatype_t *vt, int depth)
{
    if (depth > MAX_DEPTH) { // cheap way of bailing out of cycles
        return jl_printf(out, "•");
    }
    size_t n = 0;
    depth++;
    if ((uintptr_t)vt < 4096U) {
        n += jl_printf(out, "<?#%p::%p>", v, vt);
    }
    else if ((uintptr_t)v < 4096U) {
        n += jl_printf(out, "<?#%p::", v);
        n += jl_static_show_x(out, (jl_value_t*)vt, depth);
        n += jl_printf(out, ">");
    }
    else if (vt == jl_lambda_info_type) {
        jl_lambda_info_t *li = (jl_lambda_info_t*)v;
        n += jl_static_show_x(out, (jl_value_t*)li->module, depth);
        if (li->specTypes) {
            n += jl_printf(out, ".");
            n += jl_show_svec(out, li->specTypes->parameters,
                              li->name->name, "(", ")");
        }
        else {
            n += jl_printf(out, ".%s(?)", li->name->name);
        }
        // The following is nice for debugging, but allocates memory and generates a lot of output
        // so it may not be a good idea to to have it active
        //jl_printf(out, " -> ");
        //jl_static_show(out, !jl_is_expr(li->ast) ? jl_uncompress_ast(li, li->ast) : li->ast);
    }
    else if (vt == jl_simplevector_type) {
        n += jl_show_svec(out, (jl_svec_t*)v, "svec", "(", ")");
    }
    else if (vt == jl_datatype_type) {
        jl_datatype_t *dv = (jl_datatype_t*)v;
        if (dv->name->module != jl_core_module) {
            n += jl_static_show_x(out, (jl_value_t*)dv->name->module, depth);
            n += jl_printf(out, ".");
        }
        n += jl_printf(out, "%s", dv->name->name->name);
        if (dv->parameters && (jl_value_t*)dv != dv->name->primary &&
            !jl_types_equal((jl_value_t*)dv, (jl_value_t*)jl_tuple_type)) {
            size_t j, tlen = jl_nparams(dv);
            if (tlen > 0) {
                n += jl_printf(out, "{");
                for (j = 0; j < tlen; j++) {
                    jl_value_t *p = jl_tparam(dv,j);
                    n += jl_static_show_x(out, p, depth);
                    if (j != tlen-1)
                        n += jl_printf(out, ", ");
                }
                n += jl_printf(out, "}");
            }
            else if (dv->name == jl_tuple_typename) {
                n += jl_printf(out, "{}");
            }
        }
    }
    else if (vt == jl_function_type) {
        if (jl_is_gf(v)) {
            n += jl_printf(out, "%s", jl_gf_name(v)->name);
        }
        else {
            n += jl_printf(out, "#<function>");
        }
    }
    else if (vt == jl_intrinsic_type) {
        n += jl_printf(out, "#<intrinsic function %d>",
                       *(uint32_t*)jl_data_ptr(v));
    }
    else if (vt == jl_int64_type) {
        n += jl_printf(out, "%" PRId64, *(int64_t*)v);
    }
    else if (vt == jl_int32_type) {
        n += jl_printf(out, "%" PRId32, *(int32_t*)v);
    }
    else if (vt == jl_int16_type) {
        n += jl_printf(out, "%" PRId16, *(int16_t*)v);
    }
    else if (vt == jl_int8_type) {
        n += jl_printf(out, "%" PRId8, *(int8_t*)v);
    }
    else if (vt == jl_uint64_type) {
        n += jl_printf(out, "0x%016" PRIx64, *(uint64_t*)v);
    }
    else if (vt == jl_uint32_type) {
        n += jl_printf(out, "0x%08" PRIx32, *(uint32_t*)v);
    }
    else if (vt == jl_uint16_type) {
        n += jl_printf(out, "0x%04" PRIx16, *(uint16_t*)v);
    }
    else if (vt == jl_uint8_type) {
        n += jl_printf(out, "0x%02" PRIx8, *(uint8_t*)v);
    }
    else if (jl_is_cpointer_type((jl_value_t*)vt)) {
#ifdef _P64
        n += jl_printf(out, "0x%016" PRIx64, *(uint64_t*)v);
#else
        n += jl_printf(out, "0x%08" PRIx32, *(uint32_t*)v);
#endif
    }
    else if (vt == jl_float32_type) {
        n += jl_printf(out, "%g", *(float*)v);
    }
    else if (vt == jl_float64_type) {
        n += jl_printf(out, "%g", *(double*)v);
    }
    else if (vt == jl_bool_type) {
        n += jl_printf(out, "%s", *(uint8_t*)v ? "true" : "false");
    }
    else if ((jl_value_t*)vt == jl_typeof(jl_nothing)) {
        n += jl_printf(out, "nothing");
    }
    else if (vt == jl_ascii_string_type || vt == jl_utf8_string_type) {
        n += jl_printf(out, "\"%s\"", jl_iostr_data(v));
    }
    else if (vt == jl_uniontype_type) {
        n += jl_show_svec(out, ((jl_uniontype_t*)v)->types, "Union", "{", "}");
    }
    else if (vt == jl_typector_type) {
        n += jl_static_show_x(out, ((jl_typector_t*)v)->body, depth);
    }
    else if (vt == jl_tvar_type) {
        if (((jl_tvar_t*)v)->lb != jl_bottom_type) {
            n += jl_static_show(out, ((jl_tvar_t*)v)->lb);
            n += jl_printf(out, "<:");
        }
        n += jl_printf(out, "%s%s<:", (((jl_tvar_t*)v)->bound)?"#":"", ((jl_tvar_t*)v)->name->name);
        n += jl_static_show(out, ((jl_tvar_t*)v)->ub);
    }
    else if (vt == jl_module_type) {
        jl_module_t *m = (jl_module_t*)v;
        if (m->parent != m && m->parent != jl_main_module) {
            n += jl_static_show_x(out, (jl_value_t*)m->parent, depth);
            n += jl_printf(out, ".");
        }
        n += jl_printf(out, "%s", m->name->name);
    }
    else if (vt == jl_sym_type) {
        n += jl_printf(out, ":%s", ((jl_sym_t*)v)->name);
    }
    else if (vt == jl_gensym_type) {
        n += jl_printf(out, "GenSym(%" PRIuPTR ")",
                       (uintptr_t)((jl_gensym_t*)v)->id);
    }
    else if (vt == jl_symbolnode_type) {
        n += jl_printf(out, "%s::", jl_symbolnode_sym(v)->name);
        n += jl_static_show_x(out, jl_symbolnode_type(v), depth);
    }
    else if (vt == jl_globalref_type) {
        n += jl_static_show_x(out, (jl_value_t*)jl_globalref_mod(v), depth);
        n += jl_printf(out, ".%s", jl_globalref_name(v)->name);
    }
    else if (vt == jl_labelnode_type) {
        n += jl_printf(out, "%" PRIuPTR ":", jl_labelnode_label(v));
    }
    else if (vt == jl_gotonode_type) {
        n += jl_printf(out, "goto %" PRIuPTR, jl_gotonode_label(v));
    }
    else if (vt == jl_quotenode_type) {
        jl_value_t *qv = *(jl_value_t**)v;
        if (!jl_is_symbol(qv)) {
            n += jl_printf(out, "quote ");
        }
        n += jl_static_show_x(out, qv, depth);
        if (!jl_is_symbol(qv)) {
            n += jl_printf(out, " end");
        }
    }
    else if (vt == jl_newvarnode_type) {
        n += jl_printf(out, "<newvar ");
        n += jl_static_show_x(out, *(jl_value_t**)v, depth);
        n += jl_printf(out, ">");
    }
    else if (vt == jl_topnode_type) {
        n += jl_printf(out, "top(");
        n += jl_static_show_x(out, *(jl_value_t**)v, depth);
        n += jl_printf(out, ")");
    }
    else if (vt == jl_linenumbernode_type) {
        n += jl_printf(out, "# line %" PRIuPTR " %s",
                       jl_linenode_line(v), jl_linenode_file(v)->name);
    }
    else if (vt == jl_expr_type) {
        jl_expr_t *e = (jl_expr_t*)v;
        if (e->head == assign_sym && jl_array_len(e->args) == 2) {
            n += jl_static_show_x(out, jl_exprarg(e,0), depth);
            n += jl_printf(out, " = ");
            n += jl_static_show_x(out, jl_exprarg(e,1), depth);
        }
        else {
            char sep = ' ';
            if (e->head == body_sym)
                sep = '\n';
            n += jl_printf(out, "Expr(:%s", e->head->name);
            size_t i, len = jl_array_len(e->args);
            for (i = 0; i < len; i++) {
                n += jl_printf(out, ",%c", sep);
                n += jl_static_show_x(out, jl_exprarg(e,i), depth);
            }
            n += jl_printf(out, ")::");
            n += jl_static_show_x(out, e->etype, depth);
        }
    }
    else if (jl_is_array_type(vt)) {
        n += jl_static_show_x(out, (jl_value_t*)vt, depth);
        n += jl_printf(out, "[");
        size_t j, tlen = jl_array_len(v);
        jl_array_t *av = (jl_array_t*)v;
        jl_datatype_t *el_type = (jl_datatype_t*)jl_tparam0(vt);
        for (j = 0; j < tlen; j++) {
            if (av->ptrarray) {
                n += jl_static_show_x(out, jl_cellref(v, j), depth);
            } else {
                char *ptr = ((char*)av->data) + j * av->elsize;
                n += jl_static_show_x_(out, (jl_value_t*)ptr, el_type, depth);
            }
            if (j != tlen-1)
                n += jl_printf(out, ", ");
        }
        if (j < tlen) n += jl_printf(out, " ...");
        n += jl_printf(out, "]");
    }
    else if (vt == jl_loaderror_type) {
        n += jl_printf(out, "LoadError(at ");
        n += jl_static_show_x(out, *(jl_value_t**)v, depth);
        // Access the field directly to avoid allocation
        n += jl_printf(out, " line %" PRIdPTR, ((intptr_t*)v)[1]);
        n += jl_printf(out, ": ");
        n += jl_static_show_x(out, ((jl_value_t**)v)[2], depth);
        n += jl_printf(out, ")");
    }
    else if (vt == jl_errorexception_type) {
        n += jl_printf(out, "ErrorException(");
        n += jl_static_show_x(out, *(jl_value_t**)v, depth);
        n += jl_printf(out, ")");
    }
    else if (jl_is_datatype(vt)) {
        int istuple = jl_is_tuple_type(vt);
        if (!istuple)
            n += jl_static_show_x(out, (jl_value_t*)vt, depth);
        n += jl_printf(out, "(");
        size_t nb = jl_datatype_size(vt);
        size_t tlen = jl_datatype_nfields(vt);
        if (nb > 0 && tlen == 0) {
            char *data = (char*)jl_data_ptr(v);
            n += jl_printf(out, "0x");
            for(int i=nb-1; i >= 0; --i)
                n += jl_printf(out, "%02" PRIx8, data[i]);
        }
        else {
            for (size_t i = 0; i < tlen; i++) {
                if (!istuple) {
                    n += jl_printf(out, "%s", ((jl_sym_t*)jl_svecref(vt->name->names, i))->name);
                    //jl_fielddesc_t f = t->fields[i];
                    n += jl_printf(out, "=");
                }
                size_t offs = jl_field_offset(vt, i);
                char *fld_ptr = (char*)v + offs;
                if (jl_field_isptr(vt, i)) {
                    n += jl_static_show_x(out, *(jl_value_t**)fld_ptr, depth);
                } else {
                    n += jl_static_show_x_(out, (jl_value_t*)fld_ptr,
                                           (jl_datatype_t*)jl_field_type(vt, i),
                                           depth);
                }
                if (istuple && tlen==1)
                    n += jl_printf(out, ",");
                else if (i != tlen-1)
                    n += jl_printf(out, ", ");
            }
        }
        n += jl_printf(out, ")");
    }
    else {
        n += jl_printf(out, "<?#%p::", v);
        n += jl_static_show_x(out, (jl_value_t*)vt, depth);
        n += jl_printf(out, ">");
    }
    return n;
}

static size_t jl_static_show_x(JL_STREAM *out, jl_value_t *v, int depth)
{
    // mimic jl_show, but never calling a julia method and
    // (hopefully) never allocate through julia gc
    if (v == NULL) {
        return jl_printf(out, "#<null>");
    }
    else if ((uintptr_t)v < 4096U) {
        return jl_printf(out, "#<%d>", (int)(uintptr_t)v);
    }
    return jl_static_show_x_(out, v, (jl_datatype_t*)jl_typeof(v), depth);
}

DLLEXPORT size_t jl_static_show(JL_STREAM *out, jl_value_t *v)
{
    return jl_static_show_x(out, v, 0);
}

DLLEXPORT size_t jl_static_show_func_sig(JL_STREAM *s, jl_value_t *type)
{
    if (!jl_is_tuple_type(type))
        return jl_static_show(s, type);
    size_t n = 0;
    size_t tl = jl_nparams(type);
    n += jl_printf(s, "(");
    size_t i;
    for (i = 0;i < tl;i++) {
        jl_value_t *tp = jl_tparam(type, i);
        if (i != tl - 1) {
            n += jl_static_show(s, tp);
            n += jl_printf(s, ", ");
        }
        else {
            if (jl_is_vararg_type(tp)) {
                n += jl_static_show(s, jl_tparam0(tp));
                n += jl_printf(s, "...");
            }
            else {
                n += jl_static_show(s, tp);
            }
        }
    }
    n += jl_printf(s, ")");
    return n;
}

int in_jl_ = 0;
DLLEXPORT void jl_(void *jl_value)
{
    in_jl_++;
    JL_TRY {
        (void)jl_static_show((JL_STREAM*)STDERR_FILENO, (jl_value_t*)jl_value);
        jl_printf((JL_STREAM*)STDERR_FILENO,"\n");
    }
    JL_CATCH {
        jl_printf((JL_STREAM*)STDERR_FILENO, "\n!!! ERROR in jl_ -- ABORTING !!!\n");
    }
    in_jl_--;
}

DLLEXPORT void jl_breakpoint(jl_value_t *v)
{
    // put a breakpoint in you debugger here
}

#ifdef __cplusplus
}
#endif
