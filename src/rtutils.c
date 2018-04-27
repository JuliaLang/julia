// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  utility functions used by the runtime system, generated code, and Base library
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
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// exceptions -----------------------------------------------------------------

JL_DLLEXPORT void JL_NORETURN jl_error(const char *str)
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

static jl_value_t *jl_vexceptionf(jl_datatype_t *exception_type,
                                  const char *fmt, va_list args)
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
    jl_value_t *e = jl_new_struct(exception_type, msg);
    JL_GC_POP();
    return e;
}

JL_DLLEXPORT void JL_NORETURN jl_errorf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    jl_value_t *e = jl_vexceptionf(jl_errorexception_type, fmt, args);
    va_end(args);
    jl_throw(e);
}

JL_DLLEXPORT void JL_NORETURN jl_exceptionf(jl_datatype_t *exception_type,
                                            const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    jl_value_t *e = jl_vexceptionf(exception_type, fmt, args);
    va_end(args);
    jl_throw(e);
}

jl_value_t *jl_get_exceptionf(jl_datatype_t *exception_type,
                              const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    jl_value_t *e = jl_vexceptionf(exception_type, fmt, args);
    va_end(args);
    return e;
}

JL_DLLEXPORT void JL_NORETURN jl_too_few_args(const char *fname, int min)
{
    jl_exceptionf(jl_argumenterror_type, "%s: too few arguments (expected %d)", fname, min);
}

JL_DLLEXPORT void JL_NORETURN jl_too_many_args(const char *fname, int max)
{
    jl_exceptionf(jl_argumenterror_type, "%s: too many arguments (expected %d)", fname, max);
}

JL_DLLEXPORT void JL_NORETURN jl_type_error_rt(const char *fname, const char *context,
                                               jl_value_t *ty, jl_value_t *got)
{
    jl_value_t *ctxt=NULL;
    JL_GC_PUSH2(&ctxt, &got);
    ctxt = jl_pchar_to_string((char*)context, strlen(context));
    jl_value_t *ex = jl_new_struct(jl_typeerror_type, jl_symbol(fname),
                                   ctxt, ty, got);
    jl_throw(ex);
}

JL_DLLEXPORT void JL_NORETURN jl_type_error(const char *fname, jl_value_t *expected,
                                            jl_value_t *got)
{
    jl_type_error_rt(fname, "", expected, got);
}

JL_DLLEXPORT void JL_NORETURN jl_type_error_new_expr(jl_value_t *ty, jl_value_t *got)
{
    jl_type_error_rt("Type", "new", ty, got);
}

JL_DLLEXPORT void JL_NORETURN jl_undefined_var_error(jl_sym_t *var)
{
    jl_throw(jl_new_struct(jl_undefvarerror_type, var));
}

JL_DLLEXPORT void JL_NORETURN jl_bounds_error(jl_value_t *v, jl_value_t *t)
{
    JL_GC_PUSH2(&v, &t); // root arguments so the caller doesn't need to
    jl_throw(jl_new_struct((jl_datatype_t*)jl_boundserror_type, v, t));
}

JL_DLLEXPORT void JL_NORETURN jl_bounds_error_v(jl_value_t *v, jl_value_t **idxs, size_t nidxs)
{
    jl_value_t *t = NULL;
    // items in idxs are assumed to already be rooted
    JL_GC_PUSH2(&v, &t); // root v so the caller doesn't need to
    t = jl_f_tuple(NULL, idxs, nidxs);
    jl_throw(jl_new_struct((jl_datatype_t*)jl_boundserror_type, v, t));
}

JL_DLLEXPORT void JL_NORETURN jl_bounds_error_tuple_int(jl_value_t **v, size_t nv, size_t i)
{
    // values in v are expected to already be gc-rooted
    jl_bounds_error_int(jl_f_tuple(NULL, v, nv), i);
}

JL_DLLEXPORT void JL_NORETURN jl_bounds_error_unboxed_int(void *data, jl_value_t *vt, size_t i)
{
    jl_value_t *t = NULL, *v = NULL;
    // data is expected to be gc-safe (either gc-rooted, or alloca)
    // vt is expected to be gc-rooted (in a linfo-root probably)
    JL_GC_PUSH2(&v, &t);
    v = jl_new_bits(vt, data);
    t = jl_box_long(i);
    jl_throw(jl_new_struct((jl_datatype_t*)jl_boundserror_type, v, t));
}

JL_DLLEXPORT void JL_NORETURN jl_bounds_error_int(jl_value_t *v, size_t i)
{
    jl_value_t *t = NULL;
    JL_GC_PUSH2(&v, &t); // root arguments so the caller doesn't need to
    t = jl_box_long(i);
    jl_throw(jl_new_struct((jl_datatype_t*)jl_boundserror_type, v, t));
}

JL_DLLEXPORT void JL_NORETURN jl_bounds_error_ints(jl_value_t *v, size_t *idxs, size_t nidxs)
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

JL_DLLEXPORT void JL_NORETURN jl_eof_error(void)
{
    jl_datatype_t *eof_error =
        (jl_datatype_t*)jl_get_global(jl_base_module, jl_symbol("EOFError"));
    assert(eof_error != NULL);
    jl_exceptionf(eof_error, "");
}

// get kwsorter field, with appropriate error check and message
JL_DLLEXPORT jl_value_t *jl_get_keyword_sorter(jl_value_t *f)
{
    jl_methtable_t *mt = jl_gf_mtable(f);
    if (mt->kwsorter == NULL)
        jl_errorf("function %s does not accept keyword arguments", jl_symbol_name(mt->name));
    return mt->kwsorter;
}

JL_DLLEXPORT void jl_typeassert(jl_value_t *x, jl_value_t *t)
{
    if (!jl_isa(x,t))
        jl_type_error("typeassert", t, x);
}

JL_DLLEXPORT void jl_enter_handler(jl_handler_t *eh)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *current_task = ptls->current_task;
    // Must have no safepoint
    eh->prev = current_task->eh;
    eh->gcstack = ptls->pgcstack;
#ifdef JULIA_ENABLE_THREADING
    eh->gc_state = ptls->gc_state;
    eh->locks_len = current_task->locks.len;
#endif
    eh->defer_signal = ptls->defer_signal;
    eh->finalizers_inhibited = ptls->finalizers_inhibited;
    eh->world_age = ptls->world_age;
    current_task->eh = eh;
#ifdef ENABLE_TIMINGS
    eh->timing_stack = current_task->timing_stack;
#endif
}

JL_DLLEXPORT void jl_pop_handler(int n)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (__unlikely(n <= 0))
        return;
    jl_handler_t *eh = ptls->current_task->eh;
    while (--n > 0)
        eh = eh->prev;
    jl_eh_restore_state(eh);
}

JL_DLLEXPORT jl_value_t *jl_apply_with_saved_exception_state(jl_value_t **args, uint32_t nargs, int drop_exceptions)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_value_t *exc = ptls->exception_in_transit;
    jl_array_t *bt = NULL;
    jl_array_t *bt2 = NULL;
    JL_GC_PUSH3(&exc, &bt, &bt2);
    if (ptls->bt_size > 0) {
        jl_get_backtrace(&bt, &bt2);
        ptls->bt_size = 0;
    }
    jl_value_t *v;
    JL_TRY {
        v = jl_apply(args, nargs);
    }
    JL_CATCH {
        if (!drop_exceptions) {
            jl_printf(JL_STDERR, "Internal error: encountered unexpected error in runtime:\n");
            jl_static_show(JL_STDERR, ptls->exception_in_transit);
            jl_printf(JL_STDERR, "\n");
            jlbacktrace(); // written to STDERR_FILENO
        }
        v = NULL;
    }
    ptls->exception_in_transit = exc;
    if (bt != NULL) {
        // This is sufficient because bt2 roots the gc-managed values
        memcpy(ptls->bt_data, bt->data, jl_array_len(bt) * sizeof(void*));
        ptls->bt_size = jl_array_len(bt);
    }
    JL_GC_POP();
    return v;
}

// misc -----------------------------------------------------------------------

// perform f(args...) on stack
JL_DLLEXPORT jl_value_t *jl_apply_2va(jl_value_t *f, jl_value_t **args, uint32_t nargs)
{
    nargs++;
    int onstack = (nargs < jl_page_size/sizeof(jl_value_t*));
    jl_value_t **newargs;
    JL_GC_PUSHARGS(newargs, onstack ? nargs : 1);
    jl_svec_t *arg_heap = NULL;
    newargs[0] = f;  // make sure f is rooted
    if (!onstack) {
        arg_heap = jl_alloc_svec(nargs);
        newargs[0] = (jl_value_t*)arg_heap;
        newargs = jl_svec_data(arg_heap);
        newargs[0] = f;
    }
    memcpy(&newargs[1], args, (nargs-1)*sizeof(jl_value_t*));
    jl_value_t *ret = jl_apply_generic(newargs, nargs);
    JL_GC_POP();
    return ret;
}

// conversion -----------------------------------------------------------------

JL_DLLEXPORT void *(jl_symbol_name)(jl_sym_t *s)
{
    return jl_symbol_name(s);
}

// WARNING: THIS FUNCTION IS NEVER CALLED BUT INLINE BY CCALL
JL_DLLEXPORT void *jl_array_ptr(jl_array_t *a)
{
    return a->data;
}
JL_DLLEXPORT jl_value_t *jl_value_ptr(jl_value_t *a)
{
    return a;
}
JL_DLLEXPORT void jl_gc_use(jl_value_t *a)
{
    (void)a;
}

// parsing --------------------------------------------------------------------

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

JL_DLLEXPORT jl_nullable_float64_t jl_try_substrtod(char *str, size_t offset, size_t len)
{
    char *p;
    char *bstr = str+offset;
    char *pend = bstr+len;
    char *tofree = NULL;
    int hasvalue = 0;

    errno = 0;
    if (!(*pend == '\0' || isspace((unsigned char)*pend) || *pend == ',')) {
        // confusing data outside substring. must copy.
        char *newstr;
        if (len + 1 < jl_page_size) {
            newstr = (char*)alloca(len + 1);
        }
        else {
            newstr = tofree = (char*)malloc(len + 1);
        }
        memcpy(newstr, bstr, len);
        newstr[len] = 0;
        bstr = newstr;
        pend = bstr+len;
    }
    double out = jl_strtod_c(bstr, &p);

    if (errno==ERANGE && (out==0 || out==HUGE_VAL || out==-HUGE_VAL)) {
        hasvalue = 0;
    }
    else if (p == bstr) {
        hasvalue = 0;
    }
    else {
        // Deal with case where the substring might be something like "1 ",
        // which is OK, and "1 X", which we don't allow.
        hasvalue = substr_isspace(p, pend) ? 1 : 0;
    }

    if (__unlikely(tofree))
        free(tofree);

    jl_nullable_float64_t ret = {(uint8_t)hasvalue, out};
    return ret;
}

JL_DLLEXPORT int jl_substrtod(char *str, size_t offset, size_t len, double *out)
{
    jl_nullable_float64_t nd = jl_try_substrtod(str, offset, len);
    if (0 != nd.hasvalue) {
        *out = nd.value;
        return 0;
    }
    return 1;
}

// MSVC pre-2013 did not define HUGE_VALF
#ifndef HUGE_VALF
#define HUGE_VALF (1e25f * 1e25f)
#endif

JL_DLLEXPORT jl_nullable_float32_t jl_try_substrtof(char *str, size_t offset, size_t len)
{
    char *p;
    char *bstr = str+offset;
    char *pend = bstr+len;
    char *tofree = NULL;
    int hasvalue = 0;

    errno = 0;
    if (!(*pend == '\0' || isspace((unsigned char)*pend) || *pend == ',')) {
        // confusing data outside substring. must copy.
        char *newstr;
        if (len + 1 < jl_page_size) {
            newstr = (char*)alloca(len + 1);
        }
        else {
            newstr = tofree = (char*)malloc(len + 1);
        }
        memcpy(newstr, bstr, len);
        newstr[len] = 0;
        bstr = newstr;
        pend = bstr+len;
    }
#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
    float out = (float)jl_strtod_c(bstr, &p);
#else
    float out = jl_strtof_c(bstr, &p);
#endif

    if (errno==ERANGE && (out==0 || out==HUGE_VALF || out==-HUGE_VALF)) {
        hasvalue = 0;
    }
    else if (p == bstr) {
        hasvalue = 0;
    }
    else {
        // Deal with case where the substring might be something like "1 ",
        // which is OK, and "1 X", which we don't allow.
        hasvalue = substr_isspace(p, pend) ? 1 : 0;
    }

    if (__unlikely(tofree))
        free(tofree);

    jl_nullable_float32_t ret = {(uint8_t)hasvalue, out};
    return ret;
}

JL_DLLEXPORT int jl_substrtof(char *str, int offset, size_t len, float *out)
{
    jl_nullable_float32_t nf = jl_try_substrtof(str, offset, len);
    if (0 != nf.hasvalue) {
        *out = nf.value;
        return 0;
    }
    return 1;
}

// showing --------------------------------------------------------------------

JL_DLLEXPORT void jl_flush_cstdio(void)
{
    fflush(stdout);
    fflush(stderr);
}

JL_DLLEXPORT jl_value_t *jl_stdout_obj(void)
{
    if (jl_base_module == NULL) return NULL;
    jl_value_t *stdout_obj = jl_get_global(jl_base_module, jl_symbol("stdout"));
    return stdout_obj;
}

JL_DLLEXPORT jl_value_t *jl_stderr_obj(void)
{
    if (jl_base_module == NULL) return NULL;
    jl_value_t *stderr_obj = jl_get_global(jl_base_module, jl_symbol("stderr"));
    return stderr_obj;
}

// toys for debugging ---------------------------------------------------------

static size_t jl_show_svec(JL_STREAM *out, jl_svec_t *t, const char *head, const char *opn, const char *cls)
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

struct recur_list {
    struct recur_list *prev;
    jl_value_t *v;
};

static size_t jl_static_show_x(JL_STREAM *out, jl_value_t *v, struct recur_list *depth);

JL_DLLEXPORT int jl_id_start_char(uint32_t wc);
JL_DLLEXPORT int jl_id_char(uint32_t wc);

JL_DLLEXPORT int jl_is_identifier(char *str)
{
    size_t i = 0;
    uint32_t wc = u8_nextchar(str, &i);
    if (!jl_id_start_char(wc))
        return 0;
    while ((wc = u8_nextchar(str, &i)) != 0) {
        if (!jl_id_char(wc))
            return 0;
    }
    return 1;
}

// `v` might be pointing to a field inlined in a structure therefore
// `jl_typeof(v)` may not be the same with `vt` and only `vt` should be
// used to determine the type of the value.
// This is necessary to make sure that this function doesn't allocate any
// memory through the Julia GC
static size_t jl_static_show_x_(JL_STREAM *out, jl_value_t *v, jl_datatype_t *vt,
                                struct recur_list *depth)
{
    size_t n = 0;
    if ((uintptr_t)vt < 4096U) {
        n += jl_printf(out, "<?#%p::%p>", (void*)v, (void*)vt);
    }
    else if ((uintptr_t)v < 4096U) {
        n += jl_printf(out, "<?#%p::", (void*)v);
        n += jl_static_show_x(out, (jl_value_t*)vt, depth);
        n += jl_printf(out, ">");
    }
    else if (vt == jl_method_type) {
        jl_method_t *m = (jl_method_t*)v;
        n += jl_static_show_x(out, (jl_value_t*)m->module, depth);
        n += jl_printf(out, ".%s(...)", jl_symbol_name(m->name));
    }
    else if (vt == jl_method_instance_type) {
        jl_method_instance_t *li = (jl_method_instance_t*)v;
        if (jl_is_method(li->def.method)) {
            if (li->specTypes) {
                n += jl_static_show_func_sig(out, li->specTypes);
            }
            else {
                jl_method_t *m = li->def.method;
                n += jl_static_show_x(out, (jl_value_t*)m->module, depth);
                n += jl_printf(out, ".%s(?)", jl_symbol_name(m->name));
            }
        }
        else {
            n += jl_static_show_x(out, (jl_value_t*)li->def.module, depth);
            n += jl_printf(out, ".<toplevel thunk> -> ");
            n += jl_static_show_x(out, li->inferred, depth);
        }
    }
    else if (vt == jl_simplevector_type) {
        n += jl_show_svec(out, (jl_svec_t*)v, "svec", "(", ")");
    }
    else if (v == (jl_value_t*)jl_unionall_type) {
        // avoid printing `typeof(Type)` for `UnionAll`.
        n += jl_printf(out, "UnionAll");
    }
    else if (vt == jl_datatype_type) {
        jl_datatype_t *dv = (jl_datatype_t*)v;
        jl_sym_t *globname = dv->name->mt != NULL ? dv->name->mt->name : NULL;
        int globfunc = 0;
        if (globname && !strchr(jl_symbol_name(globname), '#') &&
            !strchr(jl_symbol_name(globname), '@') && dv->name->module &&
            jl_binding_resolved_p(dv->name->module, globname)) {
            jl_binding_t *b = jl_get_binding(dv->name->module, globname);
            if (b && jl_typeof(b->value) == v)
                globfunc = 1;
        }
        jl_sym_t *sym = globfunc ? globname : dv->name->name;
        char *sn = jl_symbol_name(sym);
        int hidden = !globfunc && strchr(sn, '#');
        size_t i = 0;
        int quote = 0;
        if (hidden) {
            n += jl_printf(out, "getfield(");
        }
        else if (globfunc) {
            n += jl_printf(out, "typeof(");
        }
        if (dv->name->module != jl_core_module || !jl_module_exports_p(jl_core_module, sym)) {
            n += jl_static_show_x(out, (jl_value_t*)dv->name->module, depth);
            if (!hidden) {
                n += jl_printf(out, ".");
                if (globfunc && !jl_id_start_char(u8_nextchar(sn, &i))) {
                    n += jl_printf(out, ":(");
                    quote = 1;
                }
            }
        }
        if (hidden) {
            n += jl_printf(out, ", Symbol(\"");
            n += jl_printf(out, "%s", sn);
            n += jl_printf(out, "\"))");
        }
        else {
            n += jl_printf(out, "%s", sn);
            if (globfunc) {
                n += jl_printf(out, ")");
                if (quote)
                    n += jl_printf(out, ")");
            }
        }
        if (dv->parameters && (jl_value_t*)dv != dv->name->wrapper &&
            (jl_has_free_typevars(v) ||
             (jl_value_t*)dv != (jl_value_t*)jl_tuple_type)) {
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
    else if (vt == jl_intrinsic_type) {
        int f = *(uint32_t*)jl_data_ptr(v);
        n += jl_printf(out, "#<intrinsic #%d %s>", f, jl_intrinsic_name(f));
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
        n += jl_printf(out, "%gf", *(float*)v);
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
    else if (vt == jl_string_type) {
        n += jl_printf(out, "\"");
        jl_uv_puts(out, jl_string_data(v), jl_string_len(v)); n += jl_string_len(v);
        n += jl_printf(out, "\"");
    }
    else if (v == jl_bottom_type) {
        n += jl_printf(out, "Union{}");
    }
    else if (vt == jl_uniontype_type) {
        n += jl_printf(out, "Union{");
        while (jl_is_uniontype(v)) {
            // tail-recurse on b to flatten the printing of the Union structure in the common case
            n += jl_static_show_x(out, ((jl_uniontype_t*)v)->a, depth);
            n += jl_printf(out, ", ");
            v = ((jl_uniontype_t*)v)->b;
        }
        n += jl_static_show_x(out, v, depth);
        n += jl_printf(out, "}");
    }
    else if (vt == jl_unionall_type) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        n += jl_static_show_x(out, ua->body, depth);
        n += jl_printf(out, " where ");
        n += jl_static_show_x(out, (jl_value_t*)ua->var, depth->prev);
    }
    else if (vt == jl_tvar_type) {
        // show type-var bounds only if they aren't going to be printed by UnionAll later
        jl_tvar_t *var = (jl_tvar_t*)v;
        struct recur_list *p;
        int showbounds = 1;
        for (p = depth; p != NULL; p = p->prev) {
            if (jl_is_unionall(p->v) && ((jl_unionall_t*)p->v)->var == var) {
                showbounds = 0;
                break;
            }
        }
        jl_value_t *lb = var->lb, *ub = var->ub;
        if (showbounds && lb != jl_bottom_type) {
            // show type-var lower bound if it is defined
            int ua = jl_is_unionall(lb);
            if (ua)
                n += jl_printf(out, "(");
            n += jl_static_show_x(out, lb, depth);
            if (ua)
                n += jl_printf(out, ")");
            n += jl_printf(out, "<:");
        }
        n += jl_printf(out, "%s", jl_symbol_name(var->name));
        if (showbounds && (ub != (jl_value_t*)jl_any_type || lb != jl_bottom_type)) {
            // show type-var upper bound if it is defined, or if we showed the lower bound
            int ua = jl_is_unionall(ub);
            n += jl_printf(out, "<:");
            if (ua)
                n += jl_printf(out, "(");
            n += jl_static_show_x(out, ub, depth);
            if (ua)
                n += jl_printf(out, ")");
        }
    }
    else if (vt == jl_module_type) {
        jl_module_t *m = (jl_module_t*)v;
        if (m->parent != m && m->parent != jl_main_module) {
            n += jl_static_show_x(out, (jl_value_t*)m->parent, depth);
            n += jl_printf(out, ".");
        }
        n += jl_printf(out, "%s", jl_symbol_name(m->name));
    }
    else if (vt == jl_sym_type) {
        char *sn = jl_symbol_name((jl_sym_t*)v);
        int quoted = !jl_is_identifier(sn) && jl_operator_precedence(sn) == 0;
        if (quoted)
            n += jl_printf(out, "Symbol(\"");
        else
            n += jl_printf(out, ":");
        n += jl_printf(out, "%s", sn);
        if (quoted)
            n += jl_printf(out, "\")");
    }
    else if (vt == jl_ssavalue_type) {
        n += jl_printf(out, "SSAValue(%" PRIuPTR ")",
                       (uintptr_t)((jl_ssavalue_t*)v)->id);
    }
    else if (vt == jl_globalref_type) {
        n += jl_static_show_x(out, (jl_value_t*)jl_globalref_mod(v), depth);
        char *name = jl_symbol_name(jl_globalref_name(v));
        n += jl_printf(out, jl_is_identifier(name) ? ".%s" : ".:(%s)", name);
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
        else {
            n += jl_printf(out, ":(");
        }
        n += jl_static_show_x(out, qv, depth);
        if (!jl_is_symbol(qv)) {
            n += jl_printf(out, " end");
        }
        else {
            n += jl_printf(out, ")");
        }
    }
    else if (vt == jl_newvarnode_type) {
        n += jl_printf(out, "<newvar ");
        n += jl_static_show_x(out, *(jl_value_t**)v, depth);
        n += jl_printf(out, ">");
    }
    else if (vt == jl_linenumbernode_type) {
        n += jl_printf(out, "#= ");
        n += jl_static_show_x(out, jl_linenode_file(v), depth);
        n += jl_printf(out, ":%" PRIuPTR " =#", jl_linenode_line(v));
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
            n += jl_printf(out, "Expr(:%s", jl_symbol_name(e->head));
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
        n += jl_printf(out, "Array{");
        n += jl_static_show_x(out, (jl_value_t*)jl_tparam0(vt), depth);
        n += jl_printf(out, ", (");
        size_t i, ndims = jl_array_ndims(v);
        if (ndims == 1)
            n += jl_printf(out, "%" PRIdPTR ",", jl_array_dim0(v));
        else
            for (i = 0; i < ndims; i++)
                n += jl_printf(out, (i > 0 ? ", %" PRIdPTR : "%" PRIdPTR), jl_array_dim(v, i));
        n += jl_printf(out, ")}[");
        size_t j, tlen = jl_array_len(v);
        jl_array_t *av = (jl_array_t*)v;
        jl_datatype_t *el_type = (jl_datatype_t*)jl_tparam0(vt);
        int nlsep = 0;
        if (av->flags.ptrarray) {
            // print arrays with newlines, unless the elements are probably small
            for (j = 0; j < tlen; j++) {
                jl_value_t *p = jl_array_ptr_ref(av, j);
                if (p != NULL && (uintptr_t)p >= 4096U) {
                    jl_value_t *p_ty = jl_typeof(p);
                    if ((uintptr_t)p_ty >= 4096U) {
                        if (!jl_isbits(p_ty)) {
                            nlsep = 1;
                            break;
                        }
                    }
                }
            }
        }
        if (nlsep && tlen > 1)
            n += jl_printf(out, "\n  ");
        for (j = 0; j < tlen; j++) {
            if (av->flags.ptrarray) {
                n += jl_static_show_x(out, jl_array_ptr_ref(v, j), depth);
            }
            else {
                char *ptr = ((char*)av->data) + j * av->elsize;
                n += jl_static_show_x_(out, (jl_value_t*)ptr, el_type, depth);
            }
            if (j != tlen - 1)
                n += jl_printf(out, nlsep ? ",\n  " : ", ");
        }
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
        int istuple = jl_is_tuple_type(vt), isnamedtuple = jl_is_namedtuple_type(vt);
        size_t tlen = jl_datatype_nfields(vt);
        if (isnamedtuple) {
            if (tlen == 0)
                n += jl_printf(out, "NamedTuple");
        }
        else if (!istuple) {
            n += jl_static_show_x(out, (jl_value_t*)vt, depth);
        }
        n += jl_printf(out, "(");
        size_t nb = jl_datatype_size(vt);
        if (nb > 0 && tlen == 0) {
            uint8_t *data = (uint8_t*)v;
            n += jl_printf(out, "0x");
            for(int i = nb - 1; i >= 0; --i)
                n += jl_printf(out, "%02" PRIx8, data[i]);
        }
        else {
            size_t i = 0;
            if (vt == jl_typemap_entry_type)
                i = 1;
            for (; i < tlen; i++) {
                if (!istuple) {
                    n += jl_printf(out, "%s", jl_symbol_name(jl_field_name(vt, i)));
                    n += jl_printf(out, "=");
                }
                size_t offs = jl_field_offset(vt, i);
                char *fld_ptr = (char*)v + offs;
                if (jl_field_isptr(vt, i)) {
                    n += jl_static_show_x(out, *(jl_value_t**)fld_ptr, depth);
                }
                else {
                    jl_datatype_t *ft = (jl_datatype_t*)jl_field_type(vt, i);
                    if (jl_is_uniontype(ft)) {
                        uint8_t sel = ((uint8_t*)fld_ptr)[jl_field_size(vt, i) - 1];
                        ft = (jl_datatype_t*)jl_nth_union_component((jl_value_t*)ft, sel);
                    }
                    n += jl_static_show_x_(out, (jl_value_t*)fld_ptr, ft, depth);
                }
                if ((istuple || isnamedtuple) && tlen == 1)
                    n += jl_printf(out, ",");
                else if (i != tlen - 1)
                    n += jl_printf(out, ", ");
            }
            if (vt == jl_typemap_entry_type) {
                n += jl_printf(out, ", next=↩︎\n  ");
                n += jl_static_show_x(out, jl_fieldref_noalloc(v, 0), depth);
            }
        }
        n += jl_printf(out, ")");
    }
    else {
        n += jl_printf(out, "<?#%p::", (void*)v);
        n += jl_static_show_x(out, (jl_value_t*)vt, depth);
        n += jl_printf(out, ">");
    }
    return n;
}

static size_t jl_static_show_x(JL_STREAM *out, jl_value_t *v, struct recur_list *depth)
{
    // show values without calling a julia method or allocating through the GC
    if (v == NULL) {
        return jl_printf(out, "#<null>");
    }
    else if ((uintptr_t)v < 4096U) {
        return jl_printf(out, "#<%d>", (int)(uintptr_t)v);
    }
    unsigned int dist = 1;
    struct recur_list this_item = {depth, v}, *p = depth;
    while (p) {
        if (p->v == v)
            return jl_printf(out, "<circular reference @-%u>", dist);
        dist++;
        p = p->prev;
    }
    return jl_static_show_x_(out, v, (jl_datatype_t*)jl_typeof(v), &this_item);
}

JL_DLLEXPORT size_t jl_static_show(JL_STREAM *out, jl_value_t *v)
{
    return jl_static_show_x(out, v, 0);
}

JL_DLLEXPORT size_t jl_static_show_func_sig(JL_STREAM *s, jl_value_t *type)
{
    jl_value_t *ftype = (jl_value_t*)jl_first_argument_datatype(type);
    if (ftype == NULL)
        return jl_static_show(s, type);
    size_t n = 0;
    if (jl_nparams(ftype) == 0 || ftype == ((jl_datatype_t*)ftype)->name->wrapper) {
        n += jl_printf(s, "%s", jl_symbol_name(((jl_datatype_t*)ftype)->name->mt->name));
    }
    else {
        n += jl_printf(s, "(::");
        n += jl_static_show(s, ftype);
        n += jl_printf(s, ")");
    }
    jl_unionall_t *tvars = (jl_unionall_t*)type;
    type = jl_unwrap_unionall(type);
    if (!jl_is_datatype(type)) {
        n += jl_printf(s, " ");
        n += jl_static_show(s, type);
        return n;
    }
    size_t tl = jl_nparams(type);
    n += jl_printf(s, "(");
    size_t i;
    for (i = 1; i < tl; i++) {
        jl_value_t *tp = jl_tparam(type, i);
        if (i != tl - 1) {
            n += jl_static_show(s, tp);
            n += jl_printf(s, ", ");
        }
        else {
            if (jl_is_vararg_type(tp)) {
                n += jl_static_show(s, jl_unwrap_vararg(tp));
                n += jl_printf(s, "...");
            }
            else {
                n += jl_static_show(s, tp);
            }
        }
    }
    n += jl_printf(s, ")");
    if (jl_is_unionall(tvars)) {
        int first = 1;
        n += jl_printf(s, " where {");
        while (jl_is_unionall(tvars)) {
            if (first)
                first = 0;
            else
                n += jl_printf(s, ", ");
            n += jl_static_show(s, (jl_value_t*)tvars->var);
            tvars = (jl_unionall_t*)tvars->body;
        }
        n += jl_printf(s, "}");
    }
    return n;
}

JL_DLLEXPORT void jl_(void *jl_value)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_jmp_buf *old_buf = ptls->safe_restore;
    jl_jmp_buf buf;
    ptls->safe_restore = &buf;
    if (!jl_setjmp(buf, 0)) {
        jl_static_show((JL_STREAM*)STDERR_FILENO, (jl_value_t*)jl_value);
        jl_printf((JL_STREAM*)STDERR_FILENO,"\n");
    }
    else {
        jl_printf((JL_STREAM*)STDERR_FILENO, "\n!!! ERROR in jl_ -- ABORTING !!!\n");
    }
    ptls->safe_restore = old_buf;
}

JL_DLLEXPORT void jl_breakpoint(jl_value_t *v)
{
    // put a breakpoint in your debugger here
}

// logging tools --------------------------------------------------------------

void jl_log(int level, jl_value_t *module, jl_value_t *group, jl_value_t *id,
            jl_value_t *file, jl_value_t *line, jl_value_t *kwargs,
            jl_value_t *msg)
{
    static jl_value_t *logmsg_func = NULL;
    if (!logmsg_func && jl_base_module) {
        jl_value_t *corelogging = jl_get_global(jl_base_module, jl_symbol("CoreLogging"));
        if (corelogging && jl_is_module(corelogging)) {
            logmsg_func = jl_get_global((jl_module_t*)corelogging, jl_symbol("logmsg_shim"));
        }
    }
    if (!logmsg_func) {
        ios_t str_;
        ios_mem(&str_, 300);
        uv_stream_t* str = (uv_stream_t*)&str_;
        if (jl_is_string(msg)) {
            jl_uv_puts(str, jl_string_data(msg), jl_string_len(msg));
        }
        else if (jl_is_symbol(msg)) {
            jl_printf(str, "%s", jl_symbol_name((jl_sym_t*)msg));
        }
        jl_printf(str, "\n@ ");
        if (jl_is_string(file)) {
            jl_uv_puts(str, jl_string_data(file), jl_string_len(file));
        }
        else if (jl_is_symbol(file)) {
            jl_printf(str, "%s", jl_symbol_name((jl_sym_t*)file));
        }
        jl_printf(str, ":");
        jl_static_show(str, line);
        jl_safe_printf("%s [Fallback logging]: %.*s\n",
                       level < JL_LOGLEVEL_INFO ? "Debug" :
                       level < JL_LOGLEVEL_WARN ? "Info" :
                       level < JL_LOGLEVEL_ERROR ? "Warning" : "Error",
                       (int)str_.size, str_.buf);
        ios_close(&str_);
        return;
    }
    jl_value_t **args;
    const int nargs = 9;
    JL_GC_PUSHARGS(args, nargs);
    args[0] = logmsg_func;
    args[1] = jl_box_long(level);
    args[2] = msg;
    // Would some of the jl_nothing here be better as `missing` instead?
    args[3] = module ? module  : jl_nothing;
    args[4] = group  ? group   : jl_nothing;
    args[5] = id     ? id      : jl_nothing;
    args[6] = file   ? file    : jl_nothing;
    args[7] = line   ? line    : jl_nothing;
    args[8] = kwargs ? kwargs  : (jl_value_t*)jl_alloc_vec_any(0);
    jl_apply(args, nargs);
    JL_GC_POP();
}

void jl_depwarn(const char *msg, jl_value_t *sym)
{
    static jl_value_t *depwarn_func = NULL;
    if (!depwarn_func && jl_base_module) {
        depwarn_func = jl_get_global(jl_base_module, jl_symbol("depwarn"));
    }
    if (!depwarn_func) {
        jl_safe_printf("WARNING: %s\n", msg);
        return;
    }
    jl_value_t **depwarn_args;
    JL_GC_PUSHARGS(depwarn_args, 3);
    depwarn_args[0] = depwarn_func;
    depwarn_args[1] = jl_cstr_to_string(msg);
    depwarn_args[2] = sym;
    jl_apply(depwarn_args, 3);
    JL_GC_POP();
}

JL_DLLEXPORT void jl_depwarn_partial_indexing(size_t n)
{
    static jl_value_t *depwarn_func = NULL;
    if (!depwarn_func && jl_base_module) {
        depwarn_func = jl_get_global(jl_base_module, jl_symbol("_depwarn_for_trailing_indices"));
    }
    if (!depwarn_func) {
        jl_safe_printf("WARNING: omitting indices for non-singleton trailing dimensions is deprecated. Use "
            "`reshape(A, Val(%zd))` or add trailing `1` indices to make the dimensionality of the array match "
            "the number of indices\n", n);
        return;
    }
    jl_value_t **depwarn_args;
    JL_GC_PUSHARGS(depwarn_args, 2);
    depwarn_args[0] = depwarn_func;
    depwarn_args[1] = jl_box_long(n);
    jl_apply(depwarn_args, 2);
    JL_GC_POP();
}

#ifdef __cplusplus
}
#endif
