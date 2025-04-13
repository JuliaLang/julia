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

jl_value_t *jl_vexceptionf(jl_datatype_t *exception_type,
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

// with function name / location description, plus extra context
JL_DLLEXPORT void JL_NORETURN jl_type_error_rt(const char *fname, const char *context,
                                               jl_value_t *expected JL_MAYBE_UNROOTED,
                                               jl_value_t *got JL_MAYBE_UNROOTED)
{
    jl_value_t *ctxt=NULL;
    JL_GC_PUSH3(&ctxt, &expected, &got);
    ctxt = jl_pchar_to_string((char*)context, strlen(context));
    jl_value_t *ex = jl_new_struct(jl_typeerror_type, jl_symbol(fname), ctxt, expected, got);
    jl_throw(ex);
}

// with function name or description only
JL_DLLEXPORT void JL_NORETURN jl_type_error(const char *fname,
                                            jl_value_t *expected JL_MAYBE_UNROOTED,
                                            jl_value_t *got JL_MAYBE_UNROOTED)
{
    jl_type_error_rt(fname, "", expected, got);
}

JL_DLLEXPORT void JL_NORETURN jl_undefined_var_error(jl_sym_t *var, jl_value_t *scope)
{
    if (!jl_undefvarerror_type) {
        const char *s1 = "";
        const char *s2 = "";
        if (scope) {
            if (jl_is_symbol(scope)) {
                s1 = ", :";
                s2 = jl_symbol_name((jl_sym_t*)scope);
            }
            else if (jl_is_module(scope)) {
                s1 = ", module ";
                s2 = jl_symbol_name(((jl_module_t*)scope)->name);
            }
            else {
                s1 = ", ";
                s2 = "unknown scope";
            }
        }
        jl_errorf("UndefVarError(%s%s%s)", jl_symbol_name(var), s1, s2);
    }
    jl_value_t *active_age = NULL;
    JL_GC_PUSH2(&scope, &active_age);
    active_age = jl_box_long(jl_current_task->world_age);
    jl_throw(jl_new_struct(jl_undefvarerror_type, var, active_age, scope));
}

JL_DLLEXPORT void JL_NORETURN jl_has_no_field_error(jl_datatype_t *t, jl_sym_t *var)
{
    jl_throw(jl_new_struct(jl_fielderror_type, t, var));
}

JL_DLLEXPORT void JL_NORETURN jl_argument_error(char *str) // == jl_exceptionf(jl_argumenterror_type, "%s", str)
{
    jl_value_t *msg = jl_pchar_to_string((char*)str, strlen(str));
    JL_GC_PUSH1(&msg);
    jl_throw(jl_new_struct(jl_argumenterror_type, msg));
}

JL_DLLEXPORT void JL_NORETURN jl_atomic_error(char *str) // == jl_exceptionf(jl_atomicerror_type, "%s", str)
{
    jl_value_t *msg = jl_pchar_to_string((char*)str, strlen(str));
    JL_GC_PUSH1(&msg);
    jl_throw(jl_new_struct(jl_atomicerror_type, msg));
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

JL_DLLEXPORT void JL_NORETURN jl_bounds_error_int(jl_value_t *v JL_MAYBE_UNROOTED, size_t i)
{
    jl_value_t *t = NULL;
    JL_GC_PUSH2(&v, &t); // root arguments so the caller doesn't need to
    t = jl_box_long(i);
    jl_throw(jl_new_struct((jl_datatype_t*)jl_boundserror_type, v, t));
}

JL_DLLEXPORT void JL_NORETURN jl_bounds_error_ints(jl_value_t *v JL_MAYBE_UNROOTED,
                                                   size_t *idxs, size_t nidxs)
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

JL_DLLEXPORT void jl_typeassert(jl_value_t *x, jl_value_t *t)
{
    if (!jl_isa(x,t))
        jl_type_error("typeassert", t, x);
}

#ifndef HAVE_SSP
JL_DLLEXPORT uintptr_t __stack_chk_guard = (uintptr_t)0xBAD57ACCBAD67ACC; // 0xBADSTACKBADSTACK

JL_DLLEXPORT void __stack_chk_fail(void)
{
    /* put your panic function or similar in here */
    fprintf(stderr, "fatal error: stack corruption detected\n");
    jl_gc_debug_critical_error();
    abort(); // end with abort, since the compiler destroyed the stack upon entry to this function, there's no going back now
}
#endif

// exceptions -----------------------------------------------------------------

JL_DLLEXPORT void jl_enter_handler(jl_task_t *ct, jl_handler_t *eh)
{
    // Must have no safepoint
    eh->prev = ct->eh;
    eh->gcstack = ct->gcstack;
    eh->scope = ct->scope;
    eh->gc_state = jl_atomic_load_relaxed(&ct->ptls->gc_state);
    eh->locks_len = ct->ptls->locks.len;
    eh->defer_signal = ct->ptls->defer_signal;
    eh->world_age = ct->world_age;
#ifdef ENABLE_TIMINGS
    eh->timing_stack = ct->ptls->timing_stack;
#endif
}

// Restore thread local state to saved state in error handler `eh`.
// This is executed in two circumstances:
// * We leave a try block through normal control flow
// * An exception causes a nonlocal jump to the catch block. In this case
//   there's additional cleanup required, eg pushing the exception stack.
JL_DLLEXPORT void jl_eh_restore_state(jl_task_t *ct, jl_handler_t *eh)
{
#ifdef _OS_WINDOWS_
    if (ct->ptls->needs_resetstkoflw) {
        _resetstkoflw();
        ct->ptls->needs_resetstkoflw = 0;
    }
#endif
    // `eh` may be not equal to `ct->eh`. See `jl_pop_handler`
    // This function should **NOT** have any safepoint before the ones at the
    // end.
    jl_ptls_t ptls = ct->ptls;
    sig_atomic_t old_defer_signal = ptls->defer_signal;
    ct->eh = eh->prev;
    ct->gcstack = eh->gcstack;
    ct->scope = eh->scope;
    small_arraylist_t *locks = &ptls->locks;
    int unlocks = locks->len > eh->locks_len;
    if (unlocks) {
        for (size_t i = locks->len; i > eh->locks_len; i--)
            jl_mutex_unlock_nogc((jl_mutex_t*)locks->items[i - 1]);
        locks->len = eh->locks_len;
    }
    ct->world_age = eh->world_age;
    ptls->defer_signal = eh->defer_signal;
    int8_t old_gc_state = jl_atomic_load_relaxed(&ptls->gc_state);
    if (old_gc_state != eh->gc_state)
        jl_atomic_store_release(&ptls->gc_state, eh->gc_state);
    if (!old_gc_state || !eh->gc_state) // it was or is unsafe now
        jl_gc_safepoint_(ptls);
    jl_value_t *exception = ptls->sig_exception;
    JL_GC_PROMISE_ROOTED(exception);
    if (exception) {
        int8_t oldstate = jl_gc_unsafe_enter(ptls);
        /* The temporary ptls->bt_data is rooted by special purpose code in the
        GC. This exists only for the purpose of preserving bt_data until we
        set ptls->bt_size=0 below. */
        jl_push_excstack(ct, &ct->excstack, exception,
                         ptls->bt_data, ptls->bt_size);
        ptls->bt_size = 0;
        ptls->sig_exception = NULL;
        jl_gc_unsafe_leave(ptls, oldstate);
    }
    if (old_defer_signal && !eh->defer_signal)
        jl_sigint_safepoint(ptls);
    if (jl_atomic_load_relaxed(&jl_gc_have_pending_finalizers) &&
            unlocks && eh->locks_len == 0) {
        jl_gc_run_pending_finalizers(ct);
    }
}

JL_DLLEXPORT void jl_eh_restore_state_noexcept(jl_task_t *ct, jl_handler_t *eh)
{
    assert(ct->gcstack == eh->gcstack && "Incorrect GC usage under try catch");
    ct->scope = eh->scope;
    ct->eh = eh->prev;
    ct->ptls->defer_signal = eh->defer_signal; // optional, but certain try-finally (in stream.jl) may be slightly harder to write without this
}

JL_DLLEXPORT void jl_pop_handler(jl_task_t *ct, int n)
{
    if (__unlikely(n <= 0))
        return;
    jl_handler_t *eh = ct->eh;
    while (--n > 0)
        eh = eh->prev;
    jl_eh_restore_state(ct, eh);
}

JL_DLLEXPORT void jl_pop_handler_noexcept(jl_task_t *ct, int n)
{
    if (__unlikely(n <= 0))
        return;
    jl_handler_t *eh = ct->eh;
    while (--n > 0)
        eh = eh->prev;
    jl_eh_restore_state_noexcept(ct, eh);
}

JL_DLLEXPORT size_t jl_excstack_state(jl_task_t *ct) JL_NOTSAFEPOINT
{
    jl_excstack_t *s = ct->excstack;
    return s ? s->top : 0;
}

JL_DLLEXPORT void jl_restore_excstack(jl_task_t *ct, size_t state) JL_NOTSAFEPOINT
{
    jl_excstack_t *s = ct->excstack;
    if (s) {
        assert(s->top >= state);
        s->top = state;
    }
}

static void jl_copy_excstack(jl_excstack_t *dest, jl_excstack_t *src) JL_NOTSAFEPOINT
{
    assert(dest->reserved_size >= src->top);
    memcpy(jl_excstack_raw(dest), jl_excstack_raw(src), sizeof(jl_bt_element_t)*src->top);
    dest->top = src->top;
}

static void jl_reserve_excstack(jl_task_t *ct, jl_excstack_t **stack JL_REQUIRE_ROOTED_SLOT,
                                size_t reserved_size)
{
    jl_excstack_t *s = *stack;
    if (s && s->reserved_size >= reserved_size)
        return;
    size_t bufsz = sizeof(jl_excstack_t) + sizeof(uintptr_t)*reserved_size;
    jl_excstack_t *new_s = (jl_excstack_t*)jl_gc_alloc_buf(ct->ptls, bufsz);
    new_s->top = 0;
    new_s->reserved_size = reserved_size;
    if (s)
        jl_copy_excstack(new_s, s);
    *stack = new_s;
    jl_gc_wb(ct, new_s);
}

void jl_push_excstack(jl_task_t *ct, jl_excstack_t **stack JL_REQUIRE_ROOTED_SLOT JL_ROOTING_ARGUMENT,
                      jl_value_t *exception JL_ROOTED_ARGUMENT,
                      jl_bt_element_t *bt_data, size_t bt_size)
{
    jl_reserve_excstack(ct, stack, (*stack ? (*stack)->top : 0) + bt_size + 2);
    jl_excstack_t *s = *stack;
    jl_bt_element_t *rawstack = jl_excstack_raw(s);
    memcpy(rawstack + s->top, bt_data, sizeof(jl_bt_element_t)*bt_size);
    s->top += bt_size + 2;
    rawstack[s->top-2].uintptr = bt_size;
    rawstack[s->top-1].jlvalue = exception;
}

// conversion -----------------------------------------------------------------

JL_DLLEXPORT void *(jl_symbol_name)(jl_sym_t *s)
{
    return jl_symbol_name(s);
}

// WARNING: THIS FUNCTION IS NEVER CALLED BUT INLINE BY CCALL
JL_DLLEXPORT void *jl_array_ptr(jl_array_t *a)
{
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(a->ref.mem))->layout;
    if (layout->flags.arrayelem_isunion || layout->size == 0)
        return (char*)a->ref.mem->ptr + (size_t)jl_array_data_(a);
    return jl_array_data_(a);
}
JL_DLLEXPORT jl_value_t *jl_value_ptr(jl_value_t *a)
{
    return a;
}

// optimization of setfield which bypasses boxing of the idx (and checking field type validity)
JL_DLLEXPORT void jl_set_nth_field(jl_value_t *v, size_t idx0, jl_value_t *rhs)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    if (!st->name->mutabl)
        jl_errorf("setfield!: immutable struct of type %s cannot be changed", jl_symbol_name(st->name->name));
    if (idx0 >= jl_datatype_nfields(st))
        jl_bounds_error_int(v, idx0 + 1);
    //jl_value_t *ft = jl_field_type(st, idx0);
    //if (!jl_isa(rhs, ft)) {
    //    jl_type_error("setfield!", ft, rhs);
    //}
    //int isatomic = jl_field_isatomic(st, idx0);
    //if (isatomic) ...
    set_nth_field(st, v, idx0, rhs, 0);
}


// parsing --------------------------------------------------------------------

static int substr_isspace(char *p, char *pend)
{
    while (p != pend) {
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
            newstr = tofree = (char*)malloc_s(len + 1);
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
            newstr = tofree = (char*)malloc_s(len + 1);
        }
        memcpy(newstr, bstr, len);
        newstr[len] = 0;
        bstr = newstr;
        pend = bstr+len;
    }
#if defined(_OS_WINDOWS_) && !defined(_COMPILER_GCC_)
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

JL_DLLEXPORT void jl_flush_cstdio(void) JL_NOTSAFEPOINT
{
    fflush(stdout);
    fflush(stderr);
}

JL_DLLEXPORT jl_value_t *jl_stderr_obj(void) JL_NOTSAFEPOINT
{
    if (jl_base_module == NULL)
        return NULL;
    jl_binding_t *stderr_obj = jl_get_module_binding(jl_base_module, jl_symbol("stderr"), 0);
    return stderr_obj ? jl_get_binding_value_if_resolved_debug_only(stderr_obj) : NULL;
}

// toys for debugging ---------------------------------------------------------

struct recur_list {
    struct recur_list *prev;
    jl_value_t *v;
};

static size_t jl_static_show_x(JL_STREAM *out, jl_value_t *v, struct recur_list *depth, jl_static_show_config_t ctx) JL_NOTSAFEPOINT;
static size_t jl_static_show_x_(JL_STREAM *out, jl_value_t *v, jl_datatype_t *vt, struct recur_list *depth, jl_static_show_config_t ctx) JL_NOTSAFEPOINT;
static size_t jl_static_show_next_(JL_STREAM *out, jl_value_t *v, jl_value_t *prev, struct recur_list *depth, jl_static_show_config_t ctx) JL_NOTSAFEPOINT;

static size_t jl_show_svec(JL_STREAM *out, jl_svec_t *t, const char *head, const char *opn, const char *cls, jl_static_show_config_t ctx) JL_NOTSAFEPOINT
{
    size_t i, n=0, len = jl_svec_len(t);
    n += jl_printf(out, "%s", head);
    n += jl_printf(out, "%s", opn);
    for (i = 0; i < len; i++) {
        jl_value_t *v = jl_svecref(t,i);
        n += jl_static_show_x(out, v, 0, ctx);
        if (i != len-1)
            n += jl_printf(out, ", ");
    }
    n += jl_printf(out, "%s", cls);
    return n;
}

JL_DLLEXPORT int jl_id_start_char(uint32_t wc) JL_NOTSAFEPOINT;
JL_DLLEXPORT int jl_id_char(uint32_t wc) JL_NOTSAFEPOINT;

JL_DLLEXPORT int jl_is_identifier(const char *str) JL_NOTSAFEPOINT
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

static jl_datatype_t *nth_arg_datatype(jl_value_t *a JL_PROPAGATES_ROOT, int n) JL_NOTSAFEPOINT
{
    if (jl_is_datatype(a)) {
        if (n == 0)
            return (jl_datatype_t*)a;
        if (jl_is_tuple_type(a)) {
            if (jl_nparams(a) < n)
                return NULL;
            return nth_arg_datatype(jl_tparam(a, n - 1), 0);
        }
        return NULL;
    }
    else if (jl_is_typevar(a)) {
        return nth_arg_datatype(((jl_tvar_t*)a)->ub, n);
    }
    else if (jl_is_unionall(a)) {
        return nth_arg_datatype(((jl_unionall_t*)a)->body, n);
    }
    else if (jl_is_uniontype(a)) {
        jl_uniontype_t *u = (jl_uniontype_t*)a;
        jl_datatype_t *d1 = nth_arg_datatype(u->a, n);
        if (d1 == NULL) return NULL;
        jl_datatype_t *d2 = nth_arg_datatype(u->b, n);
        if (d2 == NULL || d1->name != d2->name)
            return NULL;
        return d1;
    }
    return NULL;
}

// get DataType of first tuple element (if present), or NULL if cannot be determined
jl_datatype_t *jl_nth_argument_datatype(jl_value_t *argtypes JL_PROPAGATES_ROOT, int n) JL_NOTSAFEPOINT
{
    return nth_arg_datatype(argtypes, n);
}

// get DataType implied by a single given type, or `nothing`
JL_DLLEXPORT jl_value_t *jl_argument_datatype(jl_value_t *argt JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    jl_datatype_t *dt = nth_arg_datatype(argt, 0);
    if (dt == NULL)
        return jl_nothing;
    return (jl_value_t*)dt;
}

static int is_globname_binding(jl_value_t *v, jl_datatype_t *dv) JL_NOTSAFEPOINT
{
    jl_sym_t *globname = dv->name->mt != NULL ? dv->name->mt->name : NULL;
    if (globname && dv->name->module) {
        jl_binding_t *b = jl_get_module_binding(dv->name->module, globname, 0);
        jl_value_t *bv = jl_get_binding_value_if_latest_resolved_and_const_debug_only(b);
        // The `||` makes this function work for both function instances and function types.
        if (bv && (bv == v || jl_typeof(bv) == v))
            return 1;
    }
    return 0;
}

static int is_globfunction(jl_value_t *v, jl_datatype_t *dv, jl_sym_t **globname_out) JL_NOTSAFEPOINT
{
    jl_sym_t *globname = dv->name->mt != NULL ? dv->name->mt->name : NULL;
    *globname_out = globname;
    if (globname && !strchr(jl_symbol_name(globname), '#') && !strchr(jl_symbol_name(globname), '@')) {
        return 1;
    }
    return 0;
}

static size_t jl_static_show_string(JL_STREAM *out, const char *str, size_t len, int wrap) JL_NOTSAFEPOINT
{
    size_t n = 0;
    if (wrap)
        n += jl_printf(out, "\"");
    if (!u8_isvalid(str, len)) {
        // alternate print algorithm that preserves data if it's not UTF-8
        static const char hexdig[] = "0123456789abcdef";
        for (size_t i = 0; i < len; i++) {
            uint8_t c = str[i];
            if (c == '\\' || c == '"' || c == '$')
                n += jl_printf(out, "\\%c", c);
            else if (c >= 32 && c < 0x7f)
                n += jl_printf(out, "%c", c);
            else
                n += jl_printf(out, "\\x%c%c", hexdig[c>>4], hexdig[c&0xf]);
        }
    }
    else {
        int special = 0;
        for (size_t i = 0; i < len; i++) {
            uint8_t c = str[i];
            if (c < 32 || c == 0x7f || c == '\\' || c == '"' || c == '$') {
                special = 1;
                break;
            }
        }
        if (!special) {
            jl_uv_puts(out, str, len);
            n += len;
        }
        else {
            char buf[512];
            size_t i = 0;
            while (i < len) {
                size_t r = u8_escape(buf, sizeof(buf), str, &i, len, "\"$", 0);
                jl_uv_puts(out, buf, r - 1);
                n += r - 1;
            }
        }
    }
    if (wrap)
        n += jl_printf(out, "\"");
    return n;
}

static size_t jl_static_show_symbol(JL_STREAM *out, jl_sym_t *name) JL_NOTSAFEPOINT
{
    size_t n = 0;
    const char *sn = jl_symbol_name(name);
    int quoted = !jl_is_identifier(sn) && !jl_is_operator(sn);
    if (quoted) {
        n += jl_printf(out, "var");
        // TODO: this is not quite right, since repr uses String escaping rules, and Symbol uses raw string rules
        n += jl_static_show_string(out, sn, strlen(sn), 1);
    }
    else {
        n += jl_printf(out, "%s", sn);
    }
    return n;
}

// `jl_static_show()` cannot call `jl_subtype()`, for the GC reasons
// explained in the comment on `jl_static_show_x_()`, below.
// This function checks if `vt <: Function` without triggering GC.
static int jl_static_is_function_(jl_datatype_t *vt) JL_NOTSAFEPOINT {
    if (!jl_function_type) {  // Make sure there's a Function type defined.
        return 0;
    }
    int _iter_count = 0;  // To prevent infinite loops from corrupt type objects.
    while (vt != jl_any_type) {
        if (vt == NULL) {
            return 0;
        } else if (_iter_count > 10000) {
            // We are very likely stuck in a cyclic datastructure, so we assume this is
            // _not_ a Function.
            return 0;
        } else if (vt == jl_function_type) {
            return 1;
        }
        vt = vt->super;
        _iter_count += 1;
    }
    return 0;
}

// `v` might be pointing to a field inlined in a structure therefore
// `jl_typeof(v)` may not be the same with `vt` and only `vt` should be
// used to determine the type of the value.
// This is necessary to make sure that this function doesn't allocate any
// memory through the Julia GC
static size_t jl_static_show_x_(JL_STREAM *out, jl_value_t *v, jl_datatype_t *vt,
                                struct recur_list *depth, jl_static_show_config_t ctx) JL_NOTSAFEPOINT
{
    size_t n = 0;
    if ((uintptr_t)vt < 4096U) {
        n += jl_printf(out, "<?#%p::%p>", (void*)v, (void*)vt);
    }
    else if ((uintptr_t)v < 4096U) {
        n += jl_printf(out, "<?#%p::", (void*)v);
        n += jl_static_show_x(out, (jl_value_t*)vt, depth, ctx);
        n += jl_printf(out, ">");
    }
    else if (vt == (jl_datatype_t*)jl_buff_tag) {
        n += jl_printf(out, "<?#%p::jl_buff_tag marked memory>", (void*)v);
    }
    else if (vt == (jl_datatype_t*)(uintptr_t)(0xbabababababababaull & ~15)) {
        n += jl_printf(out, "<?#%p::baaaaaad>", (void*)v);
    }
    // These need to be special cased because they
    // exist only by pointer identity in early startup
    else if (v == (jl_value_t*)jl_simplevector_type) {
        n += jl_printf(out, "Core.SimpleVector");
    }
    else if (v == (jl_value_t*)jl_typename_type) {
        n += jl_printf(out, "Core.TypeName");
    }
    else if (v == (jl_value_t*)jl_symbol_type) {
        n += jl_printf(out, "Symbol");
    }
    else if (v == (jl_value_t*)jl_methtable_type) {
        n += jl_printf(out, "Core.MethodTable");
    }
    else if (v == (jl_value_t*)jl_any_type) {
        n += jl_printf(out, "Any");
    }
    else if (v == (jl_value_t*)jl_type_type) {
        n += jl_printf(out, "Type");
    }
    else if (vt == jl_method_type) {
        jl_method_t *m = (jl_method_t*)v;
        n += jl_static_show_func_sig(out, m->sig);
    }
    else if (vt == jl_method_instance_type) {
        jl_method_instance_t *li = (jl_method_instance_t*)v;
        if (jl_is_method(li->def.method)) {
            n += jl_static_show_func_sig(out, li->specTypes);
            n += jl_printf(out, " from ");
            n += jl_static_show_func_sig(out, li->def.method->sig);
        }
        else {
            n += jl_static_show_x(out, (jl_value_t*)li->def.module, depth, ctx);
            n += jl_printf(out, ".<toplevel thunk> -> ");
            n += jl_static_show_x(out, jl_atomic_load_relaxed(&jl_cached_uninferred(
                jl_atomic_load_relaxed(&li->cache), 1)->inferred), depth, ctx);
        }
    }
    else if (vt == jl_typename_type) {
        n += jl_static_show_x(out, jl_unwrap_unionall(((jl_typename_t*)v)->wrapper), depth, ctx);
        n += jl_printf(out, ".name");
    }
    else if (vt == jl_simplevector_type) {
        n += jl_show_svec(out, (jl_svec_t*)v, "svec", "(", ")", ctx);
    }
    else if (v == (jl_value_t*)jl_unionall_type) {
        // avoid printing `typeof(Type)` for `UnionAll`.
        n += jl_printf(out, "UnionAll");
    }
    else if (vt == jl_vararg_type) {
        jl_vararg_t *vm = (jl_vararg_t*)v;
        n += jl_printf(out, "Vararg");
        if (vm->T) {
            n += jl_printf(out, "{");
            n += jl_static_show_x(out, vm->T, depth, ctx);
            if (vm->N) {
                n += jl_printf(out, ", ");
                n += jl_static_show_x(out, vm->N, depth, ctx);
            }
            n += jl_printf(out, "}");
        }
    }
    else if (vt == jl_datatype_type) {
        // typeof(v) == DataType, so v is a Type object.
        // Types are printed as a fully qualified name, with parameters, e.g.
        // `Base.Set{Int}`, and function types are printed as e.g. `typeof(Main.f)`
        jl_datatype_t *dv = (jl_datatype_t*)v;
        if (dv->name == jl_tuple_typename) {
            if (dv == jl_tuple_type)
                return jl_printf(out, "Tuple");
            int taillen = 1, tlen = jl_nparams(dv), i;
            for (i = tlen-2; i >= 0; i--) {
                if (jl_tparam(dv, i) == jl_tparam(dv, tlen-1))
                    taillen++;
                else
                    break;
            }
            if (taillen == tlen && taillen > 3) {
                n += jl_printf(out, "NTuple{%d, ", tlen);
                n += jl_static_show_x(out, jl_tparam0(dv), depth, ctx);
                n += jl_printf(out, "}");
            }
            else {
                n += jl_printf(out, "Tuple{");
                for (i = 0; i < (taillen > 3 ? tlen-taillen : tlen); i++) {
                    if (i > 0)
                        n += jl_printf(out, ", ");
                    n += jl_static_show_x(out, jl_tparam(dv, i), depth, ctx);
                }
                if (taillen > 3) {
                    n += jl_printf(out, ", Vararg{");
                    n += jl_static_show_x(out, jl_tparam(dv, tlen-1), depth, ctx);
                    n += jl_printf(out, ", %d}", taillen);
                }
                n += jl_printf(out, "}");
            }
            return n;
        }
        if (jl_genericmemory_type && dv->name == jl_genericmemory_typename) {
            jl_value_t *isatomic = jl_tparam0(dv);
            jl_value_t *el_type = jl_tparam1(dv);
            jl_value_t *addrspace = jl_tparam2(dv);
            if (isatomic == (jl_value_t*)jl_not_atomic_sym && addrspace && jl_is_addrspacecore(addrspace) && jl_unbox_uint8(addrspace) == 0) {
                n += jl_printf(out, "Memory{");
                n += jl_static_show_x(out, el_type, depth, ctx);
                n += jl_printf(out, "}");
                return n;
            }
        }
        if (ctx.quiet) {
            return jl_static_show_symbol(out, dv->name->name);
        }
        jl_sym_t *globname;
        int globfunc = is_globname_binding(v, dv) && is_globfunction(v, dv, &globname);
        jl_sym_t *sym = globfunc ? globname : dv->name->name;
        char *sn = jl_symbol_name(sym);
        size_t quote = 0;
        if (globfunc) {
            n += jl_printf(out, "typeof(");
        }
        if (jl_core_module && (dv->name->module != jl_core_module || !jl_module_exports_p(jl_core_module, sym))) {
            n += jl_static_show_x(out, (jl_value_t*)dv->name->module, depth, ctx);
            n += jl_printf(out, ".");
            size_t i = 0;
            if (globfunc && !jl_id_start_char(u8_nextchar(sn, &i))) {
                n += jl_printf(out, ":(");
                quote = 1;
            }
        }
        n += jl_static_show_symbol(out, sym);
        if (globfunc) {
            n += jl_printf(out, ")");
            if (quote) {
                n += jl_printf(out, ")");
            }
        }
        if (dv->parameters && (jl_value_t*)dv != dv->name->wrapper) {
            size_t j, tlen = jl_nparams(dv);
            if (tlen > 0) {
                n += jl_printf(out, "{");
                for (j = 0; j < tlen; j++) {
                    jl_value_t *p = jl_tparam(dv,j);
                    n += jl_static_show_x(out, p, depth, ctx);
                    if (j != tlen-1)
                        n += jl_printf(out, ", ");
                }
                n += jl_printf(out, "}");
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
    else if (jl_pointer_type && jl_is_cpointer_type((jl_value_t*)vt)) {
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
    else if (v == jl_nothing || (jl_nothing && (jl_value_t*)vt == jl_typeof(jl_nothing))) {
        n += jl_printf(out, "nothing");
    }
    else if (vt == jl_string_type) {
        n += jl_static_show_string(out, jl_string_data(v), jl_string_len(v), 1);
    }
    else if (v == jl_bottom_type) {
        n += jl_printf(out, "Union{}");
    }
    else if (vt == jl_uniontype_type) {
        n += jl_printf(out, "Union{");
        while (jl_is_uniontype(v)) {
            // tail-recurse on b to flatten the printing of the Union structure in the common case
            n += jl_static_show_x(out, ((jl_uniontype_t*)v)->a, depth, ctx);
            n += jl_printf(out, ", ");
            v = ((jl_uniontype_t*)v)->b;
        }
        n += jl_static_show_x(out, v, depth, ctx);
        n += jl_printf(out, "}");
    }
    else if (vt == jl_unionall_type) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        n += jl_static_show_x(out, ua->body, depth, ctx);
        n += jl_printf(out, " where ");
        n += jl_static_show_x(out, (jl_value_t*)ua->var, depth->prev, ctx);
    }
    else if (vt == jl_typename_type) {
        n += jl_printf(out, "typename(");
        n += jl_static_show_x(out, jl_unwrap_unionall(((jl_typename_t*)v)->wrapper), depth, ctx);
        n += jl_printf(out, ")");
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
            n += jl_static_show_x(out, lb, depth, ctx);
            if (ua)
                n += jl_printf(out, ")");
            n += jl_printf(out, "<:");
        }
        n += jl_static_show_symbol(out, var->name);
        if (showbounds && (ub != (jl_value_t*)jl_any_type || lb != jl_bottom_type)) {
            // show type-var upper bound if it is defined, or if we showed the lower bound
            int ua = jl_is_unionall(ub);
            n += jl_printf(out, "<:");
            if (ua)
                n += jl_printf(out, "(");
            n += jl_static_show_x(out, ub, depth, ctx);
            if (ua)
                n += jl_printf(out, ")");
        }
    }
    else if (vt == jl_module_type) {
        jl_module_t *m = (jl_module_t*)v;
        if (m->parent != m && m->parent != jl_main_module) {
            n += jl_static_show_x(out, (jl_value_t*)m->parent, depth, ctx);
            n += jl_printf(out, ".");
        }
        n += jl_static_show_symbol(out, m->name);
    }
    else if (vt == jl_symbol_type) {
        n += jl_printf(out, ":");
        n += jl_static_show_symbol(out, (jl_sym_t*)v);
    }
    else if (vt == jl_ssavalue_type) {
        n += jl_printf(out, "SSAValue(%" PRIuPTR ")",
                       (uintptr_t)((jl_ssavalue_t*)v)->id);
    }
    else if (vt == jl_globalref_type) {
        n += jl_static_show_x(out, (jl_value_t*)jl_globalref_mod(v), depth, ctx);
        jl_sym_t *name = jl_globalref_name(v);
        n += jl_printf(out, ".");
        if (jl_is_operator(jl_symbol_name(name)))
            n += jl_printf(out, ":(%s)", jl_symbol_name(name));
        else
            n += jl_static_show_symbol(out, name);
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
        n += jl_static_show_x(out, qv, depth, ctx);
        if (!jl_is_symbol(qv)) {
            n += jl_printf(out, " end");
        }
        else {
            n += jl_printf(out, ")");
        }
    }
    else if (vt == jl_newvarnode_type) {
        n += jl_printf(out, "<newvar ");
        n += jl_static_show_x(out, *(jl_value_t**)v, depth, ctx);
        n += jl_printf(out, ">");
    }
    else if (vt == jl_linenumbernode_type) {
        n += jl_printf(out, "#= ");
        n += jl_static_show_x(out, jl_linenode_file(v), depth, ctx);
        n += jl_printf(out, ":%" PRIuPTR " =#", jl_linenode_line(v));
    }
    else if (vt == jl_expr_type) {
        jl_expr_t *e = (jl_expr_t*)v;
        if (e->head == jl_assign_sym && jl_array_nrows(e->args) == 2) {
            n += jl_static_show_x(out, jl_exprarg(e, 0), depth, ctx);
            n += jl_printf(out, " = ");
            n += jl_static_show_x(out, jl_exprarg(e, 1), depth, ctx);
        }
        else {
            n += jl_printf(out, "Expr(");
            n += jl_static_show_x(out, (jl_value_t*)e->head, depth, ctx);
            size_t i, len = jl_array_nrows(e->args);
            for (i = 0; i < len; i++) {
                n += jl_printf(out, ", ");
                n += jl_static_show_x(out, jl_exprarg(e, i), depth, ctx);
            }
            n += jl_printf(out, ")");
        }
    }
    else if (jl_array_type && jl_is_array_type(vt)) {
        n += jl_printf(out, "Array{");
        jl_value_t *el_type = jl_tparam0(vt);
        n += jl_static_show_x(out, el_type, depth, ctx);
        jl_array_t *av = (jl_array_t*)v;
        size_t i, ndims = jl_array_ndims(v);
        n += jl_printf(out, ", %" PRIdPTR "}(dims=(", ndims);
        if (ndims == 1)
            n += jl_printf(out, "%" PRIdPTR ",", jl_array_dim0(v));
        else
            for (i = 0; i < ndims; i++)
                n += jl_printf(out, (i > 0 ? ", %" PRIdPTR : "%" PRIdPTR), jl_array_dim(v, i));
        n += jl_printf(out, "), mem=");
        n += jl_static_show_x(out, (jl_value_t*)av->ref.mem, depth, ctx);
        n += jl_printf(out, ")");
    }
    else if (jl_genericmemoryref_type && jl_is_genericmemoryref_type(vt)) {
        jl_genericmemoryref_t *ref = (jl_genericmemoryref_t*)v;
        n += jl_printf(out, "GenericMemoryRef(offset=");
        size_t offset = (size_t)ref->ptr_or_offset;
        if (ref->mem) {
            const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typeof(ref->mem))->layout;
            if (layout->size != 0 && !layout->flags.arrayelem_isunion)
                offset = ((char*)offset - (char*)ref->mem->ptr) / layout->size;
        }
        n += jl_printf(out, "%" PRIdPTR, offset);
        n += jl_printf(out, ", ptr_or_offset=%p, mem=", ref->ptr_or_offset);
        n += jl_static_show_x(out, (jl_value_t*)ref->mem, depth, ctx);
    }
    else if (jl_genericmemory_type && jl_is_genericmemory_type(vt)) {
        jl_genericmemory_t *m = (jl_genericmemory_t*)v;
        //jl_value_t *isatomic = jl_tparam0(vt);
        jl_value_t *el_type = jl_tparam1(vt);
        jl_value_t *addrspace = jl_tparam2(vt);
        n += jl_static_show_x(out, (jl_value_t*)vt, depth, ctx);
        size_t j, tlen = m->length;
        n += jl_printf(out, "(%" PRIdPTR ", %p)[", tlen, m->ptr);
        if (!(addrspace && jl_is_addrspacecore(addrspace) && jl_unbox_uint8(addrspace) == 0)) {
            n += jl_printf(out, "...]");
            return n;
        }
        const char *typetagdata = NULL;
        const jl_datatype_layout_t *layout = vt->layout;
        int nlsep = 0;
        if (layout->flags.arrayelem_isboxed) {
            // print arrays with newlines, unless the elements are probably small
            for (j = 0; j < tlen; j++) {
                jl_value_t **ptr = ((jl_value_t**)m->ptr) + j;
                jl_value_t *p = *ptr;
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
        else if (layout->flags.arrayelem_isunion) {
            typetagdata = jl_genericmemory_typetagdata(m);
        }
        if (layout->size == 0 && tlen >= 3) {
            n += jl_static_show_x_(out, (jl_value_t*)m->ptr, (jl_datatype_t*)el_type, depth, ctx);
            n += jl_printf(out, ", ...");
        }
        else {
            if (nlsep && tlen > 1)
                n += jl_printf(out, "\n  ");
            for (size_t j = 0; j < tlen; j++) {
                if (layout->flags.arrayelem_isboxed) {
                    jl_value_t **ptr = ((jl_value_t**)m->ptr) + j;
                    n += jl_static_show_x(out, *ptr, depth, ctx);
                }
                else {
                    char *ptr = ((char*)m->ptr) + j * layout->size;
                    n += jl_static_show_x_(out, (jl_value_t*)ptr,
                            (jl_datatype_t*)(typetagdata ? jl_nth_union_component(el_type, typetagdata[j]) : el_type),
                            depth, ctx);
                }
                if (j != tlen - 1)
                    n += jl_printf(out, nlsep ? ",\n  " : ", ");
            }
        }
        n += jl_printf(out, "]");
    }
    else if (vt == jl_loaderror_type) {
        n += jl_printf(out, "LoadError(at ");
        n += jl_static_show_x(out, *(jl_value_t**)v, depth, ctx);
        // Access the field directly to avoid allocation
        n += jl_printf(out, " line %" PRIdPTR, ((intptr_t*)v)[1]);
        n += jl_printf(out, ": ");
        n += jl_static_show_x(out, ((jl_value_t**)v)[2], depth, ctx);
        n += jl_printf(out, ")");
    }
    else if (vt == jl_errorexception_type) {
        n += jl_printf(out, "ErrorException(");
        n += jl_static_show_x(out, *(jl_value_t**)v, depth, ctx);
        n += jl_printf(out, ")");
    }
    else if (jl_static_is_function_(vt) && is_globname_binding(v, (jl_datatype_t*)vt)) {
        // v is function instance (an instance of a Function type).
        jl_datatype_t *dv = (jl_datatype_t*)vt;
        jl_sym_t *sym;
        int globfunc = is_globfunction(v, dv, &sym);
        int quote = 0;
        if (jl_core_module && (dv->name->module != jl_core_module || !jl_module_exports_p(jl_core_module, sym))) {
            n += jl_static_show_x(out, (jl_value_t*)dv->name->module, depth, ctx);
            n += jl_printf(out, ".");

            size_t i = 0;
            char *sn = jl_symbol_name(sym);
            if (globfunc && !jl_id_start_char(u8_nextchar(sn, &i))) {
                n += jl_printf(out, ":(");
                quote = 1;
            }
        }

        n += jl_static_show_symbol(out, sym);

        if (globfunc) {
            if (quote) {
                n += jl_printf(out, ")");
            }
        }
    }
    else if (jl_datatype_type && jl_is_datatype(vt)) {
        // typeof(v) isa DataType, so v is an *instance of* a type that is a Datatype,
        // meaning v is e.g. an instance of a struct. These are printed as a call to a
        // type constructor, such as e.g. `Base.UnitRange{Int64}(start=1, stop=2)`
        int istuple = jl_is_tuple_type(vt), isnamedtuple = jl_is_namedtuple_type(vt);
        size_t tlen = jl_datatype_nfields(vt);
        if (isnamedtuple) {
            if (tlen == 0)
                n += jl_printf(out, "NamedTuple");
        }
        else if (!istuple) {
            n += jl_static_show_x(out, (jl_value_t*)vt, depth, ctx);
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
            jl_value_t *names = isnamedtuple ? jl_tparam0(vt) : (jl_value_t*)jl_field_names(vt);
            for (; i < tlen; i++) {
                if (!istuple) {
                    jl_sym_t *fname = (jl_sym_t*)(isnamedtuple ? jl_fieldref_noalloc(names, i) : jl_svecref(names, i));
                    if (fname == NULL || !jl_is_symbol(fname))
                        n += jl_static_show_x(out, (jl_value_t*)fname, depth, ctx);
                    else if (jl_is_operator(jl_symbol_name(fname)))
                        n += jl_printf(out, "(%s)", jl_symbol_name(fname));
                    else
                        n += jl_static_show_symbol(out, fname);
                    n += jl_printf(out, "=");
                }
                size_t offs = jl_field_offset(vt, i);
                char *fld_ptr = (char*)v + offs;
                if (jl_field_isptr(vt, i)) {
                    n += jl_static_show_x(out, *(jl_value_t**)fld_ptr, depth, ctx);
                }
                else {
                    jl_datatype_t *ft = (jl_datatype_t*)jl_field_type_concrete(vt, i);
                    if (jl_is_uniontype(ft)) {
                        uint8_t sel = ((uint8_t*)fld_ptr)[jl_field_size(vt, i) - 1];
                        ft = (jl_datatype_t*)jl_nth_union_component((jl_value_t*)ft, sel);
                    }
                    n += jl_static_show_x_(out, (jl_value_t*)fld_ptr, ft, depth, ctx);
                }
                if ((istuple || isnamedtuple) && tlen == 1)
                    n += jl_printf(out, ",");
                else if (i != tlen - 1)
                    n += jl_printf(out, ", ");
            }
            if (vt == jl_typemap_entry_type) {
                n += jl_printf(out, ", next=↩︎\n  ");
                jl_value_t *next = (jl_value_t*)jl_atomic_load_relaxed(&((jl_typemap_entry_t*)v)->next);
                n += jl_static_show_next_(out, next, v, depth, ctx);
            }
        }
        n += jl_printf(out, ")");
    }
    else {
        n += jl_printf(out, "<?#%p::", (void*)v);
        n += jl_static_show_x(out, (jl_value_t*)vt, depth, ctx);
        n += jl_printf(out, ">");
    }
    return n;
}

static size_t jl_static_show_x(JL_STREAM *out, jl_value_t *v, struct recur_list *depth, jl_static_show_config_t ctx) JL_NOTSAFEPOINT
{
    // show values without calling a julia method or allocating through the GC
    return jl_static_show_next_(out, v, NULL, depth, ctx);
}

static size_t jl_static_show_next_(JL_STREAM *out, jl_value_t *v, jl_value_t *prev, struct recur_list *depth, jl_static_show_config_t ctx) JL_NOTSAFEPOINT
{
    // helper for showing a typemap list by following the next pointers
    // while being careful about avoiding any recursion due to malformed (circular) references
    if (v == NULL) {
        return jl_printf(out, "#<null>");
    }
    else if ((uintptr_t)v < 4096U) {
        return jl_printf(out, "#<%d>", (int)(uintptr_t)v);
    }
    unsigned int dist = 1;
    struct recur_list this_item = {depth, v},
                      *newdepth = &this_item,
                      *p = depth;
    while (p) {
        if (jl_typetagis(v, jl_typemap_entry_type) && newdepth == &this_item) {
            jl_value_t *m = p->v;
            unsigned nid = 1;
            while (m && jl_typetagis(m, jl_typemap_entry_type)) {
                if (m == v) {
                    return jl_printf(out, "<typemap reference #%u @-%u ", nid, dist) +
                           jl_static_show_x(out, (jl_value_t*)((jl_typemap_entry_t*)m)->sig, depth, ctx) +
                           jl_printf(out, ">");
                }
                if (m == prev) {
                    newdepth = depth;
                    break;
                }
                // verify that we aren't trying to follow a circular list
                // by following the list again, and ensuring this is the only link to next
                jl_value_t *mnext = (jl_value_t*)jl_atomic_load_relaxed(&((jl_typemap_entry_t*)m)->next);
                jl_value_t *m2 = p->v;
                if (m2 == mnext)
                    break;
                while (m2 && jl_typetagis(m2, jl_typemap_entry_type)) {
                    jl_value_t *mnext2 = (jl_value_t*)jl_atomic_load_relaxed(&((jl_typemap_entry_t*)m2)->next);
                    if (mnext2 == mnext) {
                        if (m2 != m)
                            mnext = NULL;
                        break;
                    }
                    m2 = mnext2;
                }
                m = mnext;
                nid++;
            }
        }
        if (p->v == v)
            return jl_printf(out, "<circular reference @-%u>", dist);
        dist++;
        p = p->prev;
    }
    return jl_static_show_x_(out, v, (jl_datatype_t*)jl_typeof(v), newdepth, ctx);
}

JL_DLLEXPORT size_t jl_static_show(JL_STREAM *out, jl_value_t *v) JL_NOTSAFEPOINT
{
    jl_static_show_config_t ctx = { /* quiet */ 0 };
    return jl_static_show_x(out, v, 0, ctx);
}

JL_DLLEXPORT size_t jl_static_show_func_sig(JL_STREAM *s, jl_value_t *type) JL_NOTSAFEPOINT
{
    jl_static_show_config_t ctx = { /* quiet */ 0 };
    return jl_static_show_func_sig_(s, type, ctx);
}

size_t jl_static_show_func_sig_(JL_STREAM *s, jl_value_t *type, jl_static_show_config_t ctx) JL_NOTSAFEPOINT
{
    size_t n = 0;
    size_t i;
    jl_value_t *ftype = (jl_value_t*)jl_nth_argument_datatype(type, 1);
    if (ftype == NULL)
        return jl_static_show(s, type);
    jl_unionall_t *tvars = (jl_unionall_t*)type;
    int nvars = jl_subtype_env_size(type);
    struct recur_list *depth = NULL;
    if (nvars > 0)  {
        depth = (struct recur_list*)alloca(sizeof(struct recur_list) * nvars);
        for (i = 0; i < nvars; i++) {
            depth[i].prev = i == 0 ? NULL : &depth[i - 1];
            depth[i].v = type;
            type = ((jl_unionall_t*)type)->body;
        }
        depth += nvars - 1;
    }
    if (!jl_is_datatype(type)) {
        n += jl_static_show(s, type);
        return n;
    }
    if ((jl_nparams(ftype) == 0 || ftype == ((jl_datatype_t*)ftype)->name->wrapper) &&
            ((jl_datatype_t*)ftype)->name->mt &&
            ((jl_datatype_t*)ftype)->name->mt != jl_type_type_mt &&
            ((jl_datatype_t*)ftype)->name->mt != jl_nonfunction_mt) {
        n += jl_static_show_symbol(s, ((jl_datatype_t*)ftype)->name->mt->name);
    }
    else {
        n += jl_printf(s, "(::");
        n += jl_static_show_x(s, ftype, depth, ctx);
        n += jl_printf(s, ")");
    }
    size_t tl = jl_nparams(type);
    n += jl_printf(s, "(");
    for (i = 1; i < tl; i++) {
        jl_value_t *tp = jl_tparam(type, i);
        if (i != tl - 1) {
            n += jl_static_show_x(s, tp, depth, ctx);
            n += jl_printf(s, ", ");
        }
        else {
            if (jl_vararg_kind(tp) == JL_VARARG_UNBOUND) {
                tp = jl_unwrap_vararg(tp);
                if (jl_is_unionall(tp))
                    n += jl_printf(s, "(");
                n += jl_static_show_x(s, tp, depth, ctx);
                if (jl_is_unionall(tp))
                    n += jl_printf(s, ")");
                n += jl_printf(s, "...");
            }
            else {
                n += jl_static_show_x(s, tp, depth, ctx);
            }
        }
    }
    n += jl_printf(s, ")");
    if (jl_is_unionall(tvars)) {
        depth -= nvars - 1;
        int first = 1;
        n += jl_printf(s, " where {");
        while (jl_is_unionall(tvars)) {
            if (!first)
                n += jl_printf(s, ", ");
            n += jl_static_show_x(s, (jl_value_t*)tvars->var, first ? NULL : depth,  ctx);
            tvars = (jl_unionall_t*)tvars->body;
            if (!first)
                depth += 1;
            first = 0;
        }
        n += jl_printf(s, "}");
    }
    return n;
}

JL_DLLEXPORT void jl_(void *jl_value) JL_NOTSAFEPOINT
{
    jl_jmp_buf *old_buf = jl_get_safe_restore();
    jl_jmp_buf buf;
    jl_set_safe_restore(&buf);
    if (!jl_setjmp(buf, 0)) {
        jl_static_show((JL_STREAM*)STDERR_FILENO, (jl_value_t*)jl_value);
        jl_printf((JL_STREAM*)STDERR_FILENO,"\n");
    }
    else {
        jl_printf((JL_STREAM*)STDERR_FILENO, "\n!!! ERROR in jl_ -- ABORTING !!!\n");
    }
    jl_set_safe_restore(old_buf);
}

JL_DLLEXPORT void jl_breakpoint(jl_value_t *v)
{
    // put a breakpoint in your debugger here
}

JL_DLLEXPORT void jl_test_failure_breakpoint(jl_value_t *v)
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
        JL_STREAM* str = (JL_STREAM*)&str_;
        if (jl_is_string(msg)) {
            jl_uv_puts(str, jl_string_data(msg), jl_string_len(msg));
        }
        else if (jl_is_symbol(msg)) {
            jl_printf(str, "%s", jl_symbol_name((jl_sym_t*)msg));
        }
        jl_printf(str, "\n@ ");
        if (jl_is_string(file)) {
            jl_static_show_string(str, jl_string_data(file), jl_string_len(file), 0);
        }
        else if (jl_is_symbol(file)) {
            jl_static_show_string(str, jl_symbol_name((jl_sym_t*)file), strlen(jl_symbol_name((jl_sym_t*)file)), 0);
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

#ifdef __cplusplus
}
#endif
