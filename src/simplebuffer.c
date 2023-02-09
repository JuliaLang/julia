// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

JL_DLLEXPORT jl_sbuf_t *(jl_sbuf)(size_t n, ...)
{
    va_list args;
    if (n == 0) return jl_emptysbuf;
    va_start(args, n);
    jl_sbuf_t *jv = jl_alloc_sbuf_uninit(n);
    for (size_t i = 0; i < n; i++)
        jl_sbufset(jv, i, va_arg(args, jl_value_t*));
    va_end(args);
    return jv;
}

jl_sbuf_t *(jl_perm_symsbuf)(size_t n, ...)
{
    if (n == 0) return jl_emptysbuf;
    jl_sbuf_t *jv = (jl_sbuf_t*)jl_gc_permobj((n + 1) * sizeof(void*), jl_simplebuffer_type);
    jl_sbuf_set_len_unsafe(jv, n);
    va_list args;
    va_start(args, n);
    for (size_t i = 0; i < n; i++)
        jl_sbufset(jv, i, jl_symbol(va_arg(args, const char*)));
    va_end(args);
    return jv;
}

JL_DLLEXPORT jl_sbuf_t *jl_sbuf1(void *a)
{
    jl_task_t *ct = jl_current_task;
    jl_sbuf_t *v = (jl_sbuf_t*)jl_gc_alloc(ct->ptls, sizeof(void*) * 2,
                                           jl_simplebuffer_type);
    jl_sbuf_set_len_unsafe(v, 1);
    jl_sbuf_data(v)[0] = (jl_value_t*)a;
    return v;
}

JL_DLLEXPORT jl_sbuf_t *jl_sbuf2(void *a, void *b)
{
    jl_task_t *ct = jl_current_task;
    jl_sbuf_t *v = (jl_sbuf_t*)jl_gc_alloc(ct->ptls, sizeof(void*) * 3,
                                           jl_simplebuffer_type);
    jl_sbuf_set_len_unsafe(v, 2);
    jl_sbuf_data(v)[0] = (jl_value_t*)a;
    jl_sbuf_data(v)[1] = (jl_value_t*)b;
    return v;
}

JL_DLLEXPORT jl_sbuf_t *jl_alloc_sbuf_uninit(size_t n)
{
    jl_task_t *ct = jl_current_task;
    if (n == 0) return jl_emptysbuf;
    jl_sbuf_t *jv = (jl_sbuf_t*)jl_gc_alloc(ct->ptls, (n + 1) * sizeof(void*),
                                            jl_simplebuffer_type);
    jl_sbuf_set_len_unsafe(jv, n);
    return jv;
}

JL_DLLEXPORT jl_sbuf_t *jl_alloc_sbuf(size_t n)
{
    if (n == 0) return jl_emptysbuf;
    jl_sbuf_t *jv = jl_alloc_sbuf_uninit(n);
    memset(jl_assume_aligned(jl_sbuf_data(jv), sizeof(void*)), 0, n * sizeof(void*));
    return jv;
}

JL_DLLEXPORT jl_sbuf_t *jl_sbuf_copy(jl_sbuf_t *a)
{
    size_t n = jl_sbuf_len(a);
    jl_sbuf_t *c = jl_alloc_sbuf_uninit(n);
    memmove_refs((void**)jl_sbuf_data(c), (void**)jl_sbuf_data(a), n);
    return c;
}

JL_DLLEXPORT jl_sbuf_t *jl_sbuf_fill(size_t n, jl_value_t *x)
{
    if (n == 0) return jl_emptysbuf;
    jl_sbuf_t *v = jl_alloc_sbuf_uninit(n);
    for (size_t i = 0; i < n; i++)
        jl_sbuf_data(v)[i] = x;
    return v;
}

JL_DLLEXPORT size_t (jl_sbuf_len)(jl_sbuf_t *t) JL_NOTSAFEPOINT
{
    return jl_sbuf_len(t);
}

JL_DLLEXPORT jl_value_t *jl_sbuf_ref(jl_sbuf_t *t JL_PROPAGATES_ROOT, ssize_t i)
{
    jl_value_t *v = jl_sbufref(t, (size_t)i);
    assert(v != NULL);
    return v;
}
