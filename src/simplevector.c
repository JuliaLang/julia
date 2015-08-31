// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "julia.h"
#include "julia_internal.h"

DLLEXPORT jl_svec_t *jl_svec(size_t n, ...)
{
    va_list args;
    if (n == 0) return jl_emptysvec;
    va_start(args, n);
    jl_svec_t *jv = jl_alloc_svec_uninit(n);
    for(size_t i=0; i < n; i++) {
        jl_svecset(jv, i, va_arg(args, jl_value_t*));
    }
    va_end(args);
    return jv;
}

jl_svec_t *jl_svec1(void *a)
{
    jl_svec_t *v = (jl_svec_t*)jl_gc_alloc_2w();
    jl_set_typeof(v, jl_simplevector_type);
    jl_svec_set_len_unsafe(v, 1);
    jl_svecset(v, 0, a);
    return v;
}

jl_svec_t *jl_svec2(void *a, void *b)
{
    jl_svec_t *v = (jl_svec_t*)jl_gc_alloc_3w();
    jl_set_typeof(v, jl_simplevector_type);
    jl_svec_set_len_unsafe(v, 2);
    jl_svecset(v, 0, a);
    jl_svecset(v, 1, b);
    return v;
}

jl_svec_t *jl_alloc_svec_uninit(size_t n)
{
    if (n == 0) return jl_emptysvec;
    jl_svec_t *jv = (jl_svec_t*)newobj((jl_value_t*)jl_simplevector_type, n+1);
    jl_svec_set_len_unsafe(jv, n);
    return jv;
}

jl_svec_t *jl_alloc_svec(size_t n)
{
    if (n == 0) return jl_emptysvec;
    jl_svec_t *jv = jl_alloc_svec_uninit(n);
    for(size_t i=0; i < n; i++) {
        jl_svecset(jv, i, NULL);
    }
    return jv;
}

jl_svec_t *jl_svec_append(jl_svec_t *a, jl_svec_t *b)
{
    jl_svec_t *c = jl_alloc_svec_uninit(jl_svec_len(a) + jl_svec_len(b));
    size_t i=0, j;
    for(j=0; j < jl_svec_len(a); j++) {
        jl_svecset(c, i, jl_svecref(a,j));
        i++;
    }
    for(j=0; j < jl_svec_len(b); j++) {
        jl_svecset(c, i, jl_svecref(b,j));
        i++;
    }
    return c;
}

jl_svec_t *jl_svec_copy(jl_svec_t *a)
{
    return jl_svec_append(a, jl_emptysvec);
}

jl_svec_t *jl_svec_fill(size_t n, jl_value_t *x)
{
    if (n==0) return jl_emptysvec;
    jl_svec_t *v = jl_alloc_svec_uninit(n);
    for(size_t i=0; i < n; i++) {
        jl_svecset(v, i, x);
    }
    return v;
}
