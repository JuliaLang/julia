// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

// STATIC_INLINE uint8_t jl_sbuf_uint8_ref(void *sb, size_t i) JL_NOTSAFEPOINT
// {
//     return ((uint8_t*)(jl_sbuf_data(sb)))[i];
// }
// STATIC_INLINE void jl_sbuf_uint8_set(void *a, size_t i, uint8_t x) JL_NOTSAFEPOINT
// {
//     ((uint8_t*)(jl_array_data(a)))[i] = x;
// }

JL_DLLEXPORT jl_sbuf_t *jl_alloc_sbuf(size_t n)
{
    if (n == 0) return jl_emptysbuf;
    jl_task_t *ct = jl_current_task;
    jl_sbuf_t *sb = (jl_sbuf_t*)jl_gc_alloc(ct->ptls, sizeof(void*) + n, jl_simplebuffer_type);
    jl_sbuf_set_len_unsafe(sb, n);
    return sb;
}

JL_DLLEXPORT jl_sbuf_t *jl_sbuf_copy(jl_sbuf_t *a)
{
    size_t n = jl_sbuf_len(a);
    jl_sbuf_t *c = jl_alloc_sbuf(n);
    memcpy((void**)jl_sbuf_data(c), (void**)jl_sbuf_data(a), n);
    return c;
}

JL_DLLEXPORT size_t (jl_sbuf_len)(jl_sbuf_t *t) JL_NOTSAFEPOINT
{
    return jl_sbuf_len(t);
}

JL_DLLEXPORT jl_value_t *jl_sbuf_ref(jl_sbuf_t *sb JL_PROPAGATES_ROOT, ssize_t i)
{
    return jl_box_uint8(((uint8_t*)(jl_sbuf_data(sb)))[i]);
}
