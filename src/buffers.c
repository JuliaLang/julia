// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#define MAXINTVAL (((size_t)-1)>>1)

JL_DLLEXPORT jl_buffer_t *jl_buffer_copy(jl_buffer_t *a)
{
    jl_buffer_t *c = jl_new_buffer((jl_value_t*)jl_buffer_eltype((jl_value_t*)a), jl_buffer_len((jl_value_t*)a));
    memcpy((void**)jl_buffer_data(c), (void**)jl_buffer_data(a), jl_buffer_nbytes((jl_value_t*)a));
    return c;
}

JL_DLLEXPORT size_t (jl_buffer_len)(jl_buffer_t *t) JL_NOTSAFEPOINT
{
    return jl_buffer_len(t);
}

JL_DLLEXPORT jl_buffer_t *jl_new_buffer(jl_value_t *eltype, size_t len)
{
    size_t elsz = 0, al = 0;
    int isunboxed = jl_islayout_inline(eltype, &elsz, &al);
    int isunion = jl_is_uniontype(eltype);
    // int hasptr = isunboxed && (jl_is_datatype(eltype) && ((jl_datatype_t*)eltype)->layout->npointers > 0);
    if (!isunboxed) {
        elsz = sizeof(void*);
        al = elsz;
    }
    else {
        elsz = LLT_ALIGN(elsz, al);
    }
    jl_task_t *ct = jl_current_task;
    size_t tot = len * elsz;
    if (isunboxed) {
        if (elsz == 1 && !isunion) {
            // extra byte for all julia allocated byte arrays
            tot++;
        }
        if (isunion) {
            // an extra byte for each isbits union array element, stored after len * elsize
            tot += len;
        }
    }
    // align data area
    int tsz = sizeof(jl_buffer_t);
    // align data area
    if (tot >= ARRAY_CACHE_ALIGN_THRESHOLD)
        tsz = LLT_ALIGN(tsz, JL_CACHE_BYTE_ALIGNMENT);
    else if (isunboxed && elsz >= 4)
        tsz = LLT_ALIGN(tsz, JL_SMALL_BYTE_ALIGNMENT);
    tsz += tot;
    jl_value_t *tparams[2];
    tparams[0] = eltype;
    tparams[1] = jl_box_long((ssize_t)elsz);
    jl_value_t *btype = jl_apply_type((jl_value_t*)jl_mutablebuffer_type, tparams, 2);
    // TODO allocate data and set first 8 bits to length before initializing type
    jl_buffer_t *sb = (jl_buffer_t*)jl_gc_alloc(ct->ptls, tot, btype);
    sb->length = len;
    return sb;
}

JL_DLLEXPORT jl_value_t *jl_bufref(jl_buffer_t *sb, size_t i)
{
    size_t len = jl_buffer_len(sb);
    jl_value_t *eltype = (jl_value_t*)jl_buffer_eltype((jl_value_t*)sb);
    size_t elsize = jl_buffer_elsize((jl_value_t*)sb);
    char *data = (char*)(sb) + sizeof(jl_buffer_t);
    jl_value_t *val_i;
    if (jl_is_uniontype(eltype)) {
        uint8_t sel = ((uint8_t*)data + (len * elsize))[i];
        eltype = jl_nth_union_component(eltype, sel);
        if (jl_is_datatype_singleton((jl_datatype_t*)eltype)) {
            val_i = ((jl_datatype_t*)eltype)->instance;
        }
        else {
            val_i = jl_new_bits(eltype, data + (i * jl_datatype_size(eltype)));
        }
    }
    else {
        val_i = jl_new_bits(eltype, data + (i * elsize));
    }
    return val_i;
}

JL_DLLEXPORT void jl_bufset(jl_buffer_t *sb JL_ROOTING_ARGUMENT, jl_value_t *rhs JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED, size_t i)
{
    size_t len = jl_buffer_len(sb);
    jl_value_t *eltype = (jl_value_t*)jl_buffer_eltype((jl_value_t*)sb);
    size_t elsize = jl_buffer_elsize((jl_value_t*)sb);
    char *data = (char*)(sb) + sizeof(jl_buffer_t);
    if (jl_is_uniontype(eltype)) {
        // set type tag
        uint8_t *psel = &((uint8_t*)data + (len * elsize))[i];
        unsigned nth = 0;
        if (!jl_find_union_component(eltype, jl_typeof(rhs), &nth))
            assert(0 && "invalid bufset to isbits union");
        *psel = nth;
        if (jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(rhs)))
            return;
    }
    switch (elsize) {
    case  0: break;
    case  1: *(uint8_t*)(data + i)  = *(uint8_t*)rhs;  break;
    case  2: *(uint16_t*)(data + (i * elsize)) = *(uint16_t*)rhs; break;
    case  4: *(uint32_t*)(data + (i * elsize)) = *(uint32_t*)rhs; break;
    case  8: *(uint64_t*)(data + (i * elsize)) = *(uint64_t*)rhs; break;
    case 16:
        memcpy(jl_assume_aligned((data + (i * elsize)), 16), jl_assume_aligned(rhs, 16), 16);
        break;
    default: memcpy((data + (i * elsize)), rhs, elsize);
    }
}
