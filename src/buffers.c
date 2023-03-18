// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#define JL_BUFFER_IMPL_NUL 1

// The only time data may not be aligned is if it's a foreign pointer or if
// it's a String. Buffer doesn't Buffer doesn't support these
// right now but this method lets us build code around the situation where it
// might in the future.
int jl_is_aligned_buffer(jl_buffer_t *buf)
{
    return ((jl_datatype_t*)(jl_typeof(buf)))->name == jl_buffer_typename;
}

// FIXME Buffer: 
// julia-allocated buffer that needs to be marked
int jl_is_unmarked_buffer(jl_buffer_t *buf) JL_NOTSAFEPOINT
{
    return ((jl_datatype_t*)(jl_typeof(buf)))->name != jl_buffer_typename;
}

// given the element type layout (jl_eltype_layout) and number of elements stored,
// provides the number of bytes necessary for storage.
STATIC_INLINE size_t jl_nbytes_eltype_data(jl_eltype_layout_t lyt, size_t len)
{
    size_t data_size = len * lyt.elsize;
    if (!lyt.isboxed) {
        if (lyt.ntags > 1)
            data_size += len;  // an extra byte for each isbits union lement
        else if (lyt.elsize == 1)
            data_size++;  // extra byte for all julia allocated byte arrays
    }
    return data_size;
}

// number of bytes in b->data and and type tag data.
size_t jl_buffer_nbytes(jl_buffer_t *b) JL_NOTSAFEPOINT
{
    jl_eltype_layout_t lyt = jl_eltype_layout(jl_buffer_eltype(b));
    size_t len = jl_buffer_len(b);
    return jl_nbytes_eltype_data(lyt, len);
}

// compute offset necessary to align data (if it can be aligned)
size_t jl_buffer_object_size(jl_buffer_t *b) JL_NOTSAFEPOINT
{
    jl_eltype_layout_t lyt = jl_eltype_layout(jl_buffer_eltype(b));
    size_t len = jl_buffer_len(b);
    size_t data_size = jl_nbytes_eltype_data(lyt, len);
    size_t obj_size = sizeof(jl_buffer_t);
    if (data_size <= ARRAY_INLINE_NBYTES) {
        // align data area
        if (data_size >= ARRAY_CACHE_ALIGN_THRESHOLD)
            obj_size = LLT_ALIGN(obj_size, JL_CACHE_BYTE_ALIGNMENT);
        else if (!lyt.isboxed && lyt.elsize >= 4)
            obj_size = LLT_ALIGN(obj_size, JL_SMALL_BYTE_ALIGNMENT);
        obj_size += data_size;
    }
    return obj_size;
}

static jl_buffer_t *_new_buffer(jl_value_t *btype, size_t len,
    jl_eltype_layout_t lyt, int8_t zeroinit)
{
    jl_buffer_t *b;
    jl_task_t *ct = jl_current_task;
    size_t data_size = jl_nbytes_eltype_data(lyt, len);
    // size of raw data and object fields
    size_t obj_size = sizeof(jl_buffer_t);
    void *data;
    if (data_size <= ARRAY_INLINE_NBYTES) {
        // align data area
        if (data_size >= ARRAY_CACHE_ALIGN_THRESHOLD)
            obj_size = LLT_ALIGN(obj_size, JL_CACHE_BYTE_ALIGNMENT);
        else if (!lyt.isboxed && lyt.elsize >= 4)
            obj_size = LLT_ALIGN(obj_size, JL_SMALL_BYTE_ALIGNMENT);
        size_t doffs = obj_size;
        obj_size = data_size + doffs;
        b = (jl_buffer_t*)jl_gc_alloc(ct->ptls, obj_size, btype);
        data = (char*)b + doffs;
    }
    else {
        data = jl_gc_managed_malloc(data_size);
        // allocate buffer **after** allocating the data
        // to make sure the buffer is still young
        b = (jl_buffer_t*)jl_gc_alloc(ct->ptls, obj_size, btype);
        // No allocation or safepoint allowed after this
        jl_gc_track_malloced_buffer(ct->ptls, b);
    }

    // zero initialize data that may otherwise error on load
    if (zeroinit)
        memset(data, 0, data_size);

    b->length = len;
    b->data = data;

    if (JL_BUFFER_IMPL_NUL && lyt.elsize == 1)
         ((char*)data)[data_size - 1] = '\0';
    return b;
}

JL_DLLEXPORT jl_buffer_t *jl_new_buffer(jl_value_t *btype, size_t len)
{
    jl_value_t *eltype = jl_tparam0(btype);
    jl_eltype_layout_t lyt = jl_eltype_layout(eltype);
    int8_t zi = (lyt.isboxed || lyt.ntags > 1 || lyt.hasptr || (jl_is_datatype(eltype) && ((jl_datatype_t*)eltype)->zeroinit));
    return _new_buffer(btype, len, lyt, zi);
}

JL_DLLEXPORT jl_value_t *jl_bufref(jl_buffer_t *b, size_t i)
{
    size_t len = jl_buffer_len(b);
    assert(i < len);
    jl_value_t *ety = (jl_value_t*)jl_tparam0(jl_typeof(b));
    jl_eltype_layout_t lyt = jl_eltype_layout(ety);
    char *data = (char*)(jl_buffer_data(b));
    if (!lyt.isboxed && jl_is_datatype(ety) && jl_datatype_size(ety) == 0) {
        return ((jl_datatype_t*)ety)->instance;
    }
    else if (!lyt.isboxed) {
        if (lyt.ntags > 1) {
            // isbits union selector bytes are always stored directly after the last array element
            uint8_t sel = (data + (len * lyt.elsize))[i];
            ety = jl_nth_union_component(ety, sel);
            if (jl_is_datatype_singleton((jl_datatype_t*)ety))
                return ((jl_datatype_t*)ety)->instance;
        }
        jl_value_t *r = undefref_check((jl_datatype_t*)ety, jl_new_bits(ety, &(data)[i * lyt.elsize]));
        if (__unlikely(r == NULL))
            jl_throw(jl_undefref_exception);
        return r;
    }
    else {
        jl_value_t *elt = jl_atomic_load_relaxed(((_Atomic(jl_value_t*)*)b->data) + i);
        if (elt == NULL)
            jl_throw(jl_undefref_exception);
        return elt;
    }
}

JL_DLLEXPORT void jl_bufset(jl_buffer_t *b JL_ROOTING_ARGUMENT, jl_value_t *rhs JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED, size_t i)
{
    size_t len = jl_buffer_len(b);
    assert(i < len);
    jl_value_t *ety = (jl_value_t*)jl_tparam0(jl_typeof(b));
    jl_eltype_layout_t lyt = jl_eltype_layout(ety);
    char *data = (char*)(jl_buffer_data(b));
    if (lyt.isboxed) {
        jl_atomic_store_release(((_Atomic(jl_value_t*)*)data) + i, rhs);
        jl_gc_wb(b, rhs);
    }
    else {
        if (lyt.ntags > 1) {
            // set type tag
            uint8_t *psel = &((uint8_t*)data + (len * lyt.elsize))[i];
            unsigned nth = 0;
            if (!jl_find_union_component(ety, jl_typeof(rhs), &nth))
                assert(0 && "invalid bufset to isbits union");
            *psel = nth;
            if (jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(rhs)))
                return;
        }
        switch (lyt.elsize) {
        case  0: break;
        case  1: *(uint8_t*)(data + i)  = *(uint8_t*)rhs;  break;
        case  2: *(uint16_t*)(data + (i * 2)) = *(uint16_t*)rhs; break;
        case  4: *(uint32_t*)(data + (i * 4)) = *(uint32_t*)rhs; break;
        case  8: *(uint64_t*)(data + (i * 8)) = *(uint64_t*)rhs; break;
        case 16:
            memcpy(jl_assume_aligned((data + (i * lyt.elsize)), 16), jl_assume_aligned(rhs, 16), 16);
            break;
        default: memcpy((data + (i * lyt.elsize)), rhs, lyt.elsize);
        }
    }
}

JL_DLLEXPORT int jl_buffer_isassigned(jl_buffer_t *b, size_t i)
{
    jl_value_t *eltype = jl_tparam0(jl_typeof(b));
    jl_eltype_layout_t lyt = jl_eltype_layout(eltype);
    if (lyt.isboxed) {
        return jl_atomic_load_relaxed(((_Atomic(jl_value_t*)*)jl_buffer_data(b)) + i) != NULL;
    }
    else if ((jl_is_datatype(eltype) && ((jl_datatype_t*)eltype)->layout->npointers > 0)) {
        jl_datatype_t *elty = (jl_datatype_t*)eltype;
         assert(elty->layout->first_ptr >= 0);
         jl_value_t **elem = (jl_value_t**)((char*)b->data + i * lyt.elsize);
         return elem[elty->layout->first_ptr] != NULL;
    }
    return 1;
}

JL_DLLEXPORT jl_buffer_t *jl_buffer_copy(jl_buffer_t *old_buf)
{
    jl_value_t *btype = jl_typeof(old_buf);
    jl_value_t *eltype = jl_tparam0(btype);
    jl_eltype_layout_t lyt = jl_eltype_layout(eltype);
    size_t len = jl_buffer_len(old_buf);
    jl_buffer_t *new_buf = _new_buffer(btype, len, lyt, 0);
    void *old_data = old_buf->data;
    void *new_data = new_buf->data;
    size_t data_len = len * lyt.elsize;
    memcpy(new_data, old_data, data_len);
    // ensure isbits union arrays copy their selector bytes correctly
    if (lyt.ntags > 1)
        memcpy(((char*)new_data) + len, ((char*)old_data) + len, len);
    return new_buf;
}
