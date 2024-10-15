// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  array constructors and primitives
*/
#include <stdlib.h>
#include <string.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_P64) && defined(UINT128MAX)
typedef __uint128_t wideint_t;
#else
typedef uint64_t wideint_t;
#endif

#define MAXINTVAL (((size_t)-1)>>1)

JL_DLLEXPORT int jl_array_validate_dims(size_t *nel, uint32_t ndims, size_t *dims)
{
    size_t i;
    size_t _nel = 1;
    for (i = 0; i < ndims; i++) {
        size_t di = dims[i];
        wideint_t prod = (wideint_t)_nel * (wideint_t)di;
        if (prod >= (wideint_t) MAXINTVAL || di >= MAXINTVAL)
            return 1;
        _nel = prod;
    }
    *nel = _nel;
    return 0;
}

#ifndef JL_NDEBUG
static inline int is_ntuple_long(jl_value_t *v)
{
    if (!jl_is_tuple(v))
        return 0;
    jl_value_t *tt = (jl_value_t*)jl_typetagof(v);
    size_t i, nfields = jl_nparams(tt);
    for (i = 0; i < nfields; i++) {
        if (jl_tparam(tt, i) != (jl_value_t*)jl_long_type) {
            return 0;
        }
    }
    return 1;
}
#endif

#define jl_array_elsize(a) (((jl_datatype_t*)jl_typetagof((a)->ref.mem))->layout->size)

static char *jl_array_typetagdata(jl_array_t *a) JL_NOTSAFEPOINT
{
    assert(jl_genericmemory_isbitsunion(a->ref.mem));
    return jl_genericmemory_typetagdata(a->ref.mem) + (uintptr_t)a->ref.ptr_or_offset;
}

STATIC_INLINE jl_array_t *_new_array(jl_value_t *atype, jl_genericmemory_t *mem, const jl_datatype_layout_t *layout, uint32_t ndims, size_t *dims)
{
    jl_task_t *ct = jl_current_task;
    size_t i;
    int tsz = sizeof(jl_array_t) + ndims*sizeof(size_t);
    jl_array_t *a = (jl_array_t*)jl_gc_alloc(ct->ptls, tsz, atype);
    a->ref.mem = mem;
    if (layout->flags.arrayelem_isunion || layout->size == 0)
        a->ref.ptr_or_offset = 0;
    else
        a->ref.ptr_or_offset = mem->ptr;
    for (i = 0; i < ndims; i++)
        a->dimsize[i] = dims[i];
    return a;
}

STATIC_INLINE jl_array_t *new_array(jl_value_t *atype, uint32_t ndims, size_t *dims)
{
    size_t nel;
    if (jl_array_validate_dims(&nel, ndims, dims))
        jl_exceptionf(jl_argumenterror_type, "invalid Array dimensions: too large for system address width");
    if (*(size_t*)jl_tparam1(atype) != ndims)
        jl_exceptionf(jl_argumenterror_type, "invalid Array dimensions");
    jl_value_t *mtype = jl_field_type_concrete((jl_datatype_t*)jl_field_type_concrete((jl_datatype_t*)atype, 0), 1);
    // extra byte for all julia allocated byte vectors
    jl_genericmemory_t *mem = jl_alloc_genericmemory(mtype, nel);
    JL_GC_PUSH1(&mem);
    jl_array_t *a = _new_array(atype, mem, ((jl_datatype_t*)mtype)->layout, ndims, dims);
    JL_GC_POP();
    return a;
}

jl_genericmemory_t *_new_genericmemory_(jl_value_t *mtype, size_t nel, int8_t isunion, int8_t zeroinit, size_t elsz);

JL_DLLEXPORT jl_genericmemory_t *jl_string_to_genericmemory(jl_value_t *str);

JL_DLLEXPORT jl_array_t *jl_ptr_to_array_1d(jl_value_t *atype, void *data,
                                            size_t nel, int own_buffer)
{
    if (*(size_t*)jl_tparam1(atype) != 1)
        jl_exceptionf(jl_argumenterror_type, "invalid Array dimensions");
    jl_value_t *mtype = jl_field_type_concrete((jl_datatype_t*)jl_field_type_concrete((jl_datatype_t*)atype, 0), 1);
    jl_genericmemory_t *mem = jl_ptr_to_genericmemory(mtype, data, nel, own_buffer);
    JL_GC_PUSH1(&mem);
    jl_array_t *a = _new_array(atype, mem, ((jl_datatype_t*)mtype)->layout, 1, &nel);
    JL_GC_POP();
    return a;
}

JL_DLLEXPORT jl_array_t *jl_ptr_to_array(jl_value_t *atype, void *data,
                                         jl_value_t *_dims, int own_buffer)
{
    size_t ndims = jl_nfields(_dims);
    assert(is_ntuple_long(_dims));
    size_t *dims = (size_t*)_dims;
    size_t nel;
    if (jl_array_validate_dims(&nel, ndims, dims))
        jl_exceptionf(jl_argumenterror_type, "invalid Array dimensions: too large for system address width");
    if (*(size_t*)jl_tparam1(atype) != ndims)
        jl_exceptionf(jl_argumenterror_type, "invalid Array dimensions");
    jl_value_t *mtype = jl_field_type_concrete((jl_datatype_t*)jl_field_type_concrete((jl_datatype_t*)atype, 0), 1);
    jl_genericmemory_t *mem = jl_ptr_to_genericmemory(mtype, data, nel, own_buffer);
    JL_GC_PUSH1(&mem);
    jl_array_t *a = _new_array(atype, mem, ((jl_datatype_t*)mtype)->layout, ndims, dims);
    JL_GC_POP();
    return a;
}

JL_DLLEXPORT jl_value_t *jl_array_to_string(jl_array_t *a)
{
    size_t len = jl_array_nrows(a); // only for Vector
    if (len == 0) {
        // this may seem like purely an optimization (which it also is), but it
        // also ensures that calling `String(a)` doesn't corrupt a previous
        // string also created the same way, where `a = StringVector(_)`.
        return jl_an_empty_string;
    }
    jl_value_t *str;
    if (a->ref.ptr_or_offset == a->ref.mem->ptr)
        str = jl_genericmemory_to_string(a->ref.mem, len);
    else
        str = jl_pchar_to_string(jl_array_data(a, char), len);
    a->ref.mem = (jl_genericmemory_t*)((jl_datatype_t*)jl_memory_uint8_type)->instance;
    a->ref.ptr_or_offset = a->ref.mem->ptr;
    a->dimsize[0] = 0;
    return str;
}

JL_DLLEXPORT jl_array_t *jl_alloc_array_1d(jl_value_t *atype, size_t nr)
{
    return new_array(atype, 1, &nr);
}

JL_DLLEXPORT jl_array_t *jl_alloc_array_2d(jl_value_t *atype, size_t nr, size_t nc)
{
    size_t dims[2] = {nr, nc};
    return new_array(atype, 2, &dims[0]);
}

JL_DLLEXPORT jl_array_t *jl_alloc_array_3d(jl_value_t *atype, size_t nr, size_t nc, size_t z)
{
    size_t dims[3] = {nr, nc, z};
    return new_array(atype, 3, &dims[0]);
}

JL_DLLEXPORT jl_array_t *jl_alloc_array_nd(jl_value_t *atype, size_t *dims, size_t ndims)
{
    return new_array(atype, ndims, dims);
}

JL_DLLEXPORT jl_array_t *jl_pchar_to_array(const char *str, size_t len)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_uint8_type, len);
    assert(jl_array_data(a, char));
    memcpy(jl_array_data(a, char), str, len);
    return a;
}

JL_DLLEXPORT jl_array_t *jl_alloc_vec_any(size_t n)
{
    return jl_alloc_array_1d(jl_array_any_type, n);
}

JL_DLLEXPORT jl_value_t *jl_apply_array_type(jl_value_t *type, size_t dim)
{
    jl_value_t *boxed_dim = jl_box_long(dim);
    JL_GC_PUSH1(&boxed_dim);
    jl_value_t *ret = jl_apply_type2((jl_value_t*)jl_array_type, type, boxed_dim);
    JL_GC_POP();
    return ret;
}

JL_DLLEXPORT void jl_array_grow_end(jl_array_t *a, size_t inc)
{
    size_t n = jl_array_nrows(a);
    size_t elsz = jl_array_elsize(a);
    char *data = jl_array_data(a,char);
    jl_value_t *mtype = (jl_value_t*)jl_typetagof(a->ref.mem);
    int isbitsunion = jl_genericmemory_isbitsunion(a->ref.mem);
    size_t newnrows = n + inc;
    if (!isbitsunion && elsz == 0) {
        jl_genericmemory_t *newmem = jl_alloc_genericmemory(mtype, MAXINTVAL - 1);
        a->ref.mem = newmem;
        jl_gc_wb(a, newmem);
        a->dimsize[0] = newnrows;
        return;
    }
    size_t oldoffset = isbitsunion ? (size_t)data : (data - (char*)a->ref.mem->ptr) / elsz;
    if (isbitsunion)
        data = (char*)a->ref.mem->ptr + oldoffset * elsz;
    size_t oldmaxsize = a->ref.mem->length;
    size_t reqmaxsize = oldoffset + newnrows;
    if (__unlikely(reqmaxsize > oldmaxsize)) {
        size_t newmaxsize;
        if (oldmaxsize < 4) // typical sequence: 0, // 4, // 6, 9, 13, 19, 28, 42, // 50, 60, 72, ...
            newmaxsize = 4;
        else if (oldmaxsize < 48)
            newmaxsize = oldmaxsize*3/2; // grow by 50%
        else
            newmaxsize = oldmaxsize*6/5; // grow by 20%
        if (newmaxsize < reqmaxsize)
            newmaxsize = reqmaxsize;
        // TODO: round this up to newmaxsize < GC_MAX_SZCLASS ? jl_gc_sizeclasses[jl_gc_szclass(newmaxsize)] : LLT_ALIGN(newmaxsize, 4096), after accounting for the object header (24 bytes)
        jl_genericmemory_t *newmem = jl_alloc_genericmemory(mtype, newmaxsize);
        char *newdata = (char*)newmem->ptr + oldoffset * elsz;
        memcpy(newdata, data, n * elsz);
        if (isbitsunion) {
            char *typetagdata = jl_array_typetagdata(a);
            char *newtypetagdata = (char*)newmem->ptr + newmaxsize * elsz + oldoffset;
            memcpy(newtypetagdata, typetagdata, n);
        }
        a->ref.mem = newmem;
        jl_gc_wb(a, newmem);
        if (isbitsunion)
            a->ref.ptr_or_offset = (void*)oldoffset;
        else
            a->ref.ptr_or_offset = newdata;
    }
    a->dimsize[0] = newnrows;
}

JL_DLLEXPORT void jl_array_del_end(jl_array_t *a, size_t dec)
{
    // assume inbounds, assume unshared
    size_t n = jl_array_nrows(a);
    if (__unlikely(n < dec))
        jl_bounds_error_int((jl_value_t*)a, 0);
    if (__unlikely(dec == 0))
        return;
    n -= dec;
    a->dimsize[0] = n;
    // don't leave behind deleted data
    if (jl_is_genericmemory_zeroinit(a->ref.mem) && !jl_genericmemory_isbitsunion(a->ref.mem)) {
        size_t elsz = jl_array_elsize(a);
        memset(jl_array_data(a,char) + n * elsz, 0, elsz * dec);
    }
}

JL_DLLEXPORT void jl_array_ptr_1d_push(jl_array_t *a, jl_value_t *item)
{
    assert(jl_typetagis(a, jl_array_any_type));
    jl_array_grow_end(a, 1);
    size_t n = jl_array_nrows(a);
    jl_array_ptr_set(a, n - 1, item);
}

JL_DLLEXPORT void jl_array_ptr_1d_append(jl_array_t *a, jl_array_t *a2)
{
    assert(jl_typetagis(a, jl_array_any_type));
    assert(jl_typetagis(a2, jl_array_any_type));
    size_t i;
    size_t n = jl_array_nrows(a);
    size_t n2 = jl_array_nrows(a2);
    jl_array_grow_end(a, n2);
    for (i = 0; i < n2; i++) {
        jl_array_ptr_set(a, n + i, jl_array_ptr_ref(a2, i));
    }
}

JL_DLLEXPORT jl_genericmemory_t *jl_genericmemory_copy_slice(jl_genericmemory_t *mem, void *data, size_t len);

JL_DLLEXPORT jl_array_t *jl_array_copy(jl_array_t *ary)
{
    size_t len = jl_array_len(ary);
    jl_genericmemory_t *mem = jl_genericmemory_copy_slice(ary->ref.mem, ary->ref.ptr_or_offset, len);
    JL_GC_PUSH1(&mem);
    jl_array_t *new_ary = _new_array((jl_value_t*)jl_typetagof(ary), mem, ((jl_datatype_t*)jl_typetagof(ary->ref.mem))->layout, jl_array_ndims(ary), &ary->dimsize[0]);
    JL_GC_POP();
    return new_ary;
}

JL_DLLEXPORT jl_value_t *jl_alloc_string(size_t len)
{
    if (len == 0)
        return jl_an_empty_string;
    size_t sz = sizeof(size_t) + len + 1; // add space for trailing \nul protector and size
    if (sz < len) // overflow
        jl_throw(jl_memory_exception);
    jl_task_t *ct = jl_current_task;
    jl_value_t *s;
    jl_ptls_t ptls = ct->ptls;
    s = (jl_value_t*)jl_gc_alloc(ptls, sz, jl_string_type);
    jl_set_typetagof(s, jl_string_tag, 0);
    *(size_t*)s = len;
    jl_string_data(s)[len] = 0;
    return s;
}

JL_DLLEXPORT jl_value_t *jl_pchar_to_string(const char *str, size_t len)
{
    jl_value_t *s = jl_alloc_string(len);
    if (len > 0)
        memcpy(jl_string_data(s), str, len);
    return s;
}

JL_DLLEXPORT jl_value_t *jl_cstr_to_string(const char *str)
{
    return jl_pchar_to_string(str, strlen(str));
}


// deprecated and unused internally, but some packages (notably OrderedCollections.jl) have not yet started to use the modern Base.unsetindex API
JL_DLLEXPORT void jl_arrayunset(jl_array_t *a, size_t i)
{
    if (i >= jl_array_len(a))
        jl_bounds_error_int((jl_value_t*)a, i + 1);
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(a->ref.mem))->layout;
    if (layout->flags.arrayelem_isboxed) {
        jl_atomic_store_relaxed(jl_array_data(a,_Atomic(jl_value_t*)) + i, NULL);
    }
    else if (layout->first_ptr >= 0) {
        size_t elsize = layout->size;
        jl_assume(elsize >= sizeof(void*) && elsize % sizeof(void*) == 0);
        memset(jl_array_data(a,char) + elsize * i, 0, elsize);
    }
}

#ifdef __cplusplus
}
#endif
