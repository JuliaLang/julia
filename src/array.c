// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  array constructors and primitives
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

#define JL_ARRAY_IMPL_NUL 1

#define JL_ARRAY_ALIGN(jl_value, nbytes) LLT_ALIGN(jl_value, nbytes)

// array constructors ---------------------------------------------------------

static inline int store_unboxed(jl_value_t *el_type) // jl_isbits
{
    return jl_is_leaf_type(el_type) && jl_is_immutable(el_type) &&
        ((jl_datatype_t*)el_type)->layout && ((jl_datatype_t*)el_type)->layout->pointerfree;
}

int jl_array_store_unboxed(jl_value_t *el_type)
{
    return store_unboxed(el_type);
}

STATIC_INLINE jl_value_t *jl_array_owner(jl_array_t *a)
{
    if (a->flags.how == 3) {
        a = (jl_array_t*)jl_array_data_owner(a);
        assert(a->flags.how != 3);
    }
    return (jl_value_t*)a;
}

#if defined(_P64) && defined(UINT128MAX)
typedef __uint128_t wideint_t;
#else
typedef uint64_t wideint_t;
#endif

size_t jl_arr_xtralloc_limit = 0;

#define MAXINTVAL (((size_t)-1)>>1)

static jl_array_t *_new_array_(jl_value_t *atype, uint32_t ndims, size_t *dims,
                               int isunboxed, int elsz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    size_t i, tot, nel=1;
    void *data;
    jl_array_t *a;

    for(i=0; i < ndims; i++) {
        wideint_t prod = (wideint_t)nel * (wideint_t)dims[i];
        if (prod > (wideint_t) MAXINTVAL)
            jl_error("invalid Array dimensions");
        nel = prod;
    }

    if (isunboxed) {
        wideint_t prod = (wideint_t)elsz * (wideint_t)nel;
        if (prod > (wideint_t) MAXINTVAL)
            jl_error("invalid Array size");
        tot = prod;
        if (elsz == 1) {
            // extra byte for all julia allocated byte arrays
            tot++;
        }
    }
    else {
        wideint_t prod = (wideint_t)sizeof(void*) * (wideint_t)nel;
        if (prod > (wideint_t) MAXINTVAL)
            jl_error("invalid Array size");
        tot = prod;
    }

    int ndimwords = jl_array_ndimwords(ndims);
    int tsz = JL_ARRAY_ALIGN(sizeof(jl_array_t) + ndimwords*sizeof(size_t), JL_CACHE_BYTE_ALIGNMENT);
    if (tot <= ARRAY_INLINE_NBYTES) {
        if (isunboxed && elsz >= 4)
            tsz = JL_ARRAY_ALIGN(tsz, JL_SMALL_BYTE_ALIGNMENT); // align data area
        size_t doffs = tsz;
        tsz += tot;
        tsz = JL_ARRAY_ALIGN(tsz, JL_SMALL_BYTE_ALIGNMENT); // align whole object
        a = (jl_array_t*)jl_gc_alloc(ptls, tsz, atype);
        // No allocation or safepoint allowed after this
        a->flags.how = 0;
        data = (char*)a + doffs;
        if (tot > 0 && !isunboxed) {
            memset(data, 0, tot);
        }
    }
    else {
        tsz = JL_ARRAY_ALIGN(tsz, JL_CACHE_BYTE_ALIGNMENT); // align whole object
        data = jl_gc_managed_malloc(tot);
        // Allocate the Array **after** allocating the data
        // to make sure the array is still young
        a = (jl_array_t*)jl_gc_alloc(ptls, tsz, atype);
        // No allocation or safepoint allowed after this
        a->flags.how = 2;
        jl_gc_track_malloced_array(ptls, a);
        if (!isunboxed)
            memset(data, 0, tot);
    }
    a->flags.pooled = tsz <= GC_MAX_SZCLASS;

    a->data = data;
    if (JL_ARRAY_IMPL_NUL && elsz == 1)
        ((char*)data)[tot - 1] = '\0';
#ifdef STORE_ARRAY_LEN
    a->length = nel;
#endif
    a->flags.ndims = ndims;
    a->flags.ptrarray = !isunboxed;
    a->elsize = elsz;
    a->flags.isshared = 0;
    a->flags.isaligned = 1;
    a->offset = 0;
    if (ndims == 1) {
        a->nrows = nel;
        a->maxsize = nel;
    }
    else {
        size_t *adims = &a->nrows;
        for(i=0; i < ndims; i++)
            adims[i] = dims[i];
    }

    return a;
}

static inline jl_array_t *_new_array(jl_value_t *atype, uint32_t ndims, size_t *dims)
{
    int isunboxed=0, elsz=sizeof(void*);
    jl_value_t *el_type = jl_tparam0(atype);
    isunboxed = store_unboxed(el_type);
    if (isunboxed)
        elsz = jl_datatype_size(el_type);
    return _new_array_(atype, ndims, dims, isunboxed, elsz);
}

jl_array_t *jl_new_array_for_deserialization(jl_value_t *atype, uint32_t ndims, size_t *dims,
                                             int isunboxed, int elsz)
{
    return _new_array_(atype, ndims, dims, isunboxed, elsz);
}

#ifndef NDEBUG
static inline int is_ntuple_long(jl_value_t *v)
{
    if (!jl_is_tuple(v))
        return 0;
    size_t nfields = jl_nfields(v);
    for (size_t i = 0; i < nfields; i++) {
        if (jl_field_type(jl_typeof(v), i) != (jl_value_t*)jl_long_type) {
            return 0;
        }
    }
    return 1;
}
#endif

JL_DLLEXPORT jl_array_t *jl_reshape_array(jl_value_t *atype, jl_array_t *data,
                                          jl_value_t *_dims)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_array_t *a;
    size_t ndims = jl_nfields(_dims);
    assert(is_ntuple_long(_dims));
    size_t *dims = (size_t*)_dims;

    int ndimwords = jl_array_ndimwords(ndims);
    int tsz = JL_ARRAY_ALIGN(sizeof(jl_array_t) + ndimwords*sizeof(size_t) + sizeof(void*), JL_SMALL_BYTE_ALIGNMENT);
    a = (jl_array_t*)jl_gc_alloc(ptls, tsz, atype);
    // No allocation or safepoint allowed after this
    a->flags.pooled = tsz <= GC_MAX_SZCLASS;
    a->flags.ndims = ndims;
    a->offset = 0;
    a->data = NULL;
    a->flags.isaligned = data->flags.isaligned;
    jl_value_t *el_type = jl_tparam0(atype);
    assert(store_unboxed(el_type) == !data->flags.ptrarray);
    if (!data->flags.ptrarray) {
        a->elsize = jl_datatype_size(el_type);
        a->flags.ptrarray = 0;
    }
    else {
        a->elsize = sizeof(void*);
        a->flags.ptrarray = 1;
    }

    // if data is itself a shared wrapper,
    // owner should point back to the original array
    jl_array_data_owner(a) = jl_array_owner(data);

    a->flags.how = 3;
    a->data = data->data;
    a->flags.isshared = 1;
    data->flags.isshared = 1;

    if (ndims == 1) {
        size_t l = dims[0];
#ifdef STORE_ARRAY_LEN
        a->length = l;
#endif
        a->nrows = l;
        a->maxsize = l;
    }
    else {
        size_t *adims = &a->nrows;
        size_t l = 1;
        wideint_t prod;
        for (size_t i = 0; i < ndims; i++) {
            adims[i] = dims[i];
            prod = (wideint_t)l * (wideint_t)adims[i];
            if (prod > (wideint_t) MAXINTVAL)
                jl_error("invalid Array dimensions");
            l = prod;
        }
#ifdef STORE_ARRAY_LEN
        a->length = l;
#endif
    }

    return a;
}

// own_buffer != 0 iff GC should call free() on this pointer eventually
JL_DLLEXPORT jl_array_t *jl_ptr_to_array_1d(jl_value_t *atype, void *data,
                                            size_t nel, int own_buffer)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    size_t elsz;
    jl_array_t *a;
    jl_value_t *el_type = jl_tparam0(atype);

    int isunboxed = store_unboxed(el_type);
    if (isunboxed)
        elsz = jl_datatype_size(el_type);
    else
        elsz = sizeof(void*);

    int ndimwords = jl_array_ndimwords(1);
    int tsz = JL_ARRAY_ALIGN(sizeof(jl_array_t) + ndimwords*sizeof(size_t), JL_CACHE_BYTE_ALIGNMENT);
    a = (jl_array_t*)jl_gc_alloc(ptls, tsz, atype);
    // No allocation or safepoint allowed after this
    a->flags.pooled = tsz <= GC_MAX_SZCLASS;
    a->data = data;
#ifdef STORE_ARRAY_LEN
    a->length = nel;
#endif
    a->elsize = elsz;
    a->flags.ptrarray = !isunboxed;
    a->flags.ndims = 1;
    a->flags.isshared = 1;
    a->flags.isaligned = 0;  // TODO: allow passing memalign'd buffers
    if (own_buffer) {
        a->flags.how = 2;
        jl_gc_track_malloced_array(ptls, a);
        jl_gc_count_allocd(nel*elsz + (elsz == 1 ? 1 : 0));
    }
    else {
        a->flags.how = 0;
    }

    a->nrows = nel;
    a->maxsize = nel;
    a->offset = 0;
    return a;
}

JL_DLLEXPORT jl_array_t *jl_ptr_to_array(jl_value_t *atype, void *data,
                                         jl_value_t *_dims, int own_buffer)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    size_t elsz, nel = 1;
    jl_array_t *a;
    size_t ndims = jl_nfields(_dims);
    wideint_t prod;
    assert(is_ntuple_long(_dims));
    size_t *dims = (size_t*)_dims;
    for (size_t i = 0; i < ndims; i++) {
        prod = (wideint_t)nel * (wideint_t)dims[i];
        if (prod > (wideint_t) MAXINTVAL)
            jl_error("invalid Array dimensions");
        nel = prod;
    }
    if (__unlikely(ndims == 1))
        return jl_ptr_to_array_1d(atype, data, nel, own_buffer);
    jl_value_t *el_type = jl_tparam0(atype);

    int isunboxed = store_unboxed(el_type);
    if (isunboxed)
        elsz = jl_datatype_size(el_type);
    else
        elsz = sizeof(void*);

    int ndimwords = jl_array_ndimwords(ndims);
    int tsz = JL_ARRAY_ALIGN(sizeof(jl_array_t) + ndimwords*sizeof(size_t), JL_CACHE_BYTE_ALIGNMENT);
    a = (jl_array_t*)jl_gc_alloc(ptls, tsz, atype);
    // No allocation or safepoint allowed after this
    a->flags.pooled = tsz <= GC_MAX_SZCLASS;
    a->data = data;
#ifdef STORE_ARRAY_LEN
    a->length = nel;
#endif
    a->elsize = elsz;
    a->flags.ptrarray = !isunboxed;
    a->flags.ndims = ndims;
    a->offset = 0;
    a->flags.isshared = 1;
    a->flags.isaligned = 0;
    if (own_buffer) {
        a->flags.how = 2;
        jl_gc_track_malloced_array(ptls, a);
        jl_gc_count_allocd(nel*elsz + (elsz == 1 ? 1 : 0));
    }
    else {
        a->flags.how = 0;
    }

    assert(ndims != 1); // handled above
    memcpy(&a->nrows, dims, ndims * sizeof(size_t));
    return a;
}

JL_DLLEXPORT jl_array_t *jl_new_array(jl_value_t *atype, jl_value_t *_dims)
{
    size_t ndims = jl_nfields(_dims);
    assert(is_ntuple_long(_dims));
    return _new_array(atype, ndims, (size_t*)_dims);
}

JL_DLLEXPORT jl_array_t *jl_alloc_array_1d(jl_value_t *atype, size_t nr)
{
    return _new_array(atype, 1, &nr);
}

JL_DLLEXPORT jl_array_t *jl_alloc_array_2d(jl_value_t *atype, size_t nr,
                                           size_t nc)
{
    size_t d[2] = {nr, nc};
    return _new_array(atype, 2, &d[0]);
}

JL_DLLEXPORT jl_array_t *jl_alloc_array_3d(jl_value_t *atype, size_t nr,
                                           size_t nc, size_t z)
{
    size_t d[3] = {nr, nc, z};
    return _new_array(atype, 3, &d[0]);
}

JL_DLLEXPORT jl_array_t *jl_pchar_to_array(const char *str, size_t len)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_uint8_type, len);
    memcpy(a->data, str, len);
    return a;
}

JL_DLLEXPORT jl_value_t *jl_array_to_string(jl_array_t *a)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (!jl_typeis(a, jl_array_uint8_type))
        jl_type_error("jl_array_to_string", (jl_value_t*)jl_array_uint8_type, (jl_value_t*)a);
    jl_value_t *s = jl_gc_alloc(ptls, sizeof(void*), jl_string_type);
    jl_set_nth_field(s, 0, (jl_value_t*)a);
    return s;
}

JL_DLLEXPORT jl_value_t *jl_pchar_to_string(const char *str, size_t len)
{
    jl_array_t *a = jl_pchar_to_array(str, len);
    JL_GC_PUSH1(&a);
    jl_value_t *s = jl_array_to_string(a);
    JL_GC_POP();
    return s;
}

JL_DLLEXPORT jl_value_t *jl_cstr_to_string(const char *str)
{
    return jl_pchar_to_string(str, strlen(str));
}

JL_DLLEXPORT jl_array_t *jl_alloc_vec_any(size_t n)
{
    return jl_alloc_array_1d(jl_array_any_type, n);
}

JL_DLLEXPORT jl_value_t *jl_apply_array_type(jl_datatype_t *type, size_t dim)
{
    jl_value_t *boxed_dim = jl_box_long(dim);
    JL_GC_PUSH1(&boxed_dim);
    jl_value_t *ret = jl_apply_type((jl_value_t*)jl_array_type, jl_svec2(type, boxed_dim));
    JL_GC_POP();
    return ret;
}

// array primitives -----------------------------------------------------------

#ifndef STORE_ARRAY_LEN
JL_DLLEXPORT size_t jl_array_len_(jl_array_t *a)
{
    size_t l = 1;
    for(size_t i=0; i < jl_array_ndims(a); i++)
        l *= jl_array_dim(a, i);
    return l;
}
#endif

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

JL_DLLEXPORT jl_value_t *jl_arrayref(jl_array_t *a, size_t i)
{
    assert(i < jl_array_len(a));
    jl_value_t *elt;
    if (!a->flags.ptrarray) {
        jl_value_t *el_type = (jl_value_t*)jl_tparam0(jl_typeof(a));
        elt = jl_new_bits(el_type, &((char*)a->data)[i*a->elsize]);
    }
    else {
        elt = ((jl_value_t**)a->data)[i];
        if (elt == NULL) {
            jl_throw(jl_undefref_exception);
        }
    }
    return elt;
}

static size_t array_nd_index(jl_array_t *a, jl_value_t **args, size_t nidxs,
                             const char *fname)
{
    size_t i=0;
    size_t k, stride=1;
    size_t nd = jl_array_ndims(a);
    for(k=0; k < nidxs; k++) {
        if (!jl_is_long(args[k]))
            jl_type_error(fname, (jl_value_t*)jl_long_type, args[k]);
        size_t ii = jl_unbox_long(args[k])-1;
        i += ii * stride;
        size_t d = k>=nd ? 1 : jl_array_dim(a, k);
        if (k < nidxs-1 && ii >= d)
            jl_bounds_error_v((jl_value_t*)a, args, nidxs);
        stride *= d;
    }
    for(; k < nd; k++)
        stride *= jl_array_dim(a, k);
    if (i >= stride)
        jl_bounds_error_v((jl_value_t*)a, args, nidxs);
    return i;
}

JL_CALLABLE(jl_f_arrayref)
{
    JL_NARGSV(arrayref, 2);
    JL_TYPECHK(arrayref, array, args[0]);
    jl_array_t *a = (jl_array_t*)args[0];
    size_t i = array_nd_index(a, &args[1], nargs-1, "arrayref");
    return jl_arrayref(a, i);
}

JL_DLLEXPORT int jl_array_isassigned(jl_array_t *a, size_t i)
{
    if (a->flags.ptrarray)
        return ((jl_value_t**)jl_array_data(a))[i] != NULL;
    return 1;
}

int jl_array_isdefined(jl_value_t **args0, int nargs)
{
    assert(jl_is_array(args0[0]));
    jl_array_t *a = (jl_array_t*)args0[0];
    jl_value_t **args = &args0[1];
    size_t nidxs = nargs-1;
    size_t i=0;
    size_t k, stride=1;
    size_t nd = jl_array_ndims(a);
    for(k=0; k < nidxs; k++) {
        if (!jl_is_long(args[k]))
            jl_type_error("isdefined", (jl_value_t*)jl_long_type, args[k]);
        size_t ii = jl_unbox_long(args[k])-1;
        i += ii * stride;
        size_t d = k>=nd ? 1 : jl_array_dim(a, k);
        if (k < nidxs-1 && ii >= d)
            return 0;
        stride *= d;
    }
    for(; k < nd; k++)
        stride *= jl_array_dim(a, k);
    if (i >= stride)
        return 0;

    if (a->flags.ptrarray)
        return ((jl_value_t**)jl_array_data(a))[i] != NULL;
    return 1;
}

JL_DLLEXPORT void jl_arrayset(jl_array_t *a, jl_value_t *rhs, size_t i)
{
    assert(i < jl_array_len(a));
    jl_value_t *el_type = jl_tparam0(jl_typeof(a));
    if (el_type != (jl_value_t*)jl_any_type) {
        if (!jl_subtype(rhs, el_type, 1))
            jl_type_error("arrayset", el_type, rhs);
    }
    if (!a->flags.ptrarray) {
        jl_assign_bits(&((char*)a->data)[i*a->elsize], rhs);
    }
    else {
        ((jl_value_t**)a->data)[i] = rhs;
        jl_gc_wb(jl_array_owner(a), rhs);
    }
}

JL_CALLABLE(jl_f_arrayset)
{
    JL_NARGSV(arrayset, 3);
    JL_TYPECHK(arrayset, array, args[0]);
    jl_array_t *a = (jl_array_t*)args[0];
    size_t i = array_nd_index(a, &args[2], nargs-2, "arrayset");
    jl_arrayset(a, args[1], i);
    return args[0];
}

JL_DLLEXPORT void jl_arrayunset(jl_array_t *a, size_t i)
{
    if (i >= jl_array_len(a))
        jl_bounds_error_int((jl_value_t*)a, i+1);
    char *ptail = (char*)a->data + i*a->elsize;
    if (a->flags.ptrarray)
        memset(ptail, 0, a->elsize);
}

// at this size and bigger, allocate resized array data with malloc
#define MALLOC_THRESH 1048576

// Resize the buffer to a max size of `newlen`
// The buffer can either be newly allocated or realloc'd, the return
// value is 1 if a new buffer is allocated and 0 if it is realloc'd.
// the caller needs to take care of moving the data from the old buffer
// to the new one if necessary.
// When this function returns, the `->data` pointer always points to
// the **beginning** of the new buffer.
static int NOINLINE array_resize_buffer(jl_array_t *a, size_t newlen)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(!a->flags.isshared || a->flags.how == 3);
    size_t elsz = a->elsize;
    size_t nbytes = newlen * elsz;
    size_t oldnbytes = a->maxsize * elsz;
    size_t oldoffsnb = a->offset * elsz;
    size_t oldlen = a->nrows;
    assert(nbytes >= oldnbytes);
    if (elsz == 1) {
        nbytes++;
        oldnbytes++;
    }
    int newbuf = 0;
    if (a->flags.how == 2) {
        // already malloc'd - use realloc
        char *olddata = (char*)a->data - oldoffsnb;
        a->data = jl_gc_managed_realloc(olddata, nbytes, oldnbytes,
                                        a->flags.isaligned, (jl_value_t*)a);
    }
    else {
        newbuf = 1;
        if (
#ifdef _P64
            nbytes >= MALLOC_THRESH
#else
            elsz > 4
#endif
            ) {
            a->data = jl_gc_managed_malloc(nbytes);
            jl_gc_track_malloced_array(ptls, a);
            a->flags.how = 2;
            a->flags.isaligned = 1;
        }
        else {
            a->data = jl_gc_alloc_buf(ptls, nbytes);
            a->flags.how = 1;
            jl_gc_wb_buf(a, a->data, nbytes);
        }
    }
    if (JL_ARRAY_IMPL_NUL && elsz == 1)
        memset((char*)a->data + oldnbytes - 1, 0, nbytes - oldnbytes + 1);
    (void)oldlen;
    assert(oldlen == a->nrows &&
           "Race condition detected: recursive resizing on the same array.");
    a->flags.isshared = 0;
    a->maxsize = newlen;
    return newbuf;
}

static void NOINLINE array_try_unshare(jl_array_t *a)
{
    if (a->flags.isshared) {
        if (a->flags.how != 3)
            jl_error("cannot resize array with shared data");
        assert(a->offset == 0);
        size_t len = jl_array_nrows(a);
        size_t es = a->elsize;
        size_t nbytes = len * es;
        char *olddata = (char*)a->data;
        int newbuf = array_resize_buffer(a, len);
        assert(newbuf);
        (void)newbuf;
        memcpy(a->data, olddata, nbytes);
    }
}

static size_t limit_overallocation(jl_array_t *a, size_t alen, size_t newlen, size_t inc)
{
    // Limit overallocation to jl_arr_xtralloc_limit
    size_t es = a->elsize;
    size_t xtra_elems_mem = (newlen - a->offset - alen - inc) * es;
    if (xtra_elems_mem > jl_arr_xtralloc_limit) {
        // prune down
        return alen + inc + a->offset + (jl_arr_xtralloc_limit / es);
    }
    return newlen;
}

STATIC_INLINE void jl_array_grow_at_beg(jl_array_t *a, size_t idx, size_t inc,
                                        size_t n)
{
    // designed to handle the case of growing and shrinking at both ends
    if (__unlikely(a->flags.isshared)) {
        if (a->flags.how != 3)
            jl_error("cannot resize array with shared data");
        if (inc == 0) {
            // If inc > 0, it will always trigger the slow path and unshare the
            // buffer
            array_try_unshare(a);
            return;
        }
    }
    size_t newnrows = n + inc;
    size_t elsz = a->elsize;
    size_t nbinc = inc * elsz;
    char *data = (char*)a->data;
    char *newdata;
    if (a->offset >= inc) {
        assert(!a->flags.isshared);
        newdata = data - nbinc;
        a->offset -= inc;
        if (idx > 0) {
            memmove(newdata, data, idx * elsz);
        }
    }
    else {
        size_t oldoffsnb = a->offset * elsz;
        size_t nb1 = idx * elsz;
        if (inc > (a->maxsize - n) / 2 - (a->maxsize - n) / 20) {
            size_t newlen = a->maxsize == 0 ? inc * 2 : a->maxsize * 2;
            while (n + 2 * inc > newlen - a->offset)
                newlen *= 2;
            newlen = limit_overallocation(a, n, newlen, 2 * inc);
            size_t newoffset = (newlen - newnrows) / 2;
            if (!array_resize_buffer(a, newlen))
                data = (char*)a->data + oldoffsnb;
            newdata = (char*)a->data + newoffset * elsz;
            // We could use memcpy if resizing allocates a new buffer,
            // hopefully it's not a particularly important optimization.
            if (idx > 0 && newdata < data)
                memmove(newdata, data, nb1);
            memmove(newdata + nbinc + nb1, data + nb1, n * elsz - nb1);
            if (idx > 0 && newdata > data)
                memmove(newdata, data, nb1);
            a->offset = newoffset;
        }
        else {
            assert(!a->flags.isshared);
            a->offset = (a->maxsize - newnrows) / 2;
            newdata = data - oldoffsnb + a->offset * elsz;
            // We could use memcpy if resizing allocates a new buffer,
            // hopefully it's not a particularly important optimization.
            if (idx > 0 && newdata < data)
                memmove(newdata, data, nb1);
            memmove(newdata + nbinc + nb1, data + nb1, n * elsz - nb1);
            if (idx > 0 && newdata > data)
                memmove(newdata, data, nb1);
        }
    }
#ifdef STORE_ARRAY_LEN
    a->length = newnrows;
#endif
    a->nrows = newnrows;
    a->data = newdata;
    if (a->flags.ptrarray) {
        memset(newdata + idx * elsz, 0, nbinc);
    }
}

STATIC_INLINE void jl_array_grow_at_end(jl_array_t *a, size_t idx,
                                        size_t inc, size_t n)
{
    // optimized for the case of only growing and shrinking at the end
    if (__unlikely(a->flags.isshared)) {
        if (a->flags.how != 3)
            jl_error("cannot resize array with shared data");
        if (inc == 0) {
            // If inc > 0, it will always trigger the slow path and unshare the
            // buffer
            array_try_unshare(a);
            return;
        }
    }
    size_t elsz = a->elsize;
    char *data = (char*)a->data;
    int has_gap = n > idx;
    if (__unlikely((n + inc) > a->maxsize - a->offset)) {
        size_t nb1 = idx * elsz;
        size_t nbinc = inc * elsz;
        size_t newlen = a->maxsize == 0 ? (inc < 4 ? 4 : inc) : a->maxsize * 2;
        while ((n + inc) > newlen - a->offset)
            newlen *= 2;
        newlen = limit_overallocation(a, n, newlen, inc);
        int newbuf = array_resize_buffer(a, newlen);
        char *newdata = (char*)a->data + a->offset * elsz;
        if (newbuf) {
            memcpy(newdata, data, nb1);
            if (has_gap) {
                memcpy(newdata + nb1 + nbinc, data + nb1, n * elsz - nb1);
            }
        }
        else if (has_gap) {
            memmove(newdata + nb1 + nbinc, newdata + nb1, n * elsz - nb1);
        }
        a->data = data = newdata;
    }
    else if (has_gap) {
        size_t nb1 = idx * elsz;
        memmove(data + nb1 + inc * elsz, data + nb1, n * elsz - nb1);
    }
    size_t newnrows = n + inc;
#ifdef STORE_ARRAY_LEN
    a->length = newnrows;
#endif
    a->nrows = newnrows;
    if (a->flags.ptrarray) {
        memset(data + idx * elsz, 0, inc * elsz);
    }
}

JL_DLLEXPORT void jl_array_grow_at(jl_array_t *a, ssize_t idx, size_t inc)
{
    // No need to explicitly unshare.
    // Shared arrays are guaranteed to trigger the slow path for growing.
    size_t n = jl_array_nrows(a);
    if (idx < 0 || idx > n)
        jl_bounds_error_int((jl_value_t*)a, idx + 1);
    if (idx + 1 < n / 2) {
        jl_array_grow_at_beg(a, idx, inc, n);
    }
    else {
        jl_array_grow_at_end(a, idx, inc, n);
    }
}

JL_DLLEXPORT void jl_array_grow_end(jl_array_t *a, size_t inc)
{
    size_t n = jl_array_nrows(a);
    jl_array_grow_at_end(a, n, inc, n);
}

JL_DLLEXPORT void jl_array_grow_beg(jl_array_t *a, size_t inc)
{
    size_t n = jl_array_nrows(a);
    jl_array_grow_at_beg(a, 0, inc, n);
}

static size_t jl_array_limit_offset(jl_array_t *a, size_t offset)
{
    // make sure offset doesn't grow forever due to deleting at beginning
    // and growing at end
    if (offset >= 13 * a->maxsize / 20)
        offset = 17 * (a->maxsize - a->nrows) / 100;
#ifdef _P64
    while (offset > (size_t)UINT32_MAX) {
        offset /= 2;
    }
#endif
    return offset;
}

STATIC_INLINE void jl_array_del_at_beg(jl_array_t *a, size_t idx, size_t dec,
                                       size_t n)
{
    // no error checking
    // assume inbounds, assume unshared
    assert(!a->flags.isshared);
    size_t elsz = a->elsize;
    size_t offset = a->offset;
    offset += dec;
#ifdef STORE_ARRAY_LEN
    a->length = n - dec;
#endif
    a->nrows = n - dec;
    size_t newoffs = jl_array_limit_offset(a, offset);
    assert(newoffs <= offset);
    size_t nbdec = dec * elsz;
    if (__unlikely(newoffs != offset) || idx > 0) {
        char *olddata = (char*)a->data;
        char *newdata = olddata - (a->offset - newoffs) * elsz;
        size_t nb1 = idx * elsz; // size in bytes of the first block
        size_t nbtotal = a->nrows * elsz; // size in bytes of the new array
        // Implicit '\0' for byte arrays
        if (elsz == 1)
            nbtotal++;
        if (idx > 0)
            memmove(newdata, olddata, nb1);
        memmove(newdata + nb1, olddata + nb1 + nbdec, nbtotal - nb1);
        a->data = newdata;
    }
    else {
        a->data = (char*)a->data + nbdec;
    }
    a->offset = newoffs;
}

STATIC_INLINE void jl_array_del_at_end(jl_array_t *a, size_t idx, size_t dec,
                                       size_t n)
{
    // no error checking
    // assume inbounds, assume unshared
    assert(!a->flags.isshared);
    char *data = (char*)a->data;
    size_t elsz = a->elsize;
    size_t last = idx + dec;
    if (n > last)
        memmove(data + idx * elsz, data + last * elsz, (n - last) * elsz);
    n -= dec;
    if (elsz == 1)
        data[n] = 0;
    a->nrows = n;
#ifdef STORE_ARRAY_LEN
    a->length = n;
#endif
}

JL_DLLEXPORT void jl_array_del_at(jl_array_t *a, ssize_t idx, size_t dec)
{
    size_t n = jl_array_nrows(a);
    size_t last = idx + dec;
    if (__unlikely(idx < 0))
        jl_bounds_error_int((jl_value_t*)a, idx + 1);
    if (__unlikely(last > n))
        jl_bounds_error_int((jl_value_t*)a, last);
    // The unsharing needs to happen before we modify the buffer
    if (__unlikely(a->flags.isshared))
        array_try_unshare(a);
    if (idx < n - last) {
        jl_array_del_at_beg(a, idx, dec, n);
    }
    else {
        jl_array_del_at_end(a, idx, dec, n);
    }
}

JL_DLLEXPORT void jl_array_del_beg(jl_array_t *a, size_t dec)
{
    size_t n = jl_array_nrows(a);
    if (__unlikely(dec > n))
        jl_bounds_error_int((jl_value_t*)a, dec);
    if (__unlikely(a->flags.isshared))
        array_try_unshare(a);
    jl_array_del_at_beg(a, 0, dec, n);
}

JL_DLLEXPORT void jl_array_del_end(jl_array_t *a, size_t dec)
{
    size_t n = jl_array_nrows(a);
    if (__unlikely(n < dec))
        jl_bounds_error_int((jl_value_t*)a, 0);
    if (__unlikely(a->flags.isshared))
        array_try_unshare(a);
    jl_array_del_at_end(a, n - dec, dec, n);
}

JL_DLLEXPORT void jl_array_sizehint(jl_array_t *a, size_t sz)
{
    size_t n = jl_array_nrows(a);
    if (sz <= n)
        return;
    size_t inc = sz - n;
    jl_array_grow_end(a, inc);
    a->nrows = n;
#ifdef STORE_ARRAY_LEN
    a->length = n;
#endif
}

JL_DLLEXPORT jl_array_t *jl_array_copy(jl_array_t *ary)
{
    size_t elsz = ary->elsize;
    jl_array_t *new_ary = _new_array_(jl_typeof(ary), jl_array_ndims(ary),
                                      &ary->nrows, !ary->flags.ptrarray, elsz);
    memcpy(new_ary->data, ary->data, jl_array_len(ary) * elsz);
    return new_ary;
}

// Copy element by element until we hit a young object, at which point
// we can continue using `memmove`.
static NOINLINE ssize_t jl_array_ptr_copy_forward(jl_value_t *owner,
                                                  void **src_p, void **dest_p,
                                                  ssize_t n)
{
    for (ssize_t i = 0; i < n; i++) {
        void *val = src_p[i];
        dest_p[i] = val;
        // `val` is young or old-unmarked
        if (val && !(jl_astaggedvalue(val)->bits.gc & GC_MARKED)) {
            jl_gc_queue_root(owner);
            return i;
        }
    }
    return n;
}

static NOINLINE ssize_t jl_array_ptr_copy_backward(jl_value_t *owner,
                                                   void **src_p, void **dest_p,
                                                   ssize_t n)
{
    for (ssize_t i = 0; i < n; i++) {
        void *val = src_p[n - i - 1];
        dest_p[n - i - 1] = val;
        // `val` is young or old-unmarked
        if (val && !(jl_astaggedvalue(val)->bits.gc & GC_MARKED)) {
            jl_gc_queue_root(owner);
            return i;
        }
    }
    return n;
}

// Unsafe, assume inbounds and that dest and src have the same eltype
JL_DLLEXPORT void jl_array_ptr_copy(jl_array_t *dest, void **dest_p,
                                    jl_array_t *src, void **src_p, ssize_t n)
{
    assert(dest->flags.ptrarray && src->flags.ptrarray);
    jl_value_t *owner = jl_array_owner(dest);
    // Destination is old and doesn't refer to any young object
    if (__unlikely(jl_astaggedvalue(owner)->bits.gc == GC_OLD_MARKED)) {
        jl_value_t *src_owner = jl_array_owner(src);
        // Source is young or might refer to young objects
        if (!(jl_astaggedvalue(src_owner)->bits.gc & GC_OLD)) {
            ssize_t done;
            if (dest_p < src_p || dest_p > src_p + n) {
                done = jl_array_ptr_copy_forward(owner, src_p, dest_p, n);
                dest_p += done;
                src_p += done;
            }
            else {
                done = jl_array_ptr_copy_backward(owner, src_p, dest_p, n);
            }
            n -= done;
        }
    }
    memmove(dest_p, src_p, n * sizeof(void*));
}

JL_DLLEXPORT void jl_array_ptr_1d_push(jl_array_t *a, jl_value_t *item)
{
    assert(jl_typeis(a, jl_array_any_type));
    jl_array_grow_end(a, 1);
    size_t n = jl_array_nrows(a);
    jl_array_ptr_set(a, n - 1, item);
}

JL_DLLEXPORT void jl_array_ptr_1d_push2(jl_array_t *a, jl_value_t *b, jl_value_t *c)
{
    assert(jl_typeis(a, jl_array_any_type));
    jl_array_grow_end(a, 2);
    size_t n = jl_array_nrows(a);
    jl_array_ptr_set(a, n - 2, b);
    jl_array_ptr_set(a, n - 1, c);
}

JL_DLLEXPORT jl_value_t *(jl_array_data_owner)(jl_array_t *a)
{
    return jl_array_data_owner(a);
}

STATIC_INLINE int jl_has_implicit_byte_owned(jl_array_t *a)
{
    assert(a->flags.how != 3);
    if (!a->flags.isshared)
        return 1;
    return a->flags.how == 1;
}

STATIC_INLINE int jl_has_implicit_byte(jl_array_t *a)
{
    // * unshared:
    //   * how: 0-2
    //     We own and allocated the data.
    //     It should have the extra byte.
    // * shared:
    //   * how: 0, 2
    //     The data might come from external source without implicit NUL byte.
    //     There could be an entra byte for a `reinterpreted` array
    //     but that should be unlikely for strings.
    //   * how: 1
    //     We allocated the data with the extra byte.
    //   * how: 3
    //     We should check the owner.
    if (a->flags.how == 3) {
        a = (jl_array_t*)jl_array_data_owner(a);
        return a->elsize == 1 && jl_has_implicit_byte_owned(a);
    }
    return jl_has_implicit_byte_owned(a);
}

// Create an array with the same content
JL_DLLEXPORT jl_array_t *jl_array_cconvert_cstring(jl_array_t *a)
{
    assert(jl_typeof(a) == jl_array_uint8_type);
    if (!jl_has_implicit_byte(a))
        a = jl_array_copy(a);
    ((char*)a->data)[a->nrows] = 0;
    return a;
}

#ifdef __cplusplus
}
#endif
