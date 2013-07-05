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

// array constructors ---------------------------------------------------------

static inline int store_unboxed(jl_value_t *el_type)
{
    return jl_is_datatype(el_type) && jl_is_leaf_type(el_type) && jl_is_immutable(el_type) && jl_is_pointerfree((jl_datatype_t*)el_type);
}

int jl_array_store_unboxed(jl_value_t *el_type)
{
    return store_unboxed(el_type);
}

// at this size use malloc
#define MALLOC_THRESH 1048576

#ifdef _P64
typedef __uint128_t wideint_t;
#else
typedef uint64_t wideint_t;
#endif

static jl_array_t *_new_array(jl_value_t *atype, uint32_t ndims, size_t *dims)
{
    size_t i, tot, nel=1;
    wideint_t prod;
    int isunboxed=0, elsz;
    void *data;
    jl_array_t *a;

    for(i=0; i < ndims; i++) {
        prod = (wideint_t)nel * (wideint_t)dims[i];
        if (prod > (wideint_t)(size_t)-1)
            jl_error("invalid Array dimensions");
        nel = prod;
    }
    jl_value_t *el_type = jl_tparam0(atype);

    isunboxed = store_unboxed(el_type);
    if (isunboxed) {
        elsz = jl_datatype_size(el_type);
        prod = (wideint_t)elsz * (wideint_t)nel;
        if (prod > (wideint_t)(size_t)-1)
            jl_error("invalid Array size");
        tot = prod;
        if (elsz == 1) {
            // hidden 0 terminator for all byte arrays
            tot++;
        }
    }
    else {
        elsz = sizeof(void*);
        prod = (wideint_t)sizeof(void*) * (wideint_t)nel;
        if (prod > (wideint_t)(size_t)-1)
            jl_error("invalid Array size");
        tot = prod;
    }

    int ndimwords = jl_array_ndimwords(ndims);
    size_t tsz = sizeof(jl_array_t);
    tsz += ndimwords*sizeof(size_t);
    if (tot <= ARRAY_INLINE_NBYTES) {
        if (isunboxed && elsz >= 4)
            tsz = (tsz+15)&-16; // align data area 16
        size_t doffs = tsz;
        tsz += tot;
        tsz = (tsz+15)&-16; // align whole object 16
        a = allocobj(tsz);
        a->type = atype;
        a->how = 0;
        data = (char*)a + doffs;
        if (tot > 0 && !isunboxed) {
            memset(data, 0, tot);
        }
    }
    else {
        tsz = (tsz+15)&-16; // align whole object size 16
        a = allocobj(tsz);
        JL_GC_PUSH1(&a);
        a->type = atype;
        // temporarily initialize to make gc-safe
        a->data = NULL;
        a->how = 2;
        data = jl_gc_managed_malloc(tot);
        jl_gc_track_malloced_array(a);
        if (!isunboxed)
            memset(data, 0, tot);
        JL_GC_POP();
    }

    a->data = data;
    if (elsz == 1) ((char*)data)[tot-1] = '\0';
#ifdef STORE_ARRAY_LEN
    a->length = nel;
#endif
    a->ndims = ndims;
    a->ptrarray = !isunboxed;
    a->elsize = elsz;
    a->isshared = 0;
    a->isaligned = 1;
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

jl_array_t *jl_reshape_array(jl_value_t *atype, jl_array_t *data, jl_tuple_t *dims)
{
    size_t i;
    jl_array_t *a;
    size_t ndims = jl_tuple_len(dims);

    int ndimwords = jl_array_ndimwords(ndims);
    a = allocobj((sizeof(jl_array_t) + sizeof(void*) + ndimwords*sizeof(size_t) + 15)&-16);
    a->type = atype;
    a->ndims = ndims;
    a->offset = 0;
    a->data = NULL;
    a->isaligned = data->isaligned;
    jl_value_t *el_type = jl_tparam0(atype);
    if (store_unboxed(el_type)) {
        a->elsize = jl_datatype_size(el_type);
        a->ptrarray = 0;
    }
    else {
        a->elsize = sizeof(void*);
        a->ptrarray = 1;
    }
    JL_GC_PUSH1(&a);

    jl_array_data_owner(a) = (jl_value_t*)data;
    a->how = 3;
    a->data = data->data;
    a->isshared = 1;
    data->isshared = 1;

    if (ndims == 1) {
        size_t l = jl_unbox_long(jl_tupleref(dims,0));
#ifdef STORE_ARRAY_LEN
        a->length = l;
#endif
        a->nrows = l;
        a->maxsize = l;
    }
    else {
        size_t *adims = &a->nrows;
        size_t l=1;
        wideint_t prod;
        for(i=0; i < ndims; i++) {
            adims[i] = jl_unbox_long(jl_tupleref(dims, i));
            prod = (wideint_t)l * (wideint_t)adims[i];
            if (prod > (wideint_t)(size_t)-1)
                jl_error("invalid Array dimensions");
            l = prod;
        }
#ifdef STORE_ARRAY_LEN
        a->length = l;
#endif
    }
    JL_GC_POP();

    return a;
}

// own_buffer != 0 iff GC should call free() on this pointer eventually
jl_array_t *jl_ptr_to_array_1d(jl_value_t *atype, void *data, size_t nel,
                               int own_buffer)
{
    size_t elsz;
    jl_array_t *a;
    jl_value_t *el_type = jl_tparam0(atype);

    int isunboxed = store_unboxed(el_type);
    if (isunboxed)
        elsz = jl_datatype_size(el_type);
    else
        elsz = sizeof(void*);

    a = allocobj((sizeof(jl_array_t)+jl_array_ndimwords(1)*sizeof(size_t)+15)&-16);
    a->type = atype;
    a->data = data;
#ifdef STORE_ARRAY_LEN
    a->length = nel;
#endif
    a->elsize = elsz;
    a->ptrarray = !isunboxed;
    a->ndims = 1;
    a->isshared = 1;
    a->isaligned = 0;  // TODO: allow passing memalign'd buffers
    if (own_buffer) {
        a->how = 2;
        jl_gc_track_malloced_array(a);
    }
    else {
        a->how = 0;
    }

    a->nrows = nel;
    a->maxsize = nel;
    a->offset = 0;
    return a;
}

jl_array_t *jl_ptr_to_array(jl_value_t *atype, void *data, jl_tuple_t *dims,
                            int own_buffer)
{
    size_t i, elsz, nel=1;
    jl_array_t *a;
    size_t ndims = jl_tuple_len(dims);
    wideint_t prod;

    for(i=0; i < ndims; i++) {
        prod = (wideint_t)nel * (wideint_t)jl_unbox_long(jl_tupleref(dims, i));
        if (prod > (wideint_t)(size_t)-1)
            jl_error("invalid Array dimensions");
        nel = prod;
    }
    jl_value_t *el_type = jl_tparam0(atype);

    int isunboxed = store_unboxed(el_type);
    if (isunboxed)
        elsz = jl_datatype_size(el_type);
    else
        elsz = sizeof(void*);

    int ndimwords = jl_array_ndimwords(ndims);
    a = allocobj((sizeof(jl_array_t) + ndimwords*sizeof(size_t)+15)&-16);
    a->type = atype;
    a->data = data;
#ifdef STORE_ARRAY_LEN
    a->length = nel;
#endif
    a->elsize = elsz;
    a->ptrarray = !isunboxed;
    a->ndims = ndims;
    a->offset = 0;
    a->isshared = 1;
    a->isaligned = 0;
    if (own_buffer) {
        a->how = 2;
        jl_gc_track_malloced_array(a);
    }
    else {
        a->how = 0;
    }

    if (ndims == 1) {
        a->nrows = nel;
        a->maxsize = nel;
    }
    else {
        size_t *adims = &a->nrows;
        for(i=0; i < ndims; i++) {
            adims[i] = jl_unbox_long(jl_tupleref(dims, i));
        }
    }
    return a;
}

jl_array_t *jl_new_array_(jl_value_t *atype, uint32_t ndims, size_t *dims)
{
    return _new_array(atype, ndims, dims);
}

jl_array_t *jl_new_array(jl_value_t *atype, jl_tuple_t *dims)
{
    size_t ndims = jl_tuple_len(dims);
    size_t *adims = alloca(ndims*sizeof(size_t));
    size_t i;
    for(i=0; i < ndims; i++)
        adims[i] = jl_unbox_long(jl_tupleref(dims,i));
    return _new_array(atype, ndims, adims);
}

jl_array_t *jl_alloc_array_1d(jl_value_t *atype, size_t nr)
{
    return _new_array(atype, 1, &nr);
}

jl_array_t *jl_alloc_array_2d(jl_value_t *atype, size_t nr, size_t nc)
{
    size_t d[2] = {nr, nc};
    return _new_array(atype, 2, &d[0]);
}

jl_array_t *jl_alloc_array_3d(jl_value_t *atype, size_t nr, size_t nc, size_t z)
{
    size_t d[3] = {nr, nc, z};
    return _new_array(atype, 3, &d[0]);
}

jl_array_t *jl_pchar_to_array(const char *str, size_t len)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_uint8_type, len);
    memcpy(a->data, str, len);
    return a;
}

jl_value_t *jl_array_to_string(jl_array_t *a)
{
    // TODO: check type of array?
    jl_datatype_t* string_type = u8_isvalid(a->data, jl_array_len(a)) == 1 ? // ASCII
        jl_ascii_string_type : jl_utf8_string_type;
    jl_value_t *s = alloc_2w();
    s->type = (jl_value_t*)string_type;
    jl_set_nth_field(s, 0, (jl_value_t*)a);
    return s;
}

jl_value_t *jl_pchar_to_string(const char *str, size_t len)
{
    jl_array_t *a = jl_pchar_to_array(str, len);
    JL_GC_PUSH1(&a);
    jl_value_t *s = jl_array_to_string(a);
    JL_GC_POP();
    return s;
}

jl_value_t *jl_cstr_to_string(const char *str)
{
    return jl_pchar_to_string(str, strlen(str));
}

jl_array_t *jl_alloc_cell_1d(size_t n)
{
    return jl_alloc_array_1d(jl_array_any_type, n);
}

// array primitives -----------------------------------------------------------

#ifndef STORE_ARRAY_LEN
DLLEXPORT size_t jl_array_len_(jl_array_t *a)
{
    size_t l = 1;
    for(size_t i=0; i < jl_array_ndims(a); i++)
        l *= jl_array_dim(a, i);
    return l;
}
#endif

JL_CALLABLE(jl_f_arraylen)
{
    JL_NARGS(arraylen, 1, 1);
    JL_TYPECHK(arraylen, array, args[0]);
    return jl_box_long(jl_array_len((jl_array_t*)args[0]));
}

JL_CALLABLE(jl_f_arraysize)
{
    if (nargs != 2) {
        JL_NARGS(arraysize, 1, 1);
    }
    JL_TYPECHK(arraysize, array, args[0]);
    jl_array_t *a = (jl_array_t*)args[0];
    size_t nd = jl_array_ndims(a);
    if (nargs == 2) {
        JL_TYPECHK(arraysize, long, args[1]);
        int dno = jl_unbox_long(args[1]);
        if (dno < 1)
            jl_error("arraysize: dimension out of range");
        if (dno > nd)
            return jl_box_long(1);
        return jl_box_long((&a->nrows)[dno-1]);
    }
    jl_tuple_t *d = jl_alloc_tuple(nd);
    JL_GC_PUSH1(&d);
    size_t i;
    for(i=0; i < nd; i++)
        jl_tupleset(d, i, jl_box_long(jl_array_dim(a,i)));
    JL_GC_POP();
    return (jl_value_t*)d;
}

jl_value_t *jl_arrayref(jl_array_t *a, size_t i)
{
    jl_value_t *el_type = (jl_value_t*)jl_tparam0(jl_typeof(a));
    jl_value_t *elt;
    if (!a->ptrarray) {
        elt = jl_new_bits((jl_datatype_t*)el_type,
                          &((char*)a->data)[i*a->elsize]);
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
                             char *fname)
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
            jl_throw(jl_bounds_exception);
        stride *= d;
    }
    for(; k < nd; k++)
        stride *= jl_array_dim(a, k);
    if (i >= stride)
        jl_throw(jl_bounds_exception);
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

int jl_array_isdefined(jl_value_t **args, int nargs)
{
    assert(jl_is_array(args[0]));
    jl_array_t *a = (jl_array_t*)args[0];
    size_t i = array_nd_index(a, &args[1], nargs-1, "isdefined");
    if (a->ptrarray)
        return ((jl_value_t**)jl_array_data(a))[i] != NULL;
    return 1;
}

void jl_arrayset(jl_array_t *a, jl_value_t *rhs, size_t i)
{
    jl_value_t *el_type = jl_tparam0(jl_typeof(a));
    if (el_type != (jl_value_t*)jl_any_type) {
        if (!jl_subtype(rhs, el_type, 1))
            jl_type_error("arrayset", el_type, rhs);
    }
    if (!a->ptrarray) {
        jl_assign_bits(&((char*)a->data)[i*a->elsize], rhs);
    }
    else {
        ((jl_value_t**)a->data)[i] = rhs;
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

void jl_arrayunset(jl_array_t *a, size_t i)
{
    if (i >= jl_array_len(a))
        jl_throw(jl_bounds_exception);
    char *ptail = (char*)a->data + i*a->elsize;
    if (a->ptrarray)
        memset(ptail, 0, a->elsize);
}

// allocate buffer of newlen elements, placing old data at given offset (in #elts)
static void array_resize_buffer(jl_array_t *a, size_t newlen, size_t oldlen, size_t offs)
{
    size_t es = a->elsize;
    size_t nbytes = newlen * es;
    size_t offsnb = offs * es;
    size_t oldnbytes = oldlen * es;
    size_t oldoffsnb = a->offset * es;
    if (es == 1)
        nbytes++;
    assert(!a->isshared);
    char *newdata;
    if (a->how == 2) {
        // already malloc'd - use realloc
        newdata = jl_gc_managed_realloc((char*)a->data - oldoffsnb, nbytes,
                                        oldnbytes+oldoffsnb, a->isaligned);
        if (offs != a->offset) {
            memmove(&newdata[offsnb], &newdata[oldoffsnb], oldnbytes);
        }
    }
    else {
        if (nbytes >= MALLOC_THRESH) {
            newdata = jl_gc_managed_malloc(nbytes);
            jl_gc_track_malloced_array(a);
            a->how = 2;
            a->isaligned = 1;
        }
        else {
            newdata = allocb(nbytes);
            a->how = 1;
        }
        memcpy(newdata + offsnb, (char*)a->data, oldnbytes);
    }

    a->data = newdata + offsnb;
    if (a->ptrarray || es==1)
        memset(newdata+offsnb+oldnbytes, 0, nbytes-oldnbytes-offsnb);
    a->maxsize = newlen;
}

void jl_array_grow_end(jl_array_t *a, size_t inc)
{
    if (a->isshared) jl_error("cannot resize array with shared data");
    // optimized for the case of only growing and shrinking at the end
    size_t alen = jl_array_nrows(a);
    if ((alen + inc) > a->maxsize - a->offset) {
        size_t newlen = a->maxsize==0 ? (inc<4?4:inc) : a->maxsize*2;
        while ((alen + inc) > newlen - a->offset)
            newlen *= 2;
        array_resize_buffer(a, newlen, alen, a->offset);
    }
#ifdef STORE_ARRAY_LEN
    a->length += inc;
#endif
    a->nrows += inc;
}

void jl_array_del_end(jl_array_t *a, size_t dec)
{
    if (a->isshared) jl_error("cannot resize array with shared data");
    if (dec == 0) return;
    if (dec > a->nrows)
        jl_throw(jl_bounds_exception);
    char *ptail = (char*)a->data + (a->nrows-dec)*a->elsize;
    if (a->ptrarray)
        memset(ptail, 0, dec*a->elsize);
    else
        ptail[0] = 0;
#ifdef STORE_ARRAY_LEN
    a->length -= dec;
#endif
    a->nrows -= dec;
}

void jl_array_sizehint(jl_array_t *a, size_t sz)
{
    if (sz <= jl_array_len(a))
        return;
    size_t inc = sz - jl_array_len(a);
    jl_array_grow_end(a, inc);
#ifdef STORE_ARRAY_LEN
    a->length -= inc;
#endif
    a->nrows -= inc;
}

void jl_array_grow_beg(jl_array_t *a, size_t inc)
{
    if (a->isshared) jl_error("cannot resize array with shared data");
    // designed to handle the case of growing and shrinking at both ends
    if (inc == 0) return;
    size_t es = a->elsize;
    size_t incnb = inc*es;
    if (a->offset >= inc) {
        a->data = (char*)a->data - incnb;
        a->offset -= inc;
    }
    else {
        size_t alen = a->nrows;
        size_t anb = alen*es;
        if (inc > (a->maxsize-alen)/2 - (a->maxsize-alen)/20) {
            size_t newlen = a->maxsize==0 ? inc*2 : a->maxsize*2;
            while (alen+2*inc > newlen-a->offset)
                newlen *= 2;
            size_t center = (newlen - (alen + inc))/2;
            array_resize_buffer(a, newlen, alen, center+inc);
            char *newdata = (char*)a->data - (center+inc)*es;
            if (a->ptrarray) {
                memset(newdata, 0, (center+inc)*es);
            }
            a->offset = center;
            a->data = newdata + center*es;
        }
        else {
            size_t center = (a->maxsize - (alen + inc))/2;
            char *newdata = (char*)a->data - es*a->offset + es*center;
            memmove(&newdata[incnb], a->data, anb);
            a->data = newdata;
            a->offset = center;
        }
    }
#ifdef STORE_ARRAY_LEN
    a->length += inc;
#endif
    a->nrows += inc;
}

void jl_array_del_beg(jl_array_t *a, size_t dec)
{
    if (a->isshared) jl_error("cannot resize array with shared data");
    if (dec == 0) return;
    if (dec > a->nrows)
        jl_throw(jl_bounds_exception);
    size_t es = a->elsize;
    size_t nb = dec*es;
    memset(a->data, 0, nb);
    size_t offset = a->offset;
    offset += dec;
    a->data = (char*)a->data + nb;
#ifdef STORE_ARRAY_LEN
    a->length -= dec;
#endif
    a->nrows -= dec;

    // make sure offset doesn't grow forever due to deleting at beginning
    // and growing at end
    size_t newoffs = offset;
    if (offset >= 13*a->maxsize/20) {
        newoffs = 17*(a->maxsize - a->nrows)/100;
    }
#ifdef _P64
    while (newoffs > (size_t)((uint32_t)-1)) {
        newoffs = newoffs/2;
    }
#endif
    if (newoffs != offset) {
        size_t anb = a->nrows*es;
        size_t delta = (offset - newoffs)*es;
        a->data = (char*)a->data - delta;
        memmove(a->data, (char*)a->data + delta, anb);
    }
    a->offset = newoffs;
}

DLLEXPORT void jl_cell_1d_push(jl_array_t *a, jl_value_t *item)
{
    assert(jl_typeis(a, jl_array_any_type));
    jl_array_grow_end(a, 1);
    jl_cellset(a, jl_array_dim(a,0)-1, item);
}
