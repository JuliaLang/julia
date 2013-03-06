/*
  array constructors and primitives
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef __WIN32__
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

static jl_array_t *_new_array(jl_value_t *atype,
                              uint32_t ndims, size_t *dims)
{
    size_t i, tot, nel=1;
    int isunboxed=0, elsz;
    void *data;
    jl_array_t *a;

    for(i=0; i < ndims; i++) {
        if ((long)dims[i] < 0)
            jl_error("invalid Array dimension size");
        nel *= dims[i];
    }
    jl_value_t *el_type = jl_tparam0(atype);

    isunboxed = store_unboxed(el_type);
    if (isunboxed) {
        elsz = jl_datatype_size(el_type);
        tot = elsz * nel;
        if (elsz == 1) {
            // hidden 0 terminator for all byte arrays
            tot++;
        }
    }
    else {
        elsz = sizeof(void*);
        tot = sizeof(void*) * nel;
    }

    int ndimwords = jl_array_ndimwords(ndims);
    if (tot <= ARRAY_INLINE_NBYTES) {
        size_t tsz = tot>sizeof(size_t) ? tot-sizeof(size_t) : tot;
        a = allocobj((sizeof(jl_array_t)+tsz+ndimwords*sizeof(size_t)+15)&-16);
        a->type = atype;
        a->ismalloc = 0;
        data = (&a->_space[0] + ndimwords*sizeof(size_t));
        if (tot > 0 && !isunboxed) {
            memset(data, 0, tot);
        }
    }
    else {
        a = allocobj((sizeof(jl_array_t)+ndimwords*sizeof(size_t)+15)&-16);
        JL_GC_PUSH(&a);
        a->type = atype;
        a->ismalloc = 1;
        // temporarily initialize to make gc-safe
        a->data = NULL;
        jl_value_t **powner = (jl_value_t**)(&a->_space[0] + ndimwords*sizeof(size_t));
        *powner = (jl_value_t*)jl_gc_managed_malloc(tot);
        data = ((jl_mallocptr_t*)*powner)->ptr;
        if (!isunboxed)
            memset(data, 0, tot);
        JL_GC_POP();
    }

    a->data = data;
    if (elsz == 1) ((char*)data)[tot-1] = '\0';
    a->length = nel;
    a->ndims = ndims;
    a->ptrarray = !isunboxed;
    a->elsize = elsz;
    if (ndims == 1) {
        a->nrows = nel;
        a->maxsize = nel;
        a->offset = 0;
    }
    else {
        size_t *adims = &a->nrows;
        for(i=0; i < ndims; i++)
            adims[i] = dims[i];
    }
    
    return a;
}

static jl_mallocptr_t *array_new_buffer(jl_array_t *a, size_t newlen);

jl_array_t *jl_reshape_array(jl_value_t *atype, jl_array_t *data,
                             jl_tuple_t *dims)
{
    size_t i;
    jl_array_t *a;
    size_t ndims = jl_tuple_len(dims);

    int ndimwords = jl_array_ndimwords(ndims);
    a = allocobj((sizeof(jl_array_t) + ndimwords*sizeof(size_t) + 15)&-16);
    a->type = atype;
    a->ndims = ndims;
    a->data = NULL;
    JL_GC_PUSH(&a);

    char *d = data->data;
    if (data->ndims == 1) d -= data->offset*data->elsize;
    if (d == jl_array_inline_data_area(data)) {
        if (data->ndims == 1) {
            // data might resize, so switch it to shared representation.
            // problem: the buffer might be used from C in a way that it's
            // assumed not to move. for now just hope this doesn't happen.
            size_t datalen = jl_array_len(data);
            jl_mallocptr_t *mp = array_new_buffer(data, datalen);
            memcpy(mp->ptr, data->data, datalen * data->elsize);
            a->data = mp->ptr;
            jl_array_data_owner(a) = (jl_value_t*)mp;
            a->ismalloc = 1;

            data->data = mp->ptr;
            data->offset = 0;
            data->maxsize = datalen;
            jl_array_data_owner(data) = (jl_value_t*)mp;
            data->ismalloc = 1;
        }
        else {
            a->ismalloc = 0;
            jl_array_data_owner(a) = (jl_value_t*)data;
        }
    }
    else {
        a->ismalloc = data->ismalloc;
        jl_array_data_owner(a) = jl_array_data_owner(data);
    }

    if (a->data == NULL) a->data = data->data;
    jl_value_t *el_type = jl_tparam0(atype);
    if (store_unboxed(el_type)) {
        a->elsize = jl_datatype_size(el_type);
        a->ptrarray = 0;
    }
    else {
        a->elsize = sizeof(void*);
        a->ptrarray = 1;
    }

    if (ndims == 1) {
        a->length = jl_unbox_long(jl_tupleref(dims,0));
        a->nrows = a->length;
        a->maxsize = a->length;
        a->offset = 0;
    }
    else {
        size_t *adims = &a->nrows;
        size_t l=1;
        for(i=0; i < ndims; i++) {
            adims[i] = jl_unbox_long(jl_tupleref(dims, i));
            if ((long)adims[i] < 0)
                jl_error("invalid Array dimension size");
            l *= adims[i];
        }
        a->length = l;
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
    a->length = nel;
    a->elsize = elsz;
    a->ptrarray = !isunboxed;
    a->ndims = 1;

    if (own_buffer) {
        a->ismalloc = 1;
        jl_array_data_owner(a) = (jl_value_t*)jl_gc_acquire_buffer(data,nel*elsz);
    }
    else {
        a->ismalloc = 0;
        jl_array_data_owner(a) = (jl_value_t*)a;
    }

    a->nrows = a->length;
    a->maxsize = a->length;
    a->offset = 0;
    
    return a;
}

jl_array_t *jl_ptr_to_array(jl_value_t *atype, void *data, jl_tuple_t *dims,
                            int own_buffer)
{
    size_t i, elsz, nel=1;
    jl_array_t *a;
    size_t ndims = jl_tuple_len(dims);

    for(i=0; i < ndims; i++) {
        nel *= jl_unbox_long(jl_tupleref(dims, i));
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
    a->length = nel;
    a->elsize = elsz;
    a->ptrarray = !isunboxed;
    a->ndims = ndims;

    if (own_buffer) {
        a->ismalloc = 1;
        jl_array_data_owner(a) = (jl_value_t*)jl_gc_acquire_buffer(data,nel*elsz);
    }
    else {
        a->ismalloc = 0;
        jl_array_data_owner(a) = (jl_value_t*)a;
    }

    if (ndims == 1) {
        a->nrows = a->length;
        a->maxsize = a->length;
        a->offset = 0;
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
    JL_GC_PUSH(&a);
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

JL_CALLABLE(jl_f_arraylen)
{
    JL_NARGS(arraylen, 1, 1);
    JL_TYPECHK(arraylen, array, args[0]);
    return jl_box_long(jl_array_len((jl_array_t*)args[0]));
}

JL_CALLABLE(jl_f_arraysize)
{
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
    else {
        JL_NARGS(arraysize, 1, 1);
    }
    jl_tuple_t *d = jl_alloc_tuple(nd);
    JL_GC_PUSH(&d);
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

static jl_mallocptr_t *array_new_buffer(jl_array_t *a, size_t newlen)
{
    size_t nbytes = newlen * a->elsize;
    if (a->elsize == 1) {
        nbytes++;
    }
    jl_mallocptr_t *mp = jl_gc_managed_malloc(nbytes);
    char *newdata = mp->ptr;
    if (a->ptrarray)
        memset(newdata, 0, nbytes);
    if (a->elsize == 1) newdata[nbytes-1] = '\0';
    return mp;
}

void jl_array_grow_end(jl_array_t *a, size_t inc)
{
    // optimized for the case of only growing and shrinking at the end
    size_t alen = jl_array_len(a);
    if ((alen + inc) > a->maxsize - a->offset) {
        size_t newlen = a->maxsize==0 ? (inc<4?4:inc) : a->maxsize*2;
        while ((alen + inc) > newlen - a->offset)
            newlen *= 2;
        jl_mallocptr_t *mp = array_new_buffer(a, newlen);
        char *newdata = mp->ptr;
        size_t es = a->elsize;
        newdata += (a->offset*es);
        size_t anb = alen*es;
        memcpy(newdata, (char*)a->data, anb);
        if (es == 1) {
            memset(newdata + anb, 0, (newlen-a->offset-alen)*es);
        }
        a->maxsize = newlen;
        a->data = newdata;
        jl_array_data_owner(a) = (jl_value_t*)mp;
        a->ismalloc = 1;
    }
    a->length += inc; a->nrows += inc;
}

void jl_array_del_end(jl_array_t *a, size_t dec)
{
    if (dec > a->length)
        jl_throw(jl_bounds_exception);
    char *ptail = (char*)a->data + (a->length-dec)*a->elsize;
    if (a->ptrarray)
        memset(ptail, 0, dec*a->elsize);
    else
        ptail[0] = 0;
    a->length -= dec; a->nrows -= dec;
}

void jl_array_sizehint(jl_array_t *a, size_t sz)
{
    if (sz <= jl_array_len(a))
        return;
    size_t inc = sz - jl_array_len(a);
    jl_array_grow_end(a, inc);
    a->length -= inc; a->nrows -= inc;
}

void jl_array_grow_beg(jl_array_t *a, size_t inc)
{
    // designed to handle the case of growing and shrinking at both ends
    if (inc == 0)
        return;
    size_t es = a->elsize;
    size_t nb = inc*es;
    if (a->offset >= inc) {
        a->data = (char*)a->data - nb;
        a->offset -= inc;
    }
    else {
        size_t alen = a->length;
        size_t anb = alen*es;
        char *newdata;
        jl_mallocptr_t *mp = NULL;
        if (inc > (a->maxsize-alen)/2 - (a->maxsize-alen)/20) {
            size_t newlen = a->maxsize==0 ? 2*inc : a->maxsize*2;
            while (alen+2*inc > newlen-a->offset)
                newlen *= 2;
            mp = array_new_buffer(a, newlen);
            newdata = mp->ptr;
            size_t center = (newlen - (alen + inc))/2;
            newdata += (center*es);
            a->maxsize = newlen;
            a->offset = center;
        }
        else {
            size_t center = (a->maxsize - (alen + inc))/2;
            newdata = (char*)a->data - es*a->offset + es*center;
            a->offset = center;
        }
        memmove(&newdata[nb], a->data, anb);
        a->data = newdata;
        if (mp) { jl_array_data_owner(a) = (jl_value_t*)mp; a->ismalloc = 1; }
    }
    a->length += inc; a->nrows += inc;
}

void jl_array_del_beg(jl_array_t *a, size_t dec)
{
    if (dec == 0)
        return;
    if (dec > a->length)
        jl_throw(jl_bounds_exception);
    size_t es = a->elsize;
    size_t nb = dec*es;
    memset(a->data, 0, nb);
    size_t offset = a->offset;
    offset += dec;
    a->data = (char*)a->data + nb;
    a->length -= dec; a->nrows -= dec;

    // make sure offset doesn't grow forever due to deleting at beginning
    // and growing at end
    size_t newoffs = offset;
    if (offset >= 13*a->maxsize/20) {
        newoffs = 17*(a->maxsize - a->length)/100;
    }
#ifdef _P64
    while (newoffs > (size_t)((uint32_t)-1)) {
        newoffs = newoffs/2;
    }
#endif
    if (newoffs != offset) {
        size_t anb = a->length*es;
        size_t delta = (offset - newoffs)*es;
        a->data = (char*)a->data - delta;
        memmove(a->data, (char*)a->data + delta, anb);
    }
    a->offset = newoffs;
}

void jl_cell_1d_push(jl_array_t *a, jl_value_t *item)
{
    assert(jl_typeis(a, jl_array_any_type));
    jl_array_grow_end(a, 1);
    jl_cellset(a, jl_array_dim(a,0)-1, item);
}
