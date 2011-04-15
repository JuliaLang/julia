/*
  array constructors and primitives
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

// array constructors ---------------------------------------------------------

static void *alloc_array_buffer(size_t nbytes, int isunboxed)
{
    void *data;
    if (nbytes > 0) {
        if (isunboxed) {
#ifdef BOEHM_GC
            if (nbytes >= 200000)
                data = GC_malloc_atomic_ignore_off_page(nbytes);
            else
#endif
                data = alloc_pod(nbytes);
        }
        else {
#ifdef BOEHM_GC
            if (nbytes >= 200000)
                data = GC_malloc_ignore_off_page(nbytes);
            else
#endif
                data = allocb(nbytes);
            memset(data, 0, nbytes);
        }
    }
    else {
        data = NULL;
    }
    return data;
}

static jl_array_t *_new_array(jl_type_t *atype, jl_tuple_t *dimst,
                              uint32_t ndims, size_t *dims)
{
    size_t i, tot;
    size_t nel=1;
    int isbytes=0, isunboxed=0, elsz;
    void *data;
    jl_array_t *a;

    if (ndims == 0) nel = 0;
    for(i=0; i < ndims; i++) {
        nel *= dims[i];
    }
    jl_type_t *el_type = (jl_type_t*)jl_tparam0(atype);

    isunboxed = jl_is_bits_type(el_type);
    if (isunboxed) {
        elsz = jl_bitstype_nbits(el_type)/8;
        tot = elsz * nel;
        if (elsz==1 && ndims==1) {
            // hidden 0 terminator for all byte arrays
            isbytes = 1;
            tot++;
        }
    }
    else {
        elsz = sizeof(void*);
        tot = sizeof(void*) * nel;
    }

    int ndimwords = (ndims > 3 ? (ndims-3) : 0);
    if (tot <= ARRAY_INLINE_NBYTES) {
        a = allocobj(sizeof(jl_array_t) + tot + (ndimwords-1)*sizeof(size_t));
        a->type = atype;
        a->dims = dimst;
        data = (&a->_space[0] + ndimwords*sizeof(size_t));
        if (tot > 0 && !isunboxed) {
            memset(data, 0, tot);
        }
    }
    else {
        a = allocobj(sizeof(jl_array_t) + (ndimwords-1)*sizeof(size_t));
        jl_gc_preserve((jl_value_t*)a);
        a->type = atype;
        a->dims = dimst;
        // temporarily initialize to make gc-safe
        a->data = NULL;
        a->length = 0;
        data = alloc_array_buffer(tot, isunboxed);
        jl_gc_unpreserve();
    }

    a->data = data;
    if (isbytes) ((char*)data)[tot-1] = '\0';
    a->length = nel;
    a->ndims = ndims;
    a->elsize = elsz;
    size_t *adims = &a->nrows;
    if (ndims == 1) {
        a->nrows = nel;
        a->maxsize = tot;
        a->offset = 0;
    }
    else {
        for(i=0; i < ndims; i++)
            adims[i] = dims[i];
    }
    
    return a;
}

jl_array_t *jl_new_array(jl_type_t *atype, jl_tuple_t *dims)
{
    size_t ndims = dims->length;
    size_t *adims = alloca(ndims*sizeof(size_t));
    size_t i;
    for(i=0; i < ndims; i++)
        adims[i] = jl_unbox_int32(jl_tupleref(dims,i));
    return _new_array(atype, dims, ndims, adims);
}

jl_array_t *jl_alloc_array_1d(jl_type_t *atype, size_t nr)
{
    return _new_array(atype, NULL, 1, &nr);
}

JL_CALLABLE(jl_new_array_internal)
{
    jl_struct_type_t *atype = (jl_struct_type_t*)env;
    jl_value_t *ndims = jl_tupleref(atype->parameters,1);
    size_t nd = jl_unbox_int32(ndims);
    JL_NARGS(Array, 1, 1);
    JL_TYPECHK(Array, tuple, args[0]);
    jl_tuple_t *d = (jl_tuple_t*)args[0];
    if (d->length != nd) {
        jl_error("Array: wrong number of dimensions");
    }
    size_t i;
    for(i=0; i < d->length; i++) {
        JL_TYPECHK(Array, int32, jl_tupleref(d,i));
    }
    return (jl_value_t*)jl_new_array((jl_type_t*)atype, d);
}

JL_CALLABLE(jl_generic_array_ctor)
{
    JL_NARGSV(Array, 1);
    JL_TYPECHK(Array, type, args[0]);
    size_t i, nd;
    size_t *dims=NULL;
    jl_tuple_t *d=NULL;
    jl_value_t *vnd=NULL, *vtp=NULL, *vnt=NULL;
    JL_GC_PUSH(&vnd, &vtp, &vnt, &d);
    if (nargs==2 && jl_is_tuple(args[1])) {
        d = (jl_tuple_t*)args[1];
        nd = d->length;
        for(i=0; i < nd; i++) {
            JL_TYPECHK(Array, int32, jl_tupleref(d,i));
        }
    }
    else {
        nd = nargs-1;
        dims = alloca(nd * sizeof(size_t));
        for(i=1; i < nargs; i++) {
            JL_TYPECHK(Array, int32, args[i]);
            dims[i-1] = jl_unbox_int32(args[i]);
        }
        /*
        d = jl_alloc_tuple_uninit(nd);
        for(i=0; i < nd; i++)
            jl_tupleset(d, i, args[i+1]);
        */
    }
    vnd = jl_box_int32(nd);
    vtp = (jl_value_t*)jl_tuple2(args[0], vnd);
    vnt = jl_apply_type((jl_value_t*)jl_array_type, (jl_tuple_t*)vtp);
    jl_value_t *ar;
    if (d != NULL)
        ar = (jl_value_t*)jl_new_array((jl_type_t*)vnt, d);
    else
        ar = (jl_value_t*)_new_array((jl_type_t*)vnt, NULL, nd, dims);
    JL_GC_POP();
    return ar;
}

jl_array_t *jl_pchar_to_array(char *str, size_t len)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_uint8_type, len);
    memcpy(a->data, str, len);
    return a;
}

jl_array_t *jl_cstr_to_array(char *str)
{
    size_t len = strlen(str);
    return jl_pchar_to_array(str, len);
}

jl_value_t *jl_pchar_to_string(char *str, size_t len)
{
    jl_array_t *a = jl_pchar_to_array(str, len);
    JL_GC_PUSH(&a);
    jl_struct_type_t* string_type = u8_isvalid(a->data, len) < 2 ?
        jl_latin1_string_type : jl_utf8_string_type;
    jl_value_t *s = jl_apply((jl_function_t*)string_type, (jl_value_t**)&a, 1);
    JL_GC_POP();
    return s;
}

jl_value_t *jl_cstr_to_string(char *str)
{
    size_t len = strlen(str);
    return jl_pchar_to_string(str, len);
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
    return jl_box_int32(((jl_array_t*)args[0])->length);
}

jl_tuple_t *jl_construct_array_size(jl_array_t *a, size_t nd)
{
    if (a->dims) return a->dims;

    jl_tuple_t *d = jl_alloc_tuple(nd);
    a->dims = d;
    size_t i;
    for(i=0; i < nd; i++)
        jl_tupleset(d, i, jl_box_int32((&a->nrows)[i]));
    return d;
}

JL_CALLABLE(jl_f_arraysize)
{
    JL_TYPECHK(arraysize, array, args[0]);
    jl_array_t *a = (jl_array_t*)args[0];
    size_t nd = jl_array_ndims(a);
    if (nargs == 2) {
        JL_TYPECHK(arraysize, int32, args[1]);
        int dno = jl_unbox_int32(args[1]);
        if (dno < 1 || dno > nd)
            jl_error("arraysize: dimension out of range");
        return jl_box_int32((&a->nrows)[dno-1]);
    }
    else {
        JL_NARGS(arraysize, 1, 1);
    }
    if (a->dims) return (jl_value_t*)a->dims;
    return (jl_value_t*)jl_construct_array_size(a, nd);
}

static jl_value_t *new_scalar(jl_bits_type_t *bt)
{
    size_t nb = jl_bitstype_nbits(bt)/8;
    jl_value_t *v = 
        (jl_value_t*)allocobj((NWORDS(LLT_ALIGN(nb,sizeof(void*)))+1)*
                              sizeof(void*));
    v->type = (jl_type_t*)bt;
    return v;
}

jl_value_t *jl_arrayref(jl_array_t *a, size_t i)
{
    jl_type_t *el_type = (jl_type_t*)jl_tparam0(jl_typeof(a));
    jl_value_t *elt;
    if (jl_is_bits_type(el_type)) {
        if (el_type == (jl_type_t*)jl_bool_type) {
            if (((int8_t*)a->data)[i] != 0)
                return jl_true;
            return jl_false;
        }
        elt = new_scalar((jl_bits_type_t*)el_type);
        size_t nb = a->elsize;
        switch (nb) {
        case 1:
            *(int8_t*)jl_bits_data(elt)  = ((int8_t*)a->data)[i];  break;
        case 2:
            *(int16_t*)jl_bits_data(elt) = ((int16_t*)a->data)[i]; break;
        case 4:
            *(int32_t*)jl_bits_data(elt) = ((int32_t*)a->data)[i]; break;
        case 8:
            *(int64_t*)jl_bits_data(elt) = ((int64_t*)a->data)[i]; break;
        default:
            memcpy(jl_bits_data(elt), &((char*)a->data)[i*nb], nb);
        }
    }
    else {
        elt = ((jl_value_t**)a->data)[i];
        if (elt == NULL) {
            jl_errorf("array[%d]: uninitialized reference error", i+1);
        }
    }
    return elt;
}

JL_CALLABLE(jl_f_arrayref)
{
    JL_NARGS(arrayref, 2, 2);
    JL_TYPECHK(arrayref, array, args[0]);
    JL_TYPECHK(arrayref, int32, args[1]);
    jl_array_t *a = (jl_array_t*)args[0];
    size_t i = jl_unbox_int32(args[1])-1;
    if (i >= a->length) {
        jl_errorf("ref array[%d]: index out of range", i+1);
    }
    return jl_arrayref(a, i);
}

void jl_arrayset(jl_array_t *a, size_t i, jl_value_t *rhs)
{
    jl_value_t *el_type = jl_tparam0(jl_typeof(a));
    if (el_type != (jl_value_t*)jl_any_type) {
        if (!jl_subtype(rhs, el_type, 1))
            jl_type_error("arrayset", el_type, rhs);
    }
    if (jl_is_bits_type(el_type)) {
        size_t nb = a->elsize;
        switch (nb) {
        case 1:
            ((int8_t*)a->data)[i]  = *(int8_t*)jl_bits_data(rhs);  break;
        case 2:
            ((int16_t*)a->data)[i] = *(int16_t*)jl_bits_data(rhs); break;
        case 4:
            ((int32_t*)a->data)[i] = *(int32_t*)jl_bits_data(rhs); break;
        case 8:
            ((int64_t*)a->data)[i] = *(int64_t*)jl_bits_data(rhs); break;
        default:
            memcpy(&((char*)a->data)[i*nb], jl_bits_data(rhs), nb);
        }
    }
    else {
        ((jl_value_t**)a->data)[i] = rhs;
    }
}

JL_CALLABLE(jl_f_arrayset)
{
    JL_NARGS(arrayset, 3, 3);
    JL_TYPECHK(arrayset, array, args[0]);
    JL_TYPECHK(arrayset, int32, args[1]);
    jl_array_t *b = (jl_array_t*)args[0];
    size_t i = jl_unbox_int32(args[1])-1;
    if (i >= b->length) {
        jl_errorf("assign array[%d]: index out of range", i+1);
    }
    jl_arrayset(b, i, args[2]);
    return args[0];
}

static void *array_new_buffer(jl_array_t *a, size_t newsize)
{
    size_t nbytes = newsize * a->elsize;
    int isbytes = 0;
    if (a->elsize == 1) {
        isbytes = 1;
        nbytes++;
    }
    int isunboxed = jl_is_bits_type(jl_tparam0(jl_typeof(a)));
    char *newdata = alloc_array_buffer(nbytes, isunboxed);
    if (isbytes) newdata[nbytes-1] = '\0';
    return newdata;
}

void jl_array_grow_end(jl_array_t *a, size_t inc)
{
    // optimized for the case of only growing and shrinking at the end
    if (inc == 0)
        return;
    size_t es = a->elsize;
    if (es*(a->length + inc) > a->maxsize - a->offset) {
        size_t newsize = a->maxsize==0 ? inc*es : a->maxsize*2;
        while (es*(a->length+inc) > newsize-a->offset)
            newsize *= 2;
        char *newdata = array_new_buffer(a, newsize);
        newdata += a->offset;
        memcpy(newdata, (char*)a->data, a->length*es);
        a->maxsize = newsize;
        a->data = newdata;
    }
    a->length += inc; a->nrows += inc;
    a->dims = NULL;
}

void jl_array_del_end(jl_array_t *a, size_t dec)
{
    if (dec == 0)
        return;
    if (dec > a->length)
        jl_error("array_del_end: index out of range");
    memset((char*)a->data + (a->length-dec)*a->elsize, 0, dec*a->elsize);
    a->length -= dec; a->nrows -= dec;
    a->dims = NULL;
}

void jl_array_grow_beg(jl_array_t *a, size_t inc)
{
    // designed to handle the case of growing and shrinking at both ends
    if (inc == 0)
        return;
    size_t es = a->elsize;
    size_t nb = inc*es;
    if (a->offset >= nb) {
        a->data = (char*)a->data - nb;
        a->offset -= nb;
    }
    else {
        size_t anb = a->length*es;
        char *newdata;
        if (inc > (a->maxsize-anb)/2 - (a->maxsize-anb)/20) {
            size_t newsize = a->maxsize==0 ? 2*nb : a->maxsize*2;
            while (anb+2*nb > newsize-a->offset)
                newsize *= 2;
            newdata = array_new_buffer(a, newsize);
            size_t center = (newsize - (anb + nb))/2;
            newdata += center;
            a->maxsize = newsize;
            a->offset = center;
        }
        else {
            size_t center = (a->maxsize - (anb + nb))/2;
            newdata = (char*)a->data - a->offset + center;
            a->offset = center;
        }
        memmove(&newdata[nb], a->data, anb);
        a->data = newdata;
    }
    a->length += inc; a->nrows += inc;
    a->dims = NULL;
}

void jl_array_del_beg(jl_array_t *a, size_t dec)
{
    if (dec == 0)
        return;
    if (dec > a->length)
        jl_error("array_del_beg: index out of range");
    size_t nb = dec*a->elsize;
    memset(a->data, 0, nb);
    a->offset += nb;
    a->data = (char*)a->data + nb;
    a->length -= dec; a->nrows -= dec;
    a->dims = NULL;

    // make sure offset doesn't grow forever due to deleting at beginning
    // and growing at end
    if (a->offset >= 13*a->maxsize/20) {
        size_t anb = a->length*a->elsize;
        size_t newoffs = 17*(a->maxsize - anb)/100;
        size_t delta = (a->offset - newoffs);
        a->data = (char*)a->data - delta;
        memmove(a->data, (char*)a->data + delta, anb);
        a->offset = newoffs;
    }
}
