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

static
jl_array_t *jl_new_array(jl_type_t *atype, jl_tuple_t *dims)
{
    size_t i, tot;
    size_t nel=1;
    size_t ndims = dims->length;
    int isbytes=0;
    void *data;
    jl_array_t *a=NULL;
    JL_GC_PUSH(&a);

    if (ndims == 0) nel = 0;
    for(i=0; i < ndims; i++) {
        jl_value_t *dimarg = jl_tupleref(dims, i);
        assert(jl_is_int32(dimarg));
        size_t d = jl_unbox_int32(dimarg);
        nel *= d;
    }
    jl_type_t *el_type = (jl_type_t*)jl_tparam0(atype);

    if (jl_is_bits_type(el_type)) {
        size_t nby = jl_bitstype_nbits(el_type)/8;
        tot = nby * nel;
        if (nby==1 && ndims==1) {
            // hidden 0 terminator for all byte arrays
            isbytes = 1;
            tot++;
        }
    }
    else {
        tot = sizeof(void*) * nel;
    }

    if (tot <= ARRAY_INLINE_NBYTES) {
        a = allocobj(sizeof(jl_array_t) + (tot - sizeof(void*)));
        a->type = atype;
        a->dims = dims;
        data = &a->_space[0];
        if (tot > 0 && !jl_is_bits_type(el_type)) {
            memset(data, 0, tot);
        }
    }
    else {
#ifdef JL_GC_MARKSWEEP
        a = alloc_4w();
#else
        a = allocobj(sizeof(jl_array_t));
#endif
        a->type = atype;
        a->dims = dims;
        // temporarily initialize to make gc-safe
        a->data = NULL;
        a->length = 0;
        if (tot > 0) {
            if (jl_is_bits_type(el_type)) {
#ifdef BOEHM_GC
                if (tot >= 200000)
                    data = GC_malloc_atomic_ignore_off_page(tot);
                else
#endif
                    data = alloc_pod(tot);
            }
            else {
#ifdef BOEHM_GC
                if (tot >= 200000)
                    data = GC_malloc_ignore_off_page(tot);
                else
#endif
                    data = allocb(tot);
                memset(data, 0, tot);
            }
        }
        else {
            data = NULL;
        }
    }

    a->data = data;
    if (isbytes) ((char*)data)[tot-1] = '\0';
    a->length = nel;

    JL_GC_POP();
    return a;
}

jl_array_t *jl_alloc_array_1d(jl_type_t *atype, size_t nr)
{
    jl_value_t *dim = jl_box_int32(nr);
    jl_tuple_t *dims=NULL;
    JL_GC_PUSH(&dim, &dims);
    dims = jl_tuple1(dim);
    jl_array_t *a = jl_new_array(atype, dims);
    JL_GC_POP();
    return a;
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
    jl_tuple_t *d=NULL;
    jl_value_t *vnd=NULL, *vtp=NULL, *vnt=NULL;
    JL_GC_PUSH(&vnd, &vtp, &vnt, &d);
    if (nargs==2 && jl_is_tuple(args[1])) {
        d = (jl_tuple_t*)args[1];
        for(i=0; i < d->length; i++) {
            JL_TYPECHK(Array, int32, jl_tupleref(d,i));
        }
        nd = d->length;
    }
    else {
        for(i=1; i < nargs; i++) {
            JL_TYPECHK(Array, int32, args[i]);
        }
        nd = nargs-1;
        d = jl_alloc_tuple_uninit(nd);
        for(i=0; i < nd; i++)
            jl_tupleset(d, i, args[i+1]);
    }
    vnd = jl_box_int32(nd);
    vtp = (jl_value_t*)jl_tuple2(args[0], vnd);
    vnt = jl_apply_type((jl_value_t*)jl_array_type, (jl_tuple_t*)vtp);
    jl_value_t *ar = (jl_value_t*)jl_new_array((jl_type_t*)vnt, d);
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
        size_t nb = jl_bitstype_nbits(el_type)/8;
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
        if (elt == NULL)
            jl_errorf("array[%d]: uninitialized reference error", i+1);
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
    if (i >= a->length)
        jl_errorf("array[%d]: index out of range", i+1);
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
        size_t nb = jl_bitstype_nbits(el_type)/8;
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
    if (i >= b->length)
        jl_errorf("array[%d]: index out of range", i+1);
    jl_arrayset(b, i, args[2]);
    return args[0];
}
