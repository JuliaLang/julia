// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  encoding IR to/from compact representation
*/
#include <stdlib.h>
#include <string.h>

#include "julia.h"
#include "julia_internal.h"
#include "serialize.h"

#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif

#ifndef _COMPILER_MICROSOFT_
#include "valgrind.h"
#else
#define RUNNING_ON_VALGRIND 0
#endif
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    ios_t *s;
    // method we're compressing for
    jl_method_t *method;
    jl_ptls_t ptls;
} jl_ircode_state;

// --- encoding ---

#define jl_encode_value(s, v) jl_encode_value_((s), (jl_value_t*)(v), 0)

static int literal_val_id(jl_ircode_state *s, jl_value_t *v) JL_GC_DISABLED
{
    jl_array_t *rs = s->method->roots;
    int i, l = jl_array_len(rs);
    if (jl_is_symbol(v) || jl_is_concrete_type(v)) {
        for (i = 0; i < l; i++) {
            if (jl_array_ptr_ref(rs, i) == v)
                return i;
        }
    }
    else {
        for (i = 0; i < l; i++) {
            if (jl_egal(jl_array_ptr_ref(rs, i), v))
                return i;
        }
    }
    jl_array_ptr_1d_push(rs, v);
    return jl_array_len(rs) - 1;
}

static void jl_encode_value_(jl_ircode_state *s, jl_value_t *v, int as_literal) JL_GC_DISABLED
{
    size_t i;

    if (v == NULL) {
        write_uint8(s->s, TAG_NULL);
        return;
    }

    void *tag = jl_lookup_ser_tag(v);
    if (tag != HT_NOTFOUND) {
        uint8_t t8 = (intptr_t)tag;
        if (t8 <= LAST_TAG)
            write_uint8(s->s, 0);
        write_uint8(s->s, t8);
    }
    else if (jl_is_symbol(v) && (tag = jl_lookup_common_symbol(v)) != HT_NOTFOUND) {
        write_uint8(s->s, TAG_COMMONSYM);
        write_uint8(s->s, (uint8_t)(size_t)tag);
    }
    else if (v == (jl_value_t*)jl_core_module) {
        write_uint8(s->s, TAG_CORE);
    }
    else if (v == (jl_value_t*)jl_base_module) {
        write_uint8(s->s, TAG_BASE);
    }
    else if (jl_typeis(v, jl_string_type) && jl_string_len(v) == 0) {
        jl_encode_value(s, jl_an_empty_string);
    }
    else if (v == (jl_value_t*)s->method->module) {
        write_uint8(s->s, TAG_NEARBYMODULE);
    }
    else if (jl_is_datatype(v) && ((jl_datatype_t*)v)->name == jl_array_typename &&
             jl_is_long(jl_tparam1(v)) && jl_unbox_long(jl_tparam1(v)) == 1 &&
             !((jl_datatype_t*)v)->hasfreetypevars) {
        write_uint8(s->s, TAG_VECTORTY);
        jl_encode_value(s, jl_tparam0(v));
    }
    else if (jl_is_datatype(v) && ((jl_datatype_t*)v)->name == jl_pointer_typename &&
             !((jl_datatype_t*)v)->hasfreetypevars) {
        write_uint8(s->s, TAG_PTRTY);
        jl_encode_value(s, jl_tparam0(v));
    }
    else if (jl_is_svec(v)) {
        size_t l = jl_svec_len(v);
        if (l <= 255) {
            write_uint8(s->s, TAG_SVEC);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            write_uint8(s->s, TAG_LONG_SVEC);
            write_int32(s->s, l);
        }
        for (i = 0; i < l; i++) {
            jl_encode_value(s, jl_svecref(v, i));
        }
    }
    else if (jl_is_globalref(v)) {
        if (jl_globalref_mod(v) == s->method->module) {
            write_uint8(s->s, TAG_NEARBYGLOBAL);
            jl_encode_value(s, jl_globalref_name(v));
        }
        else {
            write_uint8(s->s, TAG_GLOBALREF);
            jl_encode_value(s, jl_globalref_mod(v));
            jl_encode_value(s, jl_globalref_name(v));
        }
    }
    else if (jl_is_ssavalue(v) && ((jl_ssavalue_t*)v)->id < 256 && ((jl_ssavalue_t*)v)->id >= 0) {
        write_uint8(s->s, TAG_SSAVALUE);
        write_uint8(s->s, ((jl_ssavalue_t*)v)->id);
    }
    else if (jl_is_ssavalue(v) && ((jl_ssavalue_t*)v)->id <= UINT16_MAX && ((jl_ssavalue_t*)v)->id >= 0) {
        write_uint8(s->s, TAG_LONG_SSAVALUE);
        write_uint16(s->s, ((jl_ssavalue_t*)v)->id);
    }
    else if (jl_typeis(v, jl_slotnumber_type) && jl_slot_number(v) <= UINT16_MAX && jl_slot_number(v) >= 0) {
        write_uint8(s->s, TAG_SLOTNUMBER);
        write_uint16(s->s, jl_slot_number(v));
    }
    else if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        size_t l = jl_array_len(e->args);
        if (e->head == call_sym) {
            if (l == 2) {
                write_uint8(s->s, TAG_CALL1);
                jl_encode_value(s, jl_exprarg(e, 0));
                jl_encode_value(s, jl_exprarg(e, 1));
                return;
            }
            else if (l == 3) {
                write_uint8(s->s, TAG_CALL2);
                jl_encode_value(s, jl_exprarg(e, 0));
                jl_encode_value(s, jl_exprarg(e, 1));
                jl_encode_value(s, jl_exprarg(e, 2));
                return;
            }
        }
        if (l <= 255) {
            write_uint8(s->s, TAG_EXPR);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            write_uint8(s->s, TAG_LONG_EXPR);
            write_int32(s->s, l);
        }
        jl_encode_value(s, e->head);
        for (i = 0; i < l; i++) {
            jl_encode_value(s, jl_exprarg(e, i));
        }
    }
    else if (jl_is_phinode(v)) {
        jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(v, 0);
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(v, 1);
        size_t l = jl_array_len(edges);
        if (l <= 255 && jl_array_len(values) == l) {
            write_uint8(s->s, TAG_PHINODE);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            write_uint8(s->s, TAG_LONG_PHINODE);
            write_int32(s->s, l);
            write_int32(s->s, jl_array_len(values));
        }
        for (i = 0; i < l; i++) {
            jl_encode_value(s, jl_array_ptr_ref(edges, i));
        }
        l = jl_array_len(values);
        for (i = 0; i < l; i++) {
            jl_encode_value(s, jl_array_ptr_ref(values, i));
        }
    }
    else if (jl_is_phicnode(v)) {
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(v, 0);
        size_t l = jl_array_len(values);
        if (l <= 255) {
            write_uint8(s->s, TAG_PHICNODE);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            write_uint8(s->s, TAG_LONG_PHICNODE);
            write_int32(s->s, l);
        }
        for (i = 0; i < l; i++) {
            jl_encode_value(s, jl_array_ptr_ref(values, i));
        }
    }
    else if (jl_is_gotonode(v)) {
        write_uint8(s->s, TAG_GOTONODE);
        jl_encode_value(s, jl_get_nth_field(v, 0));
    }
    else if (jl_is_quotenode(v)) {
        write_uint8(s->s, TAG_QUOTENODE);
        jl_encode_value(s, jl_get_nth_field(v, 0));
    }
    else if (jl_typeis(v, jl_int64_type)) {
        void *data = jl_data_ptr(v);
        if (*(int64_t*)data >= INT16_MIN && *(int64_t*)data <= INT16_MAX) {
            write_uint8(s->s, TAG_SHORTER_INT64);
            write_uint16(s->s, (uint16_t)*(int64_t*)data);
        }
        else if (*(int64_t*)data >= S32_MIN && *(int64_t*)data <= S32_MAX) {
            write_uint8(s->s, TAG_SHORT_INT64);
            write_int32(s->s, (int32_t)*(int64_t*)data);
        }
        else {
            write_uint8(s->s, TAG_INT64);
            write_int64(s->s, *(int64_t*)data);
        }
    }
    else if (jl_typeis(v, jl_int32_type)) {
        void *data = jl_data_ptr(v);
        if (*(int32_t*)data >= INT16_MIN && *(int32_t*)data <= INT16_MAX) {
            write_uint8(s->s, TAG_SHORT_INT32);
            write_uint16(s->s, (uint16_t)*(int32_t*)data);
        }
        else {
            write_uint8(s->s, TAG_INT32);
            write_int32(s->s, *(int32_t*)data);
        }
    }
    else if (jl_typeis(v, jl_uint8_type)) {
        write_uint8(s->s, TAG_UINT8);
        write_int8(s->s, *(int8_t*)jl_data_ptr(v));
    }
    else if (jl_typeis(v, jl_lineinfonode_type)) {
        write_uint8(s->s, TAG_LINEINFO);
        for (i = 0; i < jl_datatype_nfields(jl_lineinfonode_type); i++)
            jl_encode_value(s, jl_get_nth_field(v, i));
    }
    else if (((jl_datatype_t*)jl_typeof(v))->instance == v) {
        write_uint8(s->s, TAG_SINGLETON);
        jl_encode_value(s, jl_typeof(v));
    }
    else if (as_literal && jl_typeis(v, jl_string_type)) {
        write_uint8(s->s, TAG_STRING);
        write_int32(s->s, jl_string_len(v));
        ios_write(s->s, jl_string_data(v), jl_string_len(v));
    }
    else if (as_literal && jl_is_array(v)) {
        jl_array_t *ar = (jl_array_t*)v;
        jl_value_t *et = jl_tparam0(jl_typeof(ar));
        int isunion = jl_is_uniontype(et);
        if (ar->flags.ndims == 1 && ar->elsize <= 0x1f) {
            write_uint8(s->s, TAG_ARRAY1D);
            write_uint8(s->s, (ar->flags.ptrarray << 7) | (ar->flags.hasptr << 6) | (isunion << 5) | (ar->elsize & 0x1f));
        }
        else {
            write_uint8(s->s, TAG_ARRAY);
            write_uint16(s->s, ar->flags.ndims);
            write_uint16(s->s, (ar->flags.ptrarray << 15) | (ar->flags.hasptr << 14) | (isunion << 13) | (ar->elsize & 0x1fff));
        }
        for (i = 0; i < ar->flags.ndims; i++)
            jl_encode_value(s, jl_box_long(jl_array_dim(ar,i)));
        jl_encode_value(s, jl_typeof(ar));
        size_t l = jl_array_len(ar);
        if (ar->flags.ptrarray) {
            for (i = 0; i < l; i++) {
                jl_value_t *e = jl_array_ptr_ref(v, i);
                jl_encode_value(s, e);
            }
        }
        else if (ar->flags.hasptr) {
            const char *data = (const char*)jl_array_data(ar);
            uint16_t elsz = ar->elsize;
            size_t j, np = ((jl_datatype_t*)et)->layout->npointers;
            for (i = 0; i < l; i++) {
                const char *start = data;
                for (j = 0; j < np; j++) {
                    uint32_t ptr = jl_ptr_offset((jl_datatype_t*)et, j);
                    const jl_value_t *const *fld = &((const jl_value_t *const *)data)[ptr];
                    if ((const char*)fld != start)
                        ios_write(s->s, start, (const char*)fld - start);
                    JL_GC_PROMISE_ROOTED(*fld);
                    jl_encode_value(s, *fld);
                    start = (const char*)&fld[1];
                }
                data += elsz;
                if (data != start)
                    ios_write(s->s, start, data - start);
            }
        }
        else {
            ios_write(s->s, (char*)jl_array_data(ar), l * ar->elsize);
            if (jl_array_isbitsunion(ar))
                ios_write(s->s, jl_array_typetagdata(ar), l);
        }
    }
    else {
        if (!as_literal && !(jl_is_uniontype(v) || jl_is_newvarnode(v) || jl_is_tuple(v) ||
                             jl_is_linenode(v) || jl_is_upsilonnode(v) || jl_is_pinode(v))) {
            int id = literal_val_id(s, v);
            assert(id >= 0);
            if (id < 256) {
                write_uint8(s->s, TAG_METHODROOT);
                write_uint8(s->s, id);
            }
            else {
                assert(id <= UINT16_MAX);
                write_uint8(s->s, TAG_LONG_METHODROOT);
                write_uint16(s->s, id);
            }
            return;
        }
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        if (t->size <= 255) {
            write_uint8(s->s, TAG_SHORT_GENERAL);
            write_uint8(s->s, t->size);
        }
        else {
            write_uint8(s->s, TAG_GENERAL);
            write_int32(s->s, t->size);
        }
        jl_encode_value(s, t);

        char *data = (char*)jl_data_ptr(v);
        size_t i, j, np = t->layout->npointers;
        uint32_t nf = t->layout->nfields;
        char *last = data;
        for (i = 0, j = 0; i < nf+1; i++) {
            char *ptr = data + (i < nf ? jl_field_offset(t, i) : jl_datatype_size(t));
            if (j < np) {
                char *prevptr = (char*)&((jl_value_t**)data)[jl_ptr_offset(t, j)];
                while (ptr > prevptr) {
                    // previous field contained pointers; write them and their interleaved data
                    if (prevptr > last)
                        ios_write(s->s, last, prevptr - last);
                    jl_value_t *e = *(jl_value_t**)prevptr;
                    JL_GC_PROMISE_ROOTED(e);
                    jl_encode_value(s, e);
                    last = prevptr + sizeof(jl_value_t*);
                    j++;
                    if (j < np)
                        prevptr = (char*)&((jl_value_t**)data)[jl_ptr_offset(t, j)];
                    else
                        break;
                }
            }
            if (i == nf)
                break;
        }
        char *ptr = data + jl_datatype_size(t);
        if (ptr > last)
            ios_write(s->s, last, ptr - last);
    }
}

// --- decoding ---

static jl_value_t *jl_decode_value(jl_ircode_state *s) JL_GC_DISABLED;

static jl_value_t *jl_decode_value_svec(jl_ircode_state *s, uint8_t tag) JL_GC_DISABLED
{
    size_t i, len;
    if (tag == TAG_SVEC)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    jl_svec_t *sv = jl_alloc_svec_uninit(len);
    jl_value_t **data = jl_svec_data(sv);
    for (i = 0; i < len; i++) {
        data[i] = jl_decode_value(s);
    }
    return (jl_value_t*)sv;
}

static jl_value_t *jl_decode_value_array(jl_ircode_state *s, uint8_t tag) JL_GC_DISABLED
{
    int16_t i, ndims;
    int isptr, isunion, hasptr, elsize;
    if (tag == TAG_ARRAY1D) {
        ndims = 1;
        elsize = read_uint8(s->s);
        isptr = (elsize >> 7) & 1;
        hasptr = (elsize >> 6) & 1;
        isunion = (elsize >> 5) & 1;
        elsize = elsize & 0x1f;
    }
    else {
        ndims = read_uint16(s->s);
        elsize = read_uint16(s->s);
        isptr = (elsize >> 15) & 1;
        hasptr = (elsize >> 14) & 1;
        isunion = (elsize >> 13) & 1;
        elsize = elsize & 0x3fff;
    }
    size_t *dims = (size_t*)alloca(ndims * sizeof(size_t));
    for (i = 0; i < ndims; i++) {
        dims[i] = jl_unbox_long(jl_decode_value(s));
    }
    jl_array_t *a = jl_new_array_for_deserialization(
            (jl_value_t*)NULL, ndims, dims, !isptr, hasptr, isunion, elsize);
    jl_value_t *aty = jl_decode_value(s);
    jl_set_typeof(a, aty);
    if (a->flags.ptrarray) {
        jl_value_t **data = (jl_value_t**)jl_array_data(a);
        size_t i, numel = jl_array_len(a);
        for (i = 0; i < numel; i++) {
            data[i] = jl_decode_value(s);
        }
        assert(jl_astaggedvalue(a)->bits.gc == GC_CLEAN); // gc is disabled
    }
    else if (a->flags.hasptr) {
        size_t i, numel = jl_array_len(a);
        char *data = (char*)jl_array_data(a);
        uint16_t elsz = a->elsize;
        jl_datatype_t *et = (jl_datatype_t*)jl_tparam0(jl_typeof(a));
        size_t j, np = et->layout->npointers;
        for (i = 0; i < numel; i++) {
            char *start = data;
            for (j = 0; j < np; j++) {
                uint32_t ptr = jl_ptr_offset(et, j);
                jl_value_t **fld = &((jl_value_t**)data)[ptr];
                if ((char*)fld != start)
                    ios_readall(s->s, start, (const char*)fld - start);
                *fld = jl_decode_value(s);
                start = (char*)&fld[1];
            }
            data += elsz;
            if (data != start)
                ios_readall(s->s, start, data - start);
        }
        assert(jl_astaggedvalue(a)->bits.gc == GC_CLEAN); // gc is disabled
    }
    else {
        size_t extra = jl_array_isbitsunion(a) ? jl_array_len(a) : 0;
        size_t tot = jl_array_len(a) * a->elsize + extra;
        ios_readall(s->s, (char*)jl_array_data(a), tot);
    }
    return (jl_value_t*)a;
}

static jl_value_t *jl_decode_value_expr(jl_ircode_state *s, uint8_t tag) JL_GC_DISABLED
{
    size_t i, len;
    jl_sym_t *head = NULL;
    if (tag == TAG_EXPR) {
        len = read_uint8(s->s);
    }
    else if (tag == TAG_CALL1) {
        len = 2;
        head = call_sym;
    }
    else if (tag == TAG_CALL2) {
        len = 3;
        head = call_sym;
    }
    else {
        len = read_int32(s->s);
    }
    if (head == NULL)
        head = (jl_sym_t*)jl_decode_value(s);
    jl_expr_t *e = jl_exprn(head, len);
    jl_value_t **data = (jl_value_t**)(e->args->data);
    for (i = 0; i < len; i++) {
        data[i] = jl_decode_value(s);
    }
    return (jl_value_t*)e;
}

static jl_value_t *jl_decode_value_phi(jl_ircode_state *s, uint8_t tag) JL_GC_DISABLED
{
    size_t i, len_e, len_v;
    if (tag == TAG_PHINODE) {
        len_e = len_v = read_uint8(s->s);
    }
    else {
        len_e = read_int32(s->s);
        len_v = read_int32(s->s);
    }
    jl_array_t *e = jl_alloc_vec_any(len_e);
    jl_array_t *v = jl_alloc_vec_any(len_v);
    jl_value_t *phi = jl_new_struct(jl_phinode_type, e, v);
    jl_value_t **data_e = (jl_value_t**)(e->data);
    for (i = 0; i < len_e; i++) {
        data_e[i] = jl_decode_value(s);
    }
    jl_value_t **data_v = (jl_value_t**)(v->data);
    for (i = 0; i < len_v; i++) {
        data_v[i] = jl_decode_value(s);
    }
    return phi;
}

static jl_value_t *jl_decode_value_phic(jl_ircode_state *s, uint8_t tag) JL_GC_DISABLED
{
    size_t i, len;
    if (tag == TAG_PHICNODE)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    jl_array_t *v = jl_alloc_vec_any(len);
    jl_value_t *phic = jl_new_struct(jl_phicnode_type, v);
    jl_value_t **data = (jl_value_t**)(v->data);
    for (i = 0; i < len; i++) {
        data[i] = jl_decode_value(s);
    }
    return phic;
}

static jl_value_t *jl_decode_value_globalref(jl_ircode_state *s) JL_GC_DISABLED
{
    jl_value_t *mod = jl_decode_value(s);
    jl_value_t *var = jl_decode_value(s);
    return jl_module_globalref((jl_module_t*)mod, (jl_sym_t*)var);
}

static jl_value_t *jl_decode_value_any(jl_ircode_state *s, uint8_t tag) JL_GC_DISABLED
{
    int32_t sz = (tag == TAG_SHORT_GENERAL ? read_uint8(s->s) : read_int32(s->s));
    jl_value_t *v = jl_gc_alloc(s->ptls, sz, NULL);
    jl_set_typeof(v, (void*)(intptr_t)0x50);
    jl_datatype_t *dt = (jl_datatype_t*)jl_decode_value(s);
    jl_set_typeof(v, dt);
    char *data = (char*)jl_data_ptr(v);
    size_t i, np = dt->layout->npointers;
    char *start = data;
    for (i = 0; i < np; i++) {
        uint32_t ptr = jl_ptr_offset(dt, i);
        jl_value_t **fld = &((jl_value_t**)data)[ptr];
        if ((char*)fld != start)
            ios_readall(s->s, start, (const char*)fld - start);
        *fld = jl_decode_value(s);
        start = (char*)&fld[1];
    }
    data += jl_datatype_size(dt);
    if (data != start)
        ios_readall(s->s, start, data - start);
    return v;
}

static jl_value_t *jl_decode_value(jl_ircode_state *s) JL_GC_DISABLED
{
    assert(!ios_eof(s->s));
    jl_value_t *v;
    size_t i, n;
    uint8_t tag = read_uint8(s->s);
    if (tag > LAST_TAG)
        return jl_deser_tag(tag);
    switch (tag) {
    case TAG_NULL: return NULL;
    case 0:
        tag = read_uint8(s->s);
        return jl_deser_tag(tag);
    case TAG_METHODROOT:
        return jl_array_ptr_ref(s->method->roots, read_uint8(s->s));
    case TAG_LONG_METHODROOT:
        return jl_array_ptr_ref(s->method->roots, read_uint16(s->s));
    case TAG_SVEC: JL_FALLTHROUGH; case TAG_LONG_SVEC:
        return jl_decode_value_svec(s, tag);
    case TAG_COMMONSYM:
        return jl_deser_symbol(read_uint8(s->s));
    case TAG_SSAVALUE:
        v = jl_box_ssavalue(read_uint8(s->s));
        return v;
    case TAG_LONG_SSAVALUE:
        v = jl_box_ssavalue(read_uint16(s->s));
        return v;
    case TAG_SLOTNUMBER:
        v = jl_box_slotnumber(read_uint16(s->s));
        return v;
    case TAG_ARRAY: JL_FALLTHROUGH; case TAG_ARRAY1D:
        return jl_decode_value_array(s, tag);
    case TAG_EXPR:      JL_FALLTHROUGH;
    case TAG_LONG_EXPR: JL_FALLTHROUGH;
    case TAG_CALL1:     JL_FALLTHROUGH;
    case TAG_CALL2:
        return jl_decode_value_expr(s, tag);
    case TAG_PHINODE: JL_FALLTHROUGH; case TAG_LONG_PHINODE:
        return jl_decode_value_phi(s, tag);
    case TAG_PHICNODE: JL_FALLTHROUGH; case TAG_LONG_PHICNODE:
        return jl_decode_value_phic(s, tag);
    case TAG_GOTONODE: JL_FALLTHROUGH; case TAG_QUOTENODE:
        v = jl_new_struct_uninit(tag == TAG_GOTONODE ? jl_gotonode_type : jl_quotenode_type);
        set_nth_field(tag == TAG_GOTONODE ? jl_gotonode_type : jl_quotenode_type, (void*)v, 0, jl_decode_value(s));
        return v;
    case TAG_SHORTER_INT64:
        v = jl_box_int64((int16_t)read_uint16(s->s));
        return v;
    case TAG_SHORT_INT64:
        v = jl_box_int64(read_int32(s->s));
        return v;
    case TAG_INT64:
        v = jl_box_int64((int64_t)read_uint64(s->s));
        return v;
    case TAG_SHORT_INT32:
        v = jl_box_int32((int16_t)read_uint16(s->s));
        return v;
    case TAG_INT32:
        v = jl_box_int32(read_int32(s->s));
        return v;
    case TAG_UINT8:
        return jl_box_uint8(read_uint8(s->s));
    case TAG_NEARBYGLOBAL:
        assert(s->method != NULL);
        v = jl_decode_value(s);
        return jl_module_globalref(s->method->module, (jl_sym_t*)v);
    case TAG_NEARBYMODULE:
        assert(s->method != NULL);
        return (jl_value_t*)s->method->module;
    case TAG_GLOBALREF:
        return jl_decode_value_globalref(s);
    case TAG_SINGLETON:
        return ((jl_datatype_t*)jl_decode_value(s))->instance;
    case TAG_CORE:
        return (jl_value_t*)jl_core_module;
    case TAG_BASE:
        return (jl_value_t*)jl_base_module;
    case TAG_VECTORTY:
        v = jl_decode_value(s);
        return jl_apply_type2((jl_value_t*)jl_array_type, v, jl_box_long(1));
    case TAG_PTRTY:
        v = jl_decode_value(s);
        return jl_apply_type1((jl_value_t*)jl_pointer_type, v);
    case TAG_STRING:
        n = read_int32(s->s);
        v = jl_alloc_string(n);
        ios_readall(s->s, jl_string_data(v), n);
        return v;
    case TAG_LINEINFO:
        v = jl_new_struct_uninit(jl_lineinfonode_type);
        for (i = 0; i < jl_datatype_nfields(jl_lineinfonode_type); i++) {
            //size_t offs = jl_field_offset(jl_lineinfonode_type, i);
            set_nth_field(jl_lineinfonode_type, (void*)v, i, jl_decode_value(s));
        }
        return v;
    default:
        assert(tag == TAG_GENERAL || tag == TAG_SHORT_GENERAL);
        return jl_decode_value_any(s, tag);
    }
}

// --- entry points ---

JL_DLLEXPORT jl_array_t *jl_compress_ir(jl_method_t *m, jl_code_info_t *code)
{
    JL_TIMING(AST_COMPRESS);
    JL_LOCK(&m->writelock); // protect the roots array (Might GC)
    assert(jl_is_method(m));
    assert(jl_is_code_info(code));
    ios_t dest;
    ios_mem(&dest, 0);
    int en = jl_gc_enable(0); // Might GC
    size_t i;

    if (m->roots == NULL) {
        m->roots = jl_alloc_vec_any(0);
        jl_gc_wb(m, m->roots);
    }
    jl_ircode_state s = {
        &dest,
        m,
        jl_get_ptls_states()
    };

    uint8_t flags = (code->inferred << 3)
                  | (code->inlineable << 2)
                  | (code->propagate_inbounds << 1)
                  | (code->pure << 0);
    write_uint8(s.s, flags);

    size_t nslots = jl_array_len(code->slotflags);
    assert(nslots >= m->nargs && nslots < INT32_MAX); // required by generated functions
    write_int32(s.s, nslots);
    ios_write(s.s, (char*)jl_array_data(code->slotflags), nslots);

    // N.B.: The layout of everything before this point is explicitly referenced
    // by the various jl_ir_ accessors. Make sure to adjust those if you change
    // the data layout.

    for (i = 0; i < 6; i++) {
        int copy = 1;
        if (i == 1) { // skip codelocs
            assert(jl_field_offset(jl_code_info_type, i) == offsetof(jl_code_info_t, codelocs));
            continue;
        }
        if (i == 4) { // don't copy contents of method_for_inference_limit_heuristics field
            assert(jl_field_offset(jl_code_info_type, i) == offsetof(jl_code_info_t, method_for_inference_limit_heuristics));
            copy = 0;
        }
        jl_encode_value_(&s, jl_get_nth_field((jl_value_t*)code, i), copy);
    }

    if (m->generator)
        // can't optimize generated functions
        jl_encode_value_(&s, (jl_value_t*)jl_compress_argnames(code->slotnames), 1);
    else
        jl_encode_value(&s, jl_nothing);

    size_t nstmt = jl_array_len(code->code);
    assert(nstmt == jl_array_len(code->codelocs));
    if (jl_array_len(code->linetable) < 256) {
        for (i = 0; i < nstmt; i++) {
            write_uint8(s.s, ((int32_t*)jl_array_data(code->codelocs))[i]);
        }
    }
    else if (jl_array_len(code->linetable) < 65536) {
        for (i = 0; i < nstmt; i++) {
            write_uint16(s.s, ((int32_t*)jl_array_data(code->codelocs))[i]);
        }
    }
    else {
        ios_write(s.s, (char*)jl_array_data(code->codelocs), nstmt * sizeof(int32_t));
    }

    ios_flush(s.s);
    jl_array_t *v = jl_take_buffer(&dest);
    ios_close(s.s);
    if (jl_array_len(m->roots) == 0) {
        m->roots = NULL;
    }
    JL_GC_PUSH1(&v);
    jl_gc_enable(en);
    JL_UNLOCK(&m->writelock); // Might GC
    JL_GC_POP();
    return v;
}

JL_DLLEXPORT jl_code_info_t *jl_uncompress_ir(jl_method_t *m, jl_code_instance_t *metadata, jl_array_t *data)
{
    if (jl_is_code_info(data))
        return (jl_code_info_t*)data;
    JL_TIMING(AST_UNCOMPRESS);
    JL_LOCK(&m->writelock); // protect the roots array (Might GC)
    assert(jl_is_method(m));
    assert(jl_typeis(data, jl_array_uint8_type));
    size_t i;
    ios_t src;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)data->data, jl_array_len(data), 0);
    src.size = jl_array_len(data);
    int en = jl_gc_enable(0); // Might GC
    jl_ircode_state s = {
        &src,
        m,
        jl_get_ptls_states()
    };

    jl_code_info_t *code = jl_new_code_info_uninit();
    uint8_t flags = read_uint8(s.s);
    code->inferred = !!(flags & (1 << 3));
    code->inlineable = !!(flags & (1 << 2));
    code->propagate_inbounds = !!(flags & (1 << 1));
    code->pure = !!(flags & (1 << 0));

    size_t nslots = read_int32(&src);
    code->slotflags = jl_alloc_array_1d(jl_array_uint8_type, nslots);
    ios_readall(s.s, (char*)jl_array_data(code->slotflags), nslots);

    for (i = 0; i < 6; i++) {
        if (i == 1)  // skip codelocs
            continue;
        assert(jl_field_isptr(jl_code_info_type, i));
        jl_value_t **fld = (jl_value_t**)((char*)jl_data_ptr(code) + jl_field_offset(jl_code_info_type, i));
        *fld = jl_decode_value(&s);
    }

    jl_value_t *slotnames = jl_decode_value(&s);
    if (!jl_is_string(slotnames))
        slotnames = m->slot_syms;
    code->slotnames = jl_uncompress_argnames(slotnames);

    size_t nstmt = jl_array_len(code->code);
    code->codelocs = (jl_value_t*)jl_alloc_array_1d(jl_array_int32_type, nstmt);
    if (jl_array_len(code->linetable) < 256) {
        for (i = 0; i < nstmt; i++) {
            ((int32_t*)jl_array_data(code->codelocs))[i] = read_uint8(s.s);
        }
    }
    else if (jl_array_len(code->linetable) < 65536) {
        for (i = 0; i < nstmt; i++) {
            ((int32_t*)jl_array_data(code->codelocs))[i] = read_uint16(s.s);
        }
    }
    else {
        ios_readall(s.s, (char*)jl_array_data(code->codelocs), nstmt * sizeof(int32_t));
    }

    assert(ios_getc(s.s) == -1);
    ios_close(s.s);
    JL_GC_PUSH1(&code);
    jl_gc_enable(en);
    JL_UNLOCK(&m->writelock); // Might GC
    JL_GC_POP();
    if (metadata) {
        code->min_world = metadata->min_world;
        code->max_world = metadata->max_world;
        code->rettype = metadata->rettype;
        code->parent = metadata->def;
    }
    return code;
}

JL_DLLEXPORT uint8_t jl_ir_flag_inferred(jl_array_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inferred;
    assert(jl_typeis(data, jl_array_uint8_type));
    uint8_t flags = ((uint8_t*)data->data)[0];
    return !!(flags & (1 << 3));
}

JL_DLLEXPORT uint8_t jl_ir_flag_inlineable(jl_array_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inlineable;
    assert(jl_typeis(data, jl_array_uint8_type));
    uint8_t flags = ((uint8_t*)data->data)[0];
    return !!(flags & (1 << 2));
}

JL_DLLEXPORT uint8_t jl_ir_flag_pure(jl_array_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->pure;
    assert(jl_typeis(data, jl_array_uint8_type));
    uint8_t flags = ((uint8_t*)data->data)[0];
    return !!(flags & (1 << 0));
}

JL_DLLEXPORT jl_value_t *jl_compress_argnames(jl_array_t *syms)
{
    size_t nsyms = jl_array_len(syms);
    size_t i, len = 0;
    for (i = 0; i < nsyms; i++) {
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(syms, i);
        assert(jl_is_symbol(name));
        char *namestr = jl_symbol_name(name);
        size_t namelen = strlen(namestr) + 1;
        len += namelen;
    }
    jl_value_t *str = jl_alloc_string(len);
    len = 0;
    for (i = 0; i < nsyms; i++) {
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(syms, i);
        assert(jl_is_symbol(name));
        char *namestr = jl_symbol_name(name);
        size_t namelen = strlen(namestr) + 1; // include nul-byte
        assert(len + namelen <= jl_string_len(str));
        memcpy(jl_string_data(str) + len, namestr, namelen);
        len += namelen;
    }
    assert(len == jl_string_len(str));
    return str;
}

JL_DLLEXPORT ssize_t jl_ir_nslots(jl_array_t *data)
{
    if (jl_is_code_info(data)) {
        jl_code_info_t *func = (jl_code_info_t*)data;
        return jl_array_len(func->slotnames);
    }
    else {
        assert(jl_typeis(data, jl_array_uint8_type));
        int nslots = jl_load_unaligned_i32((char*)data->data + 1);
        return nslots;
    }
}

JL_DLLEXPORT uint8_t jl_ir_slotflag(jl_array_t *data, size_t i)
{
    assert(i < jl_ir_nslots(data));
    if (jl_is_code_info(data))
        return ((uint8_t*)((jl_code_info_t*)data)->slotflags->data)[i];
    assert(jl_typeis(data, jl_array_uint8_type));
    return ((uint8_t*)data->data)[1 + sizeof(int32_t) + i];
}

JL_DLLEXPORT jl_array_t *jl_uncompress_argnames(jl_value_t *syms)
{
    assert(jl_is_string(syms));
    char *namestr;
    namestr = jl_string_data(syms);
    size_t remaining = jl_string_len(syms);
    size_t i, len = 0;
    while (remaining) {
        size_t namelen = strlen(namestr);
        len += 1;
        namestr += namelen + 1;
        remaining -= namelen + 1;
    }
    namestr = jl_string_data(syms);
    jl_array_t *names = jl_alloc_array_1d(jl_array_symbol_type, len);
    JL_GC_PUSH1(&names);
    for (i = 0; i < len; i++) {
        size_t namelen = strlen(namestr);
        jl_sym_t *name = jl_symbol_n(namestr, namelen);
        jl_array_ptr_set(names, i, name);
        namestr += namelen + 1;
    }
    JL_GC_POP();
    return names;
}

JL_DLLEXPORT jl_value_t *jl_uncompress_argname_n(jl_value_t *syms, size_t i)
{
    assert(jl_is_string(syms));
    char *namestr = jl_string_data(syms);
    size_t remaining = jl_string_len(syms);
    while (remaining) {
        size_t namelen = strlen(namestr);
        if (i-- == 0) {
            jl_sym_t *name = jl_symbol_n(namestr, namelen);
            return (jl_value_t*)name;
        }
        namestr += namelen + 1;
        remaining -= namelen + 1;
    }
    return jl_nothing;
}

#ifdef __cplusplus
}
#endif
