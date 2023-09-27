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

#include "valgrind.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    ios_t *s;
    // method we're compressing for
    jl_method_t *method;
    jl_ptls_t ptls;
    uint8_t relocatability;
} jl_ircode_state;

// type => tag hash for a few core types (e.g., Expr, PhiNode, etc)
static htable_t ser_tag;
// tag => type mapping, the reverse of ser_tag
static jl_value_t *deser_tag[256];
// hash of some common symbols, encoded as CommonSym_tag plus 1 byte
static htable_t common_symbol_tag;
static jl_value_t *deser_symbols[256];

void *jl_lookup_ser_tag(jl_value_t *v)
{
    return ptrhash_get(&ser_tag, v);
}

void *jl_lookup_common_symbol(jl_value_t *v)
{
    return ptrhash_get(&common_symbol_tag, v);
}

jl_value_t *jl_deser_tag(uint8_t tag)
{
    return deser_tag[tag];
}

jl_value_t *jl_deser_symbol(uint8_t tag)
{
    return deser_symbols[tag];
}

// --- encoding ---

#define jl_encode_value(s, v) jl_encode_value_((s), (jl_value_t*)(v), 0)

static void tagged_root(rle_reference *rr, jl_ircode_state *s, int i)
{
    if (!get_root_reference(rr, s->method, i))
        s->relocatability = 0;
}

static void literal_val_id(rle_reference *rr, jl_ircode_state *s, jl_value_t *v) JL_GC_DISABLED
{
    jl_array_t *rs = s->method->roots;
    int i, l = jl_array_len(rs);
    if (jl_is_symbol(v) || jl_is_concrete_type(v)) {
        for (i = 0; i < l; i++) {
            if (jl_array_ptr_ref(rs, i) == v)
                return tagged_root(rr, s, i);
        }
    }
    else {
        for (i = 0; i < l; i++) {
            if (jl_egal(jl_array_ptr_ref(rs, i), v))
                return tagged_root(rr, s, i);
        }
    }
    jl_add_method_root(s->method, jl_precompile_toplevel_module, v);
    return tagged_root(rr, s, jl_array_len(rs) - 1);
}

static void jl_encode_int32(jl_ircode_state *s, int32_t x)
{
    if (x >= INT16_MIN && x <= INT16_MAX) {
        write_uint8(s->s, TAG_SHORT_INT32);
        write_uint16(s->s, (uint16_t)x);
    }
    else {
        write_uint8(s->s, TAG_INT32);
        write_int32(s->s, x);
    }
}

static void jl_encode_as_indexed_root(jl_ircode_state *s, jl_value_t *v)
{
    rle_reference rr;

    literal_val_id(&rr, s, v);
    int id = rr.index;
    assert(id >= 0);
    if (rr.key) {
        write_uint8(s->s, TAG_RELOC_METHODROOT);
        write_uint64(s->s, rr.key);
    }
    if (id <= UINT8_MAX) {
        write_uint8(s->s, TAG_METHODROOT);
        write_uint8(s->s, id);
    }
    else {
        assert(id <= UINT32_MAX);
        write_uint8(s->s, TAG_LONG_METHODROOT);
        write_uint32(s->s, id);
    }
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
    else if (jl_typetagis(v, jl_string_tag << 4) && jl_string_len(v) == 0) {
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
    else if (jl_typetagis(v, jl_slotnumber_type) && jl_slot_number(v) <= UINT16_MAX && jl_slot_number(v) >= 0) {
        write_uint8(s->s, TAG_SLOTNUMBER);
        write_uint16(s->s, jl_slot_number(v));
    }
    else if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        size_t l = jl_array_len(e->args);
        if (e->head == jl_call_sym) {
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
            int32_t e = ((int32_t*)jl_array_data(edges))[i];
            if (e <= 20)
                jl_encode_value(s, jl_box_int32(e));
            else
                jl_encode_int32(s, e);
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
    else if (jl_is_gotoifnot(v)) {
        write_uint8(s->s, TAG_GOTOIFNOT);
        jl_encode_value(s, jl_get_nth_field(v, 0));
        jl_encode_value(s, jl_get_nth_field(v, 1));
    }
    else if (jl_is_argument(v)) {
        write_uint8(s->s, TAG_ARGUMENT);
        jl_encode_value(s, jl_get_nth_field(v, 0));
    }
    else if (jl_is_returnnode(v)) {
        write_uint8(s->s, TAG_RETURNNODE);
        jl_encode_value(s, jl_get_nth_field(v, 0));
    }
    else if (jl_is_quotenode(v)) {
        write_uint8(s->s, TAG_QUOTENODE);
        jl_value_t *inner = jl_quotenode_value(v);
        // we might need to return this exact object at run time, therefore codegen might
        // need to reference it as well, so it is more likely useful to give it a root
        if (jl_is_expr(inner) || jl_is_phinode(inner) || jl_is_phicnode(inner))
            jl_encode_as_indexed_root(s, inner);
        else
            jl_encode_value(s, inner);
    }
    else if (jl_typetagis(v, jl_int64_tag << 4)) {
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
            write_uint64(s->s, *(int64_t*)data);
        }
    }
    else if (jl_typetagis(v, jl_int32_tag << 4)) {
        jl_encode_int32(s, *(int32_t*)jl_data_ptr(v));
    }
    else if (jl_typetagis(v, jl_uint8_tag << 4)) {
        write_uint8(s->s, TAG_UINT8);
        write_int8(s->s, *(int8_t*)jl_data_ptr(v));
    }
    else if (jl_typetagis(v, jl_lineinfonode_type)) {
        write_uint8(s->s, TAG_LINEINFO);
        for (i = 0; i < jl_datatype_nfields(jl_lineinfonode_type); i++)
            jl_encode_value(s, jl_get_nth_field(v, i));
    }
    else if (((jl_datatype_t*)jl_typeof(v))->instance == v) {
        write_uint8(s->s, TAG_SINGLETON);
        jl_encode_value(s, jl_typeof(v));
    }
    else if (as_literal && jl_typetagis(v, jl_string_tag << 4)) {
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
    else if (as_literal || jl_is_uniontype(v) || jl_is_newvarnode(v) || jl_is_linenode(v) ||
             jl_is_upsilonnode(v) || jl_is_pinode(v) || jl_is_slotnumber(v) || jl_is_ssavalue(v) ||
             (jl_isbits(jl_typeof(v)) && jl_datatype_size(jl_typeof(v)) <= 64)) {
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        size_t tsz = jl_datatype_size(t);
        if (tsz <= 255) {
            write_uint8(s->s, TAG_SHORT_GENERAL);
            write_uint8(s->s, tsz);
        }
        else {
            write_uint8(s->s, TAG_GENERAL);
            write_int32(s->s, tsz);
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
    else {
        jl_encode_as_indexed_root(s, v);
    }
}

static jl_code_info_flags_t code_info_flags(uint8_t inferred, uint8_t propagate_inbounds, uint8_t has_fcall,
                                            uint8_t nospecializeinfer, uint8_t inlining, uint8_t constprop)
{
    jl_code_info_flags_t flags;
    flags.bits.inferred = inferred;
    flags.bits.propagate_inbounds = propagate_inbounds;
    flags.bits.has_fcall = has_fcall;
    flags.bits.nospecializeinfer = nospecializeinfer;
    flags.bits.inlining = inlining;
    flags.bits.constprop = constprop;
    return flags;
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
        elsize = elsize & 0x1fff;
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
        head = jl_call_sym;
    }
    else if (tag == TAG_CALL2) {
        len = 3;
        head = jl_call_sym;
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
    jl_array_t *e = jl_alloc_array_1d(jl_array_int32_type, len_e);
    jl_array_t *v = jl_alloc_vec_any(len_v);
    jl_value_t *phi = jl_new_struct(jl_phinode_type, e, v);
    int32_t *data_e = (int32_t*)(e->data);
    for (i = 0; i < len_e; i++) {
        data_e[i] = jl_unbox_int32(jl_decode_value(s));
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
    jl_set_typeof(v, (void*)(intptr_t)0xf50);
    jl_datatype_t *dt = (jl_datatype_t*)jl_decode_value(s);
    if (dt->smalltag)
        jl_set_typetagof(v, dt->smalltag, 0);
    else
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
    uint64_t key;
    uint8_t tag = read_uint8(s->s);
    if (tag > LAST_TAG)
        return jl_deser_tag(tag);
    switch (tag) {
    case TAG_NULL: return NULL;
    case 0:
        tag = read_uint8(s->s);
        return jl_deser_tag(tag);
    case TAG_RELOC_METHODROOT:
        key = read_uint64(s->s);
        tag = read_uint8(s->s);
        assert(tag == TAG_METHODROOT || tag == TAG_LONG_METHODROOT);
        int index = -1;
        if (tag == TAG_METHODROOT)
            index = read_uint8(s->s);
        else if (tag == TAG_LONG_METHODROOT)
            index = read_uint32(s->s);
        assert(index >= 0);
        return lookup_root(s->method, key, index);
    case TAG_METHODROOT:
        return lookup_root(s->method, 0, read_uint8(s->s));
    case TAG_LONG_METHODROOT:
        return lookup_root(s->method, 0, read_uint32(s->s));
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
        set_nth_field(tag == TAG_GOTONODE ? jl_gotonode_type : jl_quotenode_type, v, 0, jl_decode_value(s), 0);
        return v;
    case TAG_GOTOIFNOT:
        v = jl_new_struct_uninit(jl_gotoifnot_type);
        set_nth_field(jl_gotoifnot_type, v, 0, jl_decode_value(s), 0);
        set_nth_field(jl_gotoifnot_type, v, 1, jl_decode_value(s), 0);
        return v;
    case TAG_ARGUMENT:
        v = jl_new_struct_uninit(jl_argument_type);
        set_nth_field(jl_argument_type, v, 0, jl_decode_value(s), 0);
        return v;
    case TAG_RETURNNODE:
        v = jl_new_struct_uninit(jl_returnnode_type);
        set_nth_field(jl_returnnode_type, v, 0, jl_decode_value(s), 0);
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
            set_nth_field(jl_lineinfonode_type, v, i, jl_decode_value(s), 0);
        }
        return v;
    default:
        assert(tag == TAG_GENERAL || tag == TAG_SHORT_GENERAL);
        return jl_decode_value_any(s, tag);
    }
}

// --- entry points ---

typedef jl_value_t jl_string_t; // for local expressibility

JL_DLLEXPORT jl_string_t *jl_compress_ir(jl_method_t *m, jl_code_info_t *code)
{
    JL_TIMING(AST_COMPRESS, AST_COMPRESS);
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
        jl_current_task->ptls,
        1
    };

    jl_code_info_flags_t flags = code_info_flags(code->inferred, code->propagate_inbounds, code->has_fcall,
                                                 code->nospecializeinfer, code->inlining, code->constprop);
    write_uint8(s.s, flags.packed);
    write_uint8(s.s, code->purity.bits);
    write_uint16(s.s, code->inlining_cost);

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

    // For opaque closure, also save the slottypes. We technically only need the first slot type,
    // but this is simpler for now. We may want to refactor where this gets stored in the future.
    if (m->is_for_opaque_closure)
        jl_encode_value_(&s, code->slottypes, 1);

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

    write_uint8(s.s, s.relocatability);

    ios_flush(s.s);
    jl_string_t *v = jl_pchar_to_string(s.s->buf, s.s->size);
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

JL_DLLEXPORT jl_code_info_t *jl_uncompress_ir(jl_method_t *m, jl_code_instance_t *metadata, jl_string_t *data)
{
    if (jl_is_code_info(data))
        return (jl_code_info_t*)data;
    JL_TIMING(AST_UNCOMPRESS, AST_UNCOMPRESS);
    JL_LOCK(&m->writelock); // protect the roots array (Might GC)
    assert(jl_is_method(m));
    assert(jl_is_string(data));
    size_t i;
    ios_t src;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)jl_string_data(data), jl_string_len(data), 0);
    src.size = jl_string_len(data);
    int en = jl_gc_enable(0); // Might GC
    jl_ircode_state s = {
        &src,
        m,
        jl_current_task->ptls,
        1
    };

    jl_code_info_t *code = jl_new_code_info_uninit();
    jl_code_info_flags_t flags;
    flags.packed = read_uint8(s.s);
    code->inlining = flags.bits.inlining;
    code->constprop = flags.bits.constprop;
    code->inferred = flags.bits.inferred;
    code->propagate_inbounds = flags.bits.propagate_inbounds;
    code->has_fcall = flags.bits.has_fcall;
    code->nospecializeinfer = flags.bits.nospecializeinfer;
    code->purity.bits = read_uint8(s.s);
    code->inlining_cost = read_uint16(s.s);

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
    if (m->is_for_opaque_closure)
        code->slottypes = jl_decode_value(&s);

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

    (void) read_uint8(s.s);   // relocatability

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

JL_DLLEXPORT uint8_t jl_ir_flag_inferred(jl_string_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inferred;
    assert(jl_is_string(data));
    jl_code_info_flags_t flags;
    flags.packed = jl_string_data(data)[0];
    return flags.bits.inferred;
}

JL_DLLEXPORT uint8_t jl_ir_flag_inlining(jl_string_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inlining;
    assert(jl_is_string(data));
    jl_code_info_flags_t flags;
    flags.packed = jl_string_data(data)[0];
    return flags.bits.inlining;
}

JL_DLLEXPORT uint8_t jl_ir_flag_has_fcall(jl_string_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->has_fcall;
    assert(jl_is_string(data));
    jl_code_info_flags_t flags;
    flags.packed = jl_string_data(data)[0];
    return flags.bits.has_fcall;
}

JL_DLLEXPORT uint16_t jl_ir_inlining_cost(jl_string_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inlining_cost;
    assert(jl_is_string(data));
    uint16_t res = jl_load_unaligned_i16(jl_string_data(data) + 2);
    return res;
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

JL_DLLEXPORT ssize_t jl_ir_nslots(jl_value_t *data)
{
    if (jl_is_code_info(data)) {
        jl_code_info_t *func = (jl_code_info_t*)data;
        return jl_array_len(func->slotnames);
    }
    else {
        assert(jl_is_string(data));
        int nslots = jl_load_unaligned_i32(jl_string_data(data) + 2 + sizeof(uint16_t));
        return nslots;
    }
}

JL_DLLEXPORT uint8_t jl_ir_slotflag(jl_string_t *data, size_t i)
{
    assert(i < jl_ir_nslots(data));
    if (jl_is_code_info(data))
        return ((uint8_t*)((jl_code_info_t*)data)->slotflags->data)[i];
    assert(jl_is_string(data));
    return jl_string_data(data)[2 + sizeof(uint16_t) + sizeof(int32_t) + i];
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
        jl_sym_t *name = _jl_symbol(namestr, namelen);
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
            jl_sym_t *name = _jl_symbol(namestr, namelen);
            return (jl_value_t*)name;
        }
        namestr += namelen + 1;
        remaining -= namelen + 1;
    }
    return jl_nothing;
}

void jl_init_serializer(void)
{
    jl_task_t *ct = jl_current_task;
    htable_new(&ser_tag, 0);
    htable_new(&common_symbol_tag, 0);

    void *vals[] = { jl_emptysvec, jl_emptytuple, jl_false, jl_true, jl_nothing, jl_any_type,
                     jl_call_sym, jl_invoke_sym, jl_invoke_modify_sym, jl_goto_ifnot_sym, jl_return_sym, jl_symbol("tuple"),
                     jl_an_empty_string, jl_an_empty_vec_any,

                     // empirical list of very common symbols
                     #include "common_symbols1.inc"

                     jl_box_int32(0), jl_box_int32(1), jl_box_int32(2),
                     jl_box_int32(3), jl_box_int32(4), jl_box_int32(5),
                     jl_box_int32(6), jl_box_int32(7), jl_box_int32(8),
                     jl_box_int32(9), jl_box_int32(10), jl_box_int32(11),
                     jl_box_int32(12), jl_box_int32(13), jl_box_int32(14),
                     jl_box_int32(15), jl_box_int32(16), jl_box_int32(17),
                     jl_box_int32(18), jl_box_int32(19), jl_box_int32(20),

                     jl_box_int64(0), jl_box_int64(1), jl_box_int64(2),
                     jl_box_int64(3), jl_box_int64(4), jl_box_int64(5),
                     jl_box_int64(6), jl_box_int64(7), jl_box_int64(8),
                     jl_box_int64(9), jl_box_int64(10), jl_box_int64(11),
                     jl_box_int64(12), jl_box_int64(13), jl_box_int64(14),
                     jl_box_int64(15), jl_box_int64(16), jl_box_int64(17),
                     jl_box_int64(18), jl_box_int64(19), jl_box_int64(20),

                     jl_bool_type, jl_linenumbernode_type, jl_pinode_type,
                     jl_upsilonnode_type, jl_type_type, jl_bottom_type, jl_ref_type,
                     jl_pointer_type, jl_abstractarray_type, jl_nothing_type,
                     jl_vararg_type,
                     jl_densearray_type, jl_function_type, jl_typename_type,
                     jl_builtin_type, jl_task_type, jl_uniontype_type,
                     jl_array_any_type, jl_intrinsic_type,
                     jl_methtable_type, jl_typemap_level_type,
                     jl_voidpointer_type, jl_newvarnode_type, jl_abstractstring_type,
                     jl_array_symbol_type, jl_anytuple_type, jl_tparam0(jl_anytuple_type),
                     jl_emptytuple_type, jl_array_uint8_type, jl_array_uint32_type, jl_code_info_type,
                     jl_typeofbottom_type, jl_typeofbottom_type->super,
                     jl_namedtuple_type, jl_array_int32_type,
                     jl_uint32_type, jl_uint64_type,
                     jl_type_type_mt, jl_nonfunction_mt,
                     jl_opaque_closure_type,

                     ct->ptls->root_task,

                     NULL };

    // more common symbols, less common than those above. will get 2-byte encodings.
    void *common_symbols[] = {
        #include "common_symbols2.inc"
        NULL
    };

    deser_tag[TAG_SYMBOL] = (jl_value_t*)jl_symbol_type;
    deser_tag[TAG_SSAVALUE] = (jl_value_t*)jl_ssavalue_type;
    deser_tag[TAG_DATATYPE] = (jl_value_t*)jl_datatype_type;
    deser_tag[TAG_SLOTNUMBER] = (jl_value_t*)jl_slotnumber_type;
    deser_tag[TAG_SVEC] = (jl_value_t*)jl_simplevector_type;
    deser_tag[TAG_ARRAY] = (jl_value_t*)jl_array_type;
    deser_tag[TAG_EXPR] = (jl_value_t*)jl_expr_type;
    deser_tag[TAG_PHINODE] = (jl_value_t*)jl_phinode_type;
    deser_tag[TAG_PHICNODE] = (jl_value_t*)jl_phicnode_type;
    deser_tag[TAG_STRING] = (jl_value_t*)jl_string_type;
    deser_tag[TAG_MODULE] = (jl_value_t*)jl_module_type;
    deser_tag[TAG_TVAR] = (jl_value_t*)jl_tvar_type;
    deser_tag[TAG_METHOD_INSTANCE] = (jl_value_t*)jl_method_instance_type;
    deser_tag[TAG_METHOD] = (jl_value_t*)jl_method_type;
    deser_tag[TAG_CODE_INSTANCE] = (jl_value_t*)jl_code_instance_type;
    deser_tag[TAG_GLOBALREF] = (jl_value_t*)jl_globalref_type;
    deser_tag[TAG_INT32] = (jl_value_t*)jl_int32_type;
    deser_tag[TAG_INT64] = (jl_value_t*)jl_int64_type;
    deser_tag[TAG_UINT8] = (jl_value_t*)jl_uint8_type;
    deser_tag[TAG_LINEINFO] = (jl_value_t*)jl_lineinfonode_type;
    deser_tag[TAG_UNIONALL] = (jl_value_t*)jl_unionall_type;
    deser_tag[TAG_GOTONODE] = (jl_value_t*)jl_gotonode_type;
    deser_tag[TAG_QUOTENODE] = (jl_value_t*)jl_quotenode_type;
    deser_tag[TAG_GOTOIFNOT] = (jl_value_t*)jl_gotoifnot_type;
    deser_tag[TAG_RETURNNODE] = (jl_value_t*)jl_returnnode_type;
    deser_tag[TAG_ARGUMENT] = (jl_value_t*)jl_argument_type;

    intptr_t i = 0;
    while (vals[i] != NULL) {
        deser_tag[LAST_TAG+1+i] = (jl_value_t*)vals[i];
        i += 1;
    }
    assert(LAST_TAG+1+i < 256);

    for (i = 2; i < 256; i++) {
        if (deser_tag[i])
            ptrhash_put(&ser_tag, deser_tag[i], (void*)i);
    }

    i = 2;
    while (common_symbols[i-2] != NULL) {
        ptrhash_put(&common_symbol_tag, common_symbols[i-2], (void*)i);
        deser_symbols[i] = (jl_value_t*)common_symbols[i-2];
        i += 1;
    }
    assert(i <= 256);
}

#ifdef __cplusplus
}
#endif
