// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  encoding IR to/from compact representation
*/
#include <stdlib.h>
#include <string.h>

#include "julia.h"
#include "julia_internal.h"
#include "serialize.h"

#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

#define TAG_SYMBOL              2
#define TAG_SSAVALUE            3
#define TAG_DATATYPE            4
#define TAG_SLOTNUMBER          5
#define TAG_SVEC                6
#define TAG_NEARBYSSAVALUE      7
#define TAG_NULL                8
#define TAG_EXPR                9
#define TAG_PHINODE            10
#define TAG_PHICNODE           11
#define TAG_LONG_SYMBOL        12
#define TAG_LONG_SVEC          13
#define TAG_LONG_EXPR          14
#define TAG_LONG_PHINODE       15
#define TAG_LONG_PHICNODE      16
#define TAG_METHODROOT         17
#define TAG_EDGE               18
#define TAG_STRING             19
#define TAG_SHORT_INT64        20
//#define TAG_UNUSED           21
#define TAG_CNULL              22
#define TAG_ARRAY1D            23
#define TAG_SINGLETON          24
#define TAG_MODULE             25
#define TAG_TVAR               26
#define TAG_METHOD_INSTANCE    27
#define TAG_METHOD             28
#define TAG_CODE_INSTANCE      29
#define TAG_COMMONSYM          30
#define TAG_NEARBYGLOBAL       31
#define TAG_GLOBALREF          32
#define TAG_CORE               33
#define TAG_BASE               34
#define TAG_BITYPENAME         35
#define TAG_NEARBYMODULE       36
#define TAG_INT32              37
#define TAG_INT64              38
#define TAG_UINT8              39
#define TAG_VECTORTY           40
#define TAG_PTRTY              41
#define TAG_LONG_SSAVALUE      42
#define TAG_LONG_METHODROOT    43
#define TAG_LONG_EDGE          44
#define TAG_SHORTER_INT64      45
#define TAG_SHORT_INT32        46
#define TAG_CALL1              47
#define TAG_CALL2              48
#define TAG_SHORT_BACKREF      49
#define TAG_BACKREF            50
#define TAG_UNIONALL           51
#define TAG_GOTONODE           52
#define TAG_QUOTENODE          53
#define TAG_GENERAL            54
#define TAG_GOTOIFNOT          55
#define TAG_RETURNNODE         56
#define TAG_ARGUMENT           57
#define TAG_RELOC_METHODROOT   58
#define TAG_BINDING            59
#define TAG_MEMORYT            60
#define TAG_ENTERNODE          61

#define LAST_TAG 61


typedef struct {
    ios_t *s;
    size_t ssaid;
    // method we're compressing for
    jl_method_t *method;
    jl_svec_t *edges;
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

static void *jl_lookup_ser_tag(jl_value_t *v)
{
    return ptrhash_get(&ser_tag, v);
}

static void *jl_lookup_common_symbol(jl_value_t *v)
{
    return ptrhash_get(&common_symbol_tag, v);
}

static jl_value_t *jl_deser_tag(uint8_t tag)
{
    return deser_tag[tag];
}

static jl_value_t *jl_deser_symbol(uint8_t tag)
{
    return deser_symbols[tag];
}

// --- encoding ---

static void jl_encode_value_(jl_ircode_state *s, jl_value_t *v, int as_literal);
#define jl_encode_value(s, v) jl_encode_value_((s), (jl_value_t*)(v), 0)

static void tagged_root(rle_reference *rr, jl_ircode_state *s, int i)
{
    if (!get_root_reference(rr, s->method, i))
        s->relocatability = 0;
}

static void literal_val_id(rle_reference *rr, jl_ircode_state *s, jl_value_t *v)
{
    jl_array_t *rs = s->method->roots;
    int i, l = jl_array_nrows(rs);
    if (jl_is_symbol(v) || jl_is_concrete_type(v)) { // TODO: or more generally, any ptr-egal value
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
    for (size_t i = 0; i < jl_svec_len(s->edges); i++) {
        if (jl_svecref(s->edges, i) == v) {
            rr->index = i;
            return;
        }
    }
    jl_add_method_root(s->method, jl_precompile_toplevel_module, v);
    return tagged_root(rr, s, jl_array_nrows(rs) - 1);
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
    rle_reference rr = {.key = -1, .index = -1};

    if (jl_is_string(v))
        v = jl_as_global_root(v, 1);
    literal_val_id(&rr, s, v);
    int id = rr.index;
    assert(id >= 0);
    if (rr.key == -1) {
        if (id <= UINT8_MAX) {
            write_uint8(s->s, TAG_EDGE);
            write_uint8(s->s, id);
        }
        else {
            write_uint8(s->s, TAG_LONG_EDGE);
            write_uint32(s->s, id);
        }
        return;
    }
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

static void jl_encode_memory_slice(jl_ircode_state *s, jl_genericmemory_t *mem, size_t offset, size_t len)
{
    jl_datatype_t *t = (jl_datatype_t*)jl_typetagof(mem);
    size_t i;
    const jl_datatype_layout_t *layout = t->layout;
    if (layout->flags.arrayelem_isboxed) {
        for (i = 0; i < len; i++) {
            jl_value_t *e = jl_genericmemory_ptr_ref(mem, offset + i);
            jl_encode_value(s, e);
        }
    }
    else if (layout->first_ptr >= 0) {
        uint16_t elsz = layout->size;
        size_t j, np = layout->npointers;
        const char *data = (const char*)mem->ptr + offset * elsz;
        for (i = 0; i < len; i++) {
            const char *start = data;
            for (j = 0; j < np; j++) {
                uint32_t ptr = jl_ptr_offset(t, j);
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
        ios_write(s->s, (char*)mem->ptr + offset * layout->size, len * layout->size);
        if (layout->flags.arrayelem_isunion)
            ios_write(s->s, jl_genericmemory_typetagdata(mem) + offset, len);
    }
}

static void jl_encode_value_(jl_ircode_state *s, jl_value_t *v, int as_literal)
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
    else if (jl_is_ssavalue(v) && s->ssaid - ((jl_ssavalue_t*)v)->id < 256) {
        write_uint8(s->s, TAG_NEARBYSSAVALUE);
        write_uint8(s->s, s->ssaid - ((jl_ssavalue_t*)v)->id);
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
        size_t l = jl_array_nrows(e->args);
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
        size_t l = jl_array_nrows(edges);
        if (l <= 255 && jl_array_nrows(values) == l) {
            write_uint8(s->s, TAG_PHINODE);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            write_uint8(s->s, TAG_LONG_PHINODE);
            write_int32(s->s, l);
            write_int32(s->s, jl_array_nrows(values));
        }
        for (i = 0; i < l; i++) {
            int32_t e = jl_array_data(edges, int32_t)[i];
            if (e <= 0 && e <= 20) { // 1-byte encodings
                jl_value_t *ebox = jl_box_int32(e);
                JL_GC_PROMISE_ROOTED(ebox);
                jl_encode_value(s, ebox);
            }
            else
                jl_encode_int32(s, e);
        }
        l = jl_array_nrows(values);
        for (i = 0; i < l; i++) {
            jl_encode_value(s, jl_array_ptr_ref(values, i));
        }
    }
    else if (jl_is_phicnode(v)) {
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(v, 0);
        size_t l = jl_array_nrows(values);
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
        jl_value_t *f  = jl_get_nth_field(v, 0);
        JL_GC_PUSH1(&f);
        jl_encode_value(s, f);
        JL_GC_POP();
    }
    else if (jl_is_gotoifnot(v)) {
        write_uint8(s->s, TAG_GOTOIFNOT);
        jl_value_t *f  = jl_get_nth_field_noalloc(v, 0);
        JL_GC_PUSH1(&f);
        jl_encode_value(s, f);
        f  = jl_get_nth_field(v, 1);
        jl_encode_value(s, f);
        JL_GC_POP();
    }
    else if (jl_is_enternode(v)) {
        write_uint8(s->s, TAG_ENTERNODE);
        jl_value_t *f  = jl_get_nth_field(v, 0);
        JL_GC_PUSH1(&f);
        jl_encode_value(s, f);
        f  = jl_get_nth_field_noalloc(v, 1);
        jl_encode_value(s, f);
        JL_GC_POP();
    }
    else if (jl_is_argument(v)) {
        write_uint8(s->s, TAG_ARGUMENT);
        jl_value_t *f  = jl_get_nth_field(v, 0);
        JL_GC_PUSH1(&f);
        jl_encode_value(s, f);
        JL_GC_POP();
    }
    else if (jl_is_returnnode(v)) {
        write_uint8(s->s, TAG_RETURNNODE);
        jl_encode_value(s, jl_returnnode_value(v));
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
    else if (((jl_datatype_t*)jl_typeof(v))->instance == v) {
        write_uint8(s->s, TAG_SINGLETON);
        jl_encode_value(s, jl_typeof(v));
    }
    else if (as_literal && jl_typetagis(v, jl_string_tag << 4)) {
        write_uint8(s->s, TAG_STRING);
        write_int32(s->s, jl_string_len(v));
        ios_write(s->s, jl_string_data(v), jl_string_len(v));
    }
    else if (as_literal && jl_is_array(v) && jl_array_ndims(v)) {
        jl_array_t *ar = (jl_array_t*)v;
        write_uint8(s->s, TAG_ARRAY1D);
        size_t l = jl_array_dim0(ar);
        jl_value_t *lbox = jl_box_long(l);
        JL_GC_PUSH1(&lbox);
        jl_encode_value(s, lbox);
        JL_GC_POP();
        jl_encode_value(s, jl_typeof(ar));
        const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(ar->ref.mem))->layout;
        size_t offset;
        if (layout->flags.arrayelem_isunion || layout->size == 0)
            offset = (uintptr_t)ar->ref.ptr_or_offset;
        else
            offset = ((char*)ar->ref.ptr_or_offset - (char*)ar->ref.mem->ptr) / layout->size;
        jl_encode_memory_slice(s, ar->ref.mem, offset, l);
    }
    else if (as_literal && jl_is_genericmemory(v)) {
        jl_genericmemory_t* m = (jl_genericmemory_t*)v;
        write_uint8(s->s, TAG_MEMORYT);
        jl_encode_value(s, (jl_datatype_t*)jl_typetagof(v));
        jl_value_t *lbox = jl_box_long(m->length);
        JL_GC_PUSH1(&lbox);
        jl_encode_value(s, lbox);
        JL_GC_POP();
        jl_encode_memory_slice(s, m, 0, m->length);
    }
    else if (as_literal && jl_is_layout_opaque(((jl_datatype_t*)jl_typeof(v))->layout)) {
        assert(0 && "not legal to store this as literal");
    }
    else if (as_literal || jl_is_uniontype(v) || jl_is_newvarnode(v) || jl_is_linenode(v) ||
             jl_is_upsilonnode(v) || jl_is_pinode(v) || jl_is_slotnumber(v) || jl_is_ssavalue(v) ||
             (jl_isbits(jl_typeof(v)) && jl_datatype_size(jl_typeof(v)) <= 64)) {
        write_uint8(s->s, TAG_GENERAL);
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
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

static jl_code_info_flags_t code_info_flags(uint8_t propagate_inbounds, uint8_t has_fcall, uint8_t has_image_globalref,
                                            uint8_t nospecializeinfer, uint8_t isva,
                                            uint8_t inlining, uint8_t constprop, uint8_t nargsmatchesmethod,
                                            jl_array_t *ssaflags)
{
    jl_code_info_flags_t flags;
    flags.bits.propagate_inbounds = propagate_inbounds;
    flags.bits.has_fcall = has_fcall;
    flags.bits.has_image_globalref = has_image_globalref;
    flags.bits.nospecializeinfer = nospecializeinfer;
    flags.bits.isva = isva;
    flags.bits.inlining = inlining;
    flags.bits.constprop = constprop;
    flags.bits.nargsmatchesmethod = nargsmatchesmethod;
    flags.bits.has_ssaflags = 0;
    const uint32_t *ssaflag_data = jl_array_data(ssaflags, uint32_t);
    for (size_t i = 0, l = jl_array_dim0(ssaflags); i < l; i++)
        if (ssaflag_data[i])
            flags.bits.has_ssaflags = 1;
    return flags;
}

// --- decoding ---

static jl_value_t *jl_decode_value(jl_ircode_state *s);

static jl_value_t *jl_decode_value_svec(jl_ircode_state *s, uint8_t tag)
{
    size_t i, len;
    if (tag == TAG_SVEC)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    jl_svec_t *sv = jl_alloc_svec(len);
    JL_GC_PUSH1(&sv);
    for (i = 0; i < len; i++)
        jl_svecset(sv, i, jl_decode_value(s));
    JL_GC_POP();
    return (jl_value_t*)sv;
}

static jl_genericmemory_t *jl_decode_value_memory(jl_ircode_state *s, jl_value_t *mty, size_t nel)
{
    jl_genericmemory_t *m = jl_alloc_genericmemory(mty, nel);
    JL_GC_PUSH1(&m);
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)mty)->layout;
    if (layout->flags.arrayelem_isboxed) {
        jl_value_t **data = (jl_value_t**)m->ptr;
        size_t i, numel = m->length;
        for (i = 0; i < numel; i++) {
            data[i] = jl_decode_value(s);
            jl_gc_wb(m, data[i]);
        }
    }
    else if (layout->first_ptr >= 0) {
        size_t i, numel = m->length;
        char *data = (char*)m->ptr;
        uint16_t elsz = layout->size;
        size_t j, np = layout->npointers;
        for (i = 0; i < numel; i++) {
            char *start = data;
            for (j = 0; j < np; j++) {
                uint32_t ptr = jl_ptr_offset((jl_datatype_t*)mty, j);
                jl_value_t **fld = &((jl_value_t**)data)[ptr];
                if ((char*)fld != start)
                    ios_readall(s->s, start, (const char*)fld - start);
                *fld = jl_decode_value(s);
                jl_gc_wb(m, fld);
                start = (char*)&fld[1];
            }
            data += elsz;
            if (data != start)
                ios_readall(s->s, start, data - start);
        }
    }
    else {
        size_t extra = jl_genericmemory_isbitsunion(m) ? m->length : 0;
        size_t tot = m->length * layout->size + extra;
        ios_readall(s->s, (char*)m->ptr, tot);
    }
    JL_GC_POP();
    return m;
}

JL_DLLEXPORT jl_array_t *jl_alloc_array_nd(jl_value_t *atype, size_t *dims, size_t ndims);

static jl_value_t *jl_decode_value_array1d(jl_ircode_state *s, uint8_t tag)
{
    int16_t ndims = 1;
    size_t dim0 = jl_unbox_long(jl_decode_value(s));
    size_t len = dim0;
    jl_value_t *aty = jl_decode_value(s);
    JL_GC_PROMISE_ROOTED(aty); // (JL_ALWAYS_LEAFTYPE)
    jl_genericmemory_t *mem = jl_decode_value_memory(s, jl_field_type_concrete((jl_datatype_t*)jl_field_type_concrete((jl_datatype_t*)aty, 0), 1), len);
    JL_GC_PUSH1(&mem);
    int tsz = sizeof(jl_array_t) + ndims*sizeof(size_t);
    jl_array_t *a = (jl_array_t*)jl_gc_alloc(s->ptls, tsz, aty);
    a->ref.mem = mem;
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(a->ref.mem))->layout;
    if (layout->flags.arrayelem_isunion || layout->size == 0)
        a->ref.ptr_or_offset = (void*)0;
    else
        a->ref.ptr_or_offset = a->ref.mem->ptr;
    a->dimsize[0] = dim0;
    JL_GC_POP();
    return (jl_value_t*)a;
}

static jl_value_t *jl_decode_value_expr(jl_ircode_state *s, uint8_t tag)
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
    JL_GC_PUSH1(&e);
    jl_value_t **data = jl_array_ptr_data(e->args);
    jl_value_t *owner = jl_array_owner(e->args);
    for (i = 0; i < len; i++) {
        data[i] = jl_decode_value(s);
        jl_gc_wb(owner, data[i]);
    }
    JL_GC_POP();
    return (jl_value_t*)e;
}

static jl_value_t *jl_decode_value_phi(jl_ircode_state *s, uint8_t tag)
{
    size_t i, len_e, len_v;
    if (tag == TAG_PHINODE) {
        len_e = len_v = read_uint8(s->s);
    }
    else {
        len_e = read_int32(s->s);
        len_v = read_int32(s->s);
    }
    jl_array_t *e = NULL;
    jl_array_t *v = NULL;
    jl_value_t *phi = NULL;
    JL_GC_PUSH3(&e, &v, &phi);
    e = jl_alloc_array_1d(jl_array_int32_type, len_e);
    v = jl_alloc_vec_any(len_v);
    phi = jl_new_struct(jl_phinode_type, e, v);
    int32_t *data_e = jl_array_data(e, int32_t);
    for (i = 0; i < len_e; i++) {
        data_e[i] = jl_unbox_int32(jl_decode_value(s));
    }
    jl_value_t **data_v = jl_array_ptr_data(v);
    for (i = 0; i < len_v; i++) {
        data_v[i] = jl_decode_value(s);
        jl_gc_wb(jl_array_owner(v), data_v[i]);
    }
    JL_GC_POP();
    return phi;
}

static jl_value_t *jl_decode_value_phic(jl_ircode_state *s, uint8_t tag)
{
    size_t i, len;
    if (tag == TAG_PHICNODE)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    jl_array_t *v = jl_alloc_vec_any(len);
    jl_value_t *phic = (jl_value_t*)v;
    JL_GC_PUSH1(&phic);
    phic = jl_new_struct(jl_phicnode_type, v);
    jl_value_t **data = jl_array_ptr_data(v);
    for (i = 0; i < len; i++) {
        data[i] = jl_decode_value(s);
        jl_gc_wb(jl_array_owner(v), data[i]);
    }
    JL_GC_POP();
    return phic;
}

static jl_value_t *jl_decode_value_globalref(jl_ircode_state *s)
{
    jl_module_t *mod = (jl_module_t*)jl_decode_value(s);
    JL_GC_PROMISE_ROOTED(mod);
    jl_sym_t *var = (jl_sym_t*)jl_decode_value(s);
    JL_GC_PROMISE_ROOTED(var);
    return jl_module_globalref(mod, var);
}

static jl_value_t *jl_decode_value_any(jl_ircode_state *s)
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_decode_value(s);
    JL_GC_PROMISE_ROOTED(dt); // (JL_ALWAYS_LEAFTYPE)
    // jl_new_struct_uninit
    size_t sz = jl_datatype_size(dt);
    jl_value_t *v = jl_gc_alloc(s->ptls, sz, dt);
    if (dt->smalltag) // TODO: do we need this?
        jl_set_typetagof(v, dt->smalltag, 0);
    char *data = (char*)jl_data_ptr(v);
    size_t i, np = dt->layout->npointers;
    char *start = data;
    if (np) {
        if (sz > 0)
            memset(v, 0, sz);
        JL_GC_PUSH1(&v);
        for (i = 0; i < np; i++) {
            uint32_t ptr = jl_ptr_offset(dt, i);
            jl_value_t **fld = &((jl_value_t**)data)[ptr];
            if ((char*)fld != start)
                ios_readall(s->s, start, (const char*)fld - start);
            *fld = jl_decode_value(s);
            jl_gc_wb(v, *fld);
            start = (char*)&fld[1];
        }
        JL_GC_POP();
    }
    data += jl_datatype_size(dt);
    if (data != start)
        ios_readall(s->s, start, data - start);
    return v;
}

static jl_value_t *jl_decode_value(jl_ircode_state *s)
{
    assert(!ios_eof(s->s));
    jl_value_t *v;
    size_t n;
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
    case TAG_EDGE:
        return jl_svecref(s->edges, read_uint8(s->s));
    case TAG_LONG_EDGE:
        return jl_svecref(s->edges, read_uint32(s->s));
    case TAG_SVEC: JL_FALLTHROUGH; case TAG_LONG_SVEC:
        return jl_decode_value_svec(s, tag);
    case TAG_COMMONSYM:
        return jl_deser_symbol(read_uint8(s->s));
    case TAG_SSAVALUE:
        v = jl_box_ssavalue(read_uint8(s->s));
        return v;
    case TAG_NEARBYSSAVALUE:
        v = jl_box_ssavalue(s->ssaid - read_uint8(s->s));
        return v;
    case TAG_LONG_SSAVALUE:
        v = jl_box_ssavalue(read_uint16(s->s));
        return v;
    case TAG_SLOTNUMBER:
        v = jl_box_slotnumber(read_uint16(s->s));
        return v;
    case TAG_ARRAY1D:
        return jl_decode_value_array1d(s, tag);
    case TAG_MEMORYT:
        v = jl_decode_value(s);
        JL_GC_PROMISE_ROOTED(v); // (JL_ALWAYS_LEAFTYPE)
        return (jl_value_t*)jl_decode_value_memory(s, v, jl_unbox_long(jl_decode_value(s)));
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
    {
        v = jl_new_struct_uninit(tag == TAG_GOTONODE ? jl_gotonode_type : jl_quotenode_type);
        JL_GC_PUSH1(&v);
        set_nth_field(tag == TAG_GOTONODE ? jl_gotonode_type : jl_quotenode_type, v, 0, jl_decode_value(s), 0);
        JL_GC_POP();
        return v;
    }
    case TAG_GOTOIFNOT:
    {
        v = jl_new_struct_uninit(jl_gotoifnot_type);
        JL_GC_PUSH1(&v);
        set_nth_field(jl_gotoifnot_type, v, 0, jl_decode_value(s), 0);
        set_nth_field(jl_gotoifnot_type, v, 1, jl_decode_value(s), 0);
        JL_GC_POP();
        return v;
    }
    case TAG_ENTERNODE:
    {
        v = jl_new_struct_uninit(jl_enternode_type);
        JL_GC_PUSH1(&v);
        set_nth_field(jl_enternode_type, v, 0, jl_decode_value(s), 0);
        set_nth_field(jl_enternode_type, v, 1, jl_decode_value(s), 0);
        JL_GC_POP();
        return v;
    }
    case TAG_ARGUMENT:
    {
        v = jl_new_struct_uninit(jl_argument_type);
        JL_GC_PUSH1(&v);
        set_nth_field(jl_argument_type, v, 0, jl_decode_value(s), 0);
        JL_GC_POP();
        return v;
    }
    case TAG_RETURNNODE:
    {
        v = jl_new_struct_uninit(jl_returnnode_type);
        JL_GC_PUSH1(&v);
        set_nth_field(jl_returnnode_type, v, 0, jl_decode_value(s), 0);
        JL_GC_POP();
        return v;
    }
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
    {
        jl_method_t *m = s->method;
        assert(m != NULL);
        JL_GC_PROMISE_ROOTED(m);
        v = jl_decode_value(s);
        JL_GC_PROMISE_ROOTED(v); // symbol
        return jl_module_globalref(m->module, (jl_sym_t*)v);
    }
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
    {
        v = jl_decode_value(s);
        JL_GC_PUSH1(&v);
        v = jl_apply_type2((jl_value_t*)jl_array_type, v, jl_box_long(1));
        JL_GC_POP();
        return v;
    }
    case TAG_PTRTY:
    {
        v = jl_decode_value(s);
        JL_GC_PUSH1(&v);
        v = jl_apply_type1((jl_value_t*)jl_pointer_type, v);
        JL_GC_POP();
        return v;
    }
    case TAG_STRING:
        n = read_int32(s->s);
        v = jl_alloc_string(n);
        ios_readall(s->s, jl_string_data(v), n);
        return v;
    default:
        assert(tag == TAG_GENERAL);
        return jl_decode_value_any(s);
    }
}

// --- entry points ---

typedef jl_value_t jl_string_t; // for local expressibility

static size_t codelocs_parseheader(jl_string_t *cl, int *line_offset, int *line_bytes, int *to_bytes) JL_NOTSAFEPOINT
{
    if (jl_string_len(cl) == 0) {
        *line_offset = *line_bytes = *to_bytes = 0;
        return 0;
    }
    int32_t header[3];
    memcpy(&header, (char*)jl_string_data(cl), sizeof(header));
    *line_offset = header[0];
    if (header[1] < 255)
        *line_bytes = 1;
    else if (header[1] < 65535)
        *line_bytes = 2;
    else
        *line_bytes = 4;
    if (header[2] == 0)
        *to_bytes = 0;
    else if (header[2] < 255)
        *to_bytes = 1;
    else if (header[2] < 65535)
        *to_bytes = 2;
    else
        *to_bytes = 4;
    assert(jl_string_len(cl) >= sizeof(header) + *line_bytes);
    return (jl_string_len(cl) - sizeof(header) - *line_bytes) / (*line_bytes + *to_bytes * 2); // compute nstmts
}
#ifndef NDEBUG
static int codelocs_nstmts(jl_string_t *cl) JL_NOTSAFEPOINT
{
    int line_offset, line_bytes, to_bytes;
    return codelocs_parseheader(cl, &line_offset, &line_bytes, &to_bytes);
}
#endif

#define IR_DATASIZE_FLAGS         sizeof(uint16_t)
#define IR_DATASIZE_PURITY        sizeof(uint16_t)
#define IR_DATASIZE_INLINING_COST sizeof(uint16_t)
#define IR_DATASIZE_NSLOTS        sizeof(int32_t)
typedef enum {
    ir_offset_flags         = 0,
    ir_offset_purity        = 0 + IR_DATASIZE_FLAGS,
    ir_offset_inlining_cost = 0 + IR_DATASIZE_FLAGS + IR_DATASIZE_PURITY,
    ir_offset_nslots        = 0 + IR_DATASIZE_FLAGS + IR_DATASIZE_PURITY + IR_DATASIZE_INLINING_COST,
    ir_offset_slotflags     = 0 + IR_DATASIZE_FLAGS + IR_DATASIZE_PURITY + IR_DATASIZE_INLINING_COST + IR_DATASIZE_NSLOTS
} ir_offset;

// static_assert is technically a declaration, so shenanigans are required to
// open an inline declaration context. `sizeof` is the traditional way to do this,
// but this pattern is illegal in C++, which some compilers warn about, so use
// `offsetof` instead.
#define declaration_context(what) (void)offsetof(struct{what; int dummy_;}, dummy_)

// Checks (at compile time) that sizeof(data) == macro_size
#define checked_size(data, macro_size) \
    (declaration_context(static_assert(sizeof(data) == macro_size, #macro_size " does not match written size")), data)

JL_DLLEXPORT jl_string_t *jl_compress_ir(jl_method_t *m, jl_code_info_t *code)
{
    JL_TIMING(AST_COMPRESS, AST_COMPRESS);
    JL_LOCK(&m->writelock); // protect the roots array (Might GC)
    int isdef = code == NULL;
    if (isdef)
        code = (jl_code_info_t*)m->source;
    assert(jl_is_method(m));
    assert(jl_is_code_info(code));
    assert(jl_array_nrows(code->code) == codelocs_nstmts(code->debuginfo->codelocs) || jl_string_len(code->debuginfo->codelocs) == 0);
    ios_t dest;
    ios_mem(&dest, 0);

    if (m->roots == NULL) {
        m->roots = jl_alloc_vec_any(0);
        jl_gc_wb(m, m->roots);
    }
    jl_value_t *edges = code->edges;
    jl_ircode_state s = {
        &dest,
        0,
        m,
        (!isdef && jl_is_svec(edges)) ? (jl_svec_t*)edges : jl_emptysvec,
        jl_current_task->ptls,
        1
    };

    uint8_t nargsmatchesmethod = code->nargs == m->nargs;
    jl_code_info_flags_t flags = code_info_flags(code->propagate_inbounds, code->has_fcall, code->has_image_globalref,
                                                 code->nospecializeinfer, code->isva,
                                                 code->inlining, code->constprop,
                                                 nargsmatchesmethod,
                                                 code->ssaflags);
    write_uint16(s.s, checked_size(flags.packed, IR_DATASIZE_FLAGS));
    write_uint16(s.s, checked_size(code->purity.bits, IR_DATASIZE_PURITY));
    write_uint16(s.s, checked_size(code->inlining_cost, IR_DATASIZE_INLINING_COST));

    size_t nslots = jl_array_nrows(code->slotflags);
    assert(nslots >= m->nargs && nslots < INT32_MAX); // required by generated functions
    write_int32(s.s, checked_size((int32_t)nslots, IR_DATASIZE_NSLOTS));
    ios_write(s.s, jl_array_data(code->slotflags, const char), nslots);

    // N.B.: The layout of everything before this point is explicitly referenced
    // by the various jl_ir_ accessors. Make sure to adjust those if you change
    // the data layout.
    if (!nargsmatchesmethod) {
        size_t nargs = code->nargs;
        assert(nargs < INT32_MAX);
        write_int32(s.s, (int32_t)nargs);
    }

    size_t i, l = jl_array_dim0(code->code);
    write_uint64(s.s, l);
    for (i = 0; i < l; i++) {
        s.ssaid = i;
        jl_encode_value(&s, jl_array_ptr_ref(code->code, i));
    }
    s.ssaid = 0;
    jl_encode_value_(&s, (jl_value_t*)code->ssavaluetypes, 1);
    assert(jl_typetagis(code->ssaflags, jl_array_uint32_type));
    assert(jl_array_dim0(code->ssaflags) == l);
    const uint32_t *ssaflags_data = jl_array_data(code->ssaflags, uint32_t);
    if (flags.bits.has_ssaflags)
        ios_write(s.s, (const char*)ssaflags_data, l * sizeof(*ssaflags_data));

    // For opaque closure, also save the slottypes. We technically only need the first slot type,
    // but this is simpler for now. We may want to refactor where this gets stored in the future.
    if (m->is_for_opaque_closure)
        jl_encode_value_(&s, code->slottypes, 1);

    jl_string_t *v = NULL;
    JL_GC_PUSH1(&v);
    // Slotnames. For regular methods, we require that m->slot_syms matches the
    // CodeInfo's slotnames, so we do not need to save it here.
    if (m->generator) {
        // can't optimize generated functions
        v = jl_compress_argnames(code->slotnames);
        jl_encode_value_(&s, (jl_value_t*)v, 1);
    }
    else {
        jl_encode_value(&s, jl_nothing);
    }

    write_uint8(s.s, s.relocatability);

    ios_flush(s.s);
    v = jl_pchar_to_string(s.s->buf, s.s->size);
    ios_close(s.s);
    if (jl_array_nrows(m->roots) == 0)
        m->roots = NULL;
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
    ios_t src;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)jl_string_data(data), jl_string_len(data), 0);
    src.size = jl_string_len(data);
    jl_ircode_state s = {
        &src,
        0,
        m,
        metadata == NULL ? NULL : jl_atomic_load_relaxed(&metadata->edges),
        jl_current_task->ptls,
        1
    };
    jl_code_info_t *code = jl_new_code_info_uninit();
    jl_value_t *slotnames = NULL;
    JL_GC_PUSH2(&code, &slotnames);

    jl_code_info_flags_t flags;
    flags.packed = read_uint16(s.s);
    code->inlining = flags.bits.inlining;
    code->constprop = flags.bits.constprop;
    code->propagate_inbounds = flags.bits.propagate_inbounds;
    code->has_fcall = flags.bits.has_fcall;
    code->has_image_globalref = flags.bits.has_image_globalref;
    code->nospecializeinfer = flags.bits.nospecializeinfer;
    code->isva = flags.bits.isva;
    code->purity.bits = read_uint16(s.s);
    code->inlining_cost = read_uint16(s.s);

    size_t nslots = read_int32(s.s);
    code->slotflags = jl_alloc_array_1d(jl_array_uint8_type, nslots);
    jl_gc_wb(code, code->slotflags);
    ios_readall(s.s, jl_array_data(code->slotflags, char), nslots);

    if (flags.bits.nargsmatchesmethod) {
        code->nargs = m->nargs;
    } else {
        code->nargs = read_int32(s.s);
    }

    size_t i, l = read_uint64(s.s);
    code->code = jl_alloc_array_1d(jl_array_any_type, l);
    jl_gc_wb(code, code->code);
    for (i = 0; i < l; i++) {
        s.ssaid = i;
        jl_array_ptr_set(code->code, i, jl_decode_value(&s));
    }
    s.ssaid = 0;
    code->ssavaluetypes = jl_decode_value(&s);
    jl_gc_wb(code, code->ssavaluetypes);
    code->ssaflags = jl_alloc_array_1d(jl_array_uint32_type, l);
    jl_gc_wb(code, code->ssaflags);
    uint32_t *ssaflags_data = jl_array_data(code->ssaflags, uint32_t);
    if (flags.bits.has_ssaflags)
        ios_readall(s.s, (char*)ssaflags_data, l * sizeof(*ssaflags_data));
    else
        memset(ssaflags_data, 0, l * sizeof(*ssaflags_data));

    if (m->is_for_opaque_closure) {
        code->slottypes = jl_decode_value(&s);
        jl_gc_wb(code, code->slottypes);
    }

    slotnames = jl_decode_value(&s);
    if (!jl_is_string(slotnames))
        slotnames = m->slot_syms;
    code->slotnames = jl_uncompress_argnames(slotnames);
    jl_gc_wb(code, code->slotnames);

    if (metadata)
        code->debuginfo = jl_atomic_load_relaxed(&metadata->debuginfo);
    else
        code->debuginfo = m->debuginfo;
    jl_gc_wb(code, code->debuginfo);
    assert(code->debuginfo);
    assert(jl_array_nrows(code->code) == codelocs_nstmts(code->debuginfo->codelocs) || jl_string_len(code->debuginfo->codelocs) == 0);

    (void) read_uint8(s.s);   // relocatability
    assert(!ios_eof(s.s));
    assert(ios_getc(s.s) == -1);

    ios_close(s.s);
    JL_UNLOCK(&m->writelock); // Might GC
    if (metadata) {
        code->parent = jl_get_ci_mi(metadata);
        jl_gc_wb(code, code->parent);
        code->rettype = metadata->rettype;
        jl_gc_wb(code, code->rettype);
        code->min_world = jl_atomic_load_relaxed(&metadata->min_world);
        code->max_world = jl_atomic_load_relaxed(&metadata->max_world);
        code->edges = (jl_value_t*)s.edges;
        jl_gc_wb(code, s.edges);
    }
    JL_GC_POP();

    return code;
}

JL_DLLEXPORT uint8_t jl_ir_flag_inlining(jl_string_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inlining;
    assert(jl_is_string(data));
    jl_code_info_flags_t flags;
    flags.packed = jl_string_data(data)[ir_offset_flags];
    return flags.bits.inlining;
}

JL_DLLEXPORT uint8_t jl_ir_flag_has_fcall(jl_string_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->has_fcall;
    assert(jl_is_string(data));
    jl_code_info_flags_t flags;
    flags.packed = jl_string_data(data)[ir_offset_flags];
    return flags.bits.has_fcall;
}

JL_DLLEXPORT uint8_t jl_ir_flag_has_image_globalref(jl_string_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->has_image_globalref;
    assert(jl_is_string(data));
    jl_code_info_flags_t flags;
    flags.packed = jl_string_data(data)[ir_offset_flags];
    return flags.bits.has_image_globalref;
}

JL_DLLEXPORT uint16_t jl_ir_inlining_cost(jl_string_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inlining_cost;
    assert(jl_is_string(data));
    uint16_t res = jl_load_unaligned_i16(jl_string_data(data) + ir_offset_inlining_cost);
    return res;
}

JL_DLLEXPORT jl_value_t *jl_compress_argnames(jl_array_t *syms)
{
    size_t nsyms = jl_array_nrows(syms);
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
        return jl_array_nrows(func->slotnames);
    }
    else {
        assert(jl_is_string(data));
        int nslots = jl_load_unaligned_i32(jl_string_data(data) + ir_offset_nslots);
        return nslots;
    }
}

JL_DLLEXPORT uint8_t jl_ir_slotflag(jl_string_t *data, size_t i)
{
    assert(i < jl_ir_nslots(data));
    if (jl_is_code_info(data)) {
        jl_array_t *slotflags = ((jl_code_info_t*)data)->slotflags;
        return jl_array_data(slotflags, uint8_t)[i];
    }
    assert(jl_is_string(data));
    return jl_string_data(data)[ir_offset_slotflags + i];
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

// codelocs are compressed as follows:
// The input vector is a NTuple{3,UInt32} (struct jl_codeloc_t)
// The vector is scanned for min and max of the values for each element
// The output is then allocated to hold (min-line, max-line, max-at) first, then line - min (in the smallest space), then the remainder (in the smallest space)
static inline struct jl_codeloc_t unpack_codeloc(jl_string_t *cl, size_t pc, int line_offset, int line_bytes, int to_bytes) JL_NOTSAFEPOINT
{
    const char *ptr = jl_string_data(cl) + sizeof(int32_t[3]);
    if (pc == 0)
        to_bytes = 0;
    else
        ptr += line_bytes + (pc - 1) * (line_bytes + to_bytes * 2);
    uint8_t int8;
    uint16_t int16;
    uint32_t int32;
    struct jl_codeloc_t codeloc;
    switch (line_bytes) {
    case 0:
        codeloc.line = 0;
        break;
    case 1:
        memcpy(&int8, ptr, 1);
        codeloc.line = int8;
        break;
    case 2:
        memcpy(&int16, ptr, 2);
        codeloc.line = int16;
        break;
    case 4:
        memcpy(&int32, ptr, 4);
        codeloc.line = int32;
        break;
    }
    if (codeloc.line > 0)
        codeloc.line += line_offset - 1;
    ptr += line_bytes;
    switch (to_bytes) {
    case 0:
        codeloc.to = 0;
        break;
    case 1:
        memcpy(&int8, ptr, 1);
        codeloc.to = int8;
        break;
    case 2:
        memcpy(&int16, ptr, 2);
        codeloc.to = int16;
        break;
    case 4:
        memcpy(&int32, ptr, 4);
        codeloc.to = int32;
        break;
    }
    ptr += to_bytes;
    switch (to_bytes) {
    case 0:
        codeloc.pc = 0;
        break;
    case 1:
        memcpy(&int8, ptr, 1);
        codeloc.pc = int8;
        break;
    case 2:
        memcpy(&int16, ptr, 2);
        codeloc.pc = int16;
        break;
    case 3:
        memcpy(&int32, ptr, 4);
        codeloc.pc = int32;
        break;
    }
    ptr += to_bytes;
    return codeloc;
}


static const struct jl_codeloc_t badloc = {-1, 0, 0};

JL_DLLEXPORT struct jl_codeloc_t jl_uncompress1_codeloc(jl_string_t *cl, size_t pc) JL_NOTSAFEPOINT
{
    assert(jl_is_string(cl));
    int line_offset, line_bytes, to_bytes;
    size_t nstmts = codelocs_parseheader(cl, &line_offset, &line_bytes, &to_bytes);
    if (pc > nstmts)
        return badloc;
    return unpack_codeloc(cl, pc, line_offset, line_bytes, to_bytes);
}

static int allzero(jl_value_t *codelocs) JL_NOTSAFEPOINT
{
    int32_t *p = jl_array_data(codelocs,int32_t);
    int32_t *pend = p + jl_array_nrows(codelocs);
    do {
        if (*p)
            return 0;
    } while (++p < pend);
    return 1;
}

JL_DLLEXPORT jl_string_t *jl_compress_codelocs(int32_t firstline, jl_value_t *codelocs, size_t nstmts) // firstline+Vector{Int32} => Memory{UInt8}
{
    assert(jl_typeis(codelocs, jl_array_int32_type));
    if (jl_array_nrows(codelocs) == 0)
        nstmts = 0;
    assert(nstmts * 3 == jl_array_nrows(codelocs));
    if (allzero(codelocs))
        return jl_an_empty_string;
    struct jl_codeloc_t codeloc, min, max;
    size_t i;
    min.line = min.to = min.pc = firstline <= 0 ? INT32_MAX : firstline;
    max.line = max.to = max.pc = 0;
    for (i = 0; i < nstmts; i++) {
        memcpy(&codeloc, jl_array_data(codelocs,int32_t) + 3 * i, sizeof(codeloc));
#define SETMIN(x) if (codeloc.x < min.x) min.x = codeloc.x
#define SETMAX(x) if (codeloc.x > max.x) max.x = codeloc.x
        if (codeloc.line > 0)
            SETMIN(line);
        SETMAX(line);
        SETMIN(to);
        SETMAX(to);
        SETMIN(pc);
        SETMAX(pc);
#undef SETMIN
#undef SETMAX
    }
    int32_t header[3];
    header[0] = min.line > max.line ? 0 : min.line;
    header[1] = min.line > max.line ? 0 : max.line - min.line;
    header[2] = max.to > max.pc ? max.to : max.pc;
    size_t line_bytes;
    if (header[1] < 255)
        line_bytes = 1;
    else if (header[1] < 65535)
        line_bytes = 2;
    else
        line_bytes = 4;
    size_t to_bytes;
    if (header[2] == 0)
        to_bytes = 0;
    else if (header[2] < 255)
        to_bytes = 1;
    else if (header[2] < 65535)
        to_bytes = 2;
    else
        to_bytes = 4;
    jl_string_t *cl = jl_alloc_string(sizeof(header) + line_bytes + nstmts * (line_bytes + to_bytes * 2));
    // store header structure
    memcpy(jl_string_data(cl), &header, sizeof(header));
    // pack bytes
    char *ptr = jl_string_data(cl) + sizeof(header);
    uint8_t int8;
    uint16_t int16;
    uint32_t int32;
    { // store firstline value
        int8 = int16 = int32 = firstline > 0 ? firstline - header[0] + 1 : 0;
        switch (line_bytes) {
        case 0:
            break;
        case 1:
            memcpy(ptr, &int8, 1);
            break;
        case 2:
            memcpy(ptr, &int16, 2);
            break;
        case 4:
            memcpy(ptr, &int32, 4);
            break;
        }
        ptr += line_bytes;
    }
    for (i = 0; i < nstmts; i++) {
        memcpy(&codeloc, jl_array_data(codelocs,int32_t) + 3 * i, sizeof(codeloc));
        int8 = int16 = int32 = codeloc.line > 0 ? codeloc.line - header[0] + 1 : 0;
        switch (line_bytes) {
        case 0:
            break;
        case 1:
            memcpy(ptr, &int8, 1);
            break;
        case 2:
            memcpy(ptr, &int16, 2);
            break;
        case 4:
            memcpy(ptr, &int32, 4);
            break;
        }
        ptr += line_bytes;
        int8 = int16 = int32 = codeloc.to;
        switch (to_bytes) {
        case 0:
            break;
        case 1:
            memcpy(ptr, &int8, 1);
            break;
        case 2:
            memcpy(ptr, &int16, 2);
            break;
        case 4:
            memcpy(ptr, &int32, 4);
            break;
        }
        ptr += to_bytes;
        int8 = int16 = int32 = codeloc.pc;
        switch (to_bytes) {
        case 0:
            break;
        case 1:
            memcpy(ptr, &int8, 1);
            break;
        case 2:
            memcpy(ptr, &int16, 2);
            break;
        case 4:
            memcpy(ptr, &int32, 4);
            break;
        }
        ptr += to_bytes;
    }
    return cl;
}

JL_DLLEXPORT jl_value_t *jl_uncompress_codelocs(jl_string_t *cl, size_t nstmts) // Memory{UInt8} => Vector{Int32}
{
    assert(jl_is_string(cl));
    int line_offset, line_bytes, to_bytes;
    size_t nlocs = codelocs_parseheader(cl, &line_offset, &line_bytes, &to_bytes);
    assert(nlocs == 0 || nlocs == nstmts);
    jl_value_t *codelocs = (jl_value_t*)jl_alloc_array_1d(jl_array_int32_type, nstmts * 3);
    size_t i;
    for (i = 0; i < nlocs; i++) {
        struct jl_codeloc_t codeloc = unpack_codeloc(cl, i + 1, line_offset, line_bytes, to_bytes);;
        memcpy(jl_array_data(codelocs,int32_t) + i * 3, &codeloc, sizeof(codeloc));
    }
    if (nlocs == 0) {
        memset(jl_array_data(codelocs,int32_t), 0, nstmts * sizeof(struct jl_codeloc_t));
    }
    return codelocs;
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
                     jl_memory_any_type,
                     jl_memory_uint8_type,

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
    deser_tag[TAG_ARRAY1D] = (jl_value_t*)jl_array_type;
    deser_tag[TAG_MEMORYT] = (jl_value_t*)jl_genericmemory_type;
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
