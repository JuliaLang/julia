/*
  saving and restoring system images
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
#include "builtin_proto.h"

static htable_t ser_tag;
static htable_t deser_tag;
static htable_t backref_table;
static htable_t fptr_to_id;
static htable_t id_to_fptr;

static const int LongSymbol_tag = 23;
static const int LongTuple_tag  = 24;
static const int LongExpr_tag   = 25;
static const int Null_tag       = 254;
static const int BackRef_tag    = 255;

static int VALUE_TAGS;

#define write_uint8(s, n) ios_putc((n), (s))
#define read_uint8(s) ((uint8_t)ios_getc(s)

static void write_int16(ios_t *s, int16_t i)
{
    write_uint8(i       & 0xff);
    write_uint8((i>> 8) & 0xff);
}

static int16_t read_int16(ios_t *s)
{
    int b0 = read_uint8(s);
    int b1 = read_uint8(s);
    return (int16_t)(b0 | (b1<<8));
}

static void write_int32(ios_t *s, int32_t i)
{
    write_uint8(i       & 0xff);
    write_uint8((i>> 8) & 0xff);
    write_uint8((i>>16) & 0xff);
    write_uint8((i>>24) & 0xff);
}

static int32_t read_int32(ios_t *s)
{
    int b0 = read_uint8(s);
    int b1 = read_uint8(s);
    int b2 = read_uint8(s);
    int b3 = read_uint8(s);
    return b0 | (b1<<8) | (b2<<16) | (b3<<24);
}

static void write_int64(ios_t *s, int64_t i)
{
    write_uint8(i       & 0xff);
    write_uint8((i>> 8) & 0xff);
    write_uint8((i>>16) & 0xff);
    write_uint8((i>>24) & 0xff);
    write_uint8((i>>32) & 0xff);
    write_uint8((i>>40) & 0xff);
    write_uint8((i>>48) & 0xff);
    write_uint8((i>>56) & 0xff);
}

static int64_t read_int64(ios_t *s)
{
    int64_t b0 = read_uint8(s);
    int64_t b1 = read_uint8(s);
    int64_t b2 = read_uint8(s);
    int64_t b3 = read_uint8(s);
    int64_t b4 = read_uint8(s);
    int64_t b5 = read_uint8(s);
    int64_t b6 = read_uint8(s);
    int64_t b7 = read_uint8(s);
    return b0 | (b1<<8) | (b2<<16) | (b3<<24) | (b4<<32) | (b5<<40) |
        (b6<<48) | (b7<<56);
}

static void writetag(ios_t *s, jl_value_t *v)
{
    write_uint8(s, (uint8_t)ptrhash_get(&ser_tag, v));
}

static void write_as_tag(ios_t *s, uint8_t tag)
{
    if (tag < VALUE_TAGS) {
        write_uint8(s, 0);
    }
    write_uint8(s, tag);
}

void jl_serialize_value(ios_t *s, jl_value_t *v)
{
    if (v == NULL) {
        write_uint8(s, Null_tag);
        return;
    }

    void **bp = ptrhash_bp(&ser_tag, v);
    if (*bp != HT_NOTFOUND) {
        write_as_tag(s, (uint8_t)*bp);
        return;
    }

    bp = ptrhash_bp(&backref_table, v);
    if (*bp != HT_NOTFOUND) {
        write_uint8(s, BackRef_tag);
        write_int32(s, (int)*bp);
        return;
    }
    ptrhash_put(&backref_table, v, ios_pos(s));

    size_t i;
    if (jl_is_tuple(v)) {
        size_t l = ((jl_tuple_t*)v)->length;
        if (l <= 255) {
            writetag(s, jl_tuple_type);
            write_uint8(s, (uint8_t)l);
        }
        else {
            writetag(s, (jl_value_t*)LongTuple_tag);
            write_int32(s, l);
        }
        for(i=0; i < l; i++) {
            jl_serialize_value(s, jl_tupleref(v, i));
        }
    }
    else if (jl_is_symbol(v)) {
        size_t l = strlen(((jl_sym_t*)v)->name);
        if (l <= 255) {
            writetag(s, jl_symbol_type);
            write_uint8(s, (uint8_t)l);
        }
        else {
            writetag(s, (jl_value_t*)LongSymbol_tag);
            write_int32(s, l);
        }
        ios_write(s, ((jl_sym_t*)v)->name, l);
    }
    else if (jl_is_array(v)) {
        writetag(s, (jl_value_t*)jl_array_type);
        jl_value_t *elty = jl_tparam0(jl_typeof(v));
        jl_serialize_value(s, elty);
        jl_serialize_value(s, ((jl_array_t*)v)->dims);
        if (jl_is_bits_type(elty)) {
            size_t tot = ((jl_array_t*)v)->length * jl_bitstype_nbits(elty)/8;
            ios_write(s, ((jl_array_t*)v)->data, tot);
        }
        else {
            for(i=0; i < ((jl_array_t*)v)->length; i++) {
                jl_serialize_value(s, jl_cellref(v, i));
            }
        }
    }
    else if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        size_t l = e->args->length;
        if (l <= 255) {
            writetag(s, jl_expr_type);
            write_uint8(s, (uint8_t)l);
        }
        else {
            writetag(s, (jl_value_t*)LongExpr_tag);
            write_int32(s, l);
        }
        jl_serialize_value(s, e->head);
        jl_serialize_value(s, e->etype);
        for(i=0; i < l; i++) {
            jl_serialize_value(s, jl_exprarg(e, i));
        }
    }
    else if (jl_is_struct_type(v)) {
        writetag(s, jl_struct_kind);
        jl_serialize_value(s, jl_struct_kind->name);
        jl_serialize_value(s, jl_null);
        jl_serialize_value(s, ((jl_tag_type_t*)v)->name);
        jl_serialize_value(s, ((jl_tag_type_t*)v)->parameters);
        write_int32(((jl_struct_type_t*)v)->uid);
        jl_serialize_value(s, ((jl_struct_type_t*)v)->instance);
    }
    else if (jl_is_bits_type(v)) {
        writetag(s, jl_struct_kind);
        jl_serialize_value(s, jl_bits_kind->name);
        jl_serialize_value(s, jl_null);
        jl_serialize_value(s, ((jl_tag_type_t*)v)->name);
        jl_serialize_value(s, ((jl_tag_type_t*)v)->parameters);
        write_int32(s, ((jl_bits_type_t*)v)->uid);
    }
    else if (jl_is_tag_type(v)) {
        writetag(s, jl_tag_kind);
        jl_serialize_value(s, ((jl_tag_type_t*)v)->name);
        jl_serialize_value(s, ((jl_tag_type_t*)v)->parameters);
    }
    else if (jl_is_union_type(v)) {
        writetag(s, jl_union_kind);
        jl_serialize_value(s, ((jl_uniontype_t*)v)->types);
    }
    else if (jl_is_function(v)) {
        writetag(s, jl_func_kind);
        jl_function_t *f = (jl_function_t*)v;
    }
    else if (jl_is_lambda_info(v)) {
    }
    else if (jl_is_typename(v)) {
        
    }
    else if (jl_typeis(v, jl_methtable_type)) {
    }
    else if (jl_typeis(v, jl_task_type)) {
    }
    else {
        jl_value_t *t = (jl_value_t*)jl_typeof(v);
        if (jl_is_bits_type(t)) {
            int nb = ((jl_bits_type_t*)t)->nbits;
            void *data = jl_bits_data(v);
            bp = ptrhash_bp(&ser_tag, t);
            if (*bp != HT_NOTFOUND) {
                write_uint8(s, (uint8_t)*bp);
            }
            else {
                writetag(s, jl_bits_kind);
                jl_serialize_value(s, ((jl_bits_type_t*)t)->name);
                jl_serialize_value(s, ((jl_bits_type_t*)t)->parameters);
            }
            ios_write(s, data, nb/8);
        }
        else if (jl_is_struct_type(t)) {
            writetag(s, jl_struct_kind);
            jl_serialize_value(s, ((jl_struct_type_t*)t)->name);
            jl_serialize_value(s, ((jl_struct_type_t*)t)->parameters);
            size_t nf = ((jl_struct_type_t*)t)->names->length;
            size_t i;
            for(i=0; i < nf; i++) {
                jl_value_t *fld = ((jl_value_t**)v)[i+1];
                jl_serialize_value(s, fld);
            }
        }
    }
}

void jl_serialize_module(ios_t *s, jl_module_t *m)
{
}

void jl_serialize_methlist(ios_t *s, jl_methlist_t *ml)
{
}

void jl_serialize_type_cache(ios_t *s, void *tc)
{
}

void jl_serialize_finalizers(ios_t *s, htable_t *f)
{
}

void jl_init_serializer()
{
    htable_new(&ser_tag, 0);
    htable_new(&deser_tag, 0);
    htable_new(&fptr_to_id, 0);
    htable_new(&id_to_fptr, 0);
    htable_new(&backref_table, 100000);

    void *tags[] = { jl_symbol_type, jl_int8_type, jl_uint8_type,
                     jl_int16_type, jl_uint16_type, jl_int32_type,
                     jl_uint32_type, jl_int64_type, jl_uint64_type,
                     jl_float32_type, jl_float64_type,
                     jl_char_type, jl_pointer_type,
                     jl_tag_kind, jl_union_kind, jl_bits_kind, jl_struct_kind,
                     jl_func_kind, jl_tuple_type, jl_array_type, jl_expr_type,
                     (void*)LongSymbol_tag, (void*)LongTuple_tag,
                     (void*)LongExpr_tag,

                     jl_null, jl_bool_type, jl_any_type, jl_symbol("Any"),
                     jl_symbol("Array"), jl_symbol("TypeVar"),
                     jl_symbol("FuncKind"), jl_symbol("Box").
                     lambda_sym, vinf_sym, locals_sym, body_sym, return_sym,
                     call_sym, colons_sym, null_sym, goto_sym, goto_ifnot_sym,
                     label_sym, symbol_sym, jl_symbol("string"),
                     jl_symbol("T"), jl_symbol("S"),
                     jl_symbol("a"), jl_symbol("b"), jl_symbol("c"),
                     jl_symbol("d"), jl_symbol("e"), jl_symbol("f"),
                     jl_symbol("g"), jl_symbol("h"), jl_symbol("i"),
                     jl_symbol("j"), jl_symbol("k"), jl_symbol("l"),
                     jl_symbol("m"), jl_symbol("n"), jl_symbol("o"),
                     jl_symbol("p"), jl_symbol("q"), jl_symbol("r"),
                     jl_symbol("s"), jl_symbol("t"), jl_symbol("u"),
                     jl_symbol("v"), jl_symbol("w"), jl_symbol("x"),
                     jl_symbol("y"), jl_symbol("z"),
                     jl_false, jl_true,
                     jl_box_int32(0), jl_box_int32(1), jl_box_int32(2),
                     jl_box_int32(3), jl_box_int32(4),
                     NULL };
    int i=2;
    while (tags[i-2] != NULL) {
        ptrhash_put(&ser_tag, tags[i-2], (void*)i);
        ptrhash_put(&deser_tag, (void*)i, tags[i-2]);
        i += 1;
    }
    VALUE_TAGS = (int)ptrhash_get(&ser_tag, jl_null);

    void *fptrs[] = { jl_new_struct_internal, 
                      jl_generic_ctor, 
                      jl_constructor_factory_trampoline, 
                      jl_weakref_ctor, 
                      jl_new_array_internal, 
                      jl_generic_array_ctor, 
                      jl_f_throw, 
                      jl_f_is, 
                      jl_f_no_function, 
                      jl_f_typeof, 
                      jl_f_subtype, 
                      jl_f_isa, 
                      jl_f_typeassert, 
                      jl_f_apply, 
                      jl_f_top_eval, 
                      jl_f_isbound, 
                      jl_f_tuple, 
                      jl_f_tupleref, 
                      jl_f_tuplelen, 
                      jl_f_get_field, 
                      jl_f_set_field, 
                      jl_f_arraylen, 
                      jl_f_arrayref, 
                      jl_f_arrayset, 
                      jl_f_instantiate_type, 
                      jl_f_convert, 
                      jl_f_convert_to_ptr, 
                      jl_f_print_array_uint8, 
                      jl_f_show_bool, 
                      jl_f_show_char, 
                      jl_f_show_float32, 
                      jl_f_show_float64, 
                      jl_f_show_int8, 
                      jl_f_show_uint8, 
                      jl_f_show_int16, 
                      jl_f_show_uint16, 
                      jl_f_show_int32, 
                      jl_f_show_uint32, 
                      jl_f_show_int64, 
                      jl_f_show_uint64, 
                      jl_f_show_pointer, 
                      jl_f_show_typevar, 
                      jl_f_show_linfo, 
                      jl_f_show_any, 
                      jl_f_print_symbol, 
                      jl_trampoline, 
                      jl_f_new_struct_type, 
                      jl_f_new_struct_fields, 
                      jl_f_new_type_constructor, 
                      jl_f_new_tag_type, 
                      jl_f_new_tag_type_super, 
                      jl_f_new_bits_type, 
                      jl_f_typevar, 
                      jl_f_union, 
                      jl_f_methodexists, 
                      jl_f_applicable, 
                      jl_f_invoke, 
                      jl_apply_generic, 
                      jl_unprotect_stack, 
                      jl_f_task, 
                      jl_f_yieldto, 
                      jl_f_current_task, 
                      jl_f_taskdone,
                      NULL };
    i=2;
    while (fptrs[i-2] != NULL) {
        ptrhash_put(&fptr_to_id, fptrs[i-2], (void*)i);
        ptrhash_put(&id_to_fptr, (void*)i, fptrs[i-2]);
        i += 1;
    }
}
