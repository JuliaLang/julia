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
#include "newobj_internal.h"

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
#define write_int8(s, n) write_uint8(s, n)
#define read_int8(s) read_uint8(s)

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

// --- serialize ---

void jl_serialize_tag_type(ios_t *s, jl_value_t *v)
{
    jl_typename_t *tn = ((jl_tag_type_t*)v)->name;
    // when we write a TypeName we save full information about the type
    // (super, field names, field types, etc.), so other times we only
    // need to save the name and parameters.
    int longform = (v == tn->primary);
    if (jl_is_struct_type(v)) {
        writetag(s, jl_struct_kind);
        jl_serialize_value(s, jl_struct_kind->name);
        jl_serialize_value(s, jl_null);
        if (longform) {
            write_uint8(s, 1);
            jl_serialize_value(s, ((jl_struct_type_t*)v)->name);
            jl_serialize_value(s, ((jl_struct_type_t*)tt)->super);
            jl_serialize_value(s, ((jl_struct_type_t*)tt)->parameters);
            jl_serialize_value(s, ((jl_struct_type_t*)tt)->names);
            jl_serialize_value(s, ((jl_struct_type_t*)tt)->types);
            write_int32(((jl_struct_type_t*)v)->uid);
        }
        else {
            write_uint8(s, 0);
            jl_serialize_value(s, ((jl_struct_type_t*)v)->name);
            jl_serialize_value(s, ((jl_struct_type_t*)v)->parameters);
            write_int32(((jl_struct_type_t*)v)->uid);
        }
    }
    else if (jl_is_bits_type(v)) {
        writetag(s, jl_struct_kind);
        jl_serialize_value(s, jl_bits_kind->name);
        jl_serialize_value(s, jl_null);
        if (longform) {
            write_uint8(s, 1);
            jl_serialize_value(s, ((jl_tag_type_t*)v)->name);
            jl_serialize_value(s, ((jl_bits_type_t*)tt)->super);
            jl_serialize_value(s, ((jl_bits_type_t*)tt)->parameters);
            write_int32(((jl_bits_type_t*)tt)->nbits);
            write_int32(((jl_bits_type_t*)tt)->uid);
        }
        else {
            write_uint8(s, 0);
            jl_serialize_value(s, ((jl_tag_type_t*)v)->name);
            jl_serialize_value(s, ((jl_tag_type_t*)v)->parameters);
            write_int32(s, ((jl_bits_type_t*)v)->uid);
        }
    }
    else if (jl_is_tag_type(v)) {
        writetag(s, jl_tag_kind);
        if (longform) {
            write_uint8(s, 1);
            jl_serialize_value(s, ((jl_tag_type_t*)v)->name);
            jl_serialize_value(s, ((jl_tag_type_t*)tt)->super);
            jl_serialize_value(s, ((jl_tag_type_t*)tt)->parameters);
        }
        else {
            write_uint8(s, 0);
            jl_serialize_value(s, ((jl_tag_type_t*)v)->name);
            jl_serialize_value(s, ((jl_tag_type_t*)v)->parameters);
        }
    }
    else {
        assert(0);
    }
}

void jl_serialize_methlist(ios_t *s, jl_methlist_t *ml)
{
    while (ml != NULL) {
        jl_serialize_value(s, ml->sig);
        write_int8(s, ml->has_tvars);
        write_int8(s, ml->va);
        jl_serialize_value(s, ml->tvars);
        jl_serialize_value(s, ml->func);
        ml = ml->next;
    }
    jl_serialize_value(s, NULL);
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
    else if (jl_is_some_tag_type(v)) {
        jl_serialize_tag_type(s, v);
    }
    else if (jl_is_typename(v)) {
        writetag(s, jl_typename_type);
        jl_serialize_value(s, ((jl_typename_t*)v)->name);
        jl_serialize_value(s, ((jl_typename_t*)v)->primary);
    }
    else if (jl_is_union_type(v)) {
        writetag(s, jl_union_kind);
        jl_serialize_value(s, ((jl_uniontype_t*)v)->types);
    }
    else if (jl_is_function(v)) {
        writetag(s, jl_func_kind);
        jl_function_t *f = (jl_function_t*)v;
        jl_serialize_value(s, (jl_value_t*)f->linfo);
        jl_serialize_value(s, f->env);
        if (f->linfo && f->ptr != &jl_trampoline) {
            write_int32(s, 0);
        }
        else {
            void **pbp = ptrhash_bp(&fptr_to_id, f->ptr);
            if (*pbp == HT_NOTFOUND)
                jl_error("unknown function pointer");
            write_int32(s, *(ptrint_t*)pbp);
        }
    }
    else if (jl_is_lambda_info(v)) {
        jl_serialize_value(s, jl_lambda_info_type);
        jl_lambda_info_t *li = (jl_lambda_info_t*)v;
        jl_serialize_value(s, li->ast);
        jl_serialize_value(s, (jl_value_t*)li->sparams);
        jl_serialize_value(s, (jl_value_t*)li->tfunc);
        jl_serialize_value(s, (jl_value_t*)li->name);
        jl_serialize_value(s, (jl_value_t*)li->specTypes);
        write_int8(s, li->inferred);
    }
    else if (jl_typeis(v, jl_methtable_type)) {
        jl_serialize_value(s, jl_methtable_type);
        jl_methtable_t *mt = (jl_methtable_t*)v;
        jl_serialize_methlist(s, mt->defs);
        jl_serialize_methlist(s, mt->cache);
        write_int32(s, mt->n_1arg);
        int i;
        for(i=0; i < mt->n_1arg; i++) {
            jl_serialize_value(s, mt->cache_1arg[i]);
        }
        write_int8(s, mt->sealed);
        write_int32(s, mt->max_args);
    }
    else if (jl_typeis(v, jl_task_type)) {
        jl_error("Task cannot be serialized");
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
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            jl_serialize_value(s, b->name);
            jl_serialize_value(s, b->value);
            jl_serialize_value(s, b->type);
            write_int8(s, b->constp);
            write_int8(s, b->exportp);
        }
    }
    jl_serialize_value(s, NULL);
    table = m->macros.table;
    for(i=1; i < m->macros.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_serialize_value(s, table[i-1]);
            jl_serialize_value(s, table[i]);
        }
    }
    jl_serialize_value(s, NULL);
}

htable_t *jl_gc_get_finalizer_table();

void jl_serialize_finalizers(ios_t *s)
{
    htable_t *finalizer_table = jl_gc_get_finalizer_table();
    int i;
    for(i=0; i < finalizer_table->size; i+=2) {
        if (finalizer_table->table[i+1] != HT_NOTFOUND) {
            jl_serialize_value(s, finalizer_table->table[i]);
            jl_serialize_value(s, finalizer_table->table[i+1]);
        }
    }
    jl_serialize_value(s, NULL);
}

// --- deserialize ---

jl_value_t *jl_deserialize_tag_type(ios_t *s)
{
    // todo
}

jl_methlist_t *jl_deserialize_methlist(ios_t *s)
{
    jl_methlist_t *ml = NULL;
    jl_methlist_t **pnext = &ml;
    while (1) {
        jl_value_t *sig = jl_deserialize_value(s);
        if (sig == NULL)
            break;
        jl_methlist_t *node = (jl_methlist_t*)allocb(sizeof(jl_methlist_t));
        node->sig = sig;
        node->has_tvars = read_int8(s);
        node->va = read_int8(s);
        node->tvars = jl_deserialize_value(s);
        node->func = jl_deserialize_value(s);
        node->next = NULL;
        *pnext = node;
        pnext = &node->next;
    }
    return ml;
}

jl_value_t *jl_deserialize_value(ios_t *s)
{
    int8_t tag = read_uint8(s);
    if (tag == Null_tag)
        return NULL;
    if (tag == 0) {
        tag = read_uint8(s);
        return (jl_value_t*)ptrhash_get(&deser_tag, tag);
    }
    if (tag == BackRef_tag) {
        ptrint_t offs = read_int32(s);
        void **bp = ptrhash_bp(&backref_table, offs);
        assert(*bp != HT_NOTFOUND);
        return (jl_value_t*)*bp;
    }

    jl_value_t *vtag = (jl_value_t*)ptrhash_get(&deser_tag, tag);
    if (tag >= VALUE_TAGS) {
        return vtag;
    }
    int pos = ios_pos(s);

    size_t i;
    if (vtag == jl_tuple_type || vtag == (jl_value_t*)LongTuple_tag) {
        size_t len;
        if (vtag == jl_tuple_type)
            len = read_uint8(s);
        else
            len = read_int32(s);
        jl_tuple_t *tu = jl_alloc_tuple_uninit(len);
        ptrhash_put(&backref_table, pos, (jl_value_t*)tu);
        for(i=0; i < len; i++)
            jl_tupleset(tu, i, jl_deserialize_value(s));
        return (jl_value_t*)tu;
    }
    else if (vtag == jl_symbol_type || vtag == (jl_value_t*)LongSymbol_tag) {
        size_t len;
        if (vtag == jl_symbol_type)
            len = read_uint8(s);
        else
            len = read_int32(s);
        char *name = alloca(len);
        ios_read(s, name, len);
        jl_value_t *s = jl_symbol_n(name, len);
        ptrhash_put(&backref_table, pos, s);
        return s;
    }
    else if (vtag == jl_array_type) {
        jl_value_t *elty = jl_deserialize_value(s);
        jl_value_t *dims = jl_deserialize_value(s);
        jl_value_t *atype =
            jl_apply_type((jl_value_t*)jl_array_type,
                          jl_tuple2(elty,
                                    jl_box_int32(((jl_tuple_t*)dims)->length)));
        jl_array_t *a = jl_new_array((jl_type_t*)atype, dims);
        ptrhash_put(&backref_table, pos, (jl_value_t*)a);
        if (jl_is_bits_type(elty)) {
            size_t tot = a->length * jl_bitstype_nbits(elty)/8;
            ios_read(s, a->data, tot);
        }
        else {
            for(i=0; i < a->length; i++) {
                ((jl_value_t**)a->data)[i] = jl_deserialize_value(s);
            }
        }
        return (jl_value_t*)a;
    }
    else if (vtag == jl_expr_type || vtag == (jl_value_t*)LongExpr_tag) {
        size_t len;
        if (vtag == jl_expr_type)
            len = read_uint8(s);
        else
            len = read_int32(s);
        jl_expr_t *e = jl_exprn(jl_deserialize_value(s), len);
        ptrhash_put(&backref_table, pos, (jl_value_t*)e);
        e->etype = jl_deserialize_value(s);
        for(i=0; i < len; i++) {
            jl_cellset(e->args, i, jl_deserialize_value(s));
        }
    }
    else if (vtag == jl_typename_type) {
        // todo
    }
    else if (vtag == jl_union_kind) {
        // todo
    }
    else if (vtag == jl_func_kind) {
        // todo
    }
    else if (vtag == jl_lambda_info_type) {
        // todo
    }
    else if (vtag == jl_methtable_type) {
        jl_methtable_t *mt = (jl_methtable_t*)allocobj(sizeof(jl_methtable_t));
        ptrhash_put(&backref_table, pos, mt);
        mt->type = (jl_type_t*)jl_methtable_type;
        mt->defs = jl_deserialize_methlist(s);
        mt->cache = jl_deserialize_methlist(s);
        mt->n_1arg = read_int32(s);
        if (mt->n_1arg > 0) {
            mt->cache_1arg = LLT_ALLOC(mt->n_1arg*sizeof(void*));
            for(i=0; i < mt->n_1arg; i++) {
                mt->cache_1arg[i] = jl_deserialize_value(s);
            }
        }
        else {
            mt->cache_1arg = NULL;
        }
        mt->sealed = read_int8(s);
        mt->max_args = read_int32(s);
        return (jl_value_t*)mt;
    }
    else if (jl_is_bits_type(vtag) || vtag == jl_bits_kind) {
        jl_bits_type_t *bt;
        if (vtag == jl_bits_kind) {
            jl_value_t *tname = jl_deserialize_value(s);
            jl_value_t *tpara = jl_deserialize_value(s);
            bt = (jl_bits_type_t*)jl_apply_type(tname->primary, tpara);
        }
        else {
            bt = (jl_bits_type_t*)vtag;
        }
        char *data = alloca(bt->nbits/8);
        ios_read(s, data, bt->nbits/8);
        jl_value_t *v;
        if (bt == jl_int8_type)
            v = jl_box_int8(*(int8_t*)data);
        else if (bt == jl_uint8_type)
            v = jl_box_uint8(*(uint8_t*)data);
        else if (bt == jl_int16_type)
            v = jl_box_int16(*(int16_t*)data);
        else if (bt == jl_uint16_type)
            v = jl_box_uint16(*(uint16_t*)data);
        else if (bt == jl_int32_type)
            v = jl_box_int32(*(int32_t*)data);
        else if (bt == jl_uint32_type)
            v = jl_box_uint32(*(uint32_t*)data);
        else if (bt == jl_int64_type)
            v = jl_box_int64(*(int64_t*)data);
        else if (bt == jl_uint64_type)
            v = jl_box_uint64(*(uint64_t*)data);
        else if (bt == jl_bool_type)
            v = jl_box_bool(*(int8_t*)data);
        else {
            switch (bt->nbits) {
            case  8: v = jl_box8 (bt, *(int8_t*) data); break;
            case 16: v = jl_box16(bt, *(int16_t*)data); break;
            case 32: v = jl_box32(bt, *(int32_t*)data); break;
            case 64: v = jl_box64(bt, *(int64_t*)data); break;
            default: assert(0);
            }
        }
        ptrhash_put(&backref_table, pos, v);
        return v;
    }
    else if (vtag == jl_struct_kind) {
        jl_typename_t *tname = (jl_typename_t*)jl_deserialize_value(s);
        jl_tuple_t *tpara = (jl_tuple_t*)jl_deserialize_value(s);
        if (tname == jl_struct_kind->name || tname == jl_bits_kind->name ||
            tname == jl_tag_kind->name)
            return jl_deserialize_tag_type(s);
        jl_struct_type_t *typ =
            (jl_struct_type_t*)jl_apply_type(tname->primary, tpara);
        size_t nf = typ->names->length;
        jl_value_t *v = newobj((jl_type_t*)typ, nf);
        ptrhash_put(&backref_table, pos, v);
        for(i=0; i < nf; i++) {
            ((jl_value_t**)v)[i+1] = jl_deserialize_value(s);
        }
        return v;
    }
    else {
        assert(0);
    }
}

void jl_deserialize_module(ios_t *s, jl_module_t *m)
{
    while (1) {
        jl_value_t *name = jl_deserialize_value(s);
        if (name == NULL)
            break;
        jl_binding_t *b = jl_get_binding(m, (jl_sym_t*)name);
        b->value = jl_deserialize_value(s);
        b->type = jl_deserialize_value(s);
        b->constp = read_int8(s);
        b->exportp = read_int8(s);
    }
    while (1) {
        jl_value_t *name = jl_deserialize_value(s);
        if (name == NULL)
            break;
        jl_set_expander(m, (jl_sym_t*)name,
                        (jl_function_t*)jl_deserialize_value(s));
    }
}

void jl_deserialize_finalizers(ios_t *s)
{
    htable_t *finalizer_table = jl_gc_get_finalizer_table();
    while (1) {
        jl_value_t *v = jl_deserialize_value(s);
        if (v == NULL)
            break;
        jl_value_t **bp = ptrhash_bp(finalizer_table, v);
        *bp = jl_deserialize_value(s);
    }
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

                     jl_typename_type, jl_type_type, jl_methtable_type,
                     jl_bottom_type, jl_tvar_type,
                     jl_seq_type, jl_ntuple_type, jl_tensor_type,
                     jl_lambda_info_type, jl_box_type,
                     jl_typector_type, jl_intrinsic_type, jl_undef_type,
                     jl_any_func,

                     jl_symbol_type->name, jl_int8_type->name, jl_uint8_type->name,
                     jl_int16_type->name, jl_uint16_type->name, jl_int32_type->name,
                     jl_uint32_type->name, jl_int64_type->name, jl_uint64_type->name,
                     jl_float32_type->name, jl_float64_type->name,
                     jl_char_type->name, jl_pointer_type->name,
                     jl_tag_kind->name, jl_union_kind->name, jl_bits_kind->name, jl_struct_kind->name,
                     jl_func_kind->name, jl_tuple_type->name, jl_array_type->name, jl_expr_type->name,
                     jl_typename_type->name, jl_type_type->name, jl_methtable_type->name,
                     jl_bottom_type->name, jl_tvar_type->name,
                     jl_seq_type->name, jl_ntuple_type->name, jl_tensor_type->name,
                     jl_lambda_info_type->name, jl_box_type->name,
                     jl_typector_type->name, jl_intrinsic_type->name, jl_undef_type->name,

                     jl_root_task,

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
