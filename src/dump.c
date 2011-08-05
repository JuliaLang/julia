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
#include "julia.h"
#include "builtin_proto.h"
#include "newobj_internal.h"
#include "jltypes_internal.h"

static htable_t ser_tag;
static htable_t deser_tag;
static htable_t backref_table;
static htable_t fptr_to_id;
static htable_t id_to_fptr;

static const ptrint_t LongSymbol_tag = 23;
static const ptrint_t LongTuple_tag  = 24;
static const ptrint_t LongExpr_tag   = 25;
static const ptrint_t Null_tag       = 254;
static const ptrint_t BackRef_tag    = 255;

static ptrint_t VALUE_TAGS;

#define write_uint8(s, n) ios_putc((n), (s))
#define read_uint8(s) ((uint8_t)ios_getc(s))
#define write_int8(s, n) write_uint8(s, n)
#define read_int8(s) read_uint8(s)

static void write_int16(ios_t *s, int16_t i)
{
    write_uint8(s, i       & 0xff);
    write_uint8(s, (i>> 8) & 0xff);
}

static int16_t read_int16(ios_t *s)
{
    int b0 = read_uint8(s);
    int b1 = read_uint8(s);
    return (int16_t)(b0 | (b1<<8));
}

static void write_int32(ios_t *s, int32_t i)
{
    write_uint8(s, i       & 0xff);
    write_uint8(s, (i>> 8) & 0xff);
    write_uint8(s, (i>>16) & 0xff);
    write_uint8(s, (i>>24) & 0xff);
}

static int32_t read_int32(ios_t *s)
{
    int b0 = read_uint8(s);
    int b1 = read_uint8(s);
    int b2 = read_uint8(s);
    int b3 = read_uint8(s);
    return b0 | (b1<<8) | (b2<<16) | (b3<<24);
}

/*
static void write_int64(ios_t *s, int64_t i)
{
    write_uint8(s, i       & 0xff);
    write_uint8(s, (i>> 8) & 0xff);
    write_uint8(s, (i>>16) & 0xff);
    write_uint8(s, (i>>24) & 0xff);
    write_uint8(s, (i>>32) & 0xff);
    write_uint8(s, (i>>40) & 0xff);
    write_uint8(s, (i>>48) & 0xff);
    write_uint8(s, (i>>56) & 0xff);
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
*/

static void writetag(ios_t *s, void *v)
{
    write_uint8(s, (uint8_t)(ptrint_t)ptrhash_get(&ser_tag, v));
}

static void write_as_tag(ios_t *s, uint8_t tag)
{
    if (tag < VALUE_TAGS) {
        write_uint8(s, 0);
    }
    write_uint8(s, tag);
}

// --- serialize ---

#define jl_serialize_value(s, v) jl_serialize_value_(s,(jl_value_t*)(v))

void jl_serialize_value_(ios_t *s, jl_value_t *v);

void jl_serialize_fptr(ios_t *s, void *fptr)
{
    void **pbp = ptrhash_bp(&fptr_to_id, fptr);
    if (*pbp == HT_NOTFOUND)
        jl_error("unknown function pointer");
    write_int32(s, *(ptrint_t*)pbp);
}

void jl_serialize_tag_type(ios_t *s, jl_value_t *v)
{
    if (jl_is_struct_type(v)) {
        writetag(s, (jl_value_t*)jl_struct_kind);
        jl_serialize_value(s, jl_struct_kind);
        jl_serialize_value(s, ((jl_struct_type_t*)v)->name);
        jl_serialize_value(s, ((jl_struct_type_t*)v)->parameters);
        jl_serialize_value(s, ((jl_struct_type_t*)v)->super);
        jl_serialize_value(s, ((jl_struct_type_t*)v)->names);
        jl_serialize_value(s, ((jl_struct_type_t*)v)->types);
        jl_serialize_value(s, ((jl_struct_type_t*)v)->ctor_factory);
        jl_serialize_value(s, ((jl_struct_type_t*)v)->env);
        jl_serialize_value(s, ((jl_struct_type_t*)v)->linfo);
        jl_serialize_fptr(s, ((jl_struct_type_t*)v)->fptr);
        write_int32(s, ((jl_struct_type_t*)v)->uid);
    }
    else if (jl_is_bits_type(v)) {
        writetag(s, jl_struct_kind);
        jl_serialize_value(s, jl_bits_kind);
        if (v == (jl_value_t*)jl_int32_type)
            write_uint8(s, 2);
        else if (v == (jl_value_t*)jl_bool_type)
            write_uint8(s, 3);
        else if (v == (jl_value_t*)jl_int64_type)
            write_uint8(s, 4);
        else
            write_uint8(s, 0);
        jl_serialize_value(s, ((jl_tag_type_t*)v)->name);
        jl_serialize_value(s, ((jl_bits_type_t*)v)->parameters);
        write_int32(s, ((jl_bits_type_t*)v)->nbits);
        jl_serialize_value(s, ((jl_bits_type_t*)v)->super);
        write_int32(s, ((jl_bits_type_t*)v)->uid);
    }
    else {
        assert(jl_is_tag_type(v));
        writetag(s, jl_tag_kind);
        jl_serialize_value(s, ((jl_tag_type_t*)v)->name);
        jl_serialize_value(s, ((jl_tag_type_t*)v)->parameters);
        jl_serialize_value(s, ((jl_tag_type_t*)v)->super);
    }
}

void jl_serialize_methlist(ios_t *s, jl_methlist_t *ml)
{
    while (ml != NULL) {
        jl_serialize_value(s, ml->sig);
        assert(jl_is_tuple(ml->sig));
        write_int8(s, ml->has_tvars);
        write_int8(s, ml->va);
        jl_serialize_value(s, ml->tvars);
        jl_serialize_value(s, ml->func);
        jl_serialize_value(s, ml->invokes);
        ml = ml->next;
    }
    jl_serialize_value(s, NULL);
}

void jl_serialize_typecache(ios_t *s, jl_typename_t *tn)
{
    typekey_stack_t *tc = (typekey_stack_t*)tn->cache;
    while (tc != NULL) {
        jl_serialize_value(s, tc->type);
        write_int32(s, tc->n);
        int i;
        for(i=0; i < tc->n; i++) {
            jl_serialize_value(s, tc->key[i]);
        }
        tc = tc->next;
    }
    jl_serialize_value(s, NULL);
}

void jl_serialize_value_(ios_t *s, jl_value_t *v)
{
    if (v == NULL) {
        write_uint8(s, Null_tag);
        return;
    }

    void **bp = ptrhash_bp(&ser_tag, v);
    if (*bp != HT_NOTFOUND) {
        write_as_tag(s, (uint8_t)(ptrint_t)*bp);
        return;
    }

    bp = ptrhash_bp(&backref_table, v);
    if (*bp != HT_NOTFOUND) {
        write_uint8(s, BackRef_tag);
        write_int32(s, (ptrint_t)*bp);
        return;
    }
    ptrhash_put(&backref_table, v, (void*)(ptrint_t)ios_pos(s));

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
        jl_array_t *ar = (jl_array_t*)v;
        writetag(s, (jl_value_t*)jl_array_type);
        jl_value_t *elty = jl_tparam0(jl_typeof(v));
        jl_serialize_value(s, elty);
        write_int16(s, ar->ndims);
        for (i=0; i < ar->ndims; i++)
            jl_serialize_value(s, jl_box_long(jl_array_dim(ar,i)));
        if (jl_is_bits_type(elty)) {
            size_t tot = ar->length * ar->elsize;
            ios_write(s, ar->data, tot);
        }
        else {
            for(i=0; i < ar->length; i++) {
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
        jl_serialize_typecache(s, (jl_typename_t*)v);
    }
    else if (jl_is_typevar(v)) {
        writetag(s, jl_tvar_type);
        jl_serialize_value(s, ((jl_tvar_t*)v)->name);
        jl_serialize_value(s, ((jl_tvar_t*)v)->lb);
        jl_serialize_value(s, ((jl_tvar_t*)v)->ub);
        write_int8(s, ((jl_tvar_t*)v)->bound);
    }
    else if (jl_is_function(v)) {
        writetag(s, jl_func_kind);
        jl_serialize_value(s, v->type);
        jl_function_t *f = (jl_function_t*)v;
        jl_serialize_value(s, (jl_value_t*)f->linfo);
        jl_serialize_value(s, f->env);
        if (f->linfo && f->linfo->ast && jl_is_expr(f->linfo->ast) &&
            f->fptr != &jl_trampoline) {
            write_int32(s, 0);
        }
        else {
            jl_serialize_fptr(s, f->fptr);
        }
    }
    else if (jl_is_lambda_info(v)) {
        writetag(s, jl_lambda_info_type);
        jl_lambda_info_t *li = (jl_lambda_info_t*)v;
        jl_serialize_value(s, li->ast);
        jl_serialize_value(s, (jl_value_t*)li->sparams);
        jl_serialize_value(s, (jl_value_t*)li->tfunc);
        jl_serialize_value(s, (jl_value_t*)li->name);
        jl_serialize_value(s, (jl_value_t*)li->specTypes);
        jl_serialize_value(s, (jl_value_t*)li->specializations);
        write_int8(s, li->inferred);
    }
    else if (jl_typeis(v, jl_methtable_type)) {
        writetag(s, jl_methtable_type);
        jl_methtable_t *mt = (jl_methtable_t*)v;
        jl_serialize_methlist(s, mt->defs);
        jl_serialize_methlist(s, mt->cache);
        jl_serialize_value(s, mt->cache_1arg);
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
            writetag(s, jl_bits_kind);
            jl_serialize_value(s, t);
            ios_write(s, data, nb/8);
        }
        else if (jl_is_struct_type(t)) {
            writetag(s, jl_struct_kind);
            jl_serialize_value(s, t);
            size_t nf = ((jl_struct_type_t*)t)->names->length;
            size_t i;
            for(i=0; i < nf; i++) {
                jl_value_t *fld = ((jl_value_t**)v)[i+1];
                jl_serialize_value(s, fld);
            }
        }
        else {
            assert(0);
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

jl_value_t *jl_deserialize_value(ios_t *s);

jl_fptr_t jl_deserialize_fptr(ios_t *s)
{
    int fptr = read_int32(s);
    if (fptr == 0)
        return NULL;
    void **pbp = ptrhash_bp(&id_to_fptr, (void*)(ptrint_t)fptr);
    if (*pbp == HT_NOTFOUND)
        jl_error("unknown function pointer ID");
    return *(jl_fptr_t*)pbp;
}

jl_value_t *jl_deserialize_tag_type(ios_t *s, jl_struct_type_t *kind, int pos)
{
    if (kind == jl_struct_kind) {
        jl_struct_type_t *st =
            (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind,
                                      STRUCT_TYPE_NW);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, st);
        st->name = (jl_typename_t*)jl_deserialize_value(s);
        st->parameters = (jl_tuple_t*)jl_deserialize_value(s);
        st->super = (jl_tag_type_t*)jl_deserialize_value(s);
        st->names = (jl_tuple_t*)jl_deserialize_value(s);
        st->types = (jl_tuple_t*)jl_deserialize_value(s);
        st->ctor_factory = jl_deserialize_value(s);
        st->env = jl_deserialize_value(s);
        st->linfo = (jl_lambda_info_t*)jl_deserialize_value(s);
        st->fptr = jl_deserialize_fptr(s);
        st->instance = NULL;
        st->uid = read_int32(s);
        return (jl_value_t*)st;
    }
    else if (kind == jl_bits_kind) {
        int form = read_uint8(s);
        jl_bits_type_t *bt;
        if (form == 2)
            bt = jl_int32_type;
        else if (form == 3)
            bt = jl_bool_type;
        else if (form == 4)
            bt = jl_int64_type;
        else
            bt = (jl_bits_type_t*)newobj((jl_type_t*)jl_bits_kind,
                                         BITS_TYPE_NW);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, bt);
        bt->name = (jl_typename_t*)jl_deserialize_value(s);
        bt->parameters = (jl_tuple_t*)jl_deserialize_value(s);

        size_t nbits = read_int32(s);
        bt->nbits = nbits;
        bt->bnbits = jl_box_int32(nbits);
        bt->fptr = NULL;
        bt->env = NULL;
        bt->linfo = NULL;
        bt->super = (jl_tag_type_t*)jl_deserialize_value(s);
        bt->uid = read_int32(s);
        return (jl_value_t*)bt;
    }
    else {
        assert(kind == jl_tag_kind);
        jl_tag_type_t *tt =
            (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAG_TYPE_NW);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, tt);
        tt->name = (jl_typename_t*)jl_deserialize_value(s);
        tt->parameters = (jl_tuple_t*)jl_deserialize_value(s);
        tt->super = (jl_tag_type_t*)jl_deserialize_value(s);
        tt->fptr = NULL;
        tt->env = NULL;
        tt->linfo = NULL;
        return (jl_value_t*)tt;
    }
    assert(0);
    return NULL;
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
        node->sig = (jl_tuple_t*)sig;
        assert(jl_is_tuple(sig));
        node->has_tvars = read_int8(s);
        node->va = read_int8(s);
        node->tvars = (jl_tuple_t*)jl_deserialize_value(s);
        node->func = (jl_function_t*)jl_deserialize_value(s);
        node->invokes = (jl_methtable_t*)jl_deserialize_value(s);
        node->next = NULL;
        *pnext = node;
        pnext = &node->next;
    }
    return ml;
}

typekey_stack_t *jl_deserialize_typecache(ios_t *s)
{
    typekey_stack_t *tk = NULL;
    typekey_stack_t **pnext = &tk;

    while (1) {
        jl_value_t *type = jl_deserialize_value(s);
        if (type == NULL)
            break;
        typekey_stack_t *tc = (typekey_stack_t*)allocb(sizeof(typekey_stack_t));
        tc->type = (jl_type_t*)type;
        tc->tn = ((jl_tag_type_t*)type)->name;
        int n = read_int32(s);
        tc->n = n;
        tc->key = (jl_value_t**)allocb(n * sizeof(void*));
        int i;
        for(i=0; i < n; i++) {
            tc->key[i] = jl_deserialize_value(s);
        }
        tc->next = NULL;
        *pnext = tc;
        pnext = &tc->next;
    }
    return tk;
}

jl_struct_type_t *jl_idtable_type=NULL;
void jl_idtable_rehash(jl_array_t **pa, size_t newsz);

jl_value_t *jl_deserialize_value(ios_t *s)
{
    int pos = ios_pos(s);
    int32_t tag = read_uint8(s);
    if (tag == Null_tag)
        return NULL;
    if (tag == 0) {
        tag = read_uint8(s);
        return (jl_value_t*)ptrhash_get(&deser_tag, (void*)(ptrint_t)tag);
    }
    if (tag == BackRef_tag) {
        ptrint_t offs = read_int32(s);
        void **bp = ptrhash_bp(&backref_table, (void*)(ptrint_t)offs);
        assert(*bp != HT_NOTFOUND);
        return (jl_value_t*)*bp;
    }

    jl_value_t *vtag=(jl_value_t*)ptrhash_get(&deser_tag,(void*)(ptrint_t)tag);
    if (tag >= VALUE_TAGS) {
        return vtag;
    }

    size_t i;
    if (vtag == (jl_value_t*)jl_tuple_type ||
        vtag == (jl_value_t*)LongTuple_tag) {
        size_t len;
        if (vtag == (jl_value_t*)jl_tuple_type)
            len = read_uint8(s);
        else
            len = read_int32(s);
        jl_tuple_t *tu = jl_alloc_tuple_uninit(len);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, (jl_value_t*)tu);
        for(i=0; i < len; i++)
            jl_tupleset(tu, i, jl_deserialize_value(s));
        return (jl_value_t*)tu;
    }
    else if (vtag == (jl_value_t*)jl_symbol_type ||
             vtag == (jl_value_t*)LongSymbol_tag) {
        size_t len;
        if (vtag == (jl_value_t*)jl_symbol_type)
            len = read_uint8(s);
        else
            len = read_int32(s);
        char *name = alloca(len);
        ios_read(s, name, len);
        jl_value_t *s = (jl_value_t*)jl_symbol_n(name, len);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, s);
        return s;
    }
    else if (vtag == (jl_value_t*)jl_array_type) {
        jl_value_t *elty = jl_deserialize_value(s);
        int16_t ndims = read_int16(s);
        size_t *dims = alloca(ndims*sizeof(size_t));
        for(i=0; i < ndims; i++)
            dims[i] = jl_unbox_long(jl_deserialize_value(s));
        jl_value_t *atype =
            jl_apply_type((jl_value_t*)jl_array_type,
                          jl_tuple2(elty, jl_box_long(ndims)));
        jl_array_t *a = jl_new_array_((jl_type_t*)atype, ndims, dims);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, (jl_value_t*)a);
        if (jl_is_bits_type(elty)) {
            size_t tot = a->length * a->elsize;
            ios_read(s, a->data, tot);
        }
        else {
            for(i=0; i < a->length; i++) {
                ((jl_value_t**)a->data)[i] = jl_deserialize_value(s);
            }
        }
        return (jl_value_t*)a;
    }
    else if (vtag == (jl_value_t*)jl_expr_type ||
             vtag == (jl_value_t*)LongExpr_tag) {
        size_t len;
        if (vtag == (jl_value_t*)jl_expr_type)
            len = read_uint8(s);
        else
            len = read_int32(s);
        jl_expr_t *e = jl_exprn((jl_sym_t*)jl_deserialize_value(s), len);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, (jl_value_t*)e);
        e->etype = jl_deserialize_value(s);
        for(i=0; i < len; i++) {
            jl_cellset(e->args, i, jl_deserialize_value(s));
        }
        return (jl_value_t*)e;
    }
    else if (vtag == (jl_value_t*)jl_typename_type) {
        jl_sym_t *name = (jl_sym_t*)jl_deserialize_value(s);
        jl_typename_t *tn = jl_new_typename(name);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, tn);
        tn->primary = jl_deserialize_value(s);
        tn->cache = jl_deserialize_typecache(s);
        return (jl_value_t*)tn;
    }
    else if (vtag == (jl_value_t*)jl_tvar_type) {
        jl_tvar_t *tv = (jl_tvar_t*)newobj((jl_type_t*)jl_tvar_type, 4);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, tv);
        tv->name = (jl_sym_t*)jl_deserialize_value(s);
        tv->lb = jl_deserialize_value(s);
        tv->ub = jl_deserialize_value(s);
        tv->bound = read_int8(s);
        return (jl_value_t*)tv;
    }
    else if (vtag == (jl_value_t*)jl_func_kind) {
        jl_value_t *ftype = jl_deserialize_value(s);
        jl_function_t *f = (jl_function_t*)newobj((jl_type_t*)ftype, 3);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, f);
        f->linfo = (jl_lambda_info_t*)jl_deserialize_value(s);
        f->env = jl_deserialize_value(s);
        f->fptr = jl_deserialize_fptr(s);
        if (f->fptr == NULL) {
            f->fptr = &jl_trampoline;
            f->env = (jl_value_t*)jl_tuple2((jl_value_t*)f, f->env);
        }
        return (jl_value_t*)f;
    }
    else if (vtag == (jl_value_t*)jl_lambda_info_type) {
        jl_lambda_info_t *li =
            (jl_lambda_info_t*)newobj((jl_type_t*)jl_lambda_info_type, 13);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, li);
        li->ast = jl_deserialize_value(s);
        li->sparams = (jl_tuple_t*)jl_deserialize_value(s);
        li->tfunc = jl_deserialize_value(s);
        li->name = (jl_sym_t*)jl_deserialize_value(s);
        li->specTypes = jl_deserialize_value(s);
        li->specializations = (jl_tuple_t*)jl_deserialize_value(s);
        li->inferred = read_int8(s);

        li->fptr = NULL;
        li->roots = jl_null;
        li->functionObject = NULL;
        li->inInference = 0;
        li->inCompile = 0;
        li->unspecialized = NULL;
        return (jl_value_t*)li;
    }
    else if (vtag == (jl_value_t*)jl_methtable_type) {
        jl_methtable_t *mt = (jl_methtable_t*)allocobj(sizeof(jl_methtable_t));
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, mt);
        mt->type = (jl_type_t*)jl_methtable_type;
        mt->defs = jl_deserialize_methlist(s);
        mt->cache = jl_deserialize_methlist(s);
        mt->cache_1arg = (jl_array_t*)jl_deserialize_value(s);
        mt->sealed = read_int8(s);
        mt->max_args = read_int32(s);
        return (jl_value_t*)mt;
    }
    else if (vtag == (jl_value_t*)jl_bits_kind) {
        jl_bits_type_t *bt = (jl_bits_type_t*)jl_deserialize_value(s);
        int nby = bt->nbits/8;
        char *data = alloca(nby);
        ios_read(s, data, nby);
        jl_value_t *v=NULL;
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
            default:
                v = (jl_value_t*)allocobj(sizeof(void*)+nby);
                v->type = (jl_type_t*)bt;
                memcpy(jl_bits_data(v), data, nby);
            }
        }
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, v);
        return v;
    }
    else if (vtag == (jl_value_t*)jl_struct_kind) {
        jl_struct_type_t *typ = (jl_struct_type_t*)jl_deserialize_value(s);
        if (typ == jl_struct_kind || typ == jl_bits_kind)
            return jl_deserialize_tag_type(s, typ, pos);
        size_t nf = typ->names->length;
        jl_value_t *v;
        if (nf == 0 && typ->instance)
            v = typ->instance;
        else
            v = newobj((jl_type_t*)typ, nf);
        ptrhash_put(&backref_table, (void*)(ptrint_t)pos, v);
        for(i=0; i < nf; i++) {
            ((jl_value_t**)v)[i+1] = jl_deserialize_value(s);
        }
        if (nf == 0 && typ->instance==NULL)
            typ->instance = v;
        if (typ == jl_idtable_type) {
            jl_idtable_rehash(&((jl_array_t**)v)[1],
                              ((jl_array_t**)v)[1]->length);
        }
        // TODO: put WeakRefs on the weak_refs list
        return v;
    }
    else if (vtag == (jl_value_t*)jl_tag_kind) {
        return jl_deserialize_tag_type(s, jl_tag_kind, pos);
    }
    assert(0);
    return NULL;
}

void jl_deserialize_module(ios_t *s, jl_module_t *m)
{
    while (1) {
        jl_value_t *name = jl_deserialize_value(s);
        if (name == NULL)
            break;
        jl_binding_t *b = jl_get_binding(m, (jl_sym_t*)name);
        b->value = jl_deserialize_value(s);
        b->type = (jl_type_t*)jl_deserialize_value(s);
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
        void **bp = ptrhash_bp(finalizer_table, v);
        *bp = jl_deserialize_value(s);
    }
}

// --- entry points ---

DLLEXPORT
void jl_save_system_image(char *fname, char *startscriptname)
{
    jl_gc_collect();
    jl_gc_collect();
    int en = jl_gc_is_enabled();
    jl_gc_disable();
    ios_t f;
    ios_file(&f, fname, 1, 1, 1, 1);

    // values needed by C RTS code
    jl_serialize_value(&f, jl_char_type);
    jl_serialize_value(&f, jl_int8_type);
    jl_serialize_value(&f, jl_uint8_type);
    jl_serialize_value(&f, jl_int16_type);
    jl_serialize_value(&f, jl_uint16_type);
    jl_serialize_value(&f, jl_uint32_type);
    jl_serialize_value(&f, jl_uint64_type);
    jl_serialize_value(&f, jl_float32_type);
    jl_serialize_value(&f, jl_float64_type);
    jl_serialize_value(&f, jl_nothing);
    jl_serialize_value(&f, jl_weakref_type);
    jl_serialize_value(&f, jl_string_type);
    jl_serialize_value(&f, jl_ascii_string_type);
    jl_serialize_value(&f, jl_utf8_string_type);
    jl_serialize_value(&f, jl_errorexception_type);
    jl_serialize_value(&f, jl_typeerror_type);
    jl_serialize_value(&f, jl_loaderror_type);
    jl_serialize_value(&f, jl_uniontoocomplex_type);
    jl_serialize_value(&f, jl_backtrace_type);
    jl_serialize_value(&f, jl_stackovf_exception);
    jl_serialize_value(&f, jl_memory_exception);
    jl_serialize_value(&f, jl_divbyzero_exception);
    jl_serialize_value(&f, jl_undefref_exception);
    jl_serialize_value(&f, jl_interrupt_exception);
    jl_serialize_value(&f, jl_append_any_func);
    jl_serialize_value(&f, jl_method_missing_func);
    jl_serialize_value(&f, jl_get_global(jl_system_module,
                                         jl_symbol("IdTable")));
    jl_serialize_value(&f, jl_array_type->env);

    jl_serialize_module(&f, jl_system_module);
    //jl_serialize_finalizers(&f);
    write_int32(&f, jl_get_t_uid_ctr());
    write_int32(&f, jl_get_gs_ctr());
    htable_reset(&backref_table, 100000);

    ios_t ss;
    ios_file(&ss, startscriptname, 1, 0, 0, 0);
    ios_copyall(&f, &ss);
    ios_close(&ss);
    ios_putc(0, &f);

    ios_close(&f);
    if (en) jl_gc_enable();
}

extern jl_function_t *jl_typeinf_func;
extern int jl_boot_file_loaded;

DLLEXPORT
void jl_restore_system_image(char *fname)
{
    ios_t f;
    char *fpath = jl_find_file_in_path(fname);
    if (ios_file(&f, fpath, 1, 0, 0, 0) == NULL) {
        if (jl_errorexception_type == NULL) {
            ios_printf(ios_stderr, "system image file not found\n");
            exit(1);
        }
        else {
            jl_error("system image file not found");
        }
    }
#ifdef JL_GC_MARKSWEEP
    int en = jl_gc_is_enabled();
    jl_gc_disable();
#endif

    // values needed by C RTS code
    jl_char_type    = (jl_bits_type_t*)jl_deserialize_value(&f);
    jl_int8_type    = (jl_bits_type_t*)jl_deserialize_value(&f);
    jl_uint8_type   = (jl_bits_type_t*)jl_deserialize_value(&f);
    jl_int16_type   = (jl_bits_type_t*)jl_deserialize_value(&f);
    jl_uint16_type  = (jl_bits_type_t*)jl_deserialize_value(&f);
    jl_uint32_type  = (jl_bits_type_t*)jl_deserialize_value(&f);
    jl_uint64_type  = (jl_bits_type_t*)jl_deserialize_value(&f);
    jl_float32_type = (jl_bits_type_t*)jl_deserialize_value(&f);
    jl_float64_type = (jl_bits_type_t*)jl_deserialize_value(&f);
    jl_init_box_caches();
    jl_nothing = jl_deserialize_value(&f);
    jl_weakref_type = (jl_struct_type_t*)jl_deserialize_value(&f);
    jl_weakref_type->fptr = jl_weakref_ctor;
    jl_weakref_type->env = NULL;
    jl_weakref_type->linfo = NULL;
    jl_string_type = (jl_tag_type_t*)jl_deserialize_value(&f);
    jl_ascii_string_type = (jl_struct_type_t*)jl_deserialize_value(&f);
    jl_utf8_string_type = (jl_struct_type_t*)jl_deserialize_value(&f);
    jl_errorexception_type = (jl_struct_type_t*)jl_deserialize_value(&f);
    jl_typeerror_type = (jl_struct_type_t*)jl_deserialize_value(&f);
    jl_loaderror_type = (jl_struct_type_t*)jl_deserialize_value(&f);
    jl_uniontoocomplex_type = (jl_struct_type_t*)jl_deserialize_value(&f);
    jl_backtrace_type = (jl_struct_type_t*)jl_deserialize_value(&f);
    jl_stackovf_exception = jl_deserialize_value(&f);
    jl_memory_exception = jl_deserialize_value(&f);
    jl_divbyzero_exception = jl_deserialize_value(&f);
    jl_undefref_exception = jl_deserialize_value(&f);
    jl_interrupt_exception = jl_deserialize_value(&f);
    jl_append_any_func = (jl_function_t*)jl_deserialize_value(&f);
    jl_method_missing_func = (jl_function_t*)jl_deserialize_value(&f);
    jl_idtable_type = (jl_struct_type_t*)jl_deserialize_value(&f);
    jl_array_type->env = jl_deserialize_value(&f);
    jl_array_uint8_type =
        (jl_type_t*)jl_apply_type((jl_value_t*)jl_array_type,
                                  jl_tuple2(jl_uint8_type,
                                            jl_box_long(1)));

    jl_deserialize_module(&f, jl_system_module);
    //jl_deserialize_finalizers(&f);
    jl_set_t_uid_ctr(read_int32(&f));
    jl_set_gs_ctr(read_int32(&f));
    htable_reset(&backref_table, 100000);

    ios_t ss;
    ios_mem(&ss, 0);
    ios_copyuntil(&ss, &f, '\0');
    ios_close(&f);
    if (fpath != fname) free(fpath);

    jl_typeinf_func =
        (jl_function_t*)*(jl_get_bindingp(jl_system_module,
                                          jl_symbol("typeinf_ext")));
    jl_show_gf = (jl_function_t*)jl_get_global(jl_system_module,jl_symbol("show"));
    jl_convert_gf = (jl_function_t*)jl_get_global(jl_system_module,jl_symbol("convert"));
    jl_boot_file_loaded = 1;

#ifdef JL_GC_MARKSWEEP
    if (en) jl_gc_enable();
#endif

    jl_value_t *fexpr = jl_parse_file_string(ss.buf);
    JL_GC_PUSH(&fexpr);
    ios_close(&ss);
    // TODO: there is no exception handler here!
    jl_load_file_expr(fname, fexpr);
    JL_GC_POP();
}

// --- init ---

void jl_init_serializer()
{
    htable_new(&ser_tag, 0);
    htable_new(&deser_tag, 0);
    htable_new(&fptr_to_id, 0);
    htable_new(&id_to_fptr, 0);
    htable_new(&backref_table, 100000);

    void *tags[] = { jl_symbol_type, jl_tag_kind, jl_bits_kind, jl_struct_kind,
                     jl_func_kind, jl_tuple_type, jl_array_type, jl_expr_type,
                     (void*)LongSymbol_tag, (void*)LongTuple_tag,
                     (void*)LongExpr_tag, jl_intrinsic_type, jl_methtable_type,
                     jl_typename_type, jl_lambda_info_type, jl_tvar_type,
                     jl_symbolnode_type, jl_labelnode_type,
                     jl_linenumbernode_type,

                     jl_null, jl_any_type, jl_symbol("Any"),
                     jl_symbol("Array"), jl_symbol("TypeVar"),
                     jl_symbol("FuncKind"), jl_symbol("Box"),
                     lambda_sym, vinf_sym, locals_sym, body_sym, return_sym,
                     call_sym, colons_sym, null_sym, goto_sym, goto_ifnot_sym,
                     jl_symbol("string"),
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
                     jl_box_int64(0), jl_box_int64(1), jl_box_int64(2),
                     jl_box_int64(3), jl_box_int64(4),

                     jl_type_type, jl_bottom_type, jl_pointer_type,
                     jl_seq_type, jl_ntuple_type, jl_abstractarray_type,
                     jl_box_type, jl_typector_type, jl_undef_type, jl_any_func,
                     jl_task_type, jl_union_kind, jl_function_type,
                     jl_typetype_type, jl_typetype_tvar, jl_ANY_flag,

                     jl_symbol_type->name, jl_pointer_type->name,
                     jl_tag_kind->name, jl_union_kind->name, jl_bits_kind->name, jl_struct_kind->name,
                     jl_func_kind->name, jl_array_type->name, jl_expr_type->name,
                     jl_typename_type->name, jl_type_type->name, jl_methtable_type->name,
                     jl_tvar_type->name,
                     jl_seq_type->name, jl_ntuple_type->name, jl_abstractarray_type->name,
                     jl_lambda_info_type->name, jl_box_type->name,
                     jl_typector_type->name, jl_intrinsic_type->name, jl_undef_type->name,
                     jl_task_type->name, jl_symbolnode_type->name,
                     jl_labelnode_type->name, jl_linenumbernode_type->name,

                     jl_root_task,

                     NULL };
    ptrint_t i=2;
    while (tags[i-2] != NULL) {
        ptrhash_put(&ser_tag, tags[i-2], (void*)i);
        ptrhash_put(&deser_tag, (void*)i, tags[i-2]);
        i += 1;
    }
    VALUE_TAGS = (ptrint_t)ptrhash_get(&ser_tag, jl_null);

    void *fptrs[] = { jl_f_new_expr,
                      jl_f_new_box,
                      jl_weakref_ctor, 
                      jl_new_array_internal, 
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
                      jl_f_field_type, 
                      jl_f_arraylen, 
                      jl_f_arrayref, 
                      jl_f_arrayset, 
                      jl_f_arraysize, 
                      jl_f_instantiate_type, 
                      jl_f_convert, 
                      jl_f_convert_tuple,
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
                      jl_f_def_macro,
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
