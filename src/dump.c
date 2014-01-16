/*
  saving and restoring system images
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
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
static const ptrint_t LiteralVal_tag = 26;
static const ptrint_t SmallInt64_tag = 27;
static const ptrint_t IdTable_tag    = 28;
static const ptrint_t Int32_tag      = 29;
static const ptrint_t Array1d_tag    = 30;
static const ptrint_t Null_tag         = 253;
static const ptrint_t ShortBackRef_tag = 254;
static const ptrint_t BackRef_tag      = 255;

static ptrint_t VALUE_TAGS;

// pointers to non-AST-ish objects in a compressed tree
static jl_array_t *tree_literal_values=NULL;

static jl_value_t *jl_idtable_type=NULL;

// queue of types to cache
static jl_array_t *datatype_list=NULL;

#define write_uint8(s, n) ios_putc((n), (s))
#define read_uint8(s) ((uint8_t)ios_getc(s))
#define write_int8(s, n) write_uint8(s, n)
#define read_int8(s) read_uint8(s)
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

static void write_uint16(ios_t *s, uint16_t i)
{
    write_uint8(s, i       & 0xff);
    write_uint8(s, (i>> 8) & 0xff);
}

static uint16_t read_uint16(ios_t *s)
{
    int b0 = read_uint8(s);
    int b1 = read_uint8(s);
    return b0 | (b1<<8);
}

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

// --- Static Compile ---

#define jl_serialize_value(s, v) jl_serialize_value_(s,(jl_value_t*)(v))
static void jl_serialize_value_(ios_t *s, jl_value_t *v);
static jl_value_t *jl_deserialize_value(ios_t *s);
static jl_value_t *jl_deserialize_value_internal(ios_t *s);
static jl_value_t ***sysimg_gvars = NULL;

static void jl_load_sysimg_so(char *fname)
{
    // attempt to load the pre-compiled sysimg at fname
    // if this succeeds, sysimg_gvars will be a valid array
    // otherwise, it will be NULL
    uv_lib_t *sysimg_handle = jl_load_dynamic_library_e(fname, JL_RTLD_DEFAULT);
    if (sysimg_handle != 0) {
        sysimg_gvars = (jl_value_t***)jl_dlsym(sysimg_handle, "jl_sysimg_gvars");
    }
    else {
        sysimg_gvars = 0;
    }
}

static jl_value_t *jl_deserialize_gv(ios_t *s, jl_value_t *v)
{
    // Restore the GlobalVariable reference to this jl_value_t via the sysimg_gvars table
    int32_t gvname_index = read_int32(s)-1;
    if (sysimg_gvars != NULL && gvname_index >= 0) {
        *sysimg_gvars[gvname_index] = v;
    }
    return v;
}

static void jl_serialize_gv(ios_t *s, jl_value_t *v)
{
    // write the index of the literal_pointer_val into the system image
    write_int32(s, jl_get_llvm_gv(v));
}

static void jl_serialize_globalvals(ios_t *s)
{
    size_t i, len = backref_table.size;
    void **p = backref_table.table;
    for(i=0; i < len; i+=2) {
        void *offs = p[i+1];
        if (offs != HT_NOTFOUND) {
            int32_t gv = jl_get_llvm_gv((jl_value_t*)p[i]);
            if (gv != 0) {
                write_int32(s, (int)(intptr_t)offs);
                write_int32(s, gv);
            }
        }
    }
    write_int32(s, 0);
}

static void jl_deserialize_globalvals(ios_t *s)
{
    while (1) {
        intptr_t key = read_int32(s);
        if (key == 0) break;
        jl_deserialize_gv(s, (jl_value_t*)ptrhash_get(&backref_table, (void*)key));
    }
}

static void jl_serialize_gv_syms(ios_t *s, jl_sym_t *v)
{
    // ensures all symbols referenced in the code have
    // references in the system image to their global variable
    // since symbols are static, they might not have had a
    // reference anywhere in the code image other than here
    void *bp = ptrhash_get(&backref_table, v);
    if (bp == HT_NOTFOUND) {
        int32_t gv = jl_get_llvm_gv((jl_value_t*)v);
        if (gv != 0) {
            jl_serialize_value(s, v);
            write_int32(s, gv);
        }
    }
    if (v->left) jl_serialize_gv_syms(s, v->left);
    if (v->right) jl_serialize_gv_syms(s, v->right);
}

static void jl_deserialize_gv_syms(ios_t *s)
{
    while (1) {
        jl_value_t *v = jl_deserialize_value(s);
        if (!v) break;
        jl_deserialize_gv(s, v);
    }
}

struct delayed_fptrs_t {
    jl_lambda_info_t *li;
    int32_t func;
    int32_t cfunc;
} *delayed_fptrs = NULL;
static size_t delayed_fptrs_n = 0;
static size_t delayed_fptrs_max = 0;

static void jl_delayed_fptrs(jl_lambda_info_t *li, int32_t func, int32_t cfunc)
{
    // can't restore the fptrs until after the system image is fully restored,
    // since it will try to decompress the function AST to determine the argument types
    if (cfunc || func) {
        if (delayed_fptrs_max < delayed_fptrs_n + 1) {
            if (delayed_fptrs_max == 0)
                // current measurements put the number of functions at 1130
                delayed_fptrs_max = 2048;
            else
                delayed_fptrs_max *= 2;
            delayed_fptrs = (struct delayed_fptrs_t*)realloc(delayed_fptrs, delayed_fptrs_max*sizeof(delayed_fptrs[0])); //assumes sizeof==alignof
        }
        delayed_fptrs[delayed_fptrs_n].li = li;
        delayed_fptrs[delayed_fptrs_n].func = func;
        delayed_fptrs[delayed_fptrs_n].cfunc = cfunc;
        delayed_fptrs_n++;
    }
}

static void jl_update_all_fptrs()
{
    //printf("delayed_fptrs_n: %d\n", delayed_fptrs_n);
    jl_value_t ***gvars = sysimg_gvars;
    if (gvars == 0) return;
    // jl_fptr_to_llvm needs to decompress some ASTs, therefore this needs to be NULL
    // to skip trying to restore GlobalVariable pointers in jl_deserialize_gv
    sysimg_gvars = NULL;
    size_t i;
    for (i = 0; i < delayed_fptrs_n; i++) {
        jl_lambda_info_t *li = delayed_fptrs[i].li;
        int32_t func = delayed_fptrs[i].func-1;
        if (func >= 0) {
            jl_fptr_to_llvm((void*)gvars[func], li, 0);
        }
        int32_t cfunc = delayed_fptrs[i].cfunc-1;
        if (cfunc >= 0) {
            jl_fptr_to_llvm((void*)gvars[cfunc], li, 1);
        }
    }
    delayed_fptrs_n = 0;
    delayed_fptrs_max = 0;
    free(delayed_fptrs);
    delayed_fptrs = NULL;
}


// --- serialize ---

static void jl_serialize_fptr(ios_t *s, void *fptr)
{
    void **pbp = ptrhash_bp(&fptr_to_id, fptr);
    if (*pbp == HT_NOTFOUND)
        jl_error("unknown function pointer");
    write_uint16(s, *(ptrint_t*)pbp);
}

static void jl_serialize_datatype(ios_t *s, jl_datatype_t *dt)
{
    writetag(s, (jl_value_t*)jl_datatype_type);
    jl_serialize_value(s, (jl_value_t*)jl_datatype_type);
    int tag = 0;
    if (dt == jl_int32_type)
        tag = 2;
    else if (dt == jl_bool_type)
        tag = 3;
    else if (dt == jl_int64_type)
        tag = 4;
    write_uint8(s, tag);
    size_t nf = jl_tuple_len(dt->names);
    write_uint16(s, nf);
    write_int32(s, dt->size);
    if (nf > 0) {
        write_int32(s, dt->alignment);
        ios_write(s, (char*)&dt->fields[0], nf*sizeof(jl_fielddesc_t));
        jl_serialize_value(s, dt->names);
        jl_serialize_value(s, dt->types);
    }
    int has_instance = !!(dt->instance != NULL);
    write_uint8(s, dt->abstract | (dt->mutabl<<1) | (dt->pointerfree<<2) | (has_instance<<3));
    if (!dt->abstract)
        write_int32(s, dt->uid);

    jl_serialize_value(s, dt->parameters);
    jl_serialize_value(s, dt->name);
    jl_serialize_value(s, dt->super);
    jl_serialize_value(s, dt->ctor_factory);
    jl_serialize_value(s, dt->env);
    jl_serialize_value(s, dt->linfo);
    if (has_instance)
        jl_serialize_value(s, dt->instance);
    jl_serialize_fptr(s, (void*)dt->fptr);
}

static void jl_serialize_module(ios_t *s, jl_module_t *m)
{
    // set on every startup; don't save value
    jl_sym_t *jhsym = jl_symbol("JULIA_HOME");
    writetag(s, jl_module_type);
    jl_serialize_value(s, m->name);
    jl_serialize_value(s, m->parent);
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m || m != jl_main_module) {
                jl_serialize_value(s, b->name);
                if (table[i-1] == jhsym && m == jl_core_module) {
                    jl_serialize_value(s, NULL);
                }
                else {
                    jl_serialize_value(s, b->value);
                }
                jl_serialize_value(s, b->type);
                jl_serialize_value(s, b->owner);
                write_int8(s, (b->constp<<2) | (b->exportp<<1) | (b->imported));
                jl_serialize_gv(s, (jl_value_t*)b);
            }
        }
    }
    jl_serialize_value(s, NULL);
    if (m == jl_main_module) {
        write_int32(s, 1);
        jl_serialize_value(s, (jl_value_t*)jl_core_module);
    }
    else {
        write_int32(s, m->usings.len);
        for(i=0; i < m->usings.len; i++) {
            jl_serialize_value(s, (jl_value_t*)m->usings.items[i]);
        }
    }
    jl_serialize_value(s, m->constant_table);
}

static int is_ast_node(jl_value_t *v)
{
    if (jl_is_lambda_info(v)) {
        jl_lambda_info_t *li = (jl_lambda_info_t*)v;
        if (jl_is_expr(li->ast))
            li->ast = jl_compress_ast(li, li->ast);
        return 0;
    }
    return jl_is_symbol(v) || jl_is_expr(v) || jl_is_newvarnode(v) ||
        jl_typeis(v, jl_array_any_type) || jl_is_tuple(v) ||
        jl_is_uniontype(v) || jl_is_int32(v) || jl_is_int64(v) ||
        jl_is_symbolnode(v) || jl_is_bool(v) || jl_is_typevar(v) ||
        jl_is_topnode(v) || jl_is_quotenode(v) || jl_is_gotonode(v) ||
        jl_is_labelnode(v) || jl_is_linenode(v) || jl_is_getfieldnode(v);
}

static int literal_val_id(jl_value_t *v)
{
    for(int i=0; i < jl_array_len(tree_literal_values); i++) {
        if (jl_egal(jl_cellref(tree_literal_values,i), v))
            return i;
    }
    jl_cell_1d_push(tree_literal_values, v);
    return jl_array_len(tree_literal_values)-1;
}

static void jl_serialize_value_(ios_t *s, jl_value_t *v)
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

    if (tree_literal_values) {
        // compressing tree
        if (!is_ast_node(v)) {
            writetag(s, (jl_value_t*)LiteralVal_tag);
            write_uint16(s, literal_val_id(v));
            return;
        }
    }
    else {
        bp = ptrhash_bp(&backref_table, v);
        if (*bp != HT_NOTFOUND) {
            if ((uptrint_t)*bp < 65536) {
                write_uint8(s, ShortBackRef_tag);
                write_uint16(s, (uptrint_t)*bp);
            }
            else {
                write_uint8(s, BackRef_tag);
                write_int32(s, (uptrint_t)*bp);
            }
            return;
        }
        ptrhash_put(&backref_table, v, (void*)(ptrint_t)ios_pos(s));
    }

    size_t i;
    if (jl_is_tuple(v)) {
        size_t l = jl_tuple_len(v);
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
        if (ar->ndims == 1 && ar->elsize < 128) {
            writetag(s, (jl_value_t*)Array1d_tag);
            write_uint8(s, (ar->ptrarray<<7) | (ar->elsize & 0x7f));
        }
        else {
            writetag(s, (jl_value_t*)jl_array_type);
            write_uint16(s, ar->ndims);
            write_uint16(s, (ar->ptrarray<<15) | (ar->elsize & 0x7fff));
        }
        jl_serialize_value(s, jl_typeof(ar));
        for (i=0; i < ar->ndims; i++)
            jl_serialize_value(s, jl_box_long(jl_array_dim(ar,i)));
        if (!ar->ptrarray) {
            size_t tot = jl_array_len(ar) * ar->elsize;
            ios_write(s, (char*)jl_array_data(ar), tot);
        }
        else {
            for(i=0; i < jl_array_len(ar); i++) {
                jl_serialize_value(s, jl_cellref(v, i));
            }
        }
    }
    else if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        size_t l = jl_array_len(e->args);
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
    else if (jl_is_datatype(v)) {
        jl_serialize_datatype(s, (jl_datatype_t*)v);
    }
    else if (jl_is_typevar(v)) {
        writetag(s, jl_tvar_type);
        jl_serialize_value(s, ((jl_tvar_t*)v)->name);
        jl_serialize_value(s, ((jl_tvar_t*)v)->lb);
        jl_serialize_value(s, ((jl_tvar_t*)v)->ub);
        write_int8(s, ((jl_tvar_t*)v)->bound);
    }
    else if (jl_is_function(v)) {
        writetag(s, jl_function_type);
        jl_function_t *f = (jl_function_t*)v;
        jl_serialize_value(s, (jl_value_t*)f->linfo);
        jl_serialize_value(s, f->env);
        if (f->linfo && f->linfo->ast && f->fptr != &jl_trampoline) {
            jl_serialize_fptr(s, (void*)&jl_trampoline);
        }
        else {
            jl_serialize_fptr(s, (void*)f->fptr);
        }
    }
    else if (jl_is_lambda_info(v)) {
        writetag(s, jl_lambda_info_type);
        jl_lambda_info_t *li = (jl_lambda_info_t*)v;
        jl_serialize_value(s, li->ast);
        jl_serialize_value(s, (jl_value_t*)li->sparams);
        // don't save cached type info for code in the Core module, because
        // it might reference types in the old Base module.
        if (li->module == jl_core_module)
            jl_serialize_value(s, (jl_value_t*)jl_null);
        else
            jl_serialize_value(s, (jl_value_t*)li->tfunc);
        jl_serialize_value(s, (jl_value_t*)li->name);
        jl_serialize_value(s, (jl_value_t*)li->specTypes);
        jl_serialize_value(s, (jl_value_t*)li->specializations);
        write_int8(s, li->inferred);
        jl_serialize_value(s, (jl_value_t*)li->file);
        write_int32(s, li->line);
        jl_serialize_value(s, (jl_value_t*)li->module);
        jl_serialize_value(s, (jl_value_t*)li->roots);
        jl_serialize_value(s, (jl_value_t*)li->def);
        jl_serialize_value(s, (jl_value_t*)li->capt);
        // save functionObject pointers
        write_int32(s, li->functionID);
        write_int32(s, li->cFunctionID);
    }
    else if (jl_typeis(v, jl_module_type)) {
        jl_serialize_module(s, (jl_module_t*)v);
    }
    else if (jl_typeis(v, jl_task_type)) {
        jl_error("Task cannot be serialized");
    }
    else {
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        void *data = jl_data_ptr(v);
        if (t == jl_int64_type &&
            *(int64_t*)data >= S32_MIN && *(int64_t*)data <= S32_MAX) {
            writetag(s, (jl_value_t*)SmallInt64_tag);
            write_int32(s, (int32_t)*(int64_t*)data);
        }
        else if (t == jl_int32_type) {
            writetag(s, (jl_value_t*)Int32_tag);
            write_int32(s, (int32_t)*(int32_t*)data);
        }
        else {
            if ((jl_value_t*)t == jl_idtable_type)
                writetag(s, (jl_value_t*)IdTable_tag);
            else
                writetag(s, (jl_value_t*)jl_datatype_type);
            jl_serialize_value(s, t);
            size_t nf = jl_tuple_len(t->names);
            if (nf == 0 && jl_datatype_size(t)>0) {
                if (t->name == jl_pointer_type->name) {
                    write_int32(s, 0);
#ifdef _P64
                    write_int32(s, 0);
#endif
                }
                else {
                    ios_write(s, (char*)data, jl_datatype_size(t));
                }
            }
            else {
                if ((jl_value_t*)t == jl_idtable_type) {
                    jl_array_t *data = (jl_array_t*)jl_get_nth_field(v, 0);
                    jl_value_t **d = (jl_value_t**)data->data;
                    for(size_t i=0; i < jl_array_len(data); i+=2) {
                        if (d[i+1] != NULL) {
                            jl_serialize_value(s, d[i+1]);
                            jl_serialize_value(s, d[i]);
                        }
                    }
                    jl_serialize_value(s, NULL);
                }
                else {
                    for(size_t i=0; i < nf; i++) {
                        jl_serialize_value(s, jl_get_nth_field(v, i));
                    }
                }
            }
        }
    }
}

// --- deserialize ---

static jl_fptr_t jl_deserialize_fptr(ios_t *s)
{
    int fptr = read_uint16(s);
    if (fptr == 0)
        return NULL;
    void **pbp = ptrhash_bp(&id_to_fptr, (void*)(ptrint_t)fptr);
    if (*pbp == HT_NOTFOUND)
        jl_error("unknown function pointer ID");
    return *(jl_fptr_t*)pbp;
}

#define DTINSTANCE_PLACEHOLDER ((void*)2)

static jl_value_t *jl_deserialize_datatype(ios_t *s, int pos)
{
    int tag = read_uint8(s);
    uint16_t nf = read_uint16(s);
    size_t size = read_int32(s);
    jl_datatype_t *dt;
    if (tag == 2)
        dt = jl_int32_type;
    else if (tag == 3)
        dt = jl_bool_type;
    else if (tag == 4)
        dt = jl_int64_type;
    else
        dt = jl_new_uninitialized_datatype(nf);
    dt->size = size;
    dt->struct_decl = NULL;
    dt->instance = NULL;

    assert(tree_literal_values==NULL);
    ptrhash_put(&backref_table, (void*)(ptrint_t)pos, dt);

    if (nf > 0) {
        dt->alignment = read_int32(s);
        ios_read(s, (char*)&dt->fields[0], nf*sizeof(jl_fielddesc_t));
        dt->names = (jl_tuple_t*)jl_deserialize_value(s);
        dt->types = (jl_tuple_t*)jl_deserialize_value(s);
    }
    else {
        dt->alignment = dt->size;
        if (dt->alignment > MAX_ALIGN)
            dt->alignment = MAX_ALIGN;
        dt->names = dt->types = jl_null;
    }
    uint8_t flags = read_uint8(s);
    dt->abstract = flags&1;
    dt->mutabl = (flags>>1)&1;
    dt->pointerfree = (flags>>2)&1;
    int has_instance = (flags>>3)&1;
    if (!dt->abstract)
        dt->uid = read_int32(s);
    else
        dt->uid = 0;
    dt->parameters = (jl_tuple_t*)jl_deserialize_value(s);
    dt->name = (jl_typename_t*)jl_deserialize_value(s);
    dt->super = (jl_datatype_t*)jl_deserialize_value(s);
    dt->ctor_factory = jl_deserialize_value(s);
    dt->env = jl_deserialize_value(s);
    dt->linfo = (jl_lambda_info_t*)jl_deserialize_value(s);
    if (has_instance) {
        jl_value_t *instance = (jl_value_t*)jl_deserialize_value_internal(s);
        if (instance == DTINSTANCE_PLACEHOLDER)
            instance = jl_new_struct_uninit(dt);
        dt->instance = instance;
    }
    dt->fptr = jl_deserialize_fptr(s);
    if (dt->name == jl_array_type->name || dt->name == jl_pointer_type->name ||
        dt->name == jl_type_type->name || dt->name == jl_vararg_type->name ||
        dt->name == jl_abstractarray_type->name || dt->name == jl_storedarray_type->name ||
        dt->name == jl_densearray_type->name) {
        // builtin types are not serialized, so their caches aren't
        // explicitly saved. so we reconstruct the caches of builtin
        // parametric types here.
        jl_cell_1d_push(datatype_list, (jl_value_t*)dt);
    }

    return (jl_value_t*)dt;
}

jl_array_t *jl_eqtable_put(jl_array_t *h, void *key, void *val);

// Internal jl_deserialize_value. May return the placeholder value DTINSTANCE_PLACEHOLDER, unlike jl_deserialize_value
static jl_value_t *jl_deserialize_value_internal(ios_t *s)
{
    int pos = ios_pos(s);
    int32_t tag = read_uint8(s);
    if (tag == Null_tag)
        return NULL;
    if (tag == 0) {
        tag = read_uint8(s);
        jl_value_t *v = (jl_value_t*)ptrhash_get(&deser_tag, (void*)(ptrint_t)tag);
        assert(v != HT_NOTFOUND);
        return v;
    }
    if (tag == BackRef_tag || tag == ShortBackRef_tag) {
        assert(tree_literal_values == NULL);
        ptrint_t offs = (tag == BackRef_tag) ? read_int32(s) : read_uint16(s);
        void **bp = ptrhash_bp(&backref_table, (void*)(ptrint_t)offs);
        assert(*bp != HT_NOTFOUND);
        return (jl_value_t*)*bp;
    }

    jl_value_t *vtag=(jl_value_t*)ptrhash_get(&deser_tag,(void*)(ptrint_t)tag);
    if (tag >= VALUE_TAGS) {
        return vtag;
    }
    else if (vtag == (jl_value_t*)LiteralVal_tag) {
        return jl_cellref(tree_literal_values, read_uint16(s));
    }

    int usetable = (tree_literal_values == NULL);

    size_t i;
    if (vtag == (jl_value_t*)jl_tuple_type ||
        vtag == (jl_value_t*)LongTuple_tag) {
        size_t len;
        if (vtag == (jl_value_t*)jl_tuple_type)
            len = read_uint8(s);
        else
            len = read_int32(s);
        jl_tuple_t *tu = jl_alloc_tuple_uninit(len);
        if (usetable)
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
        char *name = (char*)alloca(len+1);
        ios_read(s, name, len);
        name[len] = '\0';
        jl_value_t *sym = (jl_value_t*)jl_symbol(name);
        if (usetable)
            ptrhash_put(&backref_table, (void*)(ptrint_t)pos, sym);
        return sym;
    }
    else if (vtag == (jl_value_t*)jl_array_type ||
             vtag == (jl_value_t*)Array1d_tag) {
        int16_t ndims;
        int isunboxed, elsize;
        if (vtag == (jl_value_t*)Array1d_tag) {
            ndims = 1;
            elsize = read_uint8(s);
            isunboxed = !(elsize>>7);
            elsize = elsize&0x7f;
        }
        else {
            ndims = read_uint16(s);
            elsize = read_uint16(s);
            isunboxed = !(elsize>>15);
            elsize = elsize&0x7fff;
        }
        jl_value_t *aty = jl_deserialize_value(s);
        size_t *dims = (size_t*)alloca(ndims*sizeof(size_t));
        for(i=0; i < ndims; i++)
            dims[i] = jl_unbox_long(jl_deserialize_value(s));
        jl_array_t *a = jl_new_array_for_deserialization((jl_value_t*)aty, ndims, dims, isunboxed, elsize);
        if (usetable)
            ptrhash_put(&backref_table, (void*)(ptrint_t)pos, (jl_value_t*)a);
        if (!a->ptrarray) {
            size_t tot = jl_array_len(a) * a->elsize;
            ios_read(s, (char*)jl_array_data(a), tot);
        }
        else {
            for(i=0; i < jl_array_len(a); i++) {
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
        if (usetable)
            ptrhash_put(&backref_table, (void*)(ptrint_t)pos, (jl_value_t*)e);
        e->etype = jl_deserialize_value(s);
        for(i=0; i < len; i++) {
            jl_cellset(e->args, i, jl_deserialize_value(s));
        }
        return (jl_value_t*)e;
    }
    else if (vtag == (jl_value_t*)jl_tvar_type) {
        jl_tvar_t *tv = (jl_tvar_t*)newobj((jl_value_t*)jl_tvar_type, 4);
        if (usetable)
            ptrhash_put(&backref_table, (void*)(ptrint_t)pos, tv);
        tv->name = (jl_sym_t*)jl_deserialize_value(s);
        tv->lb = jl_deserialize_value(s);
        tv->ub = jl_deserialize_value(s);
        tv->bound = read_int8(s);
        return (jl_value_t*)tv;
    }
    else if (vtag == (jl_value_t*)jl_function_type) {
        jl_function_t *f =
            (jl_function_t*)newobj((jl_value_t*)jl_function_type, 3);
        if (usetable)
            ptrhash_put(&backref_table, (void*)(ptrint_t)pos, f);
        f->linfo = (jl_lambda_info_t*)jl_deserialize_value(s);
        f->env = jl_deserialize_value(s);
        f->fptr = jl_deserialize_fptr(s);
        return (jl_value_t*)f;
    }
    else if (vtag == (jl_value_t*)jl_lambda_info_type) {
        jl_lambda_info_t *li =
            (jl_lambda_info_t*)newobj((jl_value_t*)jl_lambda_info_type,
                                      LAMBDA_INFO_NW);
        if (usetable)
            ptrhash_put(&backref_table, (void*)(ptrint_t)pos, li);
        li->ast = jl_deserialize_value(s);
        li->sparams = (jl_tuple_t*)jl_deserialize_value(s);
        li->tfunc = jl_deserialize_value(s);
        li->name = (jl_sym_t*)jl_deserialize_value(s);
        li->specTypes = (jl_tuple_t*)jl_deserialize_value(s);
        li->specializations = (jl_array_t*)jl_deserialize_value(s);
        li->inferred = read_int8(s);
        li->file = (jl_sym_t*)jl_deserialize_value(s);
        li->line = read_int32(s);
        li->module = (jl_module_t*)jl_deserialize_value(s);
        li->roots = (jl_array_t*)jl_deserialize_value(s);
        li->def = (jl_lambda_info_t*)jl_deserialize_value(s);
        li->capt = jl_deserialize_value(s);
        li->fptr = &jl_trampoline;
        li->functionObject = NULL;
        li->cFunctionObject = NULL;
        li->inInference = 0;
        li->inCompile = 0;
        li->unspecialized = NULL;
        li->functionID = 0;
        li->cFunctionID = 0;
        int32_t cfunc_llvm, func_llvm;
        func_llvm = read_int32(s);
        cfunc_llvm = read_int32(s);
        jl_delayed_fptrs(li, func_llvm, cfunc_llvm);
        return (jl_value_t*)li;
    }
    else if (vtag == (jl_value_t*)jl_module_type) {
        jl_sym_t *mname = (jl_sym_t*)jl_deserialize_value(s);
        jl_module_t *m = jl_new_module(mname);
        if (usetable)
            ptrhash_put(&backref_table, (void*)(ptrint_t)pos, m);
        m->parent = (jl_module_t*)jl_deserialize_value(s);
        while (1) {
            jl_sym_t *name = (jl_sym_t*)jl_deserialize_value(s);
            if (name == NULL)
                break;
            jl_binding_t *b = jl_get_binding_wr(m, name);
            b->value = jl_deserialize_value(s);
            b->type = (jl_value_t*)jl_deserialize_value(s);
            b->owner = (jl_module_t*)jl_deserialize_value(s);
            int8_t flags = read_int8(s);
            b->constp = (flags>>2) & 1;
            b->exportp = (flags>>1) & 1;
            b->imported = (flags) & 1;
            jl_deserialize_gv(s, (jl_value_t*)b);
        }
        size_t ni = read_int32(s);
        for(size_t i=0; i < ni; i++) {
            arraylist_push(&m->usings, jl_deserialize_value(s));
        }
        m->constant_table = (jl_array_t*)jl_deserialize_value(s);
        return (jl_value_t*)m;
    }
    else if (vtag == (jl_value_t*)SmallInt64_tag) {
        jl_value_t *v = jl_box_int64(read_int32(s));
        if (usetable)
            ptrhash_put(&backref_table, (void*)(ptrint_t)pos, v);
        return v;
    }
    else if (vtag == (jl_value_t*)Int32_tag) {
        jl_value_t *v = jl_box_int32(read_int32(s));
        if (usetable)
            ptrhash_put(&backref_table, (void*)(ptrint_t)pos, v);
        return v;
    }
    else if (vtag == (jl_value_t*)jl_datatype_type || vtag == (jl_value_t*)IdTable_tag) {
        // If the value v we are about to deserialize is some dt->instance, we have a circular 
        // reference, because v == v->type->instance. To avoid this, we put a null value in 
        // the backref table to reserve the space. If jl_deserialize_value encounters this,
        // it knows to do the allocation itself. This work, because in all instances where
        // v->type->instance != null, v->type has no fields, so there is no further serialized
        // data stored that we would need to construct the type. 
        if (usetable)
            ptrhash_put(&backref_table, (void*)(ptrint_t)pos, DTINSTANCE_PLACEHOLDER);
        jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s);
        if (dt == jl_datatype_type)
            return jl_deserialize_datatype(s, pos);
        size_t nf = jl_tuple_len(dt->names);
        jl_value_t *v;
        if (nf == 0 && jl_datatype_size(dt)>0) {
            int nby = jl_datatype_size(dt);
            char *data = (char*)alloca(nby);
            ios_read(s, data, nby);
            v = NULL;
            if (dt == jl_int32_type)
                v = jl_box_int32(*(int32_t*)data);
            else if (dt == jl_int64_type)
                v = jl_box_int64(*(int64_t*)data);
            else if (dt == jl_bool_type)
                v = jl_box_bool(*(int8_t*)data);
            else {
                switch (nby) {
                case 1: v = jl_box8 (dt, *(int8_t *)data); break;
                case 2: v = jl_box16(dt, *(int16_t*)data); break;
                case 4: v = jl_box32(dt, *(int32_t*)data); break;
                case 8: v = jl_box64(dt, *(int64_t*)data); break;
                default:
                    v = (jl_value_t*)allocobj(sizeof(void*)+nby);
                    v->type = (jl_value_t*)dt;
                    memcpy(jl_data_ptr(v), data, nby);
                }
            }
            if (usetable)
                ptrhash_put(&backref_table, (void*)(ptrint_t)pos, v);
        }
        else {
            if (dt->instance) {
                if (usetable)
                    *ptrhash_bp(&backref_table, (void*)(ptrint_t)pos) = dt->instance;
                return dt->instance;
            }
            v = jl_new_struct_uninit(dt);
            if (usetable)
                ptrhash_put(&backref_table, (void*)(ptrint_t)pos, v);
            if (vtag == (jl_value_t*)IdTable_tag) {
                jl_array_t *a = jl_alloc_cell_1d(32);
                while (1) {
                    jl_value_t *val = jl_deserialize_value(s);
                    if (val == NULL)
                        break;
                    jl_value_t *key = jl_deserialize_value(s);
                    a = jl_eqtable_put(a, key, val);
                }
                jl_set_nth_field(v, 0, (jl_value_t*)a);
            }
            else {
                for(i=0; i < nf; i++) {
                    jl_set_nth_field(v, i, jl_deserialize_value(s));
                }
            }
        }
        // TODO: put WeakRefs on the weak_refs list
        return v;
    }
    assert(0);
    return NULL;
}

static jl_value_t* jl_deserialize_value(ios_t *s)
{
    jl_value_t *v = jl_deserialize_value_internal(s);
    assert(v != DTINSTANCE_PLACEHOLDER);
    return v;
}

// --- entry points ---

DLLEXPORT
void jl_save_system_image(char *fname)
{
    jl_gc_collect();
    jl_gc_collect();
    int en = jl_gc_is_enabled();
    jl_gc_disable();
    htable_reset(&backref_table, 50000);
    ios_t f;
    if (ios_file(&f, fname, 1, 1, 1, 1) == NULL) {
        JL_PRINTF(JL_STDERR, "Cannot open system image file \"%s\" for writing.\n", fname);
        exit(1);
    }

    // orphan old Base module if present
    jl_base_module = (jl_module_t*)jl_get_global(jl_main_module, jl_symbol("Base"));

    // delete cached slow ASCIIString constructor if present
    jl_methtable_t *mt = jl_gf_mtable((jl_function_t*)jl_ascii_string_type);
    jl_array_t *spec = mt->defs->func->linfo->specializations;
    if (spec != NULL && jl_array_len(spec) > 0 &&
        ((jl_lambda_info_t*)jl_cellref(spec,0))->inferred == 0) {
        mt->cache = (jl_methlist_t*)JL_NULL;
        mt->cache_arg1 = (jl_array_t*)JL_NULL;
        mt->defs->func->linfo->tfunc = (jl_value_t*)jl_null;
        mt->defs->func->linfo->specializations = NULL;
    }

    jl_idtable_type = jl_get_global(jl_base_module, jl_symbol("ObjectIdDict"));

    jl_serialize_value(&f, jl_array_type->env);

    jl_serialize_value(&f, jl_main_module);

    // deser_tag is an array indexed from 2 until HT_NOTFOUND
    // ensure everything in there can be reassociated with its GlobalValue
    ptrint_t i=2;
    void *v = ptrhash_get(&deser_tag, (void*)i);
    while (v != HT_NOTFOUND) {
        jl_serialize_gv(&f, (jl_value_t*)v);
        v = ptrhash_get(&deser_tag, (void*)i);
        i += 1;
    }
    jl_serialize_globalvals(&f);
    jl_serialize_gv_syms(&f, jl_get_root_symbol()); // serialize symbols with GlobalValue references
    jl_serialize_value(&f, NULL); // signal the end of the symbols list

    write_int32(&f, jl_get_t_uid_ctr());
    write_int32(&f, jl_get_gs_ctr());
    htable_reset(&backref_table, 0);

    ios_close(&f);
    if (en) jl_gc_enable();
}

extern jl_function_t *jl_typeinf_func;
extern int jl_boot_file_loaded;
extern void jl_get_builtin_hooks(void);
extern void jl_get_system_hooks(void);
extern void jl_get_uv_hooks(void);

DLLEXPORT
void jl_restore_system_image(char *fname)
{
    ios_t f;
    char *fpath = fname;
    if (ios_file(&f, fpath, 1, 0, 0, 0) == NULL) {
        JL_PRINTF(JL_STDERR, "System image file \"%s\" not found\n", fname);
        exit(1);
    }
    int build_mode = (jl_compileropts.build_path != NULL);
#ifdef _OS_WINDOWS_
    //XXX: the windows linker forces our system image to be
    //     linked against only one dll, I picked libjulia-release
    if (jl_is_debugbuild()) build_mode = 1;
#endif
    if (!build_mode) {
        char *fname_shlib = (char*)alloca(strlen(fname));
        strcpy(fname_shlib, fname);
        char *fname_shlib_dot = strrchr(fname_shlib, '.');
        if (fname_shlib_dot != NULL) *fname_shlib_dot = 0;
        jl_load_sysimg_so(fname_shlib);
    }
#ifdef JL_GC_MARKSWEEP
    int en = jl_gc_is_enabled();
    jl_gc_disable();
#endif

    datatype_list = jl_alloc_cell_1d(0);

    jl_array_type->env = jl_deserialize_value(&f);
    
    jl_main_module = (jl_module_t*)jl_deserialize_value(&f);
    jl_core_module = (jl_module_t*)jl_get_global(jl_main_module,
                                                 jl_symbol("Core"));
    jl_base_module = (jl_module_t*)jl_get_global(jl_main_module,
                                                 jl_symbol("Base"));
    jl_current_module = jl_base_module; // run start_image in Base

    // deser_tag is an array indexed from 2 until HT_NOTFOUND
    // ensure everything in there is reassociated with its GlobalValue
    ptrint_t i=2;
    void *v = ptrhash_get(&deser_tag, (void*)i);
    while (v != HT_NOTFOUND) {
        jl_deserialize_gv(&f, (jl_value_t*)v);
        v = ptrhash_get(&deser_tag, (void*)i);
        i += 1;
    }
    jl_deserialize_globalvals(&f);
    jl_deserialize_gv_syms(&f);

    // cache builtin parametric types
    for(int i=0; i < jl_array_len(datatype_list); i++) {
        jl_value_t *v = jl_cellref(datatype_list, i);
        uint32_t uid = ((jl_datatype_t*)v)->uid;
        jl_cache_type_((jl_datatype_t*)v);
        ((jl_datatype_t*)v)->uid = uid;
    }

    jl_get_builtin_hooks();
    jl_get_system_hooks();
    jl_get_uv_hooks();
    jl_boot_file_loaded = 1;
    jl_typeinf_func = (jl_function_t*)jl_get_global(jl_base_module,
                                                    jl_symbol("typeinf_ext"));
    jl_init_box_caches();

    jl_set_t_uid_ctr(read_int32(&f));
    jl_set_gs_ctr(read_int32(&f));
    htable_reset(&backref_table, 0);

    ios_close(&f);
    if (fpath != fname) free(fpath);

#ifdef JL_GC_MARKSWEEP
    if (en) jl_gc_enable();
#endif
    // restore the value of our "magic" JULIA_HOME variable/constant
    jl_get_binding_wr(jl_core_module, jl_symbol("JULIA_HOME"))->value =
        jl_cstr_to_string(julia_home);
    jl_update_all_fptrs();
}

DLLEXPORT
jl_value_t *jl_ast_rettype(jl_lambda_info_t *li, jl_value_t *ast)
{
    if (jl_is_expr(ast))
        return jl_lam_body((jl_expr_t*)ast)->etype;
    tree_literal_values = li->module->constant_table;
    ios_t src;
    jl_array_t *bytes = (jl_array_t*)ast;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)bytes->data, jl_array_len(bytes), 0);
    src.size = jl_array_len(bytes);
    int en = jl_gc_is_enabled();
    jl_gc_disable();
    jl_value_t *rt = jl_deserialize_value(&src);
    if (en)
        jl_gc_enable();
    tree_literal_values = NULL;
    return rt;
}

DLLEXPORT
jl_value_t *jl_compress_ast(jl_lambda_info_t *li, jl_value_t *ast)
{
    ios_t dest;
    ios_mem(&dest, 0);
    jl_array_t *last_tlv = tree_literal_values;
    int en = jl_gc_is_enabled();
    jl_gc_disable();

    if (li->module->constant_table == NULL)
        li->module->constant_table = jl_alloc_cell_1d(0);
    tree_literal_values = li->module->constant_table;
    li->capt = (jl_value_t*)jl_lam_capt((jl_expr_t*)ast);
    if (jl_array_len(li->capt) == 0)
        li->capt = NULL;
    jl_serialize_value(&dest, jl_lam_body((jl_expr_t*)ast)->etype);
    jl_serialize_value(&dest, ast);

    //JL_PRINTF(JL_STDERR, "%d bytes, %d values\n", dest.size, vals->length);

    jl_value_t *v = (jl_value_t*)jl_takebuf_array(&dest);
    if (jl_array_len(tree_literal_values) == 0 && last_tlv == NULL) {
        li->module->constant_table = NULL;
    }
    tree_literal_values = last_tlv;
    if (en)
        jl_gc_enable();
    return v;
}

DLLEXPORT
jl_value_t *jl_uncompress_ast(jl_lambda_info_t *li, jl_value_t *data)
{
    jl_array_t *bytes = (jl_array_t*)data;
    tree_literal_values = li->module->constant_table;
    ios_t src;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)bytes->data, jl_array_len(bytes), 0);
    src.size = jl_array_len(bytes);
    int en = jl_gc_is_enabled();
    jl_gc_disable();
    jl_gc_ephemeral_on();
    (void)jl_deserialize_value(&src); // skip ret type
    jl_value_t *v = jl_deserialize_value(&src);
    jl_gc_ephemeral_off();
    if (en)
        jl_gc_enable();
    tree_literal_values = NULL;
    return v;
}

// --- init ---

void jl_init_serializer(void)
{
    htable_new(&ser_tag, 0);
    htable_new(&deser_tag, 0);
    htable_new(&fptr_to_id, 0);
    htable_new(&id_to_fptr, 0);
    htable_new(&backref_table, 50000);

    void *tags[] = { jl_symbol_type, jl_datatype_type,
                     jl_function_type, jl_tuple_type, jl_array_type,
                     jl_expr_type, (void*)LongSymbol_tag, (void*)LongTuple_tag,
                     (void*)LongExpr_tag, (void*)LiteralVal_tag,
                     (void*)SmallInt64_tag, (void*)IdTable_tag,
                     (void*)Int32_tag, (void*)Array1d_tag,
                     jl_module_type, jl_tvar_type, jl_lambda_info_type,

                     jl_null, jl_false, jl_true, jl_any_type, jl_symbol("Any"),
                     jl_symbol("Array"), jl_symbol("TypeVar"),
                     jl_symbol("Box"), jl_symbol("apply"),
                     lambda_sym, body_sym, return_sym, call_sym, colons_sym,
                     null_sym, goto_ifnot_sym, assign_sym,

                     jl_symbol("a"), jl_symbol("b"), jl_symbol("c"),
                     jl_symbol("d"), jl_symbol("e"), jl_symbol("f"),
                     jl_symbol("g"), jl_symbol("h"), jl_symbol("i"),
                     jl_symbol("j"), jl_symbol("k"), jl_symbol("l"),
                     jl_symbol("m"), jl_symbol("n"), jl_symbol("o"),
                     jl_symbol("p"), jl_symbol("q"), jl_symbol("r"),
                     jl_symbol("s"), jl_symbol("t"), jl_symbol("u"),
                     jl_symbol("v"), jl_symbol("w"), jl_symbol("x"),
                     jl_symbol("y"), jl_symbol("z"),
                     jl_symbol("A"), jl_symbol("B"), jl_symbol("C"),
                     jl_symbol("I"), jl_symbol("N"),
                     jl_symbol("T"), jl_symbol("S"),
                     jl_symbol("X"), jl_symbol("Y"),
                     jl_symbol("add_int"), jl_symbol("sub_int"),
                     jl_symbol("mul_int"), 
                     jl_symbol("add_float"), jl_symbol("sub_float"),
                     jl_symbol("mul_float"), jl_symbol("ccall"),
                     jl_symbol("box"), jl_symbol("unbox"),
                     jl_symbol("eq_int"), jl_symbol("slt_int"),
                     jl_symbol("sle_int"), jl_symbol("ne_int"),
                     jl_symbol("arrayset"), jl_symbol("arrayref"),
                     jl_symbol("arraylen"), jl_symbol("boundscheck"),
                     jl_symbol("convert"), jl_symbol("typeassert"),
                     jl_symbol("getfield"), jl_symbol("setfield"),
                     jl_symbol("tupleref"), jl_symbol("tuplelen"),
                     jl_symbol("apply_type"), tuple_sym,

                     jl_box_int32(0), jl_box_int32(1), jl_box_int32(2),
                     jl_box_int32(3), jl_box_int32(4), jl_box_int32(5),
                     jl_box_int32(6), jl_box_int32(7), jl_box_int32(8),
                     jl_box_int32(9), jl_box_int32(10), jl_box_int32(11),
                     jl_box_int32(12), jl_box_int32(13), jl_box_int32(14),
                     jl_box_int32(15), jl_box_int32(16), jl_box_int32(17),
                     jl_box_int32(18), jl_box_int32(19), jl_box_int32(20),
                     jl_box_int32(21), jl_box_int32(22), jl_box_int32(23),
                     jl_box_int32(24), jl_box_int32(25), jl_box_int32(26),
                     jl_box_int32(27), jl_box_int32(28), jl_box_int32(29),
                     jl_box_int32(30), jl_box_int32(31), jl_box_int32(32),
#ifndef _P64
                     jl_box_int32(33), jl_box_int32(34), jl_box_int32(35),
                     jl_box_int32(36), jl_box_int32(37), jl_box_int32(38),
                     jl_box_int32(39), jl_box_int32(40), jl_box_int32(41),
                     jl_box_int32(42), jl_box_int32(43), jl_box_int32(44),
                     jl_box_int32(45), jl_box_int32(46), jl_box_int32(47),
                     jl_box_int32(48), jl_box_int32(49), jl_box_int32(50),
                     jl_box_int32(51), jl_box_int32(52), jl_box_int32(53),
                     jl_box_int32(54), jl_box_int32(55), jl_box_int32(56),
                     jl_box_int32(57), jl_box_int32(58), jl_box_int32(59),
                     jl_box_int32(60), jl_box_int32(61),
#endif
                     jl_box_int64(0), jl_box_int64(1), jl_box_int64(2),
                     jl_box_int64(3), jl_box_int64(4), jl_box_int64(5),
                     jl_box_int64(6), jl_box_int64(7), jl_box_int64(8),
                     jl_box_int64(9), jl_box_int64(10), jl_box_int64(11),
                     jl_box_int64(12), jl_box_int64(13), jl_box_int64(14),
                     jl_box_int64(15), jl_box_int64(16), jl_box_int64(17),
                     jl_box_int64(18), jl_box_int64(19), jl_box_int64(20),
                     jl_box_int64(21), jl_box_int64(22), jl_box_int64(23),
                     jl_box_int64(24), jl_box_int64(25), jl_box_int64(26),
                     jl_box_int64(27), jl_box_int64(28), jl_box_int64(29),
                     jl_box_int64(30), jl_box_int64(31), jl_box_int64(32),
#ifdef _P64
                     jl_box_int64(33), jl_box_int64(34), jl_box_int64(35),
                     jl_box_int64(36), jl_box_int64(37), jl_box_int64(38),
                     jl_box_int64(39), jl_box_int64(40), jl_box_int64(41),
                     jl_box_int64(42), jl_box_int64(43), jl_box_int64(44),
                     jl_box_int64(45), jl_box_int64(46), jl_box_int64(47),
                     jl_box_int64(48), jl_box_int64(49), jl_box_int64(50),
                     jl_box_int64(51), jl_box_int64(52), jl_box_int64(53),
                     jl_box_int64(54), jl_box_int64(55), jl_box_int64(56),
                     jl_box_int64(57), jl_box_int64(58), jl_box_int64(59),
                     jl_box_int64(60), jl_box_int64(61),
#endif
                     jl_labelnode_type, jl_linenumbernode_type,
                     jl_gotonode_type, jl_quotenode_type, jl_topnode_type,
                     jl_type_type, jl_bottom_type, jl_pointer_type,
                     jl_vararg_type, jl_ntuple_type, jl_abstractarray_type,
                     jl_storedarray_type, jl_densearray_type, jl_box_type,
                     jl_typector_type, jl_undef_type, jl_top_type, jl_typename_type,
                     jl_task_type, jl_uniontype_type, jl_typetype_type, jl_typetype_tvar,
                     jl_ANY_flag, jl_array_any_type, jl_intrinsic_type, jl_method_type,
                     jl_methtable_type, jl_voidpointer_type, jl_newvarnode_type,
                     jl_array_symbol_type, jl_tupleref(jl_tuple_type,0),

                     jl_symbol_type->name, jl_pointer_type->name, jl_datatype_type->name,
                     jl_uniontype_type->name, jl_array_type->name, jl_expr_type->name,
                     jl_typename_type->name, jl_type_type->name, jl_methtable_type->name,
                     jl_method_type->name, jl_tvar_type->name, jl_vararg_type->name,
                     jl_ntuple_type->name, jl_abstractarray_type->name,
                     jl_storedarray_type->name, jl_densearray_type->name,
                     jl_lambda_info_type->name, jl_module_type->name, jl_box_type->name,
                     jl_function_type->name, jl_typector_type->name,
                     jl_intrinsic_type->name, jl_undef_type->name, jl_task_type->name,
                     jl_labelnode_type->name, jl_linenumbernode_type->name,
                     jl_gotonode_type->name, jl_quotenode_type->name,
                     jl_topnode_type->name,

                     jl_root_task, jl_bottom_func,

                     NULL };
    ptrint_t i=2;
    while (tags[i-2] != NULL) {
        ptrhash_put(&ser_tag, tags[i-2], (void*)i);
        ptrhash_put(&deser_tag, (void*)i, tags[i-2]);
        i += 1;
    }
    assert(i <= Null_tag);
    VALUE_TAGS = (ptrint_t)ptrhash_get(&ser_tag, jl_null);

    jl_fptr_t fptrs[] = { jl_f_new_expr, jl_f_new_box,
                          jl_f_throw, jl_f_is,
                          jl_f_no_function, jl_f_typeof,
                          jl_f_subtype, jl_f_isa,
                          jl_f_typeassert, jl_f_apply,
                          jl_f_top_eval, jl_f_isdefined,
                          jl_f_tuple, jl_f_tupleref,
                          jl_f_tuplelen, jl_f_get_field,
                          jl_f_set_field, jl_f_field_type,
                          jl_f_arraylen, jl_f_arrayref,
                          jl_f_arrayset, jl_f_arraysize,
                          jl_f_instantiate_type, jl_f_kwcall,
                          jl_f_convert_default, jl_f_convert_tuple,
                          jl_trampoline, jl_f_new_type_constructor,
                          jl_f_typevar, jl_f_union,
                          jl_f_methodexists, jl_f_applicable,
                          jl_f_invoke, jl_apply_generic,
                          jl_unprotect_stack, jl_f_task,
                          jl_f_yieldto, jl_f_ctor_trampoline,
                          jl_f_new_module,
                          NULL };
    i=2;
    while (fptrs[i-2] != NULL) {
        ptrhash_put(&fptr_to_id, (void*)fptrs[i-2], (void*)i);
        ptrhash_put(&id_to_fptr, (void*)i, (void*)fptrs[i-2]);
        i += 1;
    }
}
