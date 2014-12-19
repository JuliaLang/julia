/*
  saving and restoring system images
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"

#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

// hash of definitions for predefined tagged object
static htable_t ser_tag;
// array of definitions for the predefined tagged object types
// (reverse of ser_tag)
static jl_value_t* deser_tag[256];

// table of all objects that have been deserialized, indexed by pos
// (the order in the serializer stream) in MODE_MODULE, the low
// bit is reserved for flagging certain entries and pos is
// left shift by 1
static htable_t backref_table;
int backref_table_numel;
static arraylist_t backref_list;

// list of (jl_value_t *value, jl_value_t **loc, size_t pos) entries
// for anything that was flagged by the serializer for later rework of
// some sort
static arraylist_t flagref_list;

// list of any methtable objects that were deserialized in MODE_MODULE
// and need to be rehashed after assigning the uid fields to types
// (only used in MODE_MODULE and MODE_MODULE_LAMBDAS)
static arraylist_t methtable_list;

// hash of definitions for predefined function pointers
static htable_t fptr_to_id;
// array of definitions for the predefined function pointers
// (reverse of fptr_to_id)
static jl_fptr_t id_to_fptrs[] = {
  NULL, NULL,
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
  jl_trampoline, jl_f_union,
  jl_f_methodexists, jl_f_applicable,
  jl_f_invoke, jl_apply_generic,
  jl_unprotect_stack,
  jl_f_yieldto, jl_f_sizeof, jl_f_new_expr,
  NULL };

// pointers to non-AST-ish objects in a compressed tree
static jl_array_t *tree_literal_values=NULL; // (only used in MODE_AST)

static const ptrint_t LongSymbol_tag = 23;
static const ptrint_t LongTuple_tag  = 24;
static const ptrint_t LongExpr_tag   = 25;
static const ptrint_t LiteralVal_tag = 26;
static const ptrint_t SmallInt64_tag = 27;
static const ptrint_t IdTable_tag    = 28;
static const ptrint_t Int32_tag      = 29;
static const ptrint_t Array1d_tag    = 30;
static const ptrint_t Singleton_tag    = 31;
static const ptrint_t Null_tag         = 253;
static const ptrint_t ShortBackRef_tag = 254;
static const ptrint_t BackRef_tag      = 255;

static ptrint_t VALUE_TAGS;

typedef enum _DUMP_MODES {
    // not in the serializer at all, or
    // something is seriously wrong
    MODE_INVALID = 0,

    // jl_uncompress_ast
    // compressing / decompressing an AST Expr in a LambdaStaticData
    MODE_AST,

    // jl_restore_system_image
    // restoring an entire system image from disk
    MODE_SYSTEM_IMAGE,

    // jl_restore_new_module
    // restoring a single module from disk for integration
    // into the currently running system image / environment
    MODE_MODULE, // first-stage (pre type-uid assignment)
    MODE_MODULE_LAMBDAS, // second-stage (post type-uid assignment)
} DUMP_MODES;
static DUMP_MODES mode = (DUMP_MODES) 0;

static jl_value_t *jl_idtable_type=NULL;

// queue of types to cache
static jl_array_t *datatype_list=NULL; // (only used in MODE_SYSTEM_IMAGE)

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
static jl_value_t *jl_deserialize_value(ios_t *s, jl_value_t **loc);
jl_value_t ***sysimg_gvars = NULL;

extern int globalUnique;
extern void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType);
uv_lib_t *jl_sysimg_handle = NULL;
uint64_t jl_sysimage_base = 0;
#ifdef _OS_WINDOWS_
#include <dbghelp.h>
#endif

static void jl_load_sysimg_so(char *fname)
{
#ifndef _OS_WINDOWS_
    Dl_info dlinfo;
#endif
    // attempt to load the pre-compiled sysimg at fname
    // if this succeeds, sysimg_gvars will be a valid array
    // otherwise, it will be NULL
    jl_sysimg_handle = (uv_lib_t *) jl_load_dynamic_library_e(fname, JL_RTLD_DEFAULT | JL_RTLD_GLOBAL);
    if (jl_sysimg_handle != 0) {
        sysimg_gvars = (jl_value_t***)jl_dlsym(jl_sysimg_handle, "jl_sysimg_gvars");
        globalUnique = *(size_t*)jl_dlsym(jl_sysimg_handle, "jl_globalUnique");
        const char *cpu_target = (const char*)jl_dlsym(jl_sysimg_handle, "jl_sysimg_cpu_target");
        if (strcmp(cpu_target,jl_compileropts.cpu_target) != 0)
            jl_error("Julia and the system image were compiled for different architectures.\n"
                     "Please delete or regenerate sys.{so,dll,dylib}.\n");
        uint32_t info[4];
        jl_cpuid((int32_t*)info, 1);
        if (strcmp(cpu_target, "native") == 0) {
            uint64_t saved_cpuid = *(uint64_t*)jl_dlsym(jl_sysimg_handle, "jl_sysimg_cpu_cpuid");
            if (saved_cpuid != (((uint64_t)info[2])|(((uint64_t)info[3])<<32)))
                jl_error("Target architecture mismatch. Please delete or regenerate sys.{so,dll,dylib}.\n");
        }
        else if (strcmp(cpu_target,"core2") == 0) {
            int HasSSSE3 = (info[2] & 1<<9);
            if (!HasSSSE3)
                jl_error("The current host does not support SSSE3, but the system image was compiled for Core2.\n"
                         "Please delete or regenerate sys.{so,dll,dylib}.\n");
        }
#ifdef _OS_WINDOWS_
        jl_sysimage_base = (intptr_t)jl_sysimg_handle->handle;
#else
        if (dladdr((void*)sysimg_gvars, &dlinfo) != 0) {
            jl_sysimage_base = (intptr_t)dlinfo.dli_fbase;
        } else {
            jl_sysimage_base = 0;
        }
#endif
    }
    else {
        sysimg_gvars = 0;
    }
}

static jl_value_t *jl_deserialize_gv(ios_t *s, jl_value_t *v)
{
    // Restore the GlobalVariable reference to this jl_value_t via the sysimg_gvars table
    int32_t gvname_index = read_int32(s)-1;
    if (sysimg_gvars != NULL && gvname_index >= 0 && mode == MODE_SYSTEM_IMAGE) {
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
        jl_deserialize_gv(s, (jl_value_t*)backref_list.items[key]);
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
        jl_value_t *v = jl_deserialize_value(s, NULL);
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

static int is_submodule(jl_module_t *parent, jl_module_t *child)
{
    while (1) {
        if (parent == child)
            return 1;
        if (child == NULL || child == child->parent)
            return 0;
        child = child->parent;
    }
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
    int tag = 0;
    if (mode == MODE_MODULE_LAMBDAS) {
        if (dt->uid != 0)
            tag = 6; // must use apply_type
    }
    else if (mode == MODE_MODULE) {
        int internal = is_submodule(jl_current_module, dt->name->module);
        if (!internal && dt->name->primary == (jl_value_t*)dt) {
            tag = 6; // external primary type
        }
        else if (internal && dt->uid != 0) {
            tag = 5; // internal type (needs uid assigned later)
        }
        else if (dt->uid == 0) {
            tag = 0; // normal struct
        }
        else if (jl_is_null(dt->parameters)) {
            tag = 7; // external type that can be immediately recreated (with apply_type)
        }
        else { // anything else (external)
            if (jl_is_gf(dt))
                tag = 8; // external type function (needs uid assigned later, and env)
            else
                tag = 5; // external type (needs uid assigned later)
            // also flag this in the backref table as special
            uptrint_t *bp = (uptrint_t*)ptrhash_bp(&backref_table, dt);
            assert(*bp != (uptrint_t)HT_NOTFOUND);
            *bp |= 1;
        }
    }
    else if (dt == jl_int32_type)
        tag = 2;
    else if (dt == jl_bool_type)
        tag = 3;
    else if (dt == jl_int64_type)
        tag = 4;
    writetag(s, (jl_value_t*)jl_datatype_type);
    jl_serialize_value(s, (jl_value_t*)jl_datatype_type);
    write_uint8(s, tag);
    if (tag == 6) {
        jl_serialize_value(s, dt->name);
        return;
    }
    if (tag == 7) {
        jl_serialize_value(s, dt->name);
        jl_serialize_value(s, dt->parameters);
        return;
    }
    size_t nf = jl_tuple_len(dt->names);
    write_uint16(s, nf);
    write_int32(s, dt->size);
    int has_instance = !!(dt->instance != NULL);
    write_uint8(s, dt->abstract | (dt->mutabl<<1) | (dt->pointerfree<<2) | (has_instance<<3));
    if (!dt->abstract) {
        write_uint16(s, dt->ninitialized);
        if (mode != MODE_MODULE && mode != MODE_MODULE_LAMBDAS) {
            write_int32(s, dt->uid);
        }
    }
    if (has_instance)
        jl_serialize_value(s, dt->instance);
    if (nf > 0) {
        write_int32(s, dt->alignment);
        ios_write(s, (char*)&dt->fields[0], nf*sizeof(jl_fielddesc_t));
        jl_serialize_value(s, dt->names);
        jl_serialize_value(s, dt->types);
    }

    jl_serialize_value(s, dt->parameters);
    jl_serialize_value(s, dt->name);
    jl_serialize_value(s, dt->super);
}

static void jl_serialize_module(ios_t *s, jl_module_t *m)
{
    writetag(s, jl_module_type);
    jl_serialize_value(s, m->name);
    int ref_only = 0;
    if (mode == MODE_MODULE_LAMBDAS) {
        assert(!is_submodule(jl_current_module, m));
        ref_only = 1;
    }
    if (mode == MODE_MODULE) {
        if (!is_submodule(jl_current_module, m))
            ref_only = 1;
        write_int8(s, ref_only);
    }
    jl_serialize_value(s, m->parent);
    if (ref_only)
        return;
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m || m != jl_main_module) {
                jl_serialize_value(s, b->name);
                jl_serialize_value(s, b->value);
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

    if (mode == MODE_AST) {
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
        ptrint_t pos = backref_table_numel++;
        if (mode == MODE_MODULE || mode == MODE_MODULE_LAMBDAS)
            pos <<= 1;
        ptrhash_put(&backref_table, v, (void*)pos);
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
        if (mode == MODE_MODULE) {
            jl_serialize_value(s, jl_typeof(ar));
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
        jl_serialize_value(s, (jl_value_t*)li->unspecialized);
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
            if (v == t->instance) {
                assert(mode != MODE_MODULE_LAMBDAS);
                writetag(s, (jl_value_t*)Singleton_tag);
                return;
            }
            if ((jl_value_t*)t == jl_idtable_type)
                writetag(s, (jl_value_t*)IdTable_tag);
            else
                writetag(s, (jl_value_t*)jl_datatype_type);
            jl_serialize_value(s, t);
            if ((mode == MODE_MODULE || mode == MODE_MODULE_LAMBDAS) && t == jl_typename_type) {
                if (is_submodule(jl_current_module, ((jl_typename_t*)v)->module)) {
                    write_uint8(s, 0);
                }
                else {
                    write_uint8(s, 1);
                    jl_typename_t *tn = (jl_typename_t*)v;
                    jl_serialize_value(s, tn->module);
                    jl_serialize_value(s, tn->name);
                    return;
                }
            }
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
            if (mode == MODE_MODULE) {
                jl_serialize_value(s, t);
            }
        }
    }
}

void jl_serialize_methtable_from_mod(ios_t *s, jl_module_t *m, jl_sym_t *name, jl_methtable_t *mt, int8_t iskw)
{
    if (iskw) {
        if (!mt->kwsorter)
            return;
        assert(jl_is_gf(mt->kwsorter));
        mt = jl_gf_mtable(mt->kwsorter);
        assert(!mt->kwsorter);
    }
    //XXX: we are reversing the list of methods due to #8652
    struct _chain {
        jl_methlist_t *ml;
        struct _chain *next;
    } *chain = NULL;
    jl_methlist_t *ml = mt->defs;
    while (ml != JL_NULL) {
        if (is_submodule(jl_current_module, ml->func->linfo->module)) {
            struct _chain *link = (struct _chain*)alloca(sizeof(struct _chain));
            link->ml = ml;
            link->next = chain;
            chain = link;
        }
        ml = ml->next;
    }
    while (chain) {
        ml = chain->ml;
        jl_serialize_value(s, m);
        jl_serialize_value(s, name);
        write_int8(s, iskw);
        jl_serialize_value(s, ml->sig);
        jl_serialize_value(s, ml->func);
        if (jl_is_tuple(ml->tvars))
            jl_serialize_value(s, ml->tvars);
        else
            jl_serialize_value(s, jl_tuple1(ml->tvars));
        write_int8(s, ml->isstaged);
        chain = chain->next;
    }
}

void jl_serialize_lambdas_from_mod(ios_t *s, jl_module_t *m)
{
    if (m == jl_current_module) return;
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m && b->value) {
                if (jl_is_function(b->value)) {
                    jl_function_t *gf = (jl_function_t*)b->value;
                    if (jl_is_gf(gf)) {
                        jl_methtable_t *mt = jl_gf_mtable(gf);
                        jl_serialize_methtable_from_mod(s, m, b->name, mt, 0);
                        jl_serialize_methtable_from_mod(s, m, b->name, mt, 1);
                    }
                    //TODO: look in datatype cache?
                }
                else if (jl_is_module(b->value)) {
                    jl_module_t *child = (jl_module_t*)b->value;
                    if (child != m && child->parent == m && child->name == b->name) {
                        // this is the original/primary binding for the submodule
                        jl_serialize_lambdas_from_mod(s, (jl_module_t*)b->value);
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
    if (fptr < 2)
        return NULL;

    if (fptr >= sizeof(id_to_fptrs)/sizeof(*id_to_fptrs))
        jl_error("unknown function pointer ID");
    return id_to_fptrs[fptr];
}

static jl_value_t *jl_deserialize_datatype(ios_t *s, int pos, jl_value_t **loc)
{
    int tag = read_uint8(s);
    if (tag == 6 || tag == 7) {
        jl_typename_t *name = (jl_typename_t*)jl_deserialize_value(s, NULL);
        jl_value_t *dtv = name->primary;
        if (tag == 7) {
            jl_tuple_t *parameters = (jl_tuple_t*)jl_deserialize_value(s, NULL);
            dtv = jl_apply_type(dtv, parameters);
            backref_list.items[pos] = dtv;
        }
        return dtv;
    }
    uint16_t nf = read_uint16(s);
    size_t size = read_int32(s);
    uint8_t flags = read_uint8(s);
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
    dt->abstract = flags&1;
    dt->mutabl = (flags>>1)&1;
    dt->pointerfree = (flags>>2)&1;
    if (!dt->abstract) {
        dt->ninitialized = read_uint16(s);
        dt->uid = mode != MODE_MODULE && mode != MODE_MODULE_LAMBDAS ? read_int32(s) : 0;
    }
    else {
        dt->ninitialized = 0;
        dt->uid = 0;
    }
    int has_instance = (flags>>3)&1;
    if (has_instance) {
        dt->instance = jl_deserialize_value(s, &dt->instance);
        dt->instance->type = (jl_value_t*)dt;
    }
    assert(tree_literal_values==NULL && mode != MODE_AST);
    backref_list.items[pos] = dt;
    if (tag == 5 || tag == 8) {
        arraylist_push(&flagref_list, dt);
        arraylist_push(&flagref_list, loc);
        arraylist_push(&flagref_list, (void*)(uptrint_t)pos);
        if (has_instance) {
            arraylist_push(&flagref_list, dt);
            arraylist_push(&flagref_list, &dt->instance->type);
            arraylist_push(&flagref_list, (void*)(uptrint_t)-1);
        }
    }

    if (nf > 0) {
        dt->alignment = read_int32(s);
        ios_read(s, (char*)&dt->fields[0], nf*sizeof(jl_fielddesc_t));
        dt->names = (jl_tuple_t*)jl_deserialize_value(s, (jl_value_t**)&dt->names);
        dt->types = (jl_tuple_t*)jl_deserialize_value(s, (jl_value_t**)&dt->types);
    }
    else {
        dt->alignment = dt->size;
        if (dt->alignment > MAX_ALIGN)
            dt->alignment = MAX_ALIGN;
        dt->names = dt->types = jl_null;
    }
    dt->parameters = (jl_tuple_t*)jl_deserialize_value(s, (jl_value_t**)&dt->parameters);
    dt->name = (jl_typename_t*)jl_deserialize_value(s, (jl_value_t**)&dt->name);
    dt->super = (jl_datatype_t*)jl_deserialize_value(s, (jl_value_t**)&dt->super);
    if (datatype_list) {
        if (dt->name == jl_array_type->name || dt->name == jl_pointer_type->name ||
            dt->name == jl_type_type->name || dt->name == jl_vararg_type->name ||
            dt->name == jl_abstractarray_type->name ||
            dt->name == jl_densearray_type->name) {
            // builtin types are not serialized, so their caches aren't
            // explicitly saved. so we reconstruct the caches of builtin
            // parametric types here.
            jl_cell_1d_push(datatype_list, (jl_value_t*)dt);
        }
    }
    return (jl_value_t*)dt;
}

jl_array_t *jl_eqtable_put(jl_array_t *h, void *key, void *val);

static jl_value_t *jl_deserialize_value_(ios_t *s, jl_value_t *vtag, jl_value_t **loc);
static jl_value_t *jl_deserialize_value(ios_t *s, jl_value_t **loc)
{
    uint8_t tag = read_uint8(s);
    if (tag == Null_tag)
        return NULL;
    if (tag == 0) {
        tag = read_uint8(s);
        jl_value_t *v = deser_tag[tag];
        assert(v != NULL);
        return v;
    }
    if (tag == BackRef_tag || tag == ShortBackRef_tag) {
        assert(tree_literal_values == NULL && mode != MODE_AST);
        uptrint_t offs = (tag == BackRef_tag) ? read_int32(s) : read_uint16(s);
        int isdatatype = 0;
        if (mode == MODE_MODULE) {
            isdatatype = !!(offs & 1);
            offs >>= 1;
        }
        else if (mode == MODE_MODULE_LAMBDAS) {
            offs >>= 1;
        }
        assert(offs >= 0 && offs < backref_list.len);
        jl_value_t *bp = (jl_value_t*)backref_list.items[offs];
        assert(bp);
        if (isdatatype && loc != NULL) {
            arraylist_push(&flagref_list, bp);
            arraylist_push(&flagref_list, loc);
            arraylist_push(&flagref_list, (void*)(uptrint_t)-1);
        }
        return (jl_value_t*)bp;
    }

    jl_value_t *vtag = deser_tag[tag];
    if (tag >= VALUE_TAGS) {
        return vtag;
    }
    else if (vtag == (jl_value_t*)LiteralVal_tag) {
        return jl_cellref(tree_literal_values, read_uint16(s));
    }
    jl_value_t *v = jl_deserialize_value_(s, vtag, loc);
    return v;
}

static jl_value_t *jl_deserialize_value_(ios_t *s, jl_value_t *vtag, jl_value_t **loc)
{
    int usetable = (mode != MODE_AST);

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
            arraylist_push(&backref_list, (jl_value_t*)tu);
        jl_value_t **data = tu->data;
        for(i=0; i < len; i++) {
            data[i] = jl_deserialize_value(s, &data[i]);
        }
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
            arraylist_push(&backref_list, sym);
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
        int pos = backref_list.len;
        if (usetable)
            arraylist_push(&backref_list, NULL);
        jl_value_t *aty = jl_deserialize_value(s, NULL);
        size_t *dims = (size_t*)alloca(ndims*sizeof(size_t));
        for(i=0; i < ndims; i++)
            dims[i] = jl_unbox_long(jl_deserialize_value(s, NULL));
        jl_array_t *a = jl_new_array_for_deserialization((jl_value_t*)aty, ndims, dims, isunboxed, elsize);
        if (usetable)
            backref_list.items[pos] = a;
        if (!a->ptrarray) {
            size_t tot = jl_array_len(a) * a->elsize;
            ios_read(s, (char*)jl_array_data(a), tot);
        }
        else {
            jl_value_t** data = (jl_value_t**)jl_array_data(a);
            for(i=0; i < jl_array_len(a); i++) {
                data[i] = jl_deserialize_value(s, &data[i]);
            }
        }
        if (mode == MODE_MODULE) {
            aty = jl_deserialize_value(s, &jl_typeof(a));
            assert(aty == jl_typeof(a));
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
        int pos = backref_list.len;
        if (usetable)
            arraylist_push(&backref_list, NULL);
        jl_expr_t *e = jl_exprn((jl_sym_t*)jl_deserialize_value(s, NULL), len);
        if (usetable)
            backref_list.items[pos] = e;
        e->etype = jl_deserialize_value(s, &e->etype);
        jl_value_t **data = (jl_value_t**)(e->args->data);
        for(i=0; i < len; i++) {
            data[i] = jl_deserialize_value(s, &data[i]);
        }
        return (jl_value_t*)e;
    }
    else if (vtag == (jl_value_t*)jl_tvar_type) {
        jl_tvar_t *tv = (jl_tvar_t*)newobj((jl_value_t*)jl_tvar_type, 4);
        if (usetable)
            arraylist_push(&backref_list, tv);
        tv->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        tv->lb = jl_deserialize_value(s, &tv->lb);
        tv->ub = jl_deserialize_value(s, &tv->ub);
        tv->bound = read_int8(s);
        return (jl_value_t*)tv;
    }
    else if (vtag == (jl_value_t*)jl_function_type) {
        jl_function_t *f =
            (jl_function_t*)newobj((jl_value_t*)jl_function_type, 3);
        if (usetable)
            arraylist_push(&backref_list, f);
        f->linfo = (jl_lambda_info_t*)jl_deserialize_value(s, (jl_value_t**)&f->linfo);
        f->env = jl_deserialize_value(s, &f->env);
        f->fptr = jl_deserialize_fptr(s);
        return (jl_value_t*)f;
    }
    else if (vtag == (jl_value_t*)jl_lambda_info_type) {
        jl_lambda_info_t *li =
            (jl_lambda_info_t*)newobj((jl_value_t*)jl_lambda_info_type,
                                      LAMBDA_INFO_NW);
        if (usetable)
            arraylist_push(&backref_list, li);
        li->ast = jl_deserialize_value(s, &li->ast);
        li->sparams = (jl_tuple_t*)jl_deserialize_value(s, (jl_value_t**)&li->sparams);
        li->tfunc = jl_deserialize_value(s, (jl_value_t**)&li->tfunc);
        li->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        li->specTypes = (jl_tuple_t*)jl_deserialize_value(s, (jl_value_t**)&li->specTypes);
        li->specializations = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&li->specializations);
        li->inferred = read_int8(s);
        li->file = (jl_sym_t*)jl_deserialize_value(s, NULL);
        li->line = read_int32(s);
        li->module = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&li->module);
        li->roots = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&li->roots);
        li->def = (jl_lambda_info_t*)jl_deserialize_value(s, (jl_value_t**)&li->def);
        li->capt = jl_deserialize_value(s, &li->capt);
        li->fptr = &jl_trampoline;
        li->functionObject = NULL;
        li->cFunctionObject = NULL;
        li->inInference = 0;
        li->inCompile = 0;
        li->unspecialized = (jl_function_t*)jl_deserialize_value(s, (jl_value_t**)&li->unspecialized);
        li->functionID = 0;
        li->cFunctionID = 0;
        int32_t cfunc_llvm, func_llvm;
        func_llvm = read_int32(s);
        cfunc_llvm = read_int32(s);
        jl_delayed_fptrs(li, func_llvm, cfunc_llvm);
        return (jl_value_t*)li;
    }
    else if (vtag == (jl_value_t*)jl_module_type) {
        int pos = backref_list.len;
        if (usetable)
            arraylist_push(&backref_list, NULL);
        jl_sym_t *mname = (jl_sym_t*)jl_deserialize_value(s, NULL);
        int ref_only = 0;
        if (mode == MODE_MODULE_LAMBDAS) {
            ref_only = 1;
        }
        else if (mode == MODE_MODULE) {
            ref_only = read_uint8(s);
        }
        if (ref_only) {
            jl_value_t *m_ref = jl_get_global((jl_module_t*)jl_deserialize_value(s, NULL), mname);
            if (usetable)
                backref_list.items[pos] = m_ref;
            return m_ref;
        }
        jl_module_t *m = jl_new_module(mname);
        if (usetable)
            backref_list.items[pos] = m;
        m->parent = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&m->parent);
        while (1) {
            jl_sym_t *name = (jl_sym_t*)jl_deserialize_value(s, NULL);
            if (name == NULL)
                break;
            jl_binding_t *b = jl_get_binding_wr(m, name);
            b->value = jl_deserialize_value(s, &b->value);
            b->type = jl_deserialize_value(s, &b->type);
            b->owner = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&b->owner);
            int8_t flags = read_int8(s);
            b->constp = (flags>>2) & 1;
            b->exportp = (flags>>1) & 1;
            b->imported = (flags) & 1;
            jl_deserialize_gv(s, (jl_value_t*)b);
        }
        size_t i = m->usings.len;
        size_t ni = read_int32(s);
        arraylist_grow(&m->usings, ni);
        ni += i;
        while (i < ni) {
            m->usings.items[i] = jl_deserialize_value(s, (jl_value_t**)&m->usings.items[i]);
            i++;
        }
        m->constant_table = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&m->constant_table);
        return (jl_value_t*)m;
    }
    else if (vtag == (jl_value_t*)SmallInt64_tag) {
        jl_value_t *v = jl_box_int64(read_int32(s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    }
    else if (vtag == (jl_value_t*)Int32_tag) {
        jl_value_t *v = jl_box_int32(read_int32(s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    }
    else if (vtag == (jl_value_t*)jl_datatype_type || vtag == (jl_value_t*)IdTable_tag) {
        int pos = backref_list.len;
        if (usetable)
            arraylist_push(&backref_list, NULL);
        jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, NULL);
        if (dt == jl_datatype_type)
            return jl_deserialize_datatype(s, pos, loc);
        if ((mode == MODE_MODULE || mode == MODE_MODULE_LAMBDAS) && dt == jl_typename_type) {
            int ref_only = read_uint8(s);
            if (ref_only) {
                jl_module_t *m = (jl_module_t*)jl_deserialize_value(s, NULL);
                jl_sym_t *sym = (jl_sym_t*)jl_deserialize_value(s, NULL);
                jl_datatype_t *dt = (jl_datatype_t*)jl_get_global(m, sym);
                assert(jl_is_datatype(dt));
                jl_value_t *v = (jl_value_t*)dt->name;
                if (usetable)
                    backref_list.items[pos] = v;
                return v;
            }
        }
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
                backref_list.items[pos] = v;
        }
        else {
            v = jl_new_struct_uninit(dt);
            if (usetable)
               backref_list.items[pos] = v;
            if (vtag == (jl_value_t*)IdTable_tag) {
                jl_array_t *a = jl_alloc_cell_1d(32); //todo
                while (1) {
                    jl_value_t *val = jl_deserialize_value(s, NULL);
                    if (val == NULL)
                        break;
                    jl_value_t *key = jl_deserialize_value(s, NULL);
                    a = jl_eqtable_put(a, key, val);
                }
                jl_set_nth_field(v, 0, (jl_value_t*)a);
            }
            else {
                char *data = (char*)jl_data_ptr(v);
                for(i=0; i < nf; i++) {
                    jl_set_nth_field(v, i, jl_deserialize_value(s,
                        (dt->fields[i].isptr) ? (jl_value_t**)(data+jl_field_offset(dt, i)) : NULL));
                }
                if ((mode == MODE_MODULE || mode == MODE_MODULE_LAMBDAS) && jl_is_mtable(v))
                    arraylist_push(&methtable_list, v);
            }
        }
        // TODO: put WeakRefs on the weak_refs list
        if (mode == MODE_MODULE) {
            dt = (jl_datatype_t*)jl_deserialize_value(s, (jl_value_t**)&jl_typeof(v));
            assert((jl_value_t*)dt == jl_typeof(v));
        }
        return v;
    }
    else if (vtag == (jl_value_t*)Singleton_tag) {
        assert(mode != MODE_MODULE_LAMBDAS);
        jl_value_t *v = (jl_value_t*)allocobj(sizeof(void*));
        if (usetable) {
            uptrint_t pos = backref_list.len;
            arraylist_push(&backref_list, (void*)v);
            if (mode == MODE_MODULE) {
                assert(loc != NULL);
                arraylist_push(&flagref_list, v);
                arraylist_push(&flagref_list, loc);
                arraylist_push(&flagref_list, (void*)pos);
            }
        }
        return v;
    }
    assert(0);
    return NULL;
}

void jl_deserialize_lambdas_from_mod(ios_t *s)
{
    while (1) {
        jl_module_t *mod = (jl_module_t*)jl_deserialize_value(s, NULL);
        if (mod == NULL)
            return;
        jl_sym_t *name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        jl_function_t *gf = (jl_function_t*)jl_get_global(mod, name);
        int8_t iskw = read_int8(s);
        assert(jl_is_gf(gf));
        if (iskw) {
            if (!jl_gf_mtable(gf)->kwsorter) {
                jl_gf_mtable(gf)->kwsorter = jl_new_generic_function(jl_gf_name(gf));
            }
            gf = jl_gf_mtable(gf)->kwsorter;
            assert(jl_is_gf(gf));
        }
        jl_tuple_t *types = (jl_tuple_t*)jl_deserialize_value(s, NULL);
        jl_function_t *meth = (jl_function_t*)jl_deserialize_value(s, NULL);
        jl_tuple_t *tvars = (jl_tuple_t*)jl_deserialize_value(s, NULL);
        int8_t isstaged = read_int8(s);
        jl_add_method(gf, types, meth, tvars, isstaged);
    }
}

// --- entry points ---

extern jl_array_t *jl_module_init_order;

DLLEXPORT void jl_save_system_image(const char *fname)
{
    jl_gc_collect();
    jl_gc_collect();
    int en = jl_gc_is_enabled();
    jl_gc_disable();
    htable_reset(&backref_table, 250000);
    ios_t f;
    if (ios_file(&f, fname, 1, 1, 1, 1) == NULL) {
        JL_PRINTF(JL_STDERR, "Cannot open system image file \"%s\" for writing.\n", fname);
        exit(1);
    }

    // orphan old Base module if present
    jl_base_module = (jl_module_t*)jl_get_global(jl_main_module, jl_symbol("Base"));

    jl_idtable_type = jl_get_global(jl_base_module, jl_symbol("ObjectIdDict"));

    jl_serialize_value(&f, jl_main_module);

    // ensure everything in deser_tag is reassociated with its GlobalValue
    ptrint_t i=2;
    for (i=2; i < 255; i++) {
        jl_serialize_gv(&f, deser_tag[i]);
    }
    jl_serialize_globalvals(&f);
    jl_serialize_gv_syms(&f, jl_get_root_symbol()); // serialize symbols with GlobalValue references
    jl_serialize_value(&f, NULL); // signal the end of the symbols list

    // save module initialization order
    if (jl_module_init_order != NULL) {
        for(i=0; i < jl_array_len(jl_module_init_order); i++) {
            // verify that all these modules were saved
            assert(ptrhash_get(&backref_table, jl_cellref(jl_module_init_order, i)) != HT_NOTFOUND);
        }
    }
    jl_serialize_value(&f, jl_module_init_order);

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
extern void jl_get_uv_hooks();

// Takes in a path of the form "usr/lib/julia/sys.ji", such as passed in to jl_restore_system_image()
DLLEXPORT
const char * jl_get_system_image_cpu_target(const char *fname)
{
    // If passed NULL, don't even bother
    if (!fname)
        return NULL;

    // First, get "sys" from "sys.ji"
    char *fname_shlib = (char*)alloca(strlen(fname)+1);
    strcpy(fname_shlib, fname);
    char *fname_shlib_dot = strrchr(fname_shlib, '.');
    if (fname_shlib_dot != NULL)
        *fname_shlib_dot = 0;

    // Get handle to sys.so
    uv_lib_t * sysimg_handle = jl_load_dynamic_library_e(fname_shlib, JL_RTLD_DEFAULT | JL_RTLD_GLOBAL);

    // Return jl_sysimg_cpu_target if we can
    if (sysimg_handle)
        return (const char *)jl_dlsym(sysimg_handle, "jl_sysimg_cpu_target");

    // If something goes wrong, return NULL
    return NULL;
}

DLLEXPORT
void jl_restore_system_image(const char *fname)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL) {
        JL_PRINTF(JL_STDERR, "System image file \"%s\" not found\n", fname);
        exit(1);
    }
    int build_mode = 0;
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
    DUMP_MODES last_mode = mode;
    mode = MODE_SYSTEM_IMAGE;
    arraylist_new(&backref_list, 250000);

    datatype_list = jl_alloc_cell_1d(0);

    jl_main_module = (jl_module_t*)jl_deserialize_value(&f, NULL);
    jl_internal_main_module = jl_main_module;
    jl_core_module = (jl_module_t*)jl_get_global(jl_main_module,
                                                 jl_symbol("Core"));
    jl_base_module = (jl_module_t*)jl_get_global(jl_main_module,
                                                 jl_symbol("Base"));
    jl_current_module = jl_base_module; // run start_image in Base

    // ensure everything in deser_tag is reassociated with its GlobalValue
    ptrint_t i;
    for (i=2; i < 255; i++) {
        jl_deserialize_gv(&f, deser_tag[i]);
    }
    jl_deserialize_globalvals(&f);
    jl_deserialize_gv_syms(&f);

    jl_module_init_order = (jl_array_t*)jl_deserialize_value(&f, NULL);

    // cache builtin parametric types
    for(int i=0; i < jl_array_len(datatype_list); i++) {
        jl_value_t *v = jl_cellref(datatype_list, i);
        uint32_t uid = ((jl_datatype_t*)v)->uid;
        jl_cache_type_((jl_datatype_t*)v);
        ((jl_datatype_t*)v)->uid = uid;
    }
    datatype_list = NULL;

    jl_get_builtin_hooks();
    jl_get_system_hooks();
    jl_get_uv_hooks();
    jl_boot_file_loaded = 1;
    jl_typeinf_func = (jl_function_t*)jl_get_global(jl_base_module,
                                                    jl_symbol("typeinf_ext"));
    jl_init_box_caches();

    jl_set_t_uid_ctr(read_int32(&f));
    jl_set_gs_ctr(read_int32(&f));

    //ios_printf(ios_stderr, "backref_list.len = %d\n", backref_list.len);
    arraylist_free(&backref_list);
    ios_close(&f);

#ifdef JL_GC_MARKSWEEP
    if (en) jl_gc_enable();
#endif
    mode = last_mode;
    jl_update_all_fptrs();
}

void jl_init_restored_modules()
{
    if (jl_module_init_order != NULL) {
        jl_array_t *temp = jl_module_init_order;
        jl_module_init_order = NULL;
        JL_GC_PUSH1(&temp);
        int i;
        for(i=0; i < jl_array_len(temp); i++) {
            jl_value_t *mod = jl_cellref(temp, i);
            jl_module_run_initializer((jl_module_t*)mod);
        }
        jl_module_init_order = NULL;
        JL_GC_POP();
    }
}

DLLEXPORT
jl_value_t *jl_ast_rettype(jl_lambda_info_t *li, jl_value_t *ast)
{
    if (jl_is_expr(ast))
        return jl_lam_body((jl_expr_t*)ast)->etype;
    DUMP_MODES last_mode = mode;
    mode = MODE_AST;
    if (li->module->constant_table == NULL)
        li->module->constant_table = jl_alloc_cell_1d(0);
    tree_literal_values = li->module->constant_table;
    ios_t src;
    jl_array_t *bytes = (jl_array_t*)ast;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)bytes->data, jl_array_len(bytes), 0);
    src.size = jl_array_len(bytes);
    int en = jl_gc_is_enabled();
    jl_gc_disable();
    jl_value_t *rt = jl_deserialize_value(&src, NULL);
    if (en)
        jl_gc_enable();
    tree_literal_values = NULL;
    mode = last_mode;
    return rt;
}

DLLEXPORT
jl_value_t *jl_compress_ast(jl_lambda_info_t *li, jl_value_t *ast)
{
    DUMP_MODES last_mode = mode;
    mode = MODE_AST;
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
    mode = last_mode;
    return v;
}

DLLEXPORT
jl_value_t *jl_uncompress_ast(jl_lambda_info_t *li, jl_value_t *data)
{
    DUMP_MODES last_mode = mode;
    mode = MODE_AST;
    jl_array_t *bytes = (jl_array_t*)data;
    tree_literal_values = li->module->constant_table;
    ios_t src;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)bytes->data, jl_array_len(bytes), 0);
    src.size = jl_array_len(bytes);
    int en = jl_gc_is_enabled();
    jl_gc_disable();
    jl_gc_ephemeral_on();
    (void)jl_deserialize_value(&src, NULL); // skip ret type
    jl_value_t *v = jl_deserialize_value(&src, NULL);
    jl_gc_ephemeral_off();
    if (en)
        jl_gc_enable();
    tree_literal_values = NULL;
    mode = last_mode;
    return v;
}

DLLEXPORT
int jl_save_new_module(const char *fname, jl_module_t *mod)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 1, 1, 1) == NULL) {
        JL_PRINTF(JL_STDERR, "Cannot open cache file \"%s\" for writing.\n", fname);
        return 1;
    }
    htable_new(&backref_table, 5000);
    ptrhash_put(&backref_table, jl_main_module, (void*)(uintptr_t)0);

    int en = jl_gc_is_enabled();
    jl_gc_disable();
    jl_gc_ephemeral_on();
    DUMP_MODES last_mode = mode;
    mode = MODE_MODULE;
    jl_module_t *lastmod = jl_current_module;
    jl_current_module = mod;
    jl_serialize_value(&f, mod->parent);
    jl_serialize_value(&f, mod->name);
    jl_serialize_value(&f, mod);

    mode = MODE_MODULE_LAMBDAS;
    jl_serialize_lambdas_from_mod(&f, jl_main_module);
    jl_serialize_value(&f, NULL);

    jl_current_module = lastmod;
    mode = last_mode;
    jl_gc_ephemeral_off();
    if (en) jl_gc_enable();

    htable_reset(&backref_table, 0);
    ios_close(&f);

    return 0;
}

jl_function_t *jl_method_cache_insert(jl_methtable_t *mt, jl_tuple_t *type,
                                      jl_function_t *method);

DLLEXPORT
jl_module_t *jl_restore_new_module(const char *fname)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL) {
        JL_PRINTF(JL_STDERR, "Cache file \"%s\" not found\n", fname);
        return NULL;
    }
    if (ios_eof(&f)) {
        ios_close(&f);
        return NULL;
    }
    arraylist_new(&backref_list, 4000);
    arraylist_push(&backref_list, jl_main_module);
    arraylist_new(&flagref_list, 0);
    arraylist_new(&methtable_list, 0);

    int en = jl_gc_is_enabled();
    jl_gc_disable();
    DUMP_MODES last_mode = mode;
    mode = MODE_MODULE;
    jl_module_t *parent = (jl_module_t*)jl_deserialize_value(&f, NULL);
    jl_sym_t *name = (jl_sym_t*)jl_deserialize_value(&f, NULL);
    jl_binding_t *b = jl_get_binding_wr(parent, name);
    jl_declare_constant(b);
    if (b->value != NULL) {
        JL_PRINTF(JL_STDERR, "Warning: replacing module %s\n", name->name);
    }
    b->value = jl_deserialize_value(&f, &b->value);

    size_t i = 0;
    while (i < flagref_list.len) {
        jl_value_t *v, *o = (jl_value_t*)flagref_list.items[i++];
        jl_datatype_t *dt;
        if (jl_is_datatype(o)) {
            dt = (jl_datatype_t*)o;
            v = dt->instance;
        }
        else {
            dt = (jl_datatype_t*)jl_typeof(o);
            v = o;
        }
        jl_datatype_t *t = (jl_datatype_t*)jl_cache_type_(dt);
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i++];
        int offs = (int)(intptr_t)flagref_list.items[i++];
        if (t != dt) {
            jl_typeof(dt) = (jl_value_t*)(ptrint_t)2; // invalidate the old value to help catch errors
            if ((jl_value_t*)dt == o) {
                if (loc) *loc = (jl_value_t*)t;
                if (offs > 0) backref_list.items[offs] = t;
            }
        }
        if (t->instance != v) {
            jl_typeof(v) = (jl_value_t*)(ptrint_t)1; // invalidate the old value to help catch errors
            if (v == o) {
                if (loc) *loc = v;
                if (offs > 0) backref_list.items[offs] = v;
            }
        }
        size_t j = i;
        while (j < flagref_list.len) {
            if (flagref_list.items[j] == dt) {
                if (t != dt) {
                    jl_value_t **loc = (jl_value_t**)flagref_list.items[j+1];
                    int offs = (int)(intptr_t)flagref_list.items[j+2];
                    if (loc) *loc = (jl_value_t*)t;
                    if (offs > 0) backref_list.items[offs] = t;
                }
            }
            else if (flagref_list.items[j] == v) {
                if (t->instance != v) {
                    jl_value_t **loc = (jl_value_t**)flagref_list.items[j+1];
                    int offs = (int)(intptr_t)flagref_list.items[j+2];
                    if (loc) *loc = v;
                    if (offs > 0) backref_list.items[offs] = v;
                }
            }
            else {
                j += 3;
                continue;
            }
            flagref_list.len -= 3;
            if (j >= flagref_list.len)
                break;
            flagref_list.items[j+0] = flagref_list.items[flagref_list.len+0];
            flagref_list.items[j+1] = flagref_list.items[flagref_list.len+1];
            flagref_list.items[j+2] = flagref_list.items[flagref_list.len+2];
        }
    }

    mode = MODE_MODULE_LAMBDAS;
    jl_deserialize_lambdas_from_mod(&f);

    for (i = 0; i < methtable_list.len; i++) {
        jl_methtable_t *mt = (jl_methtable_t*)methtable_list.items[i];
        jl_array_t *cache_targ = mt->cache_targ;
        jl_array_t *cache_arg1 = mt->cache_arg1;
        mt->cache_targ = (jl_array_t*)JL_NULL;
        mt->cache_arg1 = (jl_array_t*)JL_NULL;
        if (cache_targ != JL_NULL) {
            size_t j, l = jl_array_len(cache_targ);
            for (j = 0; j < l; j++) {
                jl_methlist_t *ml = (jl_methlist_t*)jl_cellref(cache_targ, j);
                while (ml != NULL && ml != JL_NULL) {
                    assert(!ml->isstaged);
                    jl_method_cache_insert(mt, ml->sig, ml->func);
                    ml = ml->next;
                }
            }
        }
        if (cache_arg1 != JL_NULL) {
            size_t j, l = jl_array_len(cache_arg1);
            for (j = 0; j < l; j++) {
                jl_methlist_t *ml = (jl_methlist_t*)jl_cellref(cache_arg1, j);
                while (ml != NULL && ml != JL_NULL) {
                    assert(!ml->isstaged);
                    jl_method_cache_insert(mt, ml->sig, ml->func);
                    ml = ml->next;
                }
            }
        }
    }


    mode = last_mode;
    if (en) jl_gc_enable();
    arraylist_free(&flagref_list);
    arraylist_free(&methtable_list);
    arraylist_free(&backref_list);
    ios_close(&f);

    return (jl_module_t*)b->value;
}

// --- init ---

void jl_init_serializer(void)
{
    htable_new(&ser_tag, 0);
    htable_new(&fptr_to_id, sizeof(id_to_fptrs)/sizeof(*id_to_fptrs));
    htable_new(&backref_table, 0);

    void *tags[] = { jl_symbol_type, jl_datatype_type,
                     jl_function_type, jl_tuple_type, jl_array_type,
                     jl_expr_type, (void*)LongSymbol_tag, (void*)LongTuple_tag,
                     (void*)LongExpr_tag, (void*)LiteralVal_tag,
                     (void*)SmallInt64_tag, (void*)IdTable_tag,
                     (void*)Int32_tag, (void*)Array1d_tag, (void*)Singleton_tag,
                     jl_module_type, jl_tvar_type, jl_lambda_info_type,

                     jl_null, jl_false, jl_true, jl_nothing, jl_any_type,
                     jl_symbol("Any"), jl_symbol("Array"), jl_symbol("TypeVar"),
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
                     jl_symbol("getfield"), jl_symbol("setfield!"),
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
#endif
                     jl_labelnode_type, jl_linenumbernode_type,
                     jl_gotonode_type, jl_quotenode_type, jl_topnode_type,
                     jl_type_type, jl_bottom_type, jl_pointer_type,
                     jl_vararg_type, jl_ntuple_type, jl_abstractarray_type,
                     jl_densearray_type, jl_box_type, jl_void_type,
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
                     jl_densearray_type->name, jl_void_type->name,
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
        deser_tag[i] = (jl_value_t*)tags[i-2];
        i += 1;
    }
    assert(i <= Null_tag);
    VALUE_TAGS = (ptrint_t)ptrhash_get(&ser_tag, jl_null);

    i=2;
    while (id_to_fptrs[i] != NULL) {
        ptrhash_put(&fptr_to_id, (void*)id_to_fptrs[i], (void*)i);
        i += 1;
    }
}

#ifdef __cplusplus
}
#endif
