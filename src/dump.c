// This file is a part of Julia. License is MIT: http://julialang.org/license

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

#ifndef _COMPILER_MICROSOFT_
#include "valgrind.h"
#else
#define RUNNING_ON_VALGRIND 0
#endif

#ifdef __cplusplus
extern "C" {
#endif

// hash of definitions for predefined tagged object
static htable_t ser_tag;
// array of definitions for the predefined tagged object types
// (reverse of ser_tag)
static jl_value_t *deser_tag[256];
// hash of some common symbols, encoded as CommonSym_tag plus 1 byte
static htable_t common_symbol_tag;
static jl_value_t *deser_symbols[256];

// table of all objects that have been deserialized, indexed by pos
// (the order in the serializer stream) in MODE_MODULE, the low
// bit is reserved for flagging certain entries and pos is
// left shift by 1
static htable_t backref_table;
int backref_table_numel;
static arraylist_t backref_list;

// list of (jl_value_t **loc, size_t pos) entries
// for anything that was flagged by the serializer for later
// type-rewriting of some sort
static arraylist_t flagref_list;

// list of (size_t pos, (void *f)(jl_value_t*)) entries
// for the serializer to mark values in need of rework by function f
static arraylist_t reinit_list;

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
  jl_f_tuple, jl_f_svec,
  jl_f_get_field, jl_f_set_field, jl_f_field_type, jl_f_nfields,
  jl_f_arraylen, jl_f_arrayref,
  jl_f_arrayset, jl_f_arraysize,
  jl_f_instantiate_type, jl_f_kwcall,
  jl_trampoline, jl_f_union,
  jl_f_methodexists, jl_f_applicable,
  jl_f_invoke, jl_apply_generic,
  jl_unprotect_stack,
  jl_f_sizeof, jl_f_new_expr,
  NULL };

// pointers to non-AST-ish objects in a compressed tree
static jl_array_t *tree_literal_values=NULL; // (only used in MODE_AST)

static const ptrint_t LongSymbol_tag   = 23;
static const ptrint_t LongSvec_tag     = 24;
static const ptrint_t LongExpr_tag     = 25;
static const ptrint_t LiteralVal_tag   = 26;
static const ptrint_t SmallInt64_tag   = 27;
static const ptrint_t UNUSED_tag       = 28;
static const ptrint_t Int32_tag        = 29;
static const ptrint_t Array1d_tag      = 30;
static const ptrint_t Singleton_tag    = 31;
static const ptrint_t CommonSym_tag    = 32;
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

static void write_uint64(ios_t *s, uint64_t i)
{
    write_int32(s, i       & 0xffffffff);
    write_int32(s, (i>>32) & 0xffffffff);
}

static uint64_t read_uint64(ios_t *s)
{
    uint64_t b0 = (uint32_t)read_int32(s);
    uint64_t b1 = (uint32_t)read_int32(s);
    return b0 | (b1<<32);
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

#ifdef HAVE_CPUID
extern void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType);
#endif

extern int globalUnique;
uv_lib_t *jl_sysimg_handle = NULL;
uint64_t jl_sysimage_base = 0;
#ifdef _OS_WINDOWS_
#include <dbghelp.h>
#endif

DLLEXPORT int jl_running_on_valgrind()
{
    return RUNNING_ON_VALGRIND;
}

static void jl_load_sysimg_so()
{
#ifndef _OS_WINDOWS_
    Dl_info dlinfo;
#endif
    // attempt to load the pre-compiled sysimage from jl_sysimg_handle
    // if this succeeds, sysimg_gvars will be a valid array
    // otherwise, it will be NULL
    if (jl_sysimg_handle != 0) {
        sysimg_gvars = (jl_value_t***)jl_dlsym(jl_sysimg_handle, "jl_sysimg_gvars");
        globalUnique = *(size_t*)jl_dlsym(jl_sysimg_handle, "jl_globalUnique");
        const char *cpu_target = (const char*)jl_dlsym(jl_sysimg_handle, "jl_sysimg_cpu_target");
        if (strcmp(cpu_target,jl_options.cpu_target) != 0)
            jl_error("Julia and the system image were compiled for different architectures.\n"
                     "Please delete or regenerate sys.{so,dll,dylib}.\n");
#ifdef HAVE_CPUID
        uint32_t info[4];
        jl_cpuid((int32_t*)info, 1);
        if (strcmp(cpu_target, "native") == 0) {
            if (!RUNNING_ON_VALGRIND) {
                uint64_t saved_cpuid = *(uint64_t*)jl_dlsym(jl_sysimg_handle, "jl_sysimg_cpu_cpuid");
                if (saved_cpuid != (((uint64_t)info[2])|(((uint64_t)info[3])<<32)))
                    jl_error("Target architecture mismatch. Please delete or regenerate sys.{so,dll,dylib}.\n");
            }
        }
        else if (strcmp(cpu_target,"core2") == 0) {
            int HasSSSE3 = (info[2] & 1<<9);
            if (!HasSSSE3)
                jl_error("The current host does not support SSSE3, but the system image was compiled for Core2.\n"
                         "Please delete or regenerate sys.{so,dll,dylib}.\n");
        }
#endif

#ifdef _OS_WINDOWS_
        jl_sysimage_base = (intptr_t)jl_sysimg_handle->handle;
#else
        if (dladdr((void*)sysimg_gvars, &dlinfo) != 0) {
            jl_sysimage_base = (intptr_t)dlinfo.dli_fbase;
        }
        else {
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
    //jl_printf(JL_STDOUT, "delayed_fptrs_n: %d\n", delayed_fptrs_n);
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
    int tag = 0;
    if (mode == MODE_MODULE_LAMBDAS) {
        if (dt->uid != 0)
            tag = 6; // must use apply_type
    }
    else if (mode == MODE_MODULE) {
        int internal = jl_is_submodule(dt->name->module, jl_current_module);
        if (!internal && dt->name->primary == (jl_value_t*)dt) {
            tag = 6; // external primary type
        }
        else if (dt->uid == 0) {
            tag = 0; // normal struct
        }
        else if (!internal && jl_svec_len(dt->parameters) == 0) {
            tag = 7; // external type that can be immediately recreated (with apply_type)
        }
        else {
            tag = 5; // anything else (needs uid assigned later)
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
    size_t nf = jl_datatype_nfields(dt);
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
        assert(!jl_is_submodule(m, jl_current_module));
        ref_only = 1;
    }
    if (mode == MODE_MODULE) {
        if (!jl_is_submodule(m, jl_current_module))
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
    write_uint8(s, m->istopmod);
    write_uint64(s, m->uuid);
}

static int is_ast_node(jl_value_t *v)
{
    if (jl_is_lambda_info(v)) {
        jl_lambda_info_t *li = (jl_lambda_info_t*)v;
        if (jl_is_expr(li->ast)) {
            li->ast = jl_compress_ast(li, li->ast);
            gc_wb(li, li->ast);
        }
        return 0;
    }
    return jl_is_symbol(v) || jl_is_symbolnode(v) || jl_is_gensym(v) ||
        jl_is_expr(v) || jl_is_newvarnode(v) || jl_is_svec(v) ||
        jl_typeis(v, jl_array_any_type) || jl_is_tuple(v) ||
        jl_is_uniontype(v) || jl_is_int32(v) || jl_is_int64(v) ||
        jl_is_bool(v) || jl_is_typevar(v) ||
        jl_is_topnode(v) || jl_is_quotenode(v) || jl_is_gotonode(v) ||
        jl_is_labelnode(v) || jl_is_linenode(v) || jl_is_globalref(v);
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
    if (jl_is_symbol(v)) {
        void *idx = ptrhash_get(&common_symbol_tag, v);
        if (idx != HT_NOTFOUND) {
            writetag(s, (jl_value_t*)CommonSym_tag);
            write_uint8(s, (uint8_t)(size_t)idx);
            return;
        }
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
        if (jl_typeof(v) == jl_idtable_type) {
            arraylist_push(&reinit_list, (void*)pos);
            arraylist_push(&reinit_list, (void*)1);
        }
    }

    size_t i;
    if (jl_is_svec(v)) {
        size_t l = jl_svec_len(v);
        if (l <= 255) {
            writetag(s, jl_simplevector_type);
            write_uint8(s, (uint8_t)l);
        }
        else {
            writetag(s, (jl_value_t*)LongSvec_tag);
            write_int32(s, l);
        }
        for(i=0; i < l; i++) {
            jl_serialize_value(s, jl_svecref(v, i));
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
        if (li->module == jl_core_module) {
            jl_serialize_value(s, jl_nothing);
        }
        else {
            jl_array_t *tf = (jl_array_t*)li->tfunc;
            // go through the t-func cache, replacing ASTs with just return
            // types for abstract argument types. these ASTs are generally
            // not needed (e.g. they don't get inlined).
            if (tf && jl_typeis(tf, jl_array_any_type)) {
                size_t i, l = jl_array_len(tf);
                for(i=0; i < l; i += 3) {
                    if (!jl_is_leaf_type(jl_cellref(tf,i))) {
                        jl_value_t *ast = jl_cellref(tf,i+1);
                        if (ast && jl_is_array(ast) && jl_array_len(ast) > 500)
                            jl_cellset(tf, i+1, jl_ast_rettype(li, (jl_value_t*)ast));
                    }
                }
            }
            jl_serialize_value(s, (jl_value_t*)li->tfunc);
        }
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
        write_int32(s, li->specFunctionID);
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
            writetag(s, (jl_value_t*)jl_datatype_type);
            jl_serialize_value(s, t);
            if ((mode == MODE_MODULE || mode == MODE_MODULE_LAMBDAS) && t == jl_typename_type) {
                if (jl_is_submodule(((jl_typename_t*)v)->module, jl_current_module)) {
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
            size_t nf = jl_datatype_nfields(t);
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
                for(size_t i=0; i < nf; i++) {
                    jl_serialize_value(s, jl_get_nth_field(v, i));
                }
            }
            if (mode == MODE_MODULE) {
                jl_serialize_value(s, t);
            }
        }
    }
}

static void jl_serialize_methtable_from_mod(ios_t *s, jl_module_t *m, jl_sym_t *name, jl_methtable_t *mt, int8_t iskw)
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
    while (ml != (void*)jl_nothing) {
        if (jl_is_submodule(ml->func->linfo->module, jl_current_module)) {
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
        if (jl_is_svec(ml->tvars))
            jl_serialize_value(s, ml->tvars);
        else
            jl_serialize_value(s, jl_svec1(ml->tvars));
        write_int8(s, ml->isstaged);
        chain = chain->next;
    }
}

static void jl_serialize_lambdas_from_mod(ios_t *s, jl_module_t *m)
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

void jl_serialize_mod_list(ios_t *s)
{
    jl_module_t *m = jl_main_module;
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m &&
                    b->value &&
                    b->value != (jl_value_t*)jl_current_module &&
                    jl_is_module(b->value)) {
                jl_module_t *child = (jl_module_t*)b->value;
                if (child->name == b->name) {
                    // this is the original/primary binding for the submodule
                    size_t l = strlen(child->name->name);
                    write_int32(s, l);
                    ios_write(s, child->name->name, l);
                    write_uint64(s, child->uuid);
                }
            }
        }
    }
    write_int32(s, 0);
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
            jl_svec_t *parameters = (jl_svec_t*)jl_deserialize_value(s, NULL);
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
    dt->ditype = NULL;
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
        jl_set_typeof(dt->instance, dt);
    }
    assert(tree_literal_values==NULL && mode != MODE_AST);
    backref_list.items[pos] = dt;
    if (tag == 5) {
        arraylist_push(&flagref_list, loc);
        arraylist_push(&flagref_list, (void*)(uptrint_t)pos);
        if (has_instance) {
            arraylist_push(&flagref_list, &jl_astaggedvalue(dt->instance)->type);
            arraylist_push(&flagref_list, (void*)(uptrint_t)-1);
        }
    }

    if (nf > 0) {
        dt->alignment = read_int32(s);
        ios_read(s, (char*)&dt->fields[0], nf*sizeof(jl_fielddesc_t));
        dt->types = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&dt->types);
        gc_wb(dt, dt->types);
    }
    else {
        dt->alignment = dt->size;
        if (dt->alignment > MAX_ALIGN)
            dt->alignment = MAX_ALIGN;
        dt->types = jl_emptysvec;
    }
    dt->parameters = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&dt->parameters);
    gc_wb(dt, dt->parameters);
    dt->name = (jl_typename_t*)jl_deserialize_value(s, (jl_value_t**)&dt->name);
    gc_wb(dt, dt->name);
    dt->super = (jl_datatype_t*)jl_deserialize_value(s, (jl_value_t**)&dt->super);
    gc_wb(dt, dt->super);
    if (datatype_list) {
        if (dt->name == jl_array_type->name || dt->name == jl_ref_type->name ||
            dt->name == jl_pointer_type->name || dt->name == jl_type_type->name ||
            dt->name == jl_simplevector_type->name || dt->name == jl_abstractarray_type->name ||
            dt->name == jl_densearray_type->name || dt->name == jl_tuple_typename ||
            dt->name == jl_vararg_type->name) {
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
    if (vtag == (jl_value_t*)jl_simplevector_type ||
        vtag == (jl_value_t*)LongSvec_tag) {
        size_t len;
        if (vtag == (jl_value_t*)jl_simplevector_type)
            len = read_uint8(s);
        else
            len = read_int32(s);
        jl_svec_t *sv = jl_alloc_svec_uninit(len);
        if (usetable)
            arraylist_push(&backref_list, (jl_value_t*)sv);
        jl_value_t **data = sv->data;
        for(i=0; i < len; i++) {
            data[i] = jl_deserialize_value(s, &data[i]);
        }
        return (jl_value_t*)sv;
    }
    else if (vtag == (jl_value_t*)CommonSym_tag) {
        int tag = read_uint8(s);
        return deser_symbols[tag];
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
                if (data[i]) gc_wb(a, data[i]);
            }
        }
        if (mode == MODE_MODULE) {
            aty = jl_deserialize_value(s, &jl_astaggedvalue(a)->type);
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
        gc_wb(e, e->etype);
        jl_value_t **data = (jl_value_t**)(e->args->data);
        for(i=0; i < len; i++) {
            data[i] = jl_deserialize_value(s, &data[i]);
        }
        return (jl_value_t*)e;
    }
    else if (vtag == (jl_value_t*)jl_tvar_type) {
        jl_tvar_t *tv = (jl_tvar_t*)newobj((jl_value_t*)jl_tvar_type, NWORDS(sizeof(jl_tvar_t)));
        if (usetable)
            arraylist_push(&backref_list, tv);
        tv->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        gc_wb(tv, tv->name);
        tv->lb = jl_deserialize_value(s, &tv->lb);
        gc_wb(tv, tv->lb);
        tv->ub = jl_deserialize_value(s, &tv->ub);
        gc_wb(tv, tv->ub);
        tv->bound = read_int8(s);
        return (jl_value_t*)tv;
    }
    else if (vtag == (jl_value_t*)jl_function_type) {
        jl_function_t *f =
            (jl_function_t*)newobj((jl_value_t*)jl_function_type, NWORDS(sizeof(jl_function_t)));
        if (usetable)
            arraylist_push(&backref_list, f);
        f->linfo = (jl_lambda_info_t*)jl_deserialize_value(s, (jl_value_t**)&f->linfo);
        if (f->linfo != NULL) gc_wb(f, f->linfo);
        f->env = jl_deserialize_value(s, &f->env);
        gc_wb(f, f->env);
        f->fptr = jl_deserialize_fptr(s);
        return (jl_value_t*)f;
    }
    else if (vtag == (jl_value_t*)jl_lambda_info_type) {
        jl_lambda_info_t *li =
            (jl_lambda_info_t*)newobj((jl_value_t*)jl_lambda_info_type,
                                      NWORDS(sizeof(jl_lambda_info_t)));
        if (usetable)
            arraylist_push(&backref_list, li);
        li->ast = jl_deserialize_value(s, &li->ast);
        gc_wb(li, li->ast);
        li->sparams = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&li->sparams);
        gc_wb(li, li->sparams);
        li->tfunc = jl_deserialize_value(s, (jl_value_t**)&li->tfunc);
        gc_wb(li, li->tfunc);
        li->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        gc_wb(li, li->name);
        li->specTypes = (jl_tupletype_t*)jl_deserialize_value(s, (jl_value_t**)&li->specTypes);
        if (li->specTypes) gc_wb(li, li->specTypes);
        li->specializations = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&li->specializations);
        if (li->specializations) gc_wb(li, li->specializations);
        li->inferred = read_int8(s);
        li->file = (jl_sym_t*)jl_deserialize_value(s, NULL);
        gc_wb(li, li->file);
        li->line = read_int32(s);
        li->module = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&li->module);
        gc_wb(li, li->module);
        li->roots = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&li->roots);
        if (li->roots) gc_wb(li, li->roots);
        li->def = (jl_lambda_info_t*)jl_deserialize_value(s, (jl_value_t**)&li->def);
        gc_wb(li, li->def);
        li->capt = jl_deserialize_value(s, &li->capt);
        if (li->capt) gc_wb(li, li->capt);
        li->fptr = &jl_trampoline;
        li->functionObject = NULL;
        li->cFunctionList = NULL;
        li->specFunctionObject = NULL;
        li->inInference = 0;
        li->inCompile = 0;
        li->unspecialized = (jl_function_t*)jl_deserialize_value(s, (jl_value_t**)&li->unspecialized);
        li->functionID = 0;
        li->specFunctionID = 0;
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
        gc_wb(m, m->parent);

        while (1) {
            jl_sym_t *name = (jl_sym_t*)jl_deserialize_value(s, NULL);
            if (name == NULL)
                break;
            jl_binding_t *b = jl_get_binding_wr(m, name);
            b->value = jl_deserialize_value(s, &b->value);
            gc_wb_buf(m, b);
            if (b->value != NULL) gc_wb(m, b->value);
            b->type = jl_deserialize_value(s, &b->type);
            gc_wb(m, b->type);
            b->owner = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&b->owner);
            if (b->owner != NULL) gc_wb(m, b->owner);
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
        if (m->constant_table != NULL) gc_wb(m, m->constant_table);
        m->istopmod = read_uint8(s);
        m->uuid = read_uint64(s);
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
    else if (vtag == (jl_value_t*)jl_datatype_type) {
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
        size_t nf = jl_datatype_nfields(dt);
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
                    v = (jl_value_t*)allocobj(nby);
                    jl_set_typeof(v, dt);
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
            char *data = (char*)jl_data_ptr(v);
            for(i=0; i < nf; i++) {
                jl_set_nth_field(v, i, jl_deserialize_value(s,
                    (dt->fields[i].isptr) ? (jl_value_t**)(data+jl_field_offset(dt, i)) : NULL));
            }
            if ((mode == MODE_MODULE || mode == MODE_MODULE_LAMBDAS) && jl_is_mtable(v))
                arraylist_push(&methtable_list, v);
        }
        // TODO: put WeakRefs on the weak_refs list
        if (mode == MODE_MODULE) {
            dt = (jl_datatype_t*)jl_deserialize_value(s, &jl_astaggedvalue(v)->type);
            assert((jl_value_t*)dt == jl_typeof(v));
        }
        return v;
    }
    else if (vtag == (jl_value_t*)Singleton_tag) {
        assert(mode != MODE_MODULE_LAMBDAS);
        jl_value_t *v = (jl_value_t*)alloc_0w();
        if (usetable) {
            uptrint_t pos = backref_list.len;
            arraylist_push(&backref_list, (void*)v);
            if (mode == MODE_MODULE) {
                assert(loc != NULL);
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
        jl_tupletype_t *types = (jl_tupletype_t*)jl_deserialize_value(s, NULL);
        jl_function_t *meth = (jl_function_t*)jl_deserialize_value(s, NULL);
        jl_svec_t *tvars = (jl_svec_t*)jl_deserialize_value(s, NULL);
        int8_t isstaged = read_int8(s);
        jl_add_method(gf, types, meth, tvars, isstaged);
    }
}

int jl_deserialize_verify_mod_list(ios_t *s)
{
    while (1) {
        size_t len = read_int32(s);
        if (len == 0)
            return 1;
        char *name = (char*)alloca(len+1);
        ios_read(s, name, len);
        name[len] = '\0';
        uint64_t uuid = read_uint64(s);
        jl_module_t *m = (jl_module_t*)jl_get_global(jl_main_module, jl_symbol(name));
        if (!m) {
            jl_printf(JL_STDERR, "Module %s must be loaded first\n", name);
            return 0;
        }
        if (!jl_is_module(m)) {
            ios_close(s);
            jl_errorf("typeassert: expected %s::Module", name);
        }
        if (m->uuid != uuid) {
            jl_printf(JL_STDERR, "Module %s uuid did not match cache file\n", name);
            return 0;
        }
    }
}

// --- entry points ---

extern jl_array_t *jl_module_init_order;

DLLEXPORT void jl_save_system_image(const char *fname)
{
    jl_gc_collect(1);
    jl_gc_collect(0);
    int en = jl_gc_is_enabled();
    jl_gc_disable();
    htable_reset(&backref_table, 250000);
    arraylist_new(&reinit_list, 0);
    ios_t f;
    if (ios_file(&f, fname, 1, 1, 1, 1) == NULL) {
        jl_errorf("Cannot open system image file \"%s\" for writing.\n", fname);
    }

    // orphan old Base module if present
    jl_base_module = (jl_module_t*)jl_get_global(jl_main_module, jl_symbol("Base"));

    jl_idtable_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("ObjectIdDict")) : NULL;

    jl_serialize_value(&f, jl_main_module);
    jl_serialize_value(&f, jl_top_module);
    jl_serialize_value(&f, jl_typeinf_func);

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
        size_t i;
        for(i=0; i < jl_array_len(jl_module_init_order); i++) {
            // verify that all these modules were saved
            assert(ptrhash_get(&backref_table, jl_cellref(jl_module_init_order, i)) != HT_NOTFOUND);
        }
    }
    jl_serialize_value(&f, jl_module_init_order);

    write_int32(&f, jl_get_t_uid_ctr());
    write_int32(&f, jl_get_gs_ctr());

    // record reinitialization functions
    for (i = 0; i < reinit_list.len; i += 2) {
        write_int32(&f, (int)reinit_list.items[i]);
        write_int32(&f, (int)reinit_list.items[i+1]);
    }
    write_int32(&f, -1);

    htable_reset(&backref_table, 0);
    arraylist_free(&reinit_list);

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
void jl_preload_sysimg_so(const char *fname)
{
    // If passed NULL, don't even bother
    if (!fname)
        return;

    // First, get "sys" from "sys.ji"
    char *fname_shlib = (char*)alloca(strlen(fname)+1);
    strcpy(fname_shlib, fname);
    char *fname_shlib_dot = strrchr(fname_shlib, '.');
    if (fname_shlib_dot != NULL)
        *fname_shlib_dot = 0;

    // Get handle to sys.so
#ifdef _OS_WINDOWS_
    if (!jl_is_debugbuild()) {
#endif
        jl_sysimg_handle = (uv_lib_t*)jl_load_dynamic_library_e(fname_shlib, JL_RTLD_DEFAULT | JL_RTLD_GLOBAL);
#ifdef _OS_WINDOWS_
    }
#endif

    // set cpu target if unspecified by user and available from sysimg
    // otherwise default to native.
    if (jl_sysimg_handle && jl_options.cpu_target == NULL)
        jl_options.cpu_target = (const char *)jl_dlsym(jl_sysimg_handle, "jl_sysimg_cpu_target");
}

DLLEXPORT
void jl_restore_system_image(const char *fname)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL) {
        jl_errorf("System image file \"%s\" not found\n", fname);
    }
    int imaging_mode = jl_options.build_path != NULL;
#ifdef _OS_WINDOWS_
    //XXX: the windows linker forces our system image to be
    //     linked against only one dll, I picked libjulia-release
    if (jl_is_debugbuild()) imaging_mode = 1;
#endif
    if (!imaging_mode) {
        jl_load_sysimg_so();
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
    jl_top_module = (jl_module_t*)jl_deserialize_value(&f, NULL);
    jl_internal_main_module = jl_main_module;
    jl_typeinf_func = (jl_function_t*)jl_deserialize_value(&f, NULL);
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
    if (jl_base_module) {
        jl_get_system_hooks();
        jl_get_uv_hooks();
    }
    jl_boot_file_loaded = 1;
    jl_init_box_caches();

    jl_set_t_uid_ctr(read_int32(&f));
    jl_set_gs_ctr(read_int32(&f));

    // run reinitialization functions
    int pos = read_int32(&f);
    while (pos != -1) {
        jl_value_t *v = (jl_value_t*)backref_list.items[pos];
        switch (read_int32(&f)) {
            case 1: {
                jl_array_t **a = (jl_array_t**)&v->fieldptr[0];
                jl_idtable_rehash(a, jl_array_len(*a));
                gc_wb(v, *a);
                break;
                    }
            default:
                assert(0);
        }
        pos = read_int32(&f);
    }

    //jl_printf(JL_STDERR, "backref_list.len = %d\n", backref_list.len);
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

    if (li->module->constant_table == NULL) {
        li->module->constant_table = jl_alloc_cell_1d(0);
        gc_wb(li->module, li->module->constant_table);
    }
    tree_literal_values = li->module->constant_table;
    li->capt = (jl_value_t*)jl_lam_capt((jl_expr_t*)ast);
    gc_wb(li, li->capt);
    if (jl_array_len(li->capt) == 0)
        li->capt = NULL;
    jl_serialize_value(&dest, jl_lam_body((jl_expr_t*)ast)->etype);
    jl_serialize_value(&dest, ast);

    //jl_printf(JL_STDERR, "%d bytes, %d values\n", dest.size, vals->length);

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
    (void)jl_deserialize_value(&src, NULL); // skip ret type
    jl_value_t *v = jl_deserialize_value(&src, NULL);
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
        jl_printf(JL_STDERR, "Cannot open cache file \"%s\" for writing.\n", fname);
        return 1;
    }
    jl_module_t *lastmod = jl_current_module;
    jl_current_module = mod;
    jl_serialize_mod_list(&f);
    htable_new(&backref_table, 5000);
    ptrhash_put(&backref_table, jl_main_module, (void*)(uintptr_t)0);

    int en = jl_gc_is_enabled();
    jl_gc_disable();
    DUMP_MODES last_mode = mode;
    mode = MODE_MODULE;
    jl_serialize_value(&f, mod->parent);
    jl_serialize_value(&f, mod->name);
    jl_serialize_value(&f, mod);

    mode = MODE_MODULE_LAMBDAS;
    jl_serialize_lambdas_from_mod(&f, jl_main_module);
    jl_serialize_value(&f, NULL);

    // save module initialization order
    if (jl_module_init_order != NULL) {
        size_t i;
        for(i=0; i < jl_array_len(jl_module_init_order); i++) {
            // verify that all these modules were saved
            assert(ptrhash_get(&backref_table, jl_cellref(jl_module_init_order, i)) != HT_NOTFOUND);
        }
    }
    jl_serialize_value(&f, jl_module_init_order);

    jl_current_module = lastmod;
    mode = last_mode;
    if (en) jl_gc_enable();

    htable_reset(&backref_table, 0);
    ios_close(&f);

    return 0;
}

jl_function_t *jl_method_cache_insert(jl_methtable_t *mt, jl_tupletype_t *type,
                                      jl_function_t *method);

DLLEXPORT
jl_module_t *jl_restore_new_module(const char *fname)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL) {
        jl_printf(JL_STDERR, "Cache file \"%s\" not found\n", fname);
        return NULL;
    }
    if (ios_eof(&f)) {
        ios_close(&f);
        return NULL;
    }
    if (!jl_deserialize_verify_mod_list(&f)) {
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
        jl_printf(JL_STDERR, "Warning: replacing module %s\n", name->name);
    }
    b->value = jl_deserialize_value(&f, &b->value);

    size_t i = 0;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i++];
        jl_value_t *v, *o = *loc;
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
        int offs = (int)(intptr_t)flagref_list.items[i++];
        if (t != dt) {
            jl_set_typeof(dt, (jl_value_t*)(ptrint_t)6); // invalidate the old value to help catch errors
            if ((jl_value_t*)dt == o) {
                if (loc) *loc = (jl_value_t*)t;
                if (offs > 0) backref_list.items[offs] = t;
            }
        }
        if (t->instance != v) {
            jl_set_typeof(v, (jl_value_t*)(ptrint_t)4); // invalidate the old value to help catch errors
            if (v == o) {
                if (loc) *loc = v;
                if (offs > 0) backref_list.items[offs] = v;
            }
        }
        size_t j = i;
        while (j < flagref_list.len) {
            if (flagref_list.items[j] == dt) {
                if (t != dt) {
                    jl_value_t **loc = (jl_value_t**)flagref_list.items[j];
                    int offs = (int)(intptr_t)flagref_list.items[j+1];
                    if (loc) *loc = (jl_value_t*)t;
                    if (offs > 0) backref_list.items[offs] = t;
                }
            }
            else if (flagref_list.items[j] == v) {
                if (t->instance != v) {
                    jl_value_t **loc = (jl_value_t**)flagref_list.items[j];
                    int offs = (int)(intptr_t)flagref_list.items[j+1];
                    if (loc) *loc = v;
                    if (offs > 0) backref_list.items[offs] = v;
                }
            }
            else {
                j += 2;
                continue;
            }
            flagref_list.len -= 2;
            if (j >= flagref_list.len)
                break;
            flagref_list.items[j+0] = flagref_list.items[flagref_list.len+0];
            flagref_list.items[j+1] = flagref_list.items[flagref_list.len+1];
        }
    }

    mode = MODE_MODULE_LAMBDAS;
    jl_deserialize_lambdas_from_mod(&f);

    jl_module_init_order = (jl_array_t*)jl_deserialize_value(&f, NULL);

    for (i = 0; i < methtable_list.len; i++) {
        jl_methtable_t *mt = (jl_methtable_t*)methtable_list.items[i];
        jl_array_t *cache_targ = mt->cache_targ;
        jl_array_t *cache_arg1 = mt->cache_arg1;
        mt->cache_targ = (void*)jl_nothing;
        mt->cache_arg1 = (void*)jl_nothing;
        if (cache_targ != (void*)jl_nothing) {
            size_t j, l = jl_array_len(cache_targ);
            for (j = 0; j < l; j++) {
                jl_methlist_t *ml = (jl_methlist_t*)jl_cellref(cache_targ, j);
                while (ml != NULL && ml != (void*)jl_nothing) {
                    assert(!ml->isstaged);
                    jl_method_cache_insert(mt, ml->sig, ml->func);
                    ml = ml->next;
                }
            }
        }
        if (cache_arg1 != (void*)jl_nothing) {
            size_t j, l = jl_array_len(cache_arg1);
            for (j = 0; j < l; j++) {
                jl_methlist_t *ml = (jl_methlist_t*)jl_cellref(cache_arg1, j);
                while (ml != NULL && ml != (void*)jl_nothing) {
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

    jl_init_restored_modules();

    return (jl_module_t*)b->value;
}

// --- init ---

void jl_init_serializer(void)
{
    htable_new(&ser_tag, 0);
    htable_new(&common_symbol_tag, 0);
    htable_new(&fptr_to_id, sizeof(id_to_fptrs)/sizeof(*id_to_fptrs));
    htable_new(&backref_table, 0);

    void *tags[] = { jl_symbol_type, jl_gensym_type, jl_datatype_type,
                     jl_function_type, jl_simplevector_type, jl_array_type,
                     jl_expr_type, (void*)LongSymbol_tag, (void*)LongSvec_tag,
                     (void*)LongExpr_tag, (void*)LiteralVal_tag,
                     (void*)SmallInt64_tag, (void*)UNUSED_tag,
                     (void*)Int32_tag, (void*)Array1d_tag, (void*)Singleton_tag,
                     jl_module_type, jl_tvar_type, jl_lambda_info_type,
                     (void*)CommonSym_tag,

                     jl_emptysvec, jl_emptytuple, jl_false, jl_true, jl_nothing, jl_any_type,
                     call_sym, goto_ifnot_sym, return_sym, body_sym, line_sym,
                     lambda_sym, tuple_sym, assign_sym,

                     // empirical list of very common symbols
                     jl_symbol("getfield"),            jl_symbol("Float64"),
                     jl_symbol("apply_type"),          jl_symbol("size"),
                     jl_symbol("Any"),                 jl_symbol("sub_int"),
                     jl_symbol("colon"),
                     jl_symbol("call1"),               jl_symbol("keys"),
                     jl_symbol("Bool"),                jl_symbol("string"),
                     jl_symbol("not_int"),             jl_symbol("zext_int"),
                     jl_symbol("getindex"),            jl_symbol("Float32"),
                     jl_symbol("ccall"),               jl_symbol("jl_alloc_array_1d"),
                     jl_symbol("Void"),                jl_symbol("typeassert"),
                     jl_symbol("add_int"),             jl_symbol("arrayset"),
                     jl_symbol("start"),               jl_symbol("expr"),
                     jl_symbol("BlasInt"),             jl_symbol("Intrinsics"),
                     jl_symbol("unsafe_convert"),      jl_symbol("select_value"),
                     jl_symbol("slt_int"),             jl_symbol("copyast"),
                     jl_symbol("Array"),               jl_symbol("sext_int"),
                     jl_symbol("Base"),                jl_symbol("print_to_string"),
                     jl_symbol("UInt8"),               jl_symbol("ArgumentError"),
                     jl_symbol("Int32"),               jl_symbol("setfield!"),
                     jl_symbol("convert"),             jl_symbol("mul_int"),
                     jl_symbol("done"),                jl_symbol("and_int"),
                     jl_symbol("index"),               jl_symbol("check_top_bit"),
                     jl_symbol("cconvert"),            jl_symbol("nothing"),
                     jl_symbol("arrayref"),            jl_symbol("typeof"),
                     jl_symbol("indexed_next"),        jl_symbol("sizeof"),
                     jl_symbol("throw"),               jl_symbol("macrocall"),
                     jl_symbol("length"),              jl_symbol("first"),
                     jl_symbol("arraylen"),            jl_symbol("slt_int"),
                     jl_symbol("next"),                jl_symbol("BoundsError"),
                     jl_symbol("boundscheck"),         jl_symbol("AbstractString"),
                     jl_symbol("setindex!"),           jl_symbol("bytestring"),
                     jl_symbol("promote_type"),        jl_symbol("error"),
                     jl_symbol("checked_trunc_sint"),  jl_symbol("ptr_arg_unsafe_convert"),
                     jl_symbol("ptr_arg_cconvert"),

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
                     jl_box_int32(51), jl_box_int32(52),
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
                     jl_box_int64(51), jl_box_int64(52),
#endif
                     jl_labelnode_type, jl_linenumbernode_type,
                     jl_gotonode_type, jl_quotenode_type, jl_topnode_type,
                     jl_type_type, jl_bottom_type, jl_ref_type, jl_pointer_type,
                     jl_vararg_type, jl_ntuple_type, jl_abstractarray_type,
                     jl_densearray_type, jl_box_type, jl_void_type,
                     jl_typector_type, jl_typename_type,
                     jl_task_type, jl_uniontype_type, jl_typetype_type, jl_typetype_tvar,
                     jl_ANY_flag, jl_array_any_type, jl_intrinsic_type, jl_method_type,
                     jl_methtable_type, jl_voidpointer_type, jl_newvarnode_type,
                     jl_array_symbol_type, jl_anytuple_type, jl_tparam0(jl_anytuple_type),

                     jl_symbol_type->name, jl_gensym_type->name, jl_tuple_typename,
                     jl_ref_type->name, jl_pointer_type->name, jl_simplevector_type->name,
                     jl_datatype_type->name, jl_uniontype_type->name, jl_array_type->name,
                     jl_expr_type->name, jl_typename_type->name, jl_type_type->name,
                     jl_methtable_type->name, jl_method_type->name, jl_tvar_type->name,
                     jl_ntuple_type->name, jl_abstractarray_type->name, jl_vararg_type->name,
                     jl_densearray_type->name, jl_void_type->name, jl_lambda_info_type->name,
                     jl_module_type->name, jl_box_type->name, jl_function_type->name,
                     jl_typector_type->name, jl_intrinsic_type->name, jl_task_type->name,
                     jl_labelnode_type->name, jl_linenumbernode_type->name,
                     jl_gotonode_type->name, jl_quotenode_type->name, jl_topnode_type->name,

                     jl_root_task, jl_bottom_func,

                     NULL };

    // more common symbols, less common than those above. will get 2-byte encodings.
    void *common_symbols[] = {
        jl_symbol("Cint"),                  jl_symbol("jl_forceclose_uv"),
        jl_symbol("#s295"),                 jl_symbol("env"),
        jl_symbol("#s294"),                 jl_symbol("leave"),
        jl_symbol("box"),                   jl_symbol("terminal"),
        jl_symbol("#s293"),                 jl_symbol("_sizeof_uv_process"),
        jl_symbol("#s78"),                  jl_symbol("jl_spawn"),
        jl_symbol("#s292"),                 jl_symbol("termbuf"),
        jl_symbol("Ptr"),                   jl_symbol("trunc_int"),
        jl_symbol("#s289"),                 jl_symbol("promotion.jl"),
        jl_symbol("#s290"),                 jl_symbol("#s246"),
        jl_symbol("#s291"),                 jl_symbol("inference.jl"),
        jl_symbol("args"),                  jl_symbol("resize!"),
        jl_symbol("proc"),                  jl_symbol("ashr_int"),
        jl_symbol("tuple.jl"),              jl_symbol("UInt64"),
        jl_symbol("#s77"),                  jl_symbol("result"),
        jl_symbol("#s288"),                 jl_symbol("lshr_int"),
        jl_symbol("#s287"),                 jl_symbol("Int64"),
        jl_symbol("sle_int"),               jl_symbol("#s280"),
        jl_symbol("info"),                  jl_symbol("base"),
        jl_symbol("stop"),                  jl_symbol("IOBuffer"),
        jl_symbol("_var0"),                 jl_symbol("#s279"),
        jl_symbol("#s76"),                  jl_symbol("UInt32"),
        jl_symbol("#s286"),                 jl_symbol("rest"),
        jl_symbol("_expr"),                 jl_symbol("region"),
        jl_symbol("#s285"),                 jl_symbol("reduce.jl"),
        jl_symbol("Vararg"),                jl_symbol("offset"),
        jl_symbol("count"),                 jl_symbol("flags"),
        jl_symbol("vals"),                  jl_symbol("catdims"),
        jl_symbol("#s32"),                  jl_symbol("copy"),
        jl_symbol("#s20"),                  jl_symbol("#s278"),
        jl_symbol("base.jl"),               jl_symbol("nzval"),
        jl_symbol("endof"),                 jl_symbol("argtypes"),
        jl_symbol("#s284"),                 jl_symbol("show.jl"),
        jl_symbol("loop"),                  jl_symbol("mode"),
        jl_symbol("_apply"),                jl_symbol("Char"),
        jl_symbol("argv"),                  jl_symbol("linalg/lapack.jl"),
        jl_symbol("C_NULL"),                jl_symbol("rowval"),
        jl_symbol("dict.jl"),               jl_symbol("UV_STREAM"),
        jl_symbol("handle"),                jl_symbol("ht_keyindex"),
        jl_symbol("array.jl"),              jl_symbol("Symbol"),
        jl_symbol("operators.jl"),          jl_symbol("hash"),
        jl_symbol("string.jl"),             jl_symbol("real"),
        jl_symbol("DevNull"),               jl_symbol("float.jl"),
        jl_symbol("push!"),                 jl_symbol("stdios"),
        jl_symbol("#s282"),                 jl_symbol("multi.jl"),
        jl_symbol("Complex128"),            jl_symbol("_var1"),
        jl_symbol("Complex64"),             jl_symbol("#s18"),
        jl_symbol("#s281"),                 jl_symbol("neg_int"),
        jl_symbol("this"),                  jl_symbol("width"),
        jl_symbol("iter"),                  jl_symbol("status"),
        jl_symbol("slots"),                 jl_symbol("liblapack"),
        jl_symbol("AsyncStream"),           jl_symbol("ndel"),
        jl_symbol("#s31"),                  jl_symbol("cols"),
        jl_symbol("#s283"),                 jl_symbol("deprecated.jl"),
        jl_symbol("#s75"),                  jl_symbol("doffs"),
        jl_symbol("jl_array_ptr"),          jl_symbol("UTF8String"),
        jl_symbol("#s13"),                  jl_symbol("trans"),
        jl_symbol("write"),                 jl_symbol("depwarn"),
        jl_symbol("key"),                   jl_symbol("cur_row"),
        jl_symbol("Csize_t"),               jl_symbol("soffs"),
        jl_symbol("process.jl"),            jl_symbol("prefix"),
        jl_symbol("dest"),                  jl_symbol("rehash!"),
        jl_symbol("#s30"),                  jl_symbol("finalizer"),
        jl_symbol("print"),                 jl_symbol("#s17"),
        jl_symbol("state"),                 jl_symbol("colptr"),
        jl_symbol("BigFloat"),              jl_symbol("reshape"),
        jl_symbol("avail"),                 jl_symbol("sparse/sparsematrix.jl"),
        jl_symbol("int.jl"),                jl_symbol("skip_deleted"),
        jl_symbol("#s74"),                  jl_symbol("bitarray.jl"),
        jl_symbol("uplo"),                  jl_symbol("noinline"),
        jl_symbol("work"),                  jl_symbol("multidimensional.jl"),
        jl_symbol("BigInt"),                jl_symbol("stream.jl"),
        jl_symbol("UInt"),                  jl_symbol("ByteString"),
        jl_symbol("dims"),                  jl_symbol("sock"),
        jl_symbol("head"),                  jl_symbol("nargs"),
        jl_symbol("#s19"),                  jl_symbol("linalg/triangular.jl"),
        jl_symbol("#s60"),                  jl_symbol("uplo"),
        jl_symbol("lwork"),                 jl_symbol("#s277"),
        jl_symbol("abstractarray.jl"),      jl_symbol("config"),
        jl_symbol("path"),                  jl_symbol("#83#d"),
        jl_symbol("newsz"),                 jl_symbol("jl_array_grow_end"),
        jl_symbol("kwcall"),                jl_symbol("shl_int"),
        jl_symbol("stride"),                jl_symbol("#s284"),
        jl_symbol("block"),                 jl_symbol("chunks"),
        jl_symbol("#s73"),                  jl_symbol("checked_sadd"),
        jl_symbol("blas_int"),              jl_symbol("Function"),
        jl_symbol("or_int"),                jl_symbol("#s245"),
        jl_symbol("similar"),               jl_symbol("multimedia.jl"),
        jl_symbol("joinpath"),
        jl_symbol("stream"),                jl_symbol("diag"),
        jl_symbol("value"),                 jl_symbol("olds"),
        jl_symbol("item"),                  jl_symbol("checked_ssub"),
        jl_symbol("name"),                  jl_symbol("ndimsC"),
        jl_symbol("fieldtype"),             jl_symbol("chkstride1"),
        jl_symbol("uvtype"),                jl_symbol("vect"),
        jl_symbol("uvhandle"),              jl_symbol("mpfr.jl"),
        jl_symbol("step"),                  jl_symbol("last"),
        jl_symbol("LineEdit.jl"),           jl_symbol("kwerr"),
        jl_symbol("maxprobe"),              jl_symbol("ht_keyindex2"),
        jl_symbol("UVError"),               jl_symbol("repl"),
        jl_symbol("dict"),                  jl_symbol("cmd_gen"),
        jl_symbol("unbox"),                 jl_symbol("ROUNDING_MODE"),
        jl_symbol("#s72"),                  jl_symbol("keymap"),
        jl_symbol("Core"),                  jl_symbol("checked_trunc_uint"),
        jl_symbol("meta"),                  jl_symbol("eval"),
        jl_symbol("DimensionMismatch"),     jl_symbol("splitter"),
        jl_symbol("indent"),                jl_symbol("func"),
        jl_symbol("#s247"),                 jl_symbol("delta"),
        jl_symbol("ult_int"),               jl_symbol("alpha"),
        jl_symbol("buffer"),                jl_symbol("LAPACKException"),
        jl_symbol("Condition"),             jl_symbol("Expr"),
        jl_symbol("limit"),                 jl_symbol("disassociate_julia_struct"),
        jl_symbol("malloc"),                jl_symbol("zero"),
        jl_symbol("isempty"),               jl_symbol("detach"),
        jl_symbol("UV_RAW_FD"),             jl_symbol("jl_uv_associate_julia_struct"),
        NULL
    };

    ptrint_t i=2;
    while (tags[i-2] != NULL) {
        ptrhash_put(&ser_tag, tags[i-2], (void*)i);
        deser_tag[i] = (jl_value_t*)tags[i-2];
        i += 1;
    }
    assert(i <= Null_tag);
    VALUE_TAGS = (ptrint_t)ptrhash_get(&ser_tag, jl_emptysvec);

    i=2;
    while (id_to_fptrs[i] != NULL) {
        ptrhash_put(&fptr_to_id, (void*)id_to_fptrs[i], (void*)i);
        i += 1;
    }

    i=2;
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
