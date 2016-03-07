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

JL_DEFINE_MUTEX(dump)

// TODO: put WeakRefs on the weak_refs list during deserialization
// TODO: handle finalizers

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
// for anything that was flagged by the deserializer for later
// type-rewriting of some sort
static arraylist_t flagref_list;

// list of (size_t pos, (void *f)(jl_value_t*)) entries
// for the serializer to mark values in need of rework by function f
// during deserialization later
static arraylist_t reinit_list;

// list of any methtable objects that were deserialized in MODE_MODULE
// and need to be rehashed after assigning the uid fields to types
// (only used in MODE_MODULE and MODE_MODULE_POSTWORK)
static arraylist_t methtable_list;

// list of stuff that is being serialized
// (only used by the incremental serializer in MODE_MODULE)
static jl_array_t *serializer_worklist;

// hash of definitions for predefined function pointers
static htable_t fptr_to_id;
// array of definitions for the predefined function pointers
// (reverse of fptr_to_id)
static const jl_fptr_t id_to_fptrs[] = {
  NULL, NULL,
  jl_f_throw, jl_f_is, jl_f_typeof, jl_f_issubtype, jl_f_isa,
  jl_f_typeassert, jl_f__apply, jl_f_isdefined, jl_f_tuple, jl_f_svec,
  jl_f_getfield, jl_f_setfield, jl_f_fieldtype, jl_f_nfields,
  jl_f_arrayref, jl_f_arrayset, jl_f_arraysize, jl_f_apply_type,
  jl_f_applicable, jl_f_invoke, jl_unprotect_stack, jl_f_sizeof, jl_f__expr,
  jl_f_intrinsic_call,
  NULL };

// pointers to non-AST-ish objects in a compressed tree
static jl_array_t *tree_literal_values=NULL;    // (only used in MODE_AST)
static jl_module_t *tree_enclosing_module=NULL; // (only used in MODE_AST)

static const intptr_t LongSymbol_tag   = 23;
static const intptr_t LongSvec_tag     = 24;
static const intptr_t LongExpr_tag     = 25;
static const intptr_t LiteralVal_tag   = 26;
static const intptr_t SmallInt64_tag   = 27;
static const intptr_t SmallDataType_tag= 28;
static const intptr_t Int32_tag        = 29;
static const intptr_t Array1d_tag      = 30;
static const intptr_t Singleton_tag    = 31;
static const intptr_t CommonSym_tag    = 32;
static const intptr_t NearbyGlobal_tag = 33;  // a GlobalRef pointing to tree_enclosing_module
static const intptr_t Null_tag         = 253;
static const intptr_t ShortBackRef_tag = 254;
static const intptr_t BackRef_tag      = 255;

static intptr_t VALUE_TAGS;

typedef enum _DUMP_MODES {
    // not in the serializer at all, or
    // something is seriously wrong
    MODE_INVALID = 0,

    // jl_uncompress_ast
    // compressing / decompressing an AST Expr in a LambdaInfo
    MODE_AST,

    // jl_restore_system_image
    // restoring an entire system image from disk
    MODE_SYSTEM_IMAGE,

    // jl_restore_new_module
    // restoring a single module from disk for integration
    // into the currently running system image / environment
    MODE_MODULE, // first-stage (pre type-uid assignment)
    MODE_MODULE_POSTWORK, // second-stage (post type-uid assignment)
} DUMP_MODES;
static DUMP_MODES mode = (DUMP_MODES) 0;

static jl_value_t *jl_idtable_type=NULL;

// queue of types to cache
static jl_array_t *datatype_list=NULL; // (only used in MODE_SYSTEM_IMAGE)

#define write_uint8(s, n) ios_putc((n), (s))
#define read_uint8(s) ((uint8_t)ios_getc(s))
#define write_int8(s, n) write_uint8(s, n)
#define read_int8(s) read_uint8(s)

/* read and write in network (bigendian) order: */

static void write_int32(ios_t *s, int32_t i)
{
    write_uint8(s, (i>>24) & 0xff);
    write_uint8(s, (i>>16) & 0xff);
    write_uint8(s, (i>> 8) & 0xff);
    write_uint8(s, i       & 0xff);
}

static int32_t read_int32(ios_t *s)
{
    int b3 = read_uint8(s);
    int b2 = read_uint8(s);
    int b1 = read_uint8(s);
    int b0 = read_uint8(s);
    return b0 | (b1<<8) | (b2<<16) | (b3<<24);
}

static void write_uint64(ios_t *s, uint64_t i)
{
    write_int32(s, (i>>32) & 0xffffffff);
    write_int32(s, i       & 0xffffffff);
}

static uint64_t read_uint64(ios_t *s)
{
    uint64_t b1 = (uint32_t)read_int32(s);
    uint64_t b0 = (uint32_t)read_int32(s);
    return b0 | (b1<<32);
}

static void write_uint16(ios_t *s, uint16_t i)
{
    write_uint8(s, (i>> 8) & 0xff);
    write_uint8(s, i       & 0xff);
}

static uint16_t read_uint16(ios_t *s)
{
    int b1 = read_uint8(s);
    int b0 = read_uint8(s);
    return b0 | (b1<<8);
}

static void writetag(ios_t *s, void *v)
{
    write_uint8(s, (uint8_t)(intptr_t)ptrhash_get(&ser_tag, v));
}

static void write_as_tag(ios_t *s, uint8_t tag)
{
    if (tag < VALUE_TAGS) {
        write_uint8(s, 0);
    }
    write_uint8(s, tag);
}

static void write_float64(ios_t *s, double x)
{
    write_uint64(s, *((uint64_t*)&x));
}

// --- Static Compile ---

#define jl_serialize_value(s, v) jl_serialize_value_(s,(jl_value_t*)(v))
static void jl_serialize_value_(ios_t *s, jl_value_t *v);
static jl_value_t *jl_deserialize_value(ios_t *s, jl_value_t **loc);
static jl_value_t ***sysimg_gvars = NULL;
static void **sysimg_fvars = NULL;

#ifdef HAVE_CPUID
extern void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType);
#endif

extern int globalUnique;
static void *jl_sysimg_handle = NULL;
static uint64_t sysimage_base = 0;

JL_DLLEXPORT int jl_running_on_valgrind(void)
{
    return RUNNING_ON_VALGRIND;
}

static int jl_load_sysimg_so(void)
{
#ifndef _OS_WINDOWS_
    Dl_info dlinfo;
#endif
    // attempt to load the pre-compiled sysimage from jl_sysimg_handle
    if (jl_sysimg_handle == 0)
        return -1;

    int imaging_mode = jl_generating_output() && !jl_options.incremental;
    // in --build mode only use sysimg data, not precompiled native code
    if (!imaging_mode && jl_options.use_precompiled==JL_OPTIONS_USE_PRECOMPILED_YES) {
        sysimg_gvars = (jl_value_t***)jl_dlsym(jl_sysimg_handle, "jl_sysimg_gvars");
        sysimg_fvars = (void**)jl_dlsym(jl_sysimg_handle, "jl_sysimg_fvars");
        globalUnique = *(size_t*)jl_dlsym(jl_sysimg_handle, "jl_globalUnique");
#ifdef JULIA_ENABLE_THREADING
        size_t tls_getter_idx = *(size_t*)jl_dlsym(jl_sysimg_handle,
                                                   "jl_ptls_states_getter_idx");
        *sysimg_gvars[tls_getter_idx - 1] =
            (jl_value_t*)jl_get_ptls_states_getter();
#endif
        const char *cpu_target = (const char*)jl_dlsym(jl_sysimg_handle, "jl_sysimg_cpu_target");
        if (strcmp(cpu_target,jl_options.cpu_target) != 0)
            jl_error("Julia and the system image were compiled for different architectures.\n"
                     "Please delete or regenerate sys.{so,dll,dylib}.");
#ifdef HAVE_CPUID
        uint32_t info[4];
        jl_cpuid((int32_t*)info, 1);
        if (strcmp(cpu_target, "native") == 0) {
            if (!RUNNING_ON_VALGRIND) {
                uint64_t saved_cpuid = *(uint64_t*)jl_dlsym(jl_sysimg_handle, "jl_sysimg_cpu_cpuid");
                if (saved_cpuid != (((uint64_t)info[2])|(((uint64_t)info[3])<<32)))
                    jl_error("Target architecture mismatch. Please delete or regenerate sys.{so,dll,dylib}.");
            }
        }
        else if (strcmp(cpu_target,"core2") == 0) {
            int HasSSSE3 = (info[2] & 1<<9);
            if (!HasSSSE3)
                jl_error("The current host does not support SSSE3, but the system image was compiled for Core2.\n"
                         "Please delete or regenerate sys.{so,dll,dylib}.");
        }
#endif

#ifdef _OS_WINDOWS_
        sysimage_base = (intptr_t)jl_sysimg_handle;
#else
        if (dladdr((void*)sysimg_gvars, &dlinfo) != 0) {
            sysimage_base = (intptr_t)dlinfo.dli_fbase;
        }
        else {
            sysimage_base = 0;
        }
#endif
    }
    const char *sysimg_data = (const char*)jl_dlsym_e(jl_sysimg_handle, "jl_system_image_data");
    if (sysimg_data) {
        size_t len = *(size_t*)jl_dlsym(jl_sysimg_handle, "jl_system_image_size");
        jl_restore_system_image_data(sysimg_data, len);
        return 0;
    }
    return -1;
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
        char *offs = (char*)p[i+1];
        if (offs != HT_NOTFOUND) {
            uintptr_t pos = offs - (char*)HT_NOTFOUND - 1;
            int32_t gv = jl_get_llvm_gv((jl_value_t*)p[i]);
            if (gv != 0) {
                write_int32(s, pos + 1);
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
        jl_deserialize_gv(s, (jl_value_t*)backref_list.items[key - 1]);
    }
}

static void jl_serialize_gv_syms(ios_t *s, jl_sym_t *v)
{
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

static void jl_serialize_gv_others(ios_t *s)
{
    // ensures all objects referenced in the code have
    // references in the system image to their global variable
    // since codegen knows that some integer boxes are static,
    // they might not have had a reference anywhere in the code
    // image other than here
    int32_t i;
    for (i = -512; i < 512; i++) {
        jl_value_t *v32 = jl_box_int32(i);
        void *bp32 = ptrhash_get(&backref_table, v32);
        if (bp32 == HT_NOTFOUND) {
            int32_t gv32 = jl_get_llvm_gv(v32);
            if (gv32 != 0) {
                jl_serialize_value(s, v32);
                write_int32(s, gv32);
            }
        }
    }
    for (i = -512; i < 512; i++) {
        jl_value_t *v64 = jl_box_int64(i);
        void *bp64 = ptrhash_get(&backref_table, v64);
        if (bp64 == HT_NOTFOUND) {
            int32_t gv64 = jl_get_llvm_gv(v64);
            if (gv64 != 0) {
                jl_serialize_value(s, v64);
                write_int32(s, gv64);
            }
        }
    }
    jl_serialize_gv_syms(s, jl_get_root_symbol());
    jl_serialize_value(s, NULL); // signal the end of this list
}

static void jl_deserialize_gv_others(ios_t *s)
{
    while (1) {
        jl_value_t *v = jl_deserialize_value(s, NULL);
        if (!v) break;
        jl_deserialize_gv(s, v);
    }
}

static struct delayed_fptrs_t {
    jl_lambda_info_t *li;
    int32_t func;
    int32_t cfunc;
} *delayed_fptrs = NULL;
static size_t delayed_fptrs_n = 0;
static size_t delayed_fptrs_max = 0;
static size_t sysimg_fvars_max = 0;

static void jl_delayed_fptrs(jl_lambda_info_t *li, int32_t func, int32_t cfunc)
{
    // can't restore the fptrs until after the system image is fully restored,
    // since it will try to decompress the function AST to determine the argument types
    if (cfunc || func) {
        if (delayed_fptrs_max < delayed_fptrs_n + 1) {
            if (delayed_fptrs_max == 0)
                // current measurements put the number of functions at 4508
                delayed_fptrs_max = 4096;
            else
                delayed_fptrs_max *= 2;
            delayed_fptrs = (struct delayed_fptrs_t*)realloc(delayed_fptrs, delayed_fptrs_max*sizeof(delayed_fptrs[0])); //assumes sizeof==alignof
        }
        delayed_fptrs[delayed_fptrs_n].li = li;
        delayed_fptrs[delayed_fptrs_n].func = func;
        delayed_fptrs[delayed_fptrs_n].cfunc = cfunc;
        delayed_fptrs_n++;
        if (func > 0 && func > sysimg_fvars_max)
            sysimg_fvars_max = func;
        if (cfunc > 0 && cfunc > sysimg_fvars_max)
            sysimg_fvars_max = cfunc;
    }
}

void jl_register_fptrs(uint64_t sysimage_base, void **fptrs, jl_lambda_info_t **linfos, size_t n);

static void jl_update_all_fptrs(void)
{
    //jl_printf(JL_STDOUT, "delayed_fptrs_n: %d\n", delayed_fptrs_n);
    void **fvars = sysimg_fvars;
    if (fvars == 0) return;
    // jl_fptr_to_llvm needs to decompress some ASTs, therefore this needs to be NULL
    // to skip trying to restore GlobalVariable pointers in jl_deserialize_gv
    sysimg_gvars = NULL;
    sysimg_fvars = NULL;
    size_t i;
    jl_lambda_info_t **linfos = (jl_lambda_info_t**)malloc(sizeof(jl_lambda_info_t*) * sysimg_fvars_max);
    for (i = 0; i < delayed_fptrs_n; i++) {
        jl_lambda_info_t *li = delayed_fptrs[i].li;
        int32_t func = delayed_fptrs[i].func - 1;
        if (func >= 0) {
            jl_fptr_to_llvm((jl_fptr_t)fvars[func], li, 0);
            linfos[func] = li;
        }
        int32_t cfunc = delayed_fptrs[i].cfunc - 1;
        if (cfunc >= 0) {
            jl_fptr_to_llvm((jl_fptr_t)fvars[cfunc], li, 1);
            linfos[cfunc] = li;
        }
    }
    jl_register_fptrs(sysimage_base, fvars, linfos, sysimg_fvars_max);
    delayed_fptrs_n = 0;
    delayed_fptrs_max = 0;
    sysimg_fvars_max = 0;
    free(delayed_fptrs);
    delayed_fptrs = NULL;
}

// --- serialize ---

static void jl_serialize_fptr(ios_t *s, void *fptr)
{
    void **pbp = ptrhash_bp(&fptr_to_id, fptr);
    if (*pbp == HT_NOTFOUND || fptr == NULL)
        write_uint16(s, 1);
    else
        write_uint16(s, *(intptr_t*)pbp);
}

static int module_in_worklist(jl_module_t *mod) {
    int i, l = jl_array_len(serializer_worklist);
    for (i = 0; i < l; i++) {
        jl_module_t *workmod = (jl_module_t*)jl_cellref(serializer_worklist, i);
        if (jl_is_module(workmod) && jl_is_submodule(mod, workmod))
            return 1;
    }
    return 0;
}

static void jl_serialize_datatype(ios_t *s, jl_datatype_t *dt)
{
    int tag = 0;
    if (mode == MODE_MODULE_POSTWORK) {
        if (dt->uid != 0) {
            if (dt->name->primary == (jl_value_t*)dt)
                tag = 6; // primary type
            else
                tag = 7; // must use apply_type
        }
    }
    else if (mode == MODE_MODULE) {
        int internal = module_in_worklist(dt->name->module);
        int i, l = jl_array_len(serializer_worklist);
        for (i = 0; i < l; i++) {
            jl_module_t *mod = (jl_module_t*)jl_cellref(serializer_worklist, i);
            if (jl_is_module(mod) && jl_is_submodule(dt->name->module, mod)) {
                internal = 1;
                break;
            }
        }
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
            if (!internal) {
                // also flag this in the backref table as special
                uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, dt);
                assert(*bp != (uintptr_t)HT_NOTFOUND);
                *bp |= 1; assert(((uintptr_t)HT_NOTFOUND)|1);
            }
        }
    }
    else if (dt == jl_int32_type)
        tag = 2;
    else if (dt == jl_bool_type)
        tag = 3;
    else if (dt == jl_int64_type)
        tag = 4;
    writetag(s, (jl_value_t*)SmallDataType_tag);
    write_uint8(s, 0); // virtual size
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
    write_int8(s, dt->fielddesc_type);
    if (!dt->abstract) {
        write_uint16(s, dt->ninitialized);
        if (mode != MODE_MODULE && mode != MODE_MODULE_POSTWORK) {
            write_int32(s, dt->uid);
        }
    }
    if (has_instance)
        jl_serialize_value(s, dt->instance);
    if (nf > 0) {
        write_int32(s, dt->alignment);
        write_int8(s, dt->haspadding);
        size_t fieldsize = jl_fielddesc_size(dt->fielddesc_type);
        ios_write(s, jl_datatype_fields(dt), nf * fieldsize);
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
    if (mode == MODE_MODULE_POSTWORK) {
        assert(!module_in_worklist(m));
        ref_only = 1;
    }
    if (mode == MODE_MODULE) {
        if (!module_in_worklist(m))
            ref_only = 1;
        write_int8(s, ref_only);
    }
    jl_serialize_value(s, m->parent);
    if (ref_only) {
        assert(m->parent != m);
        return;
    }
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m || m != jl_main_module) {
                jl_serialize_value(s, b->name);
                jl_serialize_value(s, b->value);
                jl_serialize_value(s, b->globalref);
                jl_serialize_value(s, b->owner);
                write_int8(s, (b->deprecated<<3) | (b->constp<<2) | (b->exportp<<1) | (b->imported));
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
    write_uint8(s, m->istopmod);
    write_uint8(s, m->std_imports);
    write_uint64(s, m->uuid);
    write_int32(s, m->counter);
}

static int is_ast_node(jl_value_t *v)
{
    if (jl_is_lambda_info(v)) {
        jl_lambda_info_t *li = (jl_lambda_info_t*)v;
        if (jl_is_expr(li->ast)) {
            li->ast = jl_compress_ast(li, li->ast);
            jl_gc_wb(li, li->ast);
        }
        return 0;
    }
    return jl_is_symbol(v) || jl_is_slot(v) || jl_is_gensym(v) ||
        jl_is_expr(v) || jl_is_newvarnode(v) || jl_is_svec(v) ||
        jl_typeis(v, jl_array_any_type) || jl_is_tuple(v) ||
        jl_is_uniontype(v) || jl_is_int32(v) || jl_is_int64(v) ||
        jl_is_bool(v) ||
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
        write_as_tag(s, (uint8_t)(intptr_t)*bp);
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
            int id = literal_val_id(v);
            assert(id >= 0 && id < UINT16_MAX);
            write_uint16(s, id);
            return;
        }
    }
    else {
        bp = ptrhash_bp(&backref_table, v);
        if (*bp != HT_NOTFOUND) {
            uintptr_t pos = (char*)*bp - (char*)HT_NOTFOUND - 1;
            if (pos < 65536) {
                write_uint8(s, ShortBackRef_tag);
                write_uint16(s, pos);
            }
            else {
                write_uint8(s, BackRef_tag);
                write_int32(s, pos);
            }
            return;
        }
        intptr_t pos = backref_table_numel++;
        if (jl_typeof(v) == jl_idtable_type) {
            // will need to rehash this, later (after types are fully constructed)
            arraylist_push(&reinit_list, (void*)pos);
            arraylist_push(&reinit_list, (void*)1);
        }
        if (mode == MODE_MODULE && jl_is_module(v)) {
            jl_module_t *m = (jl_module_t*)v;
            if (module_in_worklist(m) && !module_in_worklist(m->parent)) {
                // will need to reinsert this into parent bindings, later (in case of any errors during reinsert)
                arraylist_push(&reinit_list, (void*)pos);
                arraylist_push(&reinit_list, (void*)2);
            }
        }
        if (mode == MODE_MODULE || mode == MODE_MODULE_POSTWORK)
            pos <<= 1;
        ptrhash_put(&backref_table, v, (char*)HT_NOTFOUND + pos + 1);
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
        size_t l = strlen(jl_symbol_name((jl_sym_t*)v));
        if (l <= 255) {
            writetag(s, jl_symbol_type);
            write_uint8(s, (uint8_t)l);
        }
        else {
            writetag(s, (jl_value_t*)LongSymbol_tag);
            write_int32(s, l);
        }
        ios_write(s, jl_symbol_name((jl_sym_t*)v), l);
    }
    else if (jl_is_globalref(v)) {
        if (mode == MODE_AST && jl_globalref_mod(v) == tree_enclosing_module) {
            writetag(s, (jl_value_t*)NearbyGlobal_tag);
            jl_serialize_value(s, jl_globalref_name(v));
        }
        else {
            writetag(s, (jl_value_t*)jl_globalref_type);
            jl_serialize_value(s, jl_globalref_mod(v));
            jl_serialize_value(s, jl_globalref_name(v));
        }
    }
    else if (jl_is_array(v)) {
        jl_array_t *ar = (jl_array_t*)v;
        if (ar->flags.ndims == 1 && ar->elsize < 128) {
            writetag(s, (jl_value_t*)Array1d_tag);
            write_uint8(s, (ar->flags.ptrarray<<7) | (ar->elsize & 0x7f));
        }
        else {
            writetag(s, (jl_value_t*)jl_array_type);
            write_uint16(s, ar->flags.ndims);
            write_uint16(s, (ar->flags.ptrarray<<15) | (ar->elsize & 0x7fff));
        }
        for (i=0; i < ar->flags.ndims; i++)
            jl_serialize_value(s, jl_box_long(jl_array_dim(ar,i)));
        jl_serialize_value(s, jl_typeof(ar));
        if (!ar->flags.ptrarray) {
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
    else if (jl_is_lambda_info(v)) {
        writetag(s, jl_lambda_info_type);
        jl_lambda_info_t *li = (jl_lambda_info_t*)v;
        jl_serialize_value(s, li->ast);
        jl_serialize_value(s, li->rettype);
        jl_serialize_value(s, (jl_value_t*)li->sparam_syms);
        jl_serialize_value(s, (jl_value_t*)li->sparam_vals);
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
                        jl_value_t *ret = jl_cellref(tf,i+1);
                        if (jl_is_tuple(ret)) {
                            jl_value_t *ast = jl_fieldref(ret, 0);
                            if (jl_is_array(ast) && jl_array_len(ast) > 500)
                                jl_cellset(tf, i+1, jl_fieldref(ret,1));
                        }
                    }
                }
            }
            jl_serialize_value(s, (jl_value_t*)li->tfunc);
        }
        jl_serialize_value(s, (jl_value_t*)li->name);
        jl_serialize_value(s, (jl_value_t*)li->specTypes);
        jl_serialize_value(s, (jl_value_t*)li->specializations);
        write_int8(s, li->inferred);
        write_int8(s, li->pure);
        write_int8(s, li->called);
        jl_serialize_value(s, (jl_value_t*)li->file);
        write_int32(s, li->line);
        write_int32(s, li->nslots);
        write_int32(s, li->ngensym);
        jl_serialize_value(s, (jl_value_t*)li->module);
        jl_serialize_value(s, (jl_value_t*)li->roots);
        jl_serialize_value(s, (jl_value_t*)li->def);
        jl_serialize_value(s, (jl_value_t*)li->unspecialized);
        jl_serialize_fptr(s, li->fptr);
        // save functionObject pointers
        write_int32(s, li->functionID);
        write_int32(s, li->specFunctionID);
        if (li->functionID)
            write_int8(s, li->jlcall_api);
        write_int8(s, li->needs_sparam_vals_ducttape);
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
                if (mode == MODE_MODULE) {
                    // also flag this in the backref table as special
                    uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, v);
                    assert(*bp != (uintptr_t)HT_NOTFOUND);
                    *bp |= 1; assert(((uintptr_t)HT_NOTFOUND)|1);
                }
                writetag(s, (jl_value_t*)Singleton_tag);
                jl_serialize_value(s, t);
                return;
            }
            if (t->size <= 255) {
                writetag(s, (jl_value_t*)SmallDataType_tag);
                write_uint8(s, t->size);
            }
            else {
                writetag(s, (jl_value_t*)jl_datatype_type);
                write_int32(s, t->size);
            }
            jl_serialize_value(s, t);
            if ((mode == MODE_MODULE || mode == MODE_MODULE_POSTWORK) && t == jl_typename_type) {
                if (module_in_worklist(((jl_typename_t*)v)->module)) {
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
                    if (jl_field_size(t, i) > 0) {
                        jl_serialize_value(s, jl_get_nth_field(v, i));
                    }
                }
            }
        }
    }
}

static void jl_serialize_methtable_from_mod(ios_t *s, jl_methtable_t *mt, int8_t iskw)
{
    jl_sym_t *name = mt->name;
    if (iskw) {
        if (!mt->kwsorter)
            return;
        assert(mt->module == jl_gf_mtable(mt->kwsorter)->module);
        mt = jl_gf_mtable(mt->kwsorter);
        assert(!mt->kwsorter);
    }
    assert(mt->module);
    jl_methlist_t *ml = mt->defs;
    while (ml != (void*)jl_nothing) {
        if (module_in_worklist(ml->func->module)) {
            jl_serialize_value(s, mt->module);
            jl_serialize_value(s, name);
            write_int8(s, iskw);
            jl_serialize_value(s, ml->sig);
            jl_serialize_value(s, ml->func);
            jl_serialize_value(s, ml->tvars);
            write_int8(s, ml->isstaged);
        }
        ml = ml->next;
    }
}

static void jl_serialize_lambdas_from_mod(ios_t *s, jl_module_t *m)
{
    if (module_in_worklist(m)) return;
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m && b->value && b->constp) {
                if (jl_is_module(b->value)) {
                    jl_module_t *child = (jl_module_t*)b->value;
                    if (child != m && child->parent == m && child->name == b->name) {
                        // this is the original/primary binding for the submodule
                        jl_serialize_lambdas_from_mod(s, (jl_module_t*)b->value);
                    }
                }
                else {
                    jl_methtable_t *mt = jl_gf_mtable(b->value);
                    if (mt && mt->name == b->name && mt->module == m) {
                        jl_serialize_methtable_from_mod(s, mt, 0);
                        jl_serialize_methtable_from_mod(s, mt, 1);
                    }
                }
            }
        }
    }
}

// serialize information about all of the modules accessible directly from Main
static void jl_serialize_mod_list(ios_t *s)
{
    jl_module_t *m = jl_main_module;
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m &&
                    b->value && b->constp &&
                    jl_is_module(b->value) &&
                    !module_in_worklist((jl_module_t*)b->value)) {
                jl_module_t *child = (jl_module_t*)b->value;
                if (child->name == b->name) {
                    // this is the original/primary binding for the submodule
                    size_t l = strlen(jl_symbol_name(child->name));
                    write_int32(s, l);
                    ios_write(s, jl_symbol_name(child->name), l);
                    write_uint64(s, child->uuid);
                }
            }
        }
    }
    write_int32(s, 0);
}

// "magic" string and version header of .ji file
static const int JI_FORMAT_VERSION = 2;
static const char JI_MAGIC[] = "\373jli\r\n\032\n"; // based on PNG signature
static const uint16_t BOM = 0xFEFF; // byte-order marker
static void jl_serialize_header(ios_t *s)
{
    ios_write(s, JI_MAGIC, strlen(JI_MAGIC));
    write_uint16(s, JI_FORMAT_VERSION);
    ios_write(s, (char *) &BOM, 2);
    write_uint8(s, sizeof(void*));
    const char *OS_NAME = jl_symbol_name(jl_get_OS_NAME());
    const char *ARCH = jl_symbol_name(jl_get_ARCH());
    ios_write(s, OS_NAME, strlen(OS_NAME)+1);
    ios_write(s, ARCH, strlen(ARCH)+1);
    ios_write(s, JULIA_VERSION_STRING, strlen(JULIA_VERSION_STRING)+1);
    const char *branch = jl_git_branch(), *commit = jl_git_commit();
    ios_write(s, branch, strlen(branch)+1);
    ios_write(s, commit, strlen(commit)+1);
}

// serialize the global _require_dependencies array of pathnames that
// are include depenencies
static void jl_serialize_dependency_list(ios_t *s)
{
    size_t total_size = 0;
    static jl_array_t *deps = NULL;
    if (!deps)
        deps = (jl_array_t*)jl_get_global(jl_base_module, jl_symbol("_require_dependencies"));

    // unique(deps) to eliminate duplicates while preserving order:
    // we preserve order so that the topmost included .jl file comes first
    static jl_value_t *unique_func = NULL;
    if (!unique_func)
        unique_func = jl_get_global(jl_base_module, jl_symbol("unique"));
    jl_value_t *uniqargs[2] = {unique_func,(jl_value_t*)deps};
    jl_array_t *udeps = deps && unique_func ? (jl_array_t*)jl_apply(uniqargs, 2) : NULL;

    JL_GC_PUSH1(&udeps);
    if (udeps) {
        size_t l = jl_array_len(udeps);
        for (size_t i=0; i < l; i++) {
            jl_value_t *dep = jl_fieldref(jl_cellref(udeps, i), 0);
            size_t slen = jl_string_len(dep);
            total_size += 4 + slen + 8;
        }
        total_size += 4;
    }
    // write the total size so that we can quickly seek past all of the
    // dependencies if we don't need them
    write_uint64(s, total_size);
    if (udeps) {
        size_t l = jl_array_len(udeps);
        for (size_t i=0; i < l; i++) {
            jl_value_t *deptuple = jl_cellref(udeps, i);
            jl_value_t *dep = jl_fieldref(deptuple, 0);
            size_t slen = jl_string_len(dep);
            write_int32(s, slen);
            ios_write(s, jl_string_data(dep), slen);
            write_float64(s, jl_unbox_float64(jl_fieldref(deptuple, 1)));
        }
        write_int32(s, 0); // terminator, for ease of reading
    }
    JL_GC_POP();
}

static int is_module_replaced(jl_module_t *m)
{
    return (jl_value_t*)m != jl_get_global(m->parent, m->name);
}

static int type_has_replaced_module(jl_value_t *t)
{
    if (jl_is_datatype(t)) {
        jl_datatype_t *dt = (jl_datatype_t*)t;
        if (is_module_replaced(dt->name->module))
            return 1;
        int i;
        for(i=0; i < jl_nparams(dt); i++)
            if (type_has_replaced_module(jl_tparam(dt,i)))
                return 1;
    }
    // TODO: might eventually need to handle more types here
    return 0;
}

static void remove_specializations_from_replaced_modules(jl_methlist_t *l)
{
    while (l != (void*)jl_nothing) {
        jl_array_t *a = l->func->specializations;
        if (a) {
            size_t len = jl_array_len(a);
            size_t i, insrt=0;
            for(i=0; i < len; i++) {
                jl_lambda_info_t *li = (jl_lambda_info_t*)jl_cellref(a, i);
                if (!(li->rettype && type_has_replaced_module(li->rettype)) &&
                    !(li->specTypes && type_has_replaced_module((jl_value_t*)li->specTypes))) {
                    jl_cellset(a, insrt, li);
                    insrt++;
                }
            }
            jl_array_del_end(a, len-insrt);
        }
        a = l->func->roots;
        if (a) {
            size_t len = jl_array_len(a);
            size_t i;
            for(i=0; i < len; i++) {
                jl_value_t *ai = jl_cellref(a, i);
                if (jl_is_type(ai) && type_has_replaced_module(ai))
                    jl_cellset(a, i, jl_nothing);
            }
        }
        l = l->next;
    }
}

static void remove_methods_from_replaced_modules_from_list(jl_methlist_t **pl)
{
    jl_methlist_t *l = *pl;
    while (l != (void*)jl_nothing) {
        if ((l->func && is_module_replaced(l->func->module)) ||
            type_has_replaced_module((jl_value_t*)l->sig))
            *pl = l->next;
        else
            pl = &l->next;
        l = l->next;
    }
}

static void remove_methods_from_replaced_modules_from_cache(jl_array_t *a)
{
    jl_value_t **data;
    size_t i, l = jl_array_len(a); data = (jl_value_t**)jl_array_data(a);
    for(i=0; i < l; i++) {
        if (data[i] != NULL)
            remove_methods_from_replaced_modules_from_list((jl_methlist_t**)&data[i]);
    }
}

static void remove_methods_from_replaced_modules(jl_methtable_t *mt)
{
    remove_methods_from_replaced_modules_from_list(&mt->defs);
    remove_methods_from_replaced_modules_from_list(&mt->cache);
    if ((jl_value_t*)mt->cache_arg1 != jl_nothing)
        remove_methods_from_replaced_modules_from_cache(mt->cache_arg1);
    if ((jl_value_t*)mt->cache_targ != jl_nothing)
        remove_methods_from_replaced_modules_from_cache(mt->cache_targ);
    remove_specializations_from_replaced_modules(mt->defs);
    if (mt->kwsorter)
        remove_methods_from_replaced_modules(jl_gf_mtable(mt->kwsorter));
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
        }
        backref_list.items[pos] = dtv;
        return dtv;
    }
    uint16_t nf = read_uint16(s);
    size_t size = read_int32(s);
    uint8_t flags = read_uint8(s);
    uint8_t fielddesc_type = read_int8(s);
    jl_datatype_t *dt;
    if (tag == 2)
        dt = jl_int32_type;
    else if (tag == 3)
        dt = jl_bool_type;
    else if (tag == 4)
        dt = jl_int64_type;
    else
        dt = jl_new_uninitialized_datatype(nf, fielddesc_type);
    assert(tree_literal_values==NULL && mode != MODE_AST);
    backref_list.items[pos] = dt;
    dt->size = size;
    dt->struct_decl = NULL;
    dt->instance = NULL;
    dt->ditype = NULL;
    dt->abstract = flags&1;
    dt->mutabl = (flags>>1)&1;
    dt->pointerfree = (flags>>2)&1;
    if (!dt->abstract) {
        dt->ninitialized = read_uint16(s);
        dt->uid = mode != MODE_MODULE && mode != MODE_MODULE_POSTWORK ? read_int32(s) : 0;
    }
    else {
        dt->ninitialized = 0;
        dt->uid = 0;
    }
    int has_instance = (flags>>3)&1;
    if (has_instance) {
        assert(mode != MODE_MODULE_POSTWORK); // there shouldn't be an instance on a type with uid = 0
        dt->instance = jl_deserialize_value(s, &dt->instance);
        jl_gc_wb(dt, dt->instance);
    }
    if (tag == 5) {
        assert(pos > 0);
        assert(mode != MODE_MODULE_POSTWORK);
        arraylist_push(&flagref_list, loc);
        arraylist_push(&flagref_list, (void*)(uintptr_t)pos);
        dt->uid = -1; // mark that this type needs a new uid
    }

    if (nf > 0) {
        dt->alignment = read_int32(s);
        dt->haspadding = read_int8(s);
        size_t fieldsize = jl_fielddesc_size(fielddesc_type);
        ios_read(s, jl_datatype_fields(dt), nf * fieldsize);
        dt->types = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&dt->types);
        jl_gc_wb(dt, dt->types);
    }
    else {
        dt->alignment = dt->size;
        dt->haspadding = 0;
        if (dt->alignment > MAX_ALIGN)
            dt->alignment = MAX_ALIGN;
        dt->types = jl_emptysvec;
    }
    dt->parameters = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&dt->parameters);
    jl_gc_wb(dt, dt->parameters);
    dt->name = (jl_typename_t*)jl_deserialize_value(s, (jl_value_t**)&dt->name);
    jl_gc_wb(dt, dt->name);
    dt->super = (jl_datatype_t*)jl_deserialize_value(s, (jl_value_t**)&dt->super);
    jl_gc_wb(dt, dt->super);
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

static jl_value_t *jl_deserialize_value_(ios_t *s, jl_value_t *vtag, jl_value_t **loc);
static jl_value_t *jl_deserialize_value(ios_t *s, jl_value_t **loc)
{
    assert(!ios_eof(s));
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
        uintptr_t offs = (tag == BackRef_tag) ? read_int32(s) : read_uint16(s);
        int isdatatype = 0;
        if (mode == MODE_MODULE) {
            isdatatype = !!(offs & 1);
            offs >>= 1;
        }
        else if (mode == MODE_MODULE_POSTWORK) {
            offs >>= 1;
        }
        // assert(offs >= 0); // offs is unsigned so this is always true
        assert(offs < backref_list.len);
        jl_value_t *bp = (jl_value_t*)backref_list.items[offs];
        assert(bp);
        if (isdatatype && loc != NULL) {
            arraylist_push(&flagref_list, loc);
            arraylist_push(&flagref_list, (void*)(uintptr_t)-1);
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
        jl_value_t **data = jl_svec_data(sv);
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
        char *name = (char*) (len >= 256 ? malloc(len+1) : alloca(len+1));
        ios_read(s, name, len);
        name[len] = '\0';
        jl_value_t *sym = (jl_value_t*)jl_symbol(name);
        if (len >= 256) free(name);
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
        size_t *dims = (size_t*)alloca(ndims*sizeof(size_t));
        for(i=0; i < ndims; i++)
            dims[i] = jl_unbox_long(jl_deserialize_value(s, NULL));
        jl_array_t *a = jl_new_array_for_deserialization((jl_value_t*)NULL, ndims, dims, isunboxed, elsize);
        if (usetable)
            backref_list.items[pos] = a;
        jl_value_t *aty = jl_deserialize_value(s, &jl_astaggedvalue(a)->type);
        jl_set_typeof(a, aty);
        if (!a->flags.ptrarray) {
            size_t tot = jl_array_len(a) * a->elsize;
            ios_read(s, (char*)jl_array_data(a), tot);
        }
        else {
            jl_value_t **data = (jl_value_t**)jl_array_data(a);
            for(i=0; i < jl_array_len(a); i++) {
                data[i] = jl_deserialize_value(s, &data[i]);
                if (data[i]) jl_gc_wb(a, data[i]);
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
        int pos = backref_list.len;
        if (usetable)
            arraylist_push(&backref_list, NULL);
        jl_expr_t *e = jl_exprn((jl_sym_t*)jl_deserialize_value(s, NULL), len);
        if (usetable)
            backref_list.items[pos] = e;
        e->etype = jl_deserialize_value(s, &e->etype);
        jl_gc_wb(e, e->etype);
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
        jl_gc_wb(tv, tv->name);
        tv->lb = jl_deserialize_value(s, &tv->lb);
        jl_gc_wb(tv, tv->lb);
        tv->ub = jl_deserialize_value(s, &tv->ub);
        jl_gc_wb(tv, tv->ub);
        tv->bound = read_int8(s);
        return (jl_value_t*)tv;
    }
    else if (vtag == (jl_value_t*)jl_lambda_info_type) {
        jl_lambda_info_t *li =
            (jl_lambda_info_t*)newobj((jl_value_t*)jl_lambda_info_type,
                                      NWORDS(sizeof(jl_lambda_info_t)));
        if (usetable)
            arraylist_push(&backref_list, li);
        li->ast = jl_deserialize_value(s, &li->ast);
        jl_gc_wb(li, li->ast);
        li->rettype = jl_deserialize_value(s, &li->rettype);
        jl_gc_wb(li, li->rettype);
        li->sparam_syms = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&li->sparam_syms);
        jl_gc_wb(li, li->sparam_syms);
        li->sparam_vals = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&li->sparam_vals);
        jl_gc_wb(li, li->sparam_vals);
        li->tfunc = jl_deserialize_value(s, (jl_value_t**)&li->tfunc);
        jl_gc_wb(li, li->tfunc);
        li->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        jl_gc_wb(li, li->name);
        li->specTypes = (jl_tupletype_t*)jl_deserialize_value(s, (jl_value_t**)&li->specTypes);
        if (li->specTypes) jl_gc_wb(li, li->specTypes);
        li->specializations = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&li->specializations);
        if (li->specializations) jl_gc_wb(li, li->specializations);
        li->inferred = read_int8(s);
        li->pure = read_int8(s);
        li->called = read_int8(s);
        li->file = (jl_sym_t*)jl_deserialize_value(s, NULL);
        jl_gc_wb(li, li->file);
        li->line = read_int32(s);
        li->nslots = read_int32(s);
        li->ngensym = read_int32(s);
        li->module = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&li->module);
        jl_gc_wb(li, li->module);
        li->roots = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&li->roots);
        if (li->roots) jl_gc_wb(li, li->roots);
        li->def = (jl_lambda_info_t*)jl_deserialize_value(s, (jl_value_t**)&li->def);
        jl_gc_wb(li, li->def);
        li->fptr = NULL;
        li->functionObjects.functionObject = NULL;
        li->functionObjects.cFunctionList = NULL;
        li->functionObjects.specFunctionObject = NULL;
        li->inInference = 0;
        li->inCompile = 0;
        li->unspecialized = (jl_lambda_info_t*)jl_deserialize_value(s, (jl_value_t**)&li->unspecialized);
        if (li->unspecialized) jl_gc_wb(li, li->unspecialized);
        li->fptr = jl_deserialize_fptr(s);
        li->functionID = 0;
        li->specFunctionID = 0;
        int32_t cfunc_llvm, func_llvm;
        func_llvm = read_int32(s);
        cfunc_llvm = read_int32(s);
        jl_delayed_fptrs(li, func_llvm, cfunc_llvm);
        li->jlcall_api = func_llvm ? read_int8(s) : 0;
        li->needs_sparam_vals_ducttape = read_int8(s);
        return (jl_value_t*)li;
    }
    else if (vtag == (jl_value_t*)jl_module_type) {
        int pos = backref_list.len;
        if (usetable)
            arraylist_push(&backref_list, NULL);
        jl_sym_t *mname = (jl_sym_t*)jl_deserialize_value(s, NULL);
        int ref_only = 0;
        if (mode == MODE_MODULE_POSTWORK) {
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
        jl_gc_wb(m, m->parent);

        while (1) {
            jl_sym_t *name = (jl_sym_t*)jl_deserialize_value(s, NULL);
            if (name == NULL)
                break;
            jl_binding_t *b = jl_get_binding_wr(m, name);
            b->value = jl_deserialize_value(s, &b->value);
            jl_gc_wb_buf(m, b);
            if (b->value != NULL) jl_gc_wb(m, b->value);
            b->globalref = jl_deserialize_value(s, &b->globalref);
            if (b->globalref != NULL) jl_gc_wb(m, b->globalref);
            b->owner = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&b->owner);
            if (b->owner != NULL) jl_gc_wb(m, b->owner);
            int8_t flags = read_int8(s);
            b->deprecated = (flags>>3) & 1;
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
        m->istopmod = read_uint8(s);
        m->std_imports = read_uint8(s);
        m->uuid = read_uint64(s);
        m->counter = read_int32(s);
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
    else if (vtag == (jl_value_t*)NearbyGlobal_tag) {
        assert(tree_enclosing_module != NULL);
        jl_value_t *sym = jl_deserialize_value(s, NULL);
        return jl_module_globalref(tree_enclosing_module, (jl_sym_t*)sym);
    }
    else if (vtag == (jl_value_t*)jl_globalref_type) {
        if (usetable) {
            jl_value_t *v = jl_new_struct_uninit(jl_globalref_type);
            arraylist_push(&backref_list, v);
            jl_value_t **data = jl_data_ptr(v);
            data[0] = jl_deserialize_value(s, &data[0]);
            data[1] = jl_deserialize_value(s, &data[1]);
            return v;
        }
        else {
            jl_value_t *mod = jl_deserialize_value(s, NULL);
            jl_value_t *var = jl_deserialize_value(s, NULL);
            return jl_module_globalref((jl_module_t*)mod, (jl_sym_t*)var);
        }
    }
    else if (vtag == (jl_value_t*)jl_datatype_type || vtag == (jl_value_t*)SmallDataType_tag) {
        int32_t sz = (vtag == (jl_value_t*)SmallDataType_tag ? read_uint8(s) : read_int32(s));
        jl_value_t *v = jl_gc_allocobj(sz);
        int pos = backref_list.len;
        if (usetable)
            arraylist_push(&backref_list, v);
        jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, &jl_astaggedvalue(v)->type);
        jl_set_typeof(v, dt);
        if (dt == jl_datatype_type)
            return jl_deserialize_datatype(s, pos, loc);
        assert(mode==MODE_AST || sz!=0 || loc);
        if ((mode == MODE_MODULE || mode == MODE_MODULE_POSTWORK) && dt == jl_typename_type) {
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
            assert(mode != MODE_MODULE_POSTWORK);
        }
        size_t nf = jl_datatype_nfields(dt);
        if (nf == 0 && jl_datatype_size(dt)>0) {
            int nby = jl_datatype_size(dt);
            ios_read(s, (char*)jl_data_ptr(v), nby);
        }
        else {
            char *data = (char*)jl_data_ptr(v);
            for(i=0; i < nf; i++) {
                if (jl_field_size(dt,i) > 0) {
                    if (jl_field_isptr(dt,i)) {
                        jl_value_t **fld = (jl_value_t**)(data+jl_field_offset(dt, i));
                        *fld = jl_deserialize_value(s, fld);
                    }
                    else {
                        jl_set_nth_field(v, i, jl_deserialize_value(s, NULL));
                    }
                }
            }
            if ((mode == MODE_MODULE || mode == MODE_MODULE_POSTWORK)) {
                if (jl_is_mtable(v))
                    arraylist_push(&methtable_list, v); // will resort this table, later
                if (dt == jl_typename_type) {
                    jl_typename_t *tn = (jl_typename_t*)v;
                    tn->uid = jl_assign_type_uid(); // make sure this has a new uid
                    tn->cache = jl_emptysvec; // the cache is refilled later (tag 5)
                    tn->linearcache = jl_emptysvec; // the cache is refilled later (tag 5)
                }
            }
        }
        return v;
    }
    else if (vtag == (jl_value_t*)Singleton_tag) {
        if (mode == MODE_MODULE_POSTWORK) {
            uintptr_t pos = backref_list.len;
            arraylist_push(&backref_list, NULL);
            jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, NULL);
            backref_list.items[pos] = dt->instance;
            return dt->instance;
        }
        jl_value_t *v = (jl_value_t*)jl_gc_alloc_0w();
        if (usetable) {
            uintptr_t pos = backref_list.len;
            arraylist_push(&backref_list, (void*)v);
            if (mode == MODE_MODULE) {
                // TODO: optimize the case where the value can easily be obtained
                // from an external module (tag == 6) as dt->instance
                assert(loc != NULL);
                arraylist_push(&flagref_list, loc);
                arraylist_push(&flagref_list, (void*)pos);
            }
        }
        jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, NULL); // no loc, since if dt is replaced, then dt->instance would be also
        jl_set_typeof(v, dt);
        return v;
    }
    assert(0);
    return NULL;
}

static void jl_deserialize_lambdas_from_mod(ios_t *s)
{
    while (1) {
        jl_module_t *mod = (jl_module_t*)jl_deserialize_value(s, NULL);
        if (mod == NULL)
            return;
        jl_sym_t *name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        jl_function_t *gf = (jl_function_t*)jl_get_global(mod, name);
        int8_t iskw = read_int8(s);
        if (iskw)
            gf = jl_get_kwsorter(((jl_datatype_t*)jl_typeof(gf))->name);
        jl_tupletype_t *types = (jl_tupletype_t*)jl_deserialize_value(s, NULL);
        jl_lambda_info_t *meth = (jl_lambda_info_t*)jl_deserialize_value(s, NULL);
        jl_svec_t *tvars = (jl_svec_t*)jl_deserialize_value(s, NULL);
        int8_t isstaged = read_int8(s);
        jl_method_table_insert(jl_gf_mtable(gf), types, meth, tvars, isstaged);
    }
}

static int jl_deserialize_verify_mod_list(ios_t *s)
{
    if (!jl_main_module->uuid) {
        jl_printf(JL_STDERR, "ERROR: Main module uuid state is invalid for module deserialization.\n");
        return 0;
    }
    while (1) {
        size_t len = read_int32(s);
        if (len == 0)
            return 1;
        char *name = (char*)alloca(len+1);
        ios_read(s, name, len);
        name[len] = '\0';
        uint64_t uuid = read_uint64(s);
        jl_sym_t *sym = jl_symbol(name);
        jl_module_t *m = (jl_module_t*)jl_get_global(jl_main_module, sym);
        if (!m) {
            static jl_value_t *require_func = NULL;
            if (!require_func)
                require_func = jl_get_global(jl_base_module, jl_symbol("require"));
            jl_value_t *reqargs[2] = {require_func, (jl_value_t*)sym};
            JL_TRY {
                jl_apply(reqargs, 2);
            }
            JL_CATCH {
                ios_close(s);
                jl_rethrow();
            }
            m = (jl_module_t*)jl_get_global(jl_main_module, sym);
        }
        if (!m) {
            jl_printf(JL_STDERR, "ERROR: requiring \"%s\" did not define a corresponding module\n", name);
            return 0;
        }
        if (!jl_is_module(m)) {
            ios_close(s);
            jl_errorf("invalid module path (%s does not name a module)", name);
        }
        if (m->uuid != uuid) {
            jl_printf(JL_STDERR,
                      "WARNING: Module %s uuid did not match cache file\n"
                      "  This is likely because module %s does not support"
                      "  precompilation but is imported by a module that does.\n",
                      name, name);
            return 0;
        }
    }
}

static int readstr_verify(ios_t *s, const char *str)
{
    size_t len = strlen(str);
    for (size_t i=0; i < len; ++i)
        if ((char) read_uint8(s) != str[i])
            return 0;
    return 1;
}

JL_DLLEXPORT int jl_deserialize_verify_header(ios_t *s)
{
    uint16_t bom;
    return (readstr_verify(s, JI_MAGIC) &&
            read_uint16(s) == JI_FORMAT_VERSION &&
            ios_read(s, (char *) &bom, 2) == 2 && bom == BOM &&
            read_uint8(s) == sizeof(void*) &&
            readstr_verify(s, jl_symbol_name(jl_get_OS_NAME())) && !read_uint8(s) &&
            readstr_verify(s, jl_symbol_name(jl_get_ARCH())) && !read_uint8(s) &&
            readstr_verify(s, JULIA_VERSION_STRING) && !read_uint8(s) &&
            readstr_verify(s, jl_git_branch()) && !read_uint8(s) &&
            readstr_verify(s, jl_git_commit()) && !read_uint8(s));
}

jl_array_t *jl_module_init_order;

static void jl_finalize_serializer(ios_t *f) {
    size_t i, l;
    // save module initialization order
    if (jl_module_init_order != NULL) {
        l = jl_array_len(jl_module_init_order);
        for(i=0; i < l; i++) {
            // verify that all these modules were saved
            assert(ptrhash_get(&backref_table, jl_cellref(jl_module_init_order, i)) != HT_NOTFOUND);
        }
    }
    if (mode != MODE_MODULE)
        jl_serialize_value(f, jl_module_init_order);

    // record list of reinitialization functions
    l = reinit_list.len;
    for (i = 0; i < l; i += 2) {
        write_int32(f, (int)((uintptr_t) reinit_list.items[i]));
        write_int32(f, (int)((uintptr_t) reinit_list.items[i+1]));
    }
    write_int32(f, -1);
}

static void jl_reinit_item(ios_t *f, jl_value_t *v, int how) {
    JL_TRY {
        switch (how) {
            case 1: { // rehash ObjectIdDict
                jl_array_t **a = (jl_array_t**)v;
                // Assume *a don't need a write barrier
                jl_idtable_rehash(a, jl_array_len(*a));
                jl_gc_wb(v, *a);
                break;
            }
            case 2: { // reinsert module v into parent (const)
                jl_module_t *mod = (jl_module_t*)v;
                jl_binding_t *b = jl_get_binding_wr(mod->parent, mod->name);
                jl_declare_constant(b); // this can throw
                if (b->value != NULL) {
                    if (!jl_is_module(b->value)) {
                        jl_errorf("invalid redefinition of constant %s",
                                  jl_symbol_name(mod->name)); // this also throws
                    }
                    if (jl_generating_output() && jl_options.incremental) {
                        jl_errorf("cannot replace module %s during incremental precompile", jl_symbol_name(mod->name));
                    }
                    jl_printf(JL_STDERR, "WARNING: replacing module %s\n",
                              jl_symbol_name(mod->name));
                }
                b->value = v;
                jl_gc_wb_binding(b, v);
                break;
            }
            default:
                assert(0);
        }
    }
    JL_CATCH {
        jl_printf(JL_STDERR, "WARNING: error while reinitializing value ");
        jl_static_show(JL_STDERR, v);
        jl_printf(JL_STDERR, ":\n");
        jl_static_show(JL_STDERR, jl_exception_in_transit);
        jl_printf(JL_STDERR, "\n");
    }
}
static jl_array_t *jl_finalize_deserializer(ios_t *f) {
    jl_array_t *init_order = NULL;
    if (mode != MODE_MODULE)
        init_order = (jl_array_t*)jl_deserialize_value(f, NULL);

    // run reinitialization functions
    int pos = read_int32(f);
    while (pos != -1) {
        jl_reinit_item(f, (jl_value_t*)backref_list.items[pos], read_int32(f));
        pos = read_int32(f);
    }
    return init_order;
}

void jl_init_restored_modules(jl_array_t *init_order)
{
    if (!init_order)
        return;
    int i;
    for(i=0; i < jl_array_len(init_order); i++) {
        jl_value_t *mod = jl_cellref(init_order, i);
        jl_module_run_initializer((jl_module_t*)mod);
    }
}


// --- entry points ---

static void jl_save_system_image_to_stream(ios_t *f)
{
    jl_gc_collect(1); // full
    jl_gc_collect(0); // incremental (sweep finalizers)
    JL_SIGATOMIC_BEGIN();
    JL_LOCK(dump); // Might GC
    int en = jl_gc_enable(0);
    htable_reset(&backref_table, 250000);
    arraylist_new(&reinit_list, 0);
    backref_table_numel = 0;

    // orphan old Base module if present
    jl_base_module = (jl_module_t*)jl_get_global(jl_main_module, jl_symbol("Base"));

    // empty!(Core.ARGS)
    if (jl_core_module != NULL) {
        jl_array_t *args = (jl_array_t*)jl_get_global(jl_core_module, jl_symbol("ARGS"));
        if (args != NULL) {
            jl_array_del_end(args, jl_array_len(args));
        }
    }

    // remove constructors (which go in a single shared method table) from modules
    // that were replaced during bootstrap.
    remove_methods_from_replaced_modules(jl_datatype_type->name->mt);

    jl_idtable_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("ObjectIdDict")) : NULL;

    jl_serialize_value(f, jl_main_module);
    jl_serialize_value(f, jl_top_module);
    jl_serialize_value(f, jl_typeinf_func);
    jl_serialize_value(f, jl_type_type->name->mt);

    // ensure everything in deser_tag is reassociated with its GlobalValue
    intptr_t i=2;
    for (i=2; i < 255; i++) {
        jl_serialize_gv(f, deser_tag[i]);
    }
    jl_serialize_globalvals(f);
    jl_serialize_gv_others(f); // serialize things that might not have visible gc roots roots with GlobalValue references

    write_int32(f, jl_get_t_uid_ctr());
    write_int32(f, jl_get_gs_ctr());
    jl_finalize_serializer(f); // done with f

    htable_reset(&backref_table, 0);
    arraylist_free(&reinit_list);

    jl_gc_enable(en);
    JL_UNLOCK(dump);
    JL_SIGATOMIC_END();
}

JL_DLLEXPORT void jl_save_system_image(const char *fname)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 1, 1, 1) == NULL) {
        jl_errorf("cannot open system image file \"%s\" for writing", fname);
    }
    JL_SIGATOMIC_BEGIN();
    jl_save_system_image_to_stream(&f);
    ios_close(&f);
    JL_SIGATOMIC_END();
}

JL_DLLEXPORT ios_t *jl_create_system_image(void)
{
    ios_t *f = (ios_t*)malloc(sizeof(ios_t));
    ios_mem(f, 1000000);
    jl_save_system_image_to_stream(f);
    return f;
}

extern jl_function_t *jl_typeinf_func;
extern int jl_boot_file_loaded;
extern void jl_get_builtins(void);
extern void jl_get_builtin_hooks(void);
extern void jl_get_system_hooks(void);

// Takes in a path of the form "usr/lib/julia/sys.{ji,so}", as passed to jl_restore_system_image()
JL_DLLEXPORT void jl_preload_sysimg_so(const char *fname)
{
    // If passed NULL, don't even bother
    if (!fname)
        return;

    // First, get "sys" from "sys.ji"
    char *fname_shlib = (char*)alloca(strlen(fname)+1);
    strcpy(fname_shlib, fname);
    char *fname_shlib_dot = strrchr(fname_shlib, '.');
    if (fname_shlib_dot != NULL) {
        if (!strcmp(fname_shlib_dot, ".ji"))
            return;  // .ji extension => load .ji file only
        *fname_shlib_dot = 0;
    }

    // Get handle to sys.so
    jl_sysimg_handle = jl_load_dynamic_library_e(fname_shlib, JL_RTLD_LOCAL | JL_RTLD_NOW);

    // set cpu target if unspecified by user and available from sysimg
    // otherwise default to native.
    if (jl_sysimg_handle && jl_options.cpu_target == NULL)
        jl_options.cpu_target = (const char *)jl_dlsym(jl_sysimg_handle, "jl_sysimg_cpu_target");
}

static void jl_restore_system_image_from_stream(ios_t *f)
{
    JL_SIGATOMIC_BEGIN();
    JL_LOCK(dump); // Might GC
    int en = jl_gc_enable(0);
    DUMP_MODES last_mode = mode;
    mode = MODE_SYSTEM_IMAGE;
    arraylist_new(&backref_list, 250000);

    datatype_list = jl_alloc_cell_1d(0);

    jl_main_module = (jl_module_t*)jl_deserialize_value(f, NULL);
    jl_top_module = (jl_module_t*)jl_deserialize_value(f, NULL);
    jl_internal_main_module = jl_main_module;
    jl_typeinf_func = (jl_function_t*)jl_deserialize_value(f, NULL);
    jl_type_type->name->mt = (jl_methtable_t*)jl_deserialize_value(f, NULL);
    jl_typector_type->name->mt = jl_uniontype_type->name->mt = jl_datatype_type->name->mt =
        jl_type_type->name->mt;

    jl_core_module = (jl_module_t*)jl_get_global(jl_main_module,
                                                 jl_symbol("Core"));
    jl_base_module = (jl_module_t*)jl_get_global(jl_main_module,
                                                 jl_symbol("Base"));
    jl_current_module = jl_base_module; // run start_image in Base

    // ensure everything in deser_tag is reassociated with its GlobalValue
    intptr_t i;
    for (i=2; i < 255; i++) {
        jl_deserialize_gv(f, deser_tag[i]);
    }
    jl_deserialize_globalvals(f);
    jl_deserialize_gv_others(f);

    int uid_ctr = read_int32(f);
    int gs_ctr = read_int32(f);
    jl_module_init_order = jl_finalize_deserializer(f); // done with f

    jl_set_t_uid_ctr(uid_ctr);
    jl_set_gs_ctr(gs_ctr);

    // cache builtin parametric types
    for(int i=0; i < jl_array_len(datatype_list); i++) {
        jl_value_t *v = jl_cellref(datatype_list, i);
        jl_cache_type_((jl_datatype_t*)v);
    }
    datatype_list = NULL;

    jl_get_builtins();
    jl_get_builtin_hooks();
    if (jl_base_module) {
        jl_get_system_hooks();
    }
    jl_boot_file_loaded = 1;
    jl_init_box_caches();

    //jl_printf(JL_STDERR, "backref_list.len = %d\n", backref_list.len);
    arraylist_free(&backref_list);

    jl_gc_enable(en);
    mode = last_mode;
    jl_update_all_fptrs();
    JL_UNLOCK(dump);
    JL_SIGATOMIC_END();
}

JL_DLLEXPORT void jl_restore_system_image(const char *fname)
{
    char *dot = (char*) strrchr(fname, '.');
    int is_ji = (dot && !strcmp(dot, ".ji"));

    if (!is_ji) {
        int err = jl_load_sysimg_so();
        if (err != 0) {
            if (jl_sysimg_handle == 0)
                jl_errorf("system image file \"%s\" not found", fname);
            jl_errorf("library \"%s\" does not contain a valid system image", fname);
        }
    }
    else {
        ios_t f;
        if (ios_file(&f, fname, 1, 0, 0, 0) == NULL)
            jl_errorf("system image file \"%s\" not found", fname);
        JL_SIGATOMIC_BEGIN();
        jl_restore_system_image_from_stream(&f);
        ios_close(&f);
        JL_SIGATOMIC_END();
    }
}

JL_DLLEXPORT void jl_restore_system_image_data(const char *buf, size_t len)
{
    ios_t f;
    JL_SIGATOMIC_BEGIN();
    ios_static_buffer(&f, (char*)buf, len);
    jl_restore_system_image_from_stream(&f);
    ios_close(&f);
    JL_SIGATOMIC_END();
}

JL_DLLEXPORT jl_value_t *jl_compress_ast(jl_lambda_info_t *li, jl_value_t *ast)
{
    JL_SIGATOMIC_BEGIN();
    JL_LOCK(dump); // Might GC
    DUMP_MODES last_mode = mode;
    mode = MODE_AST;
    ios_t dest;
    ios_mem(&dest, 0);
    jl_array_t *last_tlv = tree_literal_values;
    jl_module_t *last_tem = tree_enclosing_module;
    int en = jl_gc_enable(0); // Might GC

    if (li->def->roots == NULL) {
        li->def->roots = jl_alloc_cell_1d(0);
        jl_gc_wb(li->def, li->def->roots);
    }
    tree_literal_values = li->def->roots;
    tree_enclosing_module = li->module;
    jl_serialize_value(&dest, ast);

    //jl_printf(JL_STDERR, "%d bytes, %d values\n", dest.size, vals->length);

    jl_value_t *v = (jl_value_t*)jl_takebuf_array(&dest);
    if (jl_array_len(tree_literal_values) == 0 && last_tlv == NULL) {
        li->def->roots = NULL;
    }
    tree_literal_values = last_tlv;
    tree_enclosing_module = last_tem;
    jl_gc_enable(en);
    mode = last_mode;
    JL_UNLOCK(dump);
    JL_SIGATOMIC_END();
    return v;
}

JL_DLLEXPORT jl_value_t *jl_uncompress_ast(jl_lambda_info_t *li, jl_value_t *data)
{
    JL_SIGATOMIC_BEGIN();
    JL_LOCK(dump); // Might GC
    assert(jl_is_array(data));
    DUMP_MODES last_mode = mode;
    mode = MODE_AST;
    jl_array_t *bytes = (jl_array_t*)data;
    tree_literal_values = li->def->roots;
    tree_enclosing_module = li->module;
    ios_t src;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)bytes->data, jl_array_len(bytes), 0);
    src.size = jl_array_len(bytes);
    int en = jl_gc_enable(0); // Might GC
    jl_value_t *v = jl_deserialize_value(&src, NULL);
    jl_gc_enable(en);
    tree_literal_values = NULL;
    tree_enclosing_module = NULL;
    mode = last_mode;
    JL_UNLOCK(dump);
    JL_SIGATOMIC_END();
    return v;
}

JL_DLLEXPORT int jl_save_incremental(const char *fname, jl_array_t *worklist)
{
    char *tmpfname = strcat(strcpy((char *) alloca(strlen(fname)+8), fname), ".XXXXXX");
    ios_t f;
    if (ios_mkstemp(&f, tmpfname) == NULL) {
        jl_printf(JL_STDERR, "Cannot open cache file \"%s\" for writing.\n", tmpfname);
        return 1;
    }
    serializer_worklist = worklist;
    jl_serialize_header(&f);
    jl_serialize_mod_list(&f); // this can throw, keep it early (before any actual initialization)
    jl_serialize_dependency_list(&f);

    JL_SIGATOMIC_BEGIN();
    JL_LOCK(dump); // Might GC
    arraylist_new(&reinit_list, 0);
    htable_new(&backref_table, 5000);
    ptrhash_put(&backref_table, jl_main_module, (char*)HT_NOTFOUND + 1);
    backref_table_numel = 1;
    jl_idtable_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("ObjectIdDict")) : NULL;

    int en = jl_gc_enable(0);
    DUMP_MODES last_mode = mode;
    mode = MODE_MODULE;
    jl_serialize_value(&f, worklist);
    jl_finalize_serializer(&f); // done with MODE_MODULE
    reinit_list.len = 0;

    mode = MODE_MODULE_POSTWORK;
    jl_serialize_lambdas_from_mod(&f, jl_main_module);
    jl_serialize_value(&f, NULL); // signal end of lambdas
    jl_finalize_serializer(&f); // done with f

    mode = last_mode;
    jl_gc_enable(en);

    htable_reset(&backref_table, 0);
    arraylist_free(&reinit_list);
    ios_close(&f);
    JL_UNLOCK(dump);
    JL_SIGATOMIC_END();

    if (jl_fs_rename(tmpfname, fname) < 0) {
        jl_printf(JL_STDERR, "Cannot write cache file \"%s\".\n", fname);
        return 1;
    }

    return 0;
}

static jl_datatype_t *jl_recache_type(jl_datatype_t *dt, size_t start, jl_value_t *v)
{
    if (v == NULL)
        v = dt->instance; // the instance before unique'ing
    jl_datatype_t *t; // the type after unique'ing
    if (dt->uid == -1) {
        jl_svec_t *tt = dt->parameters;
        size_t l = jl_svec_len(tt);
        if (l == 0) { // jl_cache_type doesn't work if length(parameters) == 0
            dt->uid = jl_assign_type_uid();
            t = dt;
        }
        else {
            // recache all type parameters, then type type itself
            size_t i;
            for (i = 0; i < l; i++) {
                jl_datatype_t *p = (jl_datatype_t*)jl_svecref(tt, i);
                if (jl_is_datatype(p) && p->uid == -1) {
                    jl_datatype_t *cachep = jl_recache_type(p, start, NULL);
                    if (p != cachep)
                        jl_svecset(tt, i, cachep);
                }
                jl_datatype_t *tp = (jl_datatype_t*)jl_typeof(p);
                if (jl_is_datatype_singleton(tp)) {
                    if (tp->uid == -1) {
                        tp = jl_recache_type(tp, start, NULL);
                    }
                    if ((jl_value_t*)p != tp->instance)
                        jl_svecset(tt, i, tp->instance);
                }
            }
            dt->uid = 0;
            t = (jl_datatype_t*)jl_cache_type_(dt);
        }
    }
    else {
        t = dt;
    }
    assert(t->uid != 0);
    // delete / replace any other usages of this type in the backref list
    // with the newly constructed object
    size_t j = start;
    while (j < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[j];
        int offs = (int)(intptr_t)flagref_list.items[j+1];
        jl_value_t *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        if ((jl_value_t*)dt == o) {
            if (t != dt) {
                if (loc) *loc = (jl_value_t*)t;
                if (offs > 0) backref_list.items[offs] = t;
            }
        }
        else if (v == o) {
            if (t->instance != v) {
                *loc = t->instance;
                if (offs > 0) backref_list.items[offs] = t->instance;
            }
        }
        else {
            j += 2;
            continue;
        }
        // delete this item from the flagref list, so it won't be re-encountered later
        flagref_list.len -= 2;
        if (j >= flagref_list.len)
            break;
        flagref_list.items[j+0] = flagref_list.items[flagref_list.len+0];
        flagref_list.items[j+1] = flagref_list.items[flagref_list.len+1];
    }
    return t;
}

static void jl_recache_types(void)
{
    size_t i = 0;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i++];
        int offs = (int)(intptr_t)flagref_list.items[i++];
        jl_value_t *v, *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        jl_datatype_t *dt, *t;
        if (jl_is_datatype(o)) {
            dt = (jl_datatype_t*)o;
            v = dt->instance;
            assert(dt->uid == -1);
            t = jl_recache_type(dt, i, NULL);
        }
        else {
            dt = (jl_datatype_t*)jl_typeof(o);
            v = o;
            t = jl_recache_type(dt, i, v);
        }
        assert(dt);
        if (t != dt) {
            jl_set_typeof(dt, (jl_value_t*)(intptr_t)0x10); // invalidate the old value to help catch errors
            if ((jl_value_t*)dt == o) {
                if (loc) *loc = (jl_value_t*)t;
                if (offs > 0) backref_list.items[offs] = t;
            }
        }
        if (t->instance != v) {
            jl_set_typeof(v, (jl_value_t*)(intptr_t)0x20); // invalidate the old value to help catch errors
            if (v == o) {
                *loc = t->instance;
                if (offs > 0) backref_list.items[offs] = t->instance;
            }
        }
    }
}

static jl_array_t *_jl_restore_incremental(ios_t *f)
{
    if (ios_eof(f)) {
        ios_close(f);
        return NULL;
    }
    if (!jl_deserialize_verify_header(f) ||
        !jl_deserialize_verify_mod_list(f)) {
        ios_close(f);
        return NULL;
    }
    size_t deplen = read_uint64(f);
    ios_skip(f, deplen); // skip past the dependency list
    JL_SIGATOMIC_BEGIN();
    JL_LOCK(dump); // Might GC
    arraylist_new(&backref_list, 4000);
    arraylist_push(&backref_list, jl_main_module);
    arraylist_new(&flagref_list, 0);
    arraylist_new(&methtable_list, 0);

    int en = jl_gc_enable(0);
    DUMP_MODES last_mode = mode;
    mode = MODE_MODULE;
    jl_array_t *restored = NULL;
    jl_array_t *init_order = NULL;
    restored = (jl_array_t*)jl_deserialize_value(f, (jl_value_t**)&restored);

    jl_recache_types();
    jl_finalize_deserializer(f); // done with MODE_MODULE

    // at this point, the AST is fully reconstructed, but still completely disconnected
    // in postwork mode, all of the interconnects will be created
    mode = MODE_MODULE_POSTWORK;
    jl_deserialize_lambdas_from_mod(f); // hook up methods of external generic functions
    init_order = jl_finalize_deserializer(f); // done with f

    // Resort the internal method tables
    size_t i;
    for (i = 0; i < methtable_list.len; i++) {
        jl_methtable_t *mt = (jl_methtable_t*)methtable_list.items[i];
        jl_array_t *cache_targ = mt->cache_targ;
        jl_array_t *cache_arg1 = mt->cache_arg1;
        mt->cache_targ = (jl_array_t*)jl_nothing;
        mt->cache_arg1 = (jl_array_t*)jl_nothing;
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
    jl_gc_enable(en);
    arraylist_free(&flagref_list);
    arraylist_free(&methtable_list);
    arraylist_free(&backref_list);
    ios_close(f);
    JL_UNLOCK(dump);
    JL_SIGATOMIC_END();

    JL_GC_PUSH2(&init_order,&restored);
    jl_init_restored_modules(init_order);
    JL_GC_POP();

    return restored;
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental_from_buf(const char *buf,
                                                         size_t sz)
{
    ios_t f;
    jl_array_t *modules;
    ios_static_buffer(&f, (char*)buf, sz);
    modules = _jl_restore_incremental(&f);
    return modules ? (jl_value_t*) modules : jl_nothing;
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental(const char *fname)
{
    ios_t f;
    jl_array_t *modules;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL) {
        jl_printf(JL_STDERR, "Cache file \"%s\" not found\n", fname);
        return jl_nothing;
    }
    modules = _jl_restore_incremental(&f);
    return modules ? (jl_value_t*) modules : jl_nothing;
}

// --- init ---

void jl_init_serializer(void)
{
    htable_new(&ser_tag, 0);
    htable_new(&common_symbol_tag, 0);
    htable_new(&fptr_to_id, sizeof(id_to_fptrs)/sizeof(*id_to_fptrs));
    htable_new(&backref_table, 0);

    void *tags[] = { jl_symbol_type, jl_gensym_type, jl_datatype_type,
                     jl_simplevector_type, jl_array_type, jl_slot_type,
                     jl_expr_type, (void*)LongSymbol_tag, (void*)LongSvec_tag,
                     (void*)LongExpr_tag, (void*)LiteralVal_tag,
                     (void*)SmallInt64_tag, (void*)SmallDataType_tag,
                     (void*)Int32_tag, (void*)Array1d_tag, (void*)Singleton_tag,
                     jl_module_type, jl_tvar_type, jl_lambda_info_type,
                     (void*)CommonSym_tag, (void*)NearbyGlobal_tag, jl_globalref_type,
                     // everything above here represents a class of object rather only than a literal

                     jl_emptysvec, jl_emptytuple, jl_false, jl_true, jl_nothing, jl_any_type,
                     call_sym, goto_ifnot_sym, return_sym, body_sym, line_sym,
                     lambda_sym, jl_symbol("tuple"), assign_sym,

                     // empirical list of very common symbols
                     #include "common_symbols1.inc"

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
#endif
                     jl_labelnode_type, jl_linenumbernode_type,
                     jl_gotonode_type, jl_quotenode_type, jl_topnode_type,
                     jl_type_type, jl_bottom_type, jl_ref_type, jl_pointer_type,
                     jl_vararg_type, jl_ntuple_type, jl_abstractarray_type,
                     jl_densearray_type, jl_void_type, jl_function_type,
                     jl_typector_type, jl_typename_type, jl_builtin_type,
                     jl_task_type, jl_uniontype_type, jl_typetype_type, jl_typetype_tvar,
                     jl_ANY_flag, jl_array_any_type, jl_intrinsic_type, jl_method_type,
                     jl_methtable_type, jl_voidpointer_type, jl_newvarnode_type,
                     jl_array_symbol_type, jl_anytuple_type, jl_tparam0(jl_anytuple_type),
                     jl_typeof(jl_emptytuple),
                     jl_symbol_type->name, jl_gensym_type->name, jl_tuple_typename,
                     jl_ref_type->name, jl_pointer_type->name, jl_simplevector_type->name,
                     jl_datatype_type->name, jl_uniontype_type->name, jl_array_type->name,
                     jl_expr_type->name, jl_typename_type->name, jl_type_type->name,
                     jl_methtable_type->name, jl_method_type->name, jl_tvar_type->name,
                     jl_ntuple_type->name, jl_abstractarray_type->name, jl_vararg_type->name,
                     jl_densearray_type->name, jl_void_type->name, jl_lambda_info_type->name,
                     jl_module_type->name, jl_function_type->name, jl_slot_type->name,
                     jl_typector_type->name, jl_intrinsic_type->name, jl_task_type->name,
                     jl_labelnode_type->name, jl_linenumbernode_type->name, jl_builtin_type->name,
                     jl_gotonode_type->name, jl_quotenode_type->name, jl_topnode_type->name,
                     jl_globalref_type->name,

                     jl_root_task,

                     NULL };

    // more common symbols, less common than those above. will get 2-byte encodings.
    void *common_symbols[] = {
        #include "common_symbols2.inc"
        NULL
    };

    intptr_t i=2;
    while (tags[i-2] != NULL) {
        ptrhash_put(&ser_tag, tags[i-2], (void*)i);
        deser_tag[i] = (jl_value_t*)tags[i-2];
        i += 1;
    }
    assert(i <= Null_tag);
    VALUE_TAGS = (intptr_t)ptrhash_get(&ser_tag, jl_emptysvec);

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
