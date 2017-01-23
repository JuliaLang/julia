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
// (not used in MODE_AST)
static htable_t backref_table;
int backref_table_numel;
static arraylist_t backref_list;

// list of (jl_value_t **loc, size_t pos) entries
// for anything that was flagged by the deserializer for later
// type-rewriting of some sort
// (not used in MODE_AST)
static arraylist_t flagref_list;

// list of (size_t pos, (void *f)(jl_value_t*)) entries
// for the serializer to mark values in need of rework by function f
// during deserialization later
// (not used in MODE_AST)
static arraylist_t reinit_list;

// list of stuff that is being serialized
// (only used by the incremental serializer in MODE_MODULE)
static jl_array_t *serializer_worklist;

// list of modules being deserialized with __init__ methods
// (not used in MODE_AST)
jl_array_t *jl_module_init_order;

// hash of definitions for predefined function pointers
static htable_t fptr_to_id;
// array of definitions for the predefined function pointers
// (reverse of fptr_to_id)
static const jl_fptr_t id_to_fptrs[] = {
  NULL, NULL,
  jl_f_throw, jl_f_is, jl_f_typeof, jl_f_issubtype, jl_f_isa,
  jl_f_typeassert, jl_f__apply, jl_f__apply_pure, jl_f_isdefined,
  jl_f_tuple, jl_f_svec, jl_f_intrinsic_call,
  jl_f_getfield, jl_f_setfield, jl_f_fieldtype, jl_f_nfields,
  jl_f_arrayref, jl_f_arrayset, jl_f_arraysize, jl_f_apply_type,
  jl_f_applicable, jl_f_invoke, jl_unprotect_stack, jl_f_sizeof, jl_f__expr,
  NULL };

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
    // compressing / decompressing an AST Expr in a MethodInstance
    MODE_AST,

    // jl_restore_system_image
    // restoring an entire system image from disk
    MODE_SYSTEM_IMAGE,

    // jl_restore_new_module
    // restoring a single module from disk for integration
    // into the currently running system image / environment
    MODE_MODULE
} DUMP_MODES;

typedef struct {
    ios_t *s;
    DUMP_MODES mode;
    // pointers to non-AST-ish objects in a compressed tree
    // (only used in MODE_AST)
    jl_array_t *tree_literal_values;
    jl_module_t *tree_enclosing_module;
    jl_ptls_t ptls;
} jl_serializer_state;

static jl_value_t *jl_idtable_type = NULL;
static arraylist_t builtin_typenames;

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
static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v);
static jl_value_t *jl_deserialize_value(jl_serializer_state *s, jl_value_t **loc);
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

static jl_value_t *jl_deserialize_gv(jl_serializer_state *s, jl_value_t *v)
{
    // Restore the GlobalVariable reference to this jl_value_t via the sysimg_gvars table
    int32_t gvname_index = read_int32(s->s)-1;
    if (sysimg_gvars != NULL && gvname_index >= 0 && s->mode == MODE_SYSTEM_IMAGE) {
        *sysimg_gvars[gvname_index] = v;
    }
    return v;
}

static void jl_serialize_gv(jl_serializer_state *s, jl_value_t *v)
{
    // write the index of the literal_pointer_val into the system image
    write_int32(s->s, jl_get_llvm_gv(v));
}

static void jl_serialize_globalvals(jl_serializer_state *s)
{
    size_t i, len = backref_table.size;
    void **p = backref_table.table;
    for(i=0; i < len; i+=2) {
        char *offs = (char*)p[i+1];
        if (offs != HT_NOTFOUND) {
            uintptr_t pos = offs - (char*)HT_NOTFOUND - 1;
            int32_t gv = jl_get_llvm_gv((jl_value_t*)p[i]);
            if (gv != 0) {
                write_int32(s->s, pos + 1);
                write_int32(s->s, gv);
            }
        }
    }
    write_int32(s->s, 0);
}

static void jl_deserialize_globalvals(jl_serializer_state *s)
{
    while (1) {
        intptr_t key = read_int32(s->s);
        if (key == 0) break;
        jl_deserialize_gv(s, (jl_value_t*)backref_list.items[key - 1]);
    }
}

static void jl_serialize_gv_syms(jl_serializer_state *s, jl_sym_t *v)
{
    // since symbols are static, they might not have had a
    // reference anywhere in the code image other than here
    void *bp = ptrhash_get(&backref_table, v);
    if (bp == HT_NOTFOUND) {
        int32_t gv = jl_get_llvm_gv((jl_value_t*)v);
        if (gv != 0) {
            jl_serialize_value(s, v);
            write_int32(s->s, gv);
        }
    }
    if (v->left) jl_serialize_gv_syms(s, v->left);
    if (v->right) jl_serialize_gv_syms(s, v->right);
}

static void jl_serialize_gv_others(jl_serializer_state *s)
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
                write_int32(s->s, gv32);
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
                write_int32(s->s, gv64);
            }
        }
    }
    jl_serialize_gv_syms(s, jl_get_root_symbol());
    jl_serialize_value(s, NULL); // signal the end of this list
}

static void jl_deserialize_gv_others(jl_serializer_state *s)
{
    while (1) {
        jl_value_t *v = jl_deserialize_value(s, NULL);
        if (!v) break;
        jl_deserialize_gv(s, v);
    }
}

static struct delayed_fptrs_t {
    jl_method_instance_t *li;
    int32_t func;
    int32_t cfunc;
} *delayed_fptrs = NULL;
static size_t delayed_fptrs_n = 0;
static size_t delayed_fptrs_max = 0;
static size_t sysimg_fvars_max = 0;

static void jl_delayed_fptrs(jl_method_instance_t *li, int32_t func, int32_t cfunc)
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

void jl_register_fptrs(uint64_t sysimage_base, void **fptrs, jl_method_instance_t **linfos, size_t n);

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
    jl_method_instance_t **linfos = (jl_method_instance_t**)malloc(sizeof(jl_method_instance_t*) * sysimg_fvars_max);
    for (i = 0; i < delayed_fptrs_n; i++) {
        jl_method_instance_t *li = delayed_fptrs[i].li;
        assert(li->def);
        int32_t cfunc = delayed_fptrs[i].cfunc - 1;
        if (cfunc >= 0) {
            jl_fptr_to_llvm((jl_fptr_t)fvars[cfunc], li, 1);
            linfos[cfunc] = li;
        }
        int32_t func = delayed_fptrs[i].func - 1;
        if (func >= 0) {
            jl_fptr_to_llvm((jl_fptr_t)fvars[func], li, 0);
            linfos[func] = li;
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

static uint16_t jl_fptr_id(void *fptr)
{
    void **pbp = ptrhash_bp(&fptr_to_id, fptr);
    if (*pbp == HT_NOTFOUND || fptr == NULL)
        return 1;
    else
        return *(intptr_t*)pbp;
}

static int module_in_worklist(jl_module_t *mod)
{
    int i, l = jl_array_len(serializer_worklist);
    for (i = 0; i < l; i++) {
        jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(serializer_worklist, i);
        if (jl_is_module(workmod) && jl_is_submodule(mod, workmod))
            return 1;
    }
    return 0;
}

// compute whether a type references something internal to worklist
// and thus could not have existed before deserialize
// and thus does not need delayed unique-ing
static int type_in_worklist(jl_datatype_t *dt)
{
    if (module_in_worklist(dt->name->module))
        return 1;
    int i, l = jl_svec_len(dt->parameters);
    for (i = 0; i < l; i++) {
        jl_value_t *p = jl_unwrap_unionall(jl_tparam(dt, i));
        if (type_in_worklist((jl_datatype_t*)(jl_is_datatype(p) ? p : jl_typeof(p))))
            return 1;
    }
    return 0;
}

static int type_recursively_external(jl_datatype_t *dt);

static int type_parameter_recursively_external(jl_value_t *p0)
{
    jl_datatype_t *p = (jl_datatype_t*)p0;
    while (jl_is_unionall(p)) {
        if (!type_parameter_recursively_external(((jl_unionall_t*)p)->var->lb))
            return 0;
        if (!type_parameter_recursively_external(((jl_unionall_t*)p)->var->ub))
            return 0;
        p = (jl_datatype_t*)((jl_unionall_t*)p)->body;
    }
    if (!jl_is_datatype(p) || p->uid == 0)
        return 0;
    if (module_in_worklist(p->name->module))
        return 0;
    if (p->name->wrapper != (jl_value_t*)p0) {
        if (!type_recursively_external(p))
            return 0;
    }
    return 1;
}

// returns true if all of the parameters are tag 6 or 7
static int type_recursively_external(jl_datatype_t *dt)
{
    if (dt->uid == 0)
        return 0;
    if (jl_svec_len(dt->parameters) == 0)
        return 1;

    int i, l = jl_svec_len(dt->parameters);
    for (i = 0; i < l; i++) {
        if (!type_parameter_recursively_external(jl_tparam(dt, i)))
            return 0;
    }
    return 1;
}


static void jl_serialize_datatype(jl_serializer_state *s, jl_datatype_t *dt)
{
    int tag = 0;
    if (s->mode == MODE_MODULE) {
        int internal = module_in_worklist(dt->name->module);
        if (!internal && jl_unwrap_unionall(dt->name->wrapper) == (jl_value_t*)dt) {
            tag = 6; // external primary type
        }
        else if (dt->uid == 0) {
            tag = 0; // normal struct
        }
        else if (internal) {
            if (jl_unwrap_unionall(dt->name->wrapper) == (jl_value_t*)dt) // comes up often since functions create types
                tag = 5; // internal, and not in the typename cache (just needs uid reassigned)
            else
                tag = 10; // anything else that's internal (just needs uid reassigned and possibly recaching)
        }
        else if (type_recursively_external(dt)) {
            tag = 7; // external type that can be immediately recreated (with apply_type)
        }
        else if (type_in_worklist(dt)) {
            tag = 10; // external, but definitely new (still needs uid and caching, but not full unique-ing)
        }
        else {
            // this'll need a uid and unique-ing later
            // flag this in the backref table as special
            uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, dt);
            assert(*bp != (uintptr_t)HT_NOTFOUND);
            *bp |= 1;
            tag = 10;
        }
    }
    else if (dt == jl_int32_type)
        tag = 2;
    else if (dt == jl_bool_type)
        tag = 3;
    else if (dt == jl_int64_type)
        tag = 4;
    else if (dt == jl_uint8_type)
        tag = 8;

    if (strncmp(jl_symbol_name(dt->name->name), "#kw#", 4) == 0) {
        /* XXX: yuck, but the auto-generated kw types from the serializer isn't a real type, so we *must* be very careful */
        assert(tag == 0 || tag == 5 || tag == 6 || tag == 10);
        if (tag == 6) {
            jl_methtable_t *mt = dt->name->mt;
            jl_datatype_t *primarydt = (jl_datatype_t*)jl_unwrap_unionall(jl_get_global(mt->module, mt->name));
            assert(jl_is_datatype(primarydt));
            assert(jl_typeof(primarydt->name->mt->kwsorter) == (jl_value_t*)dt);
            dt = primarydt;
            tag = 9;
        }
    }

    writetag(s->s, (jl_value_t*)SmallDataType_tag);
    write_uint8(s->s, 0); // virtual size
    jl_serialize_value(s, (jl_value_t*)jl_datatype_type);
    write_uint8(s->s, tag);
    if (tag == 6) {
        jl_serialize_value(s, dt->name);
        return;
    }
    if (tag == 7) {
        jl_serialize_value(s, dt->name);
        jl_serialize_value(s, dt->parameters);
        return;
    }
    if (tag == 9) {
        jl_serialize_value(s, dt);
        return;
    }

    write_int32(s->s, dt->size);
    int has_instance = (dt->instance != NULL);
    int has_layout = (dt->layout != NULL);
    write_uint8(s->s, dt->abstract | (dt->mutabl<<1) | (has_layout<<2) | (has_instance<<3) |
                (dt->hasfreetypevars<<4) | (dt->isleaftype<<5));
    write_int32(s->s, dt->depth);
    if (!dt->abstract) {
        write_uint16(s->s, dt->ninitialized);
        if (s->mode != MODE_MODULE) {
            write_int32(s->s, dt->uid);
        }
    }

    if (has_layout) {
        uint8_t layout = 0;
        if (dt->layout == ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_array_type))->layout) {
            layout = 1;
        }
        else if (dt->layout == jl_void_type->layout) {
            layout = 2;
        }
        else if (dt->layout == ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_pointer_type))->layout) {
            layout = 3;
        }
        write_uint8(s->s, layout);
        if (layout == 0) {
            size_t nf = dt->layout->nfields;
            write_uint16(s->s, nf);
            write_int8(s->s, dt->layout->fielddesc_type);
            write_int32(s->s, dt->layout->alignment);
            write_int8(s->s, dt->layout->haspadding);
            write_int8(s->s, dt->layout->pointerfree);
            size_t fieldsize = jl_fielddesc_size(dt->layout->fielddesc_type);
            ios_write(s->s, (char*)(&dt->layout[1]), nf * fieldsize);
        }
    }

    if (has_instance)
        jl_serialize_value(s, dt->instance);
    jl_serialize_value(s, dt->name);
    jl_serialize_value(s, dt->parameters);
    jl_serialize_value(s, dt->super);
    jl_serialize_value(s, dt->types);
}

static void jl_serialize_module(jl_serializer_state *s, jl_module_t *m)
{
    writetag(s->s, jl_module_type);
    jl_serialize_value(s, m->name);
    int ref_only = 0;
    if (s->mode == MODE_MODULE) {
        if (!module_in_worklist(m))
            ref_only = 1;
        write_int8(s->s, ref_only);
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
                write_int8(s->s, (b->deprecated<<3) | (b->constp<<2) | (b->exportp<<1) | (b->imported));
                jl_serialize_gv(s, (jl_value_t*)b);
            }
        }
    }
    jl_serialize_value(s, NULL);
    if (m == jl_main_module) {
        write_int32(s->s, 1);
        jl_serialize_value(s, (jl_value_t*)jl_core_module);
    }
    else {
        write_int32(s->s, m->usings.len);
        for(i=0; i < m->usings.len; i++) {
            jl_serialize_value(s, (jl_value_t*)m->usings.items[i]);
        }
    }
    write_uint8(s->s, m->istopmod);
    write_uint64(s->s, m->uuid);
    write_int32(s->s, m->counter);
}

static int is_ast_node(jl_value_t *v)
{
    // TODO: this accidentally copies QuoteNode(Expr(...)) and QuoteNode(svec(...))
    return jl_is_symbol(v) || jl_is_slot(v) || jl_is_ssavalue(v) ||
        jl_is_expr(v) || jl_is_newvarnode(v) || jl_is_svec(v) || jl_is_tuple(v) ||
        jl_is_uniontype(v) || jl_is_int32(v) || jl_is_int64(v) ||
        jl_is_bool(v) || jl_is_quotenode(v) || jl_is_gotonode(v) ||
        jl_is_labelnode(v) || jl_is_linenode(v) || jl_is_globalref(v);
}

static int literal_val_id(jl_serializer_state *s, jl_value_t *v)
{
    int i, l = jl_array_len(s->tree_literal_values);
    for (i = 0; i < l; i++) {
        if (jl_egal(jl_array_ptr_ref(s->tree_literal_values, i), v))
            return i;
    }
    jl_array_ptr_1d_push(s->tree_literal_values, v);
    return jl_array_len(s->tree_literal_values) - 1;
}

static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v)
{
    if (v == NULL) {
        write_uint8(s->s, Null_tag);
        return;
    }

    void **bp = ptrhash_bp(&ser_tag, v);
    if (*bp != HT_NOTFOUND) {
        write_as_tag(s->s, (uint8_t)(intptr_t)*bp);
        return;
    }
    if (jl_is_symbol(v)) {
        void *idx = ptrhash_get(&common_symbol_tag, v);
        if (idx != HT_NOTFOUND) {
            writetag(s->s, (jl_value_t*)CommonSym_tag);
            write_uint8(s->s, (uint8_t)(size_t)idx);
            return;
        }
    }

    if (s->mode == MODE_AST) {
        // compressing tree
        if (!is_ast_node(v)) {
            writetag(s->s, (jl_value_t*)LiteralVal_tag);
            int id = literal_val_id(s, v);
            assert(id >= 0 && id < UINT16_MAX);
            write_uint16(s->s, id);
            return;
        }
    }
    else {
        bp = ptrhash_bp(&backref_table, v);
        if (*bp != HT_NOTFOUND) {
            uintptr_t pos = (char*)*bp - (char*)HT_NOTFOUND - 1;
            if (pos < 65536) {
                write_uint8(s->s, ShortBackRef_tag);
                write_uint16(s->s, pos);
            }
            else {
                write_uint8(s->s, BackRef_tag);
                write_int32(s->s, pos);
            }
            return;
        }
        intptr_t pos = backref_table_numel++;
        if (jl_typeof(v) == jl_idtable_type) {
            // will need to rehash this, later (after types are fully constructed)
            arraylist_push(&reinit_list, (void*)pos);
            arraylist_push(&reinit_list, (void*)1);
        }
        if (s->mode == MODE_MODULE) {
            if (jl_is_module(v)) {
                jl_module_t *m = (jl_module_t*)v;
                if (module_in_worklist(m) && !module_in_worklist(m->parent)) {
                    // will need to reinsert this into parent bindings, later (in case of any errors during reinsert)
                    arraylist_push(&reinit_list, (void*)pos);
                    arraylist_push(&reinit_list, (void*)2);
                }
            }
        }
        if (s->mode == MODE_MODULE) {
            // TypeMapLevels need to be rehashed
            if (jl_is_mtable(v)) {
                arraylist_push(&reinit_list, (void*)pos);
                arraylist_push(&reinit_list, (void*)3);
            }
            if (jl_is_method(v) && jl_typeof(((jl_method_t*)v)->specializations.unknown) == (jl_value_t*)jl_typemap_level_type) {
                arraylist_push(&reinit_list, (void*)pos);
                arraylist_push(&reinit_list, (void*)4);
            }
        }
        if (s->mode == MODE_MODULE)
            pos <<= 1;
        ptrhash_put(&backref_table, v, (char*)HT_NOTFOUND + pos + 1);
    }

    size_t i;
    if (jl_is_svec(v)) {
        size_t l = jl_svec_len(v);
        if (l <= 255) {
            writetag(s->s, jl_simplevector_type);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            writetag(s->s, (jl_value_t*)LongSvec_tag);
            write_int32(s->s, l);
        }
        for(i=0; i < l; i++) {
            jl_serialize_value(s, jl_svecref(v, i));
        }
    }
    else if (jl_is_symbol(v)) {
        size_t l = strlen(jl_symbol_name((jl_sym_t*)v));
        if (l <= 255) {
            writetag(s->s, jl_symbol_type);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            writetag(s->s, (jl_value_t*)LongSymbol_tag);
            write_int32(s->s, l);
        }
        ios_write(s->s, jl_symbol_name((jl_sym_t*)v), l);
    }
    else if (jl_is_globalref(v)) {
        if (s->mode == MODE_AST && jl_globalref_mod(v) == s->tree_enclosing_module) {
            writetag(s->s, (jl_value_t*)NearbyGlobal_tag);
            jl_serialize_value(s, jl_globalref_name(v));
        }
        else {
            writetag(s->s, (jl_value_t*)jl_globalref_type);
            jl_serialize_value(s, jl_globalref_mod(v));
            jl_serialize_value(s, jl_globalref_name(v));
        }
    }
    else if (jl_is_ssavalue(v) && ((jl_ssavalue_t*)v)->id < 65536) {
        writetag(s->s, (jl_value_t*)jl_ssavalue_type);
        write_uint16(s->s, ((jl_ssavalue_t*)v)->id);
    }
    else if (jl_typeis(v,jl_slotnumber_type) && jl_slot_number(v) < 65536) {
        writetag(s->s, (jl_value_t*)jl_slotnumber_type);
        write_uint16(s->s, jl_slot_number(v));
    }
    else if (jl_is_array(v)) {
        jl_array_t *ar = (jl_array_t*)v;
        if (ar->flags.ndims == 1 && ar->elsize < 128) {
            writetag(s->s, (jl_value_t*)Array1d_tag);
            write_uint8(s->s, (ar->flags.ptrarray<<7) | (ar->elsize & 0x7f));
        }
        else {
            writetag(s->s, (jl_value_t*)jl_array_type);
            write_uint16(s->s, ar->flags.ndims);
            write_uint16(s->s, (ar->flags.ptrarray<<15) | (ar->elsize & 0x7fff));
        }
        for (i=0; i < ar->flags.ndims; i++)
            jl_serialize_value(s, jl_box_long(jl_array_dim(ar,i)));
        jl_serialize_value(s, jl_typeof(ar));
        if (!ar->flags.ptrarray) {
            size_t tot = jl_array_len(ar) * ar->elsize;
            ios_write(s->s, (char*)jl_array_data(ar), tot);
        }
        else {
            for(i=0; i < jl_array_len(ar); i++) {
                jl_serialize_value(s, jl_array_ptr_ref(v, i));
            }
        }
    }
    else if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        size_t l = jl_array_len(e->args);
        if (l <= 255) {
            writetag(s->s, jl_expr_type);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            writetag(s->s, (jl_value_t*)LongExpr_tag);
            write_int32(s->s, l);
        }
        jl_serialize_value(s, e->head);
        jl_serialize_value(s, e->etype);
        for (i = 0; i < l; i++) {
            jl_serialize_value(s, jl_exprarg(e, i));
        }
    }
    else if (jl_is_datatype(v)) {
        jl_serialize_datatype(s, (jl_datatype_t*)v);
    }
    else if (jl_is_typevar(v)) {
        writetag(s->s, jl_tvar_type);
        jl_serialize_value(s, ((jl_tvar_t*)v)->name);
        jl_serialize_value(s, ((jl_tvar_t*)v)->lb);
        jl_serialize_value(s, ((jl_tvar_t*)v)->ub);
    }
    else if (jl_is_method(v)) {
        writetag(s->s, jl_method_type);
        jl_method_t *m = (jl_method_t*)v;
        int internal = 1;
        int external_mt = 0;
        if (s->mode == MODE_MODULE) {
            internal = module_in_worklist(m->module);
            if (!internal) {
                // flag this in the backref table as special
                uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, v);
                assert(*bp != (uintptr_t)HT_NOTFOUND);
                *bp |= 1;
            }
        }
        jl_serialize_value(s, (jl_value_t*)m->sig);
        if (s->mode == MODE_MODULE) {
            write_uint8(s->s, internal);
            if (!internal)
                return;
            jl_datatype_t *gf = jl_first_argument_datatype((jl_value_t*)m->sig);
            assert(jl_is_datatype(gf) && gf->name->mt);
            external_mt = !module_in_worklist(gf->name->mt->module);
        }
        union jl_typemap_t *tf = &m->specializations;
        jl_serialize_value(s, tf->unknown);
        jl_serialize_value(s, (jl_value_t*)m->name);
        write_int8(s->s, m->isstaged);
        jl_serialize_value(s, (jl_value_t*)m->file);
        write_int32(s->s, m->line);
        if (s->mode != MODE_MODULE) {
            write_int32(s->s, m->min_world);
            write_int32(s->s, m->max_world);
        }
        else {
            assert(m->max_world == ~(size_t)0 && "method replacement cannot be handled by incremental serializer");
        }
        jl_serialize_value(s, (jl_value_t*)m->tvars);
        if (external_mt)
            jl_serialize_value(s, jl_nothing);
        else
            jl_serialize_value(s, (jl_value_t*)m->ambig);
        write_int8(s->s, m->called);
        write_int32(s->s, m->nargs);
        write_int8(s->s, m->isva);
        jl_serialize_value(s, (jl_value_t*)m->module);
        jl_serialize_value(s, (jl_value_t*)m->sparam_syms);
        jl_serialize_value(s, (jl_value_t*)m->roots);
        jl_serialize_value(s, (jl_value_t*)m->source);
        jl_serialize_value(s, (jl_value_t*)m->unspecialized);
        jl_serialize_value(s, (jl_value_t*)m->generator);
        jl_serialize_value(s, (jl_value_t*)m->invokes.unknown);
    }
    else if (jl_is_method_instance(v)) {
        writetag(s->s, jl_method_instance_type);
        jl_method_instance_t *li = (jl_method_instance_t*)v;
        int internal = 0;
        if (s->mode == MODE_MODULE) {
            if (li->max_world == 0 && li->min_world == 0) {
                internal = 1; // not world-tracked
            }
            else if (!li->def || module_in_worklist(li->def->module)) {
                if (li->max_world == ~(size_t)0) {
                    internal = 2; // update world on deserialization
                }
                else {
                    internal = 3; // garbage object :(
                }
            }
            if (!internal) {
                // also flag this in the backref table as special
                uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, v);
                assert(*bp != (uintptr_t)HT_NOTFOUND);
                *bp |= 1;
            }
        }
        jl_serialize_value(s, (jl_value_t*)li->specTypes);
        if (s->mode == MODE_MODULE && !internal)
            jl_serialize_value(s, (jl_value_t*)li->def->sig);
        else
            jl_serialize_value(s, (jl_value_t*)li->def);
        if (s->mode == MODE_MODULE) {
            write_uint8(s->s, internal);
            if (!internal)
                return;
        }
        jl_serialize_value(s, li->inferred);
        jl_serialize_value(s, li->inferred_const);
        jl_serialize_value(s, li->rettype);
        jl_serialize_value(s, (jl_value_t*)li->sparam_vals);
        jl_serialize_value(s, (jl_value_t*)li->backedges);
        if (s->mode != MODE_MODULE) {
            write_int32(s->s, li->min_world);
            write_int32(s->s, li->max_world);
        }
        if (li->def) {
            uint16_t id = jl_fptr_id((void*)(uintptr_t)li->fptr);
            if (li->jlcall_api == 2) {
                write_int8(s->s, 2);
            }
            else if (id >= 2) {
                write_int8(s->s, -li->jlcall_api);
                write_uint16(s->s, id);
            }
            else if (li->functionObjectsDecls.functionObject) {
                int jlcall_api = jl_jlcall_api(li->functionObjectsDecls.functionObject);
                assert(jlcall_api);
                // save functionObject pointers
                write_int8(s->s, jlcall_api);
                write_int32(s->s, jl_assign_functionID(li->functionObjectsDecls.functionObject));
                write_int32(s->s, jl_assign_functionID(li->functionObjectsDecls.specFunctionObject));
            }
            else {
                write_int8(s->s, 0);
            }
        }
        else {
            write_int8(s->s, li->jlcall_api);
        }
    }
    else if (jl_typeis(v, jl_module_type)) {
        jl_serialize_module(s, (jl_module_t*)v);
    }
    else if (jl_typeis(v, jl_task_type)) {
        jl_error("Task cannot be serialized");
    }
    else if (jl_typeis(v, jl_string_type)) {
        writetag(s->s, jl_string_type);
        write_int32(s->s, jl_string_len(v));
        ios_write(s->s, jl_string_data(v), jl_string_len(v));
    }
    else if (jl_typeis(v, jl_typemap_entry_type)) {
        writetag(s->s, jl_typemap_entry_type);
        size_t n = 0;
        jl_typemap_entry_t *te = (jl_typemap_entry_t*)v;
        while ((jl_value_t*)te != jl_nothing) {
            n++; te = te->next;
        }
        write_int32(s->s, n);
        te = (jl_typemap_entry_t*)v;
        size_t i, nf = jl_datatype_nfields(jl_typemap_entry_type);
        while ((jl_value_t*)te != jl_nothing) {
            for (i = 1; i < nf; i++) {
                if (jl_field_size(jl_typemap_entry_type, i) > 0)
                    jl_serialize_value(s, jl_get_nth_field((jl_value_t*)te, i));
            }
            te = te->next;
        }
    }
    else {
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        void *data = jl_data_ptr(v);
        if (t == jl_int64_type &&
            *(int64_t*)data >= S32_MIN && *(int64_t*)data <= S32_MAX) {
            writetag(s->s, (jl_value_t*)SmallInt64_tag);
            write_int32(s->s, (int32_t)*(int64_t*)data);
        }
        else if (t == jl_int32_type) {
            writetag(s->s, (jl_value_t*)Int32_tag);
            write_int32(s->s, (int32_t)*(int32_t*)data);
        }
        else {
            if (v == t->instance) {
                if (s->mode == MODE_MODULE && !type_in_worklist(t)) {
                    // also flag this in the backref table as special
                    // if it might not be unique (is external)
                    uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, v);
                    assert(*bp != (uintptr_t)HT_NOTFOUND);
                    *bp |= 1;
                }
                writetag(s->s, (jl_value_t*)Singleton_tag);
                jl_serialize_value(s, t);
                return;
            }
            if (t->size <= 255) {
                writetag(s->s, (jl_value_t*)SmallDataType_tag);
                write_uint8(s->s, t->size);
            }
            else {
                writetag(s->s, (jl_value_t*)jl_datatype_type);
                write_int32(s->s, t->size);
            }
            jl_serialize_value(s, t);
            if (s->mode == MODE_MODULE) {
                if (t == jl_typename_type) {
                    if (module_in_worklist(((jl_typename_t*)v)->module)) {
                        write_uint8(s->s, 0);
                    }
                    else {
                        write_uint8(s->s, 1);
                        jl_typename_t *tn = (jl_typename_t*)v;
                        jl_serialize_value(s, tn->module);
                        jl_serialize_value(s, tn->name);
                        return;
                    }
                }
                if (t == jl_unionall_type) {
                    jl_datatype_t *d = (jl_datatype_t*)jl_unwrap_unionall(v);
                    if (jl_is_datatype(d) && d->name->wrapper == v &&
                        !module_in_worklist(d->name->module)) {
                        write_uint8(s->s, 1);
                        jl_serialize_value(s, d->name->module);
                        jl_serialize_value(s, d->name->name);
                        return;
                    }
                    else {
                        write_uint8(s->s, 0);
                    }
                }
                if (t == jl_typemap_level_type) {
                    // perform some compression on the typemap levels
                    // (which will need to be rehashed during deserialization anyhow)
                    jl_typemap_level_t *node = (jl_typemap_level_t*)v;
                    assert( // make sure this type has the expected ordering
                        offsetof(jl_typemap_level_t, arg1) == 0 * sizeof(jl_value_t*) &&
                        offsetof(jl_typemap_level_t, targ) == 2 * sizeof(jl_value_t*) &&
                        offsetof(jl_typemap_level_t, linear) == 4 * sizeof(jl_value_t*) &&
                        offsetof(jl_typemap_level_t, any) == 5 * sizeof(jl_value_t*) &&
                        offsetof(jl_typemap_level_t, key) == 6 * sizeof(jl_value_t*) &&
                        sizeof(jl_typemap_level_t) == 7 * sizeof(jl_value_t*));
                    jl_serialize_value(s, jl_nothing);
                    jl_serialize_value(s, node->arg1.values);
                    jl_serialize_value(s, jl_nothing);
                    jl_serialize_value(s, node->targ.values);
                    jl_serialize_value(s, node->linear);
                    jl_serialize_value(s, node->any.unknown);
                    jl_serialize_value(s, node->key);
                    return;
                }
            }
            size_t nf = jl_datatype_nfields(t);
            if (nf == 0 && jl_datatype_size(t)>0) {
                if (t->name == jl_pointer_typename && jl_unbox_voidpointer(v) != (void*)-1) {
                    // normalize most pointers to NULL, to help catch memory errors
                    // but permit MAP_FAILED / INVALID_HANDLE to be stored unchanged
                    write_int32(s->s, 0);
#ifdef _P64
                    write_int32(s->s, 0);
#endif
                }
                else {
                    ios_write(s->s, (char*)data, jl_datatype_size(t));
                }
            }
            else {
                size_t i;
                for (i = 0; i < nf; i++) {
                    if (jl_field_size(t, i) > 0) {
                        jl_serialize_value(s, jl_get_nth_field(v, i));
                    }
                }
            }
        }
    }
}

static void jl_serialize_missing_backedges_to_mod(jl_serializer_state *s, jl_methtable_t *mt)
{
    jl_array_t *backedges = mt->backedges;
    if (backedges) {
        size_t i, l = jl_array_len(backedges);
        for (i = 1; i < l; i += 2) {
            jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(backedges, i);
            if (caller->max_world == ~(size_t)0 && module_in_worklist(caller->def->module)) {
                jl_serialize_value(s, caller);
                jl_serialize_value(s, jl_array_ptr_ref(backedges, i - 1));
            }
        }
    }
}

static int jl_serialize_backedges_to_mod(jl_typemap_entry_t *ml, void *closure)
{
    jl_serializer_state *s = (jl_serializer_state*)closure;
    jl_method_instance_t *callee = ml->func.linfo;
    jl_array_t *backedges = callee->backedges;
    if (backedges) {
        assert(callee->max_world == ~(size_t)0);
        size_t i, l = jl_array_len(backedges);
        for (i = 0; i < l; i++) {
            jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(backedges, i);
            if (caller->max_world == ~(size_t)0 && module_in_worklist(caller->def->module)) {
                jl_serialize_value(s, caller);
                jl_serialize_value(s, callee);
            }
        }
    }
    return 1;
}

static int jl_serialize_methcache_from_mod(jl_typemap_entry_t *ml, void *closure)
{
    jl_serializer_state *s = (jl_serializer_state*)closure;
    jl_method_t *m = ml->func.method;
    if (module_in_worklist(m->module)) {
        jl_serialize_value(s, m);
        jl_serialize_value(s, ml->simplesig);
    }
    else {
        jl_typemap_visitor(m->specializations, jl_serialize_backedges_to_mod, closure);
    }
    return 1;
}

static void jl_serialize_methtable_from_mod(jl_serializer_state *s, jl_typename_t *tn)
{
    jl_typemap_visitor(tn->mt->defs, jl_serialize_methcache_from_mod, (void*)s);
}

static void jl_serialize_lambdas_from_mod(jl_serializer_state *s, jl_module_t *m)
{
    if (module_in_worklist(m))
        return;
    size_t i;
    void **table = m->bindings.table;
    for (i = 1; i < m->bindings.size; i += 2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m && b->value && b->constp) {
                jl_value_t *bv = jl_unwrap_unionall(b->value);
                if (jl_is_datatype(bv)) {
                    jl_typename_t *tn = ((jl_datatype_t*)bv)->name;
                    if (tn->module == m && tn->name == b->name && tn->wrapper == b->value) {
                        jl_methtable_t *mt = tn->mt;
                        if (mt != NULL &&
                                (jl_value_t*)mt != jl_nothing &&
                                (mt != jl_type_type_mt || tn == jl_type_typename)) {
                            jl_serialize_methtable_from_mod(s, tn);
                            jl_serialize_missing_backedges_to_mod(s, mt);
                        }
                    }
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

// serialize information about all of the modules accessible directly from Main
static void write_mod_list(ios_t *s)
{
    jl_module_t *m = jl_main_module;
    size_t i;
    void **table = m->bindings.table;
    for (i = 1; i < m->bindings.size; i += 2) {
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
static const int JI_FORMAT_VERSION = 3;
static const char JI_MAGIC[] = "\373jli\r\n\032\n"; // based on PNG signature
static const uint16_t BOM = 0xFEFF; // byte-order marker
static void write_header(ios_t *s)
{
    ios_write(s, JI_MAGIC, strlen(JI_MAGIC));
    write_uint16(s, JI_FORMAT_VERSION);
    ios_write(s, (char *) &BOM, 2);
    write_uint8(s, sizeof(void*));
    ios_write(s, JL_BUILD_UNAME, strlen(JL_BUILD_UNAME)+1);
    ios_write(s, JL_BUILD_ARCH, strlen(JL_BUILD_ARCH)+1);
    ios_write(s, JULIA_VERSION_STRING, strlen(JULIA_VERSION_STRING)+1);
    const char *branch = jl_git_branch(), *commit = jl_git_commit();
    ios_write(s, branch, strlen(branch)+1);
    ios_write(s, commit, strlen(commit)+1);
}

// serialize information about the result of deserializing this file
static void write_work_list(ios_t *s)
{
    int i, l = jl_array_len(serializer_worklist);
    for (i = 0; i < l; i++) {
        jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(serializer_worklist, i);
        if (workmod->parent == jl_main_module) {
            size_t l = strlen(jl_symbol_name(workmod->name));
            write_int32(s, l);
            ios_write(s, jl_symbol_name(workmod->name), l);
            write_uint64(s, workmod->uuid);
        }
    }
    write_int32(s, 0);
}

// serialize the global _require_dependencies array of pathnames that
// are include depenencies
static void write_dependency_list(ios_t *s)
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
    jl_value_t *uniqargs[2] = {unique_func, (jl_value_t*)deps};
    size_t last_age = jl_get_ptls_states()->world_age;
    jl_get_ptls_states()->world_age = jl_world_counter;
    jl_array_t *udeps = deps && unique_func ? (jl_array_t*)jl_apply(uniqargs, 2) : NULL;
    jl_get_ptls_states()->world_age = last_age;

    JL_GC_PUSH1(&udeps);
    if (udeps) {
        size_t l = jl_array_len(udeps);
        for (size_t i=0; i < l; i++) {
            jl_value_t *dep = jl_fieldref(jl_array_ptr_ref(udeps, i), 0);
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
            jl_value_t *deptuple = jl_array_ptr_ref(udeps, i);
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

// --- deserialize ---

static jl_fptr_t jl_deserialize_fptr(jl_serializer_state *s)
{
    int fptr = read_uint16(s->s);
    if (fptr < 2)
        return NULL;

    if (fptr >= sizeof(id_to_fptrs)/sizeof(*id_to_fptrs))
        jl_error("unknown function pointer ID");
    return id_to_fptrs[fptr];
}

static jl_value_t *jl_deserialize_datatype(jl_serializer_state *s, int pos, jl_value_t **loc)
{
    int tag = read_uint8(s->s);
    if (tag == 6 || tag == 7) {
        jl_typename_t *name = (jl_typename_t*)jl_deserialize_value(s, NULL);
        jl_value_t *dtv = name->wrapper;
        if (tag == 7) {
            jl_svec_t *parameters = (jl_svec_t*)jl_deserialize_value(s, NULL);
            dtv = jl_apply_type(dtv, jl_svec_data(parameters), jl_svec_len(parameters));
        }
        else {
            dtv = jl_unwrap_unionall(dtv);
        }
        backref_list.items[pos] = dtv;
        return dtv;
    }
    if (tag == 9) {
        jl_datatype_t *primarydt = (jl_datatype_t*)jl_deserialize_value(s, NULL);
        jl_value_t *dtv = jl_typeof(jl_get_kwsorter(primarydt->name));
        backref_list.items[pos] = dtv;
        return dtv;
    }
    size_t size = read_int32(s->s);
    uint8_t flags = read_uint8(s->s);
    uint8_t depth = read_int32(s->s);
    jl_datatype_t *dt = NULL;
    if (tag == 2)
        dt = jl_int32_type;
    else if (tag == 3)
        dt = jl_bool_type;
    else if (tag == 4)
        dt = jl_int64_type;
    else if (tag == 8)
        dt = jl_uint8_type;
    else if (tag == 0 || tag == 5 || tag == 10)
        dt = jl_new_uninitialized_datatype();
    else {
        assert(0 && "corrupt deserialization state");
        abort();
    }
    assert(s->tree_literal_values==NULL && s->mode != MODE_AST && "no new data-types expected during MODE_AST");
    assert(pos == backref_list.len - 1 && "nothing should have been deserialized since assigning pos");
    backref_list.items[pos] = dt;
    dt->size = size;
    dt->struct_decl = NULL;
    dt->instance = NULL;
    dt->ditype = NULL;
    dt->abstract = flags&1;
    dt->mutabl = (flags>>1)&1;
    int has_layout = (flags>>2)&1;
    int has_instance = (flags>>3)&1;
    dt->hasfreetypevars = (flags>>4)&1;
    dt->isleaftype = (flags>>5)&1;
    dt->depth = depth;
    dt->types = NULL;
    dt->parameters = NULL;
    dt->name = NULL;
    dt->super = NULL;
    dt->layout = NULL;
    if (!dt->abstract) {
        dt->ninitialized = read_uint16(s->s);
        dt->uid = (s->mode != MODE_MODULE) ? read_int32(s->s) : 0;
    }
    else {
        dt->ninitialized = 0;
        dt->uid = 0;
    }

    if (has_layout) {
        uint8_t layout = read_uint8(s->s);
        if (layout == 1) {
            dt->layout = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_array_type))->layout;
        }
        else if (layout == 2) {
            dt->layout = jl_void_type->layout;
        }
        else if (layout == 3) {
            dt->layout = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_pointer_type))->layout;
        }
        else {
            assert(layout == 0);
            uint16_t nf = read_uint16(s->s);
            uint8_t fielddesc_type = read_int8(s->s);
            size_t fielddesc_size = nf > 0 ? jl_fielddesc_size(fielddesc_type) : 0;
            jl_datatype_layout_t *layout = (jl_datatype_layout_t*)jl_gc_perm_alloc(
                    sizeof(jl_datatype_layout_t) + nf * fielddesc_size);
            layout->nfields = nf;
            layout->fielddesc_type = fielddesc_type;
            layout->alignment = read_int32(s->s);
            layout->haspadding = read_int8(s->s);
            layout->pointerfree = read_int8(s->s);
            ios_read(s->s, (char*)&layout[1], nf * fielddesc_size);
            dt->layout = layout;
        }
    }

    if (tag == 5) {
        dt->uid = jl_assign_type_uid();
    }
    else if (tag == 10) {
        assert(pos > 0);
        arraylist_push(&flagref_list, loc == HT_NOTFOUND ? NULL : loc);
        arraylist_push(&flagref_list, (void*)(uintptr_t)pos);
        dt->uid = -1; // mark that this type needs a new uid
    }

    if (has_instance) {
        assert(dt->uid != 0 && "there shouldn't be an instance on a type with uid = 0");
        dt->instance = jl_deserialize_value(s, &dt->instance);
        jl_gc_wb(dt, dt->instance);
    }
    dt->name = (jl_typename_t*)jl_deserialize_value(s, (jl_value_t**)&dt->name);
    jl_gc_wb(dt, dt->name);
    dt->parameters = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&dt->parameters);
    jl_gc_wb(dt, dt->parameters);
    dt->super = (jl_datatype_t*)jl_deserialize_value(s, (jl_value_t**)&dt->super);
    jl_gc_wb(dt, dt->super);
    dt->types = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&dt->types);
    jl_gc_wb(dt, dt->types);

    return (jl_value_t*)dt;
}

static jl_value_t *jl_deserialize_value_(jl_serializer_state *s, jl_value_t *vtag, jl_value_t **loc);
static jl_value_t *jl_deserialize_value(jl_serializer_state *s, jl_value_t **loc)
{
    assert(!ios_eof(s->s));
    uint8_t tag = read_uint8(s->s);
    if (tag == Null_tag)
        return NULL;
    if (tag == 0) {
        tag = read_uint8(s->s);
        jl_value_t *v = deser_tag[tag];
        assert(v != NULL);
        return v;
    }
    if (tag == BackRef_tag || tag == ShortBackRef_tag) {
        assert(s->tree_literal_values == NULL && s->mode != MODE_AST);
        uintptr_t offs = (tag == BackRef_tag) ? read_int32(s->s) : read_uint16(s->s);
        int isflagref = 0;
        if (s->mode == MODE_MODULE) {
            isflagref = !!(offs & 1);
            offs >>= 1;
        }
        // assert(offs >= 0); // offs is unsigned so this is always true
        assert(offs < backref_list.len);
        jl_value_t *bp = (jl_value_t*)backref_list.items[offs];
        assert(bp);
        if (isflagref && loc != HT_NOTFOUND) {
            assert(loc != NULL);
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
        return jl_array_ptr_ref(s->tree_literal_values, read_uint16(s->s));
    }
    jl_value_t *v = jl_deserialize_value_(s, vtag, loc);
    return v;
}

static jl_value_t *jl_deserialize_value_svec(jl_serializer_state *s, jl_value_t *vtag)
{
    int usetable = (s->mode != MODE_AST);
    size_t i, len;
    if (vtag == (jl_value_t*)jl_simplevector_type)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    jl_svec_t *sv = jl_alloc_svec_uninit(len);
    if (usetable)
        arraylist_push(&backref_list, (jl_value_t*)sv);
    jl_value_t **data = jl_svec_data(sv);
    for (i = 0; i < len; i++) {
        data[i] = jl_deserialize_value(s, &data[i]);
    }
    return (jl_value_t*)sv;
}

static jl_value_t *jl_deserialize_value_symbol(jl_serializer_state *s, jl_value_t *vtag)
{
    int usetable = (s->mode != MODE_AST);
    size_t len;
    if (vtag == (jl_value_t*)jl_symbol_type)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    char *name = (char*)(len >= 256 ? malloc(len + 1) : alloca(len + 1));
    ios_read(s->s, name, len);
    name[len] = '\0';
    jl_value_t *sym = (jl_value_t*)jl_symbol(name);
    if (len >= 256)
        free(name);
    if (usetable)
        arraylist_push(&backref_list, sym);
    return sym;
}

static jl_value_t *jl_deserialize_value_array(jl_serializer_state *s, jl_value_t *vtag)
{
    int usetable = (s->mode != MODE_AST);
    int16_t i, ndims;
    int isunboxed, elsize;
    if (vtag == (jl_value_t*)Array1d_tag) {
        ndims = 1;
        elsize = read_uint8(s->s);
        isunboxed = !(elsize >> 7);
        elsize = elsize & 0x7f;
    }
    else {
        ndims = read_uint16(s->s);
        elsize = read_uint16(s->s);
        isunboxed = !(elsize >> 15);
        elsize = elsize & 0x7fff;
    }
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, NULL);
    size_t *dims = (size_t*)alloca(ndims * sizeof(size_t));
    for (i = 0; i < ndims; i++) {
        dims[i] = jl_unbox_long(jl_deserialize_value(s, NULL));
    }
    jl_array_t *a = jl_new_array_for_deserialization((jl_value_t*)NULL, ndims, dims, isunboxed, elsize);
    if (usetable)
        backref_list.items[pos] = a;
    jl_value_t *aty = jl_deserialize_value(s, &jl_astaggedvalue(a)->type);
    jl_set_typeof(a, aty);
    if (!a->flags.ptrarray) {
        size_t tot = jl_array_len(a) * a->elsize;
        ios_read(s->s, (char*)jl_array_data(a), tot);
    }
    else {
        jl_value_t **data = (jl_value_t**)jl_array_data(a);
        size_t i, numel = jl_array_len(a);
        for (i = 0; i < numel; i++) {
            data[i] = jl_deserialize_value(s, &data[i]);
            if (data[i])
                jl_gc_wb(a, data[i]);
        }
    }
    return (jl_value_t*)a;
}

static jl_value_t *jl_deserialize_value_expr(jl_serializer_state *s, jl_value_t *vtag)
{
    int usetable = (s->mode != MODE_AST);
    size_t i, len;
    if (vtag == (jl_value_t*)jl_expr_type)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    int pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, NULL);
    jl_expr_t *e = jl_exprn((jl_sym_t*)jl_deserialize_value(s, NULL), len);
    if (usetable)
        backref_list.items[pos] = e;
    e->etype = jl_deserialize_value(s, &e->etype);
    jl_gc_wb(e, e->etype);
    jl_value_t **data = (jl_value_t**)(e->args->data);
    for (i = 0; i < len; i++) {
        data[i] = jl_deserialize_value(s, &data[i]);
    }
    return (jl_value_t*)e;
}

static jl_value_t *jl_deserialize_value_method(jl_serializer_state *s, jl_value_t **loc)
{
    int usetable = (s->mode != MODE_AST);
    jl_method_t *m =
        (jl_method_t*)jl_gc_alloc(s->ptls, sizeof(jl_method_t),
                                  jl_method_type);
    memset(m, 0, sizeof(jl_method_type));
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, m);
    m->sig = (jl_value_t*)jl_deserialize_value(s, (jl_value_t**)&m->sig);
    jl_gc_wb(m, m->sig);
    if (s->mode == MODE_MODULE) {
        int internal = read_uint8(s->s);
        if (!internal) {
            assert(loc != NULL && loc != HT_NOTFOUND);
            arraylist_push(&flagref_list, loc);
            arraylist_push(&flagref_list, (void*)pos);
            return (jl_value_t*)m;
        }
    }
    m->specializations.unknown = jl_deserialize_value(s, (jl_value_t**)&m->specializations);
    jl_gc_wb(m, m->specializations.unknown);
    m->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
    jl_gc_wb(m, m->name);
    m->isstaged = read_int8(s->s);
    m->file = (jl_sym_t*)jl_deserialize_value(s, NULL);
    m->line = read_int32(s->s);
    if (s->mode != MODE_MODULE) {
        m->min_world = read_int32(s->s);
        m->max_world = read_int32(s->s);
    }
    else {
        m->min_world = jl_world_counter;
        m->max_world = ~(size_t)0;
    }
    m->tvars = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&m->tvars);
    jl_gc_wb(m, m->tvars);
    m->ambig = jl_deserialize_value(s, (jl_value_t**)&m->ambig);
    jl_gc_wb(m, m->ambig);
    m->called = read_int8(s->s);
    m->nargs = read_int32(s->s);
    m->isva = read_int8(s->s);
    m->module = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&m->module);
    jl_gc_wb(m, m->module);
    m->sparam_syms = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&m->sparam_syms);
    jl_gc_wb(m, m->sparam_syms);
    m->roots = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&m->roots);
    if (m->roots)
        jl_gc_wb(m, m->roots);
    m->source = (jl_code_info_t*)jl_deserialize_value(s, (jl_value_t**)&m->source);
    if (m->source)
        jl_gc_wb(m, m->source);
    m->unspecialized = (jl_method_instance_t*)jl_deserialize_value(s, (jl_value_t**)&m->unspecialized);
    if (m->unspecialized)
        jl_gc_wb(m, m->unspecialized);
    m->generator = (jl_method_instance_t*)jl_deserialize_value(s, (jl_value_t**)&m->generator);
    if (m->generator)
        jl_gc_wb(m, m->generator);
    m->invokes.unknown = jl_deserialize_value(s, (jl_value_t**)&m->invokes);
    jl_gc_wb(m, m->invokes.unknown);
    m->traced = 0;
    JL_MUTEX_INIT(&m->writelock);
    return (jl_value_t*)m;
}

static jl_value_t *jl_deserialize_value_method_instance(jl_serializer_state *s, jl_value_t **loc)
{
    int usetable = (s->mode != MODE_AST);
    jl_method_instance_t *li =
        (jl_method_instance_t*)jl_gc_alloc(s->ptls, sizeof(jl_method_instance_t),
                                       jl_method_instance_type);
    memset(li, 0, sizeof(jl_method_instance_t));
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, li);

    li->specTypes = (jl_value_t*)jl_deserialize_value(s, (jl_value_t**)&li->specTypes);
    if (li->specTypes)
        jl_gc_wb(li, li->specTypes);
    li->def = (jl_method_t*)jl_deserialize_value(s, (jl_value_t**)&li->def);
    if (li->def)
        jl_gc_wb(li, li->def);

    int internal = 0;
    if (s->mode == MODE_MODULE) {
        internal = read_uint8(s->s);
        if (!internal) {
            assert(loc != NULL && loc != HT_NOTFOUND);
            arraylist_push(&flagref_list, loc);
            arraylist_push(&flagref_list, (void*)pos);
            return (jl_value_t*)li;
        }
    }

    li->inferred = jl_deserialize_value(s, &li->inferred);
    jl_gc_wb(li, li->inferred);
    li->inferred_const = jl_deserialize_value(s, &li->inferred_const);
    if (li->inferred_const)
        jl_gc_wb(li, li->inferred_const);
    li->rettype = jl_deserialize_value(s, &li->rettype);
    jl_gc_wb(li, li->rettype);
    li->sparam_vals = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&li->sparam_vals);
    jl_gc_wb(li, li->sparam_vals);
    li->backedges = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&li->backedges);
    if (li->backedges)
        jl_gc_wb(li, li->backedges);
    li->unspecialized_ducttape = NULL;
    if (s->mode != MODE_MODULE) {
        li->min_world = read_int32(s->s);
        li->max_world = read_int32(s->s);
    }
    else if (internal == 1) {
        li->min_world = 0;
        li->max_world = 0;
    }
    else if (internal == 2) {
        li->min_world = jl_world_counter;
        li->max_world = ~(size_t)0;
    }
    else if (internal == 3) {
        li->min_world = 1;
        li->max_world = 0;
    }
    else {
        assert(0 && "corrupt deserialization state");
        abort();
    }
    li->functionObjectsDecls.functionObject = NULL;
    li->functionObjectsDecls.specFunctionObject = NULL;
    li->inInference = 0;
    int8_t jlcall_api = read_int8(s->s);
    if (jlcall_api == 2 || jlcall_api == 0 || li->def == NULL) {
        li->fptr = NULL;
        li->jlcall_api = jlcall_api;
    }
    else if (jlcall_api < 0) {
        li->fptr = jl_deserialize_fptr(s);
        li->jlcall_api = -jlcall_api;
    }
    else {
        int32_t cfunc_llvm, func_llvm;
        func_llvm = read_int32(s->s);
        cfunc_llvm = read_int32(s->s);
        jl_delayed_fptrs(li, func_llvm, cfunc_llvm);
        li->fptr = NULL;
        li->jlcall_api = jlcall_api;
    }
    li->compile_traced = 0;
    return (jl_value_t*)li;
}

static jl_value_t *jl_deserialize_value_module(jl_serializer_state *s)
{
    int usetable = (s->mode != MODE_AST);
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, NULL);
    jl_sym_t *mname = (jl_sym_t*)jl_deserialize_value(s, NULL);
    if (s->mode == MODE_MODULE) {
        int ref_only = read_uint8(s->s);
        if (ref_only) {
            jl_value_t *m_ref = jl_get_global((jl_module_t*)jl_deserialize_value(s, NULL), mname);
            if (usetable)
                backref_list.items[pos] = m_ref;
            return m_ref;
        }
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
        jl_gc_wb_buf(m, b, sizeof(jl_binding_t));
        if (b->value != NULL) jl_gc_wb(m, b->value);
        b->globalref = jl_deserialize_value(s, &b->globalref);
        if (b->globalref != NULL) jl_gc_wb(m, b->globalref);
        b->owner = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&b->owner);
        if (b->owner != NULL) jl_gc_wb(m, b->owner);
        int8_t flags = read_int8(s->s);
        b->deprecated = (flags>>3) & 1;
        b->constp = (flags>>2) & 1;
        b->exportp = (flags>>1) & 1;
        b->imported = (flags) & 1;
        jl_deserialize_gv(s, (jl_value_t*)b);
    }
    size_t i = m->usings.len;
    size_t ni = read_int32(s->s);
    arraylist_grow(&m->usings, ni);
    ni += i;
    while (i < ni) {
        m->usings.items[i] = jl_deserialize_value(s, (jl_value_t**)&m->usings.items[i]);
        i++;
    }
    m->istopmod = read_uint8(s->s);
    m->uuid = read_uint64(s->s);
    m->counter = read_int32(s->s);
    return (jl_value_t*)m;
}

static jl_value_t *jl_deserialize_value_globalref(jl_serializer_state *s)
{
    int usetable = (s->mode != MODE_AST);
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

static jl_value_t *jl_deserialize_value_singleton(jl_serializer_state *s, jl_value_t **loc)
{
    int usetable = (s->mode != MODE_AST);
    jl_value_t *v = (jl_value_t*)jl_gc_alloc(s->ptls, 0, NULL);
    if (usetable) {
        uintptr_t pos = backref_list.len;
        arraylist_push(&backref_list, (void*)v);
        if (s->mode == MODE_MODULE) {
            // TODO: optimize the case where the value can easily be obtained
            // from an external module (tag == 6) as dt->instance
            assert(loc != NULL && loc != HT_NOTFOUND);
            arraylist_push(&flagref_list, loc);
            arraylist_push(&flagref_list, (void*)pos);
        }
    }
    jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, (jl_value_t**)HT_NOTFOUND); // no loc, since if dt is replaced, then dt->instance would be also
    jl_set_typeof(v, dt);
    return v;
}

static void jl_deserialize_struct(jl_serializer_state *s, jl_value_t *v, size_t startfield)
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(v);
    size_t i, nf = jl_datatype_nfields(dt);
    char *data = (char*)jl_data_ptr(v);
    for (i = startfield; i < nf; i++) {
        if (jl_field_size(dt, i) > 0) {
            if (jl_field_isptr(dt, i)) {
                jl_value_t **fld = (jl_value_t**)(data+jl_field_offset(dt, i));
                *fld = jl_deserialize_value(s, fld);
            }
            else {
                jl_set_nth_field(v, i, jl_deserialize_value(s, NULL));
            }
        }
    }
    if (s->mode == MODE_MODULE) {
        if (dt == jl_typename_type) {
            jl_typename_t *tn = (jl_typename_t*)v;
            tn->cache = jl_emptysvec; // the cache is refilled later (tag 5)
            tn->linearcache = jl_emptysvec; // the cache is refilled later (tag 5)
        }
        if (dt == jl_typemap_entry_type) {
            if (((jl_typemap_entry_t*)v)->max_world == ~(size_t)0) {
                // update world validity to reflect current state of the counter
                ((jl_typemap_entry_t*)v)->min_world = jl_world_counter;
            }
            else {
                // garbage entry - delete it :(
                ((jl_typemap_entry_t*)v)->min_world = ((jl_typemap_entry_t*)v)->max_world - 1;
            }
        }
    }
}

static jl_value_t *jl_deserialize_typemap_entry(jl_serializer_state *s)
{
    int N = read_int32(s->s); int n = N;
    jl_value_t *te = jl_nothing;
    jl_value_t **pn = &te;
    while (n > 0) {
        jl_value_t *v = jl_gc_alloc(s->ptls, jl_datatype_size(jl_typemap_entry_type), jl_typemap_entry_type);
        if (n == N && s->mode != MODE_AST)
            arraylist_push(&backref_list, v);
        jl_deserialize_struct(s, v, 1);
        ((jl_typemap_entry_t*)v)->next = (jl_typemap_entry_t*)jl_nothing;
        *pn = v;
        pn = (jl_value_t**)&((jl_typemap_entry_t*)v)->next;
        n--;
    }
    return te;
}

static jl_value_t *jl_deserialize_value_any(jl_serializer_state *s, jl_value_t *vtag, jl_value_t **loc)
{
    int usetable = (s->mode != MODE_AST);
    int32_t sz = (vtag == (jl_value_t*)SmallDataType_tag ? read_uint8(s->s) : read_int32(s->s));
    jl_value_t *v = jl_gc_alloc(s->ptls, sz, NULL);
    jl_set_typeof(v, (void*)(intptr_t)0x50);
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, v);
    jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, &jl_astaggedvalue(v)->type);
    if (dt == jl_datatype_type) {
        return jl_deserialize_datatype(s, pos, loc);
    }
    assert(s->mode == MODE_AST || sz != 0 || loc);
    if (s->mode == MODE_MODULE && dt == jl_typename_type) {
        int ref_only = read_uint8(s->s);
        if (ref_only) {
            jl_module_t *m = (jl_module_t*)jl_deserialize_value(s, NULL);
            jl_sym_t *sym = (jl_sym_t*)jl_deserialize_value(s, NULL);
            jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(jl_get_global(m, sym));
            assert(jl_is_datatype(dt));
            jl_value_t *v = (jl_value_t*)dt->name;
            if (usetable)
                backref_list.items[pos] = v;
            return v;
        }
    }
    if (s->mode == MODE_MODULE && dt == jl_unionall_type) {
        int ref_only = read_uint8(s->s);
        if (ref_only) {
            jl_module_t *m = (jl_module_t*)jl_deserialize_value(s, NULL);
            jl_sym_t *sym = (jl_sym_t*)jl_deserialize_value(s, NULL);
            jl_value_t *v = jl_get_global(m, sym);
            assert(jl_is_unionall(v));
            if (usetable)
                backref_list.items[pos] = v;
            return v;
        }
    }
    jl_set_typeof(v, dt);
    if (jl_datatype_nfields(dt) == 0 && jl_datatype_size(dt)>0) {
        int nby = jl_datatype_size(dt);
        ios_read(s->s, (char*)jl_data_ptr(v), nby);
    }
    else {
        jl_deserialize_struct(s, v, 0);
    }
    return v;
}

static jl_value_t *jl_deserialize_value_(jl_serializer_state *s, jl_value_t *vtag, jl_value_t **loc)
{
    int usetable = (s->mode != MODE_AST);
    if (vtag == (jl_value_t*)jl_simplevector_type ||
        vtag == (jl_value_t*)LongSvec_tag) {
        return jl_deserialize_value_svec(s, vtag);
    }
    else if (vtag == (jl_value_t*)CommonSym_tag) {
        return deser_symbols[read_uint8(s->s)];
    }
    else if (vtag == (jl_value_t*)jl_symbol_type ||
             vtag == (jl_value_t*)LongSymbol_tag) {
        return jl_deserialize_value_symbol(s, vtag);
    }
    else if (vtag == (jl_value_t*)jl_ssavalue_type) {
        jl_value_t *v = jl_box_ssavalue(read_uint16(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    }
    else if (vtag == (jl_value_t*)jl_slotnumber_type) {
        jl_value_t *v = jl_box_slotnumber(read_uint16(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    }
    else if (vtag == (jl_value_t*)jl_array_type ||
             vtag == (jl_value_t*)Array1d_tag) {
        return jl_deserialize_value_array(s, vtag);
    }
    else if (vtag == (jl_value_t*)jl_expr_type ||
             vtag == (jl_value_t*)LongExpr_tag) {
        return jl_deserialize_value_expr(s, vtag);
    }
    else if (vtag == (jl_value_t*)jl_tvar_type) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_gc_alloc(s->ptls, sizeof(jl_tvar_t), jl_tvar_type);
        if (usetable)
            arraylist_push(&backref_list, tv);
        tv->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        jl_gc_wb(tv, tv->name);
        tv->lb = jl_deserialize_value(s, &tv->lb);
        jl_gc_wb(tv, tv->lb);
        tv->ub = jl_deserialize_value(s, &tv->ub);
        jl_gc_wb(tv, tv->ub);
        return (jl_value_t*)tv;
    }
    else if (vtag == (jl_value_t*)jl_method_type) {
        return jl_deserialize_value_method(s, loc);
    }
    else if (vtag == (jl_value_t*)jl_method_instance_type) {
        return jl_deserialize_value_method_instance(s, loc);
    }
    else if (vtag == (jl_value_t*)jl_module_type) {
        return jl_deserialize_value_module(s);
    }
    else if (vtag == (jl_value_t*)SmallInt64_tag) {
        jl_value_t *v = jl_box_int64(read_int32(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    }
    else if (vtag == (jl_value_t*)Int32_tag) {
        jl_value_t *v = jl_box_int32(read_int32(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    }
    else if (vtag == (jl_value_t*)NearbyGlobal_tag) {
        assert(s->tree_enclosing_module != NULL);
        jl_value_t *sym = jl_deserialize_value(s, NULL);
        return jl_module_globalref(s->tree_enclosing_module, (jl_sym_t*)sym);
    }
    else if (vtag == (jl_value_t*)jl_globalref_type) {
        return jl_deserialize_value_globalref(s);
    }
    else if (vtag == (jl_value_t*)Singleton_tag) {
        return jl_deserialize_value_singleton(s, loc);
    }
    else if (vtag == (jl_value_t*)jl_string_type) {
        size_t n = read_int32(s->s);
        jl_value_t *str = jl_alloc_string(n);
        if (usetable)
            arraylist_push(&backref_list, str);
        ios_read(s->s, jl_string_data(str), n);
        return str;
    }
    else if (vtag == (jl_value_t*)jl_typemap_entry_type) {
        return jl_deserialize_typemap_entry(s);
    }
    else {
        assert(vtag == (jl_value_t*)jl_datatype_type || vtag == (jl_value_t*)SmallDataType_tag);
        return jl_deserialize_value_any(s, vtag, loc);
    }
}

typedef struct _linkedlist_t {
    struct _linkedlist_t *next;
    union {
        struct {
            jl_method_t *meth;
            jl_tupletype_t *simpletype;
        };
        struct {
            jl_method_instance_t *caller;
            jl_value_t *callee;
        };
    } def[100];
    size_t count;
} linkedlist_t;

static void jl_deserialize_methods_from_mod(jl_serializer_state *s, linkedlist_t *list)
{
    list->count = 0;
    list->next = NULL;
    while (1) {
        if (list->count == sizeof(list->def) / sizeof(list->def[0])) {
            list->next = (linkedlist_t*)malloc(sizeof(linkedlist_t));
            list = list->next;
            list->count = 0;
            list->next = NULL;
        }
        // using a linked list so that we can take these addresses
        // and have them remain constant (arraylist reallocates)
        jl_value_t **loc_meth = (jl_value_t**)&list->def[list->count].meth;
        jl_value_t **loc_styp = (jl_value_t**)&list->def[list->count].simpletype;
        *loc_meth = jl_deserialize_value(s, loc_meth);
        if (*loc_meth == NULL)
            return;
        *loc_styp = jl_deserialize_value(s, loc_styp);
        list->count++;
    }
}

static void jl_insert_methods(linkedlist_t *list)
{
    while (list) {
        size_t i;
        for (i = 0; i < list->count; i++) {
            if (jl_is_method(list->def[i].meth)) {
                jl_method_t *meth = list->def[i].meth;
                jl_datatype_t *gf = jl_first_argument_datatype((jl_value_t*)meth->sig);
                assert(jl_is_datatype(gf) && gf->name->mt);
                jl_method_table_insert(gf->name->mt, meth, list->def[i].simpletype);
            }
        }
        list = list->next;
    }
}

static void jl_insert_backedges(linkedlist_t *list)
{
    while (list) {
        size_t i;
        for (i = 0; i < list->count; i++) {
            if (!jl_is_method(list->def[i].meth)) {
                jl_method_instance_t *caller = list->def[i].caller;
                assert(jl_is_method_instance(caller));
                jl_value_t *callee = list->def[i].callee;
                if (jl_is_method_instance(callee)) {
                    jl_method_instance_add_backedge((jl_method_instance_t*)callee, caller);
                }
                else {
                    jl_datatype_t *gf = jl_first_argument_datatype(callee);
                    assert(jl_is_datatype(gf) && gf->name->mt);
                    jl_method_table_add_backedge(gf->name->mt, callee, (jl_value_t*)caller);
                }
            }
        }
        list = list->next;
    }
}


static void free_linkedlist(linkedlist_t *list)
{
    while (list) {
        linkedlist_t *prev = list;
        list = list->next;
        free(prev);
    }
}

static jl_value_t *read_verify_mod_list(ios_t *s)
{
    if (!jl_main_module->uuid) {
        return jl_get_exceptionf(jl_errorexception_type,
                "Main module uuid state is invalid for module deserialization.");
    }
    while (1) {
        size_t len = read_int32(s);
        if (len == 0)
            return NULL;
        char *name = (char*)alloca(len+1);
        ios_read(s, name, len);
        name[len] = '\0';
        uint64_t uuid = read_uint64(s);
        jl_sym_t *sym = jl_symbol(name);
        jl_module_t *m = NULL;
        if (jl_binding_resolved_p(jl_main_module, sym))
            m = (jl_module_t*)jl_get_global(jl_main_module, sym);
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
            return jl_get_exceptionf(jl_errorexception_type,
                    "Requiring \"%s\" did not define a corresponding module.", name);
        }
        if (!jl_is_module(m)) {
            ios_close(s);
            return jl_get_exceptionf(jl_errorexception_type,
                "Invalid module path (%s does not name a module).", name);
        }
        if (m->uuid != uuid) {
            return jl_get_exceptionf(jl_errorexception_type,
                "Module %s uuid did not match cache file.", name);
        }
    }
}

static int readstr_verify(ios_t *s, const char *str)
{
    size_t i, len = strlen(str);
    for (i = 0; i < len; ++i)
        if ((char)read_uint8(s) != str[i])
            return 0;
    return 1;
}

JL_DLLEXPORT int jl_read_verify_header(ios_t *s)
{
    uint16_t bom;
    return (readstr_verify(s, JI_MAGIC) &&
            read_uint16(s) == JI_FORMAT_VERSION &&
            ios_read(s, (char *) &bom, 2) == 2 && bom == BOM &&
            read_uint8(s) == sizeof(void*) &&
            readstr_verify(s, JL_BUILD_UNAME) && !read_uint8(s) &&
            readstr_verify(s, JL_BUILD_ARCH) && !read_uint8(s) &&
            readstr_verify(s, JULIA_VERSION_STRING) && !read_uint8(s) &&
            readstr_verify(s, jl_git_branch()) && !read_uint8(s) &&
            readstr_verify(s, jl_git_commit()) && !read_uint8(s));
}

static void jl_finalize_serializer(jl_serializer_state *s)
{
    size_t i, l;
    // save module initialization order
    if (jl_module_init_order != NULL) {
        l = jl_array_len(jl_module_init_order);
        for (i = 0; i < l; i++) {
            // verify that all these modules were saved
            assert(ptrhash_get(&backref_table, jl_array_ptr_ref(jl_module_init_order, i)) != HT_NOTFOUND);
        }
    }
    jl_serialize_value(s, jl_module_init_order);

    // record list of reinitialization functions
    l = reinit_list.len;
    for (i = 0; i < l; i += 2) {
        write_int32(s->s, (int)((uintptr_t) reinit_list.items[i]));
        write_int32(s->s, (int)((uintptr_t) reinit_list.items[i+1]));
    }
    write_int32(s->s, -1);
}

void jl_typemap_rehash(union jl_typemap_t ml, int8_t offs);
static void jl_reinit_item(jl_value_t *v, int how, arraylist_t *tracee_list)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_TRY {
        switch (how) {
            case 1: { // rehash ObjectIdDict
                jl_array_t **a = (jl_array_t**)v;
                // Assume *a don't need a write barrier
                *a = jl_idtable_rehash(*a, jl_array_len(*a));
                jl_gc_wb(v, *a);
                break;
            }
            case 2: { // reinsert module v into parent (const)
                jl_module_t *mod = (jl_module_t*)v;
                jl_binding_t *b = jl_get_binding_wr(mod->parent, mod->name);
                jl_declare_constant(b); // this can throw
                if (b->value != NULL) {
                    if (!jl_is_module(b->value)) {
                        jl_errorf("Invalid redefinition of constant %s.",
                                  jl_symbol_name(mod->name)); // this also throws
                    }
                    if (jl_generating_output() && jl_options.incremental) {
                        jl_errorf("Cannot replace module %s during incremental precompile.", jl_symbol_name(mod->name));
                    }
                    jl_printf(JL_STDERR, "WARNING: replacing module %s.\n",
                              jl_symbol_name(mod->name));
                }
                b->value = v;
                jl_gc_wb_binding(b, v);
                break;
            }
            case 3: { // rehash MethodTable
                jl_methtable_t *mt = (jl_methtable_t*)v;
                jl_typemap_rehash(mt->defs, 0);
                jl_typemap_rehash(mt->cache, (mt == jl_type_typename->mt) ? 0 : 1);
                if (tracee_list)
                    arraylist_push(tracee_list, mt);
                break;
            }
            case 4: { // rehash specializations tfunc
                jl_method_t *m = (jl_method_t*)v;
                jl_typemap_rehash(m->specializations, 0);
                break;
            }
            default:
                assert(0 && "corrupt deserialization state");
                abort();
        }
    }
    JL_CATCH {
        jl_printf(JL_STDERR, "WARNING: error while reinitializing value ");
        jl_static_show(JL_STDERR, v);
        jl_printf(JL_STDERR, ":\n");
        jl_static_show(JL_STDERR, ptls->exception_in_transit);
        jl_printf(JL_STDERR, "\n");
    }
}

static jl_array_t *jl_finalize_deserializer(jl_serializer_state *s, arraylist_t *tracee_list)
{
    jl_array_t *init_order = (jl_array_t*)jl_deserialize_value(s, NULL);

    // run reinitialization functions
    int pos = read_int32(s->s);
    while (pos != -1) {
        jl_reinit_item((jl_value_t*)backref_list.items[pos], read_int32(s->s), tracee_list);
        pos = read_int32(s->s);
    }
    return init_order;
}

static void jl_init_restored_modules(jl_array_t *init_order)
{
    if (!init_order)
        return;
    int i, l = jl_array_len(init_order);
    for (i = 0; i < l; i++) {
        jl_value_t *mod = jl_array_ptr_ref(init_order, i);
        if (!jl_generating_output() || jl_options.incremental) {
            jl_module_run_initializer((jl_module_t*)mod);
        }
        else {
            if (jl_module_init_order == NULL)
                jl_module_init_order = jl_alloc_vec_any(0);
            jl_array_ptr_1d_push(jl_module_init_order, mod);
        }
    }
}


// --- entry points ---

// remove cached types not referenced in the stream
static void jl_prune_type_cache(jl_svec_t *cache)
{
    size_t l = jl_svec_len(cache), ins = 0, i;
    for(i=0; i < l; i++) {
        jl_value_t *ti = jl_svecref(cache, i);
        if (ti == NULL) break;
        if (ptrhash_get(&backref_table, ti) != HT_NOTFOUND || jl_get_llvm_gv(ti) != 0)
            jl_svecset(cache, ins++, ti);
    }
    if (i > ins)
        memset(&jl_svec_data(cache)[ins], 0, (i-ins)*sizeof(jl_value_t*));
}

static void jl_save_system_image_to_stream(ios_t *f)
{
    jl_gc_collect(1); // full
    jl_gc_collect(0); // incremental (sweep finalizers)
    JL_TIMING(SYSIMG_DUMP);
    int en = jl_gc_enable(0);
    htable_reset(&backref_table, 250000);
    arraylist_new(&reinit_list, 0);
    backref_table_numel = 0;
    jl_serializer_state s = {
        f, MODE_SYSTEM_IMAGE,
        NULL, NULL,
        jl_get_ptls_states()
    };

    // orphan old Base module if present
    jl_base_module = (jl_module_t*)jl_get_global(jl_main_module, jl_symbol("Base"));

    // empty!(Core.ARGS)
    if (jl_core_module != NULL) {
        jl_array_t *args = (jl_array_t*)jl_get_global(jl_core_module, jl_symbol("ARGS"));
        if (args != NULL) {
            jl_array_del_end(args, jl_array_len(args));
        }
    }

    jl_idtable_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("ObjectIdDict")) : NULL;

    jl_serialize_value(&s, jl_main_module);
    jl_serialize_value(&s, jl_top_module);
    jl_serialize_value(&s, jl_typeinf_func);
    write_uint64(f, jl_typeinf_world);

    // deserialize method tables of builtin types
    jl_serialize_value(&s, jl_type_typename->mt);
    jl_serialize_value(&s, jl_intrinsic_type->name->mt);
    jl_serialize_value(&s, jl_sym_type->name->mt);
    jl_serialize_value(&s, jl_array_typename->mt);
    jl_serialize_value(&s, jl_module_type->name->mt);

    jl_prune_type_cache(jl_tuple_typename->cache);
    jl_prune_type_cache(jl_tuple_typename->linearcache);
    jl_prune_type_cache(jl_type_typename->cache);

    intptr_t i;
    for (i = 0; i < builtin_typenames.len; i++) {
        jl_serialize_value(&s, ((jl_typename_t*)builtin_typenames.items[i])->cache);
        jl_serialize_value(&s, ((jl_typename_t*)builtin_typenames.items[i])->linearcache);
    }

    // ensure everything in deser_tag is reassociated with its GlobalValue
    for (i = 2; i < 255; i++) {
        jl_serialize_gv(&s, deser_tag[i]);
    }

    jl_serialize_globalvals(&s);
    jl_serialize_gv_others(&s); // serialize things that might not have visible gc roots roots with GlobalValue references

    write_int32(f, jl_get_t_uid_ctr());
    write_int32(f, jl_get_gs_ctr());
    write_int32(f, jl_world_counter);
    jl_finalize_serializer(&s); // done with f and s

    htable_reset(&backref_table, 0);
    arraylist_free(&reinit_list);

    jl_gc_enable(en);
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
    JL_TIMING(SYSIMG_LOAD);
    jl_ptls_t ptls = jl_get_ptls_states();
    int en = jl_gc_enable(0);
    arraylist_new(&backref_list, 250000);
    jl_serializer_state s = {
        f, MODE_SYSTEM_IMAGE,
        NULL, NULL,
        jl_get_ptls_states()
    };

    jl_main_module = (jl_module_t*)jl_deserialize_value(&s, NULL);
    jl_top_module = (jl_module_t*)jl_deserialize_value(&s, NULL);
    jl_internal_main_module = jl_main_module;
    jl_typeinf_func = (jl_function_t*)jl_deserialize_value(&s, NULL);
    jl_typeinf_world = read_uint64(f);

    jl_type_type_mt = (jl_methtable_t*)jl_deserialize_value(&s, NULL);
    jl_type_typename->mt = jl_type_type_mt;
    jl_unionall_type->name->mt = jl_type_type_mt;
    jl_uniontype_type->name->mt = jl_type_type_mt;
    jl_datatype_type->name->mt = jl_type_type_mt;
    jl_intrinsic_type->name->mt = (jl_methtable_t*)jl_deserialize_value(&s, NULL);
    jl_sym_type->name->mt = (jl_methtable_t*)jl_deserialize_value(&s, NULL);
    jl_array_typename->mt = (jl_methtable_t*)jl_deserialize_value(&s, NULL);
    jl_module_type->name->mt = (jl_methtable_t*)jl_deserialize_value(&s, NULL);

    intptr_t i;
    for(i=0; i < builtin_typenames.len; i++) {
        jl_typename_t *tn = (jl_typename_t*)builtin_typenames.items[i];
        tn->cache = (jl_svec_t*)jl_deserialize_value(&s, NULL); jl_gc_wb(tn, tn->cache);
        tn->linearcache = (jl_svec_t*)jl_deserialize_value(&s, NULL); jl_gc_wb(tn, tn->linearcache);
        jl_resort_type_cache(tn->cache);
    }

    jl_core_module = (jl_module_t*)jl_get_global(jl_main_module,
                                                 jl_symbol("Core"));
    jl_base_module = (jl_module_t*)jl_get_global(jl_main_module,
                                                 jl_symbol("Base"));
    ptls->current_module = jl_base_module; // run start_image in Base

    // ensure everything in deser_tag is reassociated with its GlobalValue
    for (i = 2; i < 255; i++) {
        jl_deserialize_gv(&s, deser_tag[i]);
    }
    jl_deserialize_globalvals(&s);
    jl_deserialize_gv_others(&s);

    int uid_ctr = read_int32(f);
    int gs_ctr = read_int32(f);
    jl_world_counter = read_int32(f);
    jl_module_init_order = jl_finalize_deserializer(&s, NULL); // done with s and f

    jl_set_t_uid_ctr(uid_ctr);
    jl_set_gs_ctr(gs_ctr);

    jl_get_builtins();
    jl_get_builtin_hooks();
    if (jl_base_module) {
        jl_get_system_hooks();
    }
    jl_boot_file_loaded = 1;
    jl_init_box_caches();

    //jl_printf(JL_STDERR, "backref_list.len = %d\n", backref_list.len);
    arraylist_free(&backref_list);

    jl_gc_reset_alloc_count();
    jl_gc_enable(en);
    jl_update_all_fptrs();
}

JL_DLLEXPORT void jl_restore_system_image(const char *fname)
{
    char *dot = (char*) strrchr(fname, '.');
    int is_ji = (dot && !strcmp(dot, ".ji"));

    if (!is_ji) {
        int err = jl_load_sysimg_so();
        if (err != 0) {
            if (jl_sysimg_handle == 0)
                jl_errorf("System image file \"%s\" not found.", fname);
            jl_errorf("Library \"%s\" does not contain a valid system image.", fname);
        }
    }
    else {
        ios_t f;
        if (ios_file(&f, fname, 1, 0, 0, 0) == NULL)
            jl_errorf("System image file \"%s\" not found.", fname);
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

JL_DLLEXPORT jl_array_t *jl_compress_ast(jl_method_t *m, jl_array_t *ast)
{
    JL_TIMING(AST_COMPRESS);
    JL_LOCK(&m->writelock); // protect the roots array (Might GC)
    assert(jl_is_method(m));
    assert(jl_is_array(ast));
    ios_t dest;
    ios_mem(&dest, 0);
    int en = jl_gc_enable(0); // Might GC

    if (m->roots == NULL) {
        m->roots = jl_alloc_vec_any(0);
        jl_gc_wb(m, m->roots);
    }
    jl_serializer_state s = {
        &dest, MODE_AST,
        m->roots, m->module,
        jl_get_ptls_states()
    };
    size_t i, nstmts = jl_array_len(ast);
    assert(nstmts < INT32_MAX);
    write_int32(&dest, nstmts);
    for (i = 0; i < nstmts; i++) {
        jl_serialize_value(&s, jl_array_ptr_ref(ast, i));
    }

    //jl_printf(JL_STDERR, "%d bytes, %d values\n", dest.size, vals->length);

    jl_array_t *v = jl_take_buffer(&dest);
    if (jl_array_len(m->roots) == 0) {
        m->roots = NULL;
    }
    JL_GC_PUSH1(&v);
    jl_gc_enable(en);
    JL_UNLOCK(&m->writelock); // Might GC
    JL_GC_POP();
    return v;
}

JL_DLLEXPORT jl_array_t *jl_uncompress_ast(jl_method_t *m, jl_array_t *data)
{
    JL_TIMING(AST_UNCOMPRESS);
    JL_LOCK(&m->writelock); // protect the roots array (Might GC)
    assert(jl_is_method(m));
    assert(jl_is_array(data));
    jl_array_t *bytes = (jl_array_t*)data;
    ios_t src;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)bytes->data, jl_array_len(bytes), 0);
    src.size = jl_array_len(bytes);
    int en = jl_gc_enable(0); // Might GC
    jl_serializer_state s = {
        &src, MODE_AST,
        m->roots, m->module,
        jl_get_ptls_states()
    };

    size_t i, nstmts = read_int32(&src);
    jl_array_t *ast = jl_alloc_vec_any(nstmts);
    JL_GC_PUSH1(&ast);
    for (i = 0; i < nstmts; i++) {
        jl_array_ptr_set(ast, i, jl_deserialize_value(&s, NULL));
    }
    jl_gc_enable(en);
    JL_UNLOCK(&m->writelock); // Might GC
    JL_GC_POP();
    return ast;
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
    write_header(&f);
    write_work_list(&f);
    write_dependency_list(&f);
    write_mod_list(&f); // this can return errors during deserialize,
                        // best to keep it early (before any actual initialization)

    arraylist_new(&reinit_list, 0);
    htable_new(&backref_table, 5000);
    ptrhash_put(&backref_table, jl_main_module, (char*)HT_NOTFOUND + 1);
    backref_table_numel = 1;
    jl_idtable_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("ObjectIdDict")) : NULL;

    int en = jl_gc_enable(0);
    jl_serializer_state s = {
        &f, MODE_MODULE,
        NULL, NULL,
        jl_get_ptls_states()
    };
    jl_serialize_value(&s, worklist);
    jl_serialize_lambdas_from_mod(&s, jl_main_module);
    jl_serialize_value(&s, NULL); // signal end of lambdas
    jl_finalize_serializer(&s); // done with f
    serializer_worklist = NULL;

    jl_gc_enable(en);
    htable_reset(&backref_table, 0);
    arraylist_free(&reinit_list);
    ios_close(&f);

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
    jl_svec_t *tt = dt->parameters;
    if (dt->uid == 0 || dt->uid == -1) {
        // recache all type parameters
        size_t i, l = jl_svec_len(tt);
        for (i = 0; i < l; i++) {
            jl_datatype_t *p = (jl_datatype_t*)jl_svecref(tt, i);
            if (jl_is_datatype(p)) {
                if (p->uid == -1 || p->uid == 0) {
                    jl_datatype_t *cachep = jl_recache_type(p, start, NULL);
                    if (p != cachep) {
                        assert(jl_types_equal((jl_value_t*)p, (jl_value_t*)cachep));
                        jl_svecset(tt, i, cachep);
                    }
                }
            }
            else {
                jl_datatype_t *tp = (jl_datatype_t*)jl_typeof(p);
                assert(tp->uid != 0);
                if (tp->uid == -1) {
                    tp = jl_recache_type(tp, start, NULL);
                }
                if (tp->instance && (jl_value_t*)p != tp->instance)
                    jl_svecset(tt, i, tp->instance);
            }
        }
    }

    jl_datatype_t *t; // the type after unique'ing
    if (dt->uid == 0) {
        return dt;
    }
    else if (dt->uid == -1) {
        if (jl_svec_len(tt) == 0) { // jl_cache_type doesn't work if length(parameters) == 0
            dt->uid = jl_assign_type_uid();
            t = dt;
        }
        else {
            dt->uid = 0;
            t = (jl_datatype_t*)jl_cache_type_(dt);
            assert(jl_types_equal((jl_value_t*)t, (jl_value_t*)dt));
        }
    }
    else {
        t = dt;
    }
    assert(t->uid != 0);
    // delete / replace any other usages of this type in the backref list
    // with the newly constructed object
    size_t i = start;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        if ((jl_value_t*)dt == o) {
            if (t != dt) {
                if (loc)
                    *loc = (jl_value_t*)t;
                if (offs > 0)
                    backref_list.items[offs] = t;
            }
        }
        else if (v == o) {
            if (t->instance != v) {
                *loc = t->instance;
                if (offs > 0)
                    backref_list.items[offs] = t->instance;
            }
        }
        else {
            i += 2;
            continue;
        }
        // delete this item from the flagref list, so it won't be re-encountered later
        flagref_list.len -= 2;
        if (i >= flagref_list.len)
            break;
        flagref_list.items[i + 0] = flagref_list.items[flagref_list.len + 0];
        flagref_list.items[i + 1] = flagref_list.items[flagref_list.len + 1];
    }
    return t;
}

static void jl_recache_types(void)
{
    size_t i = 0;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        if (jl_is_method(o) || jl_is_method_instance(o)) {
            i += 2;
        }
        else {
            jl_value_t *v;
            jl_datatype_t *dt, *t;
            if (jl_is_datatype(o)) {
                dt = (jl_datatype_t*)o;
                v = dt->instance;
                assert(dt->uid == -1);
                t = jl_recache_type(dt, i + 2, NULL);
            }
            else {
                dt = (jl_datatype_t*)jl_typeof(o);
                v = o;
                assert(dt->instance);
                t = jl_recache_type(dt, i + 2, v);
            }
            assert(dt);
            if (t != dt) {
                assert(!type_in_worklist(t));
                jl_set_typeof(dt, (void*)(intptr_t)0x10); // invalidate the old datatype to help catch errors
                if ((jl_value_t*)dt == o) {
                    if (loc)
                        *loc = (jl_value_t*)t;
                    if (offs > 0)
                        backref_list.items[offs] = t;
                }
            }
            if (t->instance != v) {
                jl_set_typeof(v, (void*)(intptr_t)0x20); // invalidate the old value to help catch errors
                if (v == o) {
                    *loc = t->instance;
                    if (offs > 0)
                        backref_list.items[offs] = t->instance;
                }
            }
            // delete this item from the flagref list, so it won't be re-encountered later
            flagref_list.len -= 2;
            if (i >= flagref_list.len)
                break;
            flagref_list.items[i + 0] = flagref_list.items[flagref_list.len + 0];
            flagref_list.items[i + 1] = flagref_list.items[flagref_list.len + 1];
        }
    }
}

static void jl_update_backref_list(jl_value_t *old, jl_value_t *_new, size_t start)
{
    // update the backref list
    size_t i = start;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *v = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        if ((jl_value_t*)v == old) { // same item, update this entry
            if (loc)
                *loc = (jl_value_t*)_new;
            if (offs > 0)
                backref_list.items[offs] = _new;
            // delete this item from the flagref list, so it won't be re-encountered later
            flagref_list.len -= 2;
            if (i >= flagref_list.len)
                break;
            flagref_list.items[i + 0] = flagref_list.items[flagref_list.len + 0];
            flagref_list.items[i + 1] = flagref_list.items[flagref_list.len + 1];
        }
        else {
            i += 2;
        }
    }
}

jl_method_t *jl_recache_method(jl_method_t *m, size_t start)
{
    jl_datatype_t *sig = (jl_datatype_t*)m->sig;
    jl_datatype_t *ftype = jl_first_argument_datatype((jl_value_t*)sig);
    jl_methtable_t *mt = ftype->name->mt;
    jl_set_typeof(m, (void*)(intptr_t)0x30); // invalidate the old value to help catch errors
    jl_method_t *_new = (jl_method_t*)jl_methtable_lookup(mt, sig, /*TODO*/jl_world_counter);
    assert(_new && jl_is_method(_new));
    jl_update_backref_list((jl_value_t*)m, (jl_value_t*)_new, start);
    return _new;
}

jl_method_instance_t *jl_recache_method_instance(jl_method_instance_t *li, size_t start)
{
    jl_datatype_t *sig = (jl_datatype_t*)li->def;
    assert(jl_is_datatype(sig) || jl_is_unionall(sig));
    jl_datatype_t *ftype = jl_first_argument_datatype((jl_value_t*)sig);
    jl_methtable_t *mt = ftype->name->mt;
    jl_method_t *m = (jl_method_t*)jl_methtable_lookup(mt, sig, /*TODO*/jl_world_counter);
    assert(m && jl_is_method(m));

    jl_datatype_t *argtypes = (jl_datatype_t*)li->specTypes;
    jl_set_typeof(li, (void*)(intptr_t)0x40); // invalidate the old value to help catch errors
    jl_svec_t *env = jl_emptysvec;
    jl_value_t *ti = jl_type_intersection_matching((jl_value_t*)argtypes, (jl_value_t*)m->sig, &env);
    //assert(ti != jl_bottom_type); (void)ti;
    if (ti == jl_bottom_type)
        env = jl_emptysvec; // the intersection may fail now if the type system had made an incorrect subtype env in the past
    jl_method_instance_t *_new = jl_specializations_get_linfo(m, (jl_value_t*)argtypes, env, /*TODO*/jl_world_counter);
    jl_update_backref_list((jl_value_t*)li, (jl_value_t*)_new, start);
    return _new;
}

static void jl_recache_other(void)
{
    size_t i = 0;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *_new, *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        i += 2;
        if (jl_is_method(o)) {
            // lookup the real Method based on the placeholder sig
            _new = (jl_value_t*)jl_recache_method((jl_method_t*)o, i);
        }
        else if (jl_is_method_instance(o)) {
            // lookup the real MethodInstance based on the placeholder specTypes
            _new = (jl_value_t*)jl_recache_method_instance((jl_method_instance_t*)o, i);
        }
        else {
            abort();
        }
        if (loc)
            *loc = _new;
        if (offs > 0)
            backref_list.items[offs] = _new;
    }
}

extern tracer_cb jl_newmeth_tracer;
static int trace_method(jl_typemap_entry_t *entry, void *closure)
{
    jl_call_tracer(jl_newmeth_tracer, (jl_value_t*)entry->func.method);
    return 1;
}

static jl_value_t *_jl_restore_incremental(ios_t *f)
{
    if (ios_eof(f) || !jl_read_verify_header(f)) {
        ios_close(f);
        return jl_get_exceptionf(jl_errorexception_type,
                "Precompile file header verification checks failed.");
    }
    { // skip past the mod list
        size_t len;
        while ((len = read_int32(f)))
            ios_skip(f, len + sizeof(uint64_t));
    }
    { // skip past the dependency list
        size_t deplen = read_uint64(f);
        ios_skip(f, deplen);
    }

    // verify that the system state is valid
    jl_value_t *verify_error = read_verify_mod_list(f);
    if (verify_error) {
        ios_close(f);
        return verify_error;
    }

    // prepare to deserialize
    arraylist_new(&backref_list, 4000);
    arraylist_push(&backref_list, jl_main_module);
    arraylist_new(&flagref_list, 0);

    int en = jl_gc_enable(0);
    ++jl_world_counter; // reserve a world age for the deserialization
    jl_serializer_state s = {
        f, MODE_MODULE,
        NULL, NULL,
        jl_get_ptls_states()
    };
    jl_array_t *restored = NULL;
    jl_array_t *init_order = NULL;
    restored = (jl_array_t*)jl_deserialize_value(&s, (jl_value_t**)&restored);
    serializer_worklist = restored;

    // get list of external generic functions
    linkedlist_t external_methods;
    jl_deserialize_methods_from_mod(&s, &external_methods);

    arraylist_t *tracee_list = NULL;
    if (jl_newmeth_tracer)
        tracee_list = arraylist_new((arraylist_t*)malloc(sizeof(arraylist_t)), 0);

    // at this point, the AST is fully reconstructed, but still completely disconnected
    // now all of the interconnects will be created
    jl_recache_types(); // make all of the types identities correct
    init_order = jl_finalize_deserializer(&s, tracee_list); // done with f and s (needs to be after recache types)
    jl_insert_methods(&external_methods); // hook up methods of external generic functions (needs to be after recache types)
    jl_recache_other(); // make all of the other objects identities correct (needs to be after insert methods)
    jl_insert_backedges(&external_methods); // restore external backedges (needs to be after recache other)
    free_linkedlist(external_methods.next);
    serializer_worklist = NULL;

    JL_GC_PUSH2(&init_order, &restored);
    jl_gc_enable(en);
    arraylist_free(&flagref_list);
    arraylist_free(&backref_list);
    ios_close(f);

    if (tracee_list) {
        jl_methtable_t *mt;
        while ((mt = (jl_methtable_t*)arraylist_pop(tracee_list)) != NULL)
            jl_typemap_visitor(mt->defs, trace_method, NULL);
        arraylist_free(tracee_list);
        free(tracee_list);
    }
    jl_init_restored_modules(init_order);
    JL_GC_POP();

    return (jl_value_t*)restored;
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental_from_buf(const char *buf, size_t sz)
{
    ios_t f;
    ios_static_buffer(&f, (char*)buf, sz);
    return _jl_restore_incremental(&f);
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental(const char *fname)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL) {
        return jl_get_exceptionf(jl_errorexception_type,
            "Cache file \"%s\" not found.\n", fname);
    }
    return _jl_restore_incremental(&f);
}

// --- init ---

void jl_init_serializer(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    htable_new(&ser_tag, 0);
    htable_new(&common_symbol_tag, 0);
    htable_new(&fptr_to_id, sizeof(id_to_fptrs)/sizeof(*id_to_fptrs));
    htable_new(&backref_table, 0);

    void *tags[] = { jl_symbol_type, jl_ssavalue_type, jl_datatype_type, jl_slotnumber_type,
                     jl_simplevector_type, jl_array_type, jl_typedslot_type,
                     jl_expr_type, (void*)LongSymbol_tag, (void*)LongSvec_tag,
                     (void*)LongExpr_tag, (void*)LiteralVal_tag, jl_string_type,
                     (void*)SmallInt64_tag, (void*)SmallDataType_tag, jl_typemap_entry_type,
                     (void*)Int32_tag, (void*)Array1d_tag, (void*)Singleton_tag,
                     jl_module_type, jl_tvar_type, jl_method_instance_type, jl_method_type,
                     (void*)CommonSym_tag, (void*)NearbyGlobal_tag, jl_globalref_type,
                     // everything above here represents a class of object rather than only a literal

                     jl_emptysvec, jl_emptytuple, jl_false, jl_true, jl_nothing, jl_any_type,
                     call_sym, invoke_sym, goto_ifnot_sym, return_sym, body_sym, line_sym,
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
                     jl_box_int32(36), jl_box_int32(37),
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
                     jl_box_int64(36), jl_box_int64(37),
#endif
                     jl_labelnode_type, jl_linenumbernode_type, jl_gotonode_type,
                     jl_quotenode_type, jl_type_type, jl_bottom_type, jl_ref_type,
                     jl_pointer_type, jl_vararg_type, jl_abstractarray_type, jl_void_type,
                     jl_densearray_type, jl_function_type, jl_unionall_type, jl_typename_type,
                     jl_builtin_type, jl_task_type, jl_uniontype_type, jl_typetype_type,
                     jl_ANY_flag, jl_array_any_type, jl_intrinsic_type,
                     jl_abstractslot_type, jl_methtable_type, jl_typemap_level_type,
                     jl_voidpointer_type, jl_newvarnode_type, jl_abstractstring_type,
                     jl_array_symbol_type, jl_anytuple_type, jl_tparam0(jl_anytuple_type),
                     jl_typeof(jl_emptytuple), jl_array_uint8_type, jl_symbol_type->name,
                     jl_ssavalue_type->name, jl_tuple_typename, jl_code_info_type, jl_bottomtype_type,
                     ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_ref_type))->name,
                     jl_pointer_typename, jl_simplevector_type->name, jl_datatype_type->name,
                     jl_uniontype_type->name, jl_array_typename, jl_expr_type->name,
                     jl_typename_type->name, jl_type_typename, jl_methtable_type->name,
                     jl_typemap_level_type->name, jl_typemap_entry_type->name, jl_tvar_type->name,
                     ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_abstractarray_type))->name,
                     ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_densearray_type))->name,
                     jl_vararg_typename, jl_void_type->name, jl_method_instance_type->name, jl_method_type->name,
                     jl_module_type->name, jl_function_type->name, jl_typedslot_type->name,
                     jl_abstractslot_type->name, jl_slotnumber_type->name, jl_unionall_type->name,
                     jl_intrinsic_type->name, jl_task_type->name, jl_labelnode_type->name,
                     jl_linenumbernode_type->name, jl_builtin_type->name, jl_gotonode_type->name,
                     jl_quotenode_type->name, jl_globalref_type->name, jl_bottomtype_type->name,
                     jl_string_type->name, jl_abstractstring_type->name,

                     ptls->root_task,

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

    arraylist_new(&builtin_typenames, 0);
    arraylist_push(&builtin_typenames, jl_array_typename);
    arraylist_push(&builtin_typenames, ((jl_datatype_t*)jl_ref_type->body)->name);
    arraylist_push(&builtin_typenames, jl_pointer_typename);
    arraylist_push(&builtin_typenames, jl_type_typename);
    arraylist_push(&builtin_typenames, ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_abstractarray_type))->name);
    arraylist_push(&builtin_typenames, ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_densearray_type))->name);
    arraylist_push(&builtin_typenames, jl_tuple_typename);
    arraylist_push(&builtin_typenames, jl_vararg_typename);
}

#ifdef __cplusplus
}
#endif
