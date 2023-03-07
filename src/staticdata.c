// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  saving and restoring system images

  This performs serialization and deserialization of system and package images. It creates and saves a compact binary
  blob, making deserialization "simple" and fast: we "only" need to deal with uniquing, pointer relocation,
  method root insertion, registering with the garbage collector, making note of special internal types, and
  backedges/invalidation. Special objects include things like builtin functions, C-implemented types (those in jltypes.c),
  the metadata for documentation, optimal layouts, integration with native system image generation, and preparing other
  preprocessing directives.

  During serialization, the flow has several steps:

  - step 1 inserts relevant items into `serialization_order`, an `obj` => `id::Int` mapping. `id` is assigned by
    order of insertion. This stage is implemented by `jl_queue_for_serialization` and its callees;
    while it would be simplest to use recursion, this risks stack overflow, so recursion is mimicked
    using a work-queue managed by `jl_serialize_reachable`.

    It's worth emphasizing that the only goal of this stage is to insert objects into `serialization_order`.
    In later stages, such objects get written in order of `id`.

  - step 2 (the biggest of four steps) takes all items in `serialization_order` and actually serializes them ordered
    by `id`. The system is serialized into several distinct streams (see `jl_serializer_state`), a "main stream"
    (the `s` field) as well as parallel streams for writing specific categories of additional internal data (e.g.,
    global data invisible to codegen, as well as deserialization "touch-up" tables, see below). These different streams
    will be concatenated in later steps. Certain key items (e.g., builtin types & functions associated with `INSERT_TAG`
    below, integers smaller than 512) get serialized via a hard-coded tag table.

    Serialization builds "touch up" tables used during deserialization. Pointers and items requiring gc
    registration get encoded as `(location, target)` pairs in `relocs_list` and `gctags_list`, respectively.
    `location` is the site that needs updating (e.g., the address of a pointer referencing an object), and is
    set to `position(s)`, the offset of the object from the beginning of the deserialized blob.
    `target` is a bitfield-encoded index into lists of different categories of data (e.g., mutable data, constant data,
    symbols, functions, etc.) to which the pointer at `location` refers. The different lists and their bitfield flags
    are given by the `RefTags` enum: if `t` is the category tag (one of the `RefTags` enums) and `i` is the index into
    one of the corresponding categorical list, then `index = t << RELOC_TAG_OFFSET + i`. The simplest source for the
    details of this encoding can be found in the pair of functions `get_reloc_for_item` and `get_item_for_reloc`.

    `uniquing` also holds the serialized location of external DataTypes, MethodInstances, and singletons
    in the serialized blob (i.e., new-at-the-time-of-serialization specializations).

    Most of step 2 is handled by `jl_write_values`, followed by special handling of the dedicated parallel streams.

  - step 3 combines the different sections (fields of `jl_serializer_state`) into one

  - step 4 writes the values of the hard-coded tagged items and `ccallable_list`

Much of the "real work" during deserialization is done by `get_item_for_reloc`. But a few items require specific
attention:
- uniquing: during deserialization, the target item (an "external" type or MethodInstance) must be checked against
  the running system to see whether such an object already exists (i.e., whether some other previously-loaded package
  or workload has created such types/MethodInstances previously) or whether it needs to be created de-novo.
  In either case, all references at `location` must be updated to the one in the running system.
    `new_dt_objs` is a hash set of newly allocated datatype-reachable objects
- method root insertion: when new specializations generate new roots, these roots must be inserted into
  method root tables
- backedges & invalidation: external edges have to be checked against the running system and any invalidations executed.

Encoding of a pointer:
- in the location of the pointer, we initially write zero padding
- for both relocs_list and gctags_list, we write loc/backrefid (for gctags_list this is handled by the caller of write_gctaggedfield,
  for relocs_list it's handled by write_pointerfield)
- when writing to disk, both call get_reloc_for_item, and its return value (subject to modification by gc bits)
  ends up being written into the data stream (s->s), and the data stream's position written to s->relocs

External links:
- location holds the offset
- loc/0 in relocs_list

*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h> // printf
#include <inttypes.h> // PRIxPTR

#include "julia.h"
#include "julia_internal.h"
#include "julia_gcext.h"
#include "builtin_proto.h"
#include "processor.h"
#include "serialize.h"

#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif

#include "valgrind.h"
#include "julia_assert.h"

#include "staticdata_utils.c"
#include "precompile_utils.c"

#ifdef __cplusplus
extern "C" {
#endif

// TODO: put WeakRefs on the weak_refs list during deserialization
// TODO: handle finalizers

#define NUM_TAGS    157

// An array of references that need to be restored from the sysimg
// This is a manually constructed dual of the gvars array, which would be produced by codegen for Julia code, for C.
jl_value_t **const*const get_tags(void) {
    // Make sure to keep an extra slot at the end to sentinel length
    static void * _tags[NUM_TAGS] = {NULL};

    // Lazyily-initialize this list
    if (_tags[0] == NULL) {
        unsigned int i = 0;
#define INSERT_TAG(sym) _tags[i++] = &(sym)
        // builtin types
        INSERT_TAG(jl_any_type);
        INSERT_TAG(jl_symbol_type);
        INSERT_TAG(jl_ssavalue_type);
        INSERT_TAG(jl_datatype_type);
        INSERT_TAG(jl_slotnumber_type);
        INSERT_TAG(jl_simplevector_type);
        INSERT_TAG(jl_array_type);
        INSERT_TAG(jl_expr_type);
        INSERT_TAG(jl_binding_type);
        INSERT_TAG(jl_globalref_type);
        INSERT_TAG(jl_string_type);
        INSERT_TAG(jl_module_type);
        INSERT_TAG(jl_tvar_type);
        INSERT_TAG(jl_method_instance_type);
        INSERT_TAG(jl_method_type);
        INSERT_TAG(jl_code_instance_type);
        INSERT_TAG(jl_linenumbernode_type);
        INSERT_TAG(jl_lineinfonode_type);
        INSERT_TAG(jl_gotonode_type);
        INSERT_TAG(jl_quotenode_type);
        INSERT_TAG(jl_gotoifnot_type);
        INSERT_TAG(jl_argument_type);
        INSERT_TAG(jl_returnnode_type);
        INSERT_TAG(jl_const_type);
        INSERT_TAG(jl_partial_struct_type);
        INSERT_TAG(jl_partial_opaque_type);
        INSERT_TAG(jl_interconditional_type);
        INSERT_TAG(jl_method_match_type);
        INSERT_TAG(jl_pinode_type);
        INSERT_TAG(jl_phinode_type);
        INSERT_TAG(jl_phicnode_type);
        INSERT_TAG(jl_upsilonnode_type);
        INSERT_TAG(jl_type_type);
        INSERT_TAG(jl_bottom_type);
        INSERT_TAG(jl_ref_type);
        INSERT_TAG(jl_pointer_type);
        INSERT_TAG(jl_llvmpointer_type);
        INSERT_TAG(jl_vararg_type);
        INSERT_TAG(jl_abstractarray_type);
        INSERT_TAG(jl_densearray_type);
        INSERT_TAG(jl_nothing_type);
        INSERT_TAG(jl_function_type);
        INSERT_TAG(jl_typeofbottom_type);
        INSERT_TAG(jl_unionall_type);
        INSERT_TAG(jl_typename_type);
        INSERT_TAG(jl_builtin_type);
        INSERT_TAG(jl_code_info_type);
        INSERT_TAG(jl_opaque_closure_type);
        INSERT_TAG(jl_task_type);
        INSERT_TAG(jl_uniontype_type);
        INSERT_TAG(jl_abstractstring_type);
        INSERT_TAG(jl_array_any_type);
        INSERT_TAG(jl_intrinsic_type);
        INSERT_TAG(jl_methtable_type);
        INSERT_TAG(jl_typemap_level_type);
        INSERT_TAG(jl_typemap_entry_type);
        INSERT_TAG(jl_voidpointer_type);
        INSERT_TAG(jl_uint8pointer_type);
        INSERT_TAG(jl_newvarnode_type);
        INSERT_TAG(jl_anytuple_type_type);
        INSERT_TAG(jl_anytuple_type);
        INSERT_TAG(jl_namedtuple_type);
        INSERT_TAG(jl_emptytuple_type);
        INSERT_TAG(jl_array_symbol_type);
        INSERT_TAG(jl_array_uint8_type);
        INSERT_TAG(jl_array_int32_type);
        INSERT_TAG(jl_array_uint64_type);
        INSERT_TAG(jl_int32_type);
        INSERT_TAG(jl_int64_type);
        INSERT_TAG(jl_bool_type);
        INSERT_TAG(jl_uint8_type);
        INSERT_TAG(jl_uint16_type);
        INSERT_TAG(jl_uint32_type);
        INSERT_TAG(jl_uint64_type);
        INSERT_TAG(jl_char_type);
        INSERT_TAG(jl_weakref_type);
        INSERT_TAG(jl_int8_type);
        INSERT_TAG(jl_int16_type);
        INSERT_TAG(jl_float16_type);
        INSERT_TAG(jl_float32_type);
        INSERT_TAG(jl_float64_type);
        INSERT_TAG(jl_floatingpoint_type);
        INSERT_TAG(jl_number_type);
        INSERT_TAG(jl_signed_type);
        INSERT_TAG(jl_pair_type);

        // special typenames
        INSERT_TAG(jl_tuple_typename);
        INSERT_TAG(jl_pointer_typename);
        INSERT_TAG(jl_llvmpointer_typename);
        INSERT_TAG(jl_array_typename);
        INSERT_TAG(jl_type_typename);
        INSERT_TAG(jl_namedtuple_typename);
        INSERT_TAG(jl_vecelement_typename);
        INSERT_TAG(jl_opaque_closure_typename);

        // special exceptions
        INSERT_TAG(jl_errorexception_type);
        INSERT_TAG(jl_argumenterror_type);
        INSERT_TAG(jl_typeerror_type);
        INSERT_TAG(jl_methoderror_type);
        INSERT_TAG(jl_loaderror_type);
        INSERT_TAG(jl_initerror_type);
        INSERT_TAG(jl_undefvarerror_type);
        INSERT_TAG(jl_stackovf_exception);
        INSERT_TAG(jl_diverror_exception);
        INSERT_TAG(jl_interrupt_exception);
        INSERT_TAG(jl_boundserror_type);
        INSERT_TAG(jl_memory_exception);
        INSERT_TAG(jl_undefref_exception);
        INSERT_TAG(jl_readonlymemory_exception);
        INSERT_TAG(jl_atomicerror_type);

        // other special values
        INSERT_TAG(jl_emptysvec);
        INSERT_TAG(jl_emptytuple);
        INSERT_TAG(jl_false);
        INSERT_TAG(jl_true);
        INSERT_TAG(jl_an_empty_string);
        INSERT_TAG(jl_an_empty_vec_any);
        INSERT_TAG(jl_module_init_order);
        INSERT_TAG(jl_core_module);
        INSERT_TAG(jl_base_module);
        INSERT_TAG(jl_main_module);
        INSERT_TAG(jl_top_module);
        INSERT_TAG(jl_typeinf_func);
        INSERT_TAG(jl_type_type_mt);
        INSERT_TAG(jl_nonfunction_mt);
        INSERT_TAG(jl_kwcall_mt);
        INSERT_TAG(jl_kwcall_func);

        // some Core.Builtin Functions that we want to be able to reference:
        INSERT_TAG(jl_builtin_throw);
        INSERT_TAG(jl_builtin_is);
        INSERT_TAG(jl_builtin_typeof);
        INSERT_TAG(jl_builtin_sizeof);
        INSERT_TAG(jl_builtin_issubtype);
        INSERT_TAG(jl_builtin_isa);
        INSERT_TAG(jl_builtin_typeassert);
        INSERT_TAG(jl_builtin__apply_iterate);
        INSERT_TAG(jl_builtin_isdefined);
        INSERT_TAG(jl_builtin_nfields);
        INSERT_TAG(jl_builtin_tuple);
        INSERT_TAG(jl_builtin_svec);
        INSERT_TAG(jl_builtin_getfield);
        INSERT_TAG(jl_builtin_setfield);
        INSERT_TAG(jl_builtin_swapfield);
        INSERT_TAG(jl_builtin_modifyfield);
        INSERT_TAG(jl_builtin_replacefield);
        INSERT_TAG(jl_builtin_fieldtype);
        INSERT_TAG(jl_builtin_arrayref);
        INSERT_TAG(jl_builtin_const_arrayref);
        INSERT_TAG(jl_builtin_arrayset);
        INSERT_TAG(jl_builtin_arraysize);
        INSERT_TAG(jl_builtin_apply_type);
        INSERT_TAG(jl_builtin_applicable);
        INSERT_TAG(jl_builtin_invoke);
        INSERT_TAG(jl_builtin__expr);
        INSERT_TAG(jl_builtin_ifelse);
        INSERT_TAG(jl_builtin__typebody);
        INSERT_TAG(jl_builtin_donotdelete);
        INSERT_TAG(jl_builtin_compilerbarrier);
        INSERT_TAG(jl_builtin_getglobal);
        INSERT_TAG(jl_builtin_setglobal);
        // n.b. must update NUM_TAGS when you add something here
#undef INSERT_TAG
        assert(i == NUM_TAGS - 1);
    }
    return (jl_value_t**const*const) _tags;
}

// hash of definitions for predefined tagged object
static htable_t symbol_table;
static uintptr_t nsym_tag;
// array of definitions for the predefined tagged object types
// (reverse of symbol_table)
static arraylist_t deser_sym;
// Predefined tags that do not have special handling in `externally_linked`
static htable_t external_objects;

static htable_t serialization_order; // to break cycles, mark all objects that are serialized
static htable_t unique_ready; // as we serialize types, we need to know if all reachable objects are also already serialized. This tracks whether `immediate` has been set for all of them.
static htable_t nullptrs;
// FIFO queue for objects to be serialized. Anything requiring fixup upon deserialization
// must be "toplevel" in this queue. For types, parameters and field types must appear
// before the "wrapper" type so they can be properly recached against the running system.
static arraylist_t serialization_queue;
static arraylist_t layout_table;     // cache of `position(s)` for each `id` in `serialization_order`
static arraylist_t object_worklist;  // used to mimic recursion by jl_serialize_reachable

// Permanent list of void* (begin, end+1) pairs of system/package images we've loaded previously
// together with their module build_ids (used for external linkage)
// jl_linkage_blobs.items[2i:2i+1] correspond to build_ids[i]   (0-offset indexing)
// TODO: Keep this sorted so that we can use binary-search
arraylist_t jl_linkage_blobs;
arraylist_t jl_image_relocs;

// hash of definitions for predefined function pointers
static htable_t fptr_to_id;
void *native_functions;   // opaque jl_native_code_desc_t blob used for fetching data from LLVM

// table of struct field addresses to rewrite during saving
static htable_t field_replace;

// array of definitions for the predefined function pointers
// (reverse of fptr_to_id)
// This is a manually constructed dual of the fvars array, which would be produced by codegen for Julia code, for C.
static const jl_fptr_args_t id_to_fptrs[] = {
    &jl_f_throw, &jl_f_is, &jl_f_typeof, &jl_f_issubtype, &jl_f_isa,
    &jl_f_typeassert, &jl_f__apply_iterate, &jl_f__apply_pure,
    &jl_f__call_latest, &jl_f__call_in_world, &jl_f__call_in_world_total, &jl_f_isdefined,
    &jl_f_tuple, &jl_f_svec, &jl_f_intrinsic_call,
    &jl_f_getfield, &jl_f_setfield, &jl_f_swapfield, &jl_f_modifyfield,
    &jl_f_replacefield, &jl_f_fieldtype, &jl_f_nfields,
    &jl_f_arrayref, &jl_f_const_arrayref, &jl_f_arrayset, &jl_f_arraysize, &jl_f_apply_type,
    &jl_f_applicable, &jl_f_invoke, &jl_f_sizeof, &jl_f__expr, &jl_f__typevar,
    &jl_f_ifelse, &jl_f__structtype, &jl_f__abstracttype, &jl_f__primitivetype,
    &jl_f__typebody, &jl_f__setsuper, &jl_f__equiv_typedef, &jl_f_get_binding_type,
    &jl_f_set_binding_type, &jl_f_opaque_closure_call, &jl_f_donotdelete, &jl_f_compilerbarrier,
    &jl_f_getglobal, &jl_f_setglobal, &jl_f_finalizer, &jl_f__compute_sparams, &jl_f__svec_ref,
    NULL };

typedef struct {
    ios_t *s;                   // the main stream
    ios_t *const_data;          // codegen-invisible internal data (e.g., datatype layouts, list-like typename fields, foreign types, internal arrays)
    ios_t *symbols;             // names (char*) of symbols (some may be referenced by pointer in generated code)
    ios_t *relocs;              // for (de)serializing relocs_list and gctags_list
    ios_t *gvar_record;         // serialized array mapping gvid => spos
    ios_t *fptr_record;         // serialized array mapping fptrid => spos
    arraylist_t relocs_list;    // a list of (location, target) pairs, see description at top
    arraylist_t gctags_list;    //      "
    arraylist_t uniquing_types; // a list of locations that reference types that must be de-duplicated
    arraylist_t uniquing_objs;  // a list of locations that reference non-types that must be de-duplicated
    arraylist_t fixup_types;    // a list of locations of types requiring (re)caching
    arraylist_t fixup_objs;     // a list of locations of objects requiring (re)caching
    arraylist_t ccallable_list; // @ccallable entry points to install
    // mapping from a buildid_idx to a depmods_idx
    jl_array_t *buildid_depmods_idxs;
    // record of build_ids for all external linkages, in order of serialization for the current sysimg/pkgimg
    // conceptually, the base pointer for the jth externally-linked item is determined from
    //     i = findfirst(==(link_ids[j]), build_ids)
    //     blob_base = jl_linkage_blobs.items[2i]                     # 0-offset indexing
    // We need separate lists since they are intermingled at creation but split when written.
    jl_array_t *link_ids_relocs;
    jl_array_t *link_ids_gctags;
    jl_array_t *link_ids_gvars;
    jl_array_t *link_ids_external_fnvars;
    jl_ptls_t ptls;
    htable_t callers_with_edges;
    jl_image_t *image;
    int8_t incremental;
} jl_serializer_state;

static jl_value_t *jl_idtable_type = NULL;
static jl_typename_t *jl_idtable_typename = NULL;
static jl_value_t *jl_bigint_type = NULL;
static int gmp_limb_size = 0;
static jl_sym_t *jl_docmeta_sym = NULL;

#ifdef _P64
#define RELOC_TAG_OFFSET 61
#define DEPS_IDX_OFFSET 40    // only on 64-bit can we encode the dependency-index as part of the tagged reloc
#else
// this supports up to 8 RefTags, 512MB of pointer data, and 4/2 (64/32-bit) GB of constant data.
#define RELOC_TAG_OFFSET 29
#define DEPS_IDX_OFFSET RELOC_TAG_OFFSET
#endif


// Tags of category `t` are located at offsets `t << RELOC_TAG_OFFSET`
// Consequently there is room for 2^RELOC_TAG_OFFSET pointers, etc
enum RefTags {
    DataRef,            // mutable data
    ConstDataRef,       // constant data (e.g., layouts)
    TagRef,             // items serialized via their tags
    SymbolRef,          // symbols
    FunctionRef,        // functions
    SysimageLinkage,    // reference to the sysimage (from pkgimage)
    ExternalLinkage     // reference to some other pkgimage
};

// calling conventions for internal entry points.
// this is used to set the method-instance->invoke field
typedef enum {
    JL_API_NULL,
    JL_API_BOXED,
    JL_API_CONST,
    JL_API_WITH_PARAMETERS,
    JL_API_INTERPRETED,
    JL_API_BUILTIN,
    JL_API_MAX
} jl_callingconv_t;

// Sub-divisions of some RefTags
const uintptr_t BuiltinFunctionTag = ((uintptr_t)1 << (RELOC_TAG_OFFSET - 1));


#if RELOC_TAG_OFFSET <= 32
typedef uint32_t reloc_t;
#else
typedef uint64_t reloc_t;
#endif
static void write_reloc_t(ios_t *s, uintptr_t reloc_id) JL_NOTSAFEPOINT
{
    if (sizeof(reloc_t) <= sizeof(uint32_t)) {
        assert(reloc_id < UINT32_MAX);
        write_uint32(s, reloc_id);
    }
    else {
        write_uint64(s, reloc_id);
    }
}

// Reporting to PkgCacheInspector
typedef struct {
    size_t sysdata;
    size_t isbitsdata;
    size_t symboldata;
    size_t tagslist;
    size_t reloclist;
    size_t gvarlist;
    size_t fptrlist;
} pkgcachesizes;

// --- Static Compile ---
static void *jl_sysimg_handle = NULL;
static jl_image_t sysimage;

static inline uintptr_t *sysimg_gvars(uintptr_t *base, const int32_t *offsets, size_t idx)
{
    return base + offsets[idx] / sizeof(base[0]);
}

JL_DLLEXPORT int jl_running_on_valgrind(void)
{
    return RUNNING_ON_VALGRIND;
}

static void jl_load_sysimg_so(void)
{
    int imaging_mode = jl_generating_output() && !jl_options.incremental;
    // in --build mode only use sysimg data, not precompiled native code
    if (!imaging_mode && jl_options.use_sysimage_native_code==JL_OPTIONS_USE_SYSIMAGE_NATIVE_CODE_YES) {
        assert(sysimage.fptrs.base);
    }
    else {
        memset(&sysimage.fptrs, 0, sizeof(sysimage.fptrs));
    }
    const char *sysimg_data;
    jl_dlsym(jl_sysimg_handle, "jl_system_image_data", (void **)&sysimg_data, 1);
    size_t *plen;
    jl_dlsym(jl_sysimg_handle, "jl_system_image_size", (void **)&plen, 1);
    jl_restore_system_image_data(sysimg_data, *plen);
}


// --- serializer ---

#define NBOX_C 1024

static int jl_needs_serialization(jl_serializer_state *s, jl_value_t *v) JL_NOTSAFEPOINT
{
    // ignore items that are given a special relocation representation
    if (s->incremental && jl_object_in_image(v))
        return 0;

    if (v == NULL || jl_is_symbol(v) || v == jl_nothing) {
        return 0;
    }
    else if (jl_typeis(v, jl_int64_type)) {
        int64_t i64 = *(int64_t*)v + NBOX_C / 2;
        if ((uint64_t)i64 < NBOX_C)
            return 0;
    }
    else if (jl_typeis(v, jl_int32_type)) {
        int32_t i32 = *(int32_t*)v + NBOX_C / 2;
        if ((uint32_t)i32 < NBOX_C)
            return 0;
    }
    else if (jl_typeis(v, jl_uint8_type)) {
        return 0;
    }
    else if (jl_typeis(v, jl_task_type)) {
        return 0;
    }

    return 1;
}


static int caching_tag(jl_value_t *v) JL_NOTSAFEPOINT
{
    if (jl_is_method_instance(v)) {
        jl_method_instance_t *mi = (jl_method_instance_t*)v;
        jl_value_t *m = mi->def.value;
        if (jl_is_method(m) && jl_object_in_image(m))
            return 1 + type_in_worklist(mi->specTypes);
    }
    if (jl_is_datatype(v)) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        if (jl_is_tuple_type(dt) ? !dt->isconcretetype : dt->hasfreetypevars)
            return 0; // aka !is_cacheable from jltypes.c
        if (jl_object_in_image((jl_value_t*)dt->name))
            return 1 + type_in_worklist(v);
    }
    jl_value_t *dtv = jl_typeof(v);
    if (jl_is_datatype_singleton((jl_datatype_t*)dtv)) {
        return 1 - type_in_worklist(dtv); // these are already recached in the datatype in the image
    }
    return 0;
}

static int needs_recaching(jl_value_t *v) JL_NOTSAFEPOINT
{
    return caching_tag(v) == 2;
}

static int needs_uniquing(jl_value_t *v) JL_NOTSAFEPOINT
{
    assert(!jl_object_in_image(v));
    return caching_tag(v) == 1;
}

static void record_field_change(jl_value_t **addr, jl_value_t *newval) JL_NOTSAFEPOINT
{
    ptrhash_put(&field_replace, (void*)addr, newval);
}

static jl_value_t *get_replaceable_field(jl_value_t **addr, int mutabl) JL_GC_DISABLED
{
    jl_value_t *fld = (jl_value_t*)ptrhash_get(&field_replace, addr);
    if (fld == HT_NOTFOUND) {
        fld = *addr;
        if (mutabl && fld && jl_is_cpointer_type(jl_typeof(fld)) && jl_unbox_voidpointer(fld) != NULL && jl_unbox_voidpointer(fld) != (void*)(uintptr_t)-1) {
            void **nullval = ptrhash_bp(&nullptrs, (void*)jl_typeof(fld));
            if (*nullval == HT_NOTFOUND) {
                void *C_NULL = NULL;
                *nullval = (void*)jl_new_bits(jl_typeof(fld), &C_NULL);
            }
            fld = (jl_value_t*)*nullval;
        }
        return fld;
    }
    return fld;
}

static uintptr_t jl_fptr_id(void *fptr)
{
    void **pbp = ptrhash_bp(&fptr_to_id, fptr);
    if (*pbp == HT_NOTFOUND || fptr == NULL)
        return 0;
    else
        return *(uintptr_t*)pbp;
}

// `jl_queue_for_serialization` adds items to `serialization_order`
#define jl_queue_for_serialization(s, v) jl_queue_for_serialization_((s), (jl_value_t*)(v), 1, 0)
static void jl_queue_for_serialization_(jl_serializer_state *s, jl_value_t *v, int recursive, int immediate) JL_GC_DISABLED;


static void jl_queue_module_for_serialization(jl_serializer_state *s, jl_module_t *m) JL_GC_DISABLED
{
    jl_queue_for_serialization(s, m->name);
    jl_queue_for_serialization(s, m->parent);
    jl_queue_for_serialization(s, m->bindings);
    jl_queue_for_serialization(s, m->bindingkeyset);
    if (jl_options.strip_metadata) {
        jl_svec_t *table = jl_atomic_load_relaxed(&m->bindings);
        for (size_t i = 0; i < jl_svec_len(table); i++) {
            jl_binding_t *b = (jl_binding_t*)jl_svec_ref(table, i);
            if ((void*)b == jl_nothing)
                break;
            jl_sym_t *name = b->globalref->name;
            if (name == jl_docmeta_sym && jl_atomic_load_relaxed(&b->value))
                record_field_change((jl_value_t**)&b->value, jl_nothing);
        }
    }

    for (size_t i = 0; i < m->usings.len; i++) {
        jl_queue_for_serialization(s, (jl_value_t*)m->usings.items[i]);
    }
}

// Anything that requires uniquing or fixing during deserialization needs to be "toplevel"
// in serialization (i.e., have its own entry in `serialization_order`). Consequently,
// objects that act as containers for other potentially-"problematic" objects must add such "children"
// to the queue.
// Most objects use preorder traversal. But things that need uniquing require postorder:
// you want to handle uniquing of `Dict{String,Float64}` before you tackle `Vector{Dict{String,Float64}}`.
// Uniquing is done in `serialization_order`, so the very first mention of such an object must
// be the "source" rather than merely a cross-reference.
static void jl_insert_into_serialization_queue(jl_serializer_state *s, jl_value_t *v, int recursive, int immediate) JL_GC_DISABLED
{
    jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
    jl_queue_for_serialization_(s, (jl_value_t*)t, 1, immediate);

    if (!recursive)
        goto done_fields;

    if (s->incremental && jl_is_datatype(v) && immediate) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        // ensure super is queued (though possibly not yet handled, since it may have cycles)
        jl_queue_for_serialization_(s, (jl_value_t*)dt->super, 1, 1);
        // ensure all type parameters are recached
        jl_queue_for_serialization_(s, (jl_value_t*)dt->parameters, 1, 1);
        jl_value_t *singleton = dt->instance;
        if (singleton && needs_uniquing(singleton)) {
            assert(jl_needs_serialization(s, singleton)); // should be true, since we visited dt
            // do not visit dt->instance for our template object as it leads to unwanted cycles here
            // (it may get serialized from elsewhere though)
            record_field_change(&dt->instance, jl_nothing);
        }
        immediate = 0; // do not handle remaining fields immediately (just field types remains)
    }
    if (s->incremental && jl_is_method_instance(v)) {
        jl_method_instance_t *mi = (jl_method_instance_t*)v;
        jl_value_t *def = mi->def.value;
        if (needs_uniquing(v)) {
            // we only need 3 specific fields of this (the rest are not used)
            jl_queue_for_serialization(s, mi->def.value);
            jl_queue_for_serialization(s, mi->specTypes);
            jl_queue_for_serialization(s, (jl_value_t*)mi->sparam_vals);
            recursive = 0;
            goto done_fields;
        }
        else if (jl_is_method(def) && jl_object_in_image(def)) {
            // we only need 3 specific fields of this (the rest are restored afterward, if valid)
            // in particular, cache is repopulated by jl_mi_cache_insert for all foreign function,
            // so must not be present here
            record_field_change((jl_value_t**)&mi->uninferred, NULL);
            record_field_change((jl_value_t**)&mi->backedges, NULL);
            record_field_change((jl_value_t**)&mi->callbacks, NULL);
            record_field_change((jl_value_t**)&mi->cache, NULL);
        }
        else {
            assert(!needs_recaching(v));
        }
        // n.b. opaque closures cannot be inspected and relied upon like a
        // normal method since they can get improperly introduced by generated
        // functions, so if they appeared at all, we will probably serialize
        // them wrong and segfault. The jl_code_for_staged function should
        // prevent this from happening, so we do not need to detect that user
        // error now.
    }
    if (s->incremental && jl_is_globalref(v)) {
        jl_globalref_t *gr = (jl_globalref_t*)v;
        if (jl_object_in_image((jl_value_t*)gr->mod)) {
            record_field_change((jl_value_t**)&gr->binding, NULL);
        }
    }
    if (jl_is_typename(v)) {
        jl_typename_t *tn = (jl_typename_t*)v;
        // don't recurse into several fields (yet)
        jl_queue_for_serialization_(s, (jl_value_t*)tn->cache, 0, 1);
        jl_queue_for_serialization_(s, (jl_value_t*)tn->linearcache, 0, 1);
        if (s->incremental) {
            assert(!jl_object_in_image((jl_value_t*)tn->module));
            assert(!jl_object_in_image((jl_value_t*)tn->wrapper));
        }
    }
    if (s->incremental && jl_is_code_instance(v)) {
        jl_code_instance_t *ci = (jl_code_instance_t*)v;
        // make sure we don't serialize other reachable cache entries of foreign methods
        if (jl_object_in_image((jl_value_t*)ci->def->def.value)) {
            // TODO: if (ci in ci->defs->cache)
            record_field_change((jl_value_t**)&ci->next, NULL);
        }
    }


    if (immediate) // must be things that can be recursively handled, and valid as type parameters
        assert(jl_is_immutable(t) || jl_is_typevar(v) || jl_is_symbol(v) || jl_is_svec(v));

    const jl_datatype_layout_t *layout = t->layout;
    if (layout->npointers == 0) {
        // bitstypes do not require recursion
    }
    else if (jl_is_svec(v)) {
        size_t i, l = jl_svec_len(v);
        jl_value_t **data = jl_svec_data(v);
        for (i = 0; i < l; i++) {
            jl_queue_for_serialization_(s, data[i], 1, immediate);
        }
    }
    else if (jl_is_array(v)) {
        jl_array_t *ar = (jl_array_t*)v;
        const char *data = (const char*)jl_array_data(ar);
        if (ar->flags.ptrarray) {
            size_t i, l = jl_array_len(ar);
            for (i = 0; i < l; i++) {
                jl_value_t *fld = get_replaceable_field(&((jl_value_t**)data)[i], 1);
                jl_queue_for_serialization_(s, fld, 1, immediate);
            }
        }
        else if (ar->flags.hasptr) {
            uint16_t elsz = ar->elsize;
            size_t i, l = jl_array_len(ar);
            jl_datatype_t *et = (jl_datatype_t*)jl_tparam0(jl_typeof(ar));
            size_t j, np = et->layout->npointers;
            for (i = 0; i < l; i++) {
                for (j = 0; j < np; j++) {
                    uint32_t ptr = jl_ptr_offset(et, j);
                    jl_value_t *fld = get_replaceable_field(&((jl_value_t**)data)[ptr], 1);
                    jl_queue_for_serialization_(s, fld, 1, immediate);
                }
                data += elsz;
            }
        }
    }
    else if (jl_typeis(v, jl_module_type)) {
        jl_queue_module_for_serialization(s, (jl_module_t*)v);
    }
    else if (layout->nfields > 0) {
        char *data = (char*)jl_data_ptr(v);
        size_t i, np = layout->npointers;
        for (i = 0; i < np; i++) {
            uint32_t ptr = jl_ptr_offset(t, i);
            int mutabl = t->name->mutabl;
            if (jl_is_binding(v) && ((jl_binding_t*)v)->constp && i == 0) // value field depends on constp field
                mutabl = 0;
            jl_value_t *fld = get_replaceable_field(&((jl_value_t**)data)[ptr], mutabl);
            jl_queue_for_serialization_(s, fld, 1, immediate);
        }
    }

done_fields: ;

    // We've encountered an item we need to cache
    void **bp = ptrhash_bp(&serialization_order, v);
    assert(*bp != (void*)(uintptr_t)-1);
    if (s->incremental) {
        void **bp2 = ptrhash_bp(&unique_ready, v);
        if (*bp2 == HT_NOTFOUND)
            assert(*bp == (void*)(uintptr_t)-2);
        else if (*bp != (void*)(uintptr_t)-2)
            return;
    }
    else {
        assert(*bp == (void*)(uintptr_t)-2);
    }
    arraylist_push(&serialization_queue, (void*) v);
    size_t idx = serialization_queue.len - 1;
    assert(serialization_queue.len < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "too many items to serialize");

    *bp = (void*)((char*)HT_NOTFOUND + 1 + idx);
}

static void jl_queue_for_serialization_(jl_serializer_state *s, jl_value_t *v, int recursive, int immediate) JL_GC_DISABLED
{
    if (!jl_needs_serialization(s, v))
        return;

    jl_value_t *t = jl_typeof(v);
    // Items that require postorder traversal must visit their children prior to insertion into
    // the worklist/serialization_order (and also before their first use)
    if (s->incremental && !immediate) {
        if (jl_is_datatype(t) && needs_uniquing(v))
            immediate = 1;
        if (jl_is_datatype_singleton((jl_datatype_t*)t) && needs_uniquing(v))
            immediate = 1;
    }

    void **bp = ptrhash_bp(&serialization_order, v);
    if (*bp == HT_NOTFOUND) {
        *bp = (void*)(uintptr_t)(immediate ? -2 : -1);
    }
    else {
        if (!s->incremental || !immediate || !recursive)
            return;
        void **bp2 = ptrhash_bp(&unique_ready, v);
        if (*bp2 == HT_NOTFOUND)
            *bp2 = v; // now is unique_ready
        else {
            assert(*bp != (void*)(uintptr_t)-1);
            return; // already was unique_ready
        }
        assert(*bp != (void*)(uintptr_t)-2); // should be unique_ready then
        if (*bp == (void*)(uintptr_t)-1)
            *bp = (void*)(uintptr_t)-2; // now immediate
    }

    if (immediate)
        jl_insert_into_serialization_queue(s, v, recursive, immediate);
    else
        arraylist_push(&object_worklist, (void*)v);
}

// Do a pre-order traversal of the to-serialize worklist, in the identical order
// to the calls to jl_queue_for_serialization would occur in a purely recursive
// implementation, but without potentially running out of stack.
static void jl_serialize_reachable(jl_serializer_state *s) JL_GC_DISABLED
{
    size_t i, prevlen = 0;
    while (object_worklist.len) {
        // reverse!(object_worklist.items, prevlen:end);
        // prevlen is the index of the first new object
        for (i = prevlen; i < object_worklist.len; i++) {
            size_t j = object_worklist.len - i + prevlen - 1;
            void *tmp = object_worklist.items[i];
            object_worklist.items[i] = object_worklist.items[j];
            object_worklist.items[j] = tmp;
        }
        prevlen = --object_worklist.len;
        jl_value_t *v = (jl_value_t*)object_worklist.items[prevlen];
        void **bp = ptrhash_bp(&serialization_order, (void*)v);
        assert(*bp != HT_NOTFOUND && *bp != (void*)(uintptr_t)-2);
        if (*bp == (void*)(uintptr_t)-1) { // might have been eagerly handled for post-order while in the lazy pre-order queue
            *bp = (void*)(uintptr_t)-2;
            jl_insert_into_serialization_queue(s, v, 1, 0);
        }
        else {
            assert(s->incremental);
        }
    }
}

static void ios_ensureroom(ios_t *s, size_t newsize) JL_NOTSAFEPOINT
{
    size_t prevsize = s->size;
    if (prevsize < newsize) {
        ios_trunc(s, newsize);
        assert(s->size == newsize);
        memset(&s->buf[prevsize], 0, newsize - prevsize);
    }
}

static void write_padding(ios_t *s, size_t nb) JL_NOTSAFEPOINT
{
    static const char zeros[16] = {0};
    while (nb > 16) {
        ios_write(s, zeros, 16);
        nb -= 16;
    }
    if (nb != 0)
        ios_write(s, zeros, nb);
}

static void write_pointer(ios_t *s) JL_NOTSAFEPOINT
{
    assert((ios_pos(s) & (sizeof(void*) - 1)) == 0 && "stream misaligned for writing a word-sized value");
    write_uint(s, 0);
}

// Records the buildid holding `v` and returns the tagged offset within the corresponding image
static uintptr_t add_external_linkage(jl_serializer_state *s, jl_value_t *v, jl_array_t *link_ids) {
    size_t i = external_blob_index(v);
    if (i < n_linkage_blobs()) {
        // We found the sysimg/pkg that this item links against
        // Compute the relocation code
        size_t offset = (uintptr_t)v - (uintptr_t)jl_linkage_blobs.items[2*i];
        offset /= sizeof(void*);
        assert(offset < ((uintptr_t)1 << DEPS_IDX_OFFSET) && "offset to external image too large");
        assert(n_linkage_blobs() == jl_array_len(s->buildid_depmods_idxs));
        size_t depsidx = ((uint32_t*)jl_array_data(s->buildid_depmods_idxs))[i]; // map from build_id_idx -> deps_idx
        assert(depsidx < INT32_MAX);
        if (depsidx < ((uintptr_t)1 << (RELOC_TAG_OFFSET - DEPS_IDX_OFFSET)) && offset < ((uintptr_t)1 << DEPS_IDX_OFFSET))
            // if it fits in a SysimageLinkage type, use that representation
            return ((uintptr_t)SysimageLinkage << RELOC_TAG_OFFSET) + ((uintptr_t)depsidx << DEPS_IDX_OFFSET) + offset;
        // otherwise, we store the image key in `link_ids`
        assert(link_ids && jl_is_array(link_ids));
        jl_array_grow_end(link_ids, 1);
        uint32_t *link_id_data  = (uint32_t*)jl_array_data(link_ids);  // wait until after the `grow`
        link_id_data[jl_array_len(link_ids) - 1] = depsidx;
        return ((uintptr_t)ExternalLinkage << RELOC_TAG_OFFSET) + offset;
    }
    return 0;
}

// Return the integer `id` for `v`. Generically this is looked up in `serialization_order`,
// but symbols, small integers, and a couple of special items (`nothing` and the root Task)
// have special handling.
#define backref_id(s, v, link_ids) _backref_id(s, (jl_value_t*)(v), link_ids)
static uintptr_t _backref_id(jl_serializer_state *s, jl_value_t *v, jl_array_t *link_ids) JL_NOTSAFEPOINT
{
    assert(v != NULL && "cannot get backref to NULL object");
    void *idx = HT_NOTFOUND;
    if (jl_is_symbol(v)) {
        void **pidx = ptrhash_bp(&symbol_table, v);
        idx = *pidx;
        if (idx == HT_NOTFOUND) {
            size_t l = strlen(jl_symbol_name((jl_sym_t*)v));
            write_uint32(s->symbols, l);
            ios_write(s->symbols, jl_symbol_name((jl_sym_t*)v), l + 1);
            size_t offset = ++nsym_tag;
            assert(offset < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "too many symbols");
            idx = (void*)((char*)HT_NOTFOUND + ((uintptr_t)SymbolRef << RELOC_TAG_OFFSET) + offset);
            *pidx = idx;
        }
    }
    else if (v == (jl_value_t*)s->ptls->root_task) {
        return (uintptr_t)TagRef << RELOC_TAG_OFFSET;
    }
    else if (v == jl_nothing) {
        return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + 1;
    }
    else if (jl_typeis(v, jl_int64_type)) {
        int64_t i64 = *(int64_t*)v + NBOX_C / 2;
        if ((uint64_t)i64 < NBOX_C)
            return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + i64 + 2;
    }
    else if (jl_typeis(v, jl_int32_type)) {
        int32_t i32 = *(int32_t*)v + NBOX_C / 2;
        if ((uint32_t)i32 < NBOX_C)
            return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + i32 + 2 + NBOX_C;
    }
    else if (jl_typeis(v, jl_uint8_type)) {
        uint8_t u8 = *(uint8_t*)v;
        return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + u8 + 2 + NBOX_C + NBOX_C;
    }
    if (s->incremental && jl_object_in_image(v)) {
        assert(link_ids);
        uintptr_t item = add_external_linkage(s, v, link_ids);
        assert(item && "no external linkage identified");
        return item;
    }
    if (idx == HT_NOTFOUND) {
        idx = ptrhash_get(&serialization_order, v);
        if (idx == HT_NOTFOUND) {
            jl_(jl_typeof(v));
            jl_(v);
        }
        assert(idx != HT_NOTFOUND && "object missed during jl_queue_for_serialization pass");
        assert(idx != (void*)(uintptr_t)-1 && "object missed during jl_insert_into_serialization_queue pass");
        assert(idx != (void*)(uintptr_t)-2 && "object missed during jl_insert_into_serialization_queue pass");
    }
    return (char*)idx - 1 - (char*)HT_NOTFOUND;
}


static void record_uniquing(jl_serializer_state *s, jl_value_t *fld, uintptr_t offset) JL_NOTSAFEPOINT
{
    if (s->incremental && jl_needs_serialization(s, fld) && needs_uniquing(fld)) {
        if (jl_is_datatype(fld) || jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(fld)))
            arraylist_push(&s->uniquing_types, (void*)(uintptr_t)offset);
        else
            arraylist_push(&s->uniquing_objs, (void*)(uintptr_t)offset);
    }
}

// Save blank space in stream `s` for a pointer `fld`, storing both location and target
// in `relocs_list`.
static void write_pointerfield(jl_serializer_state *s, jl_value_t *fld) JL_NOTSAFEPOINT
{
    if (fld != NULL) {
        arraylist_push(&s->relocs_list, (void*)(uintptr_t)ios_pos(s->s));
        arraylist_push(&s->relocs_list, (void*)backref_id(s, fld, s->link_ids_relocs));
        record_uniquing(s, fld, ios_pos(s->s));
    }
    write_pointer(s->s);
}

// Save blank space in stream `s` for a pointer `fld`, storing both location and target
// in `gctags_list`.
static void write_gctaggedfield(jl_serializer_state *s, jl_datatype_t *ref) JL_NOTSAFEPOINT
{
    // jl_printf(JL_STDOUT, "gctaggedfield: position %p, value 0x%lx\n", (void*)(uintptr_t)ios_pos(s->s), ref);
    arraylist_push(&s->gctags_list, (void*)(uintptr_t)ios_pos(s->s));
    arraylist_push(&s->gctags_list, (void*)backref_id(s, ref, s->link_ids_gctags));
    write_pointer(s->s);
}

// Special handling from `jl_write_values` for modules
static void jl_write_module(jl_serializer_state *s, uintptr_t item, jl_module_t *m) JL_GC_DISABLED
{
    size_t reloc_offset = ios_pos(s->s);
    size_t tot = sizeof(jl_module_t);
    ios_write(s->s, (char*)m, tot);     // raw memory dump of the `jl_module_t` structure
    // will need to recreate the binding table for this
    arraylist_push(&s->fixup_objs, (void*)reloc_offset);

    // Handle the fields requiring special attention
    jl_module_t *newm = (jl_module_t*)&s->s->buf[reloc_offset];
    newm->name = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, name)));
    arraylist_push(&s->relocs_list, (void*)backref_id(s, m->name, s->link_ids_relocs));
    newm->parent = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, parent)));
    arraylist_push(&s->relocs_list, (void*)backref_id(s, m->parent, s->link_ids_relocs));
    newm->bindings = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, bindings)));
    arraylist_push(&s->relocs_list, (void*)backref_id(s, m->bindings, s->link_ids_relocs));
    newm->bindingkeyset = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, bindingkeyset)));
    arraylist_push(&s->relocs_list, (void*)backref_id(s, m->bindingkeyset, s->link_ids_relocs));
    newm->primary_world = ~(size_t)0;

    // write out the usings list
    memset(&newm->usings._space, 0, sizeof(newm->usings._space));
    if (m->usings.items == &m->usings._space[0]) {
        newm->usings.items = (void**)offsetof(jl_module_t, usings._space);
        arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings.items)));
        arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item));
        size_t i;
        for (i = 0; i < m->usings.len; i++) {
            arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings._space[i])));
            arraylist_push(&s->relocs_list, (void*)backref_id(s, m->usings._space[i], s->link_ids_relocs));
        }
    }
    else {
        newm->usings.items = (void**)tot;
        arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings.items)));
        arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item));
        size_t i;
        for (i = 0; i < m->usings.len; i++) {
            write_pointerfield(s, (jl_value_t*)m->usings.items[i]);
            tot += sizeof(void*);
        }
        for (; i < m->usings.max; i++) {
            write_pointer(s->s);
            tot += sizeof(void*);
        }
    }
    assert(ios_pos(s->s) - reloc_offset == tot);
}

static void record_gvars(jl_serializer_state *s, arraylist_t *globals) JL_NOTSAFEPOINT
{
    for (size_t i = 0; i < globals->len; i++)
        jl_queue_for_serialization(s, globals->items[i]);
}

static void record_external_fns(jl_serializer_state *s, arraylist_t *external_fns) JL_NOTSAFEPOINT
{
    if (!s->incremental) {
        assert(external_fns->len == 0);
        (void) external_fns;
        return;
    }

    // We could call jl_queue_for_serialization here, but that should
    // always be a no-op.
#ifndef JL_NDEBUG
    for (size_t i = 0; i < external_fns->len; i++) {
        jl_code_instance_t *ci = (jl_code_instance_t*)external_fns->items[i];
        assert(jl_atomic_load_relaxed(&ci->specsigflags) & 0b100);
    }
#endif
}

jl_value_t *jl_find_ptr = NULL;
// The main function for serializing all the items queued in `serialization_order`
// (They are also stored in `serialization_queue` which is order-preserving, unlike the hash table used
//  for `serialization_order`).
static void jl_write_values(jl_serializer_state *s) JL_GC_DISABLED
{
    size_t l = serialization_queue.len;

    arraylist_new(&layout_table, 0);
    arraylist_grow(&layout_table, l * 2);
    memset(layout_table.items, 0, l * 2 * sizeof(void*));

    // Serialize all entries
    for (size_t item = 0; item < l; item++) {
        jl_value_t *v = (jl_value_t*)serialization_queue.items[item];           // the object
        JL_GC_PROMISE_ROOTED(v);
        assert(!(s->incremental && jl_object_in_image(v)));
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        assert((t->instance == NULL || t->instance == v) && "detected singleton construction corruption");
        // realign stream to expected gc alignment (16 bytes)
        uintptr_t skip_header_pos = ios_pos(s->s) + sizeof(jl_taggedvalue_t);
        write_padding(s->s, LLT_ALIGN(skip_header_pos, 16) - skip_header_pos);

        // write header
        if (s->incremental && jl_needs_serialization(s, (jl_value_t*)t) && needs_uniquing((jl_value_t*)t))
            arraylist_push(&s->uniquing_types, (void*)(uintptr_t)(ios_pos(s->s)|1));
        write_gctaggedfield(s, t);
        size_t reloc_offset = ios_pos(s->s);
        assert(item < layout_table.len && layout_table.items[item] == NULL);
        layout_table.items[item] = (void*)reloc_offset;               // store the inverse mapping of `serialization_order` (`id` => object-as-streampos)

        if (s->incremental && needs_uniquing(v)) {
            if (jl_is_method_instance(v)) {
                jl_method_instance_t *mi = (jl_method_instance_t*)v;
                write_pointerfield(s, mi->def.value);
                write_pointerfield(s, mi->specTypes);
                write_pointerfield(s, (jl_value_t*)mi->sparam_vals);
                continue;
            }
            else if (!jl_is_datatype(v)) {
                assert(jl_is_datatype_singleton(t) && "unreachable");
            }
        }
        else if (s->incremental && needs_recaching(v)) {
            arraylist_push(jl_is_datatype(v) ? &s->fixup_types : &s->fixup_objs, (void*)reloc_offset);
        }
        else if (s->incremental && jl_typeis(v, jl_binding_type)) {
            jl_binding_t *b = (jl_binding_t*)v;
            if (b->globalref == NULL || jl_object_in_image((jl_value_t*)b->globalref->mod))
                jl_error("Binding cannot be serialized"); // no way (currently) to recover its identity
        }

        // write data
        if (jl_is_array(v)) {
            // Internal data for types in julia.h with `jl_array_t` field(s)
#define JL_ARRAY_ALIGN(jl_value, nbytes) LLT_ALIGN(jl_value, nbytes)
            jl_array_t *ar = (jl_array_t*)v;
            jl_value_t *et = jl_tparam0(jl_typeof(v));
            size_t alen = jl_array_len(ar);
            size_t datasize = alen * ar->elsize;
            size_t tot = datasize;
            int isbitsunion = jl_array_isbitsunion(ar);
            if (isbitsunion)
                tot += alen;
            else if (ar->elsize == 1)
                tot += 1;
            int ndimwords = jl_array_ndimwords(ar->flags.ndims);
            size_t headersize = sizeof(jl_array_t) + ndimwords*sizeof(size_t);
            // copy header
            ios_write(s->s, (char*)v, headersize);
            size_t alignment_amt = JL_SMALL_BYTE_ALIGNMENT;
            if (tot >= ARRAY_CACHE_ALIGN_THRESHOLD)
                alignment_amt = JL_CACHE_BYTE_ALIGNMENT;
            // make some header modifications in-place
            jl_array_t *newa = (jl_array_t*)&s->s->buf[reloc_offset];
            if (newa->flags.ndims == 1)
                newa->maxsize = alen;
            newa->offset = 0;
            newa->flags.how = 0;
            newa->flags.pooled = 0;
            newa->flags.isshared = 0;

            // write data
            if (!ar->flags.ptrarray && !ar->flags.hasptr) {
                // Non-pointer eltypes get encoded in the const_data section
                uintptr_t data = LLT_ALIGN(ios_pos(s->const_data), alignment_amt);
                write_padding(s->const_data, data - ios_pos(s->const_data));
                // write data and relocations
                newa->data = NULL; // relocation offset
                data /= sizeof(void*);
                assert(data < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "offset to constant data too large");
                arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_array_t, data))); // relocation location
                arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + data)); // relocation target
                if (jl_is_cpointer_type(et)) {
                    // reset Ptr fields to C_NULL (but keep MAP_FAILED / INVALID_HANDLE)
                    const intptr_t *data = (const intptr_t*)jl_array_data(ar);
                    size_t i;
                    for (i = 0; i < alen; i++) {
                        if (data[i] != -1)
                            write_pointer(s->const_data);
                        else
                            ios_write(s->const_data, (char*)&data[i], sizeof(data[i]));
                    }
                }
                else {
                    if (isbitsunion) {
                        ios_write(s->const_data, (char*)jl_array_data(ar), datasize);
                        ios_write(s->const_data, jl_array_typetagdata(ar), alen);
                    }
                    else {
                        ios_write(s->const_data, (char*)jl_array_data(ar), tot);
                    }
                }
            }
            else {
                // Pointer eltypes are encoded in the mutable data section
                size_t data = LLT_ALIGN(ios_pos(s->s), alignment_amt);
                size_t padding_amt = data - ios_pos(s->s);
                headersize += padding_amt;
                newa->data = (void*)headersize; // relocation offset
                arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_array_t, data))); // relocation location
                arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item)); // relocation target
                write_padding(s->s, padding_amt);
                if (ar->flags.hasptr) {
                    // copy all of the data first
                    const char *data = (const char*)jl_array_data(ar);
                    ios_write(s->s, data, datasize);
                    // the rewrite all of the embedded pointers to null+relocation
                    uint16_t elsz = ar->elsize;
                    size_t j, np = ((jl_datatype_t*)et)->layout->npointers;
                    size_t i;
                    for (i = 0; i < alen; i++) {
                        for (j = 0; j < np; j++) {
                            size_t offset = i * elsz + jl_ptr_offset(((jl_datatype_t*)et), j) * sizeof(jl_value_t*);
                            jl_value_t *fld = get_replaceable_field((jl_value_t**)&data[offset], 1);
                            size_t fld_pos = reloc_offset + headersize + offset;
                            if (fld != NULL) {
                                arraylist_push(&s->relocs_list, (void*)(uintptr_t)fld_pos); // relocation location
                                arraylist_push(&s->relocs_list, (void*)backref_id(s, fld, s->link_ids_relocs)); // relocation target
                                record_uniquing(s, fld, fld_pos);
                            }
                            memset(&s->s->buf[fld_pos], 0, sizeof(fld)); // relocation offset (none)
                        }
                    }
                }
                else {
                    jl_value_t **data = (jl_value_t**)jl_array_data(ar);
                    size_t i;
                    for (i = 0; i < alen; i++) {
                        jl_value_t *e = get_replaceable_field(&data[i], 1);
                        write_pointerfield(s, e);
                    }
                }
            }
        }
        else if (jl_typeis(v, jl_module_type)) {
            jl_write_module(s, item, (jl_module_t*)v);
        }
        else if (jl_typeis(v, jl_task_type)) {
            jl_error("Task cannot be serialized");
        }
        else if (jl_is_svec(v)) {
            ios_write(s->s, (char*)v, sizeof(void*));
            size_t ii, l = jl_svec_len(v);
            assert(l > 0 || (jl_svec_t*)v == jl_emptysvec);
            for (ii = 0; ii < l; ii++) {
                write_pointerfield(s, jl_svecref(v, ii));
            }
        }
        else if (jl_is_string(v)) {
            ios_write(s->s, (char*)v, sizeof(void*) + jl_string_len(v));
            write_uint8(s->s, '\0'); // null-terminated strings for easier C-compatibility
        }
        else if (jl_is_foreign_type(t) == 1) {
            jl_error("Cannot serialize instances of foreign datatypes");
        }
        else if (jl_datatype_nfields(t) == 0) {
            // The object has no fields, so we just snapshot its byte representation
            assert(!t->layout->npointers);
            assert(t->layout->npointers == 0);
            ios_write(s->s, (char*)v, jl_datatype_size(t));
        }
        else if (jl_bigint_type && jl_typeis(v, jl_bigint_type)) {
            // foreign types require special handling
            jl_value_t *sizefield = jl_get_nth_field(v, 1);
            int32_t sz = jl_unbox_int32(sizefield);
            int32_t nw = (sz == 0 ? 1 : (sz < 0 ? -sz : sz));
            size_t nb = nw * gmp_limb_size;
            ios_write(s->s, (char*)&nw, sizeof(int32_t));
            ios_write(s->s, (char*)&sz, sizeof(int32_t));
            uintptr_t data = LLT_ALIGN(ios_pos(s->const_data), 8);
            write_padding(s->const_data, data - ios_pos(s->const_data));
            data /= sizeof(void*);
            assert(data < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "offset to constant data too large");
            arraylist_push(&s->relocs_list, (void*)(reloc_offset + 8)); // relocation location
            arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + data)); // relocation target
            void *pdata = jl_unbox_voidpointer(jl_get_nth_field(v, 2));
            ios_write(s->const_data, (char*)pdata, nb);
            write_pointer(s->s);
        }
        else {
            // Generic object::DataType serialization by field
            const char *data = (const char*)v;
            size_t i, nf = jl_datatype_nfields(t);
            size_t tot = 0;
            for (i = 0; i < nf; i++) {
                size_t offset = jl_field_offset(t, i);
                const char *slot = data + offset;
                write_padding(s->s, offset - tot);
                tot = offset;
                size_t fsz = jl_field_size(t, i);
                if (t->name->mutabl && jl_is_cpointer_type(jl_field_type(t, i)) && *(intptr_t*)slot != -1) {
                    // reset Ptr fields to C_NULL (but keep MAP_FAILED / INVALID_HANDLE)
                    assert(!jl_field_isptr(t, i));
                    write_pointer(s->s);
                }
                else if (fsz > 0) {
                    ios_write(s->s, slot, fsz);
                }
                tot += fsz;
            }

            size_t np = t->layout->npointers;
            for (i = 0; i < np; i++) {
                size_t offset = jl_ptr_offset(t, i) * sizeof(jl_value_t*);
                int mutabl = t->name->mutabl;
                if (jl_is_binding(v) && ((jl_binding_t*)v)->constp && i == 0) // value field depends on constp field
                    mutabl = 0;
                jl_value_t *fld = get_replaceable_field((jl_value_t**)&data[offset], mutabl);
                size_t fld_pos = offset + reloc_offset;
                if (fld != NULL) {
                    arraylist_push(&s->relocs_list, (void*)(uintptr_t)(fld_pos)); // relocation location
                    arraylist_push(&s->relocs_list, (void*)backref_id(s, fld, s->link_ids_relocs)); // relocation target
                    record_uniquing(s, fld, fld_pos);
                }
                memset(&s->s->buf[fld_pos], 0, sizeof(fld)); // relocation offset (none)
            }

            // A few objects need additional handling beyond the generic serialization above

            if (s->incremental && jl_typeis(v, jl_typemap_entry_type)) {
                jl_typemap_entry_t *newentry = (jl_typemap_entry_t*)&s->s->buf[reloc_offset];
                if (newentry->max_world == ~(size_t)0) {
                    if (newentry->min_world > 1) {
                        newentry->min_world = ~(size_t)0;
                        arraylist_push(&s->fixup_objs, (void*)reloc_offset);
                    }
                }
                else {
                    // garbage newentry - delete it :(
                    newentry->min_world = 1;
                    newentry->max_world = 0;
                }
            }
            else if (jl_is_method(v)) {
                write_padding(s->s, sizeof(jl_method_t) - tot); // hidden fields
                jl_method_t *m = (jl_method_t*)v;
                jl_method_t *newm = (jl_method_t*)&s->s->buf[reloc_offset];
                if (s->incremental) {
                    if (newm->deleted_world != ~(size_t)0)
                        newm->deleted_world = 1;
                    else
                        arraylist_push(&s->fixup_objs, (void*)reloc_offset);
                    newm->primary_world = ~(size_t)0;
                } else {
                    newm->nroots_sysimg = m->roots ? jl_array_len(m->roots) : 0;
                }
                if (m->ccallable)
                    arraylist_push(&s->ccallable_list, (void*)reloc_offset);
            }
            else if (jl_is_method_instance(v)) {
                jl_method_instance_t *newmi = (jl_method_instance_t*)&s->s->buf[reloc_offset];
                jl_atomic_store_relaxed(&newmi->precompiled, 0);
            }
            else if (jl_is_code_instance(v)) {
                // Handle the native-code pointers
                jl_code_instance_t *m = (jl_code_instance_t*)v;
                jl_code_instance_t *newm = (jl_code_instance_t*)&s->s->buf[reloc_offset];

                if (s->incremental) {
                    arraylist_push(&s->fixup_objs, (void*)reloc_offset);
                    if (m->min_world > 1)
                        newm->min_world = ~(size_t)0;     // checks that we reprocess this upon deserialization
                    if (m->max_world != ~(size_t)0)
                        newm->max_world = 0;
                    else {
                        if (m->inferred && ptrhash_has(&s->callers_with_edges, m->def))
                            newm->max_world = 1;  // sentinel value indicating this will need validation
                        if (m->min_world > 0 && m->inferred) {
                            // TODO: also check if this object is part of the codeinst cache
                            // will check on deserialize if this cache entry is still valid
                        }
                    }
                }

                newm->invoke = NULL;
                newm->specsigflags = 0;
                newm->specptr.fptr = NULL;
                int8_t fptr_id = JL_API_NULL;
                int8_t builtin_id = 0;
                if (m->invoke == jl_fptr_const_return) {
                    fptr_id = JL_API_CONST;
                }
                else {
                    if (jl_is_method(m->def->def.method)) {
                        builtin_id = jl_fptr_id(m->specptr.fptr);
                        if (builtin_id) { // found in the table of builtins
                            assert(builtin_id >= 2);
                            fptr_id = JL_API_BUILTIN;
                        }
                        else {
                            int32_t invokeptr_id = 0;
                            int32_t specfptr_id = 0;
                            jl_get_function_id(native_functions, m, &invokeptr_id, &specfptr_id); // see if we generated code for it
                            if (invokeptr_id) {
                                if (invokeptr_id == -1) {
                                    fptr_id = JL_API_BOXED;
                                }
                                else if (invokeptr_id == -2) {
                                    fptr_id = JL_API_WITH_PARAMETERS;
                                }
                                else {
                                    assert(invokeptr_id > 0);
                                    ios_ensureroom(s->fptr_record, invokeptr_id * sizeof(void*));
                                    ios_seek(s->fptr_record, (invokeptr_id - 1) * sizeof(void*));
                                    write_reloc_t(s->fptr_record, (reloc_t)~reloc_offset);
#ifdef _P64
                                    if (sizeof(reloc_t) < 8)
                                        write_padding(s->fptr_record, 8 - sizeof(reloc_t));
#endif
                                }
                                if (specfptr_id) {
                                    assert(specfptr_id > invokeptr_id && specfptr_id > 0);
                                    ios_ensureroom(s->fptr_record, specfptr_id * sizeof(void*));
                                    ios_seek(s->fptr_record, (specfptr_id - 1) * sizeof(void*));
                                    write_reloc_t(s->fptr_record, reloc_offset);
#ifdef _P64
                                    if (sizeof(reloc_t) < 8)
                                        write_padding(s->fptr_record, 8 - sizeof(reloc_t));
#endif
                                }
                            }
                        }
                    }
                }
                newm->invoke = NULL; // relocation offset
                if (fptr_id != JL_API_NULL) {
                    assert(fptr_id < BuiltinFunctionTag && "too many functions to serialize");
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_code_instance_t, invoke))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)FunctionRef << RELOC_TAG_OFFSET) + fptr_id)); // relocation target
                }
                if (builtin_id >= 2) {
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_code_instance_t, specptr.fptr))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)FunctionRef << RELOC_TAG_OFFSET) + BuiltinFunctionTag + builtin_id - 2)); // relocation target
                }
            }
            else if (jl_is_datatype(v)) {
                jl_datatype_t *dt = (jl_datatype_t*)v;
                jl_datatype_t *newdt = (jl_datatype_t*)&s->s->buf[reloc_offset];

                if (dt->layout != NULL) {
                    size_t nf = dt->layout->nfields;
                    size_t np = dt->layout->npointers;
                    size_t fieldsize = 0;
                    uint8_t is_foreign_type = dt->layout->fielddesc_type == 3;
                    if (!is_foreign_type) {
                        fieldsize = jl_fielddesc_size(dt->layout->fielddesc_type);
                    }
                    char *flddesc = (char*)dt->layout;
                    size_t fldsize = sizeof(jl_datatype_layout_t) + nf * fieldsize;
                    if (!is_foreign_type && dt->layout->first_ptr != -1)
                        fldsize += np << dt->layout->fielddesc_type;
                    uintptr_t layout = LLT_ALIGN(ios_pos(s->const_data), sizeof(void*));
                    write_padding(s->const_data, layout - ios_pos(s->const_data)); // realign stream
                    newdt->layout = NULL; // relocation offset
                    layout /= sizeof(void*);
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_datatype_t, layout))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + layout)); // relocation target
                    ios_write(s->const_data, flddesc, fldsize);
                    if (is_foreign_type) {
                        // make sure we have space for the extra hidden pointers
                        // zero them since they will need to be re-initialized externally
                        assert(fldsize == sizeof(jl_datatype_layout_t));
                        jl_fielddescdyn_t dyn = {0, 0};
                        ios_write(s->const_data, (char*)&dyn, sizeof(jl_fielddescdyn_t));
                    }
                }
            }
            else if (jl_is_typename(v)) {
                jl_typename_t *tn = (jl_typename_t*)v;
                jl_typename_t *newtn = (jl_typename_t*)&s->s->buf[reloc_offset];
                if (tn->atomicfields != NULL) {
                    size_t nb = (jl_svec_len(tn->names) + 31) / 32 * sizeof(uint32_t);
                    uintptr_t layout = LLT_ALIGN(ios_pos(s->const_data), sizeof(void*));
                    write_padding(s->const_data, layout - ios_pos(s->const_data)); // realign stream
                    newtn->atomicfields = NULL; // relocation offset
                    layout /= sizeof(void*);
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_typename_t, atomicfields))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + layout)); // relocation target
                    ios_write(s->const_data, (char*)tn->atomicfields, nb);
                }
                if (tn->constfields != NULL) {
                    size_t nb = (jl_svec_len(tn->names) + 31) / 32 * sizeof(uint32_t);
                    uintptr_t layout = LLT_ALIGN(ios_pos(s->const_data), sizeof(void*));
                    write_padding(s->const_data, layout - ios_pos(s->const_data)); // realign stream
                    newtn->constfields = NULL; // relocation offset
                    layout /= sizeof(void*);
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_typename_t, constfields))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + layout)); // relocation target
                    ios_write(s->const_data, (char*)tn->constfields, nb);
                }
            }
            else if (jl_is_globalref(v)) {
                jl_globalref_t *gr = (jl_globalref_t*)v;
                if (s->incremental && jl_object_in_image((jl_value_t*)gr->mod)) {
                    // will need to populate the binding field later
                    arraylist_push(&s->fixup_objs, (void*)reloc_offset);
                }
            }
            else if (((jl_datatype_t*)(jl_typeof(v)))->name == jl_idtable_typename) {
                // will need to rehash this, later (after types are fully constructed)
                arraylist_push(&s->fixup_objs, (void*)reloc_offset);
            }
            else {
                write_padding(s->s, jl_datatype_size(t) - tot);
            }
        }
    }
}

// In deserialization, create Symbols and set up the
// index for backreferencing
static void jl_read_symbols(jl_serializer_state *s)
{
    assert(deser_sym.len == 0);
    uintptr_t base = (uintptr_t)&s->symbols->buf[0];
    uintptr_t end = base + s->symbols->size;
    while (base < end) {
        uint32_t len = jl_load_unaligned_i32((void*)base);
        base += 4;
        const char *str = (const char*)base;
        base += len + 1;
        //printf("symbol %3d: %s\n", len, str);
        jl_sym_t *sym = _jl_symbol(str, len);
        arraylist_push(&deser_sym, (void*)sym);
    }
}


// In serialization, extract the appropriate serializer position for RefTags-encoded index `reloc_item`.
// Used for hard-coded tagged items, `relocs_list`, and `gctags_list`
static uintptr_t get_reloc_for_item(uintptr_t reloc_item, size_t reloc_offset)
{
    enum RefTags tag = (enum RefTags)(reloc_item >> RELOC_TAG_OFFSET);
    if (tag == DataRef) {
        // first serialized segment
        // need to compute the final relocation offset via the layout table
        assert(reloc_item < layout_table.len);
        uintptr_t reloc_base = (uintptr_t)layout_table.items[reloc_item];
        assert(reloc_base != 0 && "layout offset missing for relocation item");
        // write reloc_offset into s->s at pos
        return ((uintptr_t)tag << RELOC_TAG_OFFSET) + reloc_base + reloc_offset;
    }
    else {
        // just write the item reloc_id directly
#ifndef JL_NDEBUG
        assert(reloc_offset == 0 && "offsets for relocations to builtin objects should be precomposed in the reloc_item");
        size_t offset = (reloc_item & (((uintptr_t)1 << RELOC_TAG_OFFSET) - 1));
        switch (tag) {
        case ConstDataRef:
            break;
        case SymbolRef:
            assert(offset < nsym_tag && "corrupt relocation item id");
            break;
        case TagRef:
            assert(offset < 2 * NBOX_C + 258 && "corrupt relocation item id");
            break;
        case FunctionRef:
            if (offset & BuiltinFunctionTag) {
                offset &= ~BuiltinFunctionTag;
                assert(offset < sizeof(id_to_fptrs) / sizeof(*id_to_fptrs) && "unknown function pointer id");
            }
            else {
                assert(offset < JL_API_MAX && "unknown function pointer id");
            }
            break;
        case SysimageLinkage:
            break;
        case ExternalLinkage:
            break;
        default:
            assert(0 && "corrupt relocation item id");
            abort();
        }
#endif
        return reloc_item; // pre-composed relocation + offset
    }
}

// Compute target location at deserialization
static inline uintptr_t get_item_for_reloc(jl_serializer_state *s, uintptr_t base, size_t size, uintptr_t reloc_id, jl_array_t *link_ids, int *link_index) JL_NOTSAFEPOINT
{
    enum RefTags tag = (enum RefTags)(reloc_id >> RELOC_TAG_OFFSET);
    size_t offset = (reloc_id & (((uintptr_t)1 << RELOC_TAG_OFFSET) - 1));
    switch (tag) {
    case DataRef:
        assert(offset <= size);
        return base + offset;
    case ConstDataRef:
        return (uintptr_t)s->const_data->buf + (offset * sizeof(void*));
    case SymbolRef:
        assert(offset < deser_sym.len && deser_sym.items[offset] && "corrupt relocation item id");
        return (uintptr_t)deser_sym.items[offset];
    case TagRef:
        if (offset == 0)
            return (uintptr_t)s->ptls->root_task;
        if (offset == 1)
            return (uintptr_t)jl_nothing;
        offset -= 2;
        if (offset < NBOX_C)
            return (uintptr_t)jl_box_int64((int64_t)offset - NBOX_C / 2);
        offset -= NBOX_C;
        if (offset < NBOX_C)
            return (uintptr_t)jl_box_int32((int32_t)offset - NBOX_C / 2);
        offset -= NBOX_C;
        if (offset < 256)
            return (uintptr_t)jl_box_uint8(offset);
        // offset -= 256;
        assert(0 && "corrupt relocation item id");
        jl_unreachable(); // terminate control flow if assertion is disabled.
    case FunctionRef:
        if (offset & BuiltinFunctionTag) {
            offset &= ~BuiltinFunctionTag;
            assert(offset < sizeof(id_to_fptrs) / sizeof(*id_to_fptrs) && "unknown function pointer ID");
            return (uintptr_t)id_to_fptrs[offset];
        }
        switch ((jl_callingconv_t)offset) {
        case JL_API_BOXED:
            if (s->image->fptrs.base)
                return (uintptr_t)jl_fptr_args;
            JL_FALLTHROUGH;
        case JL_API_WITH_PARAMETERS:
            if (s->image->fptrs.base)
                return (uintptr_t)jl_fptr_sparam;
            return (uintptr_t)NULL;
        case JL_API_CONST:
            return (uintptr_t)jl_fptr_const_return;
        case JL_API_INTERPRETED:
            return (uintptr_t)jl_fptr_interpret_call;
        case JL_API_BUILTIN:
            return (uintptr_t)jl_fptr_args;
        case JL_API_NULL:
        case JL_API_MAX:
        //default:
            assert("corrupt relocation item id");
        }
    case SysimageLinkage: {
#ifdef _P64
        size_t depsidx = offset >> DEPS_IDX_OFFSET;
        offset &= ((size_t)1 << DEPS_IDX_OFFSET) - 1;
#else
        size_t depsidx = 0;
#endif
        assert(depsidx < jl_array_len(s->buildid_depmods_idxs));
        size_t i = ((uint32_t*)jl_array_data(s->buildid_depmods_idxs))[depsidx];
        assert(2*i < jl_linkage_blobs.len);
        return (uintptr_t)jl_linkage_blobs.items[2*i] + offset*sizeof(void*);
    }
    case ExternalLinkage: {
        assert(link_ids);
        assert(link_index);
        assert(0 <= *link_index && *link_index < jl_array_len(link_ids));
        uint32_t depsidx = ((uint32_t*)jl_array_data(link_ids))[*link_index];
        *link_index += 1;
        assert(depsidx < jl_array_len(s->buildid_depmods_idxs));
        size_t i = ((uint32_t*)jl_array_data(s->buildid_depmods_idxs))[depsidx];
        assert(2*i < jl_linkage_blobs.len);
        return (uintptr_t)jl_linkage_blobs.items[2*i] + offset*sizeof(void*);
    }
    }
    abort();
}


static void jl_write_offsetlist(ios_t *s, char *base, size_t size, arraylist_t *list)
{
    for (size_t i = 0; i < list->len; i += 2) {
        size_t last_pos = i ? (size_t)list->items[i - 2] : 0;
        size_t pos = (size_t)list->items[i];
        size_t item = (size_t)list->items[i + 1];   // item is tagref-encoded
        uintptr_t *pv = (uintptr_t*)(base + pos);
        assert(pos < size && pos != 0);
        *pv = get_reloc_for_item(item, *pv);

        // write pos as compressed difference.
        size_t pos_diff = pos - last_pos;
        while (pos_diff) {
            assert(pos_diff >= 0);
            if (pos_diff <= 127) {
                write_int8(s, pos_diff);
                break;
            }
            else {
                // Extract the next 7 bits
                int8_t ns = pos_diff & (int8_t)0x7F;
                pos_diff >>= 7;
                // Set the high bit if there's still more
                ns |= (!!pos_diff) << 7;
                write_int8(s, ns);
            }
        }
    }
    write_int8(s, 0);
}


static void jl_write_arraylist(ios_t *s, arraylist_t *list)
{
    write_uint(s, list->len);
    ios_write(s, (const char*)list->items, list->len * sizeof(void*));
}

static void jl_write_relocations(jl_serializer_state *s)
{
    char *base = &s->s->buf[0];
    jl_write_offsetlist(s->relocs, base, s->s->size, &s->gctags_list);
    jl_write_offsetlist(s->relocs, base, s->s->size, &s->relocs_list);
    if (s->incremental) {
        jl_write_arraylist(s->relocs, &s->uniquing_types);
        jl_write_arraylist(s->relocs, &s->uniquing_objs);
        jl_write_arraylist(s->relocs, &s->fixup_types);
    }
    jl_write_arraylist(s->relocs, &s->fixup_objs);
}

static void jl_read_reloclist(jl_serializer_state *s, jl_array_t *link_ids, uint8_t bits)
{
    uintptr_t base = (uintptr_t)s->s->buf;
    size_t size = s->s->size;
    uintptr_t last_pos = 0;
    uint8_t *current = (uint8_t *)(s->relocs->buf + s->relocs->bpos);
    int link_index = 0;
    while (1) {
        // Read the offset of the next object
        size_t pos_diff = 0;
        size_t cnt = 0;
        while (1) {
            assert(s->relocs->bpos <= s->relocs->size);
            assert((char *)current <= (char *)(s->relocs->buf + s->relocs->size));
            int8_t c = *current++;
            s->relocs->bpos += 1;

            pos_diff |= ((size_t)c & 0x7F) << (7 * cnt++);
            if ((c >> 7) == 0)
                break;
        }
        if (pos_diff == 0)
            break;

        uintptr_t pos = last_pos + pos_diff;
        last_pos = pos;
        uintptr_t *pv = (uintptr_t *)(base + pos);
        uintptr_t v = *pv;
        v = get_item_for_reloc(s, base, size, v, link_ids, &link_index);
        *pv = v | bits;
    }
    assert(!link_ids || link_index == jl_array_len(link_ids));
}

static void jl_read_arraylist(ios_t *s, arraylist_t *list)
{
    size_t list_len = read_uint(s);
    arraylist_new(list, 0);
    arraylist_grow(list, list_len);
    ios_read(s, (char*)list->items, list_len * sizeof(void*));
}

void gc_sweep_sysimg(void)
{
    size_t nblobs = n_linkage_blobs();
    if (nblobs == 0)
        return;
    assert(jl_linkage_blobs.len == 2*nblobs);
    assert(jl_image_relocs.len == nblobs);
    for (size_t i = 0; i < 2*nblobs; i+=2) {
        reloc_t *relocs = (reloc_t*)jl_image_relocs.items[i>>1];
        if (!relocs)
            continue;
        uintptr_t base = (uintptr_t)jl_linkage_blobs.items[i];
        uintptr_t last_pos = 0;
        uint8_t *current = (uint8_t *)relocs;
        while (1) {
            // Read the offset of the next object
            size_t pos_diff = 0;
            size_t cnt = 0;
            while (1) {
                int8_t c = *current++;
                pos_diff |= ((size_t)c & 0x7F) << (7 * cnt++);
                if ((c >> 7) == 0)
                    break;
            }
            if (pos_diff == 0)
                break;

            uintptr_t pos = last_pos + pos_diff;
            last_pos = pos;
            jl_taggedvalue_t *o = (jl_taggedvalue_t *)(base + pos);
            o->bits.gc = GC_OLD;
        }
    }
}

// jl_write_value and jl_read_value are used for storing Julia objects that are adjuncts to
// the image proper. For example, new methods added to external callables require
// insertion into the appropriate method table.
#define jl_write_value(s, v) _jl_write_value((s), (jl_value_t*)(v))
static void _jl_write_value(jl_serializer_state *s, jl_value_t *v)
{
    if (v == NULL) {
        write_reloc_t(s->s, 0);
        return;
    }
    uintptr_t item = backref_id(s, v, NULL);
    uintptr_t reloc = get_reloc_for_item(item, 0);
    write_reloc_t(s->s, reloc);
}

static jl_value_t *jl_read_value(jl_serializer_state *s)
{
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    size_t size = s->s->size;
    uintptr_t offset = *(reloc_t*)(base + (uintptr_t)s->s->bpos);
    s->s->bpos += sizeof(reloc_t);
    if (offset == 0)
        return NULL;
    return (jl_value_t*)get_item_for_reloc(s, base, size, offset, NULL, NULL);
}

// The next two, `jl_read_offset` and `jl_delayed_reloc`, are essentially a split version
// of `jl_read_value` that allows usage of the relocation data rather than passing NULL
// to `get_item_for_reloc`.
// This works around what would otherwise be an order-dependency conundrum: objects
// that may require relocation data have to be inserted into `serialization_order`,
// and that may include some of the adjunct data that gets serialized via
// `jl_write_value`. But we can't interpret them properly until we read the relocation
// data, and that happens after we pull items out of the serialization stream.
static uintptr_t jl_read_offset(jl_serializer_state *s)
{
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    uintptr_t offset = *(reloc_t*)(base + (uintptr_t)s->s->bpos);
    s->s->bpos += sizeof(reloc_t);
    return offset;
}

static jl_value_t *jl_delayed_reloc(jl_serializer_state *s, uintptr_t offset) JL_GC_DISABLED
{
    if (!offset)
        return NULL;
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    size_t size = s->s->size;
    int link_index = 0;
    jl_value_t *ret = (jl_value_t*)get_item_for_reloc(s, base, size, offset, s->link_ids_relocs, &link_index);
    assert(!s->link_ids_relocs || link_index < jl_array_len(s->link_ids_relocs));
    return ret;
}


static void jl_update_all_fptrs(jl_serializer_state *s, jl_image_t *image)
{
    jl_image_fptrs_t fvars = image->fptrs;
    // make these NULL now so we skip trying to restore GlobalVariable pointers later
    image->gvars_base = NULL;
    image->fptrs.base = NULL;
    if (fvars.base == NULL)
        return;
    int img_fvars_max = s->fptr_record->size / sizeof(void*);
    size_t i;
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    // These will become MethodInstance references, but they start out as a list of
    // offsets into `s` for CodeInstances
    jl_method_instance_t **linfos = (jl_method_instance_t**)&s->fptr_record->buf[0];
    uint32_t clone_idx = 0;
    for (i = 0; i < img_fvars_max; i++) {
        reloc_t offset = *(reloc_t*)&linfos[i];
        linfos[i] = NULL;
        if (offset != 0) {
            int specfunc = 1;
            if (offset & ((uintptr_t)1 << (8 * sizeof(reloc_t) - 1))) {
                // if high bit is set, this is the func wrapper, not the specfunc
                specfunc = 0;
                offset = ~offset;
            }
            jl_code_instance_t *codeinst = (jl_code_instance_t*)(base + offset);
            uintptr_t base = (uintptr_t)fvars.base;
            assert(jl_is_method(codeinst->def->def.method) && codeinst->invoke != jl_fptr_const_return);
            assert(specfunc ? codeinst->invoke != NULL : codeinst->invoke == NULL);
            linfos[i] = codeinst->def;     // now it's a MethodInstance
            int32_t offset = fvars.offsets[i];
            for (; clone_idx < fvars.nclones; clone_idx++) {
                uint32_t idx = fvars.clone_idxs[clone_idx] & jl_sysimg_val_mask;
                if (idx < i)
                    continue;
                if (idx == i)
                    offset = fvars.clone_offsets[clone_idx];
                break;
            }
            void *fptr = (void*)(base + offset);
            if (specfunc) {
                codeinst->specptr.fptr = fptr;
                codeinst->specsigflags = 0b111; // TODO: set only if confirmed to be true
            }
            else {
                codeinst->invoke = (jl_callptr_t)fptr;
            }
        }
    }
    // Tell LLVM about the native code
    jl_register_fptrs(image->base, &fvars, linfos, img_fvars_max);
}

static uint32_t write_gvars(jl_serializer_state *s, arraylist_t *globals, arraylist_t *external_fns) JL_NOTSAFEPOINT
{
    size_t len = globals->len + external_fns->len;
    ios_ensureroom(s->gvar_record, len * sizeof(reloc_t));
    for (size_t i = 0; i < globals->len; i++) {
        void *g = globals->items[i];
        uintptr_t item = backref_id(s, g, s->link_ids_gvars);
        uintptr_t reloc = get_reloc_for_item(item, 0);
        write_reloc_t(s->gvar_record, reloc);
        record_uniquing(s, (jl_value_t*)g, ((i << 2) | 2)); // mark as gvar && !tag
    }
    for (size_t i = 0; i < external_fns->len; i++) {
        jl_code_instance_t *ci = (jl_code_instance_t*)external_fns->items[i];
        assert(ci && (jl_atomic_load_relaxed(&ci->specsigflags) & 0b001));
        uintptr_t item = backref_id(s, (void*)ci, s->link_ids_external_fnvars);
        uintptr_t reloc = get_reloc_for_item(item, 0);
        write_reloc_t(s->gvar_record, reloc);
    }
    return globals->len;
}

// Pointer relocation for native-code referenced global variables
static void jl_update_all_gvars(jl_serializer_state *s, jl_image_t *image, uint32_t external_fns_begin)
{
    if (image->gvars_base == NULL)
        return;
    size_t i = 0;
    size_t l = s->gvar_record->size / sizeof(reloc_t);
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    size_t size = s->s->size;
    reloc_t *gvars = (reloc_t*)&s->gvar_record->buf[0];
    int gvar_link_index = 0;
    int external_fns_link_index = 0;
    assert(l == image->ngvars);
    for (i = 0; i < l; i++) {
        uintptr_t offset = gvars[i];
        uintptr_t v = 0;
        if (i < external_fns_begin) {
            v = get_item_for_reloc(s, base, size, offset, s->link_ids_gvars, &gvar_link_index);
        }
        else {
            v = get_item_for_reloc(s, base, size, offset, s->link_ids_external_fnvars, &external_fns_link_index);
        }
        uintptr_t *gv = sysimg_gvars(image->gvars_base, image->gvars_offsets, i);
        *gv = v;
    }
    assert(!s->link_ids_gvars || gvar_link_index == jl_array_len(s->link_ids_gvars));
    assert(!s->link_ids_external_fnvars || external_fns_link_index == jl_array_len(s->link_ids_external_fnvars));
}

static void jl_root_new_gvars(jl_serializer_state *s, jl_image_t *image, uint32_t external_fns_begin)
{
    if (image->gvars_base == NULL)
        return;
    size_t i = 0;
    size_t l = s->gvar_record->size / sizeof(reloc_t);
    for (i = 0; i < l; i++) {
        uintptr_t *gv = sysimg_gvars(image->gvars_base, image->gvars_offsets, i);
        uintptr_t v = *gv;
        if (i < external_fns_begin) {
            if (!jl_is_binding(v))
                v = (uintptr_t)jl_as_global_root((jl_value_t*)v);
        } else {
            jl_code_instance_t *codeinst = (jl_code_instance_t*) v;
            assert(codeinst && (codeinst->specsigflags & 0b01) && codeinst->specptr.fptr);
            v = (uintptr_t)codeinst->specptr.fptr;
        }
        *gv = v;
    }
}


static void jl_compile_extern(jl_method_t *m, void *sysimg_handle) JL_GC_DISABLED
{
    // install ccallable entry point in JIT
    jl_svec_t *sv = m->ccallable;
    int success = jl_compile_extern_c(NULL, NULL, sysimg_handle, jl_svecref(sv, 0), jl_svecref(sv, 1));
    if (!success)
        jl_safe_printf("WARNING: @ccallable was already defined for this method name\n"); // enjoy a very bad time
    assert(success || !sysimg_handle);
}


static void jl_reinit_ccallable(arraylist_t *ccallable_list, char *base, void *sysimg_handle)
{
    for (size_t i = 0; i < ccallable_list->len; i++) {
        uintptr_t item = (uintptr_t)ccallable_list->items[i];
        jl_method_t *m = (jl_method_t*)(base + item);
        jl_compile_extern(m, sysimg_handle);
    }
}


// Code below helps slim down the images by
// removing cached types not referenced in the stream
static jl_svec_t *jl_prune_type_cache_hash(jl_svec_t *cache) JL_GC_DISABLED
{
    size_t l = jl_svec_len(cache), i;
    if (l == 0)
        return cache;
    for (i = 0; i < l; i++) {
        jl_value_t *ti = jl_svecref(cache, i);
        if (ti == jl_nothing)
            continue;
        if (ptrhash_get(&serialization_order, ti) == HT_NOTFOUND)
            jl_svecset(cache, i, jl_nothing);
    }
    void *idx = ptrhash_get(&serialization_order, cache);
    assert(idx != HT_NOTFOUND && idx != (void*)(uintptr_t)-1);
    assert(serialization_queue.items[(char*)idx - 1 - (char*)HT_NOTFOUND] == cache);
    cache = cache_rehash_set(cache, l);
    // redirect all references to the old cache to relocate to the new cache object
    ptrhash_put(&serialization_order, cache, idx);
    serialization_queue.items[(char*)idx - 1 - (char*)HT_NOTFOUND] = cache;
    return cache;
}

static void jl_prune_type_cache_linear(jl_svec_t *cache)
{
    size_t l = jl_svec_len(cache), ins = 0, i;
    for (i = 0; i < l; i++) {
        jl_value_t *ti = jl_svecref(cache, i);
        if (ti == jl_nothing)
            break;
        if (ptrhash_get(&serialization_order, ti) != HT_NOTFOUND)
            jl_svecset(cache, ins++, ti);
    }
    while (ins < l)
        jl_svecset(cache, ins++, jl_nothing);
}

static jl_value_t *strip_codeinfo_meta(jl_method_t *m, jl_value_t *ci_, int orig)
{
    jl_code_info_t *ci = NULL;
    JL_GC_PUSH1(&ci);
    int compressed = 0;
    if (!jl_is_code_info(ci_)) {
        compressed = 1;
        ci = jl_uncompress_ir(m, NULL, (jl_array_t*)ci_);
    }
    else {
        ci = (jl_code_info_t*)ci_;
    }
    // leave codelocs length the same so the compiler can assume that; just zero it
    memset(jl_array_data(ci->codelocs), 0, jl_array_len(ci->codelocs)*sizeof(int32_t));
    // empty linetable
    if (jl_is_array(ci->linetable))
        jl_array_del_end((jl_array_t*)ci->linetable, jl_array_len(ci->linetable));
    // replace slot names with `?`, except unused_sym since the compiler looks at it
    jl_sym_t *questionsym = jl_symbol("?");
    int i, l = jl_array_len(ci->slotnames);
    for (i = 0; i < l; i++) {
        jl_value_t *s = jl_array_ptr_ref(ci->slotnames, i);
        if (s != (jl_value_t*)jl_unused_sym)
            jl_array_ptr_set(ci->slotnames, i, questionsym);
    }
    if (orig) {
        m->slot_syms = jl_compress_argnames(ci->slotnames);
        jl_gc_wb(m, m->slot_syms);
    }
    jl_value_t *ret = (jl_value_t*)ci;
    if (compressed)
        ret = (jl_value_t*)jl_compress_ir(m, ci);
    JL_GC_POP();
    return ret;
}

static void strip_specializations_(jl_method_instance_t *mi)
{
    assert(jl_is_method_instance(mi));
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        jl_value_t *inferred = jl_atomic_load_relaxed(&codeinst->inferred);
        if (inferred && inferred != jl_nothing) {
            if (jl_options.strip_ir) {
                record_field_change((jl_value_t**)&codeinst->inferred, jl_nothing);
            }
            else if (jl_options.strip_metadata) {
                jl_value_t *stripped = strip_codeinfo_meta(mi->def.method, inferred, 0);
                if (jl_atomic_cmpswap_relaxed(&codeinst->inferred, &inferred, stripped)) {
                    jl_gc_wb(codeinst, stripped);
                }
            }
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    if (jl_options.strip_ir) {
        record_field_change((jl_value_t**)&mi->uninferred, NULL);
        record_field_change((jl_value_t**)&mi->backedges, NULL);
        record_field_change((jl_value_t**)&mi->callbacks, NULL);
    }
}

static int strip_all_codeinfos__(jl_typemap_entry_t *def, void *_env)
{
    jl_method_t *m = def->func.method;
    if (m->source) {
        int stripped_ir = 0;
        if (jl_options.strip_ir) {
            if (m->unspecialized) {
                jl_code_instance_t *unspec = jl_atomic_load_relaxed(&m->unspecialized->cache);
                if (unspec && jl_atomic_load_relaxed(&unspec->invoke)) {
                    // we have a generic compiled version, so can remove the IR
                    record_field_change(&m->source, jl_nothing);
                    stripped_ir = 1;
                }
            }
            if (!stripped_ir) {
                int mod_setting = jl_get_module_compile(m->module);
                // if the method is declared not to be compiled, keep IR for interpreter
                if (!(mod_setting == JL_OPTIONS_COMPILE_OFF || mod_setting == JL_OPTIONS_COMPILE_MIN)) {
                    record_field_change(&m->source, jl_nothing);
                    stripped_ir = 1;
                }
            }
        }
        if (jl_options.strip_metadata && !stripped_ir) {
            m->source = strip_codeinfo_meta(m, m->source, 1);
            jl_gc_wb(m, m->source);
        }
    }
    jl_svec_t *specializations = m->specializations;
    size_t i, l = jl_svec_len(specializations);
    for (i = 0; i < l; i++) {
        jl_value_t *mi = jl_svecref(specializations, i);
        if (mi != jl_nothing)
            strip_specializations_((jl_method_instance_t*)mi);
    }
    if (m->unspecialized)
        strip_specializations_(m->unspecialized);
    if (jl_options.strip_ir && m->root_blocks)
        record_field_change((jl_value_t**)&m->root_blocks, NULL);
    return 1;
}

static int strip_all_codeinfos_(jl_methtable_t *mt, void *_env)
{
    if (jl_options.strip_ir && mt->backedges)
        record_field_change((jl_value_t**)&mt->backedges, NULL);
    return jl_typemap_visitor(mt->defs, strip_all_codeinfos__, NULL);
}

static void jl_strip_all_codeinfos(void)
{
    jl_foreach_reachable_mtable(strip_all_codeinfos_, NULL);
}

// --- entry points ---

jl_array_t *jl_global_roots_table;
static jl_mutex_t global_roots_lock;

JL_DLLEXPORT int jl_is_globally_rooted(jl_value_t *val JL_MAYBE_UNROOTED) JL_NOTSAFEPOINT
{
    if (jl_is_concrete_type(val) || jl_is_bool(val) || jl_is_symbol(val) ||
            val == (jl_value_t*)jl_any_type || val == (jl_value_t*)jl_bottom_type || val == (jl_value_t*)jl_core_module)
        return 1;
    if (val == ((jl_datatype_t*)jl_typeof(val))->instance)
        return 1;
    return 0;
}

JL_DLLEXPORT jl_value_t *jl_as_global_root(jl_value_t *val JL_MAYBE_UNROOTED)
{
    if (jl_is_globally_rooted(val))
        return val;
    if (jl_is_uint8(val))
        return jl_box_uint8(jl_unbox_uint8(val));
    if (jl_is_int32(val)) {
        int32_t n = jl_unbox_int32(val);
        if ((uint32_t)(n+512) < 1024)
            return jl_box_int32(n);
    }
    else if (jl_is_int64(val)) {
        uint64_t n = jl_unbox_uint64(val);
        if ((uint64_t)(n+512) < 1024)
            return jl_box_int64(n);
    }
    JL_GC_PUSH1(&val);
    JL_LOCK(&global_roots_lock);
    jl_value_t *rval = jl_eqtable_getkey(jl_global_roots_table, val, NULL);
    if (rval) {
        val = rval;
    }
    else {
        jl_global_roots_table = jl_eqtable_put(jl_global_roots_table, val, jl_nothing, NULL);
    }
    JL_UNLOCK(&global_roots_lock);
    JL_GC_POP();
    return val;
}

static void jl_prepare_serialization_data(jl_array_t *mod_array, jl_array_t *newly_inferred, uint64_t worklist_key,
                           /* outputs */  jl_array_t **extext_methods, jl_array_t **new_specializations,
                                          jl_array_t **method_roots_list, jl_array_t **ext_targets, jl_array_t **edges)
{
    // extext_methods: [method1, ...], worklist-owned "extending external" methods added to functions owned by modules outside the worklist
    // ext_targets: [invokesig1, callee1, matches1, ...] non-worklist callees of worklist-owned methods
    //              ordinary dispatch: invokesig=NULL, callee is MethodInstance
    //              `invoke` dispatch: invokesig is signature, callee is MethodInstance
    //              abstract call: callee is signature
    // edges: [caller1, ext_targets_indexes1, ...] for worklist-owned methods calling external methods
    assert(edges_map == NULL);

    // Save the inferred code from newly inferred, external methods
    *new_specializations = queue_external_cis(newly_inferred);

    // Collect method extensions and edges data
    JL_GC_PUSH1(&edges_map);
    if (edges)
        edges_map = jl_alloc_vec_any(0);
    *extext_methods = jl_alloc_vec_any(0);
    jl_collect_methtable_from_mod(jl_type_type_mt, *extext_methods);
    jl_collect_methtable_from_mod(jl_nonfunction_mt, *extext_methods);
    size_t i, len = jl_array_len(mod_array);
    for (i = 0; i < len; i++) {
        jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(mod_array, i);
        assert(jl_is_module(m));
        if (m->parent == m) // some toplevel modules (really just Base) aren't actually
            jl_collect_extext_methods_from_mod(*extext_methods, m);
    }

    if (edges) {
        size_t world = jl_atomic_load_acquire(&jl_world_counter);
        jl_collect_missing_backedges(jl_type_type_mt);
        jl_collect_missing_backedges(jl_nonfunction_mt);
        // jl_collect_extext_methods_from_mod and jl_collect_missing_backedges also accumulate data in callers_with_edges.
        // Process this to extract `edges` and `ext_targets`.
        *ext_targets = jl_alloc_vec_any(0);
        *edges = jl_alloc_vec_any(0);
        *method_roots_list = jl_alloc_vec_any(0);
        // Collect the new method roots
        jl_collect_new_roots(*method_roots_list, *new_specializations, worklist_key);
        jl_collect_edges(*edges, *ext_targets, *new_specializations, world);
    }
    assert(edges_map == NULL); // jl_collect_edges clears this when done

    JL_GC_POP();
}

// In addition to the system image (where `worklist = NULL`), this can also save incremental images with external linkage
static void jl_save_system_image_to_stream(ios_t *f, jl_array_t *mod_array,
                                           jl_array_t *worklist, jl_array_t *extext_methods,
                                           jl_array_t *new_specializations, jl_array_t *method_roots_list,
                                           jl_array_t *ext_targets, jl_array_t *edges) JL_GC_DISABLED
{
    htable_new(&field_replace, 0);
    // strip metadata and IR when requested
    if (jl_options.strip_metadata || jl_options.strip_ir)
        jl_strip_all_codeinfos();

    int en = jl_gc_enable(0);
    nsym_tag = 0;
    htable_new(&symbol_table, 0);
    htable_new(&fptr_to_id, sizeof(id_to_fptrs) / sizeof(*id_to_fptrs));
    uintptr_t i;
    for (i = 0; id_to_fptrs[i] != NULL; i++) {
        ptrhash_put(&fptr_to_id, (void*)(uintptr_t)id_to_fptrs[i], (void*)(i + 2));
    }
    htable_new(&serialization_order, 25000);
    htable_new(&unique_ready, 0);
    htable_new(&nullptrs, 0);
    arraylist_new(&object_worklist, 0);
    arraylist_new(&serialization_queue, 0);
    ios_t sysimg, const_data, symbols, relocs, gvar_record, fptr_record;
    ios_mem(&sysimg, 0);
    ios_mem(&const_data, 0);
    ios_mem(&symbols, 0);
    ios_mem(&relocs, 0);
    ios_mem(&gvar_record, 0);
    ios_mem(&fptr_record, 0);
    jl_serializer_state s;
    s.incremental = !(worklist == NULL);
    s.s = &sysimg;
    s.const_data = &const_data;
    s.symbols = &symbols;
    s.relocs = &relocs;
    s.gvar_record = &gvar_record;
    s.fptr_record = &fptr_record;
    s.ptls = jl_current_task->ptls;
    arraylist_new(&s.relocs_list, 0);
    arraylist_new(&s.gctags_list, 0);
    arraylist_new(&s.uniquing_types, 0);
    arraylist_new(&s.uniquing_objs, 0);
    arraylist_new(&s.fixup_types, 0);
    arraylist_new(&s.fixup_objs, 0);
    arraylist_new(&s.ccallable_list, 0);
    s.buildid_depmods_idxs = image_to_depmodidx(mod_array);
    s.link_ids_relocs = jl_alloc_array_1d(jl_array_int32_type, 0);
    s.link_ids_gctags = jl_alloc_array_1d(jl_array_int32_type, 0);
    s.link_ids_gvars = jl_alloc_array_1d(jl_array_int32_type, 0);
    s.link_ids_external_fnvars = jl_alloc_array_1d(jl_array_int32_type, 0);
    htable_new(&s.callers_with_edges, 0);
    jl_value_t **const*const tags = get_tags(); // worklist == NULL ? get_tags() : NULL;

    arraylist_t gvars;
    arraylist_t external_fns;
    arraylist_new(&gvars, 0);
    arraylist_new(&external_fns, 0);
    if (native_functions) {
        jl_get_llvm_gvs(native_functions, &gvars);
        jl_get_llvm_external_fns(native_functions, &external_fns);
    }

    if (worklist == NULL) {
        // empty!(Core.ARGS)
        if (jl_core_module != NULL) {
            jl_array_t *args = (jl_array_t*)jl_get_global(jl_core_module, jl_symbol("ARGS"));
            if (args != NULL) {
                jl_array_del_end(args, jl_array_len(args));
            }
        }
    }
    jl_idtable_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("IdDict")) : NULL;
    jl_idtable_typename = jl_base_module ? ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_idtable_type))->name : NULL;
    jl_bigint_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("BigInt")) : NULL;
    if (jl_bigint_type) {
        gmp_limb_size = jl_unbox_long(jl_get_global((jl_module_t*)jl_get_global(jl_base_module, jl_symbol("GMP")),
                                                    jl_symbol("BITS_PER_LIMB"))) / 8;
    }
    if (jl_base_module) {
        jl_value_t *docs = jl_get_global(jl_base_module, jl_symbol("Docs"));
        if (docs && jl_is_module(docs)) {
            jl_docmeta_sym = (jl_sym_t*)jl_get_global((jl_module_t*)docs, jl_symbol("META"));
        }
    }

    { // step 1: record values (recursively) that need to go in the image
        size_t i;
        if (worklist == NULL) {
            for (i = 0; tags[i] != NULL; i++) {
                jl_value_t *tag = *tags[i];
                jl_queue_for_serialization(&s, tag);
            }
            jl_queue_for_serialization(&s, jl_global_roots_table);
            jl_queue_for_serialization(&s, s.ptls->root_task->tls);
        }
        else {
            // To ensure we don't have to manually update the list, go through all tags and queue any that are not otherwise
            // judged to be externally-linked
            htable_new(&external_objects, NUM_TAGS);
            for (size_t i = 0; tags[i] != NULL; i++) {
                jl_value_t *tag = *tags[i];
                ptrhash_put(&external_objects, tag, tag);
            }
            // Queue the worklist itself as the first item we serialize
            jl_queue_for_serialization(&s, worklist);
            jl_queue_for_serialization(&s, jl_module_init_order);
            // Classify the CodeInstances with respect to their need for validation
            classify_callers(&s.callers_with_edges, edges);
        }
        // step 1.1: as needed, serialize the data needed for insertion into the running system
        if (extext_methods) {
            assert(ext_targets);
            assert(edges);
            // Queue method extensions
            jl_queue_for_serialization(&s, extext_methods);
            // Queue the new specializations
            jl_queue_for_serialization(&s, new_specializations);
            // Queue the new roots
            jl_queue_for_serialization(&s, method_roots_list);
            // Queue the edges
            jl_queue_for_serialization(&s, ext_targets);
            jl_queue_for_serialization(&s, edges);
        }
        jl_serialize_reachable(&s);
        // step 1.2: ensure all gvars are part of the sysimage too
        record_gvars(&s, &gvars);
        record_external_fns(&s, &external_fns);
        jl_serialize_reachable(&s);
        // step 1.3: prune (garbage collect) some special weak references from
        // built-in type caches
        for (i = 0; i < serialization_queue.len; i++) {
            jl_typename_t *tn = (jl_typename_t*)serialization_queue.items[i];
            if (jl_is_typename(tn)) {
                tn->cache = jl_prune_type_cache_hash(tn->cache);
                jl_gc_wb(tn, tn->cache);
                jl_prune_type_cache_linear(tn->linearcache);
            }
        }
    }

    uint32_t external_fns_begin = 0;
    { // step 2: build all the sysimg sections
        write_padding(&sysimg, sizeof(uintptr_t));
        jl_write_values(&s);
        external_fns_begin = write_gvars(&s, &gvars, &external_fns);
        jl_write_relocations(&s);
    }

    if (sysimg.size > ((uintptr_t)1 << RELOC_TAG_OFFSET)) {
        jl_printf(
            JL_STDERR,
            "ERROR: system image too large: sysimg.size is %jd but the limit is %" PRIxPTR "\n",
            (intmax_t)sysimg.size,
            ((uintptr_t)1 << RELOC_TAG_OFFSET)
        );
        jl_exit(1);
    }
    if (const_data.size / sizeof(void*) > ((uintptr_t)1 << RELOC_TAG_OFFSET)) {
        jl_printf(
            JL_STDERR,
            "ERROR: system image too large: const_data.size is %jd but the limit is %" PRIxPTR "\n",
            (intmax_t)const_data.size,
            ((uintptr_t)1 << RELOC_TAG_OFFSET)*sizeof(void*)
        );
        jl_exit(1);
    }
    htable_free(&s.callers_with_edges);

    // step 3: combine all of the sections into one file
    assert(ios_pos(f) % JL_CACHE_BYTE_ALIGNMENT == 0);
    write_uint(f, sysimg.size - sizeof(uintptr_t));
    ios_seek(&sysimg, sizeof(uintptr_t));
    ios_copyall(f, &sysimg);
    ios_close(&sysimg);

    write_uint(f, const_data.size);
    // realign stream to max-alignment for data
    write_padding(f, LLT_ALIGN(ios_pos(f), JL_CACHE_BYTE_ALIGNMENT) - ios_pos(f));
    ios_seek(&const_data, 0);
    ios_copyall(f, &const_data);
    ios_close(&const_data);

    write_uint(f, symbols.size);
    write_padding(f, LLT_ALIGN(ios_pos(f), 8) - ios_pos(f));
    ios_seek(&symbols, 0);
    ios_copyall(f, &symbols);
    ios_close(&symbols);

    write_uint(f, relocs.size);
    write_padding(f, LLT_ALIGN(ios_pos(f), 8) - ios_pos(f));
    ios_seek(&relocs, 0);
    ios_copyall(f, &relocs);
    ios_close(&relocs);

    write_uint(f, gvar_record.size);
    write_padding(f, LLT_ALIGN(ios_pos(f), 8) - ios_pos(f));
    ios_seek(&gvar_record, 0);
    ios_copyall(f, &gvar_record);
    ios_close(&gvar_record);

    write_uint(f, fptr_record.size);
    write_padding(f, LLT_ALIGN(ios_pos(f), 8) - ios_pos(f));
    ios_seek(&fptr_record, 0);
    ios_copyall(f, &fptr_record);
    ios_close(&fptr_record);

    { // step 4: record locations of special roots
        write_padding(f, LLT_ALIGN(ios_pos(f), 8) - ios_pos(f));
        s.s = f;
        if (worklist == NULL) {
            size_t i;
            for (i = 0; tags[i] != NULL; i++) {
                jl_value_t *tag = *tags[i];
                jl_write_value(&s, tag);
            }
            jl_write_value(&s, jl_global_roots_table);
            jl_write_value(&s, s.ptls->root_task->tls);
            write_uint32(f, jl_get_gs_ctr());
            write_uint(f, jl_atomic_load_acquire(&jl_world_counter));
            write_uint(f, jl_typeinf_world);
        }
        else {
            jl_write_value(&s, worklist);
            // save module initialization order
            if (jl_module_init_order != NULL) {
                size_t i, l = jl_array_len(jl_module_init_order);
                for (i = 0; i < l; i++) {
                    // verify that all these modules were saved
                    assert(ptrhash_get(&serialization_order, jl_array_ptr_ref(jl_module_init_order, i)) != HT_NOTFOUND);
                }
            }
            jl_write_value(&s, jl_module_init_order);
            jl_write_value(&s, extext_methods);
            jl_write_value(&s, new_specializations);
            jl_write_value(&s, method_roots_list);
            jl_write_value(&s, ext_targets);
            jl_write_value(&s, edges);
        }
        write_uint32(f, jl_array_len(s.link_ids_gctags));
        ios_write(f, (char*)jl_array_data(s.link_ids_gctags), jl_array_len(s.link_ids_gctags) * sizeof(uint32_t));
        write_uint32(f, jl_array_len(s.link_ids_relocs));
        ios_write(f, (char*)jl_array_data(s.link_ids_relocs), jl_array_len(s.link_ids_relocs) * sizeof(uint32_t));
        write_uint32(f, jl_array_len(s.link_ids_gvars));
        ios_write(f, (char*)jl_array_data(s.link_ids_gvars), jl_array_len(s.link_ids_gvars) * sizeof(uint32_t));
        write_uint32(f, jl_array_len(s.link_ids_external_fnvars));
        ios_write(f, (char*)jl_array_data(s.link_ids_external_fnvars), jl_array_len(s.link_ids_external_fnvars) * sizeof(uint32_t));
        write_uint32(f, external_fns_begin);
        jl_write_arraylist(s.s, &s.ccallable_list);
    }

    assert(object_worklist.len == 0);
    arraylist_free(&object_worklist);
    arraylist_free(&serialization_queue);
    arraylist_free(&layout_table);
    arraylist_free(&s.ccallable_list);
    arraylist_free(&s.relocs_list);
    arraylist_free(&s.gctags_list);
    arraylist_free(&gvars);
    arraylist_free(&external_fns);
    htable_free(&field_replace);
    if (worklist)
        htable_free(&external_objects);
    htable_free(&serialization_order);
    htable_free(&unique_ready);
    htable_free(&nullptrs);
    htable_free(&symbol_table);
    htable_free(&fptr_to_id);
    nsym_tag = 0;

    jl_gc_enable(en);
}

static void jl_write_header_for_incremental(ios_t *f, jl_array_t *worklist, jl_array_t *mod_array, jl_array_t **udeps, int64_t *srctextpos, int64_t *checksumpos)
{
    assert(jl_precompile_toplevel_module == NULL);
    jl_precompile_toplevel_module = (jl_module_t*)jl_array_ptr_ref(worklist, jl_array_len(worklist)-1);

    *checksumpos = write_header(f, 0);
    write_uint8(f, jl_cache_flags());
    // write description of contents (name, uuid, buildid)
    write_worklist_for_header(f, worklist);
    // Determine unique (module, abspath, mtime) dependencies for the files defining modules in the worklist
    // (see Base._require_dependencies). These get stored in `udeps` and written to the ji-file header.
    // Also write Preferences.
    // last word of the dependency list is the end of the data / start of the srctextpos
    *srctextpos = write_dependency_list(f, worklist, udeps);  // srctextpos: position of srctext entry in header index (update later)
    // write description of requirements for loading (modules that must be pre-loaded if initialization is to succeed)
    // this can return errors during deserialize,
    // best to keep it early (before any actual initialization)
    write_mod_list(f, mod_array);
}

JL_DLLEXPORT void jl_create_system_image(void **_native_data, jl_array_t *worklist, bool_t emit_split,
                                         ios_t **s, ios_t **z, jl_array_t **udeps, int64_t *srctextpos)
{
    jl_gc_collect(JL_GC_FULL);
    jl_gc_collect(JL_GC_INCREMENTAL);   // sweep finalizers
    JL_TIMING(SYSIMG_DUMP);

    // iff emit_split
    // write header and src_text to one file f/s
    // write systemimg to a second file ff/z
    jl_task_t *ct = jl_current_task;
    ios_t *f = (ios_t*)malloc_s(sizeof(ios_t));
    ios_mem(f, 0);

    ios_t *ff = NULL;
    if (emit_split) {
        ff = (ios_t*)malloc_s(sizeof(ios_t));
        ios_mem(ff, 0);
    } else {
        ff = f;
    }

    jl_array_t *mod_array = NULL, *extext_methods = NULL, *new_specializations = NULL;
    jl_array_t *method_roots_list = NULL, *ext_targets = NULL, *edges = NULL;
    int64_t checksumpos = 0;
    int64_t checksumpos_ff = 0;
    int64_t datastartpos = 0;
    JL_GC_PUSH6(&mod_array, &extext_methods, &new_specializations, &method_roots_list, &ext_targets, &edges);

    if (worklist) {
        mod_array = jl_get_loaded_modules();  // __toplevel__ modules loaded in this session (from Base.loaded_modules_array)
        // Generate _native_data`
        if (_native_data != NULL) {
            jl_prepare_serialization_data(mod_array, newly_inferred, jl_worklist_key(worklist),
                                          &extext_methods, &new_specializations, NULL, NULL, NULL);
            jl_precompile_toplevel_module = (jl_module_t*)jl_array_ptr_ref(worklist, jl_array_len(worklist)-1);
            *_native_data = jl_precompile_worklist(worklist, extext_methods, new_specializations);
            jl_precompile_toplevel_module = NULL;
            extext_methods = NULL;
            new_specializations = NULL;
        }
        jl_write_header_for_incremental(f, worklist, mod_array, udeps, srctextpos, &checksumpos);
        if (emit_split) {
            checksumpos_ff = write_header(ff, 1);
            write_uint8(ff, jl_cache_flags());
            write_mod_list(ff, mod_array);
        }
        else {
            checksumpos_ff = checksumpos;
        }
    }
    else if (_native_data != NULL) {
        *_native_data = jl_precompile(jl_options.compile_enabled == JL_OPTIONS_COMPILE_ALL);
    }

    // Make sure we don't run any Julia code concurrently after this point
    // since it will invalidate our serialization preparations
    jl_gc_enable_finalizers(ct, 0);
    assert(ct->reentrant_inference == 0);
    ct->reentrant_inference = (uint16_t)-1;
    if (worklist) {
        jl_prepare_serialization_data(mod_array, newly_inferred, jl_worklist_key(worklist),
                                      &extext_methods, &new_specializations, &method_roots_list, &ext_targets, &edges);
        if (!emit_split) {
            write_int32(f, 0); // No clone_targets
            write_padding(f, LLT_ALIGN(ios_pos(f), JL_CACHE_BYTE_ALIGNMENT) - ios_pos(f));
        }
        else {
            write_padding(ff, LLT_ALIGN(ios_pos(ff), JL_CACHE_BYTE_ALIGNMENT) - ios_pos(ff));
        }
        datastartpos = ios_pos(ff);
    }
    if (_native_data != NULL)
        native_functions = *_native_data;
    jl_save_system_image_to_stream(ff, mod_array, worklist, extext_methods, new_specializations, method_roots_list, ext_targets, edges);
    if (_native_data != NULL)
        native_functions = NULL;
    // make sure we don't run any Julia code concurrently before this point
    // Re-enable running julia code for postoutput hooks, atexit, etc.
    jl_gc_enable_finalizers(ct, 1);
    ct->reentrant_inference = 0;
    jl_precompile_toplevel_module = NULL;

    if (worklist) {
        // Go back and update the checksum in the header
        int64_t dataendpos = ios_pos(ff);
        uint32_t checksum = jl_crc32c(0, &ff->buf[datastartpos], dataendpos - datastartpos);
        ios_seek(ff, checksumpos_ff);
        write_uint64(ff, checksum | ((uint64_t)0xfafbfcfd << 32));
        write_uint64(ff, datastartpos);
        write_uint64(ff, dataendpos);
        ios_seek(ff, dataendpos);

        // Write the checksum to the split header if necessary
        if (emit_split) {
            int64_t cur = ios_pos(f);
            ios_seek(f, checksumpos);
            write_uint64(f, checksum | ((uint64_t)0xfafbfcfd << 32));
            ios_seek(f, cur);
            // Next we will write the clone_targets and afterwards the srctext
        }
    }

    JL_GC_POP();
    *s = f;
    if (emit_split)
        *z = ff;
    return;
}

JL_DLLEXPORT size_t ios_write_direct(ios_t *dest, ios_t *src);

// Takes in a path of the form "usr/lib/julia/sys.so" (jl_restore_system_image should be passed the same string)
JL_DLLEXPORT void jl_preload_sysimg_so(const char *fname)
{
    if (jl_sysimg_handle)
        return; // embedded target already called jl_set_sysimg_so

    char *dot = (char*) strrchr(fname, '.');
    int is_ji = (dot && !strcmp(dot, ".ji"));

    // Get handle to sys.so
    if (!is_ji) // .ji extension => load .ji file only
        jl_set_sysimg_so(jl_load_dynamic_library(fname, JL_RTLD_LOCAL | JL_RTLD_NOW, 1));
}

// Allow passing in a module handle directly, rather than a path
JL_DLLEXPORT void jl_set_sysimg_so(void *handle)
{
    void* *jl_RTLD_DEFAULT_handle_pointer;
    int symbol_found = jl_dlsym(handle, "jl_RTLD_DEFAULT_handle_pointer", (void **)&jl_RTLD_DEFAULT_handle_pointer, 0);
    if (!symbol_found || (void*)&jl_RTLD_DEFAULT_handle != *jl_RTLD_DEFAULT_handle_pointer)
        jl_error("System image file failed consistency check: maybe opened the wrong version?");
    if (jl_options.cpu_target == NULL)
        jl_options.cpu_target = "native";
    jl_sysimg_handle = handle;
    sysimage = jl_init_processor_sysimg(handle);
}

#ifndef JL_NDEBUG
// skip the performance optimizations of jl_types_equal and just use subtyping directly
// one of these types is invalid - that's why we're doing the recache type operation
// static int jl_invalid_types_equal(jl_datatype_t *a, jl_datatype_t *b)
// {
//     return jl_subtype((jl_value_t*)a, (jl_value_t*)b) && jl_subtype((jl_value_t*)b, (jl_value_t*)a);
// }
#endif

static void jl_restore_system_image_from_stream_(ios_t *f, jl_image_t *image, jl_array_t *depmods, uint64_t checksum,
                                /* outputs */    jl_array_t **restored,         jl_array_t **init_order,
                                                 jl_array_t **extext_methods,
                                                 jl_array_t **new_specializations, jl_array_t **method_roots_list,
                                                 jl_array_t **ext_targets, jl_array_t **edges,
                                                 char **base, arraylist_t *ccallable_list, pkgcachesizes *cachesizes) JL_GC_DISABLED
{
    JL_TIMING(SYSIMG_LOAD);
    int en = jl_gc_enable(0);
    ios_t sysimg, const_data, symbols, relocs, gvar_record, fptr_record;
    jl_serializer_state s;
    s.incremental = restored != NULL; // jl_linkage_blobs.len > 0;
    s.image = image;
    s.s = NULL;
    s.const_data = &const_data;
    s.symbols = &symbols;
    s.relocs = &relocs;
    s.gvar_record = &gvar_record;
    s.fptr_record = &fptr_record;
    s.ptls = jl_current_task->ptls;
    arraylist_new(&s.relocs_list, 0);
    arraylist_new(&s.gctags_list, 0);
    s.link_ids_relocs = s.link_ids_gctags = s.link_ids_gvars = s.link_ids_external_fnvars = NULL;
    jl_value_t **const*const tags = get_tags();
    htable_t new_dt_objs;
    htable_new(&new_dt_objs, 0);
    arraylist_new(&deser_sym, 0);

    // step 1: read section map
    assert(ios_pos(f) == 0 && f->bm == bm_mem);
    size_t sizeof_sysimg = read_uint(f);
    ios_static_buffer(&sysimg, f->buf, sizeof_sysimg + sizeof(uintptr_t));
    ios_skip(f, sizeof_sysimg);

    size_t sizeof_constdata = read_uint(f);
    // realign stream to max-alignment for data
    ios_seek(f, LLT_ALIGN(ios_pos(f), JL_CACHE_BYTE_ALIGNMENT));
    ios_static_buffer(&const_data, f->buf + f->bpos, sizeof_constdata);
    ios_skip(f, sizeof_constdata);

    size_t sizeof_symbols = read_uint(f);
    ios_seek(f, LLT_ALIGN(ios_pos(f), 8));
    ios_static_buffer(&symbols, f->buf + f->bpos, sizeof_symbols);
    ios_skip(f, sizeof_symbols);

    size_t sizeof_relocations = read_uint(f);
    ios_seek(f, LLT_ALIGN(ios_pos(f), 8));
    assert(!ios_eof(f));
    ios_static_buffer(&relocs, f->buf + f->bpos, sizeof_relocations);
    ios_skip(f, sizeof_relocations);

    size_t sizeof_gvar_record = read_uint(f);
    ios_seek(f, LLT_ALIGN(ios_pos(f), 8));
    assert(!ios_eof(f));
    ios_static_buffer(&gvar_record, f->buf + f->bpos, sizeof_gvar_record);
    ios_skip(f, sizeof_gvar_record);

    size_t sizeof_fptr_record = read_uint(f);
    ios_seek(f, LLT_ALIGN(ios_pos(f), 8));
    assert(!ios_eof(f));
    ios_static_buffer(&fptr_record, f->buf + f->bpos, sizeof_fptr_record);
    ios_skip(f, sizeof_fptr_record);

    // step 2: get references to special values
    ios_seek(f, LLT_ALIGN(ios_pos(f), 8));
    assert(!ios_eof(f));
    s.s = f;
    uintptr_t offset_restored = 0, offset_init_order = 0, offset_extext_methods = 0, offset_new_specializations = 0, offset_method_roots_list = 0;
    uintptr_t offset_ext_targets = 0, offset_edges = 0;
    if (!s.incremental) {
        size_t i;
        for (i = 0; tags[i] != NULL; i++) {
            jl_value_t **tag = tags[i];
            *tag = jl_read_value(&s);
        }
        jl_global_roots_table = (jl_array_t*)jl_read_value(&s);
        // set typeof extra-special values now that we have the type set by tags above
        jl_astaggedvalue(jl_current_task)->header = (uintptr_t)jl_task_type | jl_astaggedvalue(jl_current_task)->header;
        jl_astaggedvalue(jl_nothing)->header = (uintptr_t)jl_nothing_type | jl_astaggedvalue(jl_nothing)->header;
        s.ptls->root_task->tls = jl_read_value(&s);
        jl_gc_wb(s.ptls->root_task, s.ptls->root_task->tls);
        jl_init_int32_int64_cache();
        jl_init_box_caches();

        uint32_t gs_ctr = read_uint32(f);
        jl_atomic_store_release(&jl_world_counter, read_uint(f));
        jl_typeinf_world = read_uint(f);
        jl_set_gs_ctr(gs_ctr);
    }
    else {
        jl_atomic_fetch_add(&jl_world_counter, 1);
        offset_restored = jl_read_offset(&s);
        offset_init_order = jl_read_offset(&s);
        offset_extext_methods = jl_read_offset(&s);
        offset_new_specializations = jl_read_offset(&s);
        offset_method_roots_list = jl_read_offset(&s);
        offset_ext_targets = jl_read_offset(&s);
        offset_edges = jl_read_offset(&s);
    }
    s.buildid_depmods_idxs = depmod_to_imageidx(depmods);
    size_t nlinks_gctags = read_uint32(f);
    if (nlinks_gctags > 0) {
        s.link_ids_gctags = jl_alloc_array_1d(jl_array_int32_type, nlinks_gctags);
        ios_read(f, (char*)jl_array_data(s.link_ids_gctags), nlinks_gctags * sizeof(uint32_t));
    }
    size_t nlinks_relocs = read_uint32(f);
    if (nlinks_relocs > 0) {
        s.link_ids_relocs = jl_alloc_array_1d(jl_array_int32_type, nlinks_relocs);
        ios_read(f, (char*)jl_array_data(s.link_ids_relocs), nlinks_relocs * sizeof(uint32_t));
    }
    size_t nlinks_gvars = read_uint32(f);
    if (nlinks_gvars > 0) {
        s.link_ids_gvars = jl_alloc_array_1d(jl_array_int32_type, nlinks_gvars);
        ios_read(f, (char*)jl_array_data(s.link_ids_gvars), nlinks_gvars * sizeof(uint32_t));
    }
    size_t nlinks_external_fnvars = read_uint32(f);
    if (nlinks_external_fnvars > 0) {
        s.link_ids_external_fnvars = jl_alloc_array_1d(jl_array_int32_type, nlinks_external_fnvars);
        ios_read(f, (char*)jl_array_data(s.link_ids_external_fnvars), nlinks_external_fnvars * sizeof(uint32_t));
    }
    uint32_t external_fns_begin = read_uint32(f);
    jl_read_arraylist(s.s, ccallable_list ? ccallable_list : &s.ccallable_list);
    if (s.incremental) {
        assert(restored && init_order && extext_methods && new_specializations && method_roots_list && ext_targets && edges);
        *restored = (jl_array_t*)jl_delayed_reloc(&s, offset_restored);
        *init_order = (jl_array_t*)jl_delayed_reloc(&s, offset_init_order);
        *extext_methods = (jl_array_t*)jl_delayed_reloc(&s, offset_extext_methods);
        *new_specializations = (jl_array_t*)jl_delayed_reloc(&s, offset_new_specializations);
        *method_roots_list = (jl_array_t*)jl_delayed_reloc(&s, offset_method_roots_list);
        *ext_targets = (jl_array_t*)jl_delayed_reloc(&s, offset_ext_targets);
        *edges = (jl_array_t*)jl_delayed_reloc(&s, offset_edges);
        if (!*new_specializations)
            *new_specializations = jl_alloc_vec_any(0);
    }
    s.s = NULL;

    // step 3: apply relocations
    assert(!ios_eof(f));
    jl_read_symbols(&s);
    ios_close(&symbols);

    char *image_base = (char*)&sysimg.buf[0];
    reloc_t *relocs_base = (reloc_t*)&relocs.buf[0];
    if (base)
        *base = image_base;

    s.s = &sysimg;
    jl_read_reloclist(&s, s.link_ids_gctags, GC_OLD); // gctags
    size_t sizeof_tags = ios_pos(&relocs);
    (void)sizeof_tags;
    jl_read_reloclist(&s, s.link_ids_relocs, 0); // general relocs
    // s.link_ids_gvars will be processed in `jl_update_all_gvars`
    // s.link_ids_external_fns will be processed in `jl_update_all_gvars`
    jl_update_all_gvars(&s, image, external_fns_begin); // gvars relocs
    if (s.incremental) {
        jl_read_arraylist(s.relocs, &s.uniquing_types);
        jl_read_arraylist(s.relocs, &s.uniquing_objs);
        jl_read_arraylist(s.relocs, &s.fixup_types);
    }
    else {
        arraylist_new(&s.uniquing_types, 0);
        arraylist_new(&s.uniquing_objs, 0);
        arraylist_new(&s.fixup_types, 0);
    }
    jl_read_arraylist(s.relocs, &s.fixup_objs);
    // Perform the uniquing of objects that we don't "own" and consequently can't promise
    // weren't created by some other package before this one got loaded:
    // - iterate through all objects that need to be uniqued. The first encounter has to be the
    //   "reconstructable blob". We either look up the object (if something has created it previously)
    //   or construct it for the first time, crucially outside the pointer range of any pkgimage.
    //   This ensures it stays unique-worthy.
    // - after we've stored the address of the "real" object (which for convenience we do among the data
    //   written to allow lookup/reconstruction), then we have to update references to that "reconstructable blob":
    //   instead of performing the relocation within the package image, we instead (re)direct all references
    //   to the external object.
    arraylist_t cleanup_list;
    arraylist_new(&cleanup_list, 0);
    arraylist_t delay_list;
    arraylist_new(&delay_list, 0);
    for (size_t i = 0; i < s.uniquing_types.len; i++) {
        uintptr_t item = (uintptr_t)s.uniquing_types.items[i];
        // check whether we are operating on the typetag
        // (needing to ignore GC bits) or a regular field
        int tag = (item & 1) == 1;
        // check whether this is a gvar index
        int gvar = (item & 2) == 2;
        item &= ~(uintptr_t)3;
        uintptr_t *pfld;
        jl_value_t **obj, *newobj;
        if (gvar) {
            if (image->gvars_base == NULL)
                continue;
            item >>= 2;
            assert(item < s.gvar_record->size / sizeof(reloc_t));
            pfld = sysimg_gvars(image->gvars_base, image->gvars_offsets, item);
            obj = *(jl_value_t***)pfld;
            assert(tag == 0);
        }
        else {
            pfld = (uintptr_t*)(image_base + item);
            if (tag)
                obj = (jl_value_t**)jl_typeof(jl_valueof(pfld));
            else
                obj = *(jl_value_t***)pfld;
            if ((char*)obj > (char*)pfld) {
                assert(tag == 0);
                arraylist_push(&delay_list, pfld);
                arraylist_push(&delay_list, obj);
                ptrhash_put(&new_dt_objs, (void*)obj, obj); // mark obj as invalid
                *pfld = (uintptr_t)NULL;
                continue;
            }
        }
        jl_value_t *otyp = jl_typeof(obj);   // the original type of the object that was written here
        assert(image_base < (char*)obj && (char*)obj <= image_base + sizeof_sysimg + sizeof(uintptr_t));
        if (otyp == (jl_value_t*)jl_datatype_type) {
            jl_datatype_t *dt = (jl_datatype_t*)obj[0], *newdt;
            if (jl_is_datatype(dt)) {
                newdt = dt; // already done
            }
            else {
                dt = (jl_datatype_t*)obj;
                arraylist_push(&cleanup_list, (void*)obj);
                ptrhash_remove(&new_dt_objs, (void*)obj); // unmark obj as invalid before must_be_new_dt
                if (must_be_new_dt((jl_value_t*)dt, &new_dt_objs, image_base, sizeof_sysimg))
                    newdt = NULL;
                else
                    newdt = jl_lookup_cache_type_(dt);
                if (newdt == NULL) {
                    // make a non-owned copy of obj so we don't accidentally
                    // assume this is the unique copy later
                    newdt = jl_new_uninitialized_datatype();
                    jl_astaggedvalue(newdt)->bits.gc = GC_OLD;
                    // leave most fields undefined for now, but we may need instance later,
                    // and we overwrite the name field (field 0) now so preserve it too
                    if (dt->instance) {
                        assert(dt->instance == jl_nothing);
                        newdt->instance = dt->instance = jl_gc_permobj(0, newdt);
                    }
                    static_assert(offsetof(jl_datatype_t, name) == 0, "");
                    newdt->name = dt->name;
                    ptrhash_put(&new_dt_objs, (void*)newdt, dt);
                }
                else {
                    assert(newdt->hash == dt->hash);
                }
                obj[0] = (jl_value_t*)newdt;
            }
            newobj = (jl_value_t*)newdt;
        }
        else {
            assert(!(image_base < (char*)otyp && (char*)otyp <= image_base + sizeof_sysimg + sizeof(uintptr_t)));
            assert(jl_is_datatype_singleton((jl_datatype_t*)otyp) && "unreachable");
            newobj = ((jl_datatype_t*)otyp)->instance;
            assert(newobj != jl_nothing);
            arraylist_push(&cleanup_list, (void*)obj);
        }
        if (tag)
            *pfld = (uintptr_t)newobj | GC_OLD;
        else
            *pfld = (uintptr_t)newobj;
        assert(!(image_base < (char*)newobj && (char*)newobj <= image_base + sizeof_sysimg + sizeof(uintptr_t)));
        assert(jl_typeis(obj, otyp));
    }
    // A few fields (reached via super) might be self-recursive. This is rare, but handle them now.
    // They cannot be instances though, since the type must fully exist before the singleton field can be allocated
    for (size_t i = 0; i < delay_list.len; ) {
        uintptr_t *pfld = (uintptr_t*)delay_list.items[i++];
        jl_value_t **obj = (jl_value_t **)delay_list.items[i++];
        assert(jl_is_datatype(obj));
        jl_datatype_t *dt = (jl_datatype_t*)obj[0];
        assert(jl_is_datatype(dt));
        jl_value_t *newobj = (jl_value_t*)dt;
        *pfld = (uintptr_t)newobj;
        assert(!(image_base < (char*)newobj && (char*)newobj <= image_base + sizeof_sysimg + sizeof(uintptr_t)));
    }
    arraylist_free(&delay_list);
    // now that all the fields of dt are assigned and unique, copy them into
    // their final newdt memory location: this ensures we do not accidentally
    // think this pkg image has the singular unique copy of it
    void **table = new_dt_objs.table;
    for (size_t i = 0; i < new_dt_objs.size; i += 2) {
        void *dt = table[i + 1];
        if (dt != HT_NOTFOUND) {
            jl_datatype_t *newdt = (jl_datatype_t*)table[i];
            jl_typename_t *name = newdt->name;
            static_assert(offsetof(jl_datatype_t, name) == 0, "");
            assert(*(void**)dt == (void*)newdt);
            *newdt = *(jl_datatype_t*)dt; // copy the datatype fields (except field 1, which we corrupt above)
            newdt->name = name;
        }
    }
    // we should never see these pointers again, so scramble their memory, so any attempt to look at them crashes
    for (size_t i = 0; i < cleanup_list.len; i++) {
        void *item = cleanup_list.items[i];
        jl_taggedvalue_t *o = jl_astaggedvalue(item);
        jl_value_t *t = jl_typeof(item); // n.b. might be 0xbabababa already
        if (t == (jl_value_t*)jl_datatype_type)
            memset(o, 0xba, sizeof(jl_value_t*) + sizeof(jl_datatype_t));
        else
            memset(o, 0xba, sizeof(jl_value_t*) + 0); // singleton
    }
    arraylist_grow(&cleanup_list, -cleanup_list.len);
    // finally cache all our new types now
    for (size_t i = 0; i < new_dt_objs.size; i += 2) {
        void *dt = table[i + 1];
        if (dt != HT_NOTFOUND) {
            jl_datatype_t *newdt = (jl_datatype_t*)table[i];
            jl_cache_type_(newdt);
        }
    }
    for (size_t i = 0; i < s.fixup_types.len; i++) {
        uintptr_t item = (uintptr_t)s.fixup_types.items[i];
        jl_value_t *obj = (jl_value_t*)(image_base + item);
        assert(jl_is_datatype(obj));
        jl_cache_type_((jl_datatype_t*)obj);
    }
    // Perform fixups: things like updating world ages, inserting methods & specializations, etc.
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    for (size_t i = 0; i < s.uniquing_objs.len; i++) {
        uintptr_t item = (uintptr_t)s.uniquing_objs.items[i];
        // check whether this is a gvar index
        int gvar = (item & 2) == 2;
        item &= ~(uintptr_t)3;
        uintptr_t *pfld;
        jl_value_t **obj, *newobj;
        if (gvar) {
            if (image->gvars_base == NULL)
                continue;
            item >>= 2;
            assert(item < s.gvar_record->size / sizeof(reloc_t));
            pfld = sysimg_gvars(image->gvars_base, image->gvars_offsets, item);
            obj = *(jl_value_t***)pfld;
        }
        else {
            pfld = (uintptr_t*)(image_base + item);
            obj = *(jl_value_t***)pfld;
        }
        jl_value_t *otyp = jl_typeof(obj);   // the original type of the object that was written here
        if (otyp == (jl_value_t*)jl_method_instance_type) {
            assert(image_base < (char*)obj && (char*)obj <= image_base + sizeof_sysimg + sizeof(uintptr_t));
            jl_value_t *m = obj[0];
            if (jl_is_method_instance(m)) {
                newobj = m; // already done
            }
            else {
                arraylist_push(&cleanup_list, (void*)obj);
                jl_value_t *specTypes = obj[1];
                jl_value_t *sparams = obj[2];
                newobj = (jl_value_t*)jl_specializations_get_linfo((jl_method_t*)m, specTypes, (jl_svec_t*)sparams);
                obj[0] = newobj;
            }
        }
        else {
            abort(); // should be unreachable
        }
        *pfld = (uintptr_t)newobj;
        assert(!(image_base < (char*)newobj && (char*)newobj <= image_base + sizeof_sysimg + sizeof(uintptr_t)));
        assert(jl_typeis(obj, otyp));
    }
    arraylist_free(&s.uniquing_types);
    arraylist_free(&s.uniquing_objs);
    for (size_t i = 0; i < cleanup_list.len; i++) {
        void *item = cleanup_list.items[i];
        jl_taggedvalue_t *o = jl_astaggedvalue(item);
        jl_value_t *t = jl_typeof(item);
        if (t == (jl_value_t*)jl_method_instance_type)
            memset(o, 0xba, sizeof(jl_value_t*) * 3); // only specTypes and sparams fields stored
    }
    arraylist_free(&cleanup_list);
    for (size_t i = 0; i < s.fixup_objs.len; i++) {
        uintptr_t item = (uintptr_t)s.fixup_objs.items[i];
        jl_value_t *obj = (jl_value_t*)(image_base + item);
        if (jl_typeis(obj, jl_typemap_entry_type)) {
            jl_typemap_entry_t *entry = (jl_typemap_entry_t*)obj;
            entry->min_world = world;
        }
        else if (jl_is_method(obj)) {
            jl_method_t *m = (jl_method_t*)obj;
            m->primary_world = world;
        }
        else if (jl_is_method_instance(obj)) {
            jl_method_instance_t *newobj = jl_specializations_get_or_insert((jl_method_instance_t*)obj);
            assert(newobj == (jl_method_instance_t*)obj); // strict insertion expected
            (void)newobj;
        }
        else if (jl_is_code_instance(obj)) {
            jl_code_instance_t *ci = (jl_code_instance_t*)obj;
            assert(s.incremental);
            ci->min_world = world;
            if (ci->max_world != 0)
                jl_array_ptr_1d_push(*new_specializations, (jl_value_t*)ci);
        }
        else if (jl_is_globalref(obj)) {
            continue; // wait until all the module binding tables have been initialized
        }
        else if (jl_is_module(obj)) {
            // rebuild the binding table for module v
            // TODO: maybe want to hold the lock on `v`, but that only strongly matters for async / thread safety
            // and we are already bad at that
            jl_module_t *mod = (jl_module_t*)obj;
            mod->build_id.hi = checksum;
            mod->primary_world = world;
            if (mod->usings.items != &mod->usings._space[0]) {
                // arraylist_t assumes we called malloc to get this memory, so make that true now
                void **newitems = (void**)malloc_s(mod->usings.max * sizeof(void*));
                memcpy(newitems, mod->usings.items, mod->usings.len * sizeof(void*));
                mod->usings.items = newitems;
            }
        }
        else {
            // rehash IdDict
            //assert(((jl_datatype_t*)(jl_typeof(obj)))->name == jl_idtable_typename);
            jl_array_t **a = (jl_array_t**)obj;
            assert(jl_typeis(*a, jl_array_any_type));
            *a = jl_idtable_rehash(*a, jl_array_len(*a));
            jl_gc_wb(obj, *a);
        }
    }
    // Now pick up the globalref binding pointer field
    for (size_t i = 0; i < s.fixup_objs.len; i++) {
        uintptr_t item = (uintptr_t)s.fixup_objs.items[i];
        jl_value_t *obj = (jl_value_t*)(image_base + item);
        if (jl_is_globalref(obj)) {
            jl_globalref_t *r = (jl_globalref_t*)obj;
            if (r->binding == NULL) {
                jl_globalref_t *gr = (jl_globalref_t*)jl_module_globalref(r->mod, r->name);
                r->binding = gr->binding;
                jl_gc_wb(r, gr->binding);
            }
        }
    }
    arraylist_free(&s.fixup_types);
    arraylist_free(&s.fixup_objs);

    if (s.incremental)
        jl_root_new_gvars(&s, image, external_fns_begin);
    ios_close(&relocs);
    ios_close(&const_data);
    ios_close(&gvar_record);

    htable_free(&new_dt_objs);

    s.s = NULL;

    if (0) {
        printf("sysimg size breakdown:\n"
               "     sys data: %8u\n"
               "  isbits data: %8u\n"
               "      symbols: %8u\n"
               "    tags list: %8u\n"
               "   reloc list: %8u\n"
               "    gvar list: %8u\n"
               "    fptr list: %8u\n",
            (unsigned)sizeof_sysimg,
            (unsigned)sizeof_constdata,
            (unsigned)sizeof_symbols,
            (unsigned)sizeof_tags,
            (unsigned)(sizeof_relocations - sizeof_tags),
            (unsigned)sizeof_gvar_record,
            (unsigned)sizeof_fptr_record);
    }
    if (cachesizes) {
        cachesizes->sysdata = sizeof_sysimg;
        cachesizes->isbitsdata = sizeof_constdata;
        cachesizes->symboldata = sizeof_symbols;
        cachesizes->tagslist = sizeof_tags;
        cachesizes->reloclist = sizeof_relocations - sizeof_tags;
        cachesizes->gvarlist = sizeof_gvar_record;
        cachesizes->fptrlist = sizeof_fptr_record;
    }

    s.s = &sysimg;
    jl_update_all_fptrs(&s, image); // fptr relocs and registration
    if (!ccallable_list) {
        // TODO: jl_sysimg_handle or img_handle?
        jl_reinit_ccallable(&s.ccallable_list, image_base, jl_sysimg_handle);
        arraylist_free(&s.ccallable_list);
    }
    s.s = NULL;

    ios_close(&fptr_record);
    ios_close(&sysimg);

    if (!s.incremental)
        jl_gc_reset_alloc_count();
    arraylist_free(&deser_sym);

    // Prepare for later external linkage against the sysimg
    // Also sets up images for protection against garbage collection
    arraylist_push(&jl_linkage_blobs, (void*)image_base);
    arraylist_push(&jl_linkage_blobs, (void*)(image_base + sizeof_sysimg + sizeof(uintptr_t)));
    arraylist_push(&jl_image_relocs, (void*)relocs_base);

    // jl_printf(JL_STDOUT, "%ld blobs to link against\n", jl_linkage_blobs.len >> 1);
    jl_gc_enable(en);
}

static jl_value_t *jl_validate_cache_file(ios_t *f, jl_array_t *depmods, uint64_t *checksum, int64_t *dataendpos, int64_t *datastartpos)
{
    uint8_t pkgimage = 0;
    if (ios_eof(f) || 0 == (*checksum = jl_read_verify_header(f, &pkgimage, dataendpos, datastartpos)) || (*checksum >> 32 != 0xfafbfcfd)) {
        return jl_get_exceptionf(jl_errorexception_type,
                "Precompile file header verification checks failed.");
    }
    uint8_t flags = read_uint8(f);
    if (pkgimage && !jl_match_cache_flags(flags)) {
        return jl_get_exceptionf(jl_errorexception_type, "Pkgimage flags mismatch");
    }
    if (!pkgimage) {
        // skip past the worklist
        size_t len;
        while ((len = read_int32(f)))
            ios_skip(f, len + 3 * sizeof(uint64_t));
        // skip past the dependency list
        size_t deplen = read_uint64(f);
        ios_skip(f, deplen - sizeof(uint64_t));
        read_uint64(f); // where is this write coming from?
    }

    // verify that the system state is valid
    return read_verify_mod_list(f, depmods);
}

// TODO?: refactor to make it easier to create the "package inspector"
static jl_value_t *jl_restore_package_image_from_stream(ios_t *f, jl_image_t *image, jl_array_t *depmods, int completeinfo)
{
    uint64_t checksum = 0;
    int64_t dataendpos = 0;
    int64_t datastartpos = 0;
    jl_value_t *verify_fail = jl_validate_cache_file(f, depmods, &checksum, &dataendpos, &datastartpos);

    if (verify_fail)
        return verify_fail;

    assert(datastartpos > 0 && datastartpos < dataendpos);

    jl_value_t *restored = NULL;
    jl_array_t *init_order = NULL, *extext_methods = NULL, *new_specializations = NULL, *method_roots_list = NULL, *ext_targets = NULL, *edges = NULL;
    jl_svec_t *cachesizes_sv = NULL;
    char *base;
    arraylist_t ccallable_list;
    JL_GC_PUSH8(&restored, &init_order, &extext_methods, &new_specializations, &method_roots_list, &ext_targets, &edges, &cachesizes_sv);

    { // make a permanent in-memory copy of f (excluding the header)
        ios_bufmode(f, bm_none);
        JL_SIGATOMIC_BEGIN();
        size_t len = dataendpos - datastartpos;
        char *sysimg = (char*)jl_gc_perm_alloc(len, 0, 64, 0);
        ios_seek(f, datastartpos);
        if (ios_readall(f, sysimg, len) != len || jl_crc32c(0, sysimg, len) != (uint32_t)checksum) {
            restored = jl_get_exceptionf(jl_errorexception_type, "Error reading system image file.");
            JL_SIGATOMIC_END();
        }
        else {
            ios_close(f);
            ios_static_buffer(f, sysimg, len);
            pkgcachesizes cachesizes;
            jl_restore_system_image_from_stream_(f, image, depmods, checksum, (jl_array_t**)&restored, &init_order, &extext_methods, &new_specializations, &method_roots_list, &ext_targets, &edges, &base, &ccallable_list, &cachesizes);
            JL_SIGATOMIC_END();

            // Insert method extensions
            jl_insert_methods(extext_methods);
            // No special processing of `new_specializations` is required because recaching handled it
            // Add roots to methods
            jl_copy_roots(method_roots_list, jl_worklist_key((jl_array_t*)restored));
            // Handle edges
            size_t world = jl_atomic_load_acquire(&jl_world_counter);
            jl_insert_backedges((jl_array_t*)edges, (jl_array_t*)ext_targets, (jl_array_t*)new_specializations, world); // restore external backedges (needs to be last)
            // reinit ccallables
            jl_reinit_ccallable(&ccallable_list, base, NULL);
            arraylist_free(&ccallable_list);

            if (completeinfo) {
                cachesizes_sv = jl_alloc_svec(7);
                jl_svecset(cachesizes_sv, 0, jl_box_long(cachesizes.sysdata));
                jl_svecset(cachesizes_sv, 1, jl_box_long(cachesizes.isbitsdata));
                jl_svecset(cachesizes_sv, 2, jl_box_long(cachesizes.symboldata));
                jl_svecset(cachesizes_sv, 3, jl_box_long(cachesizes.tagslist));
                jl_svecset(cachesizes_sv, 4, jl_box_long(cachesizes.reloclist));
                jl_svecset(cachesizes_sv, 5, jl_box_long(cachesizes.gvarlist));
                jl_svecset(cachesizes_sv, 6, jl_box_long(cachesizes.fptrlist));
                restored = (jl_value_t*)jl_svec(8, restored, init_order, extext_methods, new_specializations, method_roots_list,
                                                   ext_targets, edges, cachesizes_sv);
            }
            else {
                restored = (jl_value_t*)jl_svec(2, restored, init_order);
            }
        }
    }

    JL_GC_POP();
    return restored;
}

static void jl_restore_system_image_from_stream(ios_t *f, jl_image_t *image, uint32_t checksum)
{
    jl_restore_system_image_from_stream_(f, image, NULL, checksum | ((uint64_t)0xfdfcfbfa << 32), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental_from_buf(const char *buf, jl_image_t *image, size_t sz, jl_array_t *depmods, int completeinfo)
{
    ios_t f;
    ios_static_buffer(&f, (char*)buf, sz);
    jl_value_t *ret = jl_restore_package_image_from_stream(&f, image, depmods, completeinfo);
    ios_close(&f);
    return ret;
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental(const char *fname, jl_array_t *depmods, int completeinfo)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL) {
        return jl_get_exceptionf(jl_errorexception_type,
            "Cache file \"%s\" not found.\n", fname);
    }
    jl_image_t pkgimage = {};
    jl_value_t *ret = jl_restore_package_image_from_stream(&f, &pkgimage, depmods, completeinfo);
    ios_close(&f);
    return ret;
}

// TODO: need to enforce that the alignment of the buffer is suitable for vectors
JL_DLLEXPORT void jl_restore_system_image(const char *fname)
{
#ifndef JL_NDEBUG
    char *dot = fname ? (char*)strrchr(fname, '.') : NULL;
    int is_ji = (dot && !strcmp(dot, ".ji"));
    assert((is_ji || jl_sysimg_handle) && "System image file not preloaded");
#endif

    if (jl_sysimg_handle) {
        // load the pre-compiled sysimage from jl_sysimg_handle
        jl_load_sysimg_so();
    }
    else {
        ios_t f;
        if (ios_file(&f, fname, 1, 0, 0, 0) == NULL)
            jl_errorf("System image file \"%s\" not found.", fname);
        ios_bufmode(&f, bm_none);
        JL_SIGATOMIC_BEGIN();
        ios_seek_end(&f);
        size_t len = ios_pos(&f);
        char *sysimg = (char*)jl_gc_perm_alloc(len, 0, 64, 0);
        ios_seek(&f, 0);
        if (ios_readall(&f, sysimg, len) != len)
            jl_errorf("Error reading system image file.");
        ios_close(&f);
        uint32_t checksum = jl_crc32c(0, sysimg, len);
        ios_static_buffer(&f, sysimg, len);
        jl_restore_system_image_from_stream(&f, &sysimage, checksum);
        ios_close(&f);
        JL_SIGATOMIC_END();
    }
}

JL_DLLEXPORT void jl_restore_system_image_data(const char *buf, size_t len)
{
    ios_t f;
    JL_SIGATOMIC_BEGIN();
    ios_static_buffer(&f, (char*)buf, len);
    uint32_t checksum = jl_crc32c(0, buf, len);
    jl_restore_system_image_from_stream(&f, &sysimage, checksum);
    ios_close(&f);
    JL_SIGATOMIC_END();
}

JL_DLLEXPORT jl_value_t *jl_restore_package_image_from_file(const char *fname, jl_array_t *depmods, int completeinfo)
{
    void *pkgimg_handle = jl_dlopen(fname, JL_RTLD_LAZY);
    if (!pkgimg_handle) {
#ifdef _OS_WINDOWS_
        int err;
        char reason[256];
        err = GetLastError();
        win32_formatmessage(err, reason, sizeof(reason));
#else
        const char *reason = dlerror();
#endif
        jl_errorf("Error opening package file %s: %s\n", fname, reason);
    }
    const char *pkgimg_data;
    jl_dlsym(pkgimg_handle, "jl_system_image_data", (void **)&pkgimg_data, 1);
    size_t *plen;
    jl_dlsym(pkgimg_handle, "jl_system_image_size", (void **)&plen, 1);

    jl_image_t pkgimage = jl_init_processor_pkgimg(pkgimg_handle);

    jl_value_t* mod = jl_restore_incremental_from_buf(pkgimg_data, &pkgimage, *plen, depmods, completeinfo);

    return mod;
}

#ifdef __cplusplus
}
#endif
