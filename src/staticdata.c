// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  saving and restoring system images

  This performs serialization and deserialization of in-memory data. The dump.c file is similar, but has less complete coverage:
  dump.c has no knowledge of native code (and simply discards it), whereas this supports native code caching in .o files.
  Duplication is avoided by elevating the .o-serialized versions of global variables and native-compiled functions to become
  the authoritative source for such entities in the system image, with references to these objects appropriately inserted into
  the (de)serialized version of Julia's internal data. This makes deserialization simple and fast: we only need to deal with
  pointer relocation, registering with the garbage collector, and making note of special internal types. During serialization,
  we also need to pay special attention to things like builtin functions, C-implemented types (those in jltypes.c), the metadata
  for documentation, optimal layouts, integration with native system image generation, and preparing other preprocessing
  directives.

  dump.c has capabilities missing from this serializer, most notably the ability to handle external references. This is not needed
  for system images as they are self-contained. However, it would be needed to support incremental compilation of packages.

  During serialization, the flow has several steps:

  - step 1 inserts relevant items into `backref_table`, an `obj` => `id::Int` mapping. `id` is assigned by
    order of insertion. This is effectively a recursive traversal, singling out items like pointers and symbols
    that need restoration when the system image is loaded. This stage is implemented by `jl_serialize_value`
    and its callees; while it would be simplest to use recursion, this risks stack overflow, so recursion is mimicked
    using a work-queue managed by `jl_serialize_reachable`.

    It's worth emphasizing that despite the name `jl_serialize_value`, the only goal of this stage is to
    insert objects into `backref_table`. The entire system gets inserted, either directly or indirectly via
    fields of other objects. Objects requiring pointer relocation or gc registration must be inserted directly.
    In later stages, such objects get referenced by their `id`.

  - step 2 (the biggest of four steps) takes all items in `backref_table` and actually serializes them ordered
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

    Most of step 2 is handled by `jl_write_values`, followed by special handling of the dedicated parallel streams.

  - step 3 combines the different sections (fields of `jl_serializer_state`) into one

  - step 4 writes the values of the hard-coded tagged items and `reinit_list`/`ccallable_list`

The tables written to the serializer stream make deserialization fairly straightforward. Much of the "real work" is
done by `get_item_for_reloc`.

*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h> // printf

#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"
#include "processor.h"
#include "serialize.h"

#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif

#include "valgrind.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// TODO: put WeakRefs on the weak_refs list during deserialization
// TODO: handle finalizers

#define NUM_TAGS    154

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
        INSERT_TAG(jl_typedslot_type);
        INSERT_TAG(jl_expr_type);
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
        INSERT_TAG(jl_abstractslot_type);
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
        INSERT_TAG(jl_builtin_getglobal);
        INSERT_TAG(jl_builtin_setglobal);
        // n.b. must update NUM_TAGS when you add something here

        // All optional tags must be placed at the end, so that we
        // don't accidentally have a `NULL` in the middle
#ifdef SEGV_EXCEPTION
        INSERT_TAG(jl_segv_exception);
#endif
#undef INSERT_TAG
        assert(i >= (NUM_TAGS-2) && i < NUM_TAGS);
    }
    return (jl_value_t**const*const) _tags;
}

// hash of definitions for predefined tagged object
static htable_t symbol_table;
static uintptr_t nsym_tag;
// array of definitions for the predefined tagged object types
// (reverse of symbol_table)
static arraylist_t deser_sym;

// table of all objects that are serialized
static htable_t backref_table;
static int backref_table_numel;
static arraylist_t layout_table;     // cache of `position(s)` for each `id` in `backref_table`
static arraylist_t object_worklist;  // used to mimic recursion by jl_serialize_reachable

// Both `reinit_list` and `ccallable_list` are lists of (size_t pos, code) entries
// for the serializer to mark values in need of rework during deserialization
// codes:
//   1: typename   (reinit_list)
//   2: module     (reinit_list)
//   3: method     (ccallable_list)
static arraylist_t reinit_list;

// @ccallable entry points to install
static arraylist_t ccallable_list;

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
    &jl_f_tuple, &jl_f_svec, &jl_f_intrinsic_call, &jl_f_invoke_kwsorter,
    &jl_f_getfield, &jl_f_setfield, &jl_f_swapfield, &jl_f_modifyfield,
    &jl_f_replacefield, &jl_f_fieldtype, &jl_f_nfields,
    &jl_f_arrayref, &jl_f_const_arrayref, &jl_f_arrayset, &jl_f_arraysize, &jl_f_apply_type,
    &jl_f_applicable, &jl_f_invoke, &jl_f_sizeof, &jl_f__expr, &jl_f__typevar,
    &jl_f_ifelse, &jl_f__structtype, &jl_f__abstracttype, &jl_f__primitivetype,
    &jl_f__typebody, &jl_f__setsuper, &jl_f__equiv_typedef, &jl_f_get_binding_type,
    &jl_f_set_binding_type, &jl_f_opaque_closure_call, &jl_f_donotdelete,
    &jl_f_getglobal, &jl_f_setglobal, &jl_f_finalizer,
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
    jl_ptls_t ptls;
} jl_serializer_state;

static jl_value_t *jl_idtable_type = NULL;
static jl_typename_t *jl_idtable_typename = NULL;
static jl_value_t *jl_bigint_type = NULL;
static int gmp_limb_size = 0;

static jl_sym_t *jl_docmeta_sym = NULL;

// Tags of category `t` are located at offsets `t << RELOC_TAG_OFFSET`
// Consequently there is room for 2^RELOC_TAG_OFFSET pointers, etc
enum RefTags {
    DataRef,           // mutable data
    ConstDataRef,      // constant data (e.g., layouts)
    TagRef,            // items serialized via their tags
    SymbolRef,         // symbols
    BindingRef,        // module bindings
    FunctionRef,       // generic functions
    BuiltinFunctionRef // builtin functions
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


// this supports up to 8 RefTags, 512MB of pointer data, and 4/2 (64/32-bit) GB of constant data.
// if a larger size is required, will need to add support for writing larger relocations in many cases below
#define RELOC_TAG_OFFSET 29

// --- Static Compile ---

static void *jl_sysimg_handle = NULL;
static uint64_t sysimage_base = 0;
static uintptr_t *sysimg_gvars_base = NULL;
static const int32_t *sysimg_gvars_offsets = NULL;
static jl_sysimg_fptrs_t sysimg_fptrs;

static inline uintptr_t *sysimg_gvars(uintptr_t *base, size_t idx)
{
    return base + sysimg_gvars_offsets[idx] / sizeof(base[0]);
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
        jl_dlsym(jl_sysimg_handle, "jl_sysimg_gvars_base", (void **)&sysimg_gvars_base, 1);
        jl_dlsym(jl_sysimg_handle, "jl_sysimg_gvars_offsets", (void **)&sysimg_gvars_offsets, 1);
        sysimg_gvars_offsets += 1;
        assert(sysimg_fptrs.base);

        void *pgcstack_func_slot;
        jl_dlsym(jl_sysimg_handle, "jl_pgcstack_func_slot", &pgcstack_func_slot, 1);
        void *pgcstack_key_slot;
        jl_dlsym(jl_sysimg_handle, "jl_pgcstack_key_slot", &pgcstack_key_slot, 1);
        jl_pgcstack_getkey((jl_get_pgcstack_func**)pgcstack_func_slot, (jl_pgcstack_key_t*)pgcstack_key_slot);

        size_t *tls_offset_idx;
        jl_dlsym(jl_sysimg_handle, "jl_tls_offset", (void **)&tls_offset_idx, 1);
        *tls_offset_idx = (uintptr_t)(jl_tls_offset == -1 ? 0 : jl_tls_offset);

#ifdef _OS_WINDOWS_
        sysimage_base = (intptr_t)jl_sysimg_handle;
#else
        Dl_info dlinfo;
        if (dladdr((void*)sysimg_gvars_base, &dlinfo) != 0) {
            sysimage_base = (intptr_t)dlinfo.dli_fbase;
        }
        else {
            sysimage_base = 0;
        }
#endif
    }
    else {
        memset(&sysimg_fptrs, 0, sizeof(sysimg_fptrs));
    }
    const char *sysimg_data;
    jl_dlsym(jl_sysimg_handle, "jl_system_image_data", (void **)&sysimg_data, 1);
    size_t *plen;
    jl_dlsym(jl_sysimg_handle, "jl_system_image_size", (void **)&plen, 1);
    jl_restore_system_image_data(sysimg_data, *plen);
}


// --- serializer ---

static uintptr_t jl_fptr_id(void *fptr)
{
    void **pbp = ptrhash_bp(&fptr_to_id, fptr);
    if (*pbp == HT_NOTFOUND || fptr == NULL)
        return 0;
    else
        return *(uintptr_t*)pbp;
}

#define jl_serialize_value(s, v) jl_serialize_value_(s,(jl_value_t*)(v),1)
static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v, int recursive);


static void jl_serialize_module(jl_serializer_state *s, jl_module_t *m)
{
    jl_serialize_value(s, m->name);
    jl_serialize_value(s, m->parent);
    size_t i;
    void **table = m->bindings.table;
    for (i = 0; i < m->bindings.size; i += 2) {
        if (table[i+1] != HT_NOTFOUND) {
            jl_serialize_value(s, (jl_value_t*)table[i]);
            jl_binding_t *b = (jl_binding_t*)table[i+1];
            jl_serialize_value(s, b->name);
            if (jl_docmeta_sym && b->name == jl_docmeta_sym && jl_options.strip_metadata)
                jl_serialize_value(s, jl_nothing);
            else
                jl_serialize_value(s, jl_atomic_load_relaxed(&b->value));
            jl_serialize_value(s, jl_atomic_load_relaxed(&b->globalref));
            jl_serialize_value(s, b->owner);
            jl_serialize_value(s, jl_atomic_load_relaxed(&b->ty));
        }
    }

    for (i = 0; i < m->usings.len; i++) {
        jl_serialize_value(s, (jl_value_t*)m->usings.items[i]);
    }
}

static jl_value_t *get_replaceable_field(jl_value_t **addr)
{
    jl_value_t *fld = (jl_value_t*)ptrhash_get(&field_replace, addr);
    if (fld == HT_NOTFOUND)
        return *addr;
    return fld;
}

#define NBOX_C 1024

static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v, int recursive)
{
    // ignore items that are given a special representation
    if (v == NULL || jl_is_symbol(v) || v == jl_nothing) {
        return;
    }
    else if (jl_typeis(v, jl_task_type)) {
        if (v == (jl_value_t*)s->ptls->root_task) {
            jl_serialize_value(s, ((jl_task_t*)v)->tls);
            return;
        }
    }
    else if (jl_typeis(v, jl_int64_type)) {
        int64_t i64 = *(int64_t*)v + NBOX_C / 2;
        if ((uint64_t)i64 < NBOX_C)
            return;
    }
    else if (jl_typeis(v, jl_int32_type)) {
        int32_t i32 = *(int32_t*)v + NBOX_C / 2;
        if ((uint32_t)i32 < NBOX_C)
            return;
    }
    else if (jl_typeis(v, jl_uint8_type)) {
        return;
    }
    arraylist_push(&object_worklist, (void*)((uintptr_t)v | recursive));
}

static void jl_serialize_value__(jl_serializer_state *s, jl_value_t *v, int recursive)
{
    void **bp = ptrhash_bp(&backref_table, v);
    if (*bp != HT_NOTFOUND) {
        return;
    }

    size_t item = ++backref_table_numel;
    assert(item < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "too many items to serialize");
    char *pos = (char*)HT_NOTFOUND + item;
    *bp = (void*)pos;

    // some values have special representations
    jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
    jl_serialize_value(s, t);

    if (t->layout->npointers == 0) {
        // skip it
    }
    else if (jl_is_svec(v)) {
        if (!recursive)
            return;
        size_t i, l = jl_svec_len(v);
        jl_value_t **data = jl_svec_data(v);
        for (i = 0; i < l; i++) {
            jl_serialize_value(s, data[i]);
        }
    }
    else if (jl_is_array(v)) {
        jl_array_t *ar = (jl_array_t*)v;
        jl_serialize_value(s, jl_typeof(ar));
        if (ar->flags.ptrarray) {
            size_t i, l = jl_array_len(ar);
            for (i = 0; i < l; i++) {
                jl_serialize_value(s, jl_array_ptr_ref(ar, i));
            }
        }
        else if (ar->flags.hasptr) {
            const char *data = (const char*)jl_array_data(ar);
            uint16_t elsz = ar->elsize;
            size_t i, l = jl_array_len(ar);
            jl_datatype_t *et = (jl_datatype_t*)jl_tparam0(jl_typeof(ar));
            size_t j, np = et->layout->npointers;
            for (i = 0; i < l; i++) {
                for (j = 0; j < np; j++) {
                    uint32_t ptr = jl_ptr_offset(et, j);
                    jl_value_t *fld = ((jl_value_t**)data)[ptr];
                    JL_GC_PROMISE_ROOTED(fld);
                    jl_serialize_value(s, fld);
                }
                data += elsz;
            }
        }
    }
    else if (jl_typeis(v, jl_module_type)) {
        jl_serialize_module(s, (jl_module_t*)v);
    }
    else if (jl_is_typename(v)) {
        jl_typename_t *tn = (jl_typename_t*)v;
        jl_serialize_value(s, tn->name);
        jl_serialize_value(s, tn->module);
        jl_serialize_value(s, tn->names);
        jl_serialize_value(s, tn->wrapper);
        jl_serialize_value(s, tn->Typeofwrapper);
        jl_serialize_value_(s, (jl_value_t*)tn->cache, 0);
        jl_serialize_value_(s, (jl_value_t*)tn->linearcache, 0);
        jl_serialize_value(s, tn->mt);
        jl_serialize_value(s, tn->partial);
    }
    else if (t->layout->nfields > 0) {
        char *data = (char*)jl_data_ptr(v);
        size_t i, np = t->layout->npointers;
        for (i = 0; i < np; i++) {
            uint32_t ptr = jl_ptr_offset(t, i);
            jl_value_t *fld = get_replaceable_field(&((jl_value_t**)data)[ptr]);
            jl_serialize_value(s, fld);
        }
    }
}

// Do a pre-order traversal of the to-serialize worklist, in the identical order
// to the calls to jl_serialize_value would occur in a purely recursive
// implementation, but without potentially running out of stack.
static void jl_serialize_reachable(jl_serializer_state *s)
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
        uintptr_t v = (uintptr_t)object_worklist.items[prevlen];
        int recursive = v & 1;
        v &= ~(uintptr_t)1; // untag v
        jl_serialize_value__(s, (jl_value_t*)v, recursive);
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

// Maybe encode a global variable. `gid` is the LLVM index, 0 if the object is not serialized
// in the generated code (and thus not a gvar from that standpoint, maybe only stored in the internal-data sysimg).
// `reloc_id` is the RefTags-encoded `target`.
static void record_gvar(jl_serializer_state *s, int gid, uintptr_t reloc_id) JL_NOTSAFEPOINT
{
    if (gid == 0)
        return;
    ios_ensureroom(s->gvar_record, gid * sizeof(uint32_t));
    ios_seek(s->gvar_record, (gid - 1) * sizeof(uint32_t));
    assert(reloc_id < UINT32_MAX);
    write_uint32(s->gvar_record, reloc_id);
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
    write_padding(s, sizeof(void*));
}

// Return the integer `id` for `v`. Generically this is looked up in `backref_table`,
// but symbols, small integers, and a couple of special items (`nothing` and the root Task)
// have special handling.
#define backref_id(s, v) _backref_id(s, (jl_value_t*)(v))
static uintptr_t _backref_id(jl_serializer_state *s, jl_value_t *v) JL_NOTSAFEPOINT
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
    if (idx == HT_NOTFOUND) {
        idx = ptrhash_get(&backref_table, v);
        assert(idx != HT_NOTFOUND && "object missed during jl_serialize_value pass");
    }
    return (char*)idx - 1 - (char*)HT_NOTFOUND;
}


// Save blank space in stream `s` for a pointer `fld`, storing both location and target
// in `relocs_list`.
static void write_pointerfield(jl_serializer_state *s, jl_value_t *fld) JL_NOTSAFEPOINT
{
    if (fld != NULL) {
        arraylist_push(&s->relocs_list, (void*)(uintptr_t)ios_pos(s->s));
        arraylist_push(&s->relocs_list, (void*)backref_id(s, fld));
    }
    write_pointer(s->s);
}

// Save blank space in stream `s` for a pointer `fld`, storing both location and target
// in `gctags_list`.
static void write_gctaggedfield(jl_serializer_state *s, uintptr_t ref) JL_NOTSAFEPOINT
{
    arraylist_push(&s->gctags_list, (void*)(uintptr_t)ios_pos(s->s));
    arraylist_push(&s->gctags_list, (void*)ref);
    write_pointer(s->s);
}

// Special handling from `jl_write_values` for modules
static void jl_write_module(jl_serializer_state *s, uintptr_t item, jl_module_t *m)
{
    size_t reloc_offset = ios_pos(s->s);
    size_t tot = sizeof(jl_module_t);
    ios_write(s->s, (char*)m, tot);     // raw memory dump of the `jl_module_t` structure

    // Handle the fields requiring special attention
    jl_module_t *newm = (jl_module_t*)&s->s->buf[reloc_offset];
    newm->name = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, name)));
    arraylist_push(&s->relocs_list, (void*)backref_id(s, m->name));
    newm->parent = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, parent)));
    arraylist_push(&s->relocs_list, (void*)backref_id(s, m->parent));
    newm->primary_world = jl_atomic_load_acquire(&jl_world_counter);

    // write out the bindings table as a list
    // immediately after jl_module_t
    // (the ptrhash will need to be recreated on load)
    size_t count = 0;
    size_t i;
    void **table = m->bindings.table;
    for (i = 0; i < m->bindings.size; i += 2) {
        if (table[i+1] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i+1];
            write_pointerfield(s, (jl_value_t*)table[i]);
            tot += sizeof(void*);
            write_gctaggedfield(s, (uintptr_t)BindingRef << RELOC_TAG_OFFSET);
            tot += sizeof(void*);
            size_t binding_reloc_offset = ios_pos(s->s);
            record_gvar(s, jl_get_llvm_gv(native_functions, (jl_value_t*)b),
                    ((uintptr_t)DataRef << RELOC_TAG_OFFSET) + binding_reloc_offset);
            write_pointerfield(s, (jl_value_t*)b->name);
            if (jl_docmeta_sym && b->name == jl_docmeta_sym && jl_options.strip_metadata)
                write_pointerfield(s, jl_nothing);
            else
                write_pointerfield(s, jl_atomic_load_relaxed(&b->value));
            write_pointerfield(s, jl_atomic_load_relaxed(&b->globalref));
            write_pointerfield(s, (jl_value_t*)b->owner);
            write_pointerfield(s, jl_atomic_load_relaxed(&b->ty));
            size_t flag_offset = offsetof(jl_binding_t, ty) + sizeof(b->ty);
            ios_write(s->s, (char*)b + flag_offset, sizeof(*b) - flag_offset);
            tot += sizeof(jl_binding_t);
            count += 1;
        }
    }
    assert(ios_pos(s->s) - reloc_offset == tot);
    newm = (jl_module_t*)&s->s->buf[reloc_offset]; // buf might have been reallocated
    newm->bindings.size = count; // stash the count in newm->size
    newm->bindings.table = NULL;
    memset(&newm->bindings._space, 0, sizeof(newm->bindings._space));

    // write out the usings list
    memset(&newm->usings._space, 0, sizeof(newm->usings._space));
    if (m->usings.items == &m->usings._space[0]) {
        newm->usings.items = (void**)offsetof(jl_module_t, usings._space);
        arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings.items)));
        arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item));
        size_t i;
        for (i = 0; i < m->usings.len; i++) {
            arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings._space[i])));
            arraylist_push(&s->relocs_list, (void*)backref_id(s, m->usings._space[i]));
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
}

#if 0
static size_t jl_sort_size(jl_datatype_t *dt)
{
    if (dt == jl_simplevector_type)
        return SIZE_MAX - 5;
    if (dt == jl_string_type)
        return SIZE_MAX - 4;
    if (dt->name == jl_array_typename)
        return SIZE_MAX - 3;
    if (dt == jl_datatype_type)
        return SIZE_MAX - 2;
    if (dt == jl_module_type)
        return SIZE_MAX - 1;
    return jl_datatype_size(dt);
}
#endif

// Used by `qsort` to order `backref_table` by `id`
static int sysimg_sort_order(const void *pa, const void *pb)
{
    uintptr_t sa = ((uintptr_t*)pa)[1];
    uintptr_t sb = ((uintptr_t*)pb)[1];
    return (sa > sb ? 1 : (sa < sb ? -1 : 0));
#if 0
    jl_value_t *a = *(jl_value_t**)pa;
    jl_datatype_t *tya = (jl_datatype_t*)jl_typeof(a);
    size_t sa = jl_sort_size(tya);
    jl_value_t *b = *(jl_value_t**)pb;
    jl_datatype_t *tyb = (jl_datatype_t*)jl_typeof(b);
    size_t sb = jl_sort_size(tyb);
    if (sa == sb) {
        sa = tya->uid;
        sb = tyb->uid;
    }
    return (sa > sb ? 1 : (sa < sb ? -1 : 0));
#endif
}

jl_value_t *jl_find_ptr = NULL;
// The main function for serializing all the items queued in `backref_table`
static void jl_write_values(jl_serializer_state *s)
{
    arraylist_t objects_list;
    arraylist_new(&objects_list, backref_table_numel * 2);

    arraylist_new(&layout_table, 0);
    arraylist_grow(&layout_table, backref_table_numel);
    memset(layout_table.items, 0, backref_table_numel * sizeof(void*));

    // Order `backref_table` by `id`
    size_t i, len = backref_table.size;
    void **p = backref_table.table;
    for (i = 0; i < len; i += 2) {
        char *reloc_id = (char*)p[i + 1];
        if (reloc_id != HT_NOTFOUND) {
            jl_value_t *v = (jl_value_t*)p[i];
            uintptr_t item = reloc_id - 1 - (char*)HT_NOTFOUND;
            objects_list.items[objects_list.len++] = (void*)v;
            objects_list.items[objects_list.len++] = (void*)item;
        }
    }
    assert(backref_table_numel * 2 == objects_list.len);
    qsort(objects_list.items, backref_table_numel, sizeof(void*) * 2, sysimg_sort_order);

    // Serialize all entries
    for (i = 0, len = backref_table_numel * 2; i < len; i += 2) {
        jl_value_t *v = (jl_value_t*)objects_list.items[i];           // the object
        JL_GC_PROMISE_ROOTED(v);
        uintptr_t item = (uintptr_t)objects_list.items[i + 1];        // the id
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        assert((t->instance == NULL || t->instance == v) && "detected singleton construction corruption");
        // realign stream to expected gc alignment (16 bytes)
        uintptr_t skip_header_pos = ios_pos(s->s) + sizeof(jl_taggedvalue_t);
        write_padding(s->s, LLT_ALIGN(skip_header_pos, 16) - skip_header_pos);
        // write header
        write_gctaggedfield(s, backref_id(s, t));
        size_t reloc_offset = ios_pos(s->s);
        assert(item < layout_table.len && layout_table.items[item] == NULL);
        layout_table.items[item] = (void*)reloc_offset;               // store the inverse mapping of `backref_table` (`id` => object)
        record_gvar(s, jl_get_llvm_gv(native_functions, v), ((uintptr_t)DataRef << RELOC_TAG_OFFSET) + reloc_offset);

        // write data
        if (jl_is_cpointer(v)) {
            write_pointer(s->s);
        }
        else if (jl_is_array(v)) {
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
                    // reset Ptr elements to C_NULL
                    size_t i;
                    for (i = 0; i < alen; i++)
                        write_pointer(s->const_data);
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
                write_padding(s->s, padding_amt);
                headersize += padding_amt;
                newa->data = (void*)headersize; // relocation offset
                arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_array_t, data))); // relocation location
                arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item)); // relocation target
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
                            jl_value_t *fld = *(jl_value_t**)&data[offset];
                            if (fld != NULL) {
                                arraylist_push(&s->relocs_list, (void*)(uintptr_t)(reloc_offset + headersize + offset)); // relocation location
                                arraylist_push(&s->relocs_list, (void*)backref_id(s, fld)); // relocation target
                                memset(&s->s->buf[reloc_offset + headersize + offset], 0, sizeof(fld)); // relocation offset (none)
                            }
                            else {
                                assert(*(jl_value_t**)&s->s->buf[reloc_offset + headersize + offset] == NULL);
                            }
                        }
                    }
                }
                else {
                    size_t i;
                    for (i = 0; i < alen; i++) {
                        jl_value_t *e = jl_array_ptr_ref(v, i);
                        write_pointerfield(s, e);
                    }
                }
            }
        }
        else if (jl_typeis(v, jl_module_type)) {
            jl_write_module(s, item, (jl_module_t*)v);
            // will need to recreate the binding table for this
            arraylist_push(&reinit_list, (void*)item);
            arraylist_push(&reinit_list, (void*)2);
        }
        else if (jl_typeis(v, jl_task_type)) {
            jl_error("Task cannot be serialized");
        }
        else if (jl_is_svec(v)) {
            ios_write(s->s, (char*)v, sizeof(void*));
            size_t i, l = jl_svec_len(v);
            assert(l > 0 || (jl_svec_t*)v == jl_emptysvec);
            for (i = 0; i < l; i++) {
                write_pointerfield(s, jl_svecref(v, i));
            }
        }
        else if (jl_is_string(v)) {
            ios_write(s->s, (char*)v, sizeof(void*) + jl_string_len(v));
            write_uint8(s->s, '\0'); // null-terminated strings for easier C-compatibility
        }
        else if (jl_datatype_nfields(t) == 0) {
            assert(t->layout->npointers == 0);
            if (t->size > 0)
                ios_write(s->s, (char*)v, t->size);
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
                if (t->name->mutabl && jl_is_cpointer_type(jl_field_type(t, i))) {
                    // reset Ptr fields to C_NULL
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
                jl_value_t *fld = get_replaceable_field((jl_value_t**)&data[offset]);
                if (fld != NULL) {
                    arraylist_push(&s->relocs_list, (void*)(uintptr_t)(offset + reloc_offset)); // relocation location
                    arraylist_push(&s->relocs_list, (void*)backref_id(s, fld)); // relocation target
                    memset(&s->s->buf[offset + reloc_offset], 0, sizeof(fld)); // relocation offset (none)
                }
            }

            // A few objects need additional handling beyond the generic serialization above
            if (jl_is_method(v)) {
                write_padding(s->s, sizeof(jl_method_t) - tot);
                if (((jl_method_t*)v)->ccallable) {
                    arraylist_push(&ccallable_list, (void*)item);
                    arraylist_push(&ccallable_list, (void*)3);
                }
            }
            else if (jl_is_code_instance(v)) {
                // Handle the native-code pointers
                jl_code_instance_t *m = (jl_code_instance_t*)v;
                jl_code_instance_t *newm = (jl_code_instance_t*)&s->s->buf[reloc_offset];

                newm->invoke = NULL;
                newm->isspecsig = 0;
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
                                    write_uint32(s->fptr_record, ~reloc_offset);
#ifdef _P64
                                    write_padding(s->fptr_record, 4);
#endif
                                }
                                if (specfptr_id) {
                                    assert(specfptr_id > invokeptr_id && specfptr_id > 0);
                                    ios_ensureroom(s->fptr_record, specfptr_id * sizeof(void*));
                                    ios_seek(s->fptr_record, (specfptr_id - 1) * sizeof(void*));
                                    write_uint32(s->fptr_record, reloc_offset);
#ifdef _P64
                                    write_padding(s->fptr_record, 4);
#endif
                                }
                            }
                        }
                    }
                }
                newm->invoke = NULL; // relocation offset
                if (fptr_id != JL_API_NULL) {
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_code_instance_t, invoke))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)FunctionRef << RELOC_TAG_OFFSET) + fptr_id)); // relocation target
                }
                if (builtin_id >= 2) {
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_code_instance_t, specptr.fptr))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)BuiltinFunctionRef << RELOC_TAG_OFFSET) + builtin_id - 2)); // relocation target
                }
            }
            else if (jl_is_datatype(v)) {
                jl_datatype_t *dt = (jl_datatype_t*)v;
                jl_datatype_t *newdt = (jl_datatype_t*)&s->s->buf[reloc_offset];
                if (dt->layout != NULL) {
                    size_t nf = dt->layout->nfields;
                    size_t np = dt->layout->npointers;
                    size_t fieldsize = jl_fielddesc_size(dt->layout->fielddesc_type);
                    char *flddesc = (char*)dt->layout;
                    size_t fldsize = sizeof(jl_datatype_layout_t) + nf * fieldsize;
                    if (dt->layout->first_ptr != -1)
                        fldsize += np << dt->layout->fielddesc_type;
                    uintptr_t layout = LLT_ALIGN(ios_pos(s->const_data), sizeof(void*));
                    write_padding(s->const_data, layout - ios_pos(s->const_data)); // realign stream
                    newdt->layout = NULL; // relocation offset
                    layout /= sizeof(void*);
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_datatype_t, layout))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + layout)); // relocation target
                    ios_write(s->const_data, flddesc, fldsize);
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
            else if (((jl_datatype_t*)(jl_typeof(v)))->name == jl_idtable_typename) {
                // will need to rehash this, later (after types are fully constructed)
                arraylist_push(&reinit_list, (void*)item);
                arraylist_push(&reinit_list, (void*)1);
            }
            else {
                write_padding(s->s, t->size - tot);
            }
        }
    }
}


// Record all symbols that get referenced by the generated code
// and queue them for pointer relocation
static void jl_write_gv_syms(jl_serializer_state *s, jl_sym_t *v)
{
    // since symbols are static, they might not have had a
    // reference anywhere in the code image other than here
    int32_t gv = jl_get_llvm_gv(native_functions, (jl_value_t*)v);
    if (gv != 0) {
        uintptr_t item = backref_id(s, v);
        assert(item >> RELOC_TAG_OFFSET == SymbolRef);
        record_gvar(s, gv, item);
    }
    if (v->left)
        jl_write_gv_syms(s, v->left);
    if (v->right)
        jl_write_gv_syms(s, v->right);
}

// Record all hardcoded-tagged items that get referenced by
// the generated code and queue them for pointer relocation
static void jl_write_gv_tagref(jl_serializer_state *s, jl_value_t *v)
{
    int32_t gv = jl_get_llvm_gv(native_functions, (jl_value_t*)v);
    if (gv != 0) {
        uintptr_t item = backref_id(s, v);
        assert(item >> RELOC_TAG_OFFSET == TagRef);
        record_gvar(s, gv, item);
    }
}
static void jl_write_gv_tagrefs(jl_serializer_state *s)
{
    // this also ensures all objects referenced in the code have
    // references in the system image to their global variable
    // since codegen knows that some integer boxes are static,
    // they might not have had a reference anywhere in the code
    // image other than here
    size_t i;
    jl_write_gv_tagref(s, (jl_value_t*)s->ptls->root_task);
    jl_write_gv_tagref(s, s->ptls->root_task->tls);
    jl_write_gv_tagref(s, jl_nothing);
    for (i = 0; i < NBOX_C; i++) {
        jl_write_gv_tagref(s, jl_box_int32((int32_t)i - NBOX_C / 2));
        jl_write_gv_tagref(s, jl_box_int64((int64_t)i - NBOX_C / 2));
    }
    for (i = 0; i < 256; i++) {
        jl_write_gv_tagref(s, jl_box_uint8(i));
    }
}

static inline uint32_t load_uint32(uintptr_t *base)
{
    uint32_t v = jl_load_unaligned_i32((void*)*base);
    *base += 4;
    return v;
}


// In deserialization, create Symbols and set up the
// index for backreferencing
static void jl_read_symbols(jl_serializer_state *s)
{
    assert(deser_sym.len == nsym_tag);
    uintptr_t base = (uintptr_t)&s->symbols->buf[0];
    uintptr_t end = base + s->symbols->size;
    while (base < end) {
        uint32_t len = load_uint32(&base);
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
        return reloc_base + reloc_offset;
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
        case BindingRef:
            assert(offset == 0 && "corrupt relocation offset");
            break;
        case BuiltinFunctionRef:
            assert(offset < sizeof(id_to_fptrs) / sizeof(*id_to_fptrs) && "unknown function pointer id");
            break;
        case FunctionRef:
            assert(offset < JL_API_MAX && "unknown function pointer id");
            break;
        case DataRef:
        default:
            assert(0 && "corrupt relocation item id");
            abort();
        }
#endif
        return reloc_item; // pre-composed relocation + offset
    }
}

// Compute target location at deserialization
static inline uintptr_t get_item_for_reloc(jl_serializer_state *s, uintptr_t base, size_t size, uint32_t reloc_id)
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
    case BindingRef:
        return jl_buff_tag | GC_OLD_MARKED;
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
    case BuiltinFunctionRef:
        assert(offset < sizeof(id_to_fptrs) / sizeof(*id_to_fptrs) && "unknown function pointer ID");
        return (uintptr_t)id_to_fptrs[offset];
    case FunctionRef:
        switch ((jl_callingconv_t)offset) {
        case JL_API_BOXED:
            if (sysimg_fptrs.base)
                return (uintptr_t)jl_fptr_args;
            JL_FALLTHROUGH;
        case JL_API_WITH_PARAMETERS:
            if (sysimg_fptrs.base)
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
    }
    abort();
}


static void jl_write_skiplist(ios_t *s, char *base, size_t size, arraylist_t *list)
{
    size_t i;
    for (i = 0; i < list->len; i += 2) {
        size_t pos = (size_t)list->items[i];
        size_t item = (size_t)list->items[i + 1];
        uintptr_t *pv = (uintptr_t*)(base + pos);
        assert(pos < size && pos != 0);
        *pv = get_reloc_for_item(item, *pv);
        // record pos in relocations list
        // TODO: save space by using delta-compression
        assert(pos < UINT32_MAX);
        write_uint32(s, pos);
    }
    write_uint32(s, 0);
}


static void jl_write_relocations(jl_serializer_state *s)
{
    char *base = &s->s->buf[0];
    jl_write_skiplist(s->relocs, base, s->s->size, &s->gctags_list);
    jl_write_skiplist(s->relocs, base, s->s->size, &s->relocs_list);
}


static void jl_read_relocations(jl_serializer_state *s, uint8_t bits)
{
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    size_t size = s->s->size;
    while (1) {
        uintptr_t val = (uintptr_t)&s->relocs->buf[s->relocs->bpos];
        uint32_t offset = load_uint32(&val);
        s->relocs->bpos += sizeof(uint32_t);
        if (offset == 0)
            break;
        uintptr_t *pv = (uintptr_t*)(base + offset);
        uintptr_t v = *pv;
        v = get_item_for_reloc(s, base, size, v);
        *pv = v | bits;
    }
}

static char* sysimg_base;
static char* sysimg_relocs;
void gc_sweep_sysimg(void)
{
    uintptr_t base = (uintptr_t)sysimg_base;
    uintptr_t relocs = (uintptr_t)sysimg_relocs;
    if (relocs == 0)
        return;
    while (1) {
        uint32_t offset = load_uint32(&relocs);
        if (offset == 0)
            break;
        jl_taggedvalue_t *o = (jl_taggedvalue_t*)(base + offset);
        o->bits.gc = GC_OLD;
    }
}

#define jl_write_value(s, v) _jl_write_value((s), (jl_value_t*)(v))
static void _jl_write_value(jl_serializer_state *s, jl_value_t *v)
{
    if (v == NULL) {
        write_uint32(s->s, 0);
        return;
    }
    uintptr_t item = backref_id(s, v);
    uintptr_t reloc = get_reloc_for_item(item, 0);
    assert(reloc < UINT32_MAX);
    write_uint32(s->s, reloc);
}


static jl_value_t *jl_read_value(jl_serializer_state *s)
{
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    size_t size = s->s->size;
    uintptr_t val = base + s->s->bpos;
    uint32_t offset = load_uint32(&val);
    s->s->bpos += sizeof(uint32_t);
    if (offset == 0)
        return NULL;
    return (jl_value_t*)get_item_for_reloc(s, base, size, offset);
}


static void jl_update_all_fptrs(jl_serializer_state *s)
{
    jl_sysimg_fptrs_t fvars = sysimg_fptrs;
    // make these NULL now so we skip trying to restore GlobalVariable pointers later
    sysimg_gvars_base = NULL;
    sysimg_fptrs.base = NULL;
    if (fvars.base == NULL)
        return;
    int sysimg_fvars_max = s->fptr_record->size / sizeof(void*);
    size_t i;
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    // These will become MethodInstance references, but they start out as a list of
    // offsets into `s` for CodeInstances
    jl_method_instance_t **linfos = (jl_method_instance_t**)&s->fptr_record->buf[0];
    uint32_t clone_idx = 0;
    for (i = 0; i < sysimg_fvars_max; i++) {
        uintptr_t val = (uintptr_t)&linfos[i];
        uint32_t offset = load_uint32(&val);
        linfos[i] = NULL;
        if (offset != 0) {
            int specfunc = 1;
            if (offset & ((uintptr_t)1 << (8 * sizeof(uint32_t) - 1))) {
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
                codeinst->isspecsig = 1; // TODO: set only if confirmed to be true
            }
            else {
                codeinst->invoke = (jl_callptr_t)fptr;
            }
        }
    }
    // Tell LLVM about the native code
    jl_register_fptrs(sysimage_base, &fvars, linfos, sysimg_fvars_max);
}


// Pointer relocation for native-code referenced global variables
static void jl_update_all_gvars(jl_serializer_state *s)
{
    if (sysimg_gvars_base == NULL)
        return;
    size_t gvname_index = 0;
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    size_t size = s->s->size;
    uintptr_t gvars = (uintptr_t)&s->gvar_record->buf[0];
    uintptr_t end = gvars + s->gvar_record->size;
    while (gvars < end) {
        uint32_t offset = load_uint32(&gvars);
        if (offset) {
            uintptr_t v = get_item_for_reloc(s, base, size, offset);
            *sysimg_gvars(sysimg_gvars_base, gvname_index) = v;
        }
        gvname_index += 1;
    }
}


// Reinitialization
static void jl_finalize_serializer(jl_serializer_state *s, arraylist_t *list)
{
    size_t i, l;

    // record list of reinitialization functions
    l = list->len;
    for (i = 0; i < l; i += 2) {
        size_t item = (size_t)list->items[i];
        size_t reloc_offset = (size_t)layout_table.items[item];
        assert(reloc_offset != 0);
        write_uint32(s->s, (uint32_t)reloc_offset);
        write_uint32(s->s, (uint32_t)((uintptr_t)list->items[i + 1]));
    }
    write_uint32(s->s, 0);
}


static void jl_reinit_item(jl_value_t *v, int how) JL_GC_DISABLED
{
    switch (how) {
        case 1: { // rehash IdDict
            jl_array_t **a = (jl_array_t**)v;
            assert(jl_is_array(*a));
            // Assume *a don't need a write barrier
            *a = jl_idtable_rehash(*a, jl_array_len(*a));
            jl_gc_wb(v, *a);
            break;
        }
        case 2: { // rebuild the binding table for module v
            jl_module_t *mod = (jl_module_t*)v;
            assert(jl_is_module(mod));
            size_t nbindings = mod->bindings.size;
            htable_new(&mod->bindings, nbindings);
            struct binding {
                jl_sym_t *asname;
                uintptr_t tag;
                jl_binding_t b;
            } *b;
            b = (struct binding*)&mod[1];
            while (nbindings > 0) {
                ptrhash_put(&mod->bindings, b->asname, &b->b);
                b += 1;
                nbindings -= 1;
            }
            if (mod->usings.items != &mod->usings._space[0]) {
                void **newitems = (void**)malloc_s(mod->usings.max * sizeof(void*));
                memcpy(newitems, mod->usings.items, mod->usings.len * sizeof(void*));
                mod->usings.items = newitems;
            }
            break;
        }
        case 3: { // install ccallable entry point in JIT
            jl_svec_t *sv = ((jl_method_t*)v)->ccallable;
            int success = jl_compile_extern_c(NULL, NULL, jl_sysimg_handle, jl_svecref(sv, 0), jl_svecref(sv, 1));
            assert(success); (void)success;
            break;
        }
        default:
            assert(0 && "corrupt deserialization state");
            abort();
    }
}


static void jl_finalize_deserializer(jl_serializer_state *s) JL_GC_DISABLED
{
    // run reinitialization functions
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    while (1) {
        size_t offset = read_uint32(s->s);
        if (offset == 0)
            break;
        jl_value_t *v = (jl_value_t*)(base + offset);
        jl_reinit_item(v, read_uint32(s->s));
    }
}



// Code below helps slim down the images
static void jl_scan_type_cache_gv(jl_serializer_state *s, jl_svec_t *cache)
{
    size_t l = jl_svec_len(cache), i;
    for (i = 0; i < l; i++) {
        jl_value_t *ti = jl_svecref(cache, i);
        if (ti == NULL || ti == jl_nothing)
            continue;
        if (jl_get_llvm_gv(native_functions, ti)) {
            jl_serialize_value(s, ti);
        }
        else if (jl_is_datatype(ti)) {
            jl_value_t *singleton = ((jl_datatype_t*)ti)->instance;
            if (singleton && jl_get_llvm_gv(native_functions, singleton))
                jl_serialize_value(s, ti);
        }
    }
}

// remove cached types not referenced in the stream
static void jl_prune_type_cache_hash(jl_svec_t *cache)
{
    size_t l = jl_svec_len(cache), i;
    for (i = 0; i < l; i++) {
        jl_value_t *ti = jl_svecref(cache, i);
        if (ti == NULL || ti == jl_nothing)
            continue;
        if (ptrhash_get(&backref_table, ti) == HT_NOTFOUND)
            jl_svecset(cache, i, jl_nothing);
    }
}

static void jl_prune_type_cache_linear(jl_svec_t *cache)
{
    size_t l = jl_svec_len(cache), ins = 0, i;
    for (i = 0; i < l; i++) {
        jl_value_t *ti = jl_svecref(cache, i);
        if (ti == NULL)
            break;
        if (ptrhash_get(&backref_table, ti) != HT_NOTFOUND)
            jl_svecset(cache, ins++, ti);
    }
    if (i > ins) {
        memset(&jl_svec_data(cache)[ins], 0, (i - ins) * sizeof(jl_value_t*));
    }
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

static void record_field_change(jl_value_t **addr, jl_value_t *newval)
{
    ptrhash_put(&field_replace, (void*)addr, newval);
}

static void strip_specializations_(jl_method_instance_t *mi)
{
    assert(jl_is_method_instance(mi));
    jl_code_instance_t *codeinst = mi->cache;
    while (codeinst) {
        if (codeinst->inferred && codeinst->inferred != jl_nothing) {
            if (jl_options.strip_ir) {
                record_field_change(&codeinst->inferred, jl_nothing);
            }
            else if (jl_options.strip_metadata) {
                codeinst->inferred = strip_codeinfo_meta(mi->def.method, codeinst->inferred, 0);
                jl_gc_wb(codeinst, codeinst->inferred);
            }
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    if (jl_options.strip_ir) {
        record_field_change(&mi->uninferred, NULL);
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
    return 1;
}

static int strip_all_codeinfos_(jl_methtable_t *mt, void *_env)
{
    return jl_typemap_visitor(mt->defs, strip_all_codeinfos__, NULL);
}

static void jl_strip_all_codeinfos(void)
{
    jl_foreach_reachable_mtable(strip_all_codeinfos_, NULL);
}

// Method roots created during sysimg construction are exempted from
// triggering non-relocatability of compressed CodeInfos.
// Set the number of such roots in each method when the sysimg is
// serialized.
static int set_nroots_sysimg__(jl_typemap_entry_t *def, void *_env)
{
    jl_method_t *m = def->func.method;
    m->nroots_sysimg = m->roots ? jl_array_len(m->roots) : 0;
    return 1;
}

static int set_nroots_sysimg_(jl_methtable_t *mt, void *_env)
{
    return jl_typemap_visitor(mt->defs, set_nroots_sysimg__, NULL);
}

static void jl_set_nroots_sysimg(void)
{
    jl_foreach_reachable_mtable(set_nroots_sysimg_, NULL);
}

// --- entry points ---

static void jl_init_serializer2(int);
static void jl_cleanup_serializer2(void);

static void jl_save_system_image_to_stream(ios_t *f) JL_GC_DISABLED
{
    jl_gc_collect(JL_GC_FULL);
    jl_gc_collect(JL_GC_INCREMENTAL);   // sweep finalizers
    JL_TIMING(SYSIMG_DUMP);

    htable_new(&field_replace, 10000);
    // strip metadata and IR when requested
    if (jl_options.strip_metadata || jl_options.strip_ir)
        jl_strip_all_codeinfos();
    jl_set_nroots_sysimg();

    int en = jl_gc_enable(0);
    jl_init_serializer2(1);
    htable_reset(&backref_table, 250000);
    arraylist_new(&reinit_list, 0);
    arraylist_new(&ccallable_list, 0);
    arraylist_new(&object_worklist, 0);
    backref_table_numel = 0;
    ios_t sysimg, const_data, symbols, relocs, gvar_record, fptr_record;
    ios_mem(&sysimg,     1000000);
    ios_mem(&const_data,  100000);
    ios_mem(&symbols,     100000);
    ios_mem(&relocs,      100000);
    ios_mem(&gvar_record, 100000);
    ios_mem(&fptr_record, 100000);
    jl_serializer_state s;
    s.s = &sysimg;
    s.const_data = &const_data;
    s.symbols = &symbols;
    s.relocs = &relocs;
    s.gvar_record = &gvar_record;
    s.fptr_record = &fptr_record;
    s.ptls = jl_current_task->ptls;
    arraylist_new(&s.relocs_list, 0);
    arraylist_new(&s.gctags_list, 0);
    jl_value_t **const*const tags = get_tags();

    // empty!(Core.ARGS)
    if (jl_core_module != NULL) {
        jl_array_t *args = (jl_array_t*)jl_get_global(jl_core_module, jl_symbol("ARGS"));
        if (args != NULL) {
            jl_array_del_end(args, jl_array_len(args));
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
        for (i = 0; tags[i] != NULL; i++) {
            jl_value_t *tag = *tags[i];
            jl_serialize_value(&s, tag);
        }
        jl_serialize_reachable(&s);
        // step 1.1: check for values only found in the generated code
        arraylist_t typenames;
        arraylist_new(&typenames, 0);
        for (i = 0; i < backref_table.size; i += 2) {
            jl_typename_t *tn = (jl_typename_t*)backref_table.table[i];
            if (tn == HT_NOTFOUND || !jl_is_typename(tn))
                continue;
            arraylist_push(&typenames, tn);
        }
        for (i = 0; i < typenames.len; i++) {
            jl_typename_t *tn = (jl_typename_t*)typenames.items[i];
            jl_scan_type_cache_gv(&s, tn->cache);
            jl_scan_type_cache_gv(&s, tn->linearcache);
        }
        jl_serialize_reachable(&s);
        // step 1.2: prune (garbage collect) some special weak references from
        // built-in type caches
        for (i = 0; i < typenames.len; i++) {
            jl_typename_t *tn = (jl_typename_t*)typenames.items[i];
            jl_prune_type_cache_hash(tn->cache);
            jl_prune_type_cache_linear(tn->linearcache);
        }
        arraylist_free(&typenames);
    }

    { // step 2: build all the sysimg sections
        write_padding(&sysimg, sizeof(uint32_t));
        jl_write_values(&s);
        jl_write_relocations(&s);
        jl_write_gv_syms(&s, jl_get_root_symbol());
        jl_write_gv_tagrefs(&s);
    }

    if (sysimg.size > ((uintptr_t)1 << RELOC_TAG_OFFSET) ||
        const_data.size > ((uintptr_t)1 << RELOC_TAG_OFFSET)*sizeof(void*)) {
        jl_printf(JL_STDERR, "ERROR: system image too large\n");
        jl_exit(1);
    }

    // step 3: combine all of the sections into one file
    write_uint32(f, sysimg.size - sizeof(uint32_t));
    ios_seek(&sysimg, sizeof(uint32_t));
    ios_copyall(f, &sysimg);
    ios_close(&sysimg);

    write_uint32(f, const_data.size);
    // realign stream to max-alignment for data
    write_padding(f, LLT_ALIGN(ios_pos(f), 16) - ios_pos(f));
    ios_seek(&const_data, 0);
    ios_copyall(f, &const_data);
    ios_close(&const_data);

    write_uint32(f, symbols.size);
    ios_seek(&symbols, 0);
    ios_copyall(f, &symbols);
    ios_close(&symbols);

    write_uint32(f, relocs.size);
    ios_seek(&relocs, 0);
    ios_copyall(f, &relocs);
    ios_close(&relocs);

    write_uint32(f, gvar_record.size);
    ios_seek(&gvar_record, 0);
    ios_copyall(f, &gvar_record);
    ios_close(&gvar_record);

    write_uint32(f, fptr_record.size);
    ios_seek(&fptr_record, 0);
    ios_copyall(f, &fptr_record);
    ios_close(&fptr_record);

    { // step 4: record locations of special roots
        s.s = f;
        size_t i;
        for (i = 0; tags[i] != NULL; i++) {
            jl_value_t *tag = *tags[i];
            jl_write_value(&s, tag);
        }
        jl_write_value(&s, s.ptls->root_task->tls);
        write_uint32(f, jl_get_gs_ctr());
        write_uint32(f, jl_atomic_load_acquire(&jl_world_counter));
        write_uint32(f, jl_typeinf_world);
        jl_finalize_serializer(&s, &reinit_list);
        jl_finalize_serializer(&s, &ccallable_list);
    }

    assert(object_worklist.len == 0);
    arraylist_free(&object_worklist);
    arraylist_free(&layout_table);
    arraylist_free(&reinit_list);
    arraylist_free(&ccallable_list);
    arraylist_free(&s.relocs_list);
    arraylist_free(&s.gctags_list);
    htable_free(&field_replace);
    jl_cleanup_serializer2();

    jl_gc_enable(en);
}

JL_DLLEXPORT ios_t *jl_create_system_image(void *_native_data)
{
    ios_t *f = (ios_t*)malloc_s(sizeof(ios_t));
    ios_mem(f, 0);
    native_functions = _native_data;
    jl_save_system_image_to_stream(f);
    return f;
}

JL_DLLEXPORT size_t ios_write_direct(ios_t *dest, ios_t *src);
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
    sysimg_fptrs = jl_init_processor_sysimg(handle);
}

static void jl_restore_system_image_from_stream(ios_t *f) JL_GC_DISABLED
{
    JL_TIMING(SYSIMG_LOAD);
    int en = jl_gc_enable(0);
    jl_init_serializer2(0);
    ios_t sysimg, const_data, symbols, relocs, gvar_record, fptr_record;
    jl_serializer_state s;
    s.s = NULL;
    s.const_data = &const_data;
    s.symbols = &symbols;
    s.relocs = &relocs;
    s.gvar_record = &gvar_record;
    s.fptr_record = &fptr_record;
    s.ptls = jl_current_task->ptls;
    arraylist_new(&s.relocs_list, 0);
    arraylist_new(&s.gctags_list, 0);
    jl_value_t **const*const tags = get_tags();

    // step 1: read section map
    assert(ios_pos(f) == 0 && f->bm == bm_mem);
    size_t sizeof_sysimg = read_uint32(f);
    ios_static_buffer(&sysimg, f->buf, sizeof_sysimg + sizeof(uint32_t));
    ios_skip(f, sizeof_sysimg);

    size_t sizeof_constdata = read_uint32(f);
    // realign stream to max-alignment for data
    ios_seek(f, LLT_ALIGN(ios_pos(f), 16));
    ios_static_buffer(&const_data, f->buf + f->bpos, sizeof_constdata);
    ios_skip(f, sizeof_constdata);

    size_t sizeof_symbols = read_uint32(f);
    ios_static_buffer(&symbols, f->buf + f->bpos, sizeof_symbols);
    ios_skip(f, sizeof_symbols);

    size_t sizeof_relocations = read_uint32(f);
    assert(!ios_eof(f));
    ios_static_buffer(&relocs, f->buf + f->bpos, sizeof_relocations);
    ios_skip(f, sizeof_relocations);

    size_t sizeof_gvar_record = read_uint32(f);
    assert(!ios_eof(f));
    ios_static_buffer(&gvar_record, f->buf + f->bpos, sizeof_gvar_record);
    ios_skip(f, sizeof_gvar_record);

    size_t sizeof_fptr_record = read_uint32(f);
    assert(!ios_eof(f));
    ios_static_buffer(&fptr_record, f->buf + f->bpos, sizeof_fptr_record);
    ios_skip(f, sizeof_fptr_record);

    // step 2: get references to special values
    s.s = f;
    size_t i;
    for (i = 0; tags[i] != NULL; i++) {
        jl_value_t **tag = tags[i];
        *tag = jl_read_value(&s);
    }
    // set typeof extra-special values now that we have the type set by tags above
    jl_astaggedvalue(jl_current_task)->header = (uintptr_t)jl_task_type | jl_astaggedvalue(jl_current_task)->header;
    jl_astaggedvalue(jl_nothing)->header = (uintptr_t)jl_nothing_type | jl_astaggedvalue(jl_nothing)->header;
    s.ptls->root_task->tls = jl_read_value(&s);
    jl_gc_wb(s.ptls->root_task, s.ptls->root_task->tls);
    jl_init_int32_int64_cache();
    jl_init_box_caches();

    uint32_t gs_ctr = read_uint32(f);
    jl_atomic_store_release(&jl_world_counter, read_uint32(f));
    jl_typeinf_world = read_uint32(f);
    jl_set_gs_ctr(gs_ctr);
    s.s = NULL;

    // step 3: apply relocations
    assert(!ios_eof(f));
    jl_read_symbols(&s);
    ios_close(&symbols);

    sysimg_base = &sysimg.buf[0];
    sysimg_relocs = &relocs.buf[0];
    jl_gc_set_permalloc_region((void*)sysimg_base, (void*)(sysimg_base + sysimg.size));

    s.s = &sysimg;
    jl_read_relocations(&s, GC_OLD_MARKED); // gctags
    size_t sizeof_tags = ios_pos(&relocs);
    (void)sizeof_tags;
    jl_read_relocations(&s, 0); // general relocs
    ios_close(&relocs);
    ios_close(&const_data);
    jl_update_all_gvars(&s); // gvars relocs
    ios_close(&gvar_record);
    s.s = NULL;

    s.s = f;
    // reinit items except ccallables
    jl_finalize_deserializer(&s);
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

    s.s = &sysimg;
    jl_init_codegen();
    jl_update_all_fptrs(&s); // fptr relocs and registration
    // reinit ccallables, which require codegen to be initialized
    s.s = f;
    jl_finalize_deserializer(&s);

    ios_close(&fptr_record);
    ios_close(&sysimg);
    s.s = NULL;

    jl_gc_reset_alloc_count();
    jl_gc_enable(en);
    jl_cleanup_serializer2();
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
        ios_static_buffer(&f, sysimg, len);
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

// --- init ---

static void jl_init_serializer2(int for_serialize)
{
    if (for_serialize) {
        htable_new(&symbol_table, 0);
        htable_new(&fptr_to_id, sizeof(id_to_fptrs) / sizeof(*id_to_fptrs));
        htable_new(&backref_table, 0);
        uintptr_t i;
        for (i = 0; id_to_fptrs[i] != NULL; i++) {
            ptrhash_put(&fptr_to_id, (void*)(uintptr_t)id_to_fptrs[i], (void*)(i + 2));
        }
    }
    else {
        arraylist_new(&deser_sym, 0);
    }
    nsym_tag = 0;
}

static void jl_cleanup_serializer2(void)
{
    htable_reset(&symbol_table, 0);
    htable_reset(&fptr_to_id, 0);
    htable_reset(&backref_table, 0);
    arraylist_free(&deser_sym);
}

#ifdef __cplusplus
}
#endif
