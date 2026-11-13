// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <string.h>
#include <stdio.h> // printf
#include <inttypes.h> // PRIxPTR

#include <zstd.h>

#include "julia.h"
#include "julia_internal.h"
#include "julia_gcext.h"
#include "builtin_proto.h"
#include "processor.h"
#include "serialize.h"

#ifdef _OS_WINDOWS_
#include <memoryapi.h>
#include <io.h>
#else
#include <dlfcn.h>
#include <sys/mman.h>
#endif

#include "valgrind.h"
#include "julia_assert.h"
#include "eytzinger.h"
#include "bitvector.h"

static const size_t WORLD_AGE_REVALIDATION_SENTINEL = 0x1;
JL_DLLEXPORT size_t jl_require_world = ~(size_t)0;
JL_DLLEXPORT _Atomic(size_t) jl_first_image_replacement_world = ~(size_t)0;

// This structure is used to store hash tables for the memoization
// of queries in staticdata.c (currently only `type_in_worklist`).
typedef struct {
    htable_t type_in_worklist;
} jl_query_cache;

static void init_query_cache(jl_query_cache *cache) JL_NOTSAFEPOINT
{
    htable_new(&cache->type_in_worklist, 0);
}

static void destroy_query_cache(jl_query_cache *cache) JL_NOTSAFEPOINT
{
    htable_free(&cache->type_in_worklist);
}

#include "staticdata_utils.c"
#include "precompile_utils.c"

#ifdef __cplusplus
extern "C" {
#endif

// TODO: handle finalizers

// Heap values restored from the system image.
static jl_value_t **const jl_image_specials[] = {
    (jl_value_t**)&jl_method_table,
    (jl_value_t**)&jl_module_init_order,
    &jl_typeinf_func,
    &jl_compile_and_emit_func,
    &jl_libdl_dlopen_func,
#define XX(cname, jlname) &jl_builtin_instances[jl_builtin_id_##cname],
    JL_BUILTIN_FUNCTIONS(XX)
#undef XX
#define XX(name, type) (jl_value_t**)&jl_##name,
    JL_EXPORTED_DATA_POINTERS(XX)
    JL_CONST_GLOBAL_VARS(XX)
#undef XX
};

// The value written for the image special at `p`: for `jl_module_init_order`, the modules with an
// `__init__`
static jl_value_t *image_special(jl_value_t **p, jl_array_t *module_init_order) JL_NOTSAFEPOINT
{
    return p == (jl_value_t**)&jl_module_init_order ? (jl_value_t*)module_init_order : *p;
}

static htable_t serialization_order; // to break cycles, mark all objects that are serialized
static htable_t nullptrs;
static htable_t symtab_left, symtab_right; // the links of the system image's symbol table
static void *symtab_root;
// FIFO queue for objects to be serialized. Anything requiring fixup upon deserialization
// must be "toplevel" in this queue.
static arraylist_t serialization_queue;
// for each `id` in `serialization_order`: where the object was written, a position in `s->s`,
// or BITS_ITEM plus a position in `s->bits`
static arraylist_t layout_table;
// the objects written as auxdata records, in the order of their records, and the size of the
// auxdata section they take up
static arraylist_t uniquing_records;
static size_t auxdata_size;
// Offset in the heap at which the bits stream was placed, once all objects are written.
// The bits come last, so the pages the loader writes are kept apart from pages it never touches.
static size_t bits_start;
// The address at which the heap being written expects to be placed: its preferred base
static uintptr_t heap_base;
static arraylist_t object_worklist;  // used to mimic recursion by jl_serialize_reachable

// Track image locations and some associated metadata for all loaded sys / pkgimages.
// (the data of range i is an image_metadata_t* for build_ids[i])
static eyt_tree_t image_tree;
typedef struct {
    uintptr_t base;          // base address of the image
    jl_module_t *top_mod;    // owning top-level module
    size_t idx;              // range index in image_tree (for serialization)
    int coverage_compatible; // counters support the requested coverage mode
    uint32_t *deps;          // bitvector over blobs 0..idx: the sysimage, the images
                             // this one was loaded against, and itself
    _Atomic(size_t) world;   // the world in which the image's methods were activated
                             // (0 until then)
} image_metadata_t;

// readers use a snapshot without passing a safepoint, so free it after the next collection
static void image_tree_retire(void *snapshot) JL_NOTSAFEPOINT
{
    jl_gc_add_quiescent(jl_current_task->ptls, (void**)snapshot, free);
}

void jl_init_staticdata(void)
{
    eyt_tree_init(&image_tree, image_tree_retire);
}

// HT_NOTFOUND is a valid integer ID, so we store the integer ids mangled.
// This pair of functions mangles/demanges
static size_t from_seroder_entry(void *entry) JL_NOTSAFEPOINT
{
    return (size_t)((char*)entry - (char*)HT_NOTFOUND - 1);
}

static void *to_seroder_entry(size_t idx) JL_NOTSAFEPOINT
{
    return (void*)((char*)HT_NOTFOUND + 1 + idx);
}

static htable_t new_methtables;

uintptr_t jl_sysimage_start = 0;
uintptr_t jl_sysimage_size = 0;

static size_t n_linkage_blobs(void) JL_NOTSAFEPOINT
{
    return eyt_tree_nranges(&image_tree);
}

static image_metadata_t *external_blob_metadata(jl_value_t *v) JL_NOTSAFEPOINT
{
    assert((uintptr_t) v % 4 == 0 && "Object not 4-byte aligned!");
    void *data = eyt_tree_find_data(&image_tree, (uintptr_t)v);
    return data == EYT_NOTFOUND ? NULL : (image_metadata_t*)data;
}

static size_t external_blob_index(jl_value_t *v) JL_NOTSAFEPOINT
{
    image_metadata_t *meta = external_blob_metadata(v);
    return meta ? meta->idx : (size_t)-1;
}

JL_DLLEXPORT size_t jl_external_blob_index(jl_value_t *v) JL_NOTSAFEPOINT
{
    return external_blob_index(v);
}

// bitvector over blobs 0..idx: the sysimage, `depmods`, and itself.
// `depmods` already lists every module the precompile worker had loaded.
static uint32_t *image_deps_new(size_t idx, jl_array_t *depmods) JL_NOTSAFEPOINT
{
    uint32_t *deps = (uint32_t*)calloc_s(bitvector_nwords(idx + 1) * sizeof(uint32_t));
    bitvector_set(deps, 0, 1); // the sysimage
    for (size_t i = 0, ld = depmods ? jl_array_nrows(depmods) : 0; i < ld; i++) {
        image_metadata_t *dep = external_blob_metadata(jl_array_ptr_ref(depmods, i));
        if (dep == NULL || dep->idx >= idx)
            continue;
        bitvector_set(deps, dep->idx, 1);
    }
    bitvector_set(deps, idx, 1);
#ifndef NDEBUG
    // a dependency's own dependencies are among ours
    for (size_t i = 0, ld = depmods ? jl_array_nrows(depmods) : 0; i < ld; i++) {
        image_metadata_t *dep = external_blob_metadata(jl_array_ptr_ref(depmods, i));
        if (dep == NULL || dep->idx >= idx)
            continue;
        for (size_t w = 0, nw = bitvector_nwords(dep->idx + 1); w < nw; w++)
            assert((dep->deps[w] & ~deps[w]) == 0);
    }
#endif
    return deps;
}

// If linkage blob `blob` is a dependency of linkage blob `image` (or the
// sysimage, or `image` itself), the world in which the methods of `blob` were
// activated, else 0. A dependency has been loaded in full, so its world is set.
JL_DLLEXPORT size_t jl_linkage_blob_dep_world(size_t image, size_t blob) JL_NOTSAFEPOINT
{
    if (blob > image)
        return 0;
    void *data = eyt_tree_range_data(&image_tree, image);
    if (data == EYT_NOTFOUND)
        return 0;
    image_metadata_t *meta = (image_metadata_t*)data;
    if (meta->deps == NULL || !bitvector_get(meta->deps, blob))
        return 0;
    image_metadata_t *blobmeta = (image_metadata_t*)eyt_tree_range_data(&image_tree, blob);
    size_t world = jl_atomic_load_relaxed(&blobmeta->world);
    assert(world != 0);
    return world;
}

JL_DLLEXPORT uint8_t jl_object_in_image(jl_value_t *obj) JL_NOTSAFEPOINT
{
    if (obj == NULL)
        return 0;
    uint8_t in_image = jl_astaggedvalue(obj)->bits.in_image != 0;
    assert((uintptr_t) obj % 4 == 0 && "Object not 4-byte aligned!");
#ifndef NDEBUG
    if (eyt_tree_is_in_range(&image_tree, (uintptr_t)obj) != in_image) {
        // print enough context to identify the object before aborting
        jl_datatype_t *ty = (jl_datatype_t*)jl_typeof(obj);
        jl_safe_printf("jl_object_in_image inconsistency: obj %p header %zx "
                       "bits.in_image %d not matching image tree, typeof %p (%s)\n",
                       (void*)obj, (size_t)jl_astaggedvalue(obj)->header, (int)in_image,
                       (void*)ty,
                       jl_is_datatype(ty) ? jl_symbol_name(ty->name->name) : "<?>");
    }
#endif
    assert(eyt_tree_is_in_range(&image_tree, (uintptr_t)obj) == in_image);
    return in_image;
}

// Map an object to its "owning" top module
JL_DLLEXPORT jl_value_t *jl_object_top_module(jl_value_t* v) JL_NOTSAFEPOINT
{
    image_metadata_t *meta = external_blob_metadata(v);
    if (meta)
        return (jl_value_t*)meta->top_mod;
    // The object is runtime allocated
    return (jl_value_t*)jl_nothing;
}

// Whether this code instance's image has compatible coverage counters,
// as determined at load time by jl_register_image_coverage.
JL_DLLEXPORT int jl_codeinst_coverage_compatible(jl_code_instance_t *ci) JL_NOTSAFEPOINT
{
    image_metadata_t *meta = external_blob_metadata((jl_value_t*)ci);
    return meta != NULL && meta->coverage_compatible;
}

// hash of definitions for predefined function pointers
// (reverse is jl_builtin_f_addrs)
static htable_t fptr_to_id;
// added to a builtin's index in `fptr_to_id`, so that no value is HT_NOTFOUND
#define BUILTIN_IDX_BIAS 2

void *native_functions;   // opaque jl_native_code_desc_t blob used for fetching data from LLVM

// table of struct field addresses to rewrite during saving
static htable_t object_replace;
static htable_t field_replace;
static htable_t bits_replace;
#ifndef NDEBUG
// objects queued non-recursively, i.e. without the objects they refer to
static htable_t queued_without_refs;
#endif
static int queueing_done;
// internal arrays whose memory is written only up to the array length (memory => array)
static htable_t memory_trims;

// queued objects whose identity does not matter, keyed by content (idset uses egal),
// so only one object per distinct content is serialized
static jl_genericmemory_t *content_dedup_list;
static jl_genericmemory_t *content_dedup_keyset;
static size_t content_dedup_max;

// Strings and plain immutables are interchangeable with any egal copy. Types and
// singletons are excluded, since the runtime relies on their identity.
static int dedup_by_content(jl_value_t *v, jl_datatype_t *t) JL_NOTSAFEPOINT
{
    if (t == jl_string_type)
        return 1;
    return !t->name->mutabl && !jl_is_type(v) && !jl_is_vararg(v) && !jl_is_datatype_singleton(t);
}


typedef struct {
    ios_t *s;                   // the main stream
    ios_t *bits;                // pointer-free data, placed at the end of the heap (e.g., datatype layouts, internal arrays)
    ios_t *symbols;             // NUL-terminated names of symbols, the strtab section
    ios_t *imports;
    ios_t *fptr_record;
    arraylist_t memref_list;    // locations of MemoryRefs to Memory in this image

    // (heap offset, item) pairs: the item is a bind, a serialization id (see `backref_id`), or
    // BITS_ITEM plus an offset into the bits stream; `jl_finish_relocs` makes each a slide or a bind
    arraylist_t relocs_list;
    arraylist_t fixup_types;    // a list of locations of types requiring (re)caching
    arraylist_t fixup_objs;     // a list of locations of objects requiring (re)caching
    jl_array_t *buildid_depmods_idxs; // image index => dependency index
    htable_t object_imports;    // symbol, or object written as a record => bind to its import
    htable_t import_set;        // the imports, by value (keys and values are 2 + import index)
    jl_array_t *method_roots_list;
    htable_t method_roots_index;
    uint64_t worklist_key;
    jl_query_cache *query_cache;
    jl_ptls_t ptls;
    int8_t incremental;
} jl_serializer_state;

typedef struct {
    image_metadata_t **deps;    // the images that deprefs refer to: the system image, then the dependencies
    uintptr_t heap_delta;       // how far the heap is from the address its slides were written for
    uintptr_t *imports_resolved; // the address each import resolves to (provisional while it is pending)
    // the words filled from a pending import, as pairs of the word's address and 1 + the index of
    // the next pair filled from the same import
    arraylist_t waiting;
    size_t *first_waiting;      // for each import, 1 + the index of its latest pair in `waiting`
    uint8_t *pending;           // whether each import is pending (see jl_fill_bind)
} jl_loader_state;

// The specials of an incremental image
typedef struct {
    jl_array_t *worklist;
    jl_array_t *module_init_order;
    jl_array_t *extext_methods;
    jl_array_t *extext_records;
    jl_array_t *method_roots_list;
} jl_pkgimage_specials_t;

static jl_value_t *jl_bigint_type = NULL;
static jl_debuginfo_t *jl_nulldebuginfo;
static int gmp_limb_size = 0;

// The low BIND_LOW_BITS of a bind are ORed into what it resolves to.
#define BIND_BIT ((uintptr_t)1 << (8 * sizeof(void*) - 1))
#define BIND_LOW_BITS 3
#define BIND_LOW_MASK (((uintptr_t)1 << BIND_LOW_BITS) - 1)
#define BIND_PAYLOAD(x) (((x) & ~BIND_BIT) >> BIND_LOW_BITS)
// While writing, an item that is not a bind is a serialization id, or an offset into the bits stream
#define BITS_ITEM ((uintptr_t)1 << (8 * sizeof(void*) - 2))

typedef uint64_t import_t;
enum { IMPORT_IMAGE, IMPORT_SPECIAL, IMPORT_FUNCTION, IMPORT_SYMBOL, IMPORT_SINGLETON, IMPORT_DATATYPE,
       IMPORT_METHOD_INSTANCE, IMPORT_BINDING, IMPORT_SPECPTR };
#define IMPORT_HAS_RECORD(t) (IMPORT_DATATYPE <= (t) && (t) <= IMPORT_BINDING)
enum { FUNCTION_BUILTIN, FUNCTION_RUNTIME };
#define IMPORT_TYPE_OFFSET 60
#define IMPORT_ENCODE(type, payload) (((import_t)(type) << IMPORT_TYPE_OFFSET) | (import_t)(payload))
#define IMPORT_PAYLOAD(x) ((x) & (((import_t)1 << IMPORT_TYPE_OFFSET) - 1))
#define IMPORT_FUNCTION_KIND_OFFSET 57

// A dependency reference, the payload of an IMPORT_IMAGE or IMPORT_SPECPTR import
#define DEPREF_DEPS_BITS 21
#define DEPREF_OFFSET_BITS 38

// The auxdata record of an IMPORT_DATATYPE import (see doc/src/devdocs/sysimg.md).  The relocs
// before `super` are resolved together, and `super` once all the types are resolved.
typedef struct {
    uintptr_t name, parameters, types, layout, instance, super; // relocs
    uint32_t hash;
    uint16_t flags; // the bit fields of jl_datatype_t, which follow `hash`, as the compiler lays them out
    uint16_t padding;
} jl_type_record_t;
// The auxdata records of IMPORT_BINDING and IMPORT_METHOD_INSTANCE imports
typedef struct {
    uintptr_t mod, name; // relocs
} jl_binding_record_t;
typedef struct {
    uintptr_t def, specTypes, sparam_vals; // relocs
} jl_mi_record_t;
static_assert(sizeof(jl_type_record_t) == 6 * sizeof(uintptr_t) + 8, "a type record has no implicit padding");
static_assert(sizeof(jl_type_record_t) % sizeof(void*) == 0 && sizeof(jl_binding_record_t) % sizeof(void*) == 0 &&
              sizeof(jl_mi_record_t) % sizeof(void*) == 0, "records are pointer-aligned without padding between them");
static_assert(sizeof(jl_datatype_t) == offsetof(jl_datatype_t, hash) + 2 * sizeof(uint32_t), "the bit fields of jl_datatype_t fit in the 4 bytes after `hash`");
#define TYPE_FLAGS(dt) ((char*)&(dt)->hash + sizeof((dt)->hash))

// --- Static Compile ---
static jl_image_buf_t jl_sysimage_buf = { JL_IMAGE_KIND_NONE };

static inline uintptr_t *sysimg_gvars(const char *base, const int32_t *offsets, size_t idx)
{
    return (uintptr_t*)(base + offsets[idx]);
}

JL_DLLEXPORT int jl_running_on_valgrind(void)
{
    return RUNNING_ON_VALGRIND;
}

// --- serializer ---

static int jl_needs_serialization(jl_serializer_state *s, jl_value_t *v) JL_NOTSAFEPOINT
{
    // ignore items that are given a special relocation representation
    if (s->incremental && jl_object_in_image(v))
        return 0;
    return v != NULL && !(s->incremental && jl_is_symbol(v)) && v != (jl_value_t*)s->ptls->root_task;
}

// Classification of how an object must be handled on deserialization of an
// incremental image, as computed by `caching_tag`:
//  - CACHING_NONE: the object is serialized as-is.
//  - CACHING_UNIQUING: the object may already exist in the loading process (it
//    belongs to a type/method/module from an earlier image), so it must be
//    looked up and de-duplicated against the existing copy at load time.
//  - CACHING_RECACHING: the object is new to this image but depends on types
//    in the worklist, so it must be (re)inserted into the relevant caches at
//    load time.
#define CACHING_NONE      0
#define CACHING_UNIQUING  1
#define CACHING_RECACHING 2

static int caching_tag(jl_value_t *v, jl_query_cache *query_cache) JL_NOTSAFEPOINT
{
    if (jl_is_method_instance(v)) {
        jl_method_instance_t *mi = (jl_method_instance_t*)v;
        jl_value_t *m = mi->def.value;
        if (jl_is_method(m) && jl_object_in_image(m))
            return type_in_worklist(mi->specTypes, query_cache) ? CACHING_RECACHING : CACHING_UNIQUING;
    }
    if (jl_is_binding(v)) {
        jl_globalref_t *gr = ((jl_binding_t*)v)->globalref;
        if (!gr)
            return CACHING_NONE;
        if (!jl_object_in_image((jl_value_t*)gr->mod))
            return CACHING_NONE;
        return CACHING_UNIQUING;
    }
    if (jl_is_datatype(v)) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        if (jl_is_tuple_type(dt) ? !dt->isconcretetype : dt->hasfreetypevars)
            return CACHING_NONE; // aka !is_cacheable from jltypes.c
        if (jl_object_in_image((jl_value_t*)dt->name))
            return type_in_worklist(v, query_cache) ? CACHING_RECACHING : CACHING_UNIQUING;
    }
    jl_value_t *dtv = jl_typeof(v);
    if (jl_is_datatype_singleton((jl_datatype_t*)dtv)) {
        // these are already recached in the datatype in the image
        return type_in_worklist(dtv, query_cache) ? CACHING_NONE : CACHING_UNIQUING;
    }
    return CACHING_NONE;
}

static int needs_recaching(jl_value_t *v, jl_query_cache *query_cache) JL_NOTSAFEPOINT
{
    return caching_tag(v, query_cache) == CACHING_RECACHING;
}

static int needs_uniquing(jl_value_t *v, jl_query_cache *query_cache) JL_NOTSAFEPOINT
{
    assert(!jl_object_in_image(v));
    return caching_tag(v, query_cache) == CACHING_UNIQUING;
}

// whether `record_field_change` has already registered a replacement for `addr`
static int has_field_change(jl_value_t **addr) JL_NOTSAFEPOINT
{
    return ptrhash_get(&field_replace, (void*)addr) != HT_NOTFOUND;
}

// Write `newval` for the field at `addr` of `owner`. Recorded while queueing, before the
// fields of `owner` are queued.
static void record_field_change(jl_value_t *owner, jl_value_t **addr, jl_value_t *newval) JL_NOTSAFEPOINT
{
    assert(!queueing_done && "field change recorded after queueing");
    void *idx = ptrhash_get(&serialization_order, owner);
    assert((idx == HT_NOTFOUND || idx == (void*)(uintptr_t)-1 || idx == (void*)(uintptr_t)-2) &&
           "field change recorded after its object was queued");
    (void)idx;
    if (has_field_change(addr) || *addr != newval)
        ptrhash_put(&field_replace, (void*)addr, newval);
}

static jl_value_t *get_replaceable_field(jl_value_t **addr, int mutabl) JL_CANSAFEPOINT JL_GC_DISABLED
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

// Rewrite the `cache` field of each MethodInstance (and the `next` chain of the
// CodeInstances it points to) so the image stores exactly the ordered list of
// CodeInstances `cis` that codegen produced, converted to a linked list via the
// `next` field, instead of whatever the live runtime cache happened to contain.
// CodeInstances are grouped by their owning MethodInstance, preserving the order
// in which they appear in `cis`. These changes are recorded through the normal
// `field_replace` machinery, so they must be applied before the serialization
// queue pass runs (both queueing and writing consult `get_replaceable_field`).
static void jl_rewrite_mi_caches(jl_code_instance_t **cis, size_t n) JL_GC_DISABLED
{
    htable_t mi_last; // MethodInstance -> last CodeInstance recorded for it
    htable_t seen;    // CodeInstances already placed, to guard against duplicates
    htable_new(&mi_last, 0);
    htable_new(&seen, 0);
    for (size_t i = 0; i < n; i++) {
        jl_code_instance_t *ci = cis[i];
        // ignore any repeated CodeInstance: chaining it twice would corrupt the list
        if (ptrhash_get(&seen, ci) != HT_NOTFOUND)
            continue;
        ptrhash_put(&seen, ci, ci);
        jl_method_instance_t *mi = jl_get_ci_mi(ci);
        // this CodeInstance terminates its MethodInstance's chain, unless a
        // later CodeInstance for the same MethodInstance extends it below
        record_field_change((jl_value_t*)ci, (jl_value_t**)&ci->next, NULL);
        if (jl_options.trim && ci->owner == (jl_value_t*)jl_trim_sym) {
            // this is an "isolated" cache entry for `--trim` inference / AOT
            // replace the owner with `nothing` so that these are available
            // as ordinary cache entries at runtime
            record_field_change((jl_value_t*)ci, (jl_value_t**)&ci->owner, jl_nothing);
        }
        void **bp = ptrhash_bp(&mi_last, mi);
        // Register unconditionally (not via record_field_change) so this
        // cache rebuilds correctly, even if it matches what was there (which would
        // accidentally excise the item with the next write to field_replace).
        if (*bp == HT_NOTFOUND) {
            // first CodeInstance seen for this MethodInstance becomes the head.
            ptrhash_put(&field_replace, (void*)&mi->cache, (void*)ci);
        }
        else {
            // link this CodeInstance after the previous one for the same MethodInstance
            jl_code_instance_t *prev = (jl_code_instance_t*)*bp;
            ptrhash_put(&field_replace, (void*)&prev->next, (void*)ci);
        }
        *bp = ci;
    }
    htable_free(&seen);
    htable_free(&mi_last);
}

// index of the builtin with this specptr, or -1
static int jl_builtin_idx(void *fptr)
{
    void *val = ptrhash_get(&fptr_to_id, fptr);
    return val == HT_NOTFOUND ? -1 : (int)((uintptr_t)val - BUILTIN_IDX_BIAS);
}

static int effects_foldable(uint32_t effects)
{
    // N.B.: This needs to be kept in sync with Core.Compiler.is_foldable(effects, true)
    return ((effects & 0x7) == 0) && // is_consistent(effects)
           (((effects >> 10) & 0x03) == 0) && // is_noub(effects)
           (((effects >> 3) & 0x03) == 0) && // is_effect_free(effects)
           ((effects >> 6) & 0x01); // is_terminates(effects)
}


// `jl_queue_for_serialization` adds items to `serialization_order`
#define jl_queue_for_serialization(s, v) jl_queue_for_serialization_((s), (jl_value_t*)(v), 1)
static void jl_queue_for_serialization_(jl_serializer_state *s, jl_value_t *v, int recursive) JL_CANSAFEPOINT JL_GC_DISABLED;

static void jl_queue_module_for_serialization(jl_serializer_state *s, jl_module_t *m) JL_CANSAFEPOINT JL_GC_DISABLED
{
    jl_queue_for_serialization(s, m->name);
    jl_queue_for_serialization(s, m->parent);
    jl_queue_for_serialization(s, jl_options.strip_metadata ? jl_empty_sym : m->file);
    jl_queue_for_serialization(s, jl_atomic_load_relaxed(&m->bindingkeyset));
    if (jl_options.trim) {
        jl_queue_for_serialization_(s, (jl_value_t*)jl_atomic_load_relaxed(&m->bindings), 0);
        jl_svec_t *table = jl_atomic_load_relaxed(&m->bindings);
        for (size_t i = 0; i < jl_svec_len(table); i++) {
            jl_binding_t *b = (jl_binding_t*)jl_svecref(table, i);
            if ((void*)b == jl_nothing)
                break;
            jl_value_t *val = jl_get_binding_value_in_world(b, jl_atomic_load_relaxed(&jl_world_counter));
            // keep binding objects that are defined in the latest world and ...
            if (val &&
                // ... point to modules ...
                (jl_is_module(val) ||
                 // ... or point to __init__ methods ...
                 !strcmp(jl_symbol_name(b->globalref->name), "__init__") ||
                 // ... or point to Base functions accessed by the runtime
                 (m == jl_base_module && (!strcmp(jl_symbol_name(b->globalref->name), "wait") ||
                                          !strcmp(jl_symbol_name(b->globalref->name), "task_done_hook") ||
                                          !strcmp(jl_symbol_name(b->globalref->name), "_uv_hook_close"))))) {
                jl_queue_for_serialization(s, b);
            }
        }
    }
    else {
        jl_queue_for_serialization(s, jl_atomic_load_relaxed(&m->bindings));
    }

    for (size_t i = 0; i < module_usings_length(m); i++) {
        jl_queue_for_serialization(s, module_usings_getmod(m, i));
    }

    if (jl_options.trim || jl_options.strip_ir) {
        record_field_change((jl_value_t*)m, (jl_value_t**)&m->usings_backedges, jl_nothing);
        record_field_change((jl_value_t*)m, (jl_value_t**)&m->scanned_methods, jl_nothing);
    }
    else {
        jl_queue_for_serialization(s, m->usings_backedges);
        jl_queue_for_serialization(s, m->scanned_methods);
    }
}

static int codeinst_may_be_runnable(jl_code_instance_t *ci, int incremental) {
    size_t max_world = jl_atomic_load_relaxed(&ci->max_world);
    if (max_world == ~(size_t)0)
        return 1;
    if (incremental)
        return 0;
    return jl_atomic_load_relaxed(&ci->min_world) <= jl_typeinf_world && jl_typeinf_world <= max_world;
}

// Anything that requires uniquing or fixing during deserialization needs to be "toplevel"
// in serialization (i.e., have its own entry in `serialization_order`). Consequently,
// objects that act as containers for other potentially-"problematic" objects must add such "children"
// to the queue.
static void jl_insert_into_serialization_queue(jl_serializer_state *s, jl_value_t *v, int recursive) JL_CANSAFEPOINT JL_GC_DISABLED
{
    jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
    jl_queue_for_serialization_(s, (jl_value_t*)t, 1);
    const jl_datatype_layout_t *layout = t->layout;

    if (!recursive) {
#ifndef NDEBUG
        ptrhash_put(&queued_without_refs, v, v);
#endif
        goto done_fields;
    }
    // the fields of a symbol are the links of the process's symbol table
    if (jl_is_symbol(v))
        goto done_fields;

    if (s->incremental && jl_is_datatype(v)) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
#ifndef NDEBUG
        if (jl_is_datatype_singleton(dt) &&
            eyt_tree_is_in_range(&image_tree, (uintptr_t)dt->instance) !=
                (jl_astaggedvalue(dt->instance)->bits.in_image != 0)) {
            jl_safe_printf("singleton instance with inconsistent in_image bit: type %s.%s\n",
                           jl_symbol_name(dt->name->module->name),
                           jl_symbol_name(dt->name->name));
        }
#endif
        if (jl_is_datatype_singleton(dt) && needs_uniquing(dt->instance, s->query_cache)) {
            assert(jl_needs_serialization(s, dt->instance)); // should be true, since we visited dt
            // do not visit dt->instance for our template object as it leads to unwanted cycles here
            // (it may get serialized from elsewhere though)
            record_field_change((jl_value_t*)dt, &dt->instance, jl_nothing);
        }
    }
    if (jl_is_method_instance(v)) {
        jl_method_instance_t *mi = (jl_method_instance_t*)v;
        if (s->incremental) {
            jl_value_t *def = mi->def.value;
            if (needs_uniquing(v, s->query_cache)) {
                // we only need 3 specific fields of this (the rest are not used)
                jl_queue_for_serialization(s, mi->def.value);
                jl_queue_for_serialization(s, mi->specTypes);
                jl_queue_for_serialization(s, (jl_value_t*)mi->sparam_vals);
                goto done_fields;
            }
            else if (jl_is_method(def) && jl_object_in_image(def)) {
                // we only need 3 specific fields of this (the rest are restored afterward, if valid)
                // in particular, cache is repopulated by jl_mi_cache_insert for all foreign function,
                // so must not be present here
                record_field_change((jl_value_t*)mi, (jl_value_t**)&mi->cache, NULL);
            }
            else {
                assert(!needs_recaching(v, s->query_cache));
            }
            // Any back-edges will be re-validated and added by staticdata.jl, so
            // drop them from the image here
            record_field_change((jl_value_t*)mi, (jl_value_t**)&mi->backedges, NULL);
            // n.b. opaque closures cannot be inspected and relied upon like a
            // normal method since they can get improperly introduced by generated
            // functions, so if they appeared at all, we will probably serialize
            // them wrong and segfault. The jl_code_for_staged function should
            // prevent this from happening, so we do not need to detect that user
            // error now.
        }
        // Only the CodeInstances deliberately selected for the image caches
        // should be reachable through `cache`; jl_rewrite_mi_caches records the
        // chosen ordering for those MethodInstances. Any MethodInstance whose
        // cache was not rewritten gets an empty cache here, rather than leaking
        // whatever the live runtime cache happened to contain. Builtin functions
        // are an exception: their cache is not part of the codegen ordering and
        // is not repopulated at load, so keep it. Builtin-like functions are
        //  recognized by their method having neither source nor a generator
        // (cf. the jl_is_builtinfunc computation).
        jl_value_t *midef = mi->def.value;
        int is_builtin = jl_is_method(midef) &&
            ((jl_method_t*)midef)->source == NULL &&
            ((jl_method_t*)midef)->generator == NULL;
        if (native_functions && !is_builtin && !has_field_change((jl_value_t**)&mi->cache))
            record_field_change((jl_value_t*)mi, (jl_value_t**)&mi->cache, NULL);
        // don't recurse into all backedges memory (yet)
        jl_value_t *backedges = get_replaceable_field((jl_value_t**)&mi->backedges, 1);
        if (backedges) {
            assert(!jl_options.trim && !jl_options.strip_ir);
            jl_queue_for_serialization_(s, backedges, 0);
            jl_queue_for_serialization_(s, (jl_value_t*)((jl_array_t*)backedges)->ref.mem, 0);
            size_t i = 0, n = jl_array_nrows(backedges);
            while (i < n) {
                jl_value_t *invokeTypes;
                jl_code_instance_t *caller;
                i = get_next_edge((jl_array_t*)backedges, i, &invokeTypes, &caller);
                if (invokeTypes)
                    jl_queue_for_serialization(s, invokeTypes);
            }
        }
    }
    if (jl_is_binding(v)) {
        jl_binding_t *b = (jl_binding_t*)v;
        if (s->incremental && needs_uniquing(v, s->query_cache)) {
            jl_queue_for_serialization(s, b->globalref->mod);
            jl_queue_for_serialization(s, b->globalref->name);
            goto done_fields;
        }
        if (jl_options.trim || jl_options.strip_ir) {
            record_field_change((jl_value_t*)b, (jl_value_t**)&b->backedges, NULL);
        }
        else {
            // don't recurse into all backedges memory (yet)
            jl_value_t *backedges = get_replaceable_field((jl_value_t**)&b->backedges, 1);
            if (backedges) {
                jl_queue_for_serialization_(s, backedges, 0);
                jl_queue_for_serialization_(s, (jl_value_t*)((jl_array_t*)backedges)->ref.mem, 0);
                for (size_t i = 0, n = jl_array_nrows(backedges); i < n; i++) {
                    jl_value_t *b = jl_array_ptr_ref(backedges, i);
                    if (!jl_is_code_instance(b) && !jl_is_method_instance(b) && !jl_is_method(b)) // otherwise usually a Binding?
                        jl_queue_for_serialization(s, b);
                }
            }
        }
    }
    if (s->incremental && jl_is_binding_partition(v)) {
        // Partitions that have ended cannot be reached after loading (they are written
        // with an empty world range), so leave them out of the chain.
        jl_binding_partition_t *bpart = (jl_binding_partition_t*)v;
        jl_value_t *next = (jl_value_t*)jl_atomic_load_relaxed(&bpart->next);
        while (next && jl_is_binding_partition(next) &&
               jl_atomic_load_relaxed(&((jl_binding_partition_t*)next)->max_world) != ~(size_t)0)
            next = (jl_value_t*)jl_atomic_load_relaxed(&((jl_binding_partition_t*)next)->next);
        record_field_change((jl_value_t*)bpart, (jl_value_t**)&bpart->next, next);
    }
    if (jl_is_typename(v)) {
        jl_typename_t *tn = (jl_typename_t*)v;
        // don't recurse into several fields (yet)
        jl_queue_for_serialization_(s, (jl_value_t*)jl_atomic_load_relaxed(&tn->cache), 0);
        jl_queue_for_serialization_(s, (jl_value_t*)jl_atomic_load_relaxed(&tn->linearcache), 0);
        if (s->incremental) {
            assert(!jl_object_in_image((jl_value_t*)tn->module));
            assert(!jl_object_in_image((jl_value_t*)tn->wrapper));
        }
    }
    if (jl_is_mtable(v)) {
        jl_methtable_t *mt = (jl_methtable_t*)v;
        // The contributor tags name this process's linkage blobs, so they are not saved
        record_field_change((jl_value_t*)mt, (jl_value_t**)&mt->contributors, jl_an_empty_memory_any);
        // Any back-edges will be re-validated and added by staticdata.jl, so
        // drop them from the image here
        if (s->incremental || jl_options.trim || jl_options.strip_ir) {
            record_field_change((jl_value_t*)mt, (jl_value_t**)&mt->backedges, jl_an_empty_memory_any);
        }
        else {
            // don't recurse into all backedges memory (yet)
            jl_value_t *allbackedges = get_replaceable_field((jl_value_t**)&mt->backedges, 1);
            jl_queue_for_serialization_(s, allbackedges, 0);
            for (size_t i = 0, n = ((jl_genericmemory_t*)allbackedges)->length; i < n; i += 2) {
                jl_value_t *tn = jl_genericmemory_ptr_ref(allbackedges, i);
                jl_queue_for_serialization(s, tn);
                jl_value_t *table = jl_genericmemory_ptr_ref(allbackedges, i + 1);
                if (table && table != jl_nothing) {
                    // per-typename eqtable of sig => callers
                    jl_queue_for_serialization_(s, table, 0);
                    for (size_t j = 0, m = ((jl_genericmemory_t*)table)->length; j < m; j += 2) {
                        jl_value_t *t = jl_genericmemory_ptr_ref(table, j);
                        jl_value_t *callers = jl_genericmemory_ptr_ref(table, j + 1);
                        if (callers == NULL)
                            continue;
                        assert(!jl_is_code_instance(t));
                        jl_queue_for_serialization(s, t);
                        // serialize the callers list without forcing the CodeInstances
                        // in it to be serialized (unreachable ones are pruned later)
                        jl_queue_for_serialization_(s, callers, 0);
                        jl_queue_for_serialization_(s, (jl_value_t*)((jl_array_t*)callers)->ref.mem, 0);
                    }
                }
            }
        }
    }
    if (jl_is_code_instance(v)) {
        jl_code_instance_t *ci = (jl_code_instance_t*)v;
        jl_method_instance_t *mi = jl_get_ci_mi(ci);
        if (jl_options.strip_ir)
            record_field_change((jl_value_t*)ci, (jl_value_t**)&ci->edges, (jl_value_t*)jl_emptysvec);
        if (jl_options.strip_metadata)
            record_field_change((jl_value_t*)ci, (jl_value_t**)&ci->debuginfo, (jl_value_t*)jl_nulldebuginfo);
        if (s->incremental) {
            // make sure we don't serialize other reachable cache entries of foreign methods
            // Should this now be:
            // if (ci !in ci->defs->cache)
            //     record_field_change((jl_value_t*)ci, (jl_value_t**)&ci->next, NULL);
            // Why are we checking that the method/module this originates from is in_image?
            // and then disconnect this CI?
            if (jl_object_in_image((jl_value_t*)mi->def.value)) {
                // TODO: if (ci in ci->defs->cache)
                record_field_change((jl_value_t*)ci, (jl_value_t**)&ci->next, NULL);
            }
        }
        jl_value_t *inferred = jl_atomic_load_relaxed(&ci->inferred);
        if (inferred && inferred != jl_nothing && !jl_is_uint8(inferred)) { // disregard if there is nothing here to delete (e.g. builtins, unspecialized)
            jl_method_t *def = mi->def.method;
            if (jl_is_method(def)) { // don't delete toplevel code
                int is_relocatable = !s->incremental || jl_is_code_info(inferred) ||
                    (jl_is_string(inferred) && jl_string_len(inferred) > 0 && jl_string_data(inferred)[jl_string_len(inferred) - 1]);
                int may_discard_trees = !jl_get_type_infer_preserve_ir();
                int discard = 0;
                if (may_discard_trees && native_functions &&
                    jl_options.outputo != NULL && ci->owner == jl_nothing &&
                    (def->source != NULL || def->generator != NULL) &&
                    jl_is_code_info(inferred) && jl_ir_inlining_cost(inferred) == UINT16_MAX) {
                    // Drop IR retained only to generate native code (see jl_precompile_keep_ir).
                    // Optimized opaque closures keep theirs, since it can't be rebuilt.
                    record_field_change((jl_value_t*)ci, (jl_value_t**)&ci->inferred, jl_nothing);
                }
                else if (!is_relocatable) {
                    discard = 1;
                }
                else if (def->source == NULL) {
                    // don't delete code from optimized opaque closures that can't be reconstructed (and builtins)
                }
                else if (may_discard_trees && // if allowed to delete
                         (!codeinst_may_be_runnable(ci, s->incremental) || // delete all code that cannot run
                          jl_atomic_load_relaxed(&ci->invoke) == jl_fptr_const_return)) { // delete all code that just returns a constant
                    discard = 1;
                }
                else if (may_discard_trees &&
                         native_functions && // don't delete any code if making a ji file
                         (ci->owner == jl_nothing || ci->owner == (jl_value_t*)jl_trim_sym) && // don't delete code for external interpreters
                         !effects_foldable(jl_atomic_load_relaxed(&ci->ipo_purity_bits)) && // don't delete code we may want for irinterp
                         jl_ir_inlining_cost(inferred) == UINT16_MAX) { // don't delete inlineable code
                    // delete the code now: if we thought it was worth keeping, it would have been converted to object code
                    discard = 1;
                }
                if (discard) {
                    // keep only the inlining cost, so inference can later decide if it is worth getting the source back
                    if (jl_is_string(inferred) || jl_is_code_info(inferred))
                        inferred = jl_box_uint8(jl_encode_inlining_cost(jl_ir_inlining_cost(inferred)));
                    else
                        inferred = jl_nothing;
                    record_field_change((jl_value_t*)ci, (jl_value_t**)&ci->inferred, inferred);
                }
                else if (s->incremental && jl_is_string(inferred)) {
                    // New roots for external methods
                    if (jl_object_in_image((jl_value_t*)def)) {
                        void **pfound = ptrhash_bp(&s->method_roots_index, def);
                        if (*pfound == HT_NOTFOUND) {
                            *pfound = def;
                            size_t nwithkey = nroots_with_key(def, s->worklist_key);
                            if (nwithkey) {
                                jl_array_ptr_1d_push(s->method_roots_list, (jl_value_t*)def);
                                jl_array_t *newroots = jl_alloc_vec_any(nwithkey);
                                jl_array_ptr_1d_push(s->method_roots_list, (jl_value_t*)newroots);
                                rle_iter_state rootiter = rle_iter_init(0);
                                uint64_t *rletable = NULL;
                                size_t nblocks2 = 0;
                                size_t nroots = jl_array_nrows(def->roots);
                                size_t k = 0;
                                if (def->root_blocks) {
                                    rletable = jl_array_data(def->root_blocks, uint64_t);
                                    nblocks2 = jl_array_nrows(def->root_blocks);
                                }
                                while (rle_iter_increment(&rootiter, nroots, rletable, nblocks2)) {
                                    if (rootiter.key == s->worklist_key) {
                                        jl_value_t *newroot = jl_array_ptr_ref(def->roots, rootiter.i);
                                        jl_queue_for_serialization(s, newroot);
                                        jl_array_ptr_set(newroots, k++, newroot);
                                    }
                                }
                                assert(k == nwithkey);
                            }
                        }
                    }
                }
            }
        }
        if (jl_nulldebuginfo && jl_is_method(mi->def.method)) {
            // debuginfo is only used together with the inferred IR or the native code,
            // so there is no need to keep it if the image contains neither
            jl_value_t *src = get_replaceable_field((jl_value_t**)&ci->inferred, 1);
            if (!(src && (jl_is_string(src) || jl_is_code_info(src)))) {
                int32_t invokeptr_id = 0;
                int32_t specfptr_id = 0;
                if (native_functions)
                    jl_get_function_id(native_functions, ci, &invokeptr_id, &specfptr_id);
                if (invokeptr_id <= 0 && specfptr_id == 0)
                    record_field_change((jl_value_t*)ci, (jl_value_t**)&ci->debuginfo, (jl_value_t*)jl_nulldebuginfo);
            }
        }
    }

    if (layout->npointers == 0) {
        // bitstypes do not require recursion
    }
    else if (jl_is_svec(v)) {
        size_t i, l = jl_svec_len(v);
        jl_value_t **data = jl_svec_data(v);
        for (i = 0; i < l; i++) {
            jl_queue_for_serialization_(s, data[i], 1);
        }
    }
    else if (jl_is_array(v)) {
        jl_array_t *ar = (jl_array_t*)v;
        jl_value_t *mem = get_replaceable_field((jl_value_t**)&ar->ref.mem, 1);
        jl_queue_for_serialization_(s, mem, 1);
    }
    else if (jl_is_genericmemory(v)) {
        jl_genericmemory_t *m = (jl_genericmemory_t*)v;
        const char *data = (const char*)m->ptr;
        if (jl_genericmemory_how(m) == JL_GENERICMEMORY_STRINGOWNED) {
            assert(jl_is_string(jl_genericmemory_data_owner_field(m)));
        }
        else if (layout->flags.arrayelem_isboxed) {
            size_t i, l = m->length;
            for (i = 0; i < l; i++) {
                jl_value_t *fld = get_replaceable_field(&((jl_value_t**)data)[i], 1);
                jl_queue_for_serialization_(s, fld, 1);
            }
        }
        else if (layout->first_ptr >= 0) {
            uint16_t elsz = layout->size;
            size_t i, l = m->length;
            size_t j, np = layout->npointers;
            for (i = 0; i < l; i++) {
                for (j = 0; j < np; j++) {
                    uint32_t ptr = jl_ptr_offset(t, j);
                    jl_value_t *fld = get_replaceable_field(&((jl_value_t**)data)[ptr], 1);
                    jl_queue_for_serialization_(s, fld, 1);
                }
                data += elsz;
            }
        }
    }
    else if (jl_is_module(v)) {
        jl_queue_module_for_serialization(s, (jl_module_t*)v);
    }
    else if (jl_is_cancel_source(v)) {
        // Write parents - child lists are reconstruct at load time.
        jl_cancel_source_t *cs = (jl_cancel_source_t*)v;
        jl_cancel_parent_link_t *links = jl_cancel_source_links(cs);
        for (size_t i = 0; i < cs->nparents; i++)
            jl_queue_for_serialization_(s, (jl_value_t*)links[i].parent, 1);
    }
    else if (jl_is_wait_entry(v)) {
        // Entirely transient wait state: the writer resets `task` and every
        // slot, so queue nothing - in particular not the live Task the
        // layout's field walk would otherwise pick up (mirroring the
        // cancel-source case above, which skips its transient
        // waiters_head/walk_lock).
    }
    else if (layout->nfields > 0) {
        if (jl_options.trim) {
            if (jl_is_method(v)) {
                jl_method_t *m = (jl_method_t *)v;
                if (jl_is_svec(jl_atomic_load_relaxed(&m->specializations)))
                    jl_queue_for_serialization_(s, (jl_value_t*)jl_atomic_load_relaxed(&m->specializations), 0);
            }
            else if (jl_is_mtable(v)) {
                jl_methtable_t *mt = (jl_methtable_t*)v;
                jl_methtable_t *newmt = (jl_methtable_t*)ptrhash_get(&new_methtables, mt);
                if (newmt != HT_NOTFOUND)
                    record_field_change((jl_value_t*)mt, (jl_value_t **)&mt->defs, (jl_value_t*)jl_atomic_load_relaxed(&newmt->defs));
                else
                    record_field_change((jl_value_t*)mt, (jl_value_t **)&mt->defs, jl_nothing);
            }
            else if (jl_is_mcache(v)) {
                jl_methcache_t *mc = (jl_methcache_t*)v;
                jl_value_t *cache = jl_atomic_load_relaxed(&mc->cache);
                if (!jl_typetagis(cache, jl_typemap_entry_type) || ((jl_typemap_entry_t*)cache)->sig != jl_tuple_type) { // aka Builtins (maybe sometimes OpaqueClosure too)
                    record_field_change((jl_value_t*)mc, (jl_value_t **)&mc->cache, jl_nothing);
                }
                record_field_change((jl_value_t*)mc, (jl_value_t **)&mc->leafcache, jl_an_empty_memory_any);
            }
            // TODO: prune any partitions and partition data that has been deleted in the current world
            //else if (jl_is_binding(v)) {
            //    jl_binding_t *b = (jl_binding_t*)v;
            //}
            //else if (jl_is_binding_partition(v)) {
            //    jl_binding_partition_t *bpart = (jl_binding_partition_t*)v;
            //}
        }
        char *data = (char*)jl_data_ptr(v);
        size_t i, np = layout->npointers;
        size_t fldidx = 1;
        for (i = 0; i < np; i++) {
            uint32_t ptr = jl_ptr_offset(t, i);
            size_t offset = jl_ptr_offset(t, i) * sizeof(jl_value_t*);
            while (offset >= (fldidx == layout->nfields ? jl_datatype_size(t) : jl_field_offset(t, fldidx)))
                fldidx++;
            int mutabl = !jl_field_isconst(t, fldidx - 1);
            jl_value_t *fld = get_replaceable_field(&((jl_value_t**)data)[ptr], mutabl);
            jl_queue_for_serialization_(s, fld, 1);
        }
    }

done_fields: ;

    // We've encountered an item we need to cache
    void **bp = ptrhash_bp(&serialization_order, v);
    assert(*bp == (void*)(uintptr_t)-2);
    arraylist_push(&serialization_queue, (void*) v);
    size_t idx = serialization_queue.len - 1;
    assert(serialization_queue.len < BITS_ITEM && "too many items to serialize");
    *bp = to_seroder_entry(idx);
}


static void jl_queue_for_serialization_(jl_serializer_state *s, jl_value_t *v, int recursive) JL_GC_DISABLED
{
    if (!jl_needs_serialization(s, v))
        return;
    jl_value_t *replacement = (jl_value_t*)ptrhash_get(&object_replace, v);
    if (replacement != HT_NOTFOUND) {
        jl_queue_for_serialization_(s, replacement, recursive);
        return;
    }

    jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
    // check early from errors, so we have a little bit of contextual state for debugging them
    if (t == jl_task_type) {
        jl_error("Task cannot be serialized");
    }
    if (s->incremental && needs_uniquing(v, s->query_cache) && t == jl_binding_type) {
        jl_binding_t *b = (jl_binding_t*)v;
        if (b->globalref == NULL)
            jl_error("Binding cannot be serialized"); // no way (currently) to recover its identity
    }
    if (jl_is_foreign_type(t) == 1) {
        jl_error("Cannot serialize instances of foreign datatypes");
    }
    if (dedup_by_content(v, t)) {
        jl_value_t *dup = jl_idset_get(content_dedup_list, content_dedup_keyset, v);
        if (dup == NULL) {
            ssize_t idx;
            content_dedup_list = jl_idset_put_key(content_dedup_list, v, &content_dedup_max, &idx);
            content_dedup_keyset = jl_idset_put_idx(content_dedup_list, content_dedup_keyset, idx);
        }
        else if (dup != v) {
            // an egal object is already queued: _backref_id redirects references to it
            v = dup;
        }
    }

    if (s->incremental && jl_is_datatype_singleton(t) && needs_uniquing(v, s->query_cache)) {
        jl_queue_for_serialization_(s, (jl_value_t*)t, 1);
        return;
    }

    void **bp = ptrhash_bp(&serialization_order, v);
    if (*bp != HT_NOTFOUND)
        return;
    if (recursive) {
        *bp = (void*)(uintptr_t)-1; // now enqueued
        arraylist_push(&object_worklist, (void*)v);
    }
    else {
        *bp = (void*)(uintptr_t)-2; // now being inserted
        jl_insert_into_serialization_queue(s, v, 0);
    }
}

// Link the queued symbols into a search tree for the system image, inserting each one where
// `symtab_lookup` would look for it (the order on hashes is circular, so the tree cannot be
// built by sorting)
static void jl_build_symtab(void) JL_NOTSAFEPOINT
{
    symtab_root = HT_NOTFOUND;
    for (size_t i = 0; i < serialization_queue.len; i++) {
        jl_sym_t *sym = (jl_sym_t*)serialization_queue.items[i];
        if (!jl_is_symbol(sym))
            continue;
        const char *str = jl_symbol_name(sym);
        void **slot = &symtab_root;
        while (*slot != HT_NOTFOUND) {
            jl_sym_t *node = (jl_sym_t*)*slot;
            intptr_t x = (intptr_t)(sym->hash - node->hash);
            if (x == 0)
                x = strncmp(str, jl_symbol_name(node), strlen(str));
            slot = ptrhash_bp(x < 0 ? &symtab_left : &symtab_right, node);
        }
        *slot = sym;
    }
}

// Do a pre-order traversal of the to-serialize worklist, in the identical order
// to the calls to jl_queue_for_serialization would occur in a purely recursive
// implementation, but without potentially running out of stack.
static void jl_serialize_reachable(jl_serializer_state *s) JL_CANSAFEPOINT JL_GC_DISABLED
{
    size_t i, prevlen = 0;
    while (object_worklist.len) {
        // reverse!(object_worklist.items, prevlen:end);
        // prevlen is the index of the first new object
        size_t mid = prevlen + (object_worklist.len - prevlen) / 2;
        for (i = prevlen; i < mid; i++) {
            size_t j = object_worklist.len - i + prevlen - 1;
            void *tmp = object_worklist.items[i];
            object_worklist.items[i] = object_worklist.items[j];
            object_worklist.items[j] = tmp;
        }
        prevlen = --object_worklist.len;
        jl_value_t *v = (jl_value_t*)object_worklist.items[prevlen];
        void **bp = ptrhash_bp(&serialization_order, (void*)v);
        assert(*bp == (void*)(uintptr_t)-1);
        *bp = (void*)(uintptr_t)-2; // now being inserted
        jl_insert_into_serialization_queue(s, v, 1);
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

// An entry of the fptrs section is 0, or the heap offset of a CodeInstance, with FPTR_INVOKE set if
// the function is its invoke wrapper
#define FPTR_INVOKE ((uintptr_t)1)

// Set the entry for native function `id` (counting from 1)
static void write_fptr(ios_t *s, int32_t id, uintptr_t v) JL_NOTSAFEPOINT
{
    ios_ensureroom(s, id * sizeof(void*));
    ios_seek(s, (id - 1) * sizeof(void*));
    write_uint(s, v);
}

#include "support/htable.inc"

static import_t get_import(ios_t *imports, size_t i) JL_NOTSAFEPOINT
{
    return jl_load_unaligned_i64(imports->buf + i * sizeof(import_t));
}

static uint_t import_hash(uintptr_t key, void *imports) JL_NOTSAFEPOINT
{
    return (uint_t)int64hash(get_import((ios_t*)imports, key - 2));
}

static int import_eq(void *a, void *b, void *imports) JL_NOTSAFEPOINT
{
    assert((uintptr_t)a - 2 < ((ios_t*)imports)->size / sizeof(import_t) &&
           (uintptr_t)b - 2 < ((ios_t*)imports)->size / sizeof(import_t));
    return get_import((ios_t*)imports, (uintptr_t)a - 2) == get_import((ios_t*)imports, (uintptr_t)b - 2);
}

static void **importset_lookup_bp_r_impl(htable_t *h, void *key, void *ctx, int key_owned) JL_NOTSAFEPOINT;
static void **importset_lookup_bp_r(htable_t *h, void *key, void *ctx) JL_NOTSAFEPOINT;
static void **importset_peek_bp_r(htable_t *h, void *key, void *ctx) JL_NOTSAFEPOINT;
HTPROT_R(importset, static JL_UNUSED)
HTIMPL_R(importset, import_hash, import_eq, _HTIMPL_IDENTITY_KEYALLOC, _HTIMPL_NOOP_KEYFREE)

// Returns a bind to the import `imp`, adding it to the image unless it already was
static uintptr_t add_import(jl_serializer_state *s, import_t imp) JL_NOTSAFEPOINT
{
    size_t n = s->imports->size / sizeof(import_t);
    write_uint64(s->imports, imp); // tentatively, so that it can be looked up
    void **bp = importset_bp_r(&s->import_set, (void*)(n + 2), s->imports);
    if (*bp == HT_NOTFOUND)
        *bp = (void*)(n + 2);
    else
        ios_trunc(s->imports, n * sizeof(import_t));
    size_t idx = (uintptr_t)*bp - 2;
    if (idx >= BIND_BIT >> BIND_LOW_BITS) {
        jl_safe_printf("ERROR: too many imports\n");
        abort();
    }
    return BIND_BIT | idx << BIND_LOW_BITS;
}

static uintptr_t add_function_import(jl_serializer_state *s, int kind, size_t idx) JL_NOTSAFEPOINT
{
    return add_import(s, IMPORT_ENCODE(IMPORT_FUNCTION, ((import_t)kind << IMPORT_FUNCTION_KIND_OFFSET) | idx));
}

// The dependency reference to `addr`, which is in the image described by `meta`
static uint64_t depref(jl_serializer_state *s, image_metadata_t *meta, const void *addr) JL_NOTSAFEPOINT
{
    uint64_t depsidx = jl_array_data(s->buildid_depmods_idxs, uint32_t)[meta->idx];
    uint64_t offset = (uintptr_t)addr - meta->base;
    if (depsidx >> DEPREF_DEPS_BITS || offset >> DEPREF_OFFSET_BITS) {
        jl_safe_printf("ERROR: reference to a dependency image does not fit\n");
        abort();
    }
    return (depsidx << DEPREF_OFFSET_BITS) | offset;
}

static uintptr_t add_image_bind(jl_serializer_state *s, image_metadata_t *meta, const void *addr) JL_NOTSAFEPOINT
{
    return add_import(s, IMPORT_ENCODE(IMPORT_IMAGE, depref(s, meta, addr)));
}

// The object that is written in place of `v`: `v` if it is queued, and otherwise the object
// recorded to replace it, or an egal copy, if it is deduplicated by content
static jl_value_t *written_copy(jl_value_t *v) JL_NOTSAFEPOINT
{
    if (ptrhash_get(&serialization_order, v) != HT_NOTFOUND)
        return v;
    jl_value_t *replacement = (jl_value_t*)ptrhash_get(&object_replace, v);
    if (replacement != HT_NOTFOUND)
        return written_copy(replacement);
    if (dedup_by_content(v, (jl_datatype_t*)jl_typeof(v))) {
        jl_value_t *dup = jl_idset_get(content_dedup_list, content_dedup_keyset, v);
        if (dup != NULL)
            return dup;
    }
    return v;
}

// Align the bits stream to `align`, and return the item for what is written to it next
static uintptr_t bits_alloc(jl_serializer_state *s, size_t align) JL_NOTSAFEPOINT
{
    uintptr_t pos = LLT_ALIGN(ios_pos(s->bits), align);
    write_padding(s->bits, pos - ios_pos(s->bits));
    return BITS_ITEM | pos;
}

// Write the layout of `dt`, if it has one, to the bits stream, and return the item for it
static uintptr_t write_layout(jl_serializer_state *s, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    if (dt->layout == NULL)
        return 0;
    size_t nf = dt->layout->nfields;
    size_t np = dt->layout->npointers;
    size_t fieldsize = 0;
    uint8_t is_foreign_type = dt->layout->flags.fielddesc_type == JL_FIELDDESC_FOREIGN;
    if (!is_foreign_type) {
        fieldsize = jl_fielddesc_size(dt->layout->flags.fielddesc_type);
    }
    char *flddesc = (char*)dt->layout;
    size_t fldsize = sizeof(jl_datatype_layout_t) + nf * fieldsize;
    if (!is_foreign_type && dt->layout->first_ptr != -1)
        fldsize += np * jl_fielddesc_ptr_size(dt->layout->flags.fielddesc_type);
    uintptr_t layout = bits_alloc(s, sizeof(void*));
    ios_write(s->bits, flddesc, fldsize);
    if (is_foreign_type) {
        // make sure we have space for the extra hidden pointers
        // zero them since they will need to be re-initialized externally
        assert(fldsize == sizeof(jl_datatype_layout_t));
        jl_fielddescdyn_t dyn = {0, 0};
        ios_write(s->bits, (char*)&dyn, sizeof(jl_fielddescdyn_t));
    }
    return layout;
}

// Return the item for `v`: a bind, or its serialization id
#define backref_id(s, v) _backref_id(s, (jl_value_t*)(v))
static uintptr_t _backref_id(jl_serializer_state *s, jl_value_t *v) JL_GC_DISABLED JL_CANSAFEPOINT
{
    assert(v != NULL && "cannot get backref to NULL object");
    if (s->incremental && jl_is_symbol(v) && !jl_object_in_image(v)) {
        void **bp = ptrhash_bp(&s->object_imports, v);
        if (*bp == HT_NOTFOUND) {
            size_t pos = s->symbols->size;
            ios_write(s->symbols, jl_symbol_name((jl_sym_t*)v), strlen(jl_symbol_name((jl_sym_t*)v)) + 1);
            *bp = (void*)add_import(s, IMPORT_ENCODE(IMPORT_SYMBOL, pos));
        }
        return (uintptr_t)*bp;
    }
    if (v == (jl_value_t*)s->ptls->root_task)
        return add_import(s, IMPORT_ENCODE(IMPORT_SPECIAL, 0));
    if (s->incremental && jl_object_in_image(v))
        return add_image_bind(s, external_blob_metadata(v), v);
    if (s->incremental && jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(v)) && needs_uniquing(v, s->query_cache)) {
        uintptr_t dt = backref_id(s, jl_typeof(v));
        assert((dt & BIND_BIT) && get_import(s->imports, BIND_PAYLOAD(dt)) >> IMPORT_TYPE_OFFSET == IMPORT_DATATYPE &&
               "the type of a uniqued singleton has a record");
        return add_import(s, IMPORT_ENCODE(IMPORT_SINGLETON, BIND_PAYLOAD(dt)));
    }
    jl_value_t *w = written_copy(v);
    void *idx = ptrhash_get(&serialization_order, w);
    if (idx == HT_NOTFOUND) {
        jl_(jl_typeof(v));
        jl_(v);
    }
    assert(idx != HT_NOTFOUND && "object missed during jl_queue_for_serialization pass");
    assert(idx != (void*)(uintptr_t)-1 && "object missed during jl_insert_into_serialization_queue pass");
    assert(idx != (void*)(uintptr_t)-2 && "object missed during jl_insert_into_serialization_queue pass");
    if (s->incremental && needs_uniquing(v, s->query_cache)) {
        // the first reference to an object that is written as a record places its record
        void **bp = ptrhash_bp(&s->object_imports, w);
        if (*bp == HT_NOTFOUND) {
            int type = jl_is_datatype(w) ? IMPORT_DATATYPE : jl_is_method_instance(w) ? IMPORT_METHOD_INSTANCE : IMPORT_BINDING;
            *bp = (void*)add_import(s, IMPORT_ENCODE(type, auxdata_size));
            auxdata_size += jl_is_datatype(w) ? sizeof(jl_type_record_t) :
                            jl_is_binding(w) ? sizeof(jl_binding_record_t) : sizeof(jl_mi_record_t);
            arraylist_push(&uniquing_records, w);
        }
        return (uintptr_t)*bp;
    }
    return from_seroder_entry(idx);
}

#define VISITING ((void*)2) // not 1, which is HT_NOTFOUND
#define VISITED ((void*)3)

// Add the imports of the types and singletons that need uniquing among `v` and what the lookup of
// a type compares by identity through it (its parameters, and the types of values), each after the
// ones it reaches, so that the loader can resolve them in import order (see
// doc/src/devdocs/sysimg.md).
// This follows the objects as they are written, after deduplication and field replacements, and
// stops at objects of other images.  `visited` maps an object to VISITING, then to VISITED.
static void add_uniquing_imports(jl_serializer_state *s, jl_value_t *v, htable_t *visited) JL_GC_DISABLED JL_CANSAFEPOINT
{
    if (v == NULL || !jl_needs_serialization(s, v))
        return;
    v = written_copy(v);
    void *state = ptrhash_get(visited, v);
    if (state != HT_NOTFOUND) {
        assert((state == VISITED || !needs_uniquing(v, s->query_cache)) && "a type that needs uniquing reaches itself");
        return;
    }
    ptrhash_put(visited, v, VISITING);
#define FIELD(T, x, f) get_replaceable_field((jl_value_t**)&((T*)(x))->f, 0)
    if (jl_is_datatype(v)) {
        add_uniquing_imports(s, FIELD(jl_datatype_t, v, parameters), visited);
    }
    else if (jl_is_some_Type(v)) {
        add_uniquing_imports(s, jl_some_Type_T(v), visited);
    }
    else if (jl_is_svec(v)) {
        for (size_t i = 0; i < jl_svec_len(v); i++)
            add_uniquing_imports(s, jl_svecref(v, i), visited);
    }
    else if (jl_is_uniontype(v)) {
        add_uniquing_imports(s, FIELD(jl_uniontype_t, v, a), visited);
        add_uniquing_imports(s, FIELD(jl_uniontype_t, v, b), visited);
    }
    else if (jl_is_unionall(v)) {
        add_uniquing_imports(s, FIELD(jl_unionall_t, v, var), visited);
        add_uniquing_imports(s, FIELD(jl_unionall_t, v, body), visited);
    }
    else if (jl_is_typevar(v)) {
        add_uniquing_imports(s, FIELD(jl_tvar_t, v, lb), visited);
        add_uniquing_imports(s, FIELD(jl_tvar_t, v, ub), visited);
    }
    else if (jl_is_vararg(v)) {
        add_uniquing_imports(s, FIELD(jl_vararg_t, v, T), visited);
        add_uniquing_imports(s, FIELD(jl_vararg_t, v, N), visited);
    }
    else {
        add_uniquing_imports(s, jl_typeof(v), visited);
    }
#undef FIELD
    if (needs_uniquing(v, s->query_cache))
        backref_id(s, v);
    ptrhash_put(visited, v, VISITED); // the visits above may have moved the slot of `v`
}

static void record_reloc(jl_serializer_state *s, uintptr_t pos, uintptr_t target)
{
    arraylist_push(&s->relocs_list, (void *)pos);
    arraylist_push(&s->relocs_list, (void *)target);
}

static void record_pointerfield(jl_serializer_state *s, void *fld, uintptr_t pos) JL_CANSAFEPOINT JL_GC_DISABLED
{
    if (fld != NULL) {
        record_reloc(s, pos, backref_id(s, fld));
    }
}

// Save blank space in stream `s` for a pointer `fld`, storing both location and target
// in `relocs_list`.
static void write_pointerfield(jl_serializer_state *s, jl_value_t *fld) JL_CANSAFEPOINT JL_GC_DISABLED
{
    record_pointerfield(s, fld, (uintptr_t)ios_pos(s->s));
    write_pointer(s->s);
}


// Special handling from `jl_write_values` for modules
static void jl_write_module(jl_serializer_state *s, uintptr_t item, jl_module_t *m) JL_CANSAFEPOINT JL_GC_DISABLED
{
    size_t reloc_offset = ios_pos(s->s);
    size_t tot = sizeof(jl_module_t);
    ios_write(s->s, (char*)m, tot);     // raw memory dump of the `jl_module_t` structure
    // will need to recreate the binding table for this
    arraylist_push(&s->fixup_objs, (void*)reloc_offset);

    // Handle the fields requiring special attention
    jl_module_t *newm = (jl_module_t*)&s->s->buf[reloc_offset];
    newm->name = NULL;
    record_pointerfield(s, m->name, reloc_offset + offsetof(jl_module_t, name));
    newm->parent = NULL;
    record_pointerfield(s, m->parent, reloc_offset + offsetof(jl_module_t, parent));
    jl_atomic_store_relaxed(&newm->bindings, NULL);
    record_pointerfield(s, jl_atomic_load_relaxed(&m->bindings),
                        reloc_offset + offsetof(jl_module_t, bindings));
    jl_atomic_store_relaxed(&newm->bindingkeyset, NULL);
    record_pointerfield(s, jl_atomic_load_relaxed(&m->bindingkeyset),
                        reloc_offset + offsetof(jl_module_t, bindingkeyset));
    newm->file = NULL;
    record_pointerfield(s, jl_options.strip_metadata ? jl_empty_sym : m->file,
                        reloc_offset + offsetof(jl_module_t, file));
    if (jl_options.strip_metadata)
        newm->line = 0;
    newm->usings_backedges = NULL;
    record_pointerfield(s, get_replaceable_field(&m->usings_backedges, 1),
                        reloc_offset + offsetof(jl_module_t, usings_backedges));
    newm->scanned_methods = NULL;
    record_pointerfield(s, get_replaceable_field(&m->scanned_methods, 1),
                        reloc_offset + offsetof(jl_module_t, scanned_methods));

    // After reload, everything that has happened in this process happened semantically at
    // (for .incremental) or before jl_require_world, so reset this flag.
    jl_atomic_store_relaxed(&newm->export_set_changed_since_require_world, 0);

    // write out the usings list: in `usings._space`, or after the module if it does not fit there
    memset(&newm->usings._space, 0, sizeof(newm->usings._space));
    static_assert(sizeof(struct _jl_module_using) == 4*sizeof(void*), "_jl_module_using mismatch");
    size_t usings = offsetof(jl_module_t, usings._space);
    if (m->usings.items != &m->usings._space[0]) {
        usings = tot;
        tot += module_usings_max(m) * sizeof(struct _jl_module_using);
        write_padding(s->s, tot - usings);
        newm = (jl_module_t*)&s->s->buf[reloc_offset]; // the stream may have moved
    }
    newm->usings.items = (void**)usings;
    // the target is this Module, plus the offset written to `usings.items`
    record_reloc(s, reloc_offset + offsetof(jl_module_t, usings.items), item);
    for (size_t i = 0; i < module_usings_length(m); i++) {
        struct _jl_module_using *data = module_usings_getidx(m, i);
        size_t pos = reloc_offset + usings + i * sizeof(struct _jl_module_using);
        struct _jl_module_using *newdata = (struct _jl_module_using*)&s->s->buf[pos];
        newdata->min_world = data->min_world;
        newdata->max_world = data->max_world;
        newdata->flags = data->flags;
        if (s->incremental) {
            // TODO: Drop dead ones entirely?
            if (data->max_world != ~(size_t)0)
                newdata->max_world = 0;
            newdata->min_world = jl_require_world;
        }
        record_pointerfield(s, data->mod, pos);
    }
    assert(ios_pos(s->s) - reloc_offset == tot);
}

static void record_memoryref(jl_serializer_state *s, size_t reloc_offset, jl_genericmemoryref_t ref) {
    jl_genericmemoryref_t *newref = (jl_genericmemoryref_t*)&s->s->buf[reloc_offset];
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(ref.mem))->layout;
    if (layout->flags.arrayelem_isunion || layout->size == 0)
        return;
    if (s->incremental && jl_object_in_image((jl_value_t*)ref.mem)) {
        newref->ptr_or_offset = NULL;
        record_reloc(s, reloc_offset + offsetof(jl_genericmemoryref_t, ptr_or_offset),
                     add_image_bind(s, external_blob_metadata((jl_value_t*)ref.mem), ref.ptr_or_offset));
    }
    else {
        newref->ptr_or_offset = (void*)((char*)ref.ptr_or_offset - (char*)ref.mem->ptr);
        arraylist_push(&s->memref_list, (void*)reloc_offset);
    }
}

static void record_memoryrefs_inside(jl_serializer_state *s, jl_datatype_t *t, size_t reloc_offset, const char *data)
{
    assert(jl_is_datatype(t));
    size_t i, nf = jl_datatype_nfields(t);
    for (i = 0; i < nf; i++) {
        size_t offset = jl_field_offset(t, i);
        if (jl_field_isptr(t, i))
            continue;
        jl_value_t *ft = jl_field_type_concrete(t, i);
        if (jl_is_uniontype(ft))
            continue;
        // a `Type{Union{}}` field is laid out as the singleton `typeof(Union{})`,
        // so normalize it before recursing (it is otherwise a non-`DataType` kind)
        ft = normalize_typeofbottom_layout_alias(ft);
        if (jl_is_genericmemoryref_type(ft))
            record_memoryref(s, reloc_offset + offset, *(jl_genericmemoryref_t*)(data + offset));
        else
            record_memoryrefs_inside(s, (jl_datatype_t*)ft, reloc_offset + offset, data + offset);
    }
}

static void record_gvars(jl_serializer_state *s, arraylist_t *globals) JL_CANSAFEPOINT JL_GC_DISABLED
{
    for (size_t i = 0; i < globals->len; i++)
        jl_queue_for_serialization(s, globals->items[i]);
}

// How the loader sets the native code of the CodeInstance `ci`: returns its invoke as
// `jl_get_function_id` encodes it (a function of the native image, or minus the calling convention
// of a runtime function), or 0 if the loader leaves it unset; its specptr is the function
// `specfptr_id` of the native image, or the builtin `builtin_idx`.
static int32_t ci_native_code(jl_code_instance_t *ci, int *builtin_idx, int32_t *specfptr_id)
{
    int32_t invokeptr_id = 0;
    *builtin_idx = -1;
    *specfptr_id = 0;
    if (jl_atomic_load_relaxed(&ci->invoke) == jl_fptr_const_return)
        return -JL_INVOKE_CONST;
    if (jl_is_method(jl_get_ci_mi(ci)->def.method)) {
        *builtin_idx = jl_builtin_idx(jl_atomic_load_relaxed(&ci->specptr.fptr));
        if (*builtin_idx >= 0)
            return -JL_INVOKE_ARGS;
        jl_get_function_id(native_functions, ci, &invokeptr_id, specfptr_id); // see if we generated code for it
        assert(invokeptr_id != -JL_INVOKE_SPECSIG);
    }
    return invokeptr_id;
}

enum { PASS_WRITTEN, PASS_SYMBOLS, PASS_CODE_INSTANCES, PASS_OTHERS, NPASSES };

static int write_pass(jl_serializer_state *s, jl_value_t *v)
{
    if (jl_is_module(v))
        return PASS_WRITTEN;
    if (jl_is_symbol(v))
        return PASS_SYMBOLS;
    if (!jl_is_code_instance(v))
        return PASS_OTHERS;
    jl_code_instance_t *ci = (jl_code_instance_t*)v;
    int builtin_idx;
    int32_t specfptr_id;
    if ((s->incremental && jl_atomic_load_relaxed(&ci->max_world) == ~(size_t)0) ||
        ci_native_code(ci, &builtin_idx, &specfptr_id) != 0)
        return PASS_WRITTEN;
    return PASS_CODE_INSTANCES;
}

// The main function for serializing all the items queued in `serialization_order`
// (They are also stored in `serialization_queue` which is order-preserving, unlike the hash table used
//  for `serialization_order`).
static void jl_write_values(jl_serializer_state *s) JL_CANSAFEPOINT JL_GC_DISABLED
{
    size_t l = serialization_queue.len;

    arraylist_new(&layout_table, 0);
    arraylist_grow(&layout_table, l);
    memset(layout_table.items, 0, l * sizeof(void*));

    // Serialize all entries, pass by pass, and in queue order within each pass
    uint8_t *passes = (uint8_t*)malloc_s(l);
    for (size_t item = 0; item < l; item++)
        passes[item] = write_pass(s, (jl_value_t*)serialization_queue.items[item]);
    for (size_t i = 0; i < NPASSES * l; i++) {
        size_t item = i % l;
        if (passes[item] != i / l)
            continue;
        jl_value_t *v = (jl_value_t*)serialization_queue.items[item];           // the object
        JL_GC_PROMISE_ROOTED(v);
        assert(!(s->incremental && jl_object_in_image(v)));
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        assert((!jl_is_datatype_singleton(t) || t->instance == v) && "detected singleton construction corruption");
        if (s->incremental && (jl_is_datatype(v) || jl_is_method_instance(v) || jl_is_binding(v)) &&
            needs_uniquing(v, s->query_cache))
            continue; // written as a record (see _backref_id)
        int mutabl = t->name->mutabl;
        ios_t *f = s->s;
        if (t->smalltag && t != jl_symbol_type) {
            if (t->layout->npointers == 0 || t == jl_string_type) {
                if (jl_datatype_nfields(t) == 0 || mutabl == 0 || t == jl_string_type) {
                    f = s->bits;
                }
            }
        }

        // realign stream to expected gc alignment (16 bytes) after tag
        uintptr_t skip_header_pos = ios_pos(f) + sizeof(jl_taggedvalue_t);
        uintptr_t object_id_expected = mutabl &&
                 t != jl_datatype_type &&
                 t != jl_typename_type &&
                 t != jl_string_type &&
                 t != jl_symbol_type &&
                 t != jl_simplevector_type &&
                 t != jl_module_type;
        if (object_id_expected)
            skip_header_pos += sizeof(size_t);
        write_padding(f, LLT_ALIGN(skip_header_pos, 16) - skip_header_pos);

        // write header
        if (object_id_expected)
            write_uint(f, jl_object_id(v));
        // a small tag is fixed by the runtime, so it is written in its final form; any other type
        // is relocated (and its object is not in the bits stream)
        if (!t->smalltag)
            record_reloc(s, ios_pos(f), backref_id(s, t));
        write_uint(f, ((uintptr_t)t->smalltag << 4) | GC_OLD_MARKED | GC_IN_IMAGE);
        size_t reloc_offset = ios_pos(f);
        assert(item < layout_table.len && layout_table.items[item] == NULL);
        layout_table.items[item] = (void*)(reloc_offset | (f == s->bits ? BITS_ITEM : 0)); // store the inverse mapping of `serialization_order` (`id` => object-as-streampos)

        if (s->incremental && needs_recaching(v, s->query_cache))
            arraylist_push(jl_is_datatype(v) ? &s->fixup_types : &s->fixup_objs, (void*)reloc_offset);

        // write data
        if (jl_is_array(v)) {
            assert(f == s->s);
            // Internal data for types in julia.h with `jl_array_t` field(s)
            jl_array_t *ar = (jl_array_t*)v;
            // copy header
            size_t headersize = sizeof(jl_array_t) + jl_array_ndims(ar)*sizeof(size_t);
            ios_write(f, (char*)v, headersize);
            // make some header modifications in-place
            jl_array_t *newa = (jl_array_t*)&f->buf[reloc_offset];
            newa->ref.mem = NULL; // relocation offset
            record_pointerfield(s, get_replaceable_field((jl_value_t **)&ar->ref.mem, 1),
                                reloc_offset + offsetof(jl_array_t, ref.mem));
            record_memoryref(s, reloc_offset + offsetof(jl_array_t, ref), ar->ref);
        }
        else if (jl_is_genericmemory(v)) {
            assert(f == s->s);
            // Internal data for types in julia.h with `jl_genericmemory_t` field(s)
            jl_genericmemory_t *m = (jl_genericmemory_t*)v;
            const jl_datatype_layout_t *layout = t->layout;
            size_t len = m->length;
            jl_array_t *trimmed = (jl_array_t*)ptrhash_get(&memory_trims, m);
            if (trimmed != HT_NOTFOUND)
                len = jl_array_nrows(trimmed);
            {
                size_t datasize = len * layout->size;
                size_t tot = datasize;
                int isbitsunion = layout->flags.arrayelem_isunion;
                if (isbitsunion)
                    tot += len;
                size_t headersize = sizeof(jl_genericmemory_t);
                // copy header
                ios_write(f, (char*)v, headersize);
                ((jl_genericmemory_t*)&f->buf[reloc_offset])->length = len;
                // write data
                if (!layout->flags.arrayelem_isboxed && layout->first_ptr < 0) {
                    // set owner to NULL
                    write_pointer(f);
                    // Non-pointer eltypes get encoded in the bits stream
                    size_t alignment_amt = JL_SMALL_BYTE_ALIGNMENT;
                    if (tot >= ARRAY_CACHE_ALIGN_THRESHOLD)
                        alignment_amt = JL_CACHE_BYTE_ALIGNMENT;
                    // write data and relocations
                    jl_genericmemory_t *new_mem = (jl_genericmemory_t*)&f->buf[reloc_offset];
                    new_mem->ptr = NULL; // relocation offset
                    record_reloc(s, reloc_offset + offsetof(jl_genericmemory_t, ptr), bits_alloc(s, alignment_amt));
                    jl_value_t *et = jl_tparam1(t);
                    if (jl_is_cpointer_type(et)) {
                        // reset Ptr fields to C_NULL (but keep MAP_FAILED / INVALID_HANDLE)
                        const intptr_t *data = (const intptr_t*)m->ptr;
                        size_t i;
                        for (i = 0; i < len; i++) {
                            if (data[i] != -1)
                                write_pointer(s->bits);
                            else
                                ios_write(s->bits, (char*)&data[i], sizeof(data[i]));
                        }
                    }
                    else {
                        if (isbitsunion) {
                            ios_write(s->bits, (char*)m->ptr, datasize);
                            ios_write(s->bits, jl_genericmemory_typetagdata(m), len);
                        }
                        else {
                            ios_write(s->bits, (char*)m->ptr, tot);
                        }
                    }
                    if (len == 0) { // TODO: should we have a zero-page, instead of writing each type's fragment separately?
                        write_padding(s->bits, layout->size ? layout->size : isbitsunion);
                    }
                    else if (jl_genericmemory_how(m) == JL_GENERICMEMORY_STRINGOWNED) {
                        assert(jl_is_string(jl_genericmemory_data_owner_field(m)));
                        write_padding(s->bits, 1);
                    }
                }
                else {
                    // Pointer eltypes are encoded in the mutable data section
                    headersize = LLT_ALIGN(headersize, JL_SMALL_BYTE_ALIGNMENT);
                    size_t data = LLT_ALIGN(ios_pos(f), JL_SMALL_BYTE_ALIGNMENT);
                    write_padding(f, data - ios_pos(f));
                    assert(reloc_offset + headersize == ios_pos(f));
                    jl_genericmemory_t *new_mem = (jl_genericmemory_t*)&f->buf[reloc_offset];
                    new_mem->ptr = (void*)headersize; // relocation offset
                    record_reloc(s, reloc_offset + offsetof(jl_genericmemory_t, ptr),
                                 item);
                    if (!layout->flags.arrayelem_isboxed) {
                        // copy all of the data first
                        const char *data = (const char*)m->ptr;
                        ios_write(f, data, datasize);
                        // the rewrite all of the embedded pointers to null+relocation
                        uint16_t elsz = layout->size;
                        size_t j, np = layout->first_ptr < 0 ? 0 : layout->npointers;
                        size_t i;
                        for (i = 0; i < len; i++) {
                            for (j = 0; j < np; j++) {
                                size_t offset = i * elsz + jl_ptr_offset(t, j) * sizeof(jl_value_t*);
                                jl_value_t *fld = get_replaceable_field((jl_value_t**)&data[offset], 1);
                                size_t fld_pos = reloc_offset + headersize + offset;
                                record_pointerfield(s, fld, fld_pos);
                                memset(&f->buf[fld_pos], 0, sizeof(fld)); // relocation offset (none)
                            }
                        }
                    }
                    else {
                        jl_value_t **data = (jl_value_t**)m->ptr;
                        size_t i;
                        for (i = 0; i < len; i++) {
                            jl_value_t *e = get_replaceable_field(&data[i], 1);
                            write_pointerfield(s, e);
                        }
                    }
                }
            }
        }
        else if (jl_typeis(v, jl_module_type)) {
            assert(f == s->s);
            jl_write_module(s, item, (jl_module_t*)v);
        }
        else if (jl_typetagis(v, jl_task_tag << 4)) {
            abort(); // unreachable
        }
        else if (jl_is_svec(v)) {
            assert(f == s->s);
            ios_write(f, (char*)v, sizeof(void*));
            size_t ii, l = jl_svec_len(v);
            assert(l > 0 || (jl_svec_t*)v == jl_emptysvec);
            for (ii = 0; ii < l; ii++) {
                write_pointerfield(s, jl_svecref(v, ii));
            }
        }
        else if (jl_is_symbol(v)) {
            jl_sym_t *sym = (jl_sym_t*)v;
            void *left = ptrhash_get(&symtab_left, sym), *right = ptrhash_get(&symtab_right, sym);
            write_pointerfield(s, left == HT_NOTFOUND ? NULL : (jl_value_t*)left);
            write_pointerfield(s, right == HT_NOTFOUND ? NULL : (jl_value_t*)right);
            write_uint(f, sym->hash);
            ios_write(f, jl_symbol_name(sym), strlen(jl_symbol_name(sym)) + 1);
        }
        else if (jl_is_string(v)) {
            ios_write(f, (char*)v, sizeof(void*) + jl_string_len(v));
            write_uint8(f, '\0'); // null-terminated strings for easier C-compatibility
        }
        else if (jl_is_cancel_source(v)) {
            // Variable-sized. Store parents, reset children (reconstructed on load).
            assert(f == s->s);
            jl_cancel_source_t *cs = (jl_cancel_source_t*)v;
            write_pointerfield(s, jl_nothing); // child_head (weak; reset)
            write_pointerfield(s, jl_nothing); // waiters_head (transient; reset)
            write_pointerfield(s, jl_nothing); // walk_lock (transient; reset)
            write_uint8(f, jl_atomic_load_relaxed(&cs->state));
            write_padding(f, offsetof(jl_cancel_source_t, nparents) - 3 * sizeof(void*) - sizeof(uint8_t));
            ios_write(f, (char*)&cs->nparents, sizeof(uint16_t));
            write_padding(f, sizeof(jl_cancel_source_t) - offsetof(jl_cancel_source_t, nparents) - sizeof(uint16_t)); // incl. dead_count + reg_count (transient; reset)
            jl_cancel_parent_link_t *links = jl_cancel_source_links(cs);
            for (size_t i = 0; i < cs->nparents; i++) {
                write_pointerfield(s, (jl_value_t*)links[i].parent);
                write_pointerfield(s, jl_nothing); // next (weak; reset)
                write_pointer(f);                  // pprev (reset; set by relink)
            }
            // relink under the parents on load
            arraylist_push(&s->fixup_objs, (void*)reloc_offset);
        }
        else if (jl_is_wait_entry(v)) {
            // Variable-sized; entirely transient wait state - reset on write.
            assert(f == s->s);
            jl_wait_entry_t *w = (jl_wait_entry_t*)v;
            write_pointerfield(s, jl_nothing); // task (transient; reset)
            ios_write(f, (char*)&w->nslots, sizeof(uint32_t));
            write_padding(f, sizeof(jl_wait_entry_t) - offsetof(jl_wait_entry_t, nslots) - sizeof(uint32_t));
            for (size_t i = 0; i < w->nslots; i++) {
                write_pointerfield(s, jl_nothing); // owner (transient; reset)
                write_pointerfield(s, jl_nothing); // next (transient; reset)
                write_padding(f, sizeof(uint64_t)); // aux (transient; reset)
            }
        }
        else if (jl_is_foreign_type(t) == 1) {
            abort(); // unreachable
        }
        else if (jl_datatype_nfields(t) == 0) {
            // The object has no fields, so we just snapshot its byte representation
            assert(t->layout->npointers == 0);
            ios_write(f, (char*)v, jl_datatype_size(t));
        }
        else if (jl_bigint_type && jl_typetagis(v, jl_bigint_type)) {
            // foreign types require special handling
            assert(f == s->s);
            jl_value_t *sizefield = jl_get_nth_field(v, 1);
            int32_t sz = jl_unbox_int32(sizefield);
            int32_t nw = (sz == 0 ? 1 : (sz < 0 ? -sz : sz));
            size_t nb = nw * gmp_limb_size;
            ios_write(f, (char*)&nw, sizeof(int32_t));
            ios_write(f, (char*)&sz, sizeof(int32_t));
            record_reloc(s, reloc_offset + 8, bits_alloc(s, 8));
            void *pdata = jl_unbox_voidpointer(jl_get_nth_field(v, 2));
            ios_write(s->bits, (char*)pdata, nb);
            write_pointer(f);
        }
        else {
            // Generic object::DataType serialization by field
            const char *data = (const char*)v;
            size_t i, nf = jl_datatype_nfields(t);
            size_t tot = 0;
            for (i = 0; i < nf; i++) {
                size_t offset = jl_field_offset(t, i);
                const char *slot = data + offset;
                write_padding(f, offset - tot);
                tot = offset;
                size_t fsz = jl_field_size(t, i);
                jl_value_t *replace = (jl_value_t*)ptrhash_get(&bits_replace, (void*)slot);
                if (replace != HT_NOTFOUND && fsz > 0) {
                    assert(t->name->mutabl && !jl_field_isptr(t, i));
                    jl_value_t *rty = jl_typeof(replace);
                    size_t sz = jl_datatype_size(rty);
                    ios_write(f, (const char*)replace, sz);
                    jl_value_t *ft = jl_field_type_concrete(t, i);
                    int isunion = jl_is_uniontype(ft);
                    unsigned nth = 0;
                    if (!jl_find_union_component(ft, rty, &nth))
                        assert(0 && "invalid field assignment to isbits union");
                    assert(sz <= fsz - isunion);
                    write_padding(f, fsz - sz - isunion);
                    if (isunion)
                        write_uint8(f, nth);
                }
                else if (!jl_field_isconst(t, i) && jl_is_cpointer_type(jl_field_type_concrete(t, i)) && *(intptr_t*)slot != -1) {
                    // reset non-const Ptr fields to C_NULL (but keep MAP_FAILED / INVALID_HANDLE)
                    assert(!jl_field_isptr(t, i));
                    write_pointer(f);
                }
                else if (fsz > 0) {
                    ios_write(f, slot, fsz);
                }
                tot += fsz;
            }

            size_t np = t->layout->npointers;
            size_t fldidx = 1;
            for (i = 0; i < np; i++) {
                size_t offset = jl_ptr_offset(t, i) * sizeof(jl_value_t*);
                while (offset >= (fldidx == nf ? jl_datatype_size(t) : jl_field_offset(t, fldidx)))
                    fldidx++;
                int mutabl = !jl_field_isconst(t, fldidx - 1);
                jl_value_t *fld = get_replaceable_field((jl_value_t**)&data[offset], mutabl);
                size_t fld_pos = offset + reloc_offset;
                record_pointerfield(s, fld, fld_pos);
                memset(&f->buf[fld_pos], 0, sizeof(fld)); // relocation offset (none)
            }

            // Need do a tricky fieldtype walk an record all memoryref we find inlined in this value
            record_memoryrefs_inside(s, t, reloc_offset, data);

            // A few objects need additional handling beyond the generic serialization above
            if (s->incremental && jl_typetagis(v, jl_typemap_entry_type)) {
                assert(f == s->s);
                jl_typemap_entry_t *newentry = (jl_typemap_entry_t*)&s->s->buf[reloc_offset];
                if (jl_atomic_load_relaxed(&newentry->max_world) == ~(size_t)0) {
                    if (jl_atomic_load_relaxed(&newentry->min_world) > 1) {
                        jl_atomic_store_relaxed(&newentry->min_world, ~(size_t)0);
                        jl_atomic_store_relaxed(&newentry->max_world, WORLD_AGE_REVALIDATION_SENTINEL);
                        arraylist_push(&s->fixup_objs, (void*)reloc_offset);
                    }
                }
                else {
                    // garbage newentry - delete it :(
                    jl_atomic_store_relaxed(&newentry->min_world, 1);
                    jl_atomic_store_relaxed(&newentry->max_world, 0);
                }
            }
            else if (s->incremental && jl_is_binding_partition(v)) {
                jl_binding_partition_t *newbpart = (jl_binding_partition_t*)&s->s->buf[reloc_offset];
                size_t max_world = jl_atomic_load_relaxed(&newbpart->max_world);
                if (max_world == ~(size_t)0) {
                    // Still valid. Will be considered to be defined in jl_require_world
                    // after reload, which is the first world before new code runs.
                    // We use this as a quick check to determine whether a binding was
                    // invalidated. If a binding was first defined in or before
                    // jl_require_world, then we can assume that all precompile processes
                    // will have seen it consistently.
                    jl_atomic_store_relaxed(&newbpart->min_world, jl_require_world);
                }
                else {
                    // The world will not be reachable after loading
                    jl_atomic_store_relaxed(&newbpart->min_world, 1);
                    jl_atomic_store_relaxed(&newbpart->max_world, 0);
                }
            }
            else if (jl_is_unionall(v)) {
                assert(f == s->s);
                // hidden field: the memoized facts are structural, so they stay valid
                write_padding(f, offsetof(jl_unionall_t, flags) - tot);
                write_uint32(f, jl_atomic_load_relaxed(&((jl_unionall_t*)v)->flags));
                write_padding(f, sizeof(jl_unionall_t) - offsetof(jl_unionall_t, flags) - sizeof(uint32_t));
            }
            else if (jl_is_method(v)) {
                assert(f == s->s);
                write_padding(f, sizeof(jl_method_t) - tot); // hidden fields
                jl_method_t *m = (jl_method_t*)v;
                jl_method_t *newm = (jl_method_t*)&f->buf[reloc_offset];
                if (s->incremental) {
                    if (jl_atomic_load_relaxed(&newm->primary_world) > 1) {
                        jl_atomic_store_relaxed(&newm->primary_world, ~(size_t)0); // min-world
                        // Keep METHOD_SIG_NO_LOSERS across serialization, but re-derive other bits on activation
                        int dispatch_status = jl_atomic_load_relaxed(&newm->dispatch_status);
                        jl_atomic_store_relaxed(&newm->dispatch_status, dispatch_status & METHOD_SIG_NO_LOSERS);
                        arraylist_push(&s->fixup_objs, (void*)reloc_offset);
                    }
                }
                else {
                    newm->nroots_sysimg = m->roots ? jl_array_len(m->roots) : 0;
                }
            }
            else if (jl_is_method_instance(v)) {
                assert(f == s->s);
                jl_method_instance_t *newmi = (jl_method_instance_t*)&f->buf[reloc_offset];
                jl_atomic_store_relaxed(&newmi->flags, 0);
                if (s->incremental) {
                    jl_atomic_store_relaxed(&newmi->dispatch_status, 0);
                }
            }
            else if (jl_is_code_instance(v)) {
                assert(f == s->s);

                // Handle the native-code pointers
                jl_code_instance_t *ci = (jl_code_instance_t*)v;
                jl_code_instance_t *newci = (jl_code_instance_t*)&f->buf[reloc_offset];

                if (s->incremental) {
                    if (jl_atomic_load_relaxed(&ci->max_world) == ~(size_t)0) {
                        //assert(jl_atomic_load_relaxed(&ci->edges) != jl_emptysvec); // some code (such as !==) might add a method lookup restriction but not keep the edges
                        jl_atomic_store_release(&newci->min_world, ~(size_t)0);
                        jl_atomic_store_release(&newci->max_world, WORLD_AGE_REVALIDATION_SENTINEL);
                        arraylist_push(&s->fixup_objs, (void*)reloc_offset);
                    }
                    else {
                        // garbage object - delete it :(
                        jl_atomic_store_release(&newci->min_world, 1);
                        jl_atomic_store_release(&newci->max_world, 0);
                    }
                }
                newci->time_infer_total = 0;
                newci->time_infer_cache_saved = 0;
                newci->time_infer_self = 0;
                jl_atomic_store_relaxed(&newci->time_compile, 0.0);
                jl_atomic_store_relaxed(&newci->invoke, NULL);
                // preserve only JL_CI_FLAGS_NATIVE_CACHE_VALID and JL_CI_FLAGS_UNIQUE_BACKEDGES bits
                jl_atomic_store_relaxed(&newci->flags, jl_atomic_load_relaxed(&newci->flags) & (JL_CI_FLAGS_NATIVE_CACHE_VALID | JL_CI_FLAGS_UNIQUE_BACKEDGES));
                jl_atomic_store_relaxed(&newci->specptr.fptr, NULL);
                int builtin_idx;
                int32_t specfptr_id;
                int32_t invokeptr_id = ci_native_code(ci, &builtin_idx, &specfptr_id);
                if (invokeptr_id > 0)
                    write_fptr(s->fptr_record, invokeptr_id, reloc_offset | FPTR_INVOKE);
                if (invokeptr_id && specfptr_id) {
                    assert(specfptr_id > invokeptr_id && specfptr_id > 0);
                    write_fptr(s->fptr_record, specfptr_id, reloc_offset);
                }
                if (invokeptr_id < 0)
                    record_reloc(s, reloc_offset + offsetof(jl_code_instance_t, invoke),
                                 add_function_import(s, FUNCTION_RUNTIME, -invokeptr_id));
                if (builtin_idx >= 0)
                    record_reloc(s, reloc_offset + offsetof(jl_code_instance_t, specptr.fptr),
                                 add_function_import(s, FUNCTION_BUILTIN, builtin_idx));
            }
            else if (jl_is_datatype(v)) {
                assert(f == s->s);
                jl_datatype_t *newdt = (jl_datatype_t*)&f->buf[reloc_offset];
                uintptr_t layout = write_layout(s, (jl_datatype_t*)v);
                if (layout != 0) {
                    newdt->layout = NULL; // relocation offset
                    record_reloc(s, reloc_offset + offsetof(jl_datatype_t, layout), layout);
                }
            }
            else if (jl_is_typename(v)) {
                assert(f == s->s);
                jl_typename_t *tn = (jl_typename_t*)v;
                jl_typename_t *newtn = (jl_typename_t*)&f->buf[reloc_offset];
                size_t nb = (jl_svec_len(tn->names) + 31) / 32 * sizeof(uint32_t);
                if (tn->atomicfields != NULL) {
                    newtn->atomicfields = NULL; // relocation offset
                    record_reloc(s, reloc_offset + offsetof(jl_typename_t, atomicfields), bits_alloc(s, sizeof(void*)));
                    ios_write(s->bits, (char*)tn->atomicfields, nb);
                }
                if (tn->constfields != NULL) {
                    newtn->constfields = NULL; // relocation offset
                    record_reloc(s, reloc_offset + offsetof(jl_typename_t, constfields), bits_alloc(s, sizeof(void*)));
                    ios_write(s->bits, (char*)tn->constfields, nb);
                }
            }
            else if (jl_is_genericmemoryref(v)) {
                assert(f == s->s);
                record_memoryref(s, reloc_offset, *(jl_genericmemoryref_t*)v);
            }
            else {
                write_padding(f, jl_datatype_size(t) - tot);
            }
        }
    }
    free(passes);
}

// Returns the reloc for `item` (see `backref_id`) plus `offset`: a bind, or the address of a slide
// when the heap is at `heap_base`
static uintptr_t get_reloc_for_item(uintptr_t item, size_t offset)
{
    if (item & BIND_BIT) {
        assert(offset <= BIND_LOW_MASK);
        return item | offset;
    }
    assert(bits_start != 0 && "bits stream not yet placed in the heap");
    if (!(item & BITS_ITEM)) {
        assert(item < layout_table.len);
        item = (uintptr_t)layout_table.items[item];
        assert(item != 0 && "layout offset missing for relocation item");
    }
    if (item & BITS_ITEM) {
        assert(offset == 0);
        return heap_base + bits_start + (item & ~BITS_ITEM);
    }
    return heap_base + item + offset;
}

// The slides and binds bitmaps, with one bit for each word of the heap before the bits stream
typedef struct {
    ios_t slides;
    ios_t binds;
} jl_relocmaps_t;

static size_t bitmap_nwords(size_t size) JL_NOTSAFEPOINT
{
    return (size / sizeof(void*) + 63) / 64;
}

#ifndef NDEBUG
static int bitmap_test(ios_t *bitmap, size_t pos) JL_NOTSAFEPOINT
{
    size_t w = pos / sizeof(void*);
    return (jl_load_unaligned_i64(bitmap->buf + w / 64 * sizeof(uint64_t)) >> (w % 64)) & 1;
}
#endif

static void bitmap_set(jl_relocmaps_t *m, ios_t *bitmap, size_t pos) JL_NOTSAFEPOINT
{
    assert(pos % sizeof(void*) == 0 && pos < bits_start && "relocation outside the heap or in the bits stream");
    assert(!bitmap_test(&m->slides, pos) && !bitmap_test(&m->binds, pos) && "location relocated twice");
    size_t w = pos / sizeof(void*);
    char *p = bitmap->buf + w / 64 * sizeof(uint64_t);
    jl_store_unaligned_i64(p, jl_load_unaligned_i64(p) | (uint64_t)1 << (w % 64));
}

static void relocmaps_init(jl_relocmaps_t *m) JL_NOTSAFEPOINT
{
    size_t nbytes = bitmap_nwords(bits_start) * sizeof(uint64_t);
    ios_mem(&m->slides, nbytes);
    ios_ensureroom(&m->slides, nbytes);
    ios_mem(&m->binds, nbytes);
    ios_ensureroom(&m->binds, nbytes);
}

// A binds or slides section is a bitmap when its size is exactly `nwords` u64s, and otherwise a
// list of gaps
static inline int relocs_is_bitmap(size_t size, size_t nwords) JL_NOTSAFEPOINT
{
    return size == nwords * sizeof(uint64_t);
}

static inline uint32_t relocs_read_gap(const uint8_t **p) JL_NOTSAFEPOINT
{
    uint32_t gap = *(*p)++;
    if (gap == 0) {
        memcpy(&gap, *p, sizeof(gap));
        *p += sizeof(gap);
    }
    return gap;
}

// Write the final value of each relocation in `list` into the heap at `base`, and mark it as a
// slide or a bind
static void jl_finish_relocs(char *base, arraylist_t *list, jl_relocmaps_t *m)
{
    for (size_t i = 0; i < list->len; i += 2) {
        size_t pos = (size_t)list->items[i];
        uintptr_t *pv = (uintptr_t*)(base + pos);
        *pv = get_reloc_for_item((uintptr_t)list->items[i + 1], *pv);
        bitmap_set(m, *pv & BIND_BIT ? &m->binds : &m->slides, pos);
    }
}

// Turn the offset in each MemoryRef in `list` into a slide, once its Memory's data is placed
static void jl_finish_memrefs(char *base, arraylist_t *list, jl_relocmaps_t *m)
{
    for (size_t i = 0; i < list->len; i++) {
        size_t pos = (size_t)list->items[i];
        jl_genericmemoryref_t *ref = (jl_genericmemoryref_t*)(base + pos);
        size_t mem_pos = (uintptr_t)ref->mem - heap_base;
        assert(bitmap_test(&m->slides, pos + offsetof(jl_genericmemoryref_t, mem)) &&
               bitmap_test(&m->slides, mem_pos + offsetof(jl_genericmemory_t, ptr)));
        ref->ptr_or_offset = (char*)((jl_genericmemory_t*)(base + mem_pos))->ptr + (uintptr_t)ref->ptr_or_offset;
        bitmap_set(m, &m->slides, pos + offsetof(jl_genericmemoryref_t, ptr_or_offset));
    }
}

static uintptr_t jl_resolve_depref(jl_loader_state *s, uint64_t ref) JL_NOTSAFEPOINT
{
    assert(s->deps && "the system image has no dependencies");
    return s->deps[ref >> DEPREF_OFFSET_BITS]->base + (ref & (((uint64_t)1 << DEPREF_OFFSET_BITS) - 1));
}

static uintptr_t jl_resolve_import(jl_loader_state *s, import_t imp, const char *strtab) JL_CANSAFEPOINT
{
    uint64_t payload = IMPORT_PAYLOAD(imp);
    switch (imp >> IMPORT_TYPE_OFFSET) {
    case IMPORT_IMAGE:
    case IMPORT_SPECPTR: {
        uintptr_t addr = jl_resolve_depref(s, payload);
        if (imp >> IMPORT_TYPE_OFFSET == IMPORT_IMAGE)
            return addr;
        return (uintptr_t)jl_atomic_load_relaxed(&((jl_code_instance_t*)addr)->specptr.fptr);
    }
    case IMPORT_SPECIAL: // the only special is the root task
        return (uintptr_t)jl_current_task->ptls->root_task;
    case IMPORT_FUNCTION: {
        size_t idx = payload & (((import_t)1 << IMPORT_FUNCTION_KIND_OFFSET) - 1);
        if (payload >> IMPORT_FUNCTION_KIND_OFFSET == FUNCTION_BUILTIN) {
            assert(idx < jl_n_builtins);
            return (uintptr_t)jl_builtin_f_addrs[idx];
        }
        assert(JL_INVOKE_ARGS <= idx && idx < JL_INVOKE_SPECSIG);
        return (uintptr_t)jl_invoke_api_callptr((jl_invoke_api_t)idx);
    }
    case IMPORT_DATATYPE:
    case IMPORT_SINGLETON:
    case IMPORT_METHOD_INSTANCE:
    case IMPORT_BINDING:
        return 0;
    case IMPORT_SYMBOL: {
        const char *name = strtab + payload;
        return (uintptr_t)_jl_symbol(name, strlen(name));
    }
    }
    assert(0 && "corrupt import");
    abort();
}

// Replace the bind at `pv` with what it refers to.  If that is a pending import, its value is
// provisional, and the word is rewritten if it changes when the import is finalized.
static void jl_fill_bind(jl_loader_state *s, uintptr_t *pv) JL_NOTSAFEPOINT
{
    uintptr_t bind = *pv;
    size_t idx = BIND_PAYLOAD(bind);
    *pv = s->imports_resolved[idx] | (bind & BIND_LOW_MASK);
    if (s->pending[idx]) {
        arraylist_push(&s->waiting, pv);
        arraylist_push(&s->waiting, (void*)s->first_waiting[idx]);
        s->first_waiting[idx] = s->waiting.len / 2;
    }
}

// Finalize the pending import `idx` to `v`, and rewrite the words filled from it, keeping the bits
// that were added to its value (the low bits of the bind, or the GC bits of a type tag)
static void jl_finalize_import(jl_loader_state *s, size_t idx, uintptr_t v) JL_NOTSAFEPOINT
{
    uintptr_t old = s->imports_resolved[idx];
    assert(s->pending[idx]);
    s->imports_resolved[idx] = v;
    s->pending[idx] = 0;
    if (v == old)
        return;
    for (size_t i = s->first_waiting[idx]; i; i = (size_t)s->waiting.items[2 * i - 1]) {
        uintptr_t *pv = (uintptr_t*)s->waiting.items[2 * i - 2];
        assert(*pv - old < 16 && "a word filled from a pending import was written with another value");
        *pv = v + (*pv - old);
    }
}

// Replace each of the `n` relocs at `p` by its value: 0 stays NULL, a bind is filled in, and
// anything else is a slide
static void jl_resolve_relocs(jl_loader_state *s, uintptr_t *p, size_t n) JL_NOTSAFEPOINT
{
    for (size_t i = 0; i < n; i++) {
        if (p[i] & BIND_BIT)
            jl_fill_bind(s, &p[i]);
        else if (p[i] != 0)
            p[i] += s->heap_delta;
    }
}

// jl_apply_slides and jl_apply_binds walk the two forms of a section in separate loops rather than
// with one iterator, whose test of the form for every word compilers do not hoist out of the
// loop.  NOINLINE to avoid some bad decisions by LLVM (very hot loops).
static NOINLINE void jl_apply_slides(uintptr_t *heap, uintptr_t delta, const char *sec, size_t size, size_t nwords) JL_NOTSAFEPOINT
{
    if (delta == 0)
        return;
    if (relocs_is_bitmap(size, nwords)) {
        // `words` advances with the bitmap, so that GCC can address heap words as words[bit]
        uintptr_t *words = heap;
        for (const char *end = sec + size; sec != end; sec += sizeof(uint64_t), words += 64) {
            for (uint64_t bits = jl_load_unaligned_i64(sec); bits; bits &= bits - 1)
                words[__builtin_ctzll(bits)] += delta;
        }
        return;
    }
    const uint8_t *p = (const uint8_t*)sec, *end = p + size;
    for (size_t word = 0; p != end;) {
        word += relocs_read_gap(&p);
        heap[word] += delta;
    }
}

static NOINLINE void jl_apply_binds(jl_loader_state *s, uintptr_t *heap, const char *sec, size_t size, size_t nwords) JL_NOTSAFEPOINT
{
    if (relocs_is_bitmap(size, nwords)) {
        uintptr_t *words = heap;
        for (const char *end = sec + size; sec != end; sec += sizeof(uint64_t), words += 64) {
            for (uint64_t bits = jl_load_unaligned_i64(sec); bits; bits &= bits - 1)
                jl_fill_bind(s, &words[__builtin_ctzll(bits)]);
        }
        return;
    }
    const uint8_t *p = (const uint8_t*)sec, *end = p + size;
    for (size_t word = 0; p != end;) {
        word += relocs_read_gap(&p);
        jl_fill_bind(s, &heap[word]);
    }
}

// Persistent set of image objects that reference non-image objects.
// Used to track GC reachability for mutable objects in the images,
// treating them as a third, "permanent" GC generation.
arraylist_t image_remset;
jl_mutex_t image_remset_lock;

static void jl_update_all_fptrs(jl_image_t *image, char *heap, const uintptr_t *offsets, size_t img_fvars_max)
{
    jl_image_fptrs_t fvars = image->fptrs;
    size_t i;
    uintptr_t base = (uintptr_t)heap;
    if (fvars.nptrs == 0) {
        // the native code is not used, so these CodeInstances must be compiled again
        for (i = 0; i < img_fvars_max; i++) {
            if (offsets[i])
                jl_atomic_store_relaxed(&((jl_code_instance_t*)(base + (offsets[i] & ~FPTR_INVOKE)))->invoke, NULL);
        }
        return;
    }

    memcpy(image->jl_small_typeof, &jl_small_typeof, sizeof(jl_small_typeof));
    // the CodeInstance of each function, kept for the debug info
    jl_code_instance_t **linfos = (jl_code_instance_t**)calloc_s(img_fvars_max * sizeof(void*));
    uint32_t clone_idx = 0;
    for (i = 0; i < img_fvars_max; i++) {
        uintptr_t offset = offsets[i];
        if (offset != 0) {
            int specfunc = !(offset & FPTR_INVOKE);
            jl_code_instance_t *codeinst = (jl_code_instance_t*)(base + (offset & ~FPTR_INVOKE));
            linfos[i] = codeinst;
            assert(jl_is_method(jl_get_ci_mi(codeinst)->def.method) && jl_atomic_load_relaxed(&codeinst->invoke) != jl_fptr_const_return);
            assert(specfunc ? jl_atomic_load_relaxed(&codeinst->invoke) != NULL : jl_atomic_load_relaxed(&codeinst->invoke) == NULL);
            void *fptr = fvars.ptrs[i];
            for (; clone_idx < fvars.nclones; clone_idx++) {
                uint32_t idx = fvars.clone_idxs[clone_idx] & jl_sysimg_val_mask;
                if (idx < i)
                    continue;
                if (idx == i)
                    fptr = fvars.clone_ptrs[clone_idx];
                break;
            }
            if (specfunc) {
                uint8_t flags = jl_atomic_load_relaxed(&codeinst->flags);
                flags |= JL_CI_FLAGS_INVOKE_MATCHES_SPECPTR | JL_CI_FLAGS_FROM_IMAGE;
                if (jl_callptr_invoke_api(jl_atomic_load_relaxed(&codeinst->invoke)) ==
                    JL_INVOKE_SPECSIG)
                    flags |= JL_CI_FLAGS_SPECPTR_SPECIALIZED;
                jl_atomic_store_relaxed(&codeinst->specptr.fptr, fptr);
                jl_atomic_store_relaxed(&codeinst->flags, flags);
            }
            else {
                jl_atomic_store_relaxed(&codeinst->invoke, (jl_callptr_t)fptr);
            }
        }
    }
    // Tell LLVM about the native code
    jl_register_fptrs(image->base, &fvars, linfos, img_fvars_max);
}

// The reloc for `v`: 0 for NULL, a bind, or otherwise the address of `v` when the heap is at
// `heap_base`
static uintptr_t reloc_for(jl_serializer_state *s, jl_value_t *v) JL_CANSAFEPOINT JL_GC_DISABLED
{
    return v ? get_reloc_for_item(backref_id(s, v), 0) : 0;
}

static void write_reloc(jl_serializer_state *s, ios_t *f, jl_value_t *v) JL_CANSAFEPOINT JL_GC_DISABLED
{
    write_uint(f, reloc_for(s, v));
}

// Write the gvars as relocs: the objects the native code uses, then the specialized
// functions of CodeInstances in other images that it calls
static size_t write_gvars(jl_serializer_state *s, ios_t *f, arraylist_t *globals, arraylist_t *external_fns) JL_CANSAFEPOINT JL_GC_DISABLED
{
    for (size_t i = 0; i < globals->len; i++)
        write_reloc(s, f, (jl_value_t*)globals->items[i]);
    for (size_t i = 0; i < external_fns->len; i++) {
        jl_code_instance_t *ci = (jl_code_instance_t*)external_fns->items[i];
        assert(ci && (jl_atomic_load_relaxed(&ci->flags) & JL_CI_FLAGS_SPECPTR_SPECIALIZED));
        uint64_t ref = depref(s, external_blob_metadata((jl_value_t*)ci), ci);
        write_uint(f, add_import(s, IMPORT_ENCODE(IMPORT_SPECPTR, ref)));
    }
    return globals->len;
}

// Copy the gvars into the native image's slots, rooting the objects of incremental images
// (the first `nobjects`)
static void jl_update_all_gvars(jl_image_t *image, uintptr_t *gvars, size_t l, size_t nobjects, int incremental) JL_CANSAFEPOINT
{
    if (image->gvars_base == NULL)
        return;
    assert(l == image->ngvars);
    for (size_t i = 0; i < l; i++) {
        jl_value_t *v = (jl_value_t*)gvars[i];
        if (incremental && i < nobjects && !jl_is_binding(v))
            v = jl_as_global_root(v, 1);
        *sysimg_gvars(image->gvars_base, image->gvars_offsets, i) = (uintptr_t)v;
    }
}

#ifndef NDEBUG
static int has_references(jl_value_t *v) JL_NOTSAFEPOINT
{
    jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
    if (jl_is_svec(v))
        return jl_svec_len(v) != 0;
    if (jl_is_genericmemory(v))
        return ((jl_genericmemory_t*)v)->length != 0 &&
               (t->layout->flags.arrayelem_isboxed || t->layout->first_ptr >= 0);
    return t->layout != NULL && t->layout->npointers != 0;
}
#endif

static void record_object_change(jl_value_t *v, jl_value_t *replacement) JL_NOTSAFEPOINT
{
    void *idx = ptrhash_get(&serialization_order, v);
    // to avoid enqueuing child objects that will become unreachable after object replacement,
    // this function has phase restrictions:
    if (!queueing_done) {
        // `v` can have a change recorded before it is enqueued
        assert(idx == HT_NOTFOUND && "object change recorded after its object was queued");
        ptrhash_put(&object_replace, v, replacement);
        return;
    }
    // or if `v` is already enqueued it must be non-recursive (i.e. children not enqueued)
    assert(idx != HT_NOTFOUND && idx != (void*)(uintptr_t)-1 && idx != (void*)(uintptr_t)-2 &&
           "object change recorded for an object that is not written");
    assert((ptrhash_has(&queued_without_refs, v) || !has_references(v)) &&
           "object change recorded for an object queued with the objects it refers to");
    assert(serialization_queue.items[from_seroder_entry(idx)] == v);
    serialization_queue.items[from_seroder_entry(idx)] = replacement;
    // also reserve a slot for `replacement` itself, if needed
    if (ptrhash_get(&serialization_order, replacement) == HT_NOTFOUND)
        ptrhash_put(&serialization_order, replacement, idx);
}

// Code below helps slim down the images by
// removing cached types not referenced in the stream
static void jl_prune_type_cache_hash(jl_svec_t *cache) JL_CANSAFEPOINT JL_GC_DISABLED
{
    size_t l = jl_svec_len(cache), i;
    size_t sz = 0;
    if (l == 0)
        return;
    jl_svec_t *kept = jl_svec_copy(cache);
    for (i = 0; i < l; i++) {
        jl_value_t *ti = jl_svecref(kept, i);
        if (ti == jl_nothing)
            continue;
        if (ptrhash_get(&serialization_order, ti) == HT_NOTFOUND)
            jl_svecset(kept, i, jl_nothing);
        else
            sz += 1;
    }
    if (sz < HT_N_INLINE)
        sz = HT_N_INLINE;
    record_object_change((jl_value_t*)cache, (jl_value_t*)cache_rehash_set(kept, sz));
}

static void jl_prune_type_cache_linear(jl_svec_t *cache) JL_CANSAFEPOINT JL_GC_DISABLED
{
    size_t l = jl_svec_len(cache), ins = 0, i;
    if (l == 0)
        return;
    jl_svec_t *kept = jl_alloc_svec(l);
    for (i = 0; i < l; i++) {
        jl_value_t *ti = jl_svecref(cache, i);
        if (ti == jl_nothing)
            break;
        if (ptrhash_get(&serialization_order, ti) != HT_NOTFOUND)
            jl_svecset(kept, ins++, ti);
    }
    while (ins < l)
        jl_svecset(kept, ins++, jl_nothing);
    record_object_change((jl_value_t*)cache, (jl_value_t*)kept);
}

// Write the memory of an internal array, which nothing else references, only up to its
// length instead of with all its spare capacity.
static void trim_array_memory(jl_array_t *a) JL_GC_DISABLED
{
    if (a == NULL || jl_array_ndims(a) != 1)
        return;
    jl_genericmemory_t *mem = a->ref.mem;
    if (a->ref.ptr_or_offset == mem->ptr && jl_array_nrows(a) < mem->length)
        ptrhash_put(&memory_trims, mem, a);
}

static void jl_prune_mi_backedges(jl_array_t *backedges) JL_CANSAFEPOINT JL_GC_DISABLED
{
    if (backedges == NULL || jl_array_nrows(backedges) == 0)
        return;
    // not written, as for a MethodInstance that is only linked to
    if (ptrhash_get(&serialization_order, backedges) == HT_NOTFOUND)
        return;
    jl_array_t *kept = jl_array_copy(backedges);
    record_object_change((jl_value_t*)backedges, (jl_value_t*)kept);
    record_object_change((jl_value_t*)backedges->ref.mem, (jl_value_t*)kept->ref.mem);
    backedges = kept;
    size_t i = 0, ins = 0, n = jl_array_nrows(backedges);
    while (i < n) {
        jl_value_t *invokeTypes;
        jl_code_instance_t *caller;
        i = get_next_edge(backedges, i, &invokeTypes, &caller);
        if (ptrhash_get(&serialization_order, caller) != HT_NOTFOUND)
            ins = set_next_edge(backedges, ins, invokeTypes, caller);
    }
    jl_array_del_end(backedges, n - ins);
    trim_array_memory(backedges);
}

static void jl_prune_tn_backedges(jl_genericmemory_t *table) JL_CANSAFEPOINT JL_GC_DISABLED
{
    jl_genericmemory_t *kept_table = NULL; // a copy of `table`, once an entry is dropped
    _Atomic(jl_value_t*) *tab = (_Atomic(jl_value_t*)*)table->ptr;
    for (size_t i = 0, n = table->length; i < n; i += 2) {
        jl_array_t *callers = (jl_array_t*)jl_atomic_load_relaxed(&tab[i + 1]);
        if (callers == NULL || jl_array_nrows(callers) == 0)
            continue; // empty or deleted slot
        jl_array_t *kept = jl_array_copy(callers);
        record_object_change((jl_value_t*)callers, (jl_value_t*)kept);
        record_object_change((jl_value_t*)callers->ref.mem, (jl_value_t*)kept->ref.mem);
        callers = kept;
        size_t j, ins = 0, l = jl_array_nrows(callers);
        for (j = 0; j < l; j++) {
            jl_value_t *ci = jl_array_ptr_ref(callers, j);
            if (ptrhash_get(&serialization_order, ci) != HT_NOTFOUND)
                jl_array_ptr_set(callers, ins++, ci);
        }
        jl_array_del_end(callers, l - ins);
        trim_array_memory(callers);
        if (ins == 0) {
            // no caller is being serialized: drop the entry (cf. `jl_eqtable_pop`)
            if (kept_table == NULL)
                kept_table = jl_genericmemory_copy(table);
            _Atomic(jl_value_t*) *ktab = (_Atomic(jl_value_t*)*)kept_table->ptr;
            jl_gc_write_atomic(kept_table, ktab[i], jl_value_t, jl_nothing, relaxed); // clear the key
            jl_gc_write_atomic(kept_table, ktab[i + 1], jl_value_t, NULL, relaxed); // and the value
        }
    }
    if (kept_table != NULL)
        record_object_change((jl_value_t*)table, (jl_value_t*)kept_table);
}

static void jl_prune_mt_backedges(jl_genericmemory_t *allbackedges) JL_CANSAFEPOINT JL_GC_DISABLED
{
    for (size_t i = 0, n = allbackedges->length; i < n; i += 2) {
        jl_value_t *tn = jl_genericmemory_ptr_ref(allbackedges, i);
        jl_value_t *table = jl_genericmemory_ptr_ref(allbackedges, i + 1);
        if (tn && tn != jl_nothing && table && table != jl_nothing)
            jl_prune_tn_backedges((jl_genericmemory_t*)table);
    }
}

static void jl_prune_binding_backedges(jl_array_t *backedges) JL_CANSAFEPOINT JL_GC_DISABLED
{
    if (backedges == NULL || jl_array_nrows(backedges) == 0)
        return;
    // not written, as for a binding that is only linked to
    if (ptrhash_get(&serialization_order, backedges) == HT_NOTFOUND)
        return;
    jl_array_t *kept = jl_array_copy(backedges);
    record_object_change((jl_value_t*)backedges, (jl_value_t*)kept);
    record_object_change((jl_value_t*)backedges->ref.mem, (jl_value_t*)kept->ref.mem);
    backedges = kept;
    size_t i = 0, ins = 0, n = jl_array_nrows(backedges);
    for (i = 0; i < n; i++) {
        jl_value_t *b = jl_array_ptr_ref(backedges, i);
        if (ptrhash_get(&serialization_order, b) != HT_NOTFOUND) {
            jl_array_ptr_set(backedges, ins, b);
            ins++;
        }
    }
    jl_array_del_end(backedges, n - ins);
    trim_array_memory(backedges);
}

static void jl_prune_idset(_Atomic(jl_svec_t*) *pkeys, _Atomic(jl_genericmemory_t*) *pkeyset, uint_t (*key_hash)(size_t, jl_value_t*), jl_value_t *parent) JL_CANSAFEPOINT JL_GC_DISABLED
{
    jl_svec_t *keys = jl_atomic_load_relaxed(pkeys);
    size_t l = jl_svec_len(keys), i;
    if (l == 0)
        return;
    arraylist_t keys_list;
    arraylist_new(&keys_list, 0);
    for (i = 0; i < l; i++) {
        jl_value_t *k = jl_svecref(keys, i);
        if (k == jl_nothing)
            continue;
        if (ptrhash_get(&serialization_order, k) != HT_NOTFOUND)
            arraylist_push(&keys_list, k);
    }
    jl_genericmemory_t *keyset = jl_atomic_load_relaxed(pkeyset);
    _Atomic(jl_genericmemory_t*)keyset2;
    jl_atomic_store_relaxed(&keyset2, (jl_genericmemory_t*)jl_an_empty_memory_any);
    jl_svec_t *keys2 = jl_alloc_svec_uninit(keys_list.len);
    for (i = 0; i < keys_list.len; i++) {
        jl_binding_t *ref = (jl_binding_t*)keys_list.items[i];
        jl_svecset(keys2, i, ref);
        // a set without an index (none of its keys has a hash) stays without one
        if (keyset != (jl_genericmemory_t*)jl_an_empty_memory_any)
            jl_smallintset_insert(&keyset2, parent, key_hash, i, (jl_value_t*)keys2);
    }
    arraylist_free(&keys_list);
    // the pruned keys are not serialized
    record_object_change((jl_value_t*)keys, (jl_value_t*)keys2);
    if (keyset != (jl_genericmemory_t*)jl_an_empty_memory_any)
        record_object_change((jl_value_t*)keyset, (jl_value_t*)jl_atomic_load_relaxed(&keyset2));
}

static void jl_prune_method_specializations(jl_method_t *m) JL_CANSAFEPOINT JL_GC_DISABLED
{
    jl_value_t *specializations_ = jl_atomic_load_relaxed(&m->specializations);
    if (!jl_is_svec(specializations_)) {
        if (ptrhash_get(&serialization_order, specializations_) == HT_NOTFOUND)
            record_field_change((jl_value_t*)m, (jl_value_t **)&m->specializations, (jl_value_t*)jl_emptysvec);
        return;
    }
    jl_prune_idset((_Atomic(jl_svec_t*)*)&m->specializations, &m->speckeyset, speccache_hash, (jl_value_t*)m);
}

static void jl_prune_module_bindings(jl_module_t *m) JL_CANSAFEPOINT JL_GC_DISABLED
{
    jl_prune_idset(&m->bindings, &m->bindingkeyset, bindingkey_hash, (jl_value_t*)m);
}

static void strip_slotnames(jl_array_t *slotnames, int n)
{
    // replace slot names with `?`, except unused_sym since the compiler looks at it
    jl_sym_t *questionsym = jl_symbol("?");
    int i;
    for (i = 0; i < n; i++) {
        jl_value_t *s = jl_array_ptr_ref(slotnames, i);
        if (s != (jl_value_t*)jl_unused_sym)
            jl_array_ptr_set(slotnames, i, questionsym);
    }
}

static jl_value_t *strip_codeinfo_meta(jl_method_t *m, jl_value_t *ci_, jl_code_instance_t *codeinst) JL_CANSAFEPOINT
{
    jl_code_info_t *ci = NULL;
    JL_GC_PUSH1(&ci);
    int compressed = 0;
    if (!jl_is_code_info(ci_)) {
        compressed = 1;
        ci = jl_uncompress_ir(m, codeinst, (jl_value_t*)ci_);
    }
    else {
        // strip a copy
        ci = jl_copy_code_info((jl_code_info_t*)ci_);
        jl_gc_write(ci, ci->slotnames, jl_array_t, jl_array_copy(ci->slotnames));
    }
    strip_slotnames(ci->slotnames, jl_array_len(ci->slotnames));
    jl_gc_write(ci, ci->debuginfo, jl_debuginfo_t, jl_nulldebuginfo);
    jl_value_t *ret = (jl_value_t*)ci;
    if (compressed)
        ret = (jl_value_t*)jl_compress_ir(m, ci);
    JL_GC_POP();
    return ret;
}

static void strip_specializations_(jl_method_instance_t *mi) JL_CANSAFEPOINT
{
    assert(jl_is_method_instance(mi));
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        jl_value_t *inferred = jl_atomic_load_relaxed(&codeinst->inferred);
        if (inferred && inferred != jl_nothing && !jl_is_uint8(inferred)) {
            if (jl_options.strip_ir) {
                record_field_change((jl_value_t*)codeinst, (jl_value_t**)&codeinst->inferred, jl_nothing);
            }
            else if (jl_options.strip_metadata) {
                jl_value_t *stripped = strip_codeinfo_meta(mi->def.method, inferred, codeinst);
                record_field_change((jl_value_t*)codeinst, (jl_value_t**)&codeinst->inferred, stripped);
            }
        }
        // n.b. `edges` and `debuginfo` are stripped in jl_insert_into_serialization_queue,
        // which covers every serialized CodeInstance regardless of how it is reached
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    if (jl_options.trim || jl_options.strip_ir) {
        record_field_change((jl_value_t*)mi, (jl_value_t**)&mi->backedges, NULL);
    }
}

static int strip_all_codeinfos__(jl_typemap_entry_t *def, void *_env) JL_CANSAFEPOINT
{
    jl_method_t *m = def->func.method;
    if (m->source) {
        int stripped_ir = 0;
        if (jl_options.strip_ir) {
            int should_strip_ir = jl_options.trim;
            if (!should_strip_ir) {
                if (jl_atomic_load_relaxed(&m->unspecialized)) {
                    jl_code_instance_t *unspec = jl_atomic_load_relaxed(&jl_atomic_load_relaxed(&m->unspecialized)->cache);
                    if (unspec && jl_atomic_load_relaxed(&unspec->invoke)) {
                        // we have a generic compiled version, so can remove the IR
                        should_strip_ir = 1;
                    }
                }
            }
            if (!should_strip_ir) {
                int mod_setting = jl_get_module_compile(m->module);
                if (!(mod_setting == JL_OPTIONS_COMPILE_OFF || mod_setting == JL_OPTIONS_COMPILE_MIN)) {
                    // if the method is declared not to be compiled, keep IR for interpreter
                    should_strip_ir = 1;
                }
            }
            if (should_strip_ir) {
                record_field_change((jl_value_t*)m, &m->source, jl_nothing);
                record_field_change((jl_value_t*)m, (jl_value_t**)&m->roots, NULL);
                stripped_ir = 1;
            }
        }
        if (jl_options.strip_metadata) {
            if (!stripped_ir) {
                record_field_change((jl_value_t*)m, &m->source, strip_codeinfo_meta(m, m->source, NULL));
            }
            jl_array_t *slotnames = jl_uncompress_argnames(m->slot_syms);
            JL_GC_PUSH1(&slotnames);
            int tostrip = jl_array_len(slotnames);
            // for keyword methods, strip only nargs to keep the keyword names at the end for reflection
            if (jl_tparam0(jl_unwrap_unionall(m->sig)) == (jl_value_t*)jl_kwcall_type)
                tostrip = m->nargs;
            strip_slotnames(slotnames, tostrip);
            record_field_change((jl_value_t*)m, &m->slot_syms, jl_compress_argnames(slotnames));
            JL_GC_POP();
        }
    }
    if (jl_options.strip_metadata) {
        record_field_change((jl_value_t*)m, (jl_value_t**)&m->file, (jl_value_t*)jl_empty_sym);
        ptrhash_put(&bits_replace, &m->line, jl_box_int32(0));
        record_field_change((jl_value_t*)m, (jl_value_t**)&m->debuginfo, (jl_value_t*)jl_nulldebuginfo);
    }
    jl_value_t *specializations = jl_atomic_load_relaxed(&m->specializations);
    if (!jl_is_svec(specializations)) {
        strip_specializations_((jl_method_instance_t*)specializations);
    }
    else {
        size_t i, l = jl_svec_len(specializations);
        for (i = 0; i < l; i++) {
            jl_value_t *mi = jl_svecref(specializations, i);
            if (mi != jl_nothing)
                strip_specializations_((jl_method_instance_t*)mi);
        }
    }
    if (jl_atomic_load_relaxed(&m->unspecialized))
        strip_specializations_(jl_atomic_load_relaxed(&m->unspecialized));
    if (jl_options.strip_ir && m->root_blocks)
        record_field_change((jl_value_t*)m, (jl_value_t**)&m->root_blocks, NULL);
    return 1;
}

static int strip_all_codeinfos_mt(jl_methtable_t *mt, void *_env) JL_CANSAFEPOINT
{
    return jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), strip_all_codeinfos__, NULL);
}

static void jl_strip_all_codeinfos(jl_array_t *mod_array) JL_CANSAFEPOINT
{
    jl_foreach_reachable_mtable(strip_all_codeinfos_mt, mod_array, NULL);
}

static int strip_module(jl_module_t *m, jl_sym_t *docmeta_sym) JL_CANSAFEPOINT
{
    size_t world = jl_atomic_load_relaxed(&jl_world_counter);
    jl_svec_t *table = jl_atomic_load_relaxed(&m->bindings);
    for (size_t i = 0; i < jl_svec_len(table); i++) {
        jl_binding_t *b = (jl_binding_t*)jl_svecref(table, i);
        if ((void*)b == jl_nothing)
            break;
        jl_sym_t *name = b->globalref->name;
        jl_value_t *v = jl_get_binding_value_in_world(b, world);
        if (v) {
            if (jl_is_module(v)) {
                jl_module_t *child = (jl_module_t*)v;
                if (child != m && child->parent == m && child->name == name) {
                    // this is the original/primary binding for the submodule
                    if (!strip_module(child, docmeta_sym))
                        return 0;
                }
            }
        }
        if (name == docmeta_sym) {
            if (jl_atomic_load_relaxed(&b->value))
                record_field_change((jl_value_t*)b, (jl_value_t**)&b->value, jl_nothing);
            // TODO: this is a pretty stupidly unsound way to do this, but it is way to late here to do this correctly (by calling delete_binding and getting an updated world age then dropping all partitions from older worlds)
            jl_binding_partition_t *bp = jl_atomic_load_relaxed(&b->partitions);
            // The last partition's `next` holds a backreference to `b`, so stop
            // when we reach something that is no longer a partition.
            while (bp && jl_is_binding_partition((jl_value_t*)bp)) {
                if (jl_bkind_is_defined_constant(jl_binding_kind(bp))) {
                    // XXX: bp->kind = PARTITION_KIND_UNDEF_CONST;
                    record_field_change((jl_value_t*)bp, (jl_value_t**)&bp->restriction, NULL);
                }
                bp = jl_atomic_load_relaxed(&bp->next);
            }
        }
    }
    return 1;
}


static void jl_strip_all_docmeta(jl_array_t *mod_array) JL_CANSAFEPOINT
{
    jl_sym_t *docmeta_sym = NULL;
    if (jl_base_module) {
        jl_value_t *docs = jl_get_global(jl_base_module, jl_symbol("Docs"));
        if (docs && jl_is_module(docs)) {
            docmeta_sym = (jl_sym_t*)jl_get_global((jl_module_t*)docs, jl_symbol("META"));
        }
    }
    if (!docmeta_sym)
        return;
    for (size_t i = 0; i < jl_array_nrows(mod_array); i++) {
        jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(mod_array, i);
        assert(jl_is_module(m));
        if (m->parent == m) // some toplevel modules (really just Base) aren't actually
            strip_module(m, docmeta_sym);
    }
}

// --- entry points ---

jl_genericmemory_t *jl_global_roots_list;
jl_genericmemory_t *jl_global_roots_keyset;
static size_t jl_global_roots_max; // slot just past the last key of jl_global_roots_list
jl_mutex_t global_roots_lock;

jl_mutex_t precompile_field_replace_lock;
jl_svec_t *precompile_field_replace JL_GLOBALLY_ROOTED;

static inline jl_value_t *get_checked_fieldindex(const char *name, jl_datatype_t *st, jl_value_t *v, jl_value_t *arg, int mutabl) JL_CANSAFEPOINT
{
    if (mutabl) {
        if (st == jl_module_type)
            jl_error("cannot assign variables in other modules");
        if (!st->name->mutabl)
            jl_errorf("%s: immutable struct of type %s cannot be changed", name, jl_symbol_name(st->name->name));
    }
    size_t idx;
    if (jl_is_long(arg)) {
        idx = jl_unbox_long(arg) - 1;
        if (idx >= jl_datatype_nfields(st))
            jl_bounds_error(v, arg);
    }
    else if (jl_is_symbol(arg)) {
        idx = jl_field_index(st, (jl_sym_t*)arg, 1);
        arg = jl_box_long(idx);
    }
    else {
        jl_value_t *ts[2] = {(jl_value_t*)jl_long_type, (jl_value_t*)jl_symbol_type};
        jl_value_t *t = jl_type_union(ts, 2);
        jl_type_error(name, t, arg);
    }
    if (mutabl && jl_field_isconst(st, idx)) {
        jl_errorf("%s: const field .%s of type %s cannot be changed", name,
                jl_symbol_name((jl_sym_t*)jl_svecref(jl_field_names(st), idx)), jl_symbol_name(st->name->name));
    }
    return arg;
}

JL_DLLEXPORT void jl_set_precompile_field_replace(jl_value_t *val, jl_value_t *field, jl_value_t *newval)
{
    if (!jl_generating_output())
        return;
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(val);
    jl_value_t *idx = get_checked_fieldindex("setfield!", st, val, field, 1);
    JL_GC_PUSH1(&idx);
    size_t idxval = jl_unbox_long(idx);
    jl_value_t *ft = jl_field_type_concrete(st, idxval);
    if (!jl_isa(newval, ft))
        jl_type_error("setfield!", ft, newval);
    JL_LOCK(&precompile_field_replace_lock);
    if (precompile_field_replace == NULL) {
        precompile_field_replace = jl_alloc_svec(3);
        jl_svecset(precompile_field_replace, 0, jl_alloc_vec_any(0));
        jl_svecset(precompile_field_replace, 1, jl_alloc_vec_any(0));
        jl_svecset(precompile_field_replace, 2, jl_alloc_vec_any(0));
    }
    jl_array_ptr_1d_push((jl_array_t*)jl_svecref(precompile_field_replace, 0), val);
    jl_array_ptr_1d_push((jl_array_t*)jl_svecref(precompile_field_replace, 1), idx);
    jl_array_ptr_1d_push((jl_array_t*)jl_svecref(precompile_field_replace, 2), newval);
    JL_GC_POP();
    JL_UNLOCK(&precompile_field_replace_lock);
}


JL_DLLEXPORT int jl_is_globally_rooted(jl_value_t *val JL_MAYBE_UNROOTED) JL_NOTSAFEPOINT
{
    if (jl_is_datatype(val)) {
        jl_datatype_t *dt = (jl_datatype_t*)val;
        if (jl_unwrap_unionall(dt->name->wrapper) == val)
            return 1;
        return (jl_is_tuple_type(val) ? dt->isconcretetype : !dt->hasfreetypevars); // aka is_cacheable from jltypes.c
    }
    if (jl_is_bool(val) || jl_is_symbol(val) ||
            val == (jl_value_t*)jl_any_type || val == (jl_value_t*)jl_bottom_type || val == (jl_value_t*)jl_core_module)
        return 1;
    if (val == ((jl_datatype_t*)jl_typeof(val))->instance)
        return 1;
    return 0;
}

static jl_value_t *extract_wrapper(jl_value_t *t JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT JL_GLOBALLY_ROOTED
{
    t = jl_unwrap_unionall(t);
    if (jl_is_datatype(t))
        return ((jl_datatype_t*)t)->name->wrapper;
    return NULL;
}

JL_DLLEXPORT jl_value_t *jl_as_global_root(jl_value_t *val, int insert)
{
    if (jl_is_globally_rooted(val))
        return val;
    jl_value_t *tw = extract_wrapper(val);
    if (tw && (val == tw || jl_types_struct_equiv(val, tw)))
        return tw;
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
    // check table before acquiring lock to reduce writer contention
    jl_value_t *rval = jl_idset_get(jl_global_roots_list, jl_global_roots_keyset, val);
    if (rval)
        return rval;
    JL_LOCK(&global_roots_lock);
    rval = jl_idset_get(jl_global_roots_list, jl_global_roots_keyset, val);
    if (rval) {
        val = rval;
    }
    else if (insert) {
        ssize_t idx;
        jl_global_roots_list = jl_idset_put_key(jl_global_roots_list, val, &jl_global_roots_max, &idx);
        jl_global_roots_keyset = jl_idset_put_idx(jl_global_roots_list, jl_global_roots_keyset, idx);
    }
    else {
        val = NULL;
    }
    JL_UNLOCK(&global_roots_lock);
    return val;
}

// Companion to jl_collect_methtable_from_mod: method tables owned by the worklist
// are serialized without their contents, since all of their (currently valid)
// methods were collected as extext methods, to be re-added and re-activated on
// load by jl_add_methods / jl_activate_methods, which also restores their
// dispatch status.
static int jl_prune_internal_mtable(jl_methtable_t *mt, void *env)
{
    (void)env;
    if (jl_object_in_image((jl_value_t*)mt))
        return 1;
    record_field_change((jl_value_t*)mt, (jl_value_t**)&mt->defs, jl_nothing);
    jl_methcache_t *mc = mt->cache;
    record_field_change((jl_value_t*)mc, (jl_value_t**)&mc->cache, jl_nothing);
    record_field_change((jl_value_t*)mc, (jl_value_t**)&mc->leafcache, jl_an_empty_memory_any);
    return 1;
}

// Write the contents of `sec` to `f` at an 8-byte aligned offset, as a section of the payload
// starting at `payload`, store its offset and size in `out`, and close `sec`.  If the image stream
// cannot be grown to fit the section, fail rather than return with a truncated section.
static void write_section(ios_t *f, size_t payload, jl_image_section_t *out, ios_t *sec, const char *name) JL_CANSAFEPOINT
{
    write_padding(f, LLT_ALIGN(ios_pos(f), 8) - ios_pos(f));
    out->offset = ios_pos(f) - payload;
    out->size = sec->size;
    ios_seek(sec, 0);
    ios_copyall(f, sec);
    if ((size_t)ios_pos(f) != payload + out->offset + out->size) {
        jl_printf(
            JL_STDERR,
            "ERROR: failed to write system image: the %s section needs %" PRIu64 " bytes at offset %" PRIu64 " "
            "but the stream reached only %" PRIu64 " (out of memory)\n",
            name, (uint64_t)out->size, (uint64_t)(payload + out->offset), (uint64_t)ios_pos(f)
        );
        jl_exit(1);
    }
    ios_close(sec);
}

static void write_relocs_section(ios_t *f, size_t payload, jl_image_section_t *out, ios_t *bitmap, const char *name) JL_CANSAFEPOINT
{
    size_t nwords = bitmap->size / sizeof(uint64_t);
    ios_t gaps;
    ios_mem(&gaps, 0);
    size_t prev = 0;
    int fits = 1;
    for (size_t i = 0; i < nwords; i++) {
        for (uint64_t bits = jl_load_unaligned_i64(bitmap->buf + i * sizeof(uint64_t)); bits; bits &= bits - 1) {
            size_t word = 64 * i + __builtin_ctzll(bits), gap = word - prev;
            prev = word;
            fits &= gap <= UINT32_MAX;
            if (0 < gap && gap < 256) {
                write_uint8(&gaps, gap);
            }
            else {
                write_uint8(&gaps, 0);
                write_uint32(&gaps, gap);
            }
        }
    }
    int use_gaps = fits && gaps.size < bitmap->size;
    ios_close(use_gaps ? bitmap : &gaps);
    write_section(f, payload, out, use_gaps ? &gaps : bitmap, name);
}

// In addition to the system image (where `worklist = NULL`), this can also save incremental images with external linkage
static void jl_save_system_image_to_stream(ios_t *f, jl_array_t *mod_array,
                                           jl_array_t *module_init_order,
                                           jl_array_t *worklist, jl_array_t *extext_methods,
                                           jl_array_t *extext_records, jl_array_t *new_ext, jl_query_cache *query_cache,
                                           uint64_t preferred_base) JL_CANSAFEPOINT
{
    htable_new(&serialization_order, 25000);
    htable_new(&field_replace, 0);
    htable_new(&bits_replace, 0);
    htable_new(&object_replace, 0);
#ifndef NDEBUG
    htable_new(&queued_without_refs, 0);
#endif
    queueing_done = 0;
    htable_new(&memory_trims, 0);
    // the replacements recorded below are only reachable from these tables
    int en = jl_gc_enable(0);
    if (worklist)
        jl_foreach_reachable_mtable(jl_prune_internal_mtable, mod_array, NULL);
    jl_nulldebuginfo = (jl_debuginfo_t*)jl_get_global(jl_core_module, jl_symbol("NullDebugInfo"));
    // strip metadata and IR when requested
    if (jl_options.strip_metadata || jl_options.strip_ir) {
        if (jl_options.strip_metadata && jl_nulldebuginfo == NULL)
            jl_errorf("Core.NullDebugInfo required for --strip-metadata option");
        // FIXME: stripping should not rely on objects being installed in the
        // (JIT) cache to be reached for stripping / pruning in this step
        // the AOT-compiled CodeInstances etc. are (partially) provided up-front
        // to the serializer and may not be in the cache at all in this process
        jl_strip_all_codeinfos(mod_array);
        jl_strip_all_docmeta(mod_array);
    }
    // collect needed methods and prepare method table replacements
    htable_new(&new_methtables, 0);
    arraylist_t MIs;
    arraylist_new(&MIs, 0);
    arraylist_t gvars;
    arraylist_new(&gvars, 0);
    arraylist_t external_fns;
    arraylist_new(&external_fns, 0);
    // prepare hash table with any fields the user wanted us to rewrite during serialization
    if (precompile_field_replace) {
        jl_array_t *vals = (jl_array_t*)jl_svecref(precompile_field_replace, 0);
        jl_array_t *fields = (jl_array_t*)jl_svecref(precompile_field_replace, 1);
        jl_array_t *newvals = (jl_array_t*)jl_svecref(precompile_field_replace, 2);
        size_t i, l = jl_array_nrows(vals);
        assert(jl_array_nrows(fields) == l && jl_array_nrows(newvals) == l);
        for (i = 0; i < l; i++) {
            jl_value_t *val = jl_array_ptr_ref(vals, i);
            size_t field = jl_unbox_long(jl_array_ptr_ref(fields, i));
            jl_value_t *newval = jl_array_ptr_ref(newvals, i);
            jl_datatype_t *st = (jl_datatype_t*)jl_typeof(val);
            size_t offs = jl_field_offset(st, field);
            char *fldaddr = (char*)val + offs;
            if (jl_field_isptr(st, field)) {
                record_field_change(val, (jl_value_t**)fldaddr, newval);
            }
            else if (jl_field_size(st, field) > 0) {
                // replace the bits
                ptrhash_put(&bits_replace, (void*)fldaddr, newval);
                // and any pointers inside
                jl_datatype_t *rty = (jl_datatype_t*)jl_typeof(newval);
                const jl_datatype_layout_t *layout = rty->layout;
                size_t j, np = layout->npointers;
                for (j = 0; j < np; j++) {
                    uint32_t ptr = jl_ptr_offset(rty, j);
                    record_field_change(val, (jl_value_t**)fldaddr + ptr, *(((jl_value_t**)newval) + ptr));
                }
            }
        }
    }

    if (native_functions) {
        // Rewrite each MethodInstance's cache to the ordered list of
        // CodeInstances codegen emitted for this image (see jl_rewrite_mi_caches).
        size_t num_ci_order = 0;
        jl_get_llvm_mi_cache_order(native_functions, &num_ci_order, NULL);
        if (num_ci_order) {
            jl_code_instance_t **ci_order = (jl_code_instance_t**)malloc_s(num_ci_order * sizeof(jl_code_instance_t*));
            jl_get_llvm_mi_cache_order(native_functions, &num_ci_order, ci_order);
            jl_rewrite_mi_caches(ci_order, num_ci_order);
            free(ci_order);
        }
        size_t num_gvars, num_external_fns;
        jl_get_llvm_gv_inits(native_functions, &num_gvars, NULL);
        arraylist_grow(&gvars, num_gvars);
        jl_get_llvm_gv_inits(native_functions, &num_gvars, gvars.items);
        jl_get_llvm_external_fns(native_functions, &num_external_fns, NULL);
        arraylist_grow(&external_fns, num_external_fns);
        jl_get_llvm_external_fns(native_functions, &num_external_fns,
                                 (jl_code_instance_t *)external_fns.items);
        if (jl_options.trim) {
            size_t num_mis;
            jl_get_llvm_cis(native_functions, &num_mis, NULL);
            arraylist_grow(&MIs, num_mis);

            // Record MethodInstances for user-provided code (as reported by codegen)
            jl_get_llvm_cis(native_functions, &num_mis, (jl_code_instance_t**)MIs.items);
            for (size_t i = 0; i < num_mis; i++) {
                jl_code_instance_t *ci = (jl_code_instance_t*)MIs.items[i];
                MIs.items[i] = (void*)jl_get_ci_mi(ci);
            }

            // Record MethodInstances for built-ins (used when dynamically dispatching to a
            // built-in, e.g., in the Core._apply_iterate implementation)
            for (size_t i = 0; i < jl_n_builtins; i++) {
                jl_value_t *builtin = jl_builtin_instances[i];
                if (builtin == NULL)
                    continue;
                jl_method_instance_t *mi = jl_builtin_method_lookup(builtin);
                arraylist_push(&MIs, mi);
            }
        }
    }
    if (jl_options.trim) {
        jl_rebuild_methtables(&MIs, &new_methtables);
    }

    bits_start = 0;
    heap_base = preferred_base;
    htable_new(&fptr_to_id, jl_n_builtins);
    uintptr_t i;
    for (i = 0; i < jl_n_builtins; i++) {
        ptrhash_put(&fptr_to_id, (void*)(uintptr_t)jl_builtin_f_addrs[i], (void*)(i + BUILTIN_IDX_BIAS));
    }
    htable_new(&symtab_left, 0);
    htable_new(&symtab_right, 0);
    content_dedup_list = jl_alloc_memory_any(0);
    content_dedup_keyset = jl_alloc_memory_any(0);
    content_dedup_max = 0;
    htable_new(&nullptrs, 0);
    arraylist_new(&object_worklist, 0);
    arraylist_new(&serialization_queue, 0);
    ios_t sysimg, bits, symbols, imports, fptr_record, specials, gvar_refs, auxdata;
    ios_mem(&sysimg, 0);
    ios_mem(&bits, 0);
    ios_mem(&symbols, 0);
    ios_mem(&imports, 0);
    ios_mem(&specials, 0);
    ios_mem(&gvar_refs, 0);
    ios_mem(&auxdata, 0);
    ios_mem(&fptr_record, 0);
    jl_serializer_state s = {0};
    s.query_cache = query_cache;
    s.incremental = !(worklist == NULL);
    s.s = &sysimg;
    s.bits = &bits;
    s.symbols = &symbols;
    s.imports = &imports;
    s.fptr_record = &fptr_record;
    s.ptls = jl_current_task->ptls;
    arraylist_new(&s.memref_list, 0);
    arraylist_new(&s.relocs_list, 0);
    arraylist_new(&s.fixup_types, 0);
    arraylist_new(&s.fixup_objs, 0);
    s.buildid_depmods_idxs = image_to_depmodidx(mod_array);
    htable_new(&s.method_roots_index, 0);
    htable_new(&s.object_imports, 0);
    arraylist_new(&uniquing_records, 0);
    auxdata_size = 0;
    htable_new(&s.import_set, 0);
    if (worklist) {
        s.method_roots_list = jl_alloc_vec_any(0);
        s.worklist_key = jl_worklist_key(worklist);
    }

    jl_bigint_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("BigInt")) : NULL;
    if (jl_bigint_type) {
        gmp_limb_size = jl_unbox_long(jl_get_global((jl_module_t*)jl_get_global(jl_base_module, jl_symbol("GMP")),
                                                    jl_symbol("BITS_PER_LIMB"))) / 8;
    }
    jl_genericmemory_t *global_roots_list = NULL;
    jl_genericmemory_t *global_roots_keyset = NULL;

    { // step 1: record values (recursively) that need to go in the image
        size_t i;
        if (worklist == NULL) {
            // write `Core.ARGS` empty
            jl_array_t *args = jl_core_module ? (jl_array_t*)jl_get_global(jl_core_module, jl_symbol("ARGS")) : NULL;
            if (args != NULL && jl_array_len(args) != 0)
                record_object_change((jl_value_t*)args, (jl_value_t*)jl_alloc_array_1d(jl_typeof(args), 0));
            for (i = 0; i < sizeof(jl_image_specials) / sizeof(jl_image_specials[0]); i++)
                jl_queue_for_serialization(&s, image_special(jl_image_specials[i], module_init_order));
            jl_queue_for_serialization(&s, s.ptls->root_task->tls);
        }
        else {
            jl_queue_for_serialization(&s, worklist);
            jl_queue_for_serialization(&s, module_init_order);
        }
        // step 1.1: as needed, serialize the data needed for insertion into the running system
        if (extext_methods) {
            // Queue method extensions and their activation records
            jl_queue_for_serialization(&s, extext_methods);
            jl_queue_for_serialization(&s, extext_records);
            // Queue the new specializations
            jl_queue_for_serialization(&s, new_ext);
        }
        jl_serialize_reachable(&s);
        // step 1.2: ensure all gvars are part of the sysimage too
        record_gvars(&s, &gvars);
        if (jl_options.trim)
            record_gvars(&s, &MIs);
        jl_serialize_reachable(&s);
        // Beyond this point, all content should already have been visited, so now we can prune
        // the rest and add some internal root arrays.
        // step 1.3: include some other special roots
        if (s.incremental) {
            // Queue the new roots array
            jl_queue_for_serialization(&s, s.method_roots_list);
            jl_serialize_reachable(&s);
        }
        // step 1.4: prune (garbage collect) special weak references from the jl_global_roots_list
        if (worklist == NULL) {
            global_roots_list = jl_alloc_memory_any(0);
            global_roots_keyset = jl_alloc_memory_any(0);
            size_t global_roots_max = 0;
            for (size_t i = 0; i < jl_global_roots_list->length; i++) {
                jl_value_t *val = jl_genericmemory_ptr_ref(jl_global_roots_list, i);
                if (val && ptrhash_get(&serialization_order, val) != HT_NOTFOUND) {
                    ssize_t idx;
                    global_roots_list = jl_idset_put_key(global_roots_list, val, &global_roots_max, &idx);
                    global_roots_keyset = jl_idset_put_idx(global_roots_list, global_roots_keyset, idx);
                }
            }
            jl_queue_for_serialization(&s, global_roots_list);
            jl_queue_for_serialization(&s, global_roots_keyset);
            jl_serialize_reachable(&s);
        }
        queueing_done = 1;
        // step 1.5: prune (garbage collect) some special weak references known caches
        for (i = 0; i < serialization_queue.len; i++) {
            jl_value_t *v = (jl_value_t*)serialization_queue.items[i];
            if (jl_is_method(v)) {
                jl_method_t *m = (jl_method_t*)v;
                if (jl_options.trim)
                    jl_prune_method_specializations(m);
                trim_array_memory((jl_array_t*)get_replaceable_field((jl_value_t**)&m->roots, 1));
                trim_array_memory((jl_array_t*)get_replaceable_field((jl_value_t**)&m->root_blocks, 1));
            }
            else if (jl_is_module(v)) {
                if (jl_options.trim)
                    jl_prune_module_bindings((jl_module_t*)v);
            }
            else if (jl_is_typename(v)) {
                jl_typename_t *tn = (jl_typename_t*)v;
                jl_prune_type_cache_hash(jl_atomic_load_relaxed(&tn->cache));
                jl_prune_type_cache_linear(jl_atomic_load_relaxed(&tn->linearcache));
            }
            else if (jl_is_method_instance(v)) {
                jl_method_instance_t *mi = (jl_method_instance_t*)v;
                jl_value_t *backedges = get_replaceable_field((jl_value_t**)&mi->backedges, 1);
                jl_prune_mi_backedges((jl_array_t*)backedges);
            }
            else if (jl_is_binding(v)) {
                jl_binding_t *b = (jl_binding_t*)v;
                jl_value_t *backedges = get_replaceable_field((jl_value_t**)&b->backedges, 1);
                jl_prune_binding_backedges((jl_array_t*)backedges);
            }
            else if (jl_is_mtable(v)) {
                jl_methtable_t *mt = (jl_methtable_t*)v;
                jl_value_t *backedges = get_replaceable_field((jl_value_t**)&mt->backedges, 1);
                jl_prune_mt_backedges((jl_genericmemory_t*)backedges);
            }
        }
        // step 1.6: link the symbols of a system image into its symbol table
        if (worklist == NULL)
            jl_build_symtab();
    }

    jl_image_sections_t h = {0};
    { // step 2: build all the sysimg sections
        if (s.incremental) {
            // add the imports of the types that need uniquing first, in the order the loader
            // resolves them
            htable_t visited;
            htable_new(&visited, 0);
            for (size_t i = 0; i < serialization_queue.len; i++) {
                jl_value_t *v = (jl_value_t*)serialization_queue.items[i];
                if (jl_is_datatype(v) && needs_uniquing(v, s.query_cache))
                    add_uniquing_imports(&s, v, &visited);
            }
            htable_free(&visited);
        }
        jl_write_values(&s);
        // the pointer-free data goes at the end of the heap: place it before anything resolves
        // references to it, and append it once the records have written the layouts of types
        write_padding(&sysimg, LLT_ALIGN(sysimg.size, JL_CACHE_BYTE_ALIGNMENT) - sysimg.size);
        bits_start = sysimg.size;
        // the specials and the gvars are relocs, used only while loading
        if (worklist == NULL) {
            size_t i;
            for (i = 0; i < sizeof(jl_image_specials) / sizeof(jl_image_specials[0]); i++)
                write_reloc(&s, &specials, image_special(jl_image_specials[i], module_init_order));
            write_reloc(&s, &specials, (jl_value_t*)global_roots_list);
            write_reloc(&s, &specials, (jl_value_t*)global_roots_keyset);
            write_reloc(&s, &specials, (jl_value_t*)s.ptls->root_task->tls);
            write_reloc(&s, &specials, (jl_value_t*)symtab_root);
            h.world = jl_atomic_load_acquire(&jl_world_counter);
            h.typeinf_world = jl_typeinf_world;
            h.gensym_counter = jl_get_gs_ctr();
        }
        else {
            size_t i, l = jl_array_len(module_init_order);
            for (i = 0; i < l; i++) {
                // verify that all these modules were saved
                assert(ptrhash_get(&serialization_order, jl_array_ptr_ref(module_init_order, i)) != HT_NOTFOUND);
            }
            jl_pkgimage_specials_t pkg = {worklist, module_init_order, extext_methods, extext_records, s.method_roots_list};
            for (i = 0; i < sizeof(pkg) / sizeof(void*); i++)
                write_reloc(&s, &specials, ((jl_value_t**)&pkg)[i]);
        }
        h.gvar_objects = write_gvars(&s, &gvar_refs, &gvars, &external_fns);
        // write the records, at the offsets their imports were given
        for (size_t i = 0; i < uniquing_records.len; i++) {
            jl_value_t *v = (jl_value_t*)uniquing_records.items[i];
            JL_GC_PROMISE_ROOTED(v);
            if (jl_is_datatype(v)) {
                jl_datatype_t *dt = (jl_datatype_t*)v;
                uintptr_t layout = write_layout(&s, dt);
                jl_type_record_t rec = {0};
                rec.name = reloc_for(&s, get_replaceable_field((jl_value_t**)&dt->name, 1));
                rec.parameters = reloc_for(&s, get_replaceable_field((jl_value_t**)&dt->parameters, 1));
                rec.types = reloc_for(&s, get_replaceable_field((jl_value_t**)&dt->types, 1));
                rec.layout = layout ? get_reloc_for_item(layout, 0) : 0;
                rec.instance = reloc_for(&s, get_replaceable_field(&dt->instance, 1));
                rec.super = reloc_for(&s, get_replaceable_field((jl_value_t**)&dt->super, 1));
                rec.hash = dt->hash;
                memcpy(&rec.flags, TYPE_FLAGS(dt), sizeof(rec.flags));
                ios_write(&auxdata, (char*)&rec, sizeof(rec));
            }
            else if (jl_is_binding(v)) {
                jl_binding_t *b = (jl_binding_t*)v;
                jl_binding_record_t rec = {reloc_for(&s, (jl_value_t*)b->globalref->mod),
                                           reloc_for(&s, (jl_value_t*)b->globalref->name)};
                ios_write(&auxdata, (char*)&rec, sizeof(rec));
            }
            else {
                jl_method_instance_t *mi = (jl_method_instance_t*)v;
                jl_mi_record_t rec = {reloc_for(&s, mi->def.value),
                                      reloc_for(&s, mi->specTypes),
                                      reloc_for(&s, (jl_value_t*)mi->sparam_vals)};
                ios_write(&auxdata, (char*)&rec, sizeof(rec));
            }
        }
        assert(auxdata.size == auxdata_size);
        jl_image_section_t bits_section;
        write_section(&sysimg, 0, &bits_section, &bits, "pointer-free");
        assert(bits_section.offset == bits_start);
        s.bits = NULL;
    }

    // This ensures that we can use the low bit of addresses for
    // identifying end pointers in gc's eytzinger search.
    write_padding(&sysimg, LLT_ALIGN(sysimg.size, JL_CACHE_BYTE_ALIGNMENT) - sysimg.size);

    if (sysimg.size >= BITS_ITEM) {
        jl_printf(
            JL_STDERR,
            "ERROR: system image too large: sysimg.size is 0x%" PRIxPTR " but the limit is 0x%" PRIxPTR "\n",
            (uintptr_t)sysimg.size,
            BITS_ITEM
        );
        jl_exit(1);
    }

    {
        size_t payload_start = ios_pos(f);
        assert(payload_start % JL_CACHE_BYTE_ALIGNMENT == 0);
        jl_relocmaps_t maps;
        relocmaps_init(&maps);
        jl_finish_relocs(sysimg.buf, &s.relocs_list, &maps);
        jl_finish_memrefs(sysimg.buf, &s.memref_list, &maps);
        ios_t fixup_types, fixup_objs;
        ios_static_buffer(&fixup_types, (char*)s.fixup_types.items, s.fixup_types.len * sizeof(void*));
        ios_static_buffer(&fixup_objs, (char*)s.fixup_objs.items, s.fixup_objs.len * sizeof(void*));
        h.pointer_free_offset = bits_start;
        // the heap comes first, so that it can be mapped from the file, then the data used only
        // while loading
        jl_image_section_t heap;
        write_section(f, payload_start, &heap, &sysimg, "heap");
        assert(heap.offset == 0);
        h.heap_size = heap.size;
        write_section(f, payload_start, &h.imports, &imports, "imports");
        write_section(f, payload_start, &h.auxdata, &auxdata, "auxdata");
        write_section(f, payload_start, &h.strtab, &symbols, "strtab");
        write_relocs_section(f, payload_start, &h.binds, &maps.binds, "binds");
        write_relocs_section(f, payload_start, &h.slides, &maps.slides, "slides");
        write_section(f, payload_start, &h.specials, &specials, "specials");
        write_section(f, payload_start, &h.gvars, &gvar_refs, "gvars");
        write_section(f, payload_start, &h.fixup_types, &fixup_types, "fixup_types");
        write_section(f, payload_start, &h.fixup_objs, &fixup_objs, "fixup_objs");
        write_section(f, payload_start, &h.fptrs, &fptr_record, "fptrs");
        ((jl_ji_header_t*)f->buf)->sections = h;
    }

    assert(object_worklist.len == 0);
    arraylist_free(&object_worklist);
    arraylist_free(&serialization_queue);
    arraylist_free(&layout_table);
    arraylist_free(&uniquing_records);
    arraylist_free(&s.fixup_types);
    arraylist_free(&s.fixup_objs);
    arraylist_free(&s.memref_list);
    arraylist_free(&s.relocs_list);
    arraylist_free(&gvars);
    arraylist_free(&external_fns);
    htable_free(&s.object_imports);
    htable_free(&s.import_set);
    htable_free(&s.method_roots_index);
    htable_free(&field_replace);
    htable_free(&bits_replace);
    htable_free(&object_replace);
#ifndef NDEBUG
    htable_free(&queued_without_refs);
#endif
    htable_free(&memory_trims);
    htable_free(&serialization_order);
    htable_free(&symtab_left);
    htable_free(&symtab_right);
    content_dedup_list = NULL;
    content_dedup_keyset = NULL;
    htable_free(&nullptrs);
    htable_free(&fptr_to_id);
    htable_free(&new_methtables);
    bits_start = 0;
    heap_base = 0;

    jl_gc_enable(en);
}

static int ci_not_internal_cache(jl_code_instance_t *ci)
{
    jl_method_instance_t *mi = jl_get_ci_mi(ci);
    return !(jl_atomic_load_relaxed(&ci->flags) & JL_CI_FLAGS_NATIVE_CACHE_VALID) || jl_object_in_image(mi->def.value);
}

static uint8_t jl_get_toplevel_syntax_version(void) JL_CANSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    jl_module_t *toplevel = (jl_module_t*)jl_get_global_value(jl_base_module, jl_symbol("__toplevel__"), ct->world_age);
    JL_GC_PROMISE_ROOTED(toplevel);
    jl_value_t *syntax_version = jl_get_global_value(toplevel, jl_symbol("_internal_syntax_version"), ct->world_age);
    return jl_unbox_uint8(syntax_version);
}

static void jl_write_header_for_incremental(ios_t *f, jl_array_t *worklist, jl_array_t *mod_array, jl_array_t **udeps, bool_t emit_split) JL_CANSAFEPOINT
{
    write_uint8(f, (jl_cache_flags() & ~1) | (emit_split ? 1 : 0));
    // coverage instrumentation of the image, part of the cache identity
    write_uint8(f, jl_image_coverage_config());
    // write the syntax version marker. Note that unlike a VersionNumber, this is
    // private to the serialization format and only needs to be reloaded by the
    // same version of Julia that wrote it. As a result, we don't store the full
    // VersionNumber, only an index of which of the supported syntax versions to
    // select.
    write_uint8(f, jl_get_toplevel_syntax_version());
    // write description of contents (name, uuid, buildid)
    write_worklist_for_header(f, worklist);
    // Determine unique (module, abspath, fsize, hash, mtime) dependencies for the files defining modules in the worklist
    // (see Base._require_dependencies). These get stored in `udeps` and written to the ji-file header
    // (abspath will be converted to a relocatable @depot path before writing, cf. Base.replace_depot_path).
    // Also write Preferences.
    write_dependency_list(f, worklist, udeps);
    // write description of requirements for loading (modules that must be pre-loaded if initialization is to succeed)
    // this can return errors during deserialize,
    // best to keep it early (before any actual initialization)
    write_mod_list(f, mod_array);
}

JL_DLLEXPORT uint32_t jl_create_system_image(void **_native_data, jl_array_t *worklist,
                                             bool_t emit_split, bool_t compress, ios_t **s,
                                             jl_array_t **udeps JL_REQUIRE_ROOTED_SLOT,
                                             jl_array_t *module_init_order)
{
    JL_TIMING(SYSIMG_DUMP, SYSIMG_DUMP);

    jl_task_t *ct = jl_current_task;
    ios_t *f = (ios_t*)malloc_s(sizeof(ios_t));
    ios_mem(f, 0);

    jl_array_t *mod_array = NULL, *extext_methods = NULL, *extext_records = NULL, *new_ext = NULL, *ext_foreign_cis = NULL;
    JL_GC_PUSH5(&mod_array, &extext_methods, &extext_records, &new_ext, &ext_foreign_cis);

    ext_foreign_cis = jl_alloc_vec_any(0);

    mod_array = jl_get_loaded_modules();  // __toplevel__ modules loaded in this session (from Base.loaded_modules_array)
    write_header(f, (worklist ? JI_FLAG_PKGIMAGE : 0) | (emit_split ? JI_FLAG_SPLIT : 0));
    if (worklist) {
        if (_native_data != NULL) {
            if (suppress_precompile)
                newly_inferred = NULL;
            *_native_data = jl_create_native(NULL, 0, 1, jl_atomic_load_acquire(&jl_world_counter), NULL, suppress_precompile ? (jl_array_t*)jl_an_empty_vec_any : worklist, 0, module_init_order, ext_foreign_cis);
        }
        jl_write_header_for_incremental(f, worklist, mod_array, udeps, emit_split);
    }
    else if (_native_data != NULL) {
        *_native_data = jl_create_native(NULL, jl_options.trim, 0, jl_atomic_load_acquire(&jl_world_counter), mod_array, NULL, jl_options.compile_enabled == JL_OPTIONS_COMPILE_ALL, module_init_order, ext_foreign_cis);
    }
    if (_native_data != NULL)
        native_functions = *_native_data;

    jl_gc_collect(JL_GC_FULL);
    jl_gc_collect(JL_GC_INCREMENTAL);   // sweep finalizers
    // Make sure we don't run any Julia code concurrently after this point
    // since it will invalidate our serialization preparations
    jl_gc_enable_finalizers(ct, 0);
    assert((ct->reentrant_timing & 0b1110) == 0);
    ct->reentrant_timing |= 0b1000;
    if (worklist) {
        // new_exts: [code_instances, ...], anything to add to the image just for side-effects
        // Save the inferred code from newly inferred, external methods
        arraylist_t CIs;
        arraylist_new(&CIs, 0);
        if (native_functions) {
            size_t num_cis = 0;
            jl_get_llvm_cis(native_functions, &num_cis, NULL);
            arraylist_grow(&CIs, num_cis);
            jl_get_llvm_cis(native_functions, &num_cis, (jl_code_instance_t**)CIs.items);
        }
        // Create a filtered list of the compiled code instances that are
        // possibly not referenced via any other way but valid for the
        // Method cache field of an external method
        new_ext = jl_alloc_vec_any(0);
        for (size_t i = 0; i < CIs.len; i++) {
            jl_code_instance_t *ci = (jl_code_instance_t*)CIs.items[i];
            if (ci_not_internal_cache(ci))
                jl_array_ptr_1d_push(new_ext, (jl_value_t*)ci);
        }
        arraylist_free(&CIs);
        // Merge foreign & external CIs
        size_t n_ext = jl_array_nrows(ext_foreign_cis);
        for (size_t i = 0; i < n_ext; i++)
            jl_array_ptr_1d_push(new_ext, jl_array_ptr_ref(ext_foreign_cis, i));
        ext_foreign_cis = NULL; // not needed anymore, free it

        // Collect method extensions
        // extext_methods: worklist-owned "extending external" methods added to functions owned by modules outside the worklist
        // extext_records: in parallel, the ext_backinterferences of each (or nothing)
        extext_methods = jl_alloc_vec_any(0);
        extext_records = jl_alloc_vec_any(0);
        jl_collect_extext_methods(extext_methods, extext_records, mod_array);

        if (emit_split) {
            jl_clone_targets_t targets = jl_get_llvm_clone_targets(jl_options.cpu_target);
            write_uint32(f, targets.data_size);
            ios_write(f, (const char *)targets.data, targets.data_size);
            jl_free_clone_targets(&targets);
        }
        else {
            write_uint32(f, 0);
        }
    }

    write_padding(f, LLT_ALIGN(ios_pos(f), JL_CACHE_BYTE_ALIGNMENT) - ios_pos(f));
    int64_t payload_start = ios_pos(f);
    // Only a system image has a preferred address.  Its file is placed at 0x4000_0000_0000, which
    // is user address space on every 64-bit platform we support with 47 or more bits of it, and
    // above MMTk's heap range, which must not contain the image.
#ifdef _P64
    uint64_t preferred_base = !worklist ? 0x400000000000 + payload_start : 0;
#else
    uint64_t preferred_base = 0;
#endif

    jl_query_cache query_cache;
    init_query_cache(&query_cache);
    // IR retained only to generate native code is not serialized (see jl_queue_for_serialization)
    jl_set_precompile_keep_ir(0);
    jl_save_system_image_to_stream(f, mod_array, module_init_order, worklist,
                                   extext_methods, extext_records, new_ext, &query_cache, preferred_base);
    if (_native_data != NULL)
        native_functions = NULL;
    // make sure we don't run any Julia code concurrently before this point
    // Re-enable running julia code for postoutput hooks, atexit, etc.
    jl_gc_enable_finalizers(ct, 1);
    ct->reentrant_timing &= ~0b1000u;

    int64_t payload_end = ios_pos(f);
    uint32_t checksum = jl_crc32c(0, &f->buf[payload_start], payload_end - payload_start);

    if (compress) {
        size_t size = payload_end - payload_start;
        size_t bound = ZSTD_compressBound(size);
        char *buf = (char *)malloc(bound);
        size_t comp_size = ZSTD_compress(buf, bound, f->buf + payload_start, size, 15);
        if (ZSTD_isError(comp_size))
            jl_errorf("compression of system image failed: %s", ZSTD_getErrorName(comp_size));
        ios_trunc(f, payload_start);
        ios_seek(f, payload_start);
        ios_write(f, buf, comp_size);
        free(buf);
        payload_end = ios_pos(f);
    }

    jl_ji_header_t *header = (jl_ji_header_t*)f->buf;
    header->checksum = checksum;
    header->preferred_base = preferred_base;
    header->payload_start = payload_start;
    header->payload_size = payload_end - payload_start;

    destroy_query_cache(&query_cache);

    JL_GC_POP();
    *s = f;
    return checksum;
}

#ifdef JL_LIBRARY_STATIC
// The sysimage is linked into the same binary as libjulia-internal, so the
// static linker resolves its symbols and `handle` is unused.
extern const char jl_system_image_data[];
extern const size_t jl_system_image_size;
extern const uint32_t jl_system_image_checksum;
extern const jl_image_pointers_t jl_image_pointers;

#define JL_IMAGE_SYM(handle, name, out) (*(void **)(out) = (void *)&name)
#else
#define JL_IMAGE_SYM(handle, name, out) jl_dlsym(handle, #name, (void **)(out), 1, 0)
#endif

// Read and verify the .ji header in `s` into `image`.  Returns the end of the payload, or 0 if the
// header failed verification.
static int64_t jl_image_read_header(ios_t *s, jl_image_buf_t *image) JL_NOTSAFEPOINT
{
    int64_t size, heap_size;
    if (jl_read_verify_header(s, &image->flags, &image->checksum, &image->preferred_base,
                              &image->payload_start, &size, &heap_size) != 0)
        return 0;
    image->heap_size = heap_size;
    return image->payload_start + size;
}

static int jl_image_read_ji(ios_t *s, jl_image_buf_t *image, int map) JL_NOTSAFEPOINT;

// Takes in a path of the form "usr/lib/julia/sys.so"
JL_DLLEXPORT jl_image_buf_t jl_preload_sysimg(const char *fname)
{
    if (jl_sysimage_buf.kind != JL_IMAGE_KIND_NONE)
        return jl_sysimage_buf;

#ifdef JL_LIBRARY_STATIC
    // the sysimage is linked into this binary; nothing is loaded from `fname`
    return jl_set_sysimg_so(NULL);
#endif

    char *dot = (char*) strrchr(fname, '.');
    int is_ji = (dot && !strcmp(dot, ".ji"));

    if (is_ji) {
        // .ji extension => load .ji file only
        ios_t f;
        jl_image_buf_t buf = { JL_IMAGE_KIND_JI };

        if (ios_file(&f, fname, 1, 0, 0, 0) == NULL)
            jl_errorf("System image file \"%s\" not found.", fname);

        if (jl_image_read_ji(&f, &buf, 1) <= 0)
            jl_errorf("Error reading system image file.");

        ios_close(&f);

        jl_sysimage_buf = buf;
        return jl_sysimage_buf;
    }
    else {
        // Get handle to sys.so
        return jl_set_sysimg_so(jl_dlopen_e(fname, JL_RTLD_LOCAL | JL_RTLD_NOW));
    }
}

static void jl_image_load_metadata(void *handle, jl_image_buf_t *image)
{
    JL_IMAGE_SYM(handle, jl_image_pointers, &image->pointers);
    uint32_t *pchecksum;
    JL_IMAGE_SYM(handle, jl_system_image_checksum, &pchecksum);
    image->heap_checksum = *pchecksum;
    // only present if the image was built with coverage counters
    jl_dlsym(handle, "jl_image_coverage", (void **)&image->coverage, 0, 0);
}

JL_DLLEXPORT void jl_image_unpack_uncomp(void *handle, jl_image_buf_t *image)
{
    size_t *plen;
    ios_t f;
    JL_IMAGE_SYM(handle, jl_system_image_size, &plen);
    JL_IMAGE_SYM(handle, jl_system_image_data, &image->data);
    image->size = *plen;
    ios_static_buffer(&f, (char*)image->data, image->size);
    int64_t end = jl_image_read_header(&f, image);
    ios_close(&f);
    if (end) {
        image->heap = image->data + image->payload_start;
        image->loader = image->heap + image->heap_size;
    }
    jl_image_load_metadata(handle, image);
}

// Setting JULIA_IMAGE_FORCE_SLIDE to a value other than 0 places images elsewhere, which exercises
// the relocation path and keeps the image's address randomized.
static int jl_image_force_slide(void) JL_NOTSAFEPOINT
{
    const char *env = getenv("JULIA_IMAGE_FORCE_SLIDE");
    return env != NULL && env[0] != '\0' && strcmp(env, "0") != 0;
}

// A heap is placed `*lead` bytes past an allocation-granularity boundary, as it is in its file, so
// that the file can be mapped.  Return the address of the boundary that puts the heap of `image`
// at its preferred address, or 0 to place it anywhere.
static uintptr_t jl_image_preferred_start(const jl_image_buf_t *image, size_t *lead) JL_NOTSAFEPOINT
{
    *lead = image->payload_start & (jl_getallocationgranularity() - 1);
    uint64_t heap = image->preferred_base;
    if (heap == 0 || (uint64_t)(uintptr_t)heap != heap || jl_image_force_slide())
        return 0;
    return (uintptr_t)heap - *lead;
}

#ifndef _OS_WINDOWS_
// mmap exactly at `addr`, without replacing an existing mapping; returns NULL on failure
static char *jl_image_mmap_at(uintptr_t addr, size_t size, int flags, int fd, size_t offset) JL_NOTSAFEPOINT
{
#if defined(MAP_FIXED_NOREPLACE)
    flags |= MAP_FIXED_NOREPLACE;
#elif defined(MAP_EXCL)
    flags |= MAP_FIXED | MAP_EXCL; // FreeBSD
#endif
    char *data = (char *)mmap((void *)addr, size, PROT_READ | PROT_WRITE, flags, fd, offset);
    if (data == MAP_FAILED)
        return NULL;
    if ((uintptr_t)data != addr) {
        // treated as a hint, which the kernel did not honor
        munmap(data, size);
        return NULL;
    }
    return data;
}
#endif

// Read and verify the header of the .ji open in `s` into `image`, and read the file into
// `image->data`.  If `map`, map the heap copy-on-write instead, at its preferred address if it is
// available, and read only the header and the loader data, which are then temporary.
// Returns 1 on success, 0 on failure, and -1 if the .ji of a split image was not written with its
// native image.
static int jl_image_read_ji(ios_t *s, jl_image_buf_t *image, int map) JL_NOTSAFEPOINT
{
    int64_t end = jl_image_read_header(s, image);
    if (!end)
        return 0;
    if (image->is_split && image->checksum != image->heap_checksum)
        return -1;
    int64_t heap_off = image->payload_start, loader_off = heap_off + image->heap_size;
    if (map) {
        // a mapping must start on an allocation-granularity boundary, so map from the one before
        // the heap
        size_t lead;
        uintptr_t addr = jl_image_preferred_start(image, &lead);
        int64_t offset = heap_off - lead;
        size_t size = loader_off - offset;
        char *p = NULL;
#ifdef _OS_WINDOWS_
        HANDLE fmap = CreateFileMapping((HANDLE)_get_osfhandle(s->fd), NULL, PAGE_WRITECOPY, 0, 0, NULL);
        if (fmap) {
            if (addr)
                p = (char *)MapViewOfFileEx(fmap, FILE_MAP_COPY, (DWORD)(offset >> 32), (DWORD)offset, size, (void *)addr);
            if (!p)
                p = (char *)MapViewOfFile(fmap, FILE_MAP_COPY, (DWORD)(offset >> 32), (DWORD)offset, size);
            CloseHandle(fmap);
        }
#else
        if (addr)
            p = jl_image_mmap_at(addr, size, MAP_PRIVATE, s->fd, offset);
        if (!p) {
            p = (char *)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, s->fd, offset);
            if (p == MAP_FAILED)
                p = NULL;
        }
#endif
        if (!p)
            return 0;
        image->heap = p + lead;
        image->data_owned = 1;
    }
    // read the file, leaving out the heap if it is mapped
    size_t size = end - (map ? image->heap_size : 0);
    char *buf = (char *)malloc(size);
    ios_seek(s, 0);
    ios_bufmode(s, bm_none);
    int ok = ios_readall(s, buf, heap_off) == (size_t)heap_off;
    ios_seek(s, map ? loader_off : heap_off);
    ok = ok && ios_readall(s, buf + heap_off, size - heap_off) == size - heap_off;
    if (!ok) {
        free(buf);
        return 0;
    }
    image->data = buf;
    image->size = size;
    if (map)
        image->loader = buf + heap_off;
    return 1;
}

// Allocate a page-aligned buffer of at least `size` bytes (large pages preferred on Windows), at
// the address `preferred` if it is nonzero and available.
static char *jl_image_alloc_pages(size_t size, uintptr_t preferred)
{
    size_t page_size = jl_getpagesize(); /* jl_page_size is not set yet when loading sysimg */
    size_t aligned_size = LLT_ALIGN(size, page_size);
    char *data = NULL;
    int fail = 0;
#if defined(_OS_WINDOWS_)
    size_t large_page_size = GetLargePageMinimum();
    for (int anywhere = !preferred; anywhere < 2 && !data; anywhere++) {
        void *addr = anywhere ? NULL : (void *)preferred;
        if (large_page_size > 0 && size > 4 * large_page_size) {
            size_t large_aligned_size = LLT_ALIGN(size, large_page_size);
            data = (char *)VirtualAlloc(addr, large_aligned_size,
                                        MEM_COMMIT | MEM_RESERVE | MEM_LARGE_PAGES,
                                        PAGE_READWRITE);
        }
        if (!data) {
            /* Try small pages if large pages failed. */
            data = (char *)VirtualAlloc(addr, aligned_size, MEM_COMMIT | MEM_RESERVE,
                                        PAGE_READWRITE);
        }
    }
    fail = !data;
#else
    if (preferred)
        data = jl_image_mmap_at(preferred, aligned_size, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (!data) {
        data = (char *)mmap(NULL, aligned_size, PROT_READ | PROT_WRITE,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        fail = data == (void *)-1;
    }
#endif
    if (fail) {
        const char *err;
#if defined(_OS_WINDOWS_)
        char err_buf[256];
        win32_formatmessage(GetLastError(), err_buf, sizeof(err_buf));
        err = err_buf;
#else
        err = strerror(errno);
#endif
        jl_safe_printf("ERROR: failed to allocate memory for system image: %s\n", err);
        abort();
    }
    return data;
}

static void jl_image_decompress_into(ZSTD_DCtx *dctx, ZSTD_inBuffer *in, char *dst, size_t size) JL_CANSAFEPOINT
{
    ZSTD_outBuffer out = { dst, size, 0 };
    while (out.pos < size) {
        size_t res = ZSTD_decompressStream(dctx, &out, in);
        if (ZSTD_isError(res))
            jl_errorf("Failed to decompress heap image: %s", ZSTD_getErrorName(res));
        if (res == 0 && out.pos < size)
            jl_error("Decompressed heap image has unexpected size.");
    }
}

// Decompress the payload of the compressed .ji in `data`, whose header is in `image` and whose
// payload ends at `end`: the heap into new pages, at the preferred address if it is available,
// and the header and the loader data into a temporary buffer.
static void jl_image_decompress(jl_image_buf_t *image, const char *data, int64_t end) JL_CANSAFEPOINT
{
    if (!end)
        jl_error("Precompile file header verification checks failed.");
    const char *comp_data = data + image->payload_start;
    size_t comp_len = end - image->payload_start;
    unsigned long long payload_size = ZSTD_getFrameContentSize(comp_data, comp_len);
    if (payload_size == ZSTD_CONTENTSIZE_UNKNOWN || payload_size == ZSTD_CONTENTSIZE_ERROR)
        jl_error("Compressed heap image is corrupt.");
    // as when mapping, start at the allocation-granularity boundary before the heap
    size_t lead;
    uintptr_t start = jl_image_preferred_start(image, &lead);
    char *pages = jl_image_alloc_pages(lead + image->heap_size, start);
    image->size = image->payload_start + (payload_size - image->heap_size);
    char *buf = (char *)malloc(image->size);
    memcpy(buf, data, image->payload_start);
    ZSTD_DCtx *dctx = ZSTD_createDCtx();
    ZSTD_inBuffer in = { comp_data, comp_len, 0 };
    jl_image_decompress_into(dctx, &in, pages + lead, image->heap_size);
    jl_image_decompress_into(dctx, &in, buf + image->payload_start, payload_size - image->heap_size);
    ZSTD_freeDCtx(dctx);
    image->data = buf;
    image->heap = pages + lead;
    image->loader = buf + image->payload_start;
    image->data_owned = 1;
}

JL_DLLEXPORT void jl_image_unpack_zstd(void *handle, jl_image_buf_t *image) JL_CANSAFEPOINT
{
    size_t *plen;
    char *data;
    ios_t f;
    JL_IMAGE_SYM(handle, jl_system_image_size, &plen);
    JL_IMAGE_SYM(handle, jl_system_image_data, &data);
    jl_image_load_metadata(handle, image);
    ios_static_buffer(&f, data, *plen);
    int64_t end = jl_image_read_header(&f, image);
    ios_close(&f);
    jl_image_decompress(image, data, end);

    size_t page_size = jl_getpagesize();
    size_t len = (*plen) & ~(page_size - 1);
#ifdef _OS_WINDOWS_
    if (len)
        VirtualFree((void *)data, len, MEM_RELEASE);
#else
    munmap((void *)data, len);
#endif
}

// Read the .ji of the split image `handle` into `image` (see jl_image_read_ji), leaving
// `image->data` NULL if it is the .ji of a package image not written with this .so.
static void jl_image_get_split_ji(void *handle, jl_image_buf_t *image, int map) JL_CANSAFEPOINT
{
    const char *lib_path = jl_pathname_for_handle(handle);
    if (!lib_path)
        jl_error("unable to find path to native image\n");

    // Replace the file extension with ".ji". The "extension" starts at the
    // first '.' after the last path separator, so e.g. "/foo/bar/baz.so.1.2.3"
    // becomes "/foo/bar/baz.ji".
    char ji_path[JL_PATH_MAX];
    const char *basename = lib_path;
    for (const char *p = lib_path; *p; p++) {
#ifdef _OS_WINDOWS_
        if (*p == '/' || *p == '\\')
#else
        if (*p == '/')
#endif
            basename = p + 1;
    }
    const char *dot_pos = strchr(basename, '.');
    int ji_path_len = dot_pos ? dot_pos - lib_path : strlen(lib_path);
    if (ji_path_len > JL_PATH_MAX - 4)
        abort();
    memcpy(ji_path, lib_path, ji_path_len);
    snprintf(ji_path + ji_path_len, sizeof ji_path - ji_path_len, ".ji");
#ifdef _OS_WINDOWS_
    free((char *)lib_path);
#endif

    ios_t s;
    int res = 0;
    if (ios_file(&s, ji_path, 1, 0, 0, 0)) {
        res = jl_image_read_ji(&s, image, map);
        ios_close(&s);
    }
    if (res < 0) {
        if (!(image->flags & JI_FLAG_PKGIMAGE))
            jl_error("Image checksum mismatch: the heap image (.ji) was not "
                     "compiled for use with this native image.");
        return;
    }
    if (res > 0)
        return;
    jl_printf(JL_STDERR, "unable to load .ji associated with native image: %s\n", ji_path);
    abort();
}

JL_DLLEXPORT void jl_image_unpack_split(void *handle, jl_image_buf_t *image) JL_CANSAFEPOINT
{
    image->is_split = 1;
    jl_image_load_metadata(handle, image);
    jl_image_get_split_ji(handle, image, 1);
}

JL_DLLEXPORT void jl_image_unpack_split_zstd(void *handle, jl_image_buf_t *image) JL_CANSAFEPOINT
{
    image->is_split = 1;
    jl_image_load_metadata(handle, image);
    jl_image_get_split_ji(handle, image, 0);
    char *comp_data = (char*)image->data;
    if (comp_data == NULL) // checksum mismatch
        return;
    jl_image_decompress(image, comp_data, image->size);
    free(comp_data);
}

// Compute the load address of the image containing `addr`
static intptr_t jl_image_base(const void *addr) JL_NOTSAFEPOINT
{
#ifdef _OS_WINDOWS_
    // an HMODULE is the load address of the module
    return (intptr_t)jl_find_dynamic_library_by_addr((void*)addr, /* throw_err */ 0, /* close */ 0);
#else
    Dl_info dlinfo;
    if (dladdr((void*)addr, &dlinfo) != 0)
        return (intptr_t)dlinfo.dli_fbase;
    return 0;
#endif
}

// From a shared library handle, verify consistency and return a jl_image_buf_t
static jl_image_buf_t get_image_buf(void *handle, int is_pkgimage) JL_NOTSAFEPOINT
{
    jl_image_buf_t image = {
        .kind = JL_IMAGE_KIND_SO,
        .pointers = NULL,
        .data = NULL,
        .size = 0,
        .base = 0,
        .is_split = 0,
    };

#ifdef JL_LIBRARY_STATIC
    // `handle` is ignored: the sysimage is linked in (see JL_IMAGE_SYM) and
    // pkgimages cannot be loaded into a static libjulia-internal
    assert(!is_pkgimage);
    (*jl_image_unpack)(NULL, &image);
#else
    // verify that the linker resolved the symbols in this image against ourselves (libjulia-internal)
    typedef void** (JL_NOTSAFEPOINT *jl_RTLD_DEFAULT_handle_func_t)(void);
    jl_RTLD_DEFAULT_handle_func_t get_jl_RTLD_DEFAULT_handle_addr = NULL;
    if (handle != jl_RTLD_DEFAULT_handle) {
        int symbol_found = jl_dlsym(handle, "get_jl_RTLD_DEFAULT_handle_addr", (void **)&get_jl_RTLD_DEFAULT_handle_addr, 0, 0);
        if (!symbol_found || (void*)&jl_RTLD_DEFAULT_handle != (get_jl_RTLD_DEFAULT_handle_addr()))
            jl_error("Image file failed consistency check: maybe opened the wrong version?");
    }

    // verification passed, lookup the buffer pointers
    jl_image_unpack_func_t *unpack;
    jl_dlsym(handle, "jl_image_unpack", (void **)&unpack, 1, 0);
    (*unpack)(handle, &image);
#endif
    image.base = jl_image_base(image.pointers);

    return image;
}

// Allow passing in a module handle directly, rather than a path
JL_DLLEXPORT jl_image_buf_t jl_set_sysimg_so(void *handle)
{
    if (jl_sysimage_buf.kind != JL_IMAGE_KIND_NONE)
        return jl_sysimage_buf;

#ifdef JL_LIBRARY_STATIC
    // the linked-in sysimage is used regardless of `handle` (see get_image_buf);
    // record the binary containing it as the image file
    const char *path = jl_pathname_for_symbol((void*)&jl_image_pointers);
    if (path != NULL && path[0] == '\0') // ELF reports the main executable as ""
        path = jl_options.julia_bin;
    if (path != NULL)
        jl_options.image_file = path;
#endif
    jl_sysimage_buf = get_image_buf(handle, /* is_pkgimage */ 0);
    return jl_sysimage_buf;
}

// When an image is loaded with ignore_native, all subsequent image loads must ignore
// native code in the cache-file since we can't gurantuee that there are no call edges
// into the native code of the image. See https://github.com/JuliaLang/julia/pull/52123#issuecomment-1959965395.
int IMAGE_NATIVE_CODE_TAINTED = 0;

// TODO: This should possibly be in Julia
static int jl_validate_binding_partition(jl_binding_t *b, jl_binding_partition_t *bpart, size_t mod_idx, int unchanged_implicit, int no_replacement) JL_CANSAFEPOINT
{
    if (jl_atomic_load_relaxed(&bpart->max_world) != ~(size_t)0)
        return 1;
    size_t raw_kind = bpart->kind;
    enum jl_partition_kind kind = (enum jl_partition_kind)(raw_kind & PARTITION_MASK_KIND);
    if (!unchanged_implicit && jl_bkind_is_some_implicit(kind)) {
        // TODO: Should we actually update this in place or delete it from the partitions list
        // and allocate a fresh bpart?
        jl_update_loaded_bpart(b, bpart);
        bpart->kind |= (raw_kind & PARTITION_MASK_FLAG);
        if (jl_atomic_load_relaxed(&bpart->min_world) > jl_require_world)
            goto invalidated;
    }
    {
        if (!jl_bkind_is_some_explicit_import(kind) && kind != PARTITION_KIND_IMPLICIT_GLOBAL)
            return 1;
        jl_binding_t *imported_binding = (jl_binding_t*)bpart->restriction;
        jl_binding_partition_t *latest_imported_bpart = jl_atomic_load_relaxed(&imported_binding->partitions);
        if (no_replacement)
            goto add_backedge;
        if (!latest_imported_bpart)
            return 1;
        if (jl_atomic_load_relaxed(&latest_imported_bpart->min_world) <=
            jl_atomic_load_relaxed(&bpart->min_world)) {
    add_backedge:
            // Imported binding is still valid
            if ((kind == PARTITION_KIND_EXPLICIT || kind == PARTITION_KIND_IMPORTED) &&
                    external_blob_index((jl_value_t*)imported_binding) != mod_idx) {
                jl_add_binding_backedge(imported_binding, (jl_value_t*)b);
            }
            return 1;
        }
        else {
            // Binding partition was invalidated
            assert(jl_atomic_load_relaxed(&bpart->min_world) == jl_require_world);
            jl_atomic_store_relaxed(&bpart->min_world,
                jl_atomic_load_relaxed(&latest_imported_bpart->min_world));
        }
    }
invalidated:
    // We need to go through and re-validate any bindings in the same image that
    // may have imported us.
    if (b->backedges) {
        JL_LOCK(&b->globalref->mod->lock);
        for (size_t i = 0; i < jl_array_len(b->backedges); i++) {
            jl_value_t *edge = jl_array_ptr_ref(b->backedges, i);
            if (!jl_is_binding(edge))
                continue;
            jl_binding_t *bedge = (jl_binding_t*)edge;
            if (!jl_atomic_load_relaxed(&bedge->partitions))
                continue;
            JL_UNLOCK(&b->globalref->mod->lock);
            jl_validate_binding_partition(bedge, jl_atomic_load_relaxed(&bedge->partitions), mod_idx, 0, 0);
            JL_LOCK(&b->globalref->mod->lock);
        }
        JL_UNLOCK(&b->globalref->mod->lock);
    }
    if (bpart->kind & PARTITION_FLAG_EXPORTED) {
        jl_module_t *mod = b->globalref->mod;
        jl_sym_t *name = b->globalref->name;
        JL_LOCK(&mod->lock);
        jl_atomic_store_release(&mod->export_set_changed_since_require_world, 1);
        if (mod->usings_backedges != jl_nothing) {
            for (size_t i = 0; i < jl_array_len(mod->usings_backedges); i++) {
                jl_module_t *edge = (jl_module_t*)jl_array_ptr_ref(mod->usings_backedges, i);
                jl_binding_t *importee = jl_get_module_binding(edge, name, 0);
                if (!importee)
                    continue;
                if (!jl_atomic_load_relaxed(&importee->partitions))
                    continue;
                JL_UNLOCK(&mod->lock);
                jl_validate_binding_partition(importee, jl_atomic_load_relaxed(&importee->partitions), mod_idx, 0, 0);
                JL_LOCK(&mod->lock);
            }
        }
        JL_UNLOCK(&mod->lock);
        return 0;
    }
    return 1;
}

static int all_usings_unchanged_implicit(jl_module_t *mod)
{
    int unchanged_implicit = 1;
    for (size_t i = 0; unchanged_implicit && i < module_usings_length(mod); i++) {
        jl_module_t *usee = module_usings_getmod(mod, i);
        unchanged_implicit &= !jl_atomic_load_acquire(&usee->export_set_changed_since_require_world);
    }
    return unchanged_implicit;
}

// The address of a section, in the loader data that follows the heap
static char *image_section(const char *loader, const jl_image_sections_t *h, jl_image_section_t sec) JL_NOTSAFEPOINT
{
    return (char*)(loader + (sec.offset - h->heap_size));
}

static void jl_restore_system_image_from_stream_(const jl_image_buf_t *buf, jl_image_t *image,
                                                 jl_array_t *depmods,
                                /* outputs */    jl_pkgimage_specials_t *pkg,
                                                 jl_array_t **internal_methods JL_REQUIRE_ROOTED_SLOT,
                                                 image_metadata_t **meta_out) JL_CANSAFEPOINT JL_GC_DISABLED
{
    jl_task_t *ct = jl_current_task;
    int en = jl_gc_enable(0);
    jl_loader_state s = {0};

    if (jl_options.use_sysimage_native_code != JL_OPTIONS_USE_SYSIMAGE_NATIVE_CODE_YES || IMAGE_NATIVE_CODE_TAINTED) {
        memset(&image->fptrs, 0, sizeof(image->fptrs));
        image->gvars_base = NULL;
        IMAGE_NATIVE_CODE_TAINTED = 1;
    }

    // step 1: read the section table
    jl_image_sections_t h;
    memcpy(&h, buf->data + offsetof(jl_ji_header_t, sections), sizeof(h));
    size_t nwords = bitmap_nwords(h.pointer_free_offset);
    char *image_base = (char*)buf->heap;
    const char *loader = buf->loader;
    uint32_t checksum = buf->checksum;
    size_t sizeof_sysimg = h.heap_size;
    if (depmods) {
        size_t ldeps = jl_array_nrows(depmods);
        s.deps = (image_metadata_t**)malloc_s((ldeps + 1) * sizeof(image_metadata_t*));
        s.deps[0] = (image_metadata_t*)eyt_tree_range_data(&image_tree, 0);
        for (size_t i = 0; i < ldeps; i++)
            s.deps[i + 1] = external_blob_metadata(jl_array_ptr_ref(depmods, i));
    }

    // step 2: apply relocations
    size_t nimports = h.imports.size / sizeof(import_t);
    const import_t *imports = (const import_t*)(image_section(loader, &h, h.imports));
    const char *strtab = image_section(loader, &h, h.strtab);
    s.imports_resolved = (uintptr_t*)malloc_s(nimports * sizeof(uintptr_t));
    s.pending = (uint8_t*)calloc_s(nimports);
    for (size_t i = 0; i < nimports; i++) {
        import_t type = imports[i] >> IMPORT_TYPE_OFFSET;
        s.imports_resolved[i] = jl_resolve_import(&s, imports[i], strtab);
        s.pending[i] = IMPORT_HAS_RECORD(type) || type == IMPORT_SINGLETON;
    }
    arraylist_new(&s.waiting, 0);
    s.first_waiting = (size_t*)calloc_s(nimports * sizeof(size_t));
    s.heap_delta = (uintptr_t)image_base - buf->preferred_base;
    // Before anything refers to them, in import order, copy the records of types into provisional
    // types, so that every reference to a type or its singleton instance is to a complete object
    // while the imports are resolved (see doc/src/devdocs/sysimg.md), and resolve the bindings.  A
    // provisional type's `super` is computed when it is needed, so that it is computed only from
    // final types, until all the types are resolved.  MethodInstances read as NULL until they are
    // resolved.
    char *records = image_section(loader, &h, h.auxdata);
#define TYPE_RECORD(i) ((jl_type_record_t*)(records + IMPORT_PAYLOAD(imports[i])))
    for (size_t i = 0; i < nimports; i++) {
        import_t type = imports[i] >> IMPORT_TYPE_OFFSET;
        if (type == IMPORT_SINGLETON) {
            assert(IMPORT_PAYLOAD(imports[i]) < i && "a singleton must follow its type");
            s.imports_resolved[i] = (uintptr_t)((jl_datatype_t*)s.imports_resolved[IMPORT_PAYLOAD(imports[i])])->instance;
            continue;
        }
        if (type == IMPORT_BINDING) {
            jl_binding_record_t *rec = (jl_binding_record_t*)(records + IMPORT_PAYLOAD(imports[i]));
            jl_resolve_relocs(&s, (uintptr_t*)rec, sizeof(*rec) / sizeof(uintptr_t));
            jl_finalize_import(&s, i, (uintptr_t)jl_get_module_binding((jl_module_t*)rec->mod, (jl_sym_t*)rec->name, 1));
            continue;
        }
        if (type != IMPORT_DATATYPE)
            continue;
        jl_type_record_t *rec = TYPE_RECORD(i);
        jl_resolve_relocs(&s, (uintptr_t*)rec, offsetof(jl_type_record_t, super) / sizeof(uintptr_t)); // the relocs before `super`
        jl_datatype_t *dt = jl_new_uninitialized_datatype();
        s.imports_resolved[i] = (uintptr_t)dt;
        dt->name = (jl_typename_t*)rec->name;
        dt->parameters = (jl_svec_t*)rec->parameters;
        dt->types = (jl_svec_t*)rec->types;
        dt->layout = (const jl_datatype_layout_t*)rec->layout;
        dt->instance = (jl_value_t*)rec->instance;
        dt->hash = rec->hash;
        memcpy(TYPE_FLAGS(dt), &rec->flags, sizeof(rec->flags));
        if (dt->instance == jl_nothing) {
            dt->instance = jl_gc_alloc(ct->ptls, 0, dt);
            jl_astaggedvalue(dt->instance)->bits.gc = GC_OLD;
        }
    }
    jl_apply_slides((uintptr_t*)image_base, s.heap_delta, image_section(loader, &h, h.slides), h.slides.size, nwords);
    jl_apply_binds(&s, (uintptr_t*)image_base, image_section(loader, &h, h.binds), h.binds.size, nwords);

    // step 3: get references to special values
    jl_resolve_relocs(&s, (uintptr_t*)(image_section(loader, &h, h.specials)), h.specials.size / sizeof(void*));
    jl_value_t **specials = (jl_value_t**)(image_section(loader, &h, h.specials));
    if (!pkg) {
        size_t i;
        for (i = 0; i < sizeof(jl_image_specials) / sizeof(jl_image_specials[0]); i++)
            *jl_image_specials[i] = *specials++;
        export_jl_small_typeof();
        export_jl_sysimg_globals();
        jl_global_roots_list = (jl_genericmemory_t*)*specials++;
        jl_global_roots_keyset = (jl_genericmemory_t*)*specials++;
        jl_gc_write(ct->ptls->root_task, ct->ptls->root_task->tls, jl_value_t, *specials++);
        jl_set_symtab((jl_sym_t*)*specials++);
        jl_global_roots_max = jl_idset_max(jl_global_roots_list);
        jl_require_world = h.world;
        jl_atomic_store_release(&jl_world_counter, jl_require_world);
        jl_typeinf_world = h.typeinf_world;
        jl_set_gs_ctr((uint32_t)h.gensym_counter);
    }
    else {
        *pkg = *(jl_pkgimage_specials_t*)specials;
        specials += sizeof(*pkg) / sizeof(void*);
        *internal_methods = jl_alloc_vec_any(0);
    }
    assert((char*)specials == image_section(loader, &h, h.specials) + h.specials.size);
    const uintptr_t *fixup_types = (const uintptr_t*)image_section(loader, &h, h.fixup_types);
    const uintptr_t *fixup_objs = (const uintptr_t*)image_section(loader, &h, h.fixup_objs);
    size_t nfixup_types = h.fixup_types.size / sizeof(uintptr_t), nfixup_objs = h.fixup_objs.size / sizeof(uintptr_t);
    // Resolve the provisional types, and the singletons of those types, in import order: a type
    // found in the caches replaces its provisional type, and a provisional type that is not found
    // becomes the type.  What a lookup compares by identity refers to earlier imports, so it is
    // final already.  A new type is cached at once, so that a type built meanwhile (by computing a
    // lazy `super`) finds it, but it can still refer to provisional types, so the other threads are
    // paused.
    arraylist_t new_types; // the indices of the imports that became new types
    arraylist_new(&new_types, 0);
    JL_LOCK(&typecache_lock); // Might GC--prevent other threads from changing any type caches while we inspect them all
    jl_safepoint_suspend_all_threads(ct); // past this point, it is now not safe to observe the intermediate states on other threads via reflection, so temporarily pause those
    for (size_t i = 0; i < nimports; i++) {
        import_t type = imports[i] >> IMPORT_TYPE_OFFSET;
        if (type == IMPORT_SINGLETON) {
            jl_datatype_t *dt = (jl_datatype_t*)s.imports_resolved[IMPORT_PAYLOAD(imports[i])];
            assert(dt->instance);
            jl_finalize_import(&s, i, (uintptr_t)dt->instance);
            continue;
        }
        if (type != IMPORT_DATATYPE)
            continue;
        jl_datatype_t *dt = (jl_datatype_t*)s.imports_resolved[i];
        jl_datatype_t *newdt = jl_lookup_cache_type_(dt);
        if (newdt == NULL) {
            // lazy `types` computed from provisional types are computed again when they are needed
            if (TYPE_RECORD(i)->types == 0)
                dt->types = NULL;
            jl_astaggedvalue(dt)->bits.gc = GC_OLD;
            jl_cache_type_(dt);
            arraylist_push(&new_types, (void*)i);
            newdt = dt;
        }
        else {
            assert(newdt->hash == dt->hash);
        }
        jl_finalize_import(&s, i, (uintptr_t)newdt);
    }
    // set the `super` of each new type that was not computed meanwhile
    for (size_t j = 0; j < new_types.len; j++) {
        size_t i = (size_t)new_types.items[j];
        jl_datatype_t *dt = (jl_datatype_t*)s.imports_resolved[i];
        uintptr_t super = TYPE_RECORD(i)->super;
        if (dt->super == NULL && super != 0) {
            jl_resolve_relocs(&s, &super, 1);
            dt->super = (jl_datatype_t*)super;
        }
    }
    arraylist_free(&new_types);
#undef TYPE_RECORD
#ifndef NDEBUG
    // a type that the lookup missed because it compared a provisional type is a second copy
    for (size_t i = 0; i < nimports; i++) {
        if (imports[i] >> IMPORT_TYPE_OFFSET == IMPORT_DATATYPE)
            assert(jl_lookup_cache_type_((jl_datatype_t*)s.imports_resolved[i]) == (jl_datatype_t*)s.imports_resolved[i] &&
                   "a type was looked up before what it compares by identity was final");
    }
#endif
    for (size_t i = 0; i < nfixup_types; i++) {
        uintptr_t item = fixup_types[i];
        jl_value_t *obj = (jl_value_t*)(image_base + item);
        assert(jl_is_datatype(obj));
        jl_cache_type_((jl_datatype_t*)obj);
    }
    JL_UNLOCK(&typecache_lock); // Might GC
    jl_safepoint_resume_all_threads(ct); // TODO: move this later to also protect MethodInstance allocations, but we would need to acquire all jl_specializations_get_linfo and jl_module_globalref locks, which is hard
    // resolve the MethodInstances, whose records refer to the types
    for (size_t i = 0; i < nimports; i++) {
        if (imports[i] >> IMPORT_TYPE_OFFSET != IMPORT_METHOD_INSTANCE)
            continue;
        jl_mi_record_t *rec = (jl_mi_record_t*)(records + IMPORT_PAYLOAD(imports[i]));
        jl_resolve_relocs(&s, (uintptr_t*)rec, sizeof(*rec) / sizeof(uintptr_t));
        jl_finalize_import(&s, i, (uintptr_t)jl_specializations_get_linfo((jl_method_t*)rec->def, (jl_value_t*)rec->specTypes, (jl_svec_t*)rec->sparam_vals));
    }
    uintptr_t *gvars = (uintptr_t*)(image_section(loader, &h, h.gvars));
    jl_resolve_relocs(&s, gvars, h.gvars.size / sizeof(void*));
#ifndef NDEBUG
    for (size_t i = 0; i < nimports; i++)
        assert(!s.pending[i] && "an import was never resolved");
#endif
    free(s.deps);
    free(s.imports_resolved);
    arraylist_free(&s.waiting);
    free(s.first_waiting);
    free(s.pending);
    // Perform fixups: things like updating world ages, inserting methods & specializations, etc.
    for (size_t i = 0; i < nfixup_objs; i++) {
        uintptr_t item = fixup_objs[i];
        jl_value_t *obj = (jl_value_t*)(image_base + item);
        if (jl_typetagis(obj, jl_typemap_entry_type) || jl_is_method(obj) || jl_is_code_instance(obj)) {
            jl_array_ptr_1d_push(*internal_methods, obj);
            assert(pkg);
        }
        else if (jl_is_method_instance(obj)) {
            jl_method_instance_t *newobj = jl_specializations_get_or_insert((jl_method_instance_t*)obj);
            assert(newobj == (jl_method_instance_t*)obj); // strict insertion expected
            (void)newobj;
        }
        else if (jl_is_module(obj)) {
            // rebuild the usings table for module v
            // TODO: maybe want to hold the lock on `v`, but that only strongly matters for async / thread safety
            // and we are already bad at that
            jl_module_t *mod = (jl_module_t*)obj;
            mod->build_id.hi = checksum;
            if (mod->usings.items != &mod->usings._space[0]) {
                // arraylist_t assumes we called malloc to get this memory, so make that true now
                void **newitems = (void**)malloc_s(mod->usings.max * sizeof(void*));
                memcpy(newitems, mod->usings.items, mod->usings.len * sizeof(void*));
                mod->usings.items = newitems;
            }
            size_t mod_idx = external_blob_index((jl_value_t*)mod);
            if (pkg) {
                // Rebuild cross-image usings backedges
                for (size_t i = 0; i < module_usings_length(mod); ++i) {
                    struct _jl_module_using *data = module_usings_getidx(mod, i);
                    if (external_blob_index((jl_value_t*)data->mod) != mod_idx) {
                        jl_add_usings_backedge(data->mod, mod);
                    }
                }
            }
        }
        else if (jl_is_cancel_source(obj)) {
            // relink under its parents, as if newly allocated (the weak
            // child links were dropped at serialization)
            jl_cancel_source_relink((jl_cancel_source_t*)obj);
        }
        else {
            abort();
        }
    }
    if (pkg) {
        int no_replacement = jl_atomic_load_relaxed(&jl_first_image_replacement_world) == ~(size_t)0;
        for (size_t i = 0; i < nfixup_objs; i++) {
            uintptr_t item = fixup_objs[i];
            jl_value_t *obj = (jl_value_t*)(image_base + item);
            if (jl_is_module(obj)) {
                jl_module_t *mod = (jl_module_t*)obj;
                size_t mod_idx = external_blob_index((jl_value_t*)mod);
                jl_svec_t *table = jl_atomic_load_relaxed(&mod->bindings);
                int unchanged_implicit = no_replacement || all_usings_unchanged_implicit(mod);
                for (size_t i = 0; i < jl_svec_len(table); i++) {
                    jl_binding_t *b = (jl_binding_t*)jl_svecref(table, i);
                    if ((jl_value_t*)b == jl_nothing)
                        continue;
                    jl_binding_partition_t *bpart = jl_atomic_load_relaxed(&b->partitions);
                    if (!jl_validate_binding_partition(b, bpart, mod_idx, unchanged_implicit, no_replacement)) {
                        unchanged_implicit = all_usings_unchanged_implicit(mod);
                    }
                }
            }
        }
    }

    jl_update_all_gvars(image, gvars, h.gvars.size / sizeof(void*), h.gvar_objects, pkg != NULL);

    jl_update_all_fptrs(image, image_base, (const uintptr_t*)image_section(loader, &h, h.fptrs),
                        h.fptrs.size / sizeof(uintptr_t));

    if (!pkg)
        jl_gc_reset_alloc_count();

    // Prepare for later external linkage against the sysimg
    // Also sets up images for protection against garbage collection
    image_metadata_t *meta = (image_metadata_t*)malloc_s(sizeof(image_metadata_t));
    meta->base = (uintptr_t)image_base;
    meta->deps = NULL; // until the blob index is known
    // the sysimage's methods are active by jl_require_world; a package image's
    // are activated by the caller, which records the world
    jl_atomic_store_relaxed(&meta->world, pkg ? 0 : jl_require_world);
    if (meta_out)
        *meta_out = meta;
    // adopt the image's coverage counters
    meta->coverage_compatible = jl_register_image_coverage(image->coverage, !pkg);
    if (!pkg) {
        meta->top_mod = jl_top_module;
    } else {
        size_t len = jl_array_nrows(pkg->worklist);
        assert(len > 0);
        jl_module_t *topmod = (jl_module_t*)jl_array_ptr_ref(pkg->worklist, len-1);
        // Ordinarily set during deserialization, but our compiler stub image,
        // just returns a reference to the sysimage version, so we set it here.
        topmod->build_id.hi = checksum;
        assert(jl_is_module(topmod));
        meta->top_mod = topmod;
        // plain images accepted for path coverage have no code from the tracked path
        if (!meta->coverage_compatible)
            meta->coverage_compatible = jl_coverage_plain_image_accepted(checksum, topmod->build_id.lo);
    }
    // the tree assigns the index under its lock, as other images may be loading
    meta->idx = eyt_tree_add_range(&image_tree,
        (uintptr_t)image_base,
        (uintptr_t)image_base + sizeof_sysimg,
        (void*)meta);
    meta->deps = image_deps_new(meta->idx, depmods);
    if (!pkg) {
        jl_sysimage_start = (uintptr_t)image_base;
        jl_sysimage_size = sizeof_sysimg;
    }
    jl_timing_counter_inc(JL_TIMING_COUNTER_ImageSize, sizeof_sysimg);

    jl_gc_enable(en);

    if (pkg)
        jl_add_methods(pkg->extext_methods);

}

// Check that the image in `buf` can be loaded with `depmods`: returns NULL if so, else an exception
static jl_value_t *jl_validate_cache_file(const jl_image_buf_t *buf, jl_array_t *depmods) JL_CANSAFEPOINT
{
    uint32_t flags = buf->flags;
    if (!buf->heap)
        return jl_get_exceptionf(jl_errorexception_type,
                                 "Precompile file header verification checks failed.");

    if (buf->is_split != !!(flags & JI_FLAG_SPLIT))
        return jl_get_exceptionf(jl_errorexception_type, "Expected %s image, but got %s",
                                 buf->is_split ? "split" : "standalone",
                                 flags & JI_FLAG_SPLIT ? "split" : "standalone");

    if (!!depmods != !!(flags & JI_FLAG_PKGIMAGE))
        return jl_get_exceptionf(jl_errorexception_type,
                                 "Expected %s, but cache file is for %s",
                                 depmods ? "pkgimage" : "system image",
                                 flags & JI_FLAG_PKGIMAGE ? "pkgimage" : "system image");

    if (!depmods)
        return NULL;
    ios_t f; // over the image's data, which it does not own, so it needs no closing
    ios_static_buffer(&f, (char*)buf->data, buf->size);
    ios_seek(&f, sizeof(jl_ji_header_t));
    // Syntax version mismatch is not fatal to load
    if (!jl_match_cache_flags_current(read_uint8(&f)))
        return jl_get_exceptionf(jl_errorexception_type, "Pkgimage flags mismatch");
    int coverage_ok = jl_match_cache_coverage(jl_image_coverage_config(), read_uint8(&f));

    (void)read_uint8(&f); // syntax_version

    // skip past the worklist, reading the package's build id to validate coverage
    size_t len;
    int first = 1;
    while ((len = read_int32(&f))) {
        ios_skip(&f, len + 2 * sizeof(uint64_t));
        uint64_t build_id_lo = read_uint64(&f);
        if (first && !coverage_ok)
            coverage_ok = jl_coverage_plain_image_accepted(buf->checksum, build_id_lo);
        first = 0;
    }
    if (!coverage_ok)
        return jl_get_exceptionf(jl_errorexception_type, "Pkgimage coverage instrumentation mismatch");
    // skip past the dependency list
    size_t deplen = read_uint64(&f);
    ios_skip(&f, deplen - sizeof(uint64_t));
    // verify that the system state is valid
    return read_verify_mod_list(&f, depmods);
}

// TODO?: refactor to make it easier to create the "package inspector"
static jl_value_t *jl_restore_incremental_from_buf(const jl_image_buf_t *buf, jl_image_t *image, jl_array_t *depmods, int completeinfo, const char *pkgname) JL_CANSAFEPOINT
{
    JL_TIMING(LOAD_IMAGE, LOAD_Pkgimg);
    jl_timing_printf(JL_TIMING_DEFAULT_BLOCK, pkgname);
    jl_value_t *verify_fail = jl_validate_cache_file(buf, depmods);
    if (verify_fail)
        return verify_fail;

    jl_value_t *restored;
    jl_pkgimage_specials_t pkg = {0};
    jl_array_t *internal_methods = NULL;
    jl_svec_t *cachesizes_sv = NULL;
    JL_GC_PUSH7(&pkg.worklist, &pkg.module_init_order, &pkg.extext_methods, &pkg.extext_records, &pkg.method_roots_list, &internal_methods, &cachesizes_sv);

    {
        JL_SIGATOMIC_BEGIN();
        image_metadata_t *meta = NULL;
        jl_restore_system_image_from_stream_(buf, image, depmods, &pkg, &internal_methods, &meta);
        JL_SIGATOMIC_END();

        // Add roots to methods
        int failed = jl_copy_roots(pkg.method_roots_list, jl_worklist_key(pkg.worklist));
        if (failed != 0) {
            jl_printf(JL_STDERR, "Error copying roots to methods from Module: %s\n", pkgname);
            abort();
        }
        // Insert method extensions and handle edges
        int new_methods = jl_array_nrows(pkg.extext_methods) > 0;
        if (!new_methods) {
            size_t i, l = jl_array_nrows(internal_methods);
            for (i = 0; i < l; i++) {
                jl_value_t *obj = jl_array_ptr_ref(internal_methods, i);
                if (jl_is_method(obj)) {
                    new_methods = 1;
                    break;
                }
            }
        }
        JL_LOCK(&world_counter_lock);
        // allocate a world for the new methods, and insert them there, invalidating content as needed
        size_t world = jl_atomic_load_relaxed(&jl_world_counter);
        if (new_methods)
            world += 1;
        jl_atomic_store_relaxed(&meta->world, world); // before activation, which asks about this image's dependencies
        jl_activate_methods(pkg.extext_methods, pkg.extext_records, internal_methods, world, pkgname, meta->idx);
        // TODO: inject internal_methods into caches here, so the system can see them immediately as potential candidates (before validation)
        // allow users to start running in this updated world
        if (new_methods)
            jl_atomic_store_release(&jl_world_counter, world);
        // now permit more methods to be added again
        JL_UNLOCK(&world_counter_lock);

        if (completeinfo) {
            // the sizes of the image's parts, for PkgCacheInspector
            jl_image_sections_t h;
            memcpy(&h, buf->data + offsetof(jl_ji_header_t, sections), sizeof(h));
            cachesizes_sv = jl_alloc_svec(7);
            jl_svecset(cachesizes_sv, 0, jl_box_long(h.pointer_free_offset));
            jl_svecset(cachesizes_sv, 1, jl_box_long(h.heap_size - h.pointer_free_offset));
            jl_svecset(cachesizes_sv, 2, jl_box_long(h.strtab.size));
            jl_svecset(cachesizes_sv, 3, jl_box_long(h.imports.size + h.binds.size));
            jl_svecset(cachesizes_sv, 4, jl_box_long(h.slides.size + h.fixup_types.size + h.fixup_objs.size));
            jl_svecset(cachesizes_sv, 5, jl_box_long(h.gvars.size));
            jl_svecset(cachesizes_sv, 6, jl_box_long(h.fptrs.size));
            // Surface extext_methods to external inspectors (e.g.
            // PkgCacheInspector.jl). With the single global jl_method_table,
            // `extext_methods` contains all worklist methods; `internal_methods`
            // only partially overlaps it and exists only for per-object world-stamp
            // updates during the fixup walk.
            restored = (jl_value_t*)jl_svec(6, pkg.worklist, pkg.module_init_order, internal_methods, pkg.extext_methods, pkg.method_roots_list, cachesizes_sv);
        }
        else {
            restored = (jl_value_t*)jl_svec(3, pkg.worklist, pkg.module_init_order, internal_methods);
        }
    }

    JL_GC_POP();
    return restored;
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental(const char *fname, jl_array_t *depmods, int completeinfo, const char *pkgname)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL) {
        return jl_get_exceptionf(jl_errorexception_type,
            "Cache file \"%s\" not found.\n", fname);
    }
    jl_image_buf_t buf = { JL_IMAGE_KIND_JI };
    int res = jl_image_read_ji(&f, &buf, 1);
    ios_close(&f);
    if (res <= 0)
        return jl_get_exceptionf(jl_errorexception_type, "Error reading package image file \"%s\".", fname);
    jl_gc_notify_image_load(buf.heap, buf.heap_size);
    jl_image_t pkgimage = {};
    jl_value_t *ret = jl_restore_incremental_from_buf(&buf, &pkgimage, depmods, completeinfo, pkgname);
    free((char*)buf.data);
    return ret;
}

// Take `nothing` from the system image before it is relocated, so that the runtime never
// creates its own: it has no pointer fields, so it is usable where it is, and its special is
// a slide.  Returns 0 if there is no usable system image.
int jl_adopt_image_constants(jl_image_buf_t buf) JL_CANSAFEPOINT
{
    jl_image_sections_t h;
    size_t i, n = sizeof(jl_image_specials) / sizeof(jl_image_specials[0]);
    if (!buf.heap)
        return 0;
    memcpy(&h, buf.data + offsetof(jl_ji_header_t, sections), sizeof(h));
    const uintptr_t *specials = (const uintptr_t*)image_section(buf.loader, &h, h.specials);
    for (i = 0; i < n; i++) {
        if (jl_image_specials[i] == &jl_nothing)
            jl_nothing = (jl_value_t*)(buf.heap + (specials[i] - buf.preferred_base));
    }
    return 1;
}

JL_DLLEXPORT void jl_restore_system_image(jl_image_t *image, jl_image_buf_t buf)
{
    if (buf.kind == JL_IMAGE_KIND_NONE)
        return;

    if (buf.kind == JL_IMAGE_KIND_SO)
        assert(image->fptrs.ptrs); // jl_load_sysimg should already be run

    JL_SIGATOMIC_BEGIN();
    {
        JL_TIMING(LOAD_IMAGE, LOAD_Sysimg);
        jl_value_t *exc = jl_validate_cache_file(&buf, NULL);
        if (exc)
            jl_throw(exc);
        jl_restore_system_image_from_stream_(&buf, image, NULL, NULL, NULL, NULL);
        jl_init_git_commit();
    }
    if (buf.data_owned) {
        free((char*)buf.data);
        jl_sysimage_buf.data = NULL;
        jl_sysimage_buf.loader = NULL;
    }
    JL_SIGATOMIC_END();
}

JL_DLLEXPORT jl_value_t *jl_restore_package_image_from_file(const char *fname, jl_array_t *depmods, int completeinfo, const char *pkgname, int ignore_native) JL_CANSAFEPOINT
{
    void *pkgimg_handle = jl_dlopen_e(fname, JL_RTLD_LAZY);
    jl_image_buf_t buf = get_image_buf(pkgimg_handle, /* is_pkgimage */ 1);
    if (buf.data == NULL) {
        // unload it, so that a recompiled image at the same path is not served this handle
        jl_dlclose(pkgimg_handle);
        return jl_get_exceptionf(jl_errorexception_type,
            "Image checksum mismatch: the heap image (.ji) was not "
            "compiled for use with \"%s\".", fname);
    }

    jl_gc_notify_image_load(buf.heap, buf.heap_size);

    // Despite the name, this function actually parses the pkgimage
    jl_image_t pkgimage = jl_load_pkgimg(buf);

    if (ignore_native) {
        // Must disable using native code in possible downstream users of this code:
        // https://github.com/JuliaLang/julia/pull/52123#issuecomment-1959965395.
        // The easiest way to do that is to disable it in all of them.
        IMAGE_NATIVE_CODE_TAINTED = 1;
    }

    jl_value_t* mod = jl_restore_incremental_from_buf(&buf, &pkgimage, depmods, completeinfo, pkgname);
    if (buf.data_owned)
        free((char*)buf.data);
    return mod;
}


#ifdef __cplusplus
}
#endif
