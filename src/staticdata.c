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

#include <zstd.h>

#include "julia.h"
#include "julia_internal.h"
#include "julia_gcext.h"
#include "builtin_proto.h"
#include "processor.h"
#include "serialize.h"

#ifdef _OS_WINDOWS_
#include <memoryapi.h>
#else
#include <dlfcn.h>
#include <sys/mman.h>
#endif

#include "valgrind.h"
#include "julia_assert.h"
#include "eytzinger.h"

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

// TODO: put WeakRefs on the weak_refs list during deserialization
// TODO: handle finalizers

#define NUM_TAGS    6

// An array of special references that need to be restored from the sysimg
static void get_tags(jl_value_t **tags[NUM_TAGS])
{
    // Make sure to keep an extra slot at the end to sentinel length
    unsigned int i = 0;
#define INSERT_TAG(sym) tags[i++] = (jl_value_t**)&(sym)
    INSERT_TAG(jl_method_table);
    INSERT_TAG(jl_module_init_order);
    INSERT_TAG(jl_typeinf_func);
    INSERT_TAG(jl_compile_and_emit_func);
    INSERT_TAG(jl_libdl_dlopen_func);
    // n.b. must update NUM_TAGS when you add something here
#undef INSERT_TAG
    assert(i == NUM_TAGS - 1);
    tags[i] = NULL;
}

// hash of definitions for predefined tagged object
static htable_t symbol_table;
static uintptr_t nsym_tag;
// array of definitions for the predefined tagged object types
// (reverse of symbol_table)
static arraylist_t deser_sym;

static htable_t serialization_order; // to break cycles, mark all objects that are serialized
static htable_t nullptrs;
// FIFO queue for objects to be serialized. Anything requiring fixup upon deserialization
// must be "toplevel" in this queue. For types, parameters and field types must appear
// before the "wrapper" type so they can be properly recached against the running system.
static arraylist_t serialization_queue;
static arraylist_t layout_table;     // cache of `position(s)` for each `id` in `serialization_order`
static arraylist_t object_worklist;  // used to mimic recursion by jl_serialize_reachable
static arraylist_t deferred_supers;  // deferred datatype super fields, handled by jl_serialize_reachable once the pre-order recursion has unwound

// Track image locations and some associated metadata for all loaded sys / pkgimages.
// (image_tree.ranges[i].data is an image_metadata_t* for build_ids[i])
static eyt_tree_t image_tree;
typedef struct {
    uintptr_t base;          // base address of the image
    void *relocs_base;       // reloc_t* for GC sweep
    jl_module_t *top_mod;    // owning top-level module
    size_t idx;              // range index in image_tree (for serialization)
} image_metadata_t;

void jl_init_staticdata(void)
{
    eyt_tree_init(&image_tree);
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
//static size_t precompilation_world;

// Query if a Julia object is in a permalloc region (due to part of a sys- pkg-image)
static size_t n_linkage_blobs(void) JL_NOTSAFEPOINT
{
    return image_tree.nranges;
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

size_t jl_external_blob_index(jl_value_t *v) JL_NOTSAFEPOINT
{
    return external_blob_index(v);
}

// Install the dependency-closure blob bitset (sysimage, each dependency image,
// and the loading image itself, identified from the first image-owned anchor)
// for the certificate-replay checks; used around pkgimage edge verification.
static size_t *loading_closure_bits_owned = NULL;
JL_DLLEXPORT void jl_set_loading_closure_from_depmods(jl_array_t *depmods, jl_array_t *anchors)
{
    size_t nblobs = n_linkage_blobs();
    size_t nwords = (nblobs + 8 * sizeof(size_t) - 1) / (8 * sizeof(size_t));
    size_t *bits = (size_t*)calloc_s(nwords ? nwords * sizeof(size_t) : sizeof(size_t));
    bits[0] |= 1;
    if (depmods) {
        for (size_t i = 0, ld = jl_array_nrows(depmods); i < ld; i++) {
            size_t idx = external_blob_index(jl_array_ptr_ref(depmods, i));
            if (idx < nblobs)
                bits[idx / (8 * sizeof(size_t))] |= (size_t)1 << (idx % (8 * sizeof(size_t)));
        }
    }
    if (anchors) {
        for (size_t i = 0, la = jl_array_nrows(anchors); i < la; i++) {
            jl_value_t *a = jl_array_ptr_ref(anchors, i);
            if (a && jl_object_in_image(a)) {
                size_t idx = external_blob_index(a);
                if (idx < nblobs)
                    bits[idx / (8 * sizeof(size_t))] |= (size_t)1 << (idx % (8 * sizeof(size_t)));
                break;
            }
        }
    }
    assert(loading_closure_bits_owned == NULL);
    loading_closure_bits_owned = bits;
    jl_set_loading_closure_blobs(bits, nblobs);
}

JL_DLLEXPORT void jl_clear_loading_closure(void)
{
    jl_set_loading_closure_blobs(NULL, 0);
    free(loading_closure_bits_owned);
    loading_closure_bits_owned = NULL;
}

JL_DLLEXPORT uint8_t jl_object_in_image(jl_value_t *obj) JL_NOTSAFEPOINT
{
    if (obj == NULL)
        return 0;
    uint8_t in_image = jl_astaggedvalue(obj)->bits.in_image != 0;
    assert((uintptr_t) obj % 4 == 0 && "Object not 4-byte aligned!");
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

// hash of definitions for predefined function pointers
// (reverse is jl_builtin_f_addrs)
static htable_t fptr_to_id;

void *native_functions;   // opaque jl_native_code_desc_t blob used for fetching data from LLVM

// table of struct field addresses to rewrite during saving
static htable_t field_replace;
static htable_t bits_replace;


typedef struct {
    ios_t *s;                   // the main stream
    ios_t *const_data;          // GC-invisible internal data (e.g., datatype layouts, list-like typename fields, foreign types, internal arrays)
    ios_t *symbols;             // names (char*) of symbols (some may be referenced by pointer in generated code)
    ios_t *relocs;              // for (de)serializing relocs_list and gctags_list
    ios_t *gvar_record;         // serialized array mapping gvid => spos
    ios_t *fptr_record;         // serialized array mapping fptrid => spos
    arraylist_t memowner_list;  // a list of memory locations that have shared owners
    arraylist_t memref_list;    // a list of memoryref locations
    arraylist_t relocs_list;    // a list of (location, target) pairs, see description at top
    arraylist_t gctags_list;    //      "
    arraylist_t uniquing_types; // a list of locations that reference types that must be de-duplicated
    arraylist_t uniquing_super; // a list of datatypes, used in super fields, that need to be marked in uniquing_types once they are reached, for handling unique-ing of them on deserialization
    arraylist_t uniquing_objs;  // a list of locations that reference non-types that must be de-duplicated
    arraylist_t fixup_types;    // a list of locations of types requiring (re)caching
    arraylist_t fixup_objs;     // a list of locations of objects requiring (re)caching
    // mapping from a buildid_idx to a depmods_idx
    jl_array_t *buildid_depmods_idxs;
    // record of build_ids for all external linkages, in order of serialization for the current sysimg/pkgimg
    // conceptually, the base pointer for the jth externally-linked item is determined from
    //     i = findfirst(==(link_ids[j]), build_ids)
    //     blob_base = ((image_metadata_t*)image_tree.ranges[i].data)->base
    // We need separate lists since they are intermingled at creation but split when written.
    jl_array_t *link_ids_relocs;
    jl_array_t *link_ids_gctags;
    jl_array_t *link_ids_gvars;
    jl_array_t *link_ids_external_fnvars;
    jl_array_t *method_roots_list;
    htable_t method_roots_index;
    uint64_t worklist_key;
    jl_query_cache *query_cache;
    jl_ptls_t ptls;
    jl_image_t *image;
    int8_t incremental;
} jl_serializer_state;

static jl_value_t *jl_bigint_type = NULL;
static jl_debuginfo_t *jl_nulldebuginfo;
static int gmp_limb_size = 0;

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

#define SYS_EXTERNAL_LINK_UNIT sizeof(void*)

// Sub-divisions of some RefTags
const uintptr_t BuiltinFunctionTag = ((uintptr_t)1 << (RELOC_TAG_OFFSET - 1));
// Bit set on FunctionRef when invoke should be set to jl_fptr_args, and should
// not be zeroed when we want to disable native code.
const uintptr_t BuiltinInvokeTag = ((uintptr_t)1 << (RELOC_TAG_OFFSET - 2));


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

#define NBOX_C 1024

static int jl_needs_serialization(jl_serializer_state *s, jl_value_t *v) JL_NOTSAFEPOINT
{
    // ignore items that are given a special relocation representation
    if (s->incremental && jl_object_in_image(v))
        return 0;

    if (v == NULL || jl_is_symbol(v) || v == jl_nothing) {
        return 0;
    }
    else if (jl_typetagis(v, jl_int64_tag << 4)) {
        int64_t i64 = *(int64_t*)v + NBOX_C / 2;
        if ((uint64_t)i64 < NBOX_C)
            return 0;
    }
    else if (jl_typetagis(v, jl_int32_tag << 4)) {
        int32_t i32 = *(int32_t*)v + NBOX_C / 2;
        if ((uint32_t)i32 < NBOX_C)
            return 0;
    }
    else if (jl_typetagis(v, jl_uint8_tag << 4)) {
        return 0;
    }
    else if (v == (jl_value_t*)s->ptls->root_task) {
        return 0;
    }

    return 1;
}

static int caching_tag(jl_value_t *v, jl_query_cache *query_cache) JL_NOTSAFEPOINT
{
    if (jl_is_method_instance(v)) {
        jl_method_instance_t *mi = (jl_method_instance_t*)v;
        jl_value_t *m = mi->def.value;
        if (jl_is_method(m) && jl_object_in_image(m))
            return 1 + type_in_worklist(mi->specTypes, query_cache);
    }
    if (jl_is_binding(v)) {
        jl_globalref_t *gr = ((jl_binding_t*)v)->globalref;
        if (!gr)
            return 0;
        if (!jl_object_in_image((jl_value_t*)gr->mod))
            return 0;
        return 1;
    }
    if (jl_is_datatype(v)) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        if (jl_is_tuple_type(dt) ? !dt->isconcretetype : dt->hasfreetypevars)
            return 0; // aka !is_cacheable from jltypes.c
        if (jl_object_in_image((jl_value_t*)dt->name))
            return 1 + type_in_worklist(v, query_cache);
    }
    jl_value_t *dtv = jl_typeof(v);
    if (jl_is_datatype_singleton((jl_datatype_t*)dtv)) {
        return 1 - type_in_worklist(dtv, query_cache); // these are already recached in the datatype in the image
    }
    return 0;
}

static int needs_recaching(jl_value_t *v, jl_query_cache *query_cache) JL_NOTSAFEPOINT
{
    return caching_tag(v, query_cache) == 2;
}

static int needs_uniquing(jl_value_t *v, jl_query_cache *query_cache) JL_NOTSAFEPOINT
{
    assert(!jl_object_in_image(v));
    return caching_tag(v, query_cache) == 1;
}

static void record_field_change(jl_value_t **addr, jl_value_t *newval) JL_NOTSAFEPOINT
{
    if (*addr != newval)
        ptrhash_put(&field_replace, (void*)addr, newval);
}

// --- InternedCodeInstance: relocation-free edge lists (see julia.h) ---
// Words are (imagekey << DEPS_IDX_OFFSET) | (offset in 8-byte units), where
// imagekey 0 is the containing image's data section and k > 0 is the
// serialized dependency with depsidx k-1; or, with the high bit set, a
// zigzag-encoded literal Int (edge-group headers). Self-referential words
// are patched after layout (ici_fixups); external ones resolve at build time.
#define ICI_TAG_LITERAL (((uintptr_t)1) << 63)
#define ICI_TAG_REF     (((uintptr_t)1) << 62)
#define ICI_TAG_CONST   (((uintptr_t)1) << 61) // ref into the const-data stream (self image only)
static void jl_queue_for_serialization_(jl_serializer_state *s, jl_value_t *v, int recursive, int immediate) JL_GC_DISABLED;
static jl_value_t *svec_dedup_canonical(jl_serializer_state *s, jl_value_t *v) JL_GC_DISABLED;

static int ici_debug_enabled(void) JL_NOTSAFEPOINT
{
    static int on = -1;
    if (on == -1)
        on = getenv("JULIA_ICI_DEBUG") != NULL;
    return on;
}

JL_DLLEXPORT int jl_get_ici_debug_enabled(void) JL_NOTSAFEPOINT
{
    return ici_debug_enabled();
}

static uintptr_t ici_field_gate(const char *envname, uintptr_t dflt) JL_NOTSAFEPOINT
{
    char *e = getenv(envname);
    return e ? strtoull(e, NULL, 16) : dflt;
}
typedef struct { jl_value_t *target; jl_value_t *invokesig; uint32_t cls; uint32_t orig; } belog_trip_t;
static int belog_trip_cmp(const void *a_, const void *b_) JL_NOTSAFEPOINT
{
    const belog_trip_t *a = (const belog_trip_t*)a_, *b = (const belog_trip_t*)b_;
    if (a->cls != b->cls)
        return a->cls < b->cls ? -1 : 1;
    if (a->target != b->target)
        return (uintptr_t)a->target < (uintptr_t)b->target ? -1 : 1;
    if (a->invokesig != b->invokesig)
        return (uintptr_t)a->invokesig < (uintptr_t)b->invokesig ? -1 : 1;
    return a->orig < b->orig ? -1 : a->orig > b->orig ? 1 : 0;
}

// map from interned symbol to (depsidx+1)<<40 | index for symbols present in
// a dependency image's symbol table; built once per incremental save so the
// output references them as (dep, index) instead of re-serializing the names
static htable_t depsym_map;
static int depsym_map_init = 0;
static void depsym_map_build(jl_serializer_state *s) JL_GC_DISABLED;

// call/method signatures whose typename decompositions are serialized for the
// loader (save-side shift of jl_foreach_top_typename_for)
static htable_t edge_sig_set;
static int edge_sig_set_init = 0;

static int save_sig_tns_enabled(void) JL_NOTSAFEPOINT
{
    static int on = -1;
    if (on == -1) {
        char *e = getenv("JULIA_SIG_TNS");
        on = e != NULL && strcmp(e, "1") == 0;
    }
    return on;
}

static void edge_sig_note(jl_value_t *sig) JL_GC_DISABLED
{
    if (sig == NULL || !jl_is_type(sig) || !save_sig_tns_enabled())
        return;
    if (!edge_sig_set_init) {
        htable_new(&edge_sig_set, 1 << 12);
        edge_sig_set_init = 1;
    }
    void **bp = ptrhash_bp(&edge_sig_set, (void*)sig);
    if (*bp == HT_NOTFOUND)
        *bp = (void*)sig;
}

// mirror of the load-side edge-group structure walk (jl_preverify_clean_cis)
static void collect_edge_group_sigs(jl_svec_t *edges) JL_GC_DISABLED
{
    size_t n = jl_svec_len(edges);
    for (size_t j = 0; j < n; j++) {
        jl_value_t *item = jl_svecref(edges, j);
        if (item == NULL)
            continue;
        if (jl_is_long(item)) { // (n, sig, targets...) match group
            ssize_t nt = jl_unbox_long(item);
            if (nt < 0)
                nt = -nt;
            if (j + 1 < n)
                edge_sig_note(jl_svecref(edges, j + 1));
            j += 1 + (size_t)nt;
        }
        else if (jl_is_code_instance(item)) {
            edge_sig_note(jl_get_ci_mi((jl_code_instance_t*)item)->specTypes);
        }
        else if (jl_is_method_instance(item)) {
            edge_sig_note(((jl_method_instance_t*)item)->specTypes);
        }
        else if (jl_is_binding(item) || jl_is_method(item)) {
        }
        else { // (invokesig, target) pair
            jl_value_t *target = j + 1 < n ? jl_svecref(edges, j + 1) : NULL;
            if (target != NULL && !jl_is_mtable(target))
                edge_sig_note(item);
            j += 1;
        }
    }
}

static jl_typename_t *method_first_tn(jl_method_t *m) JL_NOTSAFEPOINT
{
    jl_value_t *sig = jl_unwrap_unionall(m->sig);
    if (!jl_is_datatype(sig) || jl_nparams(sig) == 0)
        return NULL;
    jl_value_t *t = jl_tparam0(sig);
    while (jl_is_unionall(t))
        t = ((jl_unionall_t*)t)->body;
    if (!jl_is_datatype(t))
        return NULL;
    return ((jl_datatype_t*)t)->name;
}

static int extext_method_cmp(const void *a_, const void *b_) JL_NOTSAFEPOINT
{
    // elements are (method, certificate) pairs; key on the method
    jl_value_t *a = ((jl_value_t**)a_)[0], *b = ((jl_value_t**)b_)[0];
    jl_typename_t *ta = jl_is_method(a) ? method_first_tn((jl_method_t*)a) : NULL;
    jl_typename_t *tb = jl_is_method(b) ? method_first_tn((jl_method_t*)b) : NULL;
    if (ta != tb)
        return (uintptr_t)ta < (uintptr_t)tb ? -1 : 1;
    return a < b ? -1 : a > b ? 1 : 0;
}

struct _sig_tn_collect {
    jl_value_t *tns[62];
    uint64_t explbits;
    int n;
    int overflow;
};

static void _sig_tn_collect_cb(jl_typename_t *tn, int explct, void *env0) JL_NOTSAFEPOINT
{
    struct _sig_tn_collect *env = (struct _sig_tn_collect*)env0;
    if (env->n >= 62) {
        env->overflow = 1;
        return;
    }
    if (explct)
        env->explbits |= (uint64_t)1 << env->n;
    env->tns[env->n++] = (jl_value_t*)tn;
}

static arraylist_t ici_fixups; // (ici, wordidx, target) triples
static int ici_fixups_init = 0;

static uintptr_t *ici_words(jl_interned_code_instance_t *ici) JL_NOTSAFEPOINT
{
    return (uintptr_t*)(ici + 1);
}
#define ICI_DEFWORD_IDX ((size_t)-1) // fixup-record sentinel for the defword slot

static int ici_would_be_const_data(jl_value_t *v) JL_NOTSAFEPOINT
{
    // mirror the stream routing in jl_write_values
    jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
    if (t->smalltag && (t->layout->npointers == 0 || t == jl_string_type))
        if (jl_datatype_nfields(t) == 0 || t->name->mutabl == 0 || t == jl_string_type)
            return 1;
    return 0;
}

// classify a field value for interning: 0 = keep the pointer field
// (NULL, symbol, unmapped image, needs uniquing); 1 = immediate word in *w;
// 2 = serialized with this image, patch after layout
static int ici_classify_value(jl_serializer_state *s, jl_value_t *e, uintptr_t *w) JL_GC_DISABLED
{
    if (e == NULL || jl_is_symbol(e))
        return 0;
    if (jl_object_in_image(e)) {
        uint32_t *blob_to_depsidx = jl_array_data(s->buildid_depmods_idxs, uint32_t);
        size_t nblobmap = jl_array_len(s->buildid_depmods_idxs);
        size_t blob = external_blob_index(e);
        if (blob >= nblobmap || blob_to_depsidx[blob] == (uint32_t)-1)
            return 0;
        image_metadata_t *meta = (image_metadata_t*)image_tree.ranges[blob].data;
        uintptr_t off = ((uintptr_t)e - (uintptr_t)meta->base) / SYS_EXTERNAL_LINK_UNIT;
        assert(off < ((uintptr_t)1 << DEPS_IDX_OFFSET));
        *w = ICI_TAG_REF | (((uintptr_t)blob_to_depsidx[blob] + 1) << DEPS_IDX_OFFSET) | off;
        return 1;
    }
    if (!jl_needs_serialization(s, e))
        return 0; // special relocation representation (boxed-int caches, root task, ...)
    if (needs_uniquing(e, s->query_cache))
        return 0;
    return 2;
}

static jl_value_t *ici_try_build(jl_serializer_state *s, jl_svec_t *edges, size_t nfieldwords) JL_GC_DISABLED
{
    if (!s->incremental || s->buildid_depmods_idxs == NULL)
        return NULL;
    size_t n = jl_svec_len(edges);
    if (n == 0 && nfieldwords == 0)
        return NULL;
    // at save time buildid_depmods_idxs maps linkage-blob index -> depsidx
    uint32_t *blob_to_depsidx = jl_array_data(s->buildid_depmods_idxs, uint32_t);
    size_t nblobmap = jl_array_len(s->buildid_depmods_idxs);
    // validate every edge is encodable before allocating
    for (size_t i = 0; i < n; i++) {
        jl_value_t *e = jl_svecref(edges, i);
        if (e == NULL)
            return NULL; // unset entry: keep the svec form
        if (jl_is_long(e))
            continue; // literal
        if (jl_object_in_image(e)) {
            size_t blob = external_blob_index(e);
            if (blob >= nblobmap || blob_to_depsidx[blob] == (uint32_t)-1)
                return NULL; // not among our recorded dependencies
        }
    }
    jl_task_t *ct = jl_current_task;
    jl_interned_code_instance_t *ici = (jl_interned_code_instance_t*)jl_gc_alloc(
        ct->ptls, sizeof(jl_interned_code_instance_t) + (n + nfieldwords) * sizeof(uintptr_t),
        jl_interned_code_instance_type);
    ici->nedges = n;
    ici->defword = 0;
    ici->fieldmask = 0;
    uintptr_t *words = ici_words(ici);
    memset(words + n, 0, nfieldwords * sizeof(uintptr_t));
    for (size_t i = 0; i < n; i++) {
        jl_value_t *e = jl_svecref(edges, i);
        if (jl_is_long(e)) {
            intptr_t v = jl_unbox_long(e);
            words[i] = ICI_TAG_LITERAL | ((((uintptr_t)v << 1) ^ (uintptr_t)(v >> 63)) & ~ICI_TAG_LITERAL);
        }
        else if (jl_object_in_image(e)) {
            size_t blob = external_blob_index(e);
            image_metadata_t *meta = (image_metadata_t*)image_tree.ranges[blob].data;
            uintptr_t off = ((uintptr_t)e - (uintptr_t)meta->base) / SYS_EXTERNAL_LINK_UNIT;
            assert(off < ((uintptr_t)1 << DEPS_IDX_OFFSET));
            words[i] = ICI_TAG_REF | (((uintptr_t)blob_to_depsidx[blob] + 1) << DEPS_IDX_OFFSET) | off;
        }
        else {
            jl_queue_for_serialization_(s, e, 1, 0);
            if (needs_uniquing(e, s->query_cache)) {
                // the image copy is a stub replaced by its canonical object at
                // load: emit this word as an ordinary relocated pointer slot so
                // the uniquing pass rewrites it (recognized at write time by
                // its clear top two bits; heap pointers never set them)
                words[i] = (uintptr_t)e;
            }
            else {
                // serialized with this image: patch after layout
                words[i] = 0;
                if (!ici_fixups_init) {
                    arraylist_new(&ici_fixups, 0);
                    ici_fixups_init = 1;
                }
                arraylist_push(&ici_fixups, (void*)ici);
                arraylist_push(&ici_fixups, (void*)i);
                arraylist_push(&ici_fixups, (void*)e);
            }
        }
    }
    return (jl_value_t*)ici;
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
    if (fptr == NULL)
        return 0;
    void *val = ptrhash_get(&fptr_to_id, fptr);
    if (val == HT_NOTFOUND)
        return 0;
    return (uintptr_t)val;
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
#define jl_queue_for_serialization(s, v) jl_queue_for_serialization_((s), (jl_value_t*)(v), 1, 0)
static void jl_queue_for_serialization_(jl_serializer_state *s, jl_value_t *v, int recursive, int immediate) JL_GC_DISABLED;

static void jl_queue_module_for_serialization(jl_serializer_state *s, jl_module_t *m) JL_GC_DISABLED
{
    jl_queue_for_serialization(s, m->name);
    jl_queue_for_serialization(s, m->parent);
    if (!jl_options.strip_metadata)
        jl_queue_for_serialization(s, m->file);
    jl_queue_for_serialization(s, jl_atomic_load_relaxed(&m->bindingkeyset));
    if (jl_options.trim) {
        jl_queue_for_serialization_(s, (jl_value_t*)jl_atomic_load_relaxed(&m->bindings), 0, 1);
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
        record_field_change((jl_value_t**)&m->usings_backedges, jl_nothing);
        record_field_change((jl_value_t**)&m->scanned_methods, jl_nothing);
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
// Most objects use preorder traversal. But things that need uniquing require postorder:
// you want to handle uniquing of `Dict{String,Float64}` before you tackle `Vector{Dict{String,Float64}}`.
// Uniquing is done in `serialization_order`, so the very first mention of such an object must
// be the "source" rather than merely a cross-reference.
static void jl_insert_into_serialization_queue_impl(jl_serializer_state *s, jl_value_t *v, int recursive, int immediate) JL_GC_DISABLED;

// --- relocation statistics support (JULIA_RELOC_STATS=<dir>) ---
// Discovery-parent spanning tree over the serialized object graph: children
// are attributed to the first object whose field walk enqueued them.
static int reloc_stats_wanted(void)
{
    static int enabled = -1;
    if (enabled == -1)
        enabled = getenv("JULIA_RELOC_STATS") != NULL;
    return enabled;
}
static htable_t reloc_parent_map;
static int reloc_parent_map_init = 0;
static jl_value_t *reloc_parent_ctx = NULL;
// named sentinels for the top-level arrays ((void*)2 ... (void*)9)
static const char *reloc_root_names[8] = { "root:worklist", "root:init_order", "root:extext_methods",
                                           "root:new_ext_cis", "root:backedge_log", "root:method_roots",
                                           "root:sig_tns", "root:other2" };
static void reloc_tag_root(jl_value_t *v, int which)
{
    if (!reloc_stats_wanted() || v == NULL || (jl_value_t*)v == jl_nothing)
        return;
    if (!reloc_parent_map_init) {
        htable_new(&reloc_parent_map, 1024);
        reloc_parent_map_init = 1;
    }
    ptrhash_put(&reloc_parent_map, (void*)v, (void*)(uintptr_t)(2 + which));
}

static void jl_insert_into_serialization_queue(jl_serializer_state *s, jl_value_t *v, int recursive, int immediate) JL_GC_DISABLED
{
    jl_value_t *saved = reloc_parent_ctx;
    reloc_parent_ctx = v;
    jl_insert_into_serialization_queue_impl(s, v, recursive, immediate);
    reloc_parent_ctx = saved;
}

static void jl_insert_into_serialization_queue_impl(jl_serializer_state *s, jl_value_t *v, int recursive, int immediate) JL_GC_DISABLED
{
    jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
    jl_queue_for_serialization_(s, (jl_value_t*)t, 1, immediate);
    const jl_datatype_layout_t *layout = t->layout;

    if (!recursive)
        goto done_fields;

    if (s->incremental && jl_is_datatype(v) && immediate) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        // ensure all type parameters are recached
        jl_queue_for_serialization_(s, (jl_value_t*)dt->parameters, 1, 1);
        if (jl_is_datatype_singleton(dt) && needs_uniquing(dt->instance, s->query_cache)) {
            assert(jl_needs_serialization(s, dt->instance)); // should be true, since we visited dt
            // do not visit dt->instance for our template object as it leads to unwanted cycles here
            // (it may get serialized from elsewhere though)
            record_field_change(&dt->instance, jl_nothing);
        }
        goto done_fields; // for now
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
                record_field_change((jl_value_t**)&mi->cache, NULL);
            }
            else {
                assert(!needs_recaching(v, s->query_cache));
            }
            // Any back-edges will be re-validated and added by staticdata.jl, so
            // drop them from the image here
            record_field_change((jl_value_t**)&mi->backedges, NULL);
            // n.b. opaque closures cannot be inspected and relied upon like a
            // normal method since they can get improperly introduced by generated
            // functions, so if they appeared at all, we will probably serialize
            // them wrong and segfault. The jl_code_for_staged function should
            // prevent this from happening, so we do not need to detect that user
            // error now.
        }
        // don't recurse into all backedges memory (yet)
        jl_value_t *backedges = get_replaceable_field((jl_value_t**)&mi->backedges, 1);
        if (backedges) {
            assert(!jl_options.trim && !jl_options.strip_ir);
            jl_queue_for_serialization_(s, (jl_value_t*)((jl_array_t*)backedges)->ref.mem, 0, 1);
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
            record_field_change((jl_value_t**)&b->backedges, NULL);
        }
        else {
            // don't recurse into all backedges memory (yet)
            jl_value_t *backedges = get_replaceable_field((jl_value_t**)&b->backedges, 1);
            if (backedges) {
                jl_queue_for_serialization_(s, (jl_value_t*)((jl_array_t*)backedges)->ref.mem, 0, 1);
                for (size_t i = 0, n = jl_array_nrows(backedges); i < n; i++) {
                    jl_value_t *b = jl_array_ptr_ref(backedges, i);
                    if (!jl_is_code_instance(b) && !jl_is_method_instance(b) && !jl_is_method(b)) // otherwise usually a Binding?
                        jl_queue_for_serialization(s, b);
                }
            }
        }
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
        jl_queue_for_serialization_(s, (jl_value_t*)jl_atomic_load_relaxed(&tn->cache), 0, 1);
        jl_queue_for_serialization_(s, (jl_value_t*)jl_atomic_load_relaxed(&tn->linearcache), 0, 1);
        if (s->incremental) {
            assert(!jl_object_in_image((jl_value_t*)tn->module));
            assert(!jl_object_in_image((jl_value_t*)tn->wrapper));
        }
    }
    if (jl_is_mtable(v)) {
        jl_methtable_t *mt = (jl_methtable_t*)v;
        // Any back-edges will be re-validated and added by staticdata.jl, so
        // drop them from the image here
        if (s->incremental || jl_options.trim || jl_options.strip_ir) {
            record_field_change((jl_value_t**)&mt->backedges, jl_an_empty_memory_any);
        }
        else {
            // don't recurse into all backedges memory (yet)
            jl_value_t *allbackedges = get_replaceable_field((jl_value_t**)&mt->backedges, 1);
            jl_queue_for_serialization_(s, allbackedges, 0, 1);
            for (size_t i = 0, n = ((jl_genericmemory_t*)allbackedges)->length; i < n; i += 2) {
                jl_value_t *tn = jl_genericmemory_ptr_ref(allbackedges, i);
                jl_queue_for_serialization(s, tn);
                jl_value_t *backedges = jl_genericmemory_ptr_ref(allbackedges, i + 1);
                if (backedges && backedges != jl_nothing) {
                    jl_queue_for_serialization_(s, (jl_value_t*)((jl_array_t*)backedges)->ref.mem, 0, 1);
                    jl_queue_for_serialization(s, backedges);
                    for (size_t i = 0, n = jl_array_nrows(backedges); i < n; i += 2) {
                        jl_value_t *t = jl_array_ptr_ref(backedges, i);
                        assert(!jl_is_code_instance(t));
                        jl_queue_for_serialization(s, t);
                    }
                }
            }
        }
    }
    if (s->incremental && jl_is_method(v))
        edge_sig_note(((jl_method_t*)v)->sig);
    if (jl_typetagis(v, jl_debuginfo_type)) {
        // intern the inlinee list and the def/linetable/codelocs fields
        // (rank order must match JL_ICI_DI_* in julia_internal.h)
        jl_debuginfo_t *di = (jl_debuginfo_t*)v;
        jl_svec_t *diedges = (jl_svec_t*)get_replaceable_field((jl_value_t**)&di->edges, 1);
        if (diedges != NULL && jl_is_svec(diedges) &&
            ptrhash_get(&field_replace, (void*)&di->edges) == HT_NOTFOUND) {
            jl_value_t **faddrs[3] = {
                (jl_value_t**)&di->def,
                (jl_value_t**)&di->linetable,
                (jl_value_t**)&di->codelocs,
            };
            jl_value_t *fv[3];
            uintptr_t fw[3];
            int fc[3];
            size_t nfw = 0;
            uintptr_t gate = ici_field_gate("JULIA_ICI_DIMASK", ~(uintptr_t)0);
            // method-source and toplevel DebugInfos (def is not a
            // MethodInstance) are reachable via raw Method.debuginfo reads
            // that no conversion chokepoint covers: keep their fields plain
            jl_value_t *didef = get_replaceable_field(&di->def, 1);
            if (didef == NULL || !jl_is_method_instance(didef))
                gate = 0;
            // only intern fields into containers the edge list needs anyway:
            // minting ~300k new containers for empty-edge leaves shifts the
            // whole data-stream layout for a marginal record win
            if (jl_svec_len(diedges) == 0)
                gate = 0;
            for (int r = 0; r < 3; r++) {
                fv[r] = get_replaceable_field(faddrs[r], 1);
                fw[r] = 0;
                fc[r] = (fv[r] == NULL || !(gate & ((uintptr_t)1 << r))) ? 0 : ici_classify_value(s, fv[r], &fw[r]);
                if (fc[r])
                    nfw++;
            }
            jl_value_t *ici = ici_try_build(s, diedges, nfw);
            if (ici != NULL) {
                jl_interned_code_instance_t *icid = (jl_interned_code_instance_t*)ici;
                record_field_change((jl_value_t**)&di->edges, ici);
                uintptr_t *words = ici_words(icid);
                size_t wi = icid->nedges;
                for (int r = 0; r < 3; r++) {
                    if (!fc[r])
                        continue;
                    icid->fieldmask |= ((uintptr_t)1 << r);
                    if (fc[r] == 1) {
                        words[wi] = fw[r];
                    }
                    else {
                        jl_queue_for_serialization_(s, fv[r], 1, 0);
                        words[wi] = 0;
                        if (!ici_fixups_init) {
                            arraylist_new(&ici_fixups, 0);
                            ici_fixups_init = 1;
                        }
                        arraylist_push(&ici_fixups, (void*)icid);
                        arraylist_push(&ici_fixups, (void*)wi);
                        arraylist_push(&ici_fixups, (void*)fv[r]);
                    }
                    record_field_change(faddrs[r], NULL);
                    wi++;
                }
            }
        }
    }
    if (jl_is_code_instance(v)) {
        jl_code_instance_t *ci = (jl_code_instance_t*)v;
        jl_method_instance_t *mi = jl_get_ci_mi(ci);
        if (s->incremental) {
            // make sure we don't serialize other reachable cache entries of foreign methods
            // Should this now be:
            // if (ci !in ci->defs->cache)
            //     record_field_change((jl_value_t**)&ci->next, NULL);
            // Why are we checking that the method/module this originates from is in_image?
            // and then disconnect this CI?
            if (jl_object_in_image((jl_value_t*)mi->def.value)) {
                // TODO: if (ci in ci->defs->cache)
                record_field_change((jl_value_t**)&ci->next, NULL);
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
                if (may_discard_trees && s->incremental && native_functions &&
                    jl_options.outputo != NULL && ci->owner == jl_nothing && def->source != NULL &&
                    jl_is_code_info(inferred) && jl_ir_inlining_cost(inferred) == UINT16_MAX) {
                    // Backstop: CodeInstance cached by a task racing the end of
                    // include phase can escape jl_finalize_precompile_inferred.
                    // Mirror the def->source guard below so optimized opaque
                    // closures (whose IR can't be reconstructed) are preserved.
                    record_field_change((jl_value_t**)&ci->inferred, jl_nothing);
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
                         (ci->owner == jl_nothing) && // don't delete code for external interpreters
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
                    record_field_change((jl_value_t**)&ci->inferred, inferred);
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
        // Drop DebugInfo for CodeInstances that retain neither inferred IR
        // nor native code in this image: with nothing to symbolize and
        // nothing to splice during inlining, the debug tree (and its
        // codelocs/linetable strings) is unreachable dead weight. Gated for
        // measurement via JULIA_KEEP_ALL_DEBUGINFO=1.
        if (s->incremental) {
            static int keepdi = -1;
            if (keepdi == -1) {
                char *e = getenv("JULIA_KEEP_ALL_DEBUGINFO");
                keepdi = e != NULL && strcmp(e, "1") == 0;
            }
            jl_value_t *inf_now = get_replaceable_field((jl_value_t**)&ci->inferred, 1);
            if (!keepdi &&
                (inf_now == NULL || inf_now == jl_nothing || jl_is_uint8(inf_now)) &&
                jl_atomic_load_relaxed(&ci->debuginfo) != NULL) {
                int32_t invokeptr_id = 0, specfptr_id = 0;
                if (native_functions)
                    jl_get_function_id(native_functions, ci, &invokeptr_id, &specfptr_id);
                if (invokeptr_id == 0 && specfptr_id == 0)
                    record_field_change((jl_value_t**)&ci->debuginfo, NULL);
            }
        }
        // Intern the edge list and (where encodable) the object fields into a
        // relocation-free container. Runs after every record_field_change
        // above so field reads see the values that will actually serialize.
        jl_svec_t *ciedges = (jl_svec_t*)get_replaceable_field((jl_value_t**)&ci->edges, 1);
        if (s->incremental && ciedges != NULL && (jl_value_t*)ciedges != jl_nothing && jl_is_svec(ciedges))
            collect_edge_group_sigs(ciedges);
        if (ciedges != NULL && (jl_value_t*)ciedges != jl_nothing && jl_is_svec(ciedges) &&
            ptrhash_get(&field_replace, (void*)&ci->edges) == HT_NOTFOUND) {
            // rank order must match JL_ICI_CI_* in julia_internal.h
            jl_value_t **faddrs[8] = {
                (jl_value_t**)&ci->owner,
                (jl_value_t**)&ci->next,
                (jl_value_t**)&ci->rettype,
                (jl_value_t**)&ci->exctype,
                (jl_value_t**)&ci->rettype_const,
                (jl_value_t**)&ci->inferred,
                (jl_value_t**)&ci->debuginfo,
                (jl_value_t**)&ci->analysis_results,
            };
            jl_value_t *fv[8];
            uintptr_t fw[8];
            int fc[8];
            size_t nfw = 0;
            uintptr_t gate = ici_field_gate("JULIA_ICI_CIMASK", ~(uintptr_t)0);
            for (int r = 0; r < 8; r++) {
                fv[r] = get_replaceable_field(faddrs[r], 1);
                fw[r] = 0;
                // `next` is runtime-mutable chain state: cache insertion writes it
                // blindly and NULL legitimately means end-of-chain, so a lazy image
                // write-back can corrupt (even cycle) live mi->cache chains. It must
                // always serialize as an ordinary pointer field.
                fc[r] = (r == JL_ICI_CI_NEXT || fv[r] == NULL || !(gate & ((uintptr_t)1 << r))) ? 0 : ici_classify_value(s, fv[r], &fw[r]);
                if (fc[r])
                    nfw++;
            }
            jl_value_t *ici = ici_try_build(s, ciedges, nfw);
            if (ici != NULL) {
                jl_interned_code_instance_t *icid = (jl_interned_code_instance_t*)ici;
                record_field_change((jl_value_t**)&ci->edges, ici);
                // def rides in its dedicated slot (jl_get_ci_mi rematerializes)
                jl_value_t *def = (gate & ((uintptr_t)1 << 8)) ? ci->def : NULL;
                if (def != NULL) {
                    uintptr_t dw = 0;
                    int dc = ici_classify_value(s, def, &dw);
                    if (dc == 1) {
                        icid->defword = dw;
                        record_field_change((jl_value_t**)&ci->def, NULL);
                    }
                    else if (dc == 2) {
                        jl_queue_for_serialization_(s, def, 1, 0);
                        if (!ici_fixups_init) {
                            arraylist_new(&ici_fixups, 0);
                            ici_fixups_init = 1;
                        }
                        arraylist_push(&ici_fixups, (void*)icid);
                        arraylist_push(&ici_fixups, (void*)ICI_DEFWORD_IDX);
                        arraylist_push(&ici_fixups, (void*)def);
                        record_field_change((jl_value_t**)&ci->def, NULL);
                    }
                }
                uintptr_t *words = ici_words(icid);
                size_t wi = icid->nedges;
                for (int r = 0; r < 8; r++) {
                    if (!fc[r])
                        continue;
                    icid->fieldmask |= ((uintptr_t)1 << r);
                    if (fc[r] == 1) {
                        words[wi] = fw[r];
                    }
                    else {
                        jl_queue_for_serialization_(s, fv[r], 1, 0); // field no longer references it
                        words[wi] = 0;
                        if (!ici_fixups_init) {
                            arraylist_new(&ici_fixups, 0);
                            ici_fixups_init = 1;
                        }
                        arraylist_push(&ici_fixups, (void*)icid);
                        arraylist_push(&ici_fixups, (void*)wi);
                        arraylist_push(&ici_fixups, (void*)fv[r]);
                    }
                    record_field_change(faddrs[r], NULL);
                    wi++;
                }
            }
        }
    }

    if (immediate) // must be things that can be recursively handled, and valid as type parameters
        assert(jl_is_immutable(t) || jl_is_typevar(v) || jl_is_symbol(v) || jl_is_svec(v));

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
        jl_value_t *mem = get_replaceable_field((jl_value_t**)&ar->ref.mem, 1);
        jl_queue_for_serialization_(s, mem, 1, immediate);
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
                jl_queue_for_serialization_(s, fld, 1, immediate);
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
                    jl_queue_for_serialization_(s, fld, 1, immediate);
                }
                data += elsz;
            }
        }
    }
    else if (jl_is_module(v)) {
        jl_queue_module_for_serialization(s, (jl_module_t*)v);
    }
    else if (layout->nfields > 0) {
        if (jl_options.trim) {
            if (jl_is_method(v)) {
                jl_method_t *m = (jl_method_t *)v;
                if (jl_is_svec(jl_atomic_load_relaxed(&m->specializations)))
                    jl_queue_for_serialization_(s, (jl_value_t*)jl_atomic_load_relaxed(&m->specializations), 0, 1);
            }
            else if (jl_is_mtable(v)) {
                jl_methtable_t *mt = (jl_methtable_t*)v;
                jl_methtable_t *newmt = (jl_methtable_t*)ptrhash_get(&new_methtables, mt);
                if (newmt != HT_NOTFOUND)
                    record_field_change((jl_value_t **)&mt->defs, (jl_value_t*)jl_atomic_load_relaxed(&newmt->defs));
                else
                    record_field_change((jl_value_t **)&mt->defs, jl_nothing);
            }
            else if (jl_is_mcache(v)) {
                jl_methcache_t *mc = (jl_methcache_t*)v;
                jl_value_t *cache = jl_atomic_load_relaxed(&mc->cache);
                if (!jl_typetagis(cache, jl_typemap_entry_type) || ((jl_typemap_entry_t*)cache)->sig != jl_tuple_type) { // aka Builtins (maybe sometimes OpaqueClosure too)
                    record_field_change((jl_value_t **)&mc->cache, jl_nothing);
                }
                record_field_change((jl_value_t **)&mc->leafcache, jl_an_empty_memory_any);
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
            jl_queue_for_serialization_(s, fld, 1, immediate);
        }
    }

done_fields: ;

    // We've encountered an item we need to cache
    void **bp = ptrhash_bp(&serialization_order, v);
    assert(*bp == (void*)(uintptr_t)-2);
    arraylist_push(&serialization_queue, (void*) v);
    size_t idx = serialization_queue.len - 1;
    assert(serialization_queue.len < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "too many items to serialize");
    *bp = to_seroder_entry(idx);

    // DataType is very unusual, in that some of the fields need to be pre-order, and some
    // (notably super) must not be (even if `jl_queue_for_serialization_` would otherwise
    // try to promote itself to be immediate)
    if (s->incremental && jl_is_datatype(v) && immediate && recursive) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        // do not handle the super field now: the supertype's parameters can reach back to
        // objects that are still on the recursion stack (e.g. `struct Baz{T} <: Bar{Foo{Baz{T}}} end`),
        // which would order the supertype before its own parameters in the queue. Defer it
        // until the recursion unwinds; any forward reference this creates in the image is
        // handled at load time by the uniquing_super/delay_list machinery.
        if (jl_needs_serialization(s, (jl_value_t*)dt->super))
            arraylist_push(&deferred_supers, (void*)dt->super);
        immediate = 0;
        char *data = (char*)jl_data_ptr(v);
        size_t i, np = layout->npointers;
        for (i = 0; i < np; i++) {
            uint32_t ptr = jl_ptr_offset(t, i);
            if (ptr * sizeof(jl_value_t*) == offsetof(jl_datatype_t, super))
                continue; // skip the super field, since it might not be quite validly ordered
            int mutabl = 1;
            jl_value_t *fld = get_replaceable_field(&((jl_value_t**)data)[ptr], mutabl);
            jl_queue_for_serialization_(s, fld, 1, immediate);
        }
    }
}


static void jl_queue_for_serialization_(jl_serializer_state *s, jl_value_t *v, int recursive, int immediate) JL_GC_DISABLED
{
    if (v == NULL)
        return;
    if (jl_is_svec(v))
        v = svec_dedup_canonical(s, v);
    if (!jl_needs_serialization(s, v))
        return;

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

    // Items that require postorder traversal must visit their children prior to insertion into
    // the worklist/serialization_order (and also before their first use)
    if (s->incremental && !immediate) {
        if (jl_is_datatype(v) && needs_uniquing(v, s->query_cache))
            immediate = 1;
        if (jl_is_datatype_singleton((jl_datatype_t*)t) && needs_uniquing(v, s->query_cache))
            immediate = 1;
    }

    void **bp = ptrhash_bp(&serialization_order, v);
    assert(!immediate || *bp != (void*)(uintptr_t)-2);
    if (*bp == HT_NOTFOUND) {
        *bp = (void*)(uintptr_t)-1; // now enqueued
        if (reloc_stats_wanted() && reloc_parent_ctx != NULL) {
            if (!reloc_parent_map_init) {
                htable_new(&reloc_parent_map, 1024);
                reloc_parent_map_init = 1;
            }
            ptrhash_put(&reloc_parent_map, (void*)v, (void*)reloc_parent_ctx);
        }
    }
    else if (!s->incremental || !immediate || !recursive || *bp != (void*)(uintptr_t)-1)
        return;

    if (immediate) {
        *bp = (void*)(uintptr_t)-2; // now immediate
        jl_insert_into_serialization_queue(s, v, recursive, immediate);
    }
    else {
        arraylist_push(&object_worklist, (void*)v);
    }
}

// Do a pre-order traversal of the to-serialize worklist, in the identical order
// to the calls to jl_queue_for_serialization would occur in a purely recursive
// implementation, but without potentially running out of stack.
static void jl_serialize_reachable(jl_serializer_state *s) JL_GC_DISABLED
{
    size_t i, prevlen = 0;
    while (1) {
        // handle deferred super fields now: the recursion stack is unwound here, so
        // everything reachable through their parameters is already validly ordered
        while (deferred_supers.len) {
            jl_value_t *super = (jl_value_t*)deferred_supers.items[--deferred_supers.len];
            jl_queue_for_serialization_(s, super, 1, 1);
        }
        if (object_worklist.len == 0)
            break;
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
static uintptr_t add_external_linkage(jl_serializer_state *s, jl_value_t *v, jl_array_t *link_ids) JL_GC_DISABLED
{
    image_metadata_t *meta = external_blob_metadata(v);
    if (meta) {
        size_t i = meta->idx;
        // We found the sysimg/pkg that this item links against
        // Compute the relocation code
        size_t offset = (uintptr_t)v - meta->base;
        assert((offset % SYS_EXTERNAL_LINK_UNIT) == 0);
        offset /= SYS_EXTERNAL_LINK_UNIT;
        assert(n_linkage_blobs() == jl_array_nrows(s->buildid_depmods_idxs));
        size_t depsidx = jl_array_data(s->buildid_depmods_idxs, uint32_t)[i]; // map from build_id_idx -> deps_idx
        assert(depsidx < INT32_MAX);
        if (depsidx < ((uintptr_t)1 << (RELOC_TAG_OFFSET - DEPS_IDX_OFFSET)) && offset < ((uintptr_t)1 << DEPS_IDX_OFFSET))
            // if it fits in a SysimageLinkage type, use that representation
            return ((uintptr_t)SysimageLinkage << RELOC_TAG_OFFSET) + ((uintptr_t)depsidx << DEPS_IDX_OFFSET) + offset;
        // otherwise, we store the image key in `link_ids`
        assert(link_ids && jl_is_array(link_ids));
        jl_array_grow_end(link_ids, 1);
        uint32_t *link_id_data  = jl_array_data(link_ids, uint32_t);  // wait until after the `grow`
        link_id_data[jl_array_nrows(link_ids) - 1] = depsidx;
        assert(offset < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "offset to external image too large");
        return ((uintptr_t)ExternalLinkage << RELOC_TAG_OFFSET) + offset;
    }
    return 0;
}

// Return the integer `id` for `v`. Generically this is looked up in `serialization_order`,
// but symbols, small integers, and a couple of special items (`nothing` and the root Task)
// have special handling.
#define backref_id(s, v, link_ids) _backref_id(s, (jl_value_t*)(v), link_ids)
static uintptr_t _backref_id(jl_serializer_state *s, jl_value_t *v, jl_array_t *link_ids) JL_GC_DISABLED JL_NOTSAFEPOINT
{
    assert(v != NULL && "cannot get backref to NULL object");
    if (jl_is_svec(v))
        v = svec_dedup_canonical(s, v); // all references resolve to the interned representative
    if (jl_is_symbol(v)) {
        void **pidx = ptrhash_bp(&symbol_table, v);
        void *idx = *pidx;
        if (idx == HT_NOTFOUND) {
            if (!depsym_map_init)
                depsym_map_build(s);
            void *shared = ptrhash_get(&depsym_map, v);
            if (shared != HT_NOTFOUND) {
                // present in a dependency image: reference it as (dep, index)
                // (record flagged by the high bit of the length field)
                write_uint32(s->symbols, 0x80000000u | (uint32_t)(((uintptr_t)shared >> 40) - 1));
                write_uint32(s->symbols, (uint32_t)(uintptr_t)shared);
            }
            else {
                size_t l = strlen(jl_symbol_name((jl_sym_t*)v));
                write_uint32(s->symbols, l);
                ios_write(s->symbols, jl_symbol_name((jl_sym_t*)v), l + 1);
            }
            size_t offset = ++nsym_tag;
            assert(offset < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "too many symbols");
            idx = to_seroder_entry(offset - 1);
            *pidx = idx;
        }
        return ((uintptr_t)SymbolRef << RELOC_TAG_OFFSET) + from_seroder_entry(idx);
    }
    else if (v == (jl_value_t*)s->ptls->root_task) {
        return (uintptr_t)TagRef << RELOC_TAG_OFFSET;
    }
    else if (v == jl_nothing) {
        return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + 1;
    }
    else if (jl_typetagis(v, jl_int64_tag << 4)) {
        int64_t i64 = *(int64_t*)v + NBOX_C / 2;
        if ((uint64_t)i64 < NBOX_C)
            return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + i64 + 2;
    }
    else if (jl_typetagis(v, jl_int32_tag << 4)) {
        int32_t i32 = *(int32_t*)v + NBOX_C / 2;
        if ((uint32_t)i32 < NBOX_C)
            return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + i32 + 2 + NBOX_C;
    }
    else if (jl_typetagis(v, jl_uint8_tag << 4)) {
        uint8_t u8 = *(uint8_t*)v;
        return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + u8 + 2 + NBOX_C + NBOX_C;
    }
    if (s->incremental && jl_object_in_image(v)) {
        assert(link_ids);
        uintptr_t item = add_external_linkage(s, v, link_ids);
        assert(item && "no external linkage identified");
        return item;
    }
    void *idx = ptrhash_get(&serialization_order, v);
    if (idx == HT_NOTFOUND) {
        jl_(jl_typeof(v));
        jl_(v);
    }
    assert(idx != HT_NOTFOUND && "object missed during jl_queue_for_serialization pass");
    assert(idx != (void*)(uintptr_t)-1 && "object missed during jl_insert_into_serialization_queue pass");
    assert(idx != (void*)(uintptr_t)-2 && "object missed during jl_insert_into_serialization_queue pass");
    return ((uintptr_t)DataRef << RELOC_TAG_OFFSET) + from_seroder_entry(idx);
}


static void record_uniquing(jl_serializer_state *s, jl_value_t *fld, uintptr_t offset) JL_NOTSAFEPOINT
{
    if (s->incremental && jl_needs_serialization(s, fld) && needs_uniquing(fld, s->query_cache)) {
        if (jl_is_datatype(fld) || jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(fld)))
            arraylist_push(&s->uniquing_types, (void*)(uintptr_t)offset);
        else if (jl_is_method_instance(fld) || jl_is_binding(fld))
            arraylist_push(&s->uniquing_objs, (void*)(uintptr_t)offset);
        else
            assert(0 && "unknown object type with needs_uniquing set");
    }
}

// --- relocation statistics (JULIA_RELOC_STATS=<dir>): classify emitted
// relocations by the (grandparent, parent, object) typename chain of the
// object being serialized — parents from the discovery spanning tree — and
// by the reference kind. Measurement aid only.
typedef struct {
    void *tn[3]; // grandparent, parent, object typenames ((void*)1 = <root>)
    size_t counts[8];
} reloc_triple_ent_t;
static reloc_triple_ent_t *reloc_triples = NULL;
static size_t reloc_triples_sz = 0;
static size_t reloc_triples_n = 0;
static jl_value_t *reloc_stats_cur = NULL;

static void *reloc_stat_tn(jl_value_t *v) JL_NOTSAFEPOINT
{
    if (v == NULL)
        return (void*)1;
    return (void*)((jl_datatype_t*)jl_typeof(v))->name;
}

static reloc_triple_ent_t *reloc_triple_slot(void *gp, void *pt, void *ob)
{
    if (reloc_triples == NULL) {
        reloc_triples_sz = 1 << 12;
        reloc_triples = (reloc_triple_ent_t*)calloc_s(reloc_triples_sz * sizeof(reloc_triple_ent_t));
    }
    while (1) {
        size_t mask = reloc_triples_sz - 1;
        size_t idx = ((((uintptr_t)gp * 0x9E3779B97F4A7C15ULL) ^
                      ((uintptr_t)pt * 0xC2B2AE3D27D4EB4FULL) ^
                      ((uintptr_t)ob * 0x165667B19E3779F9ULL)) >> 32) & mask;
        while (1) {
            reloc_triple_ent_t *e = &reloc_triples[idx];
            if (e->tn[2] == ob && e->tn[1] == pt && e->tn[0] == gp)
                return e;
            if (e->tn[2] == NULL) {
                if ((reloc_triples_n + 1) * 4 > reloc_triples_sz * 3)
                    break; // grow
                reloc_triples_n++;
                e->tn[0] = gp;
                e->tn[1] = pt;
                e->tn[2] = ob;
                return e;
            }
            idx = (idx + 1) & mask;
        }
        reloc_triple_ent_t *old = reloc_triples;
        size_t oldsz = reloc_triples_sz;
        reloc_triples_sz *= 2;
        reloc_triples = (reloc_triple_ent_t*)calloc_s(reloc_triples_sz * sizeof(reloc_triple_ent_t));
        size_t n = reloc_triples_n;
        reloc_triples_n = 0;
        for (size_t i = 0; i < oldsz; i++) {
            if (old[i].tn[2] == NULL)
                continue;
            reloc_triple_ent_t *e = reloc_triple_slot(old[i].tn[0], old[i].tn[1], old[i].tn[2]);
            memcpy(e->counts, old[i].counts, sizeof(e->counts));
        }
        (void)n;
        free(old);
    }
}

static void record_reloc_stat(uintptr_t id, int isgctag) JL_NOTSAFEPOINT
{
    if (!reloc_stats_wanted())
        return;
    jl_value_t *obj = reloc_stats_cur;
    void *ptn = (void*)1, *gptn = (void*)1;
    if (obj != NULL && reloc_parent_map_init) {
        void *p = ptrhash_get(&reloc_parent_map, (void*)obj);
        if (p != HT_NOTFOUND) {
            if ((uintptr_t)p < 16) { // named root sentinel
                ptn = p;
            }
            else {
                ptn = reloc_stat_tn((jl_value_t*)p);
                void *gp = ptrhash_get(&reloc_parent_map, p);
                if (gp != HT_NOTFOUND)
                    gptn = (uintptr_t)gp < 16 ? gp : reloc_stat_tn((jl_value_t*)gp);
            }
        }
    }
    reloc_triple_ent_t *e = reloc_triple_slot(gptn, ptn,
                                              obj == NULL ? (void*)1 : reloc_stat_tn(obj));
    int kind;
    switch (id >> RELOC_TAG_OFFSET) {
    case DataRef:          kind = 0; break; // internal to this image
    case SysimageLinkage:  kind = 1; break; // out of image (sysimage or deps)
    case ExternalLinkage:  kind = 2; break;
    default:               kind = 3; break; // tags/symbols/functions/constdata
    }
    e->counts[(isgctag ? 4 : 0) + kind]++;
}

static const char *reloc_stat_tn_name(void *tn) JL_NOTSAFEPOINT
{
    if (tn == (void*)1)
        return "<root>";
    if ((uintptr_t)tn < 16)
        return reloc_root_names[(uintptr_t)tn - 2];
    return jl_symbol_name(((jl_typename_t*)tn)->name);
}

static void dump_reloc_stats(const char *pkgname)
{
    if (!reloc_stats_wanted() || reloc_triples == NULL)
        return;
    char path[1024];
    snprintf(path, sizeof(path), "%s/%s.%d.relocstats", getenv("JULIA_RELOC_STATS"),
             pkgname ? pkgname : "unknown", (int)getpid());
    FILE *f = fopen(path, "w");
    if (f == NULL)
        return;
    fprintf(f, "# gp>parent>obj reloc_internal reloc_sysimg reloc_external reloc_other gctag_internal gctag_sysimg gctag_external gctag_other\n");
    for (size_t i = 0; i < reloc_triples_sz; i++) {
        reloc_triple_ent_t *e = &reloc_triples[i];
        if (e->tn[2] == NULL)
            continue;
        fprintf(f, "%s>%s>%s %zu %zu %zu %zu %zu %zu %zu %zu\n",
                reloc_stat_tn_name(e->tn[0]), reloc_stat_tn_name(e->tn[1]), reloc_stat_tn_name(e->tn[2]),
                e->counts[0], e->counts[1], e->counts[2], e->counts[3],
                e->counts[4], e->counts[5], e->counts[6], e->counts[7]);
    }
    fclose(f);
}

// --- serialized-heap census (shares the JULIA_RELOC_STATS gate): per object,
// (parent typename > typename > subclass) -> {count, data bytes, const bytes}.
// Subclasses expose suspect payload states (dead CodeInstances, inferred
// kinds, string sizes) for deciding what should not be serialized at all.
typedef struct {
    void *ptn;
    void *tn;
    uint8_t sub;
    size_t count, dbytes, cbytes;
} heap_stat_ent_t;
static heap_stat_ent_t *heap_stats_tab = NULL;
static size_t heap_stats_sz = 0, heap_stats_n = 0;

static uint8_t heap_stat_subclass(jl_value_t *v, jl_datatype_t *t) JL_NOTSAFEPOINT
{
    if (t == jl_code_instance_type) {
        jl_code_instance_t *ci = (jl_code_instance_t*)v;
        size_t maxw = jl_atomic_load_relaxed(&ci->max_world);
        jl_value_t *inf = jl_atomic_load_relaxed(&ci->inferred);
        uint8_t world = maxw == ~(size_t)0 ? 0 : maxw == 1 ? 1 : maxw == 0 ? 2 : 3;
        uint8_t infk = inf == NULL ? 0 : inf == jl_nothing ? 1 :
                       jl_is_uint8(inf) ? 2 : jl_is_string(inf) ? 3 : 4;
        return 1 + world * 5 + infk; // 1..20
    }
    if (t == jl_method_instance_type) {
        jl_method_instance_t *mi = (jl_method_instance_t*)v;
        return jl_atomic_load_relaxed(&mi->cache) == NULL ? 21 : 22;
    }
    return 0;
}

static const char *heap_stat_subname(uint8_t sub) JL_NOTSAFEPOINT
{
    static const char *world[4] = { "valid", "sentinel", "dead", "partial" };
    static const char *infk[5] = { "noinf", "nothing", "cost", "string", "codeinfo" };
    static char buf[64];
    if (sub == 0)
        return "";
    if (sub >= 1 && sub <= 20) {
        snprintf(buf, sizeof(buf), ":%s:%s", world[(sub - 1) / 5], infk[(sub - 1) % 5]);
        return buf;
    }
    if (sub == 21)
        return ":nocache";
    if (sub == 22)
        return ":cached";
    return ":?";
}

static FILE *heap_chain_file = NULL;
static int heap_chain_samples = 0;

// walk the discovery spanning tree upward and print the ownership chain
static void heap_stat_chain(jl_value_t *v, jl_datatype_t *t, size_t bytes, const char *why) JL_NOTSAFEPOINT
{
    if (heap_chain_file == NULL) {
        char path[1024];
        snprintf(path, sizeof(path), "%s/%d.heapchains", getenv("JULIA_RELOC_STATS"), (int)getpid());
        heap_chain_file = fopen(path, "a");
        if (heap_chain_file == NULL)
            return;
    }
    fprintf(heap_chain_file, "%s %zu %s", why, bytes, jl_symbol_name(t->name->name));
    char names[200][80];
    int nn = 0;
    jl_value_t *cur = v;
    for (int hop = 0; hop < 200 && cur != NULL; hop++) {
        void *pv = reloc_parent_map_init ? ptrhash_get(&reloc_parent_map, (void*)cur) : HT_NOTFOUND;
        if (pv == HT_NOTFOUND)
            break;
        if ((uintptr_t)pv < 16) {
            snprintf(names[nn++], 80, "%s", reloc_stat_tn_name(pv));
            break;
        }
        jl_value_t *pobj = (jl_value_t*)pv;
        jl_datatype_t *pt = (jl_datatype_t*)jl_typeof(pobj);
        if (pt == jl_binding_type) {
            jl_binding_t *b = (jl_binding_t*)pobj;
            snprintf(names[nn++], 80, "Binding(%s.%s)",
                     jl_symbol_name(b->globalref->mod->name), jl_symbol_name(b->globalref->name));
        }
        else if (pt == jl_method_type) {
            jl_method_t *m = (jl_method_t*)pobj;
            snprintf(names[nn++], 80, "Method(%s)", jl_symbol_name(m->name));
        }
        else if (pt == jl_module_type) {
            snprintf(names[nn++], 80, "Module(%s)", jl_symbol_name(((jl_module_t*)pobj)->name));
        }
        else {
            snprintf(names[nn++], 80, "%s", jl_symbol_name(pt->name->name));
        }
        if (nn >= 200)
            break;
        cur = pobj;
    }
    for (int i = 0; i < nn; i++) {
        if (nn > 18 && i == 6) {
            fprintf(heap_chain_file, " <- ...(%d)...", nn - 16);
            i = nn - 10;
        }
        fprintf(heap_chain_file, " <- %s", names[i]);
    }
    fprintf(heap_chain_file, "\n");
}

static int heap_stat_suspect(jl_datatype_t *t) JL_NOTSAFEPOINT
{
    const char *n = jl_symbol_name(t->name->name);
    return strcmp(n, "Observable") == 0 || strcmp(n, "Input") == 0 ||
           strcmp(n, "Computed") == 0 || strcmp(n, "ComputeEdge") == 0 ||
           strcmp(n, "ObserverFunction") == 0 || strcmp(n, "Scene") == 0;
}

static void heap_stat_note(jl_value_t *v, jl_datatype_t *t, size_t dbytes, size_t cbytes) JL_NOTSAFEPOINT
{
    if (dbytes + cbytes > 65536)
        heap_stat_chain(v, t, dbytes + cbytes, "WHALE");
    else if (heap_chain_samples < 400 && heap_stat_suspect(t)) {
        heap_chain_samples++;
        heap_stat_chain(v, t, dbytes + cbytes, "SUSPECT");
    }
    void *tn = (void*)t->name;
    void *ptn = (void*)1;
    if (reloc_parent_map_init) {
        void *pv = ptrhash_get(&reloc_parent_map, (void*)v);
        if (pv != HT_NOTFOUND)
            ptn = ((uintptr_t)pv < 16) ? pv : reloc_stat_tn((jl_value_t*)pv);
    }
    uint8_t sub = heap_stat_subclass(v, t);
    if (heap_stats_tab == NULL) {
        heap_stats_sz = 1 << 12;
        heap_stats_tab = (heap_stat_ent_t*)calloc_s(heap_stats_sz * sizeof(heap_stat_ent_t));
    }
    while (1) {
        size_t mask = heap_stats_sz - 1;
        size_t idx = ((((uintptr_t)ptn * 0x9E3779B97F4A7C15ULL) ^
                      ((uintptr_t)tn * 0xC2B2AE3D27D4EB4FULL) ^ (sub * 0x165667B19E3779F9ULL)) >> 32) & mask;
        while (1) {
            heap_stat_ent_t *e = &heap_stats_tab[idx];
            if (e->tn == tn && e->ptn == ptn && e->sub == sub) {
                e->count++;
                e->dbytes += dbytes;
                e->cbytes += cbytes;
                return;
            }
            if (e->tn == NULL) {
                if ((heap_stats_n + 1) * 4 > heap_stats_sz * 3)
                    break; // grow
                heap_stats_n++;
                e->ptn = ptn;
                e->tn = tn;
                e->sub = sub;
                e->count = 1;
                e->dbytes = dbytes;
                e->cbytes = cbytes;
                return;
            }
            idx = (idx + 1) & mask;
        }
        heap_stat_ent_t *old = heap_stats_tab;
        size_t oldsz = heap_stats_sz;
        heap_stats_sz *= 2;
        heap_stats_tab = (heap_stat_ent_t*)calloc_s(heap_stats_sz * sizeof(heap_stat_ent_t));
        heap_stats_n = 0;
        for (size_t i = 0; i < oldsz; i++) {
            if (old[i].tn == NULL)
                continue;
            // re-insert
            void *optn = old[i].ptn, *otn = old[i].tn;
            uint8_t osub = old[i].sub;
            size_t idx2 = ((((uintptr_t)optn * 0x9E3779B97F4A7C15ULL) ^
                          ((uintptr_t)otn * 0xC2B2AE3D27D4EB4FULL) ^ (osub * 0x165667B19E3779F9ULL)) >> 32) & (heap_stats_sz - 1);
            while (heap_stats_tab[idx2].tn != NULL)
                idx2 = (idx2 + 1) & (heap_stats_sz - 1);
            heap_stats_tab[idx2] = old[i];
            heap_stats_n++;
        }
        free(old);
    }
}

static void dump_heap_stats(const char *pkgname)
{
    if (!reloc_stats_wanted() || heap_stats_tab == NULL)
        return;
    char path[1024];
    snprintf(path, sizeof(path), "%s/%s.%d.heapstats", getenv("JULIA_RELOC_STATS"),
             pkgname ? pkgname : "unknown", (int)getpid());
    FILE *f = fopen(path, "w");
    if (f == NULL)
        return;
    fprintf(f, "# parent>type[:sub] count data_bytes const_bytes\n");
    for (size_t i = 0; i < heap_stats_sz; i++) {
        heap_stat_ent_t *e = &heap_stats_tab[i];
        if (e->tn == NULL)
            continue;
        fprintf(f, "%s>%s%s %zu %zu %zu\n",
                reloc_stat_tn_name(e->ptn), reloc_stat_tn_name(e->tn),
                heap_stat_subname(e->sub), e->count, e->dbytes, e->cbytes);
    }
    fclose(f);
    if (heap_chain_file != NULL) {
        fclose(heap_chain_file);
        heap_chain_file = NULL;
    }
}

// --- SimpleVector de-duplication: egal svecs (type-parameter lists, field
// type lists, ...) are interned to one representative during serialization,
// so their element references are written once. Safe because svecs are
// immutable after construction and egal-compared everywhere identity would
// otherwise matter; typevar-containing lists never merge across distinct
// vars since typevars are egal by identity.
static htable_t svec_dedup_memo;   // svec -> canonical svec (identity-keyed)
static htable_t svec_dedup_byhash; // content hash -> first svec with it
static int svec_dedup_init = 0;

static jl_value_t *svec_dedup_canonical(jl_serializer_state *s, jl_value_t *v) JL_GC_DISABLED
{
    if (!s->incremental || !jl_is_svec(v))
        return v;
    size_t l = jl_svec_len(v);
    if (l == 0)
        return v;
    if (!svec_dedup_init) {
        htable_new(&svec_dedup_memo, 1 << 16);
        htable_new(&svec_dedup_byhash, 1 << 16);
        svec_dedup_init = 1;
    }
    void *memo = ptrhash_get(&svec_dedup_memo, v);
    if (memo != HT_NOTFOUND)
        return (jl_value_t*)memo;
    jl_value_t *canon = v;
    for (size_t i = 0; i < l; i++) {
        if (jl_svecref(v, i) == NULL) { // unset slot: leave alone
            ptrhash_put(&svec_dedup_memo, v, v);
            return v;
        }
    }
    uintptr_t hash = jl_object_id(v);
    void **bp = ptrhash_bp(&svec_dedup_byhash, (void*)(hash | 1)); // |1: never a valid key collision with HT_NOTFOUND semantics on 0
    if (*bp == HT_NOTFOUND)
        *bp = v;
    else if (jl_egal((jl_value_t*)*bp, v))
        canon = (jl_value_t*)*bp;
    // on hash collision without egality, keep v distinct (missed dedup only)
    ptrhash_put(&svec_dedup_memo, v, canon);
    return canon;
}

// Save blank space in stream `s` for a pointer `fld`, storing both location and target
// in `relocs_list`.
static void write_pointerfield(jl_serializer_state *s, jl_value_t *fld) JL_NOTSAFEPOINT
{
    if (fld != NULL && jl_is_svec(fld))
        fld = svec_dedup_canonical(s, fld);
    if (fld != NULL) {
        uintptr_t id = backref_id(s, fld, s->link_ids_relocs);
        arraylist_push(&s->relocs_list, (void*)(uintptr_t)ios_pos(s->s));
        arraylist_push(&s->relocs_list, (void*)id);
        record_reloc_stat(id, 0);
        record_uniquing(s, fld, ios_pos(s->s));
    }
    write_pointer(s->s);
}

// Save blank space in stream `s` for a pointer `fld`, storing both location and target
// in `gctags_list`.
static void write_gctaggedfield(jl_serializer_state *s, jl_datatype_t *ref) JL_NOTSAFEPOINT
{
    // jl_printf(JL_STDOUT, "gctaggedfield: position %p, value 0x%lx\n", (void*)(uintptr_t)ios_pos(s->s), ref);
    uintptr_t id = backref_id(s, ref, s->link_ids_gctags);
    arraylist_push(&s->gctags_list, (void*)(uintptr_t)ios_pos(s->s));
    arraylist_push(&s->gctags_list, (void*)id);
    record_reloc_stat(id, 1);
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
    {
        uintptr_t rid_ = (uintptr_t)backref_id(s, m->name, s->link_ids_relocs);
        arraylist_push(&s->relocs_list, (void*)rid_);
        record_reloc_stat(rid_, 0);
    }
    newm->parent = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, parent)));
    {
        uintptr_t rid_ = (uintptr_t)backref_id(s, m->parent, s->link_ids_relocs);
        arraylist_push(&s->relocs_list, (void*)rid_);
        record_reloc_stat(rid_, 0);
    }
    jl_atomic_store_relaxed(&newm->bindings, NULL);
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, bindings)));
    {
        uintptr_t rid_ = (uintptr_t)backref_id(s, jl_atomic_load_relaxed(&m->bindings), s->link_ids_relocs);
        arraylist_push(&s->relocs_list, (void*)rid_);
        record_reloc_stat(rid_, 0);
    }
    jl_atomic_store_relaxed(&newm->bindingkeyset, NULL);
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, bindingkeyset)));
    {
        uintptr_t rid_ = (uintptr_t)backref_id(s, jl_atomic_load_relaxed(&m->bindingkeyset), s->link_ids_relocs);
        arraylist_push(&s->relocs_list, (void*)rid_);
        record_reloc_stat(rid_, 0);
    }
    newm->file = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, file)));
    {
        uintptr_t rid_ = (uintptr_t)backref_id(s, jl_options.strip_metadata ? jl_empty_sym : m->file , s->link_ids_relocs);
        arraylist_push(&s->relocs_list, (void*)rid_);
        record_reloc_stat(rid_, 0);
    }
    if (jl_options.strip_metadata)
        newm->line = 0;
    newm->usings_backedges = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings_backedges)));
    {
        uintptr_t rid_ = (uintptr_t)backref_id(s, get_replaceable_field(&m->usings_backedges, 1), s->link_ids_relocs);
        arraylist_push(&s->relocs_list, (void*)rid_);
        record_reloc_stat(rid_, 0);
    }
    newm->scanned_methods = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, scanned_methods)));
    {
        uintptr_t rid_ = (uintptr_t)backref_id(s, get_replaceable_field(&m->scanned_methods, 1), s->link_ids_relocs);
        arraylist_push(&s->relocs_list, (void*)rid_);
        record_reloc_stat(rid_, 0);
    }

    // After reload, everything that has happened in this process happened semantically at
    // (for .incremental) or before jl_require_world, so reset this flag.
    jl_atomic_store_relaxed(&newm->export_set_changed_since_require_world, 0);

    // write out the usings list
    memset(&newm->usings._space, 0, sizeof(newm->usings._space));
    if (m->usings.items == &m->usings._space[0]) {
        newm->usings.items = &newm->usings._space[0];
        // Push these relocations here, to keep them in order. This pairs with the `newm->usings.items = ` below.
        arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings.items)));
        arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item));
        size_t i;
        for (i = 0; i < module_usings_length(m); i++) {
            struct _jl_module_using *newm_data = module_usings_getidx(newm, i);
            struct _jl_module_using *data = module_usings_getidx(m, i);
            // TODO: Remove dead entries
            newm_data->min_world = data->min_world;
            newm_data->max_world = data->max_world;
            newm_data->flags = data->flags;
            if (s->incremental) {
                if (data->max_world != ~(size_t)0)
                    newm_data->max_world = 0;
                newm_data->min_world = jl_require_world;
            }
            arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings._space[4*i])));
            {
        uintptr_t rid_ = (uintptr_t)backref_id(s, data->mod, s->link_ids_relocs);
        arraylist_push(&s->relocs_list, (void*)rid_);
        record_reloc_stat(rid_, 0);
    }
        }
        newm->usings.items = (void**)offsetof(jl_module_t, usings._space);
    }
    else {
        newm->usings.items = (void**)tot;
        arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings.items)));
        arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item));
        newm = NULL; // `write_*(s->s)` below may invalidate `newm`, so defensively set it to NULL
        size_t i;
        for (i = 0; i < module_usings_length(m); i++) {
            struct _jl_module_using *data = module_usings_getidx(m, i);
            write_pointerfield(s, (jl_value_t*)data->mod);
            if (s->incremental) {
                // TODO: Drop dead ones entirely?
                write_uint(s->s, jl_require_world);
                write_uint(s->s, data->max_world == ~(size_t)0 ? ~(size_t)0 : 1);
            } else {
                write_uint(s->s, data->min_world);
                write_uint(s->s, data->max_world);
            }
            write_uint(s->s, data->flags);
            static_assert(sizeof(struct _jl_module_using) == 4*sizeof(void*), "_jl_module_using mismatch");
            tot += sizeof(struct _jl_module_using);
        }
        for (; i < module_usings_max(m); i++) {
            write_pointer(s->s);
            write_uint(s->s, 0);
            write_uint(s->s, 0);
            write_uint(s->s, 0);
            tot += sizeof(struct _jl_module_using);
        }
    }
    assert(ios_pos(s->s) - reloc_offset == tot);
}

static void record_memoryref(jl_serializer_state *s, size_t reloc_offset, jl_genericmemoryref_t ref) {
    ios_t *f = s->s;
    // make some header modifications in-place
    jl_genericmemoryref_t *newref = (jl_genericmemoryref_t*)&f->buf[reloc_offset];
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(ref.mem))->layout;
    if (!layout->flags.arrayelem_isunion && layout->size != 0) {
        newref->ptr_or_offset = (void*)((char*)ref.ptr_or_offset - (char*)ref.mem->ptr); // relocation offset (bytes)
        arraylist_push(&s->memref_list, (void*)reloc_offset); // relocation location
        arraylist_push(&s->memref_list, NULL); // relocation target (ignored)
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

static void record_gvars(jl_serializer_state *s, arraylist_t *globals) JL_GC_DISABLED
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
        assert(jl_atomic_load_relaxed(&ci->flags) & JL_CI_FLAGS_FROM_IMAGE);
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
    jl_value_t *stat_prev = NULL;
    jl_datatype_t *stat_prev_t = NULL;
    size_t stat_d0 = 0, stat_c0 = 0;
    for (size_t item = 0; item < l; item++) {
        jl_value_t *v = (jl_value_t*)serialization_queue.items[item];           // the object
        reloc_stats_cur = v;
        if (reloc_stats_wanted()) {
            if (stat_prev != NULL)
                heap_stat_note(stat_prev, stat_prev_t, ios_pos(s->s) - stat_d0,
                               ios_pos(s->const_data) - stat_c0);
            stat_prev = v;
            stat_prev_t = (jl_datatype_t*)jl_typeof(v);
            stat_d0 = ios_pos(s->s);
            stat_c0 = ios_pos(s->const_data);
        }
        JL_GC_PROMISE_ROOTED(v);
        assert(!(s->incremental && jl_object_in_image(v)));
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        assert((!jl_is_datatype_singleton(t) || t->instance == v) && "detected singleton construction corruption");
        int mutabl = t->name->mutabl;
        ios_t *f = s->s;
        if (t->smalltag) {
            if (t->layout->npointers == 0 || t == jl_string_type) {
                if (jl_datatype_nfields(t) == 0 || mutabl == 0 || t == jl_string_type) {
                    f = s->const_data;
                }
            }
        }

        // realign stream to expected gc alignment (16 bytes) after tag
        uintptr_t skip_header_pos = ios_pos(f) + sizeof(jl_taggedvalue_t);
        uintptr_t object_id_expected = mutabl &&
                 t != jl_datatype_type &&
                 t != jl_typename_type &&
                 t != jl_string_type &&
                 t != jl_simplevector_type &&
                 t != jl_module_type;
        if (object_id_expected)
            skip_header_pos += sizeof(size_t);
        write_padding(f, LLT_ALIGN(skip_header_pos, 16) - skip_header_pos);

        // write header
        if (object_id_expected)
            write_uint(f, jl_object_id(v));
        if (s->incremental && jl_needs_serialization(s, (jl_value_t*)t) && needs_uniquing((jl_value_t*)t, s->query_cache))
            arraylist_push(&s->uniquing_types, (void*)(uintptr_t)(ios_pos(f)|1));
        if (f == s->const_data)
            write_uint(s->const_data, ((uintptr_t)t->smalltag << 4) | GC_OLD_MARKED | GC_IN_IMAGE);
        else if (t->smalltag) {
            // small-tagged types (core, never uniqued) need no gctag
            // relocation: the complete header is plain data
            assert(!(s->incremental && jl_needs_serialization(s, (jl_value_t*)t) && needs_uniquing((jl_value_t*)t, s->query_cache)));
            write_uint(f, ((uintptr_t)t->smalltag << 4) | GC_OLD_MARKED | GC_IN_IMAGE);
        }
        else
            write_gctaggedfield(s, t);
        size_t reloc_offset = ios_pos(f);
        assert(item < layout_table.len && layout_table.items[item] == NULL);
        layout_table.items[item] = (void*)(reloc_offset | (f == s->const_data)); // store the inverse mapping of `serialization_order` (`id` => object-as-streampos)

        if (s->incremental) {
            if (needs_uniquing(v, s->query_cache)) {
                if (jl_is_binding(v)) {
                    jl_binding_t *b = (jl_binding_t*)v;
                    write_pointerfield(s, (jl_value_t*)b->globalref->mod);
                    write_pointerfield(s, (jl_value_t*)b->globalref->name);
                    continue;
                }
                else if (jl_is_method_instance(v)) {
                    assert(f == s->s);
                    jl_method_instance_t *mi = (jl_method_instance_t*)v;
                    write_pointerfield(s, mi->def.value);
                    write_pointerfield(s, mi->specTypes);
                    write_pointerfield(s, (jl_value_t*)mi->sparam_vals);
                    continue;
                }
                else if (jl_is_datatype(v)) {
                    // iterate in reverse, so that the element swapped in from the back upon
                    // removal is always one we have already examined
                    for (size_t i = s->uniquing_super.len; i > 0; i--) {
                        if (s->uniquing_super.items[i - 1] == (void*)v) {
                            s->uniquing_super.items[i - 1] = arraylist_pop(&s->uniquing_super);
                            arraylist_push(&s->uniquing_types, (void*)(uintptr_t)(reloc_offset|3));
                        }
                    }
                }
                else {
                    assert(jl_is_datatype_singleton(t) && "unreachable");
                }
            }
            else if (needs_recaching(v, s->query_cache)) {
                arraylist_push(jl_is_datatype(v) ? &s->fixup_types : &s->fixup_objs, (void*)reloc_offset);
            }
        }

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
            arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_array_t, ref.mem))); // relocation location
            jl_value_t *mem = get_replaceable_field((jl_value_t**)&ar->ref.mem, 1);
            uintptr_t memid = backref_id(s, mem, s->link_ids_relocs);
            arraylist_push(&s->relocs_list, (void*)memid); // relocation target
            record_reloc_stat(memid, 0);
            record_memoryref(s, reloc_offset + offsetof(jl_array_t, ref), ar->ref);
        }
        else if (jl_is_genericmemory(v)) {
            assert(f == s->s);
            // Internal data for types in julia.h with `jl_genericmemory_t` field(s)
            jl_genericmemory_t *m = (jl_genericmemory_t*)v;
            const jl_datatype_layout_t *layout = t->layout;
            size_t len = m->length;
            // if (jl_genericmemory_how(m) == JL_GENERICMEMORY_STRINGOWNED) {
            //     jl_value_t *owner = jl_genericmemory_data_owner_field(m);
            //     write_uint(f, len);
            //     write_pointerfield(s, owner);
            //     write_pointerfield(s, owner);
            //     jl_genericmemory_t *new_mem = (jl_genericmemory_t*)&f->buf[reloc_offset];
            //     assert(new_mem->ptr == NULL);
            //     new_mem->ptr = (void*)((char*)m->ptr - (char*)owner); // relocation offset
            // }
            // else
            {
                size_t datasize = len * layout->size;
                size_t tot = datasize;
                int isbitsunion = layout->flags.arrayelem_isunion;
                if (isbitsunion)
                    tot += len;
                size_t headersize = sizeof(jl_genericmemory_t);
                // copy header
                ios_write(f, (char*)v, headersize);
                // write data
                if (!layout->flags.arrayelem_isboxed && layout->first_ptr < 0) {
                    // set owner to NULL
                    write_pointer(f);
                    // Non-pointer eltypes get encoded in the const_data section
                    size_t alignment_amt = JL_SMALL_BYTE_ALIGNMENT;
                    if (tot >= ARRAY_CACHE_ALIGN_THRESHOLD)
                        alignment_amt = JL_CACHE_BYTE_ALIGNMENT;
                    uintptr_t data = LLT_ALIGN(ios_pos(s->const_data), alignment_amt);
                    write_padding(s->const_data, data - ios_pos(s->const_data));
                    // write data and relocations
                    jl_genericmemory_t *new_mem = (jl_genericmemory_t*)&f->buf[reloc_offset];
                    new_mem->ptr = NULL; // relocation offset
                    data /= sizeof(void*);
                    assert(data < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "offset to constant data too large");
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_genericmemory_t, ptr))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + data)); // relocation target
                    jl_value_t *et = jl_tparam1(t);
                    if (jl_is_cpointer_type(et)) {
                        // reset Ptr fields to C_NULL (but keep MAP_FAILED / INVALID_HANDLE)
                        const intptr_t *data = (const intptr_t*)m->ptr;
                        size_t i;
                        for (i = 0; i < len; i++) {
                            if (data[i] != -1)
                                write_pointer(s->const_data);
                            else
                                ios_write(s->const_data, (char*)&data[i], sizeof(data[i]));
                        }
                    }
                    else {
                        if (isbitsunion) {
                            ios_write(s->const_data, (char*)m->ptr, datasize);
                            ios_write(s->const_data, jl_genericmemory_typetagdata(m), len);
                        }
                        else {
                            ios_write(s->const_data, (char*)m->ptr, tot);
                        }
                    }
                    if (len == 0) { // TODO: should we have a zero-page, instead of writing each type's fragment separately?
                        write_padding(s->const_data, layout->size ? layout->size : isbitsunion);
                    }
                    else if (jl_genericmemory_how(m) == JL_GENERICMEMORY_STRINGOWNED) {
                        assert(jl_is_string(jl_genericmemory_data_owner_field(m)));
                        write_padding(s->const_data, 1);
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
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_genericmemory_t, ptr))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item)); // relocation target
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
                                if (fld != NULL) {
                                    arraylist_push(&s->relocs_list, (void*)(uintptr_t)fld_pos); // relocation location
                                    uintptr_t fldid = backref_id(s, fld, s->link_ids_relocs);
                                    arraylist_push(&s->relocs_list, (void*)fldid); // relocation target
                                    record_reloc_stat(fldid, 0);
                                    record_uniquing(s, fld, fld_pos);
                                }
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
        else if (jl_is_string(v)) {
            ios_write(f, (char*)v, sizeof(void*) + jl_string_len(v));
            write_uint8(f, '\0'); // null-terminated strings for easier C-compatibility
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
            uintptr_t data = LLT_ALIGN(ios_pos(s->const_data), 8);
            write_padding(s->const_data, data - ios_pos(s->const_data));
            data /= sizeof(void*);
            assert(data < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "offset to constant data too large");
            arraylist_push(&s->relocs_list, (void*)(reloc_offset + 8)); // relocation location
            arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + data)); // relocation target
            void *pdata = jl_unbox_voidpointer(jl_get_nth_field(v, 2));
            ios_write(s->const_data, (char*)pdata, nb);
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
                else if (t->name->mutabl && jl_is_cpointer_type(jl_field_type_concrete(t, i)) && *(intptr_t*)slot != -1) {
                    // reset Ptr fields to C_NULL (but keep MAP_FAILED / INVALID_HANDLE)
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
                if (fld != NULL) {
                    uintptr_t fld_id = backref_id(s, fld, s->link_ids_relocs);
                    arraylist_push(&s->relocs_list, (void*)(uintptr_t)(fld_pos)); // relocation location
                    arraylist_push(&s->relocs_list, (void*)fld_id); // relocation target
                    record_reloc_stat(fld_id, 0);
                    record_uniquing(s, fld, fld_pos);
                }
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
            else if (t == jl_interned_code_instance_type) {
                assert(f == s->s);
                jl_interned_code_instance_t *ici = (jl_interned_code_instance_t*)v;
                // words are final except: raw heap pointers (uniqued targets)
                // become ordinary relocated+uniqued pointer slots, and zero
                // placeholders are patched after layout via ici_fixups
                uintptr_t *words = ici_words(ici);
                size_t nwords = ici->nedges + __builtin_popcountll(ici->fieldmask);
                for (size_t i = 0; i < nwords; i++) {
                    uintptr_t w = words[i];
                    if (w != 0 && (w >> 62) == 0)
                        write_pointerfield(s, (jl_value_t*)w);
                    else
                        write_uint(f, w);
                }
            }
            else if (jl_is_method(v)) {
                assert(f == s->s);
                write_padding(f, sizeof(jl_method_t) - tot); // hidden fields
                jl_method_t *m = (jl_method_t*)v;
                jl_method_t *newm = (jl_method_t*)&f->buf[reloc_offset];
                if (s->incremental) {
                    // scanned_methods lists are session state (cleared above);
                    // 0x2 must clear with them or the load-time fetch_or path
                    // never re-adds the method to its module's list
                    jl_atomic_store_relaxed(&newm->did_scan_source,
                        jl_atomic_load_relaxed(&newm->did_scan_source) & ~(uint8_t)0x2);
                    if (jl_atomic_load_relaxed(&newm->primary_world) > 1) {
                        jl_atomic_store_relaxed(&newm->primary_world, ~(size_t)0); // min-world
                        int dispatch_status = jl_atomic_load_relaxed(&newm->dispatch_status);
                        int new_dispatch_status = 0;
                        if (!(dispatch_status & METHOD_SIG_LATEST_ONLY))
                            new_dispatch_status |= METHOD_SIG_PRECOMPILE_MANY;
                        jl_atomic_store_relaxed(&newm->dispatch_status, new_dispatch_status);
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
                jl_atomic_store_relaxed(&newci->time_compile, 0.0);
                jl_atomic_store_relaxed(&newci->invoke, NULL);
                // preserve only JL_CI_FLAGS_NATIVE_CACHE_VALID bits
                jl_atomic_store_relaxed(&newci->flags, jl_atomic_load_relaxed(&newci->flags) & JL_CI_FLAGS_NATIVE_CACHE_VALID);
                jl_atomic_store_relaxed(&newci->specptr.fptr, NULL);
                uintptr_t fptr_type = JL_INVOKE_SPECSIG;
                int8_t builtin_id = 0;
                if (jl_atomic_load_relaxed(&ci->invoke) == jl_fptr_const_return) {
                    fptr_type = JL_INVOKE_CONST;
                }
                else {
                    if (jl_is_method(jl_get_ci_mi(ci)->def.method)) {
                        builtin_id = jl_fptr_id(jl_atomic_load_relaxed(&ci->specptr.fptr));
                        if (builtin_id) { // found in the table of builtins
                            assert(builtin_id >= 2);
                            fptr_type = (uintptr_t)JL_INVOKE_ARGS | BuiltinInvokeTag;
                        }
                        else {
                            int32_t invokeptr_id = 0;
                            int32_t specfptr_id = 0;
                            jl_get_function_id(native_functions, ci, &invokeptr_id, &specfptr_id); // see if we generated code for it
                            if (invokeptr_id) {
                                if (invokeptr_id < 0) {
                                    fptr_type = (jl_invoke_api_t)-invokeptr_id;
                                    assert(fptr_type != JL_INVOKE_SPECSIG);
                                } else {
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
                jl_atomic_store_relaxed(&newci->invoke, NULL); // relocation offset
                if (fptr_type != JL_INVOKE_SPECSIG) {
                    assert(fptr_type < BuiltinFunctionTag && "too many functions to serialize");
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_code_instance_t, invoke))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)FunctionRef << RELOC_TAG_OFFSET) + fptr_type)); // relocation target
                }
                if (builtin_id >= 2) {
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_code_instance_t, specptr.fptr))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)FunctionRef << RELOC_TAG_OFFSET) + BuiltinFunctionTag + builtin_id - 2)); // relocation target
                }
            }
            else if (jl_is_datatype(v)) {
                assert(f == s->s);
                jl_datatype_t *dt = (jl_datatype_t*)v;
                jl_datatype_t *newdt = (jl_datatype_t*)&f->buf[reloc_offset];

                if (dt->layout != NULL) {
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
                void *superidx = ptrhash_get(&serialization_order, dt->super);
                if (s->incremental && superidx != HT_NOTFOUND && from_seroder_entry(superidx) > item && needs_uniquing((jl_value_t*)dt->super, s->query_cache))
                    arraylist_push(&s->uniquing_super, dt->super);
            }
            else if (jl_is_typename(v)) {
                assert(f == s->s);
                jl_typename_t *tn = (jl_typename_t*)v;
                jl_typename_t *newtn = (jl_typename_t*)&f->buf[reloc_offset];
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
                assert(f == s->s);
                jl_globalref_t *gr = (jl_globalref_t*)v;
                if (s->incremental && jl_object_in_image((jl_value_t*)gr->mod)) {
                    // will need to populate the binding field later
                    arraylist_push(&s->fixup_objs, (void*)reloc_offset);
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
    if (reloc_stats_wanted() && stat_prev != NULL)
        heap_stat_note(stat_prev, stat_prev_t, ios_pos(s->s) - stat_d0,
                       ios_pos(s->const_data) - stat_c0);
    assert(s->uniquing_super.len == 0);
}

// --- lazy decode of InternedCodeInstance words (see julia.h) ---
typedef struct {
    uintptr_t data_base;
    size_t data_size;
    uintptr_t const_base;
    uint32_t *depsmap; // depsidx -> image_tree range index
    uint32_t ndeps;
    // identity for direct compressed-IR references (TAG_IMAGE_ROOT):
    uint64_t key;      // jl_worklist_key of the image's worklist
    uint8_t has_key;
    uint8_t is_sysimg;
    // interned symbol table of the image, for cross-image symbol references
    jl_sym_t **syms;
    uint32_t nsyms;
} ici_image_info_t;
static arraylist_t ici_images;
static int ici_images_init = 0;

static void ici_register_image(uintptr_t base, size_t size, uintptr_t const_base, jl_array_t *depmods_idxs, int is_sysimg)
{
    if (!ici_images_init) {
        // pre-size: readers (compressed-IR encode on other threads) walk the
        // items array unlocked, so it must not be reallocated in practice
        arraylist_new(&ici_images, 1024);
        ici_images.len = 0;
        ici_images_init = 1;
    }
    ici_image_info_t *info = (ici_image_info_t*)malloc_s(sizeof(ici_image_info_t));
    info->data_base = base;
    info->data_size = size;
    info->const_base = const_base;
    info->key = 0;
    info->has_key = 0;
    info->is_sysimg = (uint8_t)is_sysimg;
    info->syms = NULL;
    info->nsyms = 0;
    info->ndeps = depmods_idxs ? jl_array_len(depmods_idxs) : 0;
    info->depsmap = info->ndeps ? (uint32_t*)malloc_s(info->ndeps * sizeof(uint32_t)) : NULL;
    if (info->ndeps)
        memcpy(info->depsmap, jl_array_data(depmods_idxs, uint32_t), info->ndeps * sizeof(uint32_t));
    arraylist_push(&ici_images, info);
}

static ici_image_info_t *ici_find_image(uintptr_t a) JL_NOTSAFEPOINT
{
    static ici_image_info_t *last = NULL;
    ici_image_info_t *hit = last;
    if (hit && a - hit->data_base < hit->data_size)
        return hit;
    for (size_t k = 0; k < ici_images.len; k++) {
        hit = (ici_image_info_t*)ici_images.items[k];
        if (a - hit->data_base < hit->data_size) {
            last = hit;
            return hit;
        }
    }
    return NULL;
}

// --- direct compressed-IR image references (TAG_IMAGE_ROOT) ---

// stamp the worklist key on a restored image so this session's compressed IR
// can reference its objects by (key, offset)
static void ici_set_image_key(uintptr_t base, uint64_t key) JL_NOTSAFEPOINT
{
    ici_image_info_t *info = ici_find_image(base);
    if (info != NULL && !info->is_sysimg) {
        info->key = key;
        info->has_key = 1;
    }
}


// In deserialization, create Symbols and set up the
// index for backreferencing
static void jl_read_symbols(jl_serializer_state *s)
{
    assert(deser_sym.len == 0);
    uintptr_t base = (uintptr_t)&s->symbols->buf[0];
    uintptr_t end = base + s->symbols->size;
    // per-dependency symbol tables, resolved lazily once per image
    size_t ndeps_tab = s->buildid_depmods_idxs ? jl_array_len(s->buildid_depmods_idxs) : 0;
    jl_sym_t ***dep_syms = (jl_sym_t***)alloca((ndeps_tab ? ndeps_tab : 1) * sizeof(void*));
    memset(dep_syms, 0, (ndeps_tab ? ndeps_tab : 1) * sizeof(void*));
    size_t nshared = 0, ninline = 0;
    while (base < end) {
        uint32_t len = jl_load_unaligned_i32((void*)base);
        base += 4;
        if (len & 0x80000000u) {
            // shared with a dependency image: (depsidx, index) into its
            // already-interned symbol table
            uint32_t depsidx = len & 0x7fffffffu;
            uint32_t symidx = jl_load_unaligned_i32((void*)base);
            base += 4;
            assert(depsidx < ndeps_tab);
            jl_sym_t **tab = dep_syms[depsidx];
            if (tab == NULL) {
                ici_image_info_t *info = NULL;
                if (depsidx == 0) {
                    // dependency index 0 is the sysimage
                    for (size_t k = 0; k < ici_images.len; k++) {
                        ici_image_info_t *e = (ici_image_info_t*)ici_images.items[k];
                        if (e->is_sysimg) {
                            info = e;
                            break;
                        }
                    }
                }
                else {
                    size_t i = jl_array_data(s->buildid_depmods_idxs, uint32_t)[depsidx];
                    assert(i < image_tree.nranges);
                    image_metadata_t *meta = (image_metadata_t*)image_tree.ranges[i].data;
                    info = ici_find_image(meta->base);
                }
                assert(info && info->syms && "dependency symbol table missing");
                tab = dep_syms[depsidx] = info->syms;
            }
            nshared++;
            arraylist_push(&deser_sym, (void*)tab[symidx]);
            continue;
        }
        const char *str = (const char*)base;
        base += len + 1;
        //printf("symbol %3d: %s\n", len, str);
        jl_sym_t *sym = _jl_symbol(str, len);
        ninline++;
        arraylist_push(&deser_sym, (void*)sym);
    }
    if (ici_debug_enabled())
        jl_safe_printf("SYMS: shared=%zd inline=%zd\n", nshared, ninline);
    // persist for cross-image references from images that depend on this one
    // (s->s is not set during this pass; the symbols section lies within the
    // same registered file span)
    ici_image_info_t *self = ici_find_image((uintptr_t)s->symbols->buf);
    if (self != NULL && self->syms == NULL && deser_sym.len > 0) {
        self->syms = (jl_sym_t**)malloc_s(deser_sym.len * sizeof(jl_sym_t*));
        memcpy(self->syms, deser_sym.items, deser_sym.len * sizeof(jl_sym_t*));
        self->nsyms = (uint32_t)deser_sym.len;
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
        if (reloc_base & 1) {
            // convert to a ConstDataRef
            tag = ConstDataRef;
            reloc_base &= ~(uintptr_t)1;
            assert(LLT_ALIGN(reloc_base, sizeof(void*)) == reloc_base);
            reloc_base /= sizeof(void*);
            assert(reloc_offset == 0);
        }
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
                assert(offset < jl_n_builtins && "unknown function pointer id");
            }
            else {
                assert((offset & ~BuiltinInvokeTag) < JL_INVOKE_SPECSIG &&
                       "unknown function pointer id");
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
static inline uintptr_t get_item_for_reloc(jl_serializer_state *s, uintptr_t base, uintptr_t reloc_id, jl_array_t *link_ids, int *link_index) JL_NOTSAFEPOINT
{
    enum RefTags tag = (enum RefTags)(reloc_id >> RELOC_TAG_OFFSET);
    size_t offset = (reloc_id & (((uintptr_t)1 << RELOC_TAG_OFFSET) - 1));
    switch (tag) {
    case DataRef:
        assert(offset <= s->s->size);
        return (uintptr_t)base + offset;
    case ConstDataRef:
        offset *= sizeof(void*);
        assert(offset <= s->const_data->size);
        return (uintptr_t)s->const_data->buf + offset;
    case SymbolRef:
        assert(offset < deser_sym.len && deser_sym.items[offset] && "corrupt relocation item id");
        return (uintptr_t)deser_sym.items[offset];
    case TagRef: {
        // for the purpose of this function, we only access boxes we know are perm-alloc
        jl_value_t *jl_box_int64(int64_t) JL_NOTSAFEPOINT;
        jl_value_t *jl_box_int32(int32_t) JL_NOTSAFEPOINT;
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
    }
    case FunctionRef: {
        if (offset & BuiltinFunctionTag) {
            offset &= ~BuiltinFunctionTag;
            assert(offset < jl_n_builtins && "unknown function pointer ID");
            return (uintptr_t)jl_builtin_f_addrs[offset];
        }
        jl_invoke_api_t type = (jl_invoke_api_t)(offset & ~BuiltinInvokeTag);
        uintptr_t fptr = (uintptr_t)jl_invoke_api_callptr(type);
        assert(fptr && "corrupt relocation item id");
        // If use_sysimage_native_code != yes, zero out the invoke pointer for
        // CodeInstances with native code, but not if invoke is jl_fptr_args and
        // the specptr is a builtin.
        if (s->image->fptrs.nptrs == 0 && jl_jlcall_specptr_is_native(type) &&
            !(offset & BuiltinInvokeTag))
            return 0;
        return fptr;
    }
    case SysimageLinkage: {
#ifdef _P64
        size_t depsidx = offset >> DEPS_IDX_OFFSET;
        offset &= ((size_t)1 << DEPS_IDX_OFFSET) - 1;
#else
        size_t depsidx = 0;
#endif
        if (ici_debug_enabled() &&
            (!s->buildid_depmods_idxs || depsidx >= jl_array_len(s->buildid_depmods_idxs))) {
            jl_safe_printf("BAD SYSIMG RELOC: depsidx %zd len %zd offset %zx\n",
                           depsidx, s->buildid_depmods_idxs ? (size_t)jl_array_len(s->buildid_depmods_idxs) : 0,
                           offset);
            return (uintptr_t)jl_nothing; // survive to gather more
        }
        assert(s->buildid_depmods_idxs && depsidx < jl_array_len(s->buildid_depmods_idxs));
        size_t i = jl_array_data(s->buildid_depmods_idxs, uint32_t)[depsidx];
        assert(i < image_tree.nranges);
        image_metadata_t *meta = (image_metadata_t*)image_tree.ranges[i].data;
        return meta->base + offset*SYS_EXTERNAL_LINK_UNIT;
    }
    case ExternalLinkage: {
        assert(link_ids);
        assert(link_index);
        assert(0 <= *link_index && *link_index < jl_array_len(link_ids));
        uint32_t depsidx = jl_array_data(link_ids, uint32_t)[*link_index];
        *link_index += 1;
        assert(depsidx < jl_array_len(s->buildid_depmods_idxs));
        size_t i = jl_array_data(s->buildid_depmods_idxs, uint32_t)[depsidx];
        assert(i < image_tree.nranges);
        image_metadata_t *meta = (image_metadata_t*)image_tree.ranges[i].data;
        return meta->base + offset*SYS_EXTERNAL_LINK_UNIT;
    }
    }
    abort();
}


static void depsym_map_build(jl_serializer_state *s) JL_GC_DISABLED
{
    assert(!depsym_map_init);
    htable_new(&depsym_map, 1 << 17);
    depsym_map_init = 1;
    if (ici_debug_enabled())
        jl_safe_printf("DEPSYM GATE: incremental=%d depmods=%d images_init=%d\n",
                       s->incremental, s->buildid_depmods_idxs != NULL, ici_images_init);
    if (!s->incremental || s->buildid_depmods_idxs == NULL || !ici_images_init)
        return;
    uint32_t *blob_to_depsidx = jl_array_data(s->buildid_depmods_idxs, uint32_t);
    size_t nblobmap = jl_array_len(s->buildid_depmods_idxs);
    size_t used = 0, nosyms = 0, nodep = 0;
    for (size_t k = 0; k < ici_images.len; k++) {
        ici_image_info_t *info = (ici_image_info_t*)ici_images.items[k];
        if (info->syms == NULL) {
            nosyms++;
            continue;
        }
        uintptr_t depsidx;
        if (info->is_sysimg) {
            depsidx = 0; // by convention, dependency index 0 is the sysimage
        }
        else {
            size_t blob = external_blob_index((jl_value_t*)info->data_base);
            if (blob >= nblobmap || blob_to_depsidx[blob] == (uint32_t)-1) {
                nodep++;
                continue; // not a declared dependency of this output
            }
            depsidx = blob_to_depsidx[blob];
        }
        used++;
        for (uint32_t i = 0; i < info->nsyms; i++) {
            void **bp = ptrhash_bp(&depsym_map, (void*)info->syms[i]);
            if (*bp == HT_NOTFOUND)
                *bp = (void*)(((depsidx + 1) << 40) | i);
        }
    }
    if (ici_debug_enabled())
        jl_safe_printf("DEPSYM: images=%zd used=%zd nosyms=%zd nodep=%zd mapped=%zd\n",
                       ici_images.len, used, nosyms, nodep, depsym_map.count / 2);
}

// probe: can `v` be referenced directly as (image key, offset)?
// returns 0 = no, 1 = package image (*key set), 2 = sysimage; *offset is the
// byte offset from the image's mapped base either way
JL_DLLEXPORT int jl_image_ref_of(jl_value_t *v, uint64_t *key, uint64_t *offset) JL_NOTSAFEPOINT
{
    ici_image_info_t *info = ici_find_image((uintptr_t)v);
    if (info == NULL)
        return 0;
    if (info->is_sysimg) {
        *key = 0;
        *offset = (uintptr_t)v - info->data_base;
        return 2;
    }
    if (!info->has_key)
        return 0;
    *key = info->key;
    *offset = (uintptr_t)v - info->data_base;
    return 1;
}

// resolve a direct reference; NULL when the image is not loaded (the
// dependency closure guarantee makes that unreachable for valid caches)
JL_DLLEXPORT jl_value_t *jl_image_ref_resolve(int is_sysimg, uint64_t key, uint64_t offset) JL_NOTSAFEPOINT
{
    static ici_image_info_t *last = NULL;
    ici_image_info_t *hit = last;
    if (hit == NULL || (is_sysimg ? !hit->is_sysimg : (!hit->has_key || hit->key != key))) {
        hit = NULL;
        for (size_t k = 0; k < ici_images.len; k++) {
            ici_image_info_t *e = (ici_image_info_t*)ici_images.items[k];
            if (is_sysimg ? e->is_sysimg : (e->has_key && e->key == key)) {
                hit = e;
                break;
            }
        }
        if (hit == NULL)
            return NULL;
        last = hit;
    }
    assert(offset < hit->data_size);
    return (jl_value_t*)(hit->data_base + offset);
}

static jl_value_t *ici_decode_ref(jl_interned_code_instance_t *ici, uintptr_t w) JL_NOTSAFEPOINT
{
    size_t key = (w & ~(ICI_TAG_REF | ICI_TAG_CONST)) >> DEPS_IDX_OFFSET;
    uintptr_t off = (w & (((uintptr_t)1 << DEPS_IDX_OFFSET) - 1)) * SYS_EXTERNAL_LINK_UNIT;
    ici_image_info_t *info = ici_find_image((uintptr_t)ici);
    assert(info && "InternedCodeInstance outside any registered image");
    if (ici_debug_enabled()) {
        if (!(w & ICI_TAG_REF))
            jl_safe_printf("ICI BAD WORD (no ref tag): ici=%p nedges=%zd mask=%zx w=%zx\n",
                           (void*)ici, ici->nedges, ici->fieldmask, w);
        else if ((w & ICI_TAG_CONST) && key != 0)
            jl_safe_printf("ICI BAD CONST KEY: ici=%p w=%zx\n", (void*)ici, w);
        else if (key == 0 && !(w & ICI_TAG_CONST) && off >= info->data_size)
            jl_safe_printf("ICI SELF OOB: ici=%p nedges=%zd mask=%zx w=%zx off=%zx size=%zx\n",
                           (void*)ici, ici->nedges, ici->fieldmask, w, off, info->data_size);
    }
    if (w & ICI_TAG_CONST) {
        assert(key == 0 && "const refs are self-image only");
        return (jl_value_t*)(info->const_base + off);
    }
    if (key == 0)
        return (jl_value_t*)(info->data_base + off);
    assert(key - 1 < info->ndeps);
    size_t ri = info->depsmap[key - 1];
    assert(ri < image_tree.nranges);
    image_metadata_t *meta = (image_metadata_t*)image_tree.ranges[ri].data;
    return (jl_value_t*)((uintptr_t)meta->base + off);
}

// debug aid: report which registered image (if any) contains `obj`
JL_DLLEXPORT void jl_ici_debug_whereis(void *obj) JL_NOTSAFEPOINT
{
    ici_image_info_t *info = ici_find_image((uintptr_t)obj);
    if (info)
        jl_safe_printf("  whereis: image base=%p offset=%zx size=%zx\n",
                       (void*)info->data_base, (size_t)((uintptr_t)obj - info->data_base),
                       info->data_size);
    else
        jl_safe_printf("  whereis: not in a registered incremental image\n");
}

// decode an interned owner-field word; NULL if the field was not interned
JL_DLLEXPORT jl_value_t *jl_ici_fieldref(jl_value_t *edges, int rank) JL_NOTSAFEPOINT
{
    if (edges == NULL || !jl_typetagis(edges, jl_interned_code_instance_type))
        return NULL;
    jl_interned_code_instance_t *ici = (jl_interned_code_instance_t*)edges;
    uintptr_t bit = (uintptr_t)1 << rank;
    if (!(ici->fieldmask & bit))
        return NULL;
    size_t idx = ici->nedges + __builtin_popcountll(ici->fieldmask & (bit - 1));
    jl_value_t *v = ici_decode_ref(ici, ici_words(ici)[idx]);
    if (v == NULL && ici_debug_enabled()) {
        jl_safe_printf("ICI FIELDREF NULL: ici=%p rank=%d word=%zx mask=%zx nedges=%zd\n",
                       (void*)ici, rank, (size_t)ici_words(ici)[idx],
                       (size_t)ici->fieldmask, (size_t)ici->nedges);
        jl_ici_debug_whereis((void*)ici);
    }
    return v;
}

JL_DLLEXPORT jl_value_t *jl_ici_ref(jl_interned_code_instance_t *ici, size_t i)
{
    assert(i < ici->nedges);
    uintptr_t w = ici_words(ici)[i];
    if (w & ICI_TAG_LITERAL) {
        uintptr_t z = w & ~ICI_TAG_LITERAL;
        return jl_box_long((intptr_t)((z >> 1) ^ (0 - (z & 1))));
    }
    if (w & ICI_TAG_REF)
        return ici_decode_ref(ici, w);
    return (jl_value_t*)w; // relocated (and possibly uniqued) pointer
}

// like jl_ici_ref, but returns NULL for literal words instead of boxing
JL_DLLEXPORT jl_value_t *jl_ici_ref_nobox(jl_interned_code_instance_t *ici, size_t i) JL_NOTSAFEPOINT
{
    assert(i < ici->nedges);
    uintptr_t w = ici_words(ici)[i];
    if (w & ICI_TAG_LITERAL)
        return NULL;
    if (w & ICI_TAG_REF)
        return ici_decode_ref(ici, w);
    return (jl_value_t*)w;
}

// if word `i` is an inline literal, store its value in `out` and return 1
JL_DLLEXPORT int jl_ici_literal(jl_interned_code_instance_t *ici, size_t i, intptr_t *out) JL_NOTSAFEPOINT
{
    assert(i < ici->nedges);
    uintptr_t w = ici_words(ici)[i];
    if (!(w & ICI_TAG_LITERAL))
        return 0;
    uintptr_t z = w & ~ICI_TAG_LITERAL;
    *out = (intptr_t)((z >> 1) ^ (0 - (z & 1)));
    return 1;
}

JL_DLLEXPORT void jl_ci_materialize_all(jl_code_instance_t *ci) JL_NOTSAFEPOINT;

// rematerialize an interned ci->def (and, as defense in depth, every other
// interned field of this CodeInstance): decode the words and write the
// objects back so subsequent reads take the plain-pointer paths
JL_DLLEXPORT jl_value_t *jl_ici_materialize_def(jl_code_instance_t *ci) JL_NOTSAFEPOINT
{
    jl_value_t *edges = (jl_value_t*)jl_atomic_load_relaxed(&ci->edges);
    assert(edges != NULL && jl_typetagis(edges, jl_interned_code_instance_type) &&
           "NULL ci->def without an interned container");
    jl_interned_code_instance_t *ici = (jl_interned_code_instance_t*)edges;
    assert(ici->defword != 0);
    jl_value_t *def = ici_decode_ref(ici, ici->defword);
    ci->def = def;
    jl_gc_wb(ci, def);
    jl_ci_materialize_all(ci); // fill the remaining interned fields too
    return def;
}

// materialize every interned field of `ci` (invoked by the first accessor
// that finds its field interned; also makes stray unrouted raw reads safe
// once any accessor has touched the object)
JL_DLLEXPORT void jl_ci_materialize_all(jl_code_instance_t *ci) JL_NOTSAFEPOINT
{
    jl_value_t *edges = (jl_value_t*)jl_atomic_load_relaxed(&ci->edges);
    if (edges == NULL || !jl_typetagis(edges, jl_interned_code_instance_type))
        return;
    jl_interned_code_instance_t *ici = (jl_interned_code_instance_t*)edges;
    if (ici->defword != 0 && ci->def == NULL)
        jl_ici_materialize_def(ci);
    uintptr_t mask = ici->fieldmask;
    if (mask == 0)
        return;
    jl_value_t *v;
#define ICI_MAT(rank, cond, store) \
    if ((mask & ((uintptr_t)1 << (rank))) && (cond)) { \
        v = jl_ici_fieldref(edges, rank); \
        if (v != NULL) { store; jl_gc_wb(ci, v); } \
    }
    ICI_MAT(JL_ICI_CI_OWNER, ci->owner == NULL, ci->owner = v)
    // JL_ICI_CI_NEXT is never interned nor materialized: `next` is runtime-mutable
    // chain state and NULL is a valid runtime value (see the save-side comment)
    ICI_MAT(JL_ICI_CI_RETTYPE, ci->rettype == NULL, ci->rettype = v)
    ICI_MAT(JL_ICI_CI_EXCTYPE, ci->exctype == NULL, ci->exctype = v)
    ICI_MAT(JL_ICI_CI_RETTYPE_CONST, ci->rettype_const == NULL, ci->rettype_const = v)
    ICI_MAT(JL_ICI_CI_INFERRED, jl_atomic_load_relaxed(&ci->inferred) == NULL,
            jl_atomic_store_release(&ci->inferred, v))
    ICI_MAT(JL_ICI_CI_DEBUGINFO, jl_atomic_load_relaxed(&ci->debuginfo) == NULL,
            jl_atomic_store_release(&ci->debuginfo, (jl_debuginfo_t*)v))
    ICI_MAT(JL_ICI_CI_ANALYSIS, ci->analysis_results == NULL, ci->analysis_results = v)
#undef ICI_MAT
    if (ici_debug_enabled()) {
        jl_value_t *fvals[8] = {
            ci->owner, NULL, ci->rettype, ci->exctype, ci->rettype_const,
            jl_atomic_load_relaxed(&ci->inferred),
            (jl_value_t*)jl_atomic_load_relaxed(&ci->debuginfo), ci->analysis_results
        };
        for (int r = 0; r < 8; r++) {
            if (r == JL_ICI_CI_NEXT || !(mask & ((uintptr_t)1 << r)) || fvals[r] != NULL)
                continue;
            size_t idx = ici->nedges + __builtin_popcountll(mask & (((uintptr_t)1 << r) - 1));
            jl_safe_printf("ICI MAT MISSING: ci=%p rank=%d word=%zx mask=%zx nedges=%zd\n",
                           (void*)ci, r, (size_t)ici_words(ici)[idx], (size_t)mask,
                           (size_t)ici->nedges);
            jl_ici_debug_whereis((void*)ici);
        }
    }
}

// debug: dump a CodeInstance's raw state (diagnosing missing-field reads)
JL_DLLEXPORT void jl_ici_debug_dump(jl_code_instance_t *ci)
{
    jl_value_t *edges = (jl_value_t*)jl_atomic_load_relaxed(&ci->edges);
    jl_safe_printf("ICI DUMP ci=%p in_image=%d def=%p owner=%p next=%p world=[%zd,%zx] "
                   "rettype=%p inferred=%p debuginfo=%p analysis=%p edges=%p\n",
                   (void*)ci, jl_object_in_image((jl_value_t*)ci), (void*)ci->def,
                   (void*)ci->owner, (void*)jl_atomic_load_relaxed(&ci->next),
                   (size_t)jl_atomic_load_relaxed(&ci->min_world),
                   (size_t)jl_atomic_load_relaxed(&ci->max_world),
                   (void*)ci->rettype, (void*)jl_atomic_load_relaxed(&ci->inferred),
                   (void*)jl_atomic_load_relaxed(&ci->debuginfo),
                   (void*)ci->analysis_results, (void*)edges);
    if (edges != NULL && jl_typetagis(edges, jl_interned_code_instance_type)) {
        jl_interned_code_instance_t *ici = (jl_interned_code_instance_t*)edges;
        jl_safe_printf("  edges ICI: nedges=%zd defword=%zx mask=%zx\n",
                       (size_t)ici->nedges, (size_t)ici->defword, (size_t)ici->fieldmask);
    }
    else if (edges != NULL && jl_is_svec(edges)) {
        jl_safe_printf("  edges svec len=%zd\n", jl_svec_len(edges));
    }
    jl_ici_debug_whereis((void*)ci);
}

// materializing def accessor for Julia callers (the field itself reads as
// undefined when interned, and isdefined const-folds for CodeInstance)
JL_DLLEXPORT jl_value_t *jl_ci_def(jl_code_instance_t *ci)
{
    jl_value_t *def = ci->def;
    if (def == NULL)
        def = jl_ici_materialize_def(ci);
    return def;
}

// read-only variant for signal/dump contexts (no heap write)
JL_DLLEXPORT jl_value_t *jl_ci_def_ro(jl_code_instance_t *ci) JL_NOTSAFEPOINT
{
    jl_value_t *def = ci->def;
    if (def != NULL)
        return def;
    jl_value_t *edges = (jl_value_t*)jl_atomic_load_relaxed(&ci->edges);
    if (edges == NULL || !jl_typetagis(edges, jl_interned_code_instance_type))
        return NULL;
    jl_interned_code_instance_t *ici = (jl_interned_code_instance_t*)edges;
    if (ici->defword == 0)
        return NULL;
    return ici_decode_ref(ici, ici->defword);
}

// Convert an interned DebugInfo subtree back to its plain form: fill the
// def/linetable/codelocs fields, restore `edges` to its SimpleVector form,
// and recurse through edge children and the linetable chain. Raw compiled
// field reads (`isdefined` const-folds for DebugInfo, and bootstrap-compiled
// Compiler code bypasses getproperty overrides) are only safe against
// converted nodes, so every path that hands a DebugInfo tree to Julia-visible
// structures must convert it first; after conversion the object is
// indistinguishable from a never-interned one. `edges` still holding the
// interned container doubles as the not-yet-converted marker.
JL_DLLEXPORT void jl_di_materialize_all(jl_debuginfo_t *di)
{
    jl_value_t *edges = (jl_value_t*)di->edges;
    if (edges == NULL || !jl_typetagis(edges, jl_interned_code_instance_type))
        return; // converted (or never interned)
    jl_interned_code_instance_t *ici = (jl_interned_code_instance_t*)edges;
    uintptr_t mask = ici->fieldmask;
    jl_value_t *v;
#define ICI_MATD(rank, field) \
    if ((mask & ((uintptr_t)1 << (rank))) && di->field == NULL) { \
        v = jl_ici_fieldref(edges, rank); \
        if (v != NULL) { di->field = (void*)v; jl_gc_wb(di, v); } \
    }
    ICI_MATD(JL_ICI_DI_DEF, def)
    ICI_MATD(JL_ICI_DI_LINETABLE, linetable)
    ICI_MATD(JL_ICI_DI_CODELOCS, codelocs)
#undef ICI_MATD
    jl_value_t *lt = di->linetable;
    if (lt != NULL && jl_is_debuginfo(lt))
        jl_di_materialize_all((jl_debuginfo_t*)lt);
    jl_svec_t *sv = jl_ici_to_svec(ici);
    JL_GC_PUSH1(&sv);
    for (size_t i = 0, n = jl_svec_len(sv); i < n; i++) {
        jl_value_t *el = jl_svecref(sv, i);
        if (el != NULL && jl_is_debuginfo(el))
            jl_di_materialize_all((jl_debuginfo_t*)el);
    }
    // publish last: the interned container is the not-converted marker
    di->edges = sv;
    jl_gc_wb(di, sv);
    JL_GC_POP();
    if (ici_debug_enabled()) {
        jl_value_t *d = di->def;
        jl_safe_printf("DI MAT: di=%p mask=%zx def=%p deftag=%zx nedges=%zd\n",
                       (void*)di, mask, (void*)d,
                       d ? (uintptr_t)jl_typetagof(d) : 0, jl_svec_len(sv));
    }
}

// dual-form edge list accessors: `edges` is a SimpleVector, or the interned
// word form for image objects (CodeInstance and DebugInfo edge lists)
JL_DLLEXPORT size_t jl_edgelist_len(jl_value_t *edges) JL_NOTSAFEPOINT
{
    if (jl_typetagis(edges, jl_interned_code_instance_type))
        return ((jl_interned_code_instance_t*)edges)->nedges;
    return jl_svec_len(edges);
}

JL_DLLEXPORT jl_value_t *jl_edgelist_ref(jl_value_t *edges, size_t i)
{
    if (jl_typetagis(edges, jl_interned_code_instance_type))
        return jl_ici_ref((jl_interned_code_instance_t*)edges, i);
    return jl_svecref(edges, i);
}

// materialize the interned form back into a SimpleVector, for consumers
// that hand the edge list onward (IR decompression, const CodeInfos)
JL_DLLEXPORT jl_svec_t *jl_ici_to_svec(jl_interned_code_instance_t *ici)
{
    size_t n = ici->nedges;
    jl_svec_t *out = jl_alloc_svec(n);
    JL_GC_PUSH1(&out);
    for (size_t i = 0; i < n; i++)
        jl_svecset(out, i, jl_ici_ref(ici, i));
    JL_GC_POP();
    return out;
}

static void jl_finish_relocs(char *base, size_t size, arraylist_t *list)
{
    for (size_t i = 0; i < list->len; i += 2) {
        size_t pos = (size_t)list->items[i];
        size_t item = (size_t)list->items[i + 1];   // item is tagref-encoded
        uintptr_t *pv = (uintptr_t*)(base + pos);
        assert(pos < size && pos != 0);
        *pv = get_reloc_for_item(item, *pv);
    }
}

static void jl_write_offsetlist(ios_t *s, size_t size, arraylist_t *list)
{
    for (size_t i = 0; i < list->len; i += 2) {
        size_t last_pos = i ? (size_t)list->items[i - 2] : 0;
        size_t pos = (size_t)list->items[i];
        assert(pos < size && pos != 0);
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

static void jl_read_reloclist(jl_serializer_state *s, jl_array_t *link_ids, uint8_t bits)
{
    JL_TIMING(LOAD_IMAGE, LOAD_Relocs);
    uintptr_t base = (uintptr_t)s->s->buf;
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
        if (ici_debug_enabled()) {
            // stream-shape histogram: run structure and delta sizes
            static uint64_t *h = NULL;
            if (h == NULL)
                h = (uint64_t*)calloc_s(8 * sizeof(uint64_t));
            h[0]++;                       // records
            if (pos_diff == 8) {
                h[1]++;                   // stride-8 continuation
            }
            else {
                h[2]++;                   // run break
                if (pos_diff < 128 * 8)
                    h[3]++;               // delta fits 1 varint byte even unshifted
                if ((pos_diff >> 3) < 128)
                    h[4]++;               // delta>>3 fits 1 byte
            }
            h[5] += (v >> RELOC_TAG_OFFSET) == DataRef;
            h[6] += (v >> RELOC_TAG_OFFSET) == SysimageLinkage;
            if ((h[0] & 0xFFFFF) == 0)
                jl_safe_printf("RELOC HIST: n=%zu run8=%zu breaks=%zu d1B=%zu d1Bshift=%zu dataref=%zu syslink=%zu\n",
                               (size_t)h[0], (size_t)h[1], (size_t)h[2], (size_t)h[3],
                               (size_t)h[4], (size_t)h[5], (size_t)h[6]);
        }
        v = get_item_for_reloc(s, base, v, link_ids, &link_index);
        if (bits && v && ((jl_datatype_t*)v)->smalltag)
            v = (uintptr_t)((jl_datatype_t*)v)->smalltag << 4; // TODO: should we have a representation that supports sweep without a relocation step?
        *pv = v | bits;
    }
    assert(!link_ids || link_index == jl_array_len(link_ids));
}

static void jl_read_memreflist(jl_serializer_state *s)
{
    uintptr_t base = (uintptr_t)s->s->buf;
    uintptr_t last_pos = 0;
    uint8_t *current = (uint8_t *)(s->relocs->buf + s->relocs->bpos);
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
        jl_genericmemoryref_t *pv = (jl_genericmemoryref_t*)(base + pos);
        size_t offset = (size_t)pv->ptr_or_offset;
        pv->ptr_or_offset = (void*)((char*)pv->mem->ptr + offset);
    }
}


static void jl_read_arraylist(ios_t *s, arraylist_t *list)
{
    size_t list_len = read_uint(s);
    arraylist_new(list, 0);
    arraylist_grow(list, list_len);
    ios_read(s, (char*)list->items, list_len * sizeof(void*));
}

// Persistent set of image objects that reference non-image objects.
// Used to track GC reachability for mutable objects in the images,
// treating them as a third, "permanent" GC generation.
arraylist_t image_remset;
jl_mutex_t image_remset_lock;

// jl_write_value and jl_read_value are used for storing Julia objects that are adjuncts to
// the image proper. For example, new methods added to external callables require
// insertion into the appropriate method table.
#define jl_write_value(s, v) _jl_write_value((s), (jl_value_t*)(v))
static void _jl_write_value(jl_serializer_state *s, jl_value_t *v) JL_GC_DISABLED
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
    uintptr_t base = (uintptr_t)s->s->buf;
    uintptr_t offset = *(reloc_t*)(base + (uintptr_t)s->s->bpos);
    s->s->bpos += sizeof(reloc_t);
    if (offset == 0)
        return NULL;
    return (jl_value_t*)get_item_for_reloc(s, base, offset, NULL, NULL);
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
    uintptr_t base = (uintptr_t)s->s->buf;
    int link_index = 0;
    jl_value_t *ret = (jl_value_t*)get_item_for_reloc(s, base, offset, s->link_ids_relocs, &link_index);
    assert(!s->link_ids_relocs || link_index < jl_array_len(s->link_ids_relocs));
    return ret;
}


static void jl_update_all_fptrs(jl_serializer_state *s, jl_image_t *image)
{
    jl_image_fptrs_t fvars = image->fptrs;
    // make these NULL now so we skip trying to restore GlobalVariable pointers later
    image->gvars_base = NULL;
    if (fvars.nptrs == 0)
        return;

    memcpy(image->jl_small_typeof, &jl_small_typeof, sizeof(jl_small_typeof));

    int img_fvars_max = s->fptr_record->size / sizeof(void*);
    size_t i;
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    // These will become MethodInstance references, but they start out as a list of
    // offsets into `s` for CodeInstances
    jl_code_instance_t **linfos = (jl_code_instance_t**)&s->fptr_record->buf[0];
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
            assert(jl_is_method(jl_get_ci_mi(codeinst)->def.method) && jl_atomic_load_relaxed(&codeinst->invoke) != jl_fptr_const_return);
            assert(specfunc ? jl_atomic_load_relaxed(&codeinst->invoke) != NULL : jl_atomic_load_relaxed(&codeinst->invoke) == NULL);
            linfos[i] = codeinst;
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

static uint32_t write_gvars(jl_serializer_state *s, arraylist_t *globals, arraylist_t *external_fns) JL_GC_DISABLED
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
        assert(ci && (jl_atomic_load_relaxed(&ci->flags) & JL_CI_FLAGS_SPECPTR_SPECIALIZED));
        uintptr_t item = backref_id(s, (void*)ci, s->link_ids_external_fnvars);
        uintptr_t reloc = get_reloc_for_item(item, 0);
        write_reloc_t(s->gvar_record, reloc);
    }
    return globals->len;
}

// Pointer relocation for native-code referenced global variables
static void jl_update_all_gvars(jl_serializer_state *s, jl_image_t *image, uint32_t external_fns_begin)
{
    JL_TIMING(LOAD_IMAGE, LOAD_Gvars);
    if (image->gvars_base == NULL)
        return;
    uintptr_t base = (uintptr_t)s->s->buf;
    size_t i = 0;
    size_t l = s->gvar_record->size / sizeof(reloc_t);
    reloc_t *gvars = (reloc_t*)&s->gvar_record->buf[0];
    int gvar_link_index = 0;
    int external_fns_link_index = 0;
    assert(l == image->ngvars);
    for (i = 0; i < l; i++) {
        uintptr_t offset = gvars[i];
        uintptr_t v = 0;
        if (i < external_fns_begin) {
            v = get_item_for_reloc(s, base, offset, s->link_ids_gvars, &gvar_link_index);
        }
        else {
            v = get_item_for_reloc(s, base, offset, s->link_ids_external_fnvars, &external_fns_link_index);
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
                v = (uintptr_t)jl_as_global_root((jl_value_t*)v, 1);
        }
        else {
            jl_code_instance_t *codeinst = (jl_code_instance_t*) v;
            assert(codeinst && (jl_atomic_load_relaxed(&codeinst->flags) & JL_CI_FLAGS_SPECPTR_SPECIALIZED) && jl_atomic_load_relaxed(&codeinst->specptr.fptr));
            v = (uintptr_t)jl_atomic_load_relaxed(&codeinst->specptr.fptr);
        }
        *gv = v;
    }
}

// Code below helps slim down the images by
// removing cached types not referenced in the stream
static jl_svec_t *jl_prune_type_cache_hash(jl_svec_t *cache) JL_GC_DISABLED
{
    size_t l = jl_svec_len(cache), i;
    size_t sz = 0;
    if (l == 0)
        return cache;
    for (i = 0; i < l; i++) {
        jl_value_t *ti = jl_svecref(cache, i);
        if (ti == jl_nothing)
            continue;
        if (ptrhash_get(&serialization_order, ti) == HT_NOTFOUND)
            jl_svecset(cache, i, jl_nothing);
        else
            sz += 1;
    }
    if (sz < HT_N_INLINE)
        sz = HT_N_INLINE;

    void *idx = ptrhash_get(&serialization_order, cache);
    assert(idx != HT_NOTFOUND && idx != (void*)(uintptr_t)-1);
    assert(serialization_queue.items[from_seroder_entry(idx)] == cache);
    cache = cache_rehash_set(cache, sz);
    // redirect all references to the old cache to relocate to the new cache object
    ptrhash_put(&serialization_order, cache, idx);
    serialization_queue.items[from_seroder_entry(idx)] = cache;
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

static void jl_prune_mi_backedges(jl_array_t *backedges)
{
    if (backedges == NULL)
        return;
    size_t i = 0, ins = 0, n = jl_array_nrows(backedges);
    while (i < n) {
        jl_value_t *invokeTypes;
        jl_code_instance_t *caller;
        i = get_next_edge(backedges, i, &invokeTypes, &caller);
        if (ptrhash_get(&serialization_order, caller) != HT_NOTFOUND)
            ins = set_next_edge(backedges, ins, invokeTypes, caller);
    }
    jl_array_del_end(backedges, n - ins);
}

static void jl_prune_tn_backedges(jl_array_t *backedges)
{
    size_t i = 0, ins = 0, n = jl_array_nrows(backedges);
    for (i = 1; i < n; i += 2) {
        jl_value_t *ci = jl_array_ptr_ref(backedges, i);
        if (ptrhash_get(&serialization_order, ci) != HT_NOTFOUND) {
            jl_array_ptr_set(backedges, ins++, jl_array_ptr_ref(backedges, i - 1));
            jl_array_ptr_set(backedges, ins++, ci);
        }
    }
    jl_array_del_end(backedges, n - ins);
}

static void jl_prune_mt_backedges(jl_genericmemory_t *allbackedges)
{
    for (size_t i = 0, n = allbackedges->length; i < n; i += 2) {
        jl_value_t *tn = jl_genericmemory_ptr_ref(allbackedges, i);
        jl_value_t *backedges = jl_genericmemory_ptr_ref(allbackedges, i + 1);
        if (tn && tn != jl_nothing && backedges)
            jl_prune_tn_backedges((jl_array_t*)backedges);
    }
}

static void jl_prune_binding_backedges(jl_array_t *backedges)
{
    if (backedges == NULL)
        return;
    size_t i = 0, ins = 0, n = jl_array_nrows(backedges);
    for (i = 0; i < n; i++) {
        jl_value_t *b = jl_array_ptr_ref(backedges, i);
        if (ptrhash_get(&serialization_order, b) != HT_NOTFOUND) {
            jl_array_ptr_set(backedges, ins, b);
            ins++;
        }
    }
    jl_array_del_end(backedges, n - ins);
}

uint_t bindingkey_hash(size_t idx, jl_value_t *data);
uint_t speccache_hash(size_t idx, jl_value_t *data);

static void jl_prune_idset(_Atomic(jl_svec_t*) *pkeys, _Atomic(jl_genericmemory_t*) *pkeyset, uint_t (*key_hash)(size_t, jl_value_t*), jl_value_t *parent) JL_GC_DISABLED
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
        jl_smallintset_insert(&keyset2, parent, key_hash, i, (jl_value_t*)keys2);
    }
    void *idx = ptrhash_get(&serialization_order, keys);
    assert(idx != HT_NOTFOUND && idx != (void*)(uintptr_t)-1);
    assert(serialization_queue.items[(char*)idx - 1 - (char*)HT_NOTFOUND] == keys);
    ptrhash_put(&serialization_order, keys2, idx);
    serialization_queue.items[(char*)idx - 1 - (char*)HT_NOTFOUND] = keys2;

    idx = ptrhash_get(&serialization_order, keyset);
    assert(idx != HT_NOTFOUND && idx != (void*)(uintptr_t)-1);
    assert(serialization_queue.items[(char*)idx - 1 - (char*)HT_NOTFOUND] == keyset);
    ptrhash_put(&serialization_order, jl_atomic_load_relaxed(&keyset2), idx);
    serialization_queue.items[(char*)idx - 1 - (char*)HT_NOTFOUND] = jl_atomic_load_relaxed(&keyset2);
    jl_gc_write_atomic(parent, *pkeys, jl_svec_t, keys2, relaxed);
    jl_gc_write_atomic(parent, *pkeyset, jl_genericmemory_t, jl_atomic_load_relaxed(&keyset2), relaxed);
}

static void jl_prune_method_specializations(jl_method_t *m) JL_GC_DISABLED
{
    jl_value_t *specializations_ = jl_atomic_load_relaxed(&m->specializations);
    if (!jl_is_svec(specializations_)) {
        if (ptrhash_get(&serialization_order, specializations_) == HT_NOTFOUND)
            record_field_change((jl_value_t **)&m->specializations, (jl_value_t*)jl_emptysvec);
        return;
    }
    jl_prune_idset((_Atomic(jl_svec_t*)*)&m->specializations, &m->speckeyset, speccache_hash, (jl_value_t*)m);
}

static void jl_prune_module_bindings(jl_module_t *m) JL_GC_DISABLED
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

static jl_value_t *strip_codeinfo_meta(jl_method_t *m, jl_value_t *ci_, jl_code_instance_t *codeinst)
{
    jl_code_info_t *ci = NULL;
    JL_GC_PUSH1(&ci);
    int compressed = 0;
    if (!jl_is_code_info(ci_)) {
        compressed = 1;
        ci = jl_uncompress_ir(m, codeinst, (jl_value_t*)ci_);
    }
    else {
        ci = (jl_code_info_t*)ci_;
    }
    strip_slotnames(ci->slotnames, jl_array_len(ci->slotnames));
    jl_gc_write(ci, ci->debuginfo, jl_debuginfo_t, jl_nulldebuginfo);
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
        if (inferred && inferred != jl_nothing && !jl_is_uint8(inferred)) {
            if (jl_options.strip_ir) {
                record_field_change((jl_value_t**)&codeinst->inferred, jl_nothing);
            }
            else if (jl_options.strip_metadata) {
                jl_value_t *stripped = strip_codeinfo_meta(mi->def.method, inferred, codeinst);
                jl_gc_wb(codeinst, stripped);
                jl_atomic_cmpswap_relaxed(&codeinst->inferred, &inferred, stripped);
            }
        }
        if (jl_options.strip_ir)
            record_field_change((jl_value_t**)&codeinst->edges, (jl_value_t*)jl_emptysvec);
        if (jl_options.strip_metadata)
            record_field_change((jl_value_t**)&codeinst->debuginfo, (jl_value_t*)jl_nulldebuginfo);
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    if (jl_options.trim || jl_options.strip_ir) {
        record_field_change((jl_value_t**)&mi->backedges, NULL);
    }
}

static int strip_all_codeinfos__(jl_typemap_entry_t *def, void *_env)
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
                record_field_change(&m->source, jl_nothing);
                record_field_change((jl_value_t**)&m->roots, NULL);
                stripped_ir = 1;
            }
        }
        if (jl_options.strip_metadata) {
            if (!stripped_ir) {
                jl_gc_write(m, m->source, jl_value_t, strip_codeinfo_meta(m, m->source, NULL));
            }
            jl_array_t *slotnames = jl_uncompress_argnames(m->slot_syms);
            JL_GC_PUSH1(&slotnames);
            int tostrip = jl_array_len(slotnames);
            // for keyword methods, strip only nargs to keep the keyword names at the end for reflection
            if (jl_tparam0(jl_unwrap_unionall(m->sig)) == (jl_value_t*)jl_kwcall_type)
                tostrip = m->nargs;
            strip_slotnames(slotnames, tostrip);
            jl_gc_write(m, m->slot_syms, jl_value_t, jl_compress_argnames(slotnames));
            JL_GC_POP();
        }
    }
    if (jl_options.strip_metadata) {
        record_field_change((jl_value_t**)&m->file, (jl_value_t*)jl_empty_sym);
        m->line = 0;
        record_field_change((jl_value_t**)&m->debuginfo, (jl_value_t*)jl_nulldebuginfo);
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
        record_field_change((jl_value_t**)&m->root_blocks, NULL);
    return 1;
}

static int strip_all_codeinfos_mt(jl_methtable_t *mt, void *_env)
{
    return jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), strip_all_codeinfos__, NULL);
}

static void jl_strip_all_codeinfos(jl_array_t *mod_array)
{
    jl_foreach_reachable_mtable(strip_all_codeinfos_mt, mod_array, NULL);
}

static int strip_module(jl_module_t *m, jl_sym_t *docmeta_sym)
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
                record_field_change((jl_value_t**)&b->value, jl_nothing);
            // TODO: this is a pretty stupidly unsound way to do this, but it is way to late here to do this correctly (by calling delete_binding and getting an updated world age then dropping all partitions from older worlds)
            jl_binding_partition_t *bp = jl_atomic_load_relaxed(&b->partitions);
            while (bp) {
                if (jl_bkind_is_defined_constant(jl_binding_kind(bp))) {
                    // XXX: bp->kind = PARTITION_KIND_UNDEF_CONST;
                    record_field_change((jl_value_t**)&bp->restriction, NULL);
                }
                bp = jl_atomic_load_relaxed(&bp->next);
            }
        }
    }
    return 1;
}


static void jl_strip_all_docmeta(jl_array_t *mod_array)
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
jl_mutex_t global_roots_lock;

jl_mutex_t precompile_field_replace_lock;
jl_svec_t *precompile_field_replace JL_GLOBALLY_ROOTED;

static inline jl_value_t *get_checked_fieldindex(const char *name, jl_datatype_t *st, jl_value_t *v, jl_value_t *arg, int mutabl)
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
        jl_global_roots_list = jl_idset_put_key(jl_global_roots_list, val, &idx);
        jl_global_roots_keyset = jl_idset_put_idx(jl_global_roots_list, jl_global_roots_keyset, idx);
    }
    else {
        val = NULL;
    }
    JL_UNLOCK(&global_roots_lock);
    return val;
}

// In addition to the system image (where `worklist = NULL`), this can also save incremental images with external linkage
static void jl_save_system_image_to_stream(ios_t *f, jl_array_t *mod_array,
                                           jl_array_t *module_init_order, jl_array_t *worklist, jl_array_t *extext_methods,
                                           jl_array_t *new_ext, jl_query_cache *query_cache)
{
    htable_new(&field_replace, 0);
    htable_new(&bits_replace, 0);
    // strip metadata and IR when requested
    if (jl_options.strip_metadata || jl_options.strip_ir) {
        if (jl_options.strip_metadata) {
            jl_nulldebuginfo = (jl_debuginfo_t*)jl_get_global(jl_core_module, jl_symbol("NullDebugInfo"));
            if (jl_nulldebuginfo == NULL)
                jl_errorf("Core.NullDebugInfo required for --strip-metadata option");
        }
        jl_strip_all_codeinfos(mod_array);
        jl_strip_all_docmeta(mod_array);
    }
    // collect needed methods and replace method tables that are in the tags array
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
                record_field_change((jl_value_t**)fldaddr, newval);
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
                    record_field_change((jl_value_t**)fldaddr + ptr, *(((jl_value_t**)newval) + ptr));
                }
            }
        }
    }

    int en = jl_gc_enable(0);
    if (native_functions) {
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

    nsym_tag = 0;
    htable_new(&symbol_table, 0);
    htable_new(&fptr_to_id, jl_n_builtins);
    uintptr_t i;
    for (i = 0; i < jl_n_builtins; i++) {
        ptrhash_put(&fptr_to_id, (void*)(uintptr_t)jl_builtin_f_addrs[i], (void*)(i + 2));
    }
    htable_new(&serialization_order, 25000);
    htable_new(&nullptrs, 0);
    arraylist_new(&object_worklist, 0);
    arraylist_new(&deferred_supers, 0);
    arraylist_new(&serialization_queue, 0);
    ios_t sysimg, const_data, symbols, relocs, gvar_record, fptr_record;
    ios_mem(&sysimg, 0);
    ios_mem(&const_data, 0);
    ios_mem(&symbols, 0);
    ios_mem(&relocs, 0);
    ios_mem(&gvar_record, 0);
    ios_mem(&fptr_record, 0);
    jl_serializer_state s = {0};
    jl_value_t *sig_tn_root = NULL;
    s.query_cache = query_cache;
    s.incremental = !(worklist == NULL);
    s.s = &sysimg;
    s.const_data = &const_data;
    s.symbols = &symbols;
    s.relocs = &relocs;
    s.gvar_record = &gvar_record;
    s.fptr_record = &fptr_record;
    s.ptls = jl_current_task->ptls;
    arraylist_new(&s.memowner_list, 0);
    arraylist_new(&s.memref_list, 0);
    arraylist_new(&s.relocs_list, 0);
    arraylist_new(&s.gctags_list, 0);
    arraylist_new(&s.uniquing_types, 0);
    arraylist_new(&s.uniquing_super, 0);
    arraylist_new(&s.uniquing_objs, 0);
    arraylist_new(&s.fixup_types, 0);
    arraylist_new(&s.fixup_objs, 0);
    s.buildid_depmods_idxs = image_to_depmodidx(mod_array);
    s.link_ids_relocs = jl_alloc_array_1d(jl_array_int32_type, 0);
    s.link_ids_gctags = jl_alloc_array_1d(jl_array_int32_type, 0);
    s.link_ids_gvars = jl_alloc_array_1d(jl_array_int32_type, 0);
    s.link_ids_external_fnvars = jl_alloc_array_1d(jl_array_int32_type, 0);
    s.method_roots_list = NULL;
    htable_new(&s.method_roots_index, 0);
    jl_value_t **_tags[NUM_TAGS];
    jl_value_t ***tags = s.incremental ? NULL : _tags;
    if (worklist) {
        s.method_roots_list = jl_alloc_vec_any(0);
        s.worklist_key = jl_worklist_key(worklist);
    }
    else {
        get_tags(_tags);
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
            for (i = 0; tags[i] != NULL; i++) {
                jl_value_t *tag = *tags[i];
                jl_queue_for_serialization(&s, tag);
            }
            for (i = 0; i < jl_n_builtins; i++)
                jl_queue_for_serialization(&s, jl_builtin_instances[i]);
#define XX(name, type) jl_queue_for_serialization(&s, (jl_value_t*)jl_##name);
            JL_EXPORTED_DATA_POINTERS(XX)
#undef XX
#define XX(name, type) jl_queue_for_serialization(&s, (jl_value_t*)jl_##name);
            JL_CONST_GLOBAL_VARS(XX)
#undef XX
            jl_queue_for_serialization(&s, s.ptls->root_task->tls);
        }
        else {
            // Queue the worklist itself as the first item we serialize
            reloc_tag_root((jl_value_t*)worklist, 0);
            reloc_tag_root((jl_value_t*)module_init_order, 1);
            jl_queue_for_serialization(&s, worklist);
            jl_queue_for_serialization(&s, module_init_order);
        }
        // step 1.1: as needed, serialize the data needed for insertion into the running system
        if (extext_methods) {
            // sort by first-argument typename so consecutive load-time
            // insertions and activations share hot typemap paths; the array
            // holds (method, activation certificate) pairs
            size_t nex = jl_array_nrows(extext_methods);
            if (nex > 3)
                qsort(jl_array_ptr_data(extext_methods), nex / 2, 2 * sizeof(void*), extext_method_cmp);
            // Queue method extensions
            reloc_tag_root((jl_value_t*)extext_methods, 2);
            jl_queue_for_serialization(&s, extext_methods);
            // Queue the new specializations
            reloc_tag_root((jl_value_t*)new_ext, 3);
            jl_queue_for_serialization(&s, new_ext);
        }
        jl_serialize_reachable(&s);
        if (worklist && jl_backedge_log) {
            // Prune the backedge registration log to callers that are being
            // serialized anyway (still valid and reachable): the log must not
            // root otherwise-dead or dropped CodeInstances into the image.
            jl_array_t *belog = jl_backedge_log;
            size_t bn = jl_array_nrows(belog), ins = 0;
            jl_value_t **bd = jl_array_ptr_data(belog);
            for (size_t i = 0; i + 2 < bn; i += 3) {
                jl_code_instance_t *caller = (jl_code_instance_t*)bd[i + 2];
                if (jl_atomic_load_relaxed(&caller->max_world) != ~(size_t)0)
                    continue;
                if (ptrhash_get(&serialization_order, caller) == HT_NOTFOUND)
                    continue;
                bd[ins] = bd[i];
                bd[ins + 1] = bd[i + 1];
                bd[ins + 2] = bd[i + 2];
                ins += 3;
            }
            if (ins < bn)
                jl_array_del_end(belog, bn - ins);
            // Compact the serialized form: intern each referenced object once
            // and store the triples as a varint index stream. The naive
            // 3-pointers-per-entry form was ~22% of all image relocations.
            {
                htable_t uniq;
                htable_new(&uniq, 1024);
                jl_array_t *uobjs = jl_alloc_vec_any(0);
                jl_array_t *idxbytes = NULL;
                JL_GC_PUSH2(&uobjs, &idxbytes);
                idxbytes = jl_alloc_array_1d(jl_array_uint8_type, 0);
                size_t nlog = jl_array_nrows(belog);
                size_t ntrip = nlog / 3;
                // group triples by target (then invokesig) so the loader can
                // lock and pre-size once per callee and decompose each
                // method-table signature once per run; registration order is
                // not semantically significant. Stream form: per group,
                // varint(target) varint(count), then count x (varint invokesig,
                // varint caller).
                belog_trip_t *trips = (belog_trip_t*)malloc_s((ntrip ? ntrip : 1) * sizeof(belog_trip_t));
                for (size_t i = 0; i < ntrip; i++) {
                    jl_value_t *t = jl_array_ptr_ref(belog, 3 * i);
                    trips[i].target = t;
                    trips[i].invokesig = jl_array_ptr_ref(belog, 3 * i + 1);
                    trips[i].cls = t == jl_nothing ? 1 : jl_is_method_instance(t) ? 0 : 2;
                    trips[i].orig = (uint32_t)i;
                }
                qsort(trips, ntrip, sizeof(belog_trip_t), belog_trip_cmp);
#define BELOG_UNIQ(o, out) do { \
                    void **ubp_ = ptrhash_bp(&uniq, (void*)(o)); \
                    if (*ubp_ == HT_NOTFOUND) { \
                        (out) = jl_array_nrows(uobjs); \
                        *ubp_ = (void*)((out) + 1); \
                        jl_array_ptr_1d_push(uobjs, (o)); \
                    } \
                    else { \
                        (out) = (size_t)(uintptr_t)*ubp_ - 1; \
                    } \
                } while (0)
#define BELOG_EMIT(v) do { \
                    size_t v_ = (v); \
                    uint8_t vbuf_[10]; \
                    int nb_ = 0; \
                    do { \
                        vbuf_[nb_] = v_ & 0x7f; \
                        v_ >>= 7; \
                        if (v_) \
                            vbuf_[nb_] |= 0x80; \
                        nb_++; \
                    } while (v_); \
                    size_t pos_ = jl_array_nrows(idxbytes); \
                    jl_array_grow_end(idxbytes, nb_); \
                    memcpy(jl_array_data(idxbytes, uint8_t) + pos_, vbuf_, nb_); \
                } while (0)
                for (size_t g = 0; g < ntrip; ) {
                    size_t e = g + 1;
                    while (e < ntrip && trips[e].target == trips[g].target)
                        e++;
                    size_t idx;
                    BELOG_UNIQ(trips[g].target, idx);
                    BELOG_EMIT(idx);
                    BELOG_EMIT(e - g);
                    for (size_t i = g; i < e; i++) {
                        BELOG_UNIQ(trips[i].invokesig, idx);
                        BELOG_EMIT(idx);
                        jl_value_t *caller = jl_array_ptr_ref(belog, 3 * (size_t)trips[i].orig + 2);
                        BELOG_UNIQ(caller, idx);
                        BELOG_EMIT(idx);
                    }
                    g = e;
                }
#undef BELOG_EMIT
#undef BELOG_UNIQ
                free(trips);
                htable_free(&uniq);
                jl_array_t *compact = jl_alloc_vec_any(2);
                jl_array_ptr_set(compact, 0, (jl_value_t*)uobjs);
                jl_array_ptr_set(compact, 1, (jl_value_t*)idxbytes);
                jl_backedge_log = compact; // rooted global; recording is done
                belog = compact;
                JL_GC_POP();
            }
            reloc_tag_root((jl_value_t*)belog, 4);
            jl_queue_for_serialization(&s, (jl_value_t*)belog);
            jl_serialize_reachable(&s);
        }
        if (worklist && edge_sig_set_init && edge_sig_set.count > 0) {
            // serialize the typename decompositions of every collected call
            // and method signature: the loader consumes them in place of
            // jl_foreach_top_typename_for (contents ride as ordinary pointer
            // arrays so relocation and uniquing canonicalize the typenames)
            jl_array_t *sigtab = jl_alloc_vec_any(0);
            jl_svec_t *tnsv = NULL;
            JL_GC_PUSH2(&sigtab, &tnsv);
            void **tab = edge_sig_set.table;
            for (size_t i = 0; i < edge_sig_set.size; i += 2) {
                if (tab[i + 1] == HT_NOTFOUND)
                    continue;
                jl_value_t *sig = (jl_value_t*)tab[i];
                struct _sig_tn_collect env = { {NULL}, 0, 0, 0 };
                if (!jl_foreach_top_typename_for(_sig_tn_collect_cb, sig, 1, &env))
                    continue; // not decomposable: loader falls back
                if (env.overflow)
                    continue;
                tnsv = jl_alloc_svec(1 + env.n);
                jl_svecset(tnsv, 0, jl_box_long((int64_t)env.explbits));
                for (int k = 0; k < env.n; k++)
                    jl_svecset(tnsv, 1 + k, env.tns[k]);
                jl_array_ptr_1d_push(sigtab, sig);
                jl_array_ptr_1d_push(sigtab, (jl_value_t*)tnsv);
            }
            JL_GC_POP();
            sig_tn_root = (jl_value_t*)sigtab;
            reloc_tag_root(sig_tn_root, 6);
            jl_queue_for_serialization(&s, sig_tn_root);
            jl_serialize_reachable(&s);
        }
        else if (worklist) {
            jl_queue_for_serialization(&s, jl_nothing);
        }
        // step 1.2: ensure all gvars are part of the sysimage too
        record_gvars(&s, &gvars);
        record_external_fns(&s, &external_fns);
        if (jl_options.trim)
            record_gvars(&s, &MIs);
        jl_serialize_reachable(&s);
        // Beyond this point, all content should already have been visited, so now we can prune
        // the rest and add some internal root arrays.
        // step 1.3: include some other special roots
        if (s.incremental) {
            // Queue the new roots array
            reloc_tag_root((jl_value_t*)s.method_roots_list, 5);
            jl_queue_for_serialization(&s, s.method_roots_list);
            jl_serialize_reachable(&s);
        }
        // step 1.4: prune (garbage collect) special weak references from the jl_global_roots_list
        if (worklist == NULL) {
            global_roots_list = jl_alloc_memory_any(0);
            global_roots_keyset = jl_alloc_memory_any(0);
            for (size_t i = 0; i < jl_global_roots_list->length; i++) {
                jl_value_t *val = jl_genericmemory_ptr_ref(jl_global_roots_list, i);
                if (val && ptrhash_get(&serialization_order, val) != HT_NOTFOUND) {
                    ssize_t idx;
                    global_roots_list = jl_idset_put_key(global_roots_list, val, &idx);
                    global_roots_keyset = jl_idset_put_idx(global_roots_list, global_roots_keyset, idx);
                }
            }
            jl_queue_for_serialization(&s, global_roots_list);
            jl_queue_for_serialization(&s, global_roots_keyset);
            jl_serialize_reachable(&s);
        }
        // step 1.5: prune (garbage collect) some special weak references known caches
        for (i = 0; i < serialization_queue.len; i++) {
            jl_value_t *v = (jl_value_t*)serialization_queue.items[i];
            if (jl_is_method(v)) {
                if (jl_options.trim)
                    jl_prune_method_specializations((jl_method_t*)v);
            }
            else if (jl_is_module(v)) {
                if (jl_options.trim)
                    jl_prune_module_bindings((jl_module_t*)v);
            }
            else if (jl_is_typename(v)) {
                jl_typename_t *tn = (jl_typename_t*)v;
                jl_gc_write_atomic(tn, tn->cache, jl_svec_t,
                    jl_prune_type_cache_hash(jl_atomic_load_relaxed(&tn->cache)), relaxed);
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
    }

    uint32_t external_fns_begin = 0;
    { // step 2: build all the sysimg sections
        write_padding(&sysimg, sizeof(uintptr_t));
        jl_write_values(&s);
        if (ici_fixups_init) {
            // resolve self-referential InternedCodeInstance words now that the
            // layout is final: offsets from the image data base, in units
            for (size_t i = 0; i < ici_fixups.len; i += 3) {
                jl_value_t *ici = (jl_value_t*)ici_fixups.items[i];
                size_t wordidx = (size_t)ici_fixups.items[i + 1];
                jl_value_t *target = (jl_value_t*)ici_fixups.items[i + 2];
                uintptr_t ici_reloc = get_reloc_for_item(backref_id(&s, ici, NULL), 0);
                uintptr_t tgt_reloc = get_reloc_for_item(backref_id(&s, target, NULL), 0);
                assert((ici_reloc >> RELOC_TAG_OFFSET) == DataRef);
                uintptr_t mask = (((uintptr_t)1 << RELOC_TAG_OFFSET) - 1);
                uintptr_t word;
                if ((ici_reloc >> RELOC_TAG_OFFSET) != DataRef)
                    jl_safe_printf("ICI FIXUP: container reloc tag %d\n", (int)(ici_reloc >> RELOC_TAG_OFFSET));
                if ((tgt_reloc >> RELOC_TAG_OFFSET) == DataRef) {
                    word = ICI_TAG_REF | ((tgt_reloc & mask) / SYS_EXTERNAL_LINK_UNIT); // imagekey 0 == self
                }
                else if ((tgt_reloc >> RELOC_TAG_OFFSET) == ConstDataRef) {
                    word = ICI_TAG_REF | ICI_TAG_CONST | (tgt_reloc & mask); // already in pointer units
                }
                else {
                    void *ord = ptrhash_get(&serialization_order, target);
                    void *canon = jl_is_svec(target) ? (void*)svec_dedup_canonical(&s, target) : (void*)target;
                    void *ordc = ptrhash_get(&serialization_order, canon);
                    jl_safe_printf("ICI FIXUP: tag %d type %s wordidx %zd ord=%p canon%s ordc=%p inimg=%d\n",
                                   (int)(tgt_reloc >> RELOC_TAG_OFFSET),
                                   jl_symbol_name(((jl_datatype_t*)jl_typeof(target))->name->name),
                                   wordidx, ord, canon == (void*)target ? "=self" : "!=self", ordc,
                                   jl_object_in_image(target));
                    word = 0;
                }
                size_t wordpos = (ici_reloc & mask) +
                    (wordidx == ICI_DEFWORD_IDX ? offsetof(jl_interned_code_instance_t, defword)
                                                : sizeof(jl_interned_code_instance_t) + sizeof(uintptr_t) * wordidx);
                if (wordpos + sizeof(word) > (size_t)s.s->size)
                    jl_safe_printf("ICI FIXUP OOB: wordpos %zx size %zx wordidx %zd\n",
                                   wordpos, (size_t)s.s->size, wordidx);
                else
                    memcpy(&s.s->buf[wordpos], &word, sizeof(word));
            }
            arraylist_free(&ici_fixups);
            ici_fixups_init = 0;
        }
        if (svec_dedup_init) {
            // freed only after the fixup pass: resolving fixup targets
            // re-canonicalizes svecs and must see the same tables
            htable_free(&svec_dedup_memo);
            htable_free(&svec_dedup_byhash);
            svec_dedup_init = 0;
        }
        if (depsym_map_init) {
            htable_free(&depsym_map);
            depsym_map_init = 0;
        }
        if (edge_sig_set_init) {
            htable_free(&edge_sig_set);
            edge_sig_set_init = 0;
        }
        external_fns_begin = write_gvars(&s, &gvars, &external_fns);
    }

    // This ensures that we can use the low bit of addresses for
    // identifying end pointers in gc's eytzinger search.
    write_padding(&sysimg, 4 - (sysimg.size % 4));
    write_padding(&const_data, 4 - (const_data.size % 4));

    if (sysimg.size > ((uintptr_t)1 << RELOC_TAG_OFFSET)) {
        jl_printf(
            JL_STDERR,
            "ERROR: system image too large: sysimg.size is 0x%" PRIxPTR " but the limit is 0x%" PRIxPTR "\n",
            (uintptr_t)sysimg.size,
            ((uintptr_t)1 << RELOC_TAG_OFFSET)
        );
        jl_exit(1);
    }
    if (const_data.size / sizeof(void*) > ((uintptr_t)1 << RELOC_TAG_OFFSET)) {
        jl_printf(
            JL_STDERR,
            "ERROR: system image too large: const_data.size is 0x%" PRIxPTR " but the limit is 0x%" PRIxPTR "\n",
            (uintptr_t)const_data.size,
            ((uintptr_t)1 << RELOC_TAG_OFFSET)*sizeof(void*)
        );
        jl_exit(1);
    }

    // step 3: combine all of the sections into one file
    assert(ios_pos(f) % JL_CACHE_BYTE_ALIGNMENT == 0);
    ssize_t sysimg_offset = ios_pos(f);
    write_uint(f, sysimg.size - sizeof(uintptr_t));
    ios_seek(&sysimg, sizeof(uintptr_t));
    ios_copyall(f, &sysimg);
    size_t sysimg_size = s.s->size;
    assert(ios_pos(f) - sysimg_offset == sysimg_size);
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

    // Prepare and write the relocations sections, now that the rest of the image is laid out
    char *base = &f->buf[0];
    jl_finish_relocs(base + sysimg_offset, sysimg_size, &s.gctags_list);
    jl_finish_relocs(base + sysimg_offset, sysimg_size, &s.relocs_list);
    jl_write_offsetlist(s.relocs, sysimg_size, &s.gctags_list);
    jl_write_offsetlist(s.relocs, sysimg_size, &s.relocs_list);
    jl_write_offsetlist(s.relocs, sysimg_size, &s.memowner_list);
    jl_write_offsetlist(s.relocs, sysimg_size, &s.memref_list);
    if (s.incremental) {
        jl_write_arraylist(s.relocs, &s.uniquing_types);
        jl_write_arraylist(s.relocs, &s.uniquing_objs);
        jl_write_arraylist(s.relocs, &s.fixup_types);
    }
    jl_write_arraylist(s.relocs, &s.fixup_objs);
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
            for (i = 0; i < jl_n_builtins; i++)
                jl_write_value(&s, jl_builtin_instances[i]);
#define XX(name, type) jl_write_value(&s, (jl_value_t*)jl_##name);
            JL_EXPORTED_DATA_POINTERS(XX)
#undef XX
#define XX(name, type) jl_write_value(&s, (jl_value_t*)jl_##name);
            JL_CONST_GLOBAL_VARS(XX)
#undef XX
            jl_write_value(&s, global_roots_list);
            jl_write_value(&s, global_roots_keyset);
            jl_write_value(&s, s.ptls->root_task->tls);
            write_uint32(f, jl_get_gs_ctr());
            size_t world = jl_atomic_load_acquire(&jl_world_counter);
            // assert(world == precompilation_world); // This triggers on a normal build of julia
            write_uint(f, world);
            write_uint(f, jl_typeinf_world);
        }
        else {
            jl_write_value(&s, worklist);
            // save module initialization order
            size_t i, l = jl_array_len(module_init_order);
            for (i = 0; i < l; i++) {
                // verify that all these modules were saved
                assert(ptrhash_get(&serialization_order, jl_array_ptr_ref(module_init_order, i)) != HT_NOTFOUND);
            }
            jl_write_value(&s, module_init_order);
            jl_write_value(&s, extext_methods);
            jl_write_value(&s, new_ext);
            jl_write_value(&s, jl_backedge_log ? (jl_value_t*)jl_backedge_log : jl_nothing);
            jl_write_value(&s, s.method_roots_list);
            jl_write_value(&s, sig_tn_root ? sig_tn_root : jl_nothing);
        }
        write_uint32(f, jl_array_len(s.link_ids_gctags));
        ios_write(f, (char*)jl_array_data(s.link_ids_gctags, uint32_t), jl_array_len(s.link_ids_gctags) * sizeof(uint32_t));
        write_uint32(f, jl_array_len(s.link_ids_relocs));
        ios_write(f, (char*)jl_array_data(s.link_ids_relocs, uint32_t), jl_array_len(s.link_ids_relocs) * sizeof(uint32_t));
        write_uint32(f, jl_array_len(s.link_ids_gvars));
        ios_write(f, (char*)jl_array_data(s.link_ids_gvars, uint32_t), jl_array_len(s.link_ids_gvars) * sizeof(uint32_t));
        write_uint32(f, jl_array_len(s.link_ids_external_fnvars));
        ios_write(f, (char*)jl_array_data(s.link_ids_external_fnvars, uint32_t), jl_array_len(s.link_ids_external_fnvars) * sizeof(uint32_t));
        write_uint32(f, external_fns_begin);
    }

    assert(object_worklist.len == 0);
    arraylist_free(&object_worklist);
    assert(deferred_supers.len == 0);
    arraylist_free(&deferred_supers);
    arraylist_free(&serialization_queue);
    arraylist_free(&layout_table);
    arraylist_free(&s.uniquing_types);
    arraylist_free(&s.uniquing_super);
    arraylist_free(&s.uniquing_objs);
    arraylist_free(&s.fixup_types);
    arraylist_free(&s.fixup_objs);
    arraylist_free(&s.memowner_list);
    arraylist_free(&s.memref_list);
    arraylist_free(&s.relocs_list);
    if (reloc_stats_wanted()) {
        const char *pkgname = "sysimage";
        if (worklist && jl_array_nrows(worklist) > 0) {
            jl_module_t *topmod = (jl_module_t*)jl_array_ptr_ref(worklist, jl_array_nrows(worklist) - 1);
            pkgname = jl_symbol_name(topmod->name);
        }
        dump_reloc_stats(pkgname);
        dump_heap_stats(pkgname);
    }
    arraylist_free(&s.gctags_list);
    arraylist_free(&gvars);
    arraylist_free(&external_fns);
    htable_free(&s.method_roots_index);
    htable_free(&field_replace);
    htable_free(&bits_replace);
    htable_free(&serialization_order);
    htable_free(&nullptrs);
    htable_free(&symbol_table);
    htable_free(&fptr_to_id);
    htable_free(&new_methtables);
    nsym_tag = 0;

    jl_gc_enable(en);
}

static int ci_not_internal_cache(jl_code_instance_t *ci)
{
    jl_method_instance_t *mi = jl_get_ci_mi(ci);
    return !(jl_atomic_load_relaxed(&ci->flags) & JL_CI_FLAGS_NATIVE_CACHE_VALID) || jl_object_in_image(mi->def.value);
}

static uint8_t jl_get_toplevel_syntax_version(void)
{
    jl_task_t *ct = jl_current_task;
    jl_module_t *toplevel = (jl_module_t*)jl_get_global_value(jl_base_module, jl_symbol("__toplevel__"), ct->world_age);
    JL_GC_PROMISE_ROOTED(toplevel);
    jl_value_t *syntax_version = jl_get_global_value(toplevel, jl_symbol("_internal_syntax_version"), ct->world_age);
    return jl_unbox_uint8(syntax_version);
}

static void jl_write_header_for_incremental(ios_t *f, jl_array_t *worklist, jl_array_t *mod_array, jl_array_t **udeps, int64_t *srctextpos, int64_t *checksumpos)
{
    *checksumpos = write_header(f, 0);
    write_uint8(f, jl_cache_flags());
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
    // last word of the dependency list is the end of the data / start of the srctextpos
    *srctextpos = write_dependency_list(f, worklist, udeps);  // srctextpos: position of srctext entry in header index (update later)
    // write description of requirements for loading (modules that must be pre-loaded if initialization is to succeed)
    // this can return errors during deserialize,
    // best to keep it early (before any actual initialization)
    write_mod_list(f, mod_array);
}

JL_DLLEXPORT void jl_create_system_image(void **_native_data, jl_array_t *worklist, bool_t emit_split,
                                         ios_t **s, ios_t **z, jl_array_t **udeps JL_REQUIRE_ROOTED_SLOT, int64_t *srctextpos, jl_array_t *module_init_order)
{
    JL_TIMING(SYSIMG_DUMP, SYSIMG_DUMP);

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

    jl_array_t *mod_array = NULL, *extext_methods = NULL, *new_ext = NULL, *ext_foreign_cis = NULL;
    int64_t checksumpos = 0;
    int64_t checksumpos_ff = 0;
    int64_t datastartpos = 0;
    JL_GC_PUSH4(&mod_array, &extext_methods, &new_ext, &ext_foreign_cis);

    ext_foreign_cis = jl_alloc_vec_any(0);

    mod_array = jl_get_loaded_modules();  // __toplevel__ modules loaded in this session (from Base.loaded_modules_array)
    if (worklist) {
        if (_native_data != NULL) {
            if (suppress_precompile)
                newly_inferred = NULL;
            *_native_data = jl_create_native(NULL, 0, 1, jl_atomic_load_acquire(&jl_world_counter), NULL, suppress_precompile ? (jl_array_t*)jl_an_empty_vec_any : worklist, 0, module_init_order, ext_foreign_cis);
        }
        jl_write_header_for_incremental(f, worklist, mod_array, udeps, srctextpos, &checksumpos);
        if (emit_split) {
            checksumpos_ff = write_header(ff, 1);
            write_uint8(ff, jl_cache_flags());
            write_uint8(ff, jl_get_toplevel_syntax_version());
            write_mod_list(ff, mod_array);
        }
        else {
            checksumpos_ff = checksumpos;
        }
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
        // extext_methods: [method1, ...], worklist-owned "extending external" methods added to functions owned by modules outside the worklist
        extext_methods = jl_alloc_vec_any(0);
        jl_collect_extext_methods(extext_methods, mod_array);

        if (!emit_split) {
            write_int32(f, 0); // No clone_targets
            write_padding(f, LLT_ALIGN(ios_pos(f), JL_CACHE_BYTE_ALIGNMENT) - ios_pos(f));
        }
        else {
            write_padding(ff, LLT_ALIGN(ios_pos(ff), JL_CACHE_BYTE_ALIGNMENT) - ios_pos(ff));
        }
        datastartpos = ios_pos(ff);
    }

    jl_query_cache query_cache;
    init_query_cache(&query_cache);
    jl_finalize_precompile_inferred(worklist != NULL && _native_data != NULL && jl_options.outputo != NULL);
    jl_save_system_image_to_stream(ff, mod_array, module_init_order, worklist, extext_methods, new_ext, &query_cache);
    if (_native_data != NULL)
        native_functions = NULL;
    // make sure we don't run any Julia code concurrently before this point
    // Re-enable running julia code for postoutput hooks, atexit, etc.
    jl_gc_enable_finalizers(ct, 1);
    ct->reentrant_timing &= ~0b1000u;

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

    destroy_query_cache(&query_cache);

    JL_GC_POP();
    *s = f;
    if (emit_split)
        *z = ff;
    return;
}

// Takes in a path of the form "usr/lib/julia/sys.so"
JL_DLLEXPORT jl_image_buf_t jl_preload_sysimg(const char *fname)
{
    if (jl_sysimage_buf.kind != JL_IMAGE_KIND_NONE)
        return jl_sysimage_buf;

    char *dot = (char*) strrchr(fname, '.');
    int is_ji = (dot && !strcmp(dot, ".ji"));

    if (is_ji) {
        // .ji extension => load .ji file only
        ios_t f;

        if (ios_file(&f, fname, 1, 0, 0, 0) == NULL)
            jl_errorf("System image file \"%s\" not found.", fname);
        ios_bufmode(&f, bm_none);

        ios_seek_end(&f);
        size_t len = ios_pos(&f);
        char *sysimg = (char*)jl_gc_perm_alloc(len, 0, 64, 0);
        jl_gc_notify_image_alloc(sysimg, len);
        ios_seek(&f, 0);

        if (ios_readall(&f, sysimg, len) != len)
            jl_errorf("Error reading system image file.");

        ios_close(&f);

        jl_sysimage_buf = (jl_image_buf_t) {
            .kind = JL_IMAGE_KIND_JI,
            .pointers = NULL,
            .data = sysimg,
            .size = len,
            .base = 0,
        };
        return jl_sysimage_buf;
    } else {
        // Get handle to sys.so
        return jl_set_sysimg_so(jl_load_dynamic_library(fname, JL_RTLD_LOCAL | JL_RTLD_NOW, 1));
    }
}


static void jl_prefetch_system_image(const char *data, size_t size)
{
    size_t page_size = jl_getpagesize(); /* jl_page_size is not set yet when loading sysimg */
    void *start = (void *)((uintptr_t)data & ~(page_size - 1));
    size_t size_aligned = LLT_ALIGN(size, page_size);
#ifdef _OS_WINDOWS_
    WIN32_MEMORY_RANGE_ENTRY entry = {start, size_aligned};
    PrefetchVirtualMemory(GetCurrentProcess(), 1, &entry, 0);
#else
    madvise(start, size_aligned, MADV_WILLNEED);
#endif
}

JL_DLLEXPORT void jl_image_unpack_uncomp(void *handle, jl_image_buf_t *image)
{
    size_t *plen;
    uint32_t *pchecksum;
    jl_dlsym(handle, "jl_system_image_size", (void **)&plen, 1, 0);
    jl_dlsym(handle, "jl_system_image_data", (void **)&image->data, 1, 0);
    jl_dlsym(handle, "jl_image_pointers", (void**)&image->pointers, 1, 0);
    jl_dlsym(handle, "jl_system_image_checksum", (void **)&pchecksum, 1, 0);
    image->size = *plen;
    image->checksum = *pchecksum;
    jl_prefetch_system_image(image->data, image->size);
}

JL_DLLEXPORT void jl_image_unpack_zstd(void *handle, jl_image_buf_t *image)
{
    size_t *plen;
    uint32_t *pchecksum;
    const char *data;
    jl_dlsym(handle, "jl_system_image_size", (void **)&plen, 1, 0);
    jl_dlsym(handle, "jl_system_image_data", (void **)&data, 1, 0);
    jl_dlsym(handle, "jl_image_pointers", (void **)&image->pointers, 1, 0);
    jl_dlsym(handle, "jl_system_image_checksum", (void **)&pchecksum, 1, 0);
    image->checksum = *pchecksum;
    jl_prefetch_system_image(data, *plen);
    image->size = ZSTD_getFrameContentSize(data, *plen);
    size_t page_size = jl_getpagesize(); /* jl_page_size is not set yet when loading sysimg */
    size_t aligned_size = LLT_ALIGN(image->size, page_size);
    int fail = 0;
#if defined(_OS_WINDOWS_)
    size_t large_page_size = GetLargePageMinimum();
    image->data = NULL;
    if (large_page_size > 0 && image->size > 4 * large_page_size) {
        size_t aligned_size = LLT_ALIGN(image->size, large_page_size);
        image->data = (char *)VirtualAlloc(
            NULL, aligned_size, MEM_COMMIT | MEM_RESERVE | MEM_LARGE_PAGES, PAGE_READWRITE);
    }
    if (!image->data) {
        /* Try small pages if large pages failed. */
        image->data = (char *)VirtualAlloc(NULL, aligned_size, MEM_COMMIT | MEM_RESERVE,
                                           PAGE_READWRITE);
    }
    fail = !image->data;
#else
    image->data = (char *)mmap(NULL, aligned_size, PROT_READ | PROT_WRITE,
                               MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    fail = image->data == (void *)-1;
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
        jl_printf(JL_STDERR, "ERROR: failed to allocate memory for system image: %s\n",
                  err);
        jl_exit(1);
    }

    ZSTD_decompress((void *)image->data, image->size, data, *plen);
    size_t len = (*plen) & ~(page_size - 1);
#ifdef _OS_WINDOWS_
    if (len)
        VirtualFree((void *)data, len, MEM_RELEASE);
#else
    munmap((void *)data, len);
#endif
}

// From a shared library handle, verify consistency and return a jl_image_buf_t
static jl_image_buf_t get_image_buf(void *handle, int is_pkgimage) JL_NOTSAFEPOINT
{
    // verify that the linker resolved the symbols in this image against ourselves (libjulia-internal)
    typedef void** (JL_NOTSAFEPOINT *jl_RTLD_DEFAULT_handle_func_t)(void);
    jl_RTLD_DEFAULT_handle_func_t get_jl_RTLD_DEFAULT_handle_addr = NULL;
    if (handle != jl_RTLD_DEFAULT_handle) {
        int symbol_found = jl_dlsym(handle, "get_jl_RTLD_DEFAULT_handle_addr", (void **)&get_jl_RTLD_DEFAULT_handle_addr, 0, 0);
        if (!symbol_found || (void*)&jl_RTLD_DEFAULT_handle != (get_jl_RTLD_DEFAULT_handle_addr()))
            jl_error("Image file failed consistency check: maybe opened the wrong version?");
    }

    jl_image_unpack_func_t *unpack;
    jl_image_buf_t image = {
        .kind = JL_IMAGE_KIND_SO,
        .pointers = NULL,
        .data = NULL,
        .size = 0,
        .base = 0,
    };

    // verification passed, lookup the buffer pointers
    if (jl_image_unpack == NULL || is_pkgimage) {
        // in the usual case, the sysimage was not statically linked to libjulia-internal
        // look up the external sysimage symbols via the dynamic linker
        jl_dlsym(handle, "jl_image_unpack", (void **)&unpack, 1, 0);
    }
    else {
        // the sysimage was statically linked directly against libjulia-internal
        // use the internal symbols
        unpack = &jl_image_unpack;
    }
    (*unpack)(handle, &image);

#ifdef _OS_WINDOWS_
    image.base = (intptr_t)handle;
#else
    Dl_info dlinfo;
    if (dladdr((void*)image.pointers, &dlinfo) != 0)
        image.base = (intptr_t)dlinfo.dli_fbase;
    else
        image.base = 0;
#endif

    return image;
}

// Allow passing in a module handle directly, rather than a path
JL_DLLEXPORT jl_image_buf_t jl_set_sysimg_so(void *handle)
{
    if (jl_sysimage_buf.kind != JL_IMAGE_KIND_NONE)
        return jl_sysimage_buf;

    jl_sysimage_buf = get_image_buf(handle, /* is_pkgimage */ 0);
    return jl_sysimage_buf;
}

#ifndef JL_NDEBUG
// skip the performance optimizations of jl_types_equal and just use subtyping directly
// one of these types is invalid - that's why we're doing the recache type operation
// static int jl_invalid_types_equal(jl_datatype_t *a, jl_datatype_t *b)
// {
//     return jl_subtype((jl_value_t*)a, (jl_value_t*)b) && jl_subtype((jl_value_t*)b, (jl_value_t*)a);
// }
#endif

extern void export_jl_small_typeof(void);
extern void export_jl_sysimg_globals(void);

// When an image is loaded with ignore_native, all subsequent image loads must ignore
// native code in the cache-file since we can't gurantuee that there are no call edges
// into the native code of the image. See https://github.com/JuliaLang/julia/pull/52123#issuecomment-1959965395.
int IMAGE_NATIVE_CODE_TAINTED = 0;

// TODO: This should possibly be in Julia
static int jl_validate_binding_partition(jl_binding_t *b, jl_binding_partition_t *bpart, size_t mod_idx, int unchanged_implicit, int no_replacement)
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

static void jl_restore_system_image_from_stream_(ios_t *f, jl_image_t *image,
                                                 jl_array_t *depmods, uint64_t checksum,
                                /* outputs */    jl_array_t **restored JL_REQUIRE_ROOTED_SLOT,
                                                 jl_array_t **init_order JL_REQUIRE_ROOTED_SLOT,
                                                 jl_array_t **extext_methods JL_REQUIRE_ROOTED_SLOT,
                                                 jl_array_t **internal_methods JL_REQUIRE_ROOTED_SLOT,
                                                 jl_array_t **method_roots_list JL_REQUIRE_ROOTED_SLOT,
                                                 jl_array_t **backedge_log JL_REQUIRE_ROOTED_SLOT,
                                                 pkgcachesizes *cachesizes) JL_GC_DISABLED
{
    jl_task_t *ct = jl_current_task;
    int en = jl_gc_enable(0);
    ios_t sysimg, const_data, symbols, relocs, gvar_record, fptr_record;
    jl_serializer_state s = {0};
    s.incremental = restored != NULL; // n_linkage_blobs() > 0;
    s.image = image;
    s.s = NULL;
    s.const_data = &const_data;
    s.symbols = &symbols;
    s.relocs = &relocs;
    s.gvar_record = &gvar_record;
    s.fptr_record = &fptr_record;
    s.ptls = ct->ptls;
    jl_value_t **_tags[NUM_TAGS];
    jl_value_t ***tags = s.incremental ? NULL : _tags;
    if (!s.incremental)
        get_tags(_tags);

    htable_t new_dt_objs;
    htable_new(&new_dt_objs, 0);
    arraylist_new(&deser_sym, 0);
    jl_value_t *sig_tns_root = NULL;

    if (jl_options.use_sysimage_native_code != JL_OPTIONS_USE_SYSIMAGE_NATIVE_CODE_YES || IMAGE_NATIVE_CODE_TAINTED) {
        memset(&image->fptrs, 0, sizeof(image->fptrs));
        image->gvars_base = NULL;
        IMAGE_NATIVE_CODE_TAINTED = 1;
    }

#ifdef ENABLE_TIMINGS
    // Hand-rolled sub-region zone (no cleanup attribute: ends explicitly below)
    static jl_timing_event_t *meta_timing_event = NULL;
    if (!meta_timing_event)
        meta_timing_event = jl_timing_event_create("LOAD_IMAGE", "LOAD_ImageMeta", __func__, __FILE__, __LINE__, 0);
    jl_timing_block_t meta_timing_block = { 0 };
    meta_timing_block.event = meta_timing_event;
    jl_timing_block_start(&meta_timing_block);
#endif
    // step 1: read section map
    assert(ios_pos(f) == 0 && f->bm == bm_mem);
    size_t sizeof_sysdata = read_uint(f);
    ios_static_buffer(&sysimg, f->buf, sizeof_sysdata + sizeof(uintptr_t));
    ios_skip(f, sizeof_sysdata);

    size_t sizeof_constdata = read_uint(f);
    // realign stream to max-alignment for data
    ios_seek(f, LLT_ALIGN(ios_pos(f), JL_CACHE_BYTE_ALIGNMENT));
    ios_static_buffer(&const_data, f->buf + f->bpos, sizeof_constdata);
    ios_skip(f, sizeof_constdata);

    size_t sizeof_sysimg = f->bpos;

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
    uintptr_t offset_restored = 0, offset_init_order = 0, offset_extext_methods = 0, offset_new_ext = 0, offset_backedge_log = 0, offset_method_roots_list = 0, offset_sig_tns = 0;
    if (!s.incremental) {
        size_t i;
        for (i = 0; tags[i] != NULL; i++) {
            jl_value_t **tag = tags[i];
            *tag = jl_read_value(&s);
        }
        for (i = 0; i < jl_n_builtins; i++)
            jl_builtin_instances[i] = jl_read_value(&s);
#define XX(name, type) jl_##name = (type)jl_read_value(&s);
        JL_EXPORTED_DATA_POINTERS(XX)
#undef XX
#define XX(name, type) jl_##name = (type)jl_read_value(&s);
        JL_CONST_GLOBAL_VARS(XX)
#undef XX
#define XX(name) \
        ijl_small_typeof[(jl_##name##_tag << 4) / sizeof(*ijl_small_typeof)] = jl_##name##_type;
        JL_SMALL_TYPEOF(XX)
#undef XX
        export_jl_small_typeof();
        export_jl_sysimg_globals();
        jl_global_roots_list = (jl_genericmemory_t*)jl_read_value(&s);
        jl_global_roots_keyset = (jl_genericmemory_t*)jl_read_value(&s);
        jl_gc_write(s.ptls->root_task, s.ptls->root_task->tls, jl_value_t, jl_read_value(&s));

        uint32_t gs_ctr = read_uint32(f);
        jl_require_world = read_uint(f);
        jl_atomic_store_release(&jl_world_counter, jl_require_world);
        jl_typeinf_world = read_uint(f);
        jl_set_gs_ctr(gs_ctr);
    }
    else {
        offset_restored = jl_read_offset(&s);
        offset_init_order = jl_read_offset(&s);
        offset_extext_methods = jl_read_offset(&s);
        offset_new_ext = jl_read_offset(&s);
        offset_backedge_log = jl_read_offset(&s);
        offset_method_roots_list = jl_read_offset(&s);
        offset_sig_tns = jl_read_offset(&s);
    }
    s.buildid_depmods_idxs = depmod_to_imageidx(depmods);
    ici_register_image((uintptr_t)s.s->buf, (size_t)s.s->size, (uintptr_t)s.const_data->buf,
                       s.incremental ? s.buildid_depmods_idxs : NULL, !s.incremental);
    size_t nlinks_gctags = read_uint32(f);
    if (nlinks_gctags > 0) {
        s.link_ids_gctags = jl_alloc_array_1d(jl_array_int32_type, nlinks_gctags);
        ios_read(f, (char*)jl_array_data(s.link_ids_gctags, uint32_t), nlinks_gctags * sizeof(uint32_t));
    }
    size_t nlinks_relocs = read_uint32(f);
    if (nlinks_relocs > 0) {
        s.link_ids_relocs = jl_alloc_array_1d(jl_array_int32_type, nlinks_relocs);
        ios_read(f, (char*)jl_array_data(s.link_ids_relocs, uint32_t), nlinks_relocs * sizeof(uint32_t));
    }
    size_t nlinks_gvars = read_uint32(f);
    if (nlinks_gvars > 0) {
        s.link_ids_gvars = jl_alloc_array_1d(jl_array_int32_type, nlinks_gvars);
        ios_read(f, (char*)jl_array_data(s.link_ids_gvars, uint32_t), nlinks_gvars * sizeof(uint32_t));
    }
    size_t nlinks_external_fnvars = read_uint32(f);
    if (nlinks_external_fnvars > 0) {
        s.link_ids_external_fnvars = jl_alloc_array_1d(jl_array_int32_type, nlinks_external_fnvars);
        ios_read(f, (char*)jl_array_data(s.link_ids_external_fnvars, uint32_t), nlinks_external_fnvars * sizeof(uint32_t));
    }
    uint32_t external_fns_begin = read_uint32(f);
    if (s.incremental) {
        assert(restored && init_order && extext_methods && internal_methods && method_roots_list);
        *restored = (jl_array_t*)jl_delayed_reloc(&s, offset_restored);
        *init_order = (jl_array_t*)jl_delayed_reloc(&s, offset_init_order);
        *extext_methods = (jl_array_t*)jl_delayed_reloc(&s, offset_extext_methods);
        (void)(jl_array_t*)jl_delayed_reloc(&s, offset_new_ext);
        jl_value_t *belog = jl_delayed_reloc(&s, offset_backedge_log);
        if (backedge_log)
            *backedge_log = belog == jl_nothing ? NULL : (jl_array_t*)belog;
        *method_roots_list = (jl_array_t*)jl_delayed_reloc(&s, offset_method_roots_list);
        sig_tns_root = jl_delayed_reloc(&s, offset_sig_tns);
        *internal_methods = jl_alloc_vec_any(0);
    }
    s.s = NULL;

#ifdef ENABLE_TIMINGS
    jl_timing_block_end(&meta_timing_block);
#endif
    // step 3: apply relocations
    assert(!ios_eof(f));
    {
        JL_TIMING(LOAD_IMAGE, LOAD_Symbols);
        jl_read_symbols(&s);
    }
    ios_close(&symbols);

    char *image_base = (char*)&sysimg.buf[0];
    reloc_t *relocs_base = (reloc_t*)&relocs.buf[0];

    s.s = &sysimg;
    jl_read_reloclist(&s, s.link_ids_gctags, GC_OLD_MARKED | GC_IN_IMAGE); // gctags
    size_t sizeof_tags = ios_pos(&relocs);
    (void)sizeof_tags;
    jl_read_reloclist(&s, s.link_ids_relocs, 0); // general relocs
    jl_read_memreflist(&s); // memowner_list relocs (must come before memref_list reads the pointers and after general relocs computes the pointers)
    jl_read_memreflist(&s); // memref_list relocs
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
#ifdef ENABLE_TIMINGS
    // Hand-rolled sub-region zone (no cleanup attribute: ends explicitly below)
    static jl_timing_event_t *uniquing_timing_event = NULL;
    if (!uniquing_timing_event)
        uniquing_timing_event = jl_timing_event_create("LOAD_IMAGE", "LOAD_Uniquing", __func__, __FILE__, __LINE__, 0);
    jl_timing_block_t uniquing_timing_block = { 0 };
    uniquing_timing_block.event = uniquing_timing_event;
    jl_timing_block_start(&uniquing_timing_block);
#endif
    JL_LOCK(&typecache_lock); // Might GC--prevent other threads from changing any type caches while we inspect them all
    for (size_t i = 0; i < s.uniquing_types.len; i++) {
        uintptr_t item = (uintptr_t)s.uniquing_types.items[i];
        // check whether we are operating on the typetag
        // (needing to ignore GC bits) or a regular field
        // and check whether this is a gvar index
        int tag = (item & 3);
        item &= ~(uintptr_t)3;
        uintptr_t *pfld;
        jl_value_t **obj, *newobj;
        if (tag == 3) {
            obj = (jl_value_t**)(image_base + item);
            pfld = NULL;
            for (size_t i = 0; i < delay_list.len; i += 2) {
                if (obj == (jl_value_t **)delay_list.items[i + 0]) {
                    pfld = (uintptr_t*)delay_list.items[i + 1];
                    delay_list.items[i + 1] = arraylist_pop(&delay_list);
                    delay_list.items[i + 0] = arraylist_pop(&delay_list);
                    break;
                }
            }
            assert(pfld);
        }
        else if (tag == 2) {
            if (image->gvars_base == NULL)
                continue;
            item >>= 2;
            assert(item < s.gvar_record->size / sizeof(reloc_t));
            pfld = sysimg_gvars(image->gvars_base, image->gvars_offsets, item);
            obj = *(jl_value_t***)pfld;
        }
        else {
            pfld = (uintptr_t*)(image_base + item);
            if (tag == 1)
                obj = (jl_value_t**)jl_typeof(jl_valueof(pfld));
            else
                obj = *(jl_value_t***)pfld;
            if ((char*)obj > (char*)pfld) {
                // this must be the super field
                assert(tag == 0);
                arraylist_push(&delay_list, obj);
                arraylist_push(&delay_list, pfld);
                ptrhash_put(&new_dt_objs, (void*)obj, obj); // mark obj as invalid
                *pfld = (uintptr_t)NULL;
                continue;
            }
        }
        uintptr_t otyp = jl_typetagof(obj);   // the original type of the object that was written here
        assert(image_base < (char*)obj && (char*)obj <= image_base + sizeof_sysimg);
        if (otyp == jl_datatype_tag << 4) {
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
                if (ici_debug_enabled()) {
                    static size_t uniq_hits = 0, uniq_new = 0;
                    newdt == NULL ? uniq_new++ : uniq_hits++;
                    if (((uniq_hits + uniq_new) & 0x3FFF) == 0)
                        jl_safe_printf("TYPEUNIQ: dedup-hits=%zd new-canonical=%zd\n", uniq_hits, uniq_new);
                }
                if (newdt == NULL) {
                    // make a non-owned copy of obj so we don't accidentally
                    // assume this is the unique copy later
                    newdt = jl_new_uninitialized_datatype();
                    jl_astaggedvalue(newdt)->bits.gc = GC_OLD;
                    // leave most fields undefined for now, but we may need instance later,
                    // and we overwrite the name field (field 0) now so preserve it too
                    if (dt->instance) {
                        if (dt->instance == jl_nothing)
                            dt->instance = jl_gc_permobj(ct->ptls, 0, newdt, 0);
                        newdt->instance = dt->instance;
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
            assert(!(image_base < (char*)otyp && (char*)otyp <= image_base + sizeof_sysimg));
            newobj = ((jl_datatype_t*)otyp)->instance;
            assert(newobj && newobj != jl_nothing);
            arraylist_push(&cleanup_list, (void*)obj);
        }
        if (tag == 1)
            *pfld = (uintptr_t)newobj | GC_OLD_MARKED | GC_IN_IMAGE;
        else
            *pfld = (uintptr_t)newobj;
        assert(!(image_base < (char*)newobj && (char*)newobj <= image_base + sizeof_sysimg));
        assert(jl_typetagis(obj, otyp));
    }
    assert(delay_list.len == 0);
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
        o->bits.in_image = 1;
    }
    arraylist_grow(&cleanup_list, -cleanup_list.len);
    // finally cache all our new types now
    jl_safepoint_suspend_all_threads(ct); // past this point, it is now not safe to observe the intermediate states on other threads via reflection, so temporarily pause those
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
    JL_UNLOCK(&typecache_lock); // Might GC
    jl_safepoint_resume_all_threads(ct); // TODO: move this later to also protect MethodInstance allocations, but we would need to acquire all jl_specializations_get_linfo and jl_module_globalref locks, which is hard
    // Perform fixups: things like updating world ages, inserting methods & specializations, etc.
    for (size_t i = 0; i < s.uniquing_objs.len; i++) {
        uintptr_t item = (uintptr_t)s.uniquing_objs.items[i];
        // check whether this is a gvar index
        int tag = (item & 3);
        assert(tag == 0 || tag == 2);
        item &= ~(uintptr_t)3;
        uintptr_t *pfld;
        jl_value_t **obj, *newobj;
        if (tag == 2) {
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
        uintptr_t otyp = jl_typetagof(obj);   // the original type of the object that was written here
        if (otyp == (uintptr_t)jl_method_instance_type) {
            assert(image_base < (char*)obj && (char*)obj <= image_base + sizeof_sysimg);
            jl_value_t *m = obj[0];
            if (jl_is_method_instance(m)) {
                newobj = m; // already done
            }
            else {
                arraylist_push(&cleanup_list, (void*)obj);
                jl_value_t *specTypes = obj[1];
                jl_value_t *sparams = obj[2];
                uint64_t t0_ = jl_hrtime();
                newobj = (jl_value_t*)jl_specializations_get_linfo((jl_method_t*)m, specTypes, (jl_svec_t*)sparams);
                jl_contrib_stats[29]++;
                jl_contrib_stats[30] += jl_hrtime() - t0_;
                obj[0] = newobj;
            }
        }
        else if (otyp == (uintptr_t)jl_binding_type) {
            jl_value_t *m = obj[0];
            if (jl_is_binding(m)) {
                newobj = m; // already done
            }
            else {
                arraylist_push(&cleanup_list, (void*)obj);
                jl_value_t *name = obj[1];
                newobj = (jl_value_t*)jl_get_module_binding((jl_module_t*)m, (jl_sym_t*)name, 1);
                obj[0] = newobj;
            }
        }
        else {
            abort(); // should be unreachable
        }
        *pfld = (uintptr_t)newobj;
        assert(!(image_base < (char*)newobj && (char*)newobj <= image_base + sizeof_sysimg));
        assert(jl_typetagis(obj, otyp));
    }
    arraylist_free(&s.uniquing_types);
    arraylist_free(&s.uniquing_objs);
    for (size_t i = 0; i < cleanup_list.len; i++) {
        void *item = cleanup_list.items[i];
        jl_taggedvalue_t *o = jl_astaggedvalue(item);
        jl_value_t *t = jl_typeof(item);
        if (t == (jl_value_t*)jl_method_instance_type)
            memset(o, 0xba, sizeof(jl_value_t*) * 3); // only specTypes and sparams fields stored
        else if (t == (jl_value_t*)jl_binding_type)
            memset(o, 0xba, sizeof(jl_value_t*) * 3); // stored as mod/name
        o->bits.in_image = 1;
    }
    arraylist_free(&cleanup_list);
#ifdef ENABLE_TIMINGS
    jl_timing_block_end(&uniquing_timing_block);
    static jl_timing_event_t *fixup_timing_event = NULL;
    if (!fixup_timing_event)
        fixup_timing_event = jl_timing_event_create("LOAD_IMAGE", "LOAD_Fixup", __func__, __FILE__, __LINE__, 0);
    jl_timing_block_t fixup_timing_block = { 0 };
    fixup_timing_block.event = fixup_timing_event;
    jl_timing_block_start(&fixup_timing_block);
#endif
    for (size_t i = 0; i < s.fixup_objs.len; i++) {
        uintptr_t item = (uintptr_t)s.fixup_objs.items[i];
        jl_value_t *obj = (jl_value_t*)(image_base + item);
        if (jl_typetagis(obj, jl_typemap_entry_type) || jl_is_method(obj) || jl_is_code_instance(obj)) {
            jl_array_ptr_1d_push(*internal_methods, obj);
            assert(s.incremental);
        }
        else if (jl_is_method_instance(obj)) {
            jl_method_instance_t *newobj = jl_specializations_get_or_insert((jl_method_instance_t*)obj);
            assert(newobj == (jl_method_instance_t*)obj); // strict insertion expected
            (void)newobj;
        }
        else if (jl_is_globalref(obj)) {
            jl_globalref_t *r = (jl_globalref_t*)obj;
            if (r->binding == NULL) {
                jl_globalref_t *gr = (jl_globalref_t*)jl_module_globalref(r->mod, r->name);
                jl_gc_write(r, r->binding, jl_binding_t, gr->binding);
            }
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
            if (s.incremental) {
                // Rebuild cross-image usings backedges
                for (size_t i = 0; i < module_usings_length(mod); ++i) {
                    struct _jl_module_using *data = module_usings_getidx(mod, i);
                    if (external_blob_index((jl_value_t*)data->mod) != mod_idx) {
                        jl_add_usings_backedge(data->mod, mod);
                    }
                }
            }
        }
        else {
            abort();
        }
    }
    if (s.incremental) {
        int no_replacement = jl_atomic_load_relaxed(&jl_first_image_replacement_world) == ~(size_t)0;
        for (size_t i = 0; i < s.fixup_objs.len; i++) {
            uintptr_t item = (uintptr_t)s.fixup_objs.items[i];
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
    arraylist_free(&s.fixup_types);
    arraylist_free(&s.fixup_objs);
#ifdef ENABLE_TIMINGS
    jl_timing_block_end(&fixup_timing_block);
#endif

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
            (unsigned)sizeof_sysdata,
            (unsigned)sizeof_constdata,
            (unsigned)sizeof_symbols,
            (unsigned)sizeof_tags,
            (unsigned)(sizeof_relocations - sizeof_tags),
            (unsigned)sizeof_gvar_record,
            (unsigned)sizeof_fptr_record);
    }
    if (cachesizes) {
        cachesizes->sysdata = sizeof_sysdata;
        cachesizes->isbitsdata = sizeof_constdata;
        cachesizes->symboldata = sizeof_symbols;
        cachesizes->tagslist = sizeof_tags;
        cachesizes->reloclist = sizeof_relocations - sizeof_tags;
        cachesizes->gvarlist = sizeof_gvar_record;
        cachesizes->fptrlist = sizeof_fptr_record;
    }

    s.s = &sysimg;
    {
        JL_TIMING(LOAD_IMAGE, LOAD_Fptrs);
        jl_update_all_fptrs(&s, image); // fptr relocs and registration
    }
    s.s = NULL;

    ios_close(&fptr_record);
    ios_close(&sysimg);

    if (s.incremental && sig_tns_root != NULL && sig_tns_root != jl_nothing)
        jl_register_sig_tns((jl_array_t*)sig_tns_root); // after uniquing: typenames are canonical
    if (!s.incremental)
        jl_gc_reset_alloc_count();
    arraylist_free(&deser_sym);

    // Prepare for later external linkage against the sysimg
    // Also sets up images for protection against garbage collection
    image_metadata_t *meta = (image_metadata_t*)malloc_s(sizeof(image_metadata_t));
    meta->base = (uintptr_t)image_base;
    meta->relocs_base = (void*)relocs_base;
    meta->idx = n_linkage_blobs();
    if (restored == NULL) {
        meta->top_mod = jl_top_module;
    } else {
        size_t len = jl_array_nrows(*restored);
        assert(len > 0);
        jl_module_t *topmod = (jl_module_t*)jl_array_ptr_ref(*restored, len-1);
        // Ordinarily set during deserialization, but our compiler stub image,
        // just returns a reference to the sysimage version, so we set it here.
        topmod->build_id.hi = checksum;
        assert(jl_is_module(topmod));
        meta->top_mod = topmod;
    }
    eyt_tree_add_range(&image_tree,
        (uintptr_t)image_base,
        (uintptr_t)image_base + sizeof_sysimg,
        (void*)meta);
    jl_timing_counter_inc(JL_TIMING_COUNTER_ImageSize, sizeof_sysimg + sizeof(uintptr_t));

    // jl_printf(JL_STDOUT, "%ld blobs to link against\n", image_tree.nranges);
    jl_gc_enable(en);

    if (s.incremental)
        jl_add_methods(*extext_methods);

}

static jl_value_t *jl_validate_cache_file(ios_t *f, jl_array_t *depmods, uint64_t *checksum, int64_t *dataendpos, int64_t *datastartpos)
{
    uint8_t pkgimage = 0;
    if (ios_eof(f) || 0 == (*checksum = jl_read_verify_header(f, &pkgimage, dataendpos, datastartpos)) || (*checksum >> 32 != 0xfafbfcfd)) {
        return jl_get_exceptionf(jl_errorexception_type,
                "Precompile file header verification checks failed.");
    }
    uint8_t flags = read_uint8(f);
    if (pkgimage && !jl_match_cache_flags_current(flags)) {
        return jl_get_exceptionf(jl_errorexception_type, "Pkgimage flags mismatch");
    }
    // Syntax version mismatch is not fatal to load
    (void)read_uint8(f); // syntax_version
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
static jl_value_t *jl_restore_package_image_from_stream(ios_t *f, jl_image_t *image, jl_array_t *depmods, int completeinfo, const char *pkgname, int needs_permalloc)
{
    JL_TIMING(LOAD_IMAGE, LOAD_Pkgimg);
    jl_timing_printf(JL_TIMING_DEFAULT_BLOCK, pkgname);
    uint64_t checksum = 0;
    int64_t dataendpos = 0;
    int64_t datastartpos = 0;
    jl_value_t *verify_fail = jl_validate_cache_file(f, depmods, &checksum, &dataendpos, &datastartpos);

    if (verify_fail)
        return verify_fail;

    assert(datastartpos > 0 && datastartpos < dataendpos);
    needs_permalloc = jl_options.permalloc_pkgimg || needs_permalloc;

    jl_value_t *restored = NULL;
    jl_array_t *init_order = NULL, *extext_methods = NULL, *internal_methods = NULL, *method_roots_list = NULL, *backedge_log = NULL;
    jl_svec_t *cachesizes_sv = NULL;
    JL_GC_PUSH7(&restored, &init_order, &extext_methods, &internal_methods, &method_roots_list, &cachesizes_sv, &backedge_log);

    { // make a permanent in-memory copy of f (excluding the header)
        ios_bufmode(f, bm_none);
        JL_SIGATOMIC_BEGIN();
        size_t len = dataendpos - datastartpos;
        char *sysimg;
        int success = !needs_permalloc;
        ios_seek(f, datastartpos);
        {
            JL_TIMING(LOAD_IMAGE, LOAD_ImageCopy);
            if (needs_permalloc) {
                sysimg = (char*)jl_gc_perm_alloc(len, 0, 64, 0);
                jl_gc_notify_image_alloc(sysimg, len);
            } else
                sysimg = &f->buf[f->bpos];
            if (needs_permalloc)
                success = ios_readall(f, sysimg, len) == len;
        }
        if (!success) {
            restored = jl_get_exceptionf(jl_errorexception_type, "Error reading package image file.");
            JL_SIGATOMIC_END();
        }
        else {
            if (needs_permalloc)
                ios_close(f);
            ios_static_buffer(f, sysimg, len);
            pkgcachesizes cachesizes;
            jl_restore_system_image_from_stream_(f, image, depmods, checksum, (jl_array_t**)&restored, &init_order, &extext_methods, &internal_methods, &method_roots_list, &backedge_log, &cachesizes);
            JL_SIGATOMIC_END();

            // enable direct compressed-IR references to this image
            ici_set_image_key((uintptr_t)sysimg, jl_worklist_key((jl_array_t*)restored));
            // Add roots to methods
            int failed;
            {
                JL_TIMING(LOAD_IMAGE, LOAD_CopyRoots);
                failed = jl_copy_roots(method_roots_list, jl_worklist_key((jl_array_t*)restored));
            }
            if (failed != 0) {
                jl_printf(JL_STDERR, "Error copying roots to methods from Module: %s\n", pkgname);
                abort();
            }
            // Insert method extensions and handle edges
            int new_methods = jl_array_nrows(extext_methods) > 0;
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
            jl_activate_methods(extext_methods, internal_methods, world, pkgname, depmods);
            // TODO: inject internal_methods into caches here, so the system can see them immediately as potential candidates (before validation)
            // allow users to start running in this updated world
            if (new_methods)
                jl_atomic_store_release(&jl_world_counter, world);
            // now permit more methods to be added again
            JL_UNLOCK(&world_counter_lock);

            if (completeinfo) {
                cachesizes_sv = jl_alloc_svec(7);
                jl_svecset(cachesizes_sv, 0, jl_box_long(cachesizes.sysdata));
                jl_svecset(cachesizes_sv, 1, jl_box_long(cachesizes.isbitsdata));
                jl_svecset(cachesizes_sv, 2, jl_box_long(cachesizes.symboldata));
                jl_svecset(cachesizes_sv, 3, jl_box_long(cachesizes.tagslist));
                jl_svecset(cachesizes_sv, 4, jl_box_long(cachesizes.reloclist));
                jl_svecset(cachesizes_sv, 5, jl_box_long(cachesizes.gvarlist));
                jl_svecset(cachesizes_sv, 6, jl_box_long(cachesizes.fptrlist));
                // Surface extext_methods to external inspectors (e.g.
                // PkgCacheInspector.jl). With the single global jl_method_table,
                // `extext_methods` contains all worklist methods; `internal_methods`
                // only partially overlaps it and exists only for per-object world-stamp
                // updates during the fixup walk.
                restored = (jl_value_t*)jl_svec(7, restored, init_order, internal_methods, extext_methods, method_roots_list, cachesizes_sv, backedge_log ? (jl_value_t*)backedge_log : jl_nothing);
            }
            else {
                restored = (jl_value_t*)jl_svec(4, restored, init_order, internal_methods, backedge_log ? (jl_value_t*)backedge_log : jl_nothing);
            }
        }
    }

    JL_GC_POP();
    return restored;
}

static void jl_restore_system_image_from_stream(ios_t *f, jl_image_t *image, uint32_t checksum)
{
    JL_TIMING(LOAD_IMAGE, LOAD_Sysimg);
    jl_restore_system_image_from_stream_(f, image, NULL, checksum | ((uint64_t)0xfdfcfbfa << 32), NULL, NULL, NULL, NULL, NULL, NULL, NULL);
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental_from_buf(jl_image_buf_t buf, jl_image_t *image, jl_array_t *depmods, int completeinfo, const char *pkgname, int needs_permalloc)
{
    ios_t f;
    ios_static_buffer(&f, (char*)buf.data, buf.size);
    jl_value_t *ret = jl_restore_package_image_from_stream(&f, image, depmods, completeinfo, pkgname, needs_permalloc);
    ios_close(&f);
    return ret;
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental(const char *fname, jl_array_t *depmods, int completeinfo, const char *pkgname)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL) {
        return jl_get_exceptionf(jl_errorexception_type,
            "Cache file \"%s\" not found.\n", fname);
    }
    jl_image_t pkgimage = {};
    jl_value_t *ret = jl_restore_package_image_from_stream(&f, &pkgimage, depmods, completeinfo, pkgname, 1);
    ios_close(&f);
    return ret;
}

JL_DLLEXPORT void jl_restore_system_image(jl_image_t *image, jl_image_buf_t buf)
{
    ios_t f;

    if (buf.kind == JL_IMAGE_KIND_NONE)
        return;

    if (buf.kind == JL_IMAGE_KIND_SO)
        assert(image->fptrs.ptrs); // jl_load_sysimg should already be run

    JL_SIGATOMIC_BEGIN();
    ios_static_buffer(&f, (char *)buf.data, buf.size);

    jl_restore_system_image_from_stream(&f, image, buf.checksum);

    ios_close(&f);
    JL_SIGATOMIC_END();
}

JL_DLLEXPORT jl_value_t *jl_restore_package_image_from_file(const char *fname, jl_array_t *depmods, int completeinfo, const char *pkgname, int ignore_native)
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

    jl_image_buf_t buf = get_image_buf(pkgimg_handle, /* is_pkgimage */ 1);

    jl_gc_notify_image_load(buf.data, buf.size);

    // Despite the name, this function actually parses the pkgimage
    jl_image_t pkgimage = jl_load_pkgimg(buf);

    if (ignore_native) {
        // Must disable using native code in possible downstream users of this code:
        // https://github.com/JuliaLang/julia/pull/52123#issuecomment-1959965395.
        // The easiest way to do that is to disable it in all of them.
        IMAGE_NATIVE_CODE_TAINTED = 1;
    }

    jl_value_t* mod = jl_restore_incremental_from_buf(buf, &pkgimage, depmods, completeinfo, pkgname, 0);

    return mod;
}


#ifdef __cplusplus
}
#endif
