// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_PROCESSOR_H
#define JL_PROCESSOR_H

#include "support/dtypes.h"

#include "julia.h"

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Image metadata
// Every image exports a `jl_image_pointers_t` as a global symbol `jl_image_pointers`.
// This symbol acts as a root for all other code-related symbols in the image.

// Feature indices come from the cpufeatures library's generated tables.
// The actual constants are defined in base/features_h.jl (auto-generated).
typedef uint32_t jl_cpu_feature_t;

JL_DLLEXPORT int jl_test_cpu_feature(jl_cpu_feature_t feature);

static const uint32_t jl_sysimg_tag_mask = 0x80000000u;
static const uint32_t jl_sysimg_val_mask = ~((uint32_t)0x80000000u);

// A parsed image file
typedef struct _jl_image_fptrs_t {
    // number of functions
    uint32_t nptrs;
    // function pointers
    void **ptrs;

    // Following fields contains the information about the selected target.
    // All of these fields are 0 if the selected targets have all the functions cloned.
    // Instead the offsets are stored in `nptrs` and `ptrs`.

    // number of cloned functions
    uint32_t nclones;
    // function pointer of cloned functions
    void **clone_ptrs;
    // sorted indices of the cloned functions (including the tag bit)
    const uint32_t *clone_idxs;
} jl_image_fptrs_t;

struct _jl_image_t {
    uint64_t base;
    const char *gvars_base;
    const int32_t *gvars_offsets;
    uint32_t ngvars;
    jl_image_fptrs_t fptrs;
    void **jl_small_typeof;
};

// The header for each image
// Details important counts about the image
typedef struct {
    // The version of the image format
    // Most up-to-date version is 1
    uint32_t version;
    // The number of shards in this image
    uint32_t nshards;
    // The total number of fvars in this image among all shards
    uint32_t nfvars;
    // The total number of gvars in this image among all shards
    uint32_t ngvars;
} jl_image_header_t;

// Per-shard data for image shards. Each image contains header->nshards of these.
typedef struct {
    // The array of function pointers (`void*`).
    // This includes all julia functions in sysimg as well as all other functions that are cloned.
    // The default function pointer is used if the function is cloned.
    // The first element is the size of the array, which should **NOT** be used is the number
    // of julia functions in the sysimg.
    // Each entry in this array uniquely identifies a function we are interested in
    // (the function may have multiple function pointers corresponding to different versions).
    const uintptr_t *fvar_count;
    void **fvar_ptrs;
    // This is the mapping of shard function index -> global function index
    // staticdata.c relies on the same order of functions in the global function array being
    // the same as what it saw when serializing the global function array. However, partitioning
    // into multiple shards will cause functions to be reordered. This array is used to map
    // back to the original function array for loading.
    const uint32_t *fvar_idxs;
    // This array of global variable offsets (`int32_t`) from the base pointer.
    // Similar to fvar_offsets, but for gvars
    // This is also the base data pointer
    // (all data pointers in this shard are stored as offsets to this address)
    const int32_t *gvar_offsets;
    // This is the mapping of shard global variable index -> global global variable index
    // Similar to fvar_idxs, but for gvars
    const uint32_t *gvar_idxs;
    // location and index of relocation slots.
    // Stored as pairs of function indices and `int32_t` offsets from `jl_sysimg_gvars_base`.
    // The first element is an `uint32_t` giving the number of relocations.
    // This is needed for functions whose address is used in a way that requires dispatch.
    // We currently only support one type of relocation (i.e. absolute pointer) which is enough
    // for all use in functions as well as GOT slot (for "PLT" callback).
    // Note that not all functions being cloned are assigned a slot.
    // This array is sorted by the function indices.
    // There can be more than one slot per-function,
    // i.e. there can be duplicated function indices.
    const int32_t *clone_slots;
    //  Target-specific function pointer offsets.
    //  This contains all the cloned functions that we are interested in and it needs to be decoded
    //  and used along with `jl_dispatch_fvars_idxs`.
    //  For the default target, there's no entries in this variable, if there's any relocations
    //  needed for the default target, the function pointers are taken from the global offset
    //  arrays directly.
    //  For a `clone_all` target (i.e. with the length in `jl_dispatch_fvars_idxs` tagged), this
    //  variable contains an offset array of the same length as the global one. Only the indices
    //  appearing in `jl_dispatch_fvars_idxs` need relocation and the dispatch code should return
    //  this array as the original/base function offsets.
    //  For other targets, this variable contains an offset array with the length defined in
    //  `jl_dispatch_fvars_idxs`. Tagged indices need relocations.
    void **clone_ptrs;
    //  Target-specific function indices.
    //  For each target, this includes a tagged `uint32_t` length, an optional `uint32_t` index
    //  of the base target followed by an array of tagged function indices.
    //  The base target index is required to be smaller than the index of the current target
    //  and must be the default (`0`) or a `clone_all` target.
    //  The tag bits for both the length and the indices are the top bit.
    //  A tagged length indicates that all of the functions are cloned and the indices follows
    //  are the ones that requires relocation. The base target index is omitted in this case.
    //  Otherwise, the length is the total number of functions that we are interested in
    //  for this target, which includes all cloned julia functions and
    //  all other cloned functions that requires relocation.
    //  A tagged index means that the function pointer should be filled into the GOT slots
    //  identified by `jl_dispatch_reloc_slots`. There could be more than one slot per function.
    //  (Note that a tagged index could corresponds to a function's pointer that's the same as
    //  the base one since this is the only way we currently represent relocations.)
    const uint32_t *clone_idxs;
} jl_image_shard_t;

// The TLS data for each image
typedef struct {
    void *pgcstack_func_slot;
    void *pgcstack_key_slot;
    size_t *tls_offset;
} jl_image_ptls_t;

//The root struct for images, points to all the other globals
typedef struct {
    // The image header, contains numerical global data
    const jl_image_header_t *header;
    // The shard table, contains per-shard data
    const jl_image_shard_t *shards; // points to header->nshards length array
    // The TLS data pointer
    const jl_image_ptls_t *ptls;
    // A copy of jl_small_typeof[]
    void **jl_small_typeof;

    //  serialized target data
    //  This contains the number of targets
    //  in addition to the name and feature set of each target.
    const void *target_data;
    // Original CPU target string used to build this sysimage
    const char *cpu_target_string;
} jl_image_pointers_t;

/**
 * Initialize the processor dispatch system with sysimg `hdl` (also initialize the sysimg itself).
 * The dispatch system will find the best implementation to be used in this session.
 * The decision will be based on the host CPU and features as well as the `cpu_target`
 * option. This must be called before initializing JIT and should only be called once.
 * An error will be raised if this is called more than once or none of the implementation
 * supports the current system.
 *
 * Return the data about the function pointers selected.
 */
void jl_check_cpu_target(const char *cpu_target, int imaging);
jl_image_t jl_load_sysimg(jl_image_buf_t image, const char *cpu_target);
jl_image_t jl_load_pkgimg(jl_image_buf_t image);

// Internal function to set the sysimage CPU target during initialization
void jl_set_sysimage_cpu_target(const char *cpu_target);

// Return the name of the host CPU as a julia string.
JL_DLLEXPORT jl_value_t *jl_get_cpu_name(void);
// Return the features of the host CPU as a julia string.
JL_DLLEXPORT jl_value_t *jl_get_cpu_features(void);
// Return the CPU target string used to build the current sysimage
JL_DLLEXPORT jl_value_t *jl_get_sysimage_cpu_target(void);
// Dump the name and feature set of the host CPU
JL_DLLEXPORT jl_value_t *jl_cpu_has_fma(int bits);
// Check if the CPU has native FMA instructions;
// For debugging only
JL_DLLEXPORT void jl_dump_host_cpu(void);
JL_DLLEXPORT jl_value_t* jl_check_pkgimage_clones(char* data);

JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero);
JL_DLLEXPORT int32_t jl_get_zero_subnormals(void);
JL_DLLEXPORT int32_t jl_set_default_nans(int8_t isDefault);
JL_DLLEXPORT int32_t jl_get_default_nans(void);

/**
 * System image contents.
 *
 * These symbols are typically dummy values, unless statically linking
 * libjulia-* and the sysimage together (see null_sysimage.c), in which
 * case they allow accessing the local copy of the sysimage.
 **/
typedef void jl_image_unpack_func_t(void *handle, jl_image_buf_t *image);
extern jl_image_unpack_func_t *jl_image_unpack;

#ifdef __cplusplus
}

#include <utility>
#include <string>
#include <vector>

#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
#include <cpufeatures/target_tables_x86_64.h>
#elif defined(__aarch64__) || defined(_M_ARM64)
#include <cpufeatures/target_tables_aarch64.h>
#elif defined(__riscv) && __riscv_xlen == 64
#include <cpufeatures/target_tables_riscv64.h>
#else
#include <cpufeatures/target_tables_fallback.h>
#endif
#include <cpufeatures/target_parsing.h>

// NOLINTBEGIN(clang-diagnostic-return-type-c-linkage)
/**
 * Returns the CPU name and feature string to be used by LLVM JIT.
 *
 * If the detected/specified CPU name is not available on the LLVM version specified,
 * a fallback CPU name will be used. Unsupported features will be ignored.
 */
extern "C" JL_DLLEXPORT std::pair<std::string,std::string> jl_get_llvm_target(const char *cpu_target, bool imaging) JL_NOTSAFEPOINT;

/**
 * Returns the CPU name and feature string to be used by LLVM disassembler.
 *
 * This will return a generic CPU name and a full feature string.
 */
extern "C" JL_DLLEXPORT const std::pair<std::string,std::string> &jl_get_llvm_disasm_target(void) JL_NOTSAFEPOINT;

struct jl_target_spec_t {
    std::string cpu_name;
    std::string cpu_features;
    int base;
    bool clone_all = false;
    bool opt_size = false;
    bool min_size = false;
    tp::FeatureDiff diff;
};

struct jl_clone_targets_t {
    std::vector<jl_target_spec_t> specs;
    std::vector<uint8_t> data; // serialized target identification blob
};

/**
 * Return the list of targets to clone and their serialized identification data
 */
extern "C" JL_DLLEXPORT jl_clone_targets_t jl_get_llvm_clone_targets(const char *cpu_target) JL_NOTSAFEPOINT;
// NOLINTEND(clang-diagnostic-return-type-c-linkage)
extern "C" JL_DLLEXPORT jl_value_t* jl_reflect_clone_targets();
extern "C" JL_DLLEXPORT jl_value_t *jl_feature_bits_to_string(const uint8_t *bits, int32_t nwords);
extern "C" JL_DLLEXPORT std::string jl_expand_sysimage_keyword(const char *cpu_target);
#endif

#endif
