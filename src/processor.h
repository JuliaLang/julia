// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "support/dtypes.h"

#include "julia.h"

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Related sysimg exported symbols
 *
 * In the following text function refer to an abstract identity.
 * It corresponds to a `Function` that we emit in the codegen and there might be multiple copy
 * of it in the system image. Only one of those copy will be used in a given session.
 * Function pointers refer to a real piece of code in the system image.
 * Each function might have multiple function pointers in the system image
 * and each function pointer will correspond to only one function.
 *
 * # Global function and base pointers
 * `jl_sysimg_gvars_base`:
 *     The address of this symbol is the base data pointer
 *     (all other data pointers are stored as offsets to this address)
 * `jl_sysimg_fvars_base`:
 *     The address of this symbol is the base function pointer
 *     (all other function pointers are stored as offsets to this address)
 * `jl_sysimg_fvars_offsets`: [static data]
 *     The array of function pointer offsets (`int32_t`) from the base pointer.
 *     This includes all julia functions in sysimg as well as all other functions that are cloned.
 *     The default function pointer is used if the function is cloned.
 *     The first element is the size of the array, which should **NOT** be used as the number
 *     of julia functions in the sysimg.
 *     Each entry in this array uniquely identifies a function which we are interested in
 *     (the function may have multiple function pointers corresponding to different versions).
 *     In other sysimg info, all information of functions are stored as function index which are
 *     `uint32_t` index in this array.
 *
 * # Target data and dispatch slots (Only needed by runtime during loading)
 * `jl_dispatch_target_ids`: [static data] serialize target data.
 *     This contains the number of targets which is needed to decode `jl_dispatch_fvars_idxs`
 *     in additional to the name and feature set of each target.
 * `jl_dispatch_reloc_slots`: [static data] location and index of relocation slots.
 *     Stored as pairs of function indices and `int32_t` offsets from `jl_sysimg_gvars_base`.
 *     The first element is an `uint32_t` giving the number of relocations.
 *     This is needed for functions whose address is used in a way that requires dispatch.
 *     We currently only support one type of relocation (i.e. absolute pointer) which is enough
 *     for all use in functions as well as global GOT slot (for "PLT" callback).
 *     Note that not all functions being cloned are assigned a slot.
 *     This array is sorted by the function indices.
 *     There can be more than one slot per-function,
 *     i.e. there can be duplicated function indices.
 *
 * # Target functions
 * `jl_dispatch_fvars_idxs`: [static data] Target specific functions indices.
 *     For each target, this includes a tagged `uint32_t` length, an optional `uint32_t` index
 *     of the base target followed by an array of tagged function indices.
 *     The base target index is required to be smaller than the index of the current target
 *     and must be the default (`0`) or a `clone_all` target.
 *     If it's not `0`, the function pointer array for the `clone_all` target will be used as
 *     the base function pointer offsets instead.
 *     The tag bits for both the length and the indices are the top bit.
 *     A tagged length indicates that all of the functions are cloned and the indices follows
 *     are the ones that requires relocation. The base target index is omitted in this case.
 *     Otherwise, the length is the total number of functions that we are interested in
 *     for this target, which includes all cloned julia functions and
 *     all other cloned functions that requires relocation.
 *     A tagged index means that the function pointer should be filled into the GOT slots
 *     identified by `jl_dispatch_reloc_slots`. There could be more than one slot per function.
 *     (Note that a tagged index could corresponds to a functions pointer that's the same as
 *     the base one since this is the only way we currently represent relocations.)
 *     A tagged length implicitly tags all the indices and the indices will not have the tag bit
 *     set. The lengths in this variable is needed to decode `jl_dispatch_fvars_offsets`.
 * `jl_dispatch_fvars_offsets`: [static data] Target specific function pointer offsets.
 *     This contains all the cloned functions that we are interested and it needs to be decoded
 *     and used along with `jl_dispatch_fvars_idxs`.
 *     For the default target, there's no entries in this variable, if there's any relocations
 *     needed for the default target, the function pointers are taken from the global offset
 *     arrays directly.
 *     For a `clone_all` target (i.e. with the length in `jl_dispatch_fvars_idxs` tagged), this
 *     variable contains an offset array the same length as the global one. Only the indices
 *     appeared in `jl_dispatch_fvars_idxs` needs relocation and the dispatch code should return
 *     this array as the original/base function offsets.
 *     For other targets, this variable contains an offset array with the length defined in
 *     `jl_dispatch_fvars_idxs`. Tagged indices needs relocations.
 */

enum {
    JL_TARGET_VEC_CALL = 1 << 0,
    // Clone all functions
    JL_TARGET_CLONE_ALL = 1 << 1,
    // Clone when there's scalar math operations that can benefit from target specific
    // optimizations. This includes `muladd`, `fma`, `fast`/`contract` flags.
    JL_TARGET_CLONE_MATH = 1 << 2,
    // Clone when the function has a loop
    JL_TARGET_CLONE_LOOP = 1 << 3,
    // Clone when the function uses any vectors
    // When this is specified, the cloning pass should also record if any of the cloned functions
    // used this in any function call (including the signature of the function itself)
    JL_TARGET_CLONE_SIMD = 1 << 4,
    // The CPU name is unknown
    JL_TARGET_UNKNOWN_NAME = 1 << 5,
    // Optimize for size for this target
    JL_TARGET_OPTSIZE = 1 << 6,
    // Only optimize for size for this target
    JL_TARGET_MINSIZE = 1 << 7,
};

#define JL_FEATURE_DEF_NAME(name, bit, llvmver, str) JL_FEATURE_DEF(name, bit, llvmver)
typedef enum {
#define JL_FEATURE_DEF(name, bit, llvmver) JL_X86_##name = bit,
#include "features_x86.h"
#undef JL_FEATURE_DEF
#define JL_FEATURE_DEF(name, bit, llvmver) JL_AArch32_##name = bit,
#include "features_aarch32.h"
#undef JL_FEATURE_DEF
#define JL_FEATURE_DEF(name, bit, llvmver) JL_AArch64_##name = bit,
#include "features_aarch64.h"
#undef JL_FEATURE_DEF
} jl_cpu_feature_t;
#undef JL_FEATURE_DEF_NAME

int jl_test_cpu_feature(jl_cpu_feature_t feature);

static const uint32_t jl_sysimg_tag_mask = 0x80000000u;
static const uint32_t jl_sysimg_val_mask = ~((uint32_t)0x80000000u);

typedef struct _jl_sysimg_fptrs_t {
    // base function pointer
    const char *base;
    // number of functions
    uint32_t noffsets;
    // function pointer offsets
    const int32_t *offsets;

    // Following fields contains the information about the selected target.
    // All of these fields are 0 if the selected targets have all the functions cloned.
    // Instead the offsets are stored in `noffsets` and `offsets`.

    // number of cloned functions
    uint32_t nclones;
    // function pointer offsets of cloned functions
    const int32_t *clone_offsets;
    // sorted indices of the cloned functions (including the tag bit)
    const uint32_t *clone_idxs;
} jl_sysimg_fptrs_t;

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
jl_sysimg_fptrs_t jl_init_processor_sysimg(void *hdl);

// Return the name of the host CPU as a julia string.
JL_DLLEXPORT jl_value_t *jl_get_cpu_name(void);
// Dump the name and feature set of the host CPU
// For debugging only
JL_DLLEXPORT void jl_dump_host_cpu(void);

#ifdef __cplusplus
}

#include <utility>
#include <string>
#include <vector>

extern bool jl_processor_print_help;

/**
 * Returns the CPU name and feature string to be used by LLVM JIT.
 *
 * If the detected/specified CPU name is not available on the LLVM version specified,
 * a fallback CPU name will be used. Unsupported features will be ignored.
 */
std::pair<std::string,std::vector<std::string>> jl_get_llvm_target(bool imaging, uint32_t &flags);

/**
 * Returns the CPU name and feature string to be used by LLVM disassembler.
 *
 * This will return a generic CPU name and a full feature string.
 */
const std::pair<std::string,std::string> &jl_get_llvm_disasm_target(void);

struct jl_target_spec_t {
    // LLVM target name
    std::string cpu_name;
    // LLVM feature string
    std::string cpu_features;
    // serialized identification data
    std::vector<uint8_t> data;
    // Clone condition.
    uint32_t flags;
    // Base target index.
    int base;
};
/**
 * Return the list of targets to clone
 */
std::vector<jl_target_spec_t> jl_get_llvm_clone_targets(void);
std::string jl_get_cpu_name_llvm(void);
std::string jl_get_cpu_features_llvm(void);
std::string jl_format_filename(llvm::StringRef output_pattern);
#endif
