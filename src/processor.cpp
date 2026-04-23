// This file is a part of Julia. License is MIT: https://julialang.org/license

// Processor feature detection and dispatch using the cpufeatures library.
// CPU/feature tables are generated from LLVM's TableGen data and committed
// to https://github.com/gbaraldi/cpufeatures
//
// On LLVM version bump:
//   1. cd cpufeatures && make -f Makefile.generate LLVM_VER=<new>
//   2. Review and commit regenerated generated/ headers
//   3. Update Julia's deps/cpufeatures.version with the new commit hash
//   4. The static_assert below will catch major version mismatches

#include "processor.h"

#include "julia.h"
#include "julia_internal.h"

#include <algorithm>
#include <vector>
#include <string>

#include "julia_assert.h"

#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif


// Forward declarations for sysimage CPU target storage
static std::string sysimage_cpu_target;
void jl_set_sysimage_cpu_target(const char *cpu_target);

namespace {

// Load sysimg/pkgimg, use the callback for dispatch and perform all relocations
template<typename F>
static inline jl_image_t load_sysimg_target(jl_image_buf_t image, F &&callback, void *ctx)
{
    JL_TIMING(LOAD_IMAGE, LOAD_Processor);
    jl_image_t res{};

    if (image.kind != JL_IMAGE_KIND_SO)
        return res;

    const jl_image_pointers_t *pointers = (const jl_image_pointers_t *)image.pointers;
    const void *ids = pointers->target_data;

    // Set the sysimage CPU target from the stored string
    if (pointers->cpu_target_string) {
        jl_set_sysimage_cpu_target(pointers->cpu_target_string);
    }

    jl_value_t* rejection_reason = nullptr;
    JL_GC_PUSH1(&rejection_reason);
    uint32_t target_idx = callback(ctx, ids, &rejection_reason);
    if (target_idx == UINT32_MAX) {
        jl_error(jl_string_ptr(rejection_reason));
    }
    JL_GC_POP();

    if (pointers->header->version != 1) {
        jl_error("Image file is not compatible with this version of Julia");
    }

    std::vector<void*> fvars(pointers->header->nfvars);
    std::vector<const char*> gvars(pointers->header->ngvars);

    std::vector<std::pair<uint32_t, void*>> clones;

    for (unsigned i = 0; i < pointers->header->nshards; i++) {
        auto shard = pointers->shards[i];

        void **fvar_shard = shard.fvar_ptrs;
        uintptr_t nfunc = *shard.fvar_count;
        assert(nfunc <= pointers->header->nfvars);
        const int32_t *reloc_slots = shard.clone_slots;
        const uint32_t nreloc = reloc_slots[0];
        reloc_slots++;
        const uint32_t *clone_idxs = shard.clone_idxs;
        void **clone_ptrs = shard.clone_ptrs;
        uint32_t tag_len = clone_idxs[0];
        clone_idxs++;

        assert(tag_len & jl_sysimg_tag_mask);
        std::vector<void**> base_ptrs(0);
        base_ptrs.push_back(fvar_shard);
        // Find target
        for (uint32_t i = 0; i < target_idx; i++) {
            uint32_t len = jl_sysimg_val_mask & tag_len;
            if (jl_sysimg_tag_mask & tag_len) {
                clone_idxs += len + 1;
                if (i != 0)
                    clone_ptrs += nfunc;
            }
            else {
                clone_ptrs += len;
                clone_idxs += len + 2;
            }
            tag_len = clone_idxs[-1];
            base_ptrs.push_back(tag_len & jl_sysimg_tag_mask ? clone_ptrs : nullptr);
        }

        bool clone_all = (tag_len & jl_sysimg_tag_mask) != 0;
        // Fill in return value
        if (clone_all) {
            // clone_all
            if (target_idx != 0) {
                fvar_shard = clone_ptrs;
            }
        }
        else {
            uint32_t base_idx = clone_idxs[0];
            assert(base_idx < target_idx);
            if (target_idx != 0) {
                fvar_shard = base_ptrs[base_idx];
                assert(fvar_shard);
            }
            clone_idxs++;
            unsigned start = clones.size();
            clones.resize(start + tag_len);
            auto idxs = shard.fvar_idxs;
            for (unsigned i = 0; i < tag_len; i++) {
                clones[start + i] = {(clone_idxs[i] & ~jl_sysimg_val_mask) | idxs[clone_idxs[i] & jl_sysimg_val_mask], clone_ptrs[i]};
            }
        }
        // Do relocation
        uint32_t reloc_i = 0;
        uint32_t len = jl_sysimg_val_mask & tag_len;
        for (uint32_t i = 0; i < len; i++) {
            uint32_t idx = clone_idxs[i];
            void *fptr;
            if (clone_all) {
                fptr = fvar_shard[idx];
            }
            else if (idx & jl_sysimg_tag_mask) {
                idx = idx & jl_sysimg_val_mask;
                fptr = clone_ptrs[i];
            }
            else {
                continue;
            }
            bool found = false;
            for (; reloc_i < nreloc; reloc_i++) {
                auto reloc_idx = ((const uint32_t*)reloc_slots)[reloc_i * 2];
                if (reloc_idx == idx) {
                    found = true;
                    const char *data_base = (const char*)shard.clone_slots;
                    auto slot = (const void**)(data_base + reloc_slots[reloc_i * 2 + 1]);
                    assert(slot);
                    *slot = fptr;
                }
                else if (reloc_idx > idx) {
                    break;
                }
            }
            assert(found && "Cannot find GOT entry for cloned function.");
            (void)found;
        }

        auto fidxs = shard.fvar_idxs;
        for (uint32_t i = 0; i < nfunc; i++) {
            fvars[fidxs[i]] = fvar_shard[i];
        }

        // .data base
        auto gidxs = shard.gvar_idxs;
        unsigned ngvars = shard.gvar_offsets[0];
        assert(ngvars <= pointers->header->ngvars);
        char *data_base = (char*)shard.gvar_offsets;
        for (uint32_t i = 0; i < ngvars; i++) {
            gvars[gidxs[i]] = data_base + shard.gvar_offsets[i+1];
        }
    }

    if (!fvars.empty()) {
        auto ptrs = (void**) malloc(sizeof(void*) * fvars.size());
        for (size_t i = 0; i < fvars.size(); i++) {
            assert(fvars[i] && "Missing function pointer!");
            ptrs[i] = fvars[i];
        }
        res.fptrs.ptrs = ptrs;
        res.fptrs.nptrs = fvars.size();
    }

    if (!gvars.empty()) {
        auto offsets = (int32_t*)malloc(sizeof(int32_t) * gvars.size());
        res.gvars_base = (const char*)pointers->header;
        for (size_t i = 0; i < gvars.size(); i++) {
            assert(gvars[i] && "Missing global variable pointer!");
            offsets[i] = gvars[i] - res.gvars_base;
        }
        res.gvars_offsets = offsets;
        res.ngvars = gvars.size();
    }

    if (!clones.empty()) {
        assert(!fvars.empty());
        std::sort(clones.begin(), clones.end(),
            [](const std::pair<uint32_t, const void*> &a, const std::pair<uint32_t, const void*> &b) {
                return (a.first & jl_sysimg_val_mask) < (b.first & jl_sysimg_val_mask);
        });
        auto clone_ptrs = (void**) malloc(sizeof(void*) * clones.size());
        auto clone_idxs = (uint32_t *) malloc(sizeof(uint32_t) * clones.size());
        for (size_t i = 0; i < clones.size(); i++) {
            clone_idxs[i] = clones[i].first;
            clone_ptrs[i] = clones[i].second;
        }
        res.fptrs.clone_idxs = clone_idxs;
        res.fptrs.clone_ptrs = clone_ptrs;
        res.fptrs.nclones = clones.size();
    }

    res.base = image.base;

    {
        void *pgcstack_func_slot = pointers->ptls->pgcstack_func_slot;
        void *pgcstack_key_slot = pointers->ptls->pgcstack_key_slot;
        jl_pgcstack_getkey((jl_get_pgcstack_func**)pgcstack_func_slot, (jl_pgcstack_key_t*)pgcstack_key_slot);

        size_t *tls_offset_idx = pointers->ptls->tls_offset;
        *tls_offset_idx = (uintptr_t)(jl_tls_offset == -1 ? 0 : jl_tls_offset);
    }

    res.jl_small_typeof = pointers->jl_small_typeof;

    return res;
}

} // namespace

// Unified processor detection and dispatch using the cpufeatures library.
// Replaces processor_x86.cpp, processor_arm.cpp, and processor_fallback.cpp.
// No hand-maintained CPU/feature tables — all data comes from LLVM TableGen
// via generated headers committed to the cpufeatures repository.

// Include cpufeatures generated tables (defines FeatureBits, feature_table, etc.)
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#include <cpufeatures/target_tables_x86_64.h>
#elif defined(_CPU_AARCH64_)
#include <cpufeatures/target_tables_aarch64.h>
#elif defined(__riscv) && __riscv_xlen == 64
#include <cpufeatures/target_tables_riscv64.h>
#else
#include <cpufeatures/target_tables_fallback.h>
#endif

#include <cpufeatures/target_parsing.h>
#include <cpufeatures/cross_arch.h>

// Verify the cpufeatures tables were generated from a compatible LLVM version.
#if defined(TARGET_TABLES_LLVM_VERSION_MAJOR) && defined(LLVM_VERSION_MAJOR)
static_assert(TARGET_TABLES_LLVM_VERSION_MAJOR == LLVM_VERSION_MAJOR,
    "cpufeatures tables were generated with a different LLVM major version than Julia uses");
#endif

// ============================================================================
// Debug output
// ============================================================================

static bool cpufeatures_debug_enabled() {
    static int enabled = -1;
    if (enabled == -1) {
        const char *debug_env = getenv("JULIA_DEBUG");
        enabled = debug_env && (strstr(debug_env, "cpufeatures") || strstr(debug_env, "all"));
    }
    return enabled;
}

#define CF_DEBUG(...) do { if (cpufeatures_debug_enabled()) jl_safe_printf(__VA_ARGS__); } while (0)

// ============================================================================
// Convert feature bits to a comma-separated string of feature names.
// Called from Julia's loading.jl to display ImageTarget features.
JL_DLLEXPORT jl_value_t *jl_feature_bits_to_string(const uint8_t *bits, int32_t nwords)
{
    FeatureBits fb{};
    int copy_words = nwords < TARGET_FEATURE_WORDS ? nwords : TARGET_FEATURE_WORDS;
    memcpy(fb.bits, bits, copy_words * sizeof(uint64_t));
    auto str = tp::build_feature_string(fb);
    return jl_pchar_to_string(str.data(), str.size());
}

// ============================================================================
// Host CPU detection — thin wrappers around cpufeatures library
// ============================================================================

static inline const std::string &host_cpu_name()
{
    return tp::get_host_cpu_name();
}

static std::string get_host_feature_string()
{
    auto fb = tp::get_host_features();
    return tp::build_feature_string(fb);
}

// ============================================================================
// JIT target management
// ============================================================================

static std::vector<tp::LLVMTargetSpec> jit_targets;

// If cpu_target starts with "sysimage", replace it with the target string
// stored in the loaded sysimage. Otherwise return as-is.
extern "C" std::string jl_expand_sysimage_keyword(const char *cpu_target) {
    if (!cpu_target || !*cpu_target)
        return "";
    std::string option(cpu_target);
    if (option.substr(0, 8) == "sysimage" && (option.size() == 8 || option[8] == ';')) {
        jl_value_t *target_str = jl_get_sysimage_cpu_target();
        if (target_str && jl_string_len(target_str) > 0) {
            std::string expanded(jl_string_data(target_str), jl_string_len(target_str));
            if (option.size() > 8)
                expanded += option.substr(8);
            CF_DEBUG("[cpufeatures] expanded 'sysimage' -> '%s'\n", expanded.c_str());
            return expanded;
        }
        CF_DEBUG("[cpufeatures] WARNING: 'sysimage' keyword but no stored target, using 'native'\n");
        return "native";
    }
    return option;
}

static void init_jit_targets(const char *cpu_target, bool imaging)
{

    if (!jit_targets.empty())
        return;

    auto target_str = jl_expand_sysimage_keyword(cpu_target);
    CF_DEBUG("[cpufeatures] init_jit_targets: '%s' imaging=%d\n",
             target_str.c_str(), imaging);

    if (target_str.empty())
        jl_error("Invalid target option: empty CPU name");

    auto specs = tp::resolve_targets_for_llvm(target_str);

    if (specs.empty())
        jl_error("No targets specified");

    for (auto &s : specs) {
        CF_DEBUG("[cpufeatures]   target: name='%s' base=%d features=%s\n",
                 s.cpu_name.c_str(), s.base, s.cpu_features.c_str());
        jit_targets.push_back(std::move(s));
    }
}

// ============================================================================
// Sysimage / pkgimage target matching
// ============================================================================

// Shared: deserialize image targets, match against a resolved target.
// Returns {target_index, vreg_size} or {UINT32_MAX, 0} on failure.
static std::pair<uint32_t, int> match_image_targets(
        const void *id, const tp::LLVMTargetSpec &target, jl_value_t **rejection_reason)
{
    auto image_targets = tp::deserialize_targets((const uint8_t *)id);
    CF_DEBUG("[cpufeatures]   image has %zu target(s)\n", image_targets.size());

    auto match = tp::match_targets(image_targets, target);
    if (match.best_idx < 0) {
        CF_DEBUG("[cpufeatures]   NO compatible target found!\n");
        if (rejection_reason) {
            std::string msg = "Unable to find compatible target in cached code image.";
            *rejection_reason = jl_pchar_to_string(msg.data(), msg.size());
        }
        return {UINT32_MAX, 0};
    }

    CF_DEBUG("[cpufeatures]   selected target %d '%s' (vreg_size=%d)\n",
             match.best_idx, image_targets[match.best_idx].cpu_name.c_str(), match.vreg_size);
    return {(uint32_t)match.best_idx, match.vreg_size};
}

static uint32_t match_sysimg_target(void *ctx, const void *id, jl_value_t **rejection_reason)
{
    const char *cpu_target = (const char *)ctx;
    CF_DEBUG("[cpufeatures] match_sysimg_target: cpu_target='%s'\n",
             cpu_target ? cpu_target : "(null)");

    // For multi-target strings (sysimage building), use only the first
    // target for matching against the image being loaded.
    auto target_str = jl_expand_sysimage_keyword(cpu_target);
    auto semi = target_str.find(';');
    auto first = semi != std::string::npos ? target_str.substr(0, semi) : target_str;
    auto host_specs = tp::resolve_targets_for_llvm(first);
    if (host_specs.empty())
        jl_error("No targets specified");

    auto &target = host_specs[0];
    CF_DEBUG("[cpufeatures]   JIT target: name='%s'\n", target.cpu_name.c_str());

#if defined(_CPU_X86_64_)
    // CX16 check: only error if sysimage requires it and host doesn't have it
    {
        auto sysimg_peek = tp::deserialize_targets((const uint8_t *)id);
        bool sysimg_allows_no_cx16 = false;
        for (auto &t : sysimg_peek)
            sysimg_allows_no_cx16 |= !tp::has_feature(t.en_features, "cx16");
        if (!sysimg_allows_no_cx16 && !tp::has_feature(target.en_features, "cx16")) {
            jl_error("Your CPU does not support the CX16 instruction, which is required "
                     "by this version of Julia!  This is often due to running inside of a "
                     "virtualized environment.  Please read "
                     "https://docs.julialang.org/en/v1/devdocs/sysimg/ for more.");
        }
    }
#endif

    // Match against image targets
    auto match_result = match_image_targets(id, target, rejection_reason);
    if (match_result.first == UINT32_MAX)
        return UINT32_MAX;

    // Clamp JIT vector features to match the sysimage target's vector width.
    // On x86, AVX/AVX-512 change how VecElement tuples are passed in registers
    // (FixedVectorType maps to xmm/ymm/zmm), so the JIT must not use wider
    // vectors than the sysimage clone it calls into.
    // TODO: aarch64 SVE uses scalable vectors which Julia doesn't generate
    // (only FixedVectorType/NEON), so SVE clamping is not needed for ABI
    // correctness. RISC-V V is similar. Revisit if Julia adds scalable vector
    // support.
    int matched_vreg = match_result.second;
    int host_vreg = tp::max_vector_size(target.en_features);
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
    if (matched_vreg != host_vreg) {
        if (matched_vreg < 64) {
            static const char *avx512[] = {
                "avx512f", "avx512dq", "avx512ifma", "avx512cd",
                "avx512bw", "avx512vl", "avx512vbmi", "avx512vpopcntdq",
                "avx512vbmi2", "avx512vnni", "avx512bitalg",
                "avx512vp2intersect", "avx512bf16", "avx512fp16", nullptr
            };
            for (const char **f = avx512; *f; f++) {
                const FeatureEntry *fe = find_feature(*f);
                if (fe) feature_clear(&target.en_features, fe->bit);
            }
        }
        if (matched_vreg < 32) {
            static const char *avx[] = {
                "avx", "avx2", "fma", "f16c", "fma4", "xop",
                "vaes", "vpclmulqdq", nullptr
            };
            for (const char **f = avx; *f; f++) {
                const FeatureEntry *fe = find_feature(*f);
                if (fe) feature_clear(&target.en_features, fe->bit);
            }
        }
        for (int w = 0; w < TARGET_FEATURE_WORDS; w++)
            target.dis_features.bits[w] = hw_feature_mask.bits[w] & ~target.en_features.bits[w];
        target.cpu_features = tp::build_llvm_feature_string(target.en_features, target.dis_features);
    }
#else
    (void)matched_vreg;
    (void)host_vreg;
#endif

    jit_targets.push_back(std::move(target));
    return match_result.first;
}

static uint32_t match_pkgimg_target(void *ctx, const void *id, jl_value_t **rejection_reason)
{
    auto &target = jit_targets.front();
    auto result = match_image_targets(id, target, rejection_reason);
    return result.first;
}

// ============================================================================
// Exported functions
// ============================================================================

#if defined(_CPU_X86_64_) || defined(_CPU_X86_)

extern "C" JL_DLLEXPORT void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType)
{
    asm volatile (
#if defined(__i386__) && defined(__PIC__)
        "xchg %%ebx, %%esi;"
        "cpuid;"
        "xchg %%esi, %%ebx;" :
        "=S" (CPUInfo[1]),
#else
        "cpuid" :
        "=b" (CPUInfo[1]),
#endif
        "=a" (CPUInfo[0]),
        "=c" (CPUInfo[2]),
        "=d" (CPUInfo[3]) :
        "a" (InfoType)
    );
}

extern "C" JL_DLLEXPORT void jl_cpuidex(int32_t CPUInfo[4], int32_t InfoType, int32_t subInfoType)
{
    asm volatile (
#if defined(__i386__) && defined(__PIC__)
        "xchg %%ebx, %%esi;"
        "cpuid;"
        "xchg %%esi, %%ebx;" :
        "=S" (CPUInfo[1]),
#else
        "cpuid" :
        "=b" (CPUInfo[1]),
#endif
        "=a" (CPUInfo[0]),
        "=c" (CPUInfo[2]),
        "=d" (CPUInfo[3]) :
        "a" (InfoType),
        "c" (subInfoType)
    );
}

#endif // x86

JL_DLLEXPORT void jl_dump_host_cpu(void)
{

    jl_safe_printf("CPU: %s\n", host_cpu_name().c_str());
    jl_safe_printf("Features:");
    auto host_feats = tp::get_host_features();
    bool first = true;
    for (uint32_t i = 0; i < num_features; i++) {
        if (feature_test(&host_feats, feature_table[i].bit)) {
            if (first) {
                jl_safe_printf(" %s", feature_table[i].name);
                first = false;
            } else {
                jl_safe_printf(", %s", feature_table[i].name);
            }
        }
    }
    jl_safe_printf("\n");
}

JL_DLLEXPORT jl_value_t *jl_check_pkgimage_clones(char *data)
{
    jl_value_t *rejection_reason = NULL;
    JL_GC_PUSH1(&rejection_reason);
    uint32_t match_idx = match_pkgimg_target(NULL, data, &rejection_reason);
    JL_GC_POP();
    if (match_idx == UINT32_MAX)
        return rejection_reason;
    return jl_nothing;
}

JL_DLLEXPORT jl_value_t *jl_cpu_has_fma(int bits)
{
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
    if ((bits == 32 || bits == 64) && !jit_targets.empty()) {
        const auto &feats = jit_targets.front().en_features;
        if (tp::has_feature(feats, "fma") || tp::has_feature(feats, "fma4"))
            return jl_true;
    }
#elif defined(_CPU_AARCH64_)
    if (bits == 32 || bits == 64)
        return jl_true;
#endif
    return jl_false;
}

// Validate cpu_target string before any processing.
// Called from init.c early in startup.
extern "C" JL_DLLEXPORT void jl_check_cpu_target(const char *cpu_target, int imaging)
{

    if (!cpu_target || !*cpu_target)
        return; // NULL/empty handled elsewhere

    auto target_str = jl_expand_sysimage_keyword(cpu_target);
    if (target_str.empty())
        return;

    // Handle "help": print available CPU targets and exit
    if (target_str == "help" || target_str.find(",help") != std::string::npos) {
        tp::print_cpu_targets();
        exit(0);
    }

    auto specs = tp::resolve_targets_for_llvm(target_str);

    for (auto &s : specs) {
        if (s.flags & tp::TF_UNKNOWN_NAME) {
            jl_safe_printf("Unknown cpu target: \"%s\"\n", s.cpu_name.c_str());
            exit(1);
        }
    }

    if (!imaging) {
        if (specs.size() > 1) {
            jl_safe_printf("More than one command line CPU targets specified "
                      "without a `--output-` flag specified");
            exit(1);
        }
        if (!specs.empty() && (specs[0].flags & tp::TF_CLONE_ALL)) {
            jl_safe_printf("\"clone_all\" feature specified "
                      "without a `--output-` flag specified");
            exit(1);
        }
    }
}

jl_image_t jl_load_sysimg(jl_image_buf_t image, const char *cpu_target)
{

    if (!jit_targets.empty())
        jl_error("JIT targets already initialized");
    return load_sysimg_target(image, match_sysimg_target, (void *)cpu_target);
}

jl_image_t jl_load_pkgimg(jl_image_buf_t image)
{
    if (jit_targets.empty())
        jl_error("JIT targets not initialized");
    if (jit_targets.size() > 1)
        jl_error("Expected only one JIT target");
    return load_sysimg_target(image, match_pkgimg_target, NULL);
}

#ifndef __clang_analyzer__
std::pair<std::string, std::string>
jl_get_llvm_target(const char *cpu_target, bool imaging)
{
    init_jit_targets(cpu_target, imaging);
    auto &spec = jit_targets[0];
    CF_DEBUG("[cpufeatures] jl_get_llvm_target: cpu='%s' features='%s'\n",
             spec.cpu_name.c_str(), spec.cpu_features.c_str());
    return {spec.cpu_name, spec.cpu_features};
}
#endif

#ifndef __clang_analyzer__
const std::pair<std::string, std::string> &jl_get_llvm_disasm_target(void)
{
    // Use generic CPU with all features enabled so the disassembler
    // can decode any instruction (including sysimage clones compiled
    // for targets beyond the current JIT target).
    static const auto res = [] {
        std::string features;
        for (uint32_t i = 0; i < num_features; i++) {
            if (feature_table[i].is_hw) {
                if (!features.empty()) features += ',';
                features += '+';
                features += feature_table[i].name;
            }
        }
        return std::make_pair(std::string("generic"), std::move(features));
    }();
    return res;
}
#endif

#ifndef __clang_gcanalyzer__
jl_clone_targets_t jl_get_llvm_clone_targets(const char *cpu_target)
{
    auto target_str = jl_expand_sysimage_keyword(cpu_target);
    auto specs = tp::resolve_targets_for_llvm(target_str);

    if (specs.empty())
        jl_error("No targets specified");

    jl_clone_targets_t result;

    // Serialized blob for sysimage embedding
    auto blob = tp::serialize_targets(specs);
    result.data.assign(blob.begin(), blob.end());

    // LLVM specs for codegen
    for (auto &s : specs) {
        jl_target_spec_t ele;
        ele.cpu_name = s.cpu_name;
        ele.cpu_features = s.cpu_features;
        ele.base = s.base;
        ele.clone_all = (s.flags & tp::TF_CLONE_ALL) != 0;
        ele.opt_size = (s.flags & tp::TF_OPTSIZE) != 0;
        ele.min_size = (s.flags & tp::TF_MINSIZE) != 0;
        ele.diff = s.diff;
        result.specs.push_back(std::move(ele));
    }
    return result;
}
#endif

extern "C" int jl_test_cpu_feature(jl_cpu_feature_t feature)
{
    auto host_feats = tp::get_host_features();
    if (feature >= TARGET_FEATURE_WORDS * 64)
        return 0;
    return feature_test(&host_feats, feature);
}

// ============================================================================
// Cross-architecture CPU/feature queries
// ============================================================================

extern "C" JL_DLLEXPORT size_t jl_cpufeatures_nbytes(void)
{
    return sizeof(FeatureBits);
}

extern "C" JL_DLLEXPORT int jl_cpufeatures_lookup(const char *cpu_name,
                                                    uint8_t *features_out,
                                                    size_t bufsize)
{
    if (bufsize < sizeof(FeatureBits))
        return -1;
    const CPUEntry *entry = find_cpu(cpu_name);
    if (!entry)
        return -1;
    FeatureBits hw;
    for (int i = 0; i < TARGET_FEATURE_WORDS; i++)
        hw.bits[i] = entry->features.bits[i] & hw_feature_mask.bits[i];
    memcpy(features_out, &hw, sizeof(FeatureBits));
    return 0;
}

extern "C" JL_DLLEXPORT void jl_cpufeatures_host(uint8_t *features_out, size_t bufsize)
{
    if (bufsize < sizeof(FeatureBits))
        return;
    auto fb = tp::get_host_features();
    for (int i = 0; i < TARGET_FEATURE_WORDS; i++)
        fb.bits[i] &= hw_feature_mask.bits[i];
    memcpy(features_out, &fb, sizeof(FeatureBits));
}

extern "C" JL_DLLEXPORT size_t jl_cpufeatures_cross_lookup(
        const char *arch, const char *cpu_name,
        uint8_t *features_out, size_t bufsize)
{
    tp::CrossFeatureBits fb;
    if (!tp::cross_lookup_cpu(arch, cpu_name, fb))
        return 0;
    size_t nbytes = fb.num_words * sizeof(uint64_t);
    if (bufsize < nbytes)
        return 0;
    memcpy(features_out, fb.bits, nbytes);
    return nbytes;
}

extern "C" JL_DLLEXPORT size_t jl_cpufeatures_cross_nbytes(const char *arch)
{
    return tp::cross_feature_words(arch) * sizeof(uint64_t);
}

extern "C" JL_DLLEXPORT unsigned jl_cpufeatures_cross_num_features(const char *arch)
{
    return tp::cross_num_features(arch);
}

extern "C" JL_DLLEXPORT unsigned jl_cpufeatures_cross_num_cpus(const char *arch)
{
    return tp::cross_num_cpus(arch);
}

extern "C" JL_DLLEXPORT const char *jl_cpufeatures_cross_feature_name(const char *arch, unsigned idx)
{
    return tp::cross_feature_name(arch, idx);
}

extern "C" JL_DLLEXPORT int jl_cpufeatures_cross_feature_bit(const char *arch, unsigned idx)
{
    return tp::cross_feature_bit_at(arch, idx);
}

extern "C" JL_DLLEXPORT const char *jl_cpufeatures_cross_cpu_name(const char *arch, unsigned idx)
{
    return tp::cross_cpu_name(arch, idx);
}

// ============================================================================
// FPU control
// ============================================================================

#if defined(_CPU_X86_64_) || defined(_CPU_X86_)

#include <xmmintrin.h>

static uint32_t subnormal_flags = [] {
    int32_t info[4];
    jl_cpuid(info, 0);
    if (info[0] >= 1) {
        jl_cpuid(info, 1);
        if (info[3] & (1 << 26)) {
            return 0x00008040u;
        }
        else if (info[3] & (1 << 25)) {
            return 0x00008000u;
        }
    }
    return 0u;
}();

extern "C" JL_DLLEXPORT int32_t jl_get_zero_subnormals(void)
{
    return _mm_getcsr() & subnormal_flags;
}

extern "C" JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    uint32_t flags = subnormal_flags;
    if (flags) {
        uint32_t state = _mm_getcsr();
        if (isZero) state |= flags;
        else state &= ~flags;
        _mm_setcsr(state);
        return 0;
    }
    return isZero;
}

extern "C" JL_DLLEXPORT int32_t jl_get_default_nans(void) { return 0; }
extern "C" JL_DLLEXPORT int32_t jl_set_default_nans(int8_t isDefault) { return isDefault; }

#elif defined(_CPU_AARCH64_)

extern "C" JL_DLLEXPORT int32_t jl_get_zero_subnormals(void)
{
    uint64_t fpcr;
    asm volatile ("mrs %0, fpcr" : "=r"(fpcr));
    return (fpcr & (1 << 24)) != 0;
}

extern "C" JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    uint64_t fpcr;
    asm volatile ("mrs %0, fpcr" : "=r"(fpcr));
    if (isZero) fpcr |= (1 << 24);
    else fpcr &= ~(uint64_t)(1 << 24);
    asm volatile ("msr fpcr, %0" :: "r"(fpcr));
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_get_default_nans(void)
{
    uint64_t fpcr;
    asm volatile ("mrs %0, fpcr" : "=r"(fpcr));
    return (fpcr & (1 << 25)) != 0;
}

extern "C" JL_DLLEXPORT int32_t jl_set_default_nans(int8_t isDefault)
{
    uint64_t fpcr;
    asm volatile ("mrs %0, fpcr" : "=r"(fpcr));
    if (isDefault) fpcr |= (1 << 25);
    else fpcr &= ~(uint64_t)(1 << 25);
    asm volatile ("msr fpcr, %0" :: "r"(fpcr));
    return 0;
}

#else

extern "C" JL_DLLEXPORT int32_t jl_get_zero_subnormals(void) { return 0; }
extern "C" JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero) { return isZero; }
extern "C" JL_DLLEXPORT int32_t jl_get_default_nans(void) { return 0; }
extern "C" JL_DLLEXPORT int32_t jl_set_default_nans(int8_t isDefault) { return isDefault; }

#endif


// ============================================================================
// Global exports (defined after backend)
// ============================================================================

JL_DLLEXPORT jl_value_t *jl_get_cpu_name(void)
{
    return jl_cstr_to_string(host_cpu_name().c_str());
}

JL_DLLEXPORT jl_value_t *jl_get_cpu_features(void)
{
    return jl_cstr_to_string(get_host_feature_string().c_str());
}

#ifndef __clang_analyzer__
extern "C" JL_DLLEXPORT jl_value_t* jl_reflect_clone_targets() {
    auto targets = jl_get_llvm_clone_targets(jl_options.cpu_target);
    auto &data = targets.data;
    jl_value_t *arr = (jl_value_t*)jl_alloc_array_1d(jl_array_uint8_type, data.size());
    uint8_t *out = jl_array_data(arr, uint8_t);
    memcpy(out, data.data(), data.size());
    return arr;
}
#endif

extern "C" JL_DLLEXPORT jl_value_t *jl_get_sysimage_cpu_target(void) {
    if (sysimage_cpu_target.empty()) {
        return jl_cstr_to_string("native");
    }
    return jl_cstr_to_string(sysimage_cpu_target.c_str());
}

void jl_set_sysimage_cpu_target(const char *cpu_target) {
    if (cpu_target) {
        sysimage_cpu_target = cpu_target;
    }
}
