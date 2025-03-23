// This file is a part of Julia. License is MIT: https://julialang.org/license

// Processor feature detection

#include "llvm-version.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Support/raw_ostream.h>

#include "processor.h"

#include "julia.h"
#include "julia_internal.h"

#include <map>
#include <algorithm>

#include "julia_assert.h"

#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif

#include <iostream>

// CPU target string is a list of strings separated by `;` each string starts with a CPU
// or architecture name and followed by an optional list of features separated by `,`.
// A "generic" or empty CPU name means the basic required feature set of the target ISA
// which is at least the architecture the C/C++ runtime is compiled with.

// CPU dispatch needs to determine the version to be used by the sysimg as well as
// the target and feature used by the JIT. Currently the only limitation on JIT target
// and feature is matching register size between the sysimg and JIT so that SIMD vectors
// can be passed correctly. This means disabling AVX and AVX2 if AVX was not enabled
// in sysimg and disabling AVX512 if it was not enabled in sysimg.
// This also possibly means that SVE needs to be disabled on AArch64 if sysimg doesn't have it
// enabled.

// CPU dispatch starts by first deciding the max feature set and CPU requested for JIT.
// This is the host or the target specified on the command line with features unavailable
// on the host disabled. All sysimg targets that require features not available in this set
// will be ignored.

// The next step is matching CPU name.
// If exact name match with compatible feature set exists, all versions without name match
// are ignored.
// This step will query LLVM first so it can accept CPU names that is recognized by LLVM but
// not by us (yet) when LLVM is enabled.

// If there are still more than one candidates, a feature match is performed.
// The ones with the largest register size will be used
// (i.e. AVX512 > AVX2/AVX > SSE, SVE > ASIMD). If there's a tie, the one with the most features
// enabled will be used. If there's still a tie the one that appears later in the list will be
// used. (i.e. the order in the version list is significant in this case).

// Features that are not recognized will be passed to LLVM directly during codegen
// but ignored otherwise.

// A few special features are supported:
// 1. `clone_all`
//
//     This forces the target to have all functions in sysimg cloned.
//     When used in negative form (i.e. `-clone_all`), this disables full clone that's
//     enabled by default for certain targets.
//
// 2. `base([0-9]*)`
//
//     This specifies the (0-based) base target index. The base target is the target
//     that the current target is based on, i.e. the functions that are not being cloned
//     will use the version in the base target. This option causes the base target to be
//     fully cloned (as if `clone_all` is specified for it) if it is not the default target (0).
//     The index can only be smaller than the current index.
//
// 3. `opt_size`
//
//     Optimize for size with minimum performance impact. Clang/GCC's `-Os`.
//
// 4. `min_size`
//
//     Optimize only for size. Clang's `-Oz`.

JL_DLLEXPORT bool jl_processor_print_help = false;

namespace {

// Helper functions to test/set feature bits

template<typename T1, typename T2, typename T3>
static inline bool test_bits(T1 v, T2 mask, T3 test)
{
    return T3(v & mask) == test;
}

template<typename T1, typename T2>
static inline bool test_all_bits(T1 v, T2 mask)
{
    return test_bits(v, mask, mask);
}

template<typename T1, typename T2>
static inline bool test_nbit(const T1 &bits, T2 _bitidx)
{
    auto bitidx = static_cast<uint32_t>(_bitidx);
    auto u32idx = bitidx / 32;
    auto bit = bitidx % 32;
    return (bits[u32idx] & (1 << bit)) != 0;
}

template<typename T>
static inline void unset_bits(T &bits) JL_NOTSAFEPOINT
{
    (void)bits;
}

template<typename T, typename T1, typename... Rest>
static inline void unset_bits(T &bits, T1 _bitidx, Rest... rest) JL_NOTSAFEPOINT
{
    auto bitidx = static_cast<uint32_t>(_bitidx);
    auto u32idx = bitidx / 32;
    auto bit = bitidx % 32;
    bits[u32idx] = bits[u32idx] & ~uint32_t(1 << bit);
    unset_bits(bits, rest...);
}

template<typename T, typename T1>
static inline void set_bit(T &bits, T1 _bitidx, bool val)
{
    auto bitidx = static_cast<uint32_t>(_bitidx);
    auto u32idx = bitidx / 32;
    auto bit = bitidx % 32;
    if (val) {
        bits[u32idx] = bits[u32idx] | uint32_t(1 << bit);
    }
    else {
        bits[u32idx] = bits[u32idx] & ~uint32_t(1 << bit);
    }
}

// Helper functions to create feature masks

// This can be `std::array<uint32_t,n>` on C++14
template<size_t n>
struct FeatureList {
    uint32_t eles[n];
    uint32_t &operator[](size_t pos) JL_NOTSAFEPOINT
    {
        return eles[pos];
    }
    constexpr const uint32_t &operator[](size_t pos) const
    {
        return eles[pos];
    }
    inline int nbits() const
    {
        int cnt = 0;
        for (size_t i = 0; i < n; i++)
            cnt += llvm::popcount(eles[i]);
        return cnt;
    }
    inline bool empty() const
    {
        for (size_t i = 0; i < n; i++) {
            if (eles[i]) {
                return false;
            }
        }
        return true;
    }
};

static inline constexpr uint32_t add_feature_mask_u32(uint32_t mask, uint32_t u32idx)
{
    return mask;
}

template<typename T, typename... Rest>
static inline constexpr uint32_t add_feature_mask_u32(uint32_t mask, uint32_t u32idx,
                                                      T bit, Rest... args)
{
    return add_feature_mask_u32(mask | ((int(bit) >= 0 && int(bit) / 32 == (int)u32idx) ?
                                        (1 << (int(bit) % 32)) : 0),
                                u32idx, args...);
}

template<typename... Args>
static inline constexpr uint32_t get_feature_mask_u32(uint32_t u32idx, Args... args)
{
    return add_feature_mask_u32(uint32_t(0), u32idx, args...);
}

template<uint32_t... Is> struct seq{};
template<uint32_t N, uint32_t... Is>
struct gen_seq : gen_seq<N-1, N-1, Is...>{};
template<uint32_t... Is>
struct gen_seq<0, Is...> : seq<Is...>{};

template<size_t n, uint32_t... I, typename... Args>
static inline constexpr FeatureList<n>
_get_feature_mask(seq<I...>, Args... args)
{
    return FeatureList<n>{{get_feature_mask_u32(I, args...)...}};
}

template<size_t n, typename... Args>
static inline constexpr FeatureList<n> get_feature_masks(Args... args)
{
    return _get_feature_mask<n>(gen_seq<n>(), args...);
}

template<size_t n, uint32_t... I>
static inline constexpr FeatureList<n>
_feature_mask_or(seq<I...>, const FeatureList<n> &a, const FeatureList<n> &b)
{
    return FeatureList<n>{{(a[I] | b[I])...}};
}

template<size_t n>
static inline constexpr FeatureList<n> operator|(const FeatureList<n> &a, const FeatureList<n> &b)
{
    return _feature_mask_or<n>(gen_seq<n>(), a, b);
}

template<size_t n, uint32_t... I>
static inline constexpr FeatureList<n>
_feature_mask_and(seq<I...>, const FeatureList<n> &a, const FeatureList<n> &b)
{
    return FeatureList<n>{{(a[I] & b[I])...}};
}

template<size_t n>
static inline constexpr FeatureList<n> operator&(const FeatureList<n> &a, const FeatureList<n> &b)
{
    return _feature_mask_and<n>(gen_seq<n>(), a, b);
}

template<size_t n, uint32_t... I>
static inline constexpr FeatureList<n>
_feature_mask_not(seq<I...>, const FeatureList<n> &a)
{
    return FeatureList<n>{{(~a[I])...}};
}

template<size_t n>
static inline constexpr FeatureList<n> operator~(const FeatureList<n> &a)
{
    return _feature_mask_not<n>(gen_seq<n>(), a);
}

template<size_t n>
static inline void mask_features(const FeatureList<n> masks, uint32_t *features)
{
    for (size_t i = 0; i < n; i++) {
        features[i] = features[i] & masks[i];
    }
}

// Turn feature list to a string the LLVM accept
static inline std::string join_feature_strs(const llvm::ArrayRef<std::string> &strs)
{
    size_t nstr = strs.size();
    if (!nstr)
        return std::string("");
    std::string str = strs[0];
    for (size_t i = 1; i < nstr; i++)
        str += ',' + strs[i];
    return str;
}

static inline void append_ext_features(std::string &features, const std::string &ext_features)
{
    if (ext_features.empty())
        return;
    if (!features.empty())
        features.push_back(',');
    features.append(ext_features);
}

static inline void append_ext_features(llvm::SmallVectorImpl<std::string> &features,
                                       const std::string &ext_features)
{
    if (ext_features.empty())
        return;
    const char *start = ext_features.c_str();
    const char *p = start;
    for (; *p; p++) {
        if (*p == ',') {
            features.emplace_back(start, p - start);
            start = p + 1;
        }
    }
    if (p > start) {
        features.emplace_back(start, p - start);
    }
}

/**
 * Target specific type/constant definitions, always enable.
 */

template<typename CPU, size_t n>
struct CPUSpec {
    const char *name;
    CPU cpu;
    CPU fallback;
    uint32_t llvmver;
    FeatureList<n> features;
};

struct FeatureDep {
    uint32_t feature;
    uint32_t dep;
};

// Recursively enable all features that the current feature set depends on.
template<size_t n>
static inline void enable_depends(FeatureList<n> &features, const FeatureDep *deps, size_t ndeps)
{
    bool changed = true;
    while (changed) {
        changed = false;
        for (ssize_t i = ndeps - 1; i >= 0; i--) {
            auto &dep = deps[i];
            if (!test_nbit(features, dep.feature) || test_nbit(features, dep.dep))
                continue;
            set_bit(features, dep.dep, true);
            changed = true;
        }
    }
}

// Recursively disable all features that the current feature set does not provide.
template<size_t n>
static inline void disable_depends(FeatureList<n> &features, const FeatureDep *deps, size_t ndeps)
{
    bool changed = true;
    while (changed) {
        changed = false;
        for (ssize_t i = ndeps - 1; i >= 0; i--) {
            auto &dep = deps[i];
            if (!test_nbit(features, dep.feature) || test_nbit(features, dep.dep))
                continue;
            unset_bits(features, dep.feature);
            changed = true;
        }
    }
}

template<typename CPU, size_t n>
static const CPUSpec<CPU,n> *find_cpu(uint32_t cpu, const CPUSpec<CPU,n> *cpus, uint32_t ncpus)
{
    for (uint32_t i = 0; i < ncpus; i++) {
        if (cpu == uint32_t(cpus[i].cpu)) {
            return &cpus[i];
        }
    }
    return nullptr;
}

template<typename CPU, size_t n>
static const CPUSpec<CPU,n> *find_cpu(llvm::StringRef name, const CPUSpec<CPU,n> *cpus,
                                      uint32_t ncpus)
{
    for (uint32_t i = 0; i < ncpus; i++) {
        if (name == cpus[i].name) {
            return &cpus[i];
        }
    }
    return nullptr;
}

template<typename CPU, size_t n>
static const char *find_cpu_name(uint32_t cpu, const CPUSpec<CPU,n> *cpus, uint32_t ncpus)
{
    if (auto *spec = find_cpu(cpu, cpus, ncpus))
        return spec->name;
    return "generic";
}

JL_UNUSED static uint32_t find_feature_bit(const FeatureName *features, size_t nfeatures,
                                           const char *str, size_t len)
{
    for (size_t i = 0; i < nfeatures; i++) {
        auto &feature = features[i];
        if (strncmp(feature.name, str, len) == 0 && feature.name[len] == 0) {
            return feature.bit;
        }
    }
    return UINT32_MAX;
}

// This is how we save the target identification.
// CPU name is saved as string instead of binary data like features because
// 1. CPU ID is less stable (they are not bound to hardware/OS API)
// 2. We need to support CPU names that are not recognized by us and therefore doesn't have an ID
// 3. CPU name is trivial to parse
static inline llvm::SmallVector<uint8_t, 0>
serialize_target_data(llvm::StringRef name, uint32_t nfeature, const uint32_t *features_en,
                      const uint32_t *features_dis, llvm::StringRef ext_features)
{
    llvm::SmallVector<uint8_t, 0> res;
    auto add_data = [&] (const void *data, size_t sz) {
        if (sz == 0)
            return;
        size_t old_sz = res.size();
        res.resize(old_sz + sz);
        memcpy(&res[old_sz], data, sz);
    };
    add_data(&nfeature, 4);
    add_data(features_en, 4 * nfeature);
    add_data(features_dis, 4 * nfeature);
    uint32_t namelen = name.size();
    add_data(&namelen, 4);
    add_data(name.data(), namelen);
    uint32_t ext_features_len = ext_features.size();
    add_data(&ext_features_len, 4);
    add_data(ext_features.data(), ext_features_len);
    return res;
}

template<size_t n>
static inline llvm::SmallVector<uint8_t, 0>
serialize_target_data(llvm::StringRef name, const FeatureList<n> &features_en,
                      const FeatureList<n> &features_dis, llvm::StringRef ext_features)
{
    return serialize_target_data(name, n, &features_en[0], &features_dis[0], ext_features);
}

template<size_t n>
struct TargetData {
    std::string name;
    std::string ext_features;
    struct {
        FeatureList<n> features;
        uint32_t flags;
    } en, dis;
    int base;
};

// In addition to the serialized data, the first `uint32_t` gives the number of targets saved
// and each target has a `uint32_t` flag before the serialized target data.
template<size_t n>
static inline llvm::SmallVector<TargetData<n>, 0> deserialize_target_data(const uint8_t *data)
{
    auto load_data = [&] (void *dest, size_t sz) {
        memcpy(dest, data, sz);
        data += sz;
    };
    auto load_string = [&] () {
        uint32_t len;
        load_data(&len, 4);
        std::string res((const char*)data, len);
        data += len;
        return res;
    };
    uint32_t ntarget;
    load_data(&ntarget, 4);
    llvm::SmallVector<TargetData<n>, 0> res(ntarget);
    for (uint32_t i = 0; i < ntarget; i++) {
        auto &target = res[i];
        load_data(&target.en.flags, 4);
        target.dis.flags = 0;
        // Starting serialized target data
        uint32_t nfeature;
        load_data(&nfeature, 4);
        assert(nfeature == n);
        load_data(&target.en.features[0], 4 * n);
        load_data(&target.dis.features[0], 4 * n);
        target.name = load_string();
        target.ext_features = load_string();
        target.base = 0;
    }
    return res;
}

// Try getting clone base argument. Return 1-based index. Return 0 if match failed.
static inline int get_clone_base(const char *start, const char *end)
{
    const char *prefix = "base(";
    const int prefix_len = strlen(prefix);
    if (end - start <= prefix_len)
        return 0;
    if (memcmp(start, prefix, prefix_len) != 0)
        return 0;
    start += prefix_len;
    if (*start > '9' || *start < '0')
        return 0;
    char *digit_end;
    auto idx = strtol(start, &digit_end, 10);
    if (idx < 0)
        return 0;
    if (*digit_end != ')' || digit_end + 1 != end)
        return 0;
    return (int)idx + 1;
}

// Parse cmdline string. This handles `clone_all` and `base` special features.
// Other feature names will be passed to `feature_cb` for target dependent parsing.
template<size_t n, typename F>
static inline llvm::SmallVector<TargetData<n>, 0>
parse_cmdline(const char *option, F &&feature_cb)
{
    if (!option)
        abort();

    llvm::SmallVector<TargetData<n>, 0> res;
    TargetData<n> arg{};
    auto reset_arg = [&] {
        res.push_back(arg);
        arg.name.clear();
        arg.ext_features.clear();
        memset(&arg.en.features[0], 0, 4 * n);
        memset(&arg.dis.features[0], 0, 4 * n);
        arg.en.flags = 0;
        arg.dis.flags = 0;
    };
    const char *start = option;
    for (const char *p = option; ; p++) {
        switch (*p) {
        case ',':
        case ';':
        case '\0': {
            bool done = *p == '\0';
            bool next_target = *p == ';' || done;
            if (arg.name.empty()) {
                if (p == start)
                    jl_error("Invalid target option: empty CPU name");
                arg.name.append(start, p - start);
                if (arg.name == "help") {
                    arg.name = "native";
                    jl_processor_print_help = true;
                }
                start = p + 1;
                if (next_target)
                    reset_arg();
                if (done)
                    return res;
                continue;
            }
            bool disable = false;
            const char *full = start;
            const char *fname = full;
            start = p + 1;
            if (*full == '-') {
                disable = true;
                fname++;
            }
            else if (*full == '+') {
                fname++;
            }
            if (llvm::StringRef(fname, p - fname) == "clone_all") {
                if (!disable) {
                    arg.en.flags |= JL_TARGET_CLONE_ALL;
                    arg.dis.flags &= ~JL_TARGET_CLONE_ALL;
                }
                else {
                    arg.dis.flags |= JL_TARGET_CLONE_ALL;
                    arg.en.flags &= ~JL_TARGET_CLONE_ALL;
                }
            }
            else if (llvm::StringRef(fname, p - fname) == "opt_size") {
                if (disable)
                    jl_error("Invalid target option: disabled opt_size.");
                if (arg.en.flags & JL_TARGET_MINSIZE)
                    jl_error("Conflicting target option: both opt_size and min_size are specified.");
                arg.en.flags |= JL_TARGET_OPTSIZE;
            }
            else if (llvm::StringRef(fname, p - fname) == "min_size") {
                if (disable)
                    jl_error("Invalid target option: disabled min_size.");
                if (arg.en.flags & JL_TARGET_OPTSIZE)
                    jl_error("Conflicting target option: both opt_size and min_size are specified.");
                arg.en.flags |= JL_TARGET_MINSIZE;
            }
            else if (int base = get_clone_base(fname, p)) {
                if (disable)
                    jl_error("Invalid target option: disabled base index.");
                base -= 1;
                if (base >= (int)res.size())
                    jl_error("Invalid target option: base index must refer to a previous target.");
                if (res[base].dis.flags & JL_TARGET_CLONE_ALL ||
                    !(res[base].en.flags & JL_TARGET_CLONE_ALL))
                    jl_error("Invalid target option: base target must be clone_all.");
                arg.base = base;
            }
            else if (llvm::StringRef(fname, p - fname) == "help") {
                jl_processor_print_help = true;
            }
            else {
                FeatureList<n> &list = disable ? arg.dis.features : arg.en.features;
                if (!feature_cb(fname, p - fname, list)) {
                    if (!arg.ext_features.empty())
                        arg.ext_features += ',';
                    arg.ext_features += disable ? '-' : '+';
                    arg.ext_features.append(fname, p - fname);
                }
            }
            if (next_target)
                reset_arg();
            if (done) {
                return res;
            }
        }
            JL_FALLTHROUGH;
        default:
            continue;
        }
    }
}

// Cached version of command line parsing
template<size_t n, typename F>
static inline llvm::SmallVector<TargetData<n>, 0> &get_cmdline_targets(const char *cpu_target, F &&feature_cb)
{
    static llvm::SmallVector<TargetData<n>, 0> targets =
        parse_cmdline<n>(cpu_target, std::forward<F>(feature_cb));
    return targets;
}

// Load sysimg, use the `callback` for dispatch and perform all relocations
// for the selected target.
template<typename F>
static inline jl_image_t parse_sysimg(jl_image_buf_t image, F &&callback, void *ctx)
{
    JL_TIMING(LOAD_IMAGE, LOAD_Processor);
    jl_image_t res{};

    if (image.kind != JL_IMAGE_KIND_SO)
        return res;

    const jl_image_pointers_t *pointers = (const jl_image_pointers_t *)image.pointers;
    const void *ids = pointers->target_data;
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

    llvm::SmallVector<void*, 0> fvars(pointers->header->nfvars);
    llvm::SmallVector<const char*, 0> gvars(pointers->header->ngvars);

    llvm::SmallVector<std::pair<uint32_t, void*>, 0> clones;

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
        llvm::SmallVector<void**, 0> base_ptrs(0);
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

template<typename T>
static inline void check_cmdline(T &&cmdline, bool imaging)
{
    assert(cmdline.size() > 0);
    // It's unclear what does specifying multiple target when not generating
    // sysimg means. Make it an error for now.
    if (!imaging) {
        if (cmdline.size() > 1) {
            jl_safe_printf("More than one command line CPU targets specified "
                      "without a `--output-` flag specified");
            exit(1);
        }
        if (cmdline[0].en.flags & JL_TARGET_CLONE_ALL) {
            jl_safe_printf("\"clone_all\" feature specified "
                      "without a `--output-` flag specified");
            exit(1);
        }
        if (cmdline[0].en.flags & JL_TARGET_OPTSIZE) {
            jl_safe_printf("\"opt_size\" feature specified "
                      "without a `--output-` flag specified");
            exit(1);
        }
        if (cmdline[0].en.flags & JL_TARGET_MINSIZE) {
            jl_safe_printf("\"min_size\" feature specified "
                      "without a `--output-` flag specified");
            exit(1);
        }
    }
}

struct SysimgMatch {
    uint32_t best_idx{UINT32_MAX};
    int vreg_size{0};
};

// Find the best match in the sysimg.
// Select the best one based on the largest vector register and largest compatible feature set.
template<typename S, typename T, typename F>
static inline SysimgMatch match_sysimg_targets(S &&sysimg, T &&target, F &&max_vector_size, jl_value_t **rejection_reason)
{
    SysimgMatch match;
    bool match_name = false;
    int feature_size = 0;
    llvm::SmallVector<const char *, 0> rejection_reasons;
    rejection_reasons.reserve(sysimg.size());
    for (uint32_t i = 0; i < sysimg.size(); i++) {
        auto &imgt = sysimg[i];
        if (!(imgt.en.features & target.dis.features).empty()) {
            // Check sysimg enabled features against runtime disabled features
            // This is valid (and all what we can do)
            // even if one or both of the targets are unknown.
            rejection_reasons.push_back("Rejecting this target due to use of runtime-disabled features\n");
            continue;
        }
        if (imgt.name == target.name) {
            if (!match_name) {
                match_name = true;
                match.vreg_size = 0;
                feature_size = 0;
            }
        }
        else if (match_name) {
            rejection_reasons.push_back("Rejecting this target since another target has a cpu name match\n");
            continue;
        }
        int new_vsz = max_vector_size(imgt.en.features);
        if (match.vreg_size > new_vsz) {
            rejection_reasons.push_back("Rejecting this target since another target has a larger vector register size\n");
            continue;
        }
        int new_feature_size = imgt.en.features.nbits();
        if (match.vreg_size < new_vsz) {
            match.best_idx = i;
            match.vreg_size = new_vsz;
            feature_size = new_feature_size;
            rejection_reasons.push_back("Updating best match to this target due to larger vector register size\n");
            continue;
        }
        if (new_feature_size < feature_size) {
            rejection_reasons.push_back("Rejecting this target since another target has a larger feature set\n");
            continue;
        }
        match.best_idx = i;
        feature_size = new_feature_size;
        rejection_reasons.push_back("Updating best match to this target\n");
    }
    if (match.best_idx == UINT32_MAX) {
        // Construct a nice error message for debugging purposes
        std::string error_msg = "Unable to find compatible target in cached code image.\n";
        for (size_t i = 0; i < rejection_reasons.size(); i++) {
            error_msg += "Target ";
            error_msg += std::to_string(i);
            error_msg += " (";
            error_msg += sysimg[i].name;
            error_msg += "): ";
            error_msg += rejection_reasons[i];
        }
        if (rejection_reason)
            *rejection_reason = jl_pchar_to_string(error_msg.data(), error_msg.size());
    }
    return match;
}

// Debug helper

template<typename CPU, size_t n>
static inline void dump_cpu_spec(uint32_t cpu, const FeatureList<n> &features,
                                 const FeatureName *feature_names, uint32_t nfeature_names,
                                 const CPUSpec<CPU,n> *cpus, uint32_t ncpus)
{
    bool cpu_found = false;
    for (uint32_t i = 0;i < ncpus;i++) {
        if (cpu == uint32_t(cpus[i].cpu)) {
            cpu_found = true;
            jl_safe_printf("CPU: %s\n", cpus[i].name);
            break;
        }
    }
    if (!cpu_found)
        jl_safe_printf("CPU: generic\n");
    jl_safe_printf("Features:");
    bool first = true;
    for (uint32_t i = 0;i < nfeature_names;i++) {
        if (test_nbit(&features[0], feature_names[i].bit)) {
            if (first) {
                jl_safe_printf(" %s", feature_names[i].name);
                first = false;
            }
            else {
                jl_safe_printf(", %s", feature_names[i].name);
            }
        }
    }
    jl_safe_printf("\n");
}

}

static std::string jl_get_cpu_name_llvm(void)
{
    return llvm::sys::getHostCPUName().str();
}

static std::string jl_get_cpu_features_llvm(void)
{
#if JL_LLVM_VERSION >= 190000
    auto HostFeatures = llvm::sys::getHostCPUFeatures();
#else
    llvm::StringMap<bool> HostFeatures;
    llvm::sys::getHostCPUFeatures(HostFeatures);
#endif
    std::string attr;
    for (auto &ele: HostFeatures) {
        if (ele.getValue()) {
            if (!attr.empty()) {
                attr.append(",+");
            }
            else {
                attr.append("+");
            }
            attr.append(ele.getKey().str());
        }
    }
    // Explicitly disabled features need to be added at the end so that
    // they are not re-enabled by other features that implies them by default.
    for (auto &ele: HostFeatures) {
        if (!ele.getValue()) {
            if (!attr.empty()) {
                attr.append(",-");
            }
            else {
                attr.append("-");
            }
            attr.append(ele.getKey().str());
        }
    }
    return attr;
}

#if defined(_CPU_X86_) || defined(_CPU_X86_64_)

#include "processor_x86.cpp"

#elif defined(_CPU_AARCH64_) || defined(_CPU_ARM_)

#include "processor_arm.cpp"

#else

#include "processor_fallback.cpp"

#endif

JL_DLLEXPORT jl_value_t *jl_get_cpu_name(void)
{
    return jl_cstr_to_string(host_cpu_name().c_str());
}

JL_DLLEXPORT jl_value_t *jl_get_cpu_features(void)
{
    return jl_cstr_to_string(jl_get_cpu_features_llvm().c_str());
}

extern "C" JL_DLLEXPORT jl_value_t* jl_reflect_clone_targets() {
    auto specs = jl_get_llvm_clone_targets(jl_options.cpu_target);
    const uint32_t base_flags = 0;
    llvm::SmallVector<uint8_t, 0> data;
    auto push_i32 = [&] (uint32_t v) {
        uint8_t buff[4];
        memcpy(buff, &v, 4);
        data.insert(data.end(), buff, buff + 4);
    };
    push_i32(specs.size());
    for (uint32_t i = 0; i < specs.size(); i++) {
        push_i32(base_flags | (specs[i].flags & JL_TARGET_UNKNOWN_NAME));
        auto &specdata = specs[i].data;
        data.insert(data.end(), specdata.begin(), specdata.end());
    }

    jl_value_t *arr = (jl_value_t*)jl_alloc_array_1d(jl_array_uint8_type, data.size());
    uint8_t *out = jl_array_data(arr, uint8_t);
    memcpy(out, data.data(), data.size());
    return arr;
}

extern "C" JL_DLLEXPORT void jl_reflect_feature_names(const FeatureName **fnames, size_t *nf) {
    *fnames = feature_names;
    *nf = nfeature_names;
}
