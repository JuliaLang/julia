// This file is a part of Julia. License is MIT: https://julialang.org/license

// Processor feature detection

#include "llvm-version.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Support/raw_ostream.h>

#include "processor.h"

#include "julia.h"
#include "julia_internal.h"

#include <map>
#include <algorithm>

#include "julia_assert.h"

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

bool jl_processor_print_help = false;

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
static inline void unset_bits(T &bits)
{
    (void)bits;
}

template<typename T, typename T1, typename... Rest>
static inline void unset_bits(T &bits, T1 _bitidx, Rest... rest)
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
    uint32_t &operator[](size_t pos)
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
            cnt += llvm::countPopulation(eles[i]);
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
static inline std::string join_feature_strs(const std::vector<std::string> &strs)
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

static inline void append_ext_features(std::vector<std::string> &features,
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

struct FeatureName {
    const char *name;
    uint32_t bit; // bit index into a `uint32_t` array;
    uint32_t llvmver; // 0 if it is available on the oldest LLVM version we support
};

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
    return (uint32_t)-1;
}

// This is how we save the target identification.
// CPU name is saved as string instead of binary data like features because
// 1. CPU ID is less stable (they are not bound to hardware/OS API)
// 2. We need to support CPU names that are not recognized by us and therefore doesn't have an ID
// 3. CPU name is trivial to parse
static inline std::vector<uint8_t> serialize_target_data(llvm::StringRef name,
                                                         uint32_t nfeature,
                                                         const uint32_t *features_en,
                                                         const uint32_t *features_dis,
                                                         llvm::StringRef ext_features)
{
    std::vector<uint8_t> res;
    auto add_data = [&] (const void *data, size_t sz) {
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
static inline std::vector<uint8_t> serialize_target_data(llvm::StringRef name,
                                                         const FeatureList<n> &features_en,
                                                         const FeatureList<n> &features_dis,
                                                         llvm::StringRef ext_features)
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
static inline std::vector<TargetData<n>> deserialize_target_data(const uint8_t *data)
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
    std::vector<TargetData<n>> res(ntarget);
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
static inline std::vector<TargetData<n>>
parse_cmdline(const char *option, F &&feature_cb)
{
    if (!option)
        option = "native";
    std::vector<TargetData<n>> res;
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
static inline std::vector<TargetData<n>> &get_cmdline_targets(F &&feature_cb)
{
    static std::vector<TargetData<n>> targets =
        parse_cmdline<n>(jl_options.cpu_target, std::forward<F>(feature_cb));
    return targets;
}

// Load sysimg, use the `callback` for dispatch and perform all relocations
// for the selected target.
template<typename F>
static inline jl_sysimg_fptrs_t parse_sysimg(void *hdl, F &&callback)
{
    jl_sysimg_fptrs_t res = {nullptr, 0, nullptr, 0, nullptr, nullptr};

    // .data base
    char *data_base;
    jl_dlsym(hdl, "jl_sysimg_gvars_base", (void**)&data_base, 1);
    // .text base
    char *text_base;
    jl_dlsym(hdl, "jl_sysimg_fvars_base", (void**)&text_base, 1);
    res.base = text_base;

    int32_t *offsets;
    jl_dlsym(hdl, "jl_sysimg_fvars_offsets", (void**)&offsets, 1);
    uint32_t nfunc = offsets[0];
    res.offsets = offsets + 1;

    void *ids;
    jl_dlsym(hdl, "jl_dispatch_target_ids", &ids, 1);
    uint32_t target_idx = callback(ids);

    int32_t *reloc_slots;
    jl_dlsym(hdl, "jl_dispatch_reloc_slots", (void **)&reloc_slots, 1);
    const uint32_t nreloc = reloc_slots[0];
    reloc_slots += 1;
    uint32_t *clone_idxs;
    int32_t *clone_offsets;
    jl_dlsym(hdl, "jl_dispatch_fvars_idxs", (void**)&clone_idxs, 1);
    jl_dlsym(hdl, "jl_dispatch_fvars_offsets", (void**)&clone_offsets, 1);
    uint32_t tag_len = clone_idxs[0];
    clone_idxs += 1;

    assert(tag_len & jl_sysimg_tag_mask);
    std::vector<const int32_t*> base_offsets = {res.offsets};
    // Find target
    for (uint32_t i = 0;i < target_idx;i++) {
        uint32_t len = jl_sysimg_val_mask & tag_len;
        if (jl_sysimg_tag_mask & tag_len) {
            if (i != 0)
                clone_offsets += nfunc;
            clone_idxs += len + 1;
        }
        else {
            clone_offsets += len;
            clone_idxs += len + 2;
        }
        tag_len = clone_idxs[-1];
        base_offsets.push_back(tag_len & jl_sysimg_tag_mask ? clone_offsets : nullptr);
    }

    bool clone_all = (tag_len & jl_sysimg_tag_mask) != 0;
    // Fill in return value
    if (clone_all) {
        // clone_all
        if (target_idx != 0) {
            res.offsets = clone_offsets;
        }
    }
    else {
        uint32_t base_idx = clone_idxs[0];
        assert(base_idx < target_idx);
        if (target_idx != 0) {
            res.offsets = base_offsets[base_idx];
            assert(res.offsets);
        }
        clone_idxs++;
        res.nclones = tag_len;
        res.clone_offsets = clone_offsets;
        res.clone_idxs = clone_idxs;
    }
    // Do relocation
    uint32_t reloc_i = 0;
    uint32_t len = jl_sysimg_val_mask & tag_len;
    for (uint32_t i = 0; i < len; i++) {
        uint32_t idx = clone_idxs[i];
        int32_t offset;
        if (clone_all) {
            offset = res.offsets[idx];
        }
        else if (idx & jl_sysimg_tag_mask) {
            idx = idx & jl_sysimg_val_mask;
            offset = clone_offsets[i];
        }
        else {
            continue;
        }
        bool found = false;
        for (; reloc_i < nreloc; reloc_i++) {
            auto reloc_idx = ((const uint32_t*)reloc_slots)[reloc_i * 2];
            if (reloc_idx == idx) {
                found = true;
                auto slot = (const void**)(data_base + reloc_slots[reloc_i * 2 + 1]);
                *slot = offset + res.base;
            }
            else if (reloc_idx > idx) {
                break;
            }
        }
        assert(found && "Cannot find GOT entry for cloned function.");
        (void)found;
    }

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
            jl_error("More than one command line CPU targets specified "
                     "without a `--output-` flag specified");
        }
        if (cmdline[0].en.flags & JL_TARGET_CLONE_ALL) {
            jl_error("\"clone_all\" feature specified "
                     "without a `--output-` flag specified");
        }
        if (cmdline[0].en.flags & JL_TARGET_OPTSIZE) {
            jl_error("\"opt_size\" feature specified "
                     "without a `--output-` flag specified");
        }
        if (cmdline[0].en.flags & JL_TARGET_MINSIZE) {
            jl_error("\"min_size\" feature specified "
                     "without a `--output-` flag specified");
        }
    }
}

struct SysimgMatch {
    uint32_t best_idx{(uint32_t)-1};
    int vreg_size{0};
};

// Find the best match in the sysimg.
// Select the best one based on the largest vector register and largest compatible feature set.
template<typename S, typename T, typename F>
static inline SysimgMatch match_sysimg_targets(S &&sysimg, T &&target, F &&max_vector_size)
{
    SysimgMatch match;
    bool match_name = false;
    int feature_size = 0;
    for (uint32_t i = 0; i < sysimg.size(); i++) {
        auto &imgt = sysimg[i];
        if (!(imgt.en.features & target.dis.features).empty()) {
            // Check sysimg enabled features against runtime disabled features
            // This is valid (and all what we can do)
            // even if one or both of the targets are unknown.
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
            continue;
        }
        int new_vsz = max_vector_size(imgt.en.features);
        if (match.vreg_size > new_vsz)
            continue;
        int new_feature_size = imgt.en.features.nbits();
        if (match.vreg_size < new_vsz) {
            match.best_idx = i;
            match.vreg_size = new_vsz;
            feature_size = new_feature_size;
            continue;
        }
        if (new_feature_size < feature_size)
            continue;
        match.best_idx = i;
        feature_size = new_feature_size;
    }
    if (match.best_idx == (uint32_t)-1)
        jl_error("Unable to find compatible target in system image.");
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

#if defined(_CPU_X86_) || defined(_CPU_X86_64_)

#include "processor_x86.cpp"

#elif defined(_CPU_AARCH64_) || defined(_CPU_ARM_)

#include "processor_arm.cpp"

#else

#include "processor_fallback.cpp"

#endif
