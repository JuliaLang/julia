// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/Support/raw_ostream.h>

#include "processor.h"
#include "julia.h"
#include "julia_internal.h"

#include <archspec/archspec.hpp>
#include <archspec/llvm_compat.hpp>

#include <algorithm>
#include <climits>
#include <cstring>
#include <set>
#include "julia_assert.h"

#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif

JL_DLLEXPORT bool jl_processor_print_help = false;

namespace {

// Check if debug output is enabled via JULIA_DEBUG=archspec or JULIA_DEBUG=all
static bool debug_enabled() {
    static int enabled = -1;
    if (enabled == -1) {
        const char *debug_env = getenv("JULIA_DEBUG");
        enabled = debug_env && (strstr(debug_env, "archspec") || strstr(debug_env, "all"));
    }
    return enabled;
}

#define ARCHSPEC_DEBUG(...) do { if (debug_enabled()) jl_safe_printf(__VA_ARGS__); } while (0)

static const archspec::Microarchitecture &get_host_arch() {
    static archspec::Microarchitecture host = archspec::host();
    return host;
}

static const std::string &host_cpu_name() {
    static std::string name = []() {
        return archspec::get_llvm_cpu_name(get_host_arch());
    }();
    return name;
}

static const std::string &host_cpu_features() {
    static std::string features = []() {
        return archspec::get_llvm_features_string(get_host_arch());
    }();
    return features;
}

static bool host_has_feature(const std::string &feat) {
    const auto &host = get_host_arch();
    return host.has_feature(feat);
}

struct TargetData {
    std::string name;
    std::string features;  // Extra features only
    uint32_t flags;
    int base;
};

static std::string get_full_features(const TargetData &t, std::string_view arch_family, bool is_host = false) {
    std::string cpu_features;
    if (is_host)
        cpu_features = host_cpu_features();
    else if (t.name != "generic")
        cpu_features = archspec::get_llvm_features_for_cpu(t.name, arch_family);
    
    if (!t.features.empty()) {
        if (!cpu_features.empty())
            cpu_features += ",";
        cpu_features += t.features;
    }
    return cpu_features;
}

// Global state
static llvm::SmallVector<TargetData, 0> jit_targets;           // Targets for JIT compilation
static llvm::SmallVector<TargetData, 0> cmdline_targets;       // Parsed from JULIA_CPU_TARGET
static TargetData selected_sysimg_target;                      // The target selected from sysimage (for pkgimg matching)
static std::string sysimage_cpu_target;
static bool targets_initialized = false;

static void ensure_cmdline_targets_initialized(const char *cpu_target);

static std::string flags_to_string(uint32_t flags) {
    std::string s;
    if (flags & JL_TARGET_CLONE_ALL)     s += "clone_all,";
    if (flags & JL_TARGET_CLONE_CPU)     s += "clone_cpu,";
    if (flags & JL_TARGET_CLONE_LOOP)    s += "clone_loop,";
    if (flags & JL_TARGET_CLONE_SIMD)    s += "clone_simd,";
    if (flags & JL_TARGET_CLONE_MATH)    s += "clone_math,";
    if (flags & JL_TARGET_OPTSIZE)       s += "opt_size,";
    if (flags & JL_TARGET_MINSIZE)       s += "min_size,";
    if (flags & JL_TARGET_CLONE_FLOAT16) s += "clone_float16,";
    if (flags & JL_TARGET_CLONE_BFLOAT16) s += "clone_bfloat16,";
    if (!s.empty()) s.pop_back(); // remove trailing comma
    return s.empty() ? "(none)" : s;
}

// Get archspec name for a CPU (for debug output)
// Shows the normalized name that archspec uses internally
static std::string get_archspec_name(const std::string &llvm_name) {
    const auto &host_arch = get_host_arch();
    return archspec::normalize_cpu_name(host_arch.family(), llvm_name);
}

static void print_targets(const char *label, const llvm::SmallVector<TargetData, 0> &targets, bool show_archspec = false) {
    ARCHSPEC_DEBUG("[archspec] %s: %zu target(s)\n", label, targets.size());
    for (size_t i = 0; i < targets.size(); i++) {
        const auto &t = targets[i];
        ARCHSPEC_DEBUG("[archspec]   [%zu] name='%s'", i, t.name.c_str());
        if (show_archspec && t.name != "generic") {
            std::string archspec_name = get_archspec_name(t.name);
            if (archspec_name != t.name)
                ARCHSPEC_DEBUG(" → %s", archspec_name.c_str());
        }
        ARCHSPEC_DEBUG(" features='%s' base=%d flags=[%s]\n",
                       t.features.c_str(), t.base, flags_to_string(t.flags).c_str());
    }
}

// Serialize base, name, and features - flags are handled separately by aotcompile.cpp
static llvm::SmallVector<uint8_t, 0> serialize_target_data(const TargetData &t) {
    llvm::SmallVector<uint8_t, 0> res;
    auto add_u32 = [&](uint32_t v) {
        res.insert(res.end(), reinterpret_cast<uint8_t*>(&v), 
                   reinterpret_cast<uint8_t*>(&v) + 4);
    };
    auto add_str = [&](const std::string &s) {
        add_u32(static_cast<uint32_t>(s.size()));
        res.insert(res.end(), s.begin(), s.end());
    };
    // Note: flags are written by aotcompile.cpp, not here
    add_u32(static_cast<uint32_t>(t.base));
    add_str(t.name);
    add_str(t.features);
    return res;
}

// Deserialize format: [ntarget][flags0][base0][name0][features0][flags1][base1][name1][features1]...
// Note: flags are written by jl_reflect_clone_targets/aotcompile.cpp before each target's data
static llvm::SmallVector<TargetData, 0> deserialize_target_data(const uint8_t *data) {
    llvm::SmallVector<TargetData, 0> res;
    auto read_u32 = [&]() -> uint32_t {
        uint32_t v;
        memcpy(&v, data, 4);
        data += 4;
        return v;
    };
    auto read_str = [&]() -> std::string {
        uint32_t len = read_u32();
        std::string s(reinterpret_cast<const char*>(data), len);
        data += len;
        return s;
    };
    
    uint32_t ntarget = read_u32();
    res.resize(ntarget);
    
    for (uint32_t i = 0; i < ntarget; i++) {
        auto &t = res[i];
        t.flags = read_u32();  // flags written by aotcompile.cpp
        t.base = static_cast<int>(read_u32());
        t.name = read_str();
        t.features = read_str();
    }
    return res;
}

// Check if a CPU name is valid (known to archspec or is a special name)
static bool is_valid_cpu_name(const std::string &name) {
    if (name.empty())
        return false;
    // Special names that are always valid
    if (name == "native" || name == "generic" || name == "help" || name == "sysimage")
        return true;
    
    // Check if the CPU exists in the archspec database
    const auto &db = archspec::MicroarchitectureDatabase::instance();
    
    // Try direct lookup first
    auto uarch = db.get(name);
    if (uarch)
        return true;
    
    // Try with normalized name (handle LLVM naming conventions)
    const auto &host_arch = get_host_arch();
    std::string normalized = archspec::normalize_cpu_name(host_arch.family(), name);
    if (!normalized.empty() && normalized != name) {
        uarch = db.get(normalized);
        if (uarch)
            return true;
    }
    
    // Also check x86 and aarch64 families explicitly for cross-platform validation
    std::string norm_x86 = archspec::normalize_cpu_name("x86_64", name);
    if (!norm_x86.empty()) {
        uarch = db.get(norm_x86);
        if (uarch)
            return true;
    }
    
    std::string norm_arm = archspec::normalize_cpu_name("aarch64", name);
    if (!norm_arm.empty()) {
        uarch = db.get(norm_arm);
        if (uarch)
            return true;
    }
    
    return false;
}

static llvm::SmallVector<TargetData, 0> parse_cmdline(const char *option) {
    llvm::SmallVector<TargetData, 0> res;
    if (!option) 
        return res;
    
    // Check for empty string
    if (option[0] == '\0')
        jl_error("Invalid target option: empty CPU name");
    
    std::string processed_option;
    std::string_view opt(option);
    if (opt.substr(0, 8) == "sysimage" && (opt.size() == 8 || opt[8] == ';')) {
        jl_value_t *target_str = jl_get_sysimage_cpu_target();
        if (target_str) {
            processed_option = std::string(jl_string_data(target_str), jl_string_len(target_str));
            if (opt.size() > 8)
                processed_option += opt.substr(8);
            option = processed_option.c_str();
        }
    }
    
    // Parse target string manually
    // Format: "cpu1,+feat1,-feat2;cpu2,+feat3"
    std::string_view str(option);
    size_t pos = 0;
    
    while (pos <= str.size()) {
        TargetData t;
        t.flags = 0;
        t.base = 0;
        
        // Find end of this target (semicolon or end)
        size_t end = str.find(';', pos);
        if (end == std::string_view::npos)
            end = str.size();
        
        std::string_view target_str = str.substr(pos, end - pos);
        
        // Check for empty target at the beginning (e.g., ";cpu2" or empty string)
        if (target_str.empty() || target_str[0] == ',' || target_str[0] == ';') {
            if (res.empty()) // First target must have a name
                jl_error("Invalid target option: empty CPU name");
        }
        
        // Parse tokens within this target (comma-separated)
        size_t tpos = 0;
        bool first_token = true;
        while (tpos <= target_str.size()) {
            size_t tend = target_str.find(',', tpos);
            if (tend == std::string_view::npos)
                tend = target_str.size();
            
            if (tend > tpos) {
                std::string_view token = target_str.substr(tpos, tend - tpos);
                
                if (first_token) {
                    t.name = std::string(token);
                    first_token = false;
                    // Validate CPU name (but not for "help" which is handled specially)
                    if (t.name != "help" && !is_valid_cpu_name(t.name)) {
                        std::string msg = "Invalid target option: unknown CPU name \"" + t.name + "\"";
                        jl_error(msg.c_str());
                    }
                } else {
                    // Check for special modifiers
                    bool disable = (token[0] == '-');
                    std::string_view name = disable ? token.substr(1) : 
                                            (token[0] == '+' ? token.substr(1) : token);
                    
                    if (name == "clone_all") {
                        if (!disable)
                            t.flags |= JL_TARGET_CLONE_ALL;
                    } else if (name == "opt_size") {
                        if (!disable)
                            t.flags |= JL_TARGET_OPTSIZE;
                    } else if (name == "min_size") {
                        if (!disable)
                            t.flags |= JL_TARGET_MINSIZE;
                    } else if (name.substr(0, 5) == "base(") {
                        // Parse base(N)
                        size_t paren = name.find(')');
                        if (paren != std::string_view::npos) {
                            std::string idx_str(name.substr(5, paren - 5));
                            t.base = std::stoi(idx_str);
                        }
                    } else {
                        // Regular feature (+feat or -feat)
                        if (!t.features.empty())
                            t.features += ',';
                        t.features += token;
                    }
                }
            }
            
            if (tend >= target_str.size())
                break;
            tpos = tend + 1;
        }
        
        if (!t.name.empty()) {
            if (t.name == "native")
                t.name = host_cpu_name();
            
            // Set default clone flags for non-first targets
            if (!res.empty()) {
                t.flags |= JL_TARGET_CLONE_CPU;
                t.flags |= JL_TARGET_CLONE_LOOP;
                
                if (t.features.find("+avx") != std::string::npos ||
                    t.features.find("+sve") != std::string::npos ||
                    t.features.find("+neon") != std::string::npos) {
                    t.flags |= JL_TARGET_CLONE_SIMD;
                }
                if (t.features.find("+fma") != std::string::npos) {
                    t.flags |= JL_TARGET_CLONE_MATH;
                }
            }
            
            res.push_back(std::move(t));
        }
        
        if (end >= str.size())
            break;
        pos = end + 1;
    }
    
    if (res.empty()) {
        TargetData t;
        t.name = host_cpu_name();
        t.features = "";
        t.flags = 0;
        t.base = 0;
        res.push_back(std::move(t));
    }
    
    return res;
}

static void ensure_cmdline_targets_initialized(const char *cpu_target) {
    if (targets_initialized)
        return;
    targets_initialized = true;
    
    ARCHSPEC_DEBUG("[archspec] Initializing targets from '%s'\n", cpu_target ? cpu_target : "(null)");
    
    std::string host = host_cpu_name();
    std::string host_archspec = get_archspec_name(host);
    ARCHSPEC_DEBUG("[archspec] Host CPU: %s", host.c_str());
    if (host != host_archspec)
        ARCHSPEC_DEBUG(" (archspec: %s)", host_archspec.c_str());
    ARCHSPEC_DEBUG("\n");
    
    cmdline_targets = parse_cmdline(cpu_target);
    print_targets("Parsed command line targets", cmdline_targets);
    
    // Show the requested target with its archspec name
    if (!cmdline_targets.empty()) {
        const auto &req = cmdline_targets[0];
        std::string req_archspec = get_archspec_name(req.name);
        ARCHSPEC_DEBUG("[archspec] Requested target: %s", req.name.c_str());
        if (req.name != req_archspec)
            ARCHSPEC_DEBUG(" (archspec: %s)", req_archspec.c_str());
        ARCHSPEC_DEBUG("\n");
    }
}

static int max_vector_size(const TargetData &t) {
    if (t.features.find("+avx512") != std::string::npos) return 64;
    if (t.features.find("+avx") != std::string::npos) return 32;
    if (t.features.find("+sve") != std::string::npos) return 256;
    return 16;
}

static bool features_compatible(const std::string &required_features) {
    size_t pos = 0;
    while (pos < required_features.size()) {
        size_t end = required_features.find(',', pos);
        if (end == std::string::npos) end = required_features.size();
        
        std::string feat = required_features.substr(pos, end - pos);
        if (!feat.empty() && feat[0] == '+') {
            std::string fname = feat.substr(1);
            if (!host_has_feature(fname)) {
                return false;
            }
        }
        pos = end + 1;
    }
    return true;
}

// Count how many features cpu_b has that also exist in cpu_a
// Higher count means cpu_b is a better match for cpu_a
static int count_matching_features(const std::string &cpu_a, const std::string &cpu_b) {
    if (cpu_a == "generic" || cpu_b == "generic")
        return 0;
    
    const auto &host_arch = get_host_arch();
    std::string arch_family = host_arch.family();
    
    std::string a_archspec = archspec::normalize_cpu_name(arch_family, cpu_a);
    std::string b_archspec = archspec::normalize_cpu_name(arch_family, cpu_b);
    
    auto &db = archspec::MicroarchitectureDatabase::instance();
    auto a_opt = db.get(a_archspec);
    auto b_opt = db.get(b_archspec);
    
    if (!a_opt.has_value() || !b_opt.has_value())
        return 0;
    
    const auto &a_features = a_opt.value().get().features();
    const auto &b_features = b_opt.value().get().features();
    
    int count = 0;
    for (const auto &feat : b_features) {
        if (a_features.find(feat) != a_features.end())
            count++;
    }
    return count;
}

// Parse feature modifiers string (e.g., "-rdrnd,-avx512") into sets of removed features
// Features are in LLVM format, we translate to archspec format
static std::set<std::string> parse_removed_features(const std::string &features, 
                                                     const std::string &arch_family) {
    std::set<std::string> removed;
    if (features.empty()) return removed;
    
    size_t pos = 0;
    while (pos < features.size()) {
        size_t comma = features.find(',', pos);
        if (comma == std::string::npos) comma = features.size();
        
        std::string feat = features.substr(pos, comma - pos);
        if (!feat.empty() && feat[0] == '-') {
            // Remove the '-' prefix and translate to archspec format
            std::string llvm_feat = feat.substr(1);
            std::string archspec_feat = archspec::map_llvm_feature_to_archspec(arch_family, llvm_feat);
            removed.insert(archspec_feat);
        }
        pos = comma + 1;
    }
    return removed;
}

// Check if cpu_a is compatible with (can run code compiled for) cpu_b using archspec
// Returns true if cpu_a >= cpu_b (cpu_a has all features required by cpu_b)
// If reason is provided and compatibility fails, explains why
// b_feature_modifiers: optional LLVM feature modifiers for cpu_b (e.g., "-rdrnd" means don't require rdrnd)
static bool cpu_compatible(const std::string &cpu_a, const std::string &cpu_b, 
                           std::string *reason = nullptr,
                           const std::string &b_feature_modifiers = "") {
    // Any CPU can run generic code
    if (cpu_b == "generic") {
        return true;
    }
    
    // Same CPU is always compatible
    if (cpu_a == cpu_b) {
        return true;
    }
    
    // If cpu_a is "generic", it can only run generic code (already handled above)
    // generic cannot run code compiled for specific CPUs
    if (cpu_a == "generic") {
        if (reason) {
            *reason = "'generic' can only run code compiled for 'generic', not '" + cpu_b + "'";
        }
        return false;
    }
    
    // Get archspec microarchitectures for both
    const auto &host_arch = get_host_arch();
    std::string arch_family = host_arch.family();
    
    // Normalize LLVM names to archspec format
    std::string a_archspec = archspec::normalize_cpu_name(arch_family, cpu_a);
    std::string b_archspec = archspec::normalize_cpu_name(arch_family, cpu_b);
    
    auto &db = archspec::MicroarchitectureDatabase::instance();
    auto a_opt = db.get(a_archspec);
    auto b_opt = db.get(b_archspec);
    
    // If cpu_a isn't in the database, can't determine compatibility
    if (!a_opt.has_value()) {
        if (reason) {
            *reason = a_archspec + " not in archspec database";
        }
        return false;
    }
    
    // If cpu_b isn't in the database, can't determine compatibility
    if (!b_opt.has_value()) {
        if (reason) {
            *reason = b_archspec + " not in archspec database";
        }
        return false;
    }
    
    // Check feature compatibility: cpu_a can run code for cpu_b if it has all of cpu_b's features
    const archspec::Microarchitecture &a_uarch = a_opt.value().get();
    const archspec::Microarchitecture &b_uarch = b_opt.value().get();
    
    // Parse feature modifiers to get removed features
    // e.g., "-rdrnd" means the target was compiled WITHOUT using rdrnd
    std::set<std::string> removed_features = parse_removed_features(b_feature_modifiers, arch_family);
    
    // Find which features cpu_a is missing compared to cpu_b
    // But don't count features that were explicitly removed from the target
    const auto &a_features = a_uarch.features();
    const auto &b_features = b_uarch.features();
    std::string missing;
    for (const auto &feat : b_features) {
        // Skip features that were removed from the target (they're not required)
        if (removed_features.count(feat) > 0) {
            continue;
        }
        if (a_features.find(feat) == a_features.end()) {
            if (!missing.empty()) missing += ", ";
            missing += feat;
        }
    }
    
    // Compatible if no missing features (regardless of CPU lineage)
    if (missing.empty()) {
        return true;
    }
    
    if (reason) {
        *reason = "missing: " + missing;
    }
    return false;
}

static uint32_t match_sysimg_targets(const llvm::SmallVector<TargetData, 0> &sysimg,
                                      const TargetData &host_target,
                                      jl_value_t **rejection_reason) {
    uint32_t best_idx = UINT32_MAX;
    int best_vreg = 0;
    int best_specificity = -1;  // Higher = more specific match
    
    const std::string &host_name = host_target.name;
    
    for (uint32_t i = 0; i < sysimg.size(); ++i) {
        const auto &t = sysimg[i];
        
        // Check compatibility using archspec
        // Pass t.features to account for removed features (e.g., "-rdrnd" means no rdrand required)
        std::string compat_reason;
        if (!cpu_compatible(host_name, t.name, &compat_reason, t.features)) {
            ARCHSPEC_DEBUG("[archspec]   target[%u] '%s' not compatible: %s\n", 
                           i, t.name.c_str(), compat_reason.c_str());
            continue;
        }
        
        // Calculate specificity: exact match > ancestor match > generic
        int specificity = 0;
        if (t.name == host_name)
            specificity = 2;
        else if (t.name != "generic")
            specificity = 1;  // Ancestor match
        // generic has specificity 0
        
        // Prefer more specific matches
        if (specificity < best_specificity)
            continue;
        
        if (specificity > best_specificity) {
            best_specificity = specificity;
            best_idx = UINT32_MAX;
            best_vreg = 0;
        }
        
        // Among same specificity, check features
        if (specificity == best_specificity && specificity < 2) {
            // Only do feature checking for non-name-match cases
            if (!features_compatible(t.features)) {
                continue;
            }
        }
        
        int vreg = max_vector_size(t);
        if (best_idx == UINT32_MAX || vreg > best_vreg || 
            (vreg == best_vreg && i > best_idx)) {
            best_idx = i;
            best_vreg = vreg;
        }
    }
    
    if (best_idx == UINT32_MAX && rejection_reason) {
        *rejection_reason = jl_cstr_to_string("No compatible target found in sysimage");
    }
    
    return best_idx;
}

// Select the best sysimage target for the current execution context.
//
// Target Selection Algorithm:
// ---------------------------
// When Julia starts, we need to select which version of the compiled code to use
// from the sysimage. The sysimage may contain multiple versions compiled for
// different CPU targets (e.g., generic, cortex-a57, apple-m1, etc.).
//
// We have two CPUs to consider:
//   1. The "requested" target: specified via -C flag or JULIA_CPU_TARGET (first entry)
//   2. The "host" CPU: the actual CPU we're running on
//
// For a sysimage target to be usable, BOTH conditions must hold:
//   - requested >= sysimg_target: The requested CPU must be able to run code
//     compiled for the sysimage target (i.e., has all required features)
//   - host >= sysimg_target: The actual host CPU must also be able to run
//     that code (since we're actually executing on the host)
//
// Among all compatible targets, we pick the most specific one:
//   - Specificity 3: Exact match with requested target
//   - Specificity 2: Exact match with host CPU
//   - Specificity 1: Some other named compatible CPU
//   - Specificity 0: generic (always compatible, least optimized)
//
// Example: User runs `julia -C cortex-a72` on an apple-m4 host, sysimage has
// [generic, cortex-a57, apple-m1]:
//   - generic: cortex-a72 >= generic ✓, apple-m4 >= generic ✓ → compatible
//   - cortex-a57: cortex-a72 >= cortex-a57 ✓, but apple-m4 >= cortex-a57 may fail
//     if they're in different architecture lineages
//   - apple-m1: cortex-a72 >= apple-m1 may fail (different lineage)
// Result: generic is selected as the only fully compatible option.
//
static uint32_t sysimg_init_cb(void *ctx, const void *id, jl_value_t **rejection_reason) {
    const char *cpu_target = (const char *)ctx;
    
    auto sysimg = deserialize_target_data(static_cast<const uint8_t*>(id));
    if (sysimg.empty()) {
        if (rejection_reason)
            *rejection_reason = jl_cstr_to_string("No targets in sysimage");
        return UINT32_MAX;
    }
    
    ARCHSPEC_DEBUG("[archspec] sysimg_init: %zu target(s) in image\n", sysimg.size());
    print_targets("Sysimage targets", sysimg, true /* show archspec names */);
    
    ensure_cmdline_targets_initialized(cpu_target);
    
    // The first command-line target is what the user requested (via -C or JULIA_CPU_TARGET)
    const TargetData &requested = cmdline_targets[0];
    
    uint32_t best_idx = UINT32_MAX;
    std::string host_name = host_cpu_name();
    
    ARCHSPEC_DEBUG("[archspec] Looking for target compatible with requested '%s' (host is '%s')\n", 
                   requested.name.c_str(), host_name.c_str());
    
    // Find the best sysimage target that both the requested target and the host can run.
    // We prefer targets with more matching features (closer in the CPU hierarchy).
    int best_match_score = -1;
    
    for (uint32_t i = 0; i < sysimg.size(); i++) {
        const auto &t = sysimg[i];
        
        // Condition 1: requested >= sysimg_target
        // The code compiled for this target must be runnable on the requested CPU
        // Pass t.features to account for removed features (e.g., "-rdrnd" means no rdrand required)
        std::string requested_compat_reason;
        if (!cpu_compatible(requested.name, t.name, &requested_compat_reason, t.features)) {
            ARCHSPEC_DEBUG("[archspec]   [%u] '%s': requested incompatible - %s\n",
                           i, t.name.c_str(), requested_compat_reason.c_str());
            continue;
        }
        
        // Condition 2: host >= sysimg_target  
        // The code must also be runnable on the actual host CPU
        std::string host_compat_reason;
        if (!cpu_compatible(host_name, t.name, &host_compat_reason, t.features)) {
            ARCHSPEC_DEBUG("[archspec]   [%u] '%s': host incompatible - %s\n",
                           i, t.name.c_str(), host_compat_reason.c_str());
            continue;
        }
        
        // Both conditions met - calculate match score based on feature overlap
        // Higher score = more features of sysimg target are present in requested CPU
        int match_score;
        if (t.name == requested.name) {
            match_score = INT_MAX;  // Exact match with requested target (best possible)
        } else if (t.name == "generic") {
            match_score = 0;  // generic is always compatible but least optimized
        } else {
            // Count how many features of this target match the requested CPU
            // This prefers targets that are closer in the CPU hierarchy
            match_score = count_matching_features(requested.name, t.name);
        }
        
        ARCHSPEC_DEBUG("[archspec]   [%u] '%s': ✓ (score=%d)\n",
                       i, t.name.c_str(), match_score);
        
        // Prefer higher match scores
        // When scores are equal and > 0, prefer the current (keeps first high-scoring match)
        // When scores are 0 (generic or unknown CPUs), prefer later indices (more specific targets)
        if (match_score > best_match_score || 
            (match_score == best_match_score && match_score == 0 && t.name != "generic")) {
            best_match_score = match_score;
            best_idx = i;
        }
    }
    
    if (best_idx == UINT32_MAX) {
        ARCHSPEC_DEBUG("[archspec] No compatible target found for requested '%s' on host '%s'\n",
                       requested.name.c_str(), host_name.c_str());
    }
    
    ARCHSPEC_DEBUG("[archspec] sysimg_init: selected target %u\n", best_idx);
    
    if (best_idx != UINT32_MAX) {
        // Store the selected sysimage target for pkgimage matching
        selected_sysimg_target = sysimg[best_idx];
        jit_targets = cmdline_targets;
        ARCHSPEC_DEBUG("[archspec] sysimg_init: selected sysimg target '%s'\n", 
                       selected_sysimg_target.name.c_str());
    }
    
    return best_idx;
}

static uint32_t pkgimg_init_cb(void *ctx, const void *id, jl_value_t **rejection_reason) {
    (void)ctx;
    auto pkgimg = deserialize_target_data(static_cast<const uint8_t*>(id));
    
    if (pkgimg.empty()) {
        if (rejection_reason)
            *rejection_reason = jl_cstr_to_string("No targets in pkgimage");
        return UINT32_MAX;
    }
    
    // If no sysimage was loaded yet, use first pkgimage target
    if (selected_sysimg_target.name.empty())
        return 0;
    
    // Match pkgimage targets against the selected sysimage target (not the host)
    return match_sysimg_targets(pkgimg, selected_sysimg_target, rejection_reason);
}

template<typename F>
static jl_image_t parse_sysimg(jl_image_buf_t image, F &&callback, void *ctx) {
    JL_TIMING(LOAD_IMAGE, LOAD_Processor);
    jl_image_t res{};

    if (image.kind != JL_IMAGE_KIND_SO)
        return res;

    const jl_image_pointers_t *pointers = (const jl_image_pointers_t *)image.pointers;
    const void *ids = pointers->target_data;

    if (pointers->cpu_target_string)
        jl_set_sysimage_cpu_target(pointers->cpu_target_string);

    jl_value_t *rejection_reason = nullptr;
    JL_GC_PUSH1(&rejection_reason);
    uint32_t target_idx = callback(ctx, ids, &rejection_reason);
    
    if (target_idx == UINT32_MAX) {
        if (rejection_reason)
            jl_error(jl_string_ptr(rejection_reason));
        else
            jl_error("No compatible target found");
    }
    JL_GC_POP();

    if (pointers->header->version != 1) {
        jl_error("Image file is not compatible with this version of Julia");
    }

    // The function pointer relocation logic from original processor.cpp
    // This is critical for multiversioning to work
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
        for (uint32_t j = 0; j < target_idx; j++) {
            uint32_t len = jl_sysimg_val_mask & tag_len;
            if (jl_sysimg_tag_mask & tag_len) {
                clone_idxs += len + 1;
                if (j != 0)
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
        
        if (clone_all) {
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
            for (unsigned k = 0; k < (jl_sysimg_val_mask & tag_len); k++) {
                clones[start + k] = {(clone_idxs[k] & ~jl_sysimg_val_mask) | idxs[clone_idxs[k] & jl_sysimg_val_mask], clone_ptrs[k]};
            }
        }
        
        // Do relocation
        uint32_t reloc_i = 0;
        uint32_t len = jl_sysimg_val_mask & tag_len;
        for (uint32_t k = 0; k < len; k++) {
            uint32_t idx = clone_idxs[k];
            void *fptr;
            if (clone_all) {
                fptr = fvar_shard[idx];
            }
            else if (idx & jl_sysimg_tag_mask) {
                idx = idx & jl_sysimg_val_mask;
                fptr = clone_ptrs[k];
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

        // Copy fvars
        auto fidxs = shard.fvar_idxs;
        for (uint32_t k = 0; k < nfunc; k++) {
            fvars[fidxs[k]] = fvar_shard[k];
        }
        
        // Copy gvars - note: gvar_offsets[0] contains count, actual offsets start at [1]
        auto gidxs = shard.gvar_idxs;
        unsigned ngvar_shard = shard.gvar_offsets[0];
        assert(ngvar_shard <= pointers->header->ngvars);
        char *data_base = (char*)shard.gvar_offsets;
        for (uint32_t k = 0; k < ngvar_shard; k++) {
            gvars[gidxs[k]] = data_base + shard.gvar_offsets[k+1];
        }
    }

    // Setup fptrs
    if (!fvars.empty()) {
        auto ptrs = (void**)malloc(sizeof(void*) * fvars.size());
        for (size_t i = 0; i < fvars.size(); i++) {
            assert(fvars[i] && "Missing function pointer!");
            ptrs[i] = fvars[i];
        }
        res.fptrs.ptrs = ptrs;
        res.fptrs.nptrs = fvars.size();
    }

    // Setup gvars
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

    // Setup clones
    if (!clones.empty()) {
        assert(!fvars.empty());
        std::sort(clones.begin(), clones.end(),
            [](const std::pair<uint32_t, void*> &a, const std::pair<uint32_t, void*> &b) {
                return (a.first & jl_sysimg_val_mask) < (b.first & jl_sysimg_val_mask);
        });
        auto clone_ptrs = (void**)malloc(sizeof(void*) * clones.size());
        auto clone_idxs = (uint32_t*)malloc(sizeof(uint32_t) * clones.size());
        for (size_t i = 0; i < clones.size(); i++) {
            clone_idxs[i] = clones[i].first;
            clone_ptrs[i] = clones[i].second;
        }
        res.fptrs.clone_idxs = clone_idxs;
        res.fptrs.clone_ptrs = clone_ptrs;
        res.fptrs.nclones = clones.size();
    }

    res.base = image.base;

    // Initialize PTLS
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

} // anonymous namespace

extern "C" JL_DLLEXPORT jl_value_t *jl_get_cpu_name(void) {
    return jl_cstr_to_string(host_cpu_name().c_str());
}

extern "C" JL_DLLEXPORT jl_value_t *jl_get_cpu_features(void) {
    // Return archspec features as a comma-separated string
    static std::string features = []() {
        const auto &host = get_host_arch();
        std::string result;
        for (const auto &f : host.features()) {
            if (!result.empty())
                result += ", ";
            result += f;
        }
        return result;
    }();
    return jl_cstr_to_string(features.c_str());
}

extern "C" JL_DLLEXPORT void jl_dump_host_cpu(void) {
    const auto &host = get_host_arch();
    jl_safe_printf("CPU: %s\n", host.name().c_str());
    jl_safe_printf("Vendor: %s\n", host.vendor().c_str());
    jl_safe_printf("Family: %s\n", host.family().c_str());
    jl_safe_printf("Features:");
    bool first = true;
    for (const auto &f : host.features()) {
        if (first) {
            jl_safe_printf(" %s", f.c_str());
            first = false;
        } else {
            jl_safe_printf(", %s", f.c_str());
        }
    }
    jl_safe_printf("\n");
}

extern "C" JL_DLLEXPORT jl_value_t *jl_cpu_has_fma(int bits) {
    if (bits != 32 && bits != 64)
        return jl_false;
    
    if (host_has_feature("fma") || host_has_feature("fma4"))
        return jl_true;
    
#ifdef _CPU_AARCH64_
    return jl_true;  // AArch64 always has FMA
#else
    return jl_false;
#endif
}

extern "C" JL_DLLEXPORT jl_value_t *jl_get_sysimage_cpu_target(void) {
    if (sysimage_cpu_target.empty())
        return jl_cstr_to_string(host_cpu_name().c_str());
    return jl_cstr_to_string(sysimage_cpu_target.c_str());
}

void jl_set_sysimage_cpu_target(const char *cpu_target) {
    if (!cpu_target)
        return;
    std::string_view target(cpu_target);
    // Don't store magic keywords - they should be expanded before reaching here
    if (target.substr(0, 8) == "sysimage" || target == "native")
        return;
    sysimage_cpu_target = cpu_target;
}

extern "C" JL_DLLEXPORT int jl_test_cpu_feature(jl_cpu_feature_t feature) {
    // This maps Julia's feature enum to feature names
    // Simplified - returns 0 for unknown features
    (void)feature;
    return 0;
}

extern "C" JL_DLLEXPORT std::pair<std::string, llvm::SmallVector<std::string, 0>>
jl_get_llvm_target(const char *cpu_target, bool imaging, uint32_t &flags) JL_NOTSAFEPOINT {
    flags = 0;

    ensure_cmdline_targets_initialized(cpu_target);

    // Use jit_targets if available (set after sysimg load), otherwise cmdline_targets
    const auto &targets = jit_targets.empty() ? cmdline_targets : jit_targets;
    if (!jit_targets.empty())
        flags = jit_targets[0].flags;

    // Check for invalid multi-target usage when not imaging
    if (!imaging) {
        if (targets.size() > 1) {
            jl_safe_printf("More than one command line CPU targets specified "
                      "without a `--output-` flag specified");
            exit(1);
        }
        if (!targets.empty() && (targets[0].flags & JL_TARGET_CLONE_ALL)) {
            jl_safe_printf("\"clone_all\" feature specified "
                      "without a `--output-` flag specified");
            exit(1);
        }
    }
    
    const auto &t = targets[0];
    
    llvm::SmallVector<std::string, 0> features;
    
    // Helper to split comma-separated features and add to vector
    auto add_features = [&features](const std::string &feat_str) {
        if (feat_str.empty()) return;
        size_t start = 0;
        for (size_t i = 0; i <= feat_str.size(); ++i) {
            if (i == feat_str.size() || feat_str[i] == ',') {
                if (i > start) {
                    features.push_back(feat_str.substr(start, i - start));
                }
                start = i + 1;
            }
        }
    };
    
    // Get CPU features: either from host detection or archspec database
    std::string arch_family = get_host_arch().family();
    bool is_host_cpu = (t.name == host_cpu_name());
    
    if (is_host_cpu) {
        // For host CPU, use archspec's detected features
        add_features(host_cpu_features());
    } else if (t.name != "generic") {
        // For a specific non-generic CPU, look up features from archspec
        std::string cpu_features = archspec::get_llvm_features_for_cpu(t.name, arch_family);
        add_features(cpu_features);
    }
    
    add_features(t.features);
    
    return std::make_pair(t.name, features);
}

extern "C" JL_DLLEXPORT const std::pair<std::string, std::string> &
jl_get_llvm_disasm_target(void) JL_NOTSAFEPOINT {
    static std::pair<std::string, std::string> target = []() {
        std::string cpu = host_cpu_name();
        std::string features = host_cpu_features();
        return std::make_pair(cpu, features);
    }();
    return target;
}

extern "C" JL_DLLEXPORT llvm::SmallVector<jl_target_spec_t, 0>
jl_get_llvm_clone_targets(const char *cpu_target) JL_NOTSAFEPOINT {
    ensure_cmdline_targets_initialized(cpu_target);
    const auto &targets = cmdline_targets;
    
    std::string arch_family;
#ifdef _CPU_AARCH64_
    arch_family = "aarch64";
#elif defined(_CPU_X86_64_)
    arch_family = "x86_64";
#elif defined(_CPU_ARM_)
    arch_family = "arm";
#else
    arch_family = get_host_arch().family();
#endif
    
    // Build base features string for the architecture
    std::string base_features;
#ifdef _CPU_AARCH64_
    base_features = "+neon,+fp-armv8";
#elif defined(_CPU_ARM_)
    base_features = "+v6,+vfp2";
#endif
    
    std::string host_name = host_cpu_name();
    
    llvm::SmallVector<jl_target_spec_t, 0> res;
    for (auto &t : targets) {
        jl_target_spec_t spec;
        spec.cpu_name = t.name;
        
        // Check if this is the host CPU (use runtime detection for features)
        bool is_host = (t.name == host_name);
        
        // Get full features: CPU's inherent features + any user extras
        // For host: use LLVM runtime detection
        // For known CPUs: look up in archspec database
        std::string full_features = get_full_features(t, arch_family, is_host);
        
        // Add base features if present
        if (!base_features.empty()) {
            if (full_features.empty()) {
                spec.cpu_features = base_features;
            } else {
                spec.cpu_features = base_features + "," + full_features;
            }
        } else {
            spec.cpu_features = full_features;
        }
        
        spec.flags = t.flags;
        spec.base = t.base;
        spec.data = serialize_target_data(t);
        res.push_back(std::move(spec));
    }
    
    return res;
}

extern "C" JL_DLLEXPORT jl_value_t *jl_reflect_clone_targets() {
    auto specs = jl_get_llvm_clone_targets(jl_options.cpu_target);
    
    llvm::SmallVector<uint8_t, 0> data;
    auto push_u32 = [&](uint32_t v) {
        data.insert(data.end(), reinterpret_cast<uint8_t*>(&v),
                    reinterpret_cast<uint8_t*>(&v) + 4);
    };
    
    push_u32(static_cast<uint32_t>(specs.size()));
    for (const auto &spec : specs) {
        push_u32(spec.flags);  // flags must be written here
        data.insert(data.end(), spec.data.begin(), spec.data.end());
    }
    
    jl_value_t *arr = (jl_value_t *)jl_alloc_array_1d(jl_array_uint8_type, data.size());
    uint8_t *out = jl_array_data(arr, uint8_t);
    memcpy(out, data.data(), data.size());
    return arr;
}

static const FeatureName empty_features[] = {{nullptr, 0, 0}};

extern "C" JL_DLLEXPORT void jl_reflect_feature_names(const FeatureName **fnames, size_t *nf) {
    *fnames = empty_features;
    *nf = 0;
}

jl_image_t jl_init_processor_sysimg(jl_image_buf_t image, const char *cpu_target) {
    if (!jit_targets.empty())
        jl_error("JIT targets already initialized");
    return parse_sysimg(image, sysimg_init_cb, (void *)cpu_target);
}

jl_image_t jl_init_processor_pkgimg(jl_image_buf_t image) {
    if (jit_targets.empty())
        jl_error("JIT targets not initialized");
    return parse_sysimg(image, pkgimg_init_cb, nullptr);
}

extern "C" JL_DLLEXPORT jl_value_t *jl_check_pkgimage_clones(char *data) {
    if (!data)
        return jl_cstr_to_string("No data provided");
    
    auto targets = deserialize_target_data(reinterpret_cast<uint8_t*>(data));
    
    if (targets.empty()) {
        return jl_cstr_to_string("No targets in pkgimage");
    }
    
    // If no sysimage target selected yet, accept any pkgimage
    if (selected_sysimg_target.name.empty()) {
        return jl_nothing;
    }
    
    jl_value_t *rejection_reason = nullptr;
    // Match pkgimage targets against the selected sysimage target
    uint32_t match = match_sysimg_targets(targets, selected_sysimg_target, &rejection_reason);
    
    if (match == UINT32_MAX) {
        return rejection_reason ? rejection_reason : 
               jl_cstr_to_string("No compatible target");
    }
    
    return jl_nothing;
}

#if defined(_CPU_X86_) || defined(_CPU_X86_64_)
#include <xmmintrin.h>

extern "C" JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero) {
    uint32_t flags = 0x8040;  // FTZ | DAZ
    uint32_t state = _mm_getcsr();
    if (isZero)
        state |= flags;
    else
        state &= ~flags;
    _mm_setcsr(state);
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_get_zero_subnormals(void) {
    return (_mm_getcsr() & 0x8040) == 0x8040;
}

extern "C" JL_DLLEXPORT int32_t jl_set_default_nans(int8_t isDefault) {
    (void)isDefault;
    return isDefault;  // X86 doesn't support default NaNs
}

extern "C" JL_DLLEXPORT int32_t jl_get_default_nans(void) {
    return 0;
}

#elif defined(_CPU_AARCH64_)

extern "C" JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero) {
    uint64_t fpcr;
    asm volatile("mrs %0, fpcr" : "=r"(fpcr));
    if (isZero)
        fpcr |= (1ULL << 24);  // FZ bit
    else
        fpcr &= ~(1ULL << 24);
    asm volatile("msr fpcr, %0" :: "r"(fpcr));
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_get_zero_subnormals(void) {
    uint64_t fpcr;
    asm volatile("mrs %0, fpcr" : "=r"(fpcr));
    return (fpcr & (1ULL << 24)) != 0;
}

extern "C" JL_DLLEXPORT int32_t jl_set_default_nans(int8_t isDefault) {
    uint64_t fpcr;
    asm volatile("mrs %0, fpcr" : "=r"(fpcr));
    if (isDefault)
        fpcr |= (1ULL << 25);  // DN bit
    else
        fpcr &= ~(1ULL << 25);
    asm volatile("msr fpcr, %0" :: "r"(fpcr));
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_get_default_nans(void) {
    uint64_t fpcr;
    asm volatile("mrs %0, fpcr" : "=r"(fpcr));
    return (fpcr & (1ULL << 25)) != 0;
}

#else

extern "C" JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero) {
    (void)isZero;
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_get_zero_subnormals(void) {
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_set_default_nans(int8_t isDefault) {
    (void)isDefault;
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_get_default_nans(void) {
    return 0;
}

#endif
