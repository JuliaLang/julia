// This file is a part of Julia. License is MIT: https://julialang.org/license

// Fallback processor detection and dispatch

static constexpr FeatureName *feature_names = nullptr;
static constexpr uint32_t nfeature_names = 0;

namespace Fallback {

static inline const std::string &host_cpu_name()
{
    static std::string name = jl_get_cpu_name_llvm();
    return name;
}

static const llvm::SmallVector<TargetData<1>, 0> &get_cmdline_targets(const char *cpu_target)
{
    auto feature_cb = [] (const char*, size_t, FeatureList<1>&) {
        return false;
    };
    return ::get_cmdline_targets<1>(cpu_target, feature_cb);
}

static llvm::SmallVector<TargetData<1>, 0> jit_targets;

static TargetData<1> arg_target_data(const TargetData<1> &arg, bool require_host)
{
    TargetData<1> res = arg;
    if (res.name == "native") {
        res.name = host_cpu_name();
        append_ext_features(res.ext_features, jl_get_cpu_features_llvm());
    }
    else {
        res.en.flags |= JL_TARGET_UNKNOWN_NAME;
    }
    return res;
}

static uint32_t sysimg_init_cb(void *ctx, const void *id, jl_value_t **rejection_reason)
{
    // First see what target is requested for the JIT.
    const char *cpu_target = (const char *)ctx;
    auto &cmdline = get_cmdline_targets(cpu_target);
    TargetData<1> target = arg_target_data(cmdline[0], true);
    // Find the last name match or use the default one.
    uint32_t best_idx = 0;
    auto sysimg = deserialize_target_data<1>((const uint8_t*)id);
    for (uint32_t i = 0; i < sysimg.size(); i++) {
        auto &imgt = sysimg[i];
        if (imgt.name == target.name) {
            best_idx = i;
        }
    }
    jit_targets.push_back(std::move(target));
    return best_idx;
}

static uint32_t pkgimg_init_cb(void *ctx, const void *id, jl_value_t **rejection_reason)
{
    TargetData<1> target = jit_targets.front();
    // Find the last name match or use the default one.
    uint32_t best_idx = 0;
    auto pkgimg = deserialize_target_data<1>((const uint8_t*)id);
    for (uint32_t i = 0; i < pkgimg.size(); i++) {
        auto &imgt = pkgimg[i];
        if (imgt.name == target.name) {
            best_idx = i;
        }
    }

    return best_idx;
}

static void ensure_jit_target(const char *cpu_target, bool imaging)
{
    auto &cmdline = get_cmdline_targets(cpu_target);
    check_cmdline(cmdline, imaging);
    if (!jit_targets.empty())
        return;
    for (auto &arg: cmdline) {
        auto data = arg_target_data(arg, jit_targets.empty());
        jit_targets.push_back(std::move(data));
    }
    auto ntargets = jit_targets.size();
    // Now decide the clone condition.
    for (size_t i = 1; i < ntargets; i++) {
        auto &t = jit_targets[i];
        t.en.flags |= JL_TARGET_CLONE_ALL;
    }
}

static std::pair<std::string,llvm::SmallVector<std::string, 0>>
get_llvm_target_noext(const TargetData<1> &data)
{
    return std::make_pair(data.name, llvm::SmallVector<std::string, 0>{});
}

static std::pair<std::string,llvm::SmallVector<std::string, 0>>
get_llvm_target_vec(const TargetData<1> &data)
{
    auto res0 = get_llvm_target_noext(data);
    append_ext_features(res0.second, data.ext_features);
    return res0;
}

static std::pair<std::string,std::string>
get_llvm_target_str(const TargetData<1> &data)
{
    auto res0 = get_llvm_target_noext(data);
    auto features = join_feature_strs(res0.second);
    append_ext_features(features, data.ext_features);
    return std::make_pair(std::move(res0.first), std::move(features));
}

}

using namespace Fallback;

jl_image_t jl_init_processor_sysimg(jl_image_buf_t image, const char *cpu_target)
{
    if (!jit_targets.empty())
        jl_error("JIT targets already initialized");
    return parse_sysimg(image, sysimg_init_cb, (void *)cpu_target);
}

jl_image_t jl_init_processor_pkgimg(jl_image_buf_t image)
{
    if (jit_targets.empty())
        jl_error("JIT targets not initialized");
    if (jit_targets.size() > 1)
        jl_error("Expected only one JIT target");
    return parse_sysimg(image, pkgimg_init_cb, NULL);
}

std::pair<std::string,llvm::SmallVector<std::string, 0>> jl_get_llvm_target(const char *cpu_target, bool imaging, uint32_t &flags)
{
    ensure_jit_target(cpu_target, imaging);
    flags = jit_targets[0].en.flags;
    return get_llvm_target_vec(jit_targets[0]);
}

const std::pair<std::string,std::string> &jl_get_llvm_disasm_target(void)
{
    static const auto res = get_llvm_target_str(TargetData<1>{host_cpu_name(),
                jl_get_cpu_features_llvm(), {{}, 0}, {{}, 0}, 0});
    return res;
}
#ifndef __clang_gcanalyzer__
llvm::SmallVector<jl_target_spec_t, 0> jl_get_llvm_clone_targets(const char *cpu_target)
{

    auto &cmdline = get_cmdline_targets(cpu_target);
    check_cmdline(cmdline, true);
    llvm::SmallVector<TargetData<1>, 0> image_targets;
    for (auto &arg: cmdline) {
        auto data = arg_target_data(arg, image_targets.empty());
        image_targets.push_back(std::move(data));
    }
    auto ntargets = image_targets.size();
    // Now decide the clone condition.
    for (size_t i = 1; i < ntargets; i++) {
        auto &t = image_targets[i];
        t.en.flags |= JL_TARGET_CLONE_ALL;
    }
    if (image_targets.empty())
        jl_error("No image targets found");
    llvm::SmallVector<jl_target_spec_t, 0> res;
    for (auto &target: image_targets) {
        jl_target_spec_t ele;
        std::tie(ele.cpu_name, ele.cpu_features) = get_llvm_target_str(target);
        ele.data = serialize_target_data(target.name, target.en.features,
                                         target.dis.features, target.ext_features);
        ele.flags = target.en.flags;
        ele.base = 0;
        res.push_back(ele);
    }
    return res;
}
#endif

JL_DLLEXPORT jl_value_t *jl_cpu_has_fma(int bits)
{
    return jl_false; // Match behaviour of have_fma in src/llvm-cpufeatures.cpp (assume false)
}

JL_DLLEXPORT void jl_dump_host_cpu(void)
{
    jl_safe_printf("CPU: %s\n", host_cpu_name().c_str());
    jl_safe_printf("Features: %s\n", jl_get_cpu_features_llvm().c_str());
}

JL_DLLEXPORT jl_value_t* jl_check_pkgimage_clones(char *data)
{
    jl_value_t *rejection_reason = NULL;
    JL_GC_PUSH1(&rejection_reason);
    uint32_t match_idx = pkgimg_init_cb(NULL, data, &rejection_reason);
    JL_GC_POP();
    if (match_idx == UINT32_MAX)
        return rejection_reason;
    return jl_nothing;
}

extern "C" int jl_test_cpu_feature(jl_cpu_feature_t)
{
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_get_zero_subnormals(void)
{
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    return isZero;
}

extern "C" JL_DLLEXPORT int32_t jl_get_default_nans(void)
{
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_set_default_nans(int8_t isDefault)
{
    return isDefault;
}
