// This file is a part of Julia. License is MIT: https://julialang.org/license

// Fallback processor detection and dispatch

namespace Fallback {

static inline const std::string &host_cpu_name()
{
    static std::string name = jl_get_cpu_name_llvm();
    return name;
}

static const std::vector<TargetData<1>> &get_cmdline_targets(void)
{
    auto feature_cb = [] (const char*, size_t, FeatureList<1>&) {
        return false;
    };
    return ::get_cmdline_targets<1>(feature_cb);
}

static std::vector<TargetData<1>> jit_targets;

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

static uint32_t sysimg_init_cb(const void *id)
{
    // First see what target is requested for the JIT.
    auto &cmdline = get_cmdline_targets();
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
    target = sysimg[best_idx];
    jit_targets.push_back(std::move(target));
    return best_idx;
}

static void ensure_jit_target(bool imaging)
{
    auto &cmdline = get_cmdline_targets();
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

static std::pair<std::string,std::vector<std::string>>
get_llvm_target_noext(const TargetData<1> &data)
{
    return std::make_pair(data.name, std::vector<std::string>{});
}

static std::pair<std::string,std::vector<std::string>>
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

jl_sysimg_fptrs_t jl_init_processor_sysimg(void *hdl)
{
    if (!jit_targets.empty())
        jl_error("JIT targets already initialized");
    return parse_sysimg(hdl, sysimg_init_cb);
}

std::pair<std::string,std::vector<std::string>> jl_get_llvm_target(bool imaging, uint32_t &flags)
{
    ensure_jit_target(imaging);
    flags = jit_targets[0].en.flags;
    return get_llvm_target_vec(jit_targets[0]);
}

const std::pair<std::string,std::string> &jl_get_llvm_disasm_target(void)
{
    static const auto res = get_llvm_target_str(TargetData<1>{host_cpu_name(),
                jl_get_cpu_features_llvm(), {{}, 0}, {{}, 0}, 0});
    return res;
}

std::vector<jl_target_spec_t> jl_get_llvm_clone_targets(void)
{
    if (jit_targets.empty())
        jl_error("JIT targets not initialized");
    std::vector<jl_target_spec_t> res;
    for (auto &target: jit_targets) {
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

JL_DLLEXPORT jl_value_t *jl_get_cpu_name(void)
{
    return jl_cstr_to_string(host_cpu_name().c_str());
}

JL_DLLEXPORT void jl_dump_host_cpu(void)
{
    jl_safe_printf("CPU: %s\n", host_cpu_name().c_str());
    jl_safe_printf("Features: %s\n", jl_get_cpu_features_llvm().c_str());
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
