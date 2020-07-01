// This file is a part of Julia. License is MIT: https://julialang.org/license

// X86 specific processor detection and dispatch

// CPUID

extern "C" JL_DLLEXPORT void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType)
{
#if defined _MSC_VER
    __cpuid(CPUInfo, InfoType);
#else
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
#endif
}

extern "C" JL_DLLEXPORT void jl_cpuidex(int32_t CPUInfo[4], int32_t InfoType, int32_t subInfoType)
{
#if defined _MSC_VER
    __cpuidex(CPUInfo, InfoType, subInfoType);
#else
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
#endif
}

namespace X86 {

enum class CPU : uint32_t {
    generic = 0,
    intel_nocona,
    intel_prescott,
    intel_atom_bonnell,
    intel_atom_silvermont,
    intel_atom_goldmont,
    intel_core2,
    intel_core2_penryn,
    intel_yonah,
    intel_corei7_nehalem,
    intel_corei7_westmere,
    intel_corei7_sandybridge,
    intel_corei7_ivybridge,
    intel_corei7_haswell,
    intel_corei7_broadwell,
    intel_corei7_skylake,
    intel_corei7_skylake_avx512,
    intel_corei7_cannonlake,
    intel_knights_landing,

    amd_fam10h,
    amd_athlon_fx,
    amd_athlon_64,
    amd_athlon_64_sse3,
    amd_bdver1,
    amd_bdver2,
    amd_bdver3,
    amd_bdver4,
    amd_btver1,
    amd_btver2,
    amd_k8,
    amd_k8_sse3,
    amd_opteron,
    amd_opteron_sse3,
    amd_barcelona,
    amd_znver1,
};

static constexpr size_t feature_sz = 9;
static constexpr FeatureName feature_names[] = {
#define JL_FEATURE_DEF(name, bit, llvmver) {#name, bit, llvmver},
#define JL_FEATURE_DEF_NAME(name, bit, llvmver, str) {str, bit, llvmver},
#include "features_x86.h"
#undef JL_FEATURE_DEF
#undef JL_FEATURE_DEF_NAME
};
static constexpr uint32_t nfeature_names = sizeof(feature_names) / sizeof(FeatureName);

template<typename... Args>
static inline constexpr FeatureList<feature_sz> get_feature_masks(Args... args)
{
    return ::get_feature_masks<feature_sz>(args...);
}

#define JL_FEATURE_DEF_NAME(name, bit, llvmver, str) JL_FEATURE_DEF(name, bit, llvmver)
static constexpr auto feature_masks = get_feature_masks(
#define JL_FEATURE_DEF(name, bit, llvmver) bit,
#include "features_x86.h"
#undef JL_FEATURE_DEF
    -1);

namespace Feature {
enum : uint32_t {
#define JL_FEATURE_DEF(name, bit, llvmver) name = bit,
#include "features_x86.h"
#undef JL_FEATURE_DEF
};
#undef JL_FEATURE_DEF_NAME
static constexpr FeatureDep deps[] = {
    {ssse3, sse3},
    {fma, avx},
    {sse41, ssse3},
    {sse42, sse41},
    {avx, sse42},
    {f16c, avx},
    {avx2, avx},
    {avx512f, avx2},
    {avx512dq, avx512f},
    {avx512ifma, avx512f},
    {avx512pf, avx512f},
    {avx512er, avx512f},
    {avx512cd, avx512f},
    {avx512bw, avx512f},
    {avx512vl, avx512f},
    {avx512vbmi, avx512bw},
    {avx512vpopcntdq, avx512f},
    {sse4a, sse3},
    {xop, fma4},
    {fma4, avx},
    {fma4, sse4a}
};

// We require cx16 on 64bit by default. This can be overwritten with `-cx16`
// This isn't really compatible with 32bit but we mask it off there with required LLVM version
constexpr auto generic = get_feature_masks(cx16);
constexpr auto bonnell = get_feature_masks(sse3, ssse3, cx16, movbe, sahf);
constexpr auto silvermont = bonnell | get_feature_masks(sse41, sse42, popcnt,
                                                        pclmul, aes, prfchw);
constexpr auto goldmont = silvermont | get_feature_masks(mpx, sha, rdrnd, rdseed, xsave,
                                                         xsaveopt, xsavec, xsaves, clflushopt);
constexpr auto yonah = get_feature_masks(sse3);
constexpr auto prescott = yonah;
constexpr auto core2 = get_feature_masks(sse3, ssse3, cx16, sahf);
constexpr auto nocona = get_feature_masks(sse3, cx16);
constexpr auto penryn = nocona | get_feature_masks(ssse3, sse41, sahf);
constexpr auto nehalem = penryn | get_feature_masks(sse42, popcnt);
constexpr auto westmere = nehalem | get_feature_masks(aes, pclmul);
constexpr auto sandybridge = westmere | get_feature_masks(avx, xsave, xsaveopt);
constexpr auto ivybridge = sandybridge | get_feature_masks(rdrnd, f16c, fsgsbase);
constexpr auto haswell = ivybridge | get_feature_masks(avx2, bmi, bmi2, fma, lzcnt, movbe);
constexpr auto broadwell = haswell | get_feature_masks(adx, rdseed, prfchw);
constexpr auto skylake = broadwell | get_feature_masks(mpx, rtm, xsavec, xsaves,
                                                       clflushopt); // ignore sgx; hle
constexpr auto knl = broadwell | get_feature_masks(avx512f, avx512er, avx512cd, avx512pf,
                                                   prefetchwt1);
constexpr auto skx = skylake | get_feature_masks(avx512f, avx512cd, avx512dq, avx512bw, avx512vl,
                                                 pku, clwb);
constexpr auto cannonlake = skx | get_feature_masks(avx512vbmi, avx512ifma, sha);

constexpr auto k8_sse3 = get_feature_masks(sse3, cx16);
constexpr auto amdfam10 = k8_sse3 | get_feature_masks(sse4a, lzcnt, popcnt, sahf);

constexpr auto btver1 = amdfam10 | get_feature_masks(ssse3, prfchw);
constexpr auto btver2 = btver1 | get_feature_masks(sse41, sse42, avx, aes, pclmul, bmi, f16c,
                                                   movbe, xsave, xsaveopt);

constexpr auto bdver1 = amdfam10 | get_feature_masks(xop, fma4, avx, ssse3, sse41, sse42, aes,
                                                     prfchw, pclmul, xsave, lwp);
constexpr auto bdver2 = bdver1 | get_feature_masks(f16c, bmi, tbm, fma);
constexpr auto bdver3 = bdver2 | get_feature_masks(xsaveopt, fsgsbase);
constexpr auto bdver4 = bdver3 | get_feature_masks(avx2, bmi2, mwaitx);

constexpr auto znver1 = haswell | get_feature_masks(adx, clflushopt, clzero, mwaitx, prfchw,
                                                    rdseed, sha, sse4a, xsavec, xsaves);

}

static constexpr CPUSpec<CPU, feature_sz> cpus[] = {
    {"generic", CPU::generic, CPU::generic, 0, Feature::generic},
    {"bonnell", CPU::intel_atom_bonnell, CPU::generic, 0, Feature::bonnell},
    {"silvermont", CPU::intel_atom_silvermont, CPU::generic, 0, Feature::silvermont},
    {"goldmont", CPU::intel_atom_goldmont, CPU::generic, 50000, Feature::goldmont},
    {"core2", CPU::intel_core2, CPU::generic, 0, Feature::core2},
    {"yonah", CPU::intel_yonah, CPU::generic, 0, Feature::yonah},
    {"prescott", CPU::intel_prescott, CPU::generic, 0, Feature::prescott},
    {"nocona", CPU::intel_nocona, CPU::generic, 0, Feature::nocona},
    {"penryn", CPU::intel_core2_penryn, CPU::generic, 0, Feature::penryn},
    {"nehalem", CPU::intel_corei7_nehalem, CPU::generic, 0, Feature::nehalem},
    {"westmere", CPU::intel_corei7_westmere, CPU::generic, 0, Feature::westmere},
    {"sandybridge", CPU::intel_corei7_sandybridge, CPU::generic, 0, Feature::sandybridge},
    {"ivybridge", CPU::intel_corei7_ivybridge, CPU::generic, 0, Feature::ivybridge},
    {"haswell", CPU::intel_corei7_haswell, CPU::generic, 0, Feature::haswell},
    {"broadwell", CPU::intel_corei7_broadwell, CPU::generic, 0, Feature::broadwell},
    {"skylake", CPU::intel_corei7_skylake, CPU::generic, 0, Feature::skylake},
    {"knl", CPU::intel_knights_landing, CPU::generic, 0, Feature::knl},
    {"skylake-avx512", CPU::intel_corei7_skylake_avx512, CPU::generic, 0, Feature::skx},
    {"cannonlake", CPU::intel_corei7_cannonlake, CPU::intel_corei7_skylake_avx512, 40000,
     Feature::cannonlake},

    {"athlon64", CPU::amd_athlon_64, CPU::generic, 0, Feature::generic},
    {"athlon-fx", CPU::amd_athlon_fx, CPU::generic, 0, Feature::generic},
    {"k8", CPU::amd_k8, CPU::generic, 0, Feature::generic},
    {"opteron", CPU::amd_opteron, CPU::generic, 0, Feature::generic},

    {"athlon64-sse3", CPU::amd_athlon_64_sse3, CPU::generic, 0, Feature::k8_sse3},
    {"k8-sse3", CPU::amd_k8_sse3, CPU::generic, 0, Feature::k8_sse3},
    {"opteron-sse3", CPU::amd_opteron_sse3, CPU::generic, 0, Feature::k8_sse3},

    {"amdfam10", CPU::amd_fam10h, CPU::generic, 0, Feature::amdfam10},
    {"barcelona", CPU::amd_barcelona, CPU::generic, 0, Feature::amdfam10},

    {"btver1", CPU::amd_btver1, CPU::generic, 0, Feature::btver1},
    {"btver2", CPU::amd_btver2, CPU::generic, 0, Feature::btver2},

    {"bdver1", CPU::amd_bdver1, CPU::generic, 0, Feature::bdver1},
    {"bdver2", CPU::amd_bdver2, CPU::generic, 0, Feature::bdver2},
    {"bdver3", CPU::amd_bdver3, CPU::generic, 0, Feature::bdver3},
    {"bdver4", CPU::amd_bdver4, CPU::generic, 0, Feature::bdver4},

    {"znver1", CPU::amd_znver1, CPU::generic, 0, Feature::znver1},
};
static constexpr size_t ncpu_names = sizeof(cpus) / sizeof(cpus[0]);

// For CPU model and feature detection on X86

const int SIG_INTEL = 0x756e6547; // Genu
const int SIG_AMD = 0x68747541; // Auth

static uint64_t get_xcr0(void)
{
#if defined _MSC_VER
    return _xgetbv(_XCR_XFEATURE_ENABLED_MASK);
#else
    uint32_t eax, edx;
    asm volatile ("xgetbv" : "=a" (eax), "=d" (edx) : "c" (0));
    return (uint64_t(edx) << 32) | eax;
#endif
}

static CPU get_intel_processor_name(uint32_t family, uint32_t model, uint32_t brand_id,
                                    const uint32_t *features)
{
    if (brand_id != 0)
        return CPU::generic;
    switch (family) {
    case 3:
    case 4:
    case 5:
        return CPU::generic;
    case 6:
        switch (model) {
        case 0x01: // Pentium Pro processor
        case 0x03: // Intel Pentium II OverDrive processor, Pentium II processor, model 03
        case 0x05: // Pentium II processor, model 05, Pentium II Xeon processor,
            // model 05, and Intel Celeron processor, model 05
        case 0x06: // Celeron processor, model 06
        case 0x07: // Pentium III processor, model 07, and Pentium III Xeon processor, model 07
        case 0x08: // Pentium III processor, model 08, Pentium III Xeon processor,
            // model 08, and Celeron processor, model 08
        case 0x0a: // Pentium III Xeon processor, model 0Ah
        case 0x0b: // Pentium III processor, model 0Bh
        case 0x09: // Intel Pentium M processor, Intel Celeron M processor model 09.
        case 0x0d: // Intel Pentium M processor, Intel Celeron M processor, model
            // 0Dh. All processors are manufactured using the 90 nm process.
        case 0x15: // Intel EP80579 Integrated Processor and Intel EP80579
            // Integrated Processor with Intel QuickAssist Technology
            return CPU::generic;
        case 0x0e: // Intel Core Duo processor, Intel Core Solo processor, model
            // 0Eh. All processors are manufactured using the 65 nm process.
            return CPU::intel_yonah;
        case 0x0f: // Intel Core 2 Duo processor, Intel Core 2 Duo mobile
            // processor, Intel Core 2 Quad processor, Intel Core 2 Quad
            // mobile processor, Intel Core 2 Extreme processor, Intel
            // Pentium Dual-Core processor, Intel Xeon processor, model
            // 0Fh. All processors are manufactured using the 65 nm process.
        case 0x16: // Intel Celeron processor model 16h. All processors are
            // manufactured using the 65 nm process
            return CPU::intel_core2;
        case 0x17: // Intel Core 2 Extreme processor, Intel Xeon processor, model
            // 17h. All processors are manufactured using the 45 nm process.
            //
            // 45nm: Penryn , Wolfdale, Yorkfield (XE)
        case 0x1d: // Intel Xeon processor MP. All processors are manufactured using
            // the 45 nm process.
            return CPU::intel_core2_penryn;
        case 0x1a: // Intel Core i7 processor and Intel Xeon processor. All
            // processors are manufactured using the 45 nm process.
        case 0x1e: // Intel(R) Core(TM) i7 CPU         870  @ 2.93GHz.
            // As found in a Summer 2010 model iMac.
        case 0x1f:
        case 0x2e: // Nehalem EX
            return CPU::intel_corei7_nehalem;
        case 0x25: // Intel Core i7, laptop version.
        case 0x2c: // Intel Core i7 processor and Intel Xeon processor. All
            // processors are manufactured using the 32 nm process.
        case 0x2f: // Westmere EX
            return CPU::intel_corei7_westmere;
        case 0x2a: // Intel Core i7 processor. All processors are manufactured
            // using the 32 nm process.
        case 0x2d:
            return CPU::intel_corei7_sandybridge;
        case 0x3a:
        case 0x3e: // Ivy Bridge EP
            return CPU::intel_corei7_ivybridge;

            // Haswell:
        case 0x3c:
        case 0x3f:
        case 0x45:
        case 0x46:
            return CPU::intel_corei7_haswell;

            // Broadwell:
        case 0x3d:
        case 0x47:
        case 0x4f:
        case 0x56:
            return CPU::intel_corei7_broadwell;

            // Skylake:
        case 0x4e: // Skylake mobile
        case 0x5e: // Skylake desktop
        case 0x8e: // Kaby Lake mobile
        case 0x9e: // Kaby Lake desktop
            return CPU::intel_corei7_skylake;

            // Skylake Xeon:
        case 0x55:
            return CPU::intel_corei7_skylake;

        case 0x1c: // Most 45 nm Intel Atom processors
        case 0x26: // 45 nm Atom Lincroft
        case 0x27: // 32 nm Atom Medfield
        case 0x35: // 32 nm Atom Midview
        case 0x36: // 32 nm Atom Midview
            return CPU::intel_atom_bonnell;

            // Atom Silvermont codes from the Intel software optimization guide.
        case 0x37:
        case 0x4a:
        case 0x4d:
        case 0x5a:
        case 0x5d:
        case 0x4c: // really airmont
            return CPU::intel_atom_silvermont;

            // Goldmont:
        case 0x5c:
        case 0x5f:
            return CPU::intel_atom_goldmont;

        case 0x57:
            return CPU::intel_knights_landing;

        default:
            return CPU::generic;
        }
        break;
    case 15: {
        switch (model) {
        case 0: // Pentium 4 processor, Intel Xeon processor. All processors are
            // model 00h and manufactured using the 0.18 micron process.
        case 1: // Pentium 4 processor, Intel Xeon processor, Intel Xeon
            // processor MP, and Intel Celeron processor. All processors are
            // model 01h and manufactured using the 0.18 micron process.
        case 2: // Pentium 4 processor, Mobile Intel Pentium 4 processor - M,
            // Intel Xeon processor, Intel Xeon processor MP, Intel Celeron
            // processor, and Mobile Intel Celeron processor. All processors
            // are model 02h and manufactured using the 0.13 micron process.
        default:
            return CPU::generic;

        case 3: // Pentium 4 processor, Intel Xeon processor, Intel Celeron D
            // processor. All processors are model 03h and manufactured using
            // the 90 nm process.
        case 4: // Pentium 4 processor, Pentium 4 processor Extreme Edition,
            // Pentium D processor, Intel Xeon processor, Intel Xeon
            // processor MP, Intel Celeron D processor. All processors are
            // model 04h and manufactured using the 90 nm process.
        case 6: // Pentium 4 processor, Pentium D processor, Pentium processor
            // Extreme Edition, Intel Xeon processor, Intel Xeon processor
            // MP, Intel Celeron D processor. All processors are model 06h
            // and manufactured using the 65 nm process.
#ifdef _CPU_X86_64_
            return CPU::intel_nocona;
#else
            return CPU::intel_prescott;
#endif
        }
    }
    default:
        break; /*"generic"*/
    }
    return CPU::generic;
}

static CPU get_amd_processor_name(uint32_t family, uint32_t model, const uint32_t *features)
{
    switch (family) {
    case 4:
    case 5:
    case 6:
    default:
        return CPU::generic;
    case 15:
        if (test_nbit(features, Feature::sse3))
            return CPU::amd_k8_sse3;
        switch (model) {
        case 1:
            return CPU::amd_opteron;
        case 5:
            return CPU::amd_athlon_fx;
        default:
            return CPU::amd_athlon_64;
        }
    case 16:
        switch (model) {
        case 2:
            return CPU::amd_barcelona;
        case 4:
        case 8:
        default:
            return CPU::amd_fam10h;
        }
    case 20:
        return CPU::amd_btver1;
    case 21:
        if (!test_nbit(features, Feature::avx))
            return CPU::amd_btver1;
        if (model >= 0x50 && model <= 0x6f)
            return CPU::amd_bdver4;
        if (model >= 0x30 && model <= 0x3f)
            return CPU::amd_bdver3;
        if (model >= 0x10 && model <= 0x1f)
            return CPU::amd_bdver2;
        if (model <= 0x0f)
            return CPU::amd_bdver1;
        return CPU::amd_btver1; // fallback
    case 22:
        if (!test_nbit(features, Feature::avx))
            return CPU::amd_btver1;
        return CPU::amd_btver2;
    case 23:
        if (test_nbit(features, Feature::adx))
            return CPU::amd_znver1;
        return CPU::amd_btver1;
    }
}

template<typename T>
static inline void features_disable_avx512(T &features)
{
    using namespace Feature;
    unset_bits(features, avx512f, avx512dq, avx512ifma, avx512pf, avx512er, avx512cd,
               avx512bw, avx512vl, avx512vbmi);
}

template<typename T>
static inline void features_disable_avx(T &features)
{
    using namespace Feature;
    unset_bits(features, avx, Feature::fma, f16c, xsave, avx2, xop, fma4,
               xsaveopt, xsavec, xsaves);
}

static NOINLINE std::pair<uint32_t,FeatureList<feature_sz>> _get_host_cpu(void)
{
    FeatureList<feature_sz> features = {};

    int32_t info0[4];
    jl_cpuid(info0, 0);
    uint32_t maxleaf = info0[0];
    if (maxleaf < 1)
        return std::make_pair(uint32_t(CPU::generic), features);
    int32_t info1[4];
    jl_cpuid(info1, 1);

    auto vendor = info0[1];
    auto brand_id = info1[1] & 0xff;

    auto family = (info1[0] >> 8) & 0xf; // Bits 8 - 11
    auto model = (info1[0] >> 4) & 0xf;  // Bits 4 - 7
    if (family == 6 || family == 0xf) {
        if (family == 0xf)
            // Examine extended family ID if family ID is F.
            family += (info1[0] >> 20) & 0xff; // Bits 20 - 27
        // Examine extended model ID if family ID is 6 or F.
        model += ((info1[0] >> 16) & 0xf) << 4; // Bits 16 - 19
    }

    // Fill in the features
    features[0] = info1[2];
    features[1] = info1[3];
    if (maxleaf >= 7) {
        int32_t info7[4];
        jl_cpuidex(info7, 7, 0);
        features[2] = info7[1];
        features[3] = info7[2];
        features[4] = info7[3];
    }
    int32_t infoex0[4];
    jl_cpuid(infoex0, 0x80000000);
    uint32_t maxexleaf = infoex0[0];
    if (maxexleaf >= 0x80000001) {
        int32_t infoex1[4];
        jl_cpuid(infoex1, 0x80000001);
        features[5] = infoex1[2];
        features[6] = infoex1[3];
    }
    if (maxleaf >= 0xd) {
        int32_t infod[4];
        jl_cpuidex(infod, 0xd, 0x1);
        features[7] = infod[0];
    }
    if (maxexleaf >= 0x80000008) {
        int32_t infoex8[4];
        jl_cpuidex(infoex8, 0x80000008, 0);
        features[8] = infoex8[1];
    }

    // Fix up AVX bits to account for OS support and match LLVM model
    uint64_t xcr0 = 0;
    const uint32_t avx_mask = (1 << 27) | (1 << 28);
    bool hasavx = test_all_bits(features[0], avx_mask);
    if (hasavx) {
        xcr0 = get_xcr0();
        hasavx = test_all_bits(xcr0, 0x6);
    }
    unset_bits(features, 32 + 27);
    if (!hasavx)
        features_disable_avx(features);
    bool hasavx512save = hasavx && test_all_bits(xcr0, 0xe0);
    if (!hasavx512save)
        features_disable_avx512(features);
    // Ignore feature bits that we are not interested in.
    mask_features(feature_masks, &features[0]);

    uint32_t cpu;
    if (vendor == SIG_INTEL) {
        cpu = uint32_t(get_intel_processor_name(family, model, brand_id, &features[0]));
    }
    else if (vendor == SIG_AMD) {
        cpu = uint32_t(get_amd_processor_name(family, model, &features[0]));
    }
    else {
        cpu = uint32_t(CPU::generic);
    }

    return std::make_pair(cpu, features);
}

static inline const std::pair<uint32_t,FeatureList<feature_sz>> &get_host_cpu()
{
    static auto host_cpu = _get_host_cpu();
    return host_cpu;
}

static inline const CPUSpec<CPU,feature_sz> *find_cpu(uint32_t cpu)
{
    return ::find_cpu(cpu, cpus, ncpu_names);
}

static inline const CPUSpec<CPU,feature_sz> *find_cpu(llvm::StringRef name)
{
    return ::find_cpu(name, cpus, ncpu_names);
}

static inline const char *find_cpu_name(uint32_t cpu)
{
    return ::find_cpu_name(cpu, cpus, ncpu_names);
}

static inline const std::string &host_cpu_name()
{
    static std::string name =
        (CPU)get_host_cpu().first != CPU::generic ?
        std::string(find_cpu_name(get_host_cpu().first)) :
        jl_get_cpu_name_llvm();
    return name;
}

static inline const char *normalize_cpu_name(llvm::StringRef name)
{
    if (name == "atom")
        return "bonnell";
    if (name == "slm")
        return "silvermont";
    if (name == "glm")
        return "goldmont";
    if (name == "corei7")
        return "nehalem";
    if (name == "corei7-avx")
        return "sandybridge";
    if (name == "core-avx-i")
        return "ivybridge";
    if (name == "core-avx2")
        return "haswell";
    if (name == "skx")
        return "skylake-avx512";
#ifdef _CPU_X86_
    // i686 isn't a supported target but it's a common default one so just make it mean pentium4.
    if (name == "pentium4" || name == "i686")
        return "generic";
#else
    if (name == "x86-64" || name == "x86_64")
        return "generic";
#endif
    return nullptr;
}

template<size_t n>
static inline void enable_depends(FeatureList<n> &features)
{
    ::enable_depends(features, Feature::deps, sizeof(Feature::deps) / sizeof(FeatureDep));
}

template<size_t n>
static inline void disable_depends(FeatureList<n> &features)
{
    ::disable_depends(features, Feature::deps, sizeof(Feature::deps) / sizeof(FeatureDep));
}

static const std::vector<TargetData<feature_sz>> &get_cmdline_targets(void)
{
    auto feature_cb = [] (const char *str, size_t len, FeatureList<feature_sz> &list) {
        auto fbit = find_feature_bit(feature_names, nfeature_names, str, len);
        if (fbit == (uint32_t)-1)
            return false;
        set_bit(list, fbit, true);
        return true;
    };
    auto &targets = ::get_cmdline_targets<feature_sz>(feature_cb);
    for (auto &t: targets) {
        if (auto nname = normalize_cpu_name(t.name)) {
            t.name = nname;
        }
    }
    return targets;
}

static std::vector<TargetData<feature_sz>> jit_targets;

static TargetData<feature_sz> arg_target_data(const TargetData<feature_sz> &arg, bool require_host)
{
    TargetData<feature_sz> res = arg;
    const FeatureList<feature_sz> *cpu_features = nullptr;
    if (res.name == "native") {
        res.name = host_cpu_name();
        cpu_features = &get_host_cpu().second;
    }
    else if (auto spec = find_cpu(res.name)) {
        cpu_features = &spec->features;
    }
    else {
        res.en.flags |= JL_TARGET_UNKNOWN_NAME;
    }
    if (cpu_features) {
        for (size_t i = 0; i < feature_sz; i++) {
            res.en.features[i] |= (*cpu_features)[i];
        }
    }
    enable_depends(res.en.features);
    // Mask our rdrand/rdseed/rtm/xsaveopt features that LLVM doesn't use and rr disables
    unset_bits(res.en.features, Feature::rdrnd, Feature::rdseed, Feature::rtm, Feature::xsaveopt);
    for (size_t i = 0; i < feature_sz; i++)
        res.en.features[i] &= ~res.dis.features[i];
    if (require_host) {
        for (size_t i = 0; i < feature_sz; i++) {
            res.en.features[i] &= get_host_cpu().second[i];
        }
    }
    disable_depends(res.en.features);
    if (cpu_features) {
        // If the base feature if known, fill in the disable features
        for (size_t i = 0; i < feature_sz; i++) {
            res.dis.features[i] = feature_masks[i] & ~res.en.features[i];
        }
    }
    return res;
}

static int max_vector_size(const FeatureList<feature_sz> &features)
{
    if (test_nbit(features, Feature::avx512f))
        return 64;
    if (test_nbit(features, Feature::avx))
        return 32;
    // SSE is required
    return 16;
}

static uint32_t sysimg_init_cb(const void *id)
{
    // First see what target is requested for the JIT.
    auto &cmdline = get_cmdline_targets();
    TargetData<feature_sz> target = arg_target_data(cmdline[0], true);
    // Then find the best match in the sysimg
    auto sysimg = deserialize_target_data<feature_sz>((const uint8_t*)id);
    // We translate `generic` to `pentium4` or `x86-64` before sending it to LLVM
    // (see `get_llvm_target_noext`) which will be serialized into the sysimg target data.
    // Translate them back so we can actually match them.
    // We also track to see if the sysimg allows -cx16, however if the user does
    // something silly like add +cx16 on a 32bit target, we want to disable this
    // check, hence the pointer size check.
    bool sysimg_allows_no_cx16 = sizeof(void *) == 4;;
    for (auto &t: sysimg) {
        if (auto nname = normalize_cpu_name(t.name)) {
            t.name = nname;
        }

        // Take note to see if the sysimg explicitly allows an architecture without cx16
        sysimg_allows_no_cx16 |= !test_nbit(t.en.features, Feature::cx16);
    }
    if (!sysimg_allows_no_cx16 && !test_nbit(target.en.features, Feature::cx16)) {
        jl_error("Your CPU does not support the CX16 instruction, which is required "
                 "by this version of Julia!  This is often due to running inside of a "
                 "virtualized environment.  Please read "
                 "https://docs.julialang.org/en/v1/devdocs/sysimg/ for more.");
    }
    auto match = match_sysimg_targets(sysimg, target, max_vector_size);
    // Now we've decided on which sysimg version to use.
    // Make sure the JIT target is compatible with it and save the JIT target.
    if (match.vreg_size != max_vector_size(target.en.features) &&
        (sysimg[match.best_idx].en.flags & JL_TARGET_VEC_CALL)) {
        if (match.vreg_size < 64) {
            features_disable_avx512(target.en.features);
        }
        if (match.vreg_size < 32) {
            features_disable_avx(target.en.features);
        }
    }
    jit_targets.push_back(std::move(target));
    return match.best_idx;
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
        if (t.en.flags & JL_TARGET_CLONE_ALL)
            continue;
        // The most useful one in general...
        t.en.flags |= JL_TARGET_CLONE_LOOP;
        auto &features0 = jit_targets[t.base].en.features;
        // Special case for KNL since it's so different
        if (!(t.dis.flags & JL_TARGET_CLONE_ALL)) {
            if (t.name == "knl" && jit_targets[t.base].name != "knl") {
                t.en.flags |= JL_TARGET_CLONE_ALL;
                break;
            }
        }
        static constexpr uint32_t clone_math[] = {Feature::fma, Feature::fma4};
        static constexpr uint32_t clone_simd[] = {Feature::sse3, Feature::ssse3,
                                                  Feature::sse41, Feature::sse42,
                                                  Feature::avx, Feature::avx2,
                                                  Feature::sse4a, Feature::avx512f,
                                                  Feature::avx512dq, Feature::avx512ifma,
                                                  Feature::avx512pf, Feature::avx512er,
                                                  Feature::avx512cd, Feature::avx512bw,
                                                  Feature::avx512vl, Feature::avx512vbmi,
                                                  Feature::avx512vpopcntdq};
        for (auto fe: clone_math) {
            if (!test_nbit(features0, fe) && test_nbit(t.en.features, fe)) {
                t.en.flags |= JL_TARGET_CLONE_MATH;
                break;
            }
        }
        for (auto fe: clone_simd) {
            if (!test_nbit(features0, fe) && test_nbit(t.en.features, fe)) {
                t.en.flags |= JL_TARGET_CLONE_SIMD;
                break;
            }
        }
    }
}

static std::pair<std::string,std::vector<std::string>>
get_llvm_target_noext(const TargetData<feature_sz> &data)
{
    std::string name = data.name;
    auto *spec = find_cpu(name);
    while (spec) {
        if (spec->llvmver <= JL_LLVM_VERSION)
            break;
        spec = find_cpu((uint32_t)spec->fallback);
        name = spec->name;
    }
    if (name == "generic") {
        // Use translate `generic` into what we actually require
#ifdef _CPU_X86_
        name = "pentium4";
#else
        name = "x86-64";
#endif
    }
    std::vector<std::string> features;
    for (auto &fename: feature_names) {
        if (fename.llvmver > JL_LLVM_VERSION)
            continue;
        if (test_nbit(data.en.features, fename.bit)) {
            features.insert(features.begin(), std::string("+") + fename.name);
        }
        else if (test_nbit(data.dis.features, fename.bit)) {
            features.push_back(std::string("-") + fename.name);
        }
    }
    features.push_back("+sse2");
    features.push_back("+mmx");
    features.push_back("+fxsr");
#ifdef _CPU_X86_64_
    // This is required to make LLVM happy if LLVM's feature based CPU arch guess
    // returns a value that may not have 64bit support.
    // This can happen with virtualization.
    features.push_back("+64bit");
#endif
    return std::make_pair(std::move(name), std::move(features));
}

static std::pair<std::string,std::vector<std::string>>
get_llvm_target_vec(const TargetData<feature_sz> &data)
{
    auto res0 = get_llvm_target_noext(data);
    append_ext_features(res0.second, data.ext_features);
    return res0;
}

static std::pair<std::string,std::string>
get_llvm_target_str(const TargetData<feature_sz> &data)
{
    auto res0 = get_llvm_target_noext(data);
    auto features = join_feature_strs(res0.second);
    append_ext_features(features, data.ext_features);
    return std::make_pair(std::move(res0.first), std::move(features));
}

}

using namespace X86;

JL_DLLEXPORT void jl_dump_host_cpu(void)
{
    dump_cpu_spec(get_host_cpu().first, get_host_cpu().second, feature_names, nfeature_names,
                  cpus, ncpu_names);
}

JL_DLLEXPORT jl_value_t *jl_get_cpu_name(void)
{
    return jl_cstr_to_string(host_cpu_name().c_str());
}

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
    static const auto res = get_llvm_target_str(TargetData<feature_sz>{"generic", "",
            {feature_masks, 0}, {{}, 0}, 0});
    return res;
}

std::vector<jl_target_spec_t> jl_get_llvm_clone_targets(void)
{
    if (jit_targets.empty())
        jl_error("JIT targets not initialized");
    std::vector<jl_target_spec_t> res;
    for (auto &target: jit_targets) {
        auto features_en = target.en.features;
        auto features_dis = target.dis.features;
        for (auto &fename: feature_names) {
            if (fename.llvmver > JL_LLVM_VERSION) {
                unset_bits(features_en, fename.bit);
                unset_bits(features_dis, fename.bit);
            }
        }
        X86::disable_depends(features_en);
        jl_target_spec_t ele;
        std::tie(ele.cpu_name, ele.cpu_features) = get_llvm_target_str(target);
        ele.data = serialize_target_data(target.name, features_en, features_dis,
                                         target.ext_features);
        ele.flags = target.en.flags;
        ele.base = target.base;
        res.push_back(ele);
    }
    return res;
}

extern "C" int jl_test_cpu_feature(jl_cpu_feature_t feature)
{
    if (feature >= 32 * feature_sz)
        return 0;
    return test_nbit(&get_host_cpu().second[0], feature);
}

// -- set/clear the FZ/DAZ flags on x86 & x86-64 --

// Cache of information recovered from `cpuid` since executing `cpuid` it at runtime is slow.
static uint32_t subnormal_flags = [] {
    int32_t info[4];
    jl_cpuid(info, 0);
    if (info[0] >= 1) {
        jl_cpuid(info, 1);
        if (info[3] & (1 << 26)) {
            // SSE2 supports both FZ and DAZ
            return 0x00008040;
        }
        else if (info[3] & (1 << 25)) {
            // SSE supports only the FZ flag
            return 0x00008000;
        }
    }
    return 0;
}();

// Returns non-zero if subnormals go to 0; zero otherwise.
extern "C" JL_DLLEXPORT int32_t jl_get_zero_subnormals(void)
{
    return _mm_getcsr() & subnormal_flags;
}

// Return zero on success, non-zero on failure.
extern "C" JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    uint32_t flags = subnormal_flags;
    if (flags) {
        uint32_t state = _mm_getcsr();
        if (isZero)
            state |= flags;
        else
            state &= ~flags;
        _mm_setcsr(state);
        return 0;
    }
    else {
        // Report a failure only if user is trying to enable FTZ/DAZ.
        return isZero;
    }
}
