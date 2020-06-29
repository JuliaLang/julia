// This file is a part of Julia. License is MIT: https://julialang.org/license

// ARM (AArch32/AArch64) specific processor detection and dispatch

#include <sys/stat.h>
#include <sys/utsname.h>
#include <fcntl.h>
#include <set>
#include <fstream>
#include <algorithm>

// This nesting is required to allow compilation on musl
#define USE_DYN_GETAUXVAL
#if defined(_CPU_AARCH64_)
#  undef USE_DYN_GETAUXVAL
#  include <sys/auxv.h>
#elif defined(__GLIBC_PREREQ)
#  if __GLIBC_PREREQ(2, 16)
#    undef USE_DYN_GETAUXVAL
#    include <sys/auxv.h>
#  endif
#endif

namespace ARM {
enum class CPU : uint32_t {
    generic = 0,

    // Architecture targets
    armv7_a,
    armv7_m,
    armv7e_m,
    armv7_r,
    armv8_a,
    armv8_m_base,
    armv8_m_main,
    armv8_r,
    armv8_1_a,
    armv8_2_a,
    armv8_3_a,
    // armv8_4_a,

    // ARM
    // armv6l
    arm_mpcore,
    arm_1136jf_s,
    arm_1156t2f_s,
    arm_1176jzf_s,
    arm_cortex_m0,
    arm_cortex_m1,
    // armv7ml
    arm_cortex_m3,
    arm_cortex_m4,
    arm_cortex_m7,
    // armv7l
    arm_cortex_a5,
    arm_cortex_a7,
    arm_cortex_a8,
    arm_cortex_a9,
    arm_cortex_a12,
    arm_cortex_a15,
    arm_cortex_a17,
    arm_cortex_r4,
    arm_cortex_r5,
    arm_cortex_r7,
    arm_cortex_r8,
    // armv8ml
    arm_cortex_m23,
    arm_cortex_m33,
    // armv8l
    arm_cortex_a32,
    arm_cortex_r52,
    // aarch64
    arm_cortex_a35,
    arm_cortex_a53,
    arm_cortex_a55,
    arm_cortex_a57,
    arm_cortex_a72,
    arm_cortex_a73,
    arm_cortex_a75,

    // Cavium
    // aarch64
    cavium_thunderx,
    cavium_thunderx88,
    cavium_thunderx88p1,
    cavium_thunderx81,
    cavium_thunderx83,
    cavium_thunderx2t99,
    cavium_thunderx2t99p1,

    // NVIDIA
    // aarch64
    nvidia_denver1,
    nvidia_denver2,

    // AppliedMicro
    // aarch64
    apm_xgene1,
    apm_xgene2,
    apm_xgene3,

    // Qualcomm
    // armv7l
    qualcomm_scorpion,
    qualcomm_krait,
    // aarch64
    qualcomm_kyro,
    qualcomm_falkor,
    qualcomm_saphira,

    // Samsung
    // aarch64
    samsung_exynos_m1,
    samsung_exynos_m2,
    samsung_exynos_m3,

    // Apple
    // armv7l
    apple_swift,
    // aarch64
    apple_cyclone,
    apple_typhoon,
    apple_twister,
    apple_hurricane,

    // Marvell
    // armv7l
    marvell_pj4,

    // Intel
    // armv7l
    intel_3735d,
};

#ifdef _CPU_AARCH64_
static constexpr size_t feature_sz = 3;
static constexpr FeatureName feature_names[] = {
#define JL_FEATURE_DEF(name, bit, llvmver) {#name, bit, llvmver},
#define JL_FEATURE_DEF_NAME(name, bit, llvmver, str) {str, bit, llvmver},
#include "features_aarch64.h"
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
#include "features_aarch64.h"
#undef JL_FEATURE_DEF
    -1);
static const auto real_feature_masks =
    feature_masks & FeatureList<feature_sz>{{(uint32_t)-1, (uint32_t)-1, 0}};

namespace Feature {
enum : uint32_t {
#define JL_FEATURE_DEF(name, bit, llvmver) name = bit,
#include "features_aarch64.h"
#undef JL_FEATURE_DEF
};
#undef JL_FEATURE_DEF_NAME
// This does not cover all dependencies (e.g. the ones that depends on arm versions)
static constexpr FeatureDep deps[] = {
    {0, 0} // dummy
};

constexpr auto generic = get_feature_masks();
constexpr auto armv8a_crc = get_feature_masks(crc);
constexpr auto armv8a_crc_crypto = armv8a_crc | get_feature_masks(crypto);
constexpr auto armv8_1a = armv8a_crc | get_feature_masks(v8_1a, lse, rdm); // lor, hpd
constexpr auto armv8_2a = armv8_1a | get_feature_masks(v8_2a); // ras
constexpr auto armv8_2a_crypto = armv8_2a | get_feature_masks(crypto);
constexpr auto armv8_3a = armv8_2a | get_feature_masks(v8_3a, rcpc);
constexpr auto armv8_3a_crypto = armv8_3a | get_feature_masks(crypto);

constexpr auto arm_cortex_a32 = generic; // TODO? (crc, crypto)
constexpr auto arm_cortex_a35 = generic; // TODO? (crc, crypto)
constexpr auto arm_cortex_a53 = armv8a_crc;
constexpr auto arm_cortex_a55 = armv8_2a_crypto | get_feature_masks(rcpc); // dotprod;
constexpr auto arm_cortex_a57 = armv8a_crc;
constexpr auto arm_cortex_a72 = armv8a_crc;
constexpr auto arm_cortex_a73 = armv8a_crc;
constexpr auto arm_cortex_a75 = armv8_2a_crypto | get_feature_masks(rcpc); // dotprod;
constexpr auto cavium_thunderx = armv8a_crc_crypto;
constexpr auto cavium_thunderx88 = armv8a_crc_crypto;
constexpr auto cavium_thunderx88p1 = armv8a_crc_crypto;
constexpr auto cavium_thunderx81 = armv8a_crc_crypto;
constexpr auto cavium_thunderx83 = armv8a_crc_crypto;
constexpr auto cavium_thunderx2t99 = armv8a_crc_crypto | get_feature_masks(v8_1a);
constexpr auto cavium_thunderx2t99p1 = armv8a_crc_crypto | get_feature_masks(v8_1a);
constexpr auto nvidia_denver1 = generic; // TODO? (crc, crypto)
constexpr auto nvidia_denver2 = armv8a_crc_crypto;
constexpr auto apm_xgene1 = generic;
constexpr auto apm_xgene2 = generic; // TODO?
constexpr auto apm_xgene3 = generic; // TODO?
constexpr auto qualcomm_kyro = armv8a_crc_crypto;
constexpr auto qualcomm_falkor = armv8a_crc_crypto;
constexpr auto qualcomm_saphira = armv8_3a_crypto;
constexpr auto samsung_exynos_m1 = armv8a_crc_crypto;
constexpr auto samsung_exynos_m2 = armv8a_crc_crypto;
constexpr auto samsung_exynos_m3 = armv8a_crc_crypto;
constexpr auto apple_cyclone = armv8a_crc_crypto;
constexpr auto apple_typhoon = armv8a_crc_crypto;
constexpr auto apple_twister = armv8a_crc_crypto;
constexpr auto apple_hurricane = armv8a_crc_crypto;

}

static constexpr CPUSpec<CPU, feature_sz> cpus[] = {
    {"generic", CPU::generic, CPU::generic, 0, Feature::generic},
    {"armv8.1-a", CPU::armv8_1_a, CPU::generic, 0, Feature::armv8_1a},
    {"armv8.2-a", CPU::armv8_2_a, CPU::generic, 0, Feature::armv8_2a},
    {"armv8.3_a", CPU::armv8_3_a, CPU::generic, 0, Feature::armv8_3a},
    {"cortex-a35", CPU::arm_cortex_a35, CPU::generic, 0, Feature::arm_cortex_a35},
    {"cortex-a53", CPU::arm_cortex_a53, CPU::generic, 0, Feature::arm_cortex_a53},
    {"cortex-a55", CPU::arm_cortex_a55, CPU::arm_cortex_a53, UINT32_MAX, Feature::arm_cortex_a55},
    {"cortex-a57", CPU::arm_cortex_a57, CPU::generic, 0, Feature::arm_cortex_a57},
    {"cortex-a72", CPU::arm_cortex_a72, CPU::generic, 0, Feature::arm_cortex_a72},
    {"cortex-a73", CPU::arm_cortex_a73, CPU::generic, 0, Feature::arm_cortex_a73},
    {"cortex-a75", CPU::arm_cortex_a75, CPU::arm_cortex_a73, UINT32_MAX, Feature::arm_cortex_a75},
    {"thunderx", CPU::cavium_thunderx, CPU::generic, 50000, Feature::cavium_thunderx},
    {"thunderxt88", CPU::cavium_thunderx88, CPU::generic, 50000, Feature::cavium_thunderx88},
    {"thunderxt88p1", CPU::cavium_thunderx88p1, CPU::cavium_thunderx88, UINT32_MAX,
     Feature::cavium_thunderx88p1},
    {"thunderxt81", CPU::cavium_thunderx81, CPU::generic, 50000, Feature::cavium_thunderx81},
    {"thunderxt83", CPU::cavium_thunderx83, CPU::generic, 50000, Feature::cavium_thunderx83},
    {"thunderx2t99", CPU::cavium_thunderx2t99, CPU::generic, 50000,
     Feature::cavium_thunderx2t99},
    {"thunderx2t99p1", CPU::cavium_thunderx2t99p1, CPU::cavium_thunderx2t99, UINT32_MAX,
     Feature::cavium_thunderx2t99p1},
    {"denver1", CPU::nvidia_denver1, CPU::generic, UINT32_MAX, Feature::nvidia_denver1},
    {"denver2", CPU::nvidia_denver2, CPU::generic, UINT32_MAX, Feature::nvidia_denver2},
    {"xgene1", CPU::apm_xgene1, CPU::generic, UINT32_MAX, Feature::apm_xgene1},
    {"xgene2", CPU::apm_xgene2, CPU::generic, UINT32_MAX, Feature::apm_xgene2},
    {"xgene3", CPU::apm_xgene3, CPU::generic, UINT32_MAX, Feature::apm_xgene3},
    {"kyro", CPU::qualcomm_kyro, CPU::generic, 0, Feature::qualcomm_kyro},
    {"falkor", CPU::qualcomm_falkor, CPU::generic, 40000, Feature::qualcomm_falkor},
    {"saphira", CPU::qualcomm_saphira, CPU::qualcomm_falkor, 60000, Feature::qualcomm_saphira},
    {"exynos-m1", CPU::samsung_exynos_m1, CPU::generic, 0, Feature::samsung_exynos_m1},
    {"exynos-m2", CPU::samsung_exynos_m2, CPU::samsung_exynos_m1, 40000,
     Feature::samsung_exynos_m2},
    {"exynos-m3", CPU::samsung_exynos_m3, CPU::samsung_exynos_m2, 40000,
     Feature::samsung_exynos_m3},
    {"cyclone", CPU::apple_cyclone, CPU::generic, 0, Feature::apple_cyclone},
    {"typhoon", CPU::apple_typhoon, CPU::apple_cyclone, UINT32_MAX, Feature::apple_typhoon},
    {"twister", CPU::apple_twister, CPU::apple_typhoon, UINT32_MAX, Feature::apple_twister},
    {"hurricane", CPU::apple_hurricane, CPU::apple_twister, UINT32_MAX, Feature::apple_hurricane},
};
#else
static constexpr size_t feature_sz = 3;
static constexpr FeatureName feature_names[] = {
#define JL_FEATURE_DEF(name, bit, llvmver) {#name, bit, llvmver},
#define JL_FEATURE_DEF_NAME(name, bit, llvmver, str) {str, bit, llvmver},
#include "features_aarch32.h"
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
#include "features_aarch32.h"
#undef JL_FEATURE_DEF
    -1);
static const auto real_feature_masks =
    feature_masks & FeatureList<feature_sz>{{(uint32_t)-1, (uint32_t)-1, 0}};

namespace Feature {
enum : uint32_t {
#define JL_FEATURE_DEF(name, bit, llvmver) name = bit,
#include "features_aarch32.h"
#undef JL_FEATURE_DEF
};
#undef JL_FEATURE_DEF_NAME
// This does not cover all dependencies (e.g. the ones that depends on arm versions)
static constexpr FeatureDep deps[] = {
    {neon, vfp3},
    {vfp4, vfp3},
    {crypto, neon},
};

// These are the real base requirements of the specific architectures
constexpr auto _armv7m = get_feature_masks(v7, mclass, hwdiv);
constexpr auto _armv7a = get_feature_masks(v7, aclass);
constexpr auto _armv7r = get_feature_masks(v7, rclass);
constexpr auto _armv8m = get_feature_masks(v7, v8, mclass, hwdiv);
constexpr auto _armv8a = get_feature_masks(v7, v8, aclass, neon, vfp3, vfp4, d32,
                                           hwdiv, hwdiv_arm);
constexpr auto _armv8r = get_feature_masks(v7, v8, rclass, neon, vfp3, vfp4, d32,
                                           hwdiv, hwdiv_arm);

// Set `generic` to match the feature requirement of the `C` code.
// we'll require at least these when compiling the sysimg.
#if __ARM_ARCH >= 8
#  if !defined(__ARM_ARCH_PROFILE)
constexpr auto generic = get_feature_masks(v7, v8, hwdiv);
#  elif __ARM_ARCH_PROFILE == 'A'
constexpr auto generic = _armv8a;
#  elif __ARM_ARCH_PROFILE == 'R'
constexpr auto generic = _armv8r;
#  elif __ARM_ARCH_PROFILE == 'M'
constexpr auto generic = _armv8m;
#  else
constexpr auto generic = get_feature_masks(v7, v8, hwdiv);
#  endif
#elif __ARM_ARCH == 7
#  if !defined(__ARM_ARCH_PROFILE)
constexpr auto generic = get_feature_masks(v7);
#  elif __ARM_ARCH_PROFILE == 'A'
constexpr auto generic = _armv7a;
#  elif __ARM_ARCH_PROFILE == 'R'
constexpr auto generic = _armv7r;
#  elif __ARM_ARCH_PROFILE == 'M'
constexpr auto generic = _armv7m;
#  else
constexpr auto generic = get_feature_masks(v7);
#  endif
#else
constexpr auto generic = get_feature_masks();
#endif

// All feature sets below should use or be or'ed with one of these (or generic).
// This makes sure that, for example, the `generic` target on `armv7-a` binary is equivalent
// to the `armv7-a` target.
constexpr auto armv7m = generic | _armv7m;
constexpr auto armv7a = generic | _armv7a;
constexpr auto armv7r = generic | _armv7r;
constexpr auto armv8m = generic | _armv8m;
constexpr auto armv8a = generic | _armv8a;
constexpr auto armv8r = generic | _armv8r;

// armv7l
constexpr auto arm_cortex_a5 = armv7a;
constexpr auto arm_cortex_a7 = armv7a | get_feature_masks(vfp3, vfp4, neon);
constexpr auto arm_cortex_a8 = armv7a | get_feature_masks(d32, vfp3, neon);
constexpr auto arm_cortex_a9 = armv7a;
constexpr auto arm_cortex_a12 = armv7a | get_feature_masks(d32, vfp3, vfp4, neon);
constexpr auto arm_cortex_a15 = armv7a | get_feature_masks(d32, vfp3, vfp4, neon);
constexpr auto arm_cortex_a17 = armv7a | get_feature_masks(d32, vfp3, vfp4, neon);
constexpr auto arm_cortex_r4 = armv7r | get_feature_masks(vfp3, hwdiv);
constexpr auto arm_cortex_r5 = armv7r | get_feature_masks(vfp3, hwdiv, hwdiv_arm);
constexpr auto arm_cortex_r7 = armv7r | get_feature_masks(vfp3, hwdiv, hwdiv_arm);
constexpr auto arm_cortex_r8 = armv7r | get_feature_masks(vfp3, hwdiv, hwdiv_arm);
constexpr auto qualcomm_scorpion = armv7a | get_feature_masks(v7, aclass, vfp3, neon);
constexpr auto qualcomm_krait = armv7a | get_feature_masks(vfp3, vfp4, neon, hwdiv, hwdiv_arm);
constexpr auto apple_swift = armv7a | get_feature_masks(d32, vfp3, vfp4, neon, hwdiv, hwdiv_arm);
constexpr auto marvell_pj4 = armv7a | get_feature_masks(vfp3);
constexpr auto intel_3735d = armv7a | get_feature_masks(vfp3, neon);
// armv8ml
constexpr auto arm_cortex_m23 = armv8m; // unsupported
constexpr auto arm_cortex_m33 = armv8m | get_feature_masks(v8_m_main); // unsupported
// armv8l
constexpr auto armv8a_crc = armv8a | get_feature_masks(crc);
constexpr auto armv8_1a = armv8a_crc | get_feature_masks(v8_1a);
constexpr auto armv8_2a = armv8_1a | get_feature_masks(v8_2a);
constexpr auto armv8a_crc_crypto = armv8a_crc | get_feature_masks(crypto);
constexpr auto armv8_2a_crypto = armv8_2a | get_feature_masks(crypto);
constexpr auto armv8_3a = armv8_2a | get_feature_masks(v8_3a);
constexpr auto armv8_3a_crypto = armv8_3a | get_feature_masks(crypto);

constexpr auto arm_cortex_a32 = armv8a; // TODO? (crc, crypto)
constexpr auto arm_cortex_r52 = armv8r; // TODO? (crc, crypto)
constexpr auto arm_cortex_a35 = armv8a; // TODO? (crc, crypto)
constexpr auto arm_cortex_a53 = armv8a_crc;
constexpr auto arm_cortex_a55 = armv8_2a_crypto;
constexpr auto arm_cortex_a57 = armv8a_crc;
constexpr auto arm_cortex_a72 = armv8a_crc;
constexpr auto arm_cortex_a73 = armv8a_crc;
constexpr auto arm_cortex_a75 = armv8_2a_crypto;
constexpr auto cavium_thunderx = armv8a_crc_crypto;
constexpr auto cavium_thunderx88 = armv8a_crc_crypto;
constexpr auto cavium_thunderx88p1 = armv8a_crc_crypto;
constexpr auto cavium_thunderx81 = armv8a_crc_crypto;
constexpr auto cavium_thunderx83 = armv8a_crc_crypto;
constexpr auto cavium_thunderx2t99 = armv8a_crc_crypto | get_feature_masks(v8_1a);
constexpr auto cavium_thunderx2t99p1 = armv8a_crc_crypto | get_feature_masks(v8_1a);
constexpr auto nvidia_denver1 = armv8a; // TODO? (crc, crypto)
constexpr auto nvidia_denver2 = armv8a_crc_crypto;
constexpr auto apm_xgene1 = armv8a;
constexpr auto apm_xgene2 = armv8a; // TODO?
constexpr auto apm_xgene3 = armv8a; // TODO?
constexpr auto qualcomm_kyro = armv8a_crc_crypto;
constexpr auto qualcomm_falkor = armv8a_crc_crypto;
constexpr auto qualcomm_saphira = armv8_3a_crypto;
constexpr auto samsung_exynos_m1 = armv8a_crc_crypto;
constexpr auto samsung_exynos_m2 = armv8a_crc_crypto;
constexpr auto samsung_exynos_m3 = armv8a_crc_crypto;
constexpr auto apple_cyclone = armv8a_crc_crypto;
constexpr auto apple_typhoon = armv8a_crc_crypto;
constexpr auto apple_twister = armv8a_crc_crypto;
constexpr auto apple_hurricane = armv8a_crc_crypto;

}

static constexpr CPUSpec<CPU, feature_sz> cpus[] = {
    {"generic", CPU::generic, CPU::generic, 0, Feature::generic},
    // armv6
    {"mpcore", CPU::arm_mpcore, CPU::generic, 0, Feature::generic},
    {"arm1136jf-s", CPU::arm_1136jf_s, CPU::generic, 0, Feature::generic},
    {"arm1156t2f-s", CPU::arm_1156t2f_s, CPU::generic, 0, Feature::generic},
    {"arm1176jzf-s", CPU::arm_1176jzf_s, CPU::generic, 0, Feature::generic},
    {"cortex-m0", CPU::arm_cortex_m0, CPU::generic, 0, Feature::generic},
    {"cortex-m1", CPU::arm_cortex_m1, CPU::generic, 0, Feature::generic},
    // armv7ml
    {"armv7-m", CPU::armv7_m, CPU::generic, 0, Feature::armv7m},
    {"armv7e-m", CPU::armv7e_m, CPU::generic, 0, Feature::armv7m},
    {"cortex-m3", CPU::arm_cortex_m3, CPU::generic, 0, Feature::armv7m},
    {"cortex-m4", CPU::arm_cortex_m4, CPU::generic, 0, Feature::armv7m},
    {"cortex-m7", CPU::arm_cortex_m7, CPU::generic, 0, Feature::armv7m},
    // armv7l
    {"armv7-a", CPU::armv7_a, CPU::generic, 0, Feature::armv7a},
    {"armv7-r", CPU::armv7_r, CPU::generic, 0, Feature::armv7r},
    {"cortex-a5", CPU::arm_cortex_a5, CPU::generic, 0, Feature::arm_cortex_a5},
    {"cortex-a7", CPU::arm_cortex_a7, CPU::generic, 0, Feature::arm_cortex_a7},
    {"cortex-a8", CPU::arm_cortex_a8, CPU::generic, 0, Feature::arm_cortex_a8},
    {"cortex-a9", CPU::arm_cortex_a9, CPU::generic, 0, Feature::arm_cortex_a9},
    {"cortex-a12", CPU::arm_cortex_a12, CPU::generic, 0, Feature::arm_cortex_a12},
    {"cortex-a15", CPU::arm_cortex_a15, CPU::generic, 0, Feature::arm_cortex_a15},
    {"cortex-a17", CPU::arm_cortex_a17, CPU::generic, 0, Feature::arm_cortex_a17},
    {"cortex-r4", CPU::arm_cortex_r4, CPU::generic, 0, Feature::arm_cortex_r4},
    {"cortex-r5", CPU::arm_cortex_r5, CPU::generic, 0, Feature::arm_cortex_r5},
    {"cortex-r7", CPU::arm_cortex_r7, CPU::generic, 0, Feature::arm_cortex_r7},
    {"cortex-r8", CPU::arm_cortex_r8, CPU::generic, 0, Feature::arm_cortex_r8},
    {"scorpion", CPU::qualcomm_scorpion, CPU::armv7_a, UINT32_MAX, Feature::qualcomm_scorpion},
    {"krait", CPU::qualcomm_krait, CPU::generic, 0, Feature::qualcomm_krait},
    {"swift", CPU::apple_swift, CPU::generic, 0, Feature::apple_swift},
    {"pj4", CPU::marvell_pj4, CPU::armv7_a, UINT32_MAX, Feature::marvell_pj4},
    {"3735d", CPU::intel_3735d, CPU::armv7_a, UINT32_MAX, Feature::intel_3735d},

    // armv8ml
    {"armv8-m.base", CPU::armv8_m_base, CPU::generic, 0, Feature::armv8m},
    {"armv8-m.main", CPU::armv8_m_main, CPU::generic, 0, Feature::armv8m},
    {"cortex-m23", CPU::arm_cortex_m23, CPU::armv8_m_base, 50000, Feature::arm_cortex_m23},
    {"cortex-m33", CPU::arm_cortex_m33, CPU::armv8_m_main, 50000, Feature::arm_cortex_m33},

    // armv8l
    {"armv8-a", CPU::armv8_a, CPU::generic, 0, Feature::armv8a},
    {"armv8-r", CPU::armv8_r, CPU::generic, 0, Feature::armv8r},
    {"armv8.1-a", CPU::armv8_1_a, CPU::generic, 0, Feature::armv8_1a},
    {"armv8.2-a", CPU::armv8_2_a, CPU::generic, 0, Feature::armv8_2a},
    {"armv8.3-a", CPU::armv8_3_a, CPU::generic, 0, Feature::armv8_3a},
    {"cortex-a32", CPU::arm_cortex_a32, CPU::generic, 0, Feature::arm_cortex_a32},
    {"cortex-r52", CPU::arm_cortex_r52, CPU::armv8_r, 40000, Feature::arm_cortex_r52},
    {"cortex-a35", CPU::arm_cortex_a35, CPU::generic, 0, Feature::arm_cortex_a35},
    {"cortex-a53", CPU::arm_cortex_a53, CPU::generic, 0, Feature::arm_cortex_a53},
    {"cortex-a55", CPU::arm_cortex_a55, CPU::arm_cortex_a53, 60000, Feature::arm_cortex_a55},
    {"cortex-a57", CPU::arm_cortex_a57, CPU::generic, 0, Feature::arm_cortex_a57},
    {"cortex-a72", CPU::arm_cortex_a72, CPU::generic, 0, Feature::arm_cortex_a72},
    {"cortex-a73", CPU::arm_cortex_a73, CPU::generic, 0, Feature::arm_cortex_a73},
    {"cortex-a75", CPU::arm_cortex_a75, CPU::arm_cortex_a73, 60000, Feature::arm_cortex_a75},
    {"thunderx", CPU::cavium_thunderx, CPU::armv8_a, UINT32_MAX, Feature::cavium_thunderx},
    {"thunderx88", CPU::cavium_thunderx88, CPU::armv8_a, UINT32_MAX, Feature::cavium_thunderx88},
    {"thunderx88p1", CPU::cavium_thunderx88p1, CPU::armv8_a, UINT32_MAX,
     Feature::cavium_thunderx88p1},
    {"thunderx81", CPU::cavium_thunderx81, CPU::armv8_a, UINT32_MAX,
     Feature::cavium_thunderx81},
    {"thunderx83", CPU::cavium_thunderx83, CPU::armv8_a, UINT32_MAX,
     Feature::cavium_thunderx83},
    {"thunderx2t99", CPU::cavium_thunderx2t99, CPU::armv8_a, UINT32_MAX,
     Feature::cavium_thunderx2t99},
    {"thunderx2t99p1", CPU::cavium_thunderx2t99p1, CPU::armv8_a, UINT32_MAX,
     Feature::cavium_thunderx2t99p1},
    {"denver1", CPU::nvidia_denver1, CPU::arm_cortex_a53, UINT32_MAX, Feature::nvidia_denver1},
    {"denver2", CPU::nvidia_denver2, CPU::arm_cortex_a57, UINT32_MAX, Feature::nvidia_denver2},
    {"xgene1", CPU::apm_xgene1, CPU::armv8_a, UINT32_MAX, Feature::apm_xgene1},
    {"xgene2", CPU::apm_xgene2, CPU::armv8_a, UINT32_MAX, Feature::apm_xgene2},
    {"xgene3", CPU::apm_xgene3, CPU::armv8_a, UINT32_MAX, Feature::apm_xgene3},
    {"kyro", CPU::qualcomm_kyro, CPU::armv8_a, UINT32_MAX, Feature::qualcomm_kyro},
    {"falkor", CPU::qualcomm_falkor, CPU::armv8_a, UINT32_MAX, Feature::qualcomm_falkor},
    {"saphira", CPU::qualcomm_saphira, CPU::armv8_a, UINT32_MAX, Feature::qualcomm_saphira},
    {"exynos-m1", CPU::samsung_exynos_m1, CPU::generic, 0, Feature::samsung_exynos_m1},
    {"exynos-m2", CPU::samsung_exynos_m2, CPU::samsung_exynos_m1, 40000,
     Feature::samsung_exynos_m2},
    {"exynos-m3", CPU::samsung_exynos_m3, CPU::samsung_exynos_m2, 40000,
     Feature::samsung_exynos_m3},
    {"cyclone", CPU::apple_cyclone, CPU::generic, 0, Feature::apple_cyclone},
    {"typhoon", CPU::apple_typhoon, CPU::apple_cyclone, UINT32_MAX, Feature::apple_typhoon},
    {"twister", CPU::apple_twister, CPU::apple_typhoon, UINT32_MAX, Feature::apple_twister},
    {"hurricane", CPU::apple_hurricane, CPU::apple_twister, UINT32_MAX, Feature::apple_hurricane},
};
#endif
static constexpr size_t ncpu_names = sizeof(cpus) / sizeof(cpus[0]);

// auxval reader

#ifndef AT_HWCAP
#  define AT_HWCAP 16
#endif
#ifndef AT_HWCAP2
#  define AT_HWCAP2 26
#endif

#if defined(USE_DYN_GETAUXVAL)
static unsigned long getauxval_procfs(unsigned long type)
{
    int fd = open("/proc/self/auxv", O_RDONLY);
    if (fd == -1)
        return 0;
    unsigned long val = 0;
    unsigned long buff[2];
    while (read(fd, buff, sizeof(buff)) == sizeof(buff)) {
        if (buff[0] == 0)
            break;
        if (buff[0] == type) {
            val = buff[1];
            break;
        }
    }
    close(fd);
    return val;
}

static inline unsigned long jl_getauxval(unsigned long type)
{
    // First, try resolving getauxval in libc
    auto libc = jl_dlopen(nullptr, JL_RTLD_LOCAL);
    static unsigned long (*getauxval_p)(unsigned long) = NULL;
    if (getauxval_p == NULL && jl_dlsym(libc, "getauxval", (void **)&getauxval_p, 0)) {
        return getauxval_p(type);
    }

    // If we couldn't resolve it, use procfs.
    return getauxval_procfs(type);
}
#else
static inline unsigned long jl_getauxval(unsigned long type)
{
    return getauxval(type);
}
#endif

struct CPUID {
    uint8_t implementer;
    uint8_t variant;
    uint16_t part;
    bool operator<(const CPUID &right) const
    {
        if (implementer < right.implementer)
            return true;
        if (implementer > right.implementer)
            return false;
        if (part < right.part)
            return true;
        if (part > right.part)
            return false;
        return variant < right.variant;
    }
};

// /sys/devices/system/cpu/cpu<n>/regs/identification/midr_el1 reader
static inline void get_cpuinfo_sysfs(std::set<CPUID> &res)
{
    // This only works on a 64bit 4.7+ kernel
    auto dir = opendir("/sys/devices/system/cpu");
    if (!dir)
        return;
    while (auto entry = readdir(dir)) {
        if (entry->d_type != DT_DIR)
            continue;
        if (strncmp(entry->d_name, "cpu", 3) != 0)
            continue;
        std::string stm;
        llvm::raw_string_ostream(stm) << "/sys/devices/system/cpu/" << entry->d_name << "/regs/identification/midr_el1";
        std::ifstream file(stm);
        if (!file)
            continue;
        uint64_t val = 0;
        file >> std::hex >> val;
        if (!file)
            continue;
        CPUID cpuid = {
            uint8_t(val >> 24),
            uint8_t((val >> 20) & 0xf),
            uint16_t((val >> 4) & 0xfff)
        };
        res.insert(cpuid);
    }
    closedir(dir);
}

// Use an external template since lambda's can't be templated in C++11
template<typename T, typename F>
static inline bool try_read_procfs_line(llvm::StringRef line, const char *prefix, T &out,
                                        bool &flag, F &&reset)
{
    if (!line.startswith(prefix))
        return false;
    if (flag)
        reset();
    flag = line.substr(strlen(prefix)).ltrim("\t :").getAsInteger(0, out);
    return true;
}

// /proc/cpuinfo reader
static inline void get_cpuinfo_procfs(std::set<CPUID> &res)
{
    std::ifstream file("/proc/cpuinfo");
    CPUID cpuid = {0, 0, 0};
    bool impl = false;
    bool part = false;
    bool var = false;
    auto reset = [&] () {
        if (impl && part)
            res.insert(cpuid);
        impl = false;
        part = false;
        var = false;
        memset(&cpuid, 0, sizeof(cpuid));
    };
    for (std::string line; std::getline(file, line);) {
        if (line.empty()) {
            reset();
            continue;
        }
        try_read_procfs_line(line, "CPU implementer", cpuid.implementer, impl, reset) ||
            try_read_procfs_line(line, "CPU variant", cpuid.variant, var, reset) ||
            try_read_procfs_line(line, "CPU part", cpuid.part, part, reset);
    }
    reset();
}

static std::set<CPUID> get_cpuinfo(void)
{
    std::set<CPUID> res;
    get_cpuinfo_sysfs(res);
    if (res.empty())
        get_cpuinfo_procfs(res);
    return res;
}

static CPU get_cpu_name(CPUID cpuid)
{
    switch (cpuid.implementer) {
    case 0x41: // ARM
        switch (cpuid.part) {
        case 0xb02: return CPU::arm_mpcore;
        case 0xb36: return CPU::arm_1136jf_s;
        case 0xb56: return CPU::arm_1156t2f_s;
        case 0xb76: return CPU::arm_1176jzf_s;
        case 0xc20: return CPU::arm_cortex_m0;
        case 0xc21: return CPU::arm_cortex_m1;
        case 0xc23: return CPU::arm_cortex_m3;
        case 0xc24: return CPU::arm_cortex_m4;
        case 0xc27: return CPU::arm_cortex_m7;
        case 0xd20: return CPU::arm_cortex_m23;
        case 0xd21: return CPU::arm_cortex_m33;
        case 0xc05: return CPU::arm_cortex_a5;
        case 0xc07: return CPU::arm_cortex_a7;
        case 0xc08: return CPU::arm_cortex_a8;
        case 0xc09: return CPU::arm_cortex_a9;
        case 0xc0d: return CPU::arm_cortex_a12;
        case 0xc0f: return CPU::arm_cortex_a15;
        case 0xc0e: return CPU::arm_cortex_a17;
        case 0xc14: return CPU::arm_cortex_r4;
        case 0xc15: return CPU::arm_cortex_r5;
        case 0xc17: return CPU::arm_cortex_r7;
        case 0xc18: return CPU::arm_cortex_r8;
        case 0xd13: return CPU::arm_cortex_r52;
        case 0xd01: return CPU::arm_cortex_a32;
        case 0xd04: return CPU::arm_cortex_a35;
        case 0xd03: return CPU::arm_cortex_a53;
        case 0xd05: return CPU::arm_cortex_a55;
        case 0xd07: return CPU::arm_cortex_a57;
        case 0xd08: return CPU::arm_cortex_a72;
        case 0xd09: return CPU::arm_cortex_a73;
        case 0xd0a: return CPU::arm_cortex_a75;
        default: return CPU::generic;
        }
    case 0x42: // Broadcom (Cavium)
        switch (cpuid.part) {
        case 0x516: return CPU::cavium_thunderx2t99p1;
        default: return CPU::generic;
        }
    case 0x43: // Cavium
        switch (cpuid.part) {
        case 0xa0: return CPU::cavium_thunderx;
        case 0xa1:
            if (cpuid.variant == 0)
                return CPU::cavium_thunderx88p1;
            return CPU::cavium_thunderx88;
        case 0xa2: return CPU::cavium_thunderx81;
        case 0xa3: return CPU::cavium_thunderx83;
        case 0xaf: return CPU::cavium_thunderx2t99;
        default: return CPU::generic;
        }
    case 0x4e: // NVIDIA
        switch (cpuid.part) {
        case 0x000: return CPU::nvidia_denver1;
        case 0x003: return CPU::nvidia_denver2;
        default: return CPU::generic;
        }
    case 0x50: // AppliedMicro
        // x-gene 2
        // x-gene 3
        switch (cpuid.part) {
        case 0x000: return CPU::apm_xgene1;
        default: return CPU::generic;
        }
    case 0x51: // Qualcomm
        switch (cpuid.part) {
        case 0x00f:
        case 0x02d:
            return CPU::qualcomm_scorpion;
        case 0x04d:
        case 0x06f:
            return CPU::qualcomm_krait;
        case 0x201:
        case 0x205:
        case 0x211:
            return CPU::qualcomm_kyro;
        case 0x800:
        case 0x801:
            return CPU::arm_cortex_a73; // second-generation Kryo
        case 0xc00:
            return CPU::qualcomm_falkor;
        case 0xc01:
            return CPU::qualcomm_saphira;
        default: return CPU::generic;
        }
    case 0x53: // Samsung
        // exynos-m2
        // exynos-m3
        switch (cpuid.part) {
        case 0x001: return CPU::samsung_exynos_m1;
        default: return CPU::generic;
        }
    case 0x56: // Marvell
        switch (cpuid.part) {
        case 0x581:
        case 0x584:
            return CPU::marvell_pj4;
        default: return CPU::generic;
        }
    case 0x67: // Apple
        // swift
        // cyclone
        // twister
        // hurricane
        switch (cpuid.part) {
        case 0x072: return CPU::apple_typhoon;
        default: return CPU::generic;
        }
    case 0x69: // Intel
        switch (cpuid.part) {
        case 0x001: return CPU::intel_3735d;
        default: return CPU::generic;
        }
    default:
        return CPU::generic;
    }
}

static std::pair<int,char> get_elf_arch(void)
{
#ifdef _CPU_AARCH64_
    return std::make_pair(8, 'A');
#else
    int ver = 0;
    char profile = 0;
    struct utsname name;
    if (uname(&name) >= 0) {
        // name.machine is the elf_platform in the kernel.
        if (strcmp(name.machine, "armv6l") == 0) {
            ver = 6;
        }
        else if (strcmp(name.machine, "armv7l") == 0) {
            ver = 7;
        }
        else if (strcmp(name.machine, "armv7ml") == 0) {
            ver = 7;
            profile = 'M';
        }
        else if (strcmp(name.machine, "armv8l") == 0 || strcmp(name.machine, "aarch64") == 0) {
            ver = 8;
        }
    }
    if (__ARM_ARCH > ver)
        ver = __ARM_ARCH;
#  if __ARM_ARCH > 6 && defined(__ARM_ARCH_PROFILE)
    profile = __ARM_ARCH_PROFILE;
#  endif
    return std::make_pair(ver, profile);
#endif
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

static std::pair<int,bool> feature_arch_version(const FeatureList<feature_sz> &feature)
{
#ifdef _CPU_AARCH64_
    return std::make_pair(8, false);
#else
    if (test_nbit(feature, Feature::v8))
        return std::make_pair(8, test_nbit(feature, Feature::mclass));
    if (test_nbit(feature, Feature::v7))
        return std::make_pair(7, test_nbit(feature, Feature::mclass));
    return std::make_pair(6, false);
#endif
}

static CPU generic_for_arch(std::pair<int,bool> arch)
{
#ifdef _CPU_AARCH64_
    return CPU::generic;
#else
#  if defined(__ARM_ARCH_PROFILE)
    char klass = __ARM_ARCH_PROFILE;
#  else
    char klass = arch.second ? 'M' : 'A';
#  endif
    if (arch.first >= 8) {
        if (klass == 'M') {
            return CPU::armv8_m_base;
        }
        else if (klass == 'R') {
            return CPU::armv8_r;
        }
        else {
            return CPU::armv8_a;
        }
    }
    else if (arch.first == 7) {
        if (klass == 'M') {
            return CPU::armv7_m;
        }
        else if (klass == 'R') {
            return CPU::armv7_r;
        }
        else {
            return CPU::armv7_a;
        }
    }
    return CPU::generic;
#endif
}

static bool check_cpu_arch_ver(uint32_t cpu, std::pair<int,bool> arch)
{
    auto spec = find_cpu(cpu);
    // This happens on AArch64 and indicates that the cpu name isn't a valid aarch64 CPU
    if (!spec)
        return false;
    auto cpu_arch = feature_arch_version(spec->features);
    if (arch.second != cpu_arch.second)
        return false;
    if (arch.first > cpu_arch.first)
        return false;
    return true;
}

static void shrink_big_little(std::vector<std::pair<uint32_t,CPUID>> &list,
                              const CPU *cpus, uint32_t ncpu)
{
    auto find = [&] (uint32_t name) {
        for (uint32_t i = 0; i < ncpu; i++) {
            if (cpus[i] == CPU(name)) {
                return (int)i;
            }
        }
        return -1;
    };
    int maxidx = -1;
    for (auto &ele: list) {
        int idx = find(ele.first);
        if (idx > maxidx) {
            maxidx = idx;
        }
    }
    if (maxidx >= 0) {
        list.erase(std::remove_if(list.begin(), list.end(), [&] (std::pair<uint32_t,CPUID> &ele) {
                    int idx = find(ele.first);
                    return idx != -1 && idx < maxidx;
                }), list.end());
    }
}

static NOINLINE std::pair<uint32_t,FeatureList<feature_sz>> _get_host_cpu()
{
    FeatureList<feature_sz> features = {};
    // Here we assume that only the lower 32bit are used on aarch64
    // Change the cast here when that's not the case anymore (and when there's features in the
    // high bits that we want to detect).
    features[0] = (uint32_t)jl_getauxval(AT_HWCAP);
    features[1] = (uint32_t)jl_getauxval(AT_HWCAP2);
    auto cpuinfo = get_cpuinfo();
    auto arch = get_elf_arch();
#ifdef _CPU_ARM_
    if (arch.first >= 7) {
        if (arch.second == 'M') {
            set_bit(features, Feature::mclass, true);
        }
        else if (arch.second == 'R') {
            set_bit(features, Feature::rclass, true);
        }
        else if (arch.second == 'A') {
            set_bit(features, Feature::aclass, true);
        }
    }
    switch (arch.first) {
    case 8:
    set_bit(features, Feature::v8, true);
    JL_FALLTHROUGH;
    case 7:
    set_bit(features, Feature::v7, true);
    break;
    default:
    break;
    }
#endif

    std::set<uint32_t> cpus;
    std::vector<std::pair<uint32_t,CPUID>> list;
    for (auto info: cpuinfo) {
        auto name = (uint32_t)get_cpu_name(info);
        if (name == 0)
            continue;
        if (!check_cpu_arch_ver(name, arch))
            continue;
        if (cpus.insert(name).second) {
            features = features | find_cpu(name)->features;
            list.emplace_back(name, info);
        }
    }
    // Not all elements/pairs are valid
    static constexpr CPU v8order[] = {
        CPU::arm_cortex_a32,
        CPU::arm_cortex_a35,
        CPU::arm_cortex_a53,
        CPU::arm_cortex_a55,
        CPU::arm_cortex_a57,
        CPU::arm_cortex_a72,
        CPU::arm_cortex_a73,
        CPU::arm_cortex_a75,
        CPU::nvidia_denver2,
        CPU::samsung_exynos_m1
    };
    shrink_big_little(list, v8order, sizeof(v8order) / sizeof(CPU));
#ifdef _CPU_ARM_
    // Not all elements/pairs are valid
    static constexpr CPU v7order[] = {
        CPU::arm_cortex_a5,
        CPU::arm_cortex_a7,
        CPU::arm_cortex_a8,
        CPU::arm_cortex_a9,
        CPU::arm_cortex_a12,
        CPU::arm_cortex_a15,
        CPU::arm_cortex_a17
    };
    shrink_big_little(list, v7order, sizeof(v7order) / sizeof(CPU));
#endif
    uint32_t cpu = 0;
    if (list.empty()) {
        cpu = (uint32_t)generic_for_arch(arch);
    }
    else {
        // This also covers `list.size() > 1` case which means there's a unknown combination
        // consists of CPU's we know. Unclear what else we could try so just randomly return
        // one...
        cpu = list[0].first;
    }
    // Ignore feature bits that we are not interested in.
    mask_features(feature_masks, &features[0]);

    return std::make_pair(cpu, features);
}

static inline const std::pair<uint32_t,FeatureList<feature_sz>> &get_host_cpu()
{
    static auto host_cpu = _get_host_cpu();
    return host_cpu;
}

static bool is_generic_cpu_name(uint32_t cpu)
{
    switch ((CPU)cpu) {
    case CPU::generic:
    case CPU::armv7_a:
    case CPU::armv7_m:
    case CPU::armv7e_m:
    case CPU::armv7_r:
    case CPU::armv8_a:
    case CPU::armv8_m_base:
    case CPU::armv8_m_main:
    case CPU::armv8_r:
    case CPU::armv8_1_a:
    case CPU::armv8_2_a:
    case CPU::armv8_3_a:
        return true;
    default:
        return false;
    }
}

static inline const std::string &host_cpu_name()
{
    static std::string name = [] {
        if (is_generic_cpu_name(get_host_cpu().first)) {
            auto llvm_name = jl_get_cpu_name_llvm();
            if (llvm_name != "generic") {
                return llvm_name;
            }
        }
        return std::string(find_cpu_name(get_host_cpu().first));
    }();
    return name;
}

template<size_t n>
static inline void enable_depends(FeatureList<n> &features)
{
    if (test_nbit(features, Feature::v8_3a))
        set_bit(features, Feature::v8_2a, true);
    if (test_nbit(features, Feature::v8_2a))
        set_bit(features, Feature::v8_1a, true);
    if (test_nbit(features, Feature::v8_1a))
        set_bit(features, Feature::crc, true);
#ifdef _CPU_ARM_
    if (test_nbit(features, Feature::v8_1a)) {
        set_bit(features, Feature::v8, true);
        set_bit(features, Feature::aclass, true);
    }
    if (test_nbit(features, Feature::v8_m_main)) {
        set_bit(features, Feature::v8, true);
        set_bit(features, Feature::mclass, true);
    }
    if (test_nbit(features, Feature::v8)) {
        set_bit(features, Feature::v7, true);
        if (test_nbit(features, Feature::aclass)) {
            set_bit(features, Feature::neon, true);
            set_bit(features, Feature::vfp3, true);
            set_bit(features, Feature::vfp4, true);
            set_bit(features, Feature::hwdiv_arm, true);
            set_bit(features, Feature::hwdiv, true);
            set_bit(features, Feature::d32, true);
        }
    }
    ::enable_depends(features, Feature::deps, sizeof(Feature::deps) / sizeof(FeatureDep));
#else
    if (test_nbit(features, Feature::v8_1a)) {
        set_bit(features, Feature::lse, true);
        set_bit(features, Feature::rdm, true);
    }
#endif
}

template<size_t n>
static inline void disable_depends(FeatureList<n> &features)
{
#ifdef _CPU_ARM_
    ::disable_depends(features, Feature::deps, sizeof(Feature::deps) / sizeof(FeatureDep));
#endif
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
    return ::get_cmdline_targets<feature_sz>(feature_cb);
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
#ifdef _CPU_ARM_
    if (test_nbit(features, Feature::neon))
        return 16;
    return 8;
#else
    // TODO SVE
    return 16;
#endif
}

static uint32_t sysimg_init_cb(const void *id)
{
    // First see what target is requested for the JIT.
    auto &cmdline = get_cmdline_targets();
    TargetData<feature_sz> target = arg_target_data(cmdline[0], true);
    // Then find the best match in the sysimg
    auto sysimg = deserialize_target_data<feature_sz>((const uint8_t*)id);
    auto match = match_sysimg_targets(sysimg, target, max_vector_size);
    // Now we've decided on which sysimg version to use.
    // Make sure the JIT target is compatible with it and save the JIT target.
    if (match.vreg_size != max_vector_size(target.en.features) &&
        (sysimg[match.best_idx].en.flags & JL_TARGET_VEC_CALL)) {
#ifdef _CPU_ARM_
        unset_bits(target.en.features, Feature::neon);
#endif
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
#ifdef _CPU_ARM_
        auto &features0 = jit_targets[t.base].en.features;
        static constexpr uint32_t clone_math[] = {Feature::vfp3, Feature::vfp4, Feature::neon};
        for (auto fe: clone_math) {
            if (!test_nbit(features0, fe) && test_nbit(t.en.features, fe)) {
                t.en.flags |= JL_TARGET_CLONE_MATH;
                break;
            }
        }
        static constexpr uint32_t clone_simd[] = {Feature::neon};
        for (auto fe: clone_simd) {
            if (!test_nbit(features0, fe) && test_nbit(t.en.features, fe)) {
                t.en.flags |= JL_TARGET_CLONE_SIMD;
                break;
            }
        }
#endif
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
    auto features = data.en.features;
    if (spec) {
        if (is_generic_cpu_name((uint32_t)spec->cpu)) {
            features = features | spec->features;
            name = "generic";
        }
    }
    std::vector<std::string> feature_strs;
    for (auto &fename: feature_names) {
        if (fename.llvmver > JL_LLVM_VERSION)
            continue;
        if (fename.bit >= 32 * 2)
            break;
        const char *fename_str = fename.name;
        bool enable = test_nbit(features, fename.bit);
        bool disable = test_nbit(data.dis.features, fename.bit);
#if defined(_CPU_ARM_) && JL_LLVM_VERSION < 90000
        if (fename.bit == Feature::d32) {
            if (enable) {
                feature_strs.push_back("-d16");
            }
            else if (disable) {
                feature_strs.push_back("+d16");
            }
            continue;
        }
#endif
        if (enable) {
            feature_strs.insert(feature_strs.begin(), std::string("+") + fename_str);
        }
        else if (disable) {
            feature_strs.push_back(std::string("-") + fename_str);
        }
    }
    if (test_nbit(features, Feature::v8_2a))
        feature_strs.push_back("+v8.2a");
    if (test_nbit(features, Feature::v8_1a))
        feature_strs.push_back("+v8.1a");
#ifdef _CPU_ARM_
    if (test_nbit(features, Feature::v8_m_main)) {
        feature_strs.push_back("+v8m.main");
        feature_strs.push_back("+armv8-m.main");
    }
    if (test_nbit(features, Feature::aclass))
        feature_strs.push_back("+aclass");
    if (test_nbit(features, Feature::rclass))
        feature_strs.push_back("+rclass");
    if (test_nbit(features, Feature::mclass))
        feature_strs.push_back("+mclass");
    if (test_nbit(features, Feature::v8)) {
        feature_strs.push_back("+v8");
        if (test_nbit(features, Feature::aclass))
            feature_strs.push_back("+armv8-a");
        if (test_nbit(features, Feature::rclass))
            feature_strs.push_back("+armv8-r");
        if (test_nbit(features, Feature::mclass)) {
            feature_strs.push_back("+v8m");
            feature_strs.push_back("+armv8-m.base");
        }
    }
    if (test_nbit(features, Feature::v7)) {
        feature_strs.push_back("+v7");
        if (test_nbit(features, Feature::aclass))
            feature_strs.push_back("+armv7-a");
        if (test_nbit(features, Feature::rclass))
            feature_strs.push_back("+armv7-r");
        if (test_nbit(features, Feature::mclass))
            feature_strs.push_back("+armv7-m");
    }
    feature_strs.push_back("+v6");
    feature_strs.push_back("+vfp2");
#else
    feature_strs.push_back("+neon");
    feature_strs.push_back("+fp-armv8");
#endif
    return std::make_pair(std::move(name), std::move(feature_strs));
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

static FeatureList<feature_sz> get_max_feature(void)
{
#ifdef _CPU_ARM_
    auto arch = get_elf_arch();
    auto features = real_feature_masks;
    if (arch.second == 0)
        arch.second = 'A';
    set_bit(features, Feature::v7, true);
    set_bit(features, Feature::v8, true);
    if (arch.second == 'M') {
        set_bit(features, Feature::mclass, true);
        set_bit(features, Feature::v8_m_main, true);
    }
    else if (arch.second == 'R') {
        set_bit(features, Feature::rclass, true);
    }
    else if (arch.second == 'A') {
        set_bit(features, Feature::aclass, true);
        set_bit(features, Feature::v8_1a, true);
        set_bit(features, Feature::v8_2a, true);
    }
    return features;
#else
    // There isn't currently any conflicting features on AArch64
    return feature_masks;
#endif
}

}

using namespace ARM;

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
    // RAS is not currently detectable AFAICT
    auto max_feature = get_max_feature();
    static const auto res = get_llvm_target_str(TargetData<feature_sz>{host_cpu_name(),
                "+dotprod,+ras",
                {max_feature, 0}, {feature_masks & ~max_feature, 0}, 0});
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
        ARM::disable_depends(features_en);
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

#ifdef _CPU_AARCH64_
// FZ, bit [24]
static constexpr uint32_t fpcr_fz_mask = 1 << 24;

static inline uint32_t get_fpcr_aarch64(void)
{
    uint32_t fpcr;
    asm volatile("mrs %0, fpcr" : "=r"(fpcr));
    return fpcr;
}

static inline void set_fpcr_aarch64(uint32_t fpcr)
{
    asm volatile("msr fpcr, %0" :: "r"(fpcr));
}

extern "C" JL_DLLEXPORT int32_t jl_get_zero_subnormals(void)
{
    return (get_fpcr_aarch64() & fpcr_fz_mask) != 0;
}

extern "C" JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    uint32_t fpcr = get_fpcr_aarch64();
    fpcr = isZero ? (fpcr | fpcr_fz_mask) : (fpcr & ~fpcr_fz_mask);
    set_fpcr_aarch64(fpcr);
    return 0;
}
#else
extern "C" JL_DLLEXPORT int32_t jl_get_zero_subnormals(void)
{
    return 0;
}

extern "C" JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    return isZero;
}
#endif
