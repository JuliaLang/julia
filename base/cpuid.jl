# This file is a part of Julia. License is MIT: https://julialang.org/license

module CPUID

export cpu_isa

"""
    ISA(features::Set{UInt32})

A structure which represents the Instruction Set Architecture (ISA) of a
computer.  It holds the `Set` of features of the CPU.

The numerical values of the features are automatically generated from the C
source code of Julia and stored in the `features_h.jl` Julia file.
"""
struct ISA
    features::Set{UInt32}
end

Base.:<=(a::ISA, b::ISA) = a.features <= b.features
Base.:<(a::ISA,  b::ISA) = a.features <  b.features
Base.isless(a::ISA,  b::ISA) = a < b

include(string(length(Core.ARGS) >= 2 ? Core.ARGS[2] : "", "features_h.jl"))  # include($BUILDROOT/base/features_h.jl)

# Keep in sync with `arch_march_isa_mapping`.
const ISAs_by_family = Dict(
    "i686" => [
        # Source: https://gcc.gnu.org/onlinedocs/gcc/x86-Options.html.
        # Implicit in all sets, because always required by Julia: mmx, sse, sse2
        "pentium4" => ISA(Set{UInt32}()),
        "prescott" => ISA(Set((JL_X86_sse3,))),
    ],
    "x86_64" => [
        # Source: https://gcc.gnu.org/onlinedocs/gcc/x86-Options.html.
        # Implicit in all sets, because always required by x86-64 architecture: mmx, sse, sse2
        "x86_64" => ISA(Set{UInt32}()),
        "core2" => ISA(Set((JL_X86_sse3, JL_X86_ssse3))),
        "nehalem" => ISA(Set((JL_X86_sse3, JL_X86_ssse3, JL_X86_sse41, JL_X86_sse42, JL_X86_popcnt))),
        "sandybridge" => ISA(Set((JL_X86_sse3, JL_X86_ssse3, JL_X86_sse41, JL_X86_sse42, JL_X86_popcnt, JL_X86_avx, JL_X86_aes, JL_X86_pclmul))),
        "haswell" => ISA(Set((JL_X86_movbe, JL_X86_sse3, JL_X86_ssse3, JL_X86_sse41, JL_X86_sse42, JL_X86_popcnt, JL_X86_avx, JL_X86_avx2, JL_X86_aes, JL_X86_pclmul, JL_X86_fsgsbase, JL_X86_rdrnd, JL_X86_fma, JL_X86_bmi, JL_X86_bmi2, JL_X86_f16c))),
        "skylake" => ISA(Set((JL_X86_movbe, JL_X86_sse3, JL_X86_ssse3, JL_X86_sse41, JL_X86_sse42, JL_X86_popcnt, JL_X86_avx, JL_X86_avx2, JL_X86_aes, JL_X86_pclmul, JL_X86_fsgsbase, JL_X86_rdrnd, JL_X86_fma, JL_X86_bmi, JL_X86_bmi2, JL_X86_f16c, JL_X86_rdseed, JL_X86_adx, JL_X86_prfchw, JL_X86_clflushopt, JL_X86_xsavec, JL_X86_xsaves))),
        "skylake_avx512" => ISA(Set((JL_X86_movbe, JL_X86_sse3, JL_X86_ssse3, JL_X86_sse41, JL_X86_sse42, JL_X86_popcnt, JL_X86_pku, JL_X86_avx, JL_X86_avx2, JL_X86_aes, JL_X86_pclmul, JL_X86_fsgsbase, JL_X86_rdrnd, JL_X86_fma, JL_X86_bmi, JL_X86_bmi2, JL_X86_f16c, JL_X86_rdseed, JL_X86_adx, JL_X86_prfchw, JL_X86_clflushopt, JL_X86_xsavec, JL_X86_xsaves, JL_X86_avx512f, JL_X86_clwb, JL_X86_avx512vl, JL_X86_avx512bw, JL_X86_avx512dq, JL_X86_avx512cd))),
    ],
    "armv6l" => [
        # The only armv6l processor we know of that runs Julia on armv6l
        # We don't have a good way to tell the different armv6l variants apart through features,
        # and honestly we don't care much since it's basically this one chip that people want to use with Julia.
        "arm1176jzfs" => ISA(Set{UInt32}()),
    ],
    "armv7l" => [
        "armv7l" => ISA(Set{UInt32}()),
        "armv7l+neon" => ISA(Set((JL_AArch32_neon,))),
        "armv7l+neon+vfpv4" => ISA(Set((JL_AArch32_neon, JL_AArch32_vfp4))),
    ],
    "aarch64" => [
        # Implicit in all sets, because always required: fp, asimd
        "armv8.0-a" => ISA(Set{UInt32}()),
        "armv8.1-a" => ISA(Set((JL_AArch64_lse, JL_AArch64_crc, JL_AArch64_rdm))),
        "armv8.2-a+crypto" => ISA(Set((JL_AArch64_lse, JL_AArch64_crc, JL_AArch64_rdm, JL_AArch64_aes, JL_AArch64_sha2))),
        "armv8.4-a+crypto+sve" => ISA(Set((JL_AArch64_lse, JL_AArch64_crc, JL_AArch64_rdm, JL_AArch64_fp16fml, JL_AArch64_aes, JL_AArch64_sha2, JL_AArch64_dotprod, JL_AArch64_sve))),
    ],
    "powerpc64le" => [
        # We have no way to test powerpc64le features yet, so we're only going to declare the lowest ISA:
        "power8" => ISA(Set{UInt32}()),
    ]
)

# Test a CPU feature exists on the currently-running host
test_cpu_feature(feature::UInt32) = ccall(:jl_test_cpu_feature, Bool, (UInt32,), feature)

# Normalize some variation in ARCH values (which typically come from `uname -m`)
function normalize_arch(arch::String)
    arch = lowercase(arch)
    if arch ∈ ("amd64",)
        arch = "x86_64"
    elseif arch ∈ ("i386", "i486", "i586")
        arch = "i686"
    elseif arch ∈ ("armv6",)
        arch = "armv6l"
    elseif arch ∈ ("arm", "armv7", "armv8", "armv8l")
        arch = "armv7l"
    elseif arch ∈ ("arm64",)
        arch = "aarch64"
    elseif arch ∈ ("ppc64le",)
        arch = "powerpc64le"
    end
    return arch
end

"""
    cpu_isa()

Return the [`ISA`](@ref) (instruction set architecture) of the current CPU.
"""
function cpu_isa()
    all_features = last(last(get(ISAs_by_family, normalize_arch(String(Sys.ARCH)), "" => [ISA(Set{UInt32}())]))).features
    return ISA(Set{UInt32}(feat for feat in all_features if test_cpu_feature(feat)))
end

end # module CPUID
