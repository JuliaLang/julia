# This file is a part of Julia. License is MIT: https://julialang.org/license

module CPUID

export cpu_isa

"""
    ISA(features::Set{UInt32})

A structure which represents the Instruction Set Architecture (ISA) of a
computer.  It holds the `Set` of features of the CPU.

Feature bit indices come from the cpufeatures library's generated tables
(extracted from LLVM's TableGen data at build time).
"""
struct ISA
    features::Set{UInt32}
end

Base.:<=(a::ISA, b::ISA) = a.features <= b.features
Base.:<(a::ISA,  b::ISA) = a.features <  b.features
Base.isless(a::ISA,  b::ISA) = a < b

include(string(Base.BUILDROOT, "features_h.jl"))  # include($BUILDROOT/base/features_h.jl)

"""
    _featurebytes_to_isa(buf::Vector{UInt8}) -> ISA

Convert a raw feature byte buffer (from cpufeatures) into an ISA.
"""
function _featurebytes_to_isa(buf::Vector{UInt8})
    features = Set{UInt32}()
    for byte_idx in 0:length(buf)-1
        b = buf[byte_idx + 1]
        b == 0 && continue
        for bit in 0:7
            if (b >> bit) & 1 != 0
                push!(features, UInt32(byte_idx * 8 + bit))
            end
        end
    end
    return ISA(features)
end

"""
    _cross_lookup_cpu(arch::String, name::String) -> ISA

Look up hardware features for a CPU on any architecture using the
cross-arch tables. Works regardless of host architecture.
Returns an empty ISA if the CPU or architecture is not found.
"""
function _cross_lookup_cpu(arch::String, name::String)
    nbytes = ccall(:jl_cpufeatures_cross_nbytes, Csize_t, (Cstring,), arch)
    nbytes == 0 && return ISA(Set{UInt32}())
    buf = Vector{UInt8}(undef, nbytes)
    written = ccall(:jl_cpufeatures_cross_lookup, Csize_t,
                    (Cstring, Cstring, Ptr{UInt8}, Csize_t),
                    arch, name, buf, nbytes)
    written == 0 && return ISA(Set{UInt32}())
    return _featurebytes_to_isa(buf)
end

"""
    _build_bit_to_name(arch::String) -> Dict{UInt32, String}

Build a mapping from feature bit index to feature name for an architecture.
"""
function _build_bit_to_name(arch::String)
    nfeats = ccall(:jl_cpufeatures_cross_num_features, UInt32, (Cstring,), arch)
    result = Dict{UInt32, String}()
    for i in 0:nfeats-1
        name_ptr = ccall(:jl_cpufeatures_cross_feature_name, Cstring, (Cstring, UInt32), arch, i)
        name_ptr == C_NULL && continue
        bit = ccall(:jl_cpufeatures_cross_feature_bit, Cint, (Cstring, UInt32), arch, i)
        bit < 0 && continue
        result[UInt32(bit)] = unsafe_string(name_ptr)
    end
    return result
end

"""
    feature_names(arch::String, cpu::String) -> Vector{String}
    feature_names(arch::String, isa::ISA) -> Vector{String}
    feature_names(isa::ISA) -> Vector{String}
    feature_names() -> Vector{String}

Return sorted hardware feature names. Can query by CPU name (on any
architecture) or by ISA. Defaults to the host architecture and CPU.

# Examples
```julia
feature_names()                           # host CPU features
feature_names("x86_64", "haswell")        # haswell's features
feature_names("aarch64", "cortex-x925")   # cross-arch query
```
"""
feature_names() = feature_names(string(Sys.ARCH), _host_isa())
feature_names(isa::ISA) = feature_names(string(Sys.ARCH), isa)
function feature_names(arch::String, cpu::String)
    isa = _cross_lookup_cpu(arch, cpu)
    return feature_names(arch, isa)
end
function feature_names(arch::String, isa::ISA)
    mapping = _build_bit_to_name(arch)
    return sort([get(mapping, bit, "unknown_$bit") for bit in isa.features])
end

"""
    _lookup_cpu(name::String) -> ISA

Look up hardware features for the named CPU on the host architecture.
Returns an empty ISA if the CPU name is not found.
"""
function _lookup_cpu(name::String)
    nbytes = ccall(:jl_cpufeatures_nbytes, Csize_t, ())
    buf = Vector{UInt8}(undef, nbytes)
    ret = ccall(:jl_cpufeatures_lookup, Cint, (Cstring, Ptr{UInt8}, Csize_t), name, buf, nbytes)
    ret != 0 && return ISA(Set{UInt32}())
    return _featurebytes_to_isa(buf)
end

"""
    _host_isa() -> ISA

Get the hardware features of the host CPU from the cpufeatures library.
"""
function _host_isa()
    nbytes = ccall(:jl_cpufeatures_nbytes, Csize_t, ())
    buf = Vector{UInt8}(undef, nbytes)
    ccall(:jl_cpufeatures_host, Cvoid, (Ptr{UInt8}, Csize_t), buf, nbytes)
    return _featurebytes_to_isa(buf)
end

# Build an ISA list for a given architecture family.
# Uses cross-arch lookup so it works on any host.
# Entries with empty cpuname get an empty ISA (generic baseline).
function _make_isa_list(arch::String, entries::Vector{Pair{String,String}})
    result = Pair{String,ISA}[]
    for (label, cpuname) in entries
        if isempty(cpuname)
            push!(result, label => ISA(Set{UInt32}()))
        else
            push!(result, label => _cross_lookup_cpu(arch, cpuname))
        end
    end
    return result
end

# ISA definitions per architecture family.
# CPU names are LLVM names in the cpufeatures database.
# Keep in sync with `arch_march_isa_mapping` in binaryplatforms.jl.
const ISAs_by_family = Dict(
    "i686" => _make_isa_list("x86_64", [
        "pentium4" => "",
        "prescott" => "prescott",
    ]),
    "x86_64" => _make_isa_list("x86_64", [
        "x86_64" => "",
        "core2" => "core2",
        "nehalem" => "nehalem",
        "sandybridge" => "sandybridge",
        "haswell" => "haswell",
        "skylake" => "skylake",
        "skylake_avx512" => "skylake-avx512",
    ]),
    "aarch64" => _make_isa_list("aarch64", [
        "armv8.0-a" => "",
        "armv8.1-a" => "cortex-a76",
        "armv8.2-a+crypto" => "cortex-a78",
        "a64fx" => "a64fx",
        "apple_m1" => "apple-a14",
    ]),
    "riscv64" => _make_isa_list("riscv64", [
        "riscv64" => "",
    ]),
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
    return _host_isa()
end

end # module CPUID
