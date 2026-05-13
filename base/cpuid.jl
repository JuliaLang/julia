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

# Architectures recognised as an arch prefix in `@cpu_supports`.
const _KNOWN_ARCHES = (:x86_64, :aarch64, :arm64, :riscv64)

_normalize_arch(a::Symbol) = a === :arm64 ? :aarch64 : a

# Per-arch LLVM feature-name table for `@cpu_supports` parse-time validation.
const _KNOWN_CPU_FEATURES_BY_ARCH = OncePerProcess{Dict{Symbol,Set{Symbol}}}() do
    d = Dict{Symbol,Set{Symbol}}()
    for a in ("x86_64", "aarch64", "riscv64")
        s = Set{Symbol}()
        for (_, name) in _build_bit_to_name(a)
            push!(s, Symbol(name))
        end
        d[Symbol(a)] = s
    end
    d
end

# Host-arch shorthand.
_KNOWN_CPU_FEATURES() = get(_KNOWN_CPU_FEATURES_BY_ARCH(), _normalize_arch(Sys.ARCH), Set{Symbol}())

# Expand a CPU name to its JIT-relevant feature set via the same
# `resolve_targets_for_llvm` path multiversioning uses. Returns
# `nothing` for unknown names.
function _lookup_cpu_features(name::String)
    result = ccall(:jl_cpu_uarch_expand_features, Any, (Cstring,), name)
    result === nothing && return nothing
    feature_str = result::String
    isempty(feature_str) && return Symbol[]
    return sort!([Symbol(f) for f in split(feature_str, ',')])
end

@noinline _bad_feature_arg(x) =
    error("@cpu_supports: expected literal feature names (Symbol or String), got $(repr(x))")

# Parse a single macro argument as a feature Symbol.
function _parse_feature_arg(feature)
    if feature isa Symbol
        return feature
    elseif feature isa QuoteNode && feature.value isa Symbol
        return feature.value
    elseif feature isa AbstractString
        return Symbol(feature)
    else
        _bad_feature_arg(feature)
    end
end

"""
    Base.@cpu_supports arch name1 [name2 ...] -> Bool

Compile-time CPU capability query, ANDed across names. `arch` is a literal
architecture name (`x86_64`, `aarch64`, or `riscv64`) that scopes the
query; each `nameN` is a literal naming an LLVM target feature (`avx2`,
`fma`, `"sse4.2"`) or a CPU model (`haswell`, `znver4`, `"x86-64-v3"`).
Strings are required for names that contain `-` or `.`. CPU-model names
expand to their JIT-relevant feature set. Unknown names error at
macro-expansion time.

On a host whose arch doesn't match the prefix, the call folds to `false`
at parse time — letting you write cross-arch dispatch in a single source
file.

Mirrors GCC/Clang's `__builtin_cpu_supports`, folded against the function's
effective `target-features` / `target-cpu` (so multiversioning clones get
per-clone answers automatically).

# Patterns

Dispatch — the dead branch is eliminated:
```julia
function dot(xs, ys)
    if @cpu_supports x86_64 avx2 fma
        ...  # 256-bit path
    elseif @cpu_supports aarch64 neon
        ...  # NEON path
    else
        ...  # fallback
    end
end
```

Specialized kernel guard — the body after the assert is DCE'd on clones
that don't satisfy it, so target-specific intrinsics inside never reach
backend lowering:
```julia
function kernel_avx512(xs)
    @assert @cpu_supports x86_64 avx512f
    ...
end
```
"""
macro cpu_supports(names...)
    isempty(names) && error("@cpu_supports: at least one name required")
    # Arch-independent sentinel.
    if length(names) == 1 && _parse_feature_arg(names[1]) === :__never__
        return :(Core.Intrinsics.cpu_supports(:__never__))
    end
    length(names) < 2 && error("@cpu_supports: syntax is `@cpu_supports <arch> <name>...` where <arch> ∈ $(_KNOWN_ARCHES)")
    arch_sym = _parse_feature_arg(names[1])
    arch_sym in _KNOWN_ARCHES || error("@cpu_supports: first argument must be an architecture name (one of $(_KNOWN_ARCHES)), got `$arch_sym`")
    arch = _normalize_arch(arch_sym)
    names = names[2:end]
    if arch !== _normalize_arch(Sys.ARCH)
        # Arch doesn't match host — fold to false at parse time. No further
        # validation of the feature names (we can't validate cross-arch CPU
        # models on a host of a different arch anyway).
        return false
    end
    feature_set = get(_KNOWN_CPU_FEATURES_BY_ARCH(), arch, Set{Symbol}())

    exprs = Expr[]
    for name in names
        sym = _parse_feature_arg(name)
        if sym === :__never__
            # Sentinel: always folds to false via the pass's unknown-name
            # handling. For lit-test use.
            push!(exprs, :(Core.Intrinsics.cpu_supports($(QuoteNode(sym)))))
            continue
        end
        if sym in feature_set
            push!(exprs, :(Core.Intrinsics.cpu_supports($(QuoteNode(sym)))))
            continue
        end
        # Try as a CPU model (host-arch lookup only).
        features = _lookup_cpu_features(String(sym))
        if features !== nothing
            isempty(features) && error("@cpu_supports: CPU model `$sym` has no hw features (vacuously true)")
            for f in features
                push!(exprs, :(Core.Intrinsics.cpu_supports($(QuoteNode(f)))))
            end
            continue
        end
        error("@cpu_supports: unknown feature or CPU model `$sym` for $(Sys.ARCH)")
    end
    return foldr((a, b) -> :($a & $b), exprs)
end

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
