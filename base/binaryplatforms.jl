# This file is a part of Julia. License is MIT: https://julialang.org/license

module BinaryPlatforms

export AbstractPlatform, Platform, HostPlatform, platform_dlext, tags, arch, os,
       os_version, libc, libgfortran_version, libstdcxx_version,
       cxxstring_abi, parse_dl_name_version, detect_libgfortran_version,
       detect_libstdcxx_version, detect_cxxstring_abi, call_abi, wordsize, triplet,
       select_platform, platforms_match, platform_name
import .Libc.Libdl

### Submodule with information about CPU features
include("cpuid.jl")
using .CPUID

# This exists to ease compatibility with old-style Platform objects
abstract type AbstractPlatform; end

"""
    Platform

A `Platform` represents all relevant pieces of information that a julia process may need
to know about its execution environment, such as the processor architecture, operating
system, libc implementation, etc...  It is, at its heart, a key-value mapping of tags
(such as `arch`, `os`, `libc`, etc...) to values (such as `"arch" => "x86_64"`, or
`"os" => "windows"`, etc...).  `Platform` objects are extensible in that the tag mapping
is open for users to add their own mappings to, as long as the mappings do not conflict
with the set of reserved tags: `arch`, `os`, `os_version`, `libc`, `call_abi`,
`libgfortran_version`, `libstdcxx_version`, `cxxstring_abi` and `julia_version`.

Valid tags and values are composed of alphanumeric and period characters.  All tags and
values will be lowercased when stored to reduce variation.

Example:

    Platform("x86_64", "windows"; cuda = "10.1")
"""
struct Platform <: AbstractPlatform
    tags::Dict{String,String}
    # The "compare strategy" allows selective overriding on how a tag is compared
    compare_strategies::Dict{String,Function}

    # Passing `tags` as a `Dict` avoids the need to infer different NamedTuple specializations
    function Platform(arch::String, os::String, _tags::Dict{String};
                      validate_strict::Bool = false,
                      compare_strategies::Dict{String,<:Function} = Dict{String,Function}())
        # A wee bit of normalization
        os = lowercase(os)
        arch = CPUID.normalize_arch(arch)

        tags = Dict{String,String}(
            "arch" => arch,
            "os" => os,
        )
        for (tag, value) in _tags
            value = value::Union{String,VersionNumber,Nothing}
            tag = lowercase(tag)
            if tag ∈ ("arch", "os")
                throw(ArgumentError("Cannot double-pass key $(tag)"))
            end

            # Drop `nothing` values; this means feature is not present or use default value.
            if value === nothing
                continue
            end

            # Normalize things that are known to be version numbers so that comparisons are easy.
            # Note that in our effort to be extremely compatible, we actually allow something that
            # doesn't parse nicely into a VersionNumber to persist, but if `validate_strict` is
            # set to `true`, it will cause an error later on.
            if tag ∈ ("libgfortran_version", "libstdcxx_version", "os_version")
                if isa(value, VersionNumber)
                    value = string(value)
                elseif isa(value, String)
                    v = tryparse(VersionNumber, value)
                    if isa(v, VersionNumber)
                        value = string(v)
                    end
                end
            end

            # Use `add_tag!()` to add the tag to our collection of tags
            add_tag!(tags, tag, string(value)::String)
        end

        # Auto-map call_abi and libc where necessary:
        if os == "linux" && !haskey(tags, "libc")
            # Default to `glibc` on Linux
            tags["libc"] = "glibc"
        end
        if os == "linux" && arch ∈ ("armv7l", "armv6l") && "call_abi" ∉ keys(tags)
            # default `call_abi` to `eabihf` on 32-bit ARM
            tags["call_abi"] = "eabihf"
        end

        # If the user is asking for strict validation, do so.
        if validate_strict
            validate_tags(tags)
        end

        # By default, we compare julia_version only against major and minor versions:
        if haskey(tags, "julia_version") && !haskey(compare_strategies, "julia_version")
            compare_strategies["julia_version"] = (a::String, b::String, a_comparator, b_comparator) -> begin
                a = VersionNumber(a)
                b = VersionNumber(b)
                return a.major == b.major && a.minor == b.minor
            end
        end

        return new(tags, compare_strategies)
    end
end

# Keyword interface (to avoid inference of specialized NamedTuple methods, use the Dict interface for `tags`)
function Platform(arch::String, os::String;
                  validate_strict::Bool = false,
                  compare_strategies::Dict{String,<:Function} = Dict{String,Function}(),
                  kwargs...)
    tags = Dict{String,Any}(String(tag)::String=>tagvalue(value) for (tag, value) in kwargs)
    return Platform(arch, os, tags; validate_strict, compare_strategies)
end

tagvalue(v::Union{String,VersionNumber,Nothing}) = v
tagvalue(v::Symbol) = String(v)
tagvalue(v::AbstractString) = convert(String, v)::String

# Simple tag insertion that performs a little bit of validation
function add_tag!(tags::Dict{String,String}, tag::String, value::String)
    # I know we said only alphanumeric and dots, but let's be generous so that we can expand
    # our support in the future while remaining as backwards-compatible as possible.  The
    # only characters that are absolutely disallowed right now are `-`, `+`, ` ` and things
    # that are illegal in filenames:
    nonos = raw"""+- /<>:"'\|?*"""
    if any(occursin(nono, tag) for nono in nonos)
        throw(ArgumentError("Invalid character in tag name \"$(tag)\"!"))
    end

    # Normalize and reject nonos
    value = lowercase(value)
    if any(occursin(nono, value) for nono in nonos)
        throw(ArgumentError("Invalid character in tag value \"$(value)\"!"))
    end
    tags[tag] = value
    return value
end

# Other `Platform` types can override this (I'm looking at you, `AnyPlatform`)
tags(p::Platform) = p.tags

# Make it act more like a dict
Base.getindex(p::AbstractPlatform, k::String) = getindex(tags(p), k)
Base.haskey(p::AbstractPlatform, k::String) = haskey(tags(p), k)
function Base.setindex!(p::AbstractPlatform, v::String, k::String)
    add_tag!(tags(p), k, v)
    return p
end

# Hash definition to ensure that it's stable
function Base.hash(p::Platform, h::UInt)
    h += 0x506c6174666f726d % UInt
    h = hash(p.tags, h)
    h = hash(p.compare_strategies, h)
    return h
end

# Simple equality definition; for compatibility testing, use `platforms_match()`
function Base.:(==)(a::Platform, b::Platform)
    return a.tags == b.tags && a.compare_strategies == b.compare_strategies
end


# Allow us to easily serialize Platform objects
function Base.repr(p::Platform; context=nothing)
    str = string(
        "Platform(",
        repr(arch(p)),
        ", ",
        repr(os(p)),
        "; ",
        join(("$(k) = $(repr(v))" for (k, v) in tags(p) if k ∉ ("arch", "os")), ", "),
        ")",
    )
end

# Make showing the platform a bit more palatable
function Base.show(io::IO, p::Platform)
    str = string(platform_name(p), " ", arch(p))
    # Add on all the other tags not covered by os/arch:
    other_tags = sort(collect(filter(kv -> kv[1] ∉ ("os", "arch"), tags(p))))
    if !isempty(other_tags)
        str = string(str, " {", join([string(k, "=", v) for (k, v) in other_tags], ", "), "}")
    end
    print(io, str)
end

function validate_tags(tags::Dict)
    throw_invalid_key(k) = throw(ArgumentError("Key \"$(k)\" cannot have value \"$(tags[k])\""))
    # Validate `arch`
    if tags["arch"] ∉ ("x86_64", "i686", "armv7l", "armv6l", "aarch64", "powerpc64le")
        throw_invalid_key("arch")
    end
    # Validate `os`
    if tags["os"] ∉ ("linux", "macos", "freebsd", "windows")
        throw_invalid_key("os")
    end
    # Validate `os`/`arch` combination
    throw_os_mismatch() = throw(ArgumentError("Invalid os/arch combination: $(tags["os"])/$(tags["arch"])"))
    if tags["os"] == "windows" && tags["arch"] ∉ ("x86_64", "i686", "armv7l", "aarch64")
        throw_os_mismatch()
    end
    if tags["os"] == "macos" && tags["arch"] ∉ ("x86_64", "aarch64")
        throw_os_mismatch()
    end

    # Validate `os`/`libc` combination
    throw_libc_mismatch() = throw(ArgumentError("Invalid os/libc combination: $(tags["os"])/$(tags["libc"])"))
    if tags["os"] == "linux"
        # Linux always has a `libc` entry
        if tags["libc"] ∉ ("glibc", "musl")
            throw_libc_mismatch()
        end
    else
        # Nothing else is allowed to have a `libc` entry
        if haskey(tags, "libc")
            throw_libc_mismatch()
        end
    end

    # Validate `os`/`arch`/`call_abi` combination
    throw_call_abi_mismatch() = throw(ArgumentError("Invalid os/arch/call_abi combination: $(tags["os"])/$(tags["arch"])/$(tags["call_abi"])"))
    if tags["os"] == "linux" && tags["arch"] ∈ ("armv7l", "armv6l")
        # If an ARM linux has does not have `call_abi` set to something valid, be sad.
        if !haskey(tags, "call_abi") || tags["call_abi"] ∉ ("eabihf", "eabi")
            throw_call_abi_mismatch()
        end
    else
        # Nothing else should have a `call_abi`.
        if haskey(tags, "call_abi")
            throw_call_abi_mismatch()
        end
    end

    # Validate `libgfortran_version` is a parsable `VersionNumber`
    throw_version_number(k) = throw(ArgumentError("\"$(k)\" cannot have value \"$(tags[k])\", must be a valid VersionNumber"))
    if "libgfortran_version" in keys(tags) && tryparse(VersionNumber, tags["libgfortran_version"]) === nothing
        throw_version_number("libgfortran_version")
    end

    # Validate `cxxstring_abi` is one of the two valid options:
    if "cxxstring_abi" in keys(tags) && tags["cxxstring_abi"] ∉ ("cxx03", "cxx11")
        throw_invalid_key("cxxstring_abi")
    end

    # Validate `libstdcxx_version` is a parsable `VersionNumber`
    if "libstdcxx_version" in keys(tags) && tryparse(VersionNumber, tags["libstdcxx_version"]) === nothing
        throw_version_number("libstdcxx_version")
    end
end

function set_compare_strategy!(p::Platform, key::String, f::Function)
    if !haskey(p.tags, key)
        throw(ArgumentError("Cannot set comparison strategy for nonexistant tag $(key)!"))
    end
    p.compare_strategies[key] = f
end

function get_compare_strategy(p::Platform, key::String, default = compare_default)
    if !haskey(p.tags, key)
        throw(ArgumentError("Cannot get comparison strategy for nonexistant tag $(key)!"))
    end
    return get(p.compare_strategies, key, default)
end
get_compare_strategy(p::AbstractPlatform, key::String, default = compare_default) = default



"""
    compare_default(a::String, b::String, a_requested::Bool, b_requested::Bool)

Default comparison strategy that falls back to `a == b`.  This only ever happens if both
`a` and `b` request this strategy, as any other strategy is preferrable to this one.
"""
function compare_default(a::String, b::String, a_requested::Bool, b_requested::Bool)
    return a == b
end

"""
    compare_version_cap(a::String, b::String, a_comparator, b_comparator)

Example comparison strategy for `set_comparison_strategy!()` that implements a version
cap for host platforms that support _up to_ a particular version number.  As an example,
if an artifact is built for macOS 10.9, it can run on macOS 10.11, however if it were
built for macOS 10.12, it could not.  Therefore, the host platform of macOS 10.11 has a
version cap at `v"10.11"`.

Note that because both hosts and artifacts are represented with `Platform` objects it
is possible to call `platforms_match()` with two artifacts, a host and an artifact, an
artifact and a host, and even two hosts.  We attempt to do something intelligent for all
cases, but in the case of comparing version caps between two hosts, we return `true` only
if the two host platforms are in fact identical.
"""
function compare_version_cap(a::String, b::String, a_requested::Bool, b_requested::Bool)
    a = VersionNumber(a)
    b = VersionNumber(b)

    # If both b and a requested, then we fall back to equality:
    if a_requested && b_requested
        return a == b
    end

    # Otherwise, do the comparison between the the single version cap and the single version:
    if a_requested
        return b <= a
    else
        return a <= b
    end
end



"""
    HostPlatform(p::AbstractPlatform)

Convert a `Platform` to act like a "host"; e.g. if it has a version-bound tag such as
`"libstdcxx_version" => "3.4.26"`, it will treat that value as an upper bound, rather
than a characteristic.  `Platform` objects that define artifacts generally denote the
SDK or version that the artifact was built with, but for platforms, these versions are
generally the maximal version the platform can support.  The way this transformation
is implemented is to change the appropriate comparison strategies to treat these pieces
of data as bounds rather than points in any comparison.
"""
function HostPlatform(p::AbstractPlatform)
    if haskey(p, "os_version")
        set_compare_strategy!(p, "os_version", compare_version_cap)
    end
    if haskey(p, "libstdcxx_version")
        set_compare_strategy!(p, "libstdcxx_version", compare_version_cap)
    end
    return p
end

"""
    arch(p::AbstractPlatform)

Get the architecture for the given `Platform` object as a `String`.

# Examples
```jldoctest
julia> arch(Platform("aarch64", "Linux"))
"aarch64"

julia> arch(Platform("amd64", "freebsd"))
"x86_64"
```
"""
arch(p::AbstractPlatform) = get(tags(p), "arch", nothing)

"""
    os(p::AbstractPlatform)

Get the operating system for the given `Platform` object as a `String`.

# Examples
```jldoctest
julia> os(Platform("armv7l", "Linux"))
"linux"

julia> os(Platform("aarch64", "macos"))
"macos"
```
"""
os(p::AbstractPlatform) = get(tags(p), "os", nothing)

# As a special helper, it's sometimes useful to know the current OS at compile-time
function os()
    if Sys.iswindows()
        return "windows"
    elseif Sys.isapple()
        return "macos"
    elseif Sys.isbsd()
        return "freebsd"
    else
        return "linux"
    end
end

"""
    libc(p::AbstractPlatform)

Get the libc for the given `Platform` object as a `String`.  Returns `nothing` on
platforms with no explicit `libc` choices (which is most platforms).

# Examples
```jldoctest
julia> libc(Platform("armv7l", "Linux"))
"glibc"

julia> libc(Platform("aarch64", "linux"; libc="musl"))
"musl"

julia> libc(Platform("i686", "Windows"))
```
"""
libc(p::AbstractPlatform) = get(tags(p), "libc", nothing)

"""
    call_abi(p::AbstractPlatform)

Get the call ABI for the given `Platform` object as a `String`.  Returns `nothing` on
platforms with no explicit `call_abi` choices (which is most platforms).

# Examples
```jldoctest
julia> call_abi(Platform("armv7l", "Linux"))
"eabihf"

julia> call_abi(Platform("x86_64", "macos"))
```
"""
call_abi(p::AbstractPlatform) = get(tags(p), "call_abi", nothing)

const platform_names = Dict(
    "linux" => "Linux",
    "macos" => "macOS",
    "windows" => "Windows",
    "freebsd" => "FreeBSD",
    nothing => "Unknown",
)

"""
    platform_name(p::AbstractPlatform)

Get the "platform name" of the given platform, returning e.g. "Linux" or "Windows".
"""
function platform_name(p::AbstractPlatform)
    return platform_names[os(p)]
end

function VNorNothing(d::Dict, key)
    v = get(d, key, nothing)
    if v === nothing
        return nothing
    end
    return VersionNumber(v)::VersionNumber
end

"""
    libgfortran_version(p::AbstractPlatform)

Get the libgfortran version dictated by this `Platform` object as a `VersionNumber`,
or `nothing` if no compatibility bound is imposed.
"""
libgfortran_version(p::AbstractPlatform) = VNorNothing(tags(p), "libgfortran_version")

"""
    libstdcxx_version(p::AbstractPlatform)

Get the libstdc++ version dictated by this `Platform` object, or `nothing` if no
compatibility bound is imposed.
"""
libstdcxx_version(p::AbstractPlatform) = VNorNothing(tags(p), "libstdcxx_version")

"""
    cxxstring_abi(p::AbstractPlatform)

Get the c++ string ABI dictated by this `Platform` object, or `nothing` if no ABI is imposed.
"""
cxxstring_abi(p::AbstractPlatform) = get(tags(p), "cxxstring_abi", nothing)

"""
    os_version(p::AbstractPlatform)

Get the OS version dictated by this `Platform` object, or `nothing` if no OS version is
imposed/no data is available.  This is most commonly used by MacOS and FreeBSD objects
where we have high platform SDK fragmentation, and features are available only on certain
platform versions.
"""
os_version(p::AbstractPlatform) = VNorNothing(tags(p), "os_version")

"""
    wordsize(p::AbstractPlatform)

Get the word size for the given `Platform` object.

# Examples
```jldoctest
julia> wordsize(Platform("armv7l", "linux"))
32

julia> wordsize(Platform("x86_64", "macos"))
64
```
"""
wordsize(p::AbstractPlatform) = (arch(p) ∈ ("i686", "armv6l", "armv7l")) ? 32 : 64

"""
    triplet(p::AbstractPlatform; exclude_tags::Vector{String})

Get the target triplet for the given `Platform` object as a `String`.

# Examples
```jldoctest
julia> triplet(Platform("x86_64", "MacOS"))
"x86_64-apple-darwin"

julia> triplet(Platform("i686", "Windows"))
"i686-w64-mingw32"

julia> triplet(Platform("armv7l", "Linux"; libgfortran_version="3"))
"armv7l-linux-gnueabihf-libgfortran3"
```
"""
function triplet(p::AbstractPlatform)
    str = string(
        arch(p)::Union{Symbol,String},
        os_str(p),
        libc_str(p),
        call_abi_str(p),
    )

    # Tack on optional compiler ABI flags
    if libgfortran_version(p) !== nothing
        str = string(str, "-libgfortran", libgfortran_version(p).major)
    end
    if cxxstring_abi(p) !== nothing
        str = string(str, "-", cxxstring_abi(p))
    end
    if libstdcxx_version(p) !== nothing
        str = string(str, "-libstdcxx", libstdcxx_version(p).patch)
    end

    # Tack on all extra tags
    for (tag, val) in tags(p)
        if tag ∈ ("os", "arch", "libc", "call_abi", "libgfortran_version", "libstdcxx_version", "cxxstring_abi", "os_version")
            continue
        end
        str = string(str, "-", tag, "+", val)
    end
    return str
end

function os_str(p::AbstractPlatform)
    if os(p) == "linux"
        return "-linux"
    elseif os(p) == "macos"
        osvn = os_version(p)
        if osvn !== nothing
            return "-apple-darwin$(osvn.major)"
        else
            return "-apple-darwin"
        end
    elseif os(p) == "windows"
        return "-w64-mingw32"
    elseif os(p) == "freebsd"
        osvn = os_version(p)
        if osvn !== nothing
            return "-unknown-freebsd$(osvn.major).$(osvn.minor)"
        else
            return "-unknown-freebsd"
        end
    else
        return "-unknown"
    end
end

# Helper functions for Linux and FreeBSD libc/abi mishmashes
function libc_str(p::AbstractPlatform)
    lc = libc(p)
    if lc === nothing
        return ""
    elseif lc === "glibc"
        return "-gnu"
    else
        return string("-", lc)
    end
end
function call_abi_str(p::AbstractPlatform)
    cabi = call_abi(p)
    cabi === nothing ? "" : string(cabi::Union{Symbol,String})
end

Sys.isapple(p::AbstractPlatform) = os(p) == "macos"
Sys.islinux(p::AbstractPlatform) = os(p) == "linux"
Sys.iswindows(p::AbstractPlatform) = os(p) == "windows"
Sys.isfreebsd(p::AbstractPlatform) = os(p) == "freebsd"
Sys.isbsd(p::AbstractPlatform) = os(p) ∈ ("freebsd", "macos")

const arch_mapping = Dict(
    "x86_64" => "(x86_|amd)64",
    "i686" => "i\\d86",
    "aarch64" => "(aarch64|arm64)",
    "armv7l" => "arm(v7l)?", # if we just see `arm-linux-gnueabihf`, we assume it's `armv7l`
    "armv6l" => "armv6l",
    "powerpc64le" => "p(ower)?pc64le",
)
# Keep this in sync with `CPUID.ISAs_by_family`
# These are the CPUID side of the microarchitectures targeted by GCC flags in BinaryBuilder.jl
const arch_march_isa_mapping = let
    function get_set(arch, name)
        all = CPUID.ISAs_by_family[arch]
        return all[findfirst(x -> x.first == name, all)].second
    end
    Dict(
        "i686" => [
            "pentium4" => get_set("i686", "pentium4"),
            "prescott" => get_set("i686", "prescott"),
        ],
        "x86_64" => [
            "x86_64" => get_set("x86_64", "x86_64"),
            "avx" => get_set("x86_64", "sandybridge"),
            "avx2" => get_set("x86_64", "haswell"),
            "avx512" => get_set("x86_64", "skylake_avx512"),
        ],
        "armv6l" => [
            "arm1176jzfs" => get_set("armv6l", "arm1176jzfs"),
        ],
        "armv7l" => [
            "armv7l" => get_set("armv7l", "armv7l"),
            "neonvfpv4" => get_set("armv7l", "armv7l+neon+vfpv4"),
        ],
        "aarch64" => [
            "armv8_0" => get_set("aarch64", "armv8.0-a"),
            "armv8_1" => get_set("aarch64", "armv8.1-a"),
            "armv8_2_crypto" => get_set("aarch64", "armv8.2-a+crypto"),
            "a64fx" => get_set("aarch64", "a64fx"),
            "apple_m1" => get_set("aarch64", "apple_m1"),
        ],
        "powerpc64le" => [
            "power8" => get_set("powerpc64le", "power8"),
        ]
    )
end
const os_mapping = Dict(
    "macos" => "-apple-darwin[\\d\\.]*",
    "freebsd" => "-(.*-)?freebsd[\\d\\.]*",
    "windows" => "-w64-mingw32",
    "linux" => "-(.*-)?linux",
)
const libc_mapping = Dict(
    "libc_nothing" => "",
    "glibc" => "-gnu",
    "musl" => "-musl",
)
const call_abi_mapping = Dict(
    "call_abi_nothing" => "",
    "eabihf" => "eabihf",
    "eabi" => "eabi",
)
const libgfortran_version_mapping = Dict(
    "libgfortran_nothing" => "",
    "libgfortran3" => "(-libgfortran3)|(-gcc4)", # support old-style `gccX` versioning
    "libgfortran4" => "(-libgfortran4)|(-gcc7)",
    "libgfortran5" => "(-libgfortran5)|(-gcc8)",
)
const cxxstring_abi_mapping = Dict(
    "cxxstring_nothing" => "",
    "cxx03" => "-cxx03",
    "cxx11" => "-cxx11",
)
const libstdcxx_version_mapping = Dict{String,String}(
    "libstdcxx_nothing" => "",
    "libstdcxx" => "-libstdcxx\\d+",
)

"""
    parse(::Type{Platform}, triplet::AbstractString)

Parses a string platform triplet back into a `Platform` object.
"""
function Base.parse(::Type{Platform}, triplet::String; validate_strict::Bool = false)
    # Helper function to collapse dictionary of mappings down into a regex of
    # named capture groups joined by "|" operators
    c(mapping) = string("(",join(["(?<$k>$v)" for (k, v) in mapping], "|"), ")")

    # We're going to build a mondo regex here to parse everything:
    triplet_regex = Regex(string(
        "^",
        # First, the core triplet; arch/os/libc/call_abi
        c(arch_mapping),
        c(os_mapping),
        c(libc_mapping),
        c(call_abi_mapping),
        # Next, optional things, like libgfortran/libstdcxx/cxxstring abi
        c(libgfortran_version_mapping),
        c(cxxstring_abi_mapping),
        c(libstdcxx_version_mapping),
        # Finally, the catch-all for extended tags
        "(?<tags>(?:-[^-]+\\+[^-]+)*)?",
        "\$",
    ))

    m = match(triplet_regex, triplet)
    if m !== nothing
        # Helper function to find the single named field within the giant regex
        # that is not `nothing` for each mapping we give it.
        get_field(m, mapping) = begin
            for k in keys(mapping)
                if m[k] !== nothing
                    # Convert our sentinel `nothing` values to actual `nothing`
                    if endswith(k, "_nothing")
                        return nothing
                    end
                    # Convert libgfortran/libstdcxx version numbers
                    if startswith(k, "libgfortran")
                        return VersionNumber(parse(Int,k[12:end]))
                    elseif startswith(k, "libstdcxx")
                        return VersionNumber(3, 4, parse(Int,m[k][11:end]))
                    else
                        return k
                    end
                end
            end
        end

        # Extract the information we're interested in:
        tags = Dict{String,Any}()
        arch = get_field(m, arch_mapping)
        os = get_field(m, os_mapping)
        tags["libc"] = get_field(m, libc_mapping)
        tags["call_abi"] = get_field(m, call_abi_mapping)
        tags["libgfortran_version"] = get_field(m, libgfortran_version_mapping)
        tags["libstdcxx_version"] = get_field(m, libstdcxx_version_mapping)
        tags["cxxstring_abi"] = get_field(m, cxxstring_abi_mapping)
        function split_tags(tagstr)
            tag_fields = split(tagstr, "-"; keepempty=false)
            if isempty(tag_fields)
                return Pair{String,String}[]
            end
            return map(v -> String(v[1]) => String(v[2]), split.(tag_fields, "+"))
        end
        merge!(tags, Dict(split_tags(m["tags"])))

        # Special parsing of os version number, if any exists
        function extract_os_version(os_name, pattern)
            m_osvn = match(pattern, m[os_name])
            if m_osvn !== nothing
                return VersionNumber(m_osvn.captures[1])
            end
            return nothing
        end
        os_version = nothing
        if os == "macos"
            os_version = extract_os_version("macos", r".*darwin([\d\.]+)")
        end
        if os == "freebsd"
            os_version = extract_os_version("freebsd", r".*freebsd([\d.]+)")
        end
        tags["os_version"] = os_version

        return Platform(arch, os, tags; validate_strict)
    end
    throw(ArgumentError("Platform `$(triplet)` is not an officially supported platform"))
end
Base.parse(::Type{Platform}, triplet::AbstractString; kwargs...) =
    parse(Platform, convert(String, triplet)::String; kwargs...)

function Base.tryparse(::Type{Platform}, triplet::AbstractString)
    try
        parse(Platform, triplet)
    catch e
        if isa(e, InterruptException)
            rethrow(e)
        end
        return nothing
    end
end

"""
    platform_dlext(p::AbstractPlatform = HostPlatform())

Return the dynamic library extension for the given platform, defaulting to the
currently running platform.  E.g. returns "so" for a Linux-based platform,
"dll" for a Windows-based platform, etc...
"""
function platform_dlext(p::AbstractPlatform = HostPlatform())
    if os(p) == "windows"
        return "dll"
    elseif os(p) == "macos"
        return "dylib"
    else
        return "so"
    end
end

"""
    parse_dl_name_version(path::String, platform::AbstractPlatform)

Given a path to a dynamic library, parse out what information we can
from the filename.  E.g. given something like "lib/libfoo.so.3.2",
this function returns `"libfoo", v"3.2"`.  If the path name is not a
valid dynamic library, this method throws an error.  If no soversion
can be extracted from the filename, as in "libbar.so" this method
returns `"libbar", nothing`.
"""
function parse_dl_name_version(path::String, os::String)
    # Use an extraction regex that matches the given OS
    local dlregex
    if os == "windows"
        # On Windows, libraries look like `libnettle-6.dll`
        dlregex = r"^(.*?)(?:-((?:[\.\d]+)*))?\.dll$"
    elseif os == "macos"
        # On OSX, libraries look like `libnettle.6.3.dylib`
        dlregex = r"^(.*?)((?:\.[\d]+)*)\.dylib$"
    else
        # On Linux and FreeBSD, libraries look like `libnettle.so.6.3.0`
        dlregex = r"^(.*?)\.so((?:\.[\d]+)*)$"
    end

    m = match(dlregex, basename(path))
    if m === nothing
        throw(ArgumentError("Invalid dynamic library path '$path'"))
    end

    # Extract name and version
    name = m.captures[1]
    version = m.captures[2]
    if version === nothing || isempty(version)
        version = nothing
    else
        version = VersionNumber(strip(version, '.'))
    end
    return name, version
end

# Adapter for `AbstractString`
function parse_dl_name_version(path::AbstractString, os::AbstractString)
    return parse_dl_name_version(string(path)::String, string(os)::String)
end

"""
    detect_libgfortran_version()

Inspects the current Julia process to determine the libgfortran version this Julia is
linked against (if any).
"""
function detect_libgfortran_version()
    libgfortran_paths = filter(x -> occursin("libgfortran", x), Libdl.dllist())
    if isempty(libgfortran_paths)
        # One day, I hope to not be linking against libgfortran in base Julia
        return nothing
    end
    libgfortran_path = first(libgfortran_paths)

    name, version = parse_dl_name_version(libgfortran_path, os())
    if version === nothing
        # Even though we complain about this, we allow it to continue in the hopes that
        # we shall march on to a BRIGHTER TOMORROW.  One in which we are not shackled
        # by the constraints of libgfortran compiler ABIs upon our precious programming
        # languages; one where the mistakes of yesterday are mere memories and not
        # continual maintenance burdens upon the children of the dawn; one where numeric
        # code may be cleanly implemented in a modern language and not bestowed onto the
        # next generation by grizzled ancients, documented only with a faded yellow
        # sticky note that bears a hastily-scribbled "good luck".
        @warn("Unable to determine libgfortran version from '$(libgfortran_path)'")
    end
    return version
end

"""
    detect_libstdcxx_version(max_minor_version::Int=30)

Inspects the currently running Julia process to find out what version of libstdc++
it is linked against (if any).  `max_minor_version` is the latest version in the
3.4 series of GLIBCXX where the search is performed.
"""
function detect_libstdcxx_version(max_minor_version::Int=30)
    libstdcxx_paths = filter(x -> occursin("libstdc++", x), Libdl.dllist())
    if isempty(libstdcxx_paths)
        # This can happen if we were built by clang, so we don't link against
        # libstdc++ at all.
        return nothing
    end

    # Brute-force our way through GLIBCXX_* symbols to discover which version we're linked against
    hdl = Libdl.dlopen(first(libstdcxx_paths))
    # Try all GLIBCXX versions down to GCC v4.8:
    # https://gcc.gnu.org/onlinedocs/libstdc++/manual/abi.html
    for minor_version in max_minor_version:-1:18
        if Libdl.dlsym(hdl, "GLIBCXX_3.4.$(minor_version)"; throw_error=false) !== nothing
            Libdl.dlclose(hdl)
            return VersionNumber("3.4.$(minor_version)")
        end
    end
    Libdl.dlclose(hdl)
    return nothing
end

"""
    detect_cxxstring_abi()

Inspects the currently running Julia process to see what version of the C++11 string ABI
it was compiled with (this is only relevant if compiled with `g++`; `clang` has no
incompatibilities yet, bless its heart).  In reality, this actually checks for symbols
within LLVM, but that is close enough for our purposes, as you can't mix configurations
between Julia and LLVM; they must match.
"""
function detect_cxxstring_abi()
    # First, if we're not linked against libstdc++, then early-exit because this doesn't matter.
    libstdcxx_paths = filter(x -> occursin("libstdc++", x), Libdl.dllist())
    if isempty(libstdcxx_paths)
        # We were probably built by `clang`; we don't link against `libstdc++`` at all.
        return nothing
    end

    function open_libllvm(f::Function)
        for lib_name in ("libLLVM-14jl", "libLLVM", "LLVM", "libLLVMSupport")
            hdl = Libdl.dlopen_e(lib_name)
            if hdl != C_NULL
                try
                    return f(hdl)
                finally
                    Libdl.dlclose(hdl)
                end
            end
        end
        error("Unable to open libLLVM!")
    end

    return open_libllvm() do hdl
        # Check for llvm::sys::getProcessTriple(), first without cxx11 tag:
        if Libdl.dlsym_e(hdl, "_ZN4llvm3sys16getProcessTripleEv") != C_NULL
            return "cxx03"
        elseif Libdl.dlsym_e(hdl, "_ZN4llvm3sys16getProcessTripleB5cxx11Ev") != C_NULL
            return "cxx11"
        else
            @warn("Unable to find llvm::sys::getProcessTriple() in libLLVM!")
            return nothing
        end
    end
end

"""
    host_triplet()

Build host triplet out of `Sys.MACHINE` and various introspective utilities that
detect compiler ABI values such as `libgfortran_version`, `libstdcxx_version` and
`cxxstring_abi`.  We do this without using any `Platform` tech as it must run before
we have much of that built.
"""
function host_triplet()
    str = Base.BUILD_TRIPLET

    if !occursin("-libgfortran", str)
        libgfortran_version = detect_libgfortran_version()
        if libgfortran_version !== nothing
            str = string(str, "-libgfortran", libgfortran_version.major)
        end
    end

    if !occursin("-cxx", str)
        cxxstring_abi = detect_cxxstring_abi()
        if cxxstring_abi !== nothing
            str = string(str, "-", cxxstring_abi)
        end
    end

    if !occursin("-libstdcxx", str)
        libstdcxx_version = detect_libstdcxx_version()
        if libstdcxx_version !== nothing
            str = string(str, "-libstdcxx", libstdcxx_version.patch)
        end
    end

    # Add on julia_version extended tag
    if !occursin("-julia_version+", str)
        str = string(str, "-julia_version+", VersionNumber(VERSION.major, VERSION.minor, VERSION.patch))
    end
    return str
end

"""
    HostPlatform()

Return the `Platform` object that corresponds to the current host system, with all
relevant comparison strategies set to host platform mode.  This is equivalent to:

    HostPlatform(parse(Platform, Base.BinaryPlatforms.host_triplet()))
"""
function HostPlatform()
    return HostPlatform(parse(Platform, host_triplet()))::Platform
end

"""
    platforms_match(a::AbstractPlatform, b::AbstractPlatform)

Return `true` if `a` and `b` are matching platforms, where matching is determined by
comparing all keys contained within the platform objects, and if both objects contain
entries for that key, they must match.  Comparison, by default, is performed using
the `==` operator, however this can be overridden on a key-by-key basis by adding
"comparison strategies" through `set_compare_strategy!(platform, key, func)`.

Note that as the comparison strategy is set on the `Platform` object, and not globally,
a custom comparison strategy is first looked for within the `a` object, then if none
is found, it is looked for in the `b` object.  Finally, if none is found in either, the
default of `==(ak, bk)` is used.  We throw an error if custom comparison strategies are
used on both `a` and `b` and they are not the same custom comparison.

The reserved tags `os_version` and `libstdcxx_version` use this mechanism to provide
bounded version constraints, where an artifact can specify that it was built using APIs
only available in macOS `v"10.11"` and later, or an artifact can state that it requires
a libstdc++ that is at least `v"3.4.22"`, etc...
"""
function platforms_match(a::AbstractPlatform, b::AbstractPlatform)
    for k in union(keys(tags(a)::Dict{String,String}), keys(tags(b)::Dict{String,String}))
        ak = get(tags(a), k, nothing)
        bk = get(tags(b), k, nothing)

        # Only continue if both `ak` and `bk` are not `nothing`
        if ak === nothing || bk === nothing
            continue
        end

        a_comp = get_compare_strategy(a, k)
        b_comp = get_compare_strategy(b, k)

        # Throw an error if `a` and `b` have both set non-default comparison strategies for `k`
        # and they're not the same strategy.
        if a_comp != compare_default && b_comp != compare_default && a_comp != b_comp
            throw(ArgumentError("Cannot compare Platform objects with two different non-default comparison strategies for the same key \"$(k)\""))
        end

        # Select the custom comparator, if we have one.
        comparator = a_comp
        if b_comp != compare_default
            comparator = b_comp
        end

        # Call the comparator, passing in which objects requested this comparison (one, the other, or both)
        # For some comparators this doesn't matter, but for non-symmetrical comparisons, it does.
        if !comparator(ak, bk, a_comp == comparator, b_comp == comparator)
            return false
        end
    end
    return true
end

function platforms_match(a::String, b::AbstractPlatform)
    return platforms_match(parse(Platform, a), b)
end
function platforms_match(a::AbstractPlatform, b::String)
    return platforms_match(a, parse(Platform, b))
end
platforms_match(a::String, b::String) = platforms_match(parse(Platform, a), parse(Platform, b))

# Adapters for AbstractString backedge avoidance
platforms_match(a::AbstractString, b::AbstractPlatform) = platforms_match(string(a)::String, b)
platforms_match(a::AbstractPlatform, b::AbstractString) = platforms_match(a, string(b)::String)
platforms_match(a::AbstractString, b::AbstractString) = platforms_match(string(a)::String, string(b)::String)


"""
    select_platform(download_info::Dict, platform::AbstractPlatform = HostPlatform())

Given a `download_info` dictionary mapping platforms to some value, choose
the value whose key best matches `platform`, returning `nothing` if no matches
can be found.

Platform attributes such as architecture, libc, calling ABI, etc... must all
match exactly, however attributes such as compiler ABI can have wildcards
within them such as `nothing` which matches any version of GCC.
"""
function select_platform(download_info::Dict, platform::AbstractPlatform = HostPlatform())
    ps = collect(filter(p -> platforms_match(p, platform), keys(download_info)))

    if isempty(ps)
        return nothing
    end

    # At this point, we may have multiple possibilities.  E.g. if, in the future,
    # Julia can be built without a direct dependency on libgfortran, we may match
    # multiple tarballs that vary only within their libgfortran ABI.  To narrow it
    # down, we just sort by triplet, then pick the last one.  This has the effect
    # of generally choosing the latest release (e.g. a `libgfortran5` tarball
    # rather than a `libgfortran3` tarball)
    p = last(sort(ps, by = p -> triplet(p)))
    return download_info[p]
end

# precompiles to reduce latency (see https://github.com/JuliaLang/julia/pull/43990#issuecomment-1025692379)
Dict{Platform,String}()[HostPlatform()] = ""
Platform("x86_64", "linux", Dict{String,Any}(); validate_strict=true)
Platform("x86_64", "linux", Dict{String,String}(); validate_strict=false)  # called this way from Artifacts.unpack_platform

end # module
