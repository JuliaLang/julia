# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Base.BinaryPlatforms.PkgCompat is the old BinaryPlatforms API exposed by
Pkg.jl as Pkg.BinaryPlatforms
"""
module PkgCompat

using Base.BinaryPlatforms
export platform_key_abi, platform_dlext, valid_dl_path, arch, libc,
       libgfortran_version, libstdcxx_version, cxxstring_abi, parse_dl_name_version,
       detect_libgfortran_version, detect_libstdcxx_version, detect_cxxstring_abi,
       call_abi, wordsize, triplet, select_platform, platforms_match,
       CompilerABI, Platform, UnknownPlatform, Linux, MacOS, Windows, FreeBSD

import Base.BinaryPlatforms: libgfortran_version, libstdcxx_version, platform_name,
                             wordsize, platform_dlext, tags, arch, libc, call_abi,
                             cxxstring_abi

struct UnknownPlatform <: AbstractPlatform
    UnknownPlatform(args...; kwargs...) = new()
end
tags(::UnknownPlatform) = Dict{String,String}("os"=>"unknown")


struct CompilerABI
    libgfortran_version::Union{Nothing,VersionNumber}
    libstdcxx_version::Union{Nothing,VersionNumber}
    cxxstring_abi::Union{Nothing,Symbol}

    function CompilerABI(;libgfortran_version::Union{Nothing, VersionNumber} = nothing,
                         libstdcxx_version::Union{Nothing, VersionNumber} = nothing,
                         cxxstring_abi::Union{Nothing, Symbol} = nothing)
        return new(libgfortran_version, libstdcxx_version, cxxstring_abi)
    end
end

# Easy replacement constructor
function CompilerABI(cabi::CompilerABI; libgfortran_version=nothing,
                                        libstdcxx_version=nothing,
                                        cxxstring_abi=nothing)
    return CompilerABI(;
        libgfortran_version=something(libgfortran_version, Some(cabi.libgfortran_version)),
        libstdcxx_version=something(libstdcxx_version, Some(cabi.libstdcxx_version)),
        cxxstring_abi=something(cxxstring_abi, Some(cabi.cxxstring_abi)),
    )
end

libgfortran_version(cabi::CompilerABI) = cabi.libgfortran_version
libstdcxx_version(cabi::CompilerABI) = cabi.libstdcxx_version
cxxstring_abi(cabi::CompilerABI) = cabi.cxxstring_abi

for T in (:Linux, :Windows, :MacOS, :FreeBSD)
    @eval begin
        struct $(T) <: AbstractPlatform
            p::Platform
            function $(T)(arch::Symbol; compiler_abi=nothing, kwargs...)
                if compiler_abi !== nothing
                    kwargs = (; kwargs...,
                        :libgfortran_version => libgfortran_version(compiler_abi),
                        :libstdcxx_version => libstdcxx_version(compiler_abi),
                        :cxxstring_abi => cxxstring_abi(compiler_abi)
                    )
                end
                return new(Platform(string(arch), $(string(T)); kwargs..., validate_strict=true))
            end
        end
    end
end

const PlatformUnion = Union{Linux,MacOS,Windows,FreeBSD}

# First, methods we need to coerce to Symbol for backwards-compatibility
for f in (:arch, :libc, :call_abi, :cxxstring_abi)
    @eval begin
        function $(f)(p::PlatformUnion)
            str = $(f)(p.p)
            if str === nothing
                return nothing
            end
            return Symbol(str)
        end
    end
end

# Next, things we don't need to coerce
for f in (:libgfortran_version, :libstdcxx_version, :platform_name, :wordsize, :platform_dlext, :tags, :triplet)
    @eval begin
        $(f)(p::PlatformUnion) = $(f)(p.p)
    end
end

# Finally, add equality testing between these wrapper types and other AbstractPlatforms
@eval begin
    Base.:(==)(a::PlatformUnion, b::AbstractPlatform) = b == a.p
end

# Add one-off functions
MacOS(; kwargs...) = MacOS(:x86_64; kwargs...)
FreeBSD(; kwargs...) = FreeBSD(:x86_64; kwargs...)

function triplet(p::AbstractPlatform)
    # We are going to sub off to `Base.BinaryPlatforms.triplet()` here,
    # with the important exception that we override `os_version` to better
    # mimic the old behavior of `triplet()`
    if Sys.isfreebsd(p)
        p = deepcopy(p)
        p["os_version"] = "11.1.0"
    elseif Sys.isapple(p)
        p = deepcopy(p)
        p["os_version"] = "14.0.0"
    end
    return Base.BinaryPlatforms.triplet(p)
end

"""
    platform_key_abi(machine::AbstractString)

Returns the platform key for the current platform, or any other though the
the use of the `machine` parameter.

This method is deprecated, import `Base.BinaryPlatforms` and use either `HostPlatform()`
to get the current host platform, or `parse(Base.BinaryPlatforms.Platform, triplet)`
to parse the triplet for some other platform instead.
"""
platform_key_abi() = HostPlatform()
platform_key_abi(triplet::AbstractString) = parse(Platform, triplet)

"""
    valid_dl_path(path::AbstractString, platform::Platform)

Return `true` if the given `path` ends in a valid dynamic library filename.
E.g. returns `true` for a path like `"usr/lib/libfoo.so.3.5"`, but returns
`false` for a path like `"libbar.so.f.a"`.

This method is deprecated and will be removed in Julia 2.0.
"""
function valid_dl_path(path::AbstractString, platform::AbstractPlatform)
    try
        parse_dl_name_version(path, string(os(platform))::String)
        return true
    catch e
        if isa(e, ArgumentError)
            return false
        end
        rethrow(e)
    end
end

end # module BinaryPlatforms
