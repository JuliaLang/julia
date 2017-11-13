export supported_platforms, platform_key, platform_dlext, valid_dl_path,
       arch, wordsize, triplet, Platform, Linux, MacOS, Windows

abstract type Platform end

struct Linux <: Platform
    arch::Symbol
    libc::Symbol

    function Linux(arch::Symbol, libc::Symbol=:glibc)
        if !in(arch, [:i686, :x86_64, :aarch64, :powerpc64le, :ppc64le, :armv7l])
            throw(ArgumentError("Unsupported architecture '$arch' for Linux"))
        end
        if libc !== :glibc && libc !== :musl
            throw(ArgumentError("Unsupported libc '$libc' for Linux"))
        end
        if arch === :ppc64le
            arch = :powerpc64le
        end
        new(arch, libc)
    end
end

struct MacOS <: Platform

    function MacOS(arch::Symbol)
        if arch !== :x86_64
            throw(ArgumentError("Unsupported architecture '$arch' for macOS"))
        end
        new()
    end
    MacOS() = new()
end

struct Windows <: Platform
    arch::Symbol

    function Windows(arch::Symbol)
        if arch !== :i686 && arch !== :x86_64
            throw(ArgumentError("Unsupported architecture '$arch' for Windows"))
        end
        new(arch)
    end
end

"""
    arch(platform)

Get the architecture for the given `Platform` object as a `Symbol`.

# Examples
```jldoctest
julia> arch(Linux(:aarch64))
:aarch64

julia> arch(MacOS())
:x86_64
```
"""
arch(p::Platform) = p.arch
arch(m::MacOS) = :x86_64

"""
    wordsize(platform)

Get the word size for the given `Platform` object.

# Examples
```jldoctest
julia> wordsize(Linux(:arm7vl))
32

julia> wordsize(MacOS())
64
```
"""
wordsize(l::Linux) = arch(l) === :i686 || arch(l) === :armv7l ? 32 : 64
wordsize(w::Windows) = arch(w) === :i686 ? 32 : 64
wordsize(m::MacOS) = 64

"""
    triplet(platform)

Get the target triplet for the given `Platform` object as a `String`.

# Examples
```jldoctest
julia> triplet(MacOS())
"x86_64-apple-darwin14"

julia> triplet(Windows(:i686))
"i686-w64-mingw32"

julia> triplet(Linux(:armv7l))
"arm-linux-gnueabihf"
```
"""
function triplet(l::Linux)
    c = l.libc === :glibc ? "gnu" : "musl" # Currently only glibc and musl are recognized
    if arch(l) === :armv7l
        string("arm-linux-", c, "eabihf")
    else
        string(arch(l), "-linux-", c)
    end
end
triplet(w::Windows) = string(arch(w), "-w64-mingw32")
triplet(m::MacOS) = "x86_64-apple-darwin14"

"""
    supported_platforms()

Return the list of supported platforms as an array of `Platform`s.
"""
function supported_platforms()
    return [
        Linux(:i686),
        Linux(:x86_64),
        Linux(:aarch64),
        Linux(:armv7l),
        Linux(:powerpc64le),
        MacOS(),
        Windows(:i686),
        Windows(:x86_64),
    ]
end

# Compat doesn't use the Base definitions for whatever terrible reason, so we'll overload
# both, ensuring the user gets our definitions regardless of whether they use Sys.is* or
# Compat.Sys.is*.
if isdefined(Base.Sys, :isapple)
    Base.Sys.isapple(p::Platform) = p isa MacOS
    Base.Sys.islinux(p::Platform) = p isa Linux
    Base.Sys.iswindows(p::Platform) = p isa Windows
end
Compat.Sys.isapple(p::Platform) = p isa MacOS
Compat.Sys.islinux(p::Platform) = p isa Linux
Compat.Sys.iswindows(p::Platform) = p isa Windows

"""
    platform_key(machine::AbstractString = Sys.MACHINE)

Returns the platform key for the current platform, or any other though the
the use of the `machine` parameter.
"""
function platform_key(machine::AbstractString = Sys.MACHINE)
    # First, off, if `machine` is literally one of the values of our mapping
    # above, just return the relevant key
    for key in supported_platforms()
        if machine == triplet(key)
            return key
        end
    end

    # Otherwise, try to parse the machine into one of our keys
    if startswith(machine, "x86_64-apple-darwin")
        return MacOS()
    end
    if ismatch(r"x86_64-(pc-)?(unknown-)?linux-gnu", machine)
        return Linux(:x86_64)
    end
    if ismatch(r"i\d86-(pc-)?(unknown-)?linux-gnu", machine)
        return Linux(:i686)
    end
    if ismatch(r"aarch64-(pc-)?(unknown-)?linux-gnu", machine)
        return Linux(:aarch64)
    end
    if ismatch(r"armv7l-(pc-)?(unknown-)?linux-gnueabihf", machine)
        return Linux(:armv7l)
    end
    if ismatch(r"powerpc64le-(pc-)?(unknown-)?linux-gnu", machine)
        return Linux(:powerpc64le)
    end

    throw(ArgumentError("Platform `$(machine)` is not an officially supported platform"))
end


"""
    platform_dlext(platform::Platform = platform_key())

Return the dynamic library extension for the given platform, defaulting to the
currently running platform.  E.g. returns "so" for a Linux-based platform,
"dll" for a Windows-based platform, etc...
"""
platform_dlext(l::Linux) = "so"
platform_dlext(m::MacOS) = "dylib"
platform_dlext(w::Windows) = "dll"
platform_dlext() = platform_dlext(platform_key())

"""
    valid_dl_path(path::AbstractString, platform::Platform)

Return `true` if the given `path` ends in a valid dynamic library filename.
E.g. returns `true` for a path like `"usr/lib/libfoo.so.3.5"`, but returns
`false` for a path like `"libbar.so.f.a"`.
"""
function valid_dl_path(path::AbstractString, platform::Platform)
    const dlext_regexes = Dict(
        # On Linux, libraries look like `libnettle.so.6.3.0`
        "so" => r"^(.*).so(\.[\d]+){0,3}$",
        # On OSX, libraries look like `libnettle.6.3.dylib`
        "dylib" => r"^(.*).dylib$",
        # On Windows, libraries look like `libnettle-6.dylib`
        "dll" => r"^(.*).dll$"
    )

    # Given a platform, find the dlext regex that matches it
    dlregex = dlext_regexes[platform_dlext(platform)]

    # Return whether or not that regex matches the basename of the given path
    return ismatch(dlregex, basename(path))
end
