# This file serves as a way to provide access to libraries that Base needs
# that are usually included as artifacts.  We can't use Artifacts this early
# becuase we haven't bootstrapped far enough, so this file contains "Base"
# JLLs that are manually adapted to load things from hardcoded paths so that
# we can bootstrap, but once GMP_jll, MPFR_jll, etc... are loaded, we replace
# the definitions here with the `LazyLibrary` objects defined there, as they
# may be overloaded by Preferences and refer to a different library than we
# would have used during bootstrap.
module JLLAdapters

# We're early enough that we don't have access to `Sys.iswindows()`, etc...
const UNAME = ccall(:jl_get_UNAME, Any, ())::Symbol
const early_pathsep = (UNAME === :Windows || UNAME === :NT) ? "\\" : "/"

function early_joinpath(pieces...)
    result = pieces[1]
    for piece in pieces[2:end]
        result = string(result, early_pathsep, piece)
    end
    return result
end

"""
    LazyLibraryPath

Helper type for lazily constructed library paths for use with `LazyLibrary`.
Arguments are passed to `joinpath()`.  Arguments must be able to have
`string()` called on them.

```
libfoo = LazyLibrary(LazyLibraryPath(prefix, "lib/libfoo.so.1.2.3"))
```
"""
struct LazyLibraryPath
    pieces::Vector
    LazyLibraryPath(pieces::Vector) = new(pieces)
end
LazyLibraryPath(args...) = LazyLibraryPath(collect(args))
Base.string(llp::LazyLibraryPath) = early_joinpath([string(p) for p in llp.pieces]...)
Base.cconvert(::Type{Cstring}, llp::LazyLibraryPath) = Base.cconvert(Cstring, string(llp))
# Define `print` so that we can wrap this in a `LazyString`
Base.print(io::IO, llp::LazyLibraryPath) = print(io, string(llp))

# Helper to get `Sys.BINDIR` at runtime
struct SysBindirGetter; end
Base.string(::SysBindirGetter) = string(ccall(:jl_get_julia_bindir, Any, ())::String, early_pathsep, "..")

"""
    BundledLazyLibraryPath

Helper type for lazily constructed library paths that are stored within the
bundled Julia distribution, primarily for use by Base modules.

```
libfoo = LazyLibrary(BundledLazyLibraryPath("lib/libfoo.so.1.2.3"))
```
"""
BundledLazyLibraryPath(subpath) = LazyLibraryPath(SysBindirGetter(), subpath)

# PCRE
if (UNAME === :Windows || UNAME === :NT)
    const libpcre2_8_name = "bin/libpcre2-8-0.dll"
elseif (UNAME === :Apple || UNAME === :Darwin)
    const libpcre2_8_name = "lib/libpcre2-8.0.dylib"
else
    const libpcre2_8_name = "lib/libpcre2-8.so.0"
end

const libpcre2_8 = Ref{Any}(BundledLazyLibraryPath(libpcre2_8_name))
function get_libpcre2_8()
    if isa(libpcre2_8[], LazyLibraryPath)
        return string(libpcre2_8[])
    end
    return libpcre2_8[]
end

# GMP
if (UNAME === :Windows || UNAME === :NT)
    const libgmp_name = "bin/libgmp-10.dll"
elseif (UNAME === :Apple || UNAME === :Darwin)
    const libgmp_name = "lib/libgmp.10.dylib"
else
    const libgmp_name = "lib/libgmp.so.10"
end
const libgmp = Ref{Any}(BundledLazyLibraryPath(libgmp_name))
function get_libgmp()
    if isa(libgmp[], LazyLibraryPath)
        return string(libgmp[])
    end
    return libgmp[]
end


# MPFR
if (UNAME === :Windows || UNAME === :NT)
    const libmpfr_name = "bin/libmpfr-6.dll"
elseif (UNAME === :Apple || UNAME === :Darwin)
    const libmpfr_name = "lib/libmpfr.6.dylib"
else
    const libmpfr_name = "lib/libmpfr.so.6"
end
const libmpfr = Ref{Any}(BundledLazyLibraryPath(libmpfr_name))
function get_libmpfr()
    # Work around early bootstrap problems where we need to load `libgmp`
    # when `libmpfr` is loaded.  This only works if we're far enough along
    # in bootstrap to be able to call `dlopen()`!  Later, `libmpfr[]`
    # is going to return a `LazyLibrary` that will have a dependency on
    # `libgmp[]`.
    if isa(libmpfr[], LazyLibraryPath)
        Base.Libc.Libdl.dlopen(get_libgmp())
        return string(libmpfr[])
    end
    return libmpfr[]
end


end # module JLLAdapters
