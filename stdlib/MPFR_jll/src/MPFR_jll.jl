# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/MPFR_jll.jl
baremodule MPFR_jll
using Base, Libdl, GMP_jll

export libmpfr

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libmpfr_path::String = ""

if Sys.iswindows()
    const _libmpfr_path = BundledLazyLibraryPath("libmpfr-6.dll")
elseif Sys.isapple()
    const _libmpfr_path = BundledLazyLibraryPath("libmpfr.6.dylib")
else
    const _libmpfr_path = BundledLazyLibraryPath("libmpfr.so.6")
end

_libmpfr_dependencies = LazyLibrary[libgmp]

const libmpfr = LazyLibrary(_libmpfr_path, dependencies=_libmpfr_dependencies)

function eager_mode()
    dlopen(libmpfr)
end
is_available() = true

function __init__()
    global libmpfr_path = string(_libmpfr_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libmpfr_path)
    push!(LIBPATH_list, LIBPATH[])
end

end  # module MPFR_jll
