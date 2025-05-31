# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/GMP_jll.jl
baremodule GMP_jll
using Base, Libdl, CompilerSupportLibraries_jll

export libgmp, libgmpxx

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libgmp_path::String = ""
libgmpxx_path::String = ""

if Sys.iswindows()
    const _libgmp_path = BundledLazyLibraryPath("libgmp-10.dll")
    const _libgmpxx_path = BundledLazyLibraryPath("libgmpxx-4.dll")
elseif Sys.isapple()
    const _libgmp_path = BundledLazyLibraryPath("libgmp.10.dylib")
    const _libgmpxx_path = BundledLazyLibraryPath("libgmpxx.4.dylib")
else
    const _libgmp_path = BundledLazyLibraryPath("libgmp.so.10")
    const _libgmpxx_path = BundledLazyLibraryPath("libgmpxx.so.4")
end

const libgmp = LazyLibrary(_libgmp_path)

if Sys.isfreebsd()
    _libgmpxx_dependencies = LazyLibrary[libgmp, libgcc_s]
elseif Sys.isapple()
    _libgmpxx_dependencies = LazyLibrary[libgmp]
else
    _libgmpxx_dependencies = LazyLibrary[libgmp, libstdcxx, libgcc_s]
end
const libgmpxx = LazyLibrary(
    _libgmpxx_path,
    dependencies=_libgmpxx_dependencies,
)

function eager_mode()
    CompilerSupportLibraries_jll.eager_mode()
    dlopen(libgmp)
    dlopen(libgmpxx)
end
is_available() = true

function __init__()
    global libgmp_path = string(_libgmp_path)
    global libgmpxx_path = string(_libgmpxx_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libgmp_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module GMP_jll
