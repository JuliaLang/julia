# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/MPFR_jll.jl
baremodule MPFR_jll
using Base, Libdl, GMP_jll
if Sys.iswindows()
    using CompilerSupportLibraries_jll
end

export libmpfr

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libmpfr_path::String = ""
const libmpfr = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libmpfr-6.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libmpfr.6.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libmpfr.so.6")
    else
        error("MPFR_jll: Library 'libmpfr' is not available for $(Sys.KERNEL)")
    end,
    dependencies = if Sys.iswindows()
        LazyLibrary[libgmp, libgcc_s]
    else
        LazyLibrary[libgmp]
    end
)

function eager_mode()
    GMP_jll.eager_mode()
    @static if @isdefined CompilerSupportLibraries_jll
        CompilerSupportLibraries_jll.eager_mode()
    end
    dlopen(libmpfr)
end
is_available() = true

function __init__()
    global libmpfr_path = string(libmpfr.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libmpfr_path)
    push!(LIBPATH_list, LIBPATH[])
end

end  # module MPFR_jll
