# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/GMP_jll.jl
baremodule GMP_jll
using Base, Libdl
if !Sys.isapple()
    using CompilerSupportLibraries_jll
end

export libgmp, libgmpxx

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libgmp_path::String = ""
const libgmp = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libgmp-10.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libgmp.10.dylib")
    else
        BundledLazyLibraryPath("libgmp.so.10")
    end
)

libgmpxx_path::String = ""
const libgmpxx = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libgmpxx-4.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libgmpxx.4.dylib")
    else
        BundledLazyLibraryPath("libgmpxx.so.4")
    end,
    dependencies = if Sys.isfreebsd()
        LazyLibrary[libgmp, libgcc_s]
    elseif Sys.isapple()
        LazyLibrary[libgmp]
    else
        LazyLibrary[libgmp, libstdcxx, libgcc_s]
    end
)

function eager_mode()
    @static if @isdefined CompilerSupportLibraries_jll
        CompilerSupportLibraries_jll.eager_mode()
    end
    dlopen(libgmp)
    dlopen(libgmpxx)
end
is_available() = true

function __init__()
    global libgmp_path = string(libgmp.path)
    global libgmpxx_path = string(libgmpxx.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libgmp_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module GMP_jll
