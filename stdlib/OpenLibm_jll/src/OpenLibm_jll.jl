# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/OpenLibm_jll.jl
baremodule OpenLibm_jll
using Base, Libdl
using CompilerSupportLibraries_jll

export libopenlibm

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libopenlibm_path::String = ""

if Sys.iswindows()
    const _libopenlibm_path = BundledLazyLibraryPath("libopenlibm.dll")
elseif Sys.isapple()
    const _libopenlibm_path = BundledLazyLibraryPath("libopenlibm.4.dylib")
else
    const _libopenlibm_path = BundledLazyLibraryPath("libopenlibm.so.4")
end
if Sys.iswindows()
    const _libopenlibm_dependencies = LazyLibrary[CompilerSupportLibraries_jll.libgcc_s]
else
    const _libopenlibm_dependencies = LazyLibrary[]
end
const libopenlibm = LazyLibrary(_libopenlibm_path, dependencies=_libopenlibm_dependencies)

function eager_mode()
    dlopen(libopenlibm)
    @static if Sys.iswindows()
        CompilerSupportLibraries_jll.eager_mode()
    end
end
is_available() = true

function __init__()
    global libopenlibm_path = string(_libopenlibm_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libopenlibm_path)
    push!(LIBPATH_list, LIBPATH[])
end

end  # module OpenLibm_jll
