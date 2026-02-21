# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/OpenLibm_jll.jl
baremodule OpenLibm_jll
using Base, Libdl
if Sys.iswindows()
    using CompilerSupportLibraries_jll
end

export libopenlibm

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libopenlibm_path::String = ""
const libopenlibm = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libopenlibm.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libopenlibm.4.dylib")
    else
        BundledLazyLibraryPath("libopenlibm.so.4")
    end,
    dependencies = if Sys.iswindows()
        LazyLibrary[libgcc_s]
    else
        LazyLibrary[]
    end
)

function eager_mode()
    dlopen(libopenlibm)
    @static if @isdefined CompilerSupportLibraries_jll
        CompilerSupportLibraries_jll.eager_mode()
    end
end
is_available() = true

function __init__()
    global libopenlibm_path = string(libopenlibm.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libopenlibm_path)
    push!(LIBPATH_list, LIBPATH[])
end

end  # module OpenLibm_jll
