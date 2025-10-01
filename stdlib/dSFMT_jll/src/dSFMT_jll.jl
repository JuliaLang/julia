# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/dSFMT_jll.jl

baremodule dSFMT_jll
using Base, Libdl

export libdSFMT

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libdSFMT_path::String = ""
const libdSFMT = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libdSFMT.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libdSFMT.dylib")
    else
        BundledLazyLibraryPath("libdSFMT.so")
    end
)

function eager_mode()
    dlopen(libdSFMT)
end
is_available() = true

function __init__()
    global libdSFMT_path = string(libdSFMT.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libdSFMT_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module dSFMT_jll
