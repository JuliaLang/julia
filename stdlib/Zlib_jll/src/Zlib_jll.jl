# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/Zlib_jll.jl
baremodule Zlib_jll
using Base, Libdl

export libz

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libz_path::String = ""
const libz = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libz.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libz.1.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libz.so.1")
    else
        error("Zlib_jll: Library 'libz' is not available for $(Sys.KERNEL)")
    end
)

function eager_mode()
    dlopen(libz)
end
is_available() = true

function __init__()
    global libz_path = string(libz.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libz_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module Zlib_jll
