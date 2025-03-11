# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/Zlib_jll.jl
baremodule Zlib_jll
using Base, Libdl

export libz

# These get calculated in __init__()
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libz_path::String = ""

if Sys.iswindows()
    const _libz_path = BundledLazyLibraryPath("bin\\libz.dll")
elseif Sys.isapple()
    const _libz_path = BundledLazyLibraryPath("lib/libz.1.dylib")
else
    const _libz_path = BundledLazyLibraryPath("lib/libz.so.1")
end
const libz = LazyLibrary(_libz_path)

function eager_mode()
    dlopen(libz)
end
is_available() = true

function __init__()
    global libz_path = string(_libz_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libz_path)
    push!(LIBPATH_list, LIBPATH[])
end

end  # module Zlib_jll
