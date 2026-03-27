# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LMDB_jll.jl
baremodule LMDB_jll
using Base, Libdl

export liblmdb

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

liblmdb_path::String = ""
const liblmdb = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("liblmdb.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("liblmdb.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("liblmdb.so")
    else
        error("LMDB_jll: Library 'liblmdb' is not available for $(Sys.KERNEL)")
    end
)

function eager_mode()
    dlopen(liblmdb)
end
is_available() = true

function __init__()
    global liblmdb_path = string(liblmdb.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(liblmdb_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module LMDB_jll
