# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/Zlib_jll.jl
baremodule Zlib_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libz

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libz_handle = C_NULL
libz_path = ""

if Sys.iswindows()
    const libz = "libz.dll"
elseif Sys.isapple()
    const libz = "@rpath/libz.1.dylib"
else
    const libz = "libz.so.1"
end

function __init__()
    global libz_handle = dlopen(libz)
    global libz_path = dlpath(libz_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = dirname(libz_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libz_path() = libz_path

end  # module Zlib_jll
