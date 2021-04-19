# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibUV_jll.jl

baremodule LibUV_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libuv

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
libuv_handle = C_NULL
libuv_path = ""

if Sys.iswindows()
    const libuv = "libuv-2.dll"
elseif Sys.isapple()
    const libuv = "@rpath/libuv.2.dylib"
else
    const libuv = "libuv.so.2"
end

function __init__()
    global libuv_handle = dlopen(libuv)
    global libuv_path = dlpath(libuv_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libuv_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libuv_path() = libuv_path

end  # module LibUV_jll
