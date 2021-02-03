# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/nghttp2_jll.jl
baremodule nghttp2_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libnghttp2

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libnghttp2_handle = C_NULL
libnghttp2_path = ""

if Sys.iswindows()
    const libnghttp2 = "libnghttp2-14.dll"
elseif Sys.isapple()
    const libnghttp2 = "@rpath/libnghttp2.14.dylib"
else
    const libnghttp2 = "libnghttp2.so.14"
end

function __init__()
    global libnghttp2_handle = dlopen(libnghttp2)
    global libnghttp2_path = dlpath(libnghttp2_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = dirname(libnghttp2_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libnghttp2_path() = libnghttp2_path

end  # module nghttp2_jll
