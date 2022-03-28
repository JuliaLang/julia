# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibCURL_jll.jl

baremodule LibCURL_jll
using Base, Libdl, nghttp2_jll
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libcurl

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
libcurl_handle = C_NULL
libcurl_path = ""

if Sys.iswindows()
    const libcurl = "libcurl-4.dll"
elseif Sys.isapple()
    const libcurl = "@rpath/libcurl.4.dylib"
else
    const libcurl = "libcurl.so"
end

function __init__()
    global libcurl_handle = dlopen(libcurl)
    global libcurl_path = dlpath(libcurl_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libcurl_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libcurl_path() = libcurl_path

end  # module LibCURL_jll
