# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/dSFMT_jll.jl

baremodule dSFMT_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libdSFMT

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libdSFMT_handle = C_NULL
libdSFMT_path = ""

if Sys.iswindows()
    const libdSFMT = "libdSFMT.dll"
elseif Sys.isapple()
    const libdSFMT = "@rpath/libdSFMT.dylib"
else
    const libdSFMT = "libdSFMT.so"
end

function __init__()
    global libdSFMT_handle = dlopen(libdSFMT)
    global libdSFMT_path = dlpath(libdSFMT_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = dirname(libdSFMT_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libdSFMT_path() = libdSFMT_path

end  # module dSFMT_jll
