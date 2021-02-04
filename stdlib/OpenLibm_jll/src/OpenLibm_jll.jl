# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/OpenLibm_jll.jl
baremodule OpenLibm_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libopenlibm

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libopenlibm_handle = C_NULL
libopenlibm_path = ""

if Sys.iswindows()
    const libopenlibm = "libopenlibm.dll"
elseif Sys.isapple()
    const libopenlibm = "@rpath/libopenlibm.3.dylib"
else
    const libopenlibm = "libopenlibm.so.3"
end

function __init__()
    global libopenlibm_handle = dlopen(libopenlibm)
    global libopenlibm_path = dlpath(libopenlibm_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = dirname(libopenlibm_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libopenlibm_path() = libopenlibm_path

end  # module OpenLibm_jll
