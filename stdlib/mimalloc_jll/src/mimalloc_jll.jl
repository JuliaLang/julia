# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/mimalloc_jll.jl

baremodule mimalloc_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export mimalloc

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
mimalloc_handle = C_NULL
mimalloc_path = ""

if Sys.iswindows()
    const mimalloc = "libmimalloc.dll"
elseif Sys.isapple()
    const mimalloc = "@rpath/libmimalloc.2.0.dylib"
else
    const mimalloc = "libmimalloc.so.2.0"
end

function __init__()
    global mimalloc_handle = dlopen(mimalloc)
    global mimalloc_path = dlpath(mimalloc_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(mimalloc_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_mimalloc_path() = mimalloc_path

end  # module mimalloc_jll
