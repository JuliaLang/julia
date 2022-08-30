# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/libblastrampoline_jll.jl

baremodule libblastrampoline_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libblastrampoline

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
libblastrampoline_handle = C_NULL
libblastrampoline_path = ""

const libblastrampoline = if Sys.iswindows()
    "libblastrampoline.dll"
elseif Sys.isapple()
    "@rpath/libblastrampoline.dylib"
else
    "libblastrampoline.so"
end

function __init__()
    global libblastrampoline_handle = dlopen(libblastrampoline)
    global libblastrampoline_path = dlpath(libblastrampoline_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libblastrampoline_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libblastrampoline_path() = libblastrampoline_path

end  # module libblastrampoline_jll
