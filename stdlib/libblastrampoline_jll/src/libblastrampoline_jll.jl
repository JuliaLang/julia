# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/libblastrampoline_jll.jl

baremodule libblastrampoline_jll
using Base, Libdl

const PATH_list = String[]
const LIBPATH_list = String[]

export libblastrampoline

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir::String = ""
libblastrampoline_handle::Ptr{Cvoid} = C_NULL
libblastrampoline_path::String = ""

# NOTE: keep in sync with `Base.libblas_name` and `Base.liblapack_name`.
const libblastrampoline = if Sys.iswindows()
    "libblastrampoline-5.dll"
elseif Sys.isapple()
    "@rpath/libblastrampoline.5.dylib"
else
    "libblastrampoline.so.5"
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
