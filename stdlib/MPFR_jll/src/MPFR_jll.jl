# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/MPFR_jll.jl
baremodule MPFR_jll
using Base, Libdl, GMP_jll
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libmpfr

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libmpfr_handle = C_NULL
libmpfr_path = ""

if Sys.iswindows()
    const libmpfr = "libmpfr-6.dll"
elseif Sys.isapple()
    const libmpfr = "@rpath/libmpfr.6.dylib"
else
    const libmpfr = "libmpfr.so.6"
end

function __init__()
    global libmpfr_handle = dlopen(libmpfr)
    global libmpfr_path = dlpath(libmpfr_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = dirname(libmpfr_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libmpfr_path() = libmpfr_path

end  # module MPFR_jll
