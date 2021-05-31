# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/GMP_jll.jl
baremodule GMP_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libgmp, libgmpxx

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
libgmp_handle = C_NULL
libgmp_path = ""
libgmpxx_handle = C_NULL
libgmpxx_path = ""

if Sys.iswindows()
    const libgmp = "libgmp-10.dll"
    const libgmpxx = "libgmpxx-4.dll"
elseif Sys.isapple()
    const libgmp = "@rpath/libgmp.10.dylib"
    const libgmpxx = "@rpath/libgmpxx.4.dylib"
else
    const libgmp = "libgmp.so.10"
    const libgmpxx = "libgmpxx.so.4"
end

function __init__()
    global libgmp_handle = dlopen(libgmp)
    global libgmp_path = dlpath(libgmp_handle)
    global libgmpxx_handle = dlopen(libgmpxx)
    global libgmpxx_path = dlpath(libgmpxx_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libgmp_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libgmp_path() = libgmp_path
get_libgmpxx_path() = libgmpxx_path

end  # module GMP_jll
