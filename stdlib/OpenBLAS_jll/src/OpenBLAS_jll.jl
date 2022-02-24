# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/OpenBLAS_jll.jl
baremodule OpenBLAS_jll
using Base, Libdl, CompilerSupportLibraries_jll, Base.BinaryPlatforms
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libopenblas

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
libopenblas_handle = C_NULL
libopenblas_path = ""

if Base.USE_BLAS64
    const libsuffix = "64_"
else
    const libsuffix = ""
end

if Sys.iswindows()
    const libopenblas = "libopenblas$(libsuffix).dll"
elseif Sys.isapple()
    const libopenblas = "@rpath/libopenblas$(libsuffix).dylib"
else
    const libopenblas = "libopenblas$(libsuffix).so"
end

function __init__()
    # make sure OpenBLAS does not set CPU affinity (#1070, #9639)
    if !haskey(ENV, "OPENBLAS_MAIN_FREE")
        ENV["OPENBLAS_MAIN_FREE"] = "1"
    end

    global libopenblas_handle = dlopen(libopenblas)
    global libopenblas_path = dlpath(libopenblas_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libopenblas_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libopenblas_path() = libopenblas_path

end  # module OpenBLAS_jll
