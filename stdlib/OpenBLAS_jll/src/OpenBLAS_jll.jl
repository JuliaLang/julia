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
artifact_dir::String = ""
libopenblas_handle::Ptr{Cvoid} = C_NULL
libopenblas_path::String = ""

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

    # Ensure that OpenBLAS does not grab a huge amount of memory at first,
    # since it instantly allocates scratch buffer space for the number of
    # threads it thinks it needs to use.
    # X-ref: https://github.com/xianyi/OpenBLAS/blob/c43ec53bdd00d9423fc609d7b7ecb35e7bf41b85/README.md#setting-the-number-of-threads-using-environment-variables
    # X-ref: https://github.com/JuliaLang/julia/issues/45434
    if !haskey(ENV, "OPENBLAS_NUM_THREADS") &&
       !haskey(ENV, "GOTO_NUM_THREADS") &&
       !haskey(ENV, "OMP_NUM_THREADS")
        # We set this to `1` here, and then LinearAlgebra will update
        # to the true value in its `__init__()` function.
        ENV["OPENBLAS_DEFAULT_NUM_THREADS"] = "1"
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
