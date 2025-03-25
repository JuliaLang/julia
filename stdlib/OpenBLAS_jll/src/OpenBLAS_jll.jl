# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/OpenBLAS_jll.jl
baremodule OpenBLAS_jll
using Base, Libdl, Base.BinaryPlatforms
using CompilerSupportLibraries_jll

export libopenblas

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libopenblas_path::String = ""

if Base.USE_BLAS64
    const libsuffix = "64_"
else
    const libsuffix = ""
end

if Sys.iswindows()
    const _libopenblas_path = BundledLazyLibraryPath(string("libopenblas", libsuffix, ".dll"))
elseif Sys.isapple()
    const _libopenblas_path = BundledLazyLibraryPath(string("libopenblas", libsuffix, ".dylib"))
else
    const _libopenblas_path = BundledLazyLibraryPath(string("libopenblas", libsuffix, ".so"))
end
const libopenblas = LazyLibrary(_libopenblas_path, dependencies=[libgfortran])

# Conform to LazyJLLWrappers API
function eager_mode()
    CompilerSupportLibraries_jll.eager_mode()
    dlopen(libopenblas_path)
end
is_available() = true

function __init__()
    global libopenblas_path = string(_libopenblas_path)
    # make sure OpenBLAS does not set CPU affinity (#1070, #9639)
    if !(haskey(ENV, "OPENBLAS_MAIN_FREE"))
        ENV["OPENBLAS_MAIN_FREE"] = "1"
    end

    # Ensure that OpenBLAS does not grab a huge amount of memory at first,
    # since it instantly allocates scratch buffer space for the number of
    # threads it thinks it needs to use.
    # X-ref: https://github.com/xianyi/OpenBLAS/blob/c43ec53bdd00d9423fc609d7b7ecb35e7bf41b85/README.md#setting-the-number-of-threads-using-environment-variables
    # X-ref: https://github.com/JuliaLang/julia/issues/45434
    if !(haskey(ENV, "OPENBLAS_NUM_THREADS")) && (!(haskey(ENV, "GOTO_NUM_THREADS")) && !(haskey(ENV, "OMP_NUM_THREADS")))
        # We set this to `1` here, and then LinearAlgebra will update
        # to the true value in its `__init__()` function.
        ENV["OPENBLAS_DEFAULT_NUM_THREADS"] = "1"
    end

    global libopenblas_path = string(_libopenblas_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libopenblas_path)
    push!(LIBPATH_list, LIBPATH[])
end

end  # module OpenBLAS_jll
