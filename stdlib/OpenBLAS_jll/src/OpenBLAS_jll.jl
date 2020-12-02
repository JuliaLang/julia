# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/OpenBLAS_jll.jl
module OpenBLAS_jll

using Libdl, CompilerSupportLibraries_jll, Base.BinaryPlatforms

const PATH_list = String[]
const LIBPATH_list = String[]

export libopenblas

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libopenblas_handle = C_NULL
libopenblas_path = ""

if arch(HostPlatform()) in ("x86_64", "ppc64le")
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
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    global libopenblas_handle = dlopen(libopenblas)
    global libopenblas_path = dlpath(libopenblas_handle)
end

is_available() = true

end  # module OpenBLAS_jll
