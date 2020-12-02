# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/MPFR_jll.jl
module MPFR_jll
using GMP_jll

using Libdl

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
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    global libmpfr_handle = dlopen(libmpfr)
    global libmpfr_path = dlpath(libmpfr_handle)
end

is_available() = true

end  # module MPFR_jll
