# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/GMP_jll.jl
module GMP_jll

using Libdl

const PATH_list = String[]
const LIBPATH_list = String[]

export libgmp, libgmpxx

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
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
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    global libgmp_handle = dlopen(libgmp)
    global libgmp_path = dlpath(libgmp_handle)
    global libgmpxx_handle = dlopen(libgmpxx)
    global libgmpxx_path = dlpath(libgmpxx_handle)
end

is_available() = true

end  # module GMP_jll
