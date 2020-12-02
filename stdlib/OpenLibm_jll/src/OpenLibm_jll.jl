# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/OpenLibm_jll.jl
module OpenLibm_jll

using Libdl

const PATH_list = String[]
const LIBPATH_list = String[]

export libopenlibm

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libopenlibm_handle = C_NULL
libopenlibm_path = ""

if Sys.iswindows()
    const libopenlibm = "libopenlibm.dll"
elseif Sys.isapple()
    const libopenlibm = "@rpath/libopenlibm.3.dylib"
else
    const libopenlibm = "libopenlibm.so.3"
end

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    global libopenlibm_handle = dlopen(libopenlibm)
    global libopenlibm_path = dlpath(libopenlibm_handle)
end

# JLLWrappers API
is_available() = true
get_libopenlibm_path() = libopenlibm_path

end  # module OpenLibm_jll
