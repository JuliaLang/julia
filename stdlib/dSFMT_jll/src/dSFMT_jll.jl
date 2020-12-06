# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/dSFMT_jll.jl

module dSFMT_jll

using Libdl

const PATH_list = String[]
const LIBPATH_list = String[]

export libdSFMT

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libdSFMT_handle = C_NULL
libdSFMT_path = ""

if Sys.iswindows()
    const libdSFMT = "libdSFMT.dll"
elseif Sys.isapple()
    const libdSFMT = "@rpath/libdSFMT.dylib"
else
    const libdSFMT = "libdSFMT.so"
end

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    global libdSFMT_handle = dlopen(libdSFMT)
    global libdSFMT_path = dlpath(libdSFMT_handle)
end

is_available() = true

end  # module dSFMT_jll
