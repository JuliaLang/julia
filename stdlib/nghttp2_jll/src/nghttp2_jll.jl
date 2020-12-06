# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/nghttp2_jll.jl
module nghttp2_jll

using Libdl

const PATH_list = String[]
const LIBPATH_list = String[]

export libnghttp2

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libnghttp2_handle = C_NULL
libnghttp2_path = ""

if Sys.iswindows()
    const libnghttp2 = "libnghttp2-14.dll"
elseif Sys.isapple()
    const libnghttp2 = "@rpath/libnghttp2.14.dylib"
else
    const libnghttp2 = "libnghttp2.so.14"
end

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    global libnghttp2_handle = dlopen(libnghttp2)
    global libnghttp2_path = dlpath(libnghttp2_handle)
end

is_available() = true

end  # module nghttp2_jll
