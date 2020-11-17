# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibCURL_jll.jl

module LibCURL_jll

using Libdl
using nghttp2_jll

const PATH_list = String[]
const LIBPATH_list = String[]

export libcurl

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libcurl_handle = C_NULL
libcurl_path = ""

if Sys.iswindows()
    const libcurl = "libcurl-4.dll"
elseif Sys.isapple()
    const libcurl = "@rpath/libcurl.4.dylib"
else
    const libcurl = "libcurl.so"
end

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    global libcurl_handle = dlopen(libcurl)
    global libcurl_path = dlpath(libcurl_handle)
end

is_available() = true

end  # module LibCURL_jll
