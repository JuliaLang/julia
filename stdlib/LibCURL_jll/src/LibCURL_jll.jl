# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibCURL_jll.jl

module LibCURL_jll

using Libdl

const PATH_list = String[]
const LIBPATH_list = String[]

export libcurl

# These get calculated in __init__()
libcurl_handle = C_NULL
libcurl_path = ""

if Sys.iswindows()
    const libnghttp2 = "libnghttp2-14.dll"
    const libcurl = "libcurl-4.dll"
elseif Sys.isapple()
    const libnghttp2 = "libnghttp2.14.dylib"
    const libcurl = "libcurl.4.dylib"
else
    const libnghttp2 = "libnghttp2.so"
    const libcurl = "libcurl.so"
end

function __init__()
    dlopen(libnghttp2)
    global libcurl_handle = dlopen(libcurl)
    global libcurl_path = dlpath(libcurl_handle)
end

end  # module LibCURL_jll
