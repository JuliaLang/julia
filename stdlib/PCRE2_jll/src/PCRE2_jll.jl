# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/PCRE2_jll.jl
module PCRE2_jll

using Libdl

const PATH_list = String[]
const LIBPATH_list = String[]

export libpcre2_8

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libpcre2_8_handle = C_NULL
libpcre2_8_path = ""

if Sys.iswindows()
    const libpcre2_8 = "libpcre2-8-0.dll"
elseif Sys.isapple()
    const libpcre2_8 = "@rpath/libpcre2-8.0.dylib"
else
    const libpcre2_8 = "libpcre2-8.so.0"
end

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    global libpcre2_8_handle = dlopen(libpcre2_8)
    global libpcre2_8_path = dlpath(libpcre2_8_handle)
end

is_available() = true

end  # module PCRE2_jll
