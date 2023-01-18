# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/PCRE2_jll.jl
baremodule PCRE2_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libpcre2_8

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
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
    global libpcre2_8_handle = dlopen(libpcre2_8)
    global libpcre2_8_path = dlpath(libpcre2_8_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libpcre2_8_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libpcre2_8_path() = libpcre2_8_path

end  # module PCRE2_jll
