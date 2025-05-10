# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/Zstd_jll.jl
baremodule Zstd_jll
using Base, Libdl

#const PATH_list = String[]
#const LIBPATH_list = String[]

export libzstd

## These get calculated in __init__()
#const PATH = Ref("")
#const LIBPATH = Ref("")
#artifact_dir::String = ""
#libzstd_handle::Ptr{Cvoid} = C_NULL
#libzstd_path::String = ""

if Sys.iswindows()
    const libzstd = "libzstd.dll"
elseif Sys.isapple()
    const libzstd = "@rpath/libstd.1.dylib"
else
    const libzstd = "libzstd.so.1"
end

#function __init__()
#    global libzstd_handle = dlopen(libzstd)
#    global libzstd_path = dlpath(libzstd_handle)
#    global artifact_dir = dirname(Sys.BINDIR)
#    LIBPATH[] = dirname(libzstd_path)
#    push!(LIBPATH_list, LIBPATH[])
#end

## JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
## For instance, `find_artifact_dir()` won't actually be the artifact directory, because
## there isn't one.  It instead returns the overall Julia prefix.
#is_available() = true
#find_artifact_dir() = artifact_dir
#dev_jll() = error("stdlib JLLs cannot be dev'ed")
#best_wrapper = nothing
#get_libzstd_path() = libzstd_path

end  # module Zstd_jll
