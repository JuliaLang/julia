# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibSSH2_jll.jl

baremodule LibSSH2_jll
using Base, Libdl, MbedTLS_jll
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libssh2

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
libssh2_handle = C_NULL
libssh2_path = ""

if Sys.iswindows()
    const libssh2 = "libssh2.dll"
elseif Sys.isapple()
    const libssh2 = "@rpath/libssh2.1.dylib"
else
    const libssh2 = "libssh2.so.1"
end

function __init__()
    global libssh2_handle = dlopen(libssh2)
    global libssh2_path = dlpath(libssh2_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libssh2_path)
    push!(LIBPATH_list, LIBPATH[])
end


# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libssh2_path() = libssh2_path

end  # module LibSSH2_jll
