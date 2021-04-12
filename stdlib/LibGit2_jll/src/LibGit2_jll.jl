# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibGit2_jll.jl

baremodule LibGit2_jll
using Base, Libdl, MbedTLS_jll, LibSSH2_jll
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libgit2

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
libgit2_handle = C_NULL
libgit2_path = ""

if Sys.iswindows()
    const libgit2 = "libgit2.dll"
elseif Sys.isapple()
    const libgit2 = "@rpath/libgit2.1.1.dylib"
else
    const libgit2 = "libgit2.so.1.1"
end

function __init__()
    global libgit2_handle = dlopen(libgit2)
    global libgit2_path = dlpath(libgit2_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libgit2_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libgit2_path() = libgit2_path

end  # module LibGit2_jll
