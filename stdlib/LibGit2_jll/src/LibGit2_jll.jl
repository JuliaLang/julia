# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibGit2_jll.jl

baremodule LibGit2_jll
using Base, Libdl, LibSSH2_jll
if !(Sys.iswindows() || Sys.isapple())
    # On Windows and macOS we use system SSL/crypto libraries
    using OpenSSL_jll
end

const PATH_list = String[]
const LIBPATH_list = String[]

export libgit2

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir::String = ""
libgit2_handle::Ptr{Cvoid} = C_NULL
libgit2_path::String = ""

if Sys.iswindows()
    const libgit2 = "libgit2.dll"
elseif Sys.isapple()
    const libgit2 = "@rpath/libgit2.1.9.dylib"
else
    const libgit2 = "libgit2.so.1.9"
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
