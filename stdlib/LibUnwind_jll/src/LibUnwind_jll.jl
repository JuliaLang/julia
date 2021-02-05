# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibUnwind_jll.jl

baremodule LibUnwind_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libunwind

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libunwind_handle = C_NULL
libunwind_path = ""

const libunwind = "libunwind.so.8"

function __init__()
    # We only do something on Linux/FreeBSD
    @static if Sys.islinux() || Sys.isfreebsd()
        global libunwind_handle = dlopen(libunwind)
        global libunwind_path = dlpath(libunwind_handle)
        global artifact_dir = dirname(Sys.BINDIR)
        global LIBPATH[] = dirname(libunwind_path)
        push!(LIBPATH_list, LIBPATH[])
    end
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = @static (Sys.islinux() || Sys.isfreebsd()) ? true : false
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libunwind_path() = libunwind_path

end  # module LibUnwind_jll
