# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibOSXUnwind_jll.jl

baremodule LibOSXUnwind_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libosxunwind

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libosxunwind_handle = C_NULL
libosxunwind_path = ""

const libosxunwind = "@rpath/libosxunwind.dylib"

function __init__()
    # We only dlopen something on MacOS
    @static if Sys.isapple()
        global libosxunwind_handle = dlopen(libosxunwind)
        global libosxunwind_path = dlpath(libosxunwind_handle)
        global artifact_dir = dirname(Sys.BINDIR)
        global LIBPATH[] = dirname(libosxunwind_path)
        push!(LIBPATH_list, LIBPATH[])
    end
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = @static Sys.isapple() ? true : false
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libosxunwind_path() = libosxunwind_path

end  # module LibOSXUnwind_jll
