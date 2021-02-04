# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/libLLVM_jll.jl

baremodule libLLVM_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libLLVM

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libLLVM_handle = C_NULL
libLLVM_path = ""

if Sys.iswindows()
    const libLLVM = "LLVM.dll"
elseif Sys.isapple()
    const libLLVM = "@rpath/libLLVM.dylib"
else
    const libLLVM = "libLLVM-11jl.so"
end

function __init__()
    global libLLVM_handle = dlopen(libLLVM)
    global libLLVM_path = dlpath(libLLVM_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = dirname(libLLVM_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libLLVM_path() = libLLVM_path

end  # module libLLVM_jll
