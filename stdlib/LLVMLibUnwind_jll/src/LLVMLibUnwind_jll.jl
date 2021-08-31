# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LLVMLibUnwind_jll.jl

baremodule LLVMLibUnwind_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export llvmlibunwind

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
llvmlibunwind_handle = C_NULL
llvmlibunwind_path = ""

const llvmlibunwind = "libunwind"

function __init__()
    # We only dlopen something on MacOS
    @static if Sys.isapple()
        global llvmlibunwind_handle = dlopen(llvmlibunwind)
        global llvmlibunwind_path = dlpath(llvmlibunwind_handle)
        global artifact_dir = dirname(Sys.BINDIR)
        LIBPATH[] = dirname(llvmlibunwind_path)
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
get_llvmlibunwind_path() = llvmlibunwind_path

end  # module LLVMLibUnwind_jll
