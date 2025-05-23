# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LLVMLibUnwind_jll.jl

baremodule LLVMLibUnwind_jll
using Base, Libdl

export llvmlibunwind

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
llvmlibunwind_path::String = ""

const _llvmlibunwind_path = BundledLazyLibraryPath("libunwind")
const llvmlibunwind = LazyLibrary(_llvmlibunwind_path)
function eager_mode()
    dlopen(llvmlibunwind)
end
is_available() = @static Sys.isapple() ? true : false

function __init__()
    global llvmlibunwind_path = string(_llvmlibunwind_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(llvmlibunwind_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module LLVMLibUnwind_jll
