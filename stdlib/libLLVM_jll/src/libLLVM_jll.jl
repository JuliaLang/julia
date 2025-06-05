# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/libLLVM_jll.jl

baremodule libLLVM_jll
using Base, Libdl, Zlib_jll, Zstd_jll

if !Sys.isapple()
    using CompilerSupportLibraries_jll
end

export libLLVM

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libLLVM_path::String = ""
const libLLVM = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("$(Base.libllvm_name).dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libLLVM.dylib")
    else
        BundledLazyLibraryPath("$(Base.libllvm_name).so")
    end,
    dependencies = if Sys.isapple()
        LazyLibrary[libz, libzstd]
    elseif Sys.isfreebsd()
        LazyLibrary[libz, libzstd, libgcc_s]
    else
        LazyLibrary[libz, libzstd, libstdcxx, libgcc_s]
    end
)

function eager_mode()
    @static if @isdefined CompilerSupportLibraries_jll
        CompilerSupportLibraries_jll.eager_mode()
    end
    Zlib_jll.eager_mode()
    # Zstd_jll.eager_mode() # Not lazy yet
    dlopen(libLLVM)
end
is_available() = true

function __init__()
    global libLLVM_path = string(libLLVM.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libLLVM_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module libLLVM_jll
