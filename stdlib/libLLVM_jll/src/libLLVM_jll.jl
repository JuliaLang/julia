# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/libLLVM_jll.jl

baremodule libLLVM_jll
using Base, Libdl, Zlib_jll, Zstd_jll, CompilerSupportLibraries_jll

export libLLVM

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libLLVM_path::String = ""

if Sys.iswindows()
    const _libLLVM_path = BundledLazyLibraryPath("$(Base.libllvm_name).dll")
elseif Sys.isapple()
    const _libLLVM_path = BundledLazyLibraryPath("libLLVM.dylib")
else
    const _libLLVM_path = BundledLazyLibraryPath("$(Base.libllvm_name).so")
end

if Sys.isapple()
    _libLLVM_dependencies = LazyLibrary[libz, libzstd]
elseif Sys.isfreebsd()
    _libLLVM_dependencies = LazyLibrary[libz, libzstd, libgcc_s]
else
    _libLLVM_dependencies = LazyLibrary[libz, libzstd, libstdcxx, libgcc_s]
end

const libLLVM = LazyLibrary(
    _libLLVM_path,
    dependencies=_libLLVM_dependencies,
)

function eager_mode()
    CompilerSupportLibraries_jll.eager_mode()
    Zlib_jll.eager_mode()
    # Zstd_jll.eager_mode() # Not lazy yet
    dlopen(libLLVM)
end
is_available() = true

function __init__()
    global libLLVM_path = string(_libLLVM_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libLLVM_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module libLLVM_jll
