# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibUnwind_jll.jl

baremodule LibUnwind_jll
using Base, Libdl
using CompilerSupportLibraries_jll
using Zlib_jll

export libunwind

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libunwind_path::String = ""

const _libunwind_path = BundledLazyLibraryPath("libunwind.so.8")

if Sys.isfreebsd()
    _libunwind_dependencies = LazyLibrary[libz]
else
    _libunwind_dependencies = LazyLibrary[libgcc_s, libz]
end
const libunwind = LazyLibrary(_libunwind_path, dependencies=_libunwind_dependencies)


function eager_mode()
    CompilerSupportLibraries_jll.eager_mode()
    Zlib_jll.eager_mode()
    dlopen(libunwind)
end
is_available() = @static(Sys.islinux() || Sys.isfreebsd()) ? true : false

function __init__()
    global libunwind_path = string(_libunwind_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libunwind_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module LibUnwind_jll
