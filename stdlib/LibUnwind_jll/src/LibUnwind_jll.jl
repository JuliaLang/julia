# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibUnwind_jll.jl

baremodule LibUnwind_jll
using Base, Libdl
using Zlib_jll
if !Sys.isfreebsd()
    using CompilerSupportLibraries_jll
end

export libunwind

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libunwind_path::String = ""
const libunwind = LazyLibrary(
    BundledLazyLibraryPath("libunwind.so.8"),
    dependencies = if Sys.isfreebsd()
        LazyLibrary[libz]
    else
        LazyLibrary[libgcc_s, libz]
    end
)

function eager_mode()
    @static if @isdefined CompilerSupportLibraries_jll
        CompilerSupportLibraries_jll.eager_mode()
    end
    Zlib_jll.eager_mode()
    dlopen(libunwind)
end
is_available() = @static(Sys.islinux() || Sys.isfreebsd()) ? true : false

function __init__()
    global libunwind_path = string(libunwind.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libunwind_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module LibUnwind_jll
