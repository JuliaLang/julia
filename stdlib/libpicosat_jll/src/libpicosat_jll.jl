# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/libpicosat_jll.jl

baremodule libpicosat_jll
using Base, Libdl

export libpicosat

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libpicosat_path::String = ""
const libpicosat = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libpicosat.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libpicosat.dylib")
    else
        BundledLazyLibraryPath("libpicosat.so")
    end;
    dependencies = LazyLibrary[]
)

function eager_mode()
    dlopen(libpicosat)
end
is_available() = true

function __init__()
    global libpicosat_path = string(libpicosat.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libpicosat_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module libpicosat_jll
