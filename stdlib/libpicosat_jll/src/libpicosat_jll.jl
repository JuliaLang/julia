# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/libpicosat_jll.jl
#
# libpicosat is bundled for the SAT-based dependency resolver in Pkg. It is
# not part of Julia's public interface and may stop being bundled in a future
# release; a package that needs PicoSAT should depend on libpicosat_jll from
# the General registry as usual.

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
    end
)

function eager_mode()
    dlopen(libpicosat)
end
is_available() = true

# JLLWrappers path compatibility accessor
get_libpicosat_path() = libpicosat_path

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
