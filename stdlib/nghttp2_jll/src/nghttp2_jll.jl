# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/nghttp2_jll.jl
baremodule nghttp2_jll
using Base, Libdl
if Sys.iswindows() && Sys.WORD_SIZE == 32
    using CompilerSupportLibraries_jll
end

export libnghttp2

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libnghttp2_path::String = ""
const libnghttp2 = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libnghttp2-14.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libnghttp2.14.dylib")
    else
        BundledLazyLibraryPath("libnghttp2.so.14")
    end,
    dependencies = if Sys.iswindows() && Sys.WORD_SIZE == 32
        LazyLibrary[libgcc_s]
    else
        LazyLibrary[]
    end
)

function eager_mode()
    @static if @isdefined CompilerSupportLibraries_jll
        CompilerSupportLibraries_jll.eager_mode()
    end
    dlopen(libnghttp2)
end
is_available() = true

function __init__()
    global libnghttp2_path = string(libnghttp2.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libnghttp2_path)
    push!(LIBPATH_list, LIBPATH[])
end

end  # module nghttp2_jll
