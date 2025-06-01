# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/nghttp2_jll.jl
baremodule nghttp2_jll
using Base, Libdl

export libnghttp2

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libnghttp2_path::String = ""

if Sys.iswindows()
    const _libnghttp2_path = BundledLazyLibraryPath("libnghttp2-14.dll")
elseif Sys.isapple()
    const _libnghttp2_path = BundledLazyLibraryPath("libnghttp2.14.dylib")
else
    const _libnghttp2_path = BundledLazyLibraryPath("libnghttp2.so.14")
end

const libnghttp2 = LazyLibrary(_libnghttp2_path)

function eager_mode()
    dlopen(libnghttp2)
end
is_available() = true

function __init__()
    global libnghttp2_path = string(_libnghttp2_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libnghttp2_path)
    push!(LIBPATH_list, LIBPATH[])
end

end  # module nghttp2_jll
