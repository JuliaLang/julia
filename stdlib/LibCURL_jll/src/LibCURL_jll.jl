# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibCURL_jll.jl

baremodule LibCURL_jll
using Base, Libdl, nghttp2_jll, LibSSH2_jll, Zlib_jll
if !(Sys.iswindows() || Sys.isapple())
    # On Windows and macOS we use system SSL/crypto libraries
    using OpenSSL_jll
end

export libcurl

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libcurl_path::String = ""

_libcurl_dependencies = LazyLibrary[libz, libnghttp2, libssh2]
if !(Sys.iswindows() || Sys.isapple())
    append!(_libcurl_dependencies, [libssl, libcrypto])
end

if Sys.iswindows()
    const _libcurl_path = BundledLazyLibraryPath("libcurl-4.dll")
elseif Sys.isapple()
    const _libcurl_path = BundledLazyLibraryPath("libcurl.4.dylib")
else
    const _libcurl_path = BundledLazyLibraryPath("libcurl.so.4")
end

const libcurl = LazyLibrary(
    _libcurl_path,
    dependencies=_libcurl_dependencies,
)

function eager_mode()
    Zlib_jll.eager_mode()
    nghttp2_jll.eager_mode()
    LibSSH2_jll.eager_mode()
    dlopen(libcurl)
end
is_available() = true

function __init__()
    global libcurl_path = string(_libcurl_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libcurl_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module LibCURL_jll
