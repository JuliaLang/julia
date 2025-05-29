# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibSSH2_jll.jl

baremodule LibSSH2_jll
using Base, Libdl, Zlib_jll
if !Sys.iswindows()
    # On Windows we use system SSL/crypto libraries
    using OpenSSL_jll
end

export libssh2

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libssh2_path::String = ""


if Sys.iswindows()
    const _libssh2_path = BundledLazyLibraryPath("libssh2.dll")
    _libssh2_dependencies = LazyLibrary[]
elseif Sys.isapple()
    const _libssh2_path = BundledLazyLibraryPath("libssh2.1.dylib")
    _libssh2_dependencies = LazyLibrary[libz, libcrypto]
elseif Sys.isfreebsd()
    const _libssh2_path = BundledLazyLibraryPath("libssh2.so.1")
    _libssh2_dependencies = LazyLibrary[libz, libcrypto]
else
    const _libssh2_path = BundledLazyLibraryPath("libssh2.so.1")
    _libssh2_dependencies = LazyLibrary[libcrypto]
end

const libssh2 = LazyLibrary(_libssh2_path, dependencies=_libssh2_dependencies)

function eager_mode()
    Zlib_jll.eager_mode()
    @static if !Sys.iswindows()
        OpenSSL_jll.eager_mode()
    end
    dlopen(libssh2)
end
is_available() = true

function __init__()
    global libssh2_path = string(_libssh2_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libssh2_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module LibSSH2_jll
