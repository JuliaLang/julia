# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/OpenSSL_jll.jl

baremodule OpenSSL_jll
using Base, Libdl, Base.BinaryPlatforms

export libcrypto, libssl

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libcrypto_path::String = ""
libssl_path::String = ""

if Sys.iswindows()
    if arch(HostPlatform()) == "x86_64"
        const _libcrypto_path = BundledLazyLibraryPath("libcrypto-3-x64.dll")
        const _libssl_path = BundledLazyLibraryPath("libssl-3-x64.dll")
    else
        const _libcrypto_path = BundledLazyLibraryPath("libcrypto-3.dll")
        const _libssl_path = BundledLazyLibraryPath("libssl-3.dll")
    end
elseif Sys.isapple()
    const _libcrypto_path = BundledLazyLibraryPath("libcrypto.3.dylib")
    const _libssl_path = BundledLazyLibraryPath("libssl.3.dylib")
else
    const _libcrypto_path = BundledLazyLibraryPath("libcrypto.so.3")
    const _libssl_path = BundledLazyLibraryPath("libssl.so.3")
end

const libcrypto = LazyLibrary(_libcrypto_path)

_libssl_dependencies = LazyLibrary[libcrypto]
const libssl = LazyLibrary(_libssl_path, dependencies=_libssl_dependencies)

function eager_mode()
    dlopen(libcrypto)
    dlopen(libssl)
end
is_available() = true

function __init__()
    global libcrypto_path = string(_libcrypto_path)
    global libssl_path = string(_libssl_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libssl_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module OpenSSL_jll
