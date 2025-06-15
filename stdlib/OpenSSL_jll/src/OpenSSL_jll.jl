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
const libcrypto = LazyLibrary(
    if Sys.iswindows()
        if arch(HostPlatform()) == "x86_64"
            BundledLazyLibraryPath("libcrypto-3-x64.dll")
        else
            BundledLazyLibraryPath("libcrypto-3.dll")
        end
    elseif Sys.isapple()
        BundledLazyLibraryPath("libcrypto.3.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libcrypto.so.3")
    else
        error("OpenSSL_jll: Library 'libcrypto' is not available for $(Sys.KERNEL)")
    end
)

libssl_path::String = ""
const libssl = LazyLibrary(
    if Sys.iswindows()
        if arch(HostPlatform()) == "x86_64"
            BundledLazyLibraryPath("libssl-3-x64.dll")
        else
            BundledLazyLibraryPath("libssl-3.dll")
        end
    elseif Sys.isapple()
        BundledLazyLibraryPath("libssl.3.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libssl.so.3")
    else
        error("OpenSSL_jll: Library 'libssl' is not available for $(Sys.KERNEL)")
    end;
    dependencies = LazyLibrary[libcrypto]
)

function eager_mode()
    dlopen(libcrypto)
    dlopen(libssl)
end
is_available() = true

function __init__()
    global libcrypto_path = string(libcrypto.path)
    global libssl_path = string(libssl.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libssl_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module OpenSSL_jll
