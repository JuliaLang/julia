# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/OpenSSL_jll.jl

baremodule OpenSSL_jll
using Base, Libdl, Base.BinaryPlatforms

const PATH_list = String[]
const LIBPATH_list = String[]

export libcrypto, libssl

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir::String = ""
libcrypto_handle::Ptr{Cvoid} = C_NULL
libcrypto_path::String = ""
libssl_handle::Ptr{Cvoid} = C_NULL
libssl_path::String = ""

if Sys.iswindows()
    if arch(HostPlatform()) == "x86_64"
        const libcrypto = "libcrypto-3-x64.dll"
        const libssl = "libssl-3-x64.dll"
    else
        const libcrypto = "libcrypto-3.dll"
        const libssl = "libssl-3.dll"
    end
elseif Sys.isapple()
    const libcrypto = "@rpath/libcrypto.3.dylib"
    const libssl = "@rpath/libssl.3.dylib"
else
    const libcrypto = "libcrypto.so.3"
    const libssl = "libssl.so.3"
end

function __init__()
    global libcrypto_handle = dlopen(libcrypto)
    global libcrypto_path = dlpath(libcrypto_handle)
    global libssl_handle = dlopen(libssl)
    global libssl_path = dlpath(libssl_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libssl_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libcrypto_path() = libcrypto_path
get_libssl_path() = libssl_path

end  # module OpenSSL_jll
