# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibCURL_jll.jl

baremodule LibCURL_jll
using Base, Libdl, nghttp2_jll, LibSSH2_jll, Zlib_jll
if !(Sys.iswindows() || Sys.isapple())
    using OpenSSL_jll
end
if Sys.iswindows() && Sys.WORD_SIZE == 32
    using CompilerSupportLibraries_jll
end

export libcurl

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libcurl_path::String = ""
const libcurl = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libcurl-4.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libcurl.4.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libcurl.so.4")
    else
        error("LibCURL_jll: Library 'libcurl' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows()
        if  Sys.WORD_SIZE == 32
            LazyLibrary[libz, libnghttp2, libssh2, libgcc_s]
        else
            LazyLibrary[libz, libnghttp2, libssh2]
        end
    elseif Sys.islinux() || Sys.isfreebsd()
        LazyLibrary[libz, libnghttp2, libssh2, libssl, libcrypto]
    elseif Sys.isapple()
        LazyLibrary[libz, libnghttp2, libssh2]
    end
)

function eager_mode()
    Zlib_jll.eager_mode()
    nghttp2_jll.eager_mode()
    LibSSH2_jll.eager_mode()
    @static if @isdefined CompilerSupportLibraries_jll
        CompilerSupportLibraries_jll.eager_mode()
    end
    @static if @isdefined OpenSSL_jll
        OpenSSL_jll.eager_mode()
    end
    dlopen(libcurl)
end
is_available() = true

function __init__()
    global libcurl_path = string(libcurl.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libcurl_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module LibCURL_jll
