# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibSSH2_jll.jl

baremodule LibSSH2_jll
using Base, Libdl
if Sys.isfreebsd() || Sys.isapple()
    using Zlib_jll
end
if Sys.iswindows() && Sys.WORD_SIZE == 32
    using CompilerSupportLibraries_jll
end
if !Sys.iswindows()
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
const libssh2 = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libssh2.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libssh2.1.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libssh2.so.1")
    else
        error("LibSSH2_jll: Library 'libssh2' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows()
        if Sys.WORD_SIZE == 32
            LazyLibrary[libgcc_s]
        else
            LazyLibrary[]
        end
    elseif Sys.islinux()
        LazyLibrary[libcrypto]
    elseif Sys.isfreebsd() || Sys.isapple()
        LazyLibrary[libz, libcrypto]
    end
)

function eager_mode()
    @static if @isdefined Zlib_jll
        Zlib_jll.eager_mode()
    end
    @static if @isdefined CompilerSupportLibraries_jll
        CompilerSupportLibraries_jll.eager_mode()
    end
    @static if @isdefined OpenSSL_jll
        OpenSSL_jll.eager_mode()
    end
    dlopen(libssh2)
end
is_available() = true

function __init__()
    global libssh2_path = string(libssh2.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libssh2_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module LibSSH2_jll
