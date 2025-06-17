# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibGit2_jll.jl

baremodule LibGit2_jll
using Base, Libdl, LibSSH2_jll
if !(Sys.iswindows() || Sys.isapple())
    using OpenSSL_jll
end
if Sys.iswindows() && Sys.WORD_SIZE == 32
    using CompilerSupportLibraries_jll
end

export libgit2

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libgit2_path::String = ""
const libgit2 = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libgit2.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libgit2.1.9.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libgit2.so.1.9")
    else
        error("LibGit2_jll: Library 'libgit2' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows()
        if Sys.WORD_SIZE == 32
            LazyLibrary[libssh2, libgcc_s]
        else
            LazyLibrary[libssh2]
        end
    elseif Sys.isfreebsd() || Sys.islinux()
        LazyLibrary[libssh2, libssl, libcrypto]
    else
        LazyLibrary[libssh2]
    end
)

function eager_mode()
    LibSSH2_jll.eager_mode()
    @static if @isdefined OpenSSL_jll
        OpenSSL_jll.eager_mode()
    end
    @static if @isdefined CompilerSupportLibraries_jll
        CompilerSupportLibraries_jll.eager_mode()
    end
    dlopen(libgit2)
end
is_available() = true

function __init__()
    global libgit2_path = string(libgit2.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libgit2_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module LibGit2_jll
