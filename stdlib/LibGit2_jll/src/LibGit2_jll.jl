# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibGit2_jll.jl

baremodule LibGit2_jll
using Base, Libdl, LibSSH2_jll
if !(Sys.iswindows() || Sys.isapple())
    # On Windows and macOS we use system SSL/crypto libraries
    using OpenSSL_jll
end

export libgit2

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libgit2_path::String = ""

if Sys.iswindows()
    const _libgit2_path = BundledLazyLibraryPath("libgit2.dll")
elseif Sys.isapple()
    const _libgit2_path = BundledLazyLibraryPath("libgit2.1.9.dylib")
else
    const _libgit2_path = BundledLazyLibraryPath("libgit2.so.1.9")
end

if Sys.isfreebsd()
    _libgit2_dependencies = LazyLibrary[libssh2, libssl, libcrypto]
else
    _libgit2_dependencies = LazyLibrary[libssh2]
end
const libgit2 = LazyLibrary(_libgit2_path, dependencies=_libgit2_dependencies)

function eager_mode()
    LibSSH2_jll.eager_mode()
    @static if !(Sys.iswindows() || Sys.isapple())
        OpenSSL_jll.eager_mode()
    end
    dlopen(libgit2)
end
is_available() = true

function __init__()
    global libgit2_path = string(_libgit2_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libgit2_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module LibGit2_jll
