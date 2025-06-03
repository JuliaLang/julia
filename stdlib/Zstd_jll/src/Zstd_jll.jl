# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/Zstd_jll.j:
#
baremodule Zstd_jll
using Base, Libdl

export libzstd, zstd, zstdmt

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libzstd_path::String = ""

if Sys.iswindows()
    const _libzstd_path = BundledLazyLibraryPath("libzstd-1.dll")
elseif Sys.isapple()
    const _libzstd_path = BundledLazyLibraryPath("libzstd.1.dylib")
else
    const _libzstd_path = BundledLazyLibraryPath("libzstd.so.1")
end

const libzstd = LazyLibrary(_libzstd_path)

if Sys.iswindows()
    const zstd_exe = "zstd.exe"
    const zstdmt_exe = "zstdmt.exe"
else
    const zstd_exe = "zstd"
    const zstdmt_exe = "zstdmt"
end

if Sys.iswindows()
    const pathsep = ';'
elseif Sys.isapple()
    const pathsep = ':'
else
    const pathsep = ':'
end

if Sys.iswindows()
function adjust_ENV(cmd::Cmd)
    dllPATH = Sys.BINDIR
    oldPATH = get(ENV, "PATH", "")
    newPATH = isempty(oldPATH) ? dllPATH : "$dllPATH$pathsep$oldPATH"
    return addenv(cmd, "PATH"=>newPATH)
end
else
adjust_ENV(cmd::Cmd) = cmd
end

function adjust_ENV()
    addPATH = joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR)
    oldPATH = get(ENV, "PATH", "")
    newPATH = isempty(oldPATH) ? addPATH : "$addPATH$pathsep$oldPATH"
    return ("PATH"=>newPATH,)
end

function zstd(f::Function; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true) # deprecated, for compat only
    withenv((adjust_PATH ? adjust_ENV() : ())...) do
        f(zstd())
    end
end
function zstdmt(f::Function; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true) # deprecated, for compat only
    withenv((adjust_PATH ? adjust_ENV() : ())...) do
        f(zstdmt())
    end
end
zstd() = adjust_ENV(`$(joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, zstd_exe))`)
zstdmt() = adjust_ENV(`$(joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, zstdmt_exe))`)

# Function to eagerly dlopen our library and thus resolve all dependencies
function eager_mode()
    dlopen(libzstd)
end

is_available() = true

function __init__()
    global libzstd_path = string(_libzstd_path)
    global artifact_dir = dirname(Sys.BINDIR)
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module Zstd_jll
