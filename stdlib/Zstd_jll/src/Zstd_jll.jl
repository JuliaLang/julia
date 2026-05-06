# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/Zstd_jll.jl
baremodule Zstd_jll
using Base, Libdl
if Sys.iswindows() && Sys.WORD_SIZE == 32
    using CompilerSupportLibraries_jll
end

const PATH_list = String[]
const LIBPATH_list = String[]

export libzstd, zstd, zstdmt

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir::String = ""
libzstd_handle::Ptr{Cvoid} = C_NULL
libzstd_path::String = ""
zstd_path::String = ""
zstdmt_path::String = ""

if Sys.iswindows()
    const libzstd = "libzstd-1.dll"
elseif Sys.isapple()
    const libzstd = "@rpath/libzstd.1.dylib"
else
    const libzstd = "libzstd.so.1"
end

if Sys.iswindows()
    const zstd_exe = "zstd.exe"
    const zstdmt_exe = "zstdmt.exe"
else
    const zstd_exe = "zstd"
    const zstdmt_exe = "zstdmt"
end

if Sys.iswindows()
    const pathsep = ';'
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
zstd() = adjust_ENV(`$zstd_path`)
zstdmt() = adjust_ENV(`$zstdmt_path`)

function __init__()
    global libzstd_handle = dlopen(libzstd)
    global libzstd_path = dlpath(libzstd_handle)
    global zstd_path = joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, zstd_exe)
    global zstdmt_path = joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, zstdmt_exe)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libzstd_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libzstd_path() = libzstd_path

end  # module Zstd_jll
