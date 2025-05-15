# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/Zstd_jll.j:
#
baremodule Zstd_jll
using Base, Libdl

export libzstd, zstd, zstdmt

# These get calculated in __init__()
libzstd_handle::Ptr{Cvoid} = C_NULL

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

function __init__()
    global libzstd_handle = dlopen(libzstd)
    nothing
end

end  # module Zstd_jll
