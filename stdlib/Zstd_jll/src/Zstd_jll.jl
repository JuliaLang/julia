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
function adjust_ENV(cmd::Cmd)
    dllPATH = Sys.BINDIR
    oldPATH = get(ENV, "PATH", "")
    newPATH = isempty(oldPATH) ? dllPATH : "$dllPATH;$oldPATH"
    return addenv(cmd, "PATH"=>newPATH)
end
else
adjust_ENV(cmd::Cmd) = cmd
end

zstd(f::Function) = f(zstd())
zstdmt(f::Function) = f(zstdmt())
zstd() = adjust_ENV(`$(Sys.BINDIR)/$(Base.PRIVATE_LIBEXECDIR)/$zstd_exe`)
zstdmt() = adjust_ENV(`$(Sys.BINDIR)/$(Base.PRIVATE_LIBEXECDIR)/$zstdmt_exe`)

function __init__()
    global libzstd_handle = dlopen(libzstd)
    nothing
end

end  # module Zstd_jll
