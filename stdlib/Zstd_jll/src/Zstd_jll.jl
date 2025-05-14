# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/Zstd_jll.j:
#
baremodule Zstd_jll
using Base, Libdl

export libzstd, zstd, zstdmt

if Sys.iswindows()
    const libzstd = "libzstd.dll"
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

zstd(f::Function) = f(zstd())
zstd() = `$(Sys.BINDIR)/$zstd_exe`
zstdmt(f::Function) = f(zstdmt())
zstdmt() = `$(Sys.BINDIR)/$zstdmt_exe`

function __init__()
    global libzstd_handle = dlopen(libzstd)
    nothing
end

end  # module Zstd_jll
