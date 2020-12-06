# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibSSH2_jll.jl

module LibSSH2_jll

using Libdl
using MbedTLS_jll

const PATH_list = String[]
const LIBPATH_list = String[]

export libssh2

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libssh2_handle = C_NULL
libssh2_path = ""

if Sys.iswindows()
    const libssh2 = "libssh2.dll"
elseif Sys.isapple()
    const libssh2 = "@rpath/libssh2.1.dylib"
else
    const libssh2 = "libssh2.so.1"
end

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    global libssh2_handle = dlopen(libssh2)
    global libssh2_path = dlpath(libssh2_handle)
end

is_available() = true

end  # module LibSSH2_jll
