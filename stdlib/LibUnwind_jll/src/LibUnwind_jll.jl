# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibUnwind_jll.jl

module LibUnwind_jll

using Libdl

const PATH_list = String[]
const LIBPATH_list = String[]

export libunwind

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libunwind_handle = C_NULL
libunwind_path = ""

const libunwind = "libunwind.so.8"

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    # We only do something on Linux/FreeBSD
    @static if Sys.islinux() || Sys.isfreebsd()
        global libunwind_handle = dlopen(libunwind)
        global libunwind_path = dlpath(libunwind_handle)
    end
end

is_available() = @static (Sys.islinux() || Sys.isfreebsd()) ? true : false

end  # module LibUnwind_jll
