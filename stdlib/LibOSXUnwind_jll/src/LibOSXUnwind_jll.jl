# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibOSXUnwind_jll.jl

module LibOSXUnwind_jll

using Libdl

const PATH_list = String[]
const LIBPATH_list = String[]

export libosxunwind

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libosxunwind_handle = C_NULL
libosxunwind_path = ""

const libosxunwind = "@rpath/libosxunwind.dylib"

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    # We only dlopen something on MacOS
    @static if Sys.isapple()
        global libosxunwind_handle = dlopen(libosxunwind)
        global libosxunwind_path = dlpath(libosxunwind_handle)
    end
end

is_available() = @static Sys.isapple() ? true : false

end  # module LibOSXUnwind_jll
