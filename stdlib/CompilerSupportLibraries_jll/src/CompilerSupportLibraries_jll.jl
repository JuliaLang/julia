# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/CompilerSupportLibraries_jll.jl

module CompilerSupportLibraries_jll

using Libdl, Base.BinaryPlatforms

const PATH_list = String[]
const LIBPATH_list = String[]

export libgfortran, libstdcxx, libgomp

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
libgfortran_handle = C_NULL
libgfortran_path = ""
libstdcxx_handle = C_NULL
libstdcxx_path = ""
libgomp_handle = C_NULL
libgomp_path = ""

if Sys.iswindows()
    if arch(HostPlatform()) == "x86_64"
        const libgcc_s = "libgcc_s_seh-1.dll"
    else
        const libgcc_s = "libgcc_s_sjlj-1.dll"
    end
    const libgfortran = string("libgfortran-", libgfortran_version(HostPlatform()).major, ".dll")
    const libstdcxx = "libstdc++-6.dll"
    const libgomp = "libgomp-1.dll"
elseif Sys.isapple()
    if arch(HostPlatform()) == "aarch64"
        const libgcc_s = "@rpath/libgcc_s.2.dylib"
    else
        const libgcc_s = "@rpath/libgcc_s.1.dylib"
    end
    const libgfortran = string("@rpath/", "libgfortran.", libgfortran_version(HostPlatform()).major, ".dylib")
    const libstdcxx = "@rpath/libstdc++.6.dylib"
    const libgomp = "@rpath/libgomp.1.dylib"
else
    const libgcc_s = "libgcc_s.so.1"
    const libgfortran = string("libgfortran.so.", libgfortran_version(HostPlatform()).major)
    const libstdcxx = "libstdc++.so.6"
    const libgomp = "libgomp.so.1"
end

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
    global libgcc_s_handle = dlopen(libgcc_s)
    global libgcc_s_path = dlpath(libgcc_s_handle)
    global libgfortran_handle = dlopen(libgfortran)
    global libgfortran_path = dlpath(libgfortran_handle)
    global libstdcxx_handle = dlopen(libstdcxx)
    global libstdcxx_path = dlpath(libstdcxx_handle)
    global libgomp_handle = dlopen(libgomp)
    global libgomp_path = dlpath(libgomp_handle)
end

is_available() = true

end  # module CompilerSupportLibraries_jll
