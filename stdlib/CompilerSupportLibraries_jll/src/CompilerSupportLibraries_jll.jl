# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/CompilerSupportLibraries_jll.jl

baremodule CompilerSupportLibraries_jll
using Base, Libdl, Base.BinaryPlatforms
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

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
    global libgcc_s_handle = dlopen(libgcc_s)
    global libgcc_s_path = dlpath(libgcc_s_handle)
    global libgfortran_handle = dlopen(libgfortran)
    global libgfortran_path = dlpath(libgfortran_handle)
    global libstdcxx_handle = dlopen(libstdcxx)
    global libstdcxx_path = dlpath(libstdcxx_handle)
    global libgomp_handle = dlopen(libgomp)
    global libgomp_path = dlpath(libgomp_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    global LIBPATH[] = dirname(libgcc_s_path)
    push!(LIBPATH_list, LIBPATH[])
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libgfortran_path() = libgfortran_path
get_libstdcxx_path() = libstdcxx_path
get_libgomp_path() = libgomp_path

end  # module CompilerSupportLibraries_jll
