# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/CompilerSupportLibraries_jll.jl

baremodule CompilerSupportLibraries_jll
using Base, Libdl, Base.BinaryPlatforms

const PATH_list = String[]
const LIBPATH_list = String[]

export libgfortran, libstdcxx, libgomp

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir::String = ""
libatomic_handle::Ptr{Cvoid} = C_NULL
libatomic_path::String = ""
libgcc_s_handle::Ptr{Cvoid} = C_NULL
libgcc_s_path::String = ""
libgfortran_handle::Ptr{Cvoid} = C_NULL
libgfortran_path::String = ""
libstdcxx_handle::Ptr{Cvoid} = C_NULL
libstdcxx_path::String = ""
libgomp_handle::Ptr{Cvoid} = C_NULL
libgomp_path::String = ""

if Sys.iswindows() || Sys.isapple() || arch(HostPlatform()) ∈ ("x86_64", "i686")
    libquadmath_handle::Ptr{Cvoid} = C_NULL
    libquadmath_path::String = ""
end

if Sys.iswindows()
    if arch(HostPlatform()) == "x86_64"
        const libgcc_s = "libgcc_s_seh-1.dll"
    else
        const libgcc_s = "libgcc_s_sjlj-1.dll"
    end
    const libatomic = "libatomic-1.dll"
    const libquadmath = "libquadmath-0.dll"
    const libgfortran = string("libgfortran-", libgfortran_version(HostPlatform()).major, ".dll")
    const libstdcxx = "libstdc++-6.dll"
    const libgomp = "libgomp-1.dll"
    const libssp = "libssp-0.dll"
elseif Sys.isapple()
    if arch(HostPlatform()) == "aarch64" || libgfortran_version(HostPlatform()) == v"5"
        const libgcc_s = "@rpath/libgcc_s.1.1.dylib"
    else
        const libgcc_s = "@rpath/libgcc_s.1.dylib"
    end
    const libatomic = "@rpath/libatomic.1.dylib"
    const libquadmath = "@rpath/libquadmath.0.dylib"
    const libgfortran = string("@rpath/", "libgfortran.", libgfortran_version(HostPlatform()).major, ".dylib")
    const libstdcxx = "@rpath/libstdc++.6.dylib"
    const libgomp = "@rpath/libgomp.1.dylib"
    const libssp = "@rpath/libssp.0.dylib"
else
    const libgcc_s = "libgcc_s.so.1"
    const libatomic = Sys.isfreebsd() ? "libatomic.so.3" : "libatomic.so.1"
    if arch(HostPlatform()) ∈ ("x86_64", "i686")
        const libquadmath = "libquadmath.so.0"
    end
    const libgfortran = string("libgfortran.so.", libgfortran_version(HostPlatform()).major)
    const libstdcxx = "libstdc++.so.6"
    const libgomp = "libgomp.so.1"
    if libc(HostPlatform()) != "musl"
        const libssp = "libssp.so.0"
    end
end

function __init__()
    global libgcc_s_handle = dlopen(libgcc_s)
    global libgcc_s_path = dlpath(libgcc_s_handle)
    # `libatomic` may not be present in stripped-down installations; tolerate failure.
    global libatomic_handle = dlopen(libatomic; throw_error = false)
    if libatomic_handle != C_NULL
        global libatomic_path = dlpath(libatomic_handle)
    end
    @static if @isdefined(libquadmath)
        global libquadmath_handle = dlopen(libquadmath; throw_error = false)
        if libquadmath_handle != C_NULL
            global libquadmath_path = dlpath(libquadmath_handle)
        end
    end
    global libgfortran_handle = dlopen(libgfortran)
    global libgfortran_path = dlpath(libgfortran_handle)
    global libstdcxx_handle = dlopen(libstdcxx)
    global libstdcxx_path = dlpath(libstdcxx_handle)
    global libgomp_handle = dlopen(libgomp)
    global libgomp_path = dlpath(libgomp_handle)
    @static if libc(HostPlatform()) != "musl"
        dlopen(libssp; throw_error = false)
    end
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libgcc_s_path)
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
