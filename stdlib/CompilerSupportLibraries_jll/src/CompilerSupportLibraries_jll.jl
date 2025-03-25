# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/CompilerSupportLibraries_jll.jl

baremodule CompilerSupportLibraries_jll
using Base, Libdl, Base.BinaryPlatforms

export libgfortran, libstdcxx, libgomp, libatomic, libgcc_s

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libgcc_s_path::String = ""
libgfortran_path::String = ""
libstdcxx_path::String = ""
libgomp_path::String = ""

if Sys.iswindows()
    const _libatomic_path = BundledLazyLibraryPath("libatomic-1.dll")
    const _libquadmath_path = BundledLazyLibraryPath("libquadmath-0.dll")
    if arch(HostPlatform()) == "x86_64"
        const _libgcc_s_path = BundledLazyLibraryPath("libgcc_s_seh-1.dll")
    else
        const _libgcc_s_path = BundledLazyLibraryPath("libgcc_s_sjlj-1.dll")
    end
    const _libgfortran_path = BundledLazyLibraryPath(string("libgfortran-", libgfortran_version(HostPlatform()).major, ".dll"))
    const _libstdcxx_path = BundledLazyLibraryPath("libstdc++-6.dll")
    const _libgomp_path = BundledLazyLibraryPath("libgomp-1.dll")
    const _libssp_path = BundledLazyLibraryPath("libssp-0.dll")
elseif Sys.isapple()
    const _libatomic_path = BundledLazyLibraryPath("libatomic.1.dylib")
    const _libquadmath_path = BundledLazyLibraryPath("libquadmath.0.dylib")
    if arch(HostPlatform()) == "aarch64" || libgfortran_version(HostPlatform()) == v"5"
        const _libgcc_s_path = BundledLazyLibraryPath("libgcc_s.1.1.dylib")
    else
        const _libgcc_s_path = BundledLazyLibraryPath("libgcc_s.1.dylib")
    end
    const _libgfortran_path = BundledLazyLibraryPath(string("libgfortran.", libgfortran_version(HostPlatform()).major, ".dylib"))
    const _libstdcxx_path = BundledLazyLibraryPath("libstdc++.6.dylib")
    const _libgomp_path = BundledLazyLibraryPath("libgomp.1.dylib")
    const _libssp_path = BundledLazyLibraryPath("libssp.0.dylib")
else
    if Sys.isfreebsd()
        const _libatomic_path = BundledLazyLibraryPath("libatomic.so.3")
    else
        const _libatomic_path = BundledLazyLibraryPath("libatomic.so.1")
    end
    const _libgcc_s_path = BundledLazyLibraryPath("libgcc_s.so.1")
    const _libgfortran_path = BundledLazyLibraryPath(string("libgfortran.so.", libgfortran_version(HostPlatform()).major))
    const _libstdcxx_path = BundledLazyLibraryPath("libstdc++.so.6")
    const _libgomp_path = BundledLazyLibraryPath("libgomp.so.1")
    if libc(HostPlatform()) != "musl"
        const _libssp_path = BundledLazyLibraryPath("libssp.so.0")
    end
    if arch(HostPlatform()) ∈ ("x86_64", "i686")
        const _libquadmath_path = BundledLazyLibraryPath("libquadmath.so.0")
    end
end

if @isdefined(_libatomic_path)
    const libatomic = LazyLibrary(_libatomic_path)
end
const libgcc_s = LazyLibrary(_libgcc_s_path)
libgfortran_deps = [libgcc_s]
if @isdefined _libquadmath_path
    const libquadmath = LazyLibrary(_libquadmath_path)
    push!(libgfortran_deps, libquadmath)
end
const libgfortran = LazyLibrary(_libgfortran_path, dependencies=libgfortran_deps)
const libstdcxx = LazyLibrary(_libstdcxx_path, dependencies=[libgcc_s])
const libgomp = LazyLibrary(_libgomp_path)
if @isdefined _libssp_path
    const libssp = LazyLibrary(_libssp_path)
end

# Conform to LazyJLLWrappers API
function eager_mode()
    if @isdefined(libatomic)
        dlopen(libatomic)
    end
    dlopen(libgcc_s)
    dlopen(libgomp)
    if @isdefined libquadmath
        dlopen(libquadmath)
    end
    if @isdefined libssp
        dlopen(libssp)
    end
    dlopen(libgfortran)
    dlopen(libstdcxx)
end
is_available() = true

function __init__()
    if @isdefined _libatomic_path
        global libatomic_path = string(_libatomic_path)
    end
    global libgcc_s_path = string(_libgcc_s_path)
    global libgomp_path = string(_libgomp_path)
    if @isdefined _libquadmath_path
        global libquadmath_path = string(_libquadmath_path)
    end
    if @isdefined _libssp_path
        global libssp_path = string(_libssp_path)
    end
    global libgfortran_path = string(_libgfortran_path)
    global libstdcxx_path = string(_libstdcxx_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libgcc_s_path)
    push!(LIBPATH_list, LIBPATH[])
end

end  # module CompilerSupportLibraries_jll
