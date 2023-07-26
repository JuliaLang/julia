# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/CompilerSupportLibraries_jll.jl

baremodule CompilerSupportLibraries_jll
using Base, Libdl, Base.BinaryPlatforms
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

export libgcc_s, libgomp, libstdcxx, libquadmath, libgfortran, libssp

if Sys.iswindows()
    if arch(HostPlatform()) == "x86_64"
        const libgcc_s_name = "bin/libgcc_s_seh-1.dll"
    else
        const libgcc_s_name = "bin/libgcc_s_sjlj-1.dll"
    end
    const libgomp_name = "bin/libgomp-1.dll"
    const libstdcxx_name = "bin/libstdc++-6.dll"
    const libquadmath_name = "bin/libquadmath-0.dll"
    const libgfortran_name = string("bin/libgfortran-", libgfortran_version(HostPlatform()).major, ".dll")
    const libssp_name = "bin/libssp-0.dll"
elseif Sys.isapple()
    if arch(HostPlatform()) == "aarch64" || libgfortran_version(HostPlatform()) == v"5"
        const libgcc_s_name = "lib/libgcc_s.1.1.dylib"
    else
        const libgcc_s_name = "lib/libgcc_s.1.dylib"
    end
    const libgomp_name = "lib/libgomp.1.dylib"
    const libstdcxx_name = "lib/libstdc++.6.dylib"
    const libquadmath_name = "lib/libquadmath.0.dylib"
    const libgfortran_name = string("lib/", "libgfortran.", libgfortran_version(HostPlatform()).major, ".dylib")
    const libssp_name = "lib/libssp.0.dylib"
else
    const libgcc_s_name = "lib/libgcc_s.so.1"
    const libgomp_name = "lib/libgomp.so.1"
    const libstdcxx_name = "lib/libstdc++.so.6"
    const libquadmath_name = "lib/libquadmath.so.0"
    const libgfortran_name = string("lib/libgfortran.so.", libgfortran_version(HostPlatform()).major)
    if libc(HostPlatform()) != "musl"
        const libssp_name = "lib/libssp.so.0"
    end
end

const libgcc_s_path = BundledLazyLibraryPath(libgcc_s_name)
const libgomp_path = BundledLazyLibraryPath(libgomp_name)
const libstdcxx_path = BundledLazyLibraryPath(libstdcxx_name)
const libquadmath_path = BundledLazyLibraryPath(libquadmath_name)
const libgfortran_path = BundledLazyLibraryPath(libgfortran_name)
if libc(HostPlatform()) != "musl"
    const libssp_path = BundledLazyLibraryPath(libssp_name)
end

const libgcc_s = LazyLibrary(libgcc_s_path)
const libgomp = LazyLibrary(libgomp_path)
const libstdcxx = LazyLibrary(libstdcxx_path; dependencies=[libgcc_s])
const libquadmath = LazyLibrary(libquadmath_path)
const libgfortran = LazyLibrary(libgfortran_path; dependencies=[libquadmath, libgcc_s])
if libc(HostPlatform()) != "musl"
    const libssp = LazyLibrary(libssp_path)
end

end  # module CompilerSupportLibraries_jll
