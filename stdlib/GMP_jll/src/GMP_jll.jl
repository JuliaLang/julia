# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/GMP_jll.jl
baremodule GMP_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false
export libgmp, libgmpxx

if Sys.iswindows()
    const libgmp_name = "bin/libgmp-10.dll"
    const libgmpxx_name = "bin/libgmpxx-4.dll"
elseif Sys.isapple()
    const libgmp_name = "lib/libgmp.10.dylib"
    const libgmpxx_name = "lib/libgmpxx.4.dylib"
else
    const libgmp_name = "lib/libgmp.so.10"
    const libgmpxx_name = "lib/libgmpxx.so.4"
end

const libgmp_path = BundledLazyLibraryPath(libgmp_name)
const libgmpxx_path = BundledLazyLibraryPath(libgmpxx_name)

const libgmp = LazyLibrary(libgmp_path)
const libgmpxx = LazyLibrary(libgmpxx_path; dependencies=[libgmp])

end  # module GMP_jll
