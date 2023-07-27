# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/MPFR_jll.jl
baremodule MPFR_jll
using Base, Libdl, GMP_jll
Base.Experimental.@compiler_options compile=min optimize=0 infer=false
export libmpfr

if Sys.iswindows()
    const libmpfr_name = "bin/libmpfr-6.dll"
elseif Sys.isapple()
    const libmpfr_name = "lib/libmpfr.6.dylib"
else
    const libmpfr_name = "lib/libmpfr.so.6"
end

const libmpfr_path = BundledLazyLibraryPath(libmpfr_name)
const libmpfr = LazyLibrary(libmpfr_path; dependencies=[libgmp])
end  # module MPFR_jll
