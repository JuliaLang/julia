# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/dSFMT_jll.jl

baremodule dSFMT_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

export libdSFMT

if Sys.iswindows()
    const libdSFMT_name = "bin/libdSFMT.dll"
elseif Sys.isapple()
    const libdSFMT_name = "lib/libdSFMT.dylib"
else
    const libdSFMT_name = "lib/libdSFMT.so"
end
const libdSFMT_path = BundledLazyLibraryPath(libdSFMT_name)
const libdSFMT = LazyLibrary(libdSFMT_path)

end  # module dSFMT_jll
