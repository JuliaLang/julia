# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/PCRE2_jll.jl
baremodule PCRE2_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false


export libpcre2_8

if Sys.iswindows()
    const libpcre2_8_name = "bin/libpcre2-8-0.dll"
elseif Sys.isapple()
    const libpcre2_8_name = "lib/libpcre2-8.0.dylib"
else
    const libpcre2_8_name = "lib/libpcre2-8.so.0"
end

const libpcre2_8_path = BundledLazyLibraryPath(libpcre2_8_name)
const libpcre2_8 = LazyLibrary(libpcre2_8_path)

end  # module PCRE2_jll
