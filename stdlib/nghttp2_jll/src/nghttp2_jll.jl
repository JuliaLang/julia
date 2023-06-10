# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/nghttp2_jll.jl
baremodule nghttp2_jll
using Base, Libdl
export libnghttp2

if Sys.iswindows()
    const libnghttp2_name = "bin/libnghttp2-14.dll"
elseif Sys.isapple()
    const libnghttp2_name = "lib/libnghttp2.14.dylib"
else
    const libnghttp2_name = "lib/libnghttp2.so.14"
end

const libnghttp2_path = BundledLazyLibraryPath(libnghttp2_name)
const libnghttp2 = LazyLibrary(libnghttp2_path)

end  # module nghttp2_jll
