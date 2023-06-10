# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibCURL_jll.jl

baremodule LibCURL_jll
using Base, Libdl, nghttp2_jll, LibSSH2_jll
export libcurl

if Sys.iswindows()
    const libcurl_name = "bin/libcurl-4.dll"
elseif Sys.isapple()
    const libcurl_name = "lib/libcurl.4.dylib"
else
    const libcurl_name = "lib/libcurl.so.4"
end

const libcurl_path = BundledLazyLibraryPath(libcurl_name)
const libcurl = LazyLibrary(libcurl_path; dependencies=[libssh2, libnghttp2],
                            on_load_callback = () -> begin
                                @assert ccall(dlsym(libcurl, :curl_global_init), UInt32, (Clong,), 0x03) == 0
                            end) # eventually add libz

end  # module LibCURL_jll
