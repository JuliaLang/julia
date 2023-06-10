# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LibSSH2_jll.jl

baremodule LibSSH2_jll
using Base, Libdl, MbedTLS_jll
export libssh2

if Sys.iswindows()
    const libssh2_name = "bin/libssh2.dll"
elseif Sys.isapple()
    const libssh2_name = "lib/libssh2.1.dylib"
else
    const libssh2_name = "lib/libssh2.so.1"
end

const libssh2_path = BundledLazyLibraryPath(libssh2_name)
const libssh2 = LazyLibrary(libssh2_path; dependencies=[libmbedx509, libmbedcrypto, libmbedtls])

end  # module LibSSH2_jll
