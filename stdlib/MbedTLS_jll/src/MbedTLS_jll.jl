# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/MbedTLS_jll.jl

baremodule MbedTLS_jll
using Base, Libdl
export libmbedcrypto, libmbedtls, libmbedx509

if Sys.iswindows()
    const libmbedcrypto_name = "bin/libmbedcrypto.dll"
    const libmbedx509_name = "bin/libmbedx509.dll"
    const libmbedtls_name = "bin/libmbedtls.dll"
elseif Sys.isapple()
    const libmbedcrypto_name = "lib/libmbedcrypto.7.dylib"
    const libmbedx509_name = "lib/libmbedx509.1.dylib"
    const libmbedtls_name = "lib/libmbedtls.14.dylib"
else
    const libmbedcrypto_name = "lib/libmbedcrypto.so.7"
    const libmbedx509_name = "lib/libmbedx509.so.1"
    const libmbedtls_name = "lib/libmbedtls.so.14"
end

const libmbedcrypto_path = BundledLazyLibraryPath(libmbedcrypto_name)
const libmbedtls_path = BundledLazyLibraryPath(libmbedtls_name)
const libmbedx509_path = BundledLazyLibraryPath(libmbedx509_name)

const libmbedcrypto = LazyLibrary(libmbedcrypto_path)
const libmbedx509 = LazyLibrary(libmbedx509_path; dependencies=[libmbedcrypto])
const libmbedtls = LazyLibrary(libmbedtls_path; dependencies=[libmbedcrypto, libmbedx509])

end  # module MbedTLS_jll
