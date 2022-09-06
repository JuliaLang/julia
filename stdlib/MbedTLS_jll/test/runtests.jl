# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, MbedTLS_jll

@testset "MbedTLS_jll" begin
    vstr = zeros(UInt8, 32)
    ccall((:mbedtls_version_get_string, libmbedcrypto), Cvoid, (Ref{UInt8},), vstr)
    vn = VersionNumber(unsafe_string(pointer(vstr)))
    @test vn == v"2.28.0"
end
