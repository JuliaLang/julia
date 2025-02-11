# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, OpenSSL_jll

@testset "OpenSSL_jll" begin
    major = ccall((:OPENSSL_version_major, libcrypto), Cuint, ())
    minor = ccall((:OPENSSL_version_minor, libcrypto), Cuint, ())
    patch = ccall((:OPENSSL_version_patch, libcrypto), Cuint, ())
    @test VersionNumber(major, minor, patch) == v"3.0.16"
end
