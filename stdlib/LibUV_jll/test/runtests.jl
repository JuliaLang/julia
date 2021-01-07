# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LibUV_jll

@testset "LibUV_jll" begin
    vn = VersionNumber(unsafe_string(ccall((:uv_version_string, libuv), Cstring, ())))
    @test vn == v"2.0.0-dev"
end
