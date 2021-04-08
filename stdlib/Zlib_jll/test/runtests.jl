# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Zlib_jll

@testset "Zlib_jll" begin
    @test VersionNumber(unsafe_string(ccall((:zlibVersion, libz), Cstring, ()))) == v"1.2.11"
end
