# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Zstd_jll

@testset "Zstd_jll" begin
    @test ccall((:ZSTD_versionNumber, libzstd), Cuint, ()) == 1_05_07
end
