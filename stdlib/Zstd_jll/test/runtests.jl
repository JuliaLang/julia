# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Zstd_jll

@testset "Zstd_jll" begin
    let version = ccall((:ZSTD_versionNumber, libzstd), Cuint, ())
        @test 1_00_00 <= version < 2_00_00
    end
    let version = read(`$(zstd()) --version`, String)
        @test contains(version, "Zstandard CLI")
    end
end
