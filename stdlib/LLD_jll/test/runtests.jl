# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LLD_jll

@testset "LLD_jll" begin
    @test isfile(LLD_jll.lld_path)
    @test success(`$(LLD_jll.lld()) --version`)
end
