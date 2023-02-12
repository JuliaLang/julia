# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LLD_jll

@testset "LLD_jll" begin
    @test isfile(LLD_jll.lld_path)
    flavor = Sys.isapple() ? "darwin" : (Sys.iswindows() ? "link" : "gnu")
    @test success(`$(LLD_jll.lld()) -flavor $flavor --version`)
end
