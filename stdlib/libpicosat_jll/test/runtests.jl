# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, libpicosat_jll

@testset "libpicosat_jll" begin
    @test libpicosat_jll.is_available()
    @test isa(libpicosat_jll.libpicosat_path, String)
    @test !isempty(libpicosat_jll.libpicosat_path)
end
