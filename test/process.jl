# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "run(``) tests" begin
    @test_throws ArgumentError run(Base.Cmd(``))
    @test_throws ArgumentError run(Base.AndCmds(``, ``))
    @test_throws ArgumentError run(Base.AndCmds(``, `echo "test"`))
end
