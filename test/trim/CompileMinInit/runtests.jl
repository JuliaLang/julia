# Verify the trimmed `CompileMinInit` executable ran its `compile=min` dependency's `__init__`
using Test

outdir = ARGS[1]

@testset "CompileMinInit" begin
    exe_suffix = splitext(Base.julia_exename())[2]
    exe = joinpath(outdir, "bin", "compilemininit" * exe_suffix)
    @test readchomp(`$exe`) == "initialized: true"
end
