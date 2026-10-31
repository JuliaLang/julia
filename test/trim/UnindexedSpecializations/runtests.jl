# Verify that empty `Memory{Any}` objects survive trimming (see src/)
using Test

outdir = ARGS[1]

@testset "UnindexedSpecializations" begin
    exe_suffix = splitext(Base.julia_exename())[2]
    exe = joinpath(outdir, "bin", "unindexedspecializations" * exe_suffix)
    @test readlines(`$exe`) == ["2 0", "2 true false"]
end
