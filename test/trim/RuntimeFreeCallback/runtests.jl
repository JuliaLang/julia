# Verify that the trimmed `RuntimeFreeCallback` executable has its runtime-free callbacks
using Test

outdir = ARGS[1]

@testset "RuntimeFreeCallback" begin
    exe_suffix = splitext(Base.julia_exename())[2]
    exe = joinpath(outdir, "bin", "runtimefreecallback" * exe_suffix)
    @test readchomp(`$exe`) == "36\n13\ntrue"
end
