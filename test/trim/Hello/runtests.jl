# Verify the trimmed `Hello` executable runs and stays small
using Test

outdir = ARGS[1]

@testset "Hello" begin
    exe_suffix = splitext(Base.julia_exename())[2]
    hello_exe = joinpath(outdir, "bin", "hello" * exe_suffix)
    @test readchomp(`$hello_exe arg1 arg2`) == "Hello, world!"
    @test filesize(hello_exe) < 1_900_000
end
