# Verify the trimmed `BasicJLL` executable links against and uses its JLL
using Test

outdir = ARGS[1]

@testset "BasicJLL" begin
    exe_suffix = splitext(Base.julia_exename())[2]
    basic_jll_exe = joinpath(outdir, "bin", "basicjll" * exe_suffix)
    lines = split(readchomp(`$basic_jll_exe`), "\n")
    @test lines[1] == "Julia! Hello, world!"
    @test lines[2] == lines[3]
    @test Base.VersionNumber(lines[2]) ≥ v"1.5.7"
    @test filesize(basic_jll_exe) < filesize(unsafe_string(Base.JLOptions().image_file))/10
end
