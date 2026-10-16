# Verify the trimmed `BasicJLL` executable links against and uses its JLL
using Test

outdir = ARGS[1]

@testset "BasicJLL" begin
    exe_suffix = splitext(Base.julia_exename())[2]
    basic_jll_exe = joinpath(outdir, "bin", "basicjll" * exe_suffix)
    lines = readlines(`$basic_jll_exe`)
    @test lines[1] == "Julia! Hello, world!"
    @test lines[2] == lines[3]
    @test Base.VersionNumber(lines[2]) ≥ v"1.5.7"
    sysimg = unsafe_string(Base.JLOptions().image_file)
    # a split system image keeps its heap in a sibling .ji file
    sysimg_size = filesize(sysimg) + filesize(splitext(sysimg)[1] * ".ji")
    @test filesize(basic_jll_exe) < sysimg_size/10
end
