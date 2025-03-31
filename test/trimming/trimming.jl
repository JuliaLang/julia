using Test

let exe_suffix = splitext(Base.julia_exename())[2]

    hello_exe = joinpath(@__DIR__, "hello" * exe_suffix)
    @test readchomp(`$hello_exe`) == "Hello, world!"
    @test filesize(hello_exe) < 20_000_000

    basic_jll_exe = joinpath(@__DIR__, "basic_jll" * exe_suffix)
    lines = split(readchomp(`$basic_jll_exe`), "\n")
    @test lines[1] == "Julia! Hello, world!"
    @test lines[2] == lines[3]
    @test Base.VersionNumber(lines[2]) ≥ v"1.5.7"
    @test filesize(basic_jll_exe) < filesize(unsafe_string(Base.JLOptions().image_file))/10
end
