using Test

@test length(ARGS) == 1
bindir = dirname(ARGS[1])

let exe_suffix = splitext(Base.julia_exename())[2]

    hello_exe = joinpath(bindir, "hello" * exe_suffix)
    @test readchomp(`$hello_exe arg1 arg2`) == "Hello, world!"
    @test filesize(hello_exe) < 1_900_000

    trimmability_exe = joinpath(bindir, "trimmability" * exe_suffix)
    @test readchomp(`$trimmability_exe arg1 arg2`) == "Hello, world!\n$trimmability_exe\narg1\narg2"

    basic_jll_exe = joinpath(bindir, "basic_jll" * exe_suffix)
    lines = split(readchomp(`$basic_jll_exe`), "\n")
    @test lines[1] == "Julia! Hello, world!"
    @test lines[2] == lines[3]
    @test Base.VersionNumber(lines[2]) ≥ v"1.5.7"
    @test filesize(basic_jll_exe) < filesize(unsafe_string(Base.JLOptions().image_file))/10
end
