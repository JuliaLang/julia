using Test

@test length(ARGS) == 1
bindir = dirname(ARGS[1])

let exe_suffix = splitext(Base.julia_exename())[2]

    hello_exe = joinpath(bindir, "hello" * exe_suffix)
    @test readchomp(`$hello_exe arg1 arg2`) == "Hello, world!\n$hello_exe\narg1\narg2"
    @test filesize(hello_exe) < 2_000_000

    basic_jll_exe = joinpath(bindir, "basic_jll" * exe_suffix)
    lines = split(readchomp(`$basic_jll_exe`), "\n")
    @test lines[1] == "Julia! Hello, world!"
    @test lines[2] == lines[3]
    @test Base.VersionNumber(lines[2]) ≥ v"1.5.7"
    @test filesize(basic_jll_exe) < filesize(unsafe_string(Base.JLOptions().image_file))/10

    str = read(joinpath(bindir, "bindinginfo_simplelib.log"), String)
    @test occursin("copyto_and_sum(::CVectorPair{Float32})::Float32", str)
    @test occursin(
        """
        _CVector_Float32_
          length::Int32[0]
          data::Ptr{Float32}[8]
        16 bytes""", str
    )
    @test occursin(
        """
        _CVectorPair_Float32_
          from::_CVector_Float32_[0]
          to::_CVector_Float32_[16]
        32 bytes""", str
    )
end
