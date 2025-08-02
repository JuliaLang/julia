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

    # Test that the shared library can be used in a C application
    capplication_exe = joinpath(bindir, "capplication" * exe_suffix)
    lines = split(readchomp(`$capplication_exe`), "\n")
    @test length(lines) == 2
    @test lines[1] == "Sum of copied values: 6.000000"
    @test lines[2] == "Count of same vectors: 1"

    # Test that the logging of entrypoints and types works correctly
    str = read(joinpath(bindir, "bindinginfo_libsimple.log"), String)
    @test occursin("copyto_and_sum(fromto::CVectorPair{Float32})::Float32", str)
    @test occursin(
        """
        CVector{Float32}
          length::Int32[0]
          data::Ptr{Float32}[8]
        16 bytes""", str
    )
    @test occursin(
        """
        CVectorPair{Float32}
          from::CVector{Float32}[0]
          to::CVector{Float32}[16]
        32 bytes""", str
    )
    # ensure that there is a blank line between methods and types
    lines = split(str, '\n'; keepempty=true)
    nblanks = 0
    for line in lines
        nblanks += isempty(line)
        occursin("length", line) && break
    end
    @test nblanks == 1
end
