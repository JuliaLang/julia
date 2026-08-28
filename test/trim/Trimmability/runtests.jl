# Verify the trimmed `Trimmability` executable exercises a range of constructs
using Test

outdir = ARGS[1]

@testset "Trimmability" begin
    exe_suffix = splitext(Base.julia_exename())[2]
    trimmability_exe = joinpath(outdir, "bin", "trimmability" * exe_suffix)
    # Disable codegen so that any silent fallback to the JIT fails loudly.
    lines = readlines(addenv(`$trimmability_exe arg1 arg2`, "JULIA_LOAD_CODEGEN_LIB" => "0"))
    @test lines[1] == "Hello, world!"
    @test lines[2] == trimmability_exe
    @test lines[3] == "arg1"
    @test lines[4] == "arg2"
    @test lines[5] == string(4.0+pi)
    @test parse(Float64, lines[6]) isa Float64
    @test lines[7] == "Version: 1.1.0"
    @test lines[8] == "# preferences: 0"
    @test lines[9] == "finalizers: 27 32"
    @test lines[10] == "collected: 0 kept, 10 dropped"
    @test lines[11] == "static_lib_ccall: $(VERSION.major)"
end
