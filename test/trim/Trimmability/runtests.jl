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
    @test lines[11] == "got 1:3 vs 1 with 2.5 and sym and c"
    @test lines[12] == "sparse constructors: 3 5 2 3.0"
    @test lines[13] == "sparse products: 26.0 26.0 4 16"
    @test lines[14] == "sparse structure: 4 6.0 4 4"
    @test lines[15] == "sparse broadcast: 8 19.0 4 16"
    @test lines[16] == "sparse vectors: 40.0 2 6.0 10.0"
    @test lines[17] == "sparse reductions: 10.0 5.0 6.0 5.477225575051661"
    @test lines[18] == "sparse nested reductions: 7.0 5.0 100.0"
    # TODO(#62912): SuiteSparse libraries cannot be loaded under --trim yet
    # @test lines[19] == "sparse solves: -1.0 -1.0 -1.0"
    # @test lines[20] == "sparse factorizations: 0.666667 0.666667 -1.0"
end
