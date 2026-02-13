# Run all tests in sandbox/tests/
using Test

const testdir = joinpath(@__DIR__, "tests")

@testset "Sandbox Tests" begin
    for f in sort(readdir(testdir))
        endswith(f, ".jl") || continue
        @testset "$f" begin
            include(joinpath(testdir, f))
        end
    end
end
