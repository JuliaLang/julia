# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestRowVector

using Test, LinearAlgebra, Random
using LinearAlgebra: dot

@testset "Issue 24590" begin
    a = RowVector([1; 2; 3; 4])
    b = RowVector([2; 3; 6; 10])
    @test dot(a, b) == 66
end

end # module TestRowVector
