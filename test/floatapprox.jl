# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "Floating point numbers - basic tests" begin
    @test 4.00000000000001 ≈ 4.0
    @test 5.0 ≈ 4.999999999999993
    @test 4.000000002 ≉ 4.00300002
end
@testset "Other tolerance levels" begin
    @test 4.32 ≈ 4.3 rtol=0.1 atol=0.01
    @test 1.001 ≈ 1.002 rtol=0.001 atol=0.0001
    @test 4.5 ≉ 4.9 rtol=0.001 atol=0.001
end
@testset "Complex numbers" begin
    @test 1.0 + 1.0im ≈ 1.0 + 1.00000000000001im
    @test 0.9999999999999 + 1.0im ≈ 1.0 + 1.000000000000001im
    @test 0.9999 + 1.0im ≈ 1.0 + 1.1im rtol=0.0001 atol=1.1
end
@testset "Complex <-> reals" begin
    @test 1.0 + 0im ≈ 1.0000000000001
    @test 0.9999999999999 ≈ 1.0 + 0im
    @test 1.0+1im ≉ 1.000000000000001
end
@testset "Comparing NaNs" begin
    @test 4.0 ≉ NaN
    @test NaN ≉ 4.0
    @test complex(2.3,NaN) ≉ complex(NaN,2.3)
    @test NaN ≉ NaN
    @test complex(NaN,NaN) ≉ complex(NaN,NaN)
    @test complex(NaN,2.3) ≉ complex(NaN,2.3)
    @test complex(2.3,NaN) ≉ complex(2.3,NaN)
end
@testset "Comparing Infs" begin
    @test Inf ≈ Inf
    @test Inf ≉ 1
    @test Inf ≉ -Inf
    @test complex(0.0,Inf) ≈ complex(0.0,Inf)
    @test complex(0.0,Inf) ≉ complex(0.0,-Inf)
end
@testset "integers and rationals" begin
    @test 4 ≈ 4
    @test 4 ≈ 5 atol=1
    @test 4 ≉ 6 atol=1
    @test 4 ≉ 5

    @test 1//2+3im ≈ 1//2+3im
    @test 1//3 ≈ 0.33333333333333333
    @test 1//3 ≈ 0.3333 rtol=0.0001 atol=0.0001
    @test 1+1im ≈ 1im+1
end
# Notably missing from this test suite at the moment
# * Tests for other magnitudes of numbers - very small, very big, and combinations of small and big
# * Tests for various odd combinations of types, e.g. x::Integer ≈ y::Rational

# issue #12375:
@test 1e17 ≉ 1

@testset "Tests for arrays:" begin
    @test [1,2,3] ≈ [1,2,3+1e-9]
    @test [0,1] ≈ [1e-9, 1]
    @test [0,Inf] ≈ [0,Inf]
    @test [0,Inf] ≉ [0,-Inf]
end
@testset "issue #19936" begin
    for elty in (Float16,Float32,Float64)
        nan  = elty(NaN)
        half = elty(0.5)
        @test nan ≉ nan
        @test nan ≈ nan nans=true
        @test [half, nan, half] ≉ [half, nan, half]
        @test [half, nan, half] ≈ [half, nan, half] nans=true
    end
end
