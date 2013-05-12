
# Tests where isapprox and isapproxn should return the same things
for fun in [:isapprox, :isapproxn]
    @eval begin
        # Floating point numbers - basic tests
        @test ($fun)(4.00000000000001, 4.0)
        @test ($fun)(5.0,4.999999999999993)
        @test !($fun)(4.000000002, 4.00300002)

        # Other tolerance levels
        @test ($fun)(4.32, 4.3; rtol=0.1, atol=0.01)
        @test ($fun)(1.001, 1.002; rtol=0.001, atol=0.0001)
        @test !($fun)(4.5, 4.9; rtol=0.001, atol=0.001)

        # Complex numbers
        @test ($fun)(1.0 + 1.0im, 1.0 + 1.00000000000001im)
        @test ($fun)(0.9999999999999 + 1.0im, 1.0 + 1.000000000000001im)
        @test ($fun)(0.9999 + 1.0im, 1.0 + 1.1im; rrtol = 0.0001, iatol=1.1)

        # Complex <-> reals
        @test ($fun)(1.0 + 0im, 1.0000000000001)
        @test ($fun)(0.9999999999999, 1.0 + 0im)
        @test !($fun)(1.0+1im, 1.000000000000001)

        # Comparing NaN with non-NaN values
        @test !($fun)(4.0,NaN)
        @test !($fun)(NaN,4.0)
        @test !($fun)(complex(2.3,NaN), complex(NaN,2.3))

    end
end

# Tests where isapprox and isapproxn should return differently
# scalars
@test isapproxn(NaN, NaN)
@test !isapprox(NaN, NaN)

# complex numbers
@test isapproxn(complex(NaN,NaN), complex(NaN,NaN))
@test !isapprox(complex(NaN,NaN), complex(NaN,NaN))
@test isapproxn(complex(NaN,2.3), complex(NaN,2.3))
@test !isapprox(complex(NaN,2.3), complex(NaN,2.3))
@test isapproxn(complex(2.3,NaN), complex(2.3,NaN))
@test !isapprox(complex(2.3,NaN), complex(2.3,NaN))
@test !isapproxn(complex(2.3,NaN), complex(NaN,2.3))

# Integers and rationals don't have isapproxn, so we have to test them separately
@test isapprox(4,4)
@test isapprox(4,5; tol=1)
@test !isapprox(4,6; tol=1)
@test !isapprox(4,5)

@test isapprox(1//2+3im, 1//2+3im)
@test isapprox(1//3, 0.33333333333333333)
@test isapprox(1//3, 0.3333; rtol=0.0001, atol=0.0001)

# Notably missing from this test suite at the moment
# * Tests for other magnitudes of numbers - very small, very big, and combinations of small and big
# * Tests for various odd combinations of types, e.g. isapprox(x::Integer, y::Rational)
