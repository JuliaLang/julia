
# Tests with no NaNs: isapprox and isapproxn should both pass
for fun in [:isapprox, :isapproxn]
    @eval begin
        # Floating point numbers - basic tests
        @test ($fun)(4.00000000000001, 4.0)
        @test ($fun)(5.0,4.999999999999993)
        @test !($fun)(4.000000002, 4.00300002)

        # Other tolerance levels
        @test ($fun)(4.32, 4.3, 0.1, 0.01)
        @test ($fun)(1.001, 1.002, 0.001, 0.0001)
        @test !($fun)(4.5, 4.9, 0.001, 0.001)

        # Complex numbers
        @test ($fun)(complex(1.0,1.0),complex(1.0,1.00000000000001))
        @test ($fun)(complex(0.9999999999999,1.0),complex(1.0,1.000000000000001))

        # Arrays <-> scalars
        A = [2.000000000001 2.0000000000001 1.99999999999]
        @test ($fun)(A, 2.0)
        @test ($fun)(2.0, A)

        B = [2.0 2.0 2.0]
        C = [1.10000000000000001 1.199999999999999999; 2.09999999999999 2.20000000000001]
        D = [1.1 1.2; 2.1 2.2]

        # Arrays <-> arrays
        @test ($fun)(A, B)
        @test ($fun)(C, D)

    end
end

# Tests at least one NaN: isapproxn and isequaln should both pass
for fun in [:isapproxn, :isequaln]
    @eval begin 
        # scalars
        @test ($fun)(NaN, NaN)
        @test !($fun)(4.0,NaN)
        @test !($fun)(NaN,4.0)

        # arrays
        @test ($fun)([NaN 2.3], [NaN 2.3])
        @test !($fun)([2.3 NaN], [NaN 2.3])
        @test !($fun)([NaN 2.3], [1 2.3])

        # complex scalars
        @test ($fun)(complex(NaN,NaN), complex(NaN,NaN))
        @test ($fun)(complex(NaN,2.3), complex(NaN,2.3))
        @test ($fun)(complex(2.3,NaN), complex(2.3,NaN))
        @test !($fun)(complex(2.3,NaN), complex(NaN,2.3))

        # complex arrays
        @test ($fun)([complex(NaN,NaN) complex(NaN,NaN)], [complex(NaN,NaN) complex(NaN,NaN)])
        @test ($fun)([complex(2.3,NaN) complex(2.3,NaN)], [complex(2.3,NaN) complex(2.3,NaN)])
        @test ($fun)([complex(NaN,2.3) complex(NaN,2.3)], [complex(NaN,2.3) complex(NaN,2.3)])
    end
end

# nan-values in scalars <-> arrays - not relevant for isequaln
@test isapproxn(complex(NaN,NaN), [complex(NaN,NaN) complex(NaN,NaN)])
@test isapproxn([complex(NaN,NaN) complex(NaN,NaN)], complex(NaN,NaN))
@test isapproxn(complex(NaN,2.3), [complex(NaN,2.3) complex(NaN,2.3)])
@test isapproxn(complex(2.3,NaN), [complex(2.3,NaN) complex(2.3,NaN)])
@test isapproxn([complex(NaN,2.3) complex(NaN,2.3)], complex(NaN,2.3))
@test isapproxn([complex(2.3,NaN) complex(2.3,NaN)], complex(2.3,NaN))


# Integers and rationals don't have isapproxn, so we have to test them separately
@test isapprox(4,4)
@test isapprox(4,5,1)
@test !isapprox(4,6,1)
@test !isapprox(4,5)

@test isapprox(1//2+3im, 1//2+3im)
@test isapprox(1//3, 0.33333333333333333)
@test isapprox(1//3, 0.3333, 0.0001, 0.0001)

# Notably missing from this test suite at the moment
# * Tests for other magnitudes of numbers - very small, very big, and combinations of small and big
# * Tests for various odd combinations of types, e.g. isapprox(x::Integer, y::Rational)
