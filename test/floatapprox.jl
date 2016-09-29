# This file is a part of Julia. License is MIT: http://julialang.org/license

# Floating point numbers - basic tests
@test 4.00000000000001 ≈ 4.0
@test 5.0 ≈ 4.999999999999993
@test 4.000000002 ≉ 4.00300002

# Other tolerance levels
@test isapprox(4.32, 4.3; rtol=0.1, atol=0.01)
@test isapprox(1.001, 1.002; rtol=0.001, atol=0.0001)
@test !isapprox(4.5, 4.9; rtol=0.001, atol=0.001)

# Complex numbers
@test 1.0 + 1.0im ≈ 1.0 + 1.00000000000001im
@test 0.9999999999999 + 1.0im ≈ 1.0 + 1.000000000000001im
@test isapprox(0.9999 + 1.0im, 1.0 + 1.1im; rtol = 0.0001, atol=1.1)

# Complex <-> reals
@test 1.0 + 0im ≈ 1.0000000000001
@test isapprox(0.9999999999999, 1.0 + 0im)
@test !isapprox(1.0+1im, 1.000000000000001)

# Comparing NaNs
@test 4.0 ≉ NaN
@test NaN ≉ 4.0
@test complex(2.3,NaN) ≉ complex(NaN,2.3)
@test NaN ≉ NaN
@test complex(NaN,NaN) ≉ complex(NaN,NaN)
@test complex(NaN,2.3) ≉ complex(NaN,2.3)
@test complex(2.3,NaN) ≉ complex(2.3,NaN)

# Comparing Infs
@test Inf ≈ Inf
@test Inf ≉ 1
@test Inf ≉ -Inf
@test complex(0.0,Inf) ≈ complex(0.0,Inf)
@test complex(0.0,Inf) ≉ complex(0.0,-Inf)

# Tests for integers and rationals
@test isapprox(4,4)
@test isapprox(4,5; atol=1)
@test !isapprox(4,6; atol=1)
@test !isapprox(4,5)

@test isapprox(1//2+3im, 1//2+3im)
@test isapprox(1//3, 0.33333333333333333)
@test isapprox(1//3, 0.3333; rtol=0.0001, atol=0.0001)
@test isapprox(1+1im, 1im+1)

# Notably missing from this test suite at the moment
# * Tests for other magnitudes of numbers - very small, very big, and combinations of small and big
# * Tests for various odd combinations of types, e.g. isapprox(x::Integer, y::Rational)

# issue #12375:
@test !isapprox(1e17, 1)

# Tests for arrays:
@test [1,2,3] ≈ [1,2,3+1e-9]
@test [0,1] ≈ [1e-9, 1]
@test [0,Inf] ≈ [0,Inf]
@test [0,Inf] ≉ [0,-Inf]

