# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base: checked_abs, checked_neg, checked_add, checked_sub, checked_mul,
             checked_div, checked_rem, checked_fld, checked_mod, checked_cld

# checked operations

for T in (Int8, Int16, Int32, Int64, Int128)
    # regular cases
    @test checked_abs(T(0)) === T(0)
    @test checked_neg(T(0)) === T(0)
    @test checked_add(T(0)) === T(0)
    @test checked_mul(T(0)) === T(0)

    for s in (-1, +1)
        @test checked_abs(T(3s)) === T(abs(3s))
        @test checked_neg(T(3s)) === T(-3s)
        @test checked_add(T(3s)) === T(3s)
        @test checked_mul(T(3s)) === T(3s)
        @test checked_abs(T(s*typemax(T))) === typemax(T)
        @test checked_neg(T(s*typemax(T))) === T(-s*typemax(T))
        @test checked_add(T(s*typemax(T))) === T(s*typemax(T))
        @test checked_mul(T(s*typemax(T))) === T(s*typemax(T))
    end

    # corner cases
    @test_throws OverflowError checked_abs(typemin(T))
    @test_throws OverflowError checked_neg(typemin(T))

    # regular cases
    for s1 in (-1, +1), s2 in (-1, +1)
        @test checked_add(T(4s1), T(3s2)) === T(4s1 + 3s2)
        @test checked_sub(T(4s1), T(3s2)) === T(4s1 - 3s2)
        @test checked_mul(T(4s1), T(3s2)) === T(4s1 * 3s2)
        @test checked_div(T(4s1), T(3s2)) === T(div(4s1, 3s2))
        @test checked_rem(T(4s1), T(3s2)) === T(rem(4s1, 3s2))
        @test checked_fld(T(4s1), T(3s2)) === T(fld(4s1, 3s2))
        @test checked_mod(T(4s1), T(3s2)) === T(mod(4s1, 3s2))
        @test checked_cld(T(4s1), T(3s2)) === T(cld(4s1, 3s2))
    end

    # corner cases
    halfmax = T(typemax(T)÷2)
    halfmax_plus1 = T(halfmax+1)
    halfmin = T(typemin(T)÷2)
    halfmin_minus1 = T(halfmin-1)
    sqrtmax = T(1) << T(sizeof(T)*4)
    half_sqrtmax = sqrtmax >> T(1)
    half_sqrtmax_plus1 = half_sqrtmax + T(1)

    @test checked_add(typemax(T), T(-1)) === T(typemax(T) - 1)
    @test_throws OverflowError checked_add(typemin(T), T(-1))
    @test checked_add(typemax(T), T(0)) === typemax(T)
    @test checked_add(typemin(T), T(0)) === typemin(T)
    @test_throws OverflowError checked_add(typemax(T), T(1))
    @test checked_add(typemin(T), T(1)) === T(typemin(T) + 1)
    @test checked_add(T(-1), typemax(T)) === T(typemax(T) - 1)
    @test_throws OverflowError checked_add(T(-1), typemin(T))
    @test checked_add(T(0), typemax(T)) === typemax(T)
    @test checked_add(T(0), typemin(T)) === typemin(T)
    @test_throws OverflowError checked_add(T(1), typemax(T))
    @test checked_add(T(1), typemin(T)) === T(typemin(T) + 1)
    @test checked_add(typemax(T), typemin(T)) === T(-1)
    @test checked_add(typemin(T), typemax(T)) === T(-1)
    @test_throws OverflowError checked_add(halfmax_plus1, halfmax_plus1)
    @test_throws OverflowError checked_add(halfmin, halfmin_minus1)

    @test_throws OverflowError checked_sub(typemax(T), T(-1))
    @test checked_sub(typemax(T), T(0)) === typemax(T)
    @test checked_sub(typemax(T), T(1)) === T(typemax(T) - 1)
    @test checked_sub(typemin(T), T(-1)) === T(typemin(T) + 1)
    @test checked_sub(typemin(T), T(0)) === typemin(T)
    @test_throws OverflowError checked_sub(typemin(T), T(1))
    @test checked_sub(T(0), typemax(T)) === T(typemin(T) + 1)
    @test checked_sub(T(1), typemax(T)) === T(typemin(T) + 2)
    @test checked_sub(T(-1), typemin(T)) === typemax(T)
    @test_throws OverflowError checked_sub(T(0), typemin(T))
    @test checked_sub(typemax(T), typemax(T)) === T(0)
    @test checked_sub(typemin(T), typemin(T)) === T(0)
    @test checked_sub(halfmax, T(-halfmin)) === T(-1)
    @test_throws OverflowError checked_sub(halfmin, T(-halfmin_minus1))

    @test checked_mul(typemax(T), T(0)) === T(0)
    @test checked_mul(typemin(T), T(0)) === T(0)
    @test checked_mul(typemax(T), T(1)) === typemax(T)
    @test checked_mul(typemin(T), T(1)) === typemin(T)
    @test_throws OverflowError checked_mul(sqrtmax, half_sqrtmax)
    @test checked_mul(sqrtmax, -half_sqrtmax) === T(sqrtmax * -half_sqrtmax)
    @test_throws OverflowError checked_mul(sqrtmax, -half_sqrtmax_plus1)
    @test checked_mul(-sqrtmax, half_sqrtmax) === T(-sqrtmax * half_sqrtmax)
    @test_throws OverflowError checked_mul(-sqrtmax, half_sqrtmax_plus1)
    @test_throws OverflowError checked_mul(-sqrtmax, -half_sqrtmax)

    @test checked_div(typemax(T), T(1)) === typemax(T)
    @test_throws DivideError checked_div(typemax(T), T(0))
    @test checked_div(typemax(T), T(-1)) === T(-typemax(T))
    @test checked_div(typemin(T), T(1)) === typemin(T)
    @test_throws DivideError checked_div(typemin(T), T(0))
    @test_throws DivideError checked_div(typemin(T), T(-1))
    @test checked_rem(typemax(T), T(1)) === T(0)
    @test_throws DivideError checked_rem(typemax(T), T(0))
    @test checked_rem(typemax(T), T(-1)) === T(0)
    @test checked_rem(typemin(T), T(1)) === T(0)
    @test_throws DivideError checked_rem(typemin(T), T(0))
    @test checked_rem(typemin(T), T(-1)) === T(0)
    @test checked_fld(typemax(T), T(1)) === typemax(T)
    @test_throws DivideError checked_fld(typemax(T), T(0))
    @test checked_fld(typemax(T), T(-1)) === T(-typemax(T))
    @test checked_fld(typemin(T), T(1)) === typemin(T)
    @test_throws DivideError checked_fld(typemin(T), T(0))
    @test_throws DivideError checked_fld(typemin(T), T(-1))
    @test checked_mod(typemax(T), T(1)) === T(0)
    @test_throws DivideError checked_mod(typemax(T), T(0))
    @test checked_mod(typemax(T), T(-1)) === T(0)
    @test checked_mod(typemin(T), T(1)) === T(0)
    @test_throws DivideError checked_mod(typemin(T), T(0))
    @test checked_mod(typemin(T), T(-1)) === T(0)
    @test_throws DivideError checked_cld(typemax(T), T(0))
    @test checked_cld(typemax(T), T(-1)) === T(-typemax(T))
    @test checked_cld(typemin(T), T(1)) === typemin(T)
    @test_throws DivideError checked_cld(typemin(T), T(0))
    @test_throws DivideError checked_cld(typemin(T), T(-1))
end

for T in (UInt8, UInt16, UInt32, UInt64, UInt128)
    # regular cases
    @test checked_abs(T(0)) === T(0)
    @test checked_neg(T(0)) === T(0)
    @test checked_add(T(0)) === T(0)
    @test checked_mul(T(0)) === T(0)

    @test checked_abs(T(3)) === T(3)
    @test_throws OverflowError checked_neg(T(3))
    @test checked_add(T(3)) === T(3)
    @test checked_mul(T(3)) === T(3)

    # regular cases
    @test checked_add(T(4), T(3)) === T(7)
    @test checked_sub(T(4), T(3)) === T(1)
    @test checked_mul(T(4), T(3)) === T(12)

    # corner cases
    halfmax = T(typemax(T)÷2)
    halfmax_plus1 = T(halfmax+1)
    sqrtmax = T(1) << T(sizeof(T)*4)

    @test checked_add(typemax(T), T(0)) === typemax(T)
    @test checked_add(T(0), T(0)) === T(0)
    @test_throws OverflowError checked_add(typemax(T), T(1))
    @test checked_add(T(0), T(1)) === T(T(0) + 1)
    @test checked_add(T(0), typemax(T)) === typemax(T)
    @test checked_add(T(0), T(0)) === T(0)
    @test_throws OverflowError checked_add(T(1), typemax(T))
    @test checked_add(T(1), T(0)) === T(T(0) + 1)
    @test checked_add(typemax(T), T(0)) === typemax(T)
    @test checked_add(T(0), typemax(T)) === typemax(T)
    @test_throws OverflowError checked_add(halfmax_plus1, halfmax_plus1)

    @test checked_sub(typemax(T), T(0)) === typemax(T)
    @test checked_sub(typemax(T), T(1)) === T(typemax(T) - 1)
    @test checked_sub(T(0), T(0)) === T(0)
    @test_throws OverflowError checked_sub(T(0), T(1))
    @test_throws OverflowError checked_sub(T(0), typemax(T))
    @test_throws OverflowError checked_sub(T(1), typemax(T))
    @test checked_sub(T(0), T(0)) === T(0)
    @test checked_sub(typemax(T), typemax(T)) === T(0)

    @test checked_mul(typemax(T), T(0)) === T(0)
    @test checked_mul(T(0), T(0)) === T(0)
    @test checked_mul(typemax(T), T(1)) === typemax(T)
    @test checked_mul(T(0), T(1)) === T(0)
    @test_throws OverflowError checked_mul(sqrtmax, sqrtmax)

    @test checked_div(typemax(T), T(1)) === typemax(T)
    @test_throws DivideError checked_div(typemax(T), T(0))
    @test checked_rem(typemax(T), T(1)) === T(0)
    @test_throws DivideError checked_rem(typemax(T), T(0))
    @test checked_fld(typemax(T), T(1)) === typemax(T)
    @test_throws DivideError checked_fld(typemax(T), T(0))
    @test checked_mod(typemax(T), T(1)) === T(0)
    @test_throws DivideError checked_mod(typemax(T), T(0))
    @test checked_cld(typemax(T), T(1)) === typemax(T)
    @test_throws DivideError checked_cld(typemax(T), T(0))
end

# Boolean
@test checked_add(false) === 0
@test checked_add(true) === 1
@test checked_sub(false) === 0
@test checked_sub(true) === -1
@test checked_neg(false) === 0
@test checked_neg(true) === -1
@test checked_abs(true) === true
@test checked_abs(false) === false
@test checked_mul(false) === false
@test checked_mul(true) === true

@test checked_add(true, true) === 2
@test checked_add(true, false) === 1
@test checked_add(false, false) === 0
@test checked_add(false, true) === 1

@test checked_sub(true, true) === 0
@test checked_sub(true, false) === 1
@test checked_sub(false, false) === 0
@test checked_sub(false, true) === -1

@test checked_mul(true, false) === false
@test checked_mul(false, false) === false
@test checked_mul(true, true) === true
@test checked_mul(false, true) === false

@test checked_div(true, true) === true
@test checked_div(false, true) === false
@test_throws DivideError checked_div(true, false)
@test checked_rem(true, true) === false
@test checked_rem(false, true) === false
@test_throws DivideError checked_rem(true, false)
@test checked_fld(true, true) === true
@test checked_fld(false, true) === false
@test_throws DivideError checked_fld(true, false)
@test checked_mod(true, true) === false
@test checked_mod(false, true) === false
@test_throws DivideError checked_mod(true, false)
@test checked_cld(true, true) === true
@test checked_cld(false, true) === false
@test_throws DivideError checked_cld(true, false)

# BigInt
@test checked_abs(BigInt(-1)) == BigInt(1)
@test checked_abs(BigInt(1)) == BigInt(1)
@test checked_neg(BigInt(-1)) == BigInt(1)
@test checked_neg(BigInt(1)) == BigInt(-1)

# Additional tests

@test checked_sub(UInt(4), UInt(3)) === UInt(1)
@test_throws OverflowError checked_sub(UInt(5), UInt(6))
@test checked_mul(UInt(4), UInt(3)) === UInt(12)

@test checked_sub(Int128(-1),Int128(-2)) === Int128(1)

@test_throws OverflowError checked_mul(UInt32(2)^30, UInt32(2)^2)
@test_throws OverflowError checked_mul(UInt64(2)^62, UInt64(2)^2)

@test checked_add(UInt128(1), UInt128(2)) === UInt128(3)
@test_throws OverflowError checked_add(UInt128(2)^127, UInt128(2)^127)

@test checked_sub(UInt128(2), UInt128(1)) === UInt128(1)
@test_throws OverflowError checked_sub(UInt128(3), UInt128(4))

@test checked_mul(UInt128(3), UInt128(4)) === UInt128(12)
@test_throws OverflowError checked_mul(UInt128(2)^127, UInt128(2))
