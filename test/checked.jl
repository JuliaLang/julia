# This file is a part of Julia. License is MIT: http://julialang.org/license

# Checked integer arithmetic

import Base: checked_abs, checked_neg, checked_add, checked_sub, checked_mul,
             checked_div, checked_rem, checked_fld, checked_mod, checked_cld

# Test return types
for T in (Int8,Int16,Int32,Int64,Int128, UInt8,UInt16,UInt32,UInt64,UInt128)
    z, o = T(0), T(1)
    @test typeof(checked_neg(z)) === T
    @test typeof(checked_abs(z)) === T
    @test typeof(checked_add(z)) === T
    @test typeof(checked_mul(z)) === T
    @test typeof(checked_add(z,z)) === T
    @test typeof(checked_sub(z,z)) === T
    @test typeof(checked_mul(z,z)) === T
    @test typeof(checked_div(z,o)) === T
    @test typeof(checked_rem(z,o)) === T
    @test typeof(checked_fld(z,o)) === T
    @test typeof(checked_mod(z,o)) === T
    @test typeof(checked_cld(z,o)) === T
end

# checked operations

for T in (Int8, Int16, Int32, Int64, Int128)
    # regular cases
    for s in (-1, +1)
        @test checked_abs(T(0s)) === T(abs(0s))
        @test checked_neg(T(0s)) === T(-(0s))
        @test checked_add(T(0s)) === T(0s)
        @test checked_mul(T(0s)) === T(0s)
        @test checked_abs(T(3s)) === T(abs(3s))
        @test checked_neg(T(3s)) === T(-(3s))
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
    for s1 in (-1, +1), s2 in (-1,+1)
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
    @test checked_abs(T(3)) === T(3)
    @test_throws OverflowError checked_neg(T(3))

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

@test checked_abs(BigInt(-1)) == BigInt(1)
@test checked_abs(BigInt(1)) == BigInt(1)
@test checked_neg(BigInt(-1)) == BigInt(1)
@test checked_neg(BigInt(1)) == BigInt(-1)

# Additional tests

@test checked_sub(UInt(4), UInt(3)) === UInt(1)
@test_throws OverflowError checked_sub(UInt(5), UInt(6))
@test checked_mul(UInt(4), UInt(3)) === UInt(12)

@test checked_sub(Int128(-1),Int128(-2)) === Int128(1)

if WORD_SIZE == 32
    @test_throws OverflowError checked_mul(UInt(2)^30, UInt(2)^2)
else
    @test_throws OverflowError checked_mul(UInt(2)^62, UInt(2)^2)
end

@test checked_add(UInt128(1), UInt128(2)) === UInt128(3)
@test_throws OverflowError checked_add(UInt128(2)^127, UInt128(2)^127)

@test checked_sub(UInt128(2), UInt128(1)) === UInt128(1)
@test_throws OverflowError checked_sub(UInt128(3), UInt128(4))

@test checked_mul(UInt128(3), UInt128(4)) === UInt128(12)
@test_throws OverflowError checked_mul(UInt128(2)^127, UInt128(2))
