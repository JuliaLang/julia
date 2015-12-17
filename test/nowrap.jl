# This file is a part of Julia. License is MIT: http://julialang.org/license

# unchecked integer arithmetic

import Base.NoWrap:
    unsafe_nowrap_abs, unsafe_nowrap_neg, unsafe_nowrap_add, unsafe_nowrap_sub,
    unsafe_nowrap_mul, unsafe_nowrap_div, unsafe_nowrap_rem, unsafe_nowrap_fld,
    unsafe_nowrap_mod, unsafe_nowrap_cld

# Test return types
for T in (Int8,Int16,Int32,Int64,Int128, UInt8,UInt16,UInt32,UInt64,UInt128)
    z, o = T(0), T(1)
    @test typeof(unsafe_nowrap_neg(z)) === T
    @test typeof(unsafe_nowrap_abs(z)) === T
    @test typeof(unsafe_nowrap_add(z)) === T
    @test typeof(unsafe_nowrap_mul(z)) === T
    @test typeof(unsafe_nowrap_add(z,z)) === T
    @test typeof(unsafe_nowrap_sub(z,z)) === T
    @test typeof(unsafe_nowrap_mul(z,z)) === T
    @test typeof(unsafe_nowrap_div(z,o)) === T
    @test typeof(unsafe_nowrap_rem(z,o)) === T
    @test typeof(unsafe_nowrap_fld(z,o)) === T
    @test typeof(unsafe_nowrap_mod(z,o)) === T
    @test typeof(unsafe_nowrap_cld(z,o)) === T
end

# unchecked operations

for T in (Int8, Int16, Int32, Int64, Int128)
    # regular cases
    for s in (-1, +1)
        @test unsafe_nowrap_abs(T(0s)) === T(abs(0s))
        @test unsafe_nowrap_neg(T(0s)) === T(-(0s))
        @test unsafe_nowrap_add(T(0s)) === T(0s)
        @test unsafe_nowrap_mul(T(0s)) === T(0s)
        @test unsafe_nowrap_abs(T(3s)) === T(abs(3s))
        @test unsafe_nowrap_neg(T(3s)) === T(-(3s))
        @test unsafe_nowrap_add(T(3s)) === T(3s)
        @test unsafe_nowrap_mul(T(3s)) === T(3s)
        @test unsafe_nowrap_abs(T(s*typemax(T))) === typemax(T)
        @test unsafe_nowrap_neg(T(s*typemax(T))) === T(-s*typemax(T))
        @test unsafe_nowrap_add(T(s*typemax(T))) === T(s*typemax(T))
        @test unsafe_nowrap_mul(T(s*typemax(T))) === T(s*typemax(T))
    end

    # regular cases
    for s1 in (-1, +1), s2 in (-1,+1)
        @test unsafe_nowrap_add(T(4s1), T(3s2)) === T(4s1 + 3s2)
        @test unsafe_nowrap_sub(T(4s1), T(3s2)) === T(4s1 - 3s2)
        @test unsafe_nowrap_mul(T(4s1), T(3s2)) === T(4s1 * 3s2)
        @test unsafe_nowrap_div(T(4s1), T(3s2)) === T(div(4s1, 3s2))
        @test unsafe_nowrap_rem(T(4s1), T(3s2)) === T(rem(4s1, 3s2))
        @test unsafe_nowrap_fld(T(4s1), T(3s2)) === T(fld(4s1, 3s2))
        @test unsafe_nowrap_mod(T(4s1), T(3s2)) === T(mod(4s1, 3s2))
        @test unsafe_nowrap_cld(T(4s1), T(3s2)) === T(cld(4s1, 3s2))
    end

    # corner cases
    halfmax = T(typemax(T)÷2)
    halfmax_plus1 = T(halfmax+1)
    halfmin = T(typemin(T)÷2)
    halfmin_minus1 = T(halfmin-1)
    sqrtmax = T(1) << T(sizeof(T)*4)
    half_sqrtmax = sqrtmax >> T(1)
    half_sqrtmax_plus1 = half_sqrtmax + T(1)

    @test unsafe_nowrap_add(typemax(T), T(-1)) === T(typemax(T) - 1)
    @test unsafe_nowrap_add(typemax(T), T(0)) === typemax(T)
    @test unsafe_nowrap_add(typemin(T), T(0)) === typemin(T)
    @test unsafe_nowrap_add(typemin(T), T(1)) === T(typemin(T) + 1)
    @test unsafe_nowrap_add(T(-1), typemax(T)) === T(typemax(T) - 1)
    @test unsafe_nowrap_add(T(0), typemax(T)) === typemax(T)
    @test unsafe_nowrap_add(T(0), typemin(T)) === typemin(T)
    @test unsafe_nowrap_add(T(1), typemin(T)) === T(typemin(T) + 1)
    @test unsafe_nowrap_add(typemax(T), typemin(T)) === T(-1)
    @test unsafe_nowrap_add(typemin(T), typemax(T)) === T(-1)

    @test unsafe_nowrap_sub(typemax(T), T(0)) === typemax(T)
    @test unsafe_nowrap_sub(typemax(T), T(1)) === T(typemax(T) - 1)
    @test unsafe_nowrap_sub(typemin(T), T(-1)) === T(typemin(T) + 1)
    @test unsafe_nowrap_sub(typemin(T), T(0)) === typemin(T)
    @test unsafe_nowrap_sub(T(0), typemax(T)) === T(typemin(T) + 1)
    @test unsafe_nowrap_sub(T(1), typemax(T)) === T(typemin(T) + 2)
    @test unsafe_nowrap_sub(T(-1), typemin(T)) === typemax(T)
    @test unsafe_nowrap_sub(typemax(T), typemax(T)) === T(0)
    @test unsafe_nowrap_sub(typemin(T), typemin(T)) === T(0)
    @test unsafe_nowrap_sub(halfmax, T(-halfmin)) === T(-1)

    @test unsafe_nowrap_mul(typemax(T), T(0)) === T(0)
    @test unsafe_nowrap_mul(typemin(T), T(0)) === T(0)
    @test unsafe_nowrap_mul(typemax(T), T(1)) === typemax(T)
    @test unsafe_nowrap_mul(typemin(T), T(1)) === typemin(T)
    @test unsafe_nowrap_mul(sqrtmax, -half_sqrtmax) === T(sqrtmax * -half_sqrtmax)
    @test unsafe_nowrap_mul(-sqrtmax, half_sqrtmax) === T(-sqrtmax * half_sqrtmax)

    @test unsafe_nowrap_div(typemax(T), T(1)) === typemax(T)
    @test unsafe_nowrap_div(typemax(T), T(-1)) === T(-typemax(T))
    @test unsafe_nowrap_div(typemin(T), T(1)) === typemin(T)
    @test unsafe_nowrap_rem(typemax(T), T(1)) === T(0)
    @test unsafe_nowrap_rem(typemax(T), T(-1)) === T(0)
    @test unsafe_nowrap_rem(typemin(T), T(1)) === T(0)
    @test unsafe_nowrap_rem(typemin(T), T(-1)) === T(0)
    @test unsafe_nowrap_fld(typemax(T), T(1)) === typemax(T)
    @test unsafe_nowrap_fld(typemax(T), T(-1)) === T(-typemax(T))
    @test unsafe_nowrap_fld(typemin(T), T(1)) === typemin(T)
    @test unsafe_nowrap_mod(typemax(T), T(1)) === T(0)
    @test unsafe_nowrap_mod(typemax(T), T(-1)) === T(0)
    @test unsafe_nowrap_mod(typemin(T), T(1)) === T(0)
    @test unsafe_nowrap_mod(typemin(T), T(-1)) === T(0)
    @test unsafe_nowrap_cld(typemax(T), T(-1)) === T(-typemax(T))
    @test unsafe_nowrap_cld(typemin(T), T(1)) === typemin(T)
end

for T in (UInt8, UInt16, UInt32, UInt64, UInt128)
    # regular cases
    @test unsafe_nowrap_abs(T(0)) === T(0)
    @test unsafe_nowrap_neg(T(0)) === T(0)
    @test unsafe_nowrap_abs(T(3)) === T(3)

    # regular cases
    @test unsafe_nowrap_add(T(4), T(3)) === T(7)
    @test unsafe_nowrap_sub(T(4), T(3)) === T(1)
    @test unsafe_nowrap_mul(T(4), T(3)) === T(12)

    # corner cases
    halfmax = T(typemax(T)÷2)
    halfmax_plus1 = T(halfmax+1)
    sqrtmax = T(1) << T(sizeof(T)*4)

    @test unsafe_nowrap_add(typemax(T), T(0)) === typemax(T)
    @test unsafe_nowrap_add(T(0), T(0)) === T(0)
    @test unsafe_nowrap_add(T(0), T(1)) === T(T(0) + 1)
    @test unsafe_nowrap_add(T(0), typemax(T)) === typemax(T)
    @test unsafe_nowrap_add(T(0), T(0)) === T(0)
    @test unsafe_nowrap_add(T(1), T(0)) === T(T(0) + 1)
    @test unsafe_nowrap_add(typemax(T), T(0)) === typemax(T)
    @test unsafe_nowrap_add(T(0), typemax(T)) === typemax(T)

    @test unsafe_nowrap_sub(typemax(T), T(0)) === typemax(T)
    @test unsafe_nowrap_sub(typemax(T), T(1)) === T(typemax(T) - 1)
    @test unsafe_nowrap_sub(T(0), T(0)) === T(0)
    @test unsafe_nowrap_sub(T(0), T(0)) === T(0)
    @test unsafe_nowrap_sub(typemax(T), typemax(T)) === T(0)

    @test unsafe_nowrap_mul(typemax(T), T(0)) === T(0)
    @test unsafe_nowrap_mul(T(0), T(0)) === T(0)
    @test unsafe_nowrap_mul(typemax(T), T(1)) === typemax(T)
    @test unsafe_nowrap_mul(T(0), T(1)) === T(0)

    @test unsafe_nowrap_div(typemax(T), T(1)) === typemax(T)
    @test unsafe_nowrap_rem(typemax(T), T(1)) === T(0)
    @test unsafe_nowrap_fld(typemax(T), T(1)) === typemax(T)
    @test unsafe_nowrap_mod(typemax(T), T(1)) === T(0)
    @test unsafe_nowrap_cld(typemax(T), T(1)) === typemax(T)
end
