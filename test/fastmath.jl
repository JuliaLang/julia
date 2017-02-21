# This file is a part of Julia. License is MIT: http://julialang.org/license

# fast math

# check expansions

@test macroexpand(:(@fastmath 1+2)) == :(Base.FastMath.add_fast(1,2))
@test macroexpand(:(@fastmath +)) == :(Base.FastMath.add_fast)
@test macroexpand(:(@fastmath min(1))) == :(Base.FastMath.min_fast(1))
@test macroexpand(:(@fastmath min)) == :(Base.FastMath.min_fast)
@test macroexpand(:(@fastmath x.min)) == :(x.min)

# basic arithmetic

const one32 = one(Float32)
const eps32 = eps(Float32)
const eps32_2 = eps32/2
# Note: Cannot use local functions since these are not yet optimized
fm_ieee_32(x) = x + eps32_2 + eps32_2
fm_fast_32(x) = @fastmath x + eps32_2 + eps32_2
@test fm_ieee_32(one32) == one32
@test (fm_fast_32(one32) == one32 ||
       fm_fast_32(one32) == one32 + eps32 > one32)

const one64 = one(Float64)
const eps64 = eps(Float64)
const eps64_2 = eps64/2
# Note: Cannot use local functions since these are not yet optimized
fm_ieee_64(x) = x + eps64_2 + eps64_2
fm_fast_64(x) = @fastmath x + eps64_2 + eps64_2
@test fm_ieee_64(one64) == one64
@test (fm_fast_64(one64) == one64 ||
       fm_fast_64(one64) == one64 + eps64 > one64)
# check updating operators
fm_ieee_64_upd(x) = (r=x; r+=eps64_2; r+=eps64_2)
fm_fast_64_upd(x) = @fastmath (r=x; r+=eps64_2; r+=eps64_2)
@test fm_ieee_64_upd(one64) == one64
@test (fm_fast_64_upd(one64) == one64 ||
       fm_fast_64_upd(one64) == one64 + eps64 > one64)

let epsf = 1.0f0/2^15, one_epsf = 1+epsf
    @test @fastmath(one_epsf * one_epsf - 1) ≈ Float32(65537/1073741824)
end
let eps = 1.0/2^30, one_eps = 1+eps
    @test @fastmath(one_eps * one_eps - 1) ≈ 2147483649/1152921504606846976
end

for T in (Float32, Float64, BigFloat)
    zero = convert(T, 0)
    one = convert(T, 1) + eps(T)
    two = convert(T, 2) + 1//10
    three = convert(T, 3) + 1//100

    @test @fastmath(+two) ≈ +two
    @test @fastmath(-two) ≈ -two
    @test @fastmath(zero+one+two) ≈ zero+one+two
    @test @fastmath(zero-one-two) ≈ zero-one-two
    @test @fastmath(one*two*three) ≈ one*two*three
    @test @fastmath(one/two/three) ≈ one/two/three
    @test @fastmath(rem(two,three)) ≈ rem(two,three)
    @test @fastmath(mod(two,three)) ≈ mod(two,three)
    @test @fastmath(cmp(two,two)) == cmp(two,two)
    @test @fastmath(cmp(two,three)) == cmp(two,three)
    @test @fastmath(cmp(three,two)) == cmp(three,two)
    @test @fastmath(one/zero) == convert(T,Inf)
    @test @fastmath(-one/zero) == -convert(T,Inf)
    @test isnan(@fastmath(zero/zero)) # must not throw

    for x in (zero, two, convert(T, Inf), convert(T, NaN))
        @test @fastmath(isfinite(x))
        @test !@fastmath(isinf(x))
        @test !@fastmath(isnan(x))
        @test !@fastmath(issubnormal(x))
    end
end

for T in (Complex64, Complex128, Complex{BigFloat})
    zero = convert(T,0)
    one = convert(T,1) + im*eps(real(convert(T,1)))
    two = convert(T,2) + im//10
    three = convert(T,3) + im//100

    @test @fastmath(+two) ≈ +two
    @test @fastmath(-two) ≈ -two
    @test @fastmath(zero+one+two) ≈ zero+one+two
    @test @fastmath(zero-one-two) ≈ zero-one-two
    @test @fastmath(one*two*three) ≈ one*two*three
    @test @fastmath(one/two/three) ≈ one/two/three
    @test @fastmath(three == two) == (three == two)
    @test @fastmath(three != two) == (three != two)
    @test isnan(@fastmath(one/zero))  # must not throw
    @test isnan(@fastmath(-one/zero)) # must not throw
    @test isnan(@fastmath(zero/zero)) # must not throw

    for x in (zero, two, convert(T, Inf), convert(T, NaN))
        @test @fastmath(isfinite(x))
        @test !@fastmath(isinf(x))
        @test !@fastmath(isnan(x))
        @test !@fastmath(issubnormal(x))
    end
end


# math functions

# real arithmetic
for T in (Float32, Float64, BigFloat)
    half = 1/convert(T,2)
    third = 1/convert(T,3)

    for f in (:+, :-, :abs, :abs2, :conj, :inv, :sign,
              :acos, :asin, :asinh, :atan, :atanh, :cbrt, :cos, :cosh,
              :exp10, :exp2, :exp, :expm1, :lgamma, :log10, :log1p,
              :log2, :log, :sin, :sinh, :sqrt, :tan, :tanh)
        @eval begin
            @test @fastmath($f($half)) ≈ $f($half)
            @test @fastmath($f($third)) ≈ $f($third)
        end
    end
    for f in (:acosh,)
        @eval begin
            @test @fastmath($f(1+$half)) ≈ $f(1+$half)
            @test @fastmath($f(1+$third)) ≈ $f(1+$third)
        end
    end
    for f in (:+, :-, :*, :/, :%, :(==), :!=, :<, :<=, :>, :>=, :^,
              :atan2, :hypot, :max, :min)
        @eval begin
            @test @fastmath($f($half, $third)) ≈ $f($half, $third)
            @test @fastmath($f($third, $half)) ≈ $f($third, $half)
        end
    end
    for f in (:minmax,)
        @eval begin
            @test @fastmath($f($half, $third)[1]) ≈ $f($half, $third)[1]
            @test @fastmath($f($half, $third)[2]) ≈ $f($half, $third)[2]
            @test @fastmath($f($third, $half)[1]) ≈ $f($third, $half)[1]
            @test @fastmath($f($third, $half)[2]) ≈ $f($third, $half)[2]
        end
    end
end

# complex arithmetic
for T in (Complex64, Complex128, Complex{BigFloat})
    half = (1+1im)/T(2)
    third = (1-1im)/T(3)

    # some of these functions promote their result to double
    # precision, but we want to check equality at precision T
    rtol = Base.rtoldefault(real(T))

    for f in (:+, :-, :abs, :abs2, :conj, :inv, :sign,
              :acos, :acosh, :asin, :asinh, :atan, :atanh, :cis, :cos,
              :cosh, :exp10, :exp2, :exp, :expm1, :log10, :log1p,
              :log2, :log, :sin, :sinh, :sqrt, :tan, :tanh)
        @eval begin
            @test @fastmath($f($half)) ≈ $f($half) rtol=$rtol
            @test @fastmath($f($third)) ≈ $f($third) rtol=$rtol
        end
    end
    for f in (:+, :-, :*, :/, :(==), :!=, :^)
        @eval begin
            @test @fastmath($f($half, $third)) ≈ $f($half, $third) rtol=$rtol
            @test @fastmath($f($third, $half)) ≈ $f($third, $half) rtol=$rtol
        end
    end
end

# mixed real/complex arithmetic
for T in (Float32, Float64, BigFloat)
    CT = Complex{T}
    half = 1/T(2)
    third = 1/T(3)
    chalf = (1+1im)/CT(2)
    cthird = (1-1im)/CT(3)

    for f in (:+, :-, :*, :/, :(==), :!=, :^)
        @eval begin
            @test @fastmath($f($chalf, $third)) ≈ $f($chalf, $third)
            @test @fastmath($f($half, $cthird)) ≈ $f($half, $cthird)
            @test @fastmath($f($cthird, $half)) ≈ $f($cthird, $half)
            @test @fastmath($f($third, $chalf)) ≈ $f($third, $chalf)
        end
    end

    @test @fastmath(third^3) ≈ third^3
    @test @fastmath(chalf/third) ≈ chalf/third
    @test @fastmath(chalf^3) ≈ chalf^3
    @test @fastmath(cis(third)) ≈ cis(third)
end

# issue #10544
let a = ones(2,2), b = ones(2,2)
    @test @fastmath(a[1] += 2.0) ≈ (b[1] += 2.0)
    @test @fastmath(a[2] -= 2.0) ≈ (b[2] -= 2.0)
    @test @fastmath(a[1,1] *= 2.0) ≈ (b[1,1] *= 2.0)
    @test @fastmath(a[2,2] /= 2.0) ≈ (b[2,2] /= 2.0)
    @test @fastmath(a[1,2] ^= 2.0) ≈ (b[1,2] ^= 2.0)

    # test fallthrough for unsupported ops
    local c = 0
    @test @fastmath(c |= 1) == 1
end
