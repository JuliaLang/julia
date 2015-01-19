# fast math

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
    @test isapprox((@fastmath one_epsf * one_epsf - 1),
                   float32(65537/1073741824))
end
let eps = 1.0/2^30, one_eps = 1+eps
    @test isapprox((@fastmath one_eps * one_eps - 1),
                   2147483649/1152921504606846976)
end

for T in (Float32, Float64, BigFloat)
    zero = convert(T, 0)
    one = convert(T, 1) + eps(T)
    two = convert(T, 2) + 1//10
    three = convert(T, 3) + 1//100

    @test isapprox((@fastmath +two), +two)
    @test isapprox((@fastmath -two), -two)
    @test isapprox((@fastmath zero+one+two), zero+one+two)
    @test isapprox((@fastmath zero-one-two), zero-one-two)
    @test isapprox((@fastmath one*two*three), one*two*three)
    @test isapprox((@fastmath one/two/three), one/two/three)
    @test isapprox((@fastmath rem(two, three)), rem(two, three))
    @test isapprox((@fastmath mod(two, three)), mod(two, three))
    @test (@fastmath cmp(two, two)) == cmp(two, two)
    @test (@fastmath cmp(two, three)) == cmp(two, three)
    @test (@fastmath cmp(three, two)) == cmp(three, two)
    @test (@fastmath one/zero) == convert(T, Inf)
    @test (@fastmath -one/zero) == -convert(T, Inf)
    @test isnan(@fastmath zero/zero) # must not throw

    for x in (zero, two, convert(T, Inf), convert(T, NaN))
        @test (@fastmath isfinite(x))
        @test !(@fastmath isinf(x))
        @test !(@fastmath isnan(x))
        @test !(@fastmath issubnormal(x))
    end
end

for T in (Complex64, Complex128, Complex{BigFloat})
    zero = convert(T, 0)
    one = convert(T, 1) + im*eps(real(convert(T,1)))
    two = convert(T, 2) + im//10
    three = convert(T, 3) + im//100

    @test isapprox((@fastmath +two), +two)
    @test isapprox((@fastmath -two), -two)
    @test isapprox((@fastmath zero+one+two), zero+one+two)
    @test isapprox((@fastmath zero-one-two), zero-one-two)
    @test isapprox((@fastmath one*two*three), one*two*three)
    @test isapprox((@fastmath one/two/three), one/two/three)
    @test (@fastmath three == two) == (three == two)
    @test (@fastmath three != two) == (three != two)
    @test isnan(@fastmath one/zero)  # must not throw
    @test isnan(@fastmath -one/zero) # must not throw
    @test isnan(@fastmath zero/zero) # must not throw

    for x in (zero, two, convert(T, Inf), convert(T, NaN))
        @test (@fastmath isfinite(x))
        @test !(@fastmath isinf(x))
        @test !(@fastmath isnan(x))
        @test !(@fastmath issubnormal(x))
    end
end



# math functions

for T in (Float32, Float64, BigFloat)
    half = 1/convert(T, 2)
    third = 1/convert(T, 3)

    for f in (:+, :-, :abs, :abs2, :conj, :inv, :sign,
              :acos, :asin, :asinh, :atan, :atanh, :cbrt, :cos, :cosh,
              :exp10, :exp2, :exp, :expm1, :lgamma, :log10, :log1p,
              :log2, :log, :sin, :sinh, :sqrt, :tan, :tanh)
        @test isapprox((@eval @fastmath $f($half)), (@eval $f($half)))
        @test isapprox((@eval @fastmath $f($third)), (@eval $f($third)))
    end
    for f in (:acosh,)
        @test isapprox((@eval @fastmath $f(1+$half)), (@eval $f(1+$half)))
        @test isapprox((@eval @fastmath $f(1+$third)), (@eval $f(1+$third)))
    end
    for f in (:+, :-, :*, :/, :%, :(==), :!=, :<, :<=, :>, :>=, :^,
              :atan2, :hypot, :max, :min)
        @test isapprox((@eval @fastmath $f($half, $third)),
                       (@eval $f($half, $third)))
        @test isapprox((@eval @fastmath $f($third, $half)),
                       (@eval $f($third, $half)))
    end
    for f in (:minmax,)
        @test isapprox((@eval @fastmath $f($half, $third))[1],
                       (@eval $f($half, $third))[1])
        @test isapprox((@eval @fastmath $f($half, $third))[2],
                       (@eval $f($half, $third))[2])
        @test isapprox((@eval @fastmath $f($third, $half))[1],
                       (@eval $f($third, $half))[1])
        @test isapprox((@eval @fastmath $f($third, $half))[2],
                       (@eval $f($third, $half))[2])
    end
end

for T in (Float32, Float64, BigFloat)
    CT = Complex{T}
    half = (1+1im)/convert(T, 2) # complex
    third = 1/convert(T, 3)      # real

    @test isapprox((@fastmath third^3), third^3)

    @test isapprox((@fastmath half/third), half/third)
    @test isapprox((@fastmath half^3), half^3)
    @test isapprox((@fastmath cis(third)), cis(third))
end

for T in (Complex64, Complex128, Complex{BigFloat})
    half = (1+1im)/convert(T, 2)
    third = (1-1im)/convert(T, 3)

    for f in (:+, :-, :abs, :abs2, :conj, :inv, :sign,
              :acos, :acosh, :asin, :asinh, :atan, :atanh, :cos,
              :cosh, :exp10, :exp2, :exp, :expm1, :log10, :log1p,
              :log2, :log, :sin, :sinh, :sqrt, :tan, :tanh)
        @test isapprox((@eval @fastmath $f($half)), (@eval $f($half)))
        @test isapprox((@eval @fastmath $f($third)), (@eval $f($third)))
    end
    for f in (:+, :-, :*, :/, :(==), :!=, :^)
        @test isapprox((@eval @fastmath $f($half, $third)),
                       (@eval $f($half, $third)))
        @test isapprox((@eval @fastmath $f($third, $half)),
                       (@eval $f($third, $half)))
    end
end
