# This file is a part of Julia. License is MIT: https://julialang.org/license

using InteractiveUtils: code_llvm
# fast math

@testset "check fast present in LLVM" begin
    for T in (Float16, Float32, Float64, ComplexF32, ComplexF64)
        f(x) = @fastmath x + x + x
        llvm = sprint(code_llvm, f, (T,))
        @test occursin("fast", llvm)

        g(x) = @fastmath x * x * x
        llvm = sprint(code_llvm, g, (T,))
        @test occursin("fast", llvm)
    end

    for T in (Float16, Float32, Float64)
        f(x, y, z) = @fastmath min(x, y, z)
        llvm = sprint(code_llvm, f, (T,T,T))
        @test occursin("fast", llvm)

        g(x, y, z) = @fastmath max(x, y, z)
        llvm = sprint(code_llvm, g, (T,T,T))
        @test occursin("fast", llvm)
    end
end

@testset "check expansions" begin
    @test macroexpand(Main, :(@fastmath 1+2)) == :(Base.FastMath.add_fast(1,2))
    @test macroexpand(Main, :(@fastmath +)) == :(Base.FastMath.add_fast)
    @test macroexpand(Main, :(@fastmath min(1))) == :(Base.FastMath.min_fast(1))
    @test macroexpand(Main, :(@fastmath min)) == :(Base.FastMath.min_fast)
    @test macroexpand(Main, :(@fastmath x.min)) == :(x.min)
    @test macroexpand(Main, :(@fastmath sincos(x))) == :(Base.FastMath.sincos_fast(x))
end
const one32 = one(Float32)
const eps32 = eps(Float32)
const eps32_2 = eps32/2
# Note: Cannot use local functions since these are not yet optimized
fm_ieee_32(x) = x + eps32_2 + eps32_2
fm_fast_32(x) = @fastmath x + eps32_2 + eps32_2
const one64 = one(Float64)
const eps64 = eps(Float64)
const eps64_2 = eps64/2
# Note: Cannot use local functions since these are not yet optimized
fm_ieee_64(x) = x + eps64_2 + eps64_2
fm_fast_64(x) = @fastmath x + eps64_2 + eps64_2
fm_ieee_64_upd(x) = (r=x; r+=eps64_2; r+=eps64_2)
fm_fast_64_upd(x) = @fastmath (r=x; r+=eps64_2; r+=eps64_2)
@testset "basic arithmetic" begin
    @test fm_ieee_32(one32) == one32
    @test (fm_fast_32(one32) == one32 ||
           fm_fast_32(one32) == one32 + eps32 > one32)
    @test fm_ieee_64(one64) == one64
    @test (fm_fast_64(one64) == one64 ||
           fm_fast_64(one64) == one64 + eps64 > one64)
    # check updating operators
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

        for x in (zero, two, convert(T, Inf), convert(T, NaN))
            @test @fastmath(isfinite(x))
            @test !@fastmath(isinf(x))
            @test !@fastmath(isnan(x))
            @test !@fastmath(issubnormal(x))
        end
    end

    for T in (ComplexF32, ComplexF64, Complex{BigFloat})
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
end

# math functions

@testset "real arithmetic" begin
    for T in (Float16, Float32, Float64, BigFloat)
        half = 1/convert(T,2)
        third = 1/convert(T,3)

        for f in (:+, :-, :abs, :abs2, :conj, :inv, :sign,
                  :acos, :asin, :asinh, :atan, :atanh, :cbrt, :cos, :cosh,
                  :exp10, :exp2, :exp, :log10, :log1p,
                  :log2, :log, :sin, :sinh, :sqrt, :tan, :tanh,
                  :min, :max)
            @eval begin
                @test @fastmath($f($half)) ≈ $f($half)
                @test @fastmath($f($third)) ≈ $f($third)
            end
        end
        if T != Float16
            for f in (:expm1,)
                @eval begin
                    @test @fastmath($f($half)) ≈ $f($half)
                    @test @fastmath($f($third)) ≈ $f($third)
                end
            end
        end
        for f in (:acosh,)
            @eval begin
                @test @fastmath($f(1+$half)) ≈ $f(1+$half)
                @test @fastmath($f(1+$third)) ≈ $f(1+$third)
            end
        end
        for f in (:sincos,)
            @eval begin
                @test all(@fastmath($f($half)) .≈ $f($half))
                @test all(@fastmath($f($third)) .≈ $f($third))
            end
        end
        for f in (:+, :-, :*, :/, :%, :(==), :!=, :<, :<=, :>, :>=, :^,
                  :atan, :hypot, :max, :min, :log)
            @eval begin
                @test @fastmath($f($half, $third)) ≈ $f($half, $third)
                @test @fastmath($f($third, $half)) ≈ $f($third, $half)
            end
        end

        # issue 31795
        for f in (:min, :max)
            @eval begin
                @test @fastmath($f($half, $third, 1+$half)) ≈ $f($half, $third, 1+$half)
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
end
@testset "complex arithmetic" begin
    for T in (ComplexF32, ComplexF64, Complex{BigFloat})
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
        for f in (:+, :-, :*, :/, :(==), :!=, :^, :log)
            @eval begin
                @test @fastmath($f($half, $third)) ≈ $f($half, $third) rtol=$rtol
                @test @fastmath($f($third, $half)) ≈ $f($third, $half) rtol=$rtol
            end
        end
    end
end
@testset "mixed real/complex arithmetic" begin
    for T in (Float32, Float64, BigFloat)
        CT = Complex{T}
        half = 1/T(2)
        third = 1/T(3)
        chalf = (1+1im)/CT(2)
        cthird = (1-1im)/CT(3)

        for f in (:+, :-, :*, :/, :(==), :!=, :^, :log)
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
end

@testset "reductions" begin
    @test @fastmath(maximum([1,2,3])) == 3
    @test @fastmath(minimum([1,2,3])) == 1
    @test @fastmath(maximum(abs2, [1,2,3+0im])) == 9
    @test @fastmath(minimum(sqrt, [1,2,3])) == 1
    @test @fastmath(maximum(Float32[4 5 6; 7 8 9])) == 9.0f0
    @test @fastmath(minimum(Float32[4 5 6; 7 8 9])) == 4.0f0

    @test @fastmath(maximum(Float32[4 5 6; 7 8 9]; dims=1)) == Float32[7.0 8.0 9.0]
    @test @fastmath(minimum(Float32[4 5 6; 7 8 9]; dims=2)) == Float32[4.0; 7.0;;]
    @test @fastmath(maximum(abs, [4+im -5 6-im; -7 8 -9]; dims=1)) == [7.0 8.0 9.0]
    @test @fastmath(minimum(cbrt, [4 -5 6; -7 8 -9]; dims=2)) == cbrt.([-5; -9;;])

    x = randn(3,4,5)
    x1 = sum(x; dims=1)
    x23 = sum(x; dims=(2,3))
    @test @fastmath(maximum!(x1, x)) ≈ maximum(x; dims=1)
    @test x1 ≈ maximum(x; dims=1)
    @test @fastmath(minimum!(x23, x)) ≈ minimum(x; dims=(2,3))
    @test x23 ≈ minimum(x; dims=(2,3))
    @test @fastmath(maximum!(abs, x23, x .+ im)) ≈ maximum(abs, x .+ im; dims=(2,3))
    @test @fastmath(minimum!(abs2, x1, x .+ im)) ≈ minimum(abs2, x .+ im; dims=1)
end

@testset "issue #10544" begin
    a = fill(1.,2,2)
    b = fill(1.,2,2)
    @test @fastmath(a[1] += 2.0) ≈ (b[1] += 2.0)
    @test @fastmath(a[2] -= 2.0) ≈ (b[2] -= 2.0)
    @test @fastmath(a[1,1] *= 2.0) ≈ (b[1,1] *= 2.0)
    @test @fastmath(a[2,2] /= 2.0) ≈ (b[2,2] /= 2.0)
    @test @fastmath(a[1,2] ^= 2.0) ≈ (b[1,2] ^= 2.0)

    # test fallthrough for unsupported ops
    local c = 0
    @test @fastmath(c |= 1) == 1
end

@testset "issue #23218" begin
    a = zeros(1)
    b = [1.0]
    idx = (1,)
    @fastmath a[idx...] += b[idx...]
    @test a == b
end

@testset "literal powers" begin
    @test @fastmath(2^-2) == @fastmath(2.0^-2) == 0.25
    # Issue #53817
    # Note that exponent -2^63 fails testing because of issue #53881
    # Therefore we test with -(2^63-1). For Int == Int32 there is an analogue restriction.
    # See also PR #53860.
    if Int == Int64
        @test @fastmath(2^-9223372036854775807) === 0.0
        @test_throws DomainError @fastmath(2^-9223372036854775809)
        @test @fastmath(1^-9223372036854775807) isa Float64
        @test @fastmath(1^-9223372036854775809) isa Int
    elseif Int == Int32
        @test @fastmath(2^-2147483647) === 0.0
        @test_throws DomainError @fastmath(2^-2147483649)
        @test @fastmath(1^-2147483647) isa Float64
        @test @fastmath(1^-2147483649) isa Int
    end
    @test_throws MethodError @fastmath(^(2))
end
# issue #53857
@testset "fast_pow" begin
    n = Int64(2)^52
    @test @fastmath (1 + 1 / n) ^ n ≈ ℯ
    @test @fastmath (1 + 1 / n) ^ 4503599627370496 ≈ ℯ
end

@testset "sincos fall-backs" begin
    struct FloatWrapper
        inner::Float64
    end
    Base.sin(outer::FloatWrapper) = sin(outer.inner)
    Base.cos(outer::FloatWrapper) = cos(outer.inner)
    for zilch in (FloatWrapper(0.0), 0, 0 + 0 * im)
        @test (@fastmath sincos(zilch)) == (0, 1)
    end
end

@testset "non-numeric fallbacks" begin
    @test (@fastmath :(:sin)) == :(:sin)
    @test (@fastmath "a" * "b") == "ab"
    @test (@fastmath "a" ^ 2) == "aa"
end


@testset "exp overflow and underflow" begin
    for T in (Float32,Float64)
        for func in (@fastmath exp2,exp,exp10)
            @test func(T(2000)) == T(Inf)
            @test func(T(-2000)) == T(0)
        end
    end
end

@testset "+= with indexing (#47241)" begin
    i = 0
    x = zeros(2)
    @fastmath x[i += 1] += 1
    @fastmath x[end] += 1
    @test x == [1, 1]
    @test i == 1
end

@testset "@fastmath-related crash (#49907)" begin
    x = @fastmath maximum(Float16[1,2,3]; init = Float16(0))
    @test x == Float16(3)
end

@testset "Test promotion of >=3 arg fastmath" begin
    # Bug caught in https://github.com/JuliaLang/julia/pull/54513#discussion_r1620553369
    x = @fastmath 1. + 1. + 1f0
    @test x == 3.0
end
