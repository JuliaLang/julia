# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

function div_significand_with_remainder_naive(num, den, minimum_significand_size)
    dr = let den = den
        n -> divrem(n, den, RoundToZero)
    end

    shift = 0
    (quo, rem) = dr(num << shift)

    # bit width
    bw = Base.top_set_bit

    while bw(quo) < minimum_significand_size
        shift += true
        (quo, rem) = dr(num << shift)
    end

    # suboptimal but allowed results
    (q1, r1) = dr(num << (shift + 1))
    (q2, r2) = dr(num << (shift + 2))
    (q3, r3) = dr(num << (shift + 3))

    (
        (quo, rem, shift),  # optimal result
        (q1, r1, shift + 1),
        (q2, r2, shift + 2),
        (q3, r3, shift + 3)
    )
end

function test_div_significand_with_remainder(n, d, mss)
    (0 < n)   || error("unsupported")
    (0 < d)   || error("unsupported")
    (0 < mss) || error("unsupported")
    bw = Base.top_set_bit
    wid = x -> widen(widen(x))
    t = let n = wid(n), d = wid(d)
        function(q_narrow, r_narrow, sh)
            q = wid(q_narrow)
            r = wid(r_narrow)
            @test q + r/d ≈ n*(2.0^sh)/d
            @test floor(Int, log2(abs(q))) == exponent(q) == bw(q) - 1 == sh + exponent(n/d)
        end
    end
    dswr = Base.RationalToFloat.div_significand_with_remainder
    a = @inferred dswr(n, d, mss)
    (a0, a1, a2, a3) = div_significand_with_remainder_naive(n, d, mss)
    @test a ∈ (a0, a1, a2, a3)
    t(a...)
    t(a0...)
    t(a1...)
    t(a2...)
    t(a3...)
    nothing
end

function test_div_significand_with_remainder_iters(::Type{T}, nds, msss) where {T}
    @testset "mss: $mss" for mss ∈ msss
        @testset "num: $num" for num ∈ nds
            @testset "den: $den" for den ∈ nds
                # XXX: shift `n` and `d` to vary the FP exponent more
                n = T(num)::T
                d = T(den)::T
                test_div_significand_with_remainder(widen(n), d, mss)
            end
        end
    end
end

max_mss_for_type(::Type{T}) where {T<:Unsigned} = sizeof(T)*8 - 3
max_mss_for_type(::Type{T}) where {T<:Signed}   = sizeof(T)*8 - 4
max_mss_for_type(::Type{BigInt}) = 200  # just a big value

max_nd_for_type(::Type{T}) where {T} = typemax(T)
max_nd_for_type(::Type{BigInt}) = BigInt(100000)  # just a big value

function min_no_promotion_impl(isless::F, a::T, b) where {F, T}
    if isless(b, a)
        T(b)::T
    else
        a
    end
end
min_no_promotion(a::T, b) where {T} = min_no_promotion_impl(isless, a, b)
max_no_promotion(a::T, b) where {T} = min_no_promotion_impl(((l, r) -> isless(r, l)), a, b)

function test_div_significand_with_remainder_iters(::Type{T}) where {T}
    range_typemax = function(x)
        l = one(T)::T
        u = max_nd_for_type(T)::T
        a = max_no_promotion(l, u - x)
        a:u
    end

    range_zero = function(x)
        u = max_nd_for_type(T)::T
        b = min_no_promotion(u, x)
        Base.OneTo(b)
    end

    k = 30
    nds = Vector{T}()
    append!(nds, range_zero(k))
    append!(nds, range_typemax(k))
    sort!(nds)
    unique!(nds)

    msss = Vector{Int}()
    max_mss = max_mss_for_type(T)
    append!(msss, intersect(Base.OneTo(6), Base.OneTo(max_mss)))
    push!(msss, max_mss)
    sort!(msss)
    unique!(msss)

    test_div_significand_with_remainder_iters(T, nds, msss)
end

function test_divrem_2(::Type{T}) where {T}
    dr = Base.RationalToFloat.divrem_2
    @testset "n: $n" for n ∈ false:T(typemax(Int8))
        @testset "k: $k" for k ∈ 0:6
            @test dr(n, k) == divrem(n, 1 << k, RoundToZero)
        end
    end
end

function test_machine_shift(shift::S, ::Type{T}) where {S,T}
    @testset "k: $k" for k ∈ 0:(sizeof(T)*8 - 1)
        ms = Base.RationalToFloat.machine_shift
        o = one(T)
        @test ms(shift, o, k) == shift(o, k)
    end
end

const Us = (UInt8, UInt16, UInt32, UInt64, UInt128)
const Ss = (Int8, Int16, Int32, Int64, Int128)
const Ts = (Us..., Ss..., BigInt)

@testset "machine_shift" begin
    @testset "T: $T" for T ∈ Ts
        @testset "shift: $shift" for shift ∈ (<<, >>>)
            test_machine_shift(shift, T)
        end
    end
end

@testset "divrem_2" begin
    @testset "T: $T" for T ∈ Ts
        test_divrem_2(T)
    end
end

@testset "div_significand_with_remainder" begin
    @testset "T: $T" for T ∈ Ts
        test_div_significand_with_remainder_iters(T)
    end
end
