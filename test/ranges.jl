# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Checked: checked_length
using InteractiveUtils: code_llvm

isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

@testset "range construction" begin
    @test_throws ArgumentError range(start=1, step=1, stop=2, length=10)
    @test_throws ArgumentError range(start=1, step=1, stop=10, length=11)

    r = 3.0:2:11
    @test r == range(start=first(r), step=step(r), stop=last(r)                  )
    @test r == range(start=first(r), step=step(r),               length=length(r))
    @test r == range(start=first(r),               stop=last(r), length=length(r))
    @test r == range(                step=step(r), stop=last(r), length=length(r))

    r = 4:9
    @test r === range(start=first(r), stop=last(r)                  )
    @test r === range(start=first(r),               length=length(r))
    @test r === range(                stop=last(r), length=length(r))
    @test r === range(first(r),       last(r)                       )
    # the next ones use ==, because it changes the eltype
    @test r ==  range(first(r),       last(r),      length(r)       )
    @test r ==  range(start=first(r), stop=last(r), length=length(r))
    @test r === range(                stop=last(r), length=length(r))

    r = 1:5
    o = Base.OneTo(5)
    let start=first(r), step=step(r), stop=last(r), length=length(r)
        @test o === range(;              stop        )
        @test o === range(;                    length)
        @test r === range(; start,       stop        )
        @test r === range(;              stop, length)
        # the next three lines uses ==, because it changes the eltype
        @test r ==  range(; start,       stop, length)
        @test r ==  range(; start, step,       length)
        @test r ==  range(; stop=Float64(stop))
    end

    for T = (Int8, UInt32, Float64, Char)
        @test typeof(range(start=T(5), length=3)) === typeof(range(stop=T(5), length=3))
        @test typeof(range(start=T(5), length=Int8(3))) === typeof(range(stop=T(5), length=Int8(3)))
    end
    let T = Rational{Int16}
        @test typeof(range(start=T(5), length=Int16(3))) === typeof(range(stop=T(5), length=Int16(3)))
    end


    @test first(10:3) === 10
    @test last(10:3) === 9
    @test step(10:3) === 1
    @test isempty(10:3)

    @test first(10:2:3) === 10
    @test last(10:2:3) === 9
    @test step(10:2:3) === 2
    @test isempty(10:2:3)

    @test first(10:0.2:3) === 10.0
    @test last(10:0.2:3) === 9.8
    @test step(10:0.2:3) === 0.2
    @test isempty(10:0.2:3)

    unitrangeerrstr = "promotion of types Char and Char failed to change any arguments"
    @test_throws unitrangeerrstr UnitRange('a', 'b')

    @test step(false:true) === true # PR 56405
    @test eltype((false:true) + (Int8(0):Int8(1))) === Int8
    @test eltype((false:true:true) + (Int8(0):Int8(1))) === Int8
end

using Dates, Random
isdefined(Main, :PhysQuantities) || @eval Main include("testhelpers/PhysQuantities.jl")
using .Main.PhysQuantities

# Compare precision in a manner sensitive to subnormals, which lose
# precision compared to widening.
function cmp_sn(w, hi, lo, slopbits=0)
    if !isfinite(hi)
        if abs(w) > floatmax(typeof(hi))
            return isinf(hi) && sign(w) == sign(hi)
        end
        if isnan(w) && isnan(hi)
            return true
        end
        return w == hi
    end
    if abs(w) < subnormalmin(typeof(hi))
        return (hi == zero(hi) || abs(w - widen(hi)) < abs(w)) && lo == zero(hi)
    end
    # Compare w == hi + lo unless `lo` issubnormal
    z = widen(hi) + widen(lo)
    if !issubnormal(lo) && lo != 0
        if slopbits == 0
            return z == w
        end
        wr, zr = roundshift(w, slopbits), roundshift(z, slopbits)
        return max(wr-1, zero(wr)) <= zr <= wr+1
    end
    # round w to the same number of bits as z
    zu = asbits(z)
    wu = asbits(w)
    lastbit = false
    while zu > 0 && !isodd(zu)
        lastbit = isodd(wu)
        zu = zu >> 1
        wu = wu >> 1
    end
    return wu <= zu <= wu + lastbit
end

asbits(x) = reinterpret(Base.uinttype(typeof(x)), x)

function roundshift(x, n)
    xu = asbits(x)
    lastbit = false
    for i = 1:n
        lastbit = isodd(xu)
        xu = xu >> 1
    end
    xu + lastbit
end

subnormalmin(::Type{T}) where T = reinterpret(T, Base.uinttype(T)(1))

function highprec_pair(x, y)
    slopbits = (Base.Math.significand_bits(typeof(widen(x))) + 1) -
        2*(Base.Math.significand_bits(typeof(x)) + 1)
    hi, lo = Base.add12(x, y)
    @test cmp_sn(widen(x) + widen(y), hi, lo)
    hi, lo = Base.mul12(x, y)
    @test cmp_sn(widen(x) * widen(y), hi, lo)
    y == 0 && return nothing
    hi, lo = Base.div12(x, y)
    @test cmp_sn(widen(x) / widen(y), hi, lo, slopbits)
    nothing
end
@testset "high precision" begin
    # Because ranges rely on high precision arithmetic, test those utilities first
    for (I, T) in ((Int16, Float16), (Int32, Float32), (Int64, Float64)), i = 1:10^3
        i = rand(I) >> 1  # test large values below
        hi, lo = Base.splitprec(T, i)
        @test widen(hi) + widen(lo) == i
        @test endswith(bitstring(hi), repeat('0', Base.Math.significand_bits(T) ÷ 2))
    end
    for (I, T) in ((Int16, Float16), (Int32, Float32), (Int64, Float64))
        x = T(typemax(I))
        Δi = ceil(I, eps(x))
        for i = typemax(I)-2Δi:typemax(I)-Δi
            hi, lo = Base.splitprec(T, i)
            @test widen(hi) + widen(lo) == i
            @test endswith(bitstring(hi), repeat('0', Base.Math.significand_bits(T) ÷ 2))
        end
        for i = typemin(I):typemin(I)+Δi
            hi, lo = Base.splitprec(T, i)
            @test widen(hi) + widen(lo) == i
            @test endswith(bitstring(hi), repeat('0', Base.Math.significand_bits(T) ÷ 2))
        end
    end

    # # This tests every possible pair of Float16s. It takes too long for
    # # ordinary use, which is why it's commented out.
    # function pair16()
    #     for yu in 0x0000:0xffff
    #         for xu in 0x0000:0xffff
    #             x, y = reinterpret(Float16, xu), reinterpret(Float16, yu)
    #             highprec_pair(x, y)
    #         end
    #     end
    # end

    for T in (Float16, Float32) # skip Float64 (bit representation of BigFloat is not available)
        for i = 1:10^5
            x, y = rand(T), rand(T)
            highprec_pair(x, y)
            highprec_pair(-x, y)
            highprec_pair(x, -y)
            highprec_pair(-x, -y)
        end
        # Make sure we test dynamic range too
        for i = 1:10^5
            x, y = rand(T), rand(T)
            x == 0 || y == 0 && continue
            x, y = log(x), log(y)
            highprec_pair(x, y)
        end
    end
end
asww(x) = widen(widen(x.hi)) + widen(widen(x.lo))
astuple(x) = (x.hi, x.lo)

function cmp_sn2(w, hi, lo, slopbits=0)
    if !isfinite(hi)
        if abs(w) > floatmax(typeof(hi))
            return isinf(hi) && sign(w) == sign(hi)
        end
        if isnan(w) && isnan(hi)
            return true
        end
        return w == hi
    end
    if abs(w) < subnormalmin(typeof(hi))
        return (hi == zero(hi) || abs(w - widen(hi)) < abs(w)) && lo == zero(hi)
    end
    z = widen(hi) + widen(lo)
    w == z && return true
    zu, wu = asbits(z), asbits(w)
    while zu > 0 && !isodd(zu)
        zu = zu >> 1
        wu = wu >> 1
    end
    zu = zu >> slopbits
    wu = wu >> slopbits
    return wu - 1 <= zu <= wu + 1
end
@testset "TwicePrecision" begin
    # TwicePrecision test. These routines lose accuracy if you form
    # intermediate subnormals; with Float16, this happens so frequently,
    # let's only test Float32.
    let T = Float32
        Tw = widen(T)
        slopbits = (Base.Math.significand_bits(Tw) + 1) -
            2*(Base.Math.significand_bits(T) + 1)
        for i = 1:10^5
            x = Base.TwicePrecision{T}(rand())
            y = Base.TwicePrecision{T}(rand())
            xw, yw = asww(x), asww(y)
            @test cmp_sn2(Tw(xw+yw), astuple(x+y)..., slopbits)
            @test cmp_sn2(Tw(xw-yw), astuple(x-y)..., slopbits)
            @test cmp_sn2(Tw(xw*yw), astuple(x*y)..., slopbits)
            @test cmp_sn2(Tw(xw/yw), astuple(x/y)..., slopbits)
            y = rand(T)
            yw = widen(widen(y))
            @test cmp_sn2(Tw(xw+yw), astuple(x+y)..., slopbits)
            @test cmp_sn2(Tw(xw-yw), astuple(x-y)..., slopbits)
            @test cmp_sn2(Tw(xw*yw), astuple(x*y)..., slopbits)
            @test cmp_sn2(Tw(xw/yw), astuple(x/y)..., slopbits)
        end
    end
    @testset "high precision of varying types" begin
        x = Float32(π)
        y = Float64(Base.MathConstants.γ)
        @test Base.mul12(x, y)[1] ≈ Base.mul12(Float64(π), y)[1] rtol=1e-6
        @test Base.mul12(x, y)[2] ≈ Base.mul12(Float64(π), y)[2] atol=1e-15
        @test Base.div12(x, y)[1] ≈ Base.div12(Float64(π), y)[1] rtol=1e-6
        @test Base.div12(x, y)[2] ≈ Base.div12(Float64(π), y)[2] atol=1e-15
        xtp = Base.TwicePrecision{Float32}(π)
        ytp = Base.TwicePrecision{Float64}(Base.MathConstants.γ)
        @test Float32(xtp + ytp) ≈ Float32(Base.TwicePrecision{Float64}(π) + ytp)
    end

    x1 = Base.TwicePrecision{Float64}(1)
    x0 = Base.TwicePrecision{Float64}(0)
    @test eltype(x1) == Float64
    @test eltype(typeof(x1)) == Float64
    @test zero(typeof(x1)) === x0
    xinf = Base.TwicePrecision{Float64}(Inf)
    @test Float64(x1+x0)  == 1
    @test Float64(x1+0)   == 1
    @test Float64(x1+0.0) == 1
    @test Float64(x1*x0)  == 0
    @test Float64(x1*0)   == 0
    @test Float64(x1*0.0) == 0
    @test Float64(x1/x0)  == Inf
    @test Float64(x1/0)   == Inf
    @test Float64(xinf*x1) == Inf
    @test isnan(Float64(xinf*x0))
    @test isnan(Float64(xinf*0))
    @test isnan(Float64(xinf*0.0))
    @test isnan(Float64(x0/x0))
    @test isnan(Float64(x0/0))
    @test isnan(Float64(x0/0.0))

    x = Base.TwicePrecision(PhysQuantity{1}(4.0))
    @test x.hi*2 === PhysQuantity{1}(8.0)
    @test_throws ErrorException("Int is incommensurate with PhysQuantity") x*2   # not a MethodError for convert
    @test x.hi/2 === PhysQuantity{1}(2.0)
    @test_throws ErrorException("Int is incommensurate with PhysQuantity") x/2
    @test zero(typeof(x)) === Base.TwicePrecision(PhysQuantity{1}(0.0))

    function twiceprecision_roundtrip_is_not_lossy(
        ::Type{S},
        x::T,
    ) where {S<:Number, T<:Union{Number,Base.TwicePrecision}}
        tw = Base.TwicePrecision{S}(x)
        @test x == T(tw)
    end

    function twiceprecision_is_normalized(tw::Tw) where {Tw<:Base.TwicePrecision}
        (hi, lo) = (tw.hi, tw.lo)
        normalized = Tw(Base.canonicalize2(hi, lo)...)
        @test (abs(lo) ≤ abs(hi)) & (tw == normalized)
    end

    rand_twiceprecision(::Type{T}) where {T<:Number} = Base.TwicePrecision{T}(rand(widen(T)))

    # For this test the `BigFloat` mantissa needs to be just a bit
    # larger than the `Float64` mantissa
    setprecision(BigFloat, 70) do
        n = 10
        @testset "twiceprecision roundtrip is not lossy 1" for i ∈ 1:n
            twiceprecision_roundtrip_is_not_lossy(Float64, rand(BigFloat))
        end
        @testset "twiceprecision roundtrip is not lossy 2" for i ∈ 1:n
            twiceprecision_roundtrip_is_not_lossy(Float64, rand_twiceprecision(Float32))
        end
        @testset "twiceprecision normalization 1: Float64 to Float32" for i ∈ 1:n
            twiceprecision_is_normalized(Base.TwicePrecision{Float32}(rand_twiceprecision(Float64)))
        end
        @testset "twiceprecision normalization 2: Float32 to Float64" for i ∈ 1:n
            twiceprecision_is_normalized(Base.TwicePrecision{Float64}(rand_twiceprecision(Float32)))
        end
    end

    @testset "displaying a complex range (#52713)" begin
        r = 1.0*(1:5) .+ im
        @test startswith(repr(r), repr(first(r)))
        @test endswith(repr(r), repr(last(r)))
        @test occursin(repr(step(r)), repr(r))
    end
end
@testset "ranges" begin
    @test size(10:1:0) == (0,)
    @testset "colon" begin
        @inferred((:)(10, 1, 0))
        @inferred((:)(1, .2, 2))
        @inferred((:)(1., .2, 2.))
        @inferred((:)(2, -.2, 1))
        @inferred((:)(1, 0))
        @inferred((:)(0.0, -0.5))
    end

    @testset "indexing" begin
        L32 = @inferred(range(Int32(1), stop=Int32(4), length=4))
        L64 = @inferred(range(Int64(1), stop=Int64(4), length=4))
        @test @inferred(L32[1]) === 1.0 && @inferred(L64[1]) === 1.0
        @test L32[2] == 2 && L64[2] == 2
        @test L32[3] == 3 && L64[3] == 3
        @test L32[4] == 4 && L64[4] == 4
        @test @inferred(range(1.0, stop=2.0, length=2))[1] === 1.0
        @test @inferred(range(1.0f0, stop=2.0f0, length=2))[1] === 1.0f0
        @test @inferred(range(Float16(1.0), stop=Float16(2.0), length=2))[1] === Float16(1.0)

        let r = 5:-1:1
            @test r[1]==5
            @test r[2]==4
            @test r[3]==3
            @test r[4]==2
            @test r[5]==1
        end
        @test @inferred((0.1:0.1:0.3)[2]) === 0.2
        @test @inferred((0.1f0:0.1f0:0.3f0)[2]) === 0.2f0

        @test @inferred((1:5)[1:4]) === 1:4
        @test @inferred((1.0:5)[1:4]) === 1.0:4
        @test (2:6)[1:4] == 2:5
        @test (1:6)[2:5] === 2:5
        @test (1:6)[2:2:5] === 2:2:4
        @test (1:2:13)[2:6] === 3:2:11
        @test (1:2:13)[2:3:7] === 3:6:13

        @test isempty((1:4)[5:4])
        @test_throws BoundsError (1:10)[8:-1:-2]

        let r = typemax(Int)-5:typemax(Int)-1
            @test_throws BoundsError r[7]
        end
    end
    @testset "length" begin
        @test length(.1:.1:.3) == checked_length(.1:.1:.3) == 3
        @test length(1.1:1.1:3.3) == checked_length(1.1:1.1:3.3) == 3
        @test length(1.1:1.3:3) == checked_length(1.1:1.3:3) == 2
        @test length(1:1:1.8) == checked_length(1:1:1.8) == 1
        @test length(1:.2:2) == checked_length(1:.2:2) == 6
        @test length(1.:.2:2.) == checked_length(1.:.2:2.) == 6
        @test length(2:-.2:1) == checked_length(2:-.2:1) == 6
        @test length(2.:-.2:1.) == checked_length(2.:-.2:1.) == 6
        @test length(2:.2:1) == checked_length(2:.2:1) == 0
        @test length(2.:.2:1.) == 0

        @test length(1:0) == checked_length(1:0) == 0
        @test length(0.0:-0.5) == checked_length(0.0:-0.5) == 0
        @test length(1:2:0) == checked_length(1:2:0) == 0
        let r = Char(0):Char(0x001fffff)
            @test length(r) == 2097152
            @test_throws MethodError checked_length(r) == 2097152 # this would work if checked_sub is defined on Char
        end
        let r = typemax(UInt64)//one(UInt64):1:typemax(UInt64)//one(UInt64)
            @test length(r) == 1
            @test_throws MethodError checked_length(r) == 1 # this would work if checked_sub is defined on Rational
        end
    end
    @testset "keys/values" begin
        keytype_is_correct(r) = keytype(r) == eltype(keys(r))
        valtype_is_correct(r) = valtype(r) == eltype(values(r))
        @test keytype_is_correct(1:3)
        @test keytype_is_correct(1:.3:4)
        @test keytype_is_correct(.1:.1:.3)
        @test keytype_is_correct(Int8(1):Int8(5))
        @test keytype_is_correct(Int16(1):Int8(5))
        @test keytype_is_correct(Int16(1):Int8(3):Int8(5))
        @test keytype_is_correct(Int8(1):Int16(3):Int8(5))
        @test keytype_is_correct(Int8(1):Int8(3):Int16(5))
        @test keytype_is_correct(Int64(1):Int64(5))
        @test keytype_is_correct(Int64(1):Int64(5))
        @test keytype_is_correct(Int128(1):Int128(5))
        @test keytype_is_correct(Base.OneTo(4))
        @test keytype_is_correct(Base.OneTo(Int32(4)))

        @test valtype_is_correct(1:3)
        @test valtype_is_correct(1:.3:4)
        @test valtype_is_correct(.1:.1:.3)
        @test valtype_is_correct(Int8(1):Int8(5))
        @test valtype_is_correct(Int16(1):Int8(5))
        @test valtype_is_correct(Int16(1):Int8(3):Int8(5))
        @test valtype_is_correct(Int8(1):Int16(3):Int8(5))
        @test valtype_is_correct(Int8(1):Int8(3):Int16(5))
        @test valtype_is_correct(Int64(1):Int64(5))
        @test valtype_is_correct(Int64(1):Int64(5))
        @test valtype_is_correct(Int128(1):Int128(5))
        @test valtype_is_correct(Base.OneTo(4))
        @test valtype_is_correct(Base.OneTo(Int32(4)))
    end
    @testset "findall(::Base.Fix2{typeof(in)}, ::Array)" begin
        @test findall(in(3:20), [5.2, 3.3]) == findall(in(Vector(3:20)), [5.2, 3.3])

        let span = 5:20,
            r = -7:3:42
            @test findall(in(span), r) == 5:10
            r = 15:-2:-38
            @test findall(in(span), r) == 1:6
        end
    end
    @testset "findfirst" begin
        @test findfirst(==(1), Base.IdentityUnitRange(-1:1)) == 1
        @test findfirst(isequal(3), Base.OneTo(10)) == 3
        @test findfirst(==(0), Base.OneTo(10)) === nothing
        @test findfirst(==(11), Base.OneTo(10)) === nothing
        @test @inferred((r -> Val(findfirst(iszero, r)))(Base.OneTo(10))) == Val(nothing)
        @test findfirst(isone, Base.OneTo(10)) === 1
        @test findfirst(isone, Base.OneTo(0)) === nothing
        @test findfirst(==(4), Int16(3):Int16(7)) === Int(2)
        @test findfirst(==(2), Int16(3):Int16(7)) === nothing
        @test findfirst(isequal(8), 3:7) === nothing
        @test findfirst(==(0), UnitRange(-0.5, 0.5)) === nothing
        @test findfirst(==(2), big(1):big(2)) === 2
        @test findfirst(isequal(7), 1:2:10) == 4
        @test findfirst(iszero, -5:5) == 6
        @test findfirst(iszero, 2:5) === nothing
        @test findfirst(iszero, 6:5) === nothing
        @test findfirst(isone, -5:5) == 7
        @test findfirst(isone, 2:5) === nothing
        @test findfirst(isone, 6:5) === nothing
        @test findfirst(==(7), 1:2:10) == 4
        @test findfirst(==(10), 1:2:10) === nothing
        @test findfirst(==(11), 1:2:10) === nothing
        @test findfirst(==(-7), 1:-1:-10) == 9
        @test findfirst(==(2),1:-1:2) === nothing
        @test findfirst(iszero, 5:-2:-5) === nothing
        @test findfirst(iszero, 6:-2:-6) == 4
        @test findfirst(==(Int128(2)), Int128(1):Int128(1):Int128(4)) === 2
    end
    @testset "findlast" begin
        @test findlast(==(1), Base.IdentityUnitRange(-1:1)) == 1
        @test findlast(isequal(3), Base.OneTo(10)) == 3
        @test findlast(==(0), Base.OneTo(10)) === nothing
        @test findlast(==(11), Base.OneTo(10)) === nothing
        @test @inferred((() -> Val(findlast(iszero, Base.OneTo(10))))()) == Val(nothing)
        @test findlast(isone, Base.OneTo(10)) == 1
        @test findlast(isone, Base.OneTo(0)) === nothing
        @test findlast(==(4), Int16(3):Int16(7)) === Int(2)
        @test findlast(==(2), Int16(3):Int16(7)) === nothing
        @test findlast(isequal(8), 3:7) === nothing
        @test findlast(==(0), UnitRange(-0.5, 0.5)) === nothing
        @test findlast(==(2), big(1):big(2)) === 2
        @test findlast(isequal(7), 1:2:10) == 4
        @test findlast(iszero, -5:5) == 6
        @test findlast(iszero, 2:5) === nothing
        @test findlast(iszero, 6:5) === nothing
        @test findlast(==(7), 1:2:10) == 4
        @test findlast(==(10), 1:2:10) === nothing
        @test findlast(==(11), 1:2:10) === nothing
        @test findlast(==(-7), 1:-1:-10) == 9
        @test findlast(==(2),1:-1:2) === nothing
        @test findlast(iszero, 5:-2:-5) === nothing
        @test findlast(iszero, 6:-2:-6) == 4
        @test findlast(==(Int128(2)), Int128(1):Int128(1):Int128(4)) === 2
    end
    @testset "reverse" begin
        @test reverse(reverse(1:10)) == 1:10
        @test reverse(reverse(typemin(Int):typemax(Int))) == typemin(Int):typemax(Int)
        @test reverse(reverse(typemin(Int):2:typemax(Int))) == typemin(Int):2:typemax(Int)
    end
    @testset "reverse `[Step|Unit]Range{$T}`" for T in (Int8, UInt8, Int, UInt, Int128, UInt128)
        @test reverse(T(1):T(10)) == T(10):-1:T(1)
        @test reverse(typemin(T):typemax(T)) == typemax(T):-1:typemin(T)
        @test reverse(typemin(T):2:typemax(T)) == typemax(T)-T(1):-2:typemin(T)
        @test reverse(reverse(T(1):T(10))) == T(1):T(10)
        @test reverse(reverse(typemin(T):typemax(T))) == typemin(T):typemax(T)
        @test reverse(reverse(typemin(T):2:typemax(T))) == typemin(T):2:typemax(T)
    end
    @testset "intersect" begin
        @test intersect(1:5, 2:3) === 2:3
        @test intersect(-3:5, 2:8) === 2:5
        @test intersect(-8:-3, -8:-3) === -8:-3
        @test intersect(1:5, 5:13) === 5:5
        @test isempty(intersect(-8:-3, -2:2))
        @test isempty(intersect(-3:7, 2:1))
        @test intersect(-8:-3, -2:2) === -2:-3
        @test intersect(-3:7, 2:1) === 2:1
        @test intersect(1:11, -2:3:15) === 1:3:10
        @test intersect(1:11, -2:2:15) === 2:2:10
        @test intersect(1:11, -2:1:15) === 1:1:11
        @test intersect(1:11, 15:-1:-2) === 1:1:11
        @test intersect(1:11, 15:-4:-2) === 3:4:11
        @test intersect(-20:-5, -10:3:-2) === -10:3:-7
        @test isempty(intersect(-5:5, -6:13:20))
        @test isempty(intersect(1:11, 15:4:-2))
        @test isempty(intersect(11:1, 15:-4:-2))
        @test intersect(-5:5, 1 .+ 0 .* (1:3)) == 1:1
        @test isempty(intersect(-5:5, 6 .+ 0 .* (1:3)))
        @test intersect(-15:4:7, -10:-2) === -7:4:-3
        @test intersect(13:-2:1, -2:8) === 7:-2:1
        @test isempty(intersect(13:2:1, -2:8))
        @test isempty(intersect(13:-2:1, 8:-2))
        @test intersect(5 .+ 0 .* (1:4), 2:8) == 5:5
        @test isempty(intersect(5 .+ 0 .* (1:4), -7:3))
        @test intersect(0:3:24, 0:4:24) === 0:12:24
        @test intersect(0:4:24, 0:3:24) === 0:12:24
        @test intersect(0:3:24, 24:-4:0) === 0:12:24
        @test intersect(24:-3:0, 0:4:24) === 24:-12:0
        @test intersect(24:-3:0, 24:-4:0) === 24:-12:0
        @test intersect(1:3:24, 0:4:24) === 4:12:16
        @test intersect(0:6:24, 0:4:24) === 0:12:24
        @test isempty(intersect(1:6:2400, 0:4:2400))
        @test intersect(-51:5:100, -33:7:125) === -26:35:79
        @test intersect(-51:5:100, -32:7:125) === -11:35:94
        @test intersect(0:6:24, 6 .+ 0 .* (0:4:24)) == 6:6:6
        @test intersect(12 .+ 0 .* (0:6:24), 0:4:24) == 12:12 # forms StepRangeLen(12, 0, 5)
        @test isempty(intersect(6 .+ 0 .* (0:6:24), 0:4:24))
        @test intersect(-10:3:24, -10:3:24) === -10:3:23
        @test isempty(intersect(-11:3:24, -10:3:24))
        @test intersect(-11:3:24, -10:3:24) === -11:3:-14
        @test intersect(typemin(Int):2:typemax(Int),1:10) === 2:2:10
        @test intersect(1:10, typemin(Int):2:typemax(Int)) === 2:2:10

        @test intersect(reverse(typemin(Int):2:typemax(Int)),typemin(Int):2:typemax(Int)) == reverse(typemin(Int):2:typemax(Int))
        @test intersect(typemin(Int):2:typemax(Int),reverse(typemin(Int):2:typemax(Int))) == typemin(Int):2:typemax(Int)

        @test intersect(UnitRange(1, 2), 3) === UnitRange(3, 2)
        @test intersect(UnitRange(1, 2), UnitRange(1, 5), UnitRange(3, 7), UnitRange(4, 6)) === UnitRange(4, 2)

        @test intersect(1:3, 2) === intersect(2, 1:3) === 2:2
        @test intersect(1.0:3.0, 2) == intersect(2, 1.0:3.0) == [2.0]

        @test intersect(1:typemax(Int), [1, 3]) == [1, 3]
        @test intersect([1, 3], 1:typemax(Int)) == [1, 3]

        @testset "Support StepRange with a non-numeric step" begin
            start = Date(1914, 7, 28)
            stop = Date(1918, 11, 11)

            @test intersect(start:Day(1):stop, start:Day(1):stop) == start:Day(1):stop
            @test intersect(start:Day(1):stop, start:Day(5):stop) == start:Day(5):stop
            @test intersect(start-Day(10):Day(1):stop-Day(10), start:Day(5):stop) ==
                start:Day(5):stop-Day(10)-mod(stop-start, Day(5))
        end

        @testset "Two AbstractRanges" begin
            struct DummyRange{T} <: AbstractRange{T}
                r
            end
            Base.iterate(dr::DummyRange) = iterate(dr.r)
            Base.iterate(dr::DummyRange, state) = iterate(dr.r, state)
            Base.length(dr::DummyRange) = length(dr.r)
            Base.in(x::Int, dr::DummyRange) = in(x, dr.r)
            Base.unique(dr::DummyRange) = unique(dr.r)
            r1 = DummyRange{Int}([1, 2, 3, 3, 4, 5])
            r2 = DummyRange{Int}([3, 3, 4, 5, 6])
            @test intersect(r1, r2) == [3, 4, 5]
            @test intersect(r2, r1) == [3, 4, 5]
        end
    end
    @testset "issubset" begin
        @test issubset(1:3, 1:typemax(Int)) #32461
        @test issubset(1:3, 1:3)
        @test issubset(1:3, 1:4)
        @test issubset(1:3, 0:3)
        @test issubset(1:3, 0:4)
        @test !issubset(1:5, 2:5)
        @test !issubset(1:5, 1:4)
        @test !issubset(1:5, 2:4)
        @test issubset(Base.OneTo(5), Base.OneTo(10))
        @test !issubset(Base.OneTo(10), Base.OneTo(5))
        @test issubset(1:3:10, 1:10)
        @test !issubset(1:10, 1:3:10)
        # with empty ranges
        @test issubset(2:1, 3:4) #35225
        @test issubset(2:1, 3:2)
        @test issubset(Base.OneTo(0), Base.OneTo(3))
        @test issubset(Base.OneTo(0), Base.OneTo(-3))
    end
    @testset "sort/sort!/partialsort" begin
        @test sort(UnitRange(1,2)) == UnitRange(1,2)
        @test sort!(UnitRange(1,2)) == UnitRange(1,2)
        @test sort(1:10, rev=true) == 10:-1:1
        @test sort(-3:3, by=abs) == [0,-1,1,-2,2,-3,3]
        @test partialsort(1:10, 4) == 4

        @testset "offset ranges" begin
            x = OffsetArrays.IdOffsetRange(values=4:13, indices=4:13)
            @test sort(x) === x === sort!(x)
            @test sortperm(x) == eachindex(x)
            @test issorted(x[sortperm(x)])
        end
    end
    @testset "in" begin
        @test 0 in UInt(0):100:typemax(UInt)
        @test last(UInt(0):100:typemax(UInt)) in UInt(0):100:typemax(UInt)
        @test -9223372036854775790 in -9223372036854775790:100:9223372036854775710
        @test -9223372036854775690 in -9223372036854775790:100:9223372036854775710
        @test -90 in -9223372036854775790:100:9223372036854775710
        @test 10 in -9223372036854775790:100:9223372036854775710
        @test 110 in -9223372036854775790:100:9223372036854775710
        @test 9223372036854775610 in -9223372036854775790:100:9223372036854775710
        @test 9223372036854775710 in -9223372036854775790:100:9223372036854775710


        @test !(3.5 in 1:5)
        @test (3 in 1:5)
        @test (3 in 5:-1:1)
        @test (3 in 3 .+ 0*(1:5))
        @test !(4 in 3 .+ 0*(1:5))
        @test 0. in (0. .* (1:10))
        @test !(0.1 in (0. .* (1:10)))

        let r = 0.0:0.01:1.0
            @test (r[30] in r)
        end
        let r = (-4*Int64(maxintfloat(Int === Int32 ? Float32 : Float64))):5
            @test (3 in r)
            @test (3.0 in r)
        end

        @test !(1 in 1:0)
        @test !(1.0 in 1.0:0.0)

        for r = (1:10, 1//1:10//1, 1:2:5, 1//2:1//2:5//2, 1.0:5.0, LinRange(1.5, 5.5, 9)),
            x = (NaN16, Inf32, -Inf64, 1//0, -1//0)
            @test !(x in r)
        end

        @test 1e40 ∉ 0:1.0 # Issue #45747
        @test 1e20 ∉ 0:1e-20:1e-20
        @test 1e20 ∉ 0:1e-20
        @test 1.0  ∉ 0:1e-20:1e-20
        @test 0.5  ∉ 0:1e-20:1e-20
        @test 1    ∉ 0:1e-20:1e-20

        @test_broken 17.0 ∈ 0:1e40 # Don't support really long ranges
    end
    @testset "in() works across types, including non-numeric types (#21728 and #45646)" begin
        @test 1//1 in 1:3
        @test 1//1 in 1.0:3.0
        @test !(5//1 in 1:3)
        @test !(5//1 in 1.0:3.0)
        @test Complex(1, 0) in 1:3
        @test Complex(1, 0) in 1.0:3.0
        @test Complex(1.0, 0.0) in 1:3
        @test Complex(1.0, 0.0) in 1.0:3.0
        @test !(Complex(1, 1) in 1:3)
        @test !(Complex(1, 1) in 1.0:3.0)
        @test !(Complex(1.0, 1.0) in 1:3)
        @test !(Complex(1.0, 1.0) in 1.0:3.0)
        @test !(π in 1:3)
        @test !(π in 1.0:3.0)
        @test !("a" in 1:3)
        @test !("a" in 1.0:3.0)
        @test !(1 in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))
        @test !(Complex(1, 0) in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))
        @test !(π in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))
        @test !("a" in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))

        # We use Ducks because of their propensity to stand in a row and because we know
        # that no additional methods (e.g. isfinite) are defined specifically for Ducks.
        struct Duck
            location::Int
        end
        Base.:+(x::Duck, y::Int) = Duck(x.location + y)
        Base.:-(x::Duck, y::Int) = Duck(x.location - y)
        Base.:-(x::Duck, y::Duck) = x.location - y.location
        Base.isless(x::Duck, y::Duck) = isless(x.location, y.location)

        @test Duck(3) ∈ Duck(1):2:Duck(5)
        @test Duck(3) ∈ Duck(5):-2:Duck(2)
        @test Duck(4) ∉ Duck(5):-2:Duck(1)
        @test Duck(4) ∈ Duck(1):Duck(5)
        @test Duck(0) ∉ Duck(1):Duck(5)
    end
end
@testset "indexing range with empty range (#4309)" begin
    @test (@inferred (3:6)[5:4]) === 7:6
    @test_throws BoundsError (3:6)[5:5]
    @test_throws BoundsError (3:6)[5]
    @test (@inferred (0:2:10)[7:6]) === 12:2:11
    @test_throws BoundsError (0:2:10)[7:7]

    for start in [true], stop in [true, false]
        @test (@inferred (start:stop)[1:0]) === true:false
    end
    @test (@inferred (true:false)[true:false]) == true:false

    @testset "issue #40760" begin
        empty_range = 1:0
        r = range(false, length = 0)
        @test r isa UnitRange && first(r) == 0 && last(r) == -1
        r = (true:true)[empty_range]
        @test r isa UnitRange && first(r) == true && last(r) == false
        @testset for r in Any[true:true, true:true:true, 1:2, 1:1:2]
            @test (@inferred r[1:0]) isa AbstractRange
            @test r[1:0] == empty_range
            @test (@inferred r[1:1:0]) isa AbstractRange
            @test r[1:1:0] == empty_range
        end
    end
end
# indexing with negative ranges (#8351)
for a=AbstractRange[3:6, 0:2:10], b=AbstractRange[0:1, 2:-1:0]
    @test_throws BoundsError a[b]
end

# avoiding intermediate overflow (#5065)
@test length(1:4:typemax(Int)) == div(typemax(Int), 4) + 1
@test checked_length(1:4:typemax(Int)) == div(typemax(Int), 4) + 1 # computed exactly in modulo arithmetic

@testset "overflow in length" begin
    Tset = Int === Int64 ? (Int, UInt, Int128, UInt128) :
                           (Int, UInt, Int64, UInt64, Int128, UInt128)
    for T in Tset
        @test length(zero(T):typemax(T)) == typemin(T)
        @test length(typemin(T):typemax(T)) == T(0)
        @test length(zero(T):one(T):typemax(T)) == typemin(T)
        @test length(typemin(T):one(T):typemax(T)) == T(0)
        @test_throws OverflowError checked_length(zero(T):typemax(T))
        @test_throws OverflowError checked_length(typemin(T):typemax(T))
        @test_throws OverflowError checked_length(zero(T):one(T):typemax(T))
        @test_throws OverflowError checked_length(typemin(T):one(T):typemax(T))
        @test length(one(T):typemax(T)) == checked_length(one(T):typemax(T)) == typemax(T)
        if T <: Signed
            @test length(-one(T):typemax(T)-one(T)) == typemin(T)
            @test length(-one(T):one(T):typemax(T)-one(T)) == typemin(T)
            @test length(-one(T):typemax(T)) == typemin(T) + T(1)
            @test length(zero(T):typemin(T):typemin(T)) == 2
            @test length(one(T):typemin(T):typemin(T)) == 2
            @test length(typemax(T):typemin(T):typemin(T)) == 2
            @test length(-one(T):typemin(T):typemin(T)) == 1
            @test length(zero(T):typemin(T):zero(T)) == 1
            @test length(zero(T):typemin(T):one(T)) == 0
            @test_throws OverflowError checked_length(-one(T):typemax(T)-one(T))
            @test_throws OverflowError checked_length(-one(T):one(T):typemax(T)-one(T))
            @test_throws InexactError checked_length(zero(T):typemin(T):typemin(T)) == 2 # this can be improved
            @test_throws InexactError checked_length(one(T):typemin(T):typemin(T)) == 2 # this can  be improved
            @test_throws InexactError checked_length(typemax(T):typemin(T):typemin(T)) == 2 # this can  be improved
        end
    end
end

# A number type with the overflow behavior of `UInt8`. Conversion to `Integer` returns an
# `Int32`, i.e., a type with different `typemin`/`typemax`. See  #41479
struct OverflowingReal <: Real
    val::UInt8
end
OverflowingReal(x::OverflowingReal) = x
Base.:<(x::OverflowingReal, y::OverflowingReal) = x.val < y.val
Base.:(==)(x::OverflowingReal, y::OverflowingReal) = x.val == y.val
Base.:<=(x::OverflowingReal, y::OverflowingReal) = x.val <= y.val
Base.:+(x::OverflowingReal, y::OverflowingReal) = OverflowingReal(x.val + y.val)
Base.:-(x::OverflowingReal, y::OverflowingReal) = OverflowingReal(x.val - y.val)
Base.round(x::OverflowingReal, ::RoundingMode) = x
Base.Integer(x::OverflowingReal) = Int32(x.val)
@test length(OverflowingReal(1):OverflowingReal(0)) == 0

@testset "loops involving typemin/typemax" begin
    n = 0
    s = 0
    # loops ending at typemax(Int)
    for i = (typemax(Int)-1):typemax(Int)
        s += 1
        @test s <= 2
    end
    @test s == 2

    s = 0
    for i = (typemax(Int)-2):(typemax(Int)-1)
        s += 1
        @test s <= 2
    end
    @test s == 2

    s = 0
    for i = typemin(Int):(typemin(Int)+1)
        s += 1
        @test s <= 2
    end
    @test s == 2

    # loops covering the full range of integers
    s = 0
    for i = typemin(UInt8):typemax(UInt8)
        s += 1
    end
    @test s == 256

    s = 0
    for i = typemin(UInt):typemax(UInt)
        i == 10 && break
        s += 1
    end
    @test s == 10

    s = 0
    for i = typemin(UInt8):one(UInt8):typemax(UInt8)
        s += 1
    end
    @test s == 256

    s = 0
    for i = typemin(UInt):1:typemax(UInt)
        i == 10 && break
        s += 1
    end
    @test s == 10

    # loops past typemax(Int)
    n = 0
    s = Int128(0)
    for i = typemax(UInt64)-2:typemax(UInt64)
        n += 1
        s += i
    end
    @test n == 3
    @test s == 3*Int128(typemax(UInt64)) - 3

    # loops over empty ranges
    s = 0
    for i = 0xff:0x00
        s += 1
    end
    @test s == 0

    s = 0
    for i = Int128(typemax(Int128)):Int128(typemin(Int128))
        s += 1
    end
    @test s == 0
end

@testset "sums of ranges" begin
    @test sum(1:100) == 5050
    @test sum(0:100) == 5050
    @test sum(-100:100) == 0
    @test sum(0:2:100) == 2550
end
@testset "overflowing sums (see #5798)" begin
    if Sys.WORD_SIZE == 64
        @test sum(Int128(1):10^18) == div(10^18 * (Int128(10^18)+1), 2)
        @test sum(Int128(1):10^18-1) == div(10^18 * (Int128(10^18)-1), 2)
    else
        @test sum(Int64(1):10^9) == div(10^9 * (Int64(10^9)+1), 2)
        @test sum(Int64(1):10^9-1) == div(10^9 * (Int64(10^9)-1), 2)
    end
end
@testset "Tricky sums of StepRangeLen #8272" begin
    @test sum(10000.:-0.0001:0) == 5.00000005e11
    @test sum(0:0.001:1) == 500.5
    @test sum(0:0.000001:1) == 500000.5
    @test sum(0:0.1:10) == 505.
end
@testset "broadcasted operations with scalars" for T in (Int, UInt, Int128)
    @test broadcast(-, T(1):3, 2) === T(1)-2:1
    @test broadcast(-, T(1):3, 0.25) === range(T(1)-0.25, length=T(3)) == T(1)-0.25:3-0.25
    @test broadcast(+, T(1):3) === T(1):3
    @test broadcast(+, T(1):3, 2) === T(3):5
    @test broadcast(+, T(1):3, 0.25) === range(T(1)+0.25, length=T(3)) == T(1)+0.25:3+0.25
    @test broadcast(+, T(1):2:6, 1) === T(2):2:6
    @test broadcast(+, T(1):2:6, 0.3) === range(T(1)+0.3, step=2, length=T(3)) == T(1)+0.3:2:5+0.3
    @test broadcast(-, T(1):2:6, 1) === T(0):2:4
    @test broadcast(-, T(1):2:6, 0.3) === range(T(1)-0.3, step=2, length=T(3)) == T(1)-0.3:2:5-0.3
    is_unsigned = T <: Unsigned
    @test length(broadcast(-, T(1):3, 2)) === length(T(1)-2:T(3)-2) === (is_unsigned ? T(0) : T(3))
    @test broadcast(-, T(1):3) == -T(1):-1:-T(3)
    @test broadcast(-, 2, T(1):3) == T(1):-1:-T(1)
end
@testset "operations between ranges and arrays" for T in (Int, UInt, Int128)
    @test all(([T(1):5;] + (T(5):-1:1)) .=== T(6))
    @test all(((T(5):-1:1) + [T(1):5;]) .=== T(6))
    @test all(([T(1):5;] - (T(1):5)) .=== T(0))
    @test all(((T(1):5) - [T(1):5;]) .=== T(0))
end
@testset "issue #32442: Broadcasting over views with non-`Int` indices" begin
    a=rand(UInt32,20)
    c=rand(UInt64,5)
    @test reinterpret(UInt64,view(a,UInt64.(11:20))) .- c ==
          reinterpret(UInt64,view(a,(11:20))) .- c ==
          reinterpret(UInt64,view(a,(UInt64(11):UInt64(20)))) .- c ==
          copy(reinterpret(UInt64,view(a,(UInt64(11):UInt64(20))))) .- c

    @test view(a,(Int32(11):Int32(20))) .+ [1] == a[11:20] .+ 1
end
@testset "tricky floating-point ranges" begin
    for (start, step, stop, len) in ((1, 1, 3, 3), (0, 1, 3, 4),
                                    (3, -1, -1, 5), (1, -1, -3, 5),
                                    (0, 1, 10, 11), (0, 7, 21, 4),
                                    (0, 11, 33, 4), (1, 11, 34, 4),
                                    (0, 13, 39, 4), (1, 13, 40, 4),
                                    (11, 11, 33, 3), (3, 1, 11, 9),
                                    (0, 10, 55, 0), (0, -1, 5, 0), (0, 10, 5, 0),
                                    (0, 1, 5, 0), (0, -10, 5, 0), (0, -10, 0, 1),
                                    (0, -1, 1, 0), (0, 1, -1, 0), (0, -1, -10, 11))
        r = start/10:step/10:stop/10
        a = Vector(start:step:stop)./10
        ra = Vector(r)

        @test r == a
        @test isequal(r, a)

        @test r == ra
        @test isequal(r, ra)

        @test hash(r) == hash(a)
        @test hash(r) == hash(ra)

        if len > 0
            l = range(start/10, stop=stop/10, length=len)
            la = Vector(l)

            @test a == l
            @test r == l
            @test isequal(a, l)
            @test isequal(r, l)

            @test l == la
            @test isequal(l, la)

            @test hash(l) == hash(a)
            @test hash(l) == hash(la)
        end
    end

    @test 1.0:1/49:27.0 == range(1.0, stop=27.0, length=1275) == [49:1323;]./49
    @test isequal(1.0:1/49:27.0, range(1.0, stop=27.0, length=1275))
    @test isequal(1.0:1/49:27.0, Vector(49:1323)./49)
    @test hash(1.0:1/49:27.0) == hash(range(1.0, stop=27.0, length=1275)) == hash(Vector(49:1323)./49)

    @test [prevfloat(0.1):0.1:0.3;] == [prevfloat(0.1), 0.2, 0.3]
    @test [nextfloat(0.1):0.1:0.3;] == [nextfloat(0.1), 0.2]
    @test [prevfloat(0.0):0.1:0.3;] == [prevfloat(0.0), 0.1, 0.2]
    @test [nextfloat(0.0):0.1:0.3;] == [nextfloat(0.0), 0.1, 0.2]
    @test [0.1:0.1:prevfloat(0.3);] == [0.1, 0.2]
    @test [0.1:0.1:nextfloat(0.3);] == [0.1, 0.2, nextfloat(0.3)]
    @test [0.0:0.1:prevfloat(0.3);] == [0.0, 0.1, 0.2]
    @test [0.0:0.1:nextfloat(0.3);] == [0.0, 0.1, 0.2, nextfloat(0.3)]
    @test [0.1:prevfloat(0.1):0.3;] == [0.1, 0.2, 0.3]
    @test [0.1:nextfloat(0.1):0.3;] == [0.1, 0.2]
    @test [0.0:prevfloat(0.1):0.3;] == [0.0, prevfloat(0.1), prevfloat(0.2), 0.3]
    @test [0.0:nextfloat(0.1):0.3;] == [0.0, nextfloat(0.1), nextfloat(0.2)]
end

function loop_range_values(::Type{T}) where T
    for a = -5:25,
        s = [-5:-1; 1:25; ],
        d = 1:25,
        n = -1:15

        denom = convert(T, d)
        strt = convert(T, a)/denom
        Δ     = convert(T, s)/denom
        stop  = convert(T, (a + (n - 1) * s)) / denom
        vals  = T[a:s:(a + (n - 1) * s); ] ./ denom
        r = strt:Δ:stop
        @test [r;] == vals
        @test [range(strt, stop=stop, length=length(r));] == vals
        n = length(r)
        @test [r[1:n];] == [r;]
        @test [r[2:n];] == [r;][2:end]
        @test [r[1:3:n];] == [r;][1:3:n]
        @test [r[2:2:n];] == [r;][2:2:n]
        @test [r[n:-1:2];] == [r;][n:-1:2]
        @test [r[n:-2:1];] == [r;][n:-2:1]
    end
end

@testset "issue #7420 for type $T" for T = (Float32, Float64,) # BigFloat),
    loop_range_values(T)
end

@testset "issue #20373 (unliftable ranges with exact end points)" begin
    @test [3*0.05:0.05:0.2;]    == [range(3*0.05, stop=0.2, length=2);]   == [3*0.05,0.2]
    @test [0.2:-0.05:3*0.05;]   == [range(0.2, stop=3*0.05, length=2);]   == [0.2,3*0.05]
    @test [-3*0.05:-0.05:-0.2;] == [range(-3*0.05, stop=-0.2, length=2);] == [-3*0.05,-0.2]
    @test [-0.2:0.05:-3*0.05;]  == [range(-0.2, stop=-3*0.05, length=2);] == [-0.2,-3*0.05]
end

function range_fuzztests(::Type{T}, niter, nrange) where {T}
    for i = 1:niter, n in nrange
        strt, Δ = randn(T), randn(T)
        Δ == 0 && continue
        stop = strt + (n-1)*Δ
        # `n` is not necessarily unique s.t. `strt + (n-1)*Δ == stop`
        # so test that `length(strt:Δ:stop)` satisfies this identity
        # and is the closest value to `(stop-strt)/Δ` to do so
        lo = hi = n
        while strt + (lo-1)*Δ == stop; lo -= 1; end
        while strt + (hi-1)*Δ == stop; hi += 1; end
        m = clamp(round(Int, (stop-strt)/Δ) + 1, lo+1, hi-1)
        r = strt:Δ:stop
        @test m == length(r)
        @test strt == first(r)
        @test Δ == step(r)
        # potential floating point error:
        #   stop = strt + (n-1)*Δ
        #      *          error <= eps((n-1)*Δ)/2 <= abs((n-1)*Δ)/2 * eps(T)
        #      +          error <= eps(stop)/2    <= abs(stop)/2    * eps(T)
        #   last(r)
        #     rat(strt)   error <= eps(strt)/2    <= abs(strt)/2    * eps(T)
        #     rat(Δ)      error <= (n-1)*eps(Δ)/2 <= abs((n-1)*Δ)/2 * eps(T)
        #     T(...)      error <= eps(last(r))/2 <= abs(stop)/2    * eps(T)
        @test stop ≈ last(r) atol = (abs(strt)/2 + (n-1)*abs(Δ) + abs(stop)) * eps(T)
        l = range(strt, stop=stop, length=n)
        @test n == length(l)
        @test strt == first(l)
        @test stop == last(l)
    end
end
@testset "range fuzztests for $T" for T = (Float32, Float64,)
    range_fuzztests(T, 2^15, 1:5)
end

@testset "Inexact errors on 32 bit architectures. #22613" begin
    @test first(range(log(0.2), stop=log(10.0), length=10)) == log(0.2)
    @test last(range(log(0.2), stop=log(10.0), length=10)) == log(10.0)
end

@testset "ranges with very small endpoints for type $T" for T = (Float32, Float64)
    z = zero(T)
    u = eps(z)
    @test first(range(u, stop=u, length=0)) == u
    @test last(range(u, stop=u, length=0)) == u
    @test first(range(-u, stop=u, length=0)) == -u
    @test last(range(-u, stop=u, length=0)) == u
    @test [range(-u, stop=u, length=0);] == []
    @test [range(-u, stop=-u, length=1);] == [-u]
    @test [range(-u, stop=u, length=2);] == [-u,u]
    @test [range(-u, stop=u, length=3);] == [-u,0,u]
    @test first(range(-u, stop=-u, length=0)) == -u
    @test last(range(-u, stop=-u, length=0)) == -u
    @test first(range(u, stop=-u, length=0)) == u
    @test last(range(u, stop=-u, length=0)) == -u
    @test [range(u, stop=-u, length=0);] == []
    @test [range(u, stop=u, length=1);] == [u]
    @test [range(u, stop=-u, length=2);] == [u,-u]
    @test [range(u, stop=-u, length=3);] == [u,0,-u]
    v = range(-u, stop=u, length=12)
    @test length(v) == 12
    @test [-3u:u:3u;] == [range(-3u, stop=3u, length=7);] == [-3:3;].*u
    @test [3u:-u:-3u;] == [range(3u, stop=-3u, length=7);] == [3:-1:-3;].*u
end

@testset "range with very large endpoints for type $T" for T = (Float32, Float64)
    largeint = Int(min(maxintfloat(T), typemax(Int)))
    a = floatmax()
    for i = 1:5
        @test [range(a, stop=a, length=1);] == [a]
        @test [range(-a, stop=-a, length=1);] == [-a]
        b = floatmax()
        for j = 1:5
            @test [range(-a, stop=b, length=0);] == []
            @test [range(-a, stop=b, length=2);] == [-a,b]
            @test [range(-a, stop=b, length=3);] == [-a,(b-a)/2,b]
            @test [range(a, stop=-b, length=0);] == []
            @test [range(a, stop=-b, length=2);] == [a,-b]
            @test [range(a, stop=-b, length=3);] == [a,(a-b)/2,-b]
            for c = largeint-3:largeint
                s = range(-a, stop=b, length=c)
                @test first(s) == -a
                @test last(s) == b
                @test length(s) == c
                s = range(a, stop=-b, length=c)
                @test first(s) == a
                @test last(s) == -b
                @test length(s) == c
            end
            b = prevfloat(b)
        end
        a = prevfloat(a)
    end
    @test (1:2:3)[StepRangeLen{Bool}(true,-1,2)] == [1]
end

# issue #20380
let r = LinRange(1,4,4)
    @test isa(r[1:4], LinRange)
end

@testset "range with 1 or 0 elements (whose step length is NaN)" begin
    @test issorted(range(1, stop=1, length=0))
    @test issorted(range(1, stop=1, length=1))
end
# near-equal ranges
@test 0.0:0.1:1.0 != 0.0f0:0.1f0:1.0f0

# comparing and hashing ranges
@testset "comparing and hashing ranges" begin
    Rs = AbstractRange[1:1, 1:1:1, 1:2, 1:1:2,
                       map(Int32,1:3:17), map(Int64,1:3:17), 1:0, 1:-1:0, 17:-3:0,
                       0.0:0.1:1.0, map(Float32,0.0:0.1:1.0),map(Float32,LinRange(0.0, 1.0, 11)),
                       1.0:eps():1.0 .+ 10eps(), 9007199254740990.:1.0:9007199254740994,
                       range(0, stop=1, length=20), map(Float32, range(0, stop=1, length=20)),
                       3:2, 5:-2:7, range(0.0, step=2.0, length=0), 3//2:3//2:0//1, LinRange(2,3,0),
                       Base.OneTo(1), 1:1, 1:-3:1, 1//1:1//3:1//1, range(1.0, step=2.5, length=1),
                       LinRange(1,1,1), LinRange(1,1,2)]
    for r in Rs
        local r
        ar = Vector(r)
        @test r == ar
        @test isequal(r,ar)
        @test hash(r) == hash(ar)
        for s in Rs
            as = Vector(s)
            @test isequal(r,s) == (hash(r)==hash(s))
            @test (r==s) == (ar==as)
        end
    end
end

@testset "comparing UnitRanges and OneTo" begin
    @test 1:2:10 == 1:2:10 != 1:3:10 != 1:3:13 != 2:3:13 == 2:3:11 != 2:11
    @test 1:1:10 == 1:10 == 1:10 == Base.OneTo(10) == Base.OneTo(10)
    @test 1:10 != 2:10 != 2:11 != Base.OneTo(11)
    @test Base.OneTo(10) != Base.OneTo(11) != 1:10
    @test Base.OneTo(0) == 5:4
end
# issue #2959
@test 1.0:1.5 == 1.0:1.0:1.5 == 1.0:1.0
@test_broken 1.0:(.3-.1)/.1 == 1.0:2.0 # (this is just shy of 2.0)

@testset "length with typemin/typemax" begin
    let r = typemin(Int64):2:typemax(Int64)
        @test first(r) == typemin(Int64)
        @test last(r) == typemax(Int64) - 1
        @test length(r) == typemin(Int64)
        @test_throws OverflowError checked_length(r)
    end
    let r = typemax(Int64):-2:typemin(Int64)
        @test first(r) == typemax(Int64)
        @test last(r) == typemin(Int64) + 1
        @test length(r) == typemin(Int64)
        @test_throws OverflowError checked_length(r)
    end

    let r = typemin(Int64):3:typemax(Int64)
        @test length(r) == checked_length(r) == 6148914691236517206
    end
    let r = typemax(Int64):-3:typemin(Int64)
        @test length(r) == checked_length(r) == 6148914691236517206
    end

    for s in 3:100
        r = typemin(Int):s:typemax(Int)
        br = big(typemin(Int)):big(s):big(typemax(Int))
        @test length(r) == checked_length(r) == length(br)

        r = typemax(Int):-s:typemin(Int)
        br = big(typemax(Int)):big(-s):big(typemin(Int))
        @test length(r) == checked_length(r) == length(br)
    end

    @test length(UInt(1):UInt(1):UInt(0)) == checked_length(UInt(1):UInt(1):UInt(0)) == 0
    @test length(typemax(UInt):UInt(1):(typemax(UInt)-1)) == checked_length(typemax(UInt):UInt(1):(typemax(UInt)-1)) == 0
    @test length(typemax(UInt):UInt(2):(typemax(UInt)-1)) == checked_length(typemax(UInt):UInt(2):(typemax(UInt)-1)) == 0
    @test length((typemin(Int)+3):5:(typemin(Int)+1)) == checked_length((typemin(Int)+3):5:(typemin(Int)+1)) == 0
end

# issue #6364
@test length((1:64)*(pi/5)) == 64

@testset "issue #6973" begin
    r1 = 1.0:0.1:2.0
    r2 = 1.0f0:0.2f0:3.0f0
    r3 = 1:2:21
    @test r1 + r1 == 2*r1
    @test r1 + r2 == 2.0:0.3:5.0
    @test (r1 + r2) - r2 == r1
    @test r1 + r3 == convert(StepRangeLen{Float64}, r3) + r1
    @test r3 + r3 == 2 * r3
end

@testset "issue #7114" begin
    let r = -0.004532318104333742:1.2597349521122731e-5:0.008065031416788989
        @test length(r[1:end-1]) == length(r) - 1
        @test isa(r[1:2:end],AbstractRange) && length(r[1:2:end]) == div(length(r)+1, 2)
        @test r[3:5][2] ≈ r[4]
        @test r[5:-2:1][2] ≈ r[3]
        @test_throws BoundsError r[0:10]
        @test_throws BoundsError r[1:10000]
    end

    let r = range(1/3, stop=5/7, length=6)
        @test length(r) == 6
        @test r[1] == 1/3
        @test abs(r[end] - 5/7) <= eps(5/7)
    end

    let r = range(0.25, stop=0.25, length=1)
        @test length(r) == 1
        @test_throws ArgumentError range(0.25, stop=0.5, length=1)
    end
end

# issue #7426
@test [typemax(Int):1:typemax(Int);] == [typemax(Int)]

#issue #7484
let r7484 = 0.1:0.1:1
    @test [reverse(r7484);] == reverse([r7484;])
end

@testset "issue #7387" begin
    for r in (0:1, 0.0:1.0)
        local r
        @test [r .+ im;] == [r;] .+ im
        @test [r .- im;] == [r;] .- im
        @test [r * im;] == [r;] * im
        @test [r / im;] == [r;] / im
    end
end
# Preservation of high precision upon addition
let r = (-0.1:0.1:0.3) + broadcast(+, -0.3:0.1:0.1, 1e-12)
    @test r[3] == 1e-12
end

@testset "issue #7709" begin
    @test length(map(identity, 0x01:0x05)) == 5
    @test length(map(identity, 0x0001:0x0005)) == 5
    @test length(map(identity, UInt64(1):UInt64(5))) == 5
    @test length(map(identity, UInt128(1):UInt128(5))) == 5
end
@testset "issue #8531, issue #29801" begin
    smallint = (Int === Int64 ?
                (Int8, UInt8, Int16, UInt16, Int32, UInt32) :
                (Int8, UInt8, Int16, UInt16))
    for T in smallint
        s = typemin(T):typemax(T)
        @test length(s) === checked_length(s) === 2^(8*sizeof(T))
        s = T(10):typemax(T):T(10)
        @test length(s) === checked_length(s) === 1
        s = T(10):typemax(T):T(0)
        @test length(s) === checked_length(s) === 0
        s = T(10):typemax(T):typemin(T)
        @test length(s) === checked_length(s) === 0
    end
end

# issue #8584
@test (0:1//2:2)[1:2:3] == 0:1//1:1

# issue #12278
@test length(1:UInt(0)) == checked_length(1:UInt(0)) == 0

@testset "zip" begin
    i = 0
    x = 1:2:8
    y = 2:2:8
    xy = 1:8
    for (thisx, thisy) in zip(x, y)
        @test thisx == xy[i+=1]
        @test thisy == xy[i+=1]
    end
end

@testset "issue #9962" begin
    @test eltype(0:1//3:10) <: Rational
    @test (0:1//3:10)[1] == 0
    @test (0:1//3:10)[2] == 1//3
end
@testset "converting ranges (issue #10965)" begin
    @test promote(0:1, UInt8(2):UInt8(5)) === (0:1, 2:5)
    @test convert(UnitRange{Int}, 0:5) === 0:5
    @test convert(UnitRange{Int128}, 0:5) === Int128(0):Int128(5)

    @test promote(0:1:1, UInt8(2):UInt8(1):UInt8(5)) === (0:1:1, 2:1:5)
    @test convert(StepRange{Int,Int}, 0:1:1) === 0:1:1
    @test convert(StepRange{Int128,Int128}, 0:1:1) === Int128(0):Int128(1):Int128(1)

    @test promote(0:1:1, 2:5) === (0:1:1, 2:1:5)
    @test convert(StepRange{Int128,Int128}, 0:5) === Int128(0):Int128(1):Int128(5)
    @test convert(StepRange, 0:5) === 0:1:5
    @test convert(StepRange{Int128,Int128}, 0.:5) === Int128(0):Int128(1):Int128(5)

    @test_throws ArgumentError StepRange(1.1,1,5.1)

    @test promote(0f0:inv(3f0):1f0, 0.:2.:5.) === (0:1/3:1, 0.:2.:5.)

    @test convert(StepRangeLen{Float64}, 0:1/3:1) === 0:1/3:1
    @test convert(StepRangeLen{Float64}, 0f0:inv(3f0):1f0) === 0:1/3:1

    @test promote(0:1/3:1, 0:5) === (0:1/3:1, 0.:1.:5.)
    @test convert(StepRangeLen{Float64}, 0:5) === 0.:1.:5.
    @test convert(StepRangeLen{Float64}, 0:1:5) === 0.:1.:5.
    @test convert(StepRangeLen, 0:5) == 0:5
    @test convert(StepRangeLen, 0:1:5) == 0:1:5

    @test convert(LinRange{Float64}, 0.0:0.1:0.3) === LinRange{Float64}(0.0, 0.3, 4)
    @test convert(LinRange, 0.0:0.1:0.3) === LinRange{Float64}(0.0, 0.3, 4)
    @test convert(LinRange, 0:3) === LinRange{Int}(0, 3, 4)

    @test promote('a':'z', 1:2) === ('a':'z', 1:1:2)
    @test eltype(['a':'z', 1:2]) == (StepRange{T,Int} where T)
end

@testset "Ranges with <:Integer eltype but non-integer step (issue #32419)" begin
    @test eltype(StepRange(1, 1//1, 2)) === Int
    @test_throws ArgumentError StepRange(1, 1//2, 2)
    @test eltype(StepRangeLen{Int}(1, 1//1, 2)) === Int
    @test_throws ArgumentError StepRangeLen{Int}(1, 1//2, 2)
    @test eltype(LinRange{Int}(1, 5, 3)) === Int
    @test_throws ArgumentError LinRange{Int}(1, 5, 4)
end

@testset "LinRange ops" begin
    @test 2*LinRange(0,3,4) == LinRange(0,6,4)
    @test LinRange(0,3,4)*2 == LinRange(0,6,4)
    @test LinRange(0,3,4)/3 == LinRange(0,1,4)
    @test broadcast(-, 2, LinRange(0,3,4)) == LinRange(2,-1,4)
    @test broadcast(+, 2, LinRange(0,3,4)) == LinRange(2,5,4)
    @test -LinRange{Int}(0,3,4) === LinRange{Int}(0,-3,4)
    @test -LinRange{Float64}(0.,3.,4) === LinRange{Float64}(-0.,-3.,4)
    @test reverse(LinRange{Int}(0,3,4)) === LinRange{Int}(3,0,4)
    @test reverse(LinRange{Float64}(0.,3.,4)) === LinRange{Float64}(3.,0.,4)
end

# issue #11245
@test repr(range(1, stop=2, length=3)) == "1.0:0.5:2.0"

@testset "issue 10950" begin
    r = 1//2:3
    @test length(r) == 3
    @test checked_length(r) == 3
    i = 1
    for x in r
        @test x == i//2
        i += 2
    end
    @test i == 7
end

@testset "repr" begin
    # repr/show should display the range nicely
    # to test print_range in range.jl
    replrepr(x) = repr("text/plain", x; context=IOContext(stdout, :limit=>true, :displaysize=>(24, 80)))
    nb = Sys.WORD_SIZE
    @test replrepr(1:4) == "1:4"
    @test repr("text/plain", 1:4) == "1:4"
    @test repr("text/plain", range(1, stop=5, length=7)) == "1.0:0.6666666666666666:5.0"
    @test repr("text/plain", LinRange{Float64}(1,5,7)) == "7-element LinRange{Float64, Int$nb}:\n 1.0, 1.66667, 2.33333, 3.0, 3.66667, 4.33333, 5.0"
    @test repr(range(1, stop=5, length=7)) == "1.0:0.6666666666666666:5.0"
    @test repr(LinRange{Float64}(1,5,7)) == "LinRange{Float64}(1.0, 5.0, 7)"
    @test replrepr(0:100.) == "0.0:1.0:100.0"
    # next is to test a very large range, which should be fast because print_range
    # only examines spacing of the left and right edges of the range, sufficient
    # to cover the designated screen size.
    @test replrepr(range(0, stop=100, length=10000)) == "0.0:0.010001000100010001:100.0"
    @test replrepr(LinRange{Float64}(0,100, 10000)) == "10000-element LinRange{Float64, Int$nb}:\n 0.0, 0.010001, 0.020002, 0.030003, …, 99.96, 99.97, 99.98, 99.99, 100.0"

    @test sprint(show, UnitRange(1, 2)) == "1:2"
    @test sprint(show, StepRange(1, 2, 5)) == "1:2:5"

    @test sprint(show, LinRange{Float32}(1.5, 2.5, 10)) == "LinRange{Float32}(1.5, 2.5, 10)"
end

@testset "Issue 11049, and related" begin
    @test promote(range(0f0, stop=1f0, length=3), range(0., stop=5., length=2)) ===
        (range(0., stop=1., length=3), range(0., stop=5., length=2))
    @test convert(LinRange{Float64}, range(0., stop=1., length=3)) === LinRange(0., 1., 3)
    @test convert(LinRange{Float64}, range(0f0, stop=1f0, length=3)) === LinRange(0., 1., 3)

    @test promote(range(0., stop=1., length=3), 0:5) === (range(0., stop=1., length=3),
                                                 range(0., stop=5., length=6))
    @test convert(LinRange{Float64}, 0:5) === LinRange(0., 5., 6)
    @test convert(LinRange{Float64}, 0:1:5) === LinRange(0., 5., 6)
    @test convert(LinRange, 0:5) === LinRange{Int}(0, 5, 6)
    @test convert(LinRange, 0:1:5) === LinRange{Int}(0, 5, 6)

    function test_range_index(r, s)
        @test typeof(r[s]) == typeof(r)
        @test [r;][s] == [r[s];]
    end
    test_range_index(range(0.1, stop=0.3, length=3), 1:2)
    test_range_index(range(0.1, stop=0.3, length=3), 1:0)
    test_range_index(range(1.0, stop=1.0, length=1), 1:1)
    test_range_index(range(1.0, stop=1.0, length=1), 1:0)
    test_range_index(range(1.0, stop=2.0, length=0), 1:0)

    function test_range_identity(r::AbstractRange{T}, mr) where T
        @test -r == mr
        @test -Vector(r) == Vector(mr)
        @test isa(-r, typeof(r))

        @test broadcast(+, broadcast(+, 1, r), -1) == r
        @test 1 .+ Vector(r) == Vector(1 .+ r) == Vector(r .+ 1)
        @test isa(broadcast(+, broadcast(+, 1, r), -1), typeof(r))
        @test broadcast(-, broadcast(-, 1, r), 1) == mr
        @test 1 .- Vector(r) == Vector(1 .- r) == Vector(1 .+ mr)
        @test Vector(r) .- 1 == Vector(r .- 1) == -Vector(mr .+ 1)
        @test isa(broadcast(-, broadcast(-, 1, r), 1), typeof(r))

        @test 1 * r * 1 == r
        @test 2 * r * T(0.5) == r
        @test isa(1 * r * 1, typeof(r))
        @test r / 1 == r
        @test r / 2 * 2 == r
        @test r / T(0.5) * T(0.5) == r
        @test isa(r / 1, typeof(r))

        @test (2 * Vector(r) == Vector(r * 2) == Vector(2 * r) ==
               Vector(r * T(2.0)) == Vector(T(2.0) * r) ==
               Vector(r / T(0.5)) == -Vector(mr * T(2.0)))
    end

    test_range_identity(range(1.0, stop=27.0, length=10), range(-1.0, stop=-27.0, length=10))
    test_range_identity(range(1f0, stop=27f0, length=10), range(-1f0, stop=-27f0, length=10))

    test_range_identity(range(1.0, stop=27.0, length=0), range(-1.0, stop=-27.0, length=0))
    test_range_identity(range(1f0, stop=27f0, length=0), range(-1f0, stop=-27f0, length=0))

    test_range_identity(range(1.0, stop=1.0, length=1), range(-1.0, stop=-1.0, length=1))
    test_range_identity(range(1f0, stop=1f0, length=1), range(-1f0, stop=-1f0, length=1))

    @test reverse(range(1.0, stop=27.0, length=1275)) == range(27.0, stop=1.0, length=1275)
    @test [reverse(range(1.0, stop=27.0, length=1275));] ==
        reverse([range(1.0, stop=27.0, length=1275);])
end

@testset "PR 12200 and related" begin
    for _r in (1:2:100, 1:100, 1f0:2f0:100f0, 1.0:2.0:100.0,
               range(1, stop=100, length=10), range(1f0, stop=100f0, length=10))
        float_r = float(_r)
        big_r = broadcast(big, _r)
        big_rdot = big.(_r)
        @test big_rdot == big_r
        @test typeof(big_r) == typeof(big_rdot)
        @test typeof(big_r).name === typeof(_r).name
        if eltype(_r) <: AbstractFloat
            @test isa(float_r, typeof(_r))
            @test eltype(big_r) === BigFloat
        else
            @test isa(float_r, AbstractRange)
            @test eltype(float_r) <: AbstractFloat
            @test eltype(big_r) === BigInt
        end
    end

    @test_throws DimensionMismatch range(1., stop=5., length=5) + range(1., stop=5., length=6)
    @test_throws DimensionMismatch range(1., stop=5., length=5) - range(1., stop=5., length=6)
    @test_throws DimensionMismatch range(1., stop=5., length=5) .* range(1., stop=5., length=6)
    @test_throws DimensionMismatch range(1., stop=5., length=5) ./ range(1., stop=5., length=6)

    @test_throws DimensionMismatch (1:5) + (1:6)
    @test_throws DimensionMismatch (1:5) - (1:6)
    @test_throws DimensionMismatch (1:5) .* (1:6)
    @test_throws DimensionMismatch (1:5) ./ (1:6)

    @test_throws DimensionMismatch (1.:5.) + (1.:6.)
    @test_throws DimensionMismatch (1.:5.) - (1.:6.)
    @test_throws DimensionMismatch (1.:5.) .* (1.:6.)
    @test_throws DimensionMismatch (1.:5.) ./ (1.:6.)

    function test_range_sum_diff(r1, r2, r_sum, r_diff)
        @test r1 + r2 == r_sum
        @test r2 + r1 == r_sum
        @test r1 - r2 == r_diff
        @test r2 - r1 == -r_diff

        @test Vector(r1) + Vector(r2) == Vector(r_sum)
        @test Vector(r2) + Vector(r1) == Vector(r_sum)
        @test Vector(r1) - Vector(r2) == Vector(r_diff)
        @test Vector(r2) - Vector(r1) == Vector(-r_diff)
    end

    test_range_sum_diff(1:5, 0:2:8, 1:3:13, 1:-1:-3)
    test_range_sum_diff(1.:5., 0.:2.:8., 1.:3.:13., 1.:-1.:-3.)
    test_range_sum_diff(range(1., stop=5., length=5), range(0., stop=-4., length=5),
                        range(1., stop=1., length=5), range(1., stop=9., length=5))

    test_range_sum_diff(1:5, 0.:2.:8., 1.:3.:13., 1.:-1.:-3.)
    test_range_sum_diff(1:5, range(0, stop=8, length=5),
                        range(1, stop=13, length=5), range(1, stop=-3, length=5))
    test_range_sum_diff(1.:5., range(0, stop=8, length=5),
                        range(1, stop=13, length=5), range(1, stop=-3, length=5))
end
# Issue #12388
let r = 0x02:0x05
    @test r[2:3] == 0x03:0x04
end

@testset "Issue #13738" begin
    for r in (big(1):big(2), UInt128(1):UInt128(2), 0x1:0x2)
        local r
        rr = r[r]
        @test typeof(rr) == typeof(r)
        @test r[r] == r
        # these calls to similar must not throw:
        @test size(similar(r, size(r))) == size(similar(r, length(r)))
    end
end

@testset "sign, conj, ~ (Issue #16067)" begin
    A = -1:1
    B = -1.0:1.0
    @test sign.(A) == [-1,0,1]
    @test sign.(B) == [-1,0,1]
    @test typeof(sign.(A)) === Vector{Int}
    @test typeof(sign.(B)) === Vector{Float64}

    @test conj(A) === A
    @test conj(B) === B

    @test .~A == [0,-1,-2]
    @test typeof(.~A) == Vector{Int}
end

@testset "conversion to Array" begin
    r = 1:3
    a = [1,2,3]
    @test convert(Array, r) == a
    @test convert(Array{Int}, r) == a
    @test convert(Array{Float64}, r) == a
    @test convert(Array{Int,1}, r) == a
    @test convert(Array{Float64,1}, r) == a
end

@testset "extrema" begin
    @test_throws ArgumentError minimum(1:2:-1)
    @test_throws ArgumentError argmin(Base.OneTo(-1))
    @test_throws ArgumentError maximum(Base.OneTo(-1))
    @test_throws ArgumentError argmax(1:-1)

    for (r, imin, imax) in [
            (Base.OneTo(5), 1, 5),
            (1:10, 1, 10),
            (10:-1:0, 11, 1),
            (range(10, stop=20, length=5), 1, 5),
            (range(10.3, step=-2, length=7), 7, 1),
           ]
        @test minimum(r) === minimum(r, init=typemax(eltype(r))) === r[imin]
        @test maximum(r) === maximum(r, init=typemin(eltype(r))) === r[imax]
        @test imin === argmin(r)
        @test imax === argmax(r)
        @test extrema(r) === (r[imin], r[imax])
    end

    r = 1f8-10:1f8
    rv = collect(r)
    @test argmin(r) == argmin(rv) == 1
    @test r[argmax(r)] == r[argmax(rv)] == 1f8
    @test argmax(r) == lastindex(r)
    @test argmax(rv) != lastindex(r)
end

@testset "OneTo" begin
    let r = Base.OneTo(-5)
        @test isempty(r)
        @test length(r) == checked_length(r) == 0
        @test size(r) == (0,)
        @test first(r) === 1
        @test last(r) === 0
    end
    let r = Base.OneTo(3)
        @test !isempty(r)
        @test length(r) == checked_length(r) == 3
        @test size(r) == (3,)
        @test step(r) == 1
        @test first(r) == 1
        @test first(r,2) === Base.OneTo(2)
        @test first(r,20) === r
        @test_throws ArgumentError first(r,-20)
        @test last(r) == 3
        @test minimum(r) == 1
        @test maximum(r) == 3
        @test argmin(r) == 1
        @test argmax(r) == 3
        @test r[2] == 2
        @test r[2:3] === 2:3
        @test_throws BoundsError r[4]
        @test_throws BoundsError r[0]
        @test broadcast(+, r, 1) === 2:4
        @test 2*r == 2:2:6
        @test r + r == 2:2:6
        k = 0
        for i in r
            @test i == (k += 1)
        end
        @test intersect(r, Base.OneTo(2)) == Base.OneTo(2)
        @test union(r, Base.OneTo(4)) == Base.OneTo(4)
        @test intersect(r, 0:5) == 1:3
        @test intersect(r, 2) === intersect(2, r) === 2:2
        @test findall(in(r), r) === findall(in(1:length(r)), r) ===
              findall(in(r), 1:length(r)) === 1:length(r)
        io = IOBuffer()
        show(io, r)
        str = String(take!(io))
        @test str == "Base.OneTo(3)"
    end
    let r = Base.OneTo(7)
        @test findall(in(2:(length(r) - 1)), r) === 2:(length(r) - 1)
        @test findall(in(r), 2:(length(r) - 1)) === 1:(length(r) - 2)
    end
    let r = Base.OneTo(Int8(4))
        @test first(r,4) === r
    end
    @test convert(Base.OneTo, 1:2) === Base.OneTo{Int}(2)
    @test_throws ArgumentError("first element must be 1, got 2") convert(Base.OneTo, 2:3)
    @test_throws ArgumentError("step must be 1, got 2") convert(Base.OneTo, 1:2:5)
    @test Base.OneTo(1:2) === Base.OneTo{Int}(2)
    @test Base.OneTo(1:1:2) === Base.OneTo{Int}(2)
    @test Base.OneTo{Int32}(1:2) === Base.OneTo{Int32}(2)
    @test Base.OneTo(Int32(1):Int32(2)) === Base.OneTo{Int32}(2)
    @test Base.OneTo{Int16}(3.0) === Base.OneTo{Int16}(3)
    @test_throws InexactError(:Int16, Int16, 3.2) Base.OneTo{Int16}(3.2)
end

@testset "range of other types" begin
    let r = range(0, stop=3//10, length=4)
        @test eltype(r) == Rational{Int}
        @test r[2] === 1//10
    end

    let a = 1.0,
        b = nextfloat(1.0),
        ba = BigFloat(a),
        bb = BigFloat(b),
        r = range(ba, stop=bb, length=3)
        @test eltype(r) == BigFloat
        @test r[1] == a && r[3] == b
        @test r[2] == (ba+bb)/2
    end

    let (a, b) = (rand(10), rand(10)),
        r = range(a, stop=b, length=5)
        @test r[1] == a && r[5] == b
        for i = 2:4
            x = ((5 - i) // 4) * a + ((i - 1) // 4) * b
            @test r[i] == x
        end
    end
end
@testset "issue #23178" begin
    r = range(Float16(0.1094), stop=Float16(0.9697), length=300)
    @test r[1] == Float16(0.1094)
    @test r[end] == Float16(0.9697)
end

# issue #20382
let r = @inferred((:)(big(1.0),big(2.0),big(5.0)))
    @test eltype(r) == BigFloat
end

@testset "issue #14420" begin
    for r in (range(0.10000000000000045, stop=1, length=50), 0.10000000000000045:(1-0.10000000000000045)/49:1)
        local r
        @test r[1] === 0.10000000000000045
        @test r[end] === 1.0
    end
end
@testset "issue #20381" begin
    r = range(-big(1.0), stop=big(1.0), length=4)
    @test isa(@inferred(r[2]), BigFloat)
    @test r[2] ≈ big(-1.0)/3
end

@testset "issue #20520" begin
    r = range(1.3173739f0, stop=1.3173739f0, length=3)
    @test length(r) == checked_length(r) == 3
    @test first(r) === 1.3173739f0
    @test last(r)  === 1.3173739f0
    @test r[2]     === 1.3173739f0
    r = range(1.0, stop=3+im, length=4)
    @test r[1] === 1.0+0.0im
    @test r[2] ≈ (5/3)+(1/3)im
    @test r[3] ≈ (7/3)+(2/3)im
    @test r[4] === 3.0+im
end

# ambiguity between (:) methods (#20988)
struct NotReal; val; end
Base.:+(x, y::NotReal) = x + y.val
Base.zero(y::NotReal) = zero(y.val)
Base.rem(x, y::NotReal) = rem(x, y.val)
Base.isless(x, y::NotReal) = isless(x, y.val)
@test (:)(1, NotReal(1), 5) isa StepRange{Int,NotReal}

isdefined(Main, :Furlongs) || @eval Main include("testhelpers/Furlongs.jl")
using .Main.Furlongs

@testset "dimensional correctness" begin
    @test_throws TypeError Furlong(2):Furlong(10)
    @test_throws TypeError range(Furlong(2), length=9)
    @test length(Vector(Furlong(2):Furlong(1):Furlong(10))) == 9
    @test length(range(Furlong(2), step=Furlong(1), length=9)) == checked_length(range(Furlong(2), step=Furlong(1), length=9)) == 9
    @test @inferred(length(StepRange(Furlong(2), Furlong(1), Furlong(1)))) == 0
    @test Vector(Furlong(2):Furlong(1):Furlong(10)) == Vector(range(Furlong(2), step=Furlong(1), length=9)) == Furlong.(2:10)
    @test Vector(Furlong(1.0):Furlong(0.5):Furlong(10.0)) ==
          Vector(Furlong(1):Furlong(0.5):Furlong(10)) == Furlong.(1:0.5:10)
end

@testset "sum arbitrary types" begin
    @test sum(Furlong(1):Furlong(0.5):Furlong(10)) == Furlong{1,Float64}(104.5)
    @test sum(StepRangeLen(Furlong(1), Furlong(0.5), 19)) == Furlong{1,Float64}(104.5)
    @test sum(0f0:0.001f0:1f0) == 500.5
    @test sum(0f0:0.000001f0:1f0) == 500000.5
    @test sum(0f0:0.1f0:10f0) == 505.
    @test sum(Float16(0):Float16(0.001):Float16(1)) ≈ 500.5
    @test sum(Float16(0):Float16(0.1):Float16(10)) == 505.
end

@testset "issue #22270" begin
    linsp = range(1.0, stop=2.0, length=10)
    @test typeof(linsp.ref) == Base.TwicePrecision{Float64}
    @test Float32(linsp.ref) === convert(Float32, linsp.ref)
    @test Float32(linsp.ref) ≈ linsp.ref.hi + linsp.ref.lo
end

@testset "issue #23300" begin
    x = -5:big(1.0):5
    @test map(Float64, x) === -5.0:1.0:5.0
    @test map(Float32, x) === -5.0f0:1.0f0:5.0f0
    @test map(Float16, x) === Float16(-5.0):Float16(1.0):Float16(5.0)
    @test map(BigFloat, x) === x
end

@testset "broadcasting returns ranges" begin
    x, r = 2, 1:5
    @test @inferred(x .+ r) === 3:7
    @test @inferred(r .+ x) === 3:7
    @test @inferred(r .- x) === -1:3
    @test @inferred(x .- r) === 1:-1:-3
    @test @inferred(x .* r) == 2:2:10
    @test @inferred(r .* x) == 2:2:10
    @test @inferred(r ./ x) === 0.5:0.5:2.5
    @test @inferred(x ./ r) == 2 ./ [r;] && isa(x ./ r, Vector{Float64})
    @test @inferred(r .\ x) == 2 ./ [r;] && isa(x ./ r, Vector{Float64})
    @test @inferred(x .\ r) === 0.5:0.5:2.5

    @test @inferred(2 .* (r .+ 1) .+ 2) == 6:2:14

    # issue #42291
    @test length((1:5) .- 1/7) == 5
    @test length((1:5) .+ -1/7) == 5
    @test length(-1/7 .+ (1:5)) == 5
end

@testset "Bad range calls" begin
    @test_throws ArgumentError range(1)
    @test_throws ArgumentError range(nothing)
    @test_throws ArgumentError range(1, step=4)
    @test_throws ArgumentError range(; step=1, length=6)
    @test_throws ArgumentError range(; step=2, stop=7.5)
    @test_throws ArgumentError range(1.0, step=0.25, stop=2.0, length=5)
    @test_throws ArgumentError range(; stop=nothing)
    @test_throws ArgumentError range(; length=nothing)
    @test_throws TypeError range(; length=5.5)
end

@testset "issue #23300#issuecomment-371575548" begin
    for (start, stop) in ((-5, 5), (-5.0, 5), (-5, 5.0), (-5.0, 5.0))
        @test @inferred(range(big(start), stop=big(stop), length=11)) isa LinRange{BigFloat}
        @test Float64.(@inferred(range(big(start), stop=big(stop), length=11))) == range(start, stop=stop, length=11)
        @test Float64.(@inferred(map(exp, range(big(start), stop=big(stop), length=11)))) == map(exp, range(start, stop=stop, length=11))
    end
end

@testset "Issue #26532" begin
    x = range(3, stop=3, length=5)
    @test step(x) == 0.0
    @test x isa StepRangeLen{Float64,Base.TwicePrecision{Float64},Base.TwicePrecision{Float64}}
end

@testset "Issue #44292" begin
    let x = @inferred range(0, step=0.2, length=5)
        @test x isa StepRangeLen{Float64,Base.TwicePrecision{Float64},Base.TwicePrecision{Float64}}
        @test x == [0.0, 0.2, 0.4, 0.6, 0.8]
    end

    let x = @inferred range(0.0, step=2, length=5)
        @test x isa StepRangeLen{Float64,Base.TwicePrecision{Float64},Base.TwicePrecision{Float64}}
        @test x == [0.0, 2.0, 4.0, 6.0, 8.0]
        @test x === range(0.0, step=2.0, length=5)
        @test x === range(0.0f0, step=2e0, length=5)
        @test x === range(0e0, step=2.0f0, length=5)
    end

    # start::IEEEFloat and step::Complex
    let x = @inferred range(2.0, step=1im, length=3)
        @test typeof(x) === StepRangeLen{ComplexF64, Float64, Complex{Int}, Int}
        @test x == range(2, step=1im, length=3)  # compare with integer range
        @test x == 2.0 .+ [0im, 1im, 2im]
    end

    # start::Complex and step::IEEEFloat
    let x = @inferred range(2im, step=1.0, length=3)
        @test typeof(x) === StepRangeLen{ComplexF64, Complex{Int}, Float64, Int}
        @test x == range(2im, step=1, length=3)  # compare with integer range
    end

    # stop::IEEEFloat and step::Complex
    let x = @inferred range(stop=2.0, step=1im, length=3)
        @test typeof(x) === StepRangeLen{ComplexF64, ComplexF64, Complex{Int}, Int}
        @test x == range(stop=2, step=1im, length=3)  # compare with integer range
        @test x == 2.0 .- [2im, 1im, 0im]
    end

    # stop::Complex and step::IEEEFloat
    let x = @inferred range(stop=2im, step=1.0, length=3)
        @test typeof(x) === StepRangeLen{ComplexF64, ComplexF64, Float64, Int}
        @test x == range(stop=2im, step=1, length=3)  # compare with integer range
    end

    let x = @inferred range(stop=10, step=2.0, length=5)
        @test x isa StepRangeLen{Float64,Base.TwicePrecision{Float64},Base.TwicePrecision{Float64}}
        @test x === @inferred range(stop=10.0, step=2.0, length=5)
        @test x === @inferred range(stop=10f0, step=2.0, length=5)
        @test x === @inferred range(stop=10e0, step=2.0f0, length=5)
        @test x == [2, 4, 6, 8, 10]
    end

    let x = @inferred range(stop=10.0, step=2, length=4)
        @test x isa StepRangeLen{Float64,Base.TwicePrecision{Float64},Base.TwicePrecision{Float64}}
        @test x == [4.0, 6.0, 8.0, 10.0]
    end
end

@testset "Views of ranges" begin
    @test view(Base.OneTo(10), Base.OneTo(5)) === Base.OneTo(5)
    @test view(1:10, 1:5) === 1:5
    @test view(1:10, 1:2:5) === 1:2:5
    @test view(1:2:9, 1:5) === 1:2:9
    @test view(1:10, :) === 1:10
    @test view(1:2:9, :) === 1:2:9

    # Ensure we don't hit a fallback `view` if there's a better `getindex` implementation
    vmt = collect(methods(view, Tuple{AbstractRange, AbstractRange}))
    for m in methods(getindex, Tuple{AbstractRange, AbstractRange})
        tt = Base.tuple_type_tail(m.sig)
        tt == Tuple{AbstractArray,Vararg{Any,N}} where N && continue
        vm = findfirst(sig->tt <: Base.tuple_type_tail(sig.sig), vmt)
        @test vmt[vm].sig != Tuple{typeof(view),AbstractArray,Vararg{Any,N}} where N
    end
end

@testset "Issue #26608" begin
    @test_throws BoundsError (Int8(-100):Int8(100))[400]
    @test_throws BoundsError (-100:100)[typemax(UInt)]
    @test_throws BoundsError (false:true)[3]
end

module NonStandardIntegerRangeTest

using Test

using Base.Checked: checked_length
import Base.Checked: checked_add, checked_sub

struct Position <: Integer
    val::Int
end
Position(x::Position) = x # to resolve ambiguity with boot.jl:770

struct Displacement <: Integer
    val::Int
end
Displacement(x::Displacement) = x # to resolve ambiguity with boot.jl:770

Base.:-(x::Displacement) = Displacement(-x.val)
Base.:-(x::Position, y::Position) = Displacement(x.val - y.val)
Base.:-(x::Position, y::Displacement) = Position(x.val - y.val)
Base.:-(x::Displacement, y::Displacement) = Displacement(x.val - y.val)
Base.:+(x::Position, y::Displacement) = Position(x.val + y.val)
Base.:+(x::Displacement, y::Displacement) = Displacement(x.val + y.val)
Base.:(<=)(x::Position, y::Position) = x.val <= y.val
Base.:(<)(x::Position, y::Position) = x.val < y.val
Base.:(<)(x::Displacement, y::Displacement) = x.val < y.val

# for StepRange computation:
Base.Unsigned(x::Displacement) = Unsigned(x.val)
Base.rem(x::Displacement, y::Displacement) = Displacement(rem(x.val, y.val))
Base.div(x::Displacement, y::Displacement) = Displacement(div(x.val, y.val))

# required for collect (summing lengths); alternatively, should length return Int by default?
Base.promote_rule(::Type{Displacement}, ::Type{Int}) = Int
Base.convert(::Type{Int}, x::Displacement) = x.val
Base.Int(x::Displacement) = x.val

# Unsigned complement, for testing checked_length
struct UPosition <: Unsigned
    val::UInt
end
UPosition(x::UPosition) = x # to resolve ambiguity with boot.jl:770

struct UDisplacement <: Unsigned
    val::UInt
end
UDisplacement(x::UDisplacement) = x # to resolve ambiguity with boot.jl:770

Base.show(io::IO, x::Union{Position, UPosition, Displacement, UDisplacement}) =
    # should use show if we were to do this properly (instead of just a test-helper)
    print(io, typeof(x).name.name, "(", x.val, ")")

Base.:-(x::UPosition, y::UPosition) = UDisplacement(x.val - y.val)
Base.:-(x::UPosition, y::UDisplacement) = UPosition(x.val - y.val)
Base.:+(x::UPosition, y::UDisplacement) = UPosition(x.val + y.val)
Base.:+(x::UDisplacement, y::Displacement) = UDisplacement(x.val + y.val)
Base.:+(x::UDisplacement, y::UDisplacement) = UDisplacement(x.val + y.val)
Base.:-(x::UPosition, y::Displacement) = UPosition(x.val - y.val)
checked_sub(x::UPosition, y::UPosition) = UDisplacement(checked_sub(x.val, y.val))
checked_sub(x::UPosition, y::UDisplacement) = UPosition(checked_sub(x.val, y.val))
checked_sub(x::UDisplacement, y::UDisplacement) = UDisplacement(checked_sub(x.val, y.val))
checked_add(x::UPosition, y::UDisplacement) = UPosition(checked_add(x.val, y.val))
checked_add(x::UDisplacement, y::UDisplacement) = UDisplacement(checked_add(x.val, y.val))
Base.:+(x::UPosition, y::Displacement) = UPosition(x.val + y.val)
Base.:(<=)(x::UPosition, y::UPosition) = x.val <= y.val
Base.:(<)(x::UPosition, y::UPosition) = x.val < y.val
Base.:(<)(x::UDisplacement, y::UDisplacement) = x.val < y.val

# for StepRange computation:
Base.rem(x::UDisplacement, y::Displacement) = UDisplacement(rem(x.val, y.val))
Base.div(x::UDisplacement, y::Displacement) = UDisplacement(div(x.val, y.val))
Base.rem(x::UDisplacement, y::UDisplacement) = UDisplacement(rem(x.val, y.val))
Base.div(x::UDisplacement, y::UDisplacement) = UDisplacement(div(x.val, y.val))

#Base.promote_rule(::Type{UDisplacement}, ::Type{Int}) = Int
#Base.convert(::Type{Int}, x::UDisplacement) = Int(x.val)

@testset "Ranges with nonstandard Integers" begin
    for (start, stop) in [(2, 4), (3, 3), (3, -2)]
        r = Position(start) : Position(stop)
        @test length(r) === Displacement(stop >= start ? stop - start + 1 : 0)
        start >= 0 && stop >= 0 && @test UDisplacement(length(r).val) ===
              checked_length(UPosition(start) : UPosition(stop)) ===
              checked_length(UPosition(start) : Displacement(1) : UPosition(stop)) ===
              checked_length(UPosition(start) : UDisplacement(1) : UPosition(stop))
        @test collect(r) == Position.(start : stop)
    end

    @test length(UPosition(3):Displacement(7):UPosition(100)) === checked_length(UPosition(3):Displacement(7):UPosition(100)) === UDisplacement(14)
    @test length(UPosition(100):Displacement(7):UPosition(3)) === checked_length(UPosition(100):Displacement(7):UPosition(3)) === UDisplacement(0)
    @test length(UPosition(100):Displacement(-7):UPosition(3)) === checked_length(UPosition(100):Displacement(-7):UPosition(3)) === UDisplacement(14)
    @test length(UPosition(3):Displacement(-7):UPosition(100)) === checked_length(UPosition(3):Displacement(-7):UPosition(100)) === UDisplacement(0)
    @test_throws OverflowError checked_length(zero(UPosition):UPosition(typemax(UInt)))
    @test_throws OverflowError checked_length(zero(UPosition):Displacement(1):UPosition(typemax(UInt)))
    @test_throws OverflowError checked_length(UPosition(typemax(UInt)):Displacement(-1):zero(UPosition))

    for start in [3, 0, -2]
        @test collect(Base.OneTo(Position(start))) == Position.(Base.OneTo(start))
    end

    for step in [-3, -2, -1, 1, 2, 3]
        for start in [-1, 0, 2]
            for stop in [start, start - 1, start + 2 * step, start + 2 * step + 1]
                r1 = StepRange(Position(start), Displacement(step), Position(stop))
                @test collect(r1) == Position.(start : step : stop)

                r2 = Position(start) : Displacement(step) : Position(stop)
                @test r1 === r2
            end
        end
    end
end

end # module NonStandardIntegerRangeTest

@testset "Issue #26619" begin
    @test length(UInt(100) : -1 : 1) == checked_length(UInt(100) : -1 : 1) === UInt(100)
    @test collect(UInt(5) : -1 : 3) == [UInt(5), UInt(4), UInt(3)]

    let r = UInt(5) : -2 : 2
        @test r.start === UInt(5)
        @test r.step === -2
        @test r.stop === UInt(3)
        @test collect(r) == [UInt(5), UInt(3)]
    end

    for step in [-3, -2, -1, 1, 2, 3]
        for start in [0, 15]
            for stop in [0, 15]
                @test collect(UInt(start) : step : UInt(stop)) == start : step : stop
            end
        end
    end
end

@testset "constant-valued ranges (issues #10391 and #29052)" begin
    @testset "with $(nameof(typeof(r))) of $(eltype(r))" for r in ((1:4), (1:1:4), StepRangeLen(1,1,4), (1.0:4.0))
        @test @inferred(0 * r) == [0.0, 0.0, 0.0, 0.0]
        @test @inferred(0 .* r) == [0.0, 0.0, 0.0, 0.0]
        @test @inferred(r .* 0) == [0.0, 0.0, 0.0, 0.0]
        @test @inferred(r + (4:-1:1)) == [5.0, 5.0, 5.0, 5.0]
        @test @inferred(r .+ (4:-1:1)) == [5.0, 5.0, 5.0, 5.0]
        @test @inferred(r - r) == [0.0, 0.0, 0.0, 0.0]
        @test @inferred(r .- r) == [0.0, 0.0, 0.0, 0.0]

        @test @inferred(r .+ (4.0:-1:1)) == [5.0, 5.0, 5.0, 5.0]
        @test @inferred(0.0 * r) == [0.0, 0.0, 0.0, 0.0]
        @test @inferred(0.0 .* r) == [0.0, 0.0, 0.0, 0.0]
        @test @inferred(r / Inf) == [0.0, 0.0, 0.0, 0.0]
        @test @inferred(r ./ Inf) == [0.0, 0.0, 0.0, 0.0]

        @test eval(Meta.parse(repr(0 * r))) == [0.0, 0.0, 0.0, 0.0]

        # Not constant-valued, but related methods:
        @test @inferred(-1 * r) == [-1,-2,-3,-4]
        @test @inferred(r * -1) == [-1,-2,-3,-4]
        @test @inferred(r / -1) == [-1,-2,-3,-4]

        @test @inferred(-1.0 .* r) == [-1,-2,-3,-4]
        @test @inferred(r .* -1.0) == [-1,-2,-3,-4]
        @test @inferred(r ./ -1.0) == [-1,-2,-3,-4]

        @test @inferred(-1 * reverse(r)) == [-4,-3,-2,-1]
        @test @inferred(-1.0 .* reverse(r)) == [-4,-3,-2,-1]
        @test @inferred(reverse(r) ./ -1.0) == [-4,-3,-2,-1]
    end

    @test_broken @inferred(range(0, step=0, length=4)) == [0, 0, 0, 0]
    @test @inferred(range(0, stop=0, length=4)) == [0, 0, 0, 0]
    @test @inferred(range(0.0, step=0.0, length=4)) == [0.0, 0.0, 0.0, 0.0]
    @test @inferred(range(0.0, stop=0.0, length=4)) == [0.0, 0.0, 0.0, 0.0]
    @test @inferred(range(0, step=0.0, length=4)) == [0.0, 0.0, 0.0, 0.0]
    @test @inferred(range(0.0, step=0, length=4)) == [0.0, 0.0, 0.0, 0.0]
    @test @inferred(range(0, stop=0.0, length=4)) == [0.0, 0.0, 0.0, 0.0]
    @test @inferred(range(0.0, stop=0, length=4)) == [0.0, 0.0, 0.0, 0.0]

    z4 = 0.0 * (1:4)
    @test @inferred(z4 .+ (1:4)) == 1.0:1.0:4.0
    @test @inferred(z4 .+ z4) === z4
end

@testset "getindex" begin
    @test getindex((typemax(UInt64)//one(UInt64):typemax(UInt64)//one(UInt64)), 1) == typemax(UInt64)//one(UInt64)
end

@testset "Issue #30006" begin
    @test Base.Slice(Base.OneTo(5))[Int32(1)] == Int32(1)
    @test Base.Slice(Base.OneTo(3))[Int8(2)] == Int8(2)
    @test Base.Slice(1:10)[Int32(2)] == Int32(2)
    @test Base.Slice(1:10)[Int8(2)] == Int8(2)
end

@testset "allocation of TwicePrecision call" begin
    let
        @test @allocated(0:286.493442:360) == 0
        @test @allocated(0:286:360) == 0
    end
end

@testset "range with start and stop" begin
    for starts in [-1, 0, 1, 10]
        for stops in [-2, 0, 2, 100]
            for lengths in [2, 10, 100]
                if stops >= starts
                    @test range(starts, stops, length=lengths) === range(starts, stop=stops, length=lengths)
                end
            end
            for steps in [0.01, 1, 2]
                @test range(starts, stops, step=steps) === range(starts, stop=stops, step=steps)
            end
        end
    end
end

@testset "Reverse empty ranges" begin
    @test reverse(1:0) === 0:-1:1
    @test reverse(Base.OneTo(0)) === 0:-1:1
    # Almost `1.0:-1.0:2.0`, only different is the step which is
    # `Base.TwicePrecision(-1.0, 0.0)`
    @test reverse(1.0:0.0) === StepRangeLen(Base.TwicePrecision(1.0, 0.0),
                                            Base.TwicePrecision(-1.0, -0.0), 0)
    @test reverse(reverse(1.0:0.0)) === 1.0:0.0
end

@testset "Issue #30944 ranges with non-IEEEFloat types" begin
    # We want to test the creation of a range with BigFloat start or step
    @test range(big(1.0), length=10) == big(1.0):1:10
    @test range(1, step = big(1.0), length=10) == big(1.0):1:10
    @test range(1.0, step = big(1.0), length=10) == big(1.0):1:10
end

@testset "mod with ranges" begin
    for n in -10:10
        @test mod(n, 0:4) == mod(n, 5)
        @test mod(n, 1:5) == mod1(n, 5)
        @test mod(n, 2:6) == 2 + mod(n-2, 5)
        @test mod(n, Base.OneTo(5)) == mod1(n, 5)
    end
    @test mod(Int32(3), 1:5) == 3
    @test mod(big(typemax(Int))+99, 0:4) == mod(big(typemax(Int))+99, 5)
    @test_throws MethodError mod(3.141, 1:5)
    @test_throws MethodError mod(3, UnitRange(1.0,5.0))
    @test_throws MethodError mod(3, 1:2:7)
    @test_throws DivideError mod(3, 1:0)
end

@testset "clamp with unitrange" begin
    for n in -10:10
        @test clamp(n, 0:4) == clamp(n, 0, 4)
        @test clamp(n, Base.OneTo(5)) == clamp(n, 1, 5)
    end
    @test clamp(Int32(3), 1:5) === Int(3)
    @test clamp(big(typemax(Int))+99, 0:4) == 4
    @test_throws MethodError clamp(3.141, 1:5)
    @test_throws MethodError clamp(3, UnitRange(1.0,5.0))
    @test_throws MethodError clamp(3, 1:2:7)
    @test clamp(3, 1:0) == clamp(3, 1, 0) == 0
    @test clamp(-3, 1:0) == clamp(-3, 1, 0) == 1
end

@testset "issue #33882" begin
    r = StepRangeLen('a',2,4)
    @test step(r) === 2
    @test collect(r) == ['a','c','e','g']
end

@testset "diff of ranges, #36116" begin
    for r in (0:2, 0:1:2, 0.0:1.0:2.0, LinRange(0,2,3))
        @test diff(r) == diff(collect(r)) == fill(1, 2)
        @test_throws ArgumentError diff(r, dims=2)
    end
    for r in (0:2:5, 0.1:0.1:2.0, LinRange(0,2,33))
        @test diff(r) == diff(collect(r)) == [r[i+1] - r[i] for i in 1:length(r)-1]
    end
end

@testset "Return type of indexing with ranges" begin
    for T = (Base.OneTo{Int}, UnitRange{Int}, StepRange{Int,Int}, StepRangeLen{Int}, LinRange{Int})
        @test eltype(T(1:5)) === eltype(T(1:5)[1:2])
    end
end

@testset "Type-stable intersect (#32410)" begin
    for T = (StepRange{Int,Int}, StepRange{BigInt,Int}, StepRange{BigInt,BigInt})
        @test @inferred(intersect(T(1:2:5), 1:5)) == 1:2:5
        @test @inferred(intersect(1:5, T(1:2:5))) == 1:2:5
        @test @inferred(intersect(T(5:-2:1), 1:5)) == 5:-2:1
        @test @inferred(intersect(1:5, T(5:-2:1))) == 1:2:5
        @test isempty(@inferred(intersect(T(5:2:3), 1:5)))
        @test isempty(@inferred(intersect(1:5, T(5:2:3))))
    end
    @test @inferred(intersect(1:2:5, 1//1:1:5//1)) == 1:2:5
    @test @inferred(intersect(1//1:1:5//1, 1:2:5)) == 1:2:5
    @test @inferred(intersect(big(1):big(5), 3)) == 3:3
    @test @inferred(intersect(3, big(1):big(5))) == 3:3
end

@testset "eltype of range(::Integer; step::Rational, length) (#37295)" begin
    r = range(1, step=1//2, length=3)
    @test r == [1//1, 3//2, 2//1]
    @test eltype(r) === Rational{Int}
    @test typeof(step(r)) === Rational{Int}

    r = range(1//1, step=2, length=3)
    @test r == [1, 3, 5]
    @test eltype(r) === Rational{Int}
    @test typeof(step(r)) === Int

    r = range(Int16(1), step=Rational{Int8}(1,2), length=Int16(3))
    @test r == [1//1, 3//2, 2//1]
    @test eltype(r) === Rational{Int16}
    @test typeof(step(r)) === Rational{Int8}

    r = range(Rational{Int8}(1), step=Int16(2), length=Int8(3))
    @test r == [1, 3, 5]
    @test eltype(r) === Rational{Int16}
    @test typeof(step(r)) === Int16

    r = range('a', step=2, length=3)
    @test r == ['a', 'c', 'e']
    @test eltype(r) === Char
    @test typeof(step(r)) === Int

    r = range(typemax(Int)//1, step=1, length=0)
    @test isempty(r)
    @test eltype(r) === Rational{Int}
    @test typeof(step(r)) === Int

    r = range(typemin(Int), step=-1//1, length=0)
    @test isempty(r)
    @test eltype(r) === Rational{Int}
    @test typeof(step(r)) === Rational{Int}

    r = StepRangeLen(Int8(1), Int8(2), 3)
    @test r == Int8[1, 3, 5]
    @test eltype(r) === Int8
    @test typeof(step(r)) === Int8

    r = StepRangeLen(Int8(1), Int8(2), 3, 2)
    @test r == Int8[-1, 1, 3]
    @test eltype(r) === Int8
    @test typeof(step(r)) === Int8
end

@testset "length(StepRange()) type stability" begin
    for SR in (StepRange{Int,Int128}, StepRange{Int8,Int128})
        r1, r2 = SR(1, 1, 1), SR(1, 1, 0)
        @test typeof(length(r1)) == typeof(checked_length(r1)) ==
              typeof(length(r2)) == typeof(checked_length(r2))
    end
    SR = StepRange{Union{Int64,Int128},Int}
    test_length(r, l) = length(r) === checked_length(r) === l
    @test test_length(SR(Int64(1), 1, Int128(1)), Int128(1))
    @test test_length(SR(Int64(1), 1, Int128(0)), Int128(0))
    @test test_length(SR(Int64(1), 1, Int64(1)), Int64(1))
    @test test_length(SR(Int64(1), 1, Int64(0)), Int64(0))
end

@testset "LinRange eltype for element types that wrap integers" begin
    struct RealWrapper{T <: Real} <: Real
        x :: T
    end
    Base.promote_rule(::Type{S}, ::Type{RealWrapper{T}}) where {T,S<:Real} = RealWrapper{promote_type(S, T)}
    Base.:(-)(w::RealWrapper) = RealWrapper(-w.x)
    for f in [:(+), :(-), :(*), :(/)]
        @eval Base.$f(w::RealWrapper, y::RealWrapper) = RealWrapper($f(w.x, y.x))
    end
    for f in [:(<), :(==), :(<=)]
        @eval Base.$f(w::RealWrapper, y::RealWrapper) = $f(w.x, y.x)
    end
    for T in [:Float32, :Float64]
        @eval Base.$T(w::RealWrapper) = $T(w.x)
    end
    (::Type{RealWrapper{T}})(w::RealWrapper) where {T<:Real} = RealWrapper{T}(T(w.x))
    (::Type{T})(w::RealWrapper{T}) where {T<:Real} = T(w.x)
    Base.:(==)(w::RealWrapper, y::RealWrapper) = w.x == y.x
    Base.isfinite(w::RealWrapper) = isfinite(w.x)
    Base.signbit(w::RealWrapper) = signbit(w.x)

    x = RealWrapper(2)
    r1 = range(x, stop = 2x, length = 10)
    r2 = range(Int(x), stop = Int(2x), length = 10)
    for i in eachindex(r1, r2)
        @test r1[i] ≈ r2[i]
    end
    r3 = LinRange(x, 2x, 10)
    r4 = LinRange(x, 2x, 10)
    for i in eachindex(r3, r4)
        @test r3[i] ≈ r4[i]
    end
end

@testset "Bool indexing of ranges" begin
    @test_throws ArgumentError Base.OneTo(true)
    @test_throws ArgumentError Base.OneTo(true:true:true)

    @test_throws ArgumentError (1:2)[true]
    @test_throws ArgumentError (big(1):big(2))[true]
    @test_throws ArgumentError Base.OneTo(10)[true]
    @test_throws ArgumentError (1:2:5)[true]
    @test_throws ArgumentError LinRange(1,2,2)[true]
    @test_throws ArgumentError (1.0:2.0:5.0)[true]
    r = 3:2
    r2 = r[true:false]
    @test r2 == collect(r)[true:false]
    @test r.start == r2.start && r.stop == r2.stop
    @test_throws BoundsError r[true:true]
    @test_throws BoundsError r[false:true]
    r = 3:3
    r2 = r[true:true]
    @test r2 == collect(r)[true:true]
    @test r.start == r2.start && r.stop == r2.stop
    r2 = r[false:false]
    @test r2.start == 3 && r2.stop == 2
    @test_throws BoundsError r[true:false]
    @test_throws BoundsError r[false:true]
    r = 2:3
    r2 = r[false:true]
    @test r2 == collect(r)[false:true]
    @test r2.start == r2.stop == 3
    @test_throws BoundsError r[true:false]
    @test_throws BoundsError r[true:true]

    r = 2:1
    r2 = r[true:true:false]
    @test r2 == collect(r)[true:true:false]
    @test r2 isa StepRange && r2.start == 2 && r2.step == 1 && r2.stop == 1
    @test_throws BoundsError r[false:true:false]

    r = 2:2
    r2 = r[false:true:false]
    @test r2 == collect(r)[false:true:false]
    @test r2 isa StepRange && r2.start == 2 && r2.step == 1 && r2.stop == 1
    r2 = r[true:true:true]
    @test r2 == collect(r)[true:true:true]
    @test r2 isa StepRange && r2.start == 2 && r2.step == 1 && r2.stop == 2
    @test_throws BoundsError r[true:true:false]
    @test_throws BoundsError r[false:true:true]

    r = 1:2
    r2 = r[false:true:true]
    @test r2 == collect(r)[false:true:true]
    @test r2 isa StepRange && r2.start == 2 && r2.step == 1 && r2.stop == 2
    @test_throws BoundsError r[true:true:false]
    @test_throws BoundsError r[true:true:true]

    r = 2:1:1
    r2 = r[true:true:false]
    @test r2 == collect(r)[true:true:false]
    @test r2 isa StepRange && r2.start == 2 && r2.step == 1 && r2.stop == 1
    @test_throws BoundsError r[false:true:false]

    r = 2:1:2
    r2 = r[false:true:false]
    @test r2 == collect(r)[false:true:false]
    @test r2 isa StepRange && r2.start == 2 && r2.step == 1 && r2.stop == 1
    r2 = r[true:true:true]
    @test r2 == collect(r)[true:true:true]
    @test r2 isa StepRange && r2.start == 2 && r2.step == 1 && r2.stop == 2
    @test_throws BoundsError r[true:true:false]
    @test_throws BoundsError r[false:true:true]

    r = 1:1:2
    r2 = r[false:true:true]
    @test r2 == collect(r)[false:true:true]
    @test r2 isa StepRange && r2.start == 2 && r2.step == 1 && r2.stop == 2
    @test_throws BoundsError r[true:true:false]
    @test_throws BoundsError r[true:true:true]

    r = 2.0:1.0:1.0
    r2 = r[true:true:false]
    @test r2 == collect(r)[true:true:false]
    @test r2 isa StepRangeLen && r2 == 2:1
    @test_throws BoundsError r[false:true:false]

    r = 2.0:1.0:2.0
    r2 = r[false:true:false]
    @test r2 == collect(r)[false:true:false]
    @test r2 isa StepRangeLen && r2 == 2:1
    r2 = r[true:true:true]
    @test r2 == collect(r)[true:true:true]
    @test r2 isa StepRangeLen && r2 == 2:2
    @test_throws BoundsError r[true:true:false]
    @test_throws BoundsError r[false:true:true]

    r = 1.0:1.0:2.0
    r2 = r[false:true:true]
    @test r2 == collect(r)[false:true:true]
    @test r2 isa StepRangeLen && r2 == 2:2
    @test_throws BoundsError r[true:true:false]
    @test_throws BoundsError r[true:true:true]

    r = StepRangeLen(2, 1, 0)
    r2 = r[true:true:false]
    @test r2 == collect(r)[true:true:false]
    @test r2 isa StepRangeLen && r2 == 2:1
    @test_throws BoundsError r[false:true:false]

    r = StepRangeLen(2, 1, 1)
    r2 = r[false:true:false]
    @test r2 == collect(r)[false:true:false]
    @test r2 isa StepRangeLen && r2 == 2:1
    r2 = r[true:true:true]
    @test r2 == collect(r)[true:true:true]
    @test r2 isa StepRangeLen && r2 == 2:2
    @test_throws BoundsError r[true:true:false]
    @test_throws BoundsError r[false:true:true]

    r = StepRangeLen(1, 1, 2)
    r2 = r[false:true:true]
    @test r2 == collect(r)[false:true:true]
    @test r2 isa StepRangeLen && r2 == 2:2
    @test_throws BoundsError r[true:true:false]
    @test_throws BoundsError r[true:true:true]

    r = LinRange(2, 1, 0)
    r2 = r[true:true:false]
    @test r2 == collect(r)[true:true:false]
    @test r2 isa LinRange && r2 == 2:1
    @test_throws BoundsError r[false:true:false]

    r = LinRange(2, 2, 1)
    r2 = r[false:true:false]
    @test r2 == collect(r)[false:true:false]
    @test r2 isa LinRange && r2 == 2:1
    r2 = r[true:true:true]
    @test r2 == collect(r)[true:true:true]
    @test r2 isa LinRange && r2 == 2:2
    @test_throws BoundsError r[true:true:false]
    @test_throws BoundsError r[false:true:true]

    r = LinRange(1, 2, 2)
    r2 = r[false:true:true]
    @test r2 == collect(r)[false:true:true]
    @test r2 isa LinRange && r2 == 2:2
    @test_throws BoundsError r[true:true:false]
    @test_throws BoundsError r[true:true:true]
end

@testset "Non-Int64 endpoints that are identical (#39798)" begin
    for T in DataType[Float16,Float32,Float64,Bool,Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128],
        r in [ LinRange(1, 1, 10), StepRangeLen(7, 0, 5) ]
        if first(r) > typemax(T)
            continue
        end
        let start=T(first(r)), stop=T(last(r)), step=T(step(r)), length=length(r)
            @test range(  start, stop,       length) == r
            @test range(  start, stop;       length) == r
            @test range(  start; stop,       length) == r
            @test range(; start, stop,       length) == r
        end
    end
end
@testset "PR 40320 fixes" begin
    # found by nanosoldier
    @test 0.2 * (-2:2) == -0.4:0.2:0.4  # from tests of AbstractFFTs, needs Base.TwicePrecision
    @test 0.2f0 * (-2:2) == Float32.(-0.4:0.2:0.4)  # likewise needs Float64
    @test 0.2 * (-2:1:2) == -0.4:0.2:0.4

    # https://github.com/JuliaLang/julia/issues/40846
    @test 0.1 .* (3:-1:1) ≈ [0.3, 0.2, 0.1]
    @test (10:-1:1) * 0.1 == 1:-0.1:0.1
    @test 0.2 * (-2:2:2) == [-0.4, 0, 0.4]
end

@testset "IdentityUnitRange indexing" begin
    @testset "Indexing into an IdentityUnitRange" begin
        @testset for r in Any[-1:20, Base.OneTo(20)]
            ri = Base.IdentityUnitRange(r)
            @test_throws "invalid index" ri[true]
            @testset for s in Any[Base.OneTo(6), Base.OneTo{BigInt}(6), 3:6, big(3):big(6), 3:2:7]
                @test mapreduce(==, &, ri[s], ri[s[begin]]:step(s):ri[s[end]])
                @test axes(ri[s]) == axes(s)
                @test eltype(ri[s]) == eltype(ri)
            end
        end
        @testset "Bool indices" begin
            r = 1:1
            @test Base.IdentityUnitRange(r)[true:true] == r[true:true]
            @test Base.IdentityUnitRange(r)[true:true:true] == r[true:true:true]
            @test_throws BoundsError Base.IdentityUnitRange(1:2)[true:true]
            @test_throws BoundsError Base.IdentityUnitRange(1:2)[true:true:true]
        end
    end
    @testset "Indexing with IdentityUnitRange" begin
        @testset "OneTo" begin
            @testset for endpt in Any[10, big(12), UInt(11)]
                r = Base.OneTo(endpt)
                inds = Base.IdentityUnitRange(3:5)
                rs = r[inds]
                @test rs == inds
                @test axes(rs) == axes(inds)
                @test_throws BoundsError r[Base.IdentityUnitRange(-1:100)]
            end
        end
        @testset "IdentityUnitRange" begin
            @testset for r in Any[Base.IdentityUnitRange(1:4), Base.IdentityUnitRange(Base.OneTo(4)), Base.Slice(1:4), Base.Slice(Base.OneTo(4))]
                @testset for s in Any[Base.IdentityUnitRange(3:3), Base.IdentityUnitRange(Base.OneTo(2)), Base.Slice(3:3), Base.Slice(Base.OneTo(2))]
                    rs = r[s]
                    @test rs == s
                    @test axes(rs) == axes(s)
                end
                @test_throws BoundsError r[Base.IdentityUnitRange(first(r):last(r) + 1)]
            end
        end
    end
end

@testset "non 1-based ranges indexing" begin
    struct ZeroBasedUnitRange{T,A<:AbstractUnitRange{T}} <: AbstractUnitRange{T}
        a :: A
        function ZeroBasedUnitRange(a::AbstractUnitRange{T}) where {T}
            @assert !Base.has_offset_axes(a)
            new{T, typeof(a)}(a)
        end
    end

    Base.parent(A::ZeroBasedUnitRange) = A.a
    Base.first(A::ZeroBasedUnitRange) = first(parent(A))
    Base.length(A::ZeroBasedUnitRange) = length(parent(A))
    Base.last(A::ZeroBasedUnitRange) = last(parent(A))
    Base.size(A::ZeroBasedUnitRange) = size(parent(A))
    Base.axes(A::ZeroBasedUnitRange) = map(x -> Base.IdentityUnitRange(0:x-1), size(parent(A)))
    Base.getindex(A::ZeroBasedUnitRange, i::Int) = parent(A)[i + 1]
    Base.getindex(A::ZeroBasedUnitRange, i::Integer) = parent(A)[i + 1]
    Base.firstindex(A::ZeroBasedUnitRange) = 0
    function Base.show(io::IO, A::ZeroBasedUnitRange)
        show(io, parent(A))
        print(io, " with indices $(axes(A,1))")
    end

    r = ZeroBasedUnitRange(5:8)
    @test r[0:2] == r[0]:r[2]
    @test r[0:1:2] == r[0]:1:r[2]
end

@test length(range(1, 100, length=big(100)^100)) == big(100)^100
@test length(range(big(1), big(100)^100, length=big(100)^100)) == big(100)^100
@test length(0 * (1:big(100)^100)) == big(100)^100

@testset "issue #41784" begin
    # tests `in` when step equals 0
    # test for Int
    x = 41784
    @test (x in StepRangeLen(x, 0, 0)) == false
    @test (x in StepRangeLen(x, 0, rand(1:100))) == true
    @test ((x - 1) in StepRangeLen(x, 0, rand(1:100))) == false
    @test ((x + 1) in StepRangeLen(x, 0, rand(1:100))) == false

    # test for Char
    x = 'z'
    @test (x in StepRangeLen(x, 0, 0)) == false
    @test (x in StepRangeLen(x, 0, rand(1:100))) == true
    @test ((x - 1) in StepRangeLen(x, 0, rand(1:100))) == false
    @test ((x + 1) in StepRangeLen(x, 0, rand(1:100))) == false
end

@testset "issue #42528" begin
    struct Fix42528 <: Unsigned
        val::UInt
    end
    Fix42528(a::Fix42528) = a
    Base.:(<)(a::Fix42528, b::Fix42528) = a.val < b.val
    Base.:(>=)(a::Fix42528, b::Fix42528) = a.val >= b.val
    Base.:(+)(a::Fix42528, b::Fix42528) = a.val+b.val
    Base.promote_rule(::Type{Fix42528}, ::Type{<:Unsigned}) = Fix42528
    Base.show(io::IO, ::MIME"text/plain", a::Fix42528) = print(io, "Fix42528(", a.val, ')')
    Base.show(io::IO, a::Fix42528) = print(io, "Fix42528(", a.val, ')')
    function Base.:(-)(a::Fix42528, b::Fix42528)
        a.val < b.val && throw(DomainError("Can't subtract, result outside of domain"))
        return a.val - b.val
    end
    Base.one(::Type{Fix42528}) = Fix42528(0x1)
    @test Fix42528(0x0):Fix42528(0x1) == [Fix42528(0x0), Fix42528(0x01)]
    @test iszero(length(Fix42528(0x1):Fix42528(0x0)))
    @test_throws DomainError Fix42528(0x0) - Fix42528(0x1)
end

let r = Ptr{Cvoid}(20):-UInt(2):Ptr{Cvoid}(10)
    @test isempty(r)
    @test length(r) == 0
    @test count(i -> true, r) == 0
    @test isempty(collect(r))
    @test first(r) === Ptr{Cvoid}(20)
    @test step(r) === -UInt(2)
    @test last(r) === Ptr{Cvoid}(10)
end

# test behavior of wrap-around and promotion of empty ranges (#35711)
@test length(range(0, length=UInt(0))) === UInt(0)
@test isempty(range(0, length=UInt(0)))
@test length(range(typemax(Int), length=UInt(0))) === UInt(0)
@test isempty(range(typemax(Int), length=UInt(0)))
@test length(range(0, length=UInt(0), step=UInt(2))) == UInt(0)
@test isempty(range(0, length=UInt(0), step=UInt(2)))
@test length(range(typemax(Int), length=UInt(0), step=UInt(2))) === UInt(0)
@test isempty(range(typemax(Int), length=UInt(0), step=UInt(2)))
@test length(range(typemax(Int), length=UInt(0), step=2)) === UInt(0)
@test isempty(range(typemax(Int), length=UInt(0), step=2))
@test length(range(typemax(Int), length=0, step=UInt(2))) === 0
@test isempty(range(typemax(Int), length=0, step=UInt(2)))

@test length(range(1, length=typemax(Int128))) === typemax(Int128)

@testset "firstindex(::StepRange{<:Base.BitInteger})" begin
    test_firstindex(x) = firstindex(x) === first(Base.axes1(x))
    for T in Base.BitInteger_types, S in Base.BitInteger_types
        @test test_firstindex(StepRange{T,S}(1, 1, 1))
        @test test_firstindex(StepRange{T,S}(1, 1, 0))
    end
    @test test_firstindex(StepRange{Union{Int64,Int128},Int}(Int64(1), 1, Int128(1)))
    @test test_firstindex(StepRange{Union{Int64,Int128},Int}(Int64(1), 1, Int128(0)))
end

@testset "PR 49516" begin
    struct PR49516 <: Signed
        n::Int
    end
    PR49516(f::PR49516) = f
    Base.:*(x::Integer, f::PR49516) = PR49516(*(x, f.n))
    Base.:+(f1::PR49516, f2::PR49516) = PR49516(+(f1.n, f2.n))
    Base.show(io::IO, f::PR49516) = print(io, "PR49516(", f.n, ")")

    srl = StepRangeLen(PR49516(1), PR49516(2), 10)
    @test sprint(show, srl) == "PR49516(1):PR49516(2):PR49516(19)"
end

@testset "Inline StepRange Construction #49270" begin
    x = rand(Float32, 80)
    a = rand(round(Int, length(x) / 2):length(x), 10^6)

    function test(x, a)
        c = zero(Float32)

        @inbounds for j in a
            for i in 1:8:j
                c += x[i]
            end
        end

        return c
    end

    llvm_ir(f, args) = sprint((io, args...) -> code_llvm(io, args...; debuginfo=:none), f, Base.typesof(args...))

    ir = llvm_ir(test, (x, a))
    @test !occursin("steprange_last", ir)
    @test !occursin("_colon", ir)
    @test !occursin("StepRange", ir)
end

# DimensionMismatch and LazyString
function check_ranges(rx, ry)
    if length(rx) != length(ry)
        throw(DimensionMismatch(lazy"length of rx, $(length(rx)), does not equal length of ry, $(length(ry))"))
    end
    rx, ry
end
@test Core.Compiler.is_foldable(Base.infer_effects(check_ranges, (UnitRange{Int},UnitRange{Int})))
# TODO JET.@test_opt check_ranges(1:2, 3:4)

@testset "checkbounds overflow (#26623)" begin
    # the reported issue:
    @test_throws BoundsError (1:3:4)[typemax(Int)÷3*2+3]

    # a case that using mul_with_overflow & add_with_overflow might get wrong:
    @test (-10:2:typemax(Int))[typemax(Int)÷2+2] == typemax(Int)-9
end

@testset "collect with specialized vcat" begin
    struct OneToThree <: AbstractUnitRange{Int} end
    Base.size(r::OneToThree) = (3,)
    Base.first(r::OneToThree) = 1
    Base.length(r::OneToThree) = 3
    Base.last(r::OneToThree) = 3
    function Base.getindex(r::OneToThree, i::Int)
        checkbounds(r, i)
        i
    end
    Base.vcat(r::OneToThree) = r
    r = OneToThree()
    a = Array(r)
    @test a isa Vector{Int}
    @test a == r
    @test collect(r) isa Vector{Int}
    @test collect(r) == r
end

@testset "isassigned" begin
    for (r, val) in ((1:3, 3), (1:big(2)^65, big(2)^65))
        @test isassigned(r, lastindex(r))
        # test that the indexing actually succeeds
        @test r[end] == val
        @test_throws ArgumentError isassigned(r, true)
    end

end

@testset "unsigned index #44895" begin
    x = range(-1,1,length=11)
    @test x[UInt(1)] == -1.0
    a = StepRangeLen(1,2,3,2)
    @test a[UInt(1)] == -1
end

@testset "StepRangeLen of CartesianIndex-es" begin
    CIstart = CartesianIndex(2,3)
    CIstep = CartesianIndex(1,1)
    r = StepRangeLen(CIstart, CIstep, 4)
    @test length(r) == 4
    @test first(r) == CIstart
    @test step(r) == CIstep
    @test last(r) == CartesianIndex(5,6)
    @test r[2] == CartesianIndex(3,4)

    @test repr(r) == "StepRangeLen($CIstart, $CIstep, 4)"

    r = StepRangeLen(CartesianIndex(), CartesianIndex(), 3)
    @test all(==(CartesianIndex()), r)
    @test length(r) == 3
    @test repr(r) == "StepRangeLen(CartesianIndex(), CartesianIndex(), 3)"

    errmsg = ("deliberately unsupported for CartesianIndex", "StepRangeLen")
    @test_throws errmsg range(CartesianIndex(1), step=CartesianIndex(1), length=3)
end

@testset "logrange" begin
    # basic idea
    @test logrange(2, 16, 4) ≈ [2, 4, 8, 16]
    @test logrange(1/8, 8.0, 7) ≈ [0.125, 0.25, 0.5, 1.0, 2.0, 4.0, 8.0]
    @test logrange(1000, 1, 4) ≈ [1000, 100, 10, 1]
    @test logrange(1, 10^9, 19)[1:2:end] ≈ 10 .^ (0:9)

    # endpoints
    @test logrange(0.1f0, 100, 33)[1] === 0.1f0
    @test logrange(0.789, 123_456, 135_790)[[begin, end]] == [0.789, 123_456]
    @test logrange(nextfloat(0f0), floatmax(Float32), typemax(Int))[end] === floatmax(Float32)
    @test logrange(nextfloat(Float16(0)), floatmax(Float16), 66_000)[end] === floatmax(Float16)
    @test first(logrange(pi, 2pi, 3000)) === logrange(pi, 2pi, 3000)[1] === Float64(pi)
    if Int == Int64
        @test logrange(0.1, 1000, 2^54)[end] === 1000.0
    end

    # empty, only, constant
    @test first(logrange(1, 2, 0)) === 1.0
    @test last(logrange(1, 2, 0)) === 2.0
    @test collect(logrange(1, 2, 0)) == Float64[]
    @test only(logrange(2pi, 2pi, 1)) === logrange(2pi, 2pi, 1)[1] === 2pi
    @test logrange(1, 1, 3) == fill(1.0, 3)

    # subnormal Float64
    x = logrange(1e-320, 1e-300, 21) .* 1e300
    @test x ≈ logrange(1e-20, 1, 21) rtol=1e-6

    # types
    @test eltype(logrange(1, 10, 3)) == Float64
    @test eltype(logrange(1, 10, Int32(3))) == Float64
    @test eltype(logrange(1, 10f0, 3)) == Float32
    @test eltype(logrange(1f0, 10, 3)) == Float32
    @test eltype(logrange(1, big(10), 3)) == BigFloat
    @test logrange(big"0.3", big(pi), 50)[1] == big"0.3"
    @test logrange(big"0.3", big(pi), 50)[end] == big(pi)

    # more constructors
    @test logrange(1,2,length=3) === Base.LogRange(1,2,3) == Base.LogRange{Float64}(1,2,3)
    @test logrange(1f0, 2f0, length=3) == Base.LogRange{Float32}(1,2,3)

    # errors
    @test_throws UndefKeywordError logrange(1, 10)  # no default length
    @test_throws ArgumentError logrange(1, 10, -1)  # negative length
    @test_throws ArgumentError logrange(1, 10, 1) # endpoints must not differ
    @test_throws DomainError logrange(1, -1, 3)   # needs complex numbers
    @test_throws DomainError logrange(-1, -2, 3)  # not supported, for now
    @test_throws MethodError logrange(1, 2+3im, length=4)  # not supported, for now
    @test_throws ArgumentError logrange(1, 10, 2)[true]  # bad index
    @test_throws BoundsError logrange(1, 10, 2)[3]
    @test_throws ArgumentError Base.LogRange{Int}(1,4,5)  # no integer ranges
    @test_throws MethodError Base.LogRange(1,4, length=5)  # type does not take keyword
    # (not sure if these should ideally be DomainError or ArgumentError)
    @test_throws DomainError logrange(1, Inf, 3)
    @test_throws DomainError logrange(0, 2, 3)
    @test_throws DomainError logrange(1, NaN, 3)
    @test_throws DomainError logrange(NaN, 2, 3)

    # printing
    @test repr(Base.LogRange(1,2,3)) == "LogRange{Float64}(1.0, 2.0, 3)"  # like 2-arg show
    @test repr("text/plain", Base.LogRange(1,2,3)) == "3-element Base.LogRange{Float64, Base.TwicePrecision{Float64}}:\n 1.0, 1.41421, 2.0"
    @test repr("text/plain", Base.LogRange(1,2,0)) == "LogRange{Float64}(1.0, 2.0, 0)"  # empty case
end

@testset "_log_twice64_unchecked" begin
    # it roughly works
    @test big(Base._log_twice64_unchecked(exp(1))) ≈ 1.0
    @test big(Base._log_twice64_unchecked(exp(123))) ≈ 123.0

    # it gets high accuracy
    @test abs(big(log(4.0)) - log(big(4.0))) < 1e-16
    @test abs(big(Base._log_twice64_unchecked(4.0)) - log(big(4.0))) < 1e-30

    # it handles subnormals
    @test abs(big(Base._log_twice64_unchecked(1e-310)) - log(big(1e-310))) < 1e-20

    # it accepts negative, NaN, etc without complaint:
    @test Base._log_twice64_unchecked(-0.0).lo isa Float64
    @test Base._log_twice64_unchecked(-1.23).lo isa Float64
    @test Base._log_twice64_unchecked(NaN).lo isa Float64
    @test Base._log_twice64_unchecked(Inf).lo isa Float64
end

@testset "OneTo promotion" begin
    struct MyUnitRange{T} <: AbstractUnitRange{T}
        range::UnitRange{T}
    end
    Base.first(r::MyUnitRange) = first(r.range)
    Base.last(r::MyUnitRange) = last(r.range)
    Base.size(r::MyUnitRange) = size(r.range)
    Base.length(r::MyUnitRange) = length(r.range)
    Base.getindex(r::MyUnitRange, i::Int) = getindex(r.range, i)
    @test promote(MyUnitRange(2:3), Base.OneTo(3)) == (2:3, 1:3)
    @test promote(MyUnitRange(UnitRange(3.0, 4.0)), Base.OneTo(3)) == (3.0:4.0, 1.0:3.0)
end

@testset "StepRange(::StepRangeLen)" begin
    ind = StepRangeLen(2, -1, 2)
    @test StepRange(ind) == ind
    @test StepRange(ind) isa StepRange{eltype(ind), typeof(step(ind))}
    @test StepRange{Int8}(ind) == ind
    @test StepRange{Int8}(ind) isa StepRange{Int8}
    @test StepRange{Int8,Int8}(ind) == ind
    @test StepRange{Int8,Int8}(ind) isa StepRange{Int8,Int8}

    r = StepRangeLen(3, 0, 4)
    @test_throws "step cannot be zero" StepRange(r)

    r = StepRangeLen(Date(2020,1,1), Day(1), 4)
    @test StepRange(r) == r
    @test StepRange(r) isa StepRange{Date,Day}
end
