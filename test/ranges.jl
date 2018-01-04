# This file is a part of Julia. License is MIT: https://julialang.org/license

using Dates

# Compare precision in a manner sensitive to subnormals, which lose
# precision compared to widening.
function cmp_sn(w, hi, lo, slopbits=0)
    if !isfinite(hi)
        if abs(w) > realmax(typeof(hi))
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
        if abs(w) > realmax(typeof(hi))
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

    x1 = Base.TwicePrecision{Float64}(1)
    x0 = Base.TwicePrecision{Float64}(0)
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
end
@testset "ranges" begin
    @test size(10:1:0) == (0,)
    @testset "colon" begin
        @inferred(colon(10, 1, 0))
        @inferred(colon(1, .2, 2))
        @inferred(colon(1., .2, 2.))
        @inferred(colon(2, -.2, 1))
        @inferred(colon(1, 0))
        @inferred(colon(0.0, -0.5))
    end

    @testset "indexing" begin
        L32 = @inferred(linspace(Int32(1), Int32(4), 4))
        L64 = @inferred(linspace(Int64(1), Int64(4), 4))
        @test @inferred(L32[1]) === 1.0 && @inferred(L64[1]) === 1.0
        @test L32[2] == 2 && L64[2] == 2
        @test L32[3] == 3 && L64[3] == 3
        @test L32[4] == 4 && L64[4] == 4
        @test @inferred(linspace(1.0, 2.0, 2.0f0)) === linspace(1.0, 2.0, 2)
        @test @inferred(linspace(1.0, 2.0, 2))[1] === 1.0
        @test @inferred(linspace(1.0f0, 2.0f0, 2))[1] === 1.0f0
        @test @inferred(linspace(Float16(1.0), Float16(2.0), 2))[1] === Float16(1.0)

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
        @test length(.1:.1:.3) == 3
        @test length(1.1:1.1:3.3) == 3
        @test length(1.1:1.3:3) == 2
        @test length(1:1:1.8) == 1
        @test length(1:.2:2) == 6
        @test length(1.:.2:2.) == 6
        @test length(2:-.2:1) == 6
        @test length(2.:-.2:1.) == 6
        @test length(2:.2:1) == 0
        @test length(2.:.2:1.) == 0

        @test length(1:0) == 0
        @test length(0.0:-0.5) == 0
        @test length(1:2:0) == 0
    end
    @testset "find(::OccursIn, ::Array)" begin
        @test find(occursin(3:20), [5.2, 3.3]) == find(occursin(collect(3:20)), [5.2, 3.3])

        let span = 5:20,
            r = -7:3:42
            @test find(occursin(span), r) == 5:10
            r = 15:-2:-38
            @test find(occursin(span), r) == 1:6
        end
    end
    @testset "reverse" begin
        @test reverse(reverse(1:10)) == 1:10
        @test reverse(reverse(typemin(Int):typemax(Int))) == typemin(Int):typemax(Int)
        @test reverse(reverse(typemin(Int):2:typemax(Int))) == typemin(Int):2:typemax(Int)
    end
    @testset "intersect" begin
        @test intersect(1:5, 2:3) == 2:3
        @test intersect(-3:5, 2:8) == 2:5
        @test intersect(-8:-3, -8:-3) == -8:-3
        @test intersect(1:5, 5:13) == 5:5
        @test isempty(intersect(-8:-3, -2:2))
        @test isempty(intersect(-3:7, 2:1))
        @test intersect(1:11, -2:3:15) == 1:3:10
        @test intersect(1:11, -2:2:15) == 2:2:10
        @test intersect(1:11, -2:1:15) == 1:11
        @test intersect(1:11, 15:-1:-2) == 1:11
        @test intersect(1:11, 15:-4:-2) == 3:4:11
        @test intersect(-20:-5, -10:3:-2) == -10:3:-7
        @test isempty(intersect(-5:5, -6:13:20))
        @test isempty(intersect(1:11, 15:4:-2))
        @test isempty(intersect(11:1, 15:-4:-2))
        #@test intersect(-5:5, 1+0*(1:3)) == 1:1
        #@test isempty(intersect(-5:5, 6+0*(1:3)))
        @test intersect(-15:4:7, -10:-2) == -7:4:-3
        @test intersect(13:-2:1, -2:8) == 7:-2:1
        @test isempty(intersect(13:2:1, -2:8))
        @test isempty(intersect(13:-2:1, 8:-2))
        #@test intersect(5+0*(1:4), 2:8) == 5+0*(1:4)
        #@test isempty(intersect(5+0*(1:4), -7:3))
        @test intersect(0:3:24, 0:4:24) == 0:12:24
        @test intersect(0:4:24, 0:3:24) == 0:12:24
        @test intersect(0:3:24, 24:-4:0) == 0:12:24
        @test intersect(24:-3:0, 0:4:24) == 24:-12:0
        @test intersect(24:-3:0, 24:-4:0) == 24:-12:0
        @test intersect(1:3:24, 0:4:24) == 4:12:16
        @test intersect(0:6:24, 0:4:24) == 0:12:24
        @test isempty(intersect(1:6:2400, 0:4:2400))
        @test intersect(-51:5:100, -33:7:125) == -26:35:79
        @test intersect(-51:5:100, -32:7:125) == -11:35:94
        #@test intersect(0:6:24, 6+0*(0:4:24)) == 6:6:6
        #@test intersect(12+0*(0:6:24), 0:4:24) == AbstractRange(12, 0, 5)
        #@test isempty(intersect(6+0*(0:6:24), 0:4:24))
        @test intersect(-10:3:24, -10:3:24) == -10:3:23
        @test isempty(intersect(-11:3:24, -10:3:24))
        @test intersect(typemin(Int):2:typemax(Int),1:10) == 2:2:10
        @test intersect(1:10,typemin(Int):2:typemax(Int)) == 2:2:10

        @test intersect(reverse(typemin(Int):2:typemax(Int)),typemin(Int):2:typemax(Int)) == reverse(typemin(Int):2:typemax(Int))
        @test intersect(typemin(Int):2:typemax(Int),reverse(typemin(Int):2:typemax(Int))) == typemin(Int):2:typemax(Int)

        @test intersect(UnitRange(1,2),3) == UnitRange(3,2)
        @test intersect(UnitRange(1,2), UnitRange(1,5), UnitRange(3,7), UnitRange(4,6)) == UnitRange(4,3)

        @test intersect(1:3, 2) === intersect(2, 1:3) === 2:2
        @test intersect(1.0:3.0, 2) == intersect(2, 1.0:3.0) == [2.0]
    end
    @testset "sort/sort!/partialsort" begin
        @test sort(UnitRange(1,2)) == UnitRange(1,2)
        @test sort!(UnitRange(1,2)) == UnitRange(1,2)
        @test sort(1:10, rev=true) == 10:-1:1
        @test sort(-3:3, by=abs) == [0,-1,1,-2,2,-3,3]
        @test partialsort(1:10, 4) == 4
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
        #@test (3 in 3+0*(1:5))
        #@test !(4 in 3+0*(1:5))

        let r = 0.0:0.01:1.0
            @test (r[30] in r)
        end
        let r = (-4*Int64(maxintfloat(Int === Int32 ? Float32 : Float64))):5
            @test (3 in r)
            @test (3.0 in r)
        end

        @test !(1 in 1:0)
        @test !(1.0 in 1.0:0.0)
    end
    @testset "in() works across types, including non-numeric types (#21728)" begin
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
    end
end
@testset "indexing range with empty range (#4309)" begin
    @test (3:6)[5:4] == 7:6
    @test_throws BoundsError (3:6)[5:5]
    @test_throws BoundsError (3:6)[5]
    @test (0:2:10)[7:6] == 12:2:10
    @test_throws BoundsError (0:2:10)[7:7]
end
# indexing with negative ranges (#8351)
for a=AbstractRange[3:6, 0:2:10], b=AbstractRange[0:1, 2:-1:0]
    @test_throws BoundsError a[b]
end

# avoiding intermediate overflow (#5065)
@test length(1:4:typemax(Int)) == div(typemax(Int),4) + 1

@testset "overflow in length" begin
    @test_throws OverflowError length(0:typemax(Int))
    @test_throws OverflowError length(typemin(Int):typemax(Int))
    @test_throws OverflowError length(-1:typemax(Int)-1)
end
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

    # loops covering the full range of smaller integer types
    s = 0
    for i = typemin(UInt8):typemax(UInt8)
        s += 1
    end
    @test s == 256

    s = 0
    for i = typemin(UInt8):one(UInt8):typemax(UInt8)
        s += 1
    end
    @test s == 256

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
@testset "broadcasted operations with scalars" begin
    @test broadcast(-, 1:3, 2) == -1:1
    @test broadcast(-, 1:3, 0.25) == 1-0.25:3-0.25
    @test broadcast(+, 1:3, 2) == 3:5
    @test broadcast(+, 1:3, 0.25) == 1+0.25:3+0.25
    @test broadcast(+, 1:2:6, 1) == 2:2:6
    @test broadcast(+, 1:2:6, 0.3) == 1+0.3:2:5+0.3
    @test broadcast(-, 1:2:6, 1) == 0:2:4
    @test broadcast(-, 1:2:6, 0.3) == 1-0.3:2:5-0.3
    @test broadcast(-, 2, 1:3) == 1:-1:-1
end
@testset "operations between ranges and arrays" begin
    @test all(([1:5;] + (5:-1:1)) .== 6)
    @test all(((5:-1:1) + [1:5;]) .== 6)
    @test all(([1:5;] - (1:5)) .== 0)
    @test all(((1:5) - [1:5;]) .== 0)
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
        a = collect(start:step:stop)./10
        ra = collect(r)

        @test r == a
        @test isequal(r, a)

        @test r == ra
        @test isequal(r, ra)

        @test hash(r) == hash(a)
        @test hash(r) == hash(ra)

        if len > 0
            l = linspace(start/10, stop/10, len)
            la = collect(l)

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

    @test 1.0:1/49:27.0 == linspace(1.0,27.0,1275) == [49:1323;]./49
    @test isequal(1.0:1/49:27.0, linspace(1.0,27.0,1275))
    @test isequal(1.0:1/49:27.0, collect(49:1323)./49)
    @test hash(1.0:1/49:27.0) == hash(linspace(1.0,27.0,1275)) == hash(collect(49:1323)./49)

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
@testset "issue #7420 for type $T" for T = (Float32, Float64,), # BigFloat),
    a = -5:25,
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
    @test [linspace(strt, stop, length(r));] == vals
    n = length(r)
    @test [r[1:n];] == [r;]
    @test [r[2:n];] == [r;][2:end]
    @test [r[1:3:n];] == [r;][1:3:n]
    @test [r[2:2:n];] == [r;][2:2:n]
    @test [r[n:-1:2];] == [r;][n:-1:2]
    @test [r[n:-2:1];] == [r;][n:-2:1]
end

@testset "issue #20373 (unliftable ranges with exact end points)" begin
    @test [3*0.05:0.05:0.2;]    == [linspace(3*0.05,0.2,2);]   == [3*0.05,0.2]
    @test [0.2:-0.05:3*0.05;]   == [linspace(0.2,3*0.05,2);]   == [0.2,3*0.05]
    @test [-3*0.05:-0.05:-0.2;] == [linspace(-3*0.05,-0.2,2);] == [-3*0.05,-0.2]
    @test [-0.2:0.05:-3*0.05;]  == [linspace(-0.2,-3*0.05,2);] == [-0.2,-3*0.05]
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
        @test_skip stop == last(r)
        l = linspace(strt,stop,n)
        @test n == length(l)
        @test strt == first(l)
        @test stop  == last(l)
    end
end
@testset "range fuzztests for $T" for T = (Float32, Float64,)
    range_fuzztests(T, 2^15, 1:5)
end

@testset "Inexact errors on 32 bit architectures. #22613" begin
    @test first(linspace(log(0.2), log(10.0), 10)) == log(0.2)
    @test last(linspace(log(0.2), log(10.0), 10)) == log(10.0)
    @test length(Base.floatrange(-3e9, 1.0, 1, 1.0)) == 1
end

@testset "linspace & ranges with very small endpoints for type $T" for T = (Float32, Float64)
    z = zero(T)
    u = eps(z)
    @test first(linspace(u,u,0)) == u
    @test last(linspace(u,u,0)) == u
    @test first(linspace(-u,u,0)) == -u
    @test last(linspace(-u,u,0)) == u
    @test [linspace(-u,u,0);] == []
    @test [linspace(-u,-u,1);] == [-u]
    @test [linspace(-u,u,2);] == [-u,u]
    @test [linspace(-u,u,3);] == [-u,0,u]
    @test first(linspace(-u,-u,0)) == -u
    @test last(linspace(-u,-u,0)) == -u
    @test first(linspace(u,-u,0)) == u
    @test last(linspace(u,-u,0)) == -u
    @test [linspace(u,-u,0);] == []
    @test [linspace(u,u,1);] == [u]
    @test [linspace(u,-u,2);] == [u,-u]
    @test [linspace(u,-u,3);] == [u,0,-u]
    v = linspace(-u,u,12)
    @test length(v) == 12
    @test [-3u:u:3u;] == [linspace(-3u,3u,7);] == [-3:3;].*u
    @test [3u:-u:-3u;] == [linspace(3u,-3u,7);] == [3:-1:-3;].*u
end

@testset "linspace with very large endpoints for type $T" for T = (Float32, Float64)
    largeint = Int(min(maxintfloat(T), typemax(Int)))
    a = realmax()
    for i = 1:5
        @test [linspace(a,a,1);] == [a]
        @test [linspace(-a,-a,1);] == [-a]
        b = realmax()
        for j = 1:5
            @test [linspace(-a,b,0);] == []
            @test [linspace(-a,b,2);] == [-a,b]
            @test [linspace(-a,b,3);] == [-a,(b-a)/2,b]
            @test [linspace(a,-b,0);] == []
            @test [linspace(a,-b,2);] == [a,-b]
            @test [linspace(a,-b,3);] == [a,(a-b)/2,-b]
            for c = largeint-3:largeint
                s = linspace(-a,b,c)
                @test first(s) == -a
                @test last(s) == b
                @test length(s) == c
                s = linspace(a,-b,c)
                @test first(s) == a
                @test last(s) == -b
                @test length(s) == c
            end
            b = prevfloat(b)
        end
        a = prevfloat(a)
    end
end

# issue #20380
let r = LinSpace(1,4,4)
    @test isa(r[1:4], LinSpace)
end

@testset "linspace with 1 or 0 elements (whose step length is NaN)" begin
    @test issorted(linspace(1,1,0))
    @test issorted(linspace(1,1,1))
end
# near-equal ranges
@test 0.0:0.1:1.0 != 0.0f0:0.1f0:1.0f0

# comparing and hashing ranges
@testset "comparing and hashing ranges" begin
    Rs = AbstractRange[1:1, 1:1:1, 1:2, 1:1:2,
                       map(Int32,1:3:17), map(Int64,1:3:17), 1:0, 1:-1:0, 17:-3:0,
                       0.0:0.1:1.0, map(Float32,0.0:0.1:1.0),
                       1.0:eps():1.0 .+ 10eps(), 9007199254740990.:1.0:9007199254740994,
                       linspace(0, 1, 20), map(Float32, linspace(0, 1, 20))]
    for r in Rs
        local r
        ar = collect(r)
        @test r == ar
        @test isequal(r,ar)
        @test hash(r) == hash(ar)
        for s in Rs
            as = collect(s)
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
end
# issue #2959
@test 1.0:1.5 == 1.0:1.0:1.5 == 1.0:1.0
#@test 1.0:(.3-.1)/.1 == 1.0:2.0

@testset "length with typemin/typemax" begin
    let r = typemin(Int64):2:typemax(Int64), s = typemax(Int64):-2:typemin(Int64)
        @test first(r) == typemin(Int64)
        @test last(r) == (typemax(Int64)-1)
        @test_throws OverflowError length(r)

        @test first(s) == typemax(Int64)
        @test last(s) == (typemin(Int64)+1)
        @test_throws OverflowError length(s)
    end

    @test length(typemin(Int64):3:typemax(Int64)) == 6148914691236517206
    @test length(typemax(Int64):-3:typemin(Int64)) == 6148914691236517206

    for s in 3:100
        @test length(typemin(Int):s:typemax(Int)) == length(big(typemin(Int)):big(s):big(typemax(Int)))
        @test length(typemax(Int):-s:typemin(Int)) == length(big(typemax(Int)):big(-s):big(typemin(Int)))
    end

    @test length(UInt(1):UInt(1):UInt(0)) == 0
    @test length(typemax(UInt):UInt(1):(typemax(UInt)-1)) == 0
    @test length(typemax(UInt):UInt(2):(typemax(UInt)-1)) == 0
    @test length((typemin(Int)+3):5:(typemin(Int)+1)) == 0
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

    let r = linspace(1/3, 5/7, 6)
        @test length(r) == 6
        @test r[1] == 1/3
        @test abs(r[end] - 5/7) <= eps(5/7)
    end

    let r = linspace(0.25, 0.25, 1)
        @test length(r) == 1
        @test_throws ArgumentError linspace(0.25,0.5,1)
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
@testset "mean/median" begin
    for f in (mean, median)
        for n = 2:5
            @test f(2:n) == f([2:n;])
            @test f(2:0.1:n) ≈ f([2:0.1:n;])
        end
    end
end
@testset "issue #8531" begin
    smallint = (Int === Int64 ?
                (Int8,UInt8,Int16,UInt16,Int32,UInt32) :
                (Int8,UInt8,Int16,UInt16))
    for T in smallint
        @test length(typemin(T):typemax(T)) == 2^(8*sizeof(T))
    end
end

# issue #8584
@test (0:1//2:2)[1:2:3] == 0:1//1:1

# issue #12278
@test length(1:UInt(0)) == 0

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

    @test convert(LinSpace{Float64}, 0.0:0.1:0.3) === LinSpace{Float64}(0.0, 0.3, 4)
    @test convert(LinSpace, 0.0:0.1:0.3) === LinSpace{Float64}(0.0, 0.3, 4)
    @test convert(LinSpace, 0:3) === LinSpace{Int}(0, 3, 4)

    @test promote('a':'z', 1:2) === ('a':'z', 1:1:2)
    @test eltype(['a':'z', 1:2]) == (StepRange{T,Int} where T)
end

@testset "LinSpace ops" begin
    @test start(LinSpace(0,3,4)) == 1
    @test 2*LinSpace(0,3,4) == LinSpace(0,6,4)
    @test LinSpace(0,3,4)*2 == LinSpace(0,6,4)
    @test LinSpace(0,3,4)/3 == LinSpace(0,1,4)
    @test broadcast(-, 2, LinSpace(0,3,4)) == LinSpace(2,-1,4)
    @test broadcast(+, 2, LinSpace(0,3,4)) == LinSpace(2,5,4)
    @test -LinSpace(0,3,4) == LinSpace(0,-3,4)
    @test reverse(LinSpace(0,3,4)) == LinSpace(3,0,4)
end
@testset "Issue #11245" begin
    io = IOBuffer()
    show(io, linspace(1, 2, 3))
    str = String(take!(io))
#    @test str == "linspace(1.0,2.0,3)"
    @test str == "1.0:0.5:2.0"
end

@testset "issue 10950" begin
    r = 1//2:3
    @test length(r) == 3
    i = 1
    for x in r
        @test x == i//2
        i += 2
    end
    @test i == 7
end

@testset "stringmime/repr" begin
    # stringmime/show should display the range or linspace nicely
    # to test print_range in range.jl
    replstrmime(x) = sprint((io,x) -> show(IOContext(io, :limit => true, :displaysize => (24, 80)), MIME("text/plain"), x), x)
    @test replstrmime(1:4) == "1:4"
    @test stringmime("text/plain", 1:4) == "1:4"
    @test stringmime("text/plain", linspace(1,5,7)) == "1.0:0.6666666666666666:5.0"
    @test stringmime("text/plain", LinSpace{Float64}(1,5,7)) == "7-element LinSpace{Float64}:\n 1.0,1.66667,2.33333,3.0,3.66667,4.33333,5.0"
    @test repr(linspace(1,5,7)) == "1.0:0.6666666666666666:5.0"
    @test repr(LinSpace{Float64}(1,5,7)) == "linspace(1.0,5.0,7)"
    @test replstrmime(0:100.) == "0.0:1.0:100.0"
    # next is to test a very large range, which should be fast because print_range
    # only examines spacing of the left and right edges of the range, sufficient
    # to cover the designated screen size.
    @test replstrmime(linspace(0,100, 10000)) == "0.0:0.010001000100010001:100.0"
    @test replstrmime(LinSpace{Float64}(0,100, 10000)) == "10000-element LinSpace{Float64}:\n 0.0,0.010001,0.020002,0.030003,0.040004,…,99.95,99.96,99.97,99.98,99.99,100.0"

    @test sprint(show, UnitRange(1, 2)) == "1:2"
    @test sprint(show, StepRange(1, 2, 5)) == "1:2:5"
end

@testset "Issue 11049 and related" begin
    @test promote(linspace(0f0, 1f0, 3), linspace(0., 5., 2)) ===
        (linspace(0., 1., 3), linspace(0., 5., 2))
    @test convert(LinSpace{Float64}, linspace(0., 1., 3)) === LinSpace(0., 1., 3)
    @test convert(LinSpace{Float64}, linspace(0f0, 1f0, 3)) === LinSpace(0., 1., 3)

    @test promote(linspace(0., 1., 3), 0:5) === (linspace(0., 1., 3),
                                                 linspace(0., 5., 6))
    @test convert(LinSpace{Float64}, 0:5) === LinSpace(0., 5., 6)
    @test convert(LinSpace{Float64}, 0:1:5) === LinSpace(0., 5., 6)
    @test convert(LinSpace, 0:5) === LinSpace{Int}(0, 5, 6)
    @test convert(LinSpace, 0:1:5) === LinSpace{Int}(0, 5, 6)

    function test_range_index(r, s)
        @test typeof(r[s]) == typeof(r)
        @test [r;][s] == [r[s];]
    end
    test_range_index(linspace(0.1, 0.3, 3), 1:2)
    test_range_index(linspace(0.1, 0.3, 3), 1:0)
    test_range_index(linspace(1.0, 1.0, 1), 1:1)
    test_range_index(linspace(1.0, 1.0, 1), 1:0)
    test_range_index(linspace(1.0, 2.0, 0), 1:0)

    function test_linspace_identity(r::AbstractRange{T}, mr) where T
        @test -r == mr
        @test -collect(r) == collect(mr)
        @test isa(-r, typeof(r))

        @test broadcast(+, broadcast(+, 1, r), -1) == r
        @test 1 .+ collect(r) == collect(1 .+ r) == collect(r .+ 1)
        @test isa(broadcast(+, broadcast(+, 1, r), -1), typeof(r))
        @test broadcast(-, broadcast(-, 1, r), 1) == mr
        @test 1 .- collect(r) == collect(1 .- r) == collect(1 .+ mr)
        @test collect(r) .- 1 == collect(r .- 1) == -collect(mr .+ 1)
        @test isa(broadcast(-, broadcast(-, 1, r), 1), typeof(r))

        @test 1 * r * 1 == r
        @test 2 * r * T(0.5) == r
        @test isa(1 * r * 1, typeof(r))
        @test r / 1 == r
        @test r / 2 * 2 == r
        @test r / T(0.5) * T(0.5) == r
        @test isa(r / 1, typeof(r))

        @test (2 * collect(r) == collect(r * 2) == collect(2 * r) ==
               collect(r * T(2.0)) == collect(T(2.0) * r) ==
               collect(r / T(0.5)) == -collect(mr * T(2.0)))
    end

    test_linspace_identity(linspace(1.0, 27.0, 10), linspace(-1.0, -27.0, 10))
    test_linspace_identity(linspace(1f0, 27f0, 10), linspace(-1f0, -27f0, 10))

    test_linspace_identity(linspace(1.0, 27.0, 0), linspace(-1.0, -27.0, 0))
    test_linspace_identity(linspace(1f0, 27f0, 0), linspace(-1f0, -27f0, 0))

    test_linspace_identity(linspace(1.0, 1.0, 1), linspace(-1.0, -1.0, 1))
    test_linspace_identity(linspace(1f0, 1f0, 1), linspace(-1f0, -1f0, 1))

    @test reverse(linspace(1.0, 27.0, 1275)) == linspace(27.0, 1.0, 1275)
    @test [reverse(linspace(1.0, 27.0, 1275));] ==
        reverse([linspace(1.0, 27.0, 1275);])
end
@testset "PR 12200 and related" begin
    for _r in (1:2:100, 1:100, 1f0:2f0:100f0, 1.0:2.0:100.0,
               linspace(1, 100, 10), linspace(1f0, 100f0, 10))
        float_r = float(_r)
        big_r = big.(_r)
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

    @test_throws DimensionMismatch linspace(1.,5.,5) + linspace(1.,5.,6)
    @test_throws DimensionMismatch linspace(1.,5.,5) - linspace(1.,5.,6)
    @test_throws DimensionMismatch linspace(1.,5.,5) .* linspace(1.,5.,6)
    @test_throws DimensionMismatch linspace(1.,5.,5) ./ linspace(1.,5.,6)

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

        @test collect(r1) + collect(r2) == collect(r_sum)
        @test collect(r2) + collect(r1) == collect(r_sum)
        @test collect(r1) - collect(r2) == collect(r_diff)
        @test collect(r2) - collect(r1) == collect(-r_diff)
    end

    test_range_sum_diff(1:5, 0:2:8, 1:3:13, 1:-1:-3)
    test_range_sum_diff(1.:5., 0.:2.:8., 1.:3.:13., 1.:-1.:-3.)
    test_range_sum_diff(linspace(1.,5.,5), linspace(0.,-4.,5),
                        linspace(1.,1.,5), linspace(1.,9.,5))

    test_range_sum_diff(1:5, 0.:2.:8., 1.:3.:13., 1.:-1.:-3.)
    test_range_sum_diff(1:5, linspace(0, 8, 5),
                        linspace(1, 13, 5), linspace(1, -3, 5))
    test_range_sum_diff(1.:5., linspace(0, 8, 5),
                        linspace(1, 13, 5), linspace(1, -3, 5))
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

@testset "OneTo" begin
    let r = Base.OneTo(-5)
        @test isempty(r)
        @test length(r) == 0
        @test size(r) == (0,)
    end
    let r = Base.OneTo(3)
        @test !isempty(r)
        @test length(r) == 3
        @test size(r) == (3,)
        @test step(r) == 1
        @test first(r) == 1
        @test last(r) == 3
        @test minimum(r) == 1
        @test maximum(r) == 3
        @test r[2] == 2
        @test r[2:3] === 2:3
        @test_throws BoundsError r[4]
        @test_throws BoundsError r[0]
        @test broadcast(+, r, 1) === 2:4
        @test 2*r === 2:2:6
        @test r + r === 2:2:6
        k = 0
        for i in r
            @test i == (k += 1)
        end
        @test intersect(r, Base.OneTo(2)) == Base.OneTo(2)
        @test intersect(r, 0:5) == 1:3
        @test intersect(r, 2) === intersect(2, r) === 2:2
        @test find(occursin(r), r) === find(occursin(1:length(r)), r) ===
              find(occursin(r), 1:length(r)) === 1:length(r)
        io = IOBuffer()
        show(io, r)
        str = String(take!(io))
        @test str == "Base.OneTo(3)"
    end
    let r = Base.OneTo(7)
        @test find(occursin(2:(length(r) - 1)), r) === 2:(length(r) - 1)
        @test find(occursin(r), 2:(length(r) - 1)) === 1:(length(r) - 2)
    end
end

@testset "linspace of other types" begin
    let r = linspace(0, 3//10, 4)
        @test eltype(r) == Rational{Int}
        @test r[2] === 1//10
    end

    let a = 1.0,
        b = nextfloat(1.0),
        ba = BigFloat(a),
        bb = BigFloat(b),
        r = linspace(ba, bb, 3)
        @test eltype(r) == BigFloat
        @test r[1] == a && r[3] == b
        @test r[2] == (ba+bb)/2
    end

    let (a, b) = (rand(10), rand(10)),
        r = linspace(a, b, 5)
        @test r[1] == a && r[5] == b
        for i = 2:4
            x = ((5 - i) // 4) * a + ((i - 1) // 4) * b
            @test r[i] == x
        end
    end
end
@testset "issue #23178" begin
    r = linspace(Float16(0.1094), Float16(0.9697), 300)
    @test r[1] == Float16(0.1094)
    @test r[end] == Float16(0.9697)
end

# issue #20382
let r = @inferred(colon(big(1.0),big(2.0),big(5.0)))
    @test eltype(r) == BigFloat
end

@testset "issue #14420" begin
    for r in (linspace(0.10000000000000045, 1, 50), 0.10000000000000045:(1-0.10000000000000045)/49:1)
        local r
        @test r[1] === 0.10000000000000045
        @test r[end] === 1.0
    end
end
@testset "issue #20381" begin
    r = linspace(-big(1.0),big(1.0),4)
    @test isa(@inferred(r[2]), BigFloat)
    @test r[2] ≈ big(-1.0)/3
end

@testset "issue #20520" begin
    r = linspace(1.3173739f0, 1.3173739f0, 3)
    @test length(r) == 3
    @test first(r) === 1.3173739f0
    @test last(r)  === 1.3173739f0
    @test r[2]     === 1.3173739f0
    r = linspace(1.0, 3+im, 4)
    @test r[1] === 1.0+0.0im
    @test r[2] ≈ (5/3)+(1/3)im
    @test r[3] ≈ (7/3)+(2/3)im
    @test r[4] === 3.0+im
end

# ambiguity between colon methods (#20988)
struct NotReal; val; end
Base.:+(x, y::NotReal) = x + y.val
Base.zero(y::NotReal) = zero(y.val)
Base.rem(x, y::NotReal) = rem(x, y.val)
Base.isless(x, y::NotReal) = isless(x, y.val)
@test colon(1, NotReal(1), 5) isa StepRange{Int,NotReal}

isdefined(Main, :TestHelpers) || @eval Main include("TestHelpers.jl")
using Main.TestHelpers: Furlong
@testset "dimensional correctness" begin
    @test length(collect(Furlong(2):Furlong(10))) == 9
    @test length(range(Furlong(2), 9)) == 9
    @test collect(Furlong(2):Furlong(1):Furlong(10)) == collect(range(Furlong(2),Furlong(1),9)) == Furlong.(2:10)
    @test collect(Furlong(1.0):Furlong(0.5):Furlong(10.0)) ==
          collect(Furlong(1):Furlong(0.5):Furlong(10)) == Furlong.(1:0.5:10)
end

@testset "issue #22270" begin
    linsp = linspace(1.0, 2.0, 10)
    @test typeof(linsp.ref) == Base.TwicePrecision{Float64}
    @test Float32(linsp.ref) === convert(Float32, linsp.ref)
    @test Float32(linsp.ref) ≈ linsp.ref.hi + linsp.ref.lo
end

@testset "logspace" begin
    n = 10; a = 2; b = 4
    # test default values; base = 10
    @test logspace(a, b, 50) == 10 .^ linspace(a, b, 50)
    @test logspace(a, b, n) == 10 .^ linspace(a, b, n)
    for base in (10, 2, ℯ)
        @test logspace(a, b, 50, base=base) == base.^linspace(a, b, 50)
        @test logspace(a, b, n, base=base) == base.^linspace(a, b, n)
    end
end

@testset "issue #23300" begin
    x = -5:big(1.0):5
    @test map(Float64, x) === -5.0:1.0:5.0
    @test map(Float32, x) === -5.0f0:1.0f0:5.0f0
    @test map(Float16, x) === Float16(-5.0):Float16(1.0):Float16(5.0)
    @test map(BigFloat, x) === x
end
