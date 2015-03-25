# ranges
@test size(10:1:0) == (0,)
@test length(1:.2:2) == 6
@test length(1.:.2:2.) == 6
@test length(2:-.2:1) == 6
@test length(2.:-.2:1.) == 6
@test length(2:.2:1) == 0
@test length(2.:.2:1.) == 0

@test length(1:0) == 0
@test length(0.0:-0.5) == 0
@test length(1:2:0) == 0
L32 = linspace(Int32(1), Int32(4), 4)
L64 = linspace(Int64(1), Int64(4), 4)
@test L32[1] == 1 && L64[1] == 1
@test L32[2] == 2 && L64[2] == 2
@test L32[3] == 3 && L64[3] == 3
@test L32[4] == 4 && L64[4] == 4

r = 5:-1:1
@test r[1]==5
@test r[2]==4
@test r[3]==3
@test r[4]==2
@test r[5]==1

@test length(.1:.1:.3) == 3
@test length(1.1:1.1:3.3) == 3
@test length(1.1:1.3:3) == 2
@test length(1:1:1.8) == 1

@test (1:5)[1:4] == 1:4
@test (2:6)[1:4] == 2:5
@test (1:6)[2:5] == 2:5
@test typeof((1:6)[2:5]) == typeof(2:5)
@test (1:6)[2:2:5] == 2:2:4
@test typeof((1:6)[2:2:5]) == typeof(2:2:4)
@test (1:2:13)[2:6] == 3:2:11
@test typeof((1:2:13)[2:6]) == typeof(3:2:11)
@test (1:2:13)[2:3:7] == 3:6:13
@test typeof((1:2:13)[2:3:7]) == typeof(3:6:13)

@test isempty((1:4)[5:4])
@test_throws BoundsError (1:10)[8:-1:-2]

let
    span = 5:20
    r = -7:3:42
    @test findin(r, span) == 5:10
    r = 15:-2:-38
    @test findin(r, span) == 1:6
end
#@test isempty(findin(5+0*(1:6), 2:4))
#@test findin(5+0*(1:6), 2:5) == 1:6
#@test findin(5+0*(1:6), 2:7) == 1:6
#@test findin(5+0*(1:6), 5:7) == 1:6
#@test isempty(findin(5+0*(1:6), 6:7))
#@test findin(5+0*(1:6), 5:5) == 1:6

@test reverse(reverse(1:10)) == 1:10

@test reverse(reverse(typemin(Int):typemax(Int))) == typemin(Int):typemax(Int)
@test reverse(reverse(typemin(Int):2:typemax(Int))) == typemin(Int):2:typemax(Int)

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
#@test intersect(12+0*(0:6:24), 0:4:24) == Range(12, 0, 5)
#@test isempty(intersect(6+0*(0:6:24), 0:4:24))
@test intersect(-10:3:24, -10:3:24) == -10:3:23
@test isempty(intersect(-11:3:24, -10:3:24))
@test intersect(typemin(Int):2:typemax(Int),1:10) == 2:2:10
@test intersect(1:10,typemin(Int):2:typemax(Int)) == 2:2:10

@test intersect(reverse(typemin(Int):2:typemax(Int)),typemin(Int):2:typemax(Int)) == reverse(typemin(Int):2:typemax(Int))
@test intersect(typemin(Int):2:typemax(Int),reverse(typemin(Int):2:typemax(Int))) == typemin(Int):2:typemax(Int)

@test 0 in UInt(0):100:typemax(Uint)
@test last(UInt(0):100:typemax(Uint)) in UInt(0):100:typemax(Uint)
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

r = 0.0:0.01:1.0
@test (r[30] in r)
r = (-4*Int64(maxintfloat(is(Int,Int32) ? Float32 : Float64))):5
@test (3 in r)
@test (3.0 in r)

@test !(1 in 1:0)
@test !(1.0 in 1.0:0.0)

# indexing range with empty range (#4309)
@test (3:6)[5:4] == 7:6
@test_throws BoundsError (3:6)[5:5]
@test_throws BoundsError (3:6)[5]
@test (0:2:10)[7:6] == 12:2:10
@test_throws BoundsError (0:2:10)[7:7]

# indexing with negative ranges (#8351)
for a=Range[3:6, 0:2:10], b=Range[0:1, 2:-1:0]
    @test_throws BoundsError a[b]
end

# avoiding intermediate overflow (#5065)
@test length(1:4:typemax(Int)) == div(typemax(Int),4) + 1

# overflow in length
@test_throws OverflowError length(0:typemax(Int))
@test_throws OverflowError length(typemin(Int):typemax(Int))
@test_throws OverflowError length(-1:typemax(Int)-1)

let s = 0
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

# sums (see #5798)
if WORD_SIZE == 64
    @test sum(Int128(1):10^18) == div(10^18 * (Int128(10^18)+1), 2)
    @test sum(Int128(1):10^18-1) == div(10^18 * (Int128(10^18)-1), 2)
else
    @test sum(Int64(1):10^9) == div(10^9 * (Int64(10^9)+1), 2)
    @test sum(Int64(1):10^9-1) == div(10^9 * (Int64(10^9)-1), 2)
end

# operations with scalars
@test (1:3) - 2 == -1:1
@test (1:3) - 0.25 == 1-0.25:3-0.25
@test (1:3) + 2 == 3:5
@test (1:3) + 0.25 == 1+0.25:3+0.25
@test (1:2:6) + 1 == 2:2:6
@test (1:2:6) + 0.3 == 1+0.3:2:5+0.3
@test (1:2:6) - 1 == 0:2:4
@test (1:2:6) - 0.3 == 1-0.3:2:5-0.3

# operations between ranges and arrays
@test all(([1:5;] + (5:-1:1)) .== 6)
@test all(((5:-1:1) + [1:5;]) .== 6)
@test all(([1:5;] - (1:5)) .== 0)
@test all(((1:5) - [1:5;]) .== 0)

# tricky floating-point ranges

@test [0.1:0.1:0.3;]   == [linspace(0.1,0.3,3);]     == [1:3;]./10
@test [0.0:0.1:0.3;]   == [linspace(0.0,0.3,4);]     == [0:3;]./10
@test [0.3:-0.1:-0.1;] == [linspace(0.3,-0.1,5);]    == [3:-1:-1;]./10
@test [0.1:-0.1:-0.3;] == [linspace(0.1,-0.3,5);]    == [1:-1:-3;]./10
@test [0.0:0.1:1.0;]   == [linspace(0.0,1.0,11);]    == [0:10;]./10
@test [0.0:-0.1:1.0;]  == [linspace(0.0,1.0,0);]     == []
@test [0.0:0.1:-1.0;]  == [linspace(0.0,-1.0,0);]    == []
@test [0.0:-0.1:-1.0;] == [linspace(0.0,-1.0,11);]   == [0:-1:-10;]./10
@test [1.0:1/49:27.0;] == [linspace(1.0,27.0,1275);] == [49:1323;]./49
@test [0.0:0.7:2.1;]   == [linspace(0.0,2.1,4);]     == [0:7:21;]./10
@test [0.0:1.1:3.3;]   == [linspace(0.0,3.3,4);]     == [0:11:33;]./10
@test [0.1:1.1:3.4;]   == [linspace(0.1,3.4,4);]     == [1:11:34;]./10
@test [0.0:1.3:3.9;]   == [linspace(0.0,3.9,4);]     == [0:13:39;]./10
@test [0.1:1.3:4.0;]   == [linspace(0.1,4.0,4);]     == [1:13:40;]./10
@test [1.1:1.1:3.3;]   == [linspace(1.1,3.3,3);]     == [11:11:33;]./10
@test [0.3:0.1:1.1;]   == [linspace(0.3,1.1,9);]     == [3:1:11;]./10

@test [0.0:1.0:5.5;]   == [0:10:55;]./10
@test [0.0:-1.0:0.5;]  == []
@test [0.0:1.0:0.5;]   == [0.0]

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

for T = (Float32, Float64,),# BigFloat),
    a = -5:25, s = [-5:-1;1:25;], d = 1:25, n = -1:15
    den   = convert(T,d)
    start = convert(T,a)/den
    step  = convert(T,s)/den
    stop  = convert(T,(a+(n-1)*s))/den
    vals  = T[a:s:a+(n-1)*s;]./den
    r = start:step:stop
    @test [r;] == vals
    @test [linspace(start, stop, length(r));] == vals
    # issue #7420
    n = length(r)
    @test [r[1:n];] == [r;]
    @test [r[2:n];] == [r;][2:end]
    @test [r[1:3:n];] == [r;][1:3:n]
    @test [r[2:2:n];] == [r;][2:2:n]
    @test [r[n:-1:2];] == [r;][n:-1:2]
    @test [r[n:-2:1];] == [r;][n:-2:1]
end

# near-equal ranges
@test 0.0:0.1:1.0 != 0.0f0:0.1f0:1.0f0

# comparing and hashing ranges
let
    Rs = Range[1:2, map(Int32,1:3:17), map(Int64,1:3:17), 1:0, 17:-3:0,
               0.0:0.1:1.0, map(Float32,0.0:0.1:1.0)]
    for r in Rs
        ar = collect(r)
        @test r != ar
        @test !isequal(r,ar)
        for s in Rs
            as = collect(s)
            @test !isequal(r,s) || hash(r)==hash(s)
            @test (r==s) == (ar==as)
        end
    end
end

# issue #2959
@test 1.0:1.5 == 1.0:1.0:1.5 == 1.0:1.0
#@test 1.0:(.3-.1)/.1 == 1.0:2.0

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

# issue #6364
@test length((1:64)*(pi/5)) == 64

# issue #6973
let r1 = 1.0:0.1:2.0, r2 = 1.0f0:0.2f0:3.0f0, r3 = 1:2:21
    @test r1 + r1 == 2*r1
    @test r1 + r2 == 2.0:0.3:5.0
    @test (r1 + r2) - r2 == r1
    @test r1 + r3 == convert(FloatRange{Float64}, r3) + r1
    @test r3 + r3 == 2 * r3
end

# issue #7114
r = -0.004532318104333742:1.2597349521122731e-5:0.008065031416788989
@test length(r[1:end-1]) == length(r) - 1
@test isa(r[1:2:end],Range) && length(r[1:2:end]) == div(length(r)+1, 2)
@test_approx_eq r[3:5][2] r[4]
@test_approx_eq r[5:-2:1][2] r[3]
@test_throws BoundsError r[0:10]
@test_throws BoundsError r[1:10000]

r = linspace(1/3,5/7,6)
@test length(r) == 6
@test r[1] == 1/3
@test abs(r[end] - 5/7) <= eps(5/7)
r = linspace(0.25,0.25,1)
@test length(r) == 1
@test_throws Exception linspace(0.25,0.5,1)

# issue #7426
@test [typemax(Int):1:typemax(Int);] == [typemax(Int)]

#issue #7484
r7484 = 0.1:0.1:1
@test [reverse(r7484);] == reverse([r7484;])

# issue #7387
for r in (0:1, 0.0:1.0)
    @test r+im == [r;]+im
    @test r-im == [r;]-im
    @test r*im == [r;]*im
    @test r/im == [r;]/im
end

# issue #7709
@test length(map(identity, 0x01:0x05)) == 5
@test length(map(identity, 0x0001:0x0005)) == 5
@test length(map(identity, UInt64(1):UInt64(5))) == 5
@test length(map(identity, UInt128(1):UInt128(5))) == 5

# mean/median
for f in (mean, median)
    for n = 2:5
        @test f(2:n) == f([2:n;])
        @test_approx_eq f(2:0.1:n) f([2:0.1:n;])
    end
end

# issue #8531
let smallint = (Int === Int64 ?
                (Int8,UInt8,Int16,UInt16,Int32,UInt32) :
                (Int8,UInt8,Int16,UInt16))
    for T in smallint
        @test length(typemin(T):typemax(T)) == 2^(8*sizeof(T))
    end
end

# issue #8584
@test (0:1//2:2)[1:2:3] == 0:1//1:1

# zip
let i = 0
x = 1:2:8
y = 2:2:8
xy = 1:8
for (thisx, thisy) in zip(x, y)
    @test thisx == xy[i+=1]
    @test thisy == xy[i+=1]
end
end

# issue #9962
@test eltype(0:1//3:10) <: Rational
@test (0:1//3:10)[1] == 0
@test (0:1//3:10)[2] == 1//3
