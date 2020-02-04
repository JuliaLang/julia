# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

@testset "Rationals" begin
    @test 1//1 == 1
    @test 2//2 == 1
    @test 1//1 == 1//1
    @test 2//2 == 1//1
    @test 2//4 == 3//6
    @test 1//2 + 1//2 == 1
    @test (-1)//3 == -(1//3)
    @test 1//2 + 3//4 == 5//4
    @test 1//3 * 3//4 == 1//4
    @test 1//2 / 3//4 == 2//3
    @test 1//0 == 1//0
    @test 5//0 == 1//0
    @test -1//0 == -1//0
    @test -7//0 == -1//0

    @test_throws OverflowError -(0x01//0x0f)
    @test_throws OverflowError -(typemin(Int)//1)
    @test_throws OverflowError (typemax(Int)//3) + 1
    @test_throws OverflowError (typemax(Int)//3) * 2
    @test (typemax(Int)//1) * (1//typemax(Int)) == 1
    @test (typemax(Int)//1) / (typemax(Int)//1) == 1
    @test (1//typemax(Int)) / (1//typemax(Int)) == 1
    @test_throws OverflowError (1//2)^63

    @test @inferred(rationalize(Int, 3.0, 0.0)) === 3//1
    @test @inferred(rationalize(Int, 3.0, 0)) === 3//1
    @test_throws ArgumentError rationalize(Int, big(3.0), -1.)
    # issue 26823
    @test_throws InexactError rationalize(Int, NaN)
    # issue 32569
    @test_throws ArgumentError 1 // typemin(Int)
    @test_throws ArgumentError 0 // 0
    @test -2 // typemin(Int) == -1 // (typemin(Int) >> 1)
    @test 2 // typemin(Int) == 1 // (typemin(Int) >> 1)

    for a = -5:5, b = -5:5
        if a == b == 0; continue; end
        if ispow2(b)
            @test a//b == a/b
            @test convert(Rational,a/b) == a//b
        end
        @test rationalize(a/b) == a//b
        @test a//b == a//b
        if b == 0
            @test_throws DivideError round(Integer,a//b) == round(Integer,a/b)
        else
            @test round(Integer,a//b) == round(Integer,a/b)
        end
        for c = -5:5
            @test (a//b == c) == (a/b == c)
            @test (a//b != c) == (a/b != c)
            @test (a//b <= c) == (a/b <= c)
            @test (a//b <  c) == (a/b <  c)
            @test (a//b >= c) == (a/b >= c)
            @test (a//b >  c) == (a/b >  c)
            for d = -5:5
                if c == d == 0; continue; end
                @test (a//b == c//d) == (a/b == c/d)
                @test (a//b != c//d) == (a/b != c/d)
                @test (a//b <= c//d) == (a/b <= c/d)
                @test (a//b <  c//d) == (a/b <  c/d)
                @test (a//b >= c//d) == (a/b >= c/d)
                @test (a//b >  c//d) == (a/b >  c/d)
            end
        end
    end

    @test 0.5 == 1//2
    @test 0.1 != 1//10
    @test 0.1 == 3602879701896397//36028797018963968
    @test Inf == 1//0 == 2//0 == typemax(Int)//0
    @test -Inf == -1//0 == -2//0 == -typemax(Int)//0
    @test floatmin() != 1//(BigInt(2)^1022+1)
    @test floatmin() == 1//(BigInt(2)^1022)
    @test floatmin() != 1//(BigInt(2)^1022-1)
    @test floatmin()/2 != 1//(BigInt(2)^1023+1)
    @test floatmin()/2 == 1//(BigInt(2)^1023)
    @test floatmin()/2 != 1//(BigInt(2)^1023-1)
    @test nextfloat(0.0) != 1//(BigInt(2)^1074+1)
    @test nextfloat(0.0) == 1//(BigInt(2)^1074)
    @test nextfloat(0.0) != 1//(BigInt(2)^1074-1)

    @test 1/3 < 1//3
    @test !(1//3 < 1/3)
    @test -1/3 < 1//3
    @test -1/3 > -1//3
    @test 1/3 > -1//3
    @test 1/5 > 1//5
    @test 1//3 < Inf
    @test 0//1 < Inf
    @test 1//0 == Inf
    @test -1//0 == -Inf
    @test -1//0 != Inf
    @test 1//0 != -Inf
    @test !(1//0 < Inf)
    @test !(1//3 < NaN)
    @test !(1//3 == NaN)
    @test !(1//3 > NaN)

    # PR 29561
    @test abs(one(Rational{UInt})) === one(Rational{UInt})
    @test abs(one(Rational{Int})) === one(Rational{Int})
    @test abs(-one(Rational{Int})) === one(Rational{Int})
end

@testset "Rational methods" begin
    rand_int = rand(Int8)

    for T in [Int8, Int16, Int32, Int128, BigInt]
        @test numerator(convert(T, rand_int)) == rand_int
        @test denominator(convert(T, rand_int)) == 1

        @test typemin(Rational{T}) == -one(T)//zero(T)
        @test typemax(Rational{T}) == one(T)//zero(T)
        @test widen(Rational{T}) == Rational{widen(T)}
    end

    @test Rational(Float32(rand_int)) == Rational(rand_int)

    @test Rational(Rational(rand_int)) == Rational(rand_int)

    @test begin
        var = -Rational(UInt32(0))
        var == UInt32(0)
    end

    @test Rational(rand_int, 3)/Complex(3, 2) == Complex(Rational(rand_int, 13), -Rational(rand_int*2, 39))

    @test Complex(rand_int, 0) == Rational(rand_int)
    @test Rational(rand_int) == Complex(rand_int, 0)

    @test (Complex(rand_int, 4) == Rational(rand_int)) == false
    @test (Rational(rand_int) == Complex(rand_int, 4)) == false

    @test trunc(Rational(BigInt(rand_int), BigInt(3))) == Rational(trunc(BigInt, Rational(BigInt(rand_int),BigInt(3))))
    @test  ceil(Rational(BigInt(rand_int), BigInt(3))) == Rational( ceil(BigInt, Rational(BigInt(rand_int),BigInt(3))))
    @test round(Rational(BigInt(rand_int), BigInt(3))) == Rational(round(BigInt, Rational(BigInt(rand_int),BigInt(3))))


    for a = -3:3
        @test Rational(Float32(a)) == Rational(a)
        @test Rational(a)//2 == a//2
        @test a//Rational(2) == Rational(a/2)
        @test a.//[-2, -1, 1, 2] == [-a//2, -a//1, a//1, a//2]
        for b=-3:3, c=1:3
            @test b//(a+c*im) == b*a//(a^2+c^2)-(b*c//(a^2+c^2))*im
            for d=-3:3
                @test (a+b*im)//(c+d*im) == (a*c+b*d+(b*c-a*d)*im)//(c^2+d^2)
                @test Complex(Rational(a)+b*im)//Complex(Rational(c)+d*im) == Complex(a+b*im)//Complex(c+d*im)
            end
        end
    end
end

# check type of constructed rationals
int_types = Base.BitInteger64_types
for N = int_types, D = int_types
    T = promote_type(N,D)
    @test typeof(convert(N,2)//convert(D,3)) <: Rational{T}
end

# issue #7564
@test typeof(convert(Rational{Integer},1)) === Rational{Integer}

@testset "issue #15205" begin
    T = Rational
    x = Complex{T}(1//3 + 1//4*im)
    y = Complex{T}(1//2 + 1//5*im)
    xf = Complex{BigFloat}(1//3 + 1//4*im)
    yf = Complex{BigFloat}(1//2 + 1//5*im)
    yi = 4

    @test x^y ≈ xf^yf
    @test x^yi ≈ xf^yi
    @test x^true ≈ xf^true
    @test x^false == xf^false
    @test x^1 ≈ xf^1
    @test xf^Rational(2, 1) ≈ xf*xf
    @test Complex(1., 1.)^Rational(2,1) == Complex(1., 1.)*Complex(1.,1.) == Complex(0., 2.)

    for Tf = (Float16, Float32, Float64), Ti = (Int16, Int32, Int64)
        almost_half  = Rational(div(typemax(Ti),Ti(2))  , typemax(Ti))
        over_half    = Rational(div(typemax(Ti),Ti(2))+one(Ti), typemax(Ti))
        exactly_half = Rational(one(Ti)  , Ti(2))

        @test round( almost_half) == 0//1
        @test round(-almost_half) == 0//1
        @test round(Tf,  almost_half, RoundNearestTiesUp) == 0.0
        @test round(Tf, -almost_half, RoundNearestTiesUp) == 0.0
        @test round(Tf,  almost_half, RoundNearestTiesAway) == 0.0
        @test round(Tf, -almost_half, RoundNearestTiesAway) == 0.0

        @test round( exactly_half) == 0//1 # rounds to closest _even_ integer
        @test round(-exactly_half) == 0//1 # rounds to closest _even_ integer
        @test round(Tf,  exactly_half, RoundNearestTiesUp) == 1.0
        @test round(Tf, -exactly_half, RoundNearestTiesUp) == 0.0
        @test round(Tf,  exactly_half, RoundNearestTiesAway) == 1.0
        @test round(Tf, -exactly_half, RoundNearestTiesAway) == -1.0


        @test round(over_half) == 1//1
        @test round(-over_half) == -1//1
        @test round(Tf,  over_half, RoundNearestTiesUp) == 1.0
        @test round(Tf,  over_half, RoundNearestTiesAway) == 1.0
        @test round(Tf, -over_half, RoundNearestTiesUp) == -1.0
        @test round(Tf, -over_half, RoundNearestTiesAway) == -1.0

        @test round(Tf, 11//2, RoundNearestTiesUp) == 6.0
        @test round(Tf, -11//2, RoundNearestTiesUp) == -5.0
        @test round(Tf, 11//2, RoundNearestTiesAway) == 6.0
        @test round(Tf, -11//2, RoundNearestTiesAway) == -6.0

        @test round(Tf, Ti(-1)//zero(Ti)) == -Inf
        @test round(Tf, one(1)//zero(Ti)) == Inf
        @test round(Tf, Ti(-1)//zero(Ti), RoundNearestTiesUp) == -Inf
        @test round(Tf, one(1)//zero(Ti), RoundNearestTiesUp) == Inf
        @test round(Tf, Ti(-1)//zero(Ti), RoundNearestTiesAway) == -Inf
        @test round(Tf, one(1)//zero(Ti), RoundNearestTiesAway) == Inf

        @test round(Tf, zero(Ti)//one(Ti)) == 0
        @test round(Tf, zero(Ti)//one(Ti), RoundNearestTiesUp) == 0
        @test round(Tf, zero(Ti)//one(Ti), RoundNearestTiesAway) == 0
    end
end
@testset "show and Rationals" begin
    io = IOBuffer()
    rational1 = Rational(1465, 8593)
    rational2 = Rational(-4500, 9000)
    @test sprint(show, rational1) == "1465//8593"
    @test sprint(show, rational2) == "-1//2"
    let
        io1 = IOBuffer()
        write(io1, rational1)
        io1.ptr = 1
        @test read(io1, typeof(rational1)) == rational1

        io2 = IOBuffer()
        write(io2, rational2)
        io2.ptr = 1
        @test read(io2, typeof(rational2)) == rational2
    end
end

@testset "round" begin
    @test round(11//2) == round(11//2, RoundNearest) == 6//1 # rounds to closest _even_ integer
    @test round(-11//2) == round(-11//2, RoundNearest) == -6//1 # rounds to closest _even_ integer
    @test round(13//2) == round(13//2, RoundNearest) == 6//1 # rounds to closest _even_ integer
    @test round(-13//2) == round(-13//2, RoundNearest) == -6//1 # rounds to closest _even_ integer
    @test round(11//3) == round(11//3, RoundNearest) == 4//1 # rounds to closest _even_ integer
    @test round(-11//3) == round(-11//3, RoundNearest) == -4//1 # rounds to closest _even_ integer

    @test round(11//2, RoundNearestTiesAway) == 6//1
    @test round(-11//2, RoundNearestTiesAway) == -6//1
    @test round(13//2, RoundNearestTiesAway) == 7//1
    @test round(-13//2, RoundNearestTiesAway) == -7//1
    @test round(11//3, RoundNearestTiesAway) == 4//1
    @test round(-11//3, RoundNearestTiesAway) == -4//1

    @test round(11//2, RoundNearestTiesUp) == 6//1
    @test round(-11//2, RoundNearestTiesUp) == -5//1
    @test round(13//2, RoundNearestTiesUp) == 7//1
    @test round(-13//2, RoundNearestTiesUp) == -6//1
    @test round(11//3, RoundNearestTiesUp) == 4//1
    @test round(-11//3, RoundNearestTiesUp) == -4//1

    @test round(11//2, RoundToZero) == 5//1
    @test round(-11//2, RoundToZero) == -5//1
    @test round(13//2, RoundToZero) == 6//1
    @test round(-13//2, RoundToZero) == -6//1
    @test round(11//3, RoundToZero) == 3//1
    @test round(-11//3, RoundToZero) == -3//1

    @test round(11//2, RoundUp) == 6//1
    @test round(-11//2, RoundUp) == -5//1
    @test round(13//2, RoundUp) == 7//1
    @test round(-13//2, RoundUp) == -6//1
    @test round(11//3, RoundUp) == 4//1
    @test round(-11//3, RoundUp) == -3//1

    @test round(11//2, RoundDown) == 5//1
    @test round(-11//2, RoundDown) == -6//1
    @test round(13//2, RoundDown) == 6//1
    @test round(-13//2, RoundDown) == -7//1
    @test round(11//3, RoundDown) == 3//1
    @test round(-11//3, RoundDown) == -4//1

    for T in (Float16, Float32, Float64)
        @test round(T, true//false) === convert(T, Inf)
        @test round(T, true//true) === one(T)
        @test round(T, false//true) === zero(T)
    end

    for T in (Int8, Int16, Int32, Int64, Bool)
        @test_throws DivideError round(T, true//false)
        @test round(T, true//true) === one(T)
        @test round(T, false//true) === zero(T)
    end
end

@testset "issue 1552" begin
    @test isa(rationalize(Int8, float(pi)), Rational{Int8})
    @test rationalize(Int8, float(pi)) == 22//7
    @test rationalize(Int64, 0.957762604052997) == 42499549//44373782
    @test rationalize(Int16, 0.929261477046077) == 11639//12525
    @test rationalize(Int16, 0.2264705884044309) == 77//340
    @test rationalize(Int16, 0.39999899264235683) == 2//5
    @test rationalize(Int16, 1.1264233500618559e-5) == 0//1
    @test rationalize(UInt16, 0.6666652791223875) == 2//3
    @test rationalize(Int8, 0.9374813124660655) == 15//16
    @test rationalize(Int8, 0.003803032342443835) == 0//1
end
# issue 3412
@test convert(Rational{Int32},0.5) === Int32(1)//Int32(2)

@testset "issue 6712" begin
    @test convert(Rational{BigInt},Float64(pi)) == Float64(pi)
    @test convert(Rational{BigInt},big(pi)) == big(pi)

    @test convert(Rational,0.0) == 0
    @test convert(Rational,-0.0) == 0
    @test convert(Rational,zero(BigFloat)) == 0
    @test convert(Rational,-zero(BigFloat)) == 0
    @test convert(Rational{BigInt},0.0) == 0
    @test convert(Rational{BigInt},-0.0) == 0
    @test convert(Rational{BigInt},zero(BigFloat)) == 0
    @test convert(Rational{BigInt},-zero(BigFloat)) == 0
    @test convert(Rational{BigInt},5e-324) == 5e-324
    @test convert(Rational{BigInt},floatmin(Float64)) == floatmin(Float64)
    @test convert(Rational{BigInt},floatmax(Float64)) == floatmax(Float64)

    @test isa(convert(Float64, big(1)//2), Float64)
end
@testset "issue 16513" begin
    @test convert(Rational{Int32}, pi) == 1068966896 // 340262731
    @test convert(Rational{Int64}, pi) == 2646693125139304345 // 842468587426513207
    @test convert(Rational{Int128}, pi) == 60728338969805745700507212595448411044 // 19330430665609526556707216376512714945
    @test_throws ArgumentError convert(Rational{BigInt}, pi)
end
@testset "issue 5935" begin
    @test rationalize(Int8,  nextfloat(0.1)) == 1//10
    @test rationalize(Int64, nextfloat(0.1)) == 300239975158034//3002399751580339
    @test rationalize(Int128,nextfloat(0.1)) == 300239975158034//3002399751580339
    @test rationalize(BigInt,nextfloat(0.1)) == 300239975158034//3002399751580339
    @test rationalize(Int8,  nextfloat(0.1),tol=0.5eps(0.1)) == 1//10
    @test rationalize(Int64, nextfloat(0.1),tol=0.5eps(0.1)) == 379250494936463//3792504949364629
    @test rationalize(Int128,nextfloat(0.1),tol=0.5eps(0.1)) == 379250494936463//3792504949364629
    @test rationalize(BigInt,nextfloat(0.1),tol=0.5eps(0.1)) == 379250494936463//3792504949364629
    @test rationalize(Int8,  nextfloat(0.1),tol=1.5eps(0.1)) == 1//10
    @test rationalize(Int64, nextfloat(0.1),tol=1.5eps(0.1)) == 1//10
    @test rationalize(Int128,nextfloat(0.1),tol=1.5eps(0.1)) == 1//10
    @test rationalize(BigInt,nextfloat(0.1),tol=1.5eps(0.1)) == 1//10
    @test rationalize(BigInt,nextfloat(parse(BigFloat,"0.1")),tol=1.5eps(big(0.1))) == 1//10
    @test rationalize(Int64, nextfloat(0.1),tol=0) == 7205759403792795//72057594037927936
    @test rationalize(Int128,nextfloat(0.1),tol=0) == 7205759403792795//72057594037927936
    @test rationalize(BigInt,nextfloat(0.1),tol=0) == 7205759403792795//72057594037927936

    @test rationalize(Int8,  prevfloat(0.1)) == 1//10
    @test rationalize(Int64, prevfloat(0.1)) == 1//10
    @test rationalize(Int128,prevfloat(0.1)) == 1//10
    @test rationalize(BigInt,prevfloat(0.1)) == 1//10
    @test rationalize(BigInt,prevfloat(parse(BigFloat,"0.1"))) == 1//10
    @test rationalize(Int64, prevfloat(0.1),tol=0) == 7205759403792793//72057594037927936
    @test rationalize(Int128,prevfloat(0.1),tol=0) == 7205759403792793//72057594037927936
    @test rationalize(BigInt,prevfloat(0.1),tol=0) == 7205759403792793//72057594037927936

    @test rationalize(BigInt,nextfloat(parse(BigFloat,"0.1")),tol=0) == 46316835694926478169428394003475163141307993866256225615783033603165251855975//463168356949264781694283940034751631413079938662562256157830336031652518559744


    @test rationalize(Int8, 200f0) == 1//0
    @test rationalize(Int8, -200f0) == -1//0

    @test [rationalize(1pi,tol=0.1^n) for n=1:10] == [
                 16//5
                 22//7
                201//64
                333//106
                355//113
                355//113
              75948//24175
             100798//32085
             103993//33102
             312689//99532 ]
end

@testset "issue #12536" begin
    @test Rational{Int16}(1,2) === Rational(Int16(1),Int16(2))
    @test Rational{Int16}(500000,1000000) === Rational(Int16(1),Int16(2))
end
# issue 16311
rationalize(nextfloat(0.0)) == 0//1

@testset "rational-exponent promotion rules (issue #3155)" begin
    @test 2.0f0^(1//3) == 2.0f0^(1.0f0/3)
    @test 2^(1//3) == 2^(1/3)
end

@testset "overflow in rational comparison" begin
    @test 3//2 < typemax(Int)
    @test 3//2 <= typemax(Int)
end

# issue #15920
@test Rational(0, 1) / Complex(3, 2) == 0

# issue #16282
@test_throws MethodError 3 // 4.5im

# issue #31396
@test round(1//2, RoundNearestTiesUp) === 1//1

@testset "Unary plus on Rational (issue #30749)" begin
   @test +Rational(true) == 1//1
   @test +Rational(false) == 0//1
   @test -Rational(true) == -1//1
   @test -Rational(false) == 0//1
end

# issue #27039
@testset "gcd, lcm, gcdx for Rational" begin
    a = 6 // 35
    b = 10 // 21
    @test gcd(a, b) == 2//105
    @test lcm(a, b) == 30//7
    @test gcdx(a, b) == (2//105, -11, 4)

    @test gcdx(1//0, 1//2) == (1//0, 1, 0)
    @test gcdx(1//2, 1//0) == (1//0, 0, 1)
    @test gcdx(1//0, 1//0) == (1//0, 1, 1)
    @test gcdx(1//0, 0//1) == (1//0, 1, 0)
    @test gcdx(0//1, 0//1) == (0//1, 1, 0)

    @test gcdx(1//3, 2) == (1//3, 1, 0)
    @test lcm(1//3, 1) == 1//1
    @test lcm(3//1, 1//0) == 3//1
    @test lcm(0//1, 1//0) == 0//1

    @test gcd([5, 2, 1//2]) == 1//2
end

