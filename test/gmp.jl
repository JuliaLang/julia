# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random, Serialization

a = parse(BigInt,"123456789012345678901234567890")
b = parse(BigInt,"123456789012345678901234567891")
c = parse(BigInt,"246913578024691357802469135780")
d = parse(BigInt,"-246913578024691357802469135780")
ee = typemax(Int64)
@testset "basics" begin
    @test BigInt <: Signed
    @test big(1) isa Signed

    let x = big(1)
        @test signed(x) === x
        @test convert(Signed, x) === x
        @test Signed(x) === x
        @test_throws MethodError convert(Unsigned, x) # could change in the future
    end
    @test a+BigInt(1) == b
    @test typeof(a+1) == BigInt
    @test a+1 == b
    @test isequal(a+1, b)
    @test b == a+1
    @test !(b == a)
    @test b > a
    @test b >= a
    @test !(b < a)
    @test !(b <= a)

    @test typeof(a * 2) == BigInt
    @test a*2 == c
    @test c-a == a
    @test c == a + a
    @test c+1 == a+b


    @test typeof(d) == BigInt
    @test d == -c

    @test typeof(BigInt(ee)) == BigInt
    @test BigInt(ee)+1 == parse(BigInt,"9223372036854775808")
    @testset "printing" begin
        #Multiple calls for sanity check, since we're doing direct memory manipulation
        @test string(a) == "123456789012345678901234567890"
        @test string(b) == "123456789012345678901234567891"
        @test string(c) == "246913578024691357802469135780"
        @test string(d) == "-246913578024691357802469135780"
        @test string(a) == "123456789012345678901234567890"
    end
    @testset "constructors" begin
        @test typeof(BigInt(typemax(Int8))) == BigInt
        @test typeof(BigInt(typemax(Int16))) == BigInt
        @test typeof(BigInt(typemax(Int32))) == BigInt
        @test typeof(BigInt(typemax(Int64))) == BigInt
        @test typeof(BigInt(typemax(Int128))) == BigInt

        @test typeof(BigInt(true)) == BigInt
        @test typeof(BigInt(typemax(UInt8))) == BigInt
        @test typeof(BigInt(typemax(UInt16))) == BigInt
        @test typeof(BigInt(typemax(UInt32))) == BigInt
        @test typeof(BigInt(typemax(UInt64))) == BigInt
        @test typeof(BigInt(typemax(UInt128))) == BigInt

        @test typeof(BigInt(BigInt(1))) == BigInt

        for x in (Int16(0), 1, 3//4, big(5//6), big(9))
            @test big(typeof(x)) == typeof(big(x))
            @test big(typeof(complex(x, x))) == typeof(big(complex(x, x)))
        end
    end
end
@testset "div, fld, mod, rem" begin
    for i = -10:10, j = [-10:-1; 1:10]
        @test div(BigInt(i), BigInt(j)) == div(i,j)
        @test fld(BigInt(i), BigInt(j)) == fld(i,j)
        @test mod(BigInt(i), BigInt(j)) == mod(i,j)
        @test rem(BigInt(i), BigInt(j)) == rem(i,j)
    end
end
@testset "copysign / sign" begin
    x = BigInt(1)
    y = BigInt(-1)
    @test copysign(x, y) == y
    @test copysign(y, x) == x

    @test sign(BigInt(-3)) == -1
    @test sign(BigInt( 0)) == 0
    @test sign(BigInt( 3)) == 1
end

@testset "Signed addition" begin
    @test a+Int8(1) == b
    @test a+Int16(1) == b
    @test a+Int32(1) == b
    @test a+Int64(1) == b
    @test a+Int128(1) == b
    @test Int8(1)+ a == b
    @test Int16(1)+a == b
    @test Int32(1)+a == b
    @test Int64(1)+a == b
    @test b+Int8(-1) == a
    @test b+Int16(-1) == a
    @test b+Int32(-1) == a
    @test b+Int64(-1) == a
    @test Int8(-1)+ b == a
    @test Int16(-1)+b == a
    @test Int32(-1)+b == a
    @test Int64(-1)+b == a
end
@testset "Unsigned addition" begin
    @test a+true == b
    @test a+UInt8(1) == b
    @test a+UInt16(1) == b
    @test a+UInt32(1) == b
    @test a+UInt64(1) == b
    @test a+UInt128(1) == b
    @test true+a == b
    @test UInt8(1)+ a == b
    @test UInt16(1)+a == b
    @test UInt32(1)+a == b
    @test UInt64(1)+a == b
end
@testset "Signed subtraction" begin
    @test b-Int8(1) == a
    @test b-Int16(1) == a
    @test b-Int32(1) == a
    @test b-Int64(1) == a
    @test Int8(1)- b == -a
    @test Int16(1)-b == -a
    @test Int32(1)-b == -a
    @test Int64(1)-b == -a
    @test a-Int8(-1) == b
    @test a-Int16(-1) == b
    @test a-Int32(-1) == b
    @test a-Int64(-1) == b
    @test Int8(-1)- a == -b
    @test Int16(-1)-a == -b
    @test Int32(-1)-a == -b
    @test Int64(-1)-a == -b
end
@testset "Unsigned subtraction" begin
    @test b-true == a
    @test b-UInt8(1) == a
    @test b-UInt16(1) == a
    @test b-UInt32(1) == a
    @test b-UInt64(1) == a
    @test true-b == -a
    @test UInt8(1)- b == -a
    @test UInt16(1)-b == -a
    @test UInt32(1)-b == -a
    @test UInt64(1)-b == -a
end
@testset "Signed multiplication" begin
    @test a*Int8(1) == a
    @test a*Int16(1) == a
    @test a*Int32(1) == a
    @test a*Int64(1) == a
    @test Int8(1)* a == a
    @test Int16(1)*a == a
    @test Int32(1)*a == a
    @test Int64(1)*a == a
    @test a*Int8(-1) == -a
    @test a*Int16(-1) == -a
    @test a*Int32(-1) == -a
    @test a*Int64(-1) == -a
    @test Int8(-1)* a == -a
    @test Int16(-1)*a == -a
    @test Int32(-1)*a == -a
    @test Int64(-1)*a == -a
end
@testset "Unsigned multiplication" begin
    @test a*true == a
    @test a*UInt8(1) == a
    @test a*UInt16(1) == a
    @test a*UInt32(1) == a
    @test a*UInt64(1) == a
    @test true*a == a
    @test UInt8(1)* a == a
    @test UInt16(1)*a == a
    @test UInt32(1)*a == a
    @test UInt64(1)*a == a
end

@testset "bitshifts" begin
    @test BigInt(5) << 3 == 40
    @test BigInt(5) >> 1 == 2
    @test BigInt(-5) << 3 == -40
    @test BigInt(-5) >> 1 == -3
    @test BigInt(5) >> -3 == 40
    @test BigInt(5) << -1 == 2
    @test BigInt(-5) >> -3 == -40
    @test BigInt(-5) << -1 == -3
end
@testset "boolean ops" begin
    @test ~BigInt(123) == -124
    @test BigInt(123) & BigInt(234) == 106
    @test BigInt(123) | BigInt(234) == 251
    @test BigInt(123) ⊻ BigInt(234) == 145

    @test gcd(BigInt(48), BigInt(180)) == 12
    @test lcm(BigInt(48), BigInt(180)) == 720
end
@testset "combinatorics" begin
    @test factorial(BigInt(40)) == parse(BigInt,"815915283247897734345611269596115894272000000000")
    @test binomial(BigInt(1), -1) == BigInt(0)
    @test binomial(BigInt(1), 2)  == BigInt(0)
    @test binomial(BigInt(-53), 42) == parse(BigInt,"959509335087854414441273718")
    @test binomial(BigInt(113), BigInt(42)) == parse(BigInt,"18672199984318438125634054194360")
end
let a, b
    a = rand(1:100, 10000)
    b = map(BigInt, a)
    @test sum(a) == sum(b)
end

@testset "Iterated arithmetic" begin
    local a, b, c, d, f, g
    a = parse(BigInt,"315135")
    b = parse(BigInt,"12412")
    c = parse(BigInt,"3426495623485904783478347")
    d = parse(BigInt,"-1398984130")
    f = parse(BigInt,"2413804710837418037418307081437315263635345357386985747464")
    g = parse(BigInt,"-1")

    @test +(a, b) == parse(BigInt,"327547")
    @test +(a, b, c) == parse(BigInt,"3426495623485904783805894")
    @test +(a, b, c, d) == parse(BigInt,"3426495623485903384821764")
    @test +(a, b, c, d, f) == parse(BigInt,"2413804710837418037418307081437318690130968843290370569228")
    @test +(a, b, c, d, f, g) == parse(BigInt,"2413804710837418037418307081437318690130968843290370569227")

    @test *(a, b) == parse(BigInt,"3911455620")
    @test *(a, b, c) == parse(BigInt,"13402585563389346256121263521460140")
    @test *(a, b, c, d) == parse(BigInt,"-18750004504148804423388563022070650287578200")
    @test *(a, b, c, d, f) == parse(BigInt,"-45258849200337190631492857400003938881995610529251881450243326128168934937055005474972396281351684800")
    @test *(a, b, c, d, f, g) == parse(BigInt,"45258849200337190631492857400003938881995610529251881450243326128168934937055005474972396281351684800")

    @test xor(a, b) == parse(BigInt,"327299")
    @test xor(a, b, c) == parse(BigInt,"3426495623485904783798472")
    @test xor(a, b, c, d) == parse(BigInt,"-3426495623485906178489610")
    @test xor(a, b, c, d, f) == parse(BigInt,"-2413804710837418037418307081437316711364709261074607933698")
    @test xor(a, b, c, d, f, g) == parse(BigInt,"2413804710837418037418307081437316711364709261074607933697")

    @test (&)(a, b) == parse(BigInt,"124")
    @test (&)(a, b, c) == parse(BigInt,"72")
    @test (&)(a, b, c, d) == parse(BigInt,"8")
    @test (&)(a, b, c, d, f) == parse(BigInt,"8")
    @test (&)(a, b, c, d, f, g) == parse(BigInt,"8")

    @test (|)(a, b) == parse(BigInt,"327423")
    @test (|)(a, b, c) == parse(BigInt,"3426495623485904783802111")
    @test (|)(a, b, c, d) == parse(BigInt,"-1396834561")
    @test (|)(a, b, c, d, f) == parse(BigInt,"-1358954753")
    @test (|)(a, b, c, d, f, g) == parse(BigInt,"-1")

    @test trailing_ones(a) == 8
    @test trailing_zeros(b) == 2
    @test count_ones(a) == 14
end

# Large Fibonacci to exercise BigInt
# from Bill Hart, https://groups.google.com/group/julia-dev/browse_frm/thread/798e2d1322daf633
function mul(a::Vector{BigInt}, b::Vector{BigInt})
   x = a[2]*b[2]
   c = Vector{BigInt}(undef, 3)
   c[1] = a[1]*b[1] + x
   c[2] = a[1]*b[2] + a[2]*b[3]
   c[3] = x + a[3]*b[3]
   return c
end

function bigfib(n)
    n == 0 && return BigInt(0)
    n -= 1
    r = [BigInt(1), BigInt(1), BigInt(0)]
    s = [BigInt(1), BigInt(0), BigInt(1)]
    while true
       (n & 1) == 1 && (s = mul(s,r))
       (n >>= 1) == 0 && return s[1]
       r = mul(r,r)
    end
end
@test [bigfib(n) for n=0:10] == [0,1,1,2,3,5,8,13,21,34,55]

let s, n = bigfib(1000001)
    @test ndigits(n) == 208988
    @test mod(n,big(10)^15) == 359244926937501
    @test div(n,big(10)^208973) == 316047687386689

    s = string(n)
    @test length(s) == 208988
    @test endswith(s, "359244926937501")
    @test startswith(s, "316047687386689")
end

# serialization (#5133)
let n = parse(BigInt, "359334085968622831041960188598043661065388726959079837"),
    b = IOBuffer()
    serialize(b, n)
    seek(b, 0)
    @test deserialize(b) == n
end

@testset "issue #5873" begin
    @test ndigits(big(90)) == 2
    @test ndigits(big(99)) == 2
    ndigits_mismatch(n) = ndigits(n) != ndigits(BigInt(n))
    @test !any(ndigits_mismatch, 8:9)
    @test !any(ndigits_mismatch, 64:99)
    @test !any(ndigits_mismatch, 512:999)
    @test !any(ndigits_mismatch, 8192:9999)
end
# The following should not crash (#16579)
ndigits(big(rand(Int)), base=rand(63:typemax(Int)))
ndigits(big(rand(Int)), base=big(2)^rand(2:999))

for x in big.([-20:20; rand(Int)])
    for _base in -1:1
        @test_throws DomainError ndigits(x, base=_base)
    end
end

@test Base.ndigits0zpb(big(0), big(rand(2:100))) == 0

# digits with BigInt bases (#16844)
@test digits(big(2)^256, base = big(2)^128) == [0, 0, 1]

@testset "conversion from float" begin
    @test BigInt(2.0) == BigInt(2.0f0) == BigInt(big(2.0)) == 2
    @test_throws InexactError convert(BigInt, 2.1)
    @test_throws InexactError convert(BigInt, big(2.1))
end
@testset "issue #13367" begin
    @test trunc(BigInt,2.1) == 2
    @test round(BigInt,2.1) == 2
    @test floor(BigInt,2.1) == 2
    @test ceil(BigInt,2.1) == 3

    @test trunc(BigInt,2.1f0) == 2
    @test round(BigInt,2.1f0) == 2
    @test floor(BigInt,2.1f0) == 2
    @test ceil(BigInt,2.1f0) == 3

    @test_throws InexactError trunc(BigInt,Inf)
    @test_throws InexactError round(BigInt,Inf)
    @test_throws InexactError floor(BigInt,Inf)
    @test_throws InexactError ceil(BigInt,Inf)

    @test string(big(3), base = 2) == "11"
    @test string(big(9), base = 8) == "11"
    @test string(-big(9), base = 8) == "-11"
    @test string(big(12), base = 16) == "c"
end
@testset "Issue #18849" begin
    # bin, oct, dec, hex should not call sizeof on BigInts
    # when padding is desired
    padding = 4
    low = big(4)
    high = big(2^20)
    @test string(low, pad = padding, base = 2) == "0100"
    @test string(low, pad = padding, base = 8) == "0004"
    @test string(low, pad = padding, base = 10) == "0004"
    @test string(low, pad = padding, base = 16) == "0004"

    @test string(high, pad = padding, base = 2) == "100000000000000000000"
    @test string(high, pad = padding, base = 8) == "4000000"
    @test string(high, pad = padding, base = 10) == "1048576"
    @test string(high, pad = padding, base = 16) == "100000"

    @test string(-low, pad = padding, base = 2) == "-0100" # handle negative numbers correctly
    @test string(-low, pad = padding, base = 8) == "-0004"
    @test string(-low, pad = padding, base = 10) == "-0004"
    @test string(-low, pad = padding, base = 16) == "-0004"

    @test string(-high, pad = padding, base = 2) == "-100000000000000000000"
    @test string(-high, pad = padding, base = 8) == "-4000000"
    @test string(-high, pad = padding, base = 10) == "-1048576"
    @test string(-high, pad = padding, base = 16) == "-100000"
end

# respect 0-padding on big(0)
for base in (2, 8, 10, 16)
    local base
    @test string(big(0), base=base, pad=0) == ""
end
@test string(big(0), base = rand(2:62), pad = 0) == ""

@test isqrt(big(4)) == 2
@test isqrt(big(5)) == 2

@test big(5)^true == big(5)
@test big(5)^false == one(BigInt)

@testset "math ops returning BigFloat" begin
    # operations that when applied to Int64 give Float64, should give BigFloat
    @test typeof(exp(a)) == BigFloat
    @test typeof(exp2(a)) == BigFloat
    @test typeof(exp10(a)) == BigFloat
    @test typeof(expm1(a)) == BigFloat
    @test typeof(cosh(a)) == BigFloat
    @test typeof(sinh(a)) == BigFloat
    @test typeof(tanh(a)) == BigFloat
    @test typeof(sech(a)) == BigFloat
    @test typeof(csch(a)) == BigFloat
    @test typeof(coth(a)) == BigFloat
    @test typeof(cbrt(a)) == BigFloat
    @test typeof(tan(a)) == BigFloat
    @test typeof(cos(a)) == BigFloat
    @test typeof(sin(a)) == BigFloat
end

# Issue #24298
@test mod(BigInt(6), UInt(5)) == mod(6, 5)

@testset "cmp has values in [-1, 0, 1], issue #28780" begin
    # _rand produces values whose log2 is better distributed than rand
    _rand(::Type{BigInt}, n=1000) = let x = big(2)^rand(1:rand(1:n))
        rand(-x:x)
    end
    _rand(F::Type{<:AbstractFloat}) = F(_rand(BigInt, round(Int, log2(floatmax(F))))) + rand(F)
    _rand(T) = rand(T)
    for T in (Base.BitInteger_types..., BigInt, Float64, Float32, Float16)
        @test cmp(big(2)^130, one(T)) === 1
        @test cmp(-big(2)^130, one(T)) === -1
        c = cmp(_rand(BigInt), _rand(T))
        @test c ∈ (-1, 0, 1)
        @test c isa Int
        (T <: Integer && T !== BigInt) || continue
        x = rand(T)
        @test cmp(big(2)^130, x) === cmp(x, -big(2)^130) === 1
        @test cmp(-big(2)^130, x) === cmp(x, big(2)^130) === -1
        @test cmp(big(x), x) === cmp(x, big(x)) === 0
    end
    c = cmp(_rand(BigInt), _rand(BigInt))
    @test c ∈ (-1, 0, 1)
    @test c isa Int
end

@testset "generic conversion from Integer" begin
    x = rand(Int128)
    @test BigInt(x) % Int128 === x
    y = rand(UInt128)
    @test BigInt(y) % UInt128 === y
end

@testset "conversion from typemin(T)" begin
    @test big(Int8(-128)) == big"-128"
    @test big(Int16(-32768)) == big"-32768"
    @test big(Int32(-2147483648)) == big"-2147483648"
    @test big(Int64(-9223372036854775808)) == big"-9223372036854775808"
    @test big(Int128(-170141183460469231731687303715884105728)) == big"-170141183460469231731687303715884105728"
end

@testset "conversion to Float" begin
    x = big"2"^256 + big"2"^(256-53) + 1
    @test Float64(x) == reinterpret(Float64, 0x4ff0000000000001)
    @test Float64(-x) == -Float64(x)
    x = (x >> 1) + 1
    @test Float64(x) == reinterpret(Float64, 0x4fe0000000000001)
    @test Float64(-x) == -Float64(x)

    x = big"2"^64 + big"2"^(64-24) + 1
    @test Float32(x) == reinterpret(Float32, 0x5f800001)
    @test Float32(-x) == -Float32(x)
    x = (x >> 1) + 1
    @test Float32(x) == reinterpret(Float32, 0x5f000001)
    @test Float32(-x) == -Float32(x)

    x = big"2"^15 + big"2"^(15-11) + 1
    @test Float16(x) == reinterpret(Float16, 0x7801)
    @test Float16(-x) == -Float16(x)
    x = (x >> 1) + 1
    @test Float16(x) == reinterpret(Float16, 0x7401)
    @test Float16(-x) == -Float16(x)

    for T in (Float16, Float32, Float64)
        n = exponent(floatmax(T))
        @test T(big"2"^(n+1)) === T(Inf)
        @test T(big"2"^(n+1) - big"2"^(n-precision(T))) === T(Inf)
        @test T(big"2"^(n+1) - big"2"^(n-precision(T)) - 1) === floatmax(T)
    end
end
