a = BigInt("123456789012345678901234567890")
b = BigInt("123456789012345678901234567891")

@test typeof(a+1) == BigInt
@test a+1 == b
@test isequal(a+1, b)
@test b == a+1
@test !(b == a)
@test b > a
@test b >= a
@test !(b < a)
@test !(b <= a)

c = BigInt("246913578024691357802469135780")
@test typeof(a * 2) == BigInt
@test a*2 == c
@test c-a == a
@test c == a + a
@test c+1 == a+b

d = BigInt("-246913578024691357802469135780")
@test typeof(d) == BigInt
@test d == -c

ee = typemax(Int64)
@test typeof(BigInt(ee)) == BigInt
@test BigInt(ee)+1 == BigInt("9223372036854775808")

#Multiple calls for sanity check, since we're doing direct memory manipulation
@test string(a) == "123456789012345678901234567890"
@test string(b) == "123456789012345678901234567891"
@test string(c) == "246913578024691357802469135780"
@test string(d) == "-246913578024691357802469135780"
@test string(a) == "123456789012345678901234567890"

for i = -10:10, j = [-10:-1,1:10]
    @test div(BigInt(i), BigInt(j)) == div(i,j)
    @test fld(BigInt(i), BigInt(j)) == fld(i,j)
    @test mod(BigInt(i), BigInt(j)) == mod(i,j)
    @test rem(BigInt(i), BigInt(j)) == rem(i,j)
end

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


# Signed addition
@test a+int8(1) == b
@test a+int16(1) == b
@test a+int32(1) == b
@test a+int64(1) == b
@test a+int128(1) == b
@test int8(1)+ a == b
@test int16(1)+a == b
@test int32(1)+a == b
@test int64(1)+a == b
@test b+int8(-1) == a
@test b+int16(-1) == a
@test b+int32(-1) == a
@test b+int64(-1) == a
@test int8(-1)+ b == a
@test int16(-1)+b == a
@test int32(-1)+b == a
@test int64(-1)+b == a

# Unsigned addition
@test a+true == b
@test a+uint8(1) == b
@test a+uint16(1) == b
@test a+uint32(1) == b
@test a+uint64(1) == b
@test a+uint128(1) == b
@test true+a == b
@test uint8(1)+ a == b
@test uint16(1)+a == b
@test uint32(1)+a == b
@test uint64(1)+a == b

# Signed subtraction
@test b-int8(1) == a
@test b-int16(1) == a
@test b-int32(1) == a
@test b-int64(1) == a
@test int8(1)- b == -a
@test int16(1)-b == -a
@test int32(1)-b == -a
@test int64(1)-b == -a
@test a-int8(-1) == b
@test a-int16(-1) == b
@test a-int32(-1) == b
@test a-int64(-1) == b
@test int8(-1)- a == -b
@test int16(-1)-a == -b
@test int32(-1)-a == -b
@test int64(-1)-a == -b

# Unsigned subtraction
@test b-true == a
@test b-uint8(1) == a
@test b-uint16(1) == a
@test b-uint32(1) == a
@test b-uint64(1) == a
@test true-b == -a
@test uint8(1)- b == -a
@test uint16(1)-b == -a
@test uint32(1)-b == -a
@test uint64(1)-b == -a

# Signed multiplication
@test a*int8(1) == a
@test a*int16(1) == a
@test a*int32(1) == a
@test a*int64(1) == a
@test int8(1)* a == a
@test int16(1)*a == a
@test int32(1)*a == a
@test int64(1)*a == a
@test a*int8(-1) == -a
@test a*int16(-1) == -a
@test a*int32(-1) == -a
@test a*int64(-1) == -a
@test int8(-1)* a == -a
@test int16(-1)*a == -a
@test int32(-1)*a == -a
@test int64(-1)*a == -a

# Unsigned multiplication
@test a*true == a
@test a*uint8(1) == a
@test a*uint16(1) == a
@test a*uint32(1) == a
@test a*uint64(1) == a
@test true*a == a
@test uint8(1)* a == a
@test uint16(1)*a == a
@test uint32(1)*a == a
@test uint64(1)*a == a

@test a+BigInt(1) == b

@test BigInt(5) << 3 == 40
@test BigInt(5) >> 1 == 2
@test BigInt(-5) << 3 == -40
@test BigInt(-5) >> 1 == -3

@test ~BigInt(123) == -124
@test BigInt(123) & BigInt(234) == 106
@test BigInt(123) | BigInt(234) == 251
@test BigInt(123) $ BigInt(234) == 145

@test gcd(BigInt(48), BigInt(180)) == 12
@test lcm(BigInt(48), BigInt(180)) == 720

@test factorial(BigInt(40)) == BigInt("815915283247897734345611269596115894272000000000")
@test binomial(BigInt(-53), 42) == BigInt("959509335087854414441273718")
@test binomial(BigInt(113), BigInt(42)) == BigInt("18672199984318438125634054194360")

a = rand(1:100, 10000)
b = map(BigInt, a)
@test sum(a) == sum(b)

# Iterated arithmetic
a = BigInt("315135")
b = BigInt("12412")
c = BigInt("3426495623485904783478347")
d = BigInt("-1398984130")
f = BigInt("2413804710837418037418307081437315263635345357386985747464")
g = BigInt("-1")

@test +(a, b) == BigInt("327547")
@test +(a, b, c) == BigInt("3426495623485904783805894")
@test +(a, b, c, d) == BigInt("3426495623485903384821764")
@test +(a, b, c, d, f) == BigInt("2413804710837418037418307081437318690130968843290370569228")
@test +(a, b, c, d, f, g) == BigInt("2413804710837418037418307081437318690130968843290370569227")

@test *(a, b) == BigInt("3911455620")
@test *(a, b, c) == BigInt("13402585563389346256121263521460140")
@test *(a, b, c, d) == BigInt("-18750004504148804423388563022070650287578200")
@test *(a, b, c, d, f) == BigInt("-45258849200337190631492857400003938881995610529251881450243326128168934937055005474972396281351684800")
@test *(a, b, c, d, f, g) == BigInt("45258849200337190631492857400003938881995610529251881450243326128168934937055005474972396281351684800")

@test ($)(a, b) == BigInt("327299")
@test ($)(a, b, c) == BigInt("3426495623485904783798472")
@test ($)(a, b, c, d) == BigInt("-3426495623485906178489610")
@test ($)(a, b, c, d, f) == BigInt("-2413804710837418037418307081437316711364709261074607933698")
@test ($)(a, b, c, d, f, g) == BigInt("2413804710837418037418307081437316711364709261074607933697")

@test (&)(a, b) == BigInt("124")
@test (&)(a, b, c) == BigInt("72")
@test (&)(a, b, c, d) == BigInt("8")
@test (&)(a, b, c, d, f) == BigInt("8")
@test (&)(a, b, c, d, f, g) == BigInt("8")

@test (|)(a, b) == BigInt("327423")
@test (|)(a, b, c) == BigInt("3426495623485904783802111")
@test (|)(a, b, c, d) == BigInt("-1396834561")
@test (|)(a, b, c, d, f) == BigInt("-1358954753")
@test (|)(a, b, c, d, f, g) == BigInt("-1")

@test isprime(BigInt(1000000007))
@test isprime(BigInt(1000000007), 1)
@test isprime(BigInt(10000000019))
@test isprime(BigInt("359334085968622831041960188598043661065388726959079837"))
@test !isprime(BigInt(1))
@test !isprime(BigInt(10000000020))

@test trailing_ones(a) == 8
@test trailing_zeros(b) == 2
@test count_ones(a) == 14

# Large Fibonacci to exercise BigInt
# from Bill Hart, https://groups.google.com/group/julia-dev/browse_frm/thread/798e2d1322daf633
function mul(a::Vector{BigInt}, b::Vector{BigInt})
   x = a[2]*b[2]
   c = Array(BigInt,3)
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

n = bigfib(1000001)
@test ndigits(n) == 208988
@test mod(n,big(10)^15) == 359244926937501
@test div(n,big(10)^208973) == 316047687386689

s = string(n)
@test length(s) == 208988
@test endswith(s, "359244926937501")
@test beginswith(s, "316047687386689")

# serialization (#5133)
let
    n = BigInt("359334085968622831041960188598043661065388726959079837")
    b = IOBuffer()
    serialize(b,n)
    seek(b,0)
    @test deserialize(b) == n
end

# issue #5873
@test ndigits(big(90)) == 2
@test ndigits(big(99)) == 2
ndigits_mismatch(n) = ndigits(n) != ndigits(BigInt(n))
@test !any(ndigits_mismatch, 8:9)
@test !any(ndigits_mismatch, 64:99)
@test !any(ndigits_mismatch, 512:999)
@test !any(ndigits_mismatch, 8192:9999)

# conversion from float
@test BigInt(2.0) == BigInt(2.0f0) == BigInt(big(2.0)) == 2
@test_throws InexactError convert(BigInt, 2.1)
@test_throws InexactError convert(BigInt, big(2.1))
