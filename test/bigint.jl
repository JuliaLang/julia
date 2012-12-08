cd("../extras") do
require("bigint")

a=BigInt("123456789012345678901234567890")
b=BigInt("123456789012345678901234567891")

@test typeof(a+1) == BigInt
@test a+1 == b
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

@test div(BigInt(3), BigInt(2)) == BigInt(1)
@test rem(BigInt(3), BigInt(2)) == BigInt(1)

@test a+int(1) == b
@test a+int8(1) == b
@test a+int16(1) == b
@test a+int32(1) == b
@test a+int64(1) == b

@test factorial(BigInt(40)) == BigInt("815915283247897734345611269596115894272000000000")
@test binomial(BigInt(-53), 42) == BigInt("959509335087854414441273718")
@test binomial(BigInt(113), BigInt(42)) == BigInt("18672199984318438125634054194360")

end # cd
