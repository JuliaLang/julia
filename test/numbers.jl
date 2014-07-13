# basic booleans
@test true
@test !false
@test !!true
@test !!!false

@test true  == true
@test false == false
@test true  != false
@test false != true

@test ~true == false
@test ~false == true

@test false & false == false
@test true  & false == false
@test false & true  == false
@test true  & true  == true

@test false | false == false
@test true  | false == true
@test false | true  == true
@test true  | true  == true

@test false $ false == false
@test true  $ false == true
@test false $ true  == true
@test true  $ true  == false

# the bool operator
@test bool(false) == false
@test bool(true) == true
@test bool(0) == false
@test bool(1) == true
@test bool(-1) == true
@test bool(0.0) == false
@test bool(1.0) == true
@test bool(0.1) == true
@test bool(-1.0) == true
@test bool(Complex(0,0)) == false
@test bool(Complex(1,0)) == true
@test_throws InexactError bool(Complex(0,1)) == true
@test bool(0//1) == false
@test bool(1//1) == true
@test bool(1//2) == true

# basic arithmetic
@test 2+3 == 5
@test 2.+3. == 5.
@test 2*3 == 6
@test 2.*3 == 6
@test 2. * 3. == 6.
@test min(1.0,1) == 1

@test minmax(5, 3) == (3, 5)
@test minmax(3., 5.) == (3., 5.)
@test minmax(5., 3.) == (3., 5.)
@test minmax(3., NaN) == (3., 3.)
@test minmax(NaN, 3.) == (3., 3.)
@test isequal(minmax(NaN, NaN), (NaN, NaN))

# lexing typemin(Int64)
@test (-9223372036854775808)^1 == -9223372036854775808
@test [1 -1 -9223372036854775808] == [1 -1 typemin(Int64)]

# large integer literals
@test isa(-170141183460469231731687303715884105729,BigInt)
@test isa(-170141183460469231731687303715884105728,Int128)
@test isa(-9223372036854775809,Int128)
@test isa(-9223372036854775808,Int64)
@test isa(9223372036854775807,Int64)
@test isa(9223372036854775808,Int128)
@test isa(170141183460469231731687303715884105727,Int128)
@test isa(170141183460469231731687303715884105728,BigInt)

@test isa(0170141183460469231731687303715884105728,BigInt)

# exponentiating with a negative base
@test -3^2 == -9
@test -9223372036854775808^2 == -(9223372036854775808^2)
@test -10000000000000000000^2 == -(10000000000000000000^2)
@test -170141183460469231731687303715884105728^2 ==
    -(170141183460469231731687303715884105728^2)

# numeric literal coefficients
let x = 10
    @test 2x == 20
    @test 9223372036854775808x == 92233720368547758080
    @test 170141183460469231731687303715884105728x ==
        1701411834604692317316873037158841057280
end

@test 2(10) == 20
@test 9223372036854775808(10) == 92233720368547758080
@test 170141183460469231731687303715884105728(10) ==
    1701411834604692317316873037158841057280

# definition and printing of extreme integers
@test bin(typemin(Uint8)) == "0"
@test bin(typemax(Uint8)) == "1"^8
@test oct(typemin(Uint8)) == "0"
@test oct(typemax(Uint8)) == "377"
@test dec(typemin(Uint8)) == "0"
@test dec(typemax(Uint8)) == "255"
@test hex(typemin(Uint8)) == "0"
@test hex(typemax(Uint8)) == "ff"
@test repr(typemin(Uint8)) == "0x00"
@test string(typemin(Uint8)) == "0"
@test repr(typemax(Uint8)) == "0xff"
@test string(typemax(Uint8)) == "255"
@test base(3,typemin(Uint8)) == "0"
@test base(3,typemax(Uint8)) == "100110"
@test base(12,typemin(Uint8)) == "0"
@test base(12,typemax(Uint8)) == "193"

@test bin(typemin(Uint16)) == "0"
@test bin(typemax(Uint16)) == "1"^16
@test oct(typemin(Uint16)) == "0"
@test oct(typemax(Uint16)) == "177777"
@test dec(typemin(Uint16)) == "0"
@test dec(typemax(Uint16)) == "65535"
@test hex(typemin(Uint16)) == "0"
@test hex(typemax(Uint16)) == "ffff"
@test repr(typemin(Uint16)) == "0x0000"
@test string(typemin(Uint16)) == "0"
@test repr(typemax(Uint16)) == "0xffff"
@test string(typemax(Uint16)) == "65535"
@test base(3,typemin(Uint16)) == "0"
@test base(3,typemax(Uint16)) == "10022220020"
@test base(12,typemin(Uint16)) == "0"
@test base(12,typemax(Uint16)) == "31b13"

@test bin(typemin(Uint32)) == "0"
@test bin(typemax(Uint32)) == "1"^32
@test oct(typemin(Uint32)) == "0"
@test oct(typemax(Uint32)) == "37777777777"
@test dec(typemin(Uint32)) == "0"
@test dec(typemax(Uint32)) == "4294967295"
@test hex(typemin(Uint32)) == "0"
@test hex(typemax(Uint32)) == "ffffffff"
@test repr(typemin(Uint32)) == "0x00000000"
@test string(typemin(Uint32)) == "0"
@test repr(typemax(Uint32)) == "0xffffffff"
@test string(typemax(Uint32)) == "4294967295"
@test base(3,typemin(Uint32)) == "0"
@test base(3,typemax(Uint32)) == "102002022201221111210"
@test base(12,typemin(Uint32)) == "0"
@test base(12,typemax(Uint32)) == "9ba461593"

@test bin(typemin(Uint64)) == "0"
@test bin(typemax(Uint64)) == "1"^64
@test oct(typemin(Uint64)) == "0"
@test oct(typemax(Uint64)) == "1777777777777777777777"
@test dec(typemin(Uint64)) == "0"
@test dec(typemax(Uint64)) == "18446744073709551615"
@test hex(typemin(Uint64)) == "0"
@test hex(typemax(Uint64)) == "ffffffffffffffff"
@test repr(typemin(Uint64)) == "0x0000000000000000"
@test string(typemin(Uint64)) == "0"
@test repr(typemax(Uint64)) == "0xffffffffffffffff"
@test string(typemax(Uint64)) == "18446744073709551615"
@test base(3,typemin(Uint64)) == "0"
@test base(3,typemax(Uint64)) == "11112220022122120101211020120210210211220"
@test base(12,typemin(Uint64)) == "0"
@test base(12,typemax(Uint64)) == "839365134a2a240713"

@test bin(typemin(Uint128)) == "0"
@test bin(typemax(Uint128)) == "1"^128
@test oct(typemin(Uint128)) == "0"
@test oct(typemax(Uint128)) == "3777777777777777777777777777777777777777777"
@test hex(typemin(Uint128)) == "0"
@test hex(typemax(Uint128)) == "ffffffffffffffffffffffffffffffff"
@test repr(typemin(Uint128)) == "0x00000000000000000000000000000000"
@test string(typemin(Uint128)) == "0"
@test repr(typemax(Uint128)) == "0xffffffffffffffffffffffffffffffff"
@test string(typemax(Uint128)) == "340282366920938463463374607431768211455"

@test dec(typemin(Uint128)) == "0"
@test dec(typemax(Uint128)) == "340282366920938463463374607431768211455"
@test base(3,typemin(Uint128)) == "0"
@test base(3,typemax(Uint128)) ==
    "202201102121002021012000211012011021221022212021111001022110211020010021100121010"
@test base(12,typemin(Uint128)) == "0"
@test base(12,typemax(Uint128)) == "5916b64b41143526a777873841863a6a6993"

@test bin(typemin(Int8)) == "-1"*"0"^7
@test bin(typemax(Int8)) == "1"^7
@test oct(typemin(Int8)) == "-200"
@test oct(typemax(Int8)) == "177"
@test dec(typemin(Int8)) == "-128"
@test dec(typemax(Int8)) == "127"
@test hex(typemin(Int8)) == "-80"
@test hex(typemax(Int8)) == "7f"
@test string(typemin(Int8)) == "-128"
@test string(typemax(Int8)) == "127"
@test base(3,typemin(Int8)) == "-11202"
@test base(3,typemax(Int8)) == "11201"
@test base(12,typemin(Int8)) == "-a8"
@test base(12,typemax(Int8)) == "a7"

@test bin(typemin(Int16)) == "-1"*"0"^15
@test bin(typemax(Int16)) == "1"^15
@test oct(typemin(Int16)) == "-100000"
@test oct(typemax(Int16)) == "77777"
@test dec(typemin(Int16)) == "-32768"
@test dec(typemax(Int16)) == "32767"
@test hex(typemin(Int16)) == "-8000"
@test hex(typemax(Int16)) == "7fff"
@test string(typemin(Int16)) == "-32768"
@test string(typemax(Int16)) == "32767"
@test base(3,typemin(Int16)) == "-1122221122"
@test base(3,typemax(Int16)) == "1122221121"
@test base(12,typemin(Int16)) == "-16b68"
@test base(12,typemax(Int16)) == "16b67"

@test bin(typemin(Int32)) == "-1"*"0"^31
@test bin(typemax(Int32)) == "1"^31
@test oct(typemin(Int32)) == "-20000000000"
@test oct(typemax(Int32)) == "17777777777"
@test dec(typemin(Int32)) == "-2147483648"
@test dec(typemax(Int32)) == "2147483647"
@test hex(typemin(Int32)) == "-80000000"
@test hex(typemax(Int32)) == "7fffffff"
@test string(typemin(Int32)) == "-2147483648"
@test string(typemax(Int32)) == "2147483647"
@test base(3,typemin(Int32)) == "-12112122212110202102"
@test base(3,typemax(Int32)) == "12112122212110202101"
@test base(12,typemin(Int32)) == "-4bb2308a8"
@test base(12,typemax(Int32)) == "4bb2308a7"

@test bin(typemin(Int64)) == "-1"*"0"^63
@test bin(typemax(Int64)) == "1"^63
@test oct(typemin(Int64)) == "-1000000000000000000000"
@test oct(typemax(Int64)) == "777777777777777777777"
@test dec(typemin(Int64)) == "-9223372036854775808"
@test dec(typemax(Int64)) == "9223372036854775807"
@test hex(typemin(Int64)) == "-8000000000000000"
@test hex(typemax(Int64)) == "7fffffffffffffff"
@test string(typemin(Int64)) == "-9223372036854775808"
@test string(typemax(Int64)) == "9223372036854775807"
@test base(3,typemin(Int64)) == "-2021110011022210012102010021220101220222"
@test base(3,typemax(Int64)) == "2021110011022210012102010021220101220221"
@test base(12,typemin(Int64)) == "-41a792678515120368"
@test base(12,typemax(Int64)) == "41a792678515120367"

@test bin(typemin(Int128)) == "-1"*"0"^127
@test bin(typemax(Int128)) == "1"^127
@test oct(typemin(Int128)) == "-2000000000000000000000000000000000000000000"
@test oct(typemax(Int128)) == "1777777777777777777777777777777777777777777"
@test hex(typemin(Int128)) == "-80000000000000000000000000000000"
@test hex(typemax(Int128)) == "7fffffffffffffffffffffffffffffff"

@test dec(typemin(Int128)) == "-170141183460469231731687303715884105728"
@test dec(typemax(Int128)) == "170141183460469231731687303715884105727"
@test string(typemin(Int128)) == "-170141183460469231731687303715884105728"
@test string(typemax(Int128)) == "170141183460469231731687303715884105727"
@test base(3,typemin(Int128)) ==
    "-101100201022001010121000102002120122110122221010202000122201220121120010200022002"
@test base(3,typemax(Int128)) ==
    "101100201022001010121000102002120122110122221010202000122201220121120010200022001"
@test base(12,typemin(Int128)) == "-2a695925806818735399a37a20a31b3534a8"
@test base(12,typemax(Int128)) == "2a695925806818735399a37a20a31b3534a7"

# floating-point printing
@test repr(1.0) == "1.0"
@test repr(-1.0) == "-1.0"
@test repr(0.0) == "0.0"
@test repr(-0.0) == "-0.0"
@test repr(0.1) == "0.1"
@test repr(0.2) == "0.2"
@test repr(0.3) == "0.3"
@test repr(0.1+0.2) != "0.3"
@test repr(Inf) == "Inf"
@test repr(-Inf) == "-Inf"
@test repr(NaN) == "NaN"
@test repr(-NaN) == "NaN"
@test repr(float64(pi)) == "3.141592653589793"

@test repr(1.0f0) == "1.0f0"
@test repr(-1.0f0) == "-1.0f0"
@test repr(0.0f0) == "0.0f0"
@test repr(-0.0f0) == "-0.0f0"
@test repr(0.1f0) == "0.1f0"
@test repr(0.2f0) == "0.2f0"
@test repr(0.3f0) == "0.3f0"
@test repr(0.1f0+0.2f0) == "0.3f0"
@test repr(Inf32) == "Inf32"
@test repr(-Inf32) == "-Inf32"
@test repr(NaN32) == "NaN32"
@test repr(-NaN32) == "NaN32"
@test repr(float32(pi)) == "3.1415927f0"

# signs
@test sign(1) == 1
@test sign(-1) == -1
@test sign(0) == 0
@test sign(1.0) == 1
@test sign(-1.0) == -1
@test sign(0.0) == 0
@test sign(-0.0) == 0
@test sign( 1.0/0.0) == 1
@test sign(-1.0/0.0) == -1
@test sign(Inf) == 1
@test sign(-Inf) == -1
@test isequal(sign(NaN), NaN)
@test isequal(sign(-NaN), NaN)
@test sign(2//3) == 1
@test sign(-2//3) == -1
@test sign(0//1) == 0
@test sign(-0//1) == 0
@test sign(1//0) == 1
@test sign(-1//0) == -1

@test signbit(1) == 0
@test signbit(0) == 0
@test signbit(-1) == 1
@test signbit(1.0) == 0
@test signbit(0.0) == 0
@test signbit(-0.0) == 1
@test signbit(-1.0) == 1
@test signbit(1.0/0.0) == 0
@test signbit(-1.0/0.0) == 1
@test signbit(Inf) == 0
@test signbit(-Inf) == 1
@test signbit(NaN) == 0
@test signbit(-NaN) == 1
@test signbit(2//3) == 0
@test signbit(-2//3) == 1
@test signbit(0//1) == 0
@test signbit(-0//1) == 0
@test signbit(1//0) == 0
@test signbit(-1//0) == 1

@test isnan(1)     == false
@test isnan(1.0)   == false
@test isnan(-1.0)  == false
@test isnan(Inf)   == false
@test isnan(-Inf)  == false
@test isnan(NaN)   == true
@test isnan(1//2)  == false
@test isnan(-2//3) == false
@test isnan(5//0)  == false
@test isnan(-3//0) == false

@test isinf(1)     == false
@test isinf(1.0)   == false
@test isinf(-1.0)  == false
@test isinf(Inf)   == true
@test isinf(-Inf)  == true
@test isinf(NaN)   == false
@test isinf(1//2)  == false
@test isinf(-2//3) == false
@test isinf(5//0)  == true
@test isinf(-3//0) == true

@test isfinite(1)     == true
@test isfinite(1.0)   == true
@test isfinite(-1.0)  == true
@test isfinite(Inf)   == false
@test isfinite(-Inf)  == false
@test isfinite(NaN)   == false
@test isfinite(1//2)  == true
@test isfinite(-2//3) == true
@test isfinite(5//0)  == false
@test isfinite(-3//0) == false

@test isequal(-Inf,-Inf)
@test isequal(-1.0,-1.0)
@test isequal(-0.0,-0.0)
@test isequal(+0.0,+0.0)
@test isequal(+1.0,+1.0)
@test isequal(+Inf,+Inf)
@test isequal(-NaN,-NaN)
@test isequal(-NaN,+NaN)
@test isequal(+NaN,-NaN)
@test isequal(+NaN,+NaN)

@test !isequal(-Inf,+Inf)
@test !isequal(-1.0,+1.0)
@test !isequal(-0.0,+0.0)
@test !isequal(+0.0,-0.0)
@test !isequal(+1.0,-1.0)
@test !isequal(+Inf,-Inf)

@test  isequal(-0.0f0,-0.0)
@test  isequal( 0.0f0, 0.0)
@test !isequal(-0.0f0, 0.0)
@test !isequal(0.0f0 ,-0.0)

@test !isless(-Inf,-Inf)
@test  isless(-Inf,-1.0)
@test  isless(-Inf,-0.0)
@test  isless(-Inf,+0.0)
@test  isless(-Inf,+1.0)
@test  isless(-Inf,+Inf)
@test  isless(-Inf,-NaN)
@test  isless(-Inf,+NaN)

@test !isless(-1.0,-Inf)
@test !isless(-1.0,-1.0)
@test  isless(-1.0,-0.0)
@test  isless(-1.0,+0.0)
@test  isless(-1.0,+1.0)
@test  isless(-1.0,+Inf)
@test  isless(-1.0,-NaN)
@test  isless(-1.0,+NaN)

@test !isless(-0.0,-Inf)
@test !isless(-0.0,-1.0)
@test !isless(-0.0,-0.0)
@test  isless(-0.0,+0.0)
@test  isless(-0.0,+1.0)
@test  isless(-0.0,+Inf)
@test  isless(-0.0,-NaN)
@test  isless(-0.0,+NaN)

@test !isless(+0.0,-Inf)
@test !isless(+0.0,-1.0)
@test !isless(+0.0,-0.0)
@test !isless(+0.0,+0.0)
@test  isless(+0.0,+1.0)
@test  isless(+0.0,+Inf)
@test  isless(+0.0,-NaN)
@test  isless(+0.0,+NaN)

@test !isless(+1.0,-Inf)
@test !isless(+1.0,-1.0)
@test !isless(+1.0,-0.0)
@test !isless(+1.0,+0.0)
@test !isless(+1.0,+1.0)
@test  isless(+1.0,+Inf)
@test  isless(+1.0,-NaN)
@test  isless(+1.0,+NaN)

@test !isless(+Inf,-Inf)
@test !isless(+Inf,-1.0)
@test !isless(+Inf,-0.0)
@test !isless(+Inf,+0.0)
@test !isless(+Inf,+1.0)
@test !isless(+Inf,+Inf)
@test  isless(+Inf,-NaN)
@test  isless(+Inf,+NaN)

@test !isless(-NaN,-Inf)
@test !isless(-NaN,-1.0)
@test !isless(-NaN,-0.0)
@test !isless(-NaN,+0.0)
@test !isless(-NaN,+1.0)
@test !isless(-NaN,+Inf)
@test !isless(-NaN,-NaN)
@test !isless(-NaN,+NaN)

@test !isless(+NaN,-Inf)
@test !isless(+NaN,-1.0)
@test !isless(+NaN,-0.0)
@test !isless(+NaN,+0.0)
@test !isless(+NaN,+1.0)
@test !isless(+NaN,+Inf)
@test !isless(+NaN,-NaN)
@test !isless(+NaN,+NaN)

@test  isequal(   0, 0.0)
@test  isequal( 0.0,   0)
@test !isequal(   0,-0.0)
@test !isequal(-0.0,   0)
@test   isless(-0.0,   0)
@test  !isless(   0,-0.0)

@test isless(-0.0, 0.0f0)
@test lexcmp(-0.0, 0.0f0) == -1
@test lexcmp(0.0, -0.0f0) == 1
@test lexcmp(NaN, 1) == 1
@test lexcmp(1, NaN) == -1
@test lexcmp(NaN, NaN) == 0

for x=-5:5, y=-5:5
    @test (x==y)==(float64(x)==int64(y))
    @test (x!=y)==(float64(x)!=int64(y))
    @test (x< y)==(float64(x)< int64(y))
    @test (x> y)==(float64(x)> int64(y))
    @test (x<=y)==(float64(x)<=int64(y))
    @test (x>=y)==(float64(x)>=int64(y))

    @test (x==y)==(int64(x)==float64(y))
    @test (x!=y)==(int64(x)!=float64(y))
    @test (x< y)==(int64(x)< float64(y))
    @test (x> y)==(int64(x)> float64(y))
    @test (x<=y)==(int64(x)<=float64(y))
    @test (x>=y)==(int64(x)>=float64(y))

    if x >= 0
        @test (x==y)==(uint64(x)==float64(y))
        @test (x!=y)==(uint64(x)!=float64(y))
        @test (x< y)==(uint64(x)< float64(y))
        @test (x> y)==(uint64(x)> float64(y))
        @test (x<=y)==(uint64(x)<=float64(y))
        @test (x>=y)==(uint64(x)>=float64(y))
    end
    if y >= 0
        @test (x==y)==(float64(x)==uint64(y))
        @test (x!=y)==(float64(x)!=uint64(y))
        @test (x< y)==(float64(x)< uint64(y))
        @test (x> y)==(float64(x)> uint64(y))
        @test (x<=y)==(float64(x)<=uint64(y))
        @test (x>=y)==(float64(x)>=uint64(y))
    end
end

function _cmp_(x::Union(Int64,Uint64), y::Float64)
    if x==int64(2)^53-2 && y==2.0^53-2; return  0; end
    if x==int64(2)^53-2 && y==2.0^53-1; return -1; end
    if x==int64(2)^53-2 && y==2.0^53  ; return -1; end
    if x==int64(2)^53-2 && y==2.0^53+2; return -1; end
    if x==int64(2)^53-2 && y==2.0^53+3; return -1; end
    if x==int64(2)^53-2 && y==2.0^53+4; return -1; end

    if x==int64(2)^53-1 && y==2.0^53-2; return +1; end
    if x==int64(2)^53-1 && y==2.0^53-1; return  0; end
    if x==int64(2)^53-1 && y==2.0^53  ; return -1; end
    if x==int64(2)^53-1 && y==2.0^53+2; return -1; end
    if x==int64(2)^53-1 && y==2.0^53+3; return -1; end
    if x==int64(2)^53-1 && y==2.0^53+4; return -1; end

    if x==int64(2)^53   && y==2.0^53-2; return +1; end
    if x==int64(2)^53   && y==2.0^53-1; return +1; end
    if x==int64(2)^53   && y==2.0^53  ; return  0; end
    if x==int64(2)^53   && y==2.0^53+2; return -1; end
    if x==int64(2)^53   && y==2.0^53+4; return -1; end

    if x==int64(2)^53+1 && y==2.0^53-2; return +1; end
    if x==int64(2)^53+1 && y==2.0^53-1; return +1; end
    if x==int64(2)^53+1 && y==2.0^53  ; return +1; end
    if x==int64(2)^53+1 && y==2.0^53+2; return -1; end
    if x==int64(2)^53+1 && y==2.0^53+4; return -1; end

    if x==int64(2)^53+2 && y==2.0^53-2; return +1; end
    if x==int64(2)^53+2 && y==2.0^53-1; return +1; end
    if x==int64(2)^53+2 && y==2.0^53  ; return +1; end
    if x==int64(2)^53+2 && y==2.0^53+2; return  0; end
    if x==int64(2)^53+2 && y==2.0^53+4; return -1; end

    if x==int64(2)^53+3 && y==2.0^53-2; return +1; end
    if x==int64(2)^53+3 && y==2.0^53-1; return +1; end
    if x==int64(2)^53+3 && y==2.0^53  ; return +1; end
    if x==int64(2)^53+3 && y==2.0^53+2; return +1; end
    if x==int64(2)^53+3 && y==2.0^53+4; return -1; end

    if x==int64(2)^53+4 && y==2.0^53-2; return +1; end
    if x==int64(2)^53+4 && y==2.0^53-1; return +1; end
    if x==int64(2)^53+4 && y==2.0^53  ; return +1; end
    if x==int64(2)^53+4 && y==2.0^53+2; return +1; end
    if x==int64(2)^53+4 && y==2.0^53+4; return  0; end

    if x==int64(2)^53+5 && y==2.0^53-2; return +1; end
    if x==int64(2)^53+5 && y==2.0^53-1; return +1; end
    if x==int64(2)^53+5 && y==2.0^53  ; return +1; end
    if x==int64(2)^53+5 && y==2.0^53+2; return +1; end
    if x==int64(2)^53+5 && y==2.0^53+4; return +1; end

    error("invalid: _cmp_($x,$y)")
end

for x=int64(2)^53-2:int64(2)^53+5,
    y=[2.0^53-2 2.0^53-1 2.0^53 2.0^53+2 2.0^53+4]
    u = uint64(x)
    if WORD_SIZE == 64
        @test y == float64(itrunc(y))
    else
        @test y == float64(int64(trunc(y)))
    end

    @test (x==y)==(y==x)
    @test (x!=y)==!(x==y)
    @test (-x==-y)==(-y==-x)
    @test (-x!=-y)==!(-x==-y)

    @test (x<y)==(x<=y)&(x!=y)
    @test (x<=y)==(x<y)|(x==y)
    @test (x==y)==(x<=y)&!(x<y)

    @test -x != x
    @test -y != y
    @test -x != y
    @test -y != x
    @test -x <  x
    @test -y <  y
    @test -x <  y
    @test -y <  x
    @test -x <= x
    @test -y <= y
    @test -x <= y
    @test -y <= x

    @test -y != u
    @test -y <  u
    @test -y <= u

    c = _cmp_(x,y)
    if c < 0
        @test !(x == y)
        @test  (x <  y)
        @test !(y <  x)
        @test  (x <= y)
        @test !(y <= x)

        @test !(u == y)
        @test  (u <  y)
        @test !(y <  u)
        @test  (u <= y)
        @test !(y <= u)

        @test !(-x == -y)
        @test !(-x <  -y)
        @test  (-y <  -x)
        @test !(-x <= -y)
        @test  (-y <= -x)
    elseif c > 0
        @test !(x == y)
        @test !(x <  y)
        @test  (y <  x)
        @test !(x <= y)
        @test  (y <= x)

        @test !(u == y)
        @test !(u <  y)
        @test  (y <  u)
        @test !(u <= y)
        @test  (y <= u)

        @test !(-x == -y)
        @test  (-x <  -y)
        @test !(-y <  -x)
        @test  (-x <= -y)
        @test !(-y <= -x)
    else
        @test  (x == y)
        @test !(x <  y)
        @test !(y <  x)
        @test  (x <= y)
        @test  (y <= x)

        @test  (u == y)
        @test !(u <  y)
        @test !(y <  u)
        @test  (u <= y)
        @test  (y <= u)

        @test  (-x == -y)
        @test !(-x <  -y)
        @test !(-y <  -x)
        @test  (-x <= -y)
        @test  (-y <= -x)
    end
end

@test int64(2)^62-1 != 2.0^62
@test int64(2)^62   == 2.0^62
@test int64(2)^62+1 != 2.0^62
@test 2.0^62 != int64(2)^62-1
@test 2.0^62 == int64(2)^62
@test 2.0^62 != int64(2)^62+1

@test typemax(Int64)   != +2.0^63
@test typemin(Int64)   == -2.0^63
@test typemin(Int64)+1 != -2.0^63

@test uint64(2)^60-1 != 2.0^60
@test uint64(2)^60   == 2.0^60
@test uint64(2)^60+1 != 2.0^60
@test 2.0^60 != uint64(2)^60-1
@test 2.0^60 == uint64(2)^60
@test 2.0^60 != uint64(2)^60+1

@test uint64(2)^63-1 != 2.0^63
@test uint64(2)^63   == 2.0^63
@test uint64(2)^63+1 != 2.0^63
@test 2.0^63 != uint64(2)^63-1
@test 2.0^63 == uint64(2)^63
@test 2.0^63 != uint64(2)^63+1

@test typemax(Uint64) != 2.0^64

@test typemax(Uint64) < float64(typemax(Uint64))
@test typemax(Int64) < float64(typemax(Int64))
@test typemax(Uint64) <= float64(typemax(Uint64))
@test typemax(Int64) <= float64(typemax(Int64))

@test float64(typemax(Uint64)) > typemax(Uint64)
@test float64(typemax(Int64)) > typemax(Int64)
@test float64(typemax(Uint64)) >= typemax(Uint64)
@test float64(typemax(Int64)) >= typemax(Int64)

@test float64(int128(0)) == 0.0
@test float32(int128(0)) == 0.0f0
@test float64(int128(-1)) == -1.0
@test float32(int128(-1)) == -1.0f0
@test float64(int128(3)) == 3.0
@test float32(int128(3)) == 3.0f0
@test float64(uint128(10121)) == 10121.0
@test float32(uint128(10121)) == 10121.0f0
@test float64(typemin(Int128)) == -2.0^127
@test float32(typemin(Int128)) == -2.0f0^127
@test float64(typemax(Int128)) == 2.0^127
@test float32(typemax(Int128)) == 2.0f0^127
@test float64(typemin(Uint128)) == 0.0
@test float32(typemin(Uint128)) == 0.0f0
@test float64(typemax(Uint128)) == 2.0^128
@test float32(typemax(Uint128)) == 2.0f0^128

@test int128(-2.0^127) == typemin(Int128)
@test float64(uint128(3.7e19)) == 3.7e19
@test float64(uint128(3.7e30)) == 3.7e30

@test !(NaN <= 1)
@test !(NaN >= 1)
@test !(NaN < 1)
@test !(NaN > 1)
@test !(1 <= NaN)
@test !(1 >= NaN)
@test !(1 < NaN)
@test !(1 > NaN)

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

for a = -5:5, b = -5:5
    if a == b == 0; continue; end
    if ispow2(b)
        @test a//b == a/b
        @test convert(Rational,a/b) == a//b
    end
    @test rationalize(a/b) == a//b
    @test a//b == a//b
    if b == 0
        @test_throws DivideError integer(a//b) == integer(a/b)
    else
        @test integer(a//b) == integer(a/b)
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
@test realmin() != 1//(BigInt(2)^1022+1)
@test realmin() == 1//(BigInt(2)^1022)
@test realmin() != 1//(BigInt(2)^1022-1)
@test realmin()/2 != 1//(BigInt(2)^1023+1)
@test realmin()/2 == 1//(BigInt(2)^1023)
@test realmin()/2 != 1//(BigInt(2)^1023-1)
@test nextfloat(0.0) != 1//(BigInt(2)^1074+1)
@test nextfloat(0.0) == 1//(BigInt(2)^1074)
@test nextfloat(0.0) != 1//(BigInt(2)^1074-1)

@test sqrt(2) == 1.4142135623730951

@test 1+1.5 == 2.5
@test 1.5+1 == 2.5
@test 1+1.5+2 == 4.5
@test is(typeof(convert(Complex{Int16},1)),Complex{Int16})
@test Complex(1,2)+1 == Complex(2,2)
@test Complex(1,2)+1.5 == Complex(2.5,2.0)
@test 1/Complex(2,2) == Complex(.25,-.25)
@test Complex(1.5,1.0) + 1//2 == Complex(2.0,1.0)
@test real(Complex(1//2,2//3)) == 1//2
@test imag(Complex(1//2,2//3)) == 2//3
@test Complex(1,2) + 1//2 == Complex(3//2,2//1)
@test Complex(1,2) + 1//2 * 0.5 == Complex(1.25,2.0)
@test (Complex(1,2) + 1//2) * 0.5 == Complex(0.75,1.0)
@test (Complex(1,2)/Complex(2.5,3.0))*Complex(2.5,3.0) == Complex(1,2)
@test 0.7 < real(sqrt(Complex(0,1))) < 0.707107

for T in {Int8,Int16,Int32,Int64,Int128}
    @test abs(typemin(T)) == -typemin(T)
    for x in {typemin(T),convert(T,-1),zero(T),one(T),typemax(T)}
        @test signed(unsigned(x)) == x
    end
end

for T in {Uint8,Uint16,Uint32,Uint64,Uint128},
    x in {typemin(T),one(T),typemax(T)}
    @test unsigned(signed(x)) == x
end

for S = {Int8,  Int16,  Int32,  Int64},
    U = {Uint8, Uint16, Uint32, Uint64}
    @test !(-one(S) == typemax(U))
    @test -one(S) != typemax(U)
    @test -one(S) < typemax(U)
    @test !(typemax(U) <= -one(S))
end

# check type of constructed rationals
int_types = {Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64}
for N = int_types, D = int_types
    T = promote_type(N,D)
    @test typeof(convert(N,2)//convert(D,3)) <: Rational{T}
end

# issue #7564
@test typeof(convert(Rational{Integer},1)) === Rational{Integer}

# check type of constructed complexes
real_types = {Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64, Float32, Float64,
              Rational{Int8}, Rational{Uint8}, Rational{Int16}, Rational{Uint16},
              Rational{Int32}, Rational{Uint32}, Rational{Int64}, Rational{Uint64}}
for A = real_types, B = real_types
    T = promote_type(A,B)
    @test typeof(Complex(convert(A,2),convert(B,3))) <: Complex{T}
end

# comparison should fail on complex
@test_throws MethodError complex(1,2) > 0
@test_throws MethodError complex(1,2) > complex(0,0)

# div, fld, rem, mod
for yr = {
    1:6,
    0.25:0.25:6.0,
    1//4:1//4:6//1
}, xr = {
    0:6,
    0.0:0.25:6.0,
    0//1:1//4:6//1
}
    for y = yr, x = xr
        # check basic div functionality
        if 0 <= x < 1y
            @test div(+x,+y) == 0
            @test div(+x,-y) == 0
            @test div(-x,+y) == 0
            @test div(-x,-y) == 0
        end
        if 1y <= x < 2y
            @test div(+x,+y) == +1
            @test div(+x,-y) == -1
            @test div(-x,+y) == -1
            @test div(-x,-y) == +1
        end
        if 2y <= x < 3y
            @test div(+x,+y) == +2
            @test div(+x,-y) == -2
            @test div(-x,+y) == -2
            @test div(-x,-y) == +2
        end

        # check basic fld functionality
        if 0 == x
            @test fld(+x,+y) == 0
            @test fld(+x,-y) == 0
            @test fld(-x,+y) == 0
            @test fld(-x,-y) == 0
        end
        if 0 < x < 1y
            @test fld(+x,+y) == +0
            @test fld(+x,-y) == -1
            @test fld(-x,+y) == -1
            @test fld(-x,-y) == +0
        end
        if 1y == x
            @test fld(+x,+y) == +1
            @test fld(+x,-y) == -1
            @test fld(-x,+y) == -1
            @test fld(-x,-y) == +1
        end
        if 1y < x < 2y
            @test fld(+x,+y) == +1
            @test fld(+x,-y) == -2
            @test fld(-x,+y) == -2
            @test fld(-x,-y) == +1
        end
        if 2y == x
            @test fld(+x,+y) == +2
            @test fld(+x,-y) == -2
            @test fld(-x,+y) == -2
            @test fld(-x,-y) == +2
        end
        if 2y < x < 3y
            @test fld(+x,+y) == +2
            @test fld(+x,-y) == -3
            @test fld(-x,+y) == -3
            @test fld(-x,-y) == +2
        end

        # check everything else in terms of div & fld
        d = div(x,y)
        f = fld(x,y)
        r = rem(x,y)
        m = mod(x,y)

        t1 = isa(x,Rational) && isa(y,Rational) ?
                               promote_type(typeof(num(x)),typeof(num(y))) :
             isa(x,Rational) ? promote_type(typeof(num(x)),typeof(y)) :
             isa(y,Rational) ? promote_type(typeof(x),typeof(num(y))) :
                               promote_type(typeof(x),typeof(y))

        t2 = promote_type(typeof(x),typeof(y))

        @test typeof(d) <: t1
        @test typeof(f) <: t1
        @test typeof(r) <: t2
        @test typeof(m) <: t2

        @test d == f
        @test r == m
        @test 0 <= r < y
        @test x == y*d + r

        for X=[-1,1], Y=[-1,1]
            sx = X*x
            sy = Y*y

            sd = div(sx,sy)
            sf = fld(sx,sy)
            sr = rem(sx,sy)
            sm = mod(sx,sy)

            @test typeof(sd) <: t1
            @test typeof(sf) <: t1
            @test typeof(sr) <: t2
            @test typeof(sm) <: t2

            @test sx < 0 ? -y < sr <= 0 : 0 <= sr < +y
            @test sy < 0 ? -y < sm <= 0 : 0 <= sm < +y
            @test sx == sy*sd + sr
            @test sx == sy*sf + sm
        end
    end
end

@test div(typemax(Int64)  , 1) ==  9223372036854775807
@test div(typemax(Int64)  , 2) ==  4611686018427387903
@test div(typemax(Int64)  , 7) ==  1317624576693539401
@test div(typemax(Int64)  ,-1) == -9223372036854775807
@test div(typemax(Int64)  ,-2) == -4611686018427387903
@test div(typemax(Int64)  ,-7) == -1317624576693539401
@test div(typemax(Int64)-1, 1) ==  9223372036854775806
@test div(typemax(Int64)-1, 2) ==  4611686018427387903
@test div(typemax(Int64)-1, 7) ==  1317624576693539400
@test div(typemax(Int64)-1,-1) == -9223372036854775806
@test div(typemax(Int64)-1,-2) == -4611686018427387903
@test div(typemax(Int64)-1,-7) == -1317624576693539400
@test div(typemax(Int64)-2, 1) ==  9223372036854775805
@test div(typemax(Int64)-2, 2) ==  4611686018427387902
@test div(typemax(Int64)-2, 7) ==  1317624576693539400
@test div(typemax(Int64)-2,-1) == -9223372036854775805
@test div(typemax(Int64)-2,-2) == -4611686018427387902
@test div(typemax(Int64)-2,-7) == -1317624576693539400

@test div(typemin(Int64)  , 1) == -9223372036854775807-1
@test div(typemin(Int64)  , 2) == -4611686018427387904
@test div(typemin(Int64)  , 7) == -1317624576693539401
#@test div(typemin(Int64)  ,-1) == -9223372036854775807-1 # FIXME!
@test div(typemin(Int64)  ,-2) ==  4611686018427387904
@test div(typemin(Int64)  ,-7) ==  1317624576693539401
@test div(typemin(Int64)+1, 1) == -9223372036854775807
@test div(typemin(Int64)+1, 2) == -4611686018427387903
@test div(typemin(Int64)+1, 7) == -1317624576693539401
@test div(typemin(Int64)+1,-1) ==  9223372036854775807
@test div(typemin(Int64)+1,-2) ==  4611686018427387903
@test div(typemin(Int64)+1,-7) ==  1317624576693539401
@test div(typemin(Int64)+2, 1) == -9223372036854775806
@test div(typemin(Int64)+2, 2) == -4611686018427387903
@test div(typemin(Int64)+2, 7) == -1317624576693539400
@test div(typemin(Int64)+2,-1) ==  9223372036854775806
@test div(typemin(Int64)+2,-2) ==  4611686018427387903
@test div(typemin(Int64)+2,-7) ==  1317624576693539400
@test div(typemin(Int64)+3, 1) == -9223372036854775805
@test div(typemin(Int64)+3, 2) == -4611686018427387902
@test div(typemin(Int64)+3, 7) == -1317624576693539400
@test div(typemin(Int64)+3,-1) ==  9223372036854775805
@test div(typemin(Int64)+3,-2) ==  4611686018427387902
@test div(typemin(Int64)+3,-7) ==  1317624576693539400

@test fld(typemax(Int64)  , 1) ==  9223372036854775807
@test fld(typemax(Int64)  , 2) ==  4611686018427387903
@test fld(typemax(Int64)  , 7) ==  1317624576693539401
@test fld(typemax(Int64)  ,-1) == -9223372036854775807
@test fld(typemax(Int64)  ,-2) == -4611686018427387904
@test fld(typemax(Int64)  ,-7) == -1317624576693539401
@test fld(typemax(Int64)-1, 1) ==  9223372036854775806
@test fld(typemax(Int64)-1, 2) ==  4611686018427387903
@test fld(typemax(Int64)-1, 7) ==  1317624576693539400
@test fld(typemax(Int64)-1,-1) == -9223372036854775806
@test fld(typemax(Int64)-1,-2) == -4611686018427387903
@test fld(typemax(Int64)-1,-7) == -1317624576693539401
@test fld(typemax(Int64)-2, 1) ==  9223372036854775805
@test fld(typemax(Int64)-2, 2) ==  4611686018427387902
@test fld(typemax(Int64)-2, 7) ==  1317624576693539400
@test fld(typemax(Int64)-2,-1) == -9223372036854775805
@test fld(typemax(Int64)-2,-2) == -4611686018427387903
@test fld(typemax(Int64)-2,-7) == -1317624576693539401

@test fld(typemin(Int64)  , 1) == -9223372036854775807-1
@test fld(typemin(Int64)  , 2) == -4611686018427387904
@test fld(typemin(Int64)  , 7) == -1317624576693539402
#@test fld(typemin(Int64)  ,-1) == -9223372036854775807-1 # FIXME!
@test fld(typemin(Int64)  ,-2) ==  4611686018427387904
@test fld(typemin(Int64)  ,-7) ==  1317624576693539401
@test fld(typemin(Int64)+1, 1) == -9223372036854775807
@test fld(typemin(Int64)+1, 2) == -4611686018427387904
@test fld(typemin(Int64)+1, 7) == -1317624576693539401
@test fld(typemin(Int64)+1,-1) ==  9223372036854775807
@test fld(typemin(Int64)+1,-2) ==  4611686018427387903
@test fld(typemin(Int64)+1,-7) ==  1317624576693539401
@test fld(typemin(Int64)+2, 1) == -9223372036854775806
@test fld(typemin(Int64)+2, 2) == -4611686018427387903
@test fld(typemin(Int64)+2, 7) == -1317624576693539401
@test fld(typemin(Int64)+2,-1) ==  9223372036854775806
@test fld(typemin(Int64)+2,-2) ==  4611686018427387903
@test fld(typemin(Int64)+2,-7) ==  1317624576693539400
@test fld(typemin(Int64)+3, 1) == -9223372036854775805
@test fld(typemin(Int64)+3, 2) == -4611686018427387903
@test fld(typemin(Int64)+3, 7) == -1317624576693539401
@test fld(typemin(Int64)+3,-1) ==  9223372036854775805
@test fld(typemin(Int64)+3,-2) ==  4611686018427387902
@test fld(typemin(Int64)+3,-7) ==  1317624576693539400

for x={typemin(Int64), -typemax(Int64), -typemax(Int64)+1, -typemax(Int64)+2,
       typemax(Int64)-2, typemax(Int64)-1, typemax(Int64),
       typemax(Uint64)-1, typemax(Uint64)-2, typemax(Uint64)},
    y={-7,-2,-1,1,2,7}
    if x >= 0
        @test div(unsigned(x),y) == unsigned(div(x,y))
        @test fld(unsigned(x),y) == unsigned(fld(x,y))
    end
    if isa(x,Signed) && y >= 0
        @test div(x,unsigned(y)) == div(x,y)
        @test fld(x,unsigned(y)) == fld(x,y)
    end
end

for x=0:5, y=1:5
    @test div(uint(x),uint(y)) == div(x,y)
    @test div(uint(x),y) == div(x,y)
    @test div(x,uint(y)) == div(x,y)
    @test div(uint(x),-y) == uint(div(x,-y))
    @test div(-x,uint(y)) == div(-x,y)

    @test fld(uint(x),uint(y)) == fld(x,y)
    @test fld(uint(x),y) == fld(x,y)
    @test fld(x,uint(y)) == fld(x,y)
    @test fld(uint(x),-y) == uint(fld(x,-y))
    @test fld(-x,uint(y)) == fld(-x,y)

    @test rem(uint(x),uint(y)) == rem(x,y)
    @test rem(uint(x),y) == rem(x,y)
    @test rem(x,uint(y)) == rem(x,y)
    @test rem(uint(x),-y) == rem(x,-y)
    @test rem(-x,uint(y)) == rem(-x,y)

    @test mod(uint(x),uint(y)) == mod(x,y)
    @test mod(uint(x),y) == mod(x,y)
    @test mod(x,uint(y)) == mod(x,y)
    @test mod(uint(x),-y) == mod(x,-y)
    @test mod(-x,uint(y)) == mod(-x,y)
end

@test div(typemax(Uint64)  , 1) ==  typemax(Uint64)
@test div(typemax(Uint64)  ,-1) == -typemax(Uint64)
@test div(typemax(Uint64)-1, 1) ==  typemax(Uint64)-1
@test div(typemax(Uint64)-1,-1) == -typemax(Uint64)+1
@test div(typemax(Uint64)-2, 1) ==  typemax(Uint64)-2
@test div(typemax(Uint64)-2,-1) == -typemax(Uint64)+2

@test signed(div(unsigned(typemax(Int64))+2, 1)) ==  typemax(Int64)+2
@test signed(div(unsigned(typemax(Int64))+2,-1)) == -typemax(Int64)-2
@test signed(div(unsigned(typemax(Int64))+1, 1)) ==  typemax(Int64)+1
@test signed(div(unsigned(typemax(Int64))+1,-1)) == -typemax(Int64)-1
@test signed(div(unsigned(typemax(Int64))  , 1)) ==  typemax(Int64)
@test signed(div(unsigned(typemax(Int64))  ,-1)) == -typemax(Int64)

@test signed(div(typemax(Uint),typemax(Int)))        ==  2
@test signed(div(typemax(Uint),(typemax(Int)>>1)+1)) ==  3
@test signed(div(typemax(Uint),typemax(Int)>>1))     ==  4
@test signed(div(typemax(Uint),typemin(Int)))        == -1
@test signed(div(typemax(Uint),typemin(Int)+1))      == -2
@test signed(div(typemax(Uint),typemin(Int)>>1))     == -3
@test signed(div(typemax(Uint),(typemin(Int)>>1)+1)) == -4

@test fld(typemax(Uint64)  , 1) ==  typemax(Uint64)
@test fld(typemax(Uint64)  ,-1) == -typemax(Uint64)
@test fld(typemax(Uint64)-1, 1) ==  typemax(Uint64)-1
@test fld(typemax(Uint64)-1,-1) == -typemax(Uint64)+1
@test fld(typemax(Uint64)-2, 1) ==  typemax(Uint64)-2
@test fld(typemax(Uint64)-2,-1) == -typemax(Uint64)+2

@test signed(fld(unsigned(typemax(Int64))+2, 1)) ==  typemax(Int64)+2
@test signed(fld(unsigned(typemax(Int64))+2,-1)) == -typemax(Int64)-2
@test signed(fld(unsigned(typemax(Int64))+1, 1)) ==  typemax(Int64)+1
@test signed(fld(unsigned(typemax(Int64))+1,-1)) == -typemax(Int64)-1
@test signed(fld(unsigned(typemax(Int64))  , 1)) ==  typemax(Int64)
@test signed(fld(unsigned(typemax(Int64))  ,-1)) == -typemax(Int64)

@test signed(fld(typemax(Uint),typemax(Int)))        ==  2
@test signed(fld(typemax(Uint),(typemax(Int)>>1)+1)) ==  3
@test signed(fld(typemax(Uint),typemax(Int)>>1))     ==  4
@test signed(fld(typemax(Uint),typemin(Int)))        == -2
@test signed(fld(typemax(Uint),typemin(Int)+1))      == -3
@test signed(fld(typemax(Uint),typemin(Int)>>1))     == -4
@test signed(fld(typemax(Uint),(typemin(Int)>>1)+1)) == -5

# issue #4156
@test fld(1.4,0.35667494393873234) == 3.0
@test div(1.4,0.35667494393873234) == 3.0
@test fld(0.3,0.01) == 29.0
@test div(0.3,0.01) == 29.0
# see https://github.com/JuliaLang/julia/issues/3127

# issue #3046
@test mod(int64(2),typemax(Int64)) == 2

# things related to floating-point epsilon
@test eps() == eps(Float64)
@test eps(Float64) == eps(1.0)
@test eps(Float64) == eps(1.5)
@test eps(Float32) == eps(1f0)
@test eps(float(0)) == 5e-324
@test eps(-float(0)) == 5e-324
@test eps(nextfloat(float(0))) == 5e-324
@test eps(-nextfloat(float(0))) == 5e-324
@test eps(realmin()) == 5e-324
@test eps(-realmin()) == 5e-324
@test eps(realmax()) ==  2.0^(1023-52)
@test eps(-realmax()) ==  2.0^(1023-52)
@test isnan(eps(NaN))
@test isnan(eps(Inf))
@test isnan(eps(-Inf))

@test .1+.1+.1 != .3
# TODO: uncomment when isapprox() becomes part of base.
# @test isapprox(.1+.1+.1, .3)
# @test !isapprox(.1+.1+.1-.3, 0)
# @test isapprox(.1+.1+.1-.3, 0, eps(.3))

@test div(1e50,1) == 1e50
@test fld(1e50,1) == 1e50

# rounding difficult values

for x = 2^53-10:2^53+10
    y = float64(x)
    i = WORD_SIZE == 64 ? itrunc(y) : int64(trunc(y))
    @test int64(trunc(y)) == i
    @test int64(round(y)) == i
    @test int64(floor(y)) == i
    @test int64(ceil(y))  == i
    if WORD_SIZE == 64
        @test iround(y)       == i
        @test ifloor(y)       == i
        @test iceil(y)        == i
    end
end

for x = 2^24-10:2^24+10
    y = float32(x)
    i = itrunc(y)
    @test int(trunc(y)) == i
    @test int(round(y)) == i
    @test int(floor(y)) == i
    @test int(ceil(y))  == i
    @test iround(y)     == i
    @test ifloor(y)     == i
    @test iceil(y)      == i
end

@test_throws InexactError iround(Inf)
@test_throws InexactError iround(NaN)
@test iround(2.5) == 3
@test iround(-1.9) == -2
@test_throws InexactError iround(Int64, 9.223372036854776e18)
@test       iround(Int64, 9.223372036854775e18) == 9223372036854774784
@test_throws InexactError iround(Int64, -9.223372036854778e18)
@test       iround(Int64, -9.223372036854776e18) == typemin(Int64)
@test_throws InexactError iround(Uint64, 1.8446744073709552e19)
@test       iround(Uint64, 1.844674407370955e19) == 0xfffffffffffff800
@test_throws InexactError iround(Int32, 2.1474836f9)
@test       iround(Int32, 2.1474835f9) == 2147483520
@test_throws InexactError iround(Int32, -2.147484f9)
@test       iround(Int32, -2.1474836f9) == typemin(Int32)
@test_throws InexactError iround(Uint32, 4.2949673f9)
@test       iround(Uint32, 4.294967f9) == 0xffffff00

for n = 1:100
    m = 1
    for (p,k) in factor(n)
        m *= p^k
    end
    @test n == m
end

@test iround(Uint,-0.0) == 0
@test iround(Int,-0.0) == 0

@test iround(Int, 0.5) == 1
@test iround(Int, prevfloat(0.5)) == 0
@test iround(Int, -0.5) == -1
@test iround(Int, nextfloat(-0.5)) == 0

@test iround(Uint, 0.5) == 1
@test iround(Uint, prevfloat(0.5)) == 0
@test_throws InexactError iround(Uint, -0.5)
@test iround(Uint, nextfloat(-0.5)) == 0

@test iround(Int, 0.5f0) == 1
@test iround(Int, prevfloat(0.5f0)) == 0
@test iround(Int, -0.5f0) == -1
@test iround(Int, nextfloat(-0.5f0)) == 0

@test iround(Uint, 0.5f0) == 1
@test iround(Uint, prevfloat(0.5f0)) == 0
@test_throws InexactError iround(Uint, -0.5f0)
@test iround(Uint, nextfloat(-0.5f0)) == 0

# numbers that can't be rounded by trunc(x+0.5)
@test iround(Int64, 2.0^52 + 1) == 4503599627370497
@test iround(Int32, 2.0f0^23 + 1) == 8388609

# binary literals

@test 0b1010101 == 0x55
@test isa(0b00000000,Uint8)
@test isa(0b000000000,Uint16)
@test isa(0b0000000000000000,Uint16)
@test isa(0b00000000000000000,Uint32)
@test isa(0b00000000000000000000000000000000,Uint32)
@test isa(0b000000000000000000000000000000000,Uint64)
@test isa(0b0000000000000000000000000000000000000000000000000000000000000000,Uint64)
@test isa(0b00000000000000000000000000000000000000000000000000000000000000000,Uint128)
@test isa(0b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,Uint128)
@test isa(0b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,BigInt)
@test isa(0b11111111,Uint8)
@test isa(0b111111111,Uint16)
@test isa(0b1111111111111111,Uint16)
@test isa(0b11111111111111111,Uint32)
@test isa(0b11111111111111111111111111111111,Uint32)
@test isa(0b111111111111111111111111111111111,Uint64)
@test isa(0b1111111111111111111111111111111111111111111111111111111111111111,Uint64)
@test isa(0b11111111111111111111111111111111111111111111111111111111111111111,Uint128)
@test isa(0b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111,Uint128)
@test isa(0b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111,BigInt)

# octal literals

@test 0o10 == 0x8
@test 0o100 == 0x40
@test 0o1000 == 0x200
@test 0o724 == 0x1d4
@test isa(0o377,Uint8)
@test isa(0o00,Uint8)
@test isa(0o000,Uint16)
@test isa(0o00000,Uint16)
@test isa(0o000000,Uint32)
@test isa(0o0000000000,Uint32)
@test isa(0o00000000000,Uint64)
@test isa(0o000000000000000000000,Uint64)
@test isa(0o0000000000000000000000,Uint128)
@test isa(0o000000000000000000000000000000000000000000,Uint128)
@test isa(0o0000000000000000000000000000000000000000000,BigInt)
@test isa(0o11,Uint8)
@test isa(0o111,Uint8)
@test isa(0o11111,Uint16)
@test isa(0o111111,Uint16)
@test isa(0o1111111111,Uint32)
@test isa(0o11111111111,Uint32)
@test isa(0o111111111111111111111,Uint64)
@test isa(0o1111111111111111111111,Uint64)
@test isa(0o111111111111111111111111111111111111111111,Uint128)
@test isa(0o1111111111111111111111111111111111111111111,Uint128)
@test isa(0o11111111111111111111111111111111111111111111,BigInt)
@test 0o4000000000000000000000000000000000000000000 ==
    340282366920938463463374607431768211456

# hexadecimal literals

@test isa(0x00,Uint8)
@test isa(0x000,Uint16)
@test isa(0x0000,Uint16)
@test isa(0x00000,Uint32)
@test isa(0x00000000,Uint32)
@test isa(0x000000000,Uint64)
@test isa(0x0000000000000000,Uint64)
@test isa(0x00000000000000000,Uint128)
@test isa(0x00000000000000000000000000000000,Uint128)
@test isa(0x000000000000000000000000000000000,BigInt)
@test isa(0x11,Uint8)
@test isa(0x111,Uint16)
@test isa(0x1111,Uint16)
@test isa(0x11111,Uint32)
@test isa(0x11111111,Uint32)
@test isa(0x111111111,Uint64)
@test isa(0x1111111111111111,Uint64)
@test isa(0x11111111111111111,Uint128)
@test isa(0x11111111111111111111111111111111,Uint128)
@test isa(0x111111111111111111111111111111111,BigInt)

# "-" is not part of unsigned literals
@test -0x10 == -(0x10)
@test -0b10 == -(0b10)
@test -0o10 == -(0o10)
@test -0x0010 == -(0x0010)
@test -0b0010 == -(0b0010)
@test -0o0010 == -(0o0010)
@test -0x00000000000000001 == -(0x00000000000000001)
@test -0o0000000000000000000001 == -(0o0000000000000000000001)
@test -0b00000000000000000000000000000000000000000000000000000000000000001 ==
    -(0b00000000000000000000000000000000000000000000000000000000000000001)
@test -0x000000000000000000000000000000001 == -(0x000000000000000000000000000000001)
@test -0o0000000000000000000000000000000000000000001 ==
    -(0o0000000000000000000000000000000000000000001)
@test -0b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001 ==
    -(0b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001)

@test isa(-0x00,Uint)
@test isa(-0x0000000000000000,Uint64)
@test isa(-0x00000000000000000,Uint128)
@test isa(-0x00000000000000000000000000000000,Uint128)
@test isa(-0x000000000000000000000000000000000,BigInt)

# float32 literals
@test isa(1f0,Float32)
@test isa(1.f0,Float32)
@test isa(1.0f0,Float32)
@test 1f0 == 1.
@test isa(1f1,Float32)
@test 1f1 == 10.

# hexadecimal float literals
@test 0x1p0   === 1.
@test 0x1p1   === 2.
@test 0x.1p0  === 0.0625
@test 0x.1p1  === 0.125
@test 0xfp0   === 15.
@test 0xfp1   === 30.
@test 0x.fp0  === 0.9375
@test 0x.fp1  === 1.875
@test 0x1.p0  === 1.
@test 0x1.p1  === 2.
@test 0xf.p0  === 15.
@test 0xf.p1  === 30.
@test 0x1.0p0 === 1.
@test 0x1.0p1 === 2.
@test 0x1.1p0 === 1.0625
@test 0x1.1p1 === 2.125
@test 0x1.fp0 === 1.9375
@test 0x1.fp1 === 3.875
@test 0xf.0p0 === 15.
@test 0xf.0p1 === 30.
@test 0xf.1p0 === 15.0625
@test 0xf.1p1 === 30.125
@test 0xf.fp0 === 15.9375
@test 0xf.fp1 === 31.875
@test 0x1P0   === 1.
@test 0x1P1   === 2.
@test 0x.1P0  === 0.0625
@test 0x.1P1  === 0.125
@test 0xfP0   === 15.
@test 0xfP1   === 30.
@test 0x.fP0  === 0.9375
@test 0x.fP1  === 1.875
@test 0x1.P0  === 1.
@test 0x1.P1  === 2.
@test 0xf.P0  === 15.
@test 0xf.P1  === 30.
@test 0x1.0P0 === 1.
@test 0x1.0P1 === 2.
@test 0x1.1P0 === 1.0625
@test 0x1.1P1 === 2.125
@test 0x1.fP0 === 1.9375
@test 0x1.fP1 === 3.875
@test 0xf.0P0 === 15.
@test 0xf.0P1 === 30.
@test 0xf.1P0 === 15.0625
@test 0xf.1P1 === 30.125
@test 0xf.fP0 === 15.9375
@test 0xf.fP1 === 31.875

@test -0x1.0p2 === -4.0

# eps / realmin / realmax
@test 0x1p-52 == eps()
@test 0x1p-52 + 1 != 1
@test 0x1p-53 + 1 == 1
@test 0x1p-1022 == realmin()
@test 0x1.fffffffffffffp1023 == realmax()
@test isinf(nextfloat(0x1.fffffffffffffp1023))

# custom rounding and significant-digit ops
function approx_eq(a, b, tol)
    abs(a - b) < tol
end
approx_eq(a, b) = approx_eq(a, b, 1e-6)
# rounding to digits relative to the decimal point
@test approx_eq(round(pi,0), 3.)
@test approx_eq(round(pi,1), 3.1)
@test approx_eq(round(10*pi,-1), 30.)
@test round(.1,0) == 0.
@test round(-.1,0) == -0.
@test isnan(round(NaN, 2))
@test isinf(round(Inf,2))
@test isinf(round(-Inf,2))
# round vs trunc vs floor vs ceil
@test approx_eq(round(123.456,1), 123.5)
@test approx_eq(round(-123.456,1), -123.5)
@test approx_eq(trunc(123.456,1), 123.4)
@test approx_eq(trunc(-123.456,1), -123.4)
@test approx_eq(ceil(123.456,1), 123.5)
@test approx_eq(ceil(-123.456,1), -123.4)
@test approx_eq(floor(123.456,1), 123.4)
@test approx_eq(floor(-123.456,1), -123.5)
# rounding in other bases
@test approx_eq(round(pi,2,2), 3.25)
@test approx_eq(round(pi,3,2), 3.125)
@test approx_eq(round(pi,3,5), 3.144)
# significant digits (would be nice to have a smart vectorized
# version of signif)
@test approx_eq(signif(123.456,1), 100.)
@test approx_eq(signif(123.456,3), 123.)
@test approx_eq(signif(123.456,5), 123.46)
@test approx_eq(signif(123.456,8,2), 123.5)
@test signif(0.0, 1) === 0.0
@test signif(-0.0, 1) === -0.0

# issue #1308
@test hex(~uint128(0)) == "f"^32
@test uint128(~0) == ~uint128(0)
@test int128(~0) == ~int128(0)

# issue 1552
@test isa(rationalize(Int8, float(pi)), Rational{Int8})
@test rationalize(Int8, float(pi)) == 22//7
@test rationalize(Int64, 0.957762604052997) == 42499549//44373782
@test rationalize(Int16, 0.929261477046077) == 11639//12525
@test rationalize(Int16, 0.2264705884044309) == 77//340
@test rationalize(Int16, 0.39999899264235683) == 2//5
@test rationalize(Int16, 1.1264233500618559e-5) == 0//1
@test rationalize(Uint16, 1.1264233500618559e-5) == 1//65535
@test rationalize(Uint16, 0.6666652791223875) == 2//3
@test rationalize(Int8, 0.9374813124660655) == 15//16
@test rationalize(Int8, 0.003803032342443835) == 0//1
@test rationalize(Uint8, 0.003803032342443835) == 1//255

# issue 3412
@test convert(Rational{Int32},0.5) === int32(1)//int32(2)

# issue 6712
@test convert(Rational{BigInt},float64(pi)) == float64(pi)
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
@test convert(Rational{BigInt},realmin(Float64)) == realmin(Float64)
@test convert(Rational{BigInt},realmax(Float64)) == realmax(Float64)

@test isa(convert(Float64, big(1)//2), Float64)

# primes

@test Base.primes(10000) == [
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
    73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151,
    157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233,
    239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317,
    331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419,
    421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503,
    509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607,
    613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701,
    709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811,
    821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911,
    919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997, 1009, 1013,
    1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, 1087, 1091,
    1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181,
    1187, 1193, 1201, 1213, 1217, 1223, 1229, 1231, 1237, 1249, 1259, 1277,
    1279, 1283, 1289, 1291, 1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361,
    1367, 1373, 1381, 1399, 1409, 1423, 1427, 1429, 1433, 1439, 1447, 1451,
    1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511, 1523, 1531,
    1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597, 1601, 1607, 1609,
    1613, 1619, 1621, 1627, 1637, 1657, 1663, 1667, 1669, 1693, 1697, 1699,
    1709, 1721, 1723, 1733, 1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789,
    1801, 1811, 1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877, 1879, 1889,
    1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987, 1993, 1997,
    1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053, 2063, 2069, 2081, 2083,
    2087, 2089, 2099, 2111, 2113, 2129, 2131, 2137, 2141, 2143, 2153, 2161,
    2179, 2203, 2207, 2213, 2221, 2237, 2239, 2243, 2251, 2267, 2269, 2273,
    2281, 2287, 2293, 2297, 2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357,
    2371, 2377, 2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423, 2437, 2441,
    2447, 2459, 2467, 2473, 2477, 2503, 2521, 2531, 2539, 2543, 2549, 2551,
    2557, 2579, 2591, 2593, 2609, 2617, 2621, 2633, 2647, 2657, 2659, 2663,
    2671, 2677, 2683, 2687, 2689, 2693, 2699, 2707, 2711, 2713, 2719, 2729,
    2731, 2741, 2749, 2753, 2767, 2777, 2789, 2791, 2797, 2801, 2803, 2819,
    2833, 2837, 2843, 2851, 2857, 2861, 2879, 2887, 2897, 2903, 2909, 2917,
    2927, 2939, 2953, 2957, 2963, 2969, 2971, 2999, 3001, 3011, 3019, 3023,
    3037, 3041, 3049, 3061, 3067, 3079, 3083, 3089, 3109, 3119, 3121, 3137,
    3163, 3167, 3169, 3181, 3187, 3191, 3203, 3209, 3217, 3221, 3229, 3251,
    3253, 3257, 3259, 3271, 3299, 3301, 3307, 3313, 3319, 3323, 3329, 3331,
    3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413, 3433, 3449,
    3457, 3461, 3463, 3467, 3469, 3491, 3499, 3511, 3517, 3527, 3529, 3533,
    3539, 3541, 3547, 3557, 3559, 3571, 3581, 3583, 3593, 3607, 3613, 3617,
    3623, 3631, 3637, 3643, 3659, 3671, 3673, 3677, 3691, 3697, 3701, 3709,
    3719, 3727, 3733, 3739, 3761, 3767, 3769, 3779, 3793, 3797, 3803, 3821,
    3823, 3833, 3847, 3851, 3853, 3863, 3877, 3881, 3889, 3907, 3911, 3917,
    3919, 3923, 3929, 3931, 3943, 3947, 3967, 3989, 4001, 4003, 4007, 4013,
    4019, 4021, 4027, 4049, 4051, 4057, 4073, 4079, 4091, 4093, 4099, 4111,
    4127, 4129, 4133, 4139, 4153, 4157, 4159, 4177, 4201, 4211, 4217, 4219,
    4229, 4231, 4241, 4243, 4253, 4259, 4261, 4271, 4273, 4283, 4289, 4297,
    4327, 4337, 4339, 4349, 4357, 4363, 4373, 4391, 4397, 4409, 4421, 4423,
    4441, 4447, 4451, 4457, 4463, 4481, 4483, 4493, 4507, 4513, 4517, 4519,
    4523, 4547, 4549, 4561, 4567, 4583, 4591, 4597, 4603, 4621, 4637, 4639,
    4643, 4649, 4651, 4657, 4663, 4673, 4679, 4691, 4703, 4721, 4723, 4729,
    4733, 4751, 4759, 4783, 4787, 4789, 4793, 4799, 4801, 4813, 4817, 4831,
    4861, 4871, 4877, 4889, 4903, 4909, 4919, 4931, 4933, 4937, 4943, 4951,
    4957, 4967, 4969, 4973, 4987, 4993, 4999, 5003, 5009, 5011, 5021, 5023,
    5039, 5051, 5059, 5077, 5081, 5087, 5099, 5101, 5107, 5113, 5119, 5147,
    5153, 5167, 5171, 5179, 5189, 5197, 5209, 5227, 5231, 5233, 5237, 5261,
    5273, 5279, 5281, 5297, 5303, 5309, 5323, 5333, 5347, 5351, 5381, 5387,
    5393, 5399, 5407, 5413, 5417, 5419, 5431, 5437, 5441, 5443, 5449, 5471,
    5477, 5479, 5483, 5501, 5503, 5507, 5519, 5521, 5527, 5531, 5557, 5563,
    5569, 5573, 5581, 5591, 5623, 5639, 5641, 5647, 5651, 5653, 5657, 5659,
    5669, 5683, 5689, 5693, 5701, 5711, 5717, 5737, 5741, 5743, 5749, 5779,
    5783, 5791, 5801, 5807, 5813, 5821, 5827, 5839, 5843, 5849, 5851, 5857,
    5861, 5867, 5869, 5879, 5881, 5897, 5903, 5923, 5927, 5939, 5953, 5981,
    5987, 6007, 6011, 6029, 6037, 6043, 6047, 6053, 6067, 6073, 6079, 6089,
    6091, 6101, 6113, 6121, 6131, 6133, 6143, 6151, 6163, 6173, 6197, 6199,
    6203, 6211, 6217, 6221, 6229, 6247, 6257, 6263, 6269, 6271, 6277, 6287,
    6299, 6301, 6311, 6317, 6323, 6329, 6337, 6343, 6353, 6359, 6361, 6367,
    6373, 6379, 6389, 6397, 6421, 6427, 6449, 6451, 6469, 6473, 6481, 6491,
    6521, 6529, 6547, 6551, 6553, 6563, 6569, 6571, 6577, 6581, 6599, 6607,
    6619, 6637, 6653, 6659, 6661, 6673, 6679, 6689, 6691, 6701, 6703, 6709,
    6719, 6733, 6737, 6761, 6763, 6779, 6781, 6791, 6793, 6803, 6823, 6827,
    6829, 6833, 6841, 6857, 6863, 6869, 6871, 6883, 6899, 6907, 6911, 6917,
    6947, 6949, 6959, 6961, 6967, 6971, 6977, 6983, 6991, 6997, 7001, 7013,
    7019, 7027, 7039, 7043, 7057, 7069, 7079, 7103, 7109, 7121, 7127, 7129,
    7151, 7159, 7177, 7187, 7193, 7207, 7211, 7213, 7219, 7229, 7237, 7243,
    7247, 7253, 7283, 7297, 7307, 7309, 7321, 7331, 7333, 7349, 7351, 7369,
    7393, 7411, 7417, 7433, 7451, 7457, 7459, 7477, 7481, 7487, 7489, 7499,
    7507, 7517, 7523, 7529, 7537, 7541, 7547, 7549, 7559, 7561, 7573, 7577,
    7583, 7589, 7591, 7603, 7607, 7621, 7639, 7643, 7649, 7669, 7673, 7681,
    7687, 7691, 7699, 7703, 7717, 7723, 7727, 7741, 7753, 7757, 7759, 7789,
    7793, 7817, 7823, 7829, 7841, 7853, 7867, 7873, 7877, 7879, 7883, 7901,
    7907, 7919, 7927, 7933, 7937, 7949, 7951, 7963, 7993, 8009, 8011, 8017,
    8039, 8053, 8059, 8069, 8081, 8087, 8089, 8093, 8101, 8111, 8117, 8123,
    8147, 8161, 8167, 8171, 8179, 8191, 8209, 8219, 8221, 8231, 8233, 8237,
    8243, 8263, 8269, 8273, 8287, 8291, 8293, 8297, 8311, 8317, 8329, 8353,
    8363, 8369, 8377, 8387, 8389, 8419, 8423, 8429, 8431, 8443, 8447, 8461,
    8467, 8501, 8513, 8521, 8527, 8537, 8539, 8543, 8563, 8573, 8581, 8597,
    8599, 8609, 8623, 8627, 8629, 8641, 8647, 8663, 8669, 8677, 8681, 8689,
    8693, 8699, 8707, 8713, 8719, 8731, 8737, 8741, 8747, 8753, 8761, 8779,
    8783, 8803, 8807, 8819, 8821, 8831, 8837, 8839, 8849, 8861, 8863, 8867,
    8887, 8893, 8923, 8929, 8933, 8941, 8951, 8963, 8969, 8971, 8999, 9001,
    9007, 9011, 9013, 9029, 9041, 9043, 9049, 9059, 9067, 9091, 9103, 9109,
    9127, 9133, 9137, 9151, 9157, 9161, 9173, 9181, 9187, 9199, 9203, 9209,
    9221, 9227, 9239, 9241, 9257, 9277, 9281, 9283, 9293, 9311, 9319, 9323,
    9337, 9341, 9343, 9349, 9371, 9377, 9391, 9397, 9403, 9413, 9419, 9421,
    9431, 9433, 9437, 9439, 9461, 9463, 9467, 9473, 9479, 9491, 9497, 9511,
    9521, 9533, 9539, 9547, 9551, 9587, 9601, 9613, 9619, 9623, 9629, 9631,
    9643, 9649, 9661, 9677, 9679, 9689, 9697, 9719, 9721, 9733, 9739, 9743,
    9749, 9767, 9769, 9781, 9787, 9791, 9803, 9811, 9817, 9829, 9833, 9839,
    9851, 9857, 9859, 9871, 9883, 9887, 9901, 9907, 9923, 9929, 9931, 9941,
    9949, 9967, 9973 ]

for T in [Int,BigInt], n = [1:1000,1000000]
    n = convert(T,n)
    f = factor(n)
    @test n == prod(T[p^k for (p,k)=f])
    prime = n!=1 && length(f)==1 && get(f,n,0)==1
    @test isprime(n) == prime

    s = Base.primesmask(n)
    for k = 1:n
        @test s[k] == isprime(k)
    end
end

@test !isprime(1000000003)
@test !isprime(1000000005)
@test  isprime(1000000007)
@test  isprime(1000000009)
@test !isprime(1000000011)
@test !isprime(1000000013)

@test !isprime(10000000015)
@test !isprime(10000000017)
@test  isprime(10000000019)
@test !isprime(10000000021)
@test !isprime(10000000023)

@test !isprime(9223372036854775779)
@test !isprime(9223372036854775781)
@test  isprime(9223372036854775783)
@test !isprime(9223372036854775785)
@test !isprime(9223372036854775787)

@test !isprime(0xffffffffffffffc1)
@test !isprime(0xffffffffffffffc3)
@test  isprime(0xffffffffffffffc5)
@test !isprime(0xffffffffffffffc7)
@test !isprime(0xffffffffffffffc9)

# issue #5210
@test prod([ k^v for (k,v) in factor(typemax(Uint32)) ]) == typemax(Uint32)
@test prod([ k^v for (k,v) in factor(typemax(Int8)) ]) == typemax(Int8)

# rational-exponent promotion rules (issue #3155):
@test 2.0f0^(1//3) == 2.0f0^(1.0f0/3)
@test 2^(1//3) == 2^(1/3)

# large shift amounts
@test int32(-1)>>31 == -1
@test int32(-1)>>32 == -1
@test int32(-1)>>33 == -1
@test 10>>64 == 0
@test 10>>>64 == 0
@test 10<<64 == 0

# issue #3520 - certain int literals on 32-bit systems
@test -536870913 === -536870912-1

# overflow in rational comparison
@test 3//2 < typemax(Int)
@test 3//2 <= typemax(Int)

# check gcd and related functions against GMP
for i = -20:20, j = -20:20
    local d = gcd(i,j)
    @test d >= 0
    @test lcm(i,j) >= 0
    local ib = big(i)
    local jb = big(j)
    @test d == gcd(ib,jb)
    @test lcm(i,j) == lcm(ib,jb)
    @test gcdx(i,j) == gcdx(ib,jb)
    if j == 0
        @test_throws ErrorException invmod(i,j)
        @test_throws ErrorException invmod(ib,jb)
    elseif d == 1
        n = invmod(i,j)
        @test n == invmod(ib,jb)
        @test mod(n*i,j) == mod(1,j)
    end
end

# check powermod function against GMP
for i = -10:10, p = 0:5, m = -10:10
    if m != 0
        @test powermod(i,p,m) == powermod(i,p,big(m)) == powermod(big(i),big(p),big(m))
        @test mod(i^p,m) == powermod(i,p,m) == mod(big(i)^p,big(m))
    end
end

# with m==1 should give 0
@test powermod(1,0,1) == 0
@test powermod(big(1),0,1) == 0
@test powermod(1,0,-1) == 0
@test powermod(big(1),0,-1) == 0
# divide by zero error
@test_throws DivideError powermod(1,0,0)
@test_throws DivideError powermod(big(1),0,0)
# negative power domain error
@test_throws DomainError powermod(1,-2,1)
@test_throws DomainError powermod(big(1),-2,1)

# other divide-by-zero errors
@test_throws DivideError div(1,0)
@test_throws DivideError rem(1,0)
@test_throws DivideError mod(1,0)
@test_throws DivideError div(-1,0)
@test_throws DivideError rem(-1,0)
@test_throws DivideError mod(-1,0)
@test_throws DivideError div(uint(1),uint(0))
@test_throws DivideError rem(uint(1),uint(0))
@test_throws DivideError mod(uint(1),uint(0))
@test_throws DivideError div(typemin(Int),-1)
@test rem(typemin(Int),-1) == 0
@test mod(typemin(Int),-1) == 0

# prevpow2/nextpow2:
@test nextpow2(0) == prevpow2(0) == 0
for i = -2:2
    @test nextpow2(i) == prevpow2(i) == i
end
@test nextpow2(56789) == -nextpow2(-56789) == 65536
@test prevpow2(56789) == -prevpow2(-56789) == 32768
for i = -100:100
    @test nextpow2(i) == nextpow2(big(i))
    @test prevpow2(i) == prevpow2(big(i))
end

@test nextpow(2,1) == 1
@test prevpow(2,1) == 1
@test nextpow(3,243) == 243
@test prevpow(3,243) == 243
@test nextpow(3,241) == 243
@test prevpow(3,244) == 243
for a = -1:1
    @test_throws DomainError nextpow(a, 2)
    @test_throws DomainError prevpow(a, 2)
end
@test_throws DomainError nextpow(2,0)
@test_throws DomainError prevpow(2,0)

@test nextprod([2,3,5],30) == 30
@test nextprod([2,3,5],33) == 36

@test nextfloat(0.0) == 5.0e-324
@test prevfloat(0.0) == -5.0e-324
@test nextfloat(-0.0) == 5.0e-324
@test prevfloat(-0.0) == -5.0e-324
@test nextfloat(-5.0e-324) === -0.0
@test prevfloat(5.0e-324) == 0.0
@test nextfloat(-1.0) > -1.0
@test prevfloat(-1.0) < -1.0
@test nextfloat(nextfloat(0.0),-2) == -5.0e-324
@test nextfloat(prevfloat(0.0), 2) ==  5.0e-324
@test nextfloat(Inf) === Inf
@test prevfloat(-Inf) === -Inf
@test nextfloat(Inf32) === Inf32
@test prevfloat(-Inf32) === -Inf32

@test eps(realmax(Float64)) == 1.99584030953472e292
@test eps(-realmax(Float64)) == 1.99584030953472e292

# modular multiplicative inverses of odd numbers via exponentiation

for T = (Uint8,Int8,Uint16,Int16,Uint32,Int32,Uint64,Int64,Uint128,Int128)
    for n = 1:2:1000
        @test convert(T,n*(n^typemax(T))) == one(T)
        n = rand(T) | one(T)
        @test convert(T,n*(n^typemax(T))) == one(T)
    end
end

@test false*pi === 0.0
@test pi*false === 0.0
@test true*pi === float64(pi)
@test pi*true === float64(pi)

# issue #5881
@test bits(true) == "00000001"
@test bits(false) == "00000000"

# edge cases of intrinsics
let g() = sqrt(-1.0)
    @test_throws DomainError sqrt(-1.0)
end
@test sqrt(NaN) === NaN
let g() = sqrt(NaN)
    @test g() === NaN
end
let g(x) = sqrt(x)
    @test g(NaN) === NaN
end

# widen
@test widen(1.5f0) === 1.5
@test widen(int32(42)) === int64(42)
@test widen(Int8) === Int
@test widen(Float32) === Float64
## Note: this should change to e.g. Float128 at some point
@test widen(Float64) === BigFloat
@test widen(BigInt) === BigInt

@test widemul(typemax(Int64),typemax(Int64)) == 85070591730234615847396907784232501249

# .//
@test [1,2,3] // 4 == [1//4, 2//4, 3//4]
@test [1,2,3] .// [4,5,6] == [1//4, 2//5, 3//6]
@test [1+2im,3+4im] .// [5,6] == [(1+2im)//5,(3+4im)//6]
@test [1//3+2im,3+4im] .// [5,6] == [(1//3+2im)//5,(3+4im)//6]

# issue #7441
@test_throws InexactError int32(2.0^50)

@test_throws InexactError iround(Uint8, 255.5)
@test iround(Uint8, 255.4) === 0xff

@test_throws InexactError iround(Int16, -32768.7)
@test iround(Int16, -32768.1) === int16(-32768)

# issue #7508
@test_throws ErrorException reinterpret(Int, 0x01)
