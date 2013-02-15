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
@test bool(ComplexPair(0,0)) == false
@test bool(ComplexPair(1,0)) == true
@test_fails bool(ComplexPair(0,1)) == true
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

# definition and printing of extreme integers
@test bin(typemin(Uint8)) == "0"
@test bin(typemax(Uint8)) == "1"^8
@test oct(typemin(Uint8)) == "0"
@test oct(typemax(Uint8)) == "377"
@test dec(typemin(Uint8)) == "0"
@test dec(typemax(Uint8)) == "255"
@test hex(typemin(Uint8)) == "0"
@test hex(typemax(Uint8)) == "ff"
@test string(typemin(Uint8)) == "0x00"
@test string(typemax(Uint8)) == "0xff"
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
@test string(typemin(Uint16)) == "0x0000"
@test string(typemax(Uint16)) == "0xffff"
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
@test string(typemin(Uint32)) == "0x00000000"
@test string(typemax(Uint32)) == "0xffffffff"
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
@test string(typemin(Uint64)) == "0x0000000000000000"
@test string(typemax(Uint64)) == "0xffffffffffffffff"
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
@test string(typemin(Uint128)) == "0x00000000000000000000000000000000"
@test string(typemax(Uint128)) == "0xffffffffffffffffffffffffffffffff"

if WORD_SIZE > 32
@test dec(typemin(Uint128)) == "0"
@test dec(typemax(Uint128)) == "340282366920938463463374607431768211455"
@test base(3,typemin(Uint128)) == "0"
@test base(3,typemax(Uint128)) ==
    "202201102121002021012000211012011021221022212021111001022110211020010021100121010"
@test base(12,typemin(Uint128)) == "0"
@test base(12,typemax(Uint128)) == "5916b64b41143526a777873841863a6a6993"
end

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

if WORD_SIZE > 32
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
end

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
@test repr(pi) == "3.141592653589793"

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
@test  isless(-0.0,   0)
@test !isless(   0,-0.0)

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
    @test a//b == a/b
    @test a//b == a//b
    @test a//b == convert(Rational,a/b)
    if b == 0
        @test_fails integer(a//b) == integer(a/b)
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

@test sqrt(2) == 1.4142135623730951

@test 1+1.5 == 2.5
@test 1.5+1 == 2.5
@test 1+1.5+2 == 4.5
@test is(typeof(convert(ComplexPair{Int16},1)),ComplexPair{Int16})
@test ComplexPair(1,2)+1 == ComplexPair(2,2)
@test ComplexPair(1,2)+1.5 == ComplexPair(2.5,2.0)
@test 1/ComplexPair(2,2) == ComplexPair(.25,-.25)
@test ComplexPair(1.5,1.0) + 1//2 == ComplexPair(2.0,1.0)
@test real(ComplexPair(1//2,2//3)) == 1//2
@test imag(ComplexPair(1//2,2//3)) == 2//3
@test ComplexPair(1,2) + 1//2 == ComplexPair(3//2,2//1)
@test ComplexPair(1,2) + 1//2 * 0.5 == ComplexPair(1.25,2.0)
@test (ComplexPair(1,2) + 1//2) * 0.5 == ComplexPair(0.75,1.0)
@test (ComplexPair(1,2)/ComplexPair(2.5,3.0))*ComplexPair(2.5,3.0) == ComplexPair(1,2)
@test 0.7 < real(sqrt(ComplexPair(0,1))) < 0.707107

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

# check type of constructed complexes
real_types = {Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64, Float32, Float64,
              Rational{Int8}, Rational{Uint8}, Rational{Int16}, Rational{Uint16},
              Rational{Int32}, Rational{Uint32}, Rational{Int64}, Rational{Uint64}}
for A = real_types, B = real_types
    T = promote_type(A,B)
    @test typeof(ComplexPair(convert(A,2),convert(B,3))) <: ComplexPair{T}
end

# comparison should fail on complex
@test_fails complex(1,2) > 0
@test_fails complex(1,2) > complex(0,0)

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

# things related to floating-point epsilon
@test eps(float(0)) == 5e-324
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

for n = 1:100
    m = 1
    for (p,k) in factor(n)
        m *= p^k
    end
    @test n == m
end

# binary literals

@test 0b1010101 == 0x55
@test isa(0b00000000,Uint8)
@test isa(0b000000000,Uint16)
@test isa(0b0000000000000000,Uint16)
@test isa(0b00000000000000000,Uint32)
@test isa(0b00000000000000000000000000000000,Uint32)
@test isa(0b000000000000000000000000000000000,Uint64)
@test isa(0b11111111,Uint8)
@test isa(0b111111111,Uint16)
@test isa(0b1111111111111111,Uint16)
@test isa(0b11111111111111111,Uint32)
@test isa(0b11111111111111111111111111111111,Uint32)
@test isa(0b111111111111111111111111111111111,Uint64)

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
@test isa(0o11,Uint8)
@test isa(0o111,Uint8)
@test isa(0o11111,Uint16)
@test isa(0o111111,Uint16)
@test isa(0o1111111111,Uint32)
@test isa(0o11111111111,Uint32)

# float32 literals
@test isa(1f0,Float32)
@test isa(1.f0,Float32)
@test isa(1.0f0,Float32)
@test 1f0 == 1.
@test isa(1f1,Float32)
@test 1f1 == 10.

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

# issue #1308
@test hex(~uint128(0)) == "f"^32
@test uint128(~0) == ~uint128(0)
@test int128(~0) == ~int128(0)

# issue 1552
@test isa(convert(Rational{Int8},pi),Rational{Int8})
@test convert(Rational{Int8},pi) == 22//7
@test convert(Rational{Int64},0.957762604052997) == 42499549//44373782
@test convert(Rational{Int16},0.929261477046077) == 11639//12525
@test convert(Rational{Int16},0.2264705884044309) == 77//340
@test convert(Rational{Int16},0.39999899264235683) == 2//5
@test convert(Rational{Int16},1.1264233500618559e-5) == 0//1
@test convert(Rational{Uint16},1.1264233500618559e-5) == 1//65535
@test convert(Rational{Uint16},0.6666652791223875) == 2//3
@test convert(Rational{Int8},0.9374813124660655) == 15//16
@test convert(Rational{Int8},0.003803032342443835) == 0//1
@test convert(Rational{Uint8},0.003803032342443835) == 1//255
