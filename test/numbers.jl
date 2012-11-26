# basic booleans
@assert true
@assert !false
@assert !!true
@assert !!!false

@assert true  == true
@assert false == false
@assert true  != false
@assert false != true

@assert ~true == false
@assert ~false == true

@assert false & false == false
@assert true  & false == false
@assert false & true  == false
@assert true  & true  == true

@assert false | false == false
@assert true  | false == true
@assert false | true  == true
@assert true  | true  == true

@assert false $ false == false
@assert true  $ false == true
@assert false $ true  == true
@assert true  $ true  == false

# the bool operator
@assert bool(false) == false
@assert bool(true) == true
@assert bool(0) == false
@assert bool(1) == true
@assert bool(-1) == true
@assert bool(0.0) == false
@assert bool(1.0) == true
@assert bool(0.1) == true
@assert bool(-1.0) == true
@assert bool(ComplexPair(0,0)) == false
@assert bool(ComplexPair(1,0)) == true
@assert_fails bool(ComplexPair(0,1)) == true
@assert bool(0//1) == false
@assert bool(1//1) == true
@assert bool(1//2) == true

# basic arithmetic
@assert 2+3 == 5
@assert 2.+3. == 5.
@assert 2*3 == 6
@assert 2.*3 == 6
@assert 2. * 3. == 6.
@assert min(1.0,1) == 1

# definition and printing of extreme integers
@assert bin(typemin(Uint8)) == "0"
@assert bin(typemax(Uint8)) == "1"^8
@assert oct(typemin(Uint8)) == "0"
@assert oct(typemax(Uint8)) == "377"
@assert dec(typemin(Uint8)) == "0"
@assert dec(typemax(Uint8)) == "255"
@assert hex(typemin(Uint8)) == "0"
@assert hex(typemax(Uint8)) == "ff"
@assert string(typemin(Uint8)) == "0x00"
@assert string(typemax(Uint8)) == "0xff"
@assert base(3,typemin(Uint8)) == "0"
@assert base(3,typemax(Uint8)) == "100110"
@assert base(12,typemin(Uint8)) == "0"
@assert base(12,typemax(Uint8)) == "193"

@assert bin(typemin(Uint16)) == "0"
@assert bin(typemax(Uint16)) == "1"^16
@assert oct(typemin(Uint16)) == "0"
@assert oct(typemax(Uint16)) == "177777"
@assert dec(typemin(Uint16)) == "0"
@assert dec(typemax(Uint16)) == "65535"
@assert hex(typemin(Uint16)) == "0"
@assert hex(typemax(Uint16)) == "ffff"
@assert string(typemin(Uint16)) == "0x0000"
@assert string(typemax(Uint16)) == "0xffff"
@assert base(3,typemin(Uint16)) == "0"
@assert base(3,typemax(Uint16)) == "10022220020"
@assert base(12,typemin(Uint16)) == "0"
@assert base(12,typemax(Uint16)) == "31b13"

@assert bin(typemin(Uint32)) == "0"
@assert bin(typemax(Uint32)) == "1"^32
@assert oct(typemin(Uint32)) == "0"
@assert oct(typemax(Uint32)) == "37777777777"
@assert dec(typemin(Uint32)) == "0"
@assert dec(typemax(Uint32)) == "4294967295"
@assert hex(typemin(Uint32)) == "0"
@assert hex(typemax(Uint32)) == "ffffffff"
@assert string(typemin(Uint32)) == "0x00000000"
@assert string(typemax(Uint32)) == "0xffffffff"
@assert base(3,typemin(Uint32)) == "0"
@assert base(3,typemax(Uint32)) == "102002022201221111210"
@assert base(12,typemin(Uint32)) == "0"
@assert base(12,typemax(Uint32)) == "9ba461593"

@assert bin(typemin(Uint64)) == "0"
@assert bin(typemax(Uint64)) == "1"^64
@assert oct(typemin(Uint64)) == "0"
@assert oct(typemax(Uint64)) == "1777777777777777777777"
@assert dec(typemin(Uint64)) == "0"
@assert dec(typemax(Uint64)) == "18446744073709551615"
@assert hex(typemin(Uint64)) == "0"
@assert hex(typemax(Uint64)) == "ffffffffffffffff"
@assert string(typemin(Uint64)) == "0x0000000000000000"
@assert string(typemax(Uint64)) == "0xffffffffffffffff"
@assert base(3,typemin(Uint64)) == "0"
@assert base(3,typemax(Uint64)) == "11112220022122120101211020120210210211220"
@assert base(12,typemin(Uint64)) == "0"
@assert base(12,typemax(Uint64)) == "839365134a2a240713"

@assert bin(typemin(Uint128)) == "0"
@assert bin(typemax(Uint128)) == "1"^128
@assert oct(typemin(Uint128)) == "0"
@assert oct(typemax(Uint128)) == "3777777777777777777777777777777777777777777"
@assert hex(typemin(Uint128)) == "0"
@assert hex(typemax(Uint128)) == "ffffffffffffffffffffffffffffffff"
@assert string(typemin(Uint128)) == "0x00000000000000000000000000000000"
@assert string(typemax(Uint128)) == "0xffffffffffffffffffffffffffffffff"

if WORD_SIZE > 32
@assert dec(typemin(Uint128)) == "0"
@assert dec(typemax(Uint128)) == "340282366920938463463374607431768211455"
@assert base(3,typemin(Uint128)) == "0"
@assert base(3,typemax(Uint128)) ==
    "202201102121002021012000211012011021221022212021111001022110211020010021100121010"
@assert base(12,typemin(Uint128)) == "0"
@assert base(12,typemax(Uint128)) == "5916b64b41143526a777873841863a6a6993"
end

@assert bin(typemin(Int8)) == "-1"*"0"^7
@assert bin(typemax(Int8)) == "1"^7
@assert oct(typemin(Int8)) == "-200"
@assert oct(typemax(Int8)) == "177"
@assert dec(typemin(Int8)) == "-128"
@assert dec(typemax(Int8)) == "127"
@assert hex(typemin(Int8)) == "-80"
@assert hex(typemax(Int8)) == "7f"
@assert string(typemin(Int8)) == "-128"
@assert string(typemax(Int8)) == "127"
@assert base(3,typemin(Int8)) == "-11202"
@assert base(3,typemax(Int8)) == "11201"
@assert base(12,typemin(Int8)) == "-a8"
@assert base(12,typemax(Int8)) == "a7"

@assert bin(typemin(Int16)) == "-1"*"0"^15
@assert bin(typemax(Int16)) == "1"^15
@assert oct(typemin(Int16)) == "-100000"
@assert oct(typemax(Int16)) == "77777"
@assert dec(typemin(Int16)) == "-32768"
@assert dec(typemax(Int16)) == "32767"
@assert hex(typemin(Int16)) == "-8000"
@assert hex(typemax(Int16)) == "7fff"
@assert string(typemin(Int16)) == "-32768"
@assert string(typemax(Int16)) == "32767"
@assert base(3,typemin(Int16)) == "-1122221122"
@assert base(3,typemax(Int16)) == "1122221121"
@assert base(12,typemin(Int16)) == "-16b68"
@assert base(12,typemax(Int16)) == "16b67"

@assert bin(typemin(Int32)) == "-1"*"0"^31
@assert bin(typemax(Int32)) == "1"^31
@assert oct(typemin(Int32)) == "-20000000000"
@assert oct(typemax(Int32)) == "17777777777"
@assert dec(typemin(Int32)) == "-2147483648"
@assert dec(typemax(Int32)) == "2147483647"
@assert hex(typemin(Int32)) == "-80000000"
@assert hex(typemax(Int32)) == "7fffffff"
@assert string(typemin(Int32)) == "-2147483648"
@assert string(typemax(Int32)) == "2147483647"
@assert base(3,typemin(Int32)) == "-12112122212110202102"
@assert base(3,typemax(Int32)) == "12112122212110202101"
@assert base(12,typemin(Int32)) == "-4bb2308a8"
@assert base(12,typemax(Int32)) == "4bb2308a7"

@assert bin(typemin(Int64)) == "-1"*"0"^63
@assert bin(typemax(Int64)) == "1"^63
@assert oct(typemin(Int64)) == "-1000000000000000000000"
@assert oct(typemax(Int64)) == "777777777777777777777"
@assert dec(typemin(Int64)) == "-9223372036854775808"
@assert dec(typemax(Int64)) == "9223372036854775807"
@assert hex(typemin(Int64)) == "-8000000000000000"
@assert hex(typemax(Int64)) == "7fffffffffffffff"
@assert string(typemin(Int64)) == "-9223372036854775808"
@assert string(typemax(Int64)) == "9223372036854775807"
@assert base(3,typemin(Int64)) == "-2021110011022210012102010021220101220222"
@assert base(3,typemax(Int64)) == "2021110011022210012102010021220101220221"
@assert base(12,typemin(Int64)) == "-41a792678515120368"
@assert base(12,typemax(Int64)) == "41a792678515120367"

@assert bin(typemin(Int128)) == "-1"*"0"^127
@assert bin(typemax(Int128)) == "1"^127
@assert oct(typemin(Int128)) == "-2000000000000000000000000000000000000000000"
@assert oct(typemax(Int128)) == "1777777777777777777777777777777777777777777"
@assert hex(typemin(Int128)) == "-80000000000000000000000000000000"
@assert hex(typemax(Int128)) == "7fffffffffffffffffffffffffffffff"

if WORD_SIZE > 32
@assert dec(typemin(Int128)) == "-170141183460469231731687303715884105728"
@assert dec(typemax(Int128)) == "170141183460469231731687303715884105727"
@assert string(typemin(Int128)) == "-170141183460469231731687303715884105728"
@assert string(typemax(Int128)) == "170141183460469231731687303715884105727"
@assert base(3,typemin(Int128)) ==
    "-101100201022001010121000102002120122110122221010202000122201220121120010200022002"
@assert base(3,typemax(Int128)) ==
    "101100201022001010121000102002120122110122221010202000122201220121120010200022001"
@assert base(12,typemin(Int128)) == "-2a695925806818735399a37a20a31b3534a8"
@assert base(12,typemax(Int128)) == "2a695925806818735399a37a20a31b3534a7"
end

# signs
@assert sign(1) == 1
@assert sign(-1) == -1
@assert sign(0) == 0
@assert sign(1.0) == 1
@assert sign(-1.0) == -1
@assert sign(0.0) == 0
@assert sign(-0.0) == 0
@assert sign( 1.0/0.0) == 1
@assert sign(-1.0/0.0) == -1
@assert sign(Inf) == 1
@assert sign(-Inf) == -1
@assert isequal(sign(NaN), NaN)
@assert isequal(sign(-NaN), NaN)
@assert sign(2//3) == 1
@assert sign(-2//3) == -1
@assert sign(0//1) == 0
@assert sign(-0//1) == 0
@assert sign(1//0) == 1
@assert sign(-1//0) == -1

@assert signbit(1) == 0
@assert signbit(0) == 0
@assert signbit(-1) == 1
@assert signbit(1.0) == 0
@assert signbit(0.0) == 0
@assert signbit(-0.0) == 1
@assert signbit(-1.0) == 1
@assert signbit(1.0/0.0) == 0
@assert signbit(-1.0/0.0) == 1
@assert signbit(Inf) == 0
@assert signbit(-Inf) == 1
@assert signbit(NaN) == 0
@assert signbit(-NaN) == 1
@assert signbit(2//3) == 0
@assert signbit(-2//3) == 1
@assert signbit(0//1) == 0
@assert signbit(-0//1) == 0
@assert signbit(1//0) == 0
@assert signbit(-1//0) == 1

@assert isnan(1)     == false
@assert isnan(1.0)   == false
@assert isnan(-1.0)  == false
@assert isnan(Inf)   == false
@assert isnan(-Inf)  == false
@assert isnan(NaN)   == true
@assert isnan(1//2)  == false
@assert isnan(-2//3) == false
@assert isnan(5//0)  == false
@assert isnan(-3//0) == false

@assert isinf(1)     == false
@assert isinf(1.0)   == false
@assert isinf(-1.0)  == false
@assert isinf(Inf)   == true
@assert isinf(-Inf)  == true
@assert isinf(NaN)   == false
@assert isinf(1//2)  == false
@assert isinf(-2//3) == false
@assert isinf(5//0)  == true
@assert isinf(-3//0) == true

@assert isfinite(1)     == true
@assert isfinite(1.0)   == true
@assert isfinite(-1.0)  == true
@assert isfinite(Inf)   == false
@assert isfinite(-Inf)  == false
@assert isfinite(NaN)   == false
@assert isfinite(1//2)  == true
@assert isfinite(-2//3) == true
@assert isfinite(5//0)  == false
@assert isfinite(-3//0) == false

@assert isequal(-Inf,-Inf)
@assert isequal(-1.0,-1.0)
@assert isequal(-0.0,-0.0)
@assert isequal(+0.0,+0.0)
@assert isequal(+1.0,+1.0)
@assert isequal(+Inf,+Inf)
@assert isequal(-NaN,-NaN)
@assert isequal(-NaN,+NaN)
@assert isequal(+NaN,-NaN)
@assert isequal(+NaN,+NaN)

@assert !isequal(-Inf,+Inf)
@assert !isequal(-1.0,+1.0)
@assert !isequal(-0.0,+0.0)
@assert !isequal(+0.0,-0.0)
@assert !isequal(+1.0,-1.0)
@assert !isequal(+Inf,-Inf)

@assert !isless(-Inf,-Inf)
@assert  isless(-Inf,-1.0)
@assert  isless(-Inf,-0.0)
@assert  isless(-Inf,+0.0)
@assert  isless(-Inf,+1.0)
@assert  isless(-Inf,+Inf)
@assert  isless(-Inf,-NaN)
@assert  isless(-Inf,+NaN)

@assert !isless(-1.0,-Inf)
@assert !isless(-1.0,-1.0)
@assert  isless(-1.0,-0.0)
@assert  isless(-1.0,+0.0)
@assert  isless(-1.0,+1.0)
@assert  isless(-1.0,+Inf)
@assert  isless(-1.0,-NaN)
@assert  isless(-1.0,+NaN)

@assert !isless(-0.0,-Inf)
@assert !isless(-0.0,-1.0)
@assert !isless(-0.0,-0.0)
@assert  isless(-0.0,+0.0)
@assert  isless(-0.0,+1.0)
@assert  isless(-0.0,+Inf)
@assert  isless(-0.0,-NaN)
@assert  isless(-0.0,+NaN)

@assert !isless(+0.0,-Inf)
@assert !isless(+0.0,-1.0)
@assert !isless(+0.0,-0.0)
@assert !isless(+0.0,+0.0)
@assert  isless(+0.0,+1.0)
@assert  isless(+0.0,+Inf)
@assert  isless(+0.0,-NaN)
@assert  isless(+0.0,+NaN)

@assert !isless(+1.0,-Inf)
@assert !isless(+1.0,-1.0)
@assert !isless(+1.0,-0.0)
@assert !isless(+1.0,+0.0)
@assert !isless(+1.0,+1.0)
@assert  isless(+1.0,+Inf)
@assert  isless(+1.0,-NaN)
@assert  isless(+1.0,+NaN)

@assert !isless(+Inf,-Inf)
@assert !isless(+Inf,-1.0)
@assert !isless(+Inf,-0.0)
@assert !isless(+Inf,+0.0)
@assert !isless(+Inf,+1.0)
@assert !isless(+Inf,+Inf)
@assert  isless(+Inf,-NaN)
@assert  isless(+Inf,+NaN)

@assert !isless(-NaN,-Inf)
@assert !isless(-NaN,-1.0)
@assert !isless(-NaN,-0.0)
@assert !isless(-NaN,+0.0)
@assert !isless(-NaN,+1.0)
@assert !isless(-NaN,+Inf)
@assert !isless(-NaN,-NaN)
@assert !isless(-NaN,+NaN)

@assert !isless(+NaN,-Inf)
@assert !isless(+NaN,-1.0)
@assert !isless(+NaN,-0.0)
@assert !isless(+NaN,+0.0)
@assert !isless(+NaN,+1.0)
@assert !isless(+NaN,+Inf)
@assert !isless(+NaN,-NaN)
@assert !isless(+NaN,+NaN)

@assert  isequal(   0, 0.0)
@assert  isequal( 0.0,   0)
@assert !isequal(   0,-0.0)
@assert !isequal(-0.0,   0)
@assert  isless(-0.0,   0)
@assert !isless(   0,-0.0)

for x=-5:5, y=-5:5
    @assert (x==y)==(float64(x)==int64(y))
    @assert (x!=y)==(float64(x)!=int64(y))
    @assert (x< y)==(float64(x)< int64(y))
    @assert (x> y)==(float64(x)> int64(y))
    @assert (x<=y)==(float64(x)<=int64(y))
    @assert (x>=y)==(float64(x)>=int64(y))

    @assert (x==y)==(int64(x)==float64(y))
    @assert (x!=y)==(int64(x)!=float64(y))
    @assert (x< y)==(int64(x)< float64(y))
    @assert (x> y)==(int64(x)> float64(y))
    @assert (x<=y)==(int64(x)<=float64(y))
    @assert (x>=y)==(int64(x)>=float64(y))

    if x >= 0
        @assert (x==y)==(uint64(x)==float64(y))
        @assert (x!=y)==(uint64(x)!=float64(y))
        @assert (x< y)==(uint64(x)< float64(y))
        @assert (x> y)==(uint64(x)> float64(y))
        @assert (x<=y)==(uint64(x)<=float64(y))
        @assert (x>=y)==(uint64(x)>=float64(y))
    end
    if y >= 0
        @assert (x==y)==(float64(x)==uint64(y))
        @assert (x!=y)==(float64(x)!=uint64(y))
        @assert (x< y)==(float64(x)< uint64(y))
        @assert (x> y)==(float64(x)> uint64(y))
        @assert (x<=y)==(float64(x)<=uint64(y))
        @assert (x>=y)==(float64(x)>=uint64(y))
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
    @assert y == float64(itrunc(y))
    # println("x=$x; y=float64($(itrunc(y)));")

    @assert (x==y)==(y==x)
    @assert (x!=y)==!(x==y)
    @assert (-x==-y)==(-y==-x)
    @assert (-x!=-y)==!(-x==-y)

    @assert (x<y)==(x<=y)&(x!=y)
    @assert (x<=y)==(x<y)|(x==y)
    @assert (x==y)==(x<=y)&!(x<y)

    @assert -x != x
    @assert -y != y
    @assert -x != y
    @assert -y != x
    @assert -x <  x
    @assert -y <  y
    @assert -x <  y
    @assert -y <  x
    @assert -x <= x
    @assert -y <= y
    @assert -x <= y
    @assert -y <= x

    @assert -y != u
    @assert -y <  u
    @assert -y <= u

    c = _cmp_(x,y)
    if c < 0
        @assert !(x == y)
        @assert  (x <  y)
        @assert !(y <  x)
        @assert  (x <= y)
        @assert !(y <= x)

        @assert !(u == y)
        @assert  (u <  y)
        @assert !(y <  u)
        @assert  (u <= y)
        @assert !(y <= u)

        @assert !(-x == -y)
        @assert !(-x <  -y)
        @assert  (-y <  -x)
        @assert !(-x <= -y)
        @assert  (-y <= -x)
    elseif c > 0
        @assert !(x == y)
        @assert !(x <  y)
        @assert  (y <  x)
        @assert !(x <= y)
        @assert  (y <= x)

        @assert !(u == y)
        @assert !(u <  y)
        @assert  (y <  u)
        @assert !(u <= y)
        @assert  (y <= u)

        @assert !(-x == -y)
        @assert  (-x <  -y)
        @assert !(-y <  -x)
        @assert  (-x <= -y)
        @assert !(-y <= -x)
    else
        @assert  (x == y)
        @assert !(x <  y)
        @assert !(y <  x)
        @assert  (x <= y)
        @assert  (y <= x)

        @assert  (u == y)
        @assert !(u <  y)
        @assert !(y <  u)
        @assert  (u <= y)
        @assert  (y <= u)

        @assert  (-x == -y)
        @assert !(-x <  -y)
        @assert !(-y <  -x)
        @assert  (-x <= -y)
        @assert  (-y <= -x)
    end
end

@assert int64(2)^62-1 != 2.0^62
@assert int64(2)^62   == 2.0^62
@assert int64(2)^62+1 != 2.0^62
@assert 2.0^62 != int64(2)^62-1
@assert 2.0^62 == int64(2)^62
@assert 2.0^62 != int64(2)^62+1

@assert typemax(Int64)   != +2.0^63
@assert typemin(Int64)   == -2.0^63
@assert typemin(Int64)+1 != -2.0^63

@assert uint64(2)^60-1 != 2.0^60
@assert uint64(2)^60   == 2.0^60
@assert uint64(2)^60+1 != 2.0^60
@assert 2.0^60 != uint64(2)^60-1
@assert 2.0^60 == uint64(2)^60
@assert 2.0^60 != uint64(2)^60+1

@assert uint64(2)^63-1 != 2.0^63
@assert uint64(2)^63   == 2.0^63
@assert uint64(2)^63+1 != 2.0^63
@assert 2.0^63 != uint64(2)^63-1
@assert 2.0^63 == uint64(2)^63
@assert 2.0^63 != uint64(2)^63+1

@assert typemax(Uint64) != 2.0^64

@assert !(NaN <= 1)
@assert !(NaN >= 1)
@assert !(NaN < 1)
@assert !(NaN > 1)
@assert !(1 <= NaN)
@assert !(1 >= NaN)
@assert !(1 < NaN)
@assert !(1 > NaN)

@assert 1//1 == 1
@assert 2//2 == 1
@assert 1//1 == 1//1
@assert 2//2 == 1//1
@assert 2//4 == 3//6
@assert 1//2 + 1//2 == 1
@assert (-1)//3 == -(1//3)
@assert 1//2 + 3//4 == 5//4
@assert 1//3 * 3//4 == 1//4
@assert 1//2 / 3//4 == 2//3
@assert 1//0 == 1//0
@assert 5//0 == 1//0
@assert -1//0 == -1//0
@assert -7//0 == -1//0

for a = -5:5, b = -5:5
    if a == b == 0; continue; end
    @assert a//b == a/b
    @assert a//b == a//b
    @assert a//b == rational(a/b)
    if b == 0
        @assert_fails integer(a//b) == integer(a/b)
    else
        @assert integer(a//b) == integer(a/b)
    end
    for c = -5:5
        @assert (a//b == c) == (a/b == c)
        @assert (a//b != c) == (a/b != c)
        @assert (a//b <= c) == (a/b <= c)
        @assert (a//b <  c) == (a/b <  c)
        @assert (a//b >= c) == (a/b >= c)
        @assert (a//b >  c) == (a/b >  c)
        for d = -5:5
            if c == d == 0; continue; end
            @assert (a//b == c//d) == (a/b == c/d)
            @assert (a//b != c//d) == (a/b != c/d)
            @assert (a//b <= c//d) == (a/b <= c/d)
            @assert (a//b <  c//d) == (a/b <  c/d)
            @assert (a//b >= c//d) == (a/b >= c/d)
            @assert (a//b >  c//d) == (a/b >  c/d)
        end
    end
end

@assert sqrt(2) == 1.4142135623730951

@assert 1+1.5 == 2.5
@assert 1.5+1 == 2.5
@assert 1+1.5+2 == 4.5
@assert is(typeof(convert(ComplexPair{Int16},1)),ComplexPair{Int16})
@assert ComplexPair(1,2)+1 == ComplexPair(2,2)
@assert ComplexPair(1,2)+1.5 == ComplexPair(2.5,2.0)
@assert 1/ComplexPair(2,2) == ComplexPair(.25,-.25)
@assert ComplexPair(1.5,1.0) + 1//2 == ComplexPair(2.0,1.0)
@assert real(ComplexPair(1//2,2//3)) == 1//2
@assert imag(ComplexPair(1//2,2//3)) == 2//3
@assert ComplexPair(1,2) + 1//2 == ComplexPair(3//2,2//1)
@assert ComplexPair(1,2) + 1//2 * 0.5 == ComplexPair(1.25,2.0)
@assert (ComplexPair(1,2) + 1//2) * 0.5 == ComplexPair(0.75,1.0)
@assert (ComplexPair(1,2)/ComplexPair(2.5,3.0))*ComplexPair(2.5,3.0) == ComplexPair(1,2)
@assert 0.7 < real(sqrt(ComplexPair(0,1))) < 0.707107

for S = {Int8,  Int16,  Int32,  Int64},
    U = {Uint8, Uint16, Uint32, Uint64}
    @assert !(-one(S) == typemax(U))
    @assert -one(S) != typemax(U)
    @assert -one(S) < typemax(U)
    @assert !(typemax(U) <= -one(S))
end

# check type of constructed rationals
int_types = {Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64}
for N = int_types, D = int_types
    T = promote_type(N,D)
    @assert typeof(convert(N,2)//convert(D,3)) <: Rational{T}
end

# check type of constructed complexes
real_types = {Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64, Float32, Float64,
              Rational{Int8}, Rational{Uint8}, Rational{Int16}, Rational{Uint16},
              Rational{Int32}, Rational{Uint32}, Rational{Int64}, Rational{Uint64}}
for A = real_types, B = real_types
    T = promote_type(A,B)
    @assert typeof(ComplexPair(convert(A,2),convert(B,3))) <: ComplexPair{T}
end

# comparison should fail on complex
@assert_fails complex(1,2) > 0
@assert_fails complex(1,2) > complex(0,0)

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
            @assert div(+x,+y) == 0
            @assert div(+x,-y) == 0
            @assert div(-x,+y) == 0
            @assert div(-x,-y) == 0
        end
        if 1y <= x < 2y
            @assert div(+x,+y) == +1
            @assert div(+x,-y) == -1
            @assert div(-x,+y) == -1
            @assert div(-x,-y) == +1
        end
        if 2y <= x < 3y
            @assert div(+x,+y) == +2
            @assert div(+x,-y) == -2
            @assert div(-x,+y) == -2
            @assert div(-x,-y) == +2
        end

        # check basic fld functionality
        if 0 == x
            @assert fld(+x,+y) == 0
            @assert fld(+x,-y) == 0
            @assert fld(-x,+y) == 0
            @assert fld(-x,-y) == 0
        end
        if 0 < x < 1y
            @assert fld(+x,+y) == +0
            @assert fld(+x,-y) == -1
            @assert fld(-x,+y) == -1
            @assert fld(-x,-y) == +0
        end
        if 1y == x
            @assert fld(+x,+y) == +1
            @assert fld(+x,-y) == -1
            @assert fld(-x,+y) == -1
            @assert fld(-x,-y) == +1
        end
        if 1y < x < 2y
            @assert fld(+x,+y) == +1
            @assert fld(+x,-y) == -2
            @assert fld(-x,+y) == -2
            @assert fld(-x,-y) == +1
        end
        if 2y == x
            @assert fld(+x,+y) == +2
            @assert fld(+x,-y) == -2
            @assert fld(-x,+y) == -2
            @assert fld(-x,-y) == +2
        end
        if 2y < x < 3y
            @assert fld(+x,+y) == +2
            @assert fld(+x,-y) == -3
            @assert fld(-x,+y) == -3
            @assert fld(-x,-y) == +2
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

        @assert typeof(d) <: t1
        @assert typeof(f) <: t1
        @assert typeof(r) <: t2
        @assert typeof(m) <: t2

        @assert d == f
        @assert r == m
        @assert 0 <= r < y
        @assert x == y*d + r

        for X=[-1,1], Y=[-1,1]
            sx = X*x
            sy = Y*y

            sd = div(sx,sy)
            sf = fld(sx,sy)
            sr = rem(sx,sy)
            sm = mod(sx,sy)

            @assert typeof(sd) <: t1
            @assert typeof(sf) <: t1
            @assert typeof(sr) <: t2
            @assert typeof(sm) <: t2

            @assert sx < 0 ? -y < sr <= 0 : 0 <= sr < +y
            @assert sy < 0 ? -y < sm <= 0 : 0 <= sm < +y
            @assert sx == sy*sd + sr
            @assert sx == sy*sf + sm
        end
    end
end

@assert div(typemax(Int64)  , 1) ==  9223372036854775807
@assert div(typemax(Int64)  , 2) ==  4611686018427387903
@assert div(typemax(Int64)  , 7) ==  1317624576693539401
@assert div(typemax(Int64)  ,-1) == -9223372036854775807
@assert div(typemax(Int64)  ,-2) == -4611686018427387903
@assert div(typemax(Int64)  ,-7) == -1317624576693539401
@assert div(typemax(Int64)-1, 1) ==  9223372036854775806
@assert div(typemax(Int64)-1, 2) ==  4611686018427387903
@assert div(typemax(Int64)-1, 7) ==  1317624576693539400
@assert div(typemax(Int64)-1,-1) == -9223372036854775806
@assert div(typemax(Int64)-1,-2) == -4611686018427387903
@assert div(typemax(Int64)-1,-7) == -1317624576693539400
@assert div(typemax(Int64)-2, 1) ==  9223372036854775805
@assert div(typemax(Int64)-2, 2) ==  4611686018427387902
@assert div(typemax(Int64)-2, 7) ==  1317624576693539400
@assert div(typemax(Int64)-2,-1) == -9223372036854775805
@assert div(typemax(Int64)-2,-2) == -4611686018427387902
@assert div(typemax(Int64)-2,-7) == -1317624576693539400

@assert div(typemin(Int64)  , 1) == -9223372036854775807-1
@assert div(typemin(Int64)  , 2) == -4611686018427387904
@assert div(typemin(Int64)  , 7) == -1317624576693539401
#@assert div(typemin(Int64)  ,-1) == -9223372036854775807-1 # FIXME!
@assert div(typemin(Int64)  ,-2) ==  4611686018427387904
@assert div(typemin(Int64)  ,-7) ==  1317624576693539401
@assert div(typemin(Int64)+1, 1) == -9223372036854775807
@assert div(typemin(Int64)+1, 2) == -4611686018427387903
@assert div(typemin(Int64)+1, 7) == -1317624576693539401
@assert div(typemin(Int64)+1,-1) ==  9223372036854775807
@assert div(typemin(Int64)+1,-2) ==  4611686018427387903
@assert div(typemin(Int64)+1,-7) ==  1317624576693539401
@assert div(typemin(Int64)+2, 1) == -9223372036854775806
@assert div(typemin(Int64)+2, 2) == -4611686018427387903
@assert div(typemin(Int64)+2, 7) == -1317624576693539400
@assert div(typemin(Int64)+2,-1) ==  9223372036854775806
@assert div(typemin(Int64)+2,-2) ==  4611686018427387903
@assert div(typemin(Int64)+2,-7) ==  1317624576693539400
@assert div(typemin(Int64)+3, 1) == -9223372036854775805
@assert div(typemin(Int64)+3, 2) == -4611686018427387902
@assert div(typemin(Int64)+3, 7) == -1317624576693539400
@assert div(typemin(Int64)+3,-1) ==  9223372036854775805
@assert div(typemin(Int64)+3,-2) ==  4611686018427387902
@assert div(typemin(Int64)+3,-7) ==  1317624576693539400

@assert fld(typemax(Int64)  , 1) ==  9223372036854775807
@assert fld(typemax(Int64)  , 2) ==  4611686018427387903
@assert fld(typemax(Int64)  , 7) ==  1317624576693539401
@assert fld(typemax(Int64)  ,-1) == -9223372036854775807
@assert fld(typemax(Int64)  ,-2) == -4611686018427387904
@assert fld(typemax(Int64)  ,-7) == -1317624576693539401
@assert fld(typemax(Int64)-1, 1) ==  9223372036854775806
@assert fld(typemax(Int64)-1, 2) ==  4611686018427387903
@assert fld(typemax(Int64)-1, 7) ==  1317624576693539400
@assert fld(typemax(Int64)-1,-1) == -9223372036854775806
@assert fld(typemax(Int64)-1,-2) == -4611686018427387903
@assert fld(typemax(Int64)-1,-7) == -1317624576693539401
@assert fld(typemax(Int64)-2, 1) ==  9223372036854775805
@assert fld(typemax(Int64)-2, 2) ==  4611686018427387902
@assert fld(typemax(Int64)-2, 7) ==  1317624576693539400
@assert fld(typemax(Int64)-2,-1) == -9223372036854775805
@assert fld(typemax(Int64)-2,-2) == -4611686018427387903
@assert fld(typemax(Int64)-2,-7) == -1317624576693539401

@assert fld(typemin(Int64)  , 1) == -9223372036854775807-1
@assert fld(typemin(Int64)  , 2) == -4611686018427387904
@assert fld(typemin(Int64)  , 7) == -1317624576693539402
#@assert fld(typemin(Int64)  ,-1) == -9223372036854775807-1 # FIXME!
@assert fld(typemin(Int64)  ,-2) ==  4611686018427387904
@assert fld(typemin(Int64)  ,-7) ==  1317624576693539401
@assert fld(typemin(Int64)+1, 1) == -9223372036854775807
@assert fld(typemin(Int64)+1, 2) == -4611686018427387904
@assert fld(typemin(Int64)+1, 7) == -1317624576693539401
@assert fld(typemin(Int64)+1,-1) ==  9223372036854775807
@assert fld(typemin(Int64)+1,-2) ==  4611686018427387903
@assert fld(typemin(Int64)+1,-7) ==  1317624576693539401
@assert fld(typemin(Int64)+2, 1) == -9223372036854775806
@assert fld(typemin(Int64)+2, 2) == -4611686018427387903
@assert fld(typemin(Int64)+2, 7) == -1317624576693539401
@assert fld(typemin(Int64)+2,-1) ==  9223372036854775806
@assert fld(typemin(Int64)+2,-2) ==  4611686018427387903
@assert fld(typemin(Int64)+2,-7) ==  1317624576693539400
@assert fld(typemin(Int64)+3, 1) == -9223372036854775805
@assert fld(typemin(Int64)+3, 2) == -4611686018427387903
@assert fld(typemin(Int64)+3, 7) == -1317624576693539401
@assert fld(typemin(Int64)+3,-1) ==  9223372036854775805
@assert fld(typemin(Int64)+3,-2) ==  4611686018427387902
@assert fld(typemin(Int64)+3,-7) ==  1317624576693539400

for x={typemin(Int64), -typemax(Int64), -typemax(Int64)+1, -typemax(Int64)+2,
       typemax(Int64)-2, typemax(Int64)-1, typemax(Int64),
       typemax(Uint64)-1, typemax(Uint64)-2, typemax(Uint64)},
    y={-7,-2,-1,1,2,7}
    if x >= 0
        @assert div(unsigned(x),y) == unsigned(div(x,y))
        @assert fld(unsigned(x),y) == unsigned(fld(x,y))
    end
    if isa(x,Signed) && y >= 0
        @assert div(x,unsigned(y)) == div(x,y)
        @assert fld(x,unsigned(y)) == fld(x,y)
    end
end

for x=0:5, y=1:5
    @assert div(uint(x),uint(y)) == div(x,y)
    @assert div(uint(x),y) == div(x,y)
    @assert div(x,uint(y)) == div(x,y)
    @assert div(uint(x),-y) == uint(div(x,-y))
    @assert div(-x,uint(y)) == div(-x,y)

    @assert fld(uint(x),uint(y)) == fld(x,y)
    @assert fld(uint(x),y) == fld(x,y)
    @assert fld(x,uint(y)) == fld(x,y)
    @assert fld(uint(x),-y) == uint(fld(x,-y))
    @assert fld(-x,uint(y)) == fld(-x,y)

    @assert rem(uint(x),uint(y)) == rem(x,y)
    @assert rem(uint(x),y) == rem(x,y)
    @assert rem(x,uint(y)) == rem(x,y)
    @assert rem(uint(x),-y) == rem(x,-y)
    @assert rem(-x,uint(y)) == rem(-x,y)

    @assert mod(uint(x),uint(y)) == mod(x,y)
    @assert mod(uint(x),y) == mod(x,y)
    @assert mod(x,uint(y)) == mod(x,y)
    @assert mod(uint(x),-y) == mod(x,-y)
    @assert mod(-x,uint(y)) == mod(-x,y)
end

@assert div(typemax(Uint64)  , 1) ==  typemax(Uint64)
@assert div(typemax(Uint64)  ,-1) == -typemax(Uint64)
@assert div(typemax(Uint64)-1, 1) ==  typemax(Uint64)-1
@assert div(typemax(Uint64)-1,-1) == -typemax(Uint64)+1
@assert div(typemax(Uint64)-2, 1) ==  typemax(Uint64)-2
@assert div(typemax(Uint64)-2,-1) == -typemax(Uint64)+2

@assert signed(div(unsigned(typemax(Int64))+2, 1)) ==  typemax(Int64)+2
@assert signed(div(unsigned(typemax(Int64))+2,-1)) == -typemax(Int64)-2
@assert signed(div(unsigned(typemax(Int64))+1, 1)) ==  typemax(Int64)+1
@assert signed(div(unsigned(typemax(Int64))+1,-1)) == -typemax(Int64)-1
@assert signed(div(unsigned(typemax(Int64))  , 1)) ==  typemax(Int64)
@assert signed(div(unsigned(typemax(Int64))  ,-1)) == -typemax(Int64)

@assert signed(div(typemax(Uint),typemax(Int)))        ==  2
@assert signed(div(typemax(Uint),(typemax(Int)>>1)+1)) ==  3
@assert signed(div(typemax(Uint),typemax(Int)>>1))     ==  4
@assert signed(div(typemax(Uint),typemin(Int)))        == -1
@assert signed(div(typemax(Uint),typemin(Int)+1))      == -2
@assert signed(div(typemax(Uint),typemin(Int)>>1))     == -3
@assert signed(div(typemax(Uint),(typemin(Int)>>1)+1)) == -4

@assert fld(typemax(Uint64)  , 1) ==  typemax(Uint64)
@assert fld(typemax(Uint64)  ,-1) == -typemax(Uint64)
@assert fld(typemax(Uint64)-1, 1) ==  typemax(Uint64)-1
@assert fld(typemax(Uint64)-1,-1) == -typemax(Uint64)+1
@assert fld(typemax(Uint64)-2, 1) ==  typemax(Uint64)-2
@assert fld(typemax(Uint64)-2,-1) == -typemax(Uint64)+2

@assert signed(fld(unsigned(typemax(Int64))+2, 1)) ==  typemax(Int64)+2
@assert signed(fld(unsigned(typemax(Int64))+2,-1)) == -typemax(Int64)-2
@assert signed(fld(unsigned(typemax(Int64))+1, 1)) ==  typemax(Int64)+1
@assert signed(fld(unsigned(typemax(Int64))+1,-1)) == -typemax(Int64)-1
@assert signed(fld(unsigned(typemax(Int64))  , 1)) ==  typemax(Int64)
@assert signed(fld(unsigned(typemax(Int64))  ,-1)) == -typemax(Int64)

@assert signed(fld(typemax(Uint),typemax(Int)))        ==  2
@assert signed(fld(typemax(Uint),(typemax(Int)>>1)+1)) ==  3
@assert signed(fld(typemax(Uint),typemax(Int)>>1))     ==  4
@assert signed(fld(typemax(Uint),typemin(Int)))        == -2
@assert signed(fld(typemax(Uint),typemin(Int)+1))      == -3
@assert signed(fld(typemax(Uint),typemin(Int)>>1))     == -4
@assert signed(fld(typemax(Uint),(typemin(Int)>>1)+1)) == -5

# things related to floating-point epsilon
@assert eps(float(0)) == 5e-324
@assert .1+.1+.1 != .3
# TODO: uncomment when isapprox() becomes part of base.
# @assert isapprox(.1+.1+.1, .3)
# @assert !isapprox(.1+.1+.1-.3, 0)
# @assert isapprox(.1+.1+.1-.3, 0, eps(.3))

@assert div(1e50,1) == 1e50
@assert fld(1e50,1) == 1e50

# rounding difficult values

for x = 2^53-10:2^53+10
    y = float64(x)
    i = itrunc(y)
    @assert int64(trunc(y)) == i
    @assert int64(round(y)) == i
    @assert int64(floor(y)) == i
    @assert int64(ceil(y))  == i
    @assert iround(y)       == i
    @assert ifloor(y)       == i
    @assert iceil(y)        == i
end

for x = 2^24-10:2^24+10
    y = float32(x)
    i = itrunc(y)
    @assert int(trunc(y)) == i
    @assert int(round(y)) == i
    @assert int(floor(y)) == i
    @assert int(ceil(y))  == i
    @assert iround(y)     == i
    @assert ifloor(y)     == i
    @assert iceil(y)      == i
end

for n = 1:100
    m = 1
    for (p,k) in factor(n)
        m *= p^k
    end
    @assert n == m
end

# binary literals

@assert 0b1010101 == 0x55
@assert isa(0b00000000,Uint8)
@assert isa(0b000000000,Uint16)
@assert isa(0b0000000000000000,Uint16)
@assert isa(0b00000000000000000,Uint32)
@assert isa(0b00000000000000000000000000000000,Uint32)
@assert isa(0b000000000000000000000000000000000,Uint64)
@assert isa(0b11111111,Uint8)
@assert isa(0b111111111,Uint16)
@assert isa(0b1111111111111111,Uint16)
@assert isa(0b11111111111111111,Uint32)
@assert isa(0b11111111111111111111111111111111,Uint32)
@assert isa(0b111111111111111111111111111111111,Uint64)

# octal literals

@assert 0o10 == 0x8
@assert 0o100 == 0x40
@assert 0o1000 == 0x200
@assert 0o724 == 0x1d4
@assert isa(0o377,Uint8)
@assert isa(0o00,Uint8)
@assert isa(0o000,Uint16)
@assert isa(0o00000,Uint16)
@assert isa(0o000000,Uint32)
@assert isa(0o0000000000,Uint32)
@assert isa(0o00000000000,Uint64)
@assert isa(0o11,Uint8)
@assert isa(0o111,Uint8)
@assert isa(0o11111,Uint16)
@assert isa(0o111111,Uint16)
@assert isa(0o1111111111,Uint32)
@assert isa(0o11111111111,Uint32)

# float32 literals
@assert isa(1f0,Float32)
@assert isa(1.f0,Float32)
@assert isa(1.0f0,Float32)
@assert 1f0 == 1.
@assert isa(1f1,Float32)
@assert 1f1 == 10.

# custom rounding and significant-digit ops
function approx_eq(a, b, tol)
    abs(a - b) < tol
end
approx_eq(a, b) = approx_eq(a, b, 1e-6)
# rounding to digits relative to the decimal point 
@assert approx_eq(round(pi,0), 3.)
@assert approx_eq(round(pi,1), 3.1)
@assert approx_eq(round(10*pi,-1), 30.)
@assert round(.1,0) == 0.
@assert round(-.1,0) == -0.
@assert isnan(round(NaN, 2))
@assert isinf(round(Inf,2))
@assert isinf(round(-Inf,2))
# round vs trunc vs floor vs ceil
@assert approx_eq(round(123.456,1), 123.5)
@assert approx_eq(round(-123.456,1), -123.5)
@assert approx_eq(trunc(123.456,1), 123.4)
@assert approx_eq(trunc(-123.456,1), -123.4)
@assert approx_eq(ceil(123.456,1), 123.5)
@assert approx_eq(ceil(-123.456,1), -123.4)
@assert approx_eq(floor(123.456,1), 123.4)
@assert approx_eq(floor(-123.456,1), -123.5)
# rounding in other bases
@assert approx_eq(round(pi,2,2), 3.25)
@assert approx_eq(round(pi,3,2), 3.125)
@assert approx_eq(round(pi,3,5), 3.144)
# significant digits (would be nice to have a smart vectorized
# version of signif)
@assert approx_eq(signif(123.456,1), 100.)
@assert approx_eq(signif(123.456,3), 123.)
@assert approx_eq(signif(123.456,5), 123.46)
@assert approx_eq(signif(123.456,8,2), 123.5)

# issue #1308
@assert hex(~uint128(0)) == "f"^32
@assert uint128(~0) == ~uint128(0)
@assert int128(~0) == ~int128(0)

# issue 1552
@assert isa(convert(Rational{Int8},pi),Rational{Int8})
@assert convert(Rational{Int8},pi) == 22//7
@assert convert(Rational{Int64},0.957762604052997) == 42499549//44373782
@assert convert(Rational{Int16},0.929261477046077) == 11639//12525
@assert convert(Rational{Int16},0.2264705884044309) == 77//340
@assert convert(Rational{Int16},0.39999899264235683) == 2//5
@assert convert(Rational{Int16},1.1264233500618559e-5) == 0//1
@assert convert(Rational{Uint16},1.1264233500618559e-5) == 1//65535
@assert convert(Rational{Uint16},0.6666652791223875) == 2//3
@assert convert(Rational{Int8},0.9374813124660655) == 15//16
@assert convert(Rational{Int8},0.003803032342443835) == 0//1
@assert convert(Rational{Uint8},0.003803032342443835) == 1//255
