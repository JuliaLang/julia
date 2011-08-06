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
@assert bool(ComplexPair(0,1)) == true
@assert bool(0//1) == false
@assert bool(1//1) == true
@assert bool(1//2) == true

# basic type relationships
@assert Int8 <: Int
@assert Int32 <: Int
@assert (Int8,Int8) <: (Int,Int)
@assert !(AbstractArray{Float64,2} <: AbstractArray{Number,2})
@assert !(AbstractArray{Float64,1} <: AbstractArray{Float64,2})
@assert (Int,Int...) <: (Int,Real...)
@assert (Int,Float64,Int...) <: (Int,Number...)
@assert (Int,Float64) <: (Int,Number...)
@assert (Int32,) <: (Number...)
@assert () <: (Number...)
@assert !((Int32...) <: (Int32,))
@assert !((Int32...) <: (Number,Int))
@assert !((Int...,) <: (Int,Int,Int...))
@assert !(Array{Int8,1} <: Array{Any,1})
@assert !(Array{Any,1} <: Array{Int8,1})
@assert Array{Int8,1} <: Array{Int8,1}

@assert !isa(Array,Type{Any})
@assert subtype(Type{ComplexPair},CompositeKind)
@assert isa(ComplexPair,Type{ComplexPair})
@assert subtype(Type{Ptr{None}},Type{Ptr})
let T = typevar(:T)
    @assert !is(None, tintersect(Array{None},AbstractArray{T}))
end
@assert is(None, tintersect(Type{Any},Type{ComplexPair}))
@assert is(None, tintersect(Type{Any},Type{typevar(:T,Real)}))
@assert !subtype(Type{Array{Int}},Type{AbstractArray{Int}})
@assert subtype(Type{Array{Int}},Type{Array{typevar(:T,Int)}})

# ntuples
nttest1{n}(x::NTuple{n,Size}) = n
@assert nttest1(()) == 0
@assert nttest1((1,2)) == 2
@assert NTuple <: Tuple
@assert NTuple{typevar(:T),Int32} <: (Int32...)
@assert !(NTuple{typevar(:T),Int32} <: (Int32,Int32...))
@assert (Int32...) <: NTuple{typevar(:T),Int32}
@assert (Int32,Int32...) <: NTuple{typevar(:T),Int32}

# basic arithmetic and indexing
@assert 2+3 == 5
@assert 2.+3. == 5.
@assert 2*3 == 6
@assert 2.*3 == 6
@assert 2. * 3. == 6.
@assert min(1.0,1) == 1

a = ones(4)
b = a+a
@assert b[1]==2. && b[2]==2. && b[3]==2. && b[4]==2.

@assert length((1,)) == 1
@assert length((1,2)) == 2

@assert 1+[1,2,3] == [2,3,4]
@assert [1,2,3]+1 == [2,3,4]
@assert 1-[1,2,3] == [0,-1,-2]
@assert [1,2,3]-1 == [0,1,2]

@assert 5*[1,2,3] == [5,10,15]
@assert [1,2,3]*5 == [5,10,15]
@assert 1/[1,2,5] == [1.0,0.5,0.2]
@assert [1,2,3]/5 == [0.2,0.4,0.6]

a = ones(2,2)
a[1,1] = 1
a[1,2] = 2
a[2,1] = 3
a[2,2] = 4
b = a'
assert(a[1,1] == 1. && a[1,2] == 2. &&
       a[2,1] == 3. && a[2,2] == 4.)
assert(b[1,1] == 1. && b[2,1] == 2. &&
       b[1,2] == 3. && b[2,2] == 4.)

x = (2,3)
@assert +(x...) == 5

a = rand()
b = rand()
@assert a != b

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
@assert sign(0//0) == 0
@assert sign(-0//0) == 0
@assert sign(-1//0) == -1

@assert signbit(1) == 1
@assert signbit(-1) == -1
@assert signbit(0) == 1
@assert signbit(1.0) == 1
@assert signbit(-1.0) == -1
@assert signbit(0.0) == 1
@assert signbit(-0.0) == -1
@assert signbit(1.0/0.0) == 1
@assert signbit(-1.0/0.0) == -1
@assert signbit(Inf) == 1
@assert signbit(-Inf) == -1
@assert signbit(NaN) == 1
@assert signbit(-NaN) == -1
@assert signbit(2//3) == 1
@assert signbit(-2//3) == -1
@assert signbit(0//1) == 1
@assert signbit(-0//1) == 1
@assert signbit(1//0) == 1
@assert signbit(0//0) == 1
@assert signbit(-0//0) == 1
@assert signbit(-1//0) == -1

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
@assert isnan(0//0)  == true

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
@assert isinf(0//0)  == false

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
@assert isfinite(0//0)  == false

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
    @assert isequal(a/b, a/b)
    @assert (a//b == a/b) || (isnan(a//b) && isnan(a/b))
    @assert isequal(a//b, a//b)
    for c = -5:5, d = -5:5
        @assert isequal(a//b, c//d) == isequal(a/b, c/d)
        @assert (a//b == c//d) == (a/b == c/d)
        @assert (a//b != c//d) == (a/b != c/d)
        @assert (a//b <= c//d) == (a/b <= c/d)
        @assert (a//b <  c//d) == (a/b <  c/d)
        @assert (a//b >= c//d) == (a/b >= c/d)
        @assert (a//b >  c//d) == (a/b >  c/d)
    end
end

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

# div, fld, rem, mod
for nr = {
    Range1(1,6),
    Range(0.25,0.25,6.0),
    Range(1//4,1//4,6//1)
}, ar = {
    Range1(0,6),
    Range(0.0,0.25,6.0),
    Range(0//1,1//4,6//1)
}
    for n = nr, a = ar
        # check basic div functionality
        if 0n <= a < 1n
            @assert div(+a,+n) == 0
            @assert div(+a,-n) == 0
            @assert div(-a,+n) == 0
            @assert div(-a,-n) == 0
        end
        if 1n <= a < 2n
            @assert div(+a,+n) == +1
            @assert div(+a,-n) == -1
            @assert div(-a,+n) == -1
            @assert div(-a,-n) == +1
        end
        if 2n <= a < 3n
            @assert div(+a,+n) == +2
            @assert div(+a,-n) == -2
            @assert div(-a,+n) == -2
            @assert div(-a,-n) == +2
        end

        # check basic fld functionality
        if 0n == a
            @assert fld(+a,+n) == 0
            @assert fld(+a,-n) == 0
            @assert fld(-a,+n) == 0
            @assert fld(-a,-n) == 0
        end
        if 0n < a < 1n
            @assert fld(+a,+n) == +0
            @assert fld(+a,-n) == -1
            @assert fld(-a,+n) == -1
            @assert fld(-a,-n) == +0
        end
        if 1n == a
            @assert fld(+a,+n) == +1
            @assert fld(+a,-n) == -1
            @assert fld(-a,+n) == -1
            @assert fld(-a,-n) == +1
        end
        if 1n < a < 2n
            @assert fld(+a,+n) == +1
            @assert fld(+a,-n) == -2
            @assert fld(-a,+n) == -2
            @assert fld(-a,-n) == +1
        end
        if 2n == a
            @assert fld(+a,+n) == +2
            @assert fld(+a,-n) == -2
            @assert fld(-a,+n) == -2
            @assert fld(-a,-n) == +2
        end
        if 2n < a < 3n
            @assert fld(+a,+n) == +2
            @assert fld(+a,-n) == -3
            @assert fld(-a,+n) == -3
            @assert fld(-a,-n) == +2
        end

        # check everything else in terms of div & fld
        d = div(a,n)
        f = fld(a,n)
        r = rem(a,n)
        m = mod(a,n)

        t1 = isa(a,Rational) && isa(n,Rational) ?
                               promote_type(typeof(num(a)),typeof(num(n))) :
             isa(a,Rational) ? promote_type(typeof(num(a)),typeof(n)) :
             isa(n,Rational) ? promote_type(typeof(a),typeof(num(n))) :
                               promote_type(typeof(a),typeof(n))

        t2 = promote_type(typeof(a),typeof(n))

        @assert typeof(d) <: t1
        @assert typeof(f) <: t1
        @assert typeof(r) <: t2
        @assert typeof(m) <: t2

        @assert d == f
        @assert r == m
        @assert 0 <= r < n
        @assert a == n*d + r

        for A=[-1,1], N=[-1,1]
            sa = A*a
            sn = N*n

            sd = div(sa,sn)
            sf = fld(sa,sn)
            sr = rem(sa,sn)
            sm = mod(sa,sn)

            @assert typeof(sd) <: t1
            @assert typeof(sf) <: t1
            @assert typeof(sr) <: t2
            @assert typeof(sm) <: t2

            @assert sa < 0 ? -n < sr <= 0 : 0 <= sr < +n
            @assert sn < 0 ? -n < sm <= 0 : 0 <= sm < +n
            @assert sa == sn*sd + sr
            @assert sa == sn*sf + sm
        end
    end
end

# arrays as dequeues
l = {1,2,3}
push(l,8)
@assert l[1]==1 && l[2]==2 && l[3]==3 && l[4]==8
v = pop(l)
@assert v == 8
v = pop(l)
@assert v == 3
@assert length(l)==2

# string escaping & unescaping
cx = {
    0x00000000      '\0'        "\\0"
    0x00000001      '\x01'      "\\x01"
    0x00000006      '\x06'      "\\x06"
    0x00000007      '\a'        "\\a"
    0x00000008      '\b'        "\\b"
    0x00000009      '\t'        "\\t"
    0x0000000a      '\n'        "\\n"
    0x0000000b      '\v'        "\\v"
    0x0000000c      '\f'        "\\f"
    0x0000000d      '\r'        "\\r"
    0x0000000e      '\x0e'      "\\x0e"
    0x0000001a      '\x1a'      "\\x1a"
    0x0000001b      '\e'        "\\e"
    0x0000001c      '\x1c'      "\\x1c"
    0x0000001f      '\x1f'      "\\x1f"
    0x00000020      ' '         " "
    0x0000002f      '/'         "/"
    0x00000030      '0'         "0"
    0x00000039      '9'         "9"
    0x0000003a      ':'         ":"
    0x00000040      '@'         "@"
    0x00000041      'A'         "A"
    0x0000005a      'Z'         "Z"
    0x0000005b      '['         "["
    0x00000060      '`'         "`"
    0x00000061      'a'         "a"
    0x0000007a      'z'         "z"
    0x0000007b      '{'         "{"
    0x0000007e      '~'         "~"
    0x0000007f      '\x7f'      "\\x7f"
    0x000000bf      '\ubf'      "\\ubf"
    0x000000ff      '\uff'      "\\uff"
    0x00000100      '\u100'     "\\u100"
    0x000001ff      '\u1ff'     "\\u1ff"
    0x00000fff      '\ufff'     "\\ufff"
    0x00001000      '\u1000'    "\\u1000"
    0x00001fff      '\u1fff'    "\\u1fff"
    0x0000ffff      '\uffff'    "\\uffff"
    0x00010000      '\U10000'   "\\U10000"
    0x0001ffff      '\U1ffff'   "\\U1ffff"
    0x0002ffff      '\U2ffff'   "\\U2ffff"
    0x00030000      '\U30000'   "\\U30000"
    0x000dffff      '\Udffff'   "\\Udffff"
    0x000e0000      '\Ue0000'   "\\Ue0000"
    0x000effff      '\Ueffff'   "\\Ueffff"
    0x000f0000      '\Uf0000'   "\\Uf0000"
    0x000fffff      '\Ufffff'   "\\Ufffff"
    0x00100000      '\U100000'  "\\U100000"
    0x0010ffff      '\U10ffff'  "\\U10ffff"
}

for i = 1:size(cx,1)
    @assert cx[i,1] == cx[i,2]
    @assert string(cx[i,2]) == unescape_string(cx[i,3])
    if iswascii(cx[i,2]) || !iswprint(cx[i,2])
        @assert cx[i,3] == escape_string(string(cx[i,2]))
    end
    for j = 1:size(cx,1)
        str = string(cx[i,2], cx[j,2])
        @assert str == unescape_string(escape_string(str))
    end
end

for i = 0:0x7f, p = {"","\0","x","xxx","\x7f","\uFF","\uFFF",
                     "\uFFFF","\U10000","\U10FFF","\U10FFFF"}
    c = char(i)
    cp = strcat(c,p)
    op = strcat(char(div(i,8)), oct(i%8), p)
    hp = strcat(char(div(i,16)), hex(i%16), p)
    @assert strcat(unescape_string(strcat("\\",oct(i,1),p))) == cp
    @assert strcat(unescape_string(strcat("\\",oct(i,2),p))) == cp
    @assert strcat(unescape_string(strcat("\\",oct(i,3),p))) == cp
    @assert strcat(unescape_string(strcat("\\",oct(i,4),p))) == op
    @assert strcat(unescape_string(strcat("\\x",hex(i,1),p))) == cp
    @assert strcat(unescape_string(strcat("\\x",hex(i,2),p))) == cp
    @assert strcat(unescape_string(strcat("\\x",hex(i,3),p))) == hp
end

@assert "\z" == unescape_string("\z") == "z"
@assert "\X" == unescape_string("\X") == "X"
@assert "\AbC" == unescape_string("\AbC") == "AbC"

@assert "\0" == unescape_string("\\0")
@assert "\1" == unescape_string("\\1")
@assert "\7" == unescape_string("\\7")
@assert "\0x" == unescape_string("\\0x")
@assert "\1x" == unescape_string("\\1x")
@assert "\7x" == unescape_string("\\7x")
@assert "\00" == unescape_string("\\00")
@assert "\01" == unescape_string("\\01")
@assert "\07" == unescape_string("\\07")
@assert "\70" == unescape_string("\\70")
@assert "\71" == unescape_string("\\71")
@assert "\77" == unescape_string("\\77")
@assert "\00x" == unescape_string("\\00x")
@assert "\01x" == unescape_string("\\01x")
@assert "\07x" == unescape_string("\\07x")
@assert "\70x" == unescape_string("\\70x")
@assert "\71x" == unescape_string("\\71x")
@assert "\77x" == unescape_string("\\77x")
@assert "\000" == unescape_string("\\000")
@assert "\001" == unescape_string("\\001")
@assert "\007" == unescape_string("\\007")
@assert "\070" == unescape_string("\\070")
@assert "\071" == unescape_string("\\071")
@assert "\077" == unescape_string("\\077")
@assert "\170" == unescape_string("\\170")
@assert "\171" == unescape_string("\\171")
@assert "\177" == unescape_string("\\177")
# @assert "\270" == unescape_string("\\270")
# @assert "\271" == unescape_string("\\271")
# @assert "\277" == unescape_string("\\277")
# @assert "\370" == unescape_string("\\370")
# @assert "\371" == unescape_string("\\371")
# @assert "\377" == unescape_string("\\377")
@assert "\0001" == unescape_string("\\0001")
@assert "\0011" == unescape_string("\\0011")
@assert "\0071" == unescape_string("\\0071")
@assert "\0701" == unescape_string("\\0701")
@assert "\0711" == unescape_string("\\0711")
@assert "\0771" == unescape_string("\\0771")
@assert "\1701" == unescape_string("\\1701")
@assert "\1711" == unescape_string("\\1711")
@assert "\1771" == unescape_string("\\1771")
# @assert "\2701" == unescape_string("\\2701")
# @assert "\2711" == unescape_string("\\2711")
# @assert "\2771" == unescape_string("\\2771")
# @assert "\3701" == unescape_string("\\3701")
# @assert "\3711" == unescape_string("\\3711")
# @assert "\3771" == unescape_string("\\3771")

@assert "\x0" == unescape_string("\\x0")
@assert "\x1" == unescape_string("\\x1")
@assert "\xf" == unescape_string("\\xf")
@assert "\xF" == unescape_string("\\xF")
@assert "\x0x" == unescape_string("\\x0x")
@assert "\x1x" == unescape_string("\\x1x")
@assert "\xfx" == unescape_string("\\xfx")
@assert "\xFx" == unescape_string("\\xFx")
@assert "\x00" == unescape_string("\\x00")
@assert "\x01" == unescape_string("\\x01")
@assert "\x0f" == unescape_string("\\x0f")
@assert "\x0F" == unescape_string("\\x0F")
# @assert "\xf0" == unescape_string("\\xf0")
# @assert "\xf1" == unescape_string("\\xf1")
# @assert "\xff" == unescape_string("\\xff")
# @assert "\xfF" == unescape_string("\\xfF")
# @assert "\xf0a" == unescape_string("\\xf0a")
# @assert "\xf1a" == unescape_string("\\xf1a")
# @assert "\xffa" == unescape_string("\\xffa")
# @assert "\xfFa" == unescape_string("\\xfFa")

# TODO: more Unicode testing here.

@assert S"foo\xe2\x88\x80" == "foo\xe2\x88\x80"

# TODO: the above is only one of many needed tests

# integer parsing
@assert parse_int(Int32,"0",36) == 0
@assert parse_int(Int32,"1",36) == 1
@assert parse_int(Int32,"9",36) == 9
@assert parse_int(Int32,"A",36) == 10
@assert parse_int(Int32,"a",36) == 10
@assert parse_int(Int32,"B",36) == 11
@assert parse_int(Int32,"b",36) == 11
@assert parse_int(Int32,"F",36) == 15
@assert parse_int(Int32,"f",36) == 15
@assert parse_int(Int32,"Z",36) == 35
@assert parse_int(Int32,"z",36) == 35

@assert parse_bin("0") == 0
@assert parse_bin("1") == 1
@assert parse_bin("10") == 2
@assert parse_bin("11") == 3
@assert parse_bin("1111000011110000111100001111") == 252645135

@assert parse_oct("0") == 0
@assert parse_oct("1") == 1
@assert parse_oct("7") == 7
@assert parse_oct("10") == 8
@assert parse_oct("11") == 9
@assert parse_oct("72") == 58
@assert parse_oct("3172207320") == 434704080

@assert parse_dec("0") == 0
@assert parse_dec("1") == 1
@assert parse_dec("9") == 9
@assert parse_dec("10") == 10
@assert parse_dec("3830974272") == 3830974272

@assert parse_hex("0") == 0
@assert parse_hex("1") == 1
@assert parse_hex("9") == 9
@assert parse_hex("a") == 10
@assert parse_hex("f") == 15
@assert parse_hex("10") == 16
@assert parse_hex("0BADF00D") == 195948557
@assert parse_hex("BADCAB1E") == 3135023902
@assert parse_hex("CafeBabe") == 3405691582
@assert parse_hex("DeadBeef") == 3735928559

# bits types, printing numbers
@assert string(uint32(-1)) == "4294967295"
if WORD_SIZE == 64
    @assert isa((()->box(Ptr{Int8},unbox64(int64(0))))(), Ptr{Int8})
else
    @assert isa((()->box(Ptr{Int8},unbox32(int32(0))))(), Ptr{Int8})
end
@assert isa((()->box(Char,unbox32(int32(65))))(), Char)

# conversions
function fooo()
    local x::Int8
    x = 1000
    x
end
@assert int32(fooo()) == -24
function foo()
    local x::Int8
    function bar()
        x = 1000
    end
    bar()
    x
end
@assert int32(foo()) == -24

function bar{T}(x::T)
    local z::ComplexPair{T}
    z = x
    z
end
@assert bar(3.0) == ComplexPair(3.0,0.0)

z = convert(ComplexPair{Float64},2)
@assert z == ComplexPair(2.0,0.0)

# misc
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
@assert fib(20) == 6765

# static parameters
sptest1{T}(x::T, y::T) = 42
sptest1{T,S}(x::T, y::S) = 43
@assert sptest1(1,2) == 42
@assert sptest1(1,"b") == 43

sptest2{T}(x::T) = T
@assert is(sptest2(:a),Symbol)

sptest3{T}(x::T) = y->T
let m = sptest3(:a)
    @assert is(m(0),Symbol)
end

# closures
function clotest()
    c = 0
    function inc()
        c += 1
    end
    function dec()
        c -= 1
    end
    inc(); inc()
    @assert c == 2
    dec()
    @assert c == 1
    @assert (()->c)() == 1
    return (n->(c+=n), ()->c)
end
let T = clotest()
    (inc, C) = T
    inc(11)
    @assert C() == 12
end

Yc(f) = (h->f(x->h(h)(x)))(h->f(x->h(h)(x)))
yfib = Yc(fib->(n->(n < 2 ? n : fib(n-1) + fib(n-2))))
@assert yfib(20) == 6765

@assert map((x,y)->x+y,(1,2,3),(4,5,6)) == (5,7,9)
@assert map((x,y)->x+y,
            (100001,100002,100003),
            (100004,100005,100006)) == (200005,200007,200009)

# variable scope, globals
glob_x = 23
function glotest()
    global glob_x
    glob_x = 24
    loc_x = 8
    function inner()
        global loc_x = 10
        glob_x = 88
    end
    function inner2()
        local glob_x  # override
        global loc_x
        glob_x = 2
        @assert glob_x == 2
        @assert loc_x == 10
    end
    inner()
    inner2()
    @assert glob_x == 88
    @assert loc_x == 8
end
glotest()
@assert glob_x == 88
@assert loc_x == 10

# ranges
@assert size(10:1:0) == (0,)
@assert length(1:.2:2) == 6
@assert length(Range(1.,.2,2.)) == 6
@assert length(2:-.2:1) == 6
@assert length(Range(2.,-.2,1.)) == 6
@assert length(2:.2:1) == 0
@assert length(Range(2.,.2,1.)) == 0

# comprehensions
X = [ i+2j | i=1:5, j=1:5 ]
@assert X[2,3] == 8
@assert X[4,5] == 14
@assert ones(2,3) * ones(2,3)' == [3. 3.; 3. 3.]
@assert [ [1,2] | i=1:2, : ] == [1 2; 1 2]
# where element type is a Union. try to confuse type inference.
foo32_64(x) = (x<2) ? int32(x) : int64(x)
boo32_64() = [ foo32_64(i) | i=1:2 ]
let a36 = boo32_64()
    @assert a36[1]==1 && a36[2]==2
end

# concatenation
@assert [ones(2,2)  2*ones(2,1)] == [1 1 2; 1 1 2]
@assert [ones(2,2), 2*ones(1,2)] == [1 1; 1 1; 2 2]

# "end"
X = [ i+2j | i=1:5, j=1:5 ]
@assert X[end,end] == 15
@assert X[end]     == 15  # linear index
@assert X[2,  end] == 12
@assert X[end,  2] == 9
@assert X[end-1,2] == 8
Y = [2, 1, 4, 3]
@assert X[Y[end],1] == 5
@assert X[end,Y[end]] == 11

# syntax
@assert (true ? 1 : false ? 2 : 3) == 1

# blas, lapack
n = 10
a = rand(n,n)
asym = a+a'+n*eye(n)
b = rand(n)
r = chol(asym)
@assert sum(r'*r - asym) < 1e-8
(l,u,p) = lu(a)
@assert sum(l[p,:]*u - a) < 1e-8
(q,r,p) = qr(a)
@assert sum(q*r[:,p] - a) < 1e-8
(d,v) = eig(asym)
@assert sum(asym*v[:,1]-d[1]*v[:,1]) < 1e-8
(u,s,vt) = svd(a)
@assert sum(u*s*vt - a) < 1e-8
x = a \ b
@assert sum(a*x-b) < 1e-8
x = triu(a) \ b
@assert sum(triu(a)*x-b) < 1e-8
x = tril(a) \ b
@assert sum(tril(a)*x-b) < 1e-8

# arpack
(d,v) = eigs(asym, 3)
@assert sum(asym*v[:,1]-d[1,1]*v[:,1]) < 1e-8

(d,v) = eigs(a,3)
@assert abs(sum(a*v[:,2]-d[2,2]*v[:,2])) < 1e-8

# hash table
h = HashTable()
for i=1:10000
    h[i] = i+1
end
for i=1:10000
    @assert (h[i] == i+1)
end
for i=1:2:10000
    del(h, i)
end
for i=1:2:10000
    h[i] = i+1
end
for i=1:10000
    @assert (h[i] == i+1)
end
for i=1:10000
    del(h, i)
end
@assert isempty(h)
h[77] = 100
@assert h[77]==100
for i=1:10000
    h[i] = i+1
end
for i=1:2:10000
    del(h, i)
end
for i=10001:20000
    h[i] = i+1
end
for i=2:2:10000
    @assert h[i]==i+1
end
for i=10000:20000
    @assert h[i]==i+1
end

# fft
a = rand(8) + im*rand(8)
@assert norm((1/length(a))*ifft(fft(a)) - a) < 1e-8

