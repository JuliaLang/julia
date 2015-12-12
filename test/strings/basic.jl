# This file is a part of Julia. License is MIT: http://julialang.org/license

# {starts,ends}with
@test startswith("abcd", 'a')
@test startswith("abcd", "a")
@test startswith("abcd", "ab")
@test !startswith("ab", "abcd")
@test !startswith("abcd", "bc")
@test endswith("abcd", 'd')
@test endswith("abcd", "d")
@test endswith("abcd", "cd")
@test !endswith("abcd", "dc")
@test !endswith("cd", "abcd")
@test startswith("ab\0cd", "ab\0c")
@test !startswith("ab\0cd", "ab\0d")

@test filter(x -> x ∈ ['f', 'o'], "foobar") == "foo"

# string iteration, and issue #1454
str = "é"
str_a = vcat(str...)
@test length(str_a)==1
@test str_a[1] == str[1]

str = "s\u2200"
@test str[1:end] == str

# sizeof
@test sizeof("abc") == 3
@test sizeof("\u2222") == 3

# issue #3597
@test string(utf32(['T', 'e', 's', 't'])[1:1], "X") == "TX"

for T = (UInt8,Int8,UInt16,Int16,UInt32,Int32,UInt64,Int64,UInt128,Int128,BigInt),
    b = 2:62, _ = 1:10
    n = T != BigInt ? rand(T) : BigInt(rand(Int128))
    @test parse(T,base(b,n),b) == n
end

# issue #6027
let
    # make symbol with invalid char
    sym = symbol(Char(0xdcdb))
    @test string(sym) == string(Char(0xdcdb))
    @test expand(sym) === sym
    res = string(parse(string(Char(0xdcdb)," = 1"),1,raise=false)[1])
    @test res == """\$(Expr(:error, "invalid character \\\"\\udcdb\\\"\"))"""
end

@test symbol("asdf") === :asdf
@test symbol(:abc,"def",'g',"hi",0) === :abcdefghi0
@test :a < :b
@test startswith(string(gensym("asdf")),"##asdf#")
@test gensym("asdf") != gensym("asdf")
@test gensym() != gensym()
@test startswith(string(gensym()),"##")
@test_throws ArgumentError symbol("ab\0")
@test_throws ArgumentError gensym("ab\0")

# issue #6949
let f =IOBuffer(),
    x = split("1 2 3")
    @test write(f, x) == 3
    @test takebuf_string(f) == "123"
    @test invoke(write, Tuple{IO, AbstractArray}, f, x) == 3
    @test takebuf_string(f) == "123"
end

# issue #7248
@test_throws BoundsError ind2chr("hello", -1)
@test_throws BoundsError chr2ind("hello", -1)
@test_throws BoundsError ind2chr("hellø", -1)
@test_throws BoundsError chr2ind("hellø", -1)
@test_throws BoundsError ind2chr("hello", 10)
@test_throws BoundsError chr2ind("hello", 10)
@test_throws BoundsError ind2chr("hellø", 10)
@test_throws BoundsError chr2ind("hellø", 10)
@test_throws BoundsError checkbounds("hello", 0)
@test_throws BoundsError checkbounds("hello", 6)
@test_throws BoundsError checkbounds("hello", 0:3)
@test_throws BoundsError checkbounds("hello", 4:6)
@test_throws BoundsError checkbounds("hello", [0:3;])
@test_throws BoundsError checkbounds("hello", [4:6;])
@test checkbounds("hello", 2)
@test checkbounds("hello", 1:5)
@test checkbounds("hello", [1:5;])

#=
# issue #7764
let
    srep = repeat("Σβ",2)
    s="Σβ"
    ss=SubString(s,1,endof(s))

    @test repeat(ss,2) == "ΣβΣβ"

    @test endof(srep) == 7

    @test next(srep, 3) == ('β',5)
    @test next(srep, 7) == ('β',9)

    @test srep[7] == 'β'
    @test_throws BoundsError srep[8]
end
=#

# This caused JuliaLang/JSON.jl#82
@test first('\x00':'\x7f') === '\x00'
@test last('\x00':'\x7f') === '\x7f'

# make sure substrings handle last code unit even if not start of codepoint
let s = "x\u0302"
    @test s[1:3] == s
end

# issue #9781
# float(SubString) wasn't tolerant of trailing whitespace, which was different
# to "normal" strings. This also checks we aren't being too tolerant and allowing
# any arbitrary trailing characters.
@test parse(Float64,"1\n") == 1.0
@test [parse(Float64,x) for x in split("0,1\n",",")][2] == 1.0
@test_throws ArgumentError parse(Float64,split("0,1 X\n",",")[2])
@test parse(Float32,"1\n") == 1.0
@test [parse(Float32,x) for x in split("0,1\n",",")][2] == 1.0
@test_throws ArgumentError parse(Float32,split("0,1 X\n",",")[2])

@test ucfirst("Hola")=="Hola"
@test ucfirst("hola")=="Hola"
@test ucfirst("")==""
@test ucfirst("*")=="*"

@test lcfirst("Hola")=="hola"
@test lcfirst("hola")=="hola"
@test lcfirst("")==""
@test lcfirst("*")=="*"

#more String tests
@test convert(String, UInt8[32,107,75], "*") == " kK"
@test convert(String, UInt8[132,107,75], "*") == "*kK"
@test convert(String, UInt8[32,107,75], "αβ") == " kK"
@test convert(String, UInt8[132,107,75], "αβ") == "αβkK"
@test convert(String, UInt8[], "*") == ""
@test convert(String, UInt8[255], "αβ") == "αβ"

# test AbstractString functions at beginning of string.jl
immutable tstStringType <: AbstractString
    data::Array{UInt8,1}
end
tstr = tstStringType("12");
@test_throws ErrorException endof(tstr)
@test_throws ErrorException next(tstr, Bool(1))

gstr = GenericString("12");
@test typeof(string(gstr))==GenericString
@test bytestring()==""

@test convert(Array{UInt8}, gstr) ==[49;50]
@test convert(Array{Char,1}, gstr) ==['1';'2']
@test convert(Symbol, gstr)==symbol("12")

@test getindex(gstr, Bool(1))=='1'
@test getindex(gstr,Bool(1):Bool(1))=="1"
@test getindex(gstr,AbstractVector([Bool(1):Bool(1);]))=="1"

@test done(eachindex("foobar"),7)
@test eltype(Base.EachStringIndex) == Int
@test map(uppercase, "foó") == "FOÓ"
@test chr2ind("fóobar",3) == 4

@test symbol(gstr)==symbol("12")

@test_throws ErrorException sizeof(gstr)

@test length(GenericString(""))==0

@test getindex(gstr,AbstractVector([Bool(1):Bool(1);]))=="1"

@test nextind(AbstractArray([Bool(1):Bool(1);]),1)==2

@test ind2chr(gstr,2)==2

# issue #10307
@test typeof(map(Int16,AbstractString[])) == Vector{Int16}

for T in [Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128]
    for i in [typemax(T), typemin(T)]
        s = "$i"
        @test get(tryparse(T, s)) == i
    end
end

for T in [Int8, Int16, Int32, Int64, Int128]
    for i in [typemax(T), typemin(T)]
        f = "$(i)0"
        @test isnull(tryparse(T, f))
    end
end

# issue #11142
s = "abcdefghij"
sp = pointer(s)
@test ascii(sp) == s
@test ascii(sp,5) == "abcde"
@test typeof(ascii(sp)) == String
@test typeof(utf8(sp)) == String
s = "abcde\uff\u2000\U1f596"
sp = pointer(s)
@test utf8(sp) == s
@test utf8(sp,5) == "abcde"
@test typeof(utf8(sp)) == String

@test get(tryparse(BigInt, "1234567890")) == BigInt(1234567890)
@test isnull(tryparse(BigInt, "1234567890-"))

@test get(tryparse(Float64, "64")) == 64.0
@test isnull(tryparse(Float64, "64o"))
@test get(tryparse(Float32, "32")) == 32.0f0
@test isnull(tryparse(Float32, "32o"))

# issue #10994: handle embedded NUL chars for string parsing
for T in [BigInt, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128]
    @test_throws ArgumentError parse(T, "1\0")
end
for T in [BigInt, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, Float64, Float32]
    @test isnull(tryparse(T, "1\0"))
end
let s = normalize_string("tést",:NFKC)
    @test bytestring(Base.unsafe_convert(Cstring, s)) == s
    @test bytestring(convert(Cstring, symbol(s))) == s
    @test wstring(Base.unsafe_convert(Cwstring, wstring(s))) == s
end
let s = "ba\0d"
    @test_throws ArgumentError Base.unsafe_convert(Cstring, s)
    @test_throws ArgumentError Base.unsafe_convert(Cwstring, wstring(s))
end

cstrdup(s) = @windows? ccall(:_strdup, Cstring, (Cstring,), s) : ccall(:strdup, Cstring, (Cstring,), s)
let p = cstrdup("hello")
    @test bytestring(p) == "hello" == pointer_to_string(cstrdup(p), true)
    Libc.free(p)
end
let p = @windows? ccall(:_wcsdup, Cwstring, (Cwstring,), "tést") : ccall(:wcsdup, Cwstring, (Cwstring,), "tést")
    @test wstring(p) == "tést"
    Libc.free(p)
end

# issue # 11389: Vector{UInt32} was copied with UTF32String, unlike Vector{Char}
a = UInt32[48,0]
b = UTF32String(a)
@test b == "0"
a[1] = 65
@test b == "A"
c = Char['0','\0']
d = UTF32String(c)
@test d == "0"
c[1] = 'A'
@test d == "A"

# iteration
@test [c for c in "ḟøøƀäṙ"] == ['ḟ', 'ø', 'ø', 'ƀ', 'ä', 'ṙ']
@test [i for i in eachindex("ḟøøƀäṙ")] == [1, 4, 6, 8, 10, 12]
@test [x for x in enumerate("ḟøøƀäṙ")] == [(1, 'ḟ'), (2, 'ø'), (3, 'ø'), (4, 'ƀ'), (5, 'ä'), (6, 'ṙ')]

# Issue #11140
@test isvalid(utf32("a")) == true
@test isvalid(utf32("\x00")) == true
@test isvalid(UTF32String, UInt32[0xd800,0]) == false

# test all edge conditions
for (val, pass) in (
        (0, true), (0xd7ff, true),
        (0xd800, false), (0xdfff, false),
        (0xe000, true), (0xffff, true),
        (0x10000, true), (0x10ffff, true),
        (0x110000, false)
    )
    @test isvalid(Char, val) == pass
end
for (val, pass) in (
        (b"\x00", true),
        (b"\x7f", true),
        (b"\x80", false),
        (b"\xbf", false),
        (b"\xc0", false),
        (b"\xff", false),
        (b"\xc0\x80", false),
        (b"\xc1\x80", false),
        (b"\xc2\x80", true),
        (b"\xc2\xc0", false),
        (b"\xed\x9f\xbf", true),
        (b"\xed\xa0\x80", false),
        (b"\xed\xbf\xbf", false),
        (b"\xee\x80\x80", true),
        (b"\xef\xbf\xbf", true),
        (b"\xf0\x90\x80\x80", true),
        (b"\xf4\x8f\xbf\xbf", true),
        (b"\xf4\x90\x80\x80", false),
        (b"\xf5\x80\x80\x80", false),
        (b"\ud800\udc00", false),
        (b"\udbff\udfff", false),
        (b"\ud800\u0100", false),
        (b"\udc00\u0100", false),
        (b"\udc00\ud800", false)
        )
    @test isvalid(String, val) == pass
end
for (val, pass) in (
        (UInt16[0x0000], true),
        (UInt16[0xd7ff,0], true),
        (UInt16[0xd800,0], false),
        (UInt16[0xdfff,0], false),
        (UInt16[0xe000,0], true),
        (UInt16[0xffff,0], true),
        (UInt16[0xd800,0xdc00,0], true),
        (UInt16[0xdbff,0xdfff,0], true),
        (UInt16[0xd800,0x0100,0], false),
        (UInt16[0xdc00,0x0100,0], false),
        (UInt16[0xdc00,0xd800,0], false)
        )
    @test isvalid(UTF16String, val) == pass
end
for (val, pass) in (
        (UInt32[0x0000], true),
        (UInt32[0xd7ff,0], true),
        (UInt32[0xd800,0], false),
        (UInt32[0xdfff,0], false),
        (UInt32[0xe000,0], true),
        (UInt32[0xffff,0], true),
        (UInt32[0x100000,0], true),
        (UInt32[0x10ffff,0], true),
        (UInt32[0x110000,0], false),
        )
    @test isvalid(UTF32String, val) == pass
end

# Issue #11203
@test isvalid(String, UInt8[]) == true
@test isvalid(UTF16String,UInt16[]) == true
@test isvalid(UTF32String,UInt32[]) == true

# Check UTF-8 characters
# Check ASCII range (true),
# then single continuation bytes and lead bytes with no following continuation bytes (false)
for (rng,flg) in ((0:0x7f, true), (0x80:0xff, false))
    for byt in rng
        @test isvalid(String, UInt8[byt]) == flg
    end
end
# Check overlong lead bytes for 2-character sequences (false)
for byt = 0xc0:0xc1
    @test isvalid(String, UInt8[byt,0x80]) == false
end
# Check valid lead-in to two-byte sequences (true)
for byt = 0xc2:0xdf
    for (rng,flg) in ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
        for cont in rng
            @test isvalid(String, UInt8[byt, cont]) == flg
        end
    end
end
# Check three-byte sequences
for r1 in (0xe0:0xec, 0xee:0xef)
    for byt = r1
        # Check for short sequence
        @test isvalid(String, UInt8[byt]) == false
        for (rng,flg) in ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
            for cont in rng
                @test isvalid(String, UInt8[byt, cont]) == false
                @test isvalid(String, UInt8[byt, cont, 0x80]) == flg
            end
        end
    end
end
# Check hangul characters (0xd000-0xd7ff) hangul
# Check for short sequence, or start of surrogate pair
for (rng,flg) in ((0x00:0x7f, false), (0x80:0x9f, true), (0xa0:0xff, false))
    for cont in rng
        @test isvalid(String, UInt8[0xed, cont]) == false
        @test isvalid(String, UInt8[0xed, cont, 0x80]) == flg
    end
end
# Check valid four-byte sequences
for byt = 0xf0:0xf4
    if (byt == 0xf0)
        r0 = ((0x00:0x8f, false), (0x90:0xbf, true), (0xc0:0xff, false))
    elseif byt == 0xf4
        r0 = ((0x00:0x7f, false), (0x80:0x8f, true), (0x90:0xff, false))
    else
        r0 = ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
    end
    for (rng,flg) in r0
        for cont in rng
            @test isvalid(String, UInt8[byt, cont]) == false
            @test isvalid(String, UInt8[byt, cont, 0x80]) == false
            @test isvalid(String, UInt8[byt, cont, 0x80, 0x80]) == flg
        end
    end
end
# Check five-byte sequences, should be invalid
for byt = 0xf8:0xfb
    @test isvalid(String, UInt8[byt, 0x80, 0x80, 0x80, 0x80]) == false
end
# Check six-byte sequences, should be invalid
for byt = 0xfc:0xfd
    @test isvalid(String, UInt8[byt, 0x80, 0x80, 0x80, 0x80, 0x80]) == false
end
# Check seven-byte sequences, should be invalid
@test isvalid(String, UInt8[0xfe, 0x80, 0x80, 0x80, 0x80, 0x80]) == false

# 11482

# isvalid
let s = "abcdef", u8 = "abcdef\uff", u16 = utf16(u8), u32 = utf32(u8),
    bad32 = utf32(UInt32[65,0x110000]), badch = Char[0x110000][1]

    @test !isvalid(bad32)
    @test !isvalid(badch)
    @test isvalid(s)
    @test isvalid(u8)
    @test isvalid(u16)
    @test isvalid(u32)
    @test isvalid(String,  u8)
    @test isvalid(UTF16String, u16)
    @test isvalid(UTF32String, u32)
end

# lower and upper
@test uppercase("aBc") == "ABC"
@test uppercase('A') == 'A'
@test uppercase('a') == 'A'
@test lowercase("AbC") == "abc"
@test lowercase('A') == 'a'
@test lowercase('a') == 'a'
@test uppercase('α') == '\u0391'
@test lowercase('Δ') == 'δ'
@test lowercase('\U118bf') == '\U118df'
@test uppercase('\U1044d') == '\U10425'
@test ucfirst("Abc") == "Abc"
@test ucfirst("abc") == "Abc"
@test lcfirst("ABC") == "aBC"
@test lcfirst("aBC") == "aBC"
@test ucfirst(utf32("")) == ""
@test lcfirst(utf32("")) == ""
@test ucfirst(utf32("a")) == "A"
@test lcfirst(utf32("A")) == "a"
@test lcfirst(utf32("a")) == "a"
@test ucfirst(utf32("A")) == "A"

# issue # 11464: uppercase/lowercase of UTF16String becomes a String
str = "abcdef\uff\uffff\u10ffffABCDEF"
@test typeof(uppercase("abcdef")) == String
@test typeof(uppercase(utf8(str))) == String
@test typeof(uppercase(utf16(str))) == UTF16String
@test typeof(uppercase(utf32(str))) == UTF32String
@test typeof(lowercase("ABCDEF")) == String
@test typeof(lowercase(utf8(str))) == String
@test typeof(lowercase(utf16(str))) == UTF16String
@test typeof(lowercase(utf32(str))) == UTF32String

foomap(ch) = (ch > 65)
foobar(ch) = Char(0xd800)
foobaz(ch) = reinterpret(Char, typemax(UInt32))
@test_throws UnicodeError map(foomap, utf16(str))
@test_throws UnicodeError map(foobar, utf16(str))
@test_throws UnicodeError map(foobaz, utf16(str))

@test "a".*["b","c"] == ["ab","ac"]
@test ["b","c"].*"a" == ["ba","ca"]
@test utf8("a").*["b","c"] == ["ab","ac"]
@test "a".*map(utf8,["b","c"]) == ["ab","ac"]
@test ["a","b"].*["c","d"]' == ["ac" "ad"; "bc" "bd"]
