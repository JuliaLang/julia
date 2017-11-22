# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "constructors" begin
    @test String([0x61,0x62,0x63,0x21]) == "abc!"
    @test String("abc!") == "abc!"

    @test isempty(string())
    @test eltype(GenericString) == Char
    @test start("abc") == 1
    @test cmp("ab","abc") == -1
    @test "abc" === "abc"
    @test "ab"  !== "abc"
    @test string("ab", 'c') === "abc"
    @test string() === ""
    codegen_egal_of_strings(x, y) = (x===y, x!==y)
    @test codegen_egal_of_strings(string("ab", 'c'), "abc") === (true, false)
    let strs = ["", "a", "a b c", "до свидания"]
        for x in strs, y in strs
            @test (x === y) == (object_id(x) == object_id(y))
        end
    end
end

@testset "{starts,ends}with" begin
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
end

@test filter(x -> x ∈ ['f', 'o'], "foobar") == "foo"

@testset "string iteration, and issue #1454" begin
    str = "é"
    str_a = vcat(str...)
    @test length(str_a)==1
    @test str_a[1] == str[1]

    str = "s\u2200"
    @test str[1:end] == str
end

@testset "sizeof" begin
    @test sizeof("abc") == 3
    @test sizeof("\u2222") == 3
end

# issue #3597
@test string(GenericString("Test")[1:1], "X") == "TX"

@testset "parsing Int types" begin
    let b, n
    for T = (UInt8,Int8,UInt16,Int16,UInt32,Int32,UInt64,Int64,UInt128,Int128,BigInt),
            b = 2:62,
            _ = 1:10
        n = (T != BigInt) ? rand(T) : BigInt(rand(Int128))
        @test parse(T, base(b, n), b) == n
    end
    end
end

@testset "issue #6027 - make symbol with invalid char" begin
    sym = Symbol(Char(0xdcdb))
    @test string(sym) == string(Char(0xdcdb))
    @test String(sym) == string(Char(0xdcdb))
    @test Meta.lower(Main, sym) === sym
    res = string(Meta.parse(string(Char(0xdcdb)," = 1"),1,raise=false)[1])
    @test res == """\$(Expr(:error, "invalid character \\\"\\udcdb\\\"\"))"""
end

@testset "Symbol and gensym" begin
    @test Symbol("asdf") === :asdf
    @test Symbol(:abc,"def",'g',"hi",0) === :abcdefghi0
    @test :a < :b
    @test startswith(string(gensym("asdf")),"##asdf#")
    @test gensym("asdf") != gensym("asdf")
    @test gensym() != gensym()
    @test startswith(string(gensym()),"##")
    @test_throws ArgumentError Symbol("ab\0")
    @test_throws ArgumentError gensym("ab\0")
end
@testset "issue #6949" begin
    f = IOBuffer()
    x = split("1 2 3")
    local nb = 0
    for c in x
        nb += write(f, c)
    end
    @test nb == 3
    @test String(take!(f)) == "123"
end

@testset "issue #7248" begin
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
end

@testset "issue #15624 (indexing with out of bounds empty range)" begin
    @test ""[10:9] == ""
    @test "hello"[10:9] == ""
    @test "hellø"[10:9] == ""
    @test SubString("hello", 1, 5)[10:9] == ""
    @test SubString("hello", 1, 0)[10:9] == ""
    @test SubString("hellø", 1, 5)[10:9] == ""
    @test SubString("hellø", 1, 0)[10:9] == ""
    @test SubString("", 1, 0)[10:9] == ""

    @test_throws BoundsError SubString("", 1, 6)
    @test_throws BoundsError SubString("", 1, 1)
end

@testset "issue #22500 (using `get()` to index strings with default returns)" begin
    utf8_str = "我很喜欢Julia"

    # Test that we can index in at valid locations
    @test get(utf8_str, 1, 'X') == '我'
    @test get(utf8_str, 13, 'X') == 'J'

    # Test that obviously incorrect locations return the default
    @test get(utf8_str, -1, 'X') == 'X'
    @test get(utf8_str, 1000, 'X') == 'X'

    # Test that indexing into the middle of a character returns the default
    @test get(utf8_str, 2, 'X') == 'X'
end

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

# make sure substrings do not accept code unit if it is not start of codepoint
let s = "x\u0302"
    @test_throws UnicodeError s[1:3]
    @test s[1:2]==s
end

@testset "issue #9781" begin
    # parse(Float64, SubString) wasn't tolerant of trailing whitespace, which was different
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
    @test ucfirst("Ǆxx") == ucfirst("ǆxx") == "ǅxx"

    @test lcfirst("Hola")=="hola"
    @test lcfirst("hola")=="hola"
    @test lcfirst("")==""
    @test lcfirst("*")=="*"
end
# test AbstractString functions at beginning of string.jl
struct tstStringType <: AbstractString
    data::Array{UInt8,1}
end
@testset "AbstractString functions" begin
    tstr = tstStringType(Vector{UInt8}("12"))
    @test_throws ErrorException endof(tstr)
    @test_throws ErrorException next(tstr, Bool(1))

    gstr = GenericString("12")
    @test string(gstr) isa GenericString

    @test Array{UInt8}(gstr) == [49, 50]
    @test Array{Char,1}(gstr) == ['1', '2']

    @test gstr[1] == '1'
    @test gstr[1:1] == "1"
    @test gstr[[1]] == "1"

    @test s"∀∃"[big(1)] == '∀'
    @test_throws UnicodeError GenericString("∀∃")[Int8(2)]
    @test_throws BoundsError GenericString("∀∃")[UInt16(10)]

    @test done(eachindex("foobar"),7)
    @test eltype(Base.EachStringIndex) == Int
    @test map(uppercase, "foó") == "FOÓ"
    @test chr2ind("fóobar",3) == 4

    @test Symbol(gstr)==Symbol("12")

    @test_throws ErrorException sizeof(gstr)

    @test length(GenericString(""))==0

    @test nextind(1:1, 1) == 2
    @test nextind([1], 1) == 2

    @test ind2chr(gstr,2)==2

    # tests promote_rule
    let svec = [s"12", GenericString("12"), SubString("123", 1, 2)]
        @test all(x -> x === "12", svec)
    end
end

@testset "issue #10307" begin
    @test typeof(map(x -> parse(Int16, x), AbstractString[])) == Vector{Int16}

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
end

@testset "issue #11142" begin
    s = "abcdefghij"
    sp = pointer(s)
    @test unsafe_string(sp) == s
    @test unsafe_string(sp,5) == "abcde"
    @test typeof(unsafe_string(sp)) == String
    s = "abcde\uff\u2000\U1f596"
    sp = pointer(s)
    @test unsafe_string(sp) == s
    @test unsafe_string(sp,5) == "abcde"
    @test typeof(unsafe_string(sp)) == String

    @test get(tryparse(BigInt, "1234567890")) == BigInt(1234567890)
    @test isnull(tryparse(BigInt, "1234567890-"))

    @test get(tryparse(Float64, "64")) == 64.0
    @test isnull(tryparse(Float64, "64o"))
    @test get(tryparse(Float32, "32")) == 32.0f0
    @test isnull(tryparse(Float32, "32o"))
end

@testset "issue #10994: handle embedded NUL chars for string parsing" begin
    for T in [BigInt, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128]
        @test_throws ArgumentError parse(T, "1\0")
    end
    for T in [BigInt, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, Float64, Float32]
        @test isnull(tryparse(T, "1\0"))
    end
    let s = normalize_string("tést",:NFKC)
        @test unsafe_string(Base.unsafe_convert(Cstring, Base.cconvert(Cstring, s))) == s
        @test unsafe_string(convert(Cstring, Symbol(s))) == s
    end
    @test_throws ArgumentError Base.unsafe_convert(Cstring, Base.cconvert(Cstring, "ba\0d"))

    cstrdup(s) = @static Sys.iswindows() ? ccall(:_strdup, Cstring, (Cstring,), s) : ccall(:strdup, Cstring, (Cstring,), s)
    let p = cstrdup("hello")
        @test unsafe_string(p) == "hello"
        Libc.free(p)
    end
end

@testset "iteration" begin
    @test [c for c in "ḟøøƀäṙ"] == ['ḟ', 'ø', 'ø', 'ƀ', 'ä', 'ṙ']
    @test [i for i in eachindex("ḟøøƀäṙ")] == [1, 4, 6, 8, 10, 12]
    @test [x for x in enumerate("ḟøøƀäṙ")] == [(1, 'ḟ'), (2, 'ø'), (3, 'ø'), (4, 'ƀ'), (5, 'ä'), (6, 'ṙ')]
end
@testset "isvalid edge conditions" begin
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
        @test isvalid(String, val) == pass == isvalid(String(val))
    end

    # Issue #11203
    @test isvalid(String, UInt8[]) == true == isvalid("")

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
end

@testset "issue #11482" begin
    @testset "uppercase/lowercase" begin
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
    end
    @testset "ucfirst/lcfirst" begin
        @test ucfirst("Abc") == "Abc"
        @test ucfirst("abc") == "Abc"
        @test lcfirst("ABC") == "aBC"
        @test lcfirst("aBC") == "aBC"
        @test ucfirst(GenericString("")) == ""
        @test lcfirst(GenericString("")) == ""
        @test ucfirst(GenericString("a")) == "A"
        @test lcfirst(GenericString("A")) == "a"
        @test lcfirst(GenericString("a")) == "a"
        @test ucfirst(GenericString("A")) == "A"
    end
    @testset "titlecase" begin
        @test titlecase('ǉ') == 'ǈ'
        @test titlecase("ǉubljana") == "ǈubljana"
        @test titlecase("aBc ABC") == "ABc ABC"
        @test titlecase("abcD   EFG\n\thij") == "AbcD   EFG\n\tHij"
    end
end

@testset "issue # 11464: uppercase/lowercase of GenericString becomes a String" begin
    str = "abcdef\uff\uffff\u10ffffABCDEF"
    @test typeof(uppercase("abcdef")) == String
    @test typeof(uppercase(GenericString(str))) == String
    @test typeof(lowercase("ABCDEF")) == String
    @test typeof(lowercase(GenericString(str))) == String

    foomap(ch) = (ch > Char(65))
    foobar(ch) = Char(0xd800)
    foobaz(ch) = reinterpret(Char, typemax(UInt32))
    @test_throws ArgumentError map(foomap, GenericString(str))
    @test map(foobar, GenericString(str)) == String(repeat(b"\ud800", outer=[17]))
    @test map(foobaz, GenericString(str)) == String(repeat(b"\ufffd", outer=[17]))

    @test "a".*["b","c"] == ["ab","ac"]
    @test ["b","c"].*"a" == ["ba","ca"]
    @test ["a","b"].*["c" "d"] == ["ac" "ad"; "bc" "bd"]

    @test one(String) == ""
    @test prod(["*" for i in 1:3]) == "***"
    @test prod(["*" for i in 1:0]) == ""
end

@testset "NULL pointers are handled consistently by String" begin
    @test_throws ArgumentError unsafe_string(Ptr{UInt8}(0))
    @test_throws ArgumentError unsafe_string(Ptr{UInt8}(0), 10)
end
@testset "ascii for ASCII strings and non-ASCII strings" begin
    @test ascii("Hello, world") == "Hello, world"
    @test typeof(ascii("Hello, world")) == String
    @test ascii(GenericString("Hello, world")) == "Hello, world"
    @test typeof(ascii(GenericString("Hello, world"))) == String
    @test_throws ArgumentError ascii("Hello, ∀")
    @test_throws ArgumentError ascii(GenericString("Hello, ∀"))
end
@testset "issue #17271: endof() doesn't throw an error even with invalid strings" begin
    @test endof(String(b"\x90")) == 0
    @test endof(String(b"\xce")) == 1
end
# issue #17624, missing getindex method for String
@test "abc"[:] == "abc"

@testset "issue #18280: next/nextind must return past String's underlying data" begin
    for s in ("Hello", "Σ", "こんにちは", "😊😁")
        local s
        @test next(s, endof(s))[2] > sizeof(s)
        @test nextind(s, endof(s)) > sizeof(s)
    end
end
# Test cmp with AbstractStrings that don't index the same as UTF-8, which would include
# (LegacyString.)UTF16String and (LegacyString.)UTF32String, among others.

mutable struct CharStr <: AbstractString
    chars::Vector{Char}
    CharStr(x) = new(collect(x))
end
Base.start(x::CharStr) = start(x.chars)
Base.next(x::CharStr, i::Int) = next(x.chars, i)
Base.done(x::CharStr, i::Int) = done(x.chars, i)
Base.endof(x::CharStr) = endof(x.chars)
@testset "cmp without UTF-8 indexing" begin
    # Simple case, with just ANSI Latin 1 characters
    @test "áB" != CharStr("áá") # returns false with bug
    @test cmp("áB", CharStr("áá")) == -1 # returns 0 with bug

    # Case with Unicode characters
    @test cmp("\U1f596\U1f596", CharStr("\U1f596")) == 1   # Gives BoundsError with bug
    @test cmp(CharStr("\U1f596"), "\U1f596\U1f596") == -1
end

@testset "repeat" begin
    @inferred repeat(GenericString("x"), 1)
    @test repeat("xx",3) == repeat("x",6) == repeat('x',6) == repeat(GenericString("x"), 6) == "xxxxxx"
    @test repeat("αα",3) == repeat("α",6) == repeat('α',6) == repeat(GenericString("α"), 6) == "αααααα"
    @test repeat("x",1) == repeat('x',1) == "x"^1 == 'x'^1 == GenericString("x")^1 == "x"
    @test repeat("x",0) == repeat('x',0) == "x"^0 == 'x'^0 == GenericString("x")^0 == ""

    for S in ["xxx", "ååå", "∀∀∀", "🍕🍕🍕"]
        c = S[1]
        s = string(c)
        @test_throws ArgumentError repeat(c, -1)
        @test_throws ArgumentError repeat(s, -1)
        @test_throws ArgumentError repeat(S, -1)
        @test repeat(c, 0) == ""
        @test repeat(s, 0) == ""
        @test repeat(S, 0) == ""
        @test repeat(c, 1) == s
        @test repeat(s, 1) == s
        @test repeat(S, 1) == S
        @test repeat(c, 3) == S
        @test repeat(s, 3) == S
        @test repeat(S, 3) == S*S*S
    end
end
@testset "issue #12495: check that logical indexing attempt raises ArgumentError" begin
    @test_throws ArgumentError "abc"[[true, false, true]]
    @test_throws ArgumentError "abc"[BitArray([true, false, true])]
end

@testset "concatenation" begin
    @test "ab" * "cd" == "abcd"
    @test 'a' * "bc" == "abc"
    @test "ab" * 'c' == "abc"
    @test 'a' * 'b' == "ab"
    @test 'a' * "b" * 'c' == "abc"
    @test "a" * 'b' * 'c' == "abc"
end

@testset "unrecognized escapes in string/char literals" begin
    @test_throws Meta.ParseError Meta.parse("\"\\.\"")
    @test_throws Meta.ParseError Meta.parse("\'\\.\'")
end

@testset "thisind" begin
    let strs = Any["∀α>β:α+1>β", s"∀α>β:α+1>β",
                   SubString("123∀α>β:α+1>β123", 4, 18),
                   SubString(s"123∀α>β:α+1>β123", 4, 18)]
        for s in strs
            @test thisind(s, -2) == 0
            @test thisind(s, 0) == 0
            @test thisind(s, 1) == 1
            @test thisind(s, 2) == 1
            @test thisind(s, 3) == 1
            @test thisind(s, 4) == 4
            @test thisind(s, 5) == 4
            @test thisind(s, 6) == 6
            @test thisind(s, 15) == 15
            @test thisind(s, 16) == 15
            @test thisind(s, 17) == 17
            @test thisind(s, 30) == 17
        end
    end

    let strs = Any["", s"", SubString("123", 2, 1), SubString(s"123", 2, 1)]
        for s in strs, i in -2:2
            @test thisind(s, i) == (i > 0)
        end
    end
end

@testset "prevind and nextind" begin
    let strs = Any["∀α>β:α+1>β", GenericString("∀α>β:α+1>β")]
        for i in 1:2
            @test prevind(strs[i], 1) == 0
            @test prevind(strs[i], 1, 1) == 0
            @test prevind(strs[i], 2) == 1
            @test prevind(strs[i], 2, 1) == 1
            @test prevind(strs[i], 4) == 1
            @test prevind(strs[i], 4, 1) == 1
            @test prevind(strs[i], 5) == 4
            @test prevind(strs[i], 5, 1) == 4
            @test prevind(strs[i], 5, 2) == 1
            @test prevind(strs[i], 5, 3) == 0
            @test prevind(strs[i], 15) == 14
            @test prevind(strs[i], 15, 1) == 14
            @test prevind(strs[i], 15, 2) == 13
            @test prevind(strs[i], 15, 3) == 12
            @test prevind(strs[i], 15, 4) == 10
            @test prevind(strs[i], 15, 10) == 0
            @test prevind(strs[i], 15, 9) == 1
            @test prevind(strs[i], 15, 10) == 0
            @test prevind(strs[i], 16) == 15
            @test prevind(strs[i], 16, 1) == 15
            @test prevind(strs[i], 16, 2) == 14
            @test prevind(strs[i], 20) == 15
            @test prevind(strs[i], 20, 1) == 15
            @test prevind(strs[i], 20, 10) == 1
            @test_throws ArgumentError prevind(strs[i], 20, 0)

            @test nextind(strs[i], -1) == 1
            @test nextind(strs[i], -1, 1) == 1
            @test nextind(strs[i], 0, 2) == 4
            @test nextind(strs[i], 0, 20) == 26
            @test nextind(strs[i], 0, 10) == 15
            @test nextind(strs[i], 1) == 4
            @test nextind(strs[i], 1, 1) == 4
            @test nextind(strs[i], 1, 2) == 6
            @test nextind(strs[i], 1, 9) == 15
            @test nextind(strs[i], 1, 10) == 17
            @test nextind(strs[i], 2) == 4
            @test nextind(strs[i], 2, 1) == 4
            @test nextind(strs[i], 3) == 4
            @test nextind(strs[i], 3, 1) == 4
            @test nextind(strs[i], 4) == 6
            @test nextind(strs[i], 4, 1) == 6
            @test nextind(strs[i], 14) == 15
            @test nextind(strs[i], 14, 1) == 15
            @test nextind(strs[i], 15) == 17
            @test nextind(strs[i], 15, 1) == 17
            @test nextind(strs[i], 20) == 21
            @test nextind(strs[i], 20, 1) == 21
            @test_throws ArgumentError nextind(strs[i], 20, 0)

            for x in -10:20
                n = p = x
                for j in 1:40
                    p = prevind(strs[i], p)
                    @test prevind(strs[i], x, j) == p
                    n = nextind(strs[i], n)
                    @test nextind(strs[i], x, j) == n
                end
            end
        end
        @test prevind(strs[1], -1) == -2
        @test prevind(strs[1], -1, 1) == -2

        @test prevind(strs[2], -1) == 0
        @test prevind(strs[2], -1, 1) == 0
    end
end

@testset "first and last" begin
    s = "∀ϵ≠0: ϵ²>0"
    @test_throws ArgumentError first(s, -1)
    @test first(s, 0) == ""
    @test first(s, 1) == "∀"
    @test first(s, 2) == "∀ϵ"
    @test first(s, 3) == "∀ϵ≠"
    @test first(s, 4) == "∀ϵ≠0"
    @test first(s, length(s)) == s
    @test_throws BoundsError first(s, length(s)+1)
    @test_throws ArgumentError last(s, -1)
    @test last(s, 0) == ""
    @test last(s, 1) == "0"
    @test last(s, 2) == ">0"
    @test last(s, 3) == "²>0"
    @test last(s, 4) == "ϵ²>0"
    @test last(s, length(s)) == s
    @test_throws BoundsError last(s, length(s)+1)
end

@testset "invalid code point" begin
    s = String([0x61, 0xba, 0x41])
    @test !isvalid(s)
    @test_throws UnicodeError s[2]
    e = try
        s[2]
    catch e
        e
    end
    b = IOBuffer()
    show(b, e)
    @test String(take!(b)) == "UnicodeError: invalid character index 2 (0xba is a continuation byte)"
end

@testset "ncodeunits" begin
    for (s, n) in [""     => 0, "a"   => 1, "abc"  => 3,
                   "α"    => 2, "abγ" => 4, "∀"    => 3,
                   "∀x∃y" => 8, "🍕"  => 4, "🍕∀" => 7]
        @test ncodeunits(s) == n
        @test ncodeunits(GenericString(s)) == n
    end
end
