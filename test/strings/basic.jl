# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

@testset "constructors" begin
    v = [0x61,0x62,0x63,0x21]
    @test String(v) == "abc!" && isempty(v)
    @test String("abc!") == "abc!"
    @test String(0x61:0x63) == "abc"

    # Check that resizing empty source vector does not corrupt string
    b = IOBuffer()
    write(b, "ab")
    x = take!(b)
    s = String(x)
    resize!(x, 0)
    empty!(x) # Another method which must be tested
    @test s == "ab"
    resize!(x, 1)
    @test s == "ab"

    @test isempty(string())
    @test !isempty("abc")
    @test !isempty("∀∃")
    @test !isempty(GenericString("∀∃"))
    @test isempty(GenericString(""))
    @test !isempty(GenericString("abc"))
    @test eltype(GenericString) == Char
    @test firstindex("abc") == 1
    @test cmp("ab","abc") == -1
    @test typemin(String) === ""
    @test typemin("abc") === ""
    @test "abc" === "abc"
    @test "ab"  !== "abc"
    @test string("ab", 'c') === "abc"
    @test string() === ""
    @test string(SubString("123", 2)) === "23"
    @test string("∀∃", SubString("1∀∃", 2)) === "∀∃∀∃"
    @test string("∀∃", "1∀∃") === "∀∃1∀∃"
    @test string(SubString("∀∃"), SubString("1∀∃", 2)) === "∀∃∀∃"
    @test string(s"123") === s"123"
    @test string("123", 'α', SubString("1∀∃", 2), 'a', "foo") === "123α∀∃afoo"
    codegen_egal_of_strings(x, y) = (x===y, x!==y)
    @test codegen_egal_of_strings(string("ab", 'c'), "abc") === (true, false)
    let strs = ["", "a", "a b c", "до свидания"]
        for x in strs, y in strs
            @test (x === y) == (objectid(x) == objectid(y))
        end
    end
end

@testset "{starts,ends}with" begin
    @test startswith("abcd", 'a')
    @test startswith('a')("abcd")
    @test startswith("abcd", "a")
    @test startswith("a")("abcd")
    @test startswith("abcd", "ab")
    @test startswith("ab")("abcd")
    @test !startswith("ab", "abcd")
    @test !startswith("abcd")("ab")
    @test !startswith("abcd", "bc")
    @test !startswith("bc")("abcd")
    @test endswith("abcd", 'd')
    @test endswith('d')("abcd")
    @test endswith("abcd", "d")
    @test endswith("d")("abcd")
    @test endswith("abcd", "cd")
    @test endswith("cd")("abcd")
    @test !endswith("abcd", "dc")
    @test !endswith("dc")("abcd")
    @test !endswith("cd", "abcd")
    @test !endswith("abcd")("cd")
    @test startswith("ab\0cd", "ab\0c")
    @test startswith("ab\0c")("ab\0cd")
    @test !startswith("ab\0cd", "ab\0d")
    @test !startswith("ab\0d")("ab\0cd")
    x = "∀"
    y = String(codeunits(x)[1:2])
    z = String(codeunits(x)[1:1])
    @test !startswith(x, y)
    @test !startswith(y)(x)
    @test !startswith(x, z)
    @test !startswith(z)(x)
    @test !startswith(y, z)
    @test !startswith(z)(y)
    @test startswith(x, x)
    @test startswith(x)(x)
    @test startswith(y, y)
    @test startswith(y)(y)
    @test startswith(z, z)
    @test startswith(z)(z)
    x = SubString(x)
    y = SubString(y)
    z = SubString(z)
    @test !startswith(x, y)
    @test !startswith(y)(x)
    @test !startswith(x, z)
    @test !startswith(z)(x)
    @test !startswith(y, z)
    @test !startswith(z)(y)
    @test startswith(x, x)
    @test startswith(x)(x)
    @test startswith(y, y)
    @test startswith(y)(y)
    @test startswith(z, z)
    @test startswith(z)(z)
    x = "x∀y"
    y = SubString("x\xe2\x88y", 1, 2)
    z = SubString("x\xe2y", 1, 2)
    @test !startswith(x, y)
    @test !startswith(y)(x)
    @test !startswith(x, z)
    @test !startswith(z)(x)
    @test !startswith(y, z)
    @test !startswith(z)(y)
    @test startswith(x, x)
    @test startswith(x)(x)
    @test startswith(y, y)
    @test startswith(y)(y)
    @test startswith(z, z)
    @test startswith(z)(z)
    x = "∀"
    y = String(codeunits(x)[2:3])
    z = String(codeunits(x)[3:3])
    @test !endswith(x, y)
    @test !endswith(y)(x)
    @test !endswith(x, z)
    @test !endswith(z)(x)
    @test endswith(y, z)
    @test endswith(z)(y)
    @test endswith(x, x)
    @test endswith(x)(x)
    @test endswith(y, y)
    @test endswith(y)(y)
    @test endswith(z, z)
    @test endswith(z)(z)
    x = SubString(x)
    y = SubString(y)
    z = SubString(z)
    @test !endswith(x, y)
    @test !endswith(y)(x)
    @test !endswith(x, z)
    @test !endswith(z)(x)
    @test endswith(y, z)
    @test endswith(z)(y)
    @test endswith(x, x)
    @test endswith(x)(x)
    @test endswith(y, y)
    @test endswith(y)(y)
    @test endswith(z, z)
    @test endswith(z)(z)
    x = "x∀y"
    y = SubString("x\x88\x80y", 2, 4)
    z = SubString("x\x80y", 2, 3)
    @test !endswith(x, y)
    @test !endswith(y)(x)
    @test !endswith(x, z)
    @test !endswith(z)(x)
    @test endswith(y, z)
    @test endswith(z)(y)
    @test endswith(x, x)
    @test endswith(x)(x)
    @test endswith(y, y)
    @test endswith(y)(y)
    @test endswith(z, z)
    @test endswith(z)(z)
end

@testset "SubStrings and Views" begin
    x = "abcdefg"
    @testset "basic unit range" begin
        @test SubString(x, 2:4) == "bcd"
        @test view(x, 2:4) == "bcd"
        @test view(x, 2:4) isa SubString
        @test (@view x[4:end]) == "defg"
        @test (@view x[4:end]) isa SubString
    end

    @testset "other AbstractUnitRanges" begin
        @test SubString(x, Base.OneTo(3)) == "abc"
        @test view(x, Base.OneTo(4)) == "abcd"
        @test view(x, Base.OneTo(4)) isa SubString
    end

    @testset "views but not view" begin
        # We don't (at present) make non-contiguous SubStrings with views
        @test_throws MethodError (@view x[[1,3,5]])
        @test (@views (x[[1,3,5]])) isa String

        # We don't (at present) make single character SubStrings with views
        @test_throws MethodError (@view x[3])
        @test (@views (x[3])) isa Char

        @test (@views (x[3], x[1:2], x[[1,4]])) isa Tuple{Char, SubString, String}
        @test (@views (x[3], x[1:2], x[[1,4]])) == ('c', "ab", "ad")
    end
end


@testset "filter specialization on String issue #32460" begin
     @test filter(x -> x ∉ ['작', 'Ï', 'z', 'ξ'],
                  GenericString("J'étais n작작é pour plaiÏre à toute âξme un peu fière")) ==
                  "J'étais né pour plaire à toute âme un peu fière"
     @test filter(x -> x ∉ ['작', 'Ï', 'z', 'ξ'],
                  "J'étais n작작é pour plaiÏre à toute âξme un peu fière") ==
                  "J'étais né pour plaire à toute âme un peu fière"
     @test filter(x -> x ∈ ['f', 'o'], GenericString("foobar")) == "foo"
end

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
@test string(GenericString("Test")[1:1], "X") === "TX"

@testset "parsing Int types" begin
    let b, n
    for T = (UInt8,Int8,UInt16,Int16,UInt32,Int32,UInt64,Int64,UInt128,Int128,BigInt),
            b = 2:62,
            _ = 1:10
        n = (T != BigInt) ? rand(T) : BigInt(rand(Int128))
        @test parse(T, string(n, base = b),  base = b) == n
    end
    end
end

@testset "issue #6027 - make symbol with invalid char" begin
    sym = Symbol(Char(0xdcdb))
    @test string(sym) == string(Char(0xdcdb))
    @test String(sym) == string(Char(0xdcdb))
    @test Meta.lower(Main, sym) === sym
    @test Meta.parse(string(Char(0xe0080)," = 1"), 1, raise=false)[1] ==
        Expr(:error, "invalid character \"\Ue0080\" near column 1")
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
    @test_throws BoundsError length("hello", 1, -1)
    @test_throws BoundsError prevind("hello", 0, 1)
    @test_throws BoundsError length("hellø", 1, -1)
    @test_throws BoundsError prevind("hellø", 0, 1)
    @test_throws BoundsError length("hello", 1, 10)
    @test nextind("hello", 0, 10) == 10
    @test_throws BoundsError length("hellø", 1, 10) == 9
    @test nextind("hellø", 0, 10) == 11
    @test_throws BoundsError checkbounds("hello", 0)
    @test_throws BoundsError checkbounds("hello", 6)
    @test_throws BoundsError checkbounds("hello", 0:3)
    @test_throws BoundsError checkbounds("hello", 4:6)
    @test_throws BoundsError checkbounds("hello", [0:3;])
    @test_throws BoundsError checkbounds("hello", [4:6;])
    @test checkbounds("hello", 1) === nothing
    @test checkbounds("hello", 5) === nothing
    @test checkbounds("hello", 1:3) === nothing
    @test checkbounds("hello", 3:5) === nothing
    @test checkbounds("hello", [1:3;]) === nothing
    @test checkbounds("hello", [3:5;]) === nothing
    @test checkbounds(Bool, "hello", 0) === false
    @test checkbounds(Bool, "hello", 1) === true
    @test checkbounds(Bool, "hello", 5) === true
    @test checkbounds(Bool, "hello", 6) === false
    @test checkbounds(Bool, "hello", 0:5) === false
    @test checkbounds(Bool, "hello", 1:6) === false
    @test checkbounds(Bool, "hello", 1:5) === true
    @test checkbounds(Bool, "hello", [0:5;]) === false
    @test checkbounds(Bool, "hello", [1:6;]) === false
    @test checkbounds(Bool, "hello", [1:5;]) === true
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

    # Test that indexing into the middle of a character throws
    @test_throws StringIndexError get(utf8_str, 2, 'X')
end

#=
# issue #7764
let
    srep = repeat("Σβ",2)
    s="Σβ"
    ss=SubString(s,1,lastindex(s))

    @test repeat(ss,2) == "ΣβΣβ"

    @test lastindex(srep) == 7

    @test iterate(srep, 3) == ('β',5)
    @test iterate(srep, 7) == ('β',9)

    @test srep[7] == 'β'
    @test_throws BoundsError srep[8]
end
=#

# This caused JuliaLang/JSON.jl#82
@test first('\x00':'\x7f') === '\x00'
@test last('\x00':'\x7f') === '\x7f'

# make sure substrings do not accept code unit if it is not start of codepoint
let s = "x\u0302"
    @test s[1:2] == s
    @test_throws BoundsError s[0:3]
    @test_throws BoundsError s[1:4]
    @test_throws StringIndexError s[1:3]
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
end
# test AbstractString functions at beginning of string.jl
struct tstStringType <: AbstractString
    data::Array{UInt8,1}
end
@testset "AbstractString functions" begin
    tstr = tstStringType(unsafe_wrap(Vector{UInt8},"12"))
    @test_throws MethodError ncodeunits(tstr)
    @test_throws MethodError codeunit(tstr)
    @test_throws MethodError codeunit(tstr, 1)
    @test_throws MethodError codeunit(tstr, true)
    @test_throws MethodError isvalid(tstr, 1)
    @test_throws MethodError isvalid(tstr, true)
    @test_throws MethodError iterate(tstr, 1)
    @test_throws MethodError iterate(tstr, true)
    @test_throws MethodError lastindex(tstr)

    gstr = GenericString("12")
    @test string(gstr) isa GenericString

    @test Array{UInt8}(gstr) == [49, 50]
    @test Array{Char,1}(gstr) == ['1', '2']

    @test gstr[1] == '1'
    @test gstr[1:1] == "1"
    @test gstr[[1]] == "1"

    @test s"∀∃"[big(1)] == '∀'
    @test_throws StringIndexError GenericString("∀∃")[Int8(2)]
    @test_throws BoundsError GenericString("∀∃")[UInt16(10)]

    @test first(eachindex("foobar")) === 1
    @test first(eachindex("")) === 1
    @test last(eachindex("foobar")) === lastindex("foobar")
    @test iterate(eachindex("foobar"),7) === nothing
    @test Int == eltype(Base.EachStringIndex) ==
                 eltype(Base.EachStringIndex{String}) ==
                 eltype(Base.EachStringIndex{GenericString}) ==
                 eltype(eachindex("foobar")) == eltype(eachindex(gstr))
    for T in (GenericString, String)
        @test map(uppercase, T("foó")) == "FOÓ"
        @test map(x -> 'ó', T("")) == ""
        @test map(x -> 'ó', T("x")) == "ó"
        @test map(x -> 'ó', T("xxx")) == "óóó"
    end
    @test nextind("fóobar", 0, 3) == 4

    @test Symbol(gstr) == Symbol("12")

    @test sizeof(gstr) == 2
    @test ncodeunits(gstr) == 2
    @test length(gstr) == 2
    @test length(GenericString("")) == 0

    @test nextind(1:1, 1) == 2
    @test nextind([1], 1) == 2

    @test length(gstr, 1, 2) == 2

    # no string promotion
    let svec = [s"12", GenericString("12"), SubString("123", 1, 2)]
        @test all(x -> x == "12", svec)
        @test svec isa Vector{AbstractString}
    end
end

@testset "issue #10307" begin
    @test typeof(map(x -> parse(Int16, x), AbstractString[])) == Vector{Int16}

    for T in [Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128]
        for i in [typemax(T), typemin(T)]
            s = "$i"
            @test tryparse(T, s) == i
        end
    end

    for T in [Int8, Int16, Int32, Int64, Int128]
        for i in [typemax(T), typemin(T)]
            f = "$(i)0"
            @test tryparse(T, f) === nothing
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

    @test tryparse(BigInt, "1234567890") == BigInt(1234567890)
    @test tryparse(BigInt, "1234567890-") === nothing

    @test tryparse(Float64, "64") == 64.0
    @test tryparse(Float64, "64o") === nothing
    @test tryparse(Float32, "32") == 32.0f0
    @test tryparse(Float32, "32o") === nothing
end

@testset "tryparse invalid chars" begin
    # #32314: tryparse shouldn't throw, even given strings with invalid Chars
    @test tryparse(UInt8, "\xb5")    === nothing
    @test tryparse(UInt8, "100\xb5") === nothing  # Code path for numeric overflow
end

import Unicode

@testset "issue #10994: handle embedded NUL chars for string parsing" begin
    for T in [BigInt, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128]
        @test_throws ArgumentError parse(T, "1\0")
    end
    for T in [BigInt, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, Float64, Float32]
        @test tryparse(T, "1\0") === nothing
    end
    let s = Unicode.normalize("tést",:NFKC)
        @test unsafe_string(Base.unsafe_convert(Cstring, Base.cconvert(Cstring, s))) == s
        @test unsafe_string(Base.unsafe_convert(Cstring, Symbol(s))) == s
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
            ("\x00", true),
            ("\x7f", true),
            ("\x80", false),
            ("\xbf", false),
            ("\xc0", false),
            ("\xff", false),
            ("\xc0\x80", false),
            ("\xc1\x80", false),
            ("\xc2\x80", true),
            ("\xc2\xc0", false),
            ("\xed\x9f\xbf", true),
            ("\xed\xa0\x80", false),
            ("\xed\xbf\xbf", false),
            ("\xee\x80\x80", true),
            ("\xef\xbf\xbf", true),
            ("\xf0\x90\x80\x80", true),
            ("\xf4\x8f\xbf\xbf", true),
            ("\xf4\x90\x80\x80", false),
            ("\xf5\x80\x80\x80", false),
            ("\ud800\udc00", false),
            ("\udbff\udfff", false),
            ("\ud800\u0100", false),
            ("\udc00\u0100", false),
            ("\udc00\ud800", false),
        )
        @test isvalid(String, val) == pass == isvalid(String(val))
        @test isvalid(val[1]) == pass
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
    # Check for short three-byte sequences
    @test isvalid(String, UInt8[0xe0]) == false
    for (rng, flg) in ((0x00:0x9f, false), (0xa0:0xbf, true), (0xc0:0xff, false))
        for cont in rng
            @test isvalid(String, UInt8[0xe0, cont]) == false
            bytes = UInt8[0xe0, cont, 0x80]
            @test isvalid(String, bytes) == flg
            @test isvalid(String, @view(bytes[1:end])) == flg # contiguous subarray support
        end
    end
    # Check three-byte sequences
    for r1 in (0xe1:0xec, 0xee:0xef)
        for byt in r1
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
    @test isvalid(lstrip("blablabla")) == true
    @test isvalid(SubString(String(UInt8[0xfe, 0x80, 0x80, 0x80, 0x80, 0x80]), 1,2)) == false
    # invalid Chars
    @test  isvalid('a')
    @test  isvalid('柒')
    @test !isvalid("\xff"[1])
    @test !isvalid("\xc0\x80"[1])
    @test !isvalid("\xf0\x80\x80\x80"[1])
    @test !isvalid('\ud800')
    @test  isvalid('\ud7ff')
    @test !isvalid('\udfff')
    @test  isvalid('\ue000')
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
@testset "issue #17271: lastindex() doesn't throw an error even with invalid strings" begin
    @test lastindex("\x90") == 1
    @test lastindex("\xce") == 1
end
# issue #17624, missing getindex method for String
@test "abc"[:] == "abc"

@testset "issue #18280: next/nextind must return past String's underlying data" begin
    for s in ("Hello", "Σ", "こんにちは", "😊😁")
        local s
        @test iterate(s, lastindex(s))[2] > sizeof(s)
        @test nextind(s, lastindex(s)) > sizeof(s)
    end
end
# Test cmp with AbstractStrings that don't index the same as UTF-8, which would include
# (LegacyString.)UTF16String and (LegacyString.)UTF32String, among others.

mutable struct CharStr <: AbstractString
    chars::Vector{Char}
    CharStr(x) = new(collect(x))
end
Base.iterate(x::CharStr) = iterate(x.chars)
Base.iterate(x::CharStr, i::Int) = iterate(x.chars, i)
Base.lastindex(x::CharStr) = lastindex(x.chars)
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
    @test repeat("xx",3) === repeat(SubString("xx", 2),6) === repeat("x",6) === repeat('x',6) === repeat(GenericString("x"), 6) === "xxxxxx"
    @test repeat("αα",3) === repeat(SubString("αα", 3),6) === repeat("α",6) === repeat('α',6) === repeat(GenericString("α"), 6) === "αααααα"
    @test repeat("x",1) === repeat('x',1) === "x"^1 == 'x'^1 === GenericString("x")^1 === "x"
    @test repeat("x",0) === repeat('x',0) === "x"^0 == 'x'^0 === GenericString("x")^0 === ""

    for S in ["xxx", "ååå", "∀∀∀", "🍕🍕🍕"]
        c = S[1]
        s = string(c)
        @test_throws ArgumentError repeat(c, -1)
        @test_throws ArgumentError repeat(s, -1)
        @test_throws ArgumentError repeat(S, -1)
        for T in (Int, UInt)
            @test repeat(c, T(0)) === ""
            @test repeat(s, T(0)) === ""
            @test repeat(S, T(0)) === ""
            @test repeat(c, T(1)) === s
            @test repeat(s, T(1)) === s
            @test repeat(S, T(1)) === S
            @test repeat(c, T(3)) === S
            @test repeat(s, T(3)) === S
            @test repeat(S, T(3)) === S*S*S
        end
    end
    # Issue #32160 (string allocation unsigned overflow)
    @test_throws OutOfMemoryError repeat('x', typemax(Csize_t))
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

# this tests a possible issue in subtyping with long argument lists to `string(...)`
getString(dic, key) = haskey(dic,key) ? "$(dic[key])" : ""
function getData(dic)
    val = getString(dic,"") * "," * getString(dic,"") * "," * getString(dic,"") * "," * getString(dic,"") *
        "," * getString(dic,"") * "," * getString(dic,"") * "," * getString(dic,"") * "," * getString(dic,"") *
        "," * getString(dic,"") * "," * getString(dic,"") * "," * getString(dic,"") * "," * getString(dic,"") *
        "," * getString(dic,"") * "," * getString(dic,"") * "," * getString(dic,"") * "," * getString(dic,"") *
        "," * getString(dic,"") * "," * getString(dic,"") * "," * getString(dic,"")
end
@test getData(Dict()) == ",,,,,,,,,,,,,,,,,,"

@testset "unrecognized escapes in string/char literals" begin
    @test_throws Meta.ParseError Meta.parse("\"\\.\"")
    @test_throws Meta.ParseError Meta.parse("\'\\.\'")
end

@testset "thisind" begin
    let strs = Any["∀α>β:α+1>β", s"∀α>β:α+1>β",
                   SubString("123∀α>β:α+1>β123", 4, 18),
                   SubString(s"123∀α>β:α+1>β123", 4, 18)]
        for s in strs
            @test_throws BoundsError thisind(s, -2)
            @test_throws BoundsError thisind(s, -1)
            @test thisind(s, Int8(0)) == 0
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
            @test_throws BoundsError thisind(s, 18)
            @test_throws BoundsError thisind(s, 19)
        end
    end

    let strs = Any["", s"", SubString("123", 2, 1), SubString(s"123", 2, 1)]
        for s in strs
            @test_throws BoundsError thisind(s, -1)
            @test thisind(s, 0) == 0
            @test thisind(s, 1) == 1
            @test_throws BoundsError thisind(s, 2)
        end
    end
end

@testset "prevind and nextind" begin
    for s in Any["∀α>β:α+1>β", GenericString("∀α>β:α+1>β")]
        @test_throws BoundsError prevind(s, 0)
        @test_throws BoundsError prevind(s, 0, 0)
        @test_throws BoundsError prevind(s, 0, 1)
        @test prevind(s, 1) == 0
        @test prevind(s, Int8(1), Int8(1)) == 0
        @test prevind(s, 1, 1) == 0
        @test prevind(s, 1, 0) == 1
        @test prevind(s, 2) == 1
        @test prevind(s, 2, 1) == 1
        @test prevind(s, 4) == 1
        @test prevind(s, 4, 1) == 1
        @test prevind(s, 5) == 4
        @test prevind(s, 5, 1) == 4
        @test prevind(s, 5, 2) == 1
        @test prevind(s, 5, 3) == 0
        @test prevind(s, 15) == 14
        @test prevind(s, 15, 1) == 14
        @test prevind(s, 15, 2) == 13
        @test prevind(s, 15, 3) == 12
        @test prevind(s, 15, 4) == 10
        @test prevind(s, 15, 10) == 0
        @test prevind(s, 15, 9) == 1
        @test prevind(s, 16) == 15
        @test prevind(s, 16, 1) == 15
        @test prevind(s, 16, 2) == 14
        @test prevind(s, 17) == 15
        @test prevind(s, 17, 1) == 15
        @test prevind(s, 17, 2) == 14
        @test_throws BoundsError prevind(s, 18)
        @test_throws BoundsError prevind(s, 18, 0)
        @test_throws BoundsError prevind(s, 18, 1)

        @test_throws BoundsError nextind(s, -1)
        @test_throws BoundsError nextind(s, -1, 0)
        @test_throws BoundsError nextind(s, -1, 1)
        @test nextind(s, 0, 2) == 4
        @test nextind(s, Int8(0), Int8(2)) == 4
        @test nextind(s, 0, 20) == 26
        @test nextind(s, 0, 10) == 15
        @test nextind(s, 1) == 4
        @test nextind(s, Int8(1)) == 4
        @test nextind(s, 1, 1) == 4
        @test nextind(s, 1, 2) == 6
        @test nextind(s, 1, 9) == 15
        @test nextind(s, 1, 10) == 17
        @test nextind(s, 2) == 4
        @test nextind(s, 2, 1) == 4
        @test nextind(s, 3) == 4
        @test nextind(s, 3, 1) == 4
        @test nextind(s, 4) == 6
        @test nextind(s, 4, 1) == 6
        @test nextind(s, 14) == 15
        @test nextind(s, 14, 1) == 15
        @test nextind(s, 15) == 17
        @test nextind(s, 15, 1) == 17
        @test nextind(s, 15, 2) == 18
        @test nextind(s, 16) == 17
        @test nextind(s, 16, 1) == 17
        @test nextind(s, 16, 2) == 18
        @test nextind(s, 16, 3) == 19
        @test_throws BoundsError nextind(s, 17)
        @test_throws BoundsError nextind(s, 17, 0)
        @test_throws BoundsError nextind(s, 17, 1)

        for x in 0:ncodeunits(s)+1
            n = p = x
            for j in 1:40
                if 1 ≤ p
                    p = prevind(s, p)
                    @test prevind(s, x, j) == p
                end
                if n ≤ ncodeunits(s)
                    n = nextind(s, n)
                    @test nextind(s, x, j) == n
                end
            end
        end
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
    @test first(s, length(s)+1) == s
    @test_throws ArgumentError last(s, -1)
    @test last(s, 0) == ""
    @test last(s, 1) == "0"
    @test last(s, 2) == ">0"
    @test last(s, 3) == "²>0"
    @test last(s, 4) == "ϵ²>0"
    @test last(s, length(s)) == s
    @test last(s, length(s)+1) == s
end

@testset "invalid code point" begin
    s = String([0x61, 0xba, 0x41])
    @test !isvalid(s)
    @test s[2] == reinterpret(Char, UInt32(0xba) << 24)
end

@testset "ncodeunits" begin
    for (s, n) in [""     => 0, "a"   => 1, "abc"  => 3,
                   "α"    => 2, "abγ" => 4, "∀"    => 3,
                   "∀x∃y" => 8, "🍕"  => 4, "🍕∀" => 7]
        @test ncodeunits(s) == n
        @test ncodeunits(GenericString(s)) == n
    end
end

@testset "0-step nextind and prevind" begin
    for T in [String, SubString, Base.SubstitutionString, GenericString]
        e = convert(T, "")
        @test nextind(e, 0, 0) == 0
        @test_throws BoundsError nextind(e, 1, 0)
        @test_throws BoundsError prevind(e, 0, 0)
        @test prevind(e, 1, 0) == 1

        s = convert(T, "∀x∃")
        @test nextind(s, 0, 0) == 0
        @test nextind(s, 1, 0) == 1
        @test_throws StringIndexError nextind(s, 2, 0)
        @test_throws StringIndexError nextind(s, 3, 0)
        @test nextind(s, 4, 0) == 4
        @test nextind(s, 5, 0) == 5
        @test_throws StringIndexError nextind(s, 6, 0)
        @test_throws StringIndexError nextind(s, 7, 0)
        @test_throws BoundsError nextind(s, 8, 0)

        @test_throws BoundsError prevind(s, 0, 0)
        @test prevind(s, 1, 0) == 1
        @test_throws StringIndexError prevind(s, 2, 0)
        @test_throws StringIndexError prevind(s, 3, 0)
        @test prevind(s, 4, 0) == 4
        @test prevind(s, 5, 0) == 5
        @test_throws StringIndexError prevind(s, 6, 0)
        @test_throws StringIndexError prevind(s, 7, 0)
        @test prevind(s, 8, 0) == 8
    end
end

@test unsafe_wrap(Vector{UInt8},"\xcc\xdd\xee\xff\x80") == [0xcc,0xdd,0xee,0xff,0x80]

@test iterate("a", 1)[2] == 2
@test nextind("a", 1) == 2
@test iterate("az", 1)[2] == 2
@test nextind("az", 1) == 2
@test iterate("a\xb1", 1)[2] == 2
@test nextind("a\xb1", 1) == 2
@test iterate("a\xb1z", 1)[2] == 2
@test nextind("a\xb1z", 1) == 2
@test iterate("a\xb1\x83", 1)[2] == 2
@test nextind("a\xb1\x83", 1) == 2
@test iterate("a\xb1\x83\x84", 1)[2] == 2
@test nextind("a\xb1\x83\x84", 1) == 2
@test iterate("a\xb1\x83\x84z", 1)[2] == 2
@test nextind("a\xb1\x83\x84z", 1) == 2

@test iterate("\x81", 1)[2] == 2
@test nextind("\x81", 1) == 2
@test iterate("\x81z", 1)[2] == 2
@test nextind("\x81z", 1) == 2
@test iterate("\x81\xb1", 1)[2] == 2
@test nextind("\x81\xb1", 1) == 2
@test iterate("\x81\xb1z", 1)[2] == 2
@test nextind("\x81\xb1z", 1) == 2
@test iterate("\x81\xb1\x83", 1)[2] == 2
@test nextind("\x81\xb1\x83", 1) == 2
@test iterate("\x81\xb1\x83\x84", 1)[2] == 2
@test nextind("\x81\xb1\x83\x84", 1) == 2
@test iterate("\x81\xb1\x83\x84z", 1)[2] == 2
@test nextind("\x81\xb1\x83\x84z", 1) == 2

@test iterate("\xce", 1)[2] == 2
@test nextind("\xce", 1) == 2
@test iterate("\xcez", 1)[2] == 2
@test nextind("\xcez", 1) == 2
@test iterate("\xce\xb1", 1)[2] == 3
@test nextind("\xce\xb1", 1) == 3
@test iterate("\xce\xb1z", 1)[2] == 3
@test nextind("\xce\xb1z", 1) == 3
@test iterate("\xce\xb1\x83", 1)[2] == 3
@test nextind("\xce\xb1\x83", 1) == 3
@test iterate("\xce\xb1\x83\x84", 1)[2] == 3
@test nextind("\xce\xb1\x83\x84", 1) == 3
@test iterate("\xce\xb1\x83\x84z", 1)[2] == 3
@test nextind("\xce\xb1\x83\x84z", 1) == 3

@test iterate("\xe2", 1)[2] == 2
@test nextind("\xe2", 1) == 2
@test iterate("\xe2z", 1)[2] == 2
@test nextind("\xe2z", 1) == 2
@test iterate("\xe2\x88", 1)[2] == 3
@test nextind("\xe2\x88", 1) == 3
@test iterate("\xe2\x88z", 1)[2] == 3
@test nextind("\xe2\x88z", 1) == 3
@test iterate("\xe2\x88\x83", 1)[2] == 4
@test nextind("\xe2\x88\x83", 1) == 4
@test iterate("\xe2\x88\x83z", 1)[2] == 4
@test nextind("\xe2\x88\x83z", 1) == 4
@test iterate("\xe2\x88\x83\x84", 1)[2] == 4
@test nextind("\xe2\x88\x83\x84", 1) == 4
@test iterate("\xe2\x88\x83\x84z", 1)[2] == 4
@test nextind("\xe2\x88\x83\x84z", 1) == 4

@test iterate("\xf0", 1)[2] == 2
@test nextind("\xf0", 1) == 2
@test iterate("\xf0z", 1)[2] == 2
@test nextind("\xf0z", 1) == 2
@test iterate("\xf0\x9f", 1)[2] == 3
@test nextind("\xf0\x9f", 1) == 3
@test iterate("\xf0\x9fz", 1)[2] == 3
@test nextind("\xf0\x9fz", 1) == 3
@test iterate("\xf0\x9f\x98", 1)[2] == 4
@test nextind("\xf0\x9f\x98", 1) == 4
@test iterate("\xf0\x9f\x98z", 1)[2] == 4
@test nextind("\xf0\x9f\x98z", 1) == 4
@test iterate("\xf0\x9f\x98\x84", 1)[2] == 5
@test nextind("\xf0\x9f\x98\x84", 1) == 5
@test iterate("\xf0\x9f\x98\x84z", 1)[2] == 5
@test nextind("\xf0\x9f\x98\x84z", 1) == 5

@test iterate("\xf8", 1)[2] == 2
@test nextind("\xf8", 1) == 2
@test iterate("\xf8z", 1)[2] == 2
@test nextind("\xf8z", 1) == 2
@test iterate("\xf8\x9f", 1)[2] == 2
@test nextind("\xf8\x9f", 1) == 2
@test iterate("\xf8\x9fz", 1)[2] == 2
@test nextind("\xf8\x9fz", 1) == 2
@test iterate("\xf8\x9f\x98", 1)[2] == 2
@test nextind("\xf8\x9f\x98", 1) == 2
@test iterate("\xf8\x9f\x98z", 1)[2] == 2
@test nextind("\xf8\x9f\x98z", 1) == 2
@test iterate("\xf8\x9f\x98\x84", 1)[2] == 2
@test nextind("\xf8\x9f\x98\x84", 1) == 2
@test iterate("\xf8\x9f\x98\x84z", 1)[2] == 2
@test nextind("\xf8\x9f\x98\x84z", 1) == 2

# codeunit vectors

let s = "∀x∃y", u = codeunits(s)
    @test u isa Base.CodeUnits{UInt8,String}
    @test length(u) == ncodeunits(s) == 8
    @test sizeof(u) == sizeof(s)
    @test eltype(u) === UInt8
    @test size(u) == (length(u),)
    @test strides(u) == (1,)
    @test u[1] == 0xe2
    @test u[2] == 0x88
    @test u[8] == 0x79
    @test_throws ErrorException (u[1] = 0x00)
    @test collect(u) == b"∀x∃y"
end

# issue #24388
let v = unsafe_wrap(Vector{UInt8}, "abc")
    s = String(v)
    @test_throws BoundsError v[1]
    push!(v, UInt8('x'))
    @test s == "abc"
    s = "abc"
    v = Vector{UInt8}(s)
    v[1] = 0x40
    @test s == "abc"
end

# PR #25535
let v = [0x40,0x41,0x42]
    @test String(view(v, 2:3)) == "AB"
end

# make sure length for identical String and AbstractString return the same value, PR #25533
let rng = MersenneTwister(1), strs = ["∀εa∀aε"*String(rand(rng, UInt8, 100))*"∀εa∀aε",
                                   String(rand(rng, UInt8, 200))]
    for s in strs, i in 1:ncodeunits(s)+1, j in 0:ncodeunits(s)
            @test length(s,i,j) == length(GenericString(s),i,j)
    end
    for i in 0:10, j in 1:100,
        s in [randstring(rng, i), randstring(rng, "∀∃α1", i), String(rand(rng, UInt8, i))]
        @test length(s) == length(GenericString(s))
    end
end

# conversion of SubString to the same type, issue #25525
let x = SubString("ab", 1, 1)
    y = convert(SubString{String}, x)
    @test y === x
    chop("ab") === chop.(["ab"])[1]
end
