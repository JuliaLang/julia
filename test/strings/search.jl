# This file is a part of Julia. License is MIT: https://julialang.org/license

# some test strings
astr = "Hello, world.\n"
u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

@testset "BoundsError for findnext/findprev" begin
    # I think these should give error on 4 also, and "" is not treated
    # consistently with SubString("",1,1), nor with Char[]
    for ind in (0, 5)
        @test_throws BoundsError findnext(SubString("",1,1), "foo", ind)
        @test_throws BoundsError findprev(SubString("",1,1), "foo", ind)
    end

    # Note: the commented out test will be enabled after fixes to make
    # sure that findnext/findprev are consistent
    # no matter what type of AbstractString the second argument is
    @test_throws BoundsError findnext(isequal('a'), "foo", 0)
    @test_throws BoundsError findnext(in(Char[]), "foo", 5)
    # @test_throws BoundsError findprev(in(Char[]), "foo", 0)
    @test_throws BoundsError findprev(in(Char[]), "foo", 5)

    # @test_throws ErrorException in("foobar","bar")
    @test_throws BoundsError findnext(isequal(0x1),b"\x1\x2",0)
end

@testset "ascii forward search $(typeof(str))" for str in [astr, GenericString(astr)]
    @test_throws BoundsError findnext(isequal('z'), str, 0)
    @test_throws BoundsError findnext(isequal('∀'), str, 0)
    @test findfirst(isequal('x'), str) === nothing
    @test findfirst(isequal('\0'), str) === nothing
    @test findfirst(isequal('\u80'), str) === nothing
    @test findfirst(isequal('∀'), str) === nothing
    @test findfirst(isequal('H'), str) == 1
    @test findfirst(isequal('l'), str) == 3
    @test findnext(isequal('l'), str, 4) == 4
    @test findnext(isequal('l'), str, 5) == 11
    @test findnext(isequal('l'), str, 12) === nothing
    @test findfirst(isequal(','), str) == 6
    @test findnext(isequal(','), str, 7) === nothing
    @test findfirst(isequal('\n'), str) == 14
    @test findnext(isequal('\n'), str, 15) === nothing
    @test_throws BoundsError findnext(isequal('ε'), str, nextind(str,lastindex(str))+1)
    @test_throws BoundsError findnext(isequal('a'), str, nextind(str,lastindex(str))+1)

    @test_throws BoundsError findnext('z', str, 0)
    @test_throws BoundsError findnext('∀', str, 0)
    @test findfirst('x', str) === nothing
    @test findfirst('\0', str) === nothing
    @test findfirst('\u80', str) === nothing
    @test findfirst('∀', str) === nothing
    @test findfirst('H', str) == 1
    @test findfirst('l', str) == 3
    @test findfirst('e', str) == 2
    @test findfirst('u', str) === nothing
    @test findnext('l', str, 4) == 4
    @test findnext('l', str, 5) == 11
    @test findnext('l', str, 12) === nothing
    @test findfirst(',', str) == 6
    @test findnext(',', str, 7) === nothing
    @test findfirst('\n', str) == 14
    @test findnext('\n', str, 15) === nothing
    @test_throws BoundsError findnext('ε', str, nextind(str,lastindex(str))+1)
    @test_throws BoundsError findnext('a', str, nextind(str,lastindex(str))+1)
end

@testset "ascii backward search" begin
    str = astr
    @test findlast(isequal('x'), str) === nothing
    @test findlast(isequal('\0'), str) === nothing
    @test findlast(isequal('\u80'), str) === nothing
    @test findlast(isequal('∀'), str) === nothing
    @test findlast(isequal('H'), str) == 1
    @test findprev(isequal('H'), str, 0) === nothing
    @test findlast(isequal('l'), str) == 11
    @test findprev(isequal('l'), str, 5) == 4
    @test findprev(isequal('l'), str, 4) == 4
    @test findprev(isequal('l'), str, 3) == 3
    @test findprev(isequal('l'), str, 2) === nothing
    @test findlast(isequal(','), str) == 6
    @test findprev(isequal(','), str, 5) === nothing
    @test findlast(isequal('\n'), str) == 14

    @test findlast('x', str) === nothing
    @test findlast('\0', str) === nothing
    @test findlast('\u80', str) === nothing
    @test findlast('∀', str) === nothing
    @test findlast('H', str) == 1
    @test findprev('H', str, 0) === nothing
    @test findlast('l', str) == 11
    @test findprev('l', str, 5) == 4
    @test findprev('l', str, 4) == 4
    @test findprev('l', str, 3) == 3
    @test findprev('l', str, 2) === nothing
    @test findlast(',', str) == 6
    @test findprev(',', str, 5) === nothing
    @test findlast(str, "") === nothing
    @test findlast(str^2, str) === nothing
    @test findlast('\n', str) == 14
end

@testset "utf-8 forward search $(typeof(str))" for str in (u8str, GenericString(u8str))
    @test_throws BoundsError findnext(isequal('z'), str, 0)
    @test_throws BoundsError findnext(isequal('∀'), str, 0)
    @test findfirst(isequal('z'), str) === nothing
    @test findfirst(isequal('\0'), str) === nothing
    @test findfirst(isequal('\u80'), str) === nothing
    @test findfirst(isequal('∄'), str) === nothing
    @test findfirst(isequal('∀'), str) == 1
    @test_throws StringIndexError findnext(isequal('∀'), str, 2)
    @test findnext(isequal('∀'), str, 4) === nothing
    @test findfirst(isequal('∃'), str) == 13
    @test_throws StringIndexError findnext(isequal('∃'), str, 15)
    @test findnext(isequal('∃'), str, 16) === nothing
    @test findfirst(isequal('x'), str) == 26
    @test findnext(isequal('x'), str, 27) == 43
    @test findnext(isequal('x'), str, 44) === nothing
    @test findfirst(isequal('δ'), str) == 17
    @test_throws StringIndexError findnext(isequal('δ'), str, 18)
    @test findnext(isequal('δ'), str, nextind(str,17)) == 33
    @test findnext(isequal('δ'), str, nextind(str,33)) === nothing
    @test findfirst(isequal('ε'), str) == 5
    @test findnext(isequal('ε'), str, nextind(str,5)) == 54
    @test findnext(isequal('ε'), str, nextind(str,54)) === nothing
    @test findnext(isequal('ε'), str, nextind(str,lastindex(str))) === nothing
    @test findnext(isequal('a'), str, nextind(str,lastindex(str))) === nothing
    @test_throws BoundsError findnext(isequal('ε'), str, nextind(str,lastindex(str))+1)
    @test_throws BoundsError findnext(isequal('a'), str, nextind(str,lastindex(str))+1)
end

@testset "utf-8 backward search" begin
    str = u8str
    @test findlast(isequal('z'), str) === nothing
    @test findlast(isequal('\0'), str) === nothing
    @test findlast(isequal('\u80'), str) === nothing
    @test findlast(isequal('∄'), str) === nothing
    @test findlast(isequal('∀'), str) == 1
    @test findprev(isequal('∀'), str, 0) === nothing
    @test findlast(isequal('∃'), str) == 13
    @test findprev(isequal('∃'), str, 14) == 13
    @test findprev(isequal('∃'), str, 13) == 13
    @test findprev(isequal('∃'), str, 12) === nothing
    @test findlast(isequal('x'), str) == 43
    @test findprev(isequal('x'), str, 42) == 26
    @test findprev(isequal('x'), str, 25) === nothing
    @test findlast(isequal('δ'), str) == 33
    @test findprev(isequal('δ'), str, 32) == 17
    @test findprev(isequal('δ'), str, 16) === nothing
    @test findlast(isequal('ε'), str) == 54
    @test findprev(isequal('ε'), str, 53) == 5
    @test findprev(isequal('ε'), str, 4) === nothing
end

@testset "string forward search with a single-char string" begin
    @test findfirst("x", astr) === nothing
    @test findfirst("H", astr) == 1:1
    @test findnext("H", astr, 2) === nothing
    @test findfirst("l", astr) == 3:3
    @test findnext("l", astr, 4) == 4:4
    @test findnext("l", astr, 5) == 11:11
    @test findnext("l", astr, 12) === nothing
    @test findfirst("\n", astr) == 14:14
    @test findnext("\n", astr, 15) === nothing

    @test findfirst("z", u8str) === nothing
    @test findfirst("∄", u8str) === nothing
    @test findfirst("∀", u8str) == 1:1
    @test findnext("∀", u8str, 4) === nothing
    @test findfirst("∃", u8str) == 13:13
    @test findnext("∃", u8str, 16) === nothing
    @test findfirst("x", u8str) == 26:26
    @test findnext("x", u8str, 27) == 43:43
    @test findnext("x", u8str, 44) === nothing
    @test findfirst("ε", u8str) == 5:5
    @test findnext("ε", u8str, 7) == 54:54
    @test findnext("ε", u8str, 56) === nothing
end

@testset "findprev backward search with a single-char string" begin
    @test findlast("x", astr) === nothing
    @test findlast("H", astr) == 1:1
    @test findprev("H", astr, 2) == 1:1
    @test findprev("H", astr, 0) === nothing
    @test findlast("l", astr) == 11:11
    @test findprev("l", astr, 10) == 4:4
    @test findprev("l", astr, 4) == 4:4
    @test findprev("l", astr, 3) == 3:3
    @test findprev("l", astr, 2) === nothing
    @test findlast("\n", astr) == 14:14
    @test findprev("\n", astr, 13) === nothing

    @test findlast("z", u8str) === nothing
    @test findlast("∄", u8str) === nothing
    @test findlast("∀", u8str) == 1:1
    @test findprev("∀", u8str, 0) === nothing
    #TODO: setting the limit in the middle of a wide char
    #      makes findnext fail but findprev succeed.
    #      Should findprev fail as well?
    #@test findprev("∀", u8str, 2) === nothing # gives 1:3
    @test findlast("∃", u8str) == 13:13
    @test findprev("∃", u8str, 12) === nothing
    @test findlast("x", u8str) == 43:43
    @test findprev("x", u8str, 42) == 26:26
    @test findprev("x", u8str, 25) === nothing
    @test findlast("ε", u8str) == 54:54
    @test findprev("ε", u8str, 53) == 5:5
    @test findprev("ε", u8str, 4) === nothing
end

@testset "string forward search with a single-char regex" begin
    @test findfirst(r"x", astr) === nothing
    @test findfirst(r"H", astr) == 1:1
    @test findnext(r"H", astr, 2) === nothing
    @test findfirst(r"l", astr) == 3:3
    @test findnext(r"l", astr, 4) == 4:4
    @test findnext(r"l", astr, 5) == 11:11
    @test findnext(r"l", astr, 12) === nothing
    @test findfirst(r"\n", astr) == 14:14
    @test findnext(r"\n", astr, 15) === nothing
    @test findfirst(r"z", u8str) === nothing
    @test findfirst(r"∄", u8str) === nothing
    @test findfirst(r"∀", u8str) == 1:1
    @test findnext(r"∀", u8str, 4) === nothing
    @test findfirst(r"∀", u8str) == findfirst(r"\u2200", u8str)
    @test findnext(r"∀", u8str, 4) == findnext(r"\u2200", u8str, 4)
    @test findfirst(r"∃", u8str) == 13:13
    @test findnext(r"∃", u8str, 16) === nothing
    @test findfirst(r"x", u8str) == 26:26
    @test findnext(r"x", u8str, 27) == 43:43
    @test findnext(r"x", u8str, 44) === nothing
    @test findfirst(r"ε", u8str) == 5:5
    @test findnext(r"ε", u8str, 7) == 54:54
    @test findnext(r"ε", u8str, 56) === nothing
    for i = 1:lastindex(astr)
        @test findnext(r"."s, astr, i) == i:i
    end
    for i = 1:lastindex(u8str)
        if isvalid(u8str,i)
            @test findnext(r"."s, u8str, i) == i:i
        end
    end
end

@testset "string forward search with a zero-char string" begin
    for i = 1:lastindex(astr)
        @test findnext("", astr, i) == i:i-1
    end
    for i = 1:lastindex(u8str)
        @test findnext("", u8str, i) == i:i-1
    end
    @test findfirst("", "") === 1:0
end

@testset "string backward search with a zero-char string" begin
    for i = 1:lastindex(astr)
        @test findprev("", astr, i) == i:i-1
    end
    for i = 1:lastindex(u8str)
        @test findprev("", u8str, i) == i:i-1
    end
    @test findlast("", "") === 1:0
end

@testset "string forward search with a zero-char regex" begin
    for i = 1:lastindex(astr)
        @test findnext(r"", astr, i) == i:i-1
    end
    for i = 1:lastindex(u8str)
        # TODO: should regex search fast-forward invalid indices?
        if isvalid(u8str,i)
            @test findnext(r"", u8str, i) == i:i-1
        end
    end
end

# See the comments in #54579
@testset "Search for invalid chars" begin
    @test findfirst(==('\xff'), "abc\xffde") == 4
    @test findprev(isequal('\xa6'), "abc\xa69", 5) == 4
    @test isnothing(findfirst(==('\xff'), "abcdeæd"))

    @test isnothing(findnext(==('\xa6'), "æ", 1))
    @test isnothing(findprev(==('\xa6'), "æa", 2))
end

@testset "string forward search with a two-char string literal" begin
    @test findfirst("xx", "foo,bar,baz") === nothing
    @test findfirst("fo", "foo,bar,baz") == 1:2
    @test findnext("fo", "foo,bar,baz", 3) === nothing
    @test findfirst("oo", "foo,bar,baz") == 2:3
    @test findnext("oo", "foo,bar,baz", 4) === nothing
    @test findfirst("o,", "foo,bar,baz") == 3:4
    @test findnext("o,", "foo,bar,baz", 5) === nothing
    @test findfirst(",b", "foo,bar,baz") == 4:5
    @test findnext(",b", "foo,bar,baz", 6) == 8:9
    @test findnext(",b", "foo,bar,baz", 10) === nothing
    @test findfirst("az", "foo,bar,baz") == 10:11
    @test findnext("az", "foo,bar,baz", 12) === nothing
end

# See the comments in #54579
@testset "Search for invalid chars" begin
    @test findfirst(==('\xff'), "abc\xffde") == 4
    @test findprev(isequal('\xa6'), "abc\xa69", 5) == 4
    @test isnothing(findfirst(==('\xff'), "abcdeæd"))

    @test isnothing(findnext(==('\xa6'), "æ", 1))
    @test isnothing(findprev(==('\xa6'), "æa", 2))
end

@testset "issue #9365" begin
    # string forward search with a two-char UTF-8 (2 byte) string literal
    @test findfirst("éé", "ééé") == 1:3
    @test findnext("éé", "ééé", 1) == 1:3
    # string forward search with a two-char UTF-8 (3 byte) string literal
    @test findfirst("€€", "€€€") == 1:4
    @test findnext("€€", "€€€", 1) == 1:4
    # string forward search with a two-char UTF-8 (4 byte) string literal
    @test findfirst("\U1f596\U1f596", "\U1f596\U1f596\U1f596") == 1:5
    @test findnext("\U1f596\U1f596", "\U1f596\U1f596\U1f596", 1) == 1:5

    # string forward search with a two-char UTF-8 (2 byte) string literal
    @test findfirst("éé", "éé") == 1:3
    @test findnext("éé", "éé", 1) == 1:3
    # string forward search with a two-char UTF-8 (3 byte) string literal
    @test findfirst("€€", "€€") == 1:4
    @test findnext("€€", "€€", 1) == 1:4
    # string forward search with a two-char UTF-8 (4 byte) string literal
    @test findfirst("\U1f596\U1f596", "\U1f596\U1f596") == 1:5
    @test findnext("\U1f596\U1f596", "\U1f596\U1f596", 1) == 1:5

    # string backward search with a two-char UTF-8 (2 byte) string literal
    @test findlast("éé", "ééé") == 3:5
    @test findprev("éé", "ééé", lastindex("ééé")) == 3:5
    # string backward search with a two-char UTF-8 (3 byte) string literal
    @test findlast("€€", "€€€") == 4:7
    @test findprev("€€", "€€€", lastindex("€€€")) == 4:7
    # string backward search with a two-char UTF-8 (4 byte) string literal
    @test findlast("\U1f596\U1f596", "\U1f596\U1f596\U1f596") == 5:9
    @test findprev("\U1f596\U1f596", "\U1f596\U1f596\U1f596", lastindex("\U1f596\U1f596\U1f596")) == 5:9

    # string backward search with a two-char UTF-8 (2 byte) string literal
    @test findlast("éé", "éé") == 1:3        # should really be 1:4!
    @test findprev("éé", "éé", lastindex("ééé")) == 1:3
    # string backward search with a two-char UTF-8 (3 byte) string literal
    @test findlast("€€", "€€") == 1:4        # should really be 1:6!
    @test findprev("€€", "€€", lastindex("€€€")) == 1:4
    # string backward search with a two-char UTF-8 (4 byte) string literal
    @test findlast("\U1f596\U1f596", "\U1f596\U1f596") == 1:5        # should really be 1:8!
    @test findprev("\U1f596\U1f596", "\U1f596\U1f596", lastindex("\U1f596\U1f596\U1f596")) == 1:5
end

@testset "string backward search with a two-char string literal" begin
    @test findlast("xx", "foo,bar,baz") === nothing
    @test findlast("fo", "foo,bar,baz") == 1:2
    @test findprev("fo", "foo,bar,baz", 1) === nothing
    @test findlast("oo", "foo,bar,baz") == 2:3
    @test findprev("oo", "foo,bar,baz", 2) === nothing
    @test findlast("o,", "foo,bar,baz") == 3:4
    @test findprev("o,", "foo,bar,baz", 1) === nothing
    @test findlast(",b", "foo,bar,baz") == 8:9
    @test findprev(",b", "foo,bar,baz", 6) == 4:5
    @test findprev(",b", "foo,bar,baz", 3) === nothing
    @test findlast("az", "foo,bar,baz") == 10:11
    @test findprev("az", "foo,bar,baz", 10) === nothing
end

@testset "string search with a two-char regex" begin
    @test findfirst(r"xx", "foo,bar,baz") === nothing
    @test findfirst(r"fo", "foo,bar,baz") == 1:2
    @test findnext(r"fo", "foo,bar,baz", 3) === nothing
    @test findfirst(r"oo", "foo,bar,baz") == 2:3
    @test findnext(r"oo", "foo,bar,baz", 4) === nothing
    @test findfirst(r"o,", "foo,bar,baz") == 3:4
    @test findnext(r"o,", "foo,bar,baz", 5) === nothing
    @test findfirst(r",b", "foo,bar,baz") == 4:5
    @test findnext(r",b", "foo,bar,baz", 6) == 8:9
    @test findnext(r",b", "foo,bar,baz", 10) === nothing
    @test findfirst(r"az", "foo,bar,baz") == 10:11
    @test findnext(r"az", "foo,bar,baz", 12) === nothing
end

@testset "occursin/contains" begin
    # occursin with a String and Char needle
    @test occursin("o", "foo")
    @test occursin('o', "foo")
    # occursin in curried form
    @test occursin("foo")("o")
    @test occursin("foo")('o')

    # contains
    @test contains("foo", "o")
    @test contains("foo", 'o')
    # contains in curried form
    @test contains("o")("foo")
    @test contains('o')("foo")

    @test_throws ErrorException "ab" ∈ "abc"
end

@testset "issue #15723" begin
    @test findfirst(isequal('('), "⨳(") == 4
    @test findnext(isequal('('), "(⨳(", 2) == 5
    @test findlast(isequal('('), "(⨳(") == 5
    @test findprev(isequal('('), "(⨳(", 2) == 1

    @test @inferred findall(isequal('a'), "éa") == [3]
    @test @inferred findall(isequal('€'), "€€") == [1, 4]
    @test @inferred isempty(findall(isequal('é'), ""))
end


@testset "issue #18109" begin
    s_18109 = "fooα🐨βcd3"
    @test findlast(isequal('o'), s_18109) == 3
    @test findfirst(isequal('d'), s_18109) == 13
end

@testset "findall (issue #31788)" begin
    @test findall("fooo", "foo") == UnitRange{Int}[]
    @test findall("ing", "Spinning laughing dancing") == [6:8, 15:17, 23:25]
    @test all(findall("", "foo") .=== [1:0, 2:1, 3:2, 4:3]) # use === to compare empty ranges
    @test findall("αβ", "blαh blαβ blαββy") == findall("αβ", "blαh blαβ blαββy", overlap=true) == [9:11, 16:18]
    @test findall("aa", "aaaaaa") == [1:2, 3:4, 5:6]
    @test findall("aa", "aaaaaa", overlap=true) == [1:2, 2:3, 3:4, 4:5, 5:6]
end

@testset "Findall char in string" begin
    @test findall(==('w'), "wabcwewwawk") == [1, 5, 7, 8, 10]
    @test isempty(findall(isequal("w"), "abcde!,"))
    @test findall(==('读'), "联国读大会一九四二月十读日第号决通过并颁布读") == [7, 34, 64]

    # Empty string
    @test isempty(findall(isequal('K'), ""))
    @test isempty(findall(isequal('α'), ""))

    # Finds an invalid char ONLY if it's at a char boundary in the string,
    # i.e. iterating the string would emit the given char.
    @test findall(==('\xfe'), "abκæøc\xfeα\xfeβå!") == [10, 13]
    @test isempty(findall(==('\xaf'), "abκæ读α\xe8\xaf\xfeβå!"))
    @test isempty(findall(==('\xc3'), ";æ"))
end

# issue 37280
@testset "UInt8, Int8 vector" begin
    for T in [Int8, UInt8], VT in [Int8, UInt8]
        A = T[0x40, 0x52, 0x00, 0x52, 0x00]

        for A in (A, @view(A[1:end]), codeunits(String(copyto!(Vector{UInt8}(undef,5), A))))
            @test findfirst(VT[0x30], A) === findfirst(==(VT(0x30)), A) === nothing
            @test findfirst(VT[0x52], A) === 2:2
            @test findfirst(==(VT(0x52)), A) === 2
            @test findlast(VT[0x30], A) === findlast(==(VT(0x30)), A) === nothing
            @test findlast(VT[0x52], A) === 4:4
            @test findlast(==(VT(0x52)), A) === 4
            @test findfirst(iszero, A) === 3 === findprev(iszero, A, 4)
            @test findlast(iszero, A) === 5 === findnext(iszero, A, 4)

            pattern = VT[0x52, 0x00]

            @test findfirst(pattern, A) === 2:3
            @test findnext(pattern, A, 2) === 2:3
            @test findnext(pattern, A, 3) === 4:5
            # 1 idx too far is allowed
            @test findnext(pattern, A, length(A)+1) === nothing
            @test_throws BoundsError findnext(pattern, A, -3)
            @test_throws BoundsError findnext(pattern, A, length(A)+2)

            @test findlast(pattern, A) === 4:5
            @test findprev(pattern, A, 3) === 2:3
            @test findprev(pattern, A, 5) === 4:5
            @test findprev(pattern, A, 2) === nothing
            @test findprev(pattern, A, length(A)+1) == findlast(pattern, A)
            @test findprev(pattern, A, length(A)+2) == findlast(pattern, A)
            @test_throws BoundsError findprev(pattern, A, -3)
        end
    end

    @test findall([0x01, 0x02], [0x03, 0x01, 0x02, 0x01, 0x02, 0x06]) == [2:3, 4:5]
    @test isempty(findall([0x04, 0x05], [0x03, 0x04, 0x06]))
end

# Issue 54578
@testset "No conflation of Int8 and UInt8" begin
    # Work for mixed types if the values are the same
    @test findfirst(==(Int8(1)), [0x01]) == 1
    @test findnext(iszero, Int8[0, -2, 0, -3], 2) == 3
    @test findfirst(Int8[1,4], UInt8[0, 2, 4, 1, 8, 1, 4, 2]) == 6:7
    @test findprev(UInt8[5, 6], Int8[1, 9, 2, 5, 6, 3], 6) == 4:5

    # Returns nothing for the same methods if the values are different,
    # even if the bitpatterns are the same
    @test isnothing(findfirst(==(Int8(-1)), [0xff]))
    @test isnothing(findnext(isequal(0xff), Int8[-1, -2, -1], 2))
    @test isnothing(findfirst(UInt8[0xff, 0xfe], Int8[0, -1, -2, 1, 8, 1, 4, 2]))
    @test isnothing(findprev(UInt8[0xff, 0xfe], Int8[1, 9, 2, -1, -2, 3], 6))
end

@testset "DenseArray with offsets" begin
    isdefined(Main, :OffsetDenseArrays) || @eval Main include("../testhelpers/OffsetDenseArrays.jl")
    OffsetDenseArrays = Main.OffsetDenseArrays

    A = OffsetDenseArrays.OffsetDenseArray(collect(0x61:0x69), 100)
    @test findfirst(==(0x61), A) == 101
    @test findlast(==(0x61), A) == 101
    @test findfirst(==(0x00), A) === nothing

    @test findfirst([0x62, 0x63, 0x64], A) == 102:104
    @test findlast([0x63, 0x64], A) == 103:104
    @test findall([0x62, 0x63], A) == [102:103]

    @test findfirst(iszero, A) === nothing
    A = OffsetDenseArrays.OffsetDenseArray([0x01, 0x02, 0x00, 0x03], -100)
    @test findfirst(iszero, A) == -97
    @test findnext(==(0x02), A, -99) == -98
    @test findnext(==(0x02), A, -97) === nothing
end

# NOTE: The strange edge cases are tested for here, but that does not mean
# they are intentional. Ideally, the behaviour should be changed. See issue 54584
@testset "Edge behaviours of findnext/last" begin
    # Empty haystack causes no errors
    @test isempty(findall(==('\x00'), ""))

    # Findnext errors when i is not a valid index, findprev does not
    @test_throws StringIndexError findnext(==('a'), "æøå", 2)
    @test findprev(==('æ'), "æøå", 4) == 1

    # Findnext errors when i < 1 or i > ncodeunits(s) + 1
    @test_throws BoundsError findnext(==('a'), "abc", 0)
    @test_throws BoundsError findnext(==('a'), "abc", -1)
    @test_throws BoundsError findnext(==('a'), "abc", 5)
    @test_throws BoundsError findnext(==('a'), "æøå", 8)
    @test findnext(==('a'), "æøå", 7) === nothing

    # Findprev errors when i > ncodeunits(s) + 1 or i < 0
    @test findprev(==('a'), "abc", 0) === nothing
    @test findprev(==('æ'), "æøå", 5) == 1
    @test_throws BoundsError findprev(==('a'), "abc", -1)
    @test_throws BoundsError findprev(==('æ'), "æøå", 8)

    # Findprev returns nothing when i == ncodeunits(s) + 1
    @test findprev(==('æ'), "æøå", 7) === nothing
    @test findprev(==('a'), "abc", 4) === nothing
    @test findprev(==('a'), "abc", 3) == 1
end

# issue 32568
for T = (UInt, BigInt)
    for x = (4, 5)
        @test eltype(findnext(r"l", astr, T(x))) == Int
        @test findnext(isequal('l'), astr, T(x)) isa Int
        @test findprev(isequal('l'), astr, T(x)) isa Int
        @test findnext('l', astr, T(x)) isa Int
        @test findprev('l', astr, T(x)) isa Int
    end
    for x = (5, 6)
        @test eltype(findprev(",b", "foo,bar,baz", T(x))) == Int
    end
    for x = (7, 8)
        @test eltype(findnext(",b", "foo,bar,baz", T(x))) == Int
        @test findnext(isletter, astr, T(x)) isa Int
        @test findprev(isletter, astr, T(x)) isa Int
    end
end
