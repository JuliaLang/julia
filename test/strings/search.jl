# This file is a part of Julia. License is MIT: https://julialang.org/license

# some test strings
astr = "Hello, world.\n"
u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

# I think these should give error on 4 also, and "" is not treated
# consistently with SubString("",1,1), nor with Char[]
for ind in (0, 5)
    @test_throws BoundsError findnext(SubString("",1,1), "foo", ind)
    @test_throws BoundsError findprev(SubString("",1,1), "foo", ind)
end

# Note: the commented out test will be enabled after fixes to make
# sure that findnext/findprev are consistent
# no matter what type of AbstractString the second argument is
@test_throws BoundsError findnext(equalto('a'), "foo", 0)
@test_throws BoundsError findnext(occursin(Char[]), "foo", 5)
# @test_throws BoundsError findprev(occursin(Char[]), "foo", 0)
@test_throws BoundsError findprev(occursin(Char[]), "foo", 5)

# @test_throws ErrorException in("foobar","bar")
@test_throws BoundsError findnext(equalto(0x1),b"\x1\x2",0)

# ascii forward search
for str in [astr, GenericString(astr)]
    @test_throws BoundsError findnext(equalto('z'), str, 0)
    @test_throws BoundsError findnext(equalto('∀'), str, 0)
    @test findfirst(equalto('x'), str) == nothing
    @test findfirst(equalto('\0'), str) == nothing
    @test findfirst(equalto('\u80'), str) == nothing
    @test findfirst(equalto('∀'), str) == nothing
    @test findfirst(equalto('H'), str) == 1
    @test findfirst(equalto('l'), str) == 3
    @test findnext(equalto('l'), str, 4) == 4
    @test findnext(equalto('l'), str, 5) == 11
    @test findnext(equalto('l'), str, 12) == nothing
    @test findfirst(equalto(','), str) == 6
    @test findnext(equalto(','), str, 7) == nothing
    @test findfirst(equalto('\n'), str) == 14
    @test findnext(equalto('\n'), str, 15) == nothing
    @test_throws BoundsError findnext(equalto('ε'), str, nextind(str,endof(str))+1)
    @test_throws BoundsError findnext(equalto('a'), str, nextind(str,endof(str))+1)
end

# ascii backward search
for str in [astr]
    @test findlast(equalto('x'), str) == nothing
    @test findlast(equalto('\0'), str) == nothing
    @test findlast(equalto('\u80'), str) == nothing
    @test findlast(equalto('∀'), str) == nothing
    @test findlast(equalto('H'), str) == 1
    @test findprev(equalto('H'), str, 0) == nothing
    @test findlast(equalto('l'), str) == 11
    @test findprev(equalto('l'), str, 5) == 4
    @test findprev(equalto('l'), str, 4) == 4
    @test findprev(equalto('l'), str, 3) == 3
    @test findprev(equalto('l'), str, 2) == nothing
    @test findlast(equalto(','), str) == 6
    @test findprev(equalto(','), str, 5) == nothing
    @test findlast(equalto('\n'), str) == 14
end

# utf-8 forward search
for str in (u8str, GenericString(u8str))
    @test_throws BoundsError findnext(equalto('z'), str, 0)
    @test_throws BoundsError findnext(equalto('∀'), str, 0)
    @test findfirst(equalto('z'), str) == nothing
    @test findfirst(equalto('\0'), str) == nothing
    @test findfirst(equalto('\u80'), str) == nothing
    @test findfirst(equalto('∄'), str) == nothing
    @test findfirst(equalto('∀'), str) == 1
    @test_throws StringIndexError findnext(equalto('∀'), str, 2)
    @test findnext(equalto('∀'), str, 4) == nothing
    @test findfirst(equalto('∃'), str) == 13
    @test_throws StringIndexError findnext(equalto('∃'), str, 15)
    @test findnext(equalto('∃'), str, 16) == nothing
    @test findfirst(equalto('x'), str) == 26
    @test findnext(equalto('x'), str, 27) == 43
    @test findnext(equalto('x'), str, 44) == nothing
    @test findfirst(equalto('δ'), str) == 17
    @test_throws StringIndexError findnext(equalto('δ'), str, 18)
    @test findnext(equalto('δ'), str, nextind(str,17)) == 33
    @test findnext(equalto('δ'), str, nextind(str,33)) == nothing
    @test findfirst(equalto('ε'), str) == 5
    @test findnext(equalto('ε'), str, nextind(str,5)) == 54
    @test findnext(equalto('ε'), str, nextind(str,54)) == nothing
    @test findnext(equalto('ε'), str, nextind(str,endof(str))) == nothing
    @test findnext(equalto('a'), str, nextind(str,endof(str))) == nothing
    @test_throws BoundsError findnext(equalto('ε'), str, nextind(str,endof(str))+1)
    @test_throws BoundsError findnext(equalto('a'), str, nextind(str,endof(str))+1)
end

# utf-8 backward search
for str in [u8str]
    @test findlast(equalto('z'), str) == nothing
    @test findlast(equalto('\0'), str) == nothing
    @test findlast(equalto('\u80'), str) == nothing
    @test findlast(equalto('∄'), str) == nothing
    @test findlast(equalto('∀'), str) == 1
    @test findprev(equalto('∀'), str, 0) == nothing
    @test findlast(equalto('∃'), str) == 13
    @test findprev(equalto('∃'), str, 14) == 13
    @test findprev(equalto('∃'), str, 13) == 13
    @test findprev(equalto('∃'), str, 12) == nothing
    @test findlast(equalto('x'), str) == 43
    @test findprev(equalto('x'), str, 42) == 26
    @test findprev(equalto('x'), str, 25) == nothing
    @test findlast(equalto('δ'), str) == 33
    @test findprev(equalto('δ'), str, 32) == 17
    @test findprev(equalto('δ'), str, 16) == nothing
    @test findlast(equalto('ε'), str) == 54
    @test findprev(equalto('ε'), str, 53) == 5
    @test findprev(equalto('ε'), str, 4) == nothing
end

# string forward search with a single-char string
@test findfirst("x", astr) == 0:-1
@test findfirst("H", astr) == 1:1
@test findnext("H", astr, 2) == 0:-1
@test findfirst("l", astr) == 3:3
@test findnext("l", astr, 4) == 4:4
@test findnext("l", astr, 5) == 11:11
@test findnext("l", astr, 12) == 0:-1
@test findfirst("\n", astr) == 14:14
@test findnext("\n", astr, 15) == 0:-1

@test findfirst("z", u8str) == 0:-1
@test findfirst("∄", u8str) == 0:-1
@test findfirst("∀", u8str) == 1:1
@test findnext("∀", u8str, 4) == 0:-1
@test findfirst("∃", u8str) == 13:13
@test findnext("∃", u8str, 16) == 0:-1
@test findfirst("x", u8str) == 26:26
@test findnext("x", u8str, 27) == 43:43
@test findnext("x", u8str, 44) == 0:-1
@test findfirst("ε", u8str) == 5:5
@test findnext("ε", u8str, 7) == 54:54
@test findnext("ε", u8str, 56) == 0:-1

# strifindprev  backward search with a single-char string
@test findlast("x", astr) == 0:-1
@test findlast("H", astr) == 1:1
@test findprev("H", astr, 2) == 1:1
@test findprev("H", astr, 0) == 0:-1
@test findlast("l", astr) == 11:11
@test findprev("l", astr, 10) == 4:4
@test findprev("l", astr, 4) == 4:4
@test findprev("l", astr, 3) == 3:3
@test findprev("l", astr, 2) == 0:-1
@test findlast("\n", astr) == 14:14
@test findprev("\n", astr, 13) == 0:-1

@test findlast("z", u8str) == 0:-1
@test findlast("∄", u8str) == 0:-1
@test findlast("∀", u8str) == 1:1
@test findprev("∀", u8str, 0) == 0:-1
#TODO: setting the limit in the middle of a wide char
#      makes findnext fail but findprev succeed.
#      Should findprev fail as well?
#@test findprev("∀", u8str, 2) == 0:-1 # gives 1:3
@test findlast("∃", u8str) == 13:13
@test findprev("∃", u8str, 12) == 0:-1
@test findlast("x", u8str) == 43:43
@test findprev("x", u8str, 42) == 26:26
@test findprev("x", u8str, 25) == 0:-1
@test findlast("ε", u8str) == 54:54
@test findprev("ε", u8str, 53) == 5:5
@test findprev("ε", u8str, 4) == 0:-1

# string forward search with a single-char regex
@test findfirst(r"x", astr) == 0:-1
@test findfirst(r"H", astr) == 1:1
@test findnext(r"H", astr, 2) == 0:-1
@test findfirst(r"l", astr) == 3:3
@test findnext(r"l", astr, 4) == 4:4
@test findnext(r"l", astr, 5) == 11:11
@test findnext(r"l", astr, 12) == 0:-1
@test findfirst(r"\n", astr) == 14:14
@test findnext(r"\n", astr, 15) == 0:-1
@test findfirst(r"z", u8str) == 0:-1
@test findfirst(r"∄", u8str) == 0:-1
@test findfirst(r"∀", u8str) == 1:1
@test findnext(r"∀", u8str, 4) == 0:-1
@test findfirst(r"∀", u8str) == findfirst(r"\u2200", u8str)
@test findnext(r"∀", u8str, 4) == findnext(r"\u2200", u8str, 4)
@test findfirst(r"∃", u8str) == 13:13
@test findnext(r"∃", u8str, 16) == 0:-1
@test findfirst(r"x", u8str) == 26:26
@test findnext(r"x", u8str, 27) == 43:43
@test findnext(r"x", u8str, 44) == 0:-1
@test findfirst(r"ε", u8str) == 5:5
@test findnext(r"ε", u8str, 7) == 54:54
@test findnext(r"ε", u8str, 56) == 0:-1
for i = 1:endof(astr)
    @test findnext(r"."s, astr, i) == i:i
end
for i = 1:endof(u8str)
    if isvalid(u8str,i)
        @test findnext(r"."s, u8str, i) == i:i
    end
end

# string forward search with a zero-char string
for i = 1:endof(astr)
    @test findnext("", astr, i) == i:i-1
end
for i = 1:endof(u8str)
    @test findnext("", u8str, i) == i:i-1
end
@test findfirst("", "") == 1:0

# string backward search with a zero-char string
for i = 1:endof(astr)
    @test findprev("", astr, i) == i:i-1
end
for i = 1:endof(u8str)
    @test findprev("", u8str, i) == i:i-1
end
@test findlast("", "") == 1:0

# string forward search with a zero-char regex
for i = 1:endof(astr)
    @test findnext(r"", astr, i) == i:i-1
end
for i = 1:endof(u8str)
    # TODO: should regex search fast-forward invalid indices?
    if isvalid(u8str,i)
        @test findnext(r"", u8str, i) == i:i-1
    end
end

# string forward search with a two-char string literal
@test findfirst("xx", "foo,bar,baz") == 0:-1
@test findfirst("fo", "foo,bar,baz") == 1:2
@test findnext("fo", "foo,bar,baz", 3) == 0:-1
@test findfirst("oo", "foo,bar,baz") == 2:3
@test findnext("oo", "foo,bar,baz", 4) == 0:-1
@test findfirst("o,", "foo,bar,baz") == 3:4
@test findnext("o,", "foo,bar,baz", 5) == 0:-1
@test findfirst(",b", "foo,bar,baz") == 4:5
@test findnext(",b", "foo,bar,baz", 6) == 8:9
@test findnext(",b", "foo,bar,baz", 10) == 0:-1
@test findfirst("az", "foo,bar,baz") == 10:11
@test findnext("az", "foo,bar,baz", 12) == 0:-1

# issue #9365
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
@test findprev("éé", "ééé", endof("ééé")) == 3:5
# string backward search with a two-char UTF-8 (3 byte) string literal
@test findlast("€€", "€€€") == 4:7
@test findprev("€€", "€€€", endof("€€€")) == 4:7
# string backward search with a two-char UTF-8 (4 byte) string literal
@test findlast("\U1f596\U1f596", "\U1f596\U1f596\U1f596") == 5:9
@test findprev("\U1f596\U1f596", "\U1f596\U1f596\U1f596", endof("\U1f596\U1f596\U1f596")) == 5:9

# string backward search with a two-char UTF-8 (2 byte) string literal
@test findlast("éé", "éé") == 1:3        # should really be 1:4!
@test findprev("éé", "éé", endof("ééé")) == 1:3
# string backward search with a two-char UTF-8 (3 byte) string literal
@test findlast("€€", "€€") == 1:4        # should really be 1:6!
@test findprev("€€", "€€", endof("€€€")) == 1:4
# string backward search with a two-char UTF-8 (4 byte) string literal
@test findlast("\U1f596\U1f596", "\U1f596\U1f596") == 1:5        # should really be 1:8!
@test findprev("\U1f596\U1f596", "\U1f596\U1f596", endof("\U1f596\U1f596\U1f596")) == 1:5

# string backward search with a two-char string literal
@test findlast("xx", "foo,bar,baz") == 0:-1
@test findlast("fo", "foo,bar,baz") == 1:2
@test findprev("fo", "foo,bar,baz", 1) == 0:-1
@test findlast("oo", "foo,bar,baz") == 2:3
@test findprev("oo", "foo,bar,baz", 2) == 0:-1
@test findlast("o,", "foo,bar,baz") == 3:4
@test findprev("o,", "foo,bar,baz", 1) == 0:-1
@test findlast(",b", "foo,bar,baz") == 8:9
@test findprev(",b", "foo,bar,baz", 6) == 4:5
@test findprev(",b", "foo,bar,baz", 3) == 0:-1
@test findlast("az", "foo,bar,baz") == 10:11
@test findprev("az", "foo,bar,baz", 10) == 0:-1

# string search with a two-char regex
@test findfirst(r"xx", "foo,bar,baz") == 0:-1
@test findfirst(r"fo", "foo,bar,baz") == 1:2
@test findnext(r"fo", "foo,bar,baz", 3) == 0:-1
@test findfirst(r"oo", "foo,bar,baz") == 2:3
@test findnext(r"oo", "foo,bar,baz", 4) == 0:-1
@test findfirst(r"o,", "foo,bar,baz") == 3:4
@test findnext(r"o,", "foo,bar,baz", 5) == 0:-1
@test findfirst(r",b", "foo,bar,baz") == 4:5
@test findnext(r",b", "foo,bar,baz", 6) == 8:9
@test findnext(r",b", "foo,bar,baz", 10) == 0:-1
@test findfirst(r"az", "foo,bar,baz") == 10:11
@test findnext(r"az", "foo,bar,baz", 12) == 0:-1

# contains with a String and Char needle
@test contains("foo", "o")
@test contains("foo", 'o')

@test_throws ErrorException "ab" ∈ "abc"

# issue #15723
@test findfirst(equalto('('), "⨳(") == 4
@test findnext(equalto('('), "(⨳(", 2) == 5
@test findlast(equalto('('), "(⨳(") == 5
@test findprev(equalto('('), "(⨳(", 2) == 1

@test @inferred find(equalto('a'), "éa") == [3]
@test @inferred find(equalto('€'), "€€") == [1, 4]
@test @inferred isempty(find(equalto('é'), ""))