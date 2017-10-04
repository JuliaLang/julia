# This file is a part of Julia. License is MIT: https://julialang.org/license

# some test strings
astr = "Hello, world.\n"
u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

# I think these should give error on 4 also, and "" is not treated
# consistently with SubString("",1,1), nor with Char[]
for ind in (0, 5)
    @test_throws BoundsError search("foo", SubString("",1,1), ind)
    @test_throws BoundsError rsearch("foo", SubString("",1,1), ind)
    @test_throws BoundsError searchindex("foo", SubString("",1,1), ind)
    @test_throws BoundsError rsearchindex("foo", SubString("",1,1), ind)
end

# Note: the commented out tests will be enabled after fixes to make
# sure that search/rsearch/searchindex/rsearchindex are consistent
# no matter what type of AbstractString the second argument is
@test_throws BoundsError search("foo", Char[], 0)
@test_throws BoundsError search("foo", Char[], 5)
# @test_throws BoundsError rsearch("foo", Char[], 0)
@test_throws BoundsError rsearch("foo", Char[], 5)

# @test_throws BoundsError searchindex("foo", Char[], 0)
# @test_throws BoundsError searchindex("foo", Char[], 5)
# @test_throws BoundsError rsearchindex("foo", Char[], 0)
# @test_throws BoundsError rsearchindex("foo", Char[], 5)

# @test_throws ErrorException in("foobar","bar")
@test_throws BoundsError search(b"\x1\x2",0x1,0)
@test rsearchindex(b"foo",b"o",0) == 0
@test rsearchindex(SubString("",1,1),SubString("",1,1)) == 1

@test search(b"foo",'o') == 2
@test rsearch(b"foo",'o') == 3
@test search(b"foó",'ó') == 3
@test rsearch(b"foó",'ó') == 3

# ascii search
for str in [astr, GenericString(astr)]
    @test_throws BoundsError search(str, 'z', 0)
    @test_throws BoundsError search(str, '∀', 0)
    @test search(str, 'x') == 0
    @test search(str, '\0') == 0
    @test search(str, '\u80') == 0
    @test search(str, '∀') == 0
    @test search(str, 'H') == 1
    @test search(str, 'l') == 3
    @test search(str, 'l', 4) == 4
    @test search(str, 'l', 5) == 11
    @test search(str, 'l', 12) == 0
    @test search(str, ',') == 6
    @test search(str, ',', 7) == 0
    @test search(str, '\n') == 14
    @test search(str, '\n', 15) == 0
    @test_throws BoundsError search(str, 'ε', nextind(str,endof(str))+1)
    @test_throws BoundsError search(str, 'a', nextind(str,endof(str))+1)
end

# ascii rsearch
for str in [astr]
    @test rsearch(str, 'x') == 0
    @test rsearch(str, '\0') == 0
    @test rsearch(str, '\u80') == 0
    @test rsearch(str, '∀') == 0
    @test rsearch(str, 'H') == 1
    @test rsearch(str, 'H', 0) == 0
    @test rsearch(str, 'l') == 11
    @test rsearch(str, 'l', 5) == 4
    @test rsearch(str, 'l', 4) == 4
    @test rsearch(str, 'l', 3) == 3
    @test rsearch(str, 'l', 2) == 0
    @test rsearch(str, ',') == 6
    @test rsearch(str, ',', 5) == 0
    @test rsearch(str, '\n') == 14
end

# utf-8 search
for str in (u8str, GenericString(u8str))
    @test_throws BoundsError search(str, 'z', 0)
    @test_throws BoundsError search(str, '∀', 0)
    @test search(str, 'z') == 0
    @test search(str, '\0') == 0
    @test search(str, '\u80') == 0
    @test search(str, '∄') == 0
    @test search(str, '∀') == 1
    @test_throws UnicodeError search(str, '∀', 2)
    @test search(str, '∀', 4) == 0
    @test search(str, '∃') == 13
    @test_throws UnicodeError search(str, '∃', 15)
    @test search(str, '∃', 16) == 0
    @test search(str, 'x') == 26
    @test search(str, 'x', 27) == 43
    @test search(str, 'x', 44) == 0
    @test search(str, 'δ') == 17
    @test_throws UnicodeError search(str, 'δ', 18)
    @test search(str, 'δ', nextind(str,17)) == 33
    @test search(str, 'δ', nextind(str,33)) == 0
    @test search(str, 'ε') == 5
    @test search(str, 'ε', nextind(str,5)) == 54
    @test search(str, 'ε', nextind(str,54)) == 0
    @test search(str, 'ε', nextind(str,endof(str))) == 0
    @test search(str, 'a', nextind(str,endof(str))) == 0
    @test_throws BoundsError search(str, 'ε', nextind(str,endof(str))+1)
    @test_throws BoundsError search(str, 'a', nextind(str,endof(str))+1)
end

# utf-8 rsearch
for str in [u8str]
    @test rsearch(str, 'z') == 0
    @test rsearch(str, '\0') == 0
    @test rsearch(str, '\u80') == 0
    @test rsearch(str, '∄') == 0
    @test rsearch(str, '∀') == 1
    @test rsearch(str, '∀', 0) == 0
    @test rsearch(str, '∃') == 13
    @test rsearch(str, '∃', 14) == 13
    @test rsearch(str, '∃', 13) == 13
    @test rsearch(str, '∃', 12) == 0
    @test rsearch(str, 'x') == 43
    @test rsearch(str, 'x', 42) == 26
    @test rsearch(str, 'x', 25) == 0
    @test rsearch(str, 'δ') == 33
    @test rsearch(str, 'δ', 32) == 17
    @test rsearch(str, 'δ', 16) == 0
    @test rsearch(str, 'ε') == 54
    @test rsearch(str, 'ε', 53) == 5
    @test rsearch(str, 'ε', 4) == 0
end

# string search with a single-char string
@test search(astr, "x") == 0:-1
@test search(astr, "H") == 1:1
@test search(astr, "H", 2) == 0:-1
@test search(astr, "l") == 3:3
@test search(astr, "l", 4) == 4:4
@test search(astr, "l", 5) == 11:11
@test search(astr, "l", 12) == 0:-1
@test search(astr, "\n") == 14:14
@test search(astr, "\n", 15) == 0:-1

@test search(u8str, "z") == 0:-1
@test search(u8str, "∄") == 0:-1
@test search(u8str, "∀") == 1:1
@test search(u8str, "∀", 4) == 0:-1
@test search(u8str, "∃") == 13:13
@test search(u8str, "∃", 16) == 0:-1
@test search(u8str, "x") == 26:26
@test search(u8str, "x", 27) == 43:43
@test search(u8str, "x", 44) == 0:-1
@test search(u8str, "ε") == 5:5
@test search(u8str, "ε", 7) == 54:54
@test search(u8str, "ε", 56) == 0:-1

# string rsearch with a single-char string
@test rsearch(astr, "x") == 0:-1
@test rsearch(astr, "H") == 1:1
@test rsearch(astr, "H", 2) == 1:1
@test rsearch(astr, "H", 0) == 0:-1
@test rsearch(astr, "l") == 11:11
@test rsearch(astr, "l", 10) == 4:4
@test rsearch(astr, "l", 4) == 4:4
@test rsearch(astr, "l", 3) == 3:3
@test rsearch(astr, "l", 2) == 0:-1
@test rsearch(astr, "\n") == 14:14
@test rsearch(astr, "\n", 13) == 0:-1

@test rsearch(u8str, "z") == 0:-1
@test rsearch(u8str, "∄") == 0:-1
@test rsearch(u8str, "∀") == 1:1
@test rsearch(u8str, "∀", 0) == 0:-1
#TODO: setting the limit in the middle of a wide char
#      makes search fail but rsearch succeed.
#      Should rsearch fail as well?
#@test rsearch(u8str, "∀", 2) == 0:-1 # gives 1:3
@test rsearch(u8str, "∃") == 13:13
@test rsearch(u8str, "∃", 12) == 0:-1
@test rsearch(u8str, "x") == 43:43
@test rsearch(u8str, "x", 42) == 26:26
@test rsearch(u8str, "x", 25) == 0:-1
@test rsearch(u8str, "ε") == 54:54
@test rsearch(u8str, "ε", 53) == 5:5
@test rsearch(u8str, "ε", 4) == 0:-1

# string search with a single-char regex
@test search(astr, r"x") == 0:-1
@test search(astr, r"H") == 1:1
@test search(astr, r"H", 2) == 0:-1
@test search(astr, r"l") == 3:3
@test search(astr, r"l", 4) == 4:4
@test search(astr, r"l", 5) == 11:11
@test search(astr, r"l", 12) == 0:-1
@test search(astr, r"\n") == 14:14
@test search(astr, r"\n", 15) == 0:-1
@test search(u8str, r"z") == 0:-1
@test search(u8str, r"∄") == 0:-1
@test search(u8str, r"∀") == 1:1
@test search(u8str, r"∀", 4) == 0:-1
@test search(u8str, r"∀") == search(u8str, r"\u2200")
@test search(u8str, r"∀", 4) == search(u8str, r"\u2200", 4)
@test search(u8str, r"∃") == 13:13
@test search(u8str, r"∃", 16) == 0:-1
@test search(u8str, r"x") == 26:26
@test search(u8str, r"x", 27) == 43:43
@test search(u8str, r"x", 44) == 0:-1
@test search(u8str, r"ε") == 5:5
@test search(u8str, r"ε", 7) == 54:54
@test search(u8str, r"ε", 56) == 0:-1
for i = 1:endof(astr)
    @test search(astr, r"."s, i) == i:i
end
for i = 1:endof(u8str)
    if isvalid(u8str,i)
        @test search(u8str, r"."s, i) == i:i
    end
end

# string search with a zero-char string
for i = 1:endof(astr)
    @test search(astr, "", i) == i:i-1
end
for i = 1:endof(u8str)
    @test search(u8str, "", i) == i:i-1
end
@test search("", "") == 1:0

# string rsearch with a zero-char string
for i = 1:endof(astr)
    @test rsearch(astr, "", i) == i:i-1
end
for i = 1:endof(u8str)
    @test rsearch(u8str, "", i) == i:i-1
end
@test rsearch("", "") == 1:0

# string search with a zero-char regex
for i = 1:endof(astr)
    @test search(astr, r"", i) == i:i-1
end
for i = 1:endof(u8str)
    # TODO: should regex search fast-forward invalid indices?
    if isvalid(u8str,i)
        @test search(u8str, r""s, i) == i:i-1
    end
end

# string search with a two-char string literal
@test search("foo,bar,baz", "xx") == 0:-1
@test search("foo,bar,baz", "fo") == 1:2
@test search("foo,bar,baz", "fo", 3) == 0:-1
@test search("foo,bar,baz", "oo") == 2:3
@test search("foo,bar,baz", "oo", 4) == 0:-1
@test search("foo,bar,baz", "o,") == 3:4
@test search("foo,bar,baz", "o,", 5) == 0:-1
@test search("foo,bar,baz", ",b") == 4:5
@test search("foo,bar,baz", ",b", 6) == 8:9
@test search("foo,bar,baz", ",b", 10) == 0:-1
@test search("foo,bar,baz", "az") == 10:11
@test search("foo,bar,baz", "az", 12) == 0:-1

# issue #9365
# string search with a two-char UTF-8 (2 byte) string literal
@test search("ééé", "éé") == 1:3
@test search("ééé", "éé", 1) == 1:3
# string search with a two-char UTF-8 (3 byte) string literal
@test search("€€€", "€€") == 1:4
@test search("€€€", "€€", 1) == 1:4
# string search with a two-char UTF-8 (4 byte) string literal
@test search("\U1f596\U1f596\U1f596", "\U1f596\U1f596") == 1:5
@test search("\U1f596\U1f596\U1f596", "\U1f596\U1f596", 1) == 1:5

# string search with a two-char UTF-8 (2 byte) string literal
@test search("éé", "éé") == 1:3
@test search("éé", "éé", 1) == 1:3
# string search with a two-char UTF-8 (3 byte) string literal
@test search("€€", "€€") == 1:4
@test search("€€", "€€", 1) == 1:4
# string search with a two-char UTF-8 (4 byte) string literal
@test search("\U1f596\U1f596", "\U1f596\U1f596") == 1:5
@test search("\U1f596\U1f596", "\U1f596\U1f596", 1) == 1:5

# string rsearch with a two-char UTF-8 (2 byte) string literal
@test rsearch("ééé", "éé") == 3:5
@test rsearch("ééé", "éé", endof("ééé")) == 3:5
# string rsearch with a two-char UTF-8 (3 byte) string literal
@test rsearch("€€€", "€€") == 4:7
@test rsearch("€€€", "€€", endof("€€€")) == 4:7
# string rsearch with a two-char UTF-8 (4 byte) string literal
@test rsearch("\U1f596\U1f596\U1f596", "\U1f596\U1f596") == 5:9
@test rsearch("\U1f596\U1f596\U1f596", "\U1f596\U1f596", endof("\U1f596\U1f596\U1f596")) == 5:9

# string rsearch with a two-char UTF-8 (2 byte) string literal
@test rsearch("éé", "éé") == 1:3        # should really be 1:4!
@test rsearch("éé", "éé", endof("ééé")) == 1:3
# string search with a two-char UTF-8 (3 byte) string literal
@test rsearch("€€", "€€") == 1:4        # should really be 1:6!
@test rsearch("€€", "€€", endof("€€€")) == 1:4
# string search with a two-char UTF-8 (4 byte) string literal
@test rsearch("\U1f596\U1f596", "\U1f596\U1f596") == 1:5        # should really be 1:8!
@test rsearch("\U1f596\U1f596", "\U1f596\U1f596", endof("\U1f596\U1f596\U1f596")) == 1:5

# string rsearch with a two-char string literal
@test rsearch("foo,bar,baz", "xx") == 0:-1
@test rsearch("foo,bar,baz", "fo") == 1:2
@test rsearch("foo,bar,baz", "fo", 1) == 0:-1
@test rsearch("foo,bar,baz", "oo") == 2:3
@test rsearch("foo,bar,baz", "oo", 2) == 0:-1
@test rsearch("foo,bar,baz", "o,") == 3:4
@test rsearch("foo,bar,baz", "o,", 1) == 0:-1
@test rsearch("foo,bar,baz", ",b") == 8:9
@test rsearch("foo,bar,baz", ",b", 6) == 4:5
@test rsearch("foo,bar,baz", ",b", 3) == 0:-1
@test rsearch("foo,bar,baz", "az") == 10:11
@test rsearch("foo,bar,baz", "az", 10) == 0:-1

# array rsearch
@test rsearch(UInt8[1,2,3],UInt8[2,3],3) == 2:3
@test rsearch(UInt8[1,2,3],UInt8[2,3],1) == 0:-1

# string search with a two-char regex
@test search("foo,bar,baz", r"xx") == 0:-1
@test search("foo,bar,baz", r"fo") == 1:2
@test search("foo,bar,baz", r"fo", 3) == 0:-1
@test search("foo,bar,baz", r"oo") == 2:3
@test search("foo,bar,baz", r"oo", 4) == 0:-1
@test search("foo,bar,baz", r"o,") == 3:4
@test search("foo,bar,baz", r"o,", 5) == 0:-1
@test search("foo,bar,baz", r",b") == 4:5
@test search("foo,bar,baz", r",b", 6) == 8:9
@test search("foo,bar,baz", r",b", 10) == 0:-1
@test search("foo,bar,baz", r"az") == 10:11
@test search("foo,bar,baz", r"az", 12) == 0:-1

@test searchindex("foo", 'o') == 2
@test searchindex("foo", 'o', 3) == 3

# string searchindex with a two-char UTF-8 (2 byte) string literal
@test searchindex("ééé", "éé") == 1
@test searchindex("ééé", "éé", 1) == 1
# string searchindex with a two-char UTF-8 (3 byte) string literal
@test searchindex("€€€", "€€") == 1
@test searchindex("€€€", "€€", 1) == 1
# string searchindex with a two-char UTF-8 (4 byte) string literal
@test searchindex("\U1f596\U1f596\U1f596", "\U1f596\U1f596") == 1
@test searchindex("\U1f596\U1f596\U1f596", "\U1f596\U1f596", 1) == 1

# string searchindex with a two-char UTF-8 (2 byte) string literal
@test searchindex("éé", "éé") == 1
@test searchindex("éé", "éé", 1) == 1
# string searchindex with a two-char UTF-8 (3 byte) string literal
@test searchindex("€€", "€€") == 1
@test searchindex("€€", "€€", 1) == 1
# string searchindex with a two-char UTF-8 (4 byte) string literal
@test searchindex("\U1f596\U1f596", "\U1f596\U1f596") == 1
@test searchindex("\U1f596\U1f596", "\U1f596\U1f596", 1) == 1

# contains with a String and Char needle
@test contains("foo", "o")
@test contains("foo", 'o')

# string rsearchindex with a two-char UTF-8 (2 byte) string literal
@test rsearchindex("ééé", "éé") == 3
@test rsearchindex("ééé", "éé", endof("ééé")) == 3
# string rsearchindex with a two-char UTF-8 (3 byte) string literal
@test rsearchindex("€€€", "€€") == 4
@test rsearchindex("€€€", "€€", endof("€€€")) == 4
# string rsearchindex with a two-char UTF-8 (4 byte) string literal
@test rsearchindex("\U1f596\U1f596\U1f596", "\U1f596\U1f596") == 5
@test rsearchindex("\U1f596\U1f596\U1f596", "\U1f596\U1f596", endof("\U1f596\U1f596\U1f596")) == 5

# string rsearchindex with a two-char UTF-8 (2 byte) string literal
@test rsearchindex("éé", "éé") == 1
@test rsearchindex("éé", "éé", endof("ééé")) == 1
# string searchindex with a two-char UTF-8 (3 byte) string literal
@test rsearchindex("€€", "€€") == 1
@test rsearchindex("€€", "€€", endof("€€€")) == 1
# string searchindex with a two-char UTF-8 (4 byte) string literal
@test rsearchindex("\U1f596\U1f596", "\U1f596\U1f596") == 1
@test rsearchindex("\U1f596\U1f596", "\U1f596\U1f596", endof("\U1f596\U1f596\U1f596")) == 1

@test_throws ErrorException "ab" ∈ "abc"

# issue #15723
@test findfirst(equalto('('), "⨳(") == 4
@test findnext(equalto('('), "(⨳(", 2) == 5
@test findlast(equalto('('), "(⨳(") == 5
@test findprev(equalto('('), "(⨳(", 2) == 1
