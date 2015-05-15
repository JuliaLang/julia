# This file is a part of Julia. License is MIT: http://julialang.org/license

# string escaping & unescaping
cx = Any[
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
]

for i = 1:size(cx,1)
    @test cx[i,1] == convert(UInt32, cx[i,2])
    @test string(cx[i,2]) == unescape_string(cx[i,3])
    if isascii(cx[i,2]) || !isprint(cx[i,2])
        @test cx[i,3] == escape_string(string(cx[i,2]))
    end
    for j = 1:size(cx,1)
        str = string(cx[i,2], cx[j,2])
        @test str == unescape_string(escape_string(str))
    end
end

for i = 0:0x7f, p = ["","\0","x","xxx","\x7f","\uFF","\uFFF",
                     "\uFFFF","\U10000","\U10FFF","\U10FFFF"]
    c = Char(i)
    cp = string(c,p)
    op = string(Char(div(i,8)), oct(i%8), p)
    hp = string(Char(div(i,16)), hex(i%16), p)
    @test string(unescape_string(string("\\",oct(i,1),p))) == cp
    @test string(unescape_string(string("\\",oct(i,2),p))) == cp
    @test string(unescape_string(string("\\",oct(i,3),p))) == cp
    @test string(unescape_string(string("\\",oct(i,4),p))) == op
    @test string(unescape_string(string("\\x",hex(i,1),p))) == cp
    @test string(unescape_string(string("\\x",hex(i,2),p))) == cp
    @test string(unescape_string(string("\\x",hex(i,3),p))) == hp
end

@test "\z" == unescape_string("\z") == "z"
@test "\X" == unescape_string("\X") == "X"
@test "\AbC" == unescape_string("\AbC") == "AbC"

@test "\0" == unescape_string("\\0")
@test "\1" == unescape_string("\\1")
@test "\7" == unescape_string("\\7")
@test "\0x" == unescape_string("\\0x")
@test "\1x" == unescape_string("\\1x")
@test "\7x" == unescape_string("\\7x")
@test "\00" == unescape_string("\\00")
@test "\01" == unescape_string("\\01")
@test "\07" == unescape_string("\\07")
@test "\70" == unescape_string("\\70")
@test "\71" == unescape_string("\\71")
@test "\77" == unescape_string("\\77")
@test "\00x" == unescape_string("\\00x")
@test "\01x" == unescape_string("\\01x")
@test "\07x" == unescape_string("\\07x")
@test "\70x" == unescape_string("\\70x")
@test "\71x" == unescape_string("\\71x")
@test "\77x" == unescape_string("\\77x")
@test "\000" == unescape_string("\\000")
@test "\001" == unescape_string("\\001")
@test "\007" == unescape_string("\\007")
@test "\070" == unescape_string("\\070")
@test "\071" == unescape_string("\\071")
@test "\077" == unescape_string("\\077")
@test "\170" == unescape_string("\\170")
@test "\171" == unescape_string("\\171")
@test "\177" == unescape_string("\\177")
@test "\0001" == unescape_string("\\0001")
@test "\0011" == unescape_string("\\0011")
@test "\0071" == unescape_string("\\0071")
@test "\0701" == unescape_string("\\0701")
@test "\0711" == unescape_string("\\0711")
@test "\0771" == unescape_string("\\0771")
@test "\1701" == unescape_string("\\1701")
@test "\1711" == unescape_string("\\1711")
@test "\1771" == unescape_string("\\1771")

@test "\x0" == unescape_string("\\x0")
@test "\x1" == unescape_string("\\x1")
@test "\xf" == unescape_string("\\xf")
@test "\xF" == unescape_string("\\xF")
@test "\x0x" == unescape_string("\\x0x")
@test "\x1x" == unescape_string("\\x1x")
@test "\xfx" == unescape_string("\\xfx")
@test "\xFx" == unescape_string("\\xFx")
@test "\x00" == unescape_string("\\x00")
@test "\x01" == unescape_string("\\x01")
@test "\x0f" == unescape_string("\\x0f")
@test "\x0F" == unescape_string("\\x0F")

# integer parsing
@test is(parse(Int32,"0",36),Int32(0))
@test is(parse(Int32,"1",36),Int32(1))
@test is(parse(Int32,"9",36),Int32(9))
@test is(parse(Int32,"A",36),Int32(10))
@test is(parse(Int32,"a",36),Int32(10))
@test is(parse(Int32,"B",36),Int32(11))
@test is(parse(Int32,"b",36),Int32(11))
@test is(parse(Int32,"F",36),Int32(15))
@test is(parse(Int32,"f",36),Int32(15))
@test is(parse(Int32,"Z",36),Int32(35))
@test is(parse(Int32,"z",36),Int32(35))

@test parse(Int,"0") == 0
@test parse(Int,"-0") == 0
@test parse(Int,"1") == 1
@test parse(Int,"-1") == -1
@test parse(Int,"9") == 9
@test parse(Int,"-9") == -9
@test parse(Int,"10") == 10
@test parse(Int,"-10") == -10
@test parse(Int64,"3830974272") == 3830974272
@test parse(Int64,"-3830974272") == -3830974272
@test parse(Int,'3') == 3
@test parse(Int,'3', 8) == 3

parsebin(s) = parse(Int,s,2)
parseoct(s) = parse(Int,s,8)
parsehex(s) = parse(Int,s,16)

@test parsebin("0") == 0
@test parsebin("-0") == 0
@test parsebin("1") == 1
@test parsebin("-1") == -1
@test parsebin("10") == 2
@test parsebin("-10") == -2
@test parsebin("11") == 3
@test parsebin("-11") == -3
@test parsebin("1111000011110000111100001111") == 252645135
@test parsebin("-1111000011110000111100001111") == -252645135

@test parseoct("0") == 0
@test parseoct("-0") == 0
@test parseoct("1") == 1
@test parseoct("-1") == -1
@test parseoct("7") == 7
@test parseoct("-7") == -7
@test parseoct("10") == 8
@test parseoct("-10") == -8
@test parseoct("11") == 9
@test parseoct("-11") == -9
@test parseoct("72") == 58
@test parseoct("-72") == -58
@test parseoct("3172207320") == 434704080
@test parseoct("-3172207320") == -434704080

@test parsehex("0") == 0
@test parsehex("-0") == 0
@test parsehex("1") == 1
@test parsehex("-1") == -1
@test parsehex("9") == 9
@test parsehex("-9") == -9
@test parsehex("a") == 10
@test parsehex("-a") == -10
@test parsehex("f") == 15
@test parsehex("-f") == -15
@test parsehex("10") == 16
@test parsehex("-10") == -16
@test parsehex("0BADF00D") == 195948557
@test parsehex("-0BADF00D") == -195948557
@test parse(Int64,"BADCAB1E",16) == 3135023902
@test parse(Int64,"-BADCAB1E",16) == -3135023902
@test parse(Int64,"CafeBabe",16) == 3405691582
@test parse(Int64,"-CafeBabe",16) == -3405691582
@test parse(Int64,"DeadBeef",16) == 3735928559
@test parse(Int64,"-DeadBeef",16) == -3735928559

@test parse(Int,"2\n") == 2
@test parse(Int,"   2 \n ") == 2
@test parse(Int," 2 ") == 2
@test parse(Int,"2 ") == 2
@test parse(Int," 2") == 2
@test parse(Int,"+2\n") == 2
@test parse(Int,"-2") == -2
@test_throws ArgumentError parse(Int,"   2 \n 0")
@test_throws ArgumentError parse(Int,"2x")
@test_throws ArgumentError parse(Int,"-")

# multibyte spaces
@test parse(Int, "3\u2003\u202F") == 3
@test_throws ArgumentError parse(Int, "3\u2003\u202F,")

@test parse(Int,'a') == 10
@test_throws ArgumentError parse(Int,typemax(Char))

@test parse(Int,"1234") == 1234
@test parse(Int,"0x1234") == 0x1234
@test parse(Int,"0o1234") == 0o1234
@test parse(Int,"0b1011") == 0b1011
@test parse(Int,"-1234") == -1234
@test parse(Int,"-0x1234") == -Int(0x1234)
@test parse(Int,"-0o1234") == -Int(0o1234)
@test parse(Int,"-0b1011") == -Int(0b1011)

## FIXME: #4905, do these tests for Int128/UInt128!
for T in (Int8, Int16, Int32, Int64)
    @test parse(T,string(typemin(T))) == typemin(T)
    @test parse(T,string(typemax(T))) == typemax(T)
    @test_throws OverflowError parse(T,string(big(typemin(T))-1))
    @test_throws OverflowError parse(T,string(big(typemax(T))+1))
end

for T in (UInt8,UInt16,UInt32,UInt64)
    @test parse(T,string(typemin(T))) == typemin(T)
    @test parse(T,string(typemax(T))) == typemax(T)
    @test_throws ArgumentError parse(T,string(big(typemin(T))-1))
    @test_throws OverflowError parse(T,string(big(typemax(T))+1))
end

# string manipulation
@test strip("\t  hi   \n") == "hi"

# some test strings
astr = "Hello, world.\n"
u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

# ascii search
for str in [astr, Base.GenericString(astr)]
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
for str in (u8str, Base.GenericString(u8str))
    @test search(str, 'z') == 0
    @test search(str, '\0') == 0
    @test search(str, '\u80') == 0
    @test search(str, '∄') == 0
    @test search(str, '∀') == 1
    @test search(str, '∀', 2) == 0
    @test search(str, '∃') == 13
    @test search(str, '∃', 14) == 0
    @test search(str, 'x') == 26
    @test search(str, 'x', 27) == 43
    @test search(str, 'x', 44) == 0
    @test search(str, 'δ') == 17
    @test search(str, 'δ', 18) == 33
    @test search(str, 'δ', 34) == 0
    @test search(str, 'ε') == 5
    @test search(str, 'ε', 6) == 54
    @test search(str, 'ε', 55) == 0
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

# split
@test isequal(split("foo,bar,baz", 'x'), ["foo,bar,baz"])
@test isequal(split("foo,bar,baz", ','), ["foo","bar","baz"])
@test isequal(split("foo,bar,baz", ","), ["foo","bar","baz"])
@test isequal(split("foo,bar,baz", r","), ["foo","bar","baz"])
@test isequal(split("foo,bar,baz", ','; limit=0), ["foo","bar","baz"])
@test isequal(split("foo,bar,baz", ','; limit=1), ["foo,bar,baz"])
@test isequal(split("foo,bar,baz", ','; limit=2), ["foo","bar,baz"])
@test isequal(split("foo,bar,baz", ','; limit=3), ["foo","bar","baz"])
@test isequal(split("foo,bar", "o,b"), ["fo","ar"])

@test isequal(split("", ','), [""])
@test isequal(split(",", ','), ["",""])
@test isequal(split(",,", ','), ["","",""])
@test isequal(split("", ','  ; keep=false), [])
@test isequal(split(",", ',' ; keep=false), [])
@test isequal(split(",,", ','; keep=false), [])

@test isequal(split("a b c"), ["a","b","c"])
@test isequal(split("a  b \t c\n"), ["a","b","c"])

@test isequal(rsplit("foo,bar,baz", 'x'), ["foo,bar,baz"])
@test isequal(rsplit("foo,bar,baz", ','), ["foo","bar","baz"])
@test isequal(rsplit("foo,bar,baz", ","), ["foo","bar","baz"])
@test isequal(rsplit("foo,bar,baz", ','; limit=0), ["foo","bar","baz"])
@test isequal(rsplit("foo,bar,baz", ','; limit=1), ["foo,bar,baz"])
@test isequal(rsplit("foo,bar,baz", ','; limit=2), ["foo,bar","baz"])
@test isequal(rsplit("foo,bar,baz", ','; limit=3), ["foo","bar","baz"])
@test isequal(rsplit("foo,bar", "o,b"), ["fo","ar"])

@test isequal(rsplit("", ','), [""])
@test isequal(rsplit(",", ','), ["",""])
@test isequal(rsplit(",,", ','), ["","",""])
@test isequal(rsplit(",,", ','; limit=2), [",",""])
@test isequal(rsplit("", ','  ; keep=false), [])
@test isequal(rsplit(",", ',' ; keep=false), [])
@test isequal(rsplit(",,", ','; keep=false), [])

#@test isequal(rsplit("a b c"), ["a","b","c"])
#@test isequal(rsplit("a  b \t c\n"), ["a","b","c"])

let str = "a.:.ba..:..cba.:.:.dcba.:."
@test isequal(split(str, ".:."), ["a","ba.",".cba",":.dcba",""])
@test isequal(split(str, ".:."; keep=false), ["a","ba.",".cba",":.dcba"])
@test isequal(split(str, ".:."), ["a","ba.",".cba",":.dcba",""])
@test isequal(split(str, r"\.(:\.)+"), ["a","ba.",".cba","dcba",""])
@test isequal(split(str, r"\.(:\.)+"; keep=false), ["a","ba.",".cba","dcba"])
@test isequal(split(str, r"\.+:\.+"), ["a","ba","cba",":.dcba",""])
@test isequal(split(str, r"\.+:\.+"; keep=false), ["a","ba","cba",":.dcba"])

@test isequal(rsplit(str, ".:."), ["a","ba.",".cba.:","dcba",""])
@test isequal(rsplit(str, ".:."; keep=false), ["a","ba.",".cba.:","dcba"])
@test isequal(rsplit(str, ".:."; limit=2), ["a.:.ba..:..cba.:.:.dcba", ""])
@test isequal(rsplit(str, ".:."; limit=3), ["a.:.ba..:..cba.:", "dcba", ""])
@test isequal(rsplit(str, ".:."; limit=4), ["a.:.ba.", ".cba.:", "dcba", ""])
@test isequal(rsplit(str, ".:."; limit=5), ["a", "ba.", ".cba.:", "dcba", ""])
@test isequal(rsplit(str, ".:."; limit=6), ["a", "ba.", ".cba.:", "dcba", ""])
end

# zero-width splits
@test isequal(rsplit("", ""), [""])

@test isequal(split("", ""), [""])
@test isequal(split("", r""), [""])
@test isequal(split("abc", ""), ["a","b","c"])
@test isequal(split("abc", r""), ["a","b","c"])
@test isequal(split("abcd", r"b?"), ["a","c","d"])
@test isequal(split("abcd", r"b*"), ["a","c","d"])
@test isequal(split("abcd", r"b+"), ["a","cd"])
@test isequal(split("abcd", r"b?c?"), ["a","d"])
@test isequal(split("abcd", r"[bc]?"), ["a","","d"])
@test isequal(split("abcd", r"a*"), ["","b","c","d"])
@test isequal(split("abcd", r"a+"), ["","bcd"])
@test isequal(split("abcd", r"d*"), ["a","b","c",""])
@test isequal(split("abcd", r"d+"), ["abc",""])
@test isequal(split("abcd", r"[ad]?"), ["","b","c",""])

# replace
@test replace("foobar", 'o', '0') == "f00bar"
@test replace("foobar", 'o', '0', 1) == "f0obar"
@test replace("foobar", 'o', "") == "fbar"
@test replace("foobar", 'o', "", 1) == "fobar"
@test replace("foobar", 'f', 'F') == "Foobar"
@test replace("foobar", 'r', 'R') == "foobaR"

@test replace("foofoofoo", "foo", "bar") == "barbarbar"
@test replace("foobarfoo", "foo", "baz") == "bazbarbaz"
@test replace("barfoofoo", "foo", "baz") == "barbazbaz"

@test replace("", "", "") == ""
@test replace("", "", "x") == "x"
@test replace("", "x", "y") == ""

@test replace("abcd", "", "^") == "^a^b^c^d^"
@test replace("abcd", "b", "^") == "a^cd"
@test replace("abcd", r"b?", "^") == "^a^c^d^"
@test replace("abcd", r"b+", "^") == "a^cd"
@test replace("abcd", r"b?c?", "^") == "^a^d^"
@test replace("abcd", r"[bc]?", "^") == "^a^^d^"

@test replace("foobarfoo", r"(fo|ba)", "xx") == "xxoxxrxxo"
@test replace("foobarfoo", r"(foo|ba)", "bar") == "barbarrbar"

@test replace("foobar", 'o', 'ø') == "føøbar"
@test replace("foobar", 'o', 'ø', 1) == "føobar"
@test replace("føøbar", 'ø', 'o') == "foobar"
@test replace("føøbar", 'ø', 'o', 1) == "foøbar"
@test replace("føøbar", 'ø', 'ö') == "fööbar"
@test replace("føøbar", 'ø', 'ö', 1) == "föøbar"
@test replace("føøbar", 'ø', "") == "fbar"
@test replace("føøbar", 'ø', "", 1) == "føbar"
@test replace("føøbar", 'f', 'F') == "Føøbar"
@test replace("ḟøøbar", 'ḟ', 'F') == "Føøbar"
@test replace("føøbar", 'f', 'Ḟ') == "Ḟøøbar"
@test replace("ḟøøbar", 'ḟ', 'Ḟ') == "Ḟøøbar"
@test replace("føøbar", 'r', 'R') == "føøbaR"
@test replace("føøbaṙ", 'ṙ', 'R') == "føøbaR"
@test replace("føøbar", 'r', 'Ṙ') == "føøbaṘ"
@test replace("føøbaṙ", 'ṙ', 'Ṙ') == "føøbaṘ"

@test replace("ḟøøḟøøḟøø", "ḟøø", "bar") == "barbarbar"
@test replace("ḟøøbarḟøø", "ḟøø", "baz") == "bazbarbaz"
@test replace("barḟøøḟøø", "ḟøø", "baz") == "barbazbaz"

@test replace("foofoofoo", "foo", "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
@test replace("fooƀäṙfoo", "foo", "baz") == "bazƀäṙbaz"
@test replace("ƀäṙfoofoo", "foo", "baz") == "ƀäṙbazbaz"

@test replace("foofoofoo", "foo", "bar") == "barbarbar"
@test replace("foobarfoo", "foo", "ƀäż") == "ƀäżbarƀäż"
@test replace("barfoofoo", "foo", "ƀäż") == "barƀäżƀäż"

@test replace("ḟøøḟøøḟøø", "ḟøø", "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
@test replace("ḟøøƀäṙḟøø", "ḟøø", "baz") == "bazƀäṙbaz"
@test replace("ƀäṙḟøøḟøø", "ḟøø", "baz") == "ƀäṙbazbaz"

@test replace("ḟøøḟøøḟøø", "ḟøø", "bar") == "barbarbar"
@test replace("ḟøøbarḟøø", "ḟøø", "ƀäż") == "ƀäżbarƀäż"
@test replace("barḟøøḟøø", "ḟøø", "ƀäż") == "barƀäżƀäż"

@test replace("ḟøøḟøøḟøø", "ḟøø", "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
@test replace("ḟøøƀäṙḟøø", "ḟøø", "ƀäż") == "ƀäżƀäṙƀäż"
@test replace("ƀäṙḟøøḟøø", "ḟøø", "ƀäż") == "ƀäṙƀäżƀäż"

@test replace("", "", "ẍ") == "ẍ"
@test replace("", "ẍ", "ÿ") == ""

@test replace("äƀçđ", "", "π") == "πäπƀπçπđπ"
@test replace("äƀçđ", "ƀ", "π") == "äπçđ"
@test replace("äƀçđ", r"ƀ?", "π") == "πäπçπđπ"
@test replace("äƀçđ", r"ƀ+", "π") == "äπçđ"
@test replace("äƀçđ", r"ƀ?ç?", "π") == "πäπđπ"
@test replace("äƀçđ", r"[ƀç]?", "π") == "πäππđπ"

@test replace("foobarfoo", r"(fo|ba)", "ẍẍ") == "ẍẍoẍẍrẍẍo"

@test replace("ḟøøbarḟøø", r"(ḟø|ba)", "xx") == "xxøxxrxxø"
@test replace("ḟøøbarḟøø", r"(ḟøø|ba)", "bar") == "barbarrbar"

@test replace("fooƀäṙfoo", r"(fo|ƀä)", "xx") == "xxoxxṙxxo"
@test replace("fooƀäṙfoo", r"(foo|ƀä)", "ƀäṙ") == "ƀäṙƀäṙṙƀäṙ"

@test replace("ḟøøƀäṙḟøø", r"(ḟø|ƀä)", "xx") == "xxøxxṙxxø"
@test replace("ḟøøƀäṙḟøø", r"(ḟøø|ƀä)", "ƀäṙ") == "ƀäṙƀäṙṙƀäṙ"

# lower and upper
@test uppercase("aBc") == "ABC"
@test uppercase('A') == 'A'
@test uppercase('a') == 'A'
@test lowercase("AbC") == "abc"
@test lowercase('A') == 'a'
@test lowercase('a') == 'a'
@test ucfirst("Abc") == "Abc"
@test ucfirst("abc") == "Abc"
@test lcfirst("ABC") == "aBC"
@test lcfirst("aBC") == "aBC"

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

# RepStrings and SubStrings
u8str2 = u8str^2
len_u8str = length(u8str)
slen_u8str = length(u8str)
len_u8str2 = length(u8str2)
slen_u8str2 = length(u8str2)

@test len_u8str2 == 2 * len_u8str
@test slen_u8str2 == 2 * slen_u8str

u8str2plain = utf8(u8str2)

for i1 = 1:length(u8str2)
    if !isvalid(u8str2, i1); continue; end
    for i2 = i1:length(u8str2)
        if !isvalid(u8str2, i2); continue; end
        @test length(u8str2[i1:i2]) == length(u8str2plain[i1:i2])
        @test length(u8str2[i1:i2]) == length(u8str2plain[i1:i2])
        @test u8str2[i1:i2] == u8str2plain[i1:i2]
    end
end

str="tempus fugit"              #length(str)==12
ss=SubString(str,1,length(str)) #match source string
@test length(ss)==length(str)

ss=SubString(str,1,0)    #empty SubString
@test length(ss)==0

ss=SubString(str,14,20)  #start indexed beyond source string length
@test length(ss)==0

ss=SubString(str,10,16)  #end indexed beyond source string length
@test length(ss)==3

str2=""
ss=SubString(str2,1,4)  #empty source string
@test length(ss)==0

ss=SubString(str2,1,1)  #empty source string, identical start and end index
@test length(ss)==0

str = "aa\u2200\u2222bb"
u = SubString(str, 3, 6)
@test length(u)==2
b = IOBuffer()
write(b, u)
@test takebuf_string(b) == "\u2200\u2222"

str = "føøbar"
u = SubString(str, 4, 3)
@test length(u)==0
b = IOBuffer()
write(b, u)
@test takebuf_string(b) == ""

str = "føøbar"
u = SubString(str, 10, 10)
@test length(u)==0
b = IOBuffer()
write(b, u)
@test takebuf_string(b) == ""

@test replace("\u2202", '*', '\0') == "\u2202"

# search and SubString (issue #5679)
str = "Hello, world!"
u = SubString(str, 1, 5)
@test rsearch(u, "World") == 0:-1
@test rsearch(u, 'z') == 0
@test rsearch(u, "ll") == 3:4

# quotes + interpolation (issue #455)
@test "$("string")" == "string"
arr = ["a","b","c"]
@test "[$(join(arr, " - "))]" == "[a - b - c]"

# string iteration, and issue #1454
str = "é"
str_a = vcat(str...)
@test length(str_a)==1
@test str_a[1] == str[1]

str = "s\u2200"
@test str[1:end] == str

# triple-quote delimited strings
@test """abc""" == "abc"
@test """ab"c""" == "ab\"c"
@test """ab""c""" == "ab\"\"c"
@test """ab"\"c""" == "ab\"\"c"
@test """abc\"""" == "abc\""
n = 3
@test """$n\n""" == "$n\n"
@test """$(n)""" == "3"
@test """$(2n)""" == "6"
@test """$(n+4)""" == "7"
@test """$("string")""" == "string"
a = [3,1,2]
@test """$(a[2])""" == "1"
@test """$(a[3]+7)""" == "9"
@test """$(floor(Int,4.5))""" == "4"
nl = "
"
@test """
     a
     b

     c
     """ == "a$(nl)b$(nl)$(nl)c$(nl)"
@test """
      """ == ""
@test """x
     a
    """ == "x$(nl) a$(nl)"
@test """
     $n
   """ == "  $n$(nl)"
@test """
      a
     b
       c""" == " a$(nl)b$(nl)  c"
# note tab/space mixing
@test """
	a
     b
     """ == "   a$(nl)b$(nl)"
@test """
      a
       """ == "a$(nl)"
s = "   p"
@test """
      $s""" == "$s"
@test """
       $s
      """ == " $s$(nl)"
@test """\t""" == "\t"
@test """
      \t""" == ""
@test """
      foo
      \tbar""" == "foo$(nl)\tbar"
@test """
      foo
      \tbar
      """ == "foo$(nl)\tbar$(nl)"
@test """
      foo
      bar\t""" == "foo$(nl)bar\t"
@test """
      foo
      \tbar
       """ == "foo$(nl)       bar$(nl)"

# bytes2hex and hex2bytes
hex_str = "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
bin_val = hex2bytes(hex_str)

@test div(length(hex_str), 2) == length(bin_val)
@test hex_str == bytes2hex(bin_val)

bin_val = hex2bytes("07bf")
@test bin_val[1] == 7
@test bin_val[2] == 191
@test typeof(bin_val) == Array{UInt8, 1}
@test length(bin_val) == 2

# all valid hex chars
@test "0123456789abcdefabcdef" == bytes2hex(hex2bytes("0123456789abcdefABCDEF"))

# odd size
@test_throws ArgumentError hex2bytes("0123456789abcdefABCDEF0")

#non-hex characters
@test_throws ArgumentError hex2bytes("0123456789abcdefABCDEFGH")

# sizeof
@test sizeof("abc") == 3
@test sizeof("\u2222") == 3
@test sizeof(SubString("abc\u2222def",4,4)) == 3
@test sizeof(RopeString("abc","def")) == 6

# issue #3597
@test string(utf32(['T', 'e', 's', 't'])[1:1], "X") == "TX"

# issue #3710
@test prevind(SubString("{var}",2,4),4) == 3

# printf
# int
@test (@sprintf "%d" typemax(Int64)) == "9223372036854775807"
@test (@sprintf "%i" 42) == "42"
@test (@sprintf "%u" 42) == "42"
@test (@sprintf "Test: %i" 42) == "Test: 42"
@test (@sprintf "%#x" 42) == "0x2a"
@test (@sprintf "%#o" 42) == "052"
@test (@sprintf "%X" 42) == "2A"
@test (@sprintf "%X" 42) == "2A"
@test (@sprintf "% i" 42) == " 42"
@test (@sprintf "%+i" 42) == "+42"
@test (@sprintf "%4i" 42) == "  42"
@test (@sprintf "%-4i" 42) == "42  "
# float
@test (@sprintf "%7.2f" 1.2345) == "   1.23"
@test (@sprintf "%-7.2f" 1.2345) == "1.23   "
@test (@sprintf "%07.2f" 1.2345) == "0001.23"
@test (@sprintf "%.0f" 1.2345) == "1"
@test (@sprintf "%#.0f" 1.2345) == "1."
# Inf / NaN handling
@test (@sprintf "%f" Inf) == "Inf"
@test (@sprintf "%f" NaN) == "NaN"
# scientific notation
@test (@sprintf "%.4e" 1.2345) == "1.2345e+00"
@test (@sprintf "%.0e" 3e142) == "3e+142"
@test (@sprintf "%#.0e" 3e142) == "3.e+142"
# hex float
@test (@sprintf "%a" 1.5) == "0x1.8p+0"
@test (@sprintf "%#.0a" 1.5) == "0x2.p+0"
@test (@sprintf "%+30a" 1/3) == "         +0x1.5555555555555p-2"
# chars
@test (@sprintf "%c" 65) == "A"
@test (@sprintf "%c" 'A') == "A"
@test (@sprintf "%c" 248) == "ø"
@test (@sprintf "%c" 'ø') == "ø"
# strings
@test (@sprintf "%s" "test") == "test"
@test (@sprintf "%s" "tést") == "tést"
# reasonably complex
@test (@sprintf "Test: %s%c%C%c%#-.0f." "t" 65 66 67 -42) == "Test: tABC-42.."
#test simple splatting
@test (@sprintf "%d%d" [1 2]...) == "12"
# combo
@test (@sprintf "%f %d %d %f" 1.0 [3 4]... 5) == "1.000000 3 4 5.000000"
# multi
@test (@sprintf "%s %f %9.5f %d %d %d %d%d%d%d" [1:6;]... [7,8,9,10]...) == "1 2.000000   3.00000 4 5 6 78910"
# comprehension
@test (@sprintf "%s %s %s %d %d %d %f %f %f" Any[10^x+y for x=1:3,y=1:3 ]...) == "11 101 1001 12 102 1002 13.000000 103.000000 1003.000000"

# issue #4183
@test split(SubString(ascii("x"), 2, 0), "y") == AbstractString[""]
@test split(SubString(utf8("x"), 2, 0), "y") == AbstractString[""]

# issue #4586
@test rsplit(RevString("ailuj"),'l') == ["ju","ia"]
@test parse(Float64,RevString("64")) === 46.0

# issue #6772
@test float(SubString("10",1,1)) === 1.0
@test float(SubString("1 0",1,1)) === 1.0
@test parse(Float32,SubString("10",1,1)) === 1.0f0

for T = (UInt8,Int8,UInt16,Int16,UInt32,Int32,UInt64,Int64,UInt128,Int128,BigInt),
    b = 2:62, _ = 1:10
    n = T != BigInt ? rand(T) : BigInt(rand(Int128))
    @test parse(T,base(b,n),b) == n
end

# normalize_string (Unicode normalization etc.):
@test normalize_string("\u006e\u0303", :NFC) == "\u00f1"
@test "\u006e\u0303" == normalize_string("\u00f1", :NFD)
@test normalize_string("\ufb00", :NFC) != "ff"
@test normalize_string("\ufb00", :NFKC) == "ff"
@test normalize_string("\u006e\u0303\ufb00", :NFKC) == "\u00f1"*"ff"
@test normalize_string("\u00f1\ufb00", :NFKD) == "\u006e\u0303"*"ff"
@test normalize_string("\u006e\u0303", compose=true) == "\u00f1"
@test "\u006e\u0303" == normalize_string("\u00f1", decompose=true)
@test normalize_string("\u006e\u0303\u00b5",compat=true) == "\u00f1\u03bc"
@test normalize_string("Σσς",casefold=true) == "σσσ"
@test normalize_string("∕⁄", lump=true) == "//"
@test normalize_string("\ua\n\r\r\ua", newline2lf=true) == "\ua\ua\ua\ua"
@test normalize_string("\ua\n\r\r\ua", newline2ls=true) == "\u2028\u2028\u2028\u2028"
@test normalize_string("\ua\n\r\r\ua", newline2ps=true) == "\u2029\u2029\u2029\u2029"
@test normalize_string("\u00f1", stripmark=true) == "n"
@test isempty(normalize_string("\u00ad", stripignore=true))
@test normalize_string("\t\r", stripcc=true) == "  "
@test normalize_string("\t\r", stripcc=true, newline2ls=true) == " \u2028"

#Tests from Unicode SA#15, "Unicode normalization forms"
#http://www.unicode.org/reports/tr15/

#1. Canonical equivalence
let ==(a::Array{Char},b::Array{Char}) = normalize_string(string(a...), :NFC)==normalize_string(string(b...), :NFC)
    ==(a,b) = Base.(:(==))(a,b)
    @test ['C', '̧'] == ['Ç']
    @test ['q', '̇', '̣'] == ['q', '̣', '̇']
    @test ['가'] == ['ᄀ', 'ᅡ']
    @test ['Ω'] == ['Ω']
end

#2. Compatibility Equivalence
let ==(a::Array{Char},b::Array{Char}) = normalize_string(string(a...), :NFKC)==normalize_string(string(b...), :NFKC)
    ==(a,b) = Base.(:(==))(a,b)
    @test ['ℌ'] == ['ℍ'] == ['H']
    @test ['ﻨ'] == ['ﻧ'] == ['ﻦ'] == ['ﻥ']
    @test ['①'] == ['1']
    @test ['ｶ'] == ['カ']
    @test ['︷'] == ['{']
    @test ['⁹'] == ['₉']
    @test ['㌀'] == ['ア', 'パ', 'ー', 'ト']
    @test ['¼'] == ['1', '⁄', '4']
    @test ['ǆ'] == ['d', 'ž']
end

#3. Singletons
@test normalize_string("\U212b", :NFD) == "A\U030a"
@test normalize_string("\U212b", :NFC) == "\U00c5"
@test normalize_string("\U2126", :NFC) == normalize_string("\U2126", :NFD) == "\U03a9"

#4. Canonical Composites
@test normalize_string("\U00c5", :NFC) == "\U00c5"
@test normalize_string("\U00c5", :NFD) == "A\U030a"
@test normalize_string("\U00f4", :NFC) == "\U00f4"
@test normalize_string("\U00f4", :NFD) == "o\U0302"

#5. Multiple Combining Marks
@test normalize_string("\U1e69", :NFD) == "s\U0323\U0307"
@test normalize_string("\U1e69", :NFC) == "\U1e69"
@test normalize_string("\U1e0b\U0323", :NFD) == "d\U0323\U0307"
@test normalize_string("\U1e0b\U0323", :NFC) == "\U1e0d\U0307"
@test normalize_string("q\U0307\U0323", :NFC) == "q\U0323\U0307"
@test normalize_string("q\U0307\U0323", :NFD) == "q\U0323\U0307"

#6. Compatibility Composites
@test normalize_string("\Ufb01", :NFD) == normalize_string("\Ufb01", :NFC) == "\Ufb01"
@test normalize_string("\Ufb01", :NFKD) == normalize_string("\Ufb01", :NFKC) == "fi"
@test normalize_string("2\U2075", :NFD) == normalize_string("2\U2075", :NFC) == "2\U2075"
@test normalize_string("2\U2075", :NFKD) == normalize_string("2\U2075", :NFKC) == "25"
@test normalize_string("\U1e9b\U0323", :NFD) == "\U017f\U0323\U0307"
@test normalize_string("\U1e9b\U0323", :NFC) == "\U1e9b\U0323"
@test normalize_string("\U1e9b\U0323", :NFKD) == "s\U0323\U0307"
@test normalize_string("\U1e9b\U0323", :NFKC) == "\U1e69"

# issue #5870
@test !ismatch(Regex("aa"), SubString("",1,0))
@test ismatch(Regex(""), SubString("",1,0))

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


# isvalid(), chr2ind() and ind2chr() for SubString{DirectIndexString}
let s="lorem ipsum",
    sdict=Dict(SubString(s,1,11)=>s,
               SubString(s,1,6)=>"lorem ",
               SubString(s,1,0)=>"",
               SubString(s,2,4)=>"ore",
               SubString(s,2,16)=>"orem ipsum",
               SubString(s,12,14)=>""
               )
    for (ss,s) in sdict
        for i in -1:12
            @test isvalid(ss,i)==isvalid(s,i)
        end
    end
    for (ss,s) in sdict
        for i in 1:length(ss)
            @test ind2chr(ss,i)==ind2chr(s,i)
        end
    end
    for (ss,s) in sdict
        for i in 1:length(ss)
            @test chr2ind(ss,i)==chr2ind(s,i)
        end
    end
end #let

#for isvalid(SubString{UTF8String})
let s = utf8("Σx + βz - 2")
  for i in -1:length(s)+2
      ss=SubString(s,1,i)
      @test isvalid(ss,i)==isvalid(s,i)
  end
end

ss=SubString("hello",1,5)
@test_throws BoundsError ind2chr(ss, -1)
@test_throws BoundsError chr2ind(ss, -1)
@test_throws BoundsError chr2ind(ss, 10)
@test_throws BoundsError ind2chr(ss, 10)

# length(SubString{UTF8String}) performance specialization
let s = "|η(α)-ϕ(κ)| < ε"
    @test length(SubString(s,1,0))==length(s[1:0])
    @test length(SubString(s,4,4))==length(s[4:4])
    @test length(SubString(s,1,7))==length(s[1:7])
    @test length(SubString(s,4,11))==length(s[4:11])
end

# issue #7764
let
    srep = RepString("Σβ",2)
    s="Σβ"
    ss=SubString(s,1,endof(s))

    @test ss^2 == "ΣβΣβ"
    @test RepString(ss,2) == "ΣβΣβ"

    @test endof(srep) == 7

    @test next(srep, 3) == ('β',5)
    @test next(srep, 7) == ('β',9)

    @test srep[7] == 'β'
    @test_throws BoundsError srep[8]
end

#issue #5939  uft8proc/libmojibake character predicates
let
    alower=['a', 'd', 'j', 'y', 'z']
    ulower=['α', 'β', 'γ', 'δ', 'ф', 'я']
    for c in vcat(alower,ulower)
        @test islower(c) == true
        @test isupper(c) == false
        @test isdigit(c) == false
        @test isnumber(c) == false
    end

    aupper=['A', 'D', 'J', 'Y', 'Z']
    uupper= ['Δ', 'Γ', 'Π', 'Ψ', 'ǅ', 'Ж', 'Д']

    for c in vcat(aupper,uupper)
        @test islower(c) == false
        @test isupper(c) == true
        @test isdigit(c) == false
        @test isnumber(c) == false
    end

    nocase=['א','ﺵ']
    alphas=vcat(alower,ulower,aupper,uupper,nocase)

    for c in alphas
         @test isalpha(c) == true
         @test isnumber(c) == false
    end


    anumber=['0', '1', '5', '9']
    unumber=['٣', '٥', '٨', '¹', 'ⅳ' ]

    for c in anumber
         @test isdigit(c) == true
         @test isnumber(c) == true
    end
    for c in unumber
         @test isdigit(c) == false
         @test isnumber(c) == true
    end

    alnums=vcat(alphas,anumber,unumber)
    for c in alnums
         @test isalnum(c) == true
         @test ispunct(c) == false
    end

    asymbol = ['(',')', '~', '$' ]
    usymbol = ['∪', '∩', '⊂', '⊃', '√', '€', '¥', '↰', '△', '§']

    apunct =['.',',',';',':','&']
    upunct =['‡', '؟', '჻' ]

    for c in vcat(apunct,upunct)
         @test ispunct(c) == true
         @test isalnum(c) == false
    end

    for c in vcat(alnums,asymbol,usymbol,apunct,upunct)
        @test isprint(c) == true
        @test isgraph(c) == true
        @test isspace(c) == false
        @test iscntrl(c) == false
    end

    NBSP = Char(0x0000A0)
    ENSPACE = Char(0x002002)
    EMSPACE = Char(0x002003)
    THINSPACE = Char(0x002009)
    ZWSPACE = Char(0x002060)

    uspace = [ENSPACE, EMSPACE, THINSPACE]
    aspace = [' ']
    acntrl_space = ['\t', '\n', '\v', '\f', '\r']
    for c in vcat(aspace,uspace)
        @test isspace(c) == true
        @test isprint(c) == true
        @test isgraph(c) == false
    end

    for c in vcat(acntrl_space)
        @test isspace(c) == true
        @test isprint(c) == false
        @test isgraph(c) == false
    end

    @test isspace(ZWSPACE) == false # zero-width space

    acontrol = [ Char(0x001c), Char(0x001d), Char(0x001e), Char(0x001f)]
    latincontrol = [ Char(0x0080), Char(0x0085) ]
    ucontrol = [ Char(0x200E), Char(0x202E) ]

    for c in vcat(acontrol, acntrl_space, latincontrol)
        @test iscntrl(c) == true
        @test isalnum(c) == false
        @test isprint(c) == false
        @test isgraph(c) == false
    end

    for c in ucontrol  #non-latin1 controls
        if c!=Char(0x0085)
            @test iscntrl(c) == false
            @test isspace(c) == false
            @test isalnum(c) == false
            @test isprint(c) == false
            @test isgraph(c) == false
        end
    end

end

@test isspace("  \t   \n   \r  ")==true
@test isgraph("  \t   \n   \r  ")==false
@test isprint("  \t   \n   \r  ")==false
@test isalpha("  \t   \n   \r  ")==false
@test isnumber("  \t   \n   \r  ")==false
@test ispunct("  \t   \n   \r  ")==false

@test isspace("ΣβΣβ")==false
@test isalpha("ΣβΣβ")==true
@test isgraph("ΣβΣβ")==true
@test isprint("ΣβΣβ")==true
@test isupper("ΣβΣβ")==false
@test islower("ΣβΣβ")==false
@test isnumber("ΣβΣβ")==false
@test iscntrl("ΣβΣβ")==false
@test ispunct("ΣβΣβ")==false

@test isnumber("23435")==true
@test isdigit("23435")==true
@test isalnum("23435")==true
@test isalpha("23435")==false
@test iscntrl( string(Char(0x0080))) == true
@test ispunct( "‡؟჻") ==true

@test isxdigit('0') == true
@test isxdigit("0") == true
@test isxdigit("a") == true
@test isxdigit("g") == false

@test is_valid_ascii("is_valid_ascii") == true
@test is_valid_ascii("Σ_not_valid_ascii") == false
@test is_valid_char('a') == true
@test is_valid_char('\x00') == true
@test is_valid_char(0xd800) == false

@test is_valid_utf16(utf16("a")) == true
@test is_valid_utf16(UInt16[0xd800,0]) == false
# TODO is_valid_utf8

# Issue #11140
@test is_valid_utf32(utf32("a")) == true
@test is_valid_utf32(utf32("\x00")) == true
@test is_valid_utf32(UInt32[0xd800,0]) == false

# Issue #11203
@test is_valid_ascii(UInt8[]) == true
@test is_valid_utf8(UInt8[]) == true
@test is_valid_utf16(UInt16[]) == true
@test is_valid_utf32(UInt32[]) == true

# Check UTF-8 characters
# Check ASCII range (true),
# then single continuation bytes and lead bytes with no following continuation bytes (false)
for (rng,flg) in ((0:0x7f, true), (0x80:0xff, false))
    for byt in rng
        @test is_valid_utf8(UInt8[byt]) == flg
    end
end
# Check overlong lead bytes for 2-character sequences (false)
for byt = 0xc0:0xc1
    @test is_valid_utf8(UInt8[byt,0x80]) == false
end
# Check valid lead-in to two-byte sequences (true)
for byt = 0xc2:0xdf
    for (rng,flg) in ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
        for cont in rng
            @test is_valid_utf8(UInt8[byt, cont]) == flg
        end
    end
end
# Check three-byte sequences
for r1 in (0xe0:0xec, 0xee:0xef)
    for byt = r1
        # Check for short sequence
        @test is_valid_utf8(UInt8[byt]) == false
        for (rng,flg) in ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
            for cont in rng
                @test is_valid_utf8(UInt8[byt, cont]) == false
                @test is_valid_utf8(UInt8[byt, cont, 0x80]) == flg
            end
        end
    end
end
# Check hangul characters (0xd000-0xd7ff) hangul
# Check for short sequence, or start of surrogate pair
for (rng,flg) in ((0x00:0x7f, false), (0x80:0x9f, true), (0xa0:0xff, false))
    for cont in rng
        @test is_valid_utf8(UInt8[0xed, cont]) == false
        @test is_valid_utf8(UInt8[0xed, cont, 0x80]) == flg
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
            @test is_valid_utf8(UInt8[byt, cont]) == false
            @test is_valid_utf8(UInt8[byt, cont, 0x80]) == false
            @test is_valid_utf8(UInt8[byt, cont, 0x80, 0x80]) == flg
        end
    end
end
# Check five-byte sequences, should be invalid
for byt = 0xf8:0xfb
    @test is_valid_utf8(UInt8[byt, 0x80, 0x80, 0x80, 0x80]) == false
end
# Check six-byte sequences, should be invalid
for byt = 0xfc:0xfd
    @test is_valid_utf8(UInt8[byt, 0x80, 0x80, 0x80, 0x80, 0x80]) == false
end
# Check seven-byte sequences, should be invalid
@test is_valid_utf8(UInt8[0xfe, 0x80, 0x80, 0x80, 0x80, 0x80]) == false

# This caused JuliaLang/JSON.jl#82
@test first('\x00':'\x7f') === '\x00'
@test last('\x00':'\x7f') === '\x7f'

# Tests of join()
@test join([]) == ""
@test join(["a"],"?") == "a"
@test join("HELLO",'-') == "H-E-L-L-O"
@test join(1:5, ", ", " and ") == "1, 2, 3, 4 and 5"
@test join(["apples", "bananas", "pineapples"], ", ", " and ") == "apples, bananas and pineapples"

# issue #9178 `join` calls `done()` twice on the iterables
type i9178
    nnext::Int64
    ndone::Int64
end
Base.start(jt::i9178) = (jt.nnext=0 ; jt.ndone=0 ; 0)
Base.done(jt::i9178, n) = (jt.ndone += 1 ; n > 3)
Base.next(jt::i9178, n) = (jt.nnext += 1 ; ("$(jt.nnext),$(jt.ndone)", n+1))
@test join(i9178(0,0), ";") == "1,1;2,2;3,3;4,4"

# make sure substrings handle last code unit even if not start of codepoint
let s = "x\u0302"
    @test s[1:3] == s
end

# reverseind
for T in (ASCIIString, UTF8String, UTF16String, UTF32String)
    for prefix in ("", "abcd", "\U0001d6a4\U0001d4c1", "\U0001d6a4\U0001d4c1c", " \U0001d6a4\U0001d4c1")
        for suffix in ("", "abcde", "\U0001d4c1β\U0001d6a4", "\U0001d4c1β\U0001d6a4c", " \U0001d4c1β\U0001d6a4")
            for c in ('X', 'δ', '\U0001d6a5')
                T != ASCIIString || (isascii(prefix) && isascii(suffix) && isascii(c)) || continue
                s = convert(T, string(prefix, c, suffix))
                ri = search(reverse(s), c)
                @test reverse(s) == RevString(s)
                @test c == s[reverseind(s, ri)] == reverse(s)[ri]
                s = RevString(s)
                ri = search(reverse(s), c)
                @test c == s[reverseind(s, ri)] == reverse(s)[ri]
                s = convert(T, string(prefix, prefix, c, suffix, suffix))
                pre = convert(T, prefix)
                sb = SubString(s, nextind(pre, endof(pre)), endof(convert(T, string(prefix, prefix, c, suffix))))
                ri = search(reverse(sb), c)
                @test c == sb[reverseind(sb, ri)] == reverse(sb)[ri]
            end
        end
    end
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

#more ascii tests
@test convert(ASCIIString, UInt8[32,107,75], "*") == " kK"
@test convert(ASCIIString, UInt8[132,107,75], "*") == "*kK"
@test convert(ASCIIString, UInt8[], "*") == ""
@test convert(ASCIIString, UInt8[255], "*") == "*"

@test ucfirst("Hola")=="Hola"
@test ucfirst("hola")=="Hola"
@test ucfirst("")==""
@test ucfirst("*")=="*"

@test lcfirst("Hola")=="hola"
@test lcfirst("hola")=="hola"
@test lcfirst("")==""
@test lcfirst("*")=="*"

#more UTF8String tests
@test convert(UTF8String, UInt8[32,107,75], "*") == " kK"
@test convert(UTF8String, UInt8[132,107,75], "*") == "*kK"
@test convert(UTF8String, UInt8[32,107,75], "αβ") == " kK"
@test convert(UTF8String, UInt8[132,107,75], "αβ") == "αβkK"
@test convert(UTF8String, UInt8[], "*") == ""
@test convert(UTF8String, UInt8[255], "αβ") == "αβ"

# test AbstractString functions at beginning of string.jl
immutable tstStringType <: AbstractString
    data::Array{UInt8,1}
end
tstr = tstStringType("12");
@test_throws ErrorException endof(tstr)
@test_throws ErrorException next(tstr, Bool(1))

gstr = Base.GenericString("12");
@test typeof(string(gstr))==Base.GenericString
@test bytestring()==""

@test convert(Array{UInt8}, gstr) ==[49;50]
@test convert(Array{Char,1}, gstr) ==['1';'2']
@test convert(Symbol, gstr)==symbol("12")

@test getindex(gstr, Bool(1))=='1'
@test getindex(gstr,Bool(1):Bool(1))=="1"
@test getindex(gstr,AbstractVector([Bool(1):Bool(1);]))=="1"

@test symbol(gstr)==symbol("12")

@test_throws ErrorException sizeof(gstr)

@test length(Base.GenericString(""))==0

@test getindex(gstr,AbstractVector([Bool(1):Bool(1);]))=="1"

@test nextind(AbstractArray([Bool(1):Bool(1);]),1)==2

@test ind2chr(gstr,2)==2

# issue #10307
@test typeof(map(Int16,String[])) == Vector{Int16}

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
@test typeof(ascii(sp)) == ASCIIString
@test typeof(utf8(sp)) == UTF8String
s = "abcde\uff\u2000\U1f596"
sp = pointer(s)
@test utf8(sp) == s
@test utf8(sp,5) == "abcde"
@test typeof(utf8(sp)) == UTF8String

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
