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
    cp = string(c,p)
    op = string(char(div(i,8)), oct(i%8), p)
    hp = string(char(div(i,16)), hex(i%16), p)
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
@assert "\0001" == unescape_string("\\0001")
@assert "\0011" == unescape_string("\\0011")
@assert "\0071" == unescape_string("\\0071")
@assert "\0701" == unescape_string("\\0701")
@assert "\0711" == unescape_string("\\0711")
@assert "\0771" == unescape_string("\\0771")
@assert "\1701" == unescape_string("\\1701")
@assert "\1711" == unescape_string("\\1711")
@assert "\1771" == unescape_string("\\1771")

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

@assert parse_int("0") == 0
@assert parse_int("-0") == 0
@assert parse_int("1") == 1
@assert parse_int("-1") == -1
@assert parse_int("9") == 9
@assert parse_int("-9") == -9
@assert parse_int("10") == 10
@assert parse_int("-10") == -10
@assert parse_int(Int64,"3830974272") == 3830974272
@assert parse_int(Int64,"-3830974272") == -3830974272

@assert parse_bin("0") == 0
@assert parse_bin("-0") == 0
@assert parse_bin("1") == 1
@assert parse_bin("-1") == -1
@assert parse_bin("10") == 2
@assert parse_bin("-10") == -2
@assert parse_bin("11") == 3
@assert parse_bin("-11") == -3
@assert parse_bin("1111000011110000111100001111") == 252645135
@assert parse_bin("-1111000011110000111100001111") == -252645135

@assert parse_oct("0") == 0
@assert parse_oct("-0") == 0
@assert parse_oct("1") == 1
@assert parse_oct("-1") == -1
@assert parse_oct("7") == 7
@assert parse_oct("-7") == -7
@assert parse_oct("10") == 8
@assert parse_oct("-10") == -8
@assert parse_oct("11") == 9
@assert parse_oct("-11") == -9
@assert parse_oct("72") == 58
@assert parse_oct("-72") == -58
@assert parse_oct("3172207320") == 434704080
@assert parse_oct("-3172207320") == -434704080

@assert parse_hex("0") == 0
@assert parse_hex("-0") == 0
@assert parse_hex("1") == 1
@assert parse_hex("-1") == -1
@assert parse_hex("9") == 9
@assert parse_hex("-9") == -9
@assert parse_hex("a") == 10
@assert parse_hex("-a") == -10
@assert parse_hex("f") == 15
@assert parse_hex("-f") == -15
@assert parse_hex("10") == 16
@assert parse_hex("-10") == -16
@assert parse_hex("0BADF00D") == 195948557
@assert parse_hex("-0BADF00D") == -195948557
@assert parse_int(Int64,"BADCAB1E",16) == 3135023902
@assert parse_int(Int64,"-BADCAB1E",16) == -3135023902
@assert parse_int(Int64,"CafeBabe",16) == 3405691582
@assert parse_int(Int64,"-CafeBabe",16) == -3405691582
@assert parse_int(Int64,"DeadBeef",16) == 3735928559
@assert parse_int(Int64,"-DeadBeef",16) == -3735928559

@assert parse_int("2\n") == 2
@assert parse_int("   2 \n ") == 2
@assert parse_int(" 2 ") == 2
@assert parse_int("2 ") == 2
@assert parse_int(" 2") == 2
@assert parse_int("+2\n") == 2
@assert parse_int("-2") == -2
@assert_fails parse_int("   2 \n 0")
@assert_fails parse_int("2x")
@assert_fails parse_int("-")

# string manipulation
@assert strip("\t  hi   \n") == "hi"

# some test strings
astr = "Hello, world.\n"
u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

# ascii strchr
for str in {astr, GenericString(astr)}
    @assert strchr(str, 'x') == 0
    @assert strchr(str, '\0') == 0
    @assert strchr(str, '\u80') == 0
    @assert strchr(str, '∀') == 0
    @assert strchr(str, 'H') == 1
    @assert strchr(str, 'l') == 3
    @assert strchr(str, 'l', 4) == 4
    @assert strchr(str, 'l', 5) == 11
    @assert strchr(str, 'l', 12) == 0
    @assert strchr(str, ',') == 6
    @assert strchr(str, ',', 7) == 0
    @assert strchr(str, '\n') == 14
end

# utf-8 strchr
for str in {u8str, GenericString(u8str)}
    @assert strchr(str, 'z') == 0
    @assert strchr(str, '\0') == 0
    @assert strchr(str, '\u80') == 0
    @assert strchr(str, '∄') == 0
    @assert strchr(str, '∀') == 1
    @assert strchr(str, '∀', 2) == 0
    @assert strchr(str, '∃') == 13
    @assert strchr(str, '∃', 14) == 0
    @assert strchr(str, 'x') == 26
    @assert strchr(str, 'x', 27) == 43
    @assert strchr(str, 'x', 44) == 0
    @assert strchr(str, 'δ') == 17
    @assert strchr(str, 'δ', 18) == 33
    @assert strchr(str, 'δ', 34) == 0
    @assert strchr(str, 'ε') == 5
    @assert strchr(str, 'ε', 6) == 54
    @assert strchr(str, 'ε', 55) == 0
end

# string search with a char
@assert search(astr, 'x')[1] == 0
@assert search(astr, 'H') == (1,2)
@assert search(astr, 'H', 2)[1] == 0
@assert search(astr, 'l') == (3,4)
@assert search(astr, 'l', 4) == (4,5)
@assert search(astr, 'l', 5) == (11,12)
@assert search(astr, 'l', 12)[1] == 0
@assert search(astr, '\n') == (14,15)
@assert search(astr, '\n', 15)[1] == 0
@assert search(u8str, 'z')[1] == 0
@assert search(u8str, '∄')[1] == 0
@assert search(u8str, '∀') == (1,4)
@assert search(u8str, '∀', 4)[1] == 0
@assert search(u8str, '∃') == (13,16)
@assert search(u8str, '∃', 16)[1] == 0
@assert search(u8str, 'x') == (26,27)
@assert search(u8str, 'x', 27) == (43,44)
@assert search(u8str, 'x', 44)[1] == 0
@assert search(u8str, 'ε') == (5,7)
@assert search(u8str, 'ε', 7) == (54,56)
@assert search(u8str, 'ε', 56)[1] == 0

# string search with a single-char string
@assert search(astr, "x")[1] == 0
@assert search(astr, "H") == (1,2)
@assert search(astr, "H", 2)[1] == 0
@assert search(astr, "l") == (3,4)
@assert search(astr, "l", 4) == (4,5)
@assert search(astr, "l", 5) == (11,12)
@assert search(astr, "l", 12)[1] == 0
@assert search(astr, "\n") == (14,15)
@assert search(astr, "\n", 15)[1] == 0
@assert search(u8str, "z")[1] == 0
@assert search(u8str, "∄")[1] == 0
@assert search(u8str, "∀") == (1,4)
@assert search(u8str, "∀", 4)[1] == 0
@assert search(u8str, "∃") == (13,16)
@assert search(u8str, "∃", 16)[1] == 0
@assert search(u8str, "x") == (26,27)
@assert search(u8str, "x", 27) == (43,44)
@assert search(u8str, "x", 44)[1] == 0
@assert search(u8str, "ε") == (5,7)
@assert search(u8str, "ε", 7) == (54,56)
@assert search(u8str, "ε", 56)[1] == 0

# string search with a single-char regex
@assert search(astr, r"x")[1] == 0
@assert search(astr, r"H") == (1,2)
@assert search(astr, r"H", 2)[1] == 0
@assert search(astr, r"l") == (3,4)
@assert search(astr, r"l", 4) == (4,5)
@assert search(astr, r"l", 5) == (11,12)
@assert search(astr, r"l", 12)[1] == 0
@assert search(astr, r"\n") == (14,15)
@assert search(astr, r"\n", 15)[1] == 0
@assert search(u8str, r"z")[1] == 0
@assert search(u8str, r"∄")[1] == 0
@assert search(u8str, r"∀") == (1,4)
@assert search(u8str, r"∀", 4)[1] == 0
@assert search(u8str, r"∃") == (13,16)
@assert search(u8str, r"∃", 16)[1] == 0
@assert search(u8str, r"x") == (26,27)
@assert search(u8str, r"x", 27) == (43,44)
@assert search(u8str, r"x", 44)[1] == 0
@assert search(u8str, r"ε") == (5,7)
@assert search(u8str, r"ε", 7) == (54,56)
@assert search(u8str, r"ε", 56)[1] == 0
for i = 1:length(astr)
    @assert search(astr, r"."s, i) == (i,i+1)
end
for i = 1:length(u8str)
    # TODO: should regex search fast-forward invalid indices?
    if isvalid(u8str,i)
        @assert search(u8str, r"."s, i) == (i,nextind(u8str,i))
    end
end

# string search with a zero-char string
for i = 1:length(astr)
    @assert search(astr, "", i) == (i,i)
end
for i = 1:length(u8str)
    @assert search(u8str, "", i) == (i,i)
end

# string search with a zero-char regex
for i = 1:length(astr)
    @assert search(astr, r"", i) == (i,i)
end
for i = 1:length(u8str)
    # TODO: should regex search fast-forward invalid indices?
    if isvalid(u8str,i)
        @assert search(u8str, r""s, i) == (i,i)
    end
end

# string search with a two-char string literal
@assert search("foo,bar,baz", "xx")[1] == 0
@assert search("foo,bar,baz", "fo") == (1,3)
@assert search("foo,bar,baz", "fo", 3)[1] == 0
@assert search("foo,bar,baz", "oo") == (2,4)
@assert search("foo,bar,baz", "oo", 4)[1] == 0
@assert search("foo,bar,baz", "o,") == (3,5)
@assert search("foo,bar,baz", "o,", 5)[1] == 0
@assert search("foo,bar,baz", ",b") == (4,6)
@assert search("foo,bar,baz", ",b", 6) == (8,10)
@assert search("foo,bar,baz", ",b", 10)[1] == 0
@assert search("foo,bar,baz", "az") == (10,12)
@assert search("foo,bar,baz", "az", 12)[1] == 0

# string search with a two-char regex
@assert search("foo,bar,baz", r"xx")[1] == 0
@assert search("foo,bar,baz", r"fo") == (1,3)
@assert search("foo,bar,baz", r"fo", 3)[1] == 0
@assert search("foo,bar,baz", r"oo") == (2,4)
@assert search("foo,bar,baz", r"oo", 4)[1] == 0
@assert search("foo,bar,baz", r"o,") == (3,5)
@assert search("foo,bar,baz", r"o,", 5)[1] == 0
@assert search("foo,bar,baz", r",b") == (4,6)
@assert search("foo,bar,baz", r",b", 6) == (8,10)
@assert search("foo,bar,baz", r",b", 10)[1] == 0
@assert search("foo,bar,baz", r"az") == (10,12)
@assert search("foo,bar,baz", r"az", 12)[1] == 0

# split
@assert isequal(split("foo,bar,baz", 'x'), ["foo,bar,baz"])
@assert isequal(split("foo,bar,baz", ','), ["foo","bar","baz"])
@assert isequal(split("foo,bar,baz", ","), ["foo","bar","baz"])
@assert isequal(split("foo,bar,baz", r","), ["foo","bar","baz"])
@assert isequal(split("foo,bar,baz", ',', 0), ["foo","bar","baz"])
@assert isequal(split("foo,bar,baz", ',', 1), ["foo,bar,baz"])
@assert isequal(split("foo,bar,baz", ',', 2), ["foo","bar,baz"])
@assert isequal(split("foo,bar,baz", ',', 3), ["foo","bar","baz"])
@assert isequal(split("foo,bar", "o,b"), ["fo","ar"])

@assert isequal(split("", ','), [""])
@assert isequal(split(",", ','), ["",""])
@assert isequal(split(",,", ','), ["","",""])
@assert isequal(split("", ',', false), [])
@assert isequal(split(",", ',', false), [])
@assert isequal(split(",,", ',', false), [])

@assert isequal(split("a b c"), ["a","b","c"])
@assert isequal(split("a  b \t c\n"), ["a","b","c"])

let str = "a.:.ba..:..cba.:.:.dcba.:."
@assert isequal(split(str, ".:."), ["a","ba.",".cba",":.dcba",""])
@assert isequal(split(str, ".:.", false), ["a","ba.",".cba",":.dcba"])
@assert isequal(split(str, ".:."), ["a","ba.",".cba",":.dcba",""])
@assert isequal(split(str, r"\.(:\.)+"), ["a","ba.",".cba","dcba",""])
@assert isequal(split(str, r"\.(:\.)+", false), ["a","ba.",".cba","dcba"])
@assert isequal(split(str, r"\.+:\.+"), ["a","ba","cba",":.dcba",""])
@assert isequal(split(str, r"\.+:\.+", false), ["a","ba","cba",":.dcba"])
end

# zero-width splits
@assert isequal(split("", ""), [""])
@assert isequal(split("", r""), [""])
@assert isequal(split("abc", ""), ["a","b","c"])
@assert isequal(split("abc", r""), ["a","b","c"])
@assert isequal(split("abcd", r"b?"), ["a","c","d"])
@assert isequal(split("abcd", r"b*"), ["a","c","d"])
@assert isequal(split("abcd", r"b+"), ["a","cd"])
@assert isequal(split("abcd", r"b?c?"), ["a","d"])
@assert isequal(split("abcd", r"[bc]?"), ["a","","d"])
@assert isequal(split("abcd", r"a*"), ["","b","c","d"])
@assert isequal(split("abcd", r"a+"), ["","bcd"])
@assert isequal(split("abcd", r"d*"), ["a","b","c",""])
@assert isequal(split("abcd", r"d+"), ["abc",""])
@assert isequal(split("abcd", r"[ad]?"), ["","b","c",""])

# replace
@assert replace("foobar", 'o', '0') == "f00bar"
@assert replace("foobar", 'o', '0', 1) == "f0obar"
@assert replace("foobar", 'o', "") == "fbar"
@assert replace("foobar", 'o', "", 1) == "fobar"
@assert replace("foobar", 'f', 'F') == "Foobar"
@assert replace("foobar", 'r', 'R') == "foobaR"

@assert replace("", "", "") == ""
@assert replace("", "", "x") == "x"
@assert replace("", "x", "y") == ""

@assert replace("abcd", "", "^") == "^a^b^c^d^"
@assert replace("abcd", "b", "^") == "a^cd"
@assert replace("abcd", r"b?", "^") == "^a^c^d^"
@assert replace("abcd", r"b+", "^") == "a^cd"
@assert replace("abcd", r"b?c?", "^") == "^a^d^"
@assert replace("abcd", r"[bc]?", "^") == "^a^^d^"

# {begins,ends}_with
@assert begins_with("abcd", 'a')
@assert begins_with("abcd", "a")
@assert begins_with("abcd", "ab")
@assert !begins_with("ab", "abcd")
@assert !begins_with("abcd", "bc")
@assert ends_with("abcd", 'd')
@assert ends_with("abcd", "d")
@assert ends_with("abcd", "cd")
@assert !ends_with("abcd", "dc")
@assert !ends_with("cd", "abcd")

# RepStrings and SubStrings
u8str2 = u8str^2
len_u8str = length(u8str)
slen_u8str = strlen(u8str)
len_u8str2 = length(u8str2)
slen_u8str2 = strlen(u8str2)

@assert len_u8str2 == 2 * len_u8str
@assert slen_u8str2 == 2 * slen_u8str

u8str2plain = utf8(u8str2)

for i1 = 1:length(u8str2)
    if !isvalid(u8str2, i1); continue; end
    for i2 = i1:length(u8str2)
        if !isvalid(u8str2, i2); continue; end
        @assert length(u8str2[i1:i2]) == length(u8str2plain[i1:i2])
        @assert strlen(u8str2[i1:i2]) == strlen(u8str2plain[i1:i2])
        @assert u8str2[i1:i2] == u8str2plain[i1:i2]
    end
end

# string iteration, and issue #1454
str = "é"
str_a = [str...]
@assert length(str_a)==1
@assert str_a[1] == str[1]
