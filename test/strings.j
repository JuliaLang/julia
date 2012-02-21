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

# printing numbers
@assert string(uint32(-1)) == "0xffffffff"
@assert hex(0xffffffffffff)=="ffffffffffff"
@assert hex(0xffffffffffff+1)=="1000000000000"
@assert hex(typemax(Uint64))=="ffffffffffffffff"

@assert int2str(typemin(Int64), 10) == "-9223372036854775808"
@assert int2str(typemin(Int16), 10) == "-32768"
@assert int2str(typemin(Int8 ), 10) == "-128"

# string manipulation
@assert strip("\t  hi   \n") == "hi"
