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
    cp, ch, st = cx[i,:]
    @test cp == convert(UInt32, ch)
    @test string(ch) == unescape_string(st)
    if isascii(ch) || !isprint(ch)
        @test st == escape_string(string(ch))
    end
    for j = 1:size(cx,1)
        str = string(ch, cx[j,2])
        @test str == unescape_string(escape_string(str))
    end
    @test repr(ch) == "'$(isprint(ch) ? ch : st)'"
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

extrapath = is_windows() ? joinpath(JULIA_HOME,"..","Git","usr","bin")*";" : ""
withenv("PATH" => extrapath * ENV["PATH"]) do
if !success(`iconv --version`)
    warn("iconv not found, skipping unicode tests!")
    is_windows() && warn("Use WinRPM.install(\"win_iconv\") to run these tests")
else
    # Create unicode test data directory
    unicodedir = mktempdir()

    # Use perl to generate the primary data
    primary_encoding = "UTF-32BE"
    primary_path = replace(joinpath(unicodedir, primary_encoding*".unicode"),"\\","\\\\\\\\")
    run(`perl -e "
        $$fname = \"$primary_path\";
        open(UNICODEF, \">\", \"$$fname\")         or die \"can\'t open $$fname: $$!\";
        binmode(UNICODEF);
        print UNICODEF pack \"N*\", 0xfeff, 0..0xd7ff, 0xe000..0x10ffff;
        close(UNICODEF);"` )

    # Use iconv to generate the other data
    for encoding in ["UTF-32LE", "UTF-16BE", "UTF-16LE", "UTF-8"]
        output_path = joinpath(unicodedir, encoding*".unicode")
        f = Base.Filesystem.open(output_path,Base.JL_O_WRONLY|Base.JL_O_CREAT,Base.S_IRUSR | Base.S_IWUSR | Base.S_IRGRP | Base.S_IROTH)
        run(pipeline(`iconv -f $primary_encoding -t $encoding $primary_path`, f))
        Base.Filesystem.close(f)
    end

    f=open(joinpath(unicodedir,"UTF-32LE.unicode"))
    str1 = utf32(read(f, UInt32, 1112065)[2:end])
    close(f)

    f=open(joinpath(unicodedir,"UTF-8.unicode"))
    str2 = String(read(f, UInt8, 4382595)[4:end])
    close(f)
    @test str1 == str2

    @test str1 == utf16(read(joinpath(unicodedir,"UTF-16LE.unicode"),
                             UInt16, 2160641)[2:end])

    @test str1 == utf16(read(joinpath(unicodedir,"UTF-16LE.unicode"),
                             UInt8, 2160641*2))

    @test str1 == utf16(read(joinpath(unicodedir,"UTF-16BE.unicode"),
                             UInt8, 2160641*2))


    @test str1 == utf32(read(joinpath(unicodedir,"UTF-32LE.unicode"),
                             UInt8, 1112065*4))

    @test str1 == utf32(read(joinpath(unicodedir,"UTF-32BE.unicode"),
                             UInt8, 1112065*4))


    str1 = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
    str2 = UTF32String(UInt32[
                 8704, 32, 949, 32, 62, 32, 48, 44, 32, 8707, 32,
                 948, 32, 62, 32, 48, 58, 32, 124, 120, 45, 121, 124,
                 32, 60, 32, 948, 32, 8658, 32, 124, 102, 40, 120,
                 41, 45, 102, 40, 121, 41, 124, 32, 60, 32, 949
                 ,0])
    @test str1 == str2

    # Cleanup unicode data
    for encoding in ["UTF-32BE", "UTF-32LE", "UTF-16BE", "UTF-16LE", "UTF-8"]
        rm(joinpath(unicodedir,encoding*".unicode"))
    end
    rm(unicodedir)
end
end

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

# quotes + interpolation (issue #455)
@test "$("string")" == "string"
arr = ["a","b","c"]
@test "[$(join(arr, " - "))]" == "[a - b - c]"

# join with empty input
myio = IOBuffer()
join(myio, "", "", 1)
@test isempty(takebuf_array(myio))

# unescape_chars
@test Base.unescape_chars("\\t","t") == "t"
@test_throws ArgumentError unescape_string(IOBuffer(), string('\\',"xZ"))
@test_throws ArgumentError unescape_string(IOBuffer(), string('\\',"777"))

# 11659
# The indentation code was not correctly counting tab stops
@test Base.indentation("      \t") == (8, true)
@test Base.indentation("  \tfoob") == (8, false)
@test Base.indentation(" \t \t")   == (16, true)

@test Base.unindent("\tfoo",0) == "\tfoo"
@test Base.unindent("\tfoo",4) == "    foo"
@test Base.unindent("    \tfoo",4) == "    foo"
@test Base.unindent("\t\n    \tfoo",4) == "    \n    foo"
@test Base.unindent("\tfoo\tbar",4) == "    foo     bar"
@test Base.unindent("\n\tfoo",4) == "\n    foo"
@test Base.unindent("\n    \tfoo",4) == "\n    foo"
@test Base.unindent("\n\t\n    \tfoo",4) == "\n    \n    foo"
@test Base.unindent("\n\tfoo\tbar",4) == "\n    foo     bar"

