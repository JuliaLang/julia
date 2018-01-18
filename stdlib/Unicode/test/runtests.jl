# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Unicode
using Unicode: normalize, isassigned, iscased, escape, unescape

@testset "string normalization" begin
    # normalize (Unicode normalization etc.):
    @test normalize("\u006e\u0303", :NFC) == "\u00f1"
    @test "\u006e\u0303" == normalize("\u00f1", :NFD)
    @test normalize("\ufb00", :NFC) != "ff"
    @test normalize("\ufb00", :NFKC) == "ff"
    @test normalize("\u006e\u0303\ufb00", :NFKC) == "\u00f1"*"ff"
    @test normalize("\u00f1\ufb00", :NFKD) == "\u006e\u0303"*"ff"
    @test normalize("\u006e\u0303", compose=true) == "\u00f1"
    @test "\u006e\u0303" == normalize("\u00f1", decompose=true)
    @test normalize("\u006e\u0303\u00b5",compat=true) == "\u00f1\u03bc"
    @test normalize("Σσς",casefold=true) == "σσσ"
    @test normalize("∕⁄", lump=true) == "//"
    @test normalize("\ua\n\r\r\ua", newline2lf=true) == "\ua\ua\ua\ua"
    @test normalize("\ua\n\r\r\ua", newline2ls=true) == "\u2028\u2028\u2028\u2028"
    @test normalize("\ua\n\r\r\ua", newline2ps=true) == "\u2029\u2029\u2029\u2029"
    @test normalize("\u00f1", stripmark=true) == "n"
    @test isempty(normalize("\u00ad", stripignore=true))
    @test normalize("\t\r", stripcc=true) == "  "
    @test normalize("\t\r", stripcc=true, newline2ls=true) == " \u2028"
end

@testset "unicode sa#15" begin
    #Tests from Unicode SA#15, "Unicode normalization forms"
    #http://www.unicode.org/reports/tr15/

    @testset "canonical equivalence" begin
        let ==(a::Array{Char},b::Array{Char}) = normalize(string(a...), :NFC)==normalize(string(b...), :NFC)
            ==(a,b) = Base.:(==)(a,b)
            @test ['C', '̧'] == ['Ç']
            @test ['q', '̇', '̣'] == ['q', '̣', '̇']
            @test ['가'] == ['ᄀ', 'ᅡ']
            @test ['Ω'] == ['Ω']
        end
    end

    @testset "compatibility equivalence" begin
        let ==(a::Array{Char},b::Array{Char}) = normalize(string(a...), :NFKC)==normalize(string(b...), :NFKC)
            ==(a,b) = Base.:(==)(a,b)
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
    end

    @testset "singletons" begin
        @test normalize("\U212b", :NFD) == "A\U030a"
        @test normalize("\U212b", :NFC) == "\U00c5"
        @test normalize("\U2126", :NFC) == normalize("\U2126", :NFD) == "\U03a9"
    end

    @testset "canonical composites" begin
        @test normalize("\U00c5", :NFC) == "\U00c5"
        @test normalize("\U00c5", :NFD) == "A\U030a"
        @test normalize("\U00f4", :NFC) == "\U00f4"
        @test normalize("\U00f4", :NFD) == "o\U0302"
    end

    @testset "multiple combining marks" begin
        @test normalize("\U1e69", :NFD) == "s\U0323\U0307"
        @test normalize("\U1e69", :NFC) == "\U1e69"
        @test normalize("\U1e0b\U0323", :NFD) == "d\U0323\U0307"
        @test normalize("\U1e0b\U0323", :NFC) == "\U1e0d\U0307"
        @test normalize("q\U0307\U0323", :NFC) == "q\U0323\U0307"
        @test normalize("q\U0307\U0323", :NFD) == "q\U0323\U0307"
    end

    @testset "compatibility composites" begin
        @test normalize("\Ufb01", :NFD) == normalize("\Ufb01", :NFC) == "\Ufb01"
        @test normalize("\Ufb01", :NFKD) == normalize("\Ufb01", :NFKC) == "fi"
        @test normalize("2\U2075", :NFD) == normalize("2\U2075", :NFC) == "2\U2075"
        @test normalize("2\U2075", :NFKD) == normalize("2\U2075", :NFKC) == "25"
        @test normalize("\U1e9b\U0323", :NFD) == "\U017f\U0323\U0307"
        @test normalize("\U1e9b\U0323", :NFC) == "\U1e9b\U0323"
        @test normalize("\U1e9b\U0323", :NFKD) == "s\U0323\U0307"
        @test normalize("\U1e9b\U0323", :NFKC) == "\U1e69"
    end
end

@testset "#5939 uft8proc character predicates" begin
    alower=['a', 'd', 'j', 'y', 'z']
    ulower=['α', 'β', 'γ', 'δ', 'ф', 'я']
    for c in vcat(alower,ulower)
        @test islower(c) == true
        @test isupper(c) == false
        @test isdigit(c) == false
        @test isnumeric(c) == false
    end

    aupper=['A', 'D', 'J', 'Y', 'Z']
    uupper= ['Δ', 'Γ', 'Π', 'Ψ', 'ǅ', 'Ж', 'Д']

    for c in vcat(aupper,uupper)
        @test islower(c) == false
        @test isupper(c) == true
        @test isdigit(c) == false
        @test isnumeric(c) == false
    end

    nocase=['א','ﺵ']
    alphas=vcat(alower,ulower,aupper,uupper,nocase)

    for c in alphas
        @test isalpha(c) == true
        @test isnumeric(c) == false
    end

    anumber=['0', '1', '5', '9']
    unumber=['٣', '٥', '٨', '¹', 'ⅳ' ]

    for c in anumber
        @test isdigit(c) == true
        @test isnumeric(c) == true
    end
    for c in unumber
        @test isdigit(c) == false
        @test isnumeric(c) == true
    end

    alnums=vcat(alphas,anumber,unumber)
    for c in alnums
        @test isalpha(c) || isnumeric(c)
        @test ispunct(c) == false
    end

    asymbol = ['(',')', '~', '$' ]
    usymbol = ['∪', '∩', '⊂', '⊃', '√', '€', '¥', '↰', '△', '§']

    apunct =['.',',',';',':','&']
    upunct =['‡', '؟', '჻' ]

    for c in vcat(apunct,upunct)
        @test ispunct(c) == true
        @test !isalpha(c) && !isnumeric(c)
    end

    for c in vcat(alnums,asymbol,usymbol,apunct,upunct)
        @test isprint(c) == true
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
    end

    for c in vcat(acntrl_space)
        @test isspace(c) == true
        @test isprint(c) == false
    end

    @test isspace(ZWSPACE) == false # zero-width space

    acontrol = [ Char(0x001c), Char(0x001d), Char(0x001e), Char(0x001f)]
    latincontrol = [ Char(0x0080), Char(0x0085) ]
    ucontrol = [ Char(0x200E), Char(0x202E) ]

    for c in vcat(acontrol, acntrl_space, latincontrol)
        @test iscntrl(c) == true
        @test !isalpha(c) && !isnumeric(c)
        @test isprint(c) == false
    end

    for c in ucontrol  #non-latin1 controls
        if c!=Char(0x0085)
            @test iscntrl(c) == false
            @test isspace(c) == false
            @test !isalpha(c) && !isnumeric(c)
            @test isprint(c) == false
        end
    end

    @test  all(isspace,"  \t   \n   \r  ")
    @test !all(isprint,"  \t   \n   \r  ")
    @test !all(isalpha,"  \t   \n   \r  ")
    @test !all(isnumeric,"  \t   \n   \r  ")
    @test !all(ispunct,"  \t   \n   \r  ")

    @test !all(isspace,"ΣβΣβ")
    @test  all(isalpha,"ΣβΣβ")
    @test  all(isprint,"ΣβΣβ")
    @test !all(isupper,"ΣβΣβ")
    @test !all(islower,"ΣβΣβ")
    @test !all(isnumeric,"ΣβΣβ")
    @test !all(iscntrl,"ΣβΣβ")
    @test !all(ispunct,"ΣβΣβ")

    @test  all(isnumeric,"23435")
    @test  all(isdigit,"23435")
    @test !all(isalpha,"23435")
    @test  all(iscntrl,string(Char(0x0080)))
    @test  all(ispunct, "‡؟჻")

    @test  isxdigit('0')
    @test  isxdigit('a')
    @test !isxdigit('x')
    @test !isxdigit('g')
end

@testset "utf8proc" begin
    # check utf8proc handling of CN category constants
    let c_ll = 'β', c_cn = '\u038B'
        @test Base.Unicode.category_code(c_ll) == Base.Unicode.UTF8PROC_CATEGORY_LL
        # check codepoint with category code CN
        @test Base.Unicode.category_code(c_cn) == Base.Unicode.UTF8PROC_CATEGORY_CN
    end
end

@testset "graphemes" begin
    let grphtest = (("b\u0300lahβlahb\u0302láh", ["b\u0300","l","a","h",
                                                  "β","l","a","h",
                                                  "b\u0302","l","á","h"]),
                    ("", String[]),
                    ("x\u0302", ["x\u0302"]),
                    ("\U1d4c1\u0302", ["\U1d4c1\u0302"]),
                    ("\U1d4c1\u0302\U1d4c1\u0300", ["\U1d4c1\u0302",
                                                    "\U1d4c1\u0300"]),
                    ("x",["x"]),
                    ("abc",["a","b","c"]))
        for T in (String,GenericString)
            for nf in (:NFC, :NFD)
                for (s, g) in grphtest
                    s_ = T(normalize(s, nf))
                    g_ = map(s -> normalize(s, nf), g)
                    # #9261
                    if length(s_) > 0
                        @test typeof(first(graphemes(s_))) == SubString{typeof(s_)}
                    end
                    grph = collect(graphemes(s_))
                    @test eltype(grph) == SubString{typeof(s_)}
                    @test grph == g_
                    @test length(graphemes(s_)) == length(grph)
                end
                S = [T(normalize(s)) for (s,g) in grphtest]
                G = map(graphemes, S)
                @test map(graphemes, sort!(S)) == sort!(G)
            end
        end
    end
end

@testset "#3721, #6939 up-to-date character widths" begin
    @test textwidth("") == 0
    @test textwidth('\U1f355') == 2
    @test textwidth("\U1f355") == 2
    @test textwidth(GenericString("\U1f355")) == 2
    @test textwidth("\U1f355\u0302") == 2
    @test textwidth(GenericString("\U1f355\u0302")) == 2
end

@testset "#10958 handling of embedded NUL chars" begin
    @test length("\0w") == length("\0α") == 2
    @test textwidth("\0w") == textwidth("\0α") == 1
    @test normalize("\0W", casefold=true) == "\0w"
end

@testset "ut8proc_map with GenericString" begin
    @test normalize(GenericString("\u006e\u0303"), :NFC) == "\u00f1"
end

@testset "normalize keywords" begin
    @test_throws ArgumentError normalize("\u006e\u0303", compose=false, compat=true)
    @test_throws ArgumentError normalize("\u006e\u0303", compose=false, stripmark=true)
end

@testset "isassigned" begin
    @test isassigned('\x00')
    @test isassigned('A')
    @test isassigned('α')
    @test isassigned('柒')
    @test !isassigned('\ufffe')
    @test !isassigned('\uffff')
    @test !isassigned("\xf4\x90\x80\x80"[1])
    @test !isassigned("\xf7\xbf\xbf\xbf"[1])
    @test !isassigned("\xff"[1])
end

@testset "isspace" begin
    @test isspace(' ')
    @test isspace('\t')
    @test isspace('\r')
    @test isspace('\u85')
    @test isspace('\ua0')
    @test !isspace('\ufffd')
    @test !isspace('\U10ffff')
end

@testset "grapheme iterators" begin
    let str = ascii("This is a test")
        g = graphemes(str)
        h = hash(str)
        @test hash(g) == h
        @test repr(g) == "length-14 GraphemeIterator{String} for \"$str\""
    end
end

@testset "#22693: substring graphemes" begin
    g = graphemes(SubString("123α56789", 1, 6))
    @test eltype(g) == SubString{String}
    @test collect(g) == ["1","2","3","α","5"]
end

@testset "ucfirst/lcfirst" begin
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
        @test titlecase("aBc ABC")               == "Abc Abc"
        @test titlecase("aBc ABC", strict=true)  == "Abc Abc"
        @test titlecase("aBc ABC", strict=false) == "ABc ABC"
        @test titlecase("abcD   EFG\n\thij", strict=true)  == "Abcd   Efg\n\tHij"
        @test titlecase("abcD   EFG\n\thij", strict=false) == "AbcD   EFG\n\tHij"
        @test titlecase("abc-def")                     == "Abc-Def"
        @test titlecase("abc-def", wordsep = !iscased) == "Abc-Def"
        @test titlecase("abc-def", wordsep = isspace)  == "Abc-def"
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
    @test map(foobar, GenericString(str)) == String(repeat(b"\ud800", outer=[length(str)]))
    @test map(foobaz, GenericString(str)) == String(repeat([0xff], outer=[4*length(str)]))

    @test "a".*["b","c"] == ["ab","ac"]
    @test ["b","c"].*"a" == ["ba","ca"]
    @test ["a","b"].*["c" "d"] == ["ac" "ad"; "bc" "bd"]

    @test one(String) == ""
    @test prod(["*" for i in 1:3]) == "***"
    @test prod(["*" for i in 1:0]) == ""
end

@testset "string escaping & unescaping" begin
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

    buf = IOBuffer()
    @test typeof(escape(buf, "test")) == Nothing
    @test String(take!(buf)) == "test"
    @test typeof(escape(buf, "hello", "l")) == Nothing
    @test String(take!(buf)) == "he\\l\\lo"

    @test typeof(escape("test", "t")) == String
    @test escape("test", "t") == "\\tes\\t"

    for i = 1:size(cx,1)
        cp, ch, st = cx[i,:]
        @test cp == convert(UInt32, ch)
        @test string(ch) == unescape(st)
        if isascii(ch) || !isprint(ch)
            @test st == escape(string(ch))
        end
        for j = 1:size(cx,1)
            local str = string(ch, cx[j,2])
            @test str == unescape(escape(str))
        end
        @test repr(ch) == "'$(isprint(ch) ? ch : st)'"
    end

    for i = 0:0x7f, p = ["","\0","x","xxx","\x7f","\uFF","\uFFF",
                         "\uFFFF","\U10000","\U10FFF","\U10FFFF"]
        c = Char(i)
        cp = string(c,p)
        op = string(Char(div(i,8)), oct(i%8), p)
        hp = string(Char(div(i,16)), hex(i%16), p)
        @test string(unescape(string("\\",oct(i,1),p))) == cp
        @test string(unescape(string("\\",oct(i,2),p))) == cp
        @test string(unescape(string("\\",oct(i,3),p))) == cp
        @test string(unescape(string("\\",oct(i,4),p))) == op
        @test string(unescape(string("\\x",hex(i,1),p))) == cp
        @test string(unescape(string("\\x",hex(i,2),p))) == cp
        @test string(unescape(string("\\x",hex(i,3),p))) == hp
    end

    @testset "unescape" begin
        @test "\0" == unescape("\\0")
        @test "\1" == unescape("\\1")
        @test "\7" == unescape("\\7")
        @test "\0x" == unescape("\\0x")
        @test "\1x" == unescape("\\1x")
        @test "\7x" == unescape("\\7x")
        @test "\00" == unescape("\\00")
        @test "\01" == unescape("\\01")
        @test "\07" == unescape("\\07")
        @test "\70" == unescape("\\70")
        @test "\71" == unescape("\\71")
        @test "\77" == unescape("\\77")
        @test "\00x" == unescape("\\00x")
        @test "\01x" == unescape("\\01x")
        @test "\07x" == unescape("\\07x")
        @test "\70x" == unescape("\\70x")
        @test "\71x" == unescape("\\71x")
        @test "\77x" == unescape("\\77x")
        @test "\000" == unescape("\\000")
        @test "\001" == unescape("\\001")
        @test "\007" == unescape("\\007")
        @test "\070" == unescape("\\070")
        @test "\071" == unescape("\\071")
        @test "\077" == unescape("\\077")
        @test "\170" == unescape("\\170")
        @test "\171" == unescape("\\171")
        @test "\177" == unescape("\\177")
        @test "\0001" == unescape("\\0001")
        @test "\0011" == unescape("\\0011")
        @test "\0071" == unescape("\\0071")
        @test "\0701" == unescape("\\0701")
        @test "\0711" == unescape("\\0711")
        @test "\0771" == unescape("\\0771")
        @test "\1701" == unescape("\\1701")
        @test "\1711" == unescape("\\1711")
        @test "\1771" == unescape("\\1771")

        @test "\x0" == unescape("\\x0")
        @test "\x1" == unescape("\\x1")
        @test "\xf" == unescape("\\xf")
        @test "\xF" == unescape("\\xF")
        @test "\x0x" == unescape("\\x0x")
        @test "\x1x" == unescape("\\x1x")
        @test "\xfx" == unescape("\\xfx")
        @test "\xFx" == unescape("\\xFx")
        @test "\x00" == unescape("\\x00")
        @test "\x01" == unescape("\\x01")
        @test "\x0f" == unescape("\\x0f")
        @test "\x0F" == unescape("\\x0F")
    end
end

@testset "unescape ArgumentErrors" begin
    @test_throws ArgumentError unescape(string('\\',"xZ"))
    @test_throws ArgumentError unescape(string('\\',"777"))
end
