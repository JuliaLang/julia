# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Unicode
using Unicode: normalize, isassigned, julia_chartransform
import Random

Random.seed!(12345)

@testset "string normalization" begin
    # normalize (Unicode normalization etc.):
    @test normalize("\u006e\u0303", :NFC) == "\u00f1" == normalize(SubString("ab\u006e\u0303cd",3,4), :NFC)
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
    @test normalize("\u0072\u0307\u0323", :NFC) == "\u1E5B\u0307" #26917

    # julia_chartransform identifier normalization
    @test normalize("julia\u025B\u00B5\u00B7\u0387\u2212\u210F", chartransform=julia_chartransform) ==
        "julia\u03B5\u03BC\u22C5\u22C5\u002D\u0127"
    @test julia_chartransform('\u00B5') === '\u03BC'
end

@testset "unicode sa#15" begin
    #Tests from Unicode SA#15, "Unicode normalization forms"
    #https://www.unicode.org/reports/tr15/

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
    for c in vcat(alower,ulower,['ª'])
        @test islowercase(c) == true
        @test isuppercase(c) == false
        @test isdigit(c) == false
        @test isnumeric(c) == false
    end

    aupper=['A', 'D', 'J', 'Y', 'Z']
    uupper= ['Δ', 'Γ', 'Π', 'Ψ', 'Ж', 'Д']

    for c in vcat(aupper,uupper,['Ⓐ'])
        @test islowercase(c) == false
        @test isuppercase(c) == true
        @test isdigit(c) == false
        @test isnumeric(c) == false
    end

    @test !isuppercase('ǅ') # titlecase is not uppercase
    @test Base.Unicode.iscased('ǅ') # but is "cased"

    nocase=['א','ﺵ']
    alphas=vcat(alower,ulower,aupper,uupper,nocase,['ǅ'])

    for c in alphas
        @test isletter(c) == true
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
        @test isletter(c) || isnumeric(c)
        @test ispunct(c) == false
    end

    asymbol = ['(',')', '~', '$' ]
    usymbol = ['∪', '∩', '⊂', '⊃', '√', '€', '¥', '↰', '△', '§']

    apunct =['.',',',';',':','&']
    upunct =['‡', '؟', '჻' ]

    for c in vcat(apunct,upunct)
        @test ispunct(c) == true
        @test !isletter(c) && !isnumeric(c)
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
        @test !isletter(c) && !isnumeric(c)
        @test isprint(c) == false
    end

    for c in ucontrol  #non-latin1 controls
        if c!=Char(0x0085)
            @test iscntrl(c) == false
            @test isspace(c) == false
            @test !isletter(c) && !isnumeric(c)
            @test isprint(c) == false
        end
    end

    @test  all(isspace,"  \t   \n   \r  ")
    @test !all(isprint,"  \t   \n   \r  ")
    @test !all(isletter,"  \t   \n   \r  ")
    @test !all(isnumeric,"  \t   \n   \r  ")
    @test !all(ispunct,"  \t   \n   \r  ")

    @test !all(isspace,"ΣβΣβ")
    @test  all(isletter,"ΣβΣβ")
    @test  all(isprint,"ΣβΣβ")
    @test !all(isuppercase,"ΣβΣβ")
    @test !all(islowercase,"ΣβΣβ")
    @test !all(isnumeric,"ΣβΣβ")
    @test !all(iscntrl,"ΣβΣβ")
    @test !all(ispunct,"ΣβΣβ")

    @test  all(isnumeric,"23435")
    @test  all(isdigit,"23435")
    @test !all(isletter,"23435")
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

    @test Base.Unicode.isgraphemebreak('α', 'β')
    @test !Base.Unicode.isgraphemebreak('α', '\u0302')

    for pre in ("","ä"), post in ("","x̂")
        prelen = length(graphemes(pre))
        @test graphemes(pre * "öü" * post, (1:2) .+ prelen) == "öü"
        @test graphemes(pre * "ö" * post, (1:1) .+ prelen) == "ö"
    end
    @test graphemes("äöüx", 6:5)::SubString{String} == ""
    @test_throws BoundsError graphemes("äöüx", 2:5)
    @test_throws BoundsError graphemes("äöüx", 5:5)
    @test_throws ArgumentError graphemes("äöüx", 0:1)
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
    @test isassigned(0x00)
    @test isassigned(0x0041)
    @test isassigned(Int(0x03b1))
    @test isassigned(UInt(0x67d2))
    @test !isassigned('\ufffe')
    @test !isassigned('\uffff')
    @test !isassigned(0xfffe)
    @test !isassigned(Int(0xffff))
    @test !isassigned(typemax(Int64))
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

@testset "#37680: initial graphemes" begin
    @test collect(graphemes("🤦🏼‍♂️")) == ["🤦🏼‍♂️"]
    @test collect(graphemes("👨🏻‍🤝‍👨🏽")) == ["👨🏻‍🤝‍👨🏽"]
    @test collect(graphemes("🇸🇪🇸🇪")) == ["🇸🇪","🇸🇪"]
end

@testset "uppercasefirst/lowercasefirst" begin
    @test uppercasefirst("Hola")=="Hola"
    @test uppercasefirst("hola")=="Hola"
    @test uppercasefirst("")==""
    @test uppercasefirst("*")=="*"
    @test uppercasefirst("Ǆxx") == uppercasefirst("ǆxx") == "ǅxx"

    @test lowercasefirst("Hola")=="hola"
    @test lowercasefirst("hola")=="hola"
    @test lowercasefirst("")==""
    @test lowercasefirst("*")=="*"
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
    @testset "uppercasefirst/lowercasefirst" begin
        @test uppercasefirst("Abc") == "Abc"
        @test uppercasefirst("abc") == "Abc"
        @test lowercasefirst("ABC") == "aBC"
        @test lowercasefirst("aBC") == "aBC"
        @test uppercasefirst(GenericString("")) == ""
        @test lowercasefirst(GenericString("")) == ""
        @test uppercasefirst(GenericString("a")) == "A"
        @test lowercasefirst(GenericString("A")) == "a"
        @test lowercasefirst(GenericString("a")) == "a"
        @test uppercasefirst(GenericString("A")) == "A"
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
        @test titlecase("abc-def", wordsep = !Base.Unicode.iscased) == "Abc-Def"
        @test titlecase("abc-def", wordsep = isspace)  == "Abc-def"
        @test titlecase("bôrked") == "Bôrked"
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

@testset "Grapheme breaks and iterator" begin
    u1 = reinterpret(Char, UInt32(0xc0) << 24)
    u2 = reinterpret(Char, UInt32(0xc1) << 24)

    overlong_uint =  UInt32(0xc0) << 24
    overlong_char = reinterpret(Char, overlong_uint)

    state = Ref(Int32(1))
    @test Base.Unicode.isgraphemebreak(u1, u2)
    @test Base.Unicode.isgraphemebreak!(state, u1, u2)
    @test state[] == 0

    @test_throws(
        ErrorException("An unknown error occurred while processing UTF-8 data."),
        Base.Unicode.utf8proc_error(2)
    )
    gi = Base.Unicode.graphemes("This is a string")
    @test gi isa Base.Unicode.GraphemeIterator{String}
    @test Base.Unicode.isvalid(Char, 'c')
    @test !Base.Unicode.isvalid(Char, overlong_char)
end

# the obvious, but suboptimal, algorithm:
isequal_normalized_naive(s1, s2; kws...) = normalize(s1; kws...) == normalize(s2; kws...)

@testset "Unicode equivalence" begin
    @test isequal_normalized("no\u00EBl", "noe\u0308l")
    @test !isequal_normalized("no\u00EBl", "noe\u0308l ")
    @test isequal_normalized("", "")
    @test !isequal_normalized("", " ")
    @test !isequal_normalized("no\u00EBl", "NOËL")
    @test isequal_normalized("no\u00EBl", "NOËL", casefold=true)
    @test !isequal_normalized("no\u00EBl", "noel")
    @test isequal_normalized("no\u00EBl", "noel", stripmark=true)
    @test isequal_normalized("no\u00EBl", "NOEL", stripmark=true, casefold=true)
    @test isequal_normalized("\u00B5\u0302m", "\u03BC\u0302m", chartransform=julia_chartransform)

    # issue #52408
    @testset "Sorting combining characters" begin
        for str in ("\u5bc\u5b0", "j\u5ae\u5bf\u5b2\u5b4") # julia#52408 examples
            @test isequal_normalized(str, normalize(str))
        end

        # first codepoint in every possible Unicode combining class
        let cc_chars = UInt32[0x00000334, 0x00016ff0, 0x0000093c, 0x00003099, 0x0000094d, 0x000005b0, 0x000005b1, 0x000005b2, 0x000005b3, 0x000005b4, 0x000005b5, 0x000005b6, 0x000005b7, 0x000005b8, 0x000005b9, 0x000005bb, 0x000005bc, 0x000005bd, 0x000005bf, 0x000005c1, 0x000005c2, 0x0000fb1e, 0x0000064b, 0x0000064c, 0x0000064d, 0x00000618, 0x00000619, 0x0000061a, 0x00000651, 0x00000652, 0x00000670, 0x00000711, 0x00000c55, 0x00000c56, 0x00000e38, 0x00000e48, 0x00000eb8, 0x00000ec8, 0x00000f71, 0x00000f72, 0x00000f74, 0x00000321, 0x00001dce, 0x0000031b, 0x00001dfa, 0x00000316, 0x0000059a, 0x0000302e, 0x0001d16d, 0x000005ae, 0x00000301, 0x00000315, 0x0000035c, 0x0000035d, 0x00000345],
            vowels = ['a', 'e', 'i', 'o', 'u', 'å', 'é', 'î', 'ö', 'ü'], Vowels = [vowels; uppercase.(vowels)]
            function randcc(n, n_cc) # random string with lots of combining chars
                buf = IOBuffer()
                for _ = 1:n
                    print.(buf, rand(Vowels, rand(1:5)))
                    print.(buf, Char.(rand(cc_chars, rand(0:n_cc))))
                end
                return String(take!(buf))
            end
            for _ = 1:100
                s = randcc(10,10)
                ns = normalize(s)
                cs = normalize(s, casefold=true)
                @test isequal_normalized(s, s)
                if !isequal_normalized(s, ns)
                    @show s
                end
                @test isequal_normalized(s, ns)
                @test isequal_normalized(cs, ns) == isequal_normalized_naive(cs, ns)
                @test isequal_normalized(cs, ns, casefold=true) ==
                      isequal_normalized_naive(cs, ns, casefold=true)
            end
            for _ = 1:3
                s = randcc(5,1000) # exercise sort!-based fallback
                @test isequal_normalized(s, normalize(s))
            end
            function randcc2(n, n_cc) # 2 strings with equivalent reordered combiners
                buf1 = IOBuffer()
                buf2 = IOBuffer()
                p = n_cc / length(cc_chars)
                for _ = 1:n
                    a = join(rand(Vowels, rand(1:5)))
                    print(buf1, a)
                    print(buf2, a)

                    # chars from distinct combining classes
                    # are canonically equivalent when re-ordered
                    c = Random.randsubseq(cc_chars, p)
                    print.(buf1, Char.(Random.shuffle!(c)))
                    print.(buf2, Char.(Random.shuffle!(c)))
                end
                return String(take!(buf1)), String(take!(buf2))
            end
            for _ = 1:100
                s1, s2 = randcc2(10,10)
                @test isequal_normalized(s1, s2)
            end
        end

        # combining characters in the same class are inequivalent if re-ordered:
        @test !isequal_normalized("x\u0334\u0335", "x\u0335\u0334")
    end
end

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(Unicode))
end
