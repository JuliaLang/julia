# This file is a part of Julia. License is MIT: http://julialang.org/license

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

# graphemes
let grphtest = (("b\u0300lahβlahb\u0302láh", ["b\u0300","l","a","h",
                                              "β","l","a","h",
                                              "b\u0302","l","á","h"]),
                ("", UTF8String[]),
                ("x\u0302", ["x\u0302"]),
                ("\U1d4c1\u0302", ["\U1d4c1\u0302"]),
                ("\U1d4c1\u0302\U1d4c1\u0300", ["\U1d4c1\u0302",
                                                "\U1d4c1\u0300"]),
                ("x",["x"]),
                ("abc",["a","b","c"]))
    for T in (utf8,utf16,utf32)
        for nf in (:NFC, :NFD)
            for (s, g) in grphtest
                s_ = T(normalize_string(s, nf))
                g_ = map(s -> normalize_string(s, nf), g)
                # #9261
                if length(s_) > 0
                    @test typeof(first(graphemes(s_))) == SubString{typeof(s_)}
                end
                grph = collect(graphemes(s_))
                @test eltype(grph) == SubString{typeof(s_)}
                @test grph == g_
                @test length(graphemes(s_)) == length(grph)
            end
            S = [T(normalize_string(s)) for (s,g) in grphtest]
            G = map(graphemes, S)
            @test map(graphemes, sort!(S)) == sort!(G)
        end
    end
end

# up-to-date character widths (#3721, #6939)
@test charwidth('\U1f355') == strwidth("\U1f355") == strwidth(utf16("\U1f355")) == strwidth("\U1f355\u0302") == strwidth(utf16("\U1f355\u0302")) == 2

# handling of embedded NUL chars (#10958)
@test length("\0w") == length("\0α") == 2
@test strwidth("\0w") == strwidth("\0α") == 1
@test normalize_string("\0W", casefold=true) == "\0w"

# Make sure AbstractString case is covered (for utf8proc_map)
@test normalize_string(utf32("\u006e\u0303"), :NFC) == "\u00f1"

@test_throws ArgumentError normalize_string("\u006e\u0303", compose=false, compat=true)
@test_throws ArgumentError normalize_string("\u006e\u0303", compose=false, stripmark=true)

# Get full coverage of grapheme iterator functions
let str = ascii("This is a test")
    g = graphemes(str)
    h = hash(str)
    @test hash(g) == h
    @test convert(UTF16String, g) == str
    io = IOBuffer()
    show(io, g)
    check = "length-14 GraphemeIterator{ASCIIString} for \"$str\""
    @test takebuf_string(io) == check
end
