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
