# This file is a part of Julia. License is MIT: http://julialang.org/license

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

# check handling of CN category constants
let c_ll = 'β', c_cn = '\u038B'
    @test charprop(Category.Code, c_ll) == Category.Ll
    # check codepoint with category code CN
    @test charprop(Category.Code, c_cn) == Category.Cn
end

# Make sure fastplus is called for coverage
@test lowercase('A') == 'a'
@test uppercase('a') == 'A'

@test is_assigned_char('A')

# Get full coverage of isspace function
@test isspace(' ')
@test isspace('\t')
@test isspace('\r')
@test isspace('\u85')
@test isspace('\ua0')
@test !isspace('\ufffd')
@test !isspace('\U10ffff')
