# This file is a part of Julia. License is MIT: https://julialang.org/license

## SubString and Cstring tests ##

@testset "SubString" begin
    u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
    u8str2 = u8str^2
    len_u8str = length(u8str)
    slen_u8str = length(u8str)
    len_u8str2 = length(u8str2)
    slen_u8str2 = length(u8str2)

    @test len_u8str2 == 2 * len_u8str
    @test slen_u8str2 == 2 * slen_u8str

    u8str2plain = String(u8str2)
    @test !isascii(u8str2)
    @test cmp(u8str2, u8str^3) == -1
    @test cmp(u8str2, u8str2)  == 0
    @test cmp(u8str^3, u8str2) == 1
    @test codeunit(u8str2) == codeunit(u8str2plain)

    @test convert(Union{String, SubString{String}}, u8str2)      === u8str2
    @test convert(Union{String, SubString{String}}, u8str2plain) === u8str2plain

    for i1 = 1:ncodeunits(u8str2)
        if !isvalid(u8str2, i1); continue; end
        for i2 = i1:ncodeunits(u8str2)
            if !isvalid(u8str2, i2); continue; end
            @test length(u8str2[i1:i2]) == length(u8str2plain[i1:i2])
            @test length(u8str2[i1:i2]) == length(u8str2plain[i1:i2])
            @test u8str2[i1:i2] == u8str2plain[i1:i2]
        end
    end

    # tests that SubString of a single multibyte `Char` string, like "∀" which takes 3 bytes
    # gives the same result as `getindex` (except that it is a view not a copy)
    for idx in 0:1
        @test SubString("∀", 1, idx) == "∀"[1:idx]
    end

    @testset "invalid end index" begin
        # Substring provided with invalid end index throws BoundsError
        @test_throws StringIndexError SubString("∀", 1, 2)
        @test_throws StringIndexError SubString("∀", 1, 3)
        @test_throws BoundsError SubString("∀", 1, 4)
    end

    @testset "invalid start index" begin
        # Substring provided with invalid start index throws BoundsError
        @test SubString("∀∀", 1:1) == "∀"
        @test SubString("∀∀", 1:4) == "∀∀"
        @test SubString("∀∀", 4:4) == "∀"
        @test_throws StringIndexError SubString("∀∀", 1:2)
        @test_throws StringIndexError SubString("∀∀", 1:5)
        @test_throws StringIndexError SubString("∀∀", 2:4)
        @test_throws BoundsError SubString("∀∀", 0:1)
        @test_throws BoundsError SubString("∀∀", 0:4)
        @test_throws BoundsError SubString("∀∀", 1:7)
        @test_throws BoundsError SubString("∀∀", 4:7)
    end

    # tests for SubString of more than one multibyte `Char` string
    # we are consistent with `getindex` for `String`
    for idx in [0, 1, 4]
        @test SubString("∀∀", 1, idx) == "∀∀"[1:idx]
        @test SubString("∀∀", 4, idx) == "∀∀"[4:idx]
    end

    @testset "index beyond lastindex(\"∀∀\")" begin
        for idx in [2:3; 5:6]
            @test_throws StringIndexError SubString("∀∀", 1, idx)
        end
        for idx in 7:8
            @test_throws BoundsError SubString("∀∀", 1, idx)
        end
    end

    let str="tempus fugit"              #length(str)==12
        ss=SubString(str,1,lastindex(str)) #match source string
        @test length(ss)==length(str)

        ss=SubString(str,1:lastindex(str))
        @test length(ss)==length(str)

        ss=SubString(str,1,0)    #empty SubString
        @test length(ss)==0

        ss=SubString(str,1:0)
        @test length(ss)==0

        @test_throws BoundsError SubString(str, 14, 20)  #start indexing beyond source string length
        @test_throws BoundsError SubString(str, 10, 16)  #end indexing beyond source string length

        @test_throws BoundsError SubString("", 1, 4)  #empty source string
        @test_throws BoundsError SubString("", 1, 1)  #empty source string, identical start and end index
        @test_throws BoundsError SubString("", 10, 12)
        @test SubString("", 12, 10) == ""
    end

    @test SubString("foobar", big(1), big(3)) == "foo"

    let str = "aa\u2200\u2222bb"
        u = SubString(str, 3, 6)
        @test length(u) == 2
        b = IOBuffer()
        write(b, u)
        @test String(take!(b)) == "\u2200\u2222"

        @test_throws StringIndexError SubString(str, 4, 5)
        @test_throws BoundsError iterate(u, 0)
        @test_throws BoundsError iterate(u, 8)
        @test_throws BoundsError getindex(u, 0)
        @test_throws BoundsError getindex(u, 7)
        @test_throws BoundsError getindex(u, 0:1)
        @test_throws BoundsError getindex(u, 7:7)
        @test reverseind(u, 1) == 4
        @test typeof(Base.cconvert(Ptr{Int8}, u)) == SubString{String}
        @test Base.cconvert(Ptr{Int8}, u) == u
    end

    let str = "føøbar"
        @test_throws BoundsError SubString(str, 10, 10)
        u = SubString(str, 4, 3)
        @test length(u) == 0
        b = IOBuffer()
        write(b, u)
        @test String(take!(b)) == ""
    end

    @testset "search and SubString (issue #5679)" begin
        str = "Hello, world!"
        u = SubString(str, 1, 5)
        @test findlast("World", u) === nothing
        @test findlast(isequal('z'), u) === nothing
        @test findlast("ll", u) == 3:4
    end

    @testset "SubString created from SubString" begin
        str = "Hello, world!"
        u = SubString(str, 2, 5)
        for idx in 1:4
            @test SubString(u, 2, idx) == u[2:idx]
            @test SubString(u, 2:idx) == u[2:idx]
        end
        @test_throws BoundsError SubString(u, 1, 10)
        @test_throws BoundsError SubString(u, 1:10)
        @test_throws BoundsError SubString(u, 20:30)
        @test SubString(u, 20:15) == ""
        @test_throws BoundsError SubString(u, -1:10)
        @test SubString(u, -1, -10) == ""
        @test SubString(SubString("123", 1, 2), -10, -20) == ""
    end

    # sizeof
    @test sizeof(SubString("abc\u2222def",4,4)) == 3

    # issue #3710
    @test prevind(SubString("{var}",2,4),4) == 3

    # issue #4183
    @test split(SubString("x", 2, 0), "y") == [""]

    @testset "issue #6772" begin
        @test parse(Float64, SubString("10",1,1)) === 1.0
        @test parse(Float64, SubString("1 0",1,1)) === 1.0
        @test parse(Float32, SubString("10",1,1)) === 1.0f0
    end

    @testset "issue #5870" begin
        @test !occursin(Regex("aa"), SubString("",1,0))
        @test occursin(Regex(""), SubString("",1,0))
    end
    @testset" isvalid, length, prevind, nextind for SubString{String}" begin
        s = "lorem ipsum"
        sdict = Dict(
            SubString(s, 1, 11)  => "lorem ipsum",
            SubString(s, 1, 6)   => "lorem ",
            SubString(s, 1, 0)   => "",
            SubString(s, 2, 4)   => "ore",
            SubString(s, 2, 11)  => "orem ipsum",
            SubString(s, 15, 14) => "",
        )
        for (ss, s) in sdict
            @test ncodeunits(ss) == ncodeunits(s)
            for i in -2:13
                @test isvalid(ss, i) == isvalid(s, i)
            end
            for i in 1:ncodeunits(ss), j = i-1:ncodeunits(ss)
                @test length(ss, i, j) == length(s, i, j)
            end
        end
        for (ss, s) in sdict
            @test length(ss) == length(s)
            for i in 0:ncodeunits(ss), j = 0:length(ss)+1
                @test prevind(ss, i+1, j) == prevind(s, i+1, j)
                @test nextind(ss, i, j) == nextind(s, i, j)
            end
            @test_throws BoundsError prevind(s, 0)
            @test_throws BoundsError prevind(ss, 0)
            @test_throws BoundsError nextind(s, ncodeunits(ss)+1)
            @test_throws BoundsError nextind(ss, ncodeunits(ss)+1)
        end
    end

    rng = MersenneTwister(1)
    strs = ["∀∃∀"*String(rand(rng, UInt8, 40))*"∀∃∀",
                                      String(rand(rng, UInt8, 50))]
    @testset "proper nextind/prevind/thisind for SubString{String}: $(repr(s))" for s in strs
        a = 0
        while a <= ncodeunits(s)
            a = nextind(s, a)
            b = a - 1
            while b <= ncodeunits(s)
                ss = SubString(s, a:b)
                s2 = s[a:b]
                @test ncodeunits(ss) == ncodeunits(s2)
                for i in 0:ncodeunits(ss)+1
                    @test thisind(ss, i) == thisind(s2, i)
                end
                for i in 0:ncodeunits(ss)
                    @test nextind(ss, i) == nextind(s2, i)
                    for j in 0:ncodeunits(ss)+5
                        if j > 0 || isvalid(ss, i)
                            @test nextind(ss, i, j) == nextind(s2, i, j)
                        end
                    end
                end
                for i in 1:ncodeunits(ss)+1
                    @test prevind(ss, i) == prevind(s2, i)
                    for j in 0:ncodeunits(ss)+5
                        if j > 0 || isvalid(ss, i)
                            @test prevind(ss, i, j) == prevind(s2, i, j)
                        end
                    end
                end
                b = nextind(s, b)
            end
        end
    end

    # for isvalid(SubString{String})
    let s = "Σx + βz - 2"
        for i in -1:ncodeunits(s)+2
            if checkbounds(Bool, s, i)
                if isvalid(s, i)
                    ss = SubString(s, 1, i)
                    for j = 1:ncodeunits(ss)
                        @test isvalid(ss, j) == isvalid(s, j)
                    end
                else
                    @test_throws StringIndexError SubString(s, 1, i)
                end
            elseif i > 0
                @test_throws BoundsError SubString(s, 1, i)
            else
                @test SubString(s, 1, i) == ""
            end
        end
    end

    let ss = SubString("hello", 1, 5)
        @test length(ss, 1, 0) == 0
        @test_throws BoundsError length(ss, 1, -1)
        @test_throws BoundsError length(ss, 1, 6)
        @test_throws BoundsError length(ss, 1, 10)
        @test_throws BoundsError prevind(ss, 0, 1)
        @test prevind(ss, 1, 1) == 0
        @test prevind(ss, 6, 1) == 5
        @test_throws BoundsError prevind(ss, 7, 1)
        @test_throws BoundsError nextind(ss, -1, 1)
        @test nextind(ss, 0, 1) == 1
        @test nextind(ss, 5, 1) == 6
        @test_throws BoundsError nextind(ss, 6, 1)
    end

    # length(SubString{String}) performance specialization
    let s = "|η(α)-ϕ(κ)| < ε"
        @test length(SubString(s, 1, 0)) == length(s[1:0])
        @test length(SubString(s, 4, 4)) == length(s[4:4])
        @test length(SubString(s, 1, 7)) == length(s[1:7])
        @test length(SubString(s, 4, 11)) == length(s[4:11])
    end

    @testset "reverseind" for T in (String, SubString, GenericString)
        for prefix in ("", "abcd", "\U0001d6a4\U0001d4c1", "\U0001d6a4\U0001d4c1c", " \U0001d6a4\U0001d4c1")
            for suffix in ("", "abcde", "\U0001d4c1β\U0001d6a4", "\U0001d4c1β\U0001d6a4c", " \U0001d4c1β\U0001d6a4")
                for c in ('X', 'δ', '\U0001d6a5')
                    s = convert(T, string(prefix, c, suffix))
                    r = reverse(s)
                    ri = findfirst(isequal(c), r)
                    @test c == s[reverseind(s, ri)] == r[ri]
                    s = convert(T, string(prefix, prefix, c, suffix, suffix))
                    pre = convert(T, prefix)
                    sb = SubString(s, nextind(pre, lastindex(pre)),
                                   lastindex(convert(T, string(prefix, prefix, c, suffix))))
                    r = reverse(sb)
                    ri = findfirst(isequal(c), r)
                    @test c == sb[reverseind(sb, ri)] == r[ri]
                end
            end
        end
    end

    @testset "reverseind of empty strings" begin
        for s in ("",
                  SubString("", 1, 0),
                  SubString("ab", 1, 0),
                  SubString("ab", 2, 1),
                  SubString("ab", 3, 2),
                  GenericString(""))
            @test reverseind(s, 0) == 1
            @test reverseind(s, 1) == 0
        end
    end
end

@testset "Cstring" begin
    @testset "issue #13974: comparison against pointers" begin
        str = String("foobar")
        ptr = pointer(str)
        cstring = Cstring(ptr)
        @test ptr == cstring
        @test cstring == ptr

        # convenient NULL string creation from Ptr{Cvoid}
        nullstr = Cstring(C_NULL)

        # Comparisons against NULL strings
        @test ptr != nullstr
        @test nullstr != ptr

        # Short-hand comparison against C_NULL
        @test nullstr == C_NULL
        @test C_NULL == nullstr
        @test cstring != C_NULL
        @test C_NULL != cstring
    end

    @testset "issue #31381: eltype(Cstring) != Cchar" begin
        s = Cstring(C_NULL)
        @test eltype(Cstring) == Cchar
        @test eltype(s) == Cchar
        @test pointer(s) isa Ptr{Cchar}
    end
end

@testset "Codeunits" begin
    s = "I'm a string!"
    @test codeunit(s) == UInt8
    @test codeunit(s, Int8(1)) == codeunit(s, 1)
end
