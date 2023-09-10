# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "StyledString" begin
    str = StyledString("some string")
    @test str == StyledString(str.string, Tuple{UnitRange{Int64}, Pair{Symbol, Any}}[])
    @test length(str) == 11
    @test ncodeunits(str) == 11
    @test eltype(str) == StyledChar{eltype(str.string)}
    @test first(str) == StyledChar(first(str.string), Pair{Symbol, Any}[])
    @test str[1:4] isa SubString{typeof(str)}
    @test str[1:4] == StyledString("some")
    @test "a" * str == StyledString("asome string")
    @test str * "a" == StyledString("some stringa")
    @test str * str == StyledString("some stringsome string")
    Base.textproperty!(str, 1:4, :thing, 0x01)
    Base.textproperty!(str, 5:11, :other, 0x02)
    Base.textproperty!(str, 1:11, :all, 0x03)
    @test str[3:4] == SubString(str, 3, 4)
    @test StyledString(str[3:4]) ==
        StyledString("me", [(1:2, :thing => 0x01), (1:2, :all => 0x03)])
    @test str == StyledString("some string", [(1:4, :thing => 0x01), (1:11, :all => 0x03), (5:11, :other => 0x02)])
    @test str != StyledString("some string")
    @test str != StyledString("some string", [(1:1, :thing => 0x01), (5:5, :other => 0x02), (11:11, :all => 0x03)])
    @test str != StyledString("some string", [(1:4, :thing => 0x11), (1:11, :all => 0x13), (5:11, :other => 0x12)])
    @test str != StyledString("some thingg", [(1:4, :thing => 0x01), (1:11, :all => 0x03), (5:11, :other => 0x02)])
    let allstrings =
        ['a', StyledChar('a'), StyledChar('a', :aaa => 0x04),
        "a string", StyledString("a string"),
        StyledString("a string", [(1:2, :hmm => '%')])]
        for str1 in repeat(allstrings, 2)
            for str2 in repeat(allstrings, 2)
                @test String(str1 * str2) ==
                    String(string(str1, str2)) ==
                    String(string(str1)) * String(string(str2))
                @test styledstring(str1 * str2) ==
                    styledstring(str1, str2) ==
                    styledstring(str1) * styledstring(str2)
            end
        end
    end
    @test collect(Base.eachstyle(str)) ==
        [("some", [:thing => 0x01, :all => 0x03]),
        (" string", [:all => 0x03, :other => 0x02])]
    @test ==(Base.styledstring_optimize!(
        StyledString("abc", [(1:1, :val => 1),
                             (2:2, :val => 2),
                             (2:2, :val => 1),
                             (3:3, :val => 2)])),
             StyledString("abc", [(1:2, :val => 1),
                                  (2:3, :val => 2)]))
end

@testset "StyledChar" begin
    chr = StyledChar('c')
    @test chr == StyledChar(chr.char, Pair{Symbol, Any}[])
    str = StyledString("hmm", [(1:1, :attr => "h0h0"),
                               (1:2, :attr => "h0m1"),
                               (2:3, :attr => "m1m2")])
    @test str[1] == StyledChar('h', Pair{Symbol, Any}[:attr => "h0h0"])
    @test str[2] == StyledChar('m', Pair{Symbol, Any}[:attr => "h0m1", :attr => "m1m2"])
    @test str[3] == StyledChar('m', Pair{Symbol, Any}[:attr => "m1m2"])
end

@testset "Styling preservation" begin
    str = StyledString("some string", [(1:4, :thing => 0x01), (1:11, :all => 0x03), (5:11, :other => 0x02)])
    @test match(r".e", str).match == str[3:4]
    @test  match(r"(.e)", str).captures == [str[3:4]]
    let m0 = match(r"(.)e", str)
        m1 = first(eachmatch(r"(.)e", str))
        for f in fieldnames(RegexMatch)
            @test getfield(m0, f) == getfield(m1, f)
        end
    end
    @test lpad(str, 12) ==
        StyledString(" some string", [(2:5, :thing => 0x01),
                                      (2:12, :all => 0x03),
                                      (6:12, :other => 0x02)])
    @test rpad(str, 12) ==
        StyledString("some string ", [(1:4, :thing => 0x01),
                                      (1:11, :all => 0x03),
                                      (5:11, :other => 0x02)])
    str1 = StyledString("test", [(1:4, :tag => 5)])
    str2 = StyledString("case", [(2:3, :tag => "oomph")])
    @test join([str1, str1], StyledString(" ")) ==
        StyledString("test test",
                     [(1:4, :tag => 5),
                      (6:9, :tag => 5)])
    @test join([str1, str1], StyledString(" ", [(1:1, :tag => 2)])) ==
        StyledString("test test",
                     [(1:4, :tag => 5),
                      (5:5, :tag => 2),
                      (6:9, :tag => 5)])
    @test repeat(str1, 2) == StyledString("testtest", [(1:8, :tag => 5)])
    @test repeat(str2, 2) == StyledString("casecase", [(2:3, :tag => "oomph"),
                                                       (6:7, :tag => "oomph")])
    @test repeat(str1[1], 3) == StyledString("ttt", [(1:3, :tag => 5)])
    @test reverse(str1) == StyledString("tset", [(1:4, :tag => 5)])
    @test reverse(str2) == StyledString("esac", [(2:3, :tag => "oomph")])
end
