# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "TaggedString" begin
    str = Base.TaggedString("some string")
    @test str == Base.TaggedString(str.string, Tuple{UnitRange{Int}, Pair{Symbol, Any}}[])
    @test length(str) == 11
    @test ncodeunits(str) == 11
    @test eltype(str) == Base.TaggedChar{eltype(str.string)}
    @test first(str) == Base.TaggedChar(first(str.string), Pair{Symbol, Any}[])
    @test str[1:4] isa SubString{typeof(str)}
    @test str[1:4] == Base.TaggedString("some")
    @test "a" * str == Base.TaggedString("asome string")
    @test str * "a" == Base.TaggedString("some stringa")
    @test str * str == Base.TaggedString("some stringsome string")
    Base.annotate!(str, 1:4, :thing => 0x01)
    Base.annotate!(str, 5:11, :other => 0x02)
    Base.annotate!(str, 1:11, :all => 0x03)
    @test str[3:4] == SubString(str, 3, 4)
    @test Base.TaggedString(str[3:4]) ==
        Base.TaggedString("me", [(1:2, :thing => 0x01), (1:2, :all => 0x03)])
    @test str == Base.TaggedString("some string", [(1:4, :thing => 0x01), (1:11, :all => 0x03), (5:11, :other => 0x02)])
    @test str != Base.TaggedString("some string")
    @test str != Base.TaggedString("some string", [(1:1, :thing => 0x01), (5:5, :other => 0x02), (11:11, :all => 0x03)])
    @test str != Base.TaggedString("some string", [(1:4, :thing => 0x11), (1:11, :all => 0x13), (5:11, :other => 0x12)])
    @test str != Base.TaggedString("some thingg", [(1:4, :thing => 0x01), (1:11, :all => 0x03), (5:11, :other => 0x02)])
    let allstrings =
        ['a', Base.TaggedChar('a'), Base.TaggedChar('a', [:aaa => 0x04]),
        "a string", Base.TaggedString("a string"),
        Base.TaggedString("a string", [(1:2, :hmm => '%')])]
        for str1 in repeat(allstrings, 2)
            for str2 in repeat(allstrings, 2)
                @test String(str1 * str2) ==
                    String(string(str1, str2)) ==
                    String(string(str1)) * String(string(str2))
                @test Base.taggedstring(str1 * str2) ==
                    Base.taggedstring(str1, str2) ==
                    Base.taggedstring(str1) * Base.taggedstring(str2)
            end
        end
    end
    # @test collect(Base.eachstyle(str)) ==
    #     [("some", [:thing => 0x01, :all => 0x03]),
    #     (" string", [:all => 0x03, :other => 0x02])]
    @test ==(Base.taggedstring_optimize!(
        Base.TaggedString("abc", [(1:1, :val => 1),
                             (2:2, :val => 2),
                             (2:2, :val => 1),
                             (3:3, :val => 2)])),
             Base.TaggedString("abc", [(1:2, :val => 1),
                                  (2:3, :val => 2)]))
end

@testset "TaggedChar" begin
    chr = Base.TaggedChar('c')
    @test chr == Base.TaggedChar(chr.char, Pair{Symbol, Any}[])
    str = Base.TaggedString("hmm", [(1:1, :attr => "h0h0"),
                               (1:2, :attr => "h0m1"),
                               (2:3, :attr => "m1m2")])
    @test str[1] == Base.TaggedChar('h', Pair{Symbol, Any}[:attr => "h0h0"])
    @test str[2] == Base.TaggedChar('m', Pair{Symbol, Any}[:attr => "h0m1", :attr => "m1m2"])
    @test str[3] == Base.TaggedChar('m', Pair{Symbol, Any}[:attr => "m1m2"])
end

@testset "Styling preservation" begin
    str = Base.TaggedString("some string", [(1:4, :thing => 0x01), (1:11, :all => 0x03), (5:11, :other => 0x02)])
    @test match(r".e", str).match == str[3:4]
    @test  match(r"(.e)", str).captures == [str[3:4]]
    let m0 = match(r"(.)e", str)
        m1 = first(eachmatch(r"(.)e", str))
        for f in fieldnames(RegexMatch)
            @test getfield(m0, f) == getfield(m1, f)
        end
    end
    @test lpad(str, 12) ==
        Base.TaggedString(" some string", [(2:5, :thing => 0x01),
                                      (2:12, :all => 0x03),
                                      (6:12, :other => 0x02)])
    @test rpad(str, 12) ==
        Base.TaggedString("some string ", [(1:4, :thing => 0x01),
                                      (1:11, :all => 0x03),
                                      (5:11, :other => 0x02)])
    str1 = Base.TaggedString("test", [(1:4, :tag => 5)])
    str2 = Base.TaggedString("case", [(2:3, :tag => "oomph")])
    @test join([str1, str1], Base.TaggedString(" ")) ==
        Base.TaggedString("test test",
                     [(1:4, :tag => 5),
                      (6:9, :tag => 5)])
    @test join([str1, str1], Base.TaggedString(" ", [(1:1, :tag => 2)])) ==
        Base.TaggedString("test test",
                     [(1:4, :tag => 5),
                      (5:5, :tag => 2),
                      (6:9, :tag => 5)])
    @test repeat(str1, 2) == Base.TaggedString("testtest", [(1:8, :tag => 5)])
    @test repeat(str2, 2) == Base.TaggedString("casecase", [(2:3, :tag => "oomph"),
                                                       (6:7, :tag => "oomph")])
    @test repeat(str1[1], 3) == Base.TaggedString("ttt", [(1:3, :tag => 5)])
    @test reverse(str1) == Base.TaggedString("tset", [(1:4, :tag => 5)])
    @test reverse(str2) == Base.TaggedString("esac", [(2:3, :tag => "oomph")])
end
