# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "AnnotatedString" begin
    str = Base.AnnotatedString("some string")
    @test str == Base.AnnotatedString(str.string, Tuple{UnitRange{Int}, Pair{Symbol, Any}}[])
    @test length(str) == 11
    @test ncodeunits(str) == 11
    @test eltype(str) == Base.AnnotatedChar{eltype(str.string)}
    @test first(str) == Base.AnnotatedChar(first(str.string), Pair{Symbol, Any}[])
    @test str[1:4] isa SubString{typeof(str)}
    @test str[1:4] == Base.AnnotatedString("some")
    @test "a" * str == Base.AnnotatedString("asome string")
    @test str * "a" == Base.AnnotatedString("some stringa")
    @test str * str == Base.AnnotatedString("some stringsome string")
    Base.annotate!(str, 1:4, :thing => 0x01)
    Base.annotate!(str, 6:11, :other => 0x02)
    Base.annotate!(str, 1:11, :all => 0x03)
    #  :thing :other
    #  ┌┸─┐ ┌──┸─┐
    # "some string"
    #  └───┰─────┘
    #     :all
    @test str[3:4] == SubString(str, 3, 4)
    @test Base.AnnotatedString(str[3:4]) ==
        Base.AnnotatedString("me", [(1:2, :thing => 0x01), (1:2, :all => 0x03)])
    @test Base.AnnotatedString(str[3:6]) ==
        Base.AnnotatedString("me s", [(1:2, :thing => 0x01), (1:4, :all => 0x03), (4:4, :other => 0x02)])
    @test str == Base.AnnotatedString("some string", [(1:4, :thing => 0x01), (1:11, :all => 0x03), (6:11, :other => 0x02)])
    @test str != Base.AnnotatedString("some string")
    @test str != Base.AnnotatedString("some string", [(1:1, :thing => 0x01), (6:6, :other => 0x02), (11:11, :all => 0x03)])
    @test str != Base.AnnotatedString("some string", [(1:4, :thing => 0x11), (1:11, :all => 0x13), (6:11, :other => 0x12)])
    @test str != Base.AnnotatedString("some thingg", [(1:4, :thing => 0x01), (1:11, :all => 0x03), (6:11, :other => 0x02)])
    @test Base.AnnotatedString([Base.AnnotatedChar('a', [:a => 1]), Base.AnnotatedChar('b', [:b => 2])]) ==
        Base.AnnotatedString("ab", [(1:1, :a => 1), (2:2, :b => 2)])
    let allstrings =
        ['a', Base.AnnotatedChar('a'), Base.AnnotatedChar('a', [:aaa => 0x04]),
        "a string", Base.AnnotatedString("a string"),
        Base.AnnotatedString("a string", [(1:2, :hmm => '%')])]
        for str1 in repeat(allstrings, 2)
            for str2 in repeat(allstrings, 2)
                @test String(str1 * str2) ==
                    String(string(str1, str2)) ==
                    String(string(str1)) * String(string(str2))
                @test Base.annotatedstring(str1 * str2) ==
                    Base.annotatedstring(str1, str2) ==
                    Base.annotatedstring(str1) * Base.annotatedstring(str2)
            end
        end
    end
    # @test collect(Base.eachstyle(str)) ==
    #     [("some", [:thing => 0x01, :all => 0x03]),
    #     (" string", [:all => 0x03, :other => 0x02])]
    @test ==(Base.annotatedstring_optimize!(
        Base.AnnotatedString("abc", [(1:1, :val => 1),
                             (2:2, :val => 2),
                             (2:2, :val => 1),
                             (3:3, :val => 2)])),
             Base.AnnotatedString("abc", [(1:2, :val => 1),
                                  (2:3, :val => 2)]))
end

@testset "AnnotatedChar" begin
    chr = Base.AnnotatedChar('c')
    @test chr == Base.AnnotatedChar(chr.char, Pair{Symbol, Any}[])
    str = Base.AnnotatedString("hmm", [(1:1, :attr => "h0h0"),
                               (1:2, :attr => "h0m1"),
                               (2:3, :attr => "m1m2")])
    @test str[1] == Base.AnnotatedChar('h', Pair{Symbol, Any}[:attr => "h0h0"])
    @test str[2] == Base.AnnotatedChar('m', Pair{Symbol, Any}[:attr => "h0m1", :attr => "m1m2"])
    @test str[3] == Base.AnnotatedChar('m', Pair{Symbol, Any}[:attr => "m1m2"])
end

@testset "Styling preservation" begin
    str = Base.AnnotatedString("some string", [(1:4, :thing => 0x01), (1:11, :all => 0x03), (6:11, :other => 0x02)])
    @test match(r".e", str).match == str[3:4]
    @test  match(r"(.e)", str).captures == [str[3:4]]
    let m0 = match(r"(.)e", str)
        m1 = first(eachmatch(r"(.)e", str))
        for f in fieldnames(RegexMatch)
            @test getfield(m0, f) == getfield(m1, f)
        end
    end
    @test lpad(str, 12) ==
        Base.AnnotatedString(" some string", [(2:5, :thing => 0x01),
                                      (2:12, :all => 0x03),
                                      (7:12, :other => 0x02)])
    @test rpad(str, 12) ==
        Base.AnnotatedString("some string ", [(1:4, :thing => 0x01),
                                      (1:11, :all => 0x03),
                                      (6:11, :other => 0x02)])
    str1 = Base.AnnotatedString("test", [(1:4, :label => 5)])
    str2 = Base.AnnotatedString("case", [(2:3, :label => "oomph")])
    @test join([str1, str1], Base.AnnotatedString(" ")) ==
        Base.AnnotatedString("test test",
                     [(1:4, :label => 5),
                      (6:9, :label => 5)])
    @test join([str1, str1], Base.AnnotatedString(" ", [(1:1, :label => 2)])) ==
        Base.AnnotatedString("test test",
                     [(1:4, :label => 5),
                      (5:5, :label => 2),
                      (6:9, :label => 5)])
    @test repeat(str1, 2) == Base.AnnotatedString("testtest", [(1:8, :label => 5)])
    @test repeat(str2, 2) == Base.AnnotatedString("casecase", [(2:3, :label => "oomph"),
                                                       (6:7, :label => "oomph")])
    @test repeat(str1[1], 3) == Base.AnnotatedString("ttt", [(1:3, :label => 5)])
    @test reverse(str1) == Base.AnnotatedString("tset", [(1:4, :label => 5)])
    @test reverse(str2) == Base.AnnotatedString("esac", [(2:3, :label => "oomph")])
end
