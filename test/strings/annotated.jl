# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "AnnotatedString" begin
    str = Base.AnnotatedString("some string")
    @test str == Base.AnnotatedString(str.string, Base.RegionAnnotation[])
    @test length(str) == 11
    @test ncodeunits(str) == 11
    @test codeunits(str) == codeunits("some string")
    @test codeunit(str) == UInt8
    @test codeunit(str, 1) == codeunit("some string", 1)
    @test firstindex(str) == firstindex("some string")
    @test convert(Base.AnnotatedString, str) === str
    @test eltype(str) == Base.AnnotatedChar{eltype(str.string)}
    @test first(str) == Base.AnnotatedChar(first(str.string), Pair{Symbol, Any}[])
    @test str[1:4] isa SubString{typeof(str)}
    @test str[1:4] == Base.AnnotatedString("some")
    big_byte_str = Base.AnnotatedString("आख")
    @test_throws StringIndexError big_byte_str[5]
    @test "a" * str == Base.AnnotatedString("asome string")
    @test str * "a" == Base.AnnotatedString("some stringa")
    @test str * str == Base.AnnotatedString("some stringsome string")
    @test cmp(str, "some stringy thingy") == -1
    @test cmp("some stringy thingy", str) == 1
    @test str[3:4] == SubString("me")
    @test SubString("me") == str[3:4]
    Base.annotate!(str, 1:4, :thing, 0x01)
    Base.annotate!(str, 6:11, :other, 0x02)
    Base.annotate!(str, 1:11, :all, 0x03)
    #  :thing :other
    #  ┌┸─┐ ┌──┸─┐
    # "some string"
    #  └───┰─────┘
    #     :all
    @test str[3:4] == SubString(str, 3, 4)
    @test str[3:4] != SubString("me")
    @test SubString("me") != str[3:4]
    @test Base.AnnotatedString(str[3:4]) == SubString(str, 3, 4)
    @test repeat(SubString(str, 3, 4), 2) == repeat(Base.AnnotatedString(str[3:4]), 2)
    @test reverse(SubString(str, 3, 4)) == reverse(Base.AnnotatedString(str[3:4]))
    @test Base.AnnotatedString(str[3:4]) ==
        Base.AnnotatedString("me", [(1:2, :thing, 0x01), (1:2, :all, 0x03)])
    @test Base.AnnotatedString(str[3:6]) ==
        Base.AnnotatedString("me s", [(1:2, :thing, 0x01), (4:4, :other, 0x02), (1:4, :all, 0x03)])
    @test str == Base.AnnotatedString("some string", [(1:4, :thing, 0x01), (6:11, :other, 0x02), (1:11, :all, 0x03)])
    @test str != Base.AnnotatedString("some string")
    @test str != Base.AnnotatedString("some string", [(1:1, :thing, 0x01), (1:11, :all, 0x03), (6:6, :other, 0x02)])
    @test str != Base.AnnotatedString("some string", [(1:4, :thing, 0x11), (1:11, :all, 0x13), (6:11, :other, 0x12)])
    @test str != Base.AnnotatedString("some thingg", [(1:4, :thing, 0x01), (1:11, :all, 0x03), (6:11, :other, 0x02)])
    @test Base.AnnotatedString([Base.AnnotatedChar('a', [(:a, 1)]), Base.AnnotatedChar('b', [(:b, 2)])]) ==
        Base.AnnotatedString("ab", [(1:1, :a, 1), (2:2, :b, 2)])
    let allstrings =
        ['a', Base.AnnotatedChar('a'), Base.AnnotatedChar('a', [(:aaa, 0x04)]),
         "a string", Base.AnnotatedString("a string"),
         Base.AnnotatedString("a string", [(1:2, :hmm, '%')]),
         SubString(Base.AnnotatedString("a string", [(1:2, :hmm, '%')]), 1:1)]
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
    #     [("some", [:thing, 0x01, :all, 0x03]),
    #     (" string", [:all, 0x03, :other, 0x02])]
    @test chopprefix(sprint(show, str), "Base.") ==
        "AnnotatedString{String}(\"some string\", [(1:4, :thing, 0x01), (6:11, :other, 0x02), (1:11, :all, 0x03)])"
    @test eval(Meta.parse(repr(str))) == str
    @test sprint(show, MIME("text/plain"), str) == "\"some string\""

    a = Base.AnnotatedString("hello", [(1:5, :label, 1)])
    @test first(a) == Base.AnnotatedChar('h', [(:label, 1)])

    @test Bool === Base.infer_return_type(isvalid, Tuple{Base.AnnotatedString, Vararg})
    @test Int === Base.infer_return_type(ncodeunits, Tuple{Base.AnnotatedString})
end

@testset "AnnotatedChar" begin
    chr = Base.AnnotatedChar('c')
    @test Base.AnnotatedChar(UInt32('c')) == chr
    @test convert(Base.AnnotatedChar, chr) === chr
    @test chr == Base.AnnotatedChar(chr.char, Pair{Symbol, Any}[])
    @test uppercase(chr) == Base.AnnotatedChar('C')
    @test titlecase(chr) == Base.AnnotatedChar('C')
    @test lowercase(Base.AnnotatedChar('C')) == chr
    str = Base.AnnotatedString("hmm", [(1:1, :attr, "h0h0"),
                               (1:2, :attr, "h0m1"),
                               (2:3, :attr, "m1m2")])
    @test str[1] == Base.AnnotatedChar('h', [(:attr, "h0h0")])
    @test str[2] == Base.AnnotatedChar('m', [(:attr, "h0m1"), (:attr, "m1m2")])
    @test str[3] == Base.AnnotatedChar('m', [(:attr, "m1m2")])
end

@testset "Styling preservation" begin
    str = Base.AnnotatedString("some string", [(1:4, :thing, 0x01), (1:11, :all, 0x03), (6:11, :other, 0x02)])
    @test match(r".e", str).match == str[3:4]
    @test  match(r"(.e)", str).captures == [str[3:4]]
    let m0 = match(r"(.)e", str)
        m1 = first(eachmatch(r"(.)e", str))
        for f in fieldnames(RegexMatch)
            @test getfield(m0, f) == getfield(m1, f)
        end
    end
    @test lpad(str, 12) ==
        Base.AnnotatedString(" some string", [(2:5, :thing, 0x01),
                                      (2:12, :all, 0x03),
                                      (7:12, :other, 0x02)])
    @test rpad(str, 12) ==
        Base.AnnotatedString("some string ", [(1:4, :thing, 0x01),
                                      (1:11, :all, 0x03),
                                      (6:11, :other, 0x02)])
    str1 = Base.AnnotatedString("test", [(1:4, :label, 5)])
    str2 = Base.AnnotatedString("case", [(2:3, :label, "oomph")])
    @test join([str1, str1], ' ') ==
        Base.AnnotatedString("test test",
                     [(1:4, :label, 5),
                      (6:9, :label, 5)])
    @test join([str1, str1], Base.AnnotatedString(" ", [(1:1, :label, 2)])) ==
        Base.AnnotatedString("test test",
                     [(1:4, :label, 5),
                      (5:5, :label, 2),
                      (6:9, :label, 5)])
    @test join((String(str1), str1), ' ') ==
        Base.AnnotatedString("test test", [(6:9, :label, 5)])
    @test repeat(str1, 2) == Base.AnnotatedString("testtest", [(1:8, :label, 5)])
    @test repeat(str2, 2) == Base.AnnotatedString("casecase", [(2:3, :label, "oomph"),
                                                       (6:7, :label, "oomph")])
    @test repeat(str1[1], 3) == Base.AnnotatedString("ttt", [(1:3, :label, 5)])
    @test reverse(str1) == Base.AnnotatedString("tset", [(1:4, :label, 5)])
    @test reverse(str2) == Base.AnnotatedString("esac", [(2:3, :label, "oomph")])
end

@testset "Unicode" begin
    for words in (["ᲃase", "cɦɒnɡeȿ", "can", "CHⱯNGE", "Сodeunıts"],
                  ["Сodeunıts", "ᲃase", "cɦɒnɡeȿ", "can", "CHⱯNGE"])
        ann_words = [Base.AnnotatedString(w, [(1:ncodeunits(w), :i, i)])
                     for (i, w) in enumerate(words)]
        ann_str = join(ann_words, '-')
        for transform in (lowercase, uppercase, titlecase)
            t_words = map(transform, words)
            ann_t_words = [Base.AnnotatedString(w, [(1:ncodeunits(w), :i, i)])
                        for (i, w) in enumerate(t_words)]
            ann_t_str = join(ann_t_words, '-')
            t_ann_str = transform(ann_str)
            @test String(ann_t_str) == String(t_ann_str)
            @test Base.annotations(ann_t_str) == Base.annotations(t_ann_str)
        end
        for transform in (uppercasefirst, lowercasefirst)
            t_words = vcat(transform(first(words)), words[2:end])
            ann_t_words = [Base.AnnotatedString(w, [(1:ncodeunits(w), :i, i)])
                        for (i, w) in enumerate(t_words)]
            ann_t_str = join(ann_t_words, '-')
            t_ann_str = transform(ann_str)
            @test String(ann_t_str) == String(t_ann_str)
            @test Base.annotations(ann_t_str) == Base.annotations(t_ann_str)
        end
    end
end

@testset "AnnotatedIOBuffer" begin
    aio = Base.AnnotatedIOBuffer()
    vec2ann(v::Vector{<:Tuple}) = collect(Base.RegionAnnotation, v)
    # Append-only writing
    @test write(aio, Base.AnnotatedString("hello", [(1:5, :tag, 1)])) == 5
    @test write(aio, ' ') == 1
    @test write(aio, Base.AnnotatedString("world", [(1:5, :tag, 2)])) == 5
    @test Base.annotations(aio) == vec2ann([(1:5, :tag, 1), (7:11, :tag, 2)])
    # Check `annotate!`, including region sorting
    @test truncate(aio, 0).io.size == 0
    @test write(aio, "hello world") == ncodeunits("hello world")
    @test Base.annotate!(aio, 1:5, :tag, 1) === aio
    @test Base.annotate!(aio, 7:11, :tag, 2) === aio
    @test Base.annotations(aio) == vec2ann([(1:5, :tag, 1), (7:11, :tag, 2)])
    # Reading
    @test read(seekstart(deepcopy(aio.io)), String) == "hello world"
    @test read(seekstart(deepcopy(aio)), String) == "hello world"
    @test read(seek(aio, 0), Base.AnnotatedString) == Base.AnnotatedString("hello world", [(1:5, :tag, 1), (7:11, :tag, 2)])
    @test read(seek(aio, 1), Base.AnnotatedString) == Base.AnnotatedString("ello world", [(1:4, :tag, 1), (6:10, :tag, 2)])
    @test read(seek(aio, 4), Base.AnnotatedString) == Base.AnnotatedString("o world", [(1:1, :tag, 1), (3:7, :tag, 2)])
    @test read(seek(aio, 5), Base.AnnotatedString) == Base.AnnotatedString(" world", [(2:6, :tag, 2)])
    @test read(seekend(aio), Base.AnnotatedString) == Base.AnnotatedString("")
    @test read(seekstart(truncate(deepcopy(aio), 5)), Base.AnnotatedString) == Base.AnnotatedString("hello", [(1:5, :tag, 1)])
    @test read(seekstart(truncate(deepcopy(aio), 6)), Base.AnnotatedString) == Base.AnnotatedString("hello ", [(1:5, :tag, 1)])
    @test read(seekstart(truncate(deepcopy(aio), 7)), Base.AnnotatedString) == Base.AnnotatedString("hello w", [(1:5, :tag, 1), (7:7, :tag, 2)])
    @test read(seek(aio, 0), Base.AnnotatedChar) == Base.AnnotatedChar('h', [(:tag, 1)])
    @test read(seek(aio, 5), Base.AnnotatedChar) == Base.AnnotatedChar(' ', [])
    @test read(seek(aio, 6), Base.AnnotatedChar) == Base.AnnotatedChar('w', [(:tag, 2)])
    # Check method compatibility with IOBuffer
    @test position(aio) == 7
    @test seek(aio, 4) === aio
    @test skip(aio, 2) === aio
    @test Base.annotations(copy(aio)) == Base.annotations(aio)
    @test take!(copy(aio).io) == take!(copy(aio.io))
    # Writing into the middle of the buffer
    @test write(seek(aio, 6), "alice") == 5 # Replace 'world' with 'alice'
    @test read(seekstart(aio), String) == "hello alice"
    @test Base.annotations(aio) == vec2ann([(1:5, :tag, 1), (7:11, :tag, 2)]) # Should be unchanged
    @test write(seek(aio, 0), Base.AnnotatedString("hey-o", [(1:5, :hey, 'o')])) == 5
    @test read(seekstart(aio), String) == "hey-o alice"
    @test Base.annotations(aio) == vec2ann([(7:11, :tag, 2), (1:5, :hey, 'o')]) # First annotation should have been entirely replaced
    @test write(seek(aio, 7), Base.AnnotatedString("bbi", [(1:3, :hey, 'a')])) == 3 # a[lic, bbi]e ('alice', 'abbie')
    @test read(seekstart(aio), String) == "hey-o abbie"
    @test Base.annotations(aio) == vec2ann([(7:7, :tag, 2), (11:11, :tag, 2), (1:5, :hey, 'o'), (8:10, :hey, 'a')])
    @test write(seek(aio, 0), Base.AnnotatedString("ab")) == 2 # Check first annotation's region is adjusted correctly
    @test read(seekstart(aio), String) == "aby-o abbie"
    @test Base.annotations(aio) == vec2ann([(7:7, :tag, 2), (11:11, :tag, 2), (3:5, :hey, 'o'), (8:10, :hey, 'a')])
    @test write(seek(aio, 3), Base.AnnotatedString("ss")) == 2
    @test read(seekstart(aio), String) == "abyss abbie"
    @test Base.annotations(aio) == vec2ann([(7:7, :tag, 2), (11:11, :tag, 2), (3:3, :hey, 'o'), (8:10, :hey, 'a')])
    # Writing one buffer to another
    newaio = Base.AnnotatedIOBuffer()
    @test write(newaio, seekstart(aio)) == 11
    @test read(seekstart(newaio), String) == "abyss abbie"
    @test Base.annotations(newaio) == Base.annotations(aio)
    @test write(seek(newaio, 5), seek(aio, 5)) == 6
    @test sort(Base.annotations(newaio)) == sort(Base.annotations(aio))
    @test write(newaio, seek(aio, 5)) == 6
    @test read(seekstart(newaio), String) == "abyss abbie abbie"
    @test sort(Base.annotations(newaio)) ==
        sort(vcat(Base.annotations(aio), vec2ann([(13:13, :tag, 2), (14:16, :hey, 'a'), (17:17, :tag, 2)])))
    # The `_insert_annotations!` cautious-merging optimisation
    aio = Base.AnnotatedIOBuffer()
    @test write(aio, Base.AnnotatedChar('a', [(:a, 1), (:b, 2)])) == 1
    @test Base.annotations(aio) == vec2ann([(1:1, :a, 1), (1:1, :b, 2)])
    @test write(aio, Base.AnnotatedChar('b', [(:a, 1), (:b, 2)])) == 1
    @test Base.annotations(aio) == vec2ann([(1:2, :a, 1), (1:2, :b, 2)])
    let aio2 = copy(aio) # A different start makes merging too risky to do.
        @test write(aio2, Base.AnnotatedChar('c', [(:a, 0), (:b, 2)])) == 1
        @test Base.annotations(aio2) == vec2ann([(1:2, :a, 1), (1:2, :b, 2), (3:3, :a, 0), (3:3, :b, 2)])
    end
    let aio2 = copy(aio) # Merging some run of the most recent annotations is fine though.
        @test write(aio2, Base.AnnotatedChar('c', [(:b, 2)])) == 1
        @test Base.annotations(aio2) == vec2ann([(1:2, :a, 1), (1:3, :b, 2)])
    end
    let aio2 = copy(aio) # ...and any subsequent annotations after a matching run can just be copied over.
        @test write(aio2, Base.AnnotatedChar('c', [(:b, 2), (:c, 3), (:d, 4)])) == 1
        @test Base.annotations(aio2) == vec2ann([(1:2, :a, 1), (1:3, :b, 2), (3:3, :c, 3), (3:3, :d, 4)])
    end
    let aio2 = Base.AnnotatedIOBuffer()
        @test write(aio2, Base.AnnotatedChar('a', [(:b, 1)])) == 1
        @test write(aio2, Base.AnnotatedChar('b', [(:a, 1), (:b, 1)])) == 1
        @test read(seekstart(aio2), Base.AnnotatedString) ==
            Base.AnnotatedString("ab", [(1:1, :b, 1), (2:2, :a, 1), (2:2, :b, 1)])
    end
    # Working through an IOContext
    aio = Base.AnnotatedIOBuffer()
    wrapio = IOContext(aio)
    @test write(wrapio, Base.AnnotatedString("hey", [(1:3, :x, 1)])) == 3
    @test write(wrapio, Base.AnnotatedChar('a', [(:y, 2)])) == 1
    @test read(seekstart(aio), Base.AnnotatedString) ==
        Base.AnnotatedString("heya", [(1:3, :x, 1), (4:4, :y, 2)])
    # show-ing an AnnotatedIOBuffer
    aio = Base.AnnotatedIOBuffer()
    write(aio, Base.AnnotatedString("hello", [(1:5, :tag, 1)]))
    @test sprint(show, aio) == "Base.AnnotatedIOBuffer(5 bytes, 1 annotation)"
end

@testset "Eachregion" begin
    annregions(str::String, annots::Vector{<:Tuple{UnitRange{Int}, Symbol, <:Any}}) =
        [(s, Tuple.(a)) for (s, a) in Base.eachregion(Base.AnnotatedString(str, annots))]
    # Regions that do/don't extend to the left/right edges
    @test annregions(" abc ", [(2:4, :face, :bold)]) ==
        [(" ", []),
         ("abc", [(:face, :bold)]),
         (" ", [])]
    @test annregions(" x ", [(2:2, :face, :bold)]) ==
        [(" ", []),
         ("x", [(:face, :bold)]),
         (" ", [])]
    @test annregions(" x", [(2:2, :face, :bold)]) ==
        [(" ", []),
         ("x", [(:face, :bold)])]
    @test annregions("x ", [(1:1, :face, :bold)]) ==
        [("x", [(:face, :bold)]),
         (" ", [])]
    @test annregions("x", [(1:1, :face, :bold)]) ==
        [("x", [(:face, :bold)])]
    # Overlapping/nested regions
    @test annregions(" abc ", [(2:4, :face, :bold), (3:3, :face, :italic)]) ==
        [(" ", []),
         ("a", [(:face, :bold)]),
         ("b", [(:face, :bold), (:face, :italic)]),
         ("c", [(:face, :bold)]),
         (" ", [])]
    @test annregions("abc-xyz", [(1:7, :face, :bold), (1:3, :face, :green), (4:4, :face, :yellow), (4:7, :face, :italic)]) ==
        [("abc", [(:face, :bold), (:face, :green)]),
         ("-", [(:face, :bold), (:face, :yellow), (:face, :italic)]),
         ("xyz", [(:face, :bold), (:face, :italic)])]
    # Preserving annotation order
    @test annregions("abcd", [(1:3, :face, :red), (2:2, :face, :yellow), (2:3, :face, :green), (2:4, :face, :blue)]) ==
        [("a", [(:face, :red)]),
         ("b", [(:face, :red), (:face, :yellow), (:face, :green), (:face, :blue)]),
         ("c", [(:face, :red), (:face, :green), (:face, :blue)]),
         ("d", [(:face, :blue)])]
    @test annregions("abcd", [(2:4, :face, :blue), (1:3, :face, :red), (2:3, :face, :green), (2:2, :face, :yellow)]) ==
        [("a", [(:face, :red)]),
         ("b", [(:face, :blue), (:face, :red), (:face, :green), (:face, :yellow)]),
         ("c", [(:face, :blue), (:face, :red), (:face, :green)]),
         ("d", [(:face, :blue)])]
    # Region starting after a character spanning multiple codepoints.
    @test annregions("𝟏x", [(1:4, :face, :red)]) ==
        [("𝟏", [(:face, :red)]),
         ("x", [])]
end

@testset "Replacement" begin
    astr(s::String, faceregions::Tuple{UnitRange{Int}, Symbol}...) =
        Base.AnnotatedString(s, [(r, :face, f) for (r, f) in faceregions])

    @testset "Basic Transformations" begin
        @testset "Deletion" begin
            @test replace(astr("hello world", (1:5, :red)), "hello" => "hi") ==
                astr("hi world")
            @test replace(astr("foofoo", (1:3, :red), (4:6, :green)), "foo" => "x") ==
                astr("xx")
            @test replace(astr("foofoo", (1:3, :red), (4:6, :green)), "foo" => "x", count=1) ==
                astr("xfoo", (2:4, :green))
            @test replace(astr("abcdef", (1:6, :red), (3:4, :green)), "cd" => "X") ==
                astr("abXef", (1:2, :red), (4:5, :red))
            @test replace(astr("a b c", (1:1, :red), (3:3, :green), (5:5, :blue)),
                         "a" => "x", "b" => "y", "c" => "z") ==
                astr("x y z")
        end

        @testset "Shifting" begin
            @test replace(astr("hello world", (7:11, :red)), "hello" => "hi") ==
                astr("hi world", (4:8, :red))
            @test replace(astr("hello world", (7:11, :red)), "hello" => "greetings") ==
                astr("greetings world", (11:15, :red))
            @test replace(astr("a b c", (3:3, :red)), "a" => "xxx", "c" => "y") ==
                astr("xxx b y", (5:5, :red))
            @test replace(astr("abc def", (5:7, :green)), "abc" => "x") ==
                astr("x def", (3:5, :green))
            @test replace(astr("a b c d", (3:3, :red), (5:5, :green), (7:7, :blue)), "a" => "AA") ==
                astr("AA b c d", (4:4, :red), (6:6, :green), (8:8, :blue))
            @test replace(astr("hello world", (7:11, :green)), " world" => " Julia") ==
                astr("hello Julia")
        end

        @testset "Splitting" begin
            @test replace(astr("hello world", (1:11, :red)), " " => "_") ==
                astr("hello_world", (1:5, :red), (7:11, :red))
            @test replace(astr("a b c", (1:5, :red)), " " => "_") ==
                astr("a_b_c", (1:1, :red), (3:3, :red), (5:5, :red))
            @test replace(astr("foobarbaz", (1:9, :green)), "o" => "0", "a" => "A") ==
                astr("f00bArbAz", (1:1, :green), (4:4, :green), (6:7, :green), (9:9, :green))
            @test replace(astr("a b c", (1:5, :red)), " " => "_", count=1) ==
                astr("a_b c", (1:1, :red), (3:5, :red))
            @test replace(astr("abcde", (2:4, :red)), "c" => "X") ==
                astr("abXde", (2:2, :red), (4:4, :red))
            @test replace(astr("a a a a", (1:7, :blue)), "a" => "b") ==
                astr("b b b b", (2:2, :blue), (4:4, :blue), (6:6, :blue))
        end

        @testset "Addition" begin
            @test replace(astr("hello world"), "world" => astr("Julia", (1:5, :red))) ==
                astr("hello Julia", (7:11, :red))
            @test replace(astr("hello"), "hello" => astr("hi there", (1:2, :red), (4:8, :green))) ==
                astr("hi there", (1:2, :red), (4:8, :green))
            @test replace(astr("hello world", (7:11, :green)), "hello" => astr("hi", (1:2, :red))) ==
                astr("hi world", (4:8, :green), (1:2, :red))
            @test replace(astr("a b", (1:3, :yellow)), " " => astr("_", (1:1, :red))) ==
                astr("a_b", (1:1, :yellow), (3:3, :yellow), (2:2, :red))
            @test replace(astr("a b"), "a" => astr("X", (1:1, :red)), "b" => astr("Y", (1:1, :blue))) ==
                astr("X Y", (1:1, :red), (3:3, :blue))
        end

        @testset "Combinations" begin
            @test replace(astr("a b c", (1:1, :red), (5:5, :blue)), "b" => "B") ==
                astr("a B c", (1:1, :red), (5:5, :blue))
            @test replace(astr("a b", (1:3, :red)), " " => astr("_", (1:1, :blue))) ==
                astr("a_b", (1:1, :red), (3:3, :red), (2:2, :blue))
            @test replace(astr("foo bar baz", (1:3, :red), (5:7, :green), (9:11, :blue)),
                         "foo" => "F", "a" => astr("A", (1:1, :yellow))) ==
                astr("F bAr bAz", (3:3, :green), (5:5, :green), (7:7, :blue), (9:9, :blue),
                     (4:4, :yellow), (8:8, :yellow))
        end
    end

    @testset "Pattern Types" begin
        @testset "Char" begin
            @test replace(astr("hello", (1:5, :red)), 'l' => 'L') ==
                astr("heLLo", (1:2, :red), (5:5, :red))
            @test replace(astr("hello"), 'o' => astr("O", (1:1, :red))) ==
                astr("hellO", (5:5, :red))
            @test replace(astr("aaa", (1:3, :red)), 'a' => 'b') ==
                astr("bbb")
            @test replace(astr("aaa", (1:3, :red)), 'a' => 'b', count=2) ==
                astr("bba", (3:3, :red))
            @test replace(astr("café", (1:5, :green)), 'é' => 'e') ==
                astr("cafe", (1:3, :green))
            @test replace(astr("test"), 't' => astr("TTT", (1:3, :blue))) ==
                astr("TTTesTTT", (1:3, :blue), (6:8, :blue))
            @test replace(astr("hello", (1:5, :red)), 'l' => Base.AnnotatedChar('L', [(label=:face, value=:blue)])) ==
                astr("heLLo", (1:2, :red), (5:5, :red), (3:4, :blue))
            @test replace(astr("abc", (1:3, :green)), 'b' => Base.AnnotatedChar('B', [(label=:face, value=:bold)])) ==
                astr("aBc", (1:1, :green), (3:3, :green), (2:2, :bold))
        end

        @testset "Regex" begin
            @test replace(astr("foo bar", (1:7, :green)), r"o+" => "0") ==
                astr("f0 bar", (1:1, :green), (3:6, :green))
            @test replace(astr("hello"), r"l+" => astr("L", (1:1, :red))) ==
                astr("heLo", (3:3, :red))
            @test replace(astr("ab", (1:2, :red)), r"" => "^") ==
                astr("^a^b^", (2:2, :red), (4:4, :red))
            @test replace(astr("abc", (1:3, :red)), r"b?" => "X") ==
                astr("XaXcX", (2:2, :red), (4:4, :red))
            @test replace(astr("aaa", (1:3, :red)), r"a+" => "b") ==
                astr("b")
        end

        @testset "Predicate" begin
            @test replace(astr("abc", (1:3, :red)), islowercase => 'X') ==
                astr("XXX")
        end

        @testset "Count" begin
            @test replace(astr("hello", (1:5, :red)), "l" => "L", count=0) ==
                astr("hello", (1:5, :red))
            @test replace(astr("a b c", (5:5, :red)), "a" => "A", count=1) ==
                astr("A b c", (5:5, :red))
            @test replace(astr("a b c", (1:5, :red)), " " => "_", count=1) ==
                astr("a_b c", (1:1, :red), (3:5, :red))
            @test replace(astr("a b"), "a" => astr("X", (1:1, :red)),
                         "b" => astr("Y", (1:1, :blue)), count=1) ==
                astr("X b", (1:1, :red))
            @test replace(astr("abc", (1:3, :red)), "x" => "y", count=10) ==
                astr("abc", (1:3, :red))
        end

        @testset "AnnotatedChar" begin
            @test replace(astr("test", (1:4, :red)), 't' => Base.AnnotatedChar('T', [(label=:face, value=:blue)])) ==
                astr("TesT", (2:3, :red), (1:1, :blue), (4:4, :blue))
            @test replace(astr("hello"), 'l' => Base.AnnotatedChar('L', [(label=:face, value=:bold), (label=:face, value=:red)])) ==
                astr("heLLo", (3:4, :bold), (3:4, :red))
            @test replace(astr("a b c", (1:5, :green)), ' ' => Base.AnnotatedChar('_', [(label=:face, value=:underline)])) ==
                astr("a_b_c", (1:1, :green), (3:3, :green), (5:5, :green), (2:2, :underline), (4:4, :underline))
        end

        @testset "SubString" begin
            source = astr("WORLD", (1:5, :blue))
            @test replace(astr("hello world", (1:11, :red)), "world" => SubString(source, 1:5)) ==
                astr("hello WORLD", (1:6, :red), (7:11, :blue))
            source2 = astr("TEST", (1:2, :green), (3:4, :cyan))
            @test replace(astr("foo bar"), "bar" => SubString(source2, 1:4)) ==
                astr("foo TEST", (5:6, :green), (7:8, :cyan))
            source3 = astr("annotation", (1:10, :emphasis))
            @test replace(astr("replace me", (1:10, :red)), "me" => SubString(source3, 1:2)) ==
                astr("replace an", (1:8, :red), (9:10, :emphasis))
        end
    end

    @testset "Multiple Replacements" begin
        @test replace(astr("foo bar baz", (1:3, :red), (5:7, :green), (9:11, :blue)),
                     "foo" => "F", "bar" => "B", "baz" => "Z") ==
            astr("F B Z")
        @test replace(astr("foo bar"), "foo" => astr("F", (1:1, :red)), "bar" => "B") ==
            astr("F B", (1:1, :red))
        @test replace(astr("abc", (1:3, :red)), "a" => "A", "b" => "B", "c" => "C") ==
            astr("ABC")
        @test replace(astr("foo bar foo", (1:3, :red), (9:11, :green)),
                     "foo" => "F", "bar" => "B", count=2) ==
            astr("F B foo", (5:7, :green))
        @test replace(astr("a b c", (5:5, :cyan)),
                     "a" => astr("X", (1:1, :red)), "b" => astr("Y", (1:1, :blue))) ==
            astr("X Y c", (5:5, :cyan), (1:1, :red), (3:3, :blue))
        @test replace(astr("xaybzc", (2:2, :red), (4:4, :green), (6:6, :blue)),
                     "x" => "X", "y" => "Y", "z" => "Z") ==
            astr("XaYbZc", (2:2, :red), (4:4, :green), (6:6, :blue))
        @test replace(astr("foo123bar", (1:3, :red), (7:9, :green)),
                     r"(\d+)" => astr("NUM", (1:3, :blue))) ==
            astr("fooNUMbar", (1:3, :red), (7:9, :green), (4:6, :blue))

        @testset "Pattern type combinations" begin
            @test replace(astr("a1b2c", (1:5, :red)), 'a' => "A", r"\d" => "X") ==
                astr("AXbXc", (3:3, :red), (5:5, :red))
            @test replace(astr("HeLLo", (1:5, :green)), isuppercase => 'x', "LL" => "ll") ==
                astr("xexxo", (2:2, :green), (5:5, :green))
            @test replace(astr("test123", (1:7, :blue)), isdigit => 'X', "test" => "TEST") ==
                astr("TESTXXX")
        end

        @testset "Overlapping patterns" begin
            @test replace(astr("aaaa", (1:4, :red)), "aa" => "b") ==
                astr("bb")
            @test replace(astr("abcabc", (1:3, :red), (4:6, :green)), "abc" => "X", "bc" => "Y") ==
                astr("XX")
        end

        @testset "Count with multiple patterns" begin
            @test replace(astr("a b a b", (1:7, :red)), "a" => "A", "b" => "B", count=3) ==
                astr("A B A b", (2:2, :red), (4:4, :red), (6:7, :red))
            @test replace(astr("x o x o x o", (1:11, :blue)), 'x' => "X", 'o' => "O", count=4) ==
                astr("X O X O x o", (2:2, :blue), (4:4, :blue), (6:6, :blue), (8:11, :blue))
        end
    end

    @testset "Edge Cases" begin
        @testset "Boundaries" begin
            @test replace(astr("abcdef", (4:6, :red)), "abc" => "x") ==
                astr("xdef", (2:4, :red))
            @test replace(astr("abcdef", (1:3, :red)), "def" => "x") ==
                astr("abcx", (1:3, :red))
            @test replace(astr("abcdef", (3:6, :red)), "abcd" => "X") ==
                astr("Xef", (2:3, :red))
            @test replace(astr("abcdef", (1:4, :red)), "cdef" => "X") ==
                astr("abX", (1:2, :red))
            @test replace(astr("abc", (2:2, :red)), "b" => "B") ==
                astr("aBc")
            @test replace(astr("abc", (3:3, :red)), "a" => "A") ==
                astr("Abc", (3:3, :red))
            @test replace(astr("foobar", (1:3, :red)), "foo" => "x") ==
                astr("xbar")
            @test replace(astr("foobar", (4:6, :red)), "bar" => "x") ==
                astr("foox")
        end

        @testset "Empty" begin
            @test replace(astr("hello", (1:5, :red)), "x" => "y") ==
                astr("hello", (1:5, :red))
            @test replace(astr(""), "x" => "y") == astr("")
            @test replace(astr("", (1:0, :red)), "" => "x") == astr("x")
            @test replace(astr("ab", (1:2, :red)), "" => "^") ==
                astr("^a^b^", (2:2, :red), (4:4, :red))
            @test replace(astr("hello", (1:5, :red)), "l" => "") ==
                astr("heo", (1:2, :red), (3:3, :red))
            @test replace(astr("hello world", (7:11, :green)), "hello " => astr("")) ==
                astr("world", (1:5, :green))
        end

        @testset "Unicode" begin
            @test replace(astr("føø bar", (1:4, :red)), "føø" => "foo") ==
                astr("foo bar")
            @test replace(astr("hello", (1:5, :red)), "llo" => "ḻḻø") ==
                astr("heḻḻø", (1:2, :red))
            @test replace(astr("foo"), "foo" => astr("ƀäṙ", (1:6, :red))) ==
                astr("ƀäṙ", (1:6, :red))
            @test replace(astr("a𝟏b", (1:6, :red)), "𝟏" => "1") ==
                astr("a1b", (1:1, :red), (3:3, :red))
            @test replace(astr("ḟøø bär", (1:12, :green)), " " => "_") ==
                astr("ḟøø_bär", (1:7, :green), (9:12, :green))
            @test replace(astr("a𝟏b𝟏c", (1:9, :red)), "𝟏" => "1") ==
                astr("a1b1c", (1:1, :red), (3:3, :red))
        end

        @testset "Special characters" begin
            @test replace(astr("a\nb", (1:3, :red)), "\n" => " ") ==
                astr("a b", (1:1, :red), (3:3, :red))
            @test replace(astr("a\tb", (1:3, :red)), "\t" => " ") ==
                astr("a b", (1:1, :red), (3:3, :red))
            @test replace(astr("a\0b", (1:3, :red)), "\0" => "x") ==
                astr("axb", (1:1, :red), (3:3, :red))
        end

        @testset "Annotation edge cases" begin
            @test replace(astr("hello", (1:5, :blue)), "l" => "L") ==
                astr("heLLo", (1:2, :blue), (5:5, :blue))
            @test replace(astr("aabb", (1:4, :red)), "a" => "x", "b" => "y") ==
                astr("xxyy")
            @test replace(astr("hello", (1:3, :red), (2:4, :green)), "el" => "X") ==
                astr("hXlo", (1:1, :red), (3:3, :green))
            str_multi = astr("test", (1:4, :red), (1:4, :en))
            @test replace(str_multi, "test" => "ok") == astr("ok")
            str2 = astr("a b", (1:3, :red), (1:3, :bold))
            result = replace(str2, " " => "_")
            @test String(result) == "a_b"
            @test length(Base.annotations(result)) == 4
            str3 = astr("hi world", (4:8, :red), (4:8, :bold))
            result2 = replace(str3, "hi" => "hello")
            @test String(result2) == "hello world"
            @test length(Base.annotations(result2)) == 2

            str_triple = astr("abc", (1:3, :red), (1:3, :bold), (1:3, :italic))
            @test replace(str_triple, "b" => "B") ==
                astr("aBc", (1:1, :red), (3:3, :red),
                     (1:1, :bold), (3:3, :bold),
                     (1:1, :italic), (3:3, :italic))

            str_nested = astr("abcde", (1:5, :outer), (2:4, :middle), (3:3, :inner))
            @test replace(str_nested, "c" => "X") ==
                astr("abXde", (1:2, :outer), (4:5, :outer), (2:2, :middle), (4:4, :middle))

            str_same_label = astr("test", (1:2, :val1), (3:4, :val2))
            @test replace(str_same_label, "es" => "X") ==
                astr("tXt", (1:1, :val1), (3:3, :val2))
        end

        @testset "Size variations" begin
            @test replace(astr("hello", (1:5, :red)), "hello" => "world") ==
                astr("world")
            @test replace(astr("hi", (1:2, :red)), "hi" => "hello") ==
                astr("hello")
            @test replace(astr("hello", (1:5, :red)), "hello" => "hi") ==
                astr("hi")
            @test replace(astr("a b c", (1:1, :red), (3:3, :green), (5:5, :blue)), "b" => "B") ==
                astr("a B c", (1:1, :red), (5:5, :blue))
            @test replace(astr("hello world", (1:5, :red)), "world" => "there") ==
                astr("hello there", (1:5, :red))
            @test replace(astr("hello world", (7:11, :green)), "hello" => "hi") ==
                astr("hi world", (4:8, :green))
            @test replace(astr("hi"), "hi" => astr("hello world", (1:5, :red), (7:11, :green))) ==
                astr("hello world", (1:5, :red), (7:11, :green))
            @test replace(astr("hello world"), "hello world" => astr("hi", (1:2, :red))) ==
                astr("hi", (1:2, :red))
            @test replace(astr("aabbcc", (1:6, :red)), "a" => "x", "b" => "y", "c" => "z") ==
                astr("xxyyzz")
            @test replace(astr("hello", (1:5, :red)), "hello" => astr("hello", (1:5, :blue))) ==
                astr("hello", (1:5, :blue))
        end

        @testset "Complex" begin
            @test replace(astr("a b c d", (1:7, :red)), " " => "_") ==
                astr("a_b_c_d", (1:1, :red), (3:3, :red), (5:5, :red), (7:7, :red))
            annots = [(i:i, :red) for i in 1:2:9]
            @test replace(astr("a b c d e", annots...), " " => "_") ==
                astr("a_b_c_d_e", (1:1, :red), (3:3, :red), (5:5, :red), (7:7, :red), (9:9, :red))
            @test replace(astr("abcdefgh", (1:8, :red)), "cd" => "X") ==
                astr("abXefgh", (1:2, :red), (4:7, :red))
            @test replace(astr("hello world"), "world" => "Julia") ==
                astr("hello Julia")
            @test replace(astr("hello", (1:5, :red)), "ello" => uppercase) ==
                astr("hELLO", (1:1, :red))

            str_code = astr("function test()", (1:8, :keyword), (10:13, :identifier))
            @test replace(str_code, "test" => "demo", "(" => "[", ")" => "]") ==
                astr("function demo[]", (1:8, :keyword))

            str_markdown = astr("This is *bold* text", (9:13, :emphasis))
            @test replace(str_markdown, "*" => "", "bold" => astr("BOLD", (1:4, :strong))) ==
                astr("This is BOLD text", (9:12, :strong))

            str_chain = astr("aaa", (1:3, :red))
            str_chain = replace(str_chain, "a" => astr("b", (1:1, :green)))
            str_chain = replace(str_chain, "b" => astr("c", (1:1, :blue)))
            @test String(str_chain) == "ccc"
            @test length(Base.annotations(str_chain)) == 1

            str_interleaved = astr("a1b2c3", (1:1, :red), (3:3, :green), (5:5, :blue))
            @test replace(str_interleaved, r"\d" => astr("X", (1:1, :yellow))) ==
                astr("aXbXcX", (1:1, :red), (3:3, :green), (5:5, :blue),
                     (2:2, :yellow), (4:4, :yellow), (6:6, :yellow))
        end



        @testset "Overlapping annotations" begin
            # Annotations that overlap on the same text
            str_overlap = astr("hello world", (1:5, :red), (3:9, :bold))
            @test replace(str_overlap, "ll" => "LL") ==
                astr("heLLo world", (1:2, :red), (5:5, :red), (5:9, :bold))
            # Multiple overlapping annotations
            str_multi = astr("testing", (1:7, :outer), (2:6, :middle), (3:5, :inner))
            @test replace(str_multi, "es" => "ES") ==
                astr("tESting", (1:1, :outer), (4:7, :outer),
                     (4:6, :middle),
                     (4:5, :inner))
        end

        @testset "Multiple annotations same region" begin
            # Same region with different labels
            str = astr("test", (1:4, :red), (1:4, :bold), (2:3, :italic))
            @test replace(str, "es" => "ES") ==
                astr("tESt", (1:1, :red), (4:4, :red),
                     (1:1, :bold), (4:4, :bold))
            # Same label, different values on different regions
            str2 = astr("test", (1:2, :red), (3:4, :blue))
            @test replace(str2, "es" => "ES") ==
                astr("tESt", (1:1, :red), (4:4, :blue))
        end

        @testset "Annotation merging" begin
            # Adjacent replacements with same annotations should merge
            str = astr("abc", (1:3, :red))
            result = replace(str, 'a' => astr("A", (1:1, :red)), 'b' => astr("B", (1:1, :red)), 'c' => astr("C", (1:1, :red)))
            @test String(result) == "ABC"
            @test Base.annotations(result) == [(region=1:3, label=:face, value=:red)]

            # Non-adjacent should not merge
            str2 = astr("axbxc", (1:5, :green))
            result2 = replace(str2, 'a' => astr("A", (1:1, :red)), 'c' => astr("C", (1:1, :red)))
            @test String(result2) == "AxbxC"
            # The middle section should be green, ends should be red (not merged)
            @test length(Base.annotations(result2)) >= 2
        end

        @testset "Pattern with itself" begin
            # Replace pattern with itself but different annotations
            @test replace(astr("test", (1:4, :red)), "t" => astr("t", (1:1, :blue))) ==
                astr("test", (2:3, :red), (1:1, :blue), (4:4, :blue))
            # Same length, same content, different annotation
            @test replace(astr("hello", (1:5, :red)), "hello" => astr("hello", (1:5, :blue))) ==
                astr("hello", (1:5, :blue))
        end

        @testset "Many replacements" begin
            # Many occurrences of same pattern
            str = astr("a" * "b"^20, (1:21, :red))
            result = replace(str, "b" => "B")
            @test String(result) == "a" * "B"^20
            @test Base.annotations(result) == [(region=1:1, label=:face, value=:red)]

            # Multiple different patterns
            str2 = astr("ababababab", (1:10, :green))
            result2 = replace(str2, "a" => "A", "b" => "B")
            @test String(result2) == "ABABABABAB"
            @test Base.annotations(result2) == []
        end

        @testset "Annotation spanning replacements" begin
            # Annotation covers entire string with multiple replacements
            @test replace(astr("a-b-c-d", (1:7, :red)), "-" => "_") ==
                astr("a_b_c_d", (1:1, :red), (3:3, :red), (5:5, :red), (7:7, :red))
            # Multiple replacements at different positions
            str = astr("abcdefgh", (1:8, :blue))
            @test replace(str, "b" => "B", "d" => "D", "f" => "F") ==
                astr("aBcDeFgh", (1:1, :blue), (3:3, :blue), (5:5, :blue), (7:8, :blue))
        end

        @testset "edge annotation regions" begin
            # Empty region (0:0) outside string bounds gets filtered out
            str_empty = astr("test", (0:0, :red))
            @test replace(str_empty, "t" => "T") == astr("TesT")

            # Backward range within bounds is preserved (even though it's empty)
            str_backward = astr("test", (3:2, :red))
            @test replace(str_backward, "t" => "T") == astr("TesT", (3:2, :red))

            # Backward range outside bounds gets filtered out
            str_backward_out = astr("test", (5:3, :red))
            @test replace(str_backward_out, "t" => "T") == astr("TesT")
        end
    end

    @testset "IO" begin
        buf = Base.AnnotatedIOBuffer()
        replace(buf, astr("hello", (1:5, :red)), "l" => "L")
        result = read(seekstart(buf), Base.AnnotatedString)
        @test result == astr("heLLo", (1:2, :red), (5:5, :red))

        buf = Base.AnnotatedIOBuffer()
        replace(buf, astr("a", (1:1, :red)), "a" => "x")
        replace(buf, astr("b", (1:1, :blue)), "b" => "y")
        result = read(seekstart(buf), Base.AnnotatedString)
        @test result == astr("xy")

        buf = IOBuffer()
        replace(buf, astr("hello", (1:5, :red)), "l" => "L")
        @test String(take!(buf)) == "heLLo"

        buf = Base.AnnotatedIOBuffer()
        write(buf, "prefix ")
        replace(buf, astr("test", (1:4, :green)), "t" => "T")
        result = read(seekstart(buf), Base.AnnotatedString)
        @test String(result) == "prefix TesT"
        @test Base.annotations(result) == [(region=9:10, label=:face, value=:green)]

        buf = Base.AnnotatedIOBuffer()
        replace(buf, astr("line1", (1:5, :red)), "1" => "A")
        write(buf, "\n")
        replace(buf, astr("line2", (1:5, :blue)), "2" => "B")
        result = read(seekstart(buf), Base.AnnotatedString)
        @test String(result) == "lineA\nlineB"

        buf = Base.AnnotatedIOBuffer()
        replace(buf, astr("test", (1:4, :green)), "t" => "T")
        truncate(buf, 4)
        result = read(seekstart(buf), Base.AnnotatedString)
        @test String(result) == "TesT"

        @testset "Non-appending operations" begin
            # Write, seek back, then replace (matches standard IOBuffer behavior - no truncation)
            buf = Base.AnnotatedIOBuffer()
            write(buf, astr("original", (1:8, :red)))
            seekstart(buf)
            replace(buf, astr("test", (1:4, :blue)), "t" => "T")
            result = read(seekstart(buf), Base.AnnotatedString)
            # Standard IOBuffer doesn't truncate, so we get "TesTinal" not "TesT"
            @test String(result) == "TesTinal"
            # Annotations in the replaced region should be cleared, old ones shifted
            @test Base.annotations(result) == [(region=5:8, label=:face, value=:red),
                                                (region=2:3, label=:face, value=:blue)]

            # Multiple sequential replacements to same buffer (appending)
            buf2 = Base.AnnotatedIOBuffer()
            replace(buf2, astr("first", (1:5, :red)), "i" => "I")
            write(buf2, " ")
            replace(buf2, astr("second", (1:6, :blue)), "e" => "E")
            result2 = read(seekstart(buf2), Base.AnnotatedString)
            @test String(result2) == "fIrst sEcond"
            # Check annotations are present and positioned correctly
            red_annots = filter(a -> a.value == :red, Base.annotations(result2))
            blue_annots = filter(a -> a.value == :blue, Base.annotations(result2))
            @test !isempty(red_annots)
            @test !isempty(blue_annots)

            # Writing at different positions within buffer
            buf3 = Base.AnnotatedIOBuffer()
            write(buf3, "start ")
            pos = position(buf3)
            replace(buf3, astr("middle", (1:6, :green)), "d" => "D")
            write(buf3, " end")
            result3 = read(seekstart(buf3), Base.AnnotatedString)
            @test String(result3) == "start miDDle end"
            # Check that green annotation is offset correctly
            green_annots = filter(a -> a.value == :green, Base.annotations(result3))
            @test all(a -> first(a.region) >= pos + 1, green_annots)
        end
    end
end
