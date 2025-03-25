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
