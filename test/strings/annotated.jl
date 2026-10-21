# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "AnnotatedString" begin
    str = Base.AnnotatedString("some string")
    @test length(str) == 11
    @test ncodeunits(str) == 11
    @test codeunits(str) == codeunits("some string")
    @test codeunit(str) == UInt8
    @test codeunit(str, 1) == codeunit("some string", 1)
    @test firstindex(str) == firstindex("some string")
    @test eltype(str) == Base.AnnotatedChar{eltype(str.string), Any}
    @test eltype(Base.AnnotatedString{String}) == Base.AnnotatedChar{Char}
    @test collect(Base.AnnotatedString("ab", [(1:1, 1)])) isa Vector{Base.AnnotatedChar{Char, Int}}
    @test first(str) == Base.AnnotatedChar(first(str.string), Any[])
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
    Base.annotate!(str, 1:4, 0x01)
    Base.annotate!(str, 6:11, 0x02)
    Base.annotate!(str, 1:11, 0x03)
    #  0x01  0x02
    #  ┌┸─┐ ┌──┸─┐
    # "some string"
    #  └───┰─────┘
    #     0x03
    @test str[3:4] == SubString(str, 3, 4)
    @test str[3:4] != SubString("me")
    @test SubString("me") != str[3:4]
    @test Base.AnnotatedString(str[3:4]) == SubString(str, 3, 4)
    @test repeat(SubString(str, 3, 4), 2) == repeat(Base.AnnotatedString(str[3:4]), 2)
    @test reverse(SubString(str, 3, 4)) == reverse(Base.AnnotatedString(str[3:4]))
    @test Base.AnnotatedString(str[3:4]) ==
        Base.AnnotatedString("me", [(1:2, 0x01), (1:2, 0x03)])
    @test Base.AnnotatedString(str[3:6]) ==
        Base.AnnotatedString("me s", [(1:2, 0x01), (4:4, 0x02), (1:4, 0x03)])
    @test str == Base.AnnotatedString("some string", [(1:4, 0x01), (6:11, 0x02), (1:11, 0x03)])
    @test str != Base.AnnotatedString("some string")
    @test str != Base.AnnotatedString("some string", [(1:1, 0x01), (1:11, 0x03), (6:6, 0x02)])
    @test str != Base.AnnotatedString("some string", [(1:4, 0x11), (1:11, 0x13), (6:11, 0x12)])
    @test str != Base.AnnotatedString("some thingg", [(1:4, 0x01), (1:11, 0x03), (6:11, 0x02)])
    let allstrings =
        ['a', Base.AnnotatedChar('a'), Base.AnnotatedChar('a', [0x04]),
         "a string", Base.AnnotatedString("a string"),
         Base.AnnotatedString("a string", [(1:2, '%')]),
         SubString(Base.AnnotatedString("a string", [(1:2, '%')]), 1:1)]
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
    # Values of different types are kept as they are, under the union of their types
    valued(v) = Base.AnnotatedString("x", [(1:1, v)])
    mixed = Base.annotatedstring(valued(1), valued(2.5))
    @test mixed isa Base.AnnotatedString{String, Union{Int, Float64}}
    @test Base.annotations(mixed) == [(region = 1:1, value = 1), (region = 2:2, value = 2.5)]
    @test valued(true) * valued(1) isa Base.AnnotatedString{String, Union{Bool, Int}}
    @test Base.annotatedstring(valued(1), "y", valued('c'), valued("s")) isa Base.AnnotatedString{String, Union{Int, Char, String}}
    @test (@inferred Base.annotatedstring(valued(1), valued('c'), valued("s"), valued(1.0), valued(:s))) isa Base.AnnotatedString{String, Any}
    @test join([valued(1), valued(2)], valued('c')) isa Base.AnnotatedString{String, Union{Int, Char}}
    @test join([valued(1), valued(2.5)], valued('c')) isa Base.AnnotatedString{String, Any} # The eltype no longer says
    @test replace(valued(1), "x" => valued(2.5)) isa Base.AnnotatedString{String, Union{Int, Float64}}
    # The value type is settled at compile time, also from an eltype that only may be annotated
    @test (@inferred Base.annotatedstring("y", valued(1))) isa Base.AnnotatedString{String, Int}
    @test (@inferred String join(AbstractString["y", valued(1)])) isa Base.AnnotatedString{String, Any}
    @test (@inferred String join(Union{Missing, Base.AnnotatedString{String, Int}}[valued(1), valued(2)])) isa Base.AnnotatedString{String, Int}
    @test join(split(Base.AnnotatedString("ab cd", [(1:2, :x)]))) == Base.AnnotatedString("abcd", [(1:2, :x)])
    # An empty iterator, whose eltype is `Union{}`, joins to a plain `String`
    @test join(()) === ""
    @test join(x for x in ()) === ""
    let sub = Base.AnnotatedString(SubString("abcd", 1, 3), [(1:1, 1)]) # Typed as `repeat` of the wrapped string
        @test repeat(sub, 0) == Base.AnnotatedString("")
        @test (@inferred repeat(sub, 1)) == sub
        @test repeat(sub, 1) isa Base.AnnotatedString{String, Int}
        @test repeat(sub, 1) !== sub # A copy, as an annotated string is mutable
    end
    @test uppercase(Base.AnnotatedString(SubString("abcd", 1, 3), [(1:1, 1)])) isa Base.AnnotatedString{String, Int}
    @test (@inferred join(split(valued(1) * " " * valued(2)))) isa Base.AnnotatedString{String, Int}
    @test collect(SubString(valued(1), 1, 1)) isa Vector{Base.AnnotatedChar{Char, Int}} # As its eltype says
    # A value outside the value type is refused with a clear error
    @test_throws ArgumentError Base.annotate!(valued(1), 1:1, "s")
    @test_throws ArgumentError Base.annotate!(Base.AnnotatedIOBuffer{Int}(), 1:1, "s")
    @test_throws ArgumentError Base.annotate!(Base.AnnotatedChar('c', [1]), "s")
    # @test collect(Base.eachstyle(str)) ==
    #     [("some", [:thing, 0x01, :all, 0x03]),
    #     (" string", [:all, 0x03, :other, 0x02])]
    @test chopprefix(sprint(show, str), "Base.") ==
        "AnnotatedString{String, Any}(\"some string\", [(1:4, 0x01), (6:11, 0x02), (1:11, 0x03)])"
    @test eval(Meta.parse(repr(str))) == str
    @test sprint(show, MIME("text/plain"), str) == "\"some string\""

    a = Base.AnnotatedString("hello", [(1:5, 1)])
    @test first(a) == Base.AnnotatedChar('h', [1])

    @test Bool === Base.infer_return_type(isvalid, Tuple{Base.AnnotatedString, Vararg})
    @test Int === Base.infer_return_type(ncodeunits, Tuple{Base.AnnotatedString})

    @testset "unannotate" begin
        s = "some string"
        str = Base.AnnotatedString(s, [(2:5, 3)])
        @test Base.unannotate(str) === s

        str2 = SubString(str, 2:9)
        u = Base.unannotate(str2)
        @test u isa SubString{String}
        @test u == SubString(s, 2:9)
    end
end

@testset "AnnotatedChar" begin
    chr = Base.AnnotatedChar('c')
    @test uppercase(chr) == Base.AnnotatedChar('C')
    @test titlecase(chr) == Base.AnnotatedChar('C')
    @test lowercase(Base.AnnotatedChar('C')) == chr
    # A char in another case has its own annotations
    for transform in (lowercase, uppercase, titlecase)
        original = Base.AnnotatedChar('a', [1])
        Base.annotate!(transform(original), 2)
        @test Base.annotations(original) == [1]
    end
    str = Base.AnnotatedString("hmm", [(1:1, "h0h0"),
                               (1:2, "h0m1"),
                               (2:3, "m1m2")])
    @test str[1] == Base.AnnotatedChar('h', ["h0h0"])
    @test str[2] == Base.AnnotatedChar('m', ["h0m1", "m1m2"])
    @test str[3] == Base.AnnotatedChar('m', ["m1m2"])
    chr = Base.annotate!(Base.AnnotatedChar('m'), "h0m1")
    @test chr == Base.AnnotatedChar('m', ["h0m1"])
    @test Base.annotate!(chr, "m1m2") == Base.AnnotatedChar('m', ["h0m1", "m1m2"])
end

@testset "Unstyled display fallback" begin
    # In a fresh process, so that StyledStrings' display methods can't mask the fallback
    script = """
        s = SubString(Base.AnnotatedString("αb", [(1:3, :red)]), 3, 3)
        c = Base.AnnotatedChar('γ', [:red])
        print(any(m -> nameof(m) === :StyledStrings, Base.loaded_modules_array()), ' ',
              String(s), sprint(print, c))
        """
    @test readchomp(`$(Base.julia_cmd()) --startup-file=no -e $script`) == "false bγ"
end

# A package's value type, which it claims a label for
struct Weight
    w::Int
end
Base.labelled_kind(::Val{:weight}) = Weight

@testset "Labelled annotations" begin
    # Deleting by label removes the annotations of its kind over exactly the region: a type of value, or a key
    let s = Base.AnnotatedString{String, Any}("abc", [(1:3, :size => 2), (1:3, Weight(1)), (1:2, :size => 3), (1:3, :font => 4)])
        Base.annotate!(s, 1:3, :size, nothing)
        @test Base.annotations(s) == [(region = 1:3, value = Weight(1)), (region = 1:2, value = :size => 3), (region = 1:3, value = :font => 4)]
        Base.annotate!(s, 1:3, :weight, nothing)
        @test Base.annotations(s) == [(region = 1:2, value = :size => 3), (region = 1:3, value = :font => 4)]
    end
    # Other labelled forms are deprecated: a value of the label's kind is kept, and any other becomes `label => value`
    @test_deprecated Base.AnnotatedString("ab", [(1:1, :size, 2)])
    @test_deprecated Base.AnnotatedChar('a', [(:size, 2)])
    @test_deprecated Base.annotate!(Base.AnnotatedString{String, Any}("ab"), 1:1, :size, 2)
    @test_deprecated Base.annotate!(Base.AnnotatedChar{Char, Any}('a'), :size, 2)
    @test_deprecated Base.AnnotatedString("ab", Any[(1:1, :size, 2), (2:2, 3)]) # In a vector of any values
    # A string's values that only look labelled are kept as they are, by its chars too
    let s = Base.AnnotatedString("ab", [(1:1, (:a, 1))])
        @test s[1] == Base.AnnotatedChar{Char, Tuple{Symbol, Int}}('a', [(:a, 1)])
        @test collect(s) isa Vector{Base.AnnotatedChar{Char, Tuple{Symbol, Int}}}
        @test Base.annotations(uppercase(s[1])) == [(:a, 1)]
    end
    @test Base.annotations(Base.AnnotatedChar('a', Any[(:weight, Weight(1))])) == [(:weight, Weight(1))] # Among any values, a value
    labelled = """
    struct Weight; w::Int end
    Base.labelled_kind(::Val{:weight}) = Weight
    Base.labelled_value(::Val{:heavy}, n::Int) = Weight(n) # A package's own upgrade of an old form
    size, w = Pair{Symbol, Any}(:size, 2), Weight(3)
    s = Base.AnnotatedString("abc", [(1:1, :size, 2), (2:2, :weight, w), (3:3, :weight, 4)])
    c = Base.AnnotatedChar('a', [(:size, 2), (:weight, w)])
    print(all((Base.annotations(s) == [(region = 1:1, value = size), (region = 2:2, value = w), (region = 3:3, value = :weight => 4)],
               Base.annotations(Base.AnnotatedString("a", [(region = 1:1, label = :weight, value = w)])) == [(region = 1:1, value = w)],
               Base.annotations(c) == [size, w],
               Base.annotations(Base.AnnotatedChar('a', [(label = :weight, value = w)])) == [w],
               Base.annotate!(Base.AnnotatedString{String, Any}("ab"), 1:1, :size, 2) == Base.AnnotatedString("ab", [(1:1, size)]),
               Base.annotate!(Base.AnnotatedChar{Char, Any}('a'), :weight, w) == Base.AnnotatedChar('a', [w]),
               Base.annotations(Base.AnnotatedString("a", [(1:1, :heavy, 5)])) == [(region = 1:1, value = Weight(5))],
               Base.annotations(Base.AnnotatedString("ab", Any[(1:1, :size, 2), (2:2, 3)])) == [(region = 1:1, value = size), (region = 2:2, value = 3)])))
    """
    @test read(`$(Base.julia_cmd()) --startup-file=no --depwarn=no -e $labelled`, String) == "true"
end

@testset "Construction" begin
    # Strings and chars follow the same rules; char annotations just lack a region
    for (Annotated, plain, annotof) in ((Base.AnnotatedString, "ab", v -> (1:1, v)),
                                        (Base.AnnotatedChar, 'a', identity))
        Plain, Abstract = typeof(plain), supertype(typeof(plain))
        loose = [annotof(1)]
        ints = Annotated(plain, loose).annotations
        for T in (Annotated, Annotated{Plain}, Annotated{Abstract}, Annotated{Plain, Int}, Annotated{Abstract, Int}),
            annots in (ints, loose)
            @test T(plain, annots) isa T
            @test T(plain, annots) isa Annotated{<:Any, Int}
        end
        @test Annotated{Abstract, Int}(plain) isa Annotated{Abstract, Int}
        @test Annotated{Plain}(plain, [annotof(1), annotof('c')]) isa Annotated{Plain, Any}
        @test Annotated(plain, ints).annotations === ints
        @test Annotated{Plain, Int}(plain, ints).annotations === ints
        @test Annotated{Plain, Any}(plain, ints).annotations !== ints
        annotated = Annotated(plain, ints)
        for copied in (Annotated(annotated), typeof(annotated)(annotated), Annotated{Plain, Any}(annotated))
            @test copied == annotated
            @test copied.annotations !== annotated.annotations
        end
        @test typeof(Annotated(annotated)) == typeof(annotated)
        @test convert(Annotated, annotated) === annotated
        @test convert(typeof(annotated), annotated) === annotated
        # Adding annotations widens the value type, as `vcat` does
        for added in ([annotof(:s)], Annotated(plain, [annotof(:s)]).annotations)
            @test Annotated(annotated, added) isa Annotated{Plain, Union{Int, Symbol}}
        end
    end
    @test Base.AnnotatedString("ab") == Base.AnnotatedString("ab", Base.RegionAnnotation[])
    @test Base.AnnotatedChar(UInt32('c')) == Base.AnnotatedChar('c') == Base.AnnotatedChar('c', Any[])
    @test Base.AnnotatedString("ab", [1:1 => 1, (region = 2:2, value = 2)]) == Base.AnnotatedString("ab", [(1:1, 1), (2:2, 2)])
    @test_throws ArgumentError Base.AnnotatedString("ab", Any[(1:1, 2, 3)]) # Not read as a region and a value
    @test Base.AnnotatedString([Base.AnnotatedChar('a', [1]), Base.AnnotatedChar('b', [2])]) ==
        Base.AnnotatedString("ab", [(1:1, 1), (2:2, 2)])
    @test (@inferred Base.AnnotatedString(Base.AnnotatedString("x", [(1:1, 1)]), [(1:1, 2)])) isa Base.AnnotatedString{String, Int}
    @test (@inferred Base.AnnotatedChar(Base.AnnotatedChar('x', [1]), [2])) isa Base.AnnotatedChar{Char, Int}
    @test Base.AnnotatedChar(Base.AnnotatedChar('m', ["h0m1"]), ["m2m3"]) ==
        Base.AnnotatedChar('m', ["h0m1", "m2m3"])
end

@testset "Styling preservation" begin
    str = Base.AnnotatedString("some string", [(1:4, 0x01), (1:11, 0x03), (6:11, 0x02)])
    @test match(r".e", str).match == str[3:4]
    @test  match(r"(.e)", str).captures == [str[3:4]]
    let m0 = match(r"(.)e", str)
        m1 = first(eachmatch(r"(.)e", str))
        for f in fieldnames(RegexMatch)
            @test getfield(m0, f) == getfield(m1, f)
        end
    end
    @test lpad(str, 12) ==
        Base.AnnotatedString(" some string", [(2:5, 0x01),
                                      (2:12, 0x03),
                                      (7:12, 0x02)])
    @test rpad(str, 12) ==
        Base.AnnotatedString("some string ", [(1:4, 0x01),
                                      (1:11, 0x03),
                                      (6:11, 0x02)])
    str1 = Base.AnnotatedString("test", [(1:4, 5)])
    str2 = Base.AnnotatedString("case", [(2:3, "oomph")])
    @test join([str1, str1], ' ') ==
        Base.AnnotatedString("test test",
                     [(1:4, 5),
                      (6:9, 5)])
    @test join([str1, str1], Base.AnnotatedString(" ", [(1:1, 2)])) ==
        Base.AnnotatedString("test test",
                     [(1:4, 5),
                      (5:5, 2),
                      (6:9, 5)])
    @test join((String(str1), str1), ' ') ==
        Base.AnnotatedString("test test", [(6:9, 5)])
    @test join(("test", Base.AnnotatedString("case")), ' ') isa Base.AnnotatedString{String}
    @test repeat(str1, 2) == Base.AnnotatedString("testtest", [(1:8, 5)])
    @test repeat(str2, 2) == Base.AnnotatedString("casecase", [(2:3, "oomph"),
                                                       (6:7, "oomph")])
    @test repeat(str1[1], 3) == Base.AnnotatedString("ttt", [(1:3, 5)])
    @test reverse(str1) == Base.AnnotatedString("tset", [(1:4, 5)])
    @test reverse(str2) == Base.AnnotatedString("esac", [(2:3, "oomph")])
end

@testset "Unicode" begin
    for words in (["ᲃase", "cɦɒnɡeȿ", "can", "CHⱯNGE", "Сodeunıts"],
                  ["Сodeunıts", "ᲃase", "cɦɒnɡeȿ", "can", "CHⱯNGE"])
        ann_words = [Base.AnnotatedString(w, [(1:ncodeunits(w), i)])
                     for (i, w) in enumerate(words)]
        ann_str = join(ann_words, '-')
        for transform in (lowercase, uppercase, titlecase)
            t_words = map(transform, words)
            ann_t_words = [Base.AnnotatedString(w, [(1:ncodeunits(w), i)])
                        for (i, w) in enumerate(t_words)]
            ann_t_str = join(ann_t_words, '-')
            t_ann_str = transform(ann_str)
            @test String(ann_t_str) == String(t_ann_str)
            @test Base.annotations(ann_t_str) == Base.annotations(t_ann_str)
        end
        for transform in (uppercasefirst, lowercasefirst)
            t_words = vcat(transform(first(words)), words[2:end])
            ann_t_words = [Base.AnnotatedString(w, [(1:ncodeunits(w), i)])
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
    vec2ann(v::Vector{Tuple{UnitRange{Int}, V}}) where {V} = collect(Base.RegionAnnotation{V}, v)
    # Append-only writing
    @test write(aio, Base.AnnotatedString("hello", [(1:5, 1)])) == 5
    @test write(aio, ' ') == 1
    @test write(aio, Base.AnnotatedString("world", [(1:5, 2)])) == 5
    @test Base.annotations(aio) == vec2ann([(1:5, 1), (7:11, 2)])
    # Printing through a context keeps the annotations
    ctxaio = Base.AnnotatedIOBuffer()
    print(IOContext(ctxaio, :color => true), Base.AnnotatedString("hi", [(1:2, 3)]))
    print(IOContext(ctxaio), Base.AnnotatedChar('!', [4]))
    @test Base.annotations(ctxaio) == vec2ann([(1:2, 3), (3:3, 4)])
    # Check `annotate!`, including region sorting
    @test truncate(aio, 0).io.size == 0
    @test write(aio, "hello world") == ncodeunits("hello world")
    @test Base.annotate!(aio, 1:5, 1) === aio
    @test Base.annotate!(aio, 7:11, 2) === aio
    @test Base.annotations(aio) == vec2ann([(1:5, 1), (7:11, 2)])
    # Reading
    @test read(seekstart(deepcopy(aio.io)), String) == "hello world"
    @test read(seekstart(deepcopy(aio)), String) == "hello world"
    @test read(seek(aio, 0), Base.AnnotatedString) == Base.AnnotatedString("hello world", [(1:5, 1), (7:11, 2)])
    @test read(seek(aio, 1), Base.AnnotatedString) == Base.AnnotatedString("ello world", [(1:4, 1), (6:10, 2)])
    @test read(seek(aio, 4), Base.AnnotatedString) == Base.AnnotatedString("o world", [(1:1, 1), (3:7, 2)])
    @test read(seek(aio, 5), Base.AnnotatedString) == Base.AnnotatedString(" world", [(2:6, 2)])
    @test read(seekend(aio), Base.AnnotatedString) == Base.AnnotatedString("")
    @test read(seekstart(truncate(deepcopy(aio), 5)), Base.AnnotatedString) == Base.AnnotatedString("hello", [(1:5, 1)])
    @test read(seekstart(truncate(deepcopy(aio), 6)), Base.AnnotatedString) == Base.AnnotatedString("hello ", [(1:5, 1)])
    @test read(seekstart(truncate(deepcopy(aio), 7)), Base.AnnotatedString) == Base.AnnotatedString("hello w", [(1:5, 1), (7:7, 2)])
    @test read(seek(aio, 0), Base.AnnotatedChar) == Base.AnnotatedChar('h', [1])
    @test read(seek(aio, 5), Base.AnnotatedChar) == Base.AnnotatedChar(' ', [])
    @test read(seek(aio, 6), Base.AnnotatedChar) == Base.AnnotatedChar('w', [2])
    # Check method compatibility with IOBuffer
    @test position(aio) == 7
    @test seek(aio, 4) === aio
    @test skip(aio, 2) === aio
    @test Base.annotations(copy(aio)) == Base.annotations(aio)
    @test take!(copy(aio).io) == take!(copy(aio.io))
    # Writing into the middle of the buffer
    @test write(seek(aio, 6), "alice") == 5 # Replace 'world' with 'alice'
    @test read(seekstart(aio), String) == "hello alice"
    @test Base.annotations(aio) == vec2ann([(1:5, 1), (7:11, 2)]) # Should be unchanged
    @test write(seek(aio, 0), Base.AnnotatedString("hey-o", [(1:5, 'o')])) == 5
    @test read(seekstart(aio), String) == "hey-o alice"
    @test Base.annotations(aio) == vec2ann([(7:11, 2), (1:5, 'o')]) # First annotation should have been entirely replaced
    @test write(seek(aio, 7), Base.AnnotatedString("bbi", [(1:3, 'a')])) == 3 # a[lic, bbi]e ('alice', 'abbie')
    @test read(seekstart(aio), String) == "hey-o abbie"
    @test Base.annotations(aio) == vec2ann([(7:7, 2), (11:11, 2), (1:5, 'o'), (8:10, 'a')])
    @test write(seek(aio, 0), Base.AnnotatedString("ab")) == 2 # Check first annotation's region is adjusted correctly
    @test read(seekstart(aio), String) == "aby-o abbie"
    @test Base.annotations(aio) == vec2ann([(7:7, 2), (11:11, 2), (3:5, 'o'), (8:10, 'a')])
    @test write(seek(aio, 3), Base.AnnotatedString("ss")) == 2
    @test read(seekstart(aio), String) == "abyss abbie"
    @test Base.annotations(aio) == vec2ann([(7:7, 2), (11:11, 2), (3:3, 'o'), (8:10, 'a')])
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
        sort(vcat(Base.annotations(aio), vec2ann([(13:13, 2), (14:16, 'a'), (17:17, 2)])))
    # The `_insert_annotations!` cautious-merging optimisation
    aio = Base.AnnotatedIOBuffer()
    @test write(aio, Base.AnnotatedChar('a', [1, 2])) == 1
    @test Base.annotations(aio) == vec2ann([(1:1, 1), (1:1, 2)])
    @test write(aio, Base.AnnotatedChar('b', [1, 2])) == 1
    @test Base.annotations(aio) == vec2ann([(1:2, 1), (1:2, 2)])
    let aio2 = copy(aio) # A different start makes merging too risky to do.
        @test write(aio2, Base.AnnotatedChar('c', [0, 2])) == 1
        @test Base.annotations(aio2) == vec2ann([(1:2, 1), (1:2, 2), (3:3, 0), (3:3, 2)])
    end
    let aio2 = copy(aio) # Merging some run of the most recent annotations is fine though.
        @test write(aio2, Base.AnnotatedChar('c', [2])) == 1
        @test Base.annotations(aio2) == vec2ann([(1:2, 1), (1:3, 2)])
    end
    let aio2 = copy(aio) # ...and any subsequent annotations after a matching run can just be copied over.
        @test write(aio2, Base.AnnotatedChar('c', [2, 3, 4])) == 1
        @test Base.annotations(aio2) == vec2ann([(1:2, 1), (1:3, 2), (3:3, 3), (3:3, 4)])
    end
    let aio2 = Base.AnnotatedIOBuffer()
        @test write(aio2, Base.AnnotatedChar('a', [2])) == 1
        @test write(aio2, Base.AnnotatedChar('b', [1, 2])) == 1
        @test read(seekstart(aio2), Base.AnnotatedString) ==
            Base.AnnotatedString("ab", [(1:1, 2), (2:2, 1), (2:2, 2)])
    end
    let aio2 = Base.AnnotatedIOBuffer() # Equal but distinct values are not merged
        write(aio2, Base.AnnotatedString("a", [(1:1, [1])]))
        write(aio2, Base.AnnotatedString("b", [(1:1, [1])]))
        @test length(Base.annotations(aio2)) == 2
    end
    let ints = Base.AnnotatedIOBuffer{Int}() # Between value types
        write(ints, Base.AnnotatedString("ab", [(1:1, 1)]))
        anys = Base.AnnotatedIOBuffer()
        write(anys, seekstart(ints))
        @test Base.annotations(anys) == vec2ann([(1:1, 1)])
        @test read(seekstart(anys), Base.AnnotatedString{String, Int}) isa Base.AnnotatedString{String, Int}
        write(anys, Base.AnnotatedString("c", [(1:1, "c")]))
        @test_throws MethodError read(seekstart(anys), Base.AnnotatedString{String, Int})
    end
    let ints = Base.AnnotatedIOBuffer{Int}() # Chars are read with the buffer's value type, or a given one
        write(ints, Base.AnnotatedString("ab", [(1:1, 1)]))
        @test read(seekstart(ints), Base.AnnotatedChar) isa Base.AnnotatedChar{Char, Int}
        @test read(seekstart(ints), Base.AnnotatedChar{Char, Any}) == Base.AnnotatedChar{Char, Any}('a', [1])
    end
    let nested = Base.AnnotatedString(SubString("hello", 1, 3), [(1:1, 1)]) # Over a `SubString`
        aio2 = Base.AnnotatedIOBuffer()
        @test write(aio2, nested[1:2]) == 2
        @test read(seekstart(aio2), Base.AnnotatedString) == Base.AnnotatedString("he", [(1:1, 1)])
    end
    # Working through an IOContext
    aio = Base.AnnotatedIOBuffer()
    wrapio = IOContext(aio)
    @test write(wrapio, Base.AnnotatedString("hey", [(1:3, 1)])) == 3
    @test write(wrapio, Base.AnnotatedChar('a', [2])) == 1
    @test read(seekstart(aio), Base.AnnotatedString) ==
        Base.AnnotatedString("heya", [(1:3, 1), (4:4, 2)])
    # show-ing an AnnotatedIOBuffer
    aio = Base.AnnotatedIOBuffer()
    write(aio, Base.AnnotatedString("hello", [(1:5, 1)]))
    @test chopprefix(sprint(show, aio), "Base.") == "AnnotatedIOBuffer(5 bytes, 1 annotation)"
    # A value that doesn't fit the destination throws before any text is written
    let src = Base.AnnotatedIOBuffer(), dest = Base.AnnotatedIOBuffer{Int}()
        write(src, Base.AnnotatedString("ab", [(1:1, "s")]))
        @test_throws MethodError write(dest, seekstart(src))
        @test position(dest) == 0
    end
end

@testset "Eachregion" begin
    annregions(str::String, annots::Vector{<:Tuple{UnitRange{Int}, <:Any}}) =
        [(s, collect(a)) for (s, a) in Base.eachregion(Base.AnnotatedString(str, annots))]
    # Regions that do/don't extend to the left/right edges
    @test annregions(" abc ", [(2:4, :bold)]) ==
        [(" ", []),
         ("abc", [:bold]),
         (" ", [])]
    @test annregions(" x ", [(2:2, :bold)]) ==
        [(" ", []),
         ("x", [:bold]),
         (" ", [])]
    @test annregions(" x", [(2:2, :bold)]) ==
        [(" ", []),
         ("x", [:bold])]
    @test annregions("x ", [(1:1, :bold)]) ==
        [("x", [:bold]),
         (" ", [])]
    @test annregions("x", [(1:1, :bold)]) ==
        [("x", [:bold])]
    # Overlapping/nested regions
    @test annregions(" abc ", [(2:4, :bold), (3:3, :italic)]) ==
        [(" ", []),
         ("a", [:bold]),
         ("b", [:bold, :italic]),
         ("c", [:bold]),
         (" ", [])]
    @test annregions("abc-xyz", [(1:7, :bold), (1:3, :green), (4:4, :yellow), (4:7, :italic)]) ==
        [("abc", [:bold, :green]),
         ("-", [:bold, :yellow, :italic]),
         ("xyz", [:bold, :italic])]
    # Preserving annotation order
    @test annregions("abcd", [(1:3, :red), (2:2, :yellow), (2:3, :green), (2:4, :blue)]) ==
        [("a", [:red]),
         ("b", [:red, :yellow, :green, :blue]),
         ("c", [:red, :green, :blue]),
         ("d", [:blue])]
    @test annregions("abcd", [(2:4, :blue), (1:3, :red), (2:3, :green), (2:2, :yellow)]) ==
        [("a", [:red]),
         ("b", [:blue, :red, :green, :yellow]),
         ("c", [:blue, :red, :green]),
         ("d", [:blue])]
    # Region starting after a character spanning multiple codepoints.
    @test annregions("𝟏x", [(1:4, :red)]) ==
        [("𝟏", [:red]),
         ("x", [])]
    # Annotations and subregions that start within a character
    let str = Base.AnnotatedString("aébc", [(3:3, :red)])
        regions = collect(Base.eachregion(str)) # Read after the iterator has moved on
        @test [(s, collect(a)) for (s, a) in regions] == [("a", []), ("é", [:red]), ("bc", [])]
        @test [s for (s, _) in Base.eachregion(str, 3:5)] == ["é", "bc"]
    end
    # A subregion starting within a character, whose regions read the same during iteration and after
    let str = Base.AnnotatedString("aébcdef", [(1:8, 1), (3:5, 2), (5:5, 3), (2:2, 4)])
        @test [(String(s), collect(a)) for (s, a) in Base.eachregion(str, 3:5)] ==
            [(String(s), collect(a)) for (s, a) in collect(Base.eachregion(str, 3:5))]
    end
    # Read after the iterator has left them, past an annotation beyond the end of the string
    @test [collect(a) for (_, a) in collect(Base.eachregion(Base.AnnotatedString("ab", [(1:1, 1), (5:6, 2)])))] == [[1], []]
    # Passes over one iterator at the same time don't share their state
    let regions = Base.eachregion(Base.AnnotatedString("abcd", [(1:2, 1), (2:3, 2)]))
        pass = [(String(s), collect(a)) for (s, a) in regions]
        @test [(String(s), collect(a)) for ((s, a), _) in zip(regions, regions)] == pass
        @test [(String(s), collect(a)) for (s, a) in regions for _ in regions] == repeat(pass, inner = length(pass))
    end
    # A string backed by a substring, whose regions are substrings of the same parent
    @test length(collect(Base.eachregion(Base.AnnotatedString(SubString("xab", 2:3), [(1:1, 1)])))) == 2
    # A region's values of a union type are iterated without boxing, whether the iterator is on it or has left it
    let regions = collect(Base.eachregion(Base.AnnotatedString{String, Union{Int, Symbol}}("ab", [(1:2, 1), (1:1, :x)])))
        ints(annots) = (n = 0; for value in annots; value isa Int && (n += 1) end; n)
        @test ints(regions[1][2]) == 1 && ints(regions[2][2]) == 1
        @test (@allocated ints(regions[1][2])) == (@allocated ints(regions[2][2])) == 0
    end
    # Only the annotations with values of a given type, which alone mark out the regions
    let str = Base.AnnotatedString{String, Union{Int, Symbol}}("abcd", [(1:3, 1), (2:2, :x), (3:4, 2)])
        @test [(String(s), collect(a)) for (s, a) in Base.eachregion(str, 1:4, Int)] == [("ab", [1]), ("c", [1, 2]), ("d", [2])]
        @test all(((_, a),) -> collect(a) isa Vector{Int}, Base.eachregion(str, 1:4, Int))
        @test [String(s) for (s, _) in Base.eachregion(SubString(str, 2, 4), 1:3, Int)] == ["b", "c", "d"]
        @test collect(only(Base.eachregion(str, 1:4, Float64))[2]) == Float64[]
        @test Base.eachregion(str, 1:4, Any).annotations === str.annotations # A wider type reads them in place
    end
    # Changing a string while its regions are read is unsupported, but memory safe
    let str = Base.AnnotatedString("abcdef", [(1:2, 1), (3:4, 2), (5:6, 3)])
        @test count(_ -> (Base.annotate!(str, 1:6, 9); true), Base.eachregion(str)) == 3
        regions = collect(Base.eachregion(str))
        empty!(str.annotations)
        @test_throws BoundsError collect(last(regions)[2])
    end
end

@testset "Replacement" begin
    astr(s::String, faceregions::Tuple{UnitRange{Int}, Symbol}...) =
        Base.AnnotatedString(s, [(r, f) for (r, f) in faceregions])

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
            @test replace(astr("hello", (1:5, :red)), 'l' => Base.AnnotatedChar('L', [:blue])) ==
                astr("heLLo", (1:2, :red), (5:5, :red), (3:4, :blue))
            @test replace(astr("abc", (1:3, :green)), 'b' => Base.AnnotatedChar('B', [:bold])) ==
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
            @test replace(astr("test", (1:4, :red)), 't' => Base.AnnotatedChar('T', [:blue])) ==
                astr("TesT", (2:3, :red), (1:1, :blue), (4:4, :blue))
            @test replace(astr("hello"), 'l' => Base.AnnotatedChar('L', [:bold, :red])) ==
                astr("heLLo", (3:4, :bold), (3:4, :red))
            @test replace(astr("a b c", (1:5, :green)), ' ' => Base.AnnotatedChar('_', [:underline])) ==
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
            # Same region with different values
            str = astr("test", (1:4, :red), (1:4, :bold), (2:3, :italic))
            @test replace(str, "es" => "ES") ==
                astr("tESt", (1:1, :red), (4:4, :red),
                     (1:1, :bold), (4:4, :bold))
            # Different values on different regions
            str2 = astr("test", (1:2, :red), (3:4, :blue))
            @test replace(str2, "es" => "ES") ==
                astr("tESt", (1:1, :red), (4:4, :blue))
        end

        @testset "Annotation merging" begin
            # Adjacent replacements with same annotations should merge
            str = astr("abc", (1:3, :red))
            result = replace(str, 'a' => astr("A", (1:1, :red)), 'b' => astr("B", (1:1, :red)), 'c' => astr("C", (1:1, :red)))
            @test String(result) == "ABC"
            @test Base.annotations(result) == [(region=1:3, value=:red)]

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
            @test Base.annotations(result) == [(region=1:1, value=:red)]

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
        @test Base.annotations(result) == [(region=9:10, value=:green)]

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
            @test Base.annotations(result) == [(region=5:8, value=:red),
                                                (region=2:3, value=:blue)]

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

        @testset "Typed buffers" begin
            wide = Base.AnnotatedString{String, Any}("Q", [(1:1, 1)])
            anybuf = Base.AnnotatedIOBuffer()
            replace(anybuf, astr("apple", (1:5, :red)), "p" => wide)
            typedbuf = Base.AnnotatedIOBuffer{Union{Symbol, Int}}()
            replace(typedbuf, astr("apple", (1:5, :red)), "p" => wide)
            @test read(seekstart(typedbuf), Base.AnnotatedString) == read(seekstart(anybuf), Base.AnnotatedString)
            @test_throws MethodError replace(Base.AnnotatedIOBuffer{Symbol}(), astr("apple", (1:5, :red)), "p" => wide)
        end
    end
end

# A package with its own annotation value types and how they display
module MarkedAnnotations
    import Base.AnnotatedDisplay: reduction, transition
    struct Mark end
    struct Tag # Shown in plain text only
        name::String
    end
    # The states: whether a region is marked, and the tag it is in, if any
    struct Marked
        on::Bool
    end
    struct Tagged
        name::String
    end
    reduction(::MIME"text/plain", ::Type{Mark}, values) = Marked(any(value -> value isa Mark, values))
    reduction(::MIME"text/html", ::Type{Mark}, values) = Marked(any(value -> value isa Mark, values))
    transition(io::IO, ::MIME"text/plain", from::Marked, to::Marked) = from === to || print(io, if to.on "<" else ">" end)
    transition(io::IO, ::MIME"text/html", ::Marked, ::Marked) = print(io, "<mark>")
    transition(io::IO, ::MIME"text/html", ::Marked, ::Nothing) = print(io, "</mark>")
    reduction(::MIME"text/plain", ::Type{Tag}, values) =
        Tagged(foldl((name, value) -> if value isa Tag value.name else name end, values; init = ""))
    function transition(io::IO, ::MIME"text/plain", from::Tagged, to::Tagged)
        from === to && return
        isempty(from.name) || print(io, "]")
        isempty(to.name) || print(io, "[", to.name, " ")
    end
    # A state with no fields: whether a region is flagged
    struct Flag end
    reduction(::MIME"text/html", ::Type{Flag}, values) = any(value -> value isa Flag, values)
    transition(io::IO, ::MIME"text/html", ::Bool, flagged::Bool) = flagged && print(io, "<b>")
    transition(io::IO, ::MIME"text/html", ::Bool, ::Nothing) = print(io, "</b>")
    # Displayed by each concrete type, whatever the value type that holds them
    abstract type Shade end
    struct Dark <: Shade end
    reduction(::MIME"text/plain", ::Type{Dark}, values) = Marked(any(value -> value isa Dark, values))
    struct Level{N} end # Each of its types is a level of brackets, counted by its own reduction
    struct Depth
        n::Int
    end
    reduction(::MIME"text/plain", ::Type{T}, values) where {T <: Level} = Depth(count(value -> value isa T, values))
    transition(io::IO, ::MIME"text/plain", from::Depth, to::Depth) =
        print(io, "["^max(0, to.n - from.n), "]"^max(0, from.n - to.n))
    # Types whose states break the interface: with no baseline state, or of another type
    struct Patchy end
    reduction(::MIME, ::Type{Patchy}, values) = if any(value -> value isa Patchy, values) Marked(true) end
    struct Fickle end
    reduction(::MIME, ::Type{Fickle}, values) = if any(value -> value isa Fickle, values) Tagged("f") else Marked(false) end
end

@testset "AnnotatedDisplay" begin
    (; Mark, Tag, Marked) = MarkedAnnotations
    colour(x) = sprint(print, x; context = :color => true)
    html(x) = sprint(show, MIME("text/html"), x)
    marked(V) = Base.AnnotatedString{String, V}("xy", [(1:1, Mark())])
    # A type's display is written between the regions, and only to a terminal
    @test colour(marked(Mark)) == "<x>y"
    @test sprint(print, marked(Mark)) == "xy"
    @test colour(Base.AnnotatedString{String, Int}("x", [(1:1, 1)])) == "x" # Not displayed
    # A string displays the same whatever union of value types holds it
    tagged(V) = Base.AnnotatedString{String, V}("abc", [(1:2, Mark()), (2:3, Tag("t")), (3:3, 1)])
    @test colour(tagged(Union{Mark, Tag, Int})) == "<a[t b>c]"
    @test colour(tagged(Union{Mark, Tag, Int, Symbol})) == colour(tagged(Any)) == colour(tagged(Union{Mark, Tag, Int}))
    @test colour(Base.AnnotatedString{String, Any}("x")) == "x"
    # Each type of value displays its own values, so a value type with abstract members displays as `Any` does
    (; Dark, Level, Flag) = MarkedAnnotations
    shaded(V) = colour(Base.AnnotatedString{String, V}("xy", [(1:1, Dark())]))
    @test shaded(Dark) == shaded(MarkedAnnotations.Shade) == shaded(Any) == "<x>y"
    levelled(V) = colour(Base.AnnotatedString{String, V}("ab", [(1:1, Level{1}()), (1:1, Level{2}())]))
    @test levelled(Union{Level{1}, Level{2}}) == levelled(Level) == levelled(Any) == "[[a]]b"
    # Substrings and chars, as the strings made from them
    @test colour(SubString(tagged(Any), 2, 3)) == colour(Base.AnnotatedString(SubString(tagged(Any), 2, 3)))
    @test colour(marked(Mark)[1]) == "<x>"
    # The text of HTML is always escaped
    @test html(marked(Mark)) == "<mark>x</mark>y"
    @test html(SubString(marked(Any), 1, 1)) == html(marked(Mark)[1]) == "<mark>x</mark>"
    plain = Base.AnnotatedString{String, Union{Tag, Int}}("a<b", [(1:1, Tag("t"))])
    @test html(plain) == "a&lt;b"
    # A state with no fields closes its element when it changes
    flagged(V) = html(Base.AnnotatedString{String, V}("abcde", [(2:2, Flag()), (4:5, Flag())]))
    @test flagged(Flag) == flagged(Any) == "a<b>b</b>c<b>de</b>"
    # Escaping keeps the display around the (longer) text, and an annotated buffer the annotations
    @test sprint(escape_string, marked(Mark); context = :color => true) == "<x>y"
    @test sprint(escape_string, Base.AnnotatedString{String, Mark}("a\nb", [(1:1, Mark()), (3:3, Mark())]);
                 context = :color => true) == "<a>\\n<b>"
    escaped(s; wrap = identity) = (buf = Base.AnnotatedIOBuffer(); escape_string(wrap(buf), s); read(seekstart(buf), Base.AnnotatedString))
    @test escaped(Base.AnnotatedString("a\nb\tc", [(1:1, 1), (3:3, 2), (2:5, 3)])) == Base.AnnotatedString("a\\nb\\tc", [(1:1, 1), (4:4, 2), (2:7, 3)])
    @test escaped(SubString(Base.AnnotatedString("a\nbc", [(1:3, 1)]), 2, 4)) == Base.AnnotatedString("\\nbc", [(1:3, 1)])
    @test escaped(Base.AnnotatedString("x\ty", [(2:2, 1)]); wrap = io -> IOContext(io, :color => true)) == Base.AnnotatedString("x\\ty", [(2:3, 1)])
    # Regions that start or stop inside a character cover the whole character
    @test escaped(Base.AnnotatedString("aé\nb", [(2:2, 1), (3:4, 2)])) == Base.AnnotatedString("aé\\nb", [(2:3, 1), (2:5, 2)])
    let buf = Base.AnnotatedIOBuffer() # Overwriting replaces the annotations, up to the end too
        write(buf, Base.AnnotatedString("abcdef", [(1:6, 1)]))
        seek(buf, 2)
        escape_string(buf, Base.AnnotatedString("XY", [(1:2, 2)]))
        seek(buf, 4)
        escape_string(buf, Base.AnnotatedString("Z\n", [(1:2, 3)]))
        @test read(seekstart(buf), Base.AnnotatedString) == Base.AnnotatedString("abXYZ\\n", [(1:2, 1), (3:4, 2), (5:7, 3)])
    end
    # Each region escapes as it does within the whole text, which may depend on the next character
    marks(text, region) = Base.AnnotatedString{String, Mark}(text, [(region, Mark())])
    @test String(escaped(Base.AnnotatedString("a\0" * "1", [(2:2, 1)]))) == "a\\x001"
    @test sprint(escape_string, marks("a\0" * "1", 2:2); context = :color => true) == "a<\\x00>1"
    @test sprint((io, s) -> escape_string(io, s; ascii = true), marks("é1", 1:2); context = :color => true) == "<\\u00e9>1"
    @test sprint(escape_string, marks("\0" * "7", 1:1), "7"; context = :color => true) == "<\\x00>\\7"
    @test sprint(escape_string, SubString(marks("a\0" * "1", 2:2), 1, 2); context = :color => true) == "a<\\0>"
    let buf = Base.AnnotatedIOBuffer{Float64}() # Values are converted to the buffer's value type, as by `write`
        escape_string(buf, Base.AnnotatedString("ab", [(1:1, 1)]))
        @test Base.annotations(buf) == [(region = 1:1, value = 1.0)]
    end
    # A known union's states are found without dispatch at runtime
    none = Memory{Union{Mark, Int}}(undef, 0)
    states = @inferred Base.AnnotatedDisplay.reductions(MIME("text/plain"), Union{Mark, Int}, (nothing, nothing), none)
    @test Set(states) == Set([Marked(false), nothing])
    # Values that don't display add no rendering of their own: a string displays through the
    # code compiled for the types that do
    walk = which(Base.AnnotatedDisplay.render, Tuple{Any, MIME, IO, Base.RegionIterator, Any})
    marked(Mark); html(marked(Mark))
    walks() = length(Base.specializations(walk))
    before = walks()
    for x in (1, 1.0, :s)
        mixed = Base.AnnotatedString{String, Union{Mark, typeof(x)}}("xyz", [(1:2, Mark()), (2:3, x)])
        @test colour(mixed) == colour(Base.AnnotatedString{String, Mark}("xyz", [(1:2, Mark())]))
        html(mixed)
    end
    @test walks() == before
    @test colour(Base.AnnotatedString{String, Int}("xy", [(1:1, 1)])) == "xy" # Nothing displays
    # Neither changes the text, whatever the value type holding them
    (; Patchy, Fickle) = MarkedAnnotations
    for T in (Patchy, Fickle)
        broken(V) = Base.AnnotatedString{String, V}("xy", [(1:1, T())])
        @test colour(broken(T)) == colour(broken(Union{T, Mark})) == colour(broken(Any)) == "xy"
        @test html(broken(T)) == html(broken(Union{T, Mark})) == html(broken(Any)) == "xy"
    end
    # An `Any` string is displayed by code compiled once, whatever types it holds, and however many
    D = Base.AnnotatedDisplay
    specialisations() = sum(f -> sum(m -> length(Base.specializations(m)), methods(f)), (D.render, D.renderheld, D.change))
    anyof(values...) = Base.AnnotatedString{String, Any}("abc", [(i:i, v) for (i, v) in enumerate(values)])
    colour(anyof(Mark())); html(anyof(Mark()))
    before = specialisations()
    for values in ((Mark(), Tag("t")), (Tag("t"), 1), (1.0, Mark(), :s), (Flag(), Mark(), Flag()))
        colour(anyof(values...)); html(anyof(values...))
    end
    @test specialisations() == before
    # Defining how a type displays must not invalidate code compiled for strings of unknown value
    # type. Checked in a fresh process, where only Base's methods exist as when a package is first
    # loaded; here the loaded packages' methods would make inference give up for that reason alone.
    guard = """
    import Base.AnnotatedDisplay: context, reduction, transition
    struct Late end
    struct LateState end
    struct Sink <: IO end
    Base.write(::Sink, ::UInt8) = 1
    Base.unsafe_write(::Sink, ::Ptr{UInt8}, n::UInt) = Int(n)
    # A new IO type and an unusual string type, so the print chain is compiled here rather than
    # taken from the sysimage; `v[1]` is inferred with the value type unbound.
    unknown_valtype(v::Vector{Base.AnnotatedString{SubString{String}}}) = print(IOContext(Sink(), :color => true), v[1])
    unknown_html(v::Vector{Base.AnnotatedString{SubString{String}}}) = show(Sink(), MIME("text/html"), v[1])
    strings = Base.AnnotatedString{SubString{String}}[Base.AnnotatedString{SubString{String}, Int}(SubString("x"))]
    unknown_valtype(strings)
    unknown_html(strings)
    anything = Base.AnnotatedString{String, Any}("x", [(1:1, 1)]) # And through the code compiled once for any values
    print(IOContext(Sink(), :color => true), anything)
    show(Sink(), MIME("text/html"), anything)
    log = ccall(:jl_debug_method_invalidation, Any, (Cint,), 1)
    context(::MIME, ::Type{Late}) = 0
    reduction(::MIME, ::Type{Late}, _, _) = LateState()
    transition(::IO, ::MIME, ::LateState, ::LateState) = nothing
    ccall(:jl_debug_method_invalidation, Any, (Cint,), 0)
    print(join(unique(x.def.name for x in log if x isa Core.MethodInstance), ' '))
    """
    invalidated = split(read(`$(Base.julia_cmd()) --startup-file=no -e $guard`, String))
    @test invalidated ⊆ ["context", "reduction", "transition"]
end
