@testset "StringViews" begin

    @testset "Construction" begin
        @test StringView([0x61, 0x62, 0x63]) == "abc"
        @test StringView(UInt8[0x48, 0x65, 0x6c, 0x6c, 0x6f]) == "Hello"

        @test StringView(codeunits("test")) == "test"
        @test StringView(b"hello") == "hello"

        s = StringView(UInt8[])
        @test s == ""
        @test isempty(s)

        @test StringView(UInt8[0xc3, 0xa5]) == "å"

        @test StringView(0x42:0x45) == "BCDE"

        # Non-concrete parameter
        @test_throws ArgumentError StringView{DenseVector{UInt8}}([0x01])
    end

    @testset "Construction from other types" begin
        v = UInt8[0x61, 0x62, 0x63]
        s = StringView(v)
        @test String(s) === "abc"

        # Vector{UInt8} now copies
        s = StringView(UInt8[0x48, 0x69])
        arr = Vector{UInt8}(s)
        @test arr == [0x48, 0x69]
        @test arr !== codeunits(s) # Constructor copies

        s = StringView(UInt8[0x78, 0x79, 0x7a])
        arr = Array{UInt8}(s)
        @test arr == [0x78, 0x79, 0x7a]
        @test arr !== codeunits(s) # Constructor copies

        # Vector conversion from view-backed StringView
        b = Vector{UInt8}("foobar")
        @test Vector{UInt8}(StringView(@view b[1:3])) == b[1:3]

        s = StringView(b)
        @test codeunits(String(s)) == s.data

        abc = StringView(0x61:0x63)
        @test Vector{UInt8}(abc) == 0x61:0x63

        @test Symbol(StringView(UInt8[0x61, 0x62])) === :ab
        @test Symbol(StringView(b)) == :foobar
        @test Symbol(abc) == :abc

        v = UInt8[0xf0, 0x9f, 0x98, 0x80, 0x44]
        s = StringView(v)
        @test String(s) == "😀D"

        @test Symbol(StringView(0x61:0x63)) === :abc
    end

    @testset "copy" begin
        v = UInt8[0x61, 0x62, 0x63]
        s = StringView(v)
        s2 = copy(s)
        @test s2 == s
        @test s2 !== s
        @test codeunits(s2) !== codeunits(s)

        v[1] = 0x78
        @test s == "xbc"
        @test s2 == "abc"

        # Copy is a StringView
        c = copy(StringView(Vector{UInt8}("foobar")))
        @test c isa StringView
        @test c == "foobar"
    end

    @testset "Basic properties" begin
        for v in Any[
                [0x61, 0x62, 0x63],
                0x41:0x48,
                b"abcdefg",
            ]
            s = StringView(v)
            @test sizeof(s) == sizeof(collect(v))
            @test codeunits(s) === v
            @test ncodeunits(s) == length(v)
            @test codeunit(s) === UInt8
        end

        s = StringView(UInt8[0xc3, 0xa5, 0x62])
        @test sizeof(s) == 3
        @test length(s) == 2

        @test sizeof(StringView(UInt8[])) == 0
        @test ncodeunits(StringView(UInt8[])) == 0
        @test length(StringView(UInt8[])) == 0

        b = Vector{UInt8}("foobar")
        s = StringView(b)
        @test ncodeunits(s) == sizeof(s) == length(b)
        @test codeunit(s) == UInt8
        @test codeunit(s, 3) == b[3]
    end

    @testset "Pointers" begin
        b = Vector{UInt8}("foobar")
        s = StringView(b)

        GC.@preserve b s begin
            @test pointer(s) == pointer(b) == Base.unsafe_convert(Ptr{UInt8}, s)
            @test Base.unsafe_convert(Ptr{Int8}, s) == Ptr{Int8}(pointer(s))
            @test pointer(s, 3) == pointer(b, 3)
        end

        # Non-dense arrays don't have pointer method
        abc = StringView(0x61:0x63)
        @test_throws MethodError pointer(abc)
    end

    @testset "Comparison and equality" begin
        s1 = StringView(UInt8[0x61, 0x62, 0x63])
        s2 = StringView(UInt8[0x61, 0x62, 0x63])
        @test s1 == s2

        s1 = StringView(UInt8[0x61, 0x62])
        @test s1 == "ab" == s1 # test both a == b and b == a

        @test StringView([0x61, 0x62]) == StringView(b"ab")
        @test StringView(UInt8[0x61]) != StringView(UInt8[0x62])

        s1 = StringView(0x61:0x62)
        s2 = StringView(UInt8[0x61, 0x62, 0x63])
        @test cmp(s1, s2) < 0
        @test cmp(s2, s1) > 0
        @test s1 != s2

        @test cmp(StringView(0x78:0x79), "xy") == 0
        @test StringView(UInt8[0xc3, 0xa5]) == "å"

        @test StringView(0x61:0x63) == "abc"
        @test StringView(b"test") == "test"

        b = Vector{UInt8}("foobar")
        s = StringView(b)
        @test cmp("foobar", "bar") == cmp(s, "bar") == -cmp("bar", s) == cmp(s, StringView(Vector{UInt8}("bar")))
        @test s == StringView(codeunits("foobar")) == "foobar" == s == "foobar" != StringView(Vector{UInt8}("bar"))

        abc = StringView(0x61:0x63)
        @test cmp(abc, "bar") == cmp("abc", "bar")
    end

    @testset "isascii and isvalid" begin
        @test isascii(StringView(UInt8[0x61, 0x62, 0x63]))
        @test !isascii(StringView(UInt8[0xc3, 0xa5]))
        @test isascii(StringView(UInt8[0x41, 0x42, 0x7f]))
        @test isascii(StringView(0x41:0x5a))
        @test !isascii(StringView(UInt8[0x61, 0xc3, 0xa5]))

        s = StringView(UInt8[0x61, 0x62, 0x63])
        @test isvalid(s, 1)
        @test isvalid(s, 2)
        @test !isvalid(s, 0)

        s = StringView(UInt8[0xc3, 0xa5, 0x62])
        @test isvalid(s, 1)
        @test !isvalid(s, 2)
        @test isvalid(s, 3)

        @test isvalid(StringView(0x61:0x63), 2)

        # Valid indices of invalid UTF8
        s = StringView(UInt8[0x8b, 0x52])
        @test isvalid(s, 1)
        @test isvalid(s, 2)
        @test !isvalid(s, 3)

        b = Vector{UInt8}("foobar")
        s = StringView(b)
        abc = StringView(0x61:0x63)
        invalid = StringView([0x8b, 0x52, 0x9b, 0x8d])

        @test isascii(s)
        @test !isascii(StringView(codeunits("fööbār")))

        @test isvalid(s)
        @test isvalid(abc)
        @test !isvalid(invalid)
        @test !invoke(isvalid, Tuple{StringView}, invalid)
    end

    @testset "Indexing" begin
        s = StringView(UInt8[0x61, 0x62, 0x63])
        @test s[1] == 'a'
        @test s[2] == 'b'
        @test s[3] == 'c'

        s = StringView(UInt8[0xc3, 0xa5, 0x62])
        @test s[1] == 'å'
        @test s[3] == 'b'
        @test_throws StringIndexError s[2]
        @test_throws BoundsError s[0]
        @test_throws BoundsError s[4]

        s = StringView(UInt8[0x61, 0x62, 0x63, 0x64])
        @test s[1:2] == StringView(UInt8[0x61, 0x62])
        @test s[2:4] == StringView(UInt8[0x62, 0x63, 0x64])
        @test typeof(s[1:0]) == typeof(s)

        @test StringView(UInt8[0x61, 0x62])[1:1] == StringView(UInt8[0x61])
        @test StringView(UInt8[0x61, 0x62, 0x63])[2:2] == "b"
        @test StringView(UInt8[0x61, 0x62, 0x63])[1:0] == StringView(UInt8[])

        @test StringView(0x61:0x64)[2:3] == "bc"

        # Slicing copies and preserves type
        v = [0x61, 0x62, 0x63]
        s = StringView(v)[2:3]
        @test s isa StringView{Vector{UInt8}}
        @test codeunits(s) !== v
        @test StringView(0x41:0x44)[2:3] isa StringView{UnitRange{UInt8}}
        @test_throws StringIndexError StringView(codeunits("fooα"))[1:5]
    end

    @testset "Iteration" begin
        @test collect(StringView(UInt8[0x61, 0x62, 0x63])) == ['a', 'b', 'c']
        @test collect(StringView(UInt8[0xc3, 0xa5, 0x62, 0x63])) == ['å', 'b', 'c']
        @test collect(StringView(UInt8[])) == []
        @test collect(StringView(UInt8[0x9f, 0xf0])) == ['\x9f', '\xf0']
        @test collect(StringView(UInt8[0xf0, 0x9f, 0x98, 0x80])) == ['😀']

        s = StringView(UInt8[0x61, 0xc3, 0xa5, 0x62])
        @test count(Returns(true), s) == 3
        @test collect(StringView(0x61:0x63)) == ['a', 'b', 'c']
        @test collect(StringView(b"hello")) == collect("hello")

        b = Vector{UInt8}("foobar")
        s = StringView(b)
        ss = SubString(s, 2, 5) # "ooba"
        abc = StringView(0x61:0x63)
        invalid = StringView([0x8b, 0x52, 0x9b, 0x8d])

        for str in Any[s, ss, abc, invalid]
            sS = String(str)
            @test sS == str
            @test length(sS) == length(str)
            @test collect(sS) == collect(str) ==
                getindex.(sS, eachindex(sS)) == getindex.(str, eachindex(sS))
            @test collect(eachindex(sS)) == collect(eachindex(str))
            @test sS[1:end] == str[1:end]
            @test sS[nextind(sS, 1):prevind(sS, end)] == str[nextind(str, 1):prevind(str, end)]
        end
    end

    @testset "thisind, nextind, prevind" begin
        # Thisind
        s = StringView(collect(codeunits("VeÅæy")))
        for i in [1, 2, 3, 5, 7]
            @test thisind(s, i) == i
        end
        @test thisind(s, 4) == 3
        @test thisind(s, 6) == 5
        @test thisind(s, 8) == 8
        @test thisind(s, 0) == 0

        # Nextind
        for i in [0, 1, 2, 4, 6, 7]
            @test nextind(s, i) == i + 1
        end
        @test nextind(s, 3) == 5
        @test nextind(s, 5) == 7

        # Prevind
        for i in [1, 2, 3, 4, 6, 8]
            @test prevind(s, i) == i - 1
        end
        @test prevind(s, 5) == 3
        @test prevind(s, 7) == 5

        # Make sure it works for non-dense arrays
        @test nextind(StringView(0x61:0x63), 1) == 2
    end

    @testset "Searching - findnext" begin
        @test findnext('a', StringView(UInt8[0x61, 0x62, 0x63]), 1) == 1
        @test findnext('a', StringView(UInt8[0x61, 0x62, 0x63]), 2) === nothing
        @test findnext('b', StringView(UInt8[0x61, 0x62, 0x63]), 1) == 2
        @test findnext('c', StringView(UInt8[0x61, 0x62, 0x63]), 1) == 3
        @test findnext('d', StringView(UInt8[0x61, 0x62, 0x63]), 1) === nothing

        s = StringView(UInt8[0x61, 0x61, 0x62])
        @test findnext('a', s, 1) == 1
        @test findnext('a', s, 2) == 2
        @test findnext('a', s, 3) === nothing

        s = StringView(UInt8[0xc3, 0xa5, 0x62, 0xc3, 0xa5])
        @test findnext('å', s, 1) == 1
        @test findnext('å', s, 3) == 4
        @test findnext('b', s, 1) == 3

        @test findnext('a', StringView(UInt8[]), 1) === nothing
        @test findnext('b', StringView(0x61:0x63), 1) == 2

        @test findnext('\xc3', StringView(b"æ"), 1) === nothing

        # Unicode char search
        @test findfirst(==('ø'), StringView(codeunits("abc"))) === nothing
        @test findfirst(==('ø'), StringView(codeunits("abæø"))) == 5
    end

    @testset "Searching - findprev" begin
        @test findprev('c', StringView(UInt8[0x61, 0x62, 0x63]), 3) == 3
        @test findprev('b', StringView(UInt8[0x61, 0x62, 0x63]), 3) == 2
        @test findprev('a', StringView(UInt8[0x61, 0x62, 0x63]), 3) == 1
        @test findprev('d', StringView(UInt8[0x61, 0x62, 0x63]), 3) === nothing

        s = StringView(UInt8[0x61, 0x62, 0x61])
        @test findprev('a', s, 3) == 3
        @test findprev('a', s, 2) == 1
        @test findprev('a', s, 0) === nothing

        s = StringView(UInt8[0xc3, 0xa5, 0x62, 0xc3, 0xa5])
        @test findprev('å', s, 5) == 4
        @test findprev('å', s, 3) == 1
        @test findprev('b', s, 5) == 3

        @test findprev('b', StringView(0x61:0x63), 3) == 2

        @test findlast('\xc3', StringView(b"æ")) === nothing

        # Unicode char search
        @test findlast(==('ø'), StringView(codeunits("abc"))) === nothing
        @test findlast(==('ø'), StringView(codeunits("abæø"))) == 5
    end

    @testset "Searching - findall" begin
        @test findall('a', StringView(UInt8[0x61, 0x62, 0x61])) == [1, 3]
        @test findall('a', StringView(UInt8[0x61, 0x61, 0x61])) == [1, 2, 3]
        @test findall('a', StringView(UInt8[0x62, 0x63, 0x64])) == []
        @test findall('å', StringView(UInt8[0xc3, 0xa5, 0x62, 0xc3, 0xa5])) == [1, 4]
        @test findall('b', StringView(0x61:0x63)) == [2]
    end

    @testset "Searching - general" begin
        b = Vector{UInt8}("foobar")
        s = StringView(b)
        ss = SubString(s, 2, 5) # "ooba"
        abc = StringView(0x61:0x63)

        for str in Any[s, ss, abc]
            sS, n = String(str), lastindex(str)
            @test startswith(str, "foo") == startswith(sS, "foo")
            @test endswith(str, "bar") == endswith(sS, "bar")
            @test replace(str, 'o' => "xy") == replace(sS, 'o' => "xy")
            @test replace(str, ('o', 'a') => 'x') == replace(sS, ('o', 'a') => 'x')
            @test findnext(==('b'), str, 1) === findnext(==('b'), sS, 1)
            @test findprev(==('b'), str, n) === findprev(==('b'), sS, n)
        end
    end

    @testset "Regex matching" begin
        s = StringView(UInt8[0x61, 0x62, 0x63])
        m = match(r"abc", s)
        @test m !== nothing
        @test m.match == "abc"

        s = StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f])
        m = match(r"l+", s)
        @test m !== nothing
        @test m.match == "ll"

        m = match(r"\d+", StringView(UInt8[0x31, 0x32, 0x33]))
        @test m !== nothing
        @test m.match == "123"

        @test occursin(r"bc", StringView(UInt8[0x61, 0x62, 0x63]))
        @test !occursin(r"xyz", StringView(UInt8[0x61, 0x62, 0x63]))

        @test startswith(StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f]), r"he")
        @test !startswith(StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f]), r"lo")

        @test endswith(StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f]), r"lo")
        @test !endswith(StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f]), r"he")

        s = StringView(UInt8[0x61, 0x62, 0x63, 0x64])
        m = match(r"b(c)", s)
        @test m !== nothing
        @test m.captures[1] == "c"

        b = Vector{UInt8}("foobar")
        s = StringView(b)
        ss = SubString(s, 2, 5) # "ooba"

        for str in Any[s, ss]
            @test [m.match for m in collect(eachmatch(r"[aeiou]+", str))] == ["oo", "a"]
            @test occursin(r"o+", str) && !occursin(r"z+", str)
            @test startswith(str, r"o+") == (str[1:2] == "oo")
            @test startswith(str, r"f+") == (str[1:2] == "fo")
            @test endswith(str, r"[aeiou]") == (str[end] == 'a')
            @test endswith(str, r"[q-z]") == (str[end] == 'r')
            @test findnext(r"o+", str, 4) === nothing
        end
        @test findnext(r"[aeiou]+", s, 1) == 2:3
        @test findnext(r"[aeiou]+", ss, 1) == 1:2

        sv = StringView(codeunits("foo 1234 bar"))
        m = match(r"[0-9]+", sv)
        @test m isa RegexMatch
        @test m.match == "1234"

        # Regex match of substring of stringview
        m = match(r"^([a-z]+)$", SubString(StringView(b"abc")))
        @test m !== nothing
        @test only(m.captures) isa SubString{StringView{typeof(b"abc")}}
    end

    @testset "Named subpatterns" begin
        m = match(r"(?<a>.)(.)(?<b>.)", StringView(codeunits("xyz")))
        @test haskey(m, :a)
        @test haskey(m, 2)
        @test haskey(m, "b")
        @test !haskey(m, "foo")
        @test (m[:a], m[2], m["b"]) == ("x", "y", "z")
        @test keys(m) == ["a", 2, "b"]
    end

    @testset "Parsing floats" begin
        @test tryparse(Float64, StringView(UInt8[0x31, 0x2e, 0x35])) == 1.5
        @test tryparse(Float64, StringView(UInt8[0x2d, 0x33, 0x2e, 0x31, 0x34])) == -3.14
        @test tryparse(Float64, StringView(UInt8[0x30, 0x2e, 0x30])) == 0.0
        @test tryparse(Float32, StringView(UInt8[0x32, 0x2e, 0x35])) == 2.5f0
        @test tryparse(Float64, StringView(UInt8[0x61, 0x62, 0x63])) === nothing

        s = StringView(UInt8[0x31, 0x65, 0x32])
        result = tryparse(Float64, s)
        @test result !== nothing && result ≈ 100.0

        @test tryparse(Float64, StringView(b"3.14")) ≈ 3.14
    end

    @testset "Parsing complex numbers" begin
        @test tryparse(Complex{Float64}, StringView(UInt8[0x31, 0x2b, 0x32, 0x69, 0x6d])) == 1.0 + 2.0im
        @test tryparse(Complex{Float64}, StringView(UInt8[0x2d, 0x31, 0x2b, 0x33, 0x69, 0x6d])) == -1.0 + 3.0im
        @test tryparse(Complex{Float64}, StringView(UInt8[0x32, 0x69, 0x6d])) == 2.0im
        @test tryparse(Complex{Float64}, StringView(UInt8[0x35])) == 5.0 + 0.0im
        @test tryparse(Complex{Float64}, StringView(b"2+3im")) == 2.0 + 3.0im
    end

    @testset "Parsing integers" begin
        @test tryparse(Int, StringView(UInt8[0x31, 0x32, 0x33])) == 123
        @test tryparse(Int, StringView(UInt8[0x2d, 0x34, 0x35])) == -45
        @test tryparse(Int, StringView(UInt8[0x30])) == 0
        @test tryparse(Int, StringView(UInt8[0x61, 0x62, 0x63])) === nothing
        @test tryparse(Int, StringView(UInt8[0x46, 0x46]), base = 16) == 255
        @test tryparse(Int, StringView(b"42")) == 42
    end

    @testset "Mixed parsing" begin
        for val in Any[true, 1234, 1234.5, 1234.5f0, 4.5 + 3.25im]
            sval = string(val)
            for str in Any[StringView(codeunits(sval)), SubString("foo" * sval * "bar", 4, 3 + length(sval))]
                @test parse(typeof(val), str) === val
            end
        end
    end

    @testset "SubString of StringView" begin
        s = StringView(UInt8[0x61, 0x62, 0x63, 0x64])
        sub = SubString(s, 2, 3)
        @test sub == "bc"

        s = StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f])
        sub = SubString(s, 1, 2)
        @test sub == "he"
        @test typeof(sub) <: SubString

        @test findnext('b', SubString(StringView(UInt8[0x61, 0x62, 0x63]), 1, 3), 1) == 2

        sub = SubString(StringView(0x61:0x64), 2, 3)
        @test sub == "bc"

        sub = SubString(StringView(b"hello"), 2, 4)
        @test sub == "ell"

        b = Vector{UInt8}("foobar")
        s = StringView(b)
        ss = SubString(s, 2, 5) # "ooba"

        @test Vector{UInt8}(ss) == codeunits(ss)
        @test codeunits(ss) == b[2:5]
        @test Symbol(ss) == :ooba

        @test pointer(ss) == pointer(b) + 1 == Base.unsafe_convert(Ptr{UInt8}, ss)
        @test ncodeunits(ss) == sizeof(ss) == length(b) - 2
        @test codeunit(ss) == UInt8
        @test codeunit(ss, 3) == b[4]

        @test cmp("foobar", "bar") == cmp(ss, "bar") == -cmp("bar", ss) == cmp(ss, StringView(Vector{UInt8}("bar")))
        @test ss == StringView(codeunits("ooba")) == "ooba" == ss == "ooba"
        @test isvalid(ss)
    end

    @testset "replace functionality" begin
        @test replace(StringView(UInt8[0x61, 0x62, 0x63]), "b" => "x") == "axc"
        @test replace(StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f]), "l" => "L") == "heLLo"
        @test replace(StringView(UInt8[0x61, 0x61, 0x61]), "a" => "b", count = 2) == "bba"
        @test replace(StringView(UInt8[0x78, 0x79, 0x7a]), "w" => "v") == "xyz"
        @test replace(StringView(0x61:0x63), "b" => "x") == "axc"
        @test replace(StringView(b"test"), "t" => "T") == "TesT"

        @test replace(StringView(b"abcd"), r"[bc]?" => "^") == "^a^^d^"
        @test replace(StringView(b"a"), 'a' => typeof) == "Char"
        @test replace(StringView(b"The foxes."), r"fox(es)?" => s"bus\1") == "The buses."

        @test replace(StringView(b"foobarbaz"), "oo" => "zz", "ar" => "zz", "z" => "m") == "fzzbzzbam"
        @test replace(StringView(b"foobar"), 'o' => '0', "" => "") == "f00bar"

        let v = [0x32, 0x30, 0x32, 0x31, 0x2d, 0x31, 0x31, 0x2d, 0x31, 0x30, 0x20, 0x32, 0x31, 0x3a, 0x34, 0x32, 0x3a, 0x30, 0x35, 0x2e, 0x31, 0x31, 0x35, 0x38, 0x30, 0x37],
                pat = r"(\.[\d]{3})\d+" => s"\g<1>"
            @test replace(String(copy(v)), pat) == replace(StringView(v), pat)
        end
    end

    @testset "startswith and endswith" begin
        @test startswith(StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f]), "he")
        @test !startswith(StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f]), "lo")
        @test endswith(StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f]), "lo")
        @test !endswith(StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f]), "he")

        s1 = StringView(UInt8[0x61, 0x62, 0x63])
        s2 = StringView(UInt8[0x61, 0x62])
        @test startswith(s1, s2)

        s1 = StringView(UInt8[0x78, 0x79, 0x7a])
        s2 = StringView(UInt8[0x79, 0x7a])
        @test endswith(s1, s2)

        @test startswith(StringView(0x61:0x63), "ab")
        @test endswith(StringView(b"hello"), "lo")
    end

    @testset "reverse" begin
        @test reverse(StringView(UInt8[0x61, 0x62, 0x63])) == "cba"
        @test reverse(StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f])) == "olleh"
        @test reverse(StringView(UInt8[])) == ""
        @test reverse(StringView(0x61:0x63)) == "cba"

        s = "abævø∩\0c"
        @test reverse(StringView(collect(codeunits(s)))) == reverse(s)
        @test reverse(StringView(codeunits(s))) isa StringView{Memory{UInt8}}

        # reverse returns same value as String reverse and same type
        b = Vector{UInt8}("foobar")
        s = StringView(b)
        ss = SubString(s, 2, 5)
        abc = StringView(0x61:0x63)
        invalid = StringView([0x8b, 0x52, 0x9b, 0x8d])

        for str in Any[s, abc, invalid, ss]
            @test reverse(str) == reverse(String(str))

            # Not guaranteed, but let's test it anyway, to reduce breakage
            @test reverse(str) isa StringView{Memory{UInt8}}
        end
    end

    @testset "chomp" begin
        @test chomp(StringView(UInt8[0x61, 0x62, 0x0a])) == "ab"
        @test chomp(StringView(UInt8[0x61, 0x62, 0x0d, 0x0a])) == "ab"
        @test chomp(StringView(UInt8[0x61, 0x62, 0x63])) == "abc"
        @test chomp(StringView(UInt8[0x0a])) == ""
        @test chomp(StringView(b"test\n")) == "test"
        @test chomp(StringView(codeunits("foo\n\n"))) == "foo\n"
    end

    @testset "write and print" begin
        io = IOBuffer()
        write(io, StringView(UInt8[0x61, 0x62, 0x63]))
        @test String(take!(io)) == "abc"

        io = IOBuffer()
        print(io, StringView(UInt8[0x68, 0x65, 0x6c, 0x6c, 0x6f]))
        @test String(take!(io)) == "hello"

        io = IOBuffer()
        write(io, StringView(0x61:0x63))
        @test String(take!(io)) == "abc"
    end

    @testset "typemin and one" begin
        s = StringView(UInt8[0x61, 0x62])
        @test typemin(s) isa StringView{Vector{UInt8}}
        @test typemin(s) == ""
        @test one(s) == one(typeof(s)) == typemin(s) == ""
        @test oneunit(s) == oneunit(typeof(s)) == one(s) == ""

        b = Vector{UInt8}("foobar")
        s = StringView(b)
        @test typemin(s) isa StringView{Vector{UInt8}}
        @test typemin(s) == ""
        @test one(s) == one(typeof(s)) == typemin(s) == ""
        @test oneunit(s) == oneunit(typeof(s)) == one(s) == ""
    end

    @testset "hash" begin
        # Hash matches String hash for various types.
        # We don't guarantee this, but may as well test it.
        s1 = StringView(UInt8[0x61, 0x62, 0x63])
        s2 = "abc"
        @test hash(s1) == hash(s2)

        @test hash(StringView(0x61:0x63)) == hash("abc")

        b = Vector{UInt8}("foobar")
        s = StringView(b)
        ss = SubString(s, 2, 5)
        abc = StringView(0x61:0x63)
        invalid = StringView([0x8b, 0x52, 0x9b, 0x8d])

        for str in Any[s, abc, invalid, ss]
            @test hash(str) == hash(String(str))
        end
    end

end # testset "StringViews"
