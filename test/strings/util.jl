# This file is a part of Julia. License is MIT: https://julialang.org/license

SubStr(s) = SubString("abc$(s)de", firstindex(s) + 3, lastindex(s) + 3)

@testset "textwidth" begin
    for (c, w) in [('x', 1), ('α', 1), ('🍕', 2), ('\0', 0), ('\u0302', 0), ('\xc0', 1)]
        @test textwidth(c) == w
        @test textwidth(c^3) == w*3
        @test w == @invoke textwidth(c::AbstractChar)
    end
    @test textwidth('\xc0\xa0') == 1 # overlong
    @test textwidth('\xf0\x80\x80') == 1 # malformed
    for i in 0x00:0x7f # test all ASCII chars (which have fast path)
        w = Int(ccall(:utf8proc_charwidth, Cint, (UInt32,), i))
        c = Char(i)
        @test textwidth(c) == w
        @test w == @invoke textwidth(c::AbstractChar)
    end
end

@testset "padding (lpad and rpad)" begin
    @test lpad("foo", 2) == "foo"
    @test rpad("foo", 2) == "foo"
    @test lpad("foo", 3) == "foo"
    @test rpad("foo", 3) == "foo"
    @test lpad("foo", 4) == " foo"
    @test rpad("foo", 4) == "foo "
    @test lpad("foo", 5) == "  foo"
    @test rpad("foo", 5) == "foo  "
    @test lpad("foo", 2, "123") == "foo"
    @test rpad("foo", 2, "123") == "foo"
    @test lpad("foo", 3, "123") == "foo"
    @test rpad("foo", 3, "123") == "foo"
    @test lpad("foo", 4, "123") == "1foo"
    @test rpad("foo", 4, "123") == "foo1"
    @test lpad("foo", 5, "123") == "12foo"
    @test rpad("foo", 5, "123") == "foo12"
    @test lpad("foo", 6, "123") == "123foo"
    @test rpad("foo", 6, "123") == "foo123"
    @test lpad("foo", 7, "123") == "1231foo"
    @test rpad("foo", 7, "123") == "foo1231"
    @test lpad("foo", 8, "123") == "12312foo"
    @test rpad("foo", 8, "123") == "foo12312"
    @test lpad("foo", 9, "123") == "123123foo"
    @test rpad("foo", 9, "123") == "foo123123"
    @test lpad("αβ", 2, "¹₂³") == "αβ"
    @test rpad("αβ", 2, "¹₂³") == "αβ"
    @test lpad("αβ", 3, "¹₂³") == "¹αβ"
    @test rpad("αβ", 3, "¹₂³") == "αβ¹"
    @test lpad("αβ", 4, "¹₂³") == "¹₂αβ"
    @test rpad("αβ", 4, "¹₂³") == "αβ¹₂"
    @test lpad("αβ", 5, "¹₂³") == "¹₂³αβ"
    @test rpad("αβ", 5, "¹₂³") == "αβ¹₂³"
    @test lpad("αβ", 6, "¹₂³") == "¹₂³¹αβ"
    @test rpad("αβ", 6, "¹₂³") == "αβ¹₂³¹"
    @test lpad("αβ", 7, "¹₂³") == "¹₂³¹₂αβ"
    @test rpad("αβ", 7, "¹₂³") == "αβ¹₂³¹₂"
    @test lpad("αβ", 8, "¹₂³") == "¹₂³¹₂³αβ"
    @test rpad("αβ", 8, "¹₂³") == "αβ¹₂³¹₂³"
    @test lpad("αβ", 9, "¹₂³") == "¹₂³¹₂³¹αβ"
    @test rpad("αβ", 9, "¹₂³") == "αβ¹₂³¹₂³¹"
    # Issue #32160 (unsigned underflow in lpad/rpad)
    @test lpad("xx", UInt(1), " ") == "xx"
    @test rpad("xx", UInt(1), " ") == "xx"
    # Issue #38256 (lpad/rpad defined in terms of textwidth)
    @test lpad("⟨k|H₁|k̃⟩", 12) |> textwidth == 12
    @test rpad("⟨k|H₁|k̃⟩", 12) |> textwidth == 12
    @test lpad("⟨k|H₁|k⟩", 12) |> textwidth == 12
    @test rpad("⟨k|H₁|k⟩", 12) |> textwidth == 12
    for pad in (rpad, lpad), p in ('\0', "\0", "\0\0", "\u302")
        if ncodeunits(p) == 1
            @test_throws r".*has zero textwidth.*maybe you want.*bytes.*" pad("foo", 10, p)
        else
            @test_throws r".*has zero textwidth$" pad("foo", 10, p)
        end
    end
end

@testset "string truncation (ltruncate, rtruncate, ctruncate)" begin
    @test ltruncate("foo", 4) == "foo"
    @test ltruncate("foo", 3) == "foo"
    @test ltruncate("foo", 2) == "…o"
    @test ltruncate("🍕🍕 I love 🍕", 10) == "…I love 🍕" # handle wide emojis
    @test ltruncate("🍕🍕 I love 🍕", 10, "[…]") == "[…]love 🍕"
    # when the replacement string is longer than the trunc
    # trust that the user wants the replacement string rather than erroring
    @test ltruncate("abc", 2, "xxxxxx") == "xxxxxx"

    @inferred ltruncate("xxx", 4)
    @inferred ltruncate("xxx", 2)
    @inferred ltruncate(@view("xxxxxxx"[1:4]), 4)
    @inferred ltruncate(@view("xxxxxxx"[1:4]), 2)

    @test rtruncate("foo", 4) == "foo"
    @test rtruncate("foo", 3) == "foo"
    @test rtruncate("foo", 2) == "f…"
    @test rtruncate("🍕🍕 I love 🍕", 10) == "🍕🍕 I lo…"
    @test rtruncate("🍕🍕 I love 🍕", 10, "[…]") == "🍕🍕 I […]"
    @test rtruncate("abc", 2, "xxxxxx") == "xxxxxx"

    @inferred rtruncate("xxx", 4)
    @inferred rtruncate("xxx", 2)
    @inferred rtruncate(@view("xxxxxxx"[1:4]), 4)
    @inferred rtruncate(@view("xxxxxxx"[1:4]), 2)

    @test ctruncate("foo", 4) == "foo"
    @test ctruncate("foo", 3) == "foo"
    @test ctruncate("foo", 2) == "f…"
    @test ctruncate("foo", 2; prefer_left=true) == "f…"
    @test ctruncate("foo", 2; prefer_left=false) == "…o"
    @test ctruncate("foobar", 6) == "foobar"
    @test ctruncate("foobar", 5) == "fo…ar"
    @test ctruncate("foobar", 4) == "fo…r"
    @test ctruncate("🍕🍕 I love 🍕", 10) == "🍕🍕 …e 🍕"
    @test ctruncate("🍕🍕 I love 🍕", 10, "[…]") == "🍕🍕[…] 🍕"
    @test ctruncate("abc", 2, "xxxxxx") == "xxxxxx"
    @test ctruncate("🍕🍕🍕🍕🍕🍕xxxxxxxxxxx", 9) == "🍕🍕…xxxx"

    @inferred ctruncate("xxxxx", 5)
    @inferred ctruncate("xxxxx", 3)
    @inferred ctruncate(@view("xxxxxxx"[1:5]), 5)
    @inferred ctruncate(@view("xxxxxxx"[1:5]), 3)
end

# string manipulation
@testset "lstrip/rstrip/strip" begin
    @test strip("") == ""
    @test strip(" ") == ""
    @test strip("  ") == ""
    @test strip("   ") == ""
    @test strip("\t  hi   \n") == "hi"
    @test strip(" \u2009 hi \u2009 ") == "hi"
    @test strip("foobarfoo", ['f','o']) == "bar"
    @test strip("foobarfoo", ('f','o')) == "bar"
    @test strip(ispunct, "¡Hola!") == "Hola"

    for s in ("", " ", " abc", "abc ", "  abc  "),
        f in (lstrip, rstrip, strip)

        fs = f(s)
        for T = (String, GenericString)
            local t, b
            t = convert(T,s)
            ft = f(t)
            @test s == t
            @test fs == ft
            @test typeof(ft) == SubString{T}

            b = convert(SubString{T}, t)
            fb = f(b)
            @test s == b
            @test fs == fb
            @test typeof(fb) == SubString{T}
        end
    end

    @test lstrip(isnumeric, "0123abc") == "abc"
    @test rstrip(isnumeric, "abc0123") == "abc"
    @test lstrip("ello", ['e','o']) == "llo"
    @test rstrip("ello", ['e','o']) == "ell"

    @test_throws ArgumentError strip("", "")
    @test_throws ArgumentError lstrip("", "")
    @test_throws ArgumentError rstrip("", "")
end

@testset "partition" begin
    # AbstractString to partition into SubString
    let v=collect(Iterators.partition("foobars",1))
    @test v==SubString{String}["f","o","o","b","a","r","s"]
    end

    let v=collect(Iterators.partition("foobars",2))
    @test v==SubString{String}["fo","ob","ar","s"]
    end

    for n in [7,8]
        @test collect(Iterators.partition("foobars",n))[1]=="foobars"
    end

    # HOWEVER enumerate explicitly slices String "atoms" so `Chars` are returned
    let v=collect(Iterators.partition(enumerate("foobars"),1))
        @test v==Vector{Tuple{Int64, Char}}[[(1, 'f')],[(2, 'o')],[(3, 'o')],[(4, 'b')],[(5, 'a')],[(6, 'r')], [(7, 's')]]
    end
end

@testset "rsplit/split" begin
    @test split("foo,bar,baz", 'x') == ["foo,bar,baz"]
    @test split("foo,bar,baz", ',') == ["foo","bar","baz"]
    @test split("foo,bar,baz", ",") == ["foo","bar","baz"]
    @test split("foo,bar,baz", r",") == ["foo","bar","baz"]
    @test split("foo,bar,baz", ','; limit=0) == ["foo","bar","baz"]
    @test split("foo,bar,baz", ','; limit=1) == ["foo,bar,baz"]
    @test split("foo,bar,baz", ','; limit=2) == ["foo","bar,baz"]
    @test split("foo,bar,baz", ','; limit=3) == ["foo","bar","baz"]
    @test split("foo,bar", "o,b") == ["fo","ar"]

    @test split("", ',') == [""]
    @test split(",", ',') == ["",""]
    @test split(",,", ',') == ["","",""]
    @test split("", ','  ; keepempty=false) == []
    @test split(",", ',' ; keepempty=false) == []
    @test split(",,", ','; keepempty=false) == []

    @test split("a b c") == ["a","b","c"]
    @test split("a  b \t c\n") == ["a","b","c"]
    @test split("α  β \u2009 γ\n") == ["α","β","γ"]

    @test split("a b c"; limit=2) == ["a","b c"]
    @test split("a  b \t c\n"; limit=3) == ["a","b","\t c\n"]
    @test split("a b c"; keepempty=true) == ["a","b","c"]
    @test split("a  b \t c\n"; keepempty=true) == ["a","","b","","","c",""]

    @test rsplit("foo,bar,baz", 'x') == ["foo,bar,baz"]
    @test rsplit("foo,bar,baz", ',') == ["foo","bar","baz"]
    @test rsplit("foo,bar,baz", ",") == ["foo","bar","baz"]
    @test rsplit("foo,bar,baz", ','; limit=0) == ["foo","bar","baz"]
    @test rsplit("foo,bar,baz", ','; limit=1) == ["foo,bar,baz"]
    @test rsplit("foo,bar,baz", ','; limit=2) == ["foo,bar","baz"]
    @test rsplit("foo,bar,baz", ','; limit=3) == ["foo","bar","baz"]
    @test rsplit("foo,bar", "o,b") == ["fo","ar"]

    @test rsplit("", ',') == [""]
    @test rsplit(",", ',') == ["",""]
    @test rsplit(",,", ',') == ["","",""]
    @test rsplit(",,", ','; limit=2) == [",",""]
    @test rsplit("", ','  ; keepempty=false) == []
    @test rsplit(",", ',' ; keepempty=false) == []
    @test rsplit(",,", ','; keepempty=false) == []

    @test rsplit("a b c") == ["a","b","c"]
    @test rsplit("a  b \t c\n") == ["a","b","c"]

    @test rsplit("a b c"; limit=2) == ["a b", "c"]
    @test rsplit("a  b \t c\n"; limit=3) == ["a ","b","c"]
    @test rsplit("a b c"; keepempty=true) == ["a","b","c"]
    @test rsplit("a  b \t c\n"; keepempty=true) == ["a","","b","","","c",""]

    let str = "a.:.ba..:..cba.:.:.dcba.:."
    @test split(str, ".:.") == ["a","ba.",".cba",":.dcba",""]
    @test split(str, ".:."; keepempty=false) == ["a","ba.",".cba",":.dcba"]
    @test split(str, ".:.") == ["a","ba.",".cba",":.dcba",""]
    @test split(str, r"\.(:\.)+") == ["a","ba.",".cba","dcba",""]
    @test split(str, r"\.(:\.)+"; keepempty=false) == ["a","ba.",".cba","dcba"]
    @test split(str, r"\.+:\.+") == ["a","ba","cba",":.dcba",""]
    @test split(str, r"\.+:\.+"; keepempty=false) == ["a","ba","cba",":.dcba"]

    @test rsplit(str, ".:.") == ["a","ba.",".cba.:","dcba",""]
    @test rsplit(str, ".:."; keepempty=false) == ["a","ba.",".cba.:","dcba"]
    @test rsplit(str, ".:."; limit=2) == ["a.:.ba..:..cba.:.:.dcba", ""]
    @test rsplit(str, ".:."; limit=3) == ["a.:.ba..:..cba.:", "dcba", ""]
    @test rsplit(str, ".:."; limit=4) == ["a.:.ba.", ".cba.:", "dcba", ""]
    @test rsplit(str, ".:."; limit=5) == ["a", "ba.", ".cba.:", "dcba", ""]
    @test rsplit(str, ".:."; limit=6) == ["a", "ba.", ".cba.:", "dcba", ""]
    end

    # zero-width splits
    @test split("", "") == rsplit("", "") == [""]
    @test split("abc", "") == rsplit("abc", "") == ["a","b","c"]
    @test rsplit("abc", "", limit=2) == ["ab","c"]
    @test rsplit("", "//") == [""]
    @test split("abc", "", limit=2) == ["a","bc"]

    @test split("", r"") == [""]
    @test split("abc", r"") == ["a","b","c"]
    @test split("abcd", r"b?") == ["a","c","d"]
    @test split("abcd", r"b*") == ["a","c","d"]
    @test split("abcd", r"b+") == ["a","cd"]
    @test split("abcd", r"b?c?") == ["a","d"]
    @test split("abcd", r"[bc]?") == ["a","","d"]
    @test split("abcd", r"a*") == ["","b","c","d"]
    @test split("abcd", r"a+") == ["","bcd"]
    @test split("abcd", r"d*") == ["a","b","c",""]
    @test split("abcd", r"d+") == ["abc",""]
    @test split("abcd", r"[ad]?") == ["","b","c",""]

    # multi-byte unicode characters (issue #26225)
    @test split("α β γ", " ") == rsplit("α β γ", " ") ==
          split("α β γ", isspace) == rsplit("α β γ", isspace) == ["α","β","γ"]
    @test split("ö.", ".") == rsplit("ö.", ".") == ["ö",""]
    @test split("α β γ", "β") == rsplit("α β γ", "β") == ["α "," γ"]
end

@testset "eachrsplit" begin
    @test collect(eachrsplit("", 'a')) == [""]
    @test collect(eachrsplit("", isspace; limit=3)) == [""]
    @test collect(eachrsplit("b c  d"; limit=2)) == ["d", "b c "]
    @test collect(eachrsplit("a.b.c", '.'; limit=1)) == ["a.b.c"]
    @test collect(eachrsplit("a..b..c", '.')) == ["c", "", "b", "", "a"]
    @test collect(eachrsplit("ax  b  c")) == ["c", "b", "ax"]
    @test collect(eachrsplit(" a 12 4 v ", isnumeric)) == [" v ", " ", "", " a "]
    @test collect(eachrsplit("ba", 'a')) == ["", "b"]
    @test collect(eachrsplit("   ")) == []
    @test collect(eachrsplit("aaaa", 'a'; keepempty=false)) == []
    @test collect(eachrsplit("aaaa", 'a'; limit=2)) == ["", "aaa"]
    @test collect(eachrsplit("abcdef", ['b', 'e'])) == ["f", "cd", "a"]
    @test collect(eachrsplit("abc", isletter)) == ["", "", "", ""]

    # This behaviour is quite surprising, but is consistent with split
    # See issue 45916
    @test collect(eachrsplit("a  b"; limit=2)) == ["b", "a "] # only one trailing space
    @test collect(eachrsplit("a "; limit=1)) == ["a "]
    @test collect(eachrsplit("  a  b  c  d"; limit=3)) == ["d", "c", "  a  b "]
end

@testset "replace" begin
    @test replace("\u2202", '*' => '\0') == "\u2202"

    @test replace("foobar", 'o' => '0') == "f00bar"
    @test replace("foobar", 'o' => '0', count=1) == "f0obar"
    @test replace("foobar", 'o' => "") == "fbar"
    @test replace("foobar", 'o' => "", count=1) == "fobar"
    @test replace("foobar", 'f' => 'F') == "Foobar"
    @test replace("foobar", 'r' => 'R') == "foobaR"

    @test replace("foofoofoo", "foo" => "bar") == "barbarbar"
    @test replace("foobarfoo", "foo" => "baz") == "bazbarbaz"
    @test replace("barfoofoo", "foo" => "baz") == "barbazbaz"

    @test replace("", "" => "") == ""
    @test replace("", "" => "x") == "x"
    @test replace("", "x" => "y") == ""

    @test replace("abcd", "" => "^") == "^a^b^c^d^"
    @test replace("abcd", "b" => "^") == "a^cd"
    @test replace("abcd", r"b?" => "^") == "^a^c^d^"
    @test replace("abcd", r"b+" => "^") == "a^cd"
    @test replace("abcd", r"b?c?" => "^") == "^a^d^"
    @test replace("abcd", r"[bc]?" => "^") == "^a^^d^"

    @test replace("foobarfoo", r"(fo|ba)" => "xx") == "xxoxxrxxo"
    @test replace("foobarfoo", r"(foo|ba)" => "bar") == "barbarrbar"

    @test replace("foobar", 'o' => 'ø') == "føøbar"
    @test replace("foobar", 'o' => 'ø', count=1) == "føobar"
    @test replace("føøbar", 'ø' => 'o') == "foobar"
    @test replace("føøbar", 'ø' => 'o', count=1) == "foøbar"
    @test replace("føøbar", 'ø' => 'ö') == "fööbar"
    @test replace("føøbar", 'ø' => 'ö', count=1) == "föøbar"
    @test replace("føøbar", 'ø' => "") == "fbar"
    @test replace("føøbar", 'ø' => "", count=1) == "føbar"
    @test replace("føøbar", 'f' => 'F') == "Føøbar"
    @test replace("ḟøøbar", 'ḟ' => 'F') == "Føøbar"
    @test replace("føøbar", 'f' => 'Ḟ') == "Ḟøøbar"
    @test replace("ḟøøbar", 'ḟ' => 'Ḟ') == "Ḟøøbar"
    @test replace("føøbar", 'r' => 'R') == "føøbaR"
    @test replace("føøbaṙ", 'ṙ' => 'R') == "føøbaR"
    @test replace("føøbar", 'r' => 'Ṙ') == "føøbaṘ"
    @test replace("føøbaṙ", 'ṙ' => 'Ṙ') == "føøbaṘ"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "bar") == "barbarbar"
    @test replace("ḟøøbarḟøø", "ḟøø" => "baz") == "bazbarbaz"
    @test replace("barḟøøḟøø", "ḟøø" => "baz") == "barbazbaz"

    @test replace("foofoofoo", "foo" => "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
    @test replace("fooƀäṙfoo", "foo" => "baz") == "bazƀäṙbaz"
    @test replace("ƀäṙfoofoo", "foo" => "baz") == "ƀäṙbazbaz"

    @test replace("foofoofoo", "foo" => "bar") == "barbarbar"
    @test replace("foobarfoo", "foo" => "ƀäż") == "ƀäżbarƀäż"
    @test replace("barfoofoo", "foo" => "ƀäż") == "barƀäżƀäż"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
    @test replace("ḟøøƀäṙḟøø", "ḟøø" => "baz") == "bazƀäṙbaz"
    @test replace("ƀäṙḟøøḟøø", "ḟøø" => "baz") == "ƀäṙbazbaz"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "bar") == "barbarbar"
    @test replace("ḟøøbarḟøø", "ḟøø" => "ƀäż") == "ƀäżbarƀäż"
    @test replace("barḟøøḟøø", "ḟøø" => "ƀäż") == "barƀäżƀäż"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
    @test replace("ḟøøƀäṙḟøø", "ḟøø" => "ƀäż") == "ƀäżƀäṙƀäż"
    @test replace("ƀäṙḟøøḟøø", "ḟøø" => "ƀäż") == "ƀäṙƀäżƀäż"

    @test replace("", "" => "ẍ") == "ẍ"
    @test replace("", "ẍ" => "ÿ") == ""

    @test replace("äƀçđ", "" => "π") == "πäπƀπçπđπ"
    @test replace("äƀçđ", "ƀ" => "π") == "äπçđ"
    @test replace("äƀçđ", r"ƀ?" => "π") == "πäπçπđπ"
    @test replace("äƀçđ", r"ƀ+" => "π") == "äπçđ"
    @test replace("äƀçđ", r"ƀ?ç?" => "π") == "πäπđπ"
    @test replace("äƀçđ", r"[ƀç]?" => "π") == "πäππđπ"

    @test replace("foobarfoo", r"(fo|ba)" => "ẍẍ") == "ẍẍoẍẍrẍẍo"

    @test replace("ḟøøbarḟøø", r"(ḟø|ba)" => "xx") == "xxøxxrxxø"
    @test replace("ḟøøbarḟøø", r"(ḟøø|ba)" => "bar") == "barbarrbar"

    @test replace("fooƀäṙfoo", r"(fo|ƀä)" => "xx") == "xxoxxṙxxo"
    @test replace("fooƀäṙfoo", r"(foo|ƀä)" => "ƀäṙ") == "ƀäṙƀäṙṙƀäṙ"

    @test replace("ḟøøƀäṙḟøø", r"(ḟø|ƀä)" => "xx") == "xxøxxṙxxø"
    @test replace("ḟøøƀäṙḟøø", r"(ḟøø|ƀä)" => "ƀäṙ") == "ƀäṙƀäṙṙƀäṙ"

    @test replace("foo", "oo" => uppercase) == "fOO"

    # Issue 13332
    @test replace("abc", 'b' => 2.1) == "a2.1c"

    # Issue 31456
    @test replace("The fox.", r"fox(es)?" => s"bus\1") == "The bus."
    @test replace("The foxes.", r"fox(es)?" => s"bus\1") == "The buses."
    @test replace("The quick fox quickly.", r"(quick)?\sfox(es)?\s(run)?" => s"\1 bus\2 \3") == "The quick bus quickly."

    # test replace with a count for String and GenericString
    # check that replace is a no-op if count==0
    for s in ["aaa", Test.GenericString("aaa")]
        @test replace("aaa", 'a' => 'z', count=0) == "aaa"
        @test replace(s, 'a' => 'z', count=1) == "zaa"
        @test replace(s, 'a' => 'z', count=2) == "zza"
        @test replace(s, 'a' => 'z', count=3) == "zzz"
        @test replace(s, 'a' => 'z', count=4) == "zzz"
        @test replace(s, 'a' => 'z', count=typemax(Int)) == "zzz"
        @test replace(s, 'a' => 'z')    == "zzz"
    end

    # Issue 25741
    @test replace("abc", ['a', 'd'] => 'A') == "Abc"

    # for Char pattern call Char replacement function
    @test replace("a", "a" => typeof) == "SubString{String}"
    @test replace("a", r"a" => typeof) == "SubString{String}"
    @test replace("a", 'a' => typeof) == "Char"
    @test replace("a", in("a") => typeof) == "Char"
    @test replace("a", ['a'] => typeof) == "Char"

    # Issue 36953
    @test replace("abc", "" => "_", count=1) == "_abc"

    # tests for io::IO API (in addition to internals exercised above):
    let buf = IOBuffer()
        replace(buf, "aaa", 'a' => 'z', count=0)
        replace(buf, "aaa", 'a' => 'z', count=1)
        replace(buf, "bbb", 'a' => 'z')
        replace(buf, "aaa", 'a' => 'z')
        @test String(take!(buf)) == "aaazaabbbzzz"
    end
    let tempfile = tempname()
        try
            open(tempfile, "w") do f
                replace(f, "aaa", 'a' => 'z', count=0)
                replace(f, "aaa", 'a' => 'z', count=1)
                replace(f, "bbb", 'a' => 'z')
                replace(f, "aaa", 'a' => 'z')
                print(f, "\n")
            end
            @test read(tempfile, String) == "aaazaabbbzzz\n"
        finally
            rm(tempfile, force=true)
        end
    end
end

@testset "replace many" begin
    # PR 35414 Francesco Alemanno <francescoalemanno710@gmail.com>
    @test replace("foobarbaz", "oo" => "zz", "ar" => "zz", "z" => "m") == "fzzbzzbam"
    substmp=["z" => "m", "oo" => "zz", "ar" => "zz"]
    for perm in [[1, 2, 3], [2, 1, 3], [3, 2, 1], [2, 3, 1], [1, 3, 2], [3, 1, 2]]
        @test replace("foobarbaz", substmp[perm]...) == "fzzbzzbam"
        @test replace("foobarbaz", substmp[perm]..., count=2) == "fzzbzzbaz"
        @test replace("foobarbaz", substmp[perm]..., count=1) == "fzzbarbaz"
    end
    @test replace("foobarbaz", "z" => "m", r"a.*a" => uppercase) == "foobARBAm"
    @test replace("foobarbaz", 'o' => 'z', 'a' => 'q', 'z' => 'm') == "fzzbqrbqm"


    # PR #25732 Klaus Crusius <klaus.crusius@web.de>
    @test replace("\u2202", '*' => '\0', "" => "") == "\u2202"

    @test replace("foobar", 'o' => '0', "" => "") == "f00bar"
    @test replace("foobar", 'o' => '0', count=1, "" => "") == "foobar"
    @test replace("foobar", 'o' => '0', count=2, "" => "") == "f0obar"
    @test replace("foobar", 'o' => "", "" => "") == "fbar"
    @test replace("foobar", 'o' => "", count=1, "" => "") == "foobar"
    @test replace("foobar", 'o' => "", count=2, "" => "") == "fobar"
    @test replace("foobar", 'f' => 'F', "" => "") == "Foobar"
    @test replace("foobar", 'r' => 'R', "" => "") == "foobaR"

    @test replace("foofoofoo", "foo" => "bar", "" => "") == "barbarbar"
    @test replace("foobarfoo", "foo" => "baz", "" => "") == "bazbarbaz"
    @test replace("barfoofoo", "foo" => "baz", "" => "") == "barbazbaz"

    @test replace("", "" => "", "" => "") == ""
    @test replace("", "" => "x", "" => "") == "x"
    @test replace("", "x" => "y", "" => "") == ""

    @test replace("abcd", "" => "^", "" => "") == "^a^b^c^d^"
    @test replace("abcd", "b" => "^", "" => "") == "a^cd"
    @test replace("abcd", r"b?" => "^", "" => "") == "^a^c^d^"
    @test replace("abcd", r"b+" => "^", "" => "") == "a^cd"
    @test replace("abcd", r"b?c?" => "^", "" => "") == "^a^d^"
    @test replace("abcd", r"[bc]?" => "^", "" => "") == "^a^^d^"

    @test replace("foobarfoo", r"(fo|ba)" => "xx", "" => "") == "xxoxxrxxo"
    @test replace("foobarfoo", r"(foo|ba)" => "bar", "" => "") == "barbarrbar"

    @test replace("foobar", 'o' => 'ø', "" => "") == "føøbar"
    @test replace("foobar", 'o' => 'ø', count=2, "" => "") == "føobar"
    @test replace("føøbar", 'ø' => 'o', "" => "") == "foobar"
    @test replace("føøbar", 'ø' => 'o', count=2, "" => "") == "foøbar"
    @test replace("føøbar", 'ø' => 'ö', "" => "") == "fööbar"
    @test replace("føøbar", 'ø' => 'ö', count=2, "" => "") == "föøbar"
    @test replace("føøbar", 'ø' => "", "" => "") == "fbar"
    @test replace("føøbar", 'ø' => "", count=2, "" => "") == "føbar"
    @test replace("føøbar", 'f' => 'F', "" => "") == "Føøbar"
    @test replace("ḟøøbar", 'ḟ' => 'F', "" => "") == "Føøbar"
    @test replace("føøbar", 'f' => 'Ḟ', "" => "") == "Ḟøøbar"
    @test replace("ḟøøbar", 'ḟ' => 'Ḟ', "" => "") == "Ḟøøbar"
    @test replace("føøbar", 'r' => 'R', "" => "") == "føøbaR"
    @test replace("føøbaṙ", 'ṙ' => 'R', "" => "") == "føøbaR"
    @test replace("føøbar", 'r' => 'Ṙ', "" => "") == "føøbaṘ"
    @test replace("føøbaṙ", 'ṙ' => 'Ṙ', "" => "") == "føøbaṘ"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "bar", "" => "") == "barbarbar"
    @test replace("ḟøøbarḟøø", "ḟøø" => "baz", "" => "") == "bazbarbaz"
    @test replace("barḟøøḟøø", "ḟøø" => "baz", "" => "") == "barbazbaz"

    @test replace("foofoofoo", "foo" => "ƀäṙ", "" => "") == "ƀäṙƀäṙƀäṙ"
    @test replace("fooƀäṙfoo", "foo" => "baz", "" => "") == "bazƀäṙbaz"
    @test replace("ƀäṙfoofoo", "foo" => "baz", "" => "") == "ƀäṙbazbaz"

    @test replace("foofoofoo", "foo" => "bar", "" => "") == "barbarbar"
    @test replace("foobarfoo", "foo" => "ƀäż", "" => "") == "ƀäżbarƀäż"
    @test replace("barfoofoo", "foo" => "ƀäż", "" => "") == "barƀäżƀäż"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "ƀäṙ", "" => "") == "ƀäṙƀäṙƀäṙ"
    @test replace("ḟøøƀäṙḟøø", "ḟøø" => "baz", "" => "") == "bazƀäṙbaz"
    @test replace("ƀäṙḟøøḟøø", "ḟøø" => "baz", "" => "") == "ƀäṙbazbaz"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "bar", "" => "") == "barbarbar"
    @test replace("ḟøøbarḟøø", "ḟøø" => "ƀäż", "" => "") == "ƀäżbarƀäż"
    @test replace("barḟøøḟøø", "ḟøø" => "ƀäż", "" => "") == "barƀäżƀäż"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "ƀäṙ", "" => "") == "ƀäṙƀäṙƀäṙ"
    @test replace("ḟøøƀäṙḟøø", "ḟøø" => "ƀäż", "" => "") == "ƀäżƀäṙƀäż"
    @test replace("ƀäṙḟøøḟøø", "ḟøø" => "ƀäż", "" => "") == "ƀäṙƀäżƀäż"

    @test replace("", "" => "ẍ", "" => "") == "ẍ"
    @test replace("", "ẍ" => "ÿ", "" => "") == ""

    @test replace("äƀçđ", "" => "π", "" => "") == "πäπƀπçπđπ"
    @test replace("äƀçđ", "ƀ" => "π", "" => "") == "äπçđ"
    @test replace("äƀçđ", r"ƀ?" => "π", "" => "") == "πäπçπđπ"
    @test replace("äƀçđ", r"ƀ+" => "π", "" => "") == "äπçđ"
    @test replace("äƀçđ", r"ƀ?ç?" => "π", "" => "") == "πäπđπ"
    @test replace("äƀçđ", r"[ƀç]?" => "π", "" => "") == "πäππđπ"

    @test replace("foobarfoo", r"(fo|ba)" => "ẍẍ", "" => "") == "ẍẍoẍẍrẍẍo"

    @test replace("ḟøøbarḟøø", r"(ḟø|ba)" => "xx", "" => "") == "xxøxxrxxø"
    @test replace("ḟøøbarḟøø", r"(ḟøø|ba)" => "bar", "" => "") == "barbarrbar"

    @test replace("fooƀäṙfoo", r"(fo|ƀä)" => "xx", "" => "") == "xxoxxṙxxo"
    @test replace("fooƀäṙfoo", r"(foo|ƀä)" => "ƀäṙ", "" => "") == "ƀäṙƀäṙṙƀäṙ"

    @test replace("ḟøøƀäṙḟøø", r"(ḟø|ƀä)" => "xx", "" => "") == "xxøxxṙxxø"
    @test replace("ḟøøƀäṙḟøø", r"(ḟøø|ƀä)" => "ƀäṙ", "" => "") == "ƀäṙƀäṙṙƀäṙ"

    @test replace("foo", "oo" => uppercase, "" => "") == "fOO"

    # Issue 13332
    @test replace("abc", 'b' => 2.1, "" => "") == "a2.1c"

    # test replace with a count for String and GenericString
    # check that replace is a no-op if count==0
    for s in ["aaa", Test.GenericString("aaa")]
        @test_throws DomainError replace(s, 'a' => "", count = -1, "" => "")
        @test replace(s, 'a' => 'z', count=0, "" => "")::String == s
        @test replace(s, 'a' => 'z', count=1, "" => "") == "zaa"
        @test replace(s, 'a' => 'z', count=2, "" => "") == "zza"
        @test replace(s, 'a' => 'z', count=3, "" => "") == "zzz"
        @test replace(s, 'a' => 'z', count=4, "" => "") == "zzz"
        @test replace(s, 'a' => 'z', count=typemax(Int), "" => "") == "zzz"
        @test replace(s, 'a' => 'z', "" => "") == "zzz"
    end

    let s = "abc"
        @test replace(s) === s
        @test replace(s, 'a' => 'z', "" => "") === "zbc"
        @test replace(s, 'a' => 'z', 'b' => 'y') == "zyc"
        @test replace(s, 'a' => 'z', 'c' => 'x', "b" => 'y') == "zyx"
        @test replace(s, '1' => 'z', "" => "") == s
        @test replace(s, 'b' => "BbB", "" => "", count=2) == "aBbBc"
    end

    let s = "quick quicker quickest"
        @test replace(s) === s
        @test replace(s, "quickest" => 'z', "quicker" => uppercase, "quick" => 'a') == "a QUICKER z"
        @test replace(s, "quick" => 'a', "quicker" => uppercase, "quickest" => 'z') == "a aer aest"
        @test replace(s, "quickest" => "lame", "quicker" => "is", "quick" => "Duck", count=2) == "Duck is quickest"
        @test "1q1u1i1c1k1 1q1u1i1c1k1e1r1 1q1u1i1c1k1e1s1t1" ==
              replace(s, "" => '1', "" => "") ==
              replace(s, "" => '1', "" => '2')
        @test replace(s, "qu" => "QU", "qu" => "never happens", "ick" => "") == "QU QUer QUest"
        @test replace(s, " " => '_', "r " => "r-") == "quick_quicker-quickest"
        @test replace(s, r"[aeiou]" => "ä", "ui" => "ki", "i" => "I") == "qääck qääckär qääckäst"
        @test replace(s, "i" => "I", "ui" => "ki", r"[aeiou]" => "ä") == "qkick qkickär qkickäst"
        @test replace(s, r"[^ ]+" => "word", "quicker " => "X", count=big"99") == "word word word"
        @test replace(s, "quicker " => "X", r"[^ ]+" => "word", count=big"99") == "word Xword"

        @test replace(s, r"(quick)(e)" => s"\2-\1", "x" => "X") == "quick e-quickr e-quickst"

        @test replace(s, 'q' => 'Q', 'u' => 'U') == "QUick QUicker QUickest"
        @test replace(s, 'q' => 'Q', r"u" => 'U') == "QUick QUicker QUickest"
        @test replace(s, 'q' => 'Q', ==('u') => uppercase) == "QUick QUicker QUickest"
        @test replace(s, 'q' => 'Q', islowercase => '-') == "Q---- Q------ Q-------"
        @test replace(s, ['q', 'u'] => 'K') == "KKick KKicker KKickest"
        @test replace(s, occursin("uq") => 'K') == "KKick KKicker KKickest"
        @test replace(s, ==('q') => "B") == "Buick Buicker Buickest"

        @test replace(s, "qui" => "A", 'r' => 'R') == "Ack AckeR Ackest"
        @test replace(s, 'r' => 'x', islowercase => uppercase) == "QUICK QUICKEx QUICKEST"
        @test replace(s, islowercase => uppercase, 'r' => 'x') == "QUICK QUICKER QUICKEST"
        @test replace(s, "q" => "z", islowercase => uppercase, 'r' => 'x') == "zUICK zUICKER zUICKEST"
        @test replace(s, "qui" => "A", 'r' => 'x', islowercase => uppercase) == "ACK ACKEx ACKEST"
        @test replace(s, "qui" => "A", 'r' => 'x', islowercase => uppercase) == "ACK ACKEx ACKEST"
        @test replace(s, r"q" => "z", islowercase => uppercase, 'r' => 'x') == "zUICK zUICKER zUICKEST"

        @test replace(s, "q" => s"a\0b") == "aqbuick aqbuicker aqbuickest"
        @test replace(s, "q" => s"a\0b\n\\\g<0>") == "aqb\n\\quick aqb\n\\quicker aqb\n\\quickest"
        @test_throws ErrorException("PCRE error: unknown substring") replace(s, r"q" => s"a\1b")
        @test_throws ErrorException("Bad replacement string: pattern is not a Regex") replace(s, "q" => s"a\1b")
    end
end

@testset "chomp/chop" begin
    for S in (String, SubStr, Test.GenericString)
        @test chomp(S("foo\n")) == "foo"
        @test chomp(S("fo∀\n")) == "fo∀"
        @test chomp(S("foo\r\n")) == "foo"
        @test chomp(S("fo∀\r\n")) == "fo∀"
        @test chomp(S("fo∀")) == "fo∀"
        @test chop(S("")) == ""
        @test chop(S("fooε")) == "foo"
        @test chop(S("foεo")) == "foε"
        @test chop(S("∃∃∃∃")) == "∃∃∃"
        @test chop(S("∀ϵ∃Δ"), head=0, tail=0) == "∀ϵ∃Δ"
        @test chop(S("∀ϵ∃Δ"), head=0, tail=1) == "∀ϵ∃"
        @test chop(S("∀ϵ∃Δ"), head=0, tail=2) == "∀ϵ"
        @test chop(S("∀ϵ∃Δ"), head=0, tail=3) == "∀"
        @test chop(S("∀ϵ∃Δ"), head=0, tail=4) == ""
        @test chop(S("∀ϵ∃Δ"), head=0, tail=5) == ""
        @test chop(S("∀ϵ∃Δ"), head=1, tail=0) == "ϵ∃Δ"
        @test chop(S("∀ϵ∃Δ"), head=2, tail=0) == "∃Δ"
        @test chop(S("∀ϵ∃Δ"), head=3, tail=0) == "Δ"
        @test chop(S("∀ϵ∃Δ"), head=4, tail=0) == ""
        @test chop(S("∀ϵ∃Δ"), head=5, tail=0) == ""
        @test chop(S("∀ϵ∃Δ"), head=1, tail=1) == "ϵ∃"
        @test chop(S("∀ϵ∃Δ"), head=2, tail=2) == ""
        @test chop(S("∀ϵ∃Δ"), head=3, tail=3) == ""
        @test_throws ArgumentError chop(S("∀ϵ∃Δ"), head=-3, tail=3)
        @test_throws ArgumentError chop(S("∀ϵ∃Δ"), head=3, tail=-3)
        @test_throws ArgumentError chop(S("∀ϵ∃Δ"), head=-3, tail=-3)

        for T in (String, SubStr, Test.GenericString, Regex)
            S === Test.GenericString && T === Regex && continue # not supported
            @test chopprefix(S("fo∀\n"), T("bog")) == "fo∀\n"
            @test chopprefix(S("fo∀\n"), T("\n∀foΔ")) == "fo∀\n"
            @test chopprefix(S("fo∀\n"), T("∀foΔ")) == "fo∀\n"
            @test chopprefix(S("fo∀\n"), T("f")) == "o∀\n"
            @test chopprefix(S("fo∀\n"), T("fo")) == "∀\n"
            @test chopprefix(S("fo∀\n"), T("fo∀")) == "\n"
            @test chopprefix(S("fo∀\n"), T("fo∀\n")) == ""
            @test chopprefix(S("\nfo∀"), T("bog")) == "\nfo∀"
            @test chopprefix(S("\nfo∀"), T("\n∀foΔ")) == "\nfo∀"
            @test chopprefix(S("\nfo∀"), T("\nfo∀")) == ""
            @test chopprefix(S("\nfo∀"), T("\n")) == "fo∀"
            @test chopprefix(S("\nfo∀"), T("\nf")) == "o∀"
            @test chopprefix(S("\nfo∀"), T("\nfo")) == "∀"
            @test chopprefix(S("\nfo∀"), T("\nfo∀")) == ""
            @test chopprefix(S(""), T("")) == ""
            @test chopprefix(S(""), T("asdf")) == ""
            @test chopprefix(S(""), T("∃∃∃")) == ""
            @test chopprefix(S("εfoo"), T("ε")) == "foo"
            @test chopprefix(S("ofoε"), T("o")) == "foε"
            @test chopprefix(S("∃∃∃∃"), T("∃")) == "∃∃∃"
            @test chopprefix(S("∃∃∃∃"), T("")) == "∃∃∃∃"

            @test chopsuffix(S("fo∀\n"), T("bog")) == "fo∀\n"
            @test chopsuffix(S("fo∀\n"), T("\n∀foΔ")) == "fo∀\n"
            @test chopsuffix(S("fo∀\n"), T("∀foΔ")) == "fo∀\n"
            @test chopsuffix(S("fo∀\n"), T("\n")) == "fo∀"
            @test chopsuffix(S("fo∀\n"), T("∀\n")) == "fo"
            @test chopsuffix(S("fo∀\n"), T("o∀\n")) == "f"
            @test chopsuffix(S("fo∀\n"), T("fo∀\n")) == ""
            @test chopsuffix(S("\nfo∀"), T("bog")) == "\nfo∀"
            @test chopsuffix(S("\nfo∀"), T("\n∀foΔ")) == "\nfo∀"
            @test chopsuffix(S("\nfo∀"), T("\nfo∀")) == ""
            @test chopsuffix(S("\nfo∀"), T("∀")) == "\nfo"
            @test chopsuffix(S("\nfo∀"), T("o∀")) == "\nf"
            @test chopsuffix(S("\nfo∀"), T("fo∀")) == "\n"
            @test chopsuffix(S("\nfo∀"), T("\nfo∀")) == ""
            @test chopsuffix(S(""), T("")) == ""
            @test chopsuffix(S(""), T("asdf")) == ""
            @test chopsuffix(S(""), T("∃∃∃")) == ""
            @test chopsuffix(S("fooε"), T("ε")) == "foo"
            @test chopsuffix(S("εofo"), T("o")) == "εof"
            @test chopsuffix(S("∃∃∃∃"), T("∃")) == "∃∃∃"
            @test chopsuffix(S("∃∃∃∃"), T("")) == "∃∃∃∃"
        end
        @test isa(chomp(S("foo")), SubString)
        @test isa(chop(S("foo")), SubString)

        if S !== Test.GenericString
            @test chopprefix(S("∃∃∃b∃"), r"∃+") == "b∃"
            @test chopsuffix(S("∃b∃∃∃"), r"∃+") == "∃b"
        end

        @test isa(chopprefix(S("foo"), "fo"), SubString)
        @test isa(chopsuffix(S("foo"), "oo"), SubString)
    end
end

@testset "bytes2hex and hex2bytes" begin
    hex_str = "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
    bin_val = hex2bytes(hex_str)

    @test div(length(hex_str), 2) == length(bin_val)
    @test hex_str == bytes2hex(bin_val) == sprint(bytes2hex, bin_val)

    bin_val = hex2bytes("07bf")
    @test bin_val[1] == 7
    @test bin_val[2] == 191
    @test typeof(bin_val) == Array{UInt8, 1}
    @test length(bin_val) == 2

    # all valid hex chars
    @test "0123456789abcdefabcdef" == bytes2hex(hex2bytes("0123456789abcdefABCDEF"))

    # odd size
    @test_throws ArgumentError hex2bytes("0123456789abcdefABCDEF0")

    #non-hex characters
    @test_throws ArgumentError hex2bytes("0123456789abcdefABCDEFGH")

    @testset "Issue 23161" begin
        arr = b"0123456789abcdefABCDEF"
        arr1 = Vector{UInt8}(undef, length(arr) >> 1)
        @test hex2bytes!(arr1, arr) === arr1 # check in-place
        @test "0123456789abcdefabcdef" == bytes2hex(arr1)
        @test hex2bytes("0123456789abcdefABCDEF") == hex2bytes(arr)
        @test_throws ArgumentError hex2bytes!(arr1, b"") # incorrect arr1 length
        @test hex2bytes(b"") == UInt8[]
        @test hex2bytes(view(b"012345",1:6)) == UInt8[0x01,0x23,0x45]
        @test begin
            s = view(b"012345ab",1:6)
            d = view(zeros(UInt8, 10),1:3)
            hex2bytes!(d,s) == UInt8[0x01,0x23,0x45]
        end
        # odd size
        @test_throws ArgumentError hex2bytes(b"0123456789abcdefABCDEF0")

        #non-hex characters
        @test_throws ArgumentError hex2bytes(b"0123456789abcdefABCDEFGH")
    end

    @testset "Issue 39284" begin
        @test "efcdabefcdab8967452301" == bytes2hex(Iterators.reverse(hex2bytes("0123456789abcdefABCDEF")))
        @test hex2bytes(Iterators.reverse(b"CE1A85EECc")) == UInt8[0xcc, 0xee, 0x58, 0xa1, 0xec]
    end
end

# b"" should be immutable
let testb() = b"0123"
    b = testb()
    @test eltype(b) === UInt8
    @test b isa AbstractVector
    @test_throws Base.CanonicalIndexError b[4] = '4'
    @test testb() == UInt8['0','1','2','3']
end

@testset "Base.rest" begin
    s = "aβcd"
    @test Base.rest(s) === SubString(s)
    a, b, c... = s
    @test c === SubString(s, 4)

    s = SubString("aβcd", 2)
    @test Base.rest(s) === SubString(s)
    b, c... = s
    @test c === SubString(s, 3)

    s = GenericString("aβcd")
    @test Base.rest(s) === "aβcd"
    a, b, c... = s
    @test c === "cd"
end

@testset "endswith" begin
    A = "Fun times with Julialang"
    B = "A language called Julialang"
    @test endswith(A, split(B, ' ')[end])
    @test endswith(A, 'g')
end
