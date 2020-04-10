# This file is a part of Julia. License is MIT: https://julialang.org/license

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

    # PR 35414
    @test replace("foobarbaz","oo"=>"zz","ar"=>"zz","z"=>"m") == "fzzbzzbam"
    substmp=["z"=>"m","oo"=>"zz","ar"=>"zz"]
    for perm in [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[1,3,2],[3,1,2]]
        @test replace("foobarbaz",substmp[perm]...) == "fzzbzzbam"
        @test replace("foobarbaz",substmp[perm]...,count=2) == "fzzbzzbaz"
        @test replace("foobarbaz",substmp[perm]...,count=1) == "fzzbarbaz"
    end
    @test replace("foobarbaz","z"=>"m",r"a.*a"=>uppercase) == "foobARBAm"
    @test replace("foobarbaz",'o'=>'z','a'=>'q','z'=>'m') == "fzzbqrbqm"
end

@testset "chomp/chop" begin
    @test chomp("foo\n") == "foo"
    @test chomp("fo∀\n") == "fo∀"
    @test chomp("foo\r\n") == "foo"
    @test chomp("fo∀\r\n") == "fo∀"
    @test chomp("fo∀") == "fo∀"
    @test chop("") == ""
    @test chop("fooε") == "foo"
    @test chop("foεo") == "foε"
    @test chop("∃∃∃∃") == "∃∃∃"
    @test chop("∀ϵ∃Δ", head=0, tail=0) == "∀ϵ∃Δ"
    @test chop("∀ϵ∃Δ", head=0, tail=1) == "∀ϵ∃"
    @test chop("∀ϵ∃Δ", head=0, tail=2) == "∀ϵ"
    @test chop("∀ϵ∃Δ", head=0, tail=3) == "∀"
    @test chop("∀ϵ∃Δ", head=0, tail=4) == ""
    @test chop("∀ϵ∃Δ", head=0, tail=5) == ""
    @test chop("∀ϵ∃Δ", head=1, tail=0) == "ϵ∃Δ"
    @test chop("∀ϵ∃Δ", head=2, tail=0) == "∃Δ"
    @test chop("∀ϵ∃Δ", head=3, tail=0) == "Δ"
    @test chop("∀ϵ∃Δ", head=4, tail=0) == ""
    @test chop("∀ϵ∃Δ", head=5, tail=0) == ""
    @test chop("∀ϵ∃Δ", head=1, tail=1) == "ϵ∃"
    @test chop("∀ϵ∃Δ", head=2, tail=2) == ""
    @test chop("∀ϵ∃Δ", head=3, tail=3) == ""
    @test_throws ArgumentError chop("∀ϵ∃Δ", head=-3, tail=3)
    @test_throws ArgumentError chop("∀ϵ∃Δ", head=3, tail=-3)
    @test_throws ArgumentError chop("∀ϵ∃Δ", head=-3, tail=-3)

    @test isa(chomp("foo"), SubString)
    @test isa(chop("foo"), SubString)
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
end

# b"" should be immutable
let testb() = b"0123"
    b = testb()
    @test eltype(b) === UInt8
    @test b isa AbstractVector
    @test_throws ErrorException b[4] = '4'
    @test testb() == UInt8['0','1','2','3']
end
