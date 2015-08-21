# This file is a part of Julia. License is MIT: http://julialang.org/license

# padding (lpad and rpad)
@test lpad("foo", 3) == "foo"
@test rpad("foo", 3) == "foo"
@test lpad("foo", 5) == "  foo"
@test rpad("foo", 5) == "foo  "
@test lpad("foo", 5, "  ") == "  foo"
@test rpad("foo", 5, "  ") == "foo  "
@test lpad("foo", 6, "  ") == "   foo"
@test rpad("foo", 6, "  ") == "foo   "

# string manipulation
@test strip("") == ""
@test strip(" ") == ""
@test strip("  ") == ""
@test strip("   ") == ""
@test strip("\t  hi   \n") == "hi"
@test strip("foobarfoo", ['f','o']) == "bar"
@test strip("foobarfoo", ('f','o')) == "bar"

for s in ("", " ", " abc", "abc ", "  abc  "), f in (lstrip, rstrip, strip)
    fs = f(s)
    for T = (UTF8String, UTF16String, UTF32String)
        t = convert(T,s)
        ft = f(t)
        @test s == t
        @test fs == ft
        @test typeof(ft) == typeof(t[1:end])

        b = convert(SubString{T}, t)
        fb = f(b)
        @test s == b
        @test fs == fb
        @test typeof(fb) == SubString{T}
    end
end

# split
@test isequal(split("foo,bar,baz", 'x'), ["foo,bar,baz"])
@test isequal(split("foo,bar,baz", ','), ["foo","bar","baz"])
@test isequal(split("foo,bar,baz", ","), ["foo","bar","baz"])
@test isequal(split("foo,bar,baz", r","), ["foo","bar","baz"])
@test isequal(split("foo,bar,baz", ','; limit=0), ["foo","bar","baz"])
@test isequal(split("foo,bar,baz", ','; limit=1), ["foo,bar,baz"])
@test isequal(split("foo,bar,baz", ','; limit=2), ["foo","bar,baz"])
@test isequal(split("foo,bar,baz", ','; limit=3), ["foo","bar","baz"])
@test isequal(split("foo,bar", "o,b"), ["fo","ar"])

@test isequal(split("", ','), [""])
@test isequal(split(",", ','), ["",""])
@test isequal(split(",,", ','), ["","",""])
@test isequal(split("", ','  ; keep=false), [])
@test isequal(split(",", ',' ; keep=false), [])
@test isequal(split(",,", ','; keep=false), [])

@test isequal(split("a b c"), ["a","b","c"])
@test isequal(split("a  b \t c\n"), ["a","b","c"])

@test isequal(rsplit("foo,bar,baz", 'x'), ["foo,bar,baz"])
@test isequal(rsplit("foo,bar,baz", ','), ["foo","bar","baz"])
@test isequal(rsplit("foo,bar,baz", ","), ["foo","bar","baz"])
@test isequal(rsplit("foo,bar,baz", ','; limit=0), ["foo","bar","baz"])
@test isequal(rsplit("foo,bar,baz", ','; limit=1), ["foo,bar,baz"])
@test isequal(rsplit("foo,bar,baz", ','; limit=2), ["foo,bar","baz"])
@test isequal(rsplit("foo,bar,baz", ','; limit=3), ["foo","bar","baz"])
@test isequal(rsplit("foo,bar", "o,b"), ["fo","ar"])

@test isequal(rsplit("", ','), [""])
@test isequal(rsplit(",", ','), ["",""])
@test isequal(rsplit(",,", ','), ["","",""])
@test isequal(rsplit(",,", ','; limit=2), [",",""])
@test isequal(rsplit("", ','  ; keep=false), [])
@test isequal(rsplit(",", ',' ; keep=false), [])
@test isequal(rsplit(",,", ','; keep=false), [])

#@test isequal(rsplit("a b c"), ["a","b","c"])
#@test isequal(rsplit("a  b \t c\n"), ["a","b","c"])

let str = "a.:.ba..:..cba.:.:.dcba.:."
@test isequal(split(str, ".:."), ["a","ba.",".cba",":.dcba",""])
@test isequal(split(str, ".:."; keep=false), ["a","ba.",".cba",":.dcba"])
@test isequal(split(str, ".:."), ["a","ba.",".cba",":.dcba",""])
@test isequal(split(str, r"\.(:\.)+"), ["a","ba.",".cba","dcba",""])
@test isequal(split(str, r"\.(:\.)+"; keep=false), ["a","ba.",".cba","dcba"])
@test isequal(split(str, r"\.+:\.+"), ["a","ba","cba",":.dcba",""])
@test isequal(split(str, r"\.+:\.+"; keep=false), ["a","ba","cba",":.dcba"])

@test isequal(rsplit(str, ".:."), ["a","ba.",".cba.:","dcba",""])
@test isequal(rsplit(str, ".:."; keep=false), ["a","ba.",".cba.:","dcba"])
@test isequal(rsplit(str, ".:."; limit=2), ["a.:.ba..:..cba.:.:.dcba", ""])
@test isequal(rsplit(str, ".:."; limit=3), ["a.:.ba..:..cba.:", "dcba", ""])
@test isequal(rsplit(str, ".:."; limit=4), ["a.:.ba.", ".cba.:", "dcba", ""])
@test isequal(rsplit(str, ".:."; limit=5), ["a", "ba.", ".cba.:", "dcba", ""])
@test isequal(rsplit(str, ".:."; limit=6), ["a", "ba.", ".cba.:", "dcba", ""])
end

# zero-width splits
@test isequal(rsplit("", ""), [""])

@test isequal(split("", ""), [""])
@test isequal(split("", r""), [""])
@test isequal(split("abc", ""), ["a","b","c"])
@test isequal(split("abc", r""), ["a","b","c"])
@test isequal(split("abcd", r"b?"), ["a","c","d"])
@test isequal(split("abcd", r"b*"), ["a","c","d"])
@test isequal(split("abcd", r"b+"), ["a","cd"])
@test isequal(split("abcd", r"b?c?"), ["a","d"])
@test isequal(split("abcd", r"[bc]?"), ["a","","d"])
@test isequal(split("abcd", r"a*"), ["","b","c","d"])
@test isequal(split("abcd", r"a+"), ["","bcd"])
@test isequal(split("abcd", r"d*"), ["a","b","c",""])
@test isequal(split("abcd", r"d+"), ["abc",""])
@test isequal(split("abcd", r"[ad]?"), ["","b","c",""])

# replace
@test replace("\u2202", '*', '\0') == "\u2202"

@test replace("foobar", 'o', '0') == "f00bar"
@test replace("foobar", 'o', '0', 1) == "f0obar"
@test replace("foobar", 'o', "") == "fbar"
@test replace("foobar", 'o', "", 1) == "fobar"
@test replace("foobar", 'f', 'F') == "Foobar"
@test replace("foobar", 'r', 'R') == "foobaR"

@test replace("foofoofoo", "foo", "bar") == "barbarbar"
@test replace("foobarfoo", "foo", "baz") == "bazbarbaz"
@test replace("barfoofoo", "foo", "baz") == "barbazbaz"

@test replace("", "", "") == ""
@test replace("", "", "x") == "x"
@test replace("", "x", "y") == ""

@test replace("abcd", "", "^") == "^a^b^c^d^"
@test replace("abcd", "b", "^") == "a^cd"
@test replace("abcd", r"b?", "^") == "^a^c^d^"
@test replace("abcd", r"b+", "^") == "a^cd"
@test replace("abcd", r"b?c?", "^") == "^a^d^"
@test replace("abcd", r"[bc]?", "^") == "^a^^d^"

@test replace("foobarfoo", r"(fo|ba)", "xx") == "xxoxxrxxo"
@test replace("foobarfoo", r"(foo|ba)", "bar") == "barbarrbar"

@test replace("foobar", 'o', 'ø') == "føøbar"
@test replace("foobar", 'o', 'ø', 1) == "føobar"
@test replace("føøbar", 'ø', 'o') == "foobar"
@test replace("føøbar", 'ø', 'o', 1) == "foøbar"
@test replace("føøbar", 'ø', 'ö') == "fööbar"
@test replace("føøbar", 'ø', 'ö', 1) == "föøbar"
@test replace("føøbar", 'ø', "") == "fbar"
@test replace("føøbar", 'ø', "", 1) == "føbar"
@test replace("føøbar", 'f', 'F') == "Føøbar"
@test replace("ḟøøbar", 'ḟ', 'F') == "Føøbar"
@test replace("føøbar", 'f', 'Ḟ') == "Ḟøøbar"
@test replace("ḟøøbar", 'ḟ', 'Ḟ') == "Ḟøøbar"
@test replace("føøbar", 'r', 'R') == "føøbaR"
@test replace("føøbaṙ", 'ṙ', 'R') == "føøbaR"
@test replace("føøbar", 'r', 'Ṙ') == "føøbaṘ"
@test replace("føøbaṙ", 'ṙ', 'Ṙ') == "føøbaṘ"

@test replace("ḟøøḟøøḟøø", "ḟøø", "bar") == "barbarbar"
@test replace("ḟøøbarḟøø", "ḟøø", "baz") == "bazbarbaz"
@test replace("barḟøøḟøø", "ḟøø", "baz") == "barbazbaz"

@test replace("foofoofoo", "foo", "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
@test replace("fooƀäṙfoo", "foo", "baz") == "bazƀäṙbaz"
@test replace("ƀäṙfoofoo", "foo", "baz") == "ƀäṙbazbaz"

@test replace("foofoofoo", "foo", "bar") == "barbarbar"
@test replace("foobarfoo", "foo", "ƀäż") == "ƀäżbarƀäż"
@test replace("barfoofoo", "foo", "ƀäż") == "barƀäżƀäż"

@test replace("ḟøøḟøøḟøø", "ḟøø", "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
@test replace("ḟøøƀäṙḟøø", "ḟøø", "baz") == "bazƀäṙbaz"
@test replace("ƀäṙḟøøḟøø", "ḟøø", "baz") == "ƀäṙbazbaz"

@test replace("ḟøøḟøøḟøø", "ḟøø", "bar") == "barbarbar"
@test replace("ḟøøbarḟøø", "ḟøø", "ƀäż") == "ƀäżbarƀäż"
@test replace("barḟøøḟøø", "ḟøø", "ƀäż") == "barƀäżƀäż"

@test replace("ḟøøḟøøḟøø", "ḟøø", "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
@test replace("ḟøøƀäṙḟøø", "ḟøø", "ƀäż") == "ƀäżƀäṙƀäż"
@test replace("ƀäṙḟøøḟøø", "ḟøø", "ƀäż") == "ƀäṙƀäżƀäż"

@test replace("", "", "ẍ") == "ẍ"
@test replace("", "ẍ", "ÿ") == ""

@test replace("äƀçđ", "", "π") == "πäπƀπçπđπ"
@test replace("äƀçđ", "ƀ", "π") == "äπçđ"
@test replace("äƀçđ", r"ƀ?", "π") == "πäπçπđπ"
@test replace("äƀçđ", r"ƀ+", "π") == "äπçđ"
@test replace("äƀçđ", r"ƀ?ç?", "π") == "πäπđπ"
@test replace("äƀçđ", r"[ƀç]?", "π") == "πäππđπ"

@test replace("foobarfoo", r"(fo|ba)", "ẍẍ") == "ẍẍoẍẍrẍẍo"

@test replace("ḟøøbarḟøø", r"(ḟø|ba)", "xx") == "xxøxxrxxø"
@test replace("ḟøøbarḟøø", r"(ḟøø|ba)", "bar") == "barbarrbar"

@test replace("fooƀäṙfoo", r"(fo|ƀä)", "xx") == "xxoxxṙxxo"
@test replace("fooƀäṙfoo", r"(foo|ƀä)", "ƀäṙ") == "ƀäṙƀäṙṙƀäṙ"

@test replace("ḟøøƀäṙḟøø", r"(ḟø|ƀä)", "xx") == "xxøxxṙxxø"
@test replace("ḟøøƀäṙḟøø", r"(ḟøø|ƀä)", "ƀäṙ") == "ƀäṙƀäṙṙƀäṙ"

@test replace("foo", "oo", uppercase) == "fOO"

# Issue 13332
@test replace("abc", 'b', 2.1) == "a2.1c"

# chomp/chop
@test chomp("foo\n") == "foo"
@test chop("foob") == "foo"

# bytes2hex and hex2bytes
hex_str = "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
bin_val = hex2bytes(hex_str)

@test div(length(hex_str), 2) == length(bin_val)
@test hex_str == bytes2hex(bin_val)

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
