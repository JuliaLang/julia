# This file is a part of Julia. License is MIT: http://julialang.org/license

# replace
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
