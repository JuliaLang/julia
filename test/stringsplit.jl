# This file is a part of Julia. License is MIT: http://julialang.org/license

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
