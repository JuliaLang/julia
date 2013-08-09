
function collect_eachmatch(re, str, overlap=false)
    [m.match for m in collect(eachmatch(re, str, overlap))]
end

for f in [matchall, collect_eachmatch]
    @test f(r"a?b?", "asbd") == ["a","","b","",""]
    @test f(r"a?b?", "asbd", true) == ["a","","b","",""]
    @test f(r"\w+", "hello", true) == ["hello","ello","llo","lo","o"]
    @test f(r".\s", "x \u2200 x \u2203 y") == ["x ", "∀ ", "x ", "∃ "]
    @test f(r"(\w+)(\s*)", "The dark side of the moon") ==
          ["The ", "dark ", "side ", "of ", "the ", "moon"]
    @test f(r"", "") == [""]
    @test f(r"", "", true) == [""]
    @test f(r"aa", "aaaa") == ["aa", "aa"]
    @test f(r"aa", "aaaa", true) == ["aa", "aa", "aa"]
    @test f(r"", "aaa") == ["", "", "", ""]
    @test f(r"", "aaa", true) == ["", "", "", ""]
end
