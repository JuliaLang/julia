
@test matchall(r"a?b?", "asbd") == ["a","","b","",""]
@test matchall(r"a?b?", "asbd", true) == ["a","","b","",""]
@test matchall(r"\w+", "hello", true) == ["hello","ello","llo","lo","o"]
@test matchall(r".\s", "x \u2200 x \u2203 y") == ["x ", "∀ ", "x ", "∃ "]
@test matchall(r"(\w+)(\s*)", "The dark side of the moon") ==
	  ["The ", "dark ", "side ", "of ", "the ", "moon"]

@test matchall(r"aa", "aaaa", true) == ["aa", "aa", "aa"]
@test matchall(r"", "aaa") == ["", "", "", ""]
@test matchall(r"", "aaa", true) == ["", "", "", ""]
