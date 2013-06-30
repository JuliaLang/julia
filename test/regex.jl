matches(args...) = map(m->m.match,matchall(args...))

@test matches(r"a?b?", "asbd") == ["a","","b","",""]
@test matches(r"\w+", "hello", true) == ["hello","ello","llo","lo","o"]
@test matches(r".\s", "x \u2200 x \u2203 y") == ["x ", "∀ ", "x ", "∃ "]
@test matches(r"(\w+)(\s*)", "The dark side of the moon") ==
	  ["The ", "dark ", "side ", "of ", "the ", "moon"]
