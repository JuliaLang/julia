
str = "The dark side of the moon"
matches = match_all(r"(\w+)(\s*)", str)

@test size(matches, 1) == 6
@test matches[1].match == "The "
@test matches[2].match == "dark "
@test matches[3].match == "side "
@test matches[4].match == "of "
@test matches[5].match == "the "
@test matches[6].match == "moon"

s = "x \u2200 x \u2203 y"
matches = match_all(r".\s", s)

@test size(matches, 1) == 4
@test matches[1].match == "x "
@test matches[2].match == "∀ "
@test matches[3].match == "x "
@test matches[4].match == "∃ "

r = r"a?b?"
s = "asbd"
matches = match_all(r, s)

@test size(matches, 1) == 5
@test matches[1].match == "a"
@test matches[2].match == ""
@test matches[3].match == "b"
@test matches[4].match == ""
@test matches[5].match == ""
