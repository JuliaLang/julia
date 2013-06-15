# tests for setdiff function

@test setdiff(IntSet(1, 2, 3, 4), IntSet(2, 4, 5, 6)) == IntSet(1, 3)
@test setdiff(Set(1, 2, 3, 4), Set(2, 4, 5, 6)) == Set(1, 3)

s1 = Set(1, 2, 3, 4)
setdiff!(s1, Set(2, 4, 5, 6))

@test s1 == Set(1, 3)

s2 = IntSet(1, 2, 3, 4)
setdiff!(s2, IntSet(2, 4, 5, 6))

@test s2 == IntSet(1, 3)
