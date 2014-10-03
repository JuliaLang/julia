s = IntSet(2^32)

@test length(s) == 1
for b in s; b; end # issue #8570
