s = speye(3)
o = ones(3)
@assert s * s == s
@assert s \ o == o
@assert [s s] == sparse([1, 2, 3, 1, 2, 3], [1, 2, 3, 4, 5, 6], ones(6))
@assert [s; s] == sparse([1, 4, 2, 5, 3, 6], [1, 1, 2, 2, 3, 3], ones(6))
