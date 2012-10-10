
@assert median([1.]) == 1.
@assert median([1.,3]) == 2.
@assert median([1.,3,2]) == 2.

# integer array
@assert median([1,3,2]) == 2

@assert mean([1,2,3]) == 2.
@assert var([1,2,3]) == 1.
@assert std([1,2,3]) == 1.
@assert hist([1,2,3],10) == [1,0,0,0,0,1,0,0,0,1]
@assert histc([1,2,3],[0,2,4]) == [1,2,0]

@assert quantile([1,2,3,4],0.5) == 2.5
@assert quartile([1., 3])[2] == median([1., 3])
@assert decile([0.:100.])[1] == 10.0
