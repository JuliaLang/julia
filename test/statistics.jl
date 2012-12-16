
@test median([1.]) == 1.
@test median([1.,3]) == 2.
@test median([1.,3,2]) == 2.

# integer array
@test median([1,3,2]) == 2

@test mean([1,2,3]) == 2.
@test var([1,2,3]) == 1.
@test std([1,2,3]) == 1.
@test hist([1,2,3],10) == [1,0,0,0,0,1,0,0,0,1]
@test histc([1,2,3],[0,2,4]) == [1,2,0]

@test quantile([1,2,3,4],0.5) == 2.5
@test quartile([1., 3])[2] == median([1., 3])
@test decile([0.:100.])[1] == 10.0
