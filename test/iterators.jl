require("iterators")

using Iterators

# aux function emulating a comprehension [x for x in f]
function buildvec(f)
    v = Any[]
    for i = f
        push!(v, i)
    end
    return v
end

macro test_buildvec(ex...)
    :(@test buildvec($(ex[1])) == $(ex[2]))
end

@test_buildvec take!(count(), 0)           []
@test_buildvec take!(count(), 5)           [0:4]
@test_buildvec take!(count(1,3), 5)        [1:3:13]
@test_buildvec take!(count(zeros(2,2)), 3) [i * eye(2) for i=0:2]

@test_buildvec drop(1:5, 5)  []
@test_buildvec drop(1:0, 0)  []
@test_buildvec drop(1:5, 2)  [3:5]

@test_buildvec cycle(1:0)           []
@test_buildvec take!(cycle(1:3), 8)  [1:3,1:3,1:2]

@test_buildvec repeat('a', 0)        []
@test_buildvec repeat('a', 5)        ['a' for i = 1:5]
@test_buildvec take!(repeat('a'), 5)  ['a' for i = 1:5]

@test_buildvec chain()               []
@test_buildvec chain(1:0)            []
@test_buildvec chain(4:8, 1:1)       [4:8, 1:1]
@test_buildvec chain(1:1, 4:8)       [1:1, 4:8]
@test_buildvec chain(1:3, 2:4, 1:0)  [1:3, 2:4, 1:0]

# matrix to vector
m2v(m) = reshape(m, length(m))

@test_buildvec product()              []
@test_buildvec product(1:2,1:0,1:1)   []
@test_buildvec product(1:2,1:1)       m2v([(i,j)   for i=1:2, j=1:1])
@test_buildvec product(1:1,1:3)       m2v([(i,j)   for i=1:1, j=1:3])
@test_buildvec product(2:4,1:3,4:8)   m2v([(i,j,k) for i=2:4, j=1:3, k=4:8])

# test isempty on iterators (#5827):
@test !isempty(combinations([1,2,3],0)) && isempty(combinations([],1))
