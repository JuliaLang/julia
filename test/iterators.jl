require("extras/iterators")
using Iterators

# aux function emulating a comprehension [x for x in f]
function buildvec(f)
    v = Any[]
    for i = f
        push(v, i)
    end
    return v
end

macro assert_buildvec(ex...)
    :(@test buildvec($(ex[1])) == $(ex[2]))
end

@assert_buildvec take(count(), 0)           []
@assert_buildvec take(count(), 5)           [0:4]
@assert_buildvec take(count(1,3), 5)        [1:3:13]
@assert_buildvec take(count(zeros(2,2)), 3) [i * eye(2) for i=0:2]

@assert_buildvec drop(1:5, 5)  []
@assert_buildvec drop(1:0, 0)  []
@assert_buildvec drop(1:5, 2)  [3:5]

@assert_buildvec cycle(1:0)           []
@assert_buildvec take(cycle(1:3), 8)  [1:3,1:3,1:2]

@assert_buildvec repeat('a', 0)        []
@assert_buildvec repeat('a', 5)        ['a' for i = 1:5]
@assert_buildvec take(repeat('a'), 5)  ['a' for i = 1:5]

@assert_buildvec chain()               []
@assert_buildvec chain(1:0)            []
@assert_buildvec chain(4:8, 1:1)       [4:8, 1:1]
@assert_buildvec chain(1:1, 4:8)       [1:1, 4:8]
@assert_buildvec chain(1:3, 2:4, 1:0)  [1:3, 2:4, 1:0]

# matrix to vector
m2v(m) = reshape(m, length(m))

@assert_buildvec product()              []
@assert_buildvec product(1:2,1:0,1:1)   []
@assert_buildvec product(1:2,1:1)       m2v([(i,j)   for i=1:2, j=1:1])
@assert_buildvec product(1:1,1:3)       m2v([(i,j)   for i=1:1, j=1:3])
@assert_buildvec product(2:4,1:3,4:8)   m2v([(i,j,k) for i=2:4, j=1:3, k=4:8])
