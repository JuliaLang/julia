# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "intset" begin

# Construction, collect
data_in = (1,5,100)
s = IntSet(data_in)
data_out = collect(s)
@test all(map(d->in(d,data_out), data_in))
@test length(data_out) == length(data_in)

# eltype, similar
@test is(eltype(IntSet()), Int64)
@test isequal(similar(IntSet([1,2,3])), IntSet())

# show
@test sprint(show, IntSet()) == "IntSet([])"
@test sprint(show, IntSet([1,2,3])) == "IntSet([1, 2, 3])"


s = IntSet([1,2,10,20,200,300,1000,10000,10002])
@test last(s) == 10002
@test first(s) == 1
@test length(s) == 9
@test pop!(s) == 10002
@test_throws KeyError pop!(s, -1)
@test length(s) == 8
@test shift!(s) == 1
@test length(s) == 7
@test !in(1,s)
@test !in(10002,s)
@test in(10000,s)
@test_throws ArgumentError first(IntSet())
@test_throws ArgumentError last(IntSet())
t = copy(s)
sizehint!(t, 20000) #check that hash does not depend on size of internal Array{UInt32, 1}
@test hash(s) == hash(t)

@test setdiff(IntSet([1, 2, 3, 4]), IntSet([2, 4, 5, 6])) == IntSet([1, 3])
@test symdiff(IntSet([1, 2, 3, 4]), IntSet([2, 4, 5, 6])) == IntSet([1, 3, 5, 6])

s2 = IntSet([1, 2, 3, 4])
setdiff!(s2, IntSet([2, 4, 5, 6]))

@test s2 == IntSet([1, 3])

# == with last-bit set (groups.google.com/forum/#!topic/julia-users/vZNjiIEG_sY)
s = IntSet(255)
@test s == s

# issue #7851
@test_throws ArgumentError IntSet(-1)
@test !(-1 in IntSet(1:10))

# # issue #8570
# This requires 2^29 bytes of storage, which is too much for a simple test
# s = IntSet(2^32)
# @test length(s) == 1
# for b in s; b; end

i = IntSet([1, 2, 3])

union!(i, [1, 2])
@test length(i) == 3
union!(i, [3, 4, 5])
@test length(i) == 5

@test_throws KeyError pop!(i, 10)

empty!(i)
@test length(i) == 0

@test_throws ArgumentError symdiff!(i, -3)
@test symdiff!(i, 3) == IntSet([3])
@test symdiff!(i, 257) == IntSet([3, 257])
@test symdiff!(i, [3, 6]) == IntSet([6, 257])

i = IntSet(1:6)
@test symdiff!(i, IntSet([6, 513])) == IntSet([1:5; 513])

i = IntSet([1, 2, 3])
k = IntSet([4, 5])
copy!(k, i)
@test k == i
@test !(k === i)
copy!(k, k)
@test k == i

# unions
i = IntSet([1, 2, 3])
j = union(i)
@test j == i
@test !(j === i)

j = IntSet([4, 5, 6])
@test union(i, j) == IntSet(1:6)

k = IntSet([7, 8, 9])
@test union(i, j, k) == IntSet(1:9)


## intersections
i = IntSet([1, 2, 3])
j = IntSet([4, 5, 6])

@test intersect(i) == i
@test !(intersect(i) === i)

@test intersect(i, j) == IntSet([])
push!(j, 257)
@test intersect(i, j) == IntSet([])
push!(j, 2, 3, 17)
@test intersect(i, j) == IntSet([2, 3])

## equality
i = IntSet([1, 2, 3])
j = IntSet([1, 2, 4])
@test i != j

push!(j, 257)
pop!(j, 257)
@test i != j
@test j != i

@test issubset(IntSet([1, 2, 4]), IntSet(1:10))
@test issubset(IntSet([]), IntSet([]))
@test IntSet([1, 2, 4]) < IntSet(1:10)
@test !(IntSet([]) < IntSet([]))
@test IntSet([1, 2, 4]) <= IntSet(1:10)
@test IntSet([1, 2, 4]) <= IntSet([1, 2, 4])
@test IntSet([]) <= IntSet([])

end
