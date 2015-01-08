# Set tests
# isempty
@test  isempty(Set())
@test !isempty(Set([1]))
@test !isempty(Set(["banana", "apple"]))
@test !isempty(Set([1, 1:10, "pear"]))

# ordering
@test Set() < Set([1])
@test Set([1]) < Set([1,2])
@test !(Set([3]) < Set([1,2]))
@test !(Set([3]) > Set([1,2]))
@test Set([1,2,3]) > Set([1,2])
@test !(Set([3]) <= Set([1,2]))
@test !(Set([3]) >= Set([1,2]))
@test Set([1]) <= Set([1,2])
@test Set([1,2]) <= Set([1,2])
@test Set([1,2]) >= Set([1,2])
@test Set([1,2,3]) >= Set([1,2])
@test !(Set([1,2,3]) >= Set([1,2,4]))
@test !(Set([1,2,3]) <= Set([1,2,4]))

# add, length
s = Set()
@test isempty(s)
for i in 1:1000
    push!(s, i)
    @test (length(s) == i)
end

# delete!, has, in
for i in 1:2:1000
    delete!(s, i)
end
for i in 1:2:1000
    @test !in(i  , s)
    @test  in(i+1, s)
end

# elements
data_in = (1,"banana", ())
s = Set(data_in)
data_out = collect(s)
@test is(typeof(data_out), Array{Any,1})
@test all(map(d->in(d,data_out), data_in))
@test all(map(data_in) do d
              in(d,data_out)
          end)
@test length(data_out) == length(data_in)

# homogeneous sets
@test is(typeof(Set([1,2,3])), Set{Int})
@test is(typeof(Set{Int}([3])), Set{Int})

# eltype
@test is(eltype(Set([1,"hello"])), Any)
@test is(eltype(Set{AbstractString}()), AbstractString)

# no duplicates
s = Set([1,2,3])
@test length(s) == 3
push!(s,2)
@test length(s) == 3
delete!(s,2)
@test length(s) == 2
push!(s,2)
@test length(s) == 3
pop!(s,2)
@test length(s) == 2
pop!(s)
@test length(s) == 1

# union
s = union(Set([1,2]), Set([3,4]))
@test isequal(s, Set([1,2,3,4]))
s = union(Set([5,6,7,8]), Set([7,8,9]))
@test isequal(s, Set([5,6,7,8,9]))

# intersect
s = intersect(Set([1,2]), Set([3,4]))
@test isequal(s, Set())
s = intersect(Set([5,6,7,8]), Set([7,8,9]))
@test isequal(s, Set([7,8]))
@test isequal(intersect(Set([2,3,1]), Set([4,2,3]), Set([5,4,3,2])), Set([2,3]))

# setdiff
@test isequal(setdiff(Set([1,2,3]), Set()), Set([1,2,3]))
@test isequal(setdiff(Set([1,2,3]), Set([1])),  Set([2,3]))
@test isequal(setdiff(Set([1,2,3]), Set([1,2])),  Set([3]))
@test isequal(setdiff(Set([1,2,3]), Set([1,2,3])), Set())
@test isequal(setdiff(Set([1,2,3]), Set([4])),  Set([1,2,3]))
@test isequal(setdiff(Set([1,2,3]), Set([4,1])),  Set([2,3]))

for (l,r) in ((Set([1,2]),     Set([3,4])),
              (Set([5,6,7,8]), Set([7,8,9])),
              (Set([1,2]),     Set([3,4])),
              (Set([5,6,7,8]), Set([7,8,9])),
              (Set([1,2,3]),   Set()),
              (Set([1,2,3]),   Set([1])),
              (Set([1,2,3]),   Set([1,2])),
              (Set([1,2,3]),   Set([1,2,3])),
              (Set([1,2,3]),   Set([4])),
              (Set([1,2,3]),   Set([4,1])))
    @test issubset(intersect(l,r), l)
    @test issubset(intersect(l,r), r)
    @test issubset(l, union(l,r))
    @test issubset(r, union(l,r))
    @test isequal(union(intersect(l,r),symdiff(l,r)), union(l,r))
end

@test setdiff(IntSet([1, 2, 3, 4]), IntSet([2, 4, 5, 6])) == IntSet([1, 3])
@test setdiff(Set([1, 2, 3, 4]), Set([2, 4, 5, 6])) == Set([1, 3])

@test symdiff(IntSet([1, 2, 3, 4]), IntSet([2, 4, 5, 6])) == IntSet([1, 3, 5, 6])
@test symdiff(Set([1, 2, 3, 4]), Set([2, 4, 5, 6])) == Set([1, 3, 5, 6])

s1 = Set([1, 2, 3, 4])
setdiff!(s1, Set([2, 4, 5, 6]))

@test s1 == Set([1, 3])

s2 = IntSet([1, 2, 3, 4])
setdiff!(s2, IntSet([2, 4, 5, 6]))

@test s2 == IntSet([1, 3])

# issue #7851
@test_throws ArgumentError IntSet(-1)
@test !(-1 in IntSet(0:10))

# union!
s = Set([1,3,5,7])
union!(s,(2,3,4,5))
@test isequal(s,Set([1,2,3,4,5,7]))

# setdiff!
s = Set([1,3,5,7])
setdiff!(s,(3,5))
@test isequal(s,Set([1,7]))

# similar
s = similar(Set([1,"Banana"]))
@test length(s) == 0
@test typeof(s) == Set{Any}
s = similar(Set{Float32}([2.0f0,3.0f0,4.0f0]))
@test length(s) == 0
@test typeof(s) == Set{Float32}

# copy
data_in = (1,2,9,8,4)
s = Set(data_in)
c = copy(s)
@test isequal(s,c)
push!(s,100)
push!(c,200)
@test !in(100, c)
@test !in(200, s)

# start, done, next
for data_in in ((7,8,4,5),
                ("hello", 23, 2.7, (), [], (1,8)))
    s = Set(data_in)

    s_new = Set()
    for el in s
        push!(s_new, el)
    end
    @test isequal(s, s_new)

    t = tuple(s...)
    @test length(t) == length(s)
    for e in t
        @test in(e,s)
    end
end

# zip
let i = 0
x = 1:2:8
y = 2:2:8
xy = 1:8
for (thisx, thisy) in zip(x, y)
    @test thisx == xy[i+=1]
    @test thisy == xy[i+=1]
end
end

# pop!
origs = Set([1,2,3,"apple"])
s = copy(origs)
for i in 1:length(origs)
    el = pop!(s)
    @test !in(el, s)
    @test in(el, origs)
end
@test isempty(s)

# hash
s1 = Set{ASCIIString}(["bar", "foo"])
s2 = Set{ASCIIString}(["foo", "bar"])
s3 = Set{ASCIIString}(["baz"])
@test hash(s1) == hash(s2)
@test hash(s1) != hash(s3)


# isequal
@test  isequal(Set(), Set())
@test !isequal(Set(), Set([1]))
@test  isequal(Set{Any}(Any[1,2]), Set{Int}([1,2]))
@test !isequal(Set{Any}(Any[1,2]), Set{Int}([1,2,3]))

# Comparison of unrelated types seems rather inconsistent

@test  isequal(Set{Int}(), Set{AbstractString}())
@test !isequal(Set{Int}(), Set{AbstractString}([""]))
@test !isequal(Set{AbstractString}(), Set{Int}([0]))
@test !isequal(Set{Int}([1]), Set{AbstractString}())

@test  isequal(Set{Any}([1,2,3]), Set{Int}([1,2,3]))
@test  isequal(Set{Int}([1,2,3]), Set{Any}([1,2,3]))

@test !isequal(Set{Any}([1,2,3]), Set{Int}([1,2,3,4]))
@test !isequal(Set{Int}([1,2,3]), Set{Any}([1,2,3,4]))

@test !isequal(Set{Any}([1,2,3,4]), Set{Int}([1,2,3]))
@test !isequal(Set{Int}([1,2,3,4]), Set{Any}([1,2,3]))

@test_throws ErrorException first(Set())
@test first(Set(2)) == 2

# ########## end of set tests ##########

## IntSet

s = IntSet([0,1,10,20,200,300,1000,10000,10002])
@test last(s) == 10002
@test first(s) == 0
@test length(s) == 9
@test pop!(s) == 10002
@test length(s) == 8
@test shift!(s) == 0
@test length(s) == 7
@test !in(0,s)
@test !in(10002,s)
@test in(10000,s)
@test_throws ErrorException first(IntSet())
@test_throws ErrorException last(IntSet())
t = copy(s)
sizehint!(t, 20000) #check that hash does not depend on size of internal Array{UInt32, 1}
@test hash(s) == hash(t)
@test hash(complement(s)) == hash(complement(t))

# issue #8570
s = IntSet(2^32)
@test length(s) == 1
for b in s; b; end