# dict
h = Dict()
for i=1:10000
    h[i] = i+1
end
for i=1:10000
    @test (h[i] == i+1)
end
for i=1:2:10000
    delete!(h, i)
end
for i=1:2:10000
    h[i] = i+1
end
for i=1:10000
    @test (h[i] == i+1)
end
for i=1:10000
    delete!(h, i)
end
@test isempty(h)
h[77] = 100
@test h[77]==100
for i=1:10000
    h[i] = i+1
end
for i=1:2:10000
    delete!(h, i)
end
for i=10001:20000
    h[i] = i+1
end
for i=2:2:10000
    @test h[i]==i+1
end
for i=10000:20000
    @test h[i]==i+1
end
h = Dict{Any,Any}("a" => 3)
@test h["a"] == 3
h["a","b"] = 4
@test h["a","b"] == h[("a","b")] == 4
h["a","b","c"] = 4
@test h["a","b","c"] == h[("a","b","c")] == 4

let
    z = Dict()
    get_KeyError = false
    try
        z["a"]
    catch _e123_
        get_KeyError = isa(_e123_,KeyError)
    end
    @test get_KeyError
end

_d = Dict("a"=>0)
@test isa([k for k in filter(x->length(x)==1, collect(keys(_d)))], Vector{Any})

let
    d = Dict(((1, 2), (3, 4)))
    @test d[1] === 2
    @test d[3] === 4
    d2 = Dict(1 => 2, 3 => 4)
    d3 = Dict((1 => 2, 3 => 4))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Int,Int}

    d = Dict(((1, 2), (3, "b")))
    @test d[1] === 2
    @test d[3] == "b"
    d2 = Dict(1 => 2, 3 => "b")
    d3 = Dict((1 => 2, 3 => "b"))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Int,Any}

    d = Dict(((1, 2), ("a", 4)))
    @test d[1] === 2
    @test d["a"] === 4
    d2 = Dict(1 => 2, "a" => 4)
    d3 = Dict((1 => 2, "a" => 4))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Any,Int}

    d = Dict(((1, 2), ("a", "b")))
    @test d[1] === 2
    @test d["a"] == "b"
    d2 = Dict(1 => 2, "a" => "b")
    d3 = Dict((1 => 2, "a" => "b"))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Any,Any}
end

# issue #1821
let
    d = Dict{UTF8String, Vector{Int}}()
    d["a"] = [1, 2]
    @test_throws MethodError d["b"] = 1
    @test isa(repr(d), AbstractString)  # check that printable without error
end

# issue #2344
let
    local bar
    bestkey(d, key) = key
    bestkey{K<:AbstractString,V}(d::Associative{K,V}, key) = string(key)
    bar(x) = bestkey(x, :y)
    @test bar(Dict(:x => [1,2,5])) == :y
    @test bar(Dict("x" => [1,2,5])) == "y"
end

# issue #1438
type I1438T
    id
end
import Base.hash
hash(x::I1438T, h::UInt) = hash(x.id, h)

begin
    local seq, xs, s
    seq = [26,28,29,30,31,32,33,34,35,36,-32,-35,-34,-28,37,38,39,40,-30,
           -31,41,42,43,44,-33,-36,45,46,47,48,-37,-38,49,50,51,52,-46,-50,53]
    xs = [ I1438T(id) for id=1:53 ]
    s = Set()
    for id in seq
        if id > 0
            x = xs[id]
            push!(s, x)
            @test in(x, s)                 # check that x can be found
        else
            delete!(s, xs[-id])
        end
    end
end

@test  isequal(Dict(), Dict())
@test  isequal(Dict(1 => 1), Dict(1 => 1))
@test !isequal(Dict(1 => 1), Dict())
@test !isequal(Dict(1 => 1), Dict(1 => 2))
@test !isequal(Dict(1 => 1), Dict(2 => 1))

# Generate some data to populate dicts to be compared
data_in = [ (rand(1:1000), randstring(2)) for _ in 1:1001 ]

# Populate the first dict
d1 = Dict{Int, AbstractString}()
for (k,v) in data_in
    d1[k] = v
end
data_in = collect(d1)
# shuffle the data
for i in 1:length(data_in)
    j = rand(1:length(data_in))
    data_in[i], data_in[j] = data_in[j], data_in[i]
end
# Inserting data in different (shuffled) order should result in
# equivalent dict.
d2 = Dict{Int, AbstractString}()
for (k,v) in data_in
    d2[k] = v
end

@test  isequal(d1, d2)
d3 = copy(d2)
d4 = copy(d2)
# Removing an item gives different dict
delete!(d1, data_in[rand(1:length(data_in))][1])
@test !isequal(d1, d2)
# Changing a value gives different dict
d3[data_in[rand(1:length(data_in))][1]] = randstring(3)
!isequal(d1, d3)
# Adding a pair gives different dict
d4[1001] = randstring(3)
@test !isequal(d1, d4)

@test isequal(Dict(), sizehint!(Dict(),96))

# Here is what currently happens when dictionaries of different types
# are compared. This is not necessarily desirable. These tests are
# descriptive rather than proscriptive.
@test !isequal(Dict(1 => 2), Dict("dog" => "bone"))
@test isequal(Dict{Int,Int}(), Dict{AbstractString,AbstractString}())

# get! (get with default values assigned to the given location)

let f(x) = x^2, d = Dict(8=>19)

    @test get!(d, 8, 5) == 19
    @test get!(d, 19, 2) == 2

    @test get!(d, 42) do  # d is updated with f(2)
        f(2)
    end == 4

    @test get!(d, 42) do  # d is not updated
        f(200)
    end == 4

    @test get(d, 13) do   # d is not updated
        f(4)
    end == 16

    @test d == Dict(8=>19, 19=>2, 42=>4)
end

# show
for d in (Dict("\n" => "\n", "1" => "\n", "\n" => "2"),
          [string(i) => i for i = 1:30],
          [reshape(1:i^2,i,i) => reshape(1:i^2,i,i) for i = 1:24],
          [utf8(Char['α':'α'+i]) => utf8(Char['α':'α'+i]) for i = (1:10)*10],
          Dict("key" => zeros(0, 0)))
    for cols in (12, 40, 80), rows in (2, 10, 24)
        # Ensure output is limited as requested
        s = IOBuffer()
        Base.showdict(s, d, limit=true, sz=(rows, cols))
        out = split(takebuf_string(s),'\n')
        for line in out[2:end]
            @test strwidth(line) <= cols
        end
        @test length(out) <= rows

        for f in (keys, values)
            s = IOBuffer()
            Base.showkv(s, f(d), limit=true, sz=(rows, cols))
            out = split(takebuf_string(s),'\n')
            for line in out[2:end]
                @test strwidth(line) <= cols
            end
            @test length(out) <= rows
        end
    end
    # Simply ensure these do not throw errors
    Base.showdict(IOBuffer(), d, limit=false)
    @test !isempty(summary(d))
    @test !isempty(summary(keys(d)))
    @test !isempty(summary(values(d)))
end

# issue #9463
type Alpha end
Base.show(io::IO, ::Alpha) = print(io,"α")
sbuff = IOBuffer()
Base.showdict(sbuff, Dict(Alpha()=>1), limit=true, sz=(10,20))
@test !contains(bytestring(sbuff), "…")

# issue #2540
d = Dict{Any,Any}([x => 1 for x in ['a', 'b', 'c']])
@test d == Dict('a'=>1, 'b'=>1, 'c'=> 1)

# issue #2629
d = Dict{AbstractString,AbstractString}([ a => "foo" for a in ["a","b","c"]])
@test d == Dict("a"=>"foo","b"=>"foo","c"=>"foo")

# issue #5886
d5886 = Dict()
for k5886 in 1:11
   d5886[k5886] = 1
end
for k5886 in keys(d5886)
   # undefined ref if not fixed
   d5886[k5886] += 1
end

# issue #8877
let
    a = Dict("foo"  => 0.0, "bar" => 42.0)
    b = Dict("フー" => 17, "バー" => 4711)
    @test is(typeof(merge(a, b)), Dict{UTF8String,Float64})
end

# ############# end of dict tests #############

# #################### set ####################

# show

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

# Ranges

@test isempty((1:4)[5:4])
@test_throws BoundsError (1:10)[8:-1:-2]
