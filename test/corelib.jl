# ranges
@assert size(10:1:0) == (0,)
@assert length(1:.2:2) == 6
@assert length(1.:.2:2.) == 6
@assert length(2:-.2:1) == 6
@assert length(2.:-.2:1.) == 6
@assert length(2:.2:1) == 0
@assert length(2.:.2:1.) == 0

@assert length(1:0) == 0
@assert length(0.0:-0.5) == 0
@assert length(1:2:0) == 0
L32 = linspace(int32(1), int32(4), 4)
L64 = linspace(int64(1), int64(4), 4)
@assert L32[1] == 1 && L64[1] == 1
@assert L32[2] == 2 && L64[2] == 2
@assert L32[3] == 3 && L64[3] == 3
@assert L32[4] == 4 && L64[4] == 4
lsp = linspace(1, 10, 6)
@assert isequal(lsp,[1,3,5,6,8,10])

r = [5:-1:1]
@assert r[1]==5
@assert r[2]==4
@assert r[3]==3
@assert r[4]==2
@assert r[5]==1

# comprehensions
X = [ i+2j for i=1:5, j=1:5 ]
@assert X[2,3] == 8
@assert X[4,5] == 14
@assert isequal(ones(2,3) * ones(2,3)', [3. 3.; 3. 3.])
@assert isequal([ [1,2] for i=1:2, : ], [1 2; 1 2])
# where element type is a Union. try to confuse type inference.
foo32_64(x) = (x<2) ? int32(x) : int64(x)
boo32_64() = [ foo32_64(i) for i=1:2 ]
let a36 = boo32_64()
    @assert a36[1]==1 && a36[2]==2
end
@assert isequal([1,2,3], [a for (a,b) in enumerate(2:4)])
@assert isequal([2,3,4], [b for (a,b) in enumerate(2:4)])

@assert (10.^[-1])[1] == 0.1
@assert (10.^[-1.])[1] == 0.1

# tricky space sensitive syntax cases
@assert [-1 ~1] == [(-1) (~1)]

# lexing typemin(Int64)
@assert_fails parse("9223372036854775808")
@assert_fails parse("-(9223372036854775808)")
@assert_fails parse("-9223372036854775808^1")
@assert (-9223372036854775808)^1 == -9223372036854775808
@assert [1 -1 -9223372036854775808] == [1 -1 typemin(Int64)]

# dict
h = Dict()
for i=1:10000
    h[i] = i+1
end
for i=1:10000
    @assert (h[i] == i+1)
end
for i=1:2:10000
    del(h, i)
end
for i=1:2:10000
    h[i] = i+1
end
for i=1:10000
    @assert (h[i] == i+1)
end
for i=1:10000
    del(h, i)
end
@assert isempty(h)
h[77] = 100
@assert h[77]==100
for i=1:10000
    h[i] = i+1
end
for i=1:2:10000
    del(h, i)
end
for i=10001:20000
    h[i] = i+1
end
for i=2:2:10000
    @assert h[i]==i+1
end
for i=10000:20000
    @assert h[i]==i+1
end
h = {"a" => 3}
@assert h["a"] == 3

let
    z = Dict()
    get_KeyError = false
    try
        z["a"]
    catch _e123_
        get_KeyError = isa(_e123_,KeyError)
    end
    @assert get_KeyError
end

_d = {"a"=>0}
@assert isa([k for k in filter(x->length(x)==1, keys(_d))], Vector{Any})

# issue #1438
type I1438T
    id
end
import Base.hash
hash(x::I1438T) = x.id

begin
    local seq, xs, s
    seq = [26,28,29,30,31,32,33,34,35,36,-32,-35,-34,-28,37,38,39,40,-30,
           -31,41,42,43,44,-33,-36,45,46,47,48,-37,-38,49,50,51,52,-46,-50,53]
    xs = [ I1438T(id) for id=1:53 ]
    s = Set()
    for id in seq
        if id > 0
            x = xs[id]
            add(s, x)
            @assert has(s, x)                 # check that x can be found
        else
            del(s, xs[-id])
        end
    end
end

# Helper for writing tests about error conditions
macro assert_raises(ExcType, expression)
    quote
        try
            $expression
        catch e
            if !isa(e, $ExcType)
                println("Error        : ",        e )
                println("Type of error: ", typeof(e))
                println("Expected     : ", $ExcType)
                @assert false
            end
        end
    end
end

@assert  isequal(Dict(), Dict())
@assert  isequal({1 => 1}, {1 => 1})
@assert !isequal({1 => 1}, {})
@assert !isequal({1 => 1}, {1 => 2})
@assert !isequal({1 => 1}, {2 => 1})

# Generate some data to populate dicts to be compared
data_in = [ (randi(1000), randstring(2)) for _ in 1:1001 ]

# Populate the first dict
d1 = Dict{Int, String}(length(data_in))
for (k,v) in data_in
    d1[k] = v
end
data_in = pairs(d1)
# shuffle the data
for i in 1:length(data_in)
    j = randi(length(data_in))
    data_in[i], data_in[j] = data_in[j], data_in[i]
end
# Inserting data in different (shuffled) order should result in
# equivalent dict.
d2 = Dict{Int, String}(length(data_in))
for (k,v) in data_in
    d2[k] = v
end

@assert  isequal(d1, d2)
d3 = copy(d2)
d4 = copy(d2)
# Removing an item gives different dict
del(d1, data_in[randi(length(data_in))][1])
@assert !isequal(d1, d2)
# Changing a value gives different dict
d3[data_in[randi(length(data_in))][1]] = randstring(3)
!isequal(d1, d3)
# Adding a pair gives different dict
d4[1001] = randstring(3)
@assert !isequal(d1, d4)

@assert isequal(Dict(), Dict(96))

# Here is what currently happens when dictionaries of different types
# are compared. This is not necessarily desirable. These tests are
# descriptive rather than proscriptive.
@assert_raises MethodError !isequal({1 => 2}, {"dog" => "bone"})
@assert isequal(Dict{Int, Int}(), Dict{String, String}())

# ############# end of dict tests #############

# #################### set ####################

# show

# isempty
@assert  isempty(Set())
@assert !isempty(Set(1))
@assert !isempty(Set("banana", "apple"))
@assert !isempty(Set(1, 1:10, "pear"))

# add, length
s = Set()
@assert isempty(s)
for i in 1:1000
    add(s, i)
    @assert (length(s) == i)
end

# del, has, contains
for i in 1:2:1000
    del(s, i)
end
for i in 1:2:1000
    @assert !has(s, i)
    @assert  has(s, i+1)
    @assert !contains(s, i)
    @assert  contains(s, i+1)
end

# elements
data_in = (1,"banana", ())
s = Set(data_in...)
data_out = elements(s)
@assert is(typeof(data_out), Array{Any,1})
@assert all(map(d->contains(data_out,d), data_in))
@assert all(map(data_in) do d contains(data_out, d) end)
@assert length(data_out) == length(data_in)

# homogeneous sets
@assert is(typeof(Set(1,2,3)), Set{Int})
@assert is(typeof(Set{Int}(1.0, 4//2, 3)), Set{Int})

# eltype
@assert is(eltype(Set(1,"hello")), Any)
@assert is(eltype(Set{String}()), String)

# no duplicates
s = Set(1,2,3)
@assert length(s) == 3
add(s,2)
@assert length(s) == 3
del(s,2)
@assert length(s) == 2

# get
@assert get(Set(1,2,3), 2, "ignored") == true
@assert get(Set(1,  3), 2, "ignored") == false

# union
s = union(Set(1,2), Set(3,4))
@assert isequal(s, Set(1,2,3,4))
s = union(Set(5,6,7,8), Set(7,8,9))
@assert isequal(s, Set(5,6,7,8,9))

# intersect
s = intersect(Set(1,2), Set(3,4))
@assert isequal(s, Set())
s = intersect(Set(5,6,7,8), Set(7,8,9))
@assert isequal(s, Set(7,8))

# setdiff
@assert isequal(setdiff(Set(1,2,3), Set()), Set(1,2,3))
@assert isequal(setdiff(Set(1,2,3), Set(1)),  Set(2,3))
@assert isequal(setdiff(Set(1,2,3), Set(1,2)),  Set(3))
@assert isequal(setdiff(Set(1,2,3), Set(1,2,3)), Set())
@assert isequal(setdiff(Set(1,2,3), Set(4)),  Set(1,2,3))
@assert isequal(setdiff(Set(1,2,3), Set(4,1)),  Set(2,3))

# |, &, -
for (operator, name) in ((|, union), (&, intersect), (-, setdiff))
    for (l,r) in ((Set(1,2),     Set(3,4)),
                  (Set(5,6,7,8), Set(7,8,9)),
                  (Set(1,2),     Set(3,4)),
                  (Set(5,6,7,8), Set(7,8,9)),
                  (Set(1,2,3),   Set()),
                  (Set(1,2,3),   Set(1)),
                  (Set(1,2,3),   Set(1,2)),
                  (Set(1,2,3),   Set(1,2,3)),
                  (Set(1,2,3),   Set(4)),
                  (Set(1,2,3),   Set(4,1)))
        @assert  isequal(operator(l, r), name(l, r))
    end
end
    
# add_each
s =                Set(1,  3,  5,  7)
add_each(s,           (  2,3,4,5    ))
@assert isequal(s, Set(1,2,3,4,5,  7))

# del_each
s =                Set(1,  3,  5,  7)
del_each(s,           (    3,  5    ))
@assert isequal(s, Set(1,          7))

# similar
s = similar(Set(1,"Banana"))
@assert length(s) == 0
@assert typeof(s) == Set{Any}
s = similar(Set{Float32}(2,3,4))
@assert length(s) == 0
@assert typeof(s) == Set{Float32}

# copy
data_in = (1,2,9,8,4)
s = Set(data_in...)
c = copy(s)
@assert isequal(s,c)
add(s,100)
add(c,200)
@assert !has(c, 100)
@assert !has(s, 200)

# start, done, next
for data_in in ((7,8,4,5),
                ("hello", 23, 2.7, (), [], (1,8)))
    s = Set(data_in...)

    s_new = Set()
    for el in s
        add(s_new, el)
    end
    @assert isequal(s, s_new)
    
    t = tuple(s...)
    @assert length(t) == length(s)
    for e in t
        @assert has(s,e)
    end
end

# isequal
@assert  isequal(Set(), Set())
@assert !isequal(Set(), Set(1))
@assert  isequal(Set{Any}(1,2), Set{Int}(1,2))
@assert !isequal(Set{Any}(1,2), Set{Int}(1,2,3))

# Comparison of unrelated types seems rather inconsistent

# Not sure whether the behaviour of isequal (as demonstrated below) on
# sets of unrelated types, is desirable, but it's what there is at the
# moment.
@assert                    isequal(Set{Int}(), Set{String}())
@assert_raises TypeError   isequal(Set{Int}(),    Set{String}{""})
@assert                   !isequal(Set{String}(), Set{Int}(0))
@assert_raises MethodError isequal(Set{Int}(1),   Set{String}())

@assert   isequal(Set{Any}(1,2,3), Set{Int}(1,2,3))
@assert   isequal(Set{Int}(1,2,3), Set{Any}(1,2,3))

@assert  !isequal(Set{Any}(1,2,3), Set{Int}(1,2,3,4))
@assert  !isequal(Set{Int}(1,2,3), Set{Any}(1,2,3,4))

@assert  !isequal(Set{Any}(1,2,3,4), Set{Int}(1,2,3))
@assert  !isequal(Set{Int}(1,2,3,4), Set{Any}(1,2,3))

# ########## end of set tests ##########
