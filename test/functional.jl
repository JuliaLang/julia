# This file is a part of Julia. License is MIT: http://julialang.org/license

# tests related to functional programming functions and styles

# map -- array.jl
@test isequal(map((x)->"$x"[end:end], 9:11), ["9", "0", "1"])
# TODO: @test map!() much more thoroughly
let a = [1.0, 2.0]
    map!(sin, a, a)
    @test isequal(a, sin.([1.0, 2.0]))
end
# map -- ranges.jl
@test isequal(map(sqrt, 1:5), [sqrt(i) for i in 1:5])
@test isequal(map(sqrt, 2:6), [sqrt(i) for i in 2:6])

# map on ranges should evaluate first value only once (#4453)
let io=IOBuffer(3)
    map(x->print(io,x), 1:2)
    @test String(take!(io))=="12"
end

# map over Bottom[] should return Bottom[]
# issue #6719
@test isequal(typeof(map(x -> x, Array{Union{}}(0))), Array{Union{},1})

# maps of tuples (formerly in test/core.jl) -- tuple.jl
@test map((x,y)->x+y,(1,2,3),(4,5,6)) == (5,7,9)
@test map((x,y)->x+y,
            (100001,100002,100003),
            (100004,100005,100006)) == (200005,200007,200009)

# maps of strings (character arrays) -- string.jl
@test map((c)->Char(c+1), "abcDEF") == "bcdEFG"

# issue #10633
@test isa(map(Integer, Any[1, 2]), Vector{Int})
@test isa(map(Integer, Any[]), Vector{Integer})

# filter -- array.jl
@test isequal(filter(x->(x>1), [0 1 2 3 2 1 0]), [2, 3, 2])
# TODO: @test_throws isequal(filter(x->x+1, [0 1 2 3 2 1 0]), [2, 3, 2])
@test isequal(filter(x->(x>10), [0 1 2 3 2 1 0]), [])
@test isequal(filter((ss)->length(ss)==3, ["abcd", "efg", "hij", "klmn", "opq"]), ["efg", "hij", "opq"])

# numbers
@test size(collect(1)) == size(1)

@test isa(collect(Any, [1,2]), Vector{Any})

# foreach
let a = []
    foreach(()->push!(a,0))
    @test a == [0]
    a = []
    foreach(x->push!(a,x), [1,5,10])
    @test a == [1,5,10]
    a = []
    foreach((args...)->push!(a,args), [2,4,6], [10,20,30])
    @test a == [(2,10),(4,20),(6,30)]
end

# generators (#4470, #14848)
@test sum(i/2 for i=1:2) == 1.5
@test collect(2i for i=2:5) == [4,6,8,10]
@test collect((i+10j for i=1:2,j=3:4)) == [31 41; 32 42]
@test collect((i+10j for i=1:2,j=3:4,k=1:1)) == reshape([31 41; 32 42], (2,2,1))

let A = collect(Base.Generator(x->2x, Real[1.5,2.5]))
    @test A == [3,5]
    @test isa(A,Vector{Float64})
end

let f(g) = (@test size(g.iter)==(2,3))
    f(i+j for i=1:2, j=3:5)
end

@test collect(Base.Generator(+, [1,2], [10,20])) == [11,22]

# generator ndims #16394
let gens_dims = [((i for i = 1:5),                    1),
                 ((i for i = 1:5, j = 1:5),           2),
                 ((i for i = 1:5, j = 1:5, k = 1:5),  3),
                 ((i for i = Array{Int}()),           0),
                 ((i for i = Array{Int}(1)),          1),
                 ((i for i = Array{Int}(1, 2)),       2),
                 ((i for i = Array{Int}(1, 2, 3)),    3)]
    for (gen, dim) in gens_dims
        @test ndims(gen) == ndims(collect(gen)) == dim
    end
end

# generator with destructuring
let d = Dict(:a=>1, :b=>2), a = Dict(3=>4, 5=>6)
    @test Dict( v=>(k,) for (k,v) in d) == Dict(2=>(:b,), 1=>(:a,))
    @test Dict( (x,b)=>(c,y) for (x,c) in d, (b,y) in a ) == Dict((:a,5)=>(1,6),(:b,5)=>(2,6),(:a,3)=>(1,4),(:b,3)=>(2,4))
end

let i = 1
    local g = (i+j for i=2:2, j=3:3)
    @test first(g) == 5
    @test i == 1
end

# generators and guards
let gen = (x for x in 1:10)
    @test gen.iter == 1:10
    @test gen.f(first(1:10)) == next(gen, start(gen))[1]
    for (a,b) in zip(1:10,gen)
        @test a == b
    end
end

let gen = (x * y for x in 1:10, y in 1:10)
    @test collect(gen) == collect(1:10) .* collect(1:10)'
    @test first(gen) == 1
    @test collect(gen)[1:10] == collect(1:10)
end

let gen = Base.Generator(+, 1:10, 1:10, 1:10)
    @test first(gen) == 3
    @test collect(gen) == collect(3:3:30)
end

let gen = (x for x in 1:10 if x % 2 == 0), gen2 = Iterators.filter(x->x % 2 == 0, x for x in 1:10)
    @test collect(gen) == collect(gen2)
    @test collect(gen) == collect(2:2:10)
end

let gen = ((x,y) for x in 1:10, y in 1:10 if x % 2 == 0 && y % 2 == 0),
    gen2 = Iterators.filter(x->x[1] % 2 == 0 && x[2] % 2 == 0, (x,y) for x in 1:10, y in 1:10)
    @test collect(gen) == collect(gen2)
end

# generators with nested loops (#4867)
@test [(i,j) for i=1:3 for j=1:i] == [(1,1), (2,1), (2,2), (3,1), (3,2), (3,3)]

@test [(i,j) for i=1:3 for j=1:i if j>1] == [(2,2), (3,2), (3,3)]

# issue #18707
@test [(q,d,n,p) for q = 0:25:100
                 for d = 0:10:100-q
                 for n = 0:5:100-q-d
                 for p = 100-q-d-n
                 if p < n < d < q] == [(50,30,15,5), (50,30,20,0), (50,40,10,0), (75,20,5,0)]
