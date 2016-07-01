# This file is a part of Julia. License is MIT: http://julialang.org/license

# tests related to functional programming functions and styles

# map -- array.jl
@test isequal(map((x)->"$x"[end:end], 9:11), ["9", "0", "1"])
# TODO: @test map!() much more thoroughly
let a = [1.0, 2.0]
    map!(sin, a)
    @test isequal(a, sin([1.0, 2.0]))
end
# map -- ranges.jl
@test isequal(map(i->sqrt(i), 1:5), [sqrt(i) for i in 1:5])
@test isequal(map(i->sqrt(i), 2:6), [sqrt(i) for i in 2:6])

# map on ranges should evaluate first value only once (#4453)
let io=IOBuffer(3)
    map(x->print(io,x), 1:2)
    @test takebuf_string(io)=="12"
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

# filter -- array.jl
@test isequal(filter(x->(x>1), [0 1 2 3 2 1 0]), [2, 3, 2])
# TODO: @test_throws isequal(filter(x->x+1, [0 1 2 3 2 1 0]), [2, 3, 2])
@test isequal(filter(x->(x>10), [0 1 2 3 2 1 0]), [])
@test isequal(filter((ss)->length(ss)==3, ["abcd", "efg", "hij", "klmn", "opq"]), ["efg", "hij", "opq"])

# numbers
@test size(collect(1)) == size(1)

# zip and filter iterators
# issue #4718
@test collect(filter(x->x[1], zip([true, false, true, false],"abcd"))) == [(true,'a'),(true,'c')]

let z = zip(1:2)
    @test collect(z) == [(1,), (2,)]
    # Issue #13979
    @test eltype(z) == Tuple{Int}
end

let z = zip(1:2, 3:4)
    @test collect(z) == [(1,3), (2,4)]
    @test eltype(z) == Tuple{Int,Int}
end

let z = zip(1:2, 3:4, 5:6)
    @test collect(z) == [(1,3,5), (2,4,6)]
    @test eltype(z) == Tuple{Int,Int,Int}
end

@test eltype(Filter(isodd, 1:5)) == Int

# typed `collect`
@test collect(Float64, Filter(isodd, [1,2,3,4]))[1] === 1.0

@test isa(collect(Any, [1,2]), Vector{Any})

# enumerate (issue #6284)
let b = IOBuffer("1\n2\n3\n"), a = []
    for (i,x) in enumerate(eachline(b))
        push!(a, (i,x))
    end
    @test a == [(1,"1\n"),(2,"2\n"),(3,"3\n")]
end

# zip eachline (issue #7369)
let zeb     = IOBuffer("1\n2\n3\n4\n5\n"),
    letters = ['a', 'b', 'c', 'd', 'e'],
    res     = []
    for (number, letter) in zip(eachline(zeb), letters)
        push!(res, (parse(Int,strip(number)), letter))
    end
    @test res == [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e')]
end

@test length(zip(cycle(1:3), 1:7)) == 7
@test length(zip(cycle(1:3), 1:7, cycle(1:3))) == 7
@test length(zip(1:3,Base.product(1:7,cycle(1:3)))) == 3
@test length(zip(1:3,Base.product(1:7,cycle(1:3)),8)) == 1

# rest
# ----
let s = "hello"
    _, st = next(s, start(s))
    @test collect(rest(s, st)) == ['e','l','l','o']
end

@test_throws MethodError collect(rest(countfrom(1), 5))

# countfrom
# ---------

let i = 0
    for j = countfrom(0, 2)
        @test j == i*2
        i += 1
        i <= 10 || break
    end
end

# take
# ----

let t = take(0:2:8, 10), i = 0
    @test length(collect(t)) == 5

    for j = t
        @test j == i*2
        i += 1
    end
    @test i == 5
end

let i = 0
    for j = take(0:2:100, 10)
        @test j == i*2
        i += 1
    end
    @test i == 10
end

@test length(take(1:3,typemax(Int))) == 3
@test length(take(countfrom(1),3)) == 3
@test length(take(1:6,3)) == 3

# drop
# ----

let i = 0
    for j = drop(0:2:10, 2)
        @test j == (i+2)*2
        i += 1
    end
    @test i == 4
end

@test length(drop(1:3,typemax(Int))) == 0
@test Base.iteratorsize(drop(countfrom(1),3)) == Base.IsInfinite()
@test_throws MethodError length(drop(countfrom(1), 3))

# cycle
# -----

let i = 0
    for j = cycle(0:3)
        @test j == i % 4
        i += 1
        i <= 10 || break
    end
end

# repeated
# --------

let i = 0
    for j = repeated(1, 10)
        @test j == 1
        i += 1
    end
    @test i == 10
end
let i = 0
    for j = repeated(1)
        @test j == 1
        i += 1
        i <= 10 || break
    end
end
@test eltype(repeated(0))    == Int
@test eltype(repeated(0, 5)) == Int
@test Base.iteratorsize(repeated(0))      == Base.IsInfinite()
@test Base.iteratorsize(repeated(0, 5))   == Base.HasLength()
@test Base.iteratoreltype(repeated(0))    == Base.HasEltype()
@test Base.iteratoreltype(repeated(0, 5)) == Base.HasEltype()


# product
# -------

# empty?
for itr in [Base.product(1:0),
            Base.product(1:2, 1:0),
            Base.product(1:0, 1:2),
            Base.product(1:0, 1:1, 1:2),
            Base.product(1:1, 1:0, 1:2),
            Base.product(1:1, 1:2 ,1:0)]
    @test isempty(itr)
    @test isempty(collect(itr))
end

# collect a product - first iterators runs faster
@test collect(Base.product(1:2))           == [(i,)      for i=1:2]
@test collect(Base.product(1:2, 3:4))      == [(i, j)    for i=1:2, j=3:4]
@test collect(Base.product(1:2, 3:4, 5:6)) == [(i, j, k) for i=1:2, j=3:4, k=5:6]

# iteration order
let
    expected = [(1,3,5), (2,3,5), (1,4,5), (2,4,5), (1,3,6), (2,3,6), (1,4,6), (2,4,6)]
    actual = Base.product(1:2, 3:4, 5:6)
    for (exp, act) in zip(expected, actual)
        @test exp == act
    end
end

# collect multidimensional array
let
    a, b = 1:3, [4 6;
                 5 7]
    p = Base.product(a, b)
    @test size(p)    == (3, 2, 2)
    @test length(p)  == 12
    @test ndims(p)   == 3
    @test eltype(p)  == NTuple{2, Int}
    cp = collect(p)
    for i = 1:3
        @test cp[i, :, :] == [(i, 4) (i, 6);
                              (i, 5) (i, 7)]
    end
end

# with 1D inputs
let
    a, b, c = 1:2, 1.0:10.0, Int32(1):Int32(0)

    # length
    @test length(Base.product(a))       == 2
    @test length(Base.product(a, b))    == 20
    @test length(Base.product(a, b, c)) == 0

    # size
    @test size(Base.product(a))         == (2, )
    @test size(Base.product(a, b))      == (2, 10)
    @test size(Base.product(a, b, c))   == (2, 10, 0)

    # eltype
    @test eltype(Base.product(a))       == Tuple{Int}
    @test eltype(Base.product(a, b))    == Tuple{Int, Float64}
    @test eltype(Base.product(a, b, c)) == Tuple{Int, Float64, Int32}

    # ndims
    @test ndims(Base.product(a))        == 1
    @test ndims(Base.product(a, b))     == 2
    @test ndims(Base.product(a, b, c))  == 3
end

# with multidimensional inputs
let
    a, b, c = randn(4, 4), randn(3, 3, 3), randn(2, 2, 2, 2)
    args = Any[(a,),
               (a, a),
               (a, b),
               (a, a, a),
               (a, b, c)]
    sizes = Any[(4, 4),
                (4, 4, 4, 4),
                (4, 4, 3, 3, 3),
                (4, 4, 4, 4, 4, 4),
                (4, 4, 3, 3, 3, 2, 2, 2, 2)]
    for (method, fun) in zip([size, ndims, length], [x->x, length, prod])
        for i in 1:length(args)
            @test method(Base.product(args[i]...)) == method(collect(Base.product(args[i]...))) == fun(sizes[i])
        end
    end
end

# more tests on product with iterators of various type
let
    iters = (1:2,
             rand(2, 2, 2),
             take(1:4, 2),
             Base.product(1:2, 1:3),
             Base.product(rand(2, 2), rand(1, 1, 1))
             )
    for method in [size, length, ndims, eltype]
        for i = 1:length(iters)
            args = iters[i]
            @test method(Base.product(args...)) == method(collect(Base.product(args...)))
            for j = 1:length(iters)
                args = iters[i], iters[j]
                @test method(Base.product(args...)) == method(collect(Base.product(args...)))
                for k = 1:length(iters)
                    args = iters[i], iters[j], iters[k]
                    @test method(Base.product(args...)) == method(collect(Base.product(args...)))
                end
            end
        end
    end
end

# product of finite length and infinite length iterators
let
    a = 1:2
    b = countfrom(1)
    ab = Base.product(a, b)
    ba = Base.product(b, a)
    abexp = [(1, 1), (2, 1), (1, 2), (2, 2), (1, 3), (2, 3)]
    baexp = [(1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (6, 1)]
    for (expected, actual) in zip([abexp, baexp], [ab, ba])
        for (i, el) in enumerate(actual)
            @test el == expected[i]
            i == length(expected) && break
        end
        @test_throws ArgumentError length(actual)
        @test_throws ArgumentError size(actual)
        @test_throws ArgumentError ndims(actual)
    end

    # size infinite or unknown raises an error
    for itr in Any[countfrom(1), Filter(i->0, 1:10)]
        @test_throws ArgumentError length(Base.product(itr))
        @test_throws ArgumentError   size(Base.product(itr))
        @test_throws ArgumentError  ndims(Base.product(itr))
    end
end

# iteratorsize trait business
let f1 = Filter(i->i>0, 1:10)
    @test Base.iteratorsize(Base.product(f1))               == Base.SizeUnknown()
    @test Base.iteratorsize(Base.product(1:2, f1))          == Base.SizeUnknown()
    @test Base.iteratorsize(Base.product(f1, 1:2))          == Base.SizeUnknown()
    @test Base.iteratorsize(Base.product(f1, f1))           == Base.SizeUnknown()
    @test Base.iteratorsize(Base.product(f1, countfrom(1))) == Base.IsInfinite()
    @test Base.iteratorsize(Base.product(countfrom(1), f1)) == Base.IsInfinite()
end
@test Base.iteratorsize(Base.product(1:2, countfrom(1)))          == Base.IsInfinite()
@test Base.iteratorsize(Base.product(countfrom(2), countfrom(1))) == Base.IsInfinite()
@test Base.iteratorsize(Base.product(countfrom(1), 1:2))          == Base.IsInfinite()
@test Base.iteratorsize(Base.product(1:2))                        == Base.HasShape()
@test Base.iteratorsize(Base.product(1:2, 1:2))                   == Base.HasShape()
@test Base.iteratorsize(Base.product(take(1:2, 1), take(1:2, 1))) == Base.HasShape()
@test Base.iteratorsize(Base.product(take(1:2, 2)))               == Base.HasLength()
@test Base.iteratorsize(Base.product([1 2; 3 4]))                 == Base.HasShape()

# iteratoreltype trait business
let f1 = Filter(i->i>0, 1:10)
    @test Base.iteratoreltype(Base.product(f1))               == Base.HasEltype() # FIXME? eltype(f1) is Any
    @test Base.iteratoreltype(Base.product(1:2, f1))          == Base.HasEltype() # FIXME? eltype(f1) is Any
    @test Base.iteratoreltype(Base.product(f1, 1:2))          == Base.HasEltype() # FIXME? eltype(f1) is Any
    @test Base.iteratoreltype(Base.product(f1, f1))           == Base.HasEltype() # FIXME? eltype(f1) is Any
    @test Base.iteratoreltype(Base.product(f1, countfrom(1))) == Base.HasEltype() # FIXME? eltype(f1) is Any
    @test Base.iteratoreltype(Base.product(countfrom(1), f1)) == Base.HasEltype() # FIXME? eltype(f1) is Any
end
@test Base.iteratoreltype(Base.product(1:2, countfrom(1)))          == Base.HasEltype()
@test Base.iteratoreltype(Base.product(countfrom(1), 1:2))          == Base.HasEltype()
@test Base.iteratoreltype(Base.product(1:2))                        == Base.HasEltype()
@test Base.iteratoreltype(Base.product(1:2, 1:2))                   == Base.HasEltype()
@test Base.iteratoreltype(Base.product(take(1:2, 1), take(1:2, 1))) == Base.HasEltype()
@test Base.iteratoreltype(Base.product(take(1:2, 2)))               == Base.HasEltype()
@test Base.iteratoreltype(Base.product([1 2; 3 4]))                 == Base.HasEltype()



# flatten
# -------

import Base.flatten

@test collect(flatten(Any[1:2, 4:5])) == Any[1,2,4,5]
@test collect(flatten(Any[flatten(Any[1:2, 6:5]), flatten(Any[10:7, 10:9])])) == Any[1,2]
@test collect(flatten(Any[flatten(Any[1:2, 4:5]), flatten(Any[6:7, 8:9])])) == Any[1,2,4,5,6,7,8,9]
@test collect(flatten(Any[flatten(Any[1:2, 6:5]), flatten(Any[6:7, 8:9])])) == Any[1,2,6,7,8,9]
@test collect(flatten(Any[2:1])) == Any[]
@test eltype(flatten(UnitRange{Int8}[1:2, 3:4])) == Int8
@test_throws ArgumentError collect(flatten(Any[]))

@test Base.iteratoreltype(Base.Flatten((i for i=1:2) for j=1:1)) == Base.EltypeUnknown()

# foreach
let
    a = []
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

let I = Base.IteratorND(1:27,(3,3,3))
    @test collect(I) == reshape(1:27,(3,3,3))
    @test size(I) == (3,3,3)
    @test length(I) == 27
    @test eltype(I) === Int
    @test ndims(I) == 3
end

let A = collect(Base.Generator(x->2x, Real[1.5,2.5]))
    @test A == [3,5]
    @test isa(A,Vector{Float64})
end

let f(g) = (@test size(g.iter)==(2,3))
    f(i+j for i=1:2, j=3:5)
end

@test_throws DimensionMismatch Base.IteratorND(1:2, (2,3))

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

# partition(c, n)
let v = collect(Base.partition([1,2,3,4,5], 1))
    @test all(i->v[i][1] == i, v)
end

let v = collect(Base.partition([1,2,3,4,5], 2))
    @test v[1] == [1,2]
    @test v[2] == [3,4]
    @test v[3] == [5]
end

let v = collect(Base.partition(enumerate([1,2,3,4,5]), 3))
    @test v[1] == [(1,1),(2,2),(3,3)]
    @test v[2] == [(4,4),(5,5)]
end

for n in [5,6]
    @test collect(Base.partition([1,2,3,4,5], n))[1] == [1,2,3,4,5]
    @test collect(Base.partition(enumerate([1,2,3,4,5]), n))[1] ==
          [(1,1),(2,2),(3,3),(4,4),(5,5)]
end


@test join(map(x->string(x...), Base.partition("Hello World!", 5)), "|") ==
      "Hello| Worl|d!"

let s = "Monkey 🙈🙊🙊"
    tf = (n)->join(map(x->string(x...), Base.partition(s,n)), "|")
    @test tf(10) == s
    @test tf(9) == "Monkey 🙈🙊|🙊"
    @test tf(8) == "Monkey 🙈|🙊🙊"
    @test tf(7) == "Monkey |🙈🙊🙊"
    @test tf(6) == "Monkey| 🙈🙊🙊"
    @test tf(5) == "Monke|y 🙈🙊🙊"
    @test tf(4) == "Monk|ey 🙈|🙊🙊"
    @test tf(3) == "Mon|key| 🙈🙊|🙊"
    @test tf(2) == "Mo|nk|ey| 🙈|🙊🙊"
    @test tf(1) == "M|o|n|k|e|y| |🙈|🙊|🙊"
end
