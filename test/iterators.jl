using Base.Iterators

# zip and filter iterators
# issue #4718
@test collect(Iterators.filter(x->x[1], zip([true, false, true, false],"abcd"))) == [(true,'a'),(true,'c')]

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

@test eltype(Iterators.filter(isodd, 1:5)) == Int

# typed `collect`
@test collect(Float64, Iterators.filter(isodd, [1,2,3,4]))[1] === 1.0

# check direct EachLine constructor
let b = IOBuffer("foo\n")
    @test collect(EachLine(b)) == ["foo"]
    seek(b, 0)
    @test collect(EachLine(b, chomp=false)) == ["foo\n"]
    seek(b, 0)
    @test collect(EachLine(b, ondone=()->0)) == ["foo"]
    seek(b, 0)
    @test collect(EachLine(b, chomp=false, ondone=()->0)) == ["foo\n"]
end

# enumerate (issue #6284)
let b = IOBuffer("1\n2\n3\n"), a = []
    for (i,x) in enumerate(eachline(b))
        push!(a, (i,x))
    end
    @test a == [(1,"1"),(2,"2"),(3,"3")]
end

# zip eachline (issue #7369)
let zeb = IOBuffer("1\n2\n3\n4\n5\n"),
    letters = ['a', 'b', 'c', 'd', 'e'],
    res     = []
    for (number, letter) in zip(eachline(zeb), letters)
        push!(res, (parse(Int,strip(number)), letter))
    end
    @test res == [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e')]
end

@test length(zip(cycle(1:3), 1:7)) == 7
@test length(zip(cycle(1:3), 1:7, cycle(1:3))) == 7
@test length(zip(1:3,product(1:7,cycle(1:3)))) == 3
@test length(zip(1:3,product(1:7,cycle(1:3)),8)) == 1

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

# double take
# and take/drop canonicalization
# -----------
for xs in Any["abc", [1, 2, 3]]
    @test take(take(xs, 2), 3) === take(xs, 2)
    @test take(take(xs, 4), 2) === take(xs, 2)
    @test drop(drop(xs, 1), 1) === drop(xs, 2)
    @test take(drop(xs, 1), 1) === drop(take(xs, 2), 1)
    @test take(drop(xs, 3), 0) === drop(take(xs, 2), 3)
    @test isempty(drop(drop(xs, 2), 2))
    @test drop(take(drop(xs, 1), 2), 1) === take(drop(xs, 2), 1)
    @test take(drop(take(xs, 3), 1), 1) === take(drop(xs, 1), 1)
end

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
@test Base.iteratorsize(zip(repeated(0), repeated(0))) == Base.IsInfinite()


# product
# -------

# empty?
for itr in [product(1:0),
            product(1:2, 1:0),
            product(1:0, 1:2),
            product(1:0, 1:1, 1:2),
            product(1:1, 1:0, 1:2),
            product(1:1, 1:2 ,1:0)]
    @test isempty(itr)
    @test isempty(collect(itr))
end

# collect a product - first iterators runs faster
@test collect(product(1:2))           == [(i,)      for i=1:2]
@test collect(product(1:2, 3:4))      == [(i, j)    for i=1:2, j=3:4]
@test collect(product(1:2, 3:4, 5:6)) == [(i, j, k) for i=1:2, j=3:4, k=5:6]

# iteration order
let expected = [(1,3,5), (2,3,5), (1,4,5), (2,4,5), (1,3,6), (2,3,6), (1,4,6), (2,4,6)]
    actual = product(1:2, 3:4, 5:6)
    for (exp, act) in zip(expected, actual)
        @test exp == act
    end
end

# collect multidimensional array
let (a, b) = (1:3, [4 6;
                    5 7])
    p = product(a, b)
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
let (a, b, c) = (1:2, 1.0:10.0, Int32(1):Int32(0))
    # length
    @test length(product(a))       == 2
    @test length(product(a, b))    == 20
    @test length(product(a, b, c)) == 0

    # size
    @test size(product(a))         == (2, )
    @test size(product(a, b))      == (2, 10)
    @test size(product(a, b, c))   == (2, 10, 0)

    # eltype
    @test eltype(product(a))       == Tuple{Int}
    @test eltype(product(a, b))    == Tuple{Int, Float64}
    @test eltype(product(a, b, c)) == Tuple{Int, Float64, Int32}

    # ndims
    @test ndims(product(a))        == 1
    @test ndims(product(a, b))     == 2
    @test ndims(product(a, b, c))  == 3
end

# with multidimensional inputs
let (a, b, c) = (randn(4, 4), randn(3, 3, 3), randn(2, 2, 2, 2))
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
            @test method(product(args[i]...)) == method(collect(product(args[i]...))) == fun(sizes[i])
        end
    end
end

# more tests on product with iterators of various type
let iters = (1:2,
             rand(2, 2, 2),
             take(1:4, 2),
             product(1:2, 1:3),
             product(rand(2, 2), rand(1, 1, 1))
             )
    for method in [size, length, ndims, eltype]
        for i = 1:length(iters)
            args = iters[i]
            @test method(product(args...)) == method(collect(product(args...)))
            for j = 1:length(iters)
                args = iters[i], iters[j]
                @test method(product(args...)) == method(collect(product(args...)))
                for k = 1:length(iters)
                    args = iters[i], iters[j], iters[k]
                    @test method(product(args...)) == method(collect(product(args...)))
                end
            end
        end
    end
end

# product of finite length and infinite length iterators
let a = 1:2
    b = countfrom(1)
    ab = product(a, b)
    ba = product(b, a)
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
    for itr in Any[countfrom(1), Iterators.filter(i->0, 1:10)]
        @test_throws ArgumentError length(product(itr))
        @test_throws ArgumentError   size(product(itr))
        @test_throws ArgumentError  ndims(product(itr))
    end
end

# iteratorsize trait business
let f1 = Iterators.filter(i->i>0, 1:10)
    @test Base.iteratorsize(product(f1))               == Base.SizeUnknown()
    @test Base.iteratorsize(product(1:2, f1))          == Base.SizeUnknown()
    @test Base.iteratorsize(product(f1, 1:2))          == Base.SizeUnknown()
    @test Base.iteratorsize(product(f1, f1))           == Base.SizeUnknown()
    @test Base.iteratorsize(product(f1, countfrom(1))) == Base.IsInfinite()
    @test Base.iteratorsize(product(countfrom(1), f1)) == Base.IsInfinite()
end
@test Base.iteratorsize(product(1:2, countfrom(1)))          == Base.IsInfinite()
@test Base.iteratorsize(product(countfrom(2), countfrom(1))) == Base.IsInfinite()
@test Base.iteratorsize(product(countfrom(1), 1:2))          == Base.IsInfinite()
@test Base.iteratorsize(product(1:2))                        == Base.HasShape()
@test Base.iteratorsize(product(1:2, 1:2))                   == Base.HasShape()
@test Base.iteratorsize(product(take(1:2, 1), take(1:2, 1))) == Base.HasShape()
@test Base.iteratorsize(product(take(1:2, 2)))               == Base.HasLength()
@test Base.iteratorsize(product([1 2; 3 4]))                 == Base.HasShape()

# iteratoreltype trait business
let f1 = Iterators.filter(i->i>0, 1:10)
    @test Base.iteratoreltype(product(f1))               == Base.HasEltype() # FIXME? eltype(f1) is Any
    @test Base.iteratoreltype(product(1:2, f1))          == Base.HasEltype() # FIXME? eltype(f1) is Any
    @test Base.iteratoreltype(product(f1, 1:2))          == Base.HasEltype() # FIXME? eltype(f1) is Any
    @test Base.iteratoreltype(product(f1, f1))           == Base.HasEltype() # FIXME? eltype(f1) is Any
    @test Base.iteratoreltype(product(f1, countfrom(1))) == Base.HasEltype() # FIXME? eltype(f1) is Any
    @test Base.iteratoreltype(product(countfrom(1), f1)) == Base.HasEltype() # FIXME? eltype(f1) is Any
end
@test Base.iteratoreltype(product(1:2, countfrom(1)))          == Base.HasEltype()
@test Base.iteratoreltype(product(countfrom(1), 1:2))          == Base.HasEltype()
@test Base.iteratoreltype(product(1:2))                        == Base.HasEltype()
@test Base.iteratoreltype(product(1:2, 1:2))                   == Base.HasEltype()
@test Base.iteratoreltype(product(take(1:2, 1), take(1:2, 1))) == Base.HasEltype()
@test Base.iteratoreltype(product(take(1:2, 2)))               == Base.HasEltype()
@test Base.iteratoreltype(product([1 2; 3 4]))                 == Base.HasEltype()

@test collect(product(1:2,3:4)) == [(1,3) (1,4); (2,3) (2,4)]
@test isempty(collect(product(1:0,1:2)))
@test length(product(1:2,1:10,4:6)) == 60
@test Base.iteratorsize(product(1:2, countfrom(1))) == Base.IsInfinite()

# flatten
# -------
@test collect(flatten(Any[1:2, 4:5])) == Any[1,2,4,5]
@test collect(flatten(Any[flatten(Any[1:2, 6:5]), flatten(Any[10:7, 10:9])])) == Any[1,2]
@test collect(flatten(Any[flatten(Any[1:2, 4:5]), flatten(Any[6:7, 8:9])])) == Any[1,2,4,5,6,7,8,9]
@test collect(flatten(Any[flatten(Any[1:2, 6:5]), flatten(Any[6:7, 8:9])])) == Any[1,2,6,7,8,9]
@test collect(flatten(Any[2:1])) == Any[]
@test eltype(flatten(UnitRange{Int8}[1:2, 3:4])) == Int8
@test_throws ArgumentError collect(flatten(Any[]))

@test Base.iteratoreltype(Base.Flatten((i for i=1:2) for j=1:1)) == Base.EltypeUnknown()

# partition(c, n)
let v = collect(partition([1,2,3,4,5], 1))
    @test all(i->v[i][1] == i, v)
end

let v = collect(partition([1,2,3,4,5], 2))
    @test v[1] == [1,2]
    @test v[2] == [3,4]
    @test v[3] == [5]
end

let v = collect(partition(enumerate([1,2,3,4,5]), 3))
    @test v[1] == [(1,1),(2,2),(3,3)]
    @test v[2] == [(4,4),(5,5)]
end

for n in [5,6]
    @test collect(partition([1,2,3,4,5], n))[1] == [1,2,3,4,5]
    @test collect(partition(enumerate([1,2,3,4,5]), n))[1] ==
          [(1,1),(2,2),(3,3),(4,4),(5,5)]
end


@test join(map(x->string(x...), partition("Hello World!", 5)), "|") ==
      "Hello| Worl|d!"

let s = "Monkey 🙈🙊🙊"
    tf = (n)->join(map(x->string(x...), partition(s,n)), "|")
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

# take and friends with arbitrary integers (#19214)
for T in (UInt8, UInt16, UInt32, UInt64, UInt128, Int8, Int16, Int128, BigInt)
    @test length(take(1:6, T(3))) == 3
    @test length(drop(1:6, T(3))) == 3
    @test length(repeated(1, T(5))) == 5
    @test collect(partition(1:5, T(5)))[1] == collect(1:5)
end

@testset "collect finite iterators issue #12009" begin
    @test eltype(collect(enumerate(Iterators.Filter(x -> x>0, randn(10))))) == Tuple{Int, Float64}
end
