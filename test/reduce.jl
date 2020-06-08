# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random
isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

# fold(l|r) & mapfold(l|r)
@test foldl(+, Int64[]) === Int64(0) # In reference to issues #7465/#20144 (PR #20160)
@test foldl(+, Int16[]) === Int16(0) # In reference to issues #21536
@test foldl(-, 1:5) == -13
@test foldl(-, 1:5; init=10) == -5

@test Base.mapfoldl(abs2, -, 2:5) == -46
@test Base.mapfoldl(abs2, -, 2:5; init=10) == -44

@test Base.mapfoldl(abs2, /, 2:5) ≈ 1/900
@test Base.mapfoldl(abs2, /, 2:5; init=10) ≈ 1/1440

@test Base.mapfoldl((x)-> x ⊻ true, &, [true false true false false]) == false
@test Base.mapfoldl((x)-> x ⊻ true, &, [true false true false false]; init=true) == false

@test Base.mapfoldl((x)-> x ⊻ true, |, [true false true false false]) == true
@test Base.mapfoldl((x)-> x ⊻ true, |, [true false true false false]; init=false) == true

@test foldr(+, Int64[]) === Int64(0) # In reference to issue #20144 (PR #20160)
@test foldr(+, Int16[]) === Int16(0) # In reference to issues #21536
@test foldr(-, 1:5) == 3
@test foldr(-, 1:5; init=10) == -7
@test foldr(+, [1]) == 1 # Issue #21493

@test Base.mapfoldr(abs2, -, 2:5) == -14
@test Base.mapfoldr(abs2, -, 2:5; init=10) == -4
@test @inferred(mapfoldr(x -> x + 1, (x, y) -> (x, y...), (1, 2.0, '3');
                         init = ())) == (2, 3.0, '4')

@test foldr((x, y) -> ('⟨' * x * '|' * y * '⟩'), "λ 🐨.α") == "⟨λ|⟨ |⟨🐨|⟨.|α⟩⟩⟩⟩" # issue #31780
let x = rand(10)
    @test 0 == @allocated(sum(Iterators.reverse(x)))
    @test 0 == @allocated(foldr(-, x))
end

# reduce
@test reduce(+, Int64[]) === Int64(0) # In reference to issue #20144 (PR #20160)
@test reduce(+, Int16[]) === Int16(0) # In reference to issues #21536
@test reduce((x,y)->"($x+$y)", 9:11) == "((9+10)+11)"
@test reduce(max, [8 6 7 5 3 0 9]) == 9
@test reduce(+, 1:5; init=1000) == (1000 + 1 + 2 + 3 + 4 + 5)
@test reduce(+, 1) == 1
@test_throws ArgumentError reduce(*, ())
@test_throws ArgumentError reduce(*, Union{}[])

# mapreduce
@test mapreduce(-, +, [-10 -9 -3]) == ((10 + 9) + 3)
@test mapreduce((x)->x[1:3], (x,y)->"($x+$y)", ["abcd", "efgh", "01234"]) == "((abc+efg)+012)"

# mapreduce with multiple iterators
@test mapreduce(*, +, (i for i in 2:3), (i for i in 4:5)) == 23
@test mapreduce(*, +, (i for i in 2:3), (i for i in 4:5); init = 2) == 25
@test mapreduce(*, (x,y)->"($x+$y)", ["a", "b", "c"], ["d", "e", "f"]) == "((ad+be)+cf)"
@test mapreduce(*, (x,y)->"($x+$y)", ["a", "b", "c"], ["d", "e", "f"]; init = "gh") ==
    "(((gh+ad)+be)+cf)"

@test mapreduce(*, +, [2, 3], [4, 5]) == 23
@test mapreduce(*, +, [2, 3], [4, 5]; init = 2) == 25
@test mapreduce(*, +, [2, 3], [4, 5]; dims = 1) == [23]
@test mapreduce(*, +, [2, 3], [4, 5]; dims = 1, init = 2) == [25]
@test mapreduce(*, +, [2, 3], [4, 5]; dims = 2) == [8, 15]
@test mapreduce(*, +, [2, 3], [4, 5]; dims = 2, init = 2) == [10, 17]

@test mapreduce(*, +, [2 3; 4 5], [6 7; 8 9]) == 110
@test mapreduce(*, +, [2 3; 4 5], [6 7; 8 9]; init = 2) == 112
@test mapreduce(*, +, [2 3; 4 5], [6 7; 8 9]; dims = 1) == [44 66]
@test mapreduce(*, +, [2 3; 4 5], [6 7; 8 9]; dims = 1, init = 2) == [46 68]
@test mapreduce(*, +, [2 3; 4 5], [6 7; 8 9]; dims = 2) == reshape([33, 77], :, 1)
@test mapreduce(*, +, [2 3; 4 5], [6 7; 8 9]; dims = 2, init = 2) == reshape([35, 79], :, 1)

# mapreduce() for 1- 2- and n-sized blocks (PR #19325)
@test mapreduce(-, +, [-10]) == 10
@test mapreduce(abs2, +, [-9, -3]) == 81 + 9
@test mapreduce(-, +, [-9, -3, -4, 8, -2]) == (9 + 3 + 4 - 8 + 2)
@test mapreduce(-, +, Vector(range(1.0, stop=10000.0, length=10000))) == -50005000.0
# empty mr
@test mapreduce(abs2, +, Float64[]) === 0.0
@test mapreduce(abs2, *, Float64[]) === 1.0
@test mapreduce(abs2, max, Float64[]) === 0.0
@test mapreduce(abs, max, Float64[]) === 0.0
@test_throws ArgumentError mapreduce(abs2, &, Float64[])
@test_throws ArgumentError mapreduce(abs2, |, Float64[])

# mapreduce() type stability
@test typeof(mapreduce(*, +, Int8[10])) ===
      typeof(mapreduce(*, +, Int8[10, 11])) ===
      typeof(mapreduce(*, +, Int8[10, 11, 12, 13]))
@test typeof(mapreduce(*, +, Float32[10.0])) ===
      typeof(mapreduce(*, +, Float32[10, 11])) ===
      typeof(mapreduce(*, +, Float32[10, 11, 12, 13]))
# mapreduce() type stability when f supports empty collections
@test typeof(mapreduce(abs, +, Int8[])) ===
      typeof(mapreduce(abs, +, Int8[10])) ===
      typeof(mapreduce(abs, +, Int8[10, 11])) ===
      typeof(mapreduce(abs, +, Int8[10, 11, 12, 13]))
@test typeof(mapreduce(abs, +, Float32[])) ===
      typeof(mapreduce(abs, +, Float32[10])) ===
      typeof(mapreduce(abs, +, Float32[10, 11])) ===
      typeof(mapreduce(abs, +, Float32[10, 11, 12, 13]))

# sum
@testset "sums promote to at least machine size" begin
    @testset for T in [Int8, Int16, Int32]
        @test sum(T[]) === Int(0)
    end
    @testset for T in [UInt8, UInt16, UInt32]
        @test sum(T[]) === UInt(0)
    end
    @testset for T in [Int, Int64, Int128, UInt, UInt64, UInt128,
                       Float16, Float32, Float64]
        @test sum(T[]) === T(0)
    end
    @test sum(BigInt[]) == big(0) && sum(BigInt[]) isa BigInt
end

@test sum(Bool[]) === sum(Bool[false]) === sum(Bool[false, false]) === 0
@test sum(Bool[true, false, true]) === 2

@test sum(Int8(3)) === Int(3)
@test sum(3) === 3
@test sum(3.0) === 3.0

@test sum([Int8(3)]) === Int(3)
@test sum([3]) === 3
@test sum([3.0]) === 3.0

z = reshape(1:16, (2,2,2,2))
fz = float(z)
@test sum(z) === 136
@test sum(fz) === 136.0

@test_throws ArgumentError sum(Union{}[])
@test_throws ArgumentError sum(sin, Int[])
@test sum(sin, 3) == sin(3.0)
@test sum(sin, [3]) == sin(3.0)
a = sum(sin, z)
@test a ≈ sum(sin, fz)
@test a ≈ sum(sin.(fz))

z = [-4, -3, 2, 5]
fz = float(z)
a = randn(32) # need >16 elements to trigger BLAS code path
b = complex.(randn(32), randn(32))

# check variants of summation for type-stability and other issues (#6069)
sum2(itr) = invoke(sum, Tuple{Any}, itr)
plus(x,y) = x + y
sum3(A) = reduce(plus, A)
sum4(itr) = invoke(reduce, Tuple{Function, Any}, plus, itr)
sum5(A) = reduce(plus, A; init=0)
sum6(itr) = invoke(Core.kwfunc(reduce), Tuple{NamedTuple{(:init,), Tuple{Int}}, typeof(reduce), Function, Any}, (init=0,), reduce, plus, itr)
sum7(A) = mapreduce(x->x, plus, A)
sum8(itr) = invoke(mapreduce, Tuple{Function, Function, Any}, x->x, plus, itr)
sum9(A) = mapreduce(x->x, plus, A; init=0)
sum10(itr) = invoke(Core.kwfunc(mapreduce), Tuple{NamedTuple{(:init,),Tuple{Int}}, typeof(mapreduce), Function, Function, Any}, (init=0,), mapreduce, x->x, plus, itr)
for f in (sum2, sum5, sum6, sum9, sum10)
    @test sum(z) == f(z)
    @test sum(Int[]) == f(Int[]) == 0
    @test sum(Int[7]) == f(Int[7]) == 7
    @test typeof(f(Int8[])) == typeof(f(Int8[1])) == typeof(f(Int8[1 7]))
end
for f in (sum3, sum4, sum7, sum8)
    @test sum(z) == f(z)
    @test_throws ArgumentError f(Int[])
    @test sum(Int[7]) == f(Int[7]) == 7
end
@test typeof(sum(Int8[])) == typeof(sum(Int8[1])) == typeof(sum(Int8[1 7]))

# check sum(abs, ...) for support of empty collections
@testset "sum(abs, [])" begin
    @test @inferred(sum(abs, Float64[])) === 0.0
    @test @inferred(sum(abs, Int[])) === 0
    @test @inferred(sum(abs, Set{Int}())) === 0
    @test_throws MethodError sum(abs, Any[])
end

# prod

@test prod(Int[]) === 1
@test prod(Int8[]) === Int(1)
@test prod(Float64[]) === 1.0

@test prod([3]) === 3
@test prod([Int8(3)]) === Int(3)
@test prod([UInt8(3)]) === UInt(3)
@test prod([3.0]) === 3.0

@test prod(z) === 120
@test prod(fz) === 120.0

@test prod(1:big(16)) == big(20922789888000)
@test prod(big(typemax(Int64)):big(typemax(Int64))+16) == parse(BigInt,"25300281663413827620486300433089141956148633919452440329174083959168114253708467653081909888307573358090001734956158476311046124934597861626299416732205795533726326734482449215730132757595422510465791525610410023802664753402501982524443370512346073948799084936298007821432734720004795146875180123558814648586972474376192000")

@test typeof(prod(Array(trues(10)))) == Bool

# check type-stability
prod2(itr) = invoke(prod, Tuple{Any}, itr)
@test prod(Int[]) === prod2(Int[]) === 1
@test prod(Int[7]) === prod2(Int[7]) === 7
@test typeof(prod(Int8[])) == typeof(prod(Int8[1])) == typeof(prod(Int8[1, 7])) == Int
@test typeof(prod2(Int8[])) == typeof(prod2(Int8[1])) == typeof(prod2(Int8[1 7])) == Int

# maximum & minimum & extrema

@test_throws ArgumentError maximum(Int[])
@test_throws ArgumentError minimum(Int[])

@test maximum(Int[]; init=-1) == -1
@test minimum(Int[]; init=-1) == -1

@test maximum(5) == 5
@test minimum(5) == 5
@test extrema(5) == (5, 5)
@test extrema(abs2, 5) == (25, 25)

let x = [4,3,5,2]
    @test maximum(x) == 5
    @test minimum(x) == 2
    @test extrema(x) == (2, 5)

    @test maximum(abs2, x) == 25
    @test minimum(abs2, x) == 4
    @test extrema(abs2, x) == (4, 25)
end

@test maximum([-0.,0.]) === 0.0
@test maximum([0.,-0.]) === 0.0
@test maximum([0.,-0.,0.]) === 0.0
@test minimum([-0.,0.]) === -0.0
@test minimum([0.,-0.]) === -0.0
@test minimum([0.,-0.,0.]) === -0.0

@testset "minimum/maximum checks all elements" begin
    for N in [2:20;150;300]
        for i in 1:N
            arr = fill(0., N)
            truth = rand()
            arr[i] = truth
            @test maximum(arr) == truth

            truth = -rand()
            arr[i] = truth
            @test minimum(arr) == truth

            arr[i] = NaN
            @test isnan(maximum(arr))
            @test isnan(minimum(arr))

            arr = zeros(N)
            @test minimum(arr) === 0.0
            @test maximum(arr) === 0.0

            arr[i] = -0.0
            @test minimum(arr) === -0.0
            @test maximum(arr) ===  0.0

            arr = -zeros(N)
            @test minimum(arr) === -0.0
            @test maximum(arr) === -0.0
            arr[i] = 0.0
            @test minimum(arr) === -0.0
            @test maximum(arr) === 0.0
        end
    end
end

@testset "maximum works on generic order #30320" begin
    for n in [1:20;1500]
        arr = randn(n)
        @test GenericOrder(maximum(arr)) === maximum(map(GenericOrder, arr))
        @test GenericOrder(minimum(arr)) === minimum(map(GenericOrder, arr))
        f = x -> x
        @test GenericOrder(maximum(f,arr)) === maximum(f,map(GenericOrder, arr))
        @test GenericOrder(minimum(f,arr)) === minimum(f,map(GenericOrder, arr))
    end
end

@testset "maximum no out of bounds access #30462" begin
    arr = fill(-Inf, 128,128)
    @test maximum(arr) == -Inf
    arr = fill(Inf, 128^2)
    @test minimum(arr) == Inf
    for center in [256, 1024, 4096, 128^2]
        for offset in -10:10
            len = center + offset
            x = randn()
            arr = fill(x, len)
            @test maximum(arr) === x
            @test minimum(arr) === x
        end
    end
end

@test isnan(maximum([NaN]))
@test isnan(minimum([NaN]))
@test isequal(extrema([NaN]), (NaN, NaN))

@test isnan(maximum([NaN, 2.]))
@test isnan(maximum([2., NaN]))
@test isnan(minimum([NaN, 2.]))
@test isnan(minimum([2., NaN]))
@test isequal(extrema([NaN, 2.]), (NaN,NaN))

@test isnan(maximum([NaN, 2., 3.]))
@test isnan(minimum([NaN, 2., 3.]))
@test isequal(extrema([NaN, 2., 3.]), (NaN,NaN))

@test isnan(maximum([4., 3., NaN, 5., 2.]))
@test isnan(minimum([4., 3., NaN, 5., 2.]))
@test isequal(extrema([4., 3., NaN, 5., 2.]), (NaN,NaN))

 # test long arrays
@test isnan(maximum([NaN; 1.:10000.]))
@test isnan(maximum([1.:10000.; NaN]))
@test isnan(minimum([NaN; 1.:10000.]))
@test isnan(minimum([1.:10000.; NaN]))
@test isequal(extrema([1.:10000.; NaN]), (NaN,NaN))
@test isequal(extrema([NaN; 1.:10000.]), (NaN,NaN))

@test maximum(abs2, 3:7) == 49
@test minimum(abs2, 3:7) == 9
@test extrema(abs2, 3:7) == (9, 49)

@test maximum(Int16[1]) === Int16(1)
@test maximum(Vector(Int16(1):Int16(100))) === Int16(100)
@test maximum(Int32[1,2]) === Int32(2)

A = circshift(reshape(1:24,2,3,4), (0,1,1))
@test extrema(A,dims=1) == reshape([(23,24),(19,20),(21,22),(5,6),(1,2),(3,4),(11,12),(7,8),(9,10),(17,18),(13,14),(15,16)],1,3,4)
@test extrema(A,dims=2) == reshape([(19,23),(20,24),(1,5),(2,6),(7,11),(8,12),(13,17),(14,18)],2,1,4)
@test extrema(A,dims=3) == reshape([(5,23),(6,24),(1,19),(2,20),(3,21),(4,22)],2,3,1)
@test extrema(A,dims=(1,2)) == reshape([(19,24),(1,6),(7,12),(13,18)],1,1,4)
@test extrema(A,dims=(1,3)) == reshape([(5,24),(1,20),(3,22)],1,3,1)
@test extrema(A,dims=(2,3)) == reshape([(1,23),(2,24)],2,1,1)
@test extrema(A,dims=(1,2,3)) == reshape([(1,24)],1,1,1)
@test size(extrema(A,dims=1)) == size(maximum(A,dims=1))
@test size(extrema(A,dims=(1,2))) == size(maximum(A,dims=(1,2)))
@test size(extrema(A,dims=(1,2,3))) == size(maximum(A,dims=(1,2,3)))
@test extrema(x->div(x, 2), A, dims=(2,3)) == reshape([(0,11),(1,12)],2,1,1)

# any & all

@test @inferred any([]) == false
@test @inferred any(Bool[]) == false
@test @inferred any([true]) == true
@test @inferred any([false, false]) == false
@test @inferred any([false, true]) == true
@test @inferred any([true, false]) == true
@test @inferred any([true, true]) == true
@test @inferred any([true, true, true]) == true
@test @inferred any([true, false, true]) == true
@test @inferred any([false, false, false]) == false

@test @inferred all([]) == true
@test @inferred all(Bool[]) == true
@test @inferred all([true]) == true
@test @inferred all([false, false]) == false
@test @inferred all([false, true]) == false
@test @inferred all([true, false]) == false
@test @inferred all([true, true]) == true
@test @inferred all([true, true, true]) == true
@test @inferred all([true, false, true]) == false
@test @inferred all([false, false, false]) == false

@test @inferred any(x->x>0, []) == false
@test @inferred any(x->x>0, Int[]) == false
@test @inferred any(x->x>0, [-3]) == false
@test @inferred any(x->x>0, [4]) == true
@test @inferred any(x->x>0, [-3, 4, 5]) == true

@test @inferred all(x->x>0, []) == true
@test @inferred all(x->x>0, Int[]) == true
@test @inferred all(x->x>0, [-3]) == false
@test @inferred all(x->x>0, [4]) == true
@test @inferred all(x->x>0, [-3, 4, 5]) == false

@test reduce((a, b) -> a .| b, fill(trues(5), 24))  == trues(5)
@test reduce((a, b) -> a .| b, fill(falses(5), 24)) == falses(5)
@test reduce((a, b) -> a .& b, fill(trues(5), 24))  == trues(5)
@test reduce((a, b) -> a .& b, fill(falses(5), 24)) == falses(5)

@test_throws TypeError any(x->0, [false])
@test_throws TypeError all(x->0, [false])

# short-circuiting any and all

let c = [0, 0], A = 1:1000
    any(x->(c[1]=x; x==10), A)
    all(x->(c[2]=x; x!=10), A)

    @test c == [10,10]
end

# 19151 - always short circuit
let c = Int[], d = Int[], A = 1:9
    all((push!(c, x); x < 5) for x in A)
    @test c == 1:5

    any((push!(d, x); x > 4) for x in A)
    @test d == 1:5
end

# any/all with non-boolean collections

let f(x) = x == 1 ? true : x == 2 ? false : 1
    @test any(Any[false,true,false])
    @test @inferred any(map(f, [2,1,2]))
    @test @inferred any([f(x) for x in [2,1,2]])

    @test all(Any[true,true,true])
    @test @inferred all(map(f, [1,1,1]))
    @test @inferred all([f(x) for x in [1,1,1]])

    @test_throws TypeError any([1,true])
    @test_throws TypeError all([true,1])
    @test_throws TypeError any(map(f,[3,1]))
    @test_throws TypeError all(map(f,[1,3]))
end

# any and all with functors

struct SomeFunctor end
(::SomeFunctor)(x) = true

@test @inferred any(SomeFunctor(), 1:10)
@test @inferred all(SomeFunctor(), 1:10)


# in

@test in(1, Int[]) == false
@test in(1, Int[1]) == true
@test in(1, Int[2]) == false
@test in(0, 1:3) == false
@test in(1, 1:3) == true
@test in(2, 1:3) == true

# occursin

@test occursin("fox", "quick fox") == true
@test occursin("lazy dog", "quick fox") == false

# count

@test count(x->x>0, Int[]) == count(Bool[]) == 0
@test count(x->x>0, -3:5) == count((-3:5) .> 0) == 5
@test count([true, true, false, true]) == count(BitVector([true, true, false, true])) == 3
let x = repeat([false, true, false, true, true, false], 7)
    @test count(x) == 21
    GC.@preserve x (unsafe_store!(Ptr{UInt8}(pointer(x)), 0xfe, 3))
    @test count(x) == 21
end
@test_throws TypeError count(sqrt, [1])
@test_throws TypeError count([1])
let itr = (x for x in 1:10 if x < 7)
    @test count(iseven, itr) == 3
    @test_throws TypeError count(itr)
    @test_throws TypeError count(sqrt, itr)
end
@test count(iseven(x) for x in 1:10 if x < 7) == 3
@test count(iseven(x) for x in 1:10 if x < -7) == 0

@test count(!iszero, Int[]) == 0
@test count(!iszero, Int[0]) == 0
@test count(!iszero, Int[1]) == 1
@test count(!iszero, [1, 0, 2, 0, 3, 0, 4]) == 4


## cumsum, cummin, cummax

z = rand(10^6)
let es = sum(BigFloat.(z)), es2 = sum(BigFloat.(z[1:10^5]))
    @test (es - sum(z)) < es * 1e-13
    cs = cumsum(z)
    @test (es - cs[end]) < es * 1e-13
    @test (es2 - cs[10^5]) < es2 * 1e-13
end

@test sum(Vector(map(UInt8,0:255))) == 32640
@test sum(Vector(map(UInt8,254:255))) == 509

A = reshape(map(UInt8, 101:109), (3,3))
@test @inferred(sum(A)) == 945
@test @inferred(sum(view(A, 1:3, 1:3))) == 945

A = reshape(map(UInt8, 1:100), (10,10))
@test @inferred(sum(A)) == 5050
@test @inferred(sum(view(A, 1:10, 1:10))) == 5050

# issue #11618
@test sum([-0.0]) === -0.0
@test sum([-0.0, -0.0]) === -0.0
@test prod([-0.0, -0.0]) === 0.0

# containment
let A = Vector(1:10)
    @test A ∋ 5
    @test A ∌ 11
    @test any(y->y==6,A)
end

# issue #18695
test18695(r) = sum( t^2 for t in r )
@test @inferred(test18695([1.0,2.0,3.0,4.0])) == 30.0
@test_throws ArgumentError test18695(Any[])

# issue #21107
@test foldr(-,2:2) == 2

# test neutral element not picked incorrectly for &, |
@test @inferred(foldl(&, Int[1])) === 1
@test_throws ArgumentError foldl(&, Int[])

# prod on Chars
@test prod(Char[]) == ""
@test prod(Char['a']) == "a"
@test prod(Char['a','b']) == "ab"

@testset "optimized reduce(vcat/hcat, A) for arrays" begin
    for args in ([1:2], [[1, 2]], [1:2, 3:4], [[3, 4, 5], 1:2], [1:2, [3.5, 4.5]],
                 [[1 2], [3 4; 5 6]], [reshape([1, 2], 2, 1), 3:4])
        X = reduce(vcat, args)
        Y = vcat(args...)
        @test X == Y
        @test typeof(X) === typeof(Y)
    end
    for args in ([1:2], [[1, 2]], [1:2, 3:4], [[3, 4, 5], 1:3], [1:2, [3.5, 4.5]],
                 [[1 2; 3 4], [5 6; 7 8]], [1:2, [5 6; 7 8]], [[5 6; 7 8], [1, 2]])
        X = reduce(hcat, args)
        Y = hcat(args...)
        @test X == Y
        @test typeof(X) === typeof(Y)
    end
end

# offset axes
i = Base.Slice(-3:3)
x = [j^2 for j in i]
@test sum(x) == sum(x.parent) == 28
i = Base.Slice(0:0)
x = [j+7 for j in i]
@test sum(x) == 7

@testset "initial value handling with flatten" begin
    @test mapfoldl(
        x -> (x, x),
        ((a, b), (c, d)) -> (min(a, c), max(b, d)),
        Iterators.flatten((1:2, 3:4)),
    ) == (1, 4)
end
