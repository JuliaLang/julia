# This file is a part of Julia. License is MIT: http://julialang.org/license

# fold(l|r) & mapfold(l|r)
@test foldl(-, 1:5) == -13
@test foldl(-, 10, 1:5) == -5

@test Base.mapfoldl(abs2, -, 2:5) == -46
@test Base.mapfoldl(abs2, -, 10, 2:5) == -44

@test Base.mapfoldl(abs2, /, 2:5) ≈ 1/900
@test Base.mapfoldl(abs2, /, 10, 2:5) ≈ 1/1440

@test Base.mapfoldl((x)-> x $ true, &, true, [true false true false false]) == false
@test Base.mapfoldl((x)-> x $ true, &, [true false true false false]) == false

@test Base.mapfoldl((x)-> x $ true, |, [true false true false false]) == true
@test Base.mapfoldl((x)-> x $ true, |, false, [true false true false false]) == true

@test foldr(-, 1:5) == 3
@test foldr(-, 10, 1:5) == -7

@test Base.mapfoldr(abs2, -, 2:5) == -14
@test Base.mapfoldr(abs2, -, 10, 2:5) == -4

# reduce & mapreduce
@test reduce((x,y)->"($x+$y)", 9:11) == "((9+10)+11)"
@test reduce(max, [8 6 7 5 3 0 9]) == 9
@test reduce(+, 1000, 1:5) == (1000 + 1 + 2 + 3 + 4 + 5)
@test reduce(+,1) == 1

@test mapreduce(-, +, [-10 -9 -3]) == ((10 + 9) + 3)
@test mapreduce((x)->x[1:3], (x,y)->"($x+$y)", ["abcd", "efgh", "01234"]) == "((abc+efg)+012)"

# sum

@test sum(Int8[]) === Int32(0)
@test sum(Int[]) === Int(0)
@test sum(Float64[]) === 0.0

@test sum(Int8(3)) === Int8(3)
@test sum(3) === 3
@test sum(3.0) === 3.0

@test sum([Int8(3)]) === Int32(3)
@test sum([3]) === 3
@test sum([3.0]) === 3.0

z = reshape(1:16, (2,2,2,2))
fz = float(z)
@test sum(z) === 136
@test sum(fz) === 136.0

@test_throws ArgumentError sum(sin, Int[])
@test sum(sin, 3) == sin(3.0)
@test sum(sin, [3]) == sin(3.0)
a = sum(sin, z)
@test a ≈ sum(sin, fz)
@test a ≈ sum(sin(fz))

z = [-4, -3, 2, 5]
fz = float(z)
a = randn(32) # need >16 elements to trigger BLAS code path
b = complex(randn(32), randn(32))
@test sumabs(Float64[]) === 0.0
@test sumabs([Int8(-2)]) === Int32(2)
@test sumabs(z) === 14
@test sumabs(fz) === 14.0
@test sumabs(a) ≈ sum(abs(a))
@test sumabs(b) ≈ sum(abs(b))

@test sumabs2(Float64[]) === 0.0
@test sumabs2([Int8(-2)]) === Int32(4)
@test sumabs2(z) === 54
@test sumabs2(fz) === 54.0
@test sumabs2(a) ≈ sum(abs2(a))
@test sumabs2(b) ≈ sum(abs2(b))

# check variants of summation for type-stability and other issues (#6069)
sum2(itr) = invoke(sum, Tuple{Any}, itr)
plus(x,y) = x + y
sum3(A) = reduce(plus, A)
sum4(itr) = invoke(reduce, Tuple{Function, Any}, plus, itr)
sum5(A) = reduce(plus, 0, A)
sum6(itr) = invoke(reduce, Tuple{Function, Int, Any}, plus, 0, itr)
sum7(A) = mapreduce(x->x, plus, A)
sum8(itr) = invoke(mapreduce, Tuple{Function, Function, Any}, x->x, plus, itr)
sum9(A) = mapreduce(x->x, plus, 0, A)
sum10(itr) = invoke(mapreduce, Tuple{Function, Function, Int, Any}, x->x,plus,0,itr)
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

@test sum_kbn([1,1e100,1,-1e100]) == 2
@test sum_kbn(Float64[]) == 0.0

# prod

@test prod(Int[]) === 1
@test prod(Int8[]) === Int32(1)
@test prod(Float64[]) === 1.0

@test prod([3]) === 3
@test prod([Int8(3)]) === Int32(3)
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
@test typeof(prod(Int8[])) == typeof(prod(Int8[1])) == typeof(prod(Int8[1, 7])) == Int32
@test typeof(prod2(Int8[])) == typeof(prod2(Int8[1])) == typeof(prod2(Int8[1 7])) == Int32

# maximum & minimum & extrema

@test_throws ArgumentError maximum(Int[])
@test_throws ArgumentError minimum(Int[])

@test maximum(5) == 5
@test minimum(5) == 5
@test extrema(5) == (5, 5)

@test maximum([4, 3, 5, 2]) == 5
@test minimum([4, 3, 5, 2]) == 2
@test extrema([4, 3, 5, 2]) == (2, 5)

@test isnan(maximum([NaN]))
@test isnan(minimum([NaN]))
@test isequal(extrema([NaN]), (NaN, NaN))

@test maximum([NaN, 2., 3.]) == 3.
@test minimum([NaN, 2., 3.]) == 2.
@test extrema([NaN, 2., 3.]) == (2., 3.)

@test maximum([4., 3., NaN, 5., 2.]) == 5.
@test minimum([4., 3., NaN, 5., 2.]) == 2.
@test extrema([4., 3., NaN, 5., 2.]) == (2., 5.)

@test maxabs(Int[]) == 0
@test_throws ArgumentError Base.minabs(Int[])

@test maxabs(-2) == 2
@test minabs(-2) == 2
@test maxabs([1, -2, 3, -4]) == 4
@test minabs([-1, 2, -3, 4]) == 1

@test maximum(x->abs2(x), 3:7) == 49
@test minimum(x->abs2(x), 3:7) == 9

@test maximum(Int16[1]) === Int16(1)
@test maximum(collect(Int16(1):Int16(100))) === Int16(100)
@test maximum(Int32[1,2]) === Int32(2)

@test extrema(reshape(1:24,2,3,4),1) == reshape([(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16),(17,18),(19,20),(21,22),(23,24)],1,3,4)
@test extrema(reshape(1:24,2,3,4),2) == reshape([(1,5),(2,6),(7,11),(8,12),(13,17),(14,18),(19,23),(20,24)],2,1,4)
@test extrema(reshape(1:24,2,3,4),3) == reshape([(1,19),(2,20),(3,21),(4,22),(5,23),(6,24)],2,3,1)

# any & all

@test any(Bool[]) == false
@test any([true]) == true
@test any([false, false]) == false
@test any([false, true]) == true
@test any([true, false]) == true
@test any([true, true]) == true
@test any([true, true, true]) == true
@test any([true, false, true]) == true
@test any([false, false, false]) == false

@test all(Bool[]) == true
@test all([true]) == true
@test all([false, false]) == false
@test all([false, true]) == false
@test all([true, false]) == false
@test all([true, true]) == true
@test all([true, true, true]) == true
@test all([true, false, true]) == false
@test all([false, false, false]) == false

@test any(x->x>0, Int[]) == false
@test any(x->x>0, [-3]) == false
@test any(x->x>0, [4]) == true
@test any(x->x>0, [-3, 4, 5]) == true

@test all(x->x>0, Int[]) == true
@test all(x->x>0, [-3]) == false
@test all(x->x>0, [4]) == true
@test all(x->x>0, [-3, 4, 5]) == false

@test reduce(|, fill(trues(5), 24))  == trues(5)
@test reduce(|, fill(falses(5), 24)) == falses(5)
@test reduce(&, fill(trues(5), 24))  == trues(5)
@test reduce(&, fill(falses(5), 24)) == falses(5)

@test_throws TypeError any(x->0, [false])
@test_throws TypeError all(x->0, [false])

# short-circuiting any and all

let c = [0, 0], A = 1:1000
    any(x->(c[1]=x; x==10), A)
    all(x->(c[2]=x; x!=10), A)

    @test c == [10,10]
end

# any and all with functors

immutable SomeFunctor end
(::SomeFunctor)(x) = true

@test any(SomeFunctor(), 1:10)
@test all(SomeFunctor(), 1:10)


# in

@test in(1, Int[]) == false
@test in(1, Int[1]) == true
@test in(1, Int[2]) == false
@test in(0, 1:3) == false
@test in(1, 1:3) == true
@test in(2, 1:3) == true

# contains

@test contains("quick fox", "fox") == true
@test contains("quick fox", "lazy dog") == false

# count & countnz

@test count(x->x>0, Int[]) == 0
@test count(x->x>0, -3:5) == 5

@test countnz(Int[]) == 0
@test countnz(Int[0]) == 0
@test countnz(Int[1]) == 1
@test countnz([1, 0, 2, 0, 3, 0, 4]) == 4


## cumsum, cummin, cummax

z = rand(10^6)
let es = sum_kbn(z), es2 = sum_kbn(z[1:10^5])
    @test (es - sum(z)) < es * 1e-13
    cs = cumsum(z)
    @test (es - cs[end]) < es * 1e-13
    @test (es2 - cs[10^5]) < es2 * 1e-13
end

@test isequal(cummin([1, 2, 5, -1, 3, -2]), [1, 1, 1, -1, -1, -2])
@test isequal(cummax([1, 2, 5, -1, 3, -2]), [1, 2, 5, 5, 5, 5])

@test isequal(cummax([1 0; 0 1], 1), [1 0; 1 1])
@test isequal(cummax([1 0; 0 1], 2), [1 1; 0 1])
@test isequal(cummin([1 0; 0 1], 1), [1 0; 0 0])
@test isequal(cummin([1 0; 0 1], 2), [1 0; 0 0])

@test sum(collect(map(UInt8,0:255))) == 32640
@test sum(collect(map(UInt8,254:255))) == 509

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

#contains
let A = collect(1:10)
    @test A ∋ 5
    @test A ∌ 11
    @test contains(==,A,6)
end
