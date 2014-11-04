
# fold(l|r) & mapfold(l|r)
@test foldl(-, 1:5) == -13
@test foldl(-, 10, 1:5) == -5

@test Base.mapfoldl(abs2, -, 2:5) == -46
@test Base.mapfoldl(abs2, -, 10, 2:5) == -44

@test foldr(-, 1:5) == 3
@test foldr(-, 10, 1:5) == -7

@test Base.mapfoldr(abs2, -, 2:5) == -14
@test Base.mapfoldr(abs2, -, 10, 2:5) == -4

# reduce & mapreduce
@test reduce((x,y)->"($x+$y)", [9:11]) == "((9+10)+11)"
@test reduce(max, [8 6 7 5 3 0 9]) == 9
@test reduce(+, 1000, [1:5]) == (1000 + 1 + 2 + 3 + 4 + 5)

@test mapreduce(-, +, [-10 -9 -3]) == ((10 + 9) + 3)
@test mapreduce((x)->x[1:3], (x,y)->"($x+$y)", ["abcd", "efgh", "01234"]) == "((abc+efg)+012)"

# sum

@test sum(Int8[]) === 0
@test sum(Int[]) === int(0)
@test sum(Float64[]) === 0.0

@test sum(int8(3)) === int8(3)
@test sum(3) === 3
@test sum(3.0) === 3.0

@test sum([int8(3)]) === 3
@test sum([3]) === 3
@test sum([3.0]) === 3.0

z = reshape(1:16, (2,2,2,2))
fz = float(z)
@test sum(z) === 136
@test sum(fz) === 136.0

@test_throws ErrorException sum(sin, Int[])
@test sum(sin, 3) == sin(3.0)
@test sum(sin, [3]) == sin(3.0)
a = sum(sin, z)
@test_approx_eq a sum(sin, fz)
@test_approx_eq a sum(sin(fz))

z = [-4, -3, 2, 5]
fz = float(z)
a = randn(32) # need >16 elements to trigger BLAS code path
b = complex(randn(32), randn(32))
@test sumabs(Float64[]) === 0.0
@test sumabs([int8(-2)]) === 2
@test sumabs(z) === 14
@test sumabs(fz) === 14.0
@test_approx_eq sumabs(a) sum(abs(a))
@test_approx_eq sumabs(b) sum(abs(b))

@test sumabs2(Float64[]) === 0.0
@test sumabs2([int8(-2)]) === 4
@test sumabs2(z) === 54
@test sumabs2(fz) === 54.0
@test_approx_eq sumabs2(a) sum(abs2(a))
@test_approx_eq sumabs2(b) sum(abs2(b))

# check variants of summation for type-stability and other issues (#6069)
sum2(itr) = invoke(sum, (Any,), itr)
plus(x,y) = x + y
sum3(A) = reduce(plus, A)
sum4(itr) = invoke(reduce, (Function, Any), plus, itr)
sum5(A) = reduce(plus, 0, A)
sum6(itr) = invoke(reduce, (Function, Int, Any), plus, 0, itr)
sum7(A) = mapreduce(x->x, plus, A)
sum8(itr) = invoke(mapreduce, (Function, Function, Any), x->x, plus, itr)
sum9(A) = mapreduce(x->x, plus, 0, A)
sum10(itr) = invoke(mapreduce, (Function, Function, Int, Any), x->x,plus,0,itr)
for f in (sum2, sum5, sum6, sum9, sum10)
    @test sum(z) == f(z)
    @test sum(Int[]) == f(Int[]) == 0
    @test sum(Int[7]) == f(Int[7]) == 7
    @test typeof(f(Int8[])) == typeof(f(Int8[1])) == typeof(f(Int8[1 7]))
end
for f in (sum3, sum4, sum7, sum8)
    @test sum(z) == f(z)
    @test_throws ErrorException f(Int[])
    @test sum(Int[7]) == f(Int[7]) == 7
end
@test typeof(sum(Int8[])) == typeof(sum(Int8[1])) == typeof(sum(Int8[1 7]))

@test sum_kbn([1,1e100,1,-1e100]) == 2
@test sum_kbn(Float64[]) == 0.0

# prod

@test prod(Int[]) === 1
@test prod(Int8[]) === 1
@test prod(Float64[]) === 1.0

@test prod([3]) === 3
@test prod([int8(3)]) === 3
@test prod([3.0]) === 3.0

@test prod(z) === 120
@test prod(fz) === 120.0

@test prod(1:big(16)) == big(20922789888000)
@test prod(big(typemax(Int64)):big(typemax(Int64))+16) == BigInt("25300281663413827620486300433089141956148633919452440329174083959168114253708467653081909888307573358090001734956158476311046124934597861626299416732205795533726326734482449215730132757595422510465791525610410023802664753402501982524443370512346073948799084936298007821432734720004795146875180123558814648586972474376192000")

# check type-stability
prod2(itr) = invoke(prod, (Any,), itr)
@test prod(Int[]) === prod2(Int[]) === 1
@test prod(Int[7]) === prod2(Int[7]) === 7
@test typeof(prod(Int8[])) == typeof(prod(Int8[1])) == typeof(prod(Int8[1, 7])) == Int
@test typeof(prod2(Int8[])) == typeof(prod2(Int8[1])) == typeof(prod2(Int8[1 7])) == Int

# maximum & minimum & extrema

@test_throws ErrorException maximum(Int[])
@test_throws ErrorException minimum(Int[])

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
@test_throws ErrorException Base.minabs(Int[])

@test maxabs(-2) == 2
@test minabs(-2) == 2
@test maxabs([1, -2, 3, -4]) == 4
@test minabs([-1, 2, -3, 4]) == 1

@test maximum(x->abs2(x), 3:7) == 49
@test minimum(x->abs2(x), 3:7) == 9

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

# in

@test in(1, Int[]) == false
@test in(1, Int[1]) == true
@test in(1, Int[2]) == false
@test in(0, 1:3) == false
@test in(1, 1:3) == true
@test in(2, 1:3) == true

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

@test sum(collect(uint8(0:255))) == 32640
@test sum(collect(uint8(254:255))) == 509
