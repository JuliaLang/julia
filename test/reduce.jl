## foldl & foldr

# folds -- reduce.jl
@test foldl(-,[1:5]) == -13
@test foldl(-,10,[1:5]) == foldl(-,[10,1:5])

@test foldr(-,[1:5]) == 3
@test foldr(-,10,[1:5]) == foldr(-,[1:5,10])

# reduce -- reduce.jl
@test reduce((x,y)->"($x+$y)", [9:11]) == "((9+10)+11)"
@test reduce(max, [8 6 7 5 3 0 9]) == 9
@test reduce(+, 1000, [1:5]) == (1000 + 1 + 2 + 3 + 4 + 5)

# mapreduce -- reduce.jl
@test mapreduce(-, +, [-10 -9 -3]) == ((10 + 9) + 3)
@test mapreduce((x)->x[1:3], (x,y)->"($x+$y)", ["abcd", "efgh", "01234"]) == "((abc+efg)+012)"


z = zeros(2,2,2,2)
for i=1:16
    z[i] = i
end

@test sum(z) == sum(z,(1,2,3,4))[1] == 136

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

prod2(itr) = invoke(prod, (Any,), itr)
@test prod(Int[]) == prod2(Int[]) == 1
@test prod(Int[7]) == prod2(Int[7]) == 7
@test typeof(prod(Int8[])) == typeof(prod(Int8[1])) == typeof(prod(Int8[1 7])) == typeof(prod2(Int8[])) == typeof(prod2(Int8[1])) == typeof(prod2(Int8[1 7]))

v = cell(2,2,1,1)
v[1,1,1,1] = 28.0
v[1,2,1,1] = 36.0
v[2,1,1,1] = 32.0
v[2,2,1,1] = 40.0

@test isequal(v,sum(z,(3,4)))

@test sum_kbn([1,1e100,1,-1e100]) == 2

z = rand(10^6)
let es = sum_kbn(z), es2 = sum_kbn(z[1:10^5])
    @test (es - sum(z)) < es * 1e-13
    cs = cumsum(z)
    @test (es - cs[end]) < es * 1e-13
    @test (es2 - cs[10^5]) < es2 * 1e-13
end

@test_throws ErrorException sum(sin, Int[])
@test Base.sumabs(Float64[]) === 0.0
@test Base.sumabs2(Float64[]) === 0.0

@test sum(sin, [1]) == sin(1)
@test Base.sumabs([int8(-2)]) === 2
@test Base.sumabs2([int8(-2)]) === 4

x = -2:3
@test sum(sin, x) == sum(sin(x))
@test Base.sumabs(x) === 9
@test Base.sumabs2(x) === 19

@test_approx_eq sum(sin, z) sum(sin(z))
@test_approx_eq Base.sumabs(z) sum(abs(z))
@test_approx_eq Base.sumabs2(z) sum(abs2(z))

@test maximum(5) == 5
@test minimum(5) == 5

@test maximum([4, 3, 5, 2]) == 5
@test minimum([4, 3, 5, 2]) == 2
@test extrema([4, 3, 5, 2]) == (2, 5)

@test isnan(maximum([NaN]))
@test isnan(minimum([NaN]))
@test isequal(extrema([NaN]), (NaN, NaN))

@test maximum([4., 3., NaN, 5., 2.]) == 5.
@test minimum([4., 3., NaN, 5., 2.]) == 2.
@test extrema([4., 3., NaN, 5., 2.]) == (2., 5.)

@test extrema(1:5) == (1,5)

@test Base.maxabs(-2) == 2
@test Base.minabs(-2) == 2
@test Base.maxabs([1, -2, 3, -4]) == 4
@test Base.minabs([-1, 2, -3, 4]) == 1

@test maximum(abs2, 3:7) == 49
@test minimum(abs2, 3:7) == 9

@test any([true false; false false], 2) == [true false]'
@test any([true false; false false], 1) == [true false]

@test all([true true; false true], 2) == [true false]'
@test all([true false; false true], 1) == [false false]


## cumsum, cummin, cummax

@test isequal(cummin([1, 2, 5, -1, 3, -2]), [1, 1, 1, -1, -1, -2])
@test isequal(cummax([1, 2, 5, -1, 3, -2]), [1, 2, 5, 5, 5, 5])

@test isequal(cummax([1 0; 0 1], 1), [1 0; 1 1])
@test isequal(cummax([1 0; 0 1], 2), [1 1; 0 1])
@test isequal(cummin([1 0; 0 1], 1), [1 0; 0 0])
@test isequal(cummin([1 0; 0 1], 2), [1 0; 0 0])

