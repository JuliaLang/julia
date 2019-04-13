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
