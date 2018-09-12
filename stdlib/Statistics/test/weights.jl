using Statistics
using LinearAlgebra, Random, SparseArrays, Test

@testset "Weights" begin
weight_funcs = (weights, aweights, fweights, pweights)

# Construction
@testset "$f" for f in weight_funcs
    @test isa(f([1, 2, 3]), AbstractWeights{Int})
    @test isa(f([1., 2., 3.]), AbstractWeights{Float64})
    @test isa(f([1 2 3; 4 5 6]), AbstractWeights{Int})

    @test isempty(f(Float64[]))
    @test size(f([1, 2, 3])) == (3,)

    w  = [1., 2., 3.]
    wv = f(w)
    @test eltype(wv) === Float64
    @test length(wv) === 3
    @test values(wv) === w
    @test sum(wv) === 6.0
    @test !isempty(wv)

    b  = trues(3)
    bv = f(b)
    @test eltype(bv) === Bool
    @test length(bv) === 3
    @test values(bv) === b
    @test sum(bv)    === 3
    @test !isempty(bv)

    ba = BitArray([true, false, true])
    sa = sparsevec([1., 0., 2.])

    # TODO: keep?
    #@test sum(ba, wv) === 4.0
    #@test sum(sa, wv) === 7.0
end

@testset "$f, setindex!" for f in weight_funcs
    w = [1., 2., 3.]
    wv = f(w)

    # Check getindex & sum
    @test wv[1] === 1.
    @test sum(wv) === 6.
    @test values(wv) == w

    # Test setindex! success
    @test (wv[1] = 4) === 4             # setindex! returns original val
    @test wv[1] === 4.                  # value correctly converted and set
    @test sum(wv) === 9.                # sum updated
    @test values(wv) == [4., 2., 3.]    # Test state of all values

    # Test mulivalue setindex!
    wv[1:2] = [3., 5.]
    @test wv[1] === 3.
    @test wv[2] === 5.
    @test sum(wv) === 11.
    @test values(wv) == [3., 5., 3.]   # Test state of all values

    # Test failed setindex! due to conversion error
    w = [1, 2, 3]
    wv = f(w)

    @test_throws InexactError wv[1] = 1.5   # Returns original value
    @test wv[1] === 1                       # value not updated
    @test sum(wv) === 6                     # sum not corrupted
    @test values(wv) == [1, 2, 3]           # Test state of all values
end

@testset "$f, isequal and ==" for f in weight_funcs
    x = f([1, 2, 3])

    y = f([1, 2, 3]) # same values, type and parameters
    @test isequal(x, y)
    @test x == y

    y = f([1.0, 2.0, 3.0]) # same values and type, different parameters
    @test isequal(x, y)
    @test x == y

    if f != fweights # same values and parameters, different types
        y = fweights([1, 2, 3])
        @test !isequal(x, y)
        @test x != y
    end

    x = f([1, 2, NaN]) # isequal and == treat NaN differently
    y = f([1, 2, NaN])
    @test isequal(x, y)
    @test x != y

    x = f([1.0, 2.0, 0.0]) # isequal and == treat ±0.0 differently
    y = f([1.0, 2.0, -0.0])
    @test !isequal(x, y)
    @test x == y
end

@testset "Mean $f" for f in weight_funcs
    @test mean([1:3;], weights=f([1.0, 1.0, 0.5])) ≈ 1.8
    @test mean(1:3, weights=f([1.0, 1.0, 0.5]))    ≈ 1.8

    a = reshape(1.0:27.0, 3, 3, 3)
    for wt in ([1.0, 1.0, 1.0], [1.0, 0.2, 0.0], [0.2, 0.0, 1.0])
        @test mean(a, weights=f(wt), dims=1) ≈
            sum(a.*reshape(wt, :, 1, 1), dims=1)/sum(wt)
        @test mean(a, weights=f(wt), dims=2) ≈
            sum(a.*reshape(wt, 1, :, 1), dims=2)/sum(wt)
        @test mean(a, weights=f(wt), dims=3) ≈
            sum(a.*reshape(wt, 1, 1, :), dims=3)/sum(wt)
        @test_throws DimensionMismatch mean(a, weights=f(wt), dims=4)
    end
end


# Quantile fweights
@testset "Quantile fweights" begin
    data = (
        [7, 1, 2, 4, 10],
        [7, 1, 2, 4, 10],
        [7, 1, 2, 4, 10, 15],
        [1, 2, 4, 7, 10, 15],
        [0, 10, 20, 30],
        [1, 2, 3, 4, 5],
        [1, 2, 3, 4, 5],
        [30, 40, 50, 60, 35],
        [2, 0.6, 1.3, 0.3, 0.3, 1.7, 0.7, 1.7],
        [1, 2, 2],
        [3.7, 3.3, 3.5, 2.8],
        [100, 125, 123, 60, 45, 56, 66],
        [2, 2, 2, 2, 2, 2],
        [2.3],
        [-2, -3, 1, 2, -10],
        [1, 2, 3, 4, 5],
        [5, 4, 3, 2, 1],
        [-2, 2, -1, 3, 6],
        [-10, 1, 1, -10, -10],
    )
    wt = (
        [3, 1, 1, 1, 3],
        [1, 1, 1, 1, 1],
        [3, 1, 1, 1, 3, 3],
        [1, 1, 1, 3, 3, 3],
        [30, 191, 9, 0],
        [10, 1, 1, 1, 9],
        [10, 1, 1, 1, 900],
        [1, 3, 5, 4, 2],
        [2, 2, 5, 0, 2, 2, 1, 6],
        [1, 1, 8],
        [5, 5, 4, 1],
        [30, 56, 144, 24, 55, 43, 67],
        [1, 2, 3, 4, 5, 6],
        [12],
        [7, 1, 1, 1, 6],
        [1, 0, 0, 0, 2],
        [1, 2, 3, 4, 5],
        [1, 2, 3, 2, 1],
        [0, 1, 1, 1, 1],
    )
    p = [0.0, 0.25, 0.5, 0.75, 1.0]
    function _rep(x::AbstractVector, lengths::AbstractVector{Int})
        res = similar(x, sum(lengths))
        i = 1
        for idx in 1:length(x)
            tmp = x[idx]
            for kdx in 1:lengths[idx]
                res[i] = tmp
                i += 1
            end
        end
        return res
    end
    # quantile with fweights is the same as repeated vectors
    for i = 1:length(data)
        @test quantile(data[i], p, weights=fweights(wt[i])) ≈
            quantile(_rep(data[i], wt[i]), p)
    end
    # quantile with fweights = 1  is the same as quantile
    for i = 1:length(data)
        @test quantile(data[i], p, weights=fweights(fill!(similar(wt[i]), 1))) ≈ quantile(data[i], p)
    end

    # Issue JuliaStats/StatsBase#313
    @test quantile([1, 2, 3, 4, 5], p, weights=fweights([0,1,2,1,0])) ≈
        quantile([2, 3, 3, 4], p)
    @test quantile([1, 2], 0.25, weights=fweights([1, 1])) ≈ 1.25
    @test quantile([1, 2], 0.25, weights=fweights([2, 2])) ≈ 1.0

    # test non integer frequency weights
    quantile([1, 2], 0.25, weights=fweights([1.0, 2.0])) ==
        quantile([1, 2], 0.25, weights=fweights([1, 2]))
    @test_throws ArgumentError quantile([1, 2], 0.25, weights=fweights([1.5, 2.0]))

    @test_throws ArgumentError quantile([1, 2], nextfloat(1.0), weights=fweights([1, 2]))
    @test_throws ArgumentError quantile([1, 2], prevfloat(0.0), weights=fweights([1, 2]))
end

@testset "Quantile aweights, pweights and weights" for f in (aweights, pweights, weights)
    data = (
        [7, 1, 2, 4, 10],
        [7, 1, 2, 4, 10],
        [7, 1, 2, 4, 10, 15],
        [1, 2, 4, 7, 10, 15],
        [0, 10, 20, 30],
        [1, 2, 3, 4, 5],
        [1, 2, 3, 4, 5],
        [30, 40, 50, 60, 35],
        [2, 0.6, 1.3, 0.3, 0.3, 1.7, 0.7, 1.7],
        [1, 2, 2],
        [3.7, 3.3, 3.5, 2.8],
        [100, 125, 123, 60, 45, 56, 66],
        [2, 2, 2, 2, 2, 2],
        [2.3],
        [-2, -3, 1, 2, -10],
        [1, 2, 3, 4, 5],
        [5, 4, 3, 2, 1],
        [-2, 2, -1, 3, 6],
        [-10, 1, 1, -10, -10],
    )
    wt = (
        [1, 1/3, 1/3, 1/3, 1],
        [1, 1, 1, 1, 1],
        [1, 1/3, 1/3, 1/3, 1, 1],
        [1/3, 1/3, 1/3, 1, 1, 1],
        [30, 191, 9, 0],
        [10, 1, 1, 1, 9],
        [10, 1, 1, 1, 900],
        [1, 3, 5, 4, 2],
        [2, 2, 5, 1, 2, 2, 1, 6],
        [0.1, 0.1, 0.8],
        [5, 5, 4, 1],
        [30, 56, 144, 24, 55, 43, 67],
        [0.1, 0.2, 0.3, 0.4, 0.5, 0.6],
        [12],
        [7, 1, 1, 1, 6],
        [1, 0, 0, 0, 2],
        [1, 2, 3, 4, 5],
        [0.1, 0.2, 0.3, 0.2, 0.1],
        [1, 1, 1, 1, 1],
    )
    quantile_answers = (
        [1.0, 4.0, 6.0, 8.0, 10.0],
        [1.0, 2.0, 4.0, 7.0, 10.0],
        [1.0, 4.75, 7.5, 10.4166667, 15.0],
        [1.0, 4.75, 7.5, 10.4166667, 15.0],
        [0.0, 2.6178010, 5.2356021, 7.8534031, 20.0],
        [1.0, 4.0, 4.3333333, 4.6666667, 5.0],
        [1.0, 4.2475, 4.4983333, 4.7491667, 5.0],
        [30.0, 37.5, 44.0, 51.25, 60.0],
        [0.3, 0.7, 1.3, 1.7, 2.0],
        [1.0, 2.0, 2.0, 2.0, 2.0],
        [2.8, 3.15, 3.4, 3.56, 3.7],
        [45.0, 62.149253, 102.875, 117.4097222, 125.0],
        [2.0, 2.0, 2.0, 2.0, 2.0],
        [2.3, 2.3, 2.3, 2.3, 2.3],
        [-10.0, -2.7857143, -2.4285714, -2.0714286, 2.0],
        [1.0, 2.0, 3.0, 4.0, 5.0],
        [1.0, 1.625, 2.3333333, 3.25, 5.0],
        [-2.0, -1.3333333, 0.5, 2.5, 6.0],
        [-10.0, -10.0, -10.0, 1.0, 1.0]
    )
    p = [0.0, 0.25, 0.5, 0.75, 1.0]

    Random.seed!(10)
    for i = 1:length(data)
        @test quantile(data[i], p, weights=f(wt[i])) ≈ quantile_answers[i] atol = 1e-5
        for j = 1:10
            # order of p does not matter
            reorder = sortperm(rand(length(p)))
            @test quantile(data[i], p[reorder], weights=f(wt[i])) ≈
                quantile_answers[i][reorder] atol = 1e-5
        end
        for j = 1:10
            # order of w does not matter
            reorder = sortperm(rand(length(data[i])))
            @test quantile(data[i][reorder], p, weights=f(wt[i][reorder])) ≈
                quantile_answers[i] atol = 1e-5
        end
    end
    # All equal weights corresponds to base quantile
    for v in (1, 2, 345)
        for i = 1:length(data)
            w = f(fill(v, length(data[i])))
            @test quantile(data[i], p, weights=w) ≈ quantile(data[i], p) atol = 1e-5
            for j = 1:10
                prandom = rand(4)
                @test quantile(data[i], prandom, weights=w) ≈
                    quantile(data[i], prandom) atol = 1e-5
            end
        end
    end
    # test zeros are removed
    for i = 1:length(data)
        @test quantile(vcat(1.0, data[i]), p, weights=f(vcat(0.0, wt[i]))) ≈
            quantile_answers[i] atol = 1e-5
    end
    # Syntax
    v = [7, 1, 2, 4, 10]
    w = [1, 1/3, 1/3, 1/3, 1]
    answer = 6.0
    @test quantile(data[1], 0.5, weights=f(w)) ≈  answer atol = 1e-5
end


@testset "Median $f" for f in weight_funcs
    data = [4, 3, 2, 1]
    wt = [0, 0, 0, 0]
    @test_throws ArgumentError median(data, weights=f(wt))
    @test_throws ArgumentError median(Float64[], weights=f(Float64[]))
    wt = [1, 2, 3, 4, 5]
    @test_throws ArgumentError median(data, weights=f(wt))
    # FIXME: This works now, needs to be covered
    # if VERSION >= v"1.0"
    #     @test_throws MethodError median([4 3 2 1 0], weights=f(wt))
    #     @test_throws MethodError median([[1 2] ; [4 5] ; [7 8] ; [10 11] ; [13 14]],
    #                                     weights=f(wt))
    # end
    data = [1, 3, 2, NaN, 2]
    @test isnan(median(data, weights=f(wt)))
    wt = [1, 2, NaN, 4, 5]
    @test_throws ArgumentError median(data, weights=f(wt))
    data = [1, 3, 2, 1, 2]
    @test_throws ArgumentError median(data, weights=f(wt))
    wt = [-1, -1, -1, -1, -1]
    @test_throws ArgumentError median(data, weights=f(wt))
    wt = [-1, -1, -1, 0, 0]
    @test_throws ArgumentError median(data, weights=f(wt))

    data = [4, 3, 2, 1]
    wt = [1, 2, 3, 4]
    @test median(data, weights=f(wt)) ≈
        quantile(data, 0.5, weights=f(wt)) atol = 1e-5
end

end # @testset Weights