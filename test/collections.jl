# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "into(Array, ::Vector)" begin
    xs = [1, 2, 3]
    @testset for T in [Array, Array{Float64}, Vector, Vector{Float64}]
        @test into(T, xs) isa T
        @test into(T, xs) !== xs
        @test into(T, xs) == xs
    end
end

@testset "into(Array, ::Matrix)" begin
    xs = [1 3; 2 4]
    @testset for T in [Array, Array{Float64}, Matrix, Matrix{Float64}]
        @test into(T, xs) isa T
        @test into(T, xs) !== xs
        @test into(T, xs) == xs
    end
    @testset for T in [Vector, Vector{Float64}]
        @test into(T, xs) isa T
        @test into(T, xs) !== vec(xs)
        @test into(T, xs) == vec(xs)
    end
end

@testset "into(BitArray, _)" begin
    @testset for T in [BitArray, BitVector]
        xs = BitArray([false, false])
        ys = into(T, xs)
        @test ys isa T
        @test ys == xs
        @testset "shallow copy" begin
            @test ys !== xs
            ys[1] = true
            @test xs[1] == false
        end
    end
end

@testset "into(AbstractDict, _)" begin
    @testset for T in [Dict, Base.IdDict, Base.ImmutableDict]
        xs = [:a => 1, :b => 2, :c => 3]
        @test issetequal(collect(into(T, copy(xs))::T), xs)
    end
end

@testset "into(AbstractDict, ::Pair)" begin
    @testset for T in [Dict, Base.IdDict, Base.ImmutableDict]
        @test issetequal(collect(into(T, (:a => 1) => (:b => 2))::T), [:a => 1, :b => 2])
    end
end

@testset "into(AbstractSet, ::Array)" begin
    @testset for T in [Set, Base.IdSet]
        xs = [1, 2, 2]
        @test into(T, xs) isa T
        @test into(T, xs) == Set([1, 2])
    end
end

@testset "into(AbstractSet, ::Set)" begin
    @testset for T in [Set, Base.IdSet]
        xs = Set([1, 2])
        @test into(T, xs) isa T
        @test into(T, xs) == xs
        @testset "shallow copy" begin
            push!(into(T, xs), 3)
            @test xs == Set([1, 2])
        end
    end
end

@testset "into(Tuple, _)" begin
    @test @inferred(into(Tuple{}, 1:0)) === ()
    @test @inferred(into(NTuple{1,Int}, [10])) === (10,)
    @test @inferred(into(NTuple{2,Int}, [10, 20])) === (10, 20)
    @test @inferred(into(NTuple{2}, [10, 20])) === (10, 20)
    @test @inferred(into(NTuple{2,Float64}, [10, 20])) === (10.0, 20.0)
    @test into(NTuple{3}, (1, 2.0, 3im)) === (1.0 + 0.0im, 2.0 + 0.0im, 0.0 + 3.0im)
    @test @inferred(into(Tuple{Int,Float64}, 1:2)) === (1, 2.0)
    @test into(NTuple{<:Any,Float64}, 1:3) === (1.0, 2.0, 3.0)
    @test into(Tuple, Any[1, 2.0]) === (1, 2.0)
    @test into(NTuple, Any[1, 2.0]) === (1.0, 2.0)

    @test @inferred(into(NTuple{2,Int})([10, 20])) === (10, 20)
    @test @inferred(into(NTuple{2})([10, 20])) === (10, 20)
    @test @inferred(into(NTuple{2,Float64})([10, 20])) === (10.0, 20.0)

    @test_throws(
        ArgumentError("`iterable` contains more than 0 element(s); 1-th element is `1`"),
        into(Tuple{}, 1:2)
    )
    @test_throws(
        ArgumentError("`iterable` contains more than 3 element(s); 4-th element is `40`"),
        into(NTuple{3}, 10:10:40)
    )
    @test_throws(
        ArgumentError("3 items required; `iterable` contains only 0 element(s)"),
        into(NTuple{3}, 1:0)
    )
    @test_throws(
        ArgumentError("3 items required; `iterable` contains only 2 element(s)"),
        into(NTuple{3}, 1:2)
    )
end
