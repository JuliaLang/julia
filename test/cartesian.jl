# This file is a part of Julia. License is MIT: https://julialang.org/license

@test Base.Cartesian.exprresolve(:(1 + 3)) == 4
ex = Base.Cartesian.exprresolve(:(if 5 > 4; :x; else :y; end))
@test ex.args[2] == QuoteNode(:x)

@test Base.Cartesian.lreplace!("val_col", Base.Cartesian.LReplace{String}(:col, "col", 1)) == "val_1"
@test Base.setindex(CartesianIndex(1,5,4),3,2) == CartesianIndex(1, 3, 4)

# test conversions for CartesianIndex

@testset "CartesianIndex Conversions" begin
    @test convert(Int, CartesianIndex(42)) === 42
    @test convert(Float64, CartesianIndex(42)) === 42.0
    @test convert(Tuple, CartesianIndex(42, 1)) === (42, 1)

    # can't convert higher-dimensional indices to Int
    @test_throws MethodError convert(Int, CartesianIndex(42, 1))
end

@testset "CartesianIndices overflow" begin
    I = CartesianIndices((1:typemax(Int),))
    i = last(I)
    @test iterate(I, i) === nothing

    I = CartesianIndices((1:(typemax(Int)-1),))
    i = CartesianIndex(typemax(Int))
    @test iterate(I, i) === nothing

    I = CartesianIndices((1:typemax(Int), 1:typemax(Int)))
    i = last(I)
    @test iterate(I, i) === nothing

    i = CartesianIndex(typemax(Int), 1)
    @test iterate(I, i) === (CartesianIndex(1, 2), CartesianIndex(1,2))

    # reverse cartesian indices
    I = CartesianIndices((typemin(Int):(typemin(Int)+3),))
    i = last(I)
    @test iterate(I, i) === nothing
end

@testset "CartesianIndices iteration" begin
    I = CartesianIndices((2:4, 0:1, 1:1, 3:5))
    indices = Vector{eltype(I)}()
    for i in I
        push!(indices, i)
    end
    @test length(I) == length(indices)
    @test vec(I) == indices

    empty!(indices)
    I = Iterators.reverse(I)
    for i in I
        push!(indices, i)
    end
    @test length(I) == length(indices)
    @test vec(collect(I)) == indices

    # test invalid state
    I = CartesianIndices((2:4, 3:5))
    @test iterate(I, CartesianIndex(typemax(Int), 3))[1] == CartesianIndex(2,4)
    @test iterate(I, CartesianIndex(typemax(Int), 4))[1] == CartesianIndex(2,5)
    @test iterate(I, CartesianIndex(typemax(Int), 5))    === nothing

    @test iterate(I, CartesianIndex(3, typemax(Int)))[1] == CartesianIndex(4,typemax(Int))
    @test iterate(I, CartesianIndex(4, typemax(Int)))    === nothing
end

@testset "CartesianIndices operations" begin
    I = CartesianIndices((1:3, 4:6))
    J = CartesianIndices((2:4, 3:5))

    @test @inferred(intersect(I, J)) == CartesianIndices((2:3, 4:5))
end
