# This file is a part of Julia. License is MIT: https://julialang.org/license

# Array test
isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays
using SparseArrays

using Random, LinearAlgebra
using Dates

@testset "basics" begin
    @test length([1, 2, 3]) == 3
    @test count(!iszero, [1, 2, 3]) == 3

    let a = fill(1., 4), b = a+a, c = a-a, d = a+a+a
        @test b[1] === 2. && b[2] === 2. && b[3] === 2. && b[4] === 2.
        @test c[1] === 0. && c[2] === 0. && c[3] === 0. && c[4] === 0.
        @test d[1] === 3. && d[2] === 3. && d[3] === 3. && d[4] === 3.
    end

    @test length((1,)) == 1
    @test length((1,2)) == 2

    @test isequal(1 .+ [1,2,3], [2,3,4])
    @test isequal([1,2,3] .+ 1, [2,3,4])
    @test isequal(1 .- [1,2,3], [0,-1,-2])
    @test isequal([1,2,3] .- 1, [0,1,2])

    @test isequal(5*[1,2,3], [5,10,15])
    @test isequal([1,2,3]*5, [5,10,15])
    @test isequal(1 ./ [1,2,5], [1.0,0.5,0.2])
    @test isequal([1,2,3]/5, [0.2,0.4,0.6])

    @test isequal(2 .% [1,2,3], [0,0,2])
    @test isequal([1,2,3] .% 2, [1,0,1])
    @test isequal(2 .÷ [1,2,3], [2,1,0])
    @test isequal([1,2,3] .÷ 2, [0,1,1])
    @test isequal(-2 .% [1,2,3], [0,0,-2])
    @test isequal([-1,-2,-3].%2, [-1,0,-1])
    @test isequal(-2 .÷ [1,2,3], [-2,-1,0])
    @test isequal([-1,-2,-3] .÷ 2, [0,-1,-1])

    @test isequal(1 .<< [1,2,5], [2,4,32])
    @test isequal(128 .>> [1,2,5], [64,32,4])
    @test isequal(2 .>> 1, 1)
    @test isequal(1 .<< 1, 2)
    @test isequal([1,2,5] .<< [1,2,5], [2,8,160])
    @test isequal([10,20,50] .>> [1,2,5], [5,5,1])

    a = fill(1.,2,2)
    a[1,1] = 1
    a[1,2] = 2
    a[2,1] = 3
    a[2,2] = 4
    b = copy(a')
    @test a[1,1] == 1. && a[1,2] == 2. && a[2,1] == 3. && a[2,2] == 4.
    @test b[1,1] == 1. && b[2,1] == 2. && b[1,2] == 3. && b[2,2] == 4.
    a[[1 2 3 4]] .= 0
    @test a == zeros(2,2)
    a[[1 2], [1 2]] .= 1
    @test a == fill(1.,2,2)
    a[[1 2], 1] .= 0
    @test a[1,1] == 0. && a[1,2] == 1. && a[2,1] == 0. && a[2,2] == 1.
    a[:, [1 2]] .= 2
    @test a == fill(2.,2,2)

    a = Array{Float64}(undef, 2, 2, 2, 2, 2)
    a[1,1,1,1,1] = 10
    a[1,2,1,1,2] = 20
    a[1,1,2,2,1] = 30

    @test a[1,1,1,1,1] == 10
    @test a[1,2,1,1,2] == 20
    @test a[1,1,2,2,1] == 30

    b = reshape(a, (32,))
    @test b[1]  == 10
    @test b[19] == 20
    @test b[13] == 30
    @test_throws DimensionMismatch reshape(b,(5,7))
    @test_throws DimensionMismatch reshape(b,(35,))
    @test_throws ArgumentError reinterpret(Any, b)
    c = ["hello", "world"]
    @test_throws ArgumentError reinterpret(Float32, c)
    @test_throws ArgumentError resize!(Float64[], -2)

    b = rand(32)
    a = reshape(b, (2, 2, 2, 2, 2))
    @test ndims(a) == 5
    @test a[2,1,2,2,1] == b[14]
    @test a[2,2,2,2,2] == b[end]

    @test_throws DimensionMismatch reshape(1:3, 4)

    # issue #23107
    a = [1,2,3]
    @test typeof(a)(a) !== a
    @test Array(a) !== a
    @test Array{eltype(a)}(a) !== a
    @test Vector(a) !== a
end
@testset "reshaping SubArrays" begin
    a = Array(reshape(1:5, 1, 5))
    @testset "linearfast" begin
        s = view(a, :, 2:4)
        r = reshape(s, (length(s),))
        @test length(r) == 3
        @test r[1] == 2
        @test r[3,1] == 4
        @test r[Base.ReshapedIndex(CartesianIndex((1,2)))] == 3
        @test parent(reshape(r, (1,3))) === r.parent === s
        @test parentindices(r) == (1:1, 1:3)
        @test reshape(r, (3,)) === r
        @test convert(Array{Int,1}, r) == [2,3,4]
        @test_throws MethodError convert(Array{Int,2}, r)
        @test convert(Array{Int}, r) == [2,3,4]
        @test Base.unsafe_convert(Ptr{Int}, r) == Base.unsafe_convert(Ptr{Int}, s)
        @test isa(r, StridedArray)  # issue #22411
    end
    @testset "linearslow" begin
        s = view(a, :, [2,3,5])
        r = reshape(s, length(s))
        @test length(r) == 3
        @test r[1] == 2
        @test r[3,1] == 5
        @test r[Base.ReshapedIndex(CartesianIndex((1,2)))] == 3
        @test parent(reshape(r, (1,3))) === r.parent === s
        @test parentindices(r) == (1:1, 1:3)
        @test reshape(r, (3,)) === r
        @test convert(Array{Int,1}, r) == [2,3,5]
        @test_throws MethodError convert(Array{Int,2}, r)
        @test convert(Array{Int}, r) == [2,3,5]
        @test_throws ErrorException Base.unsafe_convert(Ptr{Int}, r)
        r[2] = -1
        @test a[3] == -1
        a = zeros(0, 5)  # an empty linearslow array
        s = view(a, :, [2,3,5])
        @test length(reshape(s, length(s))) == 0
    end
end
@testset "reshape(a, Val(N))" begin
    a = fill(1,3,3)
    s = view(a, 1:2, 1:2)
    for N in (1,3)
        @test isa(reshape(a, Val(N)), Array{Int,N})
        @test isa(reshape(s, Val(N)), Base.ReshapedArray{Int,N})
    end
end
@testset "zero-dimensional reshapes" begin
    @test reshape([1]) == fill(1)
    @test reshape([1]) isa Array{Int,0}
    @test reshape(1:1) == fill(1)
    @test reshape(1:1) isa Base.ReshapedArray{Int, 0}
    @test reshape([1], Val(0)) == fill(1)
    @test reshape([1], Val(0)) isa Array{Int,0}
    @test reshape(1:1, Val(0)) == fill(1)
    @test reshape(1:1, Val(0)) isa Base.ReshapedArray{Int, 0}
    @test_throws DimensionMismatch reshape([1,2])
    @test_throws DimensionMismatch reshape([1,2], Val(0))
    @test_throws DimensionMismatch reshape(1:2)
    @test_throws DimensionMismatch reshape(1:2, Val(0))
end
@testset "sizehint!" begin
    A = zeros(40)
    resize!(A, 1)
    sizehint!(A, 1)
    @test length(A) == 1

    A = zeros(40)
    resize!(A, 20)
    sizehint!(A, 1)
    @test length(A) == 20

    A = [1:10;]
    sizehint!(A, 20)
    @test length(A) == length([1:10;])
    @test A == [1:10;]

    A = [1:10;]
    resize!(A, 5)
    sizehint!(A, 5)
    @test length(A) == 5
    @test A == [1:5;]
end
@testset "reshape with colon" begin
    # Reshape with an omitted dimension
    let A = range(1, stop=60, length=60)
        @test size(reshape(A, :))         == (60,)
        @test size(reshape(A, :, 1))      == (60, 1)
        @test size(reshape(A, (:, 2)))    == (30, 2)
        @test size(reshape(A, 3, :))      == (3, 20)
        @test size(reshape(A, 2, 3, :))   == (2, 3, 10)
        @test size(reshape(A, (2, :, 5))) == (2, 6, 5)
        @test_throws DimensionMismatch reshape(A, 7, :)
        @test_throws DimensionMismatch reshape(A, :, 2, 3, 4)
        @test_throws DimensionMismatch reshape(A, (:, :))

        B = rand(2,2,2,2)
        @test size(reshape(B, :)) == (16,)
        @test size(reshape(B, :, 4)) == (4, 4)
        @test size(reshape(B, (2, 1, :))) == (2, 1, 8)
        @test_throws DimensionMismatch reshape(B, 3, :)
        @test_throws DimensionMismatch reshape(B, :, :, 2, 2)
    end
end

struct Z26163 <: AbstractArray{Int,0}; end
Base.size(::Z26163) = ()
Base.getindex(::Z26163) = 0
struct V26163 <: AbstractArray{Int,1}; end
Base.size(::V26163) = (1,)
Base.getindex(::V26163, ::Int) = 0
@testset "reshape of custom zero- and one-dimensional arrays" begin
    z = Z26163()
    v = V26163()
    @test z == reshape(v, ()) == fill(0, ())
    @test reshape(z, 1) == v == [0]
    @test reshape(z, 1, 1) == reshape(v, 1, 1) == fill(0, 1, 1)
    @test occursin("1-element reshape", summary(reshape(z, 1)))
    @test occursin("0-dimensional reshape", summary(reshape(v, ())))
end

@test reshape(1:5, (5,)) === 1:5
@test reshape(1:5, 5) === 1:5

@testset "setindex! on a reshaped range" begin
    a = reshape(1:20, 5, 4)
    for idx in ((3,), (2,2), (Base.ReshapedIndex(1),))
        try
            a[idx...] = 7
            error("wrong error")
        catch err
            @test err.msg == "indexed assignment fails for a reshaped range; consider calling collect"
        end
    end
end

@testset "reshape isbitsunion Arrays (issue #28611)" begin
    v = Union{Float64,Missing}[]
    for i in 1:10 push!(v, i) end
    v[5] = missing
    a = @inferred(reshape(v, 2, 5))
    for (I, i) in zip(CartesianIndices((2, 5)), 1:10)
        @test a[I] === v[i]
    end
    ac = copy(a)
    @test ac isa Array{Union{Float64, Missing}, 2}
    b = @inferred(reshape(ac, 5, 2))
    for (I, J) in zip(CartesianIndices((2, 5)), CartesianIndices((5, 2)))
        @test ac[I] === b[J]
    end

    for T in (Any, Union{Float64,String})
        a = Array{T}(undef, 4)
        @test @inferred(reshape(a, (2, 2))) isa Array{T,2}
        a = Array{T}(undef, 4, 1)
        @test @inferred(reshape(a, (2, 2))) isa Array{T,2}
    end
end

@testset "conversion from ReshapedArray to Array (#18262)" begin
    a = Base.ReshapedArray(1:3, (3, 1), ())
    @test convert(Array, a) == a
    @test convert(Array{Int}, a) == a
    @test convert(Array{Float64}, a) == a
    @test convert(Matrix, a) == a
    @test convert(Matrix{Int}, a) == a
    @test convert(Matrix{Float64}, a) == a
    b = Base.ReshapedArray(1:3, (3,), ())
    @test convert(Array, b) == b
    @test convert(Array{Int}, b) == b
    @test convert(Array{Float64}, b) == b
    @test convert(Vector, b) == b
    @test convert(Vector{Int}, b) == b
    @test convert(Vector{Float64}, b) == b
end

@testset "operations with IndexLinear ReshapedArray" begin
    b = Vector(1:12)
    a = Base.ReshapedArray(b, (4,3), ())
    @test a[3,2] == 7
    @test a[6] == 6
    a[3,2] = -2
    a[6] = -3
    a[Base.ReshapedIndex(5)] = -4
    @test b[5] == -4
    @test b[6] == -3
    @test b[7] == -2
    b = reinterpret(Int, a)
    b[1] = -1
    @test vec(b) == vec(a)

    a = rand(1, 1, 8, 8, 1)
    @test @inferred(dropdims(a, dims=1)) == @inferred(dropdims(a, dims=(1,))) == reshape(a, (1, 8, 8, 1))
    @test @inferred(dropdims(a, dims=(1, 5))) == dropdims(a, dims=(5, 1)) == reshape(a, (1, 8, 8))
    @test @inferred(dropdims(a, dims=(1, 2, 5))) == dropdims(a, dims=(5, 2, 1)) == reshape(a, (8, 8))
    @test_throws UndefKeywordError dropdims(a)
    @test_throws ArgumentError dropdims(a, dims=0)
    @test_throws ArgumentError dropdims(a, dims=(1, 1))
    @test_throws ArgumentError dropdims(a, dims=(1, 2, 1))
    @test_throws ArgumentError dropdims(a, dims=(1, 1, 2))
    @test_throws ArgumentError dropdims(a, dims=3)
    @test_throws ArgumentError dropdims(a, dims=4)
    @test_throws ArgumentError dropdims(a, dims=6)

    sz = (5,8,7)
    A = reshape(1:prod(sz),sz...)
    @test A[2:6] == [2:6;]
    @test A[1:3,2,2:4] == cat(46:48,86:88,126:128; dims=2)
    @test A[:,7:-3:1,5] == [191 176 161; 192 177 162; 193 178 163; 194 179 164; 195 180 165]
    @test reshape(A, Val(2))[:,3:9] == reshape(11:45,5,7)
    rng = (2,2:3,2:2:5)
    tmp = zeros(Int,map(maximum,rng)...)
    tmp[rng...] = A[rng...]
    @test  tmp == cat(zeros(Int,2,3),[0 0 0; 0 47 52],zeros(Int,2,3),[0 0 0; 0 127 132]; dims=3)

    @test cat(1,2,3.,4.,5.; dims=[1,2]) == diagm(0 => [1,2,3.,4.,5.])
    blk = [1 2;3 4]
    tmp = cat(blk,blk; dims=[1,3])
    @test tmp[1:2,1:2,1] == blk
    @test tmp[1:2,1:2,2] == zero(blk)
    @test tmp[3:4,1:2,1] == zero(blk)
    @test tmp[3:4,1:2,2] == blk

    x = rand(2,2)
    b = x[1,:]
    @test isequal(size(b), (2,))
    b = x[:,1]
    @test isequal(size(b), (2,))

    x = rand(5,5)
    b = x[2:3,2]
    @test b[1] == x[2,2] && b[2] == x[3,2]

    B = zeros(4,5)
    B[:,3] = 1:4
    @test B == [0 0 1 0 0; 0 0 2 0 0; 0 0 3 0 0; 0 0 4 0 0]
    B[2,:] = 11:15
    @test B == [0 0 1 0 0; 11 12 13 14 15; 0 0 3 0 0; 0 0 4 0 0]
    B[[3,1],[2,4]] = [21 22; 23 24]
    @test B == [0 23 1 24 0; 11 12 13 14 15; 0 21 3 22 0; 0 0 4 0 0]
    B[4,[2,3]] .= 7
    @test B == [0 23 1 24 0; 11 12 13 14 15; 0 21 3 22 0; 0 7 7 0 0]

    @test isequal(reshape(reshape(1:27, 3, 3, 3), Val(2))[1,:], [1,  4,  7,  10,  13,  16,  19,  22,  25])
end
@testset "find(in(b), a)" begin
    # unsorted inputs
    a = [3, 5, -7, 6]
    b = [4, 6, 2, -7, 1]
    @test findall(in(b), a) == [3,4]
    @test findall(in(Int[]), a) == Int[]
    @test findall(in(a), Int[]) == Int[]
    @test findall(in(b), reshape(a, 2, 2)) == [CartesianIndex(1, 2), CartesianIndex(2, 2)]

    # sorted inputs
    a = [1,2,3,4,5,10]
    b = [2,3,4,6]
    @test findall(in(b), a) == [2,3,4]
    @test findall(in(a), b) == [1,2,3]
    @test findall(in(Int[]), a) == Int[]
    @test findall(in(a), Int[]) == Int[]
    @test findall(in(b), reshape(a, 3, 2)) ==
        [CartesianIndex(2, 1), CartesianIndex(3, 1), CartesianIndex(1, 2)]
    @test findall(in(a), reshape(b, 2, 2)) ==
        [CartesianIndex(1, 1), CartesianIndex(2, 1), CartesianIndex(1, 2)]

    a = Vector(1:3:15)
    b = Vector(2:4:10)
    @test findall(in(b), a) == [4]
    @test findall(in(b), [a[1:4]; a[4:end]]) == [4,5]

    @test findall(in(NaN), [1.0, NaN, 2.0]) == [2]
    @test findall(in(NaN), [1.0, 2.0, NaN]) == [3]

    @testset "findall(in(x), b) for uncomparable element types" begin
        a = [1 + 1im, 1 - 1im]
        @test findall(in(1 + 1im), a) == [1]
        @test findall(in(a), a)       == [1,2]
    end

    @test findall(in([1, 2]), 2) == [1]
    @test findall(in([1, 2]), 3) == []

    @test sort(findall(Dict(1=>false, 2=>true, 3=>true))) == [2, 3]

    @test findall(true) == [1]
    @test findall(false) == Int[]

    @test findall(isodd, 1) == [1]
    @test findall(isodd, 2) == Int[]
end
@testset "setindex! return type" begin
    rt = Base.return_types(setindex!, Tuple{Array{Int32, 3}, Vector{UInt8}, Vector{Int}, Int16, UnitRange{Int}})
    @test length(rt) == 1 && rt[1] === Array{Int32, 3}
end
@testset "construction" begin
    @test typeof(Vector{Int}(undef, 3)) == Vector{Int}
    @test typeof(Vector{Int}()) == Vector{Int}
    @test typeof(Vector(undef, 3)) == Vector{Any}
    @test typeof(Vector()) == Vector{Any}
    @test typeof(Matrix{Int}(undef, 2,3)) == Matrix{Int}
    @test typeof(Matrix(undef, 2,3)) == Matrix{Any}

    @test size(Vector{Int}(undef, 3)) == (3,)
    @test size(Vector{Int}()) == (0,)
    @test size(Vector(undef, 3)) == (3,)
    @test size(Vector()) == (0,)
    @test size(Matrix{Int}(undef, 2,3)) == (2,3)
    @test size(Matrix(undef, 2,3)) == (2,3)

    @test_throws MethodError Matrix()
    @test_throws MethodError Matrix{Int}()
    @test_throws MethodError Array{Int,3}()
end
@testset "get" begin
    A = reshape(1:24, 3, 8)
    x = get(A, 32, -12)
    @test x == -12
    x = get(A, 14, -12)
    @test x == 14
    x = get(A, (2,4), -12)
    @test x == 11
    x = get(A, (4,4), -12)
    @test x == -12
    X = get(A, -5:5, NaN32)
    @test eltype(X) == Float32
    @test Base.elsize(X) == sizeof(Float32)
    @test !all(isinteger, X)
    @test isnan.(X) == [trues(6);falses(5)]
    @test X[7:11] == [1:5;]
    X = get(A, (2:4, 9:-2:-13), 0)
    Xv = zeros(Int, 3, 12)
    Xv[1:2, 2:5] = A[2:3, 7:-2:1]
    @test X == Xv
    X2 = get(A, Vector{Int}[[2:4;], [9:-2:-13;]], 0)
    @test X == X2
end
@testset "arrays as dequeues" begin
    l = Any[1]
    push!(l,2,3,8)
    @test l[1]==1 && l[2]==2 && l[3]==3 && l[4]==8
    v = pop!(l)
    @test v == 8
    v = pop!(l)
    @test v == 3
    @test length(l)==2
    m = Any[]
    @test_throws ArgumentError pop!(m)
    @test_throws ArgumentError popfirst!(m)
    pushfirst!(l,4,7,5)
    @test l[1]==4 && l[2]==7 && l[3]==5 && l[4]==1 && l[5]==2
    v = popfirst!(l)
    @test v == 4
    @test length(l)==4

    v = [3, 7, 6]
    @test_throws BoundsError insert!(v, 0, 5)
    for i = 1:4
        vc = copy(v)
        @test insert!(vc, i, 5) === vc
        @test vc == [v[1:(i-1)]; 5; v[i:end]]
    end
    @test_throws BoundsError insert!(v, 5, 5)
end
@testset "concatenation" begin
    @test isequal([fill(1.,2,2)  fill(2.,2,1)], [1. 1 2; 1 1 2])
    @test isequal([fill(1.,2,2); fill(2.,1,2)], [1. 1; 1 1; 2 2])
end

@testset "typed array literals" begin
    X = Float64[1 2 3]
    Y = [1. 2. 3.]
    @test size(X) == size(Y)
    for i = 1:3 @test X[i] === Y[i] end
    X = Float64[1;2;3]
    Y = [1.,2.,3.]
    @test size(X) == size(Y)
    for i = 1:3 @test X[i] === Y[i] end
    X = Float64[1 2 3; 4 5 6]
    Y = [1. 2. 3.; 4. 5. 6.]
    @test size(X) == size(Y)
    for i = 1:length(X) @test X[i] === Y[i] end

    _array_equiv(a,b) = eltype(a) == eltype(b) && a == b
    @test _array_equiv(UInt8[1:3;4], [0x1,0x2,0x3,0x4])
    @test_throws MethodError UInt8[1:3]
    @test_throws MethodError UInt8[1:3,]
    @test_throws MethodError UInt8[1:3,4:6]
    a = Vector{UnitRange{Int}}(undef, 1); a[1] = 1:3
    @test _array_equiv([1:3,], a)
    a = Vector{UnitRange{Int}}(undef, 2); a[1] = 1:3; a[2] = 4:6
    @test _array_equiv([1:3,4:6], a)
end

@testset "typed hvcat" begin
    X = Float64[1 2 3; 4 5 6]
    X32 = Float32[X X; X X]
    @test eltype(X32) <: Float32
    for i=[1,3], j=[1,4]
        @test X32[i:(i+1), j:(j+2)] == X
    end
end
@testset "fallback hvcat" begin
    #Issue #23994
    A23994 = [1 "two"; 3im 4.0; 5 6//1]
    @test A23994[2] isa Complex{Int}
    @test A23994[3] isa Int
    @test A23994[5] isa Float64
    @test A23994[6] isa Rational{Int}
end
@testset "cat issue #27326" begin
    @test [Int; 1] == [Int, 1]
    @test [Int 1] == reshape([Int, 1], 1, :)
    @test [Int 1; String 2] == reshape([Int, String, 1, 2], 2, 2)
end
@testset "end" begin
    X = [ i+2j for i=1:5, j=1:5 ]
    @test X[end,end] == 15
    @test X[end]     == 15  # linear index
    @test X[2,  end] == 12
    @test X[end,  2] == 9
    @test X[end-1,2] == 8
    Y = [2, 1, 4, 3]
    @test X[Y[end],1] == 5
    @test X[end,Y[end]] == 11
end

@testset "findall, findfirst, findnext, findlast, findprev" begin
    a = [0,1,2,3,0,1,2,3]
    @test findall(!iszero, a) == [2,3,4,6,7,8]
    @test findall(a.==2) == [3,7]
    @test findall(isodd,a) == [2,4,6,8]
    @test findfirst(!iszero, a) == 2
    @test findfirst(a.==0) == 1
    @test findfirst(a.==5) == nothing
    @test findfirst(Dict(1=>false, 2=>true)) == 2
    @test findfirst(isequal(3), [1,2,4,1,2,3,4]) == 6
    @test findfirst(!isequal(1), [1,2,4,1,2,3,4]) == 2
    @test findfirst(isodd, [2,4,6,3,9,2,0]) == 4
    @test findfirst(isodd, [2,4,6,2,0]) == nothing
    @test findnext(!iszero,a,4) == 4
    @test findnext(!iszero,a,5) == 6
    @test findnext(!iszero,a,1) == 2
    @test findnext(isequal(1),a,4) == 6
    @test findnext(isequal(5),a,4) == nothing
    @test findlast(!iszero, a) == 8
    @test findlast(a.==0) == 5
    @test findlast(a.==5) == nothing
    @test findlast(isequal(3), [1,2,4,1,2,3,4]) == 6
    @test findlast(isodd, [2,4,6,3,9,2,0]) == 5
    @test findlast(isodd, [2,4,6,2,0]) == nothing
    @test findprev(!iszero,a,4) == 4
    @test findprev(!iszero,a,5) == 4
    @test findprev(!iszero,a,1) == nothing
    @test findprev(isequal(1),a,4) == 2
    @test findprev(isequal(1),a,8) == 6
    @test findprev(isodd, [2,4,5,3,9,2,0], 7) == 5
    @test findprev(isodd, [2,4,5,3,9,2,0], 2) == nothing
    @test findfirst(isequal(0x00), [0x01, 0x00]) == 2
    @test findlast(isequal(0x00), [0x01, 0x00]) == 2
    @test findnext(isequal(0x00), [0x00, 0x01, 0x00], 2) == 3
    @test findprev(isequal(0x00), [0x00, 0x01, 0x00], 2) == 1
end
@testset "find with Matrix" begin
    A = [1 2 0; 3 4 0]
    @test findall(isodd, A) == [CartesianIndex(1, 1), CartesianIndex(2, 1)]
    @test findall(!iszero, A) == [CartesianIndex(1, 1), CartesianIndex(2, 1),
                               CartesianIndex(1, 2), CartesianIndex(2, 2)]
    @test findfirst(isodd, A) == CartesianIndex(1, 1)
    @test findlast(isodd, A) == CartesianIndex(2, 1)
    @test findnext(isodd, A, CartesianIndex(1, 1)) == CartesianIndex(1, 1)
    @test findprev(isodd, A, CartesianIndex(2, 1)) == CartesianIndex(2, 1)
    @test findnext(isodd, A, CartesianIndex(1, 2)) === nothing
    @test findprev(iseven, A, CartesianIndex(2, 1)) === nothing
end

@testset "findmin findmax argmin argmax" begin
    @test argmax([10,12,9,11]) == 2
    @test argmin([10,12,9,11]) == 3
    @test findmin([NaN,3.2,1.8]) === (NaN,1)
    @test findmax([NaN,3.2,1.8]) === (NaN,1)
    @test findmin([NaN,3.2,1.8,NaN]) === (NaN,1)
    @test findmax([NaN,3.2,1.8,NaN]) === (NaN,1)
    @test findmin([3.2,1.8,NaN,2.0]) === (NaN,3)
    @test findmax([3.2,1.8,NaN,2.0]) === (NaN,3)

    #14085
    @test findmax(4:9) == (9,6)
    @test argmax(4:9) == 6
    @test findmin(4:9) == (4,1)
    @test argmin(4:9) == 1
    @test findmax(5:-2:1) == (5,1)
    @test argmax(5:-2:1) == 1
    @test findmin(5:-2:1) == (1,3)
    @test argmin(5:-2:1) == 3

    #23094
    @test_throws MethodError findmax(Set(["abc"]))
    @test findmin(["abc", "a"]) === ("a", 2)
    @test_throws MethodError findmax([Set([1]), Set([2])])
    @test findmin([0.0, -0.0]) === (-0.0, 2)
    @test findmax([0.0, -0.0]) === (0.0, 1)
    @test findmin([-0.0, 0.0]) === (-0.0, 1)
    @test findmax([-0.0, 0.0]) === (0.0, 2)
    @test isnan(findmin([NaN, NaN, 0.0/0.0])[1])
    @test findmin([NaN, NaN, 0.0/0.0])[2] == 1
    @test isnan(findmax([NaN, NaN, 0.0/0.0])[1])
    @test findmax([NaN, NaN, 0.0/0.0])[2] == 1

    # Check that cartesian indices are returned for matrices
    @test argmax([10 12; 9 11]) === CartesianIndex(1, 2)
    @test argmin([10 12; 9 11]) === CartesianIndex(2, 1)
    @test findmax([10 12; 9 11]) === (12, CartesianIndex(1, 2))
    @test findmin([10 12; 9 11]) === (9, CartesianIndex(2, 1))
end

@testset "permutedims" begin
    # keeps the num of dim
    p = randperm(5)
    q = randperm(5)
    a = rand(p...)
    b = permutedims(a,q)
    @test isequal(size(b), tuple(p[q]...))

    # hand made case
    y = zeros(1,2,3)
    for i = 1:6
        y[i]=i
    end

    z = zeros(3,1,2)
    for i = 1:3
        z[i] = i*2-1
        z[i+3] = i*2
    end

    # permutes correctly
    @test isequal(z,permutedims(y,[3,1,2]))
    @test isequal(z,permutedims(y,(3,1,2)))

    # of a subarray
    a = rand(5,5)
    s = view(a,2:3,2:3)
    p = permutedims(s, [2,1])
    @test p[1,1]==a[2,2] && p[1,2]==a[3,2]
    @test p[2,1]==a[2,3] && p[2,2]==a[3,3]

    # of a non-strided subarray
    a = reshape(1:60, 3, 4, 5)
    s = view(a,:,[1,2,4],[1,5])
    c = convert(Array, s)
    for p in ([1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1])
        @test permutedims(s, p) == permutedims(c, p)
        @test PermutedDimsArray(s, p) == permutedims(c, p)
    end
    @test_throws ArgumentError permutedims(a, (1,1,1))
    @test_throws ArgumentError permutedims(s, (1,1,1))
    @test_throws ArgumentError PermutedDimsArray(a, (1,1,1))
    @test_throws ArgumentError PermutedDimsArray(s, (1,1,1))
    cp = PermutedDimsArray(c, (3,2,1))
    @test pointer(cp) == pointer(c)
    @test_throws ArgumentError pointer(cp, 2)
    @test strides(cp) == (9,3,1)
    ap = PermutedDimsArray(Array(a), (2,1,3))
    @test strides(ap) == (3,1,12)

    for A in [rand(1,2,3,4),rand(2,2,2,2),rand(5,6,5,6),rand(1,1,1,1)]
        perm = randperm(4)
        @test isequal(A,permutedims(permutedims(A,perm),invperm(perm)))
        @test isequal(A,permutedims(permutedims(A,invperm(perm)),perm))
    end

    m = [1 2; 3 4]
    @test permutedims(m) == [1 3; 2 4]

    v = [1,2,3]
    @test permutedims(v) == [1 2 3]

    x = PermutedDimsArray([1 2; 3 4], (2, 1))
    @test size(x) == (2, 2)
    @test copy(x) == [1 3; 2 4]
    y = [0, 0, 0, 0]
    copyto!(y, x)
    @test y == [1, 2, 3, 4]
end

@testset "circshift" begin
    @test circshift(1:5, -1) == circshift(1:5, 4) == circshift(1:5, -6) == [2,3,4,5,1]
    @test circshift(1:5, 1) == circshift(1:5, -4) == circshift(1:5, 6)  == [5,1,2,3,4]
    a = [1:5;]
    @test_throws ArgumentError Base.circshift!(a, a, 1)
    b = copy(a)
    @test Base.circshift!(b, a, 1) == [5,1,2,3,4]
end

# unique across dim

# With hash collisions
struct HashCollision
    x::Float64
end
Base.hash(::HashCollision, h::UInt) = h

# All rows and columns unique
let A, B, C, D
    A = fill(1., 10, 10)
    A[diagind(A)] = shuffle!([1:10;])
    @test unique(A, dims=1) == A
    @test unique(A, dims=2) == A

    # 10 repeats of each row
    B = A[shuffle!(repeat(1:10, 10)), :]
    C = unique(B, dims=1)
    @test sortslices(C, dims=1) == sortslices(A, dims=1)
    @test unique(B, dims=2) == B
    @test unique(B', dims=2)' == C

    # Along third dimension
    D = cat(B, B, dims=3)
    @test unique(D, dims=1) == cat(C, C, dims=3)
    @test unique(D, dims=3) == cat(B, dims=3)

    # With hash collisions
    @test map(x -> x.x, unique(map(HashCollision, B), dims=1)) == C
end

@testset "large matrices transpose" begin
    for i = 1 : 3
        a = rand(200, 300)
        @test isequal(copy(a'), permutedims(a, [2, 1]))
    end
end

@testset "repeat" begin
    local A, A1, A2, A3, v, v2, cv, cv2, c, R, T
    A = fill(1,2,3,4)
    A1 = reshape(repeat([1,2],1,12),2,3,4)
    A2 = reshape(repeat([1 2 3],2,4),2,3,4)
    A3 = reshape(repeat([1 2 3 4],6,1),2,3,4)
    @test isequal(cumsum(A,dims=1),A1)
    @test isequal(cumsum(A,dims=2),A2)
    @test isequal(cumsum(A,dims=3),A3)

    # issue 20112
    A3 = reshape(repeat([1 2 3 4],UInt32(6),UInt32(1)),2,3,4)
    @test isequal(cumsum(A,dims=3),A3)
    @test repeat([1,2,3,4], UInt32(1)) == [1,2,3,4]
    @test repeat([1 2], UInt32(2)) == repeat([1 2], UInt32(2), UInt32(1))

    # issue 20564
    @test_throws MethodError repeat(1, 2, 3)
    @test repeat([1, 2], 1, 2, 3) == repeat([1, 2], outer = (1, 2, 3))

    # issue 29020
    @test repeat(collect(5), outer=(2, 2)) == [5 5;5 5]
    @test repeat(ones(Int64), inner=(1,2), outer=(2,2)) == [1 1 1 1;1 1 1 1]

    # issue 29614
    @test repeat(ones(2, 2), 1, 1, 1) == ones(2, 2, 1)
    @test repeat(ones(2, 2), 2, 2, 2) == ones(4, 4, 2)
    @test repeat(ones(2), 2, 2, 2) == ones(4, 2, 2)
    @test repeat(ones(2, 2), inner=(1, 1, 1), outer=(2, 2, 2)) == ones(4, 4, 2)

    R = repeat([1, 2])
    @test R == [1, 2]
    R = repeat([1, 2], inner=1)
    @test R == [1, 2]
    R = repeat([1, 2], outer=1)
    @test R == [1, 2]
    R = repeat([1, 2], inner=(1,))
    @test R == [1, 2]
    R = repeat([1, 2], outer=(1,))
    @test R == [1, 2]
    R = repeat([1, 2], inner=[1])
    @test R == [1, 2]
    R = repeat([1, 2], outer=[1])
    @test R == [1, 2]
    R = repeat([1, 2], inner=1, outer=1)
    @test R == [1, 2]
    R = repeat([1, 2], inner=(1,), outer=(1,))
    @test R == [1, 2]
    R = repeat([1, 2], inner=[1], outer=[1])
    @test R == [1, 2]

    R = repeat([1, 2], inner=2)
    @test R == [1, 1, 2, 2]
    R = repeat([1, 2], outer=2)
    @test R == [1, 2, 1, 2]
    R = repeat([1, 2], inner=(2,))
    @test R == [1, 1, 2, 2]
    R = repeat([1, 2], outer=(2,))
    @test R == [1, 2, 1, 2]
    R = repeat([1, 2], inner=[2])
    @test R == [1, 1, 2, 2]
    R = repeat([1, 2], outer=[2])
    @test R == [1, 2, 1, 2]

    R = repeat([1, 2], inner=2, outer=2)
    @test R == [1, 1, 2, 2, 1, 1, 2, 2]
    R = repeat([1, 2], inner=(2,), outer=(2,))
    @test R == [1, 1, 2, 2, 1, 1, 2, 2]
    R = repeat([1, 2], inner=[2], outer=[2])
    @test R == [1, 1, 2, 2, 1, 1, 2, 2]

    R = repeat([1, 2], inner = (1, 1), outer = (1, 1))
    @test R == reshape([1, 2], (2,1))
    R = repeat([1, 2], inner = (2, 1), outer = (1, 1))
    @test R == reshape([1, 1, 2, 2], (4,1))
    R = repeat([1, 2], inner = (1, 2), outer = (1, 1))
    @test R == [1 1; 2 2]
    R = repeat([1, 2], inner = (1, 1), outer = (2, 1))
    @test R == reshape([1, 2, 1, 2], (4,1))
    R = repeat([1, 2], inner = (1, 1), outer = (1, 2))
    @test R == [1 1; 2 2]

    R = repeat([1 2;
                3 4], inner = (1, 1), outer = (1, 1))
    @test R == [1 2;
                    3 4]
    R = repeat([1 2;
                3 4], inner = (1, 1), outer = (2, 1))
    @test R == [1 2;
                    3 4;
                    1 2;
                    3 4]
    R = repeat([1 2;
                3 4], inner = (1, 1), outer = (1, 2))
    @test R == [1 2 1 2;
                    3 4 3 4]
    R = repeat([1 2;
                3 4], inner = (1, 1), outer = (2, 2))
    @test R == [1 2 1 2;
                    3 4 3 4;
                    1 2 1 2;
                    3 4 3 4]
    R = repeat([1 2;
                3 4], inner = (2, 1), outer = (1, 1))
    @test R == [1 2;
                    1 2;
                    3 4;
                    3 4]
    R = repeat([1 2;
                3 4], inner = (2, 1), outer = (2, 1))
    @test R == [1 2;
                    1 2;
                    3 4;
                    3 4;
                    1 2;
                    1 2;
                    3 4;
                    3 4]
    R = repeat([1 2;
                3 4], inner = (2, 1), outer = (1, 2))
    @test R == [1 2 1 2;
                    1 2 1 2;
                    3 4 3 4;
                    3 4 3 4;]
    R = repeat([1 2;
                3 4], inner = (2, 1), outer = (2, 2))
    @test R == [1 2 1 2;
                    1 2 1 2;
                    3 4 3 4;
                    3 4 3 4;
                    1 2 1 2;
                    1 2 1 2;
                    3 4 3 4;
                    3 4 3 4]
    R = repeat([1 2;
                3 4], inner = (1, 2), outer = (1, 1))
    @test R == [1 1 2 2;
                    3 3 4 4]
    R = repeat([1 2;
                3 4], inner = (1, 2), outer = (2, 1))
    @test R == [1 1 2 2;
                    3 3 4 4;
                    1 1 2 2;
                    3 3 4 4]
    R = repeat([1 2;
                3 4], inner = (1, 2), outer = (1, 2))
    @test R == [1 1 2 2 1 1 2 2;
                    3 3 4 4 3 3 4 4]
    R = repeat([1 2;
                3 4], inner = (1, 2), outer = (2, 2))
    @test R == [1 1 2 2 1 1 2 2;
                    3 3 4 4 3 3 4 4;
                    1 1 2 2 1 1 2 2;
                    3 3 4 4 3 3 4 4]
    R = repeat([1 2;
                3 4], inner = (2, 2), outer = [1, 1])
    @test R == [1 1 2 2;
                    1 1 2 2;
                    3 3 4 4;
                    3 3 4 4]
    R = repeat([1 2;
                3 4], inner = (2, 2), outer = (2, 1))
    @test R == [1 1 2 2;
                    1 1 2 2;
                    3 3 4 4;
                    3 3 4 4;
                    1 1 2 2;
                    1 1 2 2;
                    3 3 4 4;
                    3 3 4 4]
    R = repeat([1 2;
                3 4], inner = (2, 2), outer = (1, 2))
    @test R == [1 1 2 2 1 1 2 2;
                    1 1 2 2 1 1 2 2;
                    3 3 4 4 3 3 4 4;
                    3 3 4 4 3 3 4 4]
    R = repeat([1 2;
                3 4], inner = (2, 2), outer = (2, 2))
    @test R == [1 1 2 2 1 1 2 2;
                    1 1 2 2 1 1 2 2;
                    3 3 4 4 3 3 4 4;
                    3 3 4 4 3 3 4 4;
                    1 1 2 2 1 1 2 2;
                    1 1 2 2 1 1 2 2;
                    3 3 4 4 3 3 4 4;
                    3 3 4 4 3 3 4 4]
    @test_throws ArgumentError repeat([1 2;
                                        3 4], inner=2, outer=(2, 2))
    @test_throws ArgumentError repeat([1 2;
                                        3 4], inner=(2, 2), outer=2)
    @test_throws ArgumentError repeat([1 2;
                                        3 4], inner=(2,), outer=(2, 2))
    @test_throws ArgumentError repeat([1 2;
                                        3 4], inner=(2, 2), outer=(2,))

    A = reshape(1:8, 2, 2, 2)
    R = repeat(A, inner = (1, 1, 2), outer = (1, 1, 1))
    T = reshape([1:4; 1:4; 5:8; 5:8], 2, 2, 4)
    @test R == T
    A = Array{Int}(undef, 2, 2, 2)
    A[:, :, 1] = [1 2;
                    3 4]
    A[:, :, 2] = [5 6;
                    7 8]
    R = repeat(A, inner = (2, 2, 2), outer = (2, 2, 2))
    @test R[1, 1, 1] == 1
    @test R[2, 2, 2] == 1
    @test R[3, 3, 3] == 8
    @test R[4, 4, 4] == 8
    @test R[5, 5, 5] == 1
    @test R[6, 6, 6] == 1
    @test R[7, 7, 7] == 8
    @test R[8, 8, 8] == 8

    R = repeat(1:2)
    @test R == [1, 2]
    R = repeat(1:2, inner=1)
    @test R == [1, 2]
    R = repeat(1:2, inner=2)
    @test R == [1, 1, 2, 2]
    R = repeat(1:2, outer=1)
    @test R == [1, 2]
    R = repeat(1:2, outer=2)
    @test R == [1, 2, 1, 2]
    R = repeat(1:2, inner=(3,), outer=(2,))
    @test R == [1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2]

    # Arrays of arrays
    @test repeat([[1], [2]], inner=2) == [[1], [1], [2], [2]]
    @test repeat([[1], [2]], outer=2) == [[1], [2], [1], [2]]
    @test repeat([[1], [2]], inner=2, outer=2) == [[1], [1], [2], [2], [1], [1], [2], [2]]

    @test size(repeat([1], inner=(0,))) == (0,)
    @test size(repeat([1], outer=(0,))) == (0,)
    @test size(repeat([1 1], inner=(0, 1))) == (0, 2)
    @test size(repeat([1 1], outer=(1, 0))) == (1, 0)
    @test size(repeat([1 1], inner=(2, 0), outer=(2, 1))) == (4, 0)
    @test size(repeat([1 1], inner=(2, 0), outer=(0, 1))) == (0, 0)

    A = rand(4,4)
    for s in Any[A[1:2:4, 1:2:4], view(A, 1:2:4, 1:2:4)]
        c = cumsum(s, dims=1)
        @test c[1,1] == A[1,1]
        @test c[2,1] == A[1,1]+A[3,1]
        @test c[1,2] == A[1,3]
        @test c[2,2] == A[1,3]+A[3,3]

        c = cumsum(s, dims=2)
        @test c[1,1] == A[1,1]
        @test c[2,1] == A[3,1]
        @test c[1,2] == A[1,1]+A[1,3]
        @test c[2,2] == A[3,1]+A[3,3]
    end

    @test repeat(BitMatrix(Matrix(I, 2, 2)), inner = (2,1), outer = (1,2)) ==
            repeat(Matrix(I, 2, 2), inner = (2,1), outer = (1,2))
end

@testset "indexing with bools" begin
    @test (1:5)[[true,false,true,false,true]] == [1,3,5]
    @test [1:5;][[true,false,true,false,true]] == [1,3,5]
    @test_throws BoundsError (1:5)[[true,false,true,false]]
    @test_throws BoundsError (1:5)[[true,false,true,false,true,false]]
    @test_throws BoundsError [1:5;][[true,false,true,false]]
    @test_throws BoundsError [1:5;][[true,false,true,false,true,false]]
    a = [1:5;]
    a[[true,false,true,false,true]] .= 6
    @test a == [6,2,6,4,6]
    a[[true,false,true,false,true]] = [7,8,9]
    @test a == [7,2,8,4,9]
    @test_throws DimensionMismatch (a[[true,false,true,false,true]] = [7,8,9,10])
    A = reshape(1:15, 3, 5)
    @test A[[true, false, true], [false, false, true, true, false]] == [7 10; 9 12]
    @test_throws BoundsError A[[true, false], [false, false, true, true, false]]
    @test_throws BoundsError A[[true, false, true], [false, true, true, false]]
    @test_throws BoundsError A[[true, false, true, true], [false, false, true, true, false]]
    @test_throws BoundsError A[[true, false, true], [false, false, true, true, false, true]]
    A = fill(1, 3, 5)
    @test_throws DimensionMismatch A[2,[true, false, true, true, false]] = 2:5
    A[2,[true, false, true, true, false]] = 2:4
    @test A == [1 1 1 1 1; 2 1 3 4 1; 1 1 1 1 1]
    @test_throws DimensionMismatch A[[true,false,true], 5] = [19]
    @test_throws DimensionMismatch A[[true,false,true], 5] = 19:21
    A[[true,false,true], 5] .= 7
    @test A == [1 1 1 1 7; 2 1 3 4 1; 1 1 1 1 7]

    B = cat(1, 2, 3; dims=3)
    @test B[:,:,[true, false, true]] == reshape([1,3], 1, 1, 2)  # issue #5454
end

# issue #2342
@test isequal(cumsum([1 2 3], dims=1), [1 2 3])

@testset "set-like operations" begin
    @test isequal(union([1,2,3], [4,3,4]), [1,2,3,4])
    @test isequal(union(['e','c','a'], ['b','a','d']), ['e','c','a','b','d'])
    @test isequal(union([1,2,3], [4,3], [5]), [1,2,3,4,5])
    @test isequal(union([1,2,3]), [1,2,3])
    @test isequal(union([1,2,3], Int64[]), Int64[1,2,3])
    @test isequal(union([1,2,3], Float64[]), Float64[1.0,2,3])
    @test isequal(union(Int64[], [1,2,3]), Int64[1,2,3])
    @test isequal(union(Int64[]), Int64[])
    @test isequal(intersect([1,2,3], [4,3,4]), [3])
    @test isequal(intersect(['e','c','a'], ['b','a','d']), ['a'])
    @test isequal(intersect([1,2,3], [3,1], [2,1,3]), [1,3])
    @test isequal(intersect([1,2,3]), [1,2,3])
    @test isequal(intersect([1,2,3], Int64[]), Int64[])
    @test isequal(intersect([1,2,3], Float64[]), Float64[])
    @test isequal(intersect(Int64[], [1,2,3]), Int64[])
    @test isequal(intersect(Int64[]), Int64[])
    @test isequal(setdiff([1,2,3,4], [2,5,4]), [1,3])
    @test isequal(setdiff([1,2,3,4], [7,8,9]), [1,2,3,4])
    @test isequal(setdiff([1,2,3,4], Int64[]), Int64[1,2,3,4])
    @test isequal(setdiff([1,2,3,4], [1,2,3,4,5]), Int64[])
    @test isequal(symdiff([1,2,3], [4,3,4]), [1,2])
    @test isequal(symdiff(['e','c','a'], ['b','a','d']), ['e','c','b','d'])
    @test isequal(symdiff([1,2,3], [4,3], [5]), [1,2,4,5])
    @test isequal(symdiff([1,2,3,4,5], [1,2,3], [3,4]), [3,5])
    @test isequal(symdiff([1,2,3]), [1,2,3])
    @test isequal(symdiff([1,2,3], Int64[]), Int64[1,2,3])
    @test isequal(symdiff([1,2,3], Float64[]), Float64[1.0,2,3])
    @test isequal(symdiff(Int64[], [1,2,3]), Int64[1,2,3])
    @test isequal(symdiff(Int64[]), Int64[])
    @testset "tuples" begin # Issue #25338
        @test union((1, 2), (3)) == [1, 2, 3]
        u = union((1, 0x2), [3])
        @test eltype(u) == Integer
        @test u == [1, 2, 3]
        @test intersect((1, 2), (3, 2)) == [2]
        @test setdiff((1, 2), (3, 2)) == [1]
        @test symdiff((1, 2), (3, 2)) == [1, 3]
    end
end

@testset "mapslices" begin
    local a, b, c, m, h, s
    a = rand(5,5)
    s = mapslices(sort, a, dims=[1])
    S = mapslices(sort, a, dims=[2])
    for i = 1:5
        @test s[:,i] == sort(a[:,i])
        @test vec(S[i,:]) == sort(vec(a[i,:]))
    end

    # issue #3613
    b = mapslices(sum, fill(1.,2,3,4), dims=[1,2])
    @test size(b) === (1,1,4)
    @test all(b.==6)

    # issue #5141
    ## Update Removed the version that removes the dimensions when dims==1:ndims(A)
    c1 = mapslices(x-> maximum(-x), a, dims=[])
    @test c1 == -a

    # other types than Number
    @test mapslices(prod,["1" "2"; "3" "4"],dims=1) == ["13" "24"]
    @test mapslices(prod,["1"],dims=1) == ["1"]

    # issue #5177

    c = fill(1,2,3,4)
    m1 = mapslices(x-> fill(1,2,3), c, dims=[1,2])
    m2 = mapslices(x-> fill(1,2,4), c, dims=[1,3])
    m3 = mapslices(x-> fill(1,3,4), c, dims=[2,3])
    @test size(m1) == size(m2) == size(m3) == size(c)

    n1 = mapslices(x-> fill(1,6), c, dims=[1,2])
    n2 = mapslices(x-> fill(1,6), c, dims=[1,3])
    n3 = mapslices(x-> fill(1,6), c, dims=[2,3])
    n1a = mapslices(x-> fill(1,1,6), c, dims=[1,2])
    n2a = mapslices(x-> fill(1,1,6), c, dims=[1,3])
    n3a = mapslices(x-> fill(1,1,6), c, dims=[2,3])
    @test size(n1a) == (1,6,4) && size(n2a) == (1,3,6)  && size(n3a) == (2,1,6)
    @test size(n1) == (6,1,4) && size(n2) == (6,3,1)  && size(n3) == (2,6,1)

    # mutating functions
    o = fill(1, 3, 4)
    m = mapslices(x->fill!(x, 0), o, dims=2)
    @test m == zeros(3, 4)
    @test o == fill(1, 3, 4)

    # issue #18524
    m = mapslices(x->tuple(x), [1 2; 3 4], dims=1)
    @test m[1,1] == ([1,3],)
    @test m[1,2] == ([2,4],)

    # issue #21123
    @test mapslices(nnz, sparse(1.0I, 3, 3), dims=1) == [1 1 1]
end

@testset "single multidimensional index" begin
    a = rand(6,6)
    I = [1 4 5; 4 2 6; 5 6 3]
    a2 = a[I]
    @test size(a2) == size(I)
    for i = 1:length(a2)
        @test a2[i] == a[I[i]]
    end
    a = [1,3,5]
    b = [1 3]
    a[b] .= 8
    @test a == [8,3,8]
end

@testset "assigning an array into itself and other aliasing issues" begin
    a = [1,3,5]
    b = [3,1,2]
    a[b] = a
    @test a == [3,5,1]
    a = [3,2,1]
    a[a] = [4,5,6]
    @test a == [6,5,4]

    A = [1,2,3,4]
    V = view(A, A)
    @test V == A
    V[1] = 2
    @test V == A == [2,2,3,4]
    V[1] = 2^30
    @test V == A == [2^30, 2, 3, 4]

    A = [2,1,4,3]
    V = view(A, :)
    A[V] = (1:4) .+ 2^30
    @test A == [2,1,4,3] .+ 2^30

    A = [2,1,4,3]
    R = reshape(view(A, :), 2, 2)
    A[R] = (1:4) .+ 2^30
    @test A == [2,1,4,3] .+ 2^30

    A = [2,1,4,3]
    R = reshape(A, 2, 2)
    A[R] = (1:4) .+ 2^30
    @test A == [2,1,4,3] .+ 2^30

    # And broadcasting
    a = [1,3,5]
    b = [3,1,2]
    a[b] .= a
    @test a == [3,5,1]
    a = [3,2,1]
    a[a] .= [4,5,6]
    @test a == [6,5,4]

    A = [2,1,4,3]
    V = view(A, :)
    A[V] .= (1:4) .+ 2^30
    @test A == [2,1,4,3] .+ 2^30

    A = [2,1,4,3]
    R = reshape(view(A, :), 2, 2)
    A[R] .= reshape((1:4) .+ 2^30, 2, 2)
    @test A == [2,1,4,3] .+ 2^30

    A = [2,1,4,3]
    R = reshape(A, 2, 2)
    A[R] .= reshape((1:4) .+ 2^30, 2, 2)
    @test A == [2,1,4,3] .+ 2^30
end

@testset "Base.mightalias unit tests" begin
    using Base: mightalias
    A = rand(5,4)
    @test @views mightalias(A[:], A[:])
    @test @views mightalias(A[:,:], A[:,:])
    @test @views mightalias(A[1:2,1:2], A[1:2,1:2])
    @test @views !mightalias(A[3:4,1:2], A[1:2,:])
    @test @views mightalias(A[3,1:1], A)
    @test @views mightalias(A[3,1:1], A[:])
    @test @views mightalias(A[3,1:1], A[:,:])
    @test @views mightalias(A, A[3,1:1])
    @test @views mightalias(A[:], A[3,1:1])
    @test @views mightalias(A[:,:], A[3,1:1])

    B = reshape(A,10,2)
    @test mightalias(A, A)
    @test mightalias(A, B)
    @test mightalias(B, A)
    @test @views mightalias(B[:], A[:])
    @test @views mightalias(B[1:2], A[1:2])
    @test @views !mightalias(B[1:end÷2], A[end÷2+1:end])

    AA = [[1],[2]]
    @test @views mightalias(AA, AA[:])
    @test @views mightalias(AA[:], AA[:])
    @test @views mightalias(AA[1:1], AA[1:2])
end

@testset "lexicographic comparison" begin
    @test cmp([1.0], [1]) == 0
    @test cmp([1], [1.0]) == 0
    @test cmp([1, 1], [1, 1]) == 0
    @test cmp([1, 1], [2, 1]) == -1
    @test cmp([2, 1], [1, 1]) == 1
    @test cmp([1, 1], [1, 2]) == -1
    @test cmp([1, 2], [1, 1]) == 1
    @test cmp([1], [1, 1]) == -1
    @test cmp([1, 1], [1]) == 1
end

@testset "sort on arrays" begin
    local a = rand(3,3)

    asr = sortslices(a, dims=1)
    @test isless(asr[1,:],asr[2,:])
    @test isless(asr[2,:],asr[3,:])

    asc = sortslices(a, dims=2)
    @test isless(asc[:,1],asc[:,2])
    @test isless(asc[:,2],asc[:,3])

    # mutating functions
    o = fill(1, 3, 4)
    m = mapslices(x->fill!(x, 0), o, dims=2)
    @test m == zeros(3, 4)
    @test o == fill(1, 3, 4)

    asr = sortslices(a, dims=1, rev=true)
    @test isless(asr[2,:],asr[1,:])
    @test isless(asr[3,:],asr[2,:])

    asc = sortslices(a, dims=2, rev=true)
    @test isless(asc[:,2],asc[:,1])
    @test isless(asc[:,3],asc[:,2])

    as = sort(a, dims=1)
    @test issorted(as[:,1])
    @test issorted(as[:,2])
    @test issorted(as[:,3])
    @test sort!(copy(a), dims=1) == as

    as = sort(a, dims=2)
    @test issorted(as[1,:])
    @test issorted(as[2,:])
    @test issorted(as[3,:])
    @test sort!(copy(a), dims=2) == as

    local b = rand(21,21,2)

    bs = sort(b, dims=1)
    for i in 1:21
        @test issorted(bs[:,i,1])
        @test issorted(bs[:,i,2])
    end
    @test sort!(copy(b), dims=1) == bs

    bs = sort(b, dims=2)
    for i in 1:21
        @test issorted(bs[i,:,1])
        @test issorted(bs[i,:,2])
    end
    @test sort!(copy(b), dims=2) == bs

    bs = sort(b, dims=3)
    @test all(bs[:,:,1] .<= bs[:,:,2])
    @test sort!(copy(b), dims=3) == bs
end

@testset "higher dimensional sortslices" begin
    A = permutedims(reshape([4 3; 2 1; 'A' 'B'; 'C' 'D'], (2, 2, 2)), (1, 3, 2))
    @test sortslices(A, dims=(1, 2)) ==
        permutedims(reshape([1 3; 2 4; 'D' 'B'; 'C' 'A'], (2, 2, 2)), (1, 3, 2))
    @test sortslices(A, dims=(2, 1)) ==
        permutedims(reshape([1 2; 3 4; 'D' 'C'; 'B' 'A'], (2, 2, 2)), (1, 3, 2))
    B = reshape(1:8, (2,2,2))
    @test sortslices(B, dims=(3,1))[:, :, 1] == [
        1 3;
        5 7
    ]
    @test sortslices(B, dims=(1,3)) == B
end

@testset "fill" begin
    @test fill!(Float64[1.0], -0.0)[1] === -0.0
    A = fill(1.,3,3)
    S = view(A, 2, 1:3)
    fill!(S, 2)
    S = view(A, 1:2, 3)
    fill!(S, 3)
    @test A == [1 1 3; 2 2 3; 1 1 1]
    rt = Base.return_types(fill!, Tuple{Array{Int32, 3}, UInt8})
    @test length(rt) == 1 && rt[1] == Array{Int32, 3}
    A = Vector{Union{UInt8,Int8}}(undef, 3)
    fill!(A, UInt8(3))
    @test A == [0x03, 0x03, 0x03]
    # Issue #9964
    A = Array{Vector{Float64}}(undef, 2)
    fill!(A, [1, 2])
    @test A[1] == [1, 2]
    @test A[1] === A[2]
    # byte arrays
    @test_throws InexactError fill!(UInt8[0], -1)
    @test_throws InexactError fill!(UInt8[0], 300)
    @test_throws InexactError fill!(Int8[0], 200)
    @test fill!(UInt8[0,0], 200) == [200,200]
    @test fill!(Int8[0,0], -2) == [-2,-2]
end

@testset "splice!" begin
    for idx in Any[1, 2, 5, 9, 10, 1:0, 2:1, 1:1, 2:2, 1:2, 2:4, 9:8, 10:9, 9:9, 10:10,
                   8:9, 9:10, 6:9, 7:10]
        for repl in Any[[], [11], [11,22], [11,22,33,44,55]]
            local a
            a = [1:10;]
            acopy = copy(a)
            @test splice!(a, idx, repl) == acopy[idx]
            @test a == [acopy[1:(first(idx)-1)]; repl; acopy[(last(idx)+1):end]]
        end
    end
end

@testset "filter!" begin
    # base case w/ Vector
    a = Vector(1:10)
    filter!(x -> x > 5, a)
    @test a == 6:10

    # different subtype of AbstractVector
    ba = rand(10) .> 0.5
    @test isa(ba, BitArray)
    filter!(x -> x, ba)
    @test all(ba)

    # empty array
    ea = []
    filter!(x -> x > 5, ea)
    @test isempty(ea)

    # non-1-indexed array
    oa = OffsetArray(Vector(1:10), -5)
    filter!(x -> x > 5, oa)
    @test oa == OffsetArray(Vector(6:10), -5)

    # empty non-1-indexed array
    eoa = OffsetArray([], -5)
    filter!(x -> x > 5, eoa)
    @test isempty(eoa)
end

@testset "deleteat!" begin
    for idx in Any[1, 2, 5, 9, 10, 1:0, 2:1, 1:1, 2:2, 1:2, 2:4, 9:8, 10:9, 9:9, 10:10,
                   8:9, 9:10, 6:9, 7:10]
        # integer indexing with AbstractArray
        a = [1:10;]; acopy = copy(a)
        @test deleteat!(a, idx) == [acopy[1:(first(idx)-1)]; acopy[(last(idx)+1):end]]

        # integer indexing with non-AbstractArray iterable
        a = [1:10;]; acopy = copy(a)
        @test deleteat!(a, (i for i in idx)) == [acopy[1:(first(idx)-1)]; acopy[(last(idx)+1):end]]

        # logical indexing
        a = [1:10;]; acopy = copy(a)
        @test deleteat!(a, map(in(idx), 1:length(a))) == [acopy[1:(first(idx)-1)]; acopy[(last(idx)+1):end]]
    end
    a = [1:10;]
    @test deleteat!(a, 11:10) == [1:10;]
    @test deleteat!(a, [1,3,5,7:10...]) == [2,4,6]
    @test_throws BoundsError deleteat!(a, 13)
    @test_throws BoundsError deleteat!(a, [1,13])
    @test_throws ArgumentError deleteat!(a, [5,3])
    @test_throws BoundsError deleteat!(a, 5:20)
    @test_throws BoundsError deleteat!(a, Bool[])
    @test_throws BoundsError deleteat!(a, [true])
    @test_throws BoundsError deleteat!(a, falses(11))

    @test_throws BoundsError deleteat!([], 1)
    @test_throws BoundsError deleteat!([], [1])
    @test_throws BoundsError deleteat!([], [2])
    @test deleteat!([], []) == []
    @test deleteat!([], Bool[]) == []
end

@testset "comprehensions" begin
    X = [ i+2j for i=1:5, j=1:5 ]
    @test X[2,3] == 8
    @test X[4,5] == 14
    @test isequal(fill(3.,2,2), [3. 3.; 3. 3.])
    # @test isequal([ [1,2] for i=1:2, : ], [1 2; 1 2])
    # where element type is a Union. try to confuse type inference.
    foo32_64(x) = (x<2) ? Int32(x) : Int64(x)
    boo32_64() = [ foo32_64(i) for i=1:2 ]
    let a36 = boo32_64()
        @test a36[1]==1 && a36[2]==2
    end
    @test isequal([1,2,3], [a for (a,b) in enumerate(2:4)])
    @test isequal([2,3,4], [b for (a,b) in enumerate(2:4)])

    @test [s for s in Union{String, Nothing}["a", nothing]] isa Vector{Union{String, Nothing}}
    @test [s for s in Union{String, Missing}["a", missing]] isa Vector{Union{String, Missing}}

    @testset "comprehension in let-bound function" begin
        let x⊙y = sum([x[i]*y[i] for i=1:length(x)])
            @test [1,2] ⊙ [3,4] == 11
        end

        @test_throws DomainError (10 .^ [-1])[1] == 0.1
        @test (10 .^ [-1.])[1] == 0.1
    end
end

# issue #24002
module I24002
s1() = 1
y = Int[i for i in 1:10]
end
@test I24002.y == [1:10;]
@test I24002.s1() == 1

@testset "pairs" begin
    A14 = [11 13; 12 14]
    @test [a for (a,b) in pairs(IndexLinear(),    A14)] == LinearIndices(axes(A14)) == [1 3; 2 4]
    @test [a for (a,b) in pairs(IndexCartesian(), A14)] == CartesianIndices(axes(A14)) == CartesianIndex.(1:2, reshape(1:2, 1, :))
    @test [b for (a,b) in pairs(IndexLinear(),    A14)] == A14
    @test [b for (a,b) in pairs(IndexCartesian(), A14)] == A14
end

@testset "reverse" begin
    @test reverse([2,3,1]) == [1,3,2]
    @test reverse([1:10;],1,4) == [4,3,2,1,5,6,7,8,9,10]
    @test reverse([1:10;],3,6) == [1,2,6,5,4,3,7,8,9,10]
    @test reverse([1:10;],6,10) == [1,2,3,4,5,10,9,8,7,6]
    @test reverse(1:10,1,4) == [4,3,2,1,5,6,7,8,9,10]
    @test reverse(1:10,3,6) == [1,2,6,5,4,3,7,8,9,10]
    @test reverse(1:10,6,10) == [1,2,3,4,5,10,9,8,7,6]
    @test reverse!([1:10;]) == [10,9,8,7,6,5,4,3,2,1]
    @test reverse!([1:10;],1,4) == [4,3,2,1,5,6,7,8,9,10]
    @test reverse!([1:10;],3,6) == [1,2,6,5,4,3,7,8,9,10]
    @test reverse!([1:10;],6,10) == [1,2,3,4,5,10,9,8,7,6]
    @test reverse!([1:10;], 11) == [1:10;]
    @test_throws BoundsError reverse!([1:10;], 1, 11)
    @test reverse!(Any[]) == Any[]
end

@testset "reverse dim" begin
    @test isequal(reverse([2,3,1], dims=1), [1,3,2])
    @test_throws ArgumentError reverse([2,3,1], dims=2)
    @test isequal(reverse([2 3 1], dims=1), [2 3 1])
    @test isequal(reverse([2 3 1], dims=2), [1 3 2])
    @test_throws ArgumentError reverse([2,3,1], dims=-1)
    @test isequal(reverse(1:10, dims=1), 10:-1:1)
    @test_throws ArgumentError reverse(1:10, dims=2)
    @test_throws ArgumentError reverse(1:10, dims=-1)
    @test isequal(reverse(Matrix{Int}(undef, 0,0),dims=1), Matrix{Int}(undef, 0,0))  # issue #5872

    a = rand(5,3)
    @test reverse(reverse(a,dims=2),dims=2) == a
    @test_throws ArgumentError reverse(a,dims=3)
    # reversed dimension is not a singleton
    # a lower dimension is not a singleton
    # eltype not allocated inline
    @test reverse(["a" "b"; "c" "d"], dims = 2) == ["b" "a"; "d" "c"]
end

@testset "isdiag, istril, istriu" begin
    @test isdiag(3)
    @test istril(4)
    @test istriu(5)
    @test !isdiag([1 2; 3 4])
    @test !istril([1 2; 3 4])
    @test !istriu([1 2; 3 4])
    @test isdiag([1 0; 0 4])
    @test istril([1 0; 3 4])
    @test istriu([1 2; 0 4])
end

# issue 4228
let A = [[i i; i i] for i=1:2]
    @test cumsum(A) == Any[[1 1; 1 1], [3 3; 3 3]]
    @test cumprod(A) == Any[[1 1; 1 1], [4 4; 4 4]]
end

@testset "prepend/append" begin
    # PR #4627
    A = [1,2]
    @test append!(A, A) == [1,2,1,2]
    @test prepend!(A, A) == [1,2,1,2,1,2,1,2]

    # iterators with length:
    @test append!([1,2], (9,8)) == [1,2,9,8] == push!([1,2], (9,8)...)
    @test prepend!([1,2], (9,8)) == [9,8,1,2] == pushfirst!([1,2], (9,8)...)
    @test append!([1,2], ()) == [1,2] == prepend!([1,2], ())
    # iterators without length:
    g = (i for i = 1:10 if iseven(i))
    @test append!([1,2], g) == [1,2,2,4,6,8,10] == push!([1,2], g...)
    @test prepend!([1,2], g) == [2,4,6,8,10,1,2] == pushfirst!([1,2], g...)
    g = (i for i = 1:2:10 if iseven(i)) # isempty(g) == true
    @test append!([1,2], g) == [1,2] == push!([1,2], g...)
    @test prepend!([1,2], g) == [1,2] == pushfirst!([1,2], g...)

    # offset array
    @test append!([1,2], OffsetArray([9,8], (-3,))) == [1,2,9,8]
    @test prepend!([1,2], OffsetArray([9,8], (-3,))) == [9,8,1,2]
end

let A = [1,2]
    s = Set([1,2,3])
    @test sort(append!(A, s)) == [1,1,2,2,3]
end

@testset "cases where shared arrays can/can't be grown" begin
    A = [1 3;2 4]
    B = reshape(A, 4)
    @test push!(B,5) == [1,2,3,4,5]
    @test pop!(B) == 5
    C = reshape(B, 1, 4)
    @test_throws MethodError push!(C, 5)

    A = [NaN]; B = [NaN]
    @test !(A==A)
    @test isequal(A,A)
    @test A===A
    @test !(A==B)
    @test isequal(A,B)
    @test A!==B
end
# complete testsuite for reducedim

# Inferred types
Nmax = 3 # TODO: go up to CARTESIAN_DIMS+2 (currently this exposes problems)
for N = 1:Nmax
    #indexing with (UnitRange, UnitRange, UnitRange)
    args = ntuple(d->UnitRange{Int}, N)
    @test Base.return_types(getindex, Tuple{Array{Float32, N}, args...}) == [Array{Float32, N}]
    @test Base.return_types(getindex, Tuple{BitArray{N}, args...}) == Any[BitArray{N}]
    @test Base.return_types(setindex!, Tuple{Array{Float32, N}, Array{Int, 1}, args...}) == [Array{Float32, N}]
    # Indexing with (UnitRange, UnitRange, Int)
    args = ntuple(d->d<N ? UnitRange{Int} : Int, N)
    N > 1 && @test Base.return_types(getindex, Tuple{Array{Float32, N}, args...}) == [Array{Float32, N-1}]
    N > 1 && @test Base.return_types(getindex, Tuple{BitArray{N}, args...}) == [BitArray{N-1}]
    N > 1 && @test Base.return_types(setindex!, Tuple{Array{Float32, N}, Array{Int, 1}, args...}) == [Array{Float32, N}]
end

# issue #6645 (32-bit)
let x = Float64[]
    for i = 1:5
        push!(x, 1.0)
    end
    @test dot(zeros(5), x) == 0.0
end

# issue #6977
@test size([]') == (1,0)

# issue #6996
@test copy(adjoint(Any[ 1 2; 3 4 ])) == copy(transpose(Any[ 1 2; 3 4 ]))

# map with promotion (issue #6541)
@test map(join, ["z", "я"]) == ["z", "я"]

# Handle block matrices
let A = [randn(2, 2) for i = 1:2, j = 1:2]
    @test issymmetric(A'A)
end
let A = [complex.(randn(2, 2), randn(2, 2)) for i = 1:2, j = 1:2]
    @test ishermitian(A'A)
end

# issue #7197
function i7197()
    S = [1 2 3; 4 5 6; 7 8 9]
    Base._ind2sub(size(S), 5)
end
@test i7197() == (2,2)

# PR #8622 and general indexin tests
@test indexin([1,3,5,7], [5,4,3]) == [nothing,3,1,nothing]
@test indexin([1 3; 5 7], [5 4; 3 2]) == [nothing CartesianIndex(2, 1); CartesianIndex(1, 1) nothing]
@test indexin((2 * x + 1 for x in 0:3), [5,4,3,5,6]) == [nothing,3,1,nothing]
@test indexin(6, [1,3,6,6,2]) == fill(3, ())
@test indexin([6], [1,3,6,6,2]) == [3]
@test indexin([3], 2:5) == [2]
@test indexin([3.0], 2:5) == [2]

#6828 - size of specific dimensions
let a = Array{Float64}(undef, 10)
    @test size(a) == (10,)
    @test size(a, 1) == 10
    @test (size(a,2), size(a,1)) == (1,10)
    aa = Array{Float64}(undef, 2,3)
    @test size(aa) == (2,3)
    @test (size(aa,4), size(aa,3), size(aa,2), size(aa,1)) == (1,1,3,2)
    aaa = Array{Float64}(undef, 9,8,7,6,5,4,3,2,1)
    @test size(aaa,1) == 9
    @test size(aaa,4) == 6
    @test size(aaa) == (9,8,7,6,5,4,3,2,1)

    #18459 Test Array{T, N} constructor
    b = Vector{Float64}(undef, 10)
    @test size(a) == size(b)
    bb = Matrix{Float64}(undef, 2,3)
    @test size(aa) == size(bb)
    bbb = Array{Float64,9}(undef, 9,8,7,6,5,4,3,2,1)
    @test size(aaa) == size(bbb)
end

# Cartesian
function cartesian_foo()
    Base.@nexprs 2 d->(a_d_d = d)
    a_2_2
end
@test cartesian_foo() == 2

@testset "Multidimensional iterators" begin
    for a in ([1:5;], reshape([2]))
        counter = 0
        for I in eachindex(a)
            counter += 1
        end
        @test counter == length(a)
        counter = 0
        for aa in a
            counter += 1
        end
        @test counter == length(a)
    end
end

function mdsum(A)
    s = 0.0
    for a in A
        s += a
    end
    s
end

function mdsum2(A)
    s = 0.0
    @inbounds for I in eachindex(A)
        s += A[I]
    end
    s
end

@testset "linear indexing" begin
    a = [1:5;]
    @test isa(Base.IndexStyle(a), Base.IndexLinear)
    b = view(a, :)
    @test isa(Base.IndexStyle(b), Base.IndexLinear)
    @test isa(Base.IndexStyle(trues(2)), Base.IndexLinear)
    @test isa(Base.IndexStyle(BitArray{2}), Base.IndexLinear)
    aa = fill(99, 10)
    aa[1:2:9] = a
    shp = [5]
    for i = 1:10
        A = reshape(a, tuple(shp...))
        @test mdsum(A) == 15
        @test mdsum2(A) == 15
        AA = reshape(aa, tuple(2, shp...))
        B = view(AA, 1:1, ntuple(i->Colon(), i)...)
        @test isa(Base.IndexStyle(B), Base.IteratorsMD.IndexCartesian)
        @test mdsum(B) == 15
        @test mdsum2(B) == 15
        pushfirst!(shp, 1)
    end

    a = [1:10;]
    shp = [2,5]
    for i = 2:10
        A = reshape(a, tuple(shp...))
        @test mdsum(A) == 55
        @test mdsum2(A) == 55
        B = view(A, ntuple(i->Colon(), i)...)
        @test mdsum(B) == 55
        @test mdsum2(B) == 55
        insert!(shp, 2, 1)
    end

    a = reshape([2])
    @test mdsum(a) == 2
    @test mdsum2(a) == 2

    a = Matrix{Float64}(undef,0,5)
    b = view(a, :, :)
    @test mdsum(b) == 0
    a = Matrix{Float64}(undef,5,0)
    b = view(a, :, :)
    @test mdsum(b) == 0
end
@testset "CartesianIndex" begin
    for a in (copy(reshape(1:60, 3, 4, 5)),
              view(copy(reshape(1:60, 3, 4, 5)), 1:3, :, :),
              view(copy(reshape(1:60, 3, 4, 5)), CartesianIndex.(1:3, (1:4)'), :),
              view(copy(reshape(1:60, 3, 4, 5)), :, CartesianIndex.(1:4, (1:5)')))
        @test a[CartesianIndex{3}(2,3,4)] == 44
        a[CartesianIndex{3}(2,3,3)] = -1
        @test a[CartesianIndex{3}(2,3,3)] == -1
        @test a[2,CartesianIndex{2}(3,4)] == 44
        a[1,CartesianIndex{2}(3,4)] = -2
        @test a[1,CartesianIndex{2}(3,4)] == -2
        @test a[CartesianIndex{1}(2),3,CartesianIndex{1}(4)] == 44
        a[CartesianIndex{1}(2),3,CartesianIndex{1}(3)] = -3
        @test a[CartesianIndex{1}(2),3,CartesianIndex{1}(3)] == -3

        @test a[:, :, CartesianIndex((1,))] == (@view a[:, :, CartesianIndex((1,))]) == a[:,:,1]
        @test a[CartesianIndex((1,)), [1,2], :] == (@view a[CartesianIndex((1,)), [1,2], :]) == a[1,[1,2],:]
        @test a[CartesianIndex((2,)), 3:4, :] == (@view a[CartesianIndex((2,)), 3:4, :]) == a[2,3:4,:]
        @test a[[CartesianIndex(1,3),CartesianIndex(2,4)],3:3] ==
              (@view a[[CartesianIndex(1,3),CartesianIndex(2,4)],3:3]) == reshape([a[1,3,3]; a[2,4,3]], 2, 1)

        @test a[[CartesianIndex()], :, :, :] == (@view a[[CartesianIndex()], :, :, :]) == reshape(a, 1, 3, 4, 5)
        @test a[:, [CartesianIndex()], :, :] == (@view a[:, [CartesianIndex()], :, :]) == reshape(a, 3, 1, 4, 5)
        @test a[:, :, [CartesianIndex()], :] == (@view a[:, :, [CartesianIndex()], :]) == reshape(a, 3, 4, 1, 5)
        @test a[:, :, :, [CartesianIndex()]] == (@view a[:, :, :, [CartesianIndex()]]) == reshape(a, 3, 4, 5, 1)
        a2 = reshape(a, Val(2))
        @test a2[[CartesianIndex()], :, :]   == (@view a2[[CartesianIndex()], :, :])   == reshape(a, 1, 3, 20)
        @test a2[:, [CartesianIndex()], :]   == (@view a2[:, [CartesianIndex()], :])   == reshape(a, 3, 1, 20)
        @test a2[:, :, [CartesianIndex()]]   == (@view a2[:, :, [CartesianIndex()]])   == reshape(a, 3, 20, 1)
        a1 = reshape(a, Val(1))
        @test a1[[CartesianIndex()], :]      == (@view a1[[CartesianIndex()], :])      == reshape(a, 1, 60)
        @test a1[:, [CartesianIndex()]]      == (@view a1[:, [CartesianIndex()]])      == reshape(a, 60, 1)

        @test_throws BoundsError a[[CartesianIndex(1,5),CartesianIndex(2,4)],3:3]
        @test_throws BoundsError a[1:4, [CartesianIndex(1,3),CartesianIndex(2,4)]]
        @test_throws BoundsError @view a[[CartesianIndex(1,5),CartesianIndex(2,4)],3:3]
        @test_throws BoundsError @view a[1:4, [CartesianIndex(1,3),CartesianIndex(2,4)]]
    end

    for a in (view(zeros(3, 4, 5), :, :, :),
              view(zeros(3, 4, 5), 1:3, :, :))
        a[CartesianIndex{3}(2,3,3)] = -1
        @test a[CartesianIndex{3}(2,3,3)] == -1
        a[1,CartesianIndex{2}(3,4)] = -2
        @test a[1,CartesianIndex{2}(3,4)] == -2
        a[CartesianIndex{1}(2),3,CartesianIndex{1}(3)] = -3
        @test a[CartesianIndex{1}(2),3,CartesianIndex{1}(3)] == -3
        a[[CartesianIndex(1,3),CartesianIndex(2,4)],3:3] .= -4
        @test a[1,3,3] == -4
        @test a[2,4,3] == -4
    end

    I1 = CartesianIndex((2,3,0))
    I2 = CartesianIndex((-1,5,2))
    @test -I1 == CartesianIndex((-2,-3,0))
    @test I1 + I2 == CartesianIndex((1,8,2))
    @test I2 + I1 == CartesianIndex((1,8,2))
    @test I1 - I2 == CartesianIndex((3,-2,-2))
    @test I2 - I1 == CartesianIndex((-3,2,2))
    @test I1 + 1*oneunit(I1) == CartesianIndex((3,4,1))
    @test I1 - 2*oneunit(I1) == CartesianIndex((0,1,-2))

    @test zero(CartesianIndex{2}) == CartesianIndex((0,0))
    @test zero(CartesianIndex((2,3))) == CartesianIndex((0,0))
    @test oneunit(CartesianIndex{2}) == CartesianIndex((1,1))
    @test oneunit(CartesianIndex((2,3))) == CartesianIndex((1,1))

    @test min(CartesianIndex((2,3)), CartesianIndex((5,2))) == CartesianIndex((2,2))
    @test max(CartesianIndex((2,3)), CartesianIndex((5,2))) == CartesianIndex((5,3))

    @test Tuple(I1) == (2,3,0)

    # CartesianIndex allows construction at a particular dimensionality
    @test length(CartesianIndex{3}()) == 3
    @test length(CartesianIndex{3}(1,2)) == 3
    @test length(CartesianIndex{3}((1,2))) == 3
    @test length(CartesianIndex{3}(1,2,3)) == 3
    @test length(CartesianIndex{3}((1,2,3))) == 3
    @test_throws ArgumentError CartesianIndex{3}(1,2,3,4)
    @test_throws ArgumentError CartesianIndex{3}((1,2,3,4))

    @test length(I1) == 3

    @test isless(CartesianIndex((1,1)), CartesianIndex((2,1)))
    @test isless(CartesianIndex((1,1)), CartesianIndex((1,2)))
    @test isless(CartesianIndex((2,1)), CartesianIndex((1,2)))
    @test !isless(CartesianIndex((1,2)), CartesianIndex((2,1)))

    a = spzeros(2,3)
    @test CartesianIndices(size(a)) == eachindex(a)
    a[CartesianIndex{2}(2,3)] = 5
    @test a[2,3] == 5
    b = view(a, 1:2, 2:3)
    b[CartesianIndex{2}(1,1)] = 7
    @test a[1,2] == 7
    @test 2*CartesianIndex{3}(1,2,3) == CartesianIndex{3}(2,4,6)
    @test CartesianIndex{3}(1,2,3)*2 == CartesianIndex{3}(2,4,6)
    @test_throws ErrorException iterate(CartesianIndex{3}(1,2,3))
    @test CartesianIndices(CartesianIndex{3}(1,2,3)) == CartesianIndices((1, 2, 3))
    @test Tuple{}(CartesianIndices{0,Tuple{}}(())) == ()

    R = CartesianIndices(map(Base.IdentityUnitRange, (2:5, 3:5)))
    @test eltype(R) <: CartesianIndex{2}
    @test eltype(typeof(R)) <: CartesianIndex{2}
    @test eltype(CartesianIndices{2}) <: CartesianIndex{2}
    indices = collect(R)
    @test indices[1] == CartesianIndex{2}(2,3)
    @test indices[2] == CartesianIndex{2}(3,3)
    @test indices[4] == CartesianIndex{2}(5,3)
    @test indices[5] == CartesianIndex{2}(2,4)
    @test indices[12] == CartesianIndex{2}(5,5)
    @test length(indices) == 12
    @test length(R) == 12
    @test ndims(R) == 2
    @test in(CartesianIndex((2,3)), R)
    @test in(CartesianIndex((3,3)), R)
    @test in(CartesianIndex((3,5)), R)
    @test in(CartesianIndex((5,5)), R)
    @test !in(CartesianIndex((1,3)), R)
    @test !in(CartesianIndex((3,2)), R)
    @test !in(CartesianIndex((3,6)), R)
    @test !in(CartesianIndex((6,5)), R)

    @test @inferred(convert(NTuple{2,UnitRange}, R)) === (2:5, 3:5)
    @test @inferred(convert(Tuple{Vararg{UnitRange}}, R)) === (2:5, 3:5)

    I = CartesianIndex(2,3)
    J = CartesianIndex(5,4)
    @test I:J === CartesianIndices((2:5, 3:4))
end

# All we really care about is that we have an optimized
# implementation, but the seed is a useful way to check that.
@test hash(CartesianIndex()) == Base.IteratorsMD.cartindexhash_seed
@test hash(CartesianIndex(1, 2)) != hash((1, 2))

@testset "itr, iterate" begin
    r = 2:3
    itr = eachindex(r)
    y = iterate(itr)
    @test y !== nothing
    y = iterate(itr, y[2])
    @test y !== nothing
    val, _ = y
    y = iterate(itr, y[2])
    @test y === nothing
    @test r[val] == 3
    r = sparse(2:3:8)
    itr = eachindex(r)
    y = iterate(itr)
    @test y !== nothing
    y = iterate(itr, y[2])
    y = iterate(itr, y[2])
    @test y !== nothing
    val, state = y
    @test r[val] == 8
    @test iterate(itr, state) == nothing
end

R = CartesianIndices((1,3))
@test iterate(R) !== nothing
R = CartesianIndices((0,3))
@test iterate(R) === nothing
R = CartesianIndices((3,0))
@test iterate(R) === nothing

@testset "multi-array eachindex" begin
    local a = zeros(2,2)
    local b = view(zeros(3,2), 1:2, :)
    @test @inferred(eachindex(Base.IndexCartesian(), a, b)) == CartesianIndices((2,2))
    @test @inferred(eachindex(Base.IndexLinear(), a, b)) == 1:4
    @test @inferred(eachindex(a, b)) == CartesianIndices((2,2))
    @test @inferred(eachindex(a, a)) == 1:4
    @test_throws DimensionMismatch eachindex(a, rand(3,3))
    @test_throws DimensionMismatch eachindex(b, rand(3,3))
end

@testset "rotates" begin
    a = [1 0 0; 0 0 0]
    @test rotr90(a,1) == [0 1; 0 0; 0 0]
    @test rotr90(a,2) == rot180(a,1)
    @test rotr90(a,3) == rotl90(a,1)
    @test rotl90(a,3) == rotr90(a,1)
    @test rotl90(a,1) == rotr90(a,3)
    @test rotl90(a,4) == a
    @test rotr90(a,4) == a
    @test rot180(a,2) == a
end

# issue #9648
let x = fill(1.5f0, 10^7)
    @test abs(1.5f7 - cumsum(x)[end]) < 3*eps(1.5f7)
    @test cumsum(x) == cumsum!(similar(x), x)
end

# PR #10164
@test eltype(Array{Int}) == Int
@test eltype(Array{Int,1}) == Int

# PR #11080
let x = fill(0.9, 1000)
    @test prod(x) ≈ cumprod(x)[end]
end

@testset "binary ops on bool arrays" begin
    A = Array(trues(5))
    @test A .+ true == [2,2,2,2,2]
    A = Array(trues(5))
    @test A .+ false == [1,1,1,1,1]
    A = Array(trues(5))
    @test true .+ A == [2,2,2,2,2]
    A = Array(trues(5))
    @test false .+ A == [1,1,1,1,1]
    A = Array(trues(5))
    @test A .- true == [0,0,0,0,0]
    A = Array(trues(5))
    @test A .- false == [1,1,1,1,1]
    A = Array(trues(5))
    @test true .- A == [0,0,0,0,0]
    A = Array(trues(5))
    @test false .- A == [-1,-1,-1,-1,-1]
end

@testset "simple transposes" begin
    o5 = fill(1.0+0im,5); o1x5 = fill(1.0+0im,1,5);
    z5 = fill(0.0+0im,5); z1x5 = fill(0.0+0im,1,5);
    o2x5 = fill(1.0+0im,2,5)
    o6 = fill(1.0+0im,6)
    @test_throws DimensionMismatch transpose!(o1x5,o6)
    @test_throws DimensionMismatch transpose!(o6,o1x5)
    @test_throws DimensionMismatch adjoint!(o1x5,o6)
    @test_throws DimensionMismatch adjoint!(o6,o1x5)
    @test_throws DimensionMismatch transpose!(o5,o2x5)
    @test_throws DimensionMismatch adjoint!(o5,o2x5)
    @test_throws DimensionMismatch transpose!(o2x5,o5)
    @test_throws DimensionMismatch adjoint!(o2x5,o5)

    transpose!(z5,o1x5)
    @test z5 == o5
    transpose!(z1x5,o5)
    @test z1x5 == o1x5
    fill!(z5, 0);fill!(z1x5, 0)
    adjoint!(z5,o1x5)
    @test z5 == o5
    adjoint!(z1x5,o5)
    @test z1x5 == o1x5
end

@testset "bounds checking for copyto!" begin
    a = rand(5,3)
    b = rand(6,7)
    @test_throws BoundsError copyto!(a,b)
    @test_throws ArgumentError copyto!(a,2:3,1:3,b,1:5,2:7)
    @test_throws ArgumentError LinearAlgebra.copy_transpose!(a,2:3,1:3,b,1:5,2:7)
end

module RetTypeDecl
    using Test
    import Base: +, *, broadcast, convert

    struct MeterUnits{T,P} <: Number
        val::T
    end
    MeterUnits(val::T, pow::Int) where {T} = MeterUnits{T,pow}(val)

    m  = MeterUnits(1.0, 1)   # 1.0 meter, i.e. units of length
    m2 = MeterUnits(1.0, 2)   # 1.0 meter^2, i.e. units of area

    (+)(x::MeterUnits{T,pow}, y::MeterUnits{T,pow}) where {T,pow} = MeterUnits{T,pow}(x.val+y.val)
    (*)(x::Int, y::MeterUnits{T,pow}) where {T,pow} = MeterUnits{typeof(x*one(T)),pow}(x*y.val)
    (*)(x::MeterUnits{T,1}, y::MeterUnits{T,1}) where {T} = MeterUnits{T,2}(x.val*y.val)
    broadcast(::typeof(*), x::MeterUnits{T,1}, y::MeterUnits{T,1}) where {T} = MeterUnits{T,2}(x.val*y.val)
    convert(::Type{MeterUnits{T,pow}}, y::Real) where {T,pow} = MeterUnits{T,pow}(convert(T,y))

    @test @inferred(m .+ [m,m]) == [m+m,m+m]
    @test @inferred([m,m] .+ m) == [m+m,m+m]
    @test @inferred(broadcast(*,m,[m,m])) == [m2,m2]
    @test @inferred(broadcast(*,[m,m],m)) == [m2,m2]
    @test @inferred([m 2m; m m]*[m,m]) == [3m2,2m2]
    @test @inferred(broadcast(*,[m m],[m,m])) == [m2 m2; m2 m2]
end

# range, range ops
@test (1:5) + (1.5:5.5) == 2.5:2.0:10.5

@testset "selectdim" begin
    f26009(A, i) = selectdim(A, 1, i)
    for A in (reshape(Vector(1:20), 4, 5),
              reshape(1:20, 4, 5))
        local A
        @test selectdim(A, 1, 2) == 2:4:20
        @test selectdim(A, 2, 2) == 5:8
        @test_throws ArgumentError selectdim(A,0,1)
        @test selectdim(A, 3, 1) == A
        @test_throws BoundsError selectdim(A, 3, 2)
        @test @inferred(f26009(A, 2:2)) == reshape(2:4:20, 1, :)
        @test @inferred(f26009(A, 2)) == 2:4:20
    end
    A = reshape(1:24, 4, 3, 2)
    @test IndexStyle(selectdim(A, 1, 1)) == IndexStyle(view(A, 1, :, :)) == IndexLinear()
    @test IndexStyle(selectdim(A, 2, 1)) == IndexStyle(view(A, :, 1, :)) == IndexCartesian()
    @test IndexStyle(selectdim(A, 3, 1)) == IndexStyle(view(A, :, :, 1)) == IndexLinear()
end

# row/column/slice iterator tests
using Base: eachrow, eachcol
@testset "row/column/slice iterators" begin
    # Simple ones
    M = [1 2 3; 4 5 6; 7 8 9]
    @test collect(eachrow(M)) == collect(eachslice(M, dims = 1)) == [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    @test collect(eachcol(M)) == collect(eachslice(M, dims = 2)) == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
    @test_throws DimensionMismatch eachslice(M, dims = 4)

    # Higher-dimensional case
    M = reshape([(1:16)...], 2, 2, 2, 2)
    @test_throws MethodError collect(eachrow(M))
    @test_throws MethodError collect(eachcol(M))
    @test collect(eachslice(M, dims = 1))[1][:, :, 1] == [1 5; 3 7]
end

###
### IndexCartesian workout
###
struct LinSlowMatrix{T} <: DenseArray{T,2}
    data::Matrix{T}
end

# This is the default, but just to be sure
Base.IndexStyle(::Type{A}) where {A<:LinSlowMatrix} = Base.IndexCartesian()

Base.size(A::LinSlowMatrix) = size(A.data)

Base.getindex(A::LinSlowMatrix, i::Integer) = error("Not defined")
Base.getindex(A::LinSlowMatrix, i::Integer, j::Integer) = A.data[i,j]

Base.setindex!(A::LinSlowMatrix, v, i::Integer) = error("Not defined")
Base.setindex!(A::LinSlowMatrix, v, i::Integer, j::Integer) = A.data[i,j] = v

A = rand(3,5)
B = LinSlowMatrix(A)
S = view(A, :, :)

@test A == B
@test B == A
@test isequal(A, B)
@test isequal(B, A)

for (a,b) in zip(A, B)
    local a,b
    @test a == b
end
for (a,s) in zip(A, S)
    local a,s
    @test a == s
end

let C = copy(B)
    @test A == C
    @test B == C
end

@test vec(A) == vec(B) == vec(S)
@test minimum(A) == minimum(B) == minimum(S)
@test maximum(A) == maximum(B) == maximum(S)

let
    a, ai = findmin(A)
    b, bi = findmin(B)
    s, si = findmin(S)
    @test a == b == s
    @test ai == bi == si
end

let
    a, ai = findmax(A)
    b, bi = findmax(B)
    s, si = findmax(S)
    @test a == b == s
    @test ai == bi == si
end

for X in (A, B, S)
    @test findmin(X) == findmin(Dict(pairs(X)))
    @test findmax(X) == findmax(Dict(pairs(X)))
end

fill!(B, 2)
@test all(x->x==2, B)

copyto!(B, A)
copyto!(S, A)

@test cat(A, B, S; dims=1) == cat(A, A, A; dims=1)
@test cat(A, B, S; dims=2) == cat(A, A, A; dims=2)

@test cumsum(A, dims=1) == cumsum(B, dims=1) == cumsum(S, dims=1)
@test cumsum(A, dims=2) == cumsum(B, dims=2) == cumsum(S, dims=2)

@test mapslices(sort, A, dims=1) == mapslices(sort, B, dims=1) == mapslices(sort, S, dims=1)
@test mapslices(sort, A, dims=2) == mapslices(sort, B, dims=2) == mapslices(sort, S, dims=2)

@test reverse(A, dims=1) == reverse(B, dims=1) == reverse(S, dims=2)
@test reverse(A, dims=2) == reverse(B, dims=2) == reverse(S, dims=2)

@test A .+ 1 == B .+ 1 == S .+ 1
@test 2*A == 2*B == 2*S
@test A/3 == B/3 == S/3

# issue #13250
x13250 = zeros(3)
x13250[UInt(1):UInt(2)] .= 1.0
@test x13250[1] == 1.0
@test x13250[2] == 1.0
@test x13250[3] == 0.0

struct SquaresVector <: AbstractArray{Int, 1}
    count::Int
end
Base.size(S::SquaresVector) = (S.count,)
Base.IndexStyle(::Type{SquaresVector}) = Base.IndexLinear()
Base.getindex(S::SquaresVector, i::Int) = i*i
foo_squares = SquaresVector(5)
@test convert(Array{Int}, foo_squares) == [1,4,9,16,25]
@test convert(Array{Int, 1}, foo_squares) == [1,4,9,16,25]

# issue #13254
let A = zeros(Int, 2, 2), B = zeros(Float64, 2, 2)
    f1() = [1]
    f2() = [1;]
    f3() = [1;2]
    f4() = [1;2.0]
    f5() = [1 2]
    f6() = [1 2.0]
    f7() = Int[1]
    f8() = Float64[1]
    f9() = Int[1;]
    f10() = Float64[1;]
    f11() = Int[1;2]
    f12() = Float64[1;2]
    f13() = Int[1;2.0]
    f14() = Int[1 2]
    f15() = Float64[1 2]
    f16() = Int[1 2.0]
    f17() = [1:2;]
    f18() = Int[1:2;]
    f19() = Float64[1:2;]
    f20() = [1:2;1:2]
    f21() = Int[1:2;1:2]
    f22() = Float64[1:2;1:2]
    f23() = [1:2;1.0:2.0]
    f24() = Int[1:2;1.0:2.0]
    f25() = [1:2 1:2]
    f26() = Int[1:2 1:2]
    f27() = Float64[1:2 1:2]
    f28() = [1:2 1.0:2.0]
    f29() = Int[1:2 1.0:2.0]
    f30() = [A;]
    f31() = Int[A;]
    f32() = Float64[A;]
    f33() = [A;A]
    f34() = Int[A;A]
    f35() = Float64[A;A]
    f36() = [A;B]
    f37() = Int[A;B]
    f38() = [A A]
    f39() = Int[A A]
    f40() = Float64[A A]
    f41() = [A B]
    f42() = Int[A B]

    for f in [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16,
              f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, f27, f28, f29, f30,
              f31, f32, f33, f34, f35, f36, f37, f38, f39, f40, f41, f42]
        @test isconcretetype(Base.return_types(f, ())[1])
    end
end

# issue #14482
@inferred map(Int8, Int[0])

# make sure @inbounds isn't used too much
mutable struct OOB_Functor{T}; a::T; end
(f::OOB_Functor)(i::Int) = f.a[i]
let f = OOB_Functor([1,2])
    @test_throws BoundsError map(f, [1,2,3,4,5])
end

@testset "issue 15654" begin
    @test cumprod([5], dims=2) == [5]
    @test cumprod([1 2; 3 4], dims=3) == [1 2; 3 4]
    @test cumprod([1 2; 3 4], dims=1) == [1 2; 3 8]
    @test cumprod([1 2; 3 4], dims=2) == [1 2; 3 12]

    @test cumsum([5], dims=2) == [5]
    @test cumsum([1 2; 3 4], dims=1) == [1 2; 4 6]
    @test cumsum([1 2; 3 4], dims=2) == [1 3; 3 7]
    @test cumsum([1 2; 3 4], dims=3) == [1 2; 3 4]

    @test cumprod!(Vector{Int}(undef, 1), [5], dims=2) == [5]
    @test cumprod!(Matrix{Int}(undef, 2, 2), [1 2; 3 4], dims=3) == [1 2; 3 4]
    @test cumprod!(Matrix{Int}(undef, 2, 2), [1 2; 3 4], dims=1) == [1 2; 3 8]
    @test cumprod!(Matrix{Int}(undef, 2, 2), [1 2; 3 4], dims=2) == [1 2; 3 12]

    @test cumsum!(Vector{Int}(undef, 1), [5], dims=2) == [5]
    @test cumsum!(Matrix{Int}(undef, 2, 2), [1 2; 3 4], dims=1) == [1 2; 4 6]
    @test cumsum!(Matrix{Int}(undef, 2, 2), [1 2; 3 4], dims=2) == [1 3; 3 7]
    @test cumsum!(Matrix{Int}(undef, 2, 2), [1 2; 3 4], dims=3) == [1 2; 3 4]
end
@testset "issue #18363" begin
    @test_throws DimensionMismatch cumsum!([0,0], 1:4)
    @test cumsum(Any[])::Vector{Any} == Any[]
    @test cumsum(Any[1, 2.3]) == [1, 3.3] == cumsum(Real[1, 2.3])::Vector{Real}
    @test cumsum([true,true,true]) == [1,2,3]
    @test cumsum(0x00:0xff)[end] === UInt(255*(255+1)÷2) # no overflow
    @test accumulate(+, 0x00:0xff)[end] === 0x80         # overflow
    @test_throws InexactError cumsum!(similar(0x00:0xff), 0x00:0xff) # overflow

    @test cumsum([[true], [true], [false]])::Vector{Vector{Int}} == [[1], [2], [2]]
end
#issue #18336
@test cumsum([-0.0, -0.0])[1] === cumsum([-0.0, -0.0])[2] === -0.0
@test cumprod(-0.0im .+ (0:0))[1] === Complex(0.0, -0.0)

module TestNLoops15895

using Base.Cartesian
using Test

# issue 15894
function f15894(d)
    s = zero(eltype(d))
    @nloops 1 i d begin
        s += @nref 1 d i
    end
    s
end
@test f15894(fill(1, 100)) == 100
end

@testset "sign, conj[!], ~" begin
    local A, B, C
    A = [-10,0,3]
    B = [-10.0,0.0,3.0]
    C = [1,im,0]

    @test sign.(A) == [-1,0,1]
    @test sign.(B) == [-1,0,1]
    @test typeof(sign.(A)) == Vector{Int}
    @test typeof(sign.(B)) == Vector{Float64}

    @test conj(A) == A
    @test conj!(copy(A)) == A
    @test conj(B) == A
    @test conj(C) == [1,-im,0]
    @test typeof(conj(A)) == Vector{Int}
    @test typeof(conj(B)) == Vector{Float64}
    @test typeof(conj(C)) == Vector{Complex{Int}}

    @test .~A == [9,-1,-4]
    @test typeof(.~A) == Vector{Int}
end

# @inbounds is expression-like, returning its value; #15558
@testset "expression-like inbounds" begin
    local A = [1,2,3]
    @test (@inbounds A[1]) == 1
    f(A, i) = @inbounds i == 0 ? (return 0) : A[i]
    @test f(A, 0) == 0
    @test f(A, 1) == 1
    g(A, i) = (i == 0 ? (@inbounds return 0) : (@inbounds A[i]))
    @test g(A, 0) == 0
    @test g(A, 1) == 1
end

@testset "issue #16247" begin
    local A = zeros(3,3)
    @test size(A[:,0x1:0x2]) == (3, 2)
    @test size(A[:,UInt(1):UInt(2)]) == (3,2)
    @test size(similar(A, UInt(3), 0x3)) == size(similar(A, (UInt(3), 0x3))) == (3,3)
end

# issue 17254
module AutoRetType

using Test

struct Foo end
for op in (:+, :*, :÷, :%, :<<, :>>, :-, :/, :\, ://, :^)
    @eval import Base.$(op)
    @eval $(op)(::Foo, ::Foo) = Foo()
end
let A = fill(Foo(), 10, 10)
    @test isa(A + A, Matrix{Foo})
    @test isa(A - A, Matrix{Foo})
    for op in (:+, :*, :÷, :%, :<<, :>>, :-, :/, :\, ://, :^)
        @test isa(broadcast(eval(op), A, A), Matrix{Foo})
    end
end

end # module AutoRetType

@testset "concatenations of dense matrices/vectors yield dense matrices/vectors" begin
    N = 4
    densevec = fill(1., N)
    densemat = Matrix(1.0I, N, N)
    # Test that concatenations of homogeneous pairs of either dense matrices or dense vectors
    # (i.e., Matrix-Matrix concatenations, and Vector-Vector concatenations) yield dense arrays
    for densearray in (densevec, densemat)
        @test isa(vcat(densearray, densearray), Array)
        @test isa(hcat(densearray, densearray), Array)
        @test isa(hvcat((2,), densearray, densearray), Array)
        @test isa(cat(densearray, densearray; dims=(1,2)), Array)
    end
    @test isa([[1,2,3]'; [1,2,3]'], Matrix{Int})
    @test isa([[1,2,3]' [1,2,3]'], Adjoint{Int, Vector{Int}})
    @test isa([Any[1.0, 2]'; Any[2.0, 2]'], Matrix{Any})
    @test isa([Any[1.0, 2]' Any[2.0, 2]'], Adjoint{Any, Vector{Any}})
    # Test that concatenations of heterogeneous Matrix-Vector pairs yield dense matrices
    @test isa(hcat(densemat, densevec), Array)
    @test isa(hcat(densevec, densemat), Array)
    @test isa(hvcat((2,), densemat, densevec), Array)
    @test isa(hvcat((2,), densevec, densemat), Array)
    @test isa(cat(densemat, densevec; dims=(1,2)), Array)
    @test isa(cat(densevec, densemat; dims=(1,2)), Array)
end

@testset "type constructor Array{T, N}(undef, d...) works (especially for N>3)" begin
    a = Array{Float64}(undef, 10)
    b = Vector{Float64}(undef, 10)
    @test size(a) == (10,)
    @test size(a, 1) == 10
    @test size(a) == size(b)
    a = Array{Float64}(undef, 2,3)
    b = Matrix{Float64}(undef, 2,3)
    @test size(a) == (2,3)
    @test (size(a, 1), size(a, 2), size(a, 3)) == (2,3,1)
    @test size(a) == size(b)
    a = Array{Float64}(undef, 9,8,7,6,5,4,3,2,1)
    b = Array{Float64,9}(undef, 9,8,7,6,5,4,3,2,1)
    @test size(a,4) == 6
    @test size(a) == (9,8,7,6,5,4,3,2,1)
    @test size(a) == size(b)
end

@testset "Converting size integers to ints" begin
    @test size(Array{Float64}(undef, unsigned(2))) == (2,)
    @test size(Array{Float64}(undef, unsigned(2), unsigned(3))) == (2, 3)
    @test size(Array{Float64}(undef, unsigned(2), unsigned(3), unsigned(4))) == (2, 3, 4)
    @test size(Array{Float64}(undef, unsigned(2), unsigned(3), unsigned(4), unsigned(5))) == (2, 3, 4, 5)
    # with number of dimensions
    @test size(Array{Float64, 3}(undef, unsigned(2), unsigned(3), unsigned(4))) == (2, 3, 4)
    # unsplatted
    @test size(Array{Float64}(undef, (unsigned(2), unsigned(3), unsigned(4)))) == (2, 3, 4)
end

@testset "type constructor Array{T, N}(nothing, d...) works (especially for N>3)" for T in (Int, String),
                                                                                      U in (Nothing, Missing)
    a = Array{Union{T, U}}(U(), 10)
    b = Vector{Union{T, U}}(U(), 10)
    @test size(a) == size(b) == (10,)
    @test all(x -> x isa U, a)
    @test all(x -> x isa U, b)
    a = Array{Union{T, U}}(U(), 2,3)
    b = Matrix{Union{T, U}}(U(), 2,3)
    @test size(a) ==  size(b) == (2,3)
    @test all(x -> x isa U, a)
    @test all(x -> x isa U, b)
    a = Array{Union{T, U}}(U(), 9,8,7,6,5,4,3,2,1)
    b = Array{Union{T, U},9}(U(), 9,8,7,6,5,4,3,2,1)
    @test size(a) ==  size(b) == (9,8,7,6,5,4,3,2,1)
    @test all(x -> x isa U, a)
    @test all(x -> x isa U, b)
end

@testset "diff" begin
    # test diff, throw ArgumentError for invalid dimension argument
    v = [7, 3, 5, 1, 9]
    @test diff(v) == [-4, 2, -4, 8]
    @test diff(v,dims=1) == [-4, 2, -4, 8]
    X = [3  9   5;
         7  4   2;
         2  1  10]
    @test diff(X,dims=1) == [4  -5 -3; -5  -3  8]
    @test diff(X,dims=2) == [6 -4; -3 -2; -1 9]
    @test diff(view(X, 1:2, 1:2),dims=1) == [4 -5]
    @test diff(view(X, 1:2, 1:2),dims=2) == reshape([6; -3], (2,1))
    @test diff(view(X, 2:3, 2:3),dims=1) == [-3 8]
    @test diff(view(X, 2:3, 2:3),dims=2) == reshape([-2; 9], (2,1))
    Y = cat([1 3; 4 3], [6 5; 1 4], dims=3)
    @test diff(Y, dims=3) == reshape([5 2; -3 1], (2, 2, 1))
    @test_throws UndefKeywordError diff(X)
    @test_throws ArgumentError diff(X,dims=3)
    @test_throws ArgumentError diff(X,dims=-1)
end

@testset "accumulate, accumulate!" begin
    @test accumulate(+, [1,2,3]) == [1, 3, 6]
    @test accumulate(min, [1 2; 3 4], dims=1) == [1 2; 1 2]
    @test accumulate(max, [1 2; 3 0], dims=2) == [1 2; 3 3]
    @test accumulate(+, Bool[]) == Int[]
    @test accumulate(*, Bool[]) == Bool[]
    @test accumulate(+, Float64[]) == Float64[]

    @test accumulate(min, [1, 2, 5, -1, 3, -2]) == [1, 1, 1, -1, -1, -2]
    @test accumulate(max, [1, 2, 5, -1, 3, -2]) == [1, 2, 5, 5, 5, 5]
    @test Base.accumulate_pairwise(min, [1, 2, 5, -1, 3, -2]) == [1, 1, 1, -1, -1, -2]
    @test Base.accumulate_pairwise(max, [1, 2, 5, -1, 3, -2]) == [1, 2, 5, 5, 5, 5]

    @test accumulate(max, [1 0; 0 1], dims=1) == [1 0; 1 1]
    @test accumulate(max, [1 0; 0 1], dims=2) == [1 1; 0 1]
    @test accumulate(min, [1 0; 0 1], dims=1) == [1 0; 0 0]
    @test accumulate(min, [1 0; 0 1], dims=2) == [1 0; 0 0]

    @test accumulate(min, [3 2 1; 3 2 1], dims=2) == [3 2 1; 3 2 1]
    @test accumulate(min, [3 2 1; 3 2 1], dims=2, init=2) == [2 2 1; 2 2 1]

    @test isa(accumulate(+, Int[]), Vector{Int})
    @test isa(accumulate(+, Int[]; init=1.), Vector{Float64})
    @test accumulate(+, [1,2]; init=1) == [2, 4]
    arr = randn(4)
    @test accumulate(*, arr; init=1) ≈ accumulate(*, arr)

    N = 5
    for arr in [rand(Float64, N), rand(Bool, N), rand(-2:2, N)]
        for (op, cumop) in [(+, cumsum), (*, cumprod)]
            @inferred accumulate(op, arr)
            accumulate_arr = accumulate(op, arr)
            @test accumulate_arr ≈ cumop(arr)
            @test accumulate_arr[end] ≈ reduce(op, arr)
            @test accumulate_arr[1] ≈ arr[1]
            @test accumulate(op, arr, dims=10) ≈ arr

            if eltype(arr) in [Int, Float64] # eltype of out easy
                out = similar(arr)
                @test accumulate!(op, out, arr) ≈ accumulate_arr
                @test out ≈ accumulate_arr
            end
        end
        arr_cop = similar(arr)
        cumprod!(arr_cop, arr)
        @test arr_cop ≈ cumprod(arr)
    end

    # exotic indexing
    arr = randn(4)
    oarr = OffsetArray(arr, (-3,))
    @test accumulate(+, oarr).parent == accumulate(+, arr)
    @test accumulate(+, oarr, init = 10).parent == accumulate(+, arr; init = 10)

    @inferred accumulate(+, randn(3))
    @inferred accumulate(+, randn(3); init=1)

    # asymmetric operation
    op(x,y) = 2x+y
    @test accumulate(op, [10,20, 30]) == [10, op(10, 20), op(op(10, 20), 30)] == [10, 40, 110]
    @test accumulate(op, [10 20 30], dims=2) == [10 op(10, 20) op(op(10, 20), 30)] == [10 40 110]

    #25506
    @test accumulate((acc, x) -> acc+x[1], [(1,2), (3,4), (5,6)]; init=0) == [1, 4, 9]
    @test accumulate(*, ['a', 'b']) == ["a", "ab"]
    @inferred accumulate(*, String[])
    @test accumulate(*, ['a' 'b'; 'c' 'd'], dims=1) == ["a" "b"; "ac" "bd"]
    @test accumulate(*, ['a' 'b'; 'c' 'd'], dims=2) == ["a" "ab"; "c" "cd"]
end

struct F21666{T <: Base.ArithmeticStyle}
    x::Float32
end

Base.ArithmeticStyle(::Type{F21666{T}}) where {T} = T()
Base.:+(x::F, y::F) where {F <: F21666} = F(x.x + y.x)
Float64(x::F21666) = Float64(x.x)
@testset "Exactness of cumsum # 21666" begin
    # test that cumsum uses more stable algorithm
    # for types with unknown/rounding arithmetic
    # we make v pretty large, because stable algorithm may have a large base case
    v = zeros(300); v[1] = 2; v[200:end] .= eps(Float32)

    f_rounds = Float64.(cumsum(F21666{Base.ArithmeticRounds}.(v)))
    f_unknown = Float64.(cumsum(F21666{Base.ArithmeticUnknown}.(v)))
    f_truth = cumsum(v)
    f_inexact = Float64.(accumulate(+, Float32.(v)))
    @test f_rounds == f_unknown
    @test f_rounds != f_inexact
    @test norm(f_truth - f_rounds) < norm(f_truth - f_inexact)
end

@testset "zeros and ones" begin
    @test ones(2) == ones(Int, 2) ==  [1,1]
    @test isa(ones(2), Vector{Float64})
    @test isa(ones(Int, 2), Vector{Int})

    function test_zeros(arr, T, s)
        @test all(arr .== 0)
        @test isa(arr, T)
        @test size(arr) == s
    end
    test_zeros(zeros(),      Array{Float64, 0}, ())
    test_zeros(zeros(2),     Vector{Float64},   (2,))
    test_zeros(zeros(2,3),   Matrix{Float64},   (2,3))
    test_zeros(zeros((2,3)), Matrix{Float64},   (2,3))

    test_zeros(zeros(Int, 6),      Vector{Int}, (6,))
    test_zeros(zeros(Int, 2, 3),   Matrix{Int}, (2,3))
    test_zeros(zeros(Int, (2, 3)), Matrix{Int}, (2,3))

    # #19265"
    @test_throws MethodError zeros(Float64, [1.])
    @test_throws MethodError ones(Float64, [0, 0])
end

# issue #11053
mutable struct T11053
    a::Float64
end
Base.:*(a::T11053, b::Real) = T11053(a.a*b)
Base.:(==)(a::T11053, b::T11053) = a.a == b.a
@test [T11053(1)] * 5 == [T11053(1)] .* 5 == [T11053(5.0)]

#15907
@test typeof(Array{Int,0}(undef)) == Array{Int,0}

# check a == b for arrays of Union type (#22403)
let TT = Union{UInt8, Int8}
    a = TT[0x0, 0x1]
    b = TT[0x0, 0x0]
    pa = pointer(a)
    pb = pointer(b)
    resize!(a, 1) # sets a[2] = 0
    resize!(b, 1)
    @assert pointer(a) == pa
    @assert pointer(b) == pb
    unsafe_store!(pa, 0x1, 2) # reset a[2] to 1
    @test length(a) == length(b) == 1
    @test a[1] == b[1] == 0x0
    @test a == b
end

let a = Vector{Int}[[1]],
    b = Vector{Float64}[[2.0]],
    c = Vector{Char}[['a']]
    @test eltype([a;b]) == Vector{Float64}
    @test eltype([a;c]) == Vector
end

# Issue #23629
@testset "issue 23629" begin
    @test_throws BoundsError zeros(2,3,0)[2,3]
    @test_throws BoundsError checkbounds(zeros(2,3,0), 2, 3)
end

@testset "indexing by Bool values" begin
    @test_throws ArgumentError zeros(Float64, 2)[false]
    @test_throws ArgumentError zeros(Float64, 2)[true]
end

@testset "issue 24707" begin
    @test eltype(Vector{Tuple{V}} where V<:Integer) >: Tuple{Integer}
end

@testset "inference hash array 22740" begin
    @test @inferred(hash([1,2,3])) == @inferred(hash(1:3))
end

@testset "hashing arrays of arrays" begin
    # issues #27865 and #26011
    @test hash([["asd"], ["asd"], ["asad"]]) == hash(Any[["asd"], ["asd"], ["asad"]])
    @test hash([["asd"], ["asd"], ["asad"]]) != hash([["asd"], ["asd"], ["asadq"]])
    @test hash([1,2,[3]]) == hash([1,2,Any[3]]) == hash([1,2,Int8[3]]) == hash([1,2,BigInt[3]]) == hash([1,2,[3.0]])
    @test hash([1,2,[3]]) != hash([1,2,[3,4]])
end

# Ensure we can hash strange custom structs — and they hash the same in arrays
struct totally_not_five26034 end
Base.isequal(::totally_not_five26034, x::Number)=isequal(5,x);
Base.isequal(x::Number, ::totally_not_five26034)=isequal(5,x);
Base.isequal(::totally_not_five26034, ::totally_not_five26034)=true;
Base.hash(::totally_not_five26034, h::UInt)=hash(5, h);
import Base.==
==(::totally_not_five26034, x::Number)= (5==x);
==(x::Number,::totally_not_five26034)= (5==x);
==(::totally_not_five26034,::totally_not_five26034)=true;
@testset "issue #26034" begin
    n5 = totally_not_five26034()
    @test hash(n5) == hash(5)
    @test isequal([4,n5,6], [4,5,6])
    @test isequal(hash([4,n5,6]), hash([4,5,6]))
    @test isequal(hash(Any[4,n5,6]), hash(Union{Int, totally_not_five26034}[4,5,6]))
    @test isequal(hash([n5,4,n5,6]), hash([n5,4,5,6]))
end

function f27079()
    X = rand(5)
    for x in X
        resize!(X, 0)
    end
    length(X)
end
@testset "iteration over resized vector" begin
    @test f27079() == 0
end

@testset "indices-related shape promotion errors" begin
    @test_throws DimensionMismatch Base.promote_shape((2,), (3,))
    @test_throws DimensionMismatch Base.promote_shape((2, 3), (2, 4))
    @test_throws DimensionMismatch Base.promote_shape((3, 2), (2, 2))
    inds_a = Base.Indices{2}([1:3, 1:2])
    inds_b = Base.Indices{2}([1:3, 1:6])
    @test_throws DimensionMismatch Base.promote_shape(inds_a, inds_b)
    inds_a = Base.Indices{2}([1:3, 1:2])
    inds_b = Base.Indices{2}([1:4, 1:2])
    @test_throws DimensionMismatch Base.promote_shape(inds_a, inds_b)
    # fails because ranges 3, 4 of inds_a are not 1:1
    inds_a = Base.Indices{4}([1:3, 1:2, 1:3, 1:2])
    inds_b = Base.Indices{2}([1:3, 1:2])
    @test_throws DimensionMismatch Base.promote_shape(inds_a, inds_b)
    # succeeds for converse reason
    inds_a = Base.Indices{2}([1:3, 1:1])
    inds_b = Base.Indices{1}([1:3])
    @test Base.promote_shape(inds_a, inds_b) == Base.promote_shape(inds_b, inds_a)
end

struct T25958
end
Base.lastindex(::T25958, args...) = (:lastindex, args...)
Base.getindex(::T25958, args...) = args
Base.view(::T25958, args...) = args
@testset "ensure @view and @views matches lowering" begin
    t = T25958()
    @test t[end]         == @view(t[end])         == @views t[end]
    @test t[1,end]       == @view(t[1,end])       == @views t[1,end]
    @test t[end,1]       == @view(t[end,1])       == @views t[end,1]
    @test t[end,end]     == @view(t[end,end])     == @views t[end,end]
    @test t[1,end,end]   == @view(t[1,end,end])   == @views t[1,end,end]
    @test t[end,1,end]   == @view(t[end,1,end])   == @views t[end,1,end]
    @test t[end,end,1]   == @view(t[end,end,1])   == @views t[end,end,1]
    @test t[end,end,end] == @view(t[end,end,end]) == @views t[end,end,end]
end

@testset "0-dimensional container operations" begin
    for op in (-, conj, real, imag)
        @test op(fill(2)) == fill(op(2))
        @test op(fill(1+2im)) == fill(op(1+2im))
    end
    for op in (+, -)
        @test op(fill(1), fill(2)) == fill(op(1, 2))
        @test op(fill(1), fill(2)) isa AbstractArray{Int, 0}
    end
    @test fill(1) + fill(2) + fill(3) == fill(1+2+3)
    @test fill(1) / 2 == fill(1/2)
    @test 2 \ fill(1) == fill(1/2)
    @test 2*fill(1) == fill(2)
    @test fill(1)*2 == fill(2)
end


# Fix oneunit bug for unitful arrays
@test oneunit([Second(1) Second(2); Second(3) Second(4)]) == [Second(1) Second(0); Second(0) Second(1)]

@testset "indexing by CartesianIndices" begin
    A = rand(10,10)
    for (I,Rs) in ((keys(A), (1:10, 1:10)),
                   (CartesianIndex(2,2):CartesianIndex(9,9), (2:9, 2:9)),
                   (CartesianIndex(5,3):CartesianIndex(6,7), (5:6, 3:7)))
        @test A[I] == A[Rs...] == @view(A[I]) == @view(A[Rs...])
        @test @view(A[I]) isa StridedArray
        @test !checkbounds(Bool, [], I)
        @test !checkbounds(Bool, fill(2,1,1,1), :, I)
        @test !checkbounds(Bool, fill(2,1,1,1), I, :)
    end
    @test !checkbounds(Bool, rand(3,3,3), :, CartesianIndex(0,0):CartesianIndex(1,1))
    @test !checkbounds(Bool, rand(3,3,3), CartesianIndex(0,0):CartesianIndex(1,1), :)
end

# Throws ArgumentError for negative dimensions in Array
@test_throws ArgumentError fill('a', -10)

@testset "Issue 33919" begin
    A = Array[rand(2, 3), rand(3, 1)]
    B = Array[rand(2, 2), rand(1, 4)]
    C = hcat(A, B)
    @test typeof(C) == Array{Array{Float64,2},2}
end

# issue #33974
let n = 12000000, k = 257000000
    # tests skipped since they use a lot of memory
    @test_skip filter(x -> x[2] < 1.0, collect(enumerate(vcat(fill(0.5, n), fill(NaN, k)))))[end] == (n, 0.5)
    @test_skip let v = collect(enumerate(vcat(fill(0.5, n), fill(NaN, k))))
        resize!(v, n)
        sizehint!(v, n)
        v[end] == (n, 0.5)
    end
end

@testset "Issue 33143" begin
    xvec = rand(4)
    xmat = reshape(xvec, 2, 2)
    push!(xvec, 1.0)
    @test xvec[5] == 1.0
    @test length(xmat) == 4
end
