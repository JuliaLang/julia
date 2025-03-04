# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random, LinearAlgebra

include(joinpath(@__DIR__,"../Compiler/test/irutils.jl"))

isdefined(Main, :InfiniteArrays) || @eval Main include("testhelpers/InfiniteArrays.jl")
using .Main.InfiniteArrays

isdefined(Main, :StructArrays) || @eval Main include("testhelpers/StructArrays.jl")
using .Main.StructArrays

isdefined(Main, :FillArrays) || @eval Main include("testhelpers/FillArrays.jl")
using .Main.FillArrays

isdefined(Main, :SizedArrays) || @eval Main include("testhelpers/SizedArrays.jl")
using .Main.SizedArrays

A = rand(5,4,3)
@testset "Bounds checking" begin
    @test checkbounds(Bool, A, 1, 1, 1) == true
    @test checkbounds(Bool, A, 5, 4, 3) == true
    @test checkbounds(Bool, A, 0, 1, 1) == false
    @test checkbounds(Bool, A, 1, 0, 1) == false
    @test checkbounds(Bool, A, 1, 1, 0) == false
    @test checkbounds(Bool, A, 6, 4, 3) == false
    @test checkbounds(Bool, A, 5, 5, 3) == false
    @test checkbounds(Bool, A, 5, 4, 4) == false
    @test checkbounds(Bool, A, 1) == true           # linear indexing
    @test checkbounds(Bool, A, 60) == true
    @test checkbounds(Bool, A, 61) == false
    @test checkbounds(Bool, A, 2, 2, 2, 1) == true  # extra indices
    @test checkbounds(Bool, A, 2, 2, 2, 2) == false
    @test checkbounds(Bool, A, 1, 1)  == false
    @test checkbounds(Bool, A, 1, 12) == false
    @test checkbounds(Bool, A, 5, 12) == false
    @test checkbounds(Bool, A, 1, 13) == false
    @test checkbounds(Bool, A, 6, 12) == false
end

@testset "single CartesianIndex" begin
    @test checkbounds(Bool, A, CartesianIndex((1, 1, 1))) == true
    @test checkbounds(Bool, A, CartesianIndex((5, 4, 3))) == true
    @test checkbounds(Bool, A, CartesianIndex((0, 1, 1))) == false
    @test checkbounds(Bool, A, CartesianIndex((1, 0, 1))) == false
    @test checkbounds(Bool, A, CartesianIndex((1, 1, 0))) == false
    @test checkbounds(Bool, A, CartesianIndex((6, 4, 3))) == false
    @test checkbounds(Bool, A, CartesianIndex((5, 5, 3))) == false
    @test checkbounds(Bool, A, CartesianIndex((5, 4, 4))) == false
    @test checkbounds(Bool, A, CartesianIndex((1,))) == false
    @test checkbounds(Bool, A, CartesianIndex((60,))) == false
    @test checkbounds(Bool, A, CartesianIndex((61,))) == false
    @test checkbounds(Bool, A, CartesianIndex((2, 2, 2, 1,))) == true
    @test checkbounds(Bool, A, CartesianIndex((2, 2, 2, 2,))) == false
    @test checkbounds(Bool, A, CartesianIndex((1, 1,)))  == false
    @test checkbounds(Bool, A, CartesianIndex((1, 12,))) == false
    @test checkbounds(Bool, A, CartesianIndex((5, 12,))) == false
    @test checkbounds(Bool, A, CartesianIndex((1, 13,))) == false
    @test checkbounds(Bool, A, CartesianIndex((6, 12,))) == false
end

@testset "mix of CartesianIndex and Int" begin
    @test checkbounds(Bool, A, CartesianIndex((1,)), 1, CartesianIndex((1,))) == true
    @test checkbounds(Bool, A, CartesianIndex((5, 4)), 3)  == true
    @test checkbounds(Bool, A, CartesianIndex((0, 1)), 1)  == false
    @test checkbounds(Bool, A, 1, CartesianIndex((0, 1)))  == false
    @test checkbounds(Bool, A, 1, 1, CartesianIndex((0,))) == false
    @test checkbounds(Bool, A, 6, CartesianIndex((4, 3)))  == false
    @test checkbounds(Bool, A, 5, CartesianIndex((5,)), 3) == false
    @test checkbounds(Bool, A, CartesianIndex((5,)), CartesianIndex((4,)), CartesianIndex((4,)))  == false
end

@testset "Infinite axes" begin
    r = OneToInf()
    @testset "CartesianIndices" begin
        C = CartesianIndices(size(r))
        ax = to_indices(r, (C,))[1]
        @test ax === r
    end
    @testset "LinearIndices" begin
        L = LinearIndices(size(r))
        ax = to_indices(r, (L,))[1]
        @test ax === L
    end
end

@testset "vector indices" begin
    @test checkbounds(Bool, A, 1:5, 1:4, 1:3) == true
    @test checkbounds(Bool, A, 0:5, 1:4, 1:3) == false
    @test checkbounds(Bool, A, 1:5, 0:4, 1:3) == false
    @test checkbounds(Bool, A, 1:5, 1:4, 0:3) == false
    @test checkbounds(Bool, A, 1:6, 1:4, 1:3) == false
    @test checkbounds(Bool, A, 1:5, 1:5, 1:3) == false
    @test checkbounds(Bool, A, 1:5, 1:4, 1:4) == false
    @test checkbounds(Bool, A, 1:60) == true
    @test checkbounds(Bool, A, 1:61) == false
    @test checkbounds(Bool, A, 2, 2, 2, 1:1) == true  # extra indices
    @test checkbounds(Bool, A, 2, 2, 2, 10:9) == true
    @test checkbounds(Bool, A, 2, 2, 2, 1:2) == false
    @test checkbounds(Bool, A, 1:5, 1:4) == false
    @test checkbounds(Bool, A, 1:5, 1:12) == false
    @test checkbounds(Bool, A, 1:5, 1:13) == false
    @test checkbounds(Bool, A, 1:6, 1:12) == false
end

@testset "logical" begin
    @test checkbounds(Bool, A, trues(5), trues(4), trues(3)) == true
    @test checkbounds(Bool, A, trues(6), trues(4), trues(3)) == false
    @test checkbounds(Bool, A, trues(5), trues(5), trues(3)) == false
    @test checkbounds(Bool, A, trues(5), trues(4), trues(4)) == false
    @test checkbounds(Bool, A, trues(60)) == true
    @test checkbounds(Bool, A, trues(61)) == false
    @test checkbounds(Bool, A, 2, 2, 2, trues(1)) == true  # extra indices
    @test checkbounds(Bool, A, 2, 2, 2, trues(2)) == false
    @test checkbounds(Bool, A, trues(5), trues(12)) == false
    @test checkbounds(Bool, A, trues(5), trues(13)) == false
    @test checkbounds(Bool, A, trues(6), trues(12)) == false
    @test checkbounds(Bool, A, trues(5, 4, 3)) == true
    @test checkbounds(Bool, A, trues(5, 4, 3, 1)) == true # issue 45867
    @test checkbounds(Bool, A, trues(5, 4, 2)) == false
    @test checkbounds(Bool, A, trues(5, 12)) == false
    @test checkbounds(Bool, A, trues(1, 5), trues(1, 4, 1), trues(1, 1, 3)) == false
    @test checkbounds(Bool, A, trues(1, 5), trues(1, 4, 1), trues(1, 1, 2)) == false
    @test checkbounds(Bool, A, trues(1, 5), trues(1, 5, 1), trues(1, 1, 3)) == false
    @test checkbounds(Bool, A, trues(1, 5), :, 2) == false
    @test checkbounds(Bool, A, trues(5, 4), trues(3)) == true
    @test checkbounds(Bool, A, trues(5), trues(4, 3, 1)) == true
    @test checkbounds(Bool, A, trues(5, 4), trues(3, 2)) == false
    @test checkbounds(Bool, A, trues(4, 4), trues(3)) == false
    @test checkbounds(Bool, A, trues(5, 4), trues(2)) == false
    @test checkbounds(Bool, A, trues(6, 4), trues(3)) == false
    @test checkbounds(Bool, A, trues(5, 4), trues(4)) == false
end

@testset "array of CartesianIndex" begin
    @test checkbounds(Bool, A, [CartesianIndex((1, 1, 1))]) == true
    @test checkbounds(Bool, A, [CartesianIndex((5, 4, 3))]) == true
    @test checkbounds(Bool, A, [CartesianIndex((0, 1, 1))]) == false
    @test checkbounds(Bool, A, [CartesianIndex((1, 0, 1))]) == false
    @test checkbounds(Bool, A, [CartesianIndex((1, 1, 0))]) == false
    @test checkbounds(Bool, A, [CartesianIndex((6, 4, 3))]) == false
    @test checkbounds(Bool, A, [CartesianIndex((5, 5, 3))]) == false
    @test checkbounds(Bool, A, [CartesianIndex((5, 4, 4))]) == false
    @test checkbounds(Bool, A, [CartesianIndex((1, 1))], 1) == true
    @test checkbounds(Bool, A, [CartesianIndex((5, 4))], 3) == true
    @test checkbounds(Bool, A, [CartesianIndex((0, 1))], 1) == false
    @test checkbounds(Bool, A, [CartesianIndex((1, 0))], 1) == false
    @test checkbounds(Bool, A, [CartesianIndex((1, 1))], 0) == false
    @test checkbounds(Bool, A, [CartesianIndex((6, 4))], 3) == false
    @test checkbounds(Bool, A, [CartesianIndex((5, 5))], 3) == false
    @test checkbounds(Bool, A, [CartesianIndex((5, 4))], 4) == false
    @test checkbounds(Bool, A, 5, [CartesianIndex((4, 3, 1))]) == true
    @test checkbounds(Bool, A, 5, [CartesianIndex((4, 3, 2))]) == false
    @test_throws ArgumentError checkbounds(Bool, A, [CartesianIndex((4, 3)), CartesianIndex((4,))])
    @test_throws ArgumentError checkbounds(Bool, A, [CartesianIndex((1,)), 1])
end

@testset "index conversion" begin
    @testset "0-dimensional" begin
        for i in ((), fill(0))
            @test LinearIndices(i)[1] == 1
            @test_throws BoundsError LinearIndices(i)[2]
            @test_throws BoundsError LinearIndices(i)[1:2]
            @test LinearIndices(i)[1,1] == 1
            @test LinearIndices(i)[] == 1
            @test size(LinearIndices(i)) == ()
            @test CartesianIndices(i)[1] == CartesianIndex()
            @test_throws BoundsError CartesianIndices(i)[2]
            @test_throws BoundsError CartesianIndices(i)[1:2]
            io = IOBuffer()
            show(io, CartesianIndices(i))
            @test String(take!(io)) == "CartesianIndices(())"
        end
    end

    @testset "1-dimensional" begin
        for i = 1:3
            @test LinearIndices((3,))[i] == i
            @test CartesianIndices((3,))[i] == CartesianIndex(i,)
        end
        @test LinearIndices((3,))[2,1] == 2
        @test LinearIndices((3,))[[1]] == [1]
        @test size(LinearIndices((3,))) == (3,)
        @test LinearIndices((3,))[1:2] === 1:2
        @test LinearIndices((3,))[1:2:3] === 1:2:3
        @test_throws BoundsError LinearIndices((3,))[2:4]
        @test_throws BoundsError CartesianIndices((3,))[2,2]
        #   ambiguity btw cartesian indexing and linear indexing in 1d when
        #   indices may be nontraditional
        @test_throws ArgumentError Base._sub2ind((1:3,), 2)
        @test_throws ArgumentError Base._ind2sub((1:3,), 2)

        ci = CartesianIndices((2:4,))
        @test first(ci) == ci[1] == CartesianIndex(2)
        @test last(ci)  == ci[end] == ci[3] == CartesianIndex(4)
        li = LinearIndices(ci)
        @test collect(li) == [1,2,3]
        @test first(li) == li[1] == 1
        @test last(li)  == li[3] == 3
        io = IOBuffer()
        show(io, ci)
        @test String(take!(io)) == "CartesianIndices((2:4,))"
    end

    @testset "2-dimensional" begin
        k = 0
        cartesian = CartesianIndices((4,3))
        linear = LinearIndices(cartesian)
        @test size(cartesian) == size(linear) == (4, 3)
        for j = 1:3, i = 1:4
            k += 1
            @test linear[i,j] == linear[k] == k
            @test cartesian[k] == CartesianIndex(i,j)
            @test LinearIndices(map(Base.Slice, (0:3,3:5)))[i-1,j+2] == k
            @test CartesianIndices(map(Base.Slice, (0:3,3:5)))[k] == CartesianIndex(i-1,j+2)
        end
        @test linear[linear] == linear
        @test linear[vec(linear)] == vec(linear)
        @test linear[cartesian] == linear
        @test linear[vec(cartesian)] == vec(linear)
        @test cartesian[linear] == cartesian
        @test cartesian[vec(linear)] == vec(cartesian)
        @test cartesian[cartesian] == cartesian
        @test cartesian[vec(cartesian)] == vec(cartesian)
        @test linear[2:3] === 2:3
        @test linear[3:-1:1] === 3:-1:1
        @test_throws BoundsError linear[4:13]
        io = IOBuffer()
        show(io, cartesian)
        @test String(take!(io)) == "CartesianIndices((4, 3))"
    end

    @testset "3-dimensional" begin
        l = 0
        for k = 1:2, j = 1:3, i = 1:4
            l += 1
            @test LinearIndices((4,3,2))[i,j,k] == l
            @test LinearIndices((4,3,2))[l] == l
            @test CartesianIndices((4,3,2))[i,j,k] == CartesianIndex(i,j,k)
            @test CartesianIndices((4,3,2))[l] == CartesianIndex(i,j,k)
            @test LinearIndices((1:4,1:3,1:2))[i,j,k] == l
            @test LinearIndices((1:4,1:3,1:2))[l] == l
            @test CartesianIndices((1:4,1:3,1:2))[i,j,k] == CartesianIndex(i,j,k)
            @test CartesianIndices((1:4,1:3,1:2))[l] == CartesianIndex(i,j,k)
        end

        l = 0
        for k = -101:-100, j = 3:5, i = 0:3
            l += 1
            @test LinearIndices(map(Base.Slice, (0:3,3:5,-101:-100)))[i,j,k] == l
            @test LinearIndices(map(Base.Slice, (0:3,3:5,-101:-100)))[l] == l
            @test CartesianIndices(map(Base.Slice, (0:3,3:5,-101:-100)))[i,j,k] == CartesianIndex(i,j,k)
            @test CartesianIndices(map(Base.Slice, (0:3,3:5,-101:-100)))[l] == CartesianIndex(i,j,k)
        end

        local A = reshape(Vector(1:9), (3,3))
        @test CartesianIndices(size(A))[6] == CartesianIndex(3,2)
        @test LinearIndices(size(A))[3, 2] == 6
        @test CartesianIndices(A)[6] == CartesianIndex(3,2)
        @test LinearIndices(A)[3, 2] == 6
        for i in 1:length(A)
            @test LinearIndices(A)[CartesianIndices(A)[i]] == i
        end

        @testset "PR #9256" begin
            function pr9256()
                m = [1 2 3; 4 5 6; 7 8 9]
                Base._ind2sub(m, 6)
            end
            @test pr9256() == (3,2)
        end
    end
end

@testset "AbstractArray fallbacks for CartesianIndices" begin
    @test ndims(CartesianIndices{3}) == 3
    @test eltype(CartesianIndices{3}) == CartesianIndex{3}
    for t in ((1:2, 1:2), (3:4,), ())
        C2 = CartesianIndices(t)
        @test ndims(C2) == length(t)
        @test ndims(typeof(C2)) == length(t)
        @test IndexStyle(C2) == IndexCartesian()
        @test eltype(C2) == CartesianIndex{length(t)}
        @test Base.IteratorSize(C2) isa Base.HasShape{length(t)}
    end
end

@testset "LinearIndices" begin
    @testset "constructors" begin
        for oinds in [
            (2, 3),
            (UInt8(2), 3),
            (2, UInt8(3)),
            (2, 1:3),
            (Base.OneTo(2), 1:3)
        ]
            R = LinearIndices(oinds)
            @test size(R) == (2, 3)
            @test axes(R) == (Base.OneTo(2), Base.OneTo(3))
            @test R[begin] == 1
            @test R[end] == 6
        end

        for oinds in [(2, ), (2, 3), (2, 3, 4)]
            R = CartesianIndices(oinds)
            @test size(R) == oinds
        end
    end

    @testset "IdentityUnitRange" begin
        function _collect(A)
            rst = eltype(A)[]
            for i in A
                push!(rst, i)
            end
            rst
        end
        function _simd_collect(A)
            rst = eltype(A)[]
            @simd for i in A
                push!(rst, i)
            end
            rst
        end

        for oinds in [
            (Base.IdentityUnitRange(0:1),),
            (Base.IdentityUnitRange(0:1), Base.IdentityUnitRange(0:2)),
            (Base.IdentityUnitRange(0:1), Base.OneTo(3)),
        ]
            R = LinearIndices(oinds)
            @test axes(R) === oinds
            @test _collect(R) == _simd_collect(R) == vec(collect(R))
        end
        R = LinearIndices((Base.IdentityUnitRange(0:1), 0:1))
        @test axes(R) == (Base.IdentityUnitRange(0:1), Base.OneTo(2))
    end

    @testset "show" begin
        A = zeros(2,3)
        for B in (A, view(A, Base.IdentityUnitRange(2:4)))
            l = LinearIndices(B)
            s = sprint(show, l)
            @test s == "LinearIndices($(axes(B)))"
        end
    end
end

@testset "copy for LinearIndices/CartesianIndices" begin
    C = CartesianIndices((1:2, 1:4))
    @test copy(C) === C
    L = LinearIndices((1:2, 1:4))
    @test copy(L) === L
end

# token type on which to dispatch testing methods in order to avoid potential
# name conflicts elsewhere in the base test suite
mutable struct TestAbstractArray end

## Tests for the abstract array interfaces with minimally defined array types

if !isdefined(@__MODULE__, :T24Linear)
    include("testhelpers/arrayindexingtypes.jl")
end

const can_inline = Base.JLOptions().can_inline != 0
function test_scalar_indexing(::Type{T}, shape, ::Type{TestAbstractArray}) where T
    N = prod(shape)
    A = reshape(Vector(1:N), shape)
    B = T(A)
    @test A == B
    # Test indexing up to 5 dimensions
    trailing5 = CartesianIndex(ntuple(Returns(1), max(ndims(B)-5, 0)))
    trailing4 = CartesianIndex(ntuple(Returns(1), max(ndims(B)-4, 0)))
    trailing3 = CartesianIndex(ntuple(Returns(1), max(ndims(B)-3, 0)))
    trailing2 = CartesianIndex(ntuple(Returns(1), max(ndims(B)-2, 0)))
    i=0
    for i5 = 1:size(B, 5)
        for i4 = 1:size(B, 4)
            for i3 = 1:size(B, 3)
                for i2 = 1:size(B, 2)
                    for i1 = 1:size(B, 1)
                        i += 1
                        @test A[i1,i2,i3,i4,i5,trailing5] == B[i1,i2,i3,i4,i5,trailing5] == i
                        @test A[i1,i2,i3,i4,i5,trailing5] ==
                              Base.unsafe_getindex(B, i1, i2, i3, i4, i5, trailing5) == i
                    end
                end
            end
        end
    end
    # Test linear indexing and partial linear indexing
    i=0
    for i1 = 1:length(B)
        i += 1
        @test A[i1] == B[i1] == i
    end
    i=0
    for i2 = 1:size(B, 2)
        for i1 = 1:size(B, 1)
            i += 1
            @test A[i1,i2,trailing2] == B[i1,i2,trailing2] == i
        end
    end
    @test A == B
    i=0
    for i3 = 1:size(B, 3)
        for i2 = 1:size(B, 2)
            for i1 = 1:size(B, 1)
                i += 1
                @test A[i1,i2,i3,trailing3] == B[i1,i2,i3,trailing3] == i
            end
        end
    end
    # Test zero-dimensional accesses
    @test A[1] == B[1] == 1
    # Test multidimensional scalar indexed assignment
    C = T(Int, shape)
    D1 = T(Int, shape)
    D2 = T(Int, shape)
    D3 = T(Int, shape)
    i=0
    for i5 = 1:size(B, 5)
        for i4 = 1:size(B, 4)
            for i3 = 1:size(B, 3)
                for i2 = 1:size(B, 2)
                    for i1 = 1:size(B, 1)
                        i += 1
                        C[i1,i2,i3,i4,i5,trailing5] = i
                        # test general unsafe_setindex!
                        Base.unsafe_setindex!(D1, i, i1,i2,i3,i4,i5,trailing5)
                        # test for dropping trailing dims
                        Base.unsafe_setindex!(D2, i, i1,i2,i3,i4,i5,trailing5, 1, 1, 1)
                        # test for expanding index argument to appropriate dims
                        Base.unsafe_setindex!(D3, i, i1,i2,i3,i4,trailing4)
                    end
                end
            end
        end
    end
    @test D1 == D2 == C == B == A
    @test D3[:, :, :, :, 1, trailing5] == D2[:, :, :, :, 1, trailing5]
    # Test linear indexing and partial linear indexing
    C = T(Int, shape)
    fill!(C, 0)
    @test C != B && C != A
    i=0
    for i1 = 1:length(C)
        i += 1
        C[i1] = i
    end
    @test C == B == A
    C = T(Int, shape)
    i=0
    C2 = reshape(C, Val(2))
    for i2 = 1:size(C2, 2)
        for i1 = 1:size(C2, 1)
            i += 1
            C2[i1,i2,trailing2] = i
        end
    end
    @test C == B == A
    C = T(Int, shape)
    i=0
    C3 = reshape(C, Val(3))
    for i3 = 1:size(C3, 3)
        for i2 = 1:size(C3, 2)
            for i1 = 1:size(C3, 1)
                i += 1
                C3[i1,i2,i3,trailing3] = i
            end
        end
    end
    @test C == B == A
    # Test zero-dimensional setindex
    if length(A) == 1
        A[] = 0; B[] = 0
        @test A[] == B[] == 0
        @test A == B
    else
        @test_throws BoundsError A[] = 0
        @test_throws BoundsError B[] = 0
        @test_throws BoundsError A[]
        @test_throws BoundsError B[]
    end
end

function test_vector_indexing(::Type{T}, shape, ::Type{TestAbstractArray}) where T
    @testset "test_vector_indexing{$(T)}" begin
        N = prod(shape)
        A = reshape(Vector(1:N), shape)
        B = T(A)
        trailing5 = CartesianIndex(ntuple(Returns(1), max(ndims(B)-5, 0)))
        trailing4 = CartesianIndex(ntuple(Returns(1), max(ndims(B)-4, 0)))
        trailing3 = CartesianIndex(ntuple(Returns(1), max(ndims(B)-3, 0)))
        trailing2 = CartesianIndex(ntuple(Returns(1), max(ndims(B)-2, 0)))
        idxs = rand(1:N, 3, 3, 3)
        @test B[idxs] == A[idxs] == idxs
        @test B[vec(idxs)] == A[vec(idxs)] == vec(idxs)
        @test B[:] == A[:] == 1:N
        @test B[1:end] == A[1:end] == 1:N
        @test B[:,:,trailing2] == A[:,:,trailing2] == B[:,:,1,trailing3] == A[:,:,1,trailing3]
            B[1:end,1:end,trailing2] == A[1:end,1:end,trailing2] == B[1:end,1:end,1,trailing3] == A[1:end,1:end,1,trailing3]

        @testset "Test with containers that aren't Int[]" begin
            @test B[[]] == A[[]] == []
            @test B[convert(Array{Any}, idxs)] == A[convert(Array{Any}, idxs)] == idxs
        end

        idx1 = rand(1:size(A, 1), 3)
        idx2 = rand(1:size(A, 2), 4, 5)
        @testset "Test adding dimensions with matrices" begin
            @test B[idx1, idx2, trailing2] == A[idx1, idx2, trailing2] == reshape(A[idx1, vec(idx2), trailing2], 3, 4, 5) == reshape(B[idx1, vec(idx2), trailing2], 3, 4, 5)
            @test B[1, idx2, trailing2] == A[1, idx2, trailing2] == reshape(A[1, vec(idx2), trailing2], 4, 5) == reshape(B[1, vec(idx2), trailing2], 4, 5)
        end
            # test removing dimensions with 0-d arrays
        @testset "test removing dimensions with 0-d arrays" begin
            idx0 = reshape([rand(1:size(A, 1))])
            @test B[idx0, idx2, trailing2] == A[idx0, idx2, trailing2] == reshape(A[idx0[], vec(idx2), trailing2], 4, 5) == reshape(B[idx0[], vec(idx2), trailing2], 4, 5)
            @test B[reshape([end]), reshape([end]), trailing2] == A[reshape([end]), reshape([end]), trailing2] == reshape([A[end,end,trailing2]]) == reshape([B[end,end,trailing2]])
        end

        mask = bitrand(shape)
        @testset "test logical indexing" begin
            let
                masks1 = (mask,)
                @test only(@inferred(to_indices(A, masks1))) isa Base.LogicalIndex{Int}
                if IndexStyle(B) isa IndexCartesian
                    @test only(@inferred(to_indices(B, masks1))) === Base.LogicalIndex(mask)
                end
            end
            @test B[mask] == A[mask] == B[findall(mask)] == A[findall(mask)] == LinearIndices(mask)[findall(mask)]
            @test B[vec(mask)] == A[vec(mask)] == LinearIndices(mask)[findall(mask)]
            mask1 = bitrand(size(A, 1))
            mask2 = bitrand(size(A, 2))
            @test B[mask1, mask2, trailing2] == A[mask1, mask2, trailing2] ==
                B[LinearIndices(mask1)[findall(mask1)], LinearIndices(mask2)[findall(mask2)], trailing2]
            @test B[mask1, 1, trailing2] == A[mask1, 1, trailing2] == LinearIndices(mask)[findall(mask1)]

            if ndims(B) > 1
                slice = ntuple(Returns(:), ndims(B)-1)
                maskfront = bitrand(shape[1:end-1])
                Bslicefront = B[slice..., 1]
                @test B[maskfront, 1] == Bslicefront[maskfront]
                @test size(B[maskfront, 1:1]) == (sum(maskfront), 1)
                maskend = bitrand(shape[2:end])
                Bsliceend = B[1, slice...]
                @test B[1 ,maskend] == Bsliceend[maskend]
                @test size(B[1:1, maskend]) == (1, sum(maskend))
            end
        end
    end
end

function test_primitives(::Type{T}, shape, ::Type{TestAbstractArray}) where T
    N = prod(shape)
    A = reshape(Vector(1:N), shape)
    B = T(A)

    # last(a)
    @test last(B) == B[lastindex(B)] == B[end] == A[end]
    @test lastindex(B) == lastindex(A) == last(LinearIndices(B))
    @test lastindex(B, 1) == lastindex(A, 1) == last(axes(B, 1))
    @test lastindex(B, 2) == lastindex(A, 2) == last(axes(B, 2))

    # first(a)
    @test first(B) == B[firstindex(B)] == B[begin] == B[1] == A[1] == A[begin]
    @test firstindex(B) == firstindex(A) == first(LinearIndices(B))
    @test firstindex(B, 1) == firstindex(A, 1) == first(axes(B, 1))
    @test firstindex(B, 2) == firstindex(A, 2) == first(axes(B, 2))

    @test !isassigned(B)
    # isassigned(a::AbstractArray, i::Integer...)
    j = rand(1:length(B))
    @test isassigned(B, j)
    if T == T24Linear
        @test !isassigned(B, length(B) + 1)
    end
    # isassigned(a::AbstractArray, i::CartesianIndex)
    @test isassigned(B, first(CartesianIndices(B)))
    ind = last(CartesianIndices(B))
    @test !isassigned(B, ind + oneunit(ind))
    # isassigned(a::AbstractArray, i::Union{Integer,CartesianIndex}...)
    @test isassigned(B, Int16.(first.(axes(B)))..., CartesianIndex(1,1))
    # Bool isn't a valid index
    @test_throws ArgumentError isassigned(B, Bool.(first.(axes(B)))..., CartesianIndex(1,1))
    @test_throws ArgumentError isassigned(B, Bool.(first.(axes(B)))...)
    @test_throws ArgumentError isassigned(B, true)
    @test_throws ArgumentError isassigned(B, false)

    # reshape(a::AbstractArray, dims::Dims)
    @test_throws DimensionMismatch reshape(B, (0, 1))

    # copyto!(dest::AbstractArray, src::AbstractArray)
    @test_throws BoundsError copyto!(Vector{Int}(undef, 10), [1:11...])

    # convert{T, N}(::Type{Array}, A::AbstractArray{T, N})
    X = [1:10...]
    Y = [1 2; 3 4]
    @test convert(Array, X) == X
    @test convert(Array, Y) == Y

    # convert{T}(::Type{Vector}, A::AbstractVector{T})
    @test convert(Vector, X) == X
    @test convert(Vector, view(X, 2:4)) == [2,3,4]
    @test_throws MethodError convert(Vector, Y)

    # convert{T}(::Type{Matrix}, A::AbstractMatrix{T})
    @test convert(Matrix, Y) == Y
    @test convert(Matrix, view(Y, 1:2, 1:2)) == Y
    @test_throws MethodError convert(Matrix, X)
end

mutable struct TestThrowNoGetindex{T} <: AbstractVector{T} end
@testset "ErrorException if getindex is not defined" begin
    Base.length(::TestThrowNoGetindex) = 2
    Base.size(::TestThrowNoGetindex) = (2,)
    @test_throws Base.CanonicalIndexError isassigned(TestThrowNoGetindex{Float64}(), 1)
end

function test_in_bounds(::Type{TestAbstractArray})
    n = rand(2:5)
    sz = rand(2:5, n)
    len = prod(sz)
    A = zeros(sz...)
    for i in 1:len
        @test checkbounds(Bool, A, i) == true
    end
    @test checkbounds(Bool, A, len + 1) == false
end

mutable struct UnimplementedFastArray{T, N} <: AbstractArray{T, N} end
Base.IndexStyle(::UnimplementedFastArray) = Base.IndexLinear()

mutable struct UnimplementedSlowArray{T, N} <: AbstractArray{T, N} end
Base.IndexStyle(::UnimplementedSlowArray) = Base.IndexCartesian()

mutable struct UnimplementedArray{T, N} <: AbstractArray{T, N} end

function test_getindex_internals(::Type{T}, shape, ::Type{TestAbstractArray}) where T
    N = prod(shape)
    A = reshape(Vector(1:N), shape)
    B = T(A)

    @test getindex(A, 1) == 1
    @test getindex(B, 1) == 1
    @test Base.unsafe_getindex(A, 1) == 1
    @test Base.unsafe_getindex(B, 1) == 1
end

function test_getindex_internals(::Type{TestAbstractArray})
    U = UnimplementedFastArray{Int, 2}()
    V = UnimplementedSlowArray{Int, 2}()
    @test_throws Base.CanonicalIndexError getindex(U, 1)
    @test_throws Base.CanonicalIndexError Base.unsafe_getindex(U, 1)
    @test_throws Base.CanonicalIndexError getindex(V, 1, 1)
    @test_throws Base.CanonicalIndexError Base.unsafe_getindex(V, 1, 1)
end

function test_setindex!_internals(::Type{T}, shape, ::Type{TestAbstractArray}) where T
    N = prod(shape)
    A = reshape(Vector(1:N), shape)
    B = T(A)

    Base.unsafe_setindex!(B, 2, 1)
    @test B[1] == 2
end

function test_setindex!_internals(::Type{TestAbstractArray})
    U = UnimplementedFastArray{Int, 2}()
    V = UnimplementedSlowArray{Int, 2}()
    @test_throws Base.CanonicalIndexError setindex!(U, 0, 1)
    @test_throws Base.CanonicalIndexError Base.unsafe_setindex!(U, 0, 1)
    @test_throws Base.CanonicalIndexError setindex!(V, 0, 1, 1)
    @test_throws Base.CanonicalIndexError Base.unsafe_setindex!(V, 0, 1, 1)
end

function test_get(::Type{TestAbstractArray})
    A = T24Linear(reshape([1:24...], 4, 3, 2))
    B = TSlow(reshape([1:24...], 4, 3, 2))

    @test get(A, (), 0) == 0
    @test get(B, (), 0) == 0
    @test get(A, (1,), 0) == get(A, 1, 0) == A[1] == 1
    @test get(B, (1,), 0) == get(B, 1, 0) == B[1] == 1
    @test get(A, (25,), 0) == get(A, 25, 0) == 0
    @test get(B, (25,), 0) == get(B, 25, 0) == 0
    @test get(A, (1,1,1), 0) == A[1,1,1] == 1
    @test get(B, (1,1,1), 0) == B[1,1,1] == 1
    @test get(A, (1,1,3), 0) == 0
    @test get(B, (1,1,3), 0) == 0

    @test get(TSlow([]), (), 0) == 0
    @test get(TSlow([1]), (), 0) == 1
    @test get(TSlow(fill(1)), (), 0) == 1

    global c = 0
    f() = (global c = c+1; 0)
    @test get(f, A, ()) == 0
    @test c == 1
    @test get(f, B, ()) == 0
    @test c == 2
    @test get(f, A, (1,)) == get(f, A, 1) == A[1] == 1
    @test c == 2
    @test get(f, B, (1,)) == get(f, B, 1) == B[1] == 1
    @test c == 2
    @test get(f, A, (25,)) == get(f, A, 25) == 0
    @test c == 4
    @test get(f, B, (25,)) == get(f, B, 25) == 0
    @test c == 6
    @test get(f, A, (1,1,1)) == A[1,1,1] == 1
    @test get(f, B, (1,1,1)) == B[1,1,1] == 1
    @test get(f, A, (1,1,3)) == 0
    @test c == 7
    @test get(f, B, (1,1,3)) == 0
    @test c == 8
    @test get(f, TSlow([]), ()) == 0
    @test c == 9
    @test get(f, TSlow([1]), ()) == 1
    @test get(f, TSlow(fill(1)), ()) == 1
end

function test_cat(::Type{TestAbstractArray})
    A = T24Linear([1:24...])
    b_int = reshape([1:27...], 3, 3, 3)
    b_float = reshape(Float64[1:27...], 3, 3, 3)
    b2hcat = Array{Float64}(undef, 3, 6, 3)
    b2vcat = Array{Float64}(undef, 6, 3, 3)
    b1 = reshape([1:9...], 3, 3)
    b2 = reshape([10:18...], 3, 3)
    b3 = reshape([19:27...], 3, 3)
    b2hcat[:, :, 1] = hcat(b1, b1)
    b2hcat[:, :, 2] = hcat(b2, b2)
    b2hcat[:, :, 3] = hcat(b3, b3)
    b2vcat[:, :, 1] = vcat(b1, b1)
    b2vcat[:, :, 2] = vcat(b2, b2)
    b2vcat[:, :, 3] = vcat(b3, b3)
    b3hcat = Array{Float64}(undef, 3, 9, 3)
    b3hcat[:, :, 1] = hcat(b1, b1, b1)
    b3hcat[:, :, 2] = hcat(b2, b2, b2)
    b3hcat[:, :, 3] = hcat(b3, b3, b3)
    b3vcat = Array{Float64}(undef, 9, 3, 3)
    b3vcat[:, :, 1] = vcat(b1, b1, b1)
    b3vcat[:, :, 2] = vcat(b2, b2, b2)
    b3vcat[:, :, 3] = vcat(b3, b3, b3)
    B = TSlow(b_int)
    B1 = TSlow([1:24...])
    B2 = TSlow([1:25...])
    C1 = TSlow([1 2; 3 4])
    C2 = TSlow([1 2 3; 4 5 6])
    C3 = TSlow([1 2; 3 4; 5 6])
    D = [1:24...]
    i = rand(1:10)

    @test cat(;dims=i) == Any[]
    @test Base.typed_hcat(Float64) == Vector{Float64}()
    @test Base.typed_vcat(Float64) == Vector{Float64}()
    @test vcat() == Any[]
    @test hcat() == Any[]
    @test vcat(1, 1.0, 3, 3.0) == [1.0, 1.0, 3.0, 3.0]
    @test hcat(1, 1.0, 3, 3.0) == [1.0 1.0 3.0 3.0]
    @test_throws DimensionMismatch hcat(B1, B2)
    @test_throws DimensionMismatch vcat(C1, C2)

    @test vcat(B) == B
    @test hcat(B) == B
    @test Base.typed_vcat(Float64, B) == TSlow(b_float)
    @test Base.typed_vcat(Float64, B, B) == TSlow(b2vcat)
    @test Base.typed_vcat(Float64, B, B, B) == TSlow(b3vcat)
    @test Base.typed_hcat(Float64, B) == TSlow(b_float)
    @test Base.typed_hcat(Float64, B, B) == TSlow(b2hcat)
    @test Base.typed_hcat(Float64, B, B, B) == TSlow(b3hcat)

    @testset "issue #49676, bad error message on v[1 +1]" begin
        # This is here because all these expressions are handled by Base.typed_hcat
        v = [1 2 3]
        @test_throws ArgumentError v[1 +1]
        @test_throws ArgumentError v[1 1]
        @test_throws ArgumentError v[[1 2] [2 3]]
    end

    @test vcat(B1, B2) == TSlow(vcat([1:24...], [1:25...]))
    @test hcat(C1, C2) == TSlow([1 2 1 2 3; 3 4 4 5 6])
    @test hcat(C1, C2, C1) == TSlow([1 2 1 2 3 1 2; 3 4 4 5 6 3 4])

    # hvcat
    for nbc in (1, 2, 3, 4, 5, 6)
        @test hvcat(nbc, 1:120...) == reshape([1:120...], nbc, round(Int, 120 / nbc))'
    end

    @test_throws ArgumentError hvcat(7, 1:20...)
    @test_throws DimensionMismatch hvcat((2), C1, C3)
    @test_throws DimensionMismatch hvcat((1), C1, C2)
    @test_throws DimensionMismatch hvcat((1), C2, C3)

    tup = tuple(rand(1:10, i)...)
    @test hvcat(tup) == []

    # check for shape mismatch
    @test_throws ArgumentError hvcat((2, 2), 1, 2, 3, 4, 5)
    @test_throws ArgumentError Base.typed_hvcat(Int, (2, 2), 1, 2, 3, 4, 5)
    # check for # of columns mismatch b/w rows
    @test_throws DimensionMismatch hvcat((3, 2), 1, 2, 3, 4, 5, 6)
    @test_throws DimensionMismatch Base.typed_hvcat(Int, (3, 2), 1, 2, 3, 4, 5, 6)

    # 18395
    @test isa(Any["a" 5; 2//3 1.0][2,1], Rational{Int})

    # 13665, 19038
    @test @inferred(hcat([1.0 2.0], 3))::Array{Float64,2} == [1.0 2.0 3.0]
    @test @inferred(vcat([1.0, 2.0], 3))::Array{Float64,1} == [1.0, 2.0, 3.0]

    @test @inferred(vcat(["a"], "b"))::Vector{String} == ["a", "b"]
    @test @inferred(vcat((1,), (2.0,)))::Vector{Tuple{Real}} == [(1,), (2.0,)]

    # 29172
    @test_throws ArgumentError cat([1], [2], dims=0)
    @test_throws ArgumentError cat([1], [2], dims=[5, -3])

    # 36041
    @test_throws MethodError cat(["a"], ["b"], dims=[1, 2])
    @test cat([1], [1], dims=[1, 2]) == I(2)

    # inferrability
    As = [zeros(2, 2) for _ = 1:2]
    @test @inferred(cat(As...; dims=Val(3))) == zeros(2, 2, 2)
    cat3v(As) = cat(As...; dims=Val(3))
    @test @inferred(cat3v(As)) == zeros(2, 2, 2)
    @test @inferred(cat(As...; dims=Val((1,2)))) == zeros(4, 4)

    r = rand(Float32, 56, 56, 64, 1);
    f(r) = cat(r, r, dims=(3,))
    @inferred f(r);
end

function test_ind2sub(::Type{TestAbstractArray})
    n = rand(2:5)
    dims = tuple(rand(1:5, n)...)
    len = prod(dims)
    A = reshape(Vector(1:len), dims...)
    I = CartesianIndices(dims)
    for i in 1:len
        @test A[I[i]] == A[i]
    end
end

# A custom linear slow array that insists upon Cartesian indexing
mutable struct TSlowNIndexes{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
end
Base.IndexStyle(::Type{A}) where {A<:TSlowNIndexes} = Base.IndexCartesian()
Base.size(A::TSlowNIndexes) = size(A.data)
Base.getindex(A::TSlowNIndexes, index::Int...) = error("Must use $(ndims(A)) indices")
Base.getindex(A::TSlowNIndexes{T,2}, i::Int, j::Int) where {T} = A.data[i,j]


@testset "issue #15689, mapping an abstract type" begin
    @test isa(map(Set, Array[[1,2],[3,4]]), Vector{Set{Int}})
end

@testset "mapping over scalars" begin
    @test map(sin, 1) === sin(1)
end

function test_UInt_indexing(::Type{TestAbstractArray})
    A = [1:100...]
    _A = Expr(:quote, A)
    for i in 1:100
        _i8 = convert(UInt8, i)
        _i16 = convert(UInt16, i)
        _i32 = convert(UInt32, i)
        for _i in (_i8, _i16, _i32)
            @eval begin
                @test $_A[$_i] == $i
            end
        end
    end
end

# Issue 13315
function test_13315(::Type{TestAbstractArray})
    U = UInt(1):UInt(2)
    @test [U;[U;]] == [UInt(1), UInt(2), UInt(1), UInt(2)]
end

# checksquare
function test_checksquare()
    @test LinearAlgebra.checksquare(zeros(2,2)) == 2
    @test LinearAlgebra.checksquare(zeros(2,2),zeros(3,3)) == [2,3]
    @test_throws DimensionMismatch LinearAlgebra.checksquare(zeros(2,3))
end

#----- run tests -------------------------------------------------------------#

@testset for T in (T24Linear, TSlow), shape in ((24,), (2, 12), (2,3,4), (1,2,3,4), (4,3,2,1))
    test_scalar_indexing(T, shape, TestAbstractArray)
    test_vector_indexing(T, shape, TestAbstractArray)
    test_primitives(T, shape, TestAbstractArray)
    test_getindex_internals(T, shape, TestAbstractArray)
    test_setindex!_internals(T, shape, TestAbstractArray)
end
test_in_bounds(TestAbstractArray)
test_getindex_internals(TestAbstractArray)
test_setindex!_internals(TestAbstractArray)
test_get(TestAbstractArray)
test_cat(TestAbstractArray)
test_ind2sub(TestAbstractArray)

include("generic_map_tests.jl")
generic_map_tests(map, map!)
@test map!(-, [1]) == [-1]

test_UInt_indexing(TestAbstractArray)
test_13315(TestAbstractArray)
test_checksquare()

A = TSlowNIndexes(rand(2,2))
@test_throws ErrorException A[1]
@test A[1,1] == A.data[1]
@test first(A) == A.data[1]

@testset "#16381" begin
    @inferred size(rand(3,2,1))
    @inferred size(rand(3,2,1), 2)

    @test @inferred(axes(rand(3,2)))    == (1:3,1:2)
    @test @inferred(axes(rand(3,2,1)))  == (1:3,1:2,1:1)
    @test @inferred(axes(rand(3,2), 1)) == 1:3
    @test @inferred(axes(rand(3,2), 2)) == 1:2
    @test @inferred(axes(rand(3,2), 3)) == 1:1
end

@testset "isinteger and isreal" begin
    @test all(isinteger, Diagonal(rand(1:5,5)))
    @test isreal(Diagonal(rand(5)))
end

@testset "unary ops" begin
    let A = Diagonal(rand(1:5,5))
        @test +(A) == A
        @test *(A) == A
    end
end

@testset "reverse dim on empty" begin
    @test reverse(Diagonal([]),dims=1) == Diagonal([])
end

@testset "ndims and friends" begin
    @test ndims(Diagonal(rand(1:5,5))) == 2
    @test ndims(Diagonal{Float64}) == 2
    @test ndims(Diagonal) == 2
    @test ndims(Vector) == 1
    @test ndims(Matrix) == 2
    @test ndims(Array{<:Any, 0}) == 0
    @test_throws MethodError ndims(Array)
end

@testset "Issue #17811" begin
    A17811 = Integer[]
    I = [abs(x) for x in A17811]
    @test isa(I, Array{Any,1})
    push!(I, 1)
    @test I == Any[1]
    @test isa(map(abs, A17811), Array{Any,1})
end

@testset "copymutable for itrs" begin
    @test Base.copymutable((1,2,3)) == [1,2,3]
end

@testset "_sub2ind for empty tuple" begin
    @test Base._sub2ind(()) == 1
end

@testset "to_shape" begin
    @test Base.to_shape(()) === ()
    @test Base.to_shape(1) === 1
    @test Base.to_shape(big(1)) === Base.to_shape(1)
    @test Base.to_shape(Int8(1)) === Base.to_shape(1)
end

@testset "issue #39923: similar" begin
    for ax in [(big(2), big(3)), (big(2), 3), (UInt64(2), 3), (2, UInt32(3)),
        (big(2), Base.OneTo(3)), (Base.OneTo(2), Base.OneTo(big(3)))]

        A = similar(ones(), Int, ax)
        @test axes(A) === (Base.OneTo(2), Base.OneTo(3))
        @test eltype(A) === Int
    end
end

@testset "issue #19267" begin
    @test ndims((1:3)[:]) == 1
    @test ndims((1:3)[:,:]) == 2
    @test ndims((1:3)[:,[1],:]) == 3
    @test ndims((1:3)[:,[1],:,[1]]) == 4
    @test ndims((1:3)[:,[1],1:1,:]) == 4
    @test ndims((1:3)[:,:,1:1,:]) == 4
    @test ndims((1:3)[:,:,1:1]) == 3
    @test ndims((1:3)[:,:,1:1,:,:,[1]]) == 6
end

@testset "issue #38192" begin
    img = cat([1 2; 3 4], [1 5; 6 7]; dims=3)
    mask = img[:,:,1] .== img[:,:,2]
    img[mask,2] .= 0
    @test img == cat([1 2; 3 4], [0 5; 6 7]; dims=3)
end

@testset "dispatch loop introduced in #19305" begin
    Z22, O33 = fill(0, 2, 2), fill(1, 3, 3)
    @test [(1:2) Z22; O33] == [[1,2] Z22; O33] == [[1 2]' Z22; O33]
end

@testset "checkbounds_indices method ambiguities #20989" begin
    @test Base.checkbounds_indices(Bool, (1:1,), ([CartesianIndex(1)],))
end

# keys, values, pairs
for A in (rand(2), rand(2,3))
    local A
    for (i, v) in pairs(A)
        @test A[i] == v
    end
    @test Array(values(A)) == A

     @test keytype(A) == keytype(typeof(A)) == eltype(keys(A))
     @test valtype(A) == valtype(typeof(A)) == eltype(values(A))
end

# nextind and prevind
@test nextind(zeros(4), 2) == 3
@test nextind(zeros(2,3), CartesianIndex(2,1)) == CartesianIndex(1, 2)
@test prevind(zeros(4), 2) == 1
@test prevind(zeros(2,3), CartesianIndex(2,1)) == CartesianIndex(1, 1)

@testset "ImageCore #40" begin
    Base.convert(::Type{Array{T,n}}, a::Array{T,n}) where {T<:Number,n} = a
    Base.convert(::Type{Array{T,n}}, a::Array) where {T<:Number,n} =
        copyto!(Array{T,n}(undef, size(a)), a)
    @test isa(empty(Dict(:a=>1, :b=>2.0), Union{}, Union{}), Dict{Union{}, Union{}})
end

@testset "zero-dimensional copy" begin
    Z = Array{Int,0}(undef); Z[] = 17
    @test Z == Array(Z) == copy(Z)
end

@testset "empty" begin
    @test isempty([])
    v = [1, 2, 3]
    v2 = empty(v)
    v3 = empty(v, Float64)
    @test !isempty(v)
    empty!(v)
    @test isempty(v)
    @test isempty(v2::Vector{Int})
    @test isempty(v3::Vector{Float64})

    S = StructArrays.StructArray{Complex{Int}}((v, v))
    for T in (Complex{Int}, ComplexF64)
        S0 = empty(S, T)
        @test S0 isa StructArrays.StructArray{T}
        @test length(S0) == 0
    end
    S0 = empty(S, String)
    @test S0 isa Vector{String}
    @test length(S0) == 0
end

@testset "abstract return type inference of function that calls `length(::Array)`" begin
    f(x) = length(x)
    @test Int === Base.infer_return_type(f, Tuple{Array})
end

@testset "CartesianIndices" begin
    xrng = 2:4
    yrng = 1:5
    CR = CartesianIndices(map(Base.Slice, (xrng,yrng)))

    for i in xrng, j in yrng
        @test CR[i,j] == CartesianIndex(i,j)
    end

    for i_lin in LinearIndices(CR)
        i = (i_lin-1) % length(xrng) + 1
        j = (i_lin-i) ÷ length(xrng) + 1
        @test CR[i_lin] == CartesianIndex(xrng[i],yrng[j])
    end

    @test CartesianIndices(fill(1., 2, 3)) == CartesianIndices((2,3))
    @test LinearIndices((2,3)) == [1 3 5; 2 4 6]

    for IType in (CartesianIndices, LinearIndices)
        I1 = IType((Base.OneTo(3),))
        I2 = IType((1:3,))
        @test !(I1 === I2)
        J1, J2 = @inferred(promote(I1, I2))
        @test J1 === J2
    end

    i = CartesianIndex(17,-2)
    @test CR .+ i === i .+ CR === CartesianIndices((19:21, -1:3)) == collect(CR) .+ i
    @test CR .- i === CartesianIndices((-15:-13, 3:7)) == collect(CR) .- i
    @test collect(i .- CR) == Ref(i) .- collect(CR) == i .- collect(CR)
end

@testset "issue #25770" begin
    @test vcat(1:3, fill(1, (2,1))) == vcat([1:3;], fill(1, (2,1))) == reshape([1,2,3,1,1], 5,1)
    @test hcat(1:2, fill(1, (2,1))) == hcat([1:2;], fill(1, (2,1))) == reshape([1,2,1,1],2,2)
    @test [(1:3) (4:6); fill(1, (3,2))] == reshape([1,2,3,1,1,1,4,5,6,1,1,1], 6,2)
end

@testset "copy!" begin
    @testset "AbstractVector" begin
        s = Vector([1, 2])
        for a = ([1], UInt[1], [3, 4, 5], UInt[3, 4, 5])
            @test s === copy!(s, Vector(a)) == Vector(a)
        end
        # issue #35649
        s = [1, 2, 3, 4]
        s2 = reshape(s, 2, 2) # shared data
        @test s === copy!(s, 11:14) == 11:14
    end
    @testset "AbstractArray" begin
        @test_throws ArgumentError copy!(zeros(2, 3), zeros(3, 2))
        s = zeros(2, 2)
        @test s === copy!(s, fill(1, 2, 2)) == fill(1, 2, 2)
        @test s === copy!(s, fill(1.0, 2, 2)) == fill(1.0, 2, 2)
    end
end

@testset "map on Dicts/Sets is forbidden" begin
    @test_throws ErrorException map(identity, Set([1,2,3]))
    @test_throws ErrorException map(identity, Dict("a"=>"b"))
end

@testset "Issue 30145" begin
    X = [1,2,3]
    @test isempty(X[Union{}[]])
end

@testset "Issue 30259" begin
    A = randn(1,2,3)
    @test get(A, CartesianIndex(1,2,3), :some_default) === A[1,2,3]
    @test get(A, CartesianIndex(2,2,3), :some_default) === :some_default
    @test get(11:15, CartesianIndex(6), nothing) === nothing
    @test get(11:15, CartesianIndex(5), nothing) === 15
end

@testset "IndexStyle for various types" begin
    @test Base.IndexStyle(UpperTriangular) == IndexCartesian() # subtype of AbstractArray, not of Array
    @test Base.IndexStyle(Vector) == IndexLinear()
    @test Base.IndexStyle(Memory) == IndexLinear()
    @test Base.IndexStyle(UnitRange) == IndexLinear()
    @test Base.IndexStyle(UpperTriangular(rand(3, 3)), [1; 2; 3]) == IndexCartesian()
    @test Base.IndexStyle(UpperTriangular(rand(3, 3)), rand(3, 3), [1; 2; 3]) == IndexCartesian()
    @test Base.IndexStyle(rand(3, 3), [1; 2; 3]) == IndexLinear()
end

@testset "promote_shape for Tuples and Dims" begin
    @test promote_shape((2, 1), (2,)) == (2, 1)
    @test_throws DimensionMismatch promote_shape((2, 3), (2,))
    @test promote_shape(Dims((2, 1)), Dims((2,))) == (2, 1)
    @test_throws DimensionMismatch promote_shape(Dims((2, 2)), Dims((2,)))
    @test_throws DimensionMismatch promote_shape(Dims((2, 3, 1)), Dims((2,2)))
end

@testset "getindex and setindex! for Ref" begin
    for x in [Ref(1), Ref([1,2,3], 1)]
        @test getindex(x) == getindex(x, CartesianIndex()) == 1
        x[CartesianIndex()] = 10
        @test getindex(x) == getindex(x, CartesianIndex()) == 10
    end
end

@testset "vcat with mixed elements" begin
    @test vcat(Nothing[], [missing], [1.0], [Int8(1)]) isa Vector{Union{Missing, Nothing, Float64}}
end

@testset "sizeof" begin
    let arrUInt8 = zeros(UInt8, 10)
        @test sizeof(arrUInt8) == 10
        @test Core.sizeof(arrUInt8) == 3 * sizeof(Int)
    end

    let arrUInt32 = zeros(UInt32, 10)
        @test sizeof(arrUInt32) == 40
        @test Core.sizeof(arrUInt32) == 3 * sizeof(Int)
    end

    let arrFloat64 = zeros(Float64, 10, 10)
        @test sizeof(arrFloat64) == 800
        @test Core.sizeof(arrFloat64) == 4 * sizeof(Int)
    end

    # Test union arrays (Issue #23321)
    let arrUnion = Union{Int64, Cvoid}[rand(Bool) ? k : nothing for k = 1:10]
        @test sizeof(arrUnion) == 80
        @test Core.sizeof(arrUnion) == 3 * sizeof(Int)
    end

    # Test non-power of 2 types (Issue #35884)
    primitive type UInt48 48 end
    UInt48(x::UInt64) = Core.Intrinsics.trunc_int(UInt48, x)
    UInt48(x::UInt32) = Core.Intrinsics.zext_int(UInt48, x)

    a = UInt48(0x00000001);
    b = UInt48(0x00000002);
    c = UInt48(0x00000003);
    let arrayOfUInt48 = [a, b, c]
        f35884(x) = sizeof(x)
        @test f35884(arrayOfUInt48) == 24
        @test Core.sizeof(arrayOfUInt48) == 3 * sizeof(Int)
    end
end

struct Strider{T,N} <: AbstractArray{T,N}
    data::Vector{T}
    offset::Int
    strides::NTuple{N,Int}
    size::NTuple{N,Int}
end
function Strider{T}(strides::NTuple{N}, size::NTuple{N}) where {T,N}
    offset = 1-sum(strides .* (strides .< 0) .* (size .- 1))
    data = Array{T}(undef, sum(abs.(strides) .* (size .- 1)) + 1)
    return Strider{T, N, Vector{T}}(data, offset, strides, size)
end
function Strider(vec::AbstractArray{T}, strides::NTuple{N}, size::NTuple{N}) where {T,N}
    offset = 1-sum(strides .* (strides .< 0) .* (size .- 1))
    @assert length(vec) >= sum(abs.(strides) .* (size .- 1)) + 1
    return Strider{T, N}(vec, offset, strides, size)
end
Base.size(S::Strider) = S.size
function Base.getindex(S::Strider{<:Any,N}, I::Vararg{Int,N}) where {N}
    return S.data[sum(S.strides .* (I .- 1)) + S.offset]
end
Base.strides(S::Strider) = S.strides
Base.elsize(::Type{<:Strider{T}}) where {T} = Base.elsize(Vector{T})
Base.cconvert(::Type{Ptr{T}}, S::Strider{T}) where {T} = memoryref(S.data.ref, S.offset)

@testset "Simple 3d strided views and permutes" for sz in ((5, 3, 2), (7, 11, 13))
    A = collect(reshape(1:prod(sz), sz))
    S = Strider(vec(A), strides(A), sz)
    @test pointer(A) == pointer(S)
    for i in 1:prod(sz)
        @test pointer(A, i) == pointer(S, i)
        @test A[i] == S[i]
    end
    for idxs in ((1:sz[1], 1:sz[2], 1:sz[3]),
                 (1:sz[1], 2:2:sz[2], sz[3]:-1:1),
                 (2:2:sz[1]-1, sz[2]:-1:1, sz[3]:-2:2),
                 (sz[1]:-1:1, sz[2]:-1:1, sz[3]:-1:1),
                 (sz[1]-1:-3:1, sz[2]:-2:3, 1:sz[3]),)
        Ai = A[idxs...]
        Av = view(A, idxs...)
        Sv = view(S, idxs...)
        Ss = Strider{Int, 3}(vec(A), sum((first.(idxs).-1).*strides(A))+1, strides(Av), length.(idxs))
        @test pointer(Av) == pointer(Sv) == pointer(Ss)
        for i in 1:length(Av)
            @test pointer(Av, i) == pointer(Sv, i) == pointer(Ss, i)
            @test Ai[i] == Av[i] == Sv[i] == Ss[i]
        end
        for perm in ((3, 2, 1), (2, 1, 3), (3, 1, 2))
            P = permutedims(A, perm)
            Ap = Base.PermutedDimsArray(A, perm)
            Sp = Base.PermutedDimsArray(S, perm)
            Ps = Strider{Int, 3}(vec(A), 1, strides(A)[collect(perm)], sz[collect(perm)])
            @test pointer(Ap) == pointer(Sp) == pointer(Ps)
            for i in 1:length(Ap)
                # This is intentionally disabled due to ambiguity. See `Base.pointer(A::PermutedDimsArray, i::Integer)`.
                # But only evaluate one iteration as broken to reduce test report noise
                i == 1 && @test_broken pointer(Ap, i) == pointer(Sp, i) == pointer(Ps, i)
                @test P[i] == Ap[i] == Sp[i] == Ps[i]
            end
            Pv = view(P, idxs[collect(perm)]...)
            Pi = P[idxs[collect(perm)]...]
            Apv = view(Ap, idxs[collect(perm)]...)
            Spv = view(Sp, idxs[collect(perm)]...)
            Pvs = Strider{Int, 3}(vec(A), sum((first.(idxs).-1).*strides(A))+1, strides(Apv), size(Apv))
            @test pointer(Apv) == pointer(Spv) == pointer(Pvs)
            for i in 1:length(Apv)
                @test pointer(Apv, i) == pointer(Spv, i) == pointer(Pvs, i)
                @test Pi[i] == Pv[i] == Apv[i] == Spv[i] == Pvs[i]
            end
            Vp = permutedims(Av, perm)
            Ip = permutedims(Ai, perm)
            Avp = Base.PermutedDimsArray(Av, perm)
            Svp = Base.PermutedDimsArray(Sv, perm)
            @test pointer(Avp) == pointer(Svp)
            for i in 1:length(Avp)
                # This is intentionally disabled due to ambiguity. See `Base.pointer(A::PermutedDimsArray, i::Integer)`
                # But only evaluate one iteration as broken to reduce test report noise
                i == 1 && @test_broken pointer(Avp, i) == pointer(Svp, i)
                @test Ip[i] == Vp[i] == Avp[i] == Svp[i]
            end
        end
    end
    # constant propagation in the PermutedDimsArray constructor
    X = @inferred (A -> PermutedDimsArray(A, (2,3,1)))(A)
    @test @inferred((X -> PermutedDimsArray(X, (3,1,2)))(X)) == A
end

@testset "simple 2d strided views, permutes, transposes" for sz in ((5, 3), (7, 11))
    A = collect(reshape(1:prod(sz), sz))
    S = Strider(vec(A), strides(A), sz)
    @test pointer(A) == pointer(S)
    for i in 1:prod(sz)
        @test pointer(A, i) == pointer(S, i)
        @test A[i] == S[i]
    end
    for idxs in ((1:sz[1], 1:sz[2]),
                 (1:sz[1], 2:2:sz[2]),
                 (2:2:sz[1]-1, sz[2]:-1:1),
                 (sz[1]:-1:1, sz[2]:-1:1),
                 (sz[1]-1:-3:1, sz[2]:-2:3),)
        Av = view(A, idxs...)
        Sv = view(S, idxs...)
        Ss = Strider{Int, 2}(vec(A), sum((first.(idxs).-1).*strides(A))+1, strides(Av), length.(idxs))
        @test pointer(Av) == pointer(Sv) == pointer(Ss)
        for i in 1:length(Av)
            @test pointer(Av, i) == pointer(Sv, i) == pointer(Ss, i)
            @test Av[i] == Sv[i] == Ss[i]
        end
        perm = (2, 1)
        P = permutedims(A, perm)
        Ap = Base.PermutedDimsArray(A, perm)
        At = transpose(A)
        Aa = adjoint(A)
        St = transpose(A)
        Sa = adjoint(A)
        Sp = Base.PermutedDimsArray(S, perm)
        Ps = Strider{Int, 2}(vec(A), 1, strides(A)[collect(perm)], sz[collect(perm)])
        @test pointer(Ap) == pointer(Sp) == pointer(Ps) == pointer(At) == pointer(Aa)
        for i in 1:length(Ap)
            # This is intentionally disabled due to ambiguity. See `Base.pointer(A::PermutedDimsArray, i::Integer)`
            # But only evaluate one iteration as broken to reduce test report noise
            i == 1 && @test_broken pointer(Ap, i) == pointer(Sp, i) == pointer(Ps, i) == pointer(At, i) == pointer(Aa, i) == pointer(St, i) == pointer(Sa, i)
            @test pointer(Ps, i) == pointer(At, i) == pointer(Aa, i) == pointer(St, i) == pointer(Sa, i)
            @test P[i] == Ap[i] == Sp[i] == Ps[i] == At[i] == Aa[i] == St[i] == Sa[i]
        end
        Pv = view(P, idxs[collect(perm)]...)
        Apv = view(Ap, idxs[collect(perm)]...)
        Atv = view(At, idxs[collect(perm)]...)
        Ata = view(Aa, idxs[collect(perm)]...)
        Stv = view(St, idxs[collect(perm)]...)
        Sta = view(Sa, idxs[collect(perm)]...)
        Spv = view(Sp, idxs[collect(perm)]...)
        Pvs = Strider{Int, 2}(vec(A), sum((first.(idxs).-1).*strides(A))+1, strides(Apv), size(Apv))
        @test pointer(Apv) == pointer(Spv) == pointer(Pvs) == pointer(Atv) == pointer(Ata)
        for i in 1:length(Apv)
            @test pointer(Apv, i) == pointer(Spv, i) == pointer(Pvs, i) == pointer(Atv, i) == pointer(Ata, i) == pointer(Stv, i) == pointer(Sta, i)
            @test Pv[i] == Apv[i] == Spv[i] == Pvs[i] == Atv[i] == Ata[i] == Stv[i] == Sta[i]
        end
        Vp = permutedims(Av, perm)
        Avp = Base.PermutedDimsArray(Av, perm)
        Avt = transpose(Av)
        Ava = adjoint(Av)
        Svt = transpose(Sv)
        Sva = adjoint(Sv)
        Svp = Base.PermutedDimsArray(Sv, perm)
        @test pointer(Avp) == pointer(Svp) == pointer(Avt) == pointer(Ava)
        for i in 1:length(Avp)
            # This is intentionally disabled due to ambiguity. See `Base.pointer(A::PermutedDimsArray, i::Integer)`
            # But only evaluate one iteration as broken to reduce test report noise
            i == 1 && @test_broken pointer(Avp, i) == pointer(Svp, i) == pointer(Avt, i) == pointer(Ava, i) == pointer(Svt, i) == pointer(Sva, i)
            @test pointer(Avt, i) == pointer(Ava, i) == pointer(Svt, i) == pointer(Sva, i)
            @test Vp[i] == Avp[i] == Svp[i] == Avt[i] == Ava[i] == Svt[i] == Sva[i]
        end
    end
end

@testset "first/last n elements of $(typeof(itr))" for itr in (collect(1:9),
                                                               [1 4 7; 2 5 8; 3 6 9],
                                                               ntuple(identity, 9))
    @test first(itr, 6) == [itr[1:6]...]
    @test first(itr, 25) == [itr[:]...]
    @test first(itr, 25) !== itr
    @test first(itr, 1) == [itr[1]]
    @test_throws ArgumentError first(itr, -6)
    @test last(itr, 6) == [itr[end-5:end]...]
    @test last(itr, 25) == [itr[:]...]
    @test last(itr, 25) !== itr
    @test last(itr, 1) == [itr[end]]
    @test_throws ArgumentError last(itr, -6)

    @testset "overflow (issue #45842)" begin
        @test_throws OverflowError first(typemin(Int):typemax(Int), 10)
        @test first(2:typemax(Int)-1, typemax(Int)÷2) === 2:((typemax(Int)÷2) + 1)
        @test last(2:typemax(Int), typemax(Int)÷2) ===
            range(stop=typemax(Int), length=typemax(Int)÷2)
    end
end

@testset "Base.rest" begin
    a = reshape(1:4, 2, 2)'
    @test Base.rest(a) == a[:]
    _, st = iterate(a)
    @test Base.rest(a, st) == [3, 2, 4]
end

@testset "issue #37741, non-int cat" begin
    @test [1; 1:BigInt(5)] == [1; 1:5]
    @test [1:BigInt(5); 1] == [1:5; 1]
end

@testset "Base.isstored" begin
    a = rand(3, 4, 5)
    @test Base.isstored(a, 1, 2, 3)
    @test_throws BoundsError Base.isstored(a, 4, 4, 5)
    @test_throws BoundsError Base.isstored(a, 3, 5, 5)
    @test_throws BoundsError Base.isstored(a, 3, 4, 6)
end

mutable struct TestPushArray{T, N} <: AbstractArray{T, N}
    data::Array{T}
end
Base.push!(tpa::TestPushArray{T}, a::T) where T = push!(tpa.data, a)
Base.pushfirst!(tpa::TestPushArray{T}, a::T) where T = pushfirst!(tpa.data, a)

push_slightly_abstract_namedtuple(v::Vector{@NamedTuple{x::Int,y::Any}}, x::Int, @nospecialize(y)) = push!(v, (; x, y))

@testset "push! and pushfirst!" begin
    a_orig = [1]
    tpa = TestPushArray{Int, 2}(a_orig)
    push!(tpa, 2, 3, 4, 5, 6)
    @test tpa.data == collect(1:6)
    a_orig = [1]
    tpa = TestPushArray{Int, 2}(a_orig)
    pushfirst!(tpa, 6, 5, 4, 3, 2)
    @test tpa.data == reverse(collect(1:6))

    let src = code_typed1(push_slightly_abstract_namedtuple, (Vector{@NamedTuple{x::Int,y::Any}},Int,Any))
        # After optimization, all `push!` and `convert` calls should have been inlined
        @test all((x)->!iscall((src, push!))(x) && !iscall((src, convert))(x), src.code)
    end
end

mutable struct SimpleArray{T} <: AbstractVector{T}
    els::Vector{T}
end
Base.size(sa::SimpleArray) = size(sa.els)
Base.getindex(sa::SimpleArray, idx...) = getindex(sa.els, idx...)
Base.setindex!(sa::SimpleArray, v, idx...) = setindex!(sa.els, v, idx...)
Base.resize!(sa::SimpleArray, n) = resize!(sa.els, n)
Base.copy(sa::SimpleArray) = SimpleArray(copy(sa.els))

isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

@testset "Failing `$f` should not grow the array $a" for
        f in (push!, append!, pushfirst!, prepend!),
        a in (["foo", "Bar"], SimpleArray(["foo", "Bar"]), OffsetVector(["foo", "Bar"], 0:1))
    for args in ((1,), (1,2), ([1], [2]), [1])
        orig = copy(a)
        @test_throws Exception f(a, args...)
        @test a == orig
    end
end

@testset "Check push!($a, $args...)" for
    a in (["foo", "Bar"], SimpleArray(["foo", "Bar"]), SimpleArray{Any}(["foo", "Bar"]), OffsetVector(["foo", "Bar"], 0:1)),
    args in (("eenie",), ("eenie", "minie"), ("eenie", "minie", "mo"))
        orig = copy(a)
        push!(a, args...)
        @test length(a) == length(orig) + length(args)
        @test a[axes(orig,1)] == orig
        @test all(a[end-length(args)+1:end] .== args)
end

@testset "Check append!($a, $args)" for
    a in (["foo", "Bar"], SimpleArray(["foo", "Bar"]), SimpleArray{Any}(["foo", "Bar"]), OffsetVector(["foo", "Bar"], 0:1)),
    args in (("eenie",), ("eenie", "minie"), ("eenie", "minie", "mo"))
        orig = copy(a)
        append!(a, args)
        @test length(a) == length(orig) + length(args)
        @test a[axes(orig,1)] == orig
        @test all(a[end-length(args)+1:end] .== args)
end

@testset "Check sizehint!($a)" for
    a in (["foo", "Bar"], SimpleArray(["foo", "Bar"]), SimpleArray{Any}(["foo", "Bar"]), OffsetVector(["foo", "Bar"], 0:1))
        @test sizehint!(a, 10) === a
end

@testset "splatting into hvcat" begin
    t = (1, 2)
    @test [t...; 3 4] == [1 2; 3 4]
    @test [0 t...; t... 0] == [0 1 2; 1 2 0]
    @test_throws ArgumentError [t...; 3 4 5]

    @test Int[t...; 3 4] == [1 2; 3 4]
    @test Int[0 t...; t... 0] == [0 1 2; 1 2 0]
    @test_throws DimensionMismatch Int[t...; 3 4 5]
end

@testset "issue #39896, modified getindex " begin
    for arr = ([1:10;], reshape([1.0:16.0;],4,4), reshape(['a':'h';],2,2,2))
        for inds = (2:5, Base.OneTo(5), BigInt(3):BigInt(5), UInt(4):UInt(3),
            Base.IdentityUnitRange(Base.OneTo(4)))
            @test arr[inds] == arr[collect(inds)]
            @test arr[inds] isa AbstractVector{eltype(arr)}
        end
    end
    # Test that ranges and arrays behave identically for indices with 1-based axes
    for r in (1:10, 1:1:10, Base.OneTo(10),
        Base.IdentityUnitRange(Base.OneTo(10)), Base.IdentityUnitRange(1:10))
        for inds = (2:5, Base.OneTo(5), BigInt(3):BigInt(5), UInt(4):UInt(3),
            Base.IdentityUnitRange(Base.OneTo(4)))
            @test r[inds] == r[collect(inds)] == collect(r)[inds] == collect(r)[collect(inds)]
        end
    end
    for arr = ([1], reshape([1.0],1,1), reshape(['a'],1,1,1))
        @test arr[true:true] == [arr[1]]
        @test arr[true:true] isa AbstractVector{eltype(arr)}
        @test arr[false:false] == []
        @test arr[false:false] isa AbstractVector{eltype(arr)}
    end
    for arr = ([1:10;], reshape([1.0:16.0;],4,4), reshape(['a':'h';],2,2,2))
        @test_throws BoundsError arr[true:true]
        @test_throws BoundsError arr[false:false]
    end
end

using Base: typed_hvncat
@testset "hvncat" begin
    a = fill(1, (2,3,2,4,5))
    b = fill(2, (1,1,2,4,5))
    c = fill(3, (1,2,2,4,5))
    d = fill(4, (1,1,1,4,5))
    e = fill(5, (1,1,1,4,5))
    f = fill(6, (1,1,1,4,5))
    g = fill(7, (2,3,1,4,5))
    h = fill(8, (3,3,3,1,2))
    i = fill(9, (3,2,3,3,2))
    j = fill(10, (3,1,3,3,2))

    result = [a; b c ;;; d e f ; g ;;;;; h ;;;; i j]
    @test size(result) == (3,3,3,4,7)
    @test result == [a; [b ;; c] ;;; [d e f] ; g ;;;;; h ;;;; i ;; j]
    @test result == cat(cat([a ; b c], [d e f ; g], dims = 3), cat(h, [i j], dims = 4), dims = 5)

    # terminating semicolons extend dimensions
    @test [1;] == [1]
    @test [1;;] == fill(1, (1,1))

    for v in (1, fill(1), fill(1,1,1), fill(1, 1, 1, 1))
        @test_throws DimensionMismatch [v; v;; v]
        @test_throws DimensionMismatch [v; v;; v; v; v]
        @test_throws DimensionMismatch [v; v; v;; v; v]
        @test_throws DimensionMismatch [v; v;; v; v;;; v; v;; v; v;; v; v]
        @test_throws DimensionMismatch [v; v;; v; v;;; v; v]
        @test_throws DimensionMismatch [v; v;; v; v;;; v; v; v;; v; v]
        @test_throws DimensionMismatch [v; v;; v; v;;; v; v;; v; v; v]
        # ensure a wrong shape with the right number of elements doesn't pass through
        @test_throws DimensionMismatch [v; v;; v; v;;; v; v; v; v]

        @test [v; v;; v; v] == fill(1, ndims(v) == 3 ? (2, 2, 1) : (2,2))
        @test [v; v;; v; v;;;] == fill(1, 2, 2, 1)
        @test [v; v;; v; v] == fill(1, ndims(v) == 3 ? (2, 2, 1) : (2,2))
        @test [v v; v v;;;] == fill(1, 2, 2, 1)
        @test [v; v;; v; v;;; v; v;; v; v;;] == fill(1, 2, 2, 2)
        @test [v; v; v;; v; v; v;;; v; v; v;; v; v; v;;] == fill(1, 3, 2, 2)
        @test [v v; v v;;; v v; v v] == fill(1, 2, 2, 2)
        @test [v v v; v v v;;; v v v; v v v] == fill(1, 2, 3, 2)
    end

    # mixed scalars and arrays work, for numbers and strings
    for v = (1, "test")
        @test [v v;;; fill(v, 1, 2)] == fill(v, 1, 2, 2)
    end

    # output dimensions are maximum of input dimensions and concatenation dimension
    begin
        v1 = fill(1, 1, 1)
        v2 = fill(1, 1, 1, 1, 1)
        v3 = fill(1, 1, 2, 1, 1)
        @test [v1 ;;; v2] == [1 ;;; 1 ;;;;]
        @test [v2 ;;; v1] == [1 ;;; 1 ;;;;]
        @test [v3 ;;; v1 v1] == [1 1 ;;; 1 1 ;;;;]
        @test [v1 v1 ;;; v3] == [1 1 ;;; 1 1 ;;;;]
        @test [v2 v1 ;;; v1 v1] == [1 1 ;;; 1 1 ;;;;]
        @test [v1 v1 ;;; v1 v2] == [1 1 ;;; 1 1 ;;;;]
        @test [v2 ;;; 1] == [1 ;;; 1 ;;;;]
        @test [1 ;;; v2] == [1 ;;; 1 ;;;;]
        @test [v3 ;;; 1 v1] == [1 1 ;;; 1 1 ;;;;]
        @test [v1 1 ;;; v3] == [1 1 ;;; 1 1 ;;;;]
        @test [v2 1 ;;; v1 v1] == [1 1 ;;; 1 1 ;;;;]
        @test [v1 1 ;;; v1 v2] == [1 1 ;;; 1 1 ;;;;]
    end

    # dims form
    for v ∈ ((), (1,), ([1],), (1, [1]), ([1], 1), ([1], [1]))
        # reject dimension < 0
        @test_throws ArgumentError hvncat(-1, v...)

        # reject shape tuple with no elements
        @test_throws ArgumentError hvncat(((),), true, v...)
    end

    # reject dims or shape with negative or zero values
    for v1 ∈ (-1, 0, 1)
        for v2 ∈ (-1, 0, 1)
            v1 == v2 == 1 && continue
            for v3 ∈ ((), (1,), ([1],), (1, [1]), ([1], 1), ([1], [1]))
                @test_throws ArgumentError hvncat((v1, v2), true, v3...)
                @test_throws str->(occursin("`shape` argument must consist of positive integers", str) ||
                                   occursin("reducing over an empty collection is not allowed", str)) hvncat(((v1,), (v2,)), true, v3...)
            end
        end
    end

    for v ∈ ((1, [1]), ([1], 1), ([1], [1]))
        # reject shape with more than one end value
        @test_throws ArgumentError hvncat(((1, 1),), true, v...)
    end

    for v ∈ ((1, 2, 3), (1, 2, [3]), ([1], [2], [3]))
        # reject shape with more values in later level
        @test_throws ArgumentError hvncat(((2, 1), (1, 1, 1)), true, v...)
    end

    # reject shapes that don't nest evenly between levels (e.g. 1 + 2 does not fit into 2)
    @test_throws DimensionMismatch hvncat(((1, 2, 1), (2, 2), (4,)), true, [1 2], [3], [4], [1 2; 3 4])

    # zero-length arrays are handled appropriately
    @test [zeros(Int, 1, 2, 0) ;;; 1 3] == [1 3;;;]
    @test [[] ;;; [] ;;; []] == Array{Any}(undef, 0, 1, 3)
    @test [[] ; 1 ;;; 2 ; []] == [1 ;;; 2]
    @test [[] ; [] ;;; [] ; []] == Array{Any}(undef, 0, 1, 2)
    @test [[] ; 1 ;;; 2] == [1 ;;; 2]
    @test [[] ; [] ;;; [] ;;; []] == Array{Any}(undef, 0, 1, 3)
    z = zeros(Int, 0, 0, 0)
    [z z ; z ;;; z ;;; z] == Array{Int}(undef, 0, 0, 0)

    for v1 ∈ (zeros(Int, 0, 0), zeros(Int, 0, 0, 0, 0), zeros(Int, 0, 0, 0, 0, 0, 0, 0))
        for v2 ∈ (1, [1])
            for v3 ∈ (2, [2])
                @test_throws DimensionMismatch [v1 ;;; v2]
                @test_throws DimensionMismatch [v1 ;;; v2 v3]
                @test_throws DimensionMismatch [v1 v1 ;;; v2 v3]
            end
        end
    end
    v1 = zeros(Int, 0, 0, 0)
    for v2 ∈ (1, [1])
        for v3 ∈ (2, [2])
            @test_throws DimensionMismatch [v1 ;;; v2 v3]
            @test_throws DimensionMismatch [v1 ;;; v2]
            @test_throws DimensionMismatch [v1 v1 ;;; v2 v3]
        end
    end

    # 0-dimension behaviors
    # exactly one argument, placed in an array
    # if already an array, copy, with type conversion as necessary
    @test_throws ArgumentError hvncat(0)
    @test hvncat(0, 1) == fill(1)
    @test hvncat(0, [1]) == [1]
    @test_throws ArgumentError hvncat(0, 1, 1)
    @test_throws ArgumentError typed_hvncat(Float64, 0)
    @test typed_hvncat(Float64, 0, 1) == fill(1.0)
    @test typed_hvncat(Float64, 0, [1]) == Float64[1.0]
    @test_throws ArgumentError typed_hvncat(Float64, 0, 1, 1)
    @test_throws ArgumentError hvncat((), true) == []
    @test hvncat((), true, 1) == fill(1)
    @test hvncat((), true, [1]) == [1]
    @test_throws ArgumentError hvncat((), true, 1, 1)
    @test_throws ArgumentError typed_hvncat(Float64, (), true) == Float64[]
    @test typed_hvncat(Float64, (), true, 1) == fill(1.0)
    @test typed_hvncat(Float64, (), true, [1]) == [1.0]
    @test_throws ArgumentError typed_hvncat(Float64, (), true, 1, 1)

    # 1-dimension behaviors
    # int form
    @test hvncat(1) == []
    @test hvncat(1, 1) == [1]
    @test hvncat(1, [1]) == [1]
    @test hvncat(1, [1 2; 3 4]) == [1 2; 3 4]
    @test hvncat(1, 1, 1) == [1 ; 1]
    @test typed_hvncat(Float64, 1) == Float64[]
    @test typed_hvncat(Float64, 1, 1) == Float64[1.0]
    @test typed_hvncat(Float64, 1, [1]) == Float64[1.0]
    @test typed_hvncat(Float64, 1, 1, 1) == Float64[1.0 ; 1.0]
    # dims form
    @test_throws ArgumentError hvncat((1,), true)
    @test hvncat((2,), true, 1, 1) == [1; 1]
    @test hvncat((2,), true, [1], [1]) == [1; 1]
    @test_throws ArgumentError hvncat((2,), true, 1)
    @test typed_hvncat(Float64, (2,), true, 1, 1) == Float64[1.0; 1.0]
    @test typed_hvncat(Float64, (2,), true, [1], [1]) == Float64[1.0; 1.0]
    @test_throws ArgumentError typed_hvncat(Float64, (2,), true, 1)
    # row_first has no effect with just one dimension of the dims form
    @test hvncat((2,), false, 1, 1) == [1; 1]
    @test typed_hvncat(Float64, (2,), false, 1, 1) == Float64[1.0; 1.0]
    # shape form
    @test hvncat(((2,),), true, 1, 1) == [1 1]
    @test hvncat(((2,),), true, [1], [1]) == [1 1]
    @test_throws ArgumentError hvncat(((2,),), true, 1)
    @test hvncat(((2,),), false, 1, 1) == [1; 1]
    @test hvncat(((2,),), false, [1], [1]) == [1; 1]
    @test typed_hvncat(Float64, ((2,),), true, 1, 1) == Float64[1.0 1.0]
    @test typed_hvncat(Float64, ((2,),), true, [1], [1]) == Float64[1.0 1.0]
    @test_throws ArgumentError typed_hvncat(Float64, ((2,),), true, 1)
    @test typed_hvncat(Float64, ((2,),), false, 1, 1) == Float64[1.0; 1.0]
    @test typed_hvncat(Float64, ((2,),), false, [1], [1]) == Float64[1.0; 1.0]

    # zero-value behaviors for int form above dimension zero
    # e.g. [;;], [;;;], though that isn't valid syntax
    @test [] == hvncat(1) isa Array{Any, 1}
    @test Array{Any, 2}(undef, 0, 0) == hvncat(2) isa Array{Any, 2}
    @test Array{Any, 3}(undef, 0, 0, 0) == hvncat(3) isa Array{Any, 3}
    @test Int[] == typed_hvncat(Int, 1) isa Array{Int, 1}
    @test Array{Int, 2}(undef, 0, 0) == typed_hvncat(Int, 2) isa Array{Int, 2}
    @test Array{Int, 3}(undef, 0, 0, 0) == typed_hvncat(Int, 3) isa Array{Int, 3}

    # Issue 43933 - semicolon precedence mistake should produce an error
    @test_throws DimensionMismatch [[1 1]; 2 ;; 3 ; [3 4]]
    @test_throws DimensionMismatch [[1 ;;; 1]; 2 ;;; 3 ; [3 ;;; 4]]

    @test [[1 2; 3 4] [5; 6]; [7 8] 9;;;] == [1 2 5; 3 4 6; 7 8 9;;;]

    #45461, #46133 - ensure non-numeric types do not error
    @test [1;;; 2;;; nothing;;; 4] == reshape([1; 2; nothing; 4], (1, 1, 4))
    @test [1 2;;; nothing 4] == reshape([1; 2; nothing; 4], (1, 2, 2))
    @test [[1 2];;; nothing 4] == reshape([1; 2; nothing; 4], (1, 2, 2))
    @test ["A";;"B";;"C";;"D"] == ["A" "B" "C" "D"]
    @test ["A";"B";;"C";"D"] == ["A" "C"; "B" "D"]
    @test [["A";"B"];;"C";"D"] == ["A" "C"; "B" "D"]
end

@testset "stack" begin
    # Basics
    for args in ([[1, 2]], [1:2, 3:4], [[1 2; 3 4], [5 6; 7 8]],
                AbstractVector[1:2, [3.5, 4.5]], Vector[[1,2], [3im, 4im]],
                [[1:2, 3:4], [5:6, 7:8]], [fill(1), fill(2)])
        X = stack(args)
        Y = cat(args...; dims=ndims(args[1])+1)
        @test X == Y
        @test typeof(X) === typeof(Y)

        X2 = stack(x for x in args)
        @test X2 == Y
        @test typeof(X2) === typeof(Y)

        X3 = stack(x for x in args if true)
        @test X3 == Y
        @test typeof(X3) === typeof(Y)

        if isconcretetype(eltype(args))
            @inferred stack(args)
            @inferred stack(x for x in args)
        end
    end

    # Higher dims
    @test size(stack([rand(2,3) for _ in 1:4, _ in 1:5])) == (2,3,4,5)
    @test size(stack(rand(2,3) for _ in 1:4, _ in 1:5)) == (2,3,4,5)
    @test size(stack(rand(2,3) for _ in 1:4, _ in 1:5 if true)) == (2, 3, 20)
    @test size(stack([rand(2,3) for _ in 1:4, _ in 1:5]; dims=1)) == (20, 2, 3)
    @test size(stack(rand(2,3) for _ in 1:4, _ in 1:5; dims=2)) == (2, 20, 3)

    # Tuples
    @test stack([(1,2), (3,4)]) == [1 3; 2 4]
    @test stack(((1,2), (3,4))) == [1 3; 2 4]
    @test stack(Any[(1,2), (3,4)]) == [1 3; 2 4]
    @test stack([(1,2), (3,4)]; dims=1) == [1 2; 3 4]
    @test stack(((1,2), (3,4)); dims=1) == [1 2; 3 4]
    @test stack(Any[(1,2), (3,4)]; dims=1) == [1 2; 3 4]
    @test size(@inferred stack(Iterators.product(1:3, 1:4))) == (2,3,4)
    @test @inferred(stack([('a', 'b'), ('c', 'd')])) == ['a' 'c'; 'b' 'd']
    @test @inferred(stack([(1,2+3im), (4, 5+6im)])) isa Matrix{Number}

    # stack(f, iter)
    @test @inferred(stack(x -> [x, 2x], 3:5)) == [3 4 5; 6 8 10]
    @test @inferred(stack(x -> x*x'/2, [1:2, 3:4])) == [0.5 1.0; 1.0 2.0;;; 4.5 6.0; 6.0 8.0]
    @test @inferred(stack(*, [1:2, 3:4], 5:6)) == [5 18; 10 24]

    # Iterators
    @test stack([(a=1,b=2), (a=3,b=4)]) == [1 3; 2 4]
    @test stack([(a=1,b=2), (c=3,d=4)]) == [1 3; 2 4]
    @test stack([(a=1,b=2), (c=3,d=4)]; dims=1) == [1 2; 3 4]
    @test stack([(a=1,b=2), (c=3,d=4)]; dims=2) == [1 3; 2 4]
    @test stack((x/y for x in 1:3) for y in 4:5) == (1:3) ./ (4:5)'
    @test stack((x/y for x in 1:3) for y in 4:5; dims=1) == (1:3)' ./ (4:5)

    # Exotic
    ips = ((Iterators.product([i,i^2], [2i,3i,4i], 1:4)) for i in 1:5)
    @test size(stack(ips)) == (2, 3, 4, 5)
    @test stack(ips) == cat(collect.(ips)...; dims=4)
    ips_cat2 = cat(reshape.(collect.(ips), Ref((2,1,3,4)))...; dims=2)
    @test stack(ips; dims=2) == ips_cat2
    @test stack(collect.(ips); dims=2) == ips_cat2
    ips_cat3 = cat(reshape.(collect.(ips), Ref((2,3,1,4)))...; dims=3)
    @test stack(ips; dims=3) == ips_cat3  # path for non-array accumulation on non-final dims
    @test stack(collect, ips; dims=3) == ips_cat3  # ... and for array accumulation
    @test stack(collect.(ips); dims=3) == ips_cat3

    # Trivial, because numbers are iterable:
    @test stack(abs2, 1:3) == [1, 4, 9] == collect(Iterators.flatten(abs2(x) for x in 1:3))

    # Allocation tests
    xv = [rand(10) for _ in 1:100]
    xt = Tuple.(xv)
    for dims in (1, 2, :)
        @test stack(xv; dims) == stack(xt; dims)
        @test_skip 9000 > @allocated stack(xv; dims)
        @test_skip 9000 > @allocated stack(xt; dims)
    end
    xr = (reshape(1:1000,10,10,10) for _ = 1:1000)
    for dims in (1, 2, 3, :)
        stack(xr; dims)
        @test_skip 8.1e6 > @allocated stack(xr; dims)
    end

    # Mismatched sizes
    @test_throws DimensionMismatch stack([1:2, 1:3])
    @test_throws DimensionMismatch stack([1:2, 1:3]; dims=1)
    @test_throws DimensionMismatch stack([1:2, 1:3]; dims=2)
    @test_throws DimensionMismatch stack([(1,2), (3,4,5)])
    @test_throws DimensionMismatch stack([(1,2), (3,4,5)]; dims=1)
    @test_throws DimensionMismatch stack(x for x in [1:2, 1:3])
    @test_throws DimensionMismatch stack([[5 6; 7 8], [1, 2, 3, 4]])
    @test_throws DimensionMismatch stack([[5 6; 7 8], [1, 2, 3, 4]]; dims=1)
    @test_throws DimensionMismatch stack(x for x in [[5 6; 7 8], [1, 2, 3, 4]])
    # Inner iterator of unknown length
    @test_throws MethodError stack((x for x in 1:3 if true) for _ in 1:4)
    @test_throws MethodError stack((x for x in 1:3 if true) for _ in 1:4; dims=1)

    @test_throws ArgumentError stack([1:3, 4:6]; dims=0)
    @test_throws ArgumentError stack([1:3, 4:6]; dims=3)
    @test_throws ArgumentError stack(abs2, 1:3; dims=2)

    @test stack(["hello", "world"]) isa Matrix{Char}
    @test_throws DimensionMismatch stack(["hello", "world!"])  # had a bug in error printing

    # Empty
    @test_throws ArgumentError stack(())
    @test_throws ArgumentError stack([])
    @test_throws ArgumentError stack(x for x in 1:3 if false)
end

@testset "tests from PR 31644" begin
    v_v_same = [rand(128) for ii in 1:100]
    v_v_diff = Any[rand(128), rand(Float32,128), rand(Int, 128)]
    v_v_diff_typed = Union{Vector{Float64},Vector{Float32},Vector{Int}}[rand(128), rand(Float32,128), rand(Int, 128)]
    for v_v in (v_v_same, v_v_diff, v_v_diff_typed)
        # Cover all combinations of iterator traits.
        g_v = (x for x in v_v)
        f_g_v = Iterators.filter(x->true, g_v)
        f_v_v = Iterators.filter(x->true, v_v);
        hcat_expected = hcat(v_v...)
        vcat_expected = vcat(v_v...)
        @testset "$(typeof(data))" for data in (v_v, g_v, f_g_v, f_v_v)
            @test stack(data) == hcat_expected
            @test vec(stack(data)) == vcat_expected
        end
    end
end

@testset "keepat!" begin
    a = [1:6;]
    @test a === keepat!(a, 1:5)
    @test a == 1:5
    @test keepat!(a, [2, 4]) == [2, 4]
    @test isempty(keepat!(a, []))

    a = [1:6;]
    @test_throws BoundsError keepat!(a, 1:10) # make sure this is not a no-op
    @test_throws BoundsError keepat!(a, 2:10)
    @test_throws ArgumentError keepat!(a, [2, 4, 3])

    b = BitVector([1, 1, 1, 0, 0])
    @test b === keepat!(b, 1:5)
    @test b == [1, 1, 1, 0, 0]
    @test keepat!(b, 2:4) == [1, 1, 0]
    @test_throws BoundsError keepat!(a, -1:10)
    @test_throws ArgumentError keepat!(a, [2, 1])
    @test isempty(keepat!(a, []))
end

@testset "reshape methods for AbstractVectors" begin
    for r in Any[1:3, Base.IdentityUnitRange(3:4)]
        @test reshape(r, :) === reshape(r, (:,)) === r
    end
    r = 3:5
    rr = reshape(r, 1, 3)
    @test length(rr) == length(r)
end

module IRUtils
    include(joinpath(@__DIR__,"../Compiler/test/irutils.jl"))
end

function check_pointer_strides(A::AbstractArray)
    # Make sure stride(A, i) is equivalent with strides(A)[i] (if 1 <= i <= ndims(A))
    dims = ntuple(identity, ndims(A))
    map(i -> stride(A, i), dims) == @inferred(strides(A)) || return false
    # Test pointer via value check.
    first(A) === Base.unsafe_load(pointer(A)) || return false
    # Test strides via value check.
    for i in eachindex(IndexLinear(), A)
        A[i] === Base.unsafe_load(pointer(A, i)) || return false
    end
    return true
end

@testset "colonful `reshape`, #54245" begin
    @test reshape([], (0, :)) isa Matrix
    @test_throws DimensionMismatch reshape([7], (0, :))
    let b = prevpow(2, typemax(Int))
        @test iszero(b*b)
        @test_throws ArgumentError reshape([7], (b, :, b))
        @test reshape([], (b, :, b)) isa Array{<:Any, 3}
    end
    for iterator ∈ (7:6, 7:7, 7:8)
        for it ∈ (iterator, map(BigInt, iterator))
            @test reshape(it, (:, Int(length(it)))) isa AbstractMatrix
            @test reshape(it, (Int(length(it)), :)) isa AbstractMatrix
            @test reshape(it, (1, :))               isa AbstractMatrix
            @test reshape(it, (:, 1))               isa AbstractMatrix
        end
    end
end

@testset "strides for ReshapedArray" begin
    # Type-based contiguous Check
    a = vec(reinterpret(reshape, Int16, reshape(view(reinterpret(Int32, randn(10)), 2:11), 5, :)))
    f(a) = only(strides(a));
    @test IRUtils.fully_eliminated(f, Base.typesof(a)) && f(a) == 1
    # General contiguous check
    a = view(rand(10,10), 1:10, 1:10)
    @test check_pointer_strides(vec(a))
    b = view(parent(a), 1:9, 1:10)
    @test_throws "Input is not strided." strides(vec(b))
    # StridedVector parent
    for n in 1:3
        a = view(collect(1:60n), 1:n:60n)
        @test check_pointer_strides(reshape(a, 3, 4, 5))
        @test check_pointer_strides(reshape(a, 5, 6, 2))
        b = view(parent(a), 60n:-n:1)
        @test check_pointer_strides(reshape(b, 3, 4, 5))
        @test check_pointer_strides(reshape(b, 5, 6, 2))
    end
    # StridedVector like parent
    a = randn(10, 10, 10)
    b = view(a, 1:10, 1:1, 5:5)
    @test check_pointer_strides(reshape(b, 2, 5))
    # Other StridedArray parent
    a = view(randn(10,10), 1:9, 1:10)
    @test check_pointer_strides(reshape(a,3,3,2,5))
    @test check_pointer_strides(reshape(a,3,3,5,2))
    @test check_pointer_strides(reshape(a,9,5,2))
    @test check_pointer_strides(reshape(a,3,3,10))
    @test check_pointer_strides(reshape(a,1,3,1,3,1,5,1,2))
    @test check_pointer_strides(reshape(a,3,3,5,1,1,2,1,1))
    @test_throws "Input is not strided." strides(reshape(a,3,6,5))
    @test_throws "Input is not strided." strides(reshape(a,3,2,3,5))
    @test_throws "Input is not strided." strides(reshape(a,3,5,3,2))
    @test_throws "Input is not strided." strides(reshape(a,5,3,3,2))
    # Zero dimensional parent
    struct FakeZeroDimArray <: AbstractArray{Int, 0} end
    Base.strides(::FakeZeroDimArray) = ()
    Base.size(::FakeZeroDimArray) = ()
    a = reshape(FakeZeroDimArray(),1,1,1)
    @test @inferred(strides(a)) == (1, 1, 1)
    # Dense parent (but not StridedArray)
    A = reinterpret(Int8, reinterpret(reshape, Int16, rand(Int8, 2, 3, 3)))
    @test check_pointer_strides(reshape(A, 3, 2, 3))
end

@testset "pointer for SubArray with none-dense parent." begin
    a = view(Matrix(reshape(0x01:0xc8, 20, :)), 1:2:20, :)
    b = reshape(a, 20, :)
    @test check_pointer_strides(view(b, 2:11, 1:5))
    @test check_pointer_strides(view(b, reshape(2:11, 2, :), 1:5))
end

@testset "stride for 0 dims array #44087" begin
    struct Fill44087 <: AbstractArray{Int,0}
        a::Int
    end
    # `stride` shouldn't work if `strides` is not defined.
    @test_throws MethodError stride(Fill44087(1), 1)
    # It is intentionally to only check the return type. (The value is somehow arbitrary)
    @test stride(fill(1), 1) isa Int
    @test stride(reinterpret(Float64, fill(Int64(1))), 1) isa Int
    @test stride(reinterpret(reshape, Float64, fill(Int64(1))), 1) isa Int
    @test stride(Base.ReshapedArray(fill(1), (), ()), 1) isa Int
end

@testset "to_indices inference (issue #42001 #44059)" begin
    CIdx = CartesianIndex
    CIdc = CartesianIndices
    @test (@inferred to_indices([], ntuple(Returns(CIdx(1)), 32))) == ntuple(Returns(1), 32)
    @test (@inferred to_indices([], ntuple(Returns(CIdc(1:1)), 32))) == ntuple(Returns(Base.OneTo(1)), 32)
    @test (@inferred to_indices([], (CIdx(), 1, CIdx(1,1,1)))) == ntuple(Returns(1), 4)
    A = randn(2, 2, 2, 2, 2, 2);
    i = CIdx((1, 1))
    @test (@inferred A[i,i,i]) === A[1]
    @test (@inferred to_indices([], (1, CIdx(1, 1), 1, CIdx(1, 1), 1, CIdx(1, 1), 1))) == ntuple(Returns(1), 10)
end

@testset "type-based offset axes check" begin
    a = randn(ComplexF64, 10)
    b = randn(ComplexF64, 4, 4, 4, 4)
    ta = reinterpret(Float64, a)
    tb = reinterpret(Float64, view(a, 1:2:10))
    tc = reinterpret(Float64, reshape(view(a, 1:3:10), 2, 2, 1))
    td = view(b, :, :, 1, 1)
    # Issue #44040
    @test IRUtils.fully_eliminated(Base.require_one_based_indexing, Base.typesof(ta, tc))
    @test IRUtils.fully_eliminated(Base.require_one_based_indexing, Base.typesof(tc, tc))
    @test IRUtils.fully_eliminated(Base.require_one_based_indexing, Base.typesof(ta, tc, tb))
    # Issue #49332
    @test IRUtils.fully_eliminated(Base.require_one_based_indexing, Base.typesof(td, td, td))
    # Ranges && CartesianIndices
    @test IRUtils.fully_eliminated(Base.require_one_based_indexing, Base.typesof(1:10, Base.OneTo(10), 1.0:2.0, LinRange(1.0, 2.0, 2), 1:2:10, CartesianIndices((1:2:10, 1:2:10))))
    # Remind us to call `any` in `Base.has_offset_axes` once our compiler is ready.
    @inline _has_offset_axes(A) = @inline any(x -> Int(first(x))::Int != 1, axes(A))
    @inline _has_offset_axes(As...) = @inline any(_has_offset_axes, As)
    a, b = zeros(2, 2, 2), zeros(2, 2)
    @test_broken IRUtils.fully_eliminated(_has_offset_axes, Base.typesof(a, a, b, b))
end

# type stable [x;;] (https://github.com/JuliaLang/julia/issues/45952)
f45952(x) = [x;;]
@inferred f45952(1.0)

@testset "isassigned with a Bool index" begin
    A = zeros(2,2)
    @test_throws "invalid index: true of type Bool" isassigned(A, 1, true)
    @test_throws "invalid index: true of type Bool" isassigned(A, true)
end

@testset "repeat for FillArrays" begin
    f = FillArrays.Fill(3, (4,))
    @test repeat(f, 2) === FillArrays.Fill(3, (8,))
    @test repeat(f, 2, 3) === FillArrays.Fill(3, (8, 3))
    @test repeat(f, inner=(1,2), outer=(3,1)) === repeat(f, 3, 2) === FillArrays.Fill(3, (12,2))
    f = FillArrays.Fill(3, (4, 2))
    @test repeat(f, 2, 3) === FillArrays.Fill(3, (8, 6))
    @test repeat(f, 2, 3, 4) === FillArrays.Fill(3, (8, 6, 4))
    @test repeat(f, inner=(1,2), outer=(3,1)) === FillArrays.Fill(3, (12, 4))
end

@testset "zero" begin
    @test zero([1 2; 3 4]) isa Matrix{Int}
    @test zero([1 2; 3 4]) == [0 0; 0 0]

    @test zero([1.0]) isa Vector{Float64}
    @test zero([1.0]) == [0.0]

    @test zero([[2,2], [3,3,3]]) isa Vector{Vector{Int}}
    @test zero([[2,2], [3,3,3]]) == [[0,0], [0, 0, 0]]


    @test zero(Union{Float64, Missing}[missing]) == [0.0]
    struct CustomNumber <: Number
        val::Float64
    end
    Base.zero(::Type{CustomNumber}) = CustomNumber(0.0)
    @test zero([CustomNumber(5.0)]) == [CustomNumber(0.0)]
    @test zero(Union{CustomNumber, Missing}[missing]) == [CustomNumber(0.0)]
    @test zero(Vector{Union{CustomNumber, Missing}}(undef, 1)) == [CustomNumber(0.0)]
end

@testset "`_prechecked_iterate` optimization" begin
    function test_prechecked_iterate(iter)
        Js = Base._prechecked_iterate(iter)
        for I in iter
            J, s = Js::NTuple{2,Any}
            @test J === I
            Js = Base._prechecked_iterate(iter, s)
        end
    end
    test_prechecked_iterate(1:10)
    test_prechecked_iterate(Base.OneTo(10))
    test_prechecked_iterate(CartesianIndices((3, 3)))
    test_prechecked_iterate(CartesianIndices(()))
    test_prechecked_iterate(LinearIndices((3, 3)))
    test_prechecked_iterate(LinearIndices(()))
    test_prechecked_iterate(Base.SCartesianIndices2{3}(1:3))
end

@testset "IndexStyles in copyto!" begin
    A = rand(3,2)
    B = zeros(size(A))
    colons = ntuple(_->:, ndims(B))
    # Ensure that the AbstractArray methods are hit
    # by using views instead of Arrays
    @testset "IndexLinear - IndexLinear" begin
        B .= 0
        copyto!(view(B, colons...), A)
        @test B == A
    end
    @testset "IndexLinear - IndexCartesian" begin
        B .= 0
        copyto!(view(B, colons...), view(A, axes(A)...))
        @test B == A
    end
    @testset "IndexCartesian - IndexLinear" begin
        B .= 0
        copyto!(view(B, axes(B)...), A)
        @test B == A
    end
    @testset "IndexCartesian - IndexCartesian" begin
        B .= 0
        copyto!(view(B, axes(B)...), view(A, axes(A)...))
        @test B == A
    end
end

@testset "reshape for offset arrays" begin
    p = Base.IdentityUnitRange(3:4)
    r = reshape(p, :, 1)
    @test r[eachindex(r)] == UnitRange(p)
    @test collect(r) == r

    struct ZeroBasedArray{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
        a :: A
        function ZeroBasedArray(a::AbstractArray)
            Base.require_one_based_indexing(a)
            new{eltype(a), ndims(a), typeof(a)}(a)
        end
    end
    Base.parent(z::ZeroBasedArray) = z.a
    Base.size(z::ZeroBasedArray) = size(parent(z))
    Base.axes(z::ZeroBasedArray) = map(x -> Base.IdentityUnitRange(0:x - 1), size(parent(z)))
    Base.getindex(z::ZeroBasedArray{<:Any, N}, i::Vararg{Int,N}) where {N} = parent(z)[map(x -> x + 1, i)...]
    Base.setindex!(z::ZeroBasedArray{<:Any, N}, val, i::Vararg{Int,N}) where {N} = parent(z)[map(x -> x + 1, i)...] = val

    z = ZeroBasedArray(collect(1:4))
    r2 = reshape(z, :, 1)
    @test r2[CartesianIndices(r2)] == r2[LinearIndices(r2)]
    r2[firstindex(r2)] = 34
    @test z[0] == 34
    r2[eachindex(r2)] = r2 .* 2
    for (i, j) in zip(eachindex(r2), eachindex(z))
        @test r2[i] == z[j]
    end
end

@testset "zero for arbitrary axes" begin
    r = SizedArrays.SOneTo(2)
    s = Base.OneTo(2)
    _to_oneto(x::Integer) = Base.OneTo(2)
    _to_oneto(x::Union{Base.OneTo, SizedArrays.SOneTo}) = x
    for (f, v) in ((zeros, 0), (ones, 1), ((x...)->fill(3,x...),3))
        for ax in ((r,r), (s, r), (2, r))
            A = f(ax...)
            @test axes(A) == map(_to_oneto, ax)
            if all(x -> x isa SizedArrays.SOneTo, ax)
                @test A isa SizedArrays.SizedArray && parent(A) isa Array
            else
                @test A isa Array
            end
            @test all(==(v), A)
        end
    end
end

@testset "one" begin
    @test one([1 2; 3 4]) == [1 0; 0 1]
    @test one([1 2; 3 4]) isa Matrix{Int}

    struct Mat <: AbstractMatrix{Int}
        p::Matrix{Int}
    end
    Base.size(m::Mat) = size(m.p)
    Base.IndexStyle(::Type{<:Mat}) = IndexLinear()
    Base.getindex(m::Mat, i::Int) = m.p[i]
    Base.setindex!(m::Mat, v, i::Int) = m.p[i] = v
    Base.similar(::Mat, ::Type{Int}, size::NTuple{2,Int}) = Mat(Matrix{Int}(undef, size))

    @test one(Mat([1 2; 3 4])) == Mat([1 0; 0 1])
    @test one(Mat([1 2; 3 4])) isa Mat
end

@testset "copyto! with non-AbstractArray src" begin
    A = zeros(4)
    x = (i for i in axes(A,1))
    copyto!(A, 1, x, 1, length(A))
    @test A == axes(A,1)
    A .= 0
    copyto!(A, 1, x, 1, 2)
    @test A[1:2] == first(x,2)
    @test iszero(A[3:end])
    A .= 0
    copyto!(A, 1, x, 1)
    @test A == axes(A,1)
end

@testset "reshape with Integer sizes" begin
    @test reshape(1:4, big(2), big(2)) == reshape(1:4, 2, 2)
    a = [1 2 3; 4 5 6]
    reshaped_arrays = (
        reshape(a, 3, 2),
        reshape(a, (3, 2)),
        reshape(a, big(3), big(2)),
        reshape(a, (big(3), big(2))),
        reshape(a, :, big(2)),
        reshape(a, (:, big(2))),
        reshape(a, big(3), :),
        reshape(a, (big(3), :)),
    )
    @test allequal(reshaped_arrays)
    for b ∈ reshaped_arrays
        @test b isa Matrix{Int}
        @test b.ref === a.ref
    end
end
@testset "AbstractArrayMath" begin
    @testset "IsReal" begin
        A = [1, 2, 3, 4]
        @test isreal(A) == true
        B = [1.1, 2.2, 3.3, 4.4]
        @test isreal(B) == true
        C = [1, 2.2, 3]
        @test isreal(C) == true
        D = Real[]
        @test isreal(D) == true
        E = [1 + 1im, 2 - 2im]
        @test isreal(E) == false
        struct MyReal <: Real
            value::Float64
        end
        F = [MyReal(1.0), MyReal(2.0)]
        @test isreal(F) == true
        G = ["a", "b", "c"]
        @test_throws MethodError isreal(G)
    end
end
