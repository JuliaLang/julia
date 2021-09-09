# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestStructuredDisplay

using Test
using LinearAlgebra

struct MyArrayWrapper{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
    a :: A
end
Base.parent(M::MyArrayWrapper) = M.a
for f in [:size, :length, :axes]
    @eval Base.$f(M::MyArrayWrapper) = $f(parent(M))
end
Base.getindex(M::MyArrayWrapper, i::Int...) = parent(M)[i...]
Base.replace_in_print_matrix(M::MyArrayWrapper, i::Integer, j::Integer, s::AbstractString) =
    Base.replace_in_print_matrix(parent(M), i, j, s)
Base.nonzeroindex(M::MyArrayWrapper{<:Any,N}, i::Vararg{Int,N}) where {N} = Base.nonzeroindex(parent(M), i...)

function print_array_str(io::IO, a)
    Base.print_array(io, a)
    String(take!(io))
end
function test_print_array(A)
    io = IOBuffer()
    for B in Any[A, adjoint(A), transpose(A)]
        M = MyArrayWrapper(B)
        for f in [identity, adjoint, transpose]
            s1 = print_array_str(io, f(B))
            s2 = print_array_str(io, f(M))
            @test s1 == s2
        end
    end
end

struct Zeros{T,N} <: AbstractArray{T,N}
    s :: NTuple{N,Int}
end
Zeros(n::Vararg{Int,N}) where {N} = Zeros(Float64, n)
Zeros(n::NTuple{N,Int}) where {N} = Zeros(Float64, n)
Zeros(::Type{T}, n::Vararg{Int,N}) where {T,N} = Zeros{T,N}(n)
Zeros(::Type{T}, n::NTuple{N,Int}) where {T,N} = Zeros{T,N}(n)
Base.size(z::Zeros) = z.s
function Base.getindex(z::Zeros{T,N}, i::Vararg{Int,N}) where {T,N}
    @boundscheck checkbounds(z, i...)
    zero(T)
end
function Base.replace_in_print_matrix(::Zeros, ::Integer, ::Integer, s::AbstractString)
    Base.replace_with_centered_mark(s)
end
Base.nonzeroindex(z::Zeros{<:Any,N}, i::Vararg{Int,N}) where {N} = false

@testset "Display of structured matrices" begin
    @testset "Diagonal" begin
        for r in Any[3:2, 3:3, 3:6, LinRange(0, 1, 7), LinRange(0, 1, 7)*im]
            test_print_array(Diagonal(r))
        end
    end
    @testset "BiDiagonal" begin
        for r in Any[1:4, LinRange(0, 1, 7), LinRange(0, 1, 7)*im], uplo in [:U, :L]
            test_print_array(Bidiagonal(r, r[2:end], uplo))
        end
    end
    @testset "TriDiagonal" begin
        test_print_array(Tridiagonal(1:3, 1:4, 1:3))
        @testset "SymTridiagonal" begin
            test_print_array(SymTridiagonal(1:4, 1:3))
        end
    end
    @testset "Triangular" begin
        for A in Any[reshape(1:1, 1, 1), reshape(1:16, 4, 4), reshape(LinRange(0, 3, 16), 4, 4)]
            @testset "UpperTriangular" begin
                test_print_array(UpperTriangular(A))
            end
            @testset "LowerTriangular" begin
                test_print_array(LowerTriangular(A))
            end
        end
    end
    @testset "Hessenberg" begin
        for A in Any[reshape(1:1, 1, 1), reshape(1:16, 4, 4), reshape(LinRange(0, 3, 16), 4, 4)]
            test_print_array(UpperHessenberg(A))
        end
    end
    @testset "Zeros" begin
        for T in [Int, Float64], s in Any[(2,), (2,2)]
            test_print_array(Zeros(T, s))
        end
    end
end

end
