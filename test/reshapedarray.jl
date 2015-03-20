module TestReshaped

using Base.Test
import Base.Reshaped

abstract MyArray{T,N} <: AbstractArray{T,N}

immutable ArrayLS{T,N} <: MyArray{T,N}  # LinearSlow
    data::Array{T,N}
end
immutable ArrayLF{T,N} <: MyArray{T,N}  # LinearFast
    data::Array{T,N}
end
Base.linearindexing(A::ArrayLF) = Base.LinearFast()

Base.size(A::MyArray) = size(A.data)
Base.getindex(A::MyArray, indx::Real) = getindex(A.data, indx)
Base.getindex(A::MyArray, i::Real, j::Real) = getindex(A.data, i, j)
Base.getindex(A::MyArray, i::Real, j::Real, k::Real) = getindex(A.data, i, j, k)

function myeq(A, B)
    size(A) == size(B) || return false
    for I in eachindex(B)
        A[I] == B[I] || return false
    end
    true
end
indextype{T,N,P,I}(::Reshaped.ReshapedArray{T,N,P,I}) = I

AA = reshape([1:15;], (3, 5))
CC = reshape([1:16;], (2,4,2))
@test isa(AA, Array)
@test isa(CC, Array)

for AT in (ArrayLF, ArrayLS)
    A = reshape(AT([1:15;]), (3, 5))
    @test indextype(A) <: (Reshaped.IndexMD{1,2},)
    @test A == AA
    @test myeq(A, AA)

    B = reshape(AT(AA), (15,))
    din = AT == ArrayLF ? 1 : 2
    @test indextype(B) <: (Reshaped.IndexMD{din,1},)
    @test B == [1:15;]
    @test myeq(B, [1:15;])

    CR = reshape(AT(CC), (4,4))
    din = AT == ArrayLF ? 1 : ndims(CC)
    @test indextype(CR) <: (Reshaped.IndexMD{din,2},)
    @test CR == reshape([1:16;], 4, 4)
    @test myeq(CR, reshape([1:16;], 4, 4))
    CR2 = reshape(AT(CC), (8,2))
    @test indextype(CR) <: (Reshaped.IndexMD{din,2},)
    @test CR2 == reshape([1:16;], 8, 2)
    @test myeq(CR2, reshape([1:16;], 8, 2))

    V = sub(CR, 2:3,2:4)
    @test V == [6 10 14; 7 11 15]
    VR = reshape(V, (3,2))
    @test indextype(VR) <: (Reshaped.IndexMD{2,2},)
    @test VR == [6 11; 7 14; 10 15]
    @test myeq(VR, [6 11; 7 14; 10 15])

    # Cases with dimension sizes of 1, some of which require FastDivInteger1
    A1 = reshape([1:15;], (1,1,15))
    R = reshape(AT(A1), (15,))
    din = AT == ArrayLF ? 1 : ndims(A1)
    @test indextype(R) <: (Reshaped.IndexMD{din,1},)
    @test R == [1:15;]
    @test myeq(R, [1:15;])
    R = reshape(AT([1:15;]), (1,1,15,))
    @test indextype(R) <: (Reshaped.IndexMD{1,3},)
    @test A1 == R
    @test myeq(A1, R)
    R = reshape(AT([1:15;]), (15,1))
    if AT == ArrayLF
        @test indextype(R) <: (Reshaped.IndexMD{1,2},)
    else
        @test indextype(R) == (Colon,Colon)
    end
    A2 = reshape([1:15;], 15, 1)
    @test R == A2
    @test myeq(R, A2)
    R = reshape(AT([1:15;]), (3,1,5))
    @test indextype(R) <: (Reshaped.IndexMD{1,3},)
    A315 = reshape([1:15;], (3,1,5))
    @test R == A315
    @test myeq(R, A315)
end

A3 = reshape([1:5*7*8;], 5, 7, 8)
S = sub(A3, 2:5, 1:3:7, 3:5)
R = reshape(S, (length(S),))
@test indextype(R) <: (Reshaped.IndexMD{3,1},)
Sc = copy(S)
@test R == Sc[:]
@test myeq(R, Sc[:])
R = reshape(S, (12,3))
@test indextype(R) <: (Reshaped.IndexMD{2,1},Colon)
@test R == reshape(Sc, (12,3))
@test myeq(R, reshape(Sc, (12,3)))
R = reshape(S, (4,9))
@test indextype(R) <: (Colon,Reshaped.IndexMD{2,1})
@test R == reshape(Sc, (4,9))
@test myeq(R, reshape(Sc, (4,9)))
R = reshape(S, (6,6))
@test indextype(R) <: (Reshaped.IndexMD{3,2},)
@test R == reshape(Sc, (6,6))
@test myeq(R, reshape(Sc, (6,6)))

end
