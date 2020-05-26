# This file is a part of Julia. License is MIT: https://julialang.org/license

adjoint(a::AbstractArray) = error("adjoint not defined for $(typeof(a)). Consider using `permutedims` for higher-dimensional arrays.")
transpose(a::AbstractArray) = error("transpose not defined for $(typeof(a)). Consider using `permutedims` for higher-dimensional arrays.")

## Matrix transposition ##

"""
    transpose!(dest,src)

Transpose array `src` and store the result in the preallocated array `dest`, which should
have a size corresponding to `(size(src,2),size(src,1))`. No in-place transposition is
supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.

# Examples
```jldoctest
julia> A = [3+2im 9+2im; 8+7im  4+6im]
2×2 Matrix{Complex{Int64}}:
 3+2im  9+2im
 8+7im  4+6im

julia> B = zeros(Complex{Int64}, 2, 2)
2×2 Matrix{Complex{Int64}}:
 0+0im  0+0im
 0+0im  0+0im

julia> transpose!(B, A);

julia> B
2×2 Matrix{Complex{Int64}}:
 3+2im  8+7im
 9+2im  4+6im

julia> A
2×2 Matrix{Complex{Int64}}:
 3+2im  9+2im
 8+7im  4+6im
```
"""
transpose!(B::AbstractMatrix, A::AbstractMatrix) = transpose_f!(transpose, B, A)

"""
    adjoint!(dest,src)

Conjugate transpose array `src` and store the result in the preallocated array `dest`, which
should have a size corresponding to `(size(src,2),size(src,1))`. No in-place transposition
is supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.

# Examples
```jldoctest
julia> A = [3+2im 9+2im; 8+7im  4+6im]
2×2 Matrix{Complex{Int64}}:
 3+2im  9+2im
 8+7im  4+6im

julia> B = zeros(Complex{Int64}, 2, 2)
2×2 Matrix{Complex{Int64}}:
 0+0im  0+0im
 0+0im  0+0im

julia> adjoint!(B, A);

julia> B
2×2 Matrix{Complex{Int64}}:
 3-2im  8-7im
 9-2im  4-6im

julia> A
2×2 Matrix{Complex{Int64}}:
 3+2im  9+2im
 8+7im  4+6im
```
"""
adjoint!(B::AbstractMatrix, A::AbstractMatrix) = transpose_f!(adjoint, B, A)
function transpose!(B::AbstractVector, A::AbstractMatrix)
    axes(B,1) == axes(A,2) && axes(A,1) == 1:1 || throw(DimensionMismatch("transpose"))
    copyto!(B, A)
end
function transpose!(B::AbstractMatrix, A::AbstractVector)
    axes(B,2) == axes(A,1) && axes(B,1) == 1:1 || throw(DimensionMismatch("transpose"))
    copyto!(B, A)
end
function adjoint!(B::AbstractVector, A::AbstractMatrix)
    axes(B,1) == axes(A,2) && axes(A,1) == 1:1 || throw(DimensionMismatch("transpose"))
    ccopy!(B, A)
end
function adjoint!(B::AbstractMatrix, A::AbstractVector)
    axes(B,2) == axes(A,1) && axes(B,1) == 1:1 || throw(DimensionMismatch("transpose"))
    ccopy!(B, A)
end

const transposebaselength=64
function transpose_f!(f, B::AbstractMatrix, A::AbstractMatrix)
    inds = axes(A)
    axes(B,1) == inds[2] && axes(B,2) == inds[1] || throw(DimensionMismatch(string(f)))

    m, n = length(inds[1]), length(inds[2])
    if m*n<=4*transposebaselength
        @inbounds begin
            for j = inds[2]
                for i = inds[1]
                    B[j,i] = f(A[i,j])
                end
            end
        end
    else
        transposeblock!(f,B,A,m,n,first(inds[1])-1,first(inds[2])-1)
    end
    return B
end
function transposeblock!(f, B::AbstractMatrix, A::AbstractMatrix, m::Int, n::Int, offseti::Int, offsetj::Int)
    if m*n<=transposebaselength
        @inbounds begin
            for j = offsetj .+ (1:n)
                for i = offseti .+ (1:m)
                    B[j,i] = f(A[i,j])
                end
            end
        end
    elseif m>n
        newm=m>>1
        transposeblock!(f,B,A,newm,n,offseti,offsetj)
        transposeblock!(f,B,A,m-newm,n,offseti+newm,offsetj)
    else
        newn=n>>1
        transposeblock!(f,B,A,m,newn,offseti,offsetj)
        transposeblock!(f,B,A,m,n-newn,offseti,offsetj+newn)
    end
    return B
end

function ccopy!(B, A)
    RB, RA = eachindex(B), eachindex(A)
    if RB == RA
        for i = RB
            B[i] = adjoint(A[i])
        end
    else
        for (i,j) = zip(RB, RA)
            B[i] = adjoint(A[j])
        end
    end
    return B
end

"""
    copy(A::Transpose)
    copy(A::Adjoint)

Eagerly evaluate the lazy matrix transpose/adjoint.
Note that the transposition is applied recursively to elements.

This operation is intended for linear algebra usage - for general data manipulation see
[`permutedims`](@ref Base.permutedims), which is non-recursive.

# Examples
```jldoctest
julia> A = [1 2im; -3im 4]
2×2 Matrix{Complex{Int64}}:
 1+0im  0+2im
 0-3im  4+0im

julia> T = transpose(A)
2×2 Transpose{Complex{Int64},Matrix{Complex{Int64}}}:
 1+0im  0-3im
 0+2im  4+0im

julia> copy(T)
2×2 Matrix{Complex{Int64}}:
 1+0im  0-3im
 0+2im  4+0im
```
"""
copy(::Union{Transpose,Adjoint})

Base.copy(A::Transpose{<:Any,<:AbstractMatrix}) = transpose!(similar(A.parent, reverse(axes(A.parent))), A.parent)
Base.copy(A::Adjoint{<:Any,<:AbstractMatrix}) = adjoint!(similar(A.parent, reverse(axes(A.parent))), A.parent)

function copy_transpose!(B::AbstractVecOrMat, ir_dest::AbstractRange{Int}, jr_dest::AbstractRange{Int},
                         A::AbstractVecOrMat, ir_src::AbstractRange{Int}, jr_src::AbstractRange{Int})
    if length(ir_dest) != length(jr_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(jr_src)," and ",length(ir_dest),")")))
    end
    if length(jr_dest) != length(ir_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(ir_src)," and ",length(jr_dest),")")))
    end
    @boundscheck checkbounds(B, ir_dest, jr_dest)
    @boundscheck checkbounds(A, ir_src, jr_src)
    idest = first(ir_dest)
    for jsrc in jr_src
        jdest = first(jr_dest)
        for isrc in ir_src
            B[idest,jdest] = A[isrc,jsrc]
            jdest += step(jr_dest)
        end
        idest += step(ir_dest)
    end
    return B
end
