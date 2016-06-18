# This file is a part of Julia. License is MIT: http://julialang.org/license

# Performance testing

import Base: unsafe_getindex
# @inline unsafe_getindex(xs...) = Base.getindex(xs...)

function sumelt(A, n)
    s = zero(eltype(A)) + zero(eltype(A))
    for k = 1:n
        for a in A
            s += a
        end
    end
    s
end

function sumeach(A, n)
    s = zero(eltype(A)) + zero(eltype(A))
    for k = 1:n
        for I in eachindex(A)
            val = unsafe_getindex(A, I)
            s += val
        end
    end
    s
end

function sumlinear(A, n)
    s = zero(eltype(A)) + zero(eltype(A))
    for k = 1:n
        for I in 1:length(A)
            val = unsafe_getindex(A, I)
            s += val
        end
    end
    s
end
function sumcartesian(A, n)
    s = zero(eltype(A)) + zero(eltype(A))
    for k = 1:n
        for I in CartesianRange(size(A))
            val = unsafe_getindex(A, I)
            s += val
        end
    end
    s
end

function sumcolon(A, n)
    s = zero(eltype(A)) + zero(eltype(A))
    nrows = size(A, 1)
    ncols = size(A, 2)
    c = Colon()
    for k = 1:n
        @simd for i = 1:ncols
            val = unsafe_getindex(A, c, i)
            s += first(val)
        end
    end
    s
end

function sumrange(A, n)
    s = zero(eltype(A)) + zero(eltype(A))
    nrows = size(A, 1)
    ncols = size(A, 2)
    r = 1:nrows
    for k = 1:n
        @simd for i = 1:ncols
            val = unsafe_getindex(A, r, i)
            s += first(val)
        end
    end
    s
end

function sumlogical(A, n)
    s = zero(eltype(A)) + zero(eltype(A))
    nrows = size(A, 1)
    ncols = size(A, 2)
    r = falses(nrows)
    r[1:4:end] = true
    for k = 1:n
        @simd for i = 1:ncols
            val = unsafe_getindex(A, r, i)
            s += first(val)
        end
    end
    s
end

function sumvector(A, n)
    s = zero(eltype(A)) + zero(eltype(A))
    nrows = size(A, 1)
    ncols = size(A, 2)
    r = rand(1:nrows, 5)
    for k = 1:n
        @simd for i = 1:ncols
            val = unsafe_getindex(A, r, i)
            s += first(val)
        end
    end
    s
end

abstract MyArray{T,N} <: AbstractArray{T,N}

immutable ArrayLS{T,N} <: MyArray{T,N}  # LinearSlow
    data::Array{T,N}
end
immutable ArrayLSLS{T,N} <: MyArray{T,N}  # LinearSlow with LinearSlow similar
    data::Array{T,N}
end
Base.similar{T}(A::ArrayLSLS, ::Type{T}, dims::Tuple{Vararg{Int}}) = ArrayLSLS(similar(A.data, T, dims))
@inline Base.setindex!(A::ArrayLSLS, v, I::Int...) = A.data[I...] = v
@inline Base.unsafe_setindex!(A::ArrayLSLS, v, I::Int...) = Base.unsafe_setindex!(A.data, v, I...)
Base.first(A::ArrayLSLS) = first(A.data)

immutable ArrayLF{T,N} <: MyArray{T,N}  # LinearFast
    data::Array{T,N}
end
immutable ArrayStrides{T,N} <: MyArray{T,N}
    data::Array{T,N}
    strides::NTuple{N,Int}
end
ArrayStrides(A::Array) = ArrayStrides(A, strides(A))

immutable ArrayStrides1{T} <: MyArray{T,2}
    data::Matrix{T}
    stride1::Int
end
ArrayStrides1(A::Array) = ArrayStrides1(A, size(A,1))

Base.size(A::MyArray) = size(A.data)

@inline Base.getindex(A::ArrayLF, i::Int) = getindex(A.data, i)
@inline Base.getindex(A::ArrayLF, i::Int, i2::Int) = getindex(A.data, i, i2)
@inline Base.getindex(A::Union{ArrayLS, ArrayLSLS}, i::Int, j::Int) = getindex(A.data, i, j)
@inline Base.unsafe_getindex(A::ArrayLF, indx::Int) = unsafe_getindex(A.data, indx)
@inline Base.unsafe_getindex(A::Union{ArrayLS, ArrayLSLS}, i::Int, j::Int) = unsafe_getindex(A.data, i, j)

@inline Base.getindex{T}(A::ArrayStrides{T,2}, i::Real, j::Real) = getindex(A.data, 1+A.strides[1]*(i-1)+A.strides[2]*(j-1))
@inline Base.getindex(A::ArrayStrides1, i::Real, j::Real) = getindex(A.data, i + A.stride1*(j-1))
@inline Base.unsafe_getindex{T}(A::ArrayStrides{T,2}, i::Real, j::Real) = unsafe_getindex(A.data, 1+A.strides[1]*(i-1)+A.strides[2]*(j-1))
@inline Base.unsafe_getindex(A::ArrayStrides1, i::Real, j::Real) = unsafe_getindex(A.data, i + A.stride1*(j-1))

# Using the qualified Base.LinearFast() in the linearindexing definition
# requires looking up the symbol in the module on each call.
import Base: LinearFast
Base.linearindexing{T<:ArrayLF}(::Type{T}) = LinearFast()

if !applicable(unsafe_getindex, [1 2], 1:1, 2)
    @inline Base.unsafe_getindex(A::Array, I...) = @inbounds return A[I...]
    @inline Base.unsafe_getindex(A::MyArray, I...) = @inbounds return A[I...]
    @inline Base.unsafe_getindex(A::SubArray, I...) = @inbounds return A[I...]
    @inline Base.unsafe_getindex(A::BitArray, I1::BitArray, I2::Int) = unsafe_getindex(A, Base.to_index(I1), I2)
end

function makearrays{T}(::Type{T}, sz)
    L = prod(sz)
    A = reshape(convert(Vector{T}, [1:L;]), sz)
    AS = ArrayLS(A)
    ASS = ArrayLSLS(A)
    AF = ArrayLF(A)
    Astrd = ArrayStrides(A)
    Astrd1 = ArrayStrides1(A)
    outersz = (sz[1]+1,sz[2]+2)
    B = reshape(convert(Vector{T}, [1:prod(outersz);]), outersz)
    Asub = view(B, 1:sz[1], 2:sz[2]+1)
    Bit = trues(sz)
    (A, AF, AS, ASS, Asub, Bit,)
end

