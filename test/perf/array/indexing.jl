# Performance testing
function sumelt(A, n)
    s = zero(eltype(A))
    @inbounds for k = 1:n
        for a in A
            s += a
        end
    end
    s
end

function sumeach(A, n)
    s = zero(eltype(A))
    for k = 1:n
        @simd for I in eachindex(A)
            @inbounds val = A[I]
            s += val
        end
    end
    s
end

function sumfast(A, n)
    s = zero(eltype(A))
    for k = 1:n
        @simd for I in fastindex(A)
            @inbounds val = A[I]
            s += val
        end
    end
    s
end

@inline fastindex(A::AbstractArray) = fastindex((A, Base.linearindexing(A)))
@inline fastindex(AL::(AbstractArray, Base.LinearFast)) = 1:length(AL[1])
@inline fastindex(AL::(AbstractArray, Base.LinearSlow)) = eachindex(AL[1])


abstract MyArray{T,N} <: AbstractArray{T,N}

immutable ArrayLS{T,N} <: MyArray{T,N}  # LinearSlow
    data::Array{T,N}
end
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
Base.getindex(A::MyArray, indx::Real) = getindex(A.data, indx)
Base.getindex(A::MyArray, i::Real, j::Real) = getindex(A.data, i, j)

Base.getindex{T}(A::ArrayStrides{T,2}, i::Real, j::Real) = getindex(A.data, 1+A.strides[1]*(i-1)+A.strides[2]*(j-1))
Base.getindex(A::ArrayStrides1, i::Real, j::Real) = getindex(A.data, i + A.stride1*(j-1))

Base.linearindexing(A::ArrayLF) = Base.LinearFast()

function makearrays{T}(::Type{T}, sz)
    L = prod(sz)
    A = reshape(convert(Vector{T}, [1:L;]), sz)
    AS = ArrayLS(A)
    AF = ArrayLF(A)
    Astrd = ArrayStrides(A)
    Astrd1 = ArrayStrides1(A)
    outersz = (sz[1]+1,sz[2]+2)
    B = reshape(convert(Vector{T}, [1:prod(outersz);]), outersz)
    Asub = sub(B, 1:sz[1], 2:sz[2]+1)
    RS = reshape(ArrayLS([1:L;]), sz)
    RF = reshape(ArrayLF([1:L;]), sz)
    RLS = reshape(ArrayLS(A), (L,))
    RLF = reshape(ArrayLF(A), (L,))
    (A, AS, AF, Astrd, Astrd1, Asub, RS, RF, RLS, RLF)
end

