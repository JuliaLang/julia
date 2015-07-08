# This file is a part of Julia. License is MIT: http://julialang.org/license

## Tests for the abstract array interfaces with minimally defined array types

# A custom linear fast array type with 24 elements that doesn't rely upon Array storage
type T24Linear{T,N,dims} <: AbstractArray{T,N}
    v1::T;  v2::T;  v3::T;  v4::T;  v5::T;  v6::T;  v7::T;  v8::T
    v9::T;  v10::T; v11::T; v12::T; v13::T; v14::T; v15::T; v16::T
    v17::T; v18::T; v19::T; v20::T; v21::T; v22::T; v23::T; v24::T
    T24Linear() = (prod(dims) == 24 || throw(DimensionMismatch("T24Linear must have 24 elements")); new())
    function T24Linear(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21,v22,v23,v24)
        prod(dims) == 24 || throw(DimensionMismatch("T24Linear must have 24 elements"))
        new(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21,v22,v23,v24)
    end
end

T24Linear{T}(::Type{T}, dims::Int...) = T24Linear(T, dims)
T24Linear{T,N}(::Type{T}, dims::NTuple{N,Int}) = T24Linear{T,N,dims}()

Base.convert{T,N  }(::Type{T24Linear     }, X::AbstractArray{T,N}) = convert(T24Linear{T,N}, X)
Base.convert{T,N,_}(::Type{T24Linear{T  }}, X::AbstractArray{_,N}) = convert(T24Linear{T,N}, X)
Base.convert{T,N  }(::Type{T24Linear{T,N}}, X::AbstractArray     ) = T24Linear{T,N,size(X)}(X...)

Base.size{T,N,dims}(::T24Linear{T,N,dims}) = dims
import Base: LinearFast
Base.linearindexing{A<:T24Linear}(::Type{A}) = LinearFast()
Base.getindex(A::T24Linear, i::Int) = getfield(A, i)
Base.setindex!{T}(A::T24Linear{T}, v, i::Int) = setfield!(A, i, convert(T, v))

# A custom linear slow sparse-like array that relies upon Dict for its storage
immutable TSlow{T,N} <: AbstractArray{T,N}
    data::Dict{NTuple{N,Int}, T}
    dims::NTuple{N,Int}
end
TSlow{T}(::Type{T}, dims::Int...) = TSlow(T, dims)
TSlow{T,N}(::Type{T}, dims::NTuple{N,Int}) = TSlow{T,N}(Dict{NTuple{N,Int}, T}(), dims)

Base.convert{T,N  }(::Type{TSlow     }, X::AbstractArray{T,N}) = convert(TSlow{T,N}, X)
Base.convert{T,N,_}(::Type{TSlow{T  }}, X::AbstractArray{_,N}) = convert(TSlow{T,N}, X)
Base.convert{T,N  }(::Type{TSlow{T,N}}, X::AbstractArray     ) = begin
    A = TSlow(T, size(X))
    cartesianmap((I...)->(A[I...] = X[I...]), size(X))
    A
end

Base.size(A::TSlow) = A.dims
Base.similar{T}(A::TSlow, ::Type{T}, dims::Dims) = TSlow(T, dims)
import Base: LinearSlow
Base.linearindexing{A<:TSlow}(::Type{A}) = LinearSlow()
# Until #11242 is merged, we need to define each dimension independently
Base.getindex{T}(A::TSlow{T,0}) = get(A.data, (), zero(T))
Base.getindex{T}(A::TSlow{T,1}, i1::Int) = get(A.data, (i1,), zero(T))
Base.getindex{T}(A::TSlow{T,2}, i1::Int, i2::Int) = get(A.data, (i1,i2), zero(T))
Base.getindex{T}(A::TSlow{T,3}, i1::Int, i2::Int, i3::Int) =
    get(A.data, (i1,i2,i3), zero(T))
Base.getindex{T}(A::TSlow{T,4}, i1::Int, i2::Int, i3::Int, i4::Int) =
    get(A.data, (i1,i2,i3,i4), zero(T))
Base.getindex{T}(A::TSlow{T,5}, i1::Int, i2::Int, i3::Int, i4::Int, i5::Int) =
    get(A.data, (i1,i2,i3,i4,i5), zero(T))

Base.setindex!{T}(A::TSlow{T,0}, v) = (A.data[()] = v)
Base.setindex!{T}(A::TSlow{T,1}, v, i1::Int) = (A.data[(i1,)] = v)
Base.setindex!{T}(A::TSlow{T,2}, v, i1::Int, i2::Int) = (A.data[(i1,i2)] = v)
Base.setindex!{T}(A::TSlow{T,3}, v, i1::Int, i2::Int, i3::Int) =
    (A.data[(i1,i2,i3)] = v)
Base.setindex!{T}(A::TSlow{T,4}, v, i1::Int, i2::Int, i3::Int, i4::Int) =
    (A.data[(i1,i2,i3,i4)] = v)
Base.setindex!{T}(A::TSlow{T,5}, v, i1::Int, i2::Int, i3::Int, i4::Int, i5::Int) =
    (A.data[(i1,i2,i3,i4,i5)] = v)


import Base: trailingsize
const can_inline = Base.JLOptions().can_inline != 0
function test_scalar_indexing{T}(::Type{T}, shape)
    N = prod(shape)
    A = reshape(1:N, shape)
    B = T(A)
    @test A == B
    # Test indexing up to 5 dimensions
    i=0
    for i5 = 1:trailingsize(B, 5)
        for i4 = 1:size(B, 4)
            for i3 = 1:size(B, 3)
                for i2 = 1:size(B, 2)
                    for i1 = 1:size(B, 1)
                        i += 1
                        @test A[i1,i2,i3,i4,i5] == B[i1,i2,i3,i4,i5] == i
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
    for i2 = 1:trailingsize(B, 2)
        for i1 = 1:size(B, 1)
            i += 1
            @test A[i1,i2] == B[i1,i2] == i
        end
    end
    @test A == B
    i=0
    for i3 = 1:trailingsize(B, 3)
        for i2 = 1:size(B, 2)
            for i1 = 1:size(B, 1)
                i += 1
                @test A[i1,i2,i3] == B[i1,i2,i3] == i
            end
        end
    end
    # Test zero-dimensional accesses
    @test A[] == B[] == A[1] == B[1] == 1
    # Test multidimensional scalar indexed assignment
    C = T(Int, shape)
    i=0
    for i5 = 1:trailingsize(B, 5)
        for i4 = 1:size(B, 4)
            for i3 = 1:size(B, 3)
                for i2 = 1:size(B, 2)
                    for i1 = 1:size(B, 1)
                        i += 1
                        C[i1,i2,i3,i4,i5] = i
                    end
                end
            end
        end
    end
    @test C == B == A
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
    for i2 = 1:trailingsize(C, 2)
        for i1 = 1:size(C, 1)
            i += 1
            C[i1,i2] = i
        end
    end
    @test C == B == A
    C = T(Int, shape)
    i=0
    for i3 = 1:trailingsize(C, 3)
        for i2 = 1:size(C, 2)
            for i1 = 1:size(C, 1)
                i += 1
                C[i1,i2,i3] = i
            end
        end
    end
    @test C == B == A
    # Test zero-dimensional setindex
    A[] = 0; B[] = 0
    @test A[] == B[] == 0
    @test A == B
end

function test_vector_indexing{T}(::Type{T}, shape)
    N = prod(shape)
    A = reshape(1:N, shape)
    B = T(A)
    idxs = rand(1:N, 3, 3, 3)
    @test B[idxs] == A[idxs] == idxs
    @test B[vec(idxs)] == A[vec(idxs)] == vec(idxs)
    @test B[:] == A[:] == collect(1:N)
    @test B[1:end] == A[1:end] == collect(1:N)
    @test B[:,:] == A[:,:] == reshape(1:N, shape[1], prod(shape[2:end]))
    @test B[1:end,1:end] == A[1:end,1:end] == reshape(1:N, shape[1], prod(shape[2:end]))
end

for T in (T24Linear, TSlow), shape in ((24,), (2, 12), (2,3,4), (1,2,3,4), (4,3,2,1))
    test_scalar_indexing(T, shape)
    test_vector_indexing(T, shape)
end
