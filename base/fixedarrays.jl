module FixedArrays

export AbstractFixedArray, FixedArrayI, FixedArrayM

using Base.Cartesian
import Base: (*), (==), A_mul_B!, eltype, getindex, length, ndims, size

## Types

abstract AbstractFixedArray{T,N,SZ} <: DenseArray{T,N}

immutable FixedArrayI{T,N,SZ,L} <: AbstractFixedArray{T,N,SZ}
    data::NTuple{L,T}
end

immutable FixedArrayM{T,N,SZ} <: AbstractFixedArray{T,N,SZ}
    data::Array{T,N}
end

## Constructors
# Note these can't be type-stable, since the size of A is unknown to the type system

function FixedArrayI(A::AbstractArray)
    L = length(A)
    FixedArrayI{eltype(A), ndims(A), size(A), L}(ntuple(L, i->A[i]))
end

toarray(A) = convert(Array, A)
toarray(A::Array) = copy(A)

FixedArrayM(A::AbstractArray) = FixedArrayM{eltype(A), ndims(A), size(A)}(toarray(A))


## Utility functions
eltype{T,N,SZ}(A::AbstractFixedArray{T,N,SZ}) = T
length{T,N,SZ}(A::AbstractFixedArray{T,N,SZ}) = prod(SZ)
 ndims{T,N,SZ}(A::AbstractFixedArray{T,N,SZ}) = N
  size{T,N,SZ}(A::AbstractFixedArray{T,N,SZ}) = SZ
  size{T,N,SZ}(A::AbstractFixedArray{T,N,SZ}, d::Integer) = SZ[d]

getindex(A::AbstractFixedArray, i::Real) = A.data[i]
@nsplat N getindex(A::FixedArrayM, I::NTuple{N,Real}...) = getindex(A.data, I...)

getindex{T,SZ}(A::FixedArrayI{T,2,SZ}, i::Real, j::Real) = A.data[i+(j-1)*SZ[1]]

## Comparisons

for (TA, TB) in ((AbstractArray,AbstractFixedArray), (AbstractArray,AbstractFixedArray))
    if TA == AbstractArray && TB == AbstractArray
        continue
    end
    @eval begin
        function (==)(A::$TA, B::$TB)
            size(A) == size(B) || return false
            for i = 1:length(B)
                if A[i] != B[i]
                    return false
                end
            end
            true
        end
    end
end

## Linear algebra

for N=1:4, K=1:4, M=1:4
    L = M*N
    @eval begin
        function (*){TA,TB}(A::FixedArrayI{TA,2,($M,$K)}, B::FixedArrayI{TB,2,($K,$N)})
            TP = typeof(one(TA)*one(TB) + one(TA)*one(TB))
            @nexprs $K d2->(@nexprs $M d1->A_d1_d2 = A.data[(d2-1)*$M+d1])
            @nexprs $N d2->(@nexprs $K d1->B_d1_d2 = B.data[(d2-1)*$K+d1])
            @nexprs $N n->(@nexprs $M m->begin
                tmp = zero(TP)
                @nexprs $K k->(tmp += A_m_k * B_k_n)
                dest_{m+(n-1)*$K} = tmp
            end)
            FixedArrayI{TP,2,($M,$N),$L}(@ntuple $L d->dest_d)
        end
    end
end

function (*){TA,TB,M,K,N}(A::FixedArrayM{TA,2,(M,K)}, B::FixedArrayM{TB,2,(K,N)})
    TP = typeof(one(TA)*one(TB) + one(TA)*one(TB))
    A_mul_B!(FixedArrayM{TP,2,(M,N)}(Array(TP, M, N)), A, B)
end

for N=1:4, K=1:4, M=1:4
    @eval begin
        function A_mul_B!{TC,TA,TB}(out::FixedArrayM{TC,2,($M,$N)}, A::AbstractFixedArray{TA,2,($M,$K)}, B::AbstractFixedArray{TB,2,($K,$N)})
            TP = typeof(one(TA)*one(TB) + one(TA)*one(TB))
            @nexprs $K d2->(@nexprs $M d1->@inbounds A_d1_d2 = A.data[(d2-1)*$M+d1])
            @nexprs $N d2->(@nexprs $K d1->@inbounds B_d1_d2 = B.data[(d2-1)*$K+d1])
            @nexprs $N n->(@nexprs $M m->begin
                tmp = zero(TP)
                @nexprs $K k->(tmp += A_m_k * B_k_n)
                @inbounds out.data[m,n] = tmp
            end)
            out
        end
    end
end

end
