# Optimized methods for weighted sum over dimensions
# (generic method is defined in base/reducedim.jl)
#
#  _wsum! is specialized for following cases:
#     (a) A is a dense matrix with eltype <: BlasReal: we call gemv!
#         The internal function that implements this is _wsum2_blas!
#
#     (b) A is a contiguous array with eltype <: BlasReal:
#         dim == 1: treat A like a matrix of size (d1, d2 x ... x dN)
#         dim == N: treat A like a matrix of size (d1 x ... x d(N-1), dN)
#         otherwise: decompose A into multiple pages, and apply _wsum2_blas!
#         for each
#         The internal function that implements this is _wsumN!
#
#     (c) A is a general dense array with eltype <: BlasReal:
#         dim <= 2: delegate to (a) and (b)
#         otherwise, decompose A into multiple pages
#         The internal function that implements this is _wsumN!

function _wsum2_blas!(R::StridedVector{T}, A::StridedMatrix{T}, w::StridedVector{T},
                      dim::Int, init::Bool) where T<:BlasReal
    beta = ifelse(init, zero(T), one(T))
    trans = dim == 1 ? 'T' : 'N'
    BLAS.gemv!(trans, one(T), A, w, beta, R)
    return R
end

function _wsumN!(R::StridedArray{T}, A::StridedArray{T,N}, w::StridedVector{T},
                 dim::Int, init::Bool) where {T<:BlasReal,N}
    if dim == 1
        m = size(A, 1)
        n = div(length(A), m)
        _wsum2_blas!(view(R,:), reshape(A, (m, n)), w, 1, init)
    elseif dim == N
        n = size(A, N)
        m = div(length(A), n)
        _wsum2_blas!(view(R,:), reshape(A, (m, n)), w, 2, init)
    else # 1 < dim < N
        m = 1
        for i = 1:dim-1
            m *= size(A, i)
        end
        n = size(A, dim)
        k = 1
        for i = dim+1:N
            k *= size(A, i)
        end
        Av = reshape(A, (m, n, k))
        Rv = reshape(R, (m, k))
        for i = 1:k
            _wsum2_blas!(view(Rv,:,i), view(Av,:,:,i), w, 2, init)
        end
    end
    return R
end

function _wsumN!(R::StridedArray{T}, A::DenseArray{T,N}, w::StridedVector{T},
                 dim::Int, init::Bool) where {T<:BlasReal,N}
    @assert N >= 3
    if dim <= 2
        m = size(A, 1)
        n = size(A, 2)
        npages = 1
        for i = 3:N
            npages *= size(A, i)
        end
        rlen = ifelse(dim == 1, n, m)
        Rv = reshape(R, (rlen, npages))
        for i = 1:npages
            _wsum2_blas!(view(Rv,:,i), view(A,:,:,i), w, dim, init)
        end
    else
        Base._wsum_general!(R, A, w, dim, init)
    end
    return R
end

_wsum!(R::StridedArray{T}, A::DenseArray{T,2}, w::StridedVector{T},
       dim::Int, init::Bool) where {T<:BlasReal} =
    _wsum2_blas!(view(R,:), A, w, dim, init)

_wsum!(R::StridedArray{T}, A::DenseArray{T}, w::StridedVector{T},
       dim::Int, init::Bool) where {T<:BlasReal} =
    _wsumN!(R, A, w, dim, init)