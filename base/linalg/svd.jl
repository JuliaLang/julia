# This file is a part of Julia. License is MIT: http://julialang.org/license

# Singular Value Decomposition
immutable SVD{T,Tr,M<:AbstractArray} <: Factorization{T}
    U::M
    S::Vector{Tr}
    Vt::M
    SVD{T,Tr,M}(U::AbstractArray{T}, S::Vector{Tr}, Vt::AbstractArray{T}) where {T,Tr,M} =
        new(U, S, Vt)
end
SVD(U::AbstractArray{T}, S::Vector{Tr}, Vt::AbstractArray{T}) where {T,Tr} = SVD{T,Tr,typeof(U)}(U, S, Vt)

"""
    svdfact!(A, thin::Bool=true) -> SVD

`svdfact!` is the same as [`svdfact`](@ref), but saves space by
overwriting the input `A`, instead of creating a copy.

If `thin=true` (default), a thin SVD is returned. For a ``M \\times N`` matrix
`A`, `U` is ``M \\times M`` for a full SVD (`thin=false`) and
``M \\times \\min(M, N)`` for a thin SVD.
"""
function svdfact!{T<:BlasFloat}(A::StridedMatrix{T}; thin::Bool=true)
    m,n = size(A)
    if m == 0 || n == 0
        u,s,vt = (eye(T, m, thin ? n : m), real(zeros(T,0)), eye(T,n,n))
    else
        u,s,vt = LAPACK.gesdd!(thin ? 'S' : 'A', A)
    end
    SVD(u,s,vt)
end

"""
    svdfact(A, thin::Bool=true) -> SVD

Compute the singular value decomposition (SVD) of `A` and return an `SVD` object.

`U`, `S`, `V` and `Vt` can be obtained from the factorization `F` with `F[:U]`,
`F[:S]`, `F[:V]` and `F[:Vt]`, such that `A = U*diagm(S)*Vt`.
The algorithm produces `Vt` and hence `Vt` is more efficient to extract than `V`.

If `thin=true` (default), a thin SVD is returned. For a ``M \\times N`` matrix
`A`, `U` is ``M \\times M`` for a full SVD (`thin=false`) and
``M \\times \\min(M, N)`` for a thin SVD.

# Example
```jldoctest
julia> A = [1. 0. 0. 0. 2.; 0. 0. 3. 0. 0.; 0. 0. 0. 0. 0.; 0. 2. 0. 0. 0.]
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0

julia> F = svdfact(A)
Base.LinAlg.SVD{Float64,Float64,Array{Float64,2}}([0.0 1.0 0.0 0.0; 1.0 0.0 0.0 0.0; 0.0 0.0 0.0 -1.0; 0.0 0.0 1.0 0.0], [3.0, 2.23607, 2.0, 0.0], [-0.0 0.0 … -0.0 0.0; 0.447214 0.0 … 0.0 0.894427; -0.0 1.0 … -0.0 0.0; 0.0 0.0 … 1.0 0.0])

julia> F[:U] * diagm(F[:S]) * F[:Vt]
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0
```
"""
function svdfact{T}(A::StridedVecOrMat{T}; thin::Bool = true)
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    svdfact!(copy_oftype(A, S), thin = thin)
end
svdfact(x::Number; thin::Bool=true) = SVD(x == 0 ? fill(one(x), 1, 1) : fill(x/abs(x), 1, 1), [abs(x)], fill(one(x), 1, 1))
svdfact(x::Integer; thin::Bool=true) = svdfact(float(x), thin=thin)

"""
    svd(A, thin::Bool=true) -> U, S, V

Computes the SVD of `A`, returning `U`, vector `S`, and `V` such that
`A == U*diagm(S)*V'`.

If `thin=true` (default), a thin SVD is returned. For a ``M \\times N`` matrix
`A`, `U` is ``M \\times M`` for a full SVD (`thin=false`) and
``M \\times \\min(M, N)`` for a thin SVD.

`svd` is a wrapper around [`svdfact`](@ref), extracting all parts
of the `SVD` factorization to a tuple. Direct use of `svdfact` is therefore more
efficient.

# Example

```jldoctest
julia> A = [1. 0. 0. 0. 2.; 0. 0. 3. 0. 0.; 0. 0. 0. 0. 0.; 0. 2. 0. 0. 0.]
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0

julia> U, S, V = svd(A)
([0.0 1.0 0.0 0.0; 1.0 0.0 0.0 0.0; 0.0 0.0 0.0 -1.0; 0.0 0.0 1.0 0.0], [3.0, 2.23607, 2.0, 0.0], [-0.0 0.447214 -0.0 0.0; 0.0 0.0 1.0 0.0; … ; -0.0 0.0 -0.0 1.0; 0.0 0.894427 0.0 0.0])

julia> U*diagm(S)*V'
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0
```
"""
function svd(A::Union{Number, AbstractArray}; thin::Bool=true)
    F = svdfact(A, thin=thin)
    F.U, F.S, F.Vt'
end

function getindex(F::SVD, d::Symbol)
    if d == :U
        return F.U
    elseif d == :S
        return F.S
    elseif d == :Vt
        return F.Vt
    elseif d == :V
        return F.Vt'
    else
        throw(KeyError(d))
    end
end

"""
    svdvals!(A)

Returns the singular values of `A`, saving space by overwriting the input.
See also [`svdvals`](@ref).
"""
svdvals!{T<:BlasFloat}(A::StridedMatrix{T}) = findfirst(size(A), 0) > 0 ? zeros(T, 0) : LAPACK.gesdd!('N', A)[2]
svdvals{T<:BlasFloat}(A::AbstractMatrix{T}) = svdvals!(copy(A))

"""
    svdvals(A)

Returns the singular values of `A`.

# Example

```jldoctest
julia> A = [1. 0. 0. 0. 2.; 0. 0. 3. 0. 0.; 0. 0. 0. 0. 0.; 0. 2. 0. 0. 0.]
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0

julia> svdvals(A)
4-element Array{Float64,1}:
 3.0
 2.23607
 2.0
 0.0
```
"""
function svdvals{T}(A::AbstractMatrix{T})
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    svdvals!(copy_oftype(A, S))
end
svdvals(x::Number) = abs(x)
svdvals{T, Tr}(S::SVD{T, Tr}) = (S[:S])::Vector{Tr}

# SVD least squares
function A_ldiv_B!{Ta,Tb}(A::SVD{Ta}, B::StridedVecOrMat{Tb})
    k = searchsortedlast(A.S, eps(real(Ta))*A.S[1], rev=true)
    view(A.Vt,1:k,:)' * (view(A.S,1:k) .\ (view(A.U,:,1:k)' * B))
end

# Generalized svd
immutable GeneralizedSVD{T,S} <: Factorization{T}
    U::S
    V::S
    Q::S
    a::Vector
    b::Vector
    k::Int
    l::Int
    R::S
    function GeneralizedSVD{T,S}(U::AbstractMatrix{T}, V::AbstractMatrix{T}, Q::AbstractMatrix{T},
                                 a::Vector, b::Vector, k::Int, l::Int, R::AbstractMatrix{T}) where {T,S}
        new(U, V, Q, a, b, k, l, R)
    end
end
function GeneralizedSVD(U::AbstractMatrix{T}, V::AbstractMatrix{T}, Q::AbstractMatrix{T},
                        a::Vector, b::Vector, k::Int, l::Int, R::AbstractMatrix{T}) where T
    GeneralizedSVD{T,typeof(U)}(U, V, Q, a, b, k, l, R)
end

"""
    svdfact!(A, B) -> GeneralizedSVD

`svdfact!` is the same as [`svdfact`](@ref), but modifies the arguments
`A` and `B` in-place, instead of making copies.
"""
function svdfact!{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T})
    # xggsvd3 replaced xggsvd in LAPACK 3.6.0
    if LAPACK.laver() < (3, 6, 0)
        U, V, Q, a, b, k, l, R = LAPACK.ggsvd!('U', 'V', 'Q', A, B)
    else
        U, V, Q, a, b, k, l, R = LAPACK.ggsvd3!('U', 'V', 'Q', A, B)
    end
    GeneralizedSVD(U, V, Q, a, b, Int(k), Int(l), R)
end
svdfact{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) = svdfact!(copy(A),copy(B))

"""
    svdfact(A, B) -> GeneralizedSVD

Compute the generalized SVD of `A` and `B`, returning a `GeneralizedSVD` factorization
object `F`, such that `A = F[:U]*F[:D1]*F[:R0]*F[:Q]'` and `B = F[:V]*F[:D2]*F[:R0]*F[:Q]'`.

For an M-by-N matrix `A` and P-by-N matrix `B`,

- `F[:U]` is a M-by-M orthogonal matrix,
- `F[:V]` is a P-by-P orthogonal matrix,
- `F[:Q]` is a N-by-N orthogonal matrix,
- `F[:R0]` is a (K+L)-by-N matrix whose rightmost (K+L)-by-(K+L) block is
           nonsingular upper block triangular,
- `F[:D1]` is a M-by-(K+L) diagonal matrix with 1s in the first K entries,
- `F[:D2]` is a P-by-(K+L) matrix whose top right L-by-L block is diagonal,

`K+L` is the effective numerical rank of the matrix `[A; B]`.

The entries of `F[:D1]` and `F[:D2]` are related, as explained in the LAPACK
documentation for the
[generalized SVD](http://www.netlib.org/lapack/lug/node36.html) and the
[xGGSVD3](http://www.netlib.org/lapack/explore-html/d6/db3/dggsvd3_8f.html)
routine which is called underneath (in LAPACK 3.6.0 and newer).
"""
function svdfact{TA,TB}(A::StridedMatrix{TA}, B::StridedMatrix{TB})
    S = promote_type(Float32, typeof(one(TA)/norm(one(TA))),TB)
    return svdfact!(copy_oftype(A, S), copy_oftype(B, S))
end

"""
    svd(A, B) -> U, V, Q, D1, D2, R0

Wrapper around [`svdfact`](@ref) extracting all parts of the
factorization to a tuple. Direct use of
`svdfact` is therefore generally more efficient. The function returns the generalized SVD of
`A` and `B`, returning `U`, `V`, `Q`, `D1`, `D2`, and `R0` such that `A = U*D1*R0*Q'` and `B =
V*D2*R0*Q'`.
"""
function svd(A::AbstractMatrix, B::AbstractMatrix)
    F = svdfact(A, B)
    F[:U], F[:V], F[:Q], F[:D1], F[:D2], F[:R0]
end

function getindex{T}(obj::GeneralizedSVD{T}, d::Symbol)
    if d == :U
        return obj.U
    elseif d == :V
        return obj.V
    elseif d == :Q
        return obj.Q
    elseif d == :alpha || d == :a
        return obj.a
    elseif d == :beta || d == :b
        return obj.b
    elseif d == :vals || d == :S
        return obj.a[1:obj.k + obj.l] ./ obj.b[1:obj.k + obj.l]
    elseif d == :D1
        m = size(obj.U, 1)
        if m - obj.k - obj.l >= 0
            return [eye(T, obj.k) zeros(T, obj.k, obj.l); zeros(T, obj.l, obj.k) diagm(obj.a[obj.k + 1:obj.k + obj.l]); zeros(T, m - obj.k - obj.l, obj.k + obj.l)]
        else
            return [eye(T, m, obj.k) [zeros(T, obj.k, m - obj.k); diagm(obj.a[obj.k + 1:m])] zeros(T, m, obj.k + obj.l - m)]
        end
    elseif d == :D2
        m = size(obj.U, 1)
        p = size(obj.V, 1)
        if m - obj.k - obj.l >= 0
            return [zeros(T, obj.l, obj.k) diagm(obj.b[obj.k + 1:obj.k + obj.l]); zeros(T, p - obj.l, obj.k + obj.l)]
        else
            return [zeros(T, p, obj.k) [diagm(obj.b[obj.k + 1:m]); zeros(T, obj.k + p - m, m - obj.k)] [zeros(T, m - obj.k, obj.k + obj.l - m); eye(T, obj.k + p - m, obj.k + obj.l - m)]]
        end
    elseif d == :R
        return obj.R
    elseif d == :R0
        n = size(obj.Q, 1)
        return [zeros(T, obj.k + obj.l, n - obj.k - obj.l) obj.R]
    else
        throw(KeyError(d))
    end
end

function svdvals!{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T})
    # xggsvd3 replaced xggsvd in LAPACK 3.6.0
    if LAPACK.laver() < (3, 6, 0)
        _, _, _, a, b, k, l, _ = LAPACK.ggsvd!('N', 'N', 'N', A, B)
    else
        _, _, _, a, b, k, l, _ = LAPACK.ggsvd3!('N', 'N', 'N', A, B)
    end
    a[1:k + l] ./ b[1:k + l]
end
svdvals{T<:BlasFloat}(A::StridedMatrix{T},B::StridedMatrix{T}) = svdvals!(copy(A),copy(B))

"""
    svdvals(A, B)

Return the generalized singular values from the generalized singular value
decomposition of `A` and `B`. See also [`svdfact`](@ref).
"""
function svdvals{TA,TB}(A::StridedMatrix{TA}, B::StridedMatrix{TB})
    S = promote_type(Float32, typeof(one(TA)/norm(one(TA))), TB)
    return svdvals!(copy_oftype(A, S), copy_oftype(B, S))
end

# Conversion
convert(::Type{AbstractMatrix}, F::SVD) = (F.U * Diagonal(F.S)) * F.Vt
convert(::Type{AbstractArray}, F::SVD) = convert(AbstractMatrix, F)
convert(::Type{Matrix}, F::SVD) = convert(Array, convert(AbstractArray, F))
convert(::Type{Array}, F::SVD) = convert(Matrix, F)
full(F::SVD) = convert(AbstractArray, F)
