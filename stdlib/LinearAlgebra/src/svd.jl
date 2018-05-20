# This file is a part of Julia. License is MIT: https://julialang.org/license

# Singular Value Decomposition
struct SVD{T,Tr,M<:AbstractArray} <: Factorization{T}
    U::M
    S::Vector{Tr}
    Vt::M
    SVD{T,Tr,M}(U::AbstractArray{T}, S::Vector{Tr}, Vt::AbstractArray{T}) where {T,Tr,M} =
        new(U, S, Vt)
end
SVD(U::AbstractArray{T}, S::Vector{Tr}, Vt::AbstractArray{T}) where {T,Tr} = SVD{T,Tr,typeof(U)}(U, S, Vt)

# iteration for destructuring into factors
Base.start(::SVD) = Val(:U)
Base.next(F::SVD, ::Val{:U}) = (F.U, Val(:S))
Base.next(F::SVD, ::Val{:S}) = (F.S, Val(:Vt))
Base.next(F::SVD, ::Val{:Vt}) = (F.Vt, Val(:done))
Base.done(F::SVD, ::Val{:done}) = true
Base.done(F::SVD, ::Any) = false

"""
    svdfact!(A; full::Bool = false) -> SVD

`svdfact!` is the same as [`svdfact`](@ref), but saves space by
overwriting the input `A`, instead of creating a copy.

# Examples
```jldoctest
julia> A = [1. 0. 0. 0. 2.; 0. 0. 3. 0. 0.; 0. 0. 0. 0. 0.; 0. 2. 0. 0. 0.]
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0

julia> F = svdfact!(A);

julia> F.U * Diagonal(F.S) * F.Vt
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0

julia> A
4×5 Array{Float64,2}:
 -2.23607   0.0   0.0  0.0  0.618034
  0.0      -3.0   1.0  0.0  0.0
  0.0       0.0   0.0  0.0  0.0
  0.0       0.0  -2.0  0.0  0.0
```
"""
function svdfact!(A::StridedMatrix{T}; full::Bool = false, thin::Union{Bool,Nothing} = nothing) where T<:BlasFloat
    # DEPRECATION TODO: remove deprecated thin argument and associated logic after 0.7
    if thin != nothing
        Base.depwarn(string("the `thin` keyword argument in `svdfact!(A; thin = $(thin))` has ",
            "been deprecated in favor of `full`, which has the opposite meaning, ",
            "e.g. `svdfact!(A; full = $(!thin))`."), :svdfact!)
        full::Bool = !thin
    end
    m,n = size(A)
    if m == 0 || n == 0
        u,s,vt = (Matrix{T}(I, m, full ? m : n), real(zeros(T,0)), Matrix{T}(I, n, n))
    else
        u,s,vt = LAPACK.gesdd!(full ? 'A' : 'S', A)
    end
    SVD(u,s,vt)
end

"""
    svdfact(A; full::Bool = false) -> SVD

Compute the singular value decomposition (SVD) of `A` and return an `SVD` object.

`U`, `S`, `V` and `Vt` can be obtained from the factorization `F` with `F.U`,
`F.S`, `F.V` and `F.Vt`, such that `A = U * Diagonal(S) * Vt`.
The algorithm produces `Vt` and hence `Vt` is more efficient to extract than `V`.
The singular values in `S` are sorted in descending order.

If `full = false` (default), a "thin" SVD is returned. For a ``M
\\times N`` matrix `A`, in the full factorization `U` is `M \\times M`
and `V` is `N \\times N`, while in the thin factorization `U` is `M
\\times K` and `V` is `N \\times K`, where `K = \\min(M,N)` is the
number of singular values.

# Examples
```jldoctest
julia> A = [1. 0. 0. 0. 2.; 0. 0. 3. 0. 0.; 0. 0. 0. 0. 0.; 0. 2. 0. 0. 0.]
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0

julia> F = svdfact(A);

julia> F.U * Diagonal(F.S) * F.Vt
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0

julia> U, S, Vt = svdfact(A); # destructuring via iteration

julia> U*Diagonal(S)*Vt ≈ A
true
```
"""
function svdfact(A::StridedVecOrMat{T}; full::Bool = false, thin::Union{Bool,Nothing} = nothing) where T
    # DEPRECATION TODO: remove deprecated thin argument and associated logic after 0.7
    if thin != nothing
        Base.depwarn(string("the `thin` keyword argument in `svdfact(A; thin = $(thin))` has ",
            "been deprecated in favor of `full`, which has the opposite meaning, ",
            "e.g. `svdfact(A; full = $(!thin))`."), :svdfact)
        full::Bool = !thin
    end
    svdfact!(copy_oftype(A, eigtype(T)), full = full)
end
function svdfact(x::Number; full::Bool = false, thin::Union{Bool,Nothing} = nothing)
    # DEPRECATION TODO: remove deprecated thin argument and associated logic after 0.7
    if thin != nothing
        Base.depwarn(string("the `thin` keyword argument in `svdfact(A; thin = $(thin))` has ",
            "been deprecated in favor of `full`, which has the opposite meaning, ",
            "e.g. `svdfact(A; full = $(!thin))`."), :svdfact)
        full::Bool = !thin
    end
    return SVD(x == 0 ? fill(one(x), 1, 1) : fill(x/abs(x), 1, 1), [abs(x)], fill(one(x), 1, 1))
end
function svdfact(x::Integer; full::Bool = false, thin::Union{Bool,Nothing} = nothing)
    # DEPRECATION TODO: remove deprecated thin argument and associated logic after 0.7
    if thin != nothing
        Base.depwarn(string("the `thin` keyword argument in `svdfact(A; thin = $(thin))` has ",
            "been deprecated in favor of `full`, which has the opposite meaning, ",
            "e.g. `svdfact(A; full = $(!thin))`."), :svdfact)
        full::Bool = !thin
    end
    return svdfact(float(x), full = full)
end


function getproperty(F::SVD, d::Symbol)
    if d == :V
        return getfield(F, :Vt)'
    else
        return getfield(F, d)
    end
end

Base.propertynames(F::SVD, private::Bool=false) =
    private ? (:V, fieldnames(typeof(F))...) : (:U, :S, :V, :Vt)

"""
    svdvals!(A)

Return the singular values of `A`, saving space by overwriting the input.
See also [`svdvals`](@ref) and [`svdfact`](@ref).

# Examples
```jldoctest
julia> A = [1. 0. 0. 0. 2.; 0. 0. 3. 0. 0.; 0. 0. 0. 0. 0.; 0. 2. 0. 0. 0.]
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0

julia> svdvals!(A)
4-element Array{Float64,1}:
 3.0
 2.23606797749979
 2.0
 0.0

julia> A
4×5 Array{Float64,2}:
 -2.23607   0.0   0.0  0.0  0.618034
  0.0      -3.0   1.0  0.0  0.0
  0.0       0.0   0.0  0.0  0.0
  0.0       0.0  -2.0  0.0  0.0
```
"""
svdvals!(A::StridedMatrix{T}) where {T<:BlasFloat} = isempty(A) ? zeros(real(T), 0) : LAPACK.gesdd!('N', A)[2]
svdvals(A::AbstractMatrix{<:BlasFloat}) = svdvals!(copy(A))

"""
    svdvals(A)

Return the singular values of `A` in descending order.

# Examples
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
 2.23606797749979
 2.0
 0.0
```
"""
svdvals(A::AbstractMatrix{T}) where T = svdvals!(copy_oftype(A, eigtype(T)))
svdvals(x::Number) = abs(x)
svdvals(S::SVD{<:Any,T}) where {T} = (S.S)::Vector{T}

# SVD least squares
function ldiv!(A::SVD{T}, B::StridedVecOrMat) where T
    k = searchsortedlast(A.S, eps(real(T))*A.S[1], rev=true)
    view(A.Vt,1:k,:)' * (view(A.S,1:k) .\ (view(A.U,:,1:k)' * B))
end

# Generalized svd
struct GeneralizedSVD{T,S} <: Factorization{T}
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

# iteration for destructuring into factors
Base.start(::GeneralizedSVD) = Val(:U)
Base.next(F::GeneralizedSVD, ::Val{:U}) = (F.U, Val(:V))
Base.next(F::GeneralizedSVD, ::Val{:V}) = (F.V, Val(:Q))
Base.next(F::GeneralizedSVD, ::Val{:Q}) = (F.Q, Val(:D1))
Base.next(F::GeneralizedSVD, ::Val{:D1}) = (F.D1, Val(:D2))
Base.next(F::GeneralizedSVD, ::Val{:D2}) = (F.D2, Val(:R0))
Base.next(F::GeneralizedSVD, ::Val{:R0}) = (F.R0, Val(:done))
Base.done(F::GeneralizedSVD, ::Val{:done}) = true
Base.done(F::GeneralizedSVD, ::Any) = false

"""
    svdfact!(A, B) -> GeneralizedSVD

`svdfact!` is the same as [`svdfact`](@ref), but modifies the arguments
`A` and `B` in-place, instead of making copies.

# Examples
```jldoctest
julia> A = [1. 0.; 0. -1.]
2×2 Array{Float64,2}:
 1.0   0.0
 0.0  -1.0

julia> B = [0. 1.; 1. 0.]
2×2 Array{Float64,2}:
 0.0  1.0
 1.0  0.0

julia> F = svdfact!(A, B);

julia> F.U*F.D1*F.R0*F.Q'
2×2 Array{Float64,2}:
 1.0   0.0
 0.0  -1.0

julia> F.V*F.D2*F.R0*F.Q'
2×2 Array{Float64,2}:
 0.0  1.0
 1.0  0.0

julia> A
2×2 Array{Float64,2}:
 1.41421   0.0
 0.0      -1.41421

julia> B
2×2 Array{Float64,2}:
 1.0  -0.0
 0.0  -1.0
```
"""
function svdfact!(A::StridedMatrix{T}, B::StridedMatrix{T}) where T<:BlasFloat
    # xggsvd3 replaced xggsvd in LAPACK 3.6.0
    if LAPACK.version() < v"3.6.0"
        U, V, Q, a, b, k, l, R = LAPACK.ggsvd!('U', 'V', 'Q', A, B)
    else
        U, V, Q, a, b, k, l, R = LAPACK.ggsvd3!('U', 'V', 'Q', A, B)
    end
    GeneralizedSVD(U, V, Q, a, b, Int(k), Int(l), R)
end
svdfact(A::StridedMatrix{T}, B::StridedMatrix{T}) where {T<:BlasFloat} = svdfact!(copy(A),copy(B))

"""
    svdfact(A, B) -> GeneralizedSVD

Compute the generalized SVD of `A` and `B`, returning a `GeneralizedSVD` factorization
object `F`, such that `A = F.U*F.D1*F.R0*F.Q'` and `B = F.V*F.D2*F.R0*F.Q'`.

For an M-by-N matrix `A` and P-by-N matrix `B`,

- `U` is a M-by-M orthogonal matrix,
- `V` is a P-by-P orthogonal matrix,
- `Q` is a N-by-N orthogonal matrix,
- `D1` is a M-by-(K+L) diagonal matrix with 1s in the first K entries,
- `D2` is a P-by-(K+L) matrix whose top right L-by-L block is diagonal,
- `R0` is a (K+L)-by-N matrix whose rightmost (K+L)-by-(K+L) block is
           nonsingular upper block triangular,

`K+L` is the effective numerical rank of the matrix `[A; B]`.

The entries of `F.D1` and `F.D2` are related, as explained in the LAPACK
documentation for the
[generalized SVD](http://www.netlib.org/lapack/lug/node36.html) and the
[xGGSVD3](http://www.netlib.org/lapack/explore-html/d6/db3/dggsvd3_8f.html)
routine which is called underneath (in LAPACK 3.6.0 and newer).

# Examples
```jldoctest
julia> A = [1. 0.; 0. -1.]
2×2 Array{Float64,2}:
 1.0   0.0
 0.0  -1.0

julia> B = [0. 1.; 1. 0.]
2×2 Array{Float64,2}:
 0.0  1.0
 1.0  0.0

julia> F = svdfact(A, B);

julia> F.U*F.D1*F.R0*F.Q'
2×2 Array{Float64,2}:
 1.0   0.0
 0.0  -1.0

julia> F.V*F.D2*F.R0*F.Q'
2×2 Array{Float64,2}:
 0.0  1.0
 1.0  0.0

julia> U, V, Q, D1, D2, R0 = svdfact(A, B); # destructuring via iteration

julia> U*D1*R0*Q' ≈ A
true

julia> V*D2*R0*Q' ≈ B
true
```
"""
function svdfact(A::StridedMatrix{TA}, B::StridedMatrix{TB}) where {TA,TB}
    S = promote_type(eigtype(TA),TB)
    return svdfact!(copy_oftype(A, S), copy_oftype(B, S))
end
# This method can be heavily optimized but it is probably not critical
# and might introduce bugs or inconsistencies relative to the 1x1 matrix
# version
svdfact(x::Number, y::Number) = svdfact(fill(x, 1, 1), fill(y, 1, 1))

@inline function getproperty(F::GeneralizedSVD{T}, d::Symbol) where T
    Fa = getfield(F, :a)
    Fb = getfield(F, :b)
    Fk = getfield(F, :k)
    Fl = getfield(F, :l)
    FU = getfield(F, :U)
    FV = getfield(F, :V)
    FQ = getfield(F, :Q)
    FR = getfield(F, :R)
    if d == :alpha
        return Fa
    elseif d == :beta
        return Fb
    elseif d == :vals || d == :S
        return Fa[1:Fk + Fl] ./ Fb[1:Fk + Fl]
    elseif d == :D1
        m = size(FU, 1)
        if m - Fk - Fl >= 0
            return [Matrix{T}(I, Fk, Fk)  zeros(T, Fk, Fl)            ;
                    zeros(T, Fl, Fk)      Diagonal(Fa[Fk + 1:Fk + Fl]);
                    zeros(T, m - Fk - Fl, Fk + Fl)                    ]
        else
            return [Matrix{T}(I, m, Fk) [zeros(T, Fk, m - Fk); Diagonal(Fa[Fk + 1:m])] zeros(T, m, Fk + Fl - m)]
        end
    elseif d == :D2
        m = size(FU, 1)
        p = size(FV, 1)
        if m - Fk - Fl >= 0
            return [zeros(T, Fl, Fk) Diagonal(Fb[Fk + 1:Fk + Fl]); zeros(T, p - Fl, Fk + Fl)]
        else
            return [zeros(T, p, Fk) [Diagonal(Fb[Fk + 1:m]); zeros(T, Fk + p - m, m - Fk)] [zeros(T, m - Fk, Fk + Fl - m); Matrix{T}(I, Fk + p - m, Fk + Fl - m)]]
        end
    elseif d == :R0
        n = size(FQ, 1)
        return [zeros(T, Fk + Fl, n - Fk - Fl) FR]
    else
        getfield(F, d)
    end
end

Base.propertynames(F::GeneralizedSVD) =
    (:alpha, :beta, :vals, :S, :D1, :D2, :R0, fieldnames(typeof(F))...)

"""
    svdvals!(A, B)

Return the generalized singular values from the generalized singular value
decomposition of `A` and `B`, saving space by overwriting `A` and `B`.
See also [`svdfact`](@ref) and [`svdvals`](@ref).

# Examples
```jldoctest
julia> A = [1. 0.; 0. -1.]
2×2 Array{Float64,2}:
 1.0   0.0
 0.0  -1.0

julia> B = [0. 1.; 1. 0.]
2×2 Array{Float64,2}:
 0.0  1.0
 1.0  0.0

julia> svdvals!(A, B)
2-element Array{Float64,1}:
 1.0
 1.0

julia> A
2×2 Array{Float64,2}:
 1.41421   0.0
 0.0      -1.41421

julia> B
2×2 Array{Float64,2}:
 1.0  -0.0
 0.0  -1.0
```
"""
function svdvals!(A::StridedMatrix{T}, B::StridedMatrix{T}) where T<:BlasFloat
    # xggsvd3 replaced xggsvd in LAPACK 3.6.0
    if LAPACK.version() < v"3.6.0"
        _, _, _, a, b, k, l, _ = LAPACK.ggsvd!('N', 'N', 'N', A, B)
    else
        _, _, _, a, b, k, l, _ = LAPACK.ggsvd3!('N', 'N', 'N', A, B)
    end
    a[1:k + l] ./ b[1:k + l]
end
svdvals(A::StridedMatrix{T},B::StridedMatrix{T}) where {T<:BlasFloat} = svdvals!(copy(A),copy(B))

"""
    svdvals(A, B)

Return the generalized singular values from the generalized singular value
decomposition of `A` and `B`. See also [`svdfact`](@ref).

# Examples
```jldoctest
julia> A = [1. 0.; 0. -1.]
2×2 Array{Float64,2}:
 1.0   0.0
 0.0  -1.0

julia> B = [0. 1.; 1. 0.]
2×2 Array{Float64,2}:
 0.0  1.0
 1.0  0.0

julia> svdvals(A, B)
2-element Array{Float64,1}:
 1.0
 1.0
```
"""
function svdvals(A::StridedMatrix{TA}, B::StridedMatrix{TB}) where {TA,TB}
    S = promote_type(eigtype(TA), TB)
    return svdvals!(copy_oftype(A, S), copy_oftype(B, S))
end
svdvals(x::Number, y::Number) = abs(x/y)

# Conversion
AbstractMatrix(F::SVD) = (F.U * Diagonal(F.S)) * F.Vt
AbstractArray(F::SVD) = AbstractMatrix(F)
Matrix(F::SVD) = Array(AbstractArray(F))
Array(F::SVD) = Matrix(F)
