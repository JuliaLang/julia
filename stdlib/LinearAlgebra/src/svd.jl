# This file is a part of Julia. License is MIT: https://julialang.org/license

# Singular Value Decomposition
"""
    SVD <: Factorization

Matrix factorization type of the singular value decomposition (SVD) of a matrix `A`.
This is the return type of [`svd(_)`](@ref), the corresponding matrix factorization function.

If `F::SVD` is the factorization object, `U`, `S`, `V` and `Vt` can be obtained
via `F.U`, `F.S`, `F.V` and `F.Vt`, such that `A = U * Diagonal(S) * Vt`.
The singular values in `S` are sorted in descending order.

Iterating the decomposition produces the components `U`, `S`, and `V`.

# Examples
```jldoctest
julia> A = [1. 0. 0. 0. 2.; 0. 0. 3. 0. 0.; 0. 0. 0. 0. 0.; 0. 2. 0. 0. 0.]
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0

julia> F = svd(A)
SVD{Float64,Float64,Array{Float64,2}}
U factor:
4×4 Array{Float64,2}:
 0.0  1.0  0.0   0.0
 1.0  0.0  0.0   0.0
 0.0  0.0  0.0  -1.0
 0.0  0.0  1.0   0.0
singular values:
4-element Array{Float64,1}:
 3.0
 2.23606797749979
 2.0
 0.0
Vt factor:
4×5 Array{Float64,2}:
 -0.0       0.0  1.0  -0.0  0.0
  0.447214  0.0  0.0   0.0  0.894427
 -0.0       1.0  0.0  -0.0  0.0
  0.0       0.0  0.0   1.0  0.0

julia> F.U * Diagonal(F.S) * F.Vt
4×5 Array{Float64,2}:
 1.0  0.0  0.0  0.0  2.0
 0.0  0.0  3.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  2.0  0.0  0.0  0.0

julia> u, s, v = F; # destructuring via iteration

julia> u == F.U && s == F.S && v == F.V
true
```
"""
struct SVD{T,Tr,M<:AbstractArray{T}} <: Factorization{T}
    U::M
    S::Vector{Tr}
    Vt::M
    function SVD{T,Tr,M}(U, S, Vt) where {T,Tr,M<:AbstractArray{T}}
        require_one_based_indexing(U, S, Vt)
        new{T,Tr,M}(U, S, Vt)
    end
end
SVD(U::AbstractArray{T}, S::Vector{Tr}, Vt::AbstractArray{T}) where {T,Tr} = SVD{T,Tr,typeof(U)}(U, S, Vt)
function SVD{T}(U::AbstractArray, S::AbstractVector{Tr}, Vt::AbstractArray) where {T,Tr}
    SVD(convert(AbstractArray{T}, U),
        convert(Vector{Tr}, S),
        convert(AbstractArray{T}, Vt))
end


# iteration for destructuring into components
Base.iterate(S::SVD) = (S.U, Val(:S))
Base.iterate(S::SVD, ::Val{:S}) = (S.S, Val(:V))
Base.iterate(S::SVD, ::Val{:V}) = (S.V, Val(:done))
Base.iterate(S::SVD, ::Val{:done}) = nothing


default_svd_alg(A) = DivideAndConquer()


"""
    svd!(A; full::Bool = false, alg::Algorithm = default_svd_alg(A)) -> SVD

`svd!` is the same as [`svd`](@ref), but saves space by
overwriting the input `A`, instead of creating a copy. See documentation of [`svd`](@ref) for details.
```
"""
function svd!(A::AbstractMatrix{T}; full::Bool = false, alg::Algorithm = default_svd_alg(A)) where T<:BlasFloat
    m,n = size(A)
    if m == 0 || n == 0
        u,s,vt = (Matrix{T}(I, m, full ? m : n), real(zeros(T,0)), Matrix{T}(I, n, n))
    else
        u,s,vt = _svd!(A,full,alg)
    end
    SVD(u,s,vt)
end


_svd!(A::StridedMatrix{T}, full::Bool, alg::Algorithm) where T<:BlasFloat = throw(ArgumentError("Unsupported value for `alg` keyword."))
_svd!(A::StridedMatrix{T}, full::Bool, alg::DivideAndConquer) where T<:BlasFloat = LAPACK.gesdd!(full ? 'A' : 'S', A)
function _svd!(A::StridedMatrix{T}, full::Bool, alg::QRIteration) where T<:BlasFloat
    c = full ? 'A' : 'S'
    u,s,vt = LAPACK.gesvd!(c, c, A)
end



"""
    svd(A; full::Bool = false, alg::Algorithm = default_svd_alg(A)) -> SVD

Compute the singular value decomposition (SVD) of `A` and return an `SVD` object.

`U`, `S`, `V` and `Vt` can be obtained from the factorization `F` with `F.U`,
`F.S`, `F.V` and `F.Vt`, such that `A = U * Diagonal(S) * Vt`.
The algorithm produces `Vt` and hence `Vt` is more efficient to extract than `V`.
The singular values in `S` are sorted in descending order.

Iterating the decomposition produces the components `U`, `S`, and `V`.

If `full = false` (default), a "thin" SVD is returned. For a ``M
\\times N`` matrix `A`, in the full factorization `U` is `M \\times M`
and `V` is `N \\times N`, while in the thin factorization `U` is `M
\\times K` and `V` is `N \\times K`, where `K = \\min(M,N)` is the
number of singular values.

If `alg = DivideAndConquer()` a divide-and-conquer algorithm is used to calculate the SVD.
Another (typically slower but more accurate) option is `alg = QRIteration()`.

!!! compat "Julia 1.3"
    The `alg` keyword argument requires Julia 1.3 or later.

# Examples
```jldoctest
julia> A = rand(4,3);

julia> F = svd(A); # Store the Factorization Object

julia> A ≈ F.U * Diagonal(F.S) * F.Vt
true

julia> U, S, V = F; # destructuring via iteration

julia> A ≈ U * Diagonal(S) * V'
true

julia> Uonly, = svd(A); # Store U only

julia> Uonly == U
true
```
"""
function svd(A::AbstractVecOrMat{T}; full::Bool = false, alg::Algorithm = default_svd_alg(A)) where T
    svd!(copy_oftype(A, eigtype(T)), full = full, alg = alg)
end
function svd(x::Number; full::Bool = false, alg::Algorithm = default_svd_alg(x))
    SVD(x == 0 ? fill(one(x), 1, 1) : fill(x/abs(x), 1, 1), [abs(x)], fill(one(x), 1, 1))
end
function svd(x::Integer; full::Bool = false, alg::Algorithm = default_svd_alg(x))
    svd(float(x), full = full, alg = alg)
end
function svd(A::Adjoint; full::Bool = false, alg::Algorithm = default_svd_alg(A))
    s = svd(A.parent, full = full, alg = alg)
    return SVD(s.Vt', s.S, s.U')
end
function svd(A::Transpose; full::Bool = false, alg::Algorithm = default_svd_alg(A))
    s = svd(A.parent, full = full, alg = alg)
    return SVD(transpose(s.Vt), s.S, transpose(s.U))
end

function getproperty(F::SVD, d::Symbol)
    if d === :V
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
See also [`svdvals`](@ref) and [`svd`](@ref).
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
function ldiv!(A::SVD{T}, B::AbstractVecOrMat) where T
    k = searchsortedlast(A.S, eps(real(T))*A.S[1], rev=true)
    view(A.Vt,1:k,:)' * (view(A.S,1:k) .\ (view(A.U,:,1:k)' * B))
end

function inv(F::SVD{T}) where T
    @inbounds for i in eachindex(F.S)
        iszero(F.S[i]) && throw(SingularException(i))
    end
    k = searchsortedlast(F.S, eps(real(T))*F.S[1], rev=true)
    @views (F.S[1:k] .\ F.Vt[1:k, :])' * F.U[:,1:k]'
end

size(A::SVD, dim::Integer) = dim == 1 ? size(A.U, dim) : size(A.Vt, dim)
size(A::SVD) = (size(A, 1), size(A, 2))

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::SVD{<:Any,<:Any,<:AbstractArray})
    summary(io, F); println(io)
    println(io, "U factor:")
    show(io, mime, F.U)
    println(io, "\nsingular values:")
    show(io, mime, F.S)
    println(io, "\nVt factor:")
    show(io, mime, F.Vt)
end

# Generalized svd
"""
    GeneralizedSVD <: Factorization

Matrix factorization type of the generalized singular value decomposition (SVD)
of two matrices `A` and `B`, such that `A = F.U*F.D1*F.R0*F.Q'` and
`B = F.V*F.D2*F.R0*F.Q'`. This is the return type of [`svd(_, _)`](@ref), the
corresponding matrix factorization function.

For an M-by-N matrix `A` and P-by-N matrix `B`,

- `U` is a M-by-M orthogonal matrix,
- `V` is a P-by-P orthogonal matrix,
- `Q` is a N-by-N orthogonal matrix,
- `D1` is a M-by-(K+L) diagonal matrix with 1s in the first K entries,
- `D2` is a P-by-(K+L) matrix whose top right L-by-L block is diagonal,
- `R0` is a (K+L)-by-N matrix whose rightmost (K+L)-by-(K+L) block is
           nonsingular upper block triangular,

`K+L` is the effective numerical rank of the matrix `[A; B]`.

Iterating the decomposition produces the components `U`, `V`, `Q`, `D1`, `D2`, and `R0`.

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

julia> F = svd(A, B)
GeneralizedSVD{Float64,Array{Float64,2}}
U factor:
2×2 Array{Float64,2}:
 1.0  0.0
 0.0  1.0
V factor:
2×2 Array{Float64,2}:
 -0.0  -1.0
  1.0   0.0
Q factor:
2×2 Array{Float64,2}:
 1.0  0.0
 0.0  1.0
D1 factor:
2×2 SparseArrays.SparseMatrixCSC{Float64,Int64} with 2 stored entries:
  [1, 1]  =  0.707107
  [2, 2]  =  0.707107
D2 factor:
2×2 SparseArrays.SparseMatrixCSC{Float64,Int64} with 2 stored entries:
  [1, 1]  =  0.707107
  [2, 2]  =  0.707107
R0 factor:
2×2 Array{Float64,2}:
 1.41421   0.0
 0.0      -1.41421

julia> F.U*F.D1*F.R0*F.Q'
2×2 Array{Float64,2}:
 1.0   0.0
 0.0  -1.0

julia> F.V*F.D2*F.R0*F.Q'
2×2 Array{Float64,2}:
 0.0  1.0
 1.0  0.0
```
"""
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

# iteration for destructuring into components
Base.iterate(S::GeneralizedSVD) = (S.U, Val(:V))
Base.iterate(S::GeneralizedSVD, ::Val{:V}) = (S.V, Val(:Q))
Base.iterate(S::GeneralizedSVD, ::Val{:Q}) = (S.Q, Val(:D1))
Base.iterate(S::GeneralizedSVD, ::Val{:D1}) = (S.D1, Val(:D2))
Base.iterate(S::GeneralizedSVD, ::Val{:D2}) = (S.D2, Val(:R0))
Base.iterate(S::GeneralizedSVD, ::Val{:R0}) = (S.R0, Val(:done))
Base.iterate(S::GeneralizedSVD, ::Val{:done}) = nothing

"""
    svd!(A, B) -> GeneralizedSVD

`svd!` is the same as [`svd`](@ref), but modifies the arguments
`A` and `B` in-place, instead of making copies. See documentation of [`svd`](@ref) for details.
```
"""
function svd!(A::StridedMatrix{T}, B::StridedMatrix{T}) where T<:BlasFloat
    # xggsvd3 replaced xggsvd in LAPACK 3.6.0
    if LAPACK.version() < v"3.6.0"
        U, V, Q, a, b, k, l, R = LAPACK.ggsvd!('U', 'V', 'Q', A, B)
    else
        U, V, Q, a, b, k, l, R = LAPACK.ggsvd3!('U', 'V', 'Q', A, B)
    end
    GeneralizedSVD(U, V, Q, a, b, Int(k), Int(l), R)
end
svd(A::AbstractMatrix{T}, B::AbstractMatrix{T}) where {T<:BlasFloat} = svd!(copy(A),copy(B))

"""

    svd(A, B) -> GeneralizedSVD

Compute the generalized SVD of `A` and `B`, returning a `GeneralizedSVD` factorization
object `F` such that `[A;B] = [F.U * F.D1; F.V * F.D2] * F.R0 * F.Q'`

- `U` is a M-by-M orthogonal matrix,
- `V` is a P-by-P orthogonal matrix,
- `Q` is a N-by-N orthogonal matrix,
- `D1` is a M-by-(K+L) diagonal matrix with 1s in the first K entries,
- `D2` is a P-by-(K+L) matrix whose top right L-by-L block is diagonal,
- `R0` is a (K+L)-by-N matrix whose rightmost (K+L)-by-(K+L) block is
           nonsingular upper block triangular,

`K+L` is the effective numerical rank of the matrix `[A; B]`.

Iterating the decomposition produces the components `U`, `V`, `Q`, `D1`, `D2`, and `R0`.

The generalized SVD is used in applications such as when one wants to compare how much belongs
to `A` vs. how much belongs to `B`, as in human vs yeast genome, or signal vs noise, or between
clusters vs within clusters. (See Edelman and Wang for discussion: https://arxiv.org/abs/1901.00485)

It decomposes `[A; B]` into `[UC; VS]H`, where `[UC; VS]` is a natural orthogonal basis for the
column space of `[A; B]`, and `H = RQ'` is a natural non-orthogonal basis for the rowspace of `[A;B]`,
where the top rows are most closely attributed to the `A` matrix, and the bottom to the `B` matrix.
The multi-cosine/sine matrices `C` and `S` provide a multi-measure of how much `A` vs how much `B`,
and `U` and `V` provide directions in which these are measured.

# Examples
```jldoctest
julia> A = randn(3,2); B=randn(4,2);

julia> F = svd(A, B);

julia> U,V,Q,C,S,R = F;

julia> H = R*Q';

julia> [A; B] ≈ [U*C; V*S]*H
true

julia> [A; B] ≈ [F.U*F.D1; F.V*F.D2]*F.R0*F.Q'
true

julia> Uonly, = svd(A,B);

julia> U == Uonly
true
```
"""
function svd(A::AbstractMatrix{TA}, B::AbstractMatrix{TB}) where {TA,TB}
    S = promote_type(eigtype(TA),TB)
    return svd!(copy_oftype(A, S), copy_oftype(B, S))
end
# This method can be heavily optimized but it is probably not critical
# and might introduce bugs or inconsistencies relative to the 1x1 matrix
# version
svd(x::Number, y::Number) = svd(fill(x, 1, 1), fill(y, 1, 1))

@inline function getproperty(F::GeneralizedSVD{T}, d::Symbol) where T
    Fa = getfield(F, :a)
    Fb = getfield(F, :b)
    Fk = getfield(F, :k)
    Fl = getfield(F, :l)
    FU = getfield(F, :U)
    FV = getfield(F, :V)
    FQ = getfield(F, :Q)
    FR = getfield(F, :R)
    if d === :alpha
        return Fa
    elseif d === :beta
        return Fb
    elseif d === :vals || d === :S
        return Fa[1:Fk + Fl] ./ Fb[1:Fk + Fl]
    elseif d === :D1
        m = size(FU, 1)
        if m - Fk - Fl >= 0
            return [Matrix{T}(I, Fk, Fk)  zeros(T, Fk, Fl)            ;
                    zeros(T, Fl, Fk)      Diagonal(Fa[Fk + 1:Fk + Fl]);
                    zeros(T, m - Fk - Fl, Fk + Fl)                    ]
        else
            return [Matrix{T}(I, m, Fk) [zeros(T, Fk, m - Fk); Diagonal(Fa[Fk + 1:m])] zeros(T, m, Fk + Fl - m)]
        end
    elseif d === :D2
        m = size(FU, 1)
        p = size(FV, 1)
        if m - Fk - Fl >= 0
            return [zeros(T, Fl, Fk) Diagonal(Fb[Fk + 1:Fk + Fl]); zeros(T, p - Fl, Fk + Fl)]
        else
            return [zeros(T, p, Fk) [Diagonal(Fb[Fk + 1:m]); zeros(T, Fk + p - m, m - Fk)] [zeros(T, m - Fk, Fk + Fl - m); Matrix{T}(I, Fk + p - m, Fk + Fl - m)]]
        end
    elseif d === :R0
        n = size(FQ, 1)
        return [zeros(T, Fk + Fl, n - Fk - Fl) FR]
    else
        getfield(F, d)
    end
end

Base.propertynames(F::GeneralizedSVD) =
    (:alpha, :beta, :vals, :S, :D1, :D2, :R0, fieldnames(typeof(F))...)

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::GeneralizedSVD{<:Any,<:AbstractArray})
    summary(io, F); println(io)
    println(io, "U factor:")
    show(io, mime, F.U)
    println(io, "\nV factor:")
    show(io, mime, F.V)
    println(io, "\nQ factor:")
    show(io, mime, F.Q)
    println(io, "\nD1 factor:")
    show(io, mime, F.D1)
    println(io, "\nD2 factor:")
    show(io, mime, F.D2)
    println(io, "\nR0 factor:")
    show(io, mime, F.R0)
end

"""
    svdvals!(A, B)

Return the generalized singular values from the generalized singular value
decomposition of `A` and `B`, saving space by overwriting `A` and `B`.
See also [`svd`](@ref) and [`svdvals`](@ref).
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
svdvals(A::AbstractMatrix{T}, B::AbstractMatrix{T}) where {T} = svdvals!(copy(A),copy(B))

"""
    svdvals(A, B)

Return the generalized singular values from the generalized singular value
decomposition of `A` and `B`. See also [`svd`](@ref).

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
function svdvals(A::AbstractMatrix{TA}, B::AbstractMatrix{TB}) where {TA,TB}
    S = promote_type(eigtype(TA), TB)
    return svdvals!(copy_oftype(A, S), copy_oftype(B, S))
end
svdvals(x::Number, y::Number) = abs(x/y)

# Conversion
AbstractMatrix(F::SVD) = (F.U * Diagonal(F.S)) * F.Vt
AbstractArray(F::SVD) = AbstractMatrix(F)
Matrix(F::SVD) = Array(AbstractArray(F))
Array(F::SVD) = Matrix(F)
