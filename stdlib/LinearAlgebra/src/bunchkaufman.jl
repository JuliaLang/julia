# This file is a part of Julia. License is MIT: https://julialang.org/license

## Create an extractor that extracts the modified original matrix, e.g.
## LD for BunchKaufman, UL for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

"""
    BunchKaufman <: Factorization

Matrix factorization type of the Bunch-Kaufman factorization of a symmetric or
Hermitian matrix `A` as `P'UDU'P` or `P'LDL'P`, depending on whether the upper
(the default) or the lower triangle is stored in `A`. If `A` is complex symmetric
then `U'` and `L'` denote the unconjugated transposes, i.e. `transpose(U)` and
`transpose(L)`, respectively. This is the return type of [`bunchkaufman`](@ref),
the corresponding matrix factorization function.

If `S::BunchKaufman` is the factorization object, the components can be obtained
via `S.D`, `S.U` or `S.L` as appropriate given `S.uplo`, and `S.p`.

Iterating the decomposition produces the components `S.D`, `S.U` or `S.L`
as appropriate given `S.uplo`, and `S.p`.

# Examples
```jldoctest
julia> A = [1 2; 2 3]
2×2 Array{Int64,2}:
 1  2
 2  3

julia> S = bunchkaufman(A) # A gets wrapped internally by Symmetric(A)
BunchKaufman{Float64,Array{Float64,2}}
D factor:
2×2 Tridiagonal{Float64,Array{Float64,1}}:
 -0.333333  0.0
  0.0       3.0
U factor:
2×2 UnitUpperTriangular{Float64,Array{Float64,2}}:
 1.0  0.666667
  ⋅   1.0
permutation:
2-element Array{Int64,1}:
 1
 2

julia> d, u, p = S; # destructuring via iteration

julia> d == S.D && u == S.U && p == S.p
true

julia> S = bunchkaufman(Symmetric(A, :L))
BunchKaufman{Float64,Array{Float64,2}}
D factor:
2×2 Tridiagonal{Float64,Array{Float64,1}}:
 3.0   0.0
 0.0  -0.333333
L factor:
2×2 UnitLowerTriangular{Float64,Array{Float64,2}}:
 1.0        ⋅
 0.666667  1.0
permutation:
2-element Array{Int64,1}:
 2
 1
```
"""
struct BunchKaufman{T,S<:AbstractMatrix} <: Factorization{T}
    LD::S
    ipiv::Vector{BlasInt}
    uplo::Char
    symmetric::Bool
    rook::Bool
    info::BlasInt

    function BunchKaufman{T,S}(LD, ipiv, uplo, symmetric, rook, info) where {T,S<:AbstractMatrix}
        require_one_based_indexing(LD)
        new(LD, ipiv, uplo, symmetric, rook, info)
    end
end
BunchKaufman(A::AbstractMatrix{T}, ipiv::Vector{BlasInt}, uplo::AbstractChar, symmetric::Bool,
             rook::Bool, info::BlasInt) where {T} =
        BunchKaufman{T,typeof(A)}(A, ipiv, uplo, symmetric, rook, info)

# iteration for destructuring into components
Base.iterate(S::BunchKaufman) = (S.D, Val(:UL))
Base.iterate(S::BunchKaufman, ::Val{:UL}) = (S.uplo == 'L' ? S.L : S.U, Val(:p))
Base.iterate(S::BunchKaufman, ::Val{:p}) = (S.p, Val(:done))
Base.iterate(S::BunchKaufman, ::Val{:done}) = nothing


"""
    bunchkaufman!(A, rook::Bool=false; check = true) -> BunchKaufman

`bunchkaufman!` is the same as [`bunchkaufman`](@ref), but saves space by overwriting the
input `A`, instead of creating a copy.
"""
function bunchkaufman!(A::RealHermSymComplexSym{T,S} where {T<:BlasReal,S<:StridedMatrix},
                       rook::Bool = false; check::Bool = true)
    LD, ipiv, info = rook ? LAPACK.sytrf_rook!(A.uplo, A.data) : LAPACK.sytrf!(A.uplo, A.data)
    check && checknonsingular(info)
    BunchKaufman(LD, ipiv, A.uplo, true, rook, info)
end
function bunchkaufman!(A::Hermitian{T,S} where {T<:BlasComplex,S<:StridedMatrix{T}},
                       rook::Bool = false; check::Bool = true)
    LD, ipiv, info = rook ? LAPACK.hetrf_rook!(A.uplo, A.data) : LAPACK.hetrf!(A.uplo, A.data)
    check && checknonsingular(info)
    BunchKaufman(LD, ipiv, A.uplo, false, rook, info)
end
function bunchkaufman!(A::StridedMatrix{<:BlasFloat}, rook::Bool = false; check::Bool = true)
    if ishermitian(A)
        return bunchkaufman!(Hermitian(A), rook; check = check)
    elseif issymmetric(A)
        return bunchkaufman!(Symmetric(A), rook; check = check)
    else
        throw(ArgumentError("Bunch-Kaufman decomposition is only valid for symmetric or Hermitian matrices"))
    end
end

"""
    bunchkaufman(A, rook::Bool=false; check = true) -> S::BunchKaufman

Compute the Bunch-Kaufman [^Bunch1977] factorization of a symmetric or
Hermitian matrix `A` as `P'*U*D*U'*P` or `P'*L*D*L'*P`, depending on
which triangle is stored in `A`, and return a [`BunchKaufman`](@ref) object.
Note that if `A` is complex symmetric then `U'` and `L'` denote
the unconjugated transposes, i.e. `transpose(U)` and `transpose(L)`.

Iterating the decomposition produces the components `S.D`, `S.U` or `S.L`
as appropriate given `S.uplo`, and `S.p`.

If `rook` is `true`, rook pivoting is used. If `rook` is false,
rook pivoting is not used.

When `check = true`, an error is thrown if the decomposition fails.
When `check = false`, responsibility for checking the decomposition's
validity (via [`issuccess`](@ref)) lies with the user.

The following functions are available for `BunchKaufman` objects:
[`size`](@ref), `\\`, [`inv`](@ref), [`issymmetric`](@ref),
[`ishermitian`](@ref), [`getindex`](@ref).

[^Bunch1977]: J R Bunch and L Kaufman, Some stable methods for calculating inertia and solving symmetric linear systems, Mathematics of Computation 31:137 (1977), 163-179. [url](http://www.ams.org/journals/mcom/1977-31-137/S0025-5718-1977-0428694-0/).

# Examples
```jldoctest
julia> A = [1 2; 2 3]
2×2 Array{Int64,2}:
 1  2
 2  3

julia> S = bunchkaufman(A) # A gets wrapped internally by Symmetric(A)
BunchKaufman{Float64,Array{Float64,2}}
D factor:
2×2 Tridiagonal{Float64,Array{Float64,1}}:
 -0.333333  0.0
  0.0       3.0
U factor:
2×2 UnitUpperTriangular{Float64,Array{Float64,2}}:
 1.0  0.666667
  ⋅   1.0
permutation:
2-element Array{Int64,1}:
 1
 2

julia> d, u, p = S; # destructuring via iteration

julia> d == S.D && u == S.U && p == S.p
true

julia> S = bunchkaufman(Symmetric(A, :L))
BunchKaufman{Float64,Array{Float64,2}}
D factor:
2×2 Tridiagonal{Float64,Array{Float64,1}}:
 3.0   0.0
 0.0  -0.333333
L factor:
2×2 UnitLowerTriangular{Float64,Array{Float64,2}}:
 1.0        ⋅
 0.666667  1.0
permutation:
2-element Array{Int64,1}:
 2
 1
```
"""
bunchkaufman(A::AbstractMatrix{T}, rook::Bool=false; check::Bool = true) where {T} =
    bunchkaufman!(copy_oftype(A, typeof(sqrt(oneunit(T)))), rook; check = check)

convert(::Type{BunchKaufman{T}}, B::BunchKaufman{T}) where {T} = B
convert(::Type{BunchKaufman{T}}, B::BunchKaufman) where {T} =
    BunchKaufman(convert(Matrix{T}, B.LD), B.ipiv, B.uplo, B.symmetric, B.rook, B.info)
convert(::Type{Factorization{T}}, B::BunchKaufman{T}) where {T} = B
convert(::Type{Factorization{T}}, B::BunchKaufman) where {T} = convert(BunchKaufman{T}, B)

size(B::BunchKaufman) = size(getfield(B, :LD))
size(B::BunchKaufman, d::Integer) = size(getfield(B, :LD), d)
issymmetric(B::BunchKaufman) = B.symmetric
ishermitian(B::BunchKaufman) = !B.symmetric

function _ipiv2perm_bk(v::AbstractVector{T}, maxi::Integer, uplo::AbstractChar, rook::Bool) where T
    require_one_based_indexing(v)
    p = T[1:maxi;]
    uploL = uplo == 'L'
    i = uploL ? 1 : maxi
    # if uplo == 'U' we construct the permutation backwards
    @inbounds while 1 <= i <= length(v)
        vi = v[i]
        if vi > 0 # the 1x1 blocks
            p[i], p[vi] = p[vi], p[i]
            i += uploL ? 1 : -1
        else # the 2x2 blocks
            if rook
                p[i], p[-vi] = p[-vi], p[i]
            end
            if uploL
                vp = rook ? -v[i+1] : -vi
                p[i + 1], p[vp] = p[vp], p[i + 1]
                i += 2
            else # 'U'
                vp = rook ? -v[i-1] : -vi
                p[i - 1], p[vp] = p[vp], p[i - 1]
                i -= 2
            end
        end
    end
    return p
end

"""
    getproperty(B::BunchKaufman, d::Symbol)

Extract the factors of the Bunch-Kaufman factorization `B`. The factorization can take the
two forms `P'*L*D*L'*P` or `P'*U*D*U'*P` (or `L*D*transpose(L)` in the complex symmetric case)
where `P` is a (symmetric) permutation matrix, `L` is a [`UnitLowerTriangular`](@ref) matrix, `U` is a
[`UnitUpperTriangular`](@ref), and `D` is a block diagonal symmetric or Hermitian matrix with
1x1 or 2x2 blocks. The argument `d` can be

- `:D`: the block diagonal matrix
- `:U`: the upper triangular factor (if factorization is `U*D*U'`)
- `:L`: the lower triangular factor (if factorization is `L*D*L'`)
- `:p`: permutation vector
- `:P`: permutation matrix

# Examples
```jldoctest
julia> A = [1 2 3; 2 1 2; 3 2 1]
3×3 Array{Int64,2}:
 1  2  3
 2  1  2
 3  2  1

julia> F = bunchkaufman(Symmetric(A, :L))
BunchKaufman{Float64,Array{Float64,2}}
D factor:
3×3 Tridiagonal{Float64,Array{Float64,1}}:
 1.0  3.0    ⋅
 3.0  1.0   0.0
  ⋅   0.0  -1.0
L factor:
3×3 UnitLowerTriangular{Float64,Array{Float64,2}}:
 1.0   ⋅    ⋅
 0.0  1.0   ⋅
 0.5  0.5  1.0
permutation:
3-element Array{Int64,1}:
 1
 3
 2

julia> F.L*F.D*F.L' - A[F.p, F.p]
3×3 Array{Float64,2}:
 0.0  0.0  0.0
 0.0  0.0  0.0
 0.0  0.0  0.0

julia> F = bunchkaufman(Symmetric(A));

julia> F.U*F.D*F.U' - F.P*A*F.P'
3×3 Array{Float64,2}:
 0.0  0.0  0.0
 0.0  0.0  0.0
 0.0  0.0  0.0
```
"""
function getproperty(B::BunchKaufman{T}, d::Symbol) where {T<:BlasFloat}
    n = size(B, 1)
    if d === :p
        return _ipiv2perm_bk(getfield(B, :ipiv), n, getfield(B, :uplo), B.rook)
    elseif d === :P
        return Matrix{T}(I, n, n)[:,invperm(B.p)]
    elseif d === :L || d === :U || d === :D
        if getfield(B, :rook)
            LUD, od = LAPACK.syconvf_rook!(getfield(B, :uplo), 'C', copy(getfield(B, :LD)), getfield(B, :ipiv))
        else
            LUD, od = LAPACK.syconv!(getfield(B, :uplo), copy(getfield(B, :LD)), getfield(B, :ipiv))
        end
        if d === :D
            if getfield(B, :uplo) == 'L'
                odl = od[1:n - 1]
                return Tridiagonal(odl, diag(LUD), getfield(B, :symmetric) ? odl : conj.(odl))
            else # 'U'
                odu = od[2:n]
                return Tridiagonal(getfield(B, :symmetric) ? odu : conj.(odu), diag(LUD), odu)
            end
        elseif d === :L
            if getfield(B, :uplo) == 'L'
                return UnitLowerTriangular(LUD)
            else
                throw(ArgumentError("factorization is U*D*transpose(U) but you requested L"))
            end
        else # :U
            if B.uplo == 'U'
                return UnitUpperTriangular(LUD)
            else
                throw(ArgumentError("factorization is L*D*transpose(L) but you requested U"))
            end
        end
    else
        getfield(B, d)
    end
end

Base.propertynames(B::BunchKaufman, private::Bool=false) =
    (:p, :P, :L, :U, :D, (private ? fieldnames(typeof(B)) : ())...)

issuccess(B::BunchKaufman) = B.info == 0

function Base.show(io::IO, mime::MIME{Symbol("text/plain")}, B::BunchKaufman)
    if issuccess(B)
        summary(io, B); println(io)
        println(io, "D factor:")
        show(io, mime, B.D)
        println(io, "\n$(B.uplo) factor:")
        show(io, mime, B.uplo == 'L' ? B.L : B.U)
        println(io, "\npermutation:")
        show(io, mime, B.p)
    else
        print(io, "Failed factorization of type $(typeof(B))")
    end
end

function inv(B::BunchKaufman{<:BlasReal})
    if B.rook
        copytri!(LAPACK.sytri_rook!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
    else
        copytri!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
    end
end

function inv(B::BunchKaufman{<:BlasComplex})
    if issymmetric(B)
        if B.rook
            copytri!(LAPACK.sytri_rook!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
        else
            copytri!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
        end
    else
        if B.rook
            copytri!(LAPACK.hetri_rook!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
        else
            copytri!(LAPACK.hetri!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
        end
    end
end

function ldiv!(B::BunchKaufman{T}, R::StridedVecOrMat{T}) where T<:BlasReal
    if B.rook
        LAPACK.sytrs_rook!(B.uplo, B.LD, B.ipiv, R)
    else
        LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, R)
    end
end
function ldiv!(B::BunchKaufman{T}, R::StridedVecOrMat{T}) where T<:BlasComplex
    if B.rook
        if issymmetric(B)
            LAPACK.sytrs_rook!(B.uplo, B.LD, B.ipiv, R)
        else
            LAPACK.hetrs_rook!(B.uplo, B.LD, B.ipiv, R)
        end
    else
        if issymmetric(B)
            LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, R)
        else
            LAPACK.hetrs!(B.uplo, B.LD, B.ipiv, R)
        end
    end
end
# There is no fallback solver for Bunch-Kaufman so we'll have to promote to same element type
function ldiv!(B::BunchKaufman{T}, R::StridedVecOrMat{S}) where {T,S}
    TS = promote_type(T,S)
    return ldiv!(convert(BunchKaufman{TS}, B), convert(AbstractArray{TS}, R))
end

function logabsdet(F::BunchKaufman)
    M = F.LD
    p = F.ipiv
    n = size(F.LD, 1)

    if !issuccess(F)
        return eltype(F)(-Inf), zero(eltype(F))
    end
    s = one(real(eltype(F)))
    i = 1
    abs_det = zero(real(eltype(F)))
    while i <= n
        if p[i] > 0
            elm = M[i,i]
            s *= sign(elm)
            abs_det += log(abs(elm))
            i += 1
        else
            # 2x2 pivot case. Make sure not to square before the subtraction by scaling
            # with the off-diagonal element. This is safe because the off diagonal is
            # always large for 2x2 pivots.
            if F.uplo == 'U'
                elm = M[i, i + 1]*(M[i,i]/M[i, i + 1]*M[i + 1, i + 1] -
                    (issymmetric(F) ? M[i, i + 1] : conj(M[i, i + 1])))
                s *= sign(elm)
                abs_det += log(abs(elm))
            else
                elm = M[i + 1,i]*(M[i, i]/M[i + 1, i]*M[i + 1, i + 1] -
                    (issymmetric(F) ? M[i + 1, i] : conj(M[i + 1, i])))
                s *= sign(elm)
                abs_det += log(abs(elm))
            end
            i += 2
        end
    end
    return abs_det, s
end

## reconstruct the original matrix
## TODO: understand the procedure described at
## http://www.nag.com/numeric/FL/nagdoc_fl22/pdf/F07/f07mdf.pdf
