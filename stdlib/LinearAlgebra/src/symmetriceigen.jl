# This file is a part of Julia. License is MIT: https://julialang.org/license

# Eigensolvers for symmetric and Hermitian matrices
eigen!(A::RealHermSymComplexHerm{<:BlasReal,<:StridedMatrix}; sortby::Union{Function,Nothing}=nothing) = Eigen(sorteig!(LAPACK.syevr!('V', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)..., sortby)...)

function eigen(A::RealHermSymComplexHerm; sortby::Union{Function,Nothing}=nothing)
    T = eltype(A)
    S = eigtype(T)
    eigen!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), sortby=sortby)
end

eigen!(A::RealHermSymComplexHerm{<:BlasReal,<:StridedMatrix}, irange::UnitRange) = Eigen(LAPACK.syevr!('V', 'I', A.uplo, A.data, 0.0, 0.0, irange.start, irange.stop, -1.0)...)

"""
    eigen(A::Union{SymTridiagonal, Hermitian, Symmetric}, irange::UnitRange) -> Eigen

Computes the eigenvalue decomposition of `A`, returning an [`Eigen`](@ref) factorization object `F`
which contains the eigenvalues in `F.values` and the eigenvectors in the columns of the
matrix `F.vectors`. (The `k`th eigenvector can be obtained from the slice `F.vectors[:, k]`.)

Iterating the decomposition produces the components `F.values` and `F.vectors`.

The following functions are available for `Eigen` objects: [`inv`](@ref), [`det`](@ref), and [`isposdef`](@ref).

The [`UnitRange`](@ref) `irange` specifies indices of the sorted eigenvalues to search for.

!!! note
    If `irange` is not `1:n`, where `n` is the dimension of `A`, then the returned factorization
    will be a *truncated* factorization.
"""
function eigen(A::RealHermSymComplexHerm, irange::UnitRange)
    T = eltype(A)
    S = eigtype(T)
    eigen!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), irange)
end

eigen!(A::RealHermSymComplexHerm{T,<:StridedMatrix}, vl::Real, vh::Real) where {T<:BlasReal} =
    Eigen(LAPACK.syevr!('V', 'V', A.uplo, A.data, convert(T, vl), convert(T, vh), 0, 0, -1.0)...)

"""
    eigen(A::Union{SymTridiagonal, Hermitian, Symmetric}, vl::Real, vu::Real) -> Eigen

Computes the eigenvalue decomposition of `A`, returning an [`Eigen`](@ref) factorization object `F`
which contains the eigenvalues in `F.values` and the eigenvectors in the columns of the
matrix `F.vectors`. (The `k`th eigenvector can be obtained from the slice `F.vectors[:, k]`.)

Iterating the decomposition produces the components `F.values` and `F.vectors`.

The following functions are available for `Eigen` objects: [`inv`](@ref), [`det`](@ref), and [`isposdef`](@ref).

`vl` is the lower bound of the window of eigenvalues to search for, and `vu` is the upper bound.

!!! note
    If [`vl`, `vu`] does not contain all eigenvalues of `A`, then the returned factorization
    will be a *truncated* factorization.
"""
function eigen(A::RealHermSymComplexHerm, vl::Real, vh::Real)
    T = eltype(A)
    S = eigtype(T)
    eigen!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), vl, vh)
end

eigvals!(A::RealHermSymComplexHerm{<:BlasReal,<:StridedMatrix}) =
    LAPACK.syevr!('N', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)[1]

function eigvals(A::RealHermSymComplexHerm)
    T = eltype(A)
    S = eigtype(T)
    eigvals!(S != T ? convert(AbstractMatrix{S}, A) : copy(A))
end

"""
    eigvals!(A::Union{SymTridiagonal, Hermitian, Symmetric}, irange::UnitRange) -> values

Same as [`eigvals`](@ref), but saves space by overwriting the input `A`, instead of creating a copy.
`irange` is a range of eigenvalue *indices* to search for - for instance, the 2nd to 8th eigenvalues.
"""
eigvals!(A::RealHermSymComplexHerm{<:BlasReal,<:StridedMatrix}, irange::UnitRange) =
    LAPACK.syevr!('N', 'I', A.uplo, A.data, 0.0, 0.0, irange.start, irange.stop, -1.0)[1]

"""
    eigvals(A::Union{SymTridiagonal, Hermitian, Symmetric}, irange::UnitRange) -> values

Returns the eigenvalues of `A`. It is possible to calculate only a subset of the
eigenvalues by specifying a [`UnitRange`](@ref) `irange` covering indices of the sorted eigenvalues,
e.g. the 2nd to 8th eigenvalues.

# Examples
```jldoctest
julia> A = SymTridiagonal([1.; 2.; 1.], [2.; 3.])
3×3 SymTridiagonal{Float64, Vector{Float64}}:
 1.0  2.0   ⋅
 2.0  2.0  3.0
  ⋅   3.0  1.0

julia> eigvals(A, 2:2)
1-element Vector{Float64}:
 0.9999999999999996

julia> eigvals(A)
3-element Vector{Float64}:
 -2.1400549446402604
  1.0000000000000002
  5.140054944640259
```
"""
function eigvals(A::RealHermSymComplexHerm, irange::UnitRange)
    T = eltype(A)
    S = eigtype(T)
    eigvals!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), irange)
end

"""
    eigvals!(A::Union{SymTridiagonal, Hermitian, Symmetric}, vl::Real, vu::Real) -> values

Same as [`eigvals`](@ref), but saves space by overwriting the input `A`, instead of creating a copy.
`vl` is the lower bound of the interval to search for eigenvalues, and `vu` is the upper bound.
"""
eigvals!(A::RealHermSymComplexHerm{T,<:StridedMatrix}, vl::Real, vh::Real) where {T<:BlasReal} =
    LAPACK.syevr!('N', 'V', A.uplo, A.data, convert(T, vl), convert(T, vh), 0, 0, -1.0)[1]

"""
    eigvals(A::Union{SymTridiagonal, Hermitian, Symmetric}, vl::Real, vu::Real) -> values

Returns the eigenvalues of `A`. It is possible to calculate only a subset of the eigenvalues
by specifying a pair `vl` and `vu` for the lower and upper boundaries of the eigenvalues.

# Examples
```jldoctest
julia> A = SymTridiagonal([1.; 2.; 1.], [2.; 3.])
3×3 SymTridiagonal{Float64, Vector{Float64}}:
 1.0  2.0   ⋅
 2.0  2.0  3.0
  ⋅   3.0  1.0

julia> eigvals(A, -1, 2)
1-element Vector{Float64}:
 1.0000000000000009

julia> eigvals(A)
3-element Vector{Float64}:
 -2.1400549446402604
  1.0000000000000002
  5.140054944640259
```
"""
function eigvals(A::RealHermSymComplexHerm, vl::Real, vh::Real)
    T = eltype(A)
    S = eigtype(T)
    eigvals!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), vl, vh)
end

eigmax(A::RealHermSymComplexHerm{<:Real,<:StridedMatrix}) = eigvals(A, size(A, 1):size(A, 1))[1]
eigmin(A::RealHermSymComplexHerm{<:Real,<:StridedMatrix}) = eigvals(A, 1:1)[1]

function eigen!(A::HermOrSym{T,S}, B::HermOrSym{T,S}; sortby::Union{Function,Nothing}=nothing) where {T<:BlasReal,S<:StridedMatrix}
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.data, B.uplo == A.uplo ? B.data : copy(B.data'))
    GeneralizedEigen(sorteig!(vals, vecs, sortby)...)
end
function eigen!(A::Hermitian{T,S}, B::Hermitian{T,S}; sortby::Union{Function,Nothing}=nothing) where {T<:BlasComplex,S<:StridedMatrix}
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.data, B.uplo == A.uplo ? B.data : copy(B.data'))
    GeneralizedEigen(sorteig!(vals, vecs, sortby)...)
end

function eigen!(A::RealHermSymComplexHerm{T,S}, B::AbstractMatrix{T}; sortby::Union{Function,Nothing}=nothing) where {T<:Number,S<:StridedMatrix}
    U = cholesky(B).U
    vals, w = eigen!(UtiAUi!(A, U))
    vecs = U \ w
    GeneralizedEigen(sorteig!(vals, vecs, sortby)...)
end

# Perform U' \ A / U in-place.
UtiAUi!(As::Symmetric, Utr::UpperTriangular) = Symmetric(_UtiAsymUi!(As.uplo, parent(As), parent(Utr)), sym_uplo(As.uplo))
UtiAUi!(As::Hermitian, Utr::UpperTriangular) = Hermitian(_UtiAsymUi!(As.uplo, parent(As), parent(Utr)), sym_uplo(As.uplo))
UtiAUi!(As::Symmetric, Udi::Diagonal) = Symmetric(_UtiAsymUi_diag!(As.uplo, parent(As), Udi), sym_uplo(As.uplo))
UtiAUi!(As::Hermitian, Udi::Diagonal) = Hermitian(_UtiAsymUi_diag!(As.uplo, parent(As), Udi), sym_uplo(As.uplo))

# U is upper triangular
function _UtiAsymUi!(uplo, A, U)
    n = size(A, 1)
    μ⁻¹ = 1 / U[1, 1]
    αμ⁻² = A[1, 1] * μ⁻¹' * μ⁻¹

    # Update (1, 1) element
    A[1, 1] = αμ⁻²
    if n > 1
        Unext = view(U, 2:n, 2:n)

        if uplo === 'U'
            # Update submatrix
            for j in 2:n, i in 2:j
                A[i, j] = (
                    A[i, j]
                    - μ⁻¹' * U[1, j] * A[1, i]'
                    - μ⁻¹ * A[1, j] * U[1, i]'
                    + αμ⁻² * U[1, j] * U[1, i]'
                )
            end

            # Update vector
            for j in 2:n
                A[1, j] = A[1, j] * μ⁻¹' - U[1, j] * αμ⁻²
            end
            ldiv!(view(A', 2:n, 1), UpperTriangular(Unext)', view(A', 2:n, 1))
        else
            # Update submatrix
            for j in 2:n, i in 2:j
                A[j, i] = (
                    A[j, i]
                    - μ⁻¹ * A[i, 1]' * U[1, j]'
                    - μ⁻¹' * U[1, i] * A[j, 1]
                    + αμ⁻² * U[1, i] * U[1, j]'
                )
            end

            # Update vector
            for j in 2:n
                A[j, 1] = A[j, 1] * μ⁻¹ - U[1, j]' * αμ⁻²
            end
            ldiv!(view(A, 2:n, 1), UpperTriangular(Unext)', view(A, 2:n, 1))
        end

        # Recurse
        _UtiAsymUi!(uplo, view(A, 2:n, 2:n), Unext)
    end

    return A
end

# U is diagonal
function _UtiAsymUi_diag!(uplo, A, U)
    n = size(A, 1)
    μ⁻¹ = 1 / U[1, 1]
    αμ⁻² = A[1, 1] * μ⁻¹' * μ⁻¹

    # Update (1, 1) element
    A[1, 1] = αμ⁻²
    if n > 1
        Unext = view(U, 2:n, 2:n)

        if uplo === 'U'
            # No need to update any submatrix when U is diagonal

            # Update vector
            for j in 2:n
                A[1, j] = A[1, j] * μ⁻¹'
            end
            ldiv!(view(A', 2:n, 1), Diagonal(Unext)', view(A', 2:n, 1))
        else
            # No need to update any submatrix when U is diagonal

            # Update vector
            for j in 2:n
                A[j, 1] = A[j, 1] * μ⁻¹
            end
            ldiv!(view(A, 2:n, 1), Diagonal(Unext)', view(A, 2:n, 1))
        end

        # Recurse
        _UtiAsymUi!(uplo, view(A, 2:n, 2:n), Unext)
    end

    return A
end

eigvals!(A::HermOrSym{T,S}, B::HermOrSym{T,S}) where {T<:BlasReal,S<:StridedMatrix} =
    LAPACK.sygvd!(1, 'N', A.uplo, A.data, B.uplo == A.uplo ? B.data : copy(B.data'))[1]
eigvals!(A::Hermitian{T,S}, B::Hermitian{T,S}) where {T<:BlasComplex,S<:StridedMatrix} =
    LAPACK.sygvd!(1, 'N', A.uplo, A.data, B.uplo == A.uplo ? B.data : copy(B.data'))[1]

eigvecs(A::HermOrSym) = eigvecs(eigen(A))
