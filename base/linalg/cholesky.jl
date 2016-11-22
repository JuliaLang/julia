# This file is a part of Julia. License is MIT: http://julialang.org/license

##########################
# Cholesky Factorization #
##########################

# The dispatch structure in the chol!, chol, cholfact, and cholfact! methods is a bit
# complicated and some explanation is therefore provided in the following
#
# In the methods below, LAPACK is called when possible, i.e. StridedMatrices with Float32,
# Float64, Complex{Float32}, and Complex{Float64} element types. For other element or
# matrix types, the unblocked Julia implementation in _chol! is used. For cholfact
# and cholfact! pivoting is supported through a Val{Bool} argument. A type argument is
# necessary for type stability since the output of cholfact and cholfact! is either
# Cholesky or PivotedCholesky. The latter is only
# supported for the four LAPACK element types. For other types, e.g. BigFloats Val{true} will
# give an error. It is required that the input is Hermitian (including real symmetric) either
# through the Hermitian and Symmetric views or exact symmetric or Hermitian elements which
# is checked for and an error is thrown if the check fails. The dispatch
# is further complicated by a limitation in the formulation of Unions. The relevant union
# would be Union{Symmetric{T<:Real,S}, Hermitian} but, right now, it doesn't work in Julia
# so we'll have to define methods for the two elements of the union separately.

# FixMe? The dispatch below seems overly complicated. One simplification could be to
# merge the two Cholesky types into one. It would remove the need for Val completely but
# the cost would be extra unnecessary/unused fields for the unpivoted Cholesky and runtime
# checks of those fields before calls to LAPACK to check which version of the Cholesky
# factorization the type represents.

immutable Cholesky{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    uplo::Char
end
Cholesky{T}(A::AbstractMatrix{T}, uplo::Symbol) = Cholesky{T,typeof(A)}(A, char_uplo(uplo))
Cholesky{T}(A::AbstractMatrix{T}, uplo::Char) = Cholesky{T,typeof(A)}(A, uplo)

immutable CholeskyPivoted{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    uplo::Char
    piv::Vector{BlasInt}
    rank::BlasInt
    tol::Real
    info::BlasInt
end
function CholeskyPivoted{T}(A::AbstractMatrix{T}, uplo::Char, piv::Vector{BlasInt},
                            rank::BlasInt, tol::Real, info::BlasInt)
    CholeskyPivoted{T,typeof(A)}(A, uplo, piv, rank, tol, info)
end


# _chol!. Internal methods for calling unpivoted Cholesky
## BLAS/LAPACK element types
function _chol!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{UpperTriangular})
    C, info = LAPACK.potrf!('U', A)
    return @assertposdef UpperTriangular(C) info
end
function _chol!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{LowerTriangular})
    C, info = LAPACK.potrf!('L', A)
    return @assertposdef LowerTriangular(C) info
end

## Non BLAS/LAPACK element types (generic)
function _chol!(A::AbstractMatrix, ::Type{UpperTriangular})
    n = checksquare(A)
    @inbounds begin
        for k = 1:n
            for i = 1:k - 1
                A[k,k] -= A[i,k]'A[i,k]
            end
            Akk = _chol!(A[k,k], UpperTriangular)
            A[k,k] = Akk
            AkkInv = inv(Akk')
            for j = k + 1:n
                for i = 1:k - 1
                    A[k,j] -= A[i,k]'A[i,j]
                end
                A[k,j] = AkkInv*A[k,j]
            end
        end
    end
    return UpperTriangular(A)
end
function _chol!(A::AbstractMatrix, ::Type{LowerTriangular})
    n = checksquare(A)
    @inbounds begin
        for k = 1:n
            for i = 1:k - 1
                A[k,k] -= A[k,i]*A[k,i]'
            end
            Akk = _chol!(A[k,k], LowerTriangular)
            A[k,k] = Akk
            AkkInv = inv(Akk)
            for j = 1:k
                for i = k + 1:n
                    if j == 1
                        A[i,k] = A[i,k]*AkkInv'
                    end
                    if j < k
                        A[i,k] -= A[i,j]*A[k,j]'*AkkInv'
                    end
                end
            end
        end
     end
    return LowerTriangular(A)
end

## Numbers
function _chol!(x::Number, uplo)
    rx = real(x)
    if rx != abs(x)
        throw(ArgumentError("x must be positive semidefinite"))
    end
    rxr = sqrt(rx)
    convert(promote_type(typeof(x), typeof(rxr)), rxr)
end

non_hermitian_error(f) = throw(ArgumentError("matrix is not symmetric/" *
    "Hermitian. This error can be avoided by calling $f(Hermitian(A)) " *
    "which will ignore either the upper or lower triangle of the matrix."))

# chol!. Destructive methods for computing Cholesky factor of real symmetric or Hermitian
# matrix
chol!(A::Hermitian) =
    _chol!(A.uplo == 'U' ? A.data : LinAlg.copytri!(A.data, 'L', true), UpperTriangular)
chol!{T<:Real,S<:StridedMatrix}(A::Symmetric{T,S}) =
    _chol!(A.uplo == 'U' ? A.data : LinAlg.copytri!(A.data, 'L', true), UpperTriangular)
function chol!(A::StridedMatrix)
    ishermitian(A) || non_hermitian_error("chol!")
    return _chol!(A, UpperTriangular)
end



# chol. Non-destructive methods for computing Cholesky factor of a real symmetric or
# Hermitian matrix. Promotes elements to a type that is stable under square roots.
function chol(A::Hermitian)
    T = promote_type(typeof(chol(one(eltype(A)))), Float32)
    AA = similar(A, T, size(A))
    if A.uplo == 'U'
        copy!(AA, A.data)
    else
        Base.ctranspose!(AA, A.data)
    end
    chol!(Hermitian(AA, :U))
end
function chol{T<:Real,S<:AbstractMatrix}(A::Symmetric{T,S})
    TT = promote_type(typeof(chol(one(T))), Float32)
    AA = similar(A, TT, size(A))
    if A.uplo == 'U'
        copy!(AA, A.data)
    else
        Base.ctranspose!(AA, A.data)
    end
    chol!(Hermitian(AA, :U))
end

## for StridedMatrices, check that matrix is symmetric/Hermitian
"""
    chol(A) -> U

Compute the Cholesky factorization of a positive definite matrix `A`
and return the UpperTriangular matrix `U` such that `A = U'U`.

# Example

```jldoctest
julia> A = [1. 2.; 2. 50.]
2×2 Array{Float64,2}:
 1.0   2.0
 2.0  50.0

julia> U = chol(A)
2×2 UpperTriangular{Float64,Array{Float64,2}}:
 1.0  2.0
  ⋅   6.78233

julia> U'U
2×2 Array{Float64,2}:
 1.0   2.0
 2.0  50.0
```
"""
function chol(A::AbstractMatrix)
    ishermitian(A) || non_hermitian_error("chol")
    return chol(Hermitian(A))
end

## Numbers
"""
    chol(x::Number) -> y

Compute the square root of a non-negative number `x`.

# Example

```jldoctest
julia> chol(16)
4.0
```
"""
chol(x::Number, args...) = _chol!(x, nothing)



# cholfact!. Destructive methods for computing Cholesky factorization of real symmetric
# or Hermitian matrix
## No pivoting
function cholfact!(A::Hermitian, ::Type{Val{false}})
    if A.uplo == 'U'
        Cholesky(_chol!(A.data, UpperTriangular).data, 'U')
    else
        Cholesky(_chol!(A.data, LowerTriangular).data, 'L')
    end
end
function cholfact!{T<:Real,S}(A::Symmetric{T,S}, ::Type{Val{false}})
    if A.uplo == 'U'
        Cholesky(_chol!(A.data, UpperTriangular).data, 'U')
    else
        Cholesky(_chol!(A.data, LowerTriangular).data, 'L')
    end
end

### for StridedMatrices, check that matrix is symmetric/Hermitian
"""
    cholfact!(A, [uplo::Symbol,] Val{false}) -> Cholesky

The same as [`cholfact`](@ref), but saves space by overwriting the input `A`,
instead of creating a copy. An [`InexactError`](@ref) exception is thrown if
the factorization produces a number not representable by the element type of
`A`, e.g. for integer types.

# Example

```jldoctest
julia> A = [1 2; 2 50]
2×2 Array{Int64,2}:
 1   2
 2  50

julia> cholfact!(A)
ERROR: InexactError()
```
"""
function cholfact!(A::StridedMatrix, uplo::Symbol, ::Type{Val{false}})
    ishermitian(A) || non_hermitian_error("cholfact!")
    return cholfact!(Hermitian(A, uplo), Val{false})
end

### Default to no pivoting (and storing of upper factor) when not explicit
cholfact!(A::Hermitian) = cholfact!(A, Val{false})
cholfact!{T<:Real,S}(A::Symmetric{T,S}) = cholfact!(A, Val{false})
#### for StridedMatrices, check that matrix is symmetric/Hermitian
function cholfact!(A::StridedMatrix, uplo::Symbol = :U)
    ishermitian(A) || non_hermitian_error("cholfact!")
    return cholfact!(Hermitian(A, uplo))
end


## With pivoting
### BLAS/LAPACK element types
function cholfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S},
        ::Type{Val{true}}; tol = 0.0)
    AA, piv, rank, info = LAPACK.pstrf!(A.uplo, A.data, tol)
    return CholeskyPivoted{eltype(AA),typeof(AA)}(AA, A.uplo, piv, rank, tol, info)
end

### Non BLAS/LAPACK element types (generic). Since generic fallback for pivoted Cholesky
### is not implemented yet we throw an error
cholfact!{T<:Real,S}(A::RealHermSymComplexHerm{T,S}, ::Type{Val{true}};
    tol = 0.0) =
        throw(ArgumentError("generic pivoted Cholesky factorization is not implemented yet"))

### for StridedMatrices, check that matrix is symmetric/Hermitian
"""
    cholfact!(A, [uplo::Symbol,] Val{true}; tol = 0.0) -> CholeskyPivoted

The same as [`cholfact`](@ref), but saves space by overwriting the input `A`,
instead of creating a copy. An [`InexactError`](@ref) exception is thrown if the
factorization produces a number not representable by the element type of `A`,
e.g. for integer types.
"""
function cholfact!(A::StridedMatrix, uplo::Symbol, ::Type{Val{true}}; tol = 0.0)
    ishermitian(A) || non_hermitian_error("cholfact!")
    return cholfact!(Hermitian(A, uplo), Val{true}; tol = tol)
end

# cholfact. Non-destructive methods for computing Cholesky factorization of real symmetric
# or Hermitian matrix
## No pivoting
cholfact(A::Hermitian, ::Type{Val{false}}) =
    cholfact!(copy_oftype(A, promote_type(typeof(chol(one(eltype(A)))),Float32)), Val{false})
cholfact{T<:Real,S<:StridedMatrix}(A::Symmetric{T,S}, ::Type{Val{false}}) =
    cholfact!(copy_oftype(A, promote_type(typeof(chol(one(eltype(A)))),Float32)), Val{false})

### for StridedMatrices, check that matrix is symmetric/Hermitian
"""
    cholfact(A, [uplo::Symbol,] Val{false}) -> Cholesky

Compute the Cholesky factorization of a dense symmetric positive definite matrix `A`
and return a `Cholesky` factorization. The matrix `A` can either be a [`Symmetric`](@ref) or [`Hermitian`](@ref)
`StridedMatrix` or a *perfectly* symmetric or Hermitian `StridedMatrix`. In the latter case,
the optional argument `uplo` may be `:L` for using the lower part or `:U` for the upper part of `A`.
The default is to use `:U`.
The triangular Cholesky factor can be obtained from the factorization `F` with: `F[:L]` and `F[:U]`.
The following functions are available for `Cholesky` objects: [`size`](@ref), [`\\`](@ref),
[`inv`](@ref), and [`det`](@ref).
A `PosDefException` exception is thrown in case the matrix is not positive definite.

# Example

```jldoctest
julia> A = [4. 12. -16.; 12. 37. -43.; -16. -43. 98.]
3×3 Array{Float64,2}:
   4.0   12.0  -16.0
  12.0   37.0  -43.0
 -16.0  -43.0   98.0

julia> C = cholfact(A)
Base.LinAlg.Cholesky{Float64,Array{Float64,2}} with factor:
[2.0 6.0 -8.0; 0.0 1.0 5.0; 0.0 0.0 3.0]

julia> C[:U]
3×3 UpperTriangular{Float64,Array{Float64,2}}:
 2.0  6.0  -8.0
  ⋅   1.0   5.0
  ⋅    ⋅    3.0

julia> C[:L]
3×3 LowerTriangular{Float64,Array{Float64,2}}:
  2.0   ⋅    ⋅
  6.0  1.0   ⋅
 -8.0  5.0  3.0

julia> C[:L] * C[:U] == A
true
```
"""
function cholfact(A::StridedMatrix, uplo::Symbol, ::Type{Val{false}})
    ishermitian(A) || non_hermitian_error("cholfact")
    return cholfact(Hermitian(A, uplo), Val{false})
end

### Default to no pivoting (and storing of upper factor) when not explicit
cholfact(A::Hermitian) = cholfact(A, Val{false})
cholfact{T<:Real,S<:StridedMatrix}(A::Symmetric{T,S}) = cholfact(A, Val{false})
#### for StridedMatrices, check that matrix is symmetric/Hermitian
function cholfact(A::StridedMatrix, uplo::Symbol = :U)
    ishermitian(A) || non_hermitian_error("cholfact")
    return cholfact(Hermitian(A, uplo))
end


## With pivoting
cholfact(A::Hermitian, ::Type{Val{true}}; tol = 0.0) =
        cholfact!(copy_oftype(A, promote_type(typeof(chol(one(eltype(A)))),Float32)),
            Val{true}; tol = tol)
cholfact{T<:Real,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, ::Type{Val{true}}; tol = 0.0) =
        cholfact!(copy_oftype(A, promote_type(typeof(chol(one(eltype(A)))),Float32)),
            Val{true}; tol = tol)

### for StridedMatrices, check that matrix is symmetric/Hermitian
"""
    cholfact(A, [uplo::Symbol,] Val{true}; tol = 0.0) -> CholeskyPivoted

Compute the pivoted Cholesky factorization of a dense symmetric positive semi-definite matrix `A`
and return a `CholeskyPivoted` factorization. The matrix `A` can either be a [`Symmetric`](@ref)
or [`Hermitian`](@ref) `StridedMatrix` or a *perfectly* symmetric or Hermitian `StridedMatrix`.
In the latter case, the optional argument `uplo` may be `:L` for using the lower part or `:U`
for the upper part of `A`. The default is to use `:U`.
The triangular Cholesky factor can be obtained from the factorization `F` with: `F[:L]` and `F[:U]`.
The following functions are available for `PivotedCholesky` objects:
[`size`](@ref), [`\\`](@ref), [`inv`](@ref), [`det`](@ref), and [`rank`](@ref).
The argument `tol` determines the tolerance for determining the rank.
For negative values, the tolerance is the machine precision.
"""
function cholfact(A::StridedMatrix, uplo::Symbol, ::Type{Val{true}}; tol = 0.0)
    ishermitian(A) || non_hermitian_error("cholfact")
    return cholfact(Hermitian(A, uplo), Val{true}; tol = tol)
end

## Number
function cholfact(x::Number, uplo::Symbol=:U)
    xf = fill(chol(x), 1, 1)
    Cholesky(xf, uplo)
end



function convert{Tnew,Told,S}(::Type{Cholesky{Tnew}}, C::Cholesky{Told,S})
    Cnew = convert(AbstractMatrix{Tnew}, C.factors)
    Cholesky{Tnew, typeof(Cnew)}(Cnew, C.uplo)
end
function convert{T,S}(::Type{Cholesky{T,S}}, C::Cholesky)
    Cnew = convert(AbstractMatrix{T}, C.factors)
    Cholesky{T, typeof(Cnew)}(Cnew, C.uplo)
end
convert{T}(::Type{Factorization{T}}, C::Cholesky{T}) = C
convert{T}(::Type{Factorization{T}}, C::Cholesky) = convert(Cholesky{T}, C)
convert{T}(::Type{CholeskyPivoted{T}},C::CholeskyPivoted{T}) = C
convert{T}(::Type{CholeskyPivoted{T}},C::CholeskyPivoted) =
    CholeskyPivoted(AbstractMatrix{T}(C.factors),C.uplo,C.piv,C.rank,C.tol,C.info)
convert{T}(::Type{Factorization{T}}, C::CholeskyPivoted{T}) = C
convert{T}(::Type{Factorization{T}}, C::CholeskyPivoted) = convert(CholeskyPivoted{T}, C)

convert(::Type{AbstractMatrix}, C::Cholesky) = C.uplo == 'U' ? C[:U]'C[:U] : C[:L]*C[:L]'
convert(::Type{AbstractArray}, C::Cholesky) = convert(AbstractMatrix, C)
convert(::Type{Matrix}, C::Cholesky) = convert(Array, convert(AbstractArray, C))
convert(::Type{Array}, C::Cholesky) = convert(Matrix, C)
full(C::Cholesky) = convert(AbstractArray, C)

function convert(::Type{AbstractMatrix}, F::CholeskyPivoted)
    ip = invperm(F[:p])
    (F[:L] * F[:U])[ip,ip]
end
convert(::Type{AbstractArray}, F::CholeskyPivoted) = convert(AbstractMatrix, F)
convert(::Type{Matrix}, F::CholeskyPivoted) = convert(Array, convert(AbstractArray, F))
convert(::Type{Array}, F::CholeskyPivoted) = convert(Matrix, F)
full(F::CholeskyPivoted) = convert(AbstractArray, F)

copy(C::Cholesky) = Cholesky(copy(C.factors), C.uplo)
copy(C::CholeskyPivoted) = CholeskyPivoted(copy(C.factors), C.uplo, C.piv, C.rank, C.tol, C.info)

size(C::Union{Cholesky, CholeskyPivoted}) = size(C.factors)
size(C::Union{Cholesky, CholeskyPivoted}, d::Integer) = size(C.factors, d)

function getindex{T,S}(C::Cholesky{T,S}, d::Symbol)
    d == :U && return UpperTriangular(Symbol(C.uplo) == d ? C.factors : C.factors')
    d == :L && return LowerTriangular(Symbol(C.uplo) == d ? C.factors : C.factors')
    d == :UL && return Symbol(C.uplo) == :U ? UpperTriangular(C.factors) : LowerTriangular(C.factors)
    throw(KeyError(d))
end
function getindex{T<:BlasFloat}(C::CholeskyPivoted{T}, d::Symbol)
    d == :U && return UpperTriangular(Symbol(C.uplo) == d ? C.factors : C.factors')
    d == :L && return LowerTriangular(Symbol(C.uplo) == d ? C.factors : C.factors')
    d == :p && return C.piv
    if d == :P
        n = size(C, 1)
        P = zeros(T, n, n)
        for i = 1:n
            P[C.piv[i],i] = one(T)
        end
        return P
    end
    throw(KeyError(d))
end

show{T,S<:AbstractMatrix}(io::IO, C::Cholesky{T,S}) =
    (println(io, "$(typeof(C)) with factor:");show(io,C[:UL]))

A_ldiv_B!{T<:BlasFloat,S<:AbstractMatrix}(C::Cholesky{T,S}, B::StridedVecOrMat{T}) =
    LAPACK.potrs!(C.uplo, C.factors, B)

function A_ldiv_B!{T,S<:AbstractMatrix}(C::Cholesky{T,S}, B::StridedVecOrMat)
    if C.uplo == 'L'
        return Ac_ldiv_B!(LowerTriangular(C.factors), A_ldiv_B!(LowerTriangular(C.factors), B))
    else
        return A_ldiv_B!(UpperTriangular(C.factors), Ac_ldiv_B!(UpperTriangular(C.factors), B))
    end
end

function A_ldiv_B!{T<:BlasFloat}(C::CholeskyPivoted{T}, B::StridedVector{T})
    chkfullrank(C)
    ipermute!(LAPACK.potrs!(C.uplo, C.factors, permute!(B, C.piv)), C.piv)
end
function A_ldiv_B!{T<:BlasFloat}(C::CholeskyPivoted{T}, B::StridedMatrix{T})
    chkfullrank(C)
    n = size(C, 1)
    for i=1:size(B, 2)
        permute!(view(B, 1:n, i), C.piv)
    end
    LAPACK.potrs!(C.uplo, C.factors, B)
    for i=1:size(B, 2)
        ipermute!(view(B, 1:n, i), C.piv)
    end
    B
end

function A_ldiv_B!(C::CholeskyPivoted, B::StridedVector)
    if C.uplo == 'L'
        Ac_ldiv_B!(LowerTriangular(C.factors),
            A_ldiv_B!(LowerTriangular(C.factors), B[C.piv]))[invperm(C.piv)]
    else
        A_ldiv_B!(UpperTriangular(C.factors),
            Ac_ldiv_B!(UpperTriangular(C.factors), B[C.piv]))[invperm(C.piv)]
    end
end

function A_ldiv_B!(C::CholeskyPivoted, B::StridedMatrix)
    if C.uplo == 'L'
        Ac_ldiv_B!(LowerTriangular(C.factors),
            A_ldiv_B!(LowerTriangular(C.factors), B[C.piv,:]))[invperm(C.piv),:]
    else
        A_ldiv_B!(UpperTriangular(C.factors),
            Ac_ldiv_B!(UpperTriangular(C.factors), B[C.piv,:]))[invperm(C.piv),:]
    end
end

function det(C::Cholesky)
    dd = one(real(eltype(C)))
    for i in 1:size(C.factors,1)
        dd *= real(C.factors[i,i])^2
    end
    dd
end

function logdet(C::Cholesky)
    dd = zero(real(eltype(C)))
    for i in 1:size(C.factors,1)
        dd += log(real(C.factors[i,i]))
    end
    dd + dd # instead of 2.0dd which can change the type
end

function det(C::CholeskyPivoted)
    if C.rank < size(C.factors, 1)
        return zero(real(eltype(C)))
    else
        dd = one(real(eltype(C)))
        for i in 1:size(C.factors,1)
            dd *= real(C.factors[i,i])^2
        end
        return dd
    end
end

function logdet(C::CholeskyPivoted)
    if C.rank < size(C.factors, 1)
        return real(eltype(C))(-Inf)
    else
        dd = zero(real(eltype(C)))
        for i in 1:size(C.factors,1)
            dd += log(real(C.factors[i,i]))
        end
        return dd + dd # instead of 2.0dd which can change the type
    end
end

inv!{T<:BlasFloat,S<:StridedMatrix}(C::Cholesky{T,S}) =
    copytri!(LAPACK.potri!(C.uplo, C.factors), C.uplo, true)

inv{T<:BlasFloat,S<:StridedMatrix}(C::Cholesky{T,S}) =
    inv!(copy(C))

function inv(C::CholeskyPivoted)
    chkfullrank(C)
    ipiv = invperm(C.piv)
    copytri!(LAPACK.potri!(C.uplo, copy(C.factors)), C.uplo, true)[ipiv, ipiv]
end

function chkfullrank(C::CholeskyPivoted)
    if C.rank < size(C.factors, 1)
        throw(RankDeficientException(C.info))
    end
end

rank(C::CholeskyPivoted) = C.rank

"""
    lowrankupdate!(C::Cholesky, v::StridedVector) -> CC::Cholesky

Update a Cholesky factorization `C` with the vector `v`. If `A = C[:U]'C[:U]` then
`CC = cholfact(C[:U]'C[:U] + v*v')` but the computation of `CC` only uses `O(n^2)`
operations. The input factorization `C` is updated in place such that on exit `C == CC`.
The vector `v` is destroyed during the computation.
"""
function lowrankupdate!(C::Cholesky, v::StridedVector)
    A = C.factors
    n = length(v)
    if size(C, 1) != n
        throw(DimensionMismatch("updating vector must fit size of factorization"))
    end
    if C.uplo == 'U'
        conj!(v)
    end

    for i = 1:n

        # Compute Givens rotation
        c, s, r = givensAlgorithm(A[i,i], v[i])

        # Store new diagonal element
        A[i,i] = r

        # Update remaining elements in row/column
        if C.uplo == 'U'
            for j = i + 1:n
                Aij = A[i,j]
                vj  = v[j]
                A[i,j]  =   c*Aij + s*vj
                v[j]    = -s'*Aij + c*vj
            end
        else
            for j = i + 1:n
                Aji = A[j,i]
                vj  = v[j]
                A[j,i]  =   c*Aji + s*vj
                v[j]    = -s'*Aji + c*vj
            end
        end
    end
    return C
end

"""
    lowrankdowndate!(C::Cholesky, v::StridedVector) -> CC::Cholesky

Downdate a Cholesky factorization `C` with the vector `v`. If `A = C[:U]'C[:U]` then
`CC = cholfact(C[:U]'C[:U] - v*v')` but the computation of `CC` only uses `O(n^2)`
operations. The input factorization `C` is updated in place such that on exit `C == CC`.
The vector `v` is destroyed during the computation.
"""
function lowrankdowndate!(C::Cholesky, v::StridedVector)
    A = C.factors
    n = length(v)
    if size(C, 1) != n
        throw(DimensionMismatch("updating vector must fit size of factorization"))
    end
    if C.uplo == 'U'
        conj!(v)
    end

    for i = 1:n

        Aii = A[i,i]

        # Compute Givens rotation
        s = conj(v[i]/Aii)
        s2 = abs2(s)
        if s2 > 1
            throw(LinAlg.PosDefException(i))
        end
        c = sqrt(1 - abs2(s))

        # Store new diagonal element
        A[i,i] = c*Aii

        # Update remaining elements in row/column
        if C.uplo == 'U'
            for j = i + 1:n
                vj = v[j]
                Aij = (A[i,j] - s*vj)/c
                A[i,j] = Aij
                v[j] = -s'*Aij + c*vj
            end
        else
            for j = i + 1:n
                vj = v[j]
                Aji = (A[j,i] - s*vj)/c
                A[j,i] = Aji
                v[j] = -s'*Aji + c*vj
            end
        end
    end
    return C
end

"""
    lowrankupdate(C::Cholesky, v::StridedVector) -> CC::Cholesky

Update a Cholesky factorization `C` with the vector `v`. If `A = C[:U]'C[:U]`
then `CC = cholfact(C[:U]'C[:U] + v*v')` but the computation of `CC` only uses
`O(n^2)` operations.
"""
lowrankupdate(C::Cholesky, v::StridedVector) = lowrankupdate!(copy(C), copy(v))

"""
    lowrankdowndate(C::Cholesky, v::StridedVector) -> CC::Cholesky

Downdate a Cholesky factorization `C` with the vector `v`. If `A = C[:U]'C[:U]`
then `CC = cholfact(C[:U]'C[:U] - v*v')` but the computation of `CC` only uses
`O(n^2)` operations.
"""
lowrankdowndate(C::Cholesky, v::StridedVector) = lowrankdowndate!(copy(C), copy(v))
