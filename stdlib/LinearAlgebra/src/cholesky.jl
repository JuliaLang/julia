# This file is a part of Julia. License is MIT: https://julialang.org/license

##########################
# Cholesky Factorization #
##########################

# The dispatch structure in the cholesky, and cholesky! methods is a bit
# complicated and some explanation is therefore provided in the following
#
# In the methods below, LAPACK is called when possible, i.e. StridedMatrices with Float32,
# Float64, Complex{Float32}, and Complex{Float64} element types. For other element or
# matrix types, the unblocked Julia implementation in _chol! is used. For cholesky
# and cholesky! pivoting is supported through a Val(Bool) argument. A type argument is
# necessary for type stability since the output of cholesky and cholesky! is either
# Cholesky or CholeskyPivoted. The latter is only
# supported for the four LAPACK element types. For other types, e.g. BigFloats Val(true) will
# give an error. It is required that the input is Hermitian (including real symmetric) either
# through the Hermitian and Symmetric views or exact symmetric or Hermitian elements which
# is checked for and an error is thrown if the check fails.

# The internal structure is as follows
# - _chol! returns the factor and info without checking positive definiteness
# - cholesky/cholesky! returns Cholesky without checking positive definiteness

# FixMe? The dispatch below seems overly complicated. One simplification could be to
# merge the two Cholesky types into one. It would remove the need for Val completely but
# the cost would be extra unnecessary/unused fields for the unpivoted Cholesky and runtime
# checks of those fields before calls to LAPACK to check which version of the Cholesky
# factorization the type represents.
"""
    Cholesky <: Factorization

Matrix factorization type of the Cholesky factorization of a dense symmetric/Hermitian
positive definite matrix `A`. This is the return type of [`cholesky`](@ref),
the corresponding matrix factorization function.

The triangular Cholesky factor can be obtained from the factorization `F::Cholesky`
via `F.L` and `F.U`.

# Examples
```jldoctest
julia> A = [4. 12. -16.; 12. 37. -43.; -16. -43. 98.]
3×3 Array{Float64,2}:
   4.0   12.0  -16.0
  12.0   37.0  -43.0
 -16.0  -43.0   98.0

julia> C = cholesky(A)
Cholesky{Float64,Array{Float64,2}}
U factor:
3×3 UpperTriangular{Float64,Array{Float64,2}}:
 2.0  6.0  -8.0
  ⋅   1.0   5.0
  ⋅    ⋅    3.0

julia> C.U
3×3 UpperTriangular{Float64,Array{Float64,2}}:
 2.0  6.0  -8.0
  ⋅   1.0   5.0
  ⋅    ⋅    3.0

julia> C.L
3×3 LowerTriangular{Float64,Array{Float64,2}}:
  2.0   ⋅    ⋅
  6.0  1.0   ⋅
 -8.0  5.0  3.0

julia> C.L * C.U == A
true
```
"""
struct Cholesky{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    uplo::Char
    info::BlasInt

    function Cholesky{T,S}(factors, uplo, info) where {T,S<:AbstractMatrix}
        require_one_based_indexing(factors)
        new(factors, uplo, info)
    end
end
Cholesky(A::AbstractMatrix{T}, uplo::Symbol, info::Integer) where {T} =
    Cholesky{T,typeof(A)}(A, char_uplo(uplo), info)
Cholesky(A::AbstractMatrix{T}, uplo::AbstractChar, info::Integer) where {T} =
    Cholesky{T,typeof(A)}(A, uplo, info)

"""
    CholeskyPivoted

Matrix factorization type of the pivoted Cholesky factorization of a dense symmetric/Hermitian
positive semi-definite matrix `A`. This is the return type of [`cholesky(_, Val(true))`](@ref),
the corresponding matrix factorization function.

The triangular Cholesky factor can be obtained from the factorization `F::CholeskyPivoted`
via `F.L` and `F.U`.

# Examples
```jldoctest
julia> A = [4. 12. -16.; 12. 37. -43.; -16. -43. 98.]
3×3 Array{Float64,2}:
   4.0   12.0  -16.0
  12.0   37.0  -43.0
 -16.0  -43.0   98.0

julia> C = cholesky(A, Val(true))
CholeskyPivoted{Float64,Array{Float64,2}}
U factor with rank 3:
3×3 UpperTriangular{Float64,Array{Float64,2}}:
 9.89949  -4.34366  -1.61624
  ⋅        4.25825   1.1694
  ⋅         ⋅        0.142334
permutation:
3-element Array{Int64,1}:
 3
 2
 1
```
"""
struct CholeskyPivoted{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    uplo::Char
    piv::Vector{BlasInt}
    rank::BlasInt
    tol::Real
    info::BlasInt

    function CholeskyPivoted{T,S}(factors, uplo, piv, rank, tol, info) where {T,S<:AbstractMatrix}
        require_one_based_indexing(factors)
        new(factors, uplo, piv, rank, tol, info)
    end
end
function CholeskyPivoted(A::AbstractMatrix{T}, uplo::AbstractChar, piv::Vector{<:Integer},
                            rank::Integer, tol::Real, info::Integer) where T
    CholeskyPivoted{T,typeof(A)}(A, uplo, piv, rank, tol, info)
end

# make a copy that allow inplace Cholesky factorization
@inline choltype(A) = promote_type(typeof(sqrt(oneunit(eltype(A)))), Float32)
@inline cholcopy(A) = copy_oftype(A, choltype(A))

# _chol!. Internal methods for calling unpivoted Cholesky
## BLAS/LAPACK element types
function _chol!(A::StridedMatrix{<:BlasFloat}, ::Type{UpperTriangular})
    C, info = LAPACK.potrf!('U', A)
    return UpperTriangular(C), info
end
function _chol!(A::StridedMatrix{<:BlasFloat}, ::Type{LowerTriangular})
    C, info = LAPACK.potrf!('L', A)
    return LowerTriangular(C), info
end

## Non BLAS/LAPACK element types (generic)
function _chol!(A::AbstractMatrix, ::Type{UpperTriangular})
    require_one_based_indexing(A)
    n = checksquare(A)
    @inbounds begin
        for k = 1:n
            for i = 1:k - 1
                A[k,k] -= A[i,k]'A[i,k]
            end
            Akk, info = _chol!(A[k,k], UpperTriangular)
            if info != 0
                return UpperTriangular(A), info
            end
            A[k,k] = Akk
            AkkInv = inv(copy(Akk'))
            for j = k + 1:n
                for i = 1:k - 1
                    A[k,j] -= A[i,k]'A[i,j]
                end
                A[k,j] = AkkInv*A[k,j]
            end
        end
    end
    return UpperTriangular(A), convert(BlasInt, 0)
end
function _chol!(A::AbstractMatrix, ::Type{LowerTriangular})
    require_one_based_indexing(A)
    n = checksquare(A)
    @inbounds begin
        for k = 1:n
            for i = 1:k - 1
                A[k,k] -= A[k,i]*A[k,i]'
            end
            Akk, info = _chol!(A[k,k], LowerTriangular)
            if info != 0
                return LowerTriangular(A), info
            end
            A[k,k] = Akk
            AkkInv = inv(Akk)
            for j = 1:k - 1
                @simd for i = k + 1:n
                    A[i,k] -= A[i,j]*A[k,j]'
                end
            end
            for i = k + 1:n
                A[i,k] *= AkkInv'
            end
        end
     end
    return LowerTriangular(A), convert(BlasInt, 0)
end

## Numbers
function _chol!(x::Number, uplo)
    rx = real(x)
    rxr = sqrt(abs(rx))
    rval =  convert(promote_type(typeof(x), typeof(rxr)), rxr)
    rx == abs(x) ? (rval, convert(BlasInt, 0)) : (rval, convert(BlasInt, 1))
end

## for StridedMatrices, check that matrix is symmetric/Hermitian

# cholesky!. Destructive methods for computing Cholesky factorization of real symmetric
# or Hermitian matrix
## No pivoting (default)
function cholesky!(A::RealHermSymComplexHerm, ::Val{false}=Val(false); check::Bool = true)
    C, info = _chol!(A.data, A.uplo == 'U' ? UpperTriangular : LowerTriangular)
    check && checkpositivedefinite(info)
    return Cholesky(C.data, A.uplo, info)
end

### for StridedMatrices, check that matrix is symmetric/Hermitian
"""
    cholesky!(A, Val(false); check = true) -> Cholesky

The same as [`cholesky`](@ref), but saves space by overwriting the input `A`,
instead of creating a copy. An [`InexactError`](@ref) exception is thrown if
the factorization produces a number not representable by the element type of
`A`, e.g. for integer types.

# Examples
```jldoctest
julia> A = [1 2; 2 50]
2×2 Array{Int64,2}:
 1   2
 2  50

julia> cholesky!(A)
ERROR: InexactError: Int64(6.782329983125268)
Stacktrace:
[...]
```
"""
function cholesky!(A::StridedMatrix, ::Val{false}=Val(false); check::Bool = true)
    checksquare(A)
    if !ishermitian(A) # return with info = -1 if not Hermitian
        check && checkpositivedefinite(-1)
        return Cholesky(A, 'U', convert(BlasInt, -1))
    else
        return cholesky!(Hermitian(A), Val(false); check = check)
    end
end

## With pivoting
### BLAS/LAPACK element types
function cholesky!(A::RealHermSymComplexHerm{<:BlasReal,<:StridedMatrix},
                   ::Val{true}; tol = 0.0, check::Bool = true)
    AA, piv, rank, info = LAPACK.pstrf!(A.uplo, A.data, tol)
    C = CholeskyPivoted{eltype(AA),typeof(AA)}(AA, A.uplo, piv, rank, tol, info)
    check && chkfullrank(C)
    return C
end

### Non BLAS/LAPACK element types (generic). Since generic fallback for pivoted Cholesky
### is not implemented yet we throw an error
cholesky!(A::RealHermSymComplexHerm{<:Real}, ::Val{true}; tol = 0.0, check::Bool = true) =
    throw(ArgumentError("generic pivoted Cholesky factorization is not implemented yet"))

### for StridedMatrices, check that matrix is symmetric/Hermitian
"""
    cholesky!(A, Val(true); tol = 0.0, check = true) -> CholeskyPivoted

The same as [`cholesky`](@ref), but saves space by overwriting the input `A`,
instead of creating a copy. An [`InexactError`](@ref) exception is thrown if the
factorization produces a number not representable by the element type of `A`,
e.g. for integer types.
"""
function cholesky!(A::StridedMatrix, ::Val{true}; tol = 0.0, check::Bool = true)
    checksquare(A)
    if !ishermitian(A)
        C = CholeskyPivoted(A, 'U', Vector{BlasInt}(),convert(BlasInt, 1),
                            tol, convert(BlasInt, -1))
        check && chkfullrank(C)
        return C
    else
        return cholesky!(Hermitian(A), Val(true); tol = tol, check = check)
    end
end

# cholesky. Non-destructive methods for computing Cholesky factorization of real symmetric
# or Hermitian matrix
## No pivoting (default)
"""
    cholesky(A, Val(false); check = true) -> Cholesky

Compute the Cholesky factorization of a dense symmetric positive definite matrix `A`
and return a [`Cholesky`](@ref) factorization. The matrix `A` can either be a [`Symmetric`](@ref) or [`Hermitian`](@ref)
[`StridedMatrix`](@ref) or a *perfectly* symmetric or Hermitian `StridedMatrix`.
The triangular Cholesky factor can be obtained from the factorization `F` with: `F.L` and `F.U`.
The following functions are available for `Cholesky` objects: [`size`](@ref), [`\\`](@ref),
[`inv`](@ref), [`det`](@ref), [`logdet`](@ref) and [`isposdef`](@ref).

If you have a matrix `A` that is slightly non-Hermitian due to roundoff errors in its construction,
wrap it in `Hermitian(A)` before passing it to `cholesky` in order to treat it as perfectly Hermitian.

When `check = true`, an error is thrown if the decomposition fails.
When `check = false`, responsibility for checking the decomposition's
validity (via [`issuccess`](@ref)) lies with the user.

# Examples
```jldoctest
julia> A = [4. 12. -16.; 12. 37. -43.; -16. -43. 98.]
3×3 Array{Float64,2}:
   4.0   12.0  -16.0
  12.0   37.0  -43.0
 -16.0  -43.0   98.0

julia> C = cholesky(A)
Cholesky{Float64,Array{Float64,2}}
U factor:
3×3 UpperTriangular{Float64,Array{Float64,2}}:
 2.0  6.0  -8.0
  ⋅   1.0   5.0
  ⋅    ⋅    3.0

julia> C.U
3×3 UpperTriangular{Float64,Array{Float64,2}}:
 2.0  6.0  -8.0
  ⋅   1.0   5.0
  ⋅    ⋅    3.0

julia> C.L
3×3 LowerTriangular{Float64,Array{Float64,2}}:
  2.0   ⋅    ⋅
  6.0  1.0   ⋅
 -8.0  5.0  3.0

julia> C.L * C.U == A
true
```
"""
cholesky(A::Union{StridedMatrix,RealHermSymComplexHerm{<:Real,<:StridedMatrix}},
    ::Val{false}=Val(false); check::Bool = true) = cholesky!(cholcopy(A); check = check)


## With pivoting
"""
    cholesky(A, Val(true); tol = 0.0, check = true) -> CholeskyPivoted

Compute the pivoted Cholesky factorization of a dense symmetric positive semi-definite matrix `A`
and return a [`CholeskyPivoted`](@ref) factorization. The matrix `A` can either be a [`Symmetric`](@ref)
or [`Hermitian`](@ref) [`StridedMatrix`](@ref) or a *perfectly* symmetric or Hermitian `StridedMatrix`.
The triangular Cholesky factor can be obtained from the factorization `F` with: `F.L` and `F.U`.
The following functions are available for `CholeskyPivoted` objects:
[`size`](@ref), [`\\`](@ref), [`inv`](@ref), [`det`](@ref), and [`rank`](@ref).
The argument `tol` determines the tolerance for determining the rank.
For negative values, the tolerance is the machine precision.

If you have a matrix `A` that is slightly non-Hermitian due to roundoff errors in its construction,
wrap it in `Hermitian(A)` before passing it to `cholesky` in order to treat it as perfectly Hermitian.

When `check = true`, an error is thrown if the decomposition fails.
When `check = false`, responsibility for checking the decomposition's
validity (via [`issuccess`](@ref)) lies with the user.
"""
cholesky(A::Union{StridedMatrix,RealHermSymComplexHerm{<:Real,<:StridedMatrix}},
    ::Val{true}; tol = 0.0, check::Bool = true) =
    cholesky!(cholcopy(A), Val(true); tol = tol, check = check)

## Number
function cholesky(x::Number, uplo::Symbol=:U)
    C, info = _chol!(x, uplo)
    xf = fill(C, 1, 1)
    Cholesky(xf, uplo, info)
end


function Cholesky{T}(C::Cholesky) where T
    Cnew = convert(AbstractMatrix{T}, C.factors)
    Cholesky{T, typeof(Cnew)}(Cnew, C.uplo, C.info)
end
Factorization{T}(C::Cholesky{T}) where {T} = C
Factorization{T}(C::Cholesky) where {T} = Cholesky{T}(C)
CholeskyPivoted{T}(C::CholeskyPivoted{T}) where {T} = C
CholeskyPivoted{T}(C::CholeskyPivoted) where {T} =
    CholeskyPivoted(AbstractMatrix{T}(C.factors),C.uplo,C.piv,C.rank,C.tol,C.info)
Factorization{T}(C::CholeskyPivoted{T}) where {T} = C
Factorization{T}(C::CholeskyPivoted) where {T} = CholeskyPivoted{T}(C)

AbstractMatrix(C::Cholesky) = C.uplo == 'U' ? C.U'C.U : C.L*C.L'
AbstractArray(C::Cholesky) = AbstractMatrix(C)
Matrix(C::Cholesky) = Array(AbstractArray(C))
Array(C::Cholesky) = Matrix(C)

function AbstractMatrix(F::CholeskyPivoted)
    ip = invperm(F.p)
    U = F.U[1:F.rank,ip]
    U'U
end
AbstractArray(F::CholeskyPivoted) = AbstractMatrix(F)
Matrix(F::CholeskyPivoted) = Array(AbstractArray(F))
Array(F::CholeskyPivoted) = Matrix(F)

copy(C::Cholesky) = Cholesky(copy(C.factors), C.uplo, C.info)
copy(C::CholeskyPivoted) = CholeskyPivoted(copy(C.factors), C.uplo, C.piv, C.rank, C.tol, C.info)

size(C::Union{Cholesky, CholeskyPivoted}) = size(C.factors)
size(C::Union{Cholesky, CholeskyPivoted}, d::Integer) = size(C.factors, d)

function getproperty(C::Cholesky, d::Symbol)
    Cfactors = getfield(C, :factors)
    Cuplo    = getfield(C, :uplo)
    if d === :U
        return UpperTriangular(Cuplo === char_uplo(d) ? Cfactors : copy(Cfactors'))
    elseif d === :L
        return LowerTriangular(Cuplo === char_uplo(d) ? Cfactors : copy(Cfactors'))
    elseif d === :UL
        return (Cuplo === 'U' ? UpperTriangular(Cfactors) : LowerTriangular(Cfactors))
    else
        return getfield(C, d)
    end
end
Base.propertynames(F::Cholesky, private::Bool=false) =
    (:U, :L, :UL, (private ? fieldnames(typeof(F)) : ())...)

function getproperty(C::CholeskyPivoted{T}, d::Symbol) where T<:BlasFloat
    Cfactors = getfield(C, :factors)
    Cuplo    = getfield(C, :uplo)
    if d === :U
        return UpperTriangular(sym_uplo(Cuplo) == d ? Cfactors : copy(Cfactors'))
    elseif d === :L
        return LowerTriangular(sym_uplo(Cuplo) == d ? Cfactors : copy(Cfactors'))
    elseif d === :p
        return getfield(C, :piv)
    elseif d === :P
        n = size(C, 1)
        P = zeros(T, n, n)
        for i = 1:n
            P[getfield(C, :piv)[i], i] = one(T)
        end
        return P
    else
        return getfield(C, d)
    end
end
Base.propertynames(F::CholeskyPivoted, private::Bool=false) =
    (:U, :L, :p, :P, (private ? fieldnames(typeof(F)) : ())...)

issuccess(C::Union{Cholesky,CholeskyPivoted}) = C.info == 0

function show(io::IO, mime::MIME{Symbol("text/plain")}, C::Cholesky{<:Any,<:AbstractMatrix})
    if issuccess(C)
        summary(io, C); println(io)
        println(io, "$(C.uplo) factor:")
        show(io, mime, C.UL)
    else
        print(io, "Failed factorization of type $(typeof(C))")
    end
end

function show(io::IO, mime::MIME{Symbol("text/plain")}, C::CholeskyPivoted{<:Any,<:AbstractMatrix})
    summary(io, C); println(io)
    println(io, "$(C.uplo) factor with rank $(rank(C)):")
    show(io, mime, C.uplo == 'U' ? C.U : C.L)
    println(io, "\npermutation:")
    show(io, mime, C.p)
end

ldiv!(C::Cholesky{T,<:AbstractMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.potrs!(C.uplo, C.factors, B)

function ldiv!(C::Cholesky{<:Any,<:AbstractMatrix}, B::StridedVecOrMat)
    if C.uplo == 'L'
        return ldiv!(adjoint(LowerTriangular(C.factors)), ldiv!(LowerTriangular(C.factors), B))
    else
        return ldiv!(UpperTriangular(C.factors), ldiv!(adjoint(UpperTriangular(C.factors)), B))
    end
end

function ldiv!(C::CholeskyPivoted{T}, B::StridedVector{T}) where T<:BlasFloat
    invpermute!(LAPACK.potrs!(C.uplo, C.factors, permute!(B, C.piv)), C.piv)
end
function ldiv!(C::CholeskyPivoted{T}, B::StridedMatrix{T}) where T<:BlasFloat
    n = size(C, 1)
    for i=1:size(B, 2)
        permute!(view(B, 1:n, i), C.piv)
    end
    LAPACK.potrs!(C.uplo, C.factors, B)
    for i=1:size(B, 2)
        invpermute!(view(B, 1:n, i), C.piv)
    end
    B
end

function ldiv!(C::CholeskyPivoted, B::StridedVector)
    if C.uplo == 'L'
        ldiv!(adjoint(LowerTriangular(C.factors)),
            ldiv!(LowerTriangular(C.factors), permute!(B, C.piv)))
    else
        ldiv!(UpperTriangular(C.factors),
            ldiv!(adjoint(UpperTriangular(C.factors)), permute!(B, C.piv)))
    end
    invpermute!(B, C.piv)
end

function ldiv!(C::CholeskyPivoted, B::StridedMatrix)
    n = size(C, 1)
    for i in 1:size(B, 2)
        permute!(view(B, 1:n, i), C.piv)
    end
    if C.uplo == 'L'
        ldiv!(adjoint(LowerTriangular(C.factors)),
            ldiv!(LowerTriangular(C.factors), B))
    else
        ldiv!(UpperTriangular(C.factors),
            ldiv!(adjoint(UpperTriangular(C.factors)), B))
    end
    for i in 1:size(B, 2)
        invpermute!(view(B, 1:n, i), C.piv)
    end
    B
end

function rdiv!(B::StridedMatrix, C::Cholesky{<:Any,<:AbstractMatrix})
    if C.uplo == 'L'
        return rdiv!(rdiv!(B, adjoint(LowerTriangular(C.factors))), LowerTriangular(C.factors))
    else
        return rdiv!(rdiv!(B, UpperTriangular(C.factors)), adjoint(UpperTriangular(C.factors)))
    end
end

function LinearAlgebra.rdiv!(B::StridedMatrix, C::CholeskyPivoted)
    n = size(C, 2)
    for i in 1:size(B, 1)
        permute!(view(B, i, 1:n), C.piv)
    end
    if C.uplo == 'L'
        rdiv!(rdiv!(B, adjoint(LowerTriangular(C.factors))),
            LowerTriangular(C.factors))
    else
        rdiv!(rdiv!(B, UpperTriangular(C.factors)),
            adjoint(UpperTriangular(C.factors)))
    end
    for i in 1:size(B, 1)
        invpermute!(view(B, i, 1:n), C.piv)
    end
    B
end

isposdef(C::Union{Cholesky,CholeskyPivoted}) = C.info == 0

function det(C::Cholesky)
    dd = one(real(eltype(C)))
    @inbounds for i in 1:size(C.factors,1)
        dd *= real(C.factors[i,i])^2
    end
    return dd
end

function logdet(C::Cholesky)
    dd = zero(real(eltype(C)))
    @inbounds for i in 1:size(C.factors,1)
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

inv!(C::Cholesky{<:BlasFloat,<:StridedMatrix}) =
    copytri!(LAPACK.potri!(C.uplo, C.factors), C.uplo, true)

inv(C::Cholesky{<:BlasFloat,<:StridedMatrix}) = inv!(copy(C))

function inv(C::CholeskyPivoted)
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

Update a Cholesky factorization `C` with the vector `v`. If `A = C.U'C.U` then
`CC = cholesky(C.U'C.U + v*v')` but the computation of `CC` only uses `O(n^2)`
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

Downdate a Cholesky factorization `C` with the vector `v`. If `A = C.U'C.U` then
`CC = cholesky(C.U'C.U - v*v')` but the computation of `CC` only uses `O(n^2)`
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
            throw(LinearAlgebra.PosDefException(i))
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

Update a Cholesky factorization `C` with the vector `v`. If `A = C.U'C.U`
then `CC = cholesky(C.U'C.U + v*v')` but the computation of `CC` only uses
`O(n^2)` operations.
"""
lowrankupdate(C::Cholesky, v::StridedVector) = lowrankupdate!(copy(C), copy(v))

"""
    lowrankdowndate(C::Cholesky, v::StridedVector) -> CC::Cholesky

Downdate a Cholesky factorization `C` with the vector `v`. If `A = C.U'C.U`
then `CC = cholesky(C.U'C.U - v*v')` but the computation of `CC` only uses
`O(n^2)` operations.
"""
lowrankdowndate(C::Cholesky, v::StridedVector) = lowrankdowndate!(copy(C), copy(v))
