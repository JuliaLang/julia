# This file is a part of Julia. License is MIT: https://julialang.org/license

# Symmetric and Hermitian matrices
struct Symmetric{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    data::S
    uplo::Char
end
"""
    Symmetric(A, uplo=:U)

Construct a `Symmetric` view of the upper (if `uplo = :U`) or lower (if `uplo = :L`)
triangle of the matrix `A`.

# Examples
```jldoctest
julia> A = [1 0 2 0 3; 0 4 0 5 0; 6 0 7 0 8; 0 9 0 1 0; 2 0 3 0 4]
5×5 Array{Int64,2}:
 1  0  2  0  3
 0  4  0  5  0
 6  0  7  0  8
 0  9  0  1  0
 2  0  3  0  4

julia> Supper = Symmetric(A)
5×5 Symmetric{Int64,Array{Int64,2}}:
 1  0  2  0  3
 0  4  0  5  0
 2  0  7  0  8
 0  5  0  1  0
 3  0  8  0  4

julia> Slower = Symmetric(A, :L)
5×5 Symmetric{Int64,Array{Int64,2}}:
 1  0  6  0  2
 0  4  0  9  0
 6  0  7  0  3
 0  9  0  1  0
 2  0  3  0  4
```

Note that `Supper` will not be equal to `Slower` unless `A` is itself symmetric (e.g. if `A == A.'`).
"""
Symmetric(A::AbstractMatrix, uplo::Symbol=:U) = (checksquare(A); Symmetric{eltype(A),typeof(A)}(A, char_uplo(uplo)))

struct Hermitian{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    data::S
    uplo::Char
end
"""
    Hermitian(A, uplo=:U)

Construct a `Hermitian` view of the upper (if `uplo = :U`) or lower (if `uplo = :L`)
triangle of the matrix `A`.

# Examples
```jldoctest
julia> A = [1 0 2+2im 0 3-3im; 0 4 0 5 0; 6-6im 0 7 0 8+8im; 0 9 0 1 0; 2+2im 0 3-3im 0 4];

julia> Hupper = Hermitian(A)
5×5 Hermitian{Complex{Int64},Array{Complex{Int64},2}}:
 1+0im  0+0im  2+2im  0+0im  3-3im
 0+0im  4+0im  0+0im  5+0im  0+0im
 2-2im  0+0im  7+0im  0+0im  8+8im
 0+0im  5+0im  0+0im  1+0im  0+0im
 3+3im  0+0im  8-8im  0+0im  4+0im

julia> Hlower = Hermitian(A, :L)
5×5 Hermitian{Complex{Int64},Array{Complex{Int64},2}}:
 1+0im  0+0im  6+6im  0+0im  2-2im
 0+0im  4+0im  0+0im  9+0im  0+0im
 6-6im  0+0im  7+0im  0+0im  3+3im
 0+0im  9+0im  0+0im  1+0im  0+0im
 2+2im  0+0im  3-3im  0+0im  4+0im
```

Note that `Hupper` will not be equal to `Hlower` unless `A` is itself Hermitian (e.g. if `A == A'`).
"""
function Hermitian(A::AbstractMatrix, uplo::Symbol=:U)
    n = checksquare(A)
    for i=1:n
        isreal(A[i, i]) || throw(ArgumentError(
            "Cannot construct Hermitian from matrix with nonreal diagonals"))
    end
    Hermitian{eltype(A),typeof(A)}(A, char_uplo(uplo))
end

for (S, H) in ((:Symmetric, :Hermitian), (:Hermitian, :Symmetric))
    @eval begin
        $S(A::$S) = A
        function $S(A::$S, uplo::Symbol)
            if A.uplo == char_uplo(uplo)
                return A
            else
                throw(ArgumentError("Cannot construct $($S); uplo doesn't match"))
            end
        end
        $S(A::$H) = $S(A.data, Symbol(A.uplo))
        function $S(A::$H, uplo::Symbol)
            if A.uplo == char_uplo(uplo)
                return $S(A.data, Symbol(A.uplo))
            else
                throw(ArgumentError("Cannot construct $($S); uplo doesn't match"))
            end
        end
    end
end

const HermOrSym{T,S} = Union{Hermitian{T,S}, Symmetric{T,S}}
const RealHermSymComplexHerm{T<:Real,S} = Union{Hermitian{T,S}, Symmetric{T,S}, Hermitian{Complex{T},S}}
const RealHermSymComplexSym{T<:Real,S} = Union{Hermitian{T,S}, Symmetric{T,S}, Symmetric{Complex{T},S}}

size(A::HermOrSym, d) = size(A.data, d)
size(A::HermOrSym) = size(A.data)
@inline function getindex(A::Symmetric, i::Integer, j::Integer)
    @boundscheck checkbounds(A, i, j)
    @inbounds r = (A.uplo == 'U') == (i < j) ? A.data[i, j] : A.data[j, i]
    r
end
@inline function getindex(A::Hermitian, i::Integer, j::Integer)
    @boundscheck checkbounds(A, i, j)
    @inbounds r = (A.uplo == 'U') == (i < j) ? A.data[i, j] : conj(A.data[j, i])
    r
end

function setindex!(A::Symmetric, v, i::Integer, j::Integer)
    i == j || throw(ArgumentError("Cannot set a non-diagonal index in a symmetric matrix"))
    setindex!(A.data, v, i, j)
end

function setindex!(A::Hermitian, v, i::Integer, j::Integer)
    if i != j
        throw(ArgumentError("Cannot set a non-diagonal index in a Hermitian matrix"))
    elseif !isreal(v)
        throw(ArgumentError("Cannot set a diagonal entry in a Hermitian matrix to a nonreal value"))
    else
        setindex!(A.data, v, i, j)
    end
end

similar(A::Symmetric, ::Type{T}) where {T} = Symmetric(similar(A.data, T))
# Hermitian version can be simplified when check for imaginary part of
# diagonal in Hermitian has been removed
function similar(A::Hermitian, ::Type{T}) where T
    B = similar(A.data, T)
    for i = 1:size(A,1)
        B[i,i] = 0
    end
    return Hermitian(B)
end

# Conversion
convert(::Type{Matrix}, A::Symmetric) = copytri!(convert(Matrix, copy(A.data)), A.uplo)
convert(::Type{Matrix}, A::Hermitian) = copytri!(convert(Matrix, copy(A.data)), A.uplo, true)
convert(::Type{Array}, A::Union{Symmetric,Hermitian}) = convert(Matrix, A)
full(A::Union{Symmetric,Hermitian}) = convert(Array, A)
parent(A::HermOrSym) = A.data
convert(::Type{Symmetric{T,S}},A::Symmetric{T,S}) where {T,S<:AbstractMatrix} = A
convert(::Type{Symmetric{T,S}},A::Symmetric) where {T,S<:AbstractMatrix} = Symmetric{T,S}(convert(S,A.data),A.uplo)
convert(::Type{AbstractMatrix{T}}, A::Symmetric) where {T} = Symmetric(convert(AbstractMatrix{T}, A.data), Symbol(A.uplo))
convert(::Type{Hermitian{T,S}},A::Hermitian{T,S}) where {T,S<:AbstractMatrix} = A
convert(::Type{Hermitian{T,S}},A::Hermitian) where {T,S<:AbstractMatrix} = Hermitian{T,S}(convert(S,A.data),A.uplo)
convert(::Type{AbstractMatrix{T}}, A::Hermitian) where {T} = Hermitian(convert(AbstractMatrix{T}, A.data), Symbol(A.uplo))

copy(A::Symmetric{T,S}) where {T,S} = (B = copy(A.data); Symmetric{T,typeof(B)}(B,A.uplo))
copy(A::Hermitian{T,S}) where {T,S} = (B = copy(A.data); Hermitian{T,typeof(B)}(B,A.uplo))

function copy!(dest::Symmetric, src::Symmetric)
    if src.uplo == dest.uplo
        copy!(dest.data, src.data)
    else
        transpose!(dest.data, src.data)
    end
    return dest
end

function copy!(dest::Hermitian, src::Hermitian)
    if src.uplo == dest.uplo
        copy!(dest.data, src.data)
    else
        ctranspose!(dest.data, src.data)
    end
    return dest
end

function Base.isreal(A::HermOrSym)
    n = size(A, 1)
    @inbounds if A.uplo == 'U'
        for j in 1:n
            for i in 1:(j - (A isa Hermitian))
                if !isreal(A.data[i,j])
                    return false
                end
            end
        end
    else
        for j in 1:n
            for i in (j + (A isa Hermitian)):n
                if !isreal(A.data[i,j])
                    return false
                end
            end
        end
    end
    return true
end

ishermitian(A::Hermitian) = true
ishermitian(A::Symmetric{<:Real}) = true
ishermitian(A::Symmetric{<:Complex}) = isreal(A)
issymmetric(A::Hermitian{<:Real}) = true
issymmetric(A::Hermitian{<:Complex}) = isreal(A)
issymmetric(A::Symmetric) = true
transpose(A::Symmetric) = A
transpose(A::Hermitian{<:Real}) = A
ctranspose(A::Symmetric{<:Real}) = A
function ctranspose(A::Symmetric)
    AC = ctranspose(A.data)
    return Symmetric(AC, ifelse(A.uplo == 'U', :L, :U))
end
function transpose(A::Hermitian)
    AT = transpose(A.data)
    return Hermitian(AT, ifelse(A.uplo == 'U', :L, :U))
end
ctranspose(A::Hermitian) = A
trace(A::Hermitian) = real(trace(A.data))

Base.conj(A::HermOrSym) = typeof(A)(conj(A.data), A.uplo)
Base.conj!(A::HermOrSym) = typeof(A)(conj!(A.data), A.uplo)

# tril/triu
function tril(A::Hermitian, k::Integer=0)
    if A.uplo == 'U' && k <= 0
        return tril!(A.data',k)
    elseif A.uplo == 'U' && k > 0
        return tril!(A.data',-1) + tril!(triu(A.data),k)
    elseif A.uplo == 'L' && k <= 0
        return tril(A.data,k)
    else
        return tril(A.data,-1) + tril!(triu!(A.data'),k)
    end
end

function tril(A::Symmetric, k::Integer=0)
    if A.uplo == 'U' && k <= 0
        return tril!(A.data.',k)
    elseif A.uplo == 'U' && k > 0
        return tril!(A.data.',-1) + tril!(triu(A.data),k)
    elseif A.uplo == 'L' && k <= 0
        return tril(A.data,k)
    else
        return tril(A.data,-1) + tril!(triu!(A.data.'),k)
    end
end

function triu(A::Hermitian, k::Integer=0)
    if A.uplo == 'U' && k >= 0
        return triu(A.data,k)
    elseif A.uplo == 'U' && k < 0
        return triu(A.data,1) + triu!(tril!(A.data'),k)
    elseif A.uplo == 'L' && k >= 0
        return triu!(A.data',k)
    else
        return triu!(A.data',1) + triu!(tril(A.data),k)
    end
end

function triu(A::Symmetric, k::Integer=0)
    if A.uplo == 'U' && k >= 0
        return triu(A.data,k)
    elseif A.uplo == 'U' && k < 0
        return triu(A.data,1) + triu!(tril!(A.data.'),k)
    elseif A.uplo == 'L' && k >= 0
        return triu!(A.data.',k)
    else
        return triu!(A.data.',1) + triu!(tril(A.data),k)
    end
end

(-)(A::Symmetric{Tv,S}) where {Tv,S} = Symmetric{Tv,S}(-A.data, A.uplo)
(-)(A::Hermitian{Tv,S}) where {Tv,S} = Hermitian{Tv,S}(-A.data, A.uplo)

## Matvec
A_mul_B!(y::StridedVector{T}, A::Symmetric{T,<:StridedMatrix}, x::StridedVector{T}) where {T<:BlasFloat} =
    BLAS.symv!(A.uplo, one(T), A.data, x, zero(T), y)
A_mul_B!(y::StridedVector{T}, A::Hermitian{T,<:StridedMatrix}, x::StridedVector{T}) where {T<:BlasReal} =
    BLAS.symv!(A.uplo, one(T), A.data, x, zero(T), y)
A_mul_B!(y::StridedVector{T}, A::Hermitian{T,<:StridedMatrix}, x::StridedVector{T}) where {T<:BlasComplex} =
    BLAS.hemv!(A.uplo, one(T), A.data, x, zero(T), y)
## Matmat
A_mul_B!(C::StridedMatrix{T}, A::Symmetric{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat} =
    BLAS.symm!('L', A.uplo, one(T), A.data, B, zero(T), C)
A_mul_B!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Symmetric{T,<:StridedMatrix}) where {T<:BlasFloat} =
    BLAS.symm!('R', B.uplo, one(T), B.data, A, zero(T), C)
A_mul_B!(C::StridedMatrix{T}, A::Hermitian{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasReal} =
    BLAS.symm!('L', A.uplo, one(T), A.data, B, zero(T), C)
A_mul_B!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Hermitian{T,<:StridedMatrix}) where {T<:BlasReal} =
    BLAS.symm!('R', B.uplo, one(T), B.data, A, zero(T), C)
A_mul_B!(C::StridedMatrix{T}, A::Hermitian{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasComplex} =
    BLAS.hemm!('L', A.uplo, one(T), A.data, B, zero(T), C)
A_mul_B!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Hermitian{T,<:StridedMatrix}) where {T<:BlasComplex} =
    BLAS.hemm!('R', B.uplo, one(T), B.data, A, zero(T), C)

*(A::HermOrSym, B::HermOrSym) = A*full(B)

# Fallbacks to avoid generic_matvecmul!/generic_matmatmul!
## Symmetric{<:Number} and Hermitian{<:Real} are invariant to transpose; peel off the t
At_mul_B(A::RealHermSymComplexSym, B::AbstractVector) = A*B
At_mul_B(A::RealHermSymComplexSym, B::AbstractMatrix) = A*B
A_mul_Bt(A::AbstractMatrix, B::RealHermSymComplexSym) = A*B
## Hermitian{<:Number} and Symmetric{<:Real} are invariant to ctranspose; peel off the c
Ac_mul_B(A::RealHermSymComplexHerm, B::AbstractVector) = A*B
Ac_mul_B(A::RealHermSymComplexHerm, B::AbstractMatrix) = A*B
A_mul_Bc(A::AbstractMatrix, B::RealHermSymComplexHerm) = A*B

# ambiguities with RowVector
A_mul_Bt(A::RowVector, B::RealHermSymComplexSym) = A*B
A_mul_Bc(A::RowVector, B::RealHermSymComplexHerm) = A*B
# ambiguities with AbstractTriangular
At_mul_B(A::RealHermSymComplexSym, B::AbstractTriangular) = A*B
A_mul_Bt(A::AbstractTriangular, B::RealHermSymComplexSym) = A*B
Ac_mul_B(A::RealHermSymComplexHerm, B::AbstractTriangular) = A*B
A_mul_Bc(A::AbstractTriangular, B::RealHermSymComplexHerm) = A*B

for T in (:Symmetric, :Hermitian), op in (:+, :-, :*, :/)
    # Deal with an ambiguous case
    @eval ($op)(A::$T, x::Bool) = ($T)(($op)(A.data, x), Symbol(A.uplo))
    S = T == :Hermitian ? :Real : :Number
    @eval ($op)(A::$T, x::$S) = ($T)(($op)(A.data, x), Symbol(A.uplo))
end

function factorize(A::HermOrSym{T}) where T
    TT = typeof(sqrt(one(T)))
    if TT <: BlasFloat
        return bkfact(A)
    else # fallback
        return lufact(A)
    end
end

det(A::RealHermSymComplexHerm) = real(det(factorize(A)))
det(A::Symmetric{<:Real}) = det(factorize(A))
det(A::Symmetric) = det(factorize(A))

\(A::HermOrSym{<:Any,<:StridedMatrix}, B::AbstractVector) = \(factorize(A), B)
# Bunch-Kaufman solves can not utilize BLAS-3 for multiple right hand sides
# so using LU is faster for AbstractMatrix right hand side
\(A::HermOrSym{<:Any,<:StridedMatrix}, B::AbstractMatrix) = \(lufact(A), B)
# ambiguity with RowVector
\(A::HermOrSym{<:Any,<:StridedMatrix}, B::RowVector) = invoke(\, Tuple{AbstractMatrix, RowVector}, A, B)

function _inv(A::HermOrSym)
    n = checksquare(A)
    B = inv!(lufact(A))
    conjugate = isa(A, Hermitian)
    # symmetrize
    if A.uplo == 'U' # add to upper triangle
        @inbounds for i = 1:n, j = i:n
            B[i,j] = conjugate ? (B[i,j] + conj(B[j,i])) / 2 : (B[i,j] + B[j,i]) / 2
        end
    else # A.uplo == 'L', add to lower triangle
        @inbounds for i = 1:n, j = i:n
            B[j,i] = conjugate ? (B[j,i] + conj(B[i,j])) / 2 : (B[j,i] + B[i,j]) / 2
        end
    end
    B
end
inv(A::Hermitian{T,S}) where {T<:BlasFloat,S<:StridedMatrix} = Hermitian{T,S}(_inv(A), A.uplo)
inv(A::Symmetric{T,S}) where {T<:BlasFloat,S<:StridedMatrix} = Symmetric{T,S}(_inv(A), A.uplo)

eigfact!(A::RealHermSymComplexHerm{<:BlasReal,<:StridedMatrix}) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)...)

function eigfact(A::RealHermSymComplexHerm)
    T = eltype(A)
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A))
end

eigfact!(A::RealHermSymComplexHerm{<:BlasReal,<:StridedMatrix}, irange::UnitRange) = Eigen(LAPACK.syevr!('V', 'I', A.uplo, A.data, 0.0, 0.0, irange.start, irange.stop, -1.0)...)

"""
    eigfact(A::Union{SymTridiagonal, Hermitian, Symmetric}, irange::UnitRange) -> Eigen

Computes the eigenvalue decomposition of `A`, returning an `Eigen` factorization object `F`
which contains the eigenvalues in `F[:values]` and the eigenvectors in the columns of the
matrix `F[:vectors]`. (The `k`th eigenvector can be obtained from the slice `F[:vectors][:, k]`.)

The following functions are available for `Eigen` objects: [`inv`](@ref), [`det`](@ref), and [`isposdef`](@ref).

The `UnitRange` `irange` specifies indices of the sorted eigenvalues to search for.

!!! note
    If `irange` is not `1:n`, where `n` is the dimension of `A`, then the returned factorization
    will be a *truncated* factorization.
"""
function eigfact(A::RealHermSymComplexHerm, irange::UnitRange)
    T = eltype(A)
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), irange)
end

eigfact!(A::RealHermSymComplexHerm{T,<:StridedMatrix}, vl::Real, vh::Real) where {T<:BlasReal} =
    Eigen(LAPACK.syevr!('V', 'V', A.uplo, A.data, convert(T, vl), convert(T, vh), 0, 0, -1.0)...)

"""
    eigfact(A::Union{SymTridiagonal, Hermitian, Symmetric}, vl::Real, vu::Real) -> Eigen

Computes the eigenvalue decomposition of `A`, returning an `Eigen` factorization object `F`
which contains the eigenvalues in `F[:values]` and the eigenvectors in the columns of the
matrix `F[:vectors]`. (The `k`th eigenvector can be obtained from the slice `F[:vectors][:, k]`.)

The following functions are available for `Eigen` objects: [`inv`](@ref), [`det`](@ref), and [`isposdef`](@ref).

`vl` is the lower bound of the window of eigenvalues to search for, and `vu` is the upper bound.

!!! note
    If [`vl`, `vu`] does not contain all eigenvalues of `A`, then the returned factorization
    will be a *truncated* factorization.
"""
function eigfact(A::RealHermSymComplexHerm, vl::Real, vh::Real)
    T = eltype(A)
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), vl, vh)
end

eigvals!(A::RealHermSymComplexHerm{<:BlasReal,<:StridedMatrix}) =
    LAPACK.syevr!('N', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)[1]

function eigvals(A::RealHermSymComplexHerm)
    T = eltype(A)
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
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
eigenvalues by specifying a `UnitRange` `irange` covering indices of the sorted eigenvalues,
e.g. the 2nd to 8th eigenvalues.

```jldoctest
julia> A = SymTridiagonal([1.; 2.; 1.], [2.; 3.])
3×3 SymTridiagonal{Float64}:
 1.0  2.0   ⋅
 2.0  2.0  3.0
  ⋅   3.0  1.0

julia> eigvals(A, 2:2)
1-element Array{Float64,1}:
 1.0

julia> eigvals(A)
3-element Array{Float64,1}:
 -2.14005
  1.0
  5.14005
```
"""
function eigvals(A::RealHermSymComplexHerm, irange::UnitRange)
    T = eltype(A)
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
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

```jldoctest
julia> A = SymTridiagonal([1.; 2.; 1.], [2.; 3.])
3×3 SymTridiagonal{Float64}:
 1.0  2.0   ⋅
 2.0  2.0  3.0
  ⋅   3.0  1.0

julia> eigvals(A, -1, 2)
1-element Array{Float64,1}:
 1.0

julia> eigvals(A)
3-element Array{Float64,1}:
 -2.14005
  1.0
  5.14005
```
"""
function eigvals(A::RealHermSymComplexHerm, vl::Real, vh::Real)
    T = eltype(A)
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    eigvals!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), vl, vh)
end

eigmax(A::RealHermSymComplexHerm{<:Real,<:StridedMatrix}) = eigvals(A, size(A, 1):size(A, 1))[1]
eigmin(A::RealHermSymComplexHerm{<:Real,<:StridedMatrix}) = eigvals(A, 1:1)[1]

function eigfact!(A::HermOrSym{T,S}, B::HermOrSym{T,S}) where {T<:BlasReal,S<:StridedMatrix}
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')
    GeneralizedEigen(vals, vecs)
end
function eigfact!(A::Hermitian{T,S}, B::Hermitian{T,S}) where {T<:BlasComplex,S<:StridedMatrix}
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')
    GeneralizedEigen(vals, vecs)
end

eigvals!(A::HermOrSym{T,S}, B::HermOrSym{T,S}) where {T<:BlasReal,S<:StridedMatrix} =
    LAPACK.sygvd!(1, 'N', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')[1]
eigvals!(A::Hermitian{T,S}, B::Hermitian{T,S}) where {T<:BlasComplex,S<:StridedMatrix} =
    LAPACK.sygvd!(1, 'N', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')[1]

eigvecs(A::HermOrSym) = eigvecs(eigfact(A))

function svdvals!(A::RealHermSymComplexHerm)
    vals = eigvals!(A)
    for i = 1:length(vals)
        vals[i] = abs(vals[i])
    end
    return sort!(vals, rev = true)
end

# Matrix functions
function ^(A::Symmetric{T}, p::Integer) where T<:Real
    if p < 0
        return Symmetric(Base.power_by_squaring(inv(A), -p))
    else
        return Symmetric(Base.power_by_squaring(A, p))
    end
end
function ^(A::Symmetric{T}, p::Real) where T<:Real
    F = eigfact(A)
    if all(λ -> λ ≥ 0, F.values)
        retmat = (F.vectors * Diagonal((F.values).^p)) * F.vectors'
    else
        retmat = (F.vectors * Diagonal((complex(F.values)).^p)) * F.vectors'
    end
    return Symmetric(retmat)
end
function ^(A::Hermitian, p::Integer)
    n = checksquare(A)
    if p < 0
        retmat = Base.power_by_squaring(inv(A), -p)
    else
        retmat = Base.power_by_squaring(A, p)
    end
    for i = 1:n
        retmat[i,i] = real(retmat[i,i])
    end
    return Hermitian(retmat)
end
function ^(A::Hermitian{T}, p::Real) where T
    n = checksquare(A)
    F = eigfact(A)
    if all(λ -> λ ≥ 0, F.values)
        retmat = (F.vectors * Diagonal((F.values).^p)) * F.vectors'
        if T <: Real
            return Hermitian(retmat)
        else
            for i = 1:n
                retmat[i,i] = real(retmat[i,i])
            end
            return Hermitian(retmat)
        end
    else
        retmat = (F.vectors * Diagonal((complex(F.values).^p))) * F.vectors'
        return retmat
    end
end

function expm(A::Symmetric)
    F = eigfact(A)
    return Symmetric((F.vectors * Diagonal(exp.(F.values))) * F.vectors')
end
function expm(A::Hermitian{T}) where T
    n = checksquare(A)
    F = eigfact(A)
    retmat = (F.vectors * Diagonal(exp.(F.values))) * F.vectors'
    if T <: Real
        return real(Hermitian(retmat))
    else
        for i = 1:n
            retmat[i,i] = real(retmat[i,i])
        end
        return Hermitian(retmat)
    end
end

for (funm, func) in ([:logm,:log], [:sqrtm,:sqrt])
    @eval begin
        function ($funm)(A::Symmetric{T}) where T<:Real
            F = eigfact(A)
            if all(λ -> λ ≥ 0, F.values)
                retmat = (F.vectors * Diagonal(($func).(F.values))) * F.vectors'
            else
                retmat = (F.vectors * Diagonal(($func).(complex.(F.values)))) * F.vectors'
            end
            return Symmetric(retmat)
        end

        function ($funm)(A::Hermitian{T}) where T
            n = checksquare(A)
            F = eigfact(A)
            if all(λ -> λ ≥ 0, F.values)
                retmat = (F.vectors * Diagonal(($func).(F.values))) * F.vectors'
                if T <: Real
                    return Hermitian(retmat)
                else
                    for i = 1:n
                        retmat[i,i] = real(retmat[i,i])
                    end
                    return Hermitian(retmat)
                end
            else
                retmat = (F.vectors * Diagonal(($func).(complex(F.values)))) * F.vectors'
                return retmat
            end
        end
    end
end
