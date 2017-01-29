# This file is a part of Julia. License is MIT: http://julialang.org/license

#Symmetric and Hermitian matrices
immutable Symmetric{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    data::S
    uplo::Char
end
"""
    Symmetric(A, uplo=:U)

Construct a `Symmetric` matrix from the upper (if `uplo = :U`) or lower (if `uplo = :L`) triangle of `A`.

# Example

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
Symmetric(A::AbstractMatrix, uplo::Symbol=:U) = (checksquare(A);Symmetric{eltype(A),typeof(A)}(A, char_uplo(uplo)))
immutable Hermitian{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    data::S
    uplo::Char
end
"""
    Hermitian(A, uplo=:U)

Construct a `Hermitian` matrix from the upper (if `uplo = :U`) or lower (if `uplo = :L`) triangle of `A`.

# Example

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

typealias HermOrSym{T,S} Union{Hermitian{T,S}, Symmetric{T,S}}
typealias RealHermSymComplexHerm{T<:Real,S} Union{Hermitian{T,S}, Symmetric{T,S}, Hermitian{Complex{T},S}}

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

similar{T}(A::Symmetric, ::Type{T}) = Symmetric(similar(A.data, T))
# Hermitian version can be simplified when check for imaginary part of
# diagonal in Hermitian has been removed
function similar{T}(A::Hermitian, ::Type{T})
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
convert{T,S<:AbstractMatrix}(::Type{Symmetric{T,S}},A::Symmetric{T,S}) = A
convert{T,S<:AbstractMatrix}(::Type{Symmetric{T,S}},A::Symmetric) = Symmetric{T,S}(convert(S,A.data),A.uplo)
convert{T}(::Type{AbstractMatrix{T}}, A::Symmetric) = Symmetric(convert(AbstractMatrix{T}, A.data), Symbol(A.uplo))
convert{T,S<:AbstractMatrix}(::Type{Hermitian{T,S}},A::Hermitian{T,S}) = A
convert{T,S<:AbstractMatrix}(::Type{Hermitian{T,S}},A::Hermitian) = Hermitian{T,S}(convert(S,A.data),A.uplo)
convert{T}(::Type{AbstractMatrix{T}}, A::Hermitian) = Hermitian(convert(AbstractMatrix{T}, A.data), Symbol(A.uplo))

copy{T,S}(A::Symmetric{T,S}) = (B = copy(A.data); Symmetric{T,typeof(B)}(B,A.uplo))
copy{T,S}(A::Hermitian{T,S}) = (B = copy(A.data); Hermitian{T,typeof(B)}(B,A.uplo))

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

ishermitian(A::Hermitian) = true
ishermitian{T<:Real,S}(A::Symmetric{T,S}) = true
ishermitian{T<:Complex,S}(A::Symmetric{T,S}) = isreal(A.data)
issymmetric{T<:Real,S}(A::Hermitian{T,S}) = true
issymmetric{T<:Complex,S}(A::Hermitian{T,S}) = isreal(A.data)
issymmetric(A::Symmetric) = true
transpose(A::Symmetric) = A
ctranspose{T<:Real}(A::Symmetric{T}) = A
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

#tril/triu
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

-{Tv,S<:AbstractMatrix}(A::Symmetric{Tv,S}) = Symmetric{Tv,S}(-A.data, A.uplo)

## Matvec
A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(y::StridedVector{T}, A::Symmetric{T,S}, x::StridedVector{T}) = BLAS.symv!(A.uplo, one(T), A.data, x, zero(T), y)
A_mul_B!{T<:BlasComplex,S<:StridedMatrix}(y::StridedVector{T}, A::Hermitian{T,S}, x::StridedVector{T}) = BLAS.hemv!(A.uplo, one(T), A.data, x, zero(T), y)
##Matmat
A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(C::StridedMatrix{T}, A::Symmetric{T,S}, B::StridedMatrix{T}) = BLAS.symm!('L', A.uplo, one(T), A.data, B, zero(T), C)
A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Symmetric{T,S}) = BLAS.symm!('R', B.uplo, one(T), B.data, A, zero(T), C)
A_mul_B!{T<:BlasComplex,S<:StridedMatrix}(C::StridedMatrix{T}, A::Hermitian{T,S}, B::StridedMatrix{T}) = BLAS.hemm!('L', A.uplo, one(T), A.data, B, zero(T), C)
A_mul_B!{T<:BlasComplex,S<:StridedMatrix}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Hermitian{T,S}) = BLAS.hemm!('R', B.uplo, one(T), B.data, A, zero(T), C)

*(A::HermOrSym, B::HermOrSym) = full(A)*full(B)
*(A::StridedMatrix, B::HermOrSym) = A*full(B)

for T in (:Symmetric, :Hermitian), op in (:+, :-, :*, :/)
    # Deal with an ambiguous case
    @eval ($op)(A::$T, x::Bool) = ($T)(($op)(A.data, x), Symbol(A.uplo))
    S = T == :Hermitian ? :Real : :Number
    @eval ($op)(A::$T, x::$S) = ($T)(($op)(A.data, x), Symbol(A.uplo))
end

bkfact(A::HermOrSym) = bkfact(A.data, Symbol(A.uplo), issymmetric(A))
factorize(A::HermOrSym) = bkfact(A)

# Is just RealHermSymComplexHerm, but type alias seems to be broken
det{T<:Real,S}(A::Union{Hermitian{T,S}, Symmetric{T,S}, Hermitian{Complex{T},S}}) = real(det(bkfact(A)))
det{T<:Real}(A::Symmetric{T}) = det(bkfact(A))
det(A::Symmetric) = det(bkfact(A))

\{T,S<:StridedMatrix}(A::HermOrSym{T,S}, B::StridedVecOrMat) = \(bkfact(A.data, Symbol(A.uplo), issymmetric(A)), B)

inv{T<:BlasFloat,S<:StridedMatrix}(A::Hermitian{T,S}) = Hermitian{T,S}(inv(bkfact(A.data, Symbol(A.uplo))), A.uplo)
inv{T<:BlasFloat,S<:StridedMatrix}(A::Symmetric{T,S}) = Symmetric{T,S}(inv(bkfact(A.data, Symbol(A.uplo), true)), A.uplo)

eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)...)
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigfact{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A)))

eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, irange::UnitRange) = Eigen(LAPACK.syevr!('V', 'I', A.uplo, A.data, 0.0, 0.0, irange.start, irange.stop, -1.0)...)
# Because of #6721 it is necessary to specify the parameters explicitly here.

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
eigfact{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}, irange::UnitRange) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), irange))

eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, vl::Real, vh::Real) = Eigen(LAPACK.syevr!('V', 'V', A.uplo, A.data, convert(T, vl), convert(T, vh), 0, 0, -1.0)...)
# Because of #6721 it is necessary to specify the parameters explicitly here.
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
eigfact{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}, vl::Real, vh::Real) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), vl, vh))

eigvals!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = LAPACK.syevr!('N', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)[1]
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigvals{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(AbstractMatrix{S}, A) : copy(A)))

"""
    eigvals!(A::Union{SymTridiagonal, Hermitian, Symmetric}, irange::UnitRange) -> values

Same as [`eigvals`](@ref), but saves space by overwriting the input `A`, instead of creating a copy.
`irange` is a range of eigenvalue *indices* to search for - for instance, the 2nd to 8th eigenvalues.
"""
eigvals!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, irange::UnitRange) = LAPACK.syevr!('N', 'I', A.uplo, A.data, 0.0, 0.0, irange.start, irange.stop, -1.0)[1]
# Because of #6721 it is necessary to specify the parameters explicitly here.
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
eigvals{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}, irange::UnitRange) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), irange))

"""
    eigvals!(A::Union{SymTridiagonal, Hermitian, Symmetric}, vl::Real, vu::Real) -> values

Same as [`eigvals`](@ref), but saves space by overwriting the input `A`, instead of creating a copy.
`vl` is the lower bound of the interval to search for eigenvalues, and `vu` is the upper bound.
"""
eigvals!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, vl::Real, vh::Real) = LAPACK.syevr!('N', 'V', A.uplo, A.data, convert(T, vl), convert(T, vh), 0, 0, -1.0)[1]
# Because of #6721 it is necessary to specify the parameters explicitly here.
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
eigvals{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}, vl::Real, vh::Real) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), vl, vh))

eigmax{T<:Real,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = eigvals(A, size(A, 1):size(A, 1))[1]
eigmin{T<:Real,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = eigvals(A, 1:1)[1]

function eigfact!{T<:BlasReal,S<:StridedMatrix}(A::HermOrSym{T,S}, B::HermOrSym{T,S})
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')
    GeneralizedEigen(vals, vecs)
end
function eigfact!{T<:BlasComplex,S<:StridedMatrix}(A::Hermitian{T,S}, B::Hermitian{T,S})
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')
    GeneralizedEigen(vals, vecs)
end

eigvals!{T<:BlasReal,S<:StridedMatrix}(A::HermOrSym{T,S}, B::HermOrSym{T,S}) = LAPACK.sygvd!(1, 'N', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')[1]
eigvals!{T<:BlasComplex,S<:StridedMatrix}(A::Hermitian{T,S}, B::Hermitian{T,S}) = LAPACK.sygvd!(1, 'N', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')[1]

function svdvals!{T<:Real,S}(A::Union{Hermitian{T,S}, Symmetric{T,S}, Hermitian{Complex{T},S}}) #  the union is the same as RealHermSymComplexHerm, but right now parametric typealiases are broken
    vals = eigvals!(A)
    for i = 1:length(vals)
        vals[i] = abs(vals[i])
    end
    return sort!(vals, rev = true)
end

#Matrix-valued functions
function expm(A::Symmetric)
    F = eigfact(A)
    return Symmetric((F.vectors * Diagonal(exp.(F.values))) * F.vectors')
end
function expm{T}(A::Hermitian{T})
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

        function ($funm)(A::Symmetric)
            F = eigfact(A)
            if isposdef(F)
                retmat = (F.vectors * Diagonal(($func).(F.values))) * F.vectors'
            else
                retmat = (F.vectors * Diagonal(($func).(complex.(F.values)))) * F.vectors'
            end
            return Symmetric(retmat)
        end

        function ($funm){T}(A::Hermitian{T})
            n = checksquare(A)
            F = eigfact(A)
            if isposdef(F)
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
