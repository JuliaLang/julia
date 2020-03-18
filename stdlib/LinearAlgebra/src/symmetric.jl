# This file is a part of Julia. License is MIT: https://julialang.org/license

# Symmetric and Hermitian matrices
struct Symmetric{T,S<:AbstractMatrix{<:T}} <: AbstractMatrix{T}
    data::S
    uplo::Char

    function Symmetric{T,S}(data, uplo) where {T,S<:AbstractMatrix{<:T}}
        require_one_based_indexing(data)
        new{T,S}(data, uplo)
    end
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

Note that `Supper` will not be equal to `Slower` unless `A` is itself symmetric (e.g. if `A == transpose(A)`).
"""
function Symmetric(A::AbstractMatrix, uplo::Symbol=:U)
    checksquare(A)
    return symmetric_type(typeof(A))(A, char_uplo(uplo))
end

"""
    symmetric(A, uplo=:U)

Construct a symmetric view of `A`. If `A` is a matrix, `uplo` controls whether the upper
(if `uplo = :U`) or lower (if `uplo = :L`) triangle of `A` is used to implicitly fill the
other one. If `A` is a `Number`, it is returned as is.

If a symmetric view of a matrix is to be constructed of which the elements are neither
matrices nor numbers, an appropriate method of `symmetric` has to be implemented. In that
case, `symmetric_type` has to be implemented, too.
"""
symmetric(A::AbstractMatrix, uplo::Symbol) = Symmetric(A, uplo)
symmetric(A::Number, ::Symbol) = A

"""
    symmetric_type(T::Type)

The type of the object returned by `symmetric(::T, ::Symbol)`. For matrices, this is an
appropriately typed `Symmetric`, for `Number`s, it is the original type. If `symmetric` is
implemented for a custom type, so should be `symmetric_type`, and vice versa.
"""
function symmetric_type(::Type{T}) where {S, T<:AbstractMatrix{S}}
    return Symmetric{Union{S, promote_op(transpose, S), symmetric_type(S)}, T}
end
function symmetric_type(::Type{T}) where {S<:Number, T<:AbstractMatrix{S}}
    return Symmetric{S, T}
end
function symmetric_type(::Type{T}) where {S<:AbstractMatrix, T<:AbstractMatrix{S}}
    return Symmetric{AbstractMatrix, T}
end
symmetric_type(::Type{T}) where {T<:Number} = T

struct Hermitian{T,S<:AbstractMatrix{<:T}} <: AbstractMatrix{T}
    data::S
    uplo::Char

    function Hermitian{T,S}(data, uplo) where {T,S<:AbstractMatrix{<:T}}
        require_one_based_indexing(data)
        new{T,S}(data, uplo)
    end
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

Note that `Hupper` will not be equal to `Hlower` unless `A` is itself Hermitian (e.g. if `A == adjoint(A)`).

All non-real parts of the diagonal will be ignored.

```julia
Hermitian(fill(complex(1,1), 1, 1)) == fill(1, 1, 1)
```
"""
function Hermitian(A::AbstractMatrix, uplo::Symbol=:U)
    n = checksquare(A)
    return hermitian_type(typeof(A))(A, char_uplo(uplo))
end

"""
    hermitian(A, uplo=:U)

Construct a hermitian view of `A`. If `A` is a matrix, `uplo` controls whether the upper
(if `uplo = :U`) or lower (if `uplo = :L`) triangle of `A` is used to implicitly fill the
other one. If `A` is a `Number`, its real part is returned converted back to the input
type.

If a hermitian view of a matrix is to be constructed of which the elements are neither
matrices nor numbers, an appropriate method of `hermitian` has to be implemented. In that
case, `hermitian_type` has to be implemented, too.
"""
hermitian(A::AbstractMatrix, uplo::Symbol) = Hermitian(A, uplo)
hermitian(A::Number, ::Symbol) = convert(typeof(A), real(A))

"""
    hermitian_type(T::Type)

The type of the object returned by `hermitian(::T, ::Symbol)`. For matrices, this is an
appropriately typed `Hermitian`, for `Number`s, it is the original type. If `hermitian` is
implemented for a custom type, so should be `hermitian_type`, and vice versa.
"""
function hermitian_type(::Type{T}) where {S, T<:AbstractMatrix{S}}
    return Hermitian{Union{S, promote_op(adjoint, S), hermitian_type(S)}, T}
end
function hermitian_type(::Type{T}) where {S<:Number, T<:AbstractMatrix{S}}
    return Hermitian{S, T}
end
function hermitian_type(::Type{T}) where {S<:AbstractMatrix, T<:AbstractMatrix{S}}
    return Hermitian{AbstractMatrix, T}
end
hermitian_type(::Type{T}) where {T<:Number} = T

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
        $S(A::$H) = $S(A, sym_uplo(A.uplo))
        function $S(A::$H, uplo::Symbol)
            if A.uplo == char_uplo(uplo)
                if $H === Hermitian && !(eltype(A) <: Real) &&
                    any(!isreal, A.data[i] for i in diagind(A.data))

                    throw(ArgumentError("Cannot construct $($S)($($H))); diagonal contains complex values"))
                end
                return $S(A.data, sym_uplo(A.uplo))
            else
                throw(ArgumentError("Cannot construct $($S); uplo doesn't match"))
            end
        end
    end
end

convert(T::Type{<:Symmetric}, m::Union{Symmetric,Hermitian}) = m isa T ? m : T(m)
convert(T::Type{<:Hermitian}, m::Union{Symmetric,Hermitian}) = m isa T ? m : T(m)

const HermOrSym{T,        S} = Union{Hermitian{T,S}, Symmetric{T,S}}
const RealHermSym{T<:Real,S} = Union{Hermitian{T,S}, Symmetric{T,S}}
const RealHermSymComplexHerm{T<:Real,S} = Union{Hermitian{T,S}, Symmetric{T,S}, Hermitian{Complex{T},S}}
const RealHermSymComplexSym{T<:Real,S} = Union{Hermitian{T,S}, Symmetric{T,S}, Symmetric{Complex{T},S}}

size(A::HermOrSym, d) = size(A.data, d)
size(A::HermOrSym) = size(A.data)
@inline function getindex(A::Symmetric, i::Integer, j::Integer)
    @boundscheck checkbounds(A, i, j)
    @inbounds if i == j
        return symmetric(A.data[i, j], sym_uplo(A.uplo))::symmetric_type(eltype(A.data))
    elseif (A.uplo == 'U') == (i < j)
        return A.data[i, j]
    else
        return transpose(A.data[j, i])
    end
end
@inline function getindex(A::Hermitian, i::Integer, j::Integer)
    @boundscheck checkbounds(A, i, j)
    @inbounds if i == j
        return hermitian(A.data[i, j], sym_uplo(A.uplo))::hermitian_type(eltype(A.data))
    elseif (A.uplo == 'U') == (i < j)
        return A.data[i, j]
    else
        return adjoint(A.data[j, i])
    end
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

# For A<:Union{Symmetric,Hermitian}, similar(A[, neweltype]) should yield a matrix with the same
# symmetry type, uplo flag, and underlying storage type as A. The following methods cover these cases.
similar(A::Symmetric, ::Type{T}) where {T} = Symmetric(similar(parent(A), T), ifelse(A.uplo == 'U', :U, :L))
# If the Hermitian constructor's check ascertaining that the wrapped matrix's
# diagonal is strictly real is removed, the following method can be simplified.
function similar(A::Hermitian, ::Type{T}) where T
    B = similar(parent(A), T)
    for i in 1:size(B, 1) B[i, i] = 0 end
    return Hermitian(B, ifelse(A.uplo == 'U', :U, :L))
end
# On the other hand, similar(A, [neweltype,] shape...) should yield a matrix of the underlying
# storage type of A (not wrapped in a symmetry type). The following method covers these cases.
similar(A::Union{Symmetric,Hermitian}, ::Type{T}, dims::Dims{N}) where {T,N} = similar(parent(A), T, dims)

# Conversion
function Matrix(A::Symmetric)
    B = copytri!(convert(Matrix, copy(A.data)), A.uplo)
    for i = 1:size(A, 1)
        B[i,i] = symmetric(A[i,i], sym_uplo(A.uplo))::symmetric_type(eltype(A.data))
    end
    return B
end
function Matrix(A::Hermitian)
    B = copytri!(convert(Matrix, copy(A.data)), A.uplo, true)
    for i = 1:size(A, 1)
        B[i,i] = hermitian(A[i,i], sym_uplo(A.uplo))::hermitian_type(eltype(A.data))
    end
    return B
end
Array(A::Union{Symmetric,Hermitian}) = convert(Matrix, A)

parent(A::HermOrSym) = A.data
Symmetric{T,S}(A::Symmetric{T,S}) where {T,S<:AbstractMatrix} = A
Symmetric{T,S}(A::Symmetric) where {T,S<:AbstractMatrix} = Symmetric{T,S}(convert(S,A.data),A.uplo)
AbstractMatrix{T}(A::Symmetric) where {T} = Symmetric(convert(AbstractMatrix{T}, A.data), sym_uplo(A.uplo))
Hermitian{T,S}(A::Hermitian{T,S}) where {T,S<:AbstractMatrix} = A
Hermitian{T,S}(A::Hermitian) where {T,S<:AbstractMatrix} = Hermitian{T,S}(convert(S,A.data),A.uplo)
AbstractMatrix{T}(A::Hermitian) where {T} = Hermitian(convert(AbstractMatrix{T}, A.data), sym_uplo(A.uplo))

copy(A::Symmetric{T,S}) where {T,S} = (B = copy(A.data); Symmetric{T,typeof(B)}(B,A.uplo))
copy(A::Hermitian{T,S}) where {T,S} = (B = copy(A.data); Hermitian{T,typeof(B)}(B,A.uplo))

function copyto!(dest::Symmetric, src::Symmetric)
    if src.uplo == dest.uplo
        copyto!(dest.data, src.data)
    else
        transpose!(dest.data, src.data)
    end
    return dest
end

function copyto!(dest::Hermitian, src::Hermitian)
    if src.uplo == dest.uplo
        copyto!(dest.data, src.data)
    else
        adjoint!(dest.data, src.data)
    end
    return dest
end

# fill[stored]!
fill!(A::HermOrSym, x) = fillstored!(A, x)
function fillstored!(A::HermOrSym{T}, x) where T
    xT = convert(T, x)
    if isa(A, Hermitian)
        isreal(xT) || throw(ArgumentError("cannot fill Hermitian matrix with a nonreal value"))
    end
    if A.uplo == 'U'
        fillband!(A.data, xT, 0, size(A,2)-1)
    else # A.uplo == 'L'
        fillband!(A.data, xT, 1-size(A,1), 0)
    end
    return A
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

adjoint(A::Hermitian) = A
transpose(A::Symmetric) = A
adjoint(A::Symmetric{<:Real}) = A
transpose(A::Hermitian{<:Real}) = A
adjoint(A::Symmetric) = Adjoint(A)
transpose(A::Hermitian) = Transpose(A)

real(A::Symmetric{<:Real}) = A
real(A::Hermitian{<:Real}) = A
real(A::Symmetric) = Symmetric(real(A.data), sym_uplo(A.uplo))
real(A::Hermitian) = Hermitian(real(A.data), sym_uplo(A.uplo))
imag(A::Symmetric) = Symmetric(imag(A.data), sym_uplo(A.uplo))

Base.copy(A::Adjoint{<:Any,<:Hermitian}) = copy(A.parent)
Base.copy(A::Transpose{<:Any,<:Symmetric}) = copy(A.parent)
Base.copy(A::Adjoint{<:Any,<:Symmetric}) =
    Symmetric(copy(adjoint(A.parent.data)), ifelse(A.parent.uplo == 'U', :L, :U))
Base.copy(A::Transpose{<:Any,<:Hermitian}) =
    Hermitian(copy(transpose(A.parent.data)), ifelse(A.parent.uplo == 'U', :L, :U))

tr(A::Hermitian) = real(tr(A.data))

Base.conj(A::HermOrSym) = typeof(A)(conj(A.data), A.uplo)
Base.conj!(A::HermOrSym) = typeof(A)(conj!(A.data), A.uplo)

# tril/triu
function tril(A::Hermitian, k::Integer=0)
    if A.uplo == 'U' && k <= 0
        return tril!(copy(A.data'),k)
    elseif A.uplo == 'U' && k > 0
        return tril!(copy(A.data'),-1) + tril!(triu(A.data),k)
    elseif A.uplo == 'L' && k <= 0
        return tril(A.data,k)
    else
        return tril(A.data,-1) + tril!(triu!(copy(A.data')),k)
    end
end

function tril(A::Symmetric, k::Integer=0)
    if A.uplo == 'U' && k <= 0
        return tril!(copy(transpose(A.data)),k)
    elseif A.uplo == 'U' && k > 0
        return tril!(copy(transpose(A.data)),-1) + tril!(triu(A.data),k)
    elseif A.uplo == 'L' && k <= 0
        return tril(A.data,k)
    else
        return tril(A.data,-1) + tril!(triu!(copy(transpose(A.data))),k)
    end
end

function triu(A::Hermitian, k::Integer=0)
    if A.uplo == 'U' && k >= 0
        return triu(A.data,k)
    elseif A.uplo == 'U' && k < 0
        return triu(A.data,1) + triu!(tril!(copy(A.data')),k)
    elseif A.uplo == 'L' && k >= 0
        return triu!(copy(A.data'),k)
    else
        return triu!(copy(A.data'),1) + triu!(tril(A.data),k)
    end
end

function triu(A::Symmetric, k::Integer=0)
    if A.uplo == 'U' && k >= 0
        return triu(A.data,k)
    elseif A.uplo == 'U' && k < 0
        return triu(A.data,1) + triu!(tril!(copy(transpose(A.data))),k)
    elseif A.uplo == 'L' && k >= 0
        return triu!(copy(transpose(A.data)),k)
    else
        return triu!(copy(transpose(A.data)),1) + triu!(tril(A.data),k)
    end
end

for (T, trans, real) in [(:Symmetric, :transpose, :identity), (:Hermitian, :adjoint, :real)]
    @eval begin
        function dot(A::$T, B::$T)
            n = size(A, 2)
            if n != size(B, 2)
                throw(DimensionMismatch("A has dimensions $(size(A)) but B has dimensions $(size(B))"))
            end

            dotprod = zero(dot(first(A), first(B)))
            @inbounds if A.uplo == 'U' && B.uplo == 'U'
                for j in 1:n
                    for i in 1:(j - 1)
                        dotprod += 2 * $real(dot(A.data[i, j], B.data[i, j]))
                    end
                    dotprod += dot(A[j, j], B[j, j])
                end
            elseif A.uplo == 'L' && B.uplo == 'L'
                for j in 1:n
                    dotprod += dot(A[j, j], B[j, j])
                    for i in (j + 1):n
                        dotprod += 2 * $real(dot(A.data[i, j], B.data[i, j]))
                    end
                end
            elseif A.uplo == 'U' && B.uplo == 'L'
                for j in 1:n
                    for i in 1:(j - 1)
                        dotprod += 2 * $real(dot(A.data[i, j], $trans(B.data[j, i])))
                    end
                    dotprod += dot(A[j, j], B[j, j])
                end
            else
                for j in 1:n
                    dotprod += dot(A[j, j], B[j, j])
                    for i in (j + 1):n
                        dotprod += 2 * $real(dot(A.data[i, j], $trans(B.data[j, i])))
                    end
                end
            end
            return dotprod
        end
    end
end

(-)(A::Symmetric) = Symmetric(-A.data, sym_uplo(A.uplo))
(-)(A::Hermitian) = Hermitian(-A.data, sym_uplo(A.uplo))

## Addition/subtraction
for f ∈ (:+, :-), (Wrapper, conjugation) ∈ ((:Hermitian, :adjoint), (:Symmetric, :transpose))
    @eval begin
        function $f(A::$Wrapper, B::$Wrapper)
            if A.uplo == B.uplo
                return $Wrapper($f(parent(A), parent(B)), sym_uplo(A.uplo))
            elseif A.uplo == 'U'
                return $Wrapper($f(parent(A), $conjugation(parent(B))), :U)
            else
                return $Wrapper($f($conjugation(parent(A)), parent(B)), :U)
            end
        end
    end
end

for f in (:+, :-)
    @eval begin
        $f(A::Hermitian, B::Symmetric{<:Real}) = $f(A, Hermitian(parent(B), sym_uplo(B.uplo)))
        $f(A::Symmetric{<:Real}, B::Hermitian) = $f(Hermitian(parent(A), sym_uplo(A.uplo)), B)
    end
end

## Matvec
@inline function mul!(y::StridedVector{T}, A::Symmetric{T,<:StridedMatrix}, x::StridedVector{T},
             α::Number, β::Number) where {T<:BlasFloat}
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T}
        return BLAS.symv!(A.uplo, alpha, A.data, x, beta, y)
    else
        return generic_matvecmul!(y, 'N', A, x, MulAddMul(α, β))
    end
end
@inline function mul!(y::StridedVector{T}, A::Hermitian{T,<:StridedMatrix}, x::StridedVector{T},
             α::Number, β::Number) where {T<:BlasReal}
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T}
        return BLAS.symv!(A.uplo, alpha, A.data, x, beta, y)
    else
        return generic_matvecmul!(y, 'N', A, x, MulAddMul(α, β))
    end
end
@inline function mul!(y::StridedVector{T}, A::Hermitian{T,<:StridedMatrix}, x::StridedVector{T},
             α::Number, β::Number) where {T<:BlasComplex}
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T}
        return BLAS.hemv!(A.uplo, alpha, A.data, x, beta, y)
    else
        return generic_matvecmul!(y, 'N', A, x, MulAddMul(α, β))
    end
end
## Matmat
@inline function mul!(C::StridedMatrix{T}, A::Symmetric{T,<:StridedMatrix}, B::StridedMatrix{T},
             α::Number, β::Number) where {T<:BlasFloat}
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T}
        return BLAS.symm!('L', A.uplo, alpha, A.data, B, beta, C)
    else
        return generic_matmatmul!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))
    end
end
@inline function mul!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Symmetric{T,<:StridedMatrix},
             α::Number, β::Number) where {T<:BlasFloat}
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T}
        return BLAS.symm!('R', B.uplo, alpha, B.data, A, beta, C)
    else
        return generic_matmatmul!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))
    end
end
@inline function mul!(C::StridedMatrix{T}, A::Hermitian{T,<:StridedMatrix}, B::StridedMatrix{T},
             α::Number, β::Number) where {T<:BlasReal}
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T}
        return BLAS.symm!('L', A.uplo, alpha, A.data, B, beta, C)
    else
        return generic_matmatmul!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))
    end
end
@inline function mul!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Hermitian{T,<:StridedMatrix},
             α::Number, β::Number) where {T<:BlasReal}
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T}
        return BLAS.symm!('R', B.uplo, alpha, B.data, A, beta, C)
    else
        return generic_matmatmul!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))
    end
end
@inline function mul!(C::StridedMatrix{T}, A::Hermitian{T,<:StridedMatrix}, B::StridedMatrix{T},
             α::Number, β::Number) where {T<:BlasComplex}
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T}
        return BLAS.hemm!('L', A.uplo, alpha, A.data, B, beta, C)
    else
        return generic_matmatmul!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))
    end
end
@inline function mul!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Hermitian{T,<:StridedMatrix},
             α::Number, β::Number) where {T<:BlasComplex}
    alpha, beta = promote(α, β, zero(T))
    if alpha isa Union{Bool,T} && beta isa Union{Bool,T}
        return BLAS.hemm!('R', B.uplo, alpha, B.data, A, beta, C)
    else
        return generic_matmatmul!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))
    end
end

*(A::HermOrSym, B::HermOrSym) = A * copyto!(similar(parent(B)), B)

function dot(x::AbstractVector, A::RealHermSymComplexHerm, y::AbstractVector)
    require_one_based_indexing(x, y)
    (length(x) == length(y) == size(A, 1)) || throw(DimensionMismatch())
    data = A.data
    r = zero(eltype(x)) * zero(eltype(A)) * zero(eltype(y))
    if A.uplo == 'U'
        @inbounds for j = 1:length(y)
            r += dot(x[j], real(data[j,j]), y[j])
            @simd for i = 1:j-1
                Aij = data[i,j]
                r += dot(x[i], Aij, y[j]) + dot(x[j], adjoint(Aij), y[i])
            end
        end
    else # A.uplo == 'L'
        @inbounds for j = 1:length(y)
            r += dot(x[j], real(data[j,j]), y[j])
            @simd for i = j+1:length(y)
                Aij = data[i,j]
                r += dot(x[i], Aij, y[j]) + dot(x[j], adjoint(Aij), y[i])
            end
        end
    end
    return r
end

# Fallbacks to avoid generic_matvecmul!/generic_matmatmul!
## Symmetric{<:Number} and Hermitian{<:Real} are invariant to transpose; peel off the t
*(transA::Transpose{<:Any,<:RealHermSymComplexSym}, B::AbstractVector) = transA.parent * B
*(transA::Transpose{<:Any,<:RealHermSymComplexSym}, B::AbstractMatrix) = transA.parent * B
*(A::AbstractMatrix, transB::Transpose{<:Any,<:RealHermSymComplexSym}) = A * transB.parent
## Hermitian{<:Number} and Symmetric{<:Real} are invariant to adjoint; peel off the c
*(adjA::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::AbstractVector) = adjA.parent * B
*(adjA::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::AbstractMatrix) = adjA.parent * B
*(A::AbstractMatrix, adjB::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * adjB.parent

# ambiguities with transposed AbstractMatrix methods in linalg/matmul.jl
*(transA::Transpose{<:Any,<:RealHermSym}, transB::Transpose{<:Any,<:RealHermSym}) = transA * transB.parent
*(transA::Transpose{<:Any,<:RealHermSym}, transB::Transpose{<:Any,<:RealHermSymComplexSym}) = transA * transB.parent
*(transA::Transpose{<:Any,<:RealHermSymComplexSym}, transB::Transpose{<:Any,<:RealHermSymComplexSym}) = transA.parent * transB.parent
*(transA::Transpose{<:Any,<:RealHermSymComplexSym}, transB::Transpose{<:Any,<:RealHermSym}) = transA.parent * transB
*(transA::Transpose{<:Any,<:RealHermSymComplexSym}, transB::Transpose{<:Any,<:RealHermSymComplexHerm}) = transA.parent * transB
*(transA::Transpose{<:Any,<:RealHermSymComplexHerm}, transB::Transpose{<:Any,<:RealHermSymComplexSym}) = transA * transB.parent
*(adjA::Adjoint{<:Any,<:RealHermSym}, adjB::Adjoint{<:Any,<:RealHermSym}) = adjA * adjB.parent
*(adjA::Adjoint{<:Any,<:RealHermSymComplexHerm}, adjB::Adjoint{<:Any,<:RealHermSymComplexHerm}) = adjA.parent * adjB.parent
*(adjA::Adjoint{<:Any,<:RealHermSym}, adjB::Adjoint{<:Any,<:RealHermSymComplexHerm}) = adjA * adjB.parent
*(adjA::Adjoint{<:Any,<:RealHermSymComplexSym}, adjB::Adjoint{<:Any,<:RealHermSymComplexHerm}) = adjA * adjB.parent
*(adjA::Adjoint{<:Any,<:RealHermSymComplexHerm}, adjB::Adjoint{<:Any,<:RealHermSym}) = adjA.parent * adjB
*(adjA::Adjoint{<:Any,<:RealHermSymComplexHerm}, adjB::Adjoint{<:Any,<:RealHermSymComplexSym}) = adjA.parent * adjB

# ambiguities with AbstractTriangular
*(transA::Transpose{<:Any,<:RealHermSymComplexSym}, B::AbstractTriangular) = transA.parent * B
*(A::AbstractTriangular, transB::Transpose{<:Any,<:RealHermSymComplexSym}) = A * transB.parent
*(adjA::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::AbstractTriangular) = adjA.parent * B
*(A::AbstractTriangular, adjB::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * adjB.parent

# Scaling with Number
*(A::Symmetric, x::Number) = Symmetric(A.data*x, sym_uplo(A.uplo))
*(x::Number, A::Symmetric) = Symmetric(x*A.data, sym_uplo(A.uplo))
*(A::Hermitian, x::Real) = Hermitian(A.data*x, sym_uplo(A.uplo))
*(x::Real, A::Hermitian) = Hermitian(x*A.data, sym_uplo(A.uplo))
/(A::Symmetric, x::Number) = Symmetric(A.data/x, sym_uplo(A.uplo))
/(A::Hermitian, x::Real) = Hermitian(A.data/x, sym_uplo(A.uplo))

factorize(A::HermOrSym) = _factorize(A)
function _factorize(A::HermOrSym{T}; check::Bool=true) where T
    TT = typeof(sqrt(oneunit(T)))
    if TT <: BlasFloat
        return bunchkaufman(A; check=check)
    else # fallback
        return lu(A; check=check)
    end
end

det(A::RealHermSymComplexHerm) = real(det(_factorize(A; check=false)))
det(A::Symmetric{<:Real}) = det(_factorize(A; check=false))
det(A::Symmetric) = det(_factorize(A; check=false))

\(A::HermOrSym{<:Any,<:StridedMatrix}, B::AbstractVector) = \(factorize(A), B)
# Bunch-Kaufman solves can not utilize BLAS-3 for multiple right hand sides
# so using LU is faster for AbstractMatrix right hand side
\(A::HermOrSym{<:Any,<:StridedMatrix}, B::AbstractMatrix) = \(lu(A), B)

function _inv(A::HermOrSym)
    n = checksquare(A)
    B = inv!(lu(A))
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
inv(A::Hermitian{<:Any,<:StridedMatrix}) = Hermitian(_inv(A), sym_uplo(A.uplo))
inv(A::Symmetric{<:Any,<:StridedMatrix}) = Symmetric(_inv(A), sym_uplo(A.uplo))

eigen!(A::RealHermSymComplexHerm{<:BlasReal,<:StridedMatrix}; sortby::Union{Function,Nothing}=nothing) = Eigen(sorteig!(LAPACK.syevr!('V', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)..., sortby)...)

function eigen(A::RealHermSymComplexHerm; sortby::Union{Function,Nothing}=nothing)
    T = eltype(A)
    S = eigtype(T)
    eigen!(copy_oftype(A, S), sortby=sortby)
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
    eigen!(copy_oftype(A, S), irange)
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
    eigen!(copy_oftype(A, S), vl, vh)
end

eigvals!(A::RealHermSymComplexHerm{<:BlasReal,<:StridedMatrix}) =
    LAPACK.syevr!('N', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)[1]

function eigvals(A::RealHermSymComplexHerm)
    T = eltype(A)
    S = eigtype(T)
    eigvals!(copy_oftype(A, S))
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
3×3 SymTridiagonal{Float64,Array{Float64,1}}:
 1.0  2.0   ⋅
 2.0  2.0  3.0
  ⋅   3.0  1.0

julia> eigvals(A, 2:2)
1-element Array{Float64,1}:
 0.9999999999999996

julia> eigvals(A)
3-element Array{Float64,1}:
 -2.1400549446402604
  1.0000000000000002
  5.140054944640259
```
"""
function eigvals(A::RealHermSymComplexHerm, irange::UnitRange)
    T = eltype(A)
    S = eigtype(T)
    eigvals!(copy_oftype(A, S), irange)
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
3×3 SymTridiagonal{Float64,Array{Float64,1}}:
 1.0  2.0   ⋅
 2.0  2.0  3.0
  ⋅   3.0  1.0

julia> eigvals(A, -1, 2)
1-element Array{Float64,1}:
 1.0000000000000009

julia> eigvals(A)
3-element Array{Float64,1}:
 -2.1400549446402604
  1.0000000000000002
  5.140054944640259
```
"""
function eigvals(A::RealHermSymComplexHerm, vl::Real, vh::Real)
    T = eltype(A)
    S = eigtype(T)
    eigvals!(copy_oftype(A, S), vl, vh)
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

eigvals!(A::HermOrSym{T,S}, B::HermOrSym{T,S}) where {T<:BlasReal,S<:StridedMatrix} =
    LAPACK.sygvd!(1, 'N', A.uplo, A.data, B.uplo == A.uplo ? B.data : copy(B.data'))[1]
eigvals!(A::Hermitian{T,S}, B::Hermitian{T,S}) where {T<:BlasComplex,S<:StridedMatrix} =
    LAPACK.sygvd!(1, 'N', A.uplo, A.data, B.uplo == A.uplo ? B.data : copy(B.data'))[1]

eigvecs(A::HermOrSym) = eigvecs(eigen(A))

function svd(A::RealHermSymComplexHerm, full::Bool=false)
    vals, vecs = eigen(A)
    I = sortperm(vals; by=abs, rev=true)
    permute!(vals, I)
    Base.permutecols!!(vecs, I)         # left-singular vectors
    V = copy(vecs)                      # right-singular vectors
    # shifting -1 from singular values to right-singular vectors
    @inbounds for i = 1:length(vals)
        if vals[i] < 0
            vals[i] = -vals[i]
            for j = 1:size(V,1); V[j,i] = -V[j,i]; end
        end
    end
    return SVD(vecs, vals, V')
end

function svdvals!(A::RealHermSymComplexHerm)
    vals = eigvals!(A)
    for i = 1:length(vals)
        vals[i] = abs(vals[i])
    end
    return sort!(vals, rev = true)
end

# Matrix functions
^(A::Symmetric{<:Real}, p::Integer) = sympow(A, p)
^(A::Symmetric{<:Complex}, p::Integer) = sympow(A, p)
function sympow(A::Symmetric, p::Integer)
    if p < 0
        return Symmetric(Base.power_by_squaring(inv(A), -p))
    else
        return Symmetric(Base.power_by_squaring(A, p))
    end
end
function ^(A::Symmetric{<:Real}, p::Real)
    isinteger(p) && return integerpow(A, p)
    F = eigen(A)
    if all(λ -> λ ≥ 0, F.values)
        return Symmetric((F.vectors * Diagonal((F.values).^p)) * F.vectors')
    else
        return Symmetric((F.vectors * Diagonal((complex(F.values)).^p)) * F.vectors')
    end
end
function ^(A::Symmetric{<:Complex}, p::Real)
    isinteger(p) && return integerpow(A, p)
    return Symmetric(schurpow(A, p))
end
function ^(A::Hermitian, p::Integer)
    if p < 0
        retmat = Base.power_by_squaring(inv(A), -p)
    else
        retmat = Base.power_by_squaring(A, p)
    end
    for i = 1:size(A,1)
        retmat[i,i] = real(retmat[i,i])
    end
    return Hermitian(retmat)
end
function ^(A::Hermitian{T}, p::Real) where T
    isinteger(p) && return integerpow(A, p)
    F = eigen(A)
    if all(λ -> λ ≥ 0, F.values)
        retmat = (F.vectors * Diagonal((F.values).^p)) * F.vectors'
        if T <: Real
            return Hermitian(retmat)
        else
            for i = 1:size(A,1)
                retmat[i,i] = real(retmat[i,i])
            end
            return Hermitian(retmat)
        end
    else
        return (F.vectors * Diagonal((complex(F.values).^p))) * F.vectors'
    end
end

for func in (:exp, :cos, :sin, :tan, :cosh, :sinh, :tanh, :atan, :asinh, :atanh)
    @eval begin
        function ($func)(A::HermOrSym{<:Real})
            F = eigen(A)
            return Symmetric((F.vectors * Diagonal(($func).(F.values))) * F.vectors')
        end
        function ($func)(A::Hermitian{<:Complex})
            n = checksquare(A)
            F = eigen(A)
            retmat = (F.vectors * Diagonal(($func).(F.values))) * F.vectors'
            for i = 1:n
                retmat[i,i] = real(retmat[i,i])
            end
            return Hermitian(retmat)
        end
    end
end

for func in (:acos, :asin)
    @eval begin
        function ($func)(A::HermOrSym{<:Real})
            F = eigen(A)
            if all(λ -> -1 ≤ λ ≤ 1, F.values)
                retmat = (F.vectors * Diagonal(($func).(F.values))) * F.vectors'
            else
                retmat = (F.vectors * Diagonal(($func).(complex.(F.values)))) * F.vectors'
            end
            return Symmetric(retmat)
        end
        function ($func)(A::Hermitian{<:Complex})
            n = checksquare(A)
            F = eigen(A)
            if all(λ -> -1 ≤ λ ≤ 1, F.values)
                retmat = (F.vectors * Diagonal(($func).(F.values))) * F.vectors'
                for i = 1:n
                    retmat[i,i] = real(retmat[i,i])
                end
                return Hermitian(retmat)
            else
                return (F.vectors * Diagonal(($func).(complex.(F.values)))) * F.vectors'
            end
        end
    end
end

function acosh(A::HermOrSym{<:Real})
    F = eigen(A)
    if all(λ -> λ ≥ 1, F.values)
        retmat = (F.vectors * Diagonal(acosh.(F.values))) * F.vectors'
    else
        retmat = (F.vectors * Diagonal(acosh.(complex.(F.values)))) * F.vectors'
    end
    return Symmetric(retmat)
end
function acosh(A::Hermitian{<:Complex})
    n = checksquare(A)
    F = eigen(A)
    if all(λ -> λ ≥ 1, F.values)
        retmat = (F.vectors * Diagonal(acosh.(F.values))) * F.vectors'
        for i = 1:n
            retmat[i,i] = real(retmat[i,i])
        end
        return Hermitian(retmat)
    else
        return (F.vectors * Diagonal(acosh.(complex.(F.values)))) * F.vectors'
    end
end

function sincos(A::HermOrSym{<:Real})
    n = checksquare(A)
    F = eigen(A)
    S, C = Diagonal(similar(A, (n,))), Diagonal(similar(A, (n,)))
    for i in 1:n
        S.diag[i], C.diag[i] = sincos(F.values[i])
    end
    return Symmetric((F.vectors * S) * F.vectors'), Symmetric((F.vectors * C) * F.vectors')
end
function sincos(A::Hermitian{<:Complex})
    n = checksquare(A)
    F = eigen(A)
    S, C = Diagonal(similar(A, (n,))), Diagonal(similar(A, (n,)))
    for i in 1:n
        S.diag[i], C.diag[i] = sincos(F.values[i])
    end
    retmatS, retmatC = (F.vectors * S) * F.vectors', (F.vectors * C) * F.vectors'
    for i = 1:n
        retmatS[i,i] = real(retmatS[i,i])
        retmatC[i,i] = real(retmatC[i,i])
    end
    return Hermitian(retmatS), Hermitian(retmatC)
end


for func in (:log, :sqrt)
    @eval begin
        function ($func)(A::HermOrSym{<:Real})
            F = eigen(A)
            if all(λ -> λ ≥ 0, F.values)
                retmat = (F.vectors * Diagonal(($func).(F.values))) * F.vectors'
            else
                retmat = (F.vectors * Diagonal(($func).(complex.(F.values)))) * F.vectors'
            end
            return Symmetric(retmat)
        end

        function ($func)(A::Hermitian{<:Complex})
            n = checksquare(A)
            F = eigen(A)
            if all(λ -> λ ≥ 0, F.values)
                retmat = (F.vectors * Diagonal(($func).(F.values))) * F.vectors'
                for i = 1:n
                    retmat[i,i] = real(retmat[i,i])
                end
                return Hermitian(retmat)
            else
                retmat = (F.vectors * Diagonal(($func).(complex(F.values)))) * F.vectors'
                return retmat
            end
        end
    end
end

# disambiguation methods: *(Adj of RealHermSymComplexHerm, Trans of RealHermSymComplexSym) and symmetric partner
*(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Transpose{<:Any,<:RealHermSymComplexSym}) = A.parent * B.parent
*(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A.parent * B.parent
# disambiguation methods: *(Adj/Trans of AbsVec/AbsMat, Adj/Trans of RealHermSymComplex{Herm|Sym})
*(A::Adjoint{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * B.parent
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * B.parent
*(A::Adjoint{<:Any,<:AbstractVector}, B::Transpose{<:Any,<:RealHermSymComplexSym}) = A * B.parent
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:RealHermSymComplexSym}) = A * B.parent
*(A::Transpose{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * B.parent
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * B.parent
*(A::Transpose{<:Any,<:AbstractVector}, B::Transpose{<:Any,<:RealHermSymComplexSym}) = A * B.parent
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:RealHermSymComplexSym}) = A * B.parent
# disambiguation methods: *(Adj/Trans of RealHermSymComplex{Herm|Sym}, Adj/Trans of AbsVec/AbsMat)
*(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Adjoint{<:Any,<:AbstractVector}) = A.parent * B
*(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Adjoint{<:Any,<:AbstractMatrix}) = A.parent * B
*(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Transpose{<:Any,<:AbstractVector}) = A.parent * B
*(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Transpose{<:Any,<:AbstractMatrix}) = A.parent * B
*(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Adjoint{<:Any,<:AbstractVector}) = A.parent * B
*(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Adjoint{<:Any,<:AbstractMatrix}) = A.parent * B
*(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Transpose{<:Any,<:AbstractVector}) = A.parent * B
*(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Transpose{<:Any,<:AbstractMatrix}) = A.parent * B

# disambiguation methods: *(Adj/Trans of AbsTri or RealHermSymComplex{Herm|Sym}, Adj/Trans of other)
*(A::Adjoint{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * B.parent
*(A::Adjoint{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:RealHermSymComplexSym}) = A * B.parent
*(A::Transpose{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * B.parent
*(A::Transpose{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:RealHermSymComplexSym}) = A * B.parent
*(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Adjoint{<:Any,<:AbstractTriangular}) = A.parent * B
*(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Transpose{<:Any,<:AbstractTriangular}) = A.parent * B
*(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Adjoint{<:Any,<:AbstractTriangular}) = A.parent * B
*(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Transpose{<:Any,<:AbstractTriangular}) = A.parent * B
