# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base: copy, adjoint, getindex, show, transpose, one, zero, inv,
             hcat, vcat, hvcat
import Base.LinAlg: SingularException

"""
    UniformScaling{T<:Number}

Generically sized uniform scaling operator defined as a scalar times the
identity operator, `λ*I`. See also [`I`](@ref).

# Examples
```jldoctest
julia> J = UniformScaling(2.)
UniformScaling{Float64}
2.0*I

julia> A = [1. 2.; 3. 4.]
2×2 Array{Float64,2}:
 1.0  2.0
 3.0  4.0

julia> J*A
2×2 Array{Float64,2}:
 2.0  4.0
 6.0  8.0
```
"""
struct UniformScaling{T<:Number}
    λ::T
end

"""
    I

An object of type [`UniformScaling`](@ref), representing an identity matrix of any size.

# Examples
```jldoctest
julia> ones(5, 6) * I == ones(5, 6)
true

julia> [1 2im 3; 1im 2 3] * I
2×3 Array{Complex{Int64},2}:
 1+0im  0+2im  3+0im
 0+1im  2+0im  3+0im
```
"""
const I = UniformScaling(true)

eltype(::Type{UniformScaling{T}}) where {T} = T
ndims(J::UniformScaling) = 2
getindex(J::UniformScaling, i::Integer,j::Integer) = ifelse(i==j,J.λ,zero(J.λ))

function show(io::IO, J::UniformScaling)
    s = "$(J.λ)"
    if ismatch(r"\w+\s*[\+\-]\s*\w+", s)
        s = "($s)"
    end
    print(io, "$(typeof(J))\n$s*I")
end
copy(J::UniformScaling) = UniformScaling(J.λ)

transpose(J::UniformScaling) = J
adjoint(J::UniformScaling) = UniformScaling(adjoint(J.λ))

one(::Type{UniformScaling{T}}) where {T} = UniformScaling(one(T))
one(J::UniformScaling{T}) where {T} = one(UniformScaling{T})
oneunit(::Type{UniformScaling{T}}) where {T} = UniformScaling(oneunit(T))
oneunit(J::UniformScaling{T}) where {T} = oneunit(UniformScaling{T})
zero(::Type{UniformScaling{T}}) where {T} = UniformScaling(zero(T))
zero(J::UniformScaling{T}) where {T} = zero(UniformScaling{T})

istriu(::UniformScaling) = true
istril(::UniformScaling) = true
issymmetric(::UniformScaling) = true
ishermitian(J::UniformScaling) = isreal(J.λ)

(+)(J::UniformScaling, x::Number) = J.λ + x
(+)(x::Number, J::UniformScaling) = x + J.λ
(-)(J::UniformScaling, x::Number) = J.λ - x
(-)(x::Number, J::UniformScaling) = x - J.λ

(+)(J1::UniformScaling, J2::UniformScaling) = UniformScaling(J1.λ+J2.λ)
(+)(B::BitArray{2}, J::UniformScaling)      = Array(B) + J
(+)(J::UniformScaling, B::BitArray{2})      = J + Array(B)
(+)(J::UniformScaling, A::AbstractMatrix)   = A + J

(-)(J::UniformScaling)                      = UniformScaling(-J.λ)
(-)(J1::UniformScaling, J2::UniformScaling) = UniformScaling(J1.λ-J2.λ)
(-)(B::BitArray{2}, J::UniformScaling)      = Array(B) - J
(-)(J::UniformScaling, B::BitArray{2})      = J - Array(B)

for (t1, t2) in ((:UnitUpperTriangular, :UpperTriangular),
                 (:UnitLowerTriangular, :LowerTriangular))
    for op in (:+,:-)
        @eval begin
            ($op)(UL::$t2, J::UniformScaling) = ($t2)(($op)(UL.data, J))

            function ($op)(UL::$t1, J::UniformScaling)
                ULnew = copy_oftype(UL.data, Base.Broadcast._broadcast_eltype($op, UL, J))
                for i = 1:size(ULnew, 1)
                    ULnew[i,i] = ($op)(1, J.λ)
                end
                return ($t2)(ULnew)
            end
        end
    end
end

function (-)(J::UniformScaling, UL::Union{UpperTriangular,UnitUpperTriangular})
    ULnew = similar(parent(UL), Base.Broadcast._broadcast_eltype(-, J, UL))
    n = size(ULnew, 1)
    ULold = UL.data
    for j = 1:n
        for i = 1:j - 1
            ULnew[i,j] = -ULold[i,j]
        end
        if isa(UL, UnitUpperTriangular)
            ULnew[j,j] = J.λ - 1
        else
            ULnew[j,j] = J.λ - ULold[j,j]
        end
    end
    return UpperTriangular(ULnew)
end
function (-)(J::UniformScaling, UL::Union{LowerTriangular,UnitLowerTriangular})
    ULnew = similar(parent(UL), Base.Broadcast._broadcast_eltype(-, J, UL))
    n = size(ULnew, 1)
    ULold = UL.data
    for j = 1:n
        if isa(UL, UnitLowerTriangular)
            ULnew[j,j] = J.λ - 1
        else
            ULnew[j,j] = J.λ - ULold[j,j]
        end
        for i = j + 1:n
            ULnew[i,j] = -ULold[i,j]
        end
    end
    return LowerTriangular(ULnew)
end

function (+)(A::AbstractMatrix, J::UniformScaling)
    n = checksquare(A)
    B = similar(A, Base.Broadcast._broadcast_eltype(+, A, J))
    copy!(B,A)
    @inbounds for i = 1:n
        B[i,i] += J.λ
    end
    B
end

function (-)(A::AbstractMatrix, J::UniformScaling)
    n = checksquare(A)
    B = similar(A, Base.Broadcast._broadcast_eltype(-, A, J))
    copy!(B, A)
    @inbounds for i = 1:n
        B[i,i] -= J.λ
    end
    B
end
function (-)(J::UniformScaling, A::AbstractMatrix)
    n = checksquare(A)
    B = convert(AbstractMatrix{Base.Broadcast._broadcast_eltype(-, J, A)}, -A)
    @inbounds for j = 1:n
        B[j,j] += J.λ
    end
    B
end

inv(J::UniformScaling) = UniformScaling(inv(J.λ))
norm(J::UniformScaling, p::Real=2) = abs(J.λ)

function det(J::UniformScaling{T}) where T
    if isone(J.λ)
        one(T)
    elseif iszero(J.λ)
        zero(T)
    else
        throw(ArgumentError("Determinant of UniformScaling is only well-defined when λ = 0 or 1."))
    end
end

*(J1::UniformScaling, J2::UniformScaling) = UniformScaling(J1.λ*J2.λ)
*(B::BitArray{2}, J::UniformScaling) = *(Array(B), J::UniformScaling)
*(J::UniformScaling, B::BitArray{2}) = *(J::UniformScaling, Array(B))
*(A::AbstractMatrix, J::UniformScaling) = A*J.λ
*(J::UniformScaling, A::AbstractVecOrMat) = J.λ*A
*(x::Number, J::UniformScaling) = UniformScaling(x*J.λ)
*(J::UniformScaling, x::Number) = UniformScaling(J.λ*x)

/(J1::UniformScaling, J2::UniformScaling) = J2.λ == 0 ? throw(SingularException(1)) : UniformScaling(J1.λ/J2.λ)
/(J::UniformScaling, A::AbstractMatrix) = scale!(J.λ, inv(A))
/(A::AbstractMatrix, J::UniformScaling) = J.λ == 0 ? throw(SingularException(1)) : A/J.λ

/(J::UniformScaling, x::Number) = UniformScaling(J.λ/x)

\(J1::UniformScaling, J2::UniformScaling) = J1.λ == 0 ? throw(SingularException(1)) : UniformScaling(J1.λ\J2.λ)
\(A::Union{Bidiagonal{T},AbstractTriangular{T}}, J::UniformScaling) where {T<:Number} = scale!(inv(A), J.λ)
\(J::UniformScaling, A::AbstractVecOrMat) = J.λ == 0 ? throw(SingularException(1)) : J.λ\A
\(A::AbstractMatrix, J::UniformScaling) = scale!(inv(A), J.λ)

\(x::Number, J::UniformScaling) = UniformScaling(x\J.λ)

broadcast(::typeof(*), x::Number,J::UniformScaling) = UniformScaling(x*J.λ)
broadcast(::typeof(*), J::UniformScaling,x::Number) = UniformScaling(J.λ*x)

broadcast(::typeof(/), J::UniformScaling,x::Number) = UniformScaling(J.λ/x)

==(J1::UniformScaling,J2::UniformScaling) = (J1.λ == J2.λ)

## equality comparison with UniformScaling
==(J::UniformScaling, A::AbstractMatrix) = A == J
function ==(A::AbstractMatrix, J::UniformScaling)
    size(A, 1) == size(A, 2) || return false
    iszero(J.λ) && return iszero(A)
    isone(J.λ) && return isone(A)
    return A == J.λ*one(A)
end
function ==(A::StridedMatrix, J::UniformScaling)
    size(A, 1) == size(A, 2) || return false
    iszero(J.λ) && return iszero(A)
    isone(J.λ) && return isone(A)
    for j in indices(A, 2), i in indices(A, 1)
        ifelse(i == j, A[i, j] == J.λ, iszero(A[i, j])) || return false
    end
    return true
end

function isapprox(J1::UniformScaling{T}, J2::UniformScaling{S};
            atol::Real=0, rtol::Real=Base.rtoldefault(T,S,atol), nans::Bool=false) where {T<:Number,S<:Number}
    isapprox(J1.λ, J2.λ, rtol=rtol, atol=atol, nans=nans)
end
function isapprox(J::UniformScaling, A::AbstractMatrix;
                  atol::Real = 0,
                  rtol::Real = Base.rtoldefault(promote_leaf_eltypes(A), eltype(J), atol),
                  nans::Bool = false, norm::Function = vecnorm)
    n = checksquare(A)
    normJ = norm === Base.norm ? abs(J.λ) :
            norm === vecnorm   ? abs(J.λ) * sqrt(n) :
                                 norm(Diagonal(fill(J.λ, n)))
    return norm(A - J) <= max(atol, rtol * max(norm(A), normJ))
end
isapprox(A::AbstractMatrix, J::UniformScaling; kwargs...) = isapprox(J, A; kwargs...)

function copy!(A::AbstractMatrix, J::UniformScaling)
    size(A,1)==size(A,2) || throw(DimensionMismatch("a UniformScaling can only be copied to a square matrix"))
    fill!(A, 0)
    λ = J.λ
    for i = 1:size(A,1)
        @inbounds A[i,i] = λ
    end
    return A
end

function cond(J::UniformScaling{T}) where T
    onereal = inv(one(real(J.λ)))
    return J.λ ≠ zero(T) ? onereal : oftype(onereal, Inf)
end

# promote_to_arrays(n,k, T, A...) promotes any UniformScaling matrices
# in A to matrices of type T and sizes given by n[k:end].  n is an array
# so that the same promotion code can be used for hvcat.  We pass the type T
# so that we can re-use this code for sparse-matrix hcat etcetera.
promote_to_arrays_(n::Int, ::Type{Matrix}, J::UniformScaling{T}) where {T} = copy!(Matrix{T}(n,n), J)
promote_to_arrays_(n::Int, ::Type, A::AbstractVecOrMat) = A
promote_to_arrays(n,k, ::Type) = ()
promote_to_arrays(n,k, ::Type{T}, A) where {T} = (promote_to_arrays_(n[k], T, A),)
promote_to_arrays(n,k, ::Type{T}, A, B) where {T} =
    (promote_to_arrays_(n[k], T, A), promote_to_arrays_(n[k+1], T, B))
promote_to_arrays(n,k, ::Type{T}, A, B, C) where {T} =
    (promote_to_arrays_(n[k], T, A), promote_to_arrays_(n[k+1], T, B), promote_to_arrays_(n[k+2], T, C))
promote_to_arrays(n,k, ::Type{T}, A, B, Cs...) where {T} =
    (promote_to_arrays_(n[k], T, A), promote_to_arrays_(n[k+1], T, B), promote_to_arrays(n,k+2, T, Cs...)...)
promote_to_array_type(A::Tuple{Vararg{Union{AbstractVecOrMat,UniformScaling}}}) = Matrix

for (f,dim,name) in ((:hcat,1,"rows"), (:vcat,2,"cols"))
    @eval begin
        function $f(A::Union{AbstractVecOrMat,UniformScaling}...)
            n = 0
            for a in A
                if !isa(a, UniformScaling)
                    na = size(a,$dim)
                    n > 0 && n != na &&
                        throw(DimensionMismatch(string("number of ", $name,
                            " of each array must match (got ", n, " and ", na, ")")))
                    n = na
                end
            end
            n == 0 && throw(ArgumentError($("$f of only UniformScaling objects cannot determine the matrix size")))
            return $f(promote_to_arrays(fill(n,length(A)),1, promote_to_array_type(A), A...)...)
        end
    end
end


function hvcat(rows::Tuple{Vararg{Int}}, A::Union{AbstractVecOrMat,UniformScaling}...)
    nr = length(rows)
    sum(rows) == length(A) || throw(ArgumentError("mismatch between row sizes and number of arguments"))
    n = zeros(Int, length(A))
    needcols = false # whether we also need to infer some sizes from the column count
    j = 0
    for i = 1:nr # infer UniformScaling sizes from row counts, if possible:
        ni = 0 # number of rows in this block-row
        for k = 1:rows[i]
            if !isa(A[j+k], UniformScaling)
                na = size(A[j+k], 1)
                ni > 0 && ni != na &&
                    throw(DimensionMismatch("mismatch in number of rows"))
                ni = na
            end
        end
        if ni > 0
            for k = 1:rows[i]
                n[j+k] = ni
            end
        else # row consisted only of UniformScaling objects
            needcols = true
        end
        j += rows[i]
    end
    if needcols # some sizes still unknown, try to infer from column count
        nc = j = 0
        for i = 1:nr
            nci = 0
            rows[i] > 0 && n[j+1] == 0 && continue # column count unknown in this row
            for k = 1:rows[i]
                nci += isa(A[j+k], UniformScaling) ? n[j+k] : size(A[j+k], 2)
            end
            nc > 0 && nc != nci && throw(DimensionMismatch("mismatch in number of columns"))
            nc = nci
            j += rows[i]
        end
        nc == 0 && throw(ArgumentError("sizes of UniformScalings could not be inferred"))
        j = 0
        for i = 1:nr
            if rows[i] > 0 && n[j+1] == 0 # this row consists entirely of UniformScalings
                nci = nc ÷ rows[i]
                nci * rows[i] != nc && throw(DimensionMismatch("indivisible UniformScaling sizes"))
                for k = 1:rows[i]
                    n[j+k] = nci
                end
            end
            j += rows[i]
        end
    end
    return hvcat(rows, promote_to_arrays(n,1, promote_to_array_type(A), A...)...)
end


## Cholesky
function _chol!(J::UniformScaling, uplo)
    c, info = _chol!(J.λ, uplo)
    UniformScaling(c), info
end

chol!(J::UniformScaling, uplo) = ((J, info) = _chol!(J, uplo); @assertposdef J info)

"""
    chol(J::UniformScaling) -> C

Compute the square root of a non-negative UniformScaling `J`.

# Examples
```jldoctest
julia> chol(16I)
UniformScaling{Float64}
4.0*I
```
"""
chol(J::UniformScaling, args...) = ((C, info) = _chol!(J, nothing); @assertposdef C info)


## Matrix construction from UniformScaling
Matrix{T}(s::UniformScaling, dims::Dims{2}) where {T} = setindex!(zeros(T, dims), T(s.λ), diagind(dims...))
Matrix{T}(s::UniformScaling, m::Integer, n::Integer) where {T} = Matrix{T}(s, Dims((m, n)))
Matrix(s::UniformScaling, m::Integer, n::Integer) = Matrix(s, Dims((m, n)))
Matrix(s::UniformScaling, dims::Dims{2}) = Matrix{eltype(s)}(s, dims)
Array{T}(s::UniformScaling, dims::Dims{2}) where {T} = Matrix{T}(s, dims)
Array{T}(s::UniformScaling, m::Integer, n::Integer) where {T} = Matrix{T}(s, m, n)
Array(s::UniformScaling, m::Integer, n::Integer) = Matrix(s, m, n)
Array(s::UniformScaling, dims::Dims{2}) = Matrix(s, dims)

## Diagonal construction from UniformScaling
Diagonal{T}(s::UniformScaling, m::Integer) where {T} = Diagonal{T}(fill(T(s.λ), m))
Diagonal(s::UniformScaling, m::Integer) = Diagonal{eltype(s)}(s, m)
