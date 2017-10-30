# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base: copy, adjoint, getindex, show, transpose, one, zero, inv,
             hcat, vcat, hvcat, ^

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
julia> fill(1, (5,6)) * I == fill(1, (5,6))
true

julia> [1 2im 3; 1im 2 3] * I
2×3 Array{Complex{Int64},2}:
 1+0im  0+2im  3+0im
 0+1im  2+0im  3+0im
```
"""
const I = UniformScaling(true)

"""
    (I::UniformScaling)(n::Integer)

Construct a `Diagonal` matrix from a `UniformScaling`.

!!! compat "Julia 1.2"
     This method is available as of Julia 1.2.

# Examples
```jldoctest
julia> I(3)
3×3 Diagonal{Bool,Array{Bool,1}}:
 1  ⋅  ⋅
 ⋅  1  ⋅
 ⋅  ⋅  1

julia> (0.7*I)(3)
3×3 Diagonal{Float64,Array{Float64,1}}:
 0.7   ⋅    ⋅
  ⋅   0.7   ⋅
  ⋅    ⋅   0.7
```
"""
(I::UniformScaling)(n::Integer) = Diagonal(fill(I.λ, n))

eltype(::Type{UniformScaling{T}}) where {T} = T
ndims(J::UniformScaling) = 2
Base.has_offset_axes(::UniformScaling) = false
getindex(J::UniformScaling, i::Integer,j::Integer) = ifelse(i==j,J.λ,zero(J.λ))

function getindex(x::UniformScaling{T}, n::AbstractRange{<:Integer}, m::AbstractRange{<:Integer}) where T
    if length(n) == length(m) && step(n) == step(m)
        k = first(n) - first(m)
        if k % step(n) == 0 && length(n) - abs(k) > 0
            v = fill(x.λ, length(n) - abs(k))
            return spdiagm(k => v)
        else
            return spzeros(T, length(n), length(m))
        end
    end
    I = Int[]
    J = Int[]
    V = T[]
    @inbounds for (i,ii) in enumerate(n), (j,jj) in enumerate(m)
        if ii == jj
            push!(I, i)
            push!(J, j)
            push!(V, x.λ)
        end
    end
    return sparse(I, J, V, length(n), length(m))
end

function show(io::IO, ::MIME"text/plain", J::UniformScaling)
    s = "$(J.λ)"
    if occursin(r"\w+\s*[\+\-]\s*\w+", s)
        s = "($s)"
    end
    print(io, typeof(J), "\n$s*I")
end
copy(J::UniformScaling) = UniformScaling(J.λ)

conj(J::UniformScaling) = UniformScaling(conj(J.λ))
real(J::UniformScaling) = UniformScaling(real(J.λ))
imag(J::UniformScaling) = UniformScaling(imag(J.λ))

transpose(J::UniformScaling) = J
adjoint(J::UniformScaling) = UniformScaling(conj(J.λ))

one(::Type{UniformScaling{T}}) where {T} = UniformScaling(one(T))
one(J::UniformScaling{T}) where {T} = one(UniformScaling{T})
oneunit(::Type{UniformScaling{T}}) where {T} = UniformScaling(oneunit(T))
oneunit(J::UniformScaling{T}) where {T} = oneunit(UniformScaling{T})
zero(::Type{UniformScaling{T}}) where {T} = UniformScaling(zero(T))
zero(J::UniformScaling{T}) where {T} = zero(UniformScaling{T})

isdiag(::UniformScaling) = true
istriu(::UniformScaling) = true
istril(::UniformScaling) = true
issymmetric(::UniformScaling) = true
ishermitian(J::UniformScaling) = isreal(J.λ)
isposdef(J::UniformScaling) = isposdef(J.λ)

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
(-)(A::AbstractMatrix, J::UniformScaling)   = A + (-J)

# matrix functions
for f in ( :exp,   :log,
           :expm1, :log1p,
           :sqrt,  :cbrt,
           :sin,   :cos,   :tan,
           :asin,  :acos,  :atan,
           :csc,   :sec,   :cot,
           :acsc,  :asec,  :acot,
           :sinh,  :cosh,  :tanh,
           :asinh, :acosh, :atanh,
           :csch,  :sech,  :coth,
           :acsch, :asech, :acoth )
    @eval Base.$f(J::UniformScaling) = UniformScaling($f(J.λ))
end

# Unit{Lower/Upper}Triangular matrices become {Lower/Upper}Triangular under
# addition with a UniformScaling
for (t1, t2) in ((:UnitUpperTriangular, :UpperTriangular),
                 (:UnitLowerTriangular, :LowerTriangular))
    @eval begin
        function (+)(UL::$t1, J::UniformScaling)
            ULnew = copy_oftype(UL.data, Base._return_type(+, Tuple{eltype(UL), typeof(J)}))
            for i in axes(ULnew, 1)
                ULnew[i,i] = one(ULnew[i,i]) + J
            end
            return ($t2)(ULnew)
        end
    end
end

# Adding a complex UniformScaling to the diagonal of a Hermitian
# matrix breaks the hermiticity, if the UniformScaling is non-real.
# However, to preserve type stability, we do not special-case a
# UniformScaling{<:Complex} that happens to be real.
function (+)(A::Hermitian, J::UniformScaling{<:Complex})
    TS = Base._return_type(+, Tuple{eltype(A), typeof(J)})
    B = copytri!(copy_oftype(parent(A), TS), A.uplo, true)
    for i in diagind(B)
        B[i] = A[i] + J
    end
    return B
end

function (-)(J::UniformScaling{<:Complex}, A::Hermitian)
    TS = Base._return_type(+, Tuple{eltype(A), typeof(J)})
    B = copytri!(copy_oftype(parent(A), TS), A.uplo, true)
    B .= .-B
    for i in diagind(B)
        B[i] = J - A[i]
    end
    return B
end

function (+)(A::AbstractMatrix, J::UniformScaling)
    checksquare(A)
    B = copy_oftype(A, Base._return_type(+, Tuple{eltype(A), typeof(J)}))
    @inbounds for i in axes(A, 1)
        B[i,i] += J
    end
    return B
end

function (-)(J::UniformScaling, A::AbstractMatrix)
    checksquare(A)
    B = convert(AbstractMatrix{Base._return_type(+, Tuple{eltype(A), typeof(J)})}, -A)
    @inbounds for i in axes(A, 1)
        B[i,i] += J
    end
    return B
end

inv(J::UniformScaling) = UniformScaling(inv(J.λ))
opnorm(J::UniformScaling, p::Real=2) = opnorm(J.λ, p)

pinv(J::UniformScaling) = ifelse(iszero(J.λ),
                          UniformScaling(zero(inv(J.λ))),  # type stability
                          UniformScaling(inv(J.λ)))

function det(J::UniformScaling{T}) where T
    if isone(J.λ)
        one(T)
    elseif iszero(J.λ)
        zero(T)
    else
        throw(ArgumentError("Determinant of UniformScaling is only well-defined when λ = 0 or 1."))
    end
end

function tr(J::UniformScaling{T}) where T
    if iszero(J.λ)
        zero(T)
    else
        throw(ArgumentError("Trace of UniformScaling is only well-defined when λ = 0"))
    end
end

*(J1::UniformScaling, J2::UniformScaling) = UniformScaling(J1.λ*J2.λ)
*(B::BitArray{2}, J::UniformScaling) = *(Array(B), J::UniformScaling)
*(J::UniformScaling, B::BitArray{2}) = *(J::UniformScaling, Array(B))
*(A::AbstractMatrix, J::UniformScaling) = A*J.λ
*(v::AbstractVector, J::UniformScaling) = reshape(v, length(v), 1) * J
*(J::UniformScaling, A::AbstractVecOrMat) = J.λ*A
*(x::Number, J::UniformScaling) = UniformScaling(x*J.λ)
*(J::UniformScaling, x::Number) = UniformScaling(J.λ*x)

/(J1::UniformScaling, J2::UniformScaling) = J2.λ == 0 ? throw(SingularException(1)) : UniformScaling(J1.λ/J2.λ)
/(J::UniformScaling, A::AbstractMatrix) = lmul!(J.λ, inv(A))
/(A::AbstractMatrix, J::UniformScaling) = J.λ == 0 ? throw(SingularException(1)) : A/J.λ
/(v::AbstractVector, J::UniformScaling) = reshape(v, length(v), 1) / J

/(J::UniformScaling, x::Number) = UniformScaling(J.λ/x)

\(J1::UniformScaling, J2::UniformScaling) = J1.λ == 0 ? throw(SingularException(1)) : UniformScaling(J1.λ\J2.λ)
\(J::UniformScaling, A::AbstractVecOrMat) = J.λ == 0 ? throw(SingularException(1)) : J.λ\A
\(A::AbstractMatrix, J::UniformScaling) = rmul!(inv(A), J.λ)
\(F::Factorization, J::UniformScaling) = F \ J(size(F,1))

\(x::Number, J::UniformScaling) = UniformScaling(x\J.λ)

@inline mul!(C::AbstractMatrix, A::AbstractMatrix, J::UniformScaling, alpha::Number, beta::Number) =
    mul!(C, A, J.λ, alpha, beta)
@inline mul!(C::AbstractVecOrMat, J::UniformScaling, B::AbstractVecOrMat, alpha::Number, beta::Number) =
    mul!(C, J.λ, B, alpha, beta)
rmul!(A::AbstractMatrix, J::UniformScaling) = rmul!(A, J.λ)
lmul!(J::UniformScaling, B::AbstractVecOrMat) = lmul!(J.λ, B)
rdiv!(A::AbstractMatrix, J::UniformScaling) = rdiv!(A, J.λ)
ldiv!(J::UniformScaling, B::AbstractVecOrMat) = ldiv!(J.λ, B)
ldiv!(Y::AbstractVecOrMat, J::UniformScaling, B::AbstractVecOrMat) = (Y .= J.λ .\ B)

Broadcast.broadcasted(::typeof(*), x::Number,J::UniformScaling) = UniformScaling(x*J.λ)
Broadcast.broadcasted(::typeof(*), J::UniformScaling,x::Number) = UniformScaling(J.λ*x)

Broadcast.broadcasted(::typeof(/), J::UniformScaling,x::Number) = UniformScaling(J.λ/x)

Broadcast.broadcasted(::typeof(\), x::Number,J::UniformScaling) = UniformScaling(x\J.λ)

(^)(J::UniformScaling, x::Number) = UniformScaling((J.λ)^x)
Base.literal_pow(::typeof(^), J::UniformScaling, x::Val) = UniformScaling(Base.literal_pow(^, J.λ, x))

Broadcast.broadcasted(::typeof(^), J::UniformScaling, x::Number) = UniformScaling(J.λ^x)
function Broadcast.broadcasted(::typeof(Base.literal_pow), ::typeof(^), J::UniformScaling, x::Val)
    UniformScaling(Base.literal_pow(^, J.λ, x))
end

==(J1::UniformScaling,J2::UniformScaling) = (J1.λ == J2.λ)

## equality comparison with UniformScaling
==(J::UniformScaling, A::AbstractMatrix) = A == J
function ==(A::AbstractMatrix, J::UniformScaling)
    require_one_based_indexing(A)
    size(A, 1) == size(A, 2) || return false
    iszero(J.λ) && return iszero(A)
    isone(J.λ) && return isone(A)
    return A == J.λ*one(A)
end
function ==(A::StridedMatrix, J::UniformScaling)
    size(A, 1) == size(A, 2) || return false
    iszero(J.λ) && return iszero(A)
    isone(J.λ) && return isone(A)
    for j in axes(A, 2), i in axes(A, 1)
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
                  nans::Bool = false, norm::Function = norm)
    n = checksquare(A)
    normJ = norm === opnorm             ? abs(J.λ) :
            norm === LinearAlgebra.norm ? abs(J.λ) * sqrt(n) :
                                          norm(Diagonal(fill(J.λ, n)))
    return norm(A - J) <= max(atol, rtol * max(norm(A), normJ))
end
isapprox(A::AbstractMatrix, J::UniformScaling; kwargs...) = isapprox(J, A; kwargs...)

"""
    copyto!(dest::AbstractMatrix, src::UniformScaling)

Copies a [`UniformScaling`](@ref) onto a matrix.

!!! compat "Julia 1.1"
    In Julia 1.0 this method only supported a square destination matrix. Julia 1.1. added
    support for a rectangular matrix.
"""
function copyto!(A::AbstractMatrix, J::UniformScaling)
    require_one_based_indexing(A)
    fill!(A, 0)
    λ = J.λ
    for i = 1:min(size(A,1),size(A,2))
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
promote_to_arrays_(n::Int, ::Type{Matrix}, J::UniformScaling{T}) where {T} = copyto!(Matrix{T}(undef, n,n), J)
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
            n = -1
            for a in A
                if !isa(a, UniformScaling)
                    require_one_based_indexing(a)
                    na = size(a,$dim)
                    n >= 0 && n != na &&
                        throw(DimensionMismatch(string("number of ", $name,
                            " of each array must match (got ", n, " and ", na, ")")))
                    n = na
                end
            end
            n == -1 && throw(ArgumentError($("$f of only UniformScaling objects cannot determine the matrix size")))
            return $f(promote_to_arrays(fill(n,length(A)),1, promote_to_array_type(A), A...)...)
        end
    end
end


function hvcat(rows::Tuple{Vararg{Int}}, A::Union{AbstractVecOrMat,UniformScaling}...)
    require_one_based_indexing(A...)
    nr = length(rows)
    sum(rows) == length(A) || throw(ArgumentError("mismatch between row sizes and number of arguments"))
    n = fill(-1, length(A))
    needcols = false # whether we also need to infer some sizes from the column count
    j = 0
    for i = 1:nr # infer UniformScaling sizes from row counts, if possible:
        ni = -1 # number of rows in this block-row, -1 indicates unknown
        for k = 1:rows[i]
            if !isa(A[j+k], UniformScaling)
                na = size(A[j+k], 1)
                ni >= 0 && ni != na &&
                    throw(DimensionMismatch("mismatch in number of rows"))
                ni = na
            end
        end
        if ni >= 0
            for k = 1:rows[i]
                n[j+k] = ni
            end
        else # row consisted only of UniformScaling objects
            needcols = true
        end
        j += rows[i]
    end
    if needcols # some sizes still unknown, try to infer from column count
        nc = -1
        j = 0
        for i = 1:nr
            nci = 0
            rows[i] > 0 && n[j+1] == -1 && (j += rows[i]; continue)
            for k = 1:rows[i]
                nci += isa(A[j+k], UniformScaling) ? n[j+k] : size(A[j+k], 2)
            end
            nc >= 0 && nc != nci && throw(DimensionMismatch("mismatch in number of columns"))
            nc = nci
            j += rows[i]
        end
        nc == -1 && throw(ArgumentError("sizes of UniformScalings could not be inferred"))
        j = 0
        for i = 1:nr
            if rows[i] > 0 && n[j+1] == -1 # this row consists entirely of UniformScalings
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

## Matrix construction from UniformScaling
function Matrix{T}(s::UniformScaling, dims::Dims{2}) where {T}
    A = zeros(T, dims)
    v = T(s.λ)
    for i in diagind(dims...)
        @inbounds A[i] = v
    end
    return A
end
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

dot(x::AbstractVector, J::UniformScaling, y::AbstractVector) = dot(x, J.λ, y)
dot(x::AbstractVector, a::Number, y::AbstractVector) = sum(t -> dot(t[1], a, t[2]), zip(x, y))
dot(x::AbstractVector, a::Union{Real,Complex}, y::AbstractVector) = a*dot(x, y)
