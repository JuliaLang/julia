# This file is a part of Julia. License is MIT: https://julialang.org/license

## Triangular

# could be renamed to Triangular when that name has been fully deprecated
abstract type AbstractTriangular{T,S<:AbstractMatrix} <: AbstractMatrix{T} end

# First loop through all methods that don't need special care for upper/lower and unit diagonal
for t in (:LowerTriangular, :UnitLowerTriangular, :UpperTriangular,
          :UnitUpperTriangular)
    @eval begin
        struct $t{T,S<:AbstractMatrix{T}} <: AbstractTriangular{T,S}
            data::S

            function $t{T,S}(data) where {T,S<:AbstractMatrix{T}}
                require_one_based_indexing(data)
                checksquare(data)
                new{T,S}(data)
            end
        end
        $t(A::$t) = A
        $t{T}(A::$t{T}) where {T} = A
        function $t(A::AbstractMatrix)
            return $t{eltype(A), typeof(A)}(A)
        end
        function $t{T}(A::AbstractMatrix) where T
            $t(convert(AbstractMatrix{T}, A))
        end

        function $t{T}(A::$t) where T
            Anew = convert(AbstractMatrix{T}, A.data)
            $t(Anew)
        end
        Matrix(A::$t{T}) where {T} = Matrix{promote_with_zero(T)}(A)

        size(A::$t, d) = size(A.data, d)
        size(A::$t) = size(A.data)

        # For A<:AbstractTriangular, similar(A[, neweltype]) should yield a matrix with the same
        # triangular type and underlying storage type as A. The following method covers these cases.
        similar(A::$t, ::Type{T}) where {T} = $t(similar(parent(A), T))
        # On the other hand, similar(A, [neweltype,] shape...) should yield a matrix of the underlying
        # storage type of A (not wrapped in a triangular type). The following method covers these cases.
        similar(A::$t, ::Type{T}, dims::Dims{N}) where {T,N} = similar(parent(A), T, dims)

        copy(A::$t) = $t(copy(A.data))

        real(A::$t{<:Real}) = A
        real(A::$t{<:Complex}) = (B = real(A.data); $t(B))
    end
end

similar(A::Union{Adjoint{Ti,Tv}, Transpose{Ti,Tv}}, ::Type{T}) where {T,Ti,Tv<:LowerTriangular} =
    UpperTriangular(similar(parent(parent(A)), T))
similar(A::Union{Adjoint{Ti,Tv}, Transpose{Ti,Tv}}, ::Type{T}) where {T,Ti,Tv<:UnitLowerTriangular} =
    UnitUpperTriangular(similar(parent(parent(A)), T))
similar(A::Union{Adjoint{Ti,Tv}, Transpose{Ti,Tv}}, ::Type{T}) where {T,Ti,Tv<:UpperTriangular} =
    LowerTriangular(similar(parent(parent(A)), T))
similar(A::Union{Adjoint{Ti,Tv}, Transpose{Ti,Tv}}, ::Type{T}) where {T,Ti,Tv<:UnitUpperTriangular} =
    UnitLowerTriangular(similar(parent(parent(A)), T))


"""
    LowerTriangular(A::AbstractMatrix)

Construct a `LowerTriangular` view of the matrix `A`.

# Examples
```jldoctest
julia> A = [1.0 2.0 3.0; 4.0 5.0 6.0; 7.0 8.0 9.0]
3×3 Array{Float64,2}:
 1.0  2.0  3.0
 4.0  5.0  6.0
 7.0  8.0  9.0

julia> LowerTriangular(A)
3×3 LowerTriangular{Float64,Array{Float64,2}}:
 1.0   ⋅    ⋅
 4.0  5.0   ⋅
 7.0  8.0  9.0
```
"""
LowerTriangular
"""
    UpperTriangular(A::AbstractMatrix)

Construct an `UpperTriangular` view of the matrix `A`.

# Examples
```jldoctest
julia> A = [1.0 2.0 3.0; 4.0 5.0 6.0; 7.0 8.0 9.0]
3×3 Array{Float64,2}:
 1.0  2.0  3.0
 4.0  5.0  6.0
 7.0  8.0  9.0

julia> UpperTriangular(A)
3×3 UpperTriangular{Float64,Array{Float64,2}}:
 1.0  2.0  3.0
  ⋅   5.0  6.0
  ⋅    ⋅   9.0
```
"""
UpperTriangular
"""
    UnitLowerTriangular(A::AbstractMatrix)

Construct a `UnitLowerTriangular` view of the matrix `A`.
Such a view has the [`oneunit`](@ref) of the [`eltype`](@ref)
of `A` on its diagonal.

# Examples
```jldoctest
julia> A = [1.0 2.0 3.0; 4.0 5.0 6.0; 7.0 8.0 9.0]
3×3 Array{Float64,2}:
 1.0  2.0  3.0
 4.0  5.0  6.0
 7.0  8.0  9.0

julia> UnitLowerTriangular(A)
3×3 UnitLowerTriangular{Float64,Array{Float64,2}}:
 1.0   ⋅    ⋅
 4.0  1.0   ⋅
 7.0  8.0  1.0
```
"""
UnitLowerTriangular
"""
    UnitUpperTriangular(A::AbstractMatrix)

Construct an `UnitUpperTriangular` view of the matrix `A`.
Such a view has the [`oneunit`](@ref) of the [`eltype`](@ref)
of `A` on its diagonal.

# Examples
```jldoctest
julia> A = [1.0 2.0 3.0; 4.0 5.0 6.0; 7.0 8.0 9.0]
3×3 Array{Float64,2}:
 1.0  2.0  3.0
 4.0  5.0  6.0
 7.0  8.0  9.0

julia> UnitUpperTriangular(A)
3×3 UnitUpperTriangular{Float64,Array{Float64,2}}:
 1.0  2.0  3.0
  ⋅   1.0  6.0
  ⋅    ⋅   1.0
```
"""
UnitUpperTriangular

imag(A::UpperTriangular) = UpperTriangular(imag(A.data))
imag(A::LowerTriangular) = LowerTriangular(imag(A.data))
imag(A::UnitLowerTriangular) = LowerTriangular(tril!(imag(A.data),-1))
imag(A::UnitUpperTriangular) = UpperTriangular(triu!(imag(A.data),1))

Array(A::AbstractTriangular) = Matrix(A)
parent(A::AbstractTriangular) = A.data

# then handle all methods that requires specific handling of upper/lower and unit diagonal

function Matrix{T}(A::LowerTriangular) where T
    B = Matrix{T}(undef, size(A, 1), size(A, 1))
    copyto!(B, A.data)
    tril!(B)
    B
end
function Matrix{T}(A::UnitLowerTriangular) where T
    B = Matrix{T}(undef, size(A, 1), size(A, 1))
    copyto!(B, A.data)
    tril!(B)
    for i = 1:size(B,1)
        B[i,i] = 1
    end
    B
end
function Matrix{T}(A::UpperTriangular) where T
    B = Matrix{T}(undef, size(A, 1), size(A, 1))
    copyto!(B, A.data)
    triu!(B)
    B
end
function Matrix{T}(A::UnitUpperTriangular) where T
    B = Matrix{T}(undef, size(A, 1), size(A, 1))
    copyto!(B, A.data)
    triu!(B)
    for i = 1:size(B,1)
        B[i,i] = 1
    end
    B
end

function full!(A::LowerTriangular)
    B = A.data
    tril!(B)
    B
end
function full!(A::UnitLowerTriangular)
    B = A.data
    tril!(B)
    for i = 1:size(A,1)
        B[i,i] = 1
    end
    B
end
function full!(A::UpperTriangular)
    B = A.data
    triu!(B)
    B
end
function full!(A::UnitUpperTriangular)
    B = A.data
    triu!(B)
    for i = 1:size(A,1)
        B[i,i] = 1
    end
    B
end

getindex(A::UnitLowerTriangular{T}, i::Integer, j::Integer) where {T} =
    i > j ? A.data[i,j] : ifelse(i == j, oneunit(T), zero(T))
getindex(A::LowerTriangular, i::Integer, j::Integer) =
    i >= j ? A.data[i,j] : zero(A.data[j,i])
getindex(A::UnitUpperTriangular{T}, i::Integer, j::Integer) where {T} =
    i < j ? A.data[i,j] : ifelse(i == j, oneunit(T), zero(T))
getindex(A::UpperTriangular, i::Integer, j::Integer) =
    i <= j ? A.data[i,j] : zero(A.data[j,i])

function setindex!(A::UpperTriangular, x, i::Integer, j::Integer)
    if i > j
        x == 0 || throw(ArgumentError("cannot set index in the lower triangular part " *
            "($i, $j) of an UpperTriangular matrix to a nonzero value ($x)"))
    else
        A.data[i,j] = x
    end
    return A
end

function setindex!(A::UnitUpperTriangular, x, i::Integer, j::Integer)
    if i > j
        x == 0 || throw(ArgumentError("cannot set index in the lower triangular part " *
            "($i, $j) of a UnitUpperTriangular matrix to a nonzero value ($x)"))
    elseif i == j
        x == 1 || throw(ArgumentError("cannot set index on the diagonal ($i, $j) " *
            "of a UnitUpperTriangular matrix to a non-unit value ($x)"))
    else
        A.data[i,j] = x
    end
    return A
end

function setindex!(A::LowerTriangular, x, i::Integer, j::Integer)
    if i < j
        x == 0 || throw(ArgumentError("cannot set index in the upper triangular part " *
            "($i, $j) of a LowerTriangular matrix to a nonzero value ($x)"))
    else
        A.data[i,j] = x
    end
    return A
end

function setindex!(A::UnitLowerTriangular, x, i::Integer, j::Integer)
    if i < j
        x == 0 || throw(ArgumentError("cannot set index in the upper triangular part " *
            "($i, $j) of a UnitLowerTriangular matrix to a nonzero value ($x)"))
    elseif i == j
        x == 1 || throw(ArgumentError("cannot set index on the diagonal ($i, $j) " *
            "of a UnitLowerTriangular matrix to a non-unit value ($x)"))
    else
        A.data[i,j] = x
    end
    return A
end


## structured matrix methods ##
function Base.replace_in_print_matrix(A::Union{UpperTriangular,UnitUpperTriangular},
                                      i::Integer, j::Integer, s::AbstractString)
    return i <= j ? s : Base.replace_with_centered_mark(s)
end
function Base.replace_in_print_matrix(A::Union{LowerTriangular,UnitLowerTriangular},
                                      i::Integer, j::Integer, s::AbstractString)
    return i >= j ? s : Base.replace_with_centered_mark(s)
end

function istril(A::Union{LowerTriangular,UnitLowerTriangular}, k::Integer=0)
    k >= 0 && return true
    m, n = size(A)
    for j in max(1, k + 2):n
        for i in 1:min(j - k - 1, m)
            iszero(A[i, j]) || return false
        end
    end
    return true
end
function istriu(A::Union{UpperTriangular,UnitUpperTriangular}, k::Integer=0)
    k <= 0 && return true
    m, n = size(A)
    for j in 1:min(n, m + k - 1)
        for i in max(1, j - k + 1):m
            iszero(A[i, j]) || return false
        end
    end
    return true
end
istril(A::Adjoint) = istriu(A.parent)
istril(A::Transpose) = istriu(A.parent)
istriu(A::Adjoint) = istril(A.parent)
istriu(A::Transpose) = istril(A.parent)

function tril!(A::UpperTriangular, k::Integer=0)
    n = size(A,1)
    if k < 0
        fill!(A.data,0)
        return A
    elseif k == 0
        for j in 1:n, i in 1:j-1
            A.data[i,j] = 0
        end
        return A
    else
        return UpperTriangular(tril!(A.data,k))
    end
end
triu!(A::UpperTriangular, k::Integer=0) = UpperTriangular(triu!(A.data,k))

function tril!(A::UnitUpperTriangular{T}, k::Integer=0) where T
    n = size(A,1)
    if k < 0
        fill!(A.data, zero(T))
        return UpperTriangular(A.data)
    elseif k == 0
        fill!(A.data, zero(T))
        for i in diagind(A)
            A.data[i] = oneunit(T)
        end
        return UpperTriangular(A.data)
    else
        for i in diagind(A)
            A.data[i] = oneunit(T)
        end
        return UpperTriangular(tril!(A.data,k))
    end
end

function triu!(A::UnitUpperTriangular, k::Integer=0)
    for i in diagind(A)
        A.data[i] = oneunit(eltype(A))
    end
    return triu!(UpperTriangular(A.data),k)
end

function triu!(A::LowerTriangular, k::Integer=0)
    n = size(A,1)
    if k > 0
        fill!(A.data,0)
        return A
    elseif k == 0
        for j in 1:n, i in j+1:n
            A.data[i,j] = 0
        end
        return A
    else
        return LowerTriangular(triu!(A.data,k))
    end
end

tril!(A::LowerTriangular, k::Integer=0) = LowerTriangular(tril!(A.data,k))

function triu!(A::UnitLowerTriangular{T}, k::Integer=0) where T
    n = size(A,1)
    if k > 0
        fill!(A.data, zero(T))
        return LowerTriangular(A.data)
    elseif k == 0
        fill!(A.data, zero(T))
        for i in diagind(A)
            A.data[i] = oneunit(T)
        end
        return LowerTriangular(A.data)
    else
        for i in diagind(A)
            A.data[i] = oneunit(T)
        end
        return LowerTriangular(triu!(A.data,k))
    end
end

function tril!(A::UnitLowerTriangular, k::Integer=0)
    for i in diagind(A)
        A.data[i] = oneunit(eltype(A))
    end
    return tril!(LowerTriangular(A.data),k)
end

# TODO consolidate
adjoint(A::LowerTriangular) = Adjoint(A)
adjoint(A::UpperTriangular) = Adjoint(A)
adjoint(A::UnitLowerTriangular) = Adjoint(A)
adjoint(A::UnitUpperTriangular) = Adjoint(A)
transpose(A::LowerTriangular) = Transpose(A)
transpose(A::UpperTriangular) = Transpose(A)
transpose(A::UnitLowerTriangular) = Transpose(A)
transpose(A::UnitUpperTriangular) = Transpose(A)

# TODO consolidate
Base.copy(A::Adjoint{<:Any,<:LowerTriangular}) = adjoint!(copy(A.parent))
Base.copy(A::Adjoint{<:Any,<:UpperTriangular}) = adjoint!(copy(A.parent))
Base.copy(A::Adjoint{<:Any,<:UnitLowerTriangular}) = adjoint!(copy(A.parent))
Base.copy(A::Adjoint{<:Any,<:UnitUpperTriangular}) = adjoint!(copy(A.parent))
Base.copy(A::Transpose{<:Any,<:LowerTriangular}) = transpose!(copy(A.parent))
Base.copy(A::Transpose{<:Any,<:UpperTriangular}) = transpose!(copy(A.parent))
Base.copy(A::Transpose{<:Any,<:UnitLowerTriangular}) = transpose!(copy(A.parent))
Base.copy(A::Transpose{<:Any,<:UnitUpperTriangular}) = transpose!(copy(A.parent))

transpose!(A::LowerTriangular) = UpperTriangular(copytri!(A.data, 'L', false, true))
transpose!(A::UnitLowerTriangular) = UnitUpperTriangular(copytri!(A.data, 'L', false, true))
transpose!(A::UpperTriangular) = LowerTriangular(copytri!(A.data, 'U', false, true))
transpose!(A::UnitUpperTriangular) = UnitLowerTriangular(copytri!(A.data, 'U', false, true))
adjoint!(A::LowerTriangular) = UpperTriangular(copytri!(A.data, 'L' , true, true))
adjoint!(A::UnitLowerTriangular) = UnitUpperTriangular(copytri!(A.data, 'L' , true, true))
adjoint!(A::UpperTriangular) = LowerTriangular(copytri!(A.data, 'U' , true, true))
adjoint!(A::UnitUpperTriangular) = UnitLowerTriangular(copytri!(A.data, 'U' , true, true))

diag(A::LowerTriangular) = diag(A.data)
diag(A::UnitLowerTriangular) = fill(one(eltype(A)), size(A,1))
diag(A::UpperTriangular) = diag(A.data)
diag(A::UnitUpperTriangular) = fill(one(eltype(A)), size(A,1))

# Unary operations
-(A::LowerTriangular) = LowerTriangular(-A.data)
-(A::UpperTriangular) = UpperTriangular(-A.data)
function -(A::UnitLowerTriangular)
    Anew = -A.data
    for i = 1:size(A, 1)
        Anew[i, i] = -1
    end
    LowerTriangular(Anew)
end
function -(A::UnitUpperTriangular)
    Anew = -A.data
    for i = 1:size(A, 1)
        Anew[i, i] = -1
    end
    UpperTriangular(Anew)
end

# copy and scale
function copyto!(A::T, B::T) where T<:Union{UpperTriangular,UnitUpperTriangular}
    n = size(B,1)
    for j = 1:n
        for i = 1:(isa(B, UnitUpperTriangular) ? j-1 : j)
            @inbounds A[i,j] = B[i,j]
        end
    end
    return A
end
function copyto!(A::T, B::T) where T<:Union{LowerTriangular,UnitLowerTriangular}
    n = size(B,1)
    for j = 1:n
        for i = (isa(B, UnitLowerTriangular) ? j+1 : j):n
            @inbounds A[i,j] = B[i,j]
        end
    end
    return A
end

# Define `mul!` for (Unit){Upper,Lower}Triangular matrices times a
# number.
for (Trig, UnitTrig) in [(UpperTriangular, UnitUpperTriangular),
                         (LowerTriangular, UnitLowerTriangular)]
    for (TB, TC) in [(Trig, Number),
                     (Number, Trig),
                     (UnitTrig, Number),
                     (Number, UnitTrig)]
        @eval @inline mul!(A::$Trig, B::$TB, C::$TC, alpha::Number, beta::Number) =
            _mul!(A, B, C, MulAddMul(alpha, beta))
    end
end

@inline function _mul!(A::UpperTriangular, B::UpperTriangular, c::Number, _add::MulAddMul)
    n = checksquare(B)
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    for j = 1:n
        for i = 1:j
            @inbounds _modify!(_add, B[i,j] * c, A, (i,j))
        end
    end
    return A
end
@inline function _mul!(A::UpperTriangular, c::Number, B::UpperTriangular, _add::MulAddMul)
    n = checksquare(B)
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    for j = 1:n
        for i = 1:j
            @inbounds _modify!(_add, c * B[i,j], A, (i,j))
        end
    end
    return A
end
@inline function _mul!(A::UpperTriangular, B::UnitUpperTriangular, c::Number, _add::MulAddMul)
    n = checksquare(B)
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    for j = 1:n
        @inbounds _modify!(_add, c, A, (j,j))
        for i = 1:(j - 1)
            @inbounds _modify!(_add, B[i,j] * c, A, (i,j))
        end
    end
    return A
end
@inline function _mul!(A::UpperTriangular, c::Number, B::UnitUpperTriangular, _add::MulAddMul)
    n = checksquare(B)
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    for j = 1:n
        @inbounds _modify!(_add, c, A, (j,j))
        for i = 1:(j - 1)
            @inbounds _modify!(_add, c * B[i,j], A, (i,j))
        end
    end
    return A
end
@inline function _mul!(A::LowerTriangular, B::LowerTriangular, c::Number, _add::MulAddMul)
    n = checksquare(B)
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    for j = 1:n
        for i = j:n
            @inbounds _modify!(_add, B[i,j] * c, A, (i,j))
        end
    end
    return A
end
@inline function _mul!(A::LowerTriangular, c::Number, B::LowerTriangular, _add::MulAddMul)
    n = checksquare(B)
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    for j = 1:n
        for i = j:n
            @inbounds _modify!(_add, c * B[i,j], A, (i,j))
        end
    end
    return A
end
@inline function _mul!(A::LowerTriangular, B::UnitLowerTriangular, c::Number, _add::MulAddMul)
    n = checksquare(B)
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    for j = 1:n
        @inbounds _modify!(_add, c, A, (j,j))
        for i = (j + 1):n
            @inbounds _modify!(_add, B[i,j] * c, A, (i,j))
        end
    end
    return A
end
@inline function _mul!(A::LowerTriangular, c::Number, B::UnitLowerTriangular, _add::MulAddMul)
    n = checksquare(B)
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    for j = 1:n
        @inbounds _modify!(_add, c, A, (j,j))
        for i = (j + 1):n
            @inbounds _modify!(_add, c * B[i,j], A, (i,j))
        end
    end
    return A
end

rmul!(A::Union{UpperTriangular,LowerTriangular}, c::Number) = mul!(A, A, c)
lmul!(c::Number, A::Union{UpperTriangular,LowerTriangular}) = mul!(A, c, A)

function dot(x::AbstractVector, A::UpperTriangular, y::AbstractVector)
    require_one_based_indexing(x, y)
    m = size(A, 1)
    (length(x) == m == length(y)) || throw(DimensionMismatch())
    if iszero(m)
        return dot(zero(eltype(x)), zero(eltype(A)), zero(eltype(y)))
    end
    x₁ = x[1]
    r = dot(x₁, A[1,1], y[1])
    @inbounds for j in 2:m
        yj = y[j]
        if !iszero(yj)
            temp = adjoint(A[1,j]) * x₁
            @simd for i in 2:j
                temp += adjoint(A[i,j]) * x[i]
            end
            r += dot(temp, yj)
        end
    end
    return r
end
function dot(x::AbstractVector, A::UnitUpperTriangular, y::AbstractVector)
    require_one_based_indexing(x, y)
    m = size(A, 1)
    (length(x) == m == length(y)) || throw(DimensionMismatch())
    if iszero(m)
        return dot(zero(eltype(x)), zero(eltype(A)), zero(eltype(y)))
    end
    x₁ = first(x)
    r = dot(x₁, y[1])
    @inbounds for j in 2:m
        yj = y[j]
        if !iszero(yj)
            temp = adjoint(A[1,j]) * x₁
            @simd for i in 2:j-1
                temp += adjoint(A[i,j]) * x[i]
            end
            r += dot(temp, yj)
            r += dot(x[j], yj)
        end
    end
    return r
end
function dot(x::AbstractVector, A::LowerTriangular, y::AbstractVector)
    require_one_based_indexing(x, y)
    m = size(A, 1)
    (length(x) == m == length(y)) || throw(DimensionMismatch())
    if iszero(m)
        return dot(zero(eltype(x)), zero(eltype(A)), zero(eltype(y)))
    end
    r = zero(typeof(dot(first(x), first(A), first(y))))
    @inbounds for j in 1:m
        yj = y[j]
        if !iszero(yj)
            temp = adjoint(A[j,j]) * x[j]
            @simd for i in j+1:m
                temp += adjoint(A[i,j]) * x[i]
            end
            r += dot(temp, yj)
        end
    end
    return r
end
function dot(x::AbstractVector, A::UnitLowerTriangular, y::AbstractVector)
    require_one_based_indexing(x, y)
    m = size(A, 1)
    (length(x) == m == length(y)) || throw(DimensionMismatch())
    if iszero(m)
        return dot(zero(eltype(x)), zero(eltype(A)), zero(eltype(y)))
    end
    r = zero(typeof(dot(first(x), first(y))))
    @inbounds for j in 1:m
        yj = y[j]
        if !iszero(yj)
            temp = x[j]
            @simd for i in j+1:m
                temp += adjoint(A[i,j]) * x[i]
            end
            r += dot(temp, yj)
        end
    end
    return r
end

fillstored!(A::LowerTriangular, x)     = (fillband!(A.data, x, 1-size(A,1), 0); A)
fillstored!(A::UnitLowerTriangular, x) = (fillband!(A.data, x, 1-size(A,1), -1); A)
fillstored!(A::UpperTriangular, x)     = (fillband!(A.data, x, 0, size(A,2)-1); A)
fillstored!(A::UnitUpperTriangular, x) = (fillband!(A.data, x, 1, size(A,2)-1); A)

# Binary operations
+(A::UpperTriangular, B::UpperTriangular) = UpperTriangular(A.data + B.data)
+(A::LowerTriangular, B::LowerTriangular) = LowerTriangular(A.data + B.data)
+(A::UpperTriangular, B::UnitUpperTriangular) = UpperTriangular(A.data + triu(B.data, 1) + I)
+(A::LowerTriangular, B::UnitLowerTriangular) = LowerTriangular(A.data + tril(B.data, -1) + I)
+(A::UnitUpperTriangular, B::UpperTriangular) = UpperTriangular(triu(A.data, 1) + B.data + I)
+(A::UnitLowerTriangular, B::LowerTriangular) = LowerTriangular(tril(A.data, -1) + B.data + I)
+(A::UnitUpperTriangular, B::UnitUpperTriangular) = UpperTriangular(triu(A.data, 1) + triu(B.data, 1) + 2I)
+(A::UnitLowerTriangular, B::UnitLowerTriangular) = LowerTriangular(tril(A.data, -1) + tril(B.data, -1) + 2I)
+(A::AbstractTriangular, B::AbstractTriangular) = copyto!(similar(parent(A)), A) + copyto!(similar(parent(B)), B)

-(A::UpperTriangular, B::UpperTriangular) = UpperTriangular(A.data - B.data)
-(A::LowerTriangular, B::LowerTriangular) = LowerTriangular(A.data - B.data)
-(A::UpperTriangular, B::UnitUpperTriangular) = UpperTriangular(A.data - triu(B.data, 1) - I)
-(A::LowerTriangular, B::UnitLowerTriangular) = LowerTriangular(A.data - tril(B.data, -1) - I)
-(A::UnitUpperTriangular, B::UpperTriangular) = UpperTriangular(triu(A.data, 1) - B.data + I)
-(A::UnitLowerTriangular, B::LowerTriangular) = LowerTriangular(tril(A.data, -1) - B.data + I)
-(A::UnitUpperTriangular, B::UnitUpperTriangular) = UpperTriangular(triu(A.data, 1) - triu(B.data, 1))
-(A::UnitLowerTriangular, B::UnitLowerTriangular) = LowerTriangular(tril(A.data, -1) - tril(B.data, -1))
-(A::AbstractTriangular, B::AbstractTriangular) = copyto!(similar(parent(A)), A) - copyto!(similar(parent(B)), B)

######################
# BlasFloat routines #
######################

lmul!(A::Tridiagonal, B::AbstractTriangular) = A*full!(B) # is this necessary?

@inline mul!(C::AbstractMatrix, A::AbstractTriangular, B::Tridiagonal, alpha::Number, beta::Number) =
    mul!(C, copyto!(similar(parent(A)), A), B, alpha, beta)
@inline mul!(C::AbstractMatrix, A::Tridiagonal, B::AbstractTriangular, alpha::Number, beta::Number) =
    mul!(C, A, copyto!(similar(parent(B)), B), alpha, beta)
mul!(C::AbstractVector, A::AbstractTriangular, transB::Transpose{<:Any,<:AbstractVecOrMat}) =
    (B = transB.parent; lmul!(A, transpose!(C, B)))
mul!(C::AbstractMatrix, A::AbstractTriangular, transB::Transpose{<:Any,<:AbstractVecOrMat}) =
    (B = transB.parent; lmul!(A, transpose!(C, B)))
mul!(C::AbstractMatrix, A::AbstractTriangular, adjB::Adjoint{<:Any,<:AbstractVecOrMat}) =
    (B = adjB.parent; lmul!(A, adjoint!(C, B)))
mul!(C::AbstractVecOrMat, A::AbstractTriangular, adjB::Adjoint{<:Any,<:AbstractVecOrMat}) =
    (B = adjB.parent; lmul!(A, adjoint!(C, B)))

# The three methods for each op are neceesary to avoid ambiguities with definitions in matmul.jl
mul!(C::AbstractVector  , A::AbstractTriangular, B::AbstractVector)   = lmul!(A, copyto!(C, B))
mul!(C::AbstractMatrix  , A::AbstractTriangular, B::AbstractVecOrMat) = lmul!(A, copyto!(C, B))
mul!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat) = lmul!(A, copyto!(C, B))
function mul!(C::AbstractVector, adjA::Adjoint{<:Any,<:AbstractTriangular}, B::AbstractVector)
    return lmul!(adjA, copyto!(C, B))
end
function mul!(C::AbstractMatrix, adjA::Adjoint{<:Any,<:AbstractTriangular}, B::AbstractVecOrMat)
    return lmul!(adjA, copyto!(C, B))
end
function mul!(C::AbstractVecOrMat, adjA::Adjoint{<:Any,<:AbstractTriangular}, B::AbstractVecOrMat)
    return lmul!(adjA, copyto!(C, B))
end
function mul!(C::AbstractVector, transA::Transpose{<:Any,<:AbstractTriangular}, B::AbstractVector)
    return lmul!(transA, copyto!(C, B))
end
function mul!(C::AbstractMatrix, transA::Transpose{<:Any,<:AbstractTriangular}, B::AbstractVecOrMat)
    return lmul!(transA, copyto!(C, B))
end
function mul!(C::AbstractVecOrMat, transA::Transpose{<:Any,<:AbstractTriangular}, B::AbstractVecOrMat)
    return lmul!(transA, copyto!(C, B))
end
@inline mul!(C::AbstractMatrix, A::Adjoint{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:AbstractVecOrMat}, alpha::Number, beta::Number) =
    mul!(C, A, copy(B), alpha, beta)
@inline mul!(C::AbstractMatrix, A::Adjoint{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:AbstractVecOrMat}, alpha::Number, beta::Number) =
    mul!(C, A, copy(B), alpha, beta)
@inline mul!(C::AbstractMatrix, A::Transpose{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:AbstractVecOrMat}, alpha::Number, beta::Number) =
    mul!(C, A, copy(B), alpha, beta)
@inline mul!(C::AbstractMatrix, A::Transpose{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:AbstractVecOrMat}, alpha::Number, beta::Number) =
    mul!(C, A, copy(B), alpha, beta)
mul!(C::AbstractVector, A::Adjoint{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:AbstractVecOrMat}) = throw(MethodError(mul!, (C, A, B)))
mul!(C::AbstractVector, A::Transpose{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:AbstractVecOrMat}) = throw(MethodError(mul!, (C, A, B)))

for (t, uploc, isunitc) in ((:LowerTriangular, 'L', 'N'),
                            (:UnitLowerTriangular, 'L', 'U'),
                            (:UpperTriangular, 'U', 'N'),
                            (:UnitUpperTriangular, 'U', 'U'))
    @eval begin
        # Vector multiplication
        lmul!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasFloat} =
            BLAS.trmv!($uploc, 'N', $isunitc, A.data, b)
        lmul!(transA::Transpose{<:Any,<:$t{T,<:StridedMatrix}}, b::StridedVector{T}) where {T<:BlasFloat} =
            (A = transA.parent; BLAS.trmv!($uploc, 'T', $isunitc, A.data, b))
        lmul!(adjA::Adjoint{<:Any,<:$t{T,<:StridedMatrix}}, b::StridedVector{T}) where {T<:BlasReal} =
            (A = adjA.parent; BLAS.trmv!($uploc, 'T', $isunitc, A.data, b))
        lmul!(adjA::Adjoint{<:Any,<:$t{T,<:StridedMatrix}}, b::StridedVector{T}) where {T<:BlasComplex} =
            (A = adjA.parent; BLAS.trmv!($uploc, 'C', $isunitc, A.data, b))

        # Matrix multiplication
        lmul!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat} =
            BLAS.trmm!('L', $uploc, 'N', $isunitc, one(T), A.data, B)
        rmul!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat} =
            BLAS.trmm!('R', $uploc, 'N', $isunitc, one(T), B.data, A)

        lmul!(transA::Transpose{<:Any,<:$t{T,<:StridedMatrix}}, B::StridedMatrix{T}) where {T<:BlasFloat} =
            (A = transA.parent; BLAS.trmm!('L', $uploc, 'T', $isunitc, one(T), A.data, B))
        lmul!(adjA::Adjoint{<:Any,<:$t{T,<:StridedMatrix}}, B::StridedMatrix{T}) where {T<:BlasComplex} =
            (A = adjA.parent; BLAS.trmm!('L', $uploc, 'C', $isunitc, one(T), A.data, B))
        lmul!(adjA::Adjoint{<:Any,<:$t{T,<:StridedMatrix}}, B::StridedMatrix{T}) where {T<:BlasReal} =
            (A = adjA.parent; BLAS.trmm!('L', $uploc, 'T', $isunitc, one(T), A.data, B))

        rmul!(A::StridedMatrix{T}, transB::Transpose{<:Any,<:$t{T,<:StridedMatrix}}) where {T<:BlasFloat} =
            (B = transB.parent; BLAS.trmm!('R', $uploc, 'T', $isunitc, one(T), B.data, A))
        rmul!(A::StridedMatrix{T}, adjB::Adjoint{<:Any,<:$t{T,<:StridedMatrix}}) where {T<:BlasComplex} =
            (B = adjB.parent; BLAS.trmm!('R', $uploc, 'C', $isunitc, one(T), B.data, A))
        rmul!(A::StridedMatrix{T}, adjB::Adjoint{<:Any,<:$t{T,<:StridedMatrix}}) where {T<:BlasReal} =
            (B = adjB.parent; BLAS.trmm!('R', $uploc, 'T', $isunitc, one(T), B.data, A))

        # Left division
        ldiv!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
            LAPACK.trtrs!($uploc, 'N', $isunitc, A.data, B)
        ldiv!(transA::Transpose{<:Any,<:$t{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
            (A = transA.parent; LAPACK.trtrs!($uploc, 'T', $isunitc, A.data, B))
        ldiv!(adjA::Adjoint{<:Any,<:$t{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasReal} =
            (A = adjA.parent; LAPACK.trtrs!($uploc, 'T', $isunitc, A.data, B))
        ldiv!(adjA::Adjoint{<:Any,<:$t{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasComplex} =
            (A = adjA.parent; LAPACK.trtrs!($uploc, 'C', $isunitc, A.data, B))

        # Right division
        rdiv!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat} =
            BLAS.trsm!('R', $uploc, 'N', $isunitc, one(T), B.data, A)
        rdiv!(A::StridedMatrix{T}, transB::Transpose{<:Any,<:$t{T,<:StridedMatrix}}) where {T<:BlasFloat} =
            (B = transB.parent; BLAS.trsm!('R', $uploc, 'T', $isunitc, one(T), B.data, A))
        rdiv!(A::StridedMatrix{T}, adjB::Adjoint{<:Any,<:$t{T,<:StridedMatrix}}) where {T<:BlasReal} =
            (B = adjB.parent; BLAS.trsm!('R', $uploc, 'T', $isunitc, one(T), B.data, A))
        rdiv!(A::StridedMatrix{T}, adjB::Adjoint{<:Any,<:$t{T,<:StridedMatrix}}) where {T<:BlasComplex} =
            (B = adjB.parent; BLAS.trsm!('R', $uploc, 'C', $isunitc, one(T), B.data, A))

        # Matrix inverse
        inv!(A::$t{T,S}) where {T<:BlasFloat,S<:StridedMatrix} =
            $t{T,S}(LAPACK.trtri!($uploc, $isunitc, A.data))

        # Error bounds for triangular solve
        errorbounds(A::$t{T,<:StridedMatrix}, X::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
            LAPACK.trrfs!($uploc, 'N', $isunitc, A.data, B, X)

        # Condition numbers
        function cond(A::$t{<:BlasFloat}, p::Real=2)
            checksquare(A)
            if p == 1
                return inv(LAPACK.trcon!('O', $uploc, $isunitc, A.data))
            elseif p == Inf
                return inv(LAPACK.trcon!('I', $uploc, $isunitc, A.data))
            else # use fallback
                return cond(copyto!(similar(parent(A)), A), p)
            end
        end
    end
end

function inv(A::LowerTriangular{T}) where T
    S = typeof((zero(T)*one(T) + zero(T))/one(T))
    LowerTriangular(ldiv!(convert(AbstractArray{S}, A), Matrix{S}(I, size(A, 1), size(A, 1))))
end
function inv(A::UpperTriangular{T}) where T
    S = typeof((zero(T)*one(T) + zero(T))/one(T))
    UpperTriangular(ldiv!(convert(AbstractArray{S}, A), Matrix{S}(I, size(A, 1), size(A, 1))))
end
inv(A::UnitUpperTriangular{T}) where {T} = UnitUpperTriangular(ldiv!(A, Matrix{T}(I, size(A, 1), size(A, 1))))
inv(A::UnitLowerTriangular{T}) where {T} = UnitLowerTriangular(ldiv!(A, Matrix{T}(I, size(A, 1), size(A, 1))))

errorbounds(A::AbstractTriangular{T,<:StridedMatrix}, X::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:Union{BigFloat,Complex{BigFloat}}} =
    error("not implemented yet! Please submit a pull request.")
function errorbounds(A::AbstractTriangular{TA,<:StridedMatrix}, X::StridedVecOrMat{TX}, B::StridedVecOrMat{TB}) where {TA<:Number,TX<:Number,TB<:Number}
    TAXB = promote_type(TA, TB, TX, Float32)
    errorbounds(convert(AbstractMatrix{TAXB}, A), convert(AbstractArray{TAXB}, X), convert(AbstractArray{TAXB}, B))
end

# Eigensystems
## Notice that trecv works for quasi-triangular matrices and therefore the lower sub diagonal must be zeroed before calling the subroutine
function eigvecs(A::UpperTriangular{<:BlasFloat,<:StridedMatrix})
    LAPACK.trevc!('R', 'A', BlasInt[], triu!(A.data))
end
function eigvecs(A::UnitUpperTriangular{<:BlasFloat,<:StridedMatrix})
    for i = 1:size(A, 1)
        A.data[i,i] = 1
    end
    LAPACK.trevc!('R', 'A', BlasInt[], triu!(A.data))
end
function eigvecs(A::LowerTriangular{<:BlasFloat,<:StridedMatrix})
    LAPACK.trevc!('L', 'A', BlasInt[], copy(tril!(A.data)'))
end
function eigvecs(A::UnitLowerTriangular{<:BlasFloat,<:StridedMatrix})
    for i = 1:size(A, 1)
        A.data[i,i] = 1
    end
    LAPACK.trevc!('L', 'A', BlasInt[], copy(tril!(A.data)'))
end

####################
# Generic routines #
####################

for (t, unitt) in ((UpperTriangular, UnitUpperTriangular),
                   (LowerTriangular, UnitLowerTriangular))
    @eval begin
        (*)(A::$t, x::Number) = $t(A.data*x)

        function (*)(A::$unitt, x::Number)
            B = A.data*x
            for i = 1:size(A, 1)
                B[i,i] = x
            end
            $t(B)
        end

        (*)(x::Number, A::$t) = $t(x*A.data)

        function (*)(x::Number, A::$unitt)
            B = x*A.data
            for i = 1:size(A, 1)
                B[i,i] = x
            end
            $t(B)
        end

        (/)(A::$t, x::Number) = $t(A.data/x)

        function (/)(A::$unitt, x::Number)
            B = A.data/x
            invx = inv(x)
            for i = 1:size(A, 1)
                B[i,i] = invx
            end
            $t(B)
        end

        (\)(x::Number, A::$t) = $t(x\A.data)

        function (\)(x::Number, A::$unitt)
            B = x\A.data
            invx = inv(x)
            for i = 1:size(A, 1)
                B[i,i] = invx
            end
            $t(B)
        end
    end
end

## Generic triangular multiplication
function lmul!(A::UpperTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = A.data[i,i]*B[i,j]
            for k = i + 1:m
                Bij += A.data[i,k]*B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function lmul!(A::UnitUpperTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = B[i,j]
            for k = i + 1:m
                Bij += A.data[i,k]*B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function lmul!(A::LowerTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = A.data[i,i]*B[i,j]
            for k = 1:i - 1
                Bij += A.data[i,k]*B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end
function lmul!(A::UnitLowerTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = B[i,j]
            for k = 1:i - 1
                Bij += A.data[i,k]*B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function lmul!(adjA::Adjoint{<:Any,<:UpperTriangular}, B::StridedVecOrMat)
    A = adjA.parent
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = A.data[i,i]'B[i,j]
            for k = 1:i - 1
                Bij += A.data[k,i]'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function lmul!(adjA::Adjoint{<:Any,<:UnitUpperTriangular}, B::StridedVecOrMat)
    A = adjA.parent
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = B[i,j]
            for k = 1:i - 1
                Bij += A.data[k,i]'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function lmul!(adjA::Adjoint{<:Any,<:LowerTriangular}, B::StridedVecOrMat)
    A = adjA.parent
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = A.data[i,i]'B[i,j]
            for k = i + 1:m
                Bij += A.data[k,i]'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end
function lmul!(adjA::Adjoint{<:Any,<:UnitLowerTriangular}, B::StridedVecOrMat)
    A = adjA.parent
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = B[i,j]
            for k = i + 1:m
                Bij += A.data[k,i]'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function lmul!(transA::Transpose{<:Any,<:UpperTriangular}, B::StridedVecOrMat)
    A = transA.parent
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = transpose(A.data[i,i]) * B[i,j]
            for k = 1:i - 1
                Bij += transpose(A.data[k,i]) * B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end
function lmul!(transA::Transpose{<:Any,<:UnitUpperTriangular}, B::StridedVecOrMat)
    A = transA.parent
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = B[i,j]
            for k = 1:i - 1
                Bij += transpose(A.data[k,i]) * B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function lmul!(transA::Transpose{<:Any,<:LowerTriangular}, B::StridedVecOrMat)
    A = transA.parent
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = transpose(A.data[i,i]) * B[i,j]
            for k = i + 1:m
                Bij += transpose(A.data[k,i]) * B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end
function lmul!(transA::Transpose{<:Any,<:UnitLowerTriangular}, B::StridedVecOrMat)
    A = transA.parent
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = B[i,j]
            for k = i + 1:m
                Bij += transpose(A.data[k,i]) * B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function rmul!(A::StridedMatrix, B::UpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]*B[j,j]
            for k = 1:j - 1
                Aij += A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end
function rmul!(A::StridedMatrix, B::UnitUpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = 1:j - 1
                Aij += A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end

function rmul!(A::StridedMatrix, B::LowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]*B[j,j]
            for k = j + 1:n
                Aij += A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end
function rmul!(A::StridedMatrix, B::UnitLowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = j + 1:n
                Aij += A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end

function rmul!(A::StridedMatrix, adjB::Adjoint{<:Any,<:UpperTriangular})
    B = adjB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]*B.data[j,j]'
            for k = j + 1:n
                Aij += A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function rmul!(A::StridedMatrix, adjB::Adjoint{<:Any,<:UnitUpperTriangular})
    B = adjB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = j + 1:n
                Aij += A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function rmul!(A::StridedMatrix, adjB::Adjoint{<:Any,<:LowerTriangular})
    B = adjB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]*B.data[j,j]'
            for k = 1:j - 1
                Aij += A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function rmul!(A::StridedMatrix, adjB::Adjoint{<:Any,<:UnitLowerTriangular})
    B = adjB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = 1:j - 1
                Aij += A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function rmul!(A::StridedMatrix, transB::Transpose{<:Any,<:UpperTriangular})
    B = transB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j] * transpose(B.data[j,j])
            for k = j + 1:n
                Aij += A[i,k] * transpose(B.data[j,k])
            end
            A[i,j] = Aij
        end
    end
    A
end
function rmul!(A::StridedMatrix, transB::Transpose{<:Any,<:UnitUpperTriangular})
    B = transB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = j + 1:n
                Aij += A[i,k] * transpose(B.data[j,k])
            end
            A[i,j] = Aij
        end
    end
    A
end

function rmul!(A::StridedMatrix, transB::Transpose{<:Any,<:LowerTriangular})
    B = transB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j] * transpose(B.data[j,j])
            for k = 1:j - 1
                Aij += A[i,k] * transpose(B.data[j,k])
            end
            A[i,j] = Aij
        end
    end
    A
end

function rmul!(A::StridedMatrix, transB::Transpose{<:Any,<:UnitLowerTriangular})
    B = transB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = 1:j - 1
                Aij += A[i,k] * transpose(B.data[j,k])
            end
            A[i,j] = Aij
        end
    end
    A
end

#Generic solver using naive substitution
# manually hoisting x[j] significantly improves performance as of Dec 2015
# manually eliding bounds checking significantly improves performance as of Dec 2015
# directly indexing A.data rather than A significantly improves performance as of Dec 2015
# replacing repeated references to A.data with [Adata = A.data and references to Adata]
# does not significantly impact performance as of Dec 2015
# replacing repeated references to A.data[j,j] with [Ajj = A.data[j,j] and references to Ajj]
# does not significantly impact performance as of Dec 2015
function naivesub!(A::UpperTriangular, b::AbstractVector, x::AbstractVector = b)
    require_one_based_indexing(A, b, x)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("second dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        iszero(A.data[j,j]) && throw(SingularException(j))
        xj = x[j] = A.data[j,j] \ b[j]
        for i in j-1:-1:1 # counterintuitively 1:j-1 performs slightly better
            b[i] -= A.data[i,j] * xj
        end
    end
    x
end
function naivesub!(A::UnitUpperTriangular, b::AbstractVector, x::AbstractVector = b)
    require_one_based_indexing(A, b, x)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("second dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        xj = x[j] = b[j]
        for i in j-1:-1:1 # counterintuitively 1:j-1 performs slightly better
            b[i] -= A.data[i,j] * xj
        end
    end
    x
end
function naivesub!(A::LowerTriangular, b::AbstractVector, x::AbstractVector = b)
    require_one_based_indexing(A, b, x)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("second dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        iszero(A.data[j,j]) && throw(SingularException(j))
        xj = x[j] = A.data[j,j] \ b[j]
        for i in j+1:n
            b[i] -= A.data[i,j] * xj
        end
    end
    x
end
function naivesub!(A::UnitLowerTriangular, b::AbstractVector, x::AbstractVector = b)
    require_one_based_indexing(A, b, x)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("second dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        xj = x[j] = b[j]
        for i in j+1:n
            b[i] -= A.data[i,j] * xj
        end
    end
    x
end
# in the following transpose and conjugate transpose naive substitution variants,
# accumulating in z rather than b[j] significantly improves performance as of Dec 2015
function ldiv!(transA::Transpose{<:Any,<:LowerTriangular}, b::AbstractVector, x::AbstractVector)
    require_one_based_indexing(transA, b, x)
    A = transA.parent
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        z = b[j]
        for i in n:-1:j+1
            z -= A.data[i,j] * x[i]
        end
        iszero(A.data[j,j]) && throw(SingularException(j))
        x[j] = A.data[j,j] \ z
    end
    x
end
ldiv!(transA::Transpose{<:Any,<:LowerTriangular}, b::AbstractVector) = ldiv!(transA, b, b)

function ldiv!(transA::Transpose{<:Any,<:UnitLowerTriangular}, b::AbstractVector, x::AbstractVector)
    require_one_based_indexing(transA, b, x)
    A = transA.parent
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        z = b[j]
        for i in n:-1:j+1
            z -= A.data[i,j] * x[i]
        end
        x[j] = z
    end
    x
end
ldiv!(transA::Transpose{<:Any,<:UnitLowerTriangular}, b::AbstractVector) = ldiv!(transA, b, b)

function ldiv!(transA::Transpose{<:Any,<:UpperTriangular}, b::AbstractVector, x::AbstractVector)
    require_one_based_indexing(transA, b, x)
    A = transA.parent
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        z = b[j]
        for i in 1:j-1
            z -= A.data[i,j] * x[i]
        end
        iszero(A.data[j,j]) && throw(SingularException(j))
        x[j] = A.data[j,j] \ z
    end
    x
end
ldiv!(transA::Transpose{<:Any,<:UpperTriangular}, b::AbstractVector) = ldiv!(transA, b, b)

function ldiv!(transA::Transpose{<:Any,<:UnitUpperTriangular}, b::AbstractVector, x::AbstractVector)
    require_one_based_indexing(transA, b, x)
    A = transA.parent
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        z = b[j]
        for i in 1:j-1
            z -= A.data[i,j] * x[i]
        end
        x[j] = z
    end
    x
end
ldiv!(transA::Transpose{<:Any,<:UnitUpperTriangular}, b::AbstractVector) = ldiv!(transA, b, b)

function ldiv!(adjA::Adjoint{<:Any,<:LowerTriangular}, b::AbstractVector, x::AbstractVector)
    require_one_based_indexing(adjA, b, x)
    A = adjA.parent
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        z = b[j]
        for i in n:-1:j+1
            z -= A.data[i,j]' * x[i]
        end
        iszero(A.data[j,j]) && throw(SingularException(j))
        x[j] = A.data[j,j]' \ z
    end
    x
end
ldiv!(adjA::Adjoint{<:Any,<:LowerTriangular}, b::AbstractVector) = ldiv!(adjA, b, b)

function ldiv!(adjA::Adjoint{<:Any,<:UnitLowerTriangular}, b::AbstractVector, x::AbstractVector)
    require_one_based_indexing(adjA, b, x)
    A = adjA.parent
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        z = b[j]
        for i in n:-1:j+1
            z -= A.data[i,j]' * x[i]
        end
        x[j] = z
    end
    x
end
ldiv!(adjA::Adjoint{<:Any,<:UnitLowerTriangular}, b::AbstractVector) = ldiv!(adjA, b, b)

function ldiv!(adjA::Adjoint{<:Any,<:UpperTriangular}, b::AbstractVector, x::AbstractVector)
    require_one_based_indexing(adjA, b, x)
    A = adjA.parent
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        z = b[j]
        for i in 1:j-1
            z -= A.data[i,j]' * x[i]
        end
        iszero(A.data[j,j]) && throw(SingularException(j))
        x[j] = A.data[j,j]' \ z
    end
    x
end
ldiv!(adjA::Adjoint{<:Any,<:UpperTriangular}, b::AbstractVector) = ldiv!(adjA, b, b)

function ldiv!(adjA::Adjoint{<:Any,<:UnitUpperTriangular}, b::AbstractVector, x::AbstractVector)
    require_one_based_indexing(adjA, b, x)
    A = adjA.parent
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        z = b[j]
        for i in 1:j-1
            z -= A.data[i,j]' * x[i]
        end
        x[j] = z
    end
    x
end
ldiv!(adjA::Adjoint{<:Any,<:UnitUpperTriangular}, b::AbstractVector) = ldiv!(adjA, b, b)

function rdiv!(A::StridedMatrix, B::UpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij/B[j,j]
        end
    end
    A
end
function rdiv!(A::StridedMatrix, B::UnitUpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end

function rdiv!(A::StridedMatrix, B::LowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij/B[j,j]
        end
    end
    A
end
function rdiv!(A::StridedMatrix, B::UnitLowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end

function rdiv!(A::StridedMatrix, adjB::Adjoint{<:Any,<:UpperTriangular})
    B = adjB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij/B.data[j,j]'
        end
    end
    A
end
function rdiv!(A::StridedMatrix, adjB::Adjoint{<:Any,<:UnitUpperTriangular})
    B = adjB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function rdiv!(A::StridedMatrix, adjB::Adjoint{<:Any,<:LowerTriangular})
    B = adjB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij/B.data[j,j]'
        end
    end
    A
end
function rdiv!(A::StridedMatrix, adjB::Adjoint{<:Any,<:UnitLowerTriangular})
    B = adjB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function rdiv!(A::StridedMatrix, transB::Transpose{<:Any,<:UpperTriangular})
    B = transB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k] * transpose(B.data[j,k])
            end
            A[i,j] = Aij / transpose(B.data[j,j])
        end
    end
    A
end
function rdiv!(A::StridedMatrix, transB::Transpose{<:Any,<:UnitUpperTriangular})
    B = transB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k] * transpose(B.data[j,k])
            end
            A[i,j] = Aij
        end
    end
    A
end

function rdiv!(A::StridedMatrix, transB::Transpose{<:Any,<:LowerTriangular})
    B = transB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k] * transpose(B.data[j,k])
            end
            A[i,j] = Aij / transpose(B.data[j,j])
        end
    end
    A
end
function rdiv!(A::StridedMatrix, transB::Transpose{<:Any,<:UnitLowerTriangular})
    B = transB.parent
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k] * transpose(B.data[j,k])
            end
            A[i,j] = Aij
        end
    end
    A
end

function lmul!(adjA::Adjoint{<:Any,<:Union{LowerTriangular,UnitLowerTriangular}}, B::UpperTriangular)
    return UpperTriangular(lmul!(adjA, triu!(B.data)))
end
function lmul!(adjA::Adjoint{<:Any,<:Union{UpperTriangular,UnitUpperTriangular}}, B::LowerTriangular)
    return LowerTriangular(lmul!(adjA, tril!(B.data)))
end
function lmul!(transA::Transpose{<:Any,<:Union{LowerTriangular,UnitLowerTriangular}}, B::UpperTriangular)
    return UpperTriangular(lmul!(transA, triu!(B.data)))
end
function lmul!(transA::Transpose{<:Any,<:Union{UpperTriangular,UnitUpperTriangular}}, B::LowerTriangular)
    return LowerTriangular(lmul!(transA, tril!(B.data)))
end
function ldiv!(adjA::Adjoint{<:Any,<:Union{LowerTriangular,UnitLowerTriangular}}, B::UpperTriangular)
    return UpperTriangular(ldiv!(adjA, triu!(B.data)))
end
function ldiv!(adjA::Adjoint{<:Any,<:Union{UpperTriangular,UnitUpperTriangular}}, B::LowerTriangular)
    return LowerTriangular(ldiv!(adjA, tril!(B.data)))
end
function ldiv!(transA::Transpose{<:Any,<:Union{LowerTriangular,UnitLowerTriangular}}, B::UpperTriangular)
    return UpperTriangular(ldiv!(transA, triu!(B.data)))
end
function ldiv!(transA::Transpose{<:Any,<:Union{UpperTriangular,UnitUpperTriangular}}, B::LowerTriangular)
    return LowerTriangular(ldiv!(transA, tril!(B.data)))
end

function rdiv!(A::UpperTriangular, B::Union{UpperTriangular,UnitUpperTriangular})
    return UpperTriangular(rdiv!(triu!(A.data), B))
end
function rdiv!(A::LowerTriangular, B::Union{LowerTriangular,UnitLowerTriangular})
    return LowerTriangular(rdiv!(tril!(A.data), B))
end

function rmul!(A::UpperTriangular, adjB::Adjoint{<:Any,<:Union{LowerTriangular,UnitLowerTriangular}})
    return UpperTriangular(rmul!(triu!(A.data), adjB))
end
function rmul!(A::LowerTriangular, adjB::Adjoint{<:Any,<:Union{UpperTriangular,UnitUpperTriangular}})
    return LowerTriangular(rmul!(tril!(A.data), adjB))
end
function rmul!(A::UpperTriangular, transB::Transpose{<:Any,<:Union{LowerTriangular,UnitLowerTriangular}})
    return UpperTriangular(rmul!(triu!(A.data), transB))
end
function rmul!(A::LowerTriangular, transB::Transpose{<:Any,<:Union{UpperTriangular,UnitUpperTriangular}})
    return LowerTriangular(rmul!(tril!(A.data), transB))
end
function rdiv!(A::UpperTriangular, adjB::Adjoint{<:Any,<:Union{LowerTriangular,UnitLowerTriangular}})
    return UpperTriangular(rdiv!(triu!(A.data), adjB))
end
function rdiv!(A::LowerTriangular, adjB::Adjoint{<:Any,<:Union{UpperTriangular,UnitUpperTriangular}})
    return LowerTriangular(rdiv!(tril!(A.data), adjB))
end
function rdiv!(A::UpperTriangular, transB::Transpose{<:Any,<:Union{LowerTriangular,UnitLowerTriangular}})
    return UpperTriangular(rdiv!(triu!(A.data), transB))
end
function rdiv!(A::LowerTriangular, transB::Transpose{<:Any,<:Union{UpperTriangular,UnitUpperTriangular}})
    return LowerTriangular(rdiv!(tril!(A.data), transB))
end

# Promotion
## Promotion methods in matmul don't apply to triangular multiplication since
## it is inplace. Hence we have to make very similar definitions, but without
## allocation of a result array. For multiplication and unit diagonal division
## the element type doesn't have to be stable under division whereas that is
## necessary in the general triangular solve problem.

## Some Triangular-Triangular cases. We might want to write tailored methods
## for these cases, but I'm not sure it is worth it.

for (f, f2!) in ((:*, :lmul!), (:\, :ldiv!))
    @eval begin
        function ($f)(A::LowerTriangular, B::LowerTriangular)
            TAB = typeof(($f)(zero(eltype(A)), zero(eltype(B))) +
                         ($f)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            return LowerTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end

        function $(f)(A::UnitLowerTriangular, B::LowerTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            return LowerTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f)(A::UpperTriangular, B::UpperTriangular)
            TAB = typeof(($f)(zero(eltype(A)), zero(eltype(B))) +
                         ($f)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            return UpperTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f)(A::UnitUpperTriangular, B::UpperTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            return UpperTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end
    end
end

for (ipop, op, xformtype, xformop) in (
        (:lmul!, :*, :Adjoint, :adjoint),
        (:lmul!, :*, :Transpose, :transpose),
        (:ldiv!, :\, :Adjoint, :adjoint),
        (:ldiv!, :\, :Transpose, :transpose))
    @eval begin
        function ($op)(xformA::($xformtype){<:Any,<:UpperTriangular}, B::LowerTriangular)
            A = xformA.parent
            TAB = typeof(($op)($xformop(zero(eltype(A))), zero(eltype(B))) +
                         ($op)($xformop(zero(eltype(A))), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            return LowerTriangular(($ipop)($xformop(convert(AbstractMatrix{TAB}, A)), BB))
        end

        function ($op)(xformA::($xformtype){<:Any,<:UnitUpperTriangular}, B::LowerTriangular)
            A = xformA.parent
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            return LowerTriangular($ipop($xformop(convert(AbstractMatrix{TAB}, A)), BB))
        end

        function ($op)(xformA::($xformtype){<:Any,<:LowerTriangular}, B::UpperTriangular)
            A = xformA.parent
            TAB = typeof(($op)($xformop(zero(eltype(A))), zero(eltype(B))) +
                         ($op)($xformop(zero(eltype(A))), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            return UpperTriangular($ipop($xformop(convert(AbstractMatrix{TAB}, A)), BB))
        end

        function ($op)(xformA::($xformtype){<:Any,<:UnitLowerTriangular}, B::UpperTriangular)
            A = xformA.parent
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            return UpperTriangular($ipop($xformop(convert(AbstractMatrix{TAB}, A)), BB))
        end
    end
end

function (/)(A::LowerTriangular, B::LowerTriangular)
    TAB = typeof((/)(zero(eltype(A)), zero(eltype(B))) +
                 (/)(zero(eltype(A)), zero(eltype(B))))
    AA = similar(A, TAB, size(A))
    copyto!(AA, A)
    return LowerTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::LowerTriangular, B::UnitLowerTriangular)
    TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                 (*)(zero(eltype(A)), zero(eltype(B))))
    AA = similar(A, TAB, size(A))
    copyto!(AA, A)
    return LowerTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::UpperTriangular, B::UpperTriangular)
    TAB = typeof((/)(zero(eltype(A)), zero(eltype(B))) +
                 (/)(zero(eltype(A)), zero(eltype(B))))
    AA = similar(A, TAB, size(A))
    copyto!(AA, A)
    return UpperTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::UpperTriangular, B::UnitUpperTriangular)
    TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                 (*)(zero(eltype(A)), zero(eltype(B))))
    AA = similar(A, TAB, size(A))
    copyto!(AA, A)
    return UpperTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end

for (ipop, op, xformtype, xformop) in (
        (:rmul!, :*, :Adjoint, :adjoint),
        (:rmul!, :*, :Transpose, :transpose),
        (:rdiv!, :/, :Adjoint, :adjoint),
        (:rdiv!, :/, :Transpose, :transpose))
    @eval begin
        function ($op)(A::LowerTriangular, xformB::($xformtype){<:Any,<:UpperTriangular})
            B = xformB.parent
            TAB = typeof(($op)(zero(eltype(A)), $xformop(zero(eltype(B)))) +
                         ($op)(zero(eltype(A)), $xformop(zero(eltype(B)))))
            AA = similar(A, TAB, size(A))
            copyto!(AA, A)
            return LowerTriangular($ipop(AA, $xformop(convert(AbstractMatrix{TAB}, B))))
        end

        function ($op)(A::LowerTriangular, xformB::($xformtype){<:Any,<:UnitUpperTriangular})
            B = xformB.parent
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            AA = similar(A, TAB, size(A))
            copyto!(AA, A)
            return LowerTriangular($ipop(AA, $xformop(convert(AbstractMatrix{TAB}, B))))
        end

        function ($op)(A::UpperTriangular, xformB::($xformtype){<:Any,<:LowerTriangular})
            B = xformB.parent
            TAB = typeof(($op)(zero(eltype(A)), $xformop(zero(eltype(B)))) +
                         ($op)(zero(eltype(A)), $xformop(zero(eltype(B)))))
            AA = similar(A, TAB, size(A))
            copyto!(AA, A)
            return UpperTriangular($ipop(AA, $xformop(convert(AbstractMatrix{TAB}, B))))
        end

        function ($op)(A::UpperTriangular, xformB::($xformtype){<:Any,<:UnitLowerTriangular})
            B = xformB.parent
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            AA = similar(A, TAB, size(A))
            copyto!(AA, A)
            return UpperTriangular($ipop(AA, $xformop(convert(AbstractMatrix{TAB}, B))))
        end
    end
end

## The general promotion methods
function *(A::AbstractTriangular, B::AbstractTriangular)
    TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
    BB = similar(B, TAB, size(B))
    copyto!(BB, B)
    lmul!(convert(AbstractArray{TAB}, A), BB)
end
function *(adjA::Adjoint{<:Any,<:AbstractTriangular}, B::AbstractTriangular)
    A = adjA.parent
    TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
    BB = similar(B, TAB, size(B))
    copyto!(BB, B)
    lmul!(adjoint(convert(AbstractArray{TAB}, A)), BB)
end
function *(transA::Transpose{<:Any,<:AbstractTriangular}, B::AbstractTriangular)
    A = transA.parent
    TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
    BB = similar(B, TAB, size(B))
    copyto!(BB, B)
    lmul!(transpose(convert(AbstractArray{TAB}, A)), BB)
end

function *(A::AbstractTriangular, adjB::Adjoint{<:Any,<:AbstractTriangular})
    B = adjB.parent
    TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
    AA = similar(A, TAB, size(A))
    copyto!(AA, A)
    rmul!(AA, adjoint(convert(AbstractArray{TAB}, B)))
end
function *(A::AbstractTriangular, transB::Transpose{<:Any,<:AbstractTriangular})
    B = transB.parent
    TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
    AA = similar(A, TAB, size(A))
    copyto!(AA, A)
    rmul!(AA, transpose(convert(AbstractArray{TAB}, B)))
end

for mat in (:AbstractVector, :AbstractMatrix)
    ### Multiplication with triangle to the left and hence rhs cannot be transposed.
    @eval begin
        function *(A::AbstractTriangular, B::$mat)
            require_one_based_indexing(B)
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            lmul!(convert(AbstractArray{TAB}, A), BB)
        end
        function *(adjA::Adjoint{<:Any,<:AbstractTriangular}, B::$mat)
            require_one_based_indexing(B)
            A = adjA.parent
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            lmul!(adjoint(convert(AbstractArray{TAB}, A)), BB)
        end
        function *(transA::Transpose{<:Any,<:AbstractTriangular}, B::$mat)
            require_one_based_indexing(B)
            A = transA.parent
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            lmul!(transpose(convert(AbstractArray{TAB}, A)), BB)
        end
    end
    ### Left division with triangle to the left hence rhs cannot be transposed. No quotients.
    @eval begin
        function \(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::$mat)
            require_one_based_indexing(B)
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            ldiv!(convert(AbstractArray{TAB}, A), BB)
        end
        function \(adjA::Adjoint{<:Any,<:Union{UnitUpperTriangular,UnitLowerTriangular}}, B::$mat)
            require_one_based_indexing(B)
            A = adjA.parent
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            ldiv!(adjoint(convert(AbstractArray{TAB}, A)), BB)
        end
        function \(transA::Transpose{<:Any,<:Union{UnitUpperTriangular,UnitLowerTriangular}}, B::$mat)
            require_one_based_indexing(B)
            A = transA.parent
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            ldiv!(transpose(convert(AbstractArray{TAB}, A)), BB)
        end
    end
    ### Left division with triangle to the left hence rhs cannot be transposed. Quotients.
    @eval begin
        function \(A::Union{UpperTriangular,LowerTriangular}, B::$mat)
            require_one_based_indexing(B)
            TAB = typeof((zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))/one(eltype(A)))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            ldiv!(convert(AbstractArray{TAB}, A), BB)
        end
        function \(adjA::Adjoint{<:Any,<:Union{UpperTriangular,LowerTriangular}}, B::$mat)
            require_one_based_indexing(B)
            A = adjA.parent
            TAB = typeof((zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))/one(eltype(A)))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            ldiv!(adjoint(convert(AbstractArray{TAB}, A)), BB)
        end
        function \(transA::Transpose{<:Any,<:Union{UpperTriangular,LowerTriangular}}, B::$mat)
            require_one_based_indexing(B)
            A = transA.parent
            TAB = typeof((zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))/one(eltype(A)))
            BB = similar(B, TAB, size(B))
            copyto!(BB, B)
            ldiv!(transpose(convert(AbstractArray{TAB}, A)), BB)
        end
    end
    ### Right division with triangle to the right hence lhs cannot be transposed. No quotients.
    @eval begin
        function /(A::$mat, B::Union{UnitUpperTriangular, UnitLowerTriangular})
            require_one_based_indexing(A)
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            AA = similar(A, TAB, size(A))
            copyto!(AA, A)
            rdiv!(AA, convert(AbstractArray{TAB}, B))
        end
        function /(A::$mat, adjB::Adjoint{<:Any,<:Union{UnitUpperTriangular, UnitLowerTriangular}})
            require_one_based_indexing(A)
            B = adjB.parent
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            AA = similar(A, TAB, size(A))
            copyto!(AA, A)
            rdiv!(AA, adjoint(convert(AbstractArray{TAB}, B)))
        end
        function /(A::$mat, transB::Transpose{<:Any,<:Union{UnitUpperTriangular, UnitLowerTriangular}})
            require_one_based_indexing(A)
            B = transB.parent
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            AA = similar(A, TAB, size(A))
            copyto!(AA, A)
            rdiv!(AA, transpose(convert(AbstractArray{TAB}, B)))
        end
    end
    ### Right division with triangle to the right hence lhs cannot be transposed. Quotients.
    @eval begin
        function /(A::$mat, B::Union{UpperTriangular,LowerTriangular})
            require_one_based_indexing(A)
            TAB = typeof((zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))/one(eltype(A)))
            AA = similar(A, TAB, size(A))
            copyto!(AA, A)
            rdiv!(AA, convert(AbstractArray{TAB}, B))
        end
        function /(A::$mat, adjB::Adjoint{<:Any,<:Union{UpperTriangular,LowerTriangular}})
            require_one_based_indexing(A)
            B = adjB.parent
            TAB = typeof((zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))/one(eltype(A)))
            AA = similar(A, TAB, size(A))
            copyto!(AA, A)
            rdiv!(AA, adjoint(convert(AbstractArray{TAB}, B)))
        end
        function /(A::$mat, transB::Transpose{<:Any,<:Union{UpperTriangular,LowerTriangular}})
            require_one_based_indexing(A)
            B = transB.parent
            TAB = typeof((zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))/one(eltype(A)))
            AA = similar(A, TAB, size(A))
            copyto!(AA, A)
            rdiv!(AA, transpose(convert(AbstractArray{TAB}, B)))
        end
    end
end
### Multiplication with triangle to the right and hence lhs cannot be transposed.
# Only for AbstractMatrix, hence outside the above loop.
function *(A::AbstractMatrix, B::AbstractTriangular)
    require_one_based_indexing(A)
    TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
    AA = similar(A, TAB, size(A))
    copyto!(AA, A)
    rmul!(AA, convert(AbstractArray{TAB}, B))
end
function *(A::AbstractMatrix, adjB::Adjoint{<:Any,<:AbstractTriangular})
    require_one_based_indexing(A)
    B = adjB.parent
    TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
    AA = similar(A, TAB, size(A))
    copyto!(AA, A)
    rmul!(AA, adjoint(convert(AbstractArray{TAB}, B)))
end
function *(A::AbstractMatrix, transB::Transpose{<:Any,<:AbstractTriangular})
    require_one_based_indexing(A)
    B = transB.parent
    TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
    AA = similar(A, TAB, size(A))
    copyto!(AA, A)
    rmul!(AA, transpose(convert(AbstractArray{TAB}, B)))
end
# ambiguity resolution with definitions in linalg/rowvector.jl
*(v::AdjointAbsVec, A::AbstractTriangular) = adjoint(adjoint(A) * v.parent)
*(v::TransposeAbsVec, A::AbstractTriangular) = transpose(transpose(A) * v.parent)
*(v::AdjointAbsVec, A::Adjoint{<:Any,<:AbstractTriangular}) = adjoint(A.parent * v.parent)
*(v::TransposeAbsVec, A::Transpose{<:Any,<:AbstractTriangular}) = transpose(A.parent * v.parent)


# If these are not defined, they will fallback to the versions in matmul.jl
# and dispatch to generic_matmatmul! which is very costly to compile. The methods
# below might compute an unnecessary copy. Eliminating the copy requires adding
# all the promotion logic here once again. Since these methods are probably relatively
# rare, we chose not to bother for now.
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::AbstractTriangular) = copy(A) * B
*(A::Transpose{<:Any,<:AbstractMatrix}, B::AbstractTriangular) = copy(A) * B
*(A::AbstractTriangular, B::Adjoint{<:Any,<:AbstractMatrix}) = A * copy(B)
*(A::AbstractTriangular, B::Transpose{<:Any,<:AbstractMatrix}) = A * copy(B)
*(A::Adjoint{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:AbstractTriangular}) = A * copy(B)
*(A::Adjoint{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:AbstractMatrix}) = A * copy(B)
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:AbstractTriangular}) = copy(A) * B
*(A::Transpose{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:AbstractTriangular}) = A * copy(B)
*(A::Transpose{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:AbstractMatrix}) = A * copy(B)
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:AbstractTriangular}) = copy(A) * B

# Complex matrix power for upper triangular factor, see:
#   Higham and Lin, "A Schur-Padé algorithm for fractional powers of a Matrix",
#     SIAM J. Matrix Anal. & Appl., 32 (3), (2011) 1056–1078.
#   Higham and Lin, "An improved Schur-Padé algorithm for fractional powers of
#     a matrix and their Fréchet derivatives", SIAM. J. Matrix Anal. & Appl.,
#     34(3), (2013) 1341–1360.
function powm!(A0::UpperTriangular{<:BlasFloat}, p::Real)
    if abs(p) >= 1
        throw(ArgumentError("p must be a real number in (-1,1), got $p"))
    end

    normA0 = opnorm(A0, 1)
    rmul!(A0, 1/normA0)

    theta = [1.53e-5, 2.25e-3, 1.92e-2, 6.08e-2, 1.25e-1, 2.03e-1, 2.84e-1]
    n = checksquare(A0)

    A, m, s = invsquaring(A0, theta)
    A = I - A

    # Compute accurate diagonal of I - T
    sqrt_diag!(A0, A, s)
    for i = 1:n
        A[i, i] = -A[i, i]
    end
    # Compute the Padé approximant
    c = 0.5 * (p - m) / (2 * m - 1)
    triu!(A)
    S = c * A
    Stmp = similar(S)
    for j = m-1:-1:1
        j4 = 4 * j
        c = (-p - j) / (j4 + 2)
        for i = 1:n
            @inbounds S[i, i] = S[i, i] + 1
        end
        copyto!(Stmp, S)
        mul!(S, A, c)
        ldiv!(Stmp, S.data)

        c = (p - j) / (j4 - 2)
        for i = 1:n
            @inbounds S[i, i] = S[i, i] + 1
        end
        copyto!(Stmp, S)
        mul!(S, A, c)
        ldiv!(Stmp, S.data)
    end
    for i = 1:n
        S[i, i] = S[i, i] + 1
    end
    copyto!(Stmp, S)
    mul!(S, A, -p)
    ldiv!(Stmp, S.data)
    for i = 1:n
        @inbounds S[i, i] = S[i, i] + 1
    end

    blockpower!(A0, S, p/(2^s))
    for m = 1:s
        mul!(Stmp.data, S, S)
        copyto!(S, Stmp)
        blockpower!(A0, S, p/(2^(s-m)))
    end
    rmul!(S, normA0^p)
    return S
end
powm(A::LowerTriangular, p::Real) = copy(transpose(powm!(copy(transpose(A)), p::Real)))

# Complex matrix logarithm for the upper triangular factor, see:
#   Al-Mohy and Higham, "Improved inverse  scaling and squaring algorithms for
#     the matrix logarithm", SIAM J. Sci. Comput., 34(4), (2012), pp. C153–C169.
#   Al-Mohy, Higham and Relton, "Computing the Frechet derivative of the matrix
#     logarithm and estimating the condition number", SIAM J. Sci. Comput.,
#     35(4), (2013), C394–C410.
#
# Based on the code available at http://eprints.ma.man.ac.uk/1851/02/logm.zip,
# Copyright (c) 2011, Awad H. Al-Mohy and Nicholas J. Higham
# Julia version relicensed with permission from original authors
function log(A0::UpperTriangular{T}) where T<:BlasFloat
    maxsqrt = 100
    theta = [1.586970738772063e-005,
         2.313807884242979e-003,
         1.938179313533253e-002,
         6.209171588994762e-002,
         1.276404810806775e-001,
         2.060962623452836e-001,
         2.879093714241194e-001]
    tmax = size(theta, 1)
    n = size(A0, 1)
    A = copy(A0)
    p = 0
    m = 0

    # Compute repeated roots
    d = complex(diag(A))
    dm1 = d .- 1
    s = 0
    while norm(dm1, Inf) > theta[tmax] && s < maxsqrt
        d .= sqrt.(d)
        dm1 .= d .- 1
        s = s + 1
    end
    s0 = s
    for k = 1:min(s, maxsqrt)
        A = sqrt(A)
    end

    AmI = A - I
    d2 = sqrt(opnorm(AmI^2, 1))
    d3 = cbrt(opnorm(AmI^3, 1))
    alpha2 = max(d2, d3)
    foundm = false
    if alpha2 <= theta[2]
        m = alpha2 <= theta[1] ? 1 : 2
        foundm = true
    end

    while !foundm
        more = false
        if s > s0
            d3 = cbrt(opnorm(AmI^3, 1))
        end
        d4 = opnorm(AmI^4, 1)^(1/4)
        alpha3 = max(d3, d4)
        if alpha3 <= theta[tmax]
            local j
            for outer j = 3:tmax
                if alpha3 <= theta[j]
                    break
                end
            end
            if j <= 6
                m = j
                break
            elseif alpha3 / 2 <= theta[5] && p < 2
                more = true
                p = p + 1
           end
        end

        if !more
            d5 = opnorm(AmI^5, 1)^(1/5)
            alpha4 = max(d4, d5)
            eta = min(alpha3, alpha4)
            if eta <= theta[tmax]
                j = 0
                for outer j = 6:tmax
                    if eta <= theta[j]
                        m = j
                        break
                    end
                end
                break
            end
        end

        if s == maxsqrt
            m = tmax
            break
        end
        A = sqrt(A)
        AmI = A - I
        s = s + 1
    end

    # Compute accurate superdiagonal of T
    blockpower!(A, A0, 0.5^s)

    # Compute accurate diagonal of T
    for i = 1:n
        a = A0[i,i]
        if s == 0
            A[i,i] = a - 1
            continue
        end
        s0 = s
        if angle(a) >= pi / 2
            a = sqrt(a)
            s0 = s - 1
        end
        z0 = a - 1
        a = sqrt(a)
        r = 1 + a
        for j = 1:s0-1
            a = sqrt(a)
            r = r * (1 + a)
        end
        A[i,i] = z0 / r
    end

    # Get the Gauss-Legendre quadrature points and weights
    R = zeros(Float64, m, m)
    for i = 1:m - 1
        R[i,i+1] = i / sqrt((2 * i)^2 - 1)
        R[i+1,i] = R[i,i+1]
    end
    x,V = eigen(R)
    w = Vector{Float64}(undef, m)
    for i = 1:m
        x[i] = (x[i] + 1) / 2
        w[i] = V[1,i]^2
    end

    # Compute the Padé approximation
    Y = zeros(T, n, n)
    for k = 1:m
        Y = Y + w[k] * (A / (x[k] * A + I))
    end

    # Scale back
    lmul!(2.0^s, Y)

    # Compute accurate diagonal and superdiagonal of log(T)
    for k = 1:n-1
        Ak = A0[k,k]
        Akp1 = A0[k+1,k+1]
        logAk = log(Ak)
        logAkp1 = log(Akp1)
        Y[k,k] = logAk
        Y[k+1,k+1] = logAkp1
        if Ak == Akp1
            Y[k,k+1] = A0[k,k+1] / Ak
        elseif 2 * abs(Ak) < abs(Akp1) || 2 * abs(Akp1) < abs(Ak) || iszero(Akp1 + Ak)
            Y[k,k+1] = A0[k,k+1] * (logAkp1 - logAk) / (Akp1 - Ak)
        else
            z = (Akp1 - Ak)/(Akp1 + Ak)
            if abs(z) > 1
                Y[k,k+1] = A0[k,k+1] * (logAkp1 - logAk) / (Akp1 - Ak)
            else
                w = atanh(z) + im * pi * (unw(logAkp1-logAk) - unw(log1p(z)-log1p(-z)))
                Y[k,k+1] = 2 * A0[k,k+1] * w / (Akp1 - Ak)
            end
        end
    end

    return UpperTriangular(Y)
end
log(A::LowerTriangular) = copy(transpose(log(copy(transpose(A)))))

# Auxiliary functions for matrix logarithm and matrix power

# Compute accurate diagonal of A = A0^s - I
#   Al-Mohy, "A more accurate Briggs method for the logarithm",
#      Numer. Algorithms, 59, (2012), 393–402.
function sqrt_diag!(A0::UpperTriangular, A::UpperTriangular, s)
    n = checksquare(A0)
    @inbounds for i = 1:n
        a = complex(A0[i,i])
        if s == 0
            A[i,i] = a - 1
        else
            s0 = s
            if imag(a) >= 0 && real(a) <= 0 && a != 0
                a = sqrt(a)
                s0 = s - 1
            end
            z0 = a - 1
            a = sqrt(a)
            r = 1 + a
            for j = 1:s0-1
                a = sqrt(a)
                r = r * (1 + a)
            end
            A[i,i] = z0 / r
        end
    end
end

# Used only by powm at the moment
# Repeatedly compute the square roots of A so that in the end its
# eigenvalues are close enough to the positive real line
function invsquaring(A0::UpperTriangular, theta)
    require_one_based_indexing(theta)
    # assumes theta is in ascending order
    maxsqrt = 100
    tmax = size(theta, 1)
    n = checksquare(A0)
    A = complex(copy(A0))
    p = 0
    m = 0

    # Compute repeated roots
    d = complex(diag(A))
    dm1 = d .- 1
    s = 0
    while norm(dm1, Inf) > theta[tmax] && s < maxsqrt
        d .= sqrt.(d)
        dm1 .= d .- 1
        s = s + 1
    end
    s0 = s
    for k = 1:min(s, maxsqrt)
        A = sqrt(A)
    end

    AmI = A - I
    d2 = sqrt(opnorm(AmI^2, 1))
    d3 = cbrt(opnorm(AmI^3, 1))
    alpha2 = max(d2, d3)
    foundm = false
    if alpha2 <= theta[2]
        m = alpha2 <= theta[1] ? 1 : 2
        foundm = true
    end

    while !foundm
        more = false
        if s > s0
            d3 = cbrt(opnorm(AmI^3, 1))
        end
        d4 = opnorm(AmI^4, 1)^(1/4)
        alpha3 = max(d3, d4)
        if alpha3 <= theta[tmax]
            local j
            for outer j = 3:tmax
                if alpha3 <= theta[j]
                    break
                elseif alpha3 / 2 <= theta[5] && p < 2
                    more = true
                    p = p + 1
                end
            end
            if j <= 6
                m = j
                foundm = true
                break
            elseif alpha3 / 2 <= theta[5] && p < 2
                more = true
                p = p + 1
           end
        end

        if !more
            d5 = opnorm(AmI^5, 1)^(1/5)
            alpha4 = max(d4, d5)
            eta = min(alpha3, alpha4)
            if eta <= theta[tmax]
                j = 0
                for outer j = 6:tmax
                    if eta <= theta[j]
                        m = j
                        break
                    end
                    break
                end
            end
            if s == maxsqrt
                m = tmax
                break
            end
            A = sqrt(A)
            AmI = A - I
            s = s + 1
        end
    end

    # Compute accurate superdiagonal of T
    p = 1 / 2^s
    A = complex(A)
    blockpower!(A, A0, p)
    return A,m,s
end

# Compute accurate diagonal and superdiagonal of A = A0^p
function blockpower!(A::UpperTriangular, A0::UpperTriangular, p)
    n = checksquare(A0)
    @inbounds for k = 1:n-1
        Ak = complex(A0[k,k])
        Akp1 = complex(A0[k+1,k+1])

        Akp = Ak^p
        Akp1p = Akp1^p

        A[k,k] = Akp
        A[k+1,k+1] = Akp1p

        if Ak == Akp1
            A[k,k+1] = p * A0[k,k+1] * Ak^(p-1)
        elseif 2 * abs(Ak) < abs(Akp1) || 2 * abs(Akp1) < abs(Ak) || iszero(Akp1 + Ak)
            A[k,k+1] = A0[k,k+1] * (Akp1p - Akp) / (Akp1 - Ak)
        else
            logAk = log(Ak)
            logAkp1 = log(Akp1)
            z = (Akp1 - Ak)/(Akp1 + Ak)
            if abs(z) > 1
                A[k,k+1] = A0[k,k+1] * (Akp1p - Akp) / (Akp1 - Ak)
            else
                w = atanh(z) + im * pi * (unw(logAkp1-logAk) - unw(log1p(z)-log1p(-z)))
                dd = 2 * exp(p*(logAk+logAkp1)/2) * sinh(p*w) / (Akp1 - Ak);
                A[k,k+1] = A0[k,k+1] * dd
            end
        end
    end
end

# Unwinding number
unw(x::Real) = 0
unw(x::Number) = ceil((imag(x) - pi) / (2 * pi))

# End of auxiliary functions for matrix logarithm and matrix power

function sqrt(A::UpperTriangular)
    realmatrix = false
    if isreal(A)
        realmatrix = true
        for i = 1:checksquare(A)
            x = real(A[i,i])
            if x < zero(x)
                realmatrix = false
                break
            end
        end
    end
    # Writing an explicit if instead of using Val(realmatrix) below
    # makes the calls to sqrt(::UpperTriangular,::Val) type stable.
    if realmatrix
        return sqrt(A,Val(true))
    else
        return sqrt(A,Val(false))
    end
end
function sqrt(A::UpperTriangular{T},::Val{realmatrix}) where {T,realmatrix}
    B = A.data
    n = checksquare(B)
    t = realmatrix ? typeof(sqrt(zero(T))) : typeof(sqrt(complex(zero(T))))
    R = zeros(t, n, n)
    tt = typeof(zero(t)*zero(t))
    @inbounds for j = 1:n
        R[j,j] = realmatrix ? sqrt(B[j,j]) : sqrt(complex(B[j,j]))
        for i = j-1:-1:1
            r::tt = B[i,j]
            @simd for k = i+1:j-1
                r -= R[i,k]*R[k,j]
            end
            if !(iszero(r) || (iszero(R[i,i]) && iszero(R[j,j])))
                R[i,j] = sylvester(R[i,i],R[j,j],-r)
            end
        end
    end
    return UpperTriangular(R)
end
function sqrt(A::UnitUpperTriangular{T}) where T
    B = A.data
    n = checksquare(B)
    t = typeof(sqrt(zero(T)))
    R = Matrix{t}(I, n, n)
    tt = typeof(zero(t)*zero(t))
    half = inv(R[1,1]+R[1,1]) # for general, algebraic cases. PR#20214
    @inbounds for j = 1:n
        for i = j-1:-1:1
            r::tt = B[i,j]
            @simd for k = i+1:j-1
                r -= R[i,k]*R[k,j]
            end
            r==0 || (R[i,j] = half*r)
        end
    end
    return UnitUpperTriangular(R)
end
sqrt(A::LowerTriangular) = copy(transpose(sqrt(copy(transpose(A)))))
sqrt(A::UnitLowerTriangular) = copy(transpose(sqrt(copy(transpose(A)))))

# Generic eigensystems
eigvals(A::AbstractTriangular) = diag(A)
function eigvecs(A::AbstractTriangular{T}) where T
    TT = promote_type(T, Float32)
    if TT <: BlasFloat
        return eigvecs(convert(AbstractMatrix{TT}, A))
    else
        throw(ArgumentError("eigvecs type $(typeof(A)) not supported. Please submit a pull request."))
    end
end
det(A::UnitUpperTriangular{T}) where {T} = one(T)
det(A::UnitLowerTriangular{T}) where {T} = one(T)
logdet(A::UnitUpperTriangular{T}) where {T} = zero(T)
logdet(A::UnitLowerTriangular{T}) where {T} = zero(T)
logabsdet(A::UnitUpperTriangular{T}) where {T} = zero(T), one(T)
logabsdet(A::UnitLowerTriangular{T}) where {T} = zero(T), one(T)
det(A::UpperTriangular) = prod(diag(A.data))
det(A::LowerTriangular) = prod(diag(A.data))
function logabsdet(A::Union{UpperTriangular{T},LowerTriangular{T}}) where T
    sgn = one(T)
    abs_det = zero(real(T))
    @inbounds for i in 1:size(A,1)
        diag_i = A.data[i,i]
        sgn *= sign(diag_i)
        abs_det += log(abs(diag_i))
    end
    return abs_det, sgn
end

eigen(A::AbstractTriangular) = Eigen(eigvals(A), eigvecs(A))

# Generic singular systems
for func in (:svd, :svd!, :svdvals)
    @eval begin
        ($func)(A::AbstractTriangular; kwargs...) = ($func)(copyto!(similar(parent(A)), A); kwargs...)
    end
end

factorize(A::AbstractTriangular) = A

# disambiguation methods: *(AbstractTriangular, Adj/Trans of AbstractVector)
*(A::AbstractTriangular, B::Adjoint{<:Any,<:AbstractVector}) = adjoint(adjoint(B) * adjoint(A))
*(A::AbstractTriangular, B::Transpose{<:Any,<:AbstractVector}) = transpose(transpose(B) * transpose(A))
# disambiguation methods: *(Adj/Trans of AbstractTriangular, Trans/Ajd of AbstractTriangular)
*(A::Adjoint{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:AbstractTriangular}) = copy(A) * B
*(A::Transpose{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:AbstractTriangular}) = copy(A) * B
# disambiguation methods: *(Adj/Trans of AbstractTriangular, Adj/Trans of AbsVec or AbsMat)
*(A::Adjoint{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:AbstractVector}) = adjoint(adjoint(B) * adjoint(A))
*(A::Adjoint{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:AbstractMatrix}) = A * copy(B)
*(A::Adjoint{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:AbstractVector}) = transpose(transpose(B) * transpose(A))
*(A::Transpose{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:AbstractVector}) = transpose(transpose(B) * transpose(A))
*(A::Transpose{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:AbstractVector}) = adjoint(adjoint(B) * adjoint(A))
*(A::Transpose{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:AbstractMatrix}) = A * copy(B)
# disambiguation methods: *(Adj/Trans of AbsVec or AbsMat, Adj/Trans of AbstractTriangular)
*(A::Adjoint{<:Any,<:AbstractVector}, B::Transpose{<:Any,<:AbstractTriangular}) = adjoint(adjoint(B) * adjoint(A))
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:AbstractTriangular}) = copy(A) * B
*(A::Transpose{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:AbstractTriangular}) = transpose(transpose(B) * transpose(A))
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:AbstractTriangular}) = copy(A) * B

# disambiguation methods: /(Adjoint of AbsVec, <:AbstractTriangular)
/(u::AdjointAbsVec, A::Union{LowerTriangular,UpperTriangular}) = adjoint(adjoint(A) \ u.parent)
/(u::AdjointAbsVec, A::Union{UnitLowerTriangular,UnitUpperTriangular}) = adjoint(adjoint(A) \ u.parent)
# disambiguation methods: /(Adjoint of AbsVec, Adj/Trans of <:AbstractTriangular)
/(u::AdjointAbsVec, A::Adjoint{<:Any,<:Union{LowerTriangular,UpperTriangular}}) = adjoint(A.parent \ u.parent)
/(u::AdjointAbsVec, A::Adjoint{<:Any,<:Union{UnitLowerTriangular,UnitUpperTriangular}}) = adjoint(A.parent \ u.parent)
/(u::AdjointAbsVec, A::Transpose{<:Any,<:Union{LowerTriangular,UpperTriangular}}) = adjoint(conj(A.parent) \ u.parent)
/(u::AdjointAbsVec, A::Transpose{<:Any,<:Union{UnitLowerTriangular,UnitUpperTriangular}}) = adjoint(conj(A.parent) \ u.parent)
# disambiguation methods: /(Transpose of AbsVec, <:AbstractTriangular)
/(u::TransposeAbsVec, A::Union{LowerTriangular,UpperTriangular}) = transpose(transpose(A) \ u.parent)
/(u::TransposeAbsVec, A::Union{UnitLowerTriangular,UnitUpperTriangular}) = transpose(transpose(A) \ u.parent)
# disambiguation methods: /(Transpose of AbsVec, Adj/Trans of <:AbstractTriangular)
/(u::TransposeAbsVec, A::Adjoint{<:Any,<:Union{LowerTriangular,UpperTriangular}}) = transpose(conj(A.parent) \ u.parent)
/(u::TransposeAbsVec, A::Adjoint{<:Any,<:Union{UnitLowerTriangular,UnitUpperTriangular}}) = transpose(conj(A.parent) \ u.parent)
/(u::TransposeAbsVec, A::Transpose{<:Any,<:Union{LowerTriangular,UpperTriangular}}) = transpose(A.parent \ u.parent)
/(u::TransposeAbsVec, A::Transpose{<:Any,<:Union{UnitLowerTriangular,UnitUpperTriangular}}) = transpose(A.parent \ u.parent)
