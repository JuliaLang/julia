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
        Matrix(A::$t{T}) where {T} = Matrix{T}(A)

        AbstractMatrix{T}(A::$t) where {T} = $t{T}(A)

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

similar(A::UpperTriangular{<:Any,<:Union{Adjoint{Ti}, Transpose{Ti}}}, ::Type{T}) where {T,Ti} =
    UpperTriangular(similar(parent(parent(A)), T))
similar(A::UnitUpperTriangular{<:Any,<:Union{Adjoint{Ti}, Transpose{Ti}}}, ::Type{T}) where {T,Ti} =
    UnitUpperTriangular(similar(parent(parent(A)), T))
similar(A::LowerTriangular{<:Any,<:Union{Adjoint{Ti}, Transpose{Ti}}}, ::Type{T}) where {T,Ti} =
    LowerTriangular(similar(parent(parent(A)), T))
similar(A::UnitLowerTriangular{<:Any,<:Union{Adjoint{Ti}, Transpose{Ti}}}, ::Type{T}) where {T,Ti} =
    UnitLowerTriangular(similar(parent(parent(A)), T))


"""
    LowerTriangular(A::AbstractMatrix)

Construct a `LowerTriangular` view of the matrix `A`.

# Examples
```jldoctest
julia> A = [1.0 2.0 3.0; 4.0 5.0 6.0; 7.0 8.0 9.0]
3×3 Matrix{Float64}:
 1.0  2.0  3.0
 4.0  5.0  6.0
 7.0  8.0  9.0

julia> LowerTriangular(A)
3×3 LowerTriangular{Float64, Matrix{Float64}}:
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
3×3 Matrix{Float64}:
 1.0  2.0  3.0
 4.0  5.0  6.0
 7.0  8.0  9.0

julia> UpperTriangular(A)
3×3 UpperTriangular{Float64, Matrix{Float64}}:
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
3×3 Matrix{Float64}:
 1.0  2.0  3.0
 4.0  5.0  6.0
 7.0  8.0  9.0

julia> UnitLowerTriangular(A)
3×3 UnitLowerTriangular{Float64, Matrix{Float64}}:
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
3×3 Matrix{Float64}:
 1.0  2.0  3.0
 4.0  5.0  6.0
 7.0  8.0  9.0

julia> UnitUpperTriangular(A)
3×3 UnitUpperTriangular{Float64, Matrix{Float64}}:
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
    return _istril(A, k)
end
function istriu(A::Union{UpperTriangular,UnitUpperTriangular}, k::Integer=0)
    k <= 0 && return true
    return _istriu(A, k)
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

adjoint(A::LowerTriangular) = UpperTriangular(adjoint(A.data))
adjoint(A::UpperTriangular) = LowerTriangular(adjoint(A.data))
adjoint(A::UnitLowerTriangular) = UnitUpperTriangular(adjoint(A.data))
adjoint(A::UnitUpperTriangular) = UnitLowerTriangular(adjoint(A.data))
transpose(A::LowerTriangular) = UpperTriangular(transpose(A.data))
transpose(A::UpperTriangular) = LowerTriangular(transpose(A.data))
transpose(A::UnitLowerTriangular) = UnitUpperTriangular(transpose(A.data))
transpose(A::UnitUpperTriangular) = UnitLowerTriangular(transpose(A.data))

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
for (Trig, UnitTrig) in Any[(UpperTriangular, UnitUpperTriangular),
                            (LowerTriangular, UnitLowerTriangular)]
    for (TB, TC) in Any[(Trig, Number),
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

# The three methods are neceesary to avoid ambiguities with definitions in matmul.jl
mul!(C::AbstractVector  , A::AbstractTriangular, B::AbstractVector)   = lmul!(A, copyto!(C, B))
mul!(C::AbstractMatrix  , A::AbstractTriangular, B::AbstractVecOrMat) = lmul!(A, copyto!(C, B))
mul!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat) = lmul!(A, copyto!(C, B))

@inline mul!(C::AbstractMatrix, A::AbstractTriangular, B::Adjoint{<:Any,<:AbstractVecOrMat}, alpha::Number, beta::Number) =
    mul!(C, A, copy(B), alpha, beta)
@inline mul!(C::AbstractMatrix, A::AbstractTriangular, B::Transpose{<:Any,<:AbstractVecOrMat}, alpha::Number, beta::Number) =
    mul!(C, A, copy(B), alpha, beta)
mul!(C::AbstractVector, A::AbstractTriangular{<:Any,<:Adjoint}, B::Transpose{<:Any,<:AbstractVecOrMat}) = throw(MethodError(mul!, (C, A, B)))
mul!(C::AbstractVector, A::AbstractTriangular{<:Any,<:Transpose}, B::Transpose{<:Any,<:AbstractVecOrMat}) = throw(MethodError(mul!, (C, A, B)))

# preserve triangular structure in in-place multiplication
for (cty, aty, bty) in ((:UpperTriangular, :UpperTriangular, :UpperTriangular),
                        (:UpperTriangular, :UpperTriangular, :UnitUpperTriangular),
                        (:UpperTriangular, :UnitUpperTriangular, :UpperTriangular),
                        (:UnitUpperTriangular, :UnitUpperTriangular, :UnitUpperTriangular),
                        (:LowerTriangular, :LowerTriangular, :LowerTriangular),
                        (:LowerTriangular, :LowerTriangular, :UnitLowerTriangular),
                        (:LowerTriangular, :UnitLowerTriangular, :LowerTriangular),
                        (:UnitLowerTriangular, :UnitLowerTriangular, :UnitLowerTriangular))
    @eval function mul!(C::$cty, A::$aty, B::$bty)
        lmul!(A, copyto!(parent(C), B))
        return C
    end

    @eval @inline function mul!(C::$cty, A::$aty, B::$bty, alpha::Number, beta::Number)
        if isone(alpha) && iszero(beta)
            return mul!(C, A, B)
        else
            return generic_matmatmul!(C, 'N', 'N', A, B, MulAddMul(alpha, beta))
        end
    end
end

# direct multiplication/division
for (t, uploc, isunitc) in ((:LowerTriangular, 'L', 'N'),
                            (:UnitLowerTriangular, 'L', 'U'),
                            (:UpperTriangular, 'U', 'N'),
                            (:UnitUpperTriangular, 'U', 'U'))
    @eval begin
        # Vector multiplication
        lmul!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasFloat} =
            BLAS.trmv!($uploc, 'N', $isunitc, A.data, b)

        # Matrix multiplication
        lmul!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat} =
            BLAS.trmm!('L', $uploc, 'N', $isunitc, one(T), A.data, B)
        rmul!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat} =
            BLAS.trmm!('R', $uploc, 'N', $isunitc, one(T), B.data, A)

        # Left division
        ldiv!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
            LAPACK.trtrs!($uploc, 'N', $isunitc, A.data, B)

        # Right division
        rdiv!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat} =
            BLAS.trsm!('R', $uploc, 'N', $isunitc, one(T), B.data, A)

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

# adjoint/transpose multiplication ('uploc' reversed)
for (t, uploc, isunitc) in ((:LowerTriangular, 'U', 'N'),
                            (:UnitLowerTriangular, 'U', 'U'),
                            (:UpperTriangular, 'L', 'N'),
                            (:UnitUpperTriangular, 'L', 'U'))
    @eval begin
        # Vector multiplication
        lmul!(A::$t{<:Any,<:Transpose{T,<:StridedMatrix}}, b::StridedVector{T}) where {T<:BlasFloat} =
            BLAS.trmv!($uploc, 'T', $isunitc, parent(parent(A)), b)
        lmul!(A::$t{<:Any,<:Adjoint{T,<:StridedMatrix}}, b::StridedVector{T}) where {T<:BlasReal} =
            BLAS.trmv!($uploc, 'T', $isunitc, parent(parent(A)), b)
        lmul!(A::$t{<:Any,<:Adjoint{T,<:StridedMatrix}}, b::StridedVector{T}) where {T<:BlasComplex} =
            BLAS.trmv!($uploc, 'C', $isunitc, parent(parent(A)), b)

        # Matrix multiplication
        lmul!(A::$t{<:Any,<:Transpose{T,<:StridedMatrix}}, B::StridedMatrix{T}) where {T<:BlasFloat} =
            BLAS.trmm!('L', $uploc, 'T', $isunitc, one(T), parent(parent(A)), B)
        lmul!(A::$t{<:Any,<:Adjoint{T,<:StridedMatrix}}, B::StridedMatrix{T}) where {T<:BlasComplex} =
            BLAS.trmm!('L', $uploc, 'C', $isunitc, one(T), parent(parent(A)), B)
        lmul!(A::$t{<:Any,<:Adjoint{T,<:StridedMatrix}}, B::StridedMatrix{T}) where {T<:BlasReal} =
            BLAS.trmm!('L', $uploc, 'T', $isunitc, one(T), parent(parent(A)), B)

        rmul!(A::StridedMatrix{T}, B::$t{<:Any,<:Transpose{T,<:StridedMatrix}}) where {T<:BlasFloat} =
            BLAS.trmm!('R', $uploc, 'T', $isunitc, one(T), parent(parent(B)), A)
        rmul!(A::StridedMatrix{T}, B::$t{<:Any,<:Adjoint{T,<:StridedMatrix}}) where {T<:BlasComplex} =
            BLAS.trmm!('R', $uploc, 'C', $isunitc, one(T), parent(parent(B)), A)
        rmul!(A::StridedMatrix{T}, B::$t{<:Any,<:Adjoint{T,<:StridedMatrix}}) where {T<:BlasReal} =
            BLAS.trmm!('R', $uploc, 'T', $isunitc, one(T), parent(parent(B)), A)

        # Left division
        ldiv!(A::$t{<:Any,<:Transpose{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
            LAPACK.trtrs!($uploc, 'T', $isunitc, parent(parent(A)), B)
        ldiv!(A::$t{<:Any,<:Adjoint{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasReal} =
            LAPACK.trtrs!($uploc, 'T', $isunitc, parent(parent(A)), B)
        ldiv!(A::$t{<:Any,<:Adjoint{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasComplex} =
            LAPACK.trtrs!($uploc, 'C', $isunitc, parent(parent(A)), B)

        # Right division
        rdiv!(A::StridedMatrix{T}, B::$t{<:Any,<:Transpose{T,<:StridedMatrix}}) where {T<:BlasFloat} =
            BLAS.trsm!('R', $uploc, 'T', $isunitc, one(T), parent(parent(B)), A)
        rdiv!(A::StridedMatrix{T}, B::$t{<:Any,<:Adjoint{T,<:StridedMatrix}}) where {T<:BlasReal} =
            BLAS.trsm!('R', $uploc, 'T', $isunitc, one(T), parent(parent(B)), A)
        rdiv!(A::StridedMatrix{T}, B::$t{<:Any,<:Adjoint{T,<:StridedMatrix}}) where {T<:BlasComplex} =
            BLAS.trsm!('R', $uploc, 'C', $isunitc, one(T), parent(parent(B)), A)
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

for (t, tfun) in ((:Adjoint, :adjoint), (:Transpose, :transpose))
    @eval begin
        function lmul!(xA::UpperTriangular{<:Any,<:$t}, B::StridedVecOrMat)
            A = xA.data
            m, n = size(B, 1), size(B, 2)
            if m != size(A, 1)
                throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
            end
            pA = parent(A)
            for j = 1:n
                for i = 1:m
                    Bij = $tfun(pA[i,i])*B[i,j]
                    for k = i + 1:m
                        Bij += $tfun(pA[k,i])*B[k,j]
                    end
                    B[i,j] = Bij
                end
            end
            B
        end

        function lmul!(xA::UnitUpperTriangular{<:Any,<:$t}, B::StridedVecOrMat)
            A = xA.data
            m, n = size(B, 1), size(B, 2)
            if m != size(A, 1)
                throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
            end
            pA = parent(A)
            for j = 1:n
                for i = 1:m
                    Bij = B[i,j]
                    for k = i + 1:m
                        Bij += $tfun(pA[k,i])*B[k,j]
                    end
                    B[i,j] = Bij
                end
            end
            B
        end

        function lmul!(xA::LowerTriangular{<:Any,<:$t}, B::StridedVecOrMat)
            A = xA.data
            m, n = size(B, 1), size(B, 2)
            if m != size(A, 1)
                throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
            end
            pA = parent(A)
            for j = 1:n
                for i = m:-1:1
                    Bij = $tfun(pA[i,i])*B[i,j]
                    for k = 1:i - 1
                        Bij += $tfun(pA[k,i])*B[k,j]
                    end
                    B[i,j] = Bij
                end
            end
            B
        end
        function lmul!(xA::UnitLowerTriangular{<:Any,<:$t}, B::StridedVecOrMat)
            A = xA.data
            m, n = size(B, 1), size(B, 2)
            if m != size(A, 1)
                throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
            end
            pA = parent(A)
            for j = 1:n
                for i = m:-1:1
                    Bij = B[i,j]
                    for k = 1:i - 1
                        Bij += $tfun(pA[k,i])*B[k,j]
                    end
                    B[i,j] = Bij
                end
            end
            B
        end
    end
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

for (t, tfun) in ((:Adjoint, :adjoint), (:Transpose, :transpose))
    @eval begin
        function rmul!(A::StridedMatrix, B::UpperTriangular{<:Any,<:$t})
            m, n = size(A)
            if size(B, 1) != n
                throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
            end
            pB = parent(parent(B))
            for i = 1:m
                for j = n:-1:1
                    Aij = A[i,j]*$tfun(pB[j,j])
                    for k = 1:j - 1
                        Aij += A[i,k]*$tfun(pB[j,k])
                    end
                    A[i,j] = Aij
                end
            end
            A
        end

        function rmul!(A::StridedMatrix, B::UnitUpperTriangular{<:Any,<:$t})
            m, n = size(A)
            if size(B, 1) != n
                throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
            end
            pB = parent(parent(B))
            for i = 1:m
                for j = n:-1:1
                    Aij = A[i,j]
                    for k = 1:j - 1
                        Aij += A[i,k]*$tfun(pB[j,k])
                    end
                    A[i,j] = Aij
                end
            end
            A
        end

        function rmul!(A::StridedMatrix, B::LowerTriangular{<:Any,<:$t})
            m, n = size(A)
            if size(B, 1) != n
                throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
            end
            pB = parent(parent(B))
            for i = 1:m
                for j = 1:n
                    Aij = A[i,j]*$tfun(pB[j,j])
                    for k = j + 1:n
                        Aij += A[i,k]*$tfun(pB[j,k])
                    end
                    A[i,j] = Aij
                end
            end
            A
        end

        function rmul!(A::StridedMatrix, B::UnitLowerTriangular{<:Any,<:$t})
            m, n = size(A)
            if size(B, 1) != n
                throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
            end
            pB = parent(parent(B))
            for i = 1:m
                for j = 1:n
                    Aij = A[i,j]
                    for k = j + 1:n
                        Aij += A[i,k]*$tfun(pB[j,k])
                    end
                    A[i,j] = Aij
                end
            end
            A
        end
    end
end

#Generic solver using naive substitution
# manually hoisting b[j] significantly improves performance as of Dec 2015
# manually eliding bounds checking significantly improves performance as of Dec 2015
# directly indexing A.data rather than A significantly improves performance as of Dec 2015
# replacing repeated references to A.data with [Adata = A.data and references to Adata]
# does not significantly impact performance as of Dec 2015
# replacing repeated references to A.data[j,j] with [Ajj = A.data[j,j] and references to Ajj]
# does not significantly impact performance as of Dec 2015
function ldiv!(A::UpperTriangular, b::AbstractVector)
    require_one_based_indexing(A, b)
    n = size(A, 2)
    if !(n == length(b))
        throw(DimensionMismatch("second dimension of left hand side A, $n, and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        iszero(A.data[j,j]) && throw(SingularException(j))
        bj = b[j] = A.data[j,j] \ b[j]
        for i in j-1:-1:1 # counterintuitively 1:j-1 performs slightly better
            b[i] -= A.data[i,j] * bj
        end
    end
    return b
end
function ldiv!(A::UnitUpperTriangular, b::AbstractVector)
    require_one_based_indexing(A, b)
    n = size(A, 2)
    if !(n == length(b))
        throw(DimensionMismatch("second dimension of left hand side A, $n, and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        bj = b[j]
        for i in j-1:-1:1 # counterintuitively 1:j-1 performs slightly better
            b[i] -= A.data[i,j] * bj
        end
    end
    return b
end
function ldiv!(A::LowerTriangular, b::AbstractVector)
    require_one_based_indexing(A, b)
    n = size(A, 2)
    if !(n == length(b))
        throw(DimensionMismatch("second dimension of left hand side A, $n, and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        iszero(A.data[j,j]) && throw(SingularException(j))
        bj = b[j] = A.data[j,j] \ b[j]
        for i in j+1:n
            b[i] -= A.data[i,j] * bj
        end
    end
    return b
end
function ldiv!(A::UnitLowerTriangular, b::AbstractVector)
    require_one_based_indexing(A, b)
    n = size(A, 2)
    if !(n == length(b))
        throw(DimensionMismatch("second dimension of left hand side A, $n, and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        bj = b[j]
        for i in j+1:n
            b[i] -= A.data[i,j] * bj
        end
    end
    return b
end
function ldiv!(A::AbstractTriangular, B::AbstractMatrix)
    require_one_based_indexing(A, B)
    nA, mA = size(A)
    n = size(B, 1)
    if nA != n
        throw(DimensionMismatch("second dimension of left hand side A, $mA, and first dimension of right hand side B, $n, must be equal"))
    end
    for b in eachcol(B)
        ldiv!(A, b)
    end
    B
end

# in the following transpose and conjugate transpose naive substitution variants,
# accumulating in z rather than b[j,k] significantly improves performance as of Dec 2015
for (t, tfun) in ((:Adjoint, :adjoint), (:Transpose, :transpose))
    @eval begin
        function ldiv!(xA::UpperTriangular{<:Any,<:$t}, b::AbstractVector)
            require_one_based_indexing(xA, b)
            A = parent(parent(xA))
            n = size(A, 1)
            if !(n == length(b))
                throw(DimensionMismatch("first dimension of left hand side A, $n, and length of right hand side b, $(length(b)), must be equal"))
            end
            @inbounds for j in n:-1:1
                z = b[j]
                for i in n:-1:j+1
                    z -= $tfun(A[i,j]) * b[i]
                end
                iszero(A[j,j]) && throw(SingularException(j))
                b[j] = $tfun(A[j,j]) \ z
            end
            return b
        end

        function ldiv!(xA::UnitUpperTriangular{<:Any,<:$t}, b::AbstractVector)
            require_one_based_indexing(xA, b)
            A = parent(parent(xA))
            n = size(A, 1)
            if !(n == length(b))
                throw(DimensionMismatch("first dimension of left hand side A, $n, and length of right hand side b, $(length(b)), must be equal"))
            end
            @inbounds for j in n:-1:1
                z = b[j]
                for i in n:-1:j+1
                    z -= $tfun(A[i,j]) * b[i]
                end
                b[j] = z
            end
            return b
        end

        function ldiv!(xA::LowerTriangular{<:Any,<:$t}, b::AbstractVector)
            require_one_based_indexing(xA, b)
            A = parent(parent(xA))
            n = size(A, 1)
            if !(n == length(b))
                throw(DimensionMismatch("first dimension of left hand side A, $n, and length of right hand side b, $(length(b)), must be equal"))
            end
            @inbounds for j in 1:n
                z = b[j]
                for i in 1:j-1
                    z -= $tfun(A[i,j]) * b[i]
                end
                iszero(A[j,j]) && throw(SingularException(j))
                b[j] = $tfun(A[j,j]) \ z
            end
            return b
        end

        function ldiv!(xA::UnitLowerTriangular{<:Any,<:$t}, b::AbstractVector)
            require_one_based_indexing(xA, b)
            A = parent(parent(xA))
            n = size(A, 1)
            if !(n == length(b))
                throw(DimensionMismatch("first dimension of left hand side A, $n, and length of right hand side b, $(length(b)), must be equal"))
            end
            @inbounds for j in 1:n
                z = b[j]
                for i in 1:j-1
                    z -= $tfun(A[i,j]) * b[i]
                end
                b[j] = z
            end
            return b
        end
    end
end

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

for (t, tfun) in ((:Adjoint, :adjoint), (:Transpose, :transpose))
    @eval begin
        function rdiv!(A::StridedMatrix, xB::LowerTriangular{<:Any,<:$t})
            B = parent(parent(xB))
            m, n = size(A)
            if size(B, 1) != n
                throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
            end
            for i = 1:m
                for j = n:-1:1
                    Aij = A[i,j]
                    for k = j + 1:n
                        Aij -= A[i,k]*$tfun(B[j,k])
                    end
                    A[i,j] = Aij/$tfun(B[j,j])
                end
            end
            A
        end
        function rdiv!(A::StridedMatrix, xB::UnitLowerTriangular{<:Any,<:$t})
            B = parent(parent(xB))
            m, n = size(A)
            if size(B, 1) != n
                throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
            end
            for i = 1:m
                for j = n:-1:1
                    Aij = A[i,j]
                    for k = j + 1:n
                        Aij -= A[i,k]*$tfun(B[j,k])
                    end
                    A[i,j] = Aij
                end
            end
            A
        end

        function rdiv!(A::StridedMatrix, xB::UpperTriangular{<:Any,<:$t})
            B = parent(parent(xB))
            m, n = size(A)
            if size(B, 1) != n
                throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
            end
            for i = 1:m
                for j = 1:n
                    Aij = A[i,j]
                    for k = 1:j - 1
                        Aij -= A[i,k]*$tfun(B[j,k])
                    end
                    A[i,j] = Aij/$tfun(B[j,j])
                end
            end
            A
        end
        function rdiv!(A::StridedMatrix, xB::UnitUpperTriangular{<:Any,<:$t})
            B = parent(parent(xB))
            m, n = size(A)
            if size(B, 1) != n
                throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
            end
            for i = 1:m
                for j = 1:n
                    Aij = A[i,j]
                    for k = 1:j - 1
                        Aij -= A[i,k]*$tfun(B[j,k])
                    end
                    A[i,j] = Aij
                end
            end
            A
        end
    end
end

function lmul!(A::Union{UpperTriangular,UnitUpperTriangular}, B::UpperTriangular)
    UpperTriangular(lmul!(A, triu!(B.data)))
end
function lmul!(A::Union{LowerTriangular,UnitLowerTriangular}, B::LowerTriangular)
    return LowerTriangular(lmul!(A, tril!(B.data)))
end
function ldiv!(xA::Union{UpperTriangular,UnitUpperTriangular}, B::UpperTriangular)
    return UpperTriangular(ldiv!(xA, triu!(B.data)))
end
function ldiv!(xA::Union{LowerTriangular,UnitLowerTriangular}, B::LowerTriangular)
    return LowerTriangular(ldiv!(xA, tril!(B.data)))
end

function rdiv!(A::UpperTriangular, B::Union{UpperTriangular,UnitUpperTriangular})
    return UpperTriangular(rdiv!(triu!(A.data), B))
end
function rdiv!(A::LowerTriangular, B::Union{LowerTriangular,UnitLowerTriangular})
    return LowerTriangular(rdiv!(tril!(A.data), B))
end
function rmul!(A::UpperTriangular, B::Union{UpperTriangular,UnitUpperTriangular})
    return UpperTriangular(rmul!(triu!(A.data), B))
end
function rmul!(A::LowerTriangular, B::Union{LowerTriangular,UnitLowerTriangular})
    return LowerTriangular(rmul!(tril!(A.data), B))
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
            BB = copy_similar(B, TAB)
            return LowerTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end

        function $(f)(A::UnitLowerTriangular, B::LowerTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
             BB = copy_similar(B, TAB)
            return LowerTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end

        function $(f)(A::LowerTriangular, B::UnitLowerTriangular)
            TAB = typeof(($f)(zero(eltype(A)), zero(eltype(B))) +
                         ($f)(zero(eltype(A)), zero(eltype(B))))
             BB = copy_similar(B, TAB)
            return LowerTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end

        function $(f)(A::UnitLowerTriangular, B::UnitLowerTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
             BB = copy_similar(B, TAB)
            return UnitLowerTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f)(A::UpperTriangular, B::UpperTriangular)
            TAB = typeof(($f)(zero(eltype(A)), zero(eltype(B))) +
                         ($f)(zero(eltype(A)), zero(eltype(B))))
            BB = copy_similar(B, TAB)
            return UpperTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f)(A::UnitUpperTriangular, B::UpperTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            BB = copy_similar(B, TAB)
            return UpperTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f)(A::UpperTriangular, B::UnitUpperTriangular)
            TAB = typeof(($f)(zero(eltype(A)), zero(eltype(B))) +
                         ($f)(zero(eltype(A)), zero(eltype(B))))
            BB = copy_similar(B, TAB)
            return UpperTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f)(A::UnitUpperTriangular, B::UnitUpperTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            BB = copy_similar(B, TAB)
            return UnitUpperTriangular($f2!(convert(AbstractMatrix{TAB}, A), BB))
        end
    end
end

function (/)(A::LowerTriangular, B::LowerTriangular)
    TAB = typeof((/)(zero(eltype(A)), one(eltype(B))) +
                 (/)(zero(eltype(A)), one(eltype(B))))
    AA = copy_similar(A, TAB)
    return LowerTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::UnitLowerTriangular, B::LowerTriangular)
    TAB = typeof((/)(zero(eltype(A)), one(eltype(B))) +
                 (/)(zero(eltype(A)), one(eltype(B))))
    AA = copy_similar(A, TAB)
    return LowerTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::LowerTriangular, B::UnitLowerTriangular)
    TAB = typeof((/)(zero(eltype(A)), one(eltype(B))) +
                 (/)(zero(eltype(A)), one(eltype(B))))
    AA = copy_similar(A, TAB)
    return LowerTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::UnitLowerTriangular, B::UnitLowerTriangular)
    TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                 (*)(zero(eltype(A)), zero(eltype(B))))
    AA = copy_similar(A, TAB)
    return UnitLowerTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::UpperTriangular, B::UpperTriangular)
    TAB = typeof((/)(zero(eltype(A)), one(eltype(B))) +
                 (/)(zero(eltype(A)), one(eltype(B))))
    AA = copy_similar(A, TAB)
    return UpperTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::UnitUpperTriangular, B::UpperTriangular)
    TAB = typeof((/)(zero(eltype(A)), one(eltype(B))) +
                 (/)(zero(eltype(A)), one(eltype(B))))
    AA = copy_similar(A, TAB)
    return UpperTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::UpperTriangular, B::UnitUpperTriangular)
    TAB = typeof((/)(zero(eltype(A)), one(eltype(B))) +
                 (/)(zero(eltype(A)), one(eltype(B))))
    AA = copy_similar(A, TAB)
    return UpperTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::UnitUpperTriangular, B::UnitUpperTriangular)
    TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                 (*)(zero(eltype(A)), zero(eltype(B))))
    AA = copy_similar(A, TAB)
    return UnitUpperTriangular(rdiv!(AA, convert(AbstractMatrix{TAB}, B)))
end

_inner_type_promotion(A,B) = promote_type(eltype(A), eltype(B), typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B))))
## The general promotion methods
function *(A::AbstractTriangular, B::AbstractTriangular)
    TAB = _inner_type_promotion(A,B)
    BB = copy_similar(B, TAB)
    lmul!(convert(AbstractArray{TAB}, A), BB)
end

for mat in (:AbstractVector, :AbstractMatrix)
    ### Multiplication with triangle to the left and hence rhs cannot be transposed.
    @eval function *(A::AbstractTriangular, B::$mat)
        require_one_based_indexing(B)
        TAB = _inner_type_promotion(A,B)
        BB = copy_similar(B, TAB)
        lmul!(convert(AbstractArray{TAB}, A), BB)
    end
    ### Left division with triangle to the left hence rhs cannot be transposed. No quotients.
    @eval function \(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::$mat)
        require_one_based_indexing(B)
        TAB = _inner_type_promotion(A,B)
        BB = copy_similar(B, TAB)
        ldiv!(convert(AbstractArray{TAB}, A), BB)
    end
    ### Left division with triangle to the left hence rhs cannot be transposed. Quotients.
    @eval function \(A::Union{UpperTriangular,LowerTriangular}, B::$mat)
        require_one_based_indexing(B)
        TAB = typeof((zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))/one(eltype(A)))
        BB = copy_similar(B, TAB)
        ldiv!(convert(AbstractArray{TAB}, A), BB)
    end
    ### Right division with triangle to the right hence lhs cannot be transposed. No quotients.
    @eval function /(A::$mat, B::Union{UnitUpperTriangular, UnitLowerTriangular})
        require_one_based_indexing(A)
        TAB = _inner_type_promotion(A,B)
        AA = copy_similar(A, TAB)
        rdiv!(AA, convert(AbstractArray{TAB}, B))
    end
    ### Right division with triangle to the right hence lhs cannot be transposed. Quotients.
    @eval function /(A::$mat, B::Union{UpperTriangular,LowerTriangular})
        require_one_based_indexing(A)
        TAB = typeof((zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))/one(eltype(A)))
        AA = copy_similar(A, TAB)
        rdiv!(AA, convert(AbstractArray{TAB}, B))
    end
end
### Multiplication with triangle to the right and hence lhs cannot be transposed.
# Only for AbstractMatrix, hence outside the above loop.
function *(A::AbstractMatrix, B::AbstractTriangular)
    require_one_based_indexing(A)
    TAB = _inner_type_promotion(A,B)
    AA = copy_similar(A, TAB)
    rmul!(AA, convert(AbstractArray{TAB}, B))
end
# ambiguity resolution with definitions in linalg/rowvector.jl
*(v::AdjointAbsVec, A::AbstractTriangular) = adjoint(adjoint(A) * v.parent)
*(v::TransposeAbsVec, A::AbstractTriangular) = transpose(transpose(A) * v.parent)

# If these are not defined, they will fallback to the versions in matmul.jl
# and dispatch to generic_matmatmul! which is very costly to compile. The methods
# below might compute an unnecessary copy. Eliminating the copy requires adding
# all the promotion logic here once again. Since these methods are probably relatively
# rare, we chose not to bother for now.
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::AbstractTriangular) = copy(A) * B
*(A::Transpose{<:Any,<:AbstractMatrix}, B::AbstractTriangular) = copy(A) * B
*(A::AbstractTriangular, B::Adjoint{<:Any,<:AbstractMatrix}) = A * copy(B)
*(A::AbstractTriangular, B::Transpose{<:Any,<:AbstractMatrix}) = A * copy(B)

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
#   Al-Mohy and Higham, "Improved inverse scaling and squaring algorithms for
#     the matrix logarithm", SIAM J. Sci. Comput., 34(4), (2012), pp. C153–C169.
#   Al-Mohy, Higham and Relton, "Computing the Frechet derivative of the matrix
#     logarithm and estimating the condition number", SIAM J. Sci. Comput.,
#     35(4), (2013), C394–C410.
#
# Based on the code available at http://eprints.ma.man.ac.uk/1851/02/logm.zip,
# Copyright (c) 2011, Awad H. Al-Mohy and Nicholas J. Higham
# Julia version relicensed with permission from original authors
log(A::UpperTriangular{T}) where {T<:BlasFloat} = log_quasitriu(A)
log(A::UnitUpperTriangular{T}) where {T<:BlasFloat} = log_quasitriu(A)
log(A::LowerTriangular) = copy(transpose(log(copy(transpose(A)))))
log(A::UnitLowerTriangular) = copy(transpose(log(copy(transpose(A)))))

function log_quasitriu(A0::AbstractMatrix{T}) where T<:BlasFloat
    # allocate real A if log(A) will be real and complex A otherwise
    n = checksquare(A0)
    if isreal(A0) && (!istriu(A0) || !any(x -> real(x) < zero(real(T)), diag(A0)))
        A = T <: Complex ? real(A0) : copy(A0)
    else
        A = T <: Complex ? copy(A0) : complex(A0)
    end
    if A0 isa UnitUpperTriangular
        A = UpperTriangular(parent(A))
        @inbounds for i in 1:n
            A[i,i] = 1
        end
    end
    Y0 = _log_quasitriu!(A0, A)
    # return complex result for complex input
    Y = T <: Complex ? complex(Y0) : Y0

    if A0 isa UpperTriangular || A0 isa UnitUpperTriangular
        return UpperTriangular(Y)
    else
        return Y
    end
end
# type-stable implementation of log_quasitriu
# A is a copy of A0 that is overwritten while computing the result. It has the same eltype
# as the result.
function _log_quasitriu!(A0, A)
    # Find Padé degree m and s while replacing A with A^(1/2^s)
    m, s = _find_params_log_quasitriu!(A)

    # Compute accurate superdiagonal of A
    _pow_superdiag_quasitriu!(A, A0, 0.5^s)

    # Compute accurate block diagonal of A
    _sqrt_pow_diag_quasitriu!(A, A0, s)

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
    t = eltype(A)
    n = size(A, 1)
    Y = zeros(t, n, n)
    B = similar(A)
    for k = 1:m
        B .= t(x[k]) .* A
        @inbounds for i in 1:n
            B[i,i] += 1
        end
        Y .+= t(w[k]) .* rdiv_quasitriu!(A, B)
    end

    # Scale back
    lmul!(2.0^s, Y)

    # Compute accurate diagonal and superdiagonal of log(A)
    _log_diag_quasitriu!(Y, A0)

    return Y
end

# Auxiliary functions for matrix logarithm and matrix power

# Find Padé degree m and s while replacing A with A^(1/2^s)
#   Al-Mohy and Higham, "Improved inverse scaling and squaring algorithms for
#     the matrix logarithm", SIAM J. Sci. Comput., 34(4), (2012), pp. C153–C169.
#   from Algorithm 4.1
function _find_params_log_quasitriu!(A)
    maxsqrt = 100
    theta = [1.586970738772063e-005,
         2.313807884242979e-003,
         1.938179313533253e-002,
         6.209171588994762e-002,
         1.276404810806775e-001,
         2.060962623452836e-001,
         2.879093714241194e-001]
    tmax = size(theta, 1)
    n = size(A, 1)
    p = 0
    m = 0

    # Find s0, the smallest s such that the ρ(triu(A)^(1/2^s) - I) ≤ theta[tmax], where ρ(X)
    # is the spectral radius of X
    d = complex.(@view(A[diagind(A)]))
    dm1 = d .- 1
    s = 0
    while norm(dm1, Inf) > theta[tmax] && s < maxsqrt
        d .= sqrt.(d)
        dm1 .= d .- 1
        s = s + 1
    end
    s0 = s

    # Compute repeated roots
    for k = 1:min(s, maxsqrt)
        _sqrt_quasitriu!(A isa UpperTriangular ? parent(A) : A, A)
    end

    # these three never needed at the same time, so reuse the same temporary
    AmI = AmI4 = AmI5 = A - I
    AmI2 = AmI * AmI
    AmI3 = AmI2 * AmI
    d2 = sqrt(opnorm(AmI2, 1))
    d3 = cbrt(opnorm(AmI3, 1))
    alpha2 = max(d2, d3)
    foundm = false
    if alpha2 <= theta[2]
        m = alpha2 <= theta[1] ? 1 : 2
        foundm = true
    end

    while !foundm
        more_sqrt = false
        mul!(AmI4, AmI2, AmI2)
        d4 = opnorm(AmI4, 1)^(1/4)
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
                more_sqrt = true
                p = p + 1
           end
        end

        if !more_sqrt
            mul!(AmI5, AmI3, AmI2)
            d5 = opnorm(AmI5, 1)^(1/5)
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
        _sqrt_quasitriu!(A isa UpperTriangular ? parent(A) : A, A)
        copyto!(AmI, A)
        for i in 1:n
            @inbounds AmI[i,i] -= 1
        end
        mul!(AmI2, AmI, AmI)
        mul!(AmI3, AmI2, AmI)
        d3 = cbrt(opnorm(AmI3, 1))
        s = s + 1
    end
    return m, s
end

# Compute accurate diagonal of A = A0^s - I
function sqrt_diag!(A0::UpperTriangular, A::UpperTriangular, s)
    n = checksquare(A0)
    T = eltype(A)
    @inbounds for i = 1:n
        a = complex(A0[i,i])
        A[i,i] = _sqrt_pow(a, s)
    end
end
# Compute accurate block diagonal of A = A0^s - I for upper quasi-triangular A0 produced
# by the Schur decomposition. Diagonal is made of 1x1 and 2x2 blocks.
# 2x2 blocks are real with non-negative conjugate pair eigenvalues
function _sqrt_pow_diag_quasitriu!(A, A0, s)
    n = checksquare(A0)
    t = typeof(sqrt(zero(eltype(A))))
    i = 1
    @inbounds while i < n
        if iszero(A0[i+1,i])  # 1x1 block
            A[i,i] = _sqrt_pow(t(A0[i,i]), s)
            i += 1
        else  # real 2x2 block
            @views _sqrt_pow_diag_block_2x2!(A[i:i+1,i:i+1], A0[i:i+1,i:i+1], s)
            i += 2
        end
    end
    if i == n  # last block is 1x1
        @inbounds A[n,n] = _sqrt_pow(t(A0[n,n]), s)
    end
    return A
end
# compute a^(1/2^s)-1
#   Al-Mohy, "A more accurate Briggs method for the logarithm",
#      Numer. Algorithms, 59, (2012), 393–402.
#   Algorithm 2
function _sqrt_pow(a::Number, s)
    T = typeof(sqrt(zero(a)))
    s == 0 && return T(a) - 1
    s0 = s
    if imag(a) >= 0 && real(a) <= 0 && !iszero(a)  # angle(a) ≥ π / 2
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
    return z0 / r
end
# compute A0 = A^(1/2^s)-I for 2x2 real matrices A and A0
# A has non-negative conjugate pair eigenvalues
# "Improved Inverse Scaling and Squaring Algorithms for the Matrix Logarithm"
# SIAM J. Sci. Comput., 34(4), (2012) C153–C169. doi: 10.1137/110852553
# Algorithm 5.1
Base.@propagate_inbounds function _sqrt_pow_diag_block_2x2!(A, A0, s)
    _sqrt_real_2x2!(A, A0)
    if isone(s)
        A[1,1] -= 1
        A[2,2] -= 1
    else
        # Z = A - I
        z11, z21, z12, z22 = A[1,1] - 1, A[2,1], A[1,2], A[2,2] - 1
        # A = sqrt(A)
        _sqrt_real_2x2!(A, A)
        # P = A + I
        p11, p21, p12, p22 = A[1,1] + 1, A[2,1], A[1,2], A[2,2] + 1
        for i in 1:(s - 2)
            # A = sqrt(A)
            _sqrt_real_2x2!(A, A)
            a11, a21, a12, a22 = A[1,1], A[2,1], A[1,2], A[2,2]
            # P += P * A
            r11 = p11*(1 + a11) + p12*a21
            r22 = p21*a12 + p22*(1 + a22)
            p21 = p21*(1 + a11) + p22*a21
            p12 = p11*a12 + p12*(1 + a22)
            p11 = r11
            p22 = r22
        end
        # A = Z / P
        c = inv(p11*p22 - p21*p12)
        A[1,1] = (p22*z11 - p21*z12) * c
        A[2,1] = (p22*z21 - p21*z22) * c
        A[1,2] = (p11*z12 - p12*z11) * c
        A[2,2] = (p11*z22 - p12*z21) * c
    end
    return A
end
# Compute accurate superdiagonal of A = A0^s - I for upper quasi-triangular A0 produced
# by a Schur decomposition.
# Higham and Lin, "A Schur–Padé Algorithm for Fractional Powers of a Matrix"
# SIAM J. Matrix Anal. Appl., 32(3), (2011), 1056–1078.
# Equation 5.6
# see also blockpower for when A0 is upper triangular
function _pow_superdiag_quasitriu!(A, A0, p)
    n = checksquare(A0)
    t = eltype(A)
    k = 1
    @inbounds while k < n
        if !iszero(A[k+1,k])
            k += 2
            continue
        end
        if !(k == n - 1 || iszero(A[k+2,k+1]))
            k += 3
            continue
        end
        Ak = t(A0[k,k])
        Akp1 = t(A0[k+1,k+1])

        Akp = Ak^p
        Akp1p = Akp1^p

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
        k += 1
    end
end

# Compute accurate block diagonal and superdiagonal of A = log(A0) for upper
# quasi-triangular A0 produced by the Schur decomposition.
function _log_diag_quasitriu!(A, A0)
    n = checksquare(A0)
    t = eltype(A)
    k = 1
    @inbounds while k < n
        if iszero(A0[k+1,k])  # 1x1 block
            Ak = t(A0[k,k])
            logAk = log(Ak)
            A[k,k] = logAk
            if k < n - 2 && iszero(A0[k+2,k+1])
                Akp1 = t(A0[k+1,k+1])
                logAkp1 = log(Akp1)
                A[k+1,k+1] = logAkp1
                if Ak == Akp1
                    A[k,k+1] = A0[k,k+1] / Ak
                elseif 2 * abs(Ak) < abs(Akp1) || 2 * abs(Akp1) < abs(Ak) || iszero(Akp1 + Ak)
                    A[k,k+1] = A0[k,k+1] * (logAkp1 - logAk) / (Akp1 - Ak)
                else
                    z = (Akp1 - Ak)/(Akp1 + Ak)
                    if abs(z) > 1
                        A[k,k+1] = A0[k,k+1] * (logAkp1 - logAk) / (Akp1 - Ak)
                    else
                        w = atanh(z) + im * pi * (unw(logAkp1-logAk) - unw(log1p(z)-log1p(-z)))
                        A[k,k+1] = 2 * A0[k,k+1] * w / (Akp1 - Ak)
                    end
                end
                k += 2
            else
                k += 1
            end
        else  # real 2x2 block
            @views _log_diag_block_2x2!(A[k:k+1,k:k+1], A0[k:k+1,k:k+1])
            k += 2
        end
    end
    if k == n  # last 1x1 block
        @inbounds A[n,n] = log(t(A0[n,n]))
    end
    return A
end
# compute A0 = log(A) for 2x2 real matrices A and A0, where A0 is a diagonal 2x2 block
# produced by real Schur decomposition.
# Al-Mohy, Higham and Relton, "Computing the Frechet derivative of the matrix
# logarithm and estimating the condition number", SIAM J. Sci. Comput.,
# 35(4), (2013), C394–C410.
# Eq. 6.1
Base.@propagate_inbounds function _log_diag_block_2x2!(A, A0)
    a, b, c = A0[1,1], A0[1,2], A0[2,1]
    # avoid underflow/overflow for large/small b and c
    s = sqrt(abs(b)) * sqrt(abs(c))
    θ = atan(s, a)
    t = θ / s
    au = abs(a)
    if au > s
        a1 = log1p((s / au)^2) / 2 + log(au)
    else
        a1 = log1p((au / s)^2) / 2 + log(s)
    end
    A[1,1] = a1
    A[2,1] = c*t
    A[1,2] = b*t
    A[2,2] = a1
    return A
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

# compute A / B for upper quasi-triangular B, possibly overwriting B
function rdiv_quasitriu!(A, B)
    n = checksquare(A)
    AG = copy(A)
    # use Givens rotations to annihilate 2x2 blocks
    @inbounds for k in 1:(n-1)
        s = B[k+1,k]
        iszero(s) && continue  # 1x1 block
        G = first(givens(B[k+1,k+1], s, k, k+1))
        rmul!(B, G)
        rmul!(AG, G)
    end
    return rdiv!(AG, UpperTriangular(B))
end

# End of auxiliary functions for matrix logarithm and matrix power

sqrt(A::UpperTriangular) = sqrt_quasitriu(A)
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

# Auxiliary functions for matrix square root

# square root of upper triangular or real upper quasitriangular matrix
function sqrt_quasitriu(A0; blockwidth = eltype(A0) <: Complex ? 512 : 256)
    n = checksquare(A0)
    T = eltype(A0)
    Tr = typeof(sqrt(real(zero(T))))
    Tc = typeof(sqrt(complex(zero(T))))
    if isreal(A0)
        is_sqrt_real = true
        if istriu(A0)
            for i in 1:n
                Aii = real(A0[i,i])
                if Aii < zero(Aii)
                    is_sqrt_real = false
                    break
                end
            end
        end
        if is_sqrt_real
            R = zeros(Tr, n, n)
            A = real(A0)
        else
            R = zeros(Tc, n, n)
            A = A0
        end
    else
        A = A0
        R = zeros(Tc, n, n)
    end
    _sqrt_quasitriu!(R, A; blockwidth=blockwidth, n=n)
    Rc = eltype(A0) <: Real ? R : complex(R)
    if A0 isa UpperTriangular
        return UpperTriangular(Rc)
    elseif A0 isa UnitUpperTriangular
        return UnitUpperTriangular(Rc)
    else
        return Rc
    end
end

# in-place recursive sqrt of upper quasi-triangular matrix A from
# Deadman E., Higham N.J., Ralha R. (2013) Blocked Schur Algorithms for Computing the Matrix
# Square Root. Applied Parallel and Scientific Computing. PARA 2012. Lecture Notes in
# Computer Science, vol 7782. https://doi.org/10.1007/978-3-642-36803-5_12
function _sqrt_quasitriu!(R, A; blockwidth=64, n=checksquare(A))
    if n ≤ blockwidth || !(eltype(R) <: BlasFloat) # base case, perform "point" algorithm
        _sqrt_quasitriu_block!(R, A)
    else  # compute blockwise recursion
        split = div(n, 2)
        iszero(A[split+1, split]) || (split += 1) # don't split 2x2 diagonal block
        r1 = 1:split
        r2 = (split + 1):n
        n1, n2 = split, n - split
        A11, A12, A22 = @views A[r1,r1], A[r1,r2], A[r2,r2]
        R11, R12, R22 = @views R[r1,r1], R[r1,r2], R[r2,r2]
        # solve diagonal blocks recursively
        _sqrt_quasitriu!(R11, A11; blockwidth=blockwidth, n=n1)
        _sqrt_quasitriu!(R22, A22; blockwidth=blockwidth, n=n2)
        # solve off-diagonal block
        R12 .= .- A12
        _sylvester_quasitriu!(R11, R22, R12; blockwidth=blockwidth, nA=n1, nB=n2, raise=false)
    end
    return R
end

function _sqrt_quasitriu_block!(R, A)
    _sqrt_quasitriu_diag_block!(R, A)
    _sqrt_quasitriu_offdiag_block!(R, A)
    return R
end

function _sqrt_quasitriu_diag_block!(R, A)
    n = size(R, 1)
    ta = eltype(R) <: Complex ? complex(eltype(A)) : eltype(A)
    i = 1
    @inbounds while i < n
        if iszero(A[i + 1, i])
            R[i, i] = sqrt(ta(A[i, i]))
            i += 1
        else
            # this branch is never reached when A is complex triangular
            @views _sqrt_real_2x2!(R[i:(i + 1), i:(i + 1)], A[i:(i + 1), i:(i + 1)])
            i += 2
        end
    end
    if i == n
        R[n, n] = sqrt(ta(A[n, n]))
    end
    return R
end

function _sqrt_quasitriu_offdiag_block!(R, A)
    n = size(R, 1)
    j = 1
    @inbounds while j ≤ n
        jsize_is_2 = j < n && !iszero(A[j + 1, j])
        i = j - 1
        while i > 0
            isize_is_2 = i > 1 && !iszero(A[i, i - 1])
            if isize_is_2
                if jsize_is_2
                    _sqrt_quasitriu_offdiag_block_2x2!(R, A, i - 1, j)
                else
                    _sqrt_quasitriu_offdiag_block_2x1!(R, A, i - 1, j)
                end
                i -= 2
            else
                if jsize_is_2
                    _sqrt_quasitriu_offdiag_block_1x2!(R, A, i, j)
                else
                    _sqrt_quasitriu_offdiag_block_1x1!(R, A, i, j)
                end
                i -= 1
            end
        end
        j += 2 - !jsize_is_2
    end
    return R
end

# real square root of 2x2 diagonal block of quasi-triangular matrix from real Schur
# decomposition. Eqs 6.8-6.9 and Algorithm 6.5 of
# Higham, 2008, "Functions of Matrices: Theory and Computation", SIAM.
Base.@propagate_inbounds function _sqrt_real_2x2!(R, A)
    # in the real Schur form, A[1, 1] == A[2, 2], and A[2, 1] * A[1, 2] < 0
    θ, a21, a12 = A[1, 1], A[2, 1], A[1, 2]
    # avoid overflow/underflow of μ
    # for real sqrt, |d| ≤ 2 max(|a12|,|a21|)
    μ = sqrt(abs(a12)) * sqrt(abs(a21))
    α = _real_sqrt(θ, μ)
    c = 2α
    R[1, 1] = α
    R[2, 1] = a21 / c
    R[1, 2] = a12 / c
    R[2, 2] = α
    return R
end

# real part of square root of θ+im*μ
@inline function _real_sqrt(θ, μ)
    t = sqrt((abs(θ) + hypot(θ, μ)) / 2)
    return θ ≥ 0 ? t : μ / 2t
end

Base.@propagate_inbounds function _sqrt_quasitriu_offdiag_block_1x1!(R, A, i, j)
    Rii = R[i, i]
    Rjj = R[j, j]
    iszero(Rii) && iszero(Rjj) && return R
    t = eltype(R)
    tt = typeof(zero(t)*zero(t))
    r = tt(-A[i, j])
    @simd for k in (i + 1):(j - 1)
        r += R[i, k] * R[k, j]
    end
    iszero(r) && return R
    R[i, j] = sylvester(Rii, Rjj, r)
    return R
end

Base.@propagate_inbounds function _sqrt_quasitriu_offdiag_block_1x2!(R, A, i, j)
    jrange = j:(j + 1)
    t = eltype(R)
    tt = typeof(zero(t)*zero(t))
    r1 = tt(-A[i, j])
    r2 = tt(-A[i, j + 1])
    @simd for k in (i + 1):(j - 1)
        rik = R[i, k]
        r1 += rik * R[k, j]
        r2 += rik * R[k, j + 1]
    end
    Rjj = @view R[jrange, jrange]
    Rij = @view R[i, jrange]
    Rij[1] = r1
    Rij[2] = r2
    _sylvester_1x2!(R[i, i], Rjj, Rij)
    return R
end

Base.@propagate_inbounds function _sqrt_quasitriu_offdiag_block_2x1!(R, A, i, j)
    irange = i:(i + 1)
    t = eltype(R)
    tt = typeof(zero(t)*zero(t))
    r1 = tt(-A[i, j])
    r2 = tt(-A[i + 1, j])
    @simd for k in (i + 2):(j - 1)
        rkj = R[k, j]
        r1 += R[i, k] * rkj
        r2 += R[i + 1, k] * rkj
    end
    Rii = @view R[irange, irange]
    Rij = @view R[irange, j]
    Rij[1] = r1
    Rij[2] = r2
    @views _sylvester_2x1!(Rii, R[j, j], Rij)
    return R
end

Base.@propagate_inbounds function _sqrt_quasitriu_offdiag_block_2x2!(R, A, i, j)
    irange = i:(i + 1)
    jrange = j:(j + 1)
    t = eltype(R)
    tt = typeof(zero(t)*zero(t))
    for i′ in irange, j′ in jrange
        Cij = tt(-A[i′, j′])
        @simd for k in (i + 2):(j - 1)
            Cij += R[i′, k] * R[k, j′]
        end
        R[i′, j′] = Cij
    end
    Rii = @view R[irange, irange]
    Rjj = @view R[jrange, jrange]
    Rij = @view R[irange, jrange]
    if !iszero(Rij) && !all(isnan, Rij)
        _sylvester_2x2!(Rii, Rjj, Rij)
    end
    return R
end

# solve Sylvester's equation AX + XB = -C using blockwise recursion until the dimension of
# A and B are no greater than blockwidth, based on Algorithm 1 from
# Jonsson I, Kågström B. Recursive blocked algorithms for solving triangular systems—
# Part I: one-sided and coupled Sylvester-type matrix equations. (2002) ACM Trans Math Softw.
# 28(4), https://doi.org/10.1145/592843.592845.
# specify raise=false to avoid breaking the recursion if a LAPACKException is thrown when
# computing one of the blocks.
function _sylvester_quasitriu!(A, B, C; blockwidth=64, nA=checksquare(A), nB=checksquare(B), raise=true)
    if 1 ≤ nA ≤ blockwidth && 1 ≤ nB ≤ blockwidth
        _sylvester_quasitriu_base!(A, B, C; raise=raise)
    elseif nA ≥ 2nB ≥ 2
        _sylvester_quasitriu_split1!(A, B, C; blockwidth=blockwidth, nA=nA, nB=nB, raise=raise)
    elseif nB ≥ 2nA ≥ 2
        _sylvester_quasitriu_split2!(A, B, C; blockwidth=blockwidth, nA=nA, nB=nB, raise=raise)
    else
        _sylvester_quasitriu_splitall!(A, B, C; blockwidth=blockwidth, nA=nA, nB=nB, raise=raise)
    end
    return C
end
function _sylvester_quasitriu_base!(A, B, C; raise=true)
    try
        _, scale = LAPACK.trsyl!('N', 'N', A, B, C)
        rmul!(C, -inv(scale))
    catch e
        if !(e isa LAPACKException) || raise
            throw(e)
        end
    end
    return C
end
function _sylvester_quasitriu_split1!(A, B, C; nA=checksquare(A), kwargs...)
    iA = div(nA, 2)
    iszero(A[iA + 1, iA]) || (iA += 1)  # don't split 2x2 diagonal block
    rA1, rA2 = 1:iA, (iA + 1):nA
    nA1, nA2 = iA, nA-iA
    A11, A12, A22 = @views A[rA1,rA1], A[rA1,rA2], A[rA2,rA2]
    C1, C2 = @views C[rA1,:], C[rA2,:]
    _sylvester_quasitriu!(A22, B, C2; nA=nA2, kwargs...)
    mul!(C1, A12, C2, true, true)
    _sylvester_quasitriu!(A11, B, C1; nA=nA1, kwargs...)
    return C
end
function _sylvester_quasitriu_split2!(A, B, C; nB=checksquare(B), kwargs...)
    iB = div(nB, 2)
    iszero(B[iB + 1, iB]) || (iB += 1)  # don't split 2x2 diagonal block
    rB1, rB2 = 1:iB, (iB + 1):nB
    nB1, nB2 = iB, nB-iB
    B11, B12, B22 = @views B[rB1,rB1], B[rB1,rB2], B[rB2,rB2]
    C1, C2 = @views C[:,rB1], C[:,rB2]
    _sylvester_quasitriu!(A, B11, C1; nB=nB1, kwargs...)
    mul!(C2, C1, B12, true, true)
    _sylvester_quasitriu!(A, B22, C2; nB=nB2, kwargs...)
    return C
end
function _sylvester_quasitriu_splitall!(A, B, C; nA=checksquare(A), nB=checksquare(B), kwargs...)
    iA = div(nA, 2)
    iszero(A[iA + 1, iA]) || (iA += 1)  # don't split 2x2 diagonal block
    iB = div(nB, 2)
    iszero(B[iB + 1, iB]) || (iB += 1)  # don't split 2x2 diagonal block
    rA1, rA2 = 1:iA, (iA + 1):nA
    nA1, nA2 = iA, nA-iA
    rB1, rB2 = 1:iB, (iB + 1):nB
    nB1, nB2 = iB, nB-iB
    A11, A12, A22 = @views A[rA1,rA1], A[rA1,rA2], A[rA2,rA2]
    B11, B12, B22 = @views B[rB1,rB1], B[rB1,rB2], B[rB2,rB2]
    C11, C21, C12, C22 = @views C[rA1,rB1], C[rA2,rB1], C[rA1,rB2], C[rA2,rB2]
    _sylvester_quasitriu!(A22, B11, C21; nA=nA2, nB=nB1, kwargs...)
    mul!(C11, A12, C21, true, true)
    _sylvester_quasitriu!(A11, B11, C11; nA=nA1, nB=nB1, kwargs...)
    mul!(C22, C21, B12, true, true)
    _sylvester_quasitriu!(A22, B22, C22; nA=nA2, nB=nB2, kwargs...)
    mul!(C12, A12, C22, true, true)
    mul!(C12, C11, B12, true, true)
    _sylvester_quasitriu!(A11, B22, C12; nA=nA1, nB=nB2, kwargs...)
    return C
end

# End of auxiliary functions for matrix square root

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
*(A::AbstractTriangular, B::AdjointAbsVec) = adjoint(adjoint(B) * adjoint(A))
*(A::AbstractTriangular, B::TransposeAbsVec) = transpose(transpose(B) * transpose(A))

# disambiguation methods: /(Adjoint of AbsVec, <:AbstractTriangular)
/(u::AdjointAbsVec, A::Union{LowerTriangular,UpperTriangular}) = adjoint(adjoint(A) \ u.parent)
/(u::AdjointAbsVec, A::Union{UnitLowerTriangular,UnitUpperTriangular}) = adjoint(adjoint(A) \ u.parent)
# disambiguation methods: /(Transpose of AbsVec, <:AbstractTriangular)
/(u::TransposeAbsVec, A::Union{LowerTriangular,UpperTriangular}) = transpose(transpose(A) \ u.parent)
/(u::TransposeAbsVec, A::Union{UnitLowerTriangular,UnitUpperTriangular}) = transpose(transpose(A) \ u.parent)
# disambiguation methods: /(Transpose of AbsVec, Adj/Trans of <:AbstractTriangular)
for (tritype, comptritype) in ((:LowerTriangular, :UpperTriangular),
                               (:UnitLowerTriangular, :UnitUpperTriangular),
                               (:UpperTriangular, :LowerTriangular),
                               (:UnitUpperTriangular, :UnitLowerTriangular))
    @eval /(u::TransposeAbsVec, A::$tritype{<:Any,<:Adjoint}) = transpose($comptritype(conj(parent(parent(A)))) \ u.parent)
    @eval /(u::TransposeAbsVec, A::$tritype{<:Any,<:Transpose}) = transpose(transpose(A) \ u.parent)
end
