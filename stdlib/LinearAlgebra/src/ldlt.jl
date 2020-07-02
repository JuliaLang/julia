# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    LDLt <: Factorization

Matrix factorization type of the `LDLt` factorization of a real [`SymTridiagonal`](@ref)
matrix `S` such that `S = L*Diagonal(d)*L'`, where `L` is a [`UnitLowerTriangular`](@ref)
matrix and `d` is a vector. The main use of an `LDLt` factorization `F = ldlt(S)`
is to solve the linear system of equations `Sx = b` with `F\\b`. This is the
return type of [`ldlt`](@ref), the corresponding matrix factorization function.

The individual components of the factorization `F::LDLt` can be accessed via `getproperty`:

| Component | Description                                 |
|:---------:|:--------------------------------------------|
| `F.L`     | `L` (unit lower triangular) part of `LDLt`  |
| `F.D`     | `D` (diagonal) part of `LDLt`               |
| `F.Lt`    | `Lt` (unit upper triangular) part of `LDLt` |
| `F.d`     | diagonal values of `D` as a `Vector`        |

# Examples
```jldoctest
julia> S = SymTridiagonal([3., 4., 5.], [1., 2.])
3×3 SymTridiagonal{Float64,Vector{Float64}}:
 3.0  1.0   ⋅
 1.0  4.0  2.0
  ⋅   2.0  5.0

julia> F = ldlt(S)
LDLt{Float64,SymTridiagonal{Float64,Vector{Float64}}}
L factor:
3×3 UnitLowerTriangular{Float64,SymTridiagonal{Float64,Vector{Float64}}}:
 1.0        ⋅         ⋅
 0.333333  1.0        ⋅
 0.0       0.545455  1.0
D factor:
3×3 Diagonal{Float64,Vector{Float64}}:
 3.0   ⋅        ⋅
  ⋅   3.66667   ⋅
  ⋅    ⋅       3.90909
```
"""
struct LDLt{T,S<:AbstractMatrix{T}} <: Factorization{T}
    data::S

    function LDLt{T,S}(data) where {T,S<:AbstractMatrix{T}}
        require_one_based_indexing(data)
        new{T,S}(data)
    end
end
LDLt(data::AbstractMatrix{T}) where {T} = LDLt{T,typeof(data)}(data)
LDLt{T}(data::AbstractMatrix) where {T} = LDLt(convert(AbstractMatrix{T}, data)::AbstractMatrix{T})

size(S::LDLt) = size(S.data)
size(S::LDLt, i::Integer) = size(S.data, i)

LDLt{T,S}(F::LDLt{T,S}) where {T,S<:AbstractMatrix{T}} = F
LDLt{T,S}(F::LDLt) where {T,S<:AbstractMatrix{T}} = LDLt{T,S}(convert(S, F.data)::S)
LDLt{T}(F::LDLt{T}) where {T} = F
LDLt{T}(F::LDLt) where {T} = LDLt(convert(AbstractMatrix{T}, F.data)::AbstractMatrix{T})

Factorization{T}(F::LDLt{T}) where {T} = F
Factorization{T}(F::LDLt) where {T} = LDLt{T}(F)

function getproperty(F::LDLt, d::Symbol)
    Fdata = getfield(F, :data)
    if d === :d
        return Fdata.dv
    elseif d === :D
        return Diagonal(Fdata.dv)
    elseif d === :L
        return UnitLowerTriangular(Fdata)
    elseif d === :Lt
        return UnitUpperTriangular(Fdata)
    else
        return getfield(F, d)
    end
end

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::LDLt)
    summary(io, F); println(io)
    println(io, "L factor:")
    show(io, mime, F.L)
    println(io, "\nD factor:")
    show(io, mime, F.D)
end

# SymTridiagonal
"""
    ldlt!(S::SymTridiagonal) -> LDLt

Same as [`ldlt`](@ref), but saves space by overwriting the input `S`, instead of creating a copy.

# Examples
```jldoctest
julia> S = SymTridiagonal([3., 4., 5.], [1., 2.])
3×3 SymTridiagonal{Float64,Vector{Float64}}:
 3.0  1.0   ⋅
 1.0  4.0  2.0
  ⋅   2.0  5.0

julia> ldltS = ldlt!(S);

julia> ldltS === S
false

julia> S
3×3 SymTridiagonal{Float64,Vector{Float64}}:
 3.0       0.333333   ⋅
 0.333333  3.66667   0.545455
  ⋅        0.545455  3.90909
```
"""
function ldlt!(S::SymTridiagonal{T,V}) where {T,V}
    n = size(S,1)
    d = S.dv
    e = S.ev
    @inbounds @simd for i = 1:n-1
        e[i] /= d[i]
        d[i+1] -= e[i]^2*d[i]
    end
    return LDLt{T,SymTridiagonal{T,V}}(S)
end

"""
    ldlt(S::SymTridiagonal) -> LDLt

Compute an `LDLt` factorization of the real symmetric tridiagonal matrix `S` such that `S = L*Diagonal(d)*L'`
where `L` is a unit lower triangular matrix and `d` is a vector. The main use of an `LDLt`
factorization `F = ldlt(S)` is to solve the linear system of equations `Sx = b` with `F\\b`.

# Examples
```jldoctest
julia> S = SymTridiagonal([3., 4., 5.], [1., 2.])
3×3 SymTridiagonal{Float64,Vector{Float64}}:
 3.0  1.0   ⋅
 1.0  4.0  2.0
  ⋅   2.0  5.0

julia> ldltS = ldlt(S);

julia> b = [6., 7., 8.];

julia> ldltS \\ b
3-element Vector{Float64}:
 1.7906976744186047
 0.627906976744186
 1.3488372093023255

julia> S \\ b
3-element Vector{Float64}:
 1.7906976744186047
 0.627906976744186
 1.3488372093023255
```
"""
function ldlt(M::SymTridiagonal{T}; shift::Number=false) where T
    S = typeof((zero(T)+shift)/one(T))
    Mₛ = SymTridiagonal{S}(copy_oftype(M.dv, S), copy_oftype(M.ev, S))
    if !iszero(shift)
        Mₛ.dv .+= shift
    end
    return ldlt!(Mₛ)
end

factorize(S::SymTridiagonal) = ldlt(S)

function ldiv!(S::LDLt{<:Any,<:SymTridiagonal}, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    n, nrhs = size(B, 1), size(B, 2)
    if size(S,1) != n
        throw(DimensionMismatch("Matrix has dimensions $(size(S)) but right hand side has first dimension $n"))
    end
    d = S.data.dv
    l = S.data.ev
    @inbounds begin
        for i = 2:n
            li1 = l[i-1]
            @simd for j = 1:nrhs
                B[i,j] -= li1*B[i-1,j]
            end
        end
        dn = d[n]
        @simd for j = 1:nrhs
            B[n,j] /= dn
        end
        for i = n-1:-1:1
            di = d[i]
            li = l[i]
            @simd for j = 1:nrhs
                B[i,j] /= di
                B[i,j] -= li*B[i+1,j]
            end
        end
    end
    return B
end

rdiv!(B::AbstractVecOrMat, S::LDLt{<:Any,<:SymTridiagonal}) =
    transpose(ldiv!(S, transpose(B)))

function logabsdet(F::LDLt{<:Any,<:SymTridiagonal})
    it = (F.data[i,i] for i in 1:size(F, 1))
    return sum(log∘abs, it), prod(sign, it)
end

# Conversion methods
function SymTridiagonal(F::LDLt)
    e = copy(F.data.ev)
    d = copy(F.data.dv)
    e .*= d[1:end-1]
    d[2:end] += e .* F.data.ev
    SymTridiagonal(d, e)
end
AbstractMatrix(F::LDLt) = SymTridiagonal(F)
AbstractArray(F::LDLt) = AbstractMatrix(F)
Matrix(F::LDLt) = Array(AbstractArray(F))
Array(F::LDLt) = Matrix(F)
