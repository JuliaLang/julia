# This file is a part of Julia. License is MIT: https://julialang.org/license

## Broadcast styles
import Base.Broadcast
using Base.Broadcast: DefaultArrayStyle, Broadcasted, tail

struct StructuredMatrixStyle{T} <: Broadcast.AbstractArrayStyle{2} end
StructuredMatrixStyle{T}(::Val{2}) where {T} = StructuredMatrixStyle{T}()
StructuredMatrixStyle{T}(::Val{N}) where {T,N} = Broadcast.DefaultArrayStyle{N}()

const StructuredMatrix = Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal,LowerTriangular,UnitLowerTriangular,UpperTriangular,UnitUpperTriangular}
Broadcast.BroadcastStyle(::Type{T}) where {T<:StructuredMatrix} = StructuredMatrixStyle{T}()

# Promotion of broadcasts between structured matrices. This is slightly unusual
# as we define them symmetrically. This allows us to have a fallback to DefaultArrayStyle{2}().
# Diagonal can cavort with all the other structured matrix types.
# Bidiagonal doesn't know if it's upper or lower, so it becomes Tridiagonal
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Diagonal}, ::StructuredMatrixStyle{<:Diagonal}) =
    StructuredMatrixStyle{Diagonal}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Diagonal}, ::StructuredMatrixStyle{<:Union{Bidiagonal,SymTridiagonal,Tridiagonal}}) =
    StructuredMatrixStyle{Tridiagonal}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Diagonal}, ::StructuredMatrixStyle{<:Union{LowerTriangular,UnitLowerTriangular}}) =
    StructuredMatrixStyle{LowerTriangular}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Diagonal}, ::StructuredMatrixStyle{<:Union{UpperTriangular,UnitUpperTriangular}}) =
    StructuredMatrixStyle{UpperTriangular}()

Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Bidiagonal}, ::StructuredMatrixStyle{<:Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal}}) =
    StructuredMatrixStyle{Tridiagonal}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:SymTridiagonal}, ::StructuredMatrixStyle{<:Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal}}) =
    StructuredMatrixStyle{Tridiagonal}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Tridiagonal}, ::StructuredMatrixStyle{<:Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal}}) =
    StructuredMatrixStyle{Tridiagonal}()

Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:LowerTriangular}, ::StructuredMatrixStyle{<:Union{Diagonal,LowerTriangular,UnitLowerTriangular}}) =
    StructuredMatrixStyle{LowerTriangular}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:UpperTriangular}, ::StructuredMatrixStyle{<:Union{Diagonal,UpperTriangular,UnitUpperTriangular}}) =
    StructuredMatrixStyle{UpperTriangular}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:UnitLowerTriangular}, ::StructuredMatrixStyle{<:Union{Diagonal,LowerTriangular,UnitLowerTriangular}}) =
    StructuredMatrixStyle{LowerTriangular}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:UnitUpperTriangular}, ::StructuredMatrixStyle{<:Union{Diagonal,UpperTriangular,UnitUpperTriangular}}) =
    StructuredMatrixStyle{UpperTriangular}()

Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Union{LowerTriangular,UnitLowerTriangular}}, ::StructuredMatrixStyle{<:Union{UpperTriangular,UnitUpperTriangular}}) =
    StructuredMatrixStyle{Matrix}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Union{UpperTriangular,UnitUpperTriangular}}, ::StructuredMatrixStyle{<:Union{LowerTriangular,UnitLowerTriangular}}) =
    StructuredMatrixStyle{Matrix}()

# Make sure that `StructuredMatrixStyle{<:Matrix}` doesn't ever end up falling
# through and give back `DefaultArrayStyle{2}`
Broadcast.BroadcastStyle(T::StructuredMatrixStyle{<:Matrix}, ::StructuredMatrixStyle) = T
Broadcast.BroadcastStyle(::StructuredMatrixStyle, T::StructuredMatrixStyle{<:Matrix}) = T
Broadcast.BroadcastStyle(T::StructuredMatrixStyle{<:Matrix}, ::StructuredMatrixStyle{<:Matrix}) = T

# All other combinations fall back to the default style
Broadcast.BroadcastStyle(::StructuredMatrixStyle, ::StructuredMatrixStyle) = DefaultArrayStyle{2}()

# And a definition akin to similar using the structured type:
structured_broadcast_alloc(bc, ::Type{<:Diagonal}, ::Type{ElType}, n) where {ElType} =
    Diagonal(Array{ElType}(undef, n))
# Bidiagonal is tricky as we need to know if it's upper or lower. The promotion
# system will return Tridiagonal when there's more than one Bidiagonal, but when
# there's only one, we need to make figure out upper or lower
merge_uplos(::Nothing, ::Nothing) = nothing
merge_uplos(a, ::Nothing) = a
merge_uplos(::Nothing, b) = b
merge_uplos(a, b) = a == b ? a : 'T'

find_uplo(a::Bidiagonal) = a.uplo
find_uplo(a) = nothing
find_uplo(bc::Broadcasted) = mapreduce(find_uplo, merge_uplos, bc.args, init=nothing)

function structured_broadcast_alloc(bc, ::Type{<:Bidiagonal}, ::Type{ElType}, n) where {ElType}
    uplo = n > 0 ? find_uplo(bc) : 'U'
    n1 = max(n - 1, 0)
    if uplo == 'T'
        return Tridiagonal(Array{ElType}(undef, n1), Array{ElType}(undef, n), Array{ElType}(undef, n1))
    end
    return Bidiagonal(Array{ElType}(undef, n),Array{ElType}(undef, n1), uplo)
end
structured_broadcast_alloc(bc, ::Type{<:SymTridiagonal}, ::Type{ElType}, n) where {ElType} =
    SymTridiagonal(Array{ElType}(undef, n),Array{ElType}(undef, n-1))
structured_broadcast_alloc(bc, ::Type{<:Tridiagonal}, ::Type{ElType}, n) where {ElType} =
    Tridiagonal(Array{ElType}(undef, n-1),Array{ElType}(undef, n),Array{ElType}(undef, n-1))
structured_broadcast_alloc(bc, ::Type{<:LowerTriangular}, ::Type{ElType}, n) where {ElType} =
    LowerTriangular(Array{ElType}(undef, n, n))
structured_broadcast_alloc(bc, ::Type{<:UpperTriangular}, ::Type{ElType}, n) where {ElType} =
    UpperTriangular(Array{ElType}(undef, n, n))
structured_broadcast_alloc(bc, ::Type{<:UnitLowerTriangular}, ::Type{ElType}, n) where {ElType} =
    UnitLowerTriangular(Array{ElType}(undef, n, n))
structured_broadcast_alloc(bc, ::Type{<:UnitUpperTriangular}, ::Type{ElType}, n) where {ElType} =
    UnitUpperTriangular(Array{ElType}(undef, n, n))
structured_broadcast_alloc(bc, ::Type{<:Matrix}, ::Type{ElType}, n) where {ElType} =
    Matrix(Array{ElType}(undef, n, n))

# A _very_ limited list of structure-preserving functions known at compile-time. This list is
# derived from the formerly-implemented `broadcast` methods in 0.6. Note that this must
# preserve both zeros and ones (for Unit***erTriangular) and symmetry (for SymTridiagonal)
const TypeFuncs = Union{typeof(round),typeof(trunc),typeof(floor),typeof(ceil)}
isstructurepreserving(bc::Broadcasted) = isstructurepreserving(bc.f, bc.args...)
isstructurepreserving(::Union{typeof(abs),typeof(big)}, ::StructuredMatrix) = true
isstructurepreserving(::TypeFuncs, ::StructuredMatrix) = true
isstructurepreserving(::TypeFuncs, ::Ref{<:Type}, ::StructuredMatrix) = true
function isstructurepreserving(::typeof(Base.literal_pow), ::Ref{typeof(^)}, ::StructuredMatrix, ::Ref{Val{N}}) where N
    return N isa Integer && N > 0
end
isstructurepreserving(f, args...) = false

"""
    iszerodefined(T::Type)

Return a `Bool` indicating whether `iszero` is well-defined for objects of type
`T`. By default, this function returns `false` unless `T <: Number`. Note that
this function may return `true` even if `zero(::T)` is not defined as long as
`iszero(::T)` has a method that does not requires `zero(::T)`.

This function is used to determine if mapping the elements of an array with
a specific structure of nonzero elements preserve this structure.
For instance, it is used to determine whether the output of
`tuple.(Diagonal([1, 2]))` is `Diagonal([(1,), (2,)])` or
`[(1,) (0,); (0,) (2,)]`. For this, we need to determine whether `(0,)` is
considered to be zero. `iszero((0,))` falls back to `(0,) == zero((0,))` which
fails as `zero(::Tuple{Int})` is not defined. However,
`iszerodefined(::Tuple{Int})` is `false` hence we falls back to the comparison
`(0,) == 0` which returns `false` and decides that the correct output is
`[(1,) (0,); (0,) (2,)]`.
"""
iszerodefined(::Type) = false
iszerodefined(::Type{<:Number}) = true

fzeropreserving(bc) = (v = fzero(bc); !ismissing(v) && (iszerodefined(typeof(v)) ? iszero(v) : v == 0))
# Like sparse matrices, we assume that the zero-preservation property of a broadcasted
# expression is stable.  We can test the zero-preservability by applying the function
# in cases where all other arguments are known scalars against a zero from the structured
# matrix. If any non-structured matrix argument is not a known scalar, we give up.
fzero(x::Number) = x
fzero(::Type{T}) where T = T
fzero(r::Ref) = r[]
fzero(t::Tuple{Any}) = t[1]
fzero(S::StructuredMatrix) = zero(eltype(S))
fzero(x) = missing
function fzero(bc::Broadcast.Broadcasted)
    args = map(fzero, bc.args)
    return any(ismissing, args) ? missing : bc.f(args...)
end

function Base.similar(bc::Broadcasted{StructuredMatrixStyle{T}}, ::Type{ElType}) where {T,ElType}
    inds = axes(bc)
    if isstructurepreserving(bc) || (fzeropreserving(bc) && !(T <: Union{SymTridiagonal,UnitLowerTriangular,UnitUpperTriangular}))
        return structured_broadcast_alloc(bc, T, ElType, length(inds[1]))
    end
    return similar(convert(Broadcasted{DefaultArrayStyle{ndims(bc)}}, bc), ElType)
end

function copyto!(dest::Diagonal, bc::Broadcasted{<:StructuredMatrixStyle})
    !isstructurepreserving(bc) && !fzeropreserving(bc) && return copyto!(dest, convert(Broadcasted{Nothing}, bc))
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for i in axs[1]
        dest.diag[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i))
    end
    return dest
end

function copyto!(dest::Bidiagonal, bc::Broadcasted{<:StructuredMatrixStyle})
    !isstructurepreserving(bc) && !fzeropreserving(bc) && return copyto!(dest, convert(Broadcasted{Nothing}, bc))
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for i in axs[1]
        dest.dv[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i))
    end
    if dest.uplo == 'U'
        for i = 1:size(dest, 1)-1
            dest.ev[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i+1))
        end
    else
        for i = 1:size(dest, 1)-1
            dest.ev[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i+1, i))
        end
    end
    return dest
end

function copyto!(dest::SymTridiagonal, bc::Broadcasted{<:StructuredMatrixStyle})
    !isstructurepreserving(bc) && !fzeropreserving(bc) && return copyto!(dest, convert(Broadcasted{Nothing}, bc))
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for i in axs[1]
        dest.dv[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i))
    end
    for i = 1:size(dest, 1)-1
        v = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i+1))
        v == Broadcast._broadcast_getindex(bc, CartesianIndex(i+1, i)) || throw(ArgumentError("broadcasted assignment breaks symmetry between locations ($i, $(i+1)) and ($(i+1), $i)"))
        dest.ev[i] = v
    end
    return dest
end

function copyto!(dest::Tridiagonal, bc::Broadcasted{<:StructuredMatrixStyle})
    !isstructurepreserving(bc) && !fzeropreserving(bc) && return copyto!(dest, convert(Broadcasted{Nothing}, bc))
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for i in axs[1]
        dest.d[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i))
    end
    for i = 1:size(dest, 1)-1
        dest.du[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i+1))
        dest.dl[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i+1, i))
    end
    return dest
end

function copyto!(dest::LowerTriangular, bc::Broadcasted{<:StructuredMatrixStyle})
    !isstructurepreserving(bc) && !fzeropreserving(bc) && return copyto!(dest, convert(Broadcasted{Nothing}, bc))
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for j in axs[2]
        for i in j:axs[1][end]
            dest.data[i,j] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, j))
        end
    end
    return dest
end

function copyto!(dest::UpperTriangular, bc::Broadcasted{<:StructuredMatrixStyle})
    !isstructurepreserving(bc) && !fzeropreserving(bc) && return copyto!(dest, convert(Broadcasted{Nothing}, bc))
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for j in axs[2]
        for i in 1:j
            dest.data[i,j] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, j))
        end
    end
    return dest
end

# We can also implement `map` and its promotion in terms of broadcast with a stricter dimension check
function map(f, A::StructuredMatrix, Bs::StructuredMatrix...)
    sz = size(A)
    all(map(B->size(B)==sz, Bs)) || throw(DimensionMismatch("dimensions must match"))
    return f.(A, Bs...)
end
