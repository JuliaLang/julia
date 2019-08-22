# This file is a part of Julia. License is MIT: https://julialang.org/license

## Broadcast styles
import Base.Broadcast
using Base.Broadcast: DefaultArrayStyle, Broadcasted, tail

struct StructuredMatrixStyle{T} <: Broadcast.AbstractArrayStyle{2} end
StructuredMatrixStyle{T}(::Val{2}) where {T} = StructuredMatrixStyle{T}()
StructuredMatrixStyle{T}(::Val{N}) where {T,N} = Broadcast.DefaultArrayStyle{N}()

const StructuredMatrix = Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal,LowerTriangular,UnitLowerTriangular,UpperTriangular,UnitUpperTriangular}
Broadcast.BroadcastStyle(::Type{T}) where {T<:StructuredMatrix} = StructuredMatrixStyle{T}()

# Special support for UniformScaling — largely behave like a Diagonal, but hack into
# Broadcast to avoid computing its size.
Broadcast.BroadcastStyle(::Type{UniformScaling{T}}) where {T} = StructuredMatrixStyle{UniformScaling{T}}()
Broadcast.broadcastable(J::UniformScaling) = J
# Determining the axes is tricky — we our UniformScaling to behave like a 2D square with a size based
# upon the sizes of the other arguments.
Base.axes(bc::Broadcasted{StructuredMatrixStyle{UniformScaling{T}}}) where {T} = ensure_twod(Broadcast.combine_axes(skip_uniform_scalings(bc.args...)...))
skip_uniform_scalings(::UniformScaling, Bs...) = skip_uniform_scalings(Bs...)
skip_uniform_scalings(A, Bs...) = (A, skip_uniform_scalings(Bs...)...)
skip_uniform_scalings() = ()
ensure_twod(t::NTuple{2}) = t # TODO: ensure square
ensure_twod(t::NTuple{1}) = (t[1], t[1])
ensure_twod(t::Tuple) = throw(ArgumentError("cannot broadcast I::UniformScaling over $(length(t)) dimensions"))
Broadcast.newindex(::UniformScaling, I::CartesianIndex{2}) = I
Broadcast.check_broadcast_axes(tup, ::UniformScaling) = nothing
# And eagerly evaluate when combined with scalar expressions
function Broadcast.broadcasted(::StructuredMatrixStyle{<:UniformScaling}, f, args::Union{UniformScaling, Number, Ref}...)
    return UniformScaling(f(map(deref, args)...))
end
deref(x) = x[]
deref(J::UniformScaling) = J.λ

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

# UniformScaling "promotes" to behave like a Diagonal when in combination with any other structured matrix
Broadcast.BroadcastStyle(::StructuredMatrixStyle{UniformScaling{T}}, S::StructuredMatrixStyle) where {T} = Broadcast.BroadcastStyle(StructuredMatrixStyle{Diagonal{T}}(), S)
Broadcast.BroadcastStyle(S::StructuredMatrixStyle, ::StructuredMatrixStyle{UniformScaling{T}}) where {T} = Broadcast.BroadcastStyle(S, StructuredMatrixStyle{Diagonal{T}}())

# All other combinations fall back to the default style
Broadcast.BroadcastStyle(::StructuredMatrixStyle, ::StructuredMatrixStyle) = DefaultArrayStyle{2}()

# And a definition akin to similar using the structured type:
structured_broadcast_alloc(bc, ::Type{<:Union{UniformScaling,Diagonal}}, ::Type{ElType}, n) where {ElType} =
    Diagonal(Array{ElType}(undef, n))
# Bidiagonal is tricky as we need to know if it's upper or lower. The promotion
# system will return Tridiagonal when there's more than one Bidiagonal, but when
# there's only one, we need to make figure out upper or lower
find_bidiagonal() = throw(ArgumentError("could not find Bidiagonal within broadcast expression"))
find_bidiagonal(a::Bidiagonal, rest...) = a
find_bidiagonal(bc::Broadcast.Broadcasted, rest...) = find_bidiagonal(find_bidiagonal(bc.args...), rest...)
find_bidiagonal(x, rest...) = find_bidiagonal(rest...)
function structured_broadcast_alloc(bc, ::Type{<:Bidiagonal}, ::Type{ElType}, n) where {ElType}
    ex = find_bidiagonal(bc)
    return Bidiagonal(Array{ElType}(undef, n),Array{ElType}(undef, n-1), ex.uplo)
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

# A _very_ limited list of structure-preserving functions known at compile-time. This list is
# derived from the formerly-implemented `broadcast` methods in 0.6. Note that this must
# preserve both zeros and ones (for Unit***erTriangular) and symmetry (for SymTridiagonal)
const TypeFuncs = Union{typeof(round),typeof(trunc),typeof(floor),typeof(ceil)}
isstructurepreserving(bc::Broadcasted) = isstructurepreserving(bc.f, bc.args...)
isstructurepreserving(::Union{typeof(abs),typeof(big)}, ::StructuredMatrix) = true
isstructurepreserving(::TypeFuncs, ::StructuredMatrix) = true
isstructurepreserving(::TypeFuncs, ::Ref{<:Type}, ::StructuredMatrix) = true
isstructurepreserving(f, args...) = false

_iszero(n::Number) = iszero(n)
_iszero(x) = x == 0
fzeropreserving(bc) = (v = fzero(bc); !ismissing(v) && _iszero(v))
# Very conservatively only allow Numbers and Types in this speculative zero-test pass
fzero(x::Number) = x
fzero(::Type{T}) where T = T
fzero(S::StructuredMatrix) = zero(eltype(S))
fzero(J::UniformScaling) = zero(eltype(J))
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
