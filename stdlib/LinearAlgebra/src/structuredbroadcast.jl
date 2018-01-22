## Broadcast styles
import Base.Broadcast
using Base.Broadcast: DefaultArrayStyle, broadcast_similar

struct StructuredMatrixStyle{T} <: Broadcast.AbstractArrayStyle{2} end
StructuredMatrixStyle{T}(::Val{2}) where {T} = StructuredMatrixStyle{T}()
StructuredMatrixStyle{T}(::Val{N}) where {T,N} = Broadcast.DefaultArrayStyle{N}()

const StructuredMatrix = Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal,LowerTriangular,UnitLowerTriangular,UpperTriangular,UnitUpperTriangular}
Broadcast.BroadcastStyle(::Type{T}) where {T<:StructuredMatrix} = StructuredMatrixStyle{T}()

# Promotion of broadcasts between structured matrices. This is slightly unusual
# as we define them symmetrically. This allows us to have a fallback to DefaultArrayStyle{2}().
# Diagonal can cavort with all the other structured matrix types.
# Bidiagonal doesn't know if it's upper or lower, so it becomes Tridiagonal
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Diagonal}, ::StructuredMatrixStyle{<:Diagonal}) = StructuredMatrixStyle{Diagonal}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Diagonal}, ::StructuredMatrixStyle{<:Union{Bidiagonal,SymTridiagonal,Tridiagonal}}) = StructuredMatrixStyle{Tridiagonal}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Diagonal}, ::StructuredMatrixStyle{<:Union{LowerTriangular,UnitLowerTriangular}}) = StructuredMatrixStyle{LowerTriangular}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Diagonal}, ::StructuredMatrixStyle{<:Union{UpperTriangular,UnitUpperTriangular}}) = StructuredMatrixStyle{UpperTriangular}()

Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Bidiagonal}, ::StructuredMatrixStyle{<:Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal}}) = StructuredMatrixStyle{Tridiagonal}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:SymTridiagonal}, ::StructuredMatrixStyle{<:Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal}}) = StructuredMatrixStyle{Tridiagonal}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:Tridiagonal}, ::StructuredMatrixStyle{<:Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal}}) = StructuredMatrixStyle{Tridiagonal}()

Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:LowerTriangular}, ::StructuredMatrixStyle{<:Union{Diagonal,LowerTriangular,UnitLowerTriangular}}) = StructuredMatrixStyle{LowerTriangular}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:UpperTriangular}, ::StructuredMatrixStyle{<:Union{Diagonal,UpperTriangular,UnitUpperTriangular}}) = StructuredMatrixStyle{UpperTriangular}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:UnitLowerTriangular}, ::StructuredMatrixStyle{<:Union{Diagonal,LowerTriangular,UnitLowerTriangular}}) = StructuredMatrixStyle{LowerTriangular}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle{<:UnitUpperTriangular}, ::StructuredMatrixStyle{<:Union{Diagonal,UpperTriangular,UnitUpperTriangular}}) = StructuredMatrixStyle{UpperTriangular}()

# All other combinations fall back to the default style
Broadcast.BroadcastStyle(::StructuredMatrixStyle, ::StructuredMatrixStyle) = DefaultArrayStyle{2}()

# And structured matrices lose to the DefaultArrayStyle
Broadcast.BroadcastStyle(a::Broadcast.DefaultArrayStyle{Any}, ::StructuredMatrixStyle) = a
Broadcast.BroadcastStyle(a::Broadcast.DefaultArrayStyle{N}, ::StructuredMatrixStyle) where N = typeof(a)(Broadcast._max(Val(2),Val(N)))
Broadcast.BroadcastStyle(::StructuredMatrixStyle, ::Broadcast.VectorStyle) = Broadcast.DefaultArrayStyle{2}()
Broadcast.BroadcastStyle(::StructuredMatrixStyle, ::Broadcast.MatrixStyle) = Broadcast.DefaultArrayStyle{2}()

# And a definition of similar using the structured type:
structured_similar(::Type{<:Diagonal}, ::Type{ElType}, n) where {ElType} = Diagonal(Array{ElType}(uninitialized, n))
# TODO: this should be a Bidiagonal... but it doesn't know the upper/lower...
structured_similar(::Type{<:Bidiagonal}, ::Type{ElType}, n) where {ElType} = Tridiagonal(Array{ElType}(uninitialized, n-1),Array{ElType}(uninitialized, n),Array{ElType}(uninitialized, n-1))
structured_similar(::Type{<:SymTridiagonal}, ::Type{ElType}, n) where {ElType} = SymTridiagonal(Array{ElType}(uninitialized, n),Array{ElType}(uninitialized, n-1))
structured_similar(::Type{<:Tridiagonal}, ::Type{ElType}, n) where {ElType} = Tridiagonal(Array{ElType}(uninitialized, n-1),Array{ElType}(uninitialized, n),Array{ElType}(uninitialized, n-1))
structured_similar(::Type{<:LowerTriangular}, ::Type{ElType}, n) where {ElType} = LowerTriangular(Array{ElType}(uninitialized, n, n))
structured_similar(::Type{<:UpperTriangular}, ::Type{ElType}, n) where {ElType} = UpperTriangular(Array{ElType}(uninitialized, n, n))
structured_similar(::Type{<:UnitLowerTriangular}, ::Type{ElType}, n) where {ElType} = UnitLowerTriangular(Array{ElType}(uninitialized, n, n))
structured_similar(::Type{<:UnitUpperTriangular}, ::Type{ElType}, n) where {ElType} = UnitUpperTriangular(Array{ElType}(uninitialized, n, n))

# A _very_ limited list of structure-preserving functions known at compile-time
# This list is derived from the formerly-implemented `broadcast` methods in 0.6
# Note that this must preserve both zeros and ones (for Unit***erTriangular)
isstructurepreserving(::Any) = false
isstructurepreserving(bc::Broadcasted) = isstructurepreserving(bc.f, bc.args)
isstructurepreserving(::Union{typeof(abs),typeof(big)}, ::Broadcast.Args1{<:StructuredMatrix}) = true
isstructurepreserving(::Union{typeof(round),typeof(trunc),typeof(floor),typeof(ceil)}, ::Broadcast.Args1{<:StructuredMatrix}) = true
isstructurepreserving(::Union{typeof(round),typeof(trunc),typeof(floor),typeof(ceil)}, ::Broadcast.Args2{<:Type,<:StructuredMatrix}) = true
isstructurepreserving(f, args) = false

function Broadcast.broadcast_similar(::StructuredMatrixStyle{T}, ::Type{ElType}, inds, bc) where {T,ElType}
    if isstructurepreserving(bc)
        structured_similar(T, ElType, length(inds[1]))
    else
        # TODO: this formerly returned a sparse matrix
        broadcast_similar(DefaultArrayStyle{2}(), ElType, inds, bc)
    end
end

function copyto!(dest::Diagonal, bc::Broadcasted{<:StructuredMatrixStyle})
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for i in axs[1]
        dest.diag[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i))
    end
    dest
end

function copyto!(dest::Bidiagonal, bc::Broadcasted{<:StructuredMatrixStyle})
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
    dest
end

function copyto!(dest::SymTridiagonal, bc::Broadcasted{<:StructuredMatrixStyle})
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for i in axs[1]
        dest.dv[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i))
    end
    for i = 1:size(dest, 1)-1
        dest.ev[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i+1))
    end
    dest
end

function copyto!(dest::Tridiagonal, bc::Broadcasted{<:StructuredMatrixStyle})
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for i in axs[1]
        dest.d[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i))
    end
    for i = 1:size(dest, 1)-1
        dest.du[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, i+1))
        dest.dl[i] = Broadcast._broadcast_getindex(bc, CartesianIndex(i+1, i))
    end
    dest
end

function copyto!(dest::LowerTriangular, bc::Broadcasted{<:StructuredMatrixStyle})
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for j in axs[2]
        for i in j:axs[1][end]
            dest.data[i,j] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, j))
        end
    end
    dest
end

function copyto!(dest::UpperTriangular, bc::Broadcasted{<:StructuredMatrixStyle})
    axs = axes(dest)
    axes(bc) == axs || Broadcast.throwdm(axes(bc), axs)
    for j in axs[2]
        for i in 1:j
            dest.data[i,j] = Broadcast._broadcast_getindex(bc, CartesianIndex(i, j))
        end
    end
    dest
end
