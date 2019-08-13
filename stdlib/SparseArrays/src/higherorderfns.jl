# This file is a part of Julia. License is MIT: https://julialang.org/license

module HigherOrderFns

# This module provides higher order functions specialized for sparse arrays,
# particularly map[!]/broadcast[!] for SparseVectors and SparseMatrixCSCs at present.
import Base: map, map!, broadcast, copy, copyto!

using Base: front, tail, to_shape
using ..SparseArrays: SparseVector, SparseMatrixCSC, AbstractSparseVector,
                      AbstractSparseMatrix, AbstractSparseArray, indtype, nnz, nzrange,
                      SparseVectorUnion, AdjOrTransSparseVectorUnion, nonzeroinds, nonzeros
using Base.Broadcast: BroadcastStyle, Broadcasted, flatten
using LinearAlgebra

# This module is organized as follows:
# (0) Define BroadcastStyle rules and convenience types for dispatch
# (1) Define a common interface to SparseVectors and SparseMatrixCSCs sufficient for
#       map[!]/broadcast[!]'s purposes. The methods below are written against this interface.
# (2) Define entry points for map[!] (short children of _map_[not]zeropres!).
# (3) Define entry points for broadcast[!] (short children of _broadcast_[not]zeropres!).
# (4) Define _map_[not]zeropres! specialized for a single (input) sparse vector/matrix.
# (5) Define _map_[not]zeropres! specialized for a pair of (input) sparse vectors/matrices.
# (6) Define general _map_[not]zeropres! capable of handling >2 (input) sparse vectors/matrices.
# (7) Define _broadcast_[not]zeropres! specialized for a single (input) sparse vector/matrix.
# (8) Define _broadcast_[not]zeropres! specialized for a pair of (input) sparse vectors/matrices.
# (9) Define general _broadcast_[not]zeropres! capable of handling >2 (input) sparse vectors/matrices.
# (10) Define broadcast methods handling combinations of broadcast scalars and sparse vectors/matrices.
# (11) Define broadcast[!] methods handling combinations of scalars, sparse vectors/matrices,
#       structured matrices, and one- and two-dimensional Arrays.
# (12) Define map[!] methods handling combinations of sparse and structured matrices.


# (0) BroadcastStyle rules and convenience types for dispatch

SparseVecOrMat = Union{SparseVector,SparseMatrixCSC}

# broadcast container type promotion for combinations of sparse arrays and other types
struct SparseVecStyle <: Broadcast.AbstractArrayStyle{1} end
struct SparseMatStyle <: Broadcast.AbstractArrayStyle{2} end
Broadcast.BroadcastStyle(::Type{<:SparseVector}) = SparseVecStyle()
Broadcast.BroadcastStyle(::Type{<:SparseMatrixCSC}) = SparseMatStyle()
const SPVM = Union{SparseVecStyle,SparseMatStyle}

# SparseVecStyle handles 0-1 dimensions, SparseMatStyle 0-2 dimensions.
# SparseVecStyle promotes to SparseMatStyle for 2 dimensions.
# Fall back to DefaultArrayStyle for higher dimensionality.
SparseVecStyle(::Val{0}) = SparseVecStyle()
SparseVecStyle(::Val{1}) = SparseVecStyle()
SparseVecStyle(::Val{2}) = SparseMatStyle()
SparseVecStyle(::Val{N}) where N = Broadcast.DefaultArrayStyle{N}()
SparseMatStyle(::Val{0}) = SparseMatStyle()
SparseMatStyle(::Val{1}) = SparseMatStyle()
SparseMatStyle(::Val{2}) = SparseMatStyle()
SparseMatStyle(::Val{N}) where N = Broadcast.DefaultArrayStyle{N}()

Broadcast.BroadcastStyle(::SparseMatStyle, ::SparseVecStyle) = SparseMatStyle()

# Tuples promote to dense
Broadcast.BroadcastStyle(::SparseVecStyle, ::Broadcast.Style{Tuple}) = Broadcast.DefaultArrayStyle{1}()
Broadcast.BroadcastStyle(::SparseMatStyle, ::Broadcast.Style{Tuple}) = Broadcast.DefaultArrayStyle{2}()

struct PromoteToSparse <: Broadcast.AbstractArrayStyle{2} end
PromoteToSparse(::Val{0}) = PromoteToSparse()
PromoteToSparse(::Val{1}) = PromoteToSparse()
PromoteToSparse(::Val{2}) = PromoteToSparse()
PromoteToSparse(::Val{N}) where N = Broadcast.DefaultArrayStyle{N}()

const StructuredMatrix = Union{Diagonal,Bidiagonal,Tridiagonal,SymTridiagonal}
Broadcast.BroadcastStyle(::Type{<:Adjoint{T,<:Union{SparseVector,SparseMatrixCSC}} where T}) = PromoteToSparse()
Broadcast.BroadcastStyle(::Type{<:Transpose{T,<:Union{SparseVector,SparseMatrixCSC}} where T}) = PromoteToSparse()

Broadcast.BroadcastStyle(s::SPVM, ::Broadcast.AbstractArrayStyle{0}) = s
Broadcast.BroadcastStyle(s::SPVM, ::Broadcast.DefaultArrayStyle{0}) = s
Broadcast.BroadcastStyle(::SPVM, ::Broadcast.DefaultArrayStyle{1}) = PromoteToSparse()
Broadcast.BroadcastStyle(::SPVM, ::Broadcast.DefaultArrayStyle{2}) = PromoteToSparse()

Broadcast.BroadcastStyle(::SPVM, ::LinearAlgebra.StructuredMatrixStyle{<:StructuredMatrix}) = PromoteToSparse()
Broadcast.BroadcastStyle(::PromoteToSparse, ::LinearAlgebra.StructuredMatrixStyle{<:StructuredMatrix}) = PromoteToSparse()

Broadcast.BroadcastStyle(::PromoteToSparse, ::SPVM) = PromoteToSparse()
Broadcast.BroadcastStyle(::PromoteToSparse, ::Broadcast.Style{Tuple}) = Broadcast.DefaultArrayStyle{2}()

# FIXME: currently sparse broadcasts are only well-tested on known array types, while any AbstractArray
# could report itself as a DefaultArrayStyle().
# See https://github.com/JuliaLang/julia/pull/23939#pullrequestreview-72075382 for more details
is_supported_sparse_broadcast() = true
is_supported_sparse_broadcast(::AbstractArray, rest...) = false
is_supported_sparse_broadcast(::AbstractSparseArray, rest...) = is_supported_sparse_broadcast(rest...)
is_supported_sparse_broadcast(::StructuredMatrix, rest...) = is_supported_sparse_broadcast(rest...)
is_supported_sparse_broadcast(::Array, rest...) = is_supported_sparse_broadcast(rest...)
is_supported_sparse_broadcast(t::Union{Transpose, Adjoint}, rest...) = is_supported_sparse_broadcast(t.parent, rest...)
is_supported_sparse_broadcast(x, rest...) = axes(x) === () && is_supported_sparse_broadcast(rest...)
is_supported_sparse_broadcast(x::Ref, rest...) = is_supported_sparse_broadcast(rest...)

can_skip_sparsification(f, rest...) = false
can_skip_sparsification(::typeof(*), ::SparseVectorUnion, ::AdjOrTransSparseVectorUnion) = true

# Dispatch on broadcast operations by number of arguments
const Broadcasted0{Style<:Union{Nothing,BroadcastStyle},Axes,F} =
    Broadcasted{Style,Axes,F,Tuple{}}
const SpBroadcasted1{Style<:SPVM,Axes,F,Args<:Tuple{SparseVecOrMat}} =
    Broadcasted{Style,Axes,F,Args}
const SpBroadcasted2{Style<:SPVM,Axes,F,Args<:Tuple{SparseVecOrMat,SparseVecOrMat}} =
    Broadcasted{Style,Axes,F,Args}

# (1) The definitions below provide a common interface to sparse vectors and matrices
# sufficient for the purposes of map[!]/broadcast[!]. This interface treats sparse vectors
# as n-by-one sparse matrices which, though technically incorrect, is how broacast[!] views
# sparse vectors in practice.
@inline numrows(A::SparseVector) = A.n
@inline numrows(A::SparseMatrixCSC) = A.m
@inline numcols(A::SparseVector) = 1
@inline numcols(A::SparseMatrixCSC) = A.n
# numrows and numcols respectively yield size(A, 1) and size(A, 2), but avoid a branch
@inline columns(A::SparseVector) = 1
@inline columns(A::SparseMatrixCSC) = 1:A.n
@inline colrange(A::SparseVector, j) = 1:length(A.nzind)
@inline colrange(A::SparseMatrixCSC, j) = nzrange(A, j)
@inline colstartind(A::SparseVector, j) = one(indtype(A))
@inline colboundind(A::SparseVector, j) = convert(indtype(A), length(A.nzind) + 1)
@inline colstartind(A::SparseMatrixCSC, j) = A.colptr[j]
@inline colboundind(A::SparseMatrixCSC, j) = A.colptr[j + 1]
@inline storedinds(A::SparseVector) = A.nzind
@inline storedinds(A::SparseMatrixCSC) = A.rowval
@inline storedvals(A::SparseVecOrMat) = A.nzval
@inline setcolptr!(A::SparseVector, j, val) = val
@inline setcolptr!(A::SparseMatrixCSC, j, val) = A.colptr[j] = val
function trimstorage!(A::SparseVecOrMat, maxstored)
    resize!(storedinds(A), maxstored)
    resize!(storedvals(A), maxstored)
    return maxstored
end
function expandstorage!(A::SparseVecOrMat, maxstored)
    length(storedinds(A)) < maxstored && resize!(storedinds(A), maxstored)
    length(storedvals(A)) < maxstored && resize!(storedvals(A), maxstored)
    return maxstored
end


# (2) map[!] entry points
map(f::Tf, A::SparseVector) where {Tf} = _noshapecheck_map(f, A)
map(f::Tf, A::SparseMatrixCSC) where {Tf} = _noshapecheck_map(f, A)
map(f::Tf, A::SparseMatrixCSC, Bs::Vararg{SparseMatrixCSC,N}) where {Tf,N} =
    (_checksameshape(A, Bs...); _noshapecheck_map(f, A, Bs...))
map(f::Tf, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N}) where {Tf,N} =
    (_checksameshape(A, Bs...); _noshapecheck_map(f, A, Bs...))
map!(f::Tf, C::SparseMatrixCSC, A::SparseMatrixCSC, Bs::Vararg{SparseMatrixCSC,N}) where {Tf,N} =
    (_checksameshape(C, A, Bs...); _noshapecheck_map!(f, C, A, Bs...))
map!(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N}) where {Tf,N} =
    (_checksameshape(C, A, Bs...); _noshapecheck_map!(f, C, A, Bs...))
function _noshapecheck_map!(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N}) where {Tf,N}
    fofzeros = f(_zeros_eltypes(A, Bs...)...)
    fpreszeros = _iszero(fofzeros)
    return fpreszeros ? _map_zeropres!(f, C, A, Bs...) :
                        _map_notzeropres!(f, fofzeros, C, A, Bs...)
end
function _noshapecheck_map(f::Tf, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N}) where {Tf,N}
    fofzeros = f(_zeros_eltypes(A, Bs...)...)
    fpreszeros = _iszero(fofzeros)
    maxnnzC = fpreszeros ? min(length(A), _sumnnzs(A, Bs...)) : length(A)
    entrytypeC = Base.Broadcast.combine_eltypes(f, (A, Bs...))
    indextypeC = _promote_indtype(A, Bs...)
    C = _allocres(size(A), indextypeC, entrytypeC, maxnnzC)
    return fpreszeros ? _map_zeropres!(f, C, A, Bs...) :
                        _map_notzeropres!(f, fofzeros, C, A, Bs...)
end
# (3) broadcast[!] entry points
copy(bc::SpBroadcasted1) = _noshapecheck_map(bc.f, bc.args[1])

@inline function copyto!(C::SparseVecOrMat, bc::Broadcasted0{Nothing})
    isempty(C) && return _finishempty!(C)
    f = bc.f
    fofnoargs = f()
    if _iszero(fofnoargs) # f() is zero, so empty C
        trimstorage!(C, 0)
        _finishempty!(C)
    else # f() is nonzero, so densify C and fill with independent calls to f()
        _densestructure!(C)
        storedvals(C)[1] = fofnoargs
        broadcast!(f, view(storedvals(C), 2:length(storedvals(C))))
    end
    return C
end

function _diffshape_broadcast(f::Tf, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N}) where {Tf,N}
    fofzeros = f(_zeros_eltypes(A, Bs...)...)
    fpreszeros = _iszero(fofzeros)
    indextypeC = _promote_indtype(A, Bs...)
    entrytypeC = Base.Broadcast.combine_eltypes(f, (A, Bs...))
    shapeC = to_shape(Base.Broadcast.combine_axes(A, Bs...))
    maxnnzC = fpreszeros ? _checked_maxnnzbcres(shapeC, A, Bs...) : _densennz(shapeC)
    C = _allocres(shapeC, indextypeC, entrytypeC, maxnnzC)
    return fpreszeros ? _broadcast_zeropres!(f, C, A, Bs...) :
                        _broadcast_notzeropres!(f, fofzeros, C, A, Bs...)
end
# helper functions for map[!]/broadcast[!] entry points (and related methods below)
@inline _sumnnzs(A) = nnz(A)
@inline _sumnnzs(A, Bs...) = nnz(A) + _sumnnzs(Bs...)
@inline _iszero(x) = x == 0
@inline _iszero(x::Number) = Base.iszero(x)
@inline _iszero(x::AbstractArray) = Base.iszero(x)
@inline _zeros_eltypes(A) = (zero(eltype(A)),)
@inline _zeros_eltypes(A, Bs...) = (zero(eltype(A)), _zeros_eltypes(Bs...)...)
@inline _promote_indtype(A) = indtype(A)
@inline _promote_indtype(A, Bs...) = promote_type(indtype(A), _promote_indtype(Bs...))
@inline _aresameshape(A) = true
@inline _aresameshape(A, B) = size(A) == size(B)
@inline _aresameshape(A, B, Cs...) = _aresameshape(A, B) ? _aresameshape(B, Cs...) : false
@inline _checksameshape(As...) = _aresameshape(As...) || throw(DimensionMismatch("argument shapes must match"))
@inline _all_args_isa(t::Tuple{Any}, ::Type{T}) where T = isa(t[1], T)
@inline _all_args_isa(t::Tuple{Any,Vararg{Any}}, ::Type{T}) where T = isa(t[1], T) & _all_args_isa(tail(t), T)
@inline _all_args_isa(t::Tuple{Broadcasted}, ::Type{T}) where T = _all_args_isa(t[1].args, T)
@inline _all_args_isa(t::Tuple{Broadcasted,Vararg{Any}}, ::Type{T}) where T = _all_args_isa(t[1].args, T) & _all_args_isa(tail(t), T)
@inline _densennz(shape::NTuple{1}) = shape[1]
@inline _densennz(shape::NTuple{2}) = shape[1] * shape[2]
_maxnnzfrom(shape::NTuple{1}, A) = nnz(A) * div(shape[1], A.n)
_maxnnzfrom(shape::NTuple{2}, A::SparseVector) = nnz(A) * div(shape[1], A.n) * shape[2]
_maxnnzfrom(shape::NTuple{2}, A::SparseMatrixCSC) = nnz(A) * div(shape[1], A.m) * div(shape[2], A.n)
@inline _maxnnzfrom_each(shape, ::Tuple{}) = ()
@inline _maxnnzfrom_each(shape, As) = (_maxnnzfrom(shape, first(As)), _maxnnzfrom_each(shape, tail(As))...)
@inline _unchecked_maxnnzbcres(shape, As::Tuple) = min(_densennz(shape), sum(_maxnnzfrom_each(shape, As)))
@inline _unchecked_maxnnzbcres(shape, As...) = _unchecked_maxnnzbcres(shape, As)
@inline _checked_maxnnzbcres(shape::NTuple{1}, As...) = shape[1] != 0 ? _unchecked_maxnnzbcres(shape, As) : 0
@inline _checked_maxnnzbcres(shape::NTuple{2}, As...) = shape[1] != 0 && shape[2] != 0 ? _unchecked_maxnnzbcres(shape, As) : 0
@inline function _allocres(shape::NTuple{1}, indextype, entrytype, maxnnz)
    storedinds = Vector{indextype}(undef, maxnnz)
    storedvals = Vector{entrytype}(undef, maxnnz)
    return SparseVector(shape..., storedinds, storedvals)
end
@inline function _allocres(shape::NTuple{2}, indextype, entrytype, maxnnz)
    pointers = ones(indextype, shape[2] + 1)
    storedinds = Vector{indextype}(undef, maxnnz)
    storedvals = Vector{entrytype}(undef, maxnnz)
    return SparseMatrixCSC(shape..., pointers, storedinds, storedvals)
end

# (4) _map_zeropres!/_map_notzeropres! specialized for a single sparse vector/matrix
"Stores only the nonzero entries of `map(f, Array(A))` in `C`."
function _map_zeropres!(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat) where Tf
    spaceC::Int = min(length(storedinds(C)), length(storedvals(C)))
    Ck = 1
    @inbounds for j in columns(C)
        setcolptr!(C, j, Ck)
        for Ak in colrange(A, j)
            Cx = f(storedvals(A)[Ak])
            if !_iszero(Cx)
                Ck > spaceC && (spaceC = expandstorage!(C, Ck + nnz(A) - (Ak - 1)))
                storedinds(C)[Ck] = storedinds(A)[Ak]
                storedvals(C)[Ck] = Cx
                Ck += 1
            end
        end
    end
    @inbounds setcolptr!(C, numcols(C) + 1, Ck)
    trimstorage!(C, Ck - 1)
    return C
end
"""
Densifies `C`, storing `fillvalue` in place of each unstored entry in `A` and
`f(A[i])`/`f(A[i,j])` in place of each stored entry `A[i]`/`A[i,j]` in `A`.
"""
function _map_notzeropres!(f::Tf, fillvalue, C::SparseVecOrMat, A::SparseVecOrMat) where Tf
    # Build dense matrix structure in C, expanding storage if necessary
    _densestructure!(C)
    # Populate values
    fill!(storedvals(C), fillvalue)
    @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
        for Ak in colrange(A, j)
            Cx = f(storedvals(A)[Ak])
            Cx != fillvalue && (storedvals(C)[jo + storedinds(A)[Ak]] = Cx)
        end
    end
    # NOTE: Combining the fill! above into the loop above to avoid multiple sweeps over /
    # nonsequential access of storedvals(C) does not appear to improve performance.
    return C
end
# helper functions for these methods and some of those below
@inline _densecoloffsets(A::SparseVector) = 0
@inline _densecoloffsets(A::SparseMatrixCSC) = 0:A.m:(A.m*(A.n - 1))
function _densestructure!(A::SparseVector)
    expandstorage!(A, A.n)
    copyto!(A.nzind, 1:A.n)
    return A
end
function _densestructure!(A::SparseMatrixCSC)
    nnzA = A.m * A.n
    expandstorage!(A, nnzA)
    copyto!(A.colptr, 1:A.m:(nnzA + 1))
    for k in _densecoloffsets(A)
        copyto!(A.rowval, k + 1, 1:A.m)
    end
    return A
end


# (5) _map_zeropres!/_map_notzeropres! specialized for a pair of sparse vectors/matrices
function _map_zeropres!(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat, B::SparseVecOrMat) where Tf
    spaceC::Int = min(length(storedinds(C)), length(storedvals(C)))
    rowsentinelA = convert(indtype(A), numrows(C) + 1)
    rowsentinelB = convert(indtype(B), numrows(C) + 1)
    Ck = 1
    @inbounds for j in columns(C)
        setcolptr!(C, j, Ck)
        Ak, stopAk = colstartind(A, j), colboundind(A, j)
        Bk, stopBk = colstartind(B, j), colboundind(B, j)
        Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
        Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
        while true
            if Ai == Bi
                Ai == rowsentinelA && break # column complete
                Cx, Ci::indtype(C) = f(storedvals(A)[Ak], storedvals(B)[Bk]), Ai
                Ak += oneunit(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                Bk += oneunit(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
            elseif Ai < Bi
                Cx, Ci = f(storedvals(A)[Ak], zero(eltype(B))), Ai
                Ak += oneunit(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
            else # Bi < Ai
                Cx, Ci = f(zero(eltype(A)), storedvals(B)[Bk]), Bi
                Bk += oneunit(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
            end
            # NOTE: The ordering of the conditional chain above impacts which matrices this
            # method performs best for. In the map situation (arguments have same shape, and
            # likely same or similar stored entry pattern), the Ai == Bi and termination
            # cases are equally or more likely than the Ai < Bi and Bi < Ai cases. Hence
            # the ordering of the conditional chain above differs from that in the
            # corresponding broadcast code (below).
            if !_iszero(Cx)
                Ck > spaceC && (spaceC = expandstorage!(C, Ck + (nnz(A) - (Ak - 1)) + (nnz(B) - (Bk - 1))))
                storedinds(C)[Ck] = Ci
                storedvals(C)[Ck] = Cx
                Ck += 1
            end
        end
    end
    @inbounds setcolptr!(C, numcols(C) + 1, Ck)
    trimstorage!(C, Ck - 1)
    return C
end
function _map_notzeropres!(f::Tf, fillvalue, C::SparseVecOrMat, A::SparseVecOrMat, B::SparseVecOrMat) where Tf
    # Build dense matrix structure in C, expanding storage if necessary
    _densestructure!(C)
    # Populate values
    fill!(storedvals(C), fillvalue)
    # NOTE: Combining this fill! into the loop below to avoid multiple sweeps over /
    # nonsequential access of storedvals(C) does not appear to improve performance.
    rowsentinelA = convert(indtype(A), numrows(A) + 1)
    rowsentinelB = convert(indtype(B), numrows(B) + 1)
    @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
        Ak, stopAk = colstartind(A, j), colboundind(A, j)
        Bk, stopBk = colstartind(B, j), colboundind(B, j)
        Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
        Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
        while true
            if Ai == Bi
                Ai == rowsentinelA && break # column complete
                Cx, Ci::indtype(C) = f(storedvals(A)[Ak], storedvals(B)[Bk]), Ai
                Ak += oneunit(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                Bk += oneunit(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
            elseif Ai < Bi
                Cx, Ci = f(storedvals(A)[Ak], zero(eltype(B))), Ai
                Ak += oneunit(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
            else # Bi < Ai
                Cx, Ci = f(zero(eltype(A)), storedvals(B)[Bk]), Bi
                Bk += oneunit(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
            end
            Cx != fillvalue && (storedvals(C)[jo + Ci] = Cx)
        end
    end
    return C
end


# (6) _map_zeropres!/_map_notzeropres! for more than two sparse matrices / vectors
function _map_zeropres!(f::Tf, C::SparseVecOrMat, As::Vararg{SparseVecOrMat,N}) where {Tf,N}
    spaceC::Int = min(length(storedinds(C)), length(storedvals(C)))
    rowsentinel = numrows(C) + 1
    Ck = 1
    stopks = _colstartind_all(1, As)
    @inbounds for j in columns(C)
        setcolptr!(C, j, Ck)
        ks = stopks
        stopks = _colboundind_all(j, As)
        rows = _rowforind_all(rowsentinel, ks, stopks, As)
        activerow = min(rows...)
        while activerow < rowsentinel
            vals, ks, rows = _fusedupdate_all(rowsentinel, activerow, rows, ks, stopks, As)
            Cx = f(vals...)
            if !_iszero(Cx)
                Ck > spaceC && (spaceC = expandstorage!(C, min(length(C), Ck + _sumnnzs(As...) - (sum(ks) - N))))
                storedinds(C)[Ck] = activerow
                storedvals(C)[Ck] = Cx
                Ck += 1
            end
            activerow = min(rows...)
        end
    end
    @inbounds setcolptr!(C, numcols(C) + 1, Ck)
    trimstorage!(C, Ck - 1)
    return C
end
function _map_notzeropres!(f::Tf, fillvalue, C::SparseVecOrMat, As::Vararg{SparseVecOrMat,N}) where {Tf,N}
    # Build dense matrix structure in C, expanding storage if necessary
    _densestructure!(C)
    # Populate values
    fill!(storedvals(C), fillvalue)
    # NOTE: Combining this fill! into the loop below to avoid multiple sweeps over /
    # nonsequential access of C.nzval does not appear to improve performance.
    rowsentinel = numrows(C) + 1
    stopks = _colstartind_all(1, As)
    @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
        ks = stopks
        stopks = _colboundind_all(j, As)
        rows = _rowforind_all(rowsentinel, ks, stopks, As)
        activerow = min(rows...)
        while activerow < rowsentinel
            vals, ks, rows = _fusedupdate_all(rowsentinel, activerow, rows, ks, stopks, As)
            Cx = f(vals...)
            Cx != fillvalue && (storedvals(C)[jo + activerow] = Cx)
            activerow = min(rows...)
        end
    end
    return C
end

# helper methods for map/map! methods just above
@inline _colstartind(j, A) = colstartind(A, j)
@inline _colstartind_all(j, ::Tuple{}) = ()
@inline _colstartind_all(j, As) = (
    _colstartind(j, first(As)),
    _colstartind_all(j, tail(As))...)
@inline _colboundind(j, A) = colboundind(A, j)
@inline _colboundind_all(j, ::Tuple{}) = ()
@inline _colboundind_all(j, As) = (
    _colboundind(j, first(As)),
    _colboundind_all(j, tail(As))...)
@inline _rowforind(rowsentinel, k, stopk, A) =
    k < stopk ? storedinds(A)[k] : convert(indtype(A), rowsentinel)
@inline _rowforind_all(rowsentinel, ::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
@inline _rowforind_all(rowsentinel, ks, stopks, As) = (
    _rowforind(rowsentinel, first(ks), first(stopks), first(As)),
    _rowforind_all(rowsentinel, tail(ks), tail(stopks), tail(As))...)

@inline function _fusedupdate(rowsentinel, activerow, row, k, stopk, A)
    # returns (val, nextk, nextrow)
    if row == activerow
        nextk = k + oneunit(k)
        (storedvals(A)[k], nextk, (nextk < stopk ? storedinds(A)[nextk] : oftype(row, rowsentinel)))
    else
        (zero(eltype(A)), k, row)
    end
end
@inline _fusedupdate_all(rowsentinel, activerow, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}) = ((#=vals=#), (#=nextks=#), (#=nextrows=#))
@inline function _fusedupdate_all(rowsentinel, activerow, rows, ks, stopks, As)
    val, nextk, nextrow = _fusedupdate(rowsentinel, activerow, first(rows), first(ks), first(stopks), first(As))
    vals, nextks, nextrows = _fusedupdate_all(rowsentinel, activerow, tail(rows), tail(ks), tail(stopks), tail(As))
    return ((val, vals...), (nextk, nextks...), (nextrow, nextrows...))
end


# (7) _broadcast_zeropres!/_broadcast_notzeropres! specialized for a single (input) sparse vector/matrix
function _broadcast_zeropres!(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat) where Tf
    isempty(C) && return _finishempty!(C)
    spaceC::Int = min(length(storedinds(C)), length(storedvals(C)))
    # C and A cannot have the same shape, as we directed that case to map in broadcast's
    # entry point; here we need efficiently handle only heterogeneous C-A combinations where
    # one or both of C and A has at least one singleton dimension.
    #
    # We first divide the cases into two groups: those in which the input argument does not
    # expand vertically, and those in which the input argument expands vertically.
    #
    # Cases without vertical expansion
    Ck = 1
    if numrows(A) == numrows(C)
        @inbounds for j in columns(C)
            setcolptr!(C, j, Ck)
            bccolrangejA = numcols(A) == 1 ? colrange(A, 1) : colrange(A, j)
            for Ak in bccolrangejA
                Cx = f(storedvals(A)[Ak])
                if !_iszero(Cx)
                    Ck > spaceC && (spaceC = expandstorage!(C, _unchecked_maxnnzbcres(size(C), A)))
                    storedinds(C)[Ck] = storedinds(A)[Ak]
                    storedvals(C)[Ck] = Cx
                    Ck += 1
                end
            end
        end
    # Cases with vertical expansion
    else # numrows(A) != numrows(C) (=> numrows(A) == 1)
        @inbounds for j in columns(C)
            setcolptr!(C, j, Ck)
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Ax = Ak < stopAk ? storedvals(A)[Ak] : zero(eltype(A))
            fofAx = f(Ax)
            # if fofAx is zero, then either A's jth column is empty, or A's jth column
            # contains a nonzero value x but f(Ax) is nonetheless zero, so we need store
            # nothing in C's jth column. if to the contrary fofAx is nonzero, then we must
            # densely populate C's jth column with fofAx.
            if !_iszero(fofAx)
                for Ci::indtype(C) in 1:numrows(C)
                    Ck > spaceC && (spaceC = expandstorage!(C, _unchecked_maxnnzbcres(size(C), A)))
                    storedinds(C)[Ck] = Ci
                    storedvals(C)[Ck] = fofAx
                    Ck += 1
                end
            end
        end
    end
    @inbounds setcolptr!(C, numcols(C) + 1, Ck)
    trimstorage!(C, Ck - 1)
    return C
end
function _broadcast_notzeropres!(f::Tf, fillvalue, C::SparseVecOrMat, A::SparseVecOrMat) where Tf
    # For information on this code, see comments in similar code in _broadcast_zeropres! above
    # Build dense matrix structure in C, expanding storage if necessary
    _densestructure!(C)
    # Populate values
    fill!(storedvals(C), fillvalue)
    # Cases without vertical expansion
    if numrows(A) == numrows(C)
        @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
            bccolrangejA = numcols(A) == 1 ? colrange(A, 1) : colrange(A, j)
            for Ak in bccolrangejA
                Cx, Ci = f(storedvals(A)[Ak]), storedinds(A)[Ak]
                Cx != fillvalue && (storedvals(C)[jo + Ci] = Cx)
            end
        end
    # Cases with vertical expansion
    else # numrows(A) != numrows(C) (=> numrows(A) == 1)
        svA, svC = storedvals(A), storedvals(C)
        @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Ax = Ak < stopAk ? svA[Ak] : zero(eltype(A))
            fofAx = f(Ax)
            if fofAx != fillvalue
                for i in (jo + 1):(jo + numrows(C))
                    svC[i] = fofAx
                end
            end
        end
    end
    return C
end


# (8) _broadcast_zeropres!/_broadcast_notzeropres! specialized for a pair of (input) sparse vectors/matrices
function _broadcast_zeropres!(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat, B::SparseVecOrMat) where Tf
    isempty(C) && return _finishempty!(C)
    spaceC::Int = min(length(storedinds(C)), length(storedvals(C)))
    rowsentinelA = convert(indtype(A), numrows(C) + 1)
    rowsentinelB = convert(indtype(B), numrows(C) + 1)
    # C, A, and B cannot all have the same shape, as we directed that case to map in broadcast's
    # entry point; here we need efficiently handle only heterogeneous combinations of mats/vecs
    # with no singleton dimensions, one singleton dimension, and two singleton dimensions.
    # Cases involving objects with two singleton dimensions should be rare and optimizing
    # that case complicates the code appreciably, so we largely ignore that case's
    # performance below.
    #
    # We first divide the cases into two groups: those in which neither input argument
    # expands vertically, and those in which at least one argument expands vertically.
    #
    # NOTE: Placing the loops over columns outside the conditional chain segregating
    # argument shape combinations eliminates some code replication but unfortunately
    # hurts performance appreciably in some cases.
    #
    # Cases without vertical expansion
    Ck = 1
    if numrows(A) == numrows(B) == numrows(C)
        @inbounds for j in columns(C)
            setcolptr!(C, j, Ck)
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            # Restructuring this k/stopk code to avoid unnecessary colptr retrievals does
            # not improve performance signicantly. Leave in this less complex form.
            Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
            Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
            while true
                if Ai != Bi
                    if Ai < Bi
                        Cx, Ci = f(storedvals(A)[Ak], zero(eltype(B))), Ai
                        Ak += oneunit(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                    else # Ai > Bi
                        Cx, Ci = f(zero(eltype(A)), storedvals(B)[Bk]), Bi
                        Bk += oneunit(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                    end
                elseif #= Ai == Bi && =# Ai == rowsentinelA
                    break # column complete
                else #= Ai == Bi != rowsentinel =#
                    Cx, Ci::indtype(C) = f(storedvals(A)[Ak], storedvals(B)[Bk]), Ai
                    Ak += oneunit(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                    Bk += oneunit(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                end
                # NOTE: The ordering of the conditional chain above impacts which matrices
                # this method perform best for. In contrast to the map situation (arguments
                # have same shape, and likely same or similar stored entry pattern), where
                # the Ai == Bi and termination cases are equally or more likely than the
                # Ai < Bi and Bi < Ai cases, in the broadcast situation (arguments have
                # different shape, and likely largely disjoint expanded stored entry
                # pattern) the Ai < Bi and Bi < Ai cases are equally or more likely than the
                # Ai == Bi and termination cases. Hence the ordering of the conditional
                # chain above differs from that in the corresponding map code.
                if !_iszero(Cx)
                    Ck > spaceC && (spaceC = expandstorage!(C, _unchecked_maxnnzbcres(size(C), A, B)))
                    storedinds(C)[Ck] = Ci
                    storedvals(C)[Ck] = Cx
                    Ck += 1
                end
            end
        end
    # Cases with vertical expansion
    elseif numrows(A) == numrows(B) == 1 # && numrows(C) != 1, vertically expand both A and B
        @inbounds for j in columns(C)
            setcolptr!(C, j, Ck)
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Ax = Ak < stopAk ? storedvals(A)[Ak] : zero(eltype(A))
            Bx = Bk < stopBk ? storedvals(B)[Bk] : zero(eltype(B))
            Cx = f(Ax, Bx)
            if !_iszero(Cx)
                for Ci::indtype(C) in 1:numrows(C)
                    Ck > spaceC && (spaceC = expandstorage!(C, _unchecked_maxnnzbcres(size(C), A, B)))
                    storedinds(C)[Ck] = Ci
                    storedvals(C)[Ck] = Cx
                    Ck += 1
                end
            end
        end
    elseif numrows(A) == 1 # && numrows(B) == numrows(C) != 1 , vertically expand only A
        @inbounds for j in columns(C)
            setcolptr!(C, j, Ck)
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Ax = Ak < stopAk ? storedvals(A)[Ak] : zero(eltype(A))
            fvAzB = f(Ax, zero(eltype(B)))
            if _iszero(fvAzB)
                # either A's jth column is empty, or A's jth column contains a nonzero value
                # Ax but f(Ax, zero(eltype(B))) is nonetheless zero, so we can scan through
                # B's jth column without storing every entry in C's jth column
                while Bk < stopBk
                    Cx = f(Ax, storedvals(B)[Bk])
                    if !_iszero(Cx)
                        Ck > spaceC && (spaceC = expandstorage!(C, _unchecked_maxnnzbcres(size(C), A, B)))
                        storedinds(C)[Ck] = storedinds(B)[Bk]
                        storedvals(C)[Ck] = Cx
                        Ck += 1
                    end
                    Bk += oneunit(Bk)
                end
            else
                # A's jth column is nonempty and f(Ax, zero(eltype(B))) is not zero, so
                # we must store (likely) every entry in C's jth column
                Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                for Ci::indtype(C) in 1:numrows(C)
                    if Bi == Ci
                        Cx = f(Ax, storedvals(B)[Bk])
                        Bk += oneunit(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                    else
                        Cx = fvAzB
                    end
                    if !_iszero(Cx)
                        Ck > spaceC && (spaceC = expandstorage!(C, _unchecked_maxnnzbcres(size(C), A, B)))
                        storedinds(C)[Ck] = Ci
                        storedvals(C)[Ck] = Cx
                        Ck += 1
                    end
                end
            end
        end
    else # numrows(B) == 1 && numrows(A) == numrows(C) != 1, vertically expand only B
        @inbounds for j in columns(C)
            setcolptr!(C, j, Ck)
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Bx = Bk < stopBk ? storedvals(B)[Bk] : zero(eltype(B))
            fzAvB = f(zero(eltype(A)), Bx)
            if _iszero(fzAvB)
                # either B's jth column is empty, or B's jth column contains a nonzero value
                # Bx but f(zero(eltype(A)), Bx) is nonetheless zero, so we can scan through
                # A's jth column without storing every entry in C's jth column
                while Ak < stopAk
                    Cx = f(storedvals(A)[Ak], Bx)
                    if !_iszero(Cx)
                        Ck > spaceC && (spaceC = expandstorage!(C, _unchecked_maxnnzbcres(size(C), A, B)))
                        storedinds(C)[Ck] = storedinds(A)[Ak]
                        storedvals(C)[Ck] = Cx
                        Ck += 1
                    end
                    Ak += oneunit(Ak)
                end
            else
                # B's jth column is nonempty and f(zero(eltype(A)), Bx) is not zero, so
                # we must store (likely) every entry in C's jth column
                Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                for Ci::indtype(C) in 1:numrows(C)
                    if Ai == Ci
                        Cx = f(storedvals(A)[Ak], Bx)
                        Ak += oneunit(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                    else
                        Cx = fzAvB
                    end
                    if !_iszero(Cx)
                        Ck > spaceC && (spaceC = expandstorage!(C, _unchecked_maxnnzbcres(size(C), A, B)))
                        storedinds(C)[Ck] = Ci
                        storedvals(C)[Ck] = Cx
                        Ck += 1
                    end
                end
            end
        end
    end
    @inbounds setcolptr!(C, numcols(C) + 1, Ck)
    trimstorage!(C, Ck - 1)
    return C
end
function _broadcast_notzeropres!(f::Tf, fillvalue, C::SparseVecOrMat, A::SparseVecOrMat, B::SparseVecOrMat) where Tf
    # For information on this code, see comments in similar code in _broadcast_zeropres! above
    # Build dense matrix structure in C, expanding storage if necessary
    _densestructure!(C)
    # Populate values
    fill!(storedvals(C), fillvalue)
    rowsentinelA = convert(indtype(A), numrows(C) + 1)
    rowsentinelB = convert(indtype(B), numrows(C) + 1)
    # Cases without vertical expansion
    if numrows(A) == numrows(B) == numrows(C)
        @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
            Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
            while true
                if Ai < Bi
                    Cx, Ci = f(storedvals(A)[Ak], zero(eltype(B))), Ai
                    Ak += oneunit(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                elseif Ai > Bi
                    Cx, Ci = f(zero(eltype(A)), storedvals(B)[Bk]), Bi
                    Bk += oneunit(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                elseif #= Ai == Bi && =# Ai == rowsentinelA
                    break # column complete
                else #= Ai == Bi != rowsentinel =#
                    Cx, Ci::indtype(C) = f(storedvals(A)[Ak], storedvals(B)[Bk]), Ai
                    Ak += oneunit(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                    Bk += oneunit(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                end
                Cx != fillvalue && (storedvals(C)[jo + Ci] = Cx)
            end
        end
    # Cases with vertical expansion
    elseif numrows(A) == numrows(B) == 1 # && numrows(C) != 1, vertically expand both A and B
        @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Ax = Ak < stopAk ? storedvals(A)[Ak] : zero(eltype(A))
            Bx = Bk < stopBk ? storedvals(B)[Bk] : zero(eltype(B))
            Cx = f(Ax, Bx)
            if Cx != fillvalue
                for Ck::Int in (jo + 1):(jo + numrows(C))
                    storedvals(C)[Ck] = Cx
                end
            end
        end
    elseif numrows(A) == 1 # && numrows(B) == numrows(C) != 1, vertically expand only A
        @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Ax = Ak < stopAk ? storedvals(A)[Ak] : zero(eltype(A))
            fvAzB = f(Ax, zero(eltype(B)))
            if fvAzB == fillvalue
                while Bk < stopBk
                    Cx = f(Ax, storedvals(B)[Bk])
                    Cx != fillvalue && (storedvals(C)[jo + storedinds(B)[Bk]] = Cx)
                    Bk += oneunit(Bk)
                end
            else
                Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                for Ci::indtype(C) in 1:numrows(C)
                    if Bi == Ci
                        Cx = f(Ax, storedvals(B)[Bk])
                        Bk += oneunit(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                    else
                        Cx = fvAzB
                    end
                    Cx != fillvalue && (storedvals(C)[jo + Ci] = Cx)
                end
            end
        end
    else # numrows(B) == 1 && numrows(A) == numrows(C) != 1, vertically expand only B
        @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Bx = Bk < stopBk ? storedvals(B)[Bk] : zero(eltype(B))
            fzAvB = f(zero(eltype(A)), Bx)
            if fzAvB == fillvalue
                while Ak < stopAk
                    Cx = f(storedvals(A)[Ak], Bx)
                    Cx != fillvalue && (storedvals(C)[jo + storedinds(A)[Ak]] = Cx)
                    Ak += oneunit(Ak)
                end
            else
                Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                for Ci::indtype(C) in 1:numrows(C)
                    if Ai == Ci
                        Cx = f(storedvals(A)[Ak], Bx)
                        Ak += oneunit(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                    else
                        Cx = fzAvB
                    end
                    Cx != fillvalue && (storedvals(C)[jo + Ci] = Cx)
                end
            end
        end
    end
    return C
end
_finishempty!(C::SparseVector) = C
_finishempty!(C::SparseMatrixCSC) = (fill!(C.colptr, 1); C)

# special case - vector outer product
_copy(f::typeof(*), x::SparseVectorUnion, y::AdjOrTransSparseVectorUnion) = _outer(x, y)
@inline _outer(x::SparseVectorUnion, y::Adjoint) = return _outer(conj, x, parent(y))
@inline _outer(x::SparseVectorUnion, y::Transpose) = return _outer(identity, x, parent(y))
function _outer(trans::Tf, x, y) where Tf
    nx = length(x)
    ny = length(y)
    rowvalx = nonzeroinds(x)
    rowvaly = nonzeroinds(y)
    nzvalsx = nonzeros(x)
    nzvalsy = nonzeros(y)
    nnzx = length(nzvalsx)
    nnzy = length(nzvalsy)

    nnzC = nnzx * nnzy
    Tv = typeof(oneunit(eltype(x)) * oneunit(eltype(y)))
    Ti = promote_type(indtype(x), indtype(y))
    colptrC = zeros(Ti, ny + 1)
    rowvalC = Vector{Ti}(undef, nnzC)
    nzvalsC = Vector{Tv}(undef, nnzC)

    idx = 0
    @inbounds colptrC[1] = 1
    @inbounds for jj = 1:nnzy
        yval = nzvalsy[jj]
        iszero(yval) && continue
        col = rowvaly[jj]
        yval = trans(yval)

        for ii = 1:nnzx
            xval = nzvalsx[ii]
            iszero(xval) && continue
            idx += 1
            colptrC[col+1] += 1
            rowvalC[idx] = rowvalx[ii]
            nzvalsC[idx] = xval * yval
        end
    end
    cumsum!(colptrC, colptrC)

    return SparseMatrixCSC(nx, ny, colptrC, rowvalC, nzvalsC)
end

# (9) _broadcast_zeropres!/_broadcast_notzeropres! for more than two (input) sparse vectors/matrices
function _broadcast_zeropres!(f::Tf, C::SparseVecOrMat, As::Vararg{SparseVecOrMat,N}) where {Tf,N}
    isempty(C) && return _finishempty!(C)
    spaceC::Int = min(length(storedinds(C)), length(storedvals(C)))
    expandsverts = _expandsvert_all(C, As)
    expandshorzs = _expandshorz_all(C, As)
    rowsentinel = numrows(C) + 1
    Ck = 1
    @inbounds for j in columns(C)
        setcolptr!(C, j, Ck)
        ks = _startindforbccol_all(j, expandshorzs, As)
        stopks = _stopindforbccol_all(j, expandshorzs, As)
        # Neither fusing ks and stopks construction, nor restructuring them to avoid repeated
        # colptr lookups, improves performance significantly. So keep the less complex approach here.
        isemptys = _isemptycol_all(ks, stopks)
        defargs = _defargforcol_all(j, isemptys, expandsverts, ks, As)
        rows = _initrowforcol_all(j, rowsentinel, isemptys, expandsverts, ks, As)
        defaultCx = f(defargs...)
        activerow = min(rows...)
        if _iszero(defaultCx) # zero-preserving column scan
            while activerow < rowsentinel
                args, ks, rows = _fusedupdatebc_all(rowsentinel, activerow, rows, defargs, ks, stopks, As)
                Cx = f(args...)
                if !_iszero(Cx)
                    Ck > spaceC && (spaceC = expandstorage!(C, _unchecked_maxnnzbcres(size(C), As)))
                    storedinds(C)[Ck] = activerow
                    storedvals(C)[Ck] = Cx
                    Ck += 1
                end
                activerow = min(rows...)
            end
        else # zero-non-preserving column scan
            for Ci in 1:numrows(C)
                if Ci == activerow
                    args, ks, rows = _fusedupdatebc_all(rowsentinel, activerow, rows, defargs, ks, stopks, As)
                    Cx = f(args...)
                    activerow = min(rows...)
                else
                    Cx = defaultCx
                end
                if !_iszero(Cx)
                    Ck > spaceC && (spaceC = expandstorage!(C, _unchecked_maxnnzbcres(size(C), As)))
                    storedinds(C)[Ck] = Ci
                    storedvals(C)[Ck] = Cx
                    Ck += 1
                end
            end
        end
    end
    @inbounds setcolptr!(C, numcols(C) + 1, Ck)
    trimstorage!(C, Ck - 1)
    return C
end
function _broadcast_notzeropres!(f::Tf, fillvalue, C::SparseVecOrMat, As::Vararg{SparseVecOrMat,N}) where {Tf,N}
    isempty(C) && return _finishempty!(C)
    # Build dense matrix structure in C, expanding storage if necessary
    _densestructure!(C)
    # Populate values
    fill!(storedvals(C), fillvalue)
    expandsverts = _expandsvert_all(C, As)
    expandshorzs = _expandshorz_all(C, As)
    rowsentinel = numrows(C) + 1
    @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
        ks = _startindforbccol_all(j, expandshorzs, As)
        stopks = _stopindforbccol_all(j, expandshorzs, As)
        # Neither fusing ks and stopks construction, nor restructuring them to avoid repeated
        # colptr lookups, improves performance significantly. So keep the less complex approach here.
        isemptys = _isemptycol_all(ks, stopks)
        defargs = _defargforcol_all(j, isemptys, expandsverts, ks, As)
        rows = _initrowforcol_all(j, rowsentinel, isemptys, expandsverts, ks, As)
        defaultCx = f(defargs...)
        activerow = min(rows...)
        if defaultCx == fillvalue # fillvalue-preserving column scan
            while activerow < rowsentinel
                args, ks, rows = _fusedupdatebc_all(rowsentinel, activerow, rows, defargs, ks, stopks, As)
                Cx = f(args...)
                Cx != fillvalue && (storedvals(C)[jo + activerow] = Cx)
                activerow = min(rows...)
            end
        else # fillvalue-non-preserving column scan
            for Ci in 1:numrows(C)
                if Ci == activerow
                    args, ks, rows = _fusedupdatebc_all(rowsentinel, activerow, rows, defargs, ks, stopks, As)
                    Cx = f(args...)
                    activerow = min(rows...)
                else
                    Cx = defaultCx
                end
                Cx != fillvalue && (storedvals(C)[jo + Ci] = Cx)
            end
        end
    end
    return C
end

# helper method for broadcast/broadcast! methods just above
@inline _expandsvert(C, A) = numrows(A) != numrows(C)
@inline _expandsvert_all(C, ::Tuple{}) = ()
@inline _expandsvert_all(C, As) = (_expandsvert(C, first(As)), _expandsvert_all(C, tail(As))...)
@inline _expandshorz(C, A) = numcols(A) != numcols(C)
@inline _expandshorz_all(C, ::Tuple{}) = ()
@inline _expandshorz_all(C, As) = (_expandshorz(C, first(As)), _expandshorz_all(C, tail(As))...)
@inline _startindforbccol(j, expandshorz, A) = expandshorz ? colstartind(A, 1) : colstartind(A, j)
@inline _startindforbccol_all(j, ::Tuple{}, ::Tuple{}) = ()
@inline _startindforbccol_all(j, expandshorzs, As) = (
    _startindforbccol(j, first(expandshorzs), first(As)),
    _startindforbccol_all(j, tail(expandshorzs), tail(As))...)
@inline _stopindforbccol(j, expandshorz, A) = expandshorz ? colboundind(A, 1) : colboundind(A, j)
@inline _stopindforbccol_all(j, ::Tuple{}, ::Tuple{}) = ()
@inline _stopindforbccol_all(j, expandshorzs, As) = (
    _stopindforbccol(j, first(expandshorzs), first(As)),
    _stopindforbccol_all(j, tail(expandshorzs), tail(As))...)
@inline _isemptycol(k, stopk) = k == stopk
@inline _isemptycol_all(::Tuple{}, ::Tuple{}) = ()
@inline _isemptycol_all(ks, stopks) = (
    _isemptycol(first(ks), first(stopks)),
    _isemptycol_all(tail(ks), tail(stopks))...)
@inline _initrowforcol(j, rowsentinel, isempty, expandsvert, k, A) =
    expandsvert || isempty ? convert(indtype(A), rowsentinel) : storedinds(A)[k]
@inline _initrowforcol_all(j, rowsentinel, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
@inline _initrowforcol_all(j, rowsentinel, isemptys, expandsverts, ks, As) = (
    _initrowforcol(j, rowsentinel, first(isemptys), first(expandsverts), first(ks), first(As)),
    _initrowforcol_all(j, rowsentinel, tail(isemptys), tail(expandsverts), tail(ks), tail(As))...)
@inline _defargforcol(j, isempty, expandsvert, k, A) =
    expandsvert && !isempty ? storedvals(A)[k] : zero(eltype(A))
@inline _defargforcol_all(j, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
@inline _defargforcol_all(j, isemptys, expandsverts, ks, As) = (
    _defargforcol(j, first(isemptys), first(expandsverts), first(ks), first(As)),
    _defargforcol_all(j, tail(isemptys), tail(expandsverts), tail(ks), tail(As))...)
@inline function _fusedupdatebc(rowsentinel, activerow, row, defarg, k, stopk, A)
    # returns (val, nextk, nextrow)
    if row == activerow
        nextk = k + oneunit(k)
        (storedvals(A)[k], nextk, (nextk < stopk ? storedinds(A)[nextk] : oftype(row, rowsentinel)))
    else
        (defarg, k, row)
    end
end
@inline _fusedupdatebc_all(rowsent, activerow, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}) = ((#=vals=#), (#=nextks=#), (#=nextrows=#))
@inline function _fusedupdatebc_all(rowsentinel, activerow, rows, defargs, ks, stopks, As)
    val, nextk, nextrow = _fusedupdatebc(rowsentinel, activerow, first(rows), first(defargs), first(ks), first(stopks), first(As))
    vals, nextks, nextrows = _fusedupdatebc_all(rowsentinel, activerow, tail(rows), tail(defargs), tail(ks), tail(stopks), tail(As))
    return ((val, vals...), (nextk, nextks...), (nextrow, nextrows...))
end


# (10) broadcast over combinations of broadcast scalars and sparse vectors/matrices

# broadcast entry points for combinations of sparse arrays and other (scalar) types
@inline function copy(bc::Broadcasted{<:SPVM})
    bcf = flatten(bc)
    return _copy(bcf.f, bcf.args...)
end

_copy(f, args::SparseVector...) = _shapecheckbc(f, args...)
_copy(f, args::SparseMatrixCSC...) = _shapecheckbc(f, args...)
_copy(f, args::SparseVecOrMat...) = _diffshape_broadcast(f, args...)
# Otherwise, we incorporate scalars into the function and re-dispatch
function _copy(f, args...)
    parevalf, passedargstup = capturescalars(f, args)
    return _copy(parevalf, passedargstup...)
end
_copy(f) = throw(MethodError(_copy, (f,)))  # avoid method ambiguity

function _shapecheckbc(f, args...)
    _aresameshape(args...) ? _noshapecheck_map(f, args...) : _diffshape_broadcast(f, args...)
end


@inline function copyto!(dest::SparseVecOrMat, bc::Broadcasted{<:SPVM})
    if bc.f === identity && bc isa SpBroadcasted1 && Base.axes(dest) == (A = bc.args[1]; Base.axes(A))
        return copyto!(dest, A)
    end
    bcf = flatten(bc)
    As = map(arg->Base.unalias(dest, arg), bcf.args)
    return _copyto!(bcf.f, dest, As...)
end

@inline function _copyto!(f, dest, As::SparseVecOrMat...)
    _aresameshape(dest, As...) && return _noshapecheck_map!(f, dest, As...)
    Base.Broadcast.check_broadcast_axes(axes(dest), As...)
    fofzeros = f(_zeros_eltypes(As...)...)
    if _iszero(fofzeros)
        return _broadcast_zeropres!(f, dest, As...)
    else
        return _broadcast_notzeropres!(f, fofzeros, dest, As...)
    end
end

@inline function _copyto!(f, dest, args...)
    # args contains nothing but SparseVecOrMat and scalars
    # See below for capturescalars
    parevalf, passedsrcargstup = capturescalars(f, args)
    _copyto!(parevalf, dest, passedsrcargstup...)
end

# capturescalars takes a function (f) and a tuple of mixed sparse vectors/matrices and
# broadcast scalar arguments (mixedargs), and returns a function (parevalf, i.e. partially
# evaluated f) and a reduced argument tuple (passedargstup) containing only the sparse
# vectors/matrices in mixedargs in their original order, and such that the result of
# broadcast(parevalf, passedargstup...) is broadcast(f, mixedargs...)
@inline function capturescalars(f, mixedargs)
    let (passedsrcargstup, makeargs) = _capturescalars(mixedargs...)
        parevalf = (passed...) -> f(makeargs(passed...)...)
        return (parevalf, passedsrcargstup)
    end
end
# Work around losing Type{T}s as DataTypes within the tuple that makeargs creates
@inline capturescalars(f, mixedargs::Tuple{Ref{Type{T}}, Vararg{Any}}) where {T} =
    capturescalars((args...)->f(T, args...), Base.tail(mixedargs))
@inline capturescalars(f, mixedargs::Tuple{Ref{Type{T}}, Ref{Type{S}}, Vararg{Any}}) where {T, S} =
    # This definition is identical to the one above and necessary only for
    # avoiding method ambiguity.
    capturescalars((args...)->f(T, args...), Base.tail(mixedargs))
@inline capturescalars(f, mixedargs::Tuple{SparseVecOrMat, Ref{Type{T}}, Vararg{Any}}) where {T} =
    capturescalars((a1, args...)->f(a1, T, args...), (mixedargs[1], Base.tail(Base.tail(mixedargs))...))
@inline capturescalars(f, mixedargs::Tuple{Union{Ref,AbstractArray{<:Any,0}}, Ref{Type{T}}, Vararg{Any}}) where {T} =
    capturescalars((args...)->f(mixedargs[1], T, args...), Base.tail(Base.tail(mixedargs)))

nonscalararg(::SparseVecOrMat) = true
nonscalararg(::Any) = false
scalarwrappedarg(::Union{AbstractArray{<:Any,0},Ref}) = true
scalarwrappedarg(::Any) = false

@inline function _capturescalars()
    return (), () -> ()
end
@inline function _capturescalars(arg, mixedargs...)
    let (rest, f) = _capturescalars(mixedargs...)
        if nonscalararg(arg)
            return (arg, rest...), @inline function(head, tail...)
                (head, f(tail...)...)
            end # pass-through to broadcast
        elseif scalarwrappedarg(arg)
            return rest, @inline function(tail...)
                (arg[], f(tail...)...) # TODO: This can put a Type{T} in a tuple
            end # unwrap and add back scalararg after (in makeargs)
        else
            return rest, @inline function(tail...)
                (arg, f(tail...)...)
            end # add back scalararg after (in makeargs)
        end
    end
end
@inline function _capturescalars(arg) # this definition is just an optimization (to bottom out the recursion slightly sooner)
    if nonscalararg(arg)
        return (arg,), (head,) -> (head,) # pass-through
    elseif scalarwrappedarg(arg)
        return (), () -> (arg[],) # unwrap
    else
        return (), () -> (arg,) # add scalararg
    end
end

# NOTE: The following two method definitions work around #19096.
broadcast(f::Tf, ::Type{T}, A::SparseMatrixCSC) where {Tf,T} = broadcast(y -> f(T, y), A)
broadcast(f::Tf, A::SparseMatrixCSC, ::Type{T}) where {Tf,T} = broadcast(x -> f(x, T), A)


# (11) broadcast[!] over combinations of scalars, sparse vectors/matrices, structured matrices,
# and one- and two-dimensional Arrays (via promotion of structured matrices and Arrays)
#
# for combinations involving only scalars, sparse arrays, structured matrices, and dense
# vectors/matrices, promote all structured matrices and dense vectors/matrices to sparse
# and rebroadcast. otherwise, divert to generic AbstractArray broadcast code.

function copy(bc::Broadcasted{PromoteToSparse})
    bcf = flatten(bc)
    if can_skip_sparsification(bcf.f, bcf.args...)
        return _copy(bcf.f, bcf.args...)
    elseif is_supported_sparse_broadcast(bcf.args...)
        return _copy(bcf.f, map(_sparsifystructured, bcf.args)...)
    else
        return copy(convert(Broadcasted{Broadcast.DefaultArrayStyle{length(axes(bc))}}, bc))
    end
end

@inline function copyto!(dest::SparseVecOrMat, bc::Broadcasted{PromoteToSparse})
    bcf = flatten(bc)
    broadcast!(bcf.f, dest, map(_sparsifystructured, bcf.args)...)
end

_sparsifystructured(M::AbstractMatrix) = SparseMatrixCSC(M)
_sparsifystructured(V::AbstractVector) = SparseVector(V)
_sparsifystructured(M::AbstractSparseMatrix) = SparseMatrixCSC(M)
_sparsifystructured(V::AbstractSparseVector) = SparseVector(V)
_sparsifystructured(S::SparseVecOrMat) = S
_sparsifystructured(x) = x


# (12) map[!] over combinations of sparse and structured matrices
SparseOrStructuredMatrix = Union{SparseMatrixCSC,LinearAlgebra.StructuredMatrix}
map(f::Tf, A::SparseOrStructuredMatrix, Bs::Vararg{SparseOrStructuredMatrix,N}) where {Tf,N} =
    (_checksameshape(A, Bs...); _noshapecheck_map(f, _sparsifystructured(A), map(_sparsifystructured, Bs)...))
map!(f::Tf, C::SparseMatrixCSC, A::SparseOrStructuredMatrix, Bs::Vararg{SparseOrStructuredMatrix,N}) where {Tf,N} =
    (_checksameshape(C, A, Bs...); _noshapecheck_map!(f, C, _sparsifystructured(A), map(_sparsifystructured, Bs)...))

end
