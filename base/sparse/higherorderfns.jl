# This file is a part of Julia. License is MIT: http://julialang.org/license

module HigherOrderFns

# This module provides higher order functions specialized for sparse arrays,
# particularly map[!]/broadcast[!] for SparseVectors and SparseMatrixCSCs at present.
import Base: map, map!, broadcast, broadcast!

using Base: tail, to_shape
using ..SparseArrays: SparseVector, SparseMatrixCSC, indtype

# This module is organized as follows:
# (1) Define a common interface to SparseVectors and SparseMatrixCSCs sufficient for
#       map[!]/broadcast[!]'s purposes. The methods below are written against this interface.
# (2) Define entry points for map[!] (short children of _map_[not]zeropres!).
# (3) Define entry points for broadcast[!] (short children of _broadcast_[not]zeropres!).
# (4) Define _map_[not]zeropres! specialized for a single (input) sparse vector/matrix.
# (5) Define _map_[not]zeropres! specialized for a pair of (input) sparse vectors/matrices.
# (6) Define general _map_[not]zeropres! capable of handling >2 (input) sparse vectors/matrices.
# (7) Define _broadcast_[not]zeropres! specialized for a pair of (input) sparse vectors/matrices.
# (8) Define general _broadcast_[not]zeropres! capable of handling >2 (input) sparse vectors/matrices.
# (9) Define methods handling combinations of broadcast scalars and sparse vectors/matrices.


# (1) The definitions below provide a common interface to sparse vectors and matrices
# sufficient for the purposes of map[!]/broadcast[!]. This interface treats sparse vectors
# as n-by-one sparse matrices which, though technically incorrect, is how broacast[!] views
# sparse vectors in practice.
SparseVecOrMat = Union{SparseVector,SparseMatrixCSC}
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
map{Tf}(f::Tf, A::SparseVecOrMat) = _noshapecheck_map(f, A)
map{Tf,N}(f::Tf, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N}) =
    (_checksameshape(A, Bs...); _noshapecheck_map(f, A, Bs...))
map!{Tf,N}(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N}) =
    (_checksameshape(C, A, Bs...); _noshapecheck_map!(f, C, A, Bs...))
function _noshapecheck_map!{Tf,N}(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N})
    fofzeros = f(_zeros_eltypes(A, Bs...)...)
    fpreszeros = _iszero(fofzeros)
    return fpreszeros ? _map_zeropres!(f, C, A, Bs...) :
                        _map_notzeropres!(f, fofzeros, C, A, Bs...)
end
function _noshapecheck_map{Tf,N}(f::Tf, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N})
    fofzeros = f(_zeros_eltypes(A, Bs...)...)
    fpreszeros = _iszero(fofzeros)
    maxnnzC = fpreszeros ? min(length(A), _sumnnzs(A, Bs...)) : length(A)
    entrytypeC = Base.Broadcast._broadcast_eltype(f, A, Bs...)
    indextypeC = _promote_indtype(A, Bs...)
    C = _allocres(size(A), indextypeC, entrytypeC, maxnnzC)
    return fpreszeros ? _map_zeropres!(f, C, A, Bs...) :
                        _map_notzeropres!(f, fofzeros, C, A, Bs...)
end
# (3) broadcast[!] entry points
broadcast{Tf}(f::Tf, A::SparseVecOrMat) = _noshapecheck_map(f, A)
broadcast!{Tf}(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat) = map!(f, C, A)
function broadcast!{Tf,N}(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N})
    _aresameshape(C, A, Bs...) && return _noshapecheck_map!(f, C, A, Bs...)
    Base.Broadcast.check_broadcast_indices(indices(C), A, Bs...)
    fofzeros = f(_zeros_eltypes(A, Bs...)...)
    fpreszeros = _iszero(fofzeros)
    return fpreszeros ? _broadcast_zeropres!(f, C, A, Bs...) :
                        _broadcast_notzeropres!(f, fofzeros, C, A, Bs...)
end
# the following three similar defs are necessary for type stability in the mixed vector/matrix case
broadcast{Tf,N}(f::Tf, A::SparseVector, Bs::Vararg{SparseVector,N}) =
    _aresameshape(A, Bs...) ? _noshapecheck_map(f, A, Bs...) : _diffshape_broadcast(f, A, Bs...)
broadcast{Tf,N}(f::Tf, A::SparseMatrixCSC, Bs::Vararg{SparseMatrixCSC,N}) =
    _aresameshape(A, Bs...) ? _noshapecheck_map(f, A, Bs...) : _diffshape_broadcast(f, A, Bs...)
broadcast{Tf,N}(f::Tf, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N}) =
    _diffshape_broadcast(f, A, Bs...)
function _diffshape_broadcast{Tf,N}(f::Tf, A::SparseVecOrMat, Bs::Vararg{SparseVecOrMat,N})
    fofzeros = f(_zeros_eltypes(A, Bs...)...)
    fpreszeros = _iszero(fofzeros)
    indextypeC = _promote_indtype(A, Bs...)
    entrytypeC = Base.Broadcast._broadcast_eltype(f, A, Bs...)
    shapeC = to_shape(Base.Broadcast.broadcast_indices(A, Bs...))
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
@inline _densennz(shape::NTuple{1}) = shape[1]
@inline _densennz(shape::NTuple{2}) = shape[1] * shape[2]
_maxnnzfrom(shape::NTuple{1}, A) = nnz(A) * div(shape[1], A.n)
_maxnnzfrom(shape::NTuple{2}, A::SparseVector) = nnz(A) * div(shape[1], A.n) * shape[2]
_maxnnzfrom(shape::NTuple{2}, A::SparseMatrixCSC) = nnz(A) * div(shape[1], A.m) * div(shape[2], A.n)
@inline _maxnnzfrom_each(shape, ::Tuple{}) = ()
@inline _maxnnzfrom_each(shape, As) = (_maxnnzfrom(shape, first(As)), _maxnnzfrom_each(shape, tail(As))...)
@inline _unchecked_maxnnzbcres(shape, As) = min(_densennz(shape), sum(_maxnnzfrom_each(shape, As)))
@inline _checked_maxnnzbcres(shape::NTuple{1}, As...) = shape[1] != 0 ? _unchecked_maxnnzbcres(shape, As) : 0
@inline _checked_maxnnzbcres(shape::NTuple{2}, As...) = shape[1] != 0 && shape[2] != 0 ? _unchecked_maxnnzbcres(shape, As) : 0
@inline function _allocres(shape::NTuple{1}, indextype, entrytype, maxnnz)
    storedinds = Vector{indextype}(maxnnz)
    storedvals = Vector{entrytype}(maxnnz)
    return SparseVector(shape..., storedinds, storedvals)
end
@inline function _allocres(shape::NTuple{2}, indextype, entrytype, maxnnz)
    pointers = Vector{indextype}(shape[2] + 1)
    storedinds = Vector{indextype}(maxnnz)
    storedvals = Vector{entrytype}(maxnnz)
    return SparseMatrixCSC(shape..., pointers, storedinds, storedvals)
end
# Ambiguity killers, TODO: nix conflicting specializations
ambiguityfunnel{Tf}(f::Tf, x, y) = _aresameshape(x, y) ? _noshapecheck_map(f, x, y) : _diffshape_broadcast(f, x, y)
broadcast(::typeof(+), x::SparseVector, y::SparseVector) = ambiguityfunnel(+, x, y) # base/sparse/sparsevectors.jl:1266
broadcast(::typeof(-), x::SparseVector, y::SparseVector) = ambiguityfunnel(-, x, y) # base/sparse/sparsevectors.jl:1266
broadcast(::typeof(*), x::SparseVector, y::SparseVector) = ambiguityfunnel(*, x, y) # base/sparse/sparsevectors.jl:1266
function broadcast!(::typeof(identity), C::SparseMatrixCSC, A::SparseMatrixCSC) # from #17623, loc?
    _checksameshape(C, A); return copy!(C, A)
end


# (4) _map_zeropres!/_map_notzeropres! specialized for a single sparse vector/matrix
"Stores only the nonzero entries of `map(f, Array(A))` in `C`."
function _map_zeropres!{Tf}(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat)
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
function _map_notzeropres!{Tf}(f::Tf, fillvalue, C::SparseVecOrMat, A::SparseVecOrMat)
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
    copy!(A.nzind, 1:A.n)
    return A
end
function _densestructure!(A::SparseMatrixCSC)
    nnzA = A.m * A.n
    expandstorage!(A, nnzA)
    copy!(A.colptr, 1:A.m:(nnzA + 1))
    for k in _densecoloffsets(A)
        copy!(A.rowval, k + 1, 1:A.m)
    end
    return A
end


# (5) _map_zeropres!/_map_notzeropres! specialized for a pair of sparse vectors/matrices
function _map_zeropres!{Tf}(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat, B::SparseVecOrMat)
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
                Ak += one(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                Bk += one(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
            elseif Ai < Bi
                Cx, Ci = f(storedvals(A)[Ak], zero(eltype(B))), Ai
                Ak += one(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
            else # Bi < Ai
                Cx, Ci = f(zero(eltype(A)), storedvals(B)[Bk]), Bi
                Bk += one(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
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
function _map_notzeropres!{Tf}(f::Tf, fillvalue, C::SparseVecOrMat, A::SparseVecOrMat, B::SparseVecOrMat)
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
                Ak += one(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                Bk += one(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
            elseif Ai < Bi
                Cx, Ci = f(storedvals(A)[Ak], zero(eltype(B))), Ai
                Ak += one(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
            else # Bi < Ai
                Cx, Ci = f(zero(eltype(A)), storedvals(B)[Bk]), Bi
                Bk += one(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
            end
            Cx != fillvalue && (storedvals(C)[jo + Ci] = Cx)
        end
    end
    return C
end


# (6) _map_zeropres!/_map_notzeropres! for more than two sparse matrices / vectors
function _map_zeropres!{Tf,N}(f::Tf, C::SparseVecOrMat, As::Vararg{SparseVecOrMat,N})
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
            # activerows = _isactiverow_all(activerow, rows)
            # Cx = f(_gatherargs(activerows, ks, As)...)
            # ks = _updateind_all(activerows, ks)
            # rows = _updaterow_all(rowsentinel, activerows, rows, ks, stopks, As)
            vals, ks, rows = _fusedupdate_all(rowsentinel, activerow, rows, ks, stopks, As)
            Cx = f(vals...)
            if !_iszero(Cx)
                Ck > spaceC && (spaceC = expandstorage!(C, Ck + min(length(C), _sumnnzs(As...)) - (sum(ks) - N)))
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
function _map_notzeropres!{Tf,N}(f::Tf, fillvalue, C::SparseVecOrMat, As::Vararg{SparseVecOrMat,N})
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
            # activerows = _isactiverow_all(activerow, rows)
            # Cx = f(_gatherargs(activerows, ks, As)...)
            # ks = _updateind_all(activerows, ks)
            # rows = _updaterow_all(rowsentinel, activerows, rows, ks, stopks, As)
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
# fusing the following defs. avoids a few branches, yielding 5-30% runtime reduction
# @inline _isactiverow(activerow, row) = row == activerow
# @inline _isactiverow_all(activerow, ::Tuple{}) = ()
# @inline _isactiverow_all(activerow, rows) = (
#     _isactiverow(activerow, first(rows)),
#     _isactiverow_all(activerow, tail(rows))...)
# @inline _gatherarg(isactiverow, k, A) = isactiverow ? storedvals(A)[k] : zero(eltype(A))
# @inline _gatherargs(::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
# @inline _gatherargs(activerows, ks, As) = (
#     _gatherarg(first(activerows), first(ks), first(As)),
#     _gatherargs(tail(activerows), tail(ks), tail(As))...)
# @inline _updateind(isactiverow, k) = isactiverow ? (k + one(k)) : k
# @inline _updateind_all(::Tuple{}, ::Tuple{}) = ()
# @inline _updateind_all(activerows, ks) = (
#     _updateind(first(activerows), first(ks)),
#     _updateind_all(tail(activerows), tail(ks))...)
# @inline _updaterow(rowsentinel, isrowactive, presrow, k, stopk, A) =
#     isrowactive ? (k < stopk ? storedinds(A)[k] : oftype(presrow, rowsentinel)) : presrow
# @inline _updaterow_all(rowsentinel, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
# @inline _updaterow_all(rowsentinel, activerows, rows, ks, stopks, As) = (
#     _updaterow(rowsentinel, first(activerows), first(rows), first(ks), first(stopks), first(As)),
#     _updaterow_all(rowsentinel, tail(activerows), tail(rows), tail(ks), tail(stopks), tail(As))...)
@inline function _fusedupdate(rowsentinel, activerow, row, k, stopk, A)
    # returns (val, nextk, nextrow)
    if row == activerow
        nextk = k + one(k)
        (storedvals(A)[k], nextk, (nextk < stopk ? storedinds(A)[nextk] : oftype(row, rowsentinel)))
    else
        (zero(eltype(A)), k, row)
    end
end
@inline _fusedupdate_all(rowsentinel, activerow, rows, ks, stopks, As) =
    _fusedupdate_all((#=vals=#), (#=nextks=#), (#=nextrows=#), rowsentinel, activerow, rows, ks, stopks, As)
@inline _fusedupdate_all(vals, nextks, nextrows, rowsent, activerow, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}) =
    (vals, nextks, nextrows)
@inline function _fusedupdate_all(vals, nextks, nextrows, rowsentinel, activerow, rows, ks, stopks, As)
    val, nextk, nextrow = _fusedupdate(rowsentinel, activerow, first(rows), first(ks), first(stopks), first(As))
    return _fusedupdate_all((vals..., val), (nextks..., nextk), (nextrows..., nextrow),
                        rowsentinel, activerow, tail(rows), tail(ks), tail(stopks), tail(As))
end


# (7) _broadcast_zeropres!/_broadcast_notzeropres! specialized for a pair of (input) sparse vectors/matrices
function _broadcast_zeropres!{Tf}(f::Tf, C::SparseVecOrMat, A::SparseVecOrMat, B::SparseVecOrMat)
    isempty(C) && return _finishempty!(C)
    spaceC::Int = min(length(storedinds(C)), length(storedvals(C)))
    rowsentinelA = convert(indtype(A), numrows(C) + 1)
    rowsentinelB = convert(indtype(B), numrows(C) + 1)
    # A and B cannot have the same shape, as we directed that case to map in broadcast's
    # entry point; here we need efficiently handle only heterogeneous combinations of matrices
    # with no singleton dimensions ("matrices" hereafter), one singleton dimension ("columns"
    # and "rows"), and two singleton dimensions ("scalars"). Cases involving scalars should
    # be rare and optimizing that case complicates the code appreciably, so we largely
    # ignore that case's performance below.
    #
    # We first divide the cases into two groups: those in which neither argument expands
    # vertically (matrix-column combinations) and those in which an argument expands
    # vertically (matrix-row and column-row combinations).
    #
    # NOTE: Placing the loops over columns outside the conditional chain segregating
    # argument shape combinations eliminates some code replication but unfortunately
    # hurts performance appreciably in some cases.
    #
    # Cases without vertical expansion
    Ck = 1
    if numrows(A) == numrows(B)
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
                        Ak += one(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                    else # Ai > Bi
                        Cx, Ci = f(zero(eltype(A)), storedvals(B)[Bk]), Bi
                        Bk += one(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                    end
                elseif #= Ai == Bi && =# Ai == rowsentinelA
                    break # column complete
                else #= Ai == Bi != rowsentinel =#
                    Cx, Ci::indtype(C) = f(storedvals(A)[Ak], storedvals(B)[Bk]), Ai
                    Ak += one(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                    Bk += one(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
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
    elseif numrows(A) == 1 # && numrows(B) != 1, vertically expand first argument
        @inbounds for j in columns(C)
            setcolptr!(C, j, Ck)
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Ax = Ak < stopAk ? storedvals(A)[Ak] : zero(eltype(A))
            fvAzB = f(Ax, zero(eltype(B)))
            if fvAzB == zero(eltype(C))
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
                    Bk += one(Bk)
                end
            else
                # A's jth column is nonempty and f(Ax, zero(eltype(B))) is not zero, so
                # we must store (likely) every entry in C's jth column
                Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                for Ci::indtype(C) in 1:numrows(C)
                    if Bi == Ci
                        Cx = f(Ax, storedvals(B)[Bk])
                        Bk += one(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
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
    elseif numrows(B) == 1 # && numrows(A) != 1, vertically expand second argument
        @inbounds for j in columns(C)
            setcolptr!(C, j, Ck)
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Bx = Bk < stopBk ? storedvals(B)[Bk] : zero(eltype(B))
            fzAvB = f(zero(eltype(A)), Bx)
            if fzAvB == zero(eltype(C))
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
                    Ak += one(Ak)
                end
            else
                # B's jth column is nonempty and f(zero(eltype(A)), Bx) is not zero, so
                # we must store (likely) every entry in C's jth column
                Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                for Ci::indtype(C) in 1:numrows(C)
                    if Ai == Ci
                        Cx = f(storedvals(A)[Ak], Bx)
                        Ak += one(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
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
function _broadcast_notzeropres!{Tf}(f::Tf, fillvalue, C::SparseVecOrMat, A::SparseVecOrMat, B::SparseVecOrMat)
    # For information on this code, see comments in similar code in _broadcast_zeropres! above
    # Build dense matrix structure in C, expanding storage if necessary
    _densestructure!(C)
    # Populate values
    fill!(storedvals(C), fillvalue)
    rowsentinelA = convert(indtype(A), numrows(C) + 1)
    rowsentinelB = convert(indtype(B), numrows(C) + 1)
    # Cases without vertical expansion
    if numrows(A) == numrows(B)
        @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
            Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
            while true
                if Ai < Bi
                    Cx, Ci = f(storedvals(A)[Ak], zero(eltype(B))), Ai
                    Ak += one(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                elseif Ai > Bi
                    Cx, Ci = f(zero(eltype(A)), storedvals(B)[Bk]), Bi
                    Bk += one(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                elseif #= Ai == Bi && =# Ai == rowsentinelA
                    break # column complete
                else #= Ai == Bi != rowsentinel =#
                    Cx, Ci::indtype(C) = f(storedvals(A)[Ak], storedvals(B)[Bk]), Ai
                    Ak += one(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                    Bk += one(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                end
                Cx != fillvalue && (storedvals(C)[jo + Ci] = Cx)
            end
        end
    # Cases with vertical expansion
    elseif numrows(A) == 1 # && numrows(B) != 1, vertically expand first argument
        @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Ax = Ak < stopAk ? storedvals(A)[Ak] : zero(eltype(A))
            fvAzB = f(Ax, zero(eltype(B)))
            if fvAzB == zero(eltype(C))
                while Bk < stopBk
                    Cx = f(Ax, storedvals(B)[Bk])
                    Cx != fillvalue && (storedvals(C)[jo + storedinds(B)[Bk]] = Cx)
                    Bk += one(Bk)
                end
            else
                Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                for Ci::indtype(C) in 1:numrows(C)
                    if Bi == Ci
                        Cx = f(Ax, storedvals(B)[Bk])
                        Bk += one(Bk); Bi = Bk < stopBk ? storedinds(B)[Bk] : rowsentinelB
                    else
                        Cx = fvAzB
                    end
                    Cx != fillvalue && (storedvals(C)[jo + Ci] = Cx)
                end
            end
        end
    elseif numrows(B) == 1 # && numrows(A) != 1, vertically expand second argument
        @inbounds for (j, jo) in zip(columns(C), _densecoloffsets(C))
            Ak, stopAk = numcols(A) == 1 ? (colstartind(A, 1), colboundind(A, 1)) : (colstartind(A, j), colboundind(A, j))
            Bk, stopBk = numcols(B) == 1 ? (colstartind(B, 1), colboundind(B, 1)) : (colstartind(B, j), colboundind(B, j))
            Bx = Bk < stopBk ? storedvals(B)[Bk] : zero(eltype(B))
            fzAvB = f(zero(eltype(A)), Bx)
            if fzAvB == zero(eltype(C))
                while Ak < stopAk
                    Cx = f(storedvals(A)[Ak], Bx)
                    Cx != fillvalue && (storedvals(C)[jo + storedinds(A)[Ak]] = Cx)
                    Ak += one(Ak)
                end
            else
                Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
                for Ci::indtype(C) in 1:numrows(C)
                    if Ai == Ci
                        Cx = f(storedvals(A)[Ak], Bx)
                        Ak += one(Ak); Ai = Ak < stopAk ? storedinds(A)[Ak] : rowsentinelA
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


# (8) _broadcast_zeropres!/_broadcast_notzeropres! for more than two (input) sparse vectors/matrices
function _broadcast_zeropres!{Tf,N}(f::Tf, C::SparseVecOrMat, As::Vararg{SparseVecOrMat,N})
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
        if defaultCx == zero(eltype(C)) # zero-preserving column scan
            while activerow < rowsentinel
                # activerows = _isactiverow_all(activerow, rows)
                # Cx = f(_gatherbcargs(activerows, defargs, ks, As)...)
                # ks = _updateind_all(activerows, ks)
                # rows = _updaterow_all(rowsentinel, activerows, rows, ks, stopks, As)
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
                    # activerows = _isactiverow_all(activerow, rows)
                    # Cx = f(_gatherbcargs(activerows, defargs, ks, As)...)
                    # ks = _updateind_all(activerows, ks)
                    # rows = _updaterow_all(rowsentinel, activerows, rows, ks, stopks, As)
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
function _broadcast_notzeropres!{Tf,N}(f::Tf, fillvalue, C::SparseVecOrMat, As::Vararg{SparseVecOrMat,N})
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
                # activerows = _isactiverow_all(activerow, rows)
                # Cx = f(_gatherbcargs(activerows, defargs, ks, As)...)
                # ks = _updateind_all(activerows, ks)
                # rows = _updaterow_all(rowsentinel, activerows, rows, ks, stopks, As)
                args, ks, rows = _fusedupdatebc_all(rowsentinel, activerow, rows, defargs, ks, stopks, As)
                Cx = f(args...)
                Cx != fillvalue && (storedvals(C)[jo + activerow] = Cx)
                activerow = min(rows...)
            end
        else # fillvalue-non-preserving column scan
            for Ci in 1:numrows(C)
                if Ci == activerow
                    # activerows = _isactiverow_all(activerow, rows)
                    # Cx = f(_gatherbcargs(activerows, defargs, ks, As)...)
                    # ks = _updateind_all(activerows, ks)
                    # rows = _updaterow_all(rowsentinel, activerows, rows, ks, stopks, As)
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
# fusing the following defs. avoids a few branches and construction of a tuple, yielding 1-20% runtime reduction
# @inline _isactiverow(activerow, row) = row == activerow
# @inline _isactiverow_all(activerow, ::Tuple{}) = ()
# @inline _isactiverow_all(activerow, rows) = (
#     _isactiverow(activerow, first(rows)),
#     _isactiverow_all(activerow, tail(rows))...)
# @inline _gatherbcarg(isactiverow, defarg, k, A) = isactiverow ? storedvals(A)[k] : defarg
# @inline _gatherbcargs(::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
# @inline _gatherbcargs(activerows, defargs, ks, As) = (
#     _gatherbcarg(first(activerows), first(defargs), first(ks), first(As)),
#     _gatherbcargs(tail(activerows), tail(defargs), tail(ks), tail(As))...)
# @inline _updateind(isactiverow, k) = isactiverow ? (k + one(k)) : k
# @inline _updateind_all(::Tuple{}, ::Tuple{}) = ()
# @inline _updateind_all(activerows, ks) = (
#     _updateind(first(activerows), first(ks)),
#     _updateind_all(tail(activerows), tail(ks))...)
# @inline _updaterow(rowsentinel, isrowactive, presrow, k, stopk, A) =
#     isrowactive ? (k < stopk ? storedinds(A)[k] : oftype(presrow, rowsentinel)) : presrow
# @inline _updaterow_all(rowsentinel, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
# @inline _updaterow_all(rowsentinel, activerows, rows, ks, stopks, As) = (
#     _updaterow(rowsentinel, first(activerows), first(rows), first(ks), first(stopks), first(As)),
#     _updaterow_all(rowsentinel, tail(activerows), tail(rows), tail(ks), tail(stopks), tail(As))...)
@inline function _fusedupdatebc(rowsentinel, activerow, row, defarg, k, stopk, A)
    # returns (val, nextk, nextrow)
    if row == activerow
        nextk = k + one(k)
        (storedvals(A)[k], nextk, (nextk < stopk ? storedinds(A)[nextk] : oftype(row, rowsentinel)))
    else
        (defarg, k, row)
    end
end
@inline _fusedupdatebc_all(rowsentinel, activerow, rows, defargs, ks, stopks, As) =
    _fusedupdatebc_all((#=vals=#), (#=nextks=#), (#=nextrows=#), rowsentinel, activerow, rows, defargs, ks, stopks, As)
@inline _fusedupdatebc_all(vals, nextks, nextrows, rowsent, activerow, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}, ::Tuple{}) =
    (vals, nextks, nextrows)
@inline function _fusedupdatebc_all(vals, nextks, nextrows, rowsentinel, activerow, rows, defargs, ks, stopks, As)
    val, nextk, nextrow = _fusedupdatebc(rowsentinel, activerow, first(rows), first(defargs), first(ks), first(stopks), first(As))
    return _fusedupdatebc_all((vals..., val), (nextks..., nextk), (nextrows..., nextrow),
                        rowsentinel, activerow, tail(rows), tail(defargs), tail(ks), tail(stopks), tail(As))
end


# (9) broadcast[!] over combinations of broadcast scalars and sparse vectors/matrices
#
# TODO: The minimal snippet below is not satisfying: A better solution would achieve
# the same for (1) all broadcast scalar types (Base.Broadcast.containertype(x) == Any?) and
# (2) any combination (number, order, type mixture) of broadcast scalars.
#
broadcast{Tf}(f::Tf, x::Union{Number,Bool}, A::SparseMatrixCSC) = broadcast(y -> f(x, y), A)
broadcast{Tf}(f::Tf, A::SparseMatrixCSC, y::Union{Number,Bool}) = broadcast(x -> f(x, y), A)
# NOTE: The following two method definitions work around #19096. These definitions should
# be folded into the two preceding definitions on resolution of #19096.
broadcast{Tf,T}(f::Tf, ::Type{T}, A::SparseMatrixCSC) = broadcast(y -> f(T, y), A)
broadcast{Tf,T}(f::Tf, A::SparseMatrixCSC, ::Type{T}) = broadcast(x -> f(x, T), A)

end
