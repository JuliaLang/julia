# This file is a part of Julia. License is MIT: http://julialang.org/license

module Broadcast

using Base.Cartesian
using Base: promote_op, promote_eltype, promote_eltype_op, @get!, _msk_end, unsafe_bitgetindex, linearindices, to_shape, tail, dimlength, OneTo
import Base: .+, .-, .*, ./, .\, .//, .==, .<, .!=, .<=, .÷, .%, .<<, .>>, .^
export broadcast, broadcast!, bitbroadcast
export broadcast_getindex, broadcast_setindex!

## Broadcasting utilities ##

## Calculate the broadcast shape of the arguments, or error if incompatible
# array inputs
broadcast_shape() = ()
broadcast_shape(A) = indices(A)
@inline broadcast_shape(A, B...) = broadcast_shape((), indices(A), map(indices, B)...)
# shape inputs
broadcast_shape(shape::Tuple) = shape
@inline broadcast_shape(shape::Tuple, shape1::Tuple, shapes::Tuple...) = broadcast_shape(_bcs((), shape, shape1), shapes...)
# _bcs consolidates two shapes into a single output shape
_bcs(out, ::Tuple{}, ::Tuple{}) = out
@inline _bcs(out, ::Tuple{}, newshape) = _bcs((out..., newshape[1]), (), tail(newshape))
@inline _bcs(out, shape, ::Tuple{}) = _bcs((out..., shape[1]), tail(shape), ())
@inline function _bcs(out, shape, newshape)
    newout = _bcs1(shape[1], newshape[1])
    _bcs((out..., newout), tail(shape), tail(newshape))
end
# _bcs1 handles the logic for a single dimension
_bcs1(a::Integer, b::Integer) = a == 1 ? b : (b == 1 ? a : (a == b ? a : throw(DimensionMismatch("arrays could not be broadcast to a common size"))))
_bcs1(a::Integer, b) = a == 1 ? b : (first(b) == 1 && last(b) == a ? b : throw(DimensionMismatch("arrays could not be broadcast to a common size")))
_bcs1(a, b::Integer) = _bcs1(b, a)
_bcs1(a, b) = _bcsm(b, a) ? b : (_bcsm(a, b) ? a : throw(DimensionMismatch("arrays could not be broadcast to a common size")))
# _bcsm tests whether the second index is consistent with the first
_bcsm(a, b) = a == b || (dimlength(b) == 1 && first(b) == first(a))
_bcsm(a, b::Number) = b == 1
_bcsm(a::Number, b::Number) = a == b || b == 1

## Check that all arguments are broadcast compatible with shape
## Check that all arguments are broadcast compatible with shape
# comparing one input against a shape
check_broadcast_shape(::Tuple{}) = nothing
check_broadcast_shape(::Tuple{}, A::Union{AbstractArray,Number}) = check_broadcast_shape((), indices(A))
check_broadcast_shape(shp) = nothing
check_broadcast_shape(shp, A) = check_broadcast_shape(shp, indices(A))
check_broadcast_shape(::Tuple{}, ::Tuple{}) = nothing
check_broadcast_shape(shp, ::Tuple{}) = nothing
check_broadcast_shape(::Tuple{}, Ashp::Tuple) = throw(DimensionMismatch("cannot broadcast array to have fewer dimensions"))
function check_broadcast_shape(shp, Ashp::Tuple)
    _bcsm(shp[1], Ashp[1]) || throw(DimensionMismatch("array could not be broadcast to match destination"))
    check_broadcast_shape(tail(shp), tail(Ashp))
end
# comparing many inputs
@inline function check_broadcast_shape(shp, A, As...)
    check_broadcast_shape(shp, A)
    check_broadcast_shape(shp, As...)
end

## Indexing manipulations
# newindex(I, rule) replaces a CartesianIndex with something that is
# appropriate for a particular array/scalar. `rule` is a tuple that
# describes the manipulations that should be made.
@inline newindex(I::CartesianIndex, ::Tuple{}) = 1    # for scalars
@inline newindex(I::CartesianIndex, indexmap) = CartesianIndex(_newindex((), I.I, indexmap...))
@inline _newindex(out, I) = out  # can truncate if indexmap is shorter than I
@inline _newindex(out, I, keep::Bool, indexmap...) = _newindex((out..., ifelse(keep, I[1], 1)), tail(I), indexmap...)

newindexer(sz, x::Number) = ()
@inline newindexer(sz, A) = _newindexer(sz, size(A))
@inline _newindexer(sz, szA::Tuple{}) = ()
@inline _newindexer(sz, szA) = (sz[1] == szA[1], _newindexer(tail(sz), tail(szA))...)

# map(x->newindexer(sz, x), As), but see #15276
map_newindexer(sz, ::Tuple{}) = ()
@inline map_newindexer(sz, As) = (newindexer(sz, As[1]), map_newindexer(sz, tail(As))...)

# For output BitArrays
const bitcache_chunks = 64 # this can be changed
const bitcache_size = 64 * bitcache_chunks # do not change this

dumpbitcache(Bc::Vector{UInt64}, bind::Int, C::Vector{Bool}) =
    Base.copy_to_bitarray_chunks!(Bc, ((bind - 1) << 6) + 1, C, 1, min(bitcache_size, (length(Bc)-bind+1) << 6))

## Broadcasting core
# nargs encodes the number of As arguments (which matches the number
# of indexmaps). The first two type parameters are to ensure specialization.
@generated function _broadcast!{M,AT,nargs}(f, B::AbstractArray, indexmaps::M, As::AT, ::Type{Val{nargs}})
    quote
        $(Expr(:meta, :noinline))
        # destructure the indexmaps and As tuples
        @nexprs $nargs i->(A_i = As[i])
        @nexprs $nargs i->(imap_i = indexmaps[i])
        @simd for I in CartesianRange(indices(B))
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, imap_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = A_i[I_i])
            # call the function and store the result
            @inbounds B[I] = @ncall $nargs f val
        end
    end
end

# For BitArray outputs, we cache the result in a "small" Vector{Bool},
# and then copy in chunks into the output
@generated function _broadcast!{M,AT,nargs}(f, B::BitArray, indexmaps::M, As::AT, ::Type{Val{nargs}})
    quote
        $(Expr(:meta, :noinline))
        # destructure the indexmaps and As tuples
        @nexprs $nargs i->(A_i = As[i])
        @nexprs $nargs i->(imap_i = indexmaps[i])
        C = Vector{Bool}(bitcache_size)
        Bc = B.chunks
        ind = 1
        cind = 1
        @simd for I in CartesianRange(indices(B))
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, imap_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = A_i[I_i])
            # call the function and store the result
            @inbounds C[ind] = @ncall $nargs f val
            ind += 1
            if ind > bitcache_size
                dumpbitcache(Bc, cind, C)
                cind += bitcache_chunks
                ind = 1
            end
        end
        if ind > 1
            @inbounds C[ind:bitcache_size] = false
            dumpbitcache(Bc, cind, C)
        end
    end
end

@inline function broadcast!{nargs}(f, B::AbstractArray, As::Vararg{Any,nargs})
    check_broadcast_shape(indices(B), As...)
    sz = size(B)
    mapindex = map(x->newindexer(sz, x), As)
    _broadcast!(f, B, mapindex, As, Val{nargs})
    B
end

# broadcast with computed element type

@generated function _broadcast!{M,AT,nargs}(f, B::AbstractArray, indexmaps::M, As::AT, ::Type{Val{nargs}}, iter, st, count)
    quote
        $(Expr(:meta, :noinline))
        # destructure the indexmaps and As tuples
        @nexprs $nargs i->(A_i = As[i])
        @nexprs $nargs i->(imap_i = indexmaps[i])
        while !done(iter, st)
            I, st = next(iter, st)
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, imap_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = A_i[I_i])
            # call the function
            V = @ncall $nargs f val
            S = typeof(V)
            # store the result
            if S <: eltype(B)
                @inbounds B[I] = V
            else
                R = typejoin(eltype(B), S)
                new = similar(B, R)
                for II in take(iter, count)
                    new[II] = B[II]
                end
                new[I] = V
                return _broadcast!(f, new, indexmaps, As, Val{nargs}, iter, st, count+1)
            end
            count += 1
        end
        return B
    end
end

function broadcast_t(f, ::Type{Any}, As...)
    shp = broadcast_shape(As...)
    iter = CartesianRange(shp)
    if isempty(iter)
        return similar(Array{Union{}}, shp)
    end
    nargs = length(As)
    sz = size(iter)
    indexmaps = map(x->newindexer(sz, x), As)
    st = start(iter)
    I, st = next(iter, st)
    val = f([ As[i][newindex(I, indexmaps[i])] for i=1:nargs ]...)
    B = similar(Array{typeof(val)}, shp)
    B[I] = val
    return _broadcast!(f, B, indexmaps, As, Val{nargs}, iter, st, 1)
end

@inline broadcast_t(f, T, As...) = broadcast!(f, similar(Array{T}, broadcast_shape(As...)), As...)

@inline broadcast(f, As...) = broadcast_t(f, promote_eltype_op(f, As...), As...)

# alternate, more compact implementation; unfortunately slower.
# also the `collect` machinery doesn't yet support arbitrary index bases.
#=
@generated function _broadcast{nargs}(f, indexmaps, As, ::Type{Val{nargs}}, iter)
    quote
        collect((@ncall $nargs f i->As[i][newindex(I, indexmaps[i])]) for I in iter)
    end
end

function broadcast(f, As...)
    shp = broadcast_shape(As...)
    iter = CartesianRange(shp)
    sz = size(iter)
    indexmaps = map(x->newindexer(sz, x), As)
    naT = Val{nfields(As)}
    _broadcast(f, indexmaps, As, naT, iter)
end
=#

@inline bitbroadcast(f, As...) = broadcast!(f, similar(BitArray, broadcast_shape(As...)), As...)

broadcast_getindex(src::AbstractArray, I::AbstractArray...) = broadcast_getindex!(Array{eltype(src)}(to_shape(broadcast_shape(I...))), src, I...)
@generated function broadcast_getindex!(dest::AbstractArray, src::AbstractArray, I::AbstractArray...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        check_broadcast_shape(indices(dest), $(Isplat...))  # unnecessary if this function is never called directly
        checkbounds(src, $(Isplat...))
        @nloops $N i dest d->(@nexprs $N k->(j_d_k = size(I_k, d) == 1 ? 1 : i_d)) begin
            @nexprs $N k->(@inbounds J_k = @nref $N I_k d->j_d_k)
            @inbounds (@nref $N dest i) = (@nref $N src J)
        end
        dest
    end
end

@generated function broadcast_setindex!(A::AbstractArray, x, I::AbstractArray...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        checkbounds(A, $(Isplat...))
        shape = broadcast_shape($(Isplat...))
        @nextract $N shape d->(length(shape) < d ? OneTo(1) : shape[d])
        if !isa(x, AbstractArray)
            xA = convert(eltype(A), x)
            @nloops $N i d->shape_d d->(@nexprs $N k->(j_d_k = size(I_k, d) == 1 ? 1 : i_d)) begin
                @nexprs $N k->(@inbounds J_k = @nref $N I_k d->j_d_k)
                @inbounds (@nref $N A J) = xA
            end
        else
            X = x
            @nexprs $N d->(shapelen_d = dimlength(shape_d))
            @ncall $N Base.setindex_shape_check X shapelen
            Xstate = start(X)
            @inbounds @nloops $N i d->shape_d d->(@nexprs $N k->(j_d_k = size(I_k, d) == 1 ? 1 : i_d)) begin
                @nexprs $N k->(J_k = @nref $N I_k d->j_d_k)
                x_el, Xstate = next(X, Xstate)
                (@nref $N A J) = x_el
            end
        end
        A
    end
end

## elementwise operators ##

.÷(A::AbstractArray, B::AbstractArray) = broadcast(÷, A, B)
.%(A::AbstractArray, B::AbstractArray) = broadcast(%, A, B)
.<<(A::AbstractArray, B::AbstractArray) = broadcast(<<, A, B)
.>>(A::AbstractArray, B::AbstractArray) = broadcast(>>, A, B)

eltype_plus(As::AbstractArray...) = promote_eltype_op(+, As...)

.+(As::AbstractArray...) = broadcast!(+, Array{eltype_plus(As...)}(to_shape(broadcast_shape(As...))), As...)

function .-(A::AbstractArray, B::AbstractArray)
    broadcast!(-, Array{promote_op(-, eltype(A), eltype(B))}(to_shape(broadcast_shape(A,B))), A, B)
end

eltype_mul(As::AbstractArray...) = promote_eltype_op(*, As...)

.*(As::AbstractArray...) = broadcast!(*, Array{eltype_mul(As...)}(to_shape(broadcast_shape(As...))), As...)

function ./(A::AbstractArray, B::AbstractArray)
    broadcast!(/, Array{promote_op(/, eltype(A), eltype(B))}(to_shape(broadcast_shape(A, B))), A, B)
end

function .\(A::AbstractArray, B::AbstractArray)
    broadcast!(\, Array{promote_op(\, eltype(A), eltype(B))}(to_shape(broadcast_shape(A, B))), A, B)
end

typealias RatIntT{T<:Integer} Union{Type{Rational{T}},Type{T}}
typealias CRatIntT{T<:Integer} Union{Type{Complex{Rational{T}}},Type{Complex{T}},Type{Rational{T}},Type{T}}
type_rdiv{T<:Integer,S<:Integer}(::RatIntT{T}, ::RatIntT{S}) =
    Rational{promote_type(T,S)}
type_rdiv{T<:Integer,S<:Integer}(::CRatIntT{T}, ::CRatIntT{S}) =
    Complex{Rational{promote_type(T,S)}}
function .//(A::AbstractArray, B::AbstractArray)
    broadcast!(//, Array{type_rdiv(eltype(A), eltype(B))}(to_shape(broadcast_shape(A, B))), A, B)
end

function .^(A::AbstractArray, B::AbstractArray)
    broadcast!(^, Array{promote_op(^, eltype(A), eltype(B))}(to_shape(broadcast_shape(A, B))), A, B)
end

# ## element-wise comparison operators returning BitArray ##

.==(A::AbstractArray, B::AbstractArray) = bitbroadcast(==, A, B)
 .<(A::AbstractArray, B::AbstractArray) = bitbroadcast(<,  A, B)
.!=(A::AbstractArray, B::AbstractArray) = bitbroadcast(!=, A, B)
.<=(A::AbstractArray, B::AbstractArray) = bitbroadcast(<=, A, B)

function broadcast_bitarrays(scalarf, bitf, A::AbstractArray{Bool}, B::AbstractArray{Bool})
    local shape
    try
        shape = promote_shape(size(A), size(B))
    catch
        return bitbroadcast(scalarf, A, B)
    end
    F = BitArray(shape)
    Fc = F.chunks
    Ac = BitArray(A).chunks
    Bc = BitArray(B).chunks
    if !isempty(Ac) && !isempty(Bc)
        for i = 1:length(Fc) - 1
            Fc[i] = (bitf)(Ac[i], Bc[i])
        end
        Fc[end] = (bitf)(Ac[end], Bc[end]) & _msk_end(F)
    end
    return F
end

biteq(a::UInt64, b::UInt64) = ~a $ b
bitlt(a::UInt64, b::UInt64) = ~a & b
bitneq(a::UInt64, b::UInt64) = a $ b
bitle(a::UInt64, b::UInt64) = ~a | b

.==(A::AbstractArray{Bool}, B::AbstractArray{Bool}) = broadcast_bitarrays(==, biteq, A, B)
 .<(A::AbstractArray{Bool}, B::AbstractArray{Bool}) = broadcast_bitarrays(<,  bitlt, A, B)
.!=(A::AbstractArray{Bool}, B::AbstractArray{Bool}) = broadcast_bitarrays(!=, bitneq, A, B)
.<=(A::AbstractArray{Bool}, B::AbstractArray{Bool}) = broadcast_bitarrays(<=, bitle, A, B)

function bitcache(op, A, B, refA, refB, l::Int, ind::Int, C::Vector{Bool})
    left = l - ind + 1
    @inbounds begin
        for j = 1:min(bitcache_size, left)
            C[j] = (op)(refA(A, ind), refB(B, ind))
            ind += 1
        end
        C[left+1:bitcache_size] = false
    end
    return ind
end

# note: the following are not broadcasting, but need to be defined here to avoid
# ambiguity warnings

for (f, scalarf) in ((:.==, :(==)),
                     (:.< , :<   ),
                     (:.!=, :!=  ),
                     (:.<=, :<=  ))
    for (sigA, sigB, active, refA, refB) in ((:Any, :AbstractArray, :B,
                                              :((A,ind)->A), :((B,ind)->B[ind])),
                                             (:AbstractArray, :Any, :A,
                                              :((A,ind)->A[ind]), :((B,ind)->B)))
        shape = :(indices($active))
        @eval begin
            function ($f)(A::$sigA, B::$sigB)
                P = similar(BitArray, $shape)
                F = parent(P)
                l = length(F)
                l == 0 && return F
                Fc = F.chunks
                C = Array{Bool}(bitcache_size)
                ind = first(linearindices($active))
                cind = 1
                for i = 1:div(l + bitcache_size - 1, bitcache_size)
                    ind = bitcache($scalarf, A, B, $refA, $refB, l, ind, C)
                    dumpbitcache(Fc, cind, C)
                    cind += bitcache_chunks
                end
                return P
            end
        end
    end
end

## specialized element-wise operators for BitArray

(.^)(A::BitArray, B::AbstractArray{Bool}) = (B .<= A)
(.^)(A::AbstractArray{Bool}, B::AbstractArray{Bool}) = (B .<= A)

function bitcache_pow{T}(Ac::Vector{UInt64}, B::Array{T}, l::Int, ind::Int, C::Vector{Bool})
    left = l - ind + 1
    @inbounds begin
        for j = 1:min(bitcache_size, left)
            C[j] = unsafe_bitgetindex(Ac, ind) ^ B[ind]
            ind += 1
        end
        C[left+1:bitcache_size] = false
    end
    return ind
end
function (.^){T<:Integer}(A::BitArray, B::Array{T})
    local shape
    try
        shape = promote_shape(size(A), size(B))
    catch
        return bitbroadcast(^, A, B)
    end
    F = BitArray(shape)
    l = length(F)
    l == 0 && return F
    Ac = A.chunks
    Fc = F.chunks
    C = Array{Bool}(bitcache_size)
    ind = 1
    cind = 1
    for i = 1:div(l + bitcache_size - 1, bitcache_size)
        ind = bitcache_pow(Ac, B, l, ind, C)
        dumpbitcache(Fc, cind, C)
        cind += bitcache_chunks
    end
    return F
end

for (sigA, sigB) in ((BitArray, BitArray),
                     (AbstractArray{Bool}, BitArray),
                     (BitArray, AbstractArray{Bool}))
    @eval function (.*)(A::$sigA, B::$sigB)
        try
            return BitArray(A) & BitArray(B)
        catch
            return bitbroadcast(&, A, B)
        end
    end
end

end # module
