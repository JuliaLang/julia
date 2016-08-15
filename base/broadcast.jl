# This file is a part of Julia. License is MIT: http://julialang.org/license

module Broadcast

using Base.Cartesian
using Base: promote_eltype_op, @get!, _msk_end, unsafe_bitgetindex, linearindices, tail, OneTo, to_shape
import Base: .+, .-, .*, ./, .\, .//, .==, .<, .!=, .<=, .÷, .%, .<<, .>>, .^
export broadcast, broadcast!, bitbroadcast, dotview
export broadcast_getindex, broadcast_setindex!

## Broadcasting utilities ##

# fallback routines for broadcasting with no arguments or with scalars
# to just produce a scalar result:
broadcast(f) = f()
broadcast(f, x::Number...) = f(x...)

# special cases for "X .= ..." (broadcast!) assignments
broadcast!(::typeof(identity), X::AbstractArray, x::Number) = fill!(X, x)
broadcast!(f, X::AbstractArray) = fill!(X, f())
broadcast!(f, X::AbstractArray, x::Number...) = fill!(X, f(x...))
function broadcast!{T,S,N}(::typeof(identity), x::AbstractArray{T,N}, y::AbstractArray{S,N})
    check_broadcast_shape(size(x), size(y))
    copy!(x, y)
end

## Calculate the broadcast shape of the arguments, or error if incompatible
# array inputs
broadcast_shape() = ()
broadcast_shape(A) = indices(A)
@inline broadcast_shape(A, B...) = broadcast_shape((), indices(A), map(indices, B)...)
# shape (i.e., tuple-of-indices) inputs
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
_bcsm(a, b) = a == b || length(b) == 1
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

# newindex(I, keep, Idefault) replaces a CartesianIndex `I` with something that
# is appropriate for a particular broadcast array/scalar. `keep` is a
# NTuple{N,Bool}, where keep[d] == true means that one should preserve
# I[d]; if false, replace it with Idefault[d].
@inline newindex(I::CartesianIndex, keep, Idefault) = CartesianIndex(_newindex(I.I, keep, Idefault))
@inline _newindex(I, keep, Idefault) =
    (ifelse(keep[1], I[1], Idefault[1]), _newindex(tail(I), tail(keep), tail(Idefault))...)
@inline _newindex(I, keep::Tuple{}, Idefault) = ()  # truncate if keep is shorter than I

# newindexer(shape, A) generates `keep` and `Idefault` (for use by
# `newindex` above) for a particular array `A`, given the
# broadcast_shape `shape`
# `keep` is equivalent to map(==, indices(A), shape) (but see #17126)
newindexer(shape, x::Number) = (), ()
@inline newindexer(shape, A) = newindexer(shape, indices(A))
@inline newindexer(shape, indsA::Tuple{}) = (), ()
@inline function newindexer(shape, indsA::Tuple)
    ind1 = indsA[1]
    keep, Idefault = newindexer(tail(shape), tail(indsA))
    (shape[1] == ind1, keep...), (first(ind1), Idefault...)
end

# Equivalent to map(x->newindexer(shape, x), As) (but see #17126)
map_newindexer(shape, ::Tuple{}) = (), ()
@inline function map_newindexer(shape, As)
    A1 = As[1]
    keeps, Idefaults = map_newindexer(shape, tail(As))
    keep, Idefault = newindexer(shape, A1)
    (keep, keeps...), (Idefault, Idefaults...)
end

# For output BitArrays
const bitcache_chunks = 64 # this can be changed
const bitcache_size = 64 * bitcache_chunks # do not change this

dumpbitcache(Bc::Vector{UInt64}, bind::Int, C::Vector{Bool}) =
    Base.copy_to_bitarray_chunks!(Bc, ((bind - 1) << 6) + 1, C, 1, min(bitcache_size, (length(Bc)-bind+1) << 6))

## Broadcasting core
# nargs encodes the number of As arguments (which matches the number
# of keeps). The first two type parameters are to ensure specialization.
@generated function _broadcast!{K,ID,AT,nargs}(f, B::AbstractArray, keeps::K, Idefaults::ID, As::AT, ::Type{Val{nargs}})
    quote
        $(Expr(:meta, :noinline))
        # destructure the keeps and As tuples
        @nexprs $nargs i->(A_i = As[i])
        @nexprs $nargs i->(keep_i = keeps[i])
        @nexprs $nargs i->(Idefault_i = Idefaults[i])
        @simd for I in CartesianRange(indices(B))
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, keep_i, Idefault_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = A_i[I_i])
            # call the function and store the result
            @inbounds B[I] = @ncall $nargs f val
        end
    end
end

# For BitArray outputs, we cache the result in a "small" Vector{Bool},
# and then copy in chunks into the output
@generated function _broadcast!{K,ID,AT,nargs}(f, B::BitArray, keeps::K, Idefaults::ID, As::AT, ::Type{Val{nargs}})
    quote
        $(Expr(:meta, :noinline))
        # destructure the keeps and As tuples
        @nexprs $nargs i->(A_i = As[i])
        @nexprs $nargs i->(keep_i = keeps[i])
        @nexprs $nargs i->(Idefault_i = Idefaults[i])
        C = Vector{Bool}(bitcache_size)
        Bc = B.chunks
        ind = 1
        cind = 1
        @simd for I in CartesianRange(indices(B))
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, keep_i, Idefault_i))
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

"""
    broadcast!(f, dest, As...)

Like [`broadcast`](:func:`broadcast`), but store the result of
`broadcast(f, As...)` in the `dest` array.
Note that `dest` is only used to store the result, and does not supply
arguments to `f` unless it is also listed in the `As`,
as in `broadcast!(f, A, A, B)` to perform `A[:] = broadcast(f, A, B)`.
"""
@inline function broadcast!{nargs}(f, B::AbstractArray, As::Vararg{Any,nargs})
    shape = indices(B)
    check_broadcast_shape(shape, As...)
    keeps, Idefaults = map_newindexer(shape, As)
    _broadcast!(f, B, keeps, Idefaults, As, Val{nargs})
    B
end

# broadcast with computed element type

@generated function _broadcast!{K,ID,AT,nargs}(f, B::AbstractArray, keeps::K, Idefaults::ID, As::AT, ::Type{Val{nargs}}, iter, st, count)
    quote
        $(Expr(:meta, :noinline))
        # destructure the keeps and As tuples
        @nexprs $nargs i->(A_i = As[i])
        @nexprs $nargs i->(keep_i = keeps[i])
        @nexprs $nargs i->(Idefault_i = Idefaults[i])
        while !done(iter, st)
            I, st = next(iter, st)
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, keep_i, Idefault_i))
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
                return _broadcast!(f, new, keeps, Idefaults, As, Val{nargs}, iter, st, count+1)
            end
            count += 1
        end
        return B
    end
end

function broadcast_t(f, ::Type{Any}, As...)
    shape = broadcast_shape(As...)
    iter = CartesianRange(shape)
    if isempty(iter)
        return similar(Array{Union{}}, shape)
    end
    nargs = length(As)
    keeps, Idefaults = map_newindexer(shape, As)
    st = start(iter)
    I, st = next(iter, st)
    val = f([ As[i][newindex(I, keeps[i], Idefaults[i])] for i=1:nargs ]...)
    B = similar(Array{typeof(val)}, shape)
    B[I] = val
    return _broadcast!(f, B, keeps, Idefaults, As, Val{nargs}, iter, st, 1)
end

@inline broadcast_t(f, T, As...) = broadcast!(f, similar(Array{T}, broadcast_shape(As...)), As...)

"""
    broadcast(f, As...)

Broadcasts the arrays `As` to a common size by expanding singleton dimensions, and returns
an array of the results `f(as...)` for each position.

```jldoctest
julia> A = [1, 2, 3, 4, 5]
5-element Array{Int64,1}:
 1
 2
 3
 4
 5

julia> B = [1 2; 3 4; 5 6; 7 8; 9 10]
5×2 Array{Int64,2}:
 1   2
 3   4
 5   6
 7   8
 9  10

julia> broadcast(+, A, B)
5×2 Array{Int64,2}:
  2   3
  5   6
  8   9
 11  12
 14  15
```
"""
@inline broadcast(f, As...) = broadcast_t(f, promote_eltype_op(f, As...), As...)

# alternate, more compact implementation; unfortunately slower.
# also the `collect` machinery doesn't yet support arbitrary index bases.
#=
@generated function _broadcast{nargs}(f, keeps, As, ::Type{Val{nargs}}, iter)
    quote
        collect((@ncall $nargs f i->As[i][newindex(I, keeps[i])]) for I in iter)
    end
end

function broadcast(f, As...)
    shape = broadcast_shape(As...)
    iter = CartesianRange(shape)
    keeps, Idefaults = map_newindexer(shape, As)
    naT = Val{nfields(As)}
    _broadcast(f, keeps, Idefaults, As, naT, iter)
end
=#

"""
    bitbroadcast(f, As...)

Like [`broadcast`](:func:`broadcast`), but allocates a `BitArray` to store the
result, rather then an `Array`.

```jldoctest
julia> bitbroadcast(isodd,[1,2,3,4,5])
5-element BitArray{1}:
  true
 false
  true
 false
  true
```
"""
@inline bitbroadcast(f, As...) = broadcast!(f, similar(BitArray, broadcast_shape(As...)), As...)

"""
    broadcast_getindex(A, inds...)

Broadcasts the `inds` arrays to a common size like [`broadcast`](:func:`broadcast`)
and returns an array of the results `A[ks...]`,
where `ks` goes over the positions in the broadcast result `A`.

```jldoctest
julia> A = [1, 2, 3, 4, 5]
5-element Array{Int64,1}:
 1
 2
 3
 4
 5

julia> B = [1 2; 3 4; 5 6; 7 8; 9 10]
5×2 Array{Int64,2}:
 1   2
 3   4
 5   6
 7   8
 9  10

julia> C = broadcast(+,A,B)
5×2 Array{Int64,2}:
  2   3
  5   6
  8   9
 11  12
 14  15

julia> broadcast_getindex(C,[1,2,10])
3-element Array{Int64,1}:
  2
  5
 15
```
"""
broadcast_getindex(src::AbstractArray, I::AbstractArray...) = broadcast_getindex!(similar(Array{eltype(src)}, broadcast_shape(I...)), src, I...)
@generated function broadcast_getindex!(dest::AbstractArray, src::AbstractArray, I::AbstractArray...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        check_broadcast_shape(indices(dest), $(Isplat...))  # unnecessary if this function is never called directly
        checkbounds(src, $(Isplat...))
        @nexprs $N d->(@nexprs $N k->(Ibcast_d_k = indices(I_k, d) == OneTo(1)))
        @nloops $N i dest d->(@nexprs $N k->(j_d_k = Ibcast_d_k ? 1 : i_d)) begin
            @nexprs $N k->(@inbounds J_k = @nref $N I_k d->j_d_k)
            @inbounds (@nref $N dest i) = (@nref $N src J)
        end
        dest
    end
end

"""
    broadcast_setindex!(A, X, inds...)

Broadcasts the `X` and `inds` arrays to a common size and stores the value from each
position in `X` at the indices in `A` given by the same positions in `inds`.
"""
@generated function broadcast_setindex!(A::AbstractArray, x, I::AbstractArray...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        checkbounds(A, $(Isplat...))
        shape = broadcast_shape($(Isplat...))
        @nextract $N shape d->(length(shape) < d ? OneTo(1) : shape[d])
        @nexprs $N d->(@nexprs $N k->(Ibcast_d_k = indices(I_k, d) == 1:1))
        if !isa(x, AbstractArray)
            xA = convert(eltype(A), x)
            @nloops $N i d->shape_d d->(@nexprs $N k->(j_d_k = Ibcast_d_k ? 1 : i_d)) begin
                @nexprs $N k->(@inbounds J_k = @nref $N I_k d->j_d_k)
                @inbounds (@nref $N A J) = xA
            end
        else
            X = x
            @nexprs $N d->(shapelen_d = length(shape_d))
            @ncall $N Base.setindex_shape_check X shapelen
            Xstate = start(X)
            @inbounds @nloops $N i d->shape_d d->(@nexprs $N k->(j_d_k = Ibcast_d_k ? 1 : i_d)) begin
                @nexprs $N k->(J_k = @nref $N I_k d->j_d_k)
                x_el, Xstate = next(X, Xstate)
                (@nref $N A J) = x_el
            end
        end
        A
    end
end

## elementwise operators ##

for op in (:÷, :%, :<<, :>>, :-, :/, :\, ://, :^)
    @eval $(Symbol(:., op))(A::AbstractArray, B::AbstractArray) = broadcast($op, A, B)
end
.+(As::AbstractArray...) = broadcast(+, As...)
.*(As::AbstractArray...) = broadcast(*, As...)

# ## element-wise comparison operators returning BitArray ##

.==(A::AbstractArray, B::AbstractArray) = bitbroadcast(==, A, B)
 .<(A::AbstractArray, B::AbstractArray) = bitbroadcast(<,  A, B)
.!=(A::AbstractArray, B::AbstractArray) = bitbroadcast(!=, A, B)
.<=(A::AbstractArray, B::AbstractArray) = bitbroadcast(<=, A, B)

function broadcast_bitarrays(scalarf, bitf, A::AbstractArray{Bool}, B::AbstractArray{Bool})
    local shape
    try
        shape = promote_shape(indices(A), indices(B))
    catch
        return bitbroadcast(scalarf, A, B)
    end
    F = BitArray(to_shape(shape))
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
        shape = promote_shape(indices(A), indices(B))
    catch
        return bitbroadcast(^, A, B)
    end
    F = BitArray(to_shape(shape))
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

############################################################

# x[...] .= f.(y...) ---> broadcast!(f, dotview(x, ...), y...).
# The dotview function defaults to getindex, but we override it in
# a few cases to get the expected in-place behavior without affecting
# explicit calls to view.   (All of this can go away if slices
# are changed to generate views by default.)

dotview(args...) = getindex(args...)
dotview(A::AbstractArray, args...) = view(A, args...)
dotview{T<:AbstractArray}(A::AbstractArray{T}, args...) = getindex(A, args...)
# avoid splatting penalty in common cases:
for nargs = 0:5
    args = Symbol[Symbol("x",i) for i = 1:nargs]
    eval(Expr(:(=), Expr(:call, :dotview, args...),
                    Expr(:call, :getindex, args...)))
    eval(Expr(:(=), Expr(:call, :dotview, :(A::AbstractArray), args...),
                    Expr(:call, :view, :A, args...)))
end

end # module
