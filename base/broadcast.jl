# This file is a part of Julia. License is MIT: http://julialang.org/license

module Broadcast

using ..Cartesian
using Base: promote_op, promote_eltype, promote_eltype_op, @get!, _msk_end, unsafe_bitgetindex
using Base: AddFun, SubFun, MulFun, LDivFun, RDivFun, PowFun
import Base: .+, .-, .*, ./, .\, .//, .==, .<, .!=, .<=, .÷, .%, .<<, .>>, .^
export broadcast, broadcast!, broadcast_function, broadcast!_function, bitbroadcast
export broadcast_getindex, broadcast_setindex!

## Broadcasting utilities ##

droparg1(a, args...) = args

longer_tuple(x::Tuple{}, retx::Tuple, y::Tuple{}, rety::Tuple) = retx
longer_tuple(x::Tuple{}, retx::Tuple, y::Tuple, rety::Tuple) = rety
longer_tuple(x::Tuple, retx::Tuple, y::Tuple{}, rety::Tuple) = retx
longer_tuple(x::Tuple, retx::Tuple, y::Tuple, rety::Tuple) =
    longer_tuple(droparg1(x...), retx, droparg1(y...), rety)
longer_tuple(x::Tuple, y::Tuple) = longer_tuple(x, x, y, y)

longer_size(x::Union{AbstractArray,Number}) = size(x)
longer_size(x::Union{AbstractArray,Number}, y::Union{AbstractArray,Number}...) =
    longer_tuple(size(x), longer_size(y...))

# Calculate the broadcast shape of the arguments, or error if incompatible
broadcast_shape() = ()
function broadcast_shape(As::Union{AbstractArray,Number}...)
    sz = longer_size(As...)
    nd = length(sz)
    bshape = ones(Int, nd)
    for A in As
        for d = 1:ndims(A)
            n = size(A, d)
            if n != 1
                if bshape[d] == 1
                    bshape[d] = n
                elseif bshape[d] != n
                    throw(DimensionMismatch("arrays could not be broadcast to a common size"))
                end
            end
        end
    end
    return tuple(bshape...)::typeof(sz)
end

# Check that all arguments are broadcast compatible with shape
function check_broadcast_shape(shape::Dims, As::Union{AbstractArray,Number}...)
    for A in As
        if ndims(A) > length(shape)
            throw(DimensionMismatch("cannot broadcast array to have fewer dimensions"))
        end
        for k in 1:ndims(A)
            n, nA = shape[k], size(A, k)
            if n != nA != 1
                throw(DimensionMismatch("array could not be broadcast to match destination"))
            end
        end
    end
end

## Broadcasting core
# Generate the body for a broadcasting function f_broadcast!(B, A1, A2, ..., A$narrays),
# using function f, output B, and inputs As...
# B must have already been set to the appropriate size.

# version using cartesian indexing
function gen_broadcast_body_cartesian(nd::Int, narrays::Int, f)
    F = Expr(:quote, f)
    quote
        @assert ndims(B) == $nd
        @ncall $narrays check_broadcast_shape size(B) k->A_k
        @nloops($nd, i, B,
            d->(@nexprs $narrays k->(j_d_k = size(A_k, d) == 1 ? 1 : i_d)), # pre
            begin # body
                @nexprs $narrays k->(@inbounds v_k = @nref $nd A_k d->j_d_k)
                @inbounds (@nref $nd B i) = (@ncall $narrays $F v)
            end)
    end
end

# version using start/next for iterating over the arguments
function gen_broadcast_body_iter(nd::Int, narrays::Int, f)
    F = Expr(:quote, f)
    quote
        @assert ndims(B) == $nd
        @ncall $narrays check_broadcast_shape size(B) k->A_k
        @nexprs 1 d->(@nexprs $narrays k->(state_k_0 = state_k_{$nd} = start(A_k)))
        @nexprs $nd d->(@nexprs $narrays k->(skip_k_d = size(A_k, d) == 1))
        @nloops($nd, i, B,
            d->(@nexprs $narrays k->(state_k_{d-1} = state_k_d)),           # pre
            d->(@nexprs $narrays k->(skip_k_d || (state_k_d = state_k_0))), # post
            begin # body
                @nexprs $narrays k->(@inbounds (v_k, state_k_0) = next(A_k, state_k_0))
                @inbounds (@nref $nd B i) = (@ncall $narrays $F v)
            end)
    end
end

## Broadcasting cores specialized for returning a BitArray

const bitcache_chunks = 64 # this can be changed
const bitcache_size = 64 * bitcache_chunks # do not change this

dumpbitcache(Bc::Vector{UInt64}, bind::Int, C::Vector{Bool}) =
    Base.copy_to_bitarray_chunks!(Bc, ((bind - 1) << 6) + 1, C, 1, min(bitcache_size, (length(Bc)-bind+1) << 6))

# using cartesian indexing
function gen_broadcast_body_cartesian_tobitarray(nd::Int, narrays::Int, f)
    F = Expr(:quote, f)
    quote
        @assert ndims(B) == $nd
        @ncall $narrays check_broadcast_shape size(B) k->A_k
        C = Array(Bool, bitcache_size)
        Bc = B.chunks
        ind = 1
        cind = 1
        @nloops($nd, i, B,
            d->(@nexprs $narrays k->(j_d_k = size(A_k, d) == 1 ? 1 : i_d)), # pre
            begin # body
                @nexprs $narrays k->(@inbounds v_k = @nref $nd A_k d->j_d_k)
                @inbounds C[ind] = (@ncall $narrays $F v)
                ind += 1
                if ind > bitcache_size
                    dumpbitcache(Bc, cind, C)
                    cind += bitcache_chunks
                    ind = 1
                end
            end)
        if ind > 1
            @inbounds C[ind:bitcache_size] = false
            dumpbitcache(Bc, cind, C)
        end
    end
end

# using start/next
function gen_broadcast_body_iter_tobitarray(nd::Int, narrays::Int, f)
    F = Expr(:quote, f)
    quote
        @assert ndims(B) == $nd
        @ncall $narrays check_broadcast_shape size(B) k->A_k
        C = Array(Bool, bitcache_size)
        Bc = B.chunks
        ind = 1
        cind = 1
        @nexprs 1 d->(@nexprs $narrays k->(state_k_0 = state_k_{$nd} = start(A_k)))
        @nexprs $nd d->(@nexprs $narrays k->(skip_k_d = size(A_k, d) == 1))
        @nloops($nd, i, B,
            d->(@nexprs $narrays k->(state_k_{d-1} = state_k_d)),           # pre
            d->(@nexprs $narrays k->(skip_k_d || (state_k_d = state_k_0))), # post
            begin # body
                @nexprs $narrays k->(@inbounds (v_k, state_k_0) = next(A_k, state_k_0))
                @inbounds C[ind] = (@ncall $narrays $F v)
                ind += 1
                if ind > bitcache_size
                    dumpbitcache(Bc, cind, C)
                    cind += bitcache_chunks
                    ind = 1
                end
            end)
        if ind > 1
            @inbounds C[ind:bitcache_size] = false
            dumpbitcache(Bc, cind, C)
        end
    end
end

function gen_broadcast_function(genbody::Function, nd::Int, narrays::Int, f)
    As = [symbol("A_"*string(i)) for i = 1:narrays]
    body = genbody(nd, narrays, f)
    @eval let
        local _F_
        function _F_(B, $(As...))
            $body
        end
        _F_
    end
end

function gen_broadcast_function_tobitarray(genbody::Function, nd::Int, narrays::Int, f)
    As = [symbol("A_"*string(i)) for i = 1:narrays]
    body = genbody(nd, narrays, f)
    @eval let
        local _F_
        function _F_(B::BitArray, $(As...))
            $body
        end
        _F_
    end
end

for (Bsig, Asig, gbf, gbb) in
    ((BitArray                          , Union{Array,BitArray,Number}            ,
      :gen_broadcast_function_tobitarray, :gen_broadcast_body_iter_tobitarray     ),
     (Any                               , Union{Array,BitArray,Number}            ,
      :gen_broadcast_function           , :gen_broadcast_body_iter                ),
     (BitArray                          , Any                                     ,
      :gen_broadcast_function_tobitarray, :gen_broadcast_body_cartesian_tobitarray),
     (Any                               , Any                                     ,
      :gen_broadcast_function           , :gen_broadcast_body_cartesian           ))

    @eval let cache = Dict{Any,Dict{Int,Dict{Int,Any}}}()
        global broadcast!
        function broadcast!(f, B::$Bsig, As::$Asig...)
            nd = ndims(B)
            narrays = length(As)

            cache_f    = @get! cache      f       Dict{Int,Dict{Int,Any}}()
            cache_f_na = @get! cache_f    narrays Dict{Int,Any}()
            func       = @get! cache_f_na nd      $gbf($gbb, nd, narrays, f)

            func(B, As...)
            B
        end
    end  # let broadcast_cache
end


broadcast(f, As...) = broadcast!(f, Array(promote_eltype_op(f, As...), broadcast_shape(As...)), As...)

bitbroadcast(f, As...) = broadcast!(f, BitArray(broadcast_shape(As...)), As...)

broadcast!_function(f) = (B, As...) -> broadcast!(f, B, As...)
broadcast_function(f) = (As...) -> broadcast(f, As...)

broadcast_getindex(src::AbstractArray, I::AbstractArray...) = broadcast_getindex!(Array(eltype(src), broadcast_shape(I...)), src, I...)
@generated function broadcast_getindex!(dest::AbstractArray, src::AbstractArray, I::AbstractArray...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        check_broadcast_shape(size(dest), $(Isplat...))  # unnecessary if this function is never called directly
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
        @nextract $N shape d->(length(shape) < d ? 1 : shape[d])
        if !isa(x, AbstractArray)
            @nloops $N i d->(1:shape_d) d->(@nexprs $N k->(j_d_k = size(I_k, d) == 1 ? 1 : i_d)) begin
                @nexprs $N k->(@inbounds J_k = @nref $N I_k d->j_d_k)
                @inbounds (@nref $N A J) = x
            end
        else
            X = x
            # To call setindex_shape_check, we need to create fake 1-d indexes of the proper size
            @nexprs $N d->(fakeI_d = 1:shape_d)
            @ncall $N Base.setindex_shape_check X shape
            k = 1
            @nloops $N i d->(1:shape_d) d->(@nexprs $N k->(j_d_k = size(I_k, d) == 1 ? 1 : i_d)) begin
                @nexprs $N k->(@inbounds J_k = @nref $N I_k d->j_d_k)
                @inbounds (@nref $N A J) = X[k]
                k += 1
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

eltype_plus(As::AbstractArray...) = promote_eltype_op(AddFun(), As...)

.+(As::AbstractArray...) = broadcast!(+, Array(eltype_plus(As...), broadcast_shape(As...)), As...)

function .-(A::AbstractArray, B::AbstractArray)
    broadcast!(-, Array(promote_op(SubFun(), eltype(A), eltype(B)), broadcast_shape(A,B)), A, B)
end

eltype_mul(As::AbstractArray...) = promote_eltype_op(MulFun(), As...)

.*(As::AbstractArray...) = broadcast!(*, Array(eltype_mul(As...), broadcast_shape(As...)), As...)

function ./(A::AbstractArray, B::AbstractArray)
    broadcast!(/, Array(promote_op(RDivFun(), eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
end

function .\(A::AbstractArray, B::AbstractArray)
    broadcast!(\, Array(promote_op(LDivFun(), eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
end

typealias RatIntT{T<:Integer} Union{Type{Rational{T}},Type{T}}
typealias CRatIntT{T<:Integer} Union{Type{Complex{Rational{T}}},Type{Complex{T}},Type{Rational{T}},Type{T}}
type_rdiv{T<:Integer,S<:Integer}(::RatIntT{T}, ::RatIntT{S}) =
    Rational{promote_type(T,S)}
type_rdiv{T<:Integer,S<:Integer}(::CRatIntT{T}, ::CRatIntT{S}) =
    Complex{Rational{promote_type(T,S)}}
function .//(A::AbstractArray, B::AbstractArray)
    broadcast!(//, Array(type_rdiv(eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
end

function .^(A::AbstractArray, B::AbstractArray)
    broadcast!(^, Array(promote_op(PowFun(), eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
end

## element-wise comparison operators returning BitArray ##

for (f, scalarf, bitf, bitfbody) in ((:.==, :(==), :biteq , :(~a $ b)),
                                     (:.< , :<   , :bitlt , :(~a & b)),
                                     (:.!=, :!=  , :bitneq, :( a $ b)),
                                     (:.<=, :<=  , :bitle , :(~a | b)))
    @eval begin
        ($f)(A::AbstractArray, B::AbstractArray) = bitbroadcast($scalarf, A, B)
        ($bitf)(a::UInt64, b::UInt64) = $bitfbody
        function ($f)(A::AbstractArray{Bool}, B::AbstractArray{Bool})
            local shape
            try
                shape = promote_shape(size(A), size(B))
            catch
                return bitbroadcast($scalarf, A, B)
            end
            F = BitArray(shape)
            Fc = F.chunks
            Ac = bitpack(A).chunks
            Bc = bitpack(B).chunks
            if !isempty(Ac) && !isempty(Bc)
                for i = 1:length(Fc) - 1
                    Fc[i] = ($bitf)(Ac[i], Bc[i])
                end
                Fc[end] = ($bitf)(Ac[end], Bc[end]) & _msk_end(F)
            end
            return F
        end
    end
end

# note: the following are not broadcasting, but need to be defined here to avoid
# ambiguity warnings

for (f, cachef, scalarf) in ((:.==, :bitcache_eq , :(==)),
                             (:.< , :bitcache_lt , :<   ),
                             (:.!=, :bitcache_neq, :!=  ),
                             (:.<=, :bitcache_le , :<=  ))
    @eval ($cachef)(A::AbstractArray, B::AbstractArray, l::Int, ind::Int, C::Vector{Bool}) = 0
    for (sigA, sigB, expA, expB, shape) in ((:Any, :AbstractArray,
                                             :A, :(B[ind]),
                                             :(size(B))),
                                            (:AbstractArray, :Any,
                                             :(A[ind]), :B,
                                             :(size(A))))
        @eval begin
            function ($cachef)(A::$sigA, B::$sigB, l::Int, ind::Int, C::Vector{Bool})
                left = l - ind + 1
                @inbounds begin
                    for j = 1:min(bitcache_size, left)
                        C[j] = ($scalarf)($expA, $expB)
                        ind += 1
                    end
                    C[left+1:bitcache_size] = false
                end
                return ind
            end
            function ($f)(A::$sigA, B::$sigB)
                F = BitArray($shape)
                l = length(F)
                l == 0 && return F
                Fc = F.chunks
                C = Array(Bool, bitcache_size)
                ind = 1
                cind = 1
                for i = 1:div(l + bitcache_size - 1, bitcache_size)
                    ind = ($cachef)(A, B, l, ind, C)
                    dumpbitcache(Fc, cind, C)
                    cind += bitcache_chunks
                end
                return F
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
    C = Array(Bool, bitcache_size)
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
            return bitpack(A) & bitpack(B)
        catch
            return bitbroadcast(&, A, B)
        end
    end
end

end # module
