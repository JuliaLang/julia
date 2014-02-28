module Broadcast

using ..Cartesian
import Base.promote_eltype
import Base.(.+), Base.(.-), Base.(.*), Base.(./), Base.(.\)
export broadcast, broadcast!, broadcast_function, broadcast!_function
export broadcast_getindex, broadcast_setindex!

## Broadcasting utilities ##

droparg1(a, args...) = args

longer_tuple(x::(), retx::Tuple, y::(), rety::Tuple) = retx
longer_tuple(x::(), retx::Tuple, y::Tuple, rety::Tuple) = rety
longer_tuple(x::Tuple, retx::Tuple, y::(), rety::Tuple) = retx
longer_tuple(x::Tuple, retx::Tuple, y::Tuple, rety::Tuple) =
    longer_tuple(droparg1(x...), retx, droparg1(y...), rety)
longer_tuple(x::Tuple, y::Tuple) = longer_tuple(x, x, y, y)

longer_size(x::AbstractArray) = size(x)
longer_size(x::AbstractArray, y::AbstractArray...) =
    longer_tuple(size(x), longer_size(y...))

# Calculate the broadcast shape of the arguments, or error if incompatible
broadcast_shape() = ()
function broadcast_shape(As::AbstractArray...)
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
                    error("arrays could not be broadcast to a common size")
                end
            end
        end
    end
    return tuple(bshape...)::typeof(sz)
end

# Check that all arguments are broadcast compatible with shape
function check_broadcast_shape(shape::Dims, As::AbstractArray...)
    for A in As
        if ndims(A) > length(shape)
            error("cannot broadcast array to have fewer dimensions")
        end
        for k in 1:ndims(A)
            n, nA = shape[k], size(A, k)
            if n != nA != 1
                error("array could not be broadcast to match destination")
            end
        end
    end
end

## Broadcasting core
# Generate the body for a broadcasting function f_broadcast!(B, A1, A2, ..., A$narrays),
# using function f, output B, and inputs As...
# B must have already been set to the appropriate size.
function gen_broadcast_body(nd::Int, narrays::Int, f::Function)
    checkshape = Expr(:call, check_broadcast_shape, :(size(B)), [symbol("A_"*string(i)) for i = 1:narrays]...)
    F = Expr(:quote, f)
    quote
        @assert ndims(B) == $nd
        $checkshape
        @nloops $nd i B d->(@nexprs $narrays k->(j_d_k = size(A_k, d) == 1 ? 1 : i_d)) begin
            @nexprs $narrays k->(@inbounds v_k = @nref $nd A_k d->j_d_k)
            @inbounds (@nref $nd B i) = (@ncall $narrays $F v)
        end
    end
end

function broadcast!_function(nd::Int, narrays::Int, f::Function)
    As = [symbol("A_"*string(i)) for i = 1:narrays]
    body = gen_broadcast_body(nd, narrays, f)
    @eval begin
        local _F_
        function _F_(B, $(As...))
            $body
        end
        _F_
    end
end

let broadcast_cache = Dict()
global broadcast!
function broadcast!(f::Function, B, As...)
    nd = ndims(B)
    narrays = length(As)
    key = (f, nd, narrays)
    if !haskey(broadcast_cache,key)
        func = broadcast!_function(nd, narrays, f)
        broadcast_cache[key] = func
    else
        func = broadcast_cache[key]
    end
    func(B, As...)
    B
end
end  # let broadcast_cache

broadcast(f::Function, As...) = broadcast!(f, Array(promote_eltype(As...), broadcast_shape(As...)), As...)

broadcast!_function(f::Function) = (B, As...) -> broadcast!(f, B, As...)
broadcast_function(f::Function) = (As...) -> broadcast(f, As...)

broadcast_getindex(src::AbstractArray, I::AbstractArray...) = broadcast_getindex!(Array(eltype(src), broadcast_shape(I...)), src, I...)
@ngenerate N typeof(dest) function broadcast_getindex!(dest::AbstractArray, src::AbstractArray, I::NTuple{N, AbstractArray}...)
    check_broadcast_shape(size(dest), I...)  # unnecessary if this function is never called directly
    checkbounds(src, I...)
    @nloops N i dest d->(@nexprs N k->(j_d_k = size(I_k, d) == 1 ? 1 : i_d)) begin
        @nexprs N k->(@inbounds J_k = @nref N I_k d->j_d_k)
        @inbounds (@nref N dest i) = (@nref N src J)
    end
    dest
end

@ngenerate N typeof(A) function broadcast_setindex!(A::AbstractArray, x, I::NTuple{N, AbstractArray}...)
    checkbounds(A, I...)
    shape = broadcast_shape(I...)
    @nextract N shape d->(length(shape) < d ? 1 : shape[d])
    if !isa(x, AbstractArray)
        @nloops N i d->(1:shape_d) d->(@nexprs N k->(j_d_k = size(I_k, d) == 1 ? 1 : i_d)) begin
            @nexprs N k->(@inbounds J_k = @nref N I_k d->j_d_k)
            @inbounds (@nref N A J) = x
        end
    else
        X = x
        # To call setindex_shape_check, we need to create fake 1-d indexes of the proper size
        @nexprs N d->(fakeI_d = 1:shape_d)
        Base.setindex_shape_check(X, (@ntuple N fakeI)...)
        k = 1
        @nloops N i d->(1:shape_d) d->(@nexprs N k->(j_d_k = size(I_k, d) == 1 ? 1 : i_d)) begin
            @nexprs N k->(@inbounds J_k = @nref N I_k d->j_d_k)
            @inbounds (@nref N A J) = X[k]
            k += 1
        end
    end
    A
end

## elementwise operators ##

.*(As::AbstractArray...) = broadcast(*, As...)
.%(A::AbstractArray, B::AbstractArray) = broadcast(%, A, B)

eltype_plus(As::AbstractArray...) = promote_eltype(As...)
eltype_plus(As::AbstractArray{Bool}...) = typeof(true+true)

.+(As::AbstractArray...) = broadcast!(+, Array(eltype_plus(As...), broadcast_shape(As...)), As...)

type_minus(T, S) = promote_type(T, S)
type_minus(::Type{Bool}, ::Type{Bool}) = typeof(true-true)

function .-(A::AbstractArray, B::AbstractArray)
    broadcast!(-, Array(type_minus(eltype(A), eltype(B)), broadcast_shape(A,B)), A, B)
end

type_div(T,S) = promote_type(T,S)
type_div{T<:Integer,S<:Integer}(::Type{T},::Type{S}) = typeof(one(T)/one(S))
type_div{T,S}(::Type{Complex{T}},::Type{Complex{S}}) = Complex{type_div(T,S)}
type_div{T,S}(::Type{Complex{T}},::Type{S})          = Complex{type_div(T,S)}
type_div{T,S}(::Type{T},::Type{Complex{S}})          = Complex{type_div(T,S)}

function ./(A::AbstractArray, B::AbstractArray)
    broadcast!(/, Array(type_div(eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
end

function .\(A::AbstractArray, B::AbstractArray)
    broadcast!(\, Array(type_div(eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
end

type_pow(T,S) = promote_type(T,S)
type_pow{S<:Integer}(::Type{Bool},::Type{S}) = Bool
type_pow{S}(T,::Type{Rational{S}}) = type_pow(T, type_div(S, S))

function .^(A::AbstractArray, B::AbstractArray) 
    broadcast!(^, Array(type_pow(eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
end


end # module
