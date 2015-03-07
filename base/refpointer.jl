### General Methods for Ref{T} type

Base.eltype{T}(x::Type{Ref{T}}) = T
Base.convert{T}(::Type{Ref{T}}, x::Ref{T}) = x

# create Ref objects for general object conversion
Base.cconvert_gcroot{T}(::Type{Ref{T}}, x) = convert(Ref{T}, x)
Base.cconvert{T}(::Type{Ref{T}}, x) = cconvert(Ptr{T}, x)

### Methods for a Ref object that can store a single value of any type

type RefValue{T} <: Ref{T}
    x::T
    RefValue() = new()
    RefValue(x) = new(x)
end
RefValue{T}(x::T) = RefValue{T}(x)

Ref(x::Ref) = x
Ref(x::Any) = RefValue(x)
Ref{T}(x::Ptr{T}, i::Integer=1) = x + (i-1)*Core.sizeof(T)
Ref(x, i::Integer) = (i != 1 && error("Object only has one element"); Ref(x))
Base.call{T}(::Type{Ref{T}}) = RefValue{T}() # Ref{T}()
Base.call{T}(::Type{Ref{T}}, x) = RefValue{T}(x) # Ref{T}(x)
Base.convert{T}(::Type{Ref{T}}, x) = RefValue{T}(x)

function Base.cconvert{T}(P::Type{Ptr{T}}, b::RefValue{T})
    if isbits(T)
        return convert(P, data_pointer_from_objref(b))
    else
        return convert(P, data_pointer_from_objref(b.x))
    end
end
function Base.cconvert(P::Type{Ptr{Any}}, b::RefValue{Any})
    return convert(P, data_pointer_from_objref(b))
end
Base.cconvert{T}(::Type{Ptr{Void}}, b::RefValue{T}) = Base.convert(Ptr{Void}, Base.cconvert(Ptr{T}, b))

### Methods for a Ref object that is backed by an array at index i

# note: the following type definitions don't mean any AbstractArray is convertible to
# a data Ref. they just map the array element type to the pointer type for
# convenience in cases that work.
pointer{T}(x::AbstractArray{T}) = cconvert(Ptr{T}, x)
pointer{T}(x::AbstractArray{T}, i::Integer) = cconvert(Ptr{T},x) + (i-1)*elsize(x)

immutable RefArray{T, A<:AbstractArray, R} <: Ref{T}
    x::A
    i::Int
    roots::R # should be either ::Void or ::Any
    RefArray(x,i,roots=nothing) = (@assert(eltype(A) == T); new(x,i,roots))
end
RefArray{T}(x::AbstractArray{T},i::Int,roots::Any) = RefArray{T,typeof(x),Any}(x, 1, roots)
RefArray{T}(x::AbstractArray{T},i::Int=1,roots::Void=nothing) = RefArray{T,typeof(x),Void}(x, 1, nothing)
Base.convert{T}(::Type{Ref{T}}, x::AbstractArray{T}) = RefArray(x, 1)
Ref(x::AbstractArray, i::Integer=1) = RefArray(x, i)

function Base.cconvert{T}(P::Type{Ptr{T}}, b::RefArray{T})
    if isbits(T)
        convert(P, pointer(b.x, b.i))
    else
        convert(P, data_pointer_from_objref(b.x[b.i]))
    end
end
function Base.cconvert(P::Type{Ptr{Any}}, b::RefArray{Any})
    return convert(P, pointer(b.x, b.i))
end
Base.cconvert{T}(::Type{Ptr{Void}}, b::RefArray{T}) = Base.convert(Ptr{Void}, Base.cconvert(Ptr{T}, b))

###

Base.getindex(b::RefValue) = b.x
Base.getindex(b::RefArray) = b.x[b.i]

Base.setindex!(b::RefValue, x) = (b.x = x; b)
Base.setindex!(b::RefArray, x) = (b.x[b.i] = x; b)

###
