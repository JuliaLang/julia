# This file is a part of Julia. License is MIT: http://julialang.org/license

### General Methods for Ref{T} type

eltype{T}(x::Type{Ref{T}}) = T
convert{T}(::Type{Ref{T}}, x::Ref{T}) = x

# create Ref objects for general object conversion
unsafe_convert{T}(::Type{Ref{T}}, x) = unsafe_convert(Ptr{T}, x)

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
call{T}(::Type{Ref{T}}) = RefValue{T}() # Ref{T}()
call{T}(::Type{Ref{T}}, x) = RefValue{T}(x) # Ref{T}(x)
convert{T}(::Type{Ref{T}}, x) = RefValue{T}(x)

function unsafe_convert{T}(P::Type{Ptr{T}}, b::RefValue{T})
    if isbits(T)
        return convert(P, data_pointer_from_objref(b))
    else
        return convert(P, data_pointer_from_objref(b.x))
    end
end
function unsafe_convert(P::Type{Ptr{Any}}, b::RefValue{Any})
    return convert(P, data_pointer_from_objref(b))
end
unsafe_convert{T}(::Type{Ptr{Void}}, b::RefValue{T}) = convert(Ptr{Void}, unsafe_convert(Ptr{T}, b))

### Methods for a Ref object that is backed by an array at index i
immutable RefArray{T, A<:AbstractArray, R} <: Ref{T}
    x::A
    i::Int
    roots::R # should be either ::Void or ::Any
    RefArray(x,i,roots=nothing) = (@assert(eltype(A) == T); new(x,i,roots))
end
RefArray{T}(x::AbstractArray{T},i::Int,roots::Any) = RefArray{T,typeof(x),Any}(x, i, roots)
RefArray{T}(x::AbstractArray{T},i::Int=1,roots::Void=nothing) = RefArray{T,typeof(x),Void}(x, i, nothing)
convert{T}(::Type{Ref{T}}, x::AbstractArray{T}) = RefArray(x, 1)
Ref(x::AbstractArray, i::Integer=1) = RefArray(x, i)

function unsafe_convert{T}(P::Type{Ptr{T}}, b::RefArray{T})
    if isbits(T)
        convert(P, pointer(b.x, b.i))
    else
        convert(P, data_pointer_from_objref(b.x[b.i]))
    end
end
function unsafe_convert(P::Type{Ptr{Any}}, b::RefArray{Any})
    return convert(P, pointer(b.x, b.i))
end
unsafe_convert{T}(::Type{Ptr{Void}}, b::RefArray{T}) = convert(Ptr{Void}, unsafe_convert(Ptr{T}, b))

###

getindex(b::RefValue) = b.x
getindex(b::RefArray) = b.x[b.i]

setindex!(b::RefValue, x) = (b.x = x; b)
setindex!(b::RefArray, x) = (b.x[b.i] = x; b)

###
