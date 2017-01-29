# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
    Ref{T}

An object that safely references data of type `T`. This type is guaranteed to point to
valid, Julia-allocated memory of the correct type. The underlying data is protected from
freeing by the garbage collector as long as the `Ref` itself is referenced.

When passed as a `ccall` argument (either as a `Ptr` or `Ref` type), a `Ref` object will be
converted to a native pointer to the data it references.

There is no invalid (NULL) `Ref`.
"""
Ref

# C NUL-terminated string pointers; these can be used in ccall
# instead of Ptr{Cchar} and Ptr{Cwchar_t}, respectively, to enforce
# a check for embedded NUL chars in the string (to avoid silent truncation).
if Int === Int64
    bitstype 64 Cstring
    bitstype 64 Cwstring
else
    bitstype 32 Cstring
    bitstype 32 Cwstring
end

### General Methods for Ref{T} type

eltype{T}(x::Type{Ref{T}}) = T
convert{T}(::Type{Ref{T}}, x::Ref{T}) = x

# create Ref objects for general object conversion
unsafe_convert{T}(::Type{Ref{T}}, x) = unsafe_convert(Ptr{T}, x)

### Methods for a Ref object that can store a single value of any type

type RefValue{T} <: Ref{T}
    x::T
    RefValue{T}() where T = new()
    RefValue{T}(x) where T = new(x)
end
RefValue{T}(x::T) = RefValue{T}(x)
isassigned(x::RefValue) = isdefined(x, :x)

Ref(x::Ref) = x
Ref(x::Any) = RefValue(x)
Ref{T}(x::Ptr{T}, i::Integer=1) = x + (i-1)*Core.sizeof(T)
Ref(x, i::Integer) = (i != 1 && error("Object only has one element"); Ref(x))
(::Type{Ref{T}}){T}() = RefValue{T}() # Ref{T}()
(::Type{Ref{T}}){T}(x) = RefValue{T}(x) # Ref{T}(x)
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
immutable RefArray{T, A<:AbstractArray{T}, R} <: Ref{T}
    x::A
    i::Int
    roots::R # should be either ::Void or ::Any
    RefArray{T,A,R}(x,i,roots=nothing) where {T,A<:AbstractArray{T},R} = new(x,i,roots)
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

# convert Arrays to pointer arrays for ccall
function (::Type{Ref{P}}){P<:Union{Ptr,Cwstring,Cstring},T<:Union{Ptr,Cwstring,Cstring}}(a::Array{T}) # Ref{P<:Ptr}(a::Array{T<:Ptr})
    return RefArray(a) # effectively a no-op
end
function (::Type{Ref{P}}){P<:Union{Ptr,Cwstring,Cstring},T}(a::Array{T}) # Ref{P<:Ptr}(a::Array)
    if (!isbits(T) && T <: eltype(P))
        # this Array already has the right memory layout for the requested Ref
        return RefArray(a,1,false) # root something, so that this function is type-stable
    else
        ptrs = Array{P}(length(a)+1)
        roots = Array{Any}(length(a))
        for i = 1:length(a)
            root = cconvert(P, a[i])
            ptrs[i] = unsafe_convert(P, root)::P
            roots[i] = root
        end
        ptrs[length(a)+1] = C_NULL
        return RefArray(ptrs,1,roots)
    end
end
cconvert{P<:Ptr,T<:Ptr}(::Union{Type{Ptr{P}},Type{Ref{P}}}, a::Array{T}) = a
cconvert{P<:Union{Ptr,Cwstring,Cstring}}(::Union{Type{Ptr{P}},Type{Ref{P}}}, a::Array) = Ref{P}(a)

###

getindex(b::RefValue) = b.x
getindex(b::RefArray) = b.x[b.i]

setindex!(b::RefValue, x) = (b.x = x; b)
setindex!(b::RefArray, x) = (b.x[b.i] = x; b)

###
