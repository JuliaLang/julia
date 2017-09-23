# This file is a part of Julia. License is MIT: https://julialang.org/license

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
    primitive type Cstring  64 end
    primitive type Cwstring 64 end
else
    primitive type Cstring  32 end
    primitive type Cwstring 32 end
end

### General Methods for Ref{T} type

eltype(x::Type{Ref{T}}) where {T} = T
convert(::Type{Ref{T}}, x::Ref{T}) where {T} = x

# create Ref objects for general object conversion
unsafe_convert(::Type{Ref{T}}, x::Ref{T}) where {T} = unsafe_convert(Ptr{T}, x)
unsafe_convert(::Type{Ref{T}}, x) where {T} = unsafe_convert(Ptr{T}, x)

### Methods for a Ref object that can store a single value of any type

mutable struct RefValue{T} <: Ref{T}
    x::T
    RefValue{T}() where {T} = new()
    RefValue{T}(x) where {T} = new(x)
end
RefValue(x::T) where {T} = RefValue{T}(x)
isassigned(x::RefValue) = isdefined(x, :x)

Ref(x::Ref) = x
Ref(x::Any) = RefValue(x)
Ref(x::Ptr{T}, i::Integer=1) where {T} = x + (i-1)*Core.sizeof(T)
Ref(x, i::Integer) = (i != 1 && error("Object only has one element"); Ref(x))
Ref{T}() where {T} = RefValue{T}() # Ref{T}()
Ref{T}(x) where {T} = RefValue{T}(x) # Ref{T}(x)
convert(::Type{Ref{T}}, x) where {T} = RefValue{T}(x)

function unsafe_convert(P::Type{Ptr{T}}, b::RefValue{T}) where T
    if isbits(T) || isbitsunion(T)
        return convert(P, pointer_from_objref(b))
    elseif _isleaftype(T)
        return convert(P, pointer_from_objref(b.x))
    else
        # If the slot is not leaf type, it could be either isbits or not.
        # If it is actually an isbits type and the type inference can infer that
        # it can rebox the `b.x` if we simply call `pointer_from_objref(b.x)` on it.
        # Instead, explicitly load the pointer from the `RefValue` so that the pointer
        # is the same as the one rooted in the `RefValue` object.
        return convert(P, pointerref(Ptr{Ptr{Void}}(pointer_from_objref(b)), 1, 0))
    end
end
function unsafe_convert(P::Type{Ptr{Any}}, b::RefValue{Any})
    return convert(P, data_pointer_from_objref(b))
end
unsafe_convert(::Type{Ptr{Void}}, b::RefValue{T}) where {T} = convert(Ptr{Void}, unsafe_convert(Ptr{T}, b))

### Methods for a Ref object that is backed by an array at index i
struct RefArray{T,A<:AbstractArray{T},R} <: Ref{T}
    x::A
    i::Int
    roots::R # should be either ::Void or ::Any
    RefArray{T,A,R}(x,i,roots=nothing) where {T,A<:AbstractArray{T},R} = new(x,i,roots)
end
RefArray(x::AbstractArray{T}, i::Int, roots::Any) where {T} = RefArray{T,typeof(x),Any}(x, i, roots)
RefArray(x::AbstractArray{T}, i::Int=1, roots::Void=nothing) where {T} = RefArray{T,typeof(x),Void}(x, i, nothing)
convert(::Type{Ref{T}}, x::AbstractArray{T}) where {T} = RefArray(x, 1)
Ref(x::AbstractArray, i::Integer=1) = RefArray(x, i)

function unsafe_convert(P::Type{Ptr{T}}, b::RefArray{T}) where T
    if isbits(T)
        convert(P, pointer(b.x, b.i))
    else
        convert(P, data_pointer_from_objref(b.x[b.i]))
    end
end
function unsafe_convert(P::Type{Ptr{Any}}, b::RefArray{Any})
    return convert(P, pointer(b.x, b.i))
end
unsafe_convert(::Type{Ptr{Void}}, b::RefArray{T}) where {T} = convert(Ptr{Void}, unsafe_convert(Ptr{T}, b))

# convert Arrays to pointer arrays for ccall
function Ref{P}(a::Array{<:Union{Ptr,Cwstring,Cstring}}) where P<:Union{Ptr,Cwstring,Cstring}
    return RefArray(a) # effectively a no-op
end
function Ref{P}(a::Array{T}) where P<:Union{Ptr,Cwstring,Cstring} where T
    if (!isbits(T) && T <: eltype(P))
        # this Array already has the right memory layout for the requested Ref
        return RefArray(a,1,false) # root something, so that this function is type-stable
    else
        ptrs = Vector{P}(length(a)+1)
        roots = Vector{Any}(length(a))
        for i = 1:length(a)
            root = cconvert(P, a[i])
            ptrs[i] = unsafe_convert(P, root)::P
            roots[i] = root
        end
        ptrs[length(a)+1] = C_NULL
        return RefArray(ptrs,1,roots)
    end
end
cconvert(::Type{Ptr{P}}, a::Array{<:Ptr}) where {P<:Ptr} = a
cconvert(::Type{Ref{P}}, a::Array{<:Ptr}) where {P<:Ptr} = a
cconvert(::Type{Ptr{P}}, a::Array) where {P<:Union{Ptr,Cwstring,Cstring}} = Ref{P}(a)
cconvert(::Type{Ref{P}}, a::Array) where {P<:Union{Ptr,Cwstring,Cstring}} = Ref{P}(a)

###

getindex(b::RefValue) = b.x
getindex(b::RefArray) = b.x[b.i]

setindex!(b::RefValue, x) = (b.x = x; b)
setindex!(b::RefArray, x) = (b.x[b.i] = x; b)

###
