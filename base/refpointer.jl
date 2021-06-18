# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Ref{T}

An object that safely references data of type `T`. This type is guaranteed to point to
valid, Julia-allocated memory of the correct type. The underlying data is protected from
freeing by the garbage collector as long as the `Ref` itself is referenced.

In Julia, `Ref` objects are dereferenced (loaded or stored) with `[]`.

Creation of a `Ref` to a value `x` of type `T` is usually written `Ref(x)`.
Additionally, for creating interior pointers to containers (such as Array or Ptr),
it can be written `Ref(a, i)` for creating a reference to the `i`-th element of `a`.

`Ref{T}()` creates a reference to a value of type `T` without initialization.
For a bitstype `T`, the value will be whatever currently resides in the memory
allocated. For a non-bitstype `T`, the reference will be undefined and attempting to
dereference it will result in an error, "UndefRefError: access to undefined reference".

To check if a `Ref` is an undefined reference, use [`isassigned(ref::RefValue)`](@ref).
For example, `isassigned(Ref{T}())` is `false` if `T` is not a bitstype.
If `T` is a bitstype, `isassigned(Ref{T}())` will always be true.

When passed as a `ccall` argument (either as a `Ptr` or `Ref` type), a `Ref`
object will be converted to a native pointer to the data it references.
For most `T`, or when converted to a `Ptr{Cvoid}`, this is a pointer to the
object data. When `T` is an `isbits` type, this value may be safely mutated,
otherwise mutation is strictly undefined behavior.

As a special case, setting `T = Any` will instead cause the creation of a
pointer to the reference itself when converted to a `Ptr{Any}`
(a `jl_value_t const* const*` if T is immutable, else a `jl_value_t *const *`).
When converted to a `Ptr{Cvoid}`, it will still return a pointer to the data
region as for any other `T`.

A `C_NULL` instance of `Ptr` can be passed to a `ccall` `Ref` argument to initialize it.

# Use in broadcasting
`Ref` is sometimes used in broadcasting in order to treat the referenced values as a scalar.

# Examples

```jldoctest
julia> Ref(5)
Base.RefValue{Int64}(5)

julia> isa.(Ref([1,2,3]), [Array, Dict, Int]) # Treat reference values as scalar during broadcasting
3-element BitVector:
 1
 0
 0

julia> Ref{Function}()  # Undefined reference to a non-bitstype, Function
Base.RefValue{Function}(#undef)

julia> try
           Ref{Function}()[] # Dereferencing an undefined reference will result in an error
       catch e
           println(e)
       end
UndefRefError()

julia> Ref{Int64}()[]; # A reference to a bitstype refers to an undetermined value if not given

julia> isassigned(Ref{Int64}()) # A reference to a bitstype is always assigned
true

julia> Ref{Int64}(0)[] == 0 # Explicitly give a value for a bitstype reference
true
```
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

eltype(x::Type{<:Ref{T}}) where {T} = @isdefined(T) ? T : Any
convert(::Type{Ref{T}}, x::Ref{T}) where {T} = x
size(x::Ref) = ()
axes(x::Ref) = ()
length(x::Ref) = 1
isempty(x::Ref) = false
ndims(x::Ref) = 0
ndims(::Type{<:Ref}) = 0
iterate(r::Ref) = (r[], nothing)
iterate(r::Ref, s) = nothing
IteratorSize(::Type{<:Ref}) = HasShape{0}()

# create Ref objects for general object conversion
unsafe_convert(::Type{Ref{T}}, x::Ref{T}) where {T} = unsafe_convert(Ptr{T}, x)
unsafe_convert(::Type{Ref{T}}, x) where {T} = unsafe_convert(Ptr{T}, x)

convert(::Type{Ref{T}}, x) where {T} = RefValue{T}(x)

### Methods for a Ref object that is backed by an array at index i
struct RefArray{T,A<:AbstractArray{T},R} <: Ref{T}
    x::A
    i::Int
    roots::R # should be either ::Nothing or ::Any
    RefArray{T,A,R}(x,i,roots=nothing) where {T,A<:AbstractArray{T},R} = new(x,i,roots)
end
RefArray(x::AbstractArray{T}, i::Int, roots::Any) where {T} = RefArray{T,typeof(x),Any}(x, i, roots)
RefArray(x::AbstractArray{T}, i::Int=1, roots::Nothing=nothing) where {T} = RefArray{T,typeof(x),Nothing}(x, i, nothing)
convert(::Type{Ref{T}}, x::AbstractArray{T}) where {T} = RefArray(x, 1)

function unsafe_convert(P::Union{Type{Ptr{T}},Type{Ptr{Cvoid}}}, b::RefArray{T})::P where T
    if allocatedinline(T)
        p = pointer(b.x, b.i)
    elseif isconcretetype(T) && T.mutable
        p = pointer_from_objref(b.x[b.i])
    else
        # see comment on equivalent branch for RefValue
        p = pointerref(Ptr{Ptr{Cvoid}}(pointer(b.x, b.i)), 1, Core.sizeof(Ptr{Cvoid}))
    end
    return p
end
function unsafe_convert(::Type{Ptr{Any}}, b::RefArray{Any})::Ptr{Any}
    return pointer(b.x, b.i)
end

###
if is_primary_base_module
    Ref(x::Any) = RefValue(x)
    Ref{T}() where {T} = RefValue{T}() # Ref{T}()
    Ref{T}(x) where {T} = RefValue{T}(x) # Ref{T}(x)

    Ref(x::Ref, i::Integer) = (i != 1 && error("Ref only has one element"); x)
    Ref(x::Ptr{T}, i::Integer) where {T} = x + (i - 1) * Core.sizeof(T)

    # convert Arrays to pointer arrays for ccall
    function Ref{P}(a::Array{<:Union{Ptr,Cwstring,Cstring}}) where P<:Union{Ptr,Cwstring,Cstring}
        return RefArray(a) # effectively a no-op
    end
    function Ref{P}(a::Array{T}) where P<:Union{Ptr,Cwstring,Cstring} where T
        if (!isbitstype(T) && T <: eltype(P))
            # this Array already has the right memory layout for the requested Ref
            return RefArray(a,1,false) # root something, so that this function is type-stable
        else
            ptrs = Vector{P}(undef, length(a)+1)
            roots = Vector{Any}(undef, length(a))
            for i = 1:length(a)
                root = cconvert(P, a[i])
                ptrs[i] = unsafe_convert(P, root)::P
                roots[i] = root
            end
            ptrs[length(a)+1] = C_NULL
            return RefArray(ptrs,1,roots)
        end
    end
    Ref(x::AbstractArray, i::Integer) = RefArray(x, i)
end

cconvert(::Type{Ptr{P}}, a::Array{<:Ptr}) where {P<:Ptr} = a
cconvert(::Type{Ref{P}}, a::Array{<:Ptr}) where {P<:Ptr} = a
cconvert(::Type{Ptr{P}}, a::Array) where {P<:Union{Ptr,Cwstring,Cstring}} = Ref{P}(a)
cconvert(::Type{Ref{P}}, a::Array) where {P<:Union{Ptr,Cwstring,Cstring}} = Ref{P}(a)

# pass NTuple{N,T} as Ptr{T}/Ref{T}
cconvert(::Type{Ref{T}}, t::NTuple{N,T}) where {N,T} = Ref{NTuple{N,T}}(t)
cconvert(::Type{Ref{T}}, r::Ref{NTuple{N,T}}) where {N,T} = r
unsafe_convert(::Type{Ref{T}}, r::Ref{NTuple{N,T}}) where {N,T} =
    convert(Ptr{T}, unsafe_convert(Ptr{NTuple{N,T}}, r))
unsafe_convert(::Type{Ptr{T}}, r::Ref{NTuple{N,T}}) where {N,T} =
    convert(Ptr{T}, unsafe_convert(Ptr{NTuple{N,T}}, r))
unsafe_convert(::Type{Ptr{T}}, r::Ptr{NTuple{N,T}}) where {N,T} =
    convert(Ptr{T}, r)

###

getindex(b::RefArray) = b.x[b.i]
setindex!(b::RefArray, x) = (b.x[b.i] = x; b)

###

"""
    LLVMPtr{T, AS}

A pointer type that more closely resembles LLVM semantics: It includes the pointer address
space, and will be passed as an actual pointer instead of an integer.

This type is mainly used to interface with code that has strict requirements about pointers,
e.g., intrinsics that are selected based on the address space, or back-ends that require
pointers to be identifiable by their types.
"""
Core.LLVMPtr
