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

global _gepvalue
# Special for r::RefValue, r@a means r@x.a
gepfield(r::RefValue, sym::Symbol) = gepfield(let gepfield=_gepfield; r@x; end, sym)
gepfield(r::RefValue, idx::Integer) = gepfield(let gepfield=_gepfield; r@x; end, idx)
gepindex(r::RefValue, idxs...) = gepindex(r@x, idxs...)


Ref(x::Ref) = x
Ref(x::Any) = RefValue(x)
Ref(x::Ptr{T}, i::Integer=1) where {T} = x + (i-1)*Core.sizeof(T)
Ref(x, i::Integer) = (i != 1 && error("Object only has one element"); Ref(x))
Ref{T}() where {T} = RefValue{T}() # Ref{T}()
Ref{T}(x) where {T} = RefValue{T}(x) # Ref{T}(x)
convert(::Type{Ref{T}}, x) where {T} = RefValue{T}(x)
copy(x::RefValue{T}) where {T} = RefValue{T}(x.x)

function unsafe_convert(P::Type{Ptr{T}}, b::RefValue{T}) where T
    if isbits(T)
        return convert(P, data_pointer_from_objref(b))
    else
        return convert(P, data_pointer_from_objref(b.x))
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


## RefField
struct RefField{T} <: Ref{T}
    base
    offset::UInt
    # Basic constructors
    global _gepfield
    function _gepfield(x::ANY, idx::Integer)
        typeof(x).mutable || error("Tried to take reference to immutable type $(typeof(x))")
        new{fieldtype(typeof(x), idx)}(x, fieldoffset(typeof(x), idx))
    end
    function _gepfield(x::RefField{T}, idx::Integer) where {T}
        !fieldisptr(T, idx) || error("Can only take interior references that are inline (e.g. immutable). Tried to access field \"$(fieldname(T, idx))\" of type $T")
        new{fieldtype(T, idx)}(x.base, x.offset + fieldoffset(T, idx))
    end
end

function _gepfield(x::ANY, sym::Symbol)
    _gepfield(x, Base.fieldindex(typeof(x), sym))
end
function _gepfield(x::RefField{T}, sym::Symbol) where T
    _gepfield(x, Base.fieldindex(T, sym))
end

gepfield(x::ANY, idx::Integer) = _gepfield(x, idx)
gepfield(x::RefField{T}, idx::Integer) where {T} = _gepfield(x, idx)
gepfield(x::ANY, idx::Symbol) = _gepfield(x, idx)
gepfield(x::RefField{T}, idx::Symbol) where {T} = _gepfield(x, idx)

# Tuple is defined before us in bootstrap, so it can't refer to RefField
gepindex(x::RefField{<:Tuple}, idx) = gepfield(x, idx)

function setindex!(x::RefField{T}, v::T) where T
    unsafe_store!(Ptr{T}(pointer_from_objref(x.base)+x.offset), v)
    v
end
setindex!(x::RefField{T}, v::S) where {T,S} = setindex!(x, convert(T, v)::T)

function getindex(x::RefField{T}, v::T) where T
    unsafe_load(Ptr{T}(pointer_from_objref(x.base)+x.offset), v)
end

function setfield(x, sym, v)
    if typeof(x).mutable
        y = copy(x)
        setfield!(y, sym, v)
        y
    else
        y = Ref{typeof(x)}(x)
        (gepfield(y@[], sym))[] = v
        y[]
    end
end

function setindex(x, v, idxs...)
    if typeof(x).mutable
        y = copy(x)
        setindex!(y, v, idxs...)
        y
    else
        y = Ref{typeof(x)}(x)
        (gepindex(y@[], idxs...))[] = v
        y[]
    end
end

macro setfield(base, idx)
    if idx.head != :(=)
        error("Expected assignment as second argument")
    end
    idx, rhs = idx.args
    x, v = ntuple(i->gensym(), 2)
    setupexprs = Expr[:($x = $base), :($v = $rhs)]
    getfieldexprs, setfieldexprs = Expr[], Expr[]
    res = nothing
    while true
        y, nv = ntuple(i->gensym(), 2)
        if isa(idx, Symbol)
            idx = Expr(:quote, idx)
            (res !== nothing) && push!(getfieldexprs, :($res = getfield($x, $idx)))
            push!(setfieldexprs, :($nv = setfield($x, $idx, $v)))
            break
        elseif idx.head == :vect
            t = gensym()
            (res !== nothing) && push!(getfieldexprs, :($res = getindex($x, $t...)))
            push!(getfieldexprs, :($t = tuple($(idx.args...))))
            push!(setfieldexprs, :($nv = setindex($x, $v, $t...)))
            break
        elseif idx.head == :(.)
            (res !== nothing) && push!(getfieldexprs, :($res = getfield($y, $(idx.args[2]))))
            push!(setfieldexprs, :($nv = setfield($y, $(idx.args[2]), $v)))
            res, v, idx = y, nv, idx.args[1]
        elseif idx.head == :ref
            t = gensym()
            (res !== nothing) && push!(getfieldexprs, :($res = getindex($y, $t...)))
            push!(getfieldexprs, :($t = tuple($(idx.args[2:end]...))))
            push!(setfieldexprs, :($nv = setindex($y, $v, $t...)))
            res, v, idx = y, nv, idx.args[1]
        else
            error("Unknown field ref syntax")
        end
    end
    push!(getfieldexprs, :($y = $x))
    esc(Expr(:block,
      setupexprs...,
      reverse(getfieldexprs)...,
      setfieldexprs...))
end

###

getindex(b::RefValue) = b.x
getindex(b::RefArray) = b.x[b.i]

setindex!(b::RefValue, x) = (b.x = x; b)
setindex!(b::RefArray, x) = (b.x[b.i] = x; b)

###
