# This file is a part of Julia. License is MIT: https://julialang.org/license

## genericmemory.jl: Managed Memory

"""
    GenericMemory{kind::Symbol, T, addrspace::Int} <: AbstractVector{T}

One-dimensional dense array with elements of type `T`.
"""
GenericMemory
"""
    Memory{T} == GenericMemory{:not_atomic, T, Core.CPU}

One-dimensional dense array with elements of type `T`.
"""
Memory

## Basic functions ##

using Core: memoryrefoffset, memoryref_isassigned # import more functions which were not essential

size(a::GenericMemory, d::Int) =
    d < 1 ? error("dimension out of range") :
    d == 1 ? length(a) :
    1
size(a::GenericMemory, d::Integer) =  size(a, convert(d, Int))
size(a::GenericMemory) = (length(a),)

IndexStyle(::Type{<:GenericMemory}) = IndexLinear()

pointer(mem::GenericMemoryRef) = unsafe_convert(Ptr{Cvoid}, mem) # no bounds check, even for empty array

_unsetindex!(A::Memory, i::Int) =  (@_propagate_inbounds_meta; _unsetindex!(GenericMemoryRef(A, i)); A)
function _unsetindex!(A::MemoryRef{T}) where T
    @_terminates_locally_meta
    @_propagate_inbounds_meta
    @inline
    @boundscheck GenericMemoryRef(A, 1)
    mem = A.mem
    MemT = typeof(mem)
    arrayelem = datatype_arrayelem(MemT)
    elsz = datatype_layoutsize(MemT)
    isboxed = 1; isunion = 2
    t = @_gc_preserve_begin mem
    p = Ptr{Ptr{Cvoid}}(@inbounds pointer(A))
    if arrayelem == isboxed
        Intrinsics.atomic_pointerset(p, C_NULL, :monotonic)
    elseif arrayelem != isunion
        if !datatype_pointerfree(T::DataType)
            for j = 1:Core.sizeof(Ptr{Cvoid}):elsz
                Intrinsics.atomic_pointerset(p + j - 1, C_NULL, :monotonic)
            end
        end
    end
    @_gc_preserve_end t
    return A
end

elsize(@nospecialize _::Type{A}) where {T,A<:GenericMemory{<:Any,T}} = aligned_sizeof(T)
sizeof(a::GenericMemory) = Core.sizeof(a)

# multi arg case will be overwritten later. This is needed for bootstrapping
function isassigned(a::Memory, i::Int)
    @inline
    @boundscheck (i - 1)%UInt < length(a)%UInt || return false
    return @inbounds memoryref_isassigned(GenericMemoryRef(a, i), :not_atomic, false)
end

isassigned(a::GenericMemoryRef) = memoryref_isassigned(a, :not_atomic, @_boundscheck)

## copy ##
function unsafe_copyto!(dest::MemoryRef{T}, src::MemoryRef{T}, n) where {T}
    @_terminates_globally_meta
    n == 0 && return dest
    @boundscheck GenericMemoryRef(dest, n), GenericMemoryRef(src, n)
    ccall(:jl_genericmemory_copyto, Cvoid, (Any, Ptr{Cvoid}, Any, Ptr{Cvoid}, Int), dest.mem, dest.ptr_or_offset, src.mem, src.ptr_or_offset, Int(n))
    return dest
end

function unsafe_copyto!(dest::GenericMemoryRef, src::GenericMemoryRef, n)
    n == 0 && return dest
    @boundscheck GenericMemoryRef(dest, n), GenericMemoryRef(src, n)
    unsafe_copyto!(dest.mem, memoryrefoffset(dest), src.mem, memoryrefoffset(src), n)
    return dest
end

function unsafe_copyto!(dest::Memory{T}, doffs, src::Memory{T}, soffs, n) where{T}
    n == 0 && return dest
    unsafe_copyto!(GenericMemoryRef(dest, doffs), GenericMemoryRef(src, soffs), n)
    return dest
end

function unsafe_copyto!(dest::Memory, doffs, src::Memory, soffs, n)
    @_terminates_locally_meta
    n == 0 && return dest
    # use pointer math to determine if they are deemed to alias
    destp = pointer(dest, doffs)
    srcp = pointer(src, soffs)
    endp = pointer(src, soffs + n - 1)
    @inbounds if destp < srcp || destp > endp
        for i = 1:n
            if isassigned(src, soffs + i - 1)
                dest[doffs + i - 1] = src[soffs + i - 1]
            else
                _unsetindex!(dest, doffs + i - 1)
            end
        end
    else
        for i = n:-1:1
            if isassigned(src, soffs + i - 1)
                dest[doffs + i - 1] = src[soffs + i - 1]
            else
                _unsetindex!(dest, doffs + i - 1)
            end
        end
    end
    return dest
end

copy(a::T) where {T<:Memory} = ccall(:jl_genericmemory_copy, Ref{T}, (Any,), a)

function copyto!(dest::Memory, doffs::Integer, src::Memory, soffs::Integer, n::Integer)
    n < 0 && _throw_argerror("Number of elements to copy must be non-negative.")
    unsafe_copyto!(dest, doffs, src, soffs, n)
    return dest
end


## Constructors ##

similar(a::Memory{T}) where {T}                   = Memory{T}(undef, length(a))
similar(a::Memory{T}, S::Type) where {T}          = Memory{S}(undef, length(a))
similar(a::Memory{T}, m::Int) where {T}           = Memory{T}(undef, m)
similar(a::Memory, T::Type, dims::Dims{1})        = Memory{T}(undef, dims[1])
similar(a::Memory{T}, dims::Dims{1}) where {T}    = Memory{T}(undef, dims[1])

function fill!(a::Union{Memory{UInt8}, Memory{Int8}}, x::Integer)
    t = @_gc_preserve_begin a
    p = unsafe_convert(Ptr{Cvoid}, a)
    T = eltype(a)
    memset(p, x isa T ? x : convert(T, x), length(a))
    @_gc_preserve_end t
    return a
end

## Conversions ##

convert(::Type{T}, a::AbstractArray) where {T<:GenericMemory} = a isa T ? a : T(a)::T

promote_rule(a::Type{Memory{T}}, b::Type{Memory{S}}) where {T,S} = el_same(promote_type(T,S), a, b)
promote_rule(a::Type{GenericMemory{:atomic,T,Core.CPU}}, b::Type{GenericMemory{:atomic,S,Core.CPU}}) where {T,S} = el_same(promote_type(T,S), a, b)

## Constructors ##

if nameof(@__MODULE__) === :Base  # avoid method overwrite
# constructors should make copies
Memory{T}(x::AbstractArray{S,1}) where {T,S} = copyto_axcheck!(Memory{T}(undef, size(x)), x)
end

## copying iterators to containers

## Iteration ##

iterate(A::Memory, i=1) = (@inline; (i - 1)%UInt < length(A)%UInt ? (@inbounds A[i], i + 1) : nothing)

## Indexing: getindex ##

# Faster contiguous indexing using copyto! for AbstractUnitRange and Colon
function getindex(A::Memory, I::AbstractUnitRange{<:Integer})
    @inline
    @boundscheck checkbounds(A, I)
    lI = length(I)
    X = similar(A, axes(I))
    if lI > 0
        copyto!(X, firstindex(X), A, first(I), lI)
    end
    return X
end

# getindex for carrying out logical indexing for AbstractUnitRange{Bool} as Bool <: Integer
getindex(a::Memory, r::AbstractUnitRange{Bool}) = getindex(a, to_index(r))

getindex(A::Memory, c::Colon) = copy(A)

## Indexing: setindex! ##

function setindex!(A::Memory{T}, x, i1::Int) where {T}
    val = x isa T ? x : convert(T,x)::T
    ref = memoryref(memoryref(A), i1, @_boundscheck)
    memoryrefset!(ref, val, :not_atomic, @_boundscheck)
    return A
end
function setindex!(A::Memory{T}, x, i1::Int, i2::Int, I::Int...) where {T}
    @inline
    @boundscheck (i2 == 1 && all(==(1), I)) || throw_boundserror(A, (i1, i2, I...))
    setindex!(A, x, i1)
end

# Faster contiguous setindex! with copyto!
function setindex!(A::Memory{T}, X::Memory{T}, I::AbstractUnitRange{Int}) where T
    @inline
    @boundscheck checkbounds(A, I)
    lI = length(I)
    @boundscheck setindex_shape_check(X, lI)
    if lI > 0
        unsafe_copyto!(A, first(I), X, 1, lI)
    end
    return A
end
function setindex!(A::Memory{T}, X::Memory{T}, c::Colon) where T
    @inline
    lI = length(A)
    @boundscheck setindex_shape_check(X, lI)
    if lI > 0
        unsafe_copyto!(A, 1, X, 1, lI)
    end
    return A
end

# use memcmp for cmp on byte arrays
function cmp(a::Memory{UInt8}, b::Memory{UInt8})
    ta = @_gc_preserve_begin a
    tb = @_gc_preserve_begin b
    pa = unsafe_convert(Ptr{Cvoid}, a)
    pb = unsafe_convert(Ptr{Cvoid}, b)
    c = memcmp(pa, pb, min(length(a),length(b)))
    @_gc_preserve_end ta
    @_gc_preserve_end tb
    return c < 0 ? -1 : c > 0 ? +1 : cmp(length(a),length(b))
end

const BitIntegerMemory{N} = Union{map(T->Memory{T}, BitInteger_types)...}
# use memcmp for == on bit integer types
function ==(a::M, b::M) where {M <: BitIntegerMemory}
    if length(a) == length(b)
        ta = @_gc_preserve_begin a
        tb = @_gc_preserve_begin b
        pa = unsafe_convert(Ptr{Cvoid}, a)
        pb = unsafe_convert(Ptr{Cvoid}, b)
        c = memcmp(pa, pb, sizeof(eltype(M)) * length(a))
        @_gc_preserve_end ta
        @_gc_preserve_end tb
        return c == 0
    else
        return false
    end
end

function findall(pred::Fix2{typeof(in),<:Union{Memory{<:Real},Real}}, x::Memory{<:Real})
    if issorted(x, Sort.Forward) && issorted(pred.x, Sort.Forward)
        return _sortedfindin(x, pred.x)
    else
        return _findin(x, pred.x)
    end
end

# Copying subregions
function indcopy(sz::Dims, I::GenericMemory)
    n = length(I)
    s = sz[n]
    for i = n+1:length(sz)
        s *= sz[i]
    end
    dst = eltype(I)[_findin(I[i], i < n ? (1:sz[i]) : (1:s)) for i = 1:n]
    src = eltype(I)[I[i][_findin(I[i], i < n ? (1:sz[i]) : (1:s))] for i = 1:n]
    dst, src
end
