# This file is a part of Julia. License is MIT: https://julialang.org/license

## genericmemory.jl: Managed Memory

"""
    GenericMemory{kind::Symbol, T, addrspace=Core.CPU} <: DenseVector{T}

Fixed-size [`DenseVector{T}`](@ref DenseVector).

`kind` can currently be either `:not_atomic` or `:atomic`. For details on what `:atomic` implies, see [`AtomicMemory`](@ref)

`addrspace` can currently only be set to `Core.CPU`. It is designed to permit extension by other systems such as GPUs, which might define values such as:
```julia
module CUDA
const Generic = bitcast(Core.AddrSpace{CUDA}, 0)
const Global = bitcast(Core.AddrSpace{CUDA}, 1)
end
```
The exact semantics of these other addrspaces is defined by the specific backend, but will error if the user is attempting to access these on the CPU.

!!! compat "Julia 1.11"
    This type requires Julia 1.11 or later.
"""
GenericMemory

"""
    Memory{T} == GenericMemory{:not_atomic, T, Core.CPU}

Fixed-size [`DenseVector{T}`](@ref DenseVector).

!!! compat "Julia 1.11"
    This type requires Julia 1.11 or later.
"""
Memory

"""
    AtomicMemory{T} == GenericMemory{:atomic, T, Core.CPU}

Fixed-size [`DenseVector{T}`](@ref DenseVector).
Fetching of any of its individual elements is performed atomically
(with `:monotonic` ordering by default).

!!! warning
    The access to `AtomicMemory` must be done by either using the [`@atomic`](@ref)
    macro or the lower level interface functions: `Base.getindex_atomic`,
    `Base.setindex_atomic!`, `Base.setindexonce_atomic!`,
    `Base.swapindex_atomic!`, `Base.modifyindex_atomic!`, and `Base.replaceindex_atomic!`.

For details, see [Atomic Operations](@ref man-atomic-operations) as well as macros
[`@atomic`](@ref), [`@atomiconce`](@ref), [`@atomicswap`](@ref), and [`@atomicreplace`](@ref).

!!! compat "Julia 1.11"
    This type requires Julia 1.11 or later.

!!! compat "Julia 1.12"
    Lower level interface functions or `@atomic` macro requires Julia 1.12 or later.
"""
AtomicMemory

## Basic functions ##

using Core: memoryrefoffset, memoryref_isassigned # import more functions which were not essential

size(a::GenericMemory, d::Int) =
    d < 1 ? error("dimension out of range") :
    d == 1 ? length(a) :
    1
size(a::GenericMemory, d::Integer) =  size(a, convert(Int, d))
size(a::GenericMemory) = (length(a),)

IndexStyle(::Type{<:GenericMemory}) = IndexLinear()

parent(ref::GenericMemoryRef) = ref.mem

pointer(mem::GenericMemoryRef) = unsafe_convert(Ptr{Cvoid}, mem) # no bounds check, even for empty array

_unsetindex!(A::Memory, i::Int) =  (@_propagate_inbounds_meta; _unsetindex!(memoryref(A, i)); A)
function _unsetindex!(A::MemoryRef{T}) where T
    @_terminates_locally_meta
    @_propagate_inbounds_meta
    @inline
    @boundscheck memoryref(A, 1)
    mem = A.mem
    MemT = typeof(mem)
    arrayelem = datatype_arrayelem(MemT)
    elsz = datatype_layoutsize(MemT)
    isbits = 0; isboxed = 1; isunion = 2
    arrayelem == isbits && datatype_pointerfree(T::DataType) && return A
    t = @_gc_preserve_begin mem
    p = Ptr{Ptr{Cvoid}}(@inbounds pointer(A))
    if arrayelem == isboxed
        Intrinsics.atomic_pointerset(p, C_NULL, :monotonic)
    elseif arrayelem != isunion
        for j = 1:Core.sizeof(Ptr{Cvoid}):elsz
            # XXX: this violates memory ordering, since it writes more than one C_NULL to each
            Intrinsics.atomic_pointerset(p + j - 1, C_NULL, :monotonic)
        end
    end
    @_gc_preserve_end t
    return A
end

elsize(@nospecialize _::Type{A}) where {T,A<:GenericMemory{<:Any,T}} = aligned_sizeof(T) # XXX: probably supposed to be the stride?
sizeof(a::GenericMemory) = Core.sizeof(a)

# multi arg case will be overwritten later. This is needed for bootstrapping
function isassigned(a::GenericMemory, i::Int)
    @inline
    @boundscheck (i - 1)%UInt < length(a)%UInt || return false
    return @inbounds memoryref_isassigned(memoryref(a, i), default_access_order(a), false)
end

isassigned(a::GenericMemoryRef) = memoryref_isassigned(a, default_access_order(a), @_boundscheck)

## copy ##
function unsafe_copyto!(dest::MemoryRef{T}, src::MemoryRef{T}, n) where {T}
    @_terminates_globally_notaskstate_meta
    n == 0 && return dest
    @boundscheck memoryref(dest, n), memoryref(src, n)
    if isbitstype(T)
        tdest = @_gc_preserve_begin dest
        tsrc = @_gc_preserve_begin src
        pdest = unsafe_convert(Ptr{Cvoid}, dest)
        psrc = unsafe_convert(Ptr{Cvoid}, src)
        memmove(pdest, psrc, aligned_sizeof(T) * n)
        @_gc_preserve_end tdest
        @_gc_preserve_end tsrc
    else
        ccall(:jl_genericmemory_copyto, Cvoid, (Any, Ptr{Cvoid}, Any, Ptr{Cvoid}, Int), dest.mem, dest.ptr_or_offset, src.mem, src.ptr_or_offset, Int(n))
    end
    return dest
end

function unsafe_copyto!(dest::GenericMemoryRef, src::GenericMemoryRef, n)
    n == 0 && return dest
    @boundscheck memoryref(dest, n), memoryref(src, n)
    unsafe_copyto!(dest.mem, memoryrefoffset(dest), src.mem, memoryrefoffset(src), n)
    return dest
end

function unsafe_copyto!(dest::Memory{T}, doffs, src::Memory{T}, soffs, n) where{T}
    n == 0 && return dest
    unsafe_copyto!(memoryref(dest, doffs), memoryref(src, soffs), n)
    return dest
end

#fallback method when types don't match
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

function copy(a::T) where {T<:Memory}
    # `copy` only throws when the size exceeds the max allocation size,
    # but since we're copying an existing array, we're guaranteed that this will not happen.
    @_nothrow_meta
    newmem = T(undef, length(a))
    @inbounds unsafe_copyto!(newmem, 1, a, 1, length(a))
end

copyto!(dest::Memory, src::Memory) = copyto!(dest, 1, src, 1, length(src))
function copyto!(dest::Memory, doffs::Integer, src::Memory, soffs::Integer, n::Integer)
    n < 0 && _throw_argerror("Number of elements to copy must be non-negative.")
    unsafe_copyto!(dest, doffs, src, soffs, n)
    return dest
end


## Constructors ##

similar(a::GenericMemory) =
    typeof(a)(undef, length(a))
similar(a::GenericMemory{kind,<:Any,AS}, T::Type) where {kind,AS} =
    GenericMemory{kind,T,AS}(undef, length(a))
similar(a::GenericMemory, m::Int) =
    typeof(a)(undef, m)
similar(a::GenericMemory{kind,<:Any,AS}, T::Type, dims::Dims{1}) where {kind,AS} =
    GenericMemory{kind,T,AS}(undef, dims[1])
similar(a::GenericMemory, dims::Dims{1}) =
    typeof(a)(undef, dims[1])

function fill!(a::Union{Memory{UInt8}, Memory{Int8}}, x::Integer)
    t = @_gc_preserve_begin a
    p = unsafe_convert(Ptr{Cvoid}, a)
    T = eltype(a)
    memset(p, x isa T ? x : convert(T, x), length(a) % UInt)
    @_gc_preserve_end t
    return a
end

## Conversions ##

convert(::Type{T}, a::AbstractArray) where {T<:Memory} = a isa T ? a : T(a)::T

promote_rule(a::Type{Memory{T}}, b::Type{Memory{S}}) where {T,S} = el_same(promote_type(T,S), a, b)

## Constructors ##

# constructors should make copies
Memory{T}(x::AbstractArray{S,1}) where {T,S} = copyto_axcheck!(Memory{T}(undef, size(x)), x)

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

function _setindex!(A::Memory{T}, x::T, i1::Int) where {T}
    ref = memoryrefnew(memoryref(A), i1, @_boundscheck)
    memoryrefset!(ref, x, :not_atomic, @_boundscheck)
    return A
end

function setindex!(A::Memory{T}, x, i1::Int) where {T}
    @_propagate_inbounds_meta
    val = x isa T ? x : convert(T,x)::T
    return _setindex!(A, val, i1)
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

# get, set(once), modify, swap and replace at index, atomically
function getindex_atomic(mem::GenericMemory, order::Symbol, i::Int)
    @_propagate_inbounds_meta
    memref = memoryref(mem, i)
    return memoryrefget(memref, order, @_boundscheck)
end

function setindex_atomic!(mem::GenericMemory, order::Symbol, val, i::Int)
    @_propagate_inbounds_meta
    T = eltype(mem)
    memref = memoryref(mem, i)
    return memoryrefset!(
        memref,
        val isa T ? val : convert(T, val)::T,
        order,
        @_boundscheck
    )
end

function setindexonce_atomic!(
    mem::GenericMemory,
    success_order::Symbol,
    fail_order::Symbol,
    val,
    i::Int,
)
    @_propagate_inbounds_meta
    T = eltype(mem)
    memref = memoryref(mem, i)
    return Core.memoryrefsetonce!(
        memref,
        val isa T ? val : convert(T, val)::T,
        success_order,
        fail_order,
        @_boundscheck
    )
end

function modifyindex_atomic!(mem::GenericMemory, order::Symbol, op, val, i::Int)
    @_propagate_inbounds_meta
    memref = memoryref(mem, i)
    return Core.memoryrefmodify!(memref, op, val, order, @_boundscheck)
end

function swapindex_atomic!(mem::GenericMemory, order::Symbol, val, i::Int)
    @_propagate_inbounds_meta
    T = eltype(mem)
    memref = memoryref(mem, i)
    return Core.memoryrefswap!(
        memref,
        val isa T ? val : convert(T, val)::T,
        order,
        @_boundscheck
    )
end

function replaceindex_atomic!(
    mem::GenericMemory,
    success_order::Symbol,
    fail_order::Symbol,
    expected,
    desired,
    i::Int,
)
    @_propagate_inbounds_meta
    T = eltype(mem)
    memref = memoryref(mem, i)
    return Core.memoryrefreplace!(
        memref,
        expected,
        desired isa T ? desired : convert(T, desired)::T,
        success_order,
        fail_order,
        @_boundscheck,
    )
end
