# This file is a part of Julia. License is MIT: https://julialang.org/license

# Copied from package NonResizableVectors.jl:
#
# * https://github.com/JuliaArrays/NonResizableVectors.jl
function memory_type(::Val{isatomic}, ::Type{T}, ::Val{addrspace}) where {isatomic, T, addrspace}
    GenericMemory{isatomic::Symbol, T, addrspace::Core.AddrSpace}
end
struct GenericMemoryVector{isatomic, T, addrspace} <: AbstractArray{T,1}
    memory::GenericMemory{isatomic, T, addrspace}
    function GenericMemoryVector{isatomic, T, addrspace}(::UndefInitializer, n::Int) where {isatomic, T, addrspace}
        mt = memory_type(Val(isatomic), T, Val(addrspace))
        memory = mt(undef, n)
        new(memory)
    end
end
const MemoryVector = GenericMemoryVector{:not_atomic, T, Core.CPU} where {T}
function size(x::MemoryVector)
    size(x.memory)
end
function getindex(x::MemoryVector, index::Int)
    @_propagate_inbounds_meta
    @boundscheck checkbounds(x, index)
    @inbounds x.memory[index]
end
function setindex!(x::MemoryVector, element, index::Int)
    @_propagate_inbounds_meta
    @boundscheck checkbounds(x, index)
    @inbounds x.memory[index] = element
    x
end
function isassigned(x::MemoryVector, index::Int)
    @_propagate_inbounds_meta
    isassigned(x.memory, index)::Bool
end
function parent(x::MemoryVector)
    x.memory
end
struct GenericMemoryRefVectorImm{isatomic, T, addrspace} <: AbstractArray{T,1}
    memory_ref::GenericMemoryRef{isatomic, T, addrspace}
    function GenericMemoryRefVectorImm{isatomic, T, addrspace}(::UndefInitializer, n::Int) where {isatomic, T, addrspace}
        mt = memory_type(Val(isatomic), T, Val(addrspace))
        memory = mt(undef, n)
        memory_ref = memoryref(memory)
        new(memory_ref)
    end
end
mutable struct GenericMemoryRefVectorMut{isatomic, T, addrspace} <: AbstractArray{T,1}
    const memory_ref::GenericMemoryRef{isatomic, T, addrspace}
    function GenericMemoryRefVectorMut{isatomic, T, addrspace}(::UndefInitializer, n::Int) where {isatomic, T, addrspace}
        mt = memory_type(Val(isatomic), T, Val(addrspace))
        memory = mt(undef, n)
        memory_ref = memoryref(memory)
        new(memory_ref)
    end
end
const GenericMemoryRefVector = Union{GenericMemoryRefVectorImm{isatomic, T, addrspace}, GenericMemoryRefVectorMut{isatomic, T, addrspace}} where {isatomic, T, addrspace}
const MemoryRefVector = GenericMemoryRefVector{:not_atomic, T, Core.CPU} where {T}
const MemoryRefVectorImm = GenericMemoryRefVectorImm{:not_atomic, T, Core.CPU} where {T}
const MemoryRefVectorMut = GenericMemoryRefVectorMut{:not_atomic, T, Core.CPU} where {T}
function validated_memory_ref(x::GenericMemoryRefVector)
    x.memory_ref
end
function size(x::MemoryRefVector)
    memory_ref = validated_memory_ref(x)
    i = 1  # more generally correct: `i = memoryindex(memory_ref)`
    m = memory_ref.mem
    ml = length(m)
    l = ml - i + 1
    (l,)
end
function getindex(x::MemoryRefVector, index::Int)
    @_propagate_inbounds_meta
    @boundscheck checkbounds(x, index)
    memory_ref = validated_memory_ref(x)
    memory_ref_with_offset = @inbounds memoryref(memory_ref, index)
    memory_ref_with_offset[]
end
function setindex!(x::MemoryRefVector, element, index::Int)
    @_propagate_inbounds_meta
    @boundscheck checkbounds(x, index)
    memory_ref = validated_memory_ref(x)
    memory_ref_with_offset = @inbounds memoryref(memory_ref, index)
    memory_ref_with_offset[] = element
    x
end
function isassigned(x::MemoryRefVector, index::Int)
    @_propagate_inbounds_meta
    if !checkbounds(Bool, x, index)
        return false
    end
    memory_ref = validated_memory_ref(x)
    memory_ref_with_offset = @inbounds memoryref(memory_ref, index)
    isassigned(memory_ref_with_offset)::Bool
end
function parent(x::MemoryRefVector)
    parent(x.memory_ref)
end
const NonResizableVector = Union{MemoryVector{T}, MemoryRefVector{T}} where {T}
function IndexStyle(::Type{<:NonResizableVector})
    IndexLinear()
end
function checkbounds(x::NonResizableVector, indices...)
    checkbounds_lightboundserror(x, indices...)
end
function iterate(x::NonResizableVector, index = 1)
    index = index::Int
    if checkbounds(Bool, x, index)
        ((@inbounds x[index]), index + 1)
    else
        nothing
    end
end
function dataids(x::NonResizableVector)
    dataids(parent(x))  # forward to `dataids(::Memory)`
end
