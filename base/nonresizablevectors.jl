# This file is a part of Julia. License is MIT: https://julialang.org/license

# Copied from package NonResizableVectors.jl:
#
# * https://github.com/JuliaArrays/NonResizableVectors.jl
module NonResizableVectors
    module Miscellaneous
        export vector_supertype, memory_type
        const vector_supertype = AbstractVector  # TODO: consider switching to `DenseVector`
        function memory_type(::Val{isatomic}, ::Type{T}, ::Val{addrspace}) where {isatomic, T, addrspace}
            GenericMemory{isatomic::Symbol, T, addrspace::Core.AddrSpace}
        end
    end
    module GenericMemoryVectors
        using ..Miscellaneous
        export GenericMemoryVector, MemoryVector
        struct GenericMemoryVector{isatomic, T, addrspace} <: vector_supertype{T}
            memory::GenericMemory{isatomic, T, addrspace}
            function GenericMemoryVector{isatomic, T, addrspace}(::UndefInitializer, n::Int) where {isatomic, T, addrspace}
                mt = memory_type(Val(isatomic), T, Val(addrspace))
                memory = mt(undef, n)
                new(memory)
            end
        end
        const MemoryVector = GenericMemoryVector{:not_atomic, T, Core.CPU} where {T}
        function Base.size(x::MemoryVector)
            size(x.memory)
        end
        Base.@propagate_inbounds function Base.getindex(x::MemoryVector, index::Int)
            @boundscheck checkbounds(x, index)
            @inbounds x.memory[index]
        end
        Base.@propagate_inbounds function Base.setindex!(x::MemoryVector, element, index::Int)
            @boundscheck checkbounds(x, index)
            @inbounds x.memory[index] = element
            x
        end
        Base.@propagate_inbounds function Base.isassigned(x::MemoryVector, index::Int)
            isassigned(x.memory, index)::Bool
        end
        function Base.parent(x::MemoryVector)
            x.memory
        end
    end
    module GenericMemoryRefVectors
        using ..Miscellaneous
        export
            GenericMemoryRefVectorImm, GenericMemoryRefVectorMut, GenericMemoryRefVector,
            MemoryRefVectorImm, MemoryRefVectorMut, MemoryRefVector
        struct GenericMemoryRefVectorImm{isatomic, T, addrspace} <: vector_supertype{T}
            memory_ref::GenericMemoryRef{isatomic, T, addrspace}
            function GenericMemoryRefVectorImm{isatomic, T, addrspace}(::UndefInitializer, n::Int) where {isatomic, T, addrspace}
                mt = memory_type(Val(isatomic), T, Val(addrspace))
                memory = mt(undef, n)
                memory_ref = memoryref(memory)
                new(memory_ref)
            end
        end
        mutable struct GenericMemoryRefVectorMut{isatomic, T, addrspace} <: vector_supertype{T}
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
        function Base.size(x::MemoryRefVector)
            memory_ref = validated_memory_ref(x)
            i = 1  # more generally correct: `i = Base.memoryindex(memory_ref)`
            m = memory_ref.mem
            ml = length(m)
            l = ml - i + 1
            (l,)
        end
        Base.@propagate_inbounds function Base.getindex(x::MemoryRefVector, index::Int)
            @boundscheck checkbounds(x, index)
            memory_ref = validated_memory_ref(x)
            memory_ref_with_offset = @inbounds memoryref(memory_ref, index)
            memory_ref_with_offset[]
        end
        Base.@propagate_inbounds function Base.setindex!(x::MemoryRefVector, element, index::Int)
            @boundscheck checkbounds(x, index)
            memory_ref = validated_memory_ref(x)
            memory_ref_with_offset = @inbounds memoryref(memory_ref, index)
            memory_ref_with_offset[] = element
            x
        end
        Base.@propagate_inbounds function Base.isassigned(x::MemoryRefVector, index::Int)
            if !checkbounds(Bool, x, index)
                return false
            end
            memory_ref = validated_memory_ref(x)
            memory_ref_with_offset = @inbounds memoryref(memory_ref, index)
            isassigned(memory_ref_with_offset)::Bool
        end
        function Base.parent(x::MemoryRefVector)
            parent(x.memory_ref)
        end
    end
    using .GenericMemoryVectors, .GenericMemoryRefVectors
    using ..LightBoundsErrors: checkbounds_lightboundserror
    export
        MemoryVector, MemoryRefVectorImm, MemoryRefVectorMut
    const NonResizableVector = Union{MemoryVector{T}, MemoryRefVector{T}} where {T}
    function Base.IndexStyle(::Type{<:NonResizableVector})
        IndexLinear()
    end
    function Base.checkbounds(x::NonResizableVector, indices...)
        checkbounds_lightboundserror(x, indices...)
    end
    function Base.iterate(x::NonResizableVector, index = 1)
        index = index::Int
        if checkbounds(Bool, x, index)
            ((@inbounds x[index]), index + 1)
        else
            nothing
        end
    end
    if isdefined(Base, :dataids)  # not public: https://github.com/JuliaLang/julia/issues/51753
        function Base.dataids(x::NonResizableVector)
            Base.dataids(parent(x))  # forward to `dataids(::Memory)`
        end
    end
end
