# This file is a part of Julia. License is MIT: https://julialang.org/license

struct SummarySize
    seen::IdDict{Any,Any}
    frontier_x::Vector{Any}
    frontier_i::Vector{Int}
    exclude::Any
    chargeall::Any
end

nth_pointer_isdefined(obj, i::Int) = ccall(:jl_nth_pointer_isdefined, Cint, (Any, Csize_t), obj, i-1) != 0
get_nth_pointer(obj, i::Int) = ccall(:jl_get_nth_pointer, Any, (Any, Csize_t), obj, i-1)

"""
    Base.summarysize(obj; exclude=Union{...}, chargeall=Union{...}) -> Int

Compute the amount of memory, in bytes, used by all unique objects reachable from the argument.

# Keyword Arguments
- `exclude`: specifies the types of objects to exclude from the traversal.
- `chargeall`: specifies the types of objects to always charge the size of all of their
  fields, even if those fields would normally be excluded.

See also [`sizeof`](@ref).

# Examples
```jldoctest
julia> Base.summarysize(1.0)
8

julia> Base.summarysize(Ref(rand(100)))
848

julia> sizeof(Ref(rand(100)))
8
```
"""
function summarysize(obj;
                     exclude = Union{DataType, Core.TypeName, Core.MethodInstance},
                     chargeall = Union{Core.TypeMapEntry, Method})
    @nospecialize obj exclude chargeall
    ss = SummarySize(IdDict(), Any[], Int[], exclude, chargeall)
    size::Int = ss(obj)
    while !isempty(ss.frontier_x)
        # DFS heap traversal of everything without a specialization
        # BFS heap traversal of anything with a specialization
        x = ss.frontier_x[end]
        i = ss.frontier_i[end]
        val = nothing
        if isa(x, SimpleVector)
            nf = length(x)
            if isassigned(x, i)
                val = x[i]
            end
        elseif isa(x, GenericMemory)
            T = eltype(x)
            if Base.allocatedinline(T)
                np = datatype_npointers(T)
                nf = length(x) * np
                idx = (i-1) ÷ np + 1
                if @inbounds @inline isassigned(x, idx)
                    elt = x[idx]
                    p = (i-1) % np + 1
                    if nth_pointer_isdefined(elt, p)
                        val = get_nth_pointer(elt, p)
                    end
                end
            else
                nf = length(x)
                if @inbounds @inline isassigned(x, i)
                    val = x[i]
                end
            end
        else
            nf = datatype_npointers(typeof(x))
            if nth_pointer_isdefined(x, i)
                val = get_nth_pointer(x, i)
            end
        end
        if nf > i
            ss.frontier_i[end] = i + 1
        else
            pop!(ss.frontier_x)
            pop!(ss.frontier_i)
        end
        if val !== nothing && !isa(val, Module) && (!isa(val, ss.exclude) || isa(x, ss.chargeall))
            size += ss(val)::Int
        end
    end
    return size
end

(ss::SummarySize)(@nospecialize obj) = _summarysize(ss, obj)
# define the general case separately to make sure it is not specialized for every type
@noinline function _summarysize(ss::SummarySize, @nospecialize obj)
    issingletontype(typeof(obj)) && return 0
    # NOTE: this attempts to discover multiple copies of the same immutable value,
    # and so is somewhat approximate.
    key = ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), obj)
    haskey(ss.seen, key) ? (return 0) : (ss.seen[key] = true)
    if datatype_npointers(typeof(obj)) > 0
        push!(ss.frontier_x, obj)
        push!(ss.frontier_i, 1)
    end
    if isa(obj, UnionAll) || isa(obj, Union)
        # black-list of items that don't have a Core.sizeof
        sz = 2 * sizeof(Int)
    else
        sz = Core.sizeof(obj)
    end
    if sz == 0
        # 0-field mutable structs are not unique
        return gc_alignment(0)
    end
    return sz
end

(::SummarySize)(obj::Symbol) = 0
(::SummarySize)(obj::SummarySize) = 0

function (ss::SummarySize)(obj::String)
    key = ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), obj)
    haskey(ss.seen, key) ? (return 0) : (ss.seen[key] = true)
    return Core.sizeof(Int) + Core.sizeof(obj)
end

function (ss::SummarySize)(obj::DataType)
    key = pointer_from_objref(obj)
    haskey(ss.seen, key) ? (return 0) : (ss.seen[key] = true)
    size::Int = 7 * Core.sizeof(Int) + 6 * Core.sizeof(Int32)
    size += 4 * nfields(obj) + ifelse(Sys.WORD_SIZE == 64, 4, 0)
    size += ss(obj.parameters)::Int
    if isdefined(obj, :types)
        size += ss(obj.types)::Int
    end
    return size
end

function (ss::SummarySize)(obj::Core.TypeName)
    key = pointer_from_objref(obj)
    haskey(ss.seen, key) ? (return 0) : (ss.seen[key] = true)
    return Core.sizeof(obj) + (isdefined(obj, :mt) ? ss(obj.mt) : 0)
end

function (ss::SummarySize)(obj::GenericMemory)
    haskey(ss.seen, obj) ? (return 0) : (ss.seen[obj] = true)
    headersize = 2*sizeof(Int)
    size::Int = headersize
    datakey = unsafe_convert(Ptr{Cvoid}, obj)
    if !haskey(ss.seen, datakey)
        ss.seen[datakey] = true
        size += sizeof(obj)
        T = eltype(obj)
        if !isempty(obj) && T !== Symbol && (!Base.allocatedinline(T) || (T isa DataType && !Base.datatype_pointerfree(T)))
            push!(ss.frontier_x, obj)
            push!(ss.frontier_i, 1)
        end
    end
    return size
end

function (ss::SummarySize)(obj::SimpleVector)
    key = pointer_from_objref(obj)
    haskey(ss.seen, key) ? (return 0) : (ss.seen[key] = true)
    size::Int = Core.sizeof(obj)
    if !isempty(obj)
        push!(ss.frontier_x, obj)
        push!(ss.frontier_i, 1)
    end
    return size
end

function (ss::SummarySize)(obj::Module)
    haskey(ss.seen, obj) ? (return 0) : (ss.seen[obj] = true)
    size::Int = Core.sizeof(obj)
    for binding in names(obj, all = true)
        if isdefined(obj, binding) && !isdeprecated(obj, binding)
            value = getfield(obj, binding)
            if !isa(value, Module) || parentmodule(value) === obj
                size += ss(value)::Int
                if isa(value, UnionAll)
                    value = unwrap_unionall(value)
                end
                if isa(value, DataType) && parentmodule(value) === obj && nameof(value) === binding
                    # charge a TypeName to its module (but not to the type)
                    size += ss(value.name)::Int
                end
            end
        end
    end
    return size
end

function (ss::SummarySize)(obj::Task)
    haskey(ss.seen, obj) ? (return 0) : (ss.seen[obj] = true)
    size::Int = Core.sizeof(obj)
    if isdefined(obj, :code)
        size += ss(obj.code)::Int
    end
    size += ss(obj.storage)::Int
    size += ss(obj.donenotify)::Int
    size += ss(obj.result)::Int
    # TODO: add stack size, and possibly traverse stack roots
    return size
end

(ss::SummarySize)(obj::BigInt) = _summarysize(ss, obj) + obj.alloc*sizeof(Base.GMP.Limb)
