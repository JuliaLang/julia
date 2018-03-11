# This file is a part of Julia. License is MIT: https://julialang.org/license

struct SummarySize
    seen::IdDict{Any,Any}
    frontier_x::Vector{Any}
    frontier_i::Vector{Int}
    exclude::Any
    chargeall::Any
end

_nfields(@nospecialize x) = length(typeof(x).types)

"""
    Base.summarysize(obj; exclude=Union{...}, chargeall=Union{...}) -> Int

Compute the amount of memory used by all unique objects reachable from the argument.

# Keyword Arguments
- `exclude`: specifies the types of objects to exclude from the traversal.
- `chargeall`: specifies the types of objects to always charge the size of all of their
  fields, even if those fields would normally be excluded.
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
        elseif isa(x, Array)
            nf = length(x)
            if ccall(:jl_array_isassigned, Cint, (Any, UInt), x, i - 1) != 0
                val = x[i]
            end
        else
            nf = _nfields(x)
            ft = typeof(x).types
            if !isbits(ft[i]) && isdefined(x, i)
                val = getfield(x, i)
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
    # NOTE: this attempts to discover multiple copies of the same immutable value,
    # and so is somewhat approximate.
    key = ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), obj)
    haskey(ss.seen, key) ? (return 0) : (ss.seen[key] = true)
    if _nfields(obj) > 0
        push!(ss.frontier_x, obj)
        push!(ss.frontier_i, 1)
    end
    if isa(obj, UnionAll) || isa(obj, Union)
        # black-list of items that don't have a Core.sizeof
        return 2 * sizeof(Int)
    end
    return Core.sizeof(obj)
end

(::SummarySize)(obj::Symbol) = 0
(::SummarySize)(obj::SummarySize) = 0
(::SummarySize)(obj::String) = Core.sizeof(Int) + Core.sizeof(obj)

function (ss::SummarySize)(obj::DataType)
    key = pointer_from_objref(obj)
    haskey(ss.seen, key) ? (return 0) : (ss.seen[key] = true)
    size::Int = 7 * Core.sizeof(Int) + 6 * Core.sizeof(Int32)
    size += 4 * _nfields(obj) + ifelse(Sys.WORD_SIZE == 64, 4, 0)
    size += ss(obj.parameters)::Int
    size += ss(obj.types)::Int
    return size
end

function (ss::SummarySize)(obj::Core.TypeName)
    key = pointer_from_objref(obj)
    haskey(ss.seen, key) ? (return 0) : (ss.seen[key] = true)
    return Core.sizeof(obj) + (isdefined(obj, :mt) ? ss(obj.mt) : 0)
end

function (ss::SummarySize)(obj::Array)
    haskey(ss.seen, obj) ? (return 0) : (ss.seen[obj] = true)
    headersize = 4*sizeof(Int) + 8 + max(0, ndims(obj)-2)*sizeof(Int)
    size::Int = headersize
    datakey = unsafe_convert(Ptr{Cvoid}, obj)
    if !haskey(ss.seen, datakey)
        ss.seen[datakey] = true
        size += Core.sizeof(obj)
        if !isbits(eltype(obj)) && !isempty(obj)
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
                if isa(value, DataType) && value.name.module === obj && value.name.name === binding
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
    size += ss(obj.backtrace)::Int
    size += ss(obj.donenotify)::Int
    size += ss(obj.exception)::Int
    size += ss(obj.result)::Int
    # TODO: add stack size, and possibly traverse stack roots
    return size
end
