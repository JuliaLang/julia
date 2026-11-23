# This file is a part of Julia. License is MIT: https://julialang.org/license

struct SummarySize
    seen::Set{Ptr{Cvoid}}
    frontier_x::Vector{Any}
    frontier_i::Vector{Int}
    count::Bool
end

nth_pointer_isdefined(obj, i::Int) = ccall(:jl_nth_pointer_isdefined, Cint, (Any, Csize_t), obj, i-1) != 0
get_nth_pointer(obj, i::Int) = ccall(:jl_get_nth_pointer, Any, (Any, Csize_t), obj, i-1)

"""
    Base.summarysize(obj; count = false, exclude=Union{...}, chargeall=Union{...})::Int

Compute all unique objects reachable from the argument and return either their size in
memory (in bytes) or the number of allocations they span.

# Keyword Arguments
- `count`: if false, return the total size of the objects in memory. if true, return the
  number of allocations spanned by the object.
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

julia> Base.summarysize(Core.svec(1.0, "testing", true); count=true)
4
```
"""
function summarysize(obj;
                     count::Bool = false,
                     exclude = Union{DataType, Core.TypeName, Core.MethodInstance},
                     chargeall = Union{Core.TypeMapEntry, Method})
    @nospecialize obj
    ss = SummarySize(Set{Ptr{Cvoid}}(), Any[], Int[], count)
    return _summarysize_frontier(ss, ss(obj)::Int, exclude, chargeall)
end

# specialized on the exclude and chargeall types, so checking them is cheap
function _summarysize_frontier(ss::SummarySize, size::Int, ::Type{Excl}, ::Type{Chg}) where {Excl, Chg}
    mem = nothing # the memory whose layout is cached below
    mem_inline = false
    mem_np = 0
    mem_len = 0
    while !isempty(ss.frontier_x)
        # DFS heap traversal of everything without a specialization
        # BFS heap traversal of anything with a specialization
        x = ss.frontier_x[end]
        i = ss.frontier_i[end]
        val = nothing
        if isa(x, Core.SimpleVector)
            nf = length(x)
            if isassigned(x, i)
                val = x[i]
            end
        elseif isa(x, Core.CancellationTokenSource)
            # the strong references of a source are its (hidden, trailing)
            # parent links, enumerated here rather than via the layout
            nf = Int(x.nparents)
            val = _cancel_parent(x, i)
        elseif isa(x, Core.WaitEntryN)
            # `task` plus the (strong) owner/next pair of each hidden
            # trailing wait slot
            ns = _nslots(x)
            nf = 1 + 2 * ns
            if i == 1
                t = @atomic :monotonic x.task
                t === nothing || (val = t)
            else
                si, k = divrem(i - 2, 2)
                slot = slots(x)[si + 1]
                v = k == 0 ? slot.owner : slot.next
                v === nothing || (val = v)
            end
        elseif isa(x, GenericMemory)
            if x !== mem
                mem = x
                T = eltype(x)
                mem_inline = allocatedinline(T)
                mem_np = mem_inline ? datatype_npointers(T) : 1
                mem_len = length(x)
            end
            if !mem_inline
                nf = mem_len
                ptr = unsafe_load(Ptr{Ptr{Cvoid}}(getfield(x, :ptr)::Ptr{Cvoid}), i)
                ptr == C_NULL || (val = unsafe_pointer_to_objref(ptr))
            else
                np = mem_np
                nf = mem_len * np
                idx = (i-1) ÷ np + 1
                if @inbounds @inline isassigned(x, idx)
                    elt = x[idx]
                    p = (i-1) % np + 1
                    if nth_pointer_isdefined(elt, p)
                        val = get_nth_pointer(elt, p)
                    end
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
        if val !== nothing && !isa(val, Module) && (!isa(val, Excl) || isa(x, Chg))
            size += ss(val)::Int
        end
    end
    return size
end

(ss::SummarySize)(@nospecialize obj) = _summarysize(ss, obj, ss.count)
# define the general case separately to make sure it is not specialized for every type
@noinline function _summarysize(ss::SummarySize, @nospecialize(obj), count::Bool)
    issingletontype(typeof(obj)) && return 0
    # NOTE: this attempts to discover multiple copies of the same immutable value,
    # and so is somewhat approximate.
    key = ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), obj)
    in!(key, ss.seen) && return 0
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
    return count ? 1 : sz
end

(::SummarySize)(obj::Symbol) = 0
(::SummarySize)(obj::SummarySize) = 0

function (ss::SummarySize)(obj::String)
    key = ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), obj)
    in!(key, ss.seen) && return 0
    return (ss.count ? 1 : (Core.sizeof(Int) + Core.sizeof(obj)))
end

function (ss::SummarySize)(obj::DataType)
    key = pointer_from_objref(obj)
    in!(key, ss.seen) && return 0
    size::Int = ss.count ? 1 : sizeof(DataType)
    size += ss(obj.parameters)::Int
    if isdefined(obj, :types)
        size += ss(obj.types)::Int
    end
    return size
end

function (ss::SummarySize)(obj::Core.TypeName)
    key = pointer_from_objref(obj)
    in!(key, ss.seen) && return 0
    return (ss.count ? 1 : Core.sizeof(obj))
end

function (ss::SummarySize)(obj::GenericMemory)
    in!(pointer_from_objref(obj), ss.seen) && return 0
    headersize = 2 * sizeof(Int)
    size::Int = (ss.count ? 1 : headersize)
    datakey = unsafe_convert(Ptr{Cvoid}, obj)
    if !in!(datakey, ss.seen)
        if !ss.count
            size += sizeof(obj)
        elseif pointer_from_objref(obj) + 16 != datakey
            size += 1
        end
        T = eltype(obj)
        if !isempty(obj) && T !== Symbol && (!allocatedinline(T) || (T isa DataType && !datatype_pointerfree(T)))
            push!(ss.frontier_x, obj)
            push!(ss.frontier_i, 1)
        end
    end
    return size
end

function (ss::SummarySize)(obj::Core.SimpleVector)
    key = pointer_from_objref(obj)
    in!(key, ss.seen) && return 0
    size::Int = (ss.count ? 1 : Core.sizeof(obj))
    if !isempty(obj)
        push!(ss.frontier_x, obj)
        push!(ss.frontier_i, 1)
    end
    return size
end

function (ss::SummarySize)(obj::Module)
    in!(pointer_from_objref(obj), ss.seen) && return 0
    size::Int = (ss.count ? 1 : Core.sizeof(obj))
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

function (ss::SummarySize)(obj::Core.CancellationTokenSource)
    key = pointer_from_objref(obj)
    in!(key, ss.seen) && return 0
    # Variable-sized: Core.sizeof includes the trailing parent link entries.
    # The (strong) parent references are traversed through the iterative
    # frontier (see its branch in the frontier loop), which keeps deep chains
    # off the stack and honors `exclude`; the child list is weak - a source
    # does not keep its children alive - so it is deliberately not followed.
    if Int(obj.nparents) > 0
        push!(ss.frontier_x, obj)
        push!(ss.frontier_i, 1)
    end
    return ss.count ? 1 : Core.sizeof(obj)
end

function (ss::SummarySize)(obj::Core.WaitEntryN)
    key = pointer_from_objref(obj)
    in!(key, ss.seen) && return 0
    # Variable-sized: Core.sizeof includes the trailing wait slots, whose
    # strong owner/next references (invisible to the layout) are traversed
    # through the iterative frontier alongside `task`.
    push!(ss.frontier_x, obj)
    push!(ss.frontier_i, 1)
    return ss.count ? 1 : Core.sizeof(obj)
end

function (ss::SummarySize)(obj::Task)
    in!(pointer_from_objref(obj), ss.seen) && return 0
    size::Int = (ss.count ? 1 : Core.sizeof(obj))
    if isdefined(obj, :code)
        size += ss(obj.code)::Int
    end
    size += ss(obj.storage)::Int
    size += ss(obj.donenotify)::Int
    size += ss(obj.result)::Int
    # TODO: add stack size, and possibly traverse stack roots
    return size
end

(ss::SummarySize)(obj::BigInt) = _summarysize(ss, obj, ss.count) + (ss.count ? 1 : obj.alloc * sizeof(GMP.Limb))
