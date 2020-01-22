# This file is a part of Julia. License is MIT: https://julialang.org/license

# deep copying

# Note: deepcopy_internal(::Any, ::IdDict) is
#       only exposed for specialization by libraries

"""
    deepcopy(x)

Create a deep copy of `x`: everything is copied recursively, resulting in a fully
independent object. For example, deep-copying an array produces a new array whose elements
are deep copies of the original elements. Calling `deepcopy` on an object should generally
have the same effect as serializing and then deserializing it.

As a special case, functions can only be actually deep-copied if they are anonymous,
otherwise they are just copied. The difference is only relevant in the case of closures,
i.e. functions which may contain hidden internal references.

While it isn't normally necessary, user-defined types can override the default `deepcopy`
behavior by defining a specialized version of the function
`deepcopy_internal(x::T, dict::IdDict)` (which shouldn't otherwise be used),
where `T` is the type to be specialized for, and `dict` keeps track of objects copied
so far within the recursion. Within the definition, `deepcopy_internal` should be used
in place of `deepcopy`, and the `dict` variable should be
updated as appropriate before returning.
"""
function deepcopy(x)
    isbitstype(typeof(x)) && return x
    return deepcopy_internal(x, IdDict())::typeof(x)
end

deepcopy_internal(x::Union{Symbol,Core.MethodInstance,Method,GlobalRef,DataType,Union,UnionAll,Task,Regex},
                  stackdict::IdDict) = x
deepcopy_internal(x::Tuple, stackdict::IdDict) =
    ntuple(i->deepcopy_internal(x[i], stackdict), length(x))
deepcopy_internal(x::Module, stackdict::IdDict) = error("deepcopy of Modules not supported")

function deepcopy_internal(x::SimpleVector, stackdict::IdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    y = Core.svec(Any[deepcopy_internal(x[i], stackdict) for i = 1:length(x)]...)
    stackdict[x] = y
    return y
end

function deepcopy_internal(x::String, stackdict::IdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    y = GC.@preserve x unsafe_string(pointer(x), sizeof(x))
    stackdict[x] = y
    return y
end

function deepcopy_internal(@nospecialize(x), stackdict::IdDict)
    T = typeof(x)::DataType
    nf = nfields(x)
    if T.mutable
        if haskey(stackdict, x)
            return stackdict[x]
        end
        y = ccall(:jl_new_struct_uninit, Any, (Any,), T)
        stackdict[x] = y
        for i in 1:nf
            if isdefined(x, i)
                xi = getfield(x, i)
                xi = deepcopy_internal(xi, stackdict)::typeof(xi)
                ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), y, i-1, xi)
            end
        end
    elseif nf == 0 || isbitstype(T)
        y = x
    else
        flds = Vector{Any}(undef, nf)
        for i in 1:nf
            if isdefined(x, i)
                xi = getfield(x, i)
                xi = deepcopy_internal(xi, stackdict)::typeof(xi)
                flds[i] = xi
            else
                nf = i - 1 # rest of tail must be undefined values
                break
            end
        end
        y = ccall(:jl_new_structv, Any, (Any, Ptr{Any}, UInt32), T, flds, nf)
    end
    return y::T
end

function deepcopy_internal(x::Array, stackdict::IdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    _deepcopy_array_t(x, eltype(x), stackdict)
end

function _deepcopy_array_t(@nospecialize(x), T, stackdict::IdDict)
    if isbitstype(T)
        return (stackdict[x]=copy(x))
    end
    dest = similar(x)
    stackdict[x] = dest
    for i = 1:(length(x)::Int)
        if ccall(:jl_array_isassigned, Cint, (Any, Csize_t), x, i-1) != 0
            xi = ccall(:jl_arrayref, Any, (Any, Csize_t), x, i-1)
            if !isbits(xi)
                xi = deepcopy_internal(xi, stackdict)::typeof(xi)
            end
            ccall(:jl_arrayset, Cvoid, (Any, Any, Csize_t), dest, xi, i-1)
        end
    end
    return dest
end

function deepcopy_internal(x::Union{Dict,IdDict}, stackdict::IdDict)
    if haskey(stackdict, x)
        return stackdict[x]::typeof(x)
    end

    if isbitstype(eltype(x))
        return (stackdict[x] = copy(x))
    end

    dest = empty(x)
    stackdict[x] = dest
    for (k, v) in x
        dest[deepcopy_internal(k, stackdict)] = deepcopy_internal(v, stackdict)
    end
    dest
end
