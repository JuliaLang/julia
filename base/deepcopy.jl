# This file is a part of Julia. License is MIT: http://julialang.org/license

# deep copying

# Note: deepcopy_internal(::Any, ::ObjectIdDict) is
#       only exposed for specialization by libraries

deepcopy(x) = deepcopy_internal(x, ObjectIdDict())::typeof(x)

deepcopy_internal(x::Union{Symbol,Core.MethodInstance,Method,GlobalRef,DataType,Union,Task},
                  stackdict::ObjectIdDict) = x
deepcopy_internal(x::Tuple, stackdict::ObjectIdDict) =
    ntuple(i->deepcopy_internal(x[i], stackdict), length(x))
deepcopy_internal(x::Module, stackdict::ObjectIdDict) = error("deepcopy of Modules not supported")

function deepcopy_internal(x::SimpleVector, stackdict::ObjectIdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    y = Core.svec(Any[deepcopy_internal(x[i], stackdict) for i = 1:length(x)]...)
    stackdict[x] = y
    return y
end

function deepcopy_internal(x::ANY, stackdict::ObjectIdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    T = typeof(x)::DataType
    nf = nfields(T)
    (isbits(T) || nf == 0) && return x
    y = ccall(:jl_new_struct_uninit, Any, (Any,), T)
    if T.mutable
        stackdict[x] = y
    end
    for i in 1:nf
        if isdefined(x,i)
            ccall(:jl_set_nth_field, Void, (Any, Csize_t, Any), y, i-1,
                  deepcopy_internal(getfield(x,i), stackdict))
        end
    end
    return y::T
end

function deepcopy_internal(x::Array, stackdict::ObjectIdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    _deepcopy_array_t(x, eltype(x), stackdict)
end

function _deepcopy_array_t(x::ANY, T, stackdict::ObjectIdDict)
    if isbits(T)
        return (stackdict[x]=copy(x))
    end
    dest = similar(x)
    stackdict[x] = dest
    for i=1:length(x)
        if isassigned(x,i)
            arrayset(dest, deepcopy_internal(x[i], stackdict), i)
        end
    end
    return dest
end
