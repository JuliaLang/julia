# deep copying

# Note: deepcopy_internal(::Any, ::ObjectIdDict) is
#       only exposed for specialization by libraries

deepcopy(x) = deepcopy_internal(x, ObjectIdDict())

deepcopy_internal(x::Union(Symbol,LambdaStaticData,TopNode,QuoteNode,
                           DataType,UnionType,Task),
                  stackdict::ObjectIdDict) = x
deepcopy_internal(x::Tuple, stackdict::ObjectIdDict) =
    ntuple(length(x), i->deepcopy_internal(x[i], stackdict))
deepcopy_internal(x::Module, stackdict::ObjectIdDict) = error("deepcopy of Modules not supported")

function deepcopy_internal(x::Function, stackdict::ObjectIdDict)
    if isa(x.env, Union(MethodTable, Symbol)) || x.env === ()
        return x
    end
    invoke(deepcopy_internal, (Any, ObjectIdDict), x, stackdict)
end

function deepcopy_internal(x, stackdict::ObjectIdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    _deepcopy_t(x, typeof(x), stackdict)
end

function _deepcopy_t(x, T::DataType, stackdict::ObjectIdDict)
    isbits(T) | isempty(T.names) && return x
    if T.mutable
        y = ccall(:jl_new_struct_uninit, Any, (Any,), T)
        stackdict[x] = y
        for i in 1:length(T.names)
            if isdefined(x,i)
                y.(i) = deepcopy_internal(x.(i), stackdict)
            end
        end
    else
        fields = Any[deepcopy_internal(x.(i), stackdict) for i in 1:length(T.names)]
        y = ccall(:jl_new_structv, Any, (Any, Ptr{Void}, UInt32),
                  T, pointer(fields), length(fields))
    end
    return y::T
end

function deepcopy_internal(x::Array, stackdict::ObjectIdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    _deepcopy_array_t(x, eltype(x), stackdict)
end

function _deepcopy_array_t(x, T, stackdict::ObjectIdDict)
    if isbits(T)
        return copy(x)
    end
    dest = similar(x)
    stackdict[x] = dest
    for i=1:length(x)
        if isdefined(x,i)
            arrayset(dest, deepcopy_internal(x[i], stackdict), i)
        end
    end
    return dest
end
