# deep copying
deep_copy(x) = _deep_copy(x, ObjectIdDict())

_deep_copy(x::Union(Symbol,Function,LambdaStaticData,
              TopNode,QuoteNode,BitsKind,CompositeKind,AbstractKind,
              UnionKind), stackdict::ObjectIdDict) = x
_deep_copy(x::Tuple, stackdict::ObjectIdDict) =
    ntuple(length(x), i->_deep_copy(x[i], stackdict))
_deep_copy(x::Module, stackdict::ObjectIdDict) = error("deep_copy of Modules not supported")

function _deep_copy(x, stackdict::ObjectIdDict)
    if has(stackdict, x)
        return stackdict[x]
    end
    _deep_copy_t(x, typeof(x), stackdict)
end

_deep_copy_t(x, T::BitsKind, stackdict::ObjectIdDict) = x
function _deep_copy_t(x, T::CompositeKind, stackdict::ObjectIdDict)
    nf = length(T.names)
    dc_field(i::Int, stackdict) = _deep_copy(x.(T.names[i]), stackdict)

    ret = ccall(:jl_new_struct_uninit, Any, (Any,), T)
    stackdict[x] = ret
    for i=1:nf
        try
            ccall(:jl_set_nth_field, Any, (Any, Int, Any), ret, i-1, dc_field(i, stackdict))
        catch err
            # we ignore undefined references errors
            if !isa(err, UndefRefError)
                throw(err)
            end
        end
    end
    return ret
end
_deep_copy_t(x, T, stackdict::ObjectIdDict) =
    error("deep_copy of objects of type ", T, " not supported")


function _deep_copy(x::Array, stackdict::ObjectIdDict)
    if has(stackdict, x)
        return stackdict[x]
    end
    _deep_copy_array_t(x, eltype(x), stackdict)
end

_deep_copy_array_t(x, T::BitsKind, stackdict::ObjectIdDict) = copy(x)
function _deep_copy_array_t(x, T, stackdict::ObjectIdDict)
    dest = similar(x)
    stackdict[x] = dest
    for i=1:length(x)
        try
            # NOTE: this works around the performance problem caused by all
            # the doubled definitions of assign()
            arrayset(dest, i, _deep_copy(x[i], stackdict))
        catch err
            # we ignore undefined references errors
            if !isa(err, UndefRefError)
                throw(err)
            end
        end
    end
    return dest
end


