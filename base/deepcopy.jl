# deep copying
deepcopy(x) = _deepcopy(x, ObjectIdDict())

_deepcopy(x::Union(Symbol,Function,LambdaStaticData,
              TopNode,QuoteNode,BitsKind,CompositeKind,AbstractKind,
              UnionKind), stackdict::ObjectIdDict) = x
_deepcopy(x::Tuple, stackdict::ObjectIdDict) =
    ntuple(length(x), i->_deepcopy(x[i], stackdict))
_deepcopy(x::Module, stackdict::ObjectIdDict) = error("deepcopy of Modules not supported")

function _deepcopy(x, stackdict::ObjectIdDict)
    if has(stackdict, x)
        return stackdict[x]
    end
    _deepcopy_t(x, typeof(x), stackdict)
end

_deepcopy_t(x, T::BitsKind, stackdict::ObjectIdDict) = x
function _deepcopy_t(x, T::CompositeKind, stackdict::ObjectIdDict)
    nf = length(T.names)
    dc_field(i::Int, stackdict) = _deepcopy(x.(T.names[i]), stackdict)

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
_deepcopy_t(x, T, stackdict::ObjectIdDict) =
    error("deepcopy of objects of type ", T, " not supported")


function _deepcopy(x::Array, stackdict::ObjectIdDict)
    if has(stackdict, x)
        return stackdict[x]
    end
    _deepcopy_array_t(x, eltype(x), stackdict)
end

_deepcopy_array_t(x, T::BitsKind, stackdict::ObjectIdDict) = copy(x)
function _deepcopy_array_t(x, T, stackdict::ObjectIdDict)
    dest = similar(x)
    stackdict[x] = dest
    for i=1:length(x)
        try
            # NOTE: this works around the performance problem caused by all
            # the doubled definitions of assign()
            arrayset(dest, i, _deepcopy(x[i], stackdict))
        catch err
            # we ignore undefined references errors
            if !isa(err, UndefRefError)
                throw(err)
            end
        end
    end
    return dest
end


