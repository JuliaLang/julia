# This file is a part of Julia. License is MIT: https://julialang.org/license

# deep copying

# Note: deepcopy_internal(::Any, ::ObjectIdDict) is
#       only exposed for specialization by libraries

"""
    deepcopy(x)

Create a deep copy of `x`: everything is copied recursively, resulting usually in a fully
independent object. For example, deep-copying an array produces a new array whose elements
are deep copies of the original elements. Calling `deepcopy` on an object should generally
have the same effect as serializing and then deserializing it.

Julia objects can roughly be categorized into two groups:
- "containers", which contain other objects: this includes collections like arrays, sets, etc., but
  also composite types which contain objects corresponding to their fields;
- "atoms", which don't contain other objects, like `1` or `"a string"`, but also objects of types
  like `BigInt`, which, despite internally being implemented as `mutable struct`, are conceptually
  considered as immutables (there is no public API to mutate them);

Given this definition, a rule of thumb for defining `deepcopy` is as follows:
- for a container `x`, `deepcopy(x)` copies the outer structure of `x` and populates it
  recursively with deepcopies of the objects contained in `x`;
- for an atom `x`, `deepcopy(x) = copy(x)`.

This means for example that for `x::BigInt`, we have `deepcopy(x) === x`; in particular this is a case
where calling `deepcopy` doensn't have the same effect as serializing and then deserializing.

As a special case, functions can only be actually deep-copied if they are anonymous,
otherwise they are just copied. The difference is only relevant in the case of closures,
i.e. functions which may contain hidden internal references.

While it isn't normally necessary, user-defined types can override the default `deepcopy`
behavior by defining a specialized version of the function
`deepcopy_internal(x::T, dict::ObjectIdDict)` (which shouldn't otherwise be used),
where `T` is the type to be specialized for, and `dict` keeps track of objects copied
so far within the recursion. Within the definition, `deepcopy_internal` should be used
in place of `deepcopy`, and the `dict` variable should be
updated as appropriate before returning.
"""
deepcopy(x) = deepcopy_internal(x, ObjectIdDict())::typeof(x)

deepcopy_internal(x::Union{Symbol,Core.MethodInstance,Method,GlobalRef,DataType,Union,Task,BigFloat,BigInt},
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

function deepcopy_internal(x::String, stackdict::ObjectIdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    y = @gc_preserve x unsafe_string(pointer(x), sizeof(x))
    stackdict[x] = y
    return y
end

function deepcopy_internal(@nospecialize(x), stackdict::ObjectIdDict)
    T = typeof(x)::DataType
    nf = nfields(x)
    (isbits(T) || nf == 0) && return x
    if haskey(stackdict, x)
        return stackdict[x]
    end
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

function _deepcopy_array_t(@nospecialize(x), T, stackdict::ObjectIdDict)
    if isbits(T)
        return (stackdict[x]=copy(x))
    end
    dest = similar(x)
    stackdict[x] = dest
    for i = 1:(length(x)::Int)
        if ccall(:jl_array_isassigned, Cint, (Any, Csize_t), x, i-1) != 0
            xi = ccall(:jl_arrayref, Any, (Any, Csize_t), x, i-1)
            if !isbits(typeof(xi))
                xi = deepcopy_internal(xi, stackdict)
            end
            ccall(:jl_arrayset, Void, (Any, Any, Csize_t), dest, xi, i-1)
        end
    end
    return dest
end

function deepcopy_internal(x::Dict, stackdict::ObjectIdDict)
    if haskey(stackdict, x)
        return stackdict[x]::typeof(x)
    end

    if isbits(eltype(x))
        return (stackdict[x] = copy(x))
    end

    dest = similar(x)
    stackdict[x] = dest
    for (k, v) in x
        dest[deepcopy_internal(k, stackdict)] = deepcopy_internal(v, stackdict)
    end
    dest
end
