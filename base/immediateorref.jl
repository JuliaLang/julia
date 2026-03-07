# This file is a part of Julia. License is MIT: https://julialang.org/license

# low-level helpers for the prototype ImmediateOrRef representation
function _taggedptr_check_type(@nospecialize(T))
    isa(T, Type) || error("ImmediateOrRef parameter must be a type in this prototype")
    allocatedinline(T) &&
        error("ImmediateOrRef parameter type must be represented as an ordinary boxed object in this prototype")
    return T
end
_taggedptr_from_raw(::Type{ImmediateOrRef{T}}, raw::UInt) where {T} =
    (_taggedptr_check_type(T);
     Core.bitcast(ImmediateOrRef{T}, raw)::ImmediateOrRef{T})
function _taggedptr_from_ref(::Type{ImmediateOrRef{T}}, @nospecialize(x)) where {T}
    _taggedptr_check_type(T)
    x isa T || throw(TypeError(:_taggedptr_from_ref, T, x))
    raw = ccall(:jl_value_ptr, UInt, (Any,), x)
    (raw == 0 || (raw & UInt(1)) != 0) &&
        error("ImmediateOrRef reference encoding requires an aligned nonzero object pointer")
    return _taggedptr_from_raw(ImmediateOrRef{T}, raw)
end
getindex(x::ImmediateOrRef{T}) where {T} = begin
    raw = getrawvalue(x)
    raw == 0 && throw(UndefRefError())
    isimmediate(x) &&
        error("ImmediateOrRef immediate payload does not contain a Julia object reference")
    p = Core.bitcast(Ptr{Cvoid}, raw)
    return ccall(:jl_value_ptr, Any, (Ptr{Cvoid},), p)::T
end
function _taggedptr_setref(x::ImmediateOrRef{T}, @nospecialize(y)) where {T}
    return _taggedptr_from_ref(ImmediateOrRef{T}, y)
end

# isassigned(x) returns true if and only if x[] would work (not throw an error)
function isassigned(x::ImmediateOrRef)
    raw = getrawvalue(x)
    return raw != 0 && (raw & UInt(1)) == 0
end

# isimmediate(x) returns true if and only if x is set to a genuine raw value
# (so not to a pointer and not to NULL)
isimmediate(x::ImmediateOrRef) = (getrawvalue(x) & UInt(1)) == UInt(1)

getrawvalue(x::ImmediateOrRef) = Core.bitcast(UInt, x)::UInt
setrawvalue(x::ImmediateOrRef{T}, raw::UInt) where {T} =
    _taggedptr_from_raw(ImmediateOrRef{T}, raw)::ImmediateOrRef{T}

ImmediateOrRef{T}() where {T} = _taggedptr_from_raw(ImmediateOrRef{T}, UInt(0))
ImmediateOrRef{T}(x::T) where {T} = _taggedptr_from_ref(ImmediateOrRef{T}, x)
