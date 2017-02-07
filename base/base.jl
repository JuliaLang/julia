# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
    SystemError(prefix::AbstractString, [errno::Int32])

A system call failed with an error code (in the `errno` global variable).
"""
type SystemError <: Exception
    prefix::AbstractString
    errnum::Int32
    extrainfo
    SystemError(p::AbstractString, e::Integer, extrainfo) = new(p, e, extrainfo)
    SystemError(p::AbstractString, e::Integer) = new(p, e, nothing)
    SystemError(p::AbstractString) = new(p, Libc.errno())
end

"""
    ParseError(msg)

The expression passed to the `parse` function could not be interpreted as a valid Julia
expression.
"""
type ParseError <: Exception
    msg::AbstractString
end

"""
    ArgumentError(msg)

The parameters to a function call do not match a valid signature. Argument `msg` is a
descriptive error string.
"""
type ArgumentError <: Exception
    msg::AbstractString
end

#type UnboundError <: Exception
#    var::Symbol
#end

"""
    KeyError(key)

An indexing operation into an `Associative` (`Dict`) or `Set` like object tried to access or
delete a non-existent element.
"""
type KeyError <: Exception
    key
end

"""
    MethodError(f, args)

A method with the required type signature does not exist in the given generic function.
Alternatively, there is no unique most-specific method.
"""
type MethodError <: Exception
    f
    args
    world::UInt
    MethodError(f::ANY, args::ANY, world::UInt) = new(f, args, world)
end
MethodError(f::ANY, args::ANY) = MethodError(f, args, typemax(UInt))

"""
    EOFError()

No more data was available to read from a file or stream.
"""
type EOFError <: Exception end

"""
    DimensionMismatch([msg])

The objects called do not have matching dimensionality. Optional argument `msg` is a
descriptive error string.
"""
type DimensionMismatch <: Exception
    msg::AbstractString
end
DimensionMismatch() = DimensionMismatch("")

"""
    AssertionError([msg])

The asserted condition did not evaluate to `true`.
Optional argument `msg` is a descriptive error string.
"""
type AssertionError <: Exception
    msg::AbstractString
    AssertionError() = new("")
    AssertionError(msg) = new(msg)
end

#Generic wrapping of arbitrary exceptions
#Subtypes should put the exception in an 'error' field
abstract WrappedException <: Exception

"""
    LoadError(file::AbstractString, line::Int, error)

An error occurred while `include`ing, `require`ing, or `using` a file. The error specifics
should be available in the `.error` field.
"""
type LoadError <: WrappedException
    file::AbstractString
    line::Int
    error
end

"""
    InitError(mod::Symbol, error)

An error occurred when running a module's `__init__` function. The actual error thrown is
available in the `.error` field.
"""
type InitError <: WrappedException
    mod::Symbol
    error
end

ccall(:jl_get_system_hooks, Void, ())


==(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
==(w::WeakRef, v) = isequal(w.value, v)
==(w, v::WeakRef) = isequal(w, v.value)

function finalizer(o::ANY, f::ANY)
    if isimmutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_finalizer_th, Void, (Ptr{Void}, Any, Any),
          Core.getptls(), o, f)
end
function finalizer{T}(o::T, f::Ptr{Void})
    @_inline_meta
    if isimmutable(T)
        error("objects of type ", T, " cannot be finalized")
    end
    ccall(:jl_gc_add_ptr_finalizer, Void, (Ptr{Void}, Any, Ptr{Void}),
          Core.getptls(), o, f)
end

finalize(o::ANY) = ccall(:jl_finalize_th, Void, (Ptr{Void}, Any,),
                         Core.getptls(), o)

gc(full::Bool=true) = ccall(:jl_gc_collect, Void, (Int32,), full)
gc_enable(on::Bool) = ccall(:jl_gc_enable, Int32, (Int32,), on) != 0

immutable Nullable{T}
    hasvalue::Bool
    value::T

    Nullable{T}() where T = new(false)
    Nullable{T}(value::T, hasvalue::Bool=true) where T = new(hasvalue, value)
end
