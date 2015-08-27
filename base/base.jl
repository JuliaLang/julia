# This file is a part of Julia. License is MIT: http://julialang.org/license

type ErrorException <: Exception
    msg::AbstractString
end

type SystemError <: Exception
    prefix::AbstractString
    errnum::Int32
    SystemError(p::AbstractString, e::Integer) = new(p, e)
    SystemError(p::AbstractString) = new(p, Libc.errno())
end

type ParseError <: Exception
    msg::AbstractString
end

type ArgumentError <: Exception
    msg::AbstractString
end

#type UnboundError <: Exception
#    var::Symbol
#end

type KeyError <: Exception
    key
end

type MethodError <: Exception
    f
    args
end

type EOFError <: Exception end

type DimensionMismatch <: Exception
    msg::AbstractString
end
DimensionMismatch() = DimensionMismatch("")

type AssertionError <: Exception
    msg::AbstractString

    AssertionError() = new("")
    AssertionError(msg) = new(msg)
end

#Generic wrapping of arbitrary exceptions
#Subtypes should put the exception in an 'error' field
abstract WrappedException <: Exception

type LoadError <: WrappedException
    file::AbstractString
    line::Int
    error
end

type InitError <: WrappedException
    mod::Symbol
    error
end

ccall(:jl_get_system_hooks, Void, ())


==(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
==(w::WeakRef, v) = isequal(w.value, v)
==(w, v::WeakRef) = isequal(w, v.value)

function finalizer(o::ANY, f::Union{Function,Ptr})
    if isimmutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_finalizer, Void, (Any,Any), o, f)
end

finalize(o::ANY) = ccall(:jl_finalize, Void, (Any,), o)

gc(full::Bool=true) = ccall(:jl_gc_collect, Void, (Cint,), full)
gc_enable(on::Bool) = ccall(:jl_gc_enable, Cint, (Cint,), on)!=0

bytestring(str::UTF8String) = str
bytestring(str::ASCIIString) = UTF8String(str.data)

identity(x) = x

# used by { } syntax
function cell_1d(xs::ANY...)
    n = length(xs)
    a = Array(Any,n)
    for i=1:n
        arrayset(a,xs[i],i)
    end
    a
end

function cell_2d(nr, nc, xs::ANY...)
    a = Array(Any,nr,nc)
    for i=1:(nr*nc)
        arrayset(a,xs[i],i)
    end
    a
end

immutable Nullable{T}
    isnull::Bool
    value::T

    Nullable() = new(true)
    Nullable(value::T, isnull::Bool=false) = new(isnull, value)
end
