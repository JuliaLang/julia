# This file is a part of Julia. License is MIT: https://julialang.org/license

==(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
==(w::WeakRef, v) = isequal(w.value, v)
==(w, v::WeakRef) = isequal(w, v.value)

"""
    finalizer(f, x)

Register a function `f(x)` to be called when there are no program-accessible references to
`x`, and return `x`. The type of `x` must be a `mutable struct`, otherwise the behavior of
this function is unpredictable.
"""
function finalizer(@nospecialize(f), @nospecialize(o))
    if isimmutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_finalizer_th, Cvoid, (Ptr{Cvoid}, Any, Any),
          Core.getptls(), o, f)
    return o
end

function finalizer(f::Ptr{Cvoid}, o::T) where T
    @_inline_meta
    if isimmutable(T)
        error("objects of type ", T, " cannot be finalized")
    end
    ccall(:jl_gc_add_ptr_finalizer, Cvoid, (Ptr{Cvoid}, Any, Ptr{Cvoid}),
          Core.getptls(), o, f)
    return o
end

"""
    finalize(x)

Immediately run finalizers registered for object `x`.
"""
finalize(@nospecialize(o)) = ccall(:jl_finalize_th, Cvoid, (Ptr{Cvoid}, Any,),
                                   Core.getptls(), o)

module GC

"""
    GC.gc()

Perform garbage collection.

!!! warning
    Excessive use will likely lead to poor performance.
"""
gc(full::Bool=true) = ccall(:jl_gc_collect, Cvoid, (Int32,), full)

"""
    GC.enable(on::Bool)

Control whether garbage collection is enabled using a boolean argument (`true` for enabled,
`false` for disabled). Return previous GC state.

!!! warning
    Disabling garbage collection should be used only with caution, as it can cause memory
    use to grow without bound.
"""
enable(on::Bool) = ccall(:jl_gc_enable, Int32, (Int32,), on) != 0

"""
    GC.@preserve x1 x2 ... xn expr

Temporarily protect the given objects from being garbage collected, even if they would
otherwise be unreferenced.

The last argument is the expression during which the object(s) will be preserved.
The previous arguments are the objects to preserve.
"""
macro preserve(args...)
    syms = args[1:end-1]
    for x in syms
        isa(x, Symbol) || error("Preserved variable must be a symbol")
    end
    s, r = gensym(), gensym()
    esc(quote
        $s = $(Expr(:gc_preserve_begin, syms...))
        $r = $(args[end])
        $(Expr(:gc_preserve_end, s))
        $r
    end)
end

end # module GC
