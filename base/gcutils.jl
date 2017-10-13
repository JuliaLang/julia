# This file is a part of Julia. License is MIT: https://julialang.org/license

==(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
==(w::WeakRef, v) = isequal(w.value, v)
==(w, v::WeakRef) = isequal(w, v.value)

"""
    finalizer(x, f)

Register a function `f(x)` to be called when there are no program-accessible references to
`x`. The type of `x` must be a `mutable struct`, otherwise the behavior of this function is
unpredictable.
"""
function finalizer(@nospecialize(o), @nospecialize(f))
    if isimmutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_finalizer_th, Void, (Ptr{Void}, Any, Any),
          Core.getptls(), o, f)
end

function finalizer(o::T, f::Ptr{Void}) where T
    @_inline_meta
    if isimmutable(T)
        error("objects of type ", T, " cannot be finalized")
    end
    ccall(:jl_gc_add_ptr_finalizer, Void, (Ptr{Void}, Any, Ptr{Void}),
          Core.getptls(), o, f)
end

"""
    finalize(x)

Immediately run finalizers registered for object `x`.
"""
finalize(@nospecialize(o)) = ccall(:jl_finalize_th, Void, (Ptr{Void}, Any,),
                                   Core.getptls(), o)

"""
    gc()

Perform garbage collection. This should not generally be used.
"""
gc(full::Bool=true) = ccall(:jl_gc_collect, Void, (Int32,), full)

"""
    gc_enable(on::Bool)

Control whether garbage collection is enabled using a boolean argument (`true` for enabled,
`false` for disabled). Returns previous GC state. Disabling garbage collection should be
used only with extreme caution, as it can cause memory use to grow without bound.
"""
gc_enable(on::Bool) = ccall(:jl_gc_enable, Int32, (Int32,), on) != 0
